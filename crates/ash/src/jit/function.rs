use std::ffi::c_void;

use ash_macro::to_llvm;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::types::{
    AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType,
};
use inkwell::values::{
    AnyValue, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue,
};
use inkwell::{basic_block::BasicBlock, builder::Builder, AddressSpace, IntPredicate, FloatPredicate};

use super::module::JITModule;
use crate::hl::{
    hl_type,
    hl_type_kind_HBOOL, hl_type_kind_HBYTES, hl_type_kind_HDYN, hl_type_kind_HDYNOBJ,
    hl_type_kind_HF32, hl_type_kind_HF64, hl_type_kind_HI32, hl_type_kind_HI64, hl_type_kind_HOBJ,
    hl_type_kind_HSTRUCT, hl_type_kind_HUI16, hl_type_kind_HUI8, hl_type_kind_HVIRTUAL,
    hl_type_kind_HVOID, vdynamic, hl_runtime_obj, hl_obj_field, vdynobj, vvirtual
};
use crate::opcodes::Opcode;
use crate::types::{HLNative, HLTypeFun, Str};
use crate::{
    hl::{hl_type_kind_HFUN, hl_type_kind_HMETHOD},
    types::HLFunction,
};
use anyhow::{anyhow, Result};

/// Compute HashLink field hash at compile time (same algorithm as hlp_hash_gen)
fn hl_hash_utf8(s: &str) -> i32 {
    let mut h: i32 = 0;
    for c in s.encode_utf16() {
        h = h.wrapping_mul(223).wrapping_add(c as i32);
    }
    h.wrapping_rem(0x1FFFFF7B)
}

#[to_llvm]
unsafe extern "C" {
    fn hlp_get_dynset(d: *mut vdynamic, hfield: i32) -> *mut c_void;
    fn hlp_get_dynget(t: *mut hl_type) -> *mut c_void;
    fn hlp_get_obj_rt(ot: *mut hl_type) -> *mut hl_runtime_obj;
    fn hlp_obj_field_fetch(t: *mut hl_type, fid: i32) -> *mut hl_obj_field;
    fn hlp_alloc_dynobj() -> *mut vdynobj;
    fn hlp_alloc_virtual(t: *mut hl_type) -> *mut vvirtual;
}


/// Reference to a function or a native object
#[derive(Debug, Clone)]
pub enum FuncPtr {
    Fun(HLFunction),
    Native(HLNative),
}

impl<'ctx> JITModule<'ctx> {
    /// Declare an external native function and create a caller wrapper.
    /// Embeds the native function's address directly as inttoptr constant
    /// to avoid MCJIT symbol resolution issues with add_global_mapping.
    fn declare_native(
        &self,
        name: &str,
        param_types: &[BasicMetadataTypeEnum<'ctx>],
        ret_type: Option<BasicTypeEnum<'ctx>>,
    ) -> FunctionValue<'ctx> {
        let caller_name = format!("__native_{}_caller", name);
        if let Some(f) = self.module.get_function(&caller_name) {
            return f;
        }

        let fn_type = match ret_type {
            Some(BasicTypeEnum::IntType(t)) => t.fn_type(param_types, false),
            Some(BasicTypeEnum::FloatType(t)) => t.fn_type(param_types, false),
            Some(BasicTypeEnum::PointerType(t)) => t.fn_type(param_types, false),
            Some(BasicTypeEnum::StructType(t)) => t.fn_type(param_types, false),
            Some(BasicTypeEnum::ArrayType(t)) => t.fn_type(param_types, false),
            Some(BasicTypeEnum::VectorType(t)) => t.fn_type(param_types, false),
            None => self.context.void_type().fn_type(param_types, false),
        };

        let func_addr = self.native_function_resolver
            .resolve_function("std", name)
            .unwrap_or_else(|_| panic!("Failed to resolve native function: {}", name)) as usize;

        self.generate_native_caller_with_addr(&caller_name, fn_type, func_addr)
            .unwrap_or_else(|e| panic!("Failed to generate caller for {}: {}", name, e))
    }

    /// Get or declare an external native function, avoiding builder position clobber.
    /// The `_to_llvm` macro functions reposition the builder, so we save/restore it.
    fn get_or_declare_native(
        &self,
        name: &str,
        declare_fn: impl FnOnce(&'ctx inkwell::context::Context, &inkwell::module::Module<'ctx>, &inkwell::builder::Builder<'ctx>) -> Result<FunctionValue<'ctx>>,
    ) -> Result<FunctionValue<'ctx>> {
        if let Some(f) = self.module.get_function(name) {
            return Ok(f);
        }
        let saved_block = self.builder.get_insert_block();
        let func = declare_fn(self.context, &self.module, &self.builder)?;
        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }
        Ok(func)
    }

    fn create_function_placeholder(
        &self,
        name: &str,
        func_type: FunctionType<'ctx>,
    ) -> FunctionValue<'ctx> {
        self.module
            .add_function(name, func_type, Some(inkwell::module::Linkage::External))
    }

    pub(crate) fn get_or_create_function_value(
        &mut self,
        index: usize,
    ) -> Result<(FunctionValue<'ctx>, bool)> {
        if let Some(f_v) = self.func_cache.get(&index) {
            return Ok((*f_v, false));
        }

        let fun_ptr = self
            .findexes
            .get(&index)
            .ok_or_else(|| anyhow!("Function not found at index {}", index))?
            .clone();

        match fun_ptr {
            FuncPtr::Fun(f) => {
                let name = f.name();
                let type_fun = self.bytecode.types[f.type_.0]
                    .fun
                    .clone()
                    .expect("expect to get function type");
                let func_type = self.create_function_type(&type_fun)?;
                let placeholder = self.create_function_placeholder(&name, func_type);
                self.func_cache.insert(index, placeholder);
                Ok((placeholder, true))
            }
            FuncPtr::Native(native) => {
                let index = native.findex;
                let func = self.init_native_func(&native)?;
                self.func_cache.insert(index as usize, func);
                Ok((func, false))
            }
        }
    }

    pub(crate) fn create_function_type(
        &mut self,
        type_fun: &HLTypeFun,
    ) -> Result<FunctionType<'ctx>> {
        let ret_type = self.get_or_create_any_type(type_fun.ret.0)?;

        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let mut param_types: Vec<BasicMetadataTypeEnum<'ctx>> = type_fun
            .args
            .iter()
            .map(|arg| {
                let arg_type = self.get_or_create_any_type(arg.0).unwrap();
                (match arg_type {
                    AnyTypeEnum::FloatType(t) => t.as_basic_type_enum(),
                    AnyTypeEnum::IntType(t) => t.as_basic_type_enum(),
                    AnyTypeEnum::PointerType(t) => t.as_basic_type_enum(),
                    // Heap-allocated types are always passed by pointer
                    AnyTypeEnum::StructType(_) | AnyTypeEnum::ArrayType(_)
                    | AnyTypeEnum::FunctionType(_) | AnyTypeEnum::VectorType(_)
                    | AnyTypeEnum::VoidType(_) => ptr_type.as_basic_type_enum(),
                })
                .into()
            })
            .collect();

        let function_type = match ret_type {
            AnyTypeEnum::FloatType(t) => t.fn_type(&param_types, false),
            AnyTypeEnum::IntType(t) => t.fn_type(&param_types, false),
            AnyTypeEnum::PointerType(t) => t.fn_type(&param_types, false),
            AnyTypeEnum::VoidType(_) => self.context.void_type().fn_type(&param_types, false),
            // Heap-allocated types are always returned by pointer
            AnyTypeEnum::StructType(_) | AnyTypeEnum::ArrayType(_)
            | AnyTypeEnum::FunctionType(_) | AnyTypeEnum::VectorType(_) =>
                ptr_type.fn_type(&param_types, false),
        };

        Ok(function_type)
    }

    fn add_pending_compilation(&mut self, index: usize) {
        self.pending_compilations.push(index);
    }

    fn compile_pending_functions(&mut self) -> Result<()> {
        while let Some(index) = self.pending_compilations.pop() {
            if let Err(e) = self.compile_function(index) {
                // Compilation failure is non-fatal — stub will be used
            }
        }
        Ok(())
    }

    pub(crate) fn compile_function(&mut self, index: usize) -> Result<()> {
        // Skip if already compiled (has entry block with instructions)
        if let Some(func) = self.func_cache.get(&index) {
            if func.count_basic_blocks() > 0 && func.get_first_basic_block()
                .map_or(false, |bb| bb.get_first_instruction().is_some()) {
                return Ok(());
            }
        }

        let fun_ptr = self
            .findexes
            .get(&index)
            .ok_or_else(|| anyhow!("Function not found at index {}", index))?
            .clone();

        if let FuncPtr::Fun(mut f) = fun_ptr {
            // Run AIR optimization passes on bytecode before LLVM emission
            let pass_manager = air::pass::PassManager::new(air::pass::OptLevel::O2);
            let num_regs = f.regs.len();
            let eliminated = pass_manager.run(&mut f.ops, num_regs);
            if eliminated > 0 {
                eprintln!("AIR: eliminated {} opcodes in {}", eliminated, f.name());
            }

            // Create declaration if not in cache yet
            let function = if let Some(func) = self.func_cache.get(&index) {
                *func
            } else {
                let decl = self.create_function_declaration(&f)?;
                self.func_cache.insert(index, decl);
                decl
            };

            let basic_block = self.context.append_basic_block(function, "entry");
            self.builder.position_at_end(basic_block);

            let (registers, reg_types) = self.allocate_registers(&f)?;
            self.load_function_arguments(&f, &function, &registers)?;
            self.translate_opcodes(&f, &registers, &reg_types)?;

            if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                let ret_type = function.get_type().get_return_type();
                if let Some(ret_type) = ret_type {
                    self.builder.build_return(Some(&ret_type.const_zero()))?;
                } else {
                    self.builder.build_return(None)?;
                }
            }

            if !function.verify(true) {
                // Function verification failed (non-fatal) — stub will be used
                // Function has invalid IR. We can't delete its blocks safely
                // (would create dangling references). Leave it as-is — the module
                // verification will catch it, but MCJIT may still compile valid
                // functions correctly.
            }
        } else if let FuncPtr::Native(native) = fun_ptr {
            // Ensure native function is initialized and in func_cache
            if !self.func_cache.contains_key(&index) {
                let func = self.init_native_func(&native)?;
                self.func_cache.insert(index, func);
            }
        }

        Ok(())
    }

    pub(crate) fn create_function_value(&mut self, index: usize) -> Result<FunctionValue<'ctx>> {
        if let Some(f_v) = self.func_cache.get(&index) {
            return Ok(*f_v);
        }
        let findexes = self.findexes.clone();
        let fun_ptr = findexes
            .get(&index)
            .ok_or_else(|| anyhow!("Function not found at index {}", index))?;

        match fun_ptr {
            FuncPtr::Fun(f) => {
                // Run AIR optimization passes
                let mut f = f.clone();
                let pass_manager = air::pass::PassManager::new(air::pass::OptLevel::O2);
                let num_regs = f.regs.len();
                let eliminated = pass_manager.run(&mut f.ops, num_regs);
                if eliminated > 0 {
                    eprintln!("AIR: eliminated {} opcodes in {}", eliminated, f.name());
                }

                let function = self.create_function_declaration(&f)?;
                let basic_block = self.context.append_basic_block(function, "entry");
                self.builder.position_at_end(basic_block);

                let (registers, reg_types) = self.allocate_registers(&f)?;
                self.load_function_arguments(&f, &function, &registers)?;
                self.translate_opcodes(&f, &registers, &reg_types)?;

                if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                    let ret_type = function.get_type().get_return_type();
                    if let Some(ret_type) = ret_type {
                        self.builder.build_return(Some(&ret_type.const_zero()))?;
                    } else {
                        self.builder.build_return(None)?;
                    }
                }

                if !function.verify(true) {
                    function.print_to_stderr();
                    return Err(anyhow!("Function verification failed for findex {}", f.findex));
                }

                self.func_cache.insert(f.findex as usize, function);
                Ok(function)
            }
            FuncPtr::Native(native) => {
                let func = self.init_native_func(native)?;
                self.func_cache.insert(native.findex as usize, func);
                Ok(func)
            }
        }
    }

    fn create_function_declaration(&mut self, f: &HLFunction) -> Result<FunctionValue<'ctx>> {
        let type_fun = self.bytecode.types[f.type_.0]
            .fun
            .clone()
            .expect("expect to get function type");
        let func_type = self.create_function_type(&type_fun)?;

        Ok(self.module.add_function(&f.name(), func_type, None))
    }

    fn load_function_arguments(
        &self,
        f: &HLFunction,
        function: &FunctionValue<'ctx>,
        registers: &[PointerValue<'ctx>],
    ) -> Result<()> {
        let fun_type = self.bytecode.types[f.type_.0]
            .fun
            .as_ref()
            .expect("expected function type");
        let args_count = fun_type.args.len();

        for i in 0..args_count {
            let param = function.get_nth_param(i as u32)
                .ok_or_else(|| anyhow!("Missing function parameter {}", i))?;
            self.builder.build_store(registers[i], param)?;
        }

        Ok(())
    }

    fn allocate_registers(
        &mut self,
        f: &HLFunction,
    ) -> Result<(Vec<PointerValue<'ctx>>, Vec<BasicTypeEnum<'ctx>>)> {
        let mut ptrs = Vec::with_capacity(f.regs.len());
        let mut types = Vec::with_capacity(f.regs.len());
        for (i, reg) in f.regs.iter().enumerate() {
            let reg_type = self
                .get_register_type(reg.0)
                .expect("expected to get register type");
            types.push(reg_type);
            ptrs.push(self.builder.build_alloca(reg_type, &format!("reg_{}", i))?);
        }
        Ok((ptrs, types))
    }

    fn get_register_type(&mut self, type_index: usize) -> Result<BasicTypeEnum<'ctx>> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        Ok(match self.get_or_create_any_type(type_index)? {
            AnyTypeEnum::FloatType(t) => t.as_basic_type_enum(),
            AnyTypeEnum::IntType(t) => t.as_basic_type_enum(),
            AnyTypeEnum::PointerType(t) => t.as_basic_type_enum(),
            // Heap-allocated types are held as pointers in registers
            AnyTypeEnum::StructType(_) | AnyTypeEnum::ArrayType(_)
            | AnyTypeEnum::FunctionType(_) | AnyTypeEnum::VectorType(_)
            | AnyTypeEnum::VoidType(_) => ptr_type.as_basic_type_enum(),
        })
    }

    fn get_initialized_type(&mut self, type_index: usize) -> Result<BasicValueEnum<'ctx>> {
        if let Some(type_) = self.initialized_type_cache.get(&type_index) {
            return Ok(*type_);
        }
        let kind = self.types_[type_index].clone().kind;

        // For primitive types (kind <= HDYN), create a real C-side hl_type and store its pointer
        // This matches what HOBJ/HSTRUCT/HENUM/HVIRTUAL already do in init_indexes
        let c_type_ptr = unsafe {
            Box::into_raw(Box::new(hl_type {
                kind,
                __bindgen_anon_1: std::mem::zeroed(),
                vobj_proto: std::ptr::null_mut(),
                mark_bits: std::ptr::null_mut(),
            }))
        };

        let ptr_as_int = self.context.i64_type().const_int(c_type_ptr as u64, false);
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let ptr_to_type = ptr_as_int.const_to_pointer(ptr_type);

        self.initialized_type_cache
            .insert(type_index, ptr_to_type.into());

        Ok(ptr_to_type.into())
    }

    fn translate_opcodes(
        &mut self,
        f: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
    ) -> Result<()> {
        let function = self.builder.get_insert_block().unwrap().get_parent().unwrap();
        let num_ops = f.ops.len();

        // Pre-create all basic blocks (two-pass approach for jump resolution)
        let mut opcode_blocks: Vec<BasicBlock<'ctx>> = Vec::with_capacity(num_ops + 1);
        for i in 0..num_ops {
            opcode_blocks.push(self.context.append_basic_block(function, &format!("op_{}", i)));
        }
        // Exit block (fallthrough after last opcode)
        opcode_blocks.push(self.context.append_basic_block(function, "exit"));

        // Branch from entry block to first opcode block
        self.builder.build_unconditional_branch(opcode_blocks[0])?;

        // Emit IR for each opcode
        for (i, op) in f.ops.iter().enumerate() {
            self.builder.position_at_end(opcode_blocks[i]);

            self.translate_opcode(f, op, registers, reg_types, i, &opcode_blocks)?;

            // If the current block has no terminator, add fallthrough to next block
            let current = self.builder.get_insert_block().unwrap();
            if current.get_terminator().is_none() {
                self.builder.build_unconditional_branch(opcode_blocks[i + 1])?;
            }
        }

        // Position builder at exit block for caller to add default return if needed
        self.builder.position_at_end(opcode_blocks[num_ops]);

        Ok(())
    }

    fn translate_opcode(
        &mut self,
        f: &HLFunction,
        op: &Opcode,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        i: usize,
        opcode_blocks: &[BasicBlock<'ctx>],
    ) -> Result<()> {
        match op {
            Opcode::Mov { dst, src } => {
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "src_val",
                )?;
                self.builder.build_store(registers[dst.0 as usize], src_val);
            }
            Opcode::Int { dst, ptr } => {
                let int_val = self
                    .get_int_global(ptr.0)
                    .ok_or_else(|| anyhow!("Int constant not found"))?;
                let loaded_int = self.builder.build_load(
                    self.context.i32_type(),
                    int_val.as_pointer_value(),
                    "int_val",
                )?;
                self.builder
                    .build_store(registers[dst.0 as usize], loaded_int);
            }
            Opcode::Float { dst, ptr } => {
                let float_val = self
                    .get_float_global(ptr.0)
                    .ok_or_else(|| anyhow!("Float constant not found"))?;
                let loaded_float = self.builder.build_load(
                    self.context.f64_type(),
                    float_val.as_pointer_value(),
                    "float_val",
                )?;
                self.builder
                    .build_store(registers[dst.0 as usize], loaded_float);
            }
            Opcode::Bool { dst, value } => {
                let bool_val = self.context.bool_type().const_int(*value as u64, false);
                self.builder
                    .build_store(registers[dst.0 as usize], bool_val);
            }
            Opcode::String { dst, ptr } => {
                let string_val = self
                    .get_string_global(ptr.0)
                    .ok_or_else(|| anyhow!("String constant not found"))?;
                // Store the ADDRESS of the string constant (pointer to first byte)
                self.builder
                    .build_store(registers[dst.0 as usize], string_val.as_pointer_value());
            }
            Opcode::Null { dst } => {
                let null_val = self.context.ptr_type(AddressSpace::default()).const_null();
                self.builder
                    .build_store(registers[dst.0 as usize], null_val);
            }
            Opcode::Add { dst, a, b } => {
                let a_val = self.builder.build_load(
                    reg_types[a.0 as usize],
                    registers[a.0 as usize],
                    "a_val",
                )?;
                let b_val = self.builder.build_load(
                    reg_types[b.0 as usize],
                    registers[b.0 as usize],
                    "b_val",
                )?;
                let result = match (
                    a_val.get_type().as_any_type_enum(),
                    b_val.get_type().as_any_type_enum(),
                ) {
                    (AnyTypeEnum::IntType(_), AnyTypeEnum::IntType(_)) => self
                        .builder
                        .build_int_add(a_val.into_int_value(), b_val.into_int_value(), "add")?
                        .as_any_value_enum()
                        .into_int_value()
                        .as_basic_value_enum(),
                    (AnyTypeEnum::FloatType(_), AnyTypeEnum::FloatType(_)) => {
                        let fv = self.builder
                            .build_float_add(a_val.into_float_value(), b_val.into_float_value(), "add")?;
                        if let Some(inst) = fv.as_instruction() { inst.set_fast_math_flags(1 << 5); }
                        fv.as_basic_value_enum()
                    }
                    _ => return Err(anyhow!("Unsupported types for Add operation")),
                };
                self.builder.build_store(registers[dst.0 as usize], result);
            }
            Opcode::Sub { dst, a, b } => {
                let a_val = self.builder.build_load(
                    reg_types[a.0 as usize],
                    registers[a.0 as usize],
                    "a_val",
                )?;
                let b_val = self.builder.build_load(
                    reg_types[b.0 as usize],
                    registers[b.0 as usize],
                    "b_val",
                )?;
                let result = match (
                    a_val.get_type().as_any_type_enum(),
                    b_val.get_type().as_any_type_enum(),
                ) {
                    (AnyTypeEnum::IntType(_), AnyTypeEnum::IntType(_)) => self
                        .builder
                        .build_int_sub(a_val.into_int_value(), b_val.into_int_value(), "sub")?
                        .as_basic_value_enum(),
                    (AnyTypeEnum::FloatType(_), AnyTypeEnum::FloatType(_)) => {
                        let fv = self.builder
                            .build_float_sub(a_val.into_float_value(), b_val.into_float_value(), "sub")?;
                        if let Some(inst) = fv.as_instruction() { inst.set_fast_math_flags(1 << 5); }
                        fv.as_basic_value_enum()
                    }
                    _ => return Err(anyhow!("Unsupported types for Sub operation")),
                };
                self.builder.build_store(registers[dst.0 as usize], result);
            }
            Opcode::Mul { dst, a, b } => {
                let a_val = self.builder.build_load(
                    reg_types[a.0 as usize],
                    registers[a.0 as usize],
                    "a_val",
                )?;
                let b_val = self.builder.build_load(
                    reg_types[b.0 as usize],
                    registers[b.0 as usize],
                    "b_val",
                )?;
                let result = match (
                    a_val.get_type().as_any_type_enum(),
                    b_val.get_type().as_any_type_enum(),
                ) {
                    (AnyTypeEnum::IntType(_), AnyTypeEnum::IntType(_)) => self
                        .builder
                        .build_int_mul(a_val.into_int_value(), b_val.into_int_value(), "mul")?
                        .as_basic_value_enum(),
                    (AnyTypeEnum::FloatType(_), AnyTypeEnum::FloatType(_)) => {
                        let fv = self.builder
                            .build_float_mul(a_val.into_float_value(), b_val.into_float_value(), "mul")?;
                        if let Some(inst) = fv.as_instruction() { inst.set_fast_math_flags(1 << 5); }
                        fv.as_basic_value_enum()
                    }
                    _ => return Err(anyhow!("Unsupported types for Mul operation")),
                };
                self.builder.build_store(registers[dst.0 as usize], result);
            }
            Opcode::Call0 { dst, fun } => {
                let (function, is_placeholder) = self.get_or_create_function_value(fun.0)?;
                let result = self.builder.build_call(function, &[], "call")?;

                if result.try_as_basic_value().left().is_some() {
                    self.builder.build_store(
                        registers[dst.0 as usize],
                        result.try_as_basic_value().left().unwrap(),
                    );
                }

                if is_placeholder {
                    self.add_pending_compilation(fun.0);
                }
            }

            Opcode::Call1 { dst, fun, arg0 } => {
                let (function, is_placeholder) = self.get_or_create_function_value(fun.0)?;
                let arg0_val = self.builder.build_load(
                    reg_types[arg0.0 as usize],
                    registers[arg0.0 as usize],
                    "arg0_val",
                )?;
                let result = self
                    .builder
                    .build_call(function, &[arg0_val.into()], "call")?;

                if result.try_as_basic_value().left().is_some() {
                    self.builder.build_store(
                        registers[dst.0 as usize],
                        result.try_as_basic_value().left().unwrap(),
                    );
                }

                if is_placeholder {
                    self.add_pending_compilation(fun.0);
                }
            }
            Opcode::Call2 {
                dst,
                fun,
                arg0,
                arg1,
            } => {
                let (function, is_placeholder) = self.get_or_create_function_value(fun.0)?;
                let arg0_val = self.builder.build_load(
                    reg_types[arg0.0 as usize],
                    registers[arg0.0 as usize],
                    "arg0_val",
                )?;
                let arg1_val = self.builder.build_load(
                    reg_types[arg1.0 as usize],
                    registers[arg1.0 as usize],
                    "arg1_val",
                )?;

                let result = self.builder.build_call(
                    function,
                    &[arg0_val.into(), arg1_val.into()],
                    "call",
                )?;

                if result.try_as_basic_value().left().is_some() {
                    self.builder.build_store(
                        registers[dst.0 as usize],
                        result.try_as_basic_value().left().unwrap(),
                    );
                }

                if is_placeholder {
                    self.add_pending_compilation(fun.0);
                }
            }
            Opcode::Call3 {
                dst,
                fun,
                arg0,
                arg1,
                arg2,
            } => {
                let (function, is_placeholder) = self.get_or_create_function_value(fun.0)?;
                let arg0_val = self.builder.build_load(
                    reg_types[arg0.0 as usize],
                    registers[arg0.0 as usize],
                    "arg0_val",
                )?;
                let arg1_val = self.builder.build_load(
                    reg_types[arg1.0 as usize],
                    registers[arg1.0 as usize],
                    "arg1_val",
                )?;
                let arg2_val = self.builder.build_load(
                    reg_types[arg2.0 as usize],
                    registers[arg2.0 as usize],
                    "arg2_val",
                )?;
                let result = self.builder.build_call(
                    function,
                    &[arg0_val.into(), arg1_val.into(), arg2_val.into()],
                    "call",
                )?;

                if result.try_as_basic_value().left().is_some() {
                    self.builder.build_store(
                        registers[dst.0 as usize],
                        result.try_as_basic_value().left().unwrap(),
                    );
                }

                if is_placeholder {
                    self.add_pending_compilation(fun.0);
                }
            }
            Opcode::Ret { ret } => {
                if let Some(t) = self.types_.get(f.regs[ret.0 as usize].0) {
                    if t.kind == hl_type_kind_HVOID {
                        self.builder.build_return(None);
                        return Ok(());
                    }
                }
                let ret_val = self.builder.build_load(
                    reg_types[ret.0 as usize],
                    registers[ret.0 as usize],
                    "ret_val",
                )?;
                self.builder.build_return(Some(&ret_val));
            }
            Opcode::JTrue { cond, offset } => {
                let cond_val = self.builder.build_load(
                    reg_types[cond.0 as usize],
                    registers[cond.0 as usize],
                    "cond_val",
                )?;
                let target = opcode_blocks[(i as i32 + 1 + *offset) as usize];
                let next = opcode_blocks[i + 1];
                self.builder.build_conditional_branch(
                    cond_val.into_int_value(),
                    target,
                    next,
                )?;
            }
            Opcode::JFalse { cond, offset } => {
                let cond_val = self.builder.build_load(
                    reg_types[cond.0 as usize],
                    registers[cond.0 as usize],
                    "cond_val",
                )?;
                let target = opcode_blocks[(i as i32 + 1 + *offset) as usize];
                let next = opcode_blocks[i + 1];
                self.builder.build_conditional_branch(
                    cond_val.into_int_value(),
                    next,
                    target,
                )?;
            }
            Opcode::JAlways { offset } => {
                let target = opcode_blocks[(i as i32 + 1 + *offset) as usize];
                self.builder.build_unconditional_branch(target)?;
            }
            Opcode::GetType { dst, src } => {
                // Load the source value
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "src_val",
                )?;

                // Get the type index of the source value
                let type_index = f.regs.clone()[src.0 as usize].0;

                let typ: BasicValueEnum<'ctx> = self.get_initialized_type(type_index)?;

                // Store the type info in the destination register
                self.builder.build_store(registers[dst.0 as usize], typ);
            }

            Opcode::Type { dst, ty } => {
                let typ: BasicValueEnum<'ctx> = self.get_initialized_type(ty.0)?;
                // Store the type info in the destination register
                self.builder.build_store(registers[dst.0 as usize], typ);
            }

            Opcode::New { dst } => {
                let type_index = f.regs.clone()[dst.0 as usize].0;
                let type_kind = self.types_.clone()[type_index].kind;

                match type_kind {
                    hl_type_kind_HSTRUCT | hl_type_kind_HOBJ => {
                        let type_ = self
                            .initialized_type_cache
                            .get(&type_index)
                            .expect("Expected to get type");
                        let fun = self
                            .func_cache
                            .iter()
                            .find(|(_, f)| {
                                f.get_name().to_string_lossy() == "std_hlp_alloc_obj_caller"
                            })
                            .expect("Expected to find native function hlp_alloc_obj")
                            .1;

                        // type_ is already a pointer constant (inttoptr), pass directly
                        let type_ptr = type_.into_pointer_value();
                        let result = self.builder.build_call(*fun, &[type_ptr.into()], "call")?;
                        self.builder.build_store(
                            registers[dst.0 as usize],
                            result.try_as_basic_value().left().unwrap(),
                        );
                    }
                    hl_type_kind_HDYNOBJ => {
                        let fun = self.declare_native("hlp_alloc_dynobj", &[], Some(self.context.ptr_type(AddressSpace::default()).into()));

                        let result = self.builder.build_call(fun, &[], "call")?;
                        self.builder.build_store(
                            registers[dst.0 as usize],
                            result.try_as_basic_value().left().unwrap(),
                        );
                    }
                    hl_type_kind_HVIRTUAL => {
                        let type_ = self
                            .initialized_type_cache
                            .get(&type_index)
                            .expect("Expected to get type");
                        let fun = self.declare_native("hlp_alloc_virtual", &[self.context.ptr_type(AddressSpace::default()).into()], Some(self.context.ptr_type(AddressSpace::default()).into()));

                        // type_ is already a pointer constant, pass directly
                        let type_ptr = type_.into_pointer_value();
                        let result = self.builder.build_call(fun, &[type_ptr.into()], "call")?;
                        self.builder.build_store(
                            registers[dst.0 as usize],
                            result.try_as_basic_value().left().unwrap(),
                        );
                    }
                    _ => return Err(anyhow!("Can't call constructor on invalid type")),
                }
            }
            Opcode::SetField { obj, field, src } => {
                let obj_type_ = &self.types_[f.regs[obj.0 as usize].0];
                let obj_val = self
                    .builder
                    .build_load(
                        reg_types[obj.0 as usize],
                        registers[obj.0 as usize],
                        "obj_val",
                    )?
                    .into_pointer_value();
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "src_val",
                )?;

                match obj_type_.kind {
                    hl_type_kind_HSTRUCT | hl_type_kind_HOBJ => {
                        // Load hl_type* from object (first field at offset 0)
                        let ptr_type = self.context.ptr_type(AddressSpace::default());
                        let type_ptr = self.builder.build_load(ptr_type, obj_val, "obj_type_ptr")?.into_pointer_value();

                        // Call hl_get_obj_rt with the TYPE pointer, not the object pointer
                        let hl_get_obj_rt = self.declare_native("hlp_get_obj_rt", &[self.context.ptr_type(AddressSpace::default()).into()], Some(self.context.ptr_type(AddressSpace::default()).into()));

                        let rt_obj = self
                            .builder
                            .build_call(hl_get_obj_rt, &[type_ptr.into()], "rt_obj")?
                            .try_as_basic_value()
                            .left()
                            .unwrap();

                        // Get fields_indexes pointer from rt_obj (byte offset 40 in hl_runtime_obj)
                        let fields_indexes_gep = unsafe {
                            self.builder.build_gep(
                                self.context.i8_type(),
                                rt_obj.into_pointer_value(),
                                &[self.context.i64_type().const_int(40, false)],
                                "fields_indexes_gep",
                            )?
                        };
                        let fields_indexes = self.builder.build_load(
                            ptr_type,
                            fields_indexes_gep,
                            "fields_indexes",
                        )?.into_pointer_value();

                        // Get the byte offset for this field: fields_indexes[field] (array of i32)
                        let field_offset_ptr = unsafe {
                            self.builder.build_gep(
                                self.context.i32_type(),
                                fields_indexes,
                                &[self.context.i32_type().const_int(field.0 as u64, false)],
                                "field_offset_ptr",
                            )?
                        };
                        let field_offset = self.builder.build_load(
                            self.context.i32_type(),
                            field_offset_ptr,
                            "field_offset",
                        )?.into_int_value();

                        // Compute field pointer: (i8*)obj + field_offset
                        let field_ptr = unsafe {
                            self.builder.build_gep(
                                self.context.i8_type(),
                                obj_val,
                                &[field_offset],
                                "field_ptr",
                            )?
                        };

                        // Store the new value
                        self.builder.build_store(field_ptr, src_val)?;
                    }
                    hl_type_kind_HVIRTUAL => {
                        // Get the value pointer from vvirtual (byte offset 8)
                        let ptr_type = self.context.ptr_type(AddressSpace::default());
                        let value_gep = unsafe {
                            self.builder.build_gep(
                                self.context.i8_type(),
                                obj_val,
                                &[self.context.i64_type().const_int(8, false)],
                                "value_gep",
                            )?
                        };
                        let value_ptr = self.builder.build_load(
                            ptr_type,
                            value_gep,
                            "value",
                        )?.into_pointer_value();

                        // vfields array is at value + sizeof(ptr) = value + 8 bytes
                        let vfields_ptr = unsafe {
                            self.builder.build_gep(
                                self.context.i8_type(),
                                value_ptr,
                                &[self.context.i64_type().const_int(8, false)],
                                "vfields_ptr",
                            )?
                        };

                        // Get the field pointer: vfields[field] (array of pointers)
                        let field_ptr = unsafe {
                            self.builder.build_gep(
                                ptr_type,
                                vfields_ptr,
                                &[self.context.i32_type().const_int(field.0 as u64, false)],
                                "field_ptr",
                            )?
                        };

                        // Check if the field exists
                        let field_value_ptr = self.builder.build_load(
                            ptr_type,
                            field_ptr,
                            "field_value_ptr",
                        )?;
                        let field_exists = self
                            .builder
                            .build_is_not_null(field_value_ptr.into_pointer_value(), "field_exists")?;

                        let current_fn = self.builder.get_insert_block().unwrap().get_parent().unwrap();
                        let then_block = self
                            .context
                            .append_basic_block(current_fn, "field_exists");
                        let else_block = self
                            .context
                            .append_basic_block(current_fn, "field_not_exists");
                        let cont_block = self.context.append_basic_block(current_fn, "cont");

                        self.builder.build_conditional_branch(
                            field_exists,
                            then_block,
                            else_block,
                        )?;

                        // Field exists: *hl_vfields(o)[f] = v
                        self.builder.position_at_end(then_block);
                        self.builder.build_store(field_value_ptr.into_pointer_value(), src_val)?;
                        self.builder.build_unconditional_branch(cont_block)?;

                        // Field doesn't exist: hl_dyn_set(o,hash(field),vt,v)
                        self.builder.position_at_end(else_block);
                        let hl_dyn_set = self.declare_native("hlp_get_dynset", &[self.context.ptr_type(AddressSpace::default()).into(), self.context.i32_type().into()], Some(self.context.ptr_type(AddressSpace::default()).into()));
                        let hashed_name = obj_type_.virt.as_ref()
                            .map(|v| v.fields.get(field.0).map(|f| f.hashed_name).unwrap_or(0))
                            .unwrap_or(0);
                        let field_hash = self
                            .context
                            .i32_type()
                            .const_int(hashed_name as u64, false);
                        self.builder.build_call(
                            hl_dyn_set,
                            &[obj_val.into(), field_hash.into(), src_val.into()],
                            "dyn_set_result",
                        )?;
                        self.builder.build_unconditional_branch(cont_block)?;

                        // Continue
                        self.builder.position_at_end(cont_block);
                    }
                    _ => return Err(anyhow!("Could not set field of non-object type")),
                }
            }
            Opcode::Field { dst, obj, field } => {
                let obj_type_ = &self.types_[f.regs[obj.0 as usize].0];
                let obj_val = self.builder.build_load(
                    reg_types[obj.0 as usize],
                    registers[obj.0 as usize],
                    "obj_val",
                )?;
                match obj_type_.kind {
                    hl_type_kind_HSTRUCT | hl_type_kind_HOBJ => {
                        // Load hl_type* from object (first field at offset 0)
                        let ptr_type = self.context.ptr_type(AddressSpace::default());
                        let obj_ptr = obj_val.into_pointer_value();
                        let type_ptr = self.builder.build_load(ptr_type, obj_ptr, "obj_type_ptr")?.into_pointer_value();

                        // Call hl_get_obj_rt with the TYPE pointer, not the object pointer
                        let hl_get_obj_rt = self.declare_native("hlp_get_obj_rt", &[self.context.ptr_type(AddressSpace::default()).into()], Some(self.context.ptr_type(AddressSpace::default()).into()));
                        let rt_obj = self
                            .builder
                            .build_call(hl_get_obj_rt, &[type_ptr.into()], "rt_obj")?
                            .try_as_basic_value()
                            .left()
                            .unwrap();

                        // Get fields_indexes pointer from rt_obj (byte offset 40 in hl_runtime_obj)
                        let fields_indexes_gep = unsafe {
                            self.builder.build_gep(
                                self.context.i8_type(),
                                rt_obj.into_pointer_value(),
                                &[self.context.i64_type().const_int(40, false)],
                                "fields_indexes_gep",
                            )?
                        };
                        let fields_indexes = self.builder.build_load(
                            ptr_type,
                            fields_indexes_gep,
                            "fields_indexes",
                        )?.into_pointer_value();

                        // Get the byte offset: fields_indexes[field] (array of i32)
                        let field_offset_ptr = unsafe {
                            self.builder.build_gep(
                                self.context.i32_type(),
                                fields_indexes,
                                &[self.context.i32_type().const_int(field.0 as u64, false)],
                                "field_offset_ptr",
                            )?
                        };
                        let field_offset = self.builder.build_load(
                            self.context.i32_type(),
                            field_offset_ptr,
                            "field_offset",
                        )?.into_int_value();

                        // Compute field pointer: (i8*)obj + field_offset
                        let field_ptr = unsafe {
                            self.builder.build_gep(
                                self.context.i8_type(),
                                obj_ptr,
                                &[field_offset],
                                "field_ptr",
                            )?
                        };

                        // Load the field value using destination register type
                        let load_type = self.get_register_type(f.regs[dst.0 as usize].0)?;
                        let field_val = self.builder.build_load(
                            load_type,
                            field_ptr,
                            "field_val",
                        )?;
                        self.builder
                            .build_store(registers[dst.0 as usize], field_val)?;
                    }
                    hl_type_kind_HVIRTUAL => {
                        // Get the value pointer from vvirtual (byte offset 8)
                        let ptr_type = self.context.ptr_type(AddressSpace::default());
                        let value_gep = unsafe {
                            self.builder.build_gep(
                                self.context.i8_type(),
                                obj_val.into_pointer_value(),
                                &[self.context.i64_type().const_int(8, false)],
                                "value_gep",
                            )?
                        };
                        let value_ptr = self.builder.build_load(
                            ptr_type,
                            value_gep,
                            "value",
                        )?.into_pointer_value();

                        // vfields array is at value + sizeof(ptr) = value + 8 bytes
                        let vfields_ptr = unsafe {
                            self.builder.build_gep(
                                self.context.i8_type(),
                                value_ptr,
                                &[self.context.i64_type().const_int(8, false)],
                                "vfields_ptr",
                            )?
                        };

                        // Get the field pointer: vfields[field] (array of pointers)
                        let field_ptr = unsafe {
                            self.builder.build_gep(
                                ptr_type,
                                vfields_ptr,
                                &[self.context.i32_type().const_int(field.0 as u64, false)],
                                "field_ptr",
                            )?
                        };

                        // Check if the field exists
                        let field_value_check = self.builder.build_load(
                            ptr_type,
                            field_ptr,
                            "field_value_ptr",
                        )?;
                        let field_exists = self.builder.build_is_not_null(
                            field_value_check.into_pointer_value(),
                            "field_exists",
                        )?;

                        let current_fn = self.builder.get_insert_block().unwrap().get_parent().unwrap();
                        let then_block = self
                            .context
                            .append_basic_block(current_fn, "field_exists");
                        let else_block = self
                            .context
                            .append_basic_block(current_fn, "field_not_exists");
                        let cont_block = self.context.append_basic_block(current_fn, "cont");

                        self.builder.build_conditional_branch(
                            field_exists,
                            then_block,
                            else_block,
                        )?;

                        // Field exists: r = *hl_vfields(o)[f]
                        self.builder.position_at_end(then_block);
                        let field_value_ptr = self.builder.build_load(
                            field_ptr.get_type(),
                            field_ptr,
                            "field_value_ptr",
                        )?;
                        let field_value = self.builder.build_load(
                            field_value_ptr.get_type(),
                            field_value_ptr.into_pointer_value(),
                            "field_value",
                        )?;
                        self.builder
                            .build_store(registers[dst.0 as usize], field_value)?;
                        self.builder.build_unconditional_branch(cont_block)?;

                        // Field doesn't exist: r = hl_dyn_get(o,hash(field),vt)
                        self.builder.position_at_end(else_block);
                        let hl_dyn_get = self.declare_native("hlp_get_dynget", &[self.context.ptr_type(AddressSpace::default()).into()], Some(self.context.ptr_type(AddressSpace::default()).into()));
                        let hashed_name = obj_type_.virt.as_ref()
                            .map(|v| v.fields.get(field.0).map(|f| f.hashed_name).unwrap_or(0))
                            .unwrap_or(0);
                        let field_hash = self
                            .context
                            .i32_type()
                            .const_int(hashed_name as u64, false);
                        let result = self.builder.build_call(
                            hl_dyn_get,
                            &[registers[obj.0 as usize].into(), field_hash.into()],
                            "dyn_get_result",
                        )?;
                        let dyn_field_value = result.try_as_basic_value().left().unwrap();
                        self.builder
                            .build_store(registers[dst.0 as usize], dyn_field_value)?;
                        self.builder.build_unconditional_branch(cont_block)?;

                        // Continue
                        self.builder.position_at_end(cont_block);
                    }
                    _ => return Err(anyhow!("Could not get field of non-object type")),
                }
            }

            Opcode::GetGlobal { dst, global } => {
                let global_ptr = *self
                    .globals
                    .get(&global.0)
                    .expect("Expected to get global value");

                // All globals are pointer-sized slots, load as ptr
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let val = self.builder.build_load(
                    ptr_type,
                    global_ptr,
                    "global_load",
                )?;
                self.builder.build_store(registers[dst.0 as usize], val);
            }
            Opcode::SetGlobal { global, src } => {
                let global_ptr = *self
                    .globals
                    .get(&global.0)
                    .expect("Expected to get global value");

                // Load the value from the register, then store into global
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "src_val",
                )?;
                self.builder.build_store(
                    global_ptr,
                    src_val,
                );
            }
            Opcode::GetArray { dst, array, index } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let i32_type = self.context.i32_type();
                let i8_type = self.context.i8_type();

                let arr = self.builder.build_load(ptr_type, registers[array.0 as usize], "getarr_ptr")?.into_pointer_value();
                let idx = self.builder.build_load(i32_type, registers[index.0 as usize], "getarr_idx")?.into_int_value();

                // Data starts at offset 24 (sizeof(varray))
                let data_ptr = unsafe {
                    self.builder.build_gep(i8_type, arr,
                        &[self.context.i64_type().const_int(24, false)], "getarr_data")?
                };

                // Element size from destination type kind
                let dst_type_idx = f.regs[dst.0 as usize].0;
                let dst_kind = self.types_[dst_type_idx].kind;
                let elem_size: u64 = match dst_kind {
                    hl_type_kind_HUI8 => 1,
                    hl_type_kind_HUI16 | hl_type_kind_HBOOL => 2,
                    hl_type_kind_HI32 | hl_type_kind_HF32 => 4,
                    hl_type_kind_HI64 | hl_type_kind_HF64 => 8,
                    _ => 8, // All pointer types = 8
                };

                let elem_size_val = i32_type.const_int(elem_size, false);
                let byte_offset = self.builder.build_int_mul(idx, elem_size_val, "getarr_offset")?;
                let slot = unsafe { self.builder.build_gep(i8_type, data_ptr, &[byte_offset], "getarr_slot")? };
                let element_val = self.builder.build_load(reg_types[dst.0 as usize], slot, "getarr_val")?;
                self.builder.build_store(registers[dst.0 as usize], element_val)?;
            }

            // --- Control flow: Label, Nop ---
            Opcode::Label | Opcode::Nop => {
                // No-op: fallthrough handled by outer loop
            }

            // --- NullCheck ---
            Opcode::NullCheck { reg } => {
                let val = self.builder.build_load(
                    reg_types[reg.0 as usize],
                    registers[reg.0 as usize],
                    "null_check",
                )?;
                if val.is_pointer_value() {
                    let is_null = self.builder.build_is_null(val.into_pointer_value(), "is_null")?;
                    let function = self.builder.get_insert_block().unwrap().get_parent().unwrap();
                    let throw_block = self.context.append_basic_block(function, "null_throw");
                    self.builder.build_conditional_branch(is_null, throw_block, opcode_blocks[i + 1])?;
                    self.builder.position_at_end(throw_block);
                    self.builder.build_unreachable()?;
                }
                // Non-pointer types are never null, fall through
            }

            // --- Null/NotNull jumps ---
            Opcode::JNull { reg, offset } => {
                let val = self.builder.build_load(
                    reg_types[reg.0 as usize],
                    registers[reg.0 as usize],
                    "jnull_val",
                )?;
                let target = opcode_blocks[(i as i32 + 1 + *offset) as usize];
                let next = opcode_blocks[i + 1];
                if val.is_pointer_value() {
                    let is_null = self.builder.build_is_null(val.into_pointer_value(), "is_null")?;
                    self.builder.build_conditional_branch(is_null, target, next)?;
                } else {
                    // Non-pointer types are never null
                    self.builder.build_unconditional_branch(next)?;
                }
            }
            Opcode::JNotNull { reg, offset } => {
                let val = self.builder.build_load(
                    reg_types[reg.0 as usize],
                    registers[reg.0 as usize],
                    "jnotnull_val",
                )?;
                let target = opcode_blocks[(i as i32 + 1 + *offset) as usize];
                let next = opcode_blocks[i + 1];
                if val.is_pointer_value() {
                    let is_not_null = self.builder.build_is_not_null(val.into_pointer_value(), "is_not_null")?;
                    self.builder.build_conditional_branch(is_not_null, target, next)?;
                } else {
                    // Non-pointer types are always not-null
                    self.builder.build_unconditional_branch(target)?;
                }
            }

            // --- Comparison jumps ---
            Opcode::JSLt { a, b, offset } => {
                self.emit_comparison_jump(registers, reg_types,a, b, IntPredicate::SLT, FloatPredicate::OLT, i, *offset, opcode_blocks)?;
            }
            Opcode::JSGte { a, b, offset } => {
                self.emit_comparison_jump(registers, reg_types,a, b, IntPredicate::SGE, FloatPredicate::OGE, i, *offset, opcode_blocks)?;
            }
            Opcode::JSGt { a, b, offset } => {
                self.emit_comparison_jump(registers, reg_types,a, b, IntPredicate::SGT, FloatPredicate::OGT, i, *offset, opcode_blocks)?;
            }
            Opcode::JSLte { a, b, offset } => {
                self.emit_comparison_jump(registers, reg_types,a, b, IntPredicate::SLE, FloatPredicate::OLE, i, *offset, opcode_blocks)?;
            }
            Opcode::JULt { a, b, offset } => {
                self.emit_comparison_jump(registers, reg_types,a, b, IntPredicate::ULT, FloatPredicate::OLT, i, *offset, opcode_blocks)?;
            }
            Opcode::JUGte { a, b, offset } => {
                self.emit_comparison_jump(registers, reg_types,a, b, IntPredicate::UGE, FloatPredicate::OGE, i, *offset, opcode_blocks)?;
            }
            Opcode::JNotLt { a, b, offset } => {
                // !(a < b) is the same as a >= b
                self.emit_comparison_jump(registers, reg_types,a, b, IntPredicate::SGE, FloatPredicate::OGE, i, *offset, opcode_blocks)?;
            }
            Opcode::JNotGte { a, b, offset } => {
                // !(a >= b) is the same as a < b
                self.emit_comparison_jump(registers, reg_types,a, b, IntPredicate::SLT, FloatPredicate::OLT, i, *offset, opcode_blocks)?;
            }
            Opcode::JEq { a, b, offset } => {
                self.emit_comparison_jump(registers, reg_types,a, b, IntPredicate::EQ, FloatPredicate::OEQ, i, *offset, opcode_blocks)?;
            }
            Opcode::JNotEq { a, b, offset } => {
                self.emit_comparison_jump(registers, reg_types,a, b, IntPredicate::NE, FloatPredicate::ONE, i, *offset, opcode_blocks)?;
            }

            // --- Switch ---
            Opcode::Switch { reg, offsets, end } => {
                let val = self.builder.build_load(
                    reg_types[reg.0 as usize],
                    registers[reg.0 as usize],
                    "switch_val",
                )?.into_int_value();
                let default_target = opcode_blocks[(i as i32 + 1 + *end) as usize];
                let cases: Vec<(inkwell::values::IntValue<'ctx>, BasicBlock<'ctx>)> = offsets
                    .iter()
                    .enumerate()
                    .map(|(case_idx, off)| {
                        let case_val = self.context.i32_type().const_int(case_idx as u64, false);
                        let target = opcode_blocks[(i as i32 + 1 + *off) as usize];
                        (case_val, target)
                    })
                    .collect();
                self.builder.build_switch(val, default_target, &cases)?;
            }

            // --- Remaining arithmetic ---
            Opcode::SDiv { dst, a, b } => {
                self.emit_binary_op(registers, reg_types,dst, a, b, "sdiv", |b, av, bv| {
                    match (av.get_type().as_any_type_enum(), bv.get_type().as_any_type_enum()) {
                        (AnyTypeEnum::IntType(_), AnyTypeEnum::IntType(_)) =>
                            Ok(b.build_int_signed_div(av.into_int_value(), bv.into_int_value(), "sdiv")?.as_basic_value_enum()),
                        (AnyTypeEnum::FloatType(_), AnyTypeEnum::FloatType(_)) => {
                            let fv = b.build_float_div(av.into_float_value(), bv.into_float_value(), "sdiv")?;
                            if let Some(inst) = fv.as_instruction() { inst.set_fast_math_flags(1 << 5); }
                            Ok(fv.as_basic_value_enum())
                        }
                        _ => Err(anyhow!("Unsupported types for SDiv")),
                    }
                })?;
            }
            Opcode::UDiv { dst, a, b } => {
                self.emit_binary_op(registers, reg_types,dst, a, b, "udiv", |b, av, bv| {
                    match (av.get_type().as_any_type_enum(), bv.get_type().as_any_type_enum()) {
                        (AnyTypeEnum::IntType(_), AnyTypeEnum::IntType(_)) =>
                            Ok(b.build_int_unsigned_div(av.into_int_value(), bv.into_int_value(), "udiv")?.as_basic_value_enum()),
                        _ => Err(anyhow!("Unsupported types for UDiv")),
                    }
                })?;
            }
            Opcode::SMod { dst, a, b } => {
                self.emit_binary_op(registers, reg_types,dst, a, b, "smod", |b, av, bv| {
                    match (av.get_type().as_any_type_enum(), bv.get_type().as_any_type_enum()) {
                        (AnyTypeEnum::IntType(_), AnyTypeEnum::IntType(_)) =>
                            Ok(b.build_int_signed_rem(av.into_int_value(), bv.into_int_value(), "smod")?.as_basic_value_enum()),
                        (AnyTypeEnum::FloatType(_), AnyTypeEnum::FloatType(_)) => {
                            let fv = b.build_float_rem(av.into_float_value(), bv.into_float_value(), "smod")?;
                            if let Some(inst) = fv.as_instruction() { inst.set_fast_math_flags(1 << 5); }
                            Ok(fv.as_basic_value_enum())
                        }
                        _ => Err(anyhow!("Unsupported types for SMod")),
                    }
                })?;
            }
            Opcode::UMod { dst, a, b } => {
                self.emit_binary_op(registers, reg_types,dst, a, b, "umod", |b, av, bv| {
                    match (av.get_type().as_any_type_enum(), bv.get_type().as_any_type_enum()) {
                        (AnyTypeEnum::IntType(_), AnyTypeEnum::IntType(_)) =>
                            Ok(b.build_int_unsigned_rem(av.into_int_value(), bv.into_int_value(), "umod")?.as_basic_value_enum()),
                        _ => Err(anyhow!("Unsupported types for UMod")),
                    }
                })?;
            }
            Opcode::Shl { dst, a, b } => {
                self.emit_binary_op(registers, reg_types,dst, a, b, "shl", |b, av, bv| {
                    Ok(b.build_left_shift(av.into_int_value(), bv.into_int_value(), "shl")?.as_basic_value_enum())
                })?;
            }
            Opcode::SShr { dst, a, b } => {
                self.emit_binary_op(registers, reg_types,dst, a, b, "sshr", |b, av, bv| {
                    Ok(b.build_right_shift(av.into_int_value(), bv.into_int_value(), true, "sshr")?.as_basic_value_enum())
                })?;
            }
            Opcode::UShr { dst, a, b } => {
                self.emit_binary_op(registers, reg_types,dst, a, b, "ushr", |b, av, bv| {
                    Ok(b.build_right_shift(av.into_int_value(), bv.into_int_value(), false, "ushr")?.as_basic_value_enum())
                })?;
            }
            Opcode::And { dst, a, b } => {
                self.emit_binary_op(registers, reg_types,dst, a, b, "and", |b, av, bv| {
                    Ok(b.build_and(av.into_int_value(), bv.into_int_value(), "and")?.as_basic_value_enum())
                })?;
            }
            Opcode::Or { dst, a, b } => {
                self.emit_binary_op(registers, reg_types,dst, a, b, "or", |b, av, bv| {
                    Ok(b.build_or(av.into_int_value(), bv.into_int_value(), "or")?.as_basic_value_enum())
                })?;
            }
            Opcode::Xor { dst, a, b } => {
                self.emit_binary_op(registers, reg_types,dst, a, b, "xor", |b, av, bv| {
                    Ok(b.build_xor(av.into_int_value(), bv.into_int_value(), "xor")?.as_basic_value_enum())
                })?;
            }
            Opcode::Neg { dst, src } => {
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "neg_src",
                )?;
                let result = match src_val.get_type().as_any_type_enum() {
                    AnyTypeEnum::IntType(_) =>
                        self.builder.build_int_neg(src_val.into_int_value(), "neg")?.as_basic_value_enum(),
                    AnyTypeEnum::FloatType(_) =>
                        self.builder.build_float_neg(src_val.into_float_value(), "neg")?.as_basic_value_enum(),
                    _ => return Err(anyhow!("Unsupported type for Neg")),
                };
                self.builder.build_store(registers[dst.0 as usize], result)?;
            }
            Opcode::Not { dst, src } => {
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "not_src",
                )?.into_int_value();
                let result = self.builder.build_not(src_val, "not")?;
                self.builder.build_store(registers[dst.0 as usize], result)?;
            }
            Opcode::Incr { dst } => {
                let val = self.builder.build_load(
                    reg_types[dst.0 as usize],
                    registers[dst.0 as usize],
                    "incr_val",
                )?.into_int_value();
                let one = val.get_type().const_int(1, false);
                let result = self.builder.build_int_add(val, one, "incr")?;
                self.builder.build_store(registers[dst.0 as usize], result)?;
            }
            Opcode::Decr { dst } => {
                let val = self.builder.build_load(
                    reg_types[dst.0 as usize],
                    registers[dst.0 as usize],
                    "decr_val",
                )?.into_int_value();
                let one = val.get_type().const_int(1, false);
                let result = self.builder.build_int_sub(val, one, "decr")?;
                self.builder.build_store(registers[dst.0 as usize], result)?;
            }

            // --- Call4, CallN ---
            Opcode::Call4 { dst, fun, arg0, arg1, arg2, arg3 } => {
                let (function, is_placeholder) = self.get_or_create_function_value(fun.0)?;
                let args: Vec<BasicMetadataValueEnum> = [arg0, arg1, arg2, arg3]
                    .iter()
                    .map(|arg| {
                        self.builder.build_load(
                            reg_types[arg.0 as usize],
                            registers[arg.0 as usize],
                            "arg_val",
                        ).unwrap().into()
                    })
                    .collect();
                let result = self.builder.build_call(function, &args, "call")?;
                if let Some(ret_val) = result.try_as_basic_value().left() {
                    self.builder.build_store(registers[dst.0 as usize], ret_val)?;
                }
                if is_placeholder {
                    self.add_pending_compilation(fun.0);
                }
            }
            Opcode::CallN { dst, fun, args } => {
                let (function, is_placeholder) = self.get_or_create_function_value(fun.0)?;
                let arg_vals: Vec<BasicMetadataValueEnum> = args
                    .iter()
                    .map(|arg| {
                        self.builder.build_load(
                            reg_types[arg.0 as usize],
                            registers[arg.0 as usize],
                            "arg_val",
                        ).unwrap().into()
                    })
                    .collect();
                let result = self.builder.build_call(function, &arg_vals, "call")?;
                if let Some(ret_val) = result.try_as_basic_value().left() {
                    self.builder.build_store(registers[dst.0 as usize], ret_val)?;
                }
                if is_placeholder {
                    self.add_pending_compilation(fun.0);
                }
            }

            // --- GetThis / SetThis (delegate to Field/SetField with obj = reg 0) ---
            Opcode::GetThis { dst, field } => {
                let rewritten = Opcode::Field { dst: *dst, obj: crate::opcodes::Reg(0), field: *field };
                self.translate_opcode(f, &rewritten, registers, reg_types, i, opcode_blocks)?;
            }
            Opcode::SetThis { field, src } => {
                let rewritten = Opcode::SetField { obj: crate::opcodes::Reg(0), field: *field, src: *src };
                self.translate_opcode(f, &rewritten, registers, reg_types, i, opcode_blocks)?;
            }

            // --- CallMethod (compile-time proto resolution) ---
            Opcode::CallMethod { dst, field, args } => {
                // Resolve method at compile time from the object's type proto table
                let obj_type_idx = f.regs[args[0].0 as usize].0;
                let obj_type = &self.types_[obj_type_idx];
                let findex = obj_type.obj.as_ref()
                    .and_then(|obj| obj.proto.get(field.0).map(|p| p.findex as usize))
                    .ok_or_else(|| anyhow!("CallMethod: cannot resolve proto field {} on type {}", field.0, obj_type_idx))?;

                let (function, is_placeholder) = self.get_or_create_function_value(findex)?;
                let arg_vals: Vec<BasicMetadataValueEnum> = args
                    .iter()
                    .map(|arg| {
                        self.builder.build_load(
                            reg_types[arg.0 as usize],
                            registers[arg.0 as usize],
                            "arg_val",
                        ).unwrap().into()
                    })
                    .collect();
                let result = self.builder.build_call(function, &arg_vals, "call_method")?;
                if let Some(ret_val) = result.try_as_basic_value().left() {
                    self.builder.build_store(registers[dst.0 as usize], ret_val)?;
                }
                if is_placeholder {
                    self.add_pending_compilation(findex);
                }
            }
            // --- CallThis (same as CallMethod but this = reg 0) ---
            Opcode::CallThis { dst, field, args } => {
                let obj_type_idx = f.regs[0].0;
                let obj_type = &self.types_[obj_type_idx];
                let findex = obj_type.obj.as_ref()
                    .and_then(|obj| obj.proto.get(field.0).map(|p| p.findex as usize))
                    .ok_or_else(|| anyhow!("CallThis: cannot resolve proto field {} on type {}", field.0, obj_type_idx))?;

                let (function, is_placeholder) = self.get_or_create_function_value(findex)?;
                // Prepend reg 0 (this) to args
                let mut arg_vals: Vec<BasicMetadataValueEnum> = Vec::with_capacity(args.len() + 1);
                arg_vals.push(self.builder.build_load(
                    reg_types[0],
                    registers[0],
                    "this_val",
                )?.into());
                for arg in args {
                    arg_vals.push(self.builder.build_load(
                        reg_types[arg.0 as usize],
                        registers[arg.0 as usize],
                        "arg_val",
                    )?.into());
                }
                let result = self.builder.build_call(function, &arg_vals, "call_this")?;
                if let Some(ret_val) = result.try_as_basic_value().left() {
                    self.builder.build_store(registers[dst.0 as usize], ret_val)?;
                }
                if is_placeholder {
                    self.add_pending_compilation(findex);
                }
            }

            // --- ToDyn ---
            Opcode::ToDyn { dst, src } => {
                let src_type_idx = f.regs[src.0 as usize].0;
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "todyn_src",
                )?;
                // For pointer types (objects, strings, etc.), just copy the pointer
                if src_val.is_pointer_value() {
                    self.builder.build_store(registers[dst.0 as usize], src_val)?;
                } else {
                    // Primitives: alloca temp, store value, call hlp_make_dyn(&temp, type_ptr)
                    let ptr_type = self.context.ptr_type(AddressSpace::default());
                    let temp = self.builder.build_alloca(reg_types[src.0 as usize], "todyn_temp")?;
                    self.builder.build_store(temp, src_val)?;

                    let type_ptr = self.get_initialized_type(src_type_idx)?
                        .into_pointer_value();
                    let make_dyn = self.declare_native(
                        "hlp_make_dyn",
                        &[ptr_type.into(), ptr_type.into()],
                        Some(ptr_type.into()),
                    );
                    let result = self.builder.build_call(
                        make_dyn,
                        &[temp.into(), type_ptr.into()],
                        "todyn",
                    )?;
                    self.builder.build_store(
                        registers[dst.0 as usize],
                        result.try_as_basic_value().left().unwrap(),
                    )?;
                }
            }

            // --- UnsafeCast ---
            Opcode::UnsafeCast { dst, src } => {
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "unsafe_cast_src",
                )?;
                self.builder.build_store(registers[dst.0 as usize], src_val)?;
            }

            // --- ToSFloat ---
            Opcode::ToSFloat { dst, src } => {
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "tosfloat_src",
                )?;
                let f64_type = self.context.f64_type();
                let result: BasicValueEnum = if src_val.is_int_value() {
                    self.builder.build_signed_int_to_float(
                        src_val.into_int_value(), f64_type, "tosfloat",
                    )?.into()
                } else if src_val.is_float_value() {
                    // Already float — just ensure it's f64
                    let fv = src_val.into_float_value();
                    if fv.get_type() == self.context.f32_type() {
                        self.builder.build_float_ext(fv, f64_type, "tosfloat_ext")?.into()
                    } else {
                        fv.into()
                    }
                } else {
                    return Err(anyhow!("ToSFloat: unexpected source type"));
                };
                self.builder.build_store(registers[dst.0 as usize], result)?;
            }

            // --- ToUFloat ---
            Opcode::ToUFloat { dst, src } => {
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "toufloat_src",
                )?;
                let f64_type = self.context.f64_type();
                let result: BasicValueEnum = if src_val.is_int_value() {
                    self.builder.build_unsigned_int_to_float(
                        src_val.into_int_value(), f64_type, "toufloat",
                    )?.into()
                } else if src_val.is_float_value() {
                    let fv = src_val.into_float_value();
                    if fv.get_type() == self.context.f32_type() {
                        self.builder.build_float_ext(fv, f64_type, "toufloat_ext")?.into()
                    } else {
                        fv.into()
                    }
                } else {
                    return Err(anyhow!("ToUFloat: unexpected source type"));
                };
                self.builder.build_store(registers[dst.0 as usize], result)?;
            }

            // --- ToInt ---
            Opcode::ToInt { dst, src } => {
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "toint_src",
                )?;
                let i32_type = self.context.i32_type();
                let result: BasicValueEnum = if src_val.is_float_value() {
                    self.builder.build_float_to_signed_int(
                        src_val.into_float_value(), i32_type, "toint",
                    )?.into()
                } else if src_val.is_int_value() {
                    // Already int — truncate or extend to i32
                    let iv = src_val.into_int_value();
                    if iv.get_type().get_bit_width() > 32 {
                        self.builder.build_int_truncate(iv, i32_type, "toint_trunc")?.into()
                    } else if iv.get_type().get_bit_width() < 32 {
                        self.builder.build_int_s_extend(iv, i32_type, "toint_ext")?.into()
                    } else {
                        iv.into()
                    }
                } else {
                    return Err(anyhow!("ToInt: unexpected source type"));
                };
                self.builder.build_store(registers[dst.0 as usize], result)?;
            }

            // --- StaticClosure: allocate a vclosure wrapping the function ---
            Opcode::StaticClosure { dst, fun } => {
                let (_function, is_placeholder) = self.get_or_create_function_value(fun.0)?;
                if is_placeholder {
                    self.add_pending_compilation(fun.0);
                }

                let ptr_type = self.context.ptr_type(AddressSpace::default());

                // Load function address from functions_ptrs[findex] at runtime
                let findex = fun.0 as usize;
                let fun_addr_slot = unsafe { self.functions_ptrs.as_ptr().add(findex) } as u64;
                let fun_addr_ptr = self.context.i64_type().const_int(fun_addr_slot, false)
                    .const_to_pointer(ptr_type);
                let fun_addr = self.builder.build_load(ptr_type, fun_addr_ptr, "static_closure_fun")?
                    .into_pointer_value();

                // Get function type pointer (compile-time constant from func_types)
                let type_ptr_val = self.func_types[findex] as u64;
                let type_ptr = self.context.i64_type().const_int(type_ptr_val, false)
                    .const_to_pointer(ptr_type);

                // Call hlp_alloc_closure_void(type, fun_addr) -> *mut vclosure
                let alloc_closure = self.declare_native(
                    "hlp_alloc_closure_void",
                    &[ptr_type.into(), ptr_type.into()],
                    Some(ptr_type.into()),
                );
                let closure = self.builder.build_call(
                    alloc_closure, &[type_ptr.into(), fun_addr.into()], "static_closure",
                )?.try_as_basic_value().left().unwrap();
                self.builder.build_store(registers[dst.0 as usize], closure)?;
            }

            // --- CallClosure ---
            Opcode::CallClosure { dst, fun, args } => {
                let closure_ptr = self.builder.build_load(
                    reg_types[fun.0 as usize],
                    registers[fun.0 as usize],
                    "closure_ptr",
                )?.into_pointer_value();

                let i8_type = self.context.i8_type();
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let i32_type = self.context.i32_type();

                // vclosure.fun at offset 8
                let fun_field_gep = unsafe { self.builder.build_gep(
                    i8_type, closure_ptr,
                    &[self.context.i64_type().const_int(8, false)],
                    "closure_fun_gep",
                )? };
                let fun_ptr = self.builder.build_load(
                    ptr_type, fun_field_gep, "closure_fun",
                )?.into_pointer_value();

                // vclosure.hasValue at offset 16
                let has_value_gep = unsafe { self.builder.build_gep(
                    i8_type, closure_ptr,
                    &[self.context.i64_type().const_int(16, false)],
                    "closure_hasvalue_gep",
                )? };
                let has_value = self.builder.build_load(
                    i32_type, has_value_gep, "has_value",
                )?.into_int_value();

                // vclosure.value at offset 24
                let value_gep = unsafe { self.builder.build_gep(
                    i8_type, closure_ptr,
                    &[self.context.i64_type().const_int(24, false)],
                    "closure_value_gep",
                )? };
                let closure_value = self.builder.build_load(
                    ptr_type, value_gep, "closure_value",
                )?.into_pointer_value();


                // Load all explicit args
                let arg_vals: Vec<BasicMetadataValueEnum> = args
                    .iter()
                    .map(|arg| {
                        self.builder.build_load(
                            reg_types[arg.0 as usize],
                            registers[arg.0 as usize],
                            "arg_val",
                        ).unwrap().into()
                    })
                    .collect();

                // Determine function type from register's type info
                let fun_type_idx = f.regs[fun.0 as usize].0;
                let base_fn_type = if let Some(fun_type) = self.types_[fun_type_idx].fun.clone() {
                    self.create_function_type(&fun_type)?
                } else {
                    // Dynamic-typed closure: infer from args (all ptrs) with ptr return
                    let dyn_params: Vec<BasicMetadataTypeEnum> = args.iter()
                        .map(|_| ptr_type.into())
                        .collect();
                    // Determine return type from dst register
                    let dst_type = reg_types[dst.0 as usize];
                    match dst_type {
                        BasicTypeEnum::IntType(t) => t.fn_type(&dyn_params, false),
                        BasicTypeEnum::FloatType(t) => t.fn_type(&dyn_params, false),
                        _ => ptr_type.fn_type(&dyn_params, false),
                    }
                };

                // Build extended function type (with value prepended as first arg)
                let mut extended_params: Vec<BasicMetadataTypeEnum> = vec![ptr_type.into()];
                extended_params.extend(base_fn_type.get_param_types().iter().map(|t| {
                    let bmt: BasicMetadataTypeEnum = (*t).into();
                    bmt
                }));
                let extended_fn_type = if base_fn_type.get_return_type().is_some() {
                    let ret = base_fn_type.get_return_type().unwrap();
                    match ret {
                        BasicTypeEnum::FloatType(t) => t.fn_type(&extended_params, false),
                        BasicTypeEnum::IntType(t) => t.fn_type(&extended_params, false),
                        BasicTypeEnum::PointerType(t) => t.fn_type(&extended_params, false),
                        BasicTypeEnum::ArrayType(t) => t.fn_type(&extended_params, false),
                        BasicTypeEnum::StructType(t) => t.fn_type(&extended_params, false),
                        BasicTypeEnum::VectorType(t) => t.fn_type(&extended_params, false),
                    }
                } else {
                    self.context.void_type().fn_type(&extended_params, false)
                };

                // Branch based on hasValue
                let has_value_cmp = self.builder.build_int_compare(
                    IntPredicate::NE, has_value, i32_type.const_zero(), "has_value_cmp",
                )?;

                let function = self.builder.get_insert_block().unwrap().get_parent().unwrap();
                let call_with_value_bb = self.context.append_basic_block(function, "call_with_value");
                let call_without_value_bb = self.context.append_basic_block(function, "call_without_value");
                let call_done_bb = self.context.append_basic_block(function, "call_done");

                self.builder.build_conditional_branch(has_value_cmp, call_with_value_bb, call_without_value_bb)?;

                // --- Call WITH value (hasValue != 0) ---
                self.builder.position_at_end(call_with_value_bb);
                let mut args_with_value: Vec<BasicMetadataValueEnum> = vec![closure_value.into()];
                args_with_value.extend(arg_vals.iter().cloned());
                let result_with_value = self.builder.build_indirect_call(
                    extended_fn_type, fun_ptr, &args_with_value, "call_closure_hv",
                )?;
                if let Some(ret_val) = result_with_value.try_as_basic_value().left() {
                    self.builder.build_store(registers[dst.0 as usize], ret_val)?;
                }
                self.builder.build_unconditional_branch(call_done_bb)?;

                // --- Call WITHOUT value (hasValue == 0) ---
                self.builder.position_at_end(call_without_value_bb);
                let result_without_value = self.builder.build_indirect_call(
                    base_fn_type, fun_ptr, &arg_vals, "call_closure",
                )?;
                if let Some(ret_val) = result_without_value.try_as_basic_value().left() {
                    self.builder.build_store(registers[dst.0 as usize], ret_val)?;
                }
                self.builder.build_unconditional_branch(call_done_bb)?;

                // Continue from call_done
                self.builder.position_at_end(call_done_bb);
            }

            // --- SafeCast: for now, just copy the pointer (runtime check deferred) ---
            Opcode::SafeCast { dst, src } => {
                // Simple copy for now — full hlp_value_cast deferred
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "safecast_src",
                )?;
                self.builder.build_store(registers[dst.0 as usize], src_val)?;
            }

            // --- ToVirtual: copy pointer (virtual wrapping deferred) ---
            Opcode::ToVirtual { dst, src } => {
                // Simple copy for now — full hlp_dyn_castp deferred
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "tovirt_src",
                )?;
                self.builder.build_store(registers[dst.0 as usize], src_val)?;
            }

            // --- Trap: setjmp-based exception handling ---
            Opcode::Trap { exc, offset } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let i32_type = self.context.i32_type();

                // 1. Call hlp_setup_trap_jit() → returns *mut c_int (jmp_buf pointer)
                let setup = self.declare_native("hlp_setup_trap_jit", &[], Some(ptr_type.into()));
                let buf_ptr = self.builder.build_call(setup, &[], "trap_buf")?
                    .try_as_basic_value().left().unwrap().into_pointer_value();

                // 2. Call _setjmp(buf_ptr) via indirect call (system function, not in stdlib)
                let setjmp_addr = crate::hl::_setjmp as usize as u64;
                let setjmp_ptr = self.context.i64_type().const_int(setjmp_addr, false)
                    .const_to_pointer(ptr_type);
                let setjmp_fn_type = i32_type.fn_type(&[ptr_type.into()], false);
                let setjmp_call = self.builder.build_indirect_call(
                    setjmp_fn_type, setjmp_ptr, &[buf_ptr.into()], "setjmp_ret"
                )?;
                // Mark as returns_twice so LLVM doesn't misoptimize around setjmp at O3
                let rt_kind = inkwell::attributes::Attribute::get_named_enum_kind_id("returns_twice");
                let rt_attr = self.context.create_enum_attribute(rt_kind, 0);
                setjmp_call.add_attribute(inkwell::attributes::AttributeLoc::Function, rt_attr);
                let setjmp_result = setjmp_call.try_as_basic_value().left().unwrap().into_int_value();

                // 3. Branch: 0 → normal (protected code), non-zero → handler
                let is_exception = self.builder.build_int_compare(
                    IntPredicate::NE, setjmp_result, i32_type.const_zero(), "is_exc")?;

                let handler_block = opcode_blocks[(i as i32 + 1 + *offset) as usize];
                let normal_block = opcode_blocks[i + 1];

                // Create handler_entry block to load exc value before jumping to handler
                let function = self.builder.get_insert_block().unwrap().get_parent().unwrap();
                let handler_entry = self.context.append_basic_block(function, &format!("trap_handler_{}", i));

                self.builder.build_conditional_branch(is_exception, handler_entry, normal_block)?;

                // Emit handler entry: load exc value into exc register, then branch to handler
                self.builder.position_at_end(handler_entry);
                let get_exc = self.declare_native("hlp_get_exc_value", &[], Some(ptr_type.into()));
                let exc_val = self.builder.build_call(get_exc, &[], "exc_val")?
                    .try_as_basic_value().left().unwrap();
                self.builder.build_store(registers[exc.0 as usize], exc_val)?;
                self.builder.build_unconditional_branch(handler_block)?;
            }

            // --- EndTrap: remove trap context ---
            Opcode::EndTrap { exc: _ } => {
                let remove = self.declare_native("hlp_remove_trap_jit", &[], None);
                self.builder.build_call(remove, &[], "")?;
            }

            // --- Throw: call hlp_throw (diverging) ---
            Opcode::Throw { exc } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let exc_val = self.builder.build_load(ptr_type, registers[exc.0 as usize], "throw_val")?;
                let throw_fn = self.declare_native("hlp_throw", &[ptr_type.into()], None);
                self.builder.build_call(throw_fn, &[exc_val.into()], "")?;
                self.builder.build_unreachable()?;
            }

            // --- Rethrow: same as Throw ---
            Opcode::Rethrow { exc } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let exc_val = self.builder.build_load(ptr_type, registers[exc.0 as usize], "rethrow_val")?;
                let throw_fn = self.declare_native("hlp_throw", &[ptr_type.into()], None);
                self.builder.build_call(throw_fn, &[exc_val.into()], "")?;
                self.builder.build_unreachable()?;
            }

            // --- Ref: take address of register ---
            Opcode::Ref { dst, src } => {
                // dst = &src (pointer to the register's alloca)
                self.builder.build_store(registers[dst.0 as usize], registers[src.0 as usize])?;
            }

            // --- Unref: dereference pointer ---
            Opcode::Unref { dst, src } => {
                let ptr = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "unref_ptr",
                )?.into_pointer_value();
                let val = self.builder.build_load(
                    reg_types[dst.0 as usize],
                    ptr,
                    "unref_val",
                )?;
                self.builder.build_store(registers[dst.0 as usize], val)?;
            }

            // --- Setref: store through pointer ---
            Opcode::Setref { dst, value } => {
                let ptr = self.builder.build_load(
                    reg_types[dst.0 as usize],
                    registers[dst.0 as usize],
                    "setref_ptr",
                )?.into_pointer_value();
                let val = self.builder.build_load(
                    reg_types[value.0 as usize],
                    registers[value.0 as usize],
                    "setref_val",
                )?;
                self.builder.build_store(ptr, val)?;
            }

            // --- InstanceClosure: allocate closure binding obj as first arg ---
            Opcode::InstanceClosure { dst, fun, obj } => {
                let (_function, is_placeholder) = self.get_or_create_function_value(fun.0)?;
                if is_placeholder {
                    self.add_pending_compilation(fun.0);
                }

                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let findex = fun.0 as usize;

                // Load function address from functions_ptrs[findex]
                let fun_addr_slot = unsafe { self.functions_ptrs.as_ptr().add(findex) } as u64;
                let fun_addr_ptr = self.context.i64_type().const_int(fun_addr_slot, false)
                    .const_to_pointer(ptr_type);
                let fun_addr = self.builder.build_load(ptr_type, fun_addr_ptr, "inst_closure_fun")?
                    .into_pointer_value();

                // Get closure type via hlp_get_closure_type(func_type)
                // This removes the first param (bound obj's type) from the fn signature
                let func_type_ptr = self.func_types[findex] as u64;
                let func_type_const = self.context.i64_type().const_int(func_type_ptr, false)
                    .const_to_pointer(ptr_type);
                let get_closure_type = self.declare_native("hlp_get_closure_type",
                    &[ptr_type.into()], Some(ptr_type.into()));
                let closure_type = self.builder.build_call(get_closure_type,
                    &[func_type_const.into()], "closure_type")?
                    .try_as_basic_value().left().unwrap();

                // Load bound object
                let obj_val = self.builder.build_load(ptr_type, registers[obj.0 as usize], "inst_obj")?;

                // Call hlp_alloc_closure_ptr(closure_type, fun_addr, obj_ptr)
                let alloc = self.declare_native("hlp_alloc_closure_ptr",
                    &[ptr_type.into(), ptr_type.into(), ptr_type.into()],
                    Some(ptr_type.into()));
                let closure = self.builder.build_call(alloc,
                    &[closure_type.into(), fun_addr.into(), obj_val.into()],
                    "inst_closure")?.try_as_basic_value().left().unwrap();
                self.builder.build_store(registers[dst.0 as usize], closure)?;
            }

            // --- VirtualClosure: resolve proto method, create bound closure ---
            Opcode::VirtualClosure { dst, obj, field } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let obj_type_idx = f.regs[obj.0 as usize].0;
                let obj_type_info = self.types_[obj_type_idx].clone();

                // Resolve findex from proto table at compile time
                let findex = if let Some(ref obj_data) = obj_type_info.obj {
                    obj_data.proto[field.0 as usize].findex as usize
                } else {
                    return Err(anyhow!("VirtualClosure: obj register type has no proto table"));
                };

                let (_function, is_placeholder) = self.get_or_create_function_value(findex)?;
                if is_placeholder {
                    self.add_pending_compilation(findex);
                }

                // Load obj pointer
                let obj_val = self.builder.build_load(ptr_type, registers[obj.0 as usize], "vclos_obj")?;

                // Load function address from functions_ptrs[findex]
                let fun_addr_slot = unsafe { self.functions_ptrs.as_ptr().add(findex) } as u64;
                let fun_addr_ptr = self.context.i64_type().const_int(fun_addr_slot, false)
                    .const_to_pointer(ptr_type);
                let fun_addr = self.builder.build_load(ptr_type, fun_addr_ptr, "vclos_fun")?
                    .into_pointer_value();

                // Get closure type via hlp_get_closure_type(func_type)
                let func_type_ptr = self.func_types[findex] as u64;
                let func_type_const = self.context.i64_type().const_int(func_type_ptr, false)
                    .const_to_pointer(ptr_type);
                let get_closure_type = self.declare_native("hlp_get_closure_type",
                    &[ptr_type.into()], Some(ptr_type.into()));
                let closure_type = self.builder.build_call(get_closure_type,
                    &[func_type_const.into()], "vclos_type")?
                    .try_as_basic_value().left().unwrap();

                // Call hlp_alloc_closure_ptr(closure_type, fun_addr, obj_ptr)
                let alloc = self.declare_native("hlp_alloc_closure_ptr",
                    &[ptr_type.into(), ptr_type.into(), ptr_type.into()],
                    Some(ptr_type.into()));
                let closure = self.builder.build_call(alloc,
                    &[closure_type.into(), fun_addr.into(), obj_val.into()],
                    "vclos")?.try_as_basic_value().left().unwrap();
                self.builder.build_store(registers[dst.0 as usize], closure)?;
            }

            // --- DynGet: dynamic field access (stub) ---
            Opcode::DynGet { dst, obj, field } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let i32_type = self.context.i32_type();
                let i64_type = self.context.i64_type();

                let obj_val = self.builder.build_load(ptr_type, registers[obj.0 as usize], "dynget_obj")?;
                let field_name = &self.bytecode.strings[field.0].clone();
                let hfield = hl_hash_utf8(field_name);
                let hfield_val = i32_type.const_int(hfield as u64, true);

                let dst_type_idx = f.regs[dst.0 as usize].0;
                let dst_kind = self.types_[dst_type_idx].kind;

                match dst_kind {
                    hl_type_kind_HF64 => {
                        let getter = self.declare_native("hlp_dyn_getd",
                            &[ptr_type.into(), i32_type.into()],
                            Some(self.context.f64_type().into()));
                        let result = self.builder.build_call(getter,
                            &[obj_val.into(), hfield_val.into()], "dynget_d")?;
                        self.builder.build_store(registers[dst.0 as usize],
                            result.try_as_basic_value().left().unwrap())?;
                    }
                    hl_type_kind_HF32 => {
                        let getter = self.declare_native("hlp_dyn_getf",
                            &[ptr_type.into(), i32_type.into()],
                            Some(self.context.f32_type().into()));
                        let result = self.builder.build_call(getter,
                            &[obj_val.into(), hfield_val.into()], "dynget_f")?;
                        self.builder.build_store(registers[dst.0 as usize],
                            result.try_as_basic_value().left().unwrap())?;
                    }
                    hl_type_kind_HI64 => {
                        let getter = self.declare_native("hlp_dyn_geti64",
                            &[ptr_type.into(), i32_type.into()],
                            Some(i64_type.into()));
                        let result = self.builder.build_call(getter,
                            &[obj_val.into(), hfield_val.into()], "dynget_i64")?;
                        self.builder.build_store(registers[dst.0 as usize],
                            result.try_as_basic_value().left().unwrap())?;
                    }
                    hl_type_kind_HI32 | hl_type_kind_HBOOL | hl_type_kind_HUI8 | hl_type_kind_HUI16 => {
                        let type_ptr = self.get_initialized_type(dst_type_idx)?
                            .into_pointer_value();
                        let getter = self.declare_native("hlp_dyn_geti",
                            &[ptr_type.into(), i32_type.into(), ptr_type.into()],
                            Some(i32_type.into()));
                        let result = self.builder.build_call(getter,
                            &[obj_val.into(), hfield_val.into(), type_ptr.into()], "dynget_i")?;
                        self.builder.build_store(registers[dst.0 as usize],
                            result.try_as_basic_value().left().unwrap())?;
                    }
                    _ => {
                        // Pointer types: hlp_dyn_getp(obj, hfield, dst_type)
                        let type_ptr = self.get_initialized_type(dst_type_idx)?
                            .into_pointer_value();
                        let getter = self.declare_native("hlp_dyn_getp",
                            &[ptr_type.into(), i32_type.into(), ptr_type.into()],
                            Some(ptr_type.into()));
                        let result = self.builder.build_call(getter,
                            &[obj_val.into(), hfield_val.into(), type_ptr.into()], "dynget_p")?;
                        self.builder.build_store(registers[dst.0 as usize],
                            result.try_as_basic_value().left().unwrap())?;
                    }
                }
            }

            // --- DynSet: dynamic field set ---
            Opcode::DynSet { obj, field, src } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let i32_type = self.context.i32_type();

                let obj_val = self.builder.build_load(ptr_type, registers[obj.0 as usize], "dynset_obj")?;
                let field_name = &self.bytecode.strings[field.0].clone();
                let hfield = hl_hash_utf8(field_name);
                let hfield_val = i32_type.const_int(hfield as u64, true);

                let src_type_idx = f.regs[src.0 as usize].0;
                let src_kind = self.types_[src_type_idx].kind;
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize], registers[src.0 as usize], "dynset_src")?;

                match src_kind {
                    hl_type_kind_HF64 => {
                        let setter = self.declare_native("hlp_dyn_setd",
                            &[ptr_type.into(), i32_type.into(), self.context.f64_type().into()],
                            None);
                        self.builder.build_call(setter,
                            &[obj_val.into(), hfield_val.into(), src_val.into()], "dynset_d")?;
                    }
                    hl_type_kind_HF32 => {
                        let setter = self.declare_native("hlp_dyn_setf",
                            &[ptr_type.into(), i32_type.into(), self.context.f32_type().into()],
                            None);
                        self.builder.build_call(setter,
                            &[obj_val.into(), hfield_val.into(), src_val.into()], "dynset_f")?;
                    }
                    hl_type_kind_HI64 => {
                        let setter = self.declare_native("hlp_dyn_seti64",
                            &[ptr_type.into(), i32_type.into(), self.context.i64_type().into()],
                            None);
                        self.builder.build_call(setter,
                            &[obj_val.into(), hfield_val.into(), src_val.into()], "dynset_i64")?;
                    }
                    hl_type_kind_HI32 | hl_type_kind_HBOOL | hl_type_kind_HUI8 | hl_type_kind_HUI16 => {
                        let type_ptr = self.get_initialized_type(src_type_idx)?
                            .into_pointer_value();
                        let setter = self.declare_native("hlp_dyn_seti",
                            &[ptr_type.into(), i32_type.into(), ptr_type.into(), i32_type.into()],
                            None);
                        self.builder.build_call(setter,
                            &[obj_val.into(), hfield_val.into(), type_ptr.into(), src_val.into()], "dynset_i")?;
                    }
                    _ => {
                        // Pointer types: hlp_dyn_setp(obj, hfield, type, value)
                        let type_ptr = self.get_initialized_type(src_type_idx)?
                            .into_pointer_value();
                        let setter = self.declare_native("hlp_dyn_setp",
                            &[ptr_type.into(), i32_type.into(), ptr_type.into(), ptr_type.into()],
                            None);
                        self.builder.build_call(setter,
                            &[obj_val.into(), hfield_val.into(), type_ptr.into(), src_val.into()], "dynset_p")?;
                    }
                }
            }

            // --- Bytes: load bytes constant ---
            Opcode::Bytes { dst, ptr } => {
                if let Some(bytes_global) = self.get_bytes_global(ptr.0) {
                    self.builder.build_store(
                        registers[dst.0 as usize],
                        bytes_global.as_pointer_value(),
                    )?;
                } else {
                    let null_ptr = self.context.ptr_type(AddressSpace::default()).const_null();
                    self.builder.build_store(registers[dst.0 as usize], null_ptr)?;
                }
            }

            // --- Enum opcodes ---
            Opcode::EnumAlloc { dst, construct } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let i32_type = self.context.i32_type();
                let type_index = f.regs[dst.0 as usize].0;
                let type_ptr = self.get_initialized_type(type_index)?.into_pointer_value();

                let alloc_enum = self.declare_native("hlp_alloc_enum",
                    &[ptr_type.into(), i32_type.into()], Some(ptr_type.into()));
                let construct_val = i32_type.const_int(construct.0 as u64, false);
                let result = self.builder.build_call(alloc_enum,
                    &[type_ptr.into(), construct_val.into()], "enum_alloc")?;
                self.builder.build_store(registers[dst.0 as usize],
                    result.try_as_basic_value().left().unwrap())?;
            }
            Opcode::MakeEnum { dst, construct, args } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let i32_type = self.context.i32_type();
                let i8_type = self.context.i8_type();
                let type_index = f.regs[dst.0 as usize].0;
                let type_ptr = self.get_initialized_type(type_index)?.into_pointer_value();

                // Allocate the enum
                let alloc_enum = self.declare_native("hlp_alloc_enum",
                    &[ptr_type.into(), i32_type.into()], Some(ptr_type.into()));
                let construct_val = i32_type.const_int(construct.0 as u64, false);
                let venum_ptr = self.builder.build_call(alloc_enum,
                    &[type_ptr.into(), construct_val.into()], "make_enum")?
                    .try_as_basic_value().left().unwrap().into_pointer_value();

                // Write each arg at its pre-computed offset
                let tenum = self.types_[type_index].tenum.as_ref()
                    .ok_or_else(|| anyhow!("MakeEnum: type {} is not an enum", type_index))?;
                let construct_info = &tenum.constructs[construct.0];

                for (j, arg) in args.iter().enumerate() {
                    let arg_val = self.builder.build_load(
                        reg_types[arg.0 as usize], registers[arg.0 as usize],
                        &format!("make_enum_arg_{}", j))?;
                    let offset = construct_info.offsets[j] as u64;
                    let param_ptr = unsafe {
                        self.builder.build_gep(i8_type, venum_ptr,
                            &[self.context.i64_type().const_int(offset, false)],
                            &format!("make_enum_param_{}", j))?
                    };
                    self.builder.build_store(param_ptr, arg_val)?;
                }

                self.builder.build_store(registers[dst.0 as usize], venum_ptr)?;
            }
            Opcode::EnumIndex { dst, value } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let venum_ptr = self.builder.build_load(ptr_type, registers[value.0 as usize], "enumidx_ptr")?.into_pointer_value();
                // venum.index is i32 at offset 8
                let index_gep = unsafe {
                    self.builder.build_gep(self.context.i8_type(), venum_ptr,
                        &[self.context.i64_type().const_int(8, false)], "enumidx_gep")?
                };
                let index_val = self.builder.build_load(self.context.i32_type(), index_gep, "enumidx_val")?;
                self.builder.build_store(registers[dst.0 as usize], index_val)?;
            }
            Opcode::EnumField { dst, value, construct, field } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let venum_ptr = self.builder.build_load(ptr_type, registers[value.0 as usize], "enumfield_ptr")?.into_pointer_value();

                let value_type_idx = f.regs[value.0 as usize].0;
                let tenum = self.types_[value_type_idx].tenum.as_ref()
                    .ok_or_else(|| anyhow!("EnumField: type {} is not an enum", value_type_idx))?;
                let construct_info = &tenum.constructs[construct.0];
                let offset = construct_info.offsets[field.0] as u64;

                let param_ptr = unsafe {
                    self.builder.build_gep(self.context.i8_type(), venum_ptr,
                        &[self.context.i64_type().const_int(offset, false)], "enumfield_gep")?
                };
                let val = self.builder.build_load(reg_types[dst.0 as usize], param_ptr, "enumfield_val")?;
                self.builder.build_store(registers[dst.0 as usize], val)?;
            }
            Opcode::SetEnumField { value, field, src } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let venum_ptr = self.builder.build_load(ptr_type, registers[value.0 as usize], "setenumfield_ptr")?.into_pointer_value();
                let src_val = self.builder.build_load(reg_types[src.0 as usize], registers[src.0 as usize], "setenumfield_val")?;

                // Scan backwards to find the preceding EnumAlloc targeting the same register
                let value_type_idx = f.regs[value.0 as usize].0;
                let tenum = self.types_[value_type_idx].tenum.as_ref()
                    .ok_or_else(|| anyhow!("SetEnumField: type {} is not an enum", value_type_idx))?;

                // Find construct index from preceding opcodes
                let mut construct_idx = 0usize; // default to 0
                for prev_i in (0..i).rev() {
                    match &f.ops[prev_i] {
                        Opcode::EnumAlloc { dst, construct } if dst.0 == value.0 => {
                            construct_idx = construct.0;
                            break;
                        }
                        Opcode::MakeEnum { dst, construct, .. } if dst.0 == value.0 => {
                            construct_idx = construct.0;
                            break;
                        }
                        _ => {}
                    }
                }

                let construct_info = &tenum.constructs[construct_idx];
                let offset = construct_info.offsets[field.0] as u64;
                let param_ptr = unsafe {
                    self.builder.build_gep(self.context.i8_type(), venum_ptr,
                        &[self.context.i64_type().const_int(offset, false)], "setenumfield_gep")?
                };
                self.builder.build_store(param_ptr, src_val)?;
            }

            // --- Memory access ---
            Opcode::GetI8 { dst, bytes, index } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let base = self.builder.build_load(ptr_type, registers[bytes.0 as usize], "geti8_base")?.into_pointer_value();
                let idx = self.builder.build_load(self.context.i32_type(), registers[index.0 as usize], "geti8_idx")?.into_int_value();
                let addr = unsafe { self.builder.build_gep(self.context.i8_type(), base, &[idx], "geti8_addr")? };
                let val = self.builder.build_load(self.context.i8_type(), addr, "geti8_val")?.into_int_value();
                let ext = self.builder.build_int_s_extend(val, self.context.i32_type(), "geti8_sext")?;
                self.builder.build_store(registers[dst.0 as usize], ext)?;
            }
            Opcode::GetI16 { dst, bytes, index } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let base = self.builder.build_load(ptr_type, registers[bytes.0 as usize], "geti16_base")?.into_pointer_value();
                let idx = self.builder.build_load(self.context.i32_type(), registers[index.0 as usize], "geti16_idx")?.into_int_value();
                let addr = unsafe { self.builder.build_gep(self.context.i8_type(), base, &[idx], "geti16_addr")? };
                let val = self.builder.build_load(self.context.i16_type(), addr, "geti16_val")?.into_int_value();
                let ext = self.builder.build_int_s_extend(val, self.context.i32_type(), "geti16_sext")?;
                self.builder.build_store(registers[dst.0 as usize], ext)?;
            }
            Opcode::GetMem { dst, bytes, index } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let base = self.builder.build_load(ptr_type, registers[bytes.0 as usize], "getmem_base")?.into_pointer_value();
                let idx = self.builder.build_load(self.context.i32_type(), registers[index.0 as usize], "getmem_idx")?.into_int_value();
                let addr = unsafe { self.builder.build_gep(self.context.i8_type(), base, &[idx], "getmem_addr")? };
                let val = self.builder.build_load(reg_types[dst.0 as usize], addr, "getmem_val")?;
                self.builder.build_store(registers[dst.0 as usize], val)?;
            }
            Opcode::SetI8 { bytes, index, src } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let base = self.builder.build_load(ptr_type, registers[bytes.0 as usize], "seti8_base")?.into_pointer_value();
                let idx = self.builder.build_load(self.context.i32_type(), registers[index.0 as usize], "seti8_idx")?.into_int_value();
                let src_val = self.builder.build_load(reg_types[src.0 as usize], registers[src.0 as usize], "seti8_src")?.into_int_value();
                let addr = unsafe { self.builder.build_gep(self.context.i8_type(), base, &[idx], "seti8_addr")? };
                let trunc = self.builder.build_int_truncate(src_val, self.context.i8_type(), "seti8_trunc")?;
                self.builder.build_store(addr, trunc)?;
            }
            Opcode::SetI16 { bytes, index, src } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let base = self.builder.build_load(ptr_type, registers[bytes.0 as usize], "seti16_base")?.into_pointer_value();
                let idx = self.builder.build_load(self.context.i32_type(), registers[index.0 as usize], "seti16_idx")?.into_int_value();
                let src_val = self.builder.build_load(reg_types[src.0 as usize], registers[src.0 as usize], "seti16_src")?.into_int_value();
                let addr = unsafe { self.builder.build_gep(self.context.i8_type(), base, &[idx], "seti16_addr")? };
                let trunc = self.builder.build_int_truncate(src_val, self.context.i16_type(), "seti16_trunc")?;
                self.builder.build_store(addr, trunc)?;
            }
            Opcode::SetMem { bytes, index, src } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let base = self.builder.build_load(ptr_type, registers[bytes.0 as usize], "setmem_base")?.into_pointer_value();
                let idx = self.builder.build_load(self.context.i32_type(), registers[index.0 as usize], "setmem_idx")?.into_int_value();
                let src_val = self.builder.build_load(reg_types[src.0 as usize], registers[src.0 as usize], "setmem_src")?;
                let addr = unsafe { self.builder.build_gep(self.context.i8_type(), base, &[idx], "setmem_addr")? };
                self.builder.build_store(addr, src_val)?;
            }

            // --- Array operations ---
            Opcode::SetArray { array, index, src } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let i32_type = self.context.i32_type();
                let i8_type = self.context.i8_type();

                let arr = self.builder.build_load(ptr_type, registers[array.0 as usize], "setarr_ptr")?.into_pointer_value();
                let idx = self.builder.build_load(i32_type, registers[index.0 as usize], "setarr_idx")?.into_int_value();
                let src_val = self.builder.build_load(reg_types[src.0 as usize], registers[src.0 as usize], "setarr_val")?;

                // Data starts at offset 24 (sizeof(varray))
                let data_ptr = unsafe {
                    self.builder.build_gep(i8_type, arr,
                        &[self.context.i64_type().const_int(24, false)], "setarr_data")?
                };

                // Element size from source type kind
                let src_type_idx = f.regs[src.0 as usize].0;
                let src_kind = self.types_[src_type_idx].kind;
                let elem_size: u64 = match src_kind {
                    hl_type_kind_HUI8 => 1,
                    hl_type_kind_HUI16 | hl_type_kind_HBOOL => 2,
                    hl_type_kind_HI32 | hl_type_kind_HF32 => 4,
                    hl_type_kind_HI64 | hl_type_kind_HF64 => 8,
                    _ => 8, // All pointer types = 8
                };

                let elem_size_val = i32_type.const_int(elem_size, false);
                let byte_offset = self.builder.build_int_mul(idx, elem_size_val, "setarr_offset")?;
                let slot = unsafe { self.builder.build_gep(i8_type, data_ptr, &[byte_offset], "setarr_slot")? };
                self.builder.build_store(slot, src_val)?;
            }
            Opcode::ArraySize { dst, array } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let arr = self.builder.build_load(ptr_type, registers[array.0 as usize], "arrsize_ptr")?.into_pointer_value();
                // varray.size is at offset 16
                let size_gep = unsafe {
                    self.builder.build_gep(self.context.i8_type(), arr,
                        &[self.context.i64_type().const_int(16, false)], "arrsize_gep")?
                };
                let size = self.builder.build_load(self.context.i32_type(), size_gep, "arrsize_val")?;
                self.builder.build_store(registers[dst.0 as usize], size)?;
            }

            // --- GetTID: get type kind ---
            Opcode::GetTID { dst, src } => {
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize], registers[src.0 as usize], "gettid_src")?;
                if src_val.is_pointer_value() {
                    // Runtime: load obj->t (offset 0), then t->kind (offset 0)
                    let ptr_type = self.context.ptr_type(AddressSpace::default());
                    let obj = src_val.into_pointer_value();
                    let t_ptr = self.builder.build_load(ptr_type, obj, "gettid_type")?.into_pointer_value();
                    let kind = self.builder.build_load(self.context.i32_type(), t_ptr, "gettid_kind")?;
                    self.builder.build_store(registers[dst.0 as usize], kind)?;
                } else {
                    // Compile-time: type kind is known
                    let type_idx = f.regs[src.0 as usize].0;
                    let kind = self.types_[type_idx].kind;
                    let kind_val = self.context.i32_type().const_int(kind as u64, false);
                    self.builder.build_store(registers[dst.0 as usize], kind_val)?;
                }
            }

            // --- Assert: unreachable ---
            Opcode::Assert => {
                self.builder.build_unreachable()?;
            }

            // --- Prefetch / Asm / RefData / RefOffset: stubs ---
            Opcode::Prefetch { .. } | Opcode::Asm { .. } => {}
            // --- RefData: extract value pointer from vdynamic (offset 8) ---
            Opcode::RefData { dst, src } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let obj = self.builder.build_load(ptr_type, registers[src.0 as usize], "refdata_src")?.into_pointer_value();
                let data_gep = unsafe {
                    self.builder.build_gep(self.context.i8_type(), obj,
                        &[self.context.i64_type().const_int(8, false)], "refdata_gep")?
                };
                let data = self.builder.build_load(ptr_type, data_gep, "refdata_val")?;
                self.builder.build_store(registers[dst.0 as usize], data)?;
            }
            // --- RefOffset: pointer + byte offset ---
            Opcode::RefOffset { dst, reg, offset } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let base = self.builder.build_load(ptr_type, registers[reg.0 as usize], "refoff_base")?.into_pointer_value();
                let off = self.builder.build_load(self.context.i32_type(), registers[offset.0 as usize], "refoff_off")?.into_int_value();
                let result = unsafe {
                    self.builder.build_gep(self.context.i8_type(), base, &[off], "refoff_result")?
                };
                self.builder.build_store(registers[dst.0 as usize], result)?;
            }

            _ => return Err(anyhow!("Opcode {:?} not yet implemented in JIT", op)),
        }
        Ok(())
    }

    /// Helper: emit a comparison jump (used by JSLt, JSGte, JEq, JNotEq, etc.)
    fn emit_comparison_jump(
        &self,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        a: &crate::opcodes::Reg,
        b: &crate::opcodes::Reg,
        int_pred: IntPredicate,
        float_pred: FloatPredicate,
        i: usize,
        offset: i32,
        opcode_blocks: &[BasicBlock<'ctx>],
    ) -> Result<()> {
        let a_val = self.builder.build_load(
            reg_types[a.0 as usize],
            registers[a.0 as usize],
            "cmp_a",
        )?;
        let b_val = self.builder.build_load(
            reg_types[b.0 as usize],
            registers[b.0 as usize],
            "cmp_b",
        )?;
        let cmp = match a_val.get_type().as_any_type_enum() {
            AnyTypeEnum::IntType(_) => self.builder.build_int_compare(
                int_pred, a_val.into_int_value(), b_val.into_int_value(), "cmp",
            )?,
            AnyTypeEnum::FloatType(_) => self.builder.build_float_compare(
                float_pred, a_val.into_float_value(), b_val.into_float_value(), "cmp",
            )?,
            AnyTypeEnum::PointerType(_) => {
                let a_int = self.builder.build_ptr_to_int(a_val.into_pointer_value(), self.context.i64_type(), "a_int")?;
                let b_int = self.builder.build_ptr_to_int(b_val.into_pointer_value(), self.context.i64_type(), "b_int")?;
                self.builder.build_int_compare(int_pred, a_int, b_int, "cmp")?
            }
            _ => return Err(anyhow!("Unsupported types for comparison jump")),
        };
        let target = opcode_blocks[(i as i32 + 1 + offset) as usize];
        let next = opcode_blocks[i + 1];
        self.builder.build_conditional_branch(cmp, target, next)?;
        Ok(())
    }

    /// Helper: emit a binary arithmetic operation
    fn emit_binary_op<F>(
        &self,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        dst: &crate::opcodes::Reg,
        a: &crate::opcodes::Reg,
        b: &crate::opcodes::Reg,
        _name: &str,
        op_fn: F,
    ) -> Result<()>
    where
        F: FnOnce(&Builder<'ctx>, BasicValueEnum<'ctx>, BasicValueEnum<'ctx>) -> Result<BasicValueEnum<'ctx>>,
    {
        let a_val = self.builder.build_load(
            reg_types[a.0 as usize],
            registers[a.0 as usize],
            "a_val",
        )?;
        let b_val = self.builder.build_load(
            reg_types[b.0 as usize],
            registers[b.0 as usize],
            "b_val",
        )?;
        let result = op_fn(&self.builder, a_val, b_val)?;
        self.builder.build_store(registers[dst.0 as usize], result)?;
        Ok(())
    }

    fn declare_native_function(
        &mut self,
        lib: &str,
        name: &str,
        native_func: &HLNative,
    ) -> Result<FunctionValue<'ctx>> {
        let type_fun = self.bytecode.types[native_func.type_.0]
            .fun
            .clone()
            .expect("expected to get function type");
        let func_type = self.create_function_type(&type_fun)?;
        Ok(self.module.add_function(name, func_type, None))
    }

    /// Generate a caller function that embeds the native function's address directly
    /// as an inttoptr constant, avoiding reliance on add_global_mapping symbol resolution.
    fn generate_native_caller_with_addr(
        &self,
        name: &str,
        fn_type: FunctionType<'ctx>,
        func_addr: usize,
    ) -> Result<FunctionValue<'ctx>> {
        let saved_block = self.builder.get_insert_block();

        let function = self.module.add_function(name, fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);

        // Embed the function address directly as inttoptr constant
        let addr_int = self.context.i64_type().const_int(func_addr as u64, false);
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let func_ptr = self.builder.build_int_to_ptr(addr_int, ptr_type, "fptr")?;

        let args: Vec<BasicMetadataValueEnum> =
            function.get_param_iter().map(|arg| arg.into()).collect();

        let call_site = self.builder.build_indirect_call(fn_type, func_ptr, &args, "call")?;

        if let Some(result) = call_site.try_as_basic_value().left() {
            self.builder.build_return(Some(&result))?;
        } else {
            self.builder.build_return(None)?;
        }

        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }

        Ok(function)
    }

    pub(crate) fn init_native_func(
        &mut self,
        native_func: &HLNative,
    ) -> Result<FunctionValue<'ctx>> {
        let lib = native_func.lib.as_str();
        let name: String = format!("hlp_{}", native_func.name);

        let type_fun = self.bytecode.types[native_func.type_.0]
            .fun
            .clone()
            .expect("expected to get function type");
        let func_type = self.create_function_type(&type_fun)?;

        let func_addr = self
            .native_function_resolver
            .resolve_function(lib, name.as_str())? as usize;

        let caller_name = format!("{}_{}_caller", lib, name);
        let native_caller = self.generate_native_caller_with_addr(&caller_name, func_type, func_addr)?;

        debug_assert!(native_caller.verify(true));

        Ok(native_caller)
    }

    // fn generate_std_lib_func(&mut self, lib: &str, name: &str) -> Result<FunctionValue<'ctx>> {
    //     let name: String = format!("hlp_{}", name);

    //     let func_value = self.declare_native_function(lib, name.as_str(), native_func)?;

    //     let func_ptr = self
    //         .native_function_resolver
    //         .resolve_function(lib, name.as_str())?;

    //     // Add function mapping
    //     self.execution_engine
    //         .add_global_mapping(&func_value, func_ptr as usize);

    //     let native_caller =
    //         self.generate_native_caller_function(&format!("{}_{}_caller", lib, name), func_value)?;

    //     debug_assert!(native_caller.verify(true));
    //     // println!("{}", native_caller.print_to_string().to_string());

    //     Ok(native_caller)
    // }

    fn get_native_func(&self, native: &HLNative) -> Result<&FunctionValue<'ctx>> {
        if let Some(func) = self.func_cache.get(&(native.findex as usize)) {
            return Ok(func);
        }

        Err(anyhow!(
            "Native function not found '{}::{}'",
            native.lib,
            native.name
        ))
    }

    /// Create stub functions for all bytecode functions not yet compiled.
    /// These stubs simply return zero/null and ensure functions_ptrs has
    /// valid addresses for all function indices (needed by binding mechanism).
    fn create_stubs_for_uncompiled_functions(&mut self) -> Result<()> {
        let findexes_clone: Vec<(usize, FuncPtr)> = self.findexes.iter()
            .map(|(&k, v)| (k, v.clone()))
            .collect();
        let saved_block = self.builder.get_insert_block();

        for (findex, func_ptr) in findexes_clone {
            if self.func_cache.contains_key(&findex) {
                continue; // Already compiled
            }
            if let FuncPtr::Fun(f) = func_ptr {
                // Create a declaration with a valid stub body
                match self.create_function_declaration(&f) {
                    Ok(decl) => {
                        let stub_block = self.context.append_basic_block(decl, "stub");
                        self.builder.position_at_end(stub_block);
                        let ret_type = decl.get_type().get_return_type();
                        if let Some(ret_type) = ret_type {
                            self.builder.build_return(Some(&ret_type.const_zero())).ok();
                        } else {
                            self.builder.build_return(None).ok();
                        }
                        self.func_cache.insert(findex, decl);
                    }
                    Err(_) => {} // Skip types we can't handle
                }
            }
        }

        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }
        Ok(())
    }

    pub fn execute_main(&mut self) -> Result<()> {
        // Compile any pending functions discovered during initialization
        self.compile_pending_functions()?;

        // Create stubs for uncompiled functions so bindings can resolve them
        self.create_stubs_for_uncompiled_functions()?;

        let index = self.bytecode.entrypoint as usize;
        let function = *self
            .func_cache
            .get(&index)
            .ok_or_else(|| anyhow!("Entrypoint function not found in cache"))?;

        self.module.print_to_file("/tmp/ash_jit.ll").ok();

        // Populate functions_ptrs with actual function addresses from the JIT.
        // This must happen after compilation so the execution engine has allocated code.
        self.setup_functions_ptrs()?;

        // Register GC roots BEFORE init_constants (which allocates and might trigger GC)
        unsafe {
            type FnSetGlobals = unsafe extern "C" fn(*const *mut std::ffi::c_void, usize);
            let set_globals: FnSetGlobals = std::mem::transmute(
                self.native_function_resolver
                    .resolve_function("std", "hlp_gc_set_globals")
                    .map_err(|e| anyhow!("Cannot resolve hlp_gc_set_globals: {}", e))?,
            );
            set_globals(self.globals_data.as_ptr(), self.globals_data.len());

            type FnSetStackTop = unsafe extern "C" fn(usize);
            let set_stack_top: FnSetStackTop = std::mem::transmute(
                self.native_function_resolver
                    .resolve_function("std", "hlp_gc_set_stack_top")
                    .map_err(|e| anyhow!("Cannot resolve hlp_gc_set_stack_top: {}", e))?,
            );
            let stack_top: usize;
            core::arch::asm!("mov {}, sp", out(reg) stack_top);
            set_stack_top(stack_top);
        }

        // Materialize bytecode constants (pre-initialized globals like string literals)
        self.init_constants()?;

        unsafe {
            self.execution_engine.run_function(function, &[]);
        };

        Ok(())
    }


    /// Populate the functions_ptrs table with actual function addresses.
    /// The table was pre-allocated in init_indexes and already wired into module contexts.
    fn setup_functions_ptrs(&mut self) -> Result<()> {
        // Collect function names and findexes first to avoid borrow conflicts
        let func_entries: Vec<(usize, String)> = self.func_cache.iter()
            .map(|(&findex, func_val)| {
                (findex, func_val.get_name().to_str().unwrap_or("").to_string())
            })
            .collect();

        let mut count = 0;
        for (findex, name) in &func_entries {
            if let Ok(addr) = self.execution_engine.get_function_address(name) {
                if addr != 0 && *findex < self.functions_ptrs.len() {
                    self.functions_ptrs[*findex] = addr as *mut c_void;
                    count += 1;
                }
            }
        }

        Ok(())
    }
}

pub struct FunctionBuilder<'ctx> {
    pub(crate) builder: Builder<'ctx>,
    pub(crate) execution_engine: ExecutionEngine<'ctx>,
    pub(crate) type_: Option<FunctionType<'ctx>>,
    pub(crate) value: Option<FunctionValue<'ctx>>,
    fun: HLFunction,
}

impl<'ctx> FunctionBuilder<'ctx> {
    pub fn new(
        fun: HLFunction,
        builder: Builder<'ctx>,
        execution_engine: ExecutionEngine<'ctx>,
    ) -> Self {
        Self {
            builder,
            execution_engine,
            fun,
            type_: None,
            value: None,
        }
    }

    pub fn build(&mut self, module: &mut JITModule<'ctx>) -> Result<()> {
        let types = module.types_.clone();
        let regs = &self.fun.regs;
        let type_ = types.get(self.fun.type_.0).expect("Unknown type");
        let fun = type_.fun.as_ref().expect("Expected to get function type");
        self.type_ = module.create_function_type(fun).ok();
        self.value = module.create_function_value(self.fun.findex as usize).ok();
        Ok(())
    }
}
