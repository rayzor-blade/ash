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

        if let FuncPtr::Fun(f) = fun_ptr {
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
                let function = self.create_function_declaration(f)?;
                let basic_block = self.context.append_basic_block(function, "entry");
                self.builder.position_at_end(basic_block);

                let (registers, reg_types) = self.allocate_registers(f)?;
                self.load_function_arguments(f, &function, &registers)?;
                self.translate_opcodes(f, &registers, &reg_types)?;

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
                    (AnyTypeEnum::FloatType(_), AnyTypeEnum::FloatType(_)) => self
                        .builder
                        .build_float_add(a_val.into_float_value(), b_val.into_float_value(), "add")?
                        .as_any_value_enum()
                        .into_float_value()
                        .as_basic_value_enum(),
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
                    (AnyTypeEnum::FloatType(_), AnyTypeEnum::FloatType(_)) => self
                        .builder
                        .build_float_sub(a_val.into_float_value(), b_val.into_float_value(), "sub")?
                        .as_basic_value_enum(),
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
                    (AnyTypeEnum::FloatType(_), AnyTypeEnum::FloatType(_)) => self
                        .builder
                        .build_float_mul(a_val.into_float_value(), b_val.into_float_value(), "mul")?
                        .as_basic_value_enum(),
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
                // Load the array pointer
                let array_ptr = self.builder.build_load(
                    reg_types[array.0 as usize],
                    registers[array.0 as usize],
                    "array_ptr",
                )?;

                // Load the index
                let index_val = self.builder.build_load(
                    reg_types[index.0 as usize],
                    registers[index.0 as usize],
                    "index_val",
                )?;

                // Get the element pointer
                let element_ptr = unsafe {
                    self.builder.build_gep(
                        reg_types[array.0 as usize],
                        registers[array.0 as usize],
                        &[index_val.into_int_value()],
                        "element_ptr",
                    )?
                };

                // Load the element
                let element_val = self.builder.build_load(
                    element_ptr.get_type(),
                    element_ptr,
                    "element_val",
                )?;

                // Store the element in the destination register
                self.builder
                    .build_store(registers[dst.0 as usize], element_val)?;
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
                        (AnyTypeEnum::FloatType(_), AnyTypeEnum::FloatType(_)) =>
                            Ok(b.build_float_div(av.into_float_value(), bv.into_float_value(), "sdiv")?.as_basic_value_enum()),
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
                        (AnyTypeEnum::FloatType(_), AnyTypeEnum::FloatType(_)) =>
                            Ok(b.build_float_rem(av.into_float_value(), bv.into_float_value(), "smod")?.as_basic_value_enum()),
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
                let src_type = &self.types_[src_type_idx];
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "todyn_src",
                )?;
                // For pointer types (objects, strings, etc.), just copy the pointer
                if src_val.is_pointer_value() {
                    self.builder.build_store(registers[dst.0 as usize], src_val)?;
                } else {
                    // For primitives, store value in dst register directly
                    // Full boxing via hlp_make_dyn will be added in Phase 3
                    self.builder.build_store(registers[dst.0 as usize], src_val)?;
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
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "safecast_src",
                )?;
                self.builder.build_store(registers[dst.0 as usize], src_val)?;
            }

            // --- ToVirtual: copy pointer (virtual wrapping deferred) ---
            Opcode::ToVirtual { dst, src } => {
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "tovirt_src",
                )?;
                self.builder.build_store(registers[dst.0 as usize], src_val)?;
            }

            // --- Trap / EndTrap: no-op stubs for now (exception handling deferred) ---
            Opcode::Trap { exc: _, offset } => {
                // Jump to handler block on exception (stubbed: never traps)
                let _ = offset;
            }
            Opcode::EndTrap { exc: _ } => {
                // Remove trap context (stubbed: no-op)
            }

            // --- Throw / Rethrow: emit unreachable for now ---
            Opcode::Throw { exc: _ } | Opcode::Rethrow { exc: _ } => {
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

            // --- InstanceClosure: store function pointer (binding deferred) ---
            Opcode::InstanceClosure { dst, fun, obj: _ } => {
                let (function, is_placeholder) = self.get_or_create_function_value(fun.0)?;
                let fn_ptr = function.as_global_value().as_pointer_value();
                self.builder.build_store(registers[dst.0 as usize], fn_ptr)?;
                if is_placeholder {
                    self.add_pending_compilation(fun.0);
                }
            }

            // --- VirtualClosure: store function pointer (virtual dispatch deferred) ---
            Opcode::VirtualClosure { dst, obj: _, field: _ } => {
                let null_ptr = self.context.ptr_type(AddressSpace::default()).const_null();
                self.builder.build_store(registers[dst.0 as usize], null_ptr)?;
            }

            // --- DynGet: dynamic field access (stub: store null) ---
            Opcode::DynGet { dst, obj: _, field: _ } => {
                let null_ptr = self.context.ptr_type(AddressSpace::default()).const_null();
                self.builder.build_store(registers[dst.0 as usize], null_ptr)?;
            }

            // --- DynSet: dynamic field set (stub: no-op) ---
            Opcode::DynSet { obj: _, field: _, src: _ } => {
                // Stubbed for Phase 1
            }

            // --- Bytes: load bytes constant ---
            Opcode::Bytes { dst, ptr: _ } => {
                let null_ptr = self.context.ptr_type(AddressSpace::default()).const_null();
                self.builder.build_store(registers[dst.0 as usize], null_ptr)?;
            }

            // --- Enum opcodes (stubs) ---
            Opcode::MakeEnum { dst, construct: _, args: _ } => {
                let null_ptr = self.context.ptr_type(AddressSpace::default()).const_null();
                self.builder.build_store(registers[dst.0 as usize], null_ptr)?;
            }
            Opcode::EnumAlloc { dst, construct: _ } => {
                let null_ptr = self.context.ptr_type(AddressSpace::default()).const_null();
                self.builder.build_store(registers[dst.0 as usize], null_ptr)?;
            }
            Opcode::EnumIndex { dst, value: _ } => {
                self.builder.build_store(
                    registers[dst.0 as usize],
                    self.context.i32_type().const_zero(),
                )?;
            }
            Opcode::EnumField { dst, value: _, construct: _, field: _ } => {
                let null_ptr = self.context.ptr_type(AddressSpace::default()).const_null();
                self.builder.build_store(registers[dst.0 as usize], null_ptr)?;
            }
            Opcode::SetEnumField { value: _, field: _, src: _ } => {
                // Stubbed
            }

            // --- Memory access stubs ---
            Opcode::GetI8 { dst, bytes: _, index: _ } => {
                self.builder.build_store(registers[dst.0 as usize], self.context.i32_type().const_zero())?;
            }
            Opcode::GetI16 { dst, bytes: _, index: _ } => {
                self.builder.build_store(registers[dst.0 as usize], self.context.i32_type().const_zero())?;
            }
            Opcode::GetMem { dst, bytes: _, index: _ } => {
                let null_ptr = self.context.ptr_type(AddressSpace::default()).const_null();
                self.builder.build_store(registers[dst.0 as usize], null_ptr)?;
            }
            Opcode::SetI8 { bytes: _, index: _, src: _ } => {}
            Opcode::SetI16 { bytes: _, index: _, src: _ } => {}
            Opcode::SetMem { bytes: _, index: _, src: _ } => {}
            Opcode::SetArray { array: _, index: _, src: _ } => {}
            Opcode::ArraySize { dst, array: _ } => {
                self.builder.build_store(registers[dst.0 as usize], self.context.i32_type().const_zero())?;
            }

            // --- GetTID ---
            Opcode::GetTID { dst, src: _ } => {
                self.builder.build_store(registers[dst.0 as usize], self.context.i32_type().const_zero())?;
            }

            // --- Assert: unreachable ---
            Opcode::Assert => {
                self.builder.build_unreachable()?;
            }

            // --- Prefetch / Asm / RefData / RefOffset: stubs ---
            Opcode::Prefetch { .. } | Opcode::Asm { .. } => {}
            Opcode::RefData { dst, src: _ } | Opcode::RefOffset { dst, reg: _, offset: _ } => {
                let null_ptr = self.context.ptr_type(AddressSpace::default()).const_null();
                self.builder.build_store(registers[dst.0 as usize], null_ptr)?;
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
