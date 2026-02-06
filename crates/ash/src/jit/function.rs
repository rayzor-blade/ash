use std::ffi::c_void;

use ash_macro::to_llvm;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::types::{
    AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType,
};
use inkwell::values::{
    AnyValue, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue,
};
use inkwell::{basic_block::BasicBlock, builder::Builder, AddressSpace};

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

        let mut param_types: Vec<BasicMetadataTypeEnum<'ctx>> = type_fun
            .args
            .iter()
            .map(|arg| {
                let arg_type = self.get_or_create_any_type(arg.0).unwrap();
                (match arg_type {
                    AnyTypeEnum::ArrayType(t) => t.as_basic_type_enum(),
                    AnyTypeEnum::FloatType(t) => t.as_basic_type_enum(),
                    AnyTypeEnum::FunctionType(t) => {
                        self.context.ptr_type(AddressSpace::default()).into()
                    }
                    AnyTypeEnum::IntType(t) => t.as_basic_type_enum(),
                    AnyTypeEnum::PointerType(t) => t.as_basic_type_enum(),
                    AnyTypeEnum::StructType(t) => t.as_basic_type_enum(),
                    AnyTypeEnum::VectorType(t) => t.as_basic_type_enum(),
                    AnyTypeEnum::VoidType(t) => self
                        .context
                        .opaque_struct_type("haxe.Void")
                        .as_basic_type_enum(),
                })
                .into()
            })
            .collect();

        param_types.insert(0, self.context.ptr_type(AddressSpace::default()).into());

        let function_type = match ret_type {
            AnyTypeEnum::ArrayType(t) => t.fn_type(&param_types, false),
            AnyTypeEnum::FloatType(t) => t.fn_type(&param_types, false),
            AnyTypeEnum::FunctionType(t) => self
                .context
                .ptr_type(AddressSpace::default())
                .fn_type(&param_types, false),
            AnyTypeEnum::IntType(t) => t.fn_type(&param_types, false),
            AnyTypeEnum::PointerType(t) => t.fn_type(&param_types, false),
            AnyTypeEnum::StructType(t) => t.fn_type(&param_types, false),
            AnyTypeEnum::VectorType(t) => t.fn_type(&param_types, false),
            AnyTypeEnum::VoidType(t) => self.context.void_type().fn_type(&param_types, false),
        };

        Ok(function_type)
    }

    fn add_pending_compilation(&mut self, index: usize) {
        self.pending_compilations.push(index);
    }

    fn compile_pending_functions(&mut self) -> Result<()> {
        while let Some(index) = self.pending_compilations.pop() {
            self.compile_function(index)?;
        }
        Ok(())
    }

    pub(crate) fn compile_function(&mut self, index: usize) -> Result<()> {
        let fun_ptr = self
            .findexes
            .get(&index)
            .ok_or_else(|| anyhow!("Function not found at index {}", index))?
            .clone();

        if let FuncPtr::Fun(f) = fun_ptr {
            let function = self.create_function_value(index)?;
            let basic_block = self.context.append_basic_block(function, "entry");
            let builder = self.context.create_builder();
            builder.position_at_end(basic_block);

            let registers = self.allocate_registers(&f, &builder)?;
            self.load_function_arguments(&f, &function, &registers, &builder)?;

            self.translate_opcodes(&f, &registers, &builder)?;

            if !builder
                .get_insert_block()
                .unwrap()
                .get_terminator()
                .is_some()
            {
                builder.build_return(None);
            }

            debug_assert!(function.verify(true));

            // Replace the placeholder with the actual function
            if self.func_cache.contains_key(&index) {
                let placeholder = self.func_cache.remove(&index).unwrap();
                placeholder.replace_all_uses_with(function);
            }
            self.func_cache.insert(index, function);
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
                let builder = self.context.create_builder();
                builder.position_at_end(basic_block);

                let registers = self.allocate_registers(f, &builder)?;
                self.load_function_arguments(f, &function, &registers, &builder)?;

                self.translate_opcodes(f, &registers, &builder)?;

                // Add a return instruction if the last opcode wasn't already a return
                if !builder
                    .get_insert_block()
                    .unwrap()
                    .get_terminator()
                    .is_some()
                {
                    builder.build_return(None);
                }

                debug_assert!(function.verify(true));

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
        builder: &Builder<'ctx>,
    ) -> Result<()> {
        let reg_array_ptr = function.get_nth_param(0).unwrap().into_pointer_value();
        let fun_type = self.bytecode.types[f.type_.0]
            .fun
            .as_ref()
            .expect("expected function type");
        let args_count = fun_type.args.len();

        for i in 0..args_count {
            let arg_ptr = unsafe {
                builder.build_gep(
                    self.context.i8_type(),
                    reg_array_ptr,
                    &[self.context.i32_type().const_int(i as u64, false)],
                    &format!("arg_ptr_{}", i),
                )?
            };

            let arg_value =
                builder.build_load(arg_ptr.get_type(), arg_ptr, &format!("arg_{}", i))?;
            builder.build_store(registers[i], arg_value);
        }

        Ok(())
    }

    fn allocate_registers(
        &mut self,
        f: &HLFunction,
        builder: &Builder<'ctx>,
    ) -> Result<Vec<PointerValue<'ctx>>> {
        f.regs
            .iter()
            .enumerate()
            .map(|(i, reg)| {
                let reg_type = self
                    .get_register_type(reg.0)
                    .expect("expected to get register type");
                Ok(builder.build_alloca(reg_type, &format!("reg_{}", i))?)
            })
            .collect()
    }

    fn get_register_type(&mut self, type_index: usize) -> Result<BasicTypeEnum<'ctx>> {
        Ok(match self.get_or_create_any_type(type_index)? {
            AnyTypeEnum::ArrayType(t) => t.as_basic_type_enum(),
            AnyTypeEnum::FloatType(t) => t.as_basic_type_enum(),
            AnyTypeEnum::FunctionType(_) => self
                .context
                .ptr_type(AddressSpace::default())
                .as_basic_type_enum(),
            AnyTypeEnum::IntType(t) => t.as_basic_type_enum(),
            AnyTypeEnum::PointerType(t) => t.as_basic_type_enum(),
            AnyTypeEnum::StructType(t) => t.as_basic_type_enum(),
            AnyTypeEnum::VectorType(t) => t.as_basic_type_enum(),
            AnyTypeEnum::VoidType(_) => self
                .context
                .opaque_struct_type("haxe.Void")
                .as_basic_type_enum(),
        })
    }

    fn get_initialized_type(&mut self, type_index: usize) -> Result<BasicValueEnum<'ctx>> {
        if let Some(type_) = self.initialized_type_cache.get(&type_index) {
            return Ok(*type_);
        }
        let kind = self.types_[type_index].clone().kind;
        if kind > hl_type_kind_HDYN {
            return Ok(*self
                .initialized_type_cache
                .get(&type_index)
                .expect("Expected to get type"));
        }

        let _hl_type = self.get_hl_type_struct_type()?;
        let ptr_value = self.context.ptr_type(AddressSpace::default()).const_null();
        let type_value = _hl_type.const_named_struct(&[
            self.context.i32_type().const_int(kind as u64, false).into(),
            ptr_value.into(), // This replaces the union
            ptr_value.into(), // vobj_proto
            ptr_value.into(), // mark_bits
        ]);

        self.initialized_type_cache
            .insert(type_index, type_value.into());

        Ok(type_value.into())
    }

    fn translate_opcodes(
        &mut self,
        f: &HLFunction,
        registers: &[PointerValue<'ctx>],
        builder: &Builder<'ctx>,
    ) -> Result<()> {
        let function = builder.get_insert_block().unwrap().get_parent().unwrap();
        let mut current_block = builder.get_insert_block().unwrap();

        for (i, op) in f.ops.iter().enumerate() {
            builder.position_at_end(current_block);

            // Create a new block for the next instruction
            let next_block = self
                .context
                .append_basic_block(function, &format!("block_{}", i + 1));

            self.translate_opcode(f, op, registers, next_block)?;

            // If the current block doesn't have a terminator, create an unconditional branch to the next block
            if current_block.get_terminator().is_none() {
                builder.build_unconditional_branch(next_block);
            }

            current_block = next_block;
        }

        Ok(())
    }

    fn translate_opcode(
        &mut self,
        f: &HLFunction,
        op: &Opcode,
        registers: &[PointerValue<'ctx>],
        next_block: BasicBlock<'ctx>,
    ) -> Result<()> {
        match op {
            Opcode::Mov { dst, src } => {
                let src_val = self.builder.build_load(
                    registers[src.0 as usize].get_type(),
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
                let loaded_string = self.builder.build_load(
                    self.context.ptr_type(AddressSpace::default()),
                    string_val.as_pointer_value(),
                    "string_val",
                )?;
                self.builder
                    .build_store(registers[dst.0 as usize], loaded_string);
            }
            Opcode::Null { dst } => {
                let null_val = registers[dst.0 as usize].get_type().const_null();
                self.builder
                    .build_store(registers[dst.0 as usize], null_val);
            }
            Opcode::Add { dst, a, b } => {
                let a_val = self.builder.build_load(
                    registers[a.0 as usize].get_type(),
                    registers[a.0 as usize],
                    "a_val",
                )?;
                let b_val = self.builder.build_load(
                    registers[b.0 as usize].get_type(),
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
                    registers[a.0 as usize].get_type(),
                    registers[a.0 as usize],
                    "a_val",
                )?;
                let b_val = self.builder.build_load(
                    registers[b.0 as usize].get_type(),
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
                    registers[a.0 as usize].get_type(),
                    registers[a.0 as usize],
                    "a_val",
                )?;
                let b_val = self.builder.build_load(
                    registers[b.0 as usize].get_type(),
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
                    registers[arg0.0 as usize].get_type(),
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
                    registers[arg0.0 as usize].get_type(),
                    registers[arg0.0 as usize],
                    "arg0_val",
                )?;
                let arg1_val = self.builder.build_load(
                    registers[arg1.0 as usize].get_type(),
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
                    registers[arg0.0 as usize].get_type(),
                    registers[arg0.0 as usize],
                    "arg0_val",
                )?;
                let arg1_val = self.builder.build_load(
                    registers[arg1.0 as usize].get_type(),
                    registers[arg1.0 as usize],
                    "arg1_val",
                )?;
                let arg2_val = self.builder.build_load(
                    registers[arg2.0 as usize].get_type(),
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
                    registers[ret.0 as usize].get_type(),
                    registers[ret.0 as usize],
                    "ret_val",
                )?;
                self.builder.build_return(Some(&ret_val));
            }
            Opcode::JTrue { cond, offset } => {
                let cond_val = self.builder.build_load(
                    registers[cond.0 as usize].get_type(),
                    registers[cond.0 as usize],
                    "cond_val",
                )?;
                let jump_block = self.context.append_basic_block(
                    self.builder
                        .get_insert_block()
                        .unwrap()
                        .get_parent()
                        .unwrap(),
                    "jump_block",
                );
                self.builder.build_conditional_branch(
                    cond_val.into_int_value(),
                    jump_block,
                    next_block,
                );
                self.builder.position_at_end(jump_block);
                // You might need to implement a way to calculate the actual jump target based on the offset
            }
            Opcode::JFalse { cond, offset } => {
                let cond_val = self.builder.build_load(
                    registers[cond.0 as usize].get_type(),
                    registers[cond.0 as usize],
                    "cond_val",
                )?;
                let jump_block = self.context.append_basic_block(
                    self.builder
                        .get_insert_block()
                        .unwrap()
                        .get_parent()
                        .unwrap(),
                    "jump_block",
                );
                self.builder.build_conditional_branch(
                    cond_val.into_int_value(),
                    next_block,
                    jump_block,
                );
                self.builder.position_at_end(jump_block);
                // You might need to implement a way to calculate the actual jump target based on the offset
            }
            Opcode::JAlways { offset } => {
                let jump_block = self.context.append_basic_block(
                    self.builder
                        .get_insert_block()
                        .unwrap()
                        .get_parent()
                        .unwrap(),
                    "jump_block",
                );
                self.builder.build_unconditional_branch(jump_block);
                self.builder.position_at_end(jump_block);
                // You might need to implement a way to calculate the actual jump target based on the offset
            }
            Opcode::GetType { dst, src } => {
                // Load the source value
                let src_val = self.builder.build_load(
                    registers[src.0 as usize].get_type(),
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

                        let type_value = type_.into_pointer_value();
                        let arg0_val = self.builder.build_load(
                            type_value.get_type(),
                            type_value,
                            "arg0_val",
                        )?;
                        let result = self.builder.build_call(*fun, &[arg0_val.into()], "call")?;
                        self.builder.build_store(
                            registers[dst.0 as usize],
                            result.try_as_basic_value().left().unwrap(),
                        );
                    }
                    hl_type_kind_HDYNOBJ => {
                        let fun = hlp_alloc_dynobj_to_llvm(&self.context, &self.module, &self.builder)?;

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
                        let fun = hlp_alloc_virtual_to_llvm(&self.context, &self.module, &self.builder)?;

                        let type_value = type_.into_pointer_value();
                        let arg0_val = self.builder.build_load(
                            type_value.get_type(),
                            type_value,
                            "arg0_val",
                        )?;
                        let result = self.builder.build_call(fun, &[arg0_val.into()], "call")?;
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
                        registers[obj.0 as usize].get_type(),
                        registers[obj.0 as usize],
                        "obj_val",
                    )?
                    .into_pointer_value();
                let src_val = self.builder.build_load(
                    registers[src.0 as usize].get_type(),
                    registers[src.0 as usize],
                    "src_val",
                )?;

                let obj_type = obj_val.get_type();
                match obj_type_.kind {
                    hl_type_kind_HSTRUCT | hl_type_kind_HOBJ => {
                        // Call hl_get_obj_rt
                        let hl_get_obj_rt = hlp_get_obj_rt_to_llvm(&self.context, &self.module, &self.builder)?; 

                        let rt_obj = self
                            .builder
                            .build_call(hl_get_obj_rt, &[obj_val.into()], "rt_obj")?
                            .try_as_basic_value()
                            .left()
                            .unwrap();

                        // Get the fields_indexes pointer from rt_obj
                        let fields_indexes_ptr = unsafe {
                            self.builder.build_struct_gep(
                                rt_obj.get_type().into_struct_type(),
                                rt_obj.into_pointer_value(),
                                10, // fields_indexes is the 11th field (index 10)
                                "fields_indexes_ptr",
                            )?
                        };
                        let fields_indexes_ptr = self.builder.build_load(
                            fields_indexes_ptr.get_type(),
                            fields_indexes_ptr,
                            "fields_indexes",
                        )?;

                        // Get the field index
                        let field_index_ptr = unsafe {
                            self.builder.build_gep(
                                fields_indexes_ptr.get_type().into_pointer_type(),
                                fields_indexes_ptr.into_pointer_value(),
                                &[self.context.i32_type().const_int(field.0 as u64, false)],
                                "field_index_ptr",
                            )?
                        };
                        let field_index = self.builder.build_load(
                            self.context.i32_type(),
                            field_index_ptr,
                            "field_index",
                        )?;

                        // Get the field pointer
                        let field_ptr = unsafe {
                            self.builder.build_gep(
                                obj_val.get_type(),
                                obj_val,
                                &[self.context.i32_type().const_zero(), field_index.into_int_value()],
                                "field_ptr",
                            )?
                        };

                        // Store the new value
                        self.builder.build_store(field_ptr, src_val)?;
                    }
                    hl_type_kind_HVIRTUAL => {
                        // Get the value pointer (second field of vvirtual)
                        let value_ptr = unsafe {
                            self.builder
                                .build_struct_gep(obj_type, obj_val, 1, "value_ptr")?
                        };
                        let value_ptr =
                            self.builder
                                .build_load(value_ptr.get_type(), value_ptr, "value")?;

                        // Get the vfields pointer (value + 1)
                        let vfields_ptr = unsafe {
                            self.builder.build_gep(
                                value_ptr.into_pointer_value().get_type(),
                                value_ptr.into_pointer_value(),
                                &[self.context.i32_type().const_int(1, false)],
                                "vfields_ptr",
                            )?
                        };

                        // Get the field pointer
                        let field_ptr = unsafe {
                            self.builder.build_gep(
                                vfields_ptr.get_type(),
                                vfields_ptr,
                                &[self.context.i32_type().const_int(field.0 as u64, false)],
                                "field_ptr",
                            )?
                        };

                        // Check if the field exists
                        let field_value_ptr = self.builder.build_load(
                            field_ptr.get_type(),
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
                        let hl_dyn_set = hlp_get_dynset_to_llvm(&self.context, &self.module, &self.builder)?;
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
                    registers[obj.0 as usize].get_type(),
                    registers[obj.0 as usize],
                    "obj_val",
                )?;
                let obj_type = registers[obj.0 as usize].get_type();
                match obj_type_.kind {
                    hl_type_kind_HSTRUCT | hl_type_kind_HOBJ => {
                        // Call hl_get_obj_rt
                        let hl_get_obj_rt = hlp_get_obj_rt_to_llvm(&self.context, &self.module, &self.builder)?;
                        let rt_obj = self
                            .builder
                            .build_call(hl_get_obj_rt, &[registers[obj.0 as usize].into()], "rt_obj")?
                            .try_as_basic_value()
                            .left()
                            .unwrap();

                        // Call hl_obj_field_fetch
                        let hl_obj_field_fetch = hlp_obj_field_fetch_to_llvm(&self.context, &self.module, &self.builder)?;
                        let field_info = self
                            .builder
                            .build_call(
                                hl_obj_field_fetch,
                                &[
                                    registers[obj.0 as usize].into(),
                                    self.context
                                        .i32_type()
                                        .const_int(field.0 as u64, false)
                                        .into(),
                                ],
                                "field_info",
                            )?
                            .try_as_basic_value()
                            .left()
                            .unwrap();

                        // Get the field type
                        let field_type_ptr = unsafe {
                            self.builder.build_struct_gep(
                                field_info.get_type().into_pointer_type(),
                                field_info.into_pointer_value(),
                                1, // t is the second field (index 1)
                                "field_type_ptr",
                            )?
                        };
                        let field_type = self.builder.build_load(
                            field_type_ptr.get_type(),
                            field_type_ptr,
                            "field_type",
                        )?;

                        // Get the fields_indexes pointer from rt_obj
                        let fields_indexes_ptr = unsafe {
                            self.builder.build_struct_gep(
                                rt_obj.get_type().into_struct_type(),
                                rt_obj.into_pointer_value(),
                                10, // fields_indexes is the 11th field (index 10)
                                "fields_indexes_ptr",
                            )?
                        };
                        let fields_indexes_ptr = self.builder.build_load(
                            fields_indexes_ptr.get_type(),
                            fields_indexes_ptr,
                            "fields_indexes",
                        )?;

                        // Get the field index
                        let field_index_ptr = unsafe {
                            self.builder.build_gep(
                                fields_indexes_ptr.get_type().into_pointer_type(),
                                fields_indexes_ptr.into_pointer_value(),
                                &[self.context.i32_type().const_int(field.0 as u64, false)],
                                "field_index_ptr",
                            )?
                        };
                        let field_index = self.builder.build_load(
                            self.context.i32_type(),
                            field_index_ptr,
                            "field_index",
                        )?;

                        // Get the field pointer
                        let field_ptr = unsafe {
                            self.builder.build_gep(
                                registers[obj.0 as usize].get_type(),
                                registers[obj.0 as usize],
                                &[self.context.i32_type().const_zero(), field_index.into_int_value()],
                                "field_ptr",
                            )?
                        };

                        // Load the field value
                        let field_val = self.builder.build_load(
                            field_type.get_type().into_pointer_type(),
                            field_ptr,
                            "field_val",
                        )?;
                        self.builder
                            .build_store(registers[dst.0 as usize], field_val)?;
                    }
                    hl_type_kind_HVIRTUAL => {
                        // Get the value pointer (second field of vvirtual)
                        let value_ptr = unsafe {
                            self.builder
                                .build_struct_gep(obj_type, registers[obj.0 as usize], 1, "value_ptr")?
                        };
                        let value_ptr =
                            self.builder
                                .build_load(value_ptr.get_type(), value_ptr, "value")?;

                        // Get the vfields pointer (value + 1)
                        let vfields_ptr = unsafe {
                            self.builder.build_gep(
                                value_ptr.get_type(),
                                value_ptr.into_pointer_value(),
                                &[self.context.i32_type().const_int(1, false)],
                                "vfields_ptr",
                            )?
                        };

                        // Get the field pointer
                        let field_ptr = unsafe {
                            self.builder.build_gep(
                                vfields_ptr.get_type(),
                                vfields_ptr,
                                &[self.context.i32_type().const_int(field.0 as u64, false)],
                                "field_ptr",
                            )?
                        };

                        // Check if the field exists
                        let field_value_check = self.builder.build_load(
                            field_ptr.get_type(),
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
                        let hl_dyn_get = hlp_get_dynget_to_llvm(&self.context, &self.module, &self.builder)?;
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
                let global_val = self
                    .globals
                    .get(&global.0)
                    .expect("Expected to get global value");

                self.builder
                    .build_store(registers[dst.0 as usize], global_val.as_basic_value_enum());
            }
            Opcode::SetGlobal { global, src } => {
                let global_val = self
                    .globals
                    .get(&global.0)
                    .expect("Expected to get global value");

                self.builder.build_store(
                    global_val.as_pointer_value(),
                    registers[src.0 as usize].as_basic_value_enum(),
                );
            }
            Opcode::GetArray { dst, array, index } => {
                // Load the array pointer
                let array_ptr = self.builder.build_load(
                    registers[array.0 as usize].get_type(),
                    registers[array.0 as usize],
                    "array_ptr",
                )?;

                // Load the index
                let index_val = self.builder.build_load(
                    registers[index.0 as usize].get_type(),
                    registers[index.0 as usize],
                    "index_val",
                )?;

                // Get the element pointer
                let element_ptr = unsafe {
                    self.builder.build_gep(
                        registers[array.0 as usize].get_type(),
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

            // Add more opcode translations here...
            _ => return Err(anyhow!("Opcode {:?} not yet implemented", op)),
        }
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

    fn generate_native_caller_function(
        &self,
        name: &str,
        target_fn: FunctionValue<'ctx>,
    ) -> Result<FunctionValue<'ctx>> {
        let fn_type = target_fn.get_type();
        let function = self.module.add_function(name, fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);

        let args: Vec<BasicMetadataValueEnum> =
            function.get_param_iter().map(|arg| arg.into()).collect();

        let call_site = self.builder.build_call(target_fn, &args, "call")?;
        if let Some(result) = call_site.try_as_basic_value().left() {
            self.builder.build_return(Some(&result));
        } else {
            self.builder.build_return(None);
        }

        Ok(function)
    }

    pub(crate) fn init_native_func(
        &mut self,
        native_func: &HLNative,
    ) -> Result<FunctionValue<'ctx>> {
        let lib = native_func.lib.as_str();
        let name: String = format!("hlp_{}", native_func.name);

        let func_value = self.declare_native_function(lib, name.as_str(), native_func)?;

        let func_ptr = self
            .native_function_resolver
            .resolve_function(lib, name.as_str())?;

        // Add function mapping
        self.execution_engine
            .add_global_mapping(&func_value, func_ptr as usize);

        let native_caller =
            self.generate_native_caller_function(&format!("{}_{}_caller", lib, name), func_value)?;

        debug_assert!(native_caller.verify(true));
        // println!("{}", native_caller.print_to_string().to_string());

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

    pub fn execute_main(&mut self) -> Result<()> {
        let index = self.bytecode.entrypoint as usize;
        let (_, is_pending) = self.get_or_create_function_value(index)?;
        if is_pending {
            // Compile main function
            self.compile_function(index)?;
        }

        let function = self
            .func_cache
            .get(&index)
            .ok_or_else(|| anyhow!("Invalid function address"))?;

        unsafe { self.execution_engine.run_function(*function, &[]) };

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
