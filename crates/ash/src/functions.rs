use std::borrow::Borrow;
use std::ffi::c_void;
use std::path::{Path, PathBuf};

use crate::module::AshModule;
use crate::types::{FunPtr, Str};
use anyhow::*;
use hlbc::{opcodes::*, types::*};
use inkwell::types::{
    AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType,
};
use inkwell::values::{AnyValue, BasicMetadataValueEnum, BasicValue, FunctionValue, PointerValue};
use inkwell::{basic_block::BasicBlock, builder::Builder, AddressSpace};
use libloading::{Library, Symbol};
use std::collections::HashMap;
use std::sync::Once;
use tempfile::TempDir;



impl<'ctx> AshModule<'ctx> {
    pub(crate) fn create_function_type(
        &mut self,
        type_fun: &TypeFun,
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

    pub(crate) fn create_function_value(&mut self, index: usize) -> Result<FunctionValue<'ctx>> {
        if let Some(f_v) = self.function_values.get(&index) {
            return Ok(*f_v);
        }
        let findexes = self.function_indexes.clone();
        let fun = findexes
            .get(&index)
            .ok_or_else(|| anyhow!("Function not found at index {}", index))?;

        match fun {
            FunPtr::Fun(f) => {
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

                self.function_values.insert(f.findex.0, function);
                Ok(function)
            }
            FunPtr::Native(native) => {
                let func = self.init_native_func(native)?;
                self.function_values.insert(native.findex.0, func);
                Ok(func)
            }
        }
    }

    fn create_function_declaration(&mut self, f: &Function) -> Result<FunctionValue<'ctx>> {
        let func_type = match &self.bytecode.types.clone()[f.t.0] {
            Type::Fun(t) | Type::Method(t) => self.create_function_type(t)?,
            _ => return Err(anyhow!("Invalid function type")),
        };

        Ok(self
            .module
            .add_function(f.name(&self.bytecode).as_str(), func_type, None))
    }

    fn allocate_registers(
        &mut self,
        f: &Function,
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

    fn load_function_arguments(
        &self,
        f: &Function,
        function: &FunctionValue<'ctx>,
        registers: &[PointerValue<'ctx>],
        builder: &Builder<'ctx>,
    ) -> Result<()> {
        let reg_array_ptr = function.get_nth_param(0).unwrap().into_pointer_value();
        let args_count = f.args(&self.bytecode).len();

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

    fn translate_opcodes(
        &mut self,
        f: &Function,
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

            self.translate_opcode(op, registers, next_block)?;

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
                if let Some(FunPtr::Fun(_)) = self.function_indexes.get(&fun.0) {
                    let function = self.create_function_value(fun.0)?;
                    let result = self.builder.build_call(function, &[], "call")?;
                    self.builder.build_store(
                        registers[dst.0 as usize],
                        result.try_as_basic_value().left().unwrap(),
                    );
                    return Ok(());
                } else if let Some(FunPtr::Native(n)) = self.function_indexes.clone().get(&fun.0) {
                    let function = self.get_native_func(n)?;
                    let result = self.builder.build_call(*function, &[], "call")?;
                    self.builder.build_store(
                        registers[dst.0 as usize],
                        result.try_as_basic_value().left().unwrap(),
                    );
                    return Ok(());
                }

                return Err(anyhow!("Invalid function call"));
            }
            Opcode::Call1 { dst, fun, arg0 } => {
                let function = self.create_function_value(fun.0)?;
                let arg0_val = self.builder.build_load(
                    registers[arg0.0 as usize].get_type(),
                    registers[arg0.0 as usize],
                    "arg0_val",
                )?;
                let result = self
                    .builder
                    .build_call(function, &[arg0_val.into()], "call")?;
                self.builder.build_store(
                    registers[dst.0 as usize],
                    result.try_as_basic_value().left().unwrap(),
                );
            }
            Opcode::Ret { ret } => {
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
                let type_index = self.get_type_index(src_val.get_type().as_any_type_enum())?;

                // Retrieve the type info from type_info_globals
                let type_info = self
                    .type_info_globals
                    .get(&type_index)
                    .ok_or_else(|| anyhow!("Type info not found for index {}", type_index))?;

                // Load the type info
                let type_info_value = self.builder.build_load(
                    type_info.as_basic_value_enum().get_type(),
                    type_info.as_pointer_value(),
                    "type_info",
                )?;

                // Store the type info in the destination register
                self.builder
                    .build_store(registers[dst.0 as usize], type_info_value);
            }

            // Add more opcode translations here...
            _ => return Err(anyhow!("Opcode {:?} not yet implemented", op)),
        }
        Ok(())
    }

    fn declare_native_function(
        &mut self,
        lib: &Str,
        name: &Str,
        native_func: &Native,
    ) -> Result<FunctionValue<'ctx>> {
        // let fn_type = return_type.fn_type(arg_types.iter().map(|a| a.into()).collect(), false);
        // self.module.add_function(name, fn_type, None)

        let func_type = match &self.bytecode.types.clone()[native_func.t.0] {
            Type::Fun(t) | Type::Method(t) => self.create_function_type(t)?,
            _ => return Err(anyhow!("Invalid function type")),
        };

        Ok(self
            .module
            .add_function(name.as_str(), func_type, None))
    }

    fn generate_native_caller_function(
        &self,
        name: &Str,
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
        }
        
        Ok(function)
    }

    pub(crate) fn init_native_func(&mut self, native_func: &Native) -> Result<FunctionValue<'ctx>> {
        let lib = native_func.lib(&self.bytecode);
        let name = Str::from(format!("hlp_{}", native_func.name(&self.bytecode)));

        let func_value = self.declare_native_function(&lib, &name, native_func)?;

        let func_ptr = self
            .native_function_resolver
            .resolve_function(&lib, &name)?;
        // Add function mapping
        unsafe {
            self.execution_engine
                .add_global_mapping(&func_value, func_ptr as usize);
        }

        let native_caller = self.generate_native_caller_function(
            &Str::from(format!("{}_{}_caller", lib, name)),
            func_value,
        )?;

        native_caller.print_to_stderr();

        Ok(native_caller)
    }

    fn get_native_func(&self, native: &Native) -> Result<&FunctionValue<'ctx>> {
        if let Some(func) = self.function_values.get(&native.findex.0) {
            return Ok(func);
        }
        let lib = native.lib(&self.bytecode);
        let name = native.name(&self.bytecode);

        Err(anyhow!("Native function not found '{}::{}'", lib, name))
    }
}

