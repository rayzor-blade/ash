//! Emitting one HashLink opcode.
//!
//! Split out of `function/mod.rs`, which was over ten thousand lines with
//! `translate_opcode` alone accounting for nearly five of them. Nothing here
//! changed in the move; it is the same `impl` block on `JITModule`, so the
//! methods keep calling each other exactly as before.

use std::ffi::c_void;

use air::v2::ir::{
    BinOp as AirBinOp, BlockId as AirBlockId, CastKind as AirCastKind, CondKind as AirCondKind,
    Function as AirFunction, Instr as AirInstr, MemAccess as AirMemAccess,
    Terminator as AirTerminator, UnOp as AirUnOp, ValueId,
};
use ash_macro::to_llvm;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::types::{
    AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType,
};
use inkwell::values::{
    AnyValue, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue,
};
use inkwell::{
    basic_block::BasicBlock, builder::Builder, AddressSpace, AtomicOrdering, FloatPredicate,
    IntPredicate,
};

use crate::llvm::module::{CompiledFunctionMeta, JITModule};

use super::{hl_hash_utf8, sized_alloc_enabled};
use crate::hl::{
    hl_obj_field, hl_runtime_obj, hl_type, hl_type_kind_HABSTRACT, hl_type_kind_HBOOL,
    hl_type_kind_HBYTES, hl_type_kind_HDYN, hl_type_kind_HDYNOBJ, hl_type_kind_HF32,
    hl_type_kind_HF64, hl_type_kind_HI32, hl_type_kind_HI64, hl_type_kind_HNULL, hl_type_kind_HOBJ,
    hl_type_kind_HSTRUCT, hl_type_kind_HTYPE, hl_type_kind_HUI16, hl_type_kind_HUI8,
    hl_type_kind_HVIRTUAL, hl_type_kind_HVOID, vdynamic, vdynobj, vvirtual,
};
use crate::opcodes::{
    Opcode, RefBytes, RefEnumConstruct, RefField, RefFloat, RefFun, RefGlobal, RefInt, RefString,
    RefType, Reg,
};
use crate::types::{HLNative, HLTypeFun, Str, TypeRef};
use crate::{
    hl::{hl_type_kind_HFUN, hl_type_kind_HMETHOD},
    types::HLFunction,
};
use anyhow::{anyhow, Result};

impl<'ctx> JITModule<'ctx> {
    /// Emit one primitive operation selected by AIR V2.
    ///
    /// This is an instruction emitter, not a bytecode-function lowering
    /// route: AIR owns the CFG, phi edges, values, cells and terminators, and
    /// there is intentionally no method that walks an `HLFunction::ops` body.
    pub(super) fn translate_opcode(
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
                    .ensure_int_global(ptr.0)
                    .ok_or_else(|| anyhow!("Int constant not found"))?;
                let loaded_int = self.builder.build_load(
                    self.context.i32_type(),
                    int_val.as_pointer_value(),
                    "int_val",
                )?;
                let loaded_int = self.cast_for_call(loaded_int, reg_types[dst.0 as usize])?;
                self.builder
                    .build_store(registers[dst.0 as usize], loaded_int)?;
            }
            Opcode::Float { dst, ptr } => {
                let float_val = self
                    .ensure_float_global(ptr.0)
                    .ok_or_else(|| anyhow!("Float constant not found"))?;
                let loaded_float = self.builder.build_load(
                    self.context.f64_type(),
                    float_val.as_pointer_value(),
                    "float_val",
                )?;
                // The pool is f64; an HF32 destination is a 4-byte slot.
                let loaded_float = self.cast_for_call(loaded_float, reg_types[dst.0 as usize])?;
                self.builder
                    .build_store(registers[dst.0 as usize], loaded_float)?;
            }
            Opcode::Bool { dst, value } => {
                let bool_val = self.context.bool_type().const_int(*value as u64, false);
                self.builder
                    .build_store(registers[dst.0 as usize], bool_val);
            }
            Opcode::String { dst, ptr } => {
                let string_val = self
                    .ensure_string_global(ptr.0)
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
                        let fv = self.builder.build_float_add(
                            a_val.into_float_value(),
                            b_val.into_float_value(),
                            "add",
                        )?;
                        if let Some(inst) = fv.as_instruction() {
                            inst.set_fast_math_flags(1 << 5);
                        }
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
                        let fv = self.builder.build_float_sub(
                            a_val.into_float_value(),
                            b_val.into_float_value(),
                            "sub",
                        )?;
                        if let Some(inst) = fv.as_instruction() {
                            inst.set_fast_math_flags(1 << 5);
                        }
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
                        let fv = self.builder.build_float_mul(
                            a_val.into_float_value(),
                            b_val.into_float_value(),
                            "mul",
                        )?;
                        if let Some(inst) = fv.as_instruction() {
                            inst.set_fast_math_flags(1 << 5);
                        }
                        fv.as_basic_value_enum()
                    }
                    _ => return Err(anyhow!("Unsupported types for Mul operation")),
                };
                self.builder.build_store(registers[dst.0 as usize], result);
            }
            Opcode::Call0 { dst, fun } => {
                let (function, is_placeholder) = self.get_or_create_function_value(fun.0)?;
                let result = self.builder.build_call(function, &[], "call")?;

                if result.try_as_basic_value().basic().is_some() {
                    self.builder.build_store(
                        registers[dst.0 as usize],
                        result.try_as_basic_value().basic().unwrap(),
                    );
                }

                if is_placeholder {
                    self.add_pending_compilation(fun.0);
                }
            }

            Opcode::Call1 { dst, fun, arg0 } => {
                let arg0_val = self.builder.build_load(
                    reg_types[arg0.0 as usize],
                    registers[arg0.0 as usize],
                    "arg0_val",
                )?;

                // Machine-instruction primitives (Math.sqrt and friends) are
                // emitted here rather than called. Every entry in the table is
                // unary, which is why this is the only call arity that has to
                // check. See crate::intrinsics.
                let inlined = match self.native_intrinsic_for(fun.0) {
                    Some(intr) => self.emit_native_intrinsic(intr, arg0_val)?,
                    None => None,
                };

                if let Some(v) = inlined {
                    self.builder.build_store(registers[dst.0 as usize], v)?;
                } else {
                    let (function, is_placeholder) = self.get_or_create_function_value(fun.0)?;
                    let result = self
                        .builder
                        .build_call(function, &[arg0_val.into()], "call")?;

                    if result.try_as_basic_value().basic().is_some() {
                        self.builder.build_store(
                            registers[dst.0 as usize],
                            result.try_as_basic_value().basic().unwrap(),
                        );
                    }

                    if is_placeholder {
                        self.add_pending_compilation(fun.0);
                    }
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

                if result.try_as_basic_value().basic().is_some() {
                    self.builder.build_store(
                        registers[dst.0 as usize],
                        result.try_as_basic_value().basic().unwrap(),
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

                if result.try_as_basic_value().basic().is_some() {
                    self.builder.build_store(
                        registers[dst.0 as usize],
                        result.try_as_basic_value().basic().unwrap(),
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
                self.builder
                    .build_conditional_branch(cond_val.into_int_value(), target, next)?;
            }
            Opcode::JFalse { cond, offset } => {
                let cond_val = self.builder.build_load(
                    reg_types[cond.0 as usize],
                    registers[cond.0 as usize],
                    "cond_val",
                )?;
                let target = opcode_blocks[(i as i32 + 1 + *offset) as usize];
                let next = opcode_blocks[i + 1];
                self.builder
                    .build_conditional_branch(cond_val.into_int_value(), next, target)?;
            }
            Opcode::JAlways { offset } => {
                let target = opcode_blocks[(i as i32 + 1 + *offset) as usize];
                self.builder.build_unconditional_branch(target)?;
            }
            Opcode::GetType { dst, src } => {
                // GetType reads the runtime hl_type* from the value's ->t field (offset 0)
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "gettype_src",
                )?;
                let obj_ptr = src_val.into_pointer_value();
                // `hl_typeof(NULL)` is the void type, and `Type.typeof(null)`,
                // `Reflect.isFunction(null)` and a JSON printer walking an
                // object with a null field all rely on it. The interpreter
                // returns it; this arm dereferenced null instead, which
                // nothing but a whole-program compile ever executed.
                let current_fn = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();
                let null_block = self.context.append_basic_block(current_fn, "gettype_null");
                let load_block = self.context.append_basic_block(current_fn, "gettype_load");
                let cont_block = self.context.append_basic_block(current_fn, "gettype_cont");
                let is_null = self.builder.build_is_null(obj_ptr, "gettype_is_null")?;
                self.builder
                    .build_conditional_branch(is_null, null_block, load_block)?;
                self.builder.position_at_end(null_block);
                let void_index = self
                    .types_
                    .iter()
                    .position(|t| t.kind == hl_type_kind_HVOID)
                    .ok_or_else(|| anyhow!("GetType: no void type in the type table"))?;
                let void_type = self.get_initialized_type(void_index)?;
                self.builder
                    .build_store(registers[dst.0 as usize], void_type)?;
                self.builder.build_unconditional_branch(cont_block)?;
                self.builder.position_at_end(load_block);
                // obj->t is the first field (offset 0) of vdynamic/vobj, a pointer to hl_type
                let t_ptr = self
                    .builder
                    .build_load(ptr_type, obj_ptr, "gettype_t")?
                    .into_pointer_value();
                self.builder.build_store(registers[dst.0 as usize], t_ptr)?;
                self.builder.build_unconditional_branch(cont_block)?;
                self.builder.position_at_end(cont_block);
            }

            Opcode::Type { dst, ty } => {
                let typ: BasicValueEnum<'ctx> = self.get_initialized_type(ty.0)?;
                // Store the type info in the destination register
                self.builder.build_store(registers[dst.0 as usize], typ);
            }

            Opcode::New { dst } => {
                let type_index = f.regs.clone()[dst.0 as usize].0;
                // `kind` is Copy; cloning the whole type table to read it was pure waste.
                let type_kind = self.types_[type_index].kind;

                match type_kind {
                    hl_type_kind_HSTRUCT | hl_type_kind_HOBJ => {
                        let type_ = self
                            .initialized_type_cache
                            .get(&type_index)
                            .expect("Expected to get type");
                        // type_ is already a pointer constant (inttoptr), pass directly
                        let type_ptr = type_.into_pointer_value();

                        // What `hlp_alloc_obj` re-derives per allocation is
                        // fixed for a type: the size, and whether the class
                        // binds closures into fields at construction. Both are
                        // known here, so an ordinary class allocates through
                        // the sized entry and skips them. A struct keeps the
                        // general path -- it has no type header to stamp.
                        let sized = (type_kind == hl_type_kind_HOBJ && sized_alloc_enabled())
                            .then(|| {
                                let no_bindings = self.types_[type_index]
                                    .obj
                                    .as_ref()
                                    .is_some_and(|o| o.bindings.is_empty());
                                let size = crate::layout::object_layout_for(
                                    &self.types_,
                                    type_index,
                                    self.target_abi.pointer_bytes() as i32,
                                )
                                .map(|l| l.size);
                                match (no_bindings, size) {
                                    (true, Some(size)) if size > 0 => Some(size as u64),
                                    _ => None,
                                }
                            })
                            .flatten();

                        let result = if let Some(size) = sized {
                            // `size` is a `usize` in the runtime, so it is the
                            // target's pointer width -- i32 on wasm32, where a
                            // hardcoded i64 makes the module fail validation
                            // at the call.
                            let size_type = self.target_abi.pointer_int_type(self.context);
                            let fun = self.declare_native(
                                "hlp_alloc_obj_sized",
                                &[
                                    self.context.ptr_type(AddressSpace::default()).into(),
                                    size_type.into(),
                                ],
                                Some(self.context.ptr_type(AddressSpace::default()).into()),
                            );
                            self.builder.build_call(
                                fun,
                                &[type_ptr.into(), size_type.const_int(size, false).into()],
                                "call",
                            )?
                        } else {
                            let fun = self
                                .func_cache
                                .iter()
                                .find(|(_, f)| {
                                    f.get_name().to_string_lossy() == "std_hlp_alloc_obj_caller"
                                })
                                .expect("Expected to find native function hlp_alloc_obj")
                                .1;
                            self.builder.build_call(*fun, &[type_ptr.into()], "call")?
                        };
                        self.builder.build_store(
                            registers[dst.0 as usize],
                            result.try_as_basic_value().basic().unwrap(),
                        );
                    }
                    hl_type_kind_HDYNOBJ => {
                        let fun = self.declare_native(
                            "hlp_alloc_dynobj",
                            &[],
                            Some(self.context.ptr_type(AddressSpace::default()).into()),
                        );

                        let result = self.builder.build_call(fun, &[], "call")?;
                        self.builder.build_store(
                            registers[dst.0 as usize],
                            result.try_as_basic_value().basic().unwrap(),
                        );
                    }
                    hl_type_kind_HVIRTUAL => {
                        let type_ = self
                            .initialized_type_cache
                            .get(&type_index)
                            .expect("Expected to get type");
                        let fun = self.declare_native(
                            "hlp_alloc_virtual",
                            &[self.context.ptr_type(AddressSpace::default()).into()],
                            Some(self.context.ptr_type(AddressSpace::default()).into()),
                        );

                        // type_ is already a pointer constant, pass directly
                        let type_ptr = type_.into_pointer_value();
                        let result = self.builder.build_call(fun, &[type_ptr.into()], "call")?;
                        self.builder.build_store(
                            registers[dst.0 as usize],
                            result.try_as_basic_value().basic().unwrap(),
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
                        let field_ptr =
                            self.build_field_ptr(f.regs[obj.0 as usize].0, field.0, obj_val)?;
                        let st = self.builder.build_store(field_ptr, src_val)?;
                        self.tbaa_field(Some(st), f.regs[obj.0 as usize].0, field.0);
                    }
                    hl_type_kind_HVIRTUAL => {
                        let ptr_type = self.context.ptr_type(AddressSpace::default());
                        let vvirt_ptr = obj_val;

                        // vfields array starts at offset sizeof(vvirtual) = 24
                        // from the vvirtual pointer (after t, value, next fields)
                        let vfields_ptr = unsafe {
                            self.builder.build_gep(
                                self.context.i8_type(),
                                vvirt_ptr,
                                &[self
                                    .context
                                    .i64_type()
                                    .const_int(self.target_abi.vvirtual_fields_offset(), false)],
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
                        let field_value_ptr =
                            self.builder
                                .build_load(ptr_type, field_ptr, "field_value_ptr")?;
                        let field_exists = self.builder.build_is_not_null(
                            field_value_ptr.into_pointer_value(),
                            "field_exists",
                        )?;

                        let current_fn = self
                            .builder
                            .get_insert_block()
                            .unwrap()
                            .get_parent()
                            .unwrap();
                        let then_block =
                            self.context.append_basic_block(current_fn, "field_exists");
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
                        self.builder
                            .build_store(field_value_ptr.into_pointer_value(), src_val)?;
                        self.builder.build_unconditional_branch(cont_block)?;

                        // Field doesn't exist: box value + call hlp_obj_set_field
                        self.builder.position_at_end(else_block);
                        let hashed_name = obj_type_
                            .virt
                            .as_ref()
                            .map(|v| v.fields.get(field.0).map(|f| f.hashed_name).unwrap_or(0))
                            .unwrap_or(0);
                        let field_hash =
                            self.context.i32_type().const_int(hashed_name as u64, false);
                        let src_type_idx = f.regs[src.0 as usize].0;
                        let src_kind = self.types_[src_type_idx].kind;

                        // Box the value to a vdynamic* via hlp_make_dyn
                        let boxed_val = if src_kind == hl_type_kind_HI32
                            || src_kind == hl_type_kind_HBOOL
                            || src_kind == hl_type_kind_HUI8
                            || src_kind == hl_type_kind_HUI16
                            || src_kind == hl_type_kind_HF32
                            || src_kind == hl_type_kind_HF64
                            || src_kind == hl_type_kind_HI64
                        {
                            // Store value to a temp alloca, pass its address
                            let tmp = self.entry_alloca(reg_types[src.0 as usize], "tmp_box")?;
                            self.builder.build_store(tmp, src_val)?;
                            let type_ptr_val = self
                                .get_initialized_type(src_type_idx)?
                                .into_pointer_value();
                            let make_dyn = self.declare_native(
                                "hlp_make_dyn",
                                &[ptr_type.into(), ptr_type.into()],
                                Some(ptr_type.into()),
                            );
                            self.builder
                                .build_call(
                                    make_dyn,
                                    &[tmp.into(), type_ptr_val.into()],
                                    "boxed_val",
                                )?
                                .try_as_basic_value()
                                .basic()
                                .unwrap()
                                .into_pointer_value()
                        } else {
                            src_val.into_pointer_value()
                        };

                        // Load value (underlying object) from vvirtual offset 8
                        let fb_value_gep = unsafe {
                            self.builder.build_gep(
                                self.context.i8_type(),
                                vvirt_ptr,
                                &[self
                                    .context
                                    .i64_type()
                                    .const_int(self.target_abi.vvirtual_value_offset(), false)],
                                "sf_fb_value_gep",
                            )?
                        };
                        let fb_value_obj =
                            self.builder
                                .build_load(ptr_type, fb_value_gep, "sf_fb_value")?;

                        let obj_set_field = self.declare_native(
                            "hlp_obj_set_field",
                            &[
                                ptr_type.into(),
                                self.context.i32_type().into(),
                                ptr_type.into(),
                            ],
                            None,
                        );
                        self.builder.build_call(
                            obj_set_field,
                            &[fb_value_obj.into(), field_hash.into(), boxed_val.into()],
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
                        let field_ptr = self.build_field_ptr(
                            f.regs[obj.0 as usize].0,
                            field.0,
                            obj_val.into_pointer_value(),
                        )?;

                        // Load the field value using destination register type
                        let load_type = self.get_register_type(f.regs[dst.0 as usize].0)?;
                        let field_val =
                            self.builder.build_load(load_type, field_ptr, "field_val")?;
                        self.tbaa_field(
                            field_val.as_instruction_value(),
                            f.regs[obj.0 as usize].0,
                            field.0,
                        );

                        self.builder
                            .build_store(registers[dst.0 as usize], field_val)?;
                    }
                    hl_type_kind_HVIRTUAL => {
                        let ptr_type = self.context.ptr_type(AddressSpace::default());
                        let vvirt_ptr = obj_val.into_pointer_value();

                        // vfields array starts at offset sizeof(vvirtual) = 24
                        // from the vvirtual pointer (after t, value, next fields)
                        let vfields_ptr = unsafe {
                            self.builder.build_gep(
                                self.context.i8_type(),
                                vvirt_ptr,
                                &[self
                                    .context
                                    .i64_type()
                                    .const_int(self.target_abi.vvirtual_fields_offset(), false)],
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
                        let field_value_check =
                            self.builder
                                .build_load(ptr_type, field_ptr, "field_value_ptr")?;
                        let field_exists = self.builder.build_is_not_null(
                            field_value_check.into_pointer_value(),
                            "field_exists",
                        )?;

                        let current_fn = self
                            .builder
                            .get_insert_block()
                            .unwrap()
                            .get_parent()
                            .unwrap();
                        let then_block =
                            self.context.append_basic_block(current_fn, "field_exists");
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
                        let field_value_ptr =
                            self.builder
                                .build_load(ptr_type, field_ptr, "field_value_ptr")?;
                        // Load with the destination register's type, not ptr,
                        // to avoid reading more bytes than the field actually holds.
                        let dst_load_type = reg_types[dst.0 as usize];
                        let field_value = self.builder.build_load(
                            dst_load_type,
                            field_value_ptr.into_pointer_value(),
                            "field_value",
                        )?;
                        self.builder
                            .build_store(registers[dst.0 as usize], field_value)?;
                        self.builder.build_unconditional_branch(cont_block)?;

                        // Field doesn't exist in vfields: fall back to dynamic
                        // field access on the underlying value object
                        self.builder.position_at_end(else_block);
                        let i32_type = self.context.i32_type();
                        // Load value (underlying object) from vvirtual offset 8
                        let value_gep = unsafe {
                            self.builder.build_gep(
                                self.context.i8_type(),
                                vvirt_ptr,
                                &[self
                                    .context
                                    .i64_type()
                                    .const_int(self.target_abi.vvirtual_value_offset(), false)],
                                "fb_value_gep",
                            )?
                        };
                        let value_obj = self.builder.build_load(ptr_type, value_gep, "fb_value")?;
                        let hashed_name = obj_type_
                            .virt
                            .as_ref()
                            .map(|v| v.fields.get(field.0).map(|f| f.hashed_name).unwrap_or(0))
                            .unwrap_or(0);
                        let field_hash = i32_type.const_int(hashed_name as u64, true);
                        let dst_type_idx = f.regs[dst.0 as usize].0;
                        let dst_kind = self.types_[dst_type_idx].kind;
                        // Pick the getter by the DESTINATION's kind, the way
                        // `DynGet` does. This used to call `hlp_dyn_getp`
                        // unconditionally and store the pointer it returns
                        // into whatever register `dst` is -- `dst_kind` was
                        // computed right above and then never read. For an
                        // i32 field that stored 8 bytes of boxed pointer into
                        // a 4-byte slot and read back its low half; for an f64
                        // field it reinterpreted a pointer as a double, which
                        // is a denormal near zero rather than the value. The
                        // whole-program audit found 588 of the first shape and
                        // 292 of the second.
                        let type_ptr = self
                            .get_initialized_type(dst_type_idx)?
                            .into_pointer_value();
                        let f32_type = self.context.f32_type();
                        let f64_type = self.context.f64_type();
                        let i64_type = self.context.i64_type();
                        let (getter, args, ret): (&str, Vec<_>, BasicTypeEnum) = match dst_kind {
                            hl_type_kind_HF64 => (
                                "hlp_dyn_getd",
                                vec![ptr_type.into(), i32_type.into()],
                                f64_type.into(),
                            ),
                            hl_type_kind_HF32 => (
                                "hlp_dyn_getf",
                                vec![ptr_type.into(), i32_type.into()],
                                f32_type.into(),
                            ),
                            hl_type_kind_HI64 => (
                                "hlp_dyn_geti64",
                                vec![ptr_type.into(), i32_type.into()],
                                i64_type.into(),
                            ),
                            hl_type_kind_HI32 | hl_type_kind_HBOOL | hl_type_kind_HUI8
                            | hl_type_kind_HUI16 => (
                                "hlp_dyn_geti",
                                vec![ptr_type.into(), i32_type.into(), ptr_type.into()],
                                i32_type.into(),
                            ),
                            _ => (
                                "hlp_dyn_getp",
                                vec![ptr_type.into(), i32_type.into(), ptr_type.into()],
                                ptr_type.into(),
                            ),
                        };
                        let getter = self.declare_native(getter, &args, Some(ret.into()));
                        let mut call_args: Vec<inkwell::values::BasicMetadataValueEnum> =
                            vec![value_obj.into(), field_hash.into()];
                        if args.len() == 3 {
                            call_args.push(type_ptr.into());
                        }
                        let result = self.builder.build_call(getter, &call_args, "dyn_get_fb")?;
                        let dyn_field_value = result.try_as_basic_value().basic().unwrap();
                        // `hlp_dyn_geti` answers every narrow integer kind as
                        // i32, so a HBOOL/HUI8/HUI16 slot still needs the
                        // truncation its width implies.
                        let dyn_field_value =
                            self.cast_for_call(dyn_field_value, reg_types[dst.0 as usize])?;
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
                let val = self
                    .builder
                    .build_load(ptr_type, global_ptr, "global_load")?;
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
                self.builder.build_store(global_ptr, src_val);
            }
            Opcode::GetArray { dst, array, index } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let i32_type = self.context.i32_type();
                let i8_type = self.context.i8_type();

                let arr = self
                    .builder
                    .build_load(ptr_type, registers[array.0 as usize], "getarr_ptr")?
                    .into_pointer_value();
                let idx = self
                    .builder
                    .build_load(i32_type, registers[index.0 as usize], "getarr_idx")?
                    .into_int_value();

                // Data starts at offset 24 (sizeof(varray))
                let data_ptr = unsafe {
                    self.builder.build_gep(
                        i8_type,
                        arr,
                        &[self
                            .context
                            .i64_type()
                            .const_int(self.target_abi.varray_data_offset(), false)],
                        "getarr_data",
                    )?
                };

                // Element size from the destination register's
                // kind, via the table `crate::layout` shares with the Cranelift
                // tier so the two cannot index an array differently.
                let dst_type_idx = f.regs[dst.0 as usize].0;
                let dst_kind = self.types_[dst_type_idx].kind;
                let elem_size = crate::layout::array_elem_size_for(
                    dst_kind,
                    self.target_abi.pointer_bytes() as i32,
                ) as u64;

                let elem_size_val = i32_type.const_int(elem_size, false);
                let byte_offset =
                    self.builder
                        .build_int_mul(idx, elem_size_val, "getarr_offset")?;
                let slot = unsafe {
                    self.builder
                        .build_gep(i8_type, data_ptr, &[byte_offset], "getarr_slot")?
                };
                let element_val =
                    self.builder
                        .build_load(reg_types[dst.0 as usize], slot, "getarr_val")?;
                if let Some(i) = element_val.as_instruction_value() {
                    self.tbaa.tag(i, self.tbaa.payload());
                }
                self.builder
                    .build_store(registers[dst.0 as usize], element_val)?;
            }

            // --- Control flow: Label, Nop ---
            Opcode::Label | Opcode::Nop => {
                // No-op: fallthrough handled by outer loop
            }

            // --- NullCheck ---
            Opcode::NullCheck { reg } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let val = self.builder.build_load(
                    reg_types[reg.0 as usize],
                    registers[reg.0 as usize],
                    "null_check",
                )?;
                if val.is_pointer_value() {
                    let is_null = self
                        .builder
                        .build_is_null(val.into_pointer_value(), "is_null")?;
                    let function = self
                        .builder
                        .get_insert_block()
                        .unwrap()
                        .get_parent()
                        .unwrap();
                    let throw_block = self.context.append_basic_block(function, "null_throw");
                    self.builder.build_conditional_branch(
                        is_null,
                        throw_block,
                        opcode_blocks[i + 1],
                    )?;
                    self.builder.position_at_end(throw_block);
                    // A null here is a catchable HashLink exception, "Null
                    // access" -- what the interpreter throws and what Haxe
                    // code (and the unit suite) catches. A bare `unreachable`
                    // made it a `brk` at run time and, worse, told the
                    // optimizer the pointer is never null, so O3 could drop
                    // the test and everything guarded by it.
                    let err_fn_type = self.context.void_type().fn_type(&[ptr_type.into()], true);
                    let err_ptr = self.error_function_ptr()?;
                    let msg_ptr = self.utf16_message("Null access")?;
                    self.builder.build_indirect_call(
                        err_fn_type,
                        err_ptr,
                        &[msg_ptr.into()],
                        "null_access_throw",
                    )?;
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
                    let is_null = self
                        .builder
                        .build_is_null(val.into_pointer_value(), "is_null")?;
                    self.builder
                        .build_conditional_branch(is_null, target, next)?;
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
                    let is_not_null = self
                        .builder
                        .build_is_not_null(val.into_pointer_value(), "is_not_null")?;
                    self.builder
                        .build_conditional_branch(is_not_null, target, next)?;
                } else {
                    // Non-pointer types are always not-null
                    self.builder.build_unconditional_branch(target)?;
                }
            }

            // --- Comparison jumps ---
            Opcode::JSLt { a, b, offset } => {
                let a_kind = self.types_[f.regs[a.0 as usize].0].kind;
                self.emit_comparison_jump(
                    registers,
                    reg_types,
                    a,
                    b,
                    a_kind,
                    IntPredicate::SLT,
                    FloatPredicate::OLT,
                    i,
                    *offset,
                    opcode_blocks,
                )?;
            }
            Opcode::JSGte { a, b, offset } => {
                let a_kind = self.types_[f.regs[a.0 as usize].0].kind;
                self.emit_comparison_jump(
                    registers,
                    reg_types,
                    a,
                    b,
                    a_kind,
                    IntPredicate::SGE,
                    FloatPredicate::OGE,
                    i,
                    *offset,
                    opcode_blocks,
                )?;
            }
            Opcode::JSGt { a, b, offset } => {
                let a_kind = self.types_[f.regs[a.0 as usize].0].kind;
                self.emit_comparison_jump(
                    registers,
                    reg_types,
                    a,
                    b,
                    a_kind,
                    IntPredicate::SGT,
                    FloatPredicate::OGT,
                    i,
                    *offset,
                    opcode_blocks,
                )?;
            }
            Opcode::JSLte { a, b, offset } => {
                let a_kind = self.types_[f.regs[a.0 as usize].0].kind;
                self.emit_comparison_jump(
                    registers,
                    reg_types,
                    a,
                    b,
                    a_kind,
                    IntPredicate::SLE,
                    FloatPredicate::OLE,
                    i,
                    *offset,
                    opcode_blocks,
                )?;
            }
            Opcode::JULt { a, b, offset } => {
                let a_kind = self.types_[f.regs[a.0 as usize].0].kind;
                self.emit_comparison_jump(
                    registers,
                    reg_types,
                    a,
                    b,
                    a_kind,
                    IntPredicate::ULT,
                    FloatPredicate::OLT,
                    i,
                    *offset,
                    opcode_blocks,
                )?;
            }
            Opcode::JUGte { a, b, offset } => {
                let a_kind = self.types_[f.regs[a.0 as usize].0].kind;
                self.emit_comparison_jump(
                    registers,
                    reg_types,
                    a,
                    b,
                    a_kind,
                    IntPredicate::UGE,
                    FloatPredicate::OGE,
                    i,
                    *offset,
                    opcode_blocks,
                )?;
            }
            // Haxe inverts every float `if` through JNotLt / JNotGte, so those
            // two must jump when the operands are UNORDERED: `if (nan > 0)`
            // is `JNotLt(0, nan)` skipping the body, and an ordered OGE let
            // the body run. `nan != nan` is true, so JNotEq is UNE. JEq/JSLt/
            // JSGte/JSGt/JSLte stay ordered: they are the non-inverted forms
            // and a NaN operand makes them false, as in HashLink.
            Opcode::JNotLt { a, b, offset } => {
                // !(a < b) is the same as a >= b
                let a_kind = self.types_[f.regs[a.0 as usize].0].kind;
                self.emit_comparison_jump(
                    registers,
                    reg_types,
                    a,
                    b,
                    a_kind,
                    IntPredicate::SGE,
                    FloatPredicate::UGE,
                    i,
                    *offset,
                    opcode_blocks,
                )?;
            }
            Opcode::JNotGte { a, b, offset } => {
                // !(a >= b) is the same as a < b
                let a_kind = self.types_[f.regs[a.0 as usize].0].kind;
                self.emit_comparison_jump(
                    registers,
                    reg_types,
                    a,
                    b,
                    a_kind,
                    IntPredicate::SLT,
                    FloatPredicate::ULT,
                    i,
                    *offset,
                    opcode_blocks,
                )?;
            }
            Opcode::JEq { a, b, offset } => {
                let a_kind = self.types_[f.regs[a.0 as usize].0].kind;
                self.emit_comparison_jump(
                    registers,
                    reg_types,
                    a,
                    b,
                    a_kind,
                    IntPredicate::EQ,
                    FloatPredicate::OEQ,
                    i,
                    *offset,
                    opcode_blocks,
                )?;
            }
            Opcode::JNotEq { a, b, offset } => {
                let a_kind = self.types_[f.regs[a.0 as usize].0].kind;
                self.emit_comparison_jump(
                    registers,
                    reg_types,
                    a,
                    b,
                    a_kind,
                    IntPredicate::NE,
                    FloatPredicate::UNE,
                    i,
                    *offset,
                    opcode_blocks,
                )?;
            }

            // --- Switch ---
            Opcode::Switch { reg, offsets, end } => {
                let val = self
                    .builder
                    .build_load(
                        reg_types[reg.0 as usize],
                        registers[reg.0 as usize],
                        "switch_val",
                    )?
                    .into_int_value();
                let default_target = opcode_blocks[i + 1];
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
                self.emit_binary_op(registers, reg_types, dst, a, b, "sdiv", |b, av, bv| match (
                    av.get_type().as_any_type_enum(),
                    bv.get_type().as_any_type_enum(),
                ) {
                    (AnyTypeEnum::IntType(_), AnyTypeEnum::IntType(_)) => Ok(b
                        .build_int_signed_div(av.into_int_value(), bv.into_int_value(), "sdiv")?
                        .as_basic_value_enum()),
                    (AnyTypeEnum::FloatType(_), AnyTypeEnum::FloatType(_)) => {
                        let fv = b.build_float_div(
                            av.into_float_value(),
                            bv.into_float_value(),
                            "sdiv",
                        )?;
                        if let Some(inst) = fv.as_instruction() {
                            inst.set_fast_math_flags(1 << 5);
                        }
                        Ok(fv.as_basic_value_enum())
                    }
                    _ => Err(anyhow!("Unsupported types for SDiv")),
                })?;
            }
            Opcode::UDiv { dst, a, b } => {
                self.emit_binary_op(registers, reg_types, dst, a, b, "udiv", |b, av, bv| match (
                    av.get_type().as_any_type_enum(),
                    bv.get_type().as_any_type_enum(),
                ) {
                    (AnyTypeEnum::IntType(_), AnyTypeEnum::IntType(_)) => Ok(b
                        .build_int_unsigned_div(av.into_int_value(), bv.into_int_value(), "udiv")?
                        .as_basic_value_enum()),
                    _ => Err(anyhow!("Unsupported types for UDiv")),
                })?;
            }
            Opcode::SMod { dst, a, b } => {
                self.emit_binary_op(registers, reg_types, dst, a, b, "smod", |b, av, bv| match (
                    av.get_type().as_any_type_enum(),
                    bv.get_type().as_any_type_enum(),
                ) {
                    (AnyTypeEnum::IntType(_), AnyTypeEnum::IntType(_)) => Ok(b
                        .build_int_signed_rem(av.into_int_value(), bv.into_int_value(), "smod")?
                        .as_basic_value_enum()),
                    (AnyTypeEnum::FloatType(_), AnyTypeEnum::FloatType(_)) => {
                        let fv = b.build_float_rem(
                            av.into_float_value(),
                            bv.into_float_value(),
                            "smod",
                        )?;
                        if let Some(inst) = fv.as_instruction() {
                            inst.set_fast_math_flags(1 << 5);
                        }
                        Ok(fv.as_basic_value_enum())
                    }
                    _ => Err(anyhow!("Unsupported types for SMod")),
                })?;
            }
            Opcode::UMod { dst, a, b } => {
                self.emit_binary_op(registers, reg_types, dst, a, b, "umod", |b, av, bv| match (
                    av.get_type().as_any_type_enum(),
                    bv.get_type().as_any_type_enum(),
                ) {
                    (AnyTypeEnum::IntType(_), AnyTypeEnum::IntType(_)) => Ok(b
                        .build_int_unsigned_rem(av.into_int_value(), bv.into_int_value(), "umod")?
                        .as_basic_value_enum()),
                    _ => Err(anyhow!("Unsupported types for UMod")),
                })?;
            }
            Opcode::Shl { dst, a, b } => {
                self.emit_binary_op(registers, reg_types, dst, a, b, "shl", |b, av, bv| {
                    let (x, y) = Self::shift_operands(b, av.into_int_value(), bv.into_int_value())?;
                    Ok(b.build_left_shift(x, y, "shl")?.as_basic_value_enum())
                })?;
            }
            Opcode::SShr { dst, a, b } => {
                self.emit_binary_op(registers, reg_types, dst, a, b, "sshr", |b, av, bv| {
                    let (x, y) = Self::shift_operands(b, av.into_int_value(), bv.into_int_value())?;
                    Ok(b.build_right_shift(x, y, true, "sshr")?
                        .as_basic_value_enum())
                })?;
            }
            Opcode::UShr { dst, a, b } => {
                self.emit_binary_op(registers, reg_types, dst, a, b, "ushr", |b, av, bv| {
                    let (x, y) = Self::shift_operands(b, av.into_int_value(), bv.into_int_value())?;
                    Ok(b.build_right_shift(x, y, false, "ushr")?
                        .as_basic_value_enum())
                })?;
            }
            Opcode::And { dst, a, b } => {
                self.emit_binary_op(registers, reg_types, dst, a, b, "and", |b, av, bv| {
                    Ok(
                        b.build_and(av.into_int_value(), bv.into_int_value(), "and")?
                            .as_basic_value_enum(),
                    )
                })?;
            }
            Opcode::Or { dst, a, b } => {
                self.emit_binary_op(registers, reg_types, dst, a, b, "or", |b, av, bv| {
                    Ok(b.build_or(av.into_int_value(), bv.into_int_value(), "or")?
                        .as_basic_value_enum())
                })?;
            }
            Opcode::Xor { dst, a, b } => {
                self.emit_binary_op(registers, reg_types, dst, a, b, "xor", |b, av, bv| {
                    Ok(
                        b.build_xor(av.into_int_value(), bv.into_int_value(), "xor")?
                            .as_basic_value_enum(),
                    )
                })?;
            }
            Opcode::Neg { dst, src } => {
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "neg_src",
                )?;
                let result = match src_val.get_type().as_any_type_enum() {
                    AnyTypeEnum::IntType(_) => self
                        .builder
                        .build_int_neg(src_val.into_int_value(), "neg")?
                        .as_basic_value_enum(),
                    AnyTypeEnum::FloatType(_) => self
                        .builder
                        .build_float_neg(src_val.into_float_value(), "neg")?
                        .as_basic_value_enum(),
                    _ => return Err(anyhow!("Unsupported type for Neg")),
                };
                self.builder
                    .build_store(registers[dst.0 as usize], result)?;
            }
            Opcode::Not { dst, src } => {
                let src_val = self
                    .builder
                    .build_load(
                        reg_types[src.0 as usize],
                        registers[src.0 as usize],
                        "not_src",
                    )?
                    .into_int_value();
                let result = self.builder.build_not(src_val, "not")?;
                self.builder
                    .build_store(registers[dst.0 as usize], result)?;
            }
            Opcode::Incr { dst } => {
                let val = self
                    .builder
                    .build_load(
                        reg_types[dst.0 as usize],
                        registers[dst.0 as usize],
                        "incr_val",
                    )?
                    .into_int_value();
                let one = val.get_type().const_int(1, false);
                let result = self.builder.build_int_add(val, one, "incr")?;
                self.builder
                    .build_store(registers[dst.0 as usize], result)?;
            }
            Opcode::Decr { dst } => {
                let val = self
                    .builder
                    .build_load(
                        reg_types[dst.0 as usize],
                        registers[dst.0 as usize],
                        "decr_val",
                    )?
                    .into_int_value();
                let one = val.get_type().const_int(1, false);
                let result = self.builder.build_int_sub(val, one, "decr")?;
                self.builder
                    .build_store(registers[dst.0 as usize], result)?;
            }

            // --- Call4, CallN ---
            Opcode::Call4 {
                dst,
                fun,
                arg0,
                arg1,
                arg2,
                arg3,
            } => {
                let (function, is_placeholder) = self.get_or_create_function_value(fun.0)?;
                let args: Vec<BasicMetadataValueEnum> = [arg0, arg1, arg2, arg3]
                    .iter()
                    .map(|arg| {
                        self.builder
                            .build_load(
                                reg_types[arg.0 as usize],
                                registers[arg.0 as usize],
                                "arg_val",
                            )
                            .unwrap()
                            .into()
                    })
                    .collect();
                let result = self.builder.build_call(function, &args, "call")?;
                if let Some(ret_val) = result.try_as_basic_value().basic() {
                    self.builder
                        .build_store(registers[dst.0 as usize], ret_val)?;
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
                        self.builder
                            .build_load(
                                reg_types[arg.0 as usize],
                                registers[arg.0 as usize],
                                "arg_val",
                            )
                            .unwrap()
                            .into()
                    })
                    .collect();
                let result = self.builder.build_call(function, &arg_vals, "call")?;
                if let Some(ret_val) = result.try_as_basic_value().basic() {
                    self.builder
                        .build_store(registers[dst.0 as usize], ret_val)?;
                }
                if is_placeholder {
                    self.add_pending_compilation(fun.0);
                }
            }

            // --- IndirectCall: dispatch through functions_ptrs[findex] ---
            //
            // Emitted by the AIR IndirectCallRewritePass for hot-reload support.
            // Loads the callee address from the mutable function pointer table at
            // runtime, so recompiled functions are picked up without recompiling
            // the caller.
            Opcode::IndirectCall { dst, fun, args } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());

                // Build the ABI type directly from bytecode. Creating an LLVM
                // declaration here makes an isolated lazy module report an
                // unresolved Fun_* symbol even though the call itself goes
                // exclusively through functions_ptrs.
                let callee = self
                    .bytecode
                    .functions
                    .iter()
                    .find(|f| f.findex as usize == fun.0)
                    .ok_or_else(|| anyhow!("IndirectCall target {} is not bytecode", fun.0))?;
                let type_fun = self.bytecode.types[callee.type_.0]
                    .fun
                    .clone()
                    .ok_or_else(|| anyhow!("IndirectCall target {} has no function type", fun.0))?;
                let fn_type = self.create_function_type(&type_fun)?;

                // Load callee address from functions_ptrs[findex] at runtime
                let findex = fun.0;
                let fun_addr_ptr = self.function_slot_ptr(findex)?;
                let fun_addr = self
                    .builder
                    .build_load(ptr_type, fun_addr_ptr, "indirect_call_fn")?
                    .into_pointer_value();

                // Build argument values
                let arg_vals: Vec<BasicMetadataValueEnum> = args
                    .iter()
                    .map(|arg| {
                        self.builder
                            .build_load(
                                reg_types[arg.0 as usize],
                                registers[arg.0 as usize],
                                "arg_val",
                            )
                            .unwrap()
                            .into()
                    })
                    .collect();

                // Indirect call through the loaded pointer (stub-guarded:
                // functions_ptrs may hold interpreter sentinels in hybrid mode)
                if let Some(ret_val) =
                    self.build_stub_guarded_indirect_call(fn_type, fun_addr, &arg_vals, "icall")?
                {
                    self.builder
                        .build_store(registers[dst.0 as usize], ret_val)?;
                }
            }

            // --- GetThis / SetThis (delegate to Field/SetField with obj = reg 0) ---
            Opcode::GetThis { dst, field } => {
                let rewritten = Opcode::Field {
                    dst: *dst,
                    obj: crate::opcodes::Reg(0),
                    field: *field,
                };
                self.translate_opcode(f, &rewritten, registers, reg_types, i, opcode_blocks)?;
            }
            Opcode::SetThis { field, src } => {
                let rewritten = Opcode::SetField {
                    obj: crate::opcodes::Reg(0),
                    field: *field,
                    src: *src,
                };
                self.translate_opcode(f, &rewritten, registers, reg_types, i, opcode_blocks)?;
            }

            // --- CallMethod (compile-time proto resolution, runtime vtable for virtuals) ---
            Opcode::CallMethod { dst, field, args } => {
                let obj_type_idx = f.regs[args[0].0 as usize].0;
                let obj_type = &self.types_[obj_type_idx];
                let ptr_type = self.context.ptr_type(AddressSpace::default());

                if obj_type.kind == hl_type_kind_HVIRTUAL {
                    // Compile-time hash of the virtual field name, for the
                    // dynamic fallback (which resolves by hash, not slot).
                    let field_hash = obj_type
                        .virt
                        .as_ref()
                        .and_then(|v| v.fields.get(field.0))
                        .map(|fld| hl_hash_utf8(&fld.name))
                        .unwrap_or(0);

                    // HVIRTUAL dispatch: load function pointer from vfields[field]
                    let vvirt = self
                        .builder
                        .build_load(ptr_type, registers[args[0].0 as usize], "vvirt")?
                        .into_pointer_value();

                    let function = self
                        .builder
                        .get_insert_block()
                        .unwrap()
                        .get_parent()
                        .unwrap();
                    let nonnull_block = self.context.append_basic_block(function, "vcall_nonnull");
                    let vfields_block = self.context.append_basic_block(function, "vcall_vfields");
                    let direct_block = self.context.append_basic_block(function, "vcall_direct");
                    let fallback_block =
                        self.context.append_basic_block(function, "vcall_fallback");
                    let merge_block = self.context.append_basic_block(function, "vcall_merge");

                    // Runtime guard: at the hybrid interpreter/JIT boundary an
                    // HVIRTUAL-typed register can hold a plain HOBJ/HDYNOBJ
                    // pointer (the interpreter is dynamically typed). Only
                    // trust the vvirtual layout after checking the header's
                    // type kind; null or non-virtual goes to the hash-based
                    // fallback helper. Trusting the static type here read
                    // hl_type_obj ints as a vfields pointer — a deterministic
                    // SIGBUS at 0x2d00000058 on game.hl.
                    let vvirt_null = self.builder.build_is_null(vvirt, "vvirt_null")?;
                    self.builder.build_conditional_branch(
                        vvirt_null,
                        fallback_block,
                        nonnull_block,
                    )?;

                    self.builder.position_at_end(nonnull_block);
                    let hdr_type = self
                        .builder
                        .build_load(ptr_type, vvirt, "vvirt_type")?
                        .into_pointer_value();
                    let hdr_kind = self
                        .builder
                        .build_load(self.context.i32_type(), hdr_type, "vvirt_kind")?
                        .into_int_value();
                    let is_virt = self.builder.build_int_compare(
                        IntPredicate::EQ,
                        hdr_kind,
                        self.context
                            .i32_type()
                            .const_int(hl_type_kind_HVIRTUAL as u64, false),
                        "vvirt_is_virtual",
                    )?;
                    self.builder.build_conditional_branch(
                        is_virt,
                        vfields_block,
                        fallback_block,
                    )?;

                    // --- vfields path: a real vvirtual; try the resolved slot ---
                    self.builder.position_at_end(vfields_block);
                    // Load value (underlying object) from vvirtual offset 8
                    let value_gep = unsafe {
                        self.builder.build_gep(
                            self.context.i8_type(),
                            vvirt,
                            &[self
                                .context
                                .i64_type()
                                .const_int(self.target_abi.vvirtual_value_offset(), false)],
                            "vvirt_value_gep",
                        )?
                    };
                    let value = self
                        .builder
                        .build_load(ptr_type, value_gep, "vvirt_value")?
                        .into_pointer_value();

                    // Load vfields[field] from vvirtual offset 24 + field*8
                    let vfield_offset = self.target_abi.vvirtual_fields_offset()
                        + field.0 as u64 * self.target_abi.pointer_bytes() as u64;
                    let vfield_gep = unsafe {
                        self.builder.build_gep(
                            self.context.i8_type(),
                            vvirt,
                            &[self.context.i64_type().const_int(vfield_offset, false)],
                            "vfield_gep",
                        )?
                    };
                    let fn_ptr = self
                        .builder
                        .build_load(ptr_type, vfield_gep, "vfield_fn")?
                        .into_pointer_value();

                    // Check if vfield is null (type mismatch — need dynamic fallback)
                    let is_null = self.builder.build_is_null(fn_ptr, "vfield_null")?;
                    self.builder
                        .build_conditional_branch(is_null, fallback_block, direct_block)?;

                    // Look up virtual field's declared function type to get correct param types.
                    // Extract type indices first to avoid borrow conflicts with self.
                    let virt_fn_info: Option<(Vec<usize>, usize)> = obj_type
                        .virt
                        .as_ref()
                        .and_then(|v| v.fields.get(field.0))
                        .and_then(|fld| {
                            let ft = &self.types_[fld.type_.0];
                            if ft.kind == hl_type_kind_HFUN {
                                ft.fun.as_ref().map(|fun| {
                                    let arg_indices: Vec<usize> =
                                        fun.args.iter().map(|a| a.0).collect();
                                    (arg_indices, fun.ret.0)
                                })
                            } else {
                                None
                            }
                        });

                    // Convert type indices to LLVM types (now safe to call get_register_type)
                    let virt_fn_args: Option<Vec<BasicTypeEnum>> =
                        if let Some((ref arg_indices, _)) = virt_fn_info {
                            let mut types = vec![ptr_type.as_basic_type_enum()];
                            for &idx in arg_indices {
                                types.push(self.get_register_type(idx).unwrap_or(ptr_type.into()));
                            }
                            Some(types)
                        } else {
                            None
                        };
                    let virt_ret_type: Option<BasicTypeEnum> =
                        if let Some((_, ret_idx)) = virt_fn_info {
                            Some(self.get_register_type(ret_idx).unwrap_or(ptr_type.into()))
                        } else {
                            None
                        };

                    // Build fn_type from the virtual's declared types (not register types)
                    let mut arg_types: Vec<BasicMetadataTypeEnum> = Vec::with_capacity(args.len());
                    if let Some(ref fn_args) = virt_fn_args {
                        for t in fn_args.iter() {
                            arg_types.push((*t).into());
                        }
                    } else {
                        arg_types.push(ptr_type.into());
                        for arg in &args[1..] {
                            arg_types.push(reg_types[arg.0 as usize].into());
                        }
                    }
                    let dst_kind = self.types_[f.regs[dst.0 as usize].0].kind;
                    let ret_type = virt_ret_type;
                    let fn_type = if dst_kind == hl_type_kind_HVOID {
                        self.context.void_type().fn_type(&arg_types, false)
                    } else if let Some(rt) = ret_type {
                        rt.fn_type(&arg_types, false)
                    } else {
                        reg_types[dst.0 as usize].fn_type(&arg_types, false)
                    };

                    // Emit the tail (non-this) argument loads with casts to the
                    // declared param types, in the current insert block.
                    let build_tail_args =
                        |this: &JITModule<'ctx>| -> Result<Vec<BasicMetadataValueEnum<'ctx>>> {
                            let mut vals: Vec<BasicMetadataValueEnum> =
                                Vec::with_capacity(args.len().saturating_sub(1));
                            for (idx, arg) in args[1..].iter().enumerate() {
                                let loaded = this.builder.build_load(
                                    reg_types[arg.0 as usize],
                                    registers[arg.0 as usize],
                                    "arg_val",
                                )?;
                                // Cast to match the declared function param type
                                if let Some(ref fn_args) = virt_fn_args {
                                    let param_idx = idx + 1; // +1 for 'this'
                                    if param_idx < fn_args.len() {
                                        let expected = fn_args[param_idx];
                                        if loaded.get_type() != expected {
                                            let casted = this.cast_for_call(loaded, expected)?;
                                            vals.push(casted.into());
                                            continue;
                                        }
                                    }
                                }
                                vals.push(loaded.into());
                            }
                            Ok(vals)
                        };

                    // --- Direct path: vfield is resolved, call it (stub-guarded) ---
                    self.builder.position_at_end(direct_block);
                    let mut arg_vals: Vec<BasicMetadataValueEnum> = Vec::with_capacity(args.len());
                    arg_vals.push(value.into());
                    arg_vals.extend(build_tail_args(self)?);
                    if let Some(ret_val) = self.build_stub_guarded_indirect_call(
                        fn_type,
                        fn_ptr,
                        &arg_vals,
                        "vcall_virt",
                    )? {
                        let store_val = if ret_val.get_type() != reg_types[dst.0 as usize] {
                            self.cast_for_call(ret_val, reg_types[dst.0 as usize])?
                        } else {
                            ret_val
                        };
                        self.builder
                            .build_store(registers[dst.0 as usize], store_val)?;
                    }
                    self.builder.build_unconditional_branch(merge_block)?;

                    // --- Fallback path: not a vvirtual, or vfield is null ---
                    // The vfield being null means the DECLARED signature is
                    // not the implementation's: the interface says
                    // Iterator<Int>.next is () -> i32 while the generic
                    // implementation behind it was compiled () -> Dynamic.
                    // Calling the resolved pointer through the declared ABI
                    // reads the low 32 bits of a returned box pointer as the
                    // value (map iteration over Ints yielded truncated
                    // vdynamic addresses). So this path never guesses an ABI:
                    // box every argument by its static register type, let
                    // hlp_vcall_dyn call the method through its OWN runtime
                    // type, and dyn-cast the boxed result to the declared
                    // kind.
                    self.builder.position_at_end(fallback_block);
                    let i32_type = self.context.i32_type();
                    let n_tail = args.len() - 1;
                    let arr_val: BasicValueEnum = if n_tail == 0 {
                        ptr_type.const_null().into()
                    } else {
                        let alloc_arr = self.declare_native(
                            "hlp_alloc_dyn_array",
                            &[i32_type.into()],
                            Some(ptr_type.into()),
                        );
                        let arr = self
                            .builder
                            .build_call(
                                alloc_arr,
                                &[i32_type.const_int(n_tail as u64, false).into()],
                                "vcall_args_arr",
                            )?
                            .try_as_basic_value()
                            .basic()
                            .unwrap()
                            .into_pointer_value();
                        let make_dyn = self.declare_native(
                            "hlp_make_dyn",
                            &[ptr_type.into(), ptr_type.into()],
                            Some(ptr_type.into()),
                        );
                        for (i, arg) in args[1..].iter().enumerate() {
                            let src_type_idx = f.regs[arg.0 as usize].0;
                            let loaded = self.builder.build_load(
                                reg_types[arg.0 as usize],
                                registers[arg.0 as usize],
                                "vcall_arg",
                            )?;
                            // Same boxing rule as ToDyn: pointers are already
                            // dyn-compatible (except HABSTRACT), primitives go
                            // through hlp_make_dyn with their static type.
                            let src_is_abstract =
                                self.types_[src_type_idx].kind == hl_type_kind_HABSTRACT;
                            let boxed: BasicValueEnum =
                                if loaded.is_pointer_value() && !src_is_abstract {
                                    loaded
                                } else {
                                    let temp =
                                        self.entry_alloca(loaded.get_type(), "vcall_box_slot")?;
                                    self.builder.build_store(temp, loaded)?;
                                    let type_ptr = self
                                        .get_initialized_type(src_type_idx)?
                                        .into_pointer_value();
                                    self.builder
                                        .build_call(
                                            make_dyn,
                                            &[temp.into(), type_ptr.into()],
                                            "vcall_box",
                                        )?
                                        .try_as_basic_value()
                                        .basic()
                                        .unwrap()
                                };
                            // varray data starts at offset 24.
                            let slot_gep = unsafe {
                                self.builder.build_gep(
                                    self.context.i8_type(),
                                    arr,
                                    &[self.context.i64_type().const_int(
                                        self.target_abi.varray_data_offset()
                                            + i as u64 * self.target_abi.pointer_bytes() as u64,
                                        false,
                                    )],
                                    "vcall_arg_gep",
                                )?
                            };
                            self.builder.build_store(slot_gep, boxed)?;
                        }
                        arr.into()
                    };
                    let vcall = self.declare_native(
                        "hlp_vcall_dyn",
                        &[ptr_type.into(), i32_type.into(), ptr_type.into()],
                        Some(ptr_type.into()),
                    );
                    let hash_val = i32_type.const_int(field_hash as u32 as u64, false);
                    let ret_dyn = self
                        .builder
                        .build_call(
                            vcall,
                            &[vvirt.into(), hash_val.into(), arr_val.into()],
                            "vcall_dyn",
                        )?
                        .try_as_basic_value()
                        .basic()
                        .unwrap()
                        .into_pointer_value();
                    if dst_kind != hl_type_kind_HVOID {
                        let dst_ty = reg_types[dst.0 as usize];
                        let store_val: BasicValueEnum = if dst_kind == hl_type_kind_HVIRTUAL {
                            // The callee returns ITS declared type. When that is
                            // a view of another virtual type (IntMap.keys hands
                            // back an Iterator<Int> where the caller's erased
                            // field is Iterator<Dynamic>), storing it as-is
                            // leaves a view whose method slots were resolved
                            // for the other type: a later direct call read an
                            // i32 return as a pointer. HashLink's dynamic call
                            // casts the result to the caller's type; do the
                            // same. hl_to_virtual returns a same-typed view
                            // unchanged and null for null.
                            let dst_vt = self
                                .get_initialized_type(f.regs[dst.0 as usize].0)?
                                .into_pointer_value();
                            let to_virtual = self.declare_native(
                                "hl_to_virtual",
                                &[ptr_type.into(), ptr_type.into()],
                                Some(ptr_type.into()),
                            );
                            self.builder
                                .build_call(
                                    to_virtual,
                                    &[dst_vt.into(), ret_dyn.into()],
                                    "vcall_ret_view",
                                )?
                                .try_as_basic_value()
                                .basic()
                                .unwrap()
                        } else if dst_kind == hl_type_kind_HDYN {
                            // A Dynamic destination: the box is the value.
                            ret_dyn.into()
                        } else if dst_ty.is_pointer_type() {
                            // The callee returned ITS declared type, which is
                            // not necessarily the caller's. `ArrayBytes_Int.map`
                            // hands back an ArrayDyn where the structural type
                            // says Array<Int>; storing that pointer as-is read
                            // the ArrayDyn through ArrayBytes_Int's layout --
                            // `length` was the inner array's address and
                            // `bytes` its allowReinterpret flag, 0x1, which is
                            // the SIGSEGV at 0x1 in unit suite Issue2889. The
                            // interpreter and the Cranelift tier run the
                            // runtime's dynamic cast here: it walks the super
                            // chain and otherwise asks the object's own __cast,
                            // which for ArrayDyn reinterprets into the typed
                            // array. Same as this file's CallClosure dynamic
                            // path. Null passes through as null.
                            let dyn_type_index = self
                                .types_
                                .iter()
                                .position(|ty| ty.kind == hl_type_kind_HDYN)
                                .ok_or_else(|| anyhow!("module has no HDYN runtime type"))?;
                            let dyn_type = self
                                .get_initialized_type(dyn_type_index)?
                                .into_pointer_value();
                            let dst_runtime_type = self
                                .get_initialized_type(f.regs[dst.0 as usize].0)?
                                .into_pointer_value();
                            let result_slot =
                                self.entry_alloca(ptr_type, "vcall_dyn_result_slot")?;
                            self.builder.build_store(result_slot, ret_dyn)?;
                            let castp = self.declare_native(
                                "hlp_dyn_castp",
                                &[ptr_type.into(), ptr_type.into(), ptr_type.into()],
                                Some(ptr_type.into()),
                            );
                            self.builder
                                .build_call(
                                    castp,
                                    &[result_slot.into(), dyn_type.into(), dst_runtime_type.into()],
                                    "vcall_dyn_result_cast",
                                )?
                                .try_as_basic_value()
                                .basic()
                                .ok_or_else(|| anyhow!("hlp_dyn_castp returned void"))?
                        } else {
                            // Primitive dst: dyn-cast the box (null -> zero,
                            // numeric coercion when the box holds a wider
                            // kind), then narrow to the register width.
                            let (helper, helper_ret): (&str, BasicTypeEnum) =
                                if dst_kind == hl_type_kind_HF64 {
                                    ("hlp_dyn_todouble", self.context.f64_type().into())
                                } else if dst_kind == hl_type_kind_HF32 {
                                    ("hlp_dyn_tofloat", self.context.f32_type().into())
                                } else if dst_kind == hl_type_kind_HI64 {
                                    ("hlp_dyn_toi64", self.context.i64_type().into())
                                } else {
                                    ("hlp_dyn_toint", i32_type.into())
                                };
                            let unbox =
                                self.declare_native(helper, &[ptr_type.into()], Some(helper_ret));
                            let raw = self
                                .builder
                                .build_call(unbox, &[ret_dyn.into()], "vcall_unbox")?
                                .try_as_basic_value()
                                .basic()
                                .unwrap();
                            if raw.get_type() != dst_ty {
                                self.cast_for_call(raw, dst_ty)?
                            } else {
                                raw
                            }
                        };
                        self.builder
                            .build_store(registers[dst.0 as usize], store_val)?;
                    }
                    self.builder.build_unconditional_branch(merge_block)?;

                    // Continue at merge
                    self.builder.position_at_end(merge_block);
                } else if let Some(findex) = {
                    // field.0 is the vtable slot index (vobj_proto index).
                    // Find the proto entry whose pindex matches field.0 to
                    // get the findex for the function signature -- walking
                    // the SUPER chain: a subclass's own proto list holds
                    // only the methods it declares, so a call through a
                    // subclass-typed receiver to an inherited method found
                    // nothing here and fell through to a path that did
                    // nothing at all (a Gem's onMarbleInside called on a
                    // DtsObject receiver, the game's item pickup).
                    let mut found: Option<usize> = None;
                    let mut cur = Some(obj_type_idx);
                    while let Some(ti) = cur {
                        let Some(obj) = self.types_[ti].obj.as_ref() else {
                            break;
                        };
                        if let Some(p) = obj.proto.iter().find(|p| p.pindex as usize == field.0) {
                            found = Some(p.findex as usize);
                            break;
                        }
                        cur = obj.super_.as_ref().map(|t| t.0);
                    }
                    found
                } {
                    // Runtime vtable dispatch for HOBJ/HSTRUCT.
                    // field.0 is the vobj_proto slot index.
                    let vtable_slot = field.0 as u64;

                    // Get base function type for constructing the indirect call fn_type
                    let (function, is_placeholder) = self.get_or_create_function_value(findex)?;
                    let param_types: Vec<BasicTypeEnum> = function
                        .get_type()
                        .get_param_types()
                        .into_iter()
                        .map(|t| {
                            BasicTypeEnum::try_from(t)
                                .expect("unsupported metadata param type in method call")
                        })
                        .collect();
                    let fn_type = function.get_type();

                    // Load object pointer
                    let obj_val = self
                        .builder
                        .build_load(ptr_type, registers[args[0].0 as usize], "cm_obj")?
                        .into_pointer_value();

                    // Load hl_type* from object (offset 0)
                    let type_ptr = self
                        .builder
                        .build_load(ptr_type, obj_val, "cm_type")?
                        .into_pointer_value();

                    // Build arg values with type casting (shared by both the
                    // devirtualised and the vtable arm below).
                    let expected_params = function.count_params() as usize;
                    let mut arg_vals: Vec<BasicMetadataValueEnum> =
                        Vec::with_capacity(expected_params);
                    for (idx, arg) in args.iter().enumerate() {
                        if idx >= expected_params {
                            break;
                        }
                        let loaded = self.builder.build_load(
                            reg_types[arg.0 as usize],
                            registers[arg.0 as usize],
                            "arg_val",
                        )?;
                        if idx < param_types.len() {
                            let expected = param_types[idx];
                            if loaded.get_type() != expected {
                                let casted = self.cast_for_call(loaded, expected)?;
                                arg_vals.push(casted.into());
                            } else {
                                arg_vals.push(loaded.into());
                            }
                        } else {
                            arg_vals.push(loaded.into());
                        }
                    }
                    while arg_vals.len() < expected_params {
                        let param_type = param_types[arg_vals.len()];
                        arg_vals.push(param_type.const_zero().into());
                    }

                    // Guarded devirtualisation, same reasoning as CallClosure
                    // below: the interpreter watched this site dispatch, and a
                    // site that only ever saw one receiver type gets a
                    // type-header compare -- the header pointer never moves,
                    // unlike the vtable SLOT, which promotion patches -- and a
                    // direct call the inliner can take. A different receiver
                    // falls into the vtable path unchanged.
                    let devirt = if self.hot_reload {
                        None
                    } else {
                        let caller = f.findex as u32;
                        crate::callsite_profile::method_receiver(caller, i as u32)
                            .or_else(|| crate::callsite_profile::uniform_method_receiver(caller))
                            .and_then(|(type_ptr_c, target)| {
                                match self.get_or_create_function_value(target as usize) {
                                    Ok((callee, ph)) => {
                                        if ph {
                                            self.add_pending_compilation(target as usize);
                                        }
                                        (callee.get_type() == fn_type)
                                            .then_some((callee, type_ptr_c))
                                    }
                                    Err(_) => None,
                                }
                            })
                    };

                    let cm_function = self
                        .builder
                        .get_insert_block()
                        .unwrap()
                        .get_parent()
                        .unwrap();
                    let cm_done_bb = self.context.append_basic_block(cm_function, "cm_done");

                    if let Some((callee, type_ptr_c)) = devirt {
                        crate::profile::count("devirt method fast-arm", 1);
                        // An object's type header is written once, at
                        // allocation, and the object is live for as long as
                        // anything can load through this pointer -- so the
                        // load is `!invariant.load`, which is what lets LICM
                        // hoist the whole guard out of the loop and leave the
                        // fast arm's inlined body running guard-free.
                        if let Some(inst) = type_ptr.as_instruction() {
                            let _ = inst.set_metadata(
                                self.context.metadata_node(&[]),
                                self.context.get_kind_id("invariant.load"),
                            );
                        }
                        let hit_bb = self
                            .context
                            .append_basic_block(cm_function, "cm_devirt_hit");
                        let miss_bb = self
                            .context
                            .append_basic_block(cm_function, "cm_devirt_miss");
                        let type_int = self.builder.build_ptr_to_int(
                            type_ptr,
                            self.context.i64_type(),
                            "cm_type_int",
                        )?;
                        let guard = self.builder.build_int_compare(
                            IntPredicate::EQ,
                            type_int,
                            self.context.i64_type().const_int(type_ptr_c, false),
                            "cm_devirt_guard",
                        )?;
                        self.builder
                            .build_conditional_branch(guard, hit_bb, miss_bb)?;

                        self.builder.position_at_end(hit_bb);
                        let ret = self
                            .builder
                            .build_call(callee, &arg_vals, "cm_devirt_call")?
                            .try_as_basic_value();
                        if let Some(rv) = ret.basic() {
                            self.builder.build_store(registers[dst.0 as usize], rv)?;
                        }
                        self.builder.build_unconditional_branch(cm_done_bb)?;

                        self.builder.position_at_end(miss_bb);
                    }

                    let vobj_proto = self.vobj_proto_ptr(type_ptr)?;

                    // Load method pointer from vobj_proto[field.0]
                    let method_gep = unsafe {
                        self.builder.build_gep(
                            ptr_type,
                            vobj_proto,
                            &[self.context.i32_type().const_int(vtable_slot, false)],
                            "method_gep",
                        )?
                    };
                    let method_ptr = self
                        .builder
                        .build_load(ptr_type, method_gep, "method_ptr")?
                        .into_pointer_value();

                    // Ahead-of-time devirtualisation, from a profile a
                    // previous run left behind.
                    //
                    // The JIT arm above anchors its guard on the receiver's
                    // type header, which it can do because it compiles inside
                    // the process that watched the dispatch. AOT has no such
                    // address, so it guards on the slot it just loaded: if the
                    // vtable resolves to the function the profile named, call
                    // that function directly and let the inliner take it.
                    // Wrong or stale profiles cost one compare.
                    //
                    // The load is `!invariant.load` here and not in the JIT
                    // for a real reason: promotion patches vtable SLOTS, so
                    // there the value genuinely changes. Nothing patches
                    // anything in a finished object, which is what lets LICM
                    // lift the whole guard out of a dispatch loop and leave
                    // the inlined body running without it.
                    let aot_devirt = if self.aot {
                        self.function_name(f.findex as u32)
                            .and_then(|caller| crate::callsite_profile::aot_target_for(&caller))
                            .and_then(|target_name| self.findex_for_name(&target_name))
                            .and_then(|target| {
                                match self.get_or_create_function_value(target as usize) {
                                    Ok((callee, ph)) => {
                                        if ph {
                                            self.add_pending_compilation(target as usize);
                                        }
                                        (callee.get_type() == fn_type).then_some(callee)
                                    }
                                    Err(_) => None,
                                }
                            })
                    } else {
                        None
                    };

                    if let Some(callee) = aot_devirt {
                        crate::profile::count("devirt method aot-arm", 1);
                        if let Some(inst) = method_ptr.as_instruction() {
                            let _ = inst.set_metadata(
                                self.context.metadata_node(&[]),
                                self.context.get_kind_id("invariant.load"),
                            );
                        }
                        let hit_bb = self
                            .context
                            .append_basic_block(cm_function, "cm_aot_devirt_hit");
                        let miss_bb = self
                            .context
                            .append_basic_block(cm_function, "cm_aot_devirt_miss");
                        let want = callee.as_global_value().as_pointer_value();
                        let guard = self.builder.build_int_compare(
                            IntPredicate::EQ,
                            self.builder.build_ptr_to_int(
                                method_ptr,
                                self.context.i64_type(),
                                "cm_aot_slot",
                            )?,
                            self.builder.build_ptr_to_int(
                                want,
                                self.context.i64_type(),
                                "cm_aot_want",
                            )?,
                            "cm_aot_devirt_guard",
                        )?;
                        self.builder
                            .build_conditional_branch(guard, hit_bb, miss_bb)?;

                        self.builder.position_at_end(hit_bb);
                        let ret = self
                            .builder
                            .build_call(callee, &arg_vals, "cm_aot_devirt_call")?
                            .try_as_basic_value();
                        if let Some(rv) = ret.basic() {
                            self.builder.build_store(registers[dst.0 as usize], rv)?;
                        }
                        self.builder.build_unconditional_branch(cm_done_bb)?;
                        self.builder.position_at_end(miss_bb);
                    }

                    // Indirect call through the vtable method pointer
                    // (stub-guarded: vobj_proto slots may hold interpreter
                    // sentinels in hybrid mode)
                    if let Some(ret_val) = self.build_stub_guarded_indirect_call(
                        fn_type,
                        method_ptr,
                        &arg_vals,
                        "call_method",
                    )? {
                        self.builder
                            .build_store(registers[dst.0 as usize], ret_val)?;
                    }
                    if is_placeholder {
                        self.add_pending_compilation(findex);
                    }
                    self.builder.build_unconditional_branch(cm_done_bb)?;
                    self.builder.position_at_end(cm_done_bb);
                } else {
                    // Runtime dispatch via hl_runtime_obj.methods table
                    let obj_val = self
                        .builder
                        .build_load(ptr_type, registers[args[0].0 as usize], "vobj")?
                        .into_pointer_value();

                    // Load hl_type* from obj (offset 0)
                    let obj_type_ptr = self
                        .builder
                        .build_load(ptr_type, obj_val, "obj_type")?
                        .into_pointer_value();

                    // Call hlp_get_obj_rt to get hl_runtime_obj*
                    let hl_get_obj_rt = self.declare_native(
                        "hlp_get_obj_rt",
                        &[ptr_type.into()],
                        Some(ptr_type.into()),
                    );
                    let rt_obj = self
                        .builder
                        .build_call(hl_get_obj_rt, &[obj_type_ptr.into()], "rt_obj")?
                        .try_as_basic_value()
                        .basic()
                        .unwrap()
                        .into_pointer_value();

                    // Load the methods pointer from the target's C layout.
                    let methods_gep = unsafe {
                        self.builder.build_gep(
                            self.context.i8_type(),
                            rt_obj,
                            &[self
                                .context
                                .i64_type()
                                .const_int(self.target_abi.hl_runtime_obj_methods_offset(), false)],
                            "methods_gep",
                        )?
                    };
                    let methods_ptr = self
                        .builder
                        .build_load(ptr_type, methods_gep, "methods")?
                        .into_pointer_value();

                    // Load function pointer from methods[field]
                    let fn_ptr_gep = unsafe {
                        self.builder.build_gep(
                            ptr_type,
                            methods_ptr,
                            &[self.context.i32_type().const_int(field.0 as u64, false)],
                            "fn_ptr_gep",
                        )?
                    };
                    let fn_ptr = self
                        .builder
                        .build_load(ptr_type, fn_ptr_gep, "fn_ptr")?
                        .into_pointer_value();

                    // Build args and function type
                    let arg_vals: Vec<BasicMetadataValueEnum> = args
                        .iter()
                        .map(|arg| {
                            self.builder
                                .build_load(
                                    reg_types[arg.0 as usize],
                                    registers[arg.0 as usize],
                                    "arg_val",
                                )
                                .unwrap()
                                .into()
                        })
                        .collect();

                    let arg_types: Vec<BasicMetadataTypeEnum> = args
                        .iter()
                        .map(|arg| reg_types[arg.0 as usize].into())
                        .collect();

                    let dst_kind = self.types_[f.regs[dst.0 as usize].0].kind;
                    let fn_type = if dst_kind == hl_type_kind_HVOID {
                        self.context.void_type().fn_type(&arg_types, false)
                    } else {
                        reg_types[dst.0 as usize].fn_type(&arg_types, false)
                    };

                    if let Some(ret_val) =
                        self.build_stub_guarded_indirect_call(fn_type, fn_ptr, &arg_vals, "vcall")?
                    {
                        self.builder
                            .build_store(registers[dst.0 as usize], ret_val)?;
                    }
                }
            }
            // --- CallThis (same as CallMethod HOBJ vtable dispatch, this = reg 0) ---
            Opcode::CallThis { dst, field, args } => {
                let obj_type_idx = f.regs[0].0;
                let ptr_type = self.context.ptr_type(AddressSpace::default());

                // field.0 is the vtable slot index (vobj_proto index).
                // Find the proto entry whose pindex matches field.0 to get the
                // findex for the function signature, walking the super chain
                // since this-calls often resolve to ancestor methods.
                let findex = {
                    let mut found: Option<usize> = None;
                    let mut cur_obj = self.types_[obj_type_idx].obj.as_ref();
                    while let Some(obj) = cur_obj {
                        if let Some(p) = obj.proto.iter().find(|p| p.pindex as usize == field.0) {
                            found = Some(p.findex as usize);
                            break;
                        }
                        cur_obj = obj
                            .super_
                            .as_ref()
                            .and_then(|s| self.types_[s.0].obj.as_ref());
                    }
                    found.ok_or_else(|| {
                        anyhow!(
                            "CallThis: cannot resolve vtable slot {} on type {}",
                            field.0,
                            obj_type_idx
                        )
                    })?
                };

                // Runtime vtable dispatch for HOBJ/HSTRUCT.
                // field.0 is the vobj_proto slot index.
                let vtable_slot = field.0 as u64;

                // Get base function type for constructing the indirect call fn_type
                let (function, is_placeholder) = self.get_or_create_function_value(findex)?;
                let param_types: Vec<BasicTypeEnum> = function
                    .get_type()
                    .get_param_types()
                    .into_iter()
                    .map(|t| {
                        BasicTypeEnum::try_from(t)
                            .expect("unsupported metadata param type in method call")
                    })
                    .collect();
                let fn_type = function.get_type();

                // Load `this` object pointer (reg 0)
                let obj_val = self
                    .builder
                    .build_load(ptr_type, registers[0], "ct_obj")?
                    .into_pointer_value();

                // Load hl_type* from object (offset 0)
                let type_ptr = self
                    .builder
                    .build_load(ptr_type, obj_val, "ct_type")?
                    .into_pointer_value();

                let vobj_proto = self.vobj_proto_ptr(type_ptr)?;

                // Load method pointer from vobj_proto[field.0]
                let method_gep = unsafe {
                    self.builder.build_gep(
                        ptr_type,
                        vobj_proto,
                        &[self.context.i32_type().const_int(vtable_slot, false)],
                        "method_gep",
                    )?
                };
                let method_ptr = self
                    .builder
                    .build_load(ptr_type, method_gep, "method_ptr")?
                    .into_pointer_value();

                // Build arg values with type casting; this (reg 0) comes first
                let expected_params = function.count_params() as usize;
                let mut arg_vals: Vec<BasicMetadataValueEnum> = Vec::with_capacity(expected_params);
                for (idx, reg_idx) in std::iter::once(0usize)
                    .chain(args.iter().map(|arg| arg.0 as usize))
                    .enumerate()
                {
                    if idx >= expected_params {
                        break;
                    }
                    let loaded = self.builder.build_load(
                        reg_types[reg_idx],
                        registers[reg_idx],
                        "arg_val",
                    )?;
                    if idx < param_types.len() {
                        let expected = param_types[idx];
                        if loaded.get_type() != expected {
                            let casted = self.cast_for_call(loaded, expected)?;
                            arg_vals.push(casted.into());
                        } else {
                            arg_vals.push(loaded.into());
                        }
                    } else {
                        arg_vals.push(loaded.into());
                    }
                }
                while arg_vals.len() < expected_params {
                    let param_type = param_types[arg_vals.len()];
                    arg_vals.push(param_type.const_zero().into());
                }

                // Indirect call through the vtable method pointer
                // (stub-guarded: vobj_proto slots may hold interpreter
                // sentinels in hybrid mode)
                if let Some(ret_val) = self.build_stub_guarded_indirect_call(
                    fn_type,
                    method_ptr,
                    &arg_vals,
                    "call_this",
                )? {
                    self.builder
                        .build_store(registers[dst.0 as usize], ret_val)?;
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
                // For pointer types (objects, strings, etc.), just copy the pointer.
                // HABSTRACT is excepted: it is a pointer whose target has no
                // hl_type header, so a Dynamic holding it raw makes the
                // hl_dyn_castp on the way back out read the payload as a type.
                let src_is_abstract = self.types_[src_type_idx].kind == hl_type_kind_HABSTRACT;
                if src_val.is_pointer_value() && !src_is_abstract {
                    self.builder
                        .build_store(registers[dst.0 as usize], src_val)?;
                } else {
                    // Primitives: alloca temp, store value, call hlp_make_dyn(&temp, type_ptr)
                    let ptr_type = self.context.ptr_type(AddressSpace::default());
                    let temp = self.entry_alloca(reg_types[src.0 as usize], "todyn_temp")?;
                    self.builder.build_store(temp, src_val)?;

                    let type_ptr = self
                        .get_initialized_type(src_type_idx)?
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
                        result.try_as_basic_value().basic().unwrap(),
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
                self.builder
                    .build_store(registers[dst.0 as usize], src_val)?;
            }

            // --- ToSFloat ---
            Opcode::ToSFloat { dst, src } => {
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "tosfloat_src",
                )?;
                let f64_type = self.context.f64_type();
                let src_kind = self.types_[f.regs[src.0 as usize].0].kind;
                let src_unsigned = src_kind == hl_type_kind_HUI8 || src_kind == hl_type_kind_HUI16;
                let result: BasicValueEnum = if src_val.is_int_value() {
                    // A byte or short register is unsigned in HashLink (MOVZX
                    // before CVTSI2SD); sitofp read 200 as -56.
                    if src_unsigned {
                        self.builder
                            .build_unsigned_int_to_float(
                                src_val.into_int_value(),
                                f64_type,
                                "tosfloat",
                            )?
                            .into()
                    } else {
                        self.builder
                            .build_signed_int_to_float(
                                src_val.into_int_value(),
                                f64_type,
                                "tosfloat",
                            )?
                            .into()
                    }
                } else if src_val.is_float_value() {
                    // Already float — just ensure it's f64
                    let fv = src_val.into_float_value();
                    if fv.get_type() == self.context.f32_type() {
                        self.builder
                            .build_float_ext(fv, f64_type, "tosfloat_ext")?
                            .into()
                    } else {
                        fv.into()
                    }
                } else {
                    return Err(anyhow!("ToSFloat: unexpected source type"));
                };
                self.store_float_as_reg(
                    &registers,
                    &reg_types,
                    dst.0 as usize,
                    result.into_float_value(),
                )?;
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
                    self.builder
                        .build_unsigned_int_to_float(
                            src_val.into_int_value(),
                            f64_type,
                            "toufloat",
                        )?
                        .into()
                } else if src_val.is_float_value() {
                    let fv = src_val.into_float_value();
                    if fv.get_type() == self.context.f32_type() {
                        self.builder
                            .build_float_ext(fv, f64_type, "toufloat_ext")?
                            .into()
                    } else {
                        fv.into()
                    }
                } else {
                    return Err(anyhow!("ToUFloat: unexpected source type"));
                };
                self.store_float_as_reg(
                    &registers,
                    &reg_types,
                    dst.0 as usize,
                    result.into_float_value(),
                )?;
            }

            // --- ToInt ---
            Opcode::ToInt { dst, src } => {
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "toint_src",
                )?;
                // Convert straight to the destination register's width. Going
                // through i32 first then widening with cast_for_call (a zext)
                // turned -1 into 4294967295 for I32 -> I64 and truncated any
                // Float beyond 2^31 for F64 -> I64. HashLink: MOVSXD / CVTTSD2SI
                // at the destination width. HUI8/HUI16 registers hold unsigned
                // values (MOVZX), so those widen with zero extension.
                let dst_ty = reg_types[dst.0 as usize];
                let dst_int = if dst_ty.is_int_type() {
                    dst_ty.into_int_type()
                } else {
                    self.context.i32_type()
                };
                let src_kind = self.types_[f.regs[src.0 as usize].0].kind;
                let src_unsigned = src_kind == hl_type_kind_HUI8 || src_kind == hl_type_kind_HUI16;
                let result: BasicValueEnum = if src_val.is_float_value() {
                    self.build_float_to_int_saturating(
                        src_val.into_float_value(),
                        dst_int,
                        "toint",
                    )?
                    .into()
                } else if src_val.is_int_value() {
                    let iv = src_val.into_int_value();
                    let sw = iv.get_type().get_bit_width();
                    let dw = dst_int.get_bit_width();
                    if sw > dw {
                        self.builder
                            .build_int_truncate(iv, dst_int, "toint_trunc")?
                            .into()
                    } else if sw < dw {
                        if src_unsigned {
                            self.builder
                                .build_int_z_extend(iv, dst_int, "toint_zext")?
                                .into()
                        } else {
                            self.builder
                                .build_int_s_extend(iv, dst_int, "toint_sext")?
                                .into()
                        }
                    } else {
                        iv.into()
                    }
                } else {
                    return Err(anyhow!("ToInt: unexpected source type"));
                };
                let result = self.cast_for_call(result, reg_types[dst.0 as usize])?;
                self.builder
                    .build_store(registers[dst.0 as usize], result)?;
            }
            Opcode::StaticClosure { dst, fun } => {
                if !self.lazy_compilation {
                    let (_function, is_placeholder) = self.get_or_create_function_value(fun.0)?;
                    if is_placeholder {
                        self.add_pending_compilation(fun.0);
                    }
                }

                let ptr_type = self.context.ptr_type(AddressSpace::default());

                // Load function address from functions_ptrs[findex] at runtime
                let findex = fun.0 as usize;
                let fun_addr_ptr = self.function_slot_ptr(findex)?;
                let fun_addr = self
                    .builder
                    .build_load(ptr_type, fun_addr_ptr, "static_closure_fun")?
                    .into_pointer_value();

                // Get function type pointer (compile-time constant from func_types)
                let type_ptr = self.func_type_ptr(findex)?;

                let closure = if self.aot {
                    self.emit_static_closure(findex, type_ptr)?
                } else {
                    let alloc_closure = self.declare_native(
                        "hlp_alloc_closure_void",
                        &[ptr_type.into(), ptr_type.into()],
                        Some(ptr_type.into()),
                    );
                    self.builder
                        .build_call(
                            alloc_closure,
                            &[type_ptr.into(), fun_addr.into()],
                            "static_closure",
                        )?
                        .try_as_basic_value()
                        .basic()
                        .unwrap()
                };
                self.builder
                    .build_store(registers[dst.0 as usize], closure)?;
            }

            // --- CallClosure ---
            Opcode::CallClosure { dst, fun, args } => {
                let raw_closure_ptr = self
                    .builder
                    .build_load(
                        reg_types[fun.0 as usize],
                        registers[fun.0 as usize],
                        "closure_ptr",
                    )?
                    .into_pointer_value();

                let i8_type = self.context.i8_type();
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let i32_type = self.context.i32_type();

                // HashLink represents a signature-adapted bound closure as a
                // vclosure_wrapper: its public vclosure has hasValue == 2 and
                // the original closure lives after it at offset 32. Hybrid's
                // interpreter runner unwraps this object; LLVM must do the
                // same before reading the callable fields.
                let raw_has_value_gep = unsafe {
                    self.builder.build_gep(
                        i8_type,
                        raw_closure_ptr,
                        &[self
                            .context
                            .i64_type()
                            .const_int(self.target_abi.vclosure_has_value_offset(), false)],
                        "closure_raw_hasvalue_gep",
                    )?
                };
                let raw_has_value = self
                    .builder
                    .build_load(i32_type, raw_has_value_gep, "closure_raw_hasvalue")?
                    .into_int_value();
                let is_wrapper = self.builder.build_int_compare(
                    IntPredicate::EQ,
                    raw_has_value,
                    i32_type.const_int(2, false),
                    "closure_is_wrapper",
                )?;
                // Branch rather than load-then-select. `wrappedFun` lives at
                // offset 32, which exists only on a `vclosure_wrapper` (40
                // bytes); a plain `vclosure` is 32, and that is what
                // `hlp_alloc_closure_void`/`_ptr` allocate. Loading it
                // unconditionally and discarding it in a `select` read 8 bytes
                // past the end of every ordinary closure, on every call --
                // harmless inside an Immix block, a fault when the closure is
                // the last object before an unmapped page. Guarding the load
                // also keeps the common path within the 32 bytes the object is
                // known to have.
                let unwrap_function = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();
                let unwrap_bb = self
                    .context
                    .append_basic_block(unwrap_function, "closure_unwrap");
                let unwrap_done_bb = self
                    .context
                    .append_basic_block(unwrap_function, "closure_unwrap_done");
                let unwrap_from_bb = self.builder.get_insert_block().unwrap();
                self.builder
                    .build_conditional_branch(is_wrapper, unwrap_bb, unwrap_done_bb)?;

                self.builder.position_at_end(unwrap_bb);
                let wrapped_fun_gep = unsafe {
                    self.builder.build_gep(
                        i8_type,
                        raw_closure_ptr,
                        &[self
                            .context
                            .i64_type()
                            .const_int(self.target_abi.vclosure_wrapper_fun_offset(), false)],
                        "closure_wrapped_fun_gep",
                    )?
                };
                let wrapped_fun = self
                    .builder
                    .build_load(ptr_type, wrapped_fun_gep, "closure_wrapped_fun")?
                    .into_pointer_value();
                self.builder.build_unconditional_branch(unwrap_done_bb)?;

                self.builder.position_at_end(unwrap_done_bb);
                let closure_phi = self.builder.build_phi(ptr_type, "closure_unwrapped")?;
                closure_phi.add_incoming(&[
                    (&wrapped_fun, unwrap_bb),
                    (&raw_closure_ptr, unwrap_from_bb),
                ]);
                let closure_ptr = closure_phi.as_basic_value().into_pointer_value();

                // vclosure.fun at offset 8
                let fun_field_gep = unsafe {
                    self.builder.build_gep(
                        i8_type,
                        closure_ptr,
                        &[self
                            .context
                            .i64_type()
                            .const_int(self.target_abi.vclosure_fun_offset(), false)],
                        "closure_fun_gep",
                    )?
                };
                let fun_ptr = self
                    .builder
                    .build_load(ptr_type, fun_field_gep, "closure_fun")?
                    .into_pointer_value();

                // vclosure.hasValue at offset 16
                let has_value_gep = unsafe {
                    self.builder.build_gep(
                        i8_type,
                        closure_ptr,
                        &[self
                            .context
                            .i64_type()
                            .const_int(self.target_abi.vclosure_has_value_offset(), false)],
                        "closure_hasvalue_gep",
                    )?
                };
                let has_value = self
                    .builder
                    .build_load(i32_type, has_value_gep, "has_value")?
                    .into_int_value();

                // vclosure.value at offset 24
                let value_gep = unsafe {
                    self.builder.build_gep(
                        i8_type,
                        closure_ptr,
                        &[self
                            .context
                            .i64_type()
                            .const_int(self.target_abi.vclosure_value_offset(), false)],
                        "closure_value_gep",
                    )?
                };
                let closure_value = self
                    .builder
                    .build_load(ptr_type, value_gep, "closure_value")?
                    .into_pointer_value();

                // Load all explicit args
                let arg_vals: Vec<BasicMetadataValueEnum> = args
                    .iter()
                    .map(|arg| {
                        self.builder
                            .build_load(
                                reg_types[arg.0 as usize],
                                registers[arg.0 as usize],
                                "arg_val",
                            )
                            .unwrap()
                            .into()
                    })
                    .collect();

                // Determine function type from register's type info
                let fun_type_idx = f.regs[fun.0 as usize].0;
                let base_fn_type = if let Some(fun_type) = self.types_[fun_type_idx].fun.clone() {
                    self.create_function_type(&fun_type)?
                } else {
                    // Dynamic-typed closure: infer from args (all ptrs) with ptr return
                    let dyn_params: Vec<BasicMetadataTypeEnum> =
                        args.iter().map(|_| ptr_type.into()).collect();
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
                        BasicTypeEnum::ScalableVectorType(t) => t.fn_type(&extended_params, false),
                    }
                } else {
                    self.context.void_type().fn_type(&extended_params, false)
                };

                let function = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();
                let call_done_bb = self.context.append_basic_block(function, "call_done");

                // Guarded devirtualisation comes before the generic runtime
                // signature check. The target's real LLVM signature was
                // checked while constructing `devirt`, so a guard hit is
                // already ABI-safe. Signature-adapted closures whose target
                // does not have this call site's ABI cannot enter this arm;
                // they continue to the dynamic marshaller below. Keeping the
                // common monomorphic arm first avoids putting the Issue2889
                // safety branch inside every iteration of closure_call.
                let devirt = if self.hot_reload {
                    None
                } else {
                    let caller = f.findex as u32;
                    crate::callsite_profile::closure_target(caller, i as u32)
                        .or_else(|| crate::callsite_profile::uniform_closure_target(caller))
                        .and_then(|(target, exp_hv)| {
                            let expected_ty = if exp_hv {
                                extended_fn_type
                            } else {
                                base_fn_type
                            };
                            match self.get_or_create_function_value(target as usize) {
                                Ok((callee, is_placeholder)) => {
                                    if is_placeholder {
                                        self.add_pending_compilation(target as usize);
                                    }
                                    (callee.get_type() == expected_ty)
                                        .then_some((callee, target, exp_hv))
                                }
                                Err(_) => None,
                            }
                        })
                };

                if let Some((callee, target, exp_hv)) = devirt {
                    crate::profile::count("devirt closure fast-arm", 1);
                    // Closure header fields are immutable after allocation.
                    // This lets LICM hoist the target guard when the closure
                    // value itself is loop invariant.
                    for lv in [
                        raw_has_value.as_instruction(),
                        wrapped_fun.as_instruction(),
                        fun_ptr.as_instruction(),
                        has_value.as_instruction(),
                        closure_value.as_instruction(),
                    ]
                    .into_iter()
                    .flatten()
                    {
                        let _ = lv.set_metadata(
                            self.context.metadata_node(&[]),
                            self.context.get_kind_id("invariant.load"),
                        );
                    }

                    let devirt_bb = self.context.append_basic_block(function, "devirt_hit");
                    let signature_bb = self
                        .context
                        .append_basic_block(function, "devirt_miss_signature");
                    let fun_int = self.builder.build_ptr_to_int(
                        fun_ptr,
                        self.context.i64_type(),
                        "closure_fun_int",
                    )?;
                    let is_target = self.builder.build_int_compare(
                        IntPredicate::EQ,
                        fun_int,
                        self.context.i64_type().const_int(target as u64 + 1, false),
                        "devirt_is_target",
                    )?;
                    let hv_matches = self.builder.build_int_compare(
                        if exp_hv {
                            IntPredicate::NE
                        } else {
                            IntPredicate::EQ
                        },
                        has_value,
                        i32_type.const_zero(),
                        "devirt_hv",
                    )?;
                    let guard = self
                        .builder
                        .build_and(is_target, hv_matches, "devirt_guard")?;
                    self.builder
                        .build_conditional_branch(guard, devirt_bb, signature_bb)?;

                    self.builder.position_at_end(devirt_bb);
                    let direct_args: Vec<BasicMetadataValueEnum> = if exp_hv {
                        let mut values: Vec<BasicMetadataValueEnum> = vec![closure_value.into()];
                        values.extend(arg_vals.iter().cloned());
                        values
                    } else {
                        arg_vals.clone()
                    };
                    let ret = self
                        .builder
                        .build_call(callee, &direct_args, "devirt_call")?
                        .try_as_basic_value();
                    if let Some(value) = ret.basic() {
                        self.builder.build_store(registers[dst.0 as usize], value)?;
                    }
                    self.builder.build_unconditional_branch(call_done_bb)?;
                    self.builder.position_at_end(signature_bb);
                }

                // The register's HFUN is only the call-site contract. A
                // signature-adapted closure can carry a different runtime
                // HFUN and a wrapper body with that runtime ABI. Calling it
                // through `base_fn_type` turns scalar arguments into tiny
                // pointers (Issue2889 passed Int(1) as vdynamic* 0x1). Match
                // Cranelift's AIR V2 lowering: retain the typed fast path for
                // equal signatures, otherwise let the runtime marshal using
                // the closure's own type.
                let runtime_type = unsafe {
                    let gep = self.builder.build_gep(
                        i8_type,
                        raw_closure_ptr,
                        &[self.context.i64_type().const_zero()],
                        "closure_runtime_type_gep",
                    )?;
                    self.builder
                        .build_load(ptr_type, gep, "closure_runtime_type")?
                        .into_pointer_value()
                };
                let expected_type = self
                    .get_initialized_type(fun_type_idx)?
                    .into_pointer_value();
                let pointer_exact = self.builder.build_int_compare(
                    IntPredicate::EQ,
                    runtime_type,
                    expected_type,
                    "closure_type_pointer_exact",
                )?;
                let structural_bb = self
                    .context
                    .append_basic_block(function, "closure_type_structural");
                let typed_bb = self
                    .context
                    .append_basic_block(function, "closure_type_typed");
                let dynamic_bb = self
                    .context
                    .append_basic_block(function, "closure_type_dynamic");
                self.builder
                    .build_conditional_branch(pointer_exact, typed_bb, structural_bb)?;

                self.builder.position_at_end(structural_bb);
                let same_type = self.declare_native(
                    "hlp_same_type",
                    &[ptr_type.into(), ptr_type.into()],
                    Some(self.context.bool_type().into()),
                );
                let structurally_exact = self
                    .builder
                    .build_call(
                        same_type,
                        &[runtime_type.into(), expected_type.into()],
                        "closure_same_type",
                    )?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| anyhow!("hlp_same_type returned void"))?
                    .into_int_value();
                self.builder
                    .build_conditional_branch(structurally_exact, typed_bb, dynamic_bb)?;

                self.builder.position_at_end(dynamic_bb);
                let nargs = args.len();
                // In the entry block, and an array type rather than
                // `build_array_alloca`: the count is a constant here, and an
                // alloca in this conditional block is a DYNAMIC one, so a
                // closure called dynamically inside a loop moved the stack
                // pointer once per iteration and never gave it back. Same
                // defect as the boxing slots below, on the path a closure
                // call takes every time its runtime signature differs from
                // the call site's. The GEP below indexes it by pointer, which
                // is what an array of pointers is.
                let argv = self
                    .entry_alloca(ptr_type.array_type(nargs.max(1) as u32), "closure_dyn_argv")?;
                let make_dyn = self.declare_native(
                    "hlp_make_dyn",
                    &[ptr_type.into(), ptr_type.into()],
                    Some(ptr_type.into()),
                );
                for (index, arg) in args.iter().enumerate() {
                    let type_index = f.regs[arg.0 as usize].0;
                    let kind = self.types_[type_index].kind;
                    let loaded = self.builder.build_load(
                        reg_types[arg.0 as usize],
                        registers[arg.0 as usize],
                        "closure_dyn_arg",
                    )?;
                    let self_describing = matches!(
                        kind,
                        hl_type_kind_HDYN
                            | hl_type_kind_HFUN
                            | hl_type_kind_HOBJ
                            | crate::hl::hl_type_kind_HARRAY
                            | hl_type_kind_HVIRTUAL
                            | hl_type_kind_HDYNOBJ
                            | crate::hl::hl_type_kind_HENUM
                            | hl_type_kind_HNULL
                    );
                    let boxed = if self_describing {
                        loaded
                    } else {
                        let slot = self.entry_alloca(loaded.get_type(), "closure_dyn_box_slot")?;
                        self.builder.build_store(slot, loaded)?;
                        let type_ptr = self.get_initialized_type(type_index)?.into_pointer_value();
                        self.builder
                            .build_call(
                                make_dyn,
                                &[slot.into(), type_ptr.into()],
                                "closure_dyn_box",
                            )?
                            .try_as_basic_value()
                            .basic()
                            .ok_or_else(|| anyhow!("hlp_make_dyn returned void"))?
                    };
                    let boxed = if boxed.get_type() == ptr_type.as_basic_type_enum() {
                        boxed
                    } else {
                        self.cast_for_call(boxed, ptr_type.into())?
                    };
                    let argv_slot = unsafe {
                        self.builder.build_gep(
                            ptr_type,
                            argv,
                            &[i32_type.const_int(index as u64, false)],
                            "closure_dyn_argv_slot",
                        )?
                    };
                    self.builder.build_store(argv_slot, boxed)?;
                }
                let dyn_call = self.declare_native(
                    "hlp_dyn_call",
                    &[ptr_type.into(), ptr_type.into(), i32_type.into()],
                    Some(ptr_type.into()),
                );
                let dyn_result = self
                    .builder
                    .build_call(
                        dyn_call,
                        &[
                            raw_closure_ptr.into(),
                            argv.into(),
                            i32_type.const_int(nargs as u64, false).into(),
                        ],
                        "closure_dyn_call",
                    )?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| anyhow!("hlp_dyn_call returned void"))?
                    .into_pointer_value();
                let dst_type_index = f.regs[dst.0 as usize].0;
                let dst_kind = self.types_[dst_type_index].kind;
                if dst_kind != hl_type_kind_HVOID {
                    let dst_type = reg_types[dst.0 as usize];
                    let value: BasicValueEnum = if dst_kind == hl_type_kind_HDYN {
                        dyn_result.into()
                    } else if dst_type.is_pointer_type() {
                        let dyn_type_index = self
                            .types_
                            .iter()
                            .position(|ty| ty.kind == hl_type_kind_HDYN)
                            .ok_or_else(|| anyhow!("module has no HDYN runtime type"))?;
                        let dyn_type = self
                            .get_initialized_type(dyn_type_index)?
                            .into_pointer_value();
                        let dst_runtime_type = self
                            .get_initialized_type(dst_type_index)?
                            .into_pointer_value();
                        let result_slot = self.entry_alloca(ptr_type, "closure_dyn_result_slot")?;
                        self.builder.build_store(result_slot, dyn_result)?;
                        let castp = self.declare_native(
                            "hlp_dyn_castp",
                            &[ptr_type.into(), ptr_type.into(), ptr_type.into()],
                            Some(ptr_type.into()),
                        );
                        self.builder
                            .build_call(
                                castp,
                                &[result_slot.into(), dyn_type.into(), dst_runtime_type.into()],
                                "closure_dyn_result_cast",
                            )?
                            .try_as_basic_value()
                            .basic()
                            .ok_or_else(|| anyhow!("hlp_dyn_castp returned void"))?
                    } else {
                        let (helper, helper_ret): (&str, BasicTypeEnum) =
                            if dst_kind == hl_type_kind_HF64 {
                                ("hlp_dyn_todouble", self.context.f64_type().into())
                            } else if dst_kind == hl_type_kind_HF32 {
                                ("hlp_dyn_tofloat", self.context.f32_type().into())
                            } else if dst_kind == hl_type_kind_HI64 {
                                ("hlp_dyn_toi64", self.context.i64_type().into())
                            } else {
                                ("hlp_dyn_toint", i32_type.into())
                            };
                        let unbox =
                            self.declare_native(helper, &[ptr_type.into()], Some(helper_ret));
                        let raw = self
                            .builder
                            .build_call(unbox, &[dyn_result.into()], "closure_dyn_unbox")?
                            .try_as_basic_value()
                            .basic()
                            .ok_or_else(|| anyhow!("dynamic unbox helper returned void"))?;
                        if raw.get_type() == dst_type {
                            raw
                        } else {
                            self.cast_for_call(raw, dst_type)?
                        }
                    };
                    self.builder.build_store(registers[dst.0 as usize], value)?;
                }
                self.builder.build_unconditional_branch(call_done_bb)?;

                self.builder.position_at_end(typed_bb);

                // Branch based on hasValue
                let has_value_cmp = self.builder.build_int_compare(
                    IntPredicate::NE,
                    has_value,
                    i32_type.const_zero(),
                    "has_value_cmp",
                )?;

                let call_with_value_bb =
                    self.context.append_basic_block(function, "call_with_value");
                let call_without_value_bb = self
                    .context
                    .append_basic_block(function, "call_without_value");

                self.builder.build_conditional_branch(
                    has_value_cmp,
                    call_with_value_bb,
                    call_without_value_bb,
                )?;

                // --- Call WITH value (hasValue != 0) ---
                self.builder.position_at_end(call_with_value_bb);
                let mut args_with_value: Vec<BasicMetadataValueEnum> = vec![closure_value.into()];
                args_with_value.extend(arg_vals.iter().cloned());
                if let Some(ret_val) = self.build_stub_guarded_indirect_call(
                    extended_fn_type,
                    fun_ptr,
                    &args_with_value,
                    "call_closure_hv",
                )? {
                    self.builder
                        .build_store(registers[dst.0 as usize], ret_val)?;
                }
                self.builder.build_unconditional_branch(call_done_bb)?;

                // --- Call WITHOUT value (hasValue == 0) ---
                self.builder.position_at_end(call_without_value_bb);
                if let Some(ret_val) = self.build_stub_guarded_indirect_call(
                    base_fn_type,
                    fun_ptr,
                    &arg_vals,
                    "call_closure",
                )? {
                    self.builder
                        .build_store(registers[dst.0 as usize], ret_val)?;
                }
                self.builder.build_unconditional_branch(call_done_bb)?;

                // Continue from call_done
                self.builder.position_at_end(call_done_bb);
            }

            // --- SafeCast: unbox HNULL(T)/HDYN -> primitive T, otherwise copy ---
            Opcode::SafeCast { dst, src } => {
                let src_type_idx = f.regs[src.0 as usize].0;
                let dst_type_idx = f.regs[dst.0 as usize].0;
                let src_kind = self.types_[src_type_idx].kind;
                let dst_kind = self.types_[dst_type_idx].kind;

                // Unboxing needed when casting from a heap-boxed type (HNULL/HDYN)
                // to a primitive type. The primitive value lives at offset 8 inside
                // the vdynamic struct (the `v` union field).
                let needs_unbox = (src_kind == hl_type_kind_HNULL || src_kind == hl_type_kind_HDYN)
                    && matches!(dst_kind,
                        k if k == hl_type_kind_HBOOL || k == hl_type_kind_HI32
                            || k == hl_type_kind_HF64 || k == hl_type_kind_HF32
                            || k == hl_type_kind_HI64 || k == hl_type_kind_HUI8
                            || k == hl_type_kind_HUI16);

                if needs_unbox {
                    let ptr_type = self.context.ptr_type(AddressSpace::default());
                    let src_ptr = self
                        .builder
                        .build_load(ptr_type, registers[src.0 as usize], "safecast_src")?
                        .into_pointer_value();

                    let is_null = self.builder.build_is_null(src_ptr, "safecast_null")?;

                    let function = self
                        .builder
                        .get_insert_block()
                        .unwrap()
                        .get_parent()
                        .unwrap();
                    let null_bb = self
                        .context
                        .append_basic_block(function, "safecast_null_path");
                    let unbox_bb = self.context.append_basic_block(function, "safecast_unbox");
                    let done_bb = self.context.append_basic_block(function, "safecast_done");

                    self.builder
                        .build_conditional_branch(is_null, null_bb, unbox_bb)?;

                    // Unbox path. A raw load of vdynamic.v at offset 8 is only
                    // right when the box's runtime kind IS the destination kind
                    // — an HDYN register can hold any numeric box, and reading
                    // an Int box as f64 yields its bits as a denormal (Dynamic
                    // subtraction of 7 and 2 printed 2.47e-323, div was exact
                    // because the 2^-1074 scales cancelled). Coerce through the
                    // dyn-cast helpers instead; they switch on the box's own
                    // runtime type and match the interpreter and upstream.
                    self.builder.position_at_end(unbox_bb);
                    let dst_llvm_type = reg_types[dst.0 as usize];
                    let (helper, helper_ret): (&str, BasicTypeEnum) =
                        if dst_kind == hl_type_kind_HF64 {
                            ("hlp_dyn_todouble", self.context.f64_type().into())
                        } else if dst_kind == hl_type_kind_HF32 {
                            ("hlp_dyn_tofloat", self.context.f32_type().into())
                        } else if dst_kind == hl_type_kind_HI64 {
                            ("hlp_dyn_toi64", self.context.i64_type().into())
                        } else {
                            ("hlp_dyn_toint", self.context.i32_type().into())
                        };
                    let unbox_fn =
                        self.declare_native(helper, &[ptr_type.into()], Some(helper_ret));
                    let raw = self
                        .builder
                        .build_call(unbox_fn, &[src_ptr.into()], "safecast_unbox_call")?
                        .try_as_basic_value()
                        .basic()
                        .unwrap();
                    let unboxed = if dst_kind == hl_type_kind_HBOOL && raw.is_int_value() {
                        // HashLink reads the box's int byte as the Bool, so any
                        // non-zero value is true; truncating to i1 kept only
                        // the low bit and made a boxed 2 false.
                        let iv = raw.into_int_value();
                        self.builder
                            .build_int_compare(
                                IntPredicate::NE,
                                iv,
                                iv.get_type().const_zero(),
                                "safecast_bool",
                            )?
                            .into()
                    } else if raw.get_type() != dst_llvm_type {
                        self.cast_for_call(raw, dst_llvm_type)?
                    } else {
                        raw
                    };
                    self.builder
                        .build_store(registers[dst.0 as usize], unboxed)?;
                    self.builder.build_unconditional_branch(done_bb)?;

                    // Null path: store default value (0/false/0.0)
                    self.builder.position_at_end(null_bb);
                    let default_val = dst_llvm_type.const_zero();
                    self.builder
                        .build_store(registers[dst.0 as usize], default_val)?;
                    self.builder.build_unconditional_branch(done_bb)?;

                    self.builder.position_at_end(done_bb);
                } else if (dst_kind == hl_type_kind_HNULL || dst_kind == hl_type_kind_HDYN)
                    && !reg_types[src.0 as usize].is_pointer_type()
                {
                    // The other direction: a primitive INTO a box. Haxe spells
                    // `Null<Int64> = 29` as OSafeCast from an I32 register, and
                    // this arm used to fall through to the plain copy below --
                    // the raw 29 landed in a pointer slot and every reader then
                    // dereferenced it (unit suite Issue4436: SIGSEGV at 0x1d on
                    // native, "Can't cast ? to i64" on wasm). Box as ToDyn
                    // does, but at the box's own type: for Null<T> that is T,
                    // so a 32-bit value widens first, the way ToInt widens.
                    let ptr_type = self.context.ptr_type(AddressSpace::default());
                    let box_type_idx = if dst_kind == hl_type_kind_HNULL {
                        self.types_[dst_type_idx]
                            .tparam
                            .as_ref()
                            .map(|t| t.0)
                            .unwrap_or(src_type_idx)
                    } else {
                        src_type_idx
                    };
                    let box_kind = self.types_[box_type_idx].kind;
                    let src_val = self.builder.build_load(
                        reg_types[src.0 as usize],
                        registers[src.0 as usize],
                        "safecast_box_src",
                    )?;
                    let box_llvm_type: BasicTypeEnum = match box_kind {
                        k if k == hl_type_kind_HF64 => self.context.f64_type().into(),
                        k if k == hl_type_kind_HF32 => self.context.f32_type().into(),
                        k if k == hl_type_kind_HI64 => self.context.i64_type().into(),
                        k if k == hl_type_kind_HUI16 => self.context.i16_type().into(),
                        k if k == hl_type_kind_HUI8 || k == hl_type_kind_HBOOL => {
                            self.context.i8_type().into()
                        }
                        _ => self.context.i32_type().into(),
                    };
                    let src_unsigned = src_kind == hl_type_kind_HUI8
                        || src_kind == hl_type_kind_HUI16
                        || src_kind == hl_type_kind_HBOOL;
                    let converted: BasicValueEnum = match (src_val, box_llvm_type) {
                        (v, t) if v.get_type() == t => v,
                        (v, BasicTypeEnum::IntType(t)) if v.is_int_value() => {
                            let iv = v.into_int_value();
                            let (sw, dw) = (iv.get_type().get_bit_width(), t.get_bit_width());
                            if sw > dw {
                                self.builder
                                    .build_int_truncate(iv, t, "safecast_box_trunc")?
                                    .into()
                            } else if src_unsigned {
                                self.builder
                                    .build_int_z_extend(iv, t, "safecast_box_zext")?
                                    .into()
                            } else {
                                self.builder
                                    .build_int_s_extend(iv, t, "safecast_box_sext")?
                                    .into()
                            }
                        }
                        (v, BasicTypeEnum::FloatType(t)) if v.is_int_value() => {
                            let iv = v.into_int_value();
                            if src_unsigned {
                                self.builder
                                    .build_unsigned_int_to_float(iv, t, "safecast_box_uitofp")?
                                    .into()
                            } else {
                                self.builder
                                    .build_signed_int_to_float(iv, t, "safecast_box_sitofp")?
                                    .into()
                            }
                        }
                        (v, BasicTypeEnum::IntType(t)) if v.is_float_value() => self
                            .build_float_to_int_saturating(
                                v.into_float_value(),
                                t,
                                "safecast_box_fptosi",
                            )?
                            .into(),
                        (v, BasicTypeEnum::FloatType(t)) if v.is_float_value() => self
                            .builder
                            .build_float_cast(v.into_float_value(), t, "safecast_box_fpcast")?
                            .into(),
                        (v, t) => self.cast_for_call(v, t)?,
                    };
                    let temp = self.entry_alloca(box_llvm_type, "safecast_box_temp")?;
                    self.builder.build_store(temp, converted)?;
                    let type_ptr = self
                        .get_initialized_type(box_type_idx)?
                        .into_pointer_value();
                    let make_dyn = self.declare_native(
                        "hlp_make_dyn",
                        &[ptr_type.into(), ptr_type.into()],
                        Some(ptr_type.into()),
                    );
                    let boxed = self.builder.build_call(
                        make_dyn,
                        &[temp.into(), type_ptr.into()],
                        "safecast_box",
                    )?;
                    self.builder.build_store(
                        registers[dst.0 as usize],
                        boxed.try_as_basic_value().basic().unwrap(),
                    )?;
                } else if reg_types[src.0 as usize].is_pointer_type()
                    && matches!(dst_kind,
                        k if k == hl_type_kind_HBOOL || k == hl_type_kind_HI32
                            || k == hl_type_kind_HF64 || k == hl_type_kind_HF32
                            || k == hl_type_kind_HI64 || k == hl_type_kind_HUI8
                            || k == hl_type_kind_HUI16)
                {
                    // A reference cast to a number: `cast("foo", Int)`. There is
                    // no value to extract, so the only correct outcome is the
                    // runtime's "Can't cast String to Int" -- which the plain
                    // copy below never produced (unit suite Issue6482: the
                    // exception was not raised). The dyn-cast helpers switch on
                    // the SOURCE type and raise for anything not numeric, so
                    // handing them the register and both types gets exactly
                    // the interpreter's behaviour.
                    let ptr_type = self.context.ptr_type(AddressSpace::default());
                    let src_type_ptr = self
                        .get_initialized_type(src_type_idx)?
                        .into_pointer_value();
                    let dst_type_ptr = self
                        .get_initialized_type(dst_type_idx)?
                        .into_pointer_value();
                    let dst_llvm_type = reg_types[dst.0 as usize];
                    let (helper, helper_ret): (&str, BasicTypeEnum) =
                        if dst_kind == hl_type_kind_HF64 {
                            ("hlp_dyn_castd", self.context.f64_type().into())
                        } else if dst_kind == hl_type_kind_HF32 {
                            ("hlp_dyn_castf", self.context.f32_type().into())
                        } else if dst_kind == hl_type_kind_HI64 {
                            ("hlp_dyn_casti64", self.context.i64_type().into())
                        } else {
                            ("hlp_dyn_casti", self.context.i32_type().into())
                        };
                    // Only the int form takes the destination type; the others
                    // have one result width and need only the source.
                    let mut params: Vec<BasicMetadataTypeEnum> =
                        vec![ptr_type.into(), ptr_type.into()];
                    let mut args: Vec<BasicMetadataValueEnum> =
                        vec![registers[src.0 as usize].into(), src_type_ptr.into()];
                    if helper == "hlp_dyn_casti" {
                        params.push(ptr_type.into());
                        args.push(dst_type_ptr.into());
                    }
                    let cast_fn = self.declare_native(helper, &params, Some(helper_ret));
                    let raw = self
                        .builder
                        .build_call(cast_fn, &args, "safecast_ref_to_num")?
                        .try_as_basic_value()
                        .basic()
                        .unwrap();
                    let value = if dst_kind == hl_type_kind_HBOOL && raw.is_int_value() {
                        let iv = raw.into_int_value();
                        self.builder
                            .build_int_compare(
                                IntPredicate::NE,
                                iv,
                                iv.get_type().const_zero(),
                                "safecast_ref_bool",
                            )?
                            .into()
                    } else if raw.get_type() != dst_llvm_type {
                        self.cast_for_call(raw, dst_llvm_type)?
                    } else {
                        raw
                    };
                    self.builder.build_store(registers[dst.0 as usize], value)?;
                } else if src_kind == hl_type_kind_HDYN || src_kind == hl_type_kind_HNULL {
                    // Dynamic-to-concrete non-primitive cast: call hlp_dyn_castp to
                    // properly extract the inner value from the vdynamic wrapper.
                    // A simple pointer copy would pass the vdynamic header address
                    // instead of the actual data (e.g. bytes pointer for HBYTES).
                    let ptr_type = self.context.ptr_type(AddressSpace::default());
                    let src_type_ptr = self
                        .get_initialized_type(src_type_idx)?
                        .into_pointer_value();
                    let dst_type_ptr = self
                        .get_initialized_type(dst_type_idx)?
                        .into_pointer_value();
                    let dyn_castp = self.declare_native(
                        "hlp_dyn_castp",
                        &[ptr_type.into(), ptr_type.into(), ptr_type.into()],
                        Some(ptr_type.into()),
                    );
                    // hlp_dyn_castp expects double-indirection: data points to a slot
                    // containing the *mut vdynamic, which is exactly what the alloca is.
                    let result = self.builder.build_call(
                        dyn_castp,
                        &[
                            registers[src.0 as usize].into(),
                            src_type_ptr.into(),
                            dst_type_ptr.into(),
                        ],
                        "dyn_castp",
                    )?;
                    self.builder.build_store(
                        registers[dst.0 as usize],
                        result.try_as_basic_value().basic().unwrap(),
                    )?;
                } else if src_type_idx != dst_type_idx
                    && ((src_kind == hl_type_kind_HOBJ && dst_kind == hl_type_kind_HOBJ)
                        || (src_kind == hl_type_kind_HSTRUCT && dst_kind == hl_type_kind_HSTRUCT)
                        || (src_kind == hl_type_kind_HVIRTUAL
                            && (dst_kind == hl_type_kind_HOBJ
                                || dst_kind == hl_type_kind_HSTRUCT
                                || dst_kind == hl_type_kind_HVIRTUAL)))
                {
                    // A virtual is a WRAPPER: `vvirtual { t, value, next, fields.. }`.
                    // Casting an interface-typed register to a class used to fall
                    // through to the pointer copy below, so the class's field
                    // offsets were then applied to the wrapper -- field 3 read
                    // `vfields[1]`, a method pointer, and the next store went
                    // into code. The interpreter unwraps `value` and runs the
                    // class check; the runtime's castp does the same, including
                    // the re-wrap for a virtual-to-virtual cast.
                    let ptr_type = self.context.ptr_type(AddressSpace::default());
                    let src_type_ptr = self
                        .get_initialized_type(src_type_idx)?
                        .into_pointer_value();
                    let dst_type_ptr = self
                        .get_initialized_type(dst_type_idx)?
                        .into_pointer_value();
                    let dyn_castp = self.declare_native(
                        "hlp_dyn_castp",
                        &[ptr_type.into(), ptr_type.into(), ptr_type.into()],
                        Some(ptr_type.into()),
                    );
                    let result = self.builder.build_call(
                        dyn_castp,
                        &[
                            registers[src.0 as usize].into(),
                            src_type_ptr.into(),
                            dst_type_ptr.into(),
                        ],
                        "dyn_castp_obj",
                    )?;
                    self.builder.build_store(
                        registers[dst.0 as usize],
                        result.try_as_basic_value().basic().unwrap(),
                    )?;
                } else {
                    // Same type or non-dynamic: simple pointer copy
                    let src_val = self.builder.build_load(
                        reg_types[src.0 as usize],
                        registers[src.0 as usize],
                        "safecast_src",
                    )?;
                    self.builder
                        .build_store(registers[dst.0 as usize], src_val)?;
                }
            }

            // --- ToVirtual: wrap object in a vvirtual with resolved field/method pointers ---
            Opcode::ToVirtual { dst, src } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let dst_type_idx = f.regs[dst.0 as usize].0;
                let dst_kind = self.types_[dst_type_idx].kind;

                if dst_kind == hl_type_kind_HVIRTUAL {
                    // Get the full C-side hl_type pointer for the virtual type
                    let vt_ptr = self
                        .get_initialized_type(dst_type_idx)?
                        .into_pointer_value();
                    let src_val = self
                        .builder
                        .build_load(ptr_type, registers[src.0 as usize], "tovirt_src")?
                        .into_pointer_value();

                    let hl_to_virtual = self.declare_native(
                        "hl_to_virtual",
                        &[ptr_type.into(), ptr_type.into()],
                        Some(ptr_type.into()),
                    );
                    let result = self.builder.build_call(
                        hl_to_virtual,
                        &[vt_ptr.into(), src_val.into()],
                        "tovirt",
                    )?;
                    self.builder.build_store(
                        registers[dst.0 as usize],
                        result.try_as_basic_value().basic().unwrap(),
                    )?;
                } else {
                    // Non-virtual dst: simple pointer copy
                    let src_val = self.builder.build_load(
                        reg_types[src.0 as usize],
                        registers[src.0 as usize],
                        "tovirt_src",
                    )?;
                    self.builder
                        .build_store(registers[dst.0 as usize], src_val)?;
                }
            }

            // --- Trap: setjmp-based exception handling ---
            Opcode::Trap { exc, offset } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let i32_type = self.context.i32_type();

                // 1. Call hlp_setup_trap_jit() → returns *mut c_int (jmp_buf pointer)
                let setup = self.declare_native("hlp_setup_trap_jit", &[], Some(ptr_type.into()));
                let buf_ptr = self
                    .builder
                    .build_call(setup, &[], "trap_buf")?
                    .try_as_basic_value()
                    .basic()
                    .unwrap()
                    .into_pointer_value();

                // 2. Call _setjmp(buf_ptr) via indirect call (system function, not in stdlib)
                let setjmp_ptr = self.setjmp_ptr()?;
                let setjmp_fn_type = i32_type.fn_type(&[ptr_type.into()], false);
                let setjmp_call = self.builder.build_indirect_call(
                    setjmp_fn_type,
                    setjmp_ptr,
                    &[buf_ptr.into()],
                    "setjmp_ret",
                )?;
                // Mark as returns_twice so LLVM doesn't misoptimize around setjmp at O3
                let rt_kind =
                    inkwell::attributes::Attribute::get_named_enum_kind_id("returns_twice");
                let rt_attr = self.context.create_enum_attribute(rt_kind, 0);
                setjmp_call.add_attribute(inkwell::attributes::AttributeLoc::Function, rt_attr);
                let setjmp_result = setjmp_call
                    .try_as_basic_value()
                    .basic()
                    .unwrap()
                    .into_int_value();

                // 3. Branch: 0 → normal (protected code), non-zero → handler
                let is_exception = self.builder.build_int_compare(
                    IntPredicate::NE,
                    setjmp_result,
                    i32_type.const_zero(),
                    "is_exc",
                )?;

                let handler_block = opcode_blocks[(i as i32 + 1 + *offset) as usize];
                let normal_block = opcode_blocks[i + 1];

                // Create handler_entry block to load exc value before jumping to handler
                let function = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();
                let handler_entry = self
                    .context
                    .append_basic_block(function, &format!("trap_handler_{}", i));

                self.builder
                    .build_conditional_branch(is_exception, handler_entry, normal_block)?;

                // Emit handler entry: load exc value into exc register, then branch to handler
                self.builder.position_at_end(handler_entry);
                let get_exc = self.declare_native("hlp_get_exc_value", &[], Some(ptr_type.into()));
                let exc_val = self
                    .builder
                    .build_call(get_exc, &[], "exc_val")?
                    .try_as_basic_value()
                    .basic()
                    .unwrap();
                self.builder
                    .build_store(registers[exc.0 as usize], exc_val)?;
                // Clear the global exc_value to prevent stale values
                // contaminating nested exception handlers.
                let clear_exc = self.declare_native("hlp_clear_exc_value", &[], None);
                self.builder.build_call(clear_exc, &[], "")?;
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
                let exc_val =
                    self.builder
                        .build_load(ptr_type, registers[exc.0 as usize], "throw_val")?;
                let throw_fn = self.declare_native("hlp_throw", &[ptr_type.into()], None);
                self.builder.build_call(throw_fn, &[exc_val.into()], "")?;
                self.builder.build_unreachable()?;
            }

            // --- Rethrow: same as Throw ---
            Opcode::Rethrow { exc } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let exc_val =
                    self.builder
                        .build_load(ptr_type, registers[exc.0 as usize], "rethrow_val")?;
                let throw_fn = self.declare_native("hlp_throw", &[ptr_type.into()], None);
                self.builder.build_call(throw_fn, &[exc_val.into()], "")?;
                self.builder.build_unreachable()?;
            }

            // --- Ref: take address of register ---
            Opcode::Ref { dst, src } => {
                // dst = &src (pointer to the register's alloca)
                self.builder
                    .build_store(registers[dst.0 as usize], registers[src.0 as usize])?;
            }

            // --- Unref: dereference pointer ---
            Opcode::Unref { dst, src } => {
                let ptr = self
                    .builder
                    .build_load(
                        reg_types[src.0 as usize],
                        registers[src.0 as usize],
                        "unref_ptr",
                    )?
                    .into_pointer_value();
                let val = self
                    .builder
                    .build_load(reg_types[dst.0 as usize], ptr, "unref_val")?;
                self.builder.build_store(registers[dst.0 as usize], val)?;
            }

            // --- Setref: store through pointer ---
            Opcode::Setref { dst, value } => {
                let ptr = self
                    .builder
                    .build_load(
                        reg_types[dst.0 as usize],
                        registers[dst.0 as usize],
                        "setref_ptr",
                    )?
                    .into_pointer_value();
                let val = self.builder.build_load(
                    reg_types[value.0 as usize],
                    registers[value.0 as usize],
                    "setref_val",
                )?;
                self.builder.build_store(ptr, val)?;
            }

            // --- InstanceClosure: allocate closure binding obj as first arg ---
            Opcode::InstanceClosure { dst, fun, obj } => {
                if !self.lazy_compilation {
                    let (_function, is_placeholder) = self.get_or_create_function_value(fun.0)?;
                    if is_placeholder {
                        self.add_pending_compilation(fun.0);
                    }
                }

                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let findex = fun.0 as usize;

                // Load function address from functions_ptrs[findex]
                let fun_addr_ptr = self.function_slot_ptr(findex)?;
                let fun_addr = self
                    .builder
                    .build_load(ptr_type, fun_addr_ptr, "inst_closure_fun")?
                    .into_pointer_value();

                // This removes the first param (bound obj's type) from the fn signature
                let func_type_const = self.func_type_ptr(findex)?;
                // The METHOD's full type, unstripped: `hlp_alloc_closure_ptr`
                // does the single strip itself, exactly as upstream's
                // OInstanceClosure does (jit_emit.c passes
                // `functions[functions_indexes[fun]].type`) and as the
                // Cranelift tier does. Stripping here first made the
                // allocator strip a SECOND time, walking off the end of the
                // `hl_type_fun` it was handed.
                let closure_type: inkwell::values::BasicValueEnum = func_type_const.into();

                // Load bound object
                let obj_val =
                    self.builder
                        .build_load(ptr_type, registers[obj.0 as usize], "inst_obj")?;

                // Call hlp_alloc_closure_ptr(closure_type, fun_addr, obj_ptr)
                let alloc = self.declare_native(
                    "hlp_alloc_closure_ptr",
                    &[ptr_type.into(), ptr_type.into(), ptr_type.into()],
                    Some(ptr_type.into()),
                );
                let closure = self
                    .builder
                    .build_call(
                        alloc,
                        &[closure_type.into(), fun_addr.into(), obj_val.into()],
                        "inst_closure",
                    )?
                    .try_as_basic_value()
                    .basic()
                    .unwrap();
                self.builder
                    .build_store(registers[dst.0 as usize], closure)?;
            }

            // --- VirtualClosure: resolve proto method, create bound closure ---
            Opcode::VirtualClosure { dst, obj, field } => {
                let i8_type = self.context.i8_type();
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let obj_type_idx = f.regs[obj.0 as usize].0;
                let obj_type_info = self.types_[obj_type_idx].clone();

                // Resolve findex from proto table at compile time
                let findex = if let Some(ref obj_data) = obj_type_info.obj {
                    obj_data.proto[field.0 as usize].findex as usize
                } else {
                    return Err(anyhow!(
                        "VirtualClosure: obj register type has no proto table"
                    ));
                };

                if !self.lazy_compilation {
                    let (_function, is_placeholder) = self.get_or_create_function_value(findex)?;
                    if is_placeholder {
                        self.add_pending_compilation(findex);
                    }
                }

                // Load obj pointer
                let obj_val =
                    self.builder
                        .build_load(ptr_type, registers[obj.0 as usize], "vclos_obj")?;

                // Load function address from functions_ptrs[findex]
                let fun_addr_ptr = self.function_slot_ptr(findex)?;
                let fun_addr = self
                    .builder
                    .build_load(ptr_type, fun_addr_ptr, "vclos_fun")?
                    .into_pointer_value();

                // Virtual closures dispatch through the concrete object's
                // runtime proto. The static proto entry may name a base
                // implementation even when the object overrides it.
                let obj_ptr = obj_val.into_pointer_value();
                let obj_type_ptr = self
                    .builder
                    .build_load(ptr_type, obj_ptr, "vclos_obj_type")?
                    .into_pointer_value();
                // The concrete type's proto table, indexed by pindex.
                let vobj_proto = self.vobj_proto_ptr(obj_type_ptr)?;
                let runtime_fun_ptr = unsafe {
                    self.builder.build_gep(
                        ptr_type,
                        vobj_proto,
                        &[self.context.i32_type().const_int(field.0 as u64, false)],
                        "vclos_runtime_fun_gep",
                    )?
                };
                let runtime_fun = self
                    .builder
                    .build_load(ptr_type, runtime_fun_ptr, "vclos_runtime_fun")?
                    .into_pointer_value();
                let runtime_fun_present = self
                    .builder
                    .build_is_not_null(runtime_fun, "vclos_has_runtime_fun")?;
                let fun_addr = self
                    .builder
                    .build_select(
                        runtime_fun_present,
                        runtime_fun,
                        fun_addr,
                        "vclos_selected_fun",
                    )?
                    .into_pointer_value();

                let func_type_const = self.func_type_ptr(findex)?;
                // Full method type; the allocator strips once. See
                // OInstanceClosure above.
                let closure_type: inkwell::values::BasicValueEnum = func_type_const.into();

                // Call hlp_alloc_closure_ptr(closure_type, fun_addr, obj_ptr)
                let alloc = self.declare_native(
                    "hlp_alloc_closure_ptr",
                    &[ptr_type.into(), ptr_type.into(), ptr_type.into()],
                    Some(ptr_type.into()),
                );
                let closure = self
                    .builder
                    .build_call(
                        alloc,
                        &[closure_type.into(), fun_addr.into(), obj_ptr.into()],
                        "vclos",
                    )?
                    .try_as_basic_value()
                    .basic()
                    .unwrap();
                self.builder
                    .build_store(registers[dst.0 as usize], closure)?;
            }

            // --- DynGet: dynamic field access (stub) ---
            Opcode::DynGet { dst, obj, field } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let i32_type = self.context.i32_type();
                let i64_type = self.context.i64_type();

                let obj_val =
                    self.builder
                        .build_load(ptr_type, registers[obj.0 as usize], "dynget_obj")?;
                let field_name = &self.bytecode.strings[field.0].clone();
                let hfield = hl_hash_utf8(field_name);
                let hfield_val = i32_type.const_int(hfield as u64, true);

                let dst_type_idx = f.regs[dst.0 as usize].0;
                let dst_kind = self.types_[dst_type_idx].kind;

                match dst_kind {
                    hl_type_kind_HF64 => {
                        let getter = self.declare_native(
                            "hlp_dyn_getd",
                            &[ptr_type.into(), i32_type.into()],
                            Some(self.context.f64_type().into()),
                        );
                        let result = self.builder.build_call(
                            getter,
                            &[obj_val.into(), hfield_val.into()],
                            "dynget_d",
                        )?;
                        self.builder.build_store(
                            registers[dst.0 as usize],
                            result.try_as_basic_value().basic().unwrap(),
                        )?;
                    }
                    hl_type_kind_HF32 => {
                        let getter = self.declare_native(
                            "hlp_dyn_getf",
                            &[ptr_type.into(), i32_type.into()],
                            Some(self.context.f32_type().into()),
                        );
                        let result = self.builder.build_call(
                            getter,
                            &[obj_val.into(), hfield_val.into()],
                            "dynget_f",
                        )?;
                        self.builder.build_store(
                            registers[dst.0 as usize],
                            result.try_as_basic_value().basic().unwrap(),
                        )?;
                    }
                    hl_type_kind_HI64 => {
                        let getter = self.declare_native(
                            "hlp_dyn_geti64",
                            &[ptr_type.into(), i32_type.into()],
                            Some(i64_type.into()),
                        );
                        let result = self.builder.build_call(
                            getter,
                            &[obj_val.into(), hfield_val.into()],
                            "dynget_i64",
                        )?;
                        self.builder.build_store(
                            registers[dst.0 as usize],
                            result.try_as_basic_value().basic().unwrap(),
                        )?;
                    }
                    hl_type_kind_HI32 | hl_type_kind_HBOOL | hl_type_kind_HUI8
                    | hl_type_kind_HUI16 => {
                        let type_ptr = self
                            .get_initialized_type(dst_type_idx)?
                            .into_pointer_value();
                        let getter = self.declare_native(
                            "hlp_dyn_geti",
                            &[ptr_type.into(), i32_type.into(), ptr_type.into()],
                            Some(i32_type.into()),
                        );
                        let result = self.builder.build_call(
                            getter,
                            &[obj_val.into(), hfield_val.into(), type_ptr.into()],
                            "dynget_i",
                        )?;
                        self.builder.build_store(
                            registers[dst.0 as usize],
                            result.try_as_basic_value().basic().unwrap(),
                        )?;
                    }
                    _ => {
                        // Pointer types: hlp_dyn_getp(obj, hfield, dst_type)
                        let type_ptr = self
                            .get_initialized_type(dst_type_idx)?
                            .into_pointer_value();
                        let getter = self.declare_native(
                            "hlp_dyn_getp",
                            &[ptr_type.into(), i32_type.into(), ptr_type.into()],
                            Some(ptr_type.into()),
                        );
                        let result = self.builder.build_call(
                            getter,
                            &[obj_val.into(), hfield_val.into(), type_ptr.into()],
                            "dynget_p",
                        )?;
                        self.builder.build_store(
                            registers[dst.0 as usize],
                            result.try_as_basic_value().basic().unwrap(),
                        )?;
                    }
                }
            }

            // --- DynSet: dynamic field set ---
            Opcode::DynSet { obj, field, src } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let i32_type = self.context.i32_type();

                let obj_val =
                    self.builder
                        .build_load(ptr_type, registers[obj.0 as usize], "dynset_obj")?;
                let field_name = &self.bytecode.strings[field.0].clone();
                let hfield = hl_hash_utf8(field_name);
                let hfield_val = i32_type.const_int(hfield as u64, true);

                let src_type_idx = f.regs[src.0 as usize].0;
                let src_kind = self.types_[src_type_idx].kind;
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "dynset_src",
                )?;

                match src_kind {
                    hl_type_kind_HF64 => {
                        let setter = self.declare_native(
                            "hlp_dyn_setd",
                            &[
                                ptr_type.into(),
                                i32_type.into(),
                                self.context.f64_type().into(),
                            ],
                            None,
                        );
                        self.builder.build_call(
                            setter,
                            &[obj_val.into(), hfield_val.into(), src_val.into()],
                            "dynset_d",
                        )?;
                    }
                    hl_type_kind_HF32 => {
                        let setter = self.declare_native(
                            "hlp_dyn_setf",
                            &[
                                ptr_type.into(),
                                i32_type.into(),
                                self.context.f32_type().into(),
                            ],
                            None,
                        );
                        self.builder.build_call(
                            setter,
                            &[obj_val.into(), hfield_val.into(), src_val.into()],
                            "dynset_f",
                        )?;
                    }
                    hl_type_kind_HI64 => {
                        let setter = self.declare_native(
                            "hlp_dyn_seti64",
                            &[
                                ptr_type.into(),
                                i32_type.into(),
                                self.context.i64_type().into(),
                            ],
                            None,
                        );
                        self.builder.build_call(
                            setter,
                            &[obj_val.into(), hfield_val.into(), src_val.into()],
                            "dynset_i64",
                        )?;
                    }
                    hl_type_kind_HI32 | hl_type_kind_HBOOL | hl_type_kind_HUI8
                    | hl_type_kind_HUI16 => {
                        let type_ptr = self
                            .get_initialized_type(src_type_idx)?
                            .into_pointer_value();
                        // All four kinds share one setter, whose value
                        // parameter is i32 — but their registers are not: HBOOL
                        // loads as i1, HUI8 as i8, HUI16 as i16. Passing those
                        // straight through builds a call the LLVM verifier
                        // rejects ("Call parameter type does not match function
                        // signature"), which fails the whole module and drops
                        // the program back to the interpreter. Widen first;
                        // all three narrow kinds are unsigned, so zero-extend.
                        let src_int = src_val.into_int_value();
                        let src_i32 = if src_int.get_type().get_bit_width() < 32 {
                            self.builder
                                .build_int_z_extend(src_int, i32_type, "dynset_src_i32")?
                        } else {
                            src_int
                        };
                        let setter = self.declare_native(
                            "hlp_dyn_seti",
                            &[
                                ptr_type.into(),
                                i32_type.into(),
                                ptr_type.into(),
                                i32_type.into(),
                            ],
                            None,
                        );
                        self.builder.build_call(
                            setter,
                            &[
                                obj_val.into(),
                                hfield_val.into(),
                                type_ptr.into(),
                                src_i32.into(),
                            ],
                            "dynset_i",
                        )?;
                    }
                    _ => {
                        // Pointer types: hlp_dyn_setp(obj, hfield, type, value)
                        let type_ptr = self
                            .get_initialized_type(src_type_idx)?
                            .into_pointer_value();
                        let setter = self.declare_native(
                            "hlp_dyn_setp",
                            &[
                                ptr_type.into(),
                                i32_type.into(),
                                ptr_type.into(),
                                ptr_type.into(),
                            ],
                            None,
                        );
                        self.builder.build_call(
                            setter,
                            &[
                                obj_val.into(),
                                hfield_val.into(),
                                type_ptr.into(),
                                src_val.into(),
                            ],
                            "dynset_p",
                        )?;
                    }
                }
            }

            // --- Bytes: load bytes constant ---
            Opcode::Bytes { dst, ptr } => {
                if let Some(bytes_global) = self.ensure_bytes_global(ptr.0) {
                    self.builder
                        .build_store(registers[dst.0 as usize], bytes_global.as_pointer_value())?;
                } else {
                    let null_ptr = self.context.ptr_type(AddressSpace::default()).const_null();
                    self.builder
                        .build_store(registers[dst.0 as usize], null_ptr)?;
                }
            }

            // --- Enum opcodes ---
            Opcode::EnumAlloc { dst, construct } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let i32_type = self.context.i32_type();
                let type_index = f.regs[dst.0 as usize].0;
                let type_ptr = self.get_initialized_type(type_index)?.into_pointer_value();

                let alloc_enum = self.declare_native(
                    "hlp_alloc_enum",
                    &[ptr_type.into(), i32_type.into()],
                    Some(ptr_type.into()),
                );
                let construct_val = i32_type.const_int(construct.0 as u64, false);
                let result = self.builder.build_call(
                    alloc_enum,
                    &[type_ptr.into(), construct_val.into()],
                    "enum_alloc",
                )?;
                self.builder.build_store(
                    registers[dst.0 as usize],
                    result.try_as_basic_value().basic().unwrap(),
                )?;
            }
            Opcode::MakeEnum {
                dst,
                construct,
                args,
            } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let i32_type = self.context.i32_type();
                let i8_type = self.context.i8_type();
                let type_index = f.regs[dst.0 as usize].0;
                let type_ptr = self.get_initialized_type(type_index)?.into_pointer_value();

                // Allocate the enum
                let alloc_enum = self.declare_native(
                    "hlp_alloc_enum",
                    &[ptr_type.into(), i32_type.into()],
                    Some(ptr_type.into()),
                );
                let construct_val = i32_type.const_int(construct.0 as u64, false);
                let venum_ptr = self
                    .builder
                    .build_call(
                        alloc_enum,
                        &[type_ptr.into(), construct_val.into()],
                        "make_enum",
                    )?
                    .try_as_basic_value()
                    .basic()
                    .unwrap()
                    .into_pointer_value();

                // Write each arg at its pre-computed offset
                let tenum = self.types_[type_index]
                    .tenum
                    .as_ref()
                    .ok_or_else(|| anyhow!("MakeEnum: type {} is not an enum", type_index))?;
                let construct_info = &tenum.constructs[construct.0];

                for (j, arg) in args.iter().enumerate() {
                    let arg_val = self.builder.build_load(
                        reg_types[arg.0 as usize],
                        registers[arg.0 as usize],
                        &format!("make_enum_arg_{}", j),
                    )?;
                    let offset = construct_info.offsets[j] as u64;
                    let param_ptr = unsafe {
                        self.builder.build_gep(
                            i8_type,
                            venum_ptr,
                            &[self.context.i64_type().const_int(offset, false)],
                            &format!("make_enum_param_{}", j),
                        )?
                    };
                    self.builder.build_store(param_ptr, arg_val)?;
                }

                self.builder
                    .build_store(registers[dst.0 as usize], venum_ptr)?;
            }
            Opcode::EnumIndex { dst, value } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let venum_ptr = self
                    .builder
                    .build_load(ptr_type, registers[value.0 as usize], "enumidx_ptr")?
                    .into_pointer_value();
                // venum.index is i32 at offset 8
                let index_gep = unsafe {
                    self.builder.build_gep(
                        self.context.i8_type(),
                        venum_ptr,
                        &[self
                            .context
                            .i64_type()
                            .const_int(self.target_abi.venum_index_offset(), false)],
                        "enumidx_gep",
                    )?
                };
                let index_val =
                    self.builder
                        .build_load(self.context.i32_type(), index_gep, "enumidx_val")?;
                self.builder
                    .build_store(registers[dst.0 as usize], index_val)?;
            }
            Opcode::EnumField {
                dst,
                value,
                construct,
                field,
            } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let venum_ptr = self
                    .builder
                    .build_load(ptr_type, registers[value.0 as usize], "enumfield_ptr")?
                    .into_pointer_value();

                let value_type_idx = f.regs[value.0 as usize].0;
                let tenum = self.types_[value_type_idx]
                    .tenum
                    .as_ref()
                    .ok_or_else(|| anyhow!("EnumField: type {} is not an enum", value_type_idx))?;
                let construct_info = &tenum.constructs[construct.0];
                let offset = construct_info.offsets[field.0] as u64;

                let param_ptr = unsafe {
                    self.builder.build_gep(
                        self.context.i8_type(),
                        venum_ptr,
                        &[self.context.i64_type().const_int(offset, false)],
                        "enumfield_gep",
                    )?
                };
                let val = self.builder.build_load(
                    reg_types[dst.0 as usize],
                    param_ptr,
                    "enumfield_val",
                )?;
                self.builder.build_store(registers[dst.0 as usize], val)?;
            }
            Opcode::SetEnumField { value, field, src } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let venum_ptr = self
                    .builder
                    .build_load(ptr_type, registers[value.0 as usize], "setenumfield_ptr")?
                    .into_pointer_value();
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "setenumfield_val",
                )?;

                // Scan backwards to find the preceding EnumAlloc targeting the same register
                let value_type_idx = f.regs[value.0 as usize].0;
                let tenum = self.types_[value_type_idx].tenum.as_ref().ok_or_else(|| {
                    anyhow!("SetEnumField: type {} is not an enum", value_type_idx)
                })?;

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
                    self.builder.build_gep(
                        self.context.i8_type(),
                        venum_ptr,
                        &[self.context.i64_type().const_int(offset, false)],
                        "setenumfield_gep",
                    )?
                };
                self.builder.build_store(param_ptr, src_val)?;
            }

            // --- Memory access ---
            Opcode::GetI8 { dst, bytes, index } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let base = self
                    .builder
                    .build_load(ptr_type, registers[bytes.0 as usize], "geti8_base")?
                    .into_pointer_value();
                let idx = self
                    .builder
                    .build_load(
                        self.context.i32_type(),
                        registers[index.0 as usize],
                        "geti8_idx",
                    )?
                    .into_int_value();
                let addr = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), base, &[idx], "geti8_addr")?
                };
                let val = self
                    .builder
                    .build_load(self.context.i8_type(), addr, "geti8_val")?
                    .into_int_value();
                let ext =
                    self.builder
                        .build_int_z_extend(val, self.context.i32_type(), "geti8_zext")?;
                self.builder.build_store(registers[dst.0 as usize], ext)?;
            }
            Opcode::GetI16 { dst, bytes, index } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let base = self
                    .builder
                    .build_load(ptr_type, registers[bytes.0 as usize], "geti16_base")?
                    .into_pointer_value();
                let idx = self
                    .builder
                    .build_load(
                        self.context.i32_type(),
                        registers[index.0 as usize],
                        "geti16_idx",
                    )?
                    .into_int_value();
                let addr = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), base, &[idx], "geti16_addr")?
                };
                let val = self
                    .builder
                    .build_load(self.context.i16_type(), addr, "geti16_val")?
                    .into_int_value();
                let ext =
                    self.builder
                        .build_int_z_extend(val, self.context.i32_type(), "geti16_zext")?;
                self.builder.build_store(registers[dst.0 as usize], ext)?;
            }
            Opcode::GetMem { dst, bytes, index } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let base = self
                    .builder
                    .build_load(ptr_type, registers[bytes.0 as usize], "getmem_base")?
                    .into_pointer_value();
                let idx = self
                    .builder
                    .build_load(
                        self.context.i32_type(),
                        registers[index.0 as usize],
                        "getmem_idx",
                    )?
                    .into_int_value();
                let addr = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), base, &[idx], "getmem_addr")?
                };
                let val = self
                    .builder
                    .build_load(reg_types[dst.0 as usize], addr, "getmem_val")?;
                if let Some(i) = val.as_instruction_value() {
                    self.tbaa.tag(i, self.tbaa.payload());
                }
                self.builder.build_store(registers[dst.0 as usize], val)?;
            }
            Opcode::SetI8 { bytes, index, src } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let base = self
                    .builder
                    .build_load(ptr_type, registers[bytes.0 as usize], "seti8_base")?
                    .into_pointer_value();
                let idx = self
                    .builder
                    .build_load(
                        self.context.i32_type(),
                        registers[index.0 as usize],
                        "seti8_idx",
                    )?
                    .into_int_value();
                let src_val = self
                    .builder
                    .build_load(
                        reg_types[src.0 as usize],
                        registers[src.0 as usize],
                        "seti8_src",
                    )?
                    .into_int_value();
                let addr = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), base, &[idx], "seti8_addr")?
                };
                let trunc = self.builder.build_int_truncate(
                    src_val,
                    self.context.i8_type(),
                    "seti8_trunc",
                )?;
                self.builder.build_store(addr, trunc)?;
            }
            Opcode::SetI16 { bytes, index, src } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let base = self
                    .builder
                    .build_load(ptr_type, registers[bytes.0 as usize], "seti16_base")?
                    .into_pointer_value();
                let idx = self
                    .builder
                    .build_load(
                        self.context.i32_type(),
                        registers[index.0 as usize],
                        "seti16_idx",
                    )?
                    .into_int_value();
                let src_val = self
                    .builder
                    .build_load(
                        reg_types[src.0 as usize],
                        registers[src.0 as usize],
                        "seti16_src",
                    )?
                    .into_int_value();
                let addr = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), base, &[idx], "seti16_addr")?
                };
                let trunc = self.builder.build_int_truncate(
                    src_val,
                    self.context.i16_type(),
                    "seti16_trunc",
                )?;
                self.builder.build_store(addr, trunc)?;
            }
            Opcode::SetMem { bytes, index, src } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let base = self
                    .builder
                    .build_load(ptr_type, registers[bytes.0 as usize], "setmem_base")?
                    .into_pointer_value();
                let idx = self
                    .builder
                    .build_load(
                        self.context.i32_type(),
                        registers[index.0 as usize],
                        "setmem_idx",
                    )?
                    .into_int_value();
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "setmem_src",
                )?;
                let addr = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), base, &[idx], "setmem_addr")?
                };
                let st = self.builder.build_store(addr, src_val)?;
                self.tbaa.tag(st, self.tbaa.payload());
            }

            // --- Array operations ---
            Opcode::SetArray { array, index, src } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let i32_type = self.context.i32_type();
                let i8_type = self.context.i8_type();

                let arr = self
                    .builder
                    .build_load(ptr_type, registers[array.0 as usize], "setarr_ptr")?
                    .into_pointer_value();
                let idx = self
                    .builder
                    .build_load(i32_type, registers[index.0 as usize], "setarr_idx")?
                    .into_int_value();
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "setarr_val",
                )?;

                // Data starts at offset 24 (sizeof(varray))
                let data_ptr = unsafe {
                    self.builder.build_gep(
                        i8_type,
                        arr,
                        &[self
                            .context
                            .i64_type()
                            .const_int(self.target_abi.varray_data_offset(), false)],
                        "setarr_data",
                    )?
                };

                // Element size from the source register's
                // kind, via the table `crate::layout` shares with the Cranelift
                // tier so the two cannot index an array differently.
                let src_type_idx = f.regs[src.0 as usize].0;
                let src_kind = self.types_[src_type_idx].kind;
                let elem_size = crate::layout::array_elem_size_for(
                    src_kind,
                    self.target_abi.pointer_bytes() as i32,
                ) as u64;

                let elem_size_val = i32_type.const_int(elem_size, false);
                let byte_offset =
                    self.builder
                        .build_int_mul(idx, elem_size_val, "setarr_offset")?;
                let slot = unsafe {
                    self.builder
                        .build_gep(i8_type, data_ptr, &[byte_offset], "setarr_slot")?
                };
                let st = self.builder.build_store(slot, src_val)?;
                self.tbaa.tag(st, self.tbaa.payload());
            }
            Opcode::ArraySize { dst, array } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let arr = self
                    .builder
                    .build_load(ptr_type, registers[array.0 as usize], "arrsize_ptr")?
                    .into_pointer_value();
                // varray.size is at offset 16
                let size_gep = unsafe {
                    self.builder.build_gep(
                        self.context.i8_type(),
                        arr,
                        &[self
                            .context
                            .i64_type()
                            .const_int(self.target_abi.varray_size_offset(), false)],
                        "arrsize_gep",
                    )?
                };
                let size =
                    self.builder
                        .build_load(self.context.i32_type(), size_gep, "arrsize_val")?;
                if let Some(i) = size.as_instruction_value() {
                    self.tbaa.tag(i, self.tbaa.array_len());
                }
                self.builder.build_store(registers[dst.0 as usize], size)?;
            }

            // --- GetTID: get type kind ---
            Opcode::GetTID { dst, src } => {
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "gettid_src",
                )?;
                let src_type_kind = self.types_[f.regs[src.0 as usize].0].kind;
                if src_val.is_pointer_value() {
                    let obj = src_val.into_pointer_value();
                    if src_type_kind == hl_type_kind_HTYPE {
                        // Source is hl_type* — kind is directly at offset 0
                        let kind =
                            self.builder
                                .build_load(self.context.i32_type(), obj, "gettid_kind")?;
                        self.builder.build_store(registers[dst.0 as usize], kind)?;
                    } else {
                        // Source is an object — load obj->t (offset 0), then t->kind (offset 0)
                        let ptr_type = self.context.ptr_type(AddressSpace::default());
                        let t_ptr = self
                            .builder
                            .build_load(ptr_type, obj, "gettid_type")?
                            .into_pointer_value();
                        let kind = self.builder.build_load(
                            self.context.i32_type(),
                            t_ptr,
                            "gettid_kind",
                        )?;
                        self.builder.build_store(registers[dst.0 as usize], kind)?;
                    }
                } else {
                    // Compile-time: type kind is known
                    let type_idx = f.regs[src.0 as usize].0;
                    let kind = self.types_[type_idx].kind;
                    let kind_val = self.context.i32_type().const_int(kind as u64, false);
                    self.builder
                        .build_store(registers[dst.0 as usize], kind_val)?;
                }
            }

            // --- Assert: throw "assert", catchably ---
            // Upstream OAssert calls hl_assert() -> hl_error("assert"). The
            // unit suite EXECUTES this opcode on purpose (assert-testing
            // cases), so `unreachable` here was a licence to miscompile a
            // path that runs. hlp_error longjmps to the active trap.
            Opcode::Assert => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let err_fn_type = self.context.void_type().fn_type(&[ptr_type.into()], true);
                let err_ptr = self.error_function_ptr()?;
                let msg_ptr = self.utf16_message("assert")?;
                self.builder.build_indirect_call(
                    err_fn_type,
                    err_ptr,
                    &[msg_ptr.into()],
                    "assert_throw",
                )?;
                self.builder.build_unreachable()?;
            }

            // --- Prefetch: emit target-specific cache hint via inline asm ---
            Opcode::Prefetch { value, field, mode } => {
                let _ = field; // field offset elision is safe; prefetch is purely a hint
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let base = self
                    .builder
                    .build_load(ptr_type, registers[value.0 as usize], "prefetch_ptr")?
                    .into_pointer_value();
                let void_type = self.context.void_type();
                let fn_type = void_type.fn_type(&[ptr_type.into()], false);

                #[cfg(target_arch = "x86_64")]
                let hint = match mode {
                    0 => "prefetcht0 ($0)",
                    1 => "prefetcht1 ($0)",
                    2 => "prefetcht2 ($0)",
                    _ => "prefetchnta ($0)",
                };
                #[cfg(target_arch = "aarch64")]
                let hint = match mode {
                    0 => "prfm pldl1keep, [$0]",
                    1 => "prfm pldl2keep, [$0]",
                    2 => "prfm pldl3keep, [$0]",
                    _ => "prfm pldl1strm, [$0]",
                };
                // Fallback for other architectures: no-op
                #[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
                let hint = {
                    let _ = mode;
                    ""
                };

                if !hint.is_empty() {
                    let asm_val = self.context.create_inline_asm(
                        fn_type,
                        hint.to_string(),
                        "r".to_string(),
                        true,
                        false,
                        Some(inkwell::InlineAsmDialect::ATT),
                        false,
                    );
                    self.builder.build_indirect_call(
                        fn_type,
                        asm_val,
                        &[base.into()],
                        "prefetch",
                    )?;
                }
            }
            // --- Asm: inline assembly byte emission ---
            //
            // HashLink OAsm modes:
            //   0 → emit raw byte (p2) into code stream
            //   1 → mark physical register (p2) as clobbered
            //   2 → load VM register into physical register (p2)
            //   3 → store physical register (p2) into VM register
            //   4 → naked function (strip prologue; must be first opcode)
            //
            // Modes 1-3 are register-allocator directives for HashLink's custom JIT;
            // LLVM handles register allocation automatically so these are no-ops.
            // Mode 0 emits raw bytes via `.byte` — works on all LLVM targets.
            Opcode::Asm { mode, value, reg } => {
                let _ = reg;
                match mode {
                    0 => {
                        let byte = *value as u8;
                        let void_type = self.context.void_type();
                        let fn_type = void_type.fn_type(&[], false);
                        let asm_val = self.context.create_inline_asm(
                            fn_type,
                            format!(".byte 0x{byte:02x}"),
                            String::new(),
                            true,  // side effects
                            false, // align stack
                            Some(inkwell::InlineAsmDialect::ATT),
                            false, // can_throw
                        );
                        self.builder
                            .build_indirect_call(fn_type, asm_val, &[], "")?;
                    }
                    1 | 2 | 3 | 4 => {
                        // Register hints / naked: LLVM handles allocation automatically.
                    }
                    _ => {}
                }
            }
            // --- RefData: extract value pointer from vdynamic (offset 8) ---
            Opcode::RefData { dst, src } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let obj = self
                    .builder
                    .build_load(ptr_type, registers[src.0 as usize], "refdata_src")?
                    .into_pointer_value();
                // ORefData is the address of the first element: the varray
                // header is {t, at, size, pad} = 24 bytes. Loading at +8
                // returned the ELEMENT TYPE descriptor, so every hl.Bytes /
                // NativeArray ref then read and WROTE through an hl_type.
                let header = self.target_abi.varray_data_offset();
                let data_gep = unsafe {
                    self.builder.build_gep(
                        self.context.i8_type(),
                        obj,
                        &[self.context.i64_type().const_int(header, false)],
                        "refdata_gep",
                    )?
                };
                let _ = ptr_type;
                self.builder
                    .build_store(registers[dst.0 as usize], data_gep)?;
            }
            // --- RefOffset: pointer + byte offset ---
            Opcode::RefOffset { dst, reg, offset } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let base = self
                    .builder
                    .build_load(ptr_type, registers[reg.0 as usize], "refoff_base")?
                    .into_pointer_value();
                let off = self
                    .builder
                    .build_load(
                        self.context.i32_type(),
                        registers[offset.0 as usize],
                        "refoff_off",
                    )?
                    .into_int_value();
                let result = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), base, &[off], "refoff_result")?
                };
                self.builder
                    .build_store(registers[dst.0 as usize], result)?;
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
        // The bindgen alias, not a bare integer: MSVC types the C enum i32
        // where clang types it u32, so only the alias compiles on both.
        a_kind: crate::hl::hl_type_kind,
        int_pred: IntPredicate,
        float_pred: FloatPredicate,
        i: usize,
        offset: i32,
        opcode_blocks: &[BasicBlock<'ctx>],
    ) -> Result<()> {
        let a_val =
            self.builder
                .build_load(reg_types[a.0 as usize], registers[a.0 as usize], "cmp_a")?;
        let b_val =
            self.builder
                .build_load(reg_types[b.0 as usize], registers[b.0 as usize], "cmp_b")?;
        let cmp = match a_val.get_type().as_any_type_enum() {
            AnyTypeEnum::IntType(_) => self.builder.build_int_compare(
                int_pred,
                a_val.into_int_value(),
                b_val.into_int_value(),
                "cmp",
            )?,
            AnyTypeEnum::FloatType(_) => self.builder.build_float_compare(
                float_pred,
                a_val.into_float_value(),
                b_val.into_float_value(),
                "cmp",
            )?,
            AnyTypeEnum::PointerType(_) => {
                // A String's identity is not its value, and hlp_dyn_compare is the
                // one place that knows the difference: it uses the type's compareFun
                // when there is one, then compares the UTF-16 payload of
                // String-shaped objects, and only then falls back to pointers — so
                // routing HOBJ through it fixes `a == b` on strings while leaving
                // identity semantics intact for every other object.
                //
                // Passing an object pointer as a vdynamic* is sound because an
                // object's first word IS its hl_type*, which is all dyn_compare
                // reads of it. HBYTES and HSTRUCT must NOT come here: a raw byte
                // buffer and a struct both lack that header, so dyn_compare would
                // read their payload as a type.
                if a_kind == hl_type_kind_HDYN
                    || a_kind == hl_type_kind_HNULL
                    || a_kind == hl_type_kind_HOBJ
                    || a_kind == hl_type_kind_HVIRTUAL
                {
                    let ptr_type = self.context.ptr_type(AddressSpace::default());
                    let i32_type = self.context.i32_type();
                    let dyn_compare = self.declare_native(
                        "hlp_dyn_compare",
                        &[ptr_type.into(), ptr_type.into()],
                        Some(i32_type.into()),
                    );
                    let result = self
                        .builder
                        .build_call(dyn_compare, &[a_val.into(), b_val.into()], "dyn_cmp")?
                        .try_as_basic_value()
                        .basic()
                        .unwrap()
                        .into_int_value();
                    let zero = i32_type.const_int(0, false);
                    self.builder
                        .build_int_compare(int_pred, result, zero, "cmp")?
                } else {
                    // Non-dynamic pointer: identity comparison
                    let a_int = self.builder.build_ptr_to_int(
                        a_val.into_pointer_value(),
                        self.context.i64_type(),
                        "a_int",
                    )?;
                    let b_int = self.builder.build_ptr_to_int(
                        b_val.into_pointer_value(),
                        self.context.i64_type(),
                        "b_int",
                    )?;
                    self.builder
                        .build_int_compare(int_pred, a_int, b_int, "cmp")?
                }
            }
            _ => return Err(anyhow!("Unsupported types for comparison jump")),
        };
        let target = opcode_blocks[(i as i32 + 1 + offset) as usize];
        let next = opcode_blocks[i + 1];
        self.builder.build_conditional_branch(cmp, target, next)?;
        Ok(())
    }

    /// Helper: emit a binary arithmetic operation
    /// A shift count in HashLink is masked to the operand width (x86 `shl cl`
    /// and arm64 `lslv` both do; the interpreter does with `wrapping_shl`).
    /// LLVM's `shl`/`lshr`/`ashr` are POISON for a count >= the width, so
    /// `1 << 32`, `x << -1` or `Int64 << (i32 count)` constant-folded to
    /// arbitrary values whenever the optimizer could see the count. Bring the
    /// count to the value's width first (Int64 shifts carry an I32 count),
    /// then mask it.
    fn shift_operands(
        b: &inkwell::builder::Builder<'ctx>,
        x: inkwell::values::IntValue<'ctx>,
        y: inkwell::values::IntValue<'ctx>,
    ) -> Result<(
        inkwell::values::IntValue<'ctx>,
        inkwell::values::IntValue<'ctx>,
    )> {
        let width = x.get_type().get_bit_width();
        let y = if y.get_type().get_bit_width() > width {
            b.build_int_truncate(y, x.get_type(), "shift_count")?
        } else if y.get_type().get_bit_width() < width {
            b.build_int_z_extend(y, x.get_type(), "shift_count")?
        } else {
            y
        };
        let mask = x.get_type().const_int(u64::from(width - 1), false);
        Ok((x, b.build_and(y, mask, "shift_mask")?))
    }
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
        F: FnOnce(
            &Builder<'ctx>,
            BasicValueEnum<'ctx>,
            BasicValueEnum<'ctx>,
        ) -> Result<BasicValueEnum<'ctx>>,
    {
        let a_val =
            self.builder
                .build_load(reg_types[a.0 as usize], registers[a.0 as usize], "a_val")?;
        let b_val =
            self.builder
                .build_load(reg_types[b.0 as usize], registers[b.0 as usize], "b_val")?;
        let result = op_fn(&self.builder, a_val, b_val)?;
        self.builder
            .build_store(registers[dst.0 as usize], result)?;
        Ok(())
    }
}
