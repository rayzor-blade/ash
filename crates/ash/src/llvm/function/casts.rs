//! Emitting the AIR cast and unary-op instructions.
//!
//! Each emitter takes the AIR instruction's own fields. A value's slot is
//! `registers[v.idx()]` / `reg_types[v.idx()]` and its HL type is
//! `lowering.regs[v.idx()]`; cells follow the values at `cell_base`.

use air::v2::ir::{CastKind as AirCastKind, UnOp as AirUnOp, ValueId};
use inkwell::types::{AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicTypeEnum};
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, PointerValue};
use inkwell::{AddressSpace, IntPredicate};

use crate::hl::{
    hl_type_kind_HABSTRACT, hl_type_kind_HBOOL, hl_type_kind_HDYN, hl_type_kind_HF32,
    hl_type_kind_HF64, hl_type_kind_HI32, hl_type_kind_HI64, hl_type_kind_HNULL,
    hl_type_kind_HOBJ, hl_type_kind_HSTRUCT, hl_type_kind_HUI16, hl_type_kind_HUI8,
    hl_type_kind_HVIRTUAL,
};
use crate::llvm::module::JITModule;
use crate::types::HLFunction;
use anyhow::{anyhow, bail, Result};

impl<'ctx> JITModule<'ctx> {
    /// `Cast { kind, dst, src }`: one of the seven HL conversion opcodes,
    /// selected by `kind`.
    pub(super) fn emit_air_cast(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        kind: AirCastKind,
        dst: ValueId,
        src: ValueId,
    ) -> Result<()> {
        match kind {
            AirCastKind::ToDyn => self.emit_air_cast_to_dyn(lowering, registers, reg_types, dst, src),
            AirCastKind::ToSFloat => {
                self.emit_air_cast_to_sfloat(lowering, registers, reg_types, dst, src)
            }
            AirCastKind::ToUFloat => {
                self.emit_air_cast_to_ufloat(lowering, registers, reg_types, dst, src)
            }
            AirCastKind::ToInt => self.emit_air_cast_to_int(lowering, registers, reg_types, dst, src),
            AirCastKind::SafeCast => {
                self.emit_air_cast_safe(lowering, registers, reg_types, dst, src)
            }
            AirCastKind::UnsafeCast => {
                self.emit_air_cast_unsafe(lowering, registers, reg_types, dst, src)
            }
            AirCastKind::ToVirtual => {
                self.emit_air_cast_to_virtual(lowering, registers, reg_types, dst, src)
            }
        }
    }

    /// `UnOp { op, dst, src }` for `Neg` and `Not`. `Incr`/`Decr` are SSA
    /// defs with their own direct arm and are refused here.
    pub(super) fn emit_air_un_op(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        op: AirUnOp,
        dst: ValueId,
        src: ValueId,
    ) -> Result<()> {
        match op {
            AirUnOp::Neg => {
                let src_val = self.builder.build_load(
                    reg_types[src.idx()],
                    registers[src.idx()],
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
                    .build_store(registers[dst.idx()], result)?;
            }
            AirUnOp::Not => {
                let src_val = self
                    .builder
                    .build_load(
                        reg_types[src.idx()],
                        registers[src.idx()],
                        "not_src",
                    )?
                    .into_int_value();
                let result = self.builder.build_not(src_val, "not")?;
                self.builder
                    .build_store(registers[dst.idx()], result)?;
            }
            AirUnOp::Incr | AirUnOp::Decr => {
                bail!("emit_air_un_op: Incr/Decr have their own direct arm")
            }
        }
        Ok(())
    }

    fn emit_air_cast_to_dyn(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        dst: ValueId,
        src: ValueId,
    ) -> Result<()> {
        let src_type_idx = lowering.regs[src.idx()].0;
        let src_val = self.builder.build_load(
            reg_types[src.idx()],
            registers[src.idx()],
            "todyn_src",
        )?;
        // For pointer types (objects, strings, etc.), just copy the pointer.
        // HABSTRACT is excepted: it is a pointer whose target has no
        // hl_type header, so a Dynamic holding it raw makes the
        // hl_dyn_castp on the way back out read the payload as a type.
        let src_is_abstract = self.types_[src_type_idx].kind == hl_type_kind_HABSTRACT;
        if src_val.is_pointer_value() && !src_is_abstract {
            self.builder
                .build_store(registers[dst.idx()], src_val)?;
        } else {
            // Primitives: alloca temp, store value, call hlp_make_dyn(&temp, type_ptr)
            let ptr_type = self.context.ptr_type(AddressSpace::default());
            let temp = self.entry_alloca(reg_types[src.idx()], "todyn_temp")?;
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
                registers[dst.idx()],
                result.try_as_basic_value().basic().unwrap(),
            )?;
        }
        Ok(())
    }

    fn emit_air_cast_unsafe(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        dst: ValueId,
        src: ValueId,
    ) -> Result<()> {
        let src_val = self.builder.build_load(
            reg_types[src.idx()],
            registers[src.idx()],
            "unsafe_cast_src",
        )?;
        self.builder
            .build_store(registers[dst.idx()], src_val)?;
        Ok(())
    }

    fn emit_air_cast_to_sfloat(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        dst: ValueId,
        src: ValueId,
    ) -> Result<()> {
        let src_val = self.builder.build_load(
            reg_types[src.idx()],
            registers[src.idx()],
            "tosfloat_src",
        )?;
        let f64_type = self.context.f64_type();
        let src_kind = self.types_[lowering.regs[src.idx()].0].kind;
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
            registers,
            reg_types,
            dst.idx(),
            result.into_float_value(),
        )?;
        Ok(())
    }

    fn emit_air_cast_to_ufloat(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        dst: ValueId,
        src: ValueId,
    ) -> Result<()> {
        let src_val = self.builder.build_load(
            reg_types[src.idx()],
            registers[src.idx()],
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
            registers,
            reg_types,
            dst.idx(),
            result.into_float_value(),
        )?;
        Ok(())
    }

    fn emit_air_cast_to_int(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        dst: ValueId,
        src: ValueId,
    ) -> Result<()> {
        let src_val = self.builder.build_load(
            reg_types[src.idx()],
            registers[src.idx()],
            "toint_src",
        )?;
        // Convert straight to the destination register's width. Going
        // through i32 first then widening with cast_for_call (a zext)
        // turned -1 into 4294967295 for I32 -> I64 and truncated any
        // Float beyond 2^31 for F64 -> I64. HashLink: MOVSXD / CVTTSD2SI
        // at the destination width. HUI8/HUI16 registers hold unsigned
        // values (MOVZX), so those widen with zero extension.
        let dst_ty = reg_types[dst.idx()];
        let dst_int = if dst_ty.is_int_type() {
            dst_ty.into_int_type()
        } else {
            self.context.i32_type()
        };
        let src_kind = self.types_[lowering.regs[src.idx()].0].kind;
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
        let result = self.cast_for_call(result, reg_types[dst.idx()])?;
        self.builder
            .build_store(registers[dst.idx()], result)?;
        Ok(())
    }

    /// SafeCast: unbox HNULL(T)/HDYN -> primitive T, otherwise copy.
    fn emit_air_cast_safe(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        dst: ValueId,
        src: ValueId,
    ) -> Result<()> {
        let src_type_idx = lowering.regs[src.idx()].0;
        let dst_type_idx = lowering.regs[dst.idx()].0;
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
                .build_load(ptr_type, registers[src.idx()], "safecast_src")?
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
            // an Int box as f64 yields its bits as a denormal. Coerce
            // through the dyn-cast helpers instead; they switch on the
            // box's own runtime type and match the interpreter and upstream.
            self.builder.position_at_end(unbox_bb);
            let dst_llvm_type = reg_types[dst.idx()];
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
                // non-zero value is true; truncating to i1 would keep only
                // the low bit.
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
                .build_store(registers[dst.idx()], unboxed)?;
            self.builder.build_unconditional_branch(done_bb)?;

            // Null path: store default value (0/false/0.0)
            self.builder.position_at_end(null_bb);
            let default_val = dst_llvm_type.const_zero();
            self.builder
                .build_store(registers[dst.idx()], default_val)?;
            self.builder.build_unconditional_branch(done_bb)?;

            self.builder.position_at_end(done_bb);
        } else if (dst_kind == hl_type_kind_HNULL || dst_kind == hl_type_kind_HDYN)
            && !reg_types[src.idx()].is_pointer_type()
        {
            // The other direction: a primitive INTO a box. Haxe spells
            // `Null<Int64> = 29` as OSafeCast from an I32 register. Box as
            // ToDyn does, but at the box's own type: for Null<T> that is T,
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
                reg_types[src.idx()],
                registers[src.idx()],
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
                registers[dst.idx()],
                boxed.try_as_basic_value().basic().unwrap(),
            )?;
        } else if reg_types[src.idx()].is_pointer_type()
            && matches!(dst_kind,
                k if k == hl_type_kind_HBOOL || k == hl_type_kind_HI32
                    || k == hl_type_kind_HF64 || k == hl_type_kind_HF32
                    || k == hl_type_kind_HI64 || k == hl_type_kind_HUI8
                    || k == hl_type_kind_HUI16)
        {
            // A reference cast to a number: `cast("foo", Int)`. There is
            // no value to extract, so the only correct outcome is the
            // runtime's "Can't cast String to Int". The dyn-cast helpers
            // switch on the SOURCE type and raise for anything not numeric,
            // so handing them the register and both types gets exactly
            // the interpreter's behaviour.
            let ptr_type = self.context.ptr_type(AddressSpace::default());
            let src_type_ptr = self
                .get_initialized_type(src_type_idx)?
                .into_pointer_value();
            let dst_type_ptr = self
                .get_initialized_type(dst_type_idx)?
                .into_pointer_value();
            let dst_llvm_type = reg_types[dst.idx()];
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
                vec![registers[src.idx()].into(), src_type_ptr.into()];
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
            self.builder.build_store(registers[dst.idx()], value)?;
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
                    registers[src.idx()].into(),
                    src_type_ptr.into(),
                    dst_type_ptr.into(),
                ],
                "dyn_castp",
            )?;
            self.builder.build_store(
                registers[dst.idx()],
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
            // A virtual is a WRAPPER: `vvirtual { t, value, next, fields.. }`,
            // so a plain pointer copy would apply the class's field offsets
            // to the wrapper. The runtime's castp unwraps `value` and runs
            // the class check, including the re-wrap for a
            // virtual-to-virtual cast.
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
                    registers[src.idx()].into(),
                    src_type_ptr.into(),
                    dst_type_ptr.into(),
                ],
                "dyn_castp_obj",
            )?;
            self.builder.build_store(
                registers[dst.idx()],
                result.try_as_basic_value().basic().unwrap(),
            )?;
        } else {
            // Same type or non-dynamic: simple pointer copy
            let src_val = self.builder.build_load(
                reg_types[src.idx()],
                registers[src.idx()],
                "safecast_src",
            )?;
            self.builder
                .build_store(registers[dst.idx()], src_val)?;
        }
        Ok(())
    }

    /// ToVirtual: wrap object in a vvirtual with resolved field/method pointers.
    fn emit_air_cast_to_virtual(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        dst: ValueId,
        src: ValueId,
    ) -> Result<()> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let dst_type_idx = lowering.regs[dst.idx()].0;
        let dst_kind = self.types_[dst_type_idx].kind;

        if dst_kind == hl_type_kind_HVIRTUAL {
            // Get the full C-side hl_type pointer for the virtual type
            let vt_ptr = self
                .get_initialized_type(dst_type_idx)?
                .into_pointer_value();
            let src_val = self
                .builder
                .build_load(ptr_type, registers[src.idx()], "tovirt_src")?
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
                registers[dst.idx()],
                result.try_as_basic_value().basic().unwrap(),
            )?;
        } else {
            // Non-virtual dst: simple pointer copy
            let src_val = self.builder.build_load(
                reg_types[src.idx()],
                registers[src.idx()],
                "tovirt_src",
            )?;
            self.builder
                .build_store(registers[dst.idx()], src_val)?;
        }
        Ok(())
    }
}
