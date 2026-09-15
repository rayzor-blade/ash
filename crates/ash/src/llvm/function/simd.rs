//! The `simd` primitives as vector IR in place of their calls.
//!
//! Each primitive takes its operands from slots, array runs or vector
//! values, applies the lane operation and stores or defines the result. The
//! semantics are `ash_simd`'s lane for lane -- IEEE 754-2019
//! minimum/maximum, wrapping integers, masked shift counts, in-order
//! reductions, saturating conversion with NaN to zero -- and the parity
//! fixture holds the two together. A vector value is `<4 x i32>` and is
//! bitcast to the operation's lane type on the way in and out; slot loads
//! and stores stay, and LLVM forwards a store to a later load of the same
//! slot within the function.

use air::v2::ir::{ValueId, VecArg, VecElem, VecIntrinsic, VecOp, VecOut};
use anyhow::{anyhow, Result};
use inkwell::intrinsics::Intrinsic;
use inkwell::types::{BasicType, BasicTypeEnum, VectorType};
use inkwell::values::{BasicValue, BasicValueEnum, IntValue, PointerValue, VectorValue};
use inkwell::{FloatPredicate, IntPredicate};

use crate::llvm::module::JITModule;

impl<'ctx> JITModule<'ctx> {
    pub(super) fn emit_air_vec_op(
        &mut self,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        v: VecIntrinsic,
        dst: ValueId,
        out: VecOut,
        args: &[VecArg],
    ) -> Result<()> {
        let (_, shapes) = v.op.layout();
        if args.len() != shapes.len() {
            return Err(anyhow!(
                "simd {:?} takes {} operands, got {}",
                v.op,
                shapes.len(),
                args.len()
            ));
        }
        let ctx = self.context;
        let lanes = v.elem.lanes();
        let ty: VectorType<'ctx> = match v.elem {
            VecElem::F32 => ctx.f32_type().vec_type(lanes),
            VecElem::F64 => ctx.f64_type().vec_type(lanes),
            VecElem::I32 => ctx.i32_type().vec_type(lanes),
            VecElem::I16 => ctx.i16_type().vec_type(lanes),
            VecElem::I8 | VecElem::U8 => ctx.i8_type().vec_type(lanes),
            VecElem::I64 => ctx.i64_type().vec_type(lanes),
        };
        let float = v.elem.is_float();
        let signed = v.elem.is_signed();
        let vty: BasicTypeEnum<'ctx> = ty.into();
        let value_ty = ctx.i32_type().vec_type(4);

        let reg = |this: &Self, a: ValueId| -> Result<BasicValueEnum<'ctx>> {
            Ok(this
                .builder
                .build_load(reg_types[a.idx()], registers[a.idx()], "simd_arg")?)
        };
        // `bytes + offset`, the offset sign-extended as HL's `Int` is.
        let slot = |this: &Self, base: ValueId, off: ValueId| -> Result<PointerValue<'ctx>> {
            let b = reg(this, base)?.into_pointer_value();
            let o = reg(this, off)?.into_int_value();
            let o = this
                .builder
                .build_int_s_extend(o, ctx.i64_type(), "simd_off")?;
            Ok(unsafe {
                this.builder
                    .build_in_bounds_gep(ctx.i8_type(), b, &[o], "simd_slot")?
            })
        };
        let run = |this: &Self, arr: ValueId, index: ValueId| -> Result<PointerValue<'ctx>> {
            this.array_run(reg(this, arr)?, reg(this, index)?, v.elem)
        };
        // Slots are wherever the program put them.
        let load_at = |this: &Self, p: PointerValue<'ctx>| -> Result<VectorValue<'ctx>> {
            let l = this.builder.build_load(ty, p, "simd_load")?;
            if let Some(i) = l.as_instruction_value() {
                i.set_alignment(1)?;
            }
            Ok(l.into_vector_value())
        };
        let store_at = |this: &Self, p: PointerValue<'ctx>, r: VectorValue<'ctx>| -> Result<()> {
            let s = this.builder.build_store(p, r)?;
            s.set_alignment(1)?;
            Ok(())
        };
        let operand = |this: &Self, a: VecArg| -> Result<BasicValueEnum<'ctx>> {
            Ok(match a {
                VecArg::Value(x) => {
                    this.builder
                        .build_bit_cast(reg(this, x)?, vty, "simd_lanes")?
                }
                VecArg::Slot { base, off } => load_at(this, slot(this, base, off)?)?.into(),
                VecArg::Array { arr, index } => load_at(this, run(this, arr, index)?)?.into(),
                VecArg::Scalar(x) => reg(this, x)?,
            })
        };
        let vec = |this: &Self, a: VecArg| -> Result<VectorValue<'ctx>> {
            Ok(operand(this, a)?.into_vector_value())
        };
        let intrinsic = |this: &Self,
                         name: &str,
                         tys: &[BasicTypeEnum<'ctx>],
                         vals: &[BasicValueEnum<'ctx>]|
         -> Result<BasicValueEnum<'ctx>> {
            let intr =
                Intrinsic::find(name).ok_or_else(|| anyhow!("LLVM intrinsic {name} not found"))?;
            let decl = intr
                .get_declaration(&this.module, tys)
                .ok_or_else(|| anyhow!("no declaration for {name}"))?;
            let a: Vec<_> = vals.iter().map(|x| (*x).into()).collect();
            let call = this.builder.build_call(decl, &a, name)?;
            call.try_as_basic_value()
                .basic()
                .ok_or_else(|| anyhow!("{name} returned void"))
        };

        let r: VectorValue<'ctx> = match v.op {
            VecOp::Add | VecOp::Sub | VecOp::Mul | VecOp::Div | VecOp::Min | VecOp::Max => {
                let a = vec(self, args[0])?;
                let b = vec(self, args[1])?;
                match (v.op, float) {
                    (VecOp::Add, true) => self.builder.build_float_add(a, b, "vadd")?,
                    (VecOp::Sub, true) => self.builder.build_float_sub(a, b, "vsub")?,
                    (VecOp::Mul, true) => self.builder.build_float_mul(a, b, "vmul")?,
                    (VecOp::Div, true) => self.builder.build_float_div(a, b, "vdiv")?,
                    (VecOp::Min, true) => {
                        intrinsic(self, "llvm.minimum", &[vty], &[a.into(), b.into()])?
                            .into_vector_value()
                    }
                    (VecOp::Max, true) => {
                        intrinsic(self, "llvm.maximum", &[vty], &[a.into(), b.into()])?
                            .into_vector_value()
                    }
                    (VecOp::Add, false) => self.builder.build_int_add(a, b, "vadd")?,
                    (VecOp::Sub, false) => self.builder.build_int_sub(a, b, "vsub")?,
                    (VecOp::Mul, false) => self.builder.build_int_mul(a, b, "vmul")?,
                    (VecOp::Min, false) => {
                        let name = if signed { "llvm.smin" } else { "llvm.umin" };
                        intrinsic(self, name, &[vty], &[a.into(), b.into()])?.into_vector_value()
                    }
                    (VecOp::Max, false) => {
                        let name = if signed { "llvm.smax" } else { "llvm.umax" };
                        intrinsic(self, name, &[vty], &[a.into(), b.into()])?.into_vector_value()
                    }
                    (VecOp::Div, false) => return Err(anyhow!("integer vector division")),
                    _ => unreachable!(),
                }
            }
            VecOp::Abs
            | VecOp::Neg
            | VecOp::Sqrt
            | VecOp::Not
            | VecOp::ToI32
            | VecOp::ToF32
            | VecOp::Copy => {
                let a = vec(self, args[0])?;
                match (v.op, float) {
                    (VecOp::Abs, true) => {
                        intrinsic(self, "llvm.fabs", &[vty], &[a.into()])?.into_vector_value()
                    }
                    (VecOp::Neg, true) => self.builder.build_float_neg(a, "vneg")?,
                    (VecOp::Sqrt, true) => {
                        intrinsic(self, "llvm.sqrt", &[vty], &[a.into()])?.into_vector_value()
                    }
                    (VecOp::Abs, false) => {
                        // Wrapping: `abs(MIN)` stays `MIN`, so no poison flag.
                        let poison = ctx.bool_type().const_zero();
                        intrinsic(self, "llvm.abs", &[vty], &[a.into(), poison.into()])?
                            .into_vector_value()
                    }
                    (VecOp::Neg, false) => self.builder.build_int_neg(a, "vneg")?,
                    (VecOp::Not, _) => self.builder.build_not(a, "vnot")?,
                    (VecOp::Copy, _) => a,
                    (VecOp::ToI32, _) => {
                        // Saturating, NaN to zero: the definition of the native.
                        let ity: BasicTypeEnum<'ctx> = ctx.i32_type().vec_type(4).into();
                        intrinsic(self, "llvm.fptosi.sat", &[ity, vty], &[a.into()])?
                            .into_vector_value()
                    }
                    (VecOp::ToF32, _) => self.builder.build_signed_int_to_float(
                        a,
                        ctx.f32_type().vec_type(4),
                        "vtof",
                    )?,
                    _ => return Err(anyhow!("simd {:?} on {:?} lanes", v.op, v.elem)),
                }
            }
            VecOp::Fma => {
                let a = vec(self, args[0])?;
                let b = vec(self, args[1])?;
                let c = vec(self, args[2])?;
                intrinsic(self, "llvm.fma", &[vty], &[a.into(), b.into(), c.into()])?
                    .into_vector_value()
            }
            VecOp::Splat => {
                let x = operand(self, args[0])?;
                // The scalar arrives as an i32; a narrow lane keeps its low
                // bits, as the native's `as` does.
                let x: BasicValueEnum<'ctx> = match v.elem {
                    VecElem::I16 => self
                        .builder
                        .build_int_truncate(x.into_int_value(), ctx.i16_type(), "splat_i16")?
                        .into(),
                    VecElem::I8 | VecElem::U8 => self
                        .builder
                        .build_int_truncate(x.into_int_value(), ctx.i8_type(), "splat_i8")?
                        .into(),
                    _ => x,
                };
                self.splat(ty, x)?
            }
            VecOp::Shl | VecOp::Shr => {
                let a = vec(self, args[0])?;
                let n = operand(self, args[1])?.into_int_value();
                let bits = v.elem.bytes() * 8;
                let n = self.builder.build_and(
                    n,
                    ctx.i32_type().const_int((bits - 1) as u64, false),
                    "shift_count",
                )?;
                let lane_int = ty.get_element_type().into_int_type();
                let n: IntValue<'ctx> = if lane_int.get_bit_width() < 32 {
                    self.builder.build_int_truncate(n, lane_int, "shift_lane")?
                } else if lane_int.get_bit_width() > 32 {
                    self.builder.build_int_z_extend(n, lane_int, "shift_lane")?
                } else {
                    n
                };
                let n = self.splat(ty, n.into())?;
                match (v.op, signed) {
                    (VecOp::Shl, _) => self.builder.build_left_shift(a, n, "vshl")?,
                    (VecOp::Shr, s) => self.builder.build_right_shift(a, n, s, "vshr")?,
                    _ => unreachable!(),
                }
            }
            VecOp::Eq | VecOp::Ne | VecOp::Lt | VecOp::Le | VecOp::Gt | VecOp::Ge => {
                let a = vec(self, args[0])?;
                let b = vec(self, args[1])?;
                let mask = if float {
                    let p = match v.op {
                        VecOp::Eq => FloatPredicate::OEQ,
                        VecOp::Ne => FloatPredicate::UNE,
                        VecOp::Lt => FloatPredicate::OLT,
                        VecOp::Le => FloatPredicate::OLE,
                        VecOp::Gt => FloatPredicate::OGT,
                        VecOp::Ge => FloatPredicate::OGE,
                        _ => unreachable!(),
                    };
                    self.builder.build_float_compare(p, a, b, "vcmp")?
                } else {
                    let p = match (v.op, signed) {
                        (VecOp::Eq, _) => IntPredicate::EQ,
                        (VecOp::Ne, _) => IntPredicate::NE,
                        (VecOp::Lt, true) => IntPredicate::SLT,
                        (VecOp::Le, true) => IntPredicate::SLE,
                        (VecOp::Gt, true) => IntPredicate::SGT,
                        (VecOp::Ge, true) => IntPredicate::SGE,
                        (VecOp::Lt, false) => IntPredicate::ULT,
                        (VecOp::Le, false) => IntPredicate::ULE,
                        (VecOp::Gt, false) => IntPredicate::UGT,
                        (VecOp::Ge, false) => IntPredicate::UGE,
                        _ => unreachable!(),
                    };
                    self.builder.build_int_compare(p, a, b, "vcmp")?
                };
                // All ones where it holds, in the lane width.
                let lane_bits = ctx.custom_width_int_type(v.elem.bytes() * 8);
                self.builder
                    .build_int_s_extend(mask, lane_bits.vec_type(lanes), "vmask")?
            }
            VecOp::Sum | VecOp::MinLane | VecOp::MaxLane => {
                let a = vec(self, args[0])?;
                let r: BasicValueEnum<'ctx> = match (v.op, float, signed) {
                    // Sequential from lane 0: `-0.0 + lane0` is lane0 for every
                    // lane0, and no reassociation flag is set.
                    (VecOp::Sum, true, _) => {
                        let start = ty.get_element_type().into_float_type().const_float(-0.0);
                        intrinsic(
                            self,
                            "llvm.vector.reduce.fadd",
                            &[vty],
                            &[start.into(), a.into()],
                        )?
                    }
                    (VecOp::MinLane, true, _) => {
                        intrinsic(self, "llvm.vector.reduce.fminimum", &[vty], &[a.into()])?
                    }
                    (VecOp::MaxLane, true, _) => {
                        intrinsic(self, "llvm.vector.reduce.fmaximum", &[vty], &[a.into()])?
                    }
                    (VecOp::Sum, false, _) => {
                        intrinsic(self, "llvm.vector.reduce.add", &[vty], &[a.into()])?
                    }
                    (VecOp::MinLane, false, true) => {
                        intrinsic(self, "llvm.vector.reduce.smin", &[vty], &[a.into()])?
                    }
                    (VecOp::MaxLane, false, true) => {
                        intrinsic(self, "llvm.vector.reduce.smax", &[vty], &[a.into()])?
                    }
                    (VecOp::MinLane, false, false) => {
                        intrinsic(self, "llvm.vector.reduce.umin", &[vty], &[a.into()])?
                    }
                    (VecOp::MaxLane, false, false) => {
                        intrinsic(self, "llvm.vector.reduce.umax", &[vty], &[a.into()])?
                    }
                    _ => unreachable!(),
                };
                // A narrow integer lane wraps in its own width before it
                // widens to `Int`.
                let r: BasicValueEnum<'ctx> = match v.elem {
                    VecElem::I16 | VecElem::I8 => self
                        .builder
                        .build_int_s_extend(r.into_int_value(), ctx.i32_type(), "reduce_i32")?
                        .into(),
                    VecElem::U8 => self
                        .builder
                        .build_int_z_extend(r.into_int_value(), ctx.i32_type(), "reduce_i32")?
                        .into(),
                    _ => r,
                };
                self.builder.build_store(registers[dst.idx()], r)?;
                return Ok(());
            }
            VecOp::LoadArray | VecOp::StoreArray => vec(self, args[0])?,
            VecOp::And | VecOp::Or | VecOp::Xor => {
                let a = vec(self, args[0])?;
                let b = vec(self, args[1])?;
                match v.op {
                    VecOp::And => self.builder.build_and(a, b, "vand")?,
                    VecOp::Or => self.builder.build_or(a, b, "vor")?,
                    VecOp::Xor => self.builder.build_xor(a, b, "vxor")?,
                    _ => unreachable!(),
                }
            }
            VecOp::Select => {
                let m = vec(self, args[0])?;
                let a = vec(self, args[1])?;
                let b = vec(self, args[2])?;
                let keep = self.builder.build_and(m, a, "sel_a")?;
                let nm = self.builder.build_not(m, "sel_nm")?;
                let drop = self.builder.build_and(nm, b, "sel_b")?;
                self.builder.build_or(keep, drop, "vselect")?
            }
        };
        match out {
            VecOut::Value => {
                let r = self.builder.build_bit_cast(r, value_ty, "simd_value")?;
                self.builder.build_store(registers[dst.idx()], r)?;
                Ok(())
            }
            VecOut::Scalar => Err(anyhow!("simd {:?} does not return a scalar", v.op)),
            VecOut::Slot { base, off } => store_at(self, slot(self, base, off)?, r),
            VecOut::Array { arr, index } => store_at(self, run(self, arr, index)?, r),
        }
    }

    /// `x` in every lane of `ty`.
    fn splat(&self, ty: VectorType<'ctx>, x: BasicValueEnum<'ctx>) -> Result<VectorValue<'ctx>> {
        let zero = self.context.i32_type().const_zero();
        let one = self
            .builder
            .build_insert_element(ty.get_undef(), x, zero, "splat_lane")?;
        let mask = VectorType::const_vector(&vec![zero; ty.get_size() as usize]);
        Ok(self
            .builder
            .build_shuffle_vector(one, ty.get_undef(), mask, "splat")?)
    }

    /// The address of element `index` of a `NativeArray`.
    fn array_run(
        &self,
        arr: BasicValueEnum<'ctx>,
        index: BasicValueEnum<'ctx>,
        elem: VecElem,
    ) -> Result<PointerValue<'ctx>> {
        let ctx = self.context;
        let index =
            self.builder
                .build_int_s_extend(index.into_int_value(), ctx.i64_type(), "arr_index")?;
        let byte = self.builder.build_int_mul(
            index,
            ctx.i64_type().const_int(elem.bytes() as u64, false),
            "arr_byte",
        )?;
        let off = self.builder.build_int_add(
            byte,
            ctx.i64_type()
                .const_int(crate::layout::VARRAY_DATA_OFFSET as u64, false),
            "arr_off",
        )?;
        Ok(unsafe {
            self.builder.build_in_bounds_gep(
                ctx.i8_type(),
                arr.into_pointer_value(),
                &[off],
                "arr_run",
            )?
        })
    }
}
