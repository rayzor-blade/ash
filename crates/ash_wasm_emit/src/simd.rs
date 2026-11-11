//! Vector values, as wasm `v128`. A vector value holds 128 bits whatever its
//! lanes are; the instruction that uses it says what it makes of them.
//!
//! The lane semantics are `ash_simd`'s: IEEE 754-2019 minimum and maximum,
//! wrapping integers, shift counts masked to the lane width, reductions in
//! lane order, and saturating conversion with NaN to zero.
use super::*;
use air::v2::ir::{BinOp, MemAccess, VecArg, VecElem, VecIntrinsic, VecOp, VecOut};

fn mem(offset: u64, align: u32) -> we::MemArg {
    we::MemArg {
        offset,
        align,
        memory_index: 0,
    }
}

fn elem_ty(e: VecElem) -> Ty {
    match e {
        VecElem::F32 => Ty::F32,
        VecElem::F64 => Ty::F64,
        VecElem::I64 => Ty::I64,
        VecElem::I32 | VecElem::I16 | VecElem::I8 | VecElem::U8 => Ty::I32,
    }
}

impl<P: Program> Fx<'_, P> {
    /// The v128 scratch local `n` (0 to 3).
    fn vscratch(&self, n: u32) -> u32 {
        self.vector_scratch + n
    }

    fn need_vectors(&self) -> Res<()> {
        if self.has_vectors {
            Ok(())
        } else {
            unsupported(NEEDS_VECTORS)
        }
    }

    /// The lane type of a vector value, from the scalar type it keeps.
    fn lanes_of(&self, v: ValueId) -> Res<VecElem> {
        let data = &self.f.values[v.0 as usize];
        let bytes = match self.program.ty(data.ty) {
            Ty::F32 => (VecElem::F32, 4),
            Ty::F64 => (VecElem::F64, 8),
            Ty::I64 => (VecElem::I64, 8),
            Ty::I32 => (VecElem::I32, 4),
            Ty::U16 => (VecElem::I16, 2),
            Ty::U8 => (VecElem::U8, 1),
            other => return unsupported(format!("vector lanes of {other:?}")),
        };
        if u32::from(data.lanes) * bytes.1 != 16 {
            return unsupported("a vector that is not 128 bits wide");
        }
        Ok(bytes.0)
    }

    /// The address of a vector access's first lane.
    fn vec_address(&mut self, kind: MemAccess, base: ValueId, index: ValueId, lane_bytes: u32, stride: u32) -> Res<()> {
        self.get(base);
        match kind {
            MemAccess::Mem | MemAccess::I8 | MemAccess::I16 => {
                if stride != lane_bytes {
                    return unsupported("a vector access that is not contiguous");
                }
                self.get(index);
                self.emit(&I::I32Add);
            }
            MemAccess::Array => {
                if stride != 1 {
                    return unsupported("a vector access that is not contiguous");
                }
                self.get(index);
                self.emit(&I::I32Const(lane_bytes as i32));
                self.emit(&I::I32Mul);
                self.emit(&I::I32Add);
                self.emit(&I::I32Const(VARRAY_DATA as i32));
                self.emit(&I::I32Add);
            }
        }
        Ok(())
    }

    pub(crate) fn vec_load(&mut self, kind: MemAccess, dst: ValueId, base: ValueId, index: ValueId, stride: u32) -> Res<()> {
        let e = self.lanes_of(dst)?;
        self.vec_address(kind, base, index, e.bytes(), stride)?;
        self.emit(&I::V128Load(mem(0, 0)));
        self.set(dst);
        Ok(())
    }

    pub(crate) fn vec_store(&mut self, kind: MemAccess, base: ValueId, index: ValueId, src: ValueId, stride: u32) -> Res<()> {
        let e = self.lanes_of(src)?;
        self.vec_address(kind, base, index, e.bytes(), stride)?;
        self.get(src);
        self.emit(&I::V128Store(mem(0, 0)));
        Ok(())
    }

    fn splat_op(&mut self, e: VecElem) {
        self.emit(&match e {
            VecElem::F32 => I::F32x4Splat,
            VecElem::F64 => I::F64x2Splat,
            VecElem::I64 => I::I64x2Splat,
            VecElem::I32 => I::I32x4Splat,
            VecElem::I16 => I::I16x8Splat,
            VecElem::I8 | VecElem::U8 => I::I8x16Splat,
        });
    }

    pub(crate) fn vec_splat(&mut self, dst: ValueId, src: ValueId) -> Res<()> {
        let e = self.lanes_of(dst)?;
        self.get(src);
        self.splat_op(e);
        self.set(dst);
        Ok(())
    }

    /// `a op b` on two vectors on the stack.
    fn lane_op(&mut self, op: BinOp, e: VecElem) -> Res<()> {
        use VecElem as E;
        if op == BinOp::Mul && matches!(e, E::I8 | E::U8) {
            // wasm has no byte multiply: widen the low and the high eight
            // lanes, multiply, and keep the low byte of each product.
            let (a, b) = (self.vscratch(0), self.vscratch(1));
            self.emit(&I::LocalSet(b));
            self.emit(&I::LocalSet(a));
            self.emit(&I::LocalGet(a));
            self.emit(&I::LocalGet(b));
            self.emit(&I::I16x8ExtMulLowI8x16U);
            self.emit(&I::LocalGet(a));
            self.emit(&I::LocalGet(b));
            self.emit(&I::I16x8ExtMulHighI8x16U);
            self.emit(&I::I8x16Shuffle([
                0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30,
            ]));
            return Ok(());
        }
        let i = match (op, e) {
            (BinOp::Add, E::F32) => I::F32x4Add,
            (BinOp::Sub, E::F32) => I::F32x4Sub,
            (BinOp::Mul, E::F32) => I::F32x4Mul,
            (BinOp::SDiv, E::F32) => I::F32x4Div,
            (BinOp::Add, E::F64) => I::F64x2Add,
            (BinOp::Sub, E::F64) => I::F64x2Sub,
            (BinOp::Mul, E::F64) => I::F64x2Mul,
            (BinOp::SDiv, E::F64) => I::F64x2Div,
            (BinOp::Add, E::I32) => I::I32x4Add,
            (BinOp::Sub, E::I32) => I::I32x4Sub,
            (BinOp::Mul, E::I32) => I::I32x4Mul,
            (BinOp::Add, E::I16) => I::I16x8Add,
            (BinOp::Sub, E::I16) => I::I16x8Sub,
            (BinOp::Mul, E::I16) => I::I16x8Mul,
            (BinOp::Add, E::I8 | E::U8) => I::I8x16Add,
            (BinOp::Sub, E::I8 | E::U8) => I::I8x16Sub,
            (BinOp::Add, E::I64) => I::I64x2Add,
            (BinOp::Sub, E::I64) => I::I64x2Sub,
            (BinOp::Mul, E::I64) => I::I64x2Mul,
            (BinOp::And, _) => I::V128And,
            (BinOp::Or, _) => I::V128Or,
            (BinOp::Xor, _) => I::V128Xor,
            (op, e) => return unsupported(format!("{op:?} on {e:?} lanes")),
        };
        self.emit(&i);
        Ok(())
    }

    pub(crate) fn vec_binop(&mut self, op: BinOp, dst: ValueId, a: ValueId, b: ValueId) -> Res<()> {
        let e = self.lanes_of(dst)?;
        self.get(a);
        self.get(b);
        self.lane_op(op, e)?;
        self.set(dst);
        Ok(())
    }

    /// The scalar form of a lane operation, on two scalars on the stack.
    fn scalar_op(&mut self, op: BinOp, t: Ty) -> Res<()> {
        let i = match (op, t) {
            (BinOp::Add, Ty::I32 | Ty::U8 | Ty::U16) => I::I32Add,
            (BinOp::Sub, Ty::I32 | Ty::U8 | Ty::U16) => I::I32Sub,
            (BinOp::Mul, Ty::I32 | Ty::U8 | Ty::U16) => I::I32Mul,
            (BinOp::And, Ty::I32 | Ty::U8 | Ty::U16) => I::I32And,
            (BinOp::Or, Ty::I32 | Ty::U8 | Ty::U16) => I::I32Or,
            (BinOp::Xor, Ty::I32 | Ty::U8 | Ty::U16) => I::I32Xor,
            (BinOp::Add, Ty::I64) => I::I64Add,
            (BinOp::Sub, Ty::I64) => I::I64Sub,
            (BinOp::Mul, Ty::I64) => I::I64Mul,
            (BinOp::And, Ty::I64) => I::I64And,
            (BinOp::Or, Ty::I64) => I::I64Or,
            (BinOp::Xor, Ty::I64) => I::I64Xor,
            (BinOp::Add, Ty::F32) => I::F32Add,
            (BinOp::Sub, Ty::F32) => I::F32Sub,
            (BinOp::Mul, Ty::F32) => I::F32Mul,
            (BinOp::SDiv, Ty::F32) => I::F32Div,
            (BinOp::Add, Ty::F64) => I::F64Add,
            (BinOp::Sub, Ty::F64) => I::F64Sub,
            (BinOp::Mul, Ty::F64) => I::F64Mul,
            (BinOp::SDiv, Ty::F64) => I::F64Div,
            (op, t) => return unsupported(format!("a reduction by {op:?} over {t:?}")),
        };
        self.emit(&i);
        Ok(())
    }

    fn extract(&mut self, e: VecElem, lane: u8) {
        self.emit(&match e {
            VecElem::F32 => I::F32x4ExtractLane(lane),
            VecElem::F64 => I::F64x2ExtractLane(lane),
            VecElem::I64 => I::I64x2ExtractLane(lane),
            VecElem::I32 => I::I32x4ExtractLane(lane),
            VecElem::I16 => I::I16x8ExtractLaneS(lane),
            VecElem::I8 => I::I8x16ExtractLaneS(lane),
            VecElem::U8 => I::I8x16ExtractLaneU(lane),
        });
    }

    fn replace(&mut self, e: VecElem, lane: u8) {
        self.emit(&match e {
            VecElem::F32 => I::F32x4ReplaceLane(lane),
            VecElem::F64 => I::F64x2ReplaceLane(lane),
            VecElem::I64 => I::I64x2ReplaceLane(lane),
            VecElem::I32 => I::I32x4ReplaceLane(lane),
            VecElem::I16 => I::I16x8ReplaceLane(lane),
            VecElem::I8 | VecElem::U8 => I::I8x16ReplaceLane(lane),
        });
    }

    /// Fold the lanes of a vector with a scalar operation, in lane order.
    pub(crate) fn vec_reduce(&mut self, op: BinOp, dst: ValueId, src: ValueId) -> Res<()> {
        let e = self.lanes_of(src)?;
        let t = self.program.ty(self.f.values[src.0 as usize].ty);
        let lanes = (16 / e.bytes()) as u8;
        self.get(src);
        self.extract(e, 0);
        for lane in 1..lanes {
            self.get(src);
            self.extract(e, lane);
            self.scalar_op(op, t)?;
        }
        self.set(dst);
        Ok(())
    }

    pub(crate) fn vec_extract(&mut self, elem: VecElem, lane: u8, dst: ValueId, src: ValueId) -> Res<()> {
        self.get(src);
        self.extract(elem, lane);
        self.set(dst);
        Ok(())
    }

    pub(crate) fn vec_insert(&mut self, elem: VecElem, lane: u8, dst: ValueId, src: ValueId, value: ValueId) -> Res<()> {
        self.get(src);
        self.get(value);
        self.replace(elem, lane);
        self.set(dst);
        Ok(())
    }

    /// An operand of a `simd` primitive, as a vector or a scalar on the stack.
    fn vec_arg(&mut self, a: &VecArg, e: VecElem) {
        match *a {
            VecArg::Value(x) | VecArg::Scalar(x) => self.get(x),
            VecArg::Slot { base, off } => {
                self.get(base);
                self.get(off);
                self.emit(&I::I32Add);
                self.emit(&I::V128Load(mem(0, 0)));
            }
            VecArg::Array { arr, index } => {
                self.get(arr);
                self.get(index);
                self.emit(&I::I32Const(e.bytes() as i32));
                self.emit(&I::I32Mul);
                self.emit(&I::I32Add);
                self.emit(&I::I32Const(VARRAY_DATA as i32));
                self.emit(&I::I32Add);
                self.emit(&I::V128Load(mem(0, 0)));
            }
        }
    }

    /// A comparison of two vectors on the stack, as a mask.
    fn compare(&mut self, op: VecOp, e: VecElem) -> Res<()> {
        use VecElem as E;
        use VecOp as O;
        let signed = e.is_signed();
        let i = match (op, e) {
            (O::Eq, E::F32) => I::F32x4Eq,
            (O::Ne, E::F32) => I::F32x4Ne,
            (O::Lt, E::F32) => I::F32x4Lt,
            (O::Le, E::F32) => I::F32x4Le,
            (O::Gt, E::F32) => I::F32x4Gt,
            (O::Ge, E::F32) => I::F32x4Ge,
            (O::Eq, E::F64) => I::F64x2Eq,
            (O::Ne, E::F64) => I::F64x2Ne,
            (O::Lt, E::F64) => I::F64x2Lt,
            (O::Le, E::F64) => I::F64x2Le,
            (O::Gt, E::F64) => I::F64x2Gt,
            (O::Ge, E::F64) => I::F64x2Ge,
            (O::Eq, E::I32) => I::I32x4Eq,
            (O::Ne, E::I32) => I::I32x4Ne,
            (O::Lt, E::I32) if signed => I::I32x4LtS,
            (O::Le, E::I32) if signed => I::I32x4LeS,
            (O::Gt, E::I32) if signed => I::I32x4GtS,
            (O::Ge, E::I32) if signed => I::I32x4GeS,
            (O::Eq, E::I16) => I::I16x8Eq,
            (O::Ne, E::I16) => I::I16x8Ne,
            (O::Lt, E::I16) if signed => I::I16x8LtS,
            (O::Le, E::I16) if signed => I::I16x8LeS,
            (O::Gt, E::I16) if signed => I::I16x8GtS,
            (O::Ge, E::I16) if signed => I::I16x8GeS,
            (O::Eq, E::I8 | E::U8) => I::I8x16Eq,
            (O::Ne, E::I8 | E::U8) => I::I8x16Ne,
            (O::Lt, E::I8) => I::I8x16LtS,
            (O::Le, E::I8) => I::I8x16LeS,
            (O::Gt, E::I8) => I::I8x16GtS,
            (O::Ge, E::I8) => I::I8x16GeS,
            (O::Lt, E::U8) => I::I8x16LtU,
            (O::Le, E::U8) => I::I8x16LeU,
            (O::Gt, E::U8) => I::I8x16GtU,
            (O::Ge, E::U8) => I::I8x16GeU,
            (O::Eq, E::I64) => I::I64x2Eq,
            (O::Ne, E::I64) => I::I64x2Ne,
            (O::Lt, E::I64) => I::I64x2LtS,
            (O::Le, E::I64) => I::I64x2LeS,
            (O::Gt, E::I64) => I::I64x2GtS,
            (O::Ge, E::I64) => I::I64x2GeS,
            (op, e) => return unsupported(format!("{op:?} on {e:?} lanes")),
        };
        self.emit(&i);
        Ok(())
    }

    /// `a < b` or `a > b` choosing between the lanes of two 64-bit vectors,
    /// which wasm has no minimum for.
    fn select_by_order(&mut self, greater: bool) {
        let (a, b) = (self.vscratch(0), self.vscratch(1));
        self.emit(&I::LocalSet(b));
        self.emit(&I::LocalSet(a));
        self.emit(&I::LocalGet(a));
        self.emit(&I::LocalGet(b));
        self.emit(&I::LocalGet(a));
        self.emit(&I::LocalGet(b));
        self.emit(&if greater { I::I64x2GtS } else { I::I64x2LtS });
        self.emit(&I::V128Bitselect);
    }

    /// A `simd` primitive.
    pub(crate) fn vec_op(&mut self, v: VecIntrinsic, dst: ValueId, out: VecOut, args: &[VecArg]) -> Res<()> {
        self.need_vectors()?;
        self.need_scratch()?;
        use VecElem as E;
        use VecOp as O;
        let e = v.elem;
        let float = e.is_float();
        let signed = e.is_signed();
        let lanes = (16 / e.bytes()) as u8;

        // An address for a result that goes to memory is pushed first, below
        // the value.
        match out {
            VecOut::Slot { base, off } => {
                self.get(base);
                self.get(off);
                self.emit(&I::I32Add);
            }
            VecOut::Array { arr, index } => {
                self.get(arr);
                self.get(index);
                self.emit(&I::I32Const(e.bytes() as i32));
                self.emit(&I::I32Mul);
                self.emit(&I::I32Add);
                self.emit(&I::I32Const(VARRAY_DATA as i32));
                self.emit(&I::I32Add);
            }
            VecOut::Value | VecOut::Scalar => {}
        }

        match v.op {
            O::Add | O::Sub | O::Mul | O::Div => {
                self.vec_arg(&args[0], e);
                self.vec_arg(&args[1], e);
                let op = match v.op {
                    O::Add => BinOp::Add,
                    O::Sub => BinOp::Sub,
                    O::Mul => BinOp::Mul,
                    _ => {
                        if !float {
                            return unsupported("an integer vector division");
                        }
                        BinOp::SDiv
                    }
                };
                self.lane_op(op, e)?;
            }
            O::Min | O::Max => {
                self.vec_arg(&args[0], e);
                self.vec_arg(&args[1], e);
                let min = v.op == O::Min;
                let i = match (e, min) {
                    (E::F32, true) => I::F32x4Min,
                    (E::F32, false) => I::F32x4Max,
                    (E::F64, true) => I::F64x2Min,
                    (E::F64, false) => I::F64x2Max,
                    (E::I32, true) if signed => I::I32x4MinS,
                    (E::I32, false) if signed => I::I32x4MaxS,
                    (E::I16, true) if signed => I::I16x8MinS,
                    (E::I16, false) if signed => I::I16x8MaxS,
                    (E::I8, true) => I::I8x16MinS,
                    (E::I8, false) => I::I8x16MaxS,
                    (E::U8, true) => I::I8x16MinU,
                    (E::U8, false) => I::I8x16MaxU,
                    (E::I64, min) => {
                        self.select_by_order(!min);
                        return self.finish_vec(out, dst);
                    }
                    (e, _) => return unsupported(format!("a minimum over {e:?} lanes")),
                };
                self.emit(&i);
            }
            O::Abs | O::Neg | O::Sqrt | O::Not | O::ToI32 | O::ToF32 | O::Copy => {
                self.vec_arg(&args[0], e);
                let i = match (v.op, e) {
                    (O::Abs, E::F32) => I::F32x4Abs,
                    (O::Abs, E::F64) => I::F64x2Abs,
                    (O::Neg, E::F32) => I::F32x4Neg,
                    (O::Neg, E::F64) => I::F64x2Neg,
                    (O::Sqrt, E::F32) => I::F32x4Sqrt,
                    (O::Sqrt, E::F64) => I::F64x2Sqrt,
                    (O::Abs, E::I32) => I::I32x4Abs,
                    (O::Abs, E::I16) => I::I16x8Abs,
                    (O::Abs, E::I8 | E::U8) => I::I8x16Abs,
                    (O::Abs, E::I64) => I::I64x2Abs,
                    (O::Neg, E::I32) => I::I32x4Neg,
                    (O::Neg, E::I16) => I::I16x8Neg,
                    (O::Neg, E::I8 | E::U8) => I::I8x16Neg,
                    (O::Neg, E::I64) => I::I64x2Neg,
                    (O::Not, _) => I::V128Not,
                    (O::ToI32, E::F32) => I::I32x4TruncSatF32x4S,
                    (O::ToF32, E::I32) => I::F32x4ConvertI32x4S,
                    (O::Copy, _) => return self.finish_vec(out, dst),
                    (op, e) => return unsupported(format!("{op:?} on {e:?} lanes")),
                };
                self.emit(&i);
            }
            O::Fma => {
                if !float {
                    return unsupported("a fused multiply-add on integer lanes");
                }
                let (a, b, c, r) = (
                    self.vscratch(0),
                    self.vscratch(1),
                    self.vscratch(2),
                    self.vscratch(3),
                );
                self.vec_arg(&args[0], e);
                self.emit(&I::LocalSet(a));
                self.vec_arg(&args[1], e);
                self.emit(&I::LocalSet(b));
                self.vec_arg(&args[2], e);
                self.emit(&I::LocalSet(c));
                self.emit(&I::LocalGet(a));
                self.emit(&I::LocalSet(r));
                let fma = self.runtime(if e == E::F32 { "fmaf" } else { "fma" })?;
                for lane in 0..lanes {
                    self.emit(&I::LocalGet(r));
                    for x in [a, b, c] {
                        self.emit(&I::LocalGet(x));
                        self.extract(e, lane);
                    }
                    self.call(fma);
                    self.replace(e, lane);
                    self.emit(&I::LocalSet(r));
                }
                self.emit(&I::LocalGet(r));
            }
            O::Splat => {
                self.vec_arg(&args[0], e);
                self.splat_op(e);
            }
            O::Shl | O::Shr => {
                self.vec_arg(&args[0], e);
                self.vec_arg(&args[1], e);
                let i = match (v.op, e) {
                    (O::Shl, E::I32) => I::I32x4Shl,
                    (O::Shl, E::I16) => I::I16x8Shl,
                    (O::Shl, E::I8 | E::U8) => I::I8x16Shl,
                    (O::Shl, E::I64) => I::I64x2Shl,
                    (O::Shr, E::I32) if signed => I::I32x4ShrS,
                    (O::Shr, E::I16) if signed => I::I16x8ShrS,
                    (O::Shr, E::I8) => I::I8x16ShrS,
                    (O::Shr, E::U8) => I::I8x16ShrU,
                    (O::Shr, E::I64) => I::I64x2ShrS,
                    (op, e) => return unsupported(format!("{op:?} on {e:?} lanes")),
                };
                self.emit(&i);
            }
            O::Eq | O::Ne | O::Lt | O::Le | O::Gt | O::Ge => {
                self.vec_arg(&args[0], e);
                self.vec_arg(&args[1], e);
                self.compare(v.op, e)?;
            }
            O::Sum | O::MinLane | O::MaxLane => {
                self.vec_arg(&args[0], e);
                let vec = self.vscratch(0);
                self.emit(&I::LocalSet(vec));
                let t = elem_ty(e);
                if v.op == O::Sum {
                    if float {
                        // Sequential from lane 0, starting at -0.0.
                        if e == E::F32 {
                            self.emit(&I::F32Const((-0.0f32).into()));
                        } else {
                            self.emit(&I::F64Const((-0.0f64).into()));
                        }
                        for lane in 0..lanes {
                            self.emit(&I::LocalGet(vec));
                            self.extract(e, lane);
                            self.emit(&if e == E::F32 { I::F32Add } else { I::F64Add });
                        }
                    } else {
                        self.emit(&I::LocalGet(vec));
                        self.extract(e, 0);
                        for lane in 1..lanes {
                            self.emit(&I::LocalGet(vec));
                            self.extract(e, lane);
                            self.scalar_op(BinOp::Add, t)?;
                        }
                        // A narrow lane wraps in its own width before it widens.
                        match e {
                            E::I16 => self.emit(&I::I32Extend16S),
                            E::I8 => self.emit(&I::I32Extend8S),
                            E::U8 => {
                                self.emit(&I::I32Const(0xff));
                                self.emit(&I::I32And);
                            }
                            _ => {}
                        }
                    }
                } else {
                    let min = v.op == O::MinLane;
                    self.emit(&I::LocalGet(vec));
                    self.extract(e, 0);
                    for lane in 1..lanes {
                        self.emit(&I::LocalGet(vec));
                        self.extract(e, lane);
                        match e {
                            E::F32 => self.emit(&if min { I::F32Min } else { I::F32Max }),
                            E::F64 => self.emit(&if min { I::F64Min } else { I::F64Max }),
                            E::I64 => return unsupported("an integer 64-bit lane reduction by order"),
                            _ => {
                                // acc, x -> the smaller or larger, by select.
                                let (acc, x) = (self.scratch_word(0), self.scratch_word(1));
                                self.emit(&I::LocalSet(x));
                                self.emit(&I::LocalSet(acc));
                                self.emit(&I::LocalGet(acc));
                                self.emit(&I::LocalGet(x));
                                self.emit(&I::LocalGet(acc));
                                self.emit(&I::LocalGet(x));
                                self.emit(&match (min, signed) {
                                    (true, true) => I::I32LtS,
                                    (true, false) => I::I32LtU,
                                    (false, true) => I::I32GtS,
                                    (false, false) => I::I32GtU,
                                });
                                self.emit(&I::Select);
                            }
                        }
                    }
                }
                if self.ty(dst) != t {
                    return unsupported("a reduction into a value of another kind");
                }
                self.set(dst);
                return Ok(());
            }
            O::LoadArray | O::StoreArray => {
                self.vec_arg(&args[0], e);
            }
            O::And | O::Or | O::Xor => {
                self.vec_arg(&args[0], e);
                self.vec_arg(&args[1], e);
                self.emit(&match v.op {
                    O::And => I::V128And,
                    O::Or => I::V128Or,
                    _ => I::V128Xor,
                });
            }
            O::Select => {
                // (m & a) | (~m & b)
                self.vec_arg(&args[1], e);
                self.vec_arg(&args[2], e);
                self.vec_arg(&args[0], e);
                self.emit(&I::V128Bitselect);
            }
        }
        self.finish_vec(out, dst)
    }

    /// The result of a vector operation is on the stack, above the address it
    /// goes to if it goes to memory.
    fn finish_vec(&mut self, out: VecOut, dst: ValueId) -> Res<()> {
        match out {
            VecOut::Value => {
                self.set(dst);
                Ok(())
            }
            VecOut::Slot { .. } | VecOut::Array { .. } => {
                self.emit(&I::V128Store(mem(0, 0)));
                Ok(())
            }
            VecOut::Scalar => unsupported("a primitive that does not return a scalar"),
        }
    }
}
