//! Executing a function from its prepared SSA IR.
//!
//! The flat interpreter and this dispatcher share their opcode semantics
//! through the `op_*` handlers in [`super::ops`]; what differs is how operands
//! are addressed. A child module of `interpreter` so it reaches
//! `HLInterpreter`'s private fields without widening them.
//!
//! Not to be confused with [`crate::ssa`], which prepares and caches the IR
//! this runs.

use anyhow::{anyhow, Result};
use std::ffi::c_void;

use ash_core::bytecode::DecodedBytecode;
use ash_core::opcodes::Reg;
use ash_core::types::HLFunction;

use crate::values::NanBoxedValue;

use ash_core::hl_bindings as hl;
use ash_core::native_lib::NativeFunctionResolver;

use crate::tiering::env_flag;
use crate::values::{CmpOp, FloatBinOp, IntBinOp};

use super::{HLExceptionPropagation, HLInterpreter, StepResult, POOL_CAP};

impl HLInterpreter {
    /// Block-at-a-time dispatch over the SSA CFG.
    pub(super) fn ssa_loop(
        &mut self,
        bc: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        func_idx: usize,
        prep: &'static crate::ssa::Prepared,
        args: &[NanBoxedValue],
    ) -> Result<NanBoxedValue> {
        let ir = prep.ir;
        let func = prep.shim;
        let mut block = 0usize;
        // Which edge control arrived on. Phi sources are keyed by it, and the
        // exceptional edge into a handler sets it too, so a handler that does
        // carry phis resolves them against the block that threw.
        let mut prev_block: Option<u32> = None;
        let mut phi_buf: Vec<(u32, NanBoxedValue)> = Vec::new();
        let mut lane_buf: Vec<(u32, Vec<NanBoxedValue>)> = Vec::new();

        'blocks: loop {
            // The tiering ladder is driven by back edges, and it was the
            // opcode loop alone that counted them: under this walker a hot
            // loop raised no demand and never asked for a compile, so a
            // hybrid run stayed interpreted no matter how hot it got. A
            // branch to a block at or before this one is the same signal the
            // opcode loop reads from a negative jump offset.
            if let Some(prev) = prev_block {
                if block <= prev as usize {
                    let frame = self.stack.last_mut().unwrap();
                    frame.backedges = frame.backedges.wrapping_add(1);
                    // Every 64th, for the reason the opcode loop gives: there
                    // are two thresholds to cross, so one signal would stall
                    // the ladder at Cranelift.
                    let hot = frame.backedges & (super::HOT_LOOP_BACKEDGES - 1) == 0;
                    // Named by the header's bytecode pc, which is what the
                    // tiering map and `compile_osr_entry` both key on -- the
                    // block index would look up a different block, or none.
                    if hot {
                        if let Some(&header_pc) = prep.block_pcs.get(block) {
                            self.note_hot_loop(bc, func_idx, header_pc);
                            // Promotion swaps a pointer, which only the next
                            // call observes; a loop entered once would keep
                            // interpreting past its own compile without this.
                            if let Some(ret) =
                                self.try_osr_transfer(bc, func_idx, header_pc, Some((prep, block, prev_block)))?
                            {
                                return Ok(ret);
                            }
                        }
                    }
                }
            }
            let blk = ir
                .blocks
                .get(block)
                .ok_or_else(|| anyhow!("SSA block {} out of range in {}", block, func.name()))?;
            let work = (blk.instrs.len() + 1).min(u32::MAX as usize) as u32;
            self.fiber_safe_point(work);
            // Polled per block rather than per call: a Haxe loop whose body
            // AIR inlined makes no calls at all, so a function-entry poll
            // never sees it.
            self.report_stall_if_asked(bc);
            // Published for the same reason the opcode loop publishes `pc`:
            // it is the only record of where a frame is when something below
            // it fails.
            // The block's bytecode pc, not its index: everything that reads
            // this reads it as a pc -- a trace resolves it against the debug
            // table, which numbered lines by opcode. Publishing the index put
            // a frame on whatever line happened to share its number.
            self.stack.last_mut().unwrap().pc =
                prep.block_pcs.get(block).copied().unwrap_or(block);

            // A phi group is a parallel copy. Read every source before writing
            // any destination, or `x, y = y, x` collapses into `x, y = y, y`.
            if !blk.phis.is_empty() {
                phi_buf.clear();
                lane_buf.clear();
                let frame = self.stack.last().unwrap();
                for phi in &blk.phis {
                    if let Some(pb) = prev_block {
                        if let Some(&(_, v)) = phi.incoming.iter().find(|(b, _)| b.0 == pb) {
                            // A vectorized accumulator is carried as a phi
                            // like any other, and its value lives in the lane
                            // side-table rather than in a register. Copying
                            // only the register left the destination with no
                            // lanes at all, which is how the widened
                            // reduction first arrived here.
                            match frame.vec_lanes.get(&v.0) {
                                Some(lanes) => lane_buf.push((phi.dst.0, lanes.clone())),
                                None => phi_buf.push((phi.dst.0, frame.registers.get(v.0))),
                            }
                        }
                    }
                }
                let frame = self.stack.last_mut().unwrap();
                for (dst, v) in phi_buf.drain(..) {
                    frame.registers.set(dst, v);
                }
                for (dst, lanes) in lane_buf.drain(..) {
                    frame.vec_lanes.insert(dst, lanes);
                }
            }

            let pcs = prep.instr_pcs.get(block);
            for (i, ins) in blk.instrs.iter().enumerate() {
                // The raising instruction's own pc, not its block's: a trace
                // resolves this against the debug table, and every
                // instruction in a block shares the block's line otherwise.
                if let Some(&pc) = pcs.and_then(|v| v.get(i)) {
                    self.stack.last_mut().unwrap().pc = pc;
                }
                let next = match self.ssa_step(bc, native_resolver, func_idx, prep, args, ins) {
                    Ok(next) => next,
                    Err(err) => {
                        let exc = err
                            .downcast_ref::<HLExceptionPropagation>()
                            .map(|exception| exception.value);
                        if let Some(exc) = exc {
                            if matches!(ins, air::v2::Instr::NullCheck { .. }) {
                                self.capture_exception_stack(bc);
                            }
                            let frame = self.stack.last_mut().unwrap();
                            if let Some((handler, cell_slot)) = frame.trap_stack.pop() {
                                frame.registers.set(cell_slot, exc);
                                Some(handler)
                            } else {
                                return Err(err);
                            }
                        } else {
                            return Err(err);
                        }
                    }
                };
                if let Some(handler) = next {
                    // A call or opcode raised and this frame's innermost trap
                    // caught it.
                    prev_block = Some(block as u32);
                    block = handler;
                    continue 'blocks;
                }
            }

            let get = |s: &Self, v: air::v2::ValueId| s.stack.last().unwrap().registers.get(v.0);
            match &blk.term {
                air::v2::Terminator::Ret { value } => return Ok(get(self, *value)),
                air::v2::Terminator::Jump { target } => {
                    prev_block = Some(block as u32);
                    block = target.idx();
                }
                air::v2::Terminator::CondJump {
                    cond,
                    a,
                    b,
                    if_true,
                    if_false,
                } => {
                    let taken = self.ssa_cond(bc, func, func_idx, *cond, *a, *b);
                    prev_block = Some(block as u32);
                    block = if taken { if_true.idx() } else { if_false.idx() };
                }
                air::v2::Terminator::Switch {
                    value,
                    targets,
                    default,
                } => {
                    let idx = get(self, *value).as_i32();
                    prev_block = Some(block as u32);
                    block = if idx >= 0 && (idx as usize) < targets.len() {
                        targets[idx as usize].idx()
                    } else {
                        default.idx()
                    };
                }
                air::v2::Terminator::Throw { exc } => {
                    self.capture_exception_stack(bc);
                    let val = get(self, *exc);
                    let frame = self.stack.last_mut().unwrap();
                    match frame.trap_stack.pop() {
                        Some((handler, cell_slot)) => {
                            frame.registers.set(cell_slot, val);
                            prev_block = Some(block as u32);
                            block = handler;
                        }
                        None => {
                            // Carried the same way the opcode loop carries it:
                            // an uncaught exception reports the frames it came
                            // from, and those are gone by the time it reaches
                            // the CLI.
                            let mut exc = self.format_hl_exception(val);
                            exc.stack = self.capture_call_stack(bc);
                            return Err(anyhow::Error::new(exc));
                        }
                    }
                }
                air::v2::Terminator::Rethrow { exc } => {
                    let val = get(self, *exc);
                    let frame = self.stack.last_mut().unwrap();
                    match frame.trap_stack.pop() {
                        Some((handler, cell_slot)) => {
                            frame.registers.set(cell_slot, val);
                            prev_block = Some(block as u32);
                            block = handler;
                        }
                        None => {
                            // Carried the same way the opcode loop carries it:
                            // an uncaught exception reports the frames it came
                            // from, and those are gone by the time it reaches
                            // the CLI.
                            let mut exc = self.format_hl_exception(val);
                            exc.stack = self.capture_call_stack(bc);
                            return Err(anyhow::Error::new(exc));
                        }
                    }
                }
                air::v2::Terminator::Trap {
                    exc_cell,
                    handler,
                    normal,
                } => {
                    let slot = prep.cell_base + exc_cell.0;
                    self.stack
                        .last_mut()
                        .unwrap()
                        .trap_stack
                        .push((handler.idx(), slot));
                    prev_block = Some(block as u32);
                    block = normal.idx();
                }
            }
        }
    }

    /// Evaluate a `CondJump` condition.
    pub(super) fn ssa_cond(
        &self,
        bc: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        cond: air::v2::CondKind,
        a: air::v2::ValueId,
        b: Option<air::v2::ValueId>,
    ) -> bool {
        use air::v2::CondKind as C;
        let va = self.stack.last().unwrap().registers.get(a.0);
        let cmp = |op: CmpOp| {
            let b = b.expect("binary condition without a second operand");
            self.compare_regs_in(bc, func, func_idx, a.0, b.0, op)
        };
        match cond {
            C::True => va.to_bool(),
            C::False => !va.to_bool(),
            C::Null => va.is_null(),
            C::NotNull => !va.is_null(),
            C::SLt => cmp(CmpOp::SLt),
            C::SGte => cmp(CmpOp::SGte),
            C::SGt => cmp(CmpOp::SGt),
            C::SLte => cmp(CmpOp::SLte),
            C::ULt => cmp(CmpOp::ULt),
            C::UGte => cmp(CmpOp::UGte),
            // The NEGATION of the comparison, not its opposite: every
            // comparison against NaN is false, so !(a < b) is true for NaN
            // where (a >= b) is false. Integers are a total order and the two
            // agree there, which is why reading them as opposites passed
            // everything except TestNaN. The opcode loop spells out the same
            // reasoning over JNotLt/JNotGte.
            C::NotLt => !cmp(CmpOp::SLt),
            C::NotGte => !cmp(CmpOp::SGte),
            C::Eq => cmp(CmpOp::Eq),
            C::NotEq => cmp(CmpOp::NotEq),
        }
    }

    /// Execute one SSA instruction.
    ///
    /// `Ok(None)` continues in the same block. `Ok(Some(b))` means a call threw
    /// and this frame's innermost trap caught it, so control resumes at `b`.
    /// `Err` propagates, which is what the reference does for everything a
    /// non-call instruction raises — including `NullCheck`, whose exception
    /// escapes its own frame's traps there too.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn ssa_step(
        &mut self,
        bc: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        func_idx: usize,
        prep: &'static crate::ssa::Prepared,
        args: &[NanBoxedValue],
        ins: &air::v2::Instr,
    ) -> Result<Option<usize>> {
        use air::v2::Instr as I;
        let func = prep.shim;
        let cell_base = prep.cell_base;

        macro_rules! get {
            ($v:expr) => {
                self.stack.last().unwrap().registers.get($v.0)
            };
        }
        macro_rules! set {
            ($v:expr, $val:expr) => {{
                let val = $val;
                self.stack.last_mut().unwrap().registers.set($v.0, val)
            }};
        }
        /// Static HL kind of a value, via the shim's per-value type table.
        macro_rules! kind {
            ($v:expr) => {
                bc.types[func.regs[$v.0 as usize].0].kind
            };
        }

        match ins {
            // ---- vectors, one lane at a time ---------------------------
            // The interpreter is scalar, so a widened value lives in the
            // frame's lane map and every vector form is a small loop over it.
            // Slower than the scalar loop the widening replaced -- and that is
            // the point: the same AIR has to RUN on every tier, or a widened
            // function could not be deoptimized back to the interpreter.
            I::VecLoad {
                kind,
                dst,
                base,
                index,
                stride,
            } => {
                let lanes = prep.ir.values[dst.0 as usize].lanes as usize;
                let idx0 = get!(index).as_i32();
                let mut out = Vec::with_capacity(lanes);
                for k in 0..lanes {
                    let at = idx0 + (k as i32) * (*stride as i32);
                    out.push(self.vec_lane_get(
                        bc, func, func_idx, *kind, dst.0, base.0, index.0, at,
                    )?);
                }
                self.stack.last_mut().unwrap().vec_lanes.insert(dst.0, out);
            }
            I::VecStore {
                kind,
                base,
                index,
                src,
                stride,
            } => {
                let idx0 = get!(index).as_i32();
                let lanes = self.lanes_of(src.0)?;
                for (k, v) in lanes.iter().enumerate() {
                    let at = idx0 + (k as i32) * (*stride as i32);
                    self.vec_lane_set(
                        bc, func, func_idx, *kind, src.0, base.0, index.0, at, *v,
                    )?;
                }
            }
            I::VecSplat { dst, src } => {
                let n = prep.ir.values[dst.0 as usize].lanes as usize;
                let v = get!(src);
                self.stack
                    .last_mut()
                    .unwrap()
                    .vec_lanes
                    .insert(dst.0, vec![v; n]);
            }
            I::VecBinOp { op, dst, a, b } => {
                let la = self.lanes_of(a.0)?;
                let lb = self.lanes_of(b.0)?;
                if la.len() != lb.len() {
                    return Err(anyhow::anyhow!("VecBinOp lane count mismatch"));
                }
                let mut out = Vec::with_capacity(la.len());
                for i in 0..la.len() {
                    out.push(Self::scalar_binop(*op, la[i], lb[i])?);
                }
                self.stack.last_mut().unwrap().vec_lanes.insert(dst.0, out);
            }
            I::VecReduce { op, dst, src } => {
                let lanes = self.lanes_of(src.0)?;
                let mut acc = *lanes
                    .first()
                    .ok_or_else(|| anyhow::anyhow!("VecReduce over an empty vector"))?;
                for v in &lanes[1..] {
                    acc = Self::scalar_binop(*op, acc, *v)?;
                }
                set!(dst, acc);
            }
            // ---- values -----------------------------------------------
            I::Param { dst, reg } => {
                // Registers past the argument list are the HL default, which is
                // what a fresh frame slot already holds.
                let v = args
                    .get(*reg as usize)
                    .copied()
                    .unwrap_or_else(NanBoxedValue::void);
                set!(dst, v);
            }
            I::Copy { dst, src } => {
                let v = get!(src);
                set!(dst, v);
            }
            // Through `int_at`, not `bc.ints` directly: a pass may MINT a
            // constant the module's pool does not hold -- the widener's
            // identity and its `& ~(VF-1)` mask both do -- and those live in
            // the function's own list, indexed after the pool. Indexing the
            // pool with one of those numbers is an out-of-bounds panic, which
            // is what this did the first time a widened loop reached here.
            I::Int { dst, idx } => {
                let v = prep
                    .ir
                    .int_at(*idx, |i| bc.ints.get(i).copied())
                    .ok_or_else(|| anyhow!("int constant {idx} is not in the pool"))?;
                set!(dst, NanBoxedValue::from_i32(v))
            }
            I::Float { dst, idx } => set!(dst, NanBoxedValue::from_f64(bc.floats[*idx])),
            I::Bool { dst, value } => set!(dst, NanBoxedValue::from_bool(*value)),
            I::Bytes { dst, idx } => {
                let pos = bc.bytes_pos[*idx];
                let bytes_ptr = bc
                    .bytes_data
                    .get(pos..)
                    .ok_or_else(|| anyhow!("Bytes constant out of bounds: {idx}"))?
                    .as_ptr();
                set!(dst, NanBoxedValue::from_bytes_ptr(bytes_ptr as usize));
            }
            I::String { dst, idx } => {
                // HashLink strings are UTF-16 internally; the cache owns the
                // null-terminated buffers the pointer refers to.
                let utf16_ptr = if let Some(cached) = self.utf16_strings.get(idx) {
                    cached.as_ptr()
                } else {
                    let s = bc
                        .strings
                        .get(*idx)
                        .ok_or_else(|| anyhow!("String constant out of bounds: {}", idx))?;
                    let mut buf: Vec<u16> = s.encode_utf16().collect();
                    buf.push(0);
                    self.utf16_strings.insert(*idx, buf);
                    self.utf16_strings[idx].as_ptr()
                };
                set!(dst, NanBoxedValue::from_bytes_ptr(utf16_ptr as usize));
            }
            I::Null { dst } => set!(dst, NanBoxedValue::null()),

            // ---- arithmetic -------------------------------------------
            I::BinOp { op, dst, a, b } => {
                use air::v2::BinOp as B;
                let va = get!(a);
                let vb = get!(b);
                let r = match op {
                    B::Add => va
                        .binary_int_op(vb, IntBinOp::Add)
                        .or_else(|| va.binary_float_op(vb, FloatBinOp::Add)),
                    B::Sub => va
                        .binary_int_op(vb, IntBinOp::Sub)
                        .or_else(|| va.binary_float_op(vb, FloatBinOp::Sub)),
                    B::Mul => va
                        .binary_int_op(vb, IntBinOp::Mul)
                        .or_else(|| va.binary_float_op(vb, FloatBinOp::Mul)),
                    B::SDiv => va
                        .binary_int_op(vb, IntBinOp::SDiv)
                        .or_else(|| va.binary_float_op(vb, FloatBinOp::SDiv)),
                    B::SMod => va
                        .binary_int_op(vb, IntBinOp::SMod)
                        .or_else(|| va.binary_float_op(vb, FloatBinOp::SMod)),
                    B::UDiv => va.binary_int_op(vb, IntBinOp::UDiv),
                    B::UMod => {
                        let r = vb.as_i32() as u32;
                        if r == 0 {
                            return Err(anyhow!("UMod: division by zero"));
                        }
                        Some(NanBoxedValue::from_i32(((va.as_i32() as u32) % r) as i32))
                    }
                    B::Shl => va.binary_int_op(vb, IntBinOp::Shl),
                    B::SShr => va.binary_int_op(vb, IntBinOp::SShr),
                    B::UShr => va.binary_int_op(vb, IntBinOp::UShr),
                    B::And => va.binary_int_op(vb, IntBinOp::And),
                    B::Or => va.binary_int_op(vb, IntBinOp::Or),
                    B::Xor => va.binary_int_op(vb, IntBinOp::Xor),
                };
                let r = r.ok_or_else(|| {
                    anyhow!(
                        "{:?}: incompatible types {:?}, {:?} in {} (dst=v{}, a=v{}, b=v{})",
                        op,
                        va,
                        vb,
                        func.name(),
                        dst.0,
                        a.0,
                        b.0
                    )
                })?;
                set!(dst, r);
            }
            I::Fma { dst, a, b, c } => {
                // Deliberately two roundings, not `mul_add`. The FMA peephole
                // exists for backends that emit a hardware fused multiply-add;
                // this interpreter is the bit-exact reference the others are
                // measured against, and it rounds every operation — fusing here
                // would move the measuring stick.
                let r = get!(a).as_f64() * get!(b).as_f64() + get!(c).as_f64();
                set!(dst, NanBoxedValue::from_f64(r));
            }
            I::UnOp { op, dst, src } => {
                let v = get!(src);
                let r = match op {
                    // Mirrors the opcode arms exactly, including leaving a
                    // non-numeric value untouched rather than erroring.
                    air::v2::UnOp::Incr => {
                        if v.is_i32() {
                            NanBoxedValue::from_i32(v.as_i32().wrapping_add(1))
                        } else if v.is_i64() {
                            NanBoxedValue::from_i64(v.as_i64_lossy().wrapping_add(1))
                        } else if v.is_f64() {
                            NanBoxedValue::from_f64(v.as_f64() + 1.0)
                        } else {
                            v
                        }
                    }
                    air::v2::UnOp::Decr => {
                        if v.is_i32() {
                            NanBoxedValue::from_i32(v.as_i32().wrapping_sub(1))
                        } else if v.is_i64() {
                            NanBoxedValue::from_i64(v.as_i64_lossy().wrapping_sub(1))
                        } else if v.is_f64() {
                            NanBoxedValue::from_f64(v.as_f64() - 1.0)
                        } else {
                            v
                        }
                    }
                    air::v2::UnOp::Neg => {
                        if v.is_i32() {
                            NanBoxedValue::from_i32(v.as_i32().wrapping_neg())
                        } else if v.is_i64() {
                            NanBoxedValue::from_i64(v.as_i64_lossy().wrapping_neg())
                        } else if v.is_f64() {
                            NanBoxedValue::from_f64(-v.as_f64())
                        } else {
                            return Err(anyhow!("Neg: unsupported type {:?}", v));
                        }
                    }
                    air::v2::UnOp::Not => {
                        if v.is_i32() {
                            NanBoxedValue::from_i32(!v.as_i32())
                        } else if v.is_i64() {
                            NanBoxedValue::from_i64(!v.as_i64_lossy())
                        } else if v.is_bool() {
                            NanBoxedValue::from_bool(!v.as_bool())
                        } else {
                            return Err(anyhow!("Not: unsupported type {:?}", v));
                        }
                    }
                };
                set!(dst, r);
            }

            // ---- calls -------------------------------------------------
            I::Intrinsic {
                kind, dst, args: a, ..
            } => {
                // Inline Rust, no FFI dispatch, no marshal. Semantics are
                // pinned to the ash_std bodies these replaced — RoundHalfUp
                // is floor(x + 0.5) and the i32 conversions are Rust `as`
                // (saturating, NaN -> 0).
                use air::v2::ir::IntrinsicKind as K;
                let r = match kind {
                    K::PtrCompare => {
                        let (pa, pb) = (get!(&a[0]).as_ptr(), get!(&a[1]).as_ptr());
                        NanBoxedValue::from_i32(match pa.cmp(&pb) {
                            std::cmp::Ordering::Equal => 0,
                            std::cmp::Ordering::Greater => 1,
                            std::cmp::Ordering::Less => -1,
                        })
                    }
                    _ => {
                        let x = get!(&a[0]).as_f64();
                        match kind {
                            K::Sqrt => NanBoxedValue::from_f64(x.sqrt()),
                            K::Abs => NanBoxedValue::from_f64(x.abs()),
                            K::Floor => NanBoxedValue::from_f64(x.floor()),
                            K::Ceil => NanBoxedValue::from_f64(x.ceil()),
                            K::RoundHalfUp => NanBoxedValue::from_f64((x + 0.5).floor()),
                            K::FloorToI32 => NanBoxedValue::from_i32(x.floor() as i32),
                            K::CeilToI32 => NanBoxedValue::from_i32(x.ceil() as i32),
                            K::RoundHalfUpToI32 => {
                                NanBoxedValue::from_i32((x + 0.5).floor() as i32)
                            }
                            K::IsNaN => NanBoxedValue::from_bool(x.is_nan()),
                            K::IsFinite => NanBoxedValue::from_bool(x.is_finite()),
                            K::PtrCompare => unreachable!("handled above"),
                        }
                    }
                };
                set!(dst, r);
            }
            I::Call { dst, fun, args: a } => {
                // From the pool `ssa_call` returns them to. Collecting a fresh
                // one made every call above arity zero a malloc and a free,
                // with the pool filling up and never being read -- the same
                // one-sided pooling the opcode loop had, mirrored.
                let mut argv = self.arg_pool.pop().unwrap_or_default();
                argv.clear();
                argv.reserve(a.len());
                for v in a.iter() {
                    let x = get!(v);
                    argv.push(x);
                }
                return self.ssa_call(bc, native_resolver, func, *fun, argv, dst.0);
            }
            I::CallMethod {
                dst,
                field,
                args: a,
            } => {
                let regs: Vec<Reg> = a.iter().map(|v| Reg(v.0)).collect();
                let staged =
                    self.op_call_method(bc, func, func_idx, false, dst.0, *field, &regs)?;
                return self.ssa_staged_call(bc, native_resolver, func, staged);
            }
            I::CallClosure { dst, fun, args: a } => {
                let regs: Vec<Reg> = a.iter().map(|v| Reg(v.0)).collect();
                let staged = self.op_call_closure(bc, func, func_idx, dst.0, fun.0, &regs)?;
                return self.ssa_staged_call(bc, native_resolver, func, staged);
            }
            I::StaticClosure { dst, fun } => {
                self.op_static_closure(bc, func, func_idx, dst.0, *fun)?;
            }
            I::InstanceClosure { dst, fun, obj } => {
                self.op_instance_closure(bc, func, func_idx, dst.0, *fun, obj.0)?;
            }
            I::VirtualClosure { dst, obj, field } => {
                self.op_virtual_closure(bc, func, func_idx, dst.0, obj.0, *field as u32)?;
            }

            // ---- globals and fields ------------------------------------
            I::GetGlobal { dst, global } => {
                let mut val = self
                    .globals
                    .get(*global)
                    .copied()
                    .unwrap_or_else(NanBoxedValue::null);
                // Native stdlib may have written a global_value slot without
                // going through SetGlobal.
                if val.is_null() {
                    let (gd, nglobals) = self.c_type_factory.globals_data();
                    if !gd.is_null() && *global < nglobals {
                        let raw = unsafe { *gd.add(*global) };
                        if !raw.is_null() {
                            val = NanBoxedValue::from_ptr(raw as usize);
                            self.globals[*global] = val;
                        }
                    }
                }
                set!(dst, val);
            }
            I::SetGlobal { global, src } => {
                let val = get!(src);
                if *global >= self.globals.len() {
                    self.globals.resize(*global + 1, NanBoxedValue::null());
                }
                self.globals[*global] = val;
                let (gd, nglobals) = self.c_type_factory.globals_data();
                if !gd.is_null() && *global < nglobals {
                    unsafe {
                        *gd.add(*global) = if val.is_null() || val.is_void() {
                            std::ptr::null_mut()
                        } else {
                            val.as_ptr() as *mut c_void
                        };
                    }
                }
            }
            I::FieldGet {
                dst, obj, field, ..
            } => {
                self.op_field_get(bc, func, func_idx, dst.0, obj.0, *field)?;
            }
            I::FieldSet {
                obj, field, src, ..
            } => {
                self.op_field_set(bc, func, func_idx, obj.0, *field, src.0)?;
            }
            I::DynGet { dst, obj, field } => {
                self.op_dyn_get(bc, func, func_idx, dst.0, obj.0, *field)?;
            }
            I::DynSet { obj, field, src } => {
                self.op_dyn_set(bc, func, func_idx, obj.0, *field, src.0)?;
            }

            // ---- casts -------------------------------------------------
            I::Cast { kind, dst, src } => {
                use air::v2::CastKind as K;
                match kind {
                    K::ToDyn => {
                        self.op_to_dyn(bc, func, func_idx, dst.0, src.0)?;
                    }
                    K::SafeCast => {
                        // A converting cast (HOBJ -> unrelated HOBJ) is not a
                        // value the opcode can produce on its own: it has to run
                        // the class's `__cast`, which op_safe_cast hands back as
                        // a staged `StepResult::Call`. Dropping that staged call
                        // leaves `dst` holding the scratch value op_safe_cast
                        // parked there — the *source* pointer — so the cast
                        // silently degrades to the reinterpret this opcode
                        // exists to avoid, and the next field read dereferences
                        // an integer. Dispatch it the way CallMethod and
                        // CallClosure dispatch theirs.
                        let staged = self.op_safe_cast(bc, func, func_idx, dst.0, src.0)?;
                        match staged {
                            StepResult::Call { .. } => {
                                return self.ssa_staged_call(bc, native_resolver, func, staged);
                            }
                            // SSA trap entries store handler block IDs in the
                            // same tuple where the opcode interpreter stores
                            // absolute PCs. invalid_cast_step has already put
                            // the exception value in the trap cell.
                            StepResult::JumpAbs(handler) => return Ok(Some(handler)),
                            _ => {}
                        }
                    }
                    K::ToSFloat => {
                        let v = get!(src);
                        let f = if v.is_i32() {
                            v.as_i32() as f64
                        } else {
                            v.as_f64()
                        };
                        set!(dst, NanBoxedValue::from_f64(f));
                    }
                    K::ToUFloat => {
                        let v = get!(src);
                        let f = if v.is_i32() {
                            (v.as_i32() as u32) as f64
                        } else {
                            v.as_f64()
                        };
                        set!(dst, NanBoxedValue::from_f64(f));
                    }
                    K::ToInt => {
                        // Same dst-width rule as the opcode dispatcher: the
                        // destination decides i32 vs i64 (haxe.Int64 widens
                        // through this cast).
                        let v = get!(src);
                        let dk = kind!(dst);
                        if dk == hl::hl_type_kind_HI64 {
                            let i = if v.is_f64() {
                                v.as_f64() as i64
                            } else if v.is_i32() {
                                v.as_i32() as i64
                            } else {
                                v.as_i64_lossy()
                            };
                            set!(dst, NanBoxedValue::from_i64(i));
                        } else {
                            let i = if v.is_f64() {
                                v.as_f64() as i32
                            } else if v.is_i32() {
                                v.as_i32()
                            } else {
                                v.as_i64_lossy() as i32
                            };
                            set!(dst, NanBoxedValue::from_i32(i));
                        }
                    }
                    K::UnsafeCast => {
                        let v = get!(src);
                        set!(dst, v);
                    }
                    K::ToVirtual => {
                        self.op_to_virtual(func, dst.0, src.0)?;
                    }
                }
            }
            I::NullCheck { value } => {
                if get!(value).is_null() {
                    if env_flag!("ASH_TRACE_NULLACC") {
                        eprintln!("[nullacc/ssa] {} v{}", func.name(), value.0);
                    }
                    let stack = self.capture_call_stack(bc);
                    return Err(anyhow::Error::new(HLExceptionPropagation {
                        value: self.internal_exception_value("Null access"),
                        message: Some("Null access".to_string()),
                        stack,
                    }));
                }
            }

            // ---- memory ------------------------------------------------
            I::MemGet {
                kind,
                dst,
                base,
                index,
            } => match kind {
                air::v2::MemAccess::Array => {
                    self.op_get_array(bc, func, func_idx, dst.0, base.0, index.0)?;
                }
                k => {
                    let b = get!(base);
                    let idx = get!(index).as_i32();
                    let val = if b.is_null() || b.is_void() || idx < 0 {
                        NanBoxedValue::from_i32(0)
                    } else {
                        let addr = (b.as_ptr() as *const u8).wrapping_add(idx as usize);
                        match k {
                            air::v2::MemAccess::I8 => {
                                NanBoxedValue::from_i32(unsafe { *addr as i32 })
                            }
                            air::v2::MemAccess::I16 => {
                                NanBoxedValue::from_i32(unsafe { *(addr as *const u16) as i32 })
                            }
                            _ => Self::read_value_from_ptr(addr, kind!(dst)),
                        }
                    };
                    set!(dst, val);
                }
            },
            I::MemSet {
                kind,
                base,
                index,
                src,
            } => match kind {
                air::v2::MemAccess::Array => {
                    self.op_set_array(bc, func, func_idx, base.0, index.0, src.0)?;
                }
                k => {
                    let b = get!(base);
                    let idx = get!(index).as_i32();
                    let v = get!(src);
                    if !b.is_null() && !b.is_void() && idx >= 0 {
                        let addr = (b.as_ptr() as *mut u8).wrapping_add(idx as usize);
                        match k {
                            air::v2::MemAccess::I8 => unsafe { *addr = v.as_i32() as u8 },
                            air::v2::MemAccess::I16 => unsafe {
                                *(addr as *mut u16) = v.as_i32() as u16
                            },
                            _ => {
                                if (addr as usize) < 0x1000 {
                                    eprintln!(
                                        "[CRASH GUARD] SetMem bad addr={:p} base={:?} idx={} in {}",
                                        addr,
                                        b,
                                        idx,
                                        func.name()
                                    );
                                } else {
                                    Self::write_value_to_ptr(addr, v, kind!(src));
                                }
                            }
                        }
                    }
                }
            },

            // ---- allocation and type queries ---------------------------
            I::New { dst } => {
                self.op_new(bc, func, func_idx, dst.0)?;
            }
            I::ArraySize { dst, array } => {
                let arr = get!(array);
                let size = if kind!(array) == hl::hl_type_kind_HARRAY
                    && !arr.is_null()
                    && !arr.is_void()
                {
                    // varray: t@0, at@8, size@16
                    unsafe { *((arr.as_ptr() as *const u8).add(16) as *const i32) }
                } else {
                    0i32
                };
                set!(dst, NanBoxedValue::from_i32(size));
            }
            I::TypeConst { dst, ty } => {
                let p = self.c_type_factory.get(ty.0 as usize);
                set!(dst, NanBoxedValue::from_ptr(p as usize));
            }
            I::GetType { dst, src } => {
                let v = get!(src);
                let src_ty = func.regs[src.0 as usize].0;
                let ptr: usize = if v.is_null() || v.is_void() {
                    let void_idx = bc
                        .types
                        .iter()
                        .position(|t| t.kind == hl::hl_type_kind_HVOID)
                        .unwrap_or(src_ty);
                    self.c_type_factory.get(void_idx) as usize
                } else if v.is_ptr() && !v.is_null() && v.as_ptr() != 0 {
                    match bc.types[src_ty].kind {
                        hl::hl_type_kind_HDYN
                        | hl::hl_type_kind_HOBJ
                        | hl::hl_type_kind_HSTRUCT
                        | hl::hl_type_kind_HVIRTUAL
                        | hl::hl_type_kind_HENUM
                        | hl::hl_type_kind_HDYNOBJ
                        | hl::hl_type_kind_HNULL => unsafe { *(v.as_ptr() as *const usize) },
                        _ => self.c_type_factory.get(src_ty) as usize,
                    }
                } else {
                    self.c_type_factory.get(src_ty) as usize
                };
                set!(dst, NanBoxedValue::from_ptr(ptr));
            }
            I::GetTID { dst, src } => {
                let v = get!(src);
                let k = if v.is_ptr() && !v.is_null() && v.as_ptr() != 0 {
                    unsafe { *(v.as_ptr() as *const hl::hl_type_kind) as i32 }
                } else {
                    bc.types[func.regs[src.0 as usize].0].kind as i32
                };
                set!(dst, NanBoxedValue::from_i32(k));
            }

            // ---- references --------------------------------------------
            I::Unref { dst, src } => {
                let p = get!(src).as_ptr() as *const i64;
                let r = if p.is_null() {
                    NanBoxedValue::null()
                } else {
                    let raw = unsafe { *p };
                    match kind!(dst) {
                        hl::hl_type_kind_HI32 | hl::hl_type_kind_HUI8 | hl::hl_type_kind_HUI16 => {
                            NanBoxedValue::from_i32(raw as i32)
                        }
                        hl::hl_type_kind_HF64 | hl::hl_type_kind_HF32 => {
                            NanBoxedValue::from_f64(f64::from_bits(raw as u64))
                        }
                        // Low 32 bits only: a native writes a c_int here, and
                        // the NaN tag bits would make the full i64 always true.
                        hl::hl_type_kind_HBOOL => NanBoxedValue::from_bool((raw as i32) != 0),
                        _ => NanBoxedValue::from_ptr(raw as usize),
                    }
                };
                set!(dst, r);
            }
            I::SetRef { r, value } => {
                let p = get!(r).as_ptr() as *mut NanBoxedValue;
                if !p.is_null() {
                    let v = get!(value);
                    unsafe { *p = v };
                }
            }
            I::RefData { dst, src } => {
                let v = get!(src);
                let data = v.as_ptr() + std::mem::size_of::<hl::varray>();
                set!(dst, NanBoxedValue::from_ptr(data));
            }
            I::RefOffset { dst, base, offset } => {
                let r =
                    NanBoxedValue::from_ptr(get!(base).as_ptr() + get!(offset).as_i32() as usize);
                set!(dst, r);
            }

            // ---- enums -------------------------------------------------
            I::MakeEnum {
                dst,
                construct,
                args: a,
            } => {
                let c_type_ptr = self.c_type_factory.get(func.regs[dst.0 as usize].0);
                let val = Self::alloc_enum_value(self.fn_alloc_enum, c_type_ptr, *construct as i32);
                if !val.is_null() {
                    let argv: Vec<NanBoxedValue> = a.iter().map(|v| get!(v)).collect();
                    unsafe {
                        let tenum = (*c_type_ptr).__bindgen_anon_1.tenum;
                        let c = &*(*tenum).constructs.add(*construct);
                        let base = val;
                        for (i, v) in argv.into_iter().enumerate() {
                            if i >= c.nparams as usize {
                                break;
                            }
                            let offset = *c.offsets.add(i) as usize;
                            let param_kind = (*(*c.params.add(i))).kind;
                            Self::write_value_to_ptr(base.add(offset), v, param_kind);
                        }
                    }
                }
                set!(
                    dst,
                    if val.is_null() {
                        NanBoxedValue::null()
                    } else {
                        NanBoxedValue::from_ptr(val as usize)
                    }
                );
            }
            I::EnumAlloc { dst, construct } => {
                let c_type_ptr = self.c_type_factory.get(func.regs[dst.0 as usize].0);
                let val = Self::alloc_enum_value(self.fn_alloc_enum, c_type_ptr, *construct as i32);
                set!(
                    dst,
                    if val.is_null() {
                        NanBoxedValue::null()
                    } else {
                        NanBoxedValue::from_ptr(val as usize)
                    }
                );
            }
            I::EnumIndex { dst, value } => {
                let v = get!(value);
                let index = if v.is_null() || v.is_void() {
                    0i32
                } else {
                    // venum: t@0, index@8
                    unsafe { *(v.as_ptr() as *const u8).add(8).cast::<i32>() }
                };
                set!(dst, NanBoxedValue::from_i32(index));
            }
            I::EnumField {
                dst,
                value,
                construct,
                field,
            } => {
                let v = get!(value);
                let c_type_ptr = self.c_type_factory.get(func.regs[value.0 as usize].0);
                let r = if v.is_null() || v.is_void() || c_type_ptr.is_null() {
                    NanBoxedValue::null()
                } else {
                    unsafe {
                        let tenum = (*c_type_ptr).__bindgen_anon_1.tenum;
                        if tenum.is_null() || *construct >= (*tenum).nconstructs as usize {
                            NanBoxedValue::null()
                        } else {
                            let c = &*(*tenum).constructs.add(*construct);
                            if *field >= c.nparams as usize {
                                NanBoxedValue::null()
                            } else {
                                let offset = *c.offsets.add(*field) as usize;
                                let param_kind = (*(*c.params.add(*field))).kind;
                                Self::read_value_from_ptr(
                                    (v.as_ptr() as *const u8).add(offset),
                                    param_kind,
                                )
                            }
                        }
                    }
                };
                set!(dst, r);
            }
            I::SetEnumField {
                value, field, src, ..
            } => {
                let v = get!(value);
                let src_val = get!(src);
                let c_type_ptr = self.c_type_factory.get(func.regs[value.0 as usize].0);
                if !v.is_null() && !v.is_void() && !c_type_ptr.is_null() {
                    unsafe {
                        let tenum = (*c_type_ptr).__bindgen_anon_1.tenum;
                        if !tenum.is_null() {
                            // The construct comes off the live venum, not the
                            // instruction: that is what the reference does, and
                            // the two disagree when a register is reused.
                            let ci = *(v.as_ptr() as *const u8).add(8).cast::<i32>() as usize;
                            if ci < (*tenum).nconstructs as usize {
                                let c = &*(*tenum).constructs.add(ci);
                                if *field < c.nparams as usize {
                                    let offset = *c.offsets.add(*field) as usize;
                                    let param_kind = (*(*c.params.add(*field))).kind;
                                    Self::write_value_to_ptr(
                                        (v.as_ptr() as *mut u8).add(offset),
                                        src_val,
                                        param_kind,
                                    );
                                }
                            }
                        }
                    }
                }
            }

            // ---- cells (pinned registers) -------------------------------
            I::CellGet { dst, cell } => {
                let v = self.stack.last().unwrap().registers.get(cell_base + cell.0);
                set!(dst, v);
            }
            I::CellSet { cell, src } => {
                let v = get!(src);
                self.stack
                    .last_mut()
                    .unwrap()
                    .registers
                    .set(cell_base + cell.0, v);
            }
            I::CellIncr { cell } => {
                let frame = self.stack.last_mut().unwrap();
                let slot = cell_base + cell.0;
                let v = frame.registers.get(slot);
                if v.is_i32() {
                    frame
                        .registers
                        .set(slot, NanBoxedValue::from_i32(v.as_i32().wrapping_add(1)));
                } else if v.is_i64() {
                    frame.registers.set(
                        slot,
                        NanBoxedValue::from_i64(v.as_i64_lossy().wrapping_add(1)),
                    );
                } else if v.is_f64() {
                    frame
                        .registers
                        .set(slot, NanBoxedValue::from_f64(v.as_f64() + 1.0));
                }
            }
            I::CellDecr { cell } => {
                let frame = self.stack.last_mut().unwrap();
                let slot = cell_base + cell.0;
                let v = frame.registers.get(slot);
                if v.is_i32() {
                    frame
                        .registers
                        .set(slot, NanBoxedValue::from_i32(v.as_i32().wrapping_sub(1)));
                } else if v.is_i64() {
                    frame.registers.set(
                        slot,
                        NanBoxedValue::from_i64(v.as_i64_lossy().wrapping_sub(1)),
                    );
                } else if v.is_f64() {
                    frame
                        .registers
                        .set(slot, NanBoxedValue::from_f64(v.as_f64() - 1.0));
                }
            }
            I::CellRef { dst, cell } => {
                // Address of the cell's frame slot, exactly as `Ref` takes the
                // address of a register slot: natives write through it and the
                // cell is updated in place. The slot is stable across nested
                // calls because the frame's `Vec` is its own allocation.
                let frame = self.stack.last_mut().unwrap();
                let p = frame.registers.slot_ptr(cell_base + cell.0) as usize;
                frame.registers.set(dst.0, NanBoxedValue::from_ptr(p));
            }

            // ---- trap regions -------------------------------------------
            I::EndTrap { .. } => {
                // Pop the handler and leave the cell alone. Ending a trap says
                // nothing about the value in its exception cell, and lowering
                // reuses that cell for other things: Template's try/catch
                // wrapper stores the call result there, ends the trap, then
                // reads it back to return it. Clearing on the way out returned
                // null from every such function, and the opcode loop -- which
                // only pops -- is the reference.
                self.stack.last_mut().unwrap().trap_stack.pop();
            }

            // ---- misc ---------------------------------------------------
            I::Assert => {
                // Catchable, like upstream hl_assert() — see the classic
                // dispatcher's Opcode::Assert.
                let stack = self.capture_call_stack(bc);
                return Err(anyhow::Error::new(HLExceptionPropagation {
                    value: self.internal_exception_value("assert"),
                    message: Some("assert".to_string()),
                    stack,
                }));
            }
            // A position marker for a shadow call stack the interpreter does
            // not keep: its frames are its own, and lowering emits the marker
            // only for a target whose frames record positions.
            I::Prefetch { .. } | I::Asm { .. } | I::Pos { .. } => {}
        }

        Ok(None)
    }

    /// Perform a staged call produced by one of the shared `op_call_*` methods.
    pub(super) fn ssa_staged_call(
        &mut self,
        bc: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        func: &HLFunction,
        staged: StepResult,
    ) -> Result<Option<usize>> {
        match staged {
            StepResult::Call { findex, args, dst } => {
                self.ssa_call(bc, native_resolver, func, findex, args, dst)
            }
            // The closure paths answer `Continue` when they resolved to a value
            // rather than a call (a null receiver, say).
            _ => Ok(None),
        }
    }

    /// Call `findex`, store the coerced result, and let this frame's innermost
    /// trap catch an exception coming back out.
    pub(super) fn ssa_call(
        &mut self,
        bc: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        func: &HLFunction,
        findex: usize,
        mut args: Vec<NanBoxedValue>,
        dst: u32,
    ) -> Result<Option<usize>> {
        // Same reclaim as the serialize trampoline: the callee has copied the
        // arguments into its own registers, so the buffer goes back to the pool
        // before the result is examined.
        let call_result = self.call_function(bc, native_resolver, findex, &args);
        if self.arg_pool.len() < POOL_CAP {
            args.clear();
            self.arg_pool.push(args);
        }
        match call_result {
            Ok(ret) => {
                let dst_kind = bc.types[func.regs[dst as usize].0].kind;
                let coerced = Self::coerce_value_for_static_kind(ret, dst_kind);
                self.stack.last_mut().unwrap().registers.set(dst, coerced);
                Ok(None)
            }
            Err(e) => {
                if let Some(exc_val) = e.downcast_ref::<HLExceptionPropagation>().map(|x| x.value) {
                    let frame = self.stack.last_mut().unwrap();
                    if let Some((handler, cell_slot)) = frame.trap_stack.pop() {
                        frame.registers.set(cell_slot, exc_val);
                        return Ok(Some(handler));
                    }
                }
                Err(e)
            }
        }
    }
}

impl HLInterpreter {
    /// Lanes of a widened value in the current frame.
    fn lanes_of(&self, v: u32) -> anyhow::Result<Vec<NanBoxedValue>> {
        self.stack
            .last()
            .and_then(|f| f.vec_lanes.get(&v))
            .cloned()
            .ok_or_else(|| anyhow::anyhow!("v{v} has no lanes in this frame"))
    }

    /// Read one lane through the instruction's OWN registers.
    ///
    /// The scalar array path takes register indices, so the lane index is
    /// written into the instruction's index register, the scalar op runs, and
    /// both registers are put back. Borrowing the real registers rather than
    /// inventing scratch slots keeps this correct for any register file the
    /// shim happens to have.
    #[allow(clippy::too_many_arguments)]
    fn vec_lane_get(
        &mut self,
        bc: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        kind: air::v2::MemAccess,
        dst: u32,
        base: u32,
        index: u32,
        at: i32,
    ) -> anyhow::Result<NanBoxedValue> {
        let (saved_dst, saved_idx) = {
            let f = self.stack.last().unwrap();
            (f.registers.get(dst), f.registers.get(index))
        };
        self.stack
            .last_mut()
            .unwrap()
            .registers
            .set(index, NanBoxedValue::from_i32(at));
        let r = match kind {
            air::v2::MemAccess::Array => {
                self.op_get_array(bc, func, func_idx, dst, base, index).map(|_| ())
            }
            k => {
                let b = self.stack.last().unwrap().registers.get(base);
                let val = Self::read_raw_lane(b, at, k, bc, func, dst);
                self.stack.last_mut().unwrap().registers.set(dst, val);
                Ok(())
            }
        };
        let out = self.stack.last().unwrap().registers.get(dst);
        {
            let f = self.stack.last_mut().unwrap();
            f.registers.set(dst, saved_dst);
            f.registers.set(index, saved_idx);
        }
        r?;
        Ok(out)
    }

    #[allow(clippy::too_many_arguments)]
    fn vec_lane_set(
        &mut self,
        bc: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        kind: air::v2::MemAccess,
        src: u32,
        base: u32,
        index: u32,
        at: i32,
        value: NanBoxedValue,
    ) -> anyhow::Result<()> {
        let (saved_src, saved_idx) = {
            let f = self.stack.last().unwrap();
            (f.registers.get(src), f.registers.get(index))
        };
        {
            let f = self.stack.last_mut().unwrap();
            f.registers.set(src, value);
            f.registers.set(index, NanBoxedValue::from_i32(at));
        }
        let r = match kind {
            air::v2::MemAccess::Array => {
                self.op_set_array(bc, func, func_idx, base, index, src).map(|_| ())
            }
            k => {
                let b = self.stack.last().unwrap().registers.get(base);
                Self::write_raw_lane(b, at, k, value, bc, func, src);
                Ok(())
            }
        };
        {
            let f = self.stack.last_mut().unwrap();
            f.registers.set(src, saved_src);
            f.registers.set(index, saved_idx);
        }
        r
    }

    /// The non-array read, mirroring the scalar `MemGet` arm exactly.
    fn read_raw_lane(
        b: NanBoxedValue,
        idx: i32,
        k: air::v2::MemAccess,
        bc: &DecodedBytecode,
        func: &HLFunction,
        dst: u32,
    ) -> NanBoxedValue {
        if b.is_null() || b.is_void() || idx < 0 {
            return NanBoxedValue::from_i32(0);
        }
        let addr = (b.as_ptr() as *const u8).wrapping_add(idx as usize);
        match k {
            air::v2::MemAccess::I8 => NanBoxedValue::from_i32(unsafe { *addr as i32 }),
            air::v2::MemAccess::I16 => {
                NanBoxedValue::from_i32(unsafe { *(addr as *const u16) as i32 })
            }
            _ => Self::read_value_from_ptr(addr, bc.types[func.regs[dst as usize].0].kind),
        }
    }

    /// The non-array write, mirroring the scalar `MemSet` arm.
    fn write_raw_lane(
        b: NanBoxedValue,
        idx: i32,
        k: air::v2::MemAccess,
        value: NanBoxedValue,
        bc: &DecodedBytecode,
        func: &HLFunction,
        src: u32,
    ) {
        if b.is_null() || b.is_void() || idx < 0 {
            return;
        }
        let addr = (b.as_ptr() as *mut u8).wrapping_add(idx as usize);
        match k {
            air::v2::MemAccess::I8 => unsafe { *addr = value.as_i32() as u8 },
            air::v2::MemAccess::I16 => unsafe { *(addr as *mut u16) = value.as_i32() as u16 },
            _ => unsafe {
                Self::write_value_at(addr, bc.types[func.regs[src as usize].0].kind, value)
            },
        }
    }

    /// The scalar combine used by `VecBinOp` and `VecReduce`.
    fn scalar_binop(
        op: air::v2::BinOp,
        a: NanBoxedValue,
        b: NanBoxedValue,
    ) -> anyhow::Result<NanBoxedValue> {
        use crate::values::{FloatBinOp, IntBinOp};
        use air::v2::BinOp as B;
        let r = match op {
            B::Add => a
                .binary_int_op(b, IntBinOp::Add)
                .or_else(|| a.binary_float_op(b, FloatBinOp::Add)),
            B::Sub => a
                .binary_int_op(b, IntBinOp::Sub)
                .or_else(|| a.binary_float_op(b, FloatBinOp::Sub)),
            B::Mul => a
                .binary_int_op(b, IntBinOp::Mul)
                .or_else(|| a.binary_float_op(b, FloatBinOp::Mul)),
            B::SDiv => a
                .binary_int_op(b, IntBinOp::SDiv)
                .or_else(|| a.binary_float_op(b, FloatBinOp::SDiv)),
            B::UDiv => a.binary_int_op(b, IntBinOp::UDiv),
            B::And => a.binary_int_op(b, IntBinOp::And),
            B::Or => a.binary_int_op(b, IntBinOp::Or),
            B::Xor => a.binary_int_op(b, IntBinOp::Xor),
            other => return Err(anyhow::anyhow!("unsupported vector op {other:?}")),
        };
        r.ok_or_else(|| anyhow::anyhow!("vector lane operands are not numeric"))
    }
}
