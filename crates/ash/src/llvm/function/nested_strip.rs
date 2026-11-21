//! A counted loop whose fiber poll sits in an outer loop.
//!
//! The poll is a call with volatile and atomic loads, and LLVM's loop
//! vectorizer will not widen a loop that holds one, even when it is only
//! reached once per strip. So a call-free innermost loop of the shape
//!
//! ```text
//! for (i = ..; i < bound; i += k) body
//! ```
//!
//! is emitted as two loops: the outer one polls and then sets `end` to
//! `min(bound, i + STRIP)`; the inner one runs `i < end` with nothing in it
//! that is not the body. Safepoint latency stays one strip, as before.
//!
//! Only the LLVM emission changes. The AIR is untouched, so the interpreter,
//! the OSR transfer and the Cranelift tier see the loop they always did.
//! A loop that carries a floating-point value from one trip to the next keeps
//! the single polled loop: LLVM will not vectorize it, so the split would only
//! add compile time.
//! `ASH_NESTED_STRIP=0` emits the single polled loop instead; safe to run with.

use air::v2::analysis::NaturalLoop;
use air::v2::ir::{
    BinOp as AirBinOp, BlockId as AirBlockId, CondKind as AirCondKind, Function as AirFunction,
    Instr as AirInstr, Terminator as AirTerminator, UnOp as AirUnOp, ValueId,
};
use anyhow::{Result, anyhow};
use inkwell::basic_block::BasicBlock;
use inkwell::types::BasicTypeEnum;
use inkwell::values::PointerValue;
use inkwell::IntPredicate;

use crate::llvm::module::JITModule;

/// Whether the nested form is used. `ASH_NESTED_STRIP=0` turns it off.
pub(super) fn nested_strip_enabled() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| !matches!(std::env::var("ASH_NESTED_STRIP").as_deref(), Ok("0")))
}

/// What the AIR says about a loop that can take the nested form.
#[derive(Clone, Copy)]
pub(super) struct NestedPlan {
    pub phi: ValueId,
    pub bound: ValueId,
    pub signed: bool,
    /// The header's true edge leaves the loop (`i >= bound`), rather than
    /// staying in it (`i < bound`).
    pub exit_on_true: bool,
}

/// A planned loop with its stack slot for the end of the current strip.
pub(super) struct NestedStrip<'ctx> {
    pub plan: NestedPlan,
    pub end: PointerValue<'ctx>,
}

/// Instructions that lower to straight-line code. Anything else may call into
/// the runtime, which is the one thing the inner loop must not hold.
fn straight_line(instr: &AirInstr) -> bool {
    matches!(
        instr,
        AirInstr::Param { .. }
            | AirInstr::Pos { .. }
            | AirInstr::Copy { .. }
            | AirInstr::Int { .. }
            | AirInstr::Float { .. }
            | AirInstr::Bool { .. }
            | AirInstr::Null { .. }
            | AirInstr::BinOp { .. }
            | AirInstr::UnOp { .. }
            | AirInstr::Fma { .. }
            | AirInstr::Intrinsic { .. }
            | AirInstr::Cast { .. }
            | AirInstr::FieldGet { .. }
            | AirInstr::FieldSet { .. }
            | AirInstr::MemGet { .. }
            | AirInstr::MemSet { .. }
            | AirInstr::NullCheck { .. }
            | AirInstr::ArraySize { .. }
            | AirInstr::VecLoad { .. }
            | AirInstr::VecStore { .. }
            | AirInstr::VecSplat { .. }
            | AirInstr::VecBinOp { .. }
            | AirInstr::VecReduce { .. }
            | AirInstr::VecOp { .. }
            | AirInstr::VecExtract { .. }
            | AirInstr::VecInsert { .. }
            | AirInstr::Prefetch { .. }
            | AirInstr::CellGet { .. }
            | AirInstr::CellSet { .. }
            | AirInstr::CellIncr { .. }
            | AirInstr::CellDecr { .. }
    )
}

impl<'ctx> JITModule<'ctx> {
    /// The nested form for `lp`, if its shape allows: innermost, one latch, a
    /// header that tests `phi < bound` against a loop-invariant bound, a
    /// positive constant step, and a body with no calls and no exceptions.
    pub(super) fn nested_strip_plan(
        &self,
        air: &AirFunction,
        lp: &NaturalLoop,
        reg_types: &[BasicTypeEnum<'ctx>],
    ) -> Option<NestedPlan> {
        if !lp.children.is_empty() || lp.latches.len() != 1 {
            return None;
        }
        let in_loop = |b: AirBlockId| lp.blocks.contains(&b);
        let AirTerminator::CondJump {
            cond,
            a,
            b: Some(bound),
            if_true,
            if_false,
        } = &air.blocks[lp.header.idx()].term
        else {
            return None;
        };
        // The value the header compares, and what the latch hands it back.
        let phi = *a;
        let stepped = air.blocks[lp.header.idx()]
            .phis
            .iter()
            .find(|p| p.dst == phi)
            .and_then(|p| p.incoming.iter().find(|(from, _)| *from == lp.latches[0]))
            .map(|(_, v)| *v)?;
        let (signed, exit_on_true) = match cond {
            AirCondKind::SLt => (true, false),
            AirCondKind::ULt => (false, false),
            AirCondKind::SGte => (true, true),
            AirCondKind::UGte => (false, true),
            _ => return None,
        };
        // Exactly one edge leaves the loop, and it is the one the condition
        // says it is.
        let leaves = |t: &AirBlockId| !in_loop(*t);
        let shape_ok = if exit_on_true {
            leaves(if_true) && in_loop(*if_false)
        } else {
            in_loop(*if_true) && leaves(if_false)
        };
        if !shape_ok {
            return None;
        }
        // 32 bits at most, so the strip arithmetic fits in an i64.
        let BasicTypeEnum::IntType(int_ty) = reg_types[phi.idx()] else {
            return None;
        };
        if int_ty.get_bit_width() > 32 || reg_types[bound.idx()] != reg_types[phi.idx()] {
            return None;
        }
        // A floating-point recurrence cannot be vectorized without fast-math,
        // so nesting such a loop would cost compile time and win nothing.
        let header = &air.blocks[lp.header.idx()];
        if header
            .phis
            .iter()
            .any(|p| matches!(reg_types[p.dst.idx()], BasicTypeEnum::FloatType(_)))
        {
            return None;
        }
        // The bound is defined before the loop, so it is the same every trip.
        let defined_here = |v: ValueId| {
            lp.blocks.iter().any(|blk| {
                let block = &air.blocks[blk.idx()];
                block.phis.iter().any(|p| p.dst == v)
                    || block.instrs.iter().any(|i| i.dst() == Some(v))
            })
        };
        if defined_here(*bound) {
            return None;
        }
        // Counting up by a constant: the strip ends `STRIP` steps on.
        let mut counts_up = false;
        for blk in &lp.blocks {
            for instr in &air.blocks[blk.idx()].instrs {
                match instr {
                    AirInstr::UnOp {
                        op: AirUnOp::Incr,
                        dst,
                        src,
                    } if *dst == stepped && *src == phi => counts_up = true,
                    AirInstr::BinOp {
                        op: AirBinOp::Add,
                        dst,
                        a,
                        b,
                    } if *dst == stepped && *a == phi => {
                        counts_up = self.air_int_constant(air, *b).is_some_and(|k| k > 0);
                    }
                    _ => {}
                }
            }
        }
        if !counts_up {
            return None;
        }
        // Nothing in the loop may call out or unwind.
        for blk in &lp.blocks {
            let block = &air.blocks[blk.idx()];
            if block.handler.is_some()
                || !block.instrs.iter().all(straight_line)
                || !matches!(
                    block.term,
                    AirTerminator::Jump { .. }
                        | AirTerminator::CondJump { .. }
                        | AirTerminator::Ret { .. }
                )
            {
                return None;
            }
        }
        Some(NestedPlan {
            phi,
            bound: *bound,
            signed,
            exit_on_true,
        })
    }

    /// The value of an `Int` constant that `v` is defined by, if it is one.
    fn air_int_constant(&self, air: &AirFunction, v: ValueId) -> Option<i32> {
        air.blocks.iter().flat_map(|b| b.instrs.iter()).find_map(|i| match i {
            AirInstr::Int { dst, idx } if *dst == v => self.bytecode.ints.get(*idx).copied(),
            _ => None,
        })
    }

    /// At the top of a strip: `end = min(bound, i + STRIP)`, kept in the slot
    /// the inner loop's test reads. Widened to an i64, so it cannot wrap.
    pub(super) fn emit_strip_end(
        &self,
        strip: &NestedStrip<'ctx>,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
    ) -> Result<()> {
        let plan = strip.plan;
        let BasicTypeEnum::IntType(int_ty) = reg_types[plan.phi.idx()] else {
            return Err(anyhow!("nested strip over a non-integer induction value"));
        };
        let wide = self.context.i64_type();
        let phi = self
            .builder
            .build_load(int_ty, registers[plan.phi.idx()], "air_strip_i")?
            .into_int_value();
        let bound = self
            .builder
            .build_load(int_ty, registers[plan.bound.idx()], "air_strip_bound")?
            .into_int_value();
        let (phi, bound) = if plan.signed {
            (
                self.builder.build_int_s_extend(phi, wide, "air_strip_iw")?,
                self.builder.build_int_s_extend(bound, wide, "air_strip_bw")?,
            )
        } else {
            (
                self.builder.build_int_z_extend(phi, wide, "air_strip_iw")?,
                self.builder.build_int_z_extend(bound, wide, "air_strip_bw")?,
            )
        };
        let ahead = self.builder.build_int_add(
            phi,
            wide.const_int(air::v2::passes::stripmine::STRIP as u64, false),
            "air_strip_ahead",
        )?;
        let predicate = if plan.signed {
            IntPredicate::SLT
        } else {
            IntPredicate::ULT
        };
        let short = self
            .builder
            .build_int_compare(predicate, ahead, bound, "air_strip_short")?;
        let end = self
            .builder
            .build_select(short, ahead, bound, "air_strip_end")?
            .into_int_value();
        let end = self.builder.build_int_truncate(end, int_ty, "air_strip_end32")?;
        self.builder.build_store(strip.end, end)?;
        Ok(())
    }

    /// The header's branch for a nested loop: stay in the inner loop while
    /// `i < end`; at the end of a strip either start the next one through the
    /// poll, or leave by the original exit edge.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn emit_nested_header_branch(
        &mut self,
        air: &AirFunction,
        header: AirBlockId,
        strip: &NestedStrip<'ctx>,
        stay: (AirBlockId, BasicBlock<'ctx>),
        leave: (AirBlockId, BasicBlock<'ctx>),
        poll: BasicBlock<'ctx>,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
    ) -> Result<()> {
        let plan = strip.plan;
        let BasicTypeEnum::IntType(int_ty) = reg_types[plan.phi.idx()] else {
            return Err(anyhow!("nested strip over a non-integer induction value"));
        };
        let function = self
            .builder
            .get_insert_block()
            .and_then(|b| b.get_parent())
            .ok_or_else(|| anyhow!("AIR terminator has no parent function"))?;
        let predicate = if plan.signed {
            IntPredicate::SLT
        } else {
            IntPredicate::ULT
        };
        let phi = self
            .builder
            .build_load(int_ty, registers[plan.phi.idx()], "air_strip_i")?
            .into_int_value();
        let end = self
            .builder
            .build_load(int_ty, strip.end, "air_strip_end")?
            .into_int_value();
        let in_strip = self
            .builder
            .build_int_compare(predicate, phi, end, "air_in_strip")?;
        let stay_edge = self.context.append_basic_block(function, "air_true_edge");
        let strip_done = self.context.append_basic_block(function, "air_strip_done");
        self.builder
            .build_conditional_branch(in_strip, stay_edge, strip_done)?;

        self.builder.position_at_end(stay_edge);
        self.emit_air_phi_edge(air, header, stay.0, registers, reg_types)?;
        self.builder.build_unconditional_branch(stay.1)?;

        // The strip is spent. More to do: poll and start the next. None: the
        // exit edge, with the copies the original false edge carried.
        self.builder.position_at_end(strip_done);
        let bound = self
            .builder
            .build_load(int_ty, registers[plan.bound.idx()], "air_strip_bound")?
            .into_int_value();
        let more = self
            .builder
            .build_int_compare(predicate, phi, bound, "air_more")?;
        let leave_edge = self.context.append_basic_block(function, "air_false_edge");
        self.builder
            .build_conditional_branch(more, poll, leave_edge)?;
        self.builder.position_at_end(leave_edge);
        self.emit_air_phi_edge(air, header, leave.0, registers, reg_types)?;
        self.builder.build_unconditional_branch(leave.1)?;
        Ok(())
    }
}
