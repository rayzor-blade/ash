//! Loop widening: the transform behind [`vectorize`](crate::v2::vectorize).
//!
//! The analysis proves a loop's iterations may run several at a time and names
//! the obstacle for every loop it declines. This turns an accepted plan into
//! the vector instructions both backends lower.
//!
//! # The subset, and why
//!
//! Only loops whose trip count is a compile-time constant multiple of the
//! vector width. A loop over a runtime length is refused, and that is
//! deliberate: covering it needs a scalar epilogue for the remainder, and an
//! epilogue is where a wrong widening stops being a missed optimization and
//! becomes a wrong answer. The analysis makes the same argument about itself
//! -- "every unsound vectorizer in the wild is one that transformed on an
//! assumption this file is supposed to prove."
//!
//! Also refused for now, each because it needs machinery this does not have:
//!
//! * **Reductions.** A vector accumulator plus a post-loop [`Instr::VecReduce`]
//!   means editing the exit block and its phis.
//! * **Guarded exits.** [`LoopPlan::guard_exits`] marks bounds and null checks
//!   whose throw must be hoisted to one pre-loop check covering the whole
//!   vector range. The analysis is explicit that a plan with these "is not a
//!   plan the transform may take without doing that".
//! * **Non-unit strides.** Widenable with a gather, which the IR has no
//!   instruction for.
//!
//! Every refusal is reported by [`explain`], because a transform that silently
//! does nothing is indistinguishable from one that is not running.

use super::{Pass, PassOptions, PassStats};
use crate::v2::analysis::{CfgInfo, LoopForest};
use crate::v2::ir::*;
use crate::v2::vectorize::{self, LoopPlan, VecOptions};
use anyhow::Result;
use std::collections::{HashMap, HashSet};

/// Lanes per vector. Four 32-bit lanes is 128 bits, which every target ash
/// runs on has.
pub const VF: u32 = 4;

/// Why a *widenable* loop was still not widened. Distinct from
/// [`vectorize::Refusal`], which says why a loop was not widenable.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Decline {
    TripCountNotConstant,
    TripCountNotMultiple(i64),
    HasReduction,
    HasGuardExits,
    NonUnitStride(i64),
    TooSmall(usize),
    /// The loop body writes a value the widening would have to keep scalar
    /// and vector at once.
    MixedUse(ValueId),
}

/// Widen every loop that qualifies.
pub struct Widen<'m> {
    pub info: &'m dyn crate::v2::module::ModuleInfo,
}

impl Pass for Widen<'_> {
    fn name(&self) -> &'static str {
        "widen"
    }

    fn run(&self, f: &mut Function, _opts: &PassOptions) -> Result<PassStats> {
        let mut stats = PassStats::default();
        let opts = VecOptions::default();
        for plan in vectorize::analyze_with(f, &opts, &|i| self.info.int_value(i))
            .into_iter()
            .filter(|p| p.vectorizable())
        {
            if widen_loop(f, &plan, &opts, self.info).is_ok() {
                stats.replaced += 1;
            }
        }
        Ok(stats)
    }
}

/// What would happen to each widenable loop, without changing anything.
pub fn explain(
    f: &Function,
    info: &dyn crate::v2::module::ModuleInfo,
) -> Vec<(BlockId, Result<i64, Decline>)> {
    let opts = VecOptions::default();
    vectorize::analyze_with(f, &opts, &|i| info.int_value(i))
        .into_iter()
        .filter(|p| p.vectorizable())
        .map(|p| (p.header, check(f, &p, &opts, info)))
        .collect()
}

/// The constant an `Int` defines, resolved through the pool.
fn const_of(f: &Function, v: ValueId, info: &dyn crate::v2::module::ModuleInfo) -> Option<i64> {
    for blk in &f.blocks {
        for ins in &blk.instrs {
            if ins.dst() == Some(v) {
                return match ins {
                    Instr::Int { idx, .. } => f
                        .int_at(*idx, |i| info.int_value(i))
                        .map(|x| x as i64),
                    _ => None,
                };
            }
        }
    }
    None
}

/// The induction phi's value on the entry edge.
fn induction_start(
    f: &Function,
    plan: &LoopPlan,
    info: &dyn crate::v2::module::ModuleInfo,
) -> Option<i64> {
    let (iv, _) = plan.induction?;
    let phi = f.blocks[plan.header.idx()]
        .phis
        .iter()
        .find(|p| p.dst == iv)?;
    phi.incoming
        .iter()
        .find(|(pred, _)| *pred != plan.header)
        .and_then(|(_, val)| const_of(f, *val, info))
}

/// Trip count, when every term is a compile-time constant.
fn const_trip_count(
    f: &Function,
    plan: &LoopPlan,
    info: &dyn crate::v2::module::ModuleInfo,
) -> Option<i64> {
    let bound = plan.bound?;
    let (_, step) = plan.induction?;
    if step == 0 {
        return None;
    }
    let limit = const_of(f, bound.limit, info)?;
    let start = induction_start(f, plan, info)?;
    let span = limit - start;
    if span <= 0 || span % step != 0 {
        return None;
    }
    Some(span / step)
}

fn check(
    f: &Function,
    plan: &LoopPlan,
    opts: &VecOptions,
    info: &dyn crate::v2::module::ModuleInfo,
) -> Result<i64, Decline> {
    if !plan.reductions.is_empty() {
        return Err(Decline::HasReduction);
    }
    if !plan.guard_exits.is_empty() {
        return Err(Decline::HasGuardExits);
    }
    if plan.body_size < opts.min_body {
        return Err(Decline::TooSmall(plan.body_size));
    }
    if let Some(a) = plan.accesses.iter().find(|a| a.stride != 1) {
        return Err(Decline::NonUnitStride(a.stride));
    }
    let trips = const_trip_count(f, plan, info).ok_or(Decline::TripCountNotConstant)?;
    if trips % VF as i64 != 0 {
        return Err(Decline::TripCountNotMultiple(trips));
    }
    Ok(trips)
}

fn loop_blocks(f: &Function, header: BlockId) -> HashSet<BlockId> {
    let cfg = CfgInfo::build(f);
    let forest = LoopForest::analyze(f, &cfg);
    forest
        .innermost_first()
        .into_iter()
        .map(|l| forest.get(l))
        .find(|nl| nl.header == header)
        .map(|nl| nl.blocks.iter().copied().collect())
        .unwrap_or_default()
}

fn widen_loop(
    f: &mut Function,
    plan: &LoopPlan,
    opts: &VecOptions,
    info: &dyn crate::v2::module::ModuleInfo,
) -> Result<(), Decline> {
    check(f, plan, opts, info)?;
    let (iv, step) = plan.induction.ok_or(Decline::TripCountNotConstant)?;
    let body = loop_blocks(f, plan.header);

    // Which values become vectors: every loaded value, and transitively every
    // elementwise result computed from one.
    let mut widened: HashMap<ValueId, ValueId> = HashMap::new();
    for a in plan.accesses.iter().filter(|a| !a.is_store) {
        let (ty, reg) = (f.value_ty(a.at), f.value_reg(a.at));
        let w = f.new_vector_value(ty, reg, VF as u16);
        widened.insert(a.at, w);
    }
    loop {
        let mut grew = false;
        for b in &body {
            for ins in f.blocks[b.idx()].instrs.clone() {
                let Instr::BinOp { dst, a, b: rb, .. } = ins else {
                    continue;
                };
                if widened.contains_key(&dst) {
                    continue;
                }
                if widened.contains_key(&a) || widened.contains_key(&rb) {
                    let (ty, reg) = (f.value_ty(dst), f.value_reg(dst));
                    let w = f.new_vector_value(ty, reg, VF as u16);
                    widened.insert(dst, w);
                    grew = true;
                }
            }
        }
        if !grew {
            break;
        }
    }

    // The induction variable must stay scalar: it addresses the vector.
    if widened.contains_key(&iv) {
        return Err(Decline::MixedUse(iv));
    }

    // Stores the analysis accepted, by the value they write.
    let store_vals: HashSet<ValueId> = plan
        .accesses
        .iter()
        .filter(|a| a.is_store)
        .map(|a| a.at)
        .collect();
    for b in body.iter().copied().collect::<Vec<_>>() {
        let mut out: Vec<Instr> = Vec::new();
        for ins in f.blocks[b.idx()].instrs.clone() {
            match &ins {
                Instr::MemGet {
                    kind,
                    dst,
                    base,
                    index,
                } if widened.contains_key(dst) => out.push(Instr::VecLoad {
                    kind: *kind,
                    dst: widened[dst],
                    base: *base,
                    index: *index,
                    stride: lane_stride(*kind),
                }),
                Instr::MemSet {
                    kind,
                    base,
                    index,
                    src,
                } if store_vals.contains(src) => {
                    // A stored value that is not itself widened is loop
                    // invariant -- the analysis classified it, or the access
                    // would not be here -- so every lane gets the same one.
                    let ws = vector_operand(f, &mut out, &mut widened, *src);
                    out.push(Instr::VecStore {
                        kind: *kind,
                        base: *base,
                        index: *index,
                        src: ws,
                        stride: lane_stride(*kind),
                    });
                }
                Instr::BinOp { op, dst, a, b: rb } if widened.contains_key(dst) => {
                    let wa = vector_operand(f, &mut out, &mut widened, *a);
                    let wb = vector_operand(f, &mut out, &mut widened, *rb);
                    out.push(Instr::VecBinOp {
                        op: *op,
                        dst: widened[dst],
                        a: wa,
                        b: wb,
                    });
                }
                other => out.push(other.clone()),
            }
        }
        f.blocks[b.idx()].instrs = out;
    }

    retime_induction(f, plan, iv, step, info);
    Ok(())
}

/// The widened form of `v`, broadcasting a scalar where one meets a vector.
fn vector_operand(
    f: &mut Function,
    out: &mut Vec<Instr>,
    widened: &mut HashMap<ValueId, ValueId>,
    v: ValueId,
) -> ValueId {
    if let Some(w) = widened.get(&v) {
        return *w;
    }
    let (ty, reg) = (f.value_ty(v), f.value_reg(v));
    let w = f.new_vector_value(ty, reg, VF as u16);
    out.push(Instr::VecSplat { dst: w, src: v });
    widened.insert(v, w);
    w
}

/// Advance the induction by a whole vector per iteration.
///
/// Every address in the body was affine in it, so this is what makes the
/// widened loop cover the same range in a quarter of the trips.
fn retime_induction(
    f: &mut Function,
    plan: &LoopPlan,
    iv: ValueId,
    step: i64,
    info: &dyn crate::v2::module::ModuleInfo,
) {
    let header = plan.header;
    let Some(phi) = f.blocks[header.idx()]
        .phis
        .iter()
        .find(|p| p.dst == iv)
        .cloned()
    else {
        return;
    };
    // The back edge is the incoming whose predecessor is inside the loop; the
    // other one is the entry value.
    let body = loop_blocks(f, header);
    let Some(&(_, back)) = phi.incoming.iter().find(|(p, _)| body.contains(p)) else {
        return;
    };
    // `back` is `iv + step`; point its constant operand at `step * VF`.
    let want = (step * VF as i64) as i32;
    let new_idx = f.intern_int(want, |i| info.int_value(i));
    for b in 0..f.blocks.len() {
        let mut target: Option<ValueId> = None;
        for ins in &f.blocks[b].instrs {
            if ins.dst() == Some(back) {
                if let Instr::BinOp { op: BinOp::Add, b: rb, .. } = ins {
                    target = Some(*rb);
                }
            }
        }
        let Some(t) = target else { continue };
        for ins in &mut f.blocks[b].instrs {
            if ins.dst() == Some(t) {
                if let Instr::Int { idx, .. } = ins {
                    *idx = new_idx;
                }
            }
        }
        return;
    }
}

/// Index units between lanes, in whatever unit the access kind indexes by.
fn lane_stride(kind: MemAccess) -> u32 {
    match kind {
        MemAccess::I8 => 1,
        MemAccess::I16 => 2,
        // `Mem` indexes in bytes; `Array` in elements.
        MemAccess::Mem => 4,
        MemAccess::Array => 1,
    }
}
