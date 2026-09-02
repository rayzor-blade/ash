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
    /// A guard whose condition this cannot prove for the whole vector range.
    /// Hoisting needs the test to be the induction variable against something
    /// loop-invariant; anything else would be assumed rather than proven.
    UnprovableGuard(BlockId),
    /// No single entry edge to put the hoisted check on.
    NoPreheader,
    /// A remainder loop is needed and the induction does not step by one.
    EpilogueNeedsUnitStep(i64),
    /// The loop has a guard to hoist but no compile-time last index to prove
    /// it at.
    GuardNeedsConstantTrip,
    /// The loop's limit is recomputed inside the loop, so the preheader
    /// cannot compute the vector trip count from it.
    LimitNotInvariant,
    /// A byte-indexed access whose element width the embedder cannot name, so
    /// there is no way to tell a contiguous walk from a strided one.
    UnknownElementSize(TypeRef),
    NonUnitStride(i64),
    TooSmall(usize),
    /// The loop body writes a value the widening would have to keep scalar
    /// and vector at once.
    MixedUse(ValueId),
}

// What the TRANSFORM did, loop by loop, for the surveys.
//
// The analysis verdict and the transform's are different questions -- a loop
// can be vectorizable and still declined by `check` -- and re-running the
// analysis afterwards cannot recover the second, because a widened loop no
// longer looks like one the analysis would accept. So the pass records it.
thread_local! {
    static OUTCOMES: std::cell::RefCell<Vec<(BlockId, Result<(), Decline>)>> =
        const { std::cell::RefCell::new(Vec::new()) };
}

fn record(header: BlockId, r: Result<(), Decline>) {
    OUTCOMES.with(|o| o.borrow_mut().push((header, r)));
}

/// Every outcome recorded on this thread since the last call, and clear.
pub fn take_outcomes() -> Vec<(BlockId, Result<(), Decline>)> {
    OUTCOMES.with(|o| std::mem::take(&mut *o.borrow_mut()))
}

/// Widen every loop that qualifies.
pub struct Widen<'m> {
    pub info: &'m dyn crate::v2::module::ModuleInfo,
}

impl Pass for Widen<'_> {
    fn name(&self) -> &'static str {
        "widen"
    }

    fn run(&self, f: &mut Function, pass_opts: &PassOptions) -> Result<PassStats> {
        let mut stats = PassStats::default();
        if !pass_opts.widen {
            return Ok(stats);
        }
        let opts = VecOptions::default();
        let plans: Vec<_> = vectorize::analyze_with(f, &opts, &|i| self.info.int_value(i))
            .into_iter()
            .filter(|p| p.vectorizable())
            .filter(|p| !f.scalar_remainders.contains(&p.header))
            .collect();
        for plan in plans {
            // `widen_loop` edits as it goes and can refuse partway -- a guard
            // is proven and removed, then a later one turns out unprovable.
            // Half a widening is not a slower program, it is a wrong one, so
            // a refusal puts the function back exactly as it was.
            let backup = f.clone();
            match widen_loop(f, &plan, &opts, self.info) {
                Ok(()) => {
                    stats.replaced += 1;
                    record(plan.header, Ok(()));
                }
                Err(d) => {
                    *f = backup;
                    record(plan.header, Err(d));
                }
            }
        }
        Ok(stats)
    }
}

/// What would happen to each widenable loop, without changing anything.
pub fn explain(
    f: &Function,
    info: &dyn crate::v2::module::ModuleInfo,
) -> Vec<(BlockId, Result<Option<i64>, Decline>)> {
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
) -> Result<Option<i64>, Decline> {
    if !plan.reductions.is_empty() {
        return Err(Decline::HasReduction);
    }
    // Guards are hoistable, not fatal -- see `hoist_guards`. What is fatal is
    // a guard this cannot prove.
    for &g in &plan.guard_exits {
        recognize_guard(f, plan, g).ok_or(Decline::UnprovableGuard(g))?;
    }
    if plan.body_size < opts.min_body {
        return Err(Decline::TooSmall(plan.body_size));
    }
    // Contiguity, not the number 1. An `Array` access indexes in elements so
    // a step of 1 is back-to-back; every other kind indexes in BYTES, where
    // back-to-back is the element's own width -- HL scales those itself, so
    // `a[i]` on an Int array arrives here as `i << 2` and steps by 4.
    for a in &plan.accesses {
        match a.contiguous_stride(info.type_size(a.elem)) {
            Some(want) if want == a.stride => {}
            Some(_) => return Err(Decline::NonUnitStride(a.stride)),
            None => return Err(Decline::UnknownElementSize(a.elem)),
        }
    }
    // A constant trip count that divides the width needs no remainder
    // handling at all. Anything else gets a scalar epilogue, which is where
    // the leftover iterations run -- so a runtime length is no longer a
    // refusal, but it does require the step to be 1: the epilogue's entry
    // index is `start + (n & ~(VF-1))`, and that arithmetic is only this
    // simple for a unit step.
    match const_trip_count(f, plan, info) {
        Some(t) => {
            if t % VF as i64 != 0 {
                plan.bound.ok_or(Decline::TripCountNotConstant)?;
            }
            Ok(Some(t))
        }
        None => {
            let (_, step) = plan.induction.ok_or(Decline::TripCountNotConstant)?;
            if step != 1 {
                return Err(Decline::EpilogueNeedsUnitStep(step));
            }
            plan.bound.ok_or(Decline::TripCountNotConstant)?;
            Ok(None)
        }
    }
}

/// A guard this transform can prove for a whole vector range.
///
/// The test must be the induction variable against a loop-invariant value,
/// which is what an HL array bounds check is. Anything else -- a test on a
/// value that varies per iteration for another reason, a comparison of two
/// varying values -- would have to be ASSUMED monotone in the induction
/// variable, and assuming is how an unsound vectorizer happens.
struct Guard {
    block: BlockId,
    cond: CondKind,
    /// The side of the comparison that is the induction variable.
    iv_first: bool,
    limit: ValueId,
    /// Where the guard goes when it does NOT throw.
    ok: BlockId,
    /// Where it throws.
    throws: BlockId,
}

fn recognize_guard(f: &Function, plan: &LoopPlan, b: BlockId) -> Option<Guard> {
    let (iv, _) = plan.induction?;
    let Terminator::CondJump {
        cond,
        a,
        b: rb,
        if_true,
        if_false,
    } = &f.blocks[b.idx()].term
    else {
        return None;
    };
    let rb = (*rb)?;
    let body = loop_blocks(f, plan.header);
    // Exactly one side leaves the loop, and that side is the throw.
    let (ok, throws) = match (body.contains(if_true), body.contains(if_false)) {
        (true, false) => (*if_true, *if_false),
        (false, true) => (*if_false, *if_true),
        _ => return None,
    };
    let (iv_first, limit) = if *a == iv {
        (true, rb)
    } else if rb == iv {
        (false, *a)
    } else {
        return None;
    };
    // The limit must not move inside the loop.
    let moves = body.iter().any(|blk| {
        f.blocks[blk.idx()].instrs.iter().any(|i| i.dst() == Some(limit))
            || f.blocks[blk.idx()].phis.iter().any(|p| p.dst == limit)
    });
    if moves {
        return None;
    }
    Some(Guard {
        block: b,
        cond: *cond,
        iv_first,
        limit,
        ok,
        throws,
    })
}

/// Prove every guard once, before the loop, then take it out of the body.
///
/// A vector lane executes whether or not the scalar loop would have reached
/// it, so a guard left in the body proves nothing about lanes 1..N -- it is
/// tested against the induction variable, which now steps by a whole vector.
/// The check therefore moves to the preheader and is evaluated at the LAST
/// index the loop will reach: if it holds there it holds for every index the
/// loop touches, because the induction is monotone and the limit is
/// invariant.
fn hoist_guards(
    f: &mut Function,
    plan: &LoopPlan,
    guards: &[Guard],
    last_index: ValueId,
) -> Result<(), Decline> {
    for g in guards {
        // The preheader gets `if !guard(last) { throw }`, spliced in as the
        // entry edge's terminator.
        let cfg = CfgInfo::build(f);
        let forest = LoopForest::analyze(f, &cfg);
        let lid = forest
            .innermost_first()
            .into_iter()
            .find(|l| forest.get(*l).header == plan.header)
            .ok_or(Decline::NoPreheader)?;
        let preds = forest.entry_preds(&cfg, lid);
        let [pre] = preds[..] else {
            return Err(Decline::NoPreheader);
        };
        let Terminator::Jump { target } = f.blocks[pre.idx()].term else {
            return Err(Decline::NoPreheader);
        };
        // A fresh block carrying the hoisted test, between preheader and loop.
        let check = BlockId(f.blocks.len() as u32);
        let (a, b) = if g.iv_first {
            (last_index, Some(g.limit))
        } else {
            (g.limit, Some(last_index))
        };
        // Same handler as the preheader: the hoisted check sits on that edge
        // and must be covered by whatever trap region already covers it.
        let handler = f.blocks[pre.idx()].handler;
        f.blocks.push(Block {
            phis: Vec::new(),
            instrs: Vec::new(),
            handler,
            term: Terminator::CondJump {
                cond: g.cond,
                a,
                b,
                // Same polarity as the guard: the in-loop side continues.
                if_true: target,
                if_false: g.throws,
            },
        });
        f.blocks[pre.idx()].term = Terminator::Jump { target: check };
        // The header is now entered from the check block, not the preheader,
        // so its phis have to name the new predecessor.
        for phi in &mut f.blocks[target.idx()].phis {
            for (p, _) in phi.incoming.iter_mut() {
                if *p == pre {
                    *p = check;
                }
            }
        }
        // The guard is proven; the body branch becomes a straight edge.
        f.blocks[g.block.idx()].term = Terminator::Jump { target: g.ok };
        // The throw block lost a predecessor.
        for phi in &mut f.blocks[g.throws.idx()].phis {
            phi.incoming.retain(|(p, _)| *p != g.block);
        }
    }
    Ok(())
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
    let trips = check(f, plan, opts, info)?;
    let (iv, step) = plan.induction.ok_or(Decline::TripCountNotConstant)?;

    // A trip count that is not a known multiple of the width leaves a
    // remainder, and the remainder runs scalar. The copy has to be taken
    // BEFORE anything below touches the body -- it is a copy of the scalar
    // loop, guards and all.
    let epilogue = if trips.map_or(true, |t| t % VF as i64 != 0) {
        Some(prepare_epilogue(f, plan, iv)?)
    } else {
        None
    };

    // Guards first: a lane executes whether or not the scalar loop would have
    // reached it, so every bounds check has to be proven for the whole range
    // BEFORE the body is widened, and taken out of the body so it is not
    // re-tested against an induction variable that now steps by a vector.
    if !plan.guard_exits.is_empty() {
        let guards: Vec<Guard> = plan
            .guard_exits
            .iter()
            .map(|&b| recognize_guard(f, plan, b).ok_or(Decline::UnprovableGuard(b)))
            .collect::<Result<_, _>>()?;
        // The last index the loop reaches: start + (trips - 1) * step. A
        // constant, because `check` already established every term is one.
        let start = induction_start(f, plan, info).ok_or(Decline::TripCountNotConstant)?;
        // The hoisted test is evaluated at a specific index, so a loop whose
        // length is only known at runtime has no index to name here.
        let ct = trips.ok_or(Decline::GuardNeedsConstantTrip)?;
        let last = start + (ct - 1) * step;
        let idx = f.intern_int(last as i32, |i| info.int_value(i));
        let last_val = f.new_value(f.value_ty(iv), f.value_reg(iv));
        // Materialize it in the entry block, after the Params -- the verifier
        // requires those to come first.
        let at = f.blocks[0]
            .instrs
            .iter()
            .position(|i| !matches!(i, Instr::Param { .. }))
            .unwrap_or(f.blocks[0].instrs.len());
        f.blocks[0].instrs.insert(
            at,
            Instr::Int {
                dst: last_val,
                idx,
            },
        );
        hoist_guards(f, plan, &guards, last_val)?;
    }
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
    // The step each access actually walks, which `check` has already proven
    // is the contiguous one for its kind and element width. Taking it from
    // the analysis rather than a table keyed on the kind is what lets a
    // byte-indexed access carry its element's width instead of a guess.
    let access_stride: HashMap<ValueId, u32> = plan
        .accesses
        .iter()
        .map(|a| (a.at, a.stride.unsigned_abs() as u32))
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
                    stride: access_stride[dst],
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
                        stride: access_stride[src],
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
    if let Some(epi) = epilogue {
        wire_epilogue(f, plan, iv, epi, info)?;
    }
    Ok(())
}

/// What the epilogue needs to know, captured before the body is widened.
struct Epilogue {
    /// Every block as it was BEFORE widening -- the scalar remainder is a
    /// copy of this, not of what the transform leaves behind.
    snap: Vec<Block>,
    body: Vec<BlockId>,
    /// The block the loop is entered from.
    pre: BlockId,
    /// The induction's value on that edge, i.e. where counting starts.
    entry_iv: ValueId,
}

fn prepare_epilogue(f: &Function, plan: &LoopPlan, iv: ValueId) -> Result<Epilogue, Decline> {
    let bound = plan.bound.ok_or(Decline::TripCountNotConstant)?;
    let mut body: Vec<BlockId> = loop_blocks(f, plan.header).into_iter().collect();
    if body.is_empty() {
        return Err(Decline::NoPreheader);
    }
    body.sort_by_key(|b| b.0);

    // The vector trip count is computed in the preheader, so the limit has to
    // be a value that already exists there. A limit defined inside the loop
    // is not one, and using it would put a use before its definition.
    let limit_in_loop = body.iter().any(|b| {
        f.blocks[b.idx()].phis.iter().any(|p| p.dst == bound.limit)
            || f.blocks[b.idx()]
                .instrs
                .iter()
                .any(|i| i.dst() == Some(bound.limit))
    });
    if limit_in_loop {
        return Err(Decline::LimitNotInvariant);
    }

    let cfg = CfgInfo::build(f);
    let forest = LoopForest::analyze(f, &cfg);
    let lid = forest
        .innermost_first()
        .into_iter()
        .find(|l| forest.get(*l).header == plan.header)
        .ok_or(Decline::NoPreheader)?;
    let preds = forest.entry_preds(&cfg, lid);
    let [pre] = preds[..] else {
        return Err(Decline::NoPreheader);
    };

    let entry_iv = f.blocks[plan.header.idx()]
        .phis
        .iter()
        .find(|p| p.dst == iv)
        .and_then(|p| {
            p.incoming
                .iter()
                .find(|(b, _)| !body.contains(b))
                .map(|(_, v)| *v)
        })
        .ok_or(Decline::TripCountNotConstant)?;

    Ok(Epilogue {
        snap: f.blocks.clone(),
        body,
        pre,
        entry_iv,
    })
}

/// Bound the widened loop at `start + (n & ~(VF-1))` and run the leftover
/// iterations in a scalar copy of the body.
///
/// Without this a vectorizer only ever handles loops whose length divides the
/// width, which on real code is almost none of them -- the whole-program
/// survey found one widenable loop and declined it for exactly this. The
/// epilogue is also where a wrong widening stops being a missed optimization
/// and becomes a wrong answer, so the vector loop is bounded FIRST and the
/// copy simply continues from where it stopped.
fn wire_epilogue(
    f: &mut Function,
    plan: &LoopPlan,
    iv: ValueId,
    epi: Epilogue,
    info: &dyn crate::v2::module::ModuleInfo,
) -> Result<(), Decline> {
    let bound = plan.bound.ok_or(Decline::TripCountNotConstant)?;
    let Epilogue {
        snap,
        body,
        pre,
        entry_iv,
    } = epi;

    let ity = f.value_ty(iv);
    let ireg = f.value_reg(iv);
    let mask = f.intern_int(!(VF as i32 - 1), |i| info.int_value(i));

    // n = limit - start ; vn = n & ~(VF-1) ; vend = start + vn
    let n = f.new_value(ity, ireg);
    let mask_v = f.new_value(ity, ireg);
    let vn = f.new_value(ity, ireg);
    let vend = f.new_value(ity, ireg);
    f.blocks[pre.idx()].instrs.extend([
        Instr::BinOp {
            op: BinOp::Sub,
            dst: n,
            a: bound.limit,
            b: entry_iv,
        },
        Instr::Int {
            dst: mask_v,
            idx: mask,
        },
        Instr::BinOp {
            op: BinOp::And,
            dst: vn,
            a: n,
            b: mask_v,
        },
        Instr::BinOp {
            op: BinOp::Add,
            dst: vend,
            a: entry_iv,
            b: vn,
        },
    ]);

    // The widened loop now tests against `vend` rather than the real limit,
    // so it stops on a whole-vector boundary and never runs a partial one.
    let Terminator::CondJump {
        a,
        b,
        if_true,
        if_false,
        ..
    } = &mut f.blocks[bound.exit.idx()].term
    else {
        return Err(Decline::NoPreheader);
    };
    if bound.iv_first {
        *b = Some(vend);
    } else {
        *a = vend;
    }
    // Where the widened loop leaves is where the remainder begins.
    let (if_true, if_false) = (*if_true, *if_false);
    let normal_exit = if body.contains(&if_true) {
        if_false
    } else {
        if_true
    };

    let (bmap, vmap) = clone_blocks(f, &snap, &body);
    let epi_header = bmap[&plan.header];

    if let Terminator::CondJump {
        if_true, if_false, ..
    } = &mut f.blocks[bound.exit.idx()].term
    {
        if *if_true == normal_exit {
            *if_true = epi_header;
        } else {
            *if_false = epi_header;
        }
    }
    // The exit block lost that predecessor; it gains the copy's exit instead,
    // which `clone_blocks` has already recorded.
    for phi in &mut f.blocks[normal_exit.idx()].phis {
        phi.incoming.retain(|(p, _)| *p != bound.exit);
    }

    // It is a remainder, so it is shorter than a vector: widening it could
    // only produce another remainder, which is what four pass rounds did
    // before this was recorded.
    f.scalar_remainders.push(epi_header);

    // The copy is entered from the widened loop's exit, counting from `vend`.
    let iv_copy = vmap[&iv];
    for phi in &mut f.blocks[epi_header.idx()].phis {
        for (p, v) in phi.incoming.iter_mut() {
            if *p == pre {
                *p = bound.exit;
                if phi.dst == iv_copy {
                    *v = vend;
                }
            }
        }
    }
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

/// Copy a loop's blocks, giving every value and block a fresh identity.
///
/// The remainder loop is a second copy of the same body: the widened one runs
/// whole vectors, this one finishes what is left. Same remapping the inliner
/// does when it copies a callee, restricted to a set of blocks inside one
/// Copy a set of blocks, giving every value they define a fresh identity.
///
/// `snap` is the block table as it was when the copy was decided on, so the
/// copy reflects the loop before the transform edited it. Blocks outside the
/// set are shared, and so are the values they define -- those are the same
/// value in both copies.
fn clone_blocks(
    f: &mut Function,
    snap: &[Block],
    body: &[BlockId],
) -> (HashMap<BlockId, BlockId>, HashMap<ValueId, ValueId>) {
    let base = f.blocks.len();
    let bmap: HashMap<BlockId, BlockId> = body
        .iter()
        .enumerate()
        .map(|(i, &b)| (b, BlockId((base + i) as u32)))
        .collect();

    let mut vmap: HashMap<ValueId, ValueId> = HashMap::new();
    for &b in body {
        let defs: Vec<ValueId> = snap[b.idx()]
            .phis
            .iter()
            .map(|p| p.dst)
            .chain(snap[b.idx()].instrs.iter().filter_map(|i| i.dst()))
            .collect();
        for d in defs {
            let (ty, reg) = (f.value_ty(d), f.value_reg(d));
            let nv = f.new_value(ty, reg);
            vmap.insert(d, nv);
        }
    }

    let map_v = |v: ValueId, vmap: &HashMap<ValueId, ValueId>| *vmap.get(&v).unwrap_or(&v);
    let map_b = |b: BlockId, bmap: &HashMap<BlockId, BlockId>| *bmap.get(&b).unwrap_or(&b);

    let mut cloned: Vec<Block> = Vec::with_capacity(body.len());
    for &b in body {
        let src = &snap[b.idx()];
        let phis = src
            .phis
            .iter()
            .map(|p| Phi {
                dst: map_v(p.dst, &vmap),
                incoming: p
                    .incoming
                    .iter()
                    .map(|&(pb, v)| (map_b(pb, &bmap), map_v(v, &vmap)))
                    .collect(),
            })
            .collect();
        let mut instrs = src.instrs.clone();
        for ins in &mut instrs {
            ins.map_uses(&mut |v| map_v(v, &vmap));
            ins.map_dst(&mut |v| map_v(v, &vmap));
        }
        let mut term = src.term.clone();
        term.map_uses(&mut |v| map_v(v, &vmap));
        term.map_targets(&mut |t| map_b(t, &bmap));
        cloned.push(Block {
            phis,
            instrs,
            term,
            handler: src.handler,
        });
    }
    f.blocks.extend(cloned);

    // Blocks the copy leaves to now have an extra predecessor, and a phi that
    // does not name every predecessor is not SSA. The value on the new edge
    // is whatever the original edge carried, mapped into the copy.
    for &b in body {
        for t in snap[b.idx()].term.successors() {
            if bmap.contains_key(&t) {
                continue;
            }
            for phi in &mut f.blocks[t.idx()].phis {
                let Some(&(_, v)) = snap[t.idx()].phis.iter().find(|p| p.dst == phi.dst).and_then(
                    |p| p.incoming.iter().find(|(pb, _)| *pb == b),
                ) else {
                    continue;
                };
                phi.incoming.push((bmap[&b], map_v(v, &vmap)));
            }
        }
    }
    (bmap, vmap)
}
