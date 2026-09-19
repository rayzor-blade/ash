//! Removing a comparison that a dominating branch already decided.
//!
//! `arr[i]` inside a loop over the array lowers to two tests on the same pair
//! of values: the loop's own exit test, and the bounds guard in front of the
//! element read. In nbody's inner loop that is a `cmp` and a branch to a throw
//! on every iteration, for a fact the loop header established a moment before.
//!
//! This is [`super::nullcheck`]'s shape with the condition generalised: a
//! `CondJump` proves its condition on the `if_true` edge and its negation on
//! the `if_false` edge, and both facts hold in every block the edge's target
//! dominates -- provided that target is entered only through that edge, or
//! another predecessor could arrive without the fact. A later `CondJump` on a
//! fact already known becomes an unconditional jump, and the block it stops
//! reaching is left to [`super::dce`].
//!
//! The bounds check is UNSIGNED -- `i <u len`, one test for both ends of
//! the range -- and the loop's exit test is signed, so the fact the loop
//! proves is `i <s len`, which is the guard's condition only when `i` is
//! not negative. That is a range fact about `i`, and this pass establishes
//! it for the loop counters HL emits: a header phi that starts at a
//! non-negative constant (or at a counter already proven, plus one) and
//! advances by one under a test that bounds it above, so it never wraps.
//! The increment has to be dominated by that test, which is what rules out
//! a counter that steps first and tests afterwards.
//!
//! Integer operands only. `!(a < b)` is `a >= b` for integers and is not for
//! floats, where a NaN makes both false, so a float guard is never a fact.
//!
//! Registered after GVN, which is what makes the loop's limit and the
//! guard's limit one value; the pass proves nothing about two values it
//! cannot see are equal. It answers the common case -- no comparison
//! repeated -- from a scan of the terminators alone, before building
//! anything.

use super::{DefSite, Pass, PassOptions, PassStats, def_sites};
use crate::v2::analysis::CfgInfo;
use crate::v2::ir::*;
use crate::v2::module::ModuleInfo;
use anyhow::Result;

/// Turns a `CondJump` whose answer is already known into a `Jump`.
pub struct RedundantGuardElim<'m> {
    /// For the constant pool: a counter's start has to be read.
    pub info: &'m dyn ModuleInfo,
}

/// A comparison known to hold: `cond(a, b)`.
type Fact = (CondKind, ValueId, ValueId);

/// The condition that is true exactly when `cond` is false.
///
/// Only the total orders. `NotLt` and `NotGte` are the unordered float forms
/// and are deliberately absent, as are the unary conditions.
fn negate(cond: CondKind) -> Option<CondKind> {
    Some(match cond {
        CondKind::SLt => CondKind::SGte,
        CondKind::SGte => CondKind::SLt,
        CondKind::SGt => CondKind::SLte,
        CondKind::SLte => CondKind::SGt,
        CondKind::ULt => CondKind::UGte,
        CondKind::UGte => CondKind::ULt,
        CondKind::Eq => CondKind::NotEq,
        CondKind::NotEq => CondKind::Eq,
        _ => return None,
    })
}

/// Follow `Copy` chains to the value that actually holds the operand, so a
/// guard and the check it proves agree even when one went through a move.
fn copy_root(f: &Function, defs: &[Option<DefSite>], mut v: ValueId) -> ValueId {
    for _ in 0..f.values.len() {
        let Some(d) = defs[v.idx()] else { return v };
        let Some(k) = d.instr_idx() else { return v };
        match &f.blocks[d.block.idx()].instrs[k] {
            Instr::Copy { src, .. } => v = *src,
            _ => return v,
        }
    }
    v
}

/// `(fact, block)` pairs where entering `block` proves `fact`.
fn guard_anchors(f: &Function, cfg: &CfgInfo, defs: &[Option<DefSite>]) -> Vec<(Fact, BlockId)> {
    let mut out = Vec::new();
    for (b, blk) in f.blocks.iter().enumerate() {
        let Terminator::CondJump {
            cond,
            a,
            b: Some(rhs),
            if_true,
            if_false,
        } = &blk.term
        else {
            continue;
        };
        let Some(inverse) = negate(*cond) else {
            continue;
        };
        if f.is_float(f.value_ty(*a)) || f.is_float(f.value_ty(*rhs)) {
            continue;
        }
        let (a, rhs) = (copy_root(f, defs, *a), copy_root(f, defs, *rhs));
        for (target, known) in [(*if_true, *cond), (*if_false, inverse)] {
            // Only when the target is entered exclusively through this edge;
            // otherwise another predecessor arrives without the fact.
            if cfg.preds[target.idx()].len() == 1 && cfg.preds[target.idx()][0].idx() == b {
                out.push(((known, a, rhs), target));
            }
        }
    }
    out
}

/// Whether any two `CondJump`s compare the same pair of integer values, in
/// either order. Nothing below can decide a comparison that is not repeated,
/// so a function without one is answered here, at the cost of a scan.
fn has_repeated_comparison(f: &Function) -> bool {
    let mut pairs: Vec<(ValueId, ValueId)> = Vec::new();
    for blk in &f.blocks {
        if let Terminator::CondJump {
            a, b: Some(rhs), ..
        } = &blk.term
            && !f.is_float(f.value_ty(*a))
        {
            let pair = if a.0 <= rhs.0 { (*a, *rhs) } else { (*rhs, *a) };
            pairs.push(pair);
        }
    }
    pairs.sort_unstable();
    pairs.windows(2).any(|w| w[0] == w[1])
}

/// The values proven never negative.
///
/// Constants that are not negative, and loop counters: a header phi whose
/// entry values are all proven and whose latch value is `phi + 1` computed
/// under a fact `phi <s x`, so the increment cannot wrap; and `v + 1`
/// itself, computed under `v <s x`, for a proven `v`. The last is how an
/// inner loop that starts at `i + 1` inherits the outer counter's range.
/// Run to a fixpoint because a counter's entry value may be another
/// counter's derived start.
fn non_negative(
    f: &Function,
    cfg: &CfgInfo,
    defs: &[Option<DefSite>],
    anchors: &[(Fact, BlockId)],
    info: &dyn ModuleInfo,
) -> Vec<bool> {
    let n = f.values.len();
    let mut known = vec![false; n];
    // `v + 1` from `src`, or `Incr src`, and where it is computed.
    let step_of = |v: ValueId| -> Option<(ValueId, BlockId)> {
        let d = defs[v.idx()]?;
        let k = d.instr_idx()?;
        match &f.blocks[d.block.idx()].instrs[k] {
            Instr::UnOp {
                op: UnOp::Incr,
                src,
                ..
            } => Some((copy_root(f, defs, *src), d.block)),
            Instr::BinOp {
                op: BinOp::Add,
                a,
                b,
                ..
            } => {
                let one = |c: ValueId| {
                    let d = defs[c.idx()]?;
                    let k = d.instr_idx()?;
                    match &f.blocks[d.block.idx()].instrs[k] {
                        Instr::Int { idx, .. } => f.int_at(*idx, |i| info.int_value(i)),
                        _ => None,
                    }
                };
                if one(*b) == Some(1) {
                    Some((copy_root(f, defs, *a), d.block))
                } else if one(*a) == Some(1) {
                    Some((copy_root(f, defs, *b), d.block))
                } else {
                    None
                }
            }
            _ => None,
        }
    };
    // `v <s x` for some `x` holds throughout `block`.
    let bounded_above_in = |v: ValueId, block: BlockId| -> bool {
        anchors.iter().any(|((cond, a, b), anchor)| {
            cfg.dominates(*anchor, block)
                && ((*cond == CondKind::SLt && *a == v) || (*cond == CondKind::SGt && *b == v))
        })
    };
    // A block is a loop header for a phi when one of the phi's predecessors
    // is dominated by the block: that edge is the back edge.
    loop {
        let mut changed = false;
        for (bi, blk) in f.blocks.iter().enumerate() {
            let here = BlockId(bi as u32);
            for phi in &blk.phis {
                if known[phi.dst.idx()] || f.is_float(f.value_ty(phi.dst)) {
                    continue;
                }
                let mut ok = !phi.incoming.is_empty();
                for (pred, v) in &phi.incoming {
                    let v = copy_root(f, defs, *v);
                    if cfg.dominates(here, *pred) {
                        // The latch: a step of one from this phi, taken only
                        // while the phi is bounded above.
                        ok &= step_of(v).is_some_and(|(src, at)| {
                            src == phi.dst && bounded_above_in(phi.dst, at)
                        });
                    } else {
                        ok &= known[v.idx()];
                    }
                }
                if ok {
                    known[phi.dst.idx()] = true;
                    changed = true;
                }
            }
            for ins in &blk.instrs {
                let Some(dst) = ins.dst() else { continue };
                if known[dst.idx()] || f.is_float(f.value_ty(dst)) {
                    continue;
                }
                let proven = match ins {
                    Instr::Int { idx, .. } => f
                        .int_at(*idx, |i| info.int_value(i))
                        .is_some_and(|c| c >= 0),
                    Instr::Copy { src, .. } => known[copy_root(f, defs, *src).idx()],
                    _ => step_of(dst)
                        .is_some_and(|(src, at)| known[src.idx()] && bounded_above_in(src, at)),
                };
                if proven {
                    known[dst.idx()] = true;
                    changed = true;
                }
            }
        }
        if !changed {
            return known;
        }
    }
}

impl Pass for RedundantGuardElim<'_> {
    fn name(&self) -> &'static str {
        "redundant-guard-elim"
    }

    fn run(&self, f: &mut Function, _opts: &PassOptions) -> Result<PassStats> {
        let mut stats = PassStats::default();
        if !has_repeated_comparison(f) {
            return Ok(stats);
        }
        let cfg = CfgInfo::build(f);
        let defs = def_sites(f);
        let anchors = guard_anchors(f, &cfg, &defs);
        if anchors.is_empty() {
            return Ok(stats);
        }
        let nonneg = non_negative(f, &cfg, &defs, &anchors, self.info);

        // Scoped dominator-tree walk, as in `nullcheck`: a fact holds in the
        // blocks its anchor dominates and nowhere else. A Vec rather than a
        // set because `CondKind` is not `Hash` and the live set on any path is
        // a handful of entries.
        let mut known: Vec<Fact> = Vec::new();
        let mut decided: Vec<(usize, bool)> = Vec::new();
        enum Item {
            Visit(usize),
            Undo(Vec<Fact>),
        }
        let mut walk = vec![Item::Visit(cfg.dom.rpo[0])];
        while let Some(item) = walk.pop() {
            let b = match item {
                Item::Undo(facts) => {
                    for fact in facts {
                        if let Some(at) = known.iter().rposition(|k| *k == fact) {
                            known.remove(at);
                        }
                    }
                    continue;
                }
                Item::Visit(b) => b,
            };
            let bid = BlockId(b as u32);
            let mut added: Vec<Fact> = Vec::new();
            for &(fact, anchor) in &anchors {
                if anchor == bid && !known.contains(&fact) {
                    known.push(fact);
                    added.push(fact);
                }
            }

            if let Terminator::CondJump {
                cond,
                a,
                b: Some(rhs),
                ..
            } = &f.blocks[b].term
                && !f.is_float(f.value_ty(*a))
                && !f.is_float(f.value_ty(*rhs))
            {
                let (ra, rb) = (copy_root(f, &defs, *a), copy_root(f, &defs, *rhs));
                // `a <s b` with `a` not negative puts `b` above zero too, and
                // the unsigned order agrees with the signed one on such a
                // pair: the bounds check `a <u b` is decided.
                let below = || {
                    nonneg[ra.idx()]
                        && (known.contains(&(CondKind::SLt, ra, rb))
                            || known.contains(&(CondKind::SGt, rb, ra)))
                };
                if known.contains(&(*cond, ra, rb)) {
                    decided.push((b, true));
                } else if negate(*cond).is_some_and(|n| known.contains(&(n, ra, rb))) {
                    decided.push((b, false));
                } else if *cond == CondKind::ULt && below() {
                    decided.push((b, true));
                } else if *cond == CondKind::UGte && below() {
                    decided.push((b, false));
                }
            }

            walk.push(Item::Undo(added));
            for &child in &cfg.dom.dom_children[b] {
                walk.push(Item::Visit(child));
            }
        }

        if decided.is_empty() {
            return Ok(stats);
        }
        for (b, take_true) in decided {
            let Terminator::CondJump {
                if_true, if_false, ..
            } = f.blocks[b].term
            else {
                continue;
            };
            let target = if take_true { if_true } else { if_false };
            f.blocks[b].term = Terminator::Jump { target };
            stats.eliminated += 1;
        }
        // The block the guard used to reach -- a throw, in the shape this
        // exists for -- is usually left with no predecessors, and `verify`
        // rejects an unreachable block. Nothing later prunes them: dce removes
        // dead VALUES. So the pass cleans up after itself.
        prune_unreachable(f);
        Ok(stats)
    }
}

/// Drop blocks no longer reachable from the entry, renumbering what remains
/// and dropping the phi arms that named them.
fn prune_unreachable(f: &mut Function) {
    let n = f.blocks.len();
    let mut seen = vec![false; n];
    let mut stack = vec![0usize];
    seen[0] = true;
    while let Some(b) = stack.pop() {
        let mut succs = f.blocks[b].term.successors();
        if let Some(h) = f.blocks[b].handler {
            succs.push(h);
        }
        for s in succs {
            if !seen[s.idx()] {
                seen[s.idx()] = true;
                stack.push(s.idx());
            }
        }
    }
    if seen.iter().all(|reached| *reached) {
        return;
    }

    let mut new_id = vec![None; n];
    let mut next = 0u32;
    for (b, reached) in seen.iter().enumerate() {
        if *reached {
            new_id[b] = Some(BlockId(next));
            next += 1;
        }
    }
    let remap = |b: BlockId| new_id[b.idx()].expect("successor of a reachable block is reachable");

    let old = std::mem::take(&mut f.blocks);
    f.blocks = old
        .into_iter()
        .enumerate()
        .filter(|(b, _)| seen[*b])
        .map(|(_, mut blk)| {
            for phi in &mut blk.phis {
                phi.incoming.retain(|(pred, _)| seen[pred.idx()]);
                for (pred, _) in &mut phi.incoming {
                    *pred = remap(*pred);
                }
            }
            blk.term.map_targets(&mut |t| remap(t));
            blk.handler = blk.handler.map(remap);
            blk
        })
        .collect();
    f.scalar_remainders = f
        .scalar_remainders
        .iter()
        .filter(|b| seen[b.idx()])
        .map(|b| remap(*b))
        .collect();
}
