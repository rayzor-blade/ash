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
//! NOT IN THE PIPELINE, deliberately. Measured over the benchmark corpus it
//! fires twice in total, both in deltablue, and never on nbody -- the loop it
//! was written for. It costs 1.10ms, 4.8% of pipeline time, on deltablue.
//!
//! What stops it: the loop's limit and the bounds check's limit are different
//! SSA values. nbody reads `bodies.length` once into `size` for the loop, and
//! the accessor reads the length again for its own check, and GVN does not
//! unify the two. This pass proves nothing about two values it cannot see are
//! equal, by design -- that is what makes it sound. Wire it in when the limits
//! arrive as one value, and it removes the guard; the tests pin both the
//! removal and the refusal.
//!
//! Integer operands only. `!(a < b)` is `a >= b` for integers and is not for
//! floats, where a NaN makes both false, so a float guard is never a fact.

use super::{def_sites, DefSite, Pass, PassOptions, PassStats};
use crate::v2::analysis::CfgInfo;
use crate::v2::ir::*;
use anyhow::Result;

/// Turns a `CondJump` whose answer is already known into a `Jump`.
pub struct RedundantGuardElim;

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

impl Pass for RedundantGuardElim {
    fn name(&self) -> &'static str {
        "redundant-guard-elim"
    }

    fn run(&self, f: &mut Function, _opts: &PassOptions) -> Result<PassStats> {
        let mut stats = PassStats::default();
        let cfg = CfgInfo::build(f);
        let defs = def_sites(f);
        let anchors = guard_anchors(f, &cfg, &defs);
        if anchors.is_empty() {
            return Ok(stats);
        }

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
            {
                if !f.is_float(f.value_ty(*a)) && !f.is_float(f.value_ty(*rhs)) {
                    let (ra, rb) = (copy_root(f, &defs, *a), copy_root(f, &defs, *rhs));
                    if known.contains(&(*cond, ra, rb)) {
                        decided.push((b, true));
                    } else if negate(*cond).is_some_and(|n| known.contains(&(n, ra, rb))) {
                        decided.push((b, false));
                    }
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
