//! Loop-invariant code motion.

use super::{
    def_sites, feeds_handler_phi, handler_blocks, param_values, privatize, Pass, PassOptions,
    PassStats, RegClaims,
};
use crate::v2::analysis::{clobbers_all, read_class, write_class, CfgInfo, LoopForest, LoopId};
use crate::v2::ir::*;
use anyhow::{bail, Result};
use std::collections::HashSet;

/// Incoming phi sources: `(predecessor block, value)` pairs.
type PhiSources = Vec<(BlockId, ValueId)>;

/// Hoists loop-invariant computations into a loop preheader.
///
/// Guarantees:
/// * only instructions that define a value and whose effect is `Pure` or
///   `ReadMem` are hoisted; writes, allocations, calls and anything that may
///   throw stay where they are, so exception order, allocation identity and
///   GC pressure are unchanged;
/// * a `ReadMem` load is hoisted only when nothing in the loop writes a
///   class that aliases it, nothing in the loop clobbers all memory, and its
///   block dominates every exiting block of the loop — the load then
///   provably executes at least once whenever the preheader does, so
///   speculating it cannot fault where the original could not;
/// * the preheader always has the same [`Block::handler`] as the hoisted
///   instruction's block, so nothing crosses a trap-region boundary;
/// * an existing single-entry predecessor is reused as the preheader when it
///   qualifies, otherwise one is created and the header's phis are split
///   across it; the CFG and dominance are recomputed after every structural
///   change;
/// * hoisted values get a private register ([`privatize`]) because their live
///   range now spans the whole loop.
pub struct LoopInvariantCodeMotion;

/// Bound on structural rounds; each round hoists one loop's worth of
/// instructions or creates one preheader.
const MAX_ROUNDS: usize = 64;

impl Pass for LoopInvariantCodeMotion {
    fn name(&self) -> &'static str {
        "licm"
    }

    fn run(&self, f: &mut Function, _opts: &PassOptions) -> Result<PassStats> {
        let mut stats = PassStats::default();
        let mut skip: HashSet<BlockId> = HashSet::new();

        for _ in 0..MAX_ROUNDS {
            let cfg = CfgInfo::build(f);
            let forest = LoopForest::analyze(f, &cfg);
            if forest.is_empty() {
                break;
            }
            let target = forest.innermost_first().into_iter().find(|&l| {
                !skip.contains(&forest.get(l).header) && has_candidate(f, &cfg, &forest, l)
            });
            let Some(l) = target else { break };
            let header = forest.get(l).header;

            let ph = match existing_preheader(f, &cfg, &forest, l) {
                Some(p) => p,
                None => match create_preheader(f, &cfg, &forest, l)? {
                    // The CFG changed; recompute before hoisting anything.
                    Some(_) => continue,
                    None => {
                        skip.insert(header);
                        continue;
                    }
                },
            };
            let n = hoist_into(f, &cfg, &forest, l, ph);
            if n == 0 {
                skip.insert(header);
            }
            stats.hoisted += n;
        }
        Ok(stats)
    }
}

/// True when the instruction may be moved to a preheader with handler
/// `ph_handler`, assuming its operands are already loop-invariant.
fn movable(
    f: &Function,
    cfg: &CfgInfo,
    forest: &LoopForest,
    l: LoopId,
    b: BlockId,
    k: usize,
) -> bool {
    let lp = forest.get(l);
    let ins = &f.blocks[b.idx()].instrs[k];
    if ins.dst().is_none() || matches!(ins, Instr::Param { .. }) || ins.may_throw() {
        return false;
    }
    match ins.effect() {
        Effect::Pure => true,
        Effect::ReadMem => {
            let Some(class) = read_class(ins) else {
                return false;
            };
            // No aliasing write and no full clobber anywhere in the loop.
            for &lb in &lp.blocks {
                for other in &f.blocks[lb.idx()].instrs {
                    if clobbers_all(other) {
                        return false;
                    }
                    if let Some(w) = write_class(other) {
                        if w.may_alias(class) {
                            return false;
                        }
                    }
                }
            }
            // Guaranteed to execute: dominates every way out of the loop.
            forest
                .exiting_blocks(cfg, l)
                .iter()
                .all(|&e| cfg.dominates(b, e))
        }
        _ => false,
    }
}

/// Operands all defined outside the loop.
fn operands_invariant(
    forest: &LoopForest,
    l: LoopId,
    defs: &[Option<super::DefSite>],
    ins: &Instr,
) -> bool {
    let lp = forest.get(l);
    ins.uses().into_iter().all(|u| match defs[u.idx()] {
        Some(d) => !lp.contains(d.block),
        None => false,
    })
}

fn has_candidate(f: &Function, cfg: &CfgInfo, forest: &LoopForest, l: LoopId) -> bool {
    let defs = def_sites(f);
    let handlers = handler_blocks(f);
    let lp = forest.get(l);
    for &b in &lp.blocks {
        for (k, ins) in f.blocks[b.idx()].instrs.iter().enumerate() {
            if !movable(f, cfg, forest, l, b, k) {
                continue;
            }
            if !operands_invariant(forest, l, &defs, ins) {
                continue;
            }
            let dst = ins.dst().expect("movable implies a dst");
            if feeds_handler_phi(f, &handlers, dst) {
                continue;
            }
            return true;
        }
    }
    false
}

/// Hoists loop allocations that cannot escape the iteration.
///
/// Separate from [`LoopInvariantCodeMotion`], and scheduled after it and after
/// [`super::sroa`], because the two compete for the same allocations and SROA's
/// outcome is strictly better: it removes the object, this only stops
/// re-creating it. SROA needs a later manager round to see past a loop's dead
/// header phi, and hoisting first would take the allocation before it got
/// there. Running last means this only ever sees what SROA has already
/// declined.
pub struct LoopAllocHoisting;

impl Pass for LoopAllocHoisting {
    fn name(&self) -> &'static str {
        "loop-alloc-hoist"
    }

    fn run(&self, f: &mut Function, _opts: &PassOptions) -> Result<PassStats> {
        let mut stats = PassStats::default();
        let mut skip: HashSet<BlockId> = HashSet::new();

        for _ in 0..MAX_ROUNDS {
            let cfg = CfgInfo::build(f);
            let forest = LoopForest::analyze(f, &cfg);
            if forest.is_empty() {
                break;
            }
            let target = forest.innermost_first().into_iter().find(|&l| {
                !skip.contains(&forest.get(l).header)
                    && !super::escape::hoistable_allocs(f, &forest, l).is_empty()
            });
            let Some(l) = target else { break };
            let header = forest.get(l).header;

            let ph = match existing_preheader(f, &cfg, &forest, l) {
                Some(p) => p,
                None => match create_preheader(f, &cfg, &forest, l)? {
                    Some(_) => continue,
                    None => {
                        skip.insert(header);
                        continue;
                    }
                },
            };
            let n = hoist_allocs_into(f, &forest, l, ph);
            if n == 0 {
                skip.insert(header);
            }
            stats.hoisted += n;
        }
        Ok(stats)
    }
}

/// Move allocations the loop cannot let escape into the preheader, so one
/// object serves every iteration instead of one per iteration.
///
/// The judgement is [`super::escape`]'s: the pointer must not outlive the
/// iteration, and every field the loop reads must be written first on every
/// path that reaches the read, so no iteration can observe what the previous
/// one left behind. What remains here is the mechanics — the same
/// `privatize` / remove / push as ordinary hoisting, and the same refusal to
/// cross a trap-region boundary.
///
/// The allocation is what moves; the field writes stay in the loop and
/// re-initialize the object each time round. That is what makes this cheaper
/// than scalar replacement, which has to understand the object rather than
/// merely outlive it.
pub(super) fn hoist_allocs_into(
    f: &mut Function,
    forest: &LoopForest,
    l: LoopId,
    ph: BlockId,
) -> usize {
    let ph_handler = f.blocks[ph.idx()].handler;
    let mut claims = RegClaims::build(f);
    let is_param = param_values(f);
    let mut moved = 0usize;

    loop {
        if matches!(
            f.blocks[ph.idx()].instrs.last(),
            Some(Instr::EndTrap { .. })
        ) {
            break;
        }
        // Recomputed each round: moving one allocation shifts the indices of
        // the rest, and takes it out of the loop so it is not offered twice.
        let handlers = handler_blocks(f);
        let cands = super::escape::hoistable_allocs(f, forest, l);
        let mut pick = None;
        for &(b, k) in &cands {
            if f.blocks[b.idx()].handler != ph_handler {
                continue;
            }
            let Some(dst) = f.blocks[b.idx()].instrs.get(k).and_then(|i| i.dst()) else {
                continue;
            };
            if feeds_handler_phi(f, &handlers, dst) {
                continue;
            }
            pick = Some((b, k, dst));
            break;
        }
        let Some((b, k, dst)) = pick else { break };
        if !privatize(f, dst, &mut claims, is_param[dst.idx()]) {
            break;
        }
        let ins = f.blocks[b.idx()].instrs.remove(k);
        f.blocks[ph.idx()].instrs.push(ins);
        moved += 1;
    }
    moved
}

/// Move every hoistable instruction of `l` into `ph`, to a fixed point.
fn hoist_into(
    f: &mut Function,
    cfg: &CfgInfo,
    forest: &LoopForest,
    l: LoopId,
    ph: BlockId,
) -> usize {
    let handlers = handler_blocks(f);
    let ph_handler = f.blocks[ph.idx()].handler;
    let mut claims = RegClaims::build(f);
    let is_param = param_values(f);
    let mut moved = 0usize;

    loop {
        if matches!(
            f.blocks[ph.idx()].instrs.last(),
            Some(Instr::EndTrap { .. })
        ) {
            break; // an EndTrap must stay the last instruction of its block
        }
        let defs = def_sites(f);
        let lp = forest.get(l).clone();
        let mut pick: Option<(BlockId, usize)> = None;
        'outer: for &b in &lp.blocks {
            if f.blocks[b.idx()].handler != ph_handler {
                continue; // would cross a trap-region boundary
            }
            for k in 0..f.blocks[b.idx()].instrs.len() {
                if !movable(f, cfg, forest, l, b, k) {
                    continue;
                }
                let ins = &f.blocks[b.idx()].instrs[k];
                if !operands_invariant(forest, l, &defs, ins) {
                    continue;
                }
                let dst = ins.dst().expect("movable implies a dst");
                if feeds_handler_phi(f, &handlers, dst) {
                    continue;
                }
                pick = Some((b, k));
                break 'outer;
            }
        }
        let Some((b, k)) = pick else { break };
        let dst = f.blocks[b.idx()].instrs[k].dst().expect("checked above");
        if !privatize(f, dst, &mut claims, is_param[dst.idx()]) {
            break;
        }
        let ins = f.blocks[b.idx()].instrs.remove(k);
        f.blocks[ph.idx()].instrs.push(ins);
        moved += 1;
    }
    moved
}

/// An existing block usable as the loop's preheader without changing the CFG.
pub(super) fn existing_preheader(
    f: &Function,
    cfg: &CfgInfo,
    forest: &LoopForest,
    l: LoopId,
) -> Option<BlockId> {
    let lp = forest.get(l);
    let entries = forest.entry_preds(cfg, l);
    if entries.len() != 1 {
        return None;
    }
    let p = entries[0];
    if lp.contains(p) {
        return None;
    }
    if !matches!(f.blocks[p.idx()].term, Terminator::Jump { target } if target == lp.header) {
        return None;
    }
    if f.blocks[p.idx()].handler != f.blocks[lp.header.idx()].handler {
        return None;
    }
    Some(p)
}

/// Insert a preheader in front of the loop header, moving every entry edge on
/// to it. Returns `None` when the loop's shape forbids it.
pub(super) fn create_preheader(
    f: &mut Function,
    cfg: &CfgInfo,
    forest: &LoopForest,
    l: LoopId,
) -> Result<Option<BlockId>> {
    let lp = forest.get(l).clone();
    let header = lp.header;
    let entries = forest.entry_preds(cfg, l);
    if entries.is_empty() {
        return Ok(None); // header is the function entry: no edge to split
    }
    // A handler block is entered through edges that have no program point.
    if handler_blocks(f)[header.idx()] {
        return Ok(None);
    }
    let h_handler = f.blocks[header.idx()].handler;
    for &p in &entries {
        if f.blocks[p.idx()].handler != h_handler {
            return Ok(None);
        }
        if matches!(f.blocks[p.idx()].term, Terminator::Trap { .. }) {
            return Ok(None);
        }
    }

    let ph = BlockId(f.blocks.len() as u32);
    let mut ph_phis: Vec<Phi> = Vec::new();

    // Split each header phi: entry sources move to the preheader, back-edge
    // sources stay.
    for pi in 0..f.blocks[header.idx()].phis.len() {
        let phi = f.blocks[header.idx()].phis[pi].clone();
        let (entry_srcs, back_srcs): (PhiSources, PhiSources) = phi
            .incoming
            .iter()
            .copied()
            .partition(|(p, _)| entries.contains(p));
        if entry_srcs.is_empty() {
            bail!("loop header b{} has a phi with no entry source", header.0);
        }
        let ph_value = if entry_srcs.len() == 1 {
            entry_srcs[0].1
        } else {
            let ty = f.value_ty(phi.dst);
            let reg = f.value_reg(phi.dst);
            let nv = f.new_value(ty, reg);
            ph_phis.push(Phi {
                dst: nv,
                incoming: entry_srcs.clone(),
            });
            nv
        };
        let mut incoming = vec![(ph, ph_value)];
        incoming.extend(back_srcs);
        f.blocks[header.idx()].phis[pi].incoming = incoming;
    }

    f.blocks.push(Block {
        phis: ph_phis,
        instrs: vec![],
        term: Terminator::Jump { target: header },
        handler: h_handler,
    });
    for &p in &entries {
        f.blocks[p.idx()].term.map_targets(&mut |t| {
            if t == header {
                ph
            } else {
                t
            }
        });
    }
    Ok(Some(ph))
}
