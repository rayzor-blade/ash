//! Dead-code elimination.

use super::{compact_values, Pass, PassOptions, PassStats};
use crate::v2::ir::*;
use anyhow::Result;

/// Removes values that nothing reads and whose computation is unobservable.
///
/// Guarantees:
/// * an instruction is removed only when it defines a value with no uses and
///   its effect is at most [`Effect::Alloc`] — reads and allocations are
///   unobservable once dead, writes, throws and calls never are;
/// * `Param` instructions are kept: they are the function's entry-register
///   surface, not computation;
/// * phis with no uses are removed, which is what lets chains of dead loop
///   values disappear;
/// * runs to a fixed point, so a dead value feeding only other dead values
///   goes away in one pass.
pub struct DeadCodeElim;

fn is_dead(ins: &Instr, counts: &[usize]) -> bool {
    if matches!(ins, Instr::Param { .. }) {
        return false;
    }
    match ins.dst() {
        Some(d) => counts[d.idx()] == 0 && ins.effect() <= Effect::Alloc && !ins.may_throw(),
        None => false,
    }
}

impl Pass for DeadCodeElim {
    fn name(&self) -> &'static str {
        "dce"
    }

    fn run(&self, f: &mut Function, _opts: &PassOptions) -> Result<PassStats> {
        let mut stats = PassStats::default();
        loop {
            let counts = f.use_counts();
            let mut removed = 0usize;
            for blk in f.blocks.iter_mut() {
                let before = blk.instrs.len();
                blk.instrs.retain(|ins| !is_dead(ins, &counts));
                removed += before - blk.instrs.len();
                let before = blk.phis.len();
                blk.phis.retain(|p| counts[p.dst.idx()] > 0);
                removed += before - blk.phis.len();
            }
            // A cycle of phis feeding only each other never reaches use count
            // zero — lowering builds exactly that when it carries a register
            // around a loop nobody reads. Mark phis reachable from a non-phi
            // use and delete the rest of the graph.
            let mut incoming: std::collections::HashMap<ValueId, Vec<ValueId>> =
                std::collections::HashMap::new();
            for blk in &f.blocks {
                for p in &blk.phis {
                    incoming.insert(p.dst, p.incoming.iter().map(|&(_, v)| v).collect());
                }
            }
            let mut live: std::collections::HashSet<ValueId> = std::collections::HashSet::new();
            let mut work: Vec<ValueId> = Vec::new();
            let seed = |v: ValueId,
                        live: &mut std::collections::HashSet<ValueId>,
                        work: &mut Vec<ValueId>| {
                if incoming.contains_key(&v) && live.insert(v) {
                    work.push(v);
                }
            };
            for blk in &f.blocks {
                for ins in &blk.instrs {
                    for u in ins.uses() {
                        seed(u, &mut live, &mut work);
                    }
                }
                for u in blk.term.uses() {
                    seed(u, &mut live, &mut work);
                }
            }
            while let Some(v) = work.pop() {
                for &u in &incoming[&v] {
                    if incoming.contains_key(&u) && live.insert(u) {
                        work.push(u);
                    }
                }
            }
            for blk in f.blocks.iter_mut() {
                let before = blk.phis.len();
                blk.phis.retain(|p| live.contains(&p.dst));
                removed += before - blk.phis.len();
            }
            if removed == 0 {
                break;
            }
            stats.eliminated += removed;
        }
        if stats.eliminated > 0 {
            compact_values(f)?;
        }
        Ok(stats)
    }
}
