//! Forward a cell's stored value to the loads that follow it.
//!
//! Lowering pins an address-taken register to a cell, which is a frame slot.
//! Every read of it is then a load, even when the value that was just written
//! is still sitting in a register — and in a hot loop that is a store and a
//! load per iteration for a value the machine never needed to spill. The
//! call benchmarks all show the shape:
//!
//! ```text
//! CellSet { cell: 0, src: v25 }
//! CellGet { dst: v12, cell: 0 }      // v12 IS v25
//! ```
//!
//! This replaces the loads with the stored value. The `CellSet` stays: the
//! cell may still be read through its address, and proving otherwise is
//! [`escape`](super::escape)'s job, not this pass's.
//!
//! # When forwarding is legal
//!
//! Only within one block, and the rule depends on whether the cell's address
//! is ever taken.
//!
//! For a cell that is never [`Instr::CellRef`]'d, nothing but `CellSet`,
//! `CellIncr` and `CellDecr` can change it, and all three are visible here.
//!
//! For a cell whose address IS taken, a callee could hold that address and
//! write through it, so the run additionally ends at anything that is not
//! pure or a plain read. That still covers the case that matters: the
//! accumulator loops store and immediately reload, with nothing in between,
//! and adjacency needs no aliasing argument at all — no instruction executes
//! between the write and the read, so the read must return what was written.
//! (These cells are address-taken exactly once, outside the loop, to print
//! the result; refusing the whole cell on that basis was too blunt.)
//!
//! Block boundaries end the run because a predecessor may have stored
//! something else; a cross-block version is mem2reg, and that is a larger
//! change than this earns.

use super::{compact_values, replace_all_uses, Pass, PassOptions, PassStats};
use crate::v2::ir::*;
use anyhow::Result;
use std::collections::{HashMap, HashSet};

pub struct CellForwarding;

impl Pass for CellForwarding {
    fn name(&self) -> &'static str {
        "cellfwd"
    }

    fn run(&self, f: &mut Function, _opts: &PassOptions) -> Result<PassStats> {
        let mut stats = PassStats::default();

        // Cells whose address is taken anywhere: forwarded under the
        // stricter rule below rather than skipped.
        let escaped: HashSet<u32> = f
            .blocks
            .iter()
            .flat_map(|b| b.instrs.iter())
            .filter_map(|i| match i {
                Instr::CellRef { cell, .. } => Some(cell.0),
                _ => None,
            })
            .collect();

        // (load, value it becomes), collected before any rewriting so the
        // walk reads a stable function.
        let mut forwards: Vec<(ValueId, ValueId)> = Vec::new();
        let mut dead: Vec<(usize, usize)> = Vec::new();

        for (bi, blk) in f.blocks.iter().enumerate() {
            let mut live: HashMap<u32, ValueId> = HashMap::new();
            for (k, ins) in blk.instrs.iter().enumerate() {
                // An escaped cell can be written through its address by
                // anything that calls or writes, so only pure work and plain
                // reads may sit between its store and the load.
                if ins.effect() > Effect::ReadMem {
                    live.retain(|c, _| !escaped.contains(c));
                }
                match ins {
                    Instr::CellSet { cell, src } => {
                        live.insert(cell.0, *src);
                    }
                    // The value changes and this pass does not model by how
                    // much, so the run ends rather than forwarding a stale
                    // value.
                    Instr::CellIncr { cell } | Instr::CellDecr { cell } => {
                        live.remove(&cell.0);
                    }
                    Instr::CellGet { dst, cell } => {
                        if let Some(&src) = live.get(&cell.0) {
                            // Types must agree: a cell's slot type and the
                            // loaded value's type are the same by
                            // construction, but a Copy-free rewrite has to
                            // hold that itself.
                            if f.value_ty(*dst) == f.value_ty(src) {
                                forwards.push((*dst, src));
                                dead.push((bi, k));
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        if forwards.is_empty() {
            return Ok(stats);
        }

        // A forward's target can itself be a forwarded load: a `CellSet`
        // whose `src` is a `CellGet` result (a `Copy` between two pinned
        // registers collapses to exactly that once GVN has run). Applying
        // (v2 -> v1) after (v1 -> vA) deleted v1's load would leave v2's
        // uses pointing at a value with no definition, so every forward is
        // resolved through the chain to a value whose definition survives
        // before anything is rewritten. Chains are acyclic: a load can only
        // be forwarded to a value defined before it in program order.
        let chain: HashMap<ValueId, ValueId> = forwards.iter().copied().collect();

        // A forwarded value's live range now reaches every use of the load,
        // so it needs a register of its own for de-SSA to stay correct —
        // the same rule SROA follows when it replaces a field read.
        let is_param = super::param_values(f);
        let mut claims = super::RegClaims::build(f);
        // Loads whose forward was applied (their `CellGet` dies), and loads
        // whose forward was refused by `privatize` (their `CellGet` stays and
        // remains a valid chain target).
        let mut applied_to: HashMap<ValueId, ValueId> = HashMap::new();
        let mut refused: HashSet<ValueId> = HashSet::new();
        let mut applied: Vec<(usize, usize)> = Vec::new();
        for (from, to) in forwards.iter().copied() {
            // Resolve `to` through the chain until a value whose definition
            // is certain to survive. Following a link that has not been
            // decided yet is sound: every link records a value equality, and
            // a link that later gets refused keeps its load — and with it
            // the definition — so stopping there would also have been fine.
            let mut target = to;
            loop {
                if refused.contains(&target) {
                    break; // load stays; target is defined
                }
                match applied_to.get(&target).or_else(|| chain.get(&target)) {
                    Some(&next) => target = next,
                    None => break, // not a forwarded load; definition survives
                }
            }
            if !super::privatize(f, target, &mut claims, is_param[target.idx()]) {
                refused.insert(from); // leave this one alone; the load stays
                continue;
            }
            stats.replaced += replace_all_uses(f, from, target);
            applied_to.insert(from, target);
            if let Some(pos) = dead
                .iter()
                .position(|&(b, k)| matches!(&f.blocks[b].instrs[k], Instr::CellGet { dst, .. } if *dst == from))
            {
                applied.push(dead[pos]);
            }
        }

        // Remove back-to-front so earlier indices stay valid.
        applied.sort_unstable();
        applied.dedup();
        for &(b, k) in applied.iter().rev() {
            f.blocks[b].instrs.remove(k);
            stats.eliminated += 1;
        }
        // The deleted loads' values must leave the table, or the verifier's
        // "every value is defined" totality check refuses the function even
        // though nothing uses them anymore.
        if stats.eliminated > 0 {
            compact_values(f)?;
        }
        Ok(stats)
    }
}
