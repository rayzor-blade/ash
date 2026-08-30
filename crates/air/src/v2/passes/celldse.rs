//! Remove a `CellSet` whose value is overwritten before anything reads it.
//!
//! [`cellfwd`](super::cellfwd) removes the *load* that follows a store and
//! leaves the store, because proving a store dead needs more than the one
//! block it works in. This is that proof.
//!
//! The shape it exists for is an accumulator captured by a closure. Lowering
//! pins the address-taken register to a cell, so the loop stores to it every
//! iteration even though the value also flows through the phi network:
//!
//! ```text
//! b6: CallClosure { dst: v20, ... }
//!     CellSet     { cell: 0, src: v20 }     // nothing in the loop reads it
//! b7: CellSet     { cell: 0, src: v17 }     // re-established from the phi
//!     CellRef     { dst: v23, cell: 0 }     // and only now read
//! ```
//!
//! # What makes a store dead
//!
//! A backward analysis. A cell is *dead* at a point when every path from there
//! overwrites it before reading it, and a `CellSet` to a cell dead immediately
//! after it can be dropped.
//!
//! - `CellGet` and `CellRef` make a cell live; `CellIncr`/`CellDecr` read
//!   before writing, so they do too.
//! - A call can read a cell only if it could already hold that cell's address,
//!   so only calls reachable from a `CellRef` of that cell count. Treating
//!   every call as reading every address-taken cell is too blunt to be useful:
//!   the shape above takes the address once, after the loop, to print, and
//!   that alone would keep every store in the loop.
//! - At function exit every cell is dead. A cell is a frame slot, so a value
//!   left in one cannot be observed; an address outliving the frame is already
//!   dangling.
//!
//! Blocks meet by intersection, so one reading path keeps the store.
//!
//! # Why exception-bearing functions are refused
//!
//! A first version reasoned about exceptional edges directly, meeting with the
//! handler's entry set at each instruction that can throw. It miscompiled: a
//! value stored in a `try` and read in the `catch` was dropped, which the
//! parity matrix caught as "nested: inner caught" becoming "nested: none".
//! `Block::handler` on the storing block is not the whole exceptional CFG, and
//! a store-removing pass has to be right rather than close.

use std::collections::{HashMap, HashSet};

use super::{Pass, PassOptions, PassStats};
use crate::v2::ir::*;
use anyhow::Result;

pub struct DeadCellStoreElim;

/// For each cell whose address is taken, the blocks where a callee could
/// already hold that address: those reachable from a block that takes it.
fn address_reachable(f: &Function) -> HashMap<u32, HashSet<usize>> {
    let mut origins: HashMap<u32, Vec<usize>> = HashMap::new();
    for (bi, b) in f.blocks.iter().enumerate() {
        for i in &b.instrs {
            if let Instr::CellRef { cell, .. } = i {
                origins.entry(cell.0).or_default().push(bi);
            }
        }
    }
    let succs: Vec<Vec<usize>> = f
        .blocks
        .iter()
        .map(|b| b.term.successors().iter().map(|s| s.idx()).collect())
        .collect();
    let mut out: HashMap<u32, HashSet<usize>> = HashMap::new();
    for (cell, starts) in origins {
        let mut seen: HashSet<usize> = HashSet::new();
        let mut work = starts;
        while let Some(b) = work.pop() {
            if !seen.insert(b) {
                continue;
            }
            for &s in &succs[b] {
                work.push(s);
            }
        }
        out.insert(cell, seen);
    }
    out
}

fn may_call(i: &Instr) -> bool {
    matches!(
        i,
        Instr::Call { .. }
            | Instr::CallMethod { .. }
            | Instr::CallClosure { .. }
            | Instr::Intrinsic { .. }
    )
}

/// Cells the instructions actually mention. Read from the code rather than
/// `f.cells`, so the pass does not depend on how lowering sizes that table.
fn cells_used(f: &Function) -> HashSet<u32> {
    let mut all = HashSet::new();
    for b in &f.blocks {
        for i in &b.instrs {
            match i {
                Instr::CellSet { cell, .. }
                | Instr::CellGet { cell, .. }
                | Instr::CellRef { cell, .. }
                | Instr::CellIncr { cell, .. }
                | Instr::CellDecr { cell, .. } => {
                    all.insert(cell.0);
                }
                _ => {}
            }
        }
    }
    all
}

/// Step `dead` backwards over one block's instructions.
fn transfer(
    f: &Function,
    block: usize,
    mut dead: HashSet<u32>,
    reach: &HashMap<u32, HashSet<usize>>,
) -> HashSet<u32> {
    for i in f.blocks[block].instrs.iter().rev() {
        match i {
            Instr::CellSet { cell, .. } => {
                dead.insert(cell.0);
            }
            Instr::CellGet { cell, .. }
            | Instr::CellRef { cell, .. }
            | Instr::CellIncr { cell, .. }
            | Instr::CellDecr { cell, .. } => {
                dead.remove(&cell.0);
            }
            other if may_call(other) => {
                for (cell, blocks) in reach {
                    if blocks.contains(&block) {
                        dead.remove(cell);
                    }
                }
            }
            _ => {}
        }
    }
    dead
}

impl Pass for DeadCellStoreElim {
    fn name(&self) -> &'static str {
        "celldse"
    }

    fn run(&self, f: &mut Function, _opts: &PassOptions) -> Result<PassStats> {
        let mut stats = PassStats::default();
        // A/B switch: whether shrinking the IR pays for its own analysis is a
        // measurement, not an argument.
        if std::env::var_os("ASH_AIR_NO_CELLDSE").is_some() || f.blocks.is_empty() {
            return Ok(stats);
        }
        if f
            .blocks
            .iter()
            .any(|b| b.handler.is_some() || matches!(b.term, Terminator::Trap { .. }))
        {
            return Ok(stats);
        }
        let all_cells = cells_used(f);
        if all_cells.is_empty() {
            return Ok(stats);
        }
        let reach = address_reachable(f);
        let succs: Vec<Vec<usize>> = f
            .blocks
            .iter()
            .map(|b| b.term.successors().iter().map(|s| s.idx()).collect())
            .collect();

        let meet = |dead_in: &Vec<HashSet<u32>>, b: usize| -> HashSet<u32> {
            if succs[b].is_empty() {
                all_cells.clone()
            } else {
                let mut it = succs[b].iter();
                let first = dead_in[*it.next().unwrap()].clone();
                it.fold(first, |acc, s| &acc & &dead_in[*s])
            }
        };

        let mut dead_in: Vec<HashSet<u32>> = vec![all_cells.clone(); f.blocks.len()];
        for _ in 0..f.blocks.len() * 2 + 4 {
            let mut changed = false;
            for b in (0..f.blocks.len()).rev() {
                let out = transfer(f, b, meet(&dead_in, b), &reach);
                if out != dead_in[b] {
                    dead_in[b] = out;
                    changed = true;
                }
            }
            if !changed {
                break;
            }
        }

        for b in 0..f.blocks.len() {
            let mut dead = meet(&dead_in, b);
            let mut drop_at: Vec<usize> = Vec::new();
            for (ii, i) in f.blocks[b].instrs.iter().enumerate().rev() {
                match i {
                    Instr::CellSet { cell, .. } => {
                        if dead.contains(&cell.0) {
                            drop_at.push(ii);
                        }
                        dead.insert(cell.0);
                    }
                    Instr::CellGet { cell, .. }
                    | Instr::CellRef { cell, .. }
                    | Instr::CellIncr { cell, .. }
                    | Instr::CellDecr { cell, .. } => {
                        dead.remove(&cell.0);
                    }
                    other if may_call(other) => {
                        for (cell, blocks) in &reach {
                            if blocks.contains(&b) {
                                dead.remove(cell);
                            }
                        }
                    }
                    _ => {}
                }
            }
            for ii in drop_at {
                f.blocks[b].instrs.remove(ii);
                stats.eliminated += 1;
            }
        }
        Ok(stats)
    }
}
