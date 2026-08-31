//! Which values are live where.
//!
//! AIR has never needed this: a value's register is the HL register it came
//! from, and de-SSA puts it back there, so nothing ever had to ask which
//! values interfere. Three things now do.
//!
//! * **OSR.** An entry resumes at a loop header and takes the live state as a
//!   buffer indexed by register. Several values share a register over a
//!   function's life, so "which value holds register 7" is only answerable at
//!   a program point. Inverting the value->register map globally picks an
//!   arbitrary one, which is a wrong answer rather than a slow one.
//! * **Register assignment.** Returning every value to its originating
//!   register means a value that cannot go home needs a fresh one: one large
//!   function serializes 19 registers into 236. Reuse needs interference, and
//!   interference needs this.
//! * **Dependence.** The vectorizer refuses over a thousand loops for
//!   loop-carried dependence, a question that starts here.
//!
//! # Phis
//!
//! A phi is a parallel copy on the edge, not an instruction in the block. Its
//! destination is defined at block entry, so it is not live-in; its sources
//! are live at the end of the *predecessor the edge comes from*, and only
//! that one. Treating a phi as an ordinary use would keep every source live
//! along every edge, which is wrong in exactly the case OSR cares about -- a
//! loop header, whose phis take one value from outside the loop and one from
//! the back edge.

use std::collections::BTreeSet;

use super::analysis::CfgInfo;
use super::ir::{BlockId, Function, ValueId};

/// Live-in and live-out sets, one per block.
#[derive(Debug, Clone)]
pub struct Liveness {
    live_in: Vec<BTreeSet<ValueId>>,
    live_out: Vec<BTreeSet<ValueId>>,
}

impl Liveness {
    /// Backward dataflow to a fixpoint.
    ///
    /// Reverse block order because a backward analysis converges fastest
    /// against the direction it flows; the loop runs to a fixpoint either way,
    /// so the order is a speed choice and not a correctness one.
    pub fn analyze(f: &Function, cfg: &CfgInfo) -> Self {
        let n = f.blocks.len();
        let mut live_in: Vec<BTreeSet<ValueId>> = vec![BTreeSet::new(); n];
        let mut live_out: Vec<BTreeSet<ValueId>> = vec![BTreeSet::new(); n];

        let mut changed = true;
        while changed {
            changed = false;
            for b in (0..n).rev() {
                let mut out: BTreeSet<ValueId> = BTreeSet::new();
                for &s in &cfg.succs[b] {
                    // The successor's live-in already excludes its own phi
                    // destinations, which are defined there rather than
                    // arriving from here.
                    out.extend(live_in[s.idx()].iter().copied());
                    for phi in &f.blocks[s.idx()].phis {
                        for &(pred, v) in &phi.incoming {
                            if pred.idx() == b {
                                out.insert(v);
                            }
                        }
                    }
                }

                let mut live = out.clone();
                live.extend(f.blocks[b].term.uses());
                for ins in f.blocks[b].instrs.iter().rev() {
                    if let Some(d) = ins.dst() {
                        live.remove(&d);
                    }
                    live.extend(ins.uses());
                }
                for phi in &f.blocks[b].phis {
                    live.remove(&phi.dst);
                }

                if live != live_in[b] || out != live_out[b] {
                    live_in[b] = live;
                    live_out[b] = out;
                    changed = true;
                }
            }
        }

        Liveness { live_in, live_out }
    }

    /// Values live on entry to `b`, i.e. defined before it and read at or
    /// after it. This is the set an OSR entry at `b` has to be handed.
    pub fn live_in(&self, b: BlockId) -> &BTreeSet<ValueId> {
        &self.live_in[b.idx()]
    }

    /// Values live on exit from `b`.
    pub fn live_out(&self, b: BlockId) -> &BTreeSet<ValueId> {
        &self.live_out[b.idx()]
    }
}
