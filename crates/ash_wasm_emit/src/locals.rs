//! Which wasm local holds each AIR value.
//!
//! A value gets a local of its own only if some other value of the same wasm
//! type is live where it is defined. Two values that are never live together
//! share one, which keeps a large function's local count near the number of
//! values live at once instead of the number ever defined.
//!
//! Interference is the usual definition for SSA: a value interferes with every
//! value live right after the instruction that defines it. The destinations
//! of a block's phis are written together on the entering edge, so they
//! interfere with each other and with everything live on entry.
use air::v2::analysis::CfgInfo;
use air::v2::ir::{BlockId, Function, ValueId};
use air::v2::liveness::Liveness;
use std::collections::BTreeSet;
use wasm_encoder::ValType;

use crate::Ty;

pub struct Assignment {
    /// The index, among the allocated locals, of each value's local. A value
    /// with no wasm representation points one past the last real local.
    pub slot: Vec<u32>,
    /// The type of each allocated local, ending with the one the
    /// representation-less values share.
    pub types: Vec<ValType>,
}

pub fn assign(f: &Function, cfg: &CfgInfo, tys: &[Ty]) -> Assignment {
    let n = f.values.len();
    let class = |v: usize| tys[v].wasm();
    let live = Liveness::analyze(f, cfg);

    let mut adj: Vec<Vec<u32>> = vec![Vec::new(); n];
    let mut edge = |a: ValueId, b: ValueId| {
        let (a, b) = (a.0 as usize, b.0 as usize);
        if a != b && class(a).is_some() && class(a) == class(b) {
            adj[a].push(b as u32);
            adj[b].push(a as u32);
        }
    };
    for (bi, blk) in f.blocks.iter().enumerate() {
        let mut now: BTreeSet<ValueId> = live.live_out(BlockId(bi as u32)).clone();
        now.extend(blk.term.uses());
        for ins in blk.instrs.iter().rev() {
            if let Some(d) = ins.dst() {
                for &l in &now {
                    edge(d, l);
                }
                now.remove(&d);
            }
            now.extend(ins.uses());
        }
        let phis: Vec<ValueId> = blk.phis.iter().map(|p| p.dst).collect();
        for (i, &d) in phis.iter().enumerate() {
            for &l in &now {
                edge(d, l);
            }
            for &other in &phis[i + 1..] {
                edge(d, other);
            }
        }
    }

    // Colour in program order, so that a value takes the lowest local the
    // values defined before it left free.
    let mut order: Vec<usize> = Vec::with_capacity(n);
    let mut seen = vec![false; n];
    let mut visit = |v: ValueId, order: &mut Vec<usize>| {
        let i = v.0 as usize;
        if !seen[i] {
            seen[i] = true;
            order.push(i);
        }
    };
    for b in cfg.rpo() {
        let blk = &f.blocks[b.idx()];
        for p in &blk.phis {
            visit(p.dst, &mut order);
        }
        for ins in &blk.instrs {
            if let Some(d) = ins.dst() {
                visit(d, &mut order);
            }
        }
    }
    for i in 0..n {
        visit(ValueId(i as u32), &mut order);
    }

    let none = u32::MAX;
    let mut slot = vec![none; n];
    let mut types: Vec<ValType> = Vec::new();
    let mut taken: Vec<u32> = Vec::new();
    let mut stamp = 0u32;
    for v in order {
        let Some(ty) = class(v) else { continue };
        stamp += 1;
        for &u in &adj[v] {
            let s = slot[u as usize];
            if s != none {
                taken[s as usize] = stamp;
            }
        }
        let k = (0..types.len())
            .find(|&k| types[k] == ty && taken[k] != stamp)
            .unwrap_or_else(|| {
                types.push(ty);
                taken.push(0);
                types.len() - 1
            });
        slot[v] = k as u32;
    }
    // Values with no representation never hold anything; they share one local.
    let dummy = types.len() as u32;
    types.push(ValType::I32);
    for s in &mut slot {
        if *s == none {
            *s = dummy;
        }
    }
    Assignment { slot, types }
}
