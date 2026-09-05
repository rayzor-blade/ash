//! Structural verifier for AIR v2 functions, plus CFG comparison utilities
//! (condensation + isomorphism + dominance equivalence) used by the
//! round-trip property tests.
//!
//! `verify` checks:
//! * index validity of every value / block / cell reference,
//! * single definition per SSA value, every value defined,
//! * `Param` instructions only at the head of the entry block,
//! * definitions dominate uses (phi sources checked at the predecessor),
//! * phi arity: incoming set == predecessor set, one entry per predecessor,
//! * type consistency (phi/copy/cell/binop/unop operands),
//! * `EndTrap` only as the last instruction of a block,
//! * balanced trap regions via block-level trap-stack dataflow (multiple
//!   `EndTrap`s per `Trap` on different paths are fine; mismatched or
//!   unbalanced regions are rejected), and `handler` field consistency,
//! * all blocks reachable from the entry.

use super::ir::*;
use super::lower::build_domtree_from_succs;
use crate::dominance::DominatorTree;
use anyhow::{bail, Result};

pub fn verify(f: &Function) -> Result<()> {
    let nb = f.blocks.len();
    if nb == 0 {
        bail!("function has no blocks");
    }
    let nv = f.values.len();
    let nc = f.cells.len();

    let check_v = |v: ValueId, what: &str, b: usize| -> Result<()> {
        if v.idx() >= nv {
            bail!("b{}: {} references out-of-range value v{}", b, what, v.0);
        }
        Ok(())
    };
    let check_b = |t: BlockId, what: &str, b: usize| -> Result<()> {
        if t.idx() >= nb {
            bail!("b{}: {} targets out-of-range block b{}", b, what, t.0);
        }
        Ok(())
    };
    let check_c = |c: CellId, what: &str, b: usize| -> Result<()> {
        if c.idx() >= nc {
            bail!("b{}: {} references out-of-range cell c{}", b, what, c.0);
        }
        Ok(())
    };

    // ---- definitions: single-def, everything defined ----------------------
    // Position encoding within a block: phis at 0, instr k at k+1,
    // terminator at instrs.len()+1.
    let mut def: Vec<Option<(usize, usize)>> = vec![None; nv];
    for (b, blk) in f.blocks.iter().enumerate() {
        for phi in &blk.phis {
            check_v(phi.dst, "phi dst", b)?;
            if def[phi.dst.idx()].replace((b, 0)).is_some() {
                bail!("value v{} defined more than once", phi.dst.0);
            }
            for &(p, v) in &phi.incoming {
                check_b(p, "phi incoming pred", b)?;
                check_v(v, "phi incoming value", b)?;
            }
        }
        for (k, ins) in blk.instrs.iter().enumerate() {
            if let Some(d) = ins.dst() {
                check_v(d, "instr dst", b)?;
                if def[d.idx()].replace((b, k + 1)).is_some() {
                    bail!("value v{} defined more than once", d.0);
                }
            }
            for u in ins.uses() {
                check_v(u, "instr operand", b)?;
            }
            match ins {
                Instr::CellGet { cell, .. }
                | Instr::CellSet { cell, .. }
                | Instr::CellIncr { cell }
                | Instr::CellDecr { cell }
                | Instr::CellRef { cell, .. }
                | Instr::EndTrap { cell, .. } => check_c(*cell, "cell op", b)?,
                _ => {}
            }
        }
        for u in blk.term.uses() {
            check_v(u, "terminator operand", b)?;
        }
        for t in blk.term.successors() {
            check_b(t, "terminator", b)?;
        }
        if let Some(h) = blk.handler {
            check_b(h, "handler", b)?;
        }
        if let Terminator::Trap { exc_cell, .. } = blk.term {
            check_c(exc_cell, "trap exc", b)?;
        }
        if let Terminator::CondJump { cond, b: rhs, .. } = &blk.term {
            if cond.is_unary() != rhs.is_none() {
                bail!("b{}: CondJump {:?} operand arity mismatch", b, cond);
            }
        }
    }
    for (v, d) in def.iter().enumerate() {
        if d.is_none() {
            bail!("value v{} is never defined", v);
        }
    }

    // ---- Param placement --------------------------------------------------
    for (b, blk) in f.blocks.iter().enumerate() {
        let mut seen_non_param = false;
        let mut param_regs: Vec<u32> = vec![];
        for ins in &blk.instrs {
            if let Instr::Param { reg, .. } = ins {
                if b != 0 {
                    bail!("Param in non-entry block b{}", b);
                }
                if seen_non_param {
                    bail!("Param after non-Param instruction in entry block");
                }
                if param_regs.contains(reg) {
                    bail!("duplicate Param for r{}", reg);
                }
                if (*reg as usize) >= f.reg_types.len() {
                    bail!("Param register r{} out of range", reg);
                }
                if f.cells.iter().any(|c| c.reg == *reg) {
                    bail!("Param for pinned register r{}", reg);
                }
                param_regs.push(*reg);
            } else {
                seen_non_param = true;
            }
        }
        if !blk.phis.is_empty() && b == 0 {
            bail!("entry block must not have phis");
        }
    }

    // ---- CFG, reachability, dominance -------------------------------------
    let succs: Vec<Vec<usize>> = (0..nb)
        .map(|b| {
            f.succ_blocks(BlockId(b as u32))
                .into_iter()
                .map(|s| s.idx())
                .collect()
        })
        .collect();
    let dom = build_domtree_from_succs(&succs);
    for (b, &rpo) in dom.rpo_number.iter().enumerate().take(nb) {
        if rpo == usize::MAX {
            bail!("block b{} is unreachable from the entry", b);
        }
    }
    let preds = f.preds();

    // ---- phi arity / incoming sets ----------------------------------------
    for (b, blk) in f.blocks.iter().enumerate() {
        let pset = &preds[b];
        for phi in &blk.phis {
            if phi.incoming.len() != pset.len() {
                bail!(
                    "b{}: phi v{} arity {} but block has {} predecessors",
                    b,
                    phi.dst.0,
                    phi.incoming.len(),
                    pset.len()
                );
            }
            let mut seen: Vec<BlockId> = vec![];
            for &(p, _) in &phi.incoming {
                if seen.contains(&p) {
                    bail!(
                        "b{}: phi v{} has duplicate incoming pred b{}",
                        b,
                        phi.dst.0,
                        p.0
                    );
                }
                if !pset.contains(&p) {
                    bail!(
                        "b{}: phi v{} incoming from non-predecessor b{}",
                        b,
                        phi.dst.0,
                        p.0
                    );
                }
                seen.push(p);
            }
        }
    }

    // ---- def-dominates-use -------------------------------------------------
    let dominates = |da: (usize, usize), b: usize, pos: usize| -> bool {
        let (db, dp) = da;
        if db == b {
            dp < pos
        } else {
            dom.dominates(db, b)
        }
    };
    for (b, blk) in f.blocks.iter().enumerate() {
        for (k, ins) in blk.instrs.iter().enumerate() {
            for u in ins.uses() {
                let d = def[u.idx()].unwrap();
                if !dominates(d, b, k + 1) {
                    bail!("b{}: use of v{} not dominated by its definition", b, u.0);
                }
            }
        }
        let tpos = blk.instrs.len() + 1;
        for u in blk.term.uses() {
            let d = def[u.idx()].unwrap();
            if !dominates(d, b, tpos) {
                bail!(
                    "b{}: terminator use of v{} not dominated by definition",
                    b,
                    u.0
                );
            }
        }
        for phi in &blk.phis {
            for &(p, v) in &phi.incoming {
                let (db, _) = def[v.idx()].unwrap();
                // Must dominate the END of the predecessor block.
                if db != p.idx() && !dom.dominates(db, p.idx()) {
                    bail!(
                        "b{}: phi v{} source v{} (from b{}) not dominated by its def",
                        b,
                        phi.dst.0,
                        v.0,
                        p.0
                    );
                }
            }
        }
    }

    // ---- type consistency --------------------------------------------------
    for (b, blk) in f.blocks.iter().enumerate() {
        for phi in &blk.phis {
            let ty = f.value_ty(phi.dst);
            for &(_, v) in &phi.incoming {
                if f.value_ty(v) != ty {
                    bail!(
                        "b{}: phi v{} type {:?} != incoming v{} type {:?}",
                        b,
                        phi.dst.0,
                        ty,
                        v.0,
                        f.value_ty(v)
                    );
                }
            }
        }
        for ins in &blk.instrs {
            match ins {
                Instr::Copy { dst, src } if f.value_ty(*dst) != f.value_ty(*src) => {
                    bail!("b{}: Copy between different types", b);
                }
                Instr::BinOp { dst, a, b: rb, .. } => {
                    let ty = f.value_ty(*dst);
                    if f.value_ty(*a) != ty || f.value_ty(*rb) != ty {
                        bail!("b{}: BinOp operand/result type mismatch", b);
                    }
                }
                Instr::Fma { dst, a, b: mb, c } => {
                    let ty = f.value_ty(*dst);
                    if f.value_ty(*a) != ty || f.value_ty(*mb) != ty || f.value_ty(*c) != ty {
                        bail!("b{}: Fma operand/result type mismatch", b);
                    }
                    if !f.is_float(ty) {
                        bail!("b{}: Fma on non-float type {:?}", b, ty);
                    }
                }
                Instr::UnOp { dst, src, .. } if f.value_ty(*dst) != f.value_ty(*src) => {
                    bail!("b{}: UnOp operand/result type mismatch", b);
                }
                Instr::CellGet { dst, cell } if f.value_ty(*dst) != f.cells[cell.idx()].ty => {
                    bail!("b{}: CellGet type mismatch", b);
                }
                Instr::CellSet { cell, src } if f.value_ty(*src) != f.cells[cell.idx()].ty => {
                    bail!("b{}: CellSet type mismatch", b);
                }
                // ---- vector forms ---------------------------------------
                // Width is carried on the value, so it is the verifier's job
                // to keep it agreeing: a scalar reaching a lane-wise operand
                // is the mistake that would otherwise reach a backend and
                // become a wrong-width machine instruction.
                Instr::VecLoad { dst, index, .. } => {
                    if f.value_lanes(*dst) < 2 {
                        bail!("b{}: VecLoad into scalar v{}", b, dst.0);
                    }
                    if f.value_lanes(*index) != 1 {
                        bail!("b{}: VecLoad index must be scalar", b);
                    }
                }
                Instr::VecStore { src, index, .. } => {
                    if f.value_lanes(*src) < 2 {
                        bail!("b{}: VecStore of scalar v{}", b, src.0);
                    }
                    if f.value_lanes(*index) != 1 {
                        bail!("b{}: VecStore index must be scalar", b);
                    }
                }
                Instr::VecSplat { dst, src } => {
                    if f.value_lanes(*src) != 1 {
                        bail!("b{}: VecSplat source must be scalar", b);
                    }
                    if f.value_lanes(*dst) < 2 {
                        bail!("b{}: VecSplat into scalar v{}", b, dst.0);
                    }
                    if f.value_ty(*dst) != f.value_ty(*src) {
                        bail!("b{}: VecSplat element type mismatch", b);
                    }
                }
                Instr::VecBinOp { dst, a, b: rb, .. } => {
                    let lanes = f.value_lanes(*dst);
                    if lanes < 2 {
                        bail!("b{}: VecBinOp into scalar v{}", b, dst.0);
                    }
                    if f.value_lanes(*a) != lanes || f.value_lanes(*rb) != lanes {
                        bail!("b{}: VecBinOp lane count mismatch", b);
                    }
                    let ty = f.value_ty(*dst);
                    if f.value_ty(*a) != ty || f.value_ty(*rb) != ty {
                        bail!("b{}: VecBinOp element type mismatch", b);
                    }
                }
                Instr::VecReduce { dst, src, .. } => {
                    if f.value_lanes(*src) < 2 {
                        bail!("b{}: VecReduce of scalar v{}", b, src.0);
                    }
                    if f.value_lanes(*dst) != 1 {
                        bail!("b{}: VecReduce must produce a scalar", b);
                    }
                    if f.value_ty(*dst) != f.value_ty(*src) {
                        bail!("b{}: VecReduce element type mismatch", b);
                    }
                }
                _ => {}
            }
        }
    }

    // ---- EndTrap placement -------------------------------------------------
    for (b, blk) in f.blocks.iter().enumerate() {
        for (k, ins) in blk.instrs.iter().enumerate() {
            if matches!(ins, Instr::EndTrap { .. }) && k + 1 != blk.instrs.len() {
                bail!("b{}: EndTrap must be the last instruction of its block", b);
            }
        }
    }

    // ---- balanced trap regions (block-level dataflow) ----------------------
    verify_trap_regions(f, &succs)?;

    // ---- module declarations -----------------------------------------------
    for imp in f.natives.iter() {
        if imp.name.is_empty() {
            bail!("native declaration for findex {} has no name", imp.findex);
        }
        if f.natives.get(imp.findex) != Some(imp) {
            bail!("native table is not keyed consistently by findex");
        }
    }
    if f.float_types.windows(2).any(|w| w[0] >= w[1]) {
        bail!("float type table must be sorted and duplicate-free");
    }

    Ok(())
}

/// Block-level trap-stack dataflow. Each stack entry is
/// `(handler block, exception cell)` of an open region.
fn verify_trap_regions(f: &Function, _succs: &[Vec<usize>]) -> Result<()> {
    let nb = f.blocks.len();
    type Stack = Vec<(usize, u32)>;
    let mut at: Vec<Option<Stack>> = vec![None; nb];
    at[0] = Some(vec![]);
    let mut wl = vec![0usize];

    while let Some(b) = wl.pop() {
        let s0 = at[b].clone().unwrap();
        let blk = &f.blocks[b];

        // handler field must match the innermost open region.
        let expect = s0.last().map(|&(h, _)| h);
        if blk.handler.map(|h| h.idx()) != expect {
            bail!(
                "b{}: handler field {:?} inconsistent with trap dataflow ({:?})",
                b,
                blk.handler.map(|h| h.idx()),
                expect
            );
        }

        let mut propagate = |t: usize, st: Stack, wl: &mut Vec<usize>| -> Result<()> {
            match &at[t] {
                None => {
                    at[t] = Some(st);
                    wl.push(t);
                }
                Some(prev) => {
                    if *prev != st {
                        bail!("inconsistent trap stacks merging into b{}", t);
                    }
                }
            }
            Ok(())
        };

        // Exceptional edge: uses the entry stack (throw happens before any
        // trailing EndTrap executes) and pops the innermost region.
        if let Some(h) = blk.handler {
            if f.block_may_throw(BlockId(b as u32)) {
                let mut st = s0.clone();
                let (top_h, _) = st.pop().unwrap();
                if top_h != h.idx() {
                    bail!("b{}: exceptional edge does not target innermost handler", b);
                }
                propagate(h.idx(), st, &mut wl)?;
            }
        }

        // Normal flow: apply a trailing EndTrap, then the terminator.
        let mut s1 = s0.clone();
        if let Some(Instr::EndTrap { cell, .. }) = blk.instrs.last() {
            match s1.pop() {
                None => bail!("b{}: EndTrap with no open trap region", b),
                Some((_, exc)) => {
                    if exc != cell.0 {
                        bail!(
                            "b{}: EndTrap cell c{} does not match innermost trap cell c{}",
                            b,
                            cell.0,
                            exc
                        );
                    }
                }
            }
        }
        match &blk.term {
            Terminator::Trap {
                exc_cell,
                handler,
                normal,
            } => {
                let mut pushed = s1.clone();
                pushed.push((handler.idx(), exc_cell.0));
                propagate(normal.idx(), pushed, &mut wl)?;
                propagate(handler.idx(), s1.clone(), &mut wl)?;
            }
            Terminator::Ret { .. } => {
                if !s1.is_empty() {
                    bail!("b{}: Ret inside an open trap region", b);
                }
            }
            Terminator::Throw { .. } | Terminator::Rethrow { .. } => {}
            t => {
                for succ in t.successors() {
                    propagate(succ.idx(), s1.clone(), &mut wl)?;
                }
            }
        }
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// CFG comparison: condensation, isomorphism, dominance equivalence
// ---------------------------------------------------------------------------

/// A condensed CFG: straight-line chains (single-succ -> single-pred edges)
/// are contracted to one node. Robust against serialization dropping
/// redundant `JAlways +0`-style jumps.
pub struct CondensedCfg {
    /// Condensed node id for each original block (dense, entry is node 0).
    pub node_of: Vec<usize>,
    /// Ordered successor lists per condensed node.
    pub succs: Vec<Vec<usize>>,
    pub num_nodes: usize,
}

pub fn condense_cfg(f: &Function) -> CondensedCfg {
    let nb = f.blocks.len();
    let succs: Vec<Vec<usize>> = (0..nb)
        .map(|b| {
            f.succ_blocks(BlockId(b as u32))
                .into_iter()
                .map(|s| s.idx())
                .collect()
        })
        .collect();
    let mut preds: Vec<Vec<usize>> = vec![vec![]; nb];
    for (b, ss) in succs.iter().enumerate() {
        for &s in ss {
            if !preds[s].contains(&b) {
                preds[s].push(b);
            }
        }
    }
    // b is absorbed into its unique pred p when p's unique succ is b.
    let absorbed = |b: usize| -> Option<usize> {
        if b == 0 {
            return None;
        }
        if preds[b].len() != 1 {
            return None;
        }
        let p = preds[b][0];
        if p == b {
            return None;
        }
        if succs[p].len() == 1 && succs[p][0] == b {
            Some(p)
        } else {
            None
        }
    };
    let mut leader = vec![usize::MAX; nb];
    for (b, l) in leader.iter_mut().enumerate() {
        let mut cur = b;
        let mut seen = vec![b];
        while let Some(p) = absorbed(cur) {
            if seen.contains(&p) {
                break; // defensive: pathological cycle
            }
            seen.push(p);
            cur = p;
        }
        *l = cur;
    }
    // Compact node ids, entry (leader of block 0) first, then ascending.
    let mut leaders: Vec<usize> = (0..nb).filter(|&b| leader[b] == b).collect();
    leaders.sort_unstable();
    let entry_leader = leader[0];
    leaders.retain(|&l| l != entry_leader);
    leaders.insert(0, entry_leader);
    let mut node_id = vec![usize::MAX; nb];
    for (id, &l) in leaders.iter().enumerate() {
        node_id[l] = id;
    }
    let node_of: Vec<usize> = (0..nb).map(|b| node_id[leader[b]]).collect();

    // Successor lists: follow each leader's chain to its tail, then map the
    // tail's successors.
    let mut csuccs: Vec<Vec<usize>> = vec![vec![]; leaders.len()];
    for (id, &l) in leaders.iter().enumerate() {
        let mut tail = l;
        loop {
            let next = if succs[tail].len() == 1 {
                let s = succs[tail][0];
                if leader[s] == l && s != l && absorbed(s) == Some(tail) {
                    Some(s)
                } else {
                    None
                }
            } else {
                None
            };
            match next {
                Some(s) => tail = s,
                None => break,
            }
        }
        let mut out = vec![];
        for &s in &succs[tail] {
            let t = node_of[s];
            if !out.contains(&t) {
                out.push(t);
            }
        }
        csuccs[id] = out;
    }
    CondensedCfg {
        node_of,
        succs: csuccs,
        num_nodes: leaders.len(),
    }
}

/// Checks that the condensed CFGs of two functions are isomorphic (ordered
/// successor lists, rooted at the entry) and that their dominator trees
/// correspond under the isomorphism. Returns the block-level explanation on
/// failure.
pub fn check_cfg_equivalent(f1: &Function, f2: &Function) -> Result<()> {
    let c1 = condense_cfg(f1);
    let c2 = condense_cfg(f2);
    if c1.num_nodes != c2.num_nodes {
        bail!(
            "condensed CFG size mismatch: {} vs {} nodes",
            c1.num_nodes,
            c2.num_nodes
        );
    }
    let n = c1.num_nodes;
    let mut m12 = vec![usize::MAX; n];
    let mut m21 = vec![usize::MAX; n];
    m12[0] = 0;
    m21[0] = 0;
    let mut stack = vec![(0usize, 0usize)];
    let mut visited = vec![false; n];
    while let Some((a, b)) = stack.pop() {
        if visited[a] {
            continue;
        }
        visited[a] = true;
        let sa = &c1.succs[a];
        let sb = &c2.succs[b];
        if sa.len() != sb.len() {
            bail!(
                "CFG not isomorphic: node {} has {} successors vs {} on the other side",
                a,
                sa.len(),
                sb.len()
            );
        }
        for (&x, &y) in sa.iter().zip(sb.iter()) {
            if m12[x] == usize::MAX && m21[y] == usize::MAX {
                m12[x] = y;
                m21[y] = x;
            } else if m12[x] != y || m21[y] != x {
                bail!("CFG not isomorphic: inconsistent mapping {} -> {}", x, y);
            }
            stack.push((x, y));
        }
    }
    if visited.iter().any(|v| !v) {
        bail!("CFG comparison did not reach all nodes");
    }

    // Dominance equivalence on the condensed graphs.
    let d1: DominatorTree = build_domtree_from_succs(&c1.succs);
    let d2: DominatorTree = build_domtree_from_succs(&c2.succs);
    for a in 0..n {
        let b = m12[a];
        if m12[d1.idom[a]] != d2.idom[b] {
            bail!(
                "dominance mismatch: idom({}) maps to {} but is {}",
                a,
                m12[d1.idom[a]],
                d2.idom[b]
            );
        }
    }
    Ok(())
}
