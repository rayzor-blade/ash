//! Lowering: HL opcode arrays + register types -> [`Function`] in SSA form.
//!
//! Pipeline:
//! 1. Op-level trap-region dataflow (computes reachability, per-op trap
//!    stacks, and validates region balance — multiple `EndTrap`s per `Trap`
//!    on different paths are supported).
//! 2. Pinned-register discovery (`Ref` sources, `Incr`/`Decr` targets, trap
//!    exception registers, registers written inside trap regions) -> cells.
//! 3. `SetEnumField` construct resolution (EnumAlloc/MakeEnum backward scan,
//!    done once here; the construct is explicit in the IR forever).
//! 4. Basic-block partition over reachable ops (unreachable code is dropped).
//! 5. CFG construction including exceptional edges to trap handlers.
//! 6. Phi placement at iterated dominance frontiers + dominator-tree renaming
//!    walk that converts ops to typed [`Instr`]s with [`ValueId`] operands.

use super::ir::*;
use crate::dominance::DominatorTree;
use crate::opcodes::{Opcode, Reg};
use anyhow::{anyhow, bail, Result};
use std::collections::{BTreeMap, HashMap, HashSet};

/// Lower HL opcodes to an AIR v2 function.
///
/// * `ops` — the function's opcode array (raw bytecode; `IndirectCall` is a
///   v1 pass artifact and is rejected).
/// * `reg_types` — the function's register-type table (one `TypeRef` per HL
///   register). Call return/argument types are not needed: destination
///   register types carry all result types.
pub fn lower(ops: &[Opcode], reg_types: &[TypeRef]) -> Result<Function> {
    if ops.is_empty() {
        bail!("cannot lower an empty function");
    }
    for op in ops {
        if matches!(op, Opcode::IndirectCall { .. }) {
            bail!("IndirectCall is a v1 rewrite-pass artifact, not lowerable bytecode");
        }
    }

    let n = ops.len();

    // ---- 1. trap-region dataflow (op level) ------------------------------
    // stack_at[i] = Some(stack of open Trap op-indices) iff op i is reachable.
    let mut stack_at: Vec<Option<Vec<usize>>> = vec![None; n];
    stack_at[0] = Some(vec![]);
    let mut worklist = vec![0usize];
    while let Some(i) = worklist.pop() {
        let s = stack_at[i].clone().unwrap();
        let mut push_succ = |j: usize, sj: Vec<usize>, wl: &mut Vec<usize>| -> Result<()> {
            if j >= n {
                bail!("op {} control flow escapes the function (target {})", i, j);
            }
            match &stack_at[j] {
                None => {
                    stack_at[j] = Some(sj);
                    wl.push(j);
                }
                Some(prev) => {
                    if *prev != sj {
                        bail!("inconsistent trap nesting merging into op {}", j);
                    }
                }
            }
            Ok(())
        };
        match &ops[i] {
            Opcode::Trap { offset, .. } => {
                let handler = op_target(i, *offset)?;
                let mut pushed = s.clone();
                pushed.push(i);
                push_succ(i + 1, pushed, &mut worklist)?;
                push_succ(handler, s.clone(), &mut worklist)?;
            }
            Opcode::EndTrap { .. } => {
                let mut popped = s.clone();
                if popped.pop().is_none() {
                    bail!("EndTrap at op {} with no open trap region", i);
                }
                push_succ(i + 1, popped, &mut worklist)?;
            }
            Opcode::Ret { .. } => {
                if !s.is_empty() {
                    bail!("Ret at op {} inside an open trap region", i);
                }
            }
            Opcode::Throw { .. } | Opcode::Rethrow { .. } => {}
            Opcode::JAlways { offset } => {
                push_succ(op_target(i, *offset)?, s.clone(), &mut worklist)?;
            }
            Opcode::Switch { offsets, .. } => {
                push_succ(i + 1, s.clone(), &mut worklist)?;
                for off in offsets {
                    push_succ(op_target(i, *off)?, s.clone(), &mut worklist)?;
                }
            }
            op => {
                if let Some(off) = cond_jump_offset(op) {
                    push_succ(i + 1, s.clone(), &mut worklist)?;
                    push_succ(op_target(i, off)?, s.clone(), &mut worklist)?;
                } else {
                    push_succ(i + 1, s.clone(), &mut worklist)?;
                }
            }
        }
    }
    let reachable: Vec<bool> = stack_at.iter().map(|s| s.is_some()).collect();

    // ---- 2. pinned registers -> cells ------------------------------------
    // Priority order determines the recorded PinReason.
    let mut pin_reason: BTreeMap<u32, PinReason> = BTreeMap::new();
    let pin = |r: u32, reason: PinReason, m: &mut BTreeMap<u32, PinReason>| {
        m.entry(r).or_insert(reason);
    };
    for (i, op) in ops.iter().enumerate() {
        if !reachable[i] {
            continue;
        }
        if let Opcode::Ref { src, .. } = op {
            pin(src.0, PinReason::RefTaken, &mut pin_reason);
        }
    }
    for (i, op) in ops.iter().enumerate() {
        if !reachable[i] {
            continue;
        }
        if let Opcode::Incr { dst } | Opcode::Decr { dst } = op {
            pin(dst.0, PinReason::IncrDecr, &mut pin_reason);
        }
    }
    for (i, op) in ops.iter().enumerate() {
        if !reachable[i] {
            continue;
        }
        if let Opcode::Trap { exc, .. } = op {
            pin(exc.0, PinReason::TrapExc, &mut pin_reason);
        }
    }
    for (i, op) in ops.iter().enumerate() {
        if !reachable[i] {
            continue;
        }
        if stack_at[i].as_ref().is_some_and(|s| !s.is_empty()) {
            for w in crate::opcode_info::writes(op) {
                pin(w.0, PinReason::TrapWritten, &mut pin_reason);
            }
        }
    }
    let mut cells: Vec<CellData> = Vec::new();
    let mut cell_of: HashMap<u32, CellId> = HashMap::new();
    for (&r, &reason) in &pin_reason {
        if (r as usize) >= reg_types.len() {
            bail!("pinned register r{} out of range of register type table", r);
        }
        let id = CellId(cells.len() as u32);
        cells.push(CellData {
            reg: r,
            ty: reg_types[r as usize],
            reason,
        });
        cell_of.insert(r, id);
    }

    // ---- 3. SetEnumField construct resolution ----------------------------
    let mut construct_of: HashMap<usize, usize> = HashMap::new();
    for (i, op) in ops.iter().enumerate() {
        if !reachable[i] {
            continue;
        }
        if let Opcode::SetEnumField { value, .. } = op {
            let mut construct = 0usize; // JIT's default when no alloc found
            for j in (0..i).rev() {
                match &ops[j] {
                    Opcode::EnumAlloc { dst, construct: c } if dst.0 == value.0 => {
                        construct = c.0;
                        break;
                    }
                    Opcode::MakeEnum {
                        dst, construct: c, ..
                    } if dst.0 == value.0 => {
                        construct = c.0;
                        break;
                    }
                    _ => {}
                }
            }
            construct_of.insert(i, construct);
        }
    }

    // ---- 4. basic-block partition over reachable ops ---------------------
    let mut leader = vec![false; n];
    leader[0] = true;
    for (i, op) in ops.iter().enumerate() {
        if !reachable[i] {
            continue;
        }
        if is_block_ender(op) && i + 1 < n && reachable[i + 1] {
            leader[i + 1] = true;
        }
        for t in op_jump_targets(op, i)? {
            if reachable[t] {
                leader[t] = true;
            } else {
                bail!("jump target op {} is unreachable (inconsistent CFG)", t);
            }
        }
    }
    // Runs of reachable ops delimited by leaders. Real block k has id k+1
    // (block 0 is the synthetic entry).
    let mut runs: Vec<(usize, usize)> = Vec::new();
    let mut op_block: Vec<Option<usize>> = vec![None; n];
    {
        let mut i = 0usize;
        while i < n {
            if !reachable[i] {
                i += 1;
                continue;
            }
            debug_assert!(leader[i], "reachable op {} is not in any block", i);
            let start = i;
            let mut end = i;
            while end + 1 < n && reachable[end + 1] && !leader[end + 1] {
                end += 1;
            }
            let bid = runs.len() + 1;
            for j in start..=end {
                op_block[j] = Some(bid);
            }
            runs.push((start, end));
            i = end + 1;
        }
    }
    let n_blocks = runs.len() + 1;
    let blk_of = |op_idx: usize| -> Result<BlockId> {
        op_block
            .get(op_idx)
            .copied()
            .flatten()
            .map(|b| BlockId(b as u32))
            .ok_or_else(|| anyhow!("op {} is not part of any block", op_idx))
    };

    // ---- 5. CFG (block level, incl. exceptional edges) -------------------
    // Per-block innermost handler (as a block id).
    let mut handlers: Vec<Option<BlockId>> = vec![None; n_blocks];
    for (k, &(start, _)) in runs.iter().enumerate() {
        if let Some(stack) = &stack_at[start] {
            if let Some(&trap_op) = stack.last() {
                let h_target = match &ops[trap_op] {
                    Opcode::Trap { offset, .. } => op_target(trap_op, *offset)?,
                    _ => unreachable!(),
                };
                handlers[k + 1] = Some(blk_of(h_target)?);
            }
        }
    }

    let mut succs: Vec<Vec<usize>> = vec![vec![]; n_blocks];
    succs[0] = vec![1]; // synthetic entry -> first real block
    for (k, &(start, end)) in runs.iter().enumerate() {
        let bid = k + 1;
        let mut list: Vec<usize> = Vec::new();
        let add = |b: BlockId, list: &mut Vec<usize>| {
            if !list.contains(&b.idx()) {
                list.push(b.idx());
            }
        };
        match &ops[end] {
            Opcode::JAlways { offset } => add(blk_of(op_target(end, *offset)?)?, &mut list),
            Opcode::Switch { offsets, .. } => {
                for off in offsets {
                    add(blk_of(op_target(end, *off)?)?, &mut list);
                }
                add(blk_of(end + 1)?, &mut list);
            }
            Opcode::Ret { .. } | Opcode::Throw { .. } | Opcode::Rethrow { .. } => {}
            Opcode::Trap { offset, .. } => {
                add(blk_of(end + 1)?, &mut list);
                add(blk_of(op_target(end, *offset)?)?, &mut list);
            }
            op => {
                if let Some(off) = cond_jump_offset(op) {
                    add(blk_of(op_target(end, off)?)?, &mut list);
                    add(blk_of(end + 1)?, &mut list);
                } else {
                    add(blk_of(end + 1)?, &mut list);
                }
            }
        }
        if let Some(h) = handlers[bid] {
            let throws = ops[start..=end].iter().any(op_may_throw);
            if throws {
                add(h, &mut list);
            }
        }
        succs[bid] = list;
    }

    let dom = build_domtree_from_succs(&succs);
    for b in 0..n_blocks {
        if dom.rpo_number[b] == usize::MAX {
            bail!("block {} unreachable in block-level CFG (lowering bug)", b);
        }
    }
    let mut preds: Vec<Vec<usize>> = vec![vec![]; n_blocks];
    for (b, ss) in succs.iter().enumerate() {
        for &s in ss {
            if !preds[s].contains(&b) {
                preds[s].push(b);
            }
        }
    }

    // ---- 6a. phi placement at iterated dominance frontiers ---------------
    let num_regs = reg_types.len();
    let is_pinned = |r: u32| cell_of.contains_key(&r);

    let mut def_blocks: Vec<HashSet<usize>> = vec![HashSet::new(); num_regs];
    for (r, defs) in def_blocks.iter_mut().enumerate() {
        if !is_pinned(r as u32) {
            defs.insert(0); // Param definitions at the synthetic entry
        }
    }
    for (k, &(start, end)) in runs.iter().enumerate() {
        for op in &ops[start..=end] {
            for w in crate::opcode_info::writes(op) {
                if !is_pinned(w.0) && (w.0 as usize) < num_regs {
                    def_blocks[w.0 as usize].insert(k + 1);
                }
            }
        }
    }

    let mut phi_regs: Vec<Vec<u32>> = vec![vec![]; n_blocks];
    for (r, defs) in def_blocks.iter().enumerate() {
        if is_pinned(r as u32) || defs.is_empty() {
            continue;
        }
        let mut wl: Vec<usize> = defs.iter().copied().collect();
        wl.sort_unstable();
        let mut has_phi: HashSet<usize> = HashSet::new();
        let mut ever: HashSet<usize> = defs.clone();
        while let Some(x) = wl.pop() {
            for &y in &dom.dom_frontier[x] {
                if has_phi.insert(y) {
                    phi_regs[y].push(r as u32);
                    if ever.insert(y) {
                        wl.push(y);
                    }
                }
            }
        }
    }

    // ---- 6b. renaming walk + instruction building ------------------------
    let mut func = Function {
        values: Vec::new(),
        cells,
        blocks: Vec::new(),
        reg_types: reg_types.to_vec(),
    };

    // Per-block phi state: (base reg, dst value, incoming).
    let mut phi_nodes: Vec<Vec<(u32, Option<ValueId>, Vec<(BlockId, ValueId)>)>> = phi_regs
        .iter()
        .map(|regs| regs.iter().map(|&r| (r, None, vec![])).collect())
        .collect();

    let mut built: Vec<Option<(Vec<Instr>, Terminator)>> = vec![None; n_blocks];
    let mut stacks: Vec<Vec<ValueId>> = vec![vec![]; num_regs];

    // Iterative dominator-tree preorder walk with stack restoration.
    enum WalkItem {
        Visit(usize),
        Restore(Vec<usize>),
    }
    let mut walk = vec![WalkItem::Visit(dom.rpo[0])];
    while let Some(item) = walk.pop() {
        let b = match item {
            WalkItem::Restore(saved) => {
                for (r, s) in saved.iter().enumerate() {
                    stacks[r].truncate(*s);
                }
                continue;
            }
            WalkItem::Visit(b) => b,
        };
        let saved: Vec<usize> = stacks.iter().map(|s| s.len()).collect();

        // Phi definitions first.
        for node in phi_nodes[b].iter_mut() {
            let r = node.0;
            let v = func.new_value(reg_types[r as usize], r);
            node.1 = Some(v);
            stacks[r as usize].push(v);
        }

        let mut instrs: Vec<Instr> = Vec::new();
        let term: Terminator;
        if b == 0 {
            // Synthetic entry: Param values for every non-pinned register.
            for r in 0..num_regs {
                if !is_pinned(r as u32) {
                    let v = func.new_value(reg_types[r], r as u32);
                    instrs.push(Instr::Param {
                        dst: v,
                        reg: r as u32,
                    });
                    stacks[r].push(v);
                }
            }
            term = Terminator::Jump { target: BlockId(1) };
        } else {
            let (start, end) = runs[b - 1];
            term = convert_ops(
                ops,
                start,
                end,
                reg_types,
                &cell_of,
                &construct_of,
                &blk_of,
                &mut func,
                &mut stacks,
                &mut instrs,
            )?;
        }

        // Fill phi sources in CFG successors.
        for &s in &succs[b] {
            for node in phi_nodes[s].iter_mut() {
                let r = node.0 as usize;
                let cur = *stacks[r]
                    .last()
                    .ok_or_else(|| anyhow!("no live SSA value for r{} feeding phi in b{}", r, s))?;
                node.2.push((BlockId(b as u32), cur));
            }
        }

        built[b] = Some((instrs, term));

        walk.push(WalkItem::Restore(saved));
        for &child in dom.dom_children[b].iter().rev() {
            walk.push(WalkItem::Visit(child));
        }
    }

    // ---- assemble --------------------------------------------------------
    for b in 0..n_blocks {
        let (instrs, term) = built[b]
            .take()
            .ok_or_else(|| anyhow!("block {} never visited by renaming walk", b))?;
        let phis = phi_nodes[b]
            .iter()
            .map(|(_, dst, incoming)| Phi {
                dst: dst.expect("phi dst assigned during walk"),
                incoming: incoming.clone(),
            })
            .collect();
        func.blocks.push(Block {
            phis,
            instrs,
            term,
            handler: handlers[b],
        });
    }
    Ok(func)
}

// ---------------------------------------------------------------------------
// op conversion
// ---------------------------------------------------------------------------

#[allow(clippy::too_many_arguments)]
fn convert_ops(
    ops: &[Opcode],
    start: usize,
    end: usize,
    reg_types: &[TypeRef],
    cell_of: &HashMap<u32, CellId>,
    construct_of: &HashMap<usize, usize>,
    blk_of: &dyn Fn(usize) -> Result<BlockId>,
    func: &mut Function,
    stacks: &mut [Vec<ValueId>],
    instrs: &mut Vec<Instr>,
) -> Result<Terminator> {
    // Read the current SSA name of a register, going through a CellGet for
    // pinned registers.
    macro_rules! use_reg {
        ($r:expr) => {{
            let r: u32 = ($r).0;
            if let Some(&cell) = cell_of.get(&r) {
                let v = func.new_value(func.cells[cell.idx()].ty, r);
                instrs.push(Instr::CellGet { dst: v, cell });
                v
            } else {
                *stacks
                    .get(r as usize)
                    .and_then(|s| s.last())
                    .ok_or_else(|| anyhow!("read of r{} before any definition", r))?
            }
        }};
    }
    // Create the destination value for a register write. Returns the value
    // and, for pinned registers, the cell that must receive it afterwards.
    macro_rules! def_reg {
        ($r:expr) => {{
            let r: u32 = ($r).0;
            let ty = *reg_types
                .get(r as usize)
                .ok_or_else(|| anyhow!("write to r{} outside register type table", r))?;
            if let Some(&cell) = cell_of.get(&r) {
                (func.new_value(ty, r), Some(cell))
            } else {
                let v = func.new_value(ty, r);
                stacks[r as usize].push(v);
                (v, None)
            }
        }};
    }
    macro_rules! finish_def {
        ($pin:expr) => {
            if let Some(cell) = $pin {
                let src = instrs
                    .last()
                    .and_then(|i| i.dst())
                    .expect("pinned def emits an instruction with a dst");
                instrs.push(Instr::CellSet { cell, src });
            }
        };
    }
    macro_rules! cell_for {
        ($r:expr) => {
            *cell_of
                .get(&($r).0)
                .ok_or_else(|| anyhow!("r{} expected to be pinned", ($r).0))?
        };
    }
    macro_rules! cond {
        ($kind:expr, $a:expr, $b:expr, $i:expr, $off:expr) => {{
            let a = use_reg!($a);
            let b = match $b {
                Some(rb) => Some(use_reg!(rb)),
                None => None,
            };
            Terminator::CondJump {
                cond: $kind,
                a,
                b,
                if_true: blk_of(op_target($i, $off)?)?,
                if_false: blk_of($i + 1)?,
            }
        }};
    }

    for i in start..=end {
        let is_last = i == end;
        match &ops[i] {
            // ---- terminators (always last op of a block) ----
            Opcode::JAlways { offset } => {
                debug_assert!(is_last);
                return Ok(Terminator::Jump {
                    target: blk_of(op_target(i, *offset)?)?,
                });
            }
            Opcode::JTrue { cond, offset } => {
                return Ok(cond!(CondKind::True, *cond, None::<Reg>, i, *offset))
            }
            Opcode::JFalse { cond, offset } => {
                return Ok(cond!(CondKind::False, *cond, None::<Reg>, i, *offset))
            }
            Opcode::JNull { reg, offset } => {
                return Ok(cond!(CondKind::Null, *reg, None::<Reg>, i, *offset))
            }
            Opcode::JNotNull { reg, offset } => {
                return Ok(cond!(CondKind::NotNull, *reg, None::<Reg>, i, *offset))
            }
            Opcode::JSLt { a, b, offset } => {
                return Ok(cond!(CondKind::SLt, *a, Some(*b), i, *offset))
            }
            Opcode::JSGte { a, b, offset } => {
                return Ok(cond!(CondKind::SGte, *a, Some(*b), i, *offset))
            }
            Opcode::JSGt { a, b, offset } => {
                return Ok(cond!(CondKind::SGt, *a, Some(*b), i, *offset))
            }
            Opcode::JSLte { a, b, offset } => {
                return Ok(cond!(CondKind::SLte, *a, Some(*b), i, *offset))
            }
            Opcode::JULt { a, b, offset } => {
                return Ok(cond!(CondKind::ULt, *a, Some(*b), i, *offset))
            }
            Opcode::JUGte { a, b, offset } => {
                return Ok(cond!(CondKind::UGte, *a, Some(*b), i, *offset))
            }
            Opcode::JNotLt { a, b, offset } => {
                return Ok(cond!(CondKind::NotLt, *a, Some(*b), i, *offset))
            }
            Opcode::JNotGte { a, b, offset } => {
                return Ok(cond!(CondKind::NotGte, *a, Some(*b), i, *offset))
            }
            Opcode::JEq { a, b, offset } => {
                return Ok(cond!(CondKind::Eq, *a, Some(*b), i, *offset))
            }
            Opcode::JNotEq { a, b, offset } => {
                return Ok(cond!(CondKind::NotEq, *a, Some(*b), i, *offset))
            }
            Opcode::Switch { reg, offsets, .. } => {
                let value = use_reg!(*reg);
                let mut targets = Vec::with_capacity(offsets.len());
                for off in offsets {
                    targets.push(blk_of(op_target(i, *off)?)?);
                }
                return Ok(Terminator::Switch {
                    value,
                    targets,
                    default: blk_of(i + 1)?,
                });
            }
            Opcode::Ret { ret } => {
                let value = use_reg!(*ret);
                return Ok(Terminator::Ret { value });
            }
            Opcode::Throw { exc } => {
                let exc = use_reg!(*exc);
                return Ok(Terminator::Throw { exc });
            }
            Opcode::Rethrow { exc } => {
                let exc = use_reg!(*exc);
                return Ok(Terminator::Rethrow { exc });
            }
            Opcode::Trap { exc, offset } => {
                return Ok(Terminator::Trap {
                    exc_cell: cell_for!(*exc),
                    handler: blk_of(op_target(i, *offset)?)?,
                    normal: blk_of(i + 1)?,
                });
            }

            // ---- structural no-ops ----
            Opcode::Label | Opcode::Nop => {}

            // ---- straight-line instructions ----
            Opcode::Mov { dst, src } => {
                let s = use_reg!(*src);
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::Copy { dst: d, src: s });
                finish_def!(pin);
            }
            Opcode::Int { dst, ptr } => {
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::Int { dst: d, idx: ptr.0 });
                finish_def!(pin);
            }
            Opcode::Float { dst, ptr } => {
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::Float { dst: d, idx: ptr.0 });
                finish_def!(pin);
            }
            Opcode::Bool { dst, value } => {
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::Bool {
                    dst: d,
                    value: *value,
                });
                finish_def!(pin);
            }
            Opcode::Bytes { dst, ptr } => {
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::Bytes { dst: d, idx: ptr.0 });
                finish_def!(pin);
            }
            Opcode::String { dst, ptr } => {
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::String { dst: d, idx: ptr.0 });
                finish_def!(pin);
            }
            Opcode::Null { dst } => {
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::Null { dst: d });
                finish_def!(pin);
            }
            Opcode::Add { dst, a, b }
            | Opcode::Sub { dst, a, b }
            | Opcode::Mul { dst, a, b }
            | Opcode::SDiv { dst, a, b }
            | Opcode::UDiv { dst, a, b }
            | Opcode::SMod { dst, a, b }
            | Opcode::UMod { dst, a, b }
            | Opcode::Shl { dst, a, b }
            | Opcode::SShr { dst, a, b }
            | Opcode::UShr { dst, a, b }
            | Opcode::And { dst, a, b }
            | Opcode::Or { dst, a, b }
            | Opcode::Xor { dst, a, b } => {
                let op = match &ops[i] {
                    Opcode::Add { .. } => BinOp::Add,
                    Opcode::Sub { .. } => BinOp::Sub,
                    Opcode::Mul { .. } => BinOp::Mul,
                    Opcode::SDiv { .. } => BinOp::SDiv,
                    Opcode::UDiv { .. } => BinOp::UDiv,
                    Opcode::SMod { .. } => BinOp::SMod,
                    Opcode::UMod { .. } => BinOp::UMod,
                    Opcode::Shl { .. } => BinOp::Shl,
                    Opcode::SShr { .. } => BinOp::SShr,
                    Opcode::UShr { .. } => BinOp::UShr,
                    Opcode::And { .. } => BinOp::And,
                    Opcode::Or { .. } => BinOp::Or,
                    _ => BinOp::Xor,
                };
                let va = use_reg!(*a);
                let vb = use_reg!(*b);
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::BinOp {
                    op,
                    dst: d,
                    a: va,
                    b: vb,
                });
                finish_def!(pin);
            }
            Opcode::Neg { dst, src } | Opcode::Not { dst, src } => {
                let op = if matches!(&ops[i], Opcode::Neg { .. }) {
                    UnOp::Neg
                } else {
                    UnOp::Not
                };
                let s = use_reg!(*src);
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::UnOp { op, dst: d, src: s });
                finish_def!(pin);
            }
            Opcode::Incr { dst } => {
                instrs.push(Instr::CellIncr {
                    cell: cell_for!(*dst),
                });
            }
            Opcode::Decr { dst } => {
                instrs.push(Instr::CellDecr {
                    cell: cell_for!(*dst),
                });
            }
            Opcode::Call0 { dst, fun } => {
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::Call {
                    dst: d,
                    fun: fun.0,
                    args: vec![],
                });
                finish_def!(pin);
            }
            Opcode::Call1 { dst, fun, arg0 } => {
                let a0 = use_reg!(*arg0);
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::Call {
                    dst: d,
                    fun: fun.0,
                    args: vec![a0],
                });
                finish_def!(pin);
            }
            Opcode::Call2 {
                dst,
                fun,
                arg0,
                arg1,
            } => {
                let a0 = use_reg!(*arg0);
                let a1 = use_reg!(*arg1);
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::Call {
                    dst: d,
                    fun: fun.0,
                    args: vec![a0, a1],
                });
                finish_def!(pin);
            }
            Opcode::Call3 {
                dst,
                fun,
                arg0,
                arg1,
                arg2,
            } => {
                let a0 = use_reg!(*arg0);
                let a1 = use_reg!(*arg1);
                let a2 = use_reg!(*arg2);
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::Call {
                    dst: d,
                    fun: fun.0,
                    args: vec![a0, a1, a2],
                });
                finish_def!(pin);
            }
            Opcode::Call4 {
                dst,
                fun,
                arg0,
                arg1,
                arg2,
                arg3,
            } => {
                let a0 = use_reg!(*arg0);
                let a1 = use_reg!(*arg1);
                let a2 = use_reg!(*arg2);
                let a3 = use_reg!(*arg3);
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::Call {
                    dst: d,
                    fun: fun.0,
                    args: vec![a0, a1, a2, a3],
                });
                finish_def!(pin);
            }
            Opcode::CallN { dst, fun, args } => {
                let mut vargs = Vec::with_capacity(args.len());
                for a in args {
                    vargs.push(use_reg!(*a));
                }
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::Call {
                    dst: d,
                    fun: fun.0,
                    args: vargs,
                });
                finish_def!(pin);
            }
            Opcode::CallMethod { dst, field, args } => {
                let mut vargs = Vec::with_capacity(args.len());
                for a in args {
                    vargs.push(use_reg!(*a));
                }
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::CallMethod {
                    dst: d,
                    field: field.0,
                    args: vargs,
                });
                finish_def!(pin);
            }
            Opcode::CallThis { dst, field, args } => {
                // Normalize: `this` (reg0) becomes the explicit receiver.
                let this = use_reg!(Reg(0));
                let mut vargs = Vec::with_capacity(args.len() + 1);
                vargs.push(this);
                for a in args {
                    vargs.push(use_reg!(*a));
                }
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::CallMethod {
                    dst: d,
                    field: field.0,
                    args: vargs,
                });
                finish_def!(pin);
            }
            Opcode::CallClosure { dst, fun, args } => {
                let f = use_reg!(*fun);
                let mut vargs = Vec::with_capacity(args.len());
                for a in args {
                    vargs.push(use_reg!(*a));
                }
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::CallClosure {
                    dst: d,
                    fun: f,
                    args: vargs,
                });
                finish_def!(pin);
            }
            Opcode::StaticClosure { dst, fun } => {
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::StaticClosure { dst: d, fun: fun.0 });
                finish_def!(pin);
            }
            Opcode::InstanceClosure { dst, fun, obj } => {
                let o = use_reg!(*obj);
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::InstanceClosure {
                    dst: d,
                    fun: fun.0,
                    obj: o,
                });
                finish_def!(pin);
            }
            Opcode::VirtualClosure { dst, obj, field } => {
                // `field` is an immediate proto index, NOT a register read.
                let o = use_reg!(*obj);
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::VirtualClosure {
                    dst: d,
                    obj: o,
                    field: field.0 as usize,
                });
                finish_def!(pin);
            }
            Opcode::GetGlobal { dst, global } => {
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::GetGlobal {
                    dst: d,
                    global: global.0,
                });
                finish_def!(pin);
            }
            Opcode::SetGlobal { global, src } => {
                let s = use_reg!(*src);
                instrs.push(Instr::SetGlobal {
                    global: global.0,
                    src: s,
                });
            }
            Opcode::Field { dst, obj, field } => {
                let o = use_reg!(*obj);
                let obj_ty = func.value_ty(o);
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::FieldGet {
                    dst: d,
                    obj: o,
                    obj_ty,
                    field: field.0,
                });
                finish_def!(pin);
            }
            Opcode::SetField { obj, field, src } => {
                let o = use_reg!(*obj);
                let s = use_reg!(*src);
                let obj_ty = func.value_ty(o);
                instrs.push(Instr::FieldSet {
                    obj: o,
                    obj_ty,
                    field: field.0,
                    src: s,
                });
            }
            Opcode::GetThis { dst, field } => {
                let this = use_reg!(Reg(0));
                let obj_ty = func.value_ty(this);
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::FieldGet {
                    dst: d,
                    obj: this,
                    obj_ty,
                    field: field.0,
                });
                finish_def!(pin);
            }
            Opcode::SetThis { field, src } => {
                let this = use_reg!(Reg(0));
                let s = use_reg!(*src);
                let obj_ty = func.value_ty(this);
                instrs.push(Instr::FieldSet {
                    obj: this,
                    obj_ty,
                    field: field.0,
                    src: s,
                });
            }
            Opcode::DynGet { dst, obj, field } => {
                let o = use_reg!(*obj);
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::DynGet {
                    dst: d,
                    obj: o,
                    field: field.0,
                });
                finish_def!(pin);
            }
            Opcode::DynSet { obj, field, src } => {
                let o = use_reg!(*obj);
                let s = use_reg!(*src);
                instrs.push(Instr::DynSet {
                    obj: o,
                    field: field.0,
                    src: s,
                });
            }
            Opcode::ToDyn { dst, src }
            | Opcode::ToSFloat { dst, src }
            | Opcode::ToUFloat { dst, src }
            | Opcode::ToInt { dst, src }
            | Opcode::SafeCast { dst, src }
            | Opcode::UnsafeCast { dst, src }
            | Opcode::ToVirtual { dst, src } => {
                let kind = match &ops[i] {
                    Opcode::ToDyn { .. } => CastKind::ToDyn,
                    Opcode::ToSFloat { .. } => CastKind::ToSFloat,
                    Opcode::ToUFloat { .. } => CastKind::ToUFloat,
                    Opcode::ToInt { .. } => CastKind::ToInt,
                    Opcode::SafeCast { .. } => CastKind::SafeCast,
                    Opcode::UnsafeCast { .. } => CastKind::UnsafeCast,
                    _ => CastKind::ToVirtual,
                };
                let s = use_reg!(*src);
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::Cast {
                    kind,
                    dst: d,
                    src: s,
                });
                finish_def!(pin);
            }
            Opcode::NullCheck { reg } => {
                let v = use_reg!(*reg);
                instrs.push(Instr::NullCheck { value: v });
            }
            Opcode::EndTrap { exc } => {
                instrs.push(Instr::EndTrap {
                    cell: cell_for!(*exc),
                });
            }
            Opcode::GetI8 { dst, bytes, index }
            | Opcode::GetI16 { dst, bytes, index }
            | Opcode::GetMem { dst, bytes, index } => {
                let kind = match &ops[i] {
                    Opcode::GetI8 { .. } => MemAccess::I8,
                    Opcode::GetI16 { .. } => MemAccess::I16,
                    _ => MemAccess::Mem,
                };
                let base = use_reg!(*bytes);
                let idx = use_reg!(*index);
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::MemGet {
                    kind,
                    dst: d,
                    base,
                    index: idx,
                });
                finish_def!(pin);
            }
            Opcode::GetArray { dst, array, index } => {
                let base = use_reg!(*array);
                let idx = use_reg!(*index);
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::MemGet {
                    kind: MemAccess::Array,
                    dst: d,
                    base,
                    index: idx,
                });
                finish_def!(pin);
            }
            Opcode::SetI8 { bytes, index, src }
            | Opcode::SetI16 { bytes, index, src }
            | Opcode::SetMem { bytes, index, src } => {
                let kind = match &ops[i] {
                    Opcode::SetI8 { .. } => MemAccess::I8,
                    Opcode::SetI16 { .. } => MemAccess::I16,
                    _ => MemAccess::Mem,
                };
                let base = use_reg!(*bytes);
                let idx = use_reg!(*index);
                let s = use_reg!(*src);
                instrs.push(Instr::MemSet {
                    kind,
                    base,
                    index: idx,
                    src: s,
                });
            }
            Opcode::SetArray { array, index, src } => {
                let base = use_reg!(*array);
                let idx = use_reg!(*index);
                let s = use_reg!(*src);
                instrs.push(Instr::MemSet {
                    kind: MemAccess::Array,
                    base,
                    index: idx,
                    src: s,
                });
            }
            Opcode::New { dst } => {
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::New { dst: d });
                finish_def!(pin);
            }
            Opcode::ArraySize { dst, array } => {
                let a = use_reg!(*array);
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::ArraySize { dst: d, array: a });
                finish_def!(pin);
            }
            Opcode::Type { dst, ty } => {
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::TypeConst {
                    dst: d,
                    ty: TypeRef(ty.0 as u32),
                });
                finish_def!(pin);
            }
            Opcode::GetType { dst, src } => {
                let s = use_reg!(*src);
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::GetType { dst: d, src: s });
                finish_def!(pin);
            }
            Opcode::GetTID { dst, src } => {
                let s = use_reg!(*src);
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::GetTID { dst: d, src: s });
                finish_def!(pin);
            }
            Opcode::Ref { dst, src } => {
                let cell = cell_for!(*src);
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::CellRef { dst: d, cell });
                finish_def!(pin);
            }
            Opcode::Unref { dst, src } => {
                let s = use_reg!(*src);
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::Unref { dst: d, src: s });
                finish_def!(pin);
            }
            Opcode::Setref { dst, value } => {
                let r = use_reg!(*dst);
                let v = use_reg!(*value);
                instrs.push(Instr::SetRef { r, value: v });
            }
            Opcode::RefData { dst, src } => {
                let s = use_reg!(*src);
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::RefData { dst: d, src: s });
                finish_def!(pin);
            }
            Opcode::RefOffset { dst, reg, offset } => {
                let base = use_reg!(*reg);
                let off = use_reg!(*offset);
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::RefOffset {
                    dst: d,
                    base,
                    offset: off,
                });
                finish_def!(pin);
            }
            Opcode::MakeEnum {
                dst,
                construct,
                args,
            } => {
                let mut vargs = Vec::with_capacity(args.len());
                for a in args {
                    vargs.push(use_reg!(*a));
                }
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::MakeEnum {
                    dst: d,
                    construct: construct.0,
                    args: vargs,
                });
                finish_def!(pin);
            }
            Opcode::EnumAlloc { dst, construct } => {
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::EnumAlloc {
                    dst: d,
                    construct: construct.0,
                });
                finish_def!(pin);
            }
            Opcode::EnumIndex { dst, value } => {
                let v = use_reg!(*value);
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::EnumIndex { dst: d, value: v });
                finish_def!(pin);
            }
            Opcode::EnumField {
                dst,
                value,
                construct,
                field,
            } => {
                let v = use_reg!(*value);
                let (d, pin) = def_reg!(*dst);
                instrs.push(Instr::EnumField {
                    dst: d,
                    value: v,
                    construct: construct.0,
                    field: field.0,
                });
                finish_def!(pin);
            }
            Opcode::SetEnumField { value, field, src } => {
                let construct = *construct_of
                    .get(&i)
                    .ok_or_else(|| anyhow!("SetEnumField at op {} missing construct", i))?;
                let v = use_reg!(*value);
                let s = use_reg!(*src);
                instrs.push(Instr::SetEnumField {
                    value: v,
                    construct,
                    field: field.0,
                    src: s,
                });
            }
            Opcode::Assert => instrs.push(Instr::Assert),
            Opcode::Prefetch { value, field, mode } => {
                let v = use_reg!(*value);
                instrs.push(Instr::Prefetch {
                    value: v,
                    field: field.0,
                    mode: *mode,
                });
            }
            Opcode::Asm { mode, value, reg } => {
                instrs.push(Instr::Asm {
                    mode: *mode,
                    value: *value,
                    reg: reg.0,
                });
            }
            Opcode::IndirectCall { .. } => {
                bail!("IndirectCall reached conversion (should be rejected earlier)")
            }
        }
    }
    // Fell through the end of the block without a control op.
    Ok(Terminator::Jump {
        target: blk_of(end + 1)?,
    })
}

// ---------------------------------------------------------------------------
// op-level helpers
// ---------------------------------------------------------------------------

fn op_target(i: usize, offset: i32) -> Result<usize> {
    let t = i as i64 + 1 + offset as i64;
    if t < 0 {
        bail!("jump at op {} targets negative index", i);
    }
    Ok(t as usize)
}

/// Offset of a *conditional* jump opcode (not JAlways/Switch/Trap).
fn cond_jump_offset(op: &Opcode) -> Option<i32> {
    match op {
        Opcode::JTrue { offset, .. }
        | Opcode::JFalse { offset, .. }
        | Opcode::JNull { offset, .. }
        | Opcode::JNotNull { offset, .. }
        | Opcode::JSLt { offset, .. }
        | Opcode::JSGte { offset, .. }
        | Opcode::JSGt { offset, .. }
        | Opcode::JSLte { offset, .. }
        | Opcode::JULt { offset, .. }
        | Opcode::JUGte { offset, .. }
        | Opcode::JNotLt { offset, .. }
        | Opcode::JNotGte { offset, .. }
        | Opcode::JEq { offset, .. }
        | Opcode::JNotEq { offset, .. } => Some(*offset),
        _ => None,
    }
}

/// Ops that end their basic block.
fn is_block_ender(op: &Opcode) -> bool {
    cond_jump_offset(op).is_some()
        || matches!(
            op,
            Opcode::JAlways { .. }
                | Opcode::Switch { .. }
                | Opcode::Ret { .. }
                | Opcode::Throw { .. }
                | Opcode::Rethrow { .. }
                | Opcode::Trap { .. }
                | Opcode::EndTrap { .. }
        )
}

/// Explicit jump targets of an op (excluding fall-through).
fn op_jump_targets(op: &Opcode, i: usize) -> Result<Vec<usize>> {
    let mut out = vec![];
    match op {
        Opcode::JAlways { offset } | Opcode::Trap { offset, .. } => {
            out.push(op_target(i, *offset)?);
        }
        Opcode::Switch { offsets, .. } => {
            for off in offsets {
                out.push(op_target(i, *off)?);
            }
        }
        _ => {
            if let Some(off) = cond_jump_offset(op) {
                out.push(op_target(i, off)?);
            }
        }
    }
    Ok(out)
}

/// Opcode-level may-throw classification. Must agree with
/// [`Instr::may_throw`] on the lowered form (drives exceptional CFG edges).
fn op_may_throw(op: &Opcode) -> bool {
    matches!(
        op,
        Opcode::Call0 { .. }
            | Opcode::Call1 { .. }
            | Opcode::Call2 { .. }
            | Opcode::Call3 { .. }
            | Opcode::Call4 { .. }
            | Opcode::CallN { .. }
            | Opcode::CallMethod { .. }
            | Opcode::CallThis { .. }
            | Opcode::CallClosure { .. }
            | Opcode::SDiv { .. }
            | Opcode::UDiv { .. }
            | Opcode::SMod { .. }
            | Opcode::UMod { .. }
            | Opcode::SafeCast { .. }
            | Opcode::DynGet { .. }
            | Opcode::DynSet { .. }
            | Opcode::NullCheck { .. }
            | Opcode::Assert
            | Opcode::Asm { .. }
            | Opcode::Throw { .. }
            | Opcode::Rethrow { .. }
    )
}

/// Build a dominator tree over a raw successor-list CFG by adapting it to the
/// v1 [`crate::cfg::CFG`] shape (dominance only reads successors/preds).
pub(crate) fn build_domtree_from_succs(succs: &[Vec<usize>]) -> DominatorTree {
    let n = succs.len();
    let mut preds: Vec<Vec<usize>> = vec![vec![]; n];
    for (b, ss) in succs.iter().enumerate() {
        for &s in ss {
            if !preds[s].contains(&b) {
                preds[s].push(b);
            }
        }
    }
    let blocks = (0..n)
        .map(|id| crate::cfg::BasicBlock {
            id,
            start: 0,
            end: 0,
            successors: succs[id].clone(),
            predecessors: preds[id].clone(),
        })
        .collect();
    let cfg = crate::cfg::CFG {
        blocks,
        block_of: vec![],
    };
    DominatorTree::build(&cfg)
}
