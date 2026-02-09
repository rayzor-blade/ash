use std::collections::HashSet;
use crate::cfg::{BlockId, CFG};
use crate::dominance::DominatorTree;
use crate::opcodes::{Opcode, Reg};
use crate::opcode_info;

/// A phi node at a block entry: dst = phi(sources).
#[derive(Debug, Clone)]
pub struct PhiNode {
    pub dst: Reg,
    pub base_reg: u32,
    pub sources: Vec<(BlockId, Reg)>,
}

/// SSA form: the opcode array has been renamed with SSA register IDs,
/// and phi nodes are stored in a side-table per block.
pub struct SSAForm {
    pub phis: Vec<Vec<PhiNode>>,
    pub num_ssa_regs: usize,
    pub base_reg: Vec<u32>,
    pub num_original_regs: usize,
    pub pinned: HashSet<u32>,
}

impl SSAForm {
    /// Construct SSA form from opcodes. Mutates ops in-place (renames registers).
    pub fn construct(
        ops: &mut Vec<Opcode>,
        num_regs: usize,
        cfg: &CFG,
        dom: &DominatorTree,
    ) -> Self {
        let n_blocks = cfg.blocks.len();
        if n_blocks == 0 {
            return SSAForm {
                phis: vec![],
                num_ssa_regs: num_regs,
                base_reg: (0..num_regs as u32).collect(),
                num_original_regs: num_regs,
                pinned: HashSet::new(),
            };
        }

        // Pre-scan: find pinned registers (Incr/Decr targets and Ref-taken)
        let mut pinned = HashSet::new();
        for op in ops.iter() {
            match op {
                Opcode::Incr { dst } | Opcode::Decr { dst } => { pinned.insert(dst.0); }
                Opcode::Ref { src, .. } => { pinned.insert(src.0); }
                _ => {}
            }
        }

        // Phase 1: Compute definition sites for each register
        let mut def_blocks: Vec<HashSet<BlockId>> = vec![HashSet::new(); num_regs];

        // All registers are implicitly defined at entry (function params, default-initialized)
        for v in 0..num_regs {
            if !pinned.contains(&(v as u32)) {
                def_blocks[v].insert(0);
            }
        }

        for block in &cfg.blocks {
            for i in block.start..=block.end {
                for w in opcode_info::writes(&ops[i]) {
                    if !pinned.contains(&w.0) {
                        def_blocks[w.0 as usize].insert(block.id);
                    }
                }
            }
        }

        // Phase 2: Insert phi nodes at iterated dominance frontiers
        let mut phis: Vec<Vec<PhiNode>> = vec![vec![]; n_blocks];

        for v in 0..num_regs {
            if pinned.contains(&(v as u32)) { continue; }
            if def_blocks[v].is_empty() { continue; }

            let mut worklist: Vec<BlockId> = def_blocks[v].iter().copied().collect();
            let mut has_phi: HashSet<BlockId> = HashSet::new();
            let mut ever_on_worklist: HashSet<BlockId> = def_blocks[v].clone();

            while let Some(x) = worklist.pop() {
                for &y in &dom.dom_frontier[x] {
                    if has_phi.insert(y) {
                        phis[y].push(PhiNode {
                            dst: Reg(0), // placeholder, assigned during renaming
                            base_reg: v as u32,
                            sources: Vec::new(),
                        });
                        if ever_on_worklist.insert(y) {
                            worklist.push(y);
                        }
                    }
                }
            }
        }

        // Phase 3: Rename registers via dominator tree preorder walk
        let mut base_reg: Vec<u32> = (0..num_regs as u32).collect();
        let mut next_ssa_reg = num_regs as u32;

        // Per-register stack of current SSA name
        let mut stacks: Vec<Vec<Reg>> = Vec::with_capacity(num_regs);
        for v in 0..num_regs {
            // Initial version: the original register itself
            stacks.push(vec![Reg(v as u32)]);
        }

        fn new_version(
            base: u32,
            next_ssa_reg: &mut u32,
            base_reg_map: &mut Vec<u32>,
            stacks: &mut [Vec<Reg>],
        ) -> Reg {
            let ssa_id = *next_ssa_reg;
            *next_ssa_reg += 1;
            base_reg_map.push(base);
            let reg = Reg(ssa_id);
            stacks[base as usize].push(reg);
            reg
        }

        fn current_name(stacks: &[Vec<Reg>], base: u32) -> Reg {
            *stacks[base as usize].last().unwrap()
        }

        // Iterative domtree preorder walk using explicit stack
        // Stack entries: (block_id, phase)
        // phase 0: process block (rename phis, opcodes, fill successor phis, push children)
        // phase 1: restore stacks (pop back to saved sizes)
        let mut walk_stack: Vec<(BlockId, bool, Vec<usize>)> = vec![];
        // (block_id, is_restore_phase, saved_stack_sizes)
        walk_stack.push((dom.rpo[0], false, vec![]));

        while let Some((block_id, is_restore, saved_sizes)) = walk_stack.pop() {
            if is_restore {
                // Restore stacks to saved sizes
                for v in 0..num_regs {
                    stacks[v].truncate(saved_sizes[v]);
                }
                continue;
            }

            // Save stack sizes for later restoration
            let saved: Vec<usize> = stacks.iter().map(|s| s.len()).collect();

            // Process phi nodes at this block: each phi defines a new version
            for phi in &mut phis[block_id] {
                phi.dst = new_version(phi.base_reg, &mut next_ssa_reg, &mut base_reg, &mut stacks);
            }

            // Process opcodes in the block
            let block = &cfg.blocks[block_id];
            for i in block.start..=block.end {
                // Rename reads first (before writes, important for Incr/Decr which are pinned anyway)
                rename_reads(&mut ops[i], &stacks, &pinned);

                // Rename writes: create new SSA version
                rename_writes(
                    &mut ops[i],
                    &mut next_ssa_reg,
                    &mut base_reg,
                    &mut stacks,
                    &pinned,
                );
            }

            // Fill in phi sources in successor blocks
            for &succ in &cfg.blocks[block_id].successors {
                for phi in &mut phis[succ] {
                    let cur = current_name(&stacks, phi.base_reg);
                    phi.sources.push((block_id, cur));
                }
            }

            // Push restore entry FIRST (will be processed AFTER children)
            walk_stack.push((block_id, true, saved));

            // Push dominator tree children in reverse order (so first child is processed first)
            for &child in dom.dom_children[block_id].iter().rev() {
                walk_stack.push((child, false, vec![]));
            }
        }

        SSAForm {
            phis,
            num_ssa_regs: next_ssa_reg as usize,
            base_reg,
            num_original_regs: num_regs,
            pinned,
        }
    }

    /// Destroy SSA form: rename all SSA registers back to their base registers.
    /// Phi nodes are discarded.
    pub fn destroy(&self, ops: &mut Vec<Opcode>) {
        for op in ops.iter_mut() {
            rename_to_base(op, &self.base_reg);
        }
    }
}

/// Rename all read registers in an opcode to their current SSA name.
fn rename_reads(op: &mut Opcode, stacks: &[Vec<Reg>], pinned: &HashSet<u32>) {
    macro_rules! ren {
        ($r:expr) => {
            if !pinned.contains(&(*$r).0) {
                if let Some(top) = stacks.get((*$r).0 as usize).and_then(|s| s.last()) {
                    *$r = *top;
                }
            }
        };
    }

    match op {
        Opcode::Mov { src, .. } => { ren!(src); }
        Opcode::Add { a, b, .. } | Opcode::Sub { a, b, .. } | Opcode::Mul { a, b, .. }
        | Opcode::SDiv { a, b, .. } | Opcode::UDiv { a, b, .. }
        | Opcode::SMod { a, b, .. } | Opcode::UMod { a, b, .. }
        | Opcode::Shl { a, b, .. } | Opcode::SShr { a, b, .. } | Opcode::UShr { a, b, .. }
        | Opcode::And { a, b, .. } | Opcode::Or { a, b, .. } | Opcode::Xor { a, b, .. } => {
            ren!(a); ren!(b);
        }
        Opcode::Neg { src, .. } | Opcode::Not { src, .. } => { ren!(src); }
        // Incr/Decr are pinned — skip
        Opcode::Incr { .. } | Opcode::Decr { .. } => {}

        Opcode::Call1 { arg0, .. } => { ren!(arg0); }
        Opcode::Call2 { arg0, arg1, .. } => { ren!(arg0); ren!(arg1); }
        Opcode::Call3 { arg0, arg1, arg2, .. } => { ren!(arg0); ren!(arg1); ren!(arg2); }
        Opcode::Call4 { arg0, arg1, arg2, arg3, .. } => { ren!(arg0); ren!(arg1); ren!(arg2); ren!(arg3); }
        Opcode::CallN { args, .. } | Opcode::CallMethod { args, .. }
        | Opcode::CallThis { args, .. } => {
            for a in args.iter_mut() { ren!(a); }
        }
        Opcode::CallClosure { fun, args, .. } => {
            ren!(fun);
            for a in args.iter_mut() { ren!(a); }
        }

        Opcode::InstanceClosure { obj, .. } => { ren!(obj); }
        Opcode::VirtualClosure { obj, field, .. } => { ren!(obj); ren!(field); }

        Opcode::SetGlobal { src, .. } => { ren!(src); }
        Opcode::Field { obj, .. } => { ren!(obj); }
        Opcode::SetField { obj, src, .. } => { ren!(obj); ren!(src); }
        Opcode::SetThis { src, .. } => { ren!(src); }
        Opcode::DynGet { obj, .. } => { ren!(obj); }
        Opcode::DynSet { obj, src, .. } => { ren!(obj); ren!(src); }

        Opcode::JTrue { cond, .. } | Opcode::JFalse { cond, .. } => { ren!(cond); }
        Opcode::JNull { reg, .. } | Opcode::JNotNull { reg, .. } => { ren!(reg); }
        Opcode::JSLt { a, b, .. } | Opcode::JSGte { a, b, .. }
        | Opcode::JSGt { a, b, .. } | Opcode::JSLte { a, b, .. }
        | Opcode::JULt { a, b, .. } | Opcode::JUGte { a, b, .. }
        | Opcode::JNotLt { a, b, .. } | Opcode::JNotGte { a, b, .. }
        | Opcode::JEq { a, b, .. } | Opcode::JNotEq { a, b, .. } => {
            ren!(a); ren!(b);
        }

        Opcode::ToDyn { src, .. } | Opcode::ToSFloat { src, .. }
        | Opcode::ToUFloat { src, .. } | Opcode::ToInt { src, .. }
        | Opcode::SafeCast { src, .. } | Opcode::UnsafeCast { src, .. }
        | Opcode::ToVirtual { src, .. } => { ren!(src); }

        Opcode::Ret { ret } => { ren!(ret); }
        Opcode::Throw { exc } | Opcode::Rethrow { exc } => { ren!(exc); }
        Opcode::Switch { reg, .. } => { ren!(reg); }
        Opcode::NullCheck { reg } => { ren!(reg); }

        Opcode::GetI8 { bytes, index, .. } | Opcode::GetI16 { bytes, index, .. }
        | Opcode::GetMem { bytes, index, .. } | Opcode::GetArray { array: bytes, index, .. } => {
            ren!(bytes); ren!(index);
        }
        Opcode::SetI8 { bytes, index, src } | Opcode::SetI16 { bytes, index, src }
        | Opcode::SetMem { bytes, index, src } | Opcode::SetArray { array: bytes, index, src } => {
            ren!(bytes); ren!(index); ren!(src);
        }

        Opcode::ArraySize { array, .. } => { ren!(array); }
        Opcode::GetType { src, .. } | Opcode::GetTID { src, .. } => { ren!(src); }
        Opcode::Ref { src, .. } | Opcode::Unref { src, .. } => { ren!(src); }
        Opcode::Setref { dst, value } => { ren!(dst); ren!(value); }

        Opcode::MakeEnum { args, .. } => { for a in args.iter_mut() { ren!(a); } }
        Opcode::EnumIndex { value, .. } | Opcode::EnumField { value, .. } => { ren!(value); }
        Opcode::SetEnumField { value, src, .. } => { ren!(value); ren!(src); }

        Opcode::RefData { src, .. } => { ren!(src); }
        Opcode::RefOffset { reg, offset, .. } => { ren!(reg); ren!(offset); }
        Opcode::Prefetch { value, .. } => { ren!(value); }

        Opcode::Int { .. } | Opcode::Float { .. } | Opcode::Bool { .. }
        | Opcode::Bytes { .. } | Opcode::String { .. } | Opcode::Null { .. }
        | Opcode::Call0 { .. } | Opcode::StaticClosure { .. }
        | Opcode::GetGlobal { .. } | Opcode::GetThis { .. }
        | Opcode::JAlways { .. } | Opcode::Label | Opcode::New { .. }
        | Opcode::Type { .. } | Opcode::EnumAlloc { .. } | Opcode::Assert
        | Opcode::Nop | Opcode::Asm { .. } | Opcode::Trap { .. } | Opcode::EndTrap { .. } => {}
    }
}

/// Rename written registers to new SSA versions.
fn rename_writes(
    op: &mut Opcode,
    next_ssa_reg: &mut u32,
    base_reg_map: &mut Vec<u32>,
    stacks: &mut [Vec<Reg>],
    pinned: &HashSet<u32>,
) {
    let written = opcode_info::writes(op);
    for w in written {
        if pinned.contains(&w.0) { continue; }
        // Create new SSA version
        let base = w.0;
        let ssa_id = *next_ssa_reg;
        *next_ssa_reg += 1;
        base_reg_map.push(base);
        let new_reg = Reg(ssa_id);
        stacks[base as usize].push(new_reg);

        // Substitute in the opcode
        set_write(op, w, new_reg);
    }
}

/// Set the write register of an opcode from `old` to `new_reg`.
fn set_write(op: &mut Opcode, old: Reg, new_reg: Reg) {
    macro_rules! sw {
        ($r:expr) => { if *$r == old { *$r = new_reg; } };
    }

    match op {
        Opcode::Mov { dst, .. } | Opcode::Int { dst, .. } | Opcode::Float { dst, .. }
        | Opcode::Bool { dst, .. } | Opcode::Bytes { dst, .. } | Opcode::String { dst, .. }
        | Opcode::Null { dst } | Opcode::Add { dst, .. } | Opcode::Sub { dst, .. }
        | Opcode::Mul { dst, .. } | Opcode::SDiv { dst, .. } | Opcode::UDiv { dst, .. }
        | Opcode::SMod { dst, .. } | Opcode::UMod { dst, .. }
        | Opcode::Shl { dst, .. } | Opcode::SShr { dst, .. } | Opcode::UShr { dst, .. }
        | Opcode::And { dst, .. } | Opcode::Or { dst, .. } | Opcode::Xor { dst, .. }
        | Opcode::Neg { dst, .. } | Opcode::Not { dst, .. }
        | Opcode::Call0 { dst, .. } | Opcode::Call1 { dst, .. } | Opcode::Call2 { dst, .. }
        | Opcode::Call3 { dst, .. } | Opcode::Call4 { dst, .. } | Opcode::CallN { dst, .. }
        | Opcode::CallMethod { dst, .. } | Opcode::CallThis { dst, .. }
        | Opcode::CallClosure { dst, .. }
        | Opcode::StaticClosure { dst, .. } | Opcode::InstanceClosure { dst, .. }
        | Opcode::VirtualClosure { dst, .. }
        | Opcode::GetGlobal { dst, .. } | Opcode::Field { dst, .. }
        | Opcode::GetThis { dst, .. } | Opcode::DynGet { dst, .. }
        | Opcode::ToDyn { dst, .. } | Opcode::ToSFloat { dst, .. }
        | Opcode::ToUFloat { dst, .. } | Opcode::ToInt { dst, .. }
        | Opcode::SafeCast { dst, .. } | Opcode::UnsafeCast { dst, .. }
        | Opcode::ToVirtual { dst, .. }
        | Opcode::New { dst } | Opcode::ArraySize { dst, .. }
        | Opcode::Type { dst, .. } | Opcode::GetType { dst, .. } | Opcode::GetTID { dst, .. }
        | Opcode::Ref { dst, .. } | Opcode::Unref { dst, .. }
        | Opcode::MakeEnum { dst, .. } | Opcode::EnumAlloc { dst, .. }
        | Opcode::EnumIndex { dst, .. } | Opcode::EnumField { dst, .. }
        | Opcode::RefData { dst, .. } | Opcode::RefOffset { dst, .. }
        | Opcode::GetI8 { dst, .. } | Opcode::GetI16 { dst, .. }
        | Opcode::GetMem { dst, .. } | Opcode::GetArray { dst, .. } => {
            sw!(dst);
        }
        Opcode::Trap { exc, .. } => { sw!(exc); }
        // Incr/Decr pinned, everything else has no write
        _ => {}
    }
}

/// Rename all registers in an opcode back to their base registers (de-SSA).
fn rename_to_base(op: &mut Opcode, base_reg_map: &[u32]) {
    macro_rules! to_base {
        ($r:expr) => {
            if ((*$r).0 as usize) < base_reg_map.len() {
                *$r = Reg(base_reg_map[(*$r).0 as usize]);
            }
        };
    }

    match op {
        Opcode::Mov { dst, src } => { to_base!(dst); to_base!(src); }
        Opcode::Int { dst, .. } | Opcode::Float { dst, .. }
        | Opcode::Bool { dst, .. } | Opcode::Bytes { dst, .. }
        | Opcode::String { dst, .. } | Opcode::Null { dst } => { to_base!(dst); }

        Opcode::Add { dst, a, b } | Opcode::Sub { dst, a, b } | Opcode::Mul { dst, a, b }
        | Opcode::SDiv { dst, a, b } | Opcode::UDiv { dst, a, b }
        | Opcode::SMod { dst, a, b } | Opcode::UMod { dst, a, b }
        | Opcode::Shl { dst, a, b } | Opcode::SShr { dst, a, b } | Opcode::UShr { dst, a, b }
        | Opcode::And { dst, a, b } | Opcode::Or { dst, a, b } | Opcode::Xor { dst, a, b } => {
            to_base!(dst); to_base!(a); to_base!(b);
        }
        Opcode::Neg { dst, src } | Opcode::Not { dst, src } => { to_base!(dst); to_base!(src); }
        Opcode::Incr { dst } | Opcode::Decr { dst } => { to_base!(dst); }

        Opcode::Call0 { dst, .. } => { to_base!(dst); }
        Opcode::Call1 { dst, arg0, .. } => { to_base!(dst); to_base!(arg0); }
        Opcode::Call2 { dst, arg0, arg1, .. } => { to_base!(dst); to_base!(arg0); to_base!(arg1); }
        Opcode::Call3 { dst, arg0, arg1, arg2, .. } => { to_base!(dst); to_base!(arg0); to_base!(arg1); to_base!(arg2); }
        Opcode::Call4 { dst, arg0, arg1, arg2, arg3, .. } => { to_base!(dst); to_base!(arg0); to_base!(arg1); to_base!(arg2); to_base!(arg3); }
        Opcode::CallN { dst, args, .. } | Opcode::CallMethod { dst, args, .. }
        | Opcode::CallThis { dst, args, .. } => {
            to_base!(dst);
            for a in args.iter_mut() { to_base!(a); }
        }
        Opcode::CallClosure { dst, fun, args } => {
            to_base!(dst); to_base!(fun);
            for a in args.iter_mut() { to_base!(a); }
        }

        Opcode::StaticClosure { dst, .. } => { to_base!(dst); }
        Opcode::InstanceClosure { dst, obj, .. } => { to_base!(dst); to_base!(obj); }
        Opcode::VirtualClosure { dst, obj, field } => { to_base!(dst); to_base!(obj); to_base!(field); }

        Opcode::GetGlobal { dst, .. } => { to_base!(dst); }
        Opcode::SetGlobal { src, .. } => { to_base!(src); }
        Opcode::Field { dst, obj, .. } => { to_base!(dst); to_base!(obj); }
        Opcode::SetField { obj, src, .. } => { to_base!(obj); to_base!(src); }
        Opcode::GetThis { dst, .. } => { to_base!(dst); }
        Opcode::SetThis { src, .. } => { to_base!(src); }
        Opcode::DynGet { dst, obj, .. } => { to_base!(dst); to_base!(obj); }
        Opcode::DynSet { obj, src, .. } => { to_base!(obj); to_base!(src); }

        Opcode::JTrue { cond, .. } | Opcode::JFalse { cond, .. } => { to_base!(cond); }
        Opcode::JNull { reg, .. } | Opcode::JNotNull { reg, .. } => { to_base!(reg); }
        Opcode::JSLt { a, b, .. } | Opcode::JSGte { a, b, .. }
        | Opcode::JSGt { a, b, .. } | Opcode::JSLte { a, b, .. }
        | Opcode::JULt { a, b, .. } | Opcode::JUGte { a, b, .. }
        | Opcode::JNotLt { a, b, .. } | Opcode::JNotGte { a, b, .. }
        | Opcode::JEq { a, b, .. } | Opcode::JNotEq { a, b, .. } => {
            to_base!(a); to_base!(b);
        }

        Opcode::ToDyn { dst, src } | Opcode::ToSFloat { dst, src }
        | Opcode::ToUFloat { dst, src } | Opcode::ToInt { dst, src }
        | Opcode::SafeCast { dst, src } | Opcode::UnsafeCast { dst, src }
        | Opcode::ToVirtual { dst, src } => { to_base!(dst); to_base!(src); }

        Opcode::Ret { ret } => { to_base!(ret); }
        Opcode::Throw { exc } | Opcode::Rethrow { exc } => { to_base!(exc); }
        Opcode::Switch { reg, .. } => { to_base!(reg); }
        Opcode::NullCheck { reg } => { to_base!(reg); }
        Opcode::Trap { exc, .. } => { to_base!(exc); }
        Opcode::EndTrap { exc } => { to_base!(exc); }

        Opcode::GetI8 { dst, bytes, index } | Opcode::GetI16 { dst, bytes, index }
        | Opcode::GetMem { dst, bytes, index } | Opcode::GetArray { dst, array: bytes, index } => {
            to_base!(dst); to_base!(bytes); to_base!(index);
        }
        Opcode::SetI8 { bytes, index, src } | Opcode::SetI16 { bytes, index, src }
        | Opcode::SetMem { bytes, index, src } | Opcode::SetArray { array: bytes, index, src } => {
            to_base!(bytes); to_base!(index); to_base!(src);
        }

        Opcode::New { dst } => { to_base!(dst); }
        Opcode::ArraySize { dst, array } => { to_base!(dst); to_base!(array); }
        Opcode::Type { dst, .. } => { to_base!(dst); }
        Opcode::GetType { dst, src } | Opcode::GetTID { dst, src } => { to_base!(dst); to_base!(src); }
        Opcode::Ref { dst, src } | Opcode::Unref { dst, src } => { to_base!(dst); to_base!(src); }
        Opcode::Setref { dst, value } => { to_base!(dst); to_base!(value); }

        Opcode::MakeEnum { dst, args, .. } => {
            to_base!(dst);
            for a in args.iter_mut() { to_base!(a); }
        }
        Opcode::EnumAlloc { dst, .. } => { to_base!(dst); }
        Opcode::EnumIndex { dst, value } => { to_base!(dst); to_base!(value); }
        Opcode::EnumField { dst, value, .. } => { to_base!(dst); to_base!(value); }
        Opcode::SetEnumField { value, src, .. } => { to_base!(value); to_base!(src); }

        Opcode::RefData { dst, src } => { to_base!(dst); to_base!(src); }
        Opcode::RefOffset { dst, reg, offset } => { to_base!(dst); to_base!(reg); to_base!(offset); }
        Opcode::Prefetch { value, .. } => { to_base!(value); }

        Opcode::JAlways { .. } | Opcode::Label | Opcode::Assert
        | Opcode::Nop | Opcode::Asm { .. } => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cfg::CFG;
    use crate::dominance::DominatorTree;
    use crate::opcodes::*;

    #[test]
    fn test_linear_no_phis() {
        let mut ops = vec![
            Opcode::Int { dst: Reg(0), ptr: RefInt(0) },
            Opcode::Int { dst: Reg(1), ptr: RefInt(1) },
            Opcode::Add { dst: Reg(2), a: Reg(0), b: Reg(1) },
            Opcode::Ret { ret: Reg(2) },
        ];
        let cfg = CFG::build(&ops);
        let dom = DominatorTree::build(&cfg);
        let ssa = SSAForm::construct(&mut ops, 3, &cfg, &dom);

        // Single block: no phi nodes needed
        assert!(ssa.phis[0].is_empty());
        // All registers should be renamed (version > original)
        assert!(ssa.num_ssa_regs > 3);
    }

    #[test]
    fn test_diamond_phi_insertion() {
        // Diamond CFG:
        // Block 0: JTrue → block 2
        // Block 1: Int(r1, 0), JAlways → block 3
        // Block 2: Int(r1, 1)
        // Block 3: Ret(r1) — needs phi for r1
        let mut ops = vec![
            Opcode::JTrue { cond: Reg(0), offset: 2 },
            Opcode::Int { dst: Reg(1), ptr: RefInt(0) },
            Opcode::JAlways { offset: 1 },
            Opcode::Int { dst: Reg(1), ptr: RefInt(1) },
            Opcode::Ret { ret: Reg(1) },
        ];
        let cfg = CFG::build(&ops);
        let dom = DominatorTree::build(&cfg);
        let ssa = SSAForm::construct(&mut ops, 2, &cfg, &dom);

        // The join block (block 3, op4) should have a phi for register 1
        let join_block = cfg.block_of[4];
        let r1_phis: Vec<_> = ssa.phis[join_block].iter().filter(|p| p.base_reg == 1).collect();
        assert!(!r1_phis.is_empty(), "expected phi for r1 at join block");
        assert_eq!(r1_phis[0].sources.len(), 2, "phi should have 2 sources");
    }

    #[test]
    fn test_round_trip_identity() {
        // Construct SSA then destroy — should produce equivalent opcodes
        let original_ops = vec![
            Opcode::Int { dst: Reg(0), ptr: RefInt(0) },
            Opcode::Int { dst: Reg(1), ptr: RefInt(1) },
            Opcode::Add { dst: Reg(2), a: Reg(0), b: Reg(1) },
            Opcode::Ret { ret: Reg(2) },
        ];
        let mut ops = original_ops.clone();
        let cfg = CFG::build(&ops);
        let dom = DominatorTree::build(&cfg);
        let ssa = SSAForm::construct(&mut ops, 3, &cfg, &dom);
        ssa.destroy(&mut ops);

        // After round-trip, registers should be back to base registers
        if let Opcode::Int { dst, .. } = &ops[0] { assert_eq!(*dst, Reg(0)); }
        if let Opcode::Int { dst, .. } = &ops[1] { assert_eq!(*dst, Reg(1)); }
        if let Opcode::Add { dst, a, b } = &ops[2] {
            assert_eq!(*dst, Reg(2));
            assert_eq!(*a, Reg(0));
            assert_eq!(*b, Reg(1));
        }
        if let Opcode::Ret { ret } = &ops[3] { assert_eq!(*ret, Reg(2)); }
    }

    #[test]
    fn test_round_trip_diamond() {
        let original_ops = vec![
            Opcode::JTrue { cond: Reg(0), offset: 2 },
            Opcode::Int { dst: Reg(1), ptr: RefInt(0) },
            Opcode::JAlways { offset: 1 },
            Opcode::Int { dst: Reg(1), ptr: RefInt(1) },
            Opcode::Ret { ret: Reg(1) },
        ];
        let mut ops = original_ops.clone();
        let cfg = CFG::build(&ops);
        let dom = DominatorTree::build(&cfg);
        let ssa = SSAForm::construct(&mut ops, 2, &cfg, &dom);
        ssa.destroy(&mut ops);

        // After round-trip, all registers back to base
        if let Opcode::JTrue { cond, .. } = &ops[0] { assert_eq!(*cond, Reg(0)); }
        if let Opcode::Int { dst, .. } = &ops[1] { assert_eq!(*dst, Reg(1)); }
        if let Opcode::Int { dst, .. } = &ops[3] { assert_eq!(*dst, Reg(1)); }
        if let Opcode::Ret { ret } = &ops[4] { assert_eq!(*ret, Reg(1)); }
    }

    #[test]
    fn test_ssa_registers_unique_defs() {
        // In SSA, each register should be written exactly once
        let mut ops = vec![
            Opcode::Int { dst: Reg(0), ptr: RefInt(0) },
            Opcode::Int { dst: Reg(0), ptr: RefInt(1) },  // second write to r0
            Opcode::Ret { ret: Reg(0) },
        ];
        let cfg = CFG::build(&ops);
        let dom = DominatorTree::build(&cfg);
        let ssa = SSAForm::construct(&mut ops, 1, &cfg, &dom);

        // The two Int dsts should have different SSA register IDs
        let dst0 = if let Opcode::Int { dst, .. } = &ops[0] { *dst } else { panic!() };
        let dst1 = if let Opcode::Int { dst, .. } = &ops[1] { *dst } else { panic!() };
        assert_ne!(dst0, dst1, "SSA should give distinct register IDs to each def");

        // The Ret should read the SECOND version
        let ret_reg = if let Opcode::Ret { ret } = &ops[2] { *ret } else { panic!() };
        assert_eq!(ret_reg, dst1, "Ret should read the latest version");

        // Both map back to base reg 0
        assert_eq!(ssa.base_reg[dst0.0 as usize], 0);
        assert_eq!(ssa.base_reg[dst1.0 as usize], 0);
    }
}
