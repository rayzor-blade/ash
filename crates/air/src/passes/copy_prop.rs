use crate::cfg::CFG;
use crate::opcodes::{Opcode, Reg};
use crate::opcode_info;
use crate::pass::Pass;
use std::collections::HashMap;

pub struct CopyPropPass;

/// Substitute register `from` with `to` in all read positions of an opcode.
/// Returns true if any substitution was made.
pub fn substitute_reads(op: &mut Opcode, from: Reg, to: Reg) -> bool {
    let mut changed = false;

    macro_rules! sub {
        ($r:expr) => {
            if *$r == from { *$r = to; changed = true; }
        };
    }

    match op {
        Opcode::Mov { src, .. } => { sub!(src); }
        Opcode::Add { a, b, .. } | Opcode::Sub { a, b, .. } | Opcode::Mul { a, b, .. }
        | Opcode::SDiv { a, b, .. } | Opcode::UDiv { a, b, .. }
        | Opcode::SMod { a, b, .. } | Opcode::UMod { a, b, .. }
        | Opcode::Shl { a, b, .. } | Opcode::SShr { a, b, .. } | Opcode::UShr { a, b, .. }
        | Opcode::And { a, b, .. } | Opcode::Or { a, b, .. } | Opcode::Xor { a, b, .. } => {
            sub!(a); sub!(b);
        }
        Opcode::Neg { src, .. } | Opcode::Not { src, .. } => { sub!(src); }
        Opcode::Incr { dst } | Opcode::Decr { dst } => { sub!(dst); }

        Opcode::Call1 { arg0, .. } => { sub!(arg0); }
        Opcode::Call2 { arg0, arg1, .. } => { sub!(arg0); sub!(arg1); }
        Opcode::Call3 { arg0, arg1, arg2, .. } => { sub!(arg0); sub!(arg1); sub!(arg2); }
        Opcode::Call4 { arg0, arg1, arg2, arg3, .. } => { sub!(arg0); sub!(arg1); sub!(arg2); sub!(arg3); }
        Opcode::CallN { args, .. } | Opcode::CallMethod { args, .. }
        | Opcode::CallThis { args, .. } => {
            for a in args.iter_mut() { sub!(a); }
        }
        Opcode::CallClosure { fun, args, .. } => {
            sub!(fun);
            for a in args.iter_mut() { sub!(a); }
        }

        Opcode::InstanceClosure { obj, .. } => { sub!(obj); }
        Opcode::VirtualClosure { obj, field, .. } => { sub!(obj); sub!(field); }

        Opcode::SetGlobal { src, .. } => { sub!(src); }
        Opcode::Field { obj, .. } => { sub!(obj); }
        Opcode::SetField { obj, src, .. } => { sub!(obj); sub!(src); }
        Opcode::SetThis { src, .. } => { sub!(src); }
        Opcode::DynGet { obj, .. } => { sub!(obj); }
        Opcode::DynSet { obj, src, .. } => { sub!(obj); sub!(src); }

        Opcode::JTrue { cond, .. } | Opcode::JFalse { cond, .. } => { sub!(cond); }
        Opcode::JNull { reg, .. } | Opcode::JNotNull { reg, .. } => { sub!(reg); }
        Opcode::JSLt { a, b, .. } | Opcode::JSGte { a, b, .. }
        | Opcode::JSGt { a, b, .. } | Opcode::JSLte { a, b, .. }
        | Opcode::JULt { a, b, .. } | Opcode::JUGte { a, b, .. }
        | Opcode::JNotLt { a, b, .. } | Opcode::JNotGte { a, b, .. }
        | Opcode::JEq { a, b, .. } | Opcode::JNotEq { a, b, .. } => {
            sub!(a); sub!(b);
        }

        Opcode::ToDyn { src, .. } | Opcode::ToSFloat { src, .. }
        | Opcode::ToUFloat { src, .. } | Opcode::ToInt { src, .. }
        | Opcode::SafeCast { src, .. } | Opcode::UnsafeCast { src, .. }
        | Opcode::ToVirtual { src, .. } => { sub!(src); }

        Opcode::Ret { ret } => { sub!(ret); }
        Opcode::Throw { exc } | Opcode::Rethrow { exc } => { sub!(exc); }
        Opcode::Switch { reg, .. } => { sub!(reg); }
        Opcode::NullCheck { reg } => { sub!(reg); }

        Opcode::GetI8 { bytes, index, .. } | Opcode::GetI16 { bytes, index, .. }
        | Opcode::GetMem { bytes, index, .. } | Opcode::GetArray { array: bytes, index, .. } => {
            sub!(bytes); sub!(index);
        }
        Opcode::SetI8 { bytes, index, src } | Opcode::SetI16 { bytes, index, src }
        | Opcode::SetMem { bytes, index, src } | Opcode::SetArray { array: bytes, index, src } => {
            sub!(bytes); sub!(index); sub!(src);
        }

        Opcode::ArraySize { array, .. } => { sub!(array); }
        Opcode::GetType { src, .. } | Opcode::GetTID { src, .. } => { sub!(src); }
        Opcode::Ref { src, .. } | Opcode::Unref { src, .. } => { sub!(src); }
        Opcode::Setref { dst, value } => { sub!(dst); sub!(value); }

        Opcode::MakeEnum { args, .. } => { for a in args.iter_mut() { sub!(a); } }
        Opcode::EnumIndex { value, .. } | Opcode::EnumField { value, .. } => { sub!(value); }
        Opcode::SetEnumField { value, src, .. } => { sub!(value); sub!(src); }

        Opcode::RefData { src, .. } => { sub!(src); }
        Opcode::RefOffset { reg, offset, .. } => { sub!(reg); sub!(offset); }
        Opcode::Prefetch { value, .. } => { sub!(value); }

        // No reads to substitute
        Opcode::Int { .. } | Opcode::Float { .. } | Opcode::Bool { .. }
        | Opcode::Bytes { .. } | Opcode::String { .. } | Opcode::Null { .. }
        | Opcode::Call0 { .. } | Opcode::StaticClosure { .. }
        | Opcode::GetGlobal { .. } | Opcode::GetThis { .. }
        | Opcode::JAlways { .. } | Opcode::Label | Opcode::New { .. }
        | Opcode::Type { .. } | Opcode::EnumAlloc { .. } | Opcode::Assert
        | Opcode::Nop | Opcode::Asm { .. } | Opcode::Trap { .. } | Opcode::EndTrap { .. } => {}
    }

    changed
}

impl Pass for CopyPropPass {
    fn name(&self) -> &str { "copy_prop" }

    fn run(&self, ops: &mut Vec<Opcode>, _num_regs: usize, cfg: &CFG) -> usize {
        let mut eliminated = 0;

        for block in &cfg.blocks {
            // Local copy map: dst → src (from Mov instructions)
            let mut copies: HashMap<Reg, Reg> = HashMap::new();

            for i in block.start..=block.end {
                // Apply copy substitutions to reads
                let active_copies: Vec<(Reg, Reg)> = copies.iter().map(|(&k, &v)| (k, v)).collect();
                for (from, to) in &active_copies {
                    substitute_reads(&mut ops[i], *from, *to);
                }

                // If a Mov became a self-move after substitution, eliminate it
                if let Opcode::Mov { dst, src } = &ops[i] {
                    if dst == src {
                        ops[i] = Opcode::Nop;
                        eliminated += 1;
                        continue;
                    }
                }

                // Invalidate mappings where the written register is either key or value
                let written = opcode_info::writes(&ops[i]);
                for w in &written {
                    copies.remove(w);
                    copies.retain(|_, v| v != w);
                }

                // Record new copy from Mov
                if let Opcode::Mov { dst, src } = &ops[i] {
                    copies.insert(*dst, *src);
                }
            }
        }

        eliminated
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::opcodes::*;

    #[test]
    fn test_copy_prop_substitution() {
        let mut ops = vec![
            Opcode::Mov { dst: Reg(1), src: Reg(0) },
            Opcode::Add { dst: Reg(2), a: Reg(1), b: Reg(1) },
            Opcode::Ret { ret: Reg(2) },
        ];
        let cfg = CFG::build(&ops);
        let pass = CopyPropPass;
        pass.run(&mut ops, 3, &cfg);
        // After copy prop: Add should use Reg(0) instead of Reg(1)
        if let Opcode::Add { a, b, .. } = &ops[1] {
            assert_eq!(*a, Reg(0));
            assert_eq!(*b, Reg(0));
        } else {
            panic!("expected Add");
        }
    }

    #[test]
    fn test_self_move_eliminated() {
        // Mov(r1, r0), then Mov(r2, r1) → after copy prop r2←r0, but Mov(r1,r0) stays
        // Self-move: Mov(r0, r0) → Nop
        let mut ops = vec![
            Opcode::Mov { dst: Reg(0), src: Reg(1) },
            Opcode::Mov { dst: Reg(1), src: Reg(0) },  // after sub: Mov(r1, r1) → Nop
            Opcode::Ret { ret: Reg(1) },
        ];
        let cfg = CFG::build(&ops);
        let pass = CopyPropPass;
        let elim = pass.run(&mut ops, 2, &cfg);
        // Mov(r0, r1): copies={r0→r1}
        // Mov(r1, r0): substitute r0→r1 in reads: src=r0 becomes r1. So Mov(r1, r1) → Nop
        assert_eq!(elim, 1);
        assert!(matches!(ops[1], Opcode::Nop));
    }

    #[test]
    fn test_copy_invalidated_by_overwrite() {
        let mut ops = vec![
            Opcode::Mov { dst: Reg(1), src: Reg(0) },
            Opcode::Int { dst: Reg(0), ptr: RefInt(42) },  // overwrites r0
            Opcode::Add { dst: Reg(2), a: Reg(1), b: Reg(0) },
            Opcode::Ret { ret: Reg(2) },
        ];
        let cfg = CFG::build(&ops);
        let pass = CopyPropPass;
        pass.run(&mut ops, 3, &cfg);
        // Copy r1→r0 should be invalidated by Int overwriting r0.
        // So Add should NOT substitute r1→r0 (r0 has been changed).
        if let Opcode::Add { a, b, .. } = &ops[2] {
            assert_eq!(*a, Reg(1)); // NOT substituted
            assert_eq!(*b, Reg(0));
        } else {
            panic!("expected Add");
        }
    }
}
