use crate::opcodes::{Opcode, Reg};

/// Returns the list of registers read by this opcode.
pub fn reads(op: &Opcode) -> Vec<Reg> {
    match op {
        Opcode::Mov { src, .. } => vec![*src],
        Opcode::Int { .. } | Opcode::Float { .. } | Opcode::Bool { .. }
        | Opcode::Bytes { .. } | Opcode::String { .. } | Opcode::Null { .. } => vec![],

        Opcode::Add { a, b, .. } | Opcode::Sub { a, b, .. } | Opcode::Mul { a, b, .. }
        | Opcode::SDiv { a, b, .. } | Opcode::UDiv { a, b, .. }
        | Opcode::SMod { a, b, .. } | Opcode::UMod { a, b, .. }
        | Opcode::Shl { a, b, .. } | Opcode::SShr { a, b, .. } | Opcode::UShr { a, b, .. }
        | Opcode::And { a, b, .. } | Opcode::Or { a, b, .. } | Opcode::Xor { a, b, .. } => vec![*a, *b],

        Opcode::Neg { src, .. } | Opcode::Not { src, .. } => vec![*src],
        Opcode::Incr { dst } | Opcode::Decr { dst } => vec![*dst],

        Opcode::Call0 { .. } => vec![],
        Opcode::Call1 { arg0, .. } => vec![*arg0],
        Opcode::Call2 { arg0, arg1, .. } => vec![*arg0, *arg1],
        Opcode::Call3 { arg0, arg1, arg2, .. } => vec![*arg0, *arg1, *arg2],
        Opcode::Call4 { arg0, arg1, arg2, arg3, .. } => vec![*arg0, *arg1, *arg2, *arg3],
        Opcode::CallN { args, .. } => args.clone(),
        Opcode::CallMethod { args, .. } => args.clone(),
        Opcode::CallThis { args, .. } => {
            let mut r = vec![Reg(0)]; // this = reg0
            r.extend(args.iter());
            r
        }
        Opcode::CallClosure { fun, args, .. } => {
            let mut r = vec![*fun];
            r.extend(args.iter());
            r
        }

        Opcode::StaticClosure { .. } => vec![],
        Opcode::InstanceClosure { obj, .. } => vec![*obj],
        Opcode::VirtualClosure { obj, field, .. } => vec![*obj, *field],

        Opcode::GetGlobal { .. } => vec![],
        Opcode::SetGlobal { src, .. } => vec![*src],

        Opcode::Field { obj, .. } => vec![*obj],
        Opcode::SetField { obj, src, .. } => vec![*obj, *src],
        Opcode::GetThis { .. } => vec![Reg(0)],
        Opcode::SetThis { src, .. } => vec![Reg(0), *src],

        Opcode::DynGet { obj, .. } => vec![*obj],
        Opcode::DynSet { obj, src, .. } => vec![*obj, *src],

        Opcode::JTrue { cond, .. } | Opcode::JFalse { cond, .. } => vec![*cond],
        Opcode::JNull { reg, .. } | Opcode::JNotNull { reg, .. } => vec![*reg],
        Opcode::JSLt { a, b, .. } | Opcode::JSGte { a, b, .. }
        | Opcode::JSGt { a, b, .. } | Opcode::JSLte { a, b, .. }
        | Opcode::JULt { a, b, .. } | Opcode::JUGte { a, b, .. }
        | Opcode::JNotLt { a, b, .. } | Opcode::JNotGte { a, b, .. }
        | Opcode::JEq { a, b, .. } | Opcode::JNotEq { a, b, .. } => vec![*a, *b],
        Opcode::JAlways { .. } => vec![],

        Opcode::ToDyn { src, .. } | Opcode::ToSFloat { src, .. }
        | Opcode::ToUFloat { src, .. } | Opcode::ToInt { src, .. }
        | Opcode::SafeCast { src, .. } | Opcode::UnsafeCast { src, .. }
        | Opcode::ToVirtual { src, .. } => vec![*src],

        Opcode::Label => vec![],
        Opcode::Ret { ret } => vec![*ret],
        Opcode::Throw { exc } | Opcode::Rethrow { exc } => vec![*exc],
        Opcode::Switch { reg, .. } => vec![*reg],
        Opcode::NullCheck { reg } => vec![*reg],
        Opcode::Trap { .. } => vec![],
        Opcode::EndTrap { .. } => vec![],

        Opcode::GetI8 { bytes, index, .. } | Opcode::GetI16 { bytes, index, .. }
        | Opcode::GetMem { bytes, index, .. } | Opcode::GetArray { array: bytes, index, .. } => vec![*bytes, *index],

        Opcode::SetI8 { bytes, index, src } | Opcode::SetI16 { bytes, index, src }
        | Opcode::SetMem { bytes, index, src } | Opcode::SetArray { array: bytes, index, src } => vec![*bytes, *index, *src],

        Opcode::New { .. } => vec![],
        Opcode::ArraySize { array, .. } => vec![*array],
        Opcode::Type { .. } => vec![],
        Opcode::GetType { src, .. } | Opcode::GetTID { src, .. } => vec![*src],

        Opcode::Ref { src, .. } => vec![*src],
        Opcode::Unref { src, .. } => vec![*src],
        Opcode::Setref { dst, value } => vec![*dst, *value],

        Opcode::MakeEnum { args, .. } => args.clone(),
        Opcode::EnumAlloc { .. } => vec![],
        Opcode::EnumIndex { value, .. } => vec![*value],
        Opcode::EnumField { value, .. } => vec![*value],
        Opcode::SetEnumField { value, src, .. } => vec![*value, *src],

        Opcode::Assert => vec![],
        Opcode::RefData { src, .. } => vec![*src],
        Opcode::RefOffset { reg, offset, .. } => vec![*reg, *offset],
        Opcode::Nop => vec![],
        Opcode::Prefetch { value, .. } => vec![*value],
        Opcode::Asm { .. } => vec![],
    }
}

/// Returns the list of registers written by this opcode.
pub fn writes(op: &Opcode) -> Vec<Reg> {
    match op {
        Opcode::Mov { dst, .. } | Opcode::Int { dst, .. } | Opcode::Float { dst, .. }
        | Opcode::Bool { dst, .. } | Opcode::Bytes { dst, .. } | Opcode::String { dst, .. }
        | Opcode::Null { dst, .. } => vec![*dst],

        Opcode::Add { dst, .. } | Opcode::Sub { dst, .. } | Opcode::Mul { dst, .. }
        | Opcode::SDiv { dst, .. } | Opcode::UDiv { dst, .. }
        | Opcode::SMod { dst, .. } | Opcode::UMod { dst, .. }
        | Opcode::Shl { dst, .. } | Opcode::SShr { dst, .. } | Opcode::UShr { dst, .. }
        | Opcode::And { dst, .. } | Opcode::Or { dst, .. } | Opcode::Xor { dst, .. } => vec![*dst],

        Opcode::Neg { dst, .. } | Opcode::Not { dst, .. } => vec![*dst],
        Opcode::Incr { dst } | Opcode::Decr { dst } => vec![*dst],

        Opcode::Call0 { dst, .. } | Opcode::Call1 { dst, .. } | Opcode::Call2 { dst, .. }
        | Opcode::Call3 { dst, .. } | Opcode::Call4 { dst, .. } | Opcode::CallN { dst, .. }
        | Opcode::CallMethod { dst, .. } | Opcode::CallThis { dst, .. }
        | Opcode::CallClosure { dst, .. } => vec![*dst],

        Opcode::StaticClosure { dst, .. } | Opcode::InstanceClosure { dst, .. }
        | Opcode::VirtualClosure { dst, .. } => vec![*dst],

        Opcode::GetGlobal { dst, .. } => vec![*dst],
        Opcode::SetGlobal { .. } => vec![],

        Opcode::Field { dst, .. } | Opcode::GetThis { dst, .. } => vec![*dst],
        Opcode::SetField { .. } | Opcode::SetThis { .. } => vec![],
        Opcode::DynGet { dst, .. } => vec![*dst],
        Opcode::DynSet { .. } => vec![],

        Opcode::JTrue { .. } | Opcode::JFalse { .. } | Opcode::JNull { .. }
        | Opcode::JNotNull { .. } | Opcode::JSLt { .. } | Opcode::JSGte { .. }
        | Opcode::JSGt { .. } | Opcode::JSLte { .. } | Opcode::JULt { .. }
        | Opcode::JUGte { .. } | Opcode::JNotLt { .. } | Opcode::JNotGte { .. }
        | Opcode::JEq { .. } | Opcode::JNotEq { .. } | Opcode::JAlways { .. } => vec![],

        Opcode::ToDyn { dst, .. } | Opcode::ToSFloat { dst, .. }
        | Opcode::ToUFloat { dst, .. } | Opcode::ToInt { dst, .. }
        | Opcode::SafeCast { dst, .. } | Opcode::UnsafeCast { dst, .. }
        | Opcode::ToVirtual { dst, .. } => vec![*dst],

        Opcode::Label => vec![],
        Opcode::Ret { .. } => vec![],
        Opcode::Throw { .. } | Opcode::Rethrow { .. } => vec![],
        Opcode::Switch { .. } => vec![],
        Opcode::NullCheck { .. } => vec![],
        Opcode::Trap { exc, .. } => vec![*exc],
        Opcode::EndTrap { .. } => vec![],

        Opcode::GetI8 { dst, .. } | Opcode::GetI16 { dst, .. }
        | Opcode::GetMem { dst, .. } | Opcode::GetArray { dst, .. } => vec![*dst],
        Opcode::SetI8 { .. } | Opcode::SetI16 { .. }
        | Opcode::SetMem { .. } | Opcode::SetArray { .. } => vec![],

        Opcode::New { dst } => vec![*dst],
        Opcode::ArraySize { dst, .. } => vec![*dst],
        Opcode::Type { dst, .. } => vec![*dst],
        Opcode::GetType { dst, .. } | Opcode::GetTID { dst, .. } => vec![*dst],

        Opcode::Ref { dst, .. } | Opcode::Unref { dst, .. } => vec![*dst],
        Opcode::Setref { .. } => vec![],

        Opcode::MakeEnum { dst, .. } | Opcode::EnumAlloc { dst, .. } => vec![*dst],
        Opcode::EnumIndex { dst, .. } | Opcode::EnumField { dst, .. } => vec![*dst],
        Opcode::SetEnumField { .. } => vec![],

        Opcode::Assert => vec![],
        Opcode::RefData { dst, .. } | Opcode::RefOffset { dst, .. } => vec![*dst],
        Opcode::Nop => vec![],
        Opcode::Prefetch { .. } => vec![],
        Opcode::Asm { .. } => vec![],
    }
}

/// Returns true if this opcode is a terminator (no fallthrough to next opcode).
pub fn is_terminator(op: &Opcode) -> bool {
    matches!(op, Opcode::Ret { .. } | Opcode::Throw { .. } | Opcode::Rethrow { .. } | Opcode::JAlways { .. })
}

/// Returns true if this opcode is a conditional branch.
pub fn is_conditional_branch(op: &Opcode) -> bool {
    matches!(op,
        Opcode::JTrue { .. } | Opcode::JFalse { .. } | Opcode::JNull { .. }
        | Opcode::JNotNull { .. } | Opcode::JSLt { .. } | Opcode::JSGte { .. }
        | Opcode::JSGt { .. } | Opcode::JSLte { .. } | Opcode::JULt { .. }
        | Opcode::JUGte { .. } | Opcode::JNotLt { .. } | Opcode::JNotGte { .. }
        | Opcode::JEq { .. } | Opcode::JNotEq { .. }
        | Opcode::Switch { .. } | Opcode::Trap { .. }
    )
}

/// Returns the jump offset for unconditional/conditional jumps, None for non-jump opcodes.
pub fn jump_offset(op: &Opcode) -> Option<i32> {
    match op {
        Opcode::JTrue { offset, .. } | Opcode::JFalse { offset, .. }
        | Opcode::JNull { offset, .. } | Opcode::JNotNull { offset, .. }
        | Opcode::JSLt { offset, .. } | Opcode::JSGte { offset, .. }
        | Opcode::JSGt { offset, .. } | Opcode::JSLte { offset, .. }
        | Opcode::JULt { offset, .. } | Opcode::JUGte { offset, .. }
        | Opcode::JNotLt { offset, .. } | Opcode::JNotGte { offset, .. }
        | Opcode::JEq { offset, .. } | Opcode::JNotEq { offset, .. }
        | Opcode::JAlways { offset }
        | Opcode::Trap { offset, .. } => Some(*offset),
        _ => None,
    }
}

/// Returns all possible successor opcode indices from position `i`.
/// For conditional branches: both the fallthrough and the jump target.
/// For unconditional jumps: just the target.
/// For terminators (Ret/Throw): empty.
/// For everything else: just i+1.
pub fn successors(op: &Opcode, i: usize, num_ops: usize) -> Vec<usize> {
    match op {
        Opcode::JAlways { offset } => {
            vec![(i as i32 + 1 + offset) as usize]
        }
        Opcode::Ret { .. } | Opcode::Throw { .. } | Opcode::Rethrow { .. } => {
            vec![]
        }
        Opcode::Switch { offsets, end, .. } => {
            let mut targets: Vec<usize> = offsets.iter()
                .map(|off| (i as i32 + 1 + off) as usize)
                .collect();
            targets.push((i as i32 + 1 + end) as usize);
            targets.sort();
            targets.dedup();
            targets
        }
        Opcode::Trap { offset, .. } => {
            // Normal fallthrough + exception handler target
            let mut targets = vec![i + 1];
            let handler = (i as i32 + 1 + offset) as usize;
            if handler != i + 1 {
                targets.push(handler);
            }
            targets
        }
        _ => {
            if let Some(offset) = jump_offset(op) {
                // Conditional branch: fallthrough + target
                let target = (i as i32 + 1 + offset) as usize;
                if target == i + 1 {
                    vec![i + 1]
                } else {
                    vec![i + 1, target]
                }
            } else if i + 1 <= num_ops {
                vec![i + 1]
            } else {
                vec![]
            }
        }
    }
}

/// Returns true if this opcode is "pure" (no side effects beyond writing its dst register).
/// Pure opcodes can be safely eliminated if their output is dead.
pub fn is_pure(op: &Opcode) -> bool {
    matches!(op,
        Opcode::Mov { .. } | Opcode::Int { .. } | Opcode::Float { .. }
        | Opcode::Bool { .. } | Opcode::Bytes { .. } | Opcode::String { .. }
        | Opcode::Null { .. }
        | Opcode::Add { .. } | Opcode::Sub { .. } | Opcode::Mul { .. }
        | Opcode::SDiv { .. } | Opcode::UDiv { .. }
        | Opcode::SMod { .. } | Opcode::UMod { .. }
        | Opcode::Shl { .. } | Opcode::SShr { .. } | Opcode::UShr { .. }
        | Opcode::And { .. } | Opcode::Or { .. } | Opcode::Xor { .. }
        | Opcode::Neg { .. } | Opcode::Not { .. }
        | Opcode::ToDyn { .. } | Opcode::ToSFloat { .. }
        | Opcode::ToUFloat { .. } | Opcode::ToInt { .. }
        | Opcode::UnsafeCast { .. }
        | Opcode::Type { .. }
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::opcodes::*;

    #[test]
    fn test_reads_mov() {
        let op = Opcode::Mov { dst: Reg(1), src: Reg(0) };
        assert_eq!(reads(&op), vec![Reg(0)]);
    }

    #[test]
    fn test_writes_add() {
        let op = Opcode::Add { dst: Reg(2), a: Reg(0), b: Reg(1) };
        assert_eq!(writes(&op), vec![Reg(2)]);
        assert_eq!(reads(&op), vec![Reg(0), Reg(1)]);
    }

    #[test]
    fn test_successors_jalways() {
        let op = Opcode::JAlways { offset: -3 };
        assert_eq!(successors(&op, 5, 10), vec![3]);
    }

    #[test]
    fn test_successors_conditional() {
        let op = Opcode::JSLt { a: Reg(0), b: Reg(1), offset: 5 };
        let succ = successors(&op, 3, 20);
        assert_eq!(succ, vec![4, 9]);
    }

    #[test]
    fn test_successors_ret() {
        let op = Opcode::Ret { ret: Reg(0) };
        assert!(successors(&op, 5, 10).is_empty());
    }

    #[test]
    fn test_is_pure() {
        assert!(is_pure(&Opcode::Add { dst: Reg(0), a: Reg(1), b: Reg(2) }));
        assert!(is_pure(&Opcode::Mov { dst: Reg(0), src: Reg(1) }));
        assert!(!is_pure(&Opcode::Call0 { dst: Reg(0), fun: RefFun(0) }));
        assert!(!is_pure(&Opcode::NullCheck { reg: Reg(0) }));
        assert!(!is_pure(&Opcode::SetField { obj: Reg(0), field: RefField(0), src: Reg(1) }));
    }

    #[test]
    fn test_incr_reads_and_writes_same_reg() {
        let op = Opcode::Incr { dst: Reg(3) };
        assert_eq!(reads(&op), vec![Reg(3)]);
        assert_eq!(writes(&op), vec![Reg(3)]);
    }
}
