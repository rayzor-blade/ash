//! Which exception handler is active at each opcode.
//!
//! Every engine needs this answer, and today none of them has it statically.
//! The interpreter rediscovers it at run time with a per-frame stack, and the
//! JIT sidesteps it by making each `Trap` a `setjmp` so the machine remembers
//! for it. That `setjmp` is what forces trap-bearing functions out of the LLVM
//! middle-end (a promoted value does not survive the register restore a
//! `longjmp` performs), what keeps the Cranelift tier from lowering traps at
//! all (it has no `returns_twice`), and what makes the inliner refuse
//! trap-bearing callees.
//!
//! Knowing the handler statically is what replaces all of that with an ordinary
//! branch: a throw becomes a jump to a known block, and a call that may throw
//! becomes a test and a branch to that same block.
//!
//! # Well-nestedness, and why it is checked rather than assumed
//!
//! Haxe usually emits `try`/`catch` lexically, so `Trap` and `EndTrap` pair up
//! in program order and a linear scan resolves the active handler. That is an
//! assumption about a code generator rather than a property of the format, and
//! `ASH_VERIFY_TRAPS` over the corpus shows where it breaks: **5 of the Heaps
//! sample's 5094 functions**, none in the test corpus. Every one is a region
//! with more than one normal exit, which no program-order scan can resolve
//! because liveness of the region depends on the path taken.
//!
//! So this is the cheap answer, correct for the 99.9% it can decide and
//! explicit about the rest. The general answer is dataflow over the CFG, which
//! AIR v2 already computes as `Block::handler` — one more reason for the
//! backends to lower from AIR rather than from the opcode array.

use crate::opcodes::Opcode;

/// The handler covering one opcode.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Handler {
    /// Opcode index the handler body starts at.
    pub target: usize,
    /// Register the caught exception is delivered in.
    pub exc_reg: u32,
    /// Index of the `Trap` that opened the region, for diagnostics.
    pub trap_at: usize,
}

/// Why a function's trap structure could not be resolved by scanning.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum TrapShape {
    /// Handler per opcode; `None` where no region is active.
    Nested(Vec<Option<Handler>>),
    /// An `EndTrap` reached with no region open by the scan.
    ///
    /// Not malformed bytecode: it is one region with several normal exits —
    /// `try { if (c) return a; ... return b; }` emits an `EndTrap` per exit
    /// path, and only the path actually taken has the region open. Whether a
    /// region is live at that point is a property of control flow, so no
    /// program-order scan can decide it. AIR v2 answers it by dataflow over
    /// the CFG (`Block::handler`), which is the supplier a lowering should use.
    MultipleExits(usize),
    /// A region still open at the end of the function.
    UnclosedTrap(usize),
}

/// Resolve the active handler at every opcode.
///
/// Regions are walked in program order: `Trap` opens one and `EndTrap` closes
/// it, with the innermost open region winning. `EndTrap` marks the *normal*
/// exit from the protected body, and Haxe lays the handler body out after it,
/// so a handler is naturally outside its own region — an exception raised
/// inside a `catch` belongs to the next handler out, and gets it.
pub fn analyze(ops: &[Opcode]) -> TrapShape {
    let mut active: Vec<Handler> = Vec::new();
    let mut out: Vec<Option<Handler>> = Vec::with_capacity(ops.len());

    for (i, op) in ops.iter().enumerate() {
        match op {
            Opcode::Trap { exc, offset } => {
                let target = (i as i64 + 1 + *offset as i64) as usize;
                out.push(active.last().copied());
                active.push(Handler {
                    target,
                    exc_reg: exc.0,
                    trap_at: i,
                });
                continue;
            }
            Opcode::EndTrap { .. } => {
                if active.pop().is_none() {
                    return TrapShape::MultipleExits(i);
                }
                out.push(active.last().copied());
                continue;
            }
            _ => {}
        }
        out.push(active.last().copied());
    }

    if let Some(h) = active.first() {
        return TrapShape::UnclosedTrap(h.trap_at);
    }
    TrapShape::Nested(out)
}

/// Opcodes that can raise, and therefore need a check when the caller lowers
/// them without a `setjmp` to catch for them.
///
/// Deliberately generous: anything that calls into the runtime or allocates can
/// raise, and being wrong in the other direction means a missed handler.
pub fn may_throw(op: &Opcode) -> bool {
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
            | Opcode::Throw { .. }
            | Opcode::Rethrow { .. }
            | Opcode::New { .. }
            | Opcode::NullCheck { .. }
            | Opcode::GetArray { .. }
            | Opcode::SetArray { .. }
            | Opcode::Field { .. }
            | Opcode::SetField { .. }
            | Opcode::GetThis { .. }
            | Opcode::SetThis { .. }
            | Opcode::DynGet { .. }
            | Opcode::DynSet { .. }
            | Opcode::SafeCast { .. }
            | Opcode::ToVirtual { .. }
            | Opcode::SDiv { .. }
            | Opcode::UDiv { .. }
            | Opcode::SMod { .. }
            | Opcode::UMod { .. }
    )
}

/// How many opcodes in `ops` would need an exception check, and how many are
/// covered by a handler. The ratio is what decides whether an explicit-edge
/// lowering is affordable.
pub fn check_sites(ops: &[Opcode]) -> (usize, usize) {
    let shape = analyze(ops);
    let TrapShape::Nested(handlers) = shape else {
        return (0, 0);
    };
    let mut checks = 0;
    let mut covered = 0;
    for (i, op) in ops.iter().enumerate() {
        if may_throw(op) {
            checks += 1;
            if handlers[i].is_some() {
                covered += 1;
            }
        }
    }
    (checks, covered)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::opcodes::Reg;

    fn ret() -> Opcode {
        Opcode::Ret { ret: Reg(0) }
    }

    #[test]
    fn a_function_without_traps_has_no_handlers() {
        let ops = vec![ret()];
        let TrapShape::Nested(h) = analyze(&ops) else {
            panic!("expected nested")
        };
        assert_eq!(h, vec![None]);
    }

    /// The shape Haxe emits: Trap, protected body, EndTrap, a jump over the
    /// handler, then the handler body.
    #[test]
    fn a_region_covers_its_body_but_not_its_handler() {
        let ops = vec![
            // target = 0 + 1 + 3 = 4, the handler body
            Opcode::Trap {
                exc: Reg(1),
                offset: 3,
            },
            ret(),                           // 1: protected
            Opcode::EndTrap { exc: Reg(0) }, // 2: normal exit
            Opcode::JAlways { offset: 1 },   // 3: skip the handler
            ret(),                           // 4: handler body
            ret(),                           // 5: after
        ];
        let TrapShape::Nested(h) = analyze(&ops) else {
            panic!("expected nested")
        };
        assert_eq!(h[0], None, "the Trap itself is outside its own region");
        assert!(h[1].is_some(), "the protected body is covered");
        assert_eq!(h[1].unwrap().target, 4);
        assert_eq!(h[1].unwrap().exc_reg, 1);
        assert_eq!(h[4], None, "the handler body is not covered by itself");
    }

    /// The innermost region wins, and the outer one resumes once the inner
    /// closes — the nesting that broke when EndTrap's operand was misread.
    #[test]
    fn nested_regions_resolve_innermost_first() {
        let ops = vec![
            Opcode::Trap {
                exc: Reg(1),
                offset: 6,
            }, // 0: outer, handler at 7
            Opcode::Trap {
                exc: Reg(2),
                offset: 3,
            }, // 1: inner, handler at 5
            ret(),                           // 2: inner protected
            Opcode::EndTrap { exc: Reg(0) }, // 3: close inner
            ret(),                           // 4: outer protected only
            ret(),                           // 5: inner handler
            Opcode::EndTrap { exc: Reg(0) }, // 6: close outer
            ret(),                           // 7: outer handler
        ];
        let TrapShape::Nested(h) = analyze(&ops) else {
            panic!("expected nested")
        };
        assert_eq!(h[2].unwrap().exc_reg, 2, "inner region covers its body");
        assert_eq!(h[4].unwrap().exc_reg, 1, "outer resumes after inner closes");
        assert_eq!(h[7], None, "both closed before the outer handler");
    }

    /// Real shape from the Heaps sample (findex 1039): one Trap, two EndTraps,
    /// because the protected body has two normal exits. A scan cannot resolve
    /// it and must say so rather than guess.
    #[test]
    fn a_region_with_two_normal_exits_is_reported_not_guessed() {
        let ops = vec![
            Opcode::Trap {
                exc: Reg(1),
                offset: 4,
            },
            Opcode::EndTrap { exc: Reg(0) },
            ret(),
            Opcode::EndTrap { exc: Reg(0) },
            ret(),
        ];
        assert_eq!(analyze(&ops), TrapShape::MultipleExits(3));
    }

    #[test]
    fn unbalanced_structure_is_reported_not_guessed() {
        let ops = vec![Opcode::EndTrap { exc: Reg(0) }, ret()];
        assert_eq!(analyze(&ops), TrapShape::MultipleExits(0));

        let ops = vec![
            Opcode::Trap {
                exc: Reg(1),
                offset: 1,
            },
            ret(),
        ];
        assert!(matches!(analyze(&ops), TrapShape::UnclosedTrap(0)));
    }
}
