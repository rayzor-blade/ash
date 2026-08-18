//! # On-stack replacement eligibility
//!
//! Decides, statically, whether a function's hot loop can be entered by
//! compiled code *while an interpreter frame for it is already running*, and
//! at which opcode.
//!
//! ## Why this exists
//!
//! Promotion is driven by call counts, so a hot loop inside a function that is
//! called once can never be promoted. nbody is the canonical shape: its 10M
//! iterations live in `main`, which is invoked exactly once, so the loop is
//! interpreted forever while the compiled `advance` is re-entered across the
//! interpreter boundary on every iteration. Counting *back-edges* instead of
//! calls fixes that, but only if the running frame can be handed to compiled
//! code mid-flight — which is what OSR is.
//!
//! ## Refusing is the default
//!
//! A transfer moves live state from one execution engine to another at a point
//! neither was designed around, so this module's job is mostly to say no. Every
//! refusal is detected here, at compile time, from the bytecode alone — never
//! discovered during the transfer. [`analyze`] returns every reason it found
//! rather than the first, because the list is a diagnostic: `ASH_VERIFY_OSR`
//! prints it per function, and "which loops would OSR accept?" should be a
//! measurement, not a guess.
//!
//! The refusals and their reasons:
//!
//! * **[`OsrRefusal::Trap`]** — HL exceptions are setjmp/longjmp. A frame that
//!   transfers while a trap region is active leaves an interpreter `jmp_buf`
//!   armed for a frame that no longer exists, and nothing sound owns the
//!   matching `EndTrap`. Refused for the whole function, not just for loops
//!   inside the region: a back-edge outside the region can still be reached
//!   with one active.
//! * **[`OsrRefusal::RefTaken`]** — `Ref`/`Setref` take the address of an
//!   interpreter register. Those addresses would dangle the moment the frame's
//!   values move into compiled code's own slots.
//! * **[`OsrRefusal::IrreducibleTarget`]** — the back-edge target must be a
//!   block leader that dominates the jump. A target that does not is either an
//!   irreducible loop or a jump into the middle of a block, and compiled code
//!   has no block boundary there to enter at.
//! * **[`OsrRefusal::UnrepresentableRegType`]** — every live register has to
//!   survive the round trip through a flat `i64` buffer. Kinds whose layout the
//!   [`crate::layout`] oracle itself declines are refused here rather than
//!   guessed at, so there is one answer to "what is this type's shape", not
//!   two.
//!
//! `Prefetch`/`Asm` are refused because they are x86-only and unlowered, and
//! `IndirectCall` because it is a hot-reload rewrite artifact rather than
//! something a bytecode file contains.

use std::collections::HashMap;

use air::cfg::CFG;
use air::dominance::DominatorTree;
use air::opcode_info;

use crate::bytecode::DecodedBytecode;
use crate::hl::{hl_type_kind_HOBJ, hl_type_kind_HPACKED, hl_type_kind_HSTRUCT};
use crate::opcodes::Opcode;
use crate::types::HLFunction;

/// The largest register file a transfer will marshal.
///
/// The buffer is built in the transferring Rust frame so the conservative
/// collector scans it; a heap allocation would hide those roots. That means the
/// cap is a real limit rather than a tuning knob, and a function above it is
/// refused rather than promoted to a heap buffer.
pub const MAX_OSR_REGS: usize = 64;

/// Why a function, or one of its back-edges, cannot be entered by OSR.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum OsrRefusal {
    /// Contains `Trap`/`EndTrap`; setjmp state cannot be transferred.
    Trap,
    /// A register's address escapes via `Ref`/`Setref`.
    RefTaken,
    /// x86-only opcodes that no tier lowers.
    PrefetchAsm,
    /// A hot-reload pass artifact, not real bytecode.
    IndirectCall,
    /// No backward jump, so there is no loop to enter.
    NoBackEdge,
    /// A register kind that cannot round-trip through the transfer buffer.
    UnrepresentableRegType(u32),
    /// Back-edge target is not a dominating block leader.
    IrreducibleTarget(usize),
    /// Register file exceeds [`MAX_OSR_REGS`].
    TooManyRegs(usize),
}

/// What [`analyze`] concluded about one function.
#[derive(Clone, Debug)]
pub struct OsrPlan {
    /// Empty means the function is eligible.
    pub refusals: Vec<OsrRefusal>,
    /// `(jump opcode index, target opcode index)` for each accepted back-edge.
    pub back_edges: Vec<(usize, usize)>,
    /// `hl_type_kind` per register, in register order.
    pub reg_kinds: Vec<u32>,
    pub nregs: usize,
}

impl OsrPlan {
    /// Whether any back-edge in this function may transfer.
    pub fn eligible(&self) -> bool {
        self.refusals.is_empty() && !self.back_edges.is_empty()
    }

    /// Whether `pc` is an accepted back-edge target.
    ///
    /// The probe consults this before doing anything expensive, so a function
    /// that was refused costs one lookup per back-edge and nothing more.
    pub fn eligible_target(&self, pc: usize) -> bool {
        self.eligible() && self.back_edges.iter().any(|&(_, t)| t == pc)
    }
}

/// Whether a register of this kind survives the round trip through the flat
/// `i64` transfer buffer.
///
/// `HPACKED` never does — its value is inline aggregate data, not a word. An
/// `HOBJ`/`HSTRUCT` register is a pointer and is fine regardless of what the
/// layout oracle says about its *fields*; the check that matters for those is
/// on the field access itself, which is already compiled.
fn representable(kind: u32) -> bool {
    kind != hl_type_kind_HPACKED
}

/// Which blocks are reachable from any of `seeds`, following CFG successors.
///
/// `seeds` themselves count as reachable: an instruction can reach its own
/// block again through a loop, and for the `Ref` question "the block containing
/// it" is precisely a place the reference is already live.
fn reachable_from(cfg: &CFG, seeds: &[usize]) -> Vec<bool> {
    let mut seen = vec![false; cfg.blocks.len()];
    let mut work: Vec<usize> = Vec::new();
    for &s in seeds {
        if s < seen.len() && !seen[s] {
            seen[s] = true;
            work.push(s);
        }
    }
    while let Some(b) = work.pop() {
        let Some(block) = cfg.blocks.get(b) else {
            continue;
        };
        for &succ in &block.successors {
            if succ < seen.len() && !seen[succ] {
                seen[succ] = true;
                work.push(succ);
            }
        }
    }
    seen
}

/// Static OSR analysis for one function.
///
/// Collects every refusal rather than stopping at the first, so the verifier
/// can report why a function was rejected instead of only that it was.
pub fn analyze(bc: &DecodedBytecode, f: &HLFunction) -> OsrPlan {
    let mut refusals = Vec::new();
    let nregs = f.regs.len();

    let reg_kinds: Vec<u32> = f
        .regs
        .iter()
        .map(|r| bc.types.get(r.0).map(|t| t.kind).unwrap_or(0))
        .collect();

    if nregs > MAX_OSR_REGS {
        refusals.push(OsrRefusal::TooManyRegs(nregs));
    }
    for &k in &reg_kinds {
        if !representable(k) {
            refusals.push(OsrRefusal::UnrepresentableRegType(k));
            break;
        }
    }

    for op in &f.ops {
        match op {
            Opcode::Trap { .. } | Opcode::EndTrap { .. } => {
                if !refusals.contains(&OsrRefusal::Trap) {
                    refusals.push(OsrRefusal::Trap);
                }
            }
            // Ref/Setref are handled per-target below, not per-function: a
            // reference taken in code that cannot run before a given loop
            // header is no hazard to entering at that header.
            Opcode::Prefetch { .. } | Opcode::Asm { .. } => {
                if !refusals.contains(&OsrRefusal::PrefetchAsm) {
                    refusals.push(OsrRefusal::PrefetchAsm);
                }
            }
            Opcode::IndirectCall { .. } => {
                if !refusals.contains(&OsrRefusal::IndirectCall) {
                    refusals.push(OsrRefusal::IndirectCall);
                }
            }
            _ => {}
        }
    }

    // Collect backward jumps. `Trap` also carries an offset, but a function
    // containing one is already refused above.
    let mut raw_back_edges: Vec<(usize, usize)> = Vec::new();
    for (i, op) in f.ops.iter().enumerate() {
        if matches!(op, Opcode::Trap { .. }) {
            continue;
        }
        if let Some(off) = opcode_info::jump_offset(op) {
            if off < 0 {
                let target = i as i64 + 1 + off as i64;
                if target >= 0 && (target as usize) < f.ops.len() {
                    raw_back_edges.push((i, target as usize));
                }
            }
        }
        // Switch offsets are forward in HL output, but do not assume it.
        if let Opcode::Switch { offsets, .. } = op {
            for &off in offsets {
                if off < 0 {
                    let target = i as i64 + 1 + off as i64;
                    if target >= 0 && (target as usize) < f.ops.len() {
                        raw_back_edges.push((i, target as usize));
                    }
                }
            }
        }
    }

    if raw_back_edges.is_empty() {
        refusals.push(OsrRefusal::NoBackEdge);
        return OsrPlan {
            refusals,
            back_edges: Vec::new(),
            reg_kinds,
            nregs,
        };
    }

    // A target is usable only if compiled code has a block boundary there and
    // the edge is a genuine loop back-edge (target dominates the jump).
    let cfg = CFG::build(&f.ops);
    let dom = DominatorTree::build(&cfg);

    // Blocks from which a taken register address could still be outstanding.
    //
    // Entering compiled code freezes the interpreter's register file: it stops
    // being written, but a pointer produced earlier by `Ref` still aims at it,
    // so anything reading through that pointer afterwards sees a stale value.
    // The hazard therefore exists only if a `Ref`/`Setref` can execute *before*
    // arrival at the header — which is a reachability question, not a
    // whole-function one. Refusing per function instead costs about a third of
    // all loop-bearing functions, including nbody's `main`, whose single `Ref`
    // sits past the end of every loop it would disqualify.
    let ref_blocks: Vec<usize> = f
        .ops
        .iter()
        .enumerate()
        .filter(|(_, op)| matches!(op, Opcode::Ref { .. } | Opcode::Setref { .. }))
        .filter_map(|(i, _)| cfg.block_of.get(i).copied())
        .collect();
    let ref_reaches = reachable_from(&cfg, &ref_blocks);

    let mut targets_seen: HashMap<usize, usize> = HashMap::new();
    let mut back_edges = Vec::new();

    for &(from, target) in &raw_back_edges {
        let (Some(&tb), Some(&fb)) = (cfg.block_of.get(target), cfg.block_of.get(from)) else {
            refusals.push(OsrRefusal::IrreducibleTarget(target));
            continue;
        };
        let is_leader = cfg
            .blocks
            .get(tb)
            .map(|b| b.start == target)
            .unwrap_or(false);
        if !is_leader || !dom.dominates(tb, fb) {
            refusals.push(OsrRefusal::IrreducibleTarget(target));
            continue;
        }
        if ref_reaches.get(tb).copied().unwrap_or(true) {
            if !refusals.contains(&OsrRefusal::RefTaken) {
                refusals.push(OsrRefusal::RefTaken);
            }
            continue;
        }
        *targets_seen.entry(target).or_insert(0) += 1;
        back_edges.push((from, target));
    }

    // More than one back-edge into the same header means several paths reach it
    // with different live state. v1 takes only the single-entry case.
    back_edges.retain(|&(_, t)| targets_seen.get(&t) == Some(&1));
    if back_edges.is_empty() && !refusals.iter().any(|r| matches!(r, OsrRefusal::NoBackEdge)) {
        refusals.push(OsrRefusal::NoBackEdge);
    }

    OsrPlan {
        refusals,
        back_edges,
        reg_kinds,
        nregs,
    }
}

/// One line per function, for `ASH_VERIFY_OSR=only`.
///
/// Deliberately a measurement rather than a claim: it answers "which loops
/// would OSR accept, across a whole program" in the time it takes to decode,
/// the same way `ASH_VERIFY_LAYOUT=only` does for field offsets.
pub fn report(bc: &DecodedBytecode) -> Vec<String> {
    let mut out = Vec::new();
    let mut eligible = 0usize;
    for f in &bc.functions {
        let plan = analyze(bc, f);
        // The entrypoint is the function most likely to hold a long-running
        // loop and least likely to be promoted by call count, so it is worth
        // spotting at a glance — it is the whole reason OSR exists.
        let mark = if f.findex as usize == bc.entrypoint as usize {
            " <ENTRYPOINT>"
        } else {
            ""
        };
        if plan.eligible() {
            eligible += 1;
            out.push(format!(
                "findex={:<6} {:<28} nregs={:<4} back_edges={:?}  ELIGIBLE{}",
                f.findex,
                f.name(),
                plan.nregs,
                plan.back_edges,
                mark
            ));
        } else if plan
            .refusals
            .iter()
            .any(|r| !matches!(r, OsrRefusal::NoBackEdge))
        {
            // Functions with no loop at all are the common case and say
            // nothing; report only a function that had a reason beyond that.
            // Back-edges are printed even when refused, because "this loop is
            // hot but we declined it, for X" is the line worth reading.
            out.push(format!(
                "findex={:<6} {:<28} nregs={:<4} back_edges={:?} refused={:?}{}",
                f.findex,
                f.name(),
                plan.nregs,
                plan.back_edges,
                plan.refusals,
                mark
            ));
        }
    }
    out.push(format!(
        "{} of {} functions eligible for OSR",
        eligible,
        bc.functions.len()
    ));
    out
}

/// Kinds that are pointers into the heap, for the transfer buffer's benefit.
pub fn is_heap_kind(kind: u32) -> bool {
    matches!(kind, k if k == hl_type_kind_HOBJ || k == hl_type_kind_HSTRUCT)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::opcodes::Reg;

    fn plan_of(ops: Vec<Opcode>, kinds: Vec<u32>) -> OsrPlan {
        // analyze() only reads bc.types through f.regs, so a minimal stand-in
        // is enough and keeps these tests independent of the decoder.
        let mut bc = DecodedBytecode::default();
        bc.types = kinds
            .iter()
            .map(|&k| crate::types::HLType {
                kind: k,
                ..Default::default()
            })
            .collect();
        let f = HLFunction {
            findex: 0,
            ops,
            regs: (0..kinds.len()).map(crate::types::TypeRef).collect(),
            ..Default::default()
        };
        analyze(&bc, &f)
    }

    /// A simple counted loop: the shape OSR exists for.
    fn simple_loop() -> Vec<Opcode> {
        vec![
            Opcode::Int {
                dst: Reg(0),
                ptr: air::opcodes::RefInt(0),
            },
            Opcode::Incr { dst: Reg(0) },
            Opcode::JSLt {
                a: Reg(0),
                b: Reg(0),
                offset: -2,
            },
            Opcode::Ret { ret: Reg(0) },
        ]
    }

    #[test]
    fn accepts_a_simple_back_edge() {
        let p = plan_of(simple_loop(), vec![crate::hl::hl_type_kind_HI32]);
        assert!(p.eligible(), "refusals: {:?}", p.refusals);
        assert_eq!(p.back_edges, vec![(2, 1)]);
        assert!(p.eligible_target(1));
        assert!(!p.eligible_target(0));
    }

    #[test]
    fn refuses_a_function_with_no_loop() {
        let p = plan_of(
            vec![Opcode::Ret { ret: Reg(0) }],
            vec![crate::hl::hl_type_kind_HI32],
        );
        assert!(!p.eligible());
        assert!(p.refusals.contains(&OsrRefusal::NoBackEdge));
    }

    /// Trap anywhere disqualifies the whole function, not just the region: a
    /// back-edge outside it can still run with a trap armed.
    #[test]
    fn refuses_any_function_containing_a_trap() {
        let mut ops = simple_loop();
        ops.insert(
            0,
            Opcode::Trap {
                exc: Reg(0),
                offset: 1,
            },
        );
        let p = plan_of(ops, vec![crate::hl::hl_type_kind_HI32]);
        assert!(p.refusals.contains(&OsrRefusal::Trap));
        assert!(!p.eligible());
    }

    #[test]
    fn refuses_when_a_register_address_escapes() {
        let mut ops = simple_loop();
        ops.insert(
            0,
            Opcode::Ref {
                dst: Reg(0),
                src: Reg(0),
            },
        );
        let p = plan_of(ops, vec![crate::hl::hl_type_kind_HI32]);
        assert!(p.refusals.contains(&OsrRefusal::RefTaken));
    }

    /// A reference taken *after* every loop cannot dangle across a transfer
    /// into one, so it must not disqualify it. This is nbody's `main`: twenty
    /// unrolled hot loops and a single `Ref` past the end of all of them,
    /// which a whole-function refusal would throw away entirely.
    #[test]
    fn accepts_a_loop_whose_only_ref_comes_after_it() {
        let mut ops = simple_loop();
        // simple_loop is [Int, Incr, JSLt(-2), Ret]; put the Ref before Ret.
        ops.insert(
            3,
            Opcode::Ref {
                dst: Reg(0),
                src: Reg(0),
            },
        );
        let p = plan_of(ops, vec![crate::hl::hl_type_kind_HI32]);
        assert!(
            !p.refusals.contains(&OsrRefusal::RefTaken),
            "refusals: {:?}",
            p.refusals
        );
        assert_eq!(p.back_edges, vec![(2, 1)]);
    }

    #[test]
    fn refuses_an_oversized_register_file() {
        let kinds = vec![crate::hl::hl_type_kind_HI32; MAX_OSR_REGS + 1];
        let p = plan_of(simple_loop(), kinds);
        assert!(matches!(
            p.refusals.first(),
            Some(OsrRefusal::TooManyRegs(_))
        ));
    }

    #[test]
    fn refuses_packed_registers() {
        let p = plan_of(simple_loop(), vec![hl_type_kind_HPACKED]);
        assert!(p
            .refusals
            .iter()
            .any(|r| matches!(r, OsrRefusal::UnrepresentableRegType(_))));
    }

    /// Two back-edges into one header means several paths reach it with
    /// different live state; v1 takes only the single-entry case.
    #[test]
    fn refuses_a_header_with_two_back_edges() {
        let ops = vec![
            Opcode::Int {
                dst: Reg(0),
                ptr: air::opcodes::RefInt(0),
            },
            Opcode::Incr { dst: Reg(0) },
            Opcode::JSLt {
                a: Reg(0),
                b: Reg(0),
                offset: -2,
            },
            Opcode::JSGt {
                a: Reg(0),
                b: Reg(0),
                offset: -3,
            },
            Opcode::Ret { ret: Reg(0) },
        ];
        let p = plan_of(ops, vec![crate::hl::hl_type_kind_HI32]);
        assert!(p.back_edges.is_empty(), "got {:?}", p.back_edges);
    }
}
