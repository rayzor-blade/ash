//! # On-stack replacement eligibility
//!
//! Decides which loops can be entered by compiled code *while an interpreter
//! frame for the function is already running*, and at which point.
//!
//! Promotion is driven by call counts, so a hot loop inside a function called
//! once can never be promoted. nbody is the shape: its 10M iterations live in
//! `main`, invoked exactly once. Counting back-edges fixes that, but only for
//! loops a transfer can safely enter — which is what this decides.
//!
//! ## Derived from AIR, not from the opcode array
//!
//! An earlier version of this module walked the opcode array and built its own
//! CFG, dominator tree and reachability. All of that already exists in
//! [`air::v2::analysis`], better: `LoopForest` finds natural loops with their
//! headers and latches, and `CfgInfo` answers dominance. More importantly the
//! IR answers two questions the opcode array cannot answer directly:
//!
//! * **Which registers escape by address.** AIR pins those to *cells*
//!   ([`PinReason::RefTaken`]), so "does a reference to this frame outlive the
//!   transfer" is a property of the IR rather than a reachability search.
//! * **Where trap regions are live.** `Block::handler` is a dataflow result, so
//!   a region with several normal exits resolves correctly, where a
//!   program-order scan cannot decide it at all.

use air::v2::analysis::{CfgInfo, LoopForest};
use air::v2::ir::{BlockId, Function, Instr, PinReason, Terminator};

/// The largest live-in set a transfer will marshal.
///
/// The buffer is built in the transferring frame so the conservative collector
/// scans it; a heap allocation would hide those roots. zyntax caps its
/// equivalent at 128 and reports nbody's headers needing 3 to 99, so this is
/// sized from a working implementation rather than guessed.
pub const MAX_OSR_LIVE_INS: usize = 128;

/// Why a loop cannot be entered mid-flight.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum OsrRefusal {
    /// The function can catch. HL exceptions are setjmp/longjmp, so a frame
    /// that transfers while a region is live leaves an armed `jmp_buf` for a
    /// frame that no longer exists, and nothing sound owns the matching
    /// `EndTrap`. Lifting this is what the explicit-edge exception work is for.
    Trap,
    /// A register's address escapes; AIR models those as cells. Once the frame
    /// moves, an outstanding pointer to it reads stale values.
    RefTaken,
    /// The function has no loop to enter.
    NoBackEdge,
    /// More than one latch, so several paths reach the header carrying
    /// different state. v1 takes only the single-entry case.
    MultipleLatches(usize),
    /// More live-ins than [`MAX_OSR_LIVE_INS`].
    TooManyLiveIns(usize),
}

/// What [`analyze`] concluded about one function.
#[derive(Clone, Debug)]
pub struct OsrPlan {
    /// Empty means eligible.
    pub refusals: Vec<OsrRefusal>,
    /// Header block of each loop that may be entered.
    pub entry_headers: Vec<u32>,
}

impl OsrPlan {
    pub fn eligible(&self) -> bool {
        self.refusals.is_empty() && !self.entry_headers.is_empty()
    }
}

/// Blocks reachable from any of `seeds`, following CFG successors.
///
/// Seeds count as reachable: a block can reach itself around a loop, and for
/// the address-escape question the block holding the `CellRef` is already a
/// place the pointer is live.
fn reachable_from(cfg: &CfgInfo, seeds: &[BlockId]) -> Vec<bool> {
    let mut seen = vec![false; cfg.succs.len()];
    let mut work: Vec<usize> = Vec::new();
    for s in seeds {
        if s.idx() < seen.len() && !seen[s.idx()] {
            seen[s.idx()] = true;
            work.push(s.idx());
        }
    }
    while let Some(b) = work.pop() {
        for succ in &cfg.succs[b] {
            if succ.idx() < seen.len() && !seen[succ.idx()] {
                seen[succ.idx()] = true;
                work.push(succ.idx());
            }
        }
    }
    seen
}

/// Static OSR analysis for one AIR function.
///
/// Collects every refusal rather than stopping at the first, so a report can
/// say why a function was rejected instead of only that it was.
pub fn analyze(f: &Function) -> OsrPlan {
    let mut refusals = Vec::new();

    if f.blocks
        .iter()
        .any(|b| b.handler.is_some() || matches!(b.term, Terminator::Trap { .. }))
    {
        refusals.push(OsrRefusal::Trap);
    }

    let cfg = CfgInfo::build(f);
    let forest = LoopForest::analyze(f, &cfg);
    // A loopless function stopped here. That was scope, not capability:
    // compile_osr_entry asks only that a site be a block start, and the hazards
    // guarded below -- an escaping frame pointer, a live catch region, several
    // paths carrying different state -- are properties of a BLOCK, not a loop.
    // Recursion is the shape with no loop header and the shape the interpreter
    // is worst at, so it is the one that most wants a way out.
    let loopless = forest.is_empty();

    // Where an address of the frame is taken. Entering compiled code freezes
    // the interpreter's slots, so a pointer produced *before* arrival at a
    // header would then read stale values — but one taken only in code the
    // header cannot reach is no hazard to entering there. Refusing per function
    // instead costs about a fifth of all loop-bearing functions, nbody's `main`
    // among them, whose single `Ref` sits past the end of every loop it would
    // disqualify.
    let ref_seeds: Vec<BlockId> = f
        .blocks
        .iter()
        .enumerate()
        .filter(|(_, b)| {
            b.instrs.iter().any(|i| match i {
                Instr::CellRef { cell, .. } => f
                    .cells
                    .get(cell.idx())
                    .is_some_and(|c| c.reason == PinReason::RefTaken),
                _ => false,
            })
        })
        .map(|(i, _)| BlockId(i as u32))
        .collect();
    let ref_reaches = reachable_from(&cfg, &ref_seeds);

    let mut entry_headers = Vec::new();
    if loopless {
        // Block 0 is the ordinary entry; calling the function is already the
        // way in. Single-predecessor is the same "one path carries state in"
        // property MultipleLatches demands of a loop header.
        for (i, _) in f.blocks.iter().enumerate().skip(1) {
            if cfg.preds[i].len() != 1 {
                continue;
            }
            if ref_reaches[i] {
                if !refusals.contains(&OsrRefusal::RefTaken) {
                    refusals.push(OsrRefusal::RefTaken);
                }
                continue;
            }
            entry_headers.push(BlockId(i as u32).0);
        }
        if entry_headers.is_empty() && !refusals.contains(&OsrRefusal::NoBackEdge) {
            refusals.push(OsrRefusal::NoBackEdge);
        }
        return OsrPlan { refusals, entry_headers };
    }
    for l in forest.innermost_first() {
        let lp = forest.get(l);
        if lp.latches.len() != 1 {
            refusals.push(OsrRefusal::MultipleLatches(lp.header.0 as usize));
            continue;
        }
        if ref_reaches[lp.header.idx()] {
            if !refusals.contains(&OsrRefusal::RefTaken) {
                refusals.push(OsrRefusal::RefTaken);
            }
            continue;
        }
        entry_headers.push(lp.header.0);
    }
    if entry_headers.is_empty() && !refusals.iter().any(|r| matches!(r, OsrRefusal::NoBackEdge)) {
        refusals.push(OsrRefusal::NoBackEdge);
    }

    OsrPlan {
        refusals,
        entry_headers,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use air::opcodes::{Opcode, RefInt, Reg};
    use air::v2::lower::lower;
    use air::v2::TypeRef;

    fn tys(n: usize) -> Vec<TypeRef> {
        vec![TypeRef(0); n]
    }

    fn counted_loop() -> Vec<Opcode> {
        vec![
            Opcode::Int {
                dst: Reg(0),
                ptr: RefInt(0),
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
    fn a_counted_loop_is_eligible() {
        let f = lower(&counted_loop(), &tys(1)).unwrap();
        let p = analyze(&f);
        assert!(p.eligible(), "refusals: {:?}", p.refusals);
        assert_eq!(p.entry_headers.len(), 1);
    }

    /// A loopless function is NOT refused any more, and this test asserted
    /// that it was. `analyze` gained the loopless arm above -- entry is a
    /// block-start property, and recursion, the shape with no loop header, is
    /// what the interpreter is worst at -- and this case was left behind
    /// asserting the scope the arm removed.
    #[test]
    fn a_function_without_a_loop_offers_its_straight_line_blocks() {
        let f = lower(&[Opcode::Ret { ret: Reg(0) }], &tys(1)).unwrap();
        let p = analyze(&f);
        assert!(p.eligible(), "refusals: {:?}", p.refusals);
        assert!(
            !p.refusals.contains(&OsrRefusal::NoBackEdge),
            "refusals: {:?}",
            p.refusals
        );
        // Block 0 is the ordinary entry; the way in is to call the function.
        assert!(!p.entry_headers.contains(&0), "{:?}", p.entry_headers);
    }

    /// A reference taken before the loop can still be live inside it.
    #[test]
    fn a_ref_taken_before_the_loop_refuses() {
        let mut ops = counted_loop();
        ops.insert(
            0,
            Opcode::Ref {
                dst: Reg(1),
                src: Reg(0),
            },
        );
        let f = lower(&ops, &tys(2)).unwrap();
        assert!(analyze(&f).refusals.contains(&OsrRefusal::RefTaken));
    }

    /// One taken only after every loop cannot dangle across a transfer into
    /// one. This is nbody's `main`: twenty hot loops and a single `Ref` past
    /// the end of all of them.
    #[test]
    fn a_ref_taken_after_every_loop_does_not_refuse() {
        let mut ops = counted_loop();
        ops.insert(
            3,
            Opcode::Ref {
                dst: Reg(1),
                src: Reg(0),
            },
        );
        let f = lower(&ops, &tys(2)).unwrap();
        let p = analyze(&f);
        assert!(
            !p.refusals.contains(&OsrRefusal::RefTaken),
            "{:?}",
            p.refusals
        );
        assert!(p.eligible());
    }

    /// Incr pins nothing now, so a plain counter must not be mistaken for an
    /// escaping one — the regression this guards is exactly the over-broad
    /// refusal the opcode-array version had.
    #[test]
    fn a_loop_counter_alone_does_not_refuse() {
        let f = lower(&counted_loop(), &tys(1)).unwrap();
        assert!(!analyze(&f).refusals.contains(&OsrRefusal::RefTaken));
    }
}
