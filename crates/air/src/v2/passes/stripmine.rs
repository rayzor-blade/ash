//! Strip-mining: one register test per iteration, one poll per strip.
//!
//! Every loop header carries a poll -- the collector's rendezvous and the
//! fiber's yield point -- and in an innermost loop that is two loads, a
//! compare and a branch on every iteration. A loop that advances an
//! integer by a constant each time round already carries a strip count:
//! the low bits of that value reach zero at least once every
//! `STRIP / gcd(step, STRIP)` iterations, so a backend can test them and
//! skip the poll on every other iteration. Safepoint latency stays bounded
//! by one strip.
//!
//! This pass only finds the values; each backend keeps its poll where it
//! was and puts the test in front of it, reading the header phi when the
//! poll precedes the header and the stepped value when it sits on the back
//! edge. Nothing in the IR changes, so the interpreter, the OSR transfer
//! and the register allocation of the loop are what they were. Every
//! rewrite that did change the IR -- a strip loop around the inner one, or
//! a poll block on the latch path -- moved the loop-carried values through
//! a block with two param-carrying successors and the backends' allocators
//! spilled them on every iteration, which cost more on a call benchmark
//! than the poll it removed.
//!
//! Innermost loops with one latch and an induction variable; a loop
//! stepping by a variable amount is not bounded this way and keeps its poll
//! on every iteration.

use super::{Pass, PassOptions, PassStats};
use crate::v2::analysis::{CfgInfo, LoopForest};
use crate::v2::ir::*;
use anyhow::Result;

/// Iterations between two polls, a power of two.
pub const STRIP: i32 = 256;

/// `ASH_STRIP_MINE=0` leaves every loop polling on every iteration, for
/// measuring what the test is worth; safe to run with.
pub fn enabled() -> bool {
    static CELL: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *CELL.get_or_init(|| !matches!(std::env::var("ASH_STRIP_MINE").as_deref(), Ok("0") | Ok("off")))
}

pub struct StripMine;

impl Pass for StripMine {
    fn name(&self) -> &'static str {
        "stripmine"
    }

    fn run(&self, f: &mut Function, _opts: &PassOptions) -> Result<PassStats> {
        let mut stats = PassStats::default();
        let cfg = CfgInfo::build(f);
        let forest = LoopForest::analyze(f, &cfg);
        let mut constants = std::collections::HashSet::new();
        let mut defs: std::collections::HashMap<ValueId, &Instr> = std::collections::HashMap::new();
        for blk in &f.blocks {
            for ins in &blk.instrs {
                if let Instr::Int { dst, .. } = ins {
                    constants.insert(*dst);
                }
                if let Some(d) = ins.dst() {
                    defs.insert(d, ins);
                }
            }
        }
        let mut found = Vec::new();
        for l in forest.innermost_first() {
            let lp = forest.get(l);
            if !lp.children.is_empty() || lp.latches.len() != 1 {
                continue;
            }
            if f.strip_tests.iter().any(|s| s.header == lp.header) {
                continue;
            }
            let latch = lp.latches[0];
            let Some(test) = induction(f, &defs, &constants, lp.header, latch) else {
                continue;
            };
            found.push(test);
        }
        stats.added += found.len();
        f.strip_tests.extend(found);
        Ok(stats)
    }
}

/// The header phi the loop advances by a constant each iteration, with the
/// value the latch hands it: `phi + k`, `phi - k`, `phi + 1` or `phi - 1`
/// with `k` a constant.
fn induction(
    f: &Function,
    defs: &std::collections::HashMap<ValueId, &Instr>,
    constants: &std::collections::HashSet<ValueId>,
    header: BlockId,
    latch: BlockId,
) -> Option<StripTest> {
    for phi in &f.blocks[header.idx()].phis {
        let Some(&(_, stepped)) = phi.incoming.iter().find(|(p, _)| *p == latch) else {
            continue;
        };
        let Some(def) = defs.get(&stepped) else {
            continue;
        };
        let steps = match def {
            Instr::UnOp {
                op: UnOp::Incr | UnOp::Decr,
                src,
                ..
            } => *src == phi.dst,
            Instr::BinOp {
                op: BinOp::Add | BinOp::Sub,
                a,
                b,
                ..
            } => *a == phi.dst && constants.contains(b),
            _ => false,
        };
        if steps && f.value_lanes(phi.dst) == 1 && f.value_ty(phi.dst) == f.value_ty(stepped) {
            return Some(StripTest {
                header,
                phi: phi.dst,
                stepped,
            });
        }
    }
    None
}
