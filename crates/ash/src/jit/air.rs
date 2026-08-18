//! Which bytecode optimizer the LLVM tier runs before emission.
//!
//! Two pipelines exist. [`AirMode::V1`] is `air::pass::PassManager`: three
//! passes that rewrite the opcode array in place, never changing its length or
//! the register count. [`AirMode::V2`] is [`crate::air_pipeline`], the typed
//! phi-SSA pipeline every backend is meant to share —
//! `lower -> optimize -> verify -> serialize` — which hands back a *new*
//! opcode array and a *new* register-type table.
//!
//! v2 is off by default, so an unset `ASH_AIR` reproduces today's codegen
//! exactly. `ASH_AIR=v2` turns it on; a function v2 refuses falls back to v1
//! rather than losing optimization, so switching it on can only ever be v1
//! plus whatever v2 additionally manages.
//!
//! # Why v2 needs no `has_trap` skip
//!
//! v1 is skipped for any function containing a `Trap`, because
//! `air::cfg::CFG` does not mention the opcode at all: the handler block gets
//! no incoming edge, and SSA then places phis as if the longjmp path did not
//! exist. v2 models the region instead — `Trap` is a two-successor terminator
//! (`Terminator::Trap { handler, normal }`), the exception register is pinned
//! to a memory cell (`PinReason::TrapExc`), *every* register written anywhere
//! inside an open region is pinned too (`PinReason::TrapWritten`), and
//! `Instr::may_throw` drives what LICM will not move. Pinning the written set
//! is the part that matters: a pinned register is load/store memory, never
//! SSA-renamed, so a handler reading it after a longjmp observes the last
//! store rather than a phi that assumed a normal edge. The skip is therefore
//! not carried over.

use std::collections::HashSet;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::OnceLock;

use air::passes::IndirectCallRewritePass;

use crate::air_pipeline::{self, AirOptLevel, AirPassOptions, AshModule, PipelineError};
use crate::bytecode::DecodedBytecode;
use crate::opcodes::Opcode;
use crate::types::{HLFunction, TypeRef};

/// The bytecode optimizer in force for this process.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum AirMode {
    /// No bytecode optimization at all. Only reachable via `ASH_AIR=none`;
    /// it exists so a miscompile can be bisected against "neither pipeline".
    None,
    /// AIR v1, skipped for trap-containing functions. The default.
    V1,
    /// AIR v2, falling back to v1 per refused function.
    V2,
}

/// Reads `ASH_AIR` once. Unset, `off` and `v1` all mean [`AirMode::V1`] —
/// "off" names the v2 migration being off, not the absence of an optimizer,
/// so the default keeps today's codegen.
pub(crate) fn mode() -> AirMode {
    static MODE: OnceLock<AirMode> = OnceLock::new();
    *MODE.get_or_init(|| {
        let raw = std::env::var("ASH_AIR").unwrap_or_default();
        match raw.to_ascii_lowercase().as_str() {
            "v2" | "2" => AirMode::V2,
            "none" => AirMode::None,
            "" | "off" | "0" | "v1" | "1" => AirMode::V1,
            other => {
                eprintln!("[air] unknown ASH_AIR='{other}' (expected v2|v1|off|none); using v1");
                AirMode::V1
            }
        }
    })
}

/// The v2 opt level, from `ASH_AIR_LEVEL` — the same variable the whole-module
/// sweep in `ash_cli` reads. O2 by default, which is the level v1 runs at, so
/// the two pipelines are compared at the same nominal strength.
fn level() -> AirOptLevel {
    static LEVEL: OnceLock<AirOptLevel> = OnceLock::new();
    *LEVEL.get_or_init(|| match std::env::var("ASH_AIR_LEVEL") {
        Ok(s) if !s.is_empty() => air_pipeline::parse_level(&s).unwrap_or_else(|| {
            eprintln!("[air] invalid ASH_AIR_LEVEL='{s}' (expected O0|O1|O2|O3); using O2");
            AirOptLevel::O2
        }),
        _ => AirOptLevel::O2,
    })
}

/// v2 pass options. Only the FMA peephole is exposed, via `ASH_AIR_FMA=0`,
/// because it is the one pass whose output is observable as a *different
/// number* rather than as different code — see the module docs on fusion.
fn pass_options() -> AirPassOptions {
    static OPTS: OnceLock<AirPassOptions> = OnceLock::new();
    *OPTS.get_or_init(|| {
        let mut o = AirPassOptions::default();
        if matches!(std::env::var("ASH_AIR_FMA").as_deref(), Ok("0") | Ok("off")) {
            o.fma = false;
        }
        o
    })
}

/// Optimize `f` in place, ready for LLVM emission.
///
/// `indirect_call_natives` carries the hot-reload rewrite: `Some(set)` runs
/// [`IndirectCallRewritePass`] with that set of native findexes, `None` skips
/// it. It is threaded in rather than run at the call site because *when* it
/// runs depends on which pipeline ran — see below.
pub(crate) fn optimize(
    bc: &DecodedBytecode,
    f: &mut HLFunction,
    indirect_call_natives: Option<&HashSet<usize>>,
) {
    if mode() == AirMode::V2 {
        // v2 first, hot-reload rewrite second — the reverse of v1's ordering.
        // `air::v2::lower` rejects `IndirectCall` outright ("a v1 rewrite-pass
        // artifact, not lowerable bytecode"), so rewriting first would make v2
        // refuse every function in a hot-reload build. Swapping the two is
        // sound because the rewrite is a 1:1 substitution at a fixed index —
        // `CallN` becomes `IndirectCall` in place, touching no jump offsets and
        // no registers — so it commutes with everything that precedes it. It is
        // also the better order: v2 gets to reason about a real call, with the
        // effects and throw behaviour `Instr::Call` models, instead of an
        // opcode it has no semantics for.
        if run_v2(bc, f, indirect_call_natives.is_some()) {
            rewrite_indirect(f, indirect_call_natives);
            return;
        }
        // Refused. Fall through to today's v1 path, which wants the opposite
        // order; `run_v2` leaves `f` untouched on failure, so this is clean.
    }
    rewrite_indirect(f, indirect_call_natives);
    run_v1(f);
}

fn rewrite_indirect(f: &mut HLFunction, natives: Option<&HashSet<usize>>) {
    if let Some(natives) = natives {
        IndirectCallRewritePass::new(natives.clone()).run(&mut f.ops);
    }
}

/// AIR v1: three in-place passes at O2.
fn run_v1(f: &mut HLFunction) {
    if mode() == AirMode::None {
        return;
    }
    // `air::cfg::CFG` has no notion of `Trap` — the opcode appears nowhere in
    // it — so a trap handler is left with no incoming edge and SSA phis land in
    // the wrong places. Skipping these functions is v1's workaround for that,
    // and it is why v1 leaves every `try` block unoptimized.
    if f.ops.iter().any(|op| matches!(op, Opcode::Trap { .. })) {
        return;
    }
    let num_regs = f.regs.len();
    air::pass::PassManager::new(air::pass::OptLevel::O2).run(&mut f.ops, num_regs);
}

/// AIR v2. Returns whether `f` was rewritten; on `false` it is untouched and
/// the caller should run v1 instead.
fn run_v2(bc: &DecodedBytecode, f: &mut HLFunction, hot_reload: bool) -> bool {
    // Hot reload replaces a function by patching `functions_ptrs[findex]`,
    // which only works while every call to it still goes through that table.
    // The two passes O3 adds break exactly that: the inliner pastes a callee's
    // body into its caller where no patch can reach it, and tail-recursion
    // elimination turns a self-call into a backward jump, so a reload can no
    // longer take effect between iterations. O2 has neither — and is the level
    // v1 ran at anyway.
    let level = match (hot_reload, level()) {
        (true, AirOptLevel::O3) => AirOptLevel::O2,
        (_, l) => l,
    };
    // Withholding callee bodies makes the inliner inert even if the level ever
    // does reach it; belt and braces for the same hot-reload reason.
    let m = if hot_reload {
        AshModule::new(bc).without_callees()
    } else {
        AshModule::new(bc)
    };

    match air_pipeline::optimize_full(&m, f, level, &pass_options()).map(|(s, _, _)| s) {
        Ok(s) => {
            f.ops = s.ops;
            // `serialize` only ever appends registers (de-SSA edge copies and
            // the FMA peephole's temporaries) and never renumbers, so 0..nargs
            // still hold the parameters `load_function_arguments` stores into.
            f.regs = s
                .reg_types
                .into_iter()
                .map(|t| TypeRef(t.0 as usize))
                .collect();
            // `debug` is a per-opcode line table, and the passes moved opcodes;
            // it no longer describes this body at any index. Nothing in ash
            // reads it after decoding, but a table of stale entries is a trap
            // for whoever wires up line info, so drop it rather than leave a
            // plausible-looking lie.
            f.debug.clear();
            true
        }
        Err(e) => {
            report_refusal(&e);
            false
        }
    }
}

/// Refusals should be zero — the whole corpus round-trips — so each one is
/// news. The first few are printed in full and the rest counted, because a
/// systematic refusal would otherwise bury the program's own output.
fn report_refusal(e: &PipelineError) {
    static REFUSED: AtomicUsize = AtomicUsize::new(0);
    const SHOWN: usize = 20;
    let n = REFUSED.fetch_add(1, Ordering::Relaxed);
    if n < SHOWN {
        eprintln!(
            "[air] v2 refused findex={} {} at {}: {} — using v1 for it",
            e.findex,
            e.name,
            e.stage,
            e.brief()
        );
    } else if n == SHOWN {
        eprintln!("[air] v2 refusals past {SHOWN} silenced");
    }
}
