//! Which bytecode optimizer the LLVM tier runs before emission.
//!
//! [`AirMode::V2`] is [`crate::air_pipeline`], the typed phi-SSA pipeline every
//! backend shares — `lower -> optimize -> verify -> serialize` — which hands
//! back a *new* opcode array and a *new* register-type table. It is the
//! default, and `ASH_AIR=none` is the only way to skip it.
//!
//! AIR v1 used to sit in front of it: three passes rewriting the opcode array
//! in place, skipping any function containing a `try`. It is removed. It was
//! not carrying much — v2 refused zero functions across the test corpus and the
//! Heaps sample, so the fallback never fired — and keeping it as the default
//! had a cost beyond its own: every new analysis got written against the opcode
//! array, because the opcode array was what actually ran. A function v2 refuses
//! is now left unoptimized and counted, so the gap is visible and gets closed
//! in v2 rather than papered over by a second pipeline.
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
            // v2 is the default: it is the IR the project is built around, and
            // leaving v1 in front of it made every new analysis get written
            // against the opcode array instead, which entrenched the thing we
            // are removing. v1 stays reachable as a bisect switch only.
            "" | "v2" | "2" => AirMode::V2,
            "none" => AirMode::None,
            // v1 is gone. It optimized three things in place, skipped every
            // function containing a `try`, and its presence in front of v2 was
            // what kept new analysis being written against the opcode array.
            // Measured across the corpus and the Heaps sample, v2 refused zero
            // functions, so there was nothing left for it to catch.
            "off" | "0" => AirMode::None,
            "v1" | "1" => {
                eprintln!("[air] ASH_AIR=v1: AIR v1 has been removed; using v2");
                AirMode::V2
            }
            other => {
                eprintln!("[air] unknown ASH_AIR='{other}' (expected v2|off|none); using v2");
                AirMode::V2
            }
        }
    })
}

/// The v2 opt level, from `ASH_AIR_LEVEL` — the same variable, and now the
/// same default, as [`air_pipeline::default_level`], which is what the
/// Cranelift tier and the interpreter's SSA body already consume.
///
/// This used to default to O2 while the shared cache defaulted to O3, and
/// the two passes that differ are exactly the two that matter: the inliner,
/// and SROA behind it. HL hands every `new C(...)` straight to a constructor
/// call, so an allocation escapes until that constructor is inlined — which
/// meant the LLVM tier kept per-iteration allocations the Cranelift tier had
/// already dissolved, and mandelbrot's LLVM code ran ~2x slower than its
/// Cranelift code (917ms against 420ms, two `hlp_alloc_obj` calls per
/// escape-loop iteration visible in the IR). The top tier was optimizing
/// less than the middle one.
fn level() -> AirOptLevel {
    static LEVEL: OnceLock<AirOptLevel> = OnceLock::new();
    *LEVEL.get_or_init(air_pipeline::default_level)
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
        // Refused. Fall through to v1, which wants the opposite order;
        // `run_v2` leaves `f` untouched on failure, so this is clean.
        //
        // Counted, not silent: v1 exists only to catch what v2 cannot yet
        // handle, and a refusal that nobody sees is a gap that never gets
        // closed. `ASH_PROFILE=phases` reports the total.
        // Refused: the function is left unoptimized rather than handed to a
        // second pipeline. Counted so the gap is visible and can be closed in
        // v2, which is where the fix belongs.
        crate::profile::count("air v2 refused (left unoptimized)", 1);
    }
    rewrite_indirect(f, indirect_call_natives);
}

fn rewrite_indirect(f: &mut HLFunction, natives: Option<&HashSet<usize>>) {
    if let Some(natives) = natives {
        IndirectCallRewritePass::new(natives.clone()).run(&mut f.ops);
    }
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
            dump_refused(f, &e);
            false
        }
    }
}

/// `ASH_AIR_DUMP_REFUSED=<dir>`: write the refused function's opcodes so a
/// refusal seen in the field arrives as a reproducer instead of a screenshot.
/// The corpus refuses nothing, so every real-world refusal is a lowering or
/// pass defect we cannot fix without exactly this artifact.
fn dump_refused(f: &HLFunction, e: &PipelineError) {
    let Some(dir) = std::env::var_os("ASH_AIR_DUMP_REFUSED") else {
        return;
    };
    let dir = std::path::PathBuf::from(dir);
    let _ = std::fs::create_dir_all(&dir);
    let path = dir.join(format!("refused_f{}_{}.txt", e.findex, e.stage));
    let mut body = format!(
        "findex: {}\nname: {}\nstage: {}\nerror: {}\nnregs: {}\nops: {}\n\n",
        e.findex,
        e.name,
        e.stage,
        e.brief(),
        f.regs.len(),
        f.ops.len()
    );
    for (i, (reg, _)) in f.regs.iter().zip(0..).enumerate() {
        body.push_str(&format!("reg r{i}: type#{}\n", reg.0));
    }
    body.push('\n');
    for (pc, op) in f.ops.iter().enumerate() {
        body.push_str(&format!("{pc:5}: {op:?}\n"));
    }
    match std::fs::write(&path, body) {
        Ok(()) => eprintln!("[air] refused function written to {}", path.display()),
        Err(err) => eprintln!("[air] could not write refusal dump: {err}"),
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
        // "left unoptimized", and say so — an earlier version of this line
        // claimed "using v1 for it" long after v1 was deleted, which sent at
        // least one reader hunting for a second pipeline that does not exist.
        eprintln!(
            "[air] v2 refused findex={} {} at {}: {} — function left unoptimized",
            e.findex,
            e.name,
            e.stage,
            e.brief()
        );
        if std::env::var_os("ASH_AIR_DUMP_REFUSED").is_none() {
            static HINTED: AtomicUsize = AtomicUsize::new(0);
            if HINTED.swap(1, Ordering::Relaxed) == 0 {
                eprintln!(
                    "[air] set ASH_AIR_DUMP_REFUSED=<dir> to write each refused \
function's bytecode for a bug report"
                );
            }
        }
    } else if n == SHOWN {
        eprintln!("[air] v2 refusals past {SHOWN} silenced");
    }
}
