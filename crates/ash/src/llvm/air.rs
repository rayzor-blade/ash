//! AIR V2 preparation for the LLVM tier.
//!
//! LLVM consumes the verified typed SSA graph directly. Serialization is a
//! compatibility boundary for opcode consumers and is deliberately absent
//! from this module: neither ordinary JIT functions nor OSR entries are ever
//! reconstructed as HashLink bytecode.

use std::sync::OnceLock;

use crate::air_pipeline::{self, AirOptLevel, AirPassOptions, AshModule, PipelineError};
use crate::bytecode::DecodedBytecode;
use crate::types::HLFunction;

/// Lower and optimize one function for the LLVM backend without serializing
/// AIR back into HashLink opcodes.
///
/// The serializer is a compatibility boundary for opcode consumers. LLVM is
/// not one of those consumers: feeding its output to `translate_opcodes`
/// reconstructs control flow and types that AIR already made explicit and
/// leaves the legacy bytecode lowering as the actual backend. Keep the
/// hot-reload restrictions here, at the AIR boundary, but return the verified
/// SSA function itself.
pub(crate) fn prepare_llvm(
    bc: &DecodedBytecode,
    f: &HLFunction,
    hot_reload: bool,
    isolate_callees: bool,
) -> Result<air::v2::ir::Function, PipelineError> {
    let level = match (hot_reload, level()) {
        (true, AirOptLevel::O3) => AirOptLevel::O2,
        (_, level) => level,
    };
    let callees_visible = !(hot_reload || isolate_callees);
    let module = if callees_visible {
        AshModule::new(bc)
    } else {
        AshModule::new(bc).without_callees()
    };
    let opts = pass_options();
    // Through the shared cache, not a private pipeline run. AIR for a findex
    // is a pure function of (level, fma, callees_visible), which is exactly
    // what AirConfigKey keys on, so promotion has no reason to recompute what
    // the Cranelift tier already produced -- and in Auto mode Cranelift always
    // runs first, so this is a hit. Recomputing meant every LLVM promotion
    // redid lowering, the whole pass manager, and verification for a function
    // whose optimized SSA was already sitting in memory.
    let cfg = air_pipeline::AirConfigKey {
        level,
        fma: opts.fma,
        callees_visible,
    };
    air_pipeline::optimized_with_config(&module, f, cfg).map(|o| o.ir.clone())
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

/// Whether promoting this function needs the inliner to see bodies other than
/// its own — the question that decides whether it compiles into a private
/// module or the shared one.
///
/// A private module is much cheaper to emit: MCJIT produces a module's object
/// whole, so the shared one costs everything promoted so far (35-71ms of
/// codegen per promotion against 0-7ms). That buys nothing, though, if the
/// function's hot path calls something the module does not contain, because
/// the call then survives as a call to an external declaration.
///
/// The discriminator is a call the AIR inliner did not already remove, on the
/// path that actually runs:
///
/// - A loop that still contains a call has dispatch for LLVM to improve, and
///   it needs the callee to do it — method_call and closure_call both lose
///   ~10-18% without it.
/// - A loop whose callee AIR already inlined is straight-line arithmetic;
///   inlined_call and free_call gain ~9% from the cheaper module.
/// - No loop at all means the cost is per invocation. A SELF call does not
///   count: the body is already in whatever module holds the function, so
///   fib needs nothing else and gains most of all — 72ms to 17ms.
/// Whether a shared-module promotion could inline anything at all.
///
/// The shared module's only product is inlined callees: it lowers the whole
/// transitive closure so the inliner has bodies to work with, and that is what
/// costs 2287ms per promotion on MBHaxe. If no call site in the root has a
/// callee small enough to inline, the closure is lowered for nothing --
/// measured, 8 of 21 promotions inlined nothing and paid 8158ms between them.
///
/// Direct calls name their target. Indirect ones are asked of the same
/// call-site profile the devirtualiser itself consults, so a monomorphic
/// closure or method site counts exactly when the guard would fire.
pub(crate) fn promotion_wants_full_module(
    bc: &DecodedBytecode,
    f: &HLFunction,
    hot_reload: bool,
) -> bool {
    let m = if hot_reload {
        AshModule::new(bc).without_callees()
    } else {
        AshModule::new(bc)
    };
    // Undecidable means take the safe side: the shared module is what the
    // promote path did before this choice existed.
    let Ok(opt) = air_pipeline::optimized(&m, f) else {
        return true;
    };
    let ir = &opt.ir;
    let self_findex = f.findex as usize;
    let calls_out = |b: &air::v2::ir::Block, allow_self: bool| {
        b.instrs.iter().any(|i| match i {
            air::v2::ir::Instr::Call { fun, .. } => !allow_self || *fun != self_findex,
            air::v2::ir::Instr::CallMethod { .. } | air::v2::ir::Instr::CallClosure { .. } => true,
            _ => false,
        })
    };
    let cfg = air::v2::CfgInfo::build(ir);
    let forest = air::v2::LoopForest::analyze(ir, &cfg);
    let loops = forest.innermost_first();
    if loops.is_empty() {
        return ir.blocks.iter().any(|b| calls_out(b, true));
    }
    loops.into_iter().any(|l| {
        forest
            .get(l)
            .blocks
            .iter()
            .any(|b| calls_out(&ir.blocks[b.idx()], false))
    })
}
