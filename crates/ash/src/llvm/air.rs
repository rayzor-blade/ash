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
/// costs 2287ms per promotion in a large program. If no call site in the root has a
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
    // The same key `prepare_llvm` will ask under, because it is the same
    // question about the same body. Asking `optimized` here instead meant
    // asking the cache under `callees_visible: true` while handing it a
    // module with the callees hidden: on a miss that stores a body lowered
    // WITHOUT the inliner under the key every other consumer -- the
    // interpreter, the Cranelift tier, the OSR sites -- reads for the body
    // lowered WITH it, and whichever caller reached the findex first decided
    // for all of them. Only reachable under --hot-reload, which is the one
    // configuration that already runs a second level.
    let level = match (hot_reload, level()) {
        (true, AirOptLevel::O3) => AirOptLevel::O2,
        (_, level) => level,
    };
    let callees_visible = !hot_reload;
    let m = if callees_visible {
        AshModule::new(bc)
    } else {
        AshModule::new(bc).without_callees()
    };
    let cfg = air_pipeline::AirConfigKey {
        level,
        fma: pass_options().fma,
        callees_visible,
    };
    // Undecidable means take the safe side: the shared module is what the
    // promote path did before this choice existed.
    let Ok(opt) = air_pipeline::optimized_with_config(&m, f, cfg) else {
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

/// What the optimising tier could win on a function, before spending a
/// compile to find out.
///
/// The tier decision has been driven by how often a function is CALLED, which
/// says a function is hot but nothing about whether LLVM can do better than
/// the tier below. Those are different questions, and measured on Linux the
/// answer flips: LLVM beats Cranelift 2.0x on closure_call and 1.5x on
/// method_call, and loses 8x on deltablue -- not because its code is worse
/// there, but because the compile never amortizes. A gate needs both terms.
///
/// This is the first: an upper bound on what LLVM could win, from the AIR
/// alone. It cannot see how long the program will run; the caller supplies
/// that from invocation counts.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LlvmCeiling {
    /// Nothing here for LLVM that the tier below cannot do. A body with no
    /// loop and nothing but a call is the clearest case: `GetGlobal; Call;
    /// Ret` is three instructions whose generated code is the call. A game
    /// spent 60 seconds promoting exactly that shape.
    None,
    /// Straight-line work worth optimizing, or a loop whose body is calls --
    /// LLVM cannot see through a vtable any better than Cranelift can.
    Low,
    /// A loop LLVM has room to transform: licm, gvn, unrolling, and for a
    /// vectorizable one the thing Cranelift has no answer to at all.
    High,
}

/// `ASH_PROMOTE_GATE=0` disables the gate; unset or anything else keeps it.
/// Whether a promotion with this ceiling may go through the SHARED module.
///
/// The shared path re-optimizes and re-emits every body promoted so far, so
/// its cost is the module's, not the function's: in the game a 280-byte,
/// loop-free constructor (findex 3902, ceiling Low) paid 52 seconds because
/// the module had grown to 2,873 bodies -- and held the single promoter
/// thread for half the session, so four functions reached the top tier in
/// 103 seconds. The own-module path costs milliseconds; when it is refused,
/// only a High ceiling is worth what the shared one charges.
///
/// `ASH_SHARED_PROMOTE`: `high` (default) admits High only; `all` restores
/// the old behaviour, for the A/B.
pub fn shared_promote_allows(ceiling: LlvmCeiling) -> bool {
    static MIN_HIGH: OnceLock<bool> = OnceLock::new();
    let high_only = *MIN_HIGH.get_or_init(|| {
        match std::env::var("ASH_SHARED_PROMOTE").as_deref() {
            Ok("all") | Ok("low") => false,
            Ok("high") | Err(_) | Ok("") => true,
            Ok(other) => {
                eprintln!("[tier] ignoring ASH_SHARED_PROMOTE='{other}' (expected high|all); using high");
                true
            }
        }
    });
    !high_only || ceiling == LlvmCeiling::High
}

pub fn promotion_gate_enabled() -> bool {
    static ON: OnceLock<bool> = OnceLock::new();
    *ON.get_or_init(|| std::env::var("ASH_PROMOTE_GATE").map(|v| v != "0").unwrap_or(true))
}

/// The ceiling for one function, memoized: the AIR pipeline is not something
/// to re-run per proposal, and a broker can propose the same findex tens of
/// thousands of times.
pub fn llvm_ceiling(bc: &DecodedBytecode, f: &HLFunction) -> LlvmCeiling {
    use std::collections::HashMap;
    use std::sync::Mutex;
    static CACHE: Mutex<Option<HashMap<usize, LlvmCeiling>>> = Mutex::new(None);
    let findex = f.findex as usize;
    if let Ok(g) = CACHE.lock() {
        if let Some(hit) = g.as_ref().and_then(|m| m.get(&findex).copied()) {
            return hit;
        }
    }
    let ceiling = compute_ceiling(bc, f);
    if let Ok(mut g) = CACHE.lock() {
        g.get_or_insert_with(HashMap::new).insert(findex, ceiling);
    }
    ceiling
}

fn compute_ceiling(bc: &DecodedBytecode, f: &HLFunction) -> LlvmCeiling {
    use air::v2::ir::Instr;
    // Undecidable means promote: refusing on a body we could not analyze
    // would retire a function for a reason we cannot name, which is the
    // failure mode the decline tally exists to make visible.
    let Ok(opt) = air_pipeline::optimized(&AshModule::new(bc), f) else {
        return LlvmCeiling::High;
    };
    let ir = &opt.ir;
    // With the int pool, so strides are the real magnitudes. Without it the
    // analysis falls back to reading a constant's POOL INDEX as its value,
    // which reports a step of 1 stored at index 2 as a stride of 2 and
    // refuses the loop as non-contiguous -- the ceiling would then be Low for
    // loops that are in fact the best candidates the tier has.
    let plans = air::v2::vectorize::analyze_with(
        ir,
        &air::v2::vectorize::VecOptions::default(),
        &|i| bc.ints.get(i).copied(),
    );
    if plans.iter().any(|p| p.vectorizable()) {
        return LlvmCeiling::High;
    }
    let is_call = |i: &Instr| {
        matches!(
            i,
            Instr::Call { .. } | Instr::CallMethod { .. } | Instr::CallClosure { .. }
        )
    };
    let instrs: usize = ir.blocks.iter().map(|b| b.instrs.len()).sum();
    let calls: usize = ir
        .blocks
        .iter()
        .map(|b| b.instrs.iter().filter(|i| is_call(i)).count())
        .sum();
    if plans.is_empty() {
        // No loop. Whatever LLVM does here it does once per call, so the
        // win is bounded by the body -- and a body that is mostly a call is
        // bounded by the callee, which this promotion does not compile.
        let non_call = instrs.saturating_sub(calls);
        if non_call <= 8 {
            return LlvmCeiling::None;
        }
        return LlvmCeiling::Low;
    }
    // A loop, but not a vectorizable one. Worth the optimiser when it has
    // real work between the calls, not when every iteration is dispatch.
    if calls * 3 >= instrs {
        LlvmCeiling::Low
    } else {
        LlvmCeiling::High
    }
}
