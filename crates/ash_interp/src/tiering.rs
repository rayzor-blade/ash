//! Tiered compilation: the policy that decides when a function is promoted,
//! the state shared with the compile brokers, and the two backends' entry
//! points.
//!
//! Split out of `interpreter` so the interpreter file is the interpreter. The
//! types here are `pub(crate)` rather than private because the interpreter
//! drives them; nothing outside this crate uses them.

#![allow(dead_code)]

use crate::interpreter::retier_abandoned;
use anyhow::{anyhow, Result};
use ash_core::bytecode::DecodedBytecode;
use ash_core::hl_bindings::{self as hl, hl_type};
use ash_core::llvm::module::{CompiledFunctionMeta, JITModule};
use beadie::{Bead, OsrEntry, TieredAdapter, TieredBound};
use std::collections::{HashMap, HashSet};
use std::ffi::c_void;
use std::mem::ManuallyDrop;
use std::sync::{Arc, Condvar, Mutex, OnceLock};

/// Which rungs of the interpreter → Cranelift → LLVM ladder are active.
///
/// Selected with `--jit-tier` or `ASH_TIER`; `Auto` is the shipped ladder.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum TierMode {
    /// Two tiers: Cranelift at `jit_threshold` (falling back to LLVM for
    /// functions outside the Cranelift opcode subset), LLVM at
    /// `jit_threshold * 100`.
    #[default]
    Auto,
    /// One tier, Cranelift only. Functions outside the subset never promote —
    /// useful for isolating the middle tier under test.
    Cranelift,
    /// One tier, LLVM only — the pre-ladder behaviour.
    Llvm,
    /// No promotion at all; pure interpretation.
    Off,
}

impl TierMode {
    pub fn parse(s: &str) -> Option<Self> {
        match s.trim().to_ascii_lowercase().as_str() {
            "auto" => Some(TierMode::Auto),
            "cranelift" | "cl" => Some(TierMode::Cranelift),
            "llvm" => Some(TierMode::Llvm),
            "off" | "none" => Some(TierMode::Off),
            _ => None,
        }
    }

    pub fn name(self) -> &'static str {
        match self {
            TierMode::Auto => "auto",
            TierMode::Cranelift => "cranelift",
            TierMode::Llvm => "llvm",
            TierMode::Off => "off",
        }
    }
}

#[derive(Debug, Clone)]
pub struct TieredConfig {
    pub enabled: bool,
    /// Compile every reached bytecode function synchronously before its first
    /// invocation. This is the execution policy for `--mode jit`: the
    /// interpreter remains the runtime host and native/stub bridge, but Haxe
    /// bytecode is never executed by it.
    pub compiled_only: bool,
    /// Invocations before a function is promoted to the Cranelift tier.
    pub jit_threshold: u64,
    /// Invocations before a function is promoted from the baseline JIT to
    /// the optimising tier.
    ///
    /// This is how many invocations it takes to reach the top tier by
    /// counting. A very high value does not turn the tier off; it means the
    /// counter is not what gets a function there.
    ///
    /// This rung is reached only by INTERPRETED calls and loop back-edges:
    /// once a caller is itself compiled it dispatches directly and stops
    /// ticking the callee's counter, so a threshold far above the Cranelift
    /// one is unreachable for any function whose callers compile.
    ///
    /// It carries the whole tier-1 load, so it has to fire while the program
    /// is still running. Measured against the speculative LLVM compile that
    /// used to shadow it: at 1000 closure_call is 198ms because the rung
    /// arrives too late, at 100-250 it is 156ms — better than the speculative
    /// path managed (163ms), with fib, deltablue and binary_trees unchanged.
    pub opt_threshold: u64,
    /// Cap on how wide a signature may be to promote.
    ///
    /// The marshaling path itself no longer needs one: anything past the
    /// inline eight goes through the backend's uniform entry. But letting
    /// Cranelift compile those signatures for the first time produced a frame
    /// sized for one function running another's opcodes in a large program, and the
    /// default is not the place to carry an unproven path. Raise it explicitly
    /// to exercise the uniform entry.
    pub max_jit_args: usize,
    pub min_ops_for_promotion: usize,
    pub log_promotions: bool,
    pub strict_mode: bool,
    pub hot_reload: bool,
    pub tier_mode: TierMode,
}

impl Default for TieredConfig {
    fn default() -> Self {
        Self {
            enabled: false,
            compiled_only: false,
            jit_threshold: 100,
            opt_threshold: 250,
            max_jit_args: 8,
            // 0 disables the static opcode-size gate; promotion hotness is call-count based.
            min_ops_for_promotion: 0,
            log_promotions: false,
            strict_mode: true,
            hot_reload: false,
            tier_mode: TierMode::Auto,
        }
    }
}

/// Threshold sets for common program shapes.
///
/// | Preset        | jit | opt   | Suits                                       |
/// |---------------|-----|-------|---------------------------------------------|
/// | `Script`      |  20 | 2 000 | one-shot CLI work; optimise only if the run lasts |
/// | `Application` | 100 | 1 000 | the default: balanced startup and peak      |
/// | `Game`        |  10 |   200 | frame budgets: compile during load, not play |
/// | `Server`      |  50 |   500 | long-lived processes; compile cost amortises |
/// | `Benchmark`   |   2 |    10 | drive everything through every tier          |
/// | `Development` | 100 | 5 000 | favours iteration over peak                 |
/// | `Interpreter` | off |   off | no JIT at all                               |
///
/// Most of these are shaped by what a program needs day to day. `Benchmark` is
/// not: it exists to exercise the tiers themselves, and on a short program it
/// will be SLOWER, because compiles it cannot amortise still get paid for —
/// at `jit_threshold` 1, bench_deltablue goes from 66ms to 468ms. A published
/// benchmark row should therefore name the preset its readers actually run.
///
/// A preset is a starting point, not a policy: take one and adjust, or build
/// a [`TieredConfig`] directly. `ASH_TIER1` still overrides the LLVM rung at
/// runtime whatever the config says.
#[derive(Debug, Clone)]
pub enum TierPreset {
    Script,
    Application,
    Game,
    Server,
    Benchmark,
    Development,
    Interpreter,
    /// Use the given config verbatim.
    Custom(TieredConfig),
}

impl TierPreset {
    /// Parse a preset by name. Returns `None` for an unknown name so the
    /// caller can report it rather than silently choosing a default.
    pub fn parse(s: &str) -> Option<Self> {
        match s.trim().to_ascii_lowercase().as_str() {
            "script" => Some(TierPreset::Script),
            "application" | "app" => Some(TierPreset::Application),
            "game" => Some(TierPreset::Game),
            "server" => Some(TierPreset::Server),
            "benchmark" | "bench" => Some(TierPreset::Benchmark),
            "development" | "dev" => Some(TierPreset::Development),
            "interpreter" | "interp" | "none" => Some(TierPreset::Interpreter),
            _ => None,
        }
    }

    pub fn names() -> &'static [&'static str] {
        &[
            "script",
            "application",
            "game",
            "server",
            "benchmark",
            "development",
            "interpreter",
        ]
    }

    pub fn to_config(self) -> TieredConfig {
        let base = TieredConfig::default();
        match self {
            TierPreset::Script => TieredConfig {
                enabled: true,
                jit_threshold: 20,
                opt_threshold: 2_000,
                ..base
            },
            TierPreset::Application => TieredConfig {
                enabled: true,
                jit_threshold: 100,
                opt_threshold: 250,
                ..base
            },
            // A frame budget is the constraint, not throughput. Promote while
            // the program is still loading so a compile does not land in the
            // middle of frame 400: a function called once per frame passes 10
            // in the first sixth of a second and 200 within a few seconds, so
            // both tiers are reached during startup rather than during play.
            TierPreset::Game => TieredConfig {
                enabled: true,
                jit_threshold: 10,
                opt_threshold: 200,
                ..base
            },
            TierPreset::Server => TieredConfig {
                enabled: true,
                jit_threshold: 50,
                opt_threshold: 250,
                ..base
            },
            // Not shaped by a program's needs: this one exists to pull every
            // performance lever, promoting almost at once so every function
            // passes through both tiers and the whole machinery is exercised.
            TierPreset::Benchmark => TieredConfig {
                enabled: true,
                jit_threshold: 2,
                opt_threshold: 10,
                ..base
            },
            TierPreset::Development => TieredConfig {
                enabled: true,
                jit_threshold: 100,
                opt_threshold: 5_000,
                ..base
            },
            TierPreset::Interpreter => TieredConfig {
                enabled: false,
                jit_threshold: u64::MAX,
                opt_threshold: u64::MAX,
                tier_mode: TierMode::Off,
                ..base
            },
            TierPreset::Custom(c) => c,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct TieredStats {
    pub attempted_promotions: u64,
    pub successful_promotions: u64,
    pub failed_promotions: u64,
    pub compiled_calls: u64,
    pub fallback_calls: u64,
    /// Functions whose installed code came from the Cranelift middle tier.
    pub cranelift_promotions: u64,
    /// Functions whose installed code came from the LLVM top tier (either as
    /// a tier-0 fallback or as a tier-1 upgrade).
    pub llvm_promotions: u64,
}

/// Widen a type kind to the `u32` that `ValueTypeKind::try_from` takes.
///
/// The cast is a no-op under clang (bindgen types `hl_type_kind` u32) but a
/// real conversion under MSVC (i32), so clippy's `unnecessary_cast` is wrong
/// on exactly one platform — hence the allow lives here, once.
#[allow(clippy::unnecessary_cast)]
#[inline(always)]
pub(crate) fn kind_u32(kind: hl::hl_type_kind) -> u32 {
    kind as u32
}

/// Everything needed to call one promoted function, cached per findex.
///
/// `Copy`, deliberately. This is read on every invocation of every compiled
/// function — ~10M times in one nbody run — and an `Arc<[u32]>` here cost an
/// atomic increment and decrement per call for a value that never changes once
/// the function is compiled. Signatures up to eight arguments — every one that
/// matters for throughput — keep their kinds inline, so the steady-state
/// dispatch path touches no refcount and no allocation. A wider signature
/// carries a leaked slice instead, allocated once at registration.
#[derive(Debug, Clone, Copy)]
pub(crate) struct CompiledFunctionEntry {
    pub(crate) fn_addr: usize,
    // Kinds carry the bindgen alias, not a bare integer: MSVC types the C
    // enum i32 where clang types it u32, so only the alias compiles on both.
    // Must stay agreed with ash_core's `CompiledFunctionMeta`, which is
    // alias-typed the same way.
    pub(crate) arg_kinds: [hl::hl_type_kind; 8],
    pub(crate) nargs: u8,
    pub(crate) ret_kind: hl::hl_type_kind,
    /// Backend-emitted uniform entry, `extern "C" fn(*const i64) -> i64`, for
    /// signatures no fixed-arity ladder in Rust can express. Zero when the
    /// ordinary typed call handles this function.
    pub(crate) uniform_addr: usize,
    /// The full kind list once `nargs` exceeds the inline array. Leaked once
    /// per wide function — a handful per program — so the entry stays `Copy`
    /// and the common path keeps its refcount-free dispatch.
    pub(crate) wide_kinds: Option<&'static [hl::hl_type_kind]>,
}

impl CompiledFunctionEntry {
    #[inline(always)]
    pub(crate) fn args(&self) -> &[hl::hl_type_kind] {
        match self.wide_kinds {
            Some(wide) => wide,
            None => &self.arg_kinds[..self.nargs as usize],
        }
    }
}

/// A callee's marshaling signature, read off the bytecode once at bead
/// registration. Same inline/wide split as [`CompiledFunctionEntry`].
#[derive(Debug, Clone, Copy)]
pub(crate) struct CallSignature {
    pub(crate) arg_kinds: [hl::hl_type_kind; 8],
    pub(crate) nargs: u8,
    pub(crate) ret_kind: hl::hl_type_kind,
    pub(crate) wide_kinds: Option<&'static [hl::hl_type_kind]>,
}

/// The pre-warmed LLVM JIT module, owned by whichever broker thread first
/// claims it and then kept behind `TieredSharedCtx::llvm`.
///
/// `ManuallyDrop` because LLVM objects may throw foreign exceptions during
/// drop on some platforms — the module is intentionally leaked at exit (same
/// as the old worker's `std::mem::forget` on shutdown).
pub(crate) struct LlvmModule(pub(crate) ManuallyDrop<JITModule<'static>>);

// SAFETY: the module is only ever touched while `TieredSharedCtx::llvm` is
// locked, so exactly one thread dereferences it at a time — the same
// justification the old single-broker-thread hand-off relied on, now enforced
// by the mutex instead of by there being only one broker.
unsafe impl Send for LlvmModule {}

// One tier-state cell per process, touched only at promotion time — boxing the
// large pre-warm variant would add indirection for no measurable gain.
#[allow(clippy::large_enum_variant)]
pub(crate) enum LlvmState {
    /// Handed off from the main thread's pre-warm, not yet claimed.
    Pending(PrewarmedJit),
    Ready(LlvmModule),
    /// Pre-warm failed; the LLVM tier is unavailable for this run.
    Unavailable,
}

/// Cranelift middle tier, built lazily on the broker thread at the first
/// promotion (~1 ms; deliberately not part of startup).
pub(crate) struct CraneliftTier {
    pub(crate) backend: ash_core::cranelift::AshCraneliftBackend,
    pub(crate) ctx: ash_core::cranelift::CraneliftTierContext,
}

/// Raw handles the Cranelift lowering needs; all process-lifetime shared
/// arrays, captured once in `enable_tiered`.
#[derive(Clone)]
pub(crate) struct SharedArrayHandles {
    pub(crate) globals_data: usize,
    pub(crate) nglobals: usize,
    pub(crate) functions_ptrs: usize,
    /// Runtime `hl_type*` per bytecode type index, copied out of
    /// `SharedRuntimeHandles::c_types` so the Cranelift tier can hand a type
    /// identity to an allocator without borrowing the interpreter's tables
    /// across threads.
    pub(crate) c_types: Vec<usize>,
}

/// State shared between the interpreter thread and beadie's tier brokers.
/// Compile closures capture this via `Arc`.
pub(crate) struct TieredSharedCtx {
    pub(crate) log_promotions: bool,
    /// `ASH_TIER_LOG=1` (or `--jit-log`): one line per installed function
    /// naming the findex and the tier that produced it.
    pub(crate) tier_log: bool,
    pub(crate) mode: TierMode,
    /// Whether this run prohibits interpreted Haxe frames. In this mode the
    /// Cranelift baseline is installed synchronously and its statically known
    /// re-tier sites drive LLVM OSR entry generation.
    pub(crate) compiled_only: bool,
    /// The LLVM top tier. Pre-warmed on the MAIN thread by `enable_tiered`,
    /// before any bytecode runs, because module init GC-allocates (constants,
    /// obj runtimes, enum marks) and a broker-side collection would scan the
    /// wrong stack. Only compilation happens here.
    pub(crate) llvm: Mutex<LlvmState>,
    /// The Cranelift middle tier. `None` until first use, `Some(None)` once
    /// construction has been tried and failed.
    pub(crate) cranelift: Mutex<Option<Option<Arc<CraneliftTier>>>>,
    pub(crate) arrays: SharedArrayHandles,
    /// The decoded bytecode the brokers lower from — OWNED, not borrowed.
    ///
    /// This was an AtomicUsize holding a raw pointer published by the CLI,
    /// with a SAFETY note asking the caller to keep the decode alive for the
    /// whole process. That contract is unenforceable and was duly broken:
    /// removing the exit-time join let main drop the decode while a broker
    /// was still lowering, and the broker indexed a freed Vec ("len is 469
    /// but the index is 14155380286610417437"). Leaking the decode by hand
    /// fixed that one site and CI found the next.
    ///
    /// Holding an Arc makes the lifetime a fact rather than a request: every
    /// compile thread captures this context by Arc, so the bytecode cannot
    /// outlive its readers or be dropped beneath them.
    pub(crate) bytecode: OnceLock<Arc<DecodedBytecode>>,
    /// `max(findex) + 1`, matching the length of `functions_ptrs`.
    pub(crate) max_findex: std::sync::atomic::AtomicUsize,
    /// Findexes whose installed code already came from LLVM — a tier-1
    /// upgrade for those would recompile identical code.
    pub(crate) llvm_done: Mutex<HashSet<usize>>,
    /// Findexes whose LLVM compile already failed. beadie re-proposes a
    /// promotion that returned no code, which is what lets a REFUSAL turn
    /// into code later; a failed compile would just fail again at full
    /// price, each attempt holding the global `llvm` mutex, so the answer
    /// is memoized and the re-proposals cost a null return.
    pub(crate) llvm_failed: Mutex<HashSet<usize>>,
    /// Loop headers the interpreter has probed hot, `findex -> header pcs`,
    /// written by `note_hot_loop` on the main thread and read by the broker
    /// when an LLVM promote finishes. The pcs index the SAME opcode array the
    /// interpreter executes (`air::Cache::body`), which the broker mirrors
    /// through the shared `air_pipeline::optimized` cache — an entry compiled
    /// against a separately optimized copy would name a different
    /// instruction.
    pub(crate) hot_loop_pcs: Mutex<HashMap<usize, Vec<usize>>>,
    /// Functions with a live frame of their own directly beneath a call to
    /// them — the recursion counterpart of a hot loop.
    ///
    /// A loop and a re-entry are the same evidence wearing different clothes:
    /// a frame of this function is running and has more of itself to do, so
    /// code compiled for it has somewhere to land. Recursion is the shape with
    /// no loop header, so `hot_loop_pcs` never sees it, and fib is nothing
    /// else -- 340ms on the middle tier against 34ms on the top one.
    ///
    /// "Directly beneath" is the interpreter's view of the stack, which is
    /// wider than a self-call: compiled frames are not on it, so mutual
    /// recursion registers here too once either function reaches Cranelift and
    /// stops interposing an interpreted frame. That is the same fact, not a
    /// false positive -- the frame really is live and really will be returned
    /// into. Before either side compiles, mutual recursion has no signal at
    /// all; measured on a mutual fib(38) that costs nothing, because the pair
    /// promotes as soon as the first of them compiles.
    pub(crate) live_frame: Mutex<std::collections::HashSet<usize>>,
    /// Functions observed being called from a frame that is deep in a loop of
    /// its own -- the third shape of the same evidence.
    ///
    /// A leaf carries no loop and no re-entry, so neither `hot_loop_pcs` nor
    /// `live_frame` can ever see it, and once its caller runs as compiled
    /// code the interpreter stops seeing it at all. What is visible while the
    /// caller is still interpreted is the caller's own back-edge count: a
    /// frame 64 iterations into its loop has work left, and every callee it
    /// reaches is part of that work. bench_method_call's `step` is exactly
    /// this shape -- eleven opcodes, no loop, 100M calls from one frame.
    pub(crate) called_from_loop: Mutex<std::collections::HashSet<usize>>,
    /// `findex -> [(type index, vtable slot)]`, over every HOBJ/HSTRUCT
    /// type whose vtable names the findex (own protos and inherited ones —
    /// `pindex` is absolute across the super chain, and every subclass's
    /// `vobj_proto` holds its own COPY of the ancestor's row). Built once,
    /// on the first install that needs it.
    ///
    /// This exists because `hl_get_obj_proto` fills `vobj_proto` by value
    /// from `functions_ptrs` at type-init time: promotion updates the slot
    /// it copied FROM, and without this map the copies keep the interpreter
    /// stub sentinels forever — which made every virtual dispatch from
    /// compiled code take a malloc-per-call bridge through the interpreter.
    /// Measured: 100M dispatches in 26.6s, 77% of it interpreter samples.
    pub(crate) vtable_slots: OnceLock<HashMap<usize, Vec<(usize, usize)>>>,
    /// OSR entries an LLVM promote has compiled but not yet attached,
    /// `findex -> entries`. The broker cannot attach them itself: beadie's
    /// adapter installs the main code pointer only after the compile closure
    /// returns, and `swap_compiled_with_osr` before that install would have
    /// its table orphaned by the install's generation bump. The main thread
    /// attaches on next observing the fresh pointer in `tiered_on_invoke` —
    /// for a single-invocation hot loop that observation comes from the
    /// back-edge ticks, at most 64 iterations later.
    pub(crate) pending_osr: Mutex<HashMap<usize, Vec<OsrEntry>>>,
    /// Uniform-ABI entries the backends emitted, `findex -> address`. The
    /// marshaling signature is read off the bytecode at bead registration,
    /// before any compile has happened, so the address a compile produces has
    /// to reach the entry builder some other way.
    pub(crate) uniform_entries: Mutex<HashMap<usize, usize>>,
    /// Per-findex beads for compiled-only lazy sentinel resolution. The lock
    /// protects bead creation; the backend mutexes serialize cold compiles.
    pub(crate) worker_beads: Mutex<HashMap<usize, Arc<Bead>>>,
    /// Serializes the check/compile/install sequence for cold worker entries.
    /// Dependency discovery runs after this guard is released, so recursive
    /// closure graphs do not deadlock it.
    pub(crate) worker_compile_lock: Mutex<()>,
    /// Functions whose AIR V2 closure dependencies have been prepared for
    /// native worker execution. A second OS worker waits for an in-progress
    /// scan; recursion on the preparing thread recognizes its own cycle.
    pub(crate) worker_closure_deps: Mutex<HashMap<usize, WorkerClosureDepsState>>,
    pub(crate) worker_closure_deps_changed: Condvar,
    pub(crate) attempted: std::sync::atomic::AtomicU64,
    pub(crate) failed: std::sync::atomic::AtomicU64,
    pub(crate) cranelift_promotions: std::sync::atomic::AtomicU64,
    pub(crate) llvm_promotions: std::sync::atomic::AtomicU64,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum WorkerClosureDepsState {
    Preparing(std::thread::ThreadId),
    Ready,
}

impl TieredSharedCtx {
    /// The bytecode, borrowed from this context's own Arc. No unsafe, and no
    /// lifetime obligation on the caller: the borrow cannot outlive the
    /// context, and every compile thread holds the context by Arc.
    pub(crate) fn bytecode_ptr(&self) -> Option<&DecodedBytecode> {
        self.bytecode.get().map(|b| &**b)
    }
}

/// Raw pointer to the JIT module pre-warmed on the main thread, handed off to
/// beadie's broker threads. `Send` is sound the same way `SharedRuntimeHandles`
/// is: the main thread never touches the module again, and every consumer goes
/// through `TieredSharedCtx::llvm`.
pub(crate) struct PrewarmedJit(pub(crate) *mut ManuallyDrop<JITModule<'static>>);
unsafe impl Send for PrewarmedJit {}

/// Tiered promotion state built on beadie's `TieredAdapter`.
///
/// One `TieredBound` per tierable findex (registered lazily on first call);
/// beadie owns the hotness tick, the per-tier promotion CAS, and one
/// background compile thread per tier. The interpreter keeps only marshaling
/// metadata and stats.
pub(crate) struct TieredRuntime {
    pub(crate) config: TieredConfig,
    pub(crate) adapter: TieredAdapter,
    /// findex-indexed bounds. `None` = gate not yet run, or untierable.
    pub(crate) beads: Vec<Option<TieredBound>>,
    /// findex-indexed: whether the one-time registration gate has run.
    pub(crate) gate_checked: Vec<bool>,
    /// findex-indexed cache of marshaling metadata for compiled functions.
    pub(crate) entries: Vec<Option<CompiledFunctionEntry>>,
    /// findex-indexed marshaling signature, derived from the bytecode and
    /// therefore identical for every tier.
    pub(crate) sigs: Vec<Option<CallSignature>>,
    pub(crate) shared_ctx: Arc<TieredSharedCtx>,
    /// Interp-side counters; broker-side counters live in `shared_ctx`.
    pub(crate) stats: TieredStats,
}

/// Whether anything is waiting for LLVM code for `findex`.
///
/// Three shapes of one question -- is there a live frame with work left that
/// this code could land in?
///
/// * a hot loop the interpreter probed, which is also what OSR needs to carry
///   a running frame across into the new code;
/// * a live frame of the callee directly beneath the call, which is the same
///   fact for a function with no loop header. fib is nothing but this shape;
/// * a call from a frame already deep in a loop of its own, which is the only
///   shape a leaf can have. `step` in bench_method_call has no loop and no
///   recursion, and is called 100M times from one frame.
///
/// The third is not a refinement of the first two but the load-bearing one:
/// every signal here is read from the interpreter, and the interpreter stops
/// seeing a function the moment its CALLER is compiled. A leaf's whole
/// observable life is the window before that.
///
/// Deliberately NOT a count, a rate or an elapsed time. Rate does not separate
/// them -- deltablue's wasted promotion runs at 585 calls/ms against
/// binary_trees' useful 387/ms -- and elapsed time does not either, since fib
/// asks at 1.7ms and is right to.
pub(crate) fn llvm_demand(ctx: &Arc<TieredSharedCtx>, findex: usize) -> bool {
    if ctx
        .hot_loop_pcs
        .lock()
        .expect("hot_loop_pcs mutex poisoned")
        .contains_key(&findex)
    {
        return true;
    }
    if ctx
        .live_frame
        .lock()
        .expect("live_frame mutex poisoned")
        .contains(&findex)
    {
        return true;
    }
    ctx.called_from_loop
        .lock()
        .expect("called_from_loop mutex poisoned")
        .contains(&findex)
}

/// This thread's OS id, for the tier log below.
///
/// `pthread_self` has no Windows counterpart, and the id is only ever read by
/// a human comparing two log lines, so the platform's own notion of a thread
/// id is answer enough on either side.
fn current_thread_id() -> usize {
    #[cfg(unix)]
    {
        unsafe { libc::pthread_self() as usize }
    }
    #[cfg(windows)]
    {
        unsafe { windows_sys::Win32::System::Threading::GetCurrentThreadId() as usize }
    }
}

/// Dispatch one compile job to the backend that owns `tier`.
///
/// Tier 0 in `Auto` mode tries Cranelift first and falls back to LLVM: a
/// Cranelift decline must never leave the bead with null code, because
/// beadie's primary broker treats a null tier-0 result as a permanent
/// invalidation and the function would then never reach the LLVM tier either.
/// Compile `findex` at `tier`.
///
/// Every caller runs this on whichever thread demanded the code, which is
/// regularly a fiber worker, and a compile takes long enough to be the reason
/// a collection waits -- one measured world stop spent 352ms on a worker that
/// reached no safepoint in that window. So the whole compile is declared
/// blocking: it touches no GC object, and `hl_blocking` publishes the thread's
/// stack pointer and callee-saved registers before returning, leaving the
/// stack conservatively scannable while the collector runs alongside. On a
/// thread that is not a registered mutator the primitive is a no-op.
pub(crate) fn tiered_compile_tier(
    ctx: &Arc<TieredSharedCtx>,
    tier: usize,
    findex: usize,
    bead: &Arc<Bead>,
    may_block: bool,
) -> *mut () {
    // SAFETY: the primitive only touches this thread's own mutator record.
    unsafe { ash_core::hl_bindings::hl_blocking(true) };
    let began = std::time::Instant::now();
    let code = tiered_compile_tier_inner(ctx, tier, findex, bead, may_block);
    let took = began.elapsed();
    unsafe { ash_core::hl_bindings::hl_blocking(false) };
    if took.as_millis() >= 20 && (ctx.tier_log || env_flag!("ASH_TIER_LOG")) {
        // Name as well as id: a bare pthread_self cannot be read as "this ran
        // on the frame loop" without guessing, and a tier-1 compile landing on
        // the calling thread rather than a broker is exactly the thing worth
        // knowing. Note the name is a FALLBACK -- an unnamed thread also reads
        // as "main" -- so the id stays alongside it.
        eprintln!(
            "[tier] compile findex={findex} tier={tier} took {:.1}ms on {} ({:#x})",
            took.as_secs_f64() * 1e3,
            std::thread::current().name().unwrap_or("main"),
            current_thread_id()
        );
    }
    code
}

fn tiered_compile_tier_inner(
    ctx: &Arc<TieredSharedCtx>,
    tier: usize,
    findex: usize,
    bead: &Arc<Bead>,
    may_block: bool,
) -> *mut () {
    use std::sync::atomic::Ordering;
    // Whoever compiled it first publishes through `functions_ptrs`, and that
    // slot is the one piece of state both compile paths can see. The beads
    // cannot see each other: the interpreter ticks the bead in
    // `TieredRuntime::beads`, the broker keeps its own in `worker_beads`
    // because the interpreter's Vec is not in the shared context, and
    // beadie's guard is per bead. So each decided independently that the
    // function needed compiling -- deltablue built 52 bodies for 45 distinct
    // (function, tier) pairs, main and beadie-broker each producing one.
    if tier == 0 && ctx.arrays.functions_ptrs != 0 {
        let installed =
            unsafe { *(ctx.arrays.functions_ptrs as *const *mut c_void).add(findex) };
        if installed as usize >= ash_core::llvm::stub_bridge::STUB_SENTINEL_LIMIT as usize {
            return installed.cast::<()>();
        }
    }
    ctx.attempted.fetch_add(1, Ordering::Relaxed);
    if std::env::var("ASH_TIER1_PROBE").is_ok() {
        static T0: std::sync::OnceLock<std::time::Instant> = std::sync::OnceLock::new();
        let t0 = *T0.get_or_init(std::time::Instant::now);
        eprintln!(
            "[probe] tier={tier} findex={findex} calls={} at={:.1}ms",
            bead.invocation_count(),
            t0.elapsed().as_secs_f64() * 1e3
        );
    }
    let code = match (ctx.mode, tier) {
        (TierMode::Cranelift, 0) => compile_with_cranelift(ctx, findex, bead),
        (TierMode::Llvm, 0) => compile_with_llvm(ctx, 0, findex, may_block, Some(bead)),
        (TierMode::Auto, 0) => {
            let cl = compile_with_cranelift(ctx, findex, bead);
            if cl.is_null() {
                compile_with_llvm(ctx, 0, findex, may_block, Some(bead))
            } else {
                cl
            }
        }
        (TierMode::Auto, 1) => {
            if ctx
                .llvm_failed
                .lock()
                .expect("llvm_failed mutex poisoned")
                .contains(&findex)
            {
                // Already failed once; the same compile at the same findex
                // fails the same way, so beadie's re-proposals get their
                // answer without the compile.
                return std::ptr::null_mut();
            }
            if ctx
                .llvm_done
                .lock()
                .expect("llvm_done mutex poisoned")
                .contains(&findex)
            {
                // Already LLVM code — nothing to upgrade to. A null here is
                // harmless: the promotion broker keeps the current tier.
                return std::ptr::null_mut();
            }
            // The counter proposes; demand disposes.
            //
            // An invocation count says how much this function HAS run, which
            // is not the question -- an LLVM compile only repays out of what
            // is LEFT. Demand is a frame that is still going: this function's
            // own loop, its own re-entry, or a caller's loop it is called
            // from. Across deltablue, binary_trees and nbody, eight of ten
            // LLVM compiles went to a function with no hot loop at all --
            // deltablue's four promoted and its seven looping being disjoint
            // sets -- and its broker thread was 35.7% of the CPU for a program
            // that ends in 73ms.
            //
            // The gate abstains where its evidence does not exist. The SSA
            // dispatcher has no pcs, offers no OSR entry, and sees a coarser
            // view of a frame's progress than the opcode loop; refusing on
            // that view starved deltablue of the two promotions it makes at
            // 8ms and 15ms and took the run from 94ms to 400ms. A signal that
            // cannot be read must not be treated as a signal that is absent.
            if ctx.compiled_only || crate::ssa::enabled() {
                return compile_with_llvm(ctx, 1, findex, may_block, Some(bead));
            }
            // Refusing leaves the bead on Cranelift and is a postponement,
            // not a veto: beadie lowers the tier-1 queued flag when a compile
            // returns no code and re-proposes at doubled invocation counts,
            // so a refusal costs O(log calls) re-asks and a function whose
            // demand arrives later is compiled at the first horizon after it
            // does. The window is still bounded by observability -- every
            // signal above is read from the interpreter, and a function whose
            // callers have all compiled stops ticking, at which point no
            // re-proposal fires and none could be answered.
            if !llvm_demand(ctx, findex) {
                if ctx.tier_log {
                    eprintln!("[tier] defer findex={findex} tier=llvm reason=no-demand");
                }
                return std::ptr::null_mut();
            }
            compile_with_llvm(ctx, 1, findex, may_block, Some(bead))
        }
        _ => std::ptr::null_mut(),
    };
    if code.is_null() {
        ctx.failed.fetch_add(1, Ordering::Relaxed);
    }
    code
}

/// Cranelift middle tier. Returns null when the function is outside the
/// lowerable subset or lowering declines — the caller falls back to LLVM.
pub(crate) fn compile_with_cranelift(
    ctx: &Arc<TieredSharedCtx>,
    findex: usize,
    bead: &Arc<Bead>,
) -> *mut () {
    use std::sync::atomic::Ordering;
    let Some(bytecode) = ctx.bytecode_ptr() else {
        return std::ptr::null_mut();
    };

    // Cheap static pre-flight before paying for a lowering attempt.
    let Some(func) = bytecode
        .functions
        .iter()
        .find(|f| f.findex as usize == findex)
    else {
        return std::ptr::null_mut();
    };
    // Signature checks only. The opcode gate cannot be the screen here any
    // more: this tier has two lowering paths, and the AIR one accepts
    // instructions the opcode subset refuses (address-taken registers, for
    // one). Screening on the opcode subset would refuse those before either
    // path was asked. What each path can take is decided by that path, in
    // `cranelift::air::lower_best`; a function both decline still reaches the
    // LLVM tier, because a declining Cranelift compile returns an error and
    // this returns null on it.
    if let Some(reason) = ash_core::cranelift::signature_reject_reason(bytecode, func) {
        if ctx.tier_log {
            eprintln!("[tier] decline findex={findex} tier=cranelift reason={reason}");
        }
        return std::ptr::null_mut();
    }

    let tier = {
        let mut slot = ctx.cranelift.lock().expect("cranelift mutex poisoned");
        if slot.is_none() {
            let built = (|| -> Result<Arc<CraneliftTier>> {
                let t0 = std::time::Instant::now();
                let backend = ash_core::cranelift::AshCraneliftBackend::new()?;
                // SAFETY: `bytecode` is the process-lifetime decoded bytecode
                // published by `enable_tiered`; the arrays are the shared
                // runtime tables that outlive every tier.
                let cl_ctx = unsafe {
                    ash_core::cranelift::CraneliftTierContext::new(
                        &backend,
                        bytecode,
                        ctx.arrays.globals_data as *mut c_void as *mut *mut c_void,
                        ctx.arrays.nglobals,
                        ctx.arrays.functions_ptrs as *mut c_void as *mut *mut c_void,
                        ctx.max_findex.load(Ordering::Acquire),
                        &ctx.arrays.c_types,
                    )?
                };
                if ctx.log_promotions {
                    eprintln!(
                        "[tiered] cranelift backend ready in {:.1}ms ({} native symbols registered)",
                        t0.elapsed().as_secs_f64() * 1e3,
                        backend.registered_symbols()
                    );
                }
                Ok(Arc::new(CraneliftTier {
                    backend,
                    ctx: cl_ctx,
                }))
            })();
            *slot = Some(match built {
                Ok(t) => Some(t),
                Err(e) => {
                    eprintln!("[tiered] cranelift tier unavailable: {e:#}");
                    None
                }
            });
        }
        match slot.as_ref().and_then(|o| o.as_ref()) {
            Some(t) => Arc::clone(t),
            None => return std::ptr::null_mut(),
        }
    };

    let t0 = std::time::Instant::now();
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        tier.backend.compile_findex(bead, &tier.ctx, findex)
    }));
    match result {
        Ok(Ok((addr, meta))) => {
            ctx.cranelift_promotions.fetch_add(1, Ordering::Relaxed);
            // A signature wider than the interpreter's fixed-arity ladder gets
            // a second, uniform-ABI entry compiled for it. Cranelift knows the
            // arity here, so it can emit the call the bridge cannot express.
            if meta.arg_kinds.len() > 8 {
                match ash_core::cranelift::codegen::compile_uniform_entry(
                    &tier.backend,
                    &tier.ctx,
                    bead,
                    findex,
                    addr,
                ) {
                    Ok(uniform) => {
                        ctx.uniform_entries
                            .lock()
                            .expect("uniform_entries mutex poisoned")
                            .insert(findex, uniform);
                        if ctx.tier_log {
                            eprintln!(
                                "[tier] uniform entry findex={findex} nargs={} addr={uniform:#x}",
                                meta.arg_kinds.len()
                            );
                        }
                    }
                    // Without one the bridge cannot call this function at all,
                    // so say why rather than failing later as an arity refusal.
                    Err(e) => eprintln!(
                        "[tiered] uniform entry declined findex={findex} nargs={}: {e:#}",
                        meta.arg_kinds.len()
                    ),
                }
            }
            ash_core::profile::register_jit_code(
                findex as u32,
                ash_core::profile::Tier::Cranelift,
                addr,
            );
            if ctx.tier_log {
                eprintln!(
                    "[tier] install findex={findex} name={} tier=cranelift addr={addr:#x} ops={} in {:.2}ms on {}",
                    ash_core::profile::static_name(findex as u32).unwrap_or("?"),
                    meta.num_ops,
                    t0.elapsed().as_secs_f64() * 1e3,
                    std::thread::current().name().unwrap_or("main"),
                );
            }
            // Publish through `functions_ptrs` too. The LLVM tier always did
            // (inside `install_function_address`); Cranelift installs never
            // wrote the slot, so even a DIRECT call from compiled code to a
            // Cranelift-compiled callee took the stub bridge through the
            // interpreter. The entry ABI mirrors `create_function_type`, so
            // the slot is callable by the same transmuted pointer either
            // tier would install.
            if ctx.arrays.functions_ptrs != 0 && findex < ctx.max_findex.load(Ordering::Acquire) {
                unsafe {
                    let ptrs = ctx.arrays.functions_ptrs as *mut *mut c_void;
                    *ptrs.add(findex) = addr as *mut c_void;
                }
            }
            patch_vtable_slots(ctx, findex, addr as *mut c_void);
            // The ordinary hybrid path learns OSR sites from interpreted
            // back-edges. Compiled-only JIT never has those frames, so use
            // the re-tier polls the AIR V2 Cranelift lowering placed in this
            // function. LLVM will compile matching entries and publish them
            // into these slots while the baseline keeps running.
            if ctx.compiled_only {
                let sites = ash_core::cranelift::retier_sites(findex);
                if !sites.is_empty() {
                    ctx.hot_loop_pcs
                        .lock()
                        .expect("hot_loop_pcs mutex poisoned")
                        .insert(findex, sites);
                }
            }
            // The re-tier exits a running frame climbs out through. The
            // ladder produces the LLVM code they point at; this only carves
            // the doors.
            if !ctx.compiled_only {
                let _staged = produce_cranelift_osr_entries(ctx, &tier, bead, findex);
            }
            addr as *mut ()
        }
        Ok(Err(e)) => {
            // Declines are routine and stay quiet; invalid IR is a compiler
            // bug and never is. (backend.rs already printed the detail; this
            // is the findex-level trail in case that print is ever gated.)
            let msg = format!("{e:#}");
            if ctx.tier_log || msg.contains("Verifier") || msg.contains("Regalloc") {
                eprintln!("[tier] decline findex={findex} tier=cranelift reason={msg}");
            }
            std::ptr::null_mut()
        }
        Err(_) => {
            eprintln!("[tier] cranelift lowering panicked for findex={findex}");
            std::ptr::null_mut()
        }
    }
}

/// Resolve one cold worker-lane call without touching `HLInterpreter`.
/// Generated code retains the typed call ABI and invokes the returned pointer
/// directly, so this path only needs the immutable/shared tier context. It is
/// valid for compiled-only JIT and for hybrid thread bodies: the latter are
/// compiled at dispatch specifically so their interpreter frames never cross
/// OS-thread boundaries.
/// `may_block` is false when a null answer is acceptable -- an ordinary
/// guarded call site falls through to the interpreter. A fiber worker lane
/// cannot (`jit_stub_call_bridge` raises on an unprepared sentinel), and
/// neither can closure dependencies a native caller invokes directly.
pub(crate) fn resolve_worker_stub(
    ctx: &Arc<TieredSharedCtx>,
    findex: usize,
    may_block: bool,
) -> *mut () {
    if findex >= ctx.max_findex.load(std::sync::atomic::Ordering::Acquire) {
        return std::ptr::null_mut();
    }
    let Some(bytecode) = ctx.bytecode_ptr() else {
        return std::ptr::null_mut();
    };
    if !bytecode
        .functions
        .iter()
        .any(|function| function.findex as usize == findex)
    {
        return std::ptr::null_mut();
    }

    if std::env::var_os("ASH_DBG_STUB").is_some() {
        eprintln!("[stub] resolve findex={findex}");
    }
    // Already installed? Answer without the lock. The check used to sit INSIDE
    // the critical section, so a function that was long since compiled still
    // queued behind whatever compile happened to be running -- and
    // worker_compile_lock is global, so that is every stub resolution in the
    // process behind one compile.
    let published = |findex: usize| -> *mut () {
        if ctx.arrays.functions_ptrs == 0 {
            return std::ptr::null_mut();
        }
        let installed =
            unsafe { *(ctx.arrays.functions_ptrs as *const *mut c_void).add(findex) };
        if installed as usize >= ash_core::llvm::stub_bridge::STUB_SENTINEL_LIMIT as usize {
            installed.cast::<()>()
        } else {
            std::ptr::null_mut()
        }
    };
    let early = published(findex);
    if !early.is_null() {
        return if prepare_worker_closure_dependencies(ctx, findex) {
            early
        } else {
            std::ptr::null_mut()
        };
    }

    let code = {
        // Another thread may hold this across an ENTIRE compile, and this one
        // is regularly a fiber worker -- a registered mutator. Waiting on the
        // mutex is precisely what hl_blocking exists for: nothing here touches
        // a GC object, and the primitive publishes this thread's stack pointer
        // and callee-saved registers so a collection can run alongside.
        //
        // tiered_compile_tier below already declares the compile itself
        // blocking, but the WAIT in front of it was not covered, so a thread
        // queued behind someone else's compile reached no safepoint for as
        // long as that compile took. Measured on MBHaxe: two stragglers per
        // collection and world stops of 263ms under --preset game and 866ms
        // under --preset application, against 0.01ms and no stragglers at all
        // under --mode interp, where nothing compiles.
        //
        // `may_block` decides whether waiting is allowed at all. The contract
        // above says a false answer is acceptable at an ordinary guarded call
        // site, which falls through to the interpreter -- but the lock was
        // taken unconditionally, so such a caller waited out a compile it had
        // explicitly said it did not need. That is invisible while tier 0 is
        // Cranelift and a compile is ~1ms. Measured with LLVM at tier 0, where
        // compiles run 1.6s and one reached 47s, it is a multi-second freeze
        // of whichever thread happened to touch an unresolved stub.
        let guard = if may_block {
            unsafe { ash_core::hl_bindings::hl_blocking(true) };
            let g = ctx
                .worker_compile_lock
                .lock()
                .expect("worker compile mutex poisoned");
            // SAFETY: paired with the call above; the guard is held from here,
            // so this thread is running again and must be waited for as usual.
            unsafe { ash_core::hl_bindings::hl_blocking(false) };
            g
        } else {
            match ctx.worker_compile_lock.try_lock() {
                Ok(g) => g,
                // Someone is compiling. Say so rather than stalling: this
                // caller has an interpreter to fall through to, and the next
                // invocation will find the code published.
                Err(_) => return std::ptr::null_mut(),
            }
        };
        let _compile = guard;
        let installed = published(findex);
        if !installed.is_null() {
            installed
        } else {
            let bead = {
                let mut beads = ctx
                    .worker_beads
                    .lock()
                    .expect("worker beads mutex poisoned");
                Arc::clone(
                    beads
                        .entry(findex)
                        .or_insert_with(|| Bead::new(findex as beadie::CoreHandle, None)),
                )
            };
            if let Some(code) = bead.compiled() {
                code
            } else {
                let code = tiered_compile_tier(ctx, 0, findex, &bead, may_block);
                if code.is_null() {
                    return code;
                }
                if bead.eager_install(code) || bead.compiled().is_some() {
                    bead.compiled().unwrap_or(code)
                } else {
                    return std::ptr::null_mut();
                }
            }
        }
    };
    if prepare_worker_closure_dependencies(ctx, findex) {
        code
    } else {
        std::ptr::null_mut()
    }
}

/// Compile exact closure targets that may escape from `findex` into a native
/// caller. Guarded AIR V2 call sites can resolve a sentinel lazily; an HDLL
/// invokes `vclosure.fun` directly and therefore cannot. Scan optimized AIR
/// itself -- never serialize it back into legacy HashLink opcodes.
pub(crate) fn prepare_worker_closure_dependencies(
    ctx: &Arc<TieredSharedCtx>,
    findex: usize,
) -> bool {
    let current_thread = std::thread::current().id();
    {
        let mut states = ctx
            .worker_closure_deps
            .lock()
            .expect("worker closure dependency mutex poisoned");
        loop {
            match states.get(&findex) {
                Some(WorkerClosureDepsState::Ready) => return true,
                Some(WorkerClosureDepsState::Preparing(owner)) if *owner == current_thread => {
                    return true;
                }
                Some(WorkerClosureDepsState::Preparing(_)) => {
                    states = ctx
                        .worker_closure_deps_changed
                        .wait(states)
                        .expect("worker closure dependency mutex poisoned");
                }
                None => {
                    states.insert(findex, WorkerClosureDepsState::Preparing(current_thread));
                    break;
                }
            }
        }
    }

    let prepared = (|| {
        let bytecode = ctx.bytecode_ptr()?;
        let raw = bytecode
            .functions
            .iter()
            .find(|function| function.findex as usize == findex)?;
        let module = ash_core::air_pipeline::AshModule::new(bytecode);
        let optimized = ash_core::air_pipeline::optimized(&module, raw).ok()?;
        let mut targets = Vec::new();
        for block in &optimized.ir.blocks {
            for instr in &block.instrs {
                let target = match instr {
                    air::v2::Instr::StaticClosure { fun, .. }
                    | air::v2::Instr::InstanceClosure { fun, .. } => Some(*fun),
                    _ => None,
                };
                if let Some(target) = target {
                    let is_haxe_function = bytecode
                        .functions
                        .iter()
                        .any(|function| function.findex as usize == target);
                    if is_haxe_function && !targets.contains(&target) {
                        targets.push(target);
                    }
                }
            }
        }
        targets
            .into_iter()
            .all(|target| !resolve_worker_stub(ctx, target, true).is_null())
            .then_some(())
    })()
    .is_some();

    {
        let mut states = ctx
            .worker_closure_deps
            .lock()
            .expect("worker closure dependency mutex poisoned");
        if prepared {
            states.insert(findex, WorkerClosureDepsState::Ready);
        } else {
            states.remove(&findex);
        }
    }
    ctx.worker_closure_deps_changed.notify_all();
    prepared
}

/// Write a freshly installed code address into every materialized vtable
/// row that names `findex`.
///
/// Rows materialized LATER need no patching: `hl_get_obj_proto` copies from
/// `functions_ptrs`, which already holds the real address by then. Only the
/// copies made before promotion are stale, and this walks exactly those.
/// The store is a word write racing readers the same way `functions_ptrs`
/// updates already do.
pub(crate) fn patch_vtable_slots(ctx: &TieredSharedCtx, findex: usize, addr: *mut c_void) {
    let map = ctx.vtable_slots.get_or_init(|| {
        let mut m: HashMap<usize, Vec<(usize, usize)>> = HashMap::new();
        let Some(bytecode) = ctx.bytecode_ptr() else {
            return m;
        };
        for (tidx, t) in bytecode.types.iter().enumerate() {
            if t.kind != hl::hl_type_kind_HOBJ && t.kind != hl::hl_type_kind_HSTRUCT {
                continue;
            }
            // The full vtable: own protos plus everything inherited.
            // `pindex` is the absolute slot, so no offsetting is needed.
            //
            // An inherited proto must NOT claim a slot the child overrides.
            // This walk goes most-derived first, so the first claim of a slot
            // is the override and every later one is the base it replaced.
            // Registering those too meant compiling a base method wrote its
            // address into the CHILD's slot, clobbering the override: on
            // deltablue, promoting findex 41 -- a base predicate whose body
            // really is `return false` -- silently redirected every
            // overriding call to it, and the solver's total came out ~0.4%
            // low with a different wrong value each run, depending on which
            // functions happened to promote.
            let mut claimed: std::collections::HashSet<usize> = std::collections::HashSet::new();
            let mut cur = t.obj.as_ref();
            while let Some(o) = cur {
                for p in &o.proto {
                    if p.pindex >= 0 && claimed.insert(p.pindex as usize) {
                        m.entry(p.findex as usize)
                            .or_default()
                            .push((tidx, p.pindex as usize));
                    }
                }
                cur = o
                    .super_
                    .as_ref()
                    .and_then(|s| bytecode.types.get(s.0))
                    .and_then(|st| st.obj.as_ref());
            }
        }
        m
    });
    let Some(sites) = map.get(&findex) else {
        return;
    };
    for &(tidx, slot) in sites {
        let Some(&tp) = ctx.arrays.c_types.get(tidx) else {
            continue;
        };
        if tp == 0 {
            continue;
        }
        unsafe {
            let ot = tp as *mut hl_type;
            let vp = (*ot).vobj_proto;
            // Null: never materialized. 1: the no-proto sentinel.
            if vp as usize > 1 {
                *vp.add(slot) = addr;
            }
        }
    }
}

/// Compile CRANELIFT OSR entries for the hot loop headers probed in
/// `findex`, and stage them for the main thread to attach.
///
/// This is the ladder's fast door: a tier-0 compile is ~1ms, so a frame
/// stuck in a hot loop picks up native code milliseconds after the probe
/// notices it, instead of waiting ~170ms for an LLVM promote plus an
/// LLVM entry build. The tier-up to LLVM later replaces the whole table
/// through the ordinary swap, upgrading the loop's tail on the next
/// transfer opportunity of a future frame — the code the running frame
/// already entered stays valid regardless.
pub(crate) fn produce_cranelift_osr_entries(
    ctx: &TieredSharedCtx,
    tier: &CraneliftTier,
    bead: &Arc<Bead>,
    findex: usize,
) -> usize {
    if !osr_transfer_enabled() || !ash_core::air_pipeline::air_enabled() {
        return 0;
    }
    let pcs: Vec<usize> = match ctx
        .hot_loop_pcs
        .lock()
        .expect("hot_loop_pcs mutex poisoned")
        .get(&findex)
    {
        Some(v) if !v.is_empty() => v.clone(),
        _ => return 0,
    };
    let Some(bytecode) = ctx.bytecode_ptr() else {
        return 0;
    };
    let Some(raw) = bytecode
        .functions
        .iter()
        .find(|f| f.findex as usize == findex)
    else {
        return 0;
    };
    // The OSR body must be lowered exactly as the interpreter walks it: the
    // transfer is by position through `ser.block_pcs`.
    let cfg = ash_core::air_pipeline::interpreter_config_for(raw);
    let shared = tier.ctx.air_module();
    let bare;
    let osr_module = if cfg.callees_visible {
        shared
    } else {
        bare = shared.without_callees_view();
        &bare
    };
    let Ok(opt) = ash_core::air_pipeline::optimized_with_config(osr_module, raw, cfg)
    else {
        return 0;
    };
    let plan = ash_core::osr::analyze(&opt.ir);
    if !plan.eligible() {
        return 0;
    }
    let eligible: std::collections::HashSet<usize> = plan
        .entry_headers
        .iter()
        .filter_map(|&h| opt.ser.block_pcs.get(h as usize).copied())
        .collect();

    let mut entries: Vec<OsrEntry> = Vec::new();
    for pc in pcs {
        if !eligible.contains(&pc) {
            continue;
        }
        match ash_core::cranelift::codegen::compile_osr_entry(
            &tier.backend,
            &tier.ctx,
            bead,
            findex,
            &opt,
            pc,
        ) {
            Ok(addr) => {
                // Without this the entry's samples land in the profiler's
                // `unknown` bucket — 73% of a NUC mandelbrot run was an
                // unregistered OSR entry.
                ash_core::profile::register_jit_code(
                    findex as u32,
                    ash_core::profile::Tier::Cranelift,
                    addr,
                );
                entries.push(OsrEntry {
                    site: pc as u64,
                    code: addr as *mut (),
                })
            }
            Err(e) => {
                if osr_logging() {
                    eprintln!("[osr] cranelift entry declined findex={findex} pc={pc}: {e:#}");
                }
            }
        }
    }
    if entries.is_empty() {
        return 0;
    }
    if osr_logging() {
        eprintln!(
            "[osr] staged {} cranelift entr{} for findex={findex}",
            entries.len(),
            if entries.len() == 1 { "y" } else { "ies" }
        );
    }
    // Same staging map the LLVM producer uses; the fresh-install branch
    // attaches whatever is pending when it observes the new pointer.
    let staged = entries.len();
    ctx.pending_osr
        .lock()
        .expect("pending_osr mutex poisoned")
        .insert(findex, entries);
    staged
}

/// Compile an OSR entry for every hot loop header the interpreter has
/// probed in `findex`, and stage them for the main thread to attach.
///
/// Runs on the broker, right after this findex's LLVM promote — the point
/// the user-facing design names: the ladder runs interpreter → Cranelift →
/// LLVM on invocation counts (back-edge ticks included), and OSR is how a
/// frame already inside a loop picks the LLVM code up. Nothing here compiles
/// on the main thread and nothing bypasses a tier.
///
/// Silent when there is nothing to do: no probed headers, the plan refuses
/// the function, or the pipeline declined it. `ASH_OSR=0` disables the
/// production as well as the transfer.
/// The OSR work a promotion of `findex` should carry: the probed hot-loop
/// header pcs that are eligible entry sites, and the shared optimized AIR V2
/// graph whose de-SSA map those pcs and transfer slots describe.
/// `None` when nothing was probed, nothing is eligible, or OSR is off.
pub(crate) fn osr_plan_for(
    ctx: &TieredSharedCtx,
    findex: usize,
) -> Option<(Vec<usize>, Arc<ash_core::air_pipeline::Optimized>)> {
    if !osr_transfer_enabled() || !ash_core::air_pipeline::air_enabled() {
        return None;
    }
    let pcs: Vec<usize> = match ctx
        .hot_loop_pcs
        .lock()
        .expect("hot_loop_pcs mutex poisoned")
        .get(&findex)
    {
        Some(v) if !v.is_empty() => v.clone(),
        _ => return None,
    };
    let bytecode = ctx.bytecode_ptr()?;
    let raw = bytecode
        .functions
        .iter()
        .find(|f| f.findex as usize == findex)?;
    // Lowered as the interpreter walks it: OSR transfers by position.
    let cfg = ash_core::air_pipeline::interpreter_config_for(raw);
    let m = if cfg.callees_visible {
        ash_core::air_pipeline::AshModule::new(bytecode)
    } else {
        ash_core::air_pipeline::AshModule::new(bytecode).without_callees()
    };
    let optimized = ash_core::air_pipeline::optimized_with_config(&m, raw, cfg).ok()?;
    let plan = ash_core::osr::analyze(&optimized.ir);
    let eligible: std::collections::HashSet<usize> = plan
        .entry_headers
        .iter()
        .filter_map(|&h| optimized.ser.block_pcs.get(h as usize).copied())
        .collect();
    let sites: Vec<usize> = pcs.into_iter().filter(|pc| eligible.contains(pc)).collect();
    if !plan.eligible() || sites.is_empty() {
        return None;
    }
    Some((sites, optimized))
}

pub(crate) fn produce_osr_entries(ctx: &TieredSharedCtx, findex: usize) {
    let Some((sites, optimized)) = osr_plan_for(ctx, findex) else {
        return;
    };

    let mut guard = ctx.llvm.lock().expect("tiered llvm mutex poisoned");
    let LlvmState::Ready(module) = &mut *guard else {
        return;
    };
    let mut entries: Vec<OsrEntry> = Vec::with_capacity(sites.len());
    for pc in sites {
        match module.0.compile_osr_entry(findex, pc, &optimized) {
            Ok(addr) if addr != 0 => entries.push(OsrEntry {
                site: pc as u64,
                code: addr as *mut (),
            }),
            Ok(_) => {}
            Err(e) => {
                if osr_logging() {
                    eprintln!("[osr] entry compile failed findex={findex} pc={pc}: {e:#}");
                }
            }
        }
    }
    drop(guard);
    if entries.is_empty() {
        return;
    }
    // Publish into the Cranelift re-tier slots: any frame still looping in
    // tier-1 code takes the exit on its next iteration. The staging map
    // below serves interpreter frames the same way.
    for e in &entries {
        if ash_core::cranelift::publish_retier_target(findex, e.site as usize, e.code as u64)
            && osr_logging()
        {
            eprintln!("[osr] re-tier slot filled findex={findex} pc={}", e.site);
        }
    }
    if osr_logging() {
        eprintln!(
            "[osr] staged {} entr{} for findex={findex}",
            entries.len(),
            if entries.len() == 1 { "y" } else { "ies" }
        );
    }
    ctx.pending_osr
        .lock()
        .expect("pending_osr mutex poisoned")
        .insert(findex, entries);
}

/// LLVM top tier — the pre-existing `promote_function_strict` path.
///
/// Module init must NEVER run here mid-program: it GC-allocates (constants,
/// obj runtimes, enum marks), and a broker-side collection scans the wrong
/// stack — reclaiming objects live only in main-thread frames/registers —
/// while holding the GC lock across the multi-second init stalls the main
/// thread's next allocation. Only compilation and MCJIT finalization run here.
///
/// The `llvm` mutex is held across the whole armed region, so the single
/// global tiered recovery slot stays owned by one thread at a time even
/// though two broker threads can reach this function.
/// `may_block` is false on the mutator. A null return there is a handled
/// outcome -- the guarded call site falls through to its interpreter path --
/// so the mutator declines to queue behind a broker's compile rather than
/// parking for the length of one. The comment below already recorded what
/// that parking costs; it just had no way to opt out.
/// `bead` is consulted after the module lock is taken, not only before it.
/// Promotions queue behind one mutex and a single compile can hold it for
/// seconds, so by the time a waiter is served its function may have been
/// invalidated, blacklisted, or replaced. Compiling it then produces code
/// nothing will call while every promotion behind it keeps waiting.
pub(crate) fn compile_with_llvm(
    ctx: &TieredSharedCtx,
    tier: usize,
    findex: usize,
    may_block: bool,
    bead: Option<&Arc<Bead>>,
) -> *mut () {
    // A tier-0 failure permanently invalidates the bead (beadie's primary
    // broker); a tier-1 failure is silent and the bead keeps its current tier.
    let on_fail = if tier == 0 { "blacklist" } else { "keep-tier" };
    use std::sync::atomic::Ordering;
    let t0 = std::time::Instant::now();
    let mut guard = if may_block {
        ctx.llvm.lock().expect("tiered llvm mutex poisoned")
    } else {
        match ctx.llvm.try_lock() {
            Ok(g) => g,
            Err(std::sync::TryLockError::WouldBlock) => return std::ptr::null_mut(),
            Err(std::sync::TryLockError::Poisoned(e)) => e.into_inner(),
        }
    };
    // Every promotion serialises on this one mutex and holds it for the whole
    // compile — ~500ms on this machine. Checking the abandon flag only before
    // the lock is useless: by the time the program ends, the chases are
    // already queued ON it, and each still runs to completion before the next
    // sees the flag. deltablue answered at 228ms and exited at 4.5s, with the
    // main thread 97.7% blocked in __ulock_wait behind exactly this queue.
    //
    // Checking here, holding the lock, drains that queue at lock-handoff
    // speed. Safe for every caller: retier_chase_join runs after
    // execute_entrypoint has fully returned (the event loop included), so
    // nothing can call the code this would have produced.
    if retier_abandoned() {
        return std::ptr::null_mut();
    }
    // The same reasoning per function rather than per program: this waiter may
    // have sat behind a multi-second compile, and beadie's state is the
    // authority on whether its result is still wanted.
    if let Some(bead) = bead {
        if !bead.is_valid() || bead.is_blacklisted() {
            if ctx.tier_log {
                eprintln!(
                    "[tier] dropping queued tier-{tier} compile for findex={findex}: \
                     the bead is no longer {}",
                    if bead.is_blacklisted() {
                        "eligible"
                    } else {
                        "valid"
                    }
                );
            }
            return std::ptr::null_mut();
        }
    }
    // The OSR work this promotion should carry, computed before the compile
    // so its entries ride the promotion's own module -- one middle-end run,
    // one object emission -- instead of paying for a second module after.
    let osr_plan = if tier <= 1 {
        osr_plan_for(ctx, findex)
    } else {
        None
    };
    if let LlvmState::Pending(_) = &*guard {
        let LlvmState::Pending(pw) = std::mem::replace(&mut *guard, LlvmState::Unavailable) else {
            unreachable!()
        };
        // Move the module out of its Box into the shared slot.
        *guard = LlvmState::Ready(LlvmModule(unsafe { *Box::from_raw(pw.0) }));
        if ctx.log_promotions {
            eprintln!("[tiered] broker adopted pre-warmed JIT module");
        }
    }
    let LlvmState::Ready(module) = &mut *guard else {
        if ctx.log_promotions {
            eprintln!("[tiered] {on_fail} findex={findex} reason=no pre-warmed JIT module");
        }
        return std::ptr::null_mut();
    };
    let module = &mut module.0;

    // Arm a broker-local recovery point: a hardware fault during compilation
    // (e.g., a torn read of a type pointer the main thread is still
    // initializing) longjmps back HERE — on this thread — and blacklists the
    // findex, instead of crashing the process or, worse, being misrouted into
    // the main thread's armed recovery context.
    let compile_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        if unsafe { crate::native_recovery::arm_tiered_recovery() } != 0 {
            // Drop the queue so the faulting transitive compile is not
            // re-popped (and re-faulted) by every subsequent job.
            module.clear_pending_compilations();
            return Err(anyhow!(
                "native fault during promotion (sig={} fault_addr={:#x})",
                crate::native_recovery::last_tiered_recovery_signal(),
                crate::native_recovery::last_tiered_recovery_fault_addr()
            ));
        }
        // The entry first, then the function.
        //
        // A frame already transferred into tier-0 code can only leave through
        // an OSR entry -- the promoted body is for FUTURE calls, and a loop
        // owner like `main` is called once, so for the frame that is running
        // right now the promoted body is worth nothing and the entry is worth
        // everything. Building the entry into its own small module publishes
        // it in a fraction of the promote's time; measured on method_call,
        // the frame waited 43ms for the whole promote when the entry alone
        // was ready at ~15ms, and it ran the middle tier for every one of
        // those iterations.
        if let Some((sites, optimized)) = osr_plan.as_ref() {
            let mut entries: Vec<OsrEntry> = Vec::new();
            for &pc in sites.iter() {
                match module.compile_osr_entry(findex, pc, optimized) {
                    Ok(addr) if addr != 0 => entries.push(OsrEntry {
                        site: pc as u64,
                        code: addr as *mut (),
                    }),
                    Ok(_) => {}
                    Err(e) => {
                        if osr_logging() {
                            eprintln!(
                                "[osr] LLVM AIR entry declined findex={findex} pc={pc}: {e:#}"
                            );
                        }
                    }
                }
            }
            for e in &entries {
                if ash_core::cranelift::publish_retier_target(
                    findex,
                    e.site as usize,
                    e.code as u64,
                ) && osr_logging()
                {
                    eprintln!(
                        "[osr] re-tier slot filled early findex={findex} pc={}",
                        e.site
                    );
                }
            }
            if !entries.is_empty() && !ctx.compiled_only {
                if osr_logging() {
                    eprintln!(
                        "[osr] staged {} entr{} for findex={findex} ahead of its promote",
                        entries.len(),
                        if entries.len() == 1 { "y" } else { "ies" }
                    );
                }
                // Interpreter frames read the staging map on the next install
                // they observe, the same way they do for a late entry.
                ctx.pending_osr
                    .lock()
                    .expect("pending_osr mutex poisoned")
                    .insert(findex, entries);
            }
        }
        module.promote_function_strict(findex)
    }));
    crate::native_recovery::disarm_tiered_recovery();
    let result: std::result::Result<CompiledFunctionMeta, String> = match compile_result {
        Ok(Ok(meta)) if meta.fn_addr != 0 => Ok(meta),
        Ok(Ok(_)) => Err("promotion returned null fn_addr".to_string()),
        Ok(Err(e)) => Err(e.to_string()),
        Err(_) => Err("promotion panicked".to_string()),
    };
    drop(guard);

    match result {
        Ok(meta) => {
            ctx.llvm_done
                .lock()
                .expect("llvm_done mutex poisoned")
                .insert(findex);
            ctx.llvm_promotions.fetch_add(1, Ordering::Relaxed);
            patch_vtable_slots(ctx, findex, meta.fn_addr as *mut c_void);
            // Headers probed only after the early build, or one that
            // declined: the standalone producer covers whatever is left.
            if !ctx.compiled_only {
                produce_osr_entries(ctx, findex);
            }
            // (LLVM code registers itself with the profiler in
            // install_function_address, which every promotion passes through.)
            if ctx.tier_log {
                eprintln!(
                    "[tier] install findex={findex} name={} tier=llvm addr={:#x} in {:.2}ms",
                    ash_core::profile::static_name(findex as u32).unwrap_or("?"),
                    meta.fn_addr,
                    t0.elapsed().as_secs_f64() * 1e3
                );
            }
            meta.fn_addr as *mut ()
        }
        Err(reason) => {
            // Same policy as the Cranelift sink: a verifier failure is an ash
            // codegen bug and must be visible without any logging flag.
            if ctx.log_promotions || reason.contains("did not verify") {
                eprintln!("[tiered] {on_fail} findex={findex} reason={reason}");
            }
            ctx.llvm_failed
                .lock()
                .expect("llvm_failed mutex poisoned")
                .insert(findex);
            std::ptr::null_mut()
        }
    }
}

/// Hybrid HashLink bytecode interpreter with JIT promotion support.
///
/// Executes HL bytecode directly using a register-based architecture
/// with NaN-boxed values. Tracks per-function call counts and signals
/// when a function should be promoted to JIT compilation.
/// Cache an env-var presence check (these gate the opcode-dispatch and
/// native-call hot paths, where macOS getenv takes a process-wide lock —
/// `__findenv_locked` was 51.9% of samples on an nbody profile).
/// Whether to take an OSR entry when one is installed (`ASH_OSR=0` to disable).
pub(crate) fn osr_transfer_enabled() -> bool {
    static CELL: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *CELL.get_or_init(|| !matches!(std::env::var("ASH_OSR").as_deref(), Ok("0") | Ok("off")))
}

/// Whether to report OSR decisions (`ASH_OSR_LOG`).
/// Whether a header that turns hot AFTER its function's promote may stall the
/// mutator for an LLVM entry (`ASH_LATE_LLVM_OSR=1`). Off by default: see the
/// use site for the measurement.
pub(crate) fn late_llvm_osr_enabled() -> bool {
    static CELL: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *CELL.get_or_init(|| {
        matches!(
            std::env::var("ASH_LATE_LLVM_OSR").as_deref(),
            Ok("1") | Ok("on")
        )
    })
}

pub(crate) fn osr_logging() -> bool {
    static CELL: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *CELL.get_or_init(|| std::env::var("ASH_OSR_LOG").is_ok_and(|v| v != "0" && !v.is_empty()))
}

macro_rules! env_flag {
    ($name:literal) => {{
        static CELL: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
        *CELL.get_or_init(|| std::env::var($name).is_ok())
    }};
}

/// Zero-alloc "hlp_<name>" display for native-call diagnostics.
pub(crate) use env_flag;
