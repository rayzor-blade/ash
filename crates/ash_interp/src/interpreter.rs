use anyhow::{anyhow, Result};
use std::collections::{HashMap, HashSet};
use std::ffi::c_void;
use std::ffi::CStr;
use std::mem::ManuallyDrop;
use std::path::Path;
use std::sync::{Arc, Mutex, OnceLock};

use beadie::{Bead, HotnessPolicy, OsrEntry, ThresholdPolicy, TieredAdapter, TieredBound};

use ash::bytecode::DecodedBytecode;
use ash::c_types::CTypeFactory;
use ash::hl_bindings::{self as hl, _vclosure, hl_runtime_obj, hl_type, hl_type_kind_HSTRUCT};
use ash::jit::module::{CompiledFunctionMeta, JITModule, SharedRuntimeHandles};
use ash::native_lib::NativeFunctionResolver;
use ash::opcodes::{Opcode, Reg};
use ash::types::{HLFunction, ValueTypeKind};
use inkwell::context::Context;

use crate::air::Cache as AirCache;
use crate::frame::InterpreterFrame;
use crate::ssa::Cache as SsaCache;
use crate::values::{CmpOp, FloatBinOp, IntBinOp, NanBoxedValue};

/// Function pointer types for stdlib functions resolved at runtime.
type FnAllocObj = unsafe extern "C" fn(*mut c_void) -> *mut c_void;
type FnAllocDynObj = unsafe extern "C" fn() -> *mut c_void;
type FnAllocVirtual = unsafe extern "C" fn(*mut c_void) -> *mut c_void;
type FnGetObjRt = unsafe extern "C" fn(*mut c_void) -> *mut c_void;
type FnAllocClosureVoid = unsafe extern "C" fn(*mut c_void, *mut c_void) -> *mut c_void;
type FnDynGetD = unsafe extern "C" fn(*mut c_void, i32) -> f64;
type FnDynGetF = unsafe extern "C" fn(*mut c_void, i32) -> f32;
type FnDynGetI64 = unsafe extern "C" fn(*mut c_void, i32) -> i64;
type FnDynGetI = unsafe extern "C" fn(*mut c_void, i32, *mut c_void) -> i32;
type FnDynGetP = unsafe extern "C" fn(*mut c_void, i32, *mut c_void) -> *mut c_void;
type FnDynSetD = unsafe extern "C" fn(*mut c_void, i32, f64);
type FnDynSetF = unsafe extern "C" fn(*mut c_void, i32, f32);
type FnDynSetI64 = unsafe extern "C" fn(*mut c_void, i32, i64);
type FnDynSetI = unsafe extern "C" fn(*mut c_void, i32, *mut c_void, i32);
type FnDynSetP = unsafe extern "C" fn(*mut c_void, i32, *mut c_void, *mut c_void);
type FnHashGen = unsafe extern "C" fn(*const u16, bool) -> i32;
type FnObjGetField = unsafe extern "C" fn(*mut hl::vdynamic, i32) -> *mut hl::vdynamic;
type FnValueToString = unsafe extern "C" fn(*mut hl::vdynamic, *mut i32) -> *const hl::vbyte;
type FnTypeName = unsafe extern "C" fn(*const hl::hl_type) -> *mut hl::vbyte;

/// Resolve/calculate a HashLink field hash from a bytecode string index.
/// Uses std's hlp_hash_gen when available so field names are cached for reflection/JSON.
fn hash_field_name(
    bytecode: &DecodedBytecode,
    str_idx: usize,
    fn_hash_gen: *mut c_void,
    utf16_cache: &mut HashMap<usize, Vec<u16>>,
    hash_cache: &mut HashMap<usize, i32>,
) -> Result<i32> {
    if let Some(&h) = hash_cache.get(&str_idx) {
        return Ok(h);
    }
    let utf16 = if let Some(cached) = utf16_cache.get(&str_idx) {
        cached.as_ptr()
    } else {
        let s = bytecode
            .strings
            .get(str_idx)
            .ok_or_else(|| anyhow!("Dyn field string out of bounds: {}", str_idx))?;
        let mut buf: Vec<u16> = s.encode_utf16().collect();
        buf.push(0);
        utf16_cache.insert(str_idx, buf);
        utf16_cache[&str_idx].as_ptr()
    };
    let h = if !fn_hash_gen.is_null() {
        let f: FnHashGen = unsafe { std::mem::transmute(fn_hash_gen) };
        unsafe { f(utf16, true) }
    } else {
        let slice = unsafe {
            let mut len = 0;
            while *utf16.add(len) != 0 {
                len += 1;
            }
            std::slice::from_raw_parts(utf16, len)
        };
        let mut h: i32 = 0;
        for c in slice {
            h = h.wrapping_mul(223).wrapping_add(*c as i32);
        }
        h.wrapping_rem(0x1FFFFF7B)
    };
    hash_cache.insert(str_idx, h);
    Ok(h)
}

#[inline]
unsafe fn call_setjmp_opaque(jmp_buf: *mut c_void) -> i32 {
    type SetJmpOpaque = unsafe extern "C" fn(*mut c_void) -> i32;
    let setjmp_fn: SetJmpOpaque = std::mem::transmute(hl::_setjmp as *const () as usize);
    setjmp_fn(jmp_buf)
}

/// Result of executing a single opcode.
enum StepResult {
    /// Continue to next opcode (pc already incremented)
    Continue,
    /// Jump to a relative offset from current pc
    Jump(i32),
    /// Jump to an absolute opcode index (used for exception handler entry)
    JumpAbs(usize),
    /// Return a value from the current function
    Return(NanBoxedValue),
    /// Call a function by findex, with arguments and destination register
    Call {
        findex: usize,
        args: Vec<NanBoxedValue>,
        dst: u32,
    },
}

/// What a findex resolves to, held in a dense table indexed by findex.
///
/// Functions and natives share one numbering, so one indexed load decides
/// both questions the call path used to ask two hash maps.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum CallTarget {
    /// No function or native carries this findex.
    Missing,
    /// Index into `bytecode.functions`.
    Func(u32),
    /// Index into `bytecode.natives`.
    Native(u32),
}

/// How many recycled buffers either pool keeps.
///
/// Pops and pushes are not balanced: the `Call0..CallN` arms take a buffer from
/// the pool, but `op_call_method` and `op_call_closure` build their own and the
/// trampoline reclaims those too, so a dispatch-heavy program returns more than
/// it borrows. Measured RSS does not move without this cap, because a real
/// program mixes the two, but nothing in the design bounds it -- the cap does,
/// and a buffer arriving at a full pool is simply dropped.
///
/// 64 is far past any sane call depth; the pool only has to cover the calls in
/// flight, not the calls ever made.
const POOL_CAP: usize = 64;

/// Index into `bytecode.functions` for a findex, if it names one.
///
/// Free functions rather than methods on purpose: `func_of(&self.targets, ..)` would
/// borrow all of `self`, and the call path holds a `&mut` frame from
/// `self.stack` across the lookup. Taking the table alone keeps the borrow
/// field-disjoint, which is what the two HashMap fields gave for free.
#[inline(always)]
fn func_of(targets: &[CallTarget], findex: usize) -> Option<usize> {
    match targets.get(findex) {
        Some(CallTarget::Func(i)) => Some(*i as usize),
        _ => None,
    }
}

/// Index into `bytecode.natives` for a findex, if it names one.
#[inline(always)]
fn native_of(targets: &[CallTarget], findex: usize) -> Option<usize> {
    match targets.get(findex) {
        Some(CallTarget::Native(i)) => Some(*i as usize),
        _ => None,
    }
}

/// Carries a thrown HL exception value up through the Rust call stack.
/// Distinguishable from other errors so callers can catch it via downcast.
#[derive(Debug, Clone)]
struct HLExceptionPropagation {
    value: NanBoxedValue,
    message: Option<String>,
}

impl std::fmt::Display for HLExceptionPropagation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(msg) = &self.message {
            write!(f, "HL exception: {}", msg)
        } else {
            write!(f, "HL exception: {:?}", self.value)
        }
    }
}

impl std::error::Error for HLExceptionPropagation {}

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

    fn name(self) -> &'static str {
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
    pub jit_threshold: u64,
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
            jit_threshold: 100,
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

/// Everything needed to call one promoted function, cached per findex.
///
/// `Copy`, deliberately. This is read on every invocation of every compiled
/// function — ~10M times in one nbody run — and an `Arc<[u32]>` here cost an
/// atomic increment and decrement per call for a value that never changes once
/// the function is compiled. Promotion already refuses anything above eight
/// arguments (`nargs > max_jit_args || nargs > 8`), so the kinds fit inline and
/// the steady-state dispatch path touches no refcount and no allocation.
#[derive(Debug, Clone, Copy)]
struct CompiledFunctionEntry {
    fn_addr: usize,
    arg_kinds: [u32; 8],
    nargs: u8,
    ret_kind: u32,
}

impl CompiledFunctionEntry {
    #[inline(always)]
    fn args(&self) -> &[u32] {
        &self.arg_kinds[..self.nargs as usize]
    }
}

/// The pre-warmed LLVM JIT module, owned by whichever broker thread first
/// claims it and then kept behind `TieredSharedCtx::llvm`.
///
/// `ManuallyDrop` because LLVM objects may throw foreign exceptions during
/// drop on some platforms — the module is intentionally leaked at exit (same
/// as the old worker's `std::mem::forget` on shutdown).
struct LlvmModule(ManuallyDrop<JITModule<'static>>);

// SAFETY: the module is only ever touched while `TieredSharedCtx::llvm` is
// locked, so exactly one thread dereferences it at a time — the same
// justification the old single-broker-thread hand-off relied on, now enforced
// by the mutex instead of by there being only one broker.
unsafe impl Send for LlvmModule {}

enum LlvmState {
    /// Handed off from the main thread's pre-warm, not yet claimed.
    Pending(PrewarmedJit),
    Ready(LlvmModule),
    /// Pre-warm failed; the LLVM tier is unavailable for this run.
    Unavailable,
}

/// Cranelift middle tier, built lazily on the broker thread at the first
/// promotion (~1 ms; deliberately not part of startup).
struct CraneliftTier {
    backend: ash::cranelift::AshCraneliftBackend,
    ctx: ash::cranelift::CraneliftTierContext,
}

/// Raw handles the Cranelift lowering needs; all process-lifetime shared
/// arrays, captured once in `enable_tiered`.
#[derive(Clone)]
struct SharedArrayHandles {
    globals_data: usize,
    nglobals: usize,
    functions_ptrs: usize,
    /// Runtime `hl_type*` per bytecode type index, copied out of
    /// `SharedRuntimeHandles::c_types` so the Cranelift tier can hand a type
    /// identity to an allocator without borrowing the interpreter's tables
    /// across threads.
    c_types: Vec<usize>,
}

/// State shared between the interpreter thread and beadie's tier brokers.
/// Compile closures capture this via `Arc`.
struct TieredSharedCtx {
    log_promotions: bool,
    /// `ASH_TIER_LOG=1` (or `--jit-log`): one line per installed function
    /// naming the findex and the tier that produced it.
    tier_log: bool,
    mode: TierMode,
    /// The LLVM top tier. Pre-warmed on the MAIN thread by `enable_tiered`,
    /// before any bytecode runs, because module init GC-allocates (constants,
    /// obj runtimes, enum marks) and a broker-side collection would scan the
    /// wrong stack. Only compilation happens here.
    llvm: Mutex<LlvmState>,
    /// The Cranelift middle tier. `None` until first use, `Some(None)` once
    /// construction has been tried and failed.
    cranelift: Mutex<Option<Option<Arc<CraneliftTier>>>>,
    arrays: SharedArrayHandles,
    /// Set on the first tiered invocation: the decoded bytecode, which lives
    /// for the whole process (owned by the CLI entry point).
    bytecode: std::sync::atomic::AtomicUsize,
    /// `max(findex) + 1`, matching the length of `functions_ptrs`.
    max_findex: std::sync::atomic::AtomicUsize,
    /// Findexes whose installed code already came from LLVM — a tier-1
    /// upgrade for those would recompile identical code.
    llvm_done: Mutex<HashSet<usize>>,
    /// Loop headers the interpreter has probed hot, `findex -> header pcs`,
    /// written by `note_hot_loop` on the main thread and read by the broker
    /// when an LLVM promote finishes. The pcs index the SAME opcode array the
    /// interpreter executes (`air::Cache::body`), which the broker mirrors
    /// through the shared `air_pipeline::optimized` cache — an entry compiled
    /// against a separately optimized copy would name a different
    /// instruction.
    hot_loop_pcs: Mutex<HashMap<usize, Vec<usize>>>,
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
    vtable_slots: OnceLock<HashMap<usize, Vec<(usize, usize)>>>,
    /// OSR entries an LLVM promote has compiled but not yet attached,
    /// `findex -> entries`. The broker cannot attach them itself: beadie's
    /// adapter installs the main code pointer only after the compile closure
    /// returns, and `swap_compiled_with_osr` before that install would have
    /// its table orphaned by the install's generation bump. The main thread
    /// attaches on next observing the fresh pointer in `tiered_on_invoke` —
    /// for a single-invocation hot loop that observation comes from the
    /// back-edge ticks, at most 64 iterations later.
    pending_osr: Mutex<HashMap<usize, Vec<OsrEntry>>>,
    attempted: std::sync::atomic::AtomicU64,
    failed: std::sync::atomic::AtomicU64,
    cranelift_promotions: std::sync::atomic::AtomicU64,
    llvm_promotions: std::sync::atomic::AtomicU64,
}

impl TieredSharedCtx {
    /// Publish the process-wide bytecode pointer (idempotent).
    fn set_bytecode(&self, bytecode: &DecodedBytecode) {
        use std::sync::atomic::Ordering;
        if self.bytecode.load(Ordering::Acquire) != 0 {
            return;
        }
        let max_findex = bytecode
            .functions
            .iter()
            .map(|f| f.findex as usize)
            .chain(bytecode.natives.iter().map(|n| n.findex as usize))
            .max()
            .map(|m| m + 1)
            .unwrap_or(0);
        self.max_findex.store(max_findex, Ordering::Release);
        self.bytecode
            .store(bytecode as *const _ as usize, Ordering::Release);
    }

    fn bytecode_ptr(&self) -> Option<&'static DecodedBytecode> {
        let p = self.bytecode.load(std::sync::atomic::Ordering::Acquire);
        if p == 0 {
            None
        } else {
            // SAFETY: published by `set_bytecode` from the interpreter's own
            // `&DecodedBytecode`, which the CLI keeps alive for the process.
            Some(unsafe { &*(p as *const DecodedBytecode) })
        }
    }
}

/// Raw pointer to the JIT module pre-warmed on the main thread, handed off to
/// beadie's broker threads. `Send` is sound the same way `SharedRuntimeHandles`
/// is: the main thread never touches the module again, and every consumer goes
/// through `TieredSharedCtx::llvm`.
struct PrewarmedJit(*mut ManuallyDrop<JITModule<'static>>);
unsafe impl Send for PrewarmedJit {}

/// Tiered promotion state built on beadie's `TieredAdapter`.
///
/// One `TieredBound` per tierable findex (registered lazily on first call);
/// beadie owns the hotness tick, the per-tier promotion CAS, and one
/// background compile thread per tier. The interpreter keeps only marshaling
/// metadata and stats.
struct TieredRuntime {
    config: TieredConfig,
    adapter: TieredAdapter,
    /// findex-indexed bounds. `None` = gate not yet run, or untierable.
    beads: Vec<Option<TieredBound>>,
    /// findex-indexed: whether the one-time registration gate has run.
    gate_checked: Vec<bool>,
    /// findex-indexed cache of marshaling metadata for compiled functions.
    entries: Vec<Option<CompiledFunctionEntry>>,
    /// findex-indexed marshaling signature, derived from the bytecode and
    /// therefore identical for every tier.
    sigs: Vec<Option<([u32; 8], u8, u32)>>,
    shared_ctx: Arc<TieredSharedCtx>,
    /// Interp-side counters; broker-side counters live in `shared_ctx`.
    stats: TieredStats,
}

/// Dispatch one compile job to the backend that owns `tier`.
///
/// Tier 0 in `Auto` mode tries Cranelift first and falls back to LLVM: a
/// Cranelift decline must never leave the bead with null code, because
/// beadie's primary broker treats a null tier-0 result as a permanent
/// invalidation and the function would then never reach the LLVM tier either.
fn tiered_compile_tier(
    ctx: &TieredSharedCtx,
    tier: usize,
    findex: usize,
    bead: &Arc<Bead>,
) -> *mut () {
    use std::sync::atomic::Ordering;
    ctx.attempted.fetch_add(1, Ordering::Relaxed);
    let code = match (ctx.mode, tier) {
        (TierMode::Cranelift, 0) => compile_with_cranelift(ctx, findex, bead),
        (TierMode::Llvm, 0) => compile_with_llvm(ctx, 0, findex),
        (TierMode::Auto, 0) => {
            let cl = compile_with_cranelift(ctx, findex, bead);
            if cl.is_null() {
                compile_with_llvm(ctx, 0, findex)
            } else {
                cl
            }
        }
        (TierMode::Auto, 1) => {
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
            compile_with_llvm(ctx, 1, findex)
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
fn compile_with_cranelift(ctx: &TieredSharedCtx, findex: usize, bead: &Arc<Bead>) -> *mut () {
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
    if let Some(reason) = ash::cranelift::signature_reject_reason(bytecode, func) {
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
                let backend = ash::cranelift::AshCraneliftBackend::new()?;
                // SAFETY: `bytecode` is the process-lifetime decoded bytecode
                // published by `set_bytecode`; the arrays are the shared
                // runtime tables that outlive every tier.
                let cl_ctx = unsafe {
                    ash::cranelift::CraneliftTierContext::new(
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
            ash::profile::register_jit_code(findex as u32, ash::profile::Tier::Cranelift, addr);
            if ctx.tier_log {
                eprintln!(
                    "[tier] install findex={findex} tier=cranelift addr={addr:#x} ops={} in {:.2}ms",
                    meta.num_ops,
                    t0.elapsed().as_secs_f64() * 1e3
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
            produce_cranelift_osr_entries(ctx, &tier, bead, findex);
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

/// Write a freshly installed code address into every materialized vtable
/// row that names `findex`.
///
/// Rows materialized LATER need no patching: `hl_get_obj_proto` copies from
/// `functions_ptrs`, which already holds the real address by then. Only the
/// copies made before promotion are stale, and this walks exactly those.
/// The store is a word write racing readers the same way `functions_ptrs`
/// updates already do.
fn patch_vtable_slots(ctx: &TieredSharedCtx, findex: usize, addr: *mut c_void) {
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
            let mut cur = t.obj.as_ref();
            while let Some(o) = cur {
                for p in &o.proto {
                    if p.pindex >= 0 {
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
fn produce_cranelift_osr_entries(
    ctx: &TieredSharedCtx,
    tier: &CraneliftTier,
    bead: &Arc<Bead>,
    findex: usize,
) {
    if !osr_transfer_enabled() || !ash::air_pipeline::air_enabled() {
        return;
    }
    let pcs: Vec<usize> = match ctx
        .hot_loop_pcs
        .lock()
        .expect("hot_loop_pcs mutex poisoned")
        .get(&findex)
    {
        Some(v) if !v.is_empty() => v.clone(),
        _ => return,
    };
    let Some(bytecode) = ctx.bytecode_ptr() else {
        return;
    };
    let Some(raw) = bytecode
        .functions
        .iter()
        .find(|f| f.findex as usize == findex)
    else {
        return;
    };
    let Ok(opt) = ash::air_pipeline::optimized(tier.ctx.air_module(), raw) else {
        return;
    };
    let plan = ash::osr::analyze(&opt.ir);
    if !plan.eligible() {
        return;
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
        match ash::cranelift::codegen::compile_osr_entry(
            &tier.backend,
            &tier.ctx,
            bead,
            findex,
            &opt,
            pc,
        ) {
            Ok(addr) => entries.push(OsrEntry {
                site: pc as u64,
                code: addr as *mut (),
            }),
            Err(e) => {
                if osr_logging() {
                    eprintln!(
                        "[osr] cranelift entry declined findex={findex} pc={pc}: {e:#}"
                    );
                }
            }
        }
    }
    if entries.is_empty() {
        return;
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
    ctx.pending_osr
        .lock()
        .expect("pending_osr mutex poisoned")
        .insert(findex, entries);
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
fn produce_osr_entries(ctx: &TieredSharedCtx, findex: usize) {
    if !osr_transfer_enabled() {
        return;
    }
    // The gate: only functions something actually probed hot get entries.
    // (Also the sites themselves in the raw-opcode arm below.)
    let pcs: Vec<usize> = match ctx
        .hot_loop_pcs
        .lock()
        .expect("hot_loop_pcs mutex poisoned")
        .get(&findex)
    {
        Some(v) if !v.is_empty() => v.clone(),
        _ => return,
    };
    let Some(bytecode) = ctx.bytecode_ptr() else {
        return;
    };
    let Some(raw) = bytecode
        .functions
        .iter()
        .find(|f| f.findex as usize == findex)
    else {
        return;
    };

    // Eligibility and body must come from the same place the interpreter
    // reads: the shared `optimized` cache when AIR is on, the raw opcodes
    // when it is off. The serializer's `block_pcs` maps the plan's headers
    // to pcs, which is what lets a probed pc be validated as an eligible
    // header rather than gating on the whole function.
    let m = ash::air_pipeline::AshModule::new(bytecode);
    let (plan, body, sites): (_, std::borrow::Cow<HLFunction>, Vec<usize>) =
        if ash::air_pipeline::air_enabled() {
            match ash::air_pipeline::optimized(&m, raw) {
                Ok(opt) => {
                    let mut b = raw.clone();
                    b.ops = opt.ser.ops.clone();
                    b.regs = opt
                        .ser
                        .reg_types
                        .iter()
                        .map(|t| ash::types::TypeRef(t.0 as usize))
                        .collect();
                    // The pipeline renumbers opcodes; `debug` indices no
                    // longer line up (mirrors `air::Cache::prepare`).
                    b.debug = Vec::new();
                    let plan = ash::osr::analyze(&opt.ir);
                    // Only headers that have actually been probed hot. An
                    // entry duplicates the rest of the function, so building
                    // one per statically eligible header compiled ~20 bodies
                    // for nbody and the interpreter ran most of the loop
                    // before the attach landed. Headers that turn hot later
                    // get single entries on demand (`late_osr_entry`).
                    let eligible: std::collections::HashSet<usize> = plan
                        .entry_headers
                        .iter()
                        .filter_map(|&h| opt.ser.block_pcs.get(h as usize).copied())
                        .collect();
                    let sites = pcs
                        .iter()
                        .copied()
                        .filter(|pc| eligible.contains(pc))
                        .collect();
                    (plan, std::borrow::Cow::Owned(b), sites)
                }
                Err(_) => return,
            }
        } else {
            // Raw opcodes: the probe's pcs are already in the right
            // namespace, so use those. Headers probed after the promote are
            // missed in this mode; it is the diagnostic configuration, not
            // the shipping one.
            let opts = ash::air_pipeline::AirPassOptions::default();
            match ash::air_pipeline::prepare_ir(&m, raw, ash::air_pipeline::AirOptLevel::O0, &opts)
            {
                Ok((f, _)) => (ash::osr::analyze(&f), std::borrow::Cow::Borrowed(raw), pcs),
                Err(_) => return,
            }
        };
    if !plan.eligible() {
        if osr_logging() {
            eprintln!(
                "[osr] findex={findex} not entered: {:?}",
                plan.refusals
            );
        }
        return;
    }

    let mut guard = ctx.llvm.lock().expect("tiered llvm mutex poisoned");
    let LlvmState::Ready(module) = &mut *guard else {
        return;
    };
    let mut entries: Vec<OsrEntry> = Vec::with_capacity(sites.len());
    for pc in sites {
        match module.0.compile_osr_entry(findex, pc, &body) {
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
fn compile_with_llvm(ctx: &TieredSharedCtx, tier: usize, findex: usize) -> *mut () {
    // A tier-0 failure permanently invalidates the bead (beadie's primary
    // broker); a tier-1 failure is silent and the bead keeps its current tier.
    let on_fail = if tier == 0 { "blacklist" } else { "keep-tier" };
    use std::sync::atomic::Ordering;
    let t0 = std::time::Instant::now();
    let mut guard = ctx.llvm.lock().expect("tiered llvm mutex poisoned");
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
            produce_osr_entries(ctx, findex);
            // (LLVM code registers itself with the profiler in
            // install_function_address, which every promotion passes through.)
            if ctx.tier_log {
                eprintln!(
                    "[tier] install findex={findex} tier=llvm addr={:#x} in {:.2}ms",
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
fn osr_transfer_enabled() -> bool {
    static CELL: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *CELL.get_or_init(|| !matches!(std::env::var("ASH_OSR").as_deref(), Ok("0") | Ok("off")))
}

/// Whether to report OSR decisions (`ASH_OSR_LOG`).
fn osr_logging() -> bool {
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
struct HlpName<'a>(&'a str);
impl std::fmt::Display for HlpName<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "hlp_{}", self.0)
    }
}

pub struct HLInterpreter {
    /// Global variable store (indexed by global index)
    pub globals: Vec<NanBoxedValue>,
    /// Call stack
    stack: Vec<InterpreterFrame>,
    /// Maximum call stack depth
    max_stack_depth: usize,
    /// OSR entries currently attached per findex — the main thread's mirror
    /// of each bead's table. `swap_compiled_with_osr` REPLACES the table, so
    /// an incremental attach must resend the entries already installed; this
    /// is where they are remembered.
    osr_attached: std::collections::HashMap<usize, Vec<OsrEntry>>,
    /// Loop headers seen to be hot, as `(findex, header_pc)`.
    hot_loops: std::collections::HashSet<(usize, usize)>,

    /// Argument buffers, recycled the same way as [`Self::reg_pool`].
    ///
    /// `StepResult::Call` owns its arguments, so every call above arity zero
    /// used to malloc a `Vec` for them and free it on return. The trampoline
    /// hands the buffer back once the callee has copied the arguments into its
    /// own registers, which is the only span they need to survive.
    arg_pool: Vec<Vec<NanBoxedValue>>,

    /// Register buffers from finished frames, ready for the next call.
    ///
    /// Calls nest, so buffers come back in the order they were handed out and
    /// the depth of this stack is the depth of the call stack -- it cannot run
    /// away. Frames discarded by an exception unwind are reclaimed too; missing
    /// them would only cost a malloc, but the unwind path is where a long-lived
    /// program would otherwise leak its whole call depth of buffers.
    reg_pool: Vec<Vec<NanBoxedValue>>,

    /// findex → what to run, as a dense table.
    ///
    /// This was two `HashMap<usize, usize>`, consulted on every call: the
    /// interpreter paid a hash and a probe to answer a question about a dense
    /// integer. findexes number the functions and natives of one module
    /// consecutively, so an indexed load answers it instead. See
    /// [`CallTarget`].
    targets: Vec<CallTarget>,
    /// Hot-reloaded bytecode (replaces the original for function lookup).
    /// Leaked to 'static so it can be passed to interpret_loop without borrow conflicts.
    reloaded_bytecode: Option<&'static ash::bytecode::DecodedBytecode>,
    /// AIR v2 optimized bodies, filled on first execution of each function.
    /// Inert unless `ASH_AIR=v2-serialize`; see `crate::air`.
    air: AirCache,
    /// AIR v2 SSA bodies executed directly, filled on first execution of each
    /// function. Inert unless `ASH_AIR=v2`; see `crate::ssa`.
    ssa: SsaCache,
    /// Per-native resolved function pointer cache (indexed by native array
    /// index). Backed by the process-global symbol table on first miss;
    /// kills the per-call format!/table-lock on the native hot path.
    native_fn_cache: Vec<*mut c_void>,
    /// C-level type structures for native function interop
    c_type_factory: CTypeFactory,
    /// Resolved stdlib function pointer: hlp_alloc_obj
    fn_alloc_obj: *mut c_void,
    /// Resolved stdlib function pointer: hlp_get_obj_rt
    fn_get_obj_rt: *mut c_void,
    /// Resolved stdlib function pointer: hlp_make_dyn
    fn_make_dyn: *mut c_void,
    /// Resolved stdlib function pointer: hlp_alloc_enum
    fn_alloc_enum: *mut c_void,
    /// Resolved stdlib function pointer: hlp_alloc_dynobj
    fn_alloc_dynobj: *mut c_void,
    /// Resolved stdlib function pointer: hlp_alloc_virtual
    fn_alloc_virtual: *mut c_void,
    /// Resolved stdlib function pointer: hlp_alloc_closure_void
    fn_alloc_closure_void: *mut c_void,
    /// Resolved stdlib function pointer: hlp_alloc_closure_ptr (bound closures)
    fn_alloc_closure_ptr: *mut c_void,
    /// Resolved stdlib function pointer: hlp_dyn_getd
    fn_dyn_getd: *mut c_void,
    /// Resolved stdlib function pointer: hlp_dyn_getf
    fn_dyn_getf: *mut c_void,
    /// Resolved stdlib function pointer: hlp_dyn_geti64
    fn_dyn_geti64: *mut c_void,
    /// Resolved stdlib function pointer: hlp_dyn_geti
    fn_dyn_geti: *mut c_void,
    /// Resolved stdlib function pointer: hlp_dyn_getp
    fn_dyn_getp: *mut c_void,
    /// Resolved stdlib function pointer: hlp_dyn_castp (for SafeCast)
    fn_dyn_castp: *mut c_void,
    /// Resolved stdlib function pointer: hlp_dyn_setd
    fn_dyn_setd: *mut c_void,
    /// Resolved stdlib function pointer: hlp_dyn_setf
    fn_dyn_setf: *mut c_void,
    /// Resolved stdlib function pointer: hlp_dyn_seti64
    fn_dyn_seti64: *mut c_void,
    /// Resolved stdlib function pointer: hlp_dyn_seti
    fn_dyn_seti: *mut c_void,
    /// Resolved stdlib function pointer: hlp_dyn_setp
    fn_dyn_setp: *mut c_void,
    /// Resolved stdlib function pointer: hlp_hash_gen
    fn_hash_gen: *mut c_void,
    /// Resolved stdlib function pointer: hlp_setup_trap_jit (setjmp trap for native throws)
    fn_setup_trap_jit: *mut c_void,
    /// Resolved stdlib function pointer: hlp_remove_trap_jit (pop native trap after success)
    fn_remove_trap_jit: *mut c_void,
    /// Resolved stdlib function pointer: hlp_get_exc_value (interpreter exception recovery)
    fn_get_exc_value: *mut c_void,
    /// Resolved stdlib function pointer: hlp_clear_exc_value (clears exc_value after recovery)
    fn_clear_exc_value: *mut c_void,
    /// Resolved stdlib function pointer: hlp_obj_get_field (reads dynamic object fields)
    fn_obj_get_field: *mut c_void,
    /// Resolved stdlib function pointer: hlp_value_to_string (for readable exception messages)
    fn_value_to_string: *mut c_void,
    /// Resolved stdlib function pointer: hlp_type_name (runtime type name lookup)
    fn_type_name: *mut c_void,
    /// Resolved stdlib function pointer: hlp_gc_clear_scan_roots
    fn_gc_clear_scan_roots: *mut c_void,
    /// Resolved stdlib function pointer: hlp_gc_add_scan_root
    fn_gc_add_scan_root: *mut c_void,
    /// Resolved stdlib function pointer: hlp_gc_set_stack_top
    fn_gc_set_stack_top: *mut c_void,
    /// Resolved stdlib function pointer: hlp_gc_set_globals
    fn_gc_set_globals: *mut c_void,
    /// Whether GC globals/stack top were initialized for this interpreter.
    gc_runtime_initialized: bool,
    /// Scratch space for decoded raw pointer roots (from NaN-boxed registers).
    gc_root_ptrs: Vec<usize>,
    /// Cache of UTF-16 null-terminated strings (string index → owned buffer).
    /// HashLink uses UTF-16 internally; bytecode strings are stored as UTF-8 in Rust.
    utf16_strings: HashMap<usize, Vec<u16>>,
    /// Cache of field name hashes (string index → hash value).
    field_hash_cache: HashMap<usize, i32>,
    /// Fallback storage for HVIRTUAL fields when runtime virtual indexes are unavailable.
    virtual_fields: HashMap<(usize, usize), NanBoxedValue>,
    /// Hash-keyed fallback storage for HVIRTUAL dynamic field access via hl.Api/Reflect.
    virtual_hash_fields: HashMap<(usize, i32), NanBoxedValue>,
    /// Optional tiered runtime (hybrid mode).
    tiered_runtime: Option<TieredRuntime>,
    /// Saved event thread closure (findex, bound_value) from intercepted thread_create.
    /// Used for cooperative event dispatch: the event closure is called during lock_wait
    /// to pump SDL events on the main thread without actual threading.
    event_thread_closure: Option<(usize, Option<NanBoxedValue>)>,
    /// Whether we're currently inside the event thread closure dispatch
    /// (prevents recursive calls during lock_wait → event_closure → lock_wait).
    in_event_dispatch: bool,
}

impl Drop for HLInterpreter {
    fn drop(&mut self) {
        // Beadie's broker drains its whole queue on Drop (Shutdown is FIFO
        // behind any queued compile jobs). Invalidate every bead first so
        // still-queued jobs fail the mark_compiling gate and are skipped —
        // no LLVM compiles for a process that is exiting.
        if let Some(tiered) = self.tiered_runtime.as_mut() {
            for bound in tiered.beads.iter().flatten() {
                bound.bead().invalidate();
            }
        }
    }
}

impl HLInterpreter {
    pub fn new(bytecode: &DecodedBytecode, native_resolver: &NativeFunctionResolver) -> Self {
        // Build the dense findex table. Sized to the largest findex actually
        // seen rather than to functions.len() + natives.len(): the two share
        // one numbering, and nothing guarantees it has no gaps.
        let max_findex = bytecode
            .functions
            .iter()
            .map(|f| f.findex)
            .chain(bytecode.natives.iter().map(|n| n.findex))
            .max()
            .unwrap_or(-1);
        let mut targets = vec![CallTarget::Missing; (max_findex + 1).max(0) as usize];
        for (i, f) in bytecode.functions.iter().enumerate() {
            targets[f.findex as usize] = CallTarget::Func(i as u32);
        }
        for (i, n) in bytecode.natives.iter().enumerate() {
            targets[n.findex as usize] = CallTarget::Native(i as u32);
        }

        // Initialize globals
        let globals = vec![NanBoxedValue::null(); bytecode.globals.len()];

        // Create C-level type structures for native interop
        let c_type_factory = CTypeFactory::new(bytecode);

        // Resolve internal stdlib function pointers for object operations
        let fn_alloc_obj = native_resolver
            .resolve_function("std", "hlp_alloc_obj")
            .unwrap_or(std::ptr::null_mut());
        let fn_get_obj_rt = native_resolver
            .resolve_function("std", "hlp_get_obj_rt")
            .unwrap_or(std::ptr::null_mut());
        let fn_make_dyn = native_resolver
            .resolve_function("std", "hlp_make_dyn")
            .unwrap_or(std::ptr::null_mut());
        let fn_alloc_enum = native_resolver
            .resolve_function("std", "hlp_alloc_enum")
            .unwrap_or(std::ptr::null_mut());
        let fn_alloc_dynobj = native_resolver
            .resolve_function("std", "hlp_alloc_dynobj")
            .unwrap_or(std::ptr::null_mut());
        let fn_alloc_virtual = native_resolver
            .resolve_function("std", "hlp_alloc_virtual")
            .unwrap_or(std::ptr::null_mut());
        let fn_alloc_closure_void = native_resolver
            .resolve_function("std", "hlp_alloc_closure_void")
            .unwrap_or(std::ptr::null_mut());
        let fn_alloc_closure_ptr = native_resolver
            .resolve_function("std", "hlp_alloc_closure_ptr")
            .unwrap_or(std::ptr::null_mut());
        let fn_dyn_getd = native_resolver
            .resolve_function("std", "hlp_dyn_getd")
            .unwrap_or(std::ptr::null_mut());
        let fn_dyn_getf = native_resolver
            .resolve_function("std", "hlp_dyn_getf")
            .unwrap_or(std::ptr::null_mut());
        let fn_dyn_geti64 = native_resolver
            .resolve_function("std", "hlp_dyn_geti64")
            .unwrap_or(std::ptr::null_mut());
        let fn_dyn_geti = native_resolver
            .resolve_function("std", "hlp_dyn_geti")
            .unwrap_or(std::ptr::null_mut());
        let fn_dyn_getp = native_resolver
            .resolve_function("std", "hlp_dyn_getp")
            .unwrap_or(std::ptr::null_mut());
        let fn_dyn_castp = native_resolver
            .resolve_function("std", "hlp_dyn_castp")
            .unwrap_or(std::ptr::null_mut());
        let fn_dyn_setd = native_resolver
            .resolve_function("std", "hlp_dyn_setd")
            .unwrap_or(std::ptr::null_mut());
        let fn_dyn_setf = native_resolver
            .resolve_function("std", "hlp_dyn_setf")
            .unwrap_or(std::ptr::null_mut());
        let fn_dyn_seti64 = native_resolver
            .resolve_function("std", "hlp_dyn_seti64")
            .unwrap_or(std::ptr::null_mut());
        let fn_dyn_seti = native_resolver
            .resolve_function("std", "hlp_dyn_seti")
            .unwrap_or(std::ptr::null_mut());
        let fn_dyn_setp = native_resolver
            .resolve_function("std", "hlp_dyn_setp")
            .unwrap_or(std::ptr::null_mut());
        let fn_hash_gen = native_resolver
            .resolve_function("std", "hlp_hash_gen")
            .unwrap_or(std::ptr::null_mut());
        let fn_setup_trap_jit = native_resolver
            .resolve_function("std", "hlp_setup_trap_jit")
            .unwrap_or(std::ptr::null_mut());
        let fn_remove_trap_jit = native_resolver
            .resolve_function("std", "hlp_remove_trap_jit")
            .unwrap_or(std::ptr::null_mut());
        let fn_get_exc_value = native_resolver
            .resolve_function("std", "hlp_get_exc_value")
            .unwrap_or(std::ptr::null_mut());
        let fn_clear_exc_value = native_resolver
            .resolve_function("std", "hlp_clear_exc_value")
            .unwrap_or(std::ptr::null_mut());
        let fn_obj_get_field = native_resolver
            .resolve_function("std", "hlp_obj_get_field")
            .unwrap_or(std::ptr::null_mut());
        let fn_value_to_string = native_resolver
            .resolve_function("std", "hlp_value_to_string")
            .unwrap_or(std::ptr::null_mut());
        let fn_type_name = native_resolver
            .resolve_function("std", "hlp_type_name")
            .unwrap_or(std::ptr::null_mut());
        let fn_gc_clear_scan_roots = native_resolver
            .resolve_function("std", "hlp_gc_clear_scan_roots")
            .unwrap_or(std::ptr::null_mut());
        let fn_gc_add_scan_root = native_resolver
            .resolve_function("std", "hlp_gc_add_scan_root")
            .unwrap_or(std::ptr::null_mut());
        let fn_gc_set_stack_top = native_resolver
            .resolve_function("std", "hlp_gc_set_stack_top")
            .unwrap_or(std::ptr::null_mut());
        let fn_gc_set_globals = native_resolver
            .resolve_function("std", "hlp_gc_set_globals")
            .unwrap_or(std::ptr::null_mut());
        HLInterpreter {
            globals,
            stack: Vec::with_capacity(64),
            max_stack_depth: 1000,
            targets,
            reg_pool: Vec::new(),
            arg_pool: Vec::new(),
            osr_attached: std::collections::HashMap::new(),
            hot_loops: std::collections::HashSet::new(),
            reloaded_bytecode: None,
            air: AirCache::default(),
            ssa: SsaCache::default(),
            native_fn_cache: vec![std::ptr::null_mut(); bytecode.natives.len()],
            c_type_factory,
            fn_alloc_obj,
            fn_get_obj_rt,
            fn_make_dyn,
            fn_alloc_enum,
            fn_alloc_dynobj,
            fn_alloc_virtual,
            fn_alloc_closure_void,
            fn_alloc_closure_ptr,
            fn_dyn_getd,
            fn_dyn_getf,
            fn_dyn_geti64,
            fn_dyn_geti,
            fn_dyn_getp,
            fn_dyn_castp,
            fn_dyn_setd,
            fn_dyn_setf,
            fn_dyn_seti64,
            fn_dyn_seti,
            fn_dyn_setp,
            fn_hash_gen,
            fn_setup_trap_jit,
            fn_remove_trap_jit,
            fn_get_exc_value,
            fn_clear_exc_value,
            fn_obj_get_field,
            fn_value_to_string,
            fn_type_name,
            fn_gc_clear_scan_roots,
            fn_gc_add_scan_root,
            fn_gc_set_stack_top,
            fn_gc_set_globals,
            gc_runtime_initialized: false,
            gc_root_ptrs: Vec::new(),
            utf16_strings: HashMap::new(),
            field_hash_cache: HashMap::new(),
            virtual_fields: HashMap::new(),
            virtual_hash_fields: HashMap::new(),
            tiered_runtime: None,
            event_thread_closure: None,
            in_event_dispatch: false,
        }
    }

    pub fn enable_tiered(
        &mut self,
        hl_path: &Path,
        _native_resolver: &NativeFunctionResolver,
        mut config: TieredConfig,
    ) -> Result<()> {
        if config.tier_mode == TierMode::Off {
            eprintln!("[tiered] disabled (--jit-tier=off)");
            self.tiered_runtime = None;
            return Ok(());
        }
        // Hot-reload swaps bytecode bodies underneath the interpreter; the
        // Cranelift tier lowers from a bytecode snapshot it pins for the run,
        // so it would keep executing stale code. The LLVM tier already has an
        // indirect-call rewrite for this.
        if config.hot_reload && config.tier_mode != TierMode::Llvm {
            eprintln!("[tiered] hot-reload active: forcing --jit-tier=llvm");
            config.tier_mode = TierMode::Llvm;
        }
        config.enabled = true;

        let log_promotions = config.log_promotions;
        let hot_reload = config.hot_reload;
        let hl_path = hl_path.to_path_buf();

        // Register the bytecode path for hot-reload mtime detection
        if hot_reload {
            match _native_resolver.resolve_function("std", "hlp_setup_reload_check") {
                Ok(setup_fn) => {
                    let path_str = hl_path.to_string_lossy();
                    let mut utf16: Vec<u16> = path_str.encode_utf16().collect();
                    utf16.push(0);
                    type FnSetup = unsafe extern "C" fn(*const u16);
                    let setup: FnSetup = unsafe { std::mem::transmute(setup_fn) };
                    unsafe { setup(utf16.as_ptr()) };
                    eprintln!(
                        "[hot-reload] registered bytecode path: {}",
                        hl_path.display()
                    );
                }
                Err(e) => {
                    eprintln!(
                        "[hot-reload] warning: could not register reload check: {}",
                        e
                    );
                }
            }
        }
        let (globals_data_ptr, nglobals) = self.c_type_factory.globals_data();
        let shared = SharedRuntimeHandles {
            globals_data_ptr,
            nglobals,
            c_types: self.c_type_factory.as_slice().to_vec(),
            module_ctx: self.c_type_factory.module_ctx(),
        };

        // Initialize the global reload context and register the callback
        if hot_reload {
            // Build the functions_ptrs snapshot from the module context.
            // Use bytecode function/native count to size the table.
            let old_bc = ash::bytecode::BytecodeDecoder::decode(&hl_path);
            let max_findex = old_bc.as_ref().map_or(0, |bc| {
                let max_fn = bc
                    .functions
                    .iter()
                    .map(|f| f.findex as usize)
                    .max()
                    .unwrap_or(0);
                let max_nat = bc
                    .natives
                    .iter()
                    .map(|n| n.findex as usize)
                    .max()
                    .unwrap_or(0);
                max_fn.max(max_nat) + 1
            });
            let fptrs: Vec<*mut std::ffi::c_void> = (0..max_findex)
                .map(|i| unsafe {
                    if !shared.module_ctx.is_null()
                        && !(*shared.module_ctx).functions_ptrs.is_null()
                    {
                        *(*shared.module_ctx).functions_ptrs.add(i)
                    } else {
                        std::ptr::null_mut()
                    }
                })
                .collect();

            if let Ok(old_bc) = old_bc {
                ash::reload::init_reload_context(
                    hl_path.to_path_buf(),
                    old_bc,
                    fptrs,
                    shared.clone(),
                );

                // Register the actual reload callback
                if let Ok(set_cb_fn) =
                    _native_resolver.resolve_function("std", "hlp_set_reload_callback")
                {
                    type FnSetCb = unsafe extern "C" fn(unsafe extern "C" fn(*const u16) -> bool);
                    let set_cb: FnSetCb = unsafe { std::mem::transmute(set_cb_fn) };
                    unsafe { set_cb(ash::reload::reload_callback) };
                    eprintln!("[hot-reload] reload callback registered");
                }
            }
        }
        // Pre-warm the tiered JIT module ON THE MAIN THREAD, before any
        // bytecode runs. Module init GC-allocates (constants via
        // hlp_alloc_obj + hlp_gc_register_root, obj runtimes via
        // hlp_get_obj_rt, enum marks via hlp_init_enum) — doing it lazily on
        // the broker thread mid-program both froze the main thread (GC lock
        // held across the whole multi-second init) and let a broker-side
        // collection scan the wrong stack, reclaiming objects live only in
        // main-thread frames. Compilation itself (pure LLVM + MCJIT
        // finalization) stays on the broker thread, same as before.
        // Startup narration, gated: stderr is compared against an oracle's by
        // the parity harness, and no oracle narrates ash's tiering.
        if config.log_promotions {
            eprintln!("[tiered] pre-warming JIT module on main thread (one-time startup cost)...");
        }
        let prewarm_start = std::time::Instant::now();
        let prewarmed = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let context: &'static Context = Box::leak(Box::new(Context::create()));
            let mut jit = JITModule::new_with_shared_runtime(context, &hl_path, shared.clone());
            jit.set_hot_reload(hot_reload);
            Box::into_raw(Box::new(ManuallyDrop::new(jit)))
        })) {
            Ok(ptr) => {
                if config.log_promotions {
                    eprintln!(
                        "[tiered] JIT module ready in {:.2}s",
                        prewarm_start.elapsed().as_secs_f64()
                    );
                }
                Some(PrewarmedJit(ptr))
            }
            Err(_) => {
                eprintln!("[tiered] pre-warm panicked; tier promotion disabled");
                None
            }
        };

        // Beadie owns the per-tier hotness policies and one broker thread per
        // tier. Queue-ahead submits the tier-0 compile job slightly before the
        // threshold so code is ready by the time the function is truly hot;
        // the LLVM tier fires two orders of magnitude later, once a function
        // has proven worth the heavier compile.
        let threshold = u32::try_from(config.jit_threshold).unwrap_or(u32::MAX);
        let queue_ahead = (threshold / 5).max(1);
        let tier0: Box<dyn HotnessPolicy> =
            Box::new(ThresholdPolicy::new(threshold).queue_ahead(queue_ahead));
        let policies: Vec<Box<dyn HotnessPolicy>> = match config.tier_mode {
            TierMode::Auto => vec![
                tier0,
                Box::new(ThresholdPolicy::new(threshold.saturating_mul(100))),
            ],
            _ => vec![tier0],
        };
        let adapter = TieredAdapter::new(policies);
        if config.log_promotions {
            eprintln!(
            "[tiered] ladder: mode={} tier0={} {}",
            config.tier_mode.name(),
            threshold,
            match config.tier_mode {
                TierMode::Auto => format!("tier1={} (llvm)", threshold.saturating_mul(100)),
                _ => "single tier".to_string(),
            }
            );
        }

        let shared_ctx = Arc::new(TieredSharedCtx {
            log_promotions,
            tier_log: log_promotions || std::env::var("ASH_TIER_LOG").is_ok(),
            mode: config.tier_mode,
            llvm: Mutex::new(match prewarmed {
                Some(pw) => LlvmState::Pending(pw),
                None => LlvmState::Unavailable,
            }),
            cranelift: Mutex::new(None),
            arrays: SharedArrayHandles {
                globals_data: globals_data_ptr as usize,
                nglobals,
                c_types: shared.c_types.iter().map(|p| *p as usize).collect(),
                functions_ptrs: unsafe {
                    if shared.module_ctx.is_null() {
                        0
                    } else {
                        (*shared.module_ctx).functions_ptrs as usize
                    }
                },
            },
            bytecode: std::sync::atomic::AtomicUsize::new(0),
            max_findex: std::sync::atomic::AtomicUsize::new(0),
            llvm_done: Mutex::new(HashSet::new()),
            hot_loop_pcs: Mutex::new(HashMap::new()),
            vtable_slots: OnceLock::new(),
            pending_osr: Mutex::new(HashMap::new()),
            attempted: std::sync::atomic::AtomicU64::new(0),
            failed: std::sync::atomic::AtomicU64::new(0),
            cranelift_promotions: std::sync::atomic::AtomicU64::new(0),
            llvm_promotions: std::sync::atomic::AtomicU64::new(0),
        });

        self.tiered_runtime = Some(TieredRuntime {
            config,
            adapter,
            beads: Vec::new(),
            gate_checked: Vec::new(),
            entries: Vec::new(),
            sigs: Vec::new(),
            shared_ctx,
            stats: TieredStats::default(),
        });
        Ok(())
    }

    /// Check the compile-time layout oracle against the runtime's own answers.
    ///
    /// [`ash::layout`] exists so field access can become a constant-offset load
    /// instead of a call, which is only safe if it reproduces
    /// `hlp_get_obj_rt` exactly — a disagreement is silent memory corruption
    /// rather than a wrong answer. This forces every `HOBJ`/`HSTRUCT` runtime
    /// object to be built and compares all of them.
    ///
    /// Run via `ASH_VERIFY_LAYOUT=1`. Not on by default: building every runtime
    /// object costs startup time and allocates for types the program may never
    /// touch.
    pub fn verify_layout_oracle(
        &self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
    ) -> Result<Vec<ash::layout::LayoutMismatch>> {
        type GetObjRt = unsafe extern "C" fn(
            *mut ash::hl_bindings::hl_type,
        ) -> *mut ash::hl_bindings::hl_runtime_obj;
        let addr = native_resolver
            .resolve_function("std", "hlp_get_obj_rt")
            .map_err(|e| anyhow!("cannot verify layout without hlp_get_obj_rt: {e}"))?;
        let get_obj_rt: GetObjRt = unsafe { std::mem::transmute(addr) };

        Ok(unsafe {
            ash::layout::verify_against_runtime(&bytecode.types, |ti| {
                let t = self.c_type_factory.get(ti);
                if t.is_null() {
                    return None;
                }
                Some(get_obj_rt(t))
            })
        })
    }

    pub fn tiered_stats(&self) -> Option<TieredStats> {
        use std::sync::atomic::Ordering;
        self.tiered_runtime.as_ref().map(|t| {
            let mut stats = t.stats.clone();
            stats.attempted_promotions += t.shared_ctx.attempted.load(Ordering::Relaxed);
            stats.failed_promotions += t.shared_ctx.failed.load(Ordering::Relaxed);
            stats.cranelift_promotions = t.shared_ctx.cranelift_promotions.load(Ordering::Relaxed);
            stats.llvm_promotions = t.shared_ctx.llvm_promotions.load(Ordering::Relaxed);
            stats
        })
    }

    #[inline(always)]
    fn current_stack_addr() -> usize {
        // Portable stack probe: address of a local variable approximates current SP.
        let marker = 0u8;
        (&marker as *const u8) as usize
    }

    /// Get the base (highest address) of the current thread's stack.
    /// Used for GC conservative scanning — scans from stack base down to current SP.
    fn thread_stack_base() -> usize {
        #[cfg(target_os = "macos")]
        {
            unsafe {
                let thread = libc::pthread_self();
                let addr = libc::pthread_get_stackaddr_np(thread) as usize;
                if addr != 0 {
                    return addr;
                }
            }
        }
        #[cfg(target_os = "linux")]
        {
            unsafe {
                let mut attr: libc::pthread_attr_t = std::mem::zeroed();
                if libc::pthread_attr_init(&mut attr) == 0 {
                    let mut stack_addr: *mut libc::c_void = std::ptr::null_mut();
                    let mut stack_size: libc::size_t = 0;
                    if libc::pthread_attr_getstack(&attr, &mut stack_addr, &mut stack_size) == 0 {
                        libc::pthread_attr_destroy(&mut attr);
                        return stack_addr as usize + stack_size;
                    }
                    libc::pthread_attr_destroy(&mut attr);
                }
            }
        }
        // Fallback: use a high local address
        Self::current_stack_addr() + 8 * 1024 * 1024 // assume 8MB stack
    }

    fn ensure_gc_runtime_initialized(&mut self) {
        if self.gc_runtime_initialized {
            return;
        }
        if self.fn_gc_set_globals.is_null() || self.fn_gc_set_stack_top.is_null() {
            return;
        }
        let (globals_ptr, globals_len) = self.c_type_factory.globals_data();
        if globals_ptr.is_null() {
            return;
        }
        type FnSetGlobals = unsafe extern "C" fn(*const *mut c_void, usize);
        type FnSetStackTop = unsafe extern "C" fn(usize);
        let set_globals: FnSetGlobals = unsafe { std::mem::transmute(self.fn_gc_set_globals) };
        let set_stack_top: FnSetStackTop = unsafe { std::mem::transmute(self.fn_gc_set_stack_top) };
        unsafe {
            set_globals(globals_ptr as *const *mut c_void, globals_len);
            // Set stack top to the thread's stack base (highest address),
            // not the current frame. On ARM/x86 stacks grow downward,
            // so the GC scans from stack_top down to current SP.
            let stack_top = Self::thread_stack_base();
            set_stack_top(stack_top);
        }
        self.gc_runtime_initialized = true;
    }

    /// Publish interpreter register memory as conservative GC scan ranges.
    /// This keeps live values held in bytecode registers visible to the std GC.
    fn sync_gc_scan_roots(&mut self) {
        if self.fn_gc_clear_scan_roots.is_null() || self.fn_gc_add_scan_root.is_null() {
            return;
        }
        type FnClear = unsafe extern "C" fn();
        type FnAdd = unsafe extern "C" fn(*const c_void, usize);
        let clear: FnClear = unsafe { std::mem::transmute(self.fn_gc_clear_scan_roots) };
        let add: FnAdd = unsafe { std::mem::transmute(self.fn_gc_add_scan_root) };
        unsafe { clear() };
        self.gc_root_ptrs.clear();
        for frame in &self.stack {
            for v in frame.registers.as_slice() {
                if v.is_ptr() && !v.is_null() {
                    self.gc_root_ptrs.push(v.as_ptr());
                }
            }
        }
        if !self.gc_root_ptrs.is_empty() {
            let ptr = self.gc_root_ptrs.as_ptr() as *const c_void;
            let size = self.gc_root_ptrs.len() * std::mem::size_of::<usize>();
            unsafe { add(ptr, size) };
        }
    }

    fn decode_utf16_chars(ptr: *const u16, len: i32) -> String {
        if ptr.is_null() || len <= 0 {
            return String::new();
        }
        let slice = unsafe { std::slice::from_raw_parts(ptr, len as usize) };
        String::from_utf16_lossy(slice)
    }

    fn value_to_string(&self, dyn_ptr: *mut hl::vdynamic) -> Option<String> {
        if self.fn_value_to_string.is_null() || dyn_ptr.is_null() {
            return None;
        }
        let f: FnValueToString = unsafe { std::mem::transmute(self.fn_value_to_string) };
        let mut len: i32 = 0;
        let out = unsafe { f(dyn_ptr, &mut len as *mut i32) };
        if out.is_null() || len <= 0 {
            None
        } else {
            Some(Self::decode_utf16_chars(out as *const u16, len))
        }
    }

    fn hash_literal_name(&self, name: &str) -> i32 {
        let mut utf16: Vec<u16> = name.encode_utf16().collect();
        utf16.push(0);
        if !self.fn_hash_gen.is_null() {
            let f: FnHashGen = unsafe { std::mem::transmute(self.fn_hash_gen) };
            return unsafe { f(utf16.as_ptr(), true) };
        }
        let mut h: i32 = 0;
        for c in &utf16[..utf16.len() - 1] {
            h = h.wrapping_mul(223).wrapping_add(*c as i32);
        }
        h.wrapping_rem(0x1FFFFF7B)
    }

    fn resolve_typed_field_hash(
        bytecode: &DecodedBytecode,
        obj_type_idx: usize,
        field_idx: usize,
    ) -> Option<i32> {
        let ty = bytecode.types.get(obj_type_idx)?;
        if let Some(obj) = ty.obj.as_ref() {
            if let Some(f) = obj.fields.get(field_idx) {
                if f.hashed_name != 0 {
                    return Some(f.hashed_name);
                }
            }
        }
        if let Some(virt) = ty.virt.as_ref() {
            if let Some(f) = virt.fields.get(field_idx) {
                if f.hashed_name != 0 {
                    return Some(f.hashed_name);
                }
            }
        }
        None
    }

    unsafe fn resolve_virtual_field_offset(
        c_type_ptr: *mut c_void,
        field_idx: usize,
    ) -> Option<usize> {
        if c_type_ptr.is_null() {
            return None;
        }
        let t = c_type_ptr as *mut hl_type;
        if t.is_null() || (*t).kind != hl::hl_type_kind_HVIRTUAL {
            return None;
        }
        let virt = (*t).__bindgen_anon_1.virt;
        if virt.is_null() || (*virt).indexes.is_null() || field_idx >= (*virt).nfields as usize {
            return None;
        }
        let off = *(*virt).indexes.add(field_idx);
        if off < 0 {
            None
        } else {
            Some(off as usize)
        }
    }

    unsafe fn resolve_virtual_field_index_and_type(
        obj_ptr: *mut c_void,
        hfield: i32,
    ) -> Option<(usize, *mut hl_type)> {
        if obj_ptr.is_null() {
            return None;
        }
        let t = *(obj_ptr as *const *mut hl_type);
        if t.is_null() || (*t).kind != hl::hl_type_kind_HVIRTUAL {
            return None;
        }
        let virt = (*t).__bindgen_anon_1.virt;
        if virt.is_null() || (*virt).fields.is_null() {
            return None;
        }
        for i in 0..(*virt).nfields as usize {
            let f = &*(*virt).fields.add(i);
            if f.hashed_name == hfield {
                return Some((i, f.t));
            }
        }
        None
    }

    #[inline]
    fn is_primitive_or_bytes_kind(kind: u32) -> bool {
        matches!(
            kind,
            hl::hl_type_kind_HI32
                | hl::hl_type_kind_HUI8
                | hl::hl_type_kind_HUI16
                | hl::hl_type_kind_HI64
                | hl::hl_type_kind_HF32
                | hl::hl_type_kind_HF64
                | hl::hl_type_kind_HBOOL
        )
    }

    /// Kinds where a pointer return should be unboxed from vdynamic.
    /// HBYTES is excluded — bytes pointers are raw buffers, not boxed primitives.
    fn is_unboxable_primitive_kind(kind: u32) -> bool {
        matches!(
            kind,
            hl::hl_type_kind_HI32
                | hl::hl_type_kind_HUI8
                | hl::hl_type_kind_HUI16
                | hl::hl_type_kind_HI64
                | hl::hl_type_kind_HF32
                | hl::hl_type_kind_HF64
                | hl::hl_type_kind_HBOOL
        )
    }

    #[inline]
    fn is_numeric_or_bool_kind(kind: u32) -> bool {
        matches!(
            kind,
            hl::hl_type_kind_HI32
                | hl::hl_type_kind_HUI8
                | hl::hl_type_kind_HUI16
                | hl::hl_type_kind_HI64
                | hl::hl_type_kind_HF32
                | hl::hl_type_kind_HF64
                | hl::hl_type_kind_HBOOL
        )
    }

    fn box_value_as_dynamic_with_type(
        &self,
        val: NanBoxedValue,
        field_t: *mut hl_type,
    ) -> NanBoxedValue {
        if val.is_null() || val.is_void() {
            return NanBoxedValue::null();
        }
        if field_t.is_null() {
            return if val.is_ptr() {
                NanBoxedValue::from_ptr(val.as_ptr())
            } else {
                NanBoxedValue::null()
            };
        }
        let kind = unsafe { (*field_t).kind };
        if !Self::is_primitive_or_bytes_kind(kind) {
            return NanBoxedValue::from_ptr(val.as_ptr());
        }
        if self.fn_make_dyn.is_null() {
            return val;
        }
        let mut data: i64 = match kind {
            hl::hl_type_kind_HI32 | hl::hl_type_kind_HUI8 | hl::hl_type_kind_HUI16 => {
                val.as_i32() as i64
            }
            hl::hl_type_kind_HI64 => val.as_i64_lossy(),
            hl::hl_type_kind_HF32 | hl::hl_type_kind_HF64 => val.as_f64().to_bits() as i64,
            hl::hl_type_kind_HBOOL => {
                if val.as_bool() {
                    1
                } else {
                    0
                }
            }
            hl::hl_type_kind_HBYTES => val.as_ptr() as i64,
            _ => val.as_ptr() as i64,
        };
        let make_dyn: unsafe extern "C" fn(*mut c_void, *mut c_void) -> *mut c_void =
            unsafe { std::mem::transmute(self.fn_make_dyn) };
        let dyn_ptr =
            unsafe { make_dyn(&mut data as *mut i64 as *mut c_void, field_t as *mut c_void) };
        if dyn_ptr.is_null() {
            NanBoxedValue::null()
        } else {
            NanBoxedValue::from_ptr(dyn_ptr as usize)
        }
    }

    #[inline]
    fn coerce_value_for_static_kind(&self, val: NanBoxedValue, dst_kind: u32) -> NanBoxedValue {
        if val.is_ptr()
            && !val.is_null()
            && val.as_ptr() != 0
            && Self::is_unboxable_primitive_kind(dst_kind)
        {
            // Only attempt unboxing if the pointer looks like a valid vdynamic:
            // aligned, non-tiny address, and type pointer field also looks valid.
            let addr = val.as_ptr();
            if addr > 0x10000 && addr % std::mem::align_of::<usize>() == 0 {
                let d = addr as *const hl::vdynamic;
                let t = unsafe { (*d).t };
                if !t.is_null() && (t as usize) % std::mem::align_of::<usize>() == 0 {
                    let kind = unsafe { (*t).kind };
                    // Only unbox if the source type IS a boxed primitive
                    if Self::is_unboxable_primitive_kind(kind) {
                        return unsafe {
                            Self::unbox_dynamic_to_kind(addr as *mut hl::vdynamic, dst_kind)
                                .unwrap_or(val)
                        };
                    }
                }
            }
        }
        if val.is_null() {
            return match dst_kind {
                hl::hl_type_kind_HI32 | hl::hl_type_kind_HUI8 | hl::hl_type_kind_HUI16 => {
                    NanBoxedValue::from_i32(0)
                }
                hl::hl_type_kind_HI64 => NanBoxedValue::from_i64(0),
                hl::hl_type_kind_HF32 | hl::hl_type_kind_HF64 => NanBoxedValue::from_f64(0.0),
                hl::hl_type_kind_HBOOL => NanBoxedValue::from_bool(false),
                _ => val,
            };
        }
        val
    }

    fn try_handle_virtual_obj_get_field(
        &mut self,
        args: &[NanBoxedValue],
    ) -> Option<NanBoxedValue> {
        if args.len() < 2 {
            return Some(NanBoxedValue::null());
        }
        let obj = args[0];
        if obj.is_null() || obj.is_void() {
            return Some(NanBoxedValue::null());
        }
        let obj_ptr = obj.as_ptr();
        if obj_ptr == 0 {
            return Some(NanBoxedValue::null());
        }
        let hfield = args[1].as_i32();
        let meta =
            unsafe { Self::resolve_virtual_field_index_and_type(obj_ptr as *mut c_void, hfield) };
        if meta.is_none()
            && unsafe {
                let t = *(obj_ptr as *const *mut hl_type);
                t.is_null() || (*t).kind != hl::hl_type_kind_HVIRTUAL
            }
        {
            return None;
        }
        let found = self
            .virtual_fields
            .get(&(obj_ptr, meta.map_or(usize::MAX, |(idx, _)| idx)))
            .copied()
            .or_else(|| self.virtual_hash_fields.get(&(obj_ptr, hfield)).copied());
        if env_flag!("ASH_DBG_DYN") {
            eprintln!(
                "[VGET] self={:p} obj={obj_ptr:#x} hfield={hfield} meta={:?} found={found:?} maps={}/{}",
                self as *const _,
                meta.map(|(i, _)| i),
                self.virtual_fields.len(),
                self.virtual_hash_fields.len()
            );
        }
        match found {
            Some(val) if !val.is_null() && !val.is_void() => {
                if val.is_ptr() {
                    return Some(NanBoxedValue::from_ptr(val.as_ptr()));
                }
                let field_t = meta.map_or(std::ptr::null_mut(), |(_, ft)| ft);
                Some(self.box_value_as_dynamic_with_type(val, field_t))
            }
            _ => {
                // Field not in interpreter maps — fall through to native hlp_obj_get_field
                // which can read from the virtual object's backing memory.
                None
            }
        }
    }

    fn try_handle_virtual_obj_set_field(
        &mut self,
        args: &[NanBoxedValue],
    ) -> Option<NanBoxedValue> {
        if args.len() < 3 {
            return Some(NanBoxedValue::void());
        }
        let obj = args[0];
        if obj.is_null() || obj.is_void() {
            return Some(NanBoxedValue::void());
        }
        let obj_ptr = obj.as_ptr();
        if obj_ptr == 0 {
            return Some(NanBoxedValue::void());
        }
        let hfield = args[1].as_i32();
        let src_val = args[2];
        let meta =
            unsafe { Self::resolve_virtual_field_index_and_type(obj_ptr as *mut c_void, hfield) };
        if meta.is_none()
            && unsafe {
                let t = *(obj_ptr as *const *mut hl_type);
                t.is_null() || (*t).kind != hl::hl_type_kind_HVIRTUAL
            }
        {
            return None;
        }

        match meta {
            Some((idx, field_t)) => {
                let stored = if src_val.is_null() || src_val.is_void() {
                    NanBoxedValue::null()
                } else {
                    let kind = unsafe { (*field_t).kind };
                    if Self::is_primitive_or_bytes_kind(kind) && src_val.is_ptr() {
                        unsafe {
                            Self::unbox_dynamic_to_kind(src_val.as_ptr() as *mut hl::vdynamic, kind)
                                .unwrap_or(src_val)
                        }
                    } else {
                        src_val
                    }
                };
                self.virtual_fields.insert((obj_ptr, idx), stored);
                self.virtual_hash_fields.insert((obj_ptr, hfield), stored);
            }
            None => {
                self.virtual_hash_fields.insert((obj_ptr, hfield), src_val);
            }
        }
        if env_flag!("ASH_DBG_DYN") {
            eprintln!(
                "[VSET] self={:p} obj={obj_ptr:#x} hfield={hfield} meta={:?} maps={}/{}",
                self as *const _,
                meta.map(|(i, _)| i),
                self.virtual_fields.len(),
                self.virtual_hash_fields.len()
            );
        }
        Some(NanBoxedValue::void())
    }

    fn try_handle_virtual_obj_has_field(
        &mut self,
        args: &[NanBoxedValue],
    ) -> Option<NanBoxedValue> {
        if args.len() < 2 {
            return Some(NanBoxedValue::from_bool(false));
        }
        let obj = args[0];
        if obj.is_null() || obj.is_void() {
            return Some(NanBoxedValue::from_bool(false));
        }
        let obj_ptr = obj.as_ptr();
        if obj_ptr == 0 {
            return Some(NanBoxedValue::from_bool(false));
        }
        let hfield = args[1].as_i32();
        let meta =
            unsafe { Self::resolve_virtual_field_index_and_type(obj_ptr as *mut c_void, hfield) };
        if meta.is_none()
            && unsafe {
                let t = *(obj_ptr as *const *mut hl_type);
                t.is_null() || (*t).kind != hl::hl_type_kind_HVIRTUAL
            }
        {
            return None;
        }
        let found = match meta {
            Some((idx, _)) => {
                self.virtual_fields.contains_key(&(obj_ptr, idx))
                    || self.virtual_hash_fields.contains_key(&(obj_ptr, hfield))
            }
            None => self.virtual_hash_fields.contains_key(&(obj_ptr, hfield)),
        };
        Some(NanBoxedValue::from_bool(found))
    }

    fn try_handle_virtual_obj_delete_field(
        &mut self,
        args: &[NanBoxedValue],
    ) -> Option<NanBoxedValue> {
        if args.len() < 2 {
            return Some(NanBoxedValue::from_bool(false));
        }
        let obj = args[0];
        if obj.is_null() || obj.is_void() {
            return Some(NanBoxedValue::from_bool(false));
        }
        let obj_ptr = obj.as_ptr();
        if obj_ptr == 0 {
            return Some(NanBoxedValue::from_bool(false));
        }
        let hfield = args[1].as_i32();
        let meta =
            unsafe { Self::resolve_virtual_field_index_and_type(obj_ptr as *mut c_void, hfield) };
        if meta.is_none()
            && unsafe {
                let t = *(obj_ptr as *const *mut hl_type);
                t.is_null() || (*t).kind != hl::hl_type_kind_HVIRTUAL
            }
        {
            return None;
        }
        let removed = match meta {
            Some((idx, _)) => {
                self.virtual_fields.remove(&(obj_ptr, idx)).is_some()
                    || self
                        .virtual_hash_fields
                        .remove(&(obj_ptr, hfield))
                        .is_some()
            }
            None => self
                .virtual_hash_fields
                .remove(&(obj_ptr, hfield))
                .is_some(),
        };
        Some(NanBoxedValue::from_bool(removed))
    }

    #[allow(clippy::too_many_arguments)]
    fn dyn_get_field_by_hash(
        obj_ptr: *mut c_void,
        hfield: i32,
        dst_kind: u32,
        dst_type_ptr: *mut c_void,
        fn_dyn_getd: *mut c_void,
        fn_dyn_getf: *mut c_void,
        fn_dyn_geti64: *mut c_void,
        fn_dyn_geti: *mut c_void,
        fn_dyn_getp: *mut c_void,
    ) -> NanBoxedValue {
        match dst_kind {
            hl::hl_type_kind_HF64 => {
                if fn_dyn_getd.is_null() {
                    NanBoxedValue::null()
                } else {
                    let f: FnDynGetD = unsafe { std::mem::transmute(fn_dyn_getd) };
                    NanBoxedValue::from_f64(unsafe { f(obj_ptr, hfield) })
                }
            }
            hl::hl_type_kind_HF32 => {
                if fn_dyn_getf.is_null() {
                    NanBoxedValue::null()
                } else {
                    let f: FnDynGetF = unsafe { std::mem::transmute(fn_dyn_getf) };
                    NanBoxedValue::from_f64(unsafe { f(obj_ptr, hfield) as f64 })
                }
            }
            hl::hl_type_kind_HI64 => {
                if fn_dyn_geti64.is_null() {
                    NanBoxedValue::null()
                } else {
                    let f: FnDynGetI64 = unsafe { std::mem::transmute(fn_dyn_geti64) };
                    NanBoxedValue::from_i64(unsafe { f(obj_ptr, hfield) })
                }
            }
            hl::hl_type_kind_HI32
            | hl::hl_type_kind_HBOOL
            | hl::hl_type_kind_HUI8
            | hl::hl_type_kind_HUI16 => {
                if fn_dyn_geti.is_null() {
                    NanBoxedValue::null()
                } else {
                    let f: FnDynGetI = unsafe { std::mem::transmute(fn_dyn_geti) };
                    let i = unsafe { f(obj_ptr, hfield, dst_type_ptr) };
                    if dst_kind == hl::hl_type_kind_HBOOL {
                        NanBoxedValue::from_bool(i != 0)
                    } else {
                        NanBoxedValue::from_i32(i)
                    }
                }
            }
            _ => {
                if fn_dyn_getp.is_null() {
                    NanBoxedValue::null()
                } else {
                    let f: FnDynGetP = unsafe { std::mem::transmute(fn_dyn_getp) };
                    let p = unsafe { f(obj_ptr, hfield, dst_type_ptr) };
                    if p.is_null() {
                        NanBoxedValue::null()
                    } else {
                        NanBoxedValue::from_ptr(p as usize)
                    }
                }
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn dyn_set_field_by_hash(
        obj_ptr: *mut c_void,
        hfield: i32,
        src_val: NanBoxedValue,
        src_kind: u32,
        src_type_ptr: *mut c_void,
        fn_dyn_setd: *mut c_void,
        fn_dyn_setf: *mut c_void,
        fn_dyn_seti64: *mut c_void,
        fn_dyn_seti: *mut c_void,
        fn_dyn_setp: *mut c_void,
    ) {
        match src_kind {
            hl::hl_type_kind_HF64 => {
                if !fn_dyn_setd.is_null() {
                    let f: FnDynSetD = unsafe { std::mem::transmute(fn_dyn_setd) };
                    unsafe { f(obj_ptr, hfield, src_val.as_f64()) };
                }
            }
            hl::hl_type_kind_HF32 => {
                if !fn_dyn_setf.is_null() {
                    let f: FnDynSetF = unsafe { std::mem::transmute(fn_dyn_setf) };
                    unsafe { f(obj_ptr, hfield, src_val.as_f64() as f32) };
                }
            }
            hl::hl_type_kind_HI64 => {
                if !fn_dyn_seti64.is_null() {
                    let f: FnDynSetI64 = unsafe { std::mem::transmute(fn_dyn_seti64) };
                    unsafe { f(obj_ptr, hfield, src_val.as_i64_lossy()) };
                }
            }
            hl::hl_type_kind_HI32
            | hl::hl_type_kind_HBOOL
            | hl::hl_type_kind_HUI8
            | hl::hl_type_kind_HUI16 => {
                if !fn_dyn_seti.is_null() {
                    let f: FnDynSetI = unsafe { std::mem::transmute(fn_dyn_seti) };
                    let i = if src_kind == hl::hl_type_kind_HBOOL {
                        if src_val.as_bool() {
                            1
                        } else {
                            0
                        }
                    } else {
                        src_val.as_i32()
                    };
                    unsafe { f(obj_ptr, hfield, src_type_ptr, i) };
                }
            }
            _ => {
                if !fn_dyn_setp.is_null() {
                    let f: FnDynSetP = unsafe { std::mem::transmute(fn_dyn_setp) };
                    let p = if src_val.is_null() || src_val.is_void() {
                        std::ptr::null_mut()
                    } else {
                        src_val.as_ptr() as *mut c_void
                    };
                    unsafe { f(obj_ptr, hfield, src_type_ptr, p) };
                }
            }
        }
    }

    fn dynamic_type_name(&self, d: *mut hl::vdynamic) -> Option<String> {
        if d.is_null() || self.fn_type_name.is_null() {
            return None;
        }
        let t = unsafe { (*d).t };
        if t.is_null() {
            return None;
        }
        let f: FnTypeName = unsafe { std::mem::transmute(self.fn_type_name) };
        let name_ptr = unsafe { f(t as *const hl::hl_type) };
        if name_ptr.is_null() {
            return None;
        }
        let s = unsafe { CStr::from_ptr(name_ptr as *const i8) };
        Some(s.to_string_lossy().into_owned())
    }

    /// Turn a stub-bridge failure into a real HL exception on the native trap
    /// chain. **Never returns.**
    ///
    /// The bridge runs *inside* a JIT frame, so its return value is consumed
    /// as a raw word of the callee's declared return type. Reporting a failure
    /// by returning `0` therefore hands compiled code a null it immediately
    /// uses — the reported crash was a field load at offset 0x10 off that
    /// null, one JIT instruction after the bridge returned. There is no safe
    /// poison value, so every failure leaves through `hlp_throw`:
    ///
    /// * A propagating HL exception (`Throw`/`Rethrow` with a real value)
    ///   rethrows unchanged.
    /// * An interpreter-internal failure — a `NullCheck`'s "Null access", an
    ///   unknown findex — carries no throwable `vdynamic`, so one is minted
    ///   from the message exactly the way HashLink's `hl_error` does (a bytes
    ///   dynamic). Haxe sees what it would in interpreter mode, and heaps'
    ///   `catch(e:Dynamic)` in `runMainLoop` handles it identically.
    ///
    /// The throw longjmps to the `setjmp` in `call_compiled_function`, which
    /// restores the interpreter frame stack (the Rust frames between here and
    /// there are abandoned without unwinding) and returns the exception to the
    /// interpreted caller.
    ///
    /// Aborting is the last resort, used only when the stdlib offers no way to
    /// raise at all.
    ///
    /// # Safety
    /// Must be called from the stub bridge, i.e. with a trap armed by
    /// `call_compiled_function` somewhere up the stack.
    unsafe fn raise_stub_bridge_failure(
        resolver: &NativeFunctionResolver,
        findex: usize,
        err: anyhow::Error,
    ) -> ! {
        // One line per findex: the exception itself is the report, and the old
        // unconditional log turned a per-event failure into a stderr flood.
        static REPORTED: OnceLock<Mutex<HashSet<usize>>> = OnceLock::new();
        let first = REPORTED
            .get_or_init(|| Mutex::new(HashSet::new()))
            .lock()
            .map(|mut s| s.insert(findex))
            .unwrap_or(true);
        if first {
            eprintln!(
                "[ash] stub bridge: findex {} failed, raising into the HL trap chain: {:#}",
                findex, err
            );
        }

        let throw_fn = resolver
            .resolve_function("std", "hlp_throw")
            .unwrap_or(std::ptr::null_mut());
        if let Some(hl_exc) = err.downcast_ref::<HLExceptionPropagation>() {
            let val = hl_exc.value;
            if val.is_ptr() && val.as_ptr() != 0 && !throw_fn.is_null() {
                type FnThrow = unsafe extern "C" fn(*mut c_void) -> !;
                let f: FnThrow = std::mem::transmute(throw_fn);
                f(val.as_ptr() as *mut c_void);
            }
        }

        // No throwable value: mint one from the message, as `hl_error` does.
        let error_fn = resolver
            .resolve_function("std", "hlp_error")
            .unwrap_or(std::ptr::null_mut());
        if !error_fn.is_null() {
            let msg = Self::interned_utf16_message(&format!("{:#}", err));
            // `hlp_error` is variadic (`printf`-style), but it is called here
            // with only its fixed argument, and on AAPCS64 the fixed argument
            // lands in x0 either way — the variadic tail lives on the stack.
            type FnError = unsafe extern "C" fn(*const u16) -> !;
            let f: FnError = std::mem::transmute(error_fn);
            f(msg);
        }

        eprintln!(
            "[ash] FATAL: stub bridge cannot raise for findex {} (hlp_throw/hlp_error \
             unresolvable); aborting rather than returning a poison value: {:#}",
            findex, err
        );
        std::process::abort();
    }

    /// Intern `msg` as a leaked, NUL-terminated UTF-16 buffer.
    ///
    /// `hl_error`-style exception values keep a borrowed `uchar*`, so the
    /// buffer has to outlive the throw. Interning bounds the leak by the
    /// number of distinct messages instead of the number of throws.
    fn interned_utf16_message(msg: &str) -> *const u16 {
        static MESSAGES: OnceLock<Mutex<HashMap<String, &'static [u16]>>> = OnceLock::new();
        let map = MESSAGES.get_or_init(|| Mutex::new(HashMap::new()));
        let mut guard = match map.lock() {
            Ok(g) => g,
            Err(poisoned) => poisoned.into_inner(),
        };
        let buf = *guard.entry(msg.to_string()).or_insert_with(|| {
            let mut utf16: Vec<u16> = msg.encode_utf16().collect();
            utf16.push(0);
            &*Box::leak(utf16.into_boxed_slice())
        });
        // Drop the guard before returning: the caller longjmps out of its
        // frame, which would otherwise strand the lock held forever.
        drop(guard);
        buf.as_ptr()
    }

    /// Allocate a bound closure (`InstanceClosure` / `VirtualClosure`) whose
    /// `fun` field is the interpreter's stub sentinel (`findex + 1`).
    ///
    /// `closure_type` is the destination register's declared type — the
    /// signature *without* the bound `this`, which is exactly what a
    /// `vclosure.t` must carry. Leaving it null (as this used to) breaks every
    /// later cast of the closure: `hl_dyn_castp` reads the source type out of
    /// the value's header and bails on null, so `SafeCast` of a closure held
    /// in a `Dynamic` field yielded null and the following `NullCheck` raised
    /// "Null access" — heaps' `for( et in eventTargets ) et(e)` hit this on
    /// every window event.
    ///
    /// Allocation goes through `hlp_alloc_closure_ptr`, the same helper the JIT
    /// uses, so the closure lives in the GC heap and the bound value stays
    /// traceable. The leaked `Box` fallback only runs if the stdlib helper is
    /// unresolvable.
    ///
    /// # Safety
    /// `alloc_closure_ptr`, when non-null, must be `hlp_alloc_closure_ptr`.
    unsafe fn alloc_bound_closure(
        alloc_closure_ptr: *mut c_void,
        closure_type: *mut hl_type,
        findex: usize,
        value: *mut c_void,
    ) -> NanBoxedValue {
        let fun = (findex + 1) as *mut c_void;
        if !alloc_closure_ptr.is_null() {
            type FnAllocClosurePtr =
                unsafe extern "C" fn(*mut hl_type, *mut c_void, *mut c_void) -> *mut _vclosure;
            let f: FnAllocClosurePtr = std::mem::transmute(alloc_closure_ptr);
            let c = f(closure_type, fun, value);
            if !c.is_null() {
                return NanBoxedValue::from_ptr(c as usize);
            }
        }
        let closure = Box::new(_vclosure {
            t: closure_type,
            fun,
            hasValue: 1,
            stackCount: 0,
            value,
        });
        NanBoxedValue::from_ptr(Box::into_raw(closure) as usize)
    }

    fn format_hl_exception(&self, val: NanBoxedValue) -> HLExceptionPropagation {
        let msg = if val.is_null() || val.is_void() {
            None
        } else {
            let dyn_ptr = val.as_ptr() as *mut hl::vdynamic;
            let base = self.value_to_string(dyn_ptr);
            if !self.fn_obj_get_field.is_null() {
                let get_field: FnObjGetField =
                    unsafe { std::mem::transmute(self.fn_obj_get_field) };
                let mut extracted: Option<String> = None;
                for field_name in ["__exceptionMessage", "message"] {
                    let h = self.hash_literal_name(field_name);
                    let msg_dyn = unsafe { get_field(dyn_ptr, h) };
                    if let Some(inner) = self.value_to_string(msg_dyn) {
                        if !inner.is_empty() && inner != "null" {
                            extracted = Some(inner);
                            break;
                        }
                    }
                }
                if let Some(inner) = extracted {
                    if let Some(base) = base.as_ref() {
                        if !inner.is_empty() && inner != *base {
                            Some(format!("{}: {}", base, inner))
                        } else {
                            Some(base.clone())
                        }
                    } else if !inner.is_empty() {
                        Some(inner)
                    } else {
                        base
                    }
                } else {
                    base
                }
            } else {
                base
            }
        };
        HLExceptionPropagation {
            value: val,
            message: msg,
        }
    }

    /// Intern a bytecode string as null-terminated UTF-16 and return a stable pointer.
    fn intern_utf16_string(
        &mut self,
        bytecode: &DecodedBytecode,
        str_idx: usize,
    ) -> Option<*const u16> {
        if let Some(cached) = self.utf16_strings.get(&str_idx) {
            return Some(cached.as_ptr());
        }
        let s = bytecode.strings.get(str_idx)?;
        let mut utf16: Vec<u16> = s.encode_utf16().collect();
        utf16.push(0);
        self.utf16_strings.insert(str_idx, utf16);
        Some(self.utf16_strings[&str_idx].as_ptr())
    }

    /// Execute starting from the bytecode entrypoint.
    pub fn execute_entrypoint(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
    ) -> Result<NanBoxedValue> {
        self.ensure_gc_runtime_initialized();
        // Initialize constants (pre-populated globals) before running
        self.init_constants(bytecode, native_resolver)?;

        // Register the fiber closure runner: ash_std's thread_create runs
        // Haxe thread bodies on krio fibers, and their vclosure fun pointers
        // are interpreter stubs (findex+1) natives cannot call — this shim
        // re-enters the interpreter for them. Raw-pointer context is safe:
        // fibers run on this same OS thread, strictly within
        // execute_entrypoint's dynamic extent.
        struct ClosureRunCtx {
            interp: *mut HLInterpreter,
            bytecode: *const DecodedBytecode,
            resolver: *const NativeFunctionResolver,
        }
        static mut CLOSURE_RUN_CTX: Option<ClosureRunCtx> = None;
        unsafe extern "C" fn fiber_closure_runner(
            c: *mut c_void,
            _args: *mut *mut c_void,
            _nargs: i32,
        ) -> *mut c_void {
            let Some(ctx) = (&raw const CLOSURE_RUN_CTX).as_ref().unwrap().as_ref() else {
                return std::ptr::null_mut();
            };
            let cl = c as *const hl::_vclosure;
            if cl.is_null() {
                return std::ptr::null_mut();
            }
            let fun = (*cl).fun as usize;
            if fun == 0 || fun >= 0x100000 {
                eprintln!("[ash] fiber runner: unsupported closure fun={:#x}", fun);
                return std::ptr::null_mut();
            }
            let findex = fun.wrapping_sub(1);
            let mut args_v = Vec::new();
            if (*cl).hasValue != 0 && !(*cl).value.is_null() {
                args_v.push(NanBoxedValue::from_ptr((*cl).value as usize));
            }
            let interp = &mut *ctx.interp;
            let bytecode = &*ctx.bytecode;
            match interp.call_function(bytecode, &*ctx.resolver, findex, &args_v) {
                Ok(v) => {
                    // Box the result as a vdynamic* for the native caller.
                    // Thread bodies ignore it, but the virtual-dispatch
                    // fallback (hlp_vcall_virtual_hashed) needs real values —
                    // silently returning null turned hasNext() into false.
                    if v.is_void() || v.is_null() {
                        std::ptr::null_mut()
                    } else if v.is_ptr() {
                        v.as_ptr() as *mut c_void
                    } else {
                        // Primitive: box via hlp_make_dyn with the callee's
                        // declared return type.
                        let ret_idx = func_of(&interp.targets, findex)
                            .and_then(|fi| {
                                bytecode.types[bytecode.functions[fi].type_.0]
                                    .fun
                                    .as_ref()
                                    .map(|f| f.ret.0)
                            })
                            .unwrap_or(0);
                        let kind = bytecode.types[ret_idx].kind;
                        let mut raw = interp.value_to_i64(v, kind);
                        let c_t = interp.c_type_factory.get(ret_idx) as *mut c_void;
                        if interp.fn_make_dyn.is_null() || c_t.is_null() {
                            std::ptr::null_mut()
                        } else {
                            let make_dyn: unsafe extern "C" fn(
                                *mut c_void,
                                *mut c_void,
                            )
                                -> *mut c_void = std::mem::transmute(interp.fn_make_dyn);
                            make_dyn(&mut raw as *mut i64 as *mut c_void, c_t)
                        }
                    }
                }
                Err(e) => {
                    eprintln!("[ash] fiber thread uncaught exception: {:#}", e);
                    std::ptr::null_mut()
                }
            }
        }
        unsafe {
            CLOSURE_RUN_CTX = Some(ClosureRunCtx {
                interp: self as *mut _,
                bytecode: bytecode as *const _,
                resolver: native_resolver as *const _,
            });
            let set = native_resolver
                .resolve_function("std", "hlp_set_closure_runner")
                .unwrap_or(std::ptr::null_mut());
            if !set.is_null() {
                type SetRunner = unsafe extern "C" fn(
                    unsafe extern "C" fn(*mut c_void, *mut *mut c_void, i32) -> *mut c_void,
                );
                let f: SetRunner = std::mem::transmute(set);
                f(fiber_closure_runner);
            }
        }

        // Register the JIT stub-call bridge: tiered/promoted code guards every
        // indirect call against interpreter stub sentinels (findex+1) left in
        // shared functions_ptrs/vtable/closure slots and re-enters the
        // interpreter through this bridge instead of SIGBUSing on them.
        // Args/result are raw i64 words per the callee's declared bytecode
        // signature (see ash::jit::stub_bridge for the encoding contract).
        // Same raw-pointer-context justification as the closure runner above:
        // JIT code only runs within execute_entrypoint's dynamic extent, on
        // this OS thread.
        unsafe extern "C" fn jit_stub_call_bridge(
            findex: i32,
            args: *const i64,
            nargs: i32,
        ) -> i64 {
            let Some(ctx) = (&raw const CLOSURE_RUN_CTX).as_ref().unwrap().as_ref() else {
                // Without the context there is no resolver to throw through
                // and no interpreter to run the callee: the only honest
                // outcomes are abort or a poison return, and a poison return
                // is a delayed crash somewhere else.
                eprintln!(
                    "[ash] FATAL: stub bridge called for findex {} with no interpreter \
                     context registered; aborting rather than returning a poison value",
                    findex
                );
                std::process::abort();
            };
            let interp = &mut *ctx.interp;
            let bytecode = &*ctx.bytecode;
            let resolver = &*ctx.resolver;
            let findex = findex as usize;

            // The callee's declared signature drives raw-word decoding.
            let type_idx = if let Some(fi) = func_of(&interp.targets, findex) {
                bytecode.functions[fi].type_.0
            } else if let Some(ni) = native_of(&interp.targets, findex) {
                bytecode.natives[ni].type_.0
            } else {
                HLInterpreter::raise_stub_bridge_failure(
                    resolver,
                    findex,
                    anyhow!("stub bridge: unknown findex {}", findex),
                );
            };
            let Some(fun) = bytecode.types[type_idx].fun.as_ref() else {
                HLInterpreter::raise_stub_bridge_failure(
                    resolver,
                    findex,
                    anyhow!("stub bridge: findex {} has no function type", findex),
                );
            };

            let nargs = nargs.max(0) as usize;
            let raw_args: &[i64] = if nargs == 0 || args.is_null() {
                &[]
            } else {
                std::slice::from_raw_parts(args, nargs)
            };
            let mut vals: Vec<NanBoxedValue> = Vec::with_capacity(raw_args.len());
            for (i, &raw) in raw_args.iter().enumerate() {
                let kind = fun
                    .args
                    .get(i)
                    .map(|a| bytecode.types[a.0].kind)
                    .unwrap_or(hl::hl_type_kind_HDYN);
                vals.push(interp.wrap_native_result(raw, kind));
            }
            let ret_kind = bytecode.types[fun.ret.0].kind;

            match interp.call_function(bytecode, resolver, findex, &vals) {
                Ok(v) => interp.value_to_i64(v, ret_kind),
                // Every failure leaves through the native trap chain — see
                // `raise_stub_bridge_failure`. Returning a value here would
                // hand compiled code a word it is about to use as a pointer.
                Err(e) => HLInterpreter::raise_stub_bridge_failure(resolver, findex, e),
            }
        }
        ash::jit::stub_bridge::set_stub_call_bridge(jit_stub_call_bridge);

        let entry_findex = bytecode.entrypoint as usize;
        let result = self.call_function(bytecode, native_resolver, entry_findex, &[]);

        // After main() returns, run the VM event loop.
        // haxe.MainLoop registers its tick function via hlp_sys_set_loop.
        // The HashLink VM calls this in a loop after the entrypoint returns.
        // This is what drives callbacks, timers, and the render loop.
        let get_loop = native_resolver
            .resolve_function("std", "hlp_sys_get_loop")
            .unwrap_or(std::ptr::null_mut());
        // Diagnostic, not output. It was unconditional, so every run printed a
        // pointer to stderr -- which the parity harness compares against an
        // oracle that will never print it.
        if env_flag!("ASH_TRACE_NATIVE") {
            eprintln!("[ash] Post-main: hlp_sys_get_loop={:p}", get_loop);
        }
        if !get_loop.is_null() {
            type FnGetLoop = unsafe extern "C" fn() -> *mut c_void;
            let get: FnGetLoop = unsafe { std::mem::transmute(get_loop) };
            let loop_fn = unsafe { get() };
            if !loop_fn.is_null() {
                // The loop function is a vclosure — extract findex from stub pointer
                let cl = loop_fn as *const hl::_vclosure;
                let findex = unsafe { (*cl).fun as usize }.wrapping_sub(1);
                let bound = unsafe {
                    if (*cl).hasValue != 0 && !(*cl).value.is_null() {
                        Some(NanBoxedValue::from_ptr((*cl).value as usize))
                    } else {
                        None
                    }
                };
                eprintln!("[ash] Entering VM event loop (findex={})", findex);
                // SDL pumping + frame pacing lives in the stdlib (lock_wait is a
                // pure counter check per HashLink !HL_THREADS semantics), so the
                // VM loop drives it: one pump + ~16ms sleep per tick.
                let pump = native_resolver
                    .resolve_function("std", "hlp_pump_and_sleep")
                    .unwrap_or(std::ptr::null_mut());
                type FnPump = unsafe extern "C" fn();
                loop {
                    let mut args = Vec::new();
                    if let Some(v) = bound {
                        args.push(v);
                    }
                    // The callee has copied the arguments into its own
                    // registers by the time this returns, so the buffer can go
                    // back before the result is even examined.
                    let call_result =
                        self.call_function(bytecode, native_resolver, findex, &args);
                    if self.arg_pool.len() < POOL_CAP {
                        args.clear();
                        self.arg_pool.push(args);
                    }
                    match call_result {
                        Ok(_) => {}
                        Err(e) => {
                            eprintln!("[ash] VM event loop error: {:#}", e);
                            break;
                        }
                    }
                    if !pump.is_null() {
                        unsafe {
                            let f: FnPump = std::mem::transmute(pump);
                            f();
                        }
                    }
                }
            }
        }

        result
    }

    /// Initialize bytecode constants into the globals array.
    /// Constants are pre-allocated global type singletons that must be
    /// initialized before the entrypoint runs.
    fn init_constants(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
    ) -> Result<()> {
        if bytecode.constants.is_empty() {
            return Ok(());
        }

        // Resolve hlp_alloc_closure_void for function-typed constant fields
        let fn_alloc_closure_void = native_resolver
            .resolve_function("std", "hlp_alloc_closure_void")
            .unwrap_or(std::ptr::null_mut());

        for constant in &bytecode.constants {
            let global_idx = constant.global as usize;
            if global_idx >= bytecode.globals.len() {
                continue;
            }

            let type_idx = bytecode.globals[global_idx].0;
            let hl_type_rust = &bytecode.types[type_idx];
            let c_type_ptr = self.c_type_factory.get(type_idx);

            if c_type_ptr.is_null() {
                continue;
            }

            let kind = hl_type_rust.kind;

            if kind == hl::hl_type_kind_HOBJ || kind == hl::hl_type_kind_HSTRUCT {
                let obj_data = match hl_type_rust.obj.as_ref() {
                    Some(o) => o,
                    None => continue,
                };

                // Allocate the object via hlp_alloc_obj
                let alloc_fn = self.fn_alloc_obj;
                if alloc_fn.is_null() {
                    continue;
                }

                let f: FnAllocObj = unsafe { std::mem::transmute(alloc_fn) };
                let obj_ptr = unsafe { f(c_type_ptr as *mut c_void) };
                if obj_ptr.is_null() {
                    continue;
                }

                // Store in globals — BOTH stores, like the SetGlobal opcode.
                //
                // `self.globals` is a Rust Vec: malloc memory the conservative
                // scanner never sees. `globals_data` is the C array registered
                // as a GC root. A constant written only to the Vec — which is
                // what this did, with a comment warning about descriptor
                // slots that actually concerns `(*obj).global_value`, a
                // DIFFERENT pointer — has no root at all: it survives only
                // while some scanned register happens to reference it, and
                // any collection in a gap reclaims it. That was this
                // codebase's oldest intermittent corruption: string constants
                // (every string literal is one) dying under ASH_GC_STRESS,
                // and the map tests' nondeterminism reading freed keys.
                self.globals[global_idx] = NanBoxedValue::from_ptr(obj_ptr as usize);
                {
                    let (gd, nglobals) = self.c_type_factory.globals_data();
                    if !gd.is_null() && global_idx < nglobals {
                        unsafe { *gd.add(global_idx) = obj_ptr as *mut c_void };
                    }
                }

                // Update the global_value slot ONLY when this constant IS the class descriptor
                // for its type (i.e., global_idx == type.obj.global_value - 1).
                // Do NOT update for regular instances (e.g., String constants), which would
                // overwrite the class descriptor slot with a plain data object.
                unsafe {
                    let obj = (*c_type_ptr).__bindgen_anon_1.obj;
                    if !obj.is_null() && !(*obj).global_value.is_null() {
                        let (gd, _) = self.c_type_factory.globals_data();
                        let slot_offset = (*obj).global_value.offset_from(gd as *const *mut c_void);
                        if slot_offset >= 0 && slot_offset as usize == global_idx {
                            *(*obj).global_value = obj_ptr;
                        }
                    }
                }

                // Get runtime object to compute field offsets
                let get_rt = self.fn_get_obj_rt;
                if get_rt.is_null() || constant.fields.is_empty() {
                    continue;
                }

                let rt = unsafe {
                    let get_rt_fn: FnGetObjRt = std::mem::transmute(get_rt);
                    get_rt_fn(c_type_ptr as *mut c_void) as *const hl_runtime_obj
                };

                if rt.is_null() {
                    continue;
                }

                // Calculate field start offset (skip parent fields)
                let start = unsafe { (*rt).nfields as usize - obj_data.fields.len() };

                // Get module context for function pointer stubs
                let module_ctx = self.c_type_factory.module_ctx();

                // Fill in constant fields
                for (j, &field_value) in constant.fields.iter().enumerate() {
                    if j >= obj_data.fields.len() {
                        break;
                    }

                    let field_type_idx = obj_data.fields[j].type_.0;
                    let field_kind = bytecode.types[field_type_idx].kind;
                    let field_c_type = self.c_type_factory.get(field_type_idx);

                    let field_offset = unsafe { *(*rt).fields_indexes.add(j + start) };
                    let field_addr = unsafe { (obj_ptr as *mut u8).add(field_offset as usize) };

                    match field_kind {
                        hl::hl_type_kind_HFUN | hl::hl_type_kind_HMETHOD => {
                            // field_value is a findex - create closure
                            if !fn_alloc_closure_void.is_null() {
                                let findex = field_value as usize;
                                let func_ptr = unsafe {
                                    if !module_ctx.is_null()
                                        && !(*module_ctx).functions_ptrs.is_null()
                                    {
                                        *(*module_ctx).functions_ptrs.add(findex)
                                    } else {
                                        (findex + 1) as *mut c_void
                                    }
                                };

                                let alloc_cl: FnAllocClosureVoid =
                                    unsafe { std::mem::transmute(fn_alloc_closure_void) };
                                let closure =
                                    unsafe { alloc_cl(field_c_type as *mut c_void, func_ptr) };
                                if !closure.is_null() {
                                    unsafe {
                                        *(field_addr as *mut *mut c_void) = closure as *mut c_void;
                                    }
                                }
                            }
                        }
                        k if k == hl::hl_type_kind_HOBJ || k == hl::hl_type_kind_HSTRUCT => {
                            // field_value is a global index reference
                            let ref_global = field_value as usize;
                            if ref_global < self.globals.len() {
                                let ref_val = self.globals[ref_global];
                                unsafe {
                                    *(field_addr as *mut usize) = if ref_val.is_null() {
                                        0
                                    } else {
                                        ref_val.as_ptr()
                                    };
                                }
                            }
                        }
                        hl::hl_type_kind_HBYTES => {
                            // field_value is a string index → convert to UTF-16 pointer
                            let str_idx = field_value as usize;
                            if let Some(ptr) = self.intern_utf16_string(bytecode, str_idx) {
                                unsafe {
                                    *(field_addr as *mut *const u16) = ptr;
                                }
                            }
                        }
                        hl::hl_type_kind_HTYPE => {
                            // field_value is a type index → store c_type pointer (8 bytes)
                            let type_ptr = self.c_type_factory.get(field_value as usize);
                            unsafe {
                                *(field_addr as *mut usize) = type_ptr as usize;
                            }
                        }
                        hl::hl_type_kind_HI32
                        | hl::hl_type_kind_HBOOL
                        | hl::hl_type_kind_HUI8
                        | hl::hl_type_kind_HUI16 => {
                            // field_value is an index into ints table
                            let int_val = bytecode
                                .ints
                                .get(field_value as usize)
                                .copied()
                                .unwrap_or(field_value);
                            unsafe {
                                *(field_addr as *mut i32) = int_val;
                            }
                        }
                        hl::hl_type_kind_HI64 => {
                            let int_val = bytecode
                                .ints
                                .get(field_value as usize)
                                .copied()
                                .unwrap_or(field_value);
                            unsafe {
                                *(field_addr as *mut i64) = int_val as i64;
                            }
                        }
                        hl::hl_type_kind_HF64 => {
                            let float_val = bytecode
                                .floats
                                .get(field_value as usize)
                                .copied()
                                .unwrap_or(0.0);
                            unsafe {
                                *(field_addr as *mut f64) = float_val;
                            }
                        }
                        hl::hl_type_kind_HF32 => {
                            let float_val = bytecode
                                .floats
                                .get(field_value as usize)
                                .copied()
                                .unwrap_or(0.0);
                            unsafe {
                                *(field_addr as *mut f32) = float_val as f32;
                            }
                        }
                        _ => {
                            // All other types are pointer-like (HARRAY, HDYN, HENUM, etc.)
                            // field_value=0 means null; otherwise it's a global index.
                            if field_value == 0 {
                                unsafe {
                                    *(field_addr as *mut usize) = 0;
                                }
                            } else {
                                let ref_global = field_value as usize;
                                if ref_global < self.globals.len() {
                                    let ref_val = self.globals[ref_global];
                                    unsafe {
                                        *(field_addr as *mut usize) = if ref_val.is_null() {
                                            0
                                        } else {
                                            ref_val.as_ptr()
                                        };
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // Reconcile the two global stores into one view.
        //
        // There are two: `self.globals`, which the interpreter reads first, and
        // `globals_data`, the flat C array that native and *compiled* code
        // read. Neither was complete. Native code writes global_value slots
        // during binding setup that the interpreter never saw, and the constant
        // loop above writes `self.globals` for every constant but `globals_data`
        // only for class descriptors -- so a plain constant like the "\n" that
        // `Sys.println` appends lived in one store and was null in the other.
        //
        // The interpreter never noticed, because it falls back to `globals_data`
        // only when its own slot is null. Compiled code has no such fallback:
        // Cranelift's GetGlobal loads the `globals_data` slot address directly,
        // so it read null where the interpreter read a string. That is what kept
        // the tier's field-access gate shut -- the defect was never in the field
        // offsets, it was that admitting Field made these functions eligible at
        // all.
        //
        // Whichever store has a value wins; a slot set in both is left alone.
        let (gd, nglobals) = self.c_type_factory.globals_data();
        if !gd.is_null() {
            let n = nglobals.min(self.globals.len());
            for i in 0..n {
                let raw = unsafe { *gd.add(i) };
                if !raw.is_null() && self.globals[i].is_null() {
                    self.globals[i] = NanBoxedValue::from_ptr(raw as usize);
                }
            }
            for i in 0..n {
                if unsafe { *gd.add(i) }.is_null() {
                    let v = self.globals[i];
                    if !v.is_null() && !v.is_void() {
                        unsafe { *gd.add(i) = v.as_ptr() as *mut c_void };
                    }
                }
            }
        }

        Ok(())
    }

    /// Call a function by its findex.
    /// Record that `func_idx` has a loop whose header is `header_pc` and which
    /// has gone round enough times to be worth compiling.
    ///
    /// Promotion counts calls, so this is the only signal that reaches a
    /// function invoked once. nbody's `main` is the case: its ten million
    /// iterations live in a single invocation, and the profiler charges 22.7%
    /// of the whole run to `call_function` -- the boundary crossings from that
    /// interpreted loop into compiled `advance`.
    /// Feed a hot loop's back-edges into the ordinary promotion machinery.
    ///
    /// This is the whole of the interpreter's part in OSR. Promotion counts
    /// invocations, so a loop inside a function called once never crosses a
    /// threshold however long it runs -- nbody's `main` is ten million
    /// iterations of a single call. Ticking the bead from a back-edge makes it
    /// promotable on the same terms as any other function: through the ladder,
    /// Cranelift and then LLVM, compiled on the broker.
    ///
    /// It deliberately does not compile anything itself. An earlier version
    /// did, and that was the mistake: OSR is not a way to get code compiled,
    /// it is a way for a frame that is already running to pick up code the
    /// tiering has already produced. Compiling here bypassed the ladder,
    /// ignored `--jit-tier`, blocked the main thread, and left no path to tier
    /// up afterwards -- one error with four symptoms.
    fn note_hot_loop(&mut self, bytecode: &DecodedBytecode, func_idx: usize, header_pc: usize) {
        let findex = self.bytecode_findex(func_idx);

        // Publish the header before ticking: a tick can be the one that
        // submits the LLVM compile, and the broker reads this map when that
        // compile finishes to know which entries to build.
        if self.hot_loops.insert((findex, header_pc)) {
            if let Some(t) = self.tiered_runtime.as_ref() {
                t.shared_ctx
                    .hot_loop_pcs
                    .lock()
                    .expect("hot_loop_pcs mutex poisoned")
                    .entry(findex)
                    .or_default()
                    .push(header_pc);
            }
            self.report_hot_loop(bytecode, func_idx, findex, header_pc);
            self.late_osr_entry(bytecode, func_idx, findex, header_pc);
        }

        // Tick the bead. The returned entry is the function's *normal* entry
        // point and is deliberately dropped: calling it would restart the
        // function from the top, which is the one thing a mid-loop transfer
        // must not do. Only the tick matters here.
        let _ = self.tiered_on_invoke(bytecode, findex, func_idx);
    }

    /// Build and attach ONE OSR entry for a header that turned hot after its
    /// function was already LLVM-promoted.
    ///
    /// The promote-time batch covers headers probed before the code landed;
    /// this covers the other order, which is the common one for the loop
    /// that matters — nbody's probes that drove promotion came from short
    /// init loops, and the 10M-iteration loop was reached only afterwards.
    ///
    /// This is a synchronous LLVM compile on the main thread — the thing the
    /// tiering otherwise never does — accepted here because it is one entry,
    /// once per truly-hot header, for a loop that is by definition still
    /// running: ~80ms of compile against seconds of remaining
    /// interpretation. It does not compile the FUNCTION (the ladder already
    /// did, on the broker); it builds the door into code that already
    /// exists.
    fn late_osr_entry(
        &mut self,
        bytecode: &DecodedBytecode,
        func_idx: usize,
        findex: usize,
        header_pc: usize,
    ) {
        if !osr_transfer_enabled() {
            return;
        }
        let Some(tiered) = self.tiered_runtime.as_ref() else {
            return;
        };
        let ctx = Arc::clone(&tiered.shared_ctx);
        // Some tier must have installed code for this findex — an entry is a
        // door into code that already exists.
        let Some(addr) = self
            .tiered_runtime
            .as_ref()
            .and_then(|t| t.entries.get(findex))
            .and_then(|e| e.as_ref())
            .map(|e| e.fn_addr)
        else {
            return;
        };
        let llvm_installed = ctx
            .llvm_done
            .lock()
            .expect("llvm_done mutex poisoned")
            .contains(&findex);
        if !ash::air_pipeline::air_enabled() {
            return; // raw-opcode mode has no block_pcs to validate against
        }

        let raw = &bytecode.functions[func_idx];
        let m = ash::air_pipeline::AshModule::new(bytecode);
        let Ok(opt) = ash::air_pipeline::optimized(&m, raw) else {
            return;
        };
        let plan = ash::osr::analyze(&opt.ir);
        let eligible = plan
            .entry_headers
            .iter()
            .any(|&h| opt.ser.block_pcs.get(h as usize) == Some(&header_pc));
        if !eligible {
            return;
        }

        // Prefer the fast door. A Cranelift entry costs ~1ms — cheap enough
        // to build synchronously right here, which matters because this runs
        // on the MAIN thread mid-loop. The LLVM entry is a promote-sized
        // compile and is only worth that stall when LLVM code is what is
        // installed; and even then, never while a broker holds the module
        // lock — `try_lock`, and a miss just retries on a later probe.
        let entry_addr: u64 = if !llvm_installed {
            let cl = ctx.cranelift.lock().expect("cranelift mutex poisoned");
            let Some(tier) = cl.as_ref().and_then(|o| o.as_ref()).cloned() else {
                return;
            };
            drop(cl);
            let Some(bound) = self
                .tiered_runtime
                .as_ref()
                .and_then(|t| t.beads.get(findex))
                .and_then(|b| b.as_ref())
            else {
                return;
            };
            match ash::cranelift::codegen::compile_osr_entry(
                &tier.backend,
                &tier.ctx,
                bound.bead(),
                findex,
                &opt,
                header_pc,
            ) {
                Ok(a) => a as u64,
                Err(e) => {
                    if osr_logging() {
                        eprintln!(
                            "[osr] late cranelift entry declined findex={findex} pc={header_pc}: {e:#}"
                        );
                    }
                    return;
                }
            }
        } else {
            let body = self.air.body(bytecode, func_idx);
            let Ok(mut guard) = ctx.llvm.try_lock() else {
                // A broker is compiling; blocking the interpreter behind it
                // was 11.5% of nbody's execute. The header stays in
                // `hot_loops`... which would stop this from retrying, so put
                // it back on the retry path by forgetting it was seen.
                self.hot_loops.remove(&(findex, header_pc));
                return;
            };
            let LlvmState::Ready(module) = &mut *guard else {
                return;
            };
            match module.0.compile_osr_entry(findex, header_pc, body) {
                Ok(a) if a != 0 => a,
                Ok(_) => return,
                Err(e) => {
                    if osr_logging() {
                        eprintln!(
                            "[osr] late entry compile failed findex={findex} pc={header_pc}: {e:#}"
                        );
                    }
                    return;
                }
            }
        };

        // Incremental attach: the swap replaces the whole table, so resend
        // what is already installed plus the new entry.
        let mut entries = self.osr_attached.get(&findex).cloned().unwrap_or_default();
        entries.push(OsrEntry {
            site: header_pc as u64,
            code: entry_addr as *mut (),
        });
        let Some(bound) = self
            .tiered_runtime
            .as_ref()
            .and_then(|t| t.beads.get(findex))
            .and_then(|b| b.as_ref())
        else {
            return;
        };
        if bound
            .bead()
            .swap_compiled_with_osr(addr as *mut (), entries.clone())
            .is_some()
        {
            self.osr_attached.insert(findex, entries);
            if osr_logging() {
                eprintln!("[osr] late-attached entry findex={findex} pc={header_pc}");
            }
        }
    }

    /// The once-per-header eligibility log, split out of the probe path.
    fn report_hot_loop(
        &self,
        bytecode: &DecodedBytecode,
        func_idx: usize,
        findex: usize,
        header_pc: usize,
    ) {
        // `osr::analyze` answers from the IR whether a transfer may enter at
        // all: a live trap region or a register whose address escaped means no.
        if osr_logging() {
            let m = ash::air_pipeline::AshModule::new(bytecode);
            let opts = ash::air_pipeline::AirPassOptions::default();
            let plan = ash::air_pipeline::prepare_ir(
                &m,
                &bytecode.functions[func_idx],
                ash::air_pipeline::AirOptLevel::O2,
                &opts,
            )
            .ok()
            .map(|(f, _)| ash::osr::analyze(&f));
            match &plan {
                Some(p) if p.eligible() => {
                    eprintln!("[osr] hot loop findex={findex} pc={header_pc} ELIGIBLE")
                }
                Some(p) => eprintln!(
                    "[osr] hot loop findex={findex} pc={header_pc} refused: {:?}",
                    p.refusals
                ),
                None => eprintln!("[osr] hot loop findex={findex} pc={header_pc} refused: no IR"),
            }
        }
    }

    /// Take an OSR entry if the tiering has installed one for this header.
    ///
    /// The entry is looked up on the bead, not compiled here: `Bead::osr_entry`
    /// is a binary search over the table a tier-up installed alongside its
    /// code. beadie only lets entries be attached by `swap_compiled_with_osr`
    /// -- the tier-up path -- and not by the first install, which is the
    /// library saying that OSR belongs between tiers rather than in front of
    /// them.
    ///
    /// Returns the function's result, because the compiled code runs the loop
    /// and everything after it; control does not come back for this
    /// invocation. `None` means no entry is installed yet and the interpreter
    /// should carry on.
    fn try_osr_transfer(
        &mut self,
        bytecode: &DecodedBytecode,
        func_idx: usize,
        header_pc: usize,
    ) -> Result<Option<NanBoxedValue>> {
        if !osr_transfer_enabled() {
            return Ok(None);
        }
        let findex = self.bytecode_findex(func_idx);
        let site = header_pc as u64;
        let addr = {
            let tiered = self.tiered_runtime.as_ref();
            let Some(bound) = tiered.and_then(|t| t.beads.get(findex)).and_then(|b| b.as_ref())
            else {
                return Ok(None);
            };
            match bound.bead().osr_entry(site) {
                Some(p) if !p.is_null() => p as u64,
                _ => return Ok(None),
            }
        };

        let body = self.air.body(bytecode, func_idx);
        let Some(fun_ty) = bytecode.types[body.type_.0].fun.as_ref() else {
            return Ok(None);
        };
        let ret_kind = bytecode.types[fun_ty.ret.0].kind;

        let mut buf: Vec<u64> = Vec::with_capacity(body.regs.len());
        {
            let frame = self.stack.last().expect("frame for the running function");
            for (i, reg) in body.regs.iter().enumerate() {
                let kind = bytecode.types[reg.0].kind;
                let v = frame.registers.get(i as u32);
                buf.push(self.value_to_i64(v, kind) as u64);
            }
        }
        if osr_logging() {
            eprintln!(
                "[osr] entering findex={findex} pc={header_pc} regs={} at {addr:#x}",
                buf.len()
            );
        }

        // Armed exactly as the ordinary call boundary arms one: the compiled
        // code can call something that throws, and a throw crossing this frame
        // needs a jmp_buf here.
        let stack_depth = self.stack.len();
        let fn_setup_trap = self.fn_setup_trap_jit;
        let fn_remove_trap = self.fn_remove_trap_jit;
        let mut trap_installed = false;
        if !fn_setup_trap.is_null() {
            type FnSetupTrap = unsafe extern "C" fn() -> *mut c_void;
            let setup: FnSetupTrap = unsafe { std::mem::transmute(fn_setup_trap) };
            let jmp_buf = unsafe { setup() };
            if !jmp_buf.is_null() {
                trap_installed = true;
                if unsafe { call_setjmp_opaque(jmp_buf) } != 0 {
                    for f in self.stack.drain(stack_depth..) {
                        if self.reg_pool.len() < POOL_CAP {
                            self.reg_pool.push(f.into_buffer());
                        }
                    }
                    let fn_get_exc = self.fn_get_exc_value;
                    let fn_clear_exc = self.fn_clear_exc_value;
                    if !fn_get_exc.is_null() {
                        type FnGetExc = unsafe extern "C" fn() -> *mut c_void;
                        let exc_ptr =
                            unsafe { (std::mem::transmute::<*mut c_void, FnGetExc>(fn_get_exc))() };
                        if !exc_ptr.is_null() {
                            if !fn_clear_exc.is_null() {
                                type FnClearExc = unsafe extern "C" fn();
                                unsafe {
                                    (std::mem::transmute::<*mut c_void, FnClearExc>(fn_clear_exc))()
                                };
                            }
                            return Err(anyhow::Error::new(
                                self.format_hl_exception(NanBoxedValue::from_ptr(exc_ptr as usize)),
                            ));
                        }
                    }
                    return Err(anyhow!(
                        "osr transfer longjmp without exception: findex {findex}"
                    ));
                }
            }
        }

        let raw = unsafe {
            if matches!(ret_kind, hl::hl_type_kind_HF32 | hl::hl_type_kind_HF64) {
                type FnF64 = unsafe extern "C" fn(*mut u64) -> f64;
                let f: FnF64 = std::mem::transmute(addr as usize);
                f(buf.as_mut_ptr()).to_bits() as i64
            } else {
                type FnI64 = unsafe extern "C" fn(*mut u64) -> i64;
                let f: FnI64 = std::mem::transmute(addr as usize);
                f(buf.as_mut_ptr())
            }
        };

        if trap_installed && !fn_remove_trap.is_null() {
            type FnRemoveTrap = unsafe extern "C" fn();
            let remove: FnRemoveTrap = unsafe { std::mem::transmute(fn_remove_trap) };
            unsafe { remove() };
        }

        ash::profile::count("osr transfers", 1);
        Ok(Some(self.wrap_native_result(raw, ret_kind)))
    }

    /// The findex of a function by its index in `bytecode.functions`.
    fn bytecode_findex(&self, func_idx: usize) -> usize {
        self.targets
            .iter()
            .position(|t| matches!(t, CallTarget::Func(i) if *i as usize == func_idx))
            .unwrap_or(func_idx)
    }

    pub fn call_function(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        findex: usize,
        args: &[NanBoxedValue],
    ) -> Result<NanBoxedValue> {
        self.ensure_gc_runtime_initialized();

        // Check if it's a bytecode function or native
        if let Some(func_idx) = func_of(&self.targets, findex) {
            // Hybrid tiered call path: tick the bead and dispatch to compiled
            // code once beadie's broker has installed it.
            if self.tiered_runtime.is_some() {
                if let Some(entry) = self.tiered_on_invoke(bytecode, findex, func_idx) {
                    match self.call_compiled_function(findex, &entry, args) {
                        Ok(v) => {
                            if let Some(tiered) = self.tiered_runtime.as_mut() {
                                tiered.stats.compiled_calls += 1;
                            }
                            return Ok(v);
                        }
                        Err(e) => {
                            self.record_tiered_fallback(
                                findex,
                                format!("compiled invoke failed: {}", e),
                            );
                        }
                    }
                }
            }
            self.execute_hl_function(bytecode, native_resolver, func_idx, args)
        } else if let Some(native_idx) = native_of(&self.targets, findex) {
            self.call_native(bytecode, native_resolver, native_idx, args)
        } else {
            Err(anyhow!("Function findex {} not found", findex))
        }
    }

    /// beadie `TieredAdapter::on_invoke` semantics for one bytecode-function
    /// call: tick the bead, let the adapter submit tier-0 and tier-1 compile
    /// jobs when their policies fire, and return the currently installed code.
    ///
    /// The marshaling entry is rebuilt whenever the installed pointer changes,
    /// which is exactly how a tier-1 upgrade (`swap_compiled`) is observed —
    /// the signature itself comes from the bytecode and is tier-independent.
    /// Returns `Some(entry)` when compiled code should be dispatched, `None`
    /// to keep interpreting.
    fn tiered_on_invoke(
        &mut self,
        bytecode: &DecodedBytecode,
        findex: usize,
        func_idx: usize,
    ) -> Option<CompiledFunctionEntry> {
        // One-time registration gate: untierable findexes get no bead.
        {
            let tiered = self.tiered_runtime.as_mut()?;
            if !tiered.config.enabled {
                return None;
            }
            if findex >= tiered.beads.len() {
                tiered.beads.resize_with(findex + 1, || None);
                tiered.gate_checked.resize(findex + 1, false);
                tiered.entries.resize_with(findex + 1, || None);
                tiered.sigs.resize_with(findex + 1, || None);
            }
            if !tiered.gate_checked[findex] {
                tiered.gate_checked[findex] = true;
                // Publish the bytecode the Cranelift tier lowers from.
                tiered.shared_ctx.set_bytecode(bytecode);
                match Self::tierable_reason(bytecode, func_idx, &tiered.config) {
                    Ok(()) => {
                        let bound = tiered.adapter.register(findex as beadie::CoreHandle, None);
                        tiered.beads[findex] = Some(bound);
                        let f = &bytecode.functions[func_idx];
                        if let Some(tf) = bytecode.types[f.type_.0].fun.as_ref() {
                            let mut arg_kinds = [0u32; 8];
                            let nargs = tf.args.len().min(8);
                            for (i, a) in tf.args.iter().take(8).enumerate() {
                                arg_kinds[i] = bytecode.types[a.0].kind;
                            }
                            let ret_kind = bytecode.types[tf.ret.0].kind;
                            tiered.sigs[findex] = Some((arg_kinds, nargs as u8, ret_kind));
                        }
                    }
                    Err(reason) => {
                        if tiered.config.log_promotions {
                            eprintln!("[tiered] skip findex={} reason={}", findex, reason);
                        }
                        return None;
                    }
                }
            }
        }

        // Immutable phase: tick + (possibly) submit compile jobs.
        let code = {
            let tiered = self.tiered_runtime.as_ref()?;
            let bound = tiered.beads[findex].as_ref()?;
            let ctx = Arc::clone(&tiered.shared_ctx);
            tiered.adapter.on_invoke(bound, move |tier, bead| {
                tiered_compile_tier(&ctx, tier, findex, bead)
            })?
        };

        let addr = code as usize;
        let tiered = self.tiered_runtime.as_mut()?;
        // Steady state: the bead is already compiled and its entry is cached.
        // This is the path ~10M invocations take, so it must be a compare and a
        // copy — nothing refcounted, nothing rebuilt.
        if let Some(entry) = tiered.entries[findex].as_ref() {
            if entry.fn_addr == addr {
                return Some(*entry);
            }
        }
        // Freshly installed (or newly swapped-in) code.
        //
        // Attach any OSR entries the promote staged. The re-swap with the
        // SAME code pointer is beadie's install path for a table: it bumps
        // the generation, which is what activates the entries — a table
        // installed before the adapter's own install would be orphaned by
        // that install's bump. For a single-invocation hot loop this branch
        // is reached by the loop's own back-edge ticks, so the entries are
        // live within 64 iterations of the code landing.
        if let Some(entries) = tiered
            .shared_ctx
            .pending_osr
            .lock()
            .expect("pending_osr mutex poisoned")
            .remove(&findex)
        {
            if let Some(bound) = tiered.beads[findex].as_ref() {
                let n = entries.len();
                if bound
                    .bead()
                    .swap_compiled_with_osr(addr as *mut (), entries.clone())
                    .is_some()
                {
                    self.osr_attached.insert(findex, entries);
                    if osr_logging() {
                        eprintln!("[osr] attached {n} entries findex={findex}");
                    }
                } else if osr_logging() {
                    eprintln!("[osr] attach refused findex={findex} (bead not compiled)");
                }
            }
        }
        let (arg_kinds, nargs, ret_kind) = tiered.sigs[findex]?;
        let entry = CompiledFunctionEntry {
            fn_addr: addr,
            arg_kinds,
            nargs,
            ret_kind,
        };
        tiered.stats.successful_promotions += 1;
        if tiered.config.log_promotions {
            eprintln!(
                "[tiered] promoted findex={} addr=0x{:x} gen={}",
                findex,
                addr,
                tiered.beads[findex]
                    .as_ref()
                    .map(|b| b.generation())
                    .unwrap_or(0)
            );
        }
        tiered.entries[findex] = Some(entry.clone());
        Some(entry)
    }

    /// One-time tierability gate, run at bead registration (not per call).
    fn tierable_reason(
        bytecode: &DecodedBytecode,
        func_idx: usize,
        config: &TieredConfig,
    ) -> std::result::Result<(), String> {
        if !config.strict_mode {
            return Err("non_strict_mode".to_string());
        }
        let func = &bytecode.functions[func_idx];
        // Debug escape hatch: ASH_TIERED_SKIP_FINDEXES=1,2,3 excludes specific
        // findexes from promotion (for bisecting a miscompiled hot function).
        {
            static SKIP: std::sync::OnceLock<Vec<usize>> = std::sync::OnceLock::new();
            let skip = SKIP.get_or_init(|| {
                std::env::var("ASH_TIERED_SKIP_FINDEXES")
                    .map(|v| v.split(',').filter_map(|s| s.trim().parse().ok()).collect())
                    .unwrap_or_default()
            });
            if skip.contains(&(func.findex as usize)) {
                return Err("skipped_by_env".to_string());
            }
        }
        // Static signature arg count; call_compiled_function marshals at most 8.
        let nargs = bytecode.types[func.type_.0]
            .fun
            .as_ref()
            .map(|f| f.args.len())
            .unwrap_or(0);
        if nargs > config.max_jit_args || nargs > 8 {
            return Err("arg_count_over_limit".to_string());
        }
        let func_name = func.name();
        if func_name == "init"
            || func_name == "main"
            || func_name == "__constructor__"
            || func_name.starts_with("__")
        {
            return Err("name_blacklisted".to_string());
        }
        if config.min_ops_for_promotion > 0 && func.ops.len() < config.min_ops_for_promotion {
            return Err("op_count_below_min".to_string());
        }
        if let Some(bad) = func.ops.iter().find(|op| !Self::is_v1_tierable_opcode(op)) {
            return Err(format!("unsupported_opcode op={:?}", bad));
        }
        // Cranelift-only mode has no LLVM fallback, so a function the middle
        // tier cannot lower must not register a bead at all — a null tier-0
        // result would blacklist it instead of leaving it interpreted.
        if config.tier_mode == TierMode::Cranelift {
            if let Some(reason) = ash::cranelift::lowering_reject_reason(bytecode, func) {
                return Err(format!("cranelift_{reason}"));
            }
        }
        Ok(())
    }

    fn is_v1_tierable_opcode(op: &Opcode) -> bool {
        // Allow all opcodes the full JIT supports. The v1 whitelist was too
        // conservative — it blocked GetThis/GetGlobal/Call/Field etc., preventing
        // any real Heaps functions from being promoted.
        !matches!(op, Opcode::Prefetch { .. } | Opcode::Asm { .. })
    }

    /// Deopt after a compiled invoke failed: invalidate the bead (permanent
    /// Deopt via beadie) and drop the cached marshaling entry so the function
    /// falls back to the interpreter.
    fn record_tiered_fallback(&mut self, findex: usize, reason: String) {
        if let Some(tiered) = self.tiered_runtime.as_mut() {
            tiered.stats.fallback_calls += 1;
            tiered.stats.failed_promotions += 1;
            if let Some(slot) = tiered.entries.get_mut(findex) {
                *slot = None;
            }
            if let Some(bound) = tiered.beads.get(findex).and_then(|b| b.as_ref()) {
                bound.bead().invalidate();
            }
            if tiered.config.log_promotions {
                eprintln!("[tiered] fallback findex={} reason={}", findex, reason);
            }
        }
    }

    fn call_compiled_function(
        &mut self,
        findex: usize,
        entry: &CompiledFunctionEntry,
        args: &[NanBoxedValue],
    ) -> Result<NanBoxedValue> {
        if args.len() > 8 {
            return Err(anyhow!(
                "Compiled call {} has {} args (max 8)",
                findex,
                args.len()
            ));
        }

        self.sync_gc_scan_roots();

        let func_ptr = entry.fn_addr as *mut c_void;
        // `args()`, not the raw array: the array is fixed at eight slots and
        // only the first `nargs` are real. Iterating the whole thing would put
        // trailing zero kinds into the float mask and misroute the call.
        let arg_kinds = entry.args();
        let ret_kind = entry.ret_kind;

        let is_float_kind = |k: u32| k == hl::hl_type_kind_HF32 || k == hl::hl_type_kind_HF64;
        let ret_is_float = is_float_kind(ret_kind);
        let float_mask: u32 = arg_kinds.iter().enumerate().fold(0u32, |acc, (i, &k)| {
            if is_float_kind(k) {
                acc | (1 << i)
            } else {
                acc
            }
        });

        let extract_arg = |idx: usize| -> i64 {
            let kind = if idx < arg_kinds.len() {
                arg_kinds[idx]
            } else {
                0
            };
            self.value_to_i64(args[idx], kind)
        };

        let fn_setup_trap = self.fn_setup_trap_jit;
        let fn_remove_trap = self.fn_remove_trap_jit;
        let fn_get_exc = self.fn_get_exc_value;
        let fn_clear_exc = self.fn_clear_exc_value;
        // Frame depth at the setjmp site. A throw from inside the compiled
        // callee — including one raised by the stub bridge re-entering the
        // interpreter — longjmps back here without unwinding, so the frames
        // those abandoned Rust activations pushed are still on `self.stack`
        // and have to be dropped explicitly.
        let stack_depth = self.stack.len();
        let mut trap_installed = false;
        if !fn_setup_trap.is_null() {
            type FnSetupTrap = unsafe extern "C" fn() -> *mut c_void;
            let setup: FnSetupTrap = unsafe { std::mem::transmute(fn_setup_trap) };
            let jmp_buf = unsafe { setup() };
            if !jmp_buf.is_null() {
                trap_installed = true;
                let jumped = unsafe { call_setjmp_opaque(jmp_buf) };
                if jumped != 0 {
                    for f in self.stack.drain(stack_depth..) {
                        if self.reg_pool.len() < POOL_CAP {
                            self.reg_pool.push(f.into_buffer());
                        }
                    }
                    self.sync_gc_scan_roots();
                    if !fn_get_exc.is_null() {
                        type FnGetExc = unsafe extern "C" fn() -> *mut c_void;
                        let exc_ptr =
                            unsafe { (std::mem::transmute::<*mut c_void, FnGetExc>(fn_get_exc))() };
                        if !exc_ptr.is_null() {
                            if !fn_clear_exc.is_null() {
                                type FnClearExc = unsafe extern "C" fn();
                                unsafe {
                                    (std::mem::transmute::<*mut c_void, FnClearExc>(fn_clear_exc))()
                                };
                            }
                            return Err(anyhow::Error::new(
                                self.format_hl_exception(NanBoxedValue::from_ptr(exc_ptr as usize)),
                            ));
                        }
                    }
                    return Err(anyhow!(
                        "Compiled call longjmp without exception: findex {}",
                        findex
                    ));
                }
            }
        }

        let dispatch_res: Result<i64> = if ret_is_float || float_mask != 0 {
            self.dispatch_float_native(func_ptr, args, arg_kinds, float_mask, ret_is_float)
        } else {
            Ok(unsafe {
                match args.len() {
                    0 => {
                        let f: unsafe extern "C" fn() -> i64 = std::mem::transmute(func_ptr);
                        f()
                    }
                    1 => {
                        let f: unsafe extern "C" fn(i64) -> i64 = std::mem::transmute(func_ptr);
                        f(extract_arg(0))
                    }
                    2 => {
                        let f: unsafe extern "C" fn(i64, i64) -> i64 =
                            std::mem::transmute(func_ptr);
                        f(extract_arg(0), extract_arg(1))
                    }
                    3 => {
                        let f: unsafe extern "C" fn(i64, i64, i64) -> i64 =
                            std::mem::transmute(func_ptr);
                        f(extract_arg(0), extract_arg(1), extract_arg(2))
                    }
                    4 => {
                        let f: unsafe extern "C" fn(i64, i64, i64, i64) -> i64 =
                            std::mem::transmute(func_ptr);
                        f(
                            extract_arg(0),
                            extract_arg(1),
                            extract_arg(2),
                            extract_arg(3),
                        )
                    }
                    5 => {
                        let f: unsafe extern "C" fn(i64, i64, i64, i64, i64) -> i64 =
                            std::mem::transmute(func_ptr);
                        f(
                            extract_arg(0),
                            extract_arg(1),
                            extract_arg(2),
                            extract_arg(3),
                            extract_arg(4),
                        )
                    }
                    6 => {
                        let f: unsafe extern "C" fn(i64, i64, i64, i64, i64, i64) -> i64 =
                            std::mem::transmute(func_ptr);
                        f(
                            extract_arg(0),
                            extract_arg(1),
                            extract_arg(2),
                            extract_arg(3),
                            extract_arg(4),
                            extract_arg(5),
                        )
                    }
                    7 => {
                        let f: unsafe extern "C" fn(i64, i64, i64, i64, i64, i64, i64) -> i64 =
                            std::mem::transmute(func_ptr);
                        f(
                            extract_arg(0),
                            extract_arg(1),
                            extract_arg(2),
                            extract_arg(3),
                            extract_arg(4),
                            extract_arg(5),
                            extract_arg(6),
                        )
                    }
                    8 => {
                        let f: unsafe extern "C" fn(i64, i64, i64, i64, i64, i64, i64, i64) -> i64 =
                            std::mem::transmute(func_ptr);
                        f(
                            extract_arg(0),
                            extract_arg(1),
                            extract_arg(2),
                            extract_arg(3),
                            extract_arg(4),
                            extract_arg(5),
                            extract_arg(6),
                            extract_arg(7),
                        )
                    }
                    _ => 0i64,
                }
            })
        };

        if trap_installed && !fn_remove_trap.is_null() {
            type FnRemoveTrap = unsafe extern "C" fn();
            unsafe { (std::mem::transmute::<*mut c_void, FnRemoveTrap>(fn_remove_trap))() };
        }

        let raw_result = dispatch_res?;
        Ok(self.wrap_native_result(raw_result, ret_kind))
    }

    /// Execute a HashLink bytecode function.
    fn execute_hl_function(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        func_idx: usize,
        args: &[NanBoxedValue],
    ) -> Result<NanBoxedValue> {
        // Use reloaded bytecode if available (hot-reload swapped function bodies)
        let using_reloaded = self.reloaded_bytecode.is_some();
        let bc: &DecodedBytecode = self.reloaded_bytecode.unwrap_or(bytecode);
        // Decide this function's body before the frame exists, so every site
        // that resolves it below — register count, argument binding, the
        // dispatch loop — sees the same opcode array and the same `pc` means
        // the same instruction throughout the call.
        //
        // The SSA form is tried first and is a different shape of body
        // entirely: a CFG over a value-indexed frame rather than an opcode
        // array over a register file, so it takes its own entry path. A
        // function it refuses falls through to the opcode dispatcher below,
        // which is the reference either way.
        self.ssa.prepare(bc, func_idx);
        if let Some(prep) = self.ssa.body(func_idx) {
            return self.execute_ssa_function(bc, native_resolver, func_idx, prep, args);
        }
        self.air.prepare(bc, func_idx);
        let func = self.air.body(bc, func_idx);
        if using_reloaded && env_flag!("ASH_DBG_RELOAD") {
            eprintln!(
                "[reload-exec] func_idx={} name={} nops={} using=reloaded",
                func_idx,
                func.name(),
                func.ops.len()
            );
        }

        if self.stack.len() >= self.max_stack_depth {
            return Err(anyhow!("Stack overflow (depth {})", self.stack.len()));
        }

        // Create frame with registers
        let reg_count = func.regs.len();
        let buf = self.reg_pool.pop().unwrap_or_default();
        let mut frame = InterpreterFrame::with_buffer(func_idx, reg_count, buf);

        // Bind arguments to first N registers
        let type_fun = bc.types[func.type_.0]
            .fun
            .as_ref()
            .expect("function should have fun type");
        for (i, arg) in args.iter().enumerate() {
            if i < type_fun.args.len() {
                frame.registers.set(i as u32, *arg);
            }
        }

        self.stack.push(frame);
        self.sync_gc_scan_roots();

        // Main interpretation loop — always pop the frame even on error.
        // The findex is published for the sampling profiler so a sample landing
        // in the interpreter (or in a runtime helper it called) can be charged
        // to the bytecode function being executed, not just to the loop.
        let prev_findex = ash::profile::enter_interp(bc.functions[func_idx].findex as u32);
        let result = self.interpret_loop(bc, native_resolver, func_idx);
        ash::profile::leave_interp(prev_findex);
        if let Some(f) = self.stack.pop() {
            if self.reg_pool.len() < POOL_CAP {
                self.reg_pool.push(f.into_buffer());
            }
        }
        self.sync_gc_scan_roots();
        result
    }

    /// Main opcode dispatch loop for a function.
    fn interpret_loop(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        func_idx: usize,
    ) -> Result<NanBoxedValue> {
        loop {
            let func = self.air.body(bytecode, func_idx);
            let frame = self.stack.last().unwrap();
            let pc = frame.pc;

            if pc >= func.ops.len() {
                return Ok(NanBoxedValue::void());
            }

            // Borrowed, not cloned. `Cache::body` hands back a reference tied
            // to `bytecode` rather than to `self`, so the dispatch loop never
            // needed a copy -- and `Opcode` carries a `Vec<Reg>` in its CallN,
            // CallMethod, CallThis, CallClosure and Switch variants, so the
            // copy was a heap allocation per dispatch on exactly those. The
            // sampler charged 2% of a whole nbody run to `Opcode::clone`.
            let op = &func.ops[pc];
            if env_flag!("ASH_TRACE_ASSERT") {
                eprintln!(
                    "[TRACE] f{} {} pc={} op={:?}",
                    func_idx,
                    func.name(),
                    pc,
                    op
                );
            }
            let result = self.execute_opcode(bytecode, op, func_idx)?;

            match result {
                StepResult::Continue => {
                    self.stack.last_mut().unwrap().pc += 1;
                }
                StepResult::Jump(offset) => {
                    // offset is relative to the NEXT instruction
                    let (next_pc, hot) = {
                        let frame = self.stack.last_mut().unwrap();
                        let next_pc = (frame.pc as i64) + 1 + (offset as i64);
                        let hot = if offset < 0 {
                            frame.backedges = frame.backedges.wrapping_add(1);
                            // Every 64th, not once. Promotion is driven by a
                            // count crossing a threshold, and there are two of
                            // them to cross -- Cranelift then LLVM -- so a
                            // single signal would stall the ladder at the
                            // first. The mask keeps the cost on the hot path
                            // to an add and a test.
                            frame.backedges & 63 == 0
                        } else {
                            false
                        };
                        frame.pc = next_pc as usize;
                        (next_pc as usize, hot)
                    };
                    if hot {
                        self.note_hot_loop(bytecode, func_idx, next_pc);
                        if let Some(ret) = self.try_osr_transfer(bytecode, func_idx, next_pc)? {
                            return Ok(ret);
                        }
                    }
                }
                StepResult::JumpAbs(target_pc) => {
                    self.stack.last_mut().unwrap().pc = target_pc;
                }
                StepResult::Return(value) => {
                    return Ok(value);
                }
                StepResult::Call {
                    findex,
                    mut args,
                    dst,
                } => {
                    if env_flag!("ASH_TRACE_NATIVE") {
                        let is_bc = func_of(&self.targets, findex).is_some();
                        let is_nat = native_of(&self.targets, findex).is_some();
                        let depth = self.stack.len();
                        eprintln!(
                            "[trace] call findex={} bc={} nat={} args={} depth={}",
                            findex,
                            is_bc,
                            is_nat,
                            args.len(),
                            depth
                        );
                        use std::io::Write;
                        std::io::stderr().flush().ok();
                    }
                    match self.call_function(bytecode, native_resolver, findex, &args) {
                        Ok(ret) => {
                            let dst_kind = bytecode.types[func.regs[dst as usize].0].kind;
                            let coerced = self.coerce_value_for_static_kind(ret, dst_kind);
                            self.stack.last_mut().unwrap().registers.set(dst, coerced);
                            self.stack.last_mut().unwrap().pc += 1;

                            // Check deferred hot-reload flag after native calls
                            if ash::reload::take_reload_pending() {
                                if let Some(new_bc) = ash::reload::do_reload() {
                                    // Leak the old utf16_strings cache — live NanBoxed registers
                                    // in the current (old) frame hold raw pointers into those
                                    // Vec<u16> buffers. Clearing would create dangling pointers.
                                    let old_cache = std::mem::take(&mut self.utf16_strings);
                                    Box::leak(Box::new(old_cache));

                                    // Pre-populate the new cache from the new bytecode's string
                                    // table. This ensures all Opcode::String hits return new
                                    // strings regardless of which bytecode ref interpret_loop holds.
                                    for (idx, s) in new_bc.strings.iter().enumerate() {
                                        let mut buf: Vec<u16> = s.encode_utf16().collect();
                                        buf.push(0);
                                        self.utf16_strings.insert(idx, buf);
                                    }

                                    self.field_hash_cache.clear();

                                    // Invalidate tiered JIT cache — compiled functions still
                                    // point to old code. Forces fallback to interpreter which
                                    // uses the new bytecode. Beads reload (Compiled →
                                    // Interpreted, will recompile); deopt'd beads are cleared
                                    // so the gate re-registers them fresh.
                                    if let Some(tiered) = self.tiered_runtime.as_mut() {
                                        for slot in tiered.entries.iter_mut() {
                                            *slot = None;
                                        }
                                        // Reset every bead to the interpreter
                                        // and clear all tier-promotion flags.
                                        for (findex, slot) in tiered.beads.iter_mut().enumerate() {
                                            let dead = slot
                                                .as_ref()
                                                .map(|b| {
                                                    b.reset_to_interpreter();
                                                    !b.bead().is_valid()
                                                })
                                                .unwrap_or(false);
                                            if dead {
                                                *slot = None;
                                                tiered.gate_checked[findex] = false;
                                            }
                                        }
                                        tiered
                                            .shared_ctx
                                            .llvm_done
                                            .lock()
                                            .expect("llvm_done mutex poisoned")
                                            .clear();
                                    }

                                    // Re-initialize constants from the new bytecode so that
                                    // globals (string literals, class descriptors) reflect V2.
                                    if let Err(e) = self.init_constants(&new_bc, native_resolver) {
                                        eprintln!(
                                            "[hot-reload] warning: init_constants failed: {}",
                                            e
                                        );
                                    }

                                    // Bodies optimized from V1 describe V1's
                                    // functions; a findex may not even be the
                                    // same function in V2.
                                    self.air.invalidate();
                                    self.ssa.invalidate();

                                    let leaked: &'static _ = Box::leak(Box::new(new_bc));
                                    self.reloaded_bytecode = Some(leaked);
                                }
                            }
                        }
                        Err(e) => {
                            if let Some(hl_exc) = e.downcast_ref::<HLExceptionPropagation>() {
                                let exc_val = hl_exc.value;
                                let frame = self.stack.last_mut().unwrap();
                                if let Some((target_pc, exc_reg)) = frame.trap_stack.pop() {
                                    frame.registers.set(exc_reg, exc_val);
                                    frame.pc = target_pc;
                                    // Continue from catch block (no pc increment)
                                } else {
                                    return Err(e);
                                }
                            } else {
                                return Err(e);
                            }
                        }
                    }
                }
            }
        }
    }

    /// Execute a single opcode. Returns what action the loop should take.
    fn execute_opcode(
        &mut self,
        bytecode: &DecodedBytecode,
        op: &Opcode,
        func_idx: usize,
    ) -> Result<StepResult> {
        let func = self.air.body(bytecode, func_idx);
        let frame = self.stack.last_mut().unwrap();

        match op {
            // ===== Movement / Constants =====
            Opcode::Mov { dst, src } => {
                let val = frame.registers.get(src.0);
                frame.registers.set(dst.0, val);
            }
            Opcode::Int { dst, ptr } => {
                let val = bytecode.ints[ptr.0];
                frame.registers.set(dst.0, NanBoxedValue::from_i32(val));
            }
            Opcode::Float { dst, ptr } => {
                let val = bytecode.floats[ptr.0];
                frame.registers.set(dst.0, NanBoxedValue::from_f64(val));
            }
            Opcode::Bool { dst, value } => {
                frame.registers.set(dst.0, NanBoxedValue::from_bool(*value));
            }
            Opcode::Bytes { dst, ptr } => {
                let pos = bytecode.bytes_pos[ptr.0];
                frame
                    .registers
                    .set(dst.0, NanBoxedValue::from_bytes_ptr(pos));
            }
            Opcode::String { dst, ptr } => {
                // HashLink uses UTF-16 strings internally.
                // Get or create a cached null-terminated UTF-16 version of the string.
                let utf16_ptr = if let Some(cached) = self.utf16_strings.get(&ptr.0) {
                    cached.as_ptr()
                } else {
                    let s = bytecode
                        .strings
                        .get(ptr.0)
                        .ok_or_else(|| anyhow!("String constant out of bounds: {}", ptr.0))?;
                    let mut buf: Vec<u16> = s.encode_utf16().collect();
                    buf.push(0);
                    self.utf16_strings.insert(ptr.0, buf);
                    self.utf16_strings[&ptr.0].as_ptr()
                };
                frame
                    .registers
                    .set(dst.0, NanBoxedValue::from_bytes_ptr(utf16_ptr as usize));
            }
            Opcode::Null { dst } => {
                frame.registers.set(dst.0, NanBoxedValue::null());
            }

            // ===== Arithmetic =====
            Opcode::Add { dst, a, b } => {
                let va = frame.registers.get(a.0);
                let vb = frame.registers.get(b.0);
                let result = if let Some(r) = va.binary_int_op(vb, IntBinOp::Add) {
                    r
                } else if let Some(r) = va.binary_float_op(vb, FloatBinOp::Add) {
                    r
                } else {
                    return Err(anyhow!(
                        "Add: incompatible types {:?} + {:?} in {} at pc={} (dst=r{}, a=r{}, b=r{})",
                        va,
                        vb,
                        func.name(),
                        frame.pc,
                        dst.0,
                        a.0,
                        b.0
                    ));
                };
                frame.registers.set(dst.0, result);
            }
            Opcode::Sub { dst, a, b } => {
                let va = frame.registers.get(a.0);
                let vb = frame.registers.get(b.0);
                let result = if let Some(r) = va.binary_int_op(vb, IntBinOp::Sub) {
                    r
                } else if let Some(r) = va.binary_float_op(vb, FloatBinOp::Sub) {
                    r
                } else {
                    return Err(anyhow!("Sub: incompatible types"));
                };
                frame.registers.set(dst.0, result);
            }
            Opcode::Mul { dst, a, b } => {
                let va = frame.registers.get(a.0);
                let vb = frame.registers.get(b.0);
                let result = if let Some(r) = va.binary_int_op(vb, IntBinOp::Mul) {
                    r
                } else if let Some(r) = va.binary_float_op(vb, FloatBinOp::Mul) {
                    r
                } else {
                    return Err(anyhow!("Mul: incompatible types"));
                };
                frame.registers.set(dst.0, result);
            }
            Opcode::SDiv { dst, a, b } => {
                let va = frame.registers.get(a.0);
                let vb = frame.registers.get(b.0);
                let result = if let Some(r) = va.binary_int_op(vb, IntBinOp::SDiv) {
                    r
                } else if let Some(r) = va.binary_float_op(vb, FloatBinOp::SDiv) {
                    r
                } else {
                    return Err(anyhow!("SDiv: incompatible types or div by zero"));
                };
                frame.registers.set(dst.0, result);
            }
            Opcode::UDiv { dst, a, b } => {
                let va = frame.registers.get(a.0);
                let vb = frame.registers.get(b.0);
                let result = va
                    .binary_int_op(vb, IntBinOp::UDiv)
                    .ok_or_else(|| anyhow!("UDiv: incompatible types or div by zero"))?;
                frame.registers.set(dst.0, result);
            }
            Opcode::SMod { dst, a, b } => {
                let va = frame.registers.get(a.0);
                let vb = frame.registers.get(b.0);
                let result = if let Some(r) = va.binary_int_op(vb, IntBinOp::SMod) {
                    r
                } else if let Some(r) = va.binary_float_op(vb, FloatBinOp::SMod) {
                    r
                } else {
                    return Err(anyhow!("SMod: incompatible types or div by zero"));
                };
                frame.registers.set(dst.0, result);
            }
            Opcode::UMod { dst, a, b } => {
                let va = frame.registers.get(a.0);
                let vb = frame.registers.get(b.0);
                // UMod only on integers
                let l = va.as_i32() as u32;
                let r = vb.as_i32() as u32;
                if r == 0 {
                    return Err(anyhow!("UMod: division by zero"));
                }
                frame
                    .registers
                    .set(dst.0, NanBoxedValue::from_i32((l % r) as i32));
            }
            Opcode::Shl { dst, a, b } => {
                self.int_binop(func, IntBinOp::Shl, dst.0, a.0, b.0)?;
            }
            Opcode::SShr { dst, a, b } => {
                self.int_binop(func, IntBinOp::SShr, dst.0, a.0, b.0)?;
            }
            Opcode::UShr { dst, a, b } => {
                self.int_binop(func, IntBinOp::UShr, dst.0, a.0, b.0)?;
            }
            Opcode::And { dst, a, b } => {
                self.int_binop(func, IntBinOp::And, dst.0, a.0, b.0)?;
            }
            Opcode::Or { dst, a, b } => {
                self.int_binop(func, IntBinOp::Or, dst.0, a.0, b.0)?;
            }
            Opcode::Xor { dst, a, b } => {
                self.int_binop(func, IntBinOp::Xor, dst.0, a.0, b.0)?;
            }
            Opcode::Neg { dst, src } => {
                let val = frame.registers.get(src.0);
                let result = if val.is_i32() {
                    NanBoxedValue::from_i32(val.as_i32().wrapping_neg())
                } else if val.is_f64() {
                    NanBoxedValue::from_f64(-val.as_f64())
                } else {
                    return Err(anyhow!("Neg: unsupported type {:?}", val));
                };
                frame.registers.set(dst.0, result);
            }
            Opcode::Not { dst, src } => {
                let val = frame.registers.get(src.0);
                let result = if val.is_i32() {
                    NanBoxedValue::from_i32(!val.as_i32())
                } else if val.is_bool() {
                    NanBoxedValue::from_bool(!val.as_bool())
                } else {
                    return Err(anyhow!("Not: unsupported type {:?}", val));
                };
                frame.registers.set(dst.0, result);
            }
            Opcode::Incr { dst } => {
                let val = frame.registers.get(dst.0);
                if val.is_i32() {
                    frame
                        .registers
                        .set(dst.0, NanBoxedValue::from_i32(val.as_i32().wrapping_add(1)));
                } else if val.is_f64() {
                    frame
                        .registers
                        .set(dst.0, NanBoxedValue::from_f64(val.as_f64() + 1.0));
                }
            }
            Opcode::Decr { dst } => {
                let val = frame.registers.get(dst.0);
                if val.is_i32() {
                    frame
                        .registers
                        .set(dst.0, NanBoxedValue::from_i32(val.as_i32().wrapping_sub(1)));
                } else if val.is_f64() {
                    frame
                        .registers
                        .set(dst.0, NanBoxedValue::from_f64(val.as_f64() - 1.0));
                }
            }

            // ===== Function Calls =====
            Opcode::Call0 { dst, fun } => {
                // An empty Vec never allocated, but taking one keeps every
                // Call arm symmetrical with the trampoline's reclaim.
                let mut args = self.arg_pool.pop().unwrap_or_default();
                args.clear();
                return Ok(StepResult::Call {
                    findex: fun.0,
                    args,
                    dst: dst.0,
                });
            }
            Opcode::Call1 { dst, fun, arg0 } => {
                let a0 = frame.registers.get(arg0.0);
                let mut args = self.arg_pool.pop().unwrap_or_default();
                args.clear();
                args.push(a0);
                return Ok(StepResult::Call {
                    findex: fun.0,
                    args,
                    dst: dst.0,
                });
            }
            Opcode::Call2 {
                dst,
                fun,
                arg0,
                arg1,
            } => {
                let a0 = frame.registers.get(arg0.0);
                let a1 = frame.registers.get(arg1.0);
                let mut args = self.arg_pool.pop().unwrap_or_default();
                args.clear();
                args.push(a0);
                args.push(a1);
                return Ok(StepResult::Call {
                    findex: fun.0,
                    args,
                    dst: dst.0,
                });
            }
            Opcode::Call3 {
                dst,
                fun,
                arg0,
                arg1,
                arg2,
            } => {
                let a0 = frame.registers.get(arg0.0);
                let a1 = frame.registers.get(arg1.0);
                let a2 = frame.registers.get(arg2.0);
                let mut args = self.arg_pool.pop().unwrap_or_default();
                args.clear();
                args.push(a0);
                args.push(a1);
                args.push(a2);
                return Ok(StepResult::Call {
                    findex: fun.0,
                    args,
                    dst: dst.0,
                });
            }
            Opcode::Call4 {
                dst,
                fun,
                arg0,
                arg1,
                arg2,
                arg3,
            } => {
                let a0 = frame.registers.get(arg0.0);
                let a1 = frame.registers.get(arg1.0);
                let a2 = frame.registers.get(arg2.0);
                let a3 = frame.registers.get(arg3.0);
                let mut args = self.arg_pool.pop().unwrap_or_default();
                args.clear();
                args.push(a0);
                args.push(a1);
                args.push(a2);
                args.push(a3);
                return Ok(StepResult::Call {
                    findex: fun.0,
                    args,
                    dst: dst.0,
                });
            }
            Opcode::CallN { dst, fun, args } => {
                let mut arg_vals = self.arg_pool.pop().unwrap_or_default();
                arg_vals.clear();
                arg_vals.extend(args.iter().map(|r| frame.registers.get(r.0)));
                return Ok(StepResult::Call {
                    findex: fun.0,
                    args: arg_vals,
                    dst: dst.0,
                });
            }
            Opcode::CallMethod { dst, field, args } | Opcode::CallThis { dst, field, args } => {
                return self.op_call_method(
                    bytecode,
                    func,
                    func_idx,
                    matches!(op, Opcode::CallThis { .. }),
                    dst.0,
                    field.0,
                    args,
                );
            }
            Opcode::CallClosure { dst, fun, args } => {
                return self.op_call_closure(bytecode, func, func_idx, dst.0, fun.0, args);
            }

            // ===== Closures =====
            Opcode::StaticClosure { dst, fun } => {
                return self.op_static_closure(bytecode, func, func_idx, dst.0, fun.0);
            }
            Opcode::InstanceClosure { dst, fun, obj } => {
                return self.op_instance_closure(bytecode, func, func_idx, dst.0, fun.0, obj.0);
            }
            Opcode::VirtualClosure { dst, obj, field } => {
                return self.op_virtual_closure(bytecode, func, func_idx, dst.0, obj.0, field.0);
            }

            // ===== Globals =====
            Opcode::GetGlobal { dst, global } => {
                let mut val = if global.0 < self.globals.len() {
                    self.globals[global.0]
                } else {
                    NanBoxedValue::null()
                };
                // If our NanBoxed store is null, check the shared globals_data array.
                // Native stdlib may have written to global_value slots (which point into
                // globals_data) without going through the interpreter's SetGlobal.
                if val.is_null() {
                    let (gd, nglobals) = self.c_type_factory.globals_data();
                    if !gd.is_null() && global.0 < nglobals {
                        let raw = unsafe { *gd.add(global.0) };
                        if !raw.is_null() {
                            val = NanBoxedValue::from_ptr(raw as usize);
                            // Cache it in our globals for future reads
                            self.globals[global.0] = val;
                        }
                    }
                }
                frame.registers.set(dst.0, val);
            }
            Opcode::SetGlobal { global, src } => {
                let val = frame.registers.get(src.0);
                if global.0 >= self.globals.len() {
                    self.globals.resize(global.0 + 1, NanBoxedValue::null());
                }
                self.globals[global.0] = val;
                // Also update globals_data so native code sees the new value
                let (gd, nglobals) = self.c_type_factory.globals_data();
                if !gd.is_null() && global.0 < nglobals {
                    unsafe {
                        *gd.add(global.0) = if val.is_null() || val.is_void() {
                            std::ptr::null_mut()
                        } else {
                            val.as_ptr() as *mut c_void
                        };
                    }
                }
            }

            // ===== Fields =====
            Opcode::Field { dst, obj, field } => {
                return self.op_field_get(bytecode, func, func_idx, dst.0, obj.0, field.0);
            }
            Opcode::GetThis { dst, field } => {
                let obj_type_idx = func.regs[0].0;
                let obj_kind = bytecode.types[obj_type_idx].kind;
                let obj_c_type = self.c_type_factory.get(obj_type_idx) as *mut c_void;
                let dst_kind = bytecode.types[func.regs[dst.0 as usize].0].kind;
                let get_rt = self.fn_get_obj_rt;
                let obj_val = frame.registers.get(0); // reg 0 is 'this'
                if obj_val.is_null() || obj_val.is_void() {
                    frame.registers.set(dst.0, NanBoxedValue::null());
                } else if obj_kind == hl::hl_type_kind_HOBJ || obj_kind == hl::hl_type_kind_HSTRUCT
                {
                    let obj_ptr = obj_val.as_ptr() as *mut u8;
                    let val = unsafe {
                        Self::read_obj_field(
                            obj_ptr, field.0, dst_kind, obj_c_type, obj_kind, get_rt,
                        )
                    };
                    if env_flag!("ASH_DBG_FIELD") {
                        eprintln!(
                            "[GETTHIS-OBJ] f{} pc={} obj_ty={} obj_kind={} field={} dst_kind={} -> {:?}",
                            func_idx, frame.pc, obj_type_idx, obj_kind, field.0, dst_kind, val
                        );
                    }
                    frame.registers.set(dst.0, val);
                } else if obj_kind == hl::hl_type_kind_HVIRTUAL {
                    if let Some(offset) =
                        unsafe { Self::resolve_virtual_field_offset(obj_c_type, field.0) }
                    {
                        let obj_ptr = obj_val.as_ptr() as *mut u8;
                        let addr = unsafe { obj_ptr.add(offset) };
                        let val = unsafe { Self::read_value_at(addr, dst_kind) };
                        if env_flag!("ASH_DBG_FIELD") {
                            eprintln!(
                                "[GETTHIS-VIRT] f{} pc={} obj_ty={} field={} off={} dst_kind={} -> {:?}",
                                func_idx, frame.pc, obj_type_idx, field.0, offset, dst_kind, val
                            );
                        }
                        frame.registers.set(dst.0, val);
                    } else {
                        let key = (obj_val.as_ptr(), field.0);
                        let val = if let Some(v) = self.virtual_fields.get(&key).copied() {
                            v
                        } else if let Some(hfield) =
                            Self::resolve_typed_field_hash(bytecode, obj_type_idx, field.0)
                        {
                            let dst_type_idx = func.regs[dst.0 as usize].0;
                            let dst_type_ptr = self.c_type_factory.get(dst_type_idx) as *mut c_void;
                            Self::dyn_get_field_by_hash(
                                obj_val.as_ptr() as *mut c_void,
                                hfield,
                                dst_kind,
                                dst_type_ptr,
                                self.fn_dyn_getd,
                                self.fn_dyn_getf,
                                self.fn_dyn_geti64,
                                self.fn_dyn_geti,
                                self.fn_dyn_getp,
                            )
                        } else {
                            NanBoxedValue::null()
                        };
                        if env_flag!("ASH_DBG_FIELD") {
                            eprintln!(
                                "[GETTHIS-VIRT-FALLBACK] f{} pc={} obj_ty={} field={} -> {:?}",
                                func_idx, frame.pc, obj_type_idx, field.0, val
                            );
                        }
                        frame.registers.set(dst.0, val);
                    }
                } else if let Some(hfield) =
                    Self::resolve_typed_field_hash(bytecode, obj_type_idx, field.0)
                {
                    let obj_ptr = obj_val.as_ptr() as *mut c_void;
                    let dst_type_idx = func.regs[dst.0 as usize].0;
                    let dst_type_ptr = self.c_type_factory.get(dst_type_idx) as *mut c_void;
                    let out = Self::dyn_get_field_by_hash(
                        obj_ptr,
                        hfield,
                        dst_kind,
                        dst_type_ptr,
                        self.fn_dyn_getd,
                        self.fn_dyn_getf,
                        self.fn_dyn_geti64,
                        self.fn_dyn_geti,
                        self.fn_dyn_getp,
                    );
                    frame.registers.set(dst.0, out);
                } else {
                    frame.registers.set(dst.0, NanBoxedValue::null());
                }
            }
            Opcode::SetField { obj, field, src } => {
                return self.op_field_set(bytecode, func, func_idx, obj.0, field.0, src.0);
            }
            Opcode::SetThis { field, src } => {
                let obj_type_idx = func.regs[0].0;
                let obj_kind = bytecode.types[obj_type_idx].kind;
                let obj_c_type = self.c_type_factory.get(obj_type_idx) as *mut c_void;
                let src_type_idx = func.regs[src.0 as usize].0;
                let src_kind = bytecode.types[src_type_idx].kind;
                let get_rt = self.fn_get_obj_rt;
                let obj_val = frame.registers.get(0); // reg 0 is 'this'
                if !obj_val.is_null() && !obj_val.is_void() {
                    let src_val = frame.registers.get(src.0);
                    if obj_kind == hl::hl_type_kind_HOBJ || obj_kind == hl::hl_type_kind_HSTRUCT {
                        let obj_ptr = obj_val.as_ptr() as *mut u8;
                        if env_flag!("ASH_DBG_FIELD") {
                            eprintln!(
                                "[SETTHIS-OBJ] f{} pc={} obj_ty={} obj_kind={} field={} src_kind={} src={:?}",
                                func_idx, frame.pc, obj_type_idx, obj_kind, field.0, src_kind, src_val
                            );
                        }
                        unsafe {
                            Self::write_obj_field(
                                obj_ptr, field.0, src_kind, src_val, obj_c_type, obj_kind, get_rt,
                            );
                        }
                    } else if obj_kind == hl::hl_type_kind_HVIRTUAL {
                        if let Some(offset) =
                            unsafe { Self::resolve_virtual_field_offset(obj_c_type, field.0) }
                        {
                            let obj_ptr = obj_val.as_ptr() as *mut u8;
                            let addr = unsafe { obj_ptr.add(offset) };
                            if env_flag!("ASH_DBG_FIELD") {
                                eprintln!(
                                    "[SETTHIS-VIRT] f{} pc={} obj_ty={} field={} off={} src_kind={} src={:?}",
                                    func_idx, frame.pc, obj_type_idx, field.0, offset, src_kind, src_val
                                );
                            }
                            unsafe { Self::write_value_at(addr, src_kind, src_val) };
                        } else {
                            self.virtual_fields
                                .insert((obj_val.as_ptr(), field.0), src_val);
                            if let Some(hfield) =
                                Self::resolve_typed_field_hash(bytecode, obj_type_idx, field.0)
                            {
                                let obj_ptr = obj_val.as_ptr() as *mut c_void;
                                let src_type_ptr =
                                    self.c_type_factory.get(src_type_idx) as *mut c_void;
                                Self::dyn_set_field_by_hash(
                                    obj_ptr,
                                    hfield,
                                    src_val,
                                    src_kind,
                                    src_type_ptr,
                                    self.fn_dyn_setd,
                                    self.fn_dyn_setf,
                                    self.fn_dyn_seti64,
                                    self.fn_dyn_seti,
                                    self.fn_dyn_setp,
                                );
                            }
                            if env_flag!("ASH_DBG_FIELD") {
                                eprintln!(
                                    "[SETTHIS-VIRT-FALLBACK] f{} pc={} obj_ty={} field={} src={:?}",
                                    func_idx, frame.pc, obj_type_idx, field.0, src_val
                                );
                            }
                        }
                    } else if let Some(hfield) =
                        Self::resolve_typed_field_hash(bytecode, obj_type_idx, field.0)
                    {
                        let obj_ptr = obj_val.as_ptr() as *mut c_void;
                        let src_type_ptr = self.c_type_factory.get(src_type_idx) as *mut c_void;
                        Self::dyn_set_field_by_hash(
                            obj_ptr,
                            hfield,
                            src_val,
                            src_kind,
                            src_type_ptr,
                            self.fn_dyn_setd,
                            self.fn_dyn_setf,
                            self.fn_dyn_seti64,
                            self.fn_dyn_seti,
                            self.fn_dyn_setp,
                        );
                    }
                }
            }
            Opcode::DynGet { dst, obj, field } => {
                return self.op_dyn_get(bytecode, func, func_idx, dst.0, obj.0, field.0);
            }
            Opcode::DynSet { obj, field, src } => {
                return self.op_dyn_set(bytecode, func, func_idx, obj.0, field.0, src.0);
            }

            // ===== Conditional Jumps =====
            Opcode::JTrue { cond, offset } => {
                let val = frame.registers.get(cond.0);
                if val.to_bool() {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JFalse { cond, offset } => {
                let val = frame.registers.get(cond.0);
                if !val.to_bool() {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JNull { reg, offset } => {
                let val = frame.registers.get(reg.0);
                if val.is_null() {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JNotNull { reg, offset } => {
                let val = frame.registers.get(reg.0);
                if !val.is_null() {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JSLt { a, b, offset } => {
                if self.compare_regs(bytecode, func_idx, a.0, b.0, CmpOp::SLt) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JSGte { a, b, offset } => {
                if self.compare_regs(bytecode, func_idx, a.0, b.0, CmpOp::SGte) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JSGt { a, b, offset } => {
                if self.compare_regs(bytecode, func_idx, a.0, b.0, CmpOp::SGt) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JSLte { a, b, offset } => {
                if self.compare_regs(bytecode, func_idx, a.0, b.0, CmpOp::SLte) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JULt { a, b, offset } => {
                if self.compare_regs(bytecode, func_idx, a.0, b.0, CmpOp::ULt) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JUGte { a, b, offset } => {
                if self.compare_regs(bytecode, func_idx, a.0, b.0, CmpOp::UGte) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JNotLt { a, b, offset } => {
                // JNotLt is equivalent to JGte (signed)
                if self.compare_regs(bytecode, func_idx, a.0, b.0, CmpOp::SGte) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JNotGte { a, b, offset } => {
                // JNotGte is equivalent to JLt (signed)
                if self.compare_regs(bytecode, func_idx, a.0, b.0, CmpOp::SLt) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JEq { a, b, offset } => {
                if self.compare_regs(bytecode, func_idx, a.0, b.0, CmpOp::Eq) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JNotEq { a, b, offset } => {
                if self.compare_regs(bytecode, func_idx, a.0, b.0, CmpOp::NotEq) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JAlways { offset } => {
                return Ok(StepResult::Jump(*offset));
            }

            // ===== Control Flow =====
            Opcode::Ret { ret } => {
                let val = frame.registers.get(ret.0);
                return Ok(StepResult::Return(val));
            }
            Opcode::Switch { reg, offsets, end } => {
                let val = frame.registers.get(reg.0);
                let index = val.as_i32();
                if index >= 0 && (index as usize) < offsets.len() {
                    return Ok(StepResult::Jump(offsets[index as usize]));
                } else {
                    // Out-of-range switch values fall through to the default arm,
                    // which is encoded as the next instruction in HL bytecode.
                    let _ = end;
                    return Ok(StepResult::Continue);
                }
            }
            Opcode::Label => {
                // No-op marker
            }
            Opcode::Nop => {}
            Opcode::Assert => {
                return Err(anyhow!("Assert hit at pc {}", frame.pc));
            }

            // ===== Type Operations =====
            Opcode::Type { dst, ty } => {
                // Store C-level hl_type pointer for native interop
                let c_type_ptr = self.c_type_factory.get(ty.0);
                frame
                    .registers
                    .set(dst.0, NanBoxedValue::from_ptr(c_type_ptr as usize));
            }
            Opcode::GetType { dst, src } => {
                // GetType returns the RUNTIME hl_type* of the value.
                // For reference types (HDYN, HOBJ, HSTRUCT, etc.) the hl_type*
                // is stored in the first 8 bytes of the pointed-to object.
                // For primitives, return the static C type pointer.
                let type_ref = &func.regs[src.0 as usize];
                let src_kind = bytecode.types[type_ref.0].kind;
                let val = frame.registers.get(src.0);

                let type_ptr: usize = if val.is_ptr() && !val.is_null() && val.as_ptr() != 0 {
                    match src_kind {
                        hl::hl_type_kind_HDYN
                        | hl::hl_type_kind_HOBJ
                        | hl::hl_type_kind_HSTRUCT
                        | hl::hl_type_kind_HVIRTUAL
                        | hl::hl_type_kind_HENUM
                        | hl::hl_type_kind_HDYNOBJ
                        | hl::hl_type_kind_HNULL => {
                            // First 8 bytes of object is the hl_type*
                            unsafe { *(val.as_ptr() as *const usize) }
                        }
                        _ => self.c_type_factory.get(type_ref.0) as usize,
                    }
                } else {
                    self.c_type_factory.get(type_ref.0) as usize
                };

                frame
                    .registers
                    .set(dst.0, NanBoxedValue::from_ptr(type_ptr));
            }
            Opcode::GetTID { dst, src } => {
                // GetTID returns the kind field of the hl_type* in src.
                // src should hold an hl_type* (result of GetType).
                // hl_type.kind is a u32 at offset 0.
                let val = frame.registers.get(src.0);
                let kind = if val.is_ptr() && !val.is_null() && val.as_ptr() != 0 {
                    unsafe { *(val.as_ptr() as *const u32) as i32 }
                } else {
                    // Fallback to static type kind
                    let type_ref = &func.regs[src.0 as usize];
                    bytecode.types[type_ref.0].kind as i32
                };
                frame.registers.set(dst.0, NanBoxedValue::from_i32(kind));
            }

            // ===== Casting =====
            Opcode::ToDyn { dst, src } => {
                return self.op_to_dyn(bytecode, func, func_idx, dst.0, src.0);
            }
            Opcode::ToSFloat { dst, src } => {
                let val = frame.registers.get(src.0);
                let f = if val.is_i32() {
                    val.as_i32() as f64
                } else {
                    val.as_f64()
                };
                frame.registers.set(dst.0, NanBoxedValue::from_f64(f));
            }
            Opcode::ToUFloat { dst, src } => {
                let val = frame.registers.get(src.0);
                let f = if val.is_i32() {
                    (val.as_i32() as u32) as f64
                } else {
                    val.as_f64()
                };
                frame.registers.set(dst.0, NanBoxedValue::from_f64(f));
            }
            Opcode::ToInt { dst, src } => {
                let val = frame.registers.get(src.0);
                let i = if val.is_f64() {
                    val.as_f64() as i32
                } else {
                    val.as_i32()
                };
                frame.registers.set(dst.0, NanBoxedValue::from_i32(i));
            }
            Opcode::SafeCast { dst, src } => {
                return self.op_safe_cast(bytecode, func, func_idx, dst.0, src.0);
            }
            Opcode::UnsafeCast { dst, src } => {
                let val = frame.registers.get(src.0);
                frame.registers.set(dst.0, val);
            }
            Opcode::ToVirtual { dst, src } => {
                // TODO: Convert object to virtual interface (Phase 3)
                let val = frame.registers.get(src.0);
                frame.registers.set(dst.0, val);
            }

            // ===== Object Creation =====
            Opcode::New { dst } => {
                return self.op_new(bytecode, func, func_idx, dst.0);
            }

            // ===== Array Operations =====
            Opcode::GetArray { dst, array, index } => {
                return self.op_get_array(bytecode, func, func_idx, dst.0, array.0, index.0);
            }
            Opcode::SetArray { array, index, src } => {
                return self.op_set_array(bytecode, func, func_idx, array.0, index.0, src.0);
            }
            Opcode::ArraySize { dst, array } => {
                // Read array size: varray layout has size (i32) at offset 16.
                // Only read if the register static type is HARRAY and value is non-null.
                let arr_type_kind = bytecode.types[func.regs[array.0 as usize].0].kind;
                let arr_val = frame.registers.get(array.0);
                let size = if arr_type_kind == hl::hl_type_kind_HARRAY
                    && !arr_val.is_null()
                    && !arr_val.is_void()
                {
                    let arr_ptr = arr_val.as_ptr() as *const u8;
                    // varray: t@0, at@8, size@16 (i32)
                    unsafe { *(arr_ptr.add(16) as *const i32) }
                } else {
                    0i32
                };
                frame.registers.set(dst.0, NanBoxedValue::from_i32(size));
            }

            // ===== Memory Access =====
            // bytes = base pointer, index = byte offset
            Opcode::GetI8 { dst, bytes, index } => {
                let base = frame.registers.get(bytes.0);
                let idx = frame.registers.get(index.0).as_i32();
                let val = if base.is_null() || base.is_void() || idx < 0 {
                    NanBoxedValue::from_i32(0)
                } else {
                    let addr = (base.as_ptr() as *const u8).wrapping_add(idx as usize);
                    NanBoxedValue::from_i32(unsafe { *(addr as *const u8) as i32 })
                };
                frame.registers.set(dst.0, val);
            }
            Opcode::GetI16 { dst, bytes, index } => {
                let base = frame.registers.get(bytes.0);
                let idx = frame.registers.get(index.0).as_i32();
                let val = if base.is_null() || base.is_void() || idx < 0 {
                    NanBoxedValue::from_i32(0)
                } else {
                    let addr = (base.as_ptr() as *const u8).wrapping_add(idx as usize);
                    NanBoxedValue::from_i32(unsafe { *(addr as *const u16) as i32 })
                };
                frame.registers.set(dst.0, val);
            }
            Opcode::GetMem { dst, bytes, index } => {
                let base = frame.registers.get(bytes.0);
                let idx = frame.registers.get(index.0).as_i32();
                let dst_kind = bytecode.types[func.regs[dst.0 as usize].0].kind;
                let val = if base.is_null() || base.is_void() || idx < 0 {
                    NanBoxedValue::from_i32(0)
                } else {
                    let addr = (base.as_ptr() as *const u8).wrapping_add(idx as usize);
                    Self::read_value_from_ptr(addr, dst_kind)
                };
                frame.registers.set(dst.0, val);
            }
            Opcode::SetI8 { bytes, index, src } => {
                let base = frame.registers.get(bytes.0);
                let idx = frame.registers.get(index.0).as_i32();
                let src_val = frame.registers.get(src.0);
                if !base.is_null() && !base.is_void() && idx >= 0 {
                    let addr = (base.as_ptr() as *mut u8).wrapping_add(idx as usize);
                    unsafe { *addr = src_val.as_i32() as u8 };
                }
            }
            Opcode::SetI16 { bytes, index, src } => {
                let base = frame.registers.get(bytes.0);
                let idx = frame.registers.get(index.0).as_i32();
                let src_val = frame.registers.get(src.0);
                if !base.is_null() && !base.is_void() && idx >= 0 {
                    let addr = (base.as_ptr() as *mut u8).wrapping_add(idx as usize);
                    unsafe { *(addr as *mut u16) = src_val.as_i32() as u16 };
                }
            }
            Opcode::SetMem { bytes, index, src } => {
                let base = frame.registers.get(bytes.0);
                let idx = frame.registers.get(index.0).as_i32();
                let src_val = frame.registers.get(src.0);
                let src_kind = bytecode.types[func.regs[src.0 as usize].0].kind;
                if !base.is_null() && !base.is_void() && idx >= 0 {
                    let addr = (base.as_ptr() as *mut u8).wrapping_add(idx as usize);
                    // Crash guard: check if address is accessible
                    if (addr as usize) < 0x1000 {
                        eprintln!(
                            "[CRASH GUARD] SetMem bad addr={:p} base={:?} idx={} in {} pc={}",
                            addr,
                            base,
                            idx,
                            func.name(),
                            0
                        );
                    } else {
                        Self::write_value_to_ptr(addr, src_val, src_kind);
                    }
                }
            }

            // ===== References =====
            Opcode::Ref { dst, src } => {
                // Store a pointer DIRECTLY to the src register's NanBoxedValue storage.
                //
                // HashLink semantics: native code writes through the ref pointer and the
                // source register is updated in-place (no Unref needed by bytecode).
                //
                // This works because NanBoxedValue is 8 bytes and, on little-endian,
                // the i32 payload occupies the low 4 bytes. A native c_int write of N
                // lands in bytes 0-3 → `reg.as_i32()` returns N immediately after return.
                //
                // The pointer is stable for the duration of the native call: pushing new
                // frames may move Vec<InterpreterFrame> but the inner Vec<NanBoxedValue>
                // data (separate heap allocation) does not move.
                let slot = frame.registers.slot_ptr(src.0) as usize;
                let ref_ptr = NanBoxedValue::from_ptr(slot);
                frame.registers.set(dst.0, ref_ptr);
            }
            Opcode::Unref { dst, src } => {
                // Dereference a Ref pointer to read back the value.
                let ptr_val = frame.registers.get(src.0);
                let ptr = ptr_val.as_ptr() as *const i64;
                if !ptr.is_null() {
                    let val = unsafe { *ptr };
                    let dst_kind = bytecode.types[func.regs[dst.0 as usize].0].kind;
                    let result = match dst_kind {
                        hl::hl_type_kind_HI32 | hl::hl_type_kind_HUI8 | hl::hl_type_kind_HUI16 => {
                            NanBoxedValue::from_i32(val as i32)
                        }
                        hl::hl_type_kind_HF64 | hl::hl_type_kind_HF32 => {
                            NanBoxedValue::from_f64(f64::from_bits(val as u64))
                        }
                        // Read low 32 bits: native writes a c_int (0/1) at byte offset 0.
                        // Must NOT check the full i64 because NAN_TAG bits are always nonzero.
                        hl::hl_type_kind_HBOOL => NanBoxedValue::from_bool((val as i32) != 0),
                        _ => NanBoxedValue::from_ptr(val as usize),
                    };
                    frame.registers.set(dst.0, result);
                } else {
                    frame.registers.set(dst.0, NanBoxedValue::null());
                }
            }
            Opcode::Setref { dst, value } => {
                // Write through a Ref pointer: *dst = value.
                // The pointer is the address of another register's NanBoxedValue storage.
                // Write the full NanBoxedValue so the tag bits are preserved.
                let ptr_val = frame.registers.get(dst.0);
                let ptr = ptr_val.as_ptr() as *mut NanBoxedValue;
                if !ptr.is_null() {
                    let val = frame.registers.get(value.0);
                    unsafe { *ptr = val };
                }
            }

            // ===== Enums =====
            Opcode::MakeEnum {
                dst,
                construct,
                args,
            } => {
                let type_idx = func.regs[dst.0 as usize].0;
                let c_type_ptr = self.c_type_factory.get(type_idx);
                let fn_alloc_enum = self.fn_alloc_enum;
                let val = Self::alloc_enum_value(fn_alloc_enum, c_type_ptr, construct.0 as i32);
                if !val.is_null() {
                    // Write each argument at its construct offset
                    unsafe {
                        let tenum = (*c_type_ptr).__bindgen_anon_1.tenum;
                        let c = &*(*tenum).constructs.add(construct.0);
                        let base = val as *mut u8;
                        for (i, arg_reg) in args.iter().enumerate() {
                            if i >= c.nparams as usize {
                                break;
                            }
                            let offset = *c.offsets.add(i) as usize;
                            let arg_val = frame.registers.get(arg_reg.0);
                            let param_kind = (*(*c.params.add(i))).kind;
                            Self::write_value_to_ptr(base.add(offset), arg_val, param_kind);
                        }
                    }
                }
                frame.registers.set(
                    dst.0,
                    if val.is_null() {
                        NanBoxedValue::null()
                    } else {
                        NanBoxedValue::from_ptr(val as usize)
                    },
                );
            }
            Opcode::EnumAlloc { dst, construct } => {
                let type_idx = func.regs[dst.0 as usize].0;
                let c_type_ptr = self.c_type_factory.get(type_idx);
                let fn_alloc_enum = self.fn_alloc_enum;
                let val = Self::alloc_enum_value(fn_alloc_enum, c_type_ptr, construct.0 as i32);
                frame.registers.set(
                    dst.0,
                    if val.is_null() {
                        NanBoxedValue::null()
                    } else {
                        NanBoxedValue::from_ptr(val as usize)
                    },
                );
            }
            Opcode::EnumIndex { dst, value } => {
                let val = frame.registers.get(value.0);
                let index = if val.is_null() || val.is_void() {
                    0i32
                } else {
                    // venum layout: t@0 (8 bytes), index@8 (i32)
                    unsafe { *(val.as_ptr() as *const u8).add(8).cast::<i32>() }
                };
                frame.registers.set(dst.0, NanBoxedValue::from_i32(index));
            }
            Opcode::EnumField {
                dst,
                value,
                construct,
                field,
            } => {
                let val = frame.registers.get(value.0);
                let type_idx = func.regs[value.0 as usize].0;
                let c_type_ptr = self.c_type_factory.get(type_idx);
                let result = if val.is_null() || val.is_void() || c_type_ptr.is_null() {
                    NanBoxedValue::null()
                } else {
                    unsafe {
                        let tenum = (*c_type_ptr).__bindgen_anon_1.tenum;
                        if tenum.is_null() || construct.0 >= (*tenum).nconstructs as usize {
                            NanBoxedValue::null()
                        } else {
                            let c = &*(*tenum).constructs.add(construct.0);
                            if field.0 >= c.nparams as usize {
                                NanBoxedValue::null()
                            } else {
                                let offset = *c.offsets.add(field.0) as usize;
                                let param_kind = (*(*c.params.add(field.0))).kind;
                                let base = val.as_ptr() as *const u8;
                                Self::read_value_from_ptr(base.add(offset), param_kind)
                            }
                        }
                    }
                };
                frame.registers.set(dst.0, result);
            }
            Opcode::SetEnumField { value, field, src } => {
                let val = frame.registers.get(value.0);
                let src_val = frame.registers.get(src.0);
                let type_idx = func.regs[value.0 as usize].0;
                let c_type_ptr = self.c_type_factory.get(type_idx);
                if !val.is_null() && !val.is_void() && !c_type_ptr.is_null() {
                    unsafe {
                        let tenum = (*c_type_ptr).__bindgen_anon_1.tenum;
                        if !tenum.is_null() {
                            // Get construct index from the actual venum value
                            let construct_idx =
                                *(val.as_ptr() as *const u8).add(8).cast::<i32>() as usize;
                            if construct_idx < (*tenum).nconstructs as usize {
                                let c = &*(*tenum).constructs.add(construct_idx);
                                if field.0 < c.nparams as usize {
                                    let offset = *c.offsets.add(field.0) as usize;
                                    let param_kind = (*(*c.params.add(field.0))).kind;
                                    let base = val.as_ptr() as *mut u8;
                                    Self::write_value_to_ptr(base.add(offset), src_val, param_kind);
                                }
                            }
                        }
                    }
                }
            }

            // ===== Exception Handling =====
            Opcode::Trap { exc, offset } => {
                let target_pc = (frame.pc as i64 + 1 + *offset as i64) as usize;
                frame.trap_stack.push((target_pc, exc.0));
            }
            // The operand is Haxe's `OEndTrap of bool` — a flag, not a
            // register, which is why the JIT ignores it too. Clearing
            // `registers[exc]` nulled whichever local happened to live in r0
            // or r1, so a `try` that assigned and then exited normally lost
            // the assignment.
            Opcode::EndTrap { exc: _ } => {
                frame.trap_stack.pop();
            }
            Opcode::Throw { exc } => {
                let val = frame.registers.get(exc.0);
                if let Some((target_pc, exc_reg)) = frame.trap_stack.pop() {
                    frame.registers.set(exc_reg, val);
                    return Ok(StepResult::JumpAbs(target_pc));
                } else {
                    return Err(anyhow::Error::new(self.format_hl_exception(val)));
                }
            }
            Opcode::Rethrow { exc } => {
                let val = frame.registers.get(exc.0);
                if let Some((target_pc, exc_reg)) = frame.trap_stack.pop() {
                    frame.registers.set(exc_reg, val);
                    return Ok(StepResult::JumpAbs(target_pc));
                } else {
                    return Err(anyhow::Error::new(self.format_hl_exception(val)));
                }
            }
            Opcode::NullCheck { reg } => {
                let val = frame.registers.get(reg.0);
                if val.is_null() {
                    // Throw as an HL exception (like HashLink does) so it can
                    // be caught by a Trap in the call stack.
                    return Err(anyhow::Error::new(HLExceptionPropagation {
                        value: NanBoxedValue::null(),
                        message: Some("Null access".to_string()),
                    }));
                }
            }

            // ===== Misc =====
            Opcode::RefData { dst, src } => {
                let val = frame.registers.get(src.0);
                frame.registers.set(dst.0, val);
            }
            Opcode::RefOffset { dst, reg, offset } => {
                let base = frame.registers.get(reg.0);
                let off = frame.registers.get(offset.0);
                let result = NanBoxedValue::from_ptr(base.as_ptr() + off.as_i32() as usize);
                frame.registers.set(dst.0, result);
            }
            Opcode::Prefetch { .. } => {
                // No-op on interpreter
            }
            Opcode::Asm { .. } => {
                // No-op on interpreter (x86 specific)
            }
            // IndirectCall: same as CallN in the interpreter (dispatch is always by findex)
            Opcode::IndirectCall { dst, fun, args } => {
                let arg_vals: Vec<NanBoxedValue> =
                    args.iter().map(|r| frame.registers.get(r.0)).collect();
                return Ok(StepResult::Call {
                    findex: fun.0,
                    args: arg_vals,
                    dst: dst.0,
                });
            }
        }

        Ok(StepResult::Continue)
    }

    /// Helper: perform integer binary op on two registers.
    /// Write an array element.
    ///
    /// Extracted from `execute_opcode` so the SSA dispatcher in
    /// [`crate::ssa`] runs the same semantics rather than a copy of them:
    /// register operands are plain indices into the active frame, which is
    /// the SSA value frame there and the HL register file here.
    #[allow(clippy::too_many_arguments, unused_variables)]
    fn op_set_array(
        &mut self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        array: u32,
        index: u32,
        src: u32,
    ) -> Result<StepResult> {
        let frame = self.stack.last_mut().unwrap();
        let arr_val = frame.registers.get(array);
        let idx = frame.registers.get(index).as_i32().max(0) as usize;
        let src_val = frame.registers.get(src);
        if !arr_val.is_null() && !arr_val.is_void() {
            if !arr_val.is_ptr() {
                return Err(anyhow!(
                        "SetArray: array reg r{} is not pointer in {} at pc={} (val={:?}, type_kind={})",
                        array,
                        func.name(),
                        frame.pc,
                        arr_val,
                        bytecode.types[func.regs[array as usize].0].kind
                    ));
            }
            let arr_ptr = arr_val.as_ptr() as *mut u8;
            unsafe {
                let size = *(arr_ptr.add(16) as *const i32);
                if idx >= size.max(0) as usize {
                    return Err(anyhow!(
                            "SetArray: index {} out of bounds (size={}) in {} at pc={} (arr=r{} val={:?} src={:?})",
                            idx,
                            size,
                            func.name(),
                            frame.pc,
                            array,
                            arr_val,
                            src_val
                        ));
                }
                let at = *(arr_ptr.add(8) as *const *mut hl_type);
                if !at.is_null() && !(at as usize).is_multiple_of(std::mem::align_of::<hl_type>()) {
                    return Err(anyhow!(
                            "SetArray: invalid at pointer {:p} in {} at pc={} (arr=r{} val={:?} idx={} src={:?} r4={:?} r6={:?} r16={:?})",
                            at,
                            func.name(),
                            frame.pc,
                            array,
                            arr_val,
                            idx,
                            src_val,
                            frame.registers.get(4),
                            frame.registers.get(6),
                            frame.registers.get(16)
                        ));
                }
                let at_kind = if at.is_null() {
                    hl::hl_type_kind_HDYN
                } else {
                    (*at).kind
                };
                let data = arr_ptr.add(24);
                match at_kind {
                    k if k == hl::hl_type_kind_HUI8 => *data.add(idx) = src_val.as_i32() as u8,
                    k if k == hl::hl_type_kind_HUI16 => {
                        *(data.add(idx * 2) as *mut u16) = src_val.as_i32() as u16
                    }
                    k if k == hl::hl_type_kind_HBOOL => {
                        *(data.add(idx * 2) as *mut u16) = src_val.as_bool() as u16
                    }
                    k if k == hl::hl_type_kind_HI32 => {
                        *(data.add(idx * 4) as *mut i32) = src_val.as_i32()
                    }
                    k if k == hl::hl_type_kind_HI64 => {
                        *(data.add(idx * 8) as *mut i64) = src_val.as_i64_lossy()
                    }
                    k if k == hl::hl_type_kind_HF32 => {
                        *(data.add(idx * 4) as *mut f32) = src_val.as_f64() as f32
                    }
                    k if k == hl::hl_type_kind_HF64 => {
                        *(data.add(idx * 8) as *mut f64) = src_val.as_f64()
                    }
                    k => {
                        let ptr_val = if src_val.is_null() || src_val.is_void() {
                            0usize
                        } else if (k == hl::hl_type_kind_HDYN || k == hl::hl_type_kind_HNULL)
                            && !src_val.is_ptr()
                        {
                            // Arrays of dyn/null store vdynamic*. Box primitives before write.
                            let src_type_idx = func.regs[src as usize].0;
                            let src_t = self.c_type_factory.get(src_type_idx);
                            let boxed = self.box_value_as_dynamic_with_type(src_val, src_t);
                            if boxed.is_null() || boxed.is_void() {
                                0usize
                            } else {
                                boxed.as_ptr()
                            }
                        } else {
                            src_val.as_ptr()
                        };
                        *(data.add(idx * 8) as *mut usize) = ptr_val;
                    }
                }
            }
        }

        Ok(StepResult::Continue)
    }

    /// Read an array element.
    ///
    /// Extracted from `execute_opcode` so the SSA dispatcher in
    /// [`crate::ssa`] runs the same semantics rather than a copy of them:
    /// register operands are plain indices into the active frame, which is
    /// the SSA value frame there and the HL register file here.
    #[allow(clippy::too_many_arguments, unused_variables)]
    fn op_get_array(
        &mut self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        dst: u32,
        array: u32,
        index: u32,
    ) -> Result<StepResult> {
        let frame = self.stack.last_mut().unwrap();
        let arr_val = frame.registers.get(array);
        let idx = frame.registers.get(index).as_i32().max(0) as usize;
        let dst_kind = bytecode.types[func.regs[dst as usize].0].kind;
        let val = if arr_val.is_null() || arr_val.is_void() {
            NanBoxedValue::null()
        } else if !arr_val.is_ptr() {
            return Err(anyhow!(
                "GetArray: array reg r{} is not pointer in {} at pc={} (val={:?}, type_kind={})",
                array,
                func.name(),
                frame.pc,
                arr_val,
                bytecode.types[func.regs[array as usize].0].kind
            ));
        } else {
            // varray: t@0, at@8, size@16, data@24
            let arr_ptr = arr_val.as_ptr() as *const u8;
            if (arr_ptr as usize) < 0x1000
                || (arr_ptr as usize) % std::mem::align_of::<usize>() != 0
            {
                static BAD_ARR_COUNT: std::sync::atomic::AtomicU32 =
                    std::sync::atomic::AtomicU32::new(0);
                let c = BAD_ARR_COUNT.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                if c == 0 || c == 100 || c == 10000 {
                    eprintln!(
                        "[WARN] GetArray invalid ptr={:#x} count={} in {} pc={}",
                        arr_ptr as usize,
                        c + 1,
                        func.name(),
                        frame.pc
                    );
                }
                frame.registers.set(dst, NanBoxedValue::null());
                self.stack.last_mut().unwrap().pc += 1;
                return Ok(StepResult::Continue);
            }
            unsafe {
                let size = *(arr_ptr.add(16) as *const i32);
                if idx >= size.max(0) as usize {
                    return Err(anyhow!(
                            "GetArray: index {} out of bounds (size={}) in {} at pc={} arr=r{} val={:?}",
                            idx,
                            size,
                            func.name(),
                            frame.pc,
                            array,
                            arr_val
                        ));
                }
                let at = *(arr_ptr.add(8) as *const *mut hl_type);
                if !at.is_null() && !(at as usize).is_multiple_of(std::mem::align_of::<hl_type>()) {
                    return Err(anyhow!(
                            "GetArray: invalid at pointer {:p} in {} at pc={} (arr=r{} val={:?} idx={} r4={:?} r6={:?} r16={:?})",
                            at,
                            func.name(),
                            frame.pc,
                            array,
                            arr_val,
                            idx,
                            frame.registers.get(4),
                            frame.registers.get(6),
                            frame.registers.get(16)
                        ));
                }
                let at_kind = if at.is_null() {
                    hl::hl_type_kind_HDYN
                } else {
                    (*at).kind
                };
                let data = arr_ptr.add(24);
                match at_kind {
                    k if k == hl::hl_type_kind_HUI8 => {
                        NanBoxedValue::from_i32(*data.add(idx) as i32)
                    }
                    k if k == hl::hl_type_kind_HUI16 => {
                        NanBoxedValue::from_i32(*(data.add(idx * 2) as *const u16) as i32)
                    }
                    k if k == hl::hl_type_kind_HBOOL => {
                        NanBoxedValue::from_bool(*(data.add(idx * 2) as *const u16) != 0)
                    }
                    k if k == hl::hl_type_kind_HI32 => {
                        NanBoxedValue::from_i32(*(data.add(idx * 4) as *const i32))
                    }
                    k if k == hl::hl_type_kind_HI64 => {
                        NanBoxedValue::from_i64(*(data.add(idx * 8) as *const i64))
                    }
                    k if k == hl::hl_type_kind_HF32 => {
                        NanBoxedValue::from_f64(*(data.add(idx * 4) as *const f32) as f64)
                    }
                    k if k == hl::hl_type_kind_HF64 => {
                        NanBoxedValue::from_f64(*(data.add(idx * 8) as *const f64))
                    }
                    k => {
                        let ptr_val = *(data.add(idx * 8) as *const usize);
                        if ptr_val == 0 {
                            match dst_kind {
                                hl::hl_type_kind_HI32
                                | hl::hl_type_kind_HUI8
                                | hl::hl_type_kind_HUI16 => NanBoxedValue::from_i32(0),
                                hl::hl_type_kind_HI64 => NanBoxedValue::from_i64(0),
                                hl::hl_type_kind_HF32 | hl::hl_type_kind_HF64 => {
                                    NanBoxedValue::from_f64(0.0)
                                }
                                hl::hl_type_kind_HBOOL => NanBoxedValue::from_bool(false),
                                _ => NanBoxedValue::null(),
                            }
                        } else if (k == hl::hl_type_kind_HDYN || k == hl::hl_type_kind_HNULL)
                            && Self::is_primitive_or_bytes_kind(dst_kind)
                        {
                            Self::unbox_dynamic_to_kind(ptr_val as *mut hl::vdynamic, dst_kind)
                                .unwrap_or_else(|| NanBoxedValue::from_ptr(ptr_val))
                        } else {
                            NanBoxedValue::from_ptr(ptr_val)
                        }
                    }
                }
            }
        };
        frame.registers.set(dst, val);

        Ok(StepResult::Continue)
    }

    /// Allocate a value of the destination's type.
    ///
    /// Extracted from `execute_opcode` so the SSA dispatcher in
    /// [`crate::ssa`] runs the same semantics rather than a copy of them:
    /// register operands are plain indices into the active frame, which is
    /// the SSA value frame there and the HL register file here.
    #[allow(clippy::too_many_arguments, unused_variables)]
    fn op_new(
        &mut self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        dst: u32,
    ) -> Result<StepResult> {
        let frame = self.stack.last_mut().unwrap();
        let type_idx = func.regs[dst as usize].0;
        let type_kind = bytecode.types[type_idx].kind;
        let c_type_ptr = self.c_type_factory.get(type_idx);

        let obj = match type_kind {
            hl::hl_type_kind_HOBJ | hl::hl_type_kind_HSTRUCT => {
                if c_type_ptr.is_null() || self.fn_alloc_obj.is_null() {
                    std::ptr::null_mut()
                } else {
                    let f: FnAllocObj = unsafe { std::mem::transmute(self.fn_alloc_obj) };
                    unsafe { f(c_type_ptr as *mut c_void) }
                }
            }
            hl::hl_type_kind_HDYNOBJ => {
                if self.fn_alloc_dynobj.is_null() {
                    std::ptr::null_mut()
                } else {
                    let f: FnAllocDynObj = unsafe { std::mem::transmute(self.fn_alloc_dynobj) };
                    unsafe { f() }
                }
            }
            hl::hl_type_kind_HVIRTUAL => {
                if c_type_ptr.is_null() || self.fn_alloc_virtual.is_null() {
                    std::ptr::null_mut()
                } else {
                    let f: FnAllocVirtual = unsafe { std::mem::transmute(self.fn_alloc_virtual) };
                    unsafe { f(c_type_ptr as *mut c_void) }
                }
            }
            _ => std::ptr::null_mut(),
        };

        if obj.is_null() {
            frame.registers.set(dst, NanBoxedValue::null());
        } else {
            frame
                .registers
                .set(dst, NanBoxedValue::from_ptr(obj as usize));
        }

        Ok(StepResult::Continue)
    }

    /// Checked cast, unboxing nullables and validating object hierarchies.
    ///
    /// Extracted from `execute_opcode` so the SSA dispatcher in
    /// [`crate::ssa`] runs the same semantics rather than a copy of them:
    /// register operands are plain indices into the active frame, which is
    /// the SSA value frame there and the HL register file here.
    #[allow(clippy::too_many_arguments, unused_variables)]
    fn op_safe_cast(
        &mut self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        dst: u32,
        src: u32,
    ) -> Result<StepResult> {
        let frame = self.stack.last_mut().unwrap();
        let val = frame.registers.get(src);
        let dst_type_idx = func.regs[dst as usize].0;
        let dst_kind = bytecode.types[dst_type_idx].kind;

        let result = if val.is_null() || val.is_void() {
            match dst_kind {
                hl::hl_type_kind_HI32 | hl::hl_type_kind_HUI8 | hl::hl_type_kind_HUI16 => {
                    NanBoxedValue::from_i32(0)
                }
                hl::hl_type_kind_HI64 => NanBoxedValue::from_i64(0),
                hl::hl_type_kind_HF32 | hl::hl_type_kind_HF64 => NanBoxedValue::from_f64(0.0),
                hl::hl_type_kind_HBOOL => NanBoxedValue::from_bool(false),
                _ => val,
            }
        } else if val.is_ptr() && val.as_ptr() != 0 {
            if Self::is_unboxable_primitive_kind(dst_kind) {
                // Primitive destination: unbox from vdynamic
                unsafe {
                    Self::unbox_dynamic_to_kind(val.as_ptr() as *mut hl::vdynamic, dst_kind)
                        .unwrap_or(val)
                }
            } else {
                let src_type_idx = func.regs[src as usize].0;
                let src_kind = bytecode.types[src_type_idx].kind;

                if (src_kind == hl::hl_type_kind_HDYN || src_kind == hl::hl_type_kind_HNULL)
                    && !self.fn_dyn_castp.is_null()
                {
                    // HDYN/HNULL → concrete type: use hlp_dyn_castp
                    let src_c_type = self.c_type_factory.get(src_type_idx) as *mut c_void;
                    let dst_c_type = self.c_type_factory.get(dst_type_idx) as *mut c_void;
                    type FnCastp =
                        unsafe extern "C" fn(*mut c_void, *mut c_void, *mut c_void) -> *mut c_void;
                    let castp: FnCastp = unsafe { std::mem::transmute(self.fn_dyn_castp) };
                    let mut data = val.as_ptr() as *mut c_void;
                    let result_ptr = unsafe {
                        castp(&mut data as *mut _ as *mut c_void, src_c_type, dst_c_type)
                    };
                    if result_ptr.is_null() {
                        NanBoxedValue::null()
                    } else {
                        NanBoxedValue::from_ptr(result_ptr as usize)
                    }
                } else {
                    // For HOBJ→HOBJ: call hlp_dyn_castp for type-safe cast
                    // (validates supertype chain, returns null on mismatch).
                    // For other pointer casts: plain copy.
                    let src_type_idx = func.regs[src as usize].0;
                    let src_kind = bytecode.types[src_type_idx].kind;

                    {
                        // Debug: trace HOBJ→HOBJ super chain
                        if src_kind == hl::hl_type_kind_HOBJ
                            && dst_kind == hl::hl_type_kind_HOBJ
                            && val.as_ptr() > 0x10000
                            && env_flag!("ASH_DBG_CAST")
                        {
                            static CAST_COUNT: std::sync::atomic::AtomicU32 =
                                std::sync::atomic::AtomicU32::new(0);
                            let c = CAST_COUNT.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                            if c >= 9 && c < 12 {
                                // trace casts #9+
                                let obj_ptr = val.as_ptr() as *const hl::vdynamic;
                                let header_t = unsafe { (*obj_ptr).t };
                                let dst_c = self.c_type_factory.get(dst_type_idx);
                                eprintln!(
                                        "[SafeCast-HOBJ#{}] src_tidx={} dst_tidx={} header={:p} dst_c={:p}",
                                        c, src_type_idx, dst_type_idx, header_t, dst_c
                                    );
                                if !header_t.is_null() && (header_t as usize) >= 0x10000 {
                                    unsafe {
                                        let mut cur = header_t;
                                        for d in 0..8 {
                                            if cur.is_null() || (cur as usize) < 0x10000 {
                                                break;
                                            }
                                            let k = (*cur).kind;
                                            if k != hl::hl_type_kind_HOBJ {
                                                eprintln!("  [{d}] kind={k} (not HOBJ)");
                                                break;
                                            }
                                            let obj = (*cur).__bindgen_anon_1.obj;
                                            if obj.is_null() || (obj as usize) < 0x10000 {
                                                eprintln!("  [{d}] obj={obj:p} (invalid)");
                                                break;
                                            }
                                            let name_ptr = (*obj).name;
                                            let name = if !name_ptr.is_null()
                                                && (name_ptr as usize) > 0x10000
                                            {
                                                let mut len = 0;
                                                while *name_ptr.add(len) != 0 && len < 100 {
                                                    len += 1;
                                                }
                                                String::from_utf16_lossy(
                                                    std::slice::from_raw_parts(name_ptr, len),
                                                )
                                            } else {
                                                "?".into()
                                            };
                                            let sup = (*obj).super_;
                                            eprintln!("  [{d}] type={cur:p} obj={obj:p} name={name} super={sup:p}");
                                            if sup.is_null() || (sup as usize) < 0x10000 {
                                                break;
                                            }
                                            cur = sup;
                                        }
                                    }
                                }
                            }
                        }
                        // For HOBJ→HOBJ SafeCast: check if source has __cast proto.
                        // In the interpreter, castFun can't be called (it's a stub
                        // pointer), so we call the __cast bytecode function directly.
                        if src_kind == hl::hl_type_kind_HOBJ && dst_kind == hl::hl_type_kind_HOBJ {
                            // Look up __cast proto findex from the object's runtime type
                            let obj_ptr = val.as_ptr() as *const hl::vdynamic;
                            let header_t = unsafe { (*obj_ptr).t };
                            let cast_findex = if !header_t.is_null()
                                && (header_t as usize) >= 0x10000
                                && unsafe { (*header_t).kind } == hl::hl_type_kind_HOBJ
                            {
                                unsafe {
                                    let obj_t = (*header_t).__bindgen_anon_1.obj;
                                    if !obj_t.is_null() && (obj_t as usize) >= 0x10000 {
                                        // Hash "__cast" using same algorithm as hlp_hash_gen
                                        let cast_hash = {
                                            let chars: &[u16] =
                                                &[0x5F, 0x5F, 0x63, 0x61, 0x73, 0x74]; // __cast
                                            let mut h: i32 = 0;
                                            for &c in chars {
                                                h = h.wrapping_mul(223).wrapping_add(c as i32);
                                            }
                                            h.wrapping_rem(0x1FFFFF7B)
                                        };
                                        // Search proto array in hl_type_obj
                                        let mut found: Option<usize> = None;
                                        let nproto = (*obj_t).nproto;
                                        let proto_ptr = (*obj_t).proto;
                                        if !proto_ptr.is_null() && (proto_ptr as usize) >= 0x10000 {
                                            for i in 0..nproto as usize {
                                                let proto = &*proto_ptr.add(i);
                                                if proto.hashed_name == cast_hash {
                                                    found = Some(proto.findex as usize);
                                                    break;
                                                }
                                            }
                                        }
                                        found
                                    } else {
                                        None
                                    }
                                }
                            } else {
                                None
                            };

                            if let Some(findex) = cast_findex {
                                // Call __cast(obj, dst_type) via StepResult::Call
                                let dst_c_type = self.c_type_factory.get(dst_type_idx);
                                let type_val = NanBoxedValue::from_ptr(dst_c_type as usize);
                                // Store args in registers and dispatch as a call
                                frame.registers.set(dst, val); // temp: store obj in dst
                                return Ok(StepResult::Call {
                                    findex,
                                    args: vec![val, type_val],
                                    dst: dst,
                                });
                            } else {
                                val // no __cast, just copy
                            }
                        } else {
                            val // non-HOBJ cast, just copy
                        }
                    }
                }
            }
        } else {
            val
        };
        frame.registers.set(dst, result);

        Ok(StepResult::Continue)
    }

    /// Box a value into a vdynamic for native consumption.
    ///
    /// Extracted from `execute_opcode` so the SSA dispatcher in
    /// [`crate::ssa`] runs the same semantics rather than a copy of them:
    /// register operands are plain indices into the active frame, which is
    /// the SSA value frame there and the HL register file here.
    #[allow(clippy::too_many_arguments, unused_variables)]
    fn op_to_dyn(
        &mut self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        dst: u32,
        src: u32,
    ) -> Result<StepResult> {
        let frame = self.stack.last_mut().unwrap();
        // Box a value into a vdynamic* for native code consumption.
        // Pointer types (HOBJ, HDYN, etc.) already have a vdynamic header - pass through.
        // Primitive types (HI32, HF64, HBOOL, HBYTES) need hlp_make_dyn wrapping.
        let src_type_ref = &func.regs[src as usize];
        let src_kind = bytecode.types[src_type_ref.0].kind;
        let val = frame.registers.get(src);

        let needs_boxing = matches!(
            src_kind,
            hl::hl_type_kind_HI32
                | hl::hl_type_kind_HI64
                | hl::hl_type_kind_HF32
                | hl::hl_type_kind_HF64
                | hl::hl_type_kind_HBOOL
                | hl::hl_type_kind_HBYTES
                | hl::hl_type_kind_HUI8
                | hl::hl_type_kind_HUI16
        );

        if needs_boxing && !self.fn_make_dyn.is_null() {
            let c_type_ptr = self.c_type_factory.get(src_type_ref.0);
            // Create a stack slot holding the raw value for hlp_make_dyn
            let mut data: i64 = if val.is_i32() {
                val.as_i32() as i64
            } else if val.is_f64() {
                val.as_f64().to_bits() as i64
            } else if val.is_bool() {
                val.as_bool() as i64
            } else {
                // Pointer-like (HBYTES, etc.)
                val.as_ptr() as i64
            };
            let make_dyn: unsafe extern "C" fn(*mut c_void, *mut c_void) -> *mut c_void =
                unsafe { std::mem::transmute(self.fn_make_dyn) };
            let dyn_ptr = unsafe {
                make_dyn(
                    &mut data as *mut i64 as *mut c_void,
                    c_type_ptr as *mut c_void,
                )
            };
            frame
                .registers
                .set(dst, NanBoxedValue::from_ptr(dyn_ptr as usize));
        } else {
            // Already a pointer type with vdynamic header, or no make_dyn available
            frame.registers.set(dst, val);
        }

        Ok(StepResult::Continue)
    }

    /// Write a field by name hash on a dynamic value.
    ///
    /// Extracted from `execute_opcode` so the SSA dispatcher in
    /// [`crate::ssa`] runs the same semantics rather than a copy of them:
    /// register operands are plain indices into the active frame, which is
    /// the SSA value frame there and the HL register file here.
    #[allow(clippy::too_many_arguments, unused_variables)]
    fn op_dyn_set(
        &mut self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        obj: u32,
        field: usize,
        src: u32,
    ) -> Result<StepResult> {
        let fn_hash_gen = self.fn_hash_gen;
        let frame = self.stack.last_mut().unwrap();
        let obj_val = frame.registers.get(obj);
        if obj_val.is_null() || obj_val.is_void() {
            // no-op
        } else {
            let hfield = hash_field_name(
                bytecode,
                field,
                fn_hash_gen,
                &mut self.utf16_strings,
                &mut self.field_hash_cache,
            )?;
            let obj_ptr = obj_val.as_ptr() as *mut c_void;
            let src_val = frame.registers.get(src);
            let src_type_idx = func.regs[src as usize].0;
            let src_kind = bytecode.types[src_type_idx].kind;
            if env_flag!("ASH_DBG_DYN") {
                let fname = bytecode
                    .strings
                    .get(field)
                    .map(String::as_str)
                    .unwrap_or("<oob>");
                eprintln!(
                        "[DYNSET] f{} pc={} obj={:?} field={} name={} hash={} src_ty={} src_kind={} src={:?}",
                        func_idx, frame.pc, obj_val, field, fname, hfield, src_type_idx, src_kind, src_val
                    );
            }
            let src_type_ptr = self.c_type_factory.get(src_type_idx) as *mut c_void;
            Self::dyn_set_field_by_hash(
                obj_ptr,
                hfield,
                src_val,
                src_kind,
                src_type_ptr,
                self.fn_dyn_setd,
                self.fn_dyn_setf,
                self.fn_dyn_seti64,
                self.fn_dyn_seti,
                self.fn_dyn_setp,
            );
        }

        Ok(StepResult::Continue)
    }

    /// Read a field by name hash off a dynamic value.
    ///
    /// Extracted from `execute_opcode` so the SSA dispatcher in
    /// [`crate::ssa`] runs the same semantics rather than a copy of them:
    /// register operands are plain indices into the active frame, which is
    /// the SSA value frame there and the HL register file here.
    #[allow(clippy::too_many_arguments, unused_variables)]
    fn op_dyn_get(
        &mut self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        dst: u32,
        obj: u32,
        field: usize,
    ) -> Result<StepResult> {
        let fn_hash_gen = self.fn_hash_gen;
        let frame = self.stack.last_mut().unwrap();
        let obj_val = frame.registers.get(obj);
        if obj_val.is_null() || obj_val.is_void() {
            frame.registers.set(dst, NanBoxedValue::null());
        } else {
            let hfield = hash_field_name(
                bytecode,
                field,
                fn_hash_gen,
                &mut self.utf16_strings,
                &mut self.field_hash_cache,
            )?;
            if env_flag!("ASH_DBG_DYN") {
                let fname = bytecode
                    .strings
                    .get(field)
                    .map(String::as_str)
                    .unwrap_or("<oob>");
                eprintln!(
                    "[DYNGET] f{} pc={} obj={:?} field={} name={} hash={}",
                    func_idx, frame.pc, obj_val, field, fname, hfield
                );
            }
            let obj_ptr = obj_val.as_ptr() as *mut c_void;
            let dst_type_idx = func.regs[dst as usize].0;
            let dst_kind = bytecode.types[dst_type_idx].kind;
            let dst_type_ptr = self.c_type_factory.get(dst_type_idx) as *mut c_void;
            let out = Self::dyn_get_field_by_hash(
                obj_ptr,
                hfield,
                dst_kind,
                dst_type_ptr,
                self.fn_dyn_getd,
                self.fn_dyn_getf,
                self.fn_dyn_geti64,
                self.fn_dyn_geti,
                self.fn_dyn_getp,
            );
            if env_flag!("ASH_DBG_DYN") {
                eprintln!(
                    "[DYNGET] f{} pc={} dst_kind={} -> {:?}",
                    func_idx, frame.pc, dst_kind, out
                );
            }
            frame.registers.set(dst, out);
        }

        Ok(StepResult::Continue)
    }

    /// Write `obj.field` for any object representation.
    ///
    /// Extracted from `execute_opcode` so the SSA dispatcher in
    /// [`crate::ssa`] runs the same semantics rather than a copy of them:
    /// register operands are plain indices into the active frame, which is
    /// the SSA value frame there and the HL register file here.
    #[allow(clippy::too_many_arguments, unused_variables)]
    fn op_field_set(
        &mut self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        obj: u32,
        field: usize,
        src: u32,
    ) -> Result<StepResult> {
        let frame = self.stack.last_mut().unwrap();
        let obj_type_idx = func.regs[obj as usize].0;
        let obj_kind = bytecode.types[obj_type_idx].kind;
        let obj_c_type = self.c_type_factory.get(obj_type_idx) as *mut c_void;
        let src_type_idx = func.regs[src as usize].0;
        let src_kind = bytecode.types[src_type_idx].kind;
        let get_rt = self.fn_get_obj_rt;
        let obj_val = frame.registers.get(obj);
        if env_flag!("ASH_DBG_FIELD") {
            eprintln!(
                    "[SETFIELD] f{} pc={} obj_ty={} obj_kind={} field={} src_ty={} src_kind={} obj={:?} src={:?}",
                    func_idx,
                    frame.pc,
                    obj_type_idx,
                    obj_kind,
                    field,
                    src_type_idx,
                    src_kind,
                    obj_val,
                    frame.registers.get(src)
                );
        }
        if !obj_val.is_null() && !obj_val.is_void() {
            let src_val = frame.registers.get(src);
            if obj_kind == hl::hl_type_kind_HOBJ || obj_kind == hl::hl_type_kind_HSTRUCT {
                let obj_ptr = obj_val.as_ptr() as *mut u8;
                if env_flag!("ASH_DBG_FIELD") {
                    eprintln!(
                            "[SETFIELD-OBJ] f{} pc={} obj_ty={} obj_kind={} field={} src_kind={} src={:?}",
                            func_idx, frame.pc, obj_type_idx, obj_kind, field, src_kind, src_val
                        );
                }
                unsafe {
                    Self::write_obj_field(
                        obj_ptr, field, src_kind, src_val, obj_c_type, obj_kind, get_rt,
                    );
                }
            } else if obj_kind == hl::hl_type_kind_HVIRTUAL {
                if let Some(offset) =
                    unsafe { Self::resolve_virtual_field_offset(obj_c_type, field) }
                {
                    let obj_ptr = obj_val.as_ptr() as *mut u8;
                    let addr = unsafe { obj_ptr.add(offset) };
                    if env_flag!("ASH_DBG_FIELD") {
                        eprintln!(
                                "[SETFIELD-VIRT] f{} pc={} obj_ty={} field={} off={} src_kind={} src={:?}",
                                func_idx, frame.pc, obj_type_idx, field, offset, src_kind, src_val
                            );
                    }
                    unsafe { Self::write_value_at(addr, src_kind, src_val) };
                } else {
                    self.virtual_fields
                        .insert((obj_val.as_ptr(), field), src_val);
                    if let Some(hfield) =
                        Self::resolve_typed_field_hash(bytecode, obj_type_idx, field)
                    {
                        let obj_ptr = obj_val.as_ptr() as *mut c_void;
                        let src_type_ptr = self.c_type_factory.get(src_type_idx) as *mut c_void;
                        Self::dyn_set_field_by_hash(
                            obj_ptr,
                            hfield,
                            src_val,
                            src_kind,
                            src_type_ptr,
                            self.fn_dyn_setd,
                            self.fn_dyn_setf,
                            self.fn_dyn_seti64,
                            self.fn_dyn_seti,
                            self.fn_dyn_setp,
                        );
                    }
                    if env_flag!("ASH_DBG_FIELD") {
                        eprintln!(
                            "[SETFIELD-VIRT-FALLBACK] f{} pc={} obj_ty={} field={} src={:?}",
                            func_idx, frame.pc, obj_type_idx, field, src_val
                        );
                    }
                }
            } else if let Some(hfield) =
                Self::resolve_typed_field_hash(bytecode, obj_type_idx, field)
            {
                let obj_ptr = obj_val.as_ptr() as *mut c_void;
                let src_type_ptr = self.c_type_factory.get(src_type_idx) as *mut c_void;
                Self::dyn_set_field_by_hash(
                    obj_ptr,
                    hfield,
                    src_val,
                    src_kind,
                    src_type_ptr,
                    self.fn_dyn_setd,
                    self.fn_dyn_setf,
                    self.fn_dyn_seti64,
                    self.fn_dyn_seti,
                    self.fn_dyn_setp,
                );
            }
        }

        Ok(StepResult::Continue)
    }

    /// Read `obj.field` for any object representation.
    ///
    /// Extracted from `execute_opcode` so the SSA dispatcher in
    /// [`crate::ssa`] runs the same semantics rather than a copy of them:
    /// register operands are plain indices into the active frame, which is
    /// the SSA value frame there and the HL register file here.
    #[allow(clippy::too_many_arguments, unused_variables)]
    fn op_field_get(
        &mut self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        dst: u32,
        obj: u32,
        field: usize,
    ) -> Result<StepResult> {
        let frame = self.stack.last_mut().unwrap();
        // Extract c_type info before borrowing frame mutably
        let obj_type_idx = func.regs[obj as usize].0;
        let obj_kind = bytecode.types[obj_type_idx].kind;
        let obj_c_type = self.c_type_factory.get(obj_type_idx) as *mut c_void;
        let dst_kind = bytecode.types[func.regs[dst as usize].0].kind;
        let get_rt = self.fn_get_obj_rt;
        let obj_val = frame.registers.get(obj);
        if env_flag!("ASH_DBG_FIELD") {
            eprintln!(
                "[FIELD] f{} pc={} obj_ty={} obj_kind={} field={} dst_kind={} obj={:?}",
                func_idx, frame.pc, obj_type_idx, obj_kind, field, dst_kind, obj_val
            );
        }
        if obj_val.is_null() || obj_val.is_void() {
            frame.registers.set(dst, NanBoxedValue::null());
        } else if obj_kind == hl::hl_type_kind_HOBJ || obj_kind == hl::hl_type_kind_HSTRUCT {
            let obj_ptr = obj_val.as_ptr() as *mut u8;
            let val = unsafe {
                Self::read_obj_field(obj_ptr, field, dst_kind, obj_c_type, obj_kind, get_rt)
            };
            if env_flag!("ASH_DBG_FIELD") {
                eprintln!(
                    "[GETFIELD-OBJ] f{} pc={} obj_ty={} obj_kind={} field={} dst_kind={} -> {:?}",
                    func_idx, frame.pc, obj_type_idx, obj_kind, field, dst_kind, val
                );
            }
            frame.registers.set(dst, val);
        } else if obj_kind == hl::hl_type_kind_HVIRTUAL {
            if let Some(offset) = unsafe { Self::resolve_virtual_field_offset(obj_c_type, field) } {
                let obj_ptr = obj_val.as_ptr() as *mut u8;
                let addr = unsafe { obj_ptr.add(offset) };
                let val = unsafe { Self::read_value_at(addr, dst_kind) };
                if env_flag!("ASH_DBG_FIELD") {
                    eprintln!(
                        "[GETFIELD-VIRT] f{} pc={} obj_ty={} field={} off={} dst_kind={} -> {:?}",
                        func_idx, frame.pc, obj_type_idx, field, offset, dst_kind, val
                    );
                }
                frame.registers.set(dst, val);
            } else {
                let key = (obj_val.as_ptr(), field);
                let val = if let Some(v) = self.virtual_fields.get(&key).copied() {
                    v
                } else if let Some(hfield) =
                    Self::resolve_typed_field_hash(bytecode, obj_type_idx, field)
                {
                    let dst_type_idx = func.regs[dst as usize].0;
                    let dst_type_ptr = self.c_type_factory.get(dst_type_idx) as *mut c_void;
                    Self::dyn_get_field_by_hash(
                        obj_val.as_ptr() as *mut c_void,
                        hfield,
                        dst_kind,
                        dst_type_ptr,
                        self.fn_dyn_getd,
                        self.fn_dyn_getf,
                        self.fn_dyn_geti64,
                        self.fn_dyn_geti,
                        self.fn_dyn_getp,
                    )
                } else {
                    NanBoxedValue::null()
                };
                if env_flag!("ASH_DBG_FIELD") {
                    eprintln!(
                        "[GETFIELD-VIRT-FALLBACK] f{} pc={} obj_ty={} field={} -> {:?}",
                        func_idx, frame.pc, obj_type_idx, field, val
                    );
                }
                frame.registers.set(dst, val);
            }
        } else if let Some(hfield) = Self::resolve_typed_field_hash(bytecode, obj_type_idx, field) {
            let obj_ptr = obj_val.as_ptr() as *mut c_void;
            let dst_type_idx = func.regs[dst as usize].0;
            let dst_type_ptr = self.c_type_factory.get(dst_type_idx) as *mut c_void;
            let out = Self::dyn_get_field_by_hash(
                obj_ptr,
                hfield,
                dst_kind,
                dst_type_ptr,
                self.fn_dyn_getd,
                self.fn_dyn_getf,
                self.fn_dyn_geti64,
                self.fn_dyn_geti,
                self.fn_dyn_getp,
            );
            frame.registers.set(dst, out);
        } else {
            frame.registers.set(dst, NanBoxedValue::null());
        }

        Ok(StepResult::Continue)
    }

    /// Materialize a vclosure for a virtual method of an object.
    ///
    /// Extracted from `execute_opcode` so the SSA dispatcher in
    /// [`crate::ssa`] runs the same semantics rather than a copy of them:
    /// register operands are plain indices into the active frame, which is
    /// the SSA value frame there and the HL register file here.
    #[allow(clippy::too_many_arguments, unused_variables)]
    fn op_virtual_closure(
        &mut self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        dst: u32,
        obj: u32,
        field: u32,
    ) -> Result<StepResult> {
        let frame = self.stack.last_mut().unwrap();
        // Resolve the virtual method findex from the object's proto chain,
        // then create a vclosure with the object as bound value.
        let obj_val = frame.registers.get(obj);
        if obj_val.is_null() || obj_val.is_void() {
            frame.registers.set(dst, NanBoxedValue::null());
        } else {
            let obj_ptr = obj_val.as_ptr() as *const u8;
            // The virtual field index into the interface's field table
            // We need to look up the method findex from the object's runtime type.
            // For now, look up via the object's proto chain by field index.
            let findex_opt: Option<usize> = unsafe {
                let obj_hl_type = *(obj_ptr as *const *mut hl::hl_type);
                if !obj_hl_type.is_null()
                    && ((*obj_hl_type).kind == hl::hl_type_kind_HOBJ
                        || (*obj_hl_type).kind == hl::hl_type_kind_HSTRUCT)
                {
                    let obj_data = (*obj_hl_type).__bindgen_anon_1.obj;
                    let fi = field as usize;
                    if fi < (*obj_data).nproto as usize {
                        Some((*(*obj_data).proto.add(fi)).findex as usize)
                    } else {
                        None
                    }
                } else {
                    None
                }
            };
            if let Some(findex) = findex_opt {
                let closure_type = self.c_type_factory.get(func.regs[dst as usize].0);
                let value = unsafe {
                    Self::alloc_bound_closure(
                        self.fn_alloc_closure_ptr,
                        closure_type,
                        findex,
                        obj_val.as_ptr() as *mut std::ffi::c_void,
                    )
                };
                frame.registers.set(dst, value);
            } else {
                frame.registers.set(dst, NanBoxedValue::null());
            }
        }

        Ok(StepResult::Continue)
    }

    /// Materialize a vclosure bound to an object.
    ///
    /// Extracted from `execute_opcode` so the SSA dispatcher in
    /// [`crate::ssa`] runs the same semantics rather than a copy of them:
    /// register operands are plain indices into the active frame, which is
    /// the SSA value frame there and the HL register file here.
    #[allow(clippy::too_many_arguments, unused_variables)]
    fn op_instance_closure(
        &mut self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        dst: u32,
        fun: usize,
        obj: u32,
    ) -> Result<StepResult> {
        let frame = self.stack.last_mut().unwrap();
        // Create a _vclosure with the bound object. The closure's fun
        // pointer is the stub sentinel (findex+1) so that CallClosure
        // can extract the findex. The bound object is stored in
        // vclosure.value and prepended as the first argument on CallClosure.
        let obj_val = frame.registers.get(obj);
        let obj_ptr = if obj_val.is_null() || obj_val.is_void() {
            std::ptr::null_mut()
        } else {
            obj_val.as_ptr() as *mut std::ffi::c_void
        };
        let closure_type = self.c_type_factory.get(func.regs[dst as usize].0);
        let value = unsafe {
            Self::alloc_bound_closure(self.fn_alloc_closure_ptr, closure_type, fun, obj_ptr)
        };
        frame.registers.set(dst, value);

        Ok(StepResult::Continue)
    }

    /// Materialize a vclosure for a bare function index.
    ///
    /// Extracted from `execute_opcode` so the SSA dispatcher in
    /// [`crate::ssa`] runs the same semantics rather than a copy of them:
    /// register operands are plain indices into the active frame, which is
    /// the SSA value frame there and the HL register file here.
    #[allow(clippy::too_many_arguments, unused_variables)]
    fn op_static_closure(
        &mut self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        dst: u32,
        fun: usize,
    ) -> Result<StepResult> {
        let frame = self.stack.last_mut().unwrap();
        // Materialize a real vclosure* so std natives such as
        // hl.Api.noClosure / Reflect.callMethod can consume it.
        let findex = fun;
        let type_idx = if let Some(fidx) = func_of(&self.targets, findex) {
            bytecode.functions[fidx].type_.0
        } else if let Some(nidx) = native_of(&self.targets, findex) {
            bytecode.natives[nidx].type_.0
        } else {
            usize::MAX
        };

        if type_idx != usize::MAX && !self.fn_alloc_closure_void.is_null() {
            type FnAllocClosureVoid =
                unsafe extern "C" fn(*mut c_void, *mut c_void) -> *mut _vclosure;
            let f: FnAllocClosureVoid = unsafe { std::mem::transmute(self.fn_alloc_closure_void) };
            let tptr = self.c_type_factory.get(type_idx) as *mut c_void;
            let closure = unsafe { f(tptr, (findex + 1) as *mut c_void) };
            if !closure.is_null() {
                if env_flag!("ASH_DBG_CLOSURE") {
                    eprintln!(
                        "[STATICCLOSURE] findex={} type_idx={} -> {:p}",
                        findex, type_idx, closure
                    );
                }
                frame
                    .registers
                    .set(dst, NanBoxedValue::from_ptr(closure as usize));
                return Ok(StepResult::Continue);
            }
        }

        // Fallback to interpreter-local representation.
        if env_flag!("ASH_DBG_CLOSURE") {
            eprintln!(
                "[STATICCLOSURE-FALLBACK] findex={} type_idx={} alloc_fn={:p}",
                findex, type_idx, self.fn_alloc_closure_void
            );
        }
        frame
            .registers
            .set(dst, NanBoxedValue::from_func_index(findex));

        Ok(StepResult::Continue)
    }

    /// Resolve and stage a call through a closure value.
    ///
    /// Extracted from `execute_opcode` so the SSA dispatcher in
    /// [`crate::ssa`] runs the same semantics rather than a copy of them:
    /// register operands are plain indices into the active frame, which is
    /// the SSA value frame there and the HL register file here.
    #[allow(clippy::too_many_arguments, unused_variables)]
    fn op_call_closure(
        &mut self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        dst: u32,
        fun: u32,
        args: &[Reg],
    ) -> Result<StepResult> {
        let frame = self.stack.last_mut().unwrap();
        let closure_val = frame.registers.get(fun);
        let mut arg_vals: Vec<NanBoxedValue> =
            args.iter().map(|r| frame.registers.get(r.0)).collect();

        if closure_val.is_null() || closure_val.is_void() {
            return Err(anyhow!("CallClosure on null closure (pc={})", frame.pc));
        }

        // The closure value might be:
        // 1. A TAG_FUNC: raw function index (from StaticClosure with no capture)
        // 2. A TAG_PTR to a _vclosure struct (InstanceClosure with bound value)
        let findex = if closure_val.is_func() {
            closure_val.as_func_index()
        } else {
            let raw = closure_val.as_ptr();
            if func_of(&self.targets, raw).is_some() || native_of(&self.targets, raw).is_some() {
                raw
            } else {
                // It's a pointer to a _vclosure struct
                let cl_ptr = raw as *const _vclosure;
                if cl_ptr.is_null()
                    || !(cl_ptr as usize).is_multiple_of(std::mem::align_of::<_vclosure>())
                {
                    return Err(anyhow!(
                        "CallClosure invalid closure value: {:?}",
                        closure_val
                    ));
                }
                unsafe {
                    let fun_ptr = (*cl_ptr).fun;
                    // Extract findex from stub pointer (findex+1)
                    let fi = (fun_ptr as usize).wrapping_sub(1);
                    // If the closure has a bound value, prepend it as the first arg
                    if (*cl_ptr).hasValue != 0 && !(*cl_ptr).value.is_null() {
                        let bound = NanBoxedValue::from_ptr((*cl_ptr).value as usize);
                        arg_vals.insert(0, bound);
                    }
                    fi
                }
            }
        };

        Ok(StepResult::Call {
            findex,
            args: arg_vals,
            dst,
        })
    }

    /// Resolve and stage a method call through the receiver's vtable slot.
    ///
    /// Extracted from `execute_opcode` so the SSA dispatcher in
    /// [`crate::ssa`] runs the same semantics rather than a copy of them:
    /// register operands are plain indices into the active frame, which is
    /// the SSA value frame there and the HL register file here.
    #[allow(clippy::too_many_arguments, unused_variables)]
    fn op_call_method(
        &mut self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        op_is_this: bool,
        dst: u32,
        field: usize,
        args: &[Reg],
    ) -> Result<StepResult> {
        let frame = self.stack.last_mut().unwrap();
        // CallMethod: args[0] is 'this'. CallThis: the receiver is
        // IMPLICITLY register 0 (HashLink OCallThis semantics) and
        // args hold only the real arguments — prepend Reg(0), else
        // method resolution runs against the first argument's type.
        let args_with_this: Vec<Reg> = if op_is_this {
            let mut v = Vec::with_capacity(args.len() + 1);
            v.push(Reg(0));
            v.extend(args.iter().copied());
            v
        } else {
            args.to_vec()
        };
        let args = &args_with_this;
        let arg_vals: Vec<NanBoxedValue> = args.iter().map(|r| frame.registers.get(r.0)).collect();
        let this_val = arg_vals[0];

        if this_val.is_null() || this_val.is_void() {
            return Err(anyhow!(
                "CallMethod on null object (field={}, pc={})",
                field,
                frame.pc
            ));
        }

        // HVIRTUAL dispatch: ToVirtual is a no-op in the interpreter,
        // so `this_val` holds the raw HOBJ pointer directly.
        // Resolve the findex by matching the virtual field's hashed_name
        // against the runtime object's proto chain.
        let this_reg_type_idx = func.regs[args[0].0 as usize].0;
        if this_reg_type_idx < bytecode.types.len()
            && bytecode.types[this_reg_type_idx].kind == hl::hl_type_kind_HVIRTUAL
        {
            let virt_type = self.c_type_factory.get(this_reg_type_idx);
            let obj_ptr = this_val.as_ptr() as *const u8;
            let findex_opt = unsafe {
                // Get hashed_name of the virtual field
                let virt = (*virt_type).__bindgen_anon_1.virt.as_ref();
                if let Some(virt_data) = virt {
                    if (field as i32) < virt_data.nfields {
                        let virt_field = &*virt_data.fields.add(field);
                        let hname = virt_field.hashed_name;
                        // Walk the runtime obj's proto chain for hname
                        let mut obj_hl_type = *(obj_ptr as *const *mut hl_type);
                        let mut found = None;
                        'search: while !obj_hl_type.is_null()
                            && ((*obj_hl_type).kind == hl::hl_type_kind_HOBJ
                                || (*obj_hl_type).kind == hl::hl_type_kind_HSTRUCT)
                        {
                            let obj = (*obj_hl_type).__bindgen_anon_1.obj;
                            for i in 0..(*obj).nproto as usize {
                                let pr = &*(*obj).proto.add(i);
                                if pr.hashed_name == hname {
                                    found = Some(pr.findex as usize);
                                    break 'search;
                                }
                            }
                            // Try super class
                            obj_hl_type = (*obj).super_ as *mut hl_type;
                        }
                        found
                    } else {
                        None
                    }
                } else {
                    None
                }
            };
            if let Some(findex) = findex_opt {
                return Ok(StepResult::Call {
                    findex,
                    args: arg_vals,
                    dst: dst,
                });
            }
        }

        // Try to resolve via vobj_proto (set up by hlp_get_obj_proto)
        let obj_ptr = this_val.as_ptr() as *const u8;
        let findex = unsafe {
            let type_ptr = *(obj_ptr as *const *mut hl_type);
            if !type_ptr.is_null() {
                let vobj_proto = (*type_ptr).vobj_proto;
                if !vobj_proto.is_null() && vobj_proto as usize > 1 {
                    let method_ptr = *vobj_proto.add(field);
                    if (method_ptr as u64) < ash::jit::stub_bridge::STUB_SENTINEL_LIMIT {
                        // Interpreter stub: the slot encodes findex+1.
                        (method_ptr as usize).wrapping_sub(1)
                    } else {
                        // A real code pointer — `patch_vtable_slots` wrote the
                        // compiled address into this row on promotion. The
                        // interpreter dispatches by findex (its call path then
                        // finds the compiled entry through the bead), so
                        // resolve the findex from the bytecode instead of
                        // decoding the pointer as one — which produced
                        // "findex 47297429503 not found".
                        self.resolve_method_findex_from_bytecode(bytecode, func, &args[0], field)
                            .ok_or_else(|| {
                                anyhow!("Cannot resolve method field={} on type", field)
                            })?
                    }
                } else {
                    // Fallback: resolve from bytecode type proto
                    self.resolve_method_findex_from_bytecode(bytecode, func, &args[0], field)
                        .ok_or_else(|| anyhow!("Cannot resolve method field={} on type", field))?
                }
            } else {
                self.resolve_method_findex_from_bytecode(bytecode, func, &args[0], field)
                    .ok_or_else(|| {
                        anyhow!("Cannot resolve method field={} (null type header)", field)
                    })?
            }
        };

        Ok(StepResult::Call {
            findex,
            args: arg_vals,
            dst,
        })
    }

    // =====================================================================
    // AIR v2 SSA dispatch
    //
    // See `crate::ssa` for the design. In short: the frame is
    // `values.len() + cells.len()` slots, a `ValueId` indexes it directly, and
    // the shared `op_*` methods above run the per-instruction semantics so this
    // dispatcher never holds a second copy of them.
    // =====================================================================

    /// Run one function from its prepared SSA IR.
    fn execute_ssa_function(
        &mut self,
        bc: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        func_idx: usize,
        prep: &'static crate::ssa::Prepared,
        args: &[NanBoxedValue],
    ) -> Result<NanBoxedValue> {
        if self.stack.len() >= self.max_stack_depth {
            return Err(anyhow!("Stack overflow (depth {})", self.stack.len()));
        }

        let ir = prep.ir;
        let buf = self.reg_pool.pop().unwrap_or_default();
        let mut frame =
            InterpreterFrame::with_buffer(func_idx, ir.values.len() + ir.cells.len(), buf);

        // A pinned argument register never gets a `Param`: lowering emits those
        // only for registers it promoted to SSA. On the serialize path that is
        // harmless because a cell *is* its HL register and the caller already
        // bound it; here the cell is a frame slot of its own, so the binding has
        // to happen explicitly or an argument taken by `Ref` reads as void.
        for (ci, cell) in ir.cells.iter().enumerate() {
            if let Some(v) = args.get(cell.reg as usize) {
                frame.registers.set(prep.cell_base + ci as u32, *v);
            }
        }

        self.stack.push(frame);
        self.sync_gc_scan_roots();

        let prev_findex = ash::profile::enter_interp(bc.functions[func_idx].findex as u32);
        let result = self.ssa_loop(bc, native_resolver, func_idx, prep, args);
        ash::profile::leave_interp(prev_findex);
        if let Some(f) = self.stack.pop() {
            if self.reg_pool.len() < POOL_CAP {
                self.reg_pool.push(f.into_buffer());
            }
        }
        self.sync_gc_scan_roots();
        result
    }

    /// Block-at-a-time dispatch over the SSA CFG.
    fn ssa_loop(
        &mut self,
        bc: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        func_idx: usize,
        prep: &'static crate::ssa::Prepared,
        args: &[NanBoxedValue],
    ) -> Result<NanBoxedValue> {
        let ir = prep.ir;
        let func = prep.shim;
        let mut block = 0usize;
        // Which edge control arrived on. Phi sources are keyed by it, and the
        // exceptional edge into a handler sets it too, so a handler that does
        // carry phis resolves them against the block that threw.
        let mut prev_block: Option<u32> = None;
        let mut phi_buf: Vec<(u32, NanBoxedValue)> = Vec::new();

        'blocks: loop {
            let blk = ir
                .blocks
                .get(block)
                .ok_or_else(|| anyhow!("SSA block {} out of range in {}", block, func.name()))?;
            // Published for the same reason the opcode loop publishes `pc`:
            // it is the only record of where a frame is when something below
            // it fails.
            self.stack.last_mut().unwrap().pc = block;

            // A phi group is a parallel copy. Read every source before writing
            // any destination, or `x, y = y, x` collapses into `x, y = y, y`.
            if !blk.phis.is_empty() {
                phi_buf.clear();
                let frame = self.stack.last().unwrap();
                for phi in &blk.phis {
                    if let Some(pb) = prev_block {
                        if let Some(&(_, v)) = phi.incoming.iter().find(|(b, _)| b.0 == pb) {
                            phi_buf.push((phi.dst.0, frame.registers.get(v.0)));
                        }
                    }
                }
                let frame = self.stack.last_mut().unwrap();
                for (dst, v) in phi_buf.drain(..) {
                    frame.registers.set(dst, v);
                }
            }

            for ins in &blk.instrs {
                match self.ssa_step(bc, native_resolver, func_idx, prep, args, ins)? {
                    None => {}
                    // A call raised and this frame's innermost trap caught it.
                    Some(handler) => {
                        prev_block = Some(block as u32);
                        block = handler;
                        continue 'blocks;
                    }
                }
            }

            let get = |s: &Self, v: air::v2::ValueId| s.stack.last().unwrap().registers.get(v.0);
            match &blk.term {
                air::v2::Terminator::Ret { value } => return Ok(get(self, *value)),
                air::v2::Terminator::Jump { target } => {
                    prev_block = Some(block as u32);
                    block = target.idx();
                }
                air::v2::Terminator::CondJump {
                    cond,
                    a,
                    b,
                    if_true,
                    if_false,
                } => {
                    let taken = self.ssa_cond(bc, func, func_idx, *cond, *a, *b);
                    prev_block = Some(block as u32);
                    block = if taken { if_true.idx() } else { if_false.idx() };
                }
                air::v2::Terminator::Switch {
                    value,
                    targets,
                    default,
                } => {
                    let idx = get(self, *value).as_i32();
                    prev_block = Some(block as u32);
                    block = if idx >= 0 && (idx as usize) < targets.len() {
                        targets[idx as usize].idx()
                    } else {
                        default.idx()
                    };
                }
                air::v2::Terminator::Throw { exc } | air::v2::Terminator::Rethrow { exc } => {
                    let val = get(self, *exc);
                    let frame = self.stack.last_mut().unwrap();
                    match frame.trap_stack.pop() {
                        Some((handler, cell_slot)) => {
                            frame.registers.set(cell_slot, val);
                            prev_block = Some(block as u32);
                            block = handler;
                        }
                        None => return Err(anyhow::Error::new(self.format_hl_exception(val))),
                    }
                }
                air::v2::Terminator::Trap {
                    exc_cell,
                    handler,
                    normal,
                } => {
                    let slot = prep.cell_base + exc_cell.0;
                    self.stack
                        .last_mut()
                        .unwrap()
                        .trap_stack
                        .push((handler.idx(), slot));
                    prev_block = Some(block as u32);
                    block = normal.idx();
                }
            }
        }
    }

    /// Evaluate a `CondJump` condition.
    fn ssa_cond(
        &self,
        bc: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        cond: air::v2::CondKind,
        a: air::v2::ValueId,
        b: Option<air::v2::ValueId>,
    ) -> bool {
        use air::v2::CondKind as C;
        let va = self.stack.last().unwrap().registers.get(a.0);
        let cmp = |op: CmpOp| {
            let b = b.expect("binary condition without a second operand");
            self.compare_regs_in(bc, func, func_idx, a.0, b.0, op)
        };
        match cond {
            C::True => va.to_bool(),
            C::False => !va.to_bool(),
            C::Null => va.is_null(),
            C::NotNull => !va.is_null(),
            C::SLt => cmp(CmpOp::SLt),
            C::SGte => cmp(CmpOp::SGte),
            C::SGt => cmp(CmpOp::SGt),
            C::SLte => cmp(CmpOp::SLte),
            C::ULt => cmp(CmpOp::ULt),
            C::UGte => cmp(CmpOp::UGte),
            // The reference dispatcher reads the NaN-aware forms as their
            // plain negations, and parity with it is the bar.
            C::NotLt => cmp(CmpOp::SGte),
            C::NotGte => cmp(CmpOp::SLt),
            C::Eq => cmp(CmpOp::Eq),
            C::NotEq => cmp(CmpOp::NotEq),
        }
    }

    /// Execute one SSA instruction.
    ///
    /// `Ok(None)` continues in the same block. `Ok(Some(b))` means a call threw
    /// and this frame's innermost trap caught it, so control resumes at `b`.
    /// `Err` propagates, which is what the reference does for everything a
    /// non-call instruction raises — including `NullCheck`, whose exception
    /// escapes its own frame's traps there too.
    #[allow(clippy::too_many_arguments)]
    fn ssa_step(
        &mut self,
        bc: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        func_idx: usize,
        prep: &'static crate::ssa::Prepared,
        args: &[NanBoxedValue],
        ins: &air::v2::Instr,
    ) -> Result<Option<usize>> {
        use air::v2::Instr as I;
        let func = prep.shim;
        let cell_base = prep.cell_base;

        macro_rules! get {
            ($v:expr) => {
                self.stack.last().unwrap().registers.get($v.0)
            };
        }
        macro_rules! set {
            ($v:expr, $val:expr) => {{
                let val = $val;
                self.stack.last_mut().unwrap().registers.set($v.0, val)
            }};
        }
        /// Static HL kind of a value, via the shim's per-value type table.
        macro_rules! kind {
            ($v:expr) => {
                bc.types[func.regs[$v.0 as usize].0].kind
            };
        }

        match ins {
            // ---- values -----------------------------------------------
            I::Param { dst, reg } => {
                // Registers past the argument list are the HL default, which is
                // what a fresh frame slot already holds.
                let v = args
                    .get(*reg as usize)
                    .copied()
                    .unwrap_or_else(NanBoxedValue::void);
                set!(dst, v);
            }
            I::Copy { dst, src } => {
                let v = get!(src);
                set!(dst, v);
            }
            I::Int { dst, idx } => set!(dst, NanBoxedValue::from_i32(bc.ints[*idx])),
            I::Float { dst, idx } => set!(dst, NanBoxedValue::from_f64(bc.floats[*idx])),
            I::Bool { dst, value } => set!(dst, NanBoxedValue::from_bool(*value)),
            I::Bytes { dst, idx } => {
                set!(dst, NanBoxedValue::from_bytes_ptr(bc.bytes_pos[*idx]))
            }
            I::String { dst, idx } => {
                // HashLink strings are UTF-16 internally; the cache owns the
                // null-terminated buffers the pointer refers to.
                let utf16_ptr = if let Some(cached) = self.utf16_strings.get(idx) {
                    cached.as_ptr()
                } else {
                    let s = bc
                        .strings
                        .get(*idx)
                        .ok_or_else(|| anyhow!("String constant out of bounds: {}", idx))?;
                    let mut buf: Vec<u16> = s.encode_utf16().collect();
                    buf.push(0);
                    self.utf16_strings.insert(*idx, buf);
                    self.utf16_strings[idx].as_ptr()
                };
                set!(dst, NanBoxedValue::from_bytes_ptr(utf16_ptr as usize));
            }
            I::Null { dst } => set!(dst, NanBoxedValue::null()),

            // ---- arithmetic -------------------------------------------
            I::BinOp { op, dst, a, b } => {
                use air::v2::BinOp as B;
                let va = get!(a);
                let vb = get!(b);
                let r = match op {
                    B::Add => va
                        .binary_int_op(vb, IntBinOp::Add)
                        .or_else(|| va.binary_float_op(vb, FloatBinOp::Add)),
                    B::Sub => va
                        .binary_int_op(vb, IntBinOp::Sub)
                        .or_else(|| va.binary_float_op(vb, FloatBinOp::Sub)),
                    B::Mul => va
                        .binary_int_op(vb, IntBinOp::Mul)
                        .or_else(|| va.binary_float_op(vb, FloatBinOp::Mul)),
                    B::SDiv => va
                        .binary_int_op(vb, IntBinOp::SDiv)
                        .or_else(|| va.binary_float_op(vb, FloatBinOp::SDiv)),
                    B::SMod => va
                        .binary_int_op(vb, IntBinOp::SMod)
                        .or_else(|| va.binary_float_op(vb, FloatBinOp::SMod)),
                    B::UDiv => va.binary_int_op(vb, IntBinOp::UDiv),
                    B::UMod => {
                        let r = vb.as_i32() as u32;
                        if r == 0 {
                            return Err(anyhow!("UMod: division by zero"));
                        }
                        Some(NanBoxedValue::from_i32(((va.as_i32() as u32) % r) as i32))
                    }
                    B::Shl => va.binary_int_op(vb, IntBinOp::Shl),
                    B::SShr => va.binary_int_op(vb, IntBinOp::SShr),
                    B::UShr => va.binary_int_op(vb, IntBinOp::UShr),
                    B::And => va.binary_int_op(vb, IntBinOp::And),
                    B::Or => va.binary_int_op(vb, IntBinOp::Or),
                    B::Xor => va.binary_int_op(vb, IntBinOp::Xor),
                };
                let r = r.ok_or_else(|| {
                    anyhow!(
                        "{:?}: incompatible types {:?}, {:?} in {} (dst=v{}, a=v{}, b=v{})",
                        op,
                        va,
                        vb,
                        func.name(),
                        dst.0,
                        a.0,
                        b.0
                    )
                })?;
                set!(dst, r);
            }
            I::Fma { dst, a, b, c } => {
                // Deliberately two roundings, not `mul_add`. The FMA peephole
                // exists for backends that emit a hardware fused multiply-add;
                // this interpreter is the bit-exact reference the others are
                // measured against, and it rounds every operation — fusing here
                // would move the measuring stick.
                let r = get!(a).as_f64() * get!(b).as_f64() + get!(c).as_f64();
                set!(dst, NanBoxedValue::from_f64(r));
            }
            I::UnOp { op, dst, src } => {
                let v = get!(src);
                let r = match op {
                    // Mirrors the opcode arms exactly, including leaving a
                    // non-numeric value untouched rather than erroring.
                    air::v2::UnOp::Incr => {
                        if v.is_i32() {
                            NanBoxedValue::from_i32(v.as_i32().wrapping_add(1))
                        } else if v.is_f64() {
                            NanBoxedValue::from_f64(v.as_f64() + 1.0)
                        } else {
                            v
                        }
                    }
                    air::v2::UnOp::Decr => {
                        if v.is_i32() {
                            NanBoxedValue::from_i32(v.as_i32().wrapping_sub(1))
                        } else if v.is_f64() {
                            NanBoxedValue::from_f64(v.as_f64() - 1.0)
                        } else {
                            v
                        }
                    }
                    air::v2::UnOp::Neg => {
                        if v.is_i32() {
                            NanBoxedValue::from_i32(v.as_i32().wrapping_neg())
                        } else if v.is_f64() {
                            NanBoxedValue::from_f64(-v.as_f64())
                        } else {
                            return Err(anyhow!("Neg: unsupported type {:?}", v));
                        }
                    }
                    air::v2::UnOp::Not => {
                        if v.is_i32() {
                            NanBoxedValue::from_i32(!v.as_i32())
                        } else if v.is_bool() {
                            NanBoxedValue::from_bool(!v.as_bool())
                        } else {
                            return Err(anyhow!("Not: unsupported type {:?}", v));
                        }
                    }
                };
                set!(dst, r);
            }

            // ---- calls -------------------------------------------------
            I::Intrinsic {
                kind,
                dst,
                args: a,
                ..
            } => {
                // Inline Rust, no FFI dispatch, no marshal. Semantics are
                // pinned to the ash_std bodies these replaced — RoundHalfUp
                // is floor(x + 0.5) and the i32 conversions are Rust `as`
                // (saturating, NaN -> 0).
                use air::v2::ir::IntrinsicKind as K;
                let r = match kind {
                    K::PtrCompare => {
                        let (pa, pb) = (get!(&a[0]).as_ptr() as usize, get!(&a[1]).as_ptr() as usize);
                        NanBoxedValue::from_i32(match pa.cmp(&pb) {
                            std::cmp::Ordering::Equal => 0,
                            std::cmp::Ordering::Greater => 1,
                            std::cmp::Ordering::Less => -1,
                        })
                    }
                    _ => {
                        let x = get!(&a[0]).as_f64();
                        match kind {
                            K::Sqrt => NanBoxedValue::from_f64(x.sqrt()),
                            K::Abs => NanBoxedValue::from_f64(x.abs()),
                            K::Floor => NanBoxedValue::from_f64(x.floor()),
                            K::Ceil => NanBoxedValue::from_f64(x.ceil()),
                            K::RoundHalfUp => NanBoxedValue::from_f64((x + 0.5).floor()),
                            K::FloorToI32 => NanBoxedValue::from_i32(x.floor() as i32),
                            K::CeilToI32 => NanBoxedValue::from_i32(x.ceil() as i32),
                            K::RoundHalfUpToI32 => NanBoxedValue::from_i32((x + 0.5).floor() as i32),
                            K::IsNaN => NanBoxedValue::from_bool(x.is_nan()),
                            K::IsFinite => NanBoxedValue::from_bool(x.is_finite()),
                            K::PtrCompare => unreachable!("handled above"),
                        }
                    }
                };
                set!(dst, r);
            }
            I::Call { dst, fun, args: a } => {
                let argv: Vec<NanBoxedValue> = a.iter().map(|v| get!(v)).collect();
                return self.ssa_call(bc, native_resolver, func, *fun, argv, dst.0);
            }
            I::CallMethod {
                dst,
                field,
                args: a,
            } => {
                let regs: Vec<Reg> = a.iter().map(|v| Reg(v.0)).collect();
                let staged =
                    self.op_call_method(bc, func, func_idx, false, dst.0, *field, &regs)?;
                return self.ssa_staged_call(bc, native_resolver, func, staged);
            }
            I::CallClosure { dst, fun, args: a } => {
                let regs: Vec<Reg> = a.iter().map(|v| Reg(v.0)).collect();
                let staged = self.op_call_closure(bc, func, func_idx, dst.0, fun.0, &regs)?;
                return self.ssa_staged_call(bc, native_resolver, func, staged);
            }
            I::StaticClosure { dst, fun } => {
                self.op_static_closure(bc, func, func_idx, dst.0, *fun)?;
            }
            I::InstanceClosure { dst, fun, obj } => {
                self.op_instance_closure(bc, func, func_idx, dst.0, *fun, obj.0)?;
            }
            I::VirtualClosure { dst, obj, field } => {
                self.op_virtual_closure(bc, func, func_idx, dst.0, obj.0, *field as u32)?;
            }

            // ---- globals and fields ------------------------------------
            I::GetGlobal { dst, global } => {
                let mut val = self
                    .globals
                    .get(*global)
                    .copied()
                    .unwrap_or_else(NanBoxedValue::null);
                // Native stdlib may have written a global_value slot without
                // going through SetGlobal.
                if val.is_null() {
                    let (gd, nglobals) = self.c_type_factory.globals_data();
                    if !gd.is_null() && *global < nglobals {
                        let raw = unsafe { *gd.add(*global) };
                        if !raw.is_null() {
                            val = NanBoxedValue::from_ptr(raw as usize);
                            self.globals[*global] = val;
                        }
                    }
                }
                set!(dst, val);
            }
            I::SetGlobal { global, src } => {
                let val = get!(src);
                if *global >= self.globals.len() {
                    self.globals.resize(*global + 1, NanBoxedValue::null());
                }
                self.globals[*global] = val;
                let (gd, nglobals) = self.c_type_factory.globals_data();
                if !gd.is_null() && *global < nglobals {
                    unsafe {
                        *gd.add(*global) = if val.is_null() || val.is_void() {
                            std::ptr::null_mut()
                        } else {
                            val.as_ptr() as *mut c_void
                        };
                    }
                }
            }
            I::FieldGet {
                dst, obj, field, ..
            } => {
                self.op_field_get(bc, func, func_idx, dst.0, obj.0, *field)?;
            }
            I::FieldSet {
                obj, field, src, ..
            } => {
                self.op_field_set(bc, func, func_idx, obj.0, *field, src.0)?;
            }
            I::DynGet { dst, obj, field } => {
                self.op_dyn_get(bc, func, func_idx, dst.0, obj.0, *field)?;
            }
            I::DynSet { obj, field, src } => {
                self.op_dyn_set(bc, func, func_idx, obj.0, *field, src.0)?;
            }

            // ---- casts -------------------------------------------------
            I::Cast { kind, dst, src } => {
                use air::v2::CastKind as K;
                match kind {
                    K::ToDyn => {
                        self.op_to_dyn(bc, func, func_idx, dst.0, src.0)?;
                    }
                    K::SafeCast => {
                        self.op_safe_cast(bc, func, func_idx, dst.0, src.0)?;
                    }
                    K::ToSFloat => {
                        let v = get!(src);
                        let f = if v.is_i32() {
                            v.as_i32() as f64
                        } else {
                            v.as_f64()
                        };
                        set!(dst, NanBoxedValue::from_f64(f));
                    }
                    K::ToUFloat => {
                        let v = get!(src);
                        let f = if v.is_i32() {
                            (v.as_i32() as u32) as f64
                        } else {
                            v.as_f64()
                        };
                        set!(dst, NanBoxedValue::from_f64(f));
                    }
                    K::ToInt => {
                        let v = get!(src);
                        let i = if v.is_f64() {
                            v.as_f64() as i32
                        } else {
                            v.as_i32()
                        };
                        set!(dst, NanBoxedValue::from_i32(i));
                    }
                    // ToVirtual is a no-op here for the same reason it is in the
                    // opcode dispatcher: virtual dispatch resolves off the raw
                    // object at the call site.
                    K::UnsafeCast | K::ToVirtual => {
                        let v = get!(src);
                        set!(dst, v);
                    }
                }
            }
            I::NullCheck { value } => {
                if get!(value).is_null() {
                    return Err(anyhow::Error::new(HLExceptionPropagation {
                        value: NanBoxedValue::null(),
                        message: Some("Null access".to_string()),
                    }));
                }
            }

            // ---- memory ------------------------------------------------
            I::MemGet {
                kind,
                dst,
                base,
                index,
            } => match kind {
                air::v2::MemAccess::Array => {
                    self.op_get_array(bc, func, func_idx, dst.0, base.0, index.0)?;
                }
                k => {
                    let b = get!(base);
                    let idx = get!(index).as_i32();
                    let val = if b.is_null() || b.is_void() || idx < 0 {
                        NanBoxedValue::from_i32(0)
                    } else {
                        let addr = (b.as_ptr() as *const u8).wrapping_add(idx as usize);
                        match k {
                            air::v2::MemAccess::I8 => {
                                NanBoxedValue::from_i32(unsafe { *addr as i32 })
                            }
                            air::v2::MemAccess::I16 => {
                                NanBoxedValue::from_i32(unsafe { *(addr as *const u16) as i32 })
                            }
                            _ => Self::read_value_from_ptr(addr, kind!(dst)),
                        }
                    };
                    set!(dst, val);
                }
            },
            I::MemSet {
                kind,
                base,
                index,
                src,
            } => match kind {
                air::v2::MemAccess::Array => {
                    self.op_set_array(bc, func, func_idx, base.0, index.0, src.0)?;
                }
                k => {
                    let b = get!(base);
                    let idx = get!(index).as_i32();
                    let v = get!(src);
                    if !b.is_null() && !b.is_void() && idx >= 0 {
                        let addr = (b.as_ptr() as *mut u8).wrapping_add(idx as usize);
                        match k {
                            air::v2::MemAccess::I8 => unsafe { *addr = v.as_i32() as u8 },
                            air::v2::MemAccess::I16 => unsafe {
                                *(addr as *mut u16) = v.as_i32() as u16
                            },
                            _ => {
                                if (addr as usize) < 0x1000 {
                                    eprintln!(
                                        "[CRASH GUARD] SetMem bad addr={:p} base={:?} idx={} in {}",
                                        addr,
                                        b,
                                        idx,
                                        func.name()
                                    );
                                } else {
                                    Self::write_value_to_ptr(addr, v, kind!(src));
                                }
                            }
                        }
                    }
                }
            },

            // ---- allocation and type queries ---------------------------
            I::New { dst } => {
                self.op_new(bc, func, func_idx, dst.0)?;
            }
            I::ArraySize { dst, array } => {
                let arr = get!(array);
                let size = if kind!(array) == hl::hl_type_kind_HARRAY
                    && !arr.is_null()
                    && !arr.is_void()
                {
                    // varray: t@0, at@8, size@16
                    unsafe { *((arr.as_ptr() as *const u8).add(16) as *const i32) }
                } else {
                    0i32
                };
                set!(dst, NanBoxedValue::from_i32(size));
            }
            I::TypeConst { dst, ty } => {
                let p = self.c_type_factory.get(ty.0 as usize);
                set!(dst, NanBoxedValue::from_ptr(p as usize));
            }
            I::GetType { dst, src } => {
                let v = get!(src);
                let src_ty = func.regs[src.0 as usize].0;
                let ptr: usize = if v.is_ptr() && !v.is_null() && v.as_ptr() != 0 {
                    match bc.types[src_ty].kind {
                        hl::hl_type_kind_HDYN
                        | hl::hl_type_kind_HOBJ
                        | hl::hl_type_kind_HSTRUCT
                        | hl::hl_type_kind_HVIRTUAL
                        | hl::hl_type_kind_HENUM
                        | hl::hl_type_kind_HDYNOBJ
                        | hl::hl_type_kind_HNULL => unsafe { *(v.as_ptr() as *const usize) },
                        _ => self.c_type_factory.get(src_ty) as usize,
                    }
                } else {
                    self.c_type_factory.get(src_ty) as usize
                };
                set!(dst, NanBoxedValue::from_ptr(ptr));
            }
            I::GetTID { dst, src } => {
                let v = get!(src);
                let k = if v.is_ptr() && !v.is_null() && v.as_ptr() != 0 {
                    unsafe { *(v.as_ptr() as *const u32) as i32 }
                } else {
                    bc.types[func.regs[src.0 as usize].0].kind as i32
                };
                set!(dst, NanBoxedValue::from_i32(k));
            }

            // ---- references --------------------------------------------
            I::Unref { dst, src } => {
                let p = get!(src).as_ptr() as *const i64;
                let r = if p.is_null() {
                    NanBoxedValue::null()
                } else {
                    let raw = unsafe { *p };
                    match kind!(dst) {
                        hl::hl_type_kind_HI32 | hl::hl_type_kind_HUI8 | hl::hl_type_kind_HUI16 => {
                            NanBoxedValue::from_i32(raw as i32)
                        }
                        hl::hl_type_kind_HF64 | hl::hl_type_kind_HF32 => {
                            NanBoxedValue::from_f64(f64::from_bits(raw as u64))
                        }
                        // Low 32 bits only: a native writes a c_int here, and
                        // the NaN tag bits would make the full i64 always true.
                        hl::hl_type_kind_HBOOL => NanBoxedValue::from_bool((raw as i32) != 0),
                        _ => NanBoxedValue::from_ptr(raw as usize),
                    }
                };
                set!(dst, r);
            }
            I::SetRef { r, value } => {
                let p = get!(r).as_ptr() as *mut NanBoxedValue;
                if !p.is_null() {
                    let v = get!(value);
                    unsafe { *p = v };
                }
            }
            I::RefData { dst, src } => {
                let v = get!(src);
                set!(dst, v);
            }
            I::RefOffset { dst, base, offset } => {
                let r =
                    NanBoxedValue::from_ptr(get!(base).as_ptr() + get!(offset).as_i32() as usize);
                set!(dst, r);
            }

            // ---- enums -------------------------------------------------
            I::MakeEnum {
                dst,
                construct,
                args: a,
            } => {
                let c_type_ptr = self.c_type_factory.get(func.regs[dst.0 as usize].0);
                let val = Self::alloc_enum_value(self.fn_alloc_enum, c_type_ptr, *construct as i32);
                if !val.is_null() {
                    let argv: Vec<NanBoxedValue> = a.iter().map(|v| get!(v)).collect();
                    unsafe {
                        let tenum = (*c_type_ptr).__bindgen_anon_1.tenum;
                        let c = &*(*tenum).constructs.add(*construct);
                        let base = val as *mut u8;
                        for (i, v) in argv.into_iter().enumerate() {
                            if i >= c.nparams as usize {
                                break;
                            }
                            let offset = *c.offsets.add(i) as usize;
                            let param_kind = (*(*c.params.add(i))).kind;
                            Self::write_value_to_ptr(base.add(offset), v, param_kind);
                        }
                    }
                }
                set!(
                    dst,
                    if val.is_null() {
                        NanBoxedValue::null()
                    } else {
                        NanBoxedValue::from_ptr(val as usize)
                    }
                );
            }
            I::EnumAlloc { dst, construct } => {
                let c_type_ptr = self.c_type_factory.get(func.regs[dst.0 as usize].0);
                let val = Self::alloc_enum_value(self.fn_alloc_enum, c_type_ptr, *construct as i32);
                set!(
                    dst,
                    if val.is_null() {
                        NanBoxedValue::null()
                    } else {
                        NanBoxedValue::from_ptr(val as usize)
                    }
                );
            }
            I::EnumIndex { dst, value } => {
                let v = get!(value);
                let index = if v.is_null() || v.is_void() {
                    0i32
                } else {
                    // venum: t@0, index@8
                    unsafe { *(v.as_ptr() as *const u8).add(8).cast::<i32>() }
                };
                set!(dst, NanBoxedValue::from_i32(index));
            }
            I::EnumField {
                dst,
                value,
                construct,
                field,
            } => {
                let v = get!(value);
                let c_type_ptr = self.c_type_factory.get(func.regs[value.0 as usize].0);
                let r = if v.is_null() || v.is_void() || c_type_ptr.is_null() {
                    NanBoxedValue::null()
                } else {
                    unsafe {
                        let tenum = (*c_type_ptr).__bindgen_anon_1.tenum;
                        if tenum.is_null() || *construct >= (*tenum).nconstructs as usize {
                            NanBoxedValue::null()
                        } else {
                            let c = &*(*tenum).constructs.add(*construct);
                            if *field >= c.nparams as usize {
                                NanBoxedValue::null()
                            } else {
                                let offset = *c.offsets.add(*field) as usize;
                                let param_kind = (*(*c.params.add(*field))).kind;
                                Self::read_value_from_ptr(
                                    (v.as_ptr() as *const u8).add(offset),
                                    param_kind,
                                )
                            }
                        }
                    }
                };
                set!(dst, r);
            }
            I::SetEnumField {
                value, field, src, ..
            } => {
                let v = get!(value);
                let src_val = get!(src);
                let c_type_ptr = self.c_type_factory.get(func.regs[value.0 as usize].0);
                if !v.is_null() && !v.is_void() && !c_type_ptr.is_null() {
                    unsafe {
                        let tenum = (*c_type_ptr).__bindgen_anon_1.tenum;
                        if !tenum.is_null() {
                            // The construct comes off the live venum, not the
                            // instruction: that is what the reference does, and
                            // the two disagree when a register is reused.
                            let ci = *(v.as_ptr() as *const u8).add(8).cast::<i32>() as usize;
                            if ci < (*tenum).nconstructs as usize {
                                let c = &*(*tenum).constructs.add(ci);
                                if *field < c.nparams as usize {
                                    let offset = *c.offsets.add(*field) as usize;
                                    let param_kind = (*(*c.params.add(*field))).kind;
                                    Self::write_value_to_ptr(
                                        (v.as_ptr() as *mut u8).add(offset),
                                        src_val,
                                        param_kind,
                                    );
                                }
                            }
                        }
                    }
                }
            }

            // ---- cells (pinned registers) -------------------------------
            I::CellGet { dst, cell } => {
                let v = self.stack.last().unwrap().registers.get(cell_base + cell.0);
                set!(dst, v);
            }
            I::CellSet { cell, src } => {
                let v = get!(src);
                self.stack
                    .last_mut()
                    .unwrap()
                    .registers
                    .set(cell_base + cell.0, v);
            }
            I::CellIncr { cell } => {
                let frame = self.stack.last_mut().unwrap();
                let slot = cell_base + cell.0;
                let v = frame.registers.get(slot);
                if v.is_i32() {
                    frame
                        .registers
                        .set(slot, NanBoxedValue::from_i32(v.as_i32().wrapping_add(1)));
                } else if v.is_f64() {
                    frame
                        .registers
                        .set(slot, NanBoxedValue::from_f64(v.as_f64() + 1.0));
                }
            }
            I::CellDecr { cell } => {
                let frame = self.stack.last_mut().unwrap();
                let slot = cell_base + cell.0;
                let v = frame.registers.get(slot);
                if v.is_i32() {
                    frame
                        .registers
                        .set(slot, NanBoxedValue::from_i32(v.as_i32().wrapping_sub(1)));
                } else if v.is_f64() {
                    frame
                        .registers
                        .set(slot, NanBoxedValue::from_f64(v.as_f64() - 1.0));
                }
            }
            I::CellRef { dst, cell } => {
                // Address of the cell's frame slot, exactly as `Ref` takes the
                // address of a register slot: natives write through it and the
                // cell is updated in place. The slot is stable across nested
                // calls because the frame's `Vec` is its own allocation.
                let frame = self.stack.last_mut().unwrap();
                let p = frame.registers.slot_ptr(cell_base + cell.0) as usize;
                frame.registers.set(dst.0, NanBoxedValue::from_ptr(p));
            }

            // ---- trap regions -------------------------------------------
            I::EndTrap { cell, .. } => {
                let frame = self.stack.last_mut().unwrap();
                frame.trap_stack.pop();
                frame
                    .registers
                    .set(cell_base + cell.0, NanBoxedValue::null());
            }

            // ---- misc ---------------------------------------------------
            I::Assert => return Err(anyhow!("Assert hit in {}", func.name())),
            I::Prefetch { .. } | I::Asm { .. } => {}
        }

        Ok(None)
    }

    /// Perform a staged call produced by one of the shared `op_call_*` methods.
    fn ssa_staged_call(
        &mut self,
        bc: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        func: &HLFunction,
        staged: StepResult,
    ) -> Result<Option<usize>> {
        match staged {
            StepResult::Call { findex, args, dst } => {
                self.ssa_call(bc, native_resolver, func, findex, args, dst)
            }
            // The closure paths answer `Continue` when they resolved to a value
            // rather than a call (a null receiver, say).
            _ => Ok(None),
        }
    }

    /// Call `findex`, store the coerced result, and let this frame's innermost
    /// trap catch an exception coming back out.
    fn ssa_call(
        &mut self,
        bc: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        func: &HLFunction,
        findex: usize,
        mut args: Vec<NanBoxedValue>,
        dst: u32,
    ) -> Result<Option<usize>> {
        // Same reclaim as the serialize trampoline: the callee has copied the
        // arguments into its own registers, so the buffer goes back to the pool
        // before the result is examined.
        let call_result = self.call_function(bc, native_resolver, findex, &args);
        if self.arg_pool.len() < POOL_CAP {
            args.clear();
            self.arg_pool.push(args);
        }
        match call_result {
            Ok(ret) => {
                let dst_kind = bc.types[func.regs[dst as usize].0].kind;
                let coerced = self.coerce_value_for_static_kind(ret, dst_kind);
                self.stack.last_mut().unwrap().registers.set(dst, coerced);
                Ok(None)
            }
            Err(e) => {
                if let Some(exc_val) = e.downcast_ref::<HLExceptionPropagation>().map(|x| x.value) {
                    let frame = self.stack.last_mut().unwrap();
                    if let Some((handler, cell_slot)) = frame.trap_stack.pop() {
                        frame.registers.set(cell_slot, exc_val);
                        return Ok(Some(handler));
                    }
                }
                Err(e)
            }
        }
    }

    fn int_binop(
        &mut self,
        func: &HLFunction,
        op: IntBinOp,
        dst: u32,
        a: u32,
        b: u32,
    ) -> Result<()> {
        let frame = self.stack.last_mut().unwrap();
        let va = frame.registers.get(a);
        let vb = frame.registers.get(b);
        let result = va.binary_int_op(vb, op).ok_or_else(|| {
            anyhow!(
                "{:?}: incompatible types {:?}, {:?} in {} at pc={} (dst=r{}, a=r{}, b=r{})",
                op,
                va,
                vb,
                func.name(),
                frame.pc,
                dst,
                a,
                b
            )
        })?;
        frame.registers.set(dst, result);
        Ok(())
    }

    /// Helper: compare two register values.
    fn compare_regs(
        &self,
        bytecode: &DecodedBytecode,
        func_idx: usize,
        a: u32,
        b: u32,
        op: CmpOp,
    ) -> bool {
        let func = self.air.body(bytecode, func_idx);
        self.compare_regs_in(bytecode, func, func_idx, a, b, op)
    }

    /// [`compare_regs`](Self::compare_regs) against an explicit function.
    ///
    /// The comparison is type-directed — HNULL unboxing, string-object and
    /// dynamic equality all depend on the operands' static kinds — so the SSA
    /// dispatcher in [`crate::ssa`] passes its value-type view here instead of
    /// having a second implementation of those rules.
    #[allow(clippy::too_many_arguments)]
    fn compare_regs_in(
        &self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        a: u32,
        b: u32,
        op: CmpOp,
    ) -> bool {
        let frame = self.stack.last().unwrap();
        let va = frame.registers.get(a);
        let vb = frame.registers.get(b);
        let ak = bytecode.types[func.regs[a as usize].0].kind;
        let bk = bytecode.types[func.regs[b as usize].0].kind;
        if let Some(result) = unsafe {
            self.try_compare_nullable_operands(
                bytecode, func, a as usize, va, ak, b as usize, vb, bk, op,
            )
        } {
            if env_flag!("ASH_TRACE_EQ") {
                eprintln!(
                    "[CMP-HNULL] f{} op={:?} ak={} bk={} va={:?} vb={:?} -> {}",
                    func_idx, op, ak, bk, va, vb, result
                );
            }
            return result;
        }
        if op == CmpOp::Eq || op == CmpOp::NotEq {
            if ak == hl::hl_type_kind_HBYTES && bk == hl::hl_type_kind_HBYTES {
                let pa = if va.is_null() || va.is_void() {
                    std::ptr::null()
                } else {
                    va.as_ptr() as *const u16
                };
                let pb = if vb.is_null() || vb.is_void() {
                    std::ptr::null()
                } else {
                    vb.as_ptr() as *const u16
                };
                let eq = unsafe { Self::utf16z_eq(pa, pb) };
                if env_flag!("ASH_TRACE_EQ") {
                    eprintln!(
                        "[CMP] f{} op={:?} ak={} bk={} (bytes) -> {}",
                        func_idx, op, ak, bk, eq
                    );
                }
                return if op == CmpOp::Eq { eq } else { !eq };
            }
            if ak == hl::hl_type_kind_HOBJ && bk == hl::hl_type_kind_HOBJ {
                let pa = if va.is_null() || va.is_void() {
                    std::ptr::null_mut()
                } else {
                    va.as_ptr() as *mut hl::vdynamic
                };
                let pb = if vb.is_null() || vb.is_void() {
                    std::ptr::null_mut()
                } else {
                    vb.as_ptr() as *mut hl::vdynamic
                };
                if !pa.is_null() && !pb.is_null() {
                    let ta_name = self.dynamic_type_name(pa);
                    let tb_name = self.dynamic_type_name(pb);
                    if env_flag!("ASH_TRACE_EQ") {
                        eprintln!(
                            "[CMP-OBJ] f{} op={:?} ta={:?} tb={:?} pa={:#x} pb={:#x}",
                            func_idx,
                            op,
                            ta_name,
                            tb_name,
                            va.as_ptr(),
                            vb.as_ptr()
                        );
                    }
                    if ta_name == tb_name
                        && matches!(ta_name.as_deref(), Some("String") | Some("S"))
                    {
                        let sa = unsafe {
                            self.try_extract_string_object_raw(va.as_ptr() as *mut c_void)
                        };
                        let sb = unsafe {
                            self.try_extract_string_object_raw(vb.as_ptr() as *mut c_void)
                        };
                        if env_flag!("ASH_TRACE_EQ") {
                            eprintln!(
                                "[CMP-OBJ] f{} string-extract sa={} sb={}",
                                func_idx,
                                sa.is_some(),
                                sb.is_some()
                            );
                        }
                        if let (Some((ab, al)), Some((bb, bl))) = (sa, sb) {
                            let eq = al == bl && unsafe { Self::utf16_len_eq(ab, bb, al as usize) };
                            if env_flag!("ASH_TRACE_EQ") {
                                eprintln!(
                                    "[CMP] f{} op={:?} ak={} bk={} (string-obj) -> {}",
                                    func_idx, op, ak, bk, eq
                                );
                            }
                            return if op == CmpOp::Eq { eq } else { !eq };
                        }
                    }
                }
            }
            if ak == hl::hl_type_kind_HDYN && bk == hl::hl_type_kind_HDYN {
                let pa = if va.is_null() || va.is_void() {
                    std::ptr::null_mut()
                } else {
                    va.as_ptr() as *mut hl::vdynamic
                };
                let pb = if vb.is_null() || vb.is_void() {
                    std::ptr::null_mut()
                } else {
                    vb.as_ptr() as *mut hl::vdynamic
                };
                let eq = unsafe { self.dynamic_eq(pa, pb) };
                if env_flag!("ASH_TRACE_EQ") {
                    eprintln!(
                        "[CMP] f{} op={:?} ak={} bk={} (dyn) -> {}",
                        func_idx, op, ak, bk, eq
                    );
                    if !eq {
                        let ka_dyn = if pa.is_null() || unsafe { (*pa).t.is_null() } {
                            0
                        } else {
                            unsafe { (*(*pa).t).kind }
                        };
                        let kb_dyn = if pb.is_null() || unsafe { (*pb).t.is_null() } {
                            0
                        } else {
                            unsafe { (*(*pb).t).kind }
                        };
                        eprintln!(
                            "[CMP_DYN] ka_dyn={} kb_dyn={} ta={:?} tb={:?} sa={:?} sb={:?}",
                            ka_dyn,
                            kb_dyn,
                            self.dynamic_type_name(pa),
                            self.dynamic_type_name(pb),
                            self.value_to_string(pa),
                            self.value_to_string(pb)
                        );
                    }
                }
                return if op == CmpOp::Eq { eq } else { !eq };
            }
        }
        let result = va.compare(vb, op).unwrap_or(false);
        if env_flag!("ASH_TRACE_EQ") && (op == CmpOp::Eq || op == CmpOp::NotEq) {
            eprintln!(
                "[CMP] f{} op={:?} ak={} bk={} va={:?} vb={:?} -> {}",
                func_idx, op, ak, bk, va, vb, result
            );
        }
        result
    }

    #[allow(clippy::too_many_arguments)]
    unsafe fn try_compare_nullable_operands(
        &self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        a_idx: usize,
        va: NanBoxedValue,
        ak: u32,
        b_idx: usize,
        vb: NanBoxedValue,
        bk: u32,
        op: CmpOp,
    ) -> Option<bool> {
        if ak != hl::hl_type_kind_HNULL && bk != hl::hl_type_kind_HNULL {
            return None;
        }

        let (av, ak_eff) =
            self.normalize_nullable_compare_operand(bytecode, func, a_idx, ak, va)?;
        let (bv, bk_eff) =
            self.normalize_nullable_compare_operand(bytecode, func, b_idx, bk, vb)?;

        if av.is_none() || bv.is_none() {
            let eq = av.is_none() && bv.is_none();
            return Some(match op {
                CmpOp::Eq => eq,
                CmpOp::NotEq => !eq,
                _ => false,
            });
        }

        let av = av.unwrap();
        let bv = bv.unwrap();
        if let Some(result) = Self::compare_numeric_values(av, ak_eff, bv, bk_eff, op) {
            return Some(result);
        }

        if op == CmpOp::Eq || op == CmpOp::NotEq {
            if let Some(result) = av.compare(bv, op) {
                return Some(result);
            }
        }

        None
    }

    unsafe fn normalize_nullable_compare_operand(
        &self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        reg_idx: usize,
        reg_kind: u32,
        val: NanBoxedValue,
    ) -> Option<(Option<NanBoxedValue>, u32)> {
        if reg_kind != hl::hl_type_kind_HNULL {
            return Some((Some(val), reg_kind));
        }
        if val.is_null() || val.is_void() || (val.is_ptr() && val.as_ptr() == 0) {
            return Some((None, reg_kind));
        }
        if !val.is_ptr() {
            return Some((Some(val), reg_kind));
        }

        let reg_type_idx = match func.regs.get(reg_idx) {
            Some(r) => r.0,
            None => return Some((Some(val), reg_kind)),
        };
        let reg_type = match bytecode.types.get(reg_type_idx) {
            Some(t) => t,
            None => return Some((Some(val), reg_kind)),
        };
        let tparam_idx = match reg_type.tparam.as_ref() {
            Some(tp) => tp.0,
            None => return Some((Some(val), reg_kind)),
        };
        let inner_kind = match bytecode.types.get(tparam_idx) {
            Some(t) => t.kind,
            None => return Some((Some(val), reg_kind)),
        };

        if !Self::is_primitive_or_bytes_kind(inner_kind) {
            return Some((Some(val), reg_kind));
        }

        let d = val.as_ptr() as *mut hl::vdynamic;
        if d.is_null() {
            return Some((None, inner_kind));
        }
        if let Some(unboxed) = Self::unbox_dynamic_to_kind(d, inner_kind) {
            if unboxed.is_null() || unboxed.is_void() {
                return Some((None, inner_kind));
            }
            return Some((Some(unboxed), inner_kind));
        }

        Some((Some(val), reg_kind))
    }

    fn compare_numeric_values(
        av: NanBoxedValue,
        ak: u32,
        bv: NanBoxedValue,
        bk: u32,
        op: CmpOp,
    ) -> Option<bool> {
        if !Self::is_numeric_or_bool_kind(ak) || !Self::is_numeric_or_bool_kind(bk) {
            return None;
        }

        let has_float = ak == hl::hl_type_kind_HF32
            || ak == hl::hl_type_kind_HF64
            || bk == hl::hl_type_kind_HF32
            || bk == hl::hl_type_kind_HF64;

        if has_float {
            let l = Self::numeric_as_f64(av, ak)?;
            let r = Self::numeric_as_f64(bv, bk)?;
            return Some(match op {
                CmpOp::SLt | CmpOp::ULt => l < r,
                CmpOp::SGte | CmpOp::UGte => l >= r,
                CmpOp::SGt => l > r,
                CmpOp::SLte => l <= r,
                CmpOp::Eq => l == r,
                CmpOp::NotEq => l != r,
            });
        }

        match op {
            CmpOp::ULt | CmpOp::UGte => {
                let l = Self::numeric_as_u64(av, ak)?;
                let r = Self::numeric_as_u64(bv, bk)?;
                Some(match op {
                    CmpOp::ULt => l < r,
                    CmpOp::UGte => l >= r,
                    _ => unreachable!(),
                })
            }
            CmpOp::SLt | CmpOp::SGte | CmpOp::SGt | CmpOp::SLte | CmpOp::Eq | CmpOp::NotEq => {
                let l = Self::numeric_as_i64(av, ak)?;
                let r = Self::numeric_as_i64(bv, bk)?;
                Some(match op {
                    CmpOp::SLt => l < r,
                    CmpOp::SGte => l >= r,
                    CmpOp::SGt => l > r,
                    CmpOp::SLte => l <= r,
                    CmpOp::Eq => l == r,
                    CmpOp::NotEq => l != r,
                    _ => unreachable!(),
                })
            }
        }
    }

    fn numeric_as_f64(v: NanBoxedValue, kind: u32) -> Option<f64> {
        match kind {
            k if k == hl::hl_type_kind_HI32 => Some(v.as_i32() as f64),
            k if k == hl::hl_type_kind_HUI8 => Some((v.as_i32() as u8) as f64),
            k if k == hl::hl_type_kind_HUI16 => Some((v.as_i32() as u16) as f64),
            k if k == hl::hl_type_kind_HI64 => Some(v.as_i64_lossy() as f64),
            k if k == hl::hl_type_kind_HF32 || k == hl::hl_type_kind_HF64 => Some(v.as_f64()),
            k if k == hl::hl_type_kind_HBOOL => Some(if v.as_bool() { 1.0 } else { 0.0 }),
            _ => None,
        }
    }

    fn numeric_as_i64(v: NanBoxedValue, kind: u32) -> Option<i64> {
        match kind {
            k if k == hl::hl_type_kind_HI32 => Some(v.as_i32() as i64),
            k if k == hl::hl_type_kind_HUI8 => Some((v.as_i32() as u8) as i64),
            k if k == hl::hl_type_kind_HUI16 => Some((v.as_i32() as u16) as i64),
            k if k == hl::hl_type_kind_HI64 => Some(v.as_i64_lossy()),
            k if k == hl::hl_type_kind_HBOOL => Some(if v.as_bool() { 1 } else { 0 }),
            _ => None,
        }
    }

    fn numeric_as_u64(v: NanBoxedValue, kind: u32) -> Option<u64> {
        match kind {
            k if k == hl::hl_type_kind_HI32 => Some((v.as_i32() as u32) as u64),
            k if k == hl::hl_type_kind_HUI8 => Some((v.as_i32() as u8) as u64),
            k if k == hl::hl_type_kind_HUI16 => Some((v.as_i32() as u16) as u64),
            k if k == hl::hl_type_kind_HI64 => Some(v.as_i64_lossy() as u64),
            k if k == hl::hl_type_kind_HBOOL => Some(if v.as_bool() { 1 } else { 0 }),
            _ => None,
        }
    }

    unsafe fn utf16z_eq(a: *const u16, b: *const u16) -> bool {
        if a == b {
            return true;
        }
        if a.is_null() || b.is_null() {
            return false;
        }
        let mut i = 0usize;
        loop {
            let ca = *a.add(i);
            let cb = *b.add(i);
            if ca != cb {
                return false;
            }
            if ca == 0 {
                return true;
            }
            i += 1;
        }
    }

    unsafe fn utf16_len_eq(a: *const u16, b: *const u16, len: usize) -> bool {
        if a.is_null() || b.is_null() {
            return false;
        }
        for i in 0..len {
            if *a.add(i) != *b.add(i) {
                return false;
            }
        }
        true
    }

    unsafe fn try_extract_string_object(&self, d: *mut hl::vdynamic) -> Option<(*const u16, i32)> {
        if d.is_null() || self.fn_obj_get_field.is_null() {
            return None;
        }
        let get_field: FnObjGetField = std::mem::transmute(self.fn_obj_get_field);
        let h_len = self.hash_literal_name("length");
        let h_bytes = self.hash_literal_name("bytes");
        let len_dyn = get_field(d, h_len);
        let bytes_dyn = get_field(d, h_bytes);
        if len_dyn.is_null() || bytes_dyn.is_null() {
            return None;
        }
        if (*len_dyn).t.is_null() || (*bytes_dyn).t.is_null() {
            return None;
        }
        if (*(*len_dyn).t).kind != hl::hl_type_kind_HI32 {
            return None;
        }
        if (*(*bytes_dyn).t).kind != hl::hl_type_kind_HBYTES {
            return None;
        }
        let len = (*len_dyn).v.i;
        let bytes = (*bytes_dyn).v.bytes as *const u16;
        if len < 0 || bytes.is_null() {
            return None;
        }
        Some((bytes, len))
    }

    unsafe fn try_extract_string_object_raw(
        &self,
        obj_ptr: *mut c_void,
    ) -> Option<(*const u16, i32)> {
        if obj_ptr.is_null() || self.fn_get_obj_rt.is_null() {
            return None;
        }
        let type_ptr = *(obj_ptr as *const *mut hl::hl_type);
        if type_ptr.is_null() || (*type_ptr).kind != hl::hl_type_kind_HOBJ {
            return None;
        }
        let bytes_val = Self::read_obj_field(
            obj_ptr as *mut u8,
            0,
            hl::hl_type_kind_HBYTES,
            type_ptr as *mut c_void,
            hl::hl_type_kind_HOBJ,
            self.fn_get_obj_rt,
        );
        let len_val = Self::read_obj_field(
            obj_ptr as *mut u8,
            1,
            hl::hl_type_kind_HI32,
            type_ptr as *mut c_void,
            hl::hl_type_kind_HOBJ,
            self.fn_get_obj_rt,
        );
        if bytes_val.is_null() || len_val.is_null() || len_val.is_void() {
            return None;
        }
        let bytes = bytes_val.as_ptr() as *const u16;
        let len = len_val.as_i32();
        if bytes.is_null() || len < 0 {
            return None;
        }
        Some((bytes, len))
    }

    unsafe fn dynamic_eq(&self, a: *mut hl::vdynamic, b: *mut hl::vdynamic) -> bool {
        if a == b {
            return true;
        }
        if a.is_null() || b.is_null() {
            return false;
        }
        let ta = (*a).t;
        let tb = (*b).t;
        if ta.is_null() || tb.is_null() {
            return false;
        }
        let ka = (*ta).kind;
        let kb = (*tb).kind;
        if ka == kb {
            return match ka {
                k if k == hl::hl_type_kind_HI32 => (*a).v.i == (*b).v.i,
                k if k == hl::hl_type_kind_HUI8 => (*a).v.ui8 == (*b).v.ui8,
                k if k == hl::hl_type_kind_HUI16 => (*a).v.ui16 == (*b).v.ui16,
                k if k == hl::hl_type_kind_HI64 => (*a).v.i64_ == (*b).v.i64_,
                k if k == hl::hl_type_kind_HF32 => (*a).v.f == (*b).v.f,
                k if k == hl::hl_type_kind_HF64 => (*a).v.d == (*b).v.d,
                k if k == hl::hl_type_kind_HBOOL => (*a).v.b == (*b).v.b,
                k if k == hl::hl_type_kind_HBYTES => {
                    Self::utf16z_eq((*a).v.bytes as *const u16, (*b).v.bytes as *const u16)
                }
                _ => {
                    if ka == hl::hl_type_kind_HOBJ {
                        let ta_name = self.dynamic_type_name(a);
                        let tb_name = self.dynamic_type_name(b);
                        if ta_name == tb_name
                            && matches!(ta_name.as_deref(), Some("String") | Some("S"))
                        {
                            if let (Some(sa), Some(sb)) =
                                (self.value_to_string(a), self.value_to_string(b))
                            {
                                return sa == sb;
                            }
                        }
                        let sa = self.try_extract_string_object(a);
                        let sb = self.try_extract_string_object(b);
                        if let (Some((ab, al)), Some((bb, bl))) = (sa, sb) {
                            return al == bl && Self::utf16_len_eq(ab, bb, al as usize);
                        }
                    }
                    (*a).v.ptr == (*b).v.ptr
                }
            };
        }
        // Cross-kind numeric equality (e.g. Int dynamic vs Float dynamic)
        let a_num = match ka {
            k if k == hl::hl_type_kind_HI32 => Some((*a).v.i as f64),
            k if k == hl::hl_type_kind_HUI8 => Some((*a).v.ui8 as f64),
            k if k == hl::hl_type_kind_HUI16 => Some((*a).v.ui16 as f64),
            k if k == hl::hl_type_kind_HI64 => Some((*a).v.i64_ as f64),
            k if k == hl::hl_type_kind_HF32 => Some((*a).v.f as f64),
            k if k == hl::hl_type_kind_HF64 => Some((*a).v.d),
            _ => None,
        };
        let b_num = match kb {
            k if k == hl::hl_type_kind_HI32 => Some((*b).v.i as f64),
            k if k == hl::hl_type_kind_HUI8 => Some((*b).v.ui8 as f64),
            k if k == hl::hl_type_kind_HUI16 => Some((*b).v.ui16 as f64),
            k if k == hl::hl_type_kind_HI64 => Some((*b).v.i64_ as f64),
            k if k == hl::hl_type_kind_HF32 => Some((*b).v.f as f64),
            k if k == hl::hl_type_kind_HF64 => Some((*b).v.d),
            _ => None,
        };
        match (a_num, b_num) {
            (Some(x), Some(y)) => x == y,
            _ => false,
        }
    }

    unsafe fn unbox_dynamic_to_kind(d: *mut hl::vdynamic, dst_kind: u32) -> Option<NanBoxedValue> {
        if d.is_null() || (*d).t.is_null() {
            return None;
        }
        let sk = (*(*d).t).kind;
        let as_i64 = match sk {
            hl::hl_type_kind_HI32 => Some((*d).v.i as i64),
            hl::hl_type_kind_HUI8 => Some((*d).v.ui8 as i64),
            hl::hl_type_kind_HUI16 => Some((*d).v.ui16 as i64),
            hl::hl_type_kind_HI64 => Some((*d).v.i64_),
            hl::hl_type_kind_HF32 => Some((*d).v.f as i64),
            hl::hl_type_kind_HF64 => Some((*d).v.d as i64),
            hl::hl_type_kind_HBOOL => Some(if (*d).v.b { 1 } else { 0 }),
            _ => None,
        };
        let as_f64 = match sk {
            hl::hl_type_kind_HI32 => Some((*d).v.i as f64),
            hl::hl_type_kind_HUI8 => Some((*d).v.ui8 as f64),
            hl::hl_type_kind_HUI16 => Some((*d).v.ui16 as f64),
            hl::hl_type_kind_HI64 => Some((*d).v.i64_ as f64),
            hl::hl_type_kind_HF32 => Some((*d).v.f as f64),
            hl::hl_type_kind_HF64 => Some((*d).v.d),
            hl::hl_type_kind_HBOOL => Some(if (*d).v.b { 1.0 } else { 0.0 }),
            _ => None,
        };
        match dst_kind {
            hl::hl_type_kind_HI32 => as_i64.map(|v| NanBoxedValue::from_i32(v as i32)),
            hl::hl_type_kind_HUI8 => as_i64.map(|v| NanBoxedValue::from_i32((v as u8) as i32)),
            hl::hl_type_kind_HUI16 => as_i64.map(|v| NanBoxedValue::from_i32((v as u16) as i32)),
            hl::hl_type_kind_HI64 => as_i64.map(NanBoxedValue::from_i64),
            hl::hl_type_kind_HF32 | hl::hl_type_kind_HF64 => as_f64.map(NanBoxedValue::from_f64),
            hl::hl_type_kind_HBOOL => as_i64.map(|v| NanBoxedValue::from_bool(v != 0)),
            hl::hl_type_kind_HBYTES => {
                if sk == hl::hl_type_kind_HBYTES {
                    Some(NanBoxedValue::from_bytes_ptr((*d).v.bytes as usize))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Call a native function via FFI.
    fn call_native(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        native_idx: usize,
        args: &[NanBoxedValue],
    ) -> Result<NanBoxedValue> {
        let native = &bytecode.natives[native_idx];

        // Trace every native call when ASH_TRACE_NATIVE is set
        if env_flag!("ASH_TRACE_NATIVE") {
            eprintln!(
                "[trace] native hlp_{} lib={} args={}",
                native.name,
                native.lib,
                args.len()
            );
        }

        let debug_native = env_flag!("ASH_DBG_NATIVE");
        let func_name = HlpName(&native.name);
        if debug_native
            && (native.name.contains("compare")
                || native.name.contains("eq")
                || native.name.contains("trim")
                || native.name.contains("date"))
        {
            eprintln!("[NATIVE] {} args={} vals={:?}", func_name, args.len(), args);
        }
        let debug_dyn = env_flag!("ASH_DBG_DYN");
        if debug_dyn
            && (native.name == "hash"
                || native.name == "obj_get_field"
                || native.name == "obj_set_field"
                || native.name == "obj_has_field"
                || native.name == "obj_delete_field"
                || native.name == "no_closure"
                || native.name == "get_closure_value"
                || native.name == "call_method")
        {
            eprintln!(
                "[NATIVE-DYN] {} args={} vals={:?}",
                func_name,
                args.len(),
                args
            );
            if (native.name == "obj_get_field"
                || native.name == "obj_set_field"
                || native.name == "obj_has_field"
                || native.name == "obj_delete_field"
                || native.name == "no_closure"
                || native.name == "get_closure_value"
                || native.name == "call_method")
                && !args.is_empty()
                && args[0].is_ptr()
                && args[0].as_ptr() != 0
            {
                let d = args[0].as_ptr() as *mut hl::vdynamic;
                unsafe {
                    if !d.is_null() && !(*d).t.is_null() {
                        eprintln!(
                            "[NATIVE-DYN] {} obj_kind={} obj_t={:p}",
                            func_name,
                            (*(*d).t).kind,
                            (*d).t
                        );
                    }
                    if native.name == "obj_set_field"
                        && args.len() >= 3
                        && args[2].is_ptr()
                        && args[2].as_ptr() != 0
                    {
                        let v = args[2].as_ptr() as *mut hl::vdynamic;
                        if !v.is_null() && !(*v).t.is_null() {
                            eprintln!(
                                "[NATIVE-DYN] {} val_kind={} val_t={:p}",
                                func_name,
                                (*(*v).t).kind,
                                (*v).t
                            );
                        }
                    }
                }
            }
        }

        // Intercept sort natives: they call back into bytecode closures via C function pointers,
        // which doesn't work in interpreter mode. Implement sorting here instead.
        match native.name.as_str() {
            "bsort_i32" => return self.sort_bytes_i32(bytecode, native_resolver, args),
            "bsort_f64" => return self.sort_bytes_f64(bytecode, native_resolver, args),
            "bsort_i64" => return self.sort_bytes_i64(bytecode, native_resolver, args),
            "call_method" => {
                if let Some(v) =
                    self.try_handle_call_method_native(bytecode, native_resolver, args)?
                {
                    return Ok(v);
                }
            }
            // Reflect/hl.Api field operations go to the ash_std natives like
            // every other caller. They used to be intercepted into
            // interpreter-PRIVATE shadow HashMaps ("HVIRTUAL fallback
            // storage") that the real object never learned about — state in
            // one world again, and the reason a Reflect.setField was lost
            // the moment any tier compiled a Reflect wrapper: the compiled
            // wrapper calls hlp_obj_get_field directly and reads the actual
            // object, while the interpreted setField had only fed the shadow.
            // (Traced end-to-end on test_feature_typedef_anon at
            // --jit-threshold 1: [VSET] stored shadow maps 1/1, the read
            // never re-entered the interpreter, hlp_obj_get_field returned
            // the stale field.) The natives handle virtuals correctly since
            // the Phase-11/12 fixes; the crutch now only creates divergence.
            // macOS OpenGL core profile jumps from GLSL 1.20 → 1.50 (no 1.30/1.40).
            // Heaps probes #version 130, fails, and treats it as fatal.
            // Patch the source bytes in-place so the probe succeeds.
            #[cfg(target_os = "macos")]
            "gl_shader_source" if args.len() >= 2 && args[1].is_ptr() => {
                let obj_ptr = args[1].as_ptr() as *const u8;
                if !obj_ptr.is_null() && (obj_ptr as usize) > 0x10000 {
                    // HOBJ String: UTF-16 data pointer at offset 8
                    let data_ptr = unsafe { *(obj_ptr.add(8) as *const *mut u16) };
                    if !data_ptr.is_null() && (data_ptr as usize) > 0x10000 {
                        // Read first 12 UTF-16 chars: "#version 1X0"
                        let prefix = unsafe { std::slice::from_raw_parts(data_ptr, 12) };
                        let prefix_str = String::from_utf16_lossy(prefix);
                        if prefix_str.starts_with("#version 130")
                            || prefix_str.starts_with("#version 140")
                        {
                            // Patch char at index 10: '3'/'4' → '5' (UTF-16 LE)
                            unsafe {
                                *data_ptr.add(10) = b'5' as u16;
                            }
                        }
                    }
                }
                // Fall through to normal dispatch
            }
            // No thread/event/lock interceptions needed — the stdlib's
            // non-blocking lock_wait handles single-threaded mode correctly.
            _ => {}
        }

        // Resolve the native function pointer: per-native cache first, then
        // the process-global symbol table (falls back to lazy dlsym once).
        let mut func_ptr = self
            .native_fn_cache
            .get(native_idx)
            .copied()
            .unwrap_or(std::ptr::null_mut());
        if func_ptr.is_null() {
            func_ptr =
                native_resolver.resolve_function(&native.lib, &format!("hlp_{}", native.name))?;
            if let Some(slot) = self.native_fn_cache.get_mut(native_idx) {
                *slot = func_ptr;
            }
        }

        // Get the function type signature for type-aware marshaling
        let type_fun = bytecode.types[native.type_.0]
            .fun
            .as_ref()
            .ok_or_else(|| anyhow!("Native {} has no function type", func_name))?;

        // Get return type kind for wrapping the result
        let ret_kind = bytecode.types[type_fun.ret.0].kind;

        // Get argument type kinds for extraction
        let arg_kinds: Vec<u32> = type_fun
            .args
            .iter()
            .map(|a| bytecode.types[a.0].kind)
            .collect();
        if debug_dyn
            && (native.name == "obj_get_field"
                || native.name == "obj_set_field"
                || native.name == "obj_has_field"
                || native.name == "obj_delete_field"
                || native.name == "no_closure"
                || native.name == "get_closure_value"
                || native.name == "call_method")
        {
            eprintln!(
                "[NATIVE-DYN] {} arg_kinds={:?} ret_kind={}",
                func_name, arg_kinds, ret_kind
            );
        }

        // Check if any argument or return type involves floats.
        // On ARM64, floats use separate FP registers (d0-d7) vs integer registers (x0-x7),
        // so we must use typed dispatch with explicit f64 in the right positions.
        let is_float_kind = |k: u32| k == hl::hl_type_kind_HF32 || k == hl::hl_type_kind_HF64;
        let ret_is_float = is_float_kind(ret_kind);
        let float_mask: u32 = arg_kinds.iter().enumerate().fold(0u32, |acc, (i, &k)| {
            if is_float_kind(k) {
                acc | (1 << i)
            } else {
                acc
            }
        });

        // Set up a setjmp/longjmp trap so hlp_throw can propagate through native C ABI safely.
        // This covers BOTH float and integer dispatch paths.
        let fn_setup_trap = self.fn_setup_trap_jit;
        let fn_remove_trap = self.fn_remove_trap_jit;
        let fn_get_exc = self.fn_get_exc_value;
        let fn_clear_exc = self.fn_clear_exc_value;
        // Same frame-stack invariant as `call_compiled_function`: a native that
        // re-enters the interpreter (closure runner, dynamic dispatch) and then
        // throws longjmps straight back here, leaving the frames it pushed
        // behind.
        let stack_depth = self.stack.len();
        let mut trap_installed = false;
        if !fn_setup_trap.is_null() {
            type FnSetupTrap = unsafe extern "C" fn() -> *mut c_void;
            let setup: FnSetupTrap = unsafe { std::mem::transmute(fn_setup_trap) };
            let jmp_buf = unsafe { setup() };
            if !jmp_buf.is_null() {
                trap_installed = true;
                let jumped = unsafe { call_setjmp_opaque(jmp_buf) };
                if jumped != 0 {
                    for f in self.stack.drain(stack_depth..) {
                        if self.reg_pool.len() < POOL_CAP {
                            self.reg_pool.push(f.into_buffer());
                        }
                    }
                    self.sync_gc_scan_roots();
                    if !fn_get_exc.is_null() {
                        type FnGetExc = unsafe extern "C" fn() -> *mut c_void;
                        let exc_ptr =
                            unsafe { (std::mem::transmute::<*mut c_void, FnGetExc>(fn_get_exc))() };
                        if !exc_ptr.is_null() {
                            if !fn_clear_exc.is_null() {
                                type FnClearExc = unsafe extern "C" fn();
                                unsafe {
                                    (std::mem::transmute::<*mut c_void, FnClearExc>(fn_clear_exc))()
                                };
                            }
                            return Err(anyhow::Error::new(
                                self.format_hl_exception(NanBoxedValue::from_ptr(exc_ptr as usize)),
                            ));
                        }
                    }
                    return Err(anyhow!(
                        "Native longjmp without exception value: {}",
                        func_name
                    ));
                }
            }
        }

        if ret_is_float || float_mask != 0 {
            // Arm recovery for float-dispatch native calls too
            let recovered = unsafe { crate::native_recovery::arm_native_recovery() };
            if recovered != 0 {
                crate::native_recovery::disarm_native_recovery();
                if trap_installed && !fn_remove_trap.is_null() {
                    type FnRemoveTrap = unsafe extern "C" fn();
                    unsafe { (std::mem::transmute::<*mut c_void, FnRemoveTrap>(fn_remove_trap))() };
                }
                let sig = crate::native_recovery::last_recovery_signal();
                let addr = crate::native_recovery::last_recovery_fault_addr();
                eprintln!(
                    "[ash] Recovered from signal {} (fault_addr={:#x}) in native float call: {}",
                    sig, addr, func_name
                );
                return Ok(self.wrap_native_result(0i64, ret_kind));
            }
            let raw =
                self.dispatch_float_native(func_ptr, args, &arg_kinds, float_mask, ret_is_float);
            crate::native_recovery::disarm_native_recovery();
            if trap_installed && !fn_remove_trap.is_null() {
                type FnRemoveTrap = unsafe extern "C" fn();
                unsafe { (std::mem::transmute::<*mut c_void, FnRemoveTrap>(fn_remove_trap))() };
            }
            return Ok(self.wrap_native_result(raw?, ret_kind));
        }

        // Type-aware argument extraction.
        // For HNULL parameters: if the value is a primitive (I32/F64/Bool),
        // box it into a vdynamic via hlp_make_dyn so the native gets a pointer.
        let extract_arg = |idx: usize| -> i64 {
            let kind = if idx < arg_kinds.len() {
                arg_kinds[idx]
            } else {
                0 // HVOID fallback
            };

            // HNULL(T) parameters expect a vdynamic* pointer, not raw values
            if env_flag!("ASH_DBG_ALLOC") && kind == hl::hl_type_kind_HNULL {
                eprintln!(
                    "[extract_arg] idx={} kind=HNULL val={:?} is_i32={} is_ptr={}",
                    idx,
                    args[idx],
                    args[idx].is_i32(),
                    args[idx].is_ptr()
                );
            }
            if kind == hl::hl_type_kind_HNULL && !self.fn_make_dyn.is_null() {
                let val = args[idx];
                if val.is_null() || val.is_void() {
                    return 0; // null pointer
                }
                if val.is_i32()
                    || val.is_f64()
                    || val.is_bool()
                    || (val.is_ptr() && val.as_ptr() < 0x10000)
                {
                    // Box the primitive into a vdynamic
                    // Determine the inner type from the type signature
                    let inner_type_idx = if idx < type_fun.args.len() {
                        let arg_type = &bytecode.types[type_fun.args[idx].0];
                        arg_type.tparam.as_ref().map(|t| t.0).unwrap_or(0)
                    } else {
                        0
                    };
                    let inner_c_type = self.c_type_factory.get(inner_type_idx) as *mut c_void;
                    let mut data: i64 = if val.is_i32() {
                        val.as_i32() as i64
                    } else if val.is_f64() {
                        val.as_f64().to_bits() as i64
                    } else {
                        val.as_bool() as i64
                    };
                    let make_dyn: unsafe extern "C" fn(*mut c_void, *mut c_void) -> *mut c_void =
                        unsafe { std::mem::transmute(self.fn_make_dyn) };
                    let boxed =
                        unsafe { make_dyn(&mut data as *mut i64 as *mut c_void, inner_c_type) };
                    return boxed as i64;
                }
            }

            self.value_to_i64(args[idx], kind)
        };

        if args.len() > 12 {
            if trap_installed && !fn_remove_trap.is_null() {
                type FnRemoveTrap = unsafe extern "C" fn();
                unsafe { (std::mem::transmute::<*mut c_void, FnRemoveTrap>(fn_remove_trap))() };
            }
            return Err(anyhow!(
                "Native call with {} args not yet supported",
                args.len()
            ));
        }

        // Arm the native call recovery point so SIGSEGV/SIGBUS from native code
        // (e.g., macOS GL driver bugs) is caught and turned into a recoverable error.
        let recovered = unsafe { crate::native_recovery::arm_native_recovery() };
        if recovered != 0 {
            // We got here via siglongjmp from the signal handler
            crate::native_recovery::disarm_native_recovery();
            if trap_installed && !fn_remove_trap.is_null() {
                type FnRemoveTrap = unsafe extern "C" fn();
                unsafe { (std::mem::transmute::<*mut c_void, FnRemoveTrap>(fn_remove_trap))() };
            }
            let sig = crate::native_recovery::last_recovery_signal();
            let addr = crate::native_recovery::last_recovery_fault_addr();
            let sig_name = match sig {
                11 => "SIGSEGV",
                10 => "SIGBUS",
                _ => "SIGNAL",
            };
            eprintln!(
                "[ash] Recovered from {} (fault_addr={:#x}) in native call: {}",
                sig_name, addr, func_name
            );
            return Ok(self.wrap_native_result(0i64, ret_kind));
        }

        // Dispatch based on argument count, using type-aware extraction and wrapping.
        let raw_result: i64 = unsafe {
            match args.len() {
                0 => {
                    let f: unsafe extern "C" fn() -> i64 = std::mem::transmute(func_ptr);
                    f()
                }
                1 => {
                    let f: unsafe extern "C" fn(i64) -> i64 = std::mem::transmute(func_ptr);
                    f(extract_arg(0))
                }
                2 => {
                    let f: unsafe extern "C" fn(i64, i64) -> i64 = std::mem::transmute(func_ptr);
                    f(extract_arg(0), extract_arg(1))
                }
                3 => {
                    let f: unsafe extern "C" fn(i64, i64, i64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(extract_arg(0), extract_arg(1), extract_arg(2))
                }
                4 => {
                    let f: unsafe extern "C" fn(i64, i64, i64, i64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(
                        extract_arg(0),
                        extract_arg(1),
                        extract_arg(2),
                        extract_arg(3),
                    )
                }
                5 => {
                    let f: unsafe extern "C" fn(i64, i64, i64, i64, i64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(
                        extract_arg(0),
                        extract_arg(1),
                        extract_arg(2),
                        extract_arg(3),
                        extract_arg(4),
                    )
                }
                6 => {
                    let f: unsafe extern "C" fn(i64, i64, i64, i64, i64, i64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(
                        extract_arg(0),
                        extract_arg(1),
                        extract_arg(2),
                        extract_arg(3),
                        extract_arg(4),
                        extract_arg(5),
                    )
                }
                7 => {
                    let f: unsafe extern "C" fn(i64, i64, i64, i64, i64, i64, i64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(
                        extract_arg(0),
                        extract_arg(1),
                        extract_arg(2),
                        extract_arg(3),
                        extract_arg(4),
                        extract_arg(5),
                        extract_arg(6),
                    )
                }
                8 => {
                    let f: unsafe extern "C" fn(i64, i64, i64, i64, i64, i64, i64, i64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(
                        extract_arg(0),
                        extract_arg(1),
                        extract_arg(2),
                        extract_arg(3),
                        extract_arg(4),
                        extract_arg(5),
                        extract_arg(6),
                        extract_arg(7),
                    )
                }
                9 => {
                    let f: unsafe extern "C" fn(
                        i64,
                        i64,
                        i64,
                        i64,
                        i64,
                        i64,
                        i64,
                        i64,
                        i64,
                    ) -> i64 = std::mem::transmute(func_ptr);
                    f(
                        extract_arg(0),
                        extract_arg(1),
                        extract_arg(2),
                        extract_arg(3),
                        extract_arg(4),
                        extract_arg(5),
                        extract_arg(6),
                        extract_arg(7),
                        extract_arg(8),
                    )
                }
                10 => {
                    let f: unsafe extern "C" fn(
                        i64,
                        i64,
                        i64,
                        i64,
                        i64,
                        i64,
                        i64,
                        i64,
                        i64,
                        i64,
                    ) -> i64 = std::mem::transmute(func_ptr);
                    f(
                        extract_arg(0),
                        extract_arg(1),
                        extract_arg(2),
                        extract_arg(3),
                        extract_arg(4),
                        extract_arg(5),
                        extract_arg(6),
                        extract_arg(7),
                        extract_arg(8),
                        extract_arg(9),
                    )
                }
                11 => {
                    let f: unsafe extern "C" fn(
                        i64,
                        i64,
                        i64,
                        i64,
                        i64,
                        i64,
                        i64,
                        i64,
                        i64,
                        i64,
                        i64,
                    ) -> i64 = std::mem::transmute(func_ptr);
                    f(
                        extract_arg(0),
                        extract_arg(1),
                        extract_arg(2),
                        extract_arg(3),
                        extract_arg(4),
                        extract_arg(5),
                        extract_arg(6),
                        extract_arg(7),
                        extract_arg(8),
                        extract_arg(9),
                        extract_arg(10),
                    )
                }
                12 => {
                    let f: unsafe extern "C" fn(
                        i64,
                        i64,
                        i64,
                        i64,
                        i64,
                        i64,
                        i64,
                        i64,
                        i64,
                        i64,
                        i64,
                        i64,
                    ) -> i64 = std::mem::transmute(func_ptr);
                    f(
                        extract_arg(0),
                        extract_arg(1),
                        extract_arg(2),
                        extract_arg(3),
                        extract_arg(4),
                        extract_arg(5),
                        extract_arg(6),
                        extract_arg(7),
                        extract_arg(8),
                        extract_arg(9),
                        extract_arg(10),
                        extract_arg(11),
                    )
                }
                _ => 0i64, // arg count is pre-validated above
            }
        };

        // Disarm recovery after successful call
        crate::native_recovery::disarm_native_recovery();

        if trap_installed && !fn_remove_trap.is_null() {
            type FnRemoveTrap = unsafe extern "C" fn();
            unsafe { (std::mem::transmute::<*mut c_void, FnRemoveTrap>(fn_remove_trap))() };
        }

        // Wrap return value using the correct NanBoxedValue type
        let wrapped = self.wrap_native_result(raw_result, ret_kind);
        if debug_native
            && (native.name.contains("compare")
                || native.name.contains("eq")
                || native.name.contains("trim")
                || native.name.contains("date"))
        {
            eprintln!(
                "[NATIVE] {} -> raw={} wrapped={:?}",
                func_name, raw_result, wrapped
            );
        }
        if debug_dyn
            && (native.name == "hash"
                || native.name == "obj_get_field"
                || native.name == "obj_set_field"
                || native.name == "obj_has_field"
                || native.name == "obj_delete_field"
                || native.name == "no_closure"
                || native.name == "get_closure_value"
                || native.name == "call_method")
        {
            eprintln!(
                "[NATIVE-DYN] {} -> raw={} wrapped={:?}",
                func_name, raw_result, wrapped
            );
            if wrapped.is_ptr() && wrapped.as_ptr() != 0 {
                let d = wrapped.as_ptr() as *mut hl::vdynamic;
                unsafe {
                    if !d.is_null() && !(*d).t.is_null() {
                        eprintln!(
                            "[NATIVE-DYN] {} result_kind={} result_t={:p}",
                            func_name,
                            (*(*d).t).kind,
                            (*d).t
                        );
                    }
                }
            }
        }
        Ok(wrapped)
    }

    /// Dispatch a native call that involves float arguments or float return value.
    ///
    /// Extract (findex, optional_bound_value) from a closure NanBoxedValue.
    ///
    /// Closures can be stored as:
    /// - TAG_FUNC: just a function index (StaticClosure with no capture)
    /// - TAG_PTR: pointer to a _vclosure struct (InstanceClosure or heap-allocated)
    fn closure_findex_and_value(&self, val: NanBoxedValue) -> (usize, Option<NanBoxedValue>) {
        if val.is_func() {
            (val.as_func_index(), None)
        } else if val.is_ptr() {
            let cl_ptr = val.as_ptr() as *const hl::_vclosure;
            unsafe {
                let stub = (*cl_ptr).fun as usize;
                let findex = stub.wrapping_sub(1);
                let bound = if (*cl_ptr).hasValue != 0 && !(*cl_ptr).value.is_null() {
                    Some(NanBoxedValue::from_ptr((*cl_ptr).value as usize))
                } else {
                    None
                };
                (findex, bound)
            }
        } else {
            // Fallback: treat raw i32 payload as findex
            (val.as_ptr(), None)
        }
    }

    /// Call a closure value (FUNC-tagged or PTR-to-vclosure) with the given arguments.
    /// Prepends the bound value if the closure has one (InstanceClosure pattern).
    fn call_closure_val(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        closure_val: NanBoxedValue,
        args: Vec<NanBoxedValue>,
    ) -> Result<NanBoxedValue> {
        let (findex, bound) = self.closure_findex_and_value(closure_val);
        let mut full_args = args;
        if let Some(v) = bound {
            full_args.insert(0, v);
        }
        self.call_function(bytecode, native_resolver, findex, &full_args)
    }

    fn dynamic_to_value_for_kind(&self, d: *mut hl::vdynamic, dst_kind: u32) -> NanBoxedValue {
        if d.is_null() {
            return NanBoxedValue::null();
        }
        if dst_kind == hl::hl_type_kind_HDYN {
            return NanBoxedValue::from_ptr(d as usize);
        }
        let sk = unsafe {
            if (*d).t.is_null() {
                return NanBoxedValue::null();
            }
            (*(*d).t).kind
        };
        if Self::is_primitive_or_bytes_kind(dst_kind) {
            return unsafe { Self::unbox_dynamic_to_kind(d, dst_kind) }
                .unwrap_or(NanBoxedValue::null());
        }
        if sk == dst_kind {
            match sk {
                hl::hl_type_kind_HOBJ
                | hl::hl_type_kind_HSTRUCT
                | hl::hl_type_kind_HARRAY
                | hl::hl_type_kind_HFUN
                | hl::hl_type_kind_HVIRTUAL
                | hl::hl_type_kind_HDYNOBJ
                | hl::hl_type_kind_HENUM => {
                    return NanBoxedValue::from_ptr(d as usize);
                }
                hl::hl_type_kind_HBYTES => {
                    let p = unsafe { (*d).v.bytes } as usize;
                    return if p == 0 {
                        NanBoxedValue::null()
                    } else {
                        NanBoxedValue::from_ptr(p)
                    };
                }
                _ => {
                    let p = unsafe { (*d).v.ptr } as usize;
                    return if p == 0 {
                        NanBoxedValue::null()
                    } else {
                        NanBoxedValue::from_ptr(p)
                    };
                }
            }
        }
        if sk == hl::hl_type_kind_HBYTES {
            let p = unsafe { (*d).v.bytes } as usize;
            return if p == 0 {
                NanBoxedValue::null()
            } else {
                NanBoxedValue::from_ptr(p)
            };
        }
        let p = unsafe { (*d).v.ptr } as usize;
        if p == 0 {
            NanBoxedValue::null()
        } else {
            NanBoxedValue::from_ptr(p)
        }
    }

    fn closure_arg_kinds_and_ret_type(
        &self,
        bytecode: &DecodedBytecode,
        findex: usize,
    ) -> Option<(Vec<u32>, usize)> {
        if let Some(fidx) = func_of(&self.targets, findex) {
            let t_idx = bytecode.functions[fidx].type_.0;
            let tf = bytecode.types[t_idx].fun.as_ref()?;
            let arg_kinds = tf
                .args
                .iter()
                .map(|a| bytecode.types[a.0].kind)
                .collect::<Vec<_>>();
            return Some((arg_kinds, tf.ret.0));
        }
        if let Some(nidx) = native_of(&self.targets, findex) {
            let t_idx = bytecode.natives[nidx].type_.0;
            let tf = bytecode.types[t_idx].fun.as_ref()?;
            let arg_kinds = tf
                .args
                .iter()
                .map(|a| bytecode.types[a.0].kind)
                .collect::<Vec<_>>();
            return Some((arg_kinds, tf.ret.0));
        }
        None
    }

    fn try_handle_call_method_native(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        args: &[NanBoxedValue],
    ) -> Result<Option<NanBoxedValue>> {
        let dbg = env_flag!("ASH_DBG_DYN");
        if args.len() < 2
            || args[0].is_null()
            || args[0].is_void()
            || args[1].is_null()
            || args[1].is_void()
        {
            return Ok(Some(NanBoxedValue::null()));
        }

        let closure_val = args[0];
        let varray_ptr = args[1].as_ptr() as *const hl::varray;
        if varray_ptr.is_null() {
            return Ok(Some(NanBoxedValue::null()));
        }

        let (findex, bound) = self.closure_findex_and_value(closure_val);
        let (arg_kinds, ret_type_idx) = self
            .closure_arg_kinds_and_ret_type(bytecode, findex)
            .unwrap_or((Vec::new(), 0));
        let arg_shift = if bound.is_some() { 1usize } else { 0usize };
        if dbg {
            eprintln!(
                "[CALL_METHOD] findex={} bound={} arg_kinds={:?} ret_type_idx={}",
                findex,
                bound.is_some(),
                arg_kinds,
                ret_type_idx
            );
        }

        let argc = unsafe { (*varray_ptr).size.max(0) as usize };
        let data_ptr = unsafe {
            (varray_ptr as *const u8).add(std::mem::size_of::<hl::varray>())
                as *const *mut hl::vdynamic
        };

        let mut call_args = Vec::with_capacity(argc);
        for i in 0..argc {
            let dyn_arg = unsafe { *data_ptr.add(i) };
            let expected_kind = arg_kinds
                .get(i + arg_shift)
                .copied()
                .unwrap_or(hl::hl_type_kind_HDYN);
            let v = self.dynamic_to_value_for_kind(dyn_arg, expected_kind);
            if dbg {
                let sk = unsafe {
                    if dyn_arg.is_null() || (*dyn_arg).t.is_null() {
                        0
                    } else {
                        (*(*dyn_arg).t).kind
                    }
                };
                eprintln!(
                    "[CALL_METHOD] arg{} dyn={:p} sk={} expect={} -> {:?}",
                    i, dyn_arg, sk, expected_kind, v
                );
            }
            call_args.push(v);
        }

        let ret = self.call_closure_val(bytecode, native_resolver, closure_val, call_args)?;
        if dbg {
            eprintln!("[CALL_METHOD] raw_ret={:?}", ret);
        }
        let out = if ret.is_void() {
            NanBoxedValue::null()
        } else {
            let ret_t = self.c_type_factory.get(ret_type_idx) as *mut hl_type;
            self.box_value_as_dynamic_with_type(ret, ret_t)
        };
        if dbg {
            eprintln!("[CALL_METHOD] out={:?}", out);
        }
        Ok(Some(out))
    }

    /// Cooperative SDL event pump: resolve SDL_PollEvent, poll all pending events,
    /// and call the Heaps event callback for each via the interpreter.
    /// Cooperative SDL event pump + buffer swap.
    /// Pumps SDL events (so the window stays responsive and close works)
    /// and swaps the GL buffer (so rendered content is presented).
    /// Returns false if SDL_QUIT was received (app should exit).
    fn pump_events_and_swap(&mut self) -> bool {
        let mut alive = true;
        unsafe {
            let poll = libc::dlsym(libc::RTLD_DEFAULT, b"SDL_PollEvent\0".as_ptr() as *const i8);
            let swap = libc::dlsym(
                libc::RTLD_DEFAULT,
                b"SDL_GL_SwapWindow\0".as_ptr() as *const i8,
            );
            let get_win = libc::dlsym(
                libc::RTLD_DEFAULT,
                b"SDL_GL_GetCurrentWindow\0".as_ptr() as *const i8,
            );

            if !poll.is_null() {
                let poll_fn: unsafe extern "C" fn(*mut u8) -> i32 = std::mem::transmute(poll);
                let mut event_buf = [0u8; 128]; // SDL_Event union
                while poll_fn(event_buf.as_mut_ptr()) != 0 {
                    let event_type = u32::from_ne_bytes([
                        event_buf[0],
                        event_buf[1],
                        event_buf[2],
                        event_buf[3],
                    ]);
                    // Log first few event types for debugging
                    static EVT_LOG_COUNT: std::sync::atomic::AtomicU32 =
                        std::sync::atomic::AtomicU32::new(0);
                    let c = EVT_LOG_COUNT.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                    if c < 20 || event_type == 0x100 {
                        eprintln!("[ash] SDL event type={:#x} ({})", event_type, event_type);
                    }
                    if event_type == 0x100 {
                        // SDL_QUIT
                        eprintln!("[ash] SDL_QUIT received, exiting");
                        alive = false;
                    }
                }
            }

            // Swap GL buffers
            if !swap.is_null() && !get_win.is_null() {
                let get_win_fn: unsafe extern "C" fn() -> *mut c_void =
                    std::mem::transmute(get_win);
                let swap_fn: unsafe extern "C" fn(*mut c_void) = std::mem::transmute(swap);
                let window = get_win_fn();
                static SWAP_LOGGED: std::sync::atomic::AtomicBool =
                    std::sync::atomic::AtomicBool::new(false);
                if !SWAP_LOGGED.swap(true, std::sync::atomic::Ordering::Relaxed) {
                    eprintln!(
                        "[ash] SDL_GL_SwapWindow: window={:p} swap_fn={:p}",
                        window, swap as *const ()
                    );
                }
                if !window.is_null() {
                    swap_fn(window);
                }
            }
        }
        alive
    }

    /// Interpreter-side implementation of bsort_i32 that uses the interpreter's
    /// call mechanism for the comparator closure (bytecode closures can't be called
    /// as raw C functions in interpreter mode).
    fn sort_bytes_i32(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        args: &[NanBoxedValue],
    ) -> Result<NanBoxedValue> {
        if args.len() < 4 {
            return Ok(NanBoxedValue::void());
        }
        let bytes_ptr = args[0].as_ptr() as *mut i32;
        let pos = args[1].as_i32() as isize;
        let len = args[2].as_i32() as usize;
        let cmp_val = args[3];

        if len == 0 || bytes_ptr as usize == 0 {
            return Ok(NanBoxedValue::void());
        }

        let mut data: Vec<i32> =
            unsafe { std::slice::from_raw_parts(bytes_ptr.offset(pos), len) }.to_vec();

        // Use raw pointer to avoid borrow conflict inside sort_by closure
        let self_raw = self as *mut Self;
        let bytecode_raw = bytecode as *const DecodedBytecode;
        let resolver_raw = native_resolver as *const ash::native_lib::NativeFunctionResolver;
        let mut sort_err: Option<anyhow::Error> = None;

        data.sort_by(|&a, &b| {
            if sort_err.is_some() {
                return std::cmp::Ordering::Equal;
            }
            let interp = unsafe { &mut *self_raw };
            let bc = unsafe { &*bytecode_raw };
            let nr = unsafe { &*resolver_raw };
            let call_args = vec![NanBoxedValue::from_i32(a), NanBoxedValue::from_i32(b)];
            match interp.call_closure_val(bc, nr, cmp_val, call_args) {
                Ok(r) => r.as_i32().cmp(&0),
                Err(e) => {
                    sort_err = Some(e);
                    std::cmp::Ordering::Equal
                }
            }
        });

        if let Some(e) = sort_err {
            return Err(e);
        }

        unsafe {
            let slice = std::slice::from_raw_parts_mut(bytes_ptr.offset(pos), len);
            slice.copy_from_slice(&data);
        }
        Ok(NanBoxedValue::void())
    }

    /// Interpreter-side bsort_i64.
    fn sort_bytes_i64(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        args: &[NanBoxedValue],
    ) -> Result<NanBoxedValue> {
        if args.len() < 4 {
            return Ok(NanBoxedValue::void());
        }
        let bytes_ptr = args[0].as_ptr() as *mut i64;
        let pos = args[1].as_i32() as isize;
        let len = args[2].as_i32() as usize;
        let cmp_val = args[3];

        if len == 0 || bytes_ptr as usize == 0 {
            return Ok(NanBoxedValue::void());
        }

        let mut data: Vec<i64> =
            unsafe { std::slice::from_raw_parts(bytes_ptr.offset(pos), len) }.to_vec();

        let self_raw = self as *mut Self;
        let bytecode_raw = bytecode as *const DecodedBytecode;
        let resolver_raw = native_resolver as *const ash::native_lib::NativeFunctionResolver;
        let mut sort_err: Option<anyhow::Error> = None;

        data.sort_by(|&a, &b| {
            if sort_err.is_some() {
                return std::cmp::Ordering::Equal;
            }
            let interp = unsafe { &mut *self_raw };
            let bc = unsafe { &*bytecode_raw };
            let nr = unsafe { &*resolver_raw };
            let call_args = vec![NanBoxedValue::from_i64(a), NanBoxedValue::from_i64(b)];
            match interp.call_closure_val(bc, nr, cmp_val, call_args) {
                Ok(r) => r.as_i32().cmp(&0),
                Err(e) => {
                    sort_err = Some(e);
                    std::cmp::Ordering::Equal
                }
            }
        });

        if let Some(e) = sort_err {
            return Err(e);
        }

        unsafe {
            let slice = std::slice::from_raw_parts_mut(bytes_ptr.offset(pos), len);
            slice.copy_from_slice(&data);
        }
        Ok(NanBoxedValue::void())
    }

    /// Interpreter-side bsort_f64.
    fn sort_bytes_f64(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        args: &[NanBoxedValue],
    ) -> Result<NanBoxedValue> {
        if args.len() < 4 {
            return Ok(NanBoxedValue::void());
        }
        let bytes_ptr = args[0].as_ptr() as *mut f64;
        let pos = args[1].as_i32() as isize;
        let len = args[2].as_i32() as usize;
        let cmp_val = args[3];

        if len == 0 || bytes_ptr as usize == 0 {
            return Ok(NanBoxedValue::void());
        }

        let mut data: Vec<f64> =
            unsafe { std::slice::from_raw_parts(bytes_ptr.offset(pos), len) }.to_vec();

        let self_raw = self as *mut Self;
        let bytecode_raw = bytecode as *const DecodedBytecode;
        let resolver_raw = native_resolver as *const ash::native_lib::NativeFunctionResolver;
        let mut sort_err: Option<anyhow::Error> = None;

        data.sort_by(|&a, &b| {
            if sort_err.is_some() {
                return std::cmp::Ordering::Equal;
            }
            let interp = unsafe { &mut *self_raw };
            let bc = unsafe { &*bytecode_raw };
            let nr = unsafe { &*resolver_raw };
            let call_args = vec![NanBoxedValue::from_f64(a), NanBoxedValue::from_f64(b)];
            match interp.call_closure_val(bc, nr, cmp_val, call_args) {
                Ok(r) => r.as_i32().cmp(&0),
                Err(e) => {
                    sort_err = Some(e);
                    std::cmp::Ordering::Equal
                }
            }
        });

        if let Some(e) = sort_err {
            return Err(e);
        }

        unsafe {
            let slice = std::slice::from_raw_parts_mut(bytes_ptr.offset(pos), len);
            slice.copy_from_slice(&data);
        }
        Ok(NanBoxedValue::void())
    }

    /// On ARM64 (and x86-64), floating-point arguments go into FP registers (d0-d7 / xmm0-xmm7),
    /// separate from integer/pointer registers (x0-x7 / rdi-rdi). Using a generic
    /// `fn(i64,...)->i64` transmute would put float bits into the wrong registers.
    ///
    /// This function uses typed Rust fn signatures to ensure the compiler emits
    /// correct calling-convention instructions for each pattern.
    ///
    /// Returns the raw i64 result (float results are returned as their bit representation).
    fn dispatch_float_native(
        &self,
        func_ptr: *mut std::ffi::c_void,
        args: &[NanBoxedValue],
        arg_kinds: &[u32],
        float_mask: u32,
        ret_is_float: bool,
    ) -> Result<i64> {
        let gf = |i: usize| -> f64 { args[i].as_f64() };
        let gi = |i: usize| -> i64 { self.value_to_i64(args[i], arg_kinds[i]) };

        let raw: i64 = unsafe {
            match (args.len(), ret_is_float, float_mask) {
                // --- 0 args ---
                (0, true, 0b0) => {
                    // () -> f64
                    let f: unsafe extern "C" fn() -> f64 = std::mem::transmute(func_ptr);
                    f().to_bits() as i64
                }
                // --- 1 arg ---
                (1, true, 0b0) => {
                    // (i64) -> f64  e.g. date_get_time(t:Int)
                    let f: unsafe extern "C" fn(i64) -> f64 = std::mem::transmute(func_ptr);
                    f(gi(0)).to_bits() as i64
                }
                (1, true, 0b1) => {
                    // (f64) -> f64  e.g. math_sqrt, math_abs, math_floor, ...
                    let f: unsafe extern "C" fn(f64) -> f64 = std::mem::transmute(func_ptr);
                    f(gf(0)).to_bits() as i64
                }
                (1, false, 0b1) => {
                    // (f64) -> i64  e.g. math_ffloor, math_isnan, math_isfinite
                    let f: unsafe extern "C" fn(f64) -> i64 = std::mem::transmute(func_ptr);
                    f(gf(0))
                }
                // --- 2 args ---
                (2, false, 0b01) => {
                    // (f64, i64) -> i64  e.g. hlp_ftos(d, len)
                    let f: unsafe extern "C" fn(f64, i64) -> i64 = std::mem::transmute(func_ptr);
                    f(gf(0), gi(1))
                }
                (2, true, 0b01) => {
                    // (f64, i64) -> f64
                    let f: unsafe extern "C" fn(f64, i64) -> f64 = std::mem::transmute(func_ptr);
                    f(gf(0), gi(1)).to_bits() as i64
                }
                (2, false, 0b10) => {
                    // (i64, f64) -> i64
                    let f: unsafe extern "C" fn(i64, f64) -> i64 = std::mem::transmute(func_ptr);
                    f(gi(0), gf(1))
                }
                (2, true, 0b10) => {
                    // (i64, f64) -> f64
                    let f: unsafe extern "C" fn(i64, f64) -> f64 = std::mem::transmute(func_ptr);
                    f(gi(0), gf(1)).to_bits() as i64
                }
                (2, true, 0b11) => {
                    // (f64, f64) -> f64  e.g. math_pow, math_atan2
                    let f: unsafe extern "C" fn(f64, f64) -> f64 = std::mem::transmute(func_ptr);
                    f(gf(0), gf(1)).to_bits() as i64
                }
                (2, true, 0b00) => {
                    // (i64, i64) -> f64
                    let f: unsafe extern "C" fn(i64, i64) -> f64 = std::mem::transmute(func_ptr);
                    f(gi(0), gi(1)).to_bits() as i64
                }
                (2, false, 0b11) => {
                    // (f64, f64) -> i64
                    let f: unsafe extern "C" fn(f64, f64) -> i64 = std::mem::transmute(func_ptr);
                    f(gf(0), gf(1))
                }
                // --- 3 args ---
                (3, true, 0b000) => {
                    // (i64, i64, i64) -> f64  e.g. hlp_parse_float(bytes, pos, len)
                    let f: unsafe extern "C" fn(i64, i64, i64) -> f64 =
                        std::mem::transmute(func_ptr);
                    f(gi(0), gi(1), gi(2)).to_bits() as i64
                }
                (3, false, 0b001) => {
                    // (f64, i64, i64) -> i64
                    let f: unsafe extern "C" fn(f64, i64, i64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(gf(0), gi(1), gi(2))
                }
                (3, true, 0b001) => {
                    // (f64, i64, i64) -> f64
                    let f: unsafe extern "C" fn(f64, i64, i64) -> f64 =
                        std::mem::transmute(func_ptr);
                    f(gf(0), gi(1), gi(2)).to_bits() as i64
                }
                (3, false, 0b111) => {
                    // (f64, f64, f64) -> i64
                    let f: unsafe extern "C" fn(f64, f64, f64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(gf(0), gf(1), gf(2))
                }
                (3, true, 0b111) => {
                    // (f64, f64, f64) -> f64
                    let f: unsafe extern "C" fn(f64, f64, f64) -> f64 =
                        std::mem::transmute(func_ptr);
                    f(gf(0), gf(1), gf(2)).to_bits() as i64
                }
                // --- 4 args ---
                (4, false, 0b1111) => {
                    // (f64, f64, f64, f64) -> i64  e.g. gl_clear_color(r, g, b, a)
                    let f: unsafe extern "C" fn(f64, f64, f64, f64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(gf(0), gf(1), gf(2), gf(3))
                }
                _ => {
                    return Err(anyhow!(
                        "Float native dispatch: {} args, float_mask={:#b}, ret_float={} not yet supported",
                        args.len(),
                        float_mask,
                        ret_is_float
                    ));
                }
            }
        };
        Ok(raw)
    }

    /// Convert a NanBoxedValue to an i64 for FFI passing.
    /// Uses the HL type kind to correctly interpret the value.
    fn value_to_i64(&self, val: NanBoxedValue, type_kind: u32) -> i64 {
        use ValueTypeKind::*;
        match ValueTypeKind::try_from(type_kind).unwrap_or(HNULL) {
            HVOID => 0,
            HI32 | HUI8 | HUI16 => val.as_i32() as i64,
            HI64 => val.as_i64_lossy(),
            HF32 | HF64 => {
                // Floats passed through integer registers via transmute
                val.as_f64().to_bits() as i64
            }
            HBOOL => val.as_bool() as i64,
            _ => {
                // All other types are pointer-like (HOBJ, HDYN, HBYTES, HFUN, etc.)
                if val.is_null() || val.is_void() {
                    0
                } else if val.is_ptr() {
                    val.as_ptr() as i64
                } else if val.is_i32() {
                    // Sometimes an i32 is used where a pointer is expected (e.g., 0 for null)
                    val.as_i32() as i64
                } else {
                    // TAG_I64, TAG_BYTES, TAG_FUNC, or unknown - extract raw payload
                    val.as_ptr() as i64
                }
            }
        }
    }

    /// Wrap a raw i64 return value from a native function based on the HL return type.
    fn wrap_native_result(&self, raw: i64, ret_kind: u32) -> NanBoxedValue {
        use ValueTypeKind::*;
        match ValueTypeKind::try_from(ret_kind).unwrap_or(HNULL) {
            HVOID => NanBoxedValue::void(),
            HI32 | HUI8 | HUI16 => NanBoxedValue::from_i32(raw as i32),
            HI64 => NanBoxedValue::from_i64(raw),
            HF32 | HF64 => NanBoxedValue::from_f64(f64::from_bits(raw as u64)),
            HBOOL => NanBoxedValue::from_bool(raw != 0),
            HBYTES => NanBoxedValue::from_bytes_ptr(raw as usize),
            _ => {
                // All other types are pointer-like (HOBJ, HDYN, HFUN, HARRAY, etc.)
                if raw == 0 {
                    NanBoxedValue::null()
                } else {
                    NanBoxedValue::from_ptr(raw as usize)
                }
            }
        }
    }

    /// Read a field from an object at the given field index.
    /// Uses the runtime object's fields_indexes to compute the byte offset.
    unsafe fn read_obj_field(
        obj_ptr: *mut u8,
        field_idx: usize,
        dst_kind: u32,
        obj_c_type: *mut c_void,
        obj_kind: u32,
        fn_get_obj_rt: *mut c_void,
    ) -> NanBoxedValue {
        if fn_get_obj_rt.is_null() {
            return NanBoxedValue::null();
        }

        // For HOBJ, prefer the object's own header type (supports polymorphism).
        // For HSTRUCT, use the register's declared type (structs have no header).
        let type_ptr = if obj_kind != hl_type_kind_HSTRUCT {
            let header = *(obj_ptr as *const *mut c_void);
            if !header.is_null() {
                header
            } else {
                obj_c_type
            }
        } else {
            obj_c_type
        };

        if type_ptr.is_null() {
            return NanBoxedValue::null();
        }

        // Corruption tripwire: a type pointer must be 8-aligned; a NaN-boxed
        // double here means the object's memory was reclaimed and reused.
        // Print the evidence (cross-reference with ASH_GC_TRACE_FREED) before
        // the misaligned deref aborts without it.
        {
            let bad_align = (type_ptr as usize) & 7 != 0;
            // An aligned-but-garbage header (a reused line of doubles) passes
            // the alignment check; the type's kind field gives it away.
            let bad_kind = !bad_align && {
                let k = *(type_ptr as *const i32);
                !(0..=22).contains(&k)
            };
            if bad_align || bad_kind {
                eprintln!(
                    "[gc-corrupt] FieldGet obj={:#x} header={:#x} field={field_idx}",
                    obj_ptr as usize, type_ptr as usize
                );
            }
        }
        let get_rt: FnGetObjRt = std::mem::transmute(fn_get_obj_rt);
        let rt = get_rt(type_ptr) as *const hl_runtime_obj;
        if rt.is_null() || (*rt).fields_indexes.is_null() {
            return NanBoxedValue::null();
        }

        if field_idx >= (*rt).nfields as usize {
            return NanBoxedValue::null();
        }

        let offset = *(*rt).fields_indexes.add(field_idx);
        let field_addr = obj_ptr.add(offset as usize);

        // Use dst_kind (register type) for reading — the compiler knows the correct
        // read width. The field's declared type is only used for WRITING to prevent
        // 8-byte NanBox spill into adjacent fields.
        Self::read_value_at(field_addr, dst_kind)
    }

    /// Write a value to an object field at the given field index.
    unsafe fn write_obj_field(
        obj_ptr: *mut u8,
        field_idx: usize,
        src_kind: u32,
        val: NanBoxedValue,
        obj_c_type: *mut c_void,
        obj_kind: u32,
        fn_get_obj_rt: *mut c_void,
    ) {
        if fn_get_obj_rt.is_null() {
            return;
        }

        let type_ptr = if obj_kind != hl_type_kind_HSTRUCT {
            let header = *(obj_ptr as *const *mut c_void);
            if !header.is_null() {
                header
            } else {
                obj_c_type
            }
        } else {
            obj_c_type
        };

        if type_ptr.is_null() {
            return;
        }

        let get_rt: FnGetObjRt = std::mem::transmute(fn_get_obj_rt);
        let rt = get_rt(type_ptr) as *const hl_runtime_obj;
        if rt.is_null() || (*rt).fields_indexes.is_null() {
            return;
        }

        if field_idx >= (*rt).nfields as usize {
            return;
        }

        let offset = *(*rt).fields_indexes.add(field_idx);
        let field_addr = obj_ptr.add(offset as usize) as *mut u8;

        Self::write_value_at(field_addr, src_kind, val);
    }

    /// Read a value from a raw memory address based on the HL type kind.
    unsafe fn read_value_at(addr: *const u8, kind: u32) -> NanBoxedValue {
        use ValueTypeKind::*;
        match ValueTypeKind::try_from(kind).unwrap_or(HDYN) {
            HVOID => NanBoxedValue::void(),
            HUI8 => NanBoxedValue::from_i32(*(addr as *const u8) as i32),
            HUI16 => NanBoxedValue::from_i32(*(addr as *const u16) as i32),
            HI32 => NanBoxedValue::from_i32(*(addr as *const i32)),
            HI64 => NanBoxedValue::from_i64(*(addr as *const i64)),
            HF32 => NanBoxedValue::from_f64(*(addr as *const f32) as f64),
            HF64 => NanBoxedValue::from_f64(*(addr as *const f64)),
            HBOOL => NanBoxedValue::from_bool(*(addr as *const u8) != 0),
            _ => {
                // Pointer types (OBJ, DYN, FUN, ARRAY, BYTES, ENUM, etc.)
                let ptr = *(addr as *const usize);
                if ptr == 0 {
                    NanBoxedValue::null()
                } else {
                    NanBoxedValue::from_ptr(ptr)
                }
            }
        }
    }

    /// Resolve a method findex from bytecode type proto (fallback when vobj_proto unavailable).
    fn resolve_method_findex_from_bytecode(
        &self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        this_reg: &Reg,
        proto_index: usize,
    ) -> Option<usize> {
        let type_idx = func.regs[this_reg.0 as usize].0;
        self.find_proto_findex(bytecode, type_idx, proto_index)
    }

    /// Recursively search type and its supers for a proto with matching pindex.
    fn find_proto_findex(
        &self,
        bytecode: &DecodedBytecode,
        type_idx: usize,
        proto_index: usize,
    ) -> Option<usize> {
        let hl_type_rust = &bytecode.types[type_idx];
        if let Some(ref obj) = hl_type_rust.obj {
            for proto in &obj.proto {
                if proto.pindex as usize == proto_index {
                    return Some(proto.findex as usize);
                }
            }
            // Check super type
            if let Some(ref super_) = obj.super_ {
                return self.find_proto_findex(bytecode, super_.0, proto_index);
            }
        }
        None
    }

    /// Write a value to a raw memory address based on the HL type kind.
    unsafe fn write_value_at(addr: *mut u8, kind: u32, val: NanBoxedValue) {
        use ValueTypeKind::*;
        match ValueTypeKind::try_from(kind).unwrap_or(HDYN) {
            HVOID => {}
            HUI8 => *(addr as *mut u8) = val.as_i32() as u8,
            HUI16 => *(addr as *mut u16) = val.as_i32() as u16,
            HI32 => *(addr as *mut i32) = val.as_i32(),
            HI64 => *(addr as *mut i64) = val.as_i64_lossy(),
            HF32 => *(addr as *mut f32) = val.as_f64() as f32,
            HF64 => *(addr as *mut f64) = val.as_f64(),
            HBOOL => *(addr as *mut u8) = val.as_bool() as u8,
            _ => {
                // Pointer types — but the NanBoxed value might actually
                // be a primitive (e.g., HDYN register holding an I32).
                if val.is_null() || val.is_void() {
                    *(addr as *mut usize) = 0;
                } else if val.is_i32() {
                    *(addr as *mut i32) = val.as_i32();
                } else if val.is_f64() {
                    *(addr as *mut f64) = val.as_f64();
                } else {
                    *(addr as *mut usize) = val.as_ptr();
                }
            }
        }
    }

    /// Allocate a venum value for the given type and construct index using the GC allocator.
    /// Takes fn_alloc_enum as a parameter to avoid conflicting with the frame mutable borrow.
    fn alloc_enum_value(
        fn_alloc_enum: *mut c_void,
        c_type_ptr: *mut hl_type,
        construct_idx: i32,
    ) -> *mut u8 {
        if fn_alloc_enum.is_null() || c_type_ptr.is_null() {
            return std::ptr::null_mut();
        }
        unsafe {
            let f: unsafe extern "C" fn(*mut hl_type, i32) -> *mut u8 =
                std::mem::transmute(fn_alloc_enum);
            f(c_type_ptr, construct_idx)
        }
    }

    /// Read a NanBoxedValue from a raw memory pointer using the given type kind.
    fn read_value_from_ptr(ptr: *const u8, kind: u32) -> NanBoxedValue {
        unsafe { Self::read_value_at(ptr, kind) }
    }

    /// Write a NanBoxedValue to a raw memory pointer using the given type kind.
    fn write_value_to_ptr(ptr: *mut u8, val: NanBoxedValue, kind: u32) {
        unsafe { Self::write_value_at(ptr, kind, val) }
    }
}

#[cfg(test)]
mod stub_bridge_tests {
    use super::*;
    use ash::native_lib::init_std_library;

    /// The stub bridge must never report a failure by returning a value:
    /// compiled code consumes that word as the callee's declared return type,
    /// so a `0` becomes a null pointer it dereferences immediately.
    ///
    /// This drives the exact failure the crash report came from — an
    /// interpreter-internal "Null access" with no throwable `vdynamic` — and
    /// asserts it arrives at an armed native trap as a real HL exception
    /// value, which is what lets the Haxe-side `catch` see it.
    #[test]
    fn interpreter_internal_failure_raises_into_the_trap_chain() {
        init_std_library().expect("std library");
        let resolver = NativeFunctionResolver::new();

        let setup = resolver
            .resolve_function("std", "hlp_setup_trap_jit")
            .expect("hlp_setup_trap_jit");
        let get_exc = resolver
            .resolve_function("std", "hlp_get_exc_value")
            .expect("hlp_get_exc_value");
        let clear_exc = resolver
            .resolve_function("std", "hlp_clear_exc_value")
            .expect("hlp_clear_exc_value");

        unsafe {
            type FnSetupTrap = unsafe extern "C" fn() -> *mut c_void;
            let setup: FnSetupTrap = std::mem::transmute(setup);
            let jmp_buf = setup();
            assert!(!jmp_buf.is_null(), "trap setup failed");

            if call_setjmp_opaque(jmp_buf) == 0 {
                // No HL value on this error, exactly like a `NullCheck`
                // failure raised while the bridge re-enters the interpreter.
                let err = anyhow::Error::new(HLExceptionPropagation {
                    value: NanBoxedValue::null(),
                    message: Some("Null access".to_string()),
                });
                HLInterpreter::raise_stub_bridge_failure(&resolver, 698, err);
            }

            // Reached only via longjmp out of the raise.
            type FnGetExc = unsafe extern "C" fn() -> *mut hl::vdynamic;
            let exc = std::mem::transmute::<*mut c_void, FnGetExc>(get_exc)();
            assert!(
                !exc.is_null(),
                "raise produced no exception value — the trap chain saw nothing to catch"
            );

            // `hl_error`-shaped value: a bytes dynamic carrying the message.
            let msg_ptr = (*exc).v.bytes as *const u16;
            assert!(!msg_ptr.is_null(), "exception carries no message");
            let mut units = Vec::new();
            let mut i = 0isize;
            while *msg_ptr.offset(i) != 0 && i < 512 {
                units.push(*msg_ptr.offset(i));
                i += 1;
            }
            let msg = String::from_utf16_lossy(&units);
            assert!(
                msg.contains("Null access"),
                "unexpected exception message: {msg}"
            );

            type FnClearExc = unsafe extern "C" fn();
            std::mem::transmute::<*mut c_void, FnClearExc>(clear_exc)();
        }
    }
}
