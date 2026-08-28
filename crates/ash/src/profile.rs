//! # Execution profiler
//!
//! Answers two different questions, with two different mechanisms, because no
//! single one answers both well:
//!
//! * **Where does a phase go?** — [`scope`] opens a named, nestable region and
//!   charges wall time to it. Phases are coarse and deterministic (bytecode
//!   decode, native resolution, one function's LLVM codegen), so exact timing
//!   costs nothing measurable and every nanosecond is accounted for. The report
//!   prints them as a tree with *total* and *self* time, so a parent that is
//!   slow purely because of one child reads differently from one that is slow
//!   on its own.
//!
//! * **Where does the running program go?** — a sampling profiler interrupts
//!   the main thread `ASH_PROFILE_HZ` times a second and records the program
//!   counter. Instrumenting the interpreter loop or compiled code would
//!   perturb the very thing being measured, so sampling is the only honest
//!   option there.
//!
//! Sampling is also the only way to see *inside* compiled code. An external
//! profiler can attach to the process, but every address the JIT tiers produce
//! is anonymous `mmap` memory with no symbol table, so the hot function shows
//! up as a bare address. Compiled code registers its entry points here
//! ([`register_jit_code`]), which is what lets the report say
//! `llvm  findex=1394  NBody.advance` instead of `0x14e00c1a0`.
//!
//! ## What the sample buckets mean
//!
//! Every sample is classified by its PC, which is what makes the report able to
//! separate *generated code* from *the runtime it calls into*:
//!
//! | Bucket | Meaning |
//! |---|---|
//! | `llvm` / `cranelift` | Inside code a JIT tier emitted |
//! | `interp` | Inside the bytecode interpreter loop |
//! | `runtime` | An `hlp_*` helper — the work generated code delegates |
//! | `gc` | Allocation and collection |
//! | `native` | Any other resolved symbol (libc, HDLLs) |
//! | `unknown` | Unresolvable — anonymous memory with no registered range |
//!
//! A workload whose time sits in `runtime` rather than `llvm` is not bound by
//! codegen quality; it is bound by how much it hands back to the runtime, and
//! no amount of backend tuning will move it.
//!
//! ## Enabling
//!
//! ```text
//! ASH_PROFILE=phases    phase tree only
//! ASH_PROFILE=sample    sampling only
//! ASH_PROFILE=all       both (also: ASH_PROFILE=1)
//! ASH_PROFILE_HZ=997    sample rate, default 997
//! ASH_PROFILE_OUT=path  write the report here instead of stderr
//! ```
//!
//! When disabled, [`scope`] does an atomic load and returns an inert guard, and
//! no signal handler is installed.
//!
//! ## Threads
//!
//! Phase scopes are per-thread: each thread's stack roots at a node named after
//! the thread, so a JIT worker's compile time appears under `ash-jit-worker`
//! rather than nested inside whatever the main thread happened to be doing. The
//! sampler only interrupts the thread that called [`init`], since background
//! compilation is already covered deterministically by phase scopes.

use std::collections::HashMap;
// `c_void` only appears in signal-context and dladdr code, all of it unix.
#[cfg(unix)]
use std::ffi::c_void;
use std::io::Write;
use std::sync::atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering};
use std::sync::{Mutex, OnceLock};
use std::time::Instant;

// ─────────────────────────────────────────────────────────────────────────────
// Configuration
// ─────────────────────────────────────────────────────────────────────────────

static PHASES_ON: AtomicBool = AtomicBool::new(false);
static SAMPLING_ON: AtomicBool = AtomicBool::new(false);
static INITIALIZED: AtomicBool = AtomicBool::new(false);

/// Whether phase scopes are being recorded.
#[inline(always)]
pub fn phases_enabled() -> bool {
    PHASES_ON.load(Ordering::Relaxed)
}

/// Whether the sampler is running.
#[inline(always)]
pub fn sampling_enabled() -> bool {
    SAMPLING_ON.load(Ordering::Relaxed)
}

/// Whether any profiling is active, i.e. whether [`report`] will print.
#[inline(always)]
pub fn enabled() -> bool {
    phases_enabled() || sampling_enabled()
}

/// Read `ASH_PROFILE*` and start whatever it asks for.
///
/// Call once, from the thread that will run the program: the sampler
/// interrupts *this* thread. Repeat calls are ignored, so the JIT binary and
/// the CLI can both call it unconditionally.
pub fn init() {
    if INITIALIZED.swap(true, Ordering::SeqCst) {
        return;
    }
    let spec = match std::env::var("ASH_PROFILE") {
        Ok(v) if !v.is_empty() && v != "0" => v,
        _ => return,
    };
    let (phases, sampling) = match spec.as_str() {
        "phases" | "phase" | "tree" => (true, false),
        "sample" | "sampling" | "samples" => (false, true),
        _ => (true, true),
    };
    PHASES_ON.store(phases, Ordering::SeqCst);

    // Anchor the whole run: phases use it as their denominator, and sampling
    // compares it against CPU time to tell whether the thread was starved.
    WALL_START.get_or_init(Instant::now);
    if sampling {
        match sampler::start() {
            Ok(()) => SAMPLING_ON.store(true, Ordering::SeqCst),
            Err(e) => eprintln!("[profile] sampling unavailable: {e}"),
        }
    }
}

static WALL_START: OnceLock<Instant> = OnceLock::new();

/// CPU time consumed by the *calling* thread, in milliseconds.
///
/// Both call sites — sampler start and report — run on the sampled thread, so
/// the thread-scoped clock is readable directly rather than having to be asked
/// of another thread. Returns 0 where the clock is unavailable, which makes the
/// report fall back to assuming every tick landed.
#[cfg(unix)]
fn thread_cpu_ms() -> f64 {
    unsafe {
        let mut ts: libc::timespec = std::mem::zeroed();
        if libc::clock_gettime(libc::CLOCK_THREAD_CPUTIME_ID, &mut ts) != 0 {
            return 0.0;
        }
        ts.tv_sec as f64 * 1e3 + ts.tv_nsec as f64 / 1e6
    }
}

/// No thread-scoped CPU clock is wired up on this platform (the unix version
/// reads CLOCK_THREAD_CPUTIME_ID). Only the sampler's report consumes this,
/// and the sampler does not run here, so 0 — the documented "unavailable"
/// value — is never actually read.
#[cfg(not(unix))]
fn thread_cpu_ms() -> f64 {
    0.0
}

// ─────────────────────────────────────────────────────────────────────────────
// Phase tree
// ─────────────────────────────────────────────────────────────────────────────

const NO_PARENT: u32 = u32::MAX;

struct Node {
    label: &'static str,
    parent: u32,
    children: Vec<u32>,
    /// Wall time between enter and exit, summed over every invocation.
    total_ns: u64,
    /// `total_ns` minus the time charged to child scopes.
    self_ns: u64,
    count: u64,
}

#[derive(Default)]
struct Registry {
    nodes: Vec<Node>,
    /// `(parent, label)` → node id. Roots use [`NO_PARENT`].
    index: HashMap<(u32, &'static str), u32>,
    counters: Vec<(&'static str, u64)>,
}

impl Registry {
    fn intern(&mut self, parent: u32, label: &'static str) -> u32 {
        if let Some(&id) = self.index.get(&(parent, label)) {
            return id;
        }
        let id = self.nodes.len() as u32;
        self.nodes.push(Node {
            label,
            parent,
            children: Vec::new(),
            total_ns: 0,
            self_ns: 0,
            count: 0,
        });
        self.index.insert((parent, label), id);
        if parent != NO_PARENT {
            self.nodes[parent as usize].children.push(id);
        }
        id
    }
}

fn registry() -> &'static Mutex<Registry> {
    static R: OnceLock<Mutex<Registry>> = OnceLock::new();
    R.get_or_init(|| Mutex::new(Registry::default()))
}

struct Frame {
    node: u32,
    start: Instant,
    /// Time already charged to scopes opened inside this one.
    child_ns: u64,
}

thread_local! {
    static STACK: std::cell::RefCell<Vec<Frame>> = const { std::cell::RefCell::new(Vec::new()) };
    static THREAD_ROOT: std::cell::Cell<u32> = const { std::cell::Cell::new(NO_PARENT) };
}

/// The root node for the calling thread, named after the thread itself.
fn thread_root() -> u32 {
    THREAD_ROOT.with(|c| {
        let cached = c.get();
        if cached != NO_PARENT {
            return cached;
        }
        let name: &'static str = match std::thread::current().name() {
            Some(n) => Box::leak(n.to_string().into_boxed_str()),
            None => "thread",
        };
        let id = registry().lock().unwrap().intern(NO_PARENT, name);
        c.set(id);
        id
    })
}

/// An open phase. Charges its lifetime to `label` when dropped.
#[must_use = "the phase ends when this guard is dropped; bind it to a name"]
pub struct Scope {
    active: bool,
}

impl Drop for Scope {
    fn drop(&mut self) {
        if !self.active {
            return;
        }
        let Some(frame) = STACK.with(|s| s.borrow_mut().pop()) else {
            return;
        };
        let elapsed = frame.start.elapsed().as_nanos() as u64;
        {
            let mut reg = registry().lock().unwrap();
            let node = &mut reg.nodes[frame.node as usize];
            node.total_ns += elapsed;
            node.self_ns += elapsed.saturating_sub(frame.child_ns);
            node.count += 1;
        }
        STACK.with(|s| {
            if let Some(parent) = s.borrow_mut().last_mut() {
                parent.child_ns += elapsed;
            }
        });
    }
}

/// Open a phase named `label`, nested inside whatever phase is already open on
/// this thread. The phase ends when the returned guard drops.
///
/// ```ignore
/// let _p = profile::scope("decode");
/// ```
#[inline]
pub fn scope(label: &'static str) -> Scope {
    if !phases_enabled() {
        return Scope { active: false };
    }
    let parent = STACK
        .with(|s| s.borrow().last().map(|f| f.node))
        .unwrap_or_else(thread_root);
    let node = registry().lock().unwrap().intern(parent, label);
    STACK.with(|s| {
        s.borrow_mut().push(Frame {
            node,
            start: Instant::now(),
            child_ns: 0,
        })
    });
    Scope { active: true }
}

/// Charge an already-measured duration to `label`, as if a [`scope`] had been
/// open for exactly that long.
///
/// For code that already times itself with an `Instant` and would only be made
/// harder to read by being restructured around a guard. The duration is
/// charged to the enclosing phase's children, so `self` time stays correct.
pub fn record(label: &'static str, elapsed: std::time::Duration) {
    if !phases_enabled() {
        return;
    }
    let ns = elapsed.as_nanos() as u64;
    let parent = STACK
        .with(|s| s.borrow().last().map(|f| f.node))
        .unwrap_or_else(thread_root);
    {
        let mut reg = registry().lock().unwrap();
        let id = reg.intern(parent, label);
        let node = &mut reg.nodes[id as usize];
        node.total_ns += ns;
        node.self_ns += ns;
        node.count += 1;
    }
    STACK.with(|s| {
        if let Some(top) = s.borrow_mut().last_mut() {
            top.child_ns += ns;
        }
    });
}

/// Add `n` to a named counter, reported alongside the phase tree.
///
/// For things that are worth knowing but are not durations: functions
/// compiled, opcodes lowered, promotions declined.
pub fn count(label: &'static str, n: u64) {
    if !phases_enabled() {
        return;
    }
    let mut reg = registry().lock().unwrap();
    match reg.counters.iter_mut().find(|(l, _)| *l == label) {
        Some((_, v)) => *v += n,
        None => reg.counters.push((label, n)),
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Symbol registration
// ─────────────────────────────────────────────────────────────────────────────

/// Which engine produced a piece of compiled code.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Tier {
    Cranelift,
    Llvm,
}

impl Tier {
    fn label(self) -> &'static str {
        match self {
            Tier::Cranelift => "cranelift",
            Tier::Llvm => "llvm",
        }
    }
}

struct CodeRange {
    addr: usize,
    findex: u32,
    tier: Tier,
}

fn jit_code() -> &'static Mutex<Vec<CodeRange>> {
    static C: OnceLock<Mutex<Vec<CodeRange>>> = OnceLock::new();
    C.get_or_init(|| Mutex::new(Vec::new()))
}

/// Record that `addr` is the entry point of compiled code for `findex`.
///
/// Call this from every tier's install path. Without it a sample landing in
/// generated code can only be reported as an address, because JIT memory
/// carries no symbols for `dladdr` to find.
///
/// Ranges are matched nearest-entry-below, since neither backend reports a code
/// length; see [`classify`] for the bound that keeps that from over-claiming.
pub fn register_jit_code(findex: u32, tier: Tier, addr: usize) {
    // Unconditional, not sampling-gated: the crash handler symbolizes fault
    // frames against this registry, and a crash does not schedule itself for
    // runs where the profiler happened to be on. The cost is one Vec push per
    // PROMOTION, which is measured in dozens per process.
    if addr == 0 {
        return;
    }
    if std::env::var("ASH_PROFILE_DEBUG").is_ok() {
        eprintln!("[prof-reg] findex={findex} tier={} addr={addr:#x}", tier.label());
    }
    jit_code()
        .lock()
        .unwrap()
        .push(CodeRange { addr, findex, tier });
}

/// The findex whose compiled entry is exactly `addr`, if any.
///
/// Exact match, not nearest-below: the caller has a function POINTER (from a
/// closure built by compiled code), not an arbitrary PC. Answering from this
/// registry rather than from `functions_ptrs` matters because a slot in that
/// table is overwritten on re-promotion, whereas every install pushes here —
/// so a closure still holding a tier-0 address stays resolvable after tier 1
/// has replaced the slot.
pub fn findex_at_entry(addr: usize) -> Option<u32> {
    if addr == 0 {
        return None;
    }
    let ranges = jit_code().lock().ok()?;
    ranges
        .iter()
        .find(|r| r.addr == addr)
        .map(|r| r.findex)
}

/// Which compiled function a PC falls in, for the crash handler.
///
/// Nearest-entry-below, same rule the sampler uses, since neither backend
/// reports a code size; a PC more than 2MB past the nearest entry is treated
/// as not JIT code rather than attributed to a function it cannot belong to.
/// try_lock, never lock: this is called from a signal handler, and a handler
/// that deadlocks on the mutex its own thread holds turns a crash report
/// into a hang.
pub fn describe_jit_pc(pc: usize) -> Option<(u32, &'static str, usize)> {
    let guard = jit_code().try_lock().ok()?;
    let mut best: Option<&CodeRange> = None;
    for r in guard.iter() {
        if r.addr <= pc && best.map_or(true, |b| r.addr > b.addr) {
            best = Some(r);
        }
    }
    let r = best?;
    if pc - r.addr > 2 << 20 {
        return None;
    }
    Some((r.findex, r.tier.label(), pc - r.addr))
}

/// Resolves a findex to a human-readable function name for the report.
type NameResolver = Box<dyn Fn(u32) -> Option<String> + Send + Sync>;
static NAMES: OnceLock<NameResolver> = OnceLock::new();

/// Install the findex → name mapping used when printing sample locations.
///
/// The profiler lives below the bytecode types, so the embedder supplies this
/// after decoding. Without it the report falls back to bare findexes.
pub fn set_name_resolver(f: impl Fn(u32) -> Option<String> + Send + Sync + 'static) {
    let _ = NAMES.set(Box::new(f));
}

fn resolve_name(findex: u32) -> Option<String> {
    NAMES.get().and_then(|f| f(findex))
}

// ─────────────────────────────────────────────────────────────────────────────
// Interpreter attribution
// ─────────────────────────────────────────────────────────────────────────────

/// The bytecode function the sampled thread is currently interpreting, or
/// `u32::MAX` for none.
///
/// A plain global atomic rather than a thread-local: the signal handler reads
/// it, and resolving a thread-local from a handler can call into lazy TLS
/// initialization, which is not async-signal-safe. Only the sampled thread
/// writes it, so a global is both correct and cheaper.
static CURRENT_FINDEX: AtomicU64 = AtomicU64::new(u32::MAX as u64);

/// Note that the interpreter has entered `findex`; returns the previous value
/// to restore on the way out. Cheap enough for the call path — one relaxed
/// store, behind a load that is false when sampling is off.
#[inline(always)]
pub fn enter_interp(findex: u32) -> u32 {
    if !sampling_enabled() {
        return u32::MAX;
    }
    CURRENT_FINDEX.swap(findex as u64, Ordering::Relaxed) as u32
}

/// Restore the findex saved by [`enter_interp`].
#[inline(always)]
pub fn leave_interp(prev: u32) {
    if !sampling_enabled() {
        return;
    }
    CURRENT_FINDEX.store(prev as u64, Ordering::Relaxed);
}

// ─────────────────────────────────────────────────────────────────────────────
// Sampler
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(unix)]
mod sampler {
    use super::*;

    /// Room for ~17 minutes at the default rate. Overflow stops recording
    /// rather than wrapping, so a truncated profile is reported as truncated
    /// instead of silently describing only the tail of the run.
    const CAPACITY: usize = 1 << 20;

    pub(super) static PCS: OnceLock<Box<[AtomicU64]>> = OnceLock::new();
    /// One level of caller, from the link register. Enough to tell a `madvise`
    /// the collector asked for from one the allocator did, without walking a
    /// stack that may be mid-prologue or inside JIT code with no unwind info.
    pub(super) static LRS: OnceLock<Box<[AtomicU64]>> = OnceLock::new();
    pub(super) static TAGS: OnceLock<Box<[AtomicU64]>> = OnceLock::new();
    pub(super) static WRITE: AtomicUsize = AtomicUsize::new(0);
    pub(super) static DROPPED: AtomicUsize = AtomicUsize::new(0);
    pub(super) static HZ: AtomicU64 = AtomicU64::new(997);
    /// The sampled thread's CPU clock when sampling began, so the report can
    /// measure the interval rather than the thread's whole lifetime.
    pub(super) static START_CPU_MS: AtomicU64 = AtomicU64::new(0);
    static STOP: AtomicBool = AtomicBool::new(false);

    pub(super) fn recorded() -> usize {
        WRITE.load(Ordering::Relaxed).min(CAPACITY)
    }

    /// Install the `SIGPROF` handler and spawn the thread that raises it.
    pub(super) fn start() -> Result<(), String> {
        let hz: u64 = std::env::var("ASH_PROFILE_HZ")
            .ok()
            .and_then(|v| v.parse().ok())
            .filter(|&h| h > 0 && h <= 10_000)
            .unwrap_or(997);
        HZ.store(hz, Ordering::SeqCst);
        START_CPU_MS.store(super::thread_cpu_ms() as u64, Ordering::SeqCst);

        let _ = PCS.set((0..CAPACITY).map(|_| AtomicU64::new(0)).collect());
        let _ = LRS.set((0..CAPACITY).map(|_| AtomicU64::new(0)).collect());
        let _ = TAGS.set((0..CAPACITY).map(|_| AtomicU64::new(0)).collect());

        unsafe {
            let mut sa: libc::sigaction = std::mem::zeroed();
            sa.sa_sigaction = handler as *const () as usize;
            sa.sa_flags = libc::SA_SIGINFO | libc::SA_RESTART;
            libc::sigemptyset(&mut sa.sa_mask);
            if libc::sigaction(libc::SIGPROF, &sa, std::ptr::null_mut()) != 0 {
                return Err("sigaction(SIGPROF) failed".to_string());
            }
        }

        // Capture the thread to interrupt. `pthread_kill` targets one thread,
        // which is what keeps the profile about the program rather than about
        // whichever thread the kernel picked.
        let target = unsafe { libc::pthread_self() };
        let target = TargetThread(target);

        std::thread::Builder::new()
            .name("ash-profiler".to_string())
            .spawn(move || {
                let target = target;
                let interval =
                    std::time::Duration::from_nanos(1_000_000_000 / HZ.load(Ordering::Relaxed));
                while !STOP.load(Ordering::Relaxed) {
                    std::thread::sleep(interval);
                    unsafe {
                        libc::pthread_kill(target.0, libc::SIGPROF);
                    }
                }
            })
            .map_err(|e| format!("could not spawn sampler thread: {e}"))?;
        Ok(())
    }

    /// Stop sampling. Called before the report so no sample lands mid-print.
    pub(super) fn stop() {
        STOP.store(true, Ordering::SeqCst);
        SAMPLING_ON.store(false, Ordering::SeqCst);
        unsafe {
            let mut sa: libc::sigaction = std::mem::zeroed();
            sa.sa_sigaction = libc::SIG_IGN;
            libc::sigaction(libc::SIGPROF, &sa, std::ptr::null_mut());
        }
    }

    /// `pthread_t` is a raw pointer on some platforms, so it is not `Send` by
    /// default. Sending a thread handle to the thread that will signal it is
    /// exactly what `pthread_kill` is for.
    struct TargetThread(libc::pthread_t);
    unsafe impl Send for TargetThread {}

    /// **Async-signal-safe.** Two atomic stores into preallocated memory, and
    /// plain loads out of the signal context. No allocation, no locks, no
    /// formatting.
    extern "C" fn handler(_sig: i32, _info: *mut libc::siginfo_t, ctx: *mut c_void) {
        let pc = unsafe { pc_from_context(ctx) };
        if pc == 0 {
            return;
        }
        let lr = unsafe { lr_from_context(ctx) };
        let idx = WRITE.fetch_add(1, Ordering::Relaxed);
        if idx >= CAPACITY {
            DROPPED.fetch_add(1, Ordering::Relaxed);
            return;
        }
        // `get()` on an already-initialized `OnceLock` is a load and a branch.
        if let (Some(pcs), Some(lrs), Some(tags)) = (PCS.get(), LRS.get(), TAGS.get()) {
            pcs[idx].store(pc, Ordering::Relaxed);
            lrs[idx].store(lr, Ordering::Relaxed);
            tags[idx].store(CURRENT_FINDEX.load(Ordering::Relaxed), Ordering::Relaxed);
        }
    }

    // The register file a `SA_SIGINFO` handler receives. Apple does not expose
    // these through the `libc` crate, so the (stable) layouts from
    // `<mach/arm/_structs.h>` and `<mach/i386/_structs.h>` are mirrored here.
    // Only the instruction pointer is read.

    #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
    #[repr(C)]
    struct ArmThreadState64 {
        x: [u64; 29],
        fp: u64,
        lr: u64,
        sp: u64,
        pc: u64,
        cpsr: u32,
        _pad: u32,
    }

    #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
    #[repr(C)]
    struct McontextArm64 {
        es: [u64; 2],
        ss: ArmThreadState64,
    }

    #[cfg(all(target_os = "macos", target_arch = "x86_64"))]
    #[repr(C)]
    struct X86ThreadState64 {
        rax: u64,
        rbx: u64,
        rcx: u64,
        rdx: u64,
        rdi: u64,
        rsi: u64,
        rbp: u64,
        rsp: u64,
        r8: u64,
        r9: u64,
        r10: u64,
        r11: u64,
        r12: u64,
        r13: u64,
        r14: u64,
        r15: u64,
        rip: u64,
        rflags: u64,
        cs: u64,
        fs: u64,
        gs: u64,
    }

    #[cfg(all(target_os = "macos", target_arch = "x86_64"))]
    #[repr(C)]
    struct McontextX86 {
        es: [u64; 3],
        ss: X86ThreadState64,
    }

    #[cfg(target_os = "macos")]
    #[repr(C)]
    struct UContext {
        uc_onstack: i32,
        uc_sigmask: u32,
        uc_stack_sp: *mut c_void,
        uc_stack_size: usize,
        uc_stack_flags: i32,
        _pad: i32,
        uc_link: *mut UContext,
        uc_mcsize: usize,
        #[cfg(target_arch = "aarch64")]
        uc_mcontext: *mut McontextArm64,
        #[cfg(target_arch = "x86_64")]
        uc_mcontext: *mut McontextX86,
    }

    #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
    unsafe fn pc_from_context(ctx: *mut c_void) -> u64 {
        if ctx.is_null() {
            return 0;
        }
        let mc = (*(ctx as *const UContext)).uc_mcontext;
        if mc.is_null() {
            return 0;
        }
        (*mc).ss.pc
    }

    #[cfg(all(target_os = "macos", target_arch = "x86_64"))]
    unsafe fn pc_from_context(ctx: *mut c_void) -> u64 {
        if ctx.is_null() {
            return 0;
        }
        let mc = (*(ctx as *const UContext)).uc_mcontext;
        if mc.is_null() {
            return 0;
        }
        (*mc).ss.rip
    }

    #[cfg(all(target_os = "linux", target_arch = "aarch64"))]
    unsafe fn pc_from_context(ctx: *mut c_void) -> u64 {
        if ctx.is_null() {
            return 0;
        }
        (*(ctx as *const libc::ucontext_t)).uc_mcontext.pc
    }

    #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
    unsafe fn pc_from_context(ctx: *mut c_void) -> u64 {
        if ctx.is_null() {
            return 0;
        }
        (*(ctx as *const libc::ucontext_t)).uc_mcontext.gregs[libc::REG_RIP as usize] as u64
    }

    #[cfg(not(any(
        all(
            target_os = "macos",
            any(target_arch = "aarch64", target_arch = "x86_64")
        ),
        all(
            target_os = "linux",
            any(target_arch = "aarch64", target_arch = "x86_64")
        )
    )))]
    unsafe fn pc_from_context(_ctx: *mut c_void) -> u64 {
        0
    }

    /// The caller's return address, where the architecture keeps one in a
    /// register.
    ///
    /// Only meaningful in a leaf frame — once a function has pushed `lr` to its
    /// own frame the register may hold anything — which is exactly the case
    /// this is for: attributing time in libc leaves like `madvise` to whatever
    /// called them. x86_64 keeps the return address on the stack instead, and
    /// reading it blind is unreliable enough to be worse than nothing.
    #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
    unsafe fn lr_from_context(ctx: *mut c_void) -> u64 {
        if ctx.is_null() {
            return 0;
        }
        let mc = (*(ctx as *const UContext)).uc_mcontext;
        if mc.is_null() {
            return 0;
        }
        (*mc).ss.lr
    }

    #[cfg(all(target_os = "linux", target_arch = "aarch64"))]
    unsafe fn lr_from_context(ctx: *mut c_void) -> u64 {
        if ctx.is_null() {
            return 0;
        }
        (*(ctx as *const libc::ucontext_t)).uc_mcontext.regs[30]
    }

    #[cfg(not(all(any(target_os = "macos", target_os = "linux"), target_arch = "aarch64")))]
    unsafe fn lr_from_context(_ctx: *mut c_void) -> u64 {
        0
    }
}

/// The sampler is a unix mechanism end to end — SIGPROF delivery, a
/// pthread_kill ticker, and a PC read out of the signal context — and none of
/// it has a Win32 equivalent wired up (that would be a SuspendThread +
/// GetThreadContext port, deliberately out of scope for now). Requesting it
/// degrades honestly: [`start`] errors, [`init`] prints the reason once to
/// stderr, `SAMPLING_ON` stays false, and the phase-tree profiler keeps
/// working. The statics exist only so `write_sample_profile` — compiled on
/// every platform, reachable on none of these — typechecks; `recorded()` is 0,
/// so it never gets past its empty-profile early return.
#[cfg(not(unix))]
mod sampler {
    use super::*;

    pub(super) static PCS: OnceLock<Box<[AtomicU64]>> = OnceLock::new();
    pub(super) static LRS: OnceLock<Box<[AtomicU64]>> = OnceLock::new();
    pub(super) static TAGS: OnceLock<Box<[AtomicU64]>> = OnceLock::new();
    pub(super) static DROPPED: AtomicUsize = AtomicUsize::new(0);
    pub(super) static HZ: AtomicU64 = AtomicU64::new(997);
    pub(super) static START_CPU_MS: AtomicU64 = AtomicU64::new(0);

    pub(super) fn recorded() -> usize {
        0
    }

    pub(super) fn start() -> Result<(), String> {
        Err("the sampling profiler is unix-only (SIGPROF + pthread_kill); \
             the phase tree (ASH_PROFILE=phases) still works on this platform"
            .to_string())
    }

    pub(super) fn stop() {}
}

// ─────────────────────────────────────────────────────────────────────────────
// Classification
// ─────────────────────────────────────────────────────────────────────────────

/// Where a sample landed.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
enum Bucket {
    Llvm,
    Cranelift,
    Interp,
    Runtime,
    Gc,
    Native,
    Unknown,
}

impl Bucket {
    fn label(self) -> &'static str {
        match self {
            Bucket::Llvm => "llvm",
            Bucket::Cranelift => "cranelift",
            Bucket::Interp => "interp",
            Bucket::Runtime => "runtime",
            Bucket::Gc => "gc",
            Bucket::Native => "native",
            Bucket::Unknown => "unknown",
        }
    }
}

/// A PC further than this past a registered entry point is assumed to belong to
/// something else entirely. Without code lengths from the backends, this is
/// what stops the last-registered function from absorbing every unrelated
/// address above it.
const MAX_FUNCTION_SPAN: usize = 512 * 1024;

/// Decide what a sampled address belongs to.
///
/// `dladdr` first: it covers everything with a symbol table, which is the whole
/// process except JIT memory. Only when it fails — anonymous mappings, i.e.
/// generated code — is the JIT map consulted.
fn classify(pc: u64, jit: &[CodeRange]) -> (Bucket, String) {
    if let Some((sym, image)) = dladdr_symbol(pc) {
        let short = image.rsplit('/').next().unwrap_or(&image).to_string();
        let name = demangle(&sym);
        // Order matters: the allocation entry points are named `hlp_alloc_*`,
        // so the GC test has to run before the general runtime-helper one or
        // every allocation would be charged to `runtime`.
        let bucket = if name.contains("::gc::")
            || name.contains("Immix")
            || name.starts_with("hlp_alloc")
            || name.starts_with("hlp_gc")
        {
            Bucket::Gc
        } else if name.starts_with("hlp_") || name.starts_with("hl_") {
            Bucket::Runtime
        } else if name.contains("ash_interp") || name.contains("interpreter") {
            Bucket::Interp
        } else if short.contains("ash_std") {
            Bucket::Runtime
        } else {
            Bucket::Native
        };
        return (bucket, name);
    }

    // No symbol: either JIT-generated code or an unmapped address.
    let pc = pc as usize;
    if let Some(r) = jit
        .iter()
        .rev()
        .find(|r| r.addr <= pc && pc - r.addr < MAX_FUNCTION_SPAN)
    {
        let bucket = match r.tier {
            Tier::Llvm => Bucket::Llvm,
            Tier::Cranelift => Bucket::Cranelift,
        };
        let name = resolve_name(r.findex).unwrap_or_else(|| format!("findex={}", r.findex));
        return (bucket, format!("{} [{}]", name, r.tier.label()));
    }
    // Linux last resort: name the mapping. `dladdr` on a shared object
    // resolves only exported symbols, so a cdylib's internal Rust functions
    // (all of ash_std's allocator internals) and glibc's hidden ifunc
    // memset/memcpy bodies land here — 70% of a NUC mandelbrot profile was
    // hex that was really libash_std.so and libc. A library name is not a
    // symbol, but it puts samples in the right bucket and tells the reader
    // which object to open.
    #[cfg(target_os = "linux")]
    if let Some(obj) = mapping_name(pc) {
        let bucket = if obj.contains("ash_std") {
            Bucket::Gc // its unexported hot paths are the allocator's
        } else {
            Bucket::Native
        };
        return (bucket, format!("[{obj} +unexported]"));
    }
    (Bucket::Unknown, format!("{:#x}", pc))
}

/// Basename of the mapping containing `pc`, from /proc/self/maps (cached).
#[cfg(target_os = "linux")]
fn mapping_name(pc: usize) -> Option<String> {
    use std::sync::OnceLock;
    static MAPS: OnceLock<Vec<(usize, usize, String)>> = OnceLock::new();
    let maps = MAPS.get_or_init(|| {
        let mut v = Vec::new();
        if let Ok(text) = std::fs::read_to_string("/proc/self/maps") {
            for l in text.lines() {
                let mut it = l.split_whitespace();
                let (Some(range), Some(perms)) = (it.next(), it.next()) else {
                    continue;
                };
                if !perms.contains('x') {
                    continue;
                }
                let path = it.last().unwrap_or("");
                if path.is_empty() || path.starts_with('[') {
                    continue;
                }
                if let Some((lo, hi)) = range.split_once('-') {
                    if let (Ok(lo), Ok(hi)) =
                        (usize::from_str_radix(lo, 16), usize::from_str_radix(hi, 16))
                    {
                        let base = path.rsplit('/').next().unwrap_or(path).to_string();
                        v.push((lo, hi, base));
                    }
                }
            }
        }
        v
    });
    maps.iter()
        .find(|(lo, hi, _)| (*lo..*hi).contains(&pc))
        .map(|(_, _, n)| n.clone())
}

/// Resolve `pc` to `(symbol, image path)` via `dladdr`.
#[cfg(unix)]
fn dladdr_symbol(pc: u64) -> Option<(String, String)> {
    unsafe {
        let mut info: libc::Dl_info = std::mem::zeroed();
        if libc::dladdr(pc as *const c_void, &mut info) == 0 || info.dli_sname.is_null() {
            return None;
        }
        let sym = std::ffi::CStr::from_ptr(info.dli_sname)
            .to_string_lossy()
            .into_owned();
        let image = if info.dli_fname.is_null() {
            String::new()
        } else {
            std::ffi::CStr::from_ptr(info.dli_fname)
                .to_string_lossy()
                .into_owned()
        };
        Some((sym, image))
    }
}

/// No `dladdr` here, and no sampler to feed it PCs (see [`sampler`]) — this is
/// only ever compiled for the sake of [`classify`]'s cross-platform body.
#[cfg(not(unix))]
fn dladdr_symbol(_pc: u64) -> Option<(String, String)> {
    None
}

/// Turn the mangled name `dladdr` returns into something readable.
///
/// Both Rust manglings appear in one profile: the JIT crates are compiled with
/// the legacy `_ZN..E` scheme and `ash_std` with v0 (`_R..`), and a C symbol
/// like `hlp_get_obj_rt` must survive untouched. `rustc-demangle` handles all
/// three. The leading underscore Mach-O prepends is stripped first, since it
/// hides the `_R` prefix from the v0 parser.
fn demangle(sym: &str) -> String {
    // Mach-O's `dladdr` reports symbols with the leading underscore already
    // stripped, which hides the `_R` / `_ZN` prefix the parser looks for, so
    // the restored form is tried as well.
    let restored = format!("_{sym}");
    for s in [sym, restored.as_str()] {
        let out = format!("{:#}", rustc_demangle::demangle(s));
        if out != s {
            return out;
        }
    }
    sym.to_string()
}

// ─────────────────────────────────────────────────────────────────────────────
// Report
// ─────────────────────────────────────────────────────────────────────────────

/// Print everything collected. Safe to call when profiling is off (does
/// nothing) and safe to call twice (the second call sees an empty profile).
pub fn report() {
    if !enabled() {
        return;
    }
    let sampled = sampling_enabled();
    if sampled {
        sampler::stop();
    }

    let mut out = String::new();
    if phases_enabled() {
        write_phase_tree(&mut out);
    }
    if sampled {
        write_sample_profile(&mut out);
    }

    match std::env::var("ASH_PROFILE_OUT") {
        Ok(path) if !path.is_empty() => match std::fs::File::create(&path) {
            Ok(mut f) => {
                let _ = f.write_all(out.as_bytes());
                eprintln!("[profile] written to {path}");
            }
            Err(e) => {
                eprintln!("[profile] could not write {path}: {e}");
                eprint!("{out}");
            }
        },
        _ => eprint!("{out}"),
    }
    PHASES_ON.store(false, Ordering::SeqCst);
}

fn ms(ns: u64) -> f64 {
    ns as f64 / 1e6
}

fn write_phase_tree(out: &mut String) {
    use std::fmt::Write as _;
    let reg = registry().lock().unwrap();
    let roots: Vec<u32> = reg
        .nodes
        .iter()
        .enumerate()
        .filter(|(_, n)| n.parent == NO_PARENT)
        .map(|(i, _)| i as u32)
        .collect();
    if roots.is_empty() && reg.counters.is_empty() {
        return;
    }

    let wall = WALL_START.get().map(|t| t.elapsed().as_nanos() as u64);
    let _ = writeln!(
        out,
        "\n─── phases ─────────────────────────────────────────────────────────"
    );
    if let Some(w) = wall {
        let _ = writeln!(out, "wall {:.1}ms", ms(w));
    }
    let _ = writeln!(
        out,
        "{:<44} {:>10} {:>10} {:>8}  {}",
        "phase", "total ms", "self ms", "calls", "% wall"
    );

    for root in roots {
        write_node(out, &reg, root, 0, wall);
    }

    if !reg.counters.is_empty() {
        let _ = writeln!(
            out,
            "\n─── counters ───────────────────────────────────────────────────────"
        );
        let mut counters = reg.counters.clone();
        counters.sort_by(|a, b| a.0.cmp(b.0));
        for (label, v) in counters {
            let _ = writeln!(out, "{:<44} {:>10}", label, v);
        }
    }
}

fn write_node(out: &mut String, reg: &Registry, id: u32, depth: usize, wall: Option<u64>) {
    use std::fmt::Write as _;
    let node = &reg.nodes[id as usize];
    let indent = "  ".repeat(depth);
    let pct = match wall {
        Some(w) if w > 0 => format!("{:>5.1}%", node.total_ns as f64 * 100.0 / w as f64),
        _ => String::new(),
    };
    // A root has no timings of its own; it is a heading for one thread.
    if node.parent == NO_PARENT && node.count == 0 {
        let _ = writeln!(out, "{}{}", indent, node.label);
    } else {
        let _ = writeln!(
            out,
            "{:<44} {:>10.2} {:>10.2} {:>8}  {}",
            format!("{}{}", indent, node.label),
            ms(node.total_ns),
            ms(node.self_ns),
            node.count,
            pct
        );
    }
    let mut children = node.children.clone();
    children.sort_by(|a, b| {
        reg.nodes[*b as usize]
            .total_ns
            .cmp(&reg.nodes[*a as usize].total_ns)
    });
    for c in children {
        write_node(out, reg, c, depth + 1, wall);
    }
}

fn write_sample_profile(out: &mut String) {
    use std::fmt::Write as _;
    let n = sampler::recorded();
    let dropped = sampler::DROPPED.load(Ordering::Relaxed);
    let hz = sampler::HZ.load(Ordering::Relaxed);

    let _ = writeln!(
        out,
        "\n─── samples ────────────────────────────────────────────────────────"
    );
    if n == 0 {
        let _ = writeln!(
            out,
            "no samples collected (the run may have been shorter than one {}Hz tick)",
            hz
        );
        return;
    }
    // Each sample stands for an equal slice of the CPU time the sampled thread
    // actually received — NOT for 1/hz seconds. Those differ whenever the
    // process did not have a core to itself, and conflating them is how a
    // profile taken on a loaded machine ends up quietly overstating everything.
    let wall_ms = WALL_START
        .get()
        .map(|t| t.elapsed().as_secs_f64() * 1e3)
        .unwrap_or(0.0);
    let cpu_ms = thread_cpu_ms() - sampler::START_CPU_MS.load(Ordering::Relaxed) as f64;
    let per_sample_ms = if cpu_ms > 0.0 {
        cpu_ms / n as f64
    } else {
        1000.0 / hz as f64
    };
    let _ = writeln!(
        out,
        "{} samples · {:.0}ms CPU on the sampled thread · {:.0}ms wall{}",
        n,
        cpu_ms,
        wall_ms,
        if dropped > 0 {
            format!(" · {dropped} dropped, buffer full")
        } else {
            String::new()
        }
    );

    // Whether the profile can be trusted comes from the CPU/wall ratio, not
    // from the tick count. Ticks always run under the requested rate — the
    // sampler pays a sleep and a syscall per tick, and standard signals
    // coalesce rather than queue — so a shortfall alone says nothing. A thread
    // that did not get a core, on the other hand, was measured while competing
    // with something else, and its proportions are skewed toward whatever ran
    // on-CPU: code that blocks on a lock is off-CPU and undersampled exactly
    // when contention makes it matter most.
    if wall_ms > 0.0 {
        let effective_hz = n as f64 / (wall_ms / 1000.0);
        let _ = writeln!(
            out,
            "  {:.0}Hz effective (requested {}Hz) · each sample ≈ {:.2}ms",
            effective_hz, hz, per_sample_ms
        );
        let cpu_ratio = cpu_ms / wall_ms;
        if cpu_ratio < 0.9 {
            let _ = writeln!(
                out,
                "  NOTE: the sampled thread held a core only {:.0}% of the time — it was\n\
                 \x20       competing with something else. Time spent blocked is invisible to a\n\
                 \x20       PC sampler, so these proportions are skewed toward on-CPU work.",
                cpu_ratio * 100.0
            );
        }
    }

    let mut jit = jit_code().lock().unwrap();
    jit.sort_by_key(|r| r.addr);

    let (Some(pcs), Some(tags)) = (sampler::PCS.get(), sampler::TAGS.get()) else {
        return;
    };

    // Cache classification per PC: a hot loop revisits the same handful of
    // addresses thousands of times, and `dladdr` is not cheap.
    let mut by_pc: HashMap<u64, (Bucket, String)> = HashMap::new();
    let mut by_site: HashMap<(Bucket, String), u64> = HashMap::new();
    let mut by_bucket: HashMap<Bucket, u64> = HashMap::new();
    let mut by_findex: HashMap<u32, u64> = HashMap::new();

    let lrs = sampler::LRS.get();
    for i in 0..n {
        let pc = pcs[i].load(Ordering::Relaxed);
        let mut entry = by_pc
            .entry(pc)
            .or_insert_with(|| classify(pc, &jit))
            .clone();

        // A libc leaf says nothing about which subsystem is responsible. Ask
        // the caller, and adopt its bucket when it has a real one — this is
        // what moves the collector's `madvise` and lock traffic out of
        // `native` and into `gc`, where the cost actually belongs.
        if entry.0 == Bucket::Native {
            if let Some(lr) = lrs
                .map(|l| l[i].load(Ordering::Relaxed))
                .filter(|&l| l != 0)
            {
                let caller = by_pc
                    .entry(lr)
                    .or_insert_with(|| classify(lr, &jit))
                    .clone();
                if matches!(caller.0, Bucket::Gc | Bucket::Runtime | Bucket::Interp) {
                    entry = (caller.0, format!("{}  ← {}", entry.1, caller.1));
                }
            }
        }

        *by_site.entry(entry.clone()).or_insert(0) += 1;
        *by_bucket.entry(entry.0).or_insert(0) += 1;
        let findex = tags[i].load(Ordering::Relaxed) as u32;
        if findex != u32::MAX {
            *by_findex.entry(findex).or_insert(0) += 1;
        }
    }

    let total = n as f64;
    let _ = writeln!(out, "\nby engine");
    let mut buckets: Vec<_> = by_bucket.into_iter().collect();
    buckets.sort_by(|a, b| b.1.cmp(&a.1));
    for (b, c) in buckets {
        let _ = writeln!(
            out,
            "  {:<12} {:>8} {:>7.1}% {:>10.0}ms",
            b.label(),
            c,
            c as f64 * 100.0 / total,
            c as f64 * per_sample_ms
        );
    }

    let _ = writeln!(out, "\nby location (self time)");
    let mut sites: Vec<_> = by_site.into_iter().collect();
    sites.sort_by(|a, b| b.1.cmp(&a.1));
    for ((bucket, name), c) in sites.into_iter().take(40) {
        let _ = writeln!(
            out,
            "  {:>7.1}% {:>8} {:<10} {}",
            c as f64 * 100.0 / total,
            c,
            bucket.label(),
            name
        );
    }

    if !by_findex.is_empty() {
        let _ = writeln!(
            out,
            "\nby interpreted function (samples taken while this findex was on the interpreter's stack)"
        );
        let mut fx: Vec<_> = by_findex.into_iter().collect();
        fx.sort_by(|a, b| b.1.cmp(&a.1));
        for (findex, c) in fx.into_iter().take(20) {
            let name = resolve_name(findex).unwrap_or_default();
            let _ = writeln!(
                out,
                "  {:>7.1}% {:>8} findex={:<8} {}",
                c as f64 * 100.0 / total,
                c,
                findex,
                name
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn demangles_rust_legacy_symbols() {
        assert_eq!(
            demangle("_ZN8ash_core7profile6report17h0123456789abcdefE"),
            "ash_core::profile::report"
        );
        // Already-readable C symbols pass through.
        assert_eq!(demangle("hlp_get_obj_rt"), "hlp_get_obj_rt");
        // Truncated or unexpected input is returned rather than panicking.
        assert_eq!(demangle("_ZN99tooshort"), "_ZN99tooshort");
    }

    #[test]
    fn scopes_are_inert_when_disabled() {
        // The default state: no env var read, nothing recorded.
        assert!(!phases_enabled());
        let _s = scope("never-recorded");
        count("never-counted", 5);
        assert!(registry().lock().unwrap().counters.is_empty());
    }
}
