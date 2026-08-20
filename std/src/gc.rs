use crate::error::{HLException, TrapContext, VDynamicException};
use crate::hl::{self, hl_type, hl_type_obj, vclosure, vdynamic, HL_WSIZE};
use crate::types::hlp_type_size;
use anyhow::Result;
use std::cell::RefCell;
use std::os::raw::c_void;
use std::ptr::{self, NonNull};
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::OnceLock;
use std::time::{Duration, Instant};
use std::{collections::HashSet, mem};

const BLOCK_SIZE: usize = 32 * 1024; // 32 KB
const LINE_SIZE: usize = 128; // 128 bytes
const LINES_PER_BLOCK: usize = BLOCK_SIZE / LINE_SIZE;

/// Maximum heap reservation (virtual, demand-committed — NOT resident).
const DEFAULT_HEAP_MB: usize = 512;
/// First collection fires after this many bytes allocated (wren_lift
/// gc_marksweep INITIAL_THRESHOLD pattern).
const INITIAL_TRIGGER_BYTES: usize = 4 * 1024 * 1024;
/// Adaptive threshold bounds: live*2 clamped to [floor, ceiling].
const DEFAULT_TRIGGER_FLOOR: usize = 8 * 1024 * 1024;
const TRIGGER_CEILING: usize = 64 * 1024 * 1024;
/// Wall-clock heartbeat: any allocation this long after the last collection
/// forces one, so long-idle processes deflate (wren_lift gc.rs:715-719).
const HEARTBEAT: Duration = Duration::from_secs(30);
/// Throttle for malloc_zone_pressure_relief (avoid per-alloc syscalls in
/// stress mode).
const PRESSURE_RELIEF_MIN_INTERVAL: Duration = Duration::from_millis(500);

// ── Env-gated config (OnceLock-cached — uncached getenv on the alloc hot
// path cost wren_lift 30x throughput; gc.rs:38-46) ──────────────────────────

/// How many words [`spill_callee_saved`] writes.
const CALLEE_SAVED_WORDS: usize = 10;

/// Write the callee-saved general-purpose registers into `buf`.
///
/// A conservative collector that scans only the machine stack misses anything
/// the compiler chose to keep in a register across a call. On aarch64 that is
/// x19–x28; the float registers (d8–d15) are excluded deliberately, because a
/// GC pointer is never held in one.
///
/// `#[inline(never)]` so the store cannot be sunk past the scan that reads it.
#[cfg(target_arch = "aarch64")]
#[inline(never)]
fn spill_callee_saved(buf: &mut [usize; CALLEE_SAVED_WORDS]) {
    unsafe {
        std::arch::asm!(
            "stp x19, x20, [{p}, #0]",
            "stp x21, x22, [{p}, #16]",
            "stp x23, x24, [{p}, #32]",
            "stp x25, x26, [{p}, #48]",
            "stp x27, x28, [{p}, #64]",
            p = in(reg) buf.as_mut_ptr(),
            options(nostack, preserves_flags),
        );
    }
}

/// x86-64: rbx, rbp, r12–r15.
#[cfg(target_arch = "x86_64")]
#[inline(never)]
fn spill_callee_saved(buf: &mut [usize; CALLEE_SAVED_WORDS]) {
    unsafe {
        std::arch::asm!(
            "mov [{p} + 0], rbx",
            "mov [{p} + 8], rbp",
            "mov [{p} + 16], r12",
            "mov [{p} + 24], r13",
            "mov [{p} + 32], r14",
            "mov [{p} + 40], r15",
            p = in(reg) buf.as_mut_ptr(),
            options(nostack, preserves_flags),
        );
    }
}

#[cfg(not(any(target_arch = "aarch64", target_arch = "x86_64")))]
#[inline(never)]
fn spill_callee_saved(_buf: &mut [usize; CALLEE_SAVED_WORDS]) {}

use std::sync::atomic::AtomicUsize;

// ── Mutator bump region (TLAB) ──────────────────────────────────────────────
//
// The mutator (the thread running HL code — HashLink is !HL_THREADS, so
// there is exactly one) allocates through a private bump region carved out
// of an ordinary Immix block. The fast path is a load, a line-straddle
// check, an add, a compare and a store: no lock, no condvar, no per-object
// memset (the region is zeroed once at refill), no trigger bookkeeping
// (accounted per region). Everything else — broker threads, oversized
// objects, stress mode — takes the locked path unchanged.
//
// Soundness notes:
// * the region's block is in `used_blocks` and `sweep` never frees the
//   block named by `tlab_block`, so a collection mid-region is safe: live
//   objects in it are conservatively marked from the mutator stack like any
//   others, and the bump cursor stays valid because the block stays ours;
// * small objects never straddle a 128-byte line (the straddle check), so
//   the conservative trace's line-granular scan always sees a whole object;
// * `ASH_GC_STRESS` disables the TLAB outright — stress promises a
//   collection every Nth allocation, and a bump path that skips the counter
//   would quietly break that contract.
//
// The cursor and limit are exported statics so a JIT tier can inline the
// bump sequence later; today they are used from these Rust fast paths.

#[no_mangle]
pub static ASH_TLAB_CUR: AtomicUsize = AtomicUsize::new(0);
#[no_mangle]
pub static ASH_TLAB_LIMIT: AtomicUsize = AtomicUsize::new(0);
/// pthread of the mutator, recorded when it registers its stack top.
static MUTATOR_THREAD: AtomicU64 = AtomicU64::new(0);

/// Largest object the bump region serves. At one line, nothing in the
/// region ever needs an `alloc_sizes` span entry.
const TLAB_MAX_OBJ: usize = LINE_SIZE;

/// The current thread's identity, cheap enough for a per-allocation check.
///
/// `libc::pthread_self` was 15.8% of an allocation-bound profile — it is a
/// real function call. On macOS/aarch64 the pthread pointer lives in
/// TPIDRRO_EL0, so reading the register directly is the same value for one
/// instruction. (The low bits carry the CPU number on some OS versions;
/// masking 3 bits matches what libpthread itself does — and both sides of
/// every comparison go through this same function, so even a masking
/// difference could not produce a false positive.)
#[inline(always)]
fn thread_self_fast() -> u64 {
    #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
    unsafe {
        let tpidrro: u64;
        std::arch::asm!("mrs {}, TPIDRRO_EL0", out(reg) tpidrro, options(nomem, nostack, preserves_flags));
        tpidrro & !0x7
    }
    #[cfg(not(all(target_os = "macos", target_arch = "aarch64")))]
    unsafe {
        libc::pthread_self() as u64
    }
}

#[inline]
fn on_mutator() -> bool {
    MUTATOR_THREAD.load(Ordering::Relaxed) == thread_self_fast()
}

/// TLAB enabled? Off under stress, and via ASH_GC_TLAB=0.
fn tlab_enabled() -> bool {
    static V: OnceLock<bool> = OnceLock::new();
    *V.get_or_init(|| {
        gc_stress_every() == 0
            && !matches!(std::env::var("ASH_GC_TLAB").as_deref(), Ok("0") | Ok("off"))
    })
}

/// The allocation entry point for runtime helpers.
///
/// Returns zeroed memory (the invariant every caller of the old
/// `gc_locked().allocate(..)` relied on), taking the mutator's bump region
/// when it can and the locked path when it cannot.
pub fn gc_alloc(size: usize) -> Option<NonNull<u8>> {
    let aligned = (size.max(8) + 15) & !15;
    if aligned <= TLAB_MAX_OBJ && tlab_enabled() && on_mutator() {
        let cur = ASH_TLAB_CUR.load(Ordering::Relaxed);
        if cur != 0 {
            let mut p = cur;
            if (p & (LINE_SIZE - 1)) + aligned > LINE_SIZE {
                p = (p + LINE_SIZE - 1) & !(LINE_SIZE - 1);
            }
            let np = p + aligned;
            if np <= ASH_TLAB_LIMIT.load(Ordering::Relaxed) {
                ASH_TLAB_CUR.store(np, Ordering::Relaxed);
                // Pre-zeroed at refill.
                return Some(unsafe { NonNull::new_unchecked(p as *mut u8) });
            }
        }
        return tlab_refill_then_alloc(aligned);
    }
    gc_locked().allocate(size)
}

/// Region exhausted (or never opened): take the lock, run the ordinary
/// trigger logic, carve a fresh block, zero it once, and serve the pending
/// allocation from its head.
#[cold]
fn tlab_refill_then_alloc(aligned: usize) -> Option<NonNull<u8>> {
    let mut gc = gc_locked();
    // A refill is a true safepoint, so a due trigger COLLECTS here instead
    // of deferring to the interpreter's next snapshot. During an
    // OSR-compiled phase there are no snapshots, and the deferral let the
    // heap run to full reservation before the exhaustion backstop fired:
    // mandelbrot parked at 590MB RSS with a 3.5MB live set, collections
    // arrived 508MB apart, and every allocation marched through cold
    // never-recycled pages — a large share of its memset cost. Collecting
    // on the trigger is exactly as safe as the backstop already was: same
    // thread, conservative stack scan covering the compiled frames, and
    // the registered interpreter ranges are complete as of their last
    // sync (a superset is over-retention, never under-rooting).
    gc.maybe_collect_at_safepoint();
    let block = match gc.acquire_free_block() {
        Some(b) => b,
        None => {
            gc.collect_garbage();
            gc.acquire_free_block()?
        }
    };
    let base = unsafe { gc.heap.memory.as_mut_ptr().add(block) };
    unsafe { std::ptr::write_bytes(base, 0, BLOCK_SIZE) };
    gc.heap.tlab_block = Some(block);
    // Coarse trigger accounting: the whole region counts when it is carved,
    // not per object. Slightly early triggers, never late ones.
    gc.heap.bytes_since_gc += BLOCK_SIZE;
    gc.heap.alloc_count += 1;
    GC_STATS
        .bytes_allocated
        .fetch_add(BLOCK_SIZE as u64, Ordering::Relaxed);
    ASH_TLAB_CUR.store(base as usize + aligned, Ordering::Relaxed);
    ASH_TLAB_LIMIT.store(base as usize + BLOCK_SIZE, Ordering::Relaxed);
    Some(unsafe { NonNull::new_unchecked(base) })
}

/// Trace flags, read once. `std::env::var` per allocation took the macOS
/// process-wide getenv lock on the hottest path in the program — the exact
/// mistake the opcode-dispatch env flags already document.
fn trace_alloc() -> bool {
    static V: OnceLock<bool> = OnceLock::new();
    *V.get_or_init(|| std::env::var("ASH_GC_TRACE_ALLOC").is_ok())
}
fn trace_freed() -> bool {
    static V: OnceLock<bool> = OnceLock::new();
    *V.get_or_init(|| std::env::var("ASH_GC_TRACE_FREED").is_ok())
}

fn env_usize(name: &str) -> Option<usize> {
    std::env::var(name).ok().and_then(|v| v.trim().parse().ok())
}

/// Maximum heap reservation in bytes (ASH_GC_HEAP_MB, default 512).
fn heap_max_bytes() -> usize {
    static V: OnceLock<usize> = OnceLock::new();
    *V.get_or_init(|| {
        let mb = env_usize("ASH_GC_HEAP_MB")
            .unwrap_or(DEFAULT_HEAP_MB)
            .max(32);
        (mb * 1024 * 1024 / BLOCK_SIZE) * BLOCK_SIZE
    })
}

/// Adaptive-trigger floor in bytes (ASH_GC_TRIGGER_MB overrides).
fn trigger_floor_bytes() -> usize {
    static V: OnceLock<usize> = OnceLock::new();
    *V.get_or_init(|| {
        env_usize("ASH_GC_TRIGGER_MB")
            .map(|mb| (mb * 1024 * 1024).max(1024 * 1024))
            .unwrap_or(DEFAULT_TRIGGER_FLOOR)
    })
}

/// ASH_GC_STATS=1: per-collection trace lines + end-of-run summary.
fn gc_stats_enabled() -> bool {
    static V: OnceLock<bool> = OnceLock::new();
    *V.get_or_init(|| {
        std::env::var("ASH_GC_STATS")
            .map(|v| v != "0" && !v.is_empty())
            .unwrap_or(false)
    })
}

/// ASH_GC_STRESS: collect at every Nth allocation (1 = every allocation).
/// 0 / unset = disabled. Validation tool for root-coverage bugs.
fn gc_stress_every() -> usize {
    static V: OnceLock<usize> = OnceLock::new();
    *V.get_or_init(|| match std::env::var("ASH_GC_STRESS") {
        Ok(v) if v == "0" || v.is_empty() => 0,
        Ok(v) => v.trim().parse().unwrap_or(1),
        Err(_) => 0,
    })
}

// ── GC statistics (atomics — safe to read from atexit / any thread without
// the GC lock; wren_lift GcStats gc.rs:527-539) ─────────────────────────────

struct GcStatCounters {
    collections: AtomicU64,
    blocks_reclaimed: AtomicU64,
    bytes_allocated: AtomicU64,
    external_bytes: AtomicU64,
    live_blocks: AtomicU64,
    pause_ns_total: AtomicU64,
    pause_ns_max: AtomicU64,
}

static GC_STATS: GcStatCounters = GcStatCounters {
    collections: AtomicU64::new(0),
    blocks_reclaimed: AtomicU64::new(0),
    bytes_allocated: AtomicU64::new(0),
    external_bytes: AtomicU64::new(0),
    live_blocks: AtomicU64::new(0),
    pause_ns_total: AtomicU64::new(0),
    pause_ns_max: AtomicU64::new(0),
};

fn fmt_mb(bytes: u64) -> String {
    format!("{:.1}MB", bytes as f64 / (1024.0 * 1024.0))
}

fn print_gc_stats_report() {
    let n = GC_STATS.collections.load(Ordering::Relaxed);
    let freed = GC_STATS.blocks_reclaimed.load(Ordering::Relaxed);
    let alloc = GC_STATS.bytes_allocated.load(Ordering::Relaxed);
    let ext = GC_STATS.external_bytes.load(Ordering::Relaxed);
    let live = GC_STATS.live_blocks.load(Ordering::Relaxed);
    let pt = GC_STATS.pause_ns_total.load(Ordering::Relaxed);
    let pm = GC_STATS.pause_ns_max.load(Ordering::Relaxed);
    eprintln!("[gc] ---- GC stats ----");
    eprintln!("[gc] collections:      {}", n);
    eprintln!(
        "[gc] blocks reclaimed: {} ({})",
        freed,
        fmt_mb(freed * BLOCK_SIZE as u64)
    );
    eprintln!(
        "[gc] bytes allocated:  {} (+ external {})",
        fmt_mb(alloc),
        fmt_mb(ext)
    );
    eprintln!(
        "[gc] live at last gc:  {} blocks ({})",
        live,
        fmt_mb(live * BLOCK_SIZE as u64)
    );
    eprintln!(
        "[gc] pause total:      {:.1}ms, max {:.2}ms, total {}ns",
        pt as f64 / 1e6,
        pm as f64 / 1e6,
        pt
    );
}

extern "C" fn gc_stats_atexit() {
    print_gc_stats_report();
}

/// On-demand GC stats dump (also printed at exit when ASH_GC_STATS=1).
#[no_mangle]
pub extern "C" fn hlp_gc_print_stats() {
    print_gc_stats_report();
}

// ── macOS return-to-OS hooks ────────────────────────────────────────────────

#[cfg(target_os = "macos")]
extern "C" {
    /// Asks all malloc zones to release free pages back to the OS
    /// (forces MADV_FREE_REUSABLE internally — wren_lift gc.rs:1493-1515).
    fn malloc_zone_pressure_relief(zone: *mut c_void, goal: usize) -> usize;
}

/// Demand-committed heap reservation: anonymous private mmap. Pages become
/// resident only on first touch, and fully-free blocks are returned via
/// madvise — RSS tracks live data, not configured capacity (wren_lift's
/// nursery idiom, gc.rs:386-391, plus wlift_alloc::pressure_release).
struct HeapMemory {
    base: *mut u8,
    len: usize,
}

impl HeapMemory {
    fn new(len: usize) -> Self {
        let ptr = unsafe {
            libc::mmap(
                std::ptr::null_mut(),
                len,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_PRIVATE | libc::MAP_ANON,
                -1,
                0,
            )
        };
        assert!(
            ptr != libc::MAP_FAILED,
            "GC heap reservation failed ({} bytes)",
            len
        );
        HeapMemory {
            base: ptr as *mut u8,
            len,
        }
    }

    #[inline(always)]
    fn as_ptr(&self) -> *const u8 {
        self.base
    }

    #[inline(always)]
    fn as_mut_ptr(&self) -> *mut u8 {
        self.base
    }
}

impl Drop for HeapMemory {
    fn drop(&mut self) {
        unsafe {
            libc::munmap(self.base as *mut c_void, self.len);
        }
    }
}

pub static mut GC: OnceLock<ImmixAllocator> = OnceLock::new();
pub static HL_GLOBAL_LOCK: OnceLock<std::sync::Mutex<()>> = OnceLock::new();

// ── Reentrant GC lock ───────────────────────────────────────────────────────
//
// The GC singleton (and its Rc<RefCell<..>> cells) is not thread-safe, but it
// is reached concurrently from the main thread and the tiered-JIT worker
// thread. Every extern "C" entry point that touches GC state must hold this
// lock. It is REENTRANT because GC operations nest on the same thread
// (e.g. allocate → collect_garbage, or the JIT worker holding hlp_gc_lock
// across its whole init, which itself calls hlp_alloc_*).
//
// Standard owner+depth pattern: `owner` is a unique per-thread token (0 =
// free). If the current thread already owns the lock, only `depth` is bumped;
// otherwise the thread waits on `inner`/`cond` until `owner` is 0. All
// cross-thread happens-before edges are provided by `inner`, which is always
// held while `owner` transitions between 0 and non-zero.

struct ReentrantGcLock {
    owner: std::sync::atomic::AtomicU64,
    depth: std::sync::atomic::AtomicUsize,
    /// Threads inside the slow acquire path. The uncontended release reads
    /// this to skip the mutex + broadcast entirely — the broadcast alone was
    /// 4.6% of an allocation-bound profile, fired with nobody listening.
    waiters: std::sync::atomic::AtomicUsize,
    inner: std::sync::Mutex<()>,
    cond: std::sync::Condvar,
}

static GC_LOCK: ReentrantGcLock = ReentrantGcLock {
    owner: std::sync::atomic::AtomicU64::new(0),
    depth: std::sync::atomic::AtomicUsize::new(0),
    waiters: std::sync::atomic::AtomicUsize::new(0),
    inner: std::sync::Mutex::new(()),
    cond: std::sync::Condvar::new(),
};

/// Unique, never-zero token for the current thread: its pthread handle.
/// The previous `thread_local!` token cost a `_tlv_get_addr` call (with its
/// lazy-init branch) on EVERY lock operation — 11.6% of an allocation-bound
/// profile; `pthread_self` is a register read.
#[inline]
fn gc_thread_token() -> u64 {
    unsafe { libc::pthread_self() as u64 }
}

impl ReentrantGcLock {
    fn acquire(&self) {
        use std::sync::atomic::Ordering;
        let me = gc_thread_token();
        // Fast path: we already own the lock — only this thread can have
        // stored `me` into owner, so a plain load is sufficient.
        if self.owner.load(Ordering::Relaxed) == me {
            self.depth.fetch_add(1, Ordering::Relaxed);
            return;
        }
        // Uncontended path: one CAS, no mutex.
        if self
            .owner
            .compare_exchange(0, me, Ordering::Acquire, Ordering::Relaxed)
            .is_ok()
        {
            self.depth.store(1, Ordering::Relaxed);
            return;
        }
        // Contended: register as a waiter (SeqCst pairs with release's
        // owner-store/waiters-load — see the comment there), then sleep.
        self.waiters.fetch_add(1, Ordering::SeqCst);
        let mut g = self.inner.lock().unwrap();
        while self
            .owner
            .compare_exchange(0, me, Ordering::Acquire, Ordering::Relaxed)
            .is_err()
        {
            g = self.cond.wait(g).unwrap();
        }
        self.waiters.fetch_sub(1, Ordering::Relaxed);
        self.depth.store(1, Ordering::Relaxed);
        drop(g);
    }

    fn release(&self) {
        use std::sync::atomic::Ordering;
        let me = gc_thread_token();
        debug_assert_eq!(
            self.owner.load(Ordering::Relaxed),
            me,
            "GC lock released by non-owner thread"
        );
        if self.depth.load(Ordering::Relaxed) > 1 {
            self.depth.fetch_sub(1, Ordering::Relaxed);
            return;
        }
        self.depth.store(0, Ordering::Relaxed);
        // SeqCst store then SeqCst load: either the releasing thread sees the
        // waiter's `waiters` increment and notifies under the mutex, or the
        // waiter's CAS loop (entered after its increment) sees owner == 0 and
        // takes the lock without needing the wakeup. Both orders are covered,
        // so the wakeup cannot be lost.
        self.owner.store(0, Ordering::SeqCst);
        if self.waiters.load(Ordering::SeqCst) != 0 {
            let _g = self.inner.lock().unwrap();
            self.cond.notify_all();
        }
    }

    /// Depth held by the CURRENT thread (0 if it is not the owner).
    fn held_depth(&self) -> usize {
        use std::sync::atomic::Ordering;
        if self.owner.load(Ordering::Relaxed) == gc_thread_token() {
            self.depth.load(Ordering::Relaxed)
        } else {
            0
        }
    }

    /// Force the current thread's hold depth down to `target`. Used on the
    /// longjmp throw path: guards held by frames being jumped over never run
    /// their Drop, so the thrower restores the depth recorded at trap setup.
    fn unwind_to(&self, target: usize) {
        use std::sync::atomic::Ordering;
        let me = gc_thread_token();
        if self.owner.load(Ordering::Relaxed) != me {
            return;
        }
        if self.depth.load(Ordering::Relaxed) <= target {
            return;
        }
        if target > 0 {
            self.depth.store(target, Ordering::Relaxed);
        } else {
            let g = self.inner.lock().unwrap();
            self.depth.store(0, Ordering::Relaxed);
            self.owner.store(0, Ordering::Relaxed);
            drop(g);
            self.cond.notify_all();
        }
    }
}

/// RAII guard for the reentrant GC lock.
pub(crate) struct GcGuard(());

impl Drop for GcGuard {
    fn drop(&mut self) {
        GC_LOCK.release();
    }
}

/// Acquire the reentrant GC lock. Every extern "C" entry point that touches
/// GC state must hold one of these (directly or via `gc_locked()`).
pub(crate) fn gc_guard() -> GcGuard {
    GC_LOCK.acquire();
    GcGuard(())
}

/// Lock-holding handle to the GC singleton. Derefs to `ImmixAllocator`;
/// the lock is held until the handle is dropped.
pub(crate) struct GcRef {
    gc: *mut ImmixAllocator,
    _guard: GcGuard,
}

impl std::ops::Deref for GcRef {
    type Target = ImmixAllocator;
    fn deref(&self) -> &ImmixAllocator {
        unsafe { &*self.gc }
    }
}

impl std::ops::DerefMut for GcRef {
    fn deref_mut(&mut self) -> &mut ImmixAllocator {
        unsafe { &mut *self.gc }
    }
}

/// Acquire the GC lock and return a handle to the (initialized) singleton.
pub(crate) fn gc_locked() -> GcRef {
    let guard = gc_guard();
    let gc = unsafe { GC.get_mut().expect("GC not initialized") as *mut ImmixAllocator };
    GcRef { gc, _guard: guard }
}

/// Acquire the GC lock, initializing the singleton if needed.
pub(crate) fn gc_locked_init() -> GcRef {
    let guard = gc_guard();
    let gc = unsafe { GC.get_mut_or_init(ImmixAllocator::new) as *mut ImmixAllocator };
    GcRef { gc, _guard: guard }
}

/// Depth of the current thread's hold on the GC lock (0 = not held).
pub(crate) fn gc_lock_held_depth() -> usize {
    GC_LOCK.held_depth()
}

/// Restore the current thread's GC-lock depth to `target`, releasing
/// ownership entirely when `target` is 0. Longjmp throw path only.
pub(crate) fn gc_lock_unwind_to(target: usize) {
    GC_LOCK.unwind_to(target);
}

/// Manually acquire the GC lock (reentrant). Used by the tiered-JIT worker
/// to hold the lock across its whole module init.
#[no_mangle]
pub unsafe extern "C" fn hlp_gc_lock() {
    GC_LOCK.acquire();
}

/// Manually release one level of the GC lock.
#[no_mangle]
pub unsafe extern "C" fn hlp_gc_unlock() {
    GC_LOCK.release();
}

struct ImmixHeap {
    memory: HeapMemory,
    free_blocks: Vec<usize>,
    used_blocks: HashSet<usize>,
    allocation_point: usize,
    current_block_end: usize,
    alloc_count: usize,
    /// For each line in the heap, stores the number of lines this allocation
    /// occupies if this is an allocation start, or 0 for continuation lines.
    /// Enables the GC to mark all lines of a multi-line object.
    alloc_sizes: Vec<u32>,
    /// GC-heap bytes allocated since the last collection.
    bytes_since_gc: usize,
    /// Off-heap bytes charged via track_external since the last collection
    /// (fiber stacks, JIT-side structures — wren_lift gc.rs:624-626).
    external_since_gc: usize,
    /// Collect when bytes_since_gc + external_since_gc reaches this.
    /// Adaptive: live*2 clamped to [floor, ceiling] after each collection.
    trigger_threshold: usize,
    /// Wall-clock heartbeat anchor.
    last_collect: Instant,
    /// Throttle anchor for malloc_zone_pressure_relief.
    last_pressure_relief: Instant,
    /// Blocks currently madvised MADV_FREE_REUSABLE; must be MADV_FREE_REUSE'd
    /// before reuse so live data can't be discarded under memory pressure.
    reusable_blocks: HashSet<usize>,
    /// The block the mutator's bump region currently lives in. `sweep` never
    /// frees it: the cursor points into it, and the youngest objects there
    /// may be live with their only references in mutator registers.
    tlab_block: Option<usize>,
    /// True once the interpreter has registered scan ranges. The interpreter
    /// roots its bytecode registers via a SNAPSHOT (sync_gc_scan_roots) that
    /// is complete only at the moment it is published — values written to
    /// registers after a snapshot are invisible until the next one. So in
    /// interp mode byte-driven collections are DEFERRED to the next snapshot
    /// publication (add_scan_range), where the root set is provably complete.
    /// JIT mode never sets this: its roots are the native stack, which the
    /// conservative scanner sees completely at any allocation point.
    safepoint_mode: bool,
    /// A trigger fired while in safepoint mode; collect at the next snapshot.
    collect_pending: bool,
}
#[derive(Debug, Clone)]
struct Block {
    mark_bits: [bool; LINES_PER_BLOCK],
    evacuation_candidate: bool,
}

struct RootSet {
    globals: Vec<*mut hl::vdynamic>,
    stack_roots: Vec<*mut hl::vdynamic>,
    persistent_roots: HashSet<*mut hl::vdynamic>,
    scan_ranges: Vec<(usize, usize)>,
}

pub struct ImmixAllocator {
    heap: ImmixHeap,
    blocks: Vec<Block>,
    roots: Rc<RefCell<RootSet>>,
    pub(crate) current_exception: Option<Box<HLException>>,
    pub(crate) exception_handler:
        Option<Box<dyn Fn(&mut HLException) -> Result<*mut vdynamic, VDynamicException>>>,


    stack_top: usize,
    globals_range: Option<(*const *mut c_void, usize)>,
    /// Registered fiber stacks for conservative scanning. id 0 is the main
    /// stack descriptor (base/size 0 — scanned as [saved_sp, stack_top)).
    fiber_stacks: Vec<FiberStackInfo>,
}

#[derive(Clone, Copy)]
pub(crate) struct FiberStackInfo {
    pub id: u32,
    pub base: usize,
    pub size: usize,
    /// SP recorded at the stack's last switch-out; 0 = never suspended.
    pub saved_sp: usize,
}

impl ImmixAllocator {
    #[inline(always)]
    fn current_stack_addr() -> usize {
        // Portable stack probe: address of a local variable approximates current SP.
        let marker = 0u8;
        (&marker as *const u8) as usize
    }

    pub fn new() -> Self {
        let heap_size = heap_max_bytes();
        let mut heap = ImmixHeap {
            memory: HeapMemory::new(heap_size),
            free_blocks: Vec::new(),
            used_blocks: HashSet::new(),
            allocation_point: 0,
            current_block_end: 0,
            alloc_count: 0,
            alloc_sizes: vec![0u32; heap_size / LINE_SIZE],
            bytes_since_gc: 0,
            external_since_gc: 0,
            trigger_threshold: INITIAL_TRIGGER_BYTES,
            last_collect: Instant::now(),
            last_pressure_relief: Instant::now(),
            reusable_blocks: HashSet::new(),
            tlab_block: None,
            safepoint_mode: false,
            collect_pending: false,
        };

        // Reverse order so pop() hands out low addresses first — touched
        // pages stay contiguous at the heap base.
        for i in (0..heap_size).step_by(BLOCK_SIZE).rev() {
            heap.free_blocks.push(i);
        }

        let blocks = vec![
            Block {
                mark_bits: [false; LINES_PER_BLOCK],
                evacuation_candidate: false,
            };
            heap_size / BLOCK_SIZE
        ];

        if gc_stats_enabled() {
            unsafe {
                libc::atexit(gc_stats_atexit);
            }
        }

        ImmixAllocator {
            heap,
            blocks,
            roots: Rc::new(RefCell::new(RootSet {
                globals: Vec::new(),
                stack_roots: Vec::new(),
                persistent_roots: HashSet::new(),
                scan_ranges: Vec::new(),
            })),
            current_exception: None,
            exception_handler: None,


            fiber_stacks: Vec::new(),
            stack_top: 0,
            globals_range: None,
        }
    }

    /// Byte-driven collection triggers, checked on every allocation
    /// (wren_lift gc_marksweep.rs trigger + gc.rs:667-725 heartbeat):
    /// 1. ASH_GC_STRESS: collect every Nth allocation (validation mode).
    /// 2. Allocated + external bytes since last collect >= adaptive threshold.
    /// 3. Wall-clock heartbeat so long-idle processes deflate.
    ///
    /// In interpreter (safepoint) mode a fired trigger is deferred to the
    /// next root-snapshot publication instead of collecting immediately —
    /// unless pressure is extreme (hard trigger), where collecting with a
    /// possibly-stale snapshot matches the old exhaustion-path behavior.
    /// [`Self::maybe_collect`] at a point known to be a safepoint: a due
    /// trigger collects immediately instead of deferring to the next
    /// interpreter snapshot.
    pub(crate) fn maybe_collect_at_safepoint(&mut self) {
        if self.stack_top == 0 {
            return;
        }
        let stress = gc_stress_every();
        let pressure = self.heap.bytes_since_gc + self.heap.external_since_gc;
        let due = if stress > 0 {
            self.heap.alloc_count + 1 >= stress
        } else {
            pressure >= self.heap.trigger_threshold
                || (self.heap.alloc_count & 1023 == 0
                    && self.heap.last_collect.elapsed() >= HEARTBEAT)
        };
        if due || self.heap.collect_pending {
            self.collect_garbage();
        }
    }

    fn maybe_collect(&mut self) {
        // No automatic collections before the host runtime has entered user
        // code (hlp_gc_set_stack_top): during bootstrap (constants/class
        // descriptor init) both engines hold GC pointers in host-side Rust
        // structures the conservative scanner cannot see. Bootstrap
        // allocation is finite; the exhaustion backstop still applies.
        if self.stack_top == 0 {
            return;
        }
        let stress = gc_stress_every();
        let pressure = self.heap.bytes_since_gc + self.heap.external_since_gc;
        let due = if stress > 0 {
            // alloc_count resets on every collection: collect on the Nth
            // allocation since the last one (N=1 → every allocation).
            self.heap.alloc_count + 1 >= stress
        } else {
            pressure >= self.heap.trigger_threshold
                // Heartbeat: clock read only every 1024 allocations.
                || (self.heap.alloc_count & 1023 == 0
                    && self.heap.last_collect.elapsed() >= HEARTBEAT)
        };
        if !due {
            return;
        }
        if self.heap.safepoint_mode {
            let hard = self
                .heap
                .trigger_threshold
                .saturating_mul(4)
                .max(TRIGGER_CEILING);
            if pressure < hard {
                self.heap.collect_pending = true;
                return;
            }
        }
        self.collect_garbage();
    }

    /// Take a block off the free list, un-madvising it first if its pages
    /// were marked reusable, and clearing any stale mark bits left by
    /// conservative scans of stale pointers into freed blocks.
    fn acquire_free_block(&mut self) -> Option<usize> {
        let addr = self.heap.free_blocks.pop()?;
        self.heap.used_blocks.insert(addr);
        self.reclaim_block_pages(addr);
        self.blocks[addr / BLOCK_SIZE].mark_bits = [false; LINES_PER_BLOCK];
        Some(addr)
    }

    /// MADV_FREE_REUSE a block whose pages were previously handed back via
    /// MADV_FREE_REUSABLE — without this, the kernel may discard the pages
    /// under memory pressure AFTER we've written live data into them.
    fn reclaim_block_pages(&mut self, addr: usize) {
        if self.heap.reusable_blocks.remove(&addr) {
            #[cfg(target_os = "macos")]
            unsafe {
                libc::madvise(
                    self.heap.memory.as_mut_ptr().add(addr) as *mut c_void,
                    BLOCK_SIZE,
                    libc::MADV_FREE_REUSE,
                );
            }
        }
    }

    /// Allocate process-lifetime memory (runtime type structures:
    /// hl_runtime_obj, vobj_proto, virtual lookup/index tables, mark bits).
    /// Pinned as a persistent root: these structures are referenced only from
    /// non-GC type memory the conservative scanner never sees, so an unpinned
    /// allocation would be reclaimed by the first collection that runs while
    /// no stack reference exists (surfaced by ASH_GC_STRESS).
    pub fn allocate_immortal(&mut self, size: usize) -> Option<NonNull<u8>> {
        let p = self.allocate(size)?;
        self.roots
            .borrow_mut()
            .persistent_roots
            .insert(p.as_ptr() as *mut hl::vdynamic);
        Some(p)
    }

    pub fn allocate(&mut self, size: usize) -> Option<NonNull<u8>> {
        let size = size.max(8);
        // 16-byte bump allocation. This used to round EVERY object up to a
        // full 128-byte line ("each object gets its own line"), which
        // amplified an alloc-heavy workload five-fold: 5x the footprint, 5x
        // the memset, 5x the block churn and collections — mandelbrot spent
        // 72.6% of its run in here. Lines are the MARK granularity, not the
        // allocation granularity: reclaim is whole-block, so a marked line
        // retaining a few neighbours costs nothing an entire retained block
        // was not already costing.
        //
        // Two placement rules keep the conservative marker sound:
        // * a small object never straddles a line, so a hit on its line
        //   covers all of it;
        // * a multi-line object starts on a line boundary and its span is
        //   recorded in `alloc_sizes`, exactly as before, so
        //   `mark_allocation_at_line`'s walk-back still finds real starts.
        let aligned_size = (size + 15) & !15;

        self.maybe_collect();

        if aligned_size > BLOCK_SIZE {
            return self.allocate_large(size);
        }

        let multi_line = aligned_size > LINE_SIZE - (self.heap.allocation_point & (LINE_SIZE - 1));
        let mut point = self.heap.allocation_point;
        if aligned_size >= LINE_SIZE {
            // Line-aligned start; span recorded below.
            point = (point + LINE_SIZE - 1) & !(LINE_SIZE - 1);
        } else if multi_line {
            // Would straddle a line: skip to the next boundary.
            point = (point + LINE_SIZE - 1) & !(LINE_SIZE - 1);
        }

        if point + aligned_size > self.heap.current_block_end {
            let new_block = match self.acquire_free_block() {
                Some(b) => b,
                None => {
                    // Exhaustion backstop trigger.
                    self.collect_garbage();
                    self.acquire_free_block()? // None = out of memory
                }
            };
            point = new_block;
            self.heap.current_block_end = new_block + BLOCK_SIZE;
        }

        let result = unsafe {
            let ptr = self.heap.memory.as_mut_ptr().add(point);
            // Zero the allocation — HashLink semantics require zeroed memory.
            // Reused GC blocks contain stale data that would be misinterpreted
            // as valid pointers by the conservative scanner and HDLL code.
            std::ptr::write_bytes(ptr, 0, aligned_size);
            NonNull::new_unchecked(ptr)
        };

        if trace_alloc() {
            let base = self.heap.memory.as_ptr() as usize;
            eprintln!("[gc-alloc] {:#x} size={size}", base + point);
        }
        if aligned_size >= LINE_SIZE {
            // Multi-line span for the marker's walk-back. The span is rounded
            // up so its tail line is not shared: a small object packed after
            // it would make the walk-back ambiguous.
            let start_line = point / LINE_SIZE;
            let num_lines = (aligned_size + LINE_SIZE - 1) / LINE_SIZE;
            self.heap.alloc_sizes[start_line] = num_lines as u32;
            for i in 1..num_lines {
                self.heap.alloc_sizes[start_line + i] = 0;
            }
            self.heap.allocation_point = point + num_lines * LINE_SIZE;
        } else {
            self.heap.allocation_point = point + aligned_size;
        }
        self.heap.alloc_count += 1;
        self.heap.bytes_since_gc += aligned_size;
        GC_STATS
            .bytes_allocated
            .fetch_add(aligned_size as u64, Ordering::Relaxed);

        Some(result)
    }

    pub fn allocate_large(&mut self, size: usize) -> Option<NonNull<u8>> {
        let blocks_needed = (size + BLOCK_SIZE - 1) / BLOCK_SIZE;
        // Find contiguous free blocks by sorting the free list and scanning for a run.
        self.heap.free_blocks.sort_unstable();

        let mut run_start = None;
        let mut run_len = 0;
        for i in 0..self.heap.free_blocks.len() {
            let block = self.heap.free_blocks[i];
            if run_len == 0 {
                run_start = Some(i);
                run_len = 1;
            } else {
                let prev = self.heap.free_blocks[i - 1];
                if block == prev + BLOCK_SIZE {
                    run_len += 1;
                } else {
                    run_start = Some(i);
                    run_len = 1;
                }
            }
            if run_len >= blocks_needed {
                // Found a contiguous run — remove these blocks from free list
                let start_idx = run_start.unwrap();
                let start_addr = self.heap.free_blocks[start_idx];
                let removed: Vec<usize> = self
                    .heap
                    .free_blocks
                    .drain(start_idx..start_idx + blocks_needed)
                    .collect();
                for block in removed {
                    self.heap.used_blocks.insert(block);
                    self.reclaim_block_pages(block);
                    self.blocks[block / BLOCK_SIZE].mark_bits = [false; LINES_PER_BLOCK];
                }
                self.heap.bytes_since_gc += blocks_needed * BLOCK_SIZE;
                GC_STATS
                    .bytes_allocated
                    .fetch_add((blocks_needed * BLOCK_SIZE) as u64, Ordering::Relaxed);
                // Record allocation size for GC multi-line marking
                let num_lines = (size + LINE_SIZE - 1) / LINE_SIZE;
                let start_line = start_addr / LINE_SIZE;
                self.heap.alloc_sizes[start_line] = num_lines as u32;
                for j in 1..num_lines {
                    self.heap.alloc_sizes[start_line + j] = 0;
                }
                return Some(unsafe {
                    let ptr = self.heap.memory.as_mut_ptr().add(start_addr);
                    std::ptr::write_bytes(ptr, 0, blocks_needed * BLOCK_SIZE);
                    NonNull::new_unchecked(ptr)
                });
            }
        }

        // No contiguous run found — trigger GC and retry
        self.collect_garbage();
        if self.heap.free_blocks.len() >= blocks_needed {
            return self.allocate_large(size);
        }
        None // Out of memory
    }

    pub unsafe fn allocate_closure_ptr(
        &mut self,
        t: *mut hl_type,
        fun: *mut std::ffi::c_void,
        ptr: *mut std::ffi::c_void,
    ) -> *mut vclosure {
        // Allocate memory for the closure
        let closure = self
            .allocate(mem::size_of::<vclosure>())
            .expect("Failed to allocate memory for closure")
            .as_ptr() as *mut vclosure;

        let stack = 0;

        // Initialize the closure fields
        ptr::write(
            closure,
            vclosure {
                t,
                fun,
                hasValue: 1,
                stackCount: stack,
                value: ptr,
            },
        );

        closure
    }

    pub unsafe fn is_gc_ptr<T>(&self, ptr: *const T) -> bool {
        // Cast the pointer to a usize for address arithmetic
        let addr = ptr as usize;

        // Check if the address is within the heap
        if addr < self.heap.memory.as_ptr() as usize
            || addr >= (self.heap.memory.as_ptr() as usize + self.heap.memory.len)
        {
            return false;
        }

        // Calculate the block index
        let block_index = (addr - self.heap.memory.as_ptr() as usize) / BLOCK_SIZE;

        // Check if the block is in use
        if !self.heap.used_blocks.contains(&(block_index * BLOCK_SIZE)) {
            return false;
        }

        // Calculate the line index within the block
        let line_index = ((addr % BLOCK_SIZE) / LINE_SIZE) as usize;

        // Check if the line is marked (i.e., in use)
        if !self.blocks[block_index].mark_bits[line_index] {
            return false;
        }

        // If it's a vdynamic pointer, we need to check its internal pointer as well
        if std::mem::size_of::<T>() == std::mem::size_of::<hl::vdynamic>() {
            // Safety: We've already checked that this pointer is within our heap
            let vd = unsafe { &*(ptr as *const hl::vdynamic) };

            // Check the type pointer
            if !vd.t.is_null() && !self.is_gc_ptr(vd.t) {
                return false;
            }

            // Check the value pointer for certain types
            match unsafe { (*vd.t).kind } {
                hl::hl_type_kind_HOBJ
                | hl::hl_type_kind_HFUN
                | hl::hl_type_kind_HARRAY
                | hl::hl_type_kind_HVIRTUAL
                | hl::hl_type_kind_HDYNOBJ
                | hl::hl_type_kind_HBYTES => {
                    if !self.is_gc_ptr(vd.v.ptr) {
                        return false;
                    }
                }
                _ => {} // Other types don't have additional pointers to check
            }
        }

        true
    }

    /// Mark all lines belonging to the allocation that contains `line`.
    /// Walks backwards to find the allocation start (line with alloc_sizes > 0),
    /// then marks all lines from start to start+size.
    /// Returns newly-marked (block_idx, line_idx) pairs.
    fn mark_allocation_at_line(&mut self, line: usize) -> Vec<(usize, usize)> {
        // Find allocation start by walking backwards
        let mut start = line;
        while start > 0 && self.heap.alloc_sizes[start] == 0 {
            start -= 1;
        }
        let num_lines = self.heap.alloc_sizes[start] as usize;
        let num_lines = if num_lines == 0 { 1 } else { num_lines };

        let mut newly_marked = Vec::new();
        // Small objects pack into lines with no `alloc_sizes` entry, so the
        // walk-back can land on an EARLIER multi-line span that does not
        // cover `line`. Reclaim is whole-block, so the only thing that must
        // hold is that the hit line itself is marked — do that first,
        // unconditionally.
        {
            let block_idx = line / LINES_PER_BLOCK;
            let line_idx = line % LINES_PER_BLOCK;
            if block_idx < self.blocks.len() && !self.blocks[block_idx].mark_bits[line_idx] {
                self.blocks[block_idx].mark_bits[line_idx] = true;
                newly_marked.push((block_idx, line_idx));
            }
        }
        for l in start..start + num_lines {
            let block_idx = l / LINES_PER_BLOCK;
            let line_idx = l % LINES_PER_BLOCK;
            if block_idx < self.blocks.len() && !self.blocks[block_idx].mark_bits[line_idx] {
                self.blocks[block_idx].mark_bits[line_idx] = true;
                newly_marked.push((block_idx, line_idx));
            }
        }
        newly_marked
    }

    /// Conservative mark: scan a memory range for values that look like heap pointers.
    /// For each match, mark ALL lines of the containing allocation.
    /// Returns list of newly-marked (block, line) pairs.
    fn conservative_scan_range(&mut self, start: usize, end: usize) -> Vec<(usize, usize)> {
        let heap_start = self.heap.memory.as_ptr() as usize;
        let heap_end = heap_start + self.heap.memory.len;
        let mut newly_marked = Vec::new();

        let mut addr = start;
        while addr + 8 <= end {
            let val = unsafe { *(addr as *const usize) };
            if val >= heap_start && val < heap_end {
                let offset = val - heap_start;
                let line = offset / LINE_SIZE;
                let block_idx = line / LINES_PER_BLOCK;
                let line_idx = line % LINES_PER_BLOCK;
                // Only process if the pointed-to line isn't already marked
                if block_idx < self.blocks.len() && !self.blocks[block_idx].mark_bits[line_idx] {
                    let alloc_marks = self.mark_allocation_at_line(line);
                    newly_marked.extend(alloc_marks);
                }
            }
            addr += 8;
        }
        newly_marked
    }

    /// Transitively scan all newly-marked heap lines for more heap pointers.
    /// When a new heap pointer is found, marks ALL lines of that allocation.
    fn conservative_trace(&mut self, initial: Vec<(usize, usize)>) {
        let heap_start = self.heap.memory.as_ptr() as usize;
        let heap_end = heap_start + self.heap.memory.len;
        let mut worklist = initial;

        while let Some((block_idx, line_idx)) = worklist.pop() {
            let line_start = heap_start + block_idx * BLOCK_SIZE + line_idx * LINE_SIZE;
            for off in (0..LINE_SIZE).step_by(8) {
                let val = unsafe { *((line_start + off) as *const usize) };
                if val >= heap_start && val < heap_end {
                    let offset = val - heap_start;
                    let child_line = offset / LINE_SIZE;
                    let child_block_idx = child_line / LINES_PER_BLOCK;
                    let child_line_idx = child_line % LINES_PER_BLOCK;
                    if child_block_idx < self.blocks.len()
                        && !self.blocks[child_block_idx].mark_bits[child_line_idx]
                    {
                        let alloc_marks = self.mark_allocation_at_line(child_line);
                        worklist.extend(alloc_marks);
                    }
                }
            }
        }
    }

    /// Charge off-heap memory (fiber stacks, JIT structures) as allocation
    /// pressure so it participates in the collection trigger. Reset after
    /// every collection (wren_lift gc.rs:624-626, 1250).
    pub fn track_external(&mut self, bytes: usize) {
        self.heap.external_since_gc = self.heap.external_since_gc.saturating_add(bytes);
        GC_STATS
            .external_bytes
            .fetch_add(bytes as u64, Ordering::Relaxed);
    }

    pub fn collect_garbage(&mut self) {
        let t0 = Instant::now();
        self.mark_roots();
        let freed_blocks = self.sweep();
        let pause = t0.elapsed();

        let live_blocks = self.heap.used_blocks.len();
        let live_bytes = live_blocks * BLOCK_SIZE;

        // Adaptive threshold: next collection after ~live*2 bytes of new
        // allocation (wren_lift gc_marksweep.rs:464-466), bounded so tiny
        // programs don't collect constantly and big ones don't stall.
        self.heap.trigger_threshold =
            (live_bytes.saturating_mul(2)).clamp(trigger_floor_bytes(), TRIGGER_CEILING);

        self.heap.bytes_since_gc = 0;
        self.heap.external_since_gc = 0;
        self.heap.alloc_count = 0;
        self.heap.collect_pending = false;
        // Reset so next allocation picks a fresh free block
        self.heap.allocation_point = 0;
        self.heap.current_block_end = 0;
        self.heap.last_collect = Instant::now();

        // Ask the malloc zones (Rust-side allocations: Vecs, boxes, side
        // tables) to hand free pages back to the OS. Throttled — it is a
        // whole-zone walk (wren_lift gc.rs:1493-1515).
        #[cfg(target_os = "macos")]
        if self.heap.last_pressure_relief.elapsed() >= PRESSURE_RELIEF_MIN_INTERVAL {
            unsafe {
                malloc_zone_pressure_relief(std::ptr::null_mut(), 0);
            }
            self.heap.last_pressure_relief = Instant::now();
        }

        // Stats (atomics — readable without the GC lock).
        let n = GC_STATS.collections.fetch_add(1, Ordering::Relaxed) + 1;
        GC_STATS
            .blocks_reclaimed
            .fetch_add(freed_blocks as u64, Ordering::Relaxed);
        GC_STATS
            .live_blocks
            .store(live_blocks as u64, Ordering::Relaxed);
        let pause_ns = pause.as_nanos() as u64;
        GC_STATS
            .pause_ns_total
            .fetch_add(pause_ns, Ordering::Relaxed);
        GC_STATS.pause_ns_max.fetch_max(pause_ns, Ordering::Relaxed);

        if gc_stats_enabled() {
            eprintln!(
                "[gc] #{} pause={:.2}ms freed={} blocks live={} blocks ({}) next-trigger={}",
                n,
                pause_ns as f64 / 1e6,
                freed_blocks,
                live_blocks,
                fmt_mb(live_bytes as u64),
                fmt_mb(self.heap.trigger_threshold as u64),
            );
        }
    }

    pub fn mark_roots(&mut self) {
        let roots = self.roots.clone();
        let root_set = roots.borrow();

        // Mark explicit roots using conservative approach:
        // Just mark the memory lines, then conservative_trace will follow pointers.
        let heap_start = self.heap.memory.as_ptr() as usize;
        let heap_end = heap_start + self.heap.memory.len;
        let mut all_newly_marked = Vec::new();

        for &global_ptr in &root_set.globals {
            let addr = global_ptr as usize;
            if addr >= heap_start && addr < heap_end {
                let line = (addr - heap_start) / LINE_SIZE;
                all_newly_marked.extend(self.mark_allocation_at_line(line));
            }
        }
        for &stack_ptr in &root_set.stack_roots {
            let addr = stack_ptr as usize;
            if addr >= heap_start && addr < heap_end {
                let line = (addr - heap_start) / LINE_SIZE;
                all_newly_marked.extend(self.mark_allocation_at_line(line));
            }
        }
        for &persistent_ptr in &root_set.persistent_roots {
            let addr = persistent_ptr as usize;
            if addr >= heap_start && addr < heap_end {
                let line = (addr - heap_start) / LINE_SIZE;
                all_newly_marked.extend(self.mark_allocation_at_line(line));
            }
        }
        let scan_ranges = root_set.scan_ranges.clone();
        drop(root_set);

        // Conservative scan of globals_data
        if let Some((globals_ptr, count)) = self.globals_range {
            let start = globals_ptr as usize;
            let end = start + count * 8;
            let newly_marked = self.conservative_scan_range(start, end);
            all_newly_marked.extend(newly_marked);
        }

        // Conservative scan of interpreter-provided ranges
        for (start, size) in scan_ranges {
            if size == 0 {
                continue;
            }
            let end = start.saturating_add(size);
            if end > start {
                let newly_marked = self.conservative_scan_range(start, end);
                all_newly_marked.extend(newly_marked);
            }
        }

        // Conservative scan of execution stacks. Collection always runs on
        // the allocating context's stack, which may be the main thread OR a
        // fiber stack — resolve the live probe SP against the registry.
        // Spill the callee-saved registers into this frame before probing.
        //
        // The scan below covers `[sp, stack_top)` — the machine stack, and
        // nothing else. A value held only in a callee-saved register at the
        // allocation point is therefore invisible, and the object it points
        // at is swept while still live. The interpreter never showed this
        // because its HL registers live in a scanned array; compiled frames
        // are exactly where a GC pointer sits in x19..x28 across a call into
        // an allocating native.
        //
        // `buf` is a local of this frame, so clamping the probe to its
        // address puts the spilled words inside the scanned range.
        let mut buf = [0usize; CALLEE_SAVED_WORDS];
        spill_callee_saved(&mut buf);
        let probe = Self::current_stack_addr().min(buf.as_ptr() as usize);
        // (8-align the probe: conservative_scan_range walks 8-byte words.)
        let sp = (probe + 7) & !7;
        let fiber_stacks = self.fiber_stacks.clone();
        let running_fiber = fiber_stacks
            .iter()
            .find(|f| f.size > 0 && sp >= f.base && sp < f.base + f.size)
            .map(|f| (f.id, f.base + f.size));
        match running_fiber {
            Some((_, top)) => {
                all_newly_marked.extend(self.conservative_scan_range(sp, top));
            }
            None => {
                if self.stack_top > 0 && sp < self.stack_top {
                    all_newly_marked.extend(self.conservative_scan_range(sp, self.stack_top));
                }
            }
        }
        // All OTHER registered stacks scan from their saved switch-out SP.
        for f in &fiber_stacks {
            if Some(f.id) == running_fiber.map(|(id, _)| id) || f.saved_sp == 0 {
                continue;
            }
            let start = (f.saved_sp + 7) & !7;
            let top = if f.size > 0 {
                f.base + f.size
            } else {
                // Main-stack descriptor: only meaningful while a fiber runs.
                if running_fiber.is_none() {
                    continue;
                }
                self.stack_top
            };
            if start < top {
                all_newly_marked.extend(self.conservative_scan_range(start, top));
            }
        }

        // Transitive conservative marking
        if !all_newly_marked.is_empty() {
            self.conservative_trace(all_newly_marked);
        }
    }

    pub fn mark_memory(&mut self, ptr: *mut u8, size: usize) {
        let heap_start = self.heap.memory.as_ptr() as usize;
        let heap_end = heap_start + self.heap.memory.len;
        let addr = ptr as usize;

        // Only mark memory within the heap range
        if addr < heap_start || addr >= heap_end {
            return;
        }

        let end_addr = (addr + size).min(heap_end);
        let mut current_addr = addr;

        while current_addr < end_addr {
            let offset = current_addr - heap_start;
            let block_index = offset / BLOCK_SIZE;
            let line_index = (offset % BLOCK_SIZE) / LINE_SIZE;

            if block_index < self.blocks.len() {
                self.blocks[block_index].mark_bits[line_index] = true;
            }

            current_addr += LINE_SIZE;
        }
    }

    pub fn mark_object(&mut self, ptr: *mut hl::hl_type) {
        if ptr.is_null() {
            return;
        }

        let heap_start = self.heap.memory.as_ptr() as usize;
        let addr = ptr as usize;

        // Only mark objects within the heap
        if addr < heap_start || addr >= heap_start + self.heap.memory.len {
            return;
        }

        let offset = addr - heap_start;
        let block_index = offset / BLOCK_SIZE;
        let line_index = (offset % BLOCK_SIZE) / LINE_SIZE;

        if block_index < self.blocks.len() && !self.blocks[block_index].mark_bits[line_index] {
            self.blocks[block_index].mark_bits[line_index] = true;

            // Mark children based on the type of object
            unsafe {
                match (*ptr).kind {
                    hl::hl_type_kind_HOBJ => {
                        let obj_ptr = (*ptr).__bindgen_anon_1.obj;
                        if !obj_ptr.is_null() {
                            let obj: &hl_type_obj = &*obj_ptr;
                            for i in 0..obj.nfields as usize {
                                if !obj.fields.is_null() {
                                    let field = &*obj.fields.add(i);
                                    self.mark_object(field.t);
                                }
                            }
                            if !obj.super_.is_null() {
                                self.mark_object(obj.super_);
                            }
                        }
                    }
                    hl::hl_type_kind_HFUN => {
                        let fun_ptr = (*ptr).__bindgen_anon_1.fun;
                        if !fun_ptr.is_null() {
                            let fun = &*fun_ptr;
                            for i in 0..fun.nargs as usize {
                                if !fun.args.is_null() {
                                    let arg = *fun.args.add(i);
                                    self.mark_object(arg);
                                }
                            }
                            if !fun.ret.is_null() {
                                self.mark_object(fun.ret);
                            }
                        }
                    }
                    hl::hl_type_kind_HENUM => {
                        let enum_ptr = (*ptr).__bindgen_anon_1.tenum;
                        if !enum_ptr.is_null() {
                            let enum_ = &*enum_ptr;
                            for i in 0..enum_.nconstructs as usize {
                                if !enum_.constructs.is_null() {
                                    let construct = &*enum_.constructs.add(i);
                                    for j in 0..construct.nparams as usize {
                                        if !construct.params.is_null() {
                                            let param = *construct.params.add(j);
                                            self.mark_object(param);
                                        }
                                    }
                                }
                            }
                        }
                    }
                    hl::hl_type_kind_HNULL => {
                        let inner_type = (*ptr).__bindgen_anon_1.tparam;
                        if !inner_type.is_null() {
                            self.mark_object(inner_type);
                        }
                    }
                    _ => {} // Other types might not have child pointers
                }
            }
        }
    }

    pub fn mark_vdynamic(&mut self, vd_ptr: *mut hl::vdynamic) {
        if vd_ptr.is_null() {
            return;
        }

        // Only dereference pointers within the GC heap
        let heap_start = self.heap.memory.as_ptr() as usize;
        let heap_end = heap_start + self.heap.memory.len;
        let addr = vd_ptr as usize;
        if addr < heap_start || addr >= heap_end {
            return; // Not a GC-managed pointer, skip
        }

        unsafe {
            let vd = &*vd_ptr;
            self.mark_memory(vd_ptr as *mut u8, mem::size_of::<hl::vdynamic>());

            // Mark the type
            if !vd.t.is_null() {
                self.mark_object(vd.t);
            }

            // Depending on the type, we might need to mark more data
            // Mark the value based on its type
            if vd.t.is_null() {
                return;
            }
            match (*vd.t).kind {
                hl::hl_type_kind_HOBJ => {
                    let obj_ptr = vd.v.ptr as *mut hl::vobj;
                    if !obj_ptr.is_null() {
                        self.mark_object((*obj_ptr).t);
                    }
                }
                hl::hl_type_kind_HFUN => {
                    let fun_ptr = vd.v.ptr as *mut hl::vclosure;
                    if !fun_ptr.is_null() {
                        self.mark_object((*fun_ptr).t);
                        // Mark the function value and environment if present
                        if !(*fun_ptr).fun.is_null() {
                            self.mark_memory(
                                (*fun_ptr).fun as *mut u8,
                                mem::size_of::<*mut ::std::os::raw::c_void>(),
                            );
                        }
                        if (*fun_ptr).hasValue != 0 && !(*fun_ptr).value.is_null() {
                            self.mark_vdynamic((*fun_ptr).value as *mut hl::vdynamic);
                        }
                    }
                }
                hl::hl_type_kind_HARRAY => {
                    let array_ptr = vd.v.ptr as *mut hl::varray;
                    if !array_ptr.is_null() {
                        self.mark_object((*array_ptr).t);
                        if !(*array_ptr).at.is_null() {
                            self.mark_object((*array_ptr).at);
                        }
                        // Mark the full varray allocation (header + data payload).
                        // Child pointers inside data will be discovered by conservative_trace.
                        let size = (*array_ptr).size.max(0) as usize;
                        let esize = if (*array_ptr).at.is_null() {
                            HL_WSIZE as usize
                        } else {
                            hlp_type_size((*array_ptr).at).max(0) as usize
                        };
                        let total = mem::size_of::<hl::varray>() + size * esize;
                        self.mark_memory(array_ptr as *mut u8, total);
                    }
                }
                // Add more cases for other types as needed
                _ => {}
            }
        }
    }

    /// Block-level collection: only reclaim entirely empty blocks.
    /// Partially-occupied blocks are retained intact — we do NOT zero individual
    /// dead lines, because conservative marking may miss some live objects whose
    /// data would be destroyed by zeroing.
    ///
    /// Freed blocks' pages are returned to the OS via madvise (batched per
    /// contiguous run) so RSS actually falls after a collection instead of
    /// plateauing at high-water. Returns the number of blocks reclaimed.
    pub fn sweep(&mut self) -> usize {
        let used_block_addrs: Vec<usize> = self.heap.used_blocks.iter().copied().collect();
        let mut freed: Vec<usize> = Vec::new();
        for block_addr in used_block_addrs {
            // The mutator's live bump region: marks still reset below for
            // the next cycle, but the block is never reclaimed under the
            // cursor.
            let is_tlab = self.heap.tlab_block == Some(block_addr);
            let block_index = block_addr / BLOCK_SIZE;
            let block = &mut self.blocks[block_index];
            let mut is_empty = true;
            for line_index in 0..LINES_PER_BLOCK {
                if block.mark_bits[line_index] {
                    is_empty = false;
                }
                block.mark_bits[line_index] = false; // Reset for next GC cycle
            }

            if is_empty && !is_tlab {
                self.heap.used_blocks.remove(&block_addr);
                if trace_freed() {
                    let base = self.heap.memory.as_ptr() as usize;
                    eprintln!(
                        "[gc-freed] {:#x}..{:#x}",
                        base + block_addr,
                        base + block_addr + BLOCK_SIZE
                    );
                }
                self.heap.free_blocks.push(block_addr);
                // Clear alloc_sizes for all lines in this freed block
                let base_line = block_index * LINES_PER_BLOCK;
                for l in base_line..base_line + LINES_PER_BLOCK {
                    self.heap.alloc_sizes[l] = 0;
                }
                freed.push(block_addr);
            }
        }

        // Return fully-free pages to the OS. macOS: MADV_FREE_REUSABLE drops
        // the pages from the process footprint immediately (the mechanism
        // malloc_zone_pressure_relief uses internally); elsewhere MADV_DONTNEED.
        // Safe: allocate() zeroes on reuse, and acquire_free_block REUSEs the
        // range before live data is written. Batched one madvise per
        // contiguous run to keep sweep cheap.
        // Pages go back to the OS only when the process has been QUIET —
        // specifically, when no collection ran for a heartbeat interval, the
        // signal the heartbeat trigger already computes. A churn workload
        // frees and re-acquires the same blocks every cycle, and any
        // per-sweep hand-back (even thresholded: keying the working set on
        // LIVE blocks fails once a bump region keeps liveness tiny while
        // churn is huge) turns into a REUSABLE+REUSE madvise pair per block
        // per cycle — still 17.3% of mandelbrot after the first attempt at
        // a threshold. Idle processes deflate on their heartbeat
        // collections, which is what the mechanism was for.
        let quiet = self.heap.last_collect.elapsed() >= HEARTBEAT;
        if quiet && !freed.is_empty() {
            let resident_target = 16;
            let surplus = self
                .heap
                .free_blocks
                .len()
                .saturating_sub(resident_target);
            let mut hand_back: Vec<usize> = freed
                .iter()
                .copied()
                .take(surplus)
                .filter(|a| !self.heap.reusable_blocks.contains(a))
                .collect();
            if !hand_back.is_empty() {
                hand_back.sort_unstable();
                let base = self.heap.memory.as_mut_ptr();
                let mut run_start = hand_back[0];
                let mut run_len = BLOCK_SIZE;
                let mut advise = |start: usize, len: usize| unsafe {
                    #[cfg(target_os = "macos")]
                    let advice = libc::MADV_FREE_REUSABLE;
                    #[cfg(not(target_os = "macos"))]
                    let advice = libc::MADV_DONTNEED;
                    libc::madvise(base.add(start) as *mut c_void, len, advice);
                };
                for &addr in &hand_back[1..] {
                    if addr == run_start + run_len {
                        run_len += BLOCK_SIZE;
                    } else {
                        advise(run_start, run_len);
                        run_start = addr;
                        run_len = BLOCK_SIZE;
                    }
                }
                advise(run_start, run_len);
                for &addr in &hand_back {
                    self.heap.reusable_blocks.insert(addr);
                }
            }
        }

        freed.len()
    }

    pub fn register_global(&mut self, ptr: *mut hl::vdynamic) {
        self.roots.borrow_mut().globals.push(ptr);
    }

    pub fn push_stack_root(&mut self, ptr: *mut hl::vdynamic) {
        self.roots.borrow_mut().stack_roots.push(ptr);
    }

    pub fn pop_stack_root(&mut self) {
        self.roots.borrow_mut().stack_roots.pop();
    }

    pub fn register_persistent(&mut self, ptr: *mut hl::vdynamic) {
        self.roots.borrow_mut().persistent_roots.insert(ptr);
    }

    pub fn unregister_persistent(&mut self, ptr: *mut hl::vdynamic) {
        self.roots.borrow_mut().persistent_roots.remove(&ptr);
    }

    pub fn clear_scan_ranges(&mut self) {
        self.heap.safepoint_mode = true;
        self.roots.borrow_mut().scan_ranges.clear();
    }

    /// Register an interpreter root snapshot. This is the interpreter's
    /// safepoint: the snapshot is complete at this instant, so a deferred
    /// collection trigger is honored here.
    pub fn add_scan_range(&mut self, ptr: *const c_void, size: usize) {
        self.heap.safepoint_mode = true;
        if !ptr.is_null() && size != 0 {
            self.roots
                .borrow_mut()
                .scan_ranges
                .push((ptr as usize, size));
        }
        if self.heap.collect_pending {
            self.collect_garbage();
        }
    }

    pub fn alloc_virtual(&mut self, t: *mut hl::hl_type) -> Option<NonNull<hl::vvirtual>> {
        unsafe {
            let virt = (*t).__bindgen_anon_1.virt;
            if virt.is_null() {
                return None;
            }

            let data_size = (*virt).dataSize;
            let nfields = (*virt).nfields;
            let total_size = std::mem::size_of::<hl::vvirtual>()
                + (nfields as usize * std::mem::size_of::<*mut std::os::raw::c_void>())
                + (data_size as usize);

            let ptr = self.allocate(total_size)?;
            let v = ptr.as_ptr() as *mut hl::vvirtual;

            // Initialize vvirtual struct
            (*v).t = t;
            (*v).value = std::ptr::null_mut();
            (*v).next = std::ptr::null_mut();

            // Calculate pointers to fields and vdata
            let fields = v.offset(1) as *mut *mut std::os::raw::c_void;
            let vdata = fields.add(nfields as usize) as *mut u8;

            // Initialize fields: each vfield[i] points to vdata + indexes[i]
            // indexes may be null if the virtual type hasn't been initialized yet
            // (the interpreter doesn't call hlp_init_virtual during setup).
            if !(*virt).indexes.is_null() {
                for i in 0..nfields as usize {
                    // indexes[i] stores absolute byte offset from start of allocation (v),
                    // NOT relative to vdata. Use v as base, not vdata.
                    let offset = *(*virt).indexes.add(i) as usize;
                    *fields.add(i) = (v as *mut u8).add(offset) as *mut std::os::raw::c_void;
                }
            } else {
                // No indexes available — zero all field pointers
                std::ptr::write_bytes(fields, 0, nfields as usize);
            }

            // Zero out vdata
            std::ptr::write_bytes(vdata, 0, data_size as usize);

            Some(NonNull::new_unchecked(v))
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_gc_register_root(ptr: *mut hl::vdynamic) {
    if ptr.is_null() {
        return;
    }
    let mut gc = gc_locked();
    gc.register_persistent(ptr);
}

#[no_mangle]
pub unsafe extern "C" fn hlp_zalloc(size: i32) -> *mut std::os::raw::c_void {
    if size < 0 {
        return ptr::null_mut();
    }

    let size_usize = size as usize;

    // gc_alloc returns zeroed memory; the write_bytes this used to do on
    // top of it was the same double-zero hlp_alloc_obj had.
    match gc_alloc(size_usize) {
        Some(ptr) => ptr.as_ptr() as *mut std::os::raw::c_void,
        None => ptr::null_mut(),
    }
}

#[no_mangle]
pub extern "C" fn hlp_mark_size(data_size: i32) -> i32 {
    let data_size = data_size as usize;
    let ptr_count = (data_size + HL_WSIZE as usize - 1) / HL_WSIZE as usize;
    (((ptr_count + 31) >> 5) * std::mem::size_of::<i32>() as usize)
        .try_into()
        .unwrap()
}

/// Walk all live heap objects, calling `visitor(obj_ptr, type_ptr)` for each.
/// Used by hot-reload to propagate field updates to existing objects.
#[no_mangle]
pub unsafe extern "C" fn hlp_gc_walk_heap(
    visitor: unsafe extern "C" fn(*mut hl::vdynamic, *mut hl::hl_type, *mut c_void),
    ctx: *mut c_void,
) {
    let _guard = gc_guard();
    let gc = match GC.get_mut() {
        Some(g) => g,
        None => return,
    };
    let heap_base = gc.heap.memory.as_ptr() as usize;

    for &block_addr in &gc.heap.used_blocks {
        let block_offset = block_addr - heap_base;
        let first_line = block_offset / LINE_SIZE;

        let mut line = first_line;
        let block_end_line = first_line + LINES_PER_BLOCK;

        while line < block_end_line {
            let alloc_lines = gc.heap.alloc_sizes[line] as usize;
            if alloc_lines == 0 {
                line += 1;
                continue;
            }

            let obj_addr = heap_base + line * LINE_SIZE;
            let obj = obj_addr as *mut hl::vdynamic;

            // Validate: first field must be a type pointer
            if !(*obj).t.is_null() {
                visitor(obj, (*obj).t, ctx);
            }

            line += alloc_lines;
        }
    }
}

/// Initialize the garbage collector. Must be called before any allocation.
#[no_mangle]
pub unsafe extern "C" fn hlp_gc_init() {
    gc_locked_init();
}

/// Record the stack top for conservative scanning.
/// Called once at JIT entry before running user code.
#[no_mangle]
pub unsafe extern "C" fn hlp_gc_set_stack_top(top: usize) {
    // The thread announcing its stack top IS the mutator — record it so the
    // TLAB fast path can tell itself apart from broker threads.
    MUTATOR_THREAD.store(thread_self_fast(), Ordering::Relaxed);
    let mut gc = gc_locked();
    gc.stack_top = top;
}

/// Register the globals_data array for conservative scanning.
/// Called after init_constants with pointer to globals array and count.
#[no_mangle]
pub unsafe extern "C" fn hlp_gc_set_globals(ptr: *const *mut c_void, count: usize) {
    let mut gc = gc_locked();
    gc.globals_range = Some((ptr, count));
}

/// Clear interpreter-provided conservative scan ranges.
#[no_mangle]
pub unsafe extern "C" fn hlp_gc_clear_scan_roots() {
    let mut gc = gc_locked();
    gc.clear_scan_ranges();
}

/// Add an interpreter-provided conservative scan range.
#[no_mangle]
pub unsafe extern "C" fn hlp_gc_add_scan_root(ptr: *const c_void, size: usize) {
    let mut gc = gc_locked();
    gc.add_scan_range(ptr, size);
}

/// Charge off-heap memory (fiber stacks, native buffers, JIT structures) as
/// GC allocation pressure. The charge participates in the byte-driven
/// collection trigger and resets after every collection.
#[no_mangle]
pub unsafe extern "C" fn hlp_gc_track_external(bytes: u64) {
    let mut gc = gc_locked_init();
    gc.track_external(bytes as usize);
}

// ── Fiber-stack registry (crate-internal, used by fiber.rs) ─────────────────

pub(crate) unsafe fn gc_register_fiber_stack(id: u32, base: usize, size: usize) {
    let mut gc = gc_locked();
    // Lazily register the main-stack descriptor the first time a fiber
    // appears, so mark_roots can scan the suspended main stack.
    if !gc.fiber_stacks.iter().any(|f| f.id == 0) {
        gc.fiber_stacks.push(FiberStackInfo {
            id: 0,
            base: 0,
            size: 0,
            saved_sp: 0,
        });
    }
    gc.fiber_stacks.push(FiberStackInfo {
        id,
        base,
        size,
        saved_sp: 0,
    });
}

pub(crate) unsafe fn gc_update_fiber_sp(id: u32, sp: usize) {
    let mut gc = gc_locked();
    if let Some(f) = gc.fiber_stacks.iter_mut().find(|f| f.id == id) {
        f.saved_sp = sp;
    }
}

/// Must be called BEFORE the fiber's stack memory is freed.
pub(crate) unsafe fn gc_unregister_fiber_stack(id: u32) {
    let mut gc = gc_locked();
    gc.fiber_stacks.retain(|f| f.id != id);
}

pub(crate) unsafe fn gc_add_persistent(ptr: *mut hl::vdynamic) {
    let gc = gc_locked();
    gc.roots.borrow_mut().persistent_roots.insert(ptr);
}

pub(crate) unsafe fn gc_remove_persistent(ptr: *mut hl::vdynamic) {
    let gc = gc_locked();
    gc.roots.borrow_mut().persistent_roots.remove(&ptr);
}

/// Per-thread exception state: the trap chain, the pending exception value,
/// and retired trap contexts.
///
/// This used to live in the GC singleton, so arming a trap took the global GC
/// lock -- twice per call, since removing one takes it again, and the
/// interpreter arms a trap on every call into compiled code. On nbody that
/// lock traffic was around 15% of the run, for state no other thread may
/// touch: a `longjmp` cannot cross threads, so a trap chain is only ever read
/// by the thread that built it.
///
/// Fibers are why this is per *thread* rather than per fiber: they share an
/// OS thread and each needs its own chain, which the scheduler gets by
/// swapping these cells in and out at a switch (see [`gc_swap_exc_state`]).
/// That still works — the swap happens on the thread whose state it is.
///
/// Neither cell was ever a scanned root; `mark_roots` does not look at them.
pub(crate) struct ExcState {
    pub(crate) current_trap: *mut TrapContext,
    pub(crate) exc_value: *mut vdynamic,
    /// Retired contexts, reused by the next `setup_trap`. Traps nest strictly,
    /// so these come back in the order they were handed out.
    pub(crate) trap_pool: Vec<*mut TrapContext>,
}

thread_local! {
    static EXC_STATE: RefCell<ExcState> = const {
        RefCell::new(ExcState {
            current_trap: std::ptr::null_mut(),
            exc_value: std::ptr::null_mut(),
            trap_pool: Vec::new(),
        })
    };
}

/// Run `f` against this thread's exception state.
pub(crate) fn with_exc<R>(f: impl FnOnce(&mut ExcState) -> R) -> R {
    EXC_STATE.with(|c| f(&mut c.borrow_mut()))
}

/// Swap the live exception-state cells (trap chain head + exception value)
/// with the given values — the fiber scheduler's per-fiber state switch.
pub(crate) unsafe fn gc_swap_exc_state(
    trap: &mut *mut crate::error::TrapContext,
    exc: &mut *mut hl::vdynamic,
) {
    with_exc(|st| {
        std::mem::swap(&mut st.current_trap, trap);
        std::mem::swap(&mut st.exc_value, exc);
    });
}
