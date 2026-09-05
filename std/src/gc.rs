// `static mut` + raw-pointer access is this module's deliberate story (the
// VM's single-threaded invariant): `static_mut_refs` demands the
// `&raw`/deref spelling, and these two style lints then flag exactly that
// spelling. The trio cannot all be satisfied at once.
#![allow(clippy::deref_addrof, dangerous_implicit_autorefs)]
use crate::error::{HLException, TrapContext, VDynamicException};
use crate::hl::{self, hl_type, hl_type_obj, vclosure, vdynamic, HL_WSIZE};
use crate::types::hlp_type_size;
use anyhow::Result;
use std::cell::{Cell, RefCell};
use std::os::raw::c_void;
use std::ptr::{self, NonNull};
use std::rc::Rc;
use std::sync::atomic::{AtomicBool, AtomicI32, AtomicU64, Ordering};
use std::sync::{LazyLock, OnceLock};
use std::time::{Duration, Instant};
use std::{
    collections::{HashMap, HashSet},
    mem,
};
#[cfg(windows)]
use windows_sys::Win32::System::Memory::{
    DiscardVirtualMemory, VirtualAlloc, VirtualFree, MEM_COMMIT, MEM_RELEASE, MEM_RESERVE,
    PAGE_READWRITE,
};
#[cfg(windows)]
use windows_sys::Win32::System::Threading::GetCurrentThreadId;

const BLOCK_SIZE: usize = 32 * 1024; // 32 KB
const LINE_SIZE: usize = 128; // 128 bytes
/// The stride of every conservative walk, on the heap and on the stacks.
///
/// A machine word, not eight bytes. Every walker here reads a `usize`, so an
/// eight-byte stride on a 32-bit target reads one slot and skips the next.
/// On the heap that meant half the pointer fields of every object were never
/// traced on wasm32; an object survived only while it shared a 128-byte line
/// with one that WAS reached, which dense fresh blocks usually arranged and a
/// TLAB carved from a recycled span usually did not -- hence a corruption
/// that appeared only with both enabled and looked like anything but this.
const WORD: usize = std::mem::size_of::<usize>();
/// `a` rounded up to a word boundary.
const fn word_align_up(a: usize) -> usize {
    (a + WORD - 1) & !(WORD - 1)
}
const LINES_PER_BLOCK: usize = BLOCK_SIZE / LINE_SIZE;
/// 64 line-claim bits to a word.
const MARK_WORDS: usize = LINES_PER_BLOCK / 64;

/// Floor and ceiling on the machine-derived heap cap.
///
/// The mapping itself is virtual and demand-zeroed, so a large cap costs no
/// resident memory for it. What the cap does size eagerly is the per-block
/// metadata (`blocks`), at 257 bytes per 32KB block — about heap/128. That is
/// the whole price of a bigger ceiling: measured on a trivial program, 512MB
/// costs 31MB RSS and 4GB costs 60MB, with startup time unchanged at both.
/// The ceiling is what bounds that tax on a large machine.
const HEAP_MAX_FLOOR: usize = 512 * 1024 * 1024;
/// Four gigabytes, where the address space has room for it. A 32-bit target
/// cannot even name that number in a `usize`, and wasm's memory is bounded
/// well below it anyway, so the ceiling there is what one linear memory can
/// actually reach.
#[cfg(target_pointer_width = "64")]
const HEAP_MAX_CEILING: usize = 4 * 1024 * 1024 * 1024;
#[cfg(not(target_pointer_width = "64"))]
const HEAP_MAX_CEILING: usize = 1024 * 1024 * 1024;
/// Share of usable RAM the heap cap defaults to.
const HEAP_MAX_SHARE: usize = 4;
/// First collection fires after this many bytes allocated (wren_lift
/// gc_marksweep INITIAL_THRESHOLD pattern).
const INITIAL_TRIGGER_BYTES: usize = 4 * 1024 * 1024;
/// Adaptive threshold bounds: live*growth clamped to [floor, ceiling].
const DEFAULT_TRIGGER_FLOOR: usize = 8 * 1024 * 1024;
/// Bounds on the machine-derived ceiling (see `trigger_ceiling_bytes`).
const TRIGGER_CEILING_MIN: usize = 64 * 1024 * 1024;
const TRIGGER_CEILING_MAX: usize = 512 * 1024 * 1024;
/// Share of usable memory the collector will let a program run through
/// between collections. Conservative on purpose: peak RSS is roughly the
/// live set plus this, and a runtime that sizes itself off the machine has
/// to leave the machine to everything else on it.
/// Wall-clock heartbeat: any allocation this long after the last collection
/// forces one, so long-idle processes deflate (wren_lift gc.rs:715-719).
const HEARTBEAT: Duration = Duration::from_secs(30);
/// Throttle for malloc_zone_pressure_relief (avoid per-alloc syscalls in
/// stress mode).
#[cfg_attr(not(target_os = "macos"), allow(dead_code))] // macOS-only mechanism
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

thread_local! {
    /// This thread's bump cursor and the end of its region.
    ///
    /// Per thread, not per process: the M:N fiber pool runs VM code on every
    /// `ash-vm-*` worker, and a shared cursor would either hand two threads
    /// overlapping memory or — as it did — confine the lock-free path to one
    /// owner thread and send every other worker through the global GC lock on
    /// every allocation. That serialization is what made more workers slower.
    ///
    /// `const`-initialized so the access carries no lazy-init branch.
    ///
    /// One struct rather than one `thread_local!` per field, because each
    /// `thread_local!` is its own TLS lookup: on Mach-O that is a `_tlv_get_addr`
    /// call, and the allocation fast path touched four of them (registered,
    /// cursor, limit, cursor again). Profiling binary_trees, that one symbol was
    /// 26% of the entire run -- more than the collector. The fields are read and
    /// written through a single `TLAB.with`, so the fast path pays one lookup.
    /// Same shape as the fix in `gc_thread_token` above, for the same reason.
    static TLAB: Tlab = const {
        Tlab {
            cur: Cell::new(0),
            limit: Cell::new(0),
            block: Cell::new(usize::MAX),
            registered: Cell::new(false),
        }
    };
}

/// This thread's bump region, plus whether it is a registered mutator.
struct Tlab {
    /// Bump cursor and the end of the region.
    cur: Cell<usize>,
    limit: Cell<usize>,
    /// Heap offset of the block this thread is bumping through, so a refill
    /// can hand the previous one back to the sweep.
    block: Cell<usize>,
    /// Whether this thread is registered with `MUTATOR_WORLD`. Lives here only
    /// because the allocation fast path reads it: keeping it in its own
    /// `thread_local!` made every allocation pay a second TLS lookup.
    registered: Cell<bool>,
}

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
    // One agent, one identity. A target with no threads still has to answer,
    // and a constant is the honest answer rather than a syscall that lies.
    #[cfg(not(any(unix, windows)))]
    {
        1
    }
    #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
    unsafe {
        let tpidrro: u64;
        std::arch::asm!("mrs {}, TPIDRRO_EL0", out(reg) tpidrro, options(nomem, nostack, preserves_flags));
        tpidrro & !0x7
    }
    // Linux/x86_64: glibc's pthread_self IS a load of the TCB self-pointer
    // at fs:0x10 (offsetof(struct pthread, header.self)), so read it
    // directly and skip the PLT. This is the same trick the macOS/aarch64
    // arm above already uses, on the platform CI and the bench box actually
    // run: an allocation-bound profile there showed 16.4% of samples in
    // libc with pthread_self on top, from ~900MB of allocation going
    // through on_mutator once per object.
    //
    // A wrong value here degrades rather than breaks: on_mutator would
    // answer false and allocation would take the locked path, which is
    // slower and still correct.
    #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
    unsafe {
        let tp: u64;
        std::arch::asm!(
            "mov {}, qword ptr fs:[0x10]",
            out(reg) tp,
            options(nomem, nostack, preserves_flags)
        );
        tp
    }
    // Linux/aarch64 keeps its thread pointer in TPIDR_EL0, same idea.
    #[cfg(all(target_os = "linux", target_arch = "aarch64"))]
    unsafe {
        let tp: u64;
        std::arch::asm!("mrs {}, TPIDR_EL0", out(reg) tp, options(nomem, nostack, preserves_flags));
        tp
    }
    // Windows has no pthreads, and its cheapest identity is the thread id:
    // GetCurrentThreadId is a TEB field read behind a stub, so the same
    // "not a real function call" property holds.
    #[cfg(windows)]
    unsafe {
        GetCurrentThreadId() as u64
    }
    #[cfg(all(
        unix,
        not(all(target_os = "macos", target_arch = "aarch64")),
        not(all(target_os = "linux", target_arch = "x86_64")),
        not(all(target_os = "linux", target_arch = "aarch64"))
    ))]
    unsafe {
        libc::pthread_self() as u64
    }
}

// ── Registered mutators and stop-the-world rendezvous ──────────────────────
//
// The heap lock protects allocator metadata; it cannot also be the rendezvous
// lock. A collector owns that lock while waiting, and another mutator may
// already be asleep trying to acquire it. The registry therefore has its own
// mutex/condition variable and AIR V2 polls publish machine-stack state here.

#[derive(Clone)]
struct MutatorSnapshot {
    thread: u64,
    stack_top: usize,
    stack_sp: usize,
    saved_regs: [usize; CALLEE_SAVED_WORDS],
    scan_ranges: Vec<(usize, usize)>,
}

struct MutatorRecord {
    thread: u64,
    /// How this thread came to be a mutator. Only used to explain a slow world
    /// stop: the three kinds reach a safepoint by quite different means, and
    /// which one is late is the whole diagnosis.
    role: &'static str,
    stack_top: usize,
    stopped_sp: usize,
    saved_regs: [usize; CALLEE_SAVED_WORDS],
    blocking_depth: u32,
    parked: bool,
    scan_ranges: Vec<(usize, usize)>,
    staged_scan_ranges: Vec<(usize, usize)>,
    /// A live view of the mutator's range table: `(ranges, len)`, both raw
    /// pointers into memory it owns for as long as it is registered.
    ///
    /// Publishing by copy costs the world lock and an O(depth) copy on every
    /// interpreted call, which is most of what a recursive program does. With
    /// a view, a call writes one entry and bumps a length, and the copy
    /// happens once per collection instead -- at the snapshot below, where the
    /// mutator is already stopped and the table cannot move under us.
    scan_live: Option<(usize, usize)>,
}

#[derive(Default)]
struct MutatorWorldState {
    stop_requested: bool,
    collector: u64,
    mutators: Vec<MutatorRecord>,
}

struct MutatorWorld {
    state: std::sync::Mutex<MutatorWorldState>,
    changed: std::sync::Condvar,
}

static MUTATOR_WORLD: LazyLock<MutatorWorld> = LazyLock::new(|| MutatorWorld {
    state: std::sync::Mutex::new(MutatorWorldState::default()),
    changed: std::sync::Condvar::new(),
});
static GC_STOP_REQUESTED: AtomicBool = AtomicBool::new(false);

/// How long the collector waits for every mutator to reach a safepoint before
/// giving up on this collection.
///
/// Stopping the world is normally microseconds. This is not a budget so much
/// as the point past which waiting is worse than not collecting: by then the
/// program has been frozen long enough that a deferred collection is the
/// better of two bad outcomes.
const STOP_THE_WORLD_DEADLINE: std::time::Duration = std::time::Duration::from_millis(2000);
/// When the current stop was asked for, as nanoseconds since the process's
/// first collection. A thread that takes a long time to arrive reports where
/// it was — the collector cannot walk another thread's stack, but the thread
/// itself can, once it gets here.
static GC_STOP_ASKED_NS: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
static GC_EPOCH: LazyLock<Instant> = LazyLock::new(Instant::now);

fn register_current_mutator(stack_top: usize, role: &'static str) {
    if stack_top == 0 {
        return;
    }
    let thread = thread_self_fast();
    let mut world = MUTATOR_WORLD.state.lock().unwrap();
    while world.stop_requested && world.collector != thread {
        world = MUTATOR_WORLD.changed.wait(world).unwrap();
    }
    if let Some(record) = world.mutators.iter_mut().find(|m| m.thread == thread) {
        record.stack_top = stack_top;
    } else {
        world.mutators.push(MutatorRecord {
            thread,
            role,
            stack_top,
            stopped_sp: 0,
            saved_regs: [0; CALLEE_SAVED_WORDS],
            blocking_depth: 0,
            parked: false,
            scan_ranges: Vec::new(),
            staged_scan_ranges: Vec::new(),
            scan_live: None,
        });
    }
    TLAB.with(|t| t.registered.set(true));
}

/// Register the current OS worker using the platform's real stack boundary.
/// A guessed `sp + N` can cross an unmapped guard page and make conservative
/// scanning fault, especially with custom thread stack sizes.
pub(crate) fn gc_register_current_os_thread() {
    #[cfg(target_os = "macos")]
    let stack_top = unsafe { libc::pthread_get_stackaddr_np(libc::pthread_self()) as usize };

    #[cfg(target_os = "linux")]
    let stack_top = unsafe {
        let mut attr: libc::pthread_attr_t = mem::zeroed();
        let mut top = 0usize;
        if libc::pthread_getattr_np(libc::pthread_self(), &mut attr) == 0 {
            let mut base: *mut c_void = ptr::null_mut();
            let mut size: libc::size_t = 0;
            if libc::pthread_attr_getstack(&attr, &mut base, &mut size) == 0 && !base.is_null() {
                top = base as usize + size;
            }
            libc::pthread_attr_destroy(&mut attr);
        }
        top
    };

    #[cfg(windows)]
    let stack_top = unsafe {
        let mut low = 0usize;
        let mut high = 0usize;
        windows_sys::Win32::System::Threading::GetCurrentThreadStackLimits(&mut low, &mut high);
        high
    };

    #[cfg(not(any(target_os = "macos", target_os = "linux", windows)))]
    let stack_top = {
        let anchor = 0usize;
        (&anchor as *const usize as usize) + 1024 * 1024
    };

    if stack_top != 0 {
        register_current_mutator(stack_top, "os-worker");
    }
}

pub(crate) fn gc_unregister_current_os_thread() {
    unregister_current_mutator();
}

fn unregister_current_mutator() {
    let thread = thread_self_fast();
    let mut world = MUTATOR_WORLD.state.lock().unwrap();
    world.mutators.retain(|m| m.thread != thread);
    TLAB.with(|t| t.registered.set(false));
    MUTATOR_WORLD.changed.notify_all();
    drop(world);

    // The region goes back to the sweep with the thread that owned it.
    release_tlab_region(&mut gc_locked());
}

#[inline]
fn current_mutator_registered() -> bool {
    TLAB.with(|t| t.registered.get())
}

/// Park a registered mutator at an AIR V2 or allocation safepoint.
///
/// The spill buffer remains in this frame for the whole condition-variable
/// wait, so `stopped_sp` describes live memory until the collector releases
/// the world. Its copied words also cover architectures whose compiler keeps
/// the only reference in a callee-saved register.
#[inline(never)]
pub(crate) fn gc_safepoint() {
    if !GC_STOP_REQUESTED.load(Ordering::Acquire) || !current_mutator_registered() {
        return;
    }
    let thread = thread_self_fast();
    let mut saved_regs = [0usize; CALLEE_SAVED_WORDS];
    spill_callee_saved(&mut saved_regs);
    let sp = ImmixAllocator::current_stack_addr().min(saved_regs.as_ptr() as usize);

    let mut world = MUTATOR_WORLD.state.lock().unwrap();
    if !world.stop_requested || world.collector == thread {
        return;
    }
    let Some(index) = world.mutators.iter().position(|m| m.thread == thread) else {
        return;
    };
    {
        let record = &mut world.mutators[index];
        record.stopped_sp = sp;
        record.saved_regs = saved_regs;
        record.parked = true;
    }
    // Arriving late is the interesting case: the collector can name which
    // thread it waited on but not what that thread was doing, and only the
    // thread itself can answer that.
    //
    // Capture the frames here, but resolve and print them after the world
    // restarts. Symbolication takes tens of milliseconds, and this thread is
    // holding MUTATOR_WORLD -- the lock the collector must retake to notice
    // that we just parked. Reporting from inside it therefore lengthens the
    // very stop being reported, and when two threads arrive late they
    // symbolicate one behind the other. Measured on MBHaxe: two stragglers,
    // 197.9ms and 254.7ms, for a stop whose mark and sweep together were 12ms.
    let late = if gc_stats_enabled() {
        let asked = GC_STOP_ASKED_NS.load(Ordering::Relaxed);
        let waited_ms = (GC_EPOCH.elapsed().as_nanos() as u64 - asked) as f64 / 1e6;
        (waited_ms > 20.0).then(|| (waited_ms, std::backtrace::Backtrace::force_capture()))
    } else {
        None
    };
    MUTATOR_WORLD.changed.notify_all();
    while world.stop_requested {
        world = MUTATOR_WORLD.changed.wait(world).unwrap();
    }
    if let Some(record) = world.mutators.iter_mut().find(|m| m.thread == thread) {
        record.parked = false;
        record.stopped_sp = 0;
    }
    drop(world);
    if let Some((waited_ms, frames)) = late {
        eprintln!(
            "[gc] thread {:#x} reached a safepoint {:.1}ms after the stop was asked for; it was at:\n{}",
            thread, waited_ms, frames
        );
    }
}

/// Publish or retire the saved context used while an HDLL/native call blocks
/// its OS worker. Execution of HL code while marked blocking violates the
/// HashLink contract: the collector is allowed to scan this saved context
/// without waiting for another AIR V2 poll.
pub(crate) fn gc_set_blocking(blocking: bool) -> bool {
    if !current_mutator_registered() {
        return false;
    }
    if blocking {
        gc_safepoint();
    }
    let thread = thread_self_fast();
    let mut saved_regs = [0usize; CALLEE_SAVED_WORDS];
    spill_callee_saved(&mut saved_regs);
    let sp = ImmixAllocator::current_stack_addr().min(saved_regs.as_ptr() as usize);
    let mut world = MUTATOR_WORLD.state.lock().unwrap();
    let Some(index) = world.mutators.iter().position(|m| m.thread == thread) else {
        return false;
    };

    if blocking {
        let record = &mut world.mutators[index];
        record.blocking_depth = record.blocking_depth.saturating_add(1);
        record.stopped_sp = sp;
        record.saved_regs = saved_regs;
        MUTATOR_WORLD.changed.notify_all();
        return true;
    }
    if world.mutators[index].blocking_depth == 0 {
        return false;
    }
    world.mutators[index].blocking_depth -= 1;
    if world.mutators[index].blocking_depth != 0 {
        return true;
    }

    // A thread leaving its native blocking section while collection is in
    // progress joins the parked mutators before it may execute HL again.
    if world.stop_requested && world.collector != thread {
        world.mutators[index].stopped_sp = sp;
        world.mutators[index].saved_regs = saved_regs;
        world.mutators[index].parked = true;
        MUTATOR_WORLD.changed.notify_all();
        while world.stop_requested {
            world = MUTATOR_WORLD.changed.wait(world).unwrap();
        }
        if let Some(record) = world.mutators.iter_mut().find(|m| m.thread == thread) {
            record.parked = false;
            record.stopped_sp = 0;
        }
    } else {
        world.mutators[index].stopped_sp = 0;
    }
    true
}

struct StoppedWorld {
    snapshots: Vec<MutatorSnapshot>,
    requested: bool,
    /// Whether every mutator actually stopped. False means the attempt was
    /// abandoned, and nothing may be scanned.
    stopped: bool,
}

impl Drop for StoppedWorld {
    fn drop(&mut self) {
        if !self.requested {
            return;
        }
        let mut world = MUTATOR_WORLD.state.lock().unwrap();
        world.stop_requested = false;
        world.collector = 0;
        GC_STOP_REQUESTED.store(false, Ordering::Release);
        MUTATOR_WORLD.changed.notify_all();
    }
}

fn stop_mutator_world() -> StoppedWorld {
    let collector = thread_self_fast();
    let mut world = MUTATOR_WORLD.state.lock().unwrap();
    let needs_stop = world.mutators.iter().any(|m| m.thread != collector);
    if needs_stop {
        world.stop_requested = true;
        world.collector = collector;
        GC_STOP_ASKED_NS.store(GC_EPOCH.elapsed().as_nanos() as u64, Ordering::Relaxed);
        GC_STOP_REQUESTED.store(true, Ordering::Release);
        crate::fiber::request_fiber_poll();
        // A mutator may already be sleeping in the GC-lock slow path. Wake it
        // so it can observe the stop request and publish its stack.
        GC_LOCK.wake_for_world_stop();
        // Bounded waits rather than one open-ended one, so a slow mutator can
        // be named. Stopping the world is normally microseconds; when it is
        // not, the question is always which thread had not reached a
        // safepoint, and an untimed wait cannot answer it.
        let began = Instant::now();
        let mut reported = false;
        let mut abandoned = false;
        while world
            .mutators
            .iter()
            .any(|m| m.thread != collector && !m.parked && m.blocking_depth == 0)
        {
            // A thread inside a native call that never announced itself as
            // blocking reaches no safepoint until that call returns, and this
            // wait had no end. That is not a slow collection: it is the whole
            // VM stopped for as long as some library takes.
            //
            // Not hypothetical. HashLink's ssl.hdll performs a blocking
            // mbedtls read without calling hl_blocking, so a TLS read in a
            // game froze every other thread until the peer answered -- or
            // permanently, if it never did.
            //
            // A collector cannot scan a thread that is still running, so the
            // only safe answer is to not collect. Giving up costs a deferred
            // collection; waiting costs the program.
            if began.elapsed() >= STOP_THE_WORLD_DEADLINE {
                abandoned = true;
                break;
            }
            let (guard, timed_out) = MUTATOR_WORLD
                .changed
                .wait_timeout(world, std::time::Duration::from_millis(20))
                .unwrap();
            world = guard;
            if timed_out.timed_out() && !reported && gc_stats_enabled() {
                reported = true;
                let stragglers: Vec<String> = world
                    .mutators
                    .iter()
                    .filter(|m| m.thread != collector && !m.parked && m.blocking_depth == 0)
                    .map(|m| format!("{} {:#x}", m.role, m.thread))
                    .collect();
                eprintln!(
                    "[gc] world stop waiting {:.1}ms on {} of {} mutators: {}",
                    began.elapsed().as_secs_f64() * 1e3,
                    stragglers.len(),
                    world.mutators.len(),
                    stragglers.join(", ")
                );
            }
        }
        if reported {
            eprintln!(
                "[gc] world stopped after {:.1}ms",
                began.elapsed().as_secs_f64() * 1e3
            );
        }
        if abandoned {
            let stragglers: Vec<String> = world
                .mutators
                .iter()
                .filter(|m| m.thread != collector && !m.parked && m.blocking_depth == 0)
                .map(|m| format!("{} {:#x}", m.role, m.thread))
                .collect();
            GC_STATS.stops_abandoned.fetch_add(1, Ordering::Relaxed);
            // Once by default: a program that does this does it repeatedly,
            // and a line per collection would bury everything else.
            static SAID: std::sync::Once = std::sync::Once::new();
            let mut first = false;
            SAID.call_once(|| first = true);
            if first || gc_stats_enabled() {
                eprintln!(
                    "[gc] gave up stopping the world after {:.0}ms; {} of {} mutators \
                     never reached a safepoint ({}). Collection deferred. A native call \
                     that blocks without calling hl_blocking looks exactly like this.",
                    began.elapsed().as_secs_f64() * 1e3,
                    stragglers.len(),
                    world.mutators.len(),
                    stragglers.join(", ")
                );
            }
            return StoppedWorld {
                snapshots: Vec::new(),
                requested: needs_stop,
                stopped: false,
            };
        }
    }
    let snapshots = world
        .mutators
        .iter()
        .map(|m| MutatorSnapshot {
            thread: m.thread,
            stack_top: m.stack_top,
            stack_sp: m.stopped_sp,
            saved_regs: m.saved_regs,
            scan_ranges: match m.scan_live {
                // SAFETY: the mutator is stopped, and it owns this table for
                // as long as it is registered. A stop only lands at a
                // safepoint, never between an entry's write and the length
                // bump that publishes it.
                Some((ranges, len)) if ranges != 0 && len != 0 => unsafe {
                    let n = *(len as *const usize);
                    std::slice::from_raw_parts(ranges as *const (usize, usize), n)
                        .iter()
                        .copied()
                        .filter(|&(a, sz)| a != 0 && sz != 0)
                        .collect()
                },
                _ => m.scan_ranges.clone(),
            },
        })
        .collect();
    StoppedWorld {
        snapshots,
        requested: needs_stop,
        stopped: true,
    }
}

/// Copy the registered mutator thread handles into `out`, returning how many
/// were written.
///
/// The sampling profiler interrupts one thread, the one that started it, which
/// makes a worker invisible: a fiber worker holding up a world stop for 350ms
/// leaves no trace in a profile that never signals it. `thread_self_fast`
/// returns the same value `pthread_self` does on both supported platforms, so
/// these handles can be signalled directly.
///
/// `try_lock`, deliberately: the sampler must never block on the world lock,
/// least of all while a collection holds it. A tick that finds it contended
/// simply samples nothing.
///
/// # Safety
/// `out` must be valid for `cap` `u64` writes.
#[no_mangle]
pub unsafe extern "C" fn hlp_gc_registered_threads(out: *mut u64, cap: usize) -> usize {
    if out.is_null() || cap == 0 {
        return 0;
    }
    let Ok(world) = MUTATOR_WORLD.state.try_lock() else {
        return 0;
    };
    let n = world.mutators.len().min(cap);
    for (i, m) in world.mutators.iter().take(n).enumerate() {
        *out.add(i) = m.thread;
    }
    n
}

fn mutator_scan_range_count() -> usize {
    MUTATOR_WORLD
        .state
        .lock()
        .unwrap()
        .mutators
        .iter()
        .map(|m| m.scan_ranges.len())
        .sum()
}

fn clear_current_scan_ranges() {
    let thread = thread_self_fast();
    let mut world = MUTATOR_WORLD.state.lock().unwrap();
    if let Some(record) = world.mutators.iter_mut().find(|m| m.thread == thread) {
        record.staged_scan_ranges.clear();
    }
}

fn add_current_scan_range(start: usize, size: usize) {
    if start == 0 || size == 0 {
        return;
    }
    let thread = thread_self_fast();
    let mut world = MUTATOR_WORLD.state.lock().unwrap();
    if let Some(record) = world.mutators.iter_mut().find(|m| m.thread == thread) {
        record.staged_scan_ranges.push((start, size));
    }
}

fn publish_current_scan_ranges() {
    let thread = thread_self_fast();
    let mut world = MUTATOR_WORLD.state.lock().unwrap();
    if let Some(record) = world.mutators.iter_mut().find(|m| m.thread == thread) {
        record.scan_ranges = mem::take(&mut record.staged_scan_ranges);
    }
}

/// Replace this mutator's published scan set in one hold of the world lock.
///
/// Built in place on `scan_ranges` rather than staged and swapped. No observer
/// can read that vector without this mutex — `mark_roots` and `sweep` work
/// from snapshots cloned under it — so the intermediate state is unobservable
/// and publication is atomic either way. Staging would leave the PREVIOUS set
/// sitting in `staged_scan_ranges` afterwards, and those addresses are
/// interpreter frame register buffers that go straight back to the frame pool;
/// keeping "staged is empty after a publish" means a later stray
/// `hlp_gc_scan_roots_done` cannot republish freed memory as roots. Clearing in
/// place also retains the vector's capacity, so publishing a steady root set
/// allocates nothing.
fn set_current_scan_ranges(ranges: &[(usize, usize)]) {
    let thread = thread_self_fast();
    let mut world = MUTATOR_WORLD.state.lock().unwrap();
    if let Some(record) = world.mutators.iter_mut().find(|m| m.thread == thread) {
        record.scan_ranges.clear();
        record
            .scan_ranges
            .extend(ranges.iter().copied().filter(|&(a, s)| a != 0 && s != 0));
    }
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
    if aligned <= TLAB_MAX_OBJ && tlab_enabled() {
        // One TLS lookup for the whole sequence -- see `TLAB`.
        enum Step {
            Bumped(usize),
            Refill,
            Unregistered,
        }
        let step = TLAB.with(|t| {
            if !t.registered.get() {
                return Step::Unregistered;
            }
            let cur = t.cur.get();
            if cur != 0 {
                let mut p = cur;
                if (p & (LINE_SIZE - 1)) + aligned > LINE_SIZE {
                    p = (p + LINE_SIZE - 1) & !(LINE_SIZE - 1);
                }
                let np = p + aligned;
                if np <= t.limit.get() {
                    t.cur.set(np);
                    return Step::Bumped(p);
                }
            }
            Step::Refill
        });
        match step {
            // Pre-zeroed at refill.
            Step::Bumped(p) => return Some(unsafe { NonNull::new_unchecked(p as *mut u8) }),
            Step::Refill => return tlab_refill_then_alloc(aligned),
            Step::Unregistered => {}
        }
    }
    gc_locked_init().allocate(size)
}

/// Region exhausted (or never opened): take the lock, run the ordinary
/// trigger logic, carve a fresh block, zero it once, and serve the pending
/// allocation from its head.
/// Install `block` as this thread's bump region, releasing the previous one.
///
/// The set of in-use regions lives on the heap because `sweep` consults it
/// under the same lock this runs under; only the cursor is thread-local.
/// Releasing the old block simply makes it ordinary again — its live objects
/// are marked conservatively like any others; it just stops being exempt
/// from reclamation.
fn adopt_tlab_region(gc: &mut ImmixAllocator, block: usize, cur: usize, limit: usize) {
    gc.heap.tlab_blocks.insert(thread_self_fast(), block);
    TLAB.with(|t| {
        t.block.set(block);
        t.cur.set(cur);
        t.limit.set(limit);
    });
}

/// Give up this thread's bump region entirely (thread exit).
fn release_tlab_region(gc: &mut ImmixAllocator) {
    gc.heap.tlab_blocks.remove(&thread_self_fast());
    TLAB.with(|t| {
        t.block.set(usize::MAX);
        t.cur.set(0);
        t.limit.set(0);
    });
}

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
    set_collect_origin(2);
    gc.maybe_collect_at_safepoint();
    // Recycled lines first: a span in a block the sweep kept costs nothing to
    // acquire and leaves the free list for allocations that need a whole
    // block. Spans too small for the pending object are dropped rather than
    // re-queued -- they are 128-byte crumbs and the list is rebuilt each
    // sweep.
    let want_lines = aligned.div_ceil(LINE_SIZE).max(1);
    if recycle_lines() {
        while let Some((rblock, start, len)) = gc.heap.recycle_spans.pop() {
            if len < want_lines {
                continue;
            }
            let lo = rblock + start * LINE_SIZE;
            let span_bytes = len * LINE_SIZE;
            let base = unsafe { gc.heap.memory.as_mut_ptr().add(lo) };
            // Zeroed for the same reason a fresh block is: every caller of
            // this path is promised zeroed memory.
            unsafe { std::ptr::write_bytes(base, 0, span_bytes) };
            gc.heap.bytes_since_gc += span_bytes;
            gc.heap.alloc_count += 1;
            GC_STATS
                .bytes_allocated
                .fetch_add(span_bytes as u64, Ordering::Relaxed);
            GC_STATS
                .lines_recycled
                .fetch_add(len as u64, Ordering::Relaxed);
            adopt_tlab_region(
                &mut gc,
                rblock,
                base as usize + aligned,
                base as usize + span_bytes,
            );
            return Some(unsafe { NonNull::new_unchecked(base) });
        }
    }

    let block = match gc.acquire_free_block() {
        Some(b) => b,
        None => {
            set_collect_origin(4);
            gc.collect_garbage();
            gc.acquire_free_block()?
        }
    };
    let base = unsafe { gc.heap.memory.as_mut_ptr().add(block) };
    unsafe { std::ptr::write_bytes(base, 0, BLOCK_SIZE) };
    // Coarse trigger accounting: the whole region counts when it is carved,
    // not per object. Slightly early triggers, never late ones.
    gc.heap.bytes_since_gc += BLOCK_SIZE;
    gc.heap.alloc_count += 1;
    GC_STATS
        .bytes_allocated
        .fetch_add(BLOCK_SIZE as u64, Ordering::Relaxed);
    adopt_tlab_region(
        &mut gc,
        block,
        base as usize + aligned,
        base as usize + BLOCK_SIZE,
    );
    Some(unsafe { NonNull::new_unchecked(base) })
}

/// Trace flags, read once. `std::env::var` per allocation took the macOS
/// process-wide getenv lock on the hottest path in the program — the exact
/// mistake the opcode-dispatch env flags already document.
fn trace_alloc() -> bool {
    static V: OnceLock<bool> = OnceLock::new();
    *V.get_or_init(|| std::env::var("ASH_GC_TRACE_ALLOC").is_ok())
}
fn trace_map() -> bool {
    static V: OnceLock<bool> = OnceLock::new();
    *V.get_or_init(|| std::env::var("ASH_GC_TRACE_MAP").is_ok())
}

/// Reuse the unmarked lines of a block a sweep kept, instead of retaining the
/// whole block because one line in it is live.
///
/// Off by default, and the reason is the marker rather than the allocator.
/// Reusing a line overwrites what sits in it, so it is sound only if the trace
/// marks EVERY live object. Block-level retention has been covering for that:
/// an object whose root the scanner missed survives anyway if it shares a
/// block with a marked one. The cover is already partial -- a wholly-unmarked
/// block is freed today -- but this removes what is left of it, so a missed
/// root becomes a corrupted object rather than a retained one. Prove the
/// corpus clean under `ASH_GC_STRESS` before making it the default.
/// How much new allocation to allow per unit of live data before collecting.
///
/// The collection count is the dominant GC cost on an allocation-heavy
/// program, and it is this factor -- not the ceiling -- that sets it: with a
/// 17MB live set, `live*2` collects every 34MB, so binary_trees pays 36
/// collections for 902MB and never reaches the 64MB ceiling at all.
///
/// Proportional rather than fixed, so a program with a small live set still
/// collects at a small interval -- deltablue's RSS does not move at any of
/// these values, only binary_trees' does.
///
/// 4 rather than higher because the trade is throughput against pause: on
/// binary_trees (NUC, release) 2 gives 452ms execute over 36 collections and
/// 122MB, 4 gives 418ms over 19 and 165MB, 8 gives 400ms over 10 and 189MB.
/// The knee is around 6, but a bigger interval means more garbage per
/// collection, and a frame budget cares about the longest pause rather than
/// their sum. `ASH_GC_GROWTH` overrides it.
fn growth_factor() -> usize {
    static V: OnceLock<usize> = OnceLock::new();
    *V.get_or_init(|| {
        std::env::var("ASH_GC_GROWTH")
            .ok()
            .and_then(|v| v.parse::<usize>().ok())
            .filter(|n| *n >= 1)
            .unwrap_or(4)
    })
}

/// Reuse the unmarked lines inside blocks a sweep kept. `ASH_GC_RECYCLE=0`
/// falls back to retaining those blocks whole.
///
/// On by default because block-granularity reuse fragments without bound in a
/// long-running program: measured on a game, occupancy of the retained blocks
/// fell 54% -> 41% -> 23% -> 17% while the marked set stayed flat near 195MB,
/// so 1132MB was held to store 195MB of live data and marking paid to walk
/// all of it -- pauses reached 362ms and were still growing. Recycling holds
/// occupancy near 42% and retention near 566MB on the same workload, with a
/// worst pause of 195ms.
///
/// The reason it was off: retaining a block whole hides a live object the
/// conservative scanner failed to mark, where reusing its line would zero it.
/// That risk is real, so this is gated on evidence rather than argument --
/// the suite passes under `ASH_GC_STRESS=1`, which collects on every
/// allocation, and a full game session ran clean.
fn recycle_lines() -> bool {
    static V: OnceLock<bool> = OnceLock::new();
    *V.get_or_init(|| !matches!(std::env::var("ASH_GC_RECYCLE").as_deref(), Ok("0")))
}

/// ASH_GC_HANDBACK=0 stops returning free blocks to the OS. The pages stay
/// mapped and dirty, so RSS holds steady instead of falling -- the point is to
/// attribute a growing process footprint, which on macOS is a kernel ledger
/// and not a measurement of how much memory is really held.
fn handback_enabled() -> bool {
    static V: OnceLock<bool> = OnceLock::new();
    *V.get_or_init(|| !matches!(std::env::var("ASH_GC_HANDBACK").as_deref(), Ok("0")))
}

/// ASH_GC_OCCUPANCY=1: per-collection report of how full the RETAINED blocks
/// are. A 32KB block survives on one marked line out of 256; with
/// conservative marking one integer that looks like a pointer is enough. Low
/// mean occupancy means the "live" figure is mostly retained garbage, and
/// marking cost is being paid for memory that is dead. Watch the trend rather
/// than one reading: a decaying series is fragmentation, and it is what
/// `recycle_lines` exists to arrest.
fn occupancy_stats() -> bool {
    static V: OnceLock<bool> = OnceLock::new();
    *V.get_or_init(|| std::env::var("ASH_GC_OCCUPANCY").is_ok())
}

/// Diagnostic: ASH_GC_NO_RECLAIM=1 makes sweep retain every block (marks
/// still reset; nothing returns to the free list). Splits "collector
/// reclaims a live block" from every non-reclamation corruption source.
fn no_reclaim() -> bool {
    static V: OnceLock<bool> = OnceLock::new();
    *V.get_or_init(|| matches!(std::env::var("ASH_GC_NO_RECLAIM").as_deref(), Ok("1")))
}

/// ASH_GC_SWEEP_AUDIT=1: snapshot marks before a sweep and check, for every
/// block about to be freed, that no root still points into it.
///
/// Cached like every other flag here because the freed-block branch consults
/// it once per block: a sweep that frees 8,300 blocks took the process-wide
/// getenv lock 8,300 times, inside the stop-the-world.
fn sweep_audit() -> bool {
    static V: OnceLock<bool> = OnceLock::new();
    *V.get_or_init(|| std::env::var("ASH_GC_SWEEP_AUDIT").is_ok())
}

fn trace_freed() -> bool {
    static V: OnceLock<bool> = OnceLock::new();
    *V.get_or_init(|| std::env::var("ASH_GC_TRACE_FREED").is_ok())
}
/// ASH_GC_POISON=1: fill every swept (freed) block with 0xA5 bytes. A
/// mutator that reads a prematurely-freed object then sees an unmistakable
/// pattern (0xA5A5... pointers / lengths) instead of plausible reused data,
/// which converts "mysterious corruption later" into "poison read here".
fn poison_freed() -> bool {
    static V: OnceLock<bool> = OnceLock::new();
    *V.get_or_init(|| std::env::var("ASH_GC_POISON").is_ok())
}
/// ASH_GC_QUARANTINE=1: freed blocks are never returned to the free list
/// (and are poisoned), so no reuse can ever paper over a premature free —
/// every read of a freed object hits poison. Diagnosis only: the heap only
/// grows, so pair it with a large ASH_GC_HEAP_MB.
fn quarantine_freed() -> bool {
    static V: OnceLock<bool> = OnceLock::new();
    *V.get_or_init(|| std::env::var("ASH_GC_QUARANTINE").is_ok())
}
/// Why the current collection was started — set by every collect_garbage
/// caller immediately before the call, printed by the per-collection trace
/// lines. Single mutator + GC lock make a plain static sound here.
static COLLECT_ORIGIN: std::sync::atomic::AtomicU8 = std::sync::atomic::AtomicU8::new(0);
const ORIGIN_NAMES: [&str; 7] = [
    "?",
    "snapshot-done",    // scan_roots_done honoring a deferred trigger
    "tlab-safepoint",   // tlab_refill_then_alloc's maybe_collect_at_safepoint
    "hard-pressure",    // maybe_collect past the 4x deferral bound
    "exhaustion",       // allocate's no-free-block backstop
    "large-exhaustion", // allocate_large fallback
    "explicit",         // Gc.major / hlp_gc_major
];
fn set_collect_origin(o: u8) {
    COLLECT_ORIGIN.store(o, Ordering::Relaxed);
}

fn env_usize(name: &str) -> Option<usize> {
    std::env::var(name).ok().and_then(|v| v.trim().parse().ok())
}

/// Maximum heap reservation in bytes.
///
/// `ASH_GC_HEAP_MB` overrides; otherwise a share of usable RAM, so the same
/// binary runs a game on a workstation and a script in a small container.
/// A fixed default cannot do both: a real 3D scene carries a live
/// set near 1GB, and a cap below it does not degrade — allocation fails and
/// the caller has nowhere to go.
/// Report an allocation that could not be satisfied, and stop.
///
/// Every caller is reached through an `extern "C"` boundary, where a panic
/// cannot unwind: it aborts, and the trace names whichever symbol the unwinder
/// found first. rayzor-blade#1 reported that as a wall of `ustrdup` and a
/// second panic about not being able to unwind, which says nothing about the
/// heap. Say what was being allocated and how full the heap was instead.
#[cold]
#[inline(never)]
pub fn out_of_memory(what: &str) -> ! {
    const MB: usize = 1024 * 1024;
    let live = GC_STATS.live_blocks.load(Ordering::Relaxed) as usize * BLOCK_SIZE;
    let external = GC_STATS.external_bytes.load(Ordering::Relaxed) as usize;
    let collections = GC_STATS.collections.load(Ordering::Relaxed);
    eprintln!(
        "[ash] out of memory allocating {what}\n\
         [ash]   heap cap {} MB, live {} MB, external {} MB, after {collections} collection(s)\n\
         [ash]   ASH_GC_HEAP_MB raises the cap. A heap that fills again at a\n\
         [ash]   higher cap is a leak rather than a heap that is too small.",
        heap_max_bytes() / MB,
        live / MB,
        external / MB,
    );
    // Not a panic: see above.
    std::process::exit(1);
}

fn heap_max_bytes() -> usize {
    static V: OnceLock<usize> = OnceLock::new();
    *V.get_or_init(|| {
        let bytes = match env_usize("ASH_GC_HEAP_MB") {
            Some(mb) => mb.max(32) * 1024 * 1024,
            None => (usable_ram_bytes() / HEAP_MAX_SHARE).clamp(HEAP_MAX_FLOOR, HEAP_MAX_CEILING),
        };
        (bytes / BLOCK_SIZE) * BLOCK_SIZE
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
    /// Lines served from a kept block's free spans rather than a fresh block.
    lines_recycled: AtomicU64,
    bytes_allocated: AtomicU64,
    external_bytes: AtomicU64,
    live_blocks: AtomicU64,
    pause_ns_total: AtomicU64,
    pause_ns_max: AtomicU64,
    /// Collections given up because a mutator never reached a safepoint.
    stops_abandoned: AtomicU64,
}

static GC_STATS: GcStatCounters = GcStatCounters {
    collections: AtomicU64::new(0),
    blocks_reclaimed: AtomicU64::new(0),
    lines_recycled: AtomicU64::new(0),
    bytes_allocated: AtomicU64::new(0),
    external_bytes: AtomicU64::new(0),
    live_blocks: AtomicU64::new(0),
    pause_ns_total: AtomicU64::new(0),
    pause_ns_max: AtomicU64::new(0),
    stops_abandoned: AtomicU64::new(0),
};

// ── Collection switch (`Gc.enable`) ─────────────────────────────────────────

/// Upstream's `gc_is_active`: consulted by the automatic trigger only.
/// Explicit collections and the heap-exhaustion backstop ignore it, so
/// disabling never turns an allocation into a hard failure. An atomic rather
/// than a heap field because callers reach `hlp_gc_enable` from anywhere,
/// including from under the GC lock.
static GC_ENABLED: AtomicBool = AtomicBool::new(true);

// ── Collector flags (`Gc.flags`) ────────────────────────────────────────────

/// Bit values of `hl.Gc.GcFlag`, fixed by the Haxe enum's ordinals.
const GC_FLAG_PROFILE: i32 = 1;

/// Upstream's `gc_flags`. Programs read-modify-write it (`flags.set(..)` is a
/// get, an or, and a set), so the whole word round-trips even where a bit
/// names something ash's collector does not have.
static GC_FLAGS: AtomicI32 = AtomicI32::new(0);

/// True when `flag` is currently set. Cheap enough for the allocation path.
#[inline]
fn gc_flag(flag: i32) -> bool {
    GC_FLAGS.load(Ordering::Relaxed) & flag != 0
}

/// Pressure at which a disabled collector collects anyway.
///
/// `Gc.enable(false)` with no matching re-enable is a real pattern — a load
/// screen that throws, a `finally` that never runs — and honouring it to the
/// end of the heap converts a pause the program asked to defer into an
/// out-of-memory abort. Four times the adaptive ceiling is far past any
/// legitimate no-collect window while still leaving most of the default
/// reservation in hand.
const TRIGGER_CEILING_SHARE: usize = 32;

/// Usable memory for this process, in bytes.
///
/// The cgroup limit is the real bound whenever there is one: CI runs in a
/// container with a few GB, not on the host's total, and sizing off the host
/// there is how a runtime gets OOM-killed. Falls back to physical memory, and
/// to a modest guess if neither can be read.
fn usable_ram_bytes() -> usize {
    const FALLBACK: usize = 2 * 1024 * 1024 * 1024;
    #[cfg(target_os = "linux")]
    {
        // The limit that binds is the one on THIS process's cgroup, not the
        // root's -- reading `/sys/fs/cgroup/memory.max` reports the root's
        // "max" and misses every container. `/proc/self/cgroup` names the
        // path; limits are hierarchical, so the smallest along it wins.
        let mut limit = usize::MAX;
        if let Ok(selfcg) = std::fs::read_to_string("/proc/self/cgroup") {
            for line in selfcg.lines() {
                // v2: "0::/path". v1: "N:controllers:/path".
                let rel = match line.splitn(3, ':').nth(2) {
                    Some(r) => r.trim_start_matches('/'),
                    None => continue,
                };
                let mut dir = std::path::PathBuf::from("/sys/fs/cgroup");
                let mut probe = vec![dir.clone()];
                for seg in rel.split('/').filter(|s| !s.is_empty()) {
                    dir = dir.join(seg);
                    probe.push(dir.clone());
                }
                for d in probe {
                    for name in ["memory.max", "memory/memory.limit_in_bytes"] {
                        if let Ok(t) = std::fs::read_to_string(d.join(name)) {
                            if let Ok(n) = t.trim().parse::<usize>() {
                                // v1 uses a sentinel near usize::MAX for
                                // "no limit"; v2 writes the word "max",
                                // which fails the parse and is skipped.
                                if n > 0 && n < (1 << 60) {
                                    limit = limit.min(n);
                                }
                            }
                        }
                    }
                }
            }
        }
        if limit != usize::MAX {
            return limit;
        }
        if let Ok(t) = std::fs::read_to_string("/proc/meminfo") {
            for line in t.lines() {
                if let Some(rest) = line.strip_prefix("MemTotal:") {
                    if let Some(kb) = rest.split_whitespace().next() {
                        if let Ok(kb) = kb.parse::<usize>() {
                            return kb * 1024;
                        }
                    }
                }
            }
        }
        FALLBACK
    }
    #[cfg(target_os = "macos")]
    {
        let mut out: u64 = 0;
        let mut len = std::mem::size_of::<u64>();
        let name = c"hw.memsize";
        let rc = unsafe {
            libc::sysctlbyname(
                name.as_ptr(),
                &mut out as *mut u64 as *mut c_void,
                &mut len,
                std::ptr::null_mut(),
                0,
            )
        };
        if rc == 0 && out > 0 {
            out as usize
        } else {
            FALLBACK
        }
    }
    // Without this branch Windows took FALLBACK, so the cap was
    // 2GB/HEAP_MAX_SHARE -- clamped to HEAP_MAX_FLOOR, 512MB -- on a machine
    // with any amount of RAM. A long-running program then exhausted a heap
    // sized for a machine it was not running on, and the failure surfaced as
    // an allocation panic inside an `extern "C"` frame that cannot unwind.
    #[cfg(target_os = "windows")]
    {
        use windows_sys::Win32::System::SystemInformation::{
            GlobalMemoryStatusEx, MEMORYSTATUSEX,
        };
        let mut status: MEMORYSTATUSEX = unsafe { std::mem::zeroed() };
        status.dwLength = std::mem::size_of::<MEMORYSTATUSEX>() as u32;
        if unsafe { GlobalMemoryStatusEx(&mut status) } != 0 && status.ullTotalPhys > 0 {
            return status.ullTotalPhys as usize;
        }
        FALLBACK
    }
    #[cfg(not(any(target_os = "linux", target_os = "macos", target_os = "windows")))]
    {
        FALLBACK
    }
}

/// How much new allocation the collector allows between collections, at most.
///
/// Derived from the machine rather than fixed: 64MB is a different proposition
/// on a 6GB CI container than on a 64GB workstation, and the collection COUNT
/// is the dominant GC cost on an allocation-heavy program. `ASH_GC_TRIGGER_MB`
/// still overrides it outright.
///
/// This bounds FIXED headroom only. A ceiling below the live set would make
/// the collector fire at a fraction of what is live, collecting continuously
/// and reclaiming progressively less — a 3D scene load hit that at
/// roughly one collection per second. `collect_garbage` therefore raises the
/// effective ceiling to the live set when the live set is larger, which keeps
/// peak near 2x live. Scaling this constant with the whole heap instead did
/// fix the pauses, but let a ~1GB live set accumulate 2GB of garbage.
fn trigger_ceiling_bytes() -> usize {
    static V: OnceLock<usize> = OnceLock::new();
    *V.get_or_init(|| {
        (usable_ram_bytes() / TRIGGER_CEILING_SHARE).clamp(TRIGGER_CEILING_MIN, TRIGGER_CEILING_MAX)
    })
}

/// Never defer a collection past this much pressure, whatever policy asked to.
///
/// Deferral is only ever an optimisation; running out of heap is not
/// recoverable, because the allocator's callers are `extern "C"` and a failed
/// allocation aborts rather than unwinding. Every "collect later" path is
/// therefore capped below the heap so the collection still happens while it
/// can do some good.
fn max_deferred_pressure() -> usize {
    heap_max_bytes() / 2
}

/// A GC disabled by the embedder still collects under this much pressure.
fn gc_disabled_max_pressure() -> usize {
    trigger_ceiling_bytes()
        .saturating_mul(4)
        .min(max_deferred_pressure())
}

/// Is a *triggered* collection allowed to run right now? `pressure` is the
/// byte total the trigger fired on.
fn triggered_collection_allowed(pressure: usize) -> bool {
    GC_ENABLED.load(Ordering::Relaxed) || pressure >= gc_disabled_max_pressure()
}

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
    eprintln!(
        "[gc] sizing:           ram {} / ceiling {} / growth x{}",
        fmt_mb(usable_ram_bytes() as u64),
        fmt_mb(trigger_ceiling_bytes() as u64),
        growth_factor()
    );
    eprintln!("[gc] collections:      {}", n);
    eprintln!(
        "[gc] blocks reclaimed: {} ({})",
        freed,
        fmt_mb(freed * BLOCK_SIZE as u64)
    );
    let recycled = GC_STATS.lines_recycled.load(Ordering::Relaxed);
    if recycled > 0 {
        eprintln!(
            "[gc] lines recycled:   {} ({})",
            recycled,
            fmt_mb(recycled * LINE_SIZE as u64)
        );
    }
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

/// Emit the report the `atexit` handler would have, for a caller that leaves
/// without running them. A no-op unless `ASH_GC_STATS` asked for it, so it is
/// safe to call unconditionally on the way out.
pub fn print_stats_if_enabled() {
    if gc_stats_enabled() {
        print_gc_stats_report();
    }
}

// ── macOS return-to-OS hooks ────────────────────────────────────────────────

#[cfg(target_os = "macos")]
extern "C" {
    /// Asks all malloc zones to release free pages back to the OS
    /// (forces MADV_FREE_REUSABLE internally — wren_lift gc.rs:1493-1515).
    fn malloc_zone_pressure_relief(zone: *mut c_void, goal: usize) -> usize;
}

/// Demand-committed heap reservation: anonymous private mmap (Windows: one
/// VirtualAlloc reservation, which is demand-zeroed the same way). Pages
/// become resident only on first touch, and fully-free blocks are returned
/// via madvise — RSS tracks live data, not configured capacity (wren_lift's
/// nursery idiom, gc.rs:386-391, plus wlift_alloc::pressure_release).
struct HeapMemory {
    base: *mut u8,
    len: usize,
}

impl HeapMemory {
    #[cfg(unix)]
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

    /// MEM_RESERVE|MEM_COMMIT is the demand-committed analogue: committed
    /// pages are demand-zeroed, so none is resident until the allocator
    /// touches it. The one behavioural difference from mmap is accounting —
    /// Windows charges the whole reservation against the system commit limit
    /// up front, so the 512 MB default heap needs 512 MB of RAM+pagefile
    /// headroom (not of RAM) to start. Reserving without committing would
    /// avoid that, but every block hand-out would then need its own
    /// MEM_COMMIT call, and the demand-paging this design relies on gives
    /// the same RSS curve without one.
    #[cfg(windows)]
    fn new(len: usize) -> Self {
        let ptr = unsafe {
            VirtualAlloc(
                std::ptr::null(),
                len,
                MEM_RESERVE | MEM_COMMIT,
                PAGE_READWRITE,
            )
        };
        assert!(!ptr.is_null(), "GC heap reservation failed ({} bytes)", len);
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

impl HeapMemory {
    /// The heap on a target whose memory is one linear address space.
    ///
    /// There is no `mmap` to reserve address space and no way to hand pages
    /// back, so this asks the allocator for the whole region up front and
    /// keeps it. That is the bounded, non-reclaiming heap the wasm plan calls
    /// for: it separates the ABI and startup work from root discovery, and it
    /// is honest about being a first step rather than a collector.
    #[cfg(not(any(unix, windows)))]
    fn new(len: usize) -> Self {
        // Block-aligned, and that alignment is load-bearing rather than
        // tidiness. The bump allocator finds a line boundary from the
        // ABSOLUTE address (`p & (LINE_SIZE - 1)`) while marking and sweeping
        // index lines from the OFFSET into the heap. Those two agree only if
        // the base is a multiple of LINE_SIZE. `mmap` returns page-aligned
        // memory and 4096 is a multiple of 128, so every platform with a real
        // mmap got this by luck; asking the allocator for 16 did not, and the
        // disagreement is `base % LINE_SIZE` bytes.
        //
        // What that costs: an object the bump path believes starts a line
        // actually straddles two by the sweep's reckoning, the sweep reclaims
        // the half nobody marked, and a live object loses its tail. No crash,
        // no diagnostic -- an array that reports the wrong length.
        let layout = std::alloc::Layout::from_size_align(len, BLOCK_SIZE).expect("heap layout");
        // Zeroed, because the collector reads a block's header before
        // anything has written one.
        let ptr = unsafe { std::alloc::alloc_zeroed(layout) };
        assert!(!ptr.is_null(), "could not reserve {len} bytes for the heap");
        Self { base: ptr, len }
    }
}

impl Drop for HeapMemory {
    fn drop(&mut self) {
        #[cfg(unix)]
        unsafe {
            libc::munmap(self.base as *mut c_void, self.len);
        }
        // MEM_RELEASE releases the entire original reservation and therefore
        // demands a zero size and the exact base VirtualAlloc returned;
        // passing self.len fails with ERROR_INVALID_PARAMETER and leaks the
        // whole heap.
        #[cfg(windows)]
        unsafe {
            VirtualFree(self.base as *mut c_void, 0, MEM_RELEASE);
        }
        #[cfg(not(any(unix, windows)))]
        unsafe {
            let layout = std::alloc::Layout::from_size_align_unchecked(self.len, 16);
            std::alloc::dealloc(self.base, layout);
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

/// Unique, never-zero token for the current thread.
///
/// History worth keeping, because the comment here was wrong twice. A
/// `thread_local!` token cost a `_tlv_get_addr` call with a lazy-init
/// branch on every lock operation — 11.6% of an allocation-bound profile.
/// It was replaced by `libc::pthread_self` and this comment then claimed
/// that was "a register read". It is not: on Mach-O it goes through a
/// lazy-bind stub (adrp + GOT load + indirect branch + the libSystem body),
/// twice per lock hold, and the disassembly of `acquire`/`release` shows
/// the `bl` plainly.
///
/// `thread_self_fast` is the one that really is a register read — TPIDRRO_EL0
/// on macOS/aarch64, TPIDR_EL0 on linux/aarch64, fs:0x10 on linux/x86_64,
/// GetCurrentThreadId on Windows — and it is what this should have been
/// using all along. Its value is never 0 on any of those, so the "0 means
/// free" encoding in `owner` survives.
#[inline(always)]
fn gc_thread_token() -> u64 {
    thread_self_fast()
}

#[allow(dead_code)]
fn gc_thread_token_unused() -> u64 {
    #[cfg(not(any(unix, windows)))]
    {
        1
    }
    #[cfg(unix)]
    unsafe {
        libc::pthread_self() as u64
    }
    #[cfg(windows)]
    unsafe {
        GetCurrentThreadId() as u64
    }
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
        // A collector can own this lock while it waits for our stack. Park
        // before attempting the CAS; otherwise both sides wait forever.
        gc_safepoint();
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
            if GC_STOP_REQUESTED.load(Ordering::Acquire) {
                drop(g);
                gc_safepoint();
                g = self.inner.lock().unwrap();
            } else {
                g = self.cond.wait(g).unwrap();
            }
        }
        self.waiters.fetch_sub(1, Ordering::Relaxed);
        self.depth.store(1, Ordering::Relaxed);
        drop(g);
    }

    fn release(&self) {
        use std::sync::atomic::Ordering;
        // The token is needed ONLY by the assert below, and an opaque extern
        // call cannot be dead-code-eliminated in release — the shipped dylib
        // called it and then never read the result. Gate it with the assert
        // it serves.
        #[cfg(debug_assertions)]
        {
            let me = gc_thread_token();
            debug_assert_eq!(
                self.owner.load(Ordering::Relaxed),
                me,
                "GC lock released by non-owner thread"
            );
        }
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

    fn wake_for_world_stop(&self) {
        let _guard = self.inner.lock().unwrap();
        self.cond.notify_all();
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
    let gc =
        unsafe { (*(&raw mut GC)).get_mut().expect("GC not initialized") as *mut ImmixAllocator };
    GcRef { gc, _guard: guard }
}

/// Acquire the GC lock, initializing the singleton if needed.
pub(crate) fn gc_locked_init() -> GcRef {
    let guard = gc_guard();
    let gc =
        unsafe { (*(&raw mut GC)).get_mut_or_init(ImmixAllocator::new) as *mut ImmixAllocator };
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
    /// Runs of unmarked lines inside blocks a sweep kept, as
    /// `(block_addr, first_line, line_count)`. The recycling half of Immix: a
    /// block with one live line out of 256 hands the other 255 back instead
    /// of being retained whole. Filled only when `ASH_GC_RECYCLE=1`.
    recycle_spans: Vec<(usize, usize, usize)>,
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
    #[cfg_attr(not(target_os = "macos"), allow(dead_code))] // macOS-only mechanism
    last_pressure_relief: Instant,
    /// Blocks currently madvised MADV_FREE_REUSABLE; must be MADV_FREE_REUSE'd
    /// before reuse so live data can't be discarded under memory pressure.
    reusable_blocks: HashSet<usize>,
    /// The block the mutator's bump region currently lives in. `sweep` never
    /// frees it: the cursor points into it, and the youngest objects there
    /// may be live with their only references in mutator registers.
    /// Which block each thread is bumping through. `sweep` never reclaims one:
    /// its owner is still allocating inside it.
    ///
    /// Keyed by THREAD rather than held as a set of blocks, because the
    /// recycled-span path can hand two threads separate spans of the SAME
    /// block. With a plain set, whichever refilled first would remove the
    /// block from it and expose the other thread's live region to the sweep.
    tlab_blocks: HashMap<u64, usize>,
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
#[derive(Debug)]
struct Block {
    /// One claim bit per line, packed 64 to a word. Atomic so the mark phase
    /// can run on several threads: marking is the bulk of the pause and is
    /// pointer-chasing over the whole live set, which is latency-bound rather
    /// than compute-bound. Relaxed ordering throughout — the world is stopped,
    /// so the only thing these order is the marker against itself, and a
    /// line's claim is established by the fetch_or alone.
    ///
    /// A bool per line cost 256 BYTES per block; at the 4GiB reservation's
    /// 131072 blocks that is 33.5MB of side table that the mark phase walks in
    /// random order alongside the live set itself. Packed, the same 256 claims
    /// occupy 32 bytes -- half a cache line -- and the table is 4.2MB. Mark was
    /// measured at 91% of an in-play pause and scaled with the live set while
    /// its per-line WORK stayed flat, which is the signature of a phase bound
    /// by memory rather than by instructions.
    mark_bits: [AtomicU64; MARK_WORDS],
    /// True while any multi-line allocation span is recorded in this block.
    /// The marker's walk-back only exists to find span starts; a block that
    /// never held one (every TLAB churn block) marks in O(1) instead of
    /// walking a sea of zero `alloc_sizes` entries — that walk was 76% of
    /// mandelbrot once NaN-box decoding multiplied candidate hits.
    has_span: bool,
    /// Set by the marker the first time any line in this block is claimed.
    /// Sweep reads it to skip the per-line scan of a block that nothing
    /// reached: its bits are already clear from the previous sweep, so there
    /// is nothing to read and nothing to reset. Most swept blocks are empty --
    /// 16228 of 27000 in one measured collection -- and each cost 256 byte reads
    /// to discover that.
    any_marked: AtomicBool,
}

/// Claim a line for the marker. Returns true for the thread that set it, so a
/// line is pushed onto exactly one worklist however many threads race for it.
#[inline(always)]
fn claim_line(block: &Block, line_idx: usize) -> bool {
    // Load first. A read-modify-write on every line costs about a third of the
    // mark phase, and by far the commonest case is a line that is already
    // marked — the plain load settles those without one. The swap then decides
    // the race for the few that are genuinely unclaimed.
    let word = &block.mark_bits[line_idx >> 6];
    let bit = 1u64 << (line_idx & 63);
    if word.load(Ordering::Relaxed) & bit != 0 {
        return false;
    }
    if word.fetch_or(bit, Ordering::Relaxed) & bit != 0 {
        return false;
    }
    // Only on a successful claim, and only when not already set, so the store
    // stays off the path for every line after a block's first.
    if !block.any_marked.load(Ordering::Relaxed) {
        block.any_marked.store(true, Ordering::Relaxed);
    }
    true
}

#[inline]
fn clear_marks(block: &Block) {
    for word in &block.mark_bits {
        word.store(0, Ordering::Relaxed);
    }
}

impl Block {
    #[inline(always)]
    fn is_marked(&self, line_idx: usize) -> bool {
        self.mark_bits[line_idx >> 6].load(Ordering::Relaxed) & (1u64 << (line_idx & 63)) != 0
    }

    /// Set a line's bit without reporting who won: for callers that mark a
    /// line they already know is theirs. `claim_line` is the racing form.
    #[inline(always)]
    fn set_mark(&self, line_idx: usize) {
        self.mark_bits[line_idx >> 6].fetch_or(1u64 << (line_idx & 63), Ordering::Relaxed);
    }

    #[inline]
    fn marked_line_count(&self) -> usize {
        self.mark_bits
            .iter()
            .map(|w| w.load(Ordering::Relaxed).count_ones() as usize)
            .sum()
    }
}

/// Plain copy of a block's marks, for the sweep audit.
fn snapshot_marks(block: &Block) -> [bool; LINES_PER_BLOCK] {
    std::array::from_fn(|i| block.is_marked(i))
}

struct RootSet {
    globals: Vec<*mut hl::vdynamic>,
    stack_roots: Vec<*mut hl::vdynamic>,
    persistent_roots: HashSet<*mut hl::vdynamic>,
    /// Addresses of POINTER SLOTS a native library asked us to keep live,
    /// via `hl_add_root`. Not objects -- the slot is re-read at every
    /// collection, so a library may overwrite it and the new value is rooted
    /// from that moment without telling us again.
    ///
    /// This is upstream's contract, not an ash invention: HashLink's
    /// `gc_roots` is a `void***` and its mark phase does `void *p =
    /// *gc_roots[i]`. Every hdll is written to it -- `hl_add_root(&h->data)`
    /// in uv, `hl_add_root(&on_dx_error)` in directx -- so a slot address is
    /// what actually arrives here, and it is virtually never inside our heap.
    root_slots: HashSet<usize>,
}

pub struct ImmixAllocator {
    heap: ImmixHeap,
    blocks: Vec<Block>,
    roots: Rc<RefCell<RootSet>>,
    pub(crate) current_exception: Option<Box<HLException>>,
    pub(crate) exception_handler:
        Option<Box<dyn Fn(&mut HLException) -> Result<*mut vdynamic, VDynamicException>>>,
    globals_range: Option<(*const *mut c_void, usize)>,
    /// Registered fiber stacks for conservative scanning. Each OS-thread
    /// mutator owns one id-0 main-stack descriptor; nonzero fiber ids are
    /// process-unique.
    fiber_stacks: Vec<FiberStackInfo>,
}

#[derive(Clone, Copy)]
pub(crate) struct FiberStackInfo {
    pub thread: u64,
    pub id: u32,
    pub base: usize,
    pub size: usize,
    /// SP recorded at the stack's last switch-out; 0 = never suspended.
    pub saved_sp: usize,
}

impl Default for ImmixAllocator {
    fn default() -> Self {
        Self::new()
    }
}

/// The span walk from [`ImmixAllocator::mark_allocation_at_line`], over shared
/// borrows so the mark phase can run it from several threads. Everything it
/// reads — `alloc_sizes`, `has_span`, the block table — is immutable for the
/// duration of a collection; the only mutation is the claim bit.
fn mark_allocation_shared(
    blocks: &[Block],
    alloc_sizes: &[u32],
    line: usize,
    out: &mut Vec<(usize, usize)>,
) {
    let mut start = line;
    loop {
        let b = start / LINES_PER_BLOCK;
        if blocks.get(b).is_none_or(|blk| !blk.has_span) {
            start = line;
            break;
        }
        let floor = b * LINES_PER_BLOCK;
        while start > floor && alloc_sizes[start] == 0 {
            start -= 1;
        }
        if alloc_sizes[start] != 0 {
            break;
        }
        if start == 0 {
            break;
        }
        start -= 1;
    }
    let num_lines = alloc_sizes[start] as usize;
    let num_lines = if num_lines == 0 { 1 } else { num_lines };

    let block_idx = line / LINES_PER_BLOCK;
    let line_idx = line % LINES_PER_BLOCK;
    if block_idx < blocks.len() && claim_line(&blocks[block_idx], line_idx) {
        out.push((block_idx, line_idx));
    }
    for l in start..start + num_lines {
        let block_idx = l / LINES_PER_BLOCK;
        let line_idx = l % LINES_PER_BLOCK;
        if block_idx < blocks.len() && claim_line(&blocks[block_idx], line_idx) {
            out.push((block_idx, line_idx));
        }
    }
}

/// Scan one already-claimed line for heap pointers, claiming what it reaches.
#[inline]
fn scan_line_shared(
    blocks: &[Block],
    alloc_sizes: &[u32],
    heap_start: usize,
    heap_end: usize,
    block_idx: usize,
    line_idx: usize,
    out: &mut Vec<(usize, usize)>,
) {
    let line_start = heap_start + block_idx * BLOCK_SIZE + line_idx * LINE_SIZE;
    for off in (0..LINE_SIZE).step_by(WORD) {
        let val = unsafe { *((line_start + off) as *const usize) };
        if val >= heap_start && val < heap_end {
            let child_line = (val - heap_start) / LINE_SIZE;
            let cb = child_line / LINES_PER_BLOCK;
            let cl = child_line % LINES_PER_BLOCK;
            if cb < blocks.len() && !blocks[cb].is_marked(cl) {
                mark_allocation_shared(blocks, alloc_sizes, child_line, out);
            }
        }
    }
}

/// How many threads mark. `ASH_GC_MARK_THREADS` overrides; 1 keeps the phase
/// on the collecting thread.
fn mark_threads() -> usize {
    static N: OnceLock<usize> = OnceLock::new();
    *N.get_or_init(|| {
        if let Ok(v) = std::env::var("ASH_GC_MARK_THREADS") {
            if let Ok(n) = v.parse::<usize>() {
                return n.max(1);
            }
        }
        // N-1, so the machine keeps a core for everything that is not
        // marking: the promoter thread compiling in the background, the
        // audio and display threads, the OS. `clamp(1, 8)` alone took every
        // core on a 4-core machine and left the collection contending with
        // the very threads it had just stopped the world to get ahead of.
        // On a 10-core box this is unchanged at 8.
        // One thread on wasm, and the parallel marker is not even compiled
        // there (see conservative_trace).
        if cfg!(target_family = "wasm") {
            return 1;
        }
        std::thread::available_parallelism()
            .map(|n| n.get().saturating_sub(1).clamp(1, 8))
            .unwrap_or(1)
    })
}

struct MarkQueue {
    work: std::sync::Mutex<Vec<(usize, usize)>>,
    ready: std::sync::Condvar,
    idle: std::sync::atomic::AtomicUsize,
    done: AtomicBool,
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
            recycle_spans: Vec::new(),
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
            tlab_blocks: HashMap::new(),
            safepoint_mode: false,
            collect_pending: false,
        };

        if std::env::var("ASH_GC_TRACE_MAP").is_ok() {
            let base = heap.memory.base as usize;
            eprintln!(
                "[gc-map] heap reservation {:#x}..{:#x} ({} MB)",
                base,
                base + heap_size,
                heap_size >> 20
            );
        }

        // Reverse order so pop() hands out low addresses first — touched
        // pages stay contiguous at the heap base.
        for i in (0..heap_size).step_by(BLOCK_SIZE).rev() {
            heap.free_blocks.push(i);
        }

        // Zeroed rather than element-wise cloned, so the pages stay
        // demand-committed like the heap mapping itself. `vec![elem; n]`
        // writes every element, which turned the block table into real
        // startup work proportional to the CAP: at a 4GB cap that is ~33MB of
        // memset on a program that may touch none of it, and it cost a fixed
        // ~5ms — 7% of a 56ms benchmark. Sound because an all-zero Block is a
        // valid Block: `mark_bits` is [AtomicU64; N], whose "no line claimed"
        // is 0, and `has_span` likewise.
        let block_count = heap_size / BLOCK_SIZE;
        let blocks: Vec<Block> = unsafe {
            let layout =
                std::alloc::Layout::array::<Block>(block_count).expect("block table layout");
            let ptr = std::alloc::alloc_zeroed(layout) as *mut Block;
            if ptr.is_null() {
                std::alloc::handle_alloc_error(layout);
            }
            Vec::from_raw_parts(ptr, block_count, block_count)
        };

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
                root_slots: HashSet::new(),
            })),
            current_exception: None,
            exception_handler: None,

            fiber_stacks: Vec::new(),
            globals_range: None,
        }
    }

    /// Whether an automatic collection is owed, given the stress setting and
    /// the bytes accumulated since the last cycle:
    /// 1. ASH_GC_STRESS: collect every Nth allocation (validation mode).
    /// 2. Allocated + external bytes since last collect >= adaptive threshold.
    /// 3. Wall-clock heartbeat so long-idle processes deflate.
    ///
    /// Shared by the safepoint and allocation triggers so the two cannot
    /// drift: they are the same question asked from two places.
    fn collection_due(&self, stress: usize, pressure: usize) -> bool {
        if stress > 0 {
            // alloc_count resets on every collection: collect on the Nth
            // allocation since the last one (N=1 → every allocation).
            return self.heap.alloc_count + 1 >= stress;
        }
        pressure >= self.heap.trigger_threshold
            // Heartbeat: clock read only every 1024 allocations.
            || (self.heap.alloc_count & 1023 == 0
                && self.heap.last_collect.elapsed() >= HEARTBEAT)
    }

    /// [`Self::collection_due`] checked at a point known to be a safepoint: a
    /// due trigger collects immediately instead of deferring to the next
    /// interpreter snapshot.
    pub(crate) fn maybe_collect_at_safepoint(&mut self) {
        if !current_mutator_registered() {
            return;
        }
        let stress = gc_stress_every();
        let pressure = self.heap.bytes_since_gc + self.heap.external_since_gc;
        let due = self.collection_due(stress, pressure);
        if !(due || self.heap.collect_pending) {
            return;
        }
        if !triggered_collection_allowed(pressure) {
            return;
        }
        self.collect_garbage();
    }

    fn maybe_collect(&mut self) {
        set_collect_origin(3);
        // No automatic collections before the host runtime has entered user
        // code (hlp_gc_set_stack_top): during bootstrap (constants/class
        // descriptor init) both engines hold GC pointers in host-side Rust
        // structures the conservative scanner cannot see. Bootstrap
        // allocation is finite; the exhaustion backstop still applies.
        if !current_mutator_registered() {
            return;
        }
        let stress = gc_stress_every();
        let pressure = self.heap.bytes_since_gc + self.heap.external_since_gc;
        let due = self.collection_due(stress, pressure);
        if !due {
            return;
        }
        // A disabled collector still gives ground to runaway pressure; the
        // accumulated counters are not cleared here, so re-enabling collects
        // at the very next allocation.
        if !triggered_collection_allowed(pressure) {
            return;
        }
        if self.heap.safepoint_mode {
            let hard = self
                .heap
                .trigger_threshold
                .saturating_mul(4)
                .max(trigger_ceiling_bytes())
                .min(max_deferred_pressure());
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
        clear_marks(&self.blocks[addr / BLOCK_SIZE]);
        if trace_freed() {
            let base = self.heap.memory.as_ptr() as usize;
            eprintln!(
                "[gc-reuse] {:#x}..{:#x}",
                base + addr,
                base + addr + BLOCK_SIZE
            );
        }
        Some(addr)
    }

    /// MADV_FREE_REUSE a block whose pages were previously handed back via
    /// MADV_FREE_REUSABLE — without this, the kernel may discard the pages
    /// under memory pressure AFTER we've written live data into them.
    fn reclaim_block_pages(&mut self, addr: usize) {
        if self.heap.reusable_blocks.remove(&addr) {
            if trace_map() {
                let base = self.heap.memory.as_ptr() as usize;
                eprintln!(
                    "[gc-map] REUSE {:#x}..{:#x}",
                    base + addr,
                    base + addr + BLOCK_SIZE
                );
            }
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
                    set_collect_origin(4);
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
            let num_lines = aligned_size.div_ceil(LINE_SIZE);
            for b in start_line / LINES_PER_BLOCK..=(start_line + num_lines - 1) / LINES_PER_BLOCK {
                if let Some(blk) = self.blocks.get_mut(b) {
                    blk.has_span = true;
                }
            }
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
        let blocks_needed = size.div_ceil(BLOCK_SIZE);
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
                    clear_marks(&self.blocks[block / BLOCK_SIZE]);
                }
                self.heap.bytes_since_gc += blocks_needed * BLOCK_SIZE;
                GC_STATS
                    .bytes_allocated
                    .fetch_add((blocks_needed * BLOCK_SIZE) as u64, Ordering::Relaxed);
                // Record allocation size for GC multi-line marking
                let num_lines = size.div_ceil(LINE_SIZE);
                let start_line = start_addr / LINE_SIZE;
                for b in
                    start_line / LINES_PER_BLOCK..=(start_line + num_lines - 1) / LINES_PER_BLOCK
                {
                    if let Some(blk) = self.blocks.get_mut(b) {
                        blk.has_span = true;
                    }
                }
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
        set_collect_origin(5);
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
            .unwrap_or_else(|| out_of_memory("a closure"))
            .as_ptr() as *mut vclosure;

        let stack = 0;

        // Initialize the closure fields
        ptr::write(
            closure,
            crate::types::vclosure_new_with_stack(t, fun, 1, ptr, stack),
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
        let line_index = (addr % BLOCK_SIZE) / LINE_SIZE;

        // Check if the line is marked (i.e., in use)
        if !self.blocks[block_index].is_marked(line_index) {
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
                | hl::hl_type_kind_HBYTES
                    if !self.is_gc_ptr(vd.v.ptr) =>
                {
                    return false;
                }
                _ => {} // Other types don't have additional pointers to check
            }
        }

        true
    }

    /// Mark all lines belonging to the allocation that contains `line`.
    /// Walks backwards to find the allocation start (line with alloc_sizes > 0),
    /// then marks all lines from start to start+size.
    /// Newly-marked `(block_idx, line_idx)` pairs are pushed onto `out`.
    ///
    /// The buffer is the caller's, not a fresh `Vec` per call: this runs once
    /// per pointer the collector follows, which on bench_binary_trees is 1.97M
    /// times per run, and returning an owned vector made that 1.97M
    /// malloc/free pairs for a median of ONE element each. Threading the
    /// caller's accumulator through halved total GC pause (113ms -> 55ms) with
    /// every per-collection reclaim count byte-identical.
    ///
    /// `out` is always a caller-local accumulator, never a field of `self`, so
    /// there is no aliasing hazard with the `&mut self` mark-bit writes.
    fn mark_allocation_at_line(&mut self, line: usize, out: &mut Vec<(usize, usize)>) {
        // Find the allocation start. Only multi-line spans record a start
        // (`alloc_sizes > 0`); packed small objects never need one, so a
        // block whose has_span flag is clear marks in O(1). The walk, when
        // it runs, is bounded to span-bearing blocks: spans cannot begin in
        // a block that never recorded one, so crossing into a span-free
        // predecessor is proof there is no start to find.
        let mut start = line;
        loop {
            let b = start / LINES_PER_BLOCK;
            if self.blocks.get(b).is_none_or(|blk| !blk.has_span) {
                start = line; // no span can cover `line`
                break;
            }
            let floor = b * LINES_PER_BLOCK;
            while start > floor && self.heap.alloc_sizes[start] == 0 {
                start -= 1;
            }
            if self.heap.alloc_sizes[start] != 0 {
                break;
            }
            if start == 0 {
                break;
            }
            start -= 1; // cross into the previous block (large allocations)
        }
        let num_lines = self.heap.alloc_sizes[start] as usize;
        let num_lines = if num_lines == 0 { 1 } else { num_lines };

        // Small objects pack into lines with no `alloc_sizes` entry, so the
        // walk-back can land on an EARLIER multi-line span that does not
        // cover `line`. Reclaim is whole-block, so the only thing that must
        // hold is that the hit line itself is marked — do that first,
        // unconditionally.
        {
            let block_idx = line / LINES_PER_BLOCK;
            let line_idx = line % LINES_PER_BLOCK;
            if block_idx < self.blocks.len() && claim_line(&self.blocks[block_idx], line_idx) {
                out.push((block_idx, line_idx));
            }
        }
        for l in start..start + num_lines {
            let block_idx = l / LINES_PER_BLOCK;
            let line_idx = l % LINES_PER_BLOCK;
            if block_idx < self.blocks.len() && claim_line(&self.blocks[block_idx], line_idx) {
                out.push((block_idx, line_idx));
            }
        }
    }

    /// Conservative mark: scan a memory range for values that look like heap pointers.
    /// For each match, mark ALL lines of the containing allocation.
    /// Returns list of newly-marked (block, line) pairs.
    fn conservative_scan_range(&mut self, start: usize, end: usize) -> Vec<(usize, usize)> {
        let heap_start = self.heap.memory.as_ptr() as usize;
        let heap_end = heap_start + self.heap.memory.len;
        let mut newly_marked = Vec::new();

        // Step by a machine word, not by eight. The read below is a `usize`,
        // so on a 32-bit target an eight-byte stride reads one slot and skips
        // the next -- half the pointers on the stack are never seen, the
        // objects they hold are collected, and what is left is a wild address.
        // That is what a wasm build was faulting on. Scanning every word
        // over-retains on 64-bit only if the word is not a pointer, which is
        // the conservative contract already.
        let mut addr = start;
        while addr + WORD <= end {
            let raw = unsafe { *(addr as *const usize) };
            // Two candidate interpretations per word: the raw value, and —
            // when the word carries the interpreter's NaN-box pattern — the
            // boxed 48-bit payload. The interpreter's LIVE register buffers
            // hold NaN-boxed words (0x7FF8... | tag | payload), which the
            // raw comparison can never match, and rooting registers through
            // an extracted point-in-time COPY instead left a staleness
            // window: any value written after the last snapshot was
            // invisible, and a collection in that window freed it. The
            // constants bug, the Reflect shadow maps, and this were all the
            // same disease — state the collector could not see where it
            // actually lives. Decoding here lets the buffers be scanned
            // directly. A junk word that happens to decode in-bounds only
            // over-retains, which is the conservative contract already.
            let consider = |val: usize, this: &mut Self, out: &mut Vec<(usize, usize)>| {
                if val >= heap_start && val < heap_end {
                    let offset = val - heap_start;
                    let line = offset / LINE_SIZE;
                    let block_idx = line / LINES_PER_BLOCK;
                    let line_idx = line % LINES_PER_BLOCK;
                    if block_idx < this.blocks.len() && !this.blocks[block_idx].is_marked(line_idx)
                    {
                        this.mark_allocation_at_line(line, out);
                    }
                }
            };
            consider(raw, self, &mut newly_marked);
            // Mirrors ash_interp::values: NAN_TAG 0x7FF8...<<48, 3-bit tag in
            // bits 48-50, payload in bits 0-47.
            // A boxed value is a 64-bit word, so this decoding only means
            // something where a machine word is one. On a 32-bit target the
            // pattern does not fit a `usize` and the scan below would be
            // reading half a value; roots there have to be explicit rather
            // than found by scanning (docs/wasm-target.md, phase 4).
            #[cfg(target_pointer_width = "64")]
            {
                const NAN_TAG: usize = 0x7FF8_0000_0000_0000;
                const NAN_MASK: usize = 0xFFF8_0000_0000_0000;
                const PAYLOAD_MASK: usize = 0x0000_FFFF_FFFF_FFFF;
                if raw & NAN_MASK == NAN_TAG {
                    consider(raw & PAYLOAD_MASK, self, &mut newly_marked);
                }
            }
            addr += WORD;
        }
        newly_marked
    }

    /// Transitively scan all newly-marked heap lines for more heap pointers.
    /// When a new heap pointer is found, marks ALL lines of that allocation.
    fn conservative_trace(&mut self, initial: Vec<(usize, usize)>) {
        let heap_start = self.heap.memory.as_ptr() as usize;
        let heap_end = heap_start + self.heap.memory.len;
        let threads = mark_threads();
        // Below a few hundred roots the spawn costs more than the trace saves.
        if threads <= 1 || initial.len() < 256 {
            let blocks = &self.blocks;
            let alloc_sizes = &self.heap.alloc_sizes;
            let mut worklist = initial;
            while let Some((block_idx, line_idx)) = worklist.pop() {
                scan_line_shared(
                    blocks,
                    alloc_sizes,
                    heap_start,
                    heap_end,
                    block_idx,
                    line_idx,
                    &mut worklist,
                );
            }
            return;
        }

        // Compiled out on wasm rather than merely skipped: the spawn would
        // make the module import `pthread_create`, and mark_threads() is one
        // there, so the serial loop above is the whole collector.
        #[cfg(not(target_family = "wasm"))]
        {
            // Marking is pointer-chasing over the whole live set: latency-bound,
            // so several threads keep more misses outstanding. The world is
            // already stopped, which is why this needs no write barrier -- nothing
            // mutates the heap while the trace runs, and a line is claimed exactly
            // once however many threads reach it.
            let blocks: &[Block] = &self.blocks;
            let alloc_sizes: &[u32] = &self.heap.alloc_sizes;
            let queue = MarkQueue {
                work: std::sync::Mutex::new(initial),
                ready: std::sync::Condvar::new(),
                idle: std::sync::atomic::AtomicUsize::new(0),
                done: AtomicBool::new(false),
            };
            let queue = &queue;

            std::thread::scope(|scope| {
                for _ in 0..threads {
                    scope.spawn(move || {
                        const BATCH: usize = 64;
                        const SPILL: usize = 512;
                        let mut local: Vec<(usize, usize)> = Vec::with_capacity(SPILL * 2);
                        loop {
                            if local.is_empty() {
                                let mut work = queue.work.lock().expect("mark queue poisoned");
                                loop {
                                    if !work.is_empty() {
                                        let take = work.len().min(BATCH);
                                        let at = work.len() - take;
                                        local.extend(work.drain(at..));
                                        break;
                                    }
                                    if queue.done.load(Ordering::Relaxed) {
                                        return;
                                    }
                                    // Everyone idle with an empty queue means the
                                    // trace is finished: a thread only reaches
                                    // here having drained its own local list.
                                    let idle = queue.idle.fetch_add(1, Ordering::Relaxed) + 1;
                                    if idle == threads {
                                        queue.done.store(true, Ordering::Relaxed);
                                        queue.ready.notify_all();
                                        return;
                                    }
                                    // Timed, so a lost wakeup cannot strand anyone.
                                    let (w, _) = queue
                                        .ready
                                        .wait_timeout(work, std::time::Duration::from_micros(200))
                                        .expect("mark queue poisoned");
                                    work = w;
                                    queue.idle.fetch_sub(1, Ordering::Relaxed);
                                }
                            }
                            while let Some((block_idx, line_idx)) = local.pop() {
                                scan_line_shared(
                                    blocks,
                                    alloc_sizes,
                                    heap_start,
                                    heap_end,
                                    block_idx,
                                    line_idx,
                                    &mut local,
                                );
                                if local.len() >= SPILL {
                                    let half = local.len() / 2;
                                    let mut work = queue.work.lock().expect("mark queue poisoned");
                                    work.extend(local.drain(..half));
                                    drop(work);
                                    queue.ready.notify_all();
                                }
                            }
                        }
                    });
                }
            });
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
        let stopped_world = stop_mutator_world();
        // Nothing may be scanned while a mutator is still running: its stack
        // is being written as it would be read. Dropping `stopped_world`
        // releases whoever did park, and the next allocation asks again.
        if !stopped_world.stopped {
            return;
        }
        if trace_freed() || std::env::var("ASH_GC_DEBUG_ROOTS").is_ok() {
            let seq = GC_STATS.collections.load(Ordering::Relaxed) + 1;
            let origin = ORIGIN_NAMES[COLLECT_ORIGIN.load(Ordering::Relaxed).min(6) as usize];
            let base = self.heap.memory.as_ptr() as usize;
            eprintln!(
                "[gc-collect] #{seq} origin={origin} heap={base:#x}..{:#x} ranges={} pending={}",
                base + self.heap.memory.len,
                stopped_world
                    .snapshots
                    .iter()
                    .map(|m| m.scan_ranges.len())
                    .sum::<usize>(),
                self.heap.collect_pending,
            );
        }
        // Split the pause. Which half dominates decides the fix: marking can be
        // parallelised inside the stop -- the world is already stopped, so no
        // write barrier is involved -- while sweeping only visits UNMARKED
        // blocks and could run after the world resumes. Optimising the wrong
        // half buys nothing.
        let t_stop = t0.elapsed();
        let t_mark0 = Instant::now();
        self.mark_roots(&stopped_world.snapshots);
        let t_mark = t_mark0.elapsed();
        let t_sweep0 = Instant::now();
        let freed_blocks = self.sweep(&stopped_world.snapshots);
        let t_sweep = t_sweep0.elapsed();
        let pause = t0.elapsed();

        let live_blocks = self.heap.used_blocks.len();
        let live_bytes = live_blocks * BLOCK_SIZE;

        // Adaptive threshold: next collection after ~live*2 bytes of new
        // allocation (wren_lift gc_marksweep.rs:464-466), bounded so tiny
        // programs don't collect constantly and big ones don't stall.
        // An explicit `ASH_GC_TRIGGER_MB` outranks the default ceiling: asking
        // for a floor above it is a request to collect less often, not a
        // contradiction. Without the `max` the clamp panics with `min > max`,
        // which is what any value over 64MB used to do.
        let floor = trigger_floor_bytes();
        // The ceiling bounds FIXED headroom, but it must never sit below the
        // live set: triggering at half of what is live collects continuously
        // and reclaims progressively less, which is what made a scene load
        // pause every second. Allowing one live-set's worth of garbage is the
        // ordinary space cost of mark-sweep and keeps peak near 2x live --
        // where scaling the ceiling with the whole heap instead let a ~1GB
        // live set accumulate 2GB of garbage and pushed the process past 5GB.
        let ceiling = trigger_ceiling_bytes().max(live_bytes).max(floor);
        self.heap.trigger_threshold =
            (live_bytes.saturating_mul(growth_factor())).clamp(floor, ceiling);

        self.heap.bytes_since_gc = 0;
        self.heap.external_since_gc = 0;
        self.heap.alloc_count = 0;
        self.heap.collect_pending = false;
        // Reset so next allocation picks a fresh free block
        self.heap.allocation_point = 0;
        self.heap.current_block_end = 0;
        self.heap.last_collect = Instant::now();

        // Everything above mutates heap state a resumed mutator would read:
        // the bump cursor especially, since restarting before it is cleared
        // lets a thread allocate from a block this sweep just reclaimed. From
        // here on nothing touches the heap, so the world can restart -- the
        // zone walk below and the stderr writes are not reasons to keep ten
        // threads stopped, and `pause` above has already been measured.
        drop(stopped_world);
        if gc_stats_enabled() {
            let ms = |d: std::time::Duration| d.as_secs_f64() * 1e3;
            eprintln!(
                "[gc-split] stop={:.2}ms mark={:.2}ms sweep={:.2}ms total={:.2}ms",
                ms(t_stop),
                ms(t_mark),
                ms(t_sweep),
                ms(pause)
            );
        }

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

        // `Gc.flags.set(Profile)` asks for the same per-cycle census
        // `ASH_GC_STATS` prints, so it routes here rather than to a second
        // report that could drift from this one.
        if gc_stats_enabled() || gc_flag(GC_FLAG_PROFILE) {
            eprintln!(
                "[gc] #{} origin={} pause={:.2}ms freed={} blocks live={} blocks ({}) \
                 next-trigger={} free={} blocks",
                n,
                ORIGIN_NAMES[COLLECT_ORIGIN.load(Ordering::Relaxed).min(6) as usize],
                pause_ns as f64 / 1e6,
                freed_blocks,
                live_blocks,
                fmt_mb(live_bytes as u64),
                fmt_mb(self.heap.trigger_threshold as u64),
                self.heap.free_blocks.len(),
            );
        }
    }

    fn mark_roots(&mut self, mutators: &[MutatorSnapshot]) {
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
                self.mark_allocation_at_line(line, &mut all_newly_marked);
            }
        }
        for &stack_ptr in &root_set.stack_roots {
            let addr = stack_ptr as usize;
            if addr >= heap_start && addr < heap_end {
                let line = (addr - heap_start) / LINE_SIZE;
                self.mark_allocation_at_line(line, &mut all_newly_marked);
            }
        }
        for &persistent_ptr in &root_set.persistent_roots {
            let addr = persistent_ptr as usize;
            if addr >= heap_start && addr < heap_end {
                let line = (addr - heap_start) / LINE_SIZE;
                self.mark_allocation_at_line(line, &mut all_newly_marked);
            }
        }
        // A native root is a SLOT, so read through it rather than marking the
        // address: `&stash->cb` is malloc'd and would fail the bounds check
        // above, which is precisely how these were silently dropped before --
        // the closure went unmarked, line recycling handed its 32 bytes to the
        // next Haxe string, and `hlp_dyn_call` later read the vclosure's type
        // field as UTF-16 text.
        //
        // Reading it HERE, once per collection, is what makes this upstream's
        // semantics rather than a snapshot: a slot the library overwrites
        // between calls is still correct at the next cycle.
        let slots: Vec<usize> = root_set.root_slots.iter().copied().collect();
        drop(root_set);
        for slot in slots {
            // conservative_scan_range does the read, the heap bounds check and
            // the line marking, so a slot holding null or a non-heap value is
            // ignored exactly as upstream ignores it.
            let newly = self.conservative_scan_range(slot, slot + std::mem::size_of::<usize>());
            all_newly_marked.extend(newly);
        }

        // Conservative scan of globals_data
        let dbg = std::env::var("ASH_GC_DEBUG_ROOTS").is_ok();
        if let Some((globals_ptr, count)) = self.globals_range {
            let start = globals_ptr as usize;
            let end = start + count * std::mem::size_of::<usize>();
            let newly_marked = self.conservative_scan_range(start, end);
            if dbg {
                eprintln!("[gc-roots]   globals marked {} lines", newly_marked.len());
            }
            all_newly_marked.extend(newly_marked);
        }

        // Conservative scan of interpreter-provided ranges
        for mutator in mutators {
            for &(start, size) in &mutator.scan_ranges {
                if size == 0 {
                    continue;
                }
                let end = start.saturating_add(size);
                if end > start {
                    let newly_marked = self.conservative_scan_range(start, end);
                    if dbg {
                        eprintln!(
                            "[gc-roots]   range {start:#x}+{size} marked {} lines",
                            newly_marked.len()
                        );
                    }
                    all_newly_marked.extend(newly_marked);
                }
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
        let collector = thread_self_fast();
        let collector_probe = Self::current_stack_addr().min(buf.as_ptr() as usize);
        let fiber_stacks = self.fiber_stacks.clone();
        for mutator in mutators {
            let raw_sp = if mutator.thread == collector {
                collector_probe
            } else {
                mutator.stack_sp
            };
            if raw_sp == 0 {
                continue;
            }
            // Word-align the probe: conservative_scan_range walks words.
            let sp = word_align_up(raw_sp);
            let running_fiber = fiber_stacks
                .iter()
                .find(|f| {
                    f.thread == mutator.thread && f.size > 0 && sp >= f.base && sp < f.base + f.size
                })
                .map(|f| (f.id, f.base + f.size));
            if dbg {
                let top = running_fiber
                    .map(|(_, top)| top)
                    .unwrap_or(mutator.stack_top);
                eprintln!(
                    "[gc-roots] thread={:#x} sp={sp:#x} stack_top={top:#x} span={}KB ranges={} globals={:?}",
                    mutator.thread,
                    top.saturating_sub(sp) / 1024,
                    mutator.scan_ranges.len(),
                    self.globals_range.map(|(_, c)| c)
                );
            }
            match running_fiber {
                Some((_, top)) => {
                    all_newly_marked.extend(self.conservative_scan_range(sp, top));
                }
                None => {
                    if mutator.stack_top > 0 && sp < mutator.stack_top {
                        all_newly_marked
                            .extend(self.conservative_scan_range(sp, mutator.stack_top));
                    }
                }
            }

            // All OTHER stacks owned by this mutator scan from their saved
            // switch-out SP. The id-0 descriptor is its suspended main stack.
            for f in fiber_stacks.iter().filter(|f| f.thread == mutator.thread) {
                if Some(f.id) == running_fiber.map(|(id, _)| id) || f.saved_sp == 0 {
                    continue;
                }
                let start = word_align_up(f.saved_sp);
                let top = if f.size > 0 {
                    f.base + f.size
                } else {
                    if running_fiber.is_none() {
                        continue;
                    }
                    mutator.stack_top
                };
                if start < top {
                    all_newly_marked.extend(self.conservative_scan_range(start, top));
                }
            }

            // Parked/blocked mutators copied their callee-saved registers
            // into registry-owned storage. The collector's registers are in
            // `buf`, which its live-stack scan already includes.
            if mutator.thread != collector {
                let start = mutator.saved_regs.as_ptr() as usize;
                let end = start + std::mem::size_of_val(&mutator.saved_regs);
                all_newly_marked.extend(self.conservative_scan_range(start, end));
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
                self.blocks[block_index].set_mark(line_index);
                self.blocks[block_index]
                    .any_marked
                    .store(true, Ordering::Relaxed);
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

        if block_index < self.blocks.len() && !self.blocks[block_index].is_marked(line_index) {
            self.blocks[block_index].set_mark(line_index);

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
    fn sweep(&mut self, mutators: &[MutatorSnapshot]) -> usize {
        // Last cycle's spans die with last cycle's marks. Carrying them over
        // would hand out lines in a block this sweep is about to free, and
        // they are rebuilt below anyway.
        self.heap.recycle_spans.clear();
        let used_block_addrs: Vec<usize> = self.heap.used_blocks.iter().copied().collect();
        let mut freed: Vec<usize> = Vec::new();
        let (mut occ_blocks, mut occ_marked) = (0usize, 0usize);
        let mut occ_hist = [0usize; 6];
        // The retained-heap half of the use-after-free audit needs this
        // cycle's marks AFTER the loop below has reset them: only words in
        // lines that were marked LIVE are meaningful referrers — dead lines
        // are full of stale pointers by definition and would drown the
        // signal.
        let audit_marks: Option<std::collections::HashMap<usize, [bool; LINES_PER_BLOCK]>> =
            if sweep_audit() {
                Some(
                    used_block_addrs
                        .iter()
                        .map(|&a| (a, snapshot_marks(&self.blocks[a / BLOCK_SIZE])))
                        .collect(),
                )
            } else {
                None
            };
        // Hoisted: this was a linear scan of every TLAB for every swept block.
        let tlab_set: std::collections::HashSet<usize> =
            self.heap.tlab_blocks.values().copied().collect();
        // Hoisted for the same reason: this was a fresh Vec per swept block,
        // so a sweep over ~18,000 retained blocks did ~18,000 malloc/free
        // pairs inside the stop-the-world. Reusing one buffer keeps the
        // capacity across blocks and changes nothing else.
        let mut spans: Vec<(usize, usize)> = Vec::new();
        for block_addr in used_block_addrs {
            // The mutator's live bump region: marks still reset below for
            // the next cycle, but the block is never reclaimed under the
            // cursor.
            let is_tlab = tlab_set.contains(&block_addr);
            let block_index = block_addr / BLOCK_SIZE;
            let block = &mut self.blocks[block_index];
            // Nothing reached this block, so every bit is already clear and
            // the scan below could only confirm it.
            let touched = *block.any_marked.get_mut();
            *block.any_marked.get_mut() = false;
            let mut is_empty = true;
            let mut marked_lines = 0usize;
            // Runs of unmarked lines, harvested in the pass that resets the
            // marks. Only for blocks this sweep KEEPS: an empty block goes
            // back whole, and the TLAB block is still being bumped through.
            spans.clear();
            let mut run_start: Option<usize> = None;
            // Plain reads and writes, not atomics: sweep holds `&mut self`, so
            // no marker is running and `get_mut` reaches the bit directly. The
            // read-modify-write this replaces ran 256 times per block, which at
            // a 680MB live set is millions of atomics for no ordering anyone
            // observes.
            if touched {
                for (word_index, slot) in block.mark_bits.iter_mut().enumerate() {
                    let word = std::mem::replace(slot.get_mut(), 0);
                    // A word of 64 unmarked lines is the common case on a
                    // sparsely reached block; skipping it keeps the run that
                    // `run_start` is tracking open across the whole word.
                    if word == 0 {
                        if run_start.is_none() {
                            run_start = Some(word_index << 6);
                        }
                        continue;
                    }
                    for bit_index in 0..64 {
                        let line_index = (word_index << 6) | bit_index;
                        let was_marked = word & (1u64 << bit_index) != 0;
                        if was_marked {
                            is_empty = false;
                            marked_lines += 1;
                            if let Some(start) = run_start.take() {
                                spans.push((start, line_index - start));
                            }
                        } else if run_start.is_none() {
                            run_start = Some(line_index);
                        }
                    }
                }
                if let Some(start) = run_start.take() {
                    spans.push((start, LINES_PER_BLOCK - start));
                }
            }
            // An untouched block stays `is_empty` with no spans, which is what
            // the two branches below already do with a block nothing reached:
            // reclaim it whole, or keep it whole when it is a TLAB or when
            // reclamation is off. Neither reads `spans`.

            if occupancy_stats() && !is_empty {
                occ_blocks += 1;
                occ_marked += marked_lines;
                occ_hist[match marked_lines {
                    1 => 0,
                    2..=4 => 1,
                    5..=16 => 2,
                    17..=64 => 3,
                    65..=192 => 4,
                    _ => 5,
                }] += 1;
            }
            if !is_empty && !is_tlab && recycle_lines() {
                for (start, len) in spans.drain(..) {
                    self.heap.recycle_spans.push((block_addr, start, len));
                }
            }
            if is_empty && !is_tlab && !no_reclaim() {
                self.heap.used_blocks.remove(&block_addr);
                if trace_freed() {
                    let base = self.heap.memory.as_ptr() as usize;
                    let seq = GC_STATS.collections.load(Ordering::Relaxed) + 1;
                    eprintln!(
                        "[gc-freed] #{seq} {:#x}..{:#x}",
                        base + block_addr,
                        base + block_addr + BLOCK_SIZE
                    );
                }
                // Use-after-free detector: a block about to be freed while a
                // ROOT still points into it means the tracer failed; no such
                // root means the snapshot never contained the pointer. This
                // is the question that splits every premature-free bug.
                if sweep_audit() {
                    let base = self.heap.memory.as_ptr() as usize;
                    let lo = base + block_addr;
                    let hi = lo + BLOCK_SIZE;
                    let audit = |src: &str, start: usize, end: usize| {
                        let mut p = start & !(WORD - 1);
                        while p + WORD <= end {
                            let w = unsafe { *(p as *const usize) };
                            if (lo..hi).contains(&w) {
                                eprintln!(
                                    "[gc-audit] FREED {lo:#x}..{hi:#x} but {src} @{p:#x} holds {w:#x}"
                                );
                            }
                            // Interpreter scan ranges hold NaN-BOXED words;
                            // the marker decodes them (conservative_scan_range),
                            // so the auditor must too or it is blind to every
                            // register-held root.
                            // Same 64-bit-word assumption as the marker
                            // above, and the same reason.
                            #[cfg(target_pointer_width = "64")]
                            {
                                const NAN_TAG: usize = 0x7FF8_0000_0000_0000;
                                const NAN_MASK: usize = 0xFFF8_0000_0000_0000;
                                const PAYLOAD_MASK: usize = 0x0000_FFFF_FFFF_FFFF;
                                if w & NAN_MASK == NAN_TAG {
                                    let d = w & PAYLOAD_MASK;
                                    if (lo..hi).contains(&d) {
                                        eprintln!(
                                            "[gc-audit] FREED {lo:#x}..{hi:#x} but {src} @{p:#x} holds boxed {d:#x}"
                                        );
                                    }
                                }
                            }
                            p += WORD;
                        }
                    };
                    if let Some((gp, count)) = self.globals_range {
                        audit("globals", gp as usize, gp as usize + count * std::mem::size_of::<usize>());
                    }
                    for mutator in mutators {
                        for &(rs, sz) in &mutator.scan_ranges {
                            audit("range", rs, rs + sz);
                        }
                    }
                    // Every stopped machine/fiber stack too — the mark phase
                    // scanned the same ownership-qualified ranges.
                    let collector = thread_self_fast();
                    for mutator in mutators {
                        let raw_sp = if mutator.thread == collector {
                            Self::current_stack_addr()
                        } else {
                            mutator.stack_sp
                        };
                        if raw_sp == 0 {
                            continue;
                        }
                        let sp = word_align_up(raw_sp);
                        let running = self.fiber_stacks.iter().find(|f| {
                            f.thread == mutator.thread
                                && f.size > 0
                                && sp >= f.base
                                && sp < f.base + f.size
                        });
                        let top = running
                            .map(|f| f.base + f.size)
                            .unwrap_or(mutator.stack_top);
                        if sp < top {
                            audit("stack", sp, top);
                        }
                        for fiber in self
                            .fiber_stacks
                            .iter()
                            .filter(|f| f.thread == mutator.thread && f.saved_sp != 0)
                        {
                            if running.is_some_and(|active| active.id == fiber.id) {
                                continue;
                            }
                            let saved_sp = word_align_up(fiber.saved_sp);
                            let saved_top = if fiber.size > 0 {
                                fiber.base + fiber.size
                            } else {
                                mutator.stack_top
                            };
                            if saved_sp < saved_top {
                                audit("suspended-stack", saved_sp, saved_top);
                            }
                        }
                    }
                }
                if poison_freed() || quarantine_freed() {
                    unsafe {
                        std::ptr::write_bytes(
                            self.heap.memory.as_mut_ptr().add(block_addr),
                            0xA5,
                            BLOCK_SIZE,
                        );
                    }
                }
                if !quarantine_freed() {
                    self.heap.free_blocks.push(block_addr);
                }
                // Clear alloc_sizes for all lines in this freed block
                let base_line = block_index * LINES_PER_BLOCK;
                self.heap.alloc_sizes[base_line..base_line + LINES_PER_BLOCK].fill(0);
                self.blocks[block_index].has_span = false;
                freed.push(block_addr);
            }
        }

        // Second half of the use-after-free detector: pointers INTO a freed
        // block from lines of the RETAINED heap that were marked LIVE this
        // cycle. The per-block audit above covers roots (globals /
        // interpreter ranges / machine stack); a live object whose only
        // referrer is a heap field shows up here instead. Dead lines are
        // skipped — stale pointers in garbage are expected, not evidence.
        // One O(retained heap) pass per collection, diagnosis-only.
        if !freed.is_empty() {
            if let Some(marks) = &audit_marks {
                let base = self.heap.memory.as_ptr() as usize;
                let seq = GC_STATS.collections.load(Ordering::Relaxed) + 1;
                let in_freed = |w: usize| -> bool {
                    if w < base || w >= base + self.heap.memory.len {
                        return false;
                    }
                    let off = (w - base) & !(BLOCK_SIZE - 1);
                    freed.contains(&off)
                };
                for &block_addr in self.heap.used_blocks.iter() {
                    let Some(mark_bits) = marks.get(&block_addr) else {
                        continue;
                    };
                    for (line_index, &live) in mark_bits.iter().enumerate() {
                        if !live {
                            continue;
                        }
                        let lo = base + block_addr + line_index * LINE_SIZE;
                        let mut p = lo;
                        while p + WORD <= lo + LINE_SIZE {
                            let w = unsafe { *(p as *const usize) };
                            if in_freed(w) {
                                eprintln!(
                                    "[gc-audit] #{seq} live line word @{p:#x} points into freed block ({w:#x})"
                                );
                            }
                            p += WORD;
                        }
                    }
                }
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
            let surplus = self.heap.free_blocks.len().saturating_sub(resident_target);
            let mut hand_back: Vec<usize> = freed
                .iter()
                .copied()
                .take(surplus)
                .filter(|a| !self.heap.reusable_blocks.contains(a))
                .collect();
            if !hand_back.is_empty() && handback_enabled() {
                hand_back.sort_unstable();
                let base = self.heap.memory.as_mut_ptr();
                let mut run_start = hand_back[0];
                let mut run_len = BLOCK_SIZE;
                // Returns whether the range was actually handed back. On
                // macOS the REUSABLE/REUSE pair is a LEDGER: REUSABLE debits
                // the process footprint, REUSE credits it. Recording a block
                // as reusable when the advice failed means the matching REUSE
                // still runs later and credits memory that was never debited,
                // so the footprint climbs by the whole recycled set every
                // collection while RSS stays flat. Measured on a game: a
                // steady 144MB live set churning 512MB per cycle drove the
                // reported footprint to 1.2GB, 2.4GB, 3.6GB, 4.8GB, 6.0GB in
                // even steps, and the machine paged itself to a stop.
                let advise = |start: usize, len: usize| -> bool {
                    #[cfg(unix)]
                    unsafe {
                        #[cfg(target_os = "macos")]
                        let advice = libc::MADV_FREE_REUSABLE;
                        #[cfg(not(target_os = "macos"))]
                        let advice = libc::MADV_DONTNEED;
                        return libc::madvise(base.add(start) as *mut c_void, len, advice) == 0;
                    }
                    // Windows' MADV_DONTNEED: the pages leave the working set
                    // (so RSS falls, which is the whole point here) but stay
                    // committed, so the range stays mapped and only its
                    // contents go undefined — sound for exactly the reason
                    // the unix paths are, since a reacquired block is zeroed
                    // before anything reads it.
                    #[cfg(windows)]
                    unsafe {
                        DiscardVirtualMemory(base.add(start) as *mut c_void, len);
                        return true;
                    }
                    #[allow(unreachable_code)]
                    true
                };
                let mut runs: Vec<(usize, usize)> = Vec::new();
                for &addr in &hand_back[1..] {
                    if addr == run_start + run_len {
                        run_len += BLOCK_SIZE;
                    } else {
                        runs.push((run_start, run_len));
                        run_start = addr;
                        run_len = BLOCK_SIZE;
                    }
                }
                runs.push((run_start, run_len));
                // Only a range the kernel accepted is recorded, so the REUSE
                // that pairs with it is only ever issued against a range that
                // was really handed back.
                for (start, len) in runs {
                    if !advise(start, len) {
                        continue;
                    }
                    let mut addr = start;
                    while addr < start + len {
                        self.heap.reusable_blocks.insert(addr);
                        addr += BLOCK_SIZE;
                    }
                }
                if trace_map() {
                    let base = self.heap.memory.as_ptr() as usize;
                    for &addr in &hand_back {
                        eprintln!(
                            "[gc-map] HANDBACK {:#x}..{:#x}",
                            base + addr,
                            base + addr + BLOCK_SIZE
                        );
                    }
                }
            }
        }

        if occupancy_stats() && occ_blocks > 0 {
            let seq = GC_STATS.collections.load(Ordering::Relaxed) + 1;
            let pct = occ_marked as f64 / (occ_blocks * LINES_PER_BLOCK) as f64 * 100.0;
            eprintln!(
                "[gc-occ] #{seq} retained={occ_blocks} blocks ({:.1}MB) marked_lines={occ_marked} \
                 ({:.1}MB, {pct:.1}% full)  by-marked-lines: 1={} 2-4={} 5-16={} 17-64={} 65-192={} 193+={}",
                (occ_blocks * BLOCK_SIZE) as f64 / 1048576.0,
                (occ_marked * LINE_SIZE) as f64 / 1048576.0,
                occ_hist[0], occ_hist[1], occ_hist[2], occ_hist[3], occ_hist[4], occ_hist[5],
            );
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

    /// Root the object a native slot currently points at, and keep doing so as
    /// its contents change. See [`RootSet::root_slots`].
    pub fn add_root_slot(&mut self, slot: usize) {
        self.roots.borrow_mut().root_slots.insert(slot);
    }

    pub fn remove_root_slot(&mut self, slot: usize) {
        self.roots.borrow_mut().root_slots.remove(&slot);
    }

    /// Which of the two root kinds an address was filed under. The distinction
    /// is the whole of `hl_add_root`'s contract -- a slot is dereferenced on
    /// every mark, a persistent root is marked directly -- and filing an
    /// address under the wrong one is silent until a collection frees a live
    /// object, so it is worth being able to assert on.
    pub fn has_root_slot(&self, slot: usize) -> bool {
        self.roots.borrow().root_slots.contains(&slot)
    }

    pub fn has_persistent(&self, ptr: *mut hl::vdynamic) -> bool {
        self.roots.borrow().persistent_roots.contains(&ptr)
    }

    pub fn unregister_persistent(&mut self, ptr: *mut hl::vdynamic) {
        self.roots.borrow_mut().persistent_roots.remove(&ptr);
    }

    pub fn clear_scan_ranges(&mut self) {
        self.heap.safepoint_mode = true;
        clear_current_scan_ranges();
    }

    /// Register an interpreter root snapshot. This is the interpreter's
    /// safepoint: the snapshot is complete at this instant, so a deferred
    /// collection trigger is honored here.
    pub fn add_scan_range(&mut self, ptr: *const c_void, size: usize) {
        self.heap.safepoint_mode = true;
        if !ptr.is_null() && size != 0 {
            add_current_scan_range(ptr as usize, size);
        }
        // Deliberately no pending-collection consumption here: a snapshot
        // now spans SEVERAL add calls (one per interpreter frame), and a
        // collection between them would run against a half-built root set —
        // wiped by clear, only partially repopulated. The interpreter calls
        // `hlp_gc_scan_roots_done` when the set is complete.
    }

    /// The snapshot is complete: a deferred collection is honored now.
    pub fn scan_roots_done(&mut self) {
        publish_current_scan_ranges();
        if self.heap.collect_pending {
            set_collect_origin(1);
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

    // gc_alloc returns zeroed memory, so zeroing again here is redundant
    // and would run outside the allocator's lock.
    match gc_alloc(size_usize) {
        Some(ptr) => ptr.as_ptr() as *mut std::os::raw::c_void,
        None => ptr::null_mut(),
    }
}

#[no_mangle]
pub extern "C" fn hlp_mark_size(data_size: i32) -> i32 {
    let data_size = data_size as usize;
    let ptr_count = data_size.div_ceil(HL_WSIZE as usize);
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
    let gc = match (*(&raw mut GC)).get_mut() {
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

/// Byte span the collector reserved for the allocation containing `ptr`.
///
/// Upstream answers this from a page's block size; ash's granule is the line,
/// so a multi-line allocation reports its recorded span and a small object
/// reports the one line it shares. Zero for anything outside the arena, which
/// is what upstream returns for a pointer it did not hand out.
pub(crate) unsafe fn allocation_size(ptr: *const c_void) -> usize {
    let gc = gc_locked_init();
    let base = gc.heap.memory.as_ptr() as usize;
    let addr = ptr as usize;
    if addr < base || addr >= base + gc.heap.memory.len {
        return 0;
    }
    let line = (addr - base) / LINE_SIZE;
    let mut start = line;
    loop {
        let b = start / LINES_PER_BLOCK;
        if gc.blocks.get(b).is_none_or(|blk| !blk.has_span) {
            start = line;
            break;
        }
        let floor = b * LINES_PER_BLOCK;
        while start > floor && gc.heap.alloc_sizes[start] == 0 {
            start -= 1;
        }
        if gc.heap.alloc_sizes[start] != 0 || start == 0 {
            break;
        }
        start -= 1;
    }
    let lines = gc.heap.alloc_sizes.get(start).copied().unwrap_or(0) as usize;
    if lines == 0 {
        LINE_SIZE
    } else {
        lines * LINE_SIZE
    }
}

/// Give the collector a chance to stop this thread.
///
/// Upstream's `gc_safepoint` exists so a long native loop that never allocates
/// can still be stopped by a collection. ash's own safepoint is the same
/// rendezvous the mutator registry uses; a thread the collector was never told
/// about passes straight through, exactly as upstream allows.
#[no_mangle]
pub unsafe extern "C" fn hlp_gc_safepoint() {
    gc_safepoint();
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
    register_current_mutator(top, "runtime");
}

/// HashLink-compatible OS-mutator registration used by HDLL-created worker
/// threads. `hlp_gc_set_stack_top` is the idempotent host-runtime spelling;
/// both feed the same per-thread registry.
#[no_mangle]
pub unsafe extern "C" fn hl_register_thread(stack_top: *mut c_void) {
    // A thread a native library started. It runs native code and reaches a
    // safepoint only by calling back into the runtime or by marking itself
    // blocking, so it is the likeliest to be late.
    register_current_mutator(stack_top as usize, "hdll");
}

#[no_mangle]
pub unsafe extern "C" fn hl_unregister_thread() {
    let thread = thread_self_fast();
    unregister_current_mutator();
    let mut gc = gc_locked_init();
    gc.fiber_stacks.retain(|fiber| fiber.thread != thread);
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
pub unsafe extern "C" fn hlp_gc_scan_roots_done() {
    let mut gc = gc_locked();
    gc.scan_roots_done();
}

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

/// Replace the whole interpreter-provided scan set in ONE lock hold.
///
/// One cross-dylib call and one lock acquisition per publish. Doing it per
/// stack frame instead makes root publication O(depth) in lock holds, and the
/// interpreter publishes twice per call, so the cost is quadratic in depth.
///
/// `ranges` points at `count` (addr, size) pairs. Same semantics as the
/// sequence it replaces, including honouring a deferred collection only
/// once the set is complete — never against a half-built one.
///
/// # Safety
/// `ranges` must point to `count` initialised `(usize, usize)` pairs, and
/// each pair must describe memory that stays valid until the next publish.
/// Hand the collector a live view of this mutator's scan-range table.
///
/// Called once; afterwards the mutator maintains the table itself and the
/// collector reads it when it stops the world. Replaces a per-call copy under
/// two locks -- on a recursive program that copy was most of the run.
#[no_mangle]
pub unsafe extern "C" fn hlp_gc_set_scan_roots_live(
    ranges: *const (usize, usize),
    len: *const usize,
) {
    // Same signal the copying publish gives: a mutator that publishes roots
    // can be asked to defer a collection to its next safepoint. Without it
    // the deferral branch never runs, and collections stop being batched --
    // every origin on a game session became `hard-pressure`, which is the
    // bound deferral exists to stay under, not the path it should take.
    let mut gc = gc_locked();
    gc.heap.safepoint_mode = true;
    let thread = thread_self_fast();
    let mut world = MUTATOR_WORLD.state.lock().unwrap();
    if let Some(record) = world.mutators.iter_mut().find(|m| m.thread == thread) {
        record.scan_live = Some((ranges as usize, len as usize));
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_gc_set_scan_roots(ranges: *const (usize, usize), count: usize) {
    let mut gc = gc_locked();
    gc.heap.safepoint_mode = true;
    let ranges: &[(usize, usize)] = if ranges.is_null() || count == 0 {
        &[]
    } else {
        std::slice::from_raw_parts(ranges, count)
    };
    set_current_scan_ranges(ranges);
    // The world lock is released by now, and must be: honouring a deferred
    // collection reaches stop_mutator_world, which takes it again.
    if gc.heap.collect_pending {
        set_collect_origin(1);
        gc.collect_garbage();
    }
}

/// Charge off-heap memory (fiber stacks, native buffers, JIT structures) as
/// GC allocation pressure. The charge participates in the byte-driven
/// collection trigger and resets after every collection.
#[no_mangle]
pub unsafe extern "C" fn hlp_gc_track_external(bytes: u64) {
    let mut gc = gc_locked_init();
    gc.track_external(bytes as usize);
}

/// Upstream hl_gc_enable (gc.c): flip the automatic collector on or off.
///
/// Only the *trigger* is suppressed. `Gc.major`, the heap-exhaustion backstop
/// in [`ImmixAllocator::allocate`], and the runaway-pressure escape hatch
/// ([`gc_disabled_max_pressure`]) all still collect, so a program that
/// disables the GC and never re-enables it loses collections, not the heap.
/// Nothing here takes a lock, so it cannot deadlock against a collection in
/// flight on another thread.
#[no_mangle]
pub unsafe extern "C" fn hlp_gc_enable(b: bool) {
    GC_ENABLED.store(b, Ordering::Relaxed);
}

/// `hl.Gc.flags` getter.
#[no_mangle]
pub unsafe extern "C" fn hlp_gc_get_flags() -> i32 {
    GC_FLAGS.load(Ordering::Relaxed)
}

/// `hl.Gc.flags` setter.
///
/// The word is stored whole, so a program that reads the flags, flips one bit
/// and writes them back gets its own value back.
///
/// One bit changes behaviour: `Profile` prints the per-cycle census.
///
/// The rest are stored and reported back without acting. `ForceMajor` asks
/// upstream's generational collector to promote the next cycle from minor to
/// major; ash's Immix heap has no such split — every collection is a full
/// conservative mark — so the request is already satisfied by construction.
/// (Wiring it to the allocation trigger instead would mean collecting on
/// every allocation, and since a cycle resets the bump pointer, every object
/// would then claim a fresh 32KB block.) `NoThreads` exists upstream to skip
/// a stop-the-world handshake ash's scan never performs, and skipping the
/// scan itself would lose live objects rather than save time. `DumpMem`
/// would have to write the heap from inside the allocator, which already
/// holds the GC lock the dumper takes; `Gc.dumpMemory()` produces the same
/// file from a caller that can.
#[no_mangle]
pub unsafe extern "C" fn hlp_gc_set_flags(f: i32) {
    GC_FLAGS.store(f, Ordering::Relaxed);
}

/// Upstream hl_gc_major (gc.c): collect now, whatever the trigger thinks.
///
/// `Gc.enable(false)` is deliberately not consulted, exactly as upstream's
/// `hl_gc_major` calls `gc_major()` directly: a program naming a collection
/// is not asking about the automatic trigger, and [`hlp_gc_enable`] already
/// documents that explicit collections still run.
///
/// Running a full cycle from a native entry point is the same shape as the
/// exhaustion backstop in [`ImmixAllocator::allocate`], which has always
/// collected here: the calling thread becomes the collector, its own
/// callee-saved registers are spilled into the frame its live-stack scan
/// covers, and the other mutators stop through the usual handshake. The GC
/// lock is reentrant, so a caller already holding it gets its collection
/// rather than a deadlock.
#[no_mangle]
pub unsafe extern "C" fn hlp_gc_major() {
    let mut gc = gc_locked_init();
    set_collect_origin(6); // "explicit" — see ORIGIN_NAMES
    gc.collect_garbage();
}

/// Upstream hl_gc_stats (gc.c): three counters through out-params, read by
/// `hl.Gc.stats()` as `totalAllocated` / `allocationCount` / `currentMemory`.
/// Doubles because the byte totals outrun an i32 within seconds of running.
///
/// Two of the three are ash's own numbers; the third is one ash does not have.
///
/// * `total_allocated` — [`GC_STATS`]`.bytes_allocated`, cumulative and never
///   reset, the same figure the `ASH_GC_STATS` report prints. Like upstream's
///   it counts bytes the collector HANDED OUT rather than bytes requested;
///   ash's is coarser, because a TLAB refill charges its whole 32KB region
///   when the region is carved instead of per object bumped out of it.
///   Memory charged through [`hlp_gc_track_external`] is excluded: it is
///   allocation pressure from off-heap buffers, not GC heap, and folding it
///   in would make this number disagree with the same field in the report.
/// * `allocation_count` — 0, because ash counts no allocations. The TLAB fast
///   path is a thread-local pointer bump that deliberately touches no shared
///   counter (an atomic there was the cost the TLAB exists to remove), and
///   `heap.alloc_count` counts refills and locked allocations only and resets
///   at every collection: it is the trigger's input, not a census. Reporting
///   it here would undercount by orders of magnitude AND shrink between two
///   samples, so a caller differencing successive stats calls would read the
///   allocation rate as negative. A constant zero is visibly not an answer.
/// * `current_memory` — blocks currently handed out, at block granularity.
///   Upstream reports the total size of the pages it has mapped; ash reserves
///   its entire heap up front and commits on first touch, so the reservation
///   (hundreds of MB, machine-derived) would report the same number for every
///   program. Used blocks is the figure that tracks what the program holds.
///
/// The null checks are for the C side: a `hl.Ref` from Haxe is never null,
/// but an HDLL caller that wants one field can pass NULL for the others, and
/// that must not be a store to address 0.
#[no_mangle]
pub unsafe extern "C" fn hlp_gc_stats(
    total_allocated: *mut f64,
    allocation_count: *mut f64,
    current_memory: *mut f64,
) {
    if !total_allocated.is_null() {
        *total_allocated = GC_STATS.bytes_allocated.load(Ordering::Relaxed) as f64;
    }
    if !allocation_count.is_null() {
        *allocation_count = 0.0;
    }
    if !current_memory.is_null() {
        let gc = gc_locked_init();
        *current_memory = (gc.heap.used_blocks.len() * BLOCK_SIZE) as f64;
    }
}

/// Upstream hl_gc_profile (gc.c): the `Profile` flag as a function.
///
/// It writes the same `GC_FLAGS` word [`hlp_gc_set_flags`] does, so the two
/// spellings cannot drift: `Gc.profile(true)` then reading `Gc.flags` shows
/// the bit, and `collect_garbage` already keys its per-cycle census on it.
///
/// `false` clears that bit and nothing else. Upstream writes
/// `gc_flags &= GC_PROFILE`, which keeps profiling on and clears every OTHER
/// flag — a missing `~` rather than a semantic, since upstream's own
/// `Gc.flags.unset(Profile)` does what this does.
#[no_mangle]
pub unsafe extern "C" fn hlp_gc_profile(b: bool) {
    if b {
        GC_FLAGS.fetch_or(GC_FLAG_PROFILE, Ordering::Relaxed);
    } else {
        GC_FLAGS.fetch_and(!GC_FLAG_PROFILE, Ordering::Relaxed);
    }
}

/// Upstream hl_gc_get_live_objects (gc.c): count the live objects of a type,
/// filling `arr` with as many as it holds.
///
/// Ash returns -1 — upstream's "cannot answer" value — for every type,
/// including the ones upstream answers for, and leaves `arr` untouched.
///
/// Upstream can enumerate because its heap is typed by construction: every
/// GC block begins an object, each page carries a memory kind, so walking
/// the live blocks of the matching kind and wrapping each address with
/// `hl_make_dyn` yields real objects. Ash's Immix heap has neither half.
/// Marking is per 128-byte LINE while small objects are bump-packed 16 bytes
/// apart inside one, so a marked line is not an object address — only
/// multi-line allocations record a start, in `alloc_sizes`, and only to let
/// the marker walk back — and nothing anywhere records an allocation's type.
/// Handing back line addresses as `t` would give the caller interior
/// pointers and non-objects to read fields from: a crash inside caller code,
/// pointing away from its cause.
///
/// So the honest answers are -1 or 0, and 0 is the wrong one: it asserts
/// "no live objects of this type", which is a claim about a heap ash cannot
/// enumerate. -1 leaves `for i in 0...n` empty for a caller that treats the
/// result as a count, and is a value a caller checking for failure already
/// handles. What either answer buys over the absent symbol is that the call
/// returns: an unresolved native resolves to a call-time trap (the JIT's stub,
/// the interpreter's failed lazy lookup), so a program that reaches this one
/// dies at the call site instead of being told ash cannot enumerate its heap.
#[no_mangle]
pub unsafe extern "C" fn hlp_gc_get_live_objects(_t: *mut hl_type, _arr: *mut hl::varray) -> i32 {
    -1
}

/// Read a NUL-terminated UTF-8 C string, bounded so a caller that forgets the
/// terminator walks a page rather than the address space.
///
/// UTF-8, not the usual UTF-16: `hl_gc_dump_memory` takes `const char*` and
/// `hl.Gc.dumpMemory` passes `fileName.toUtf8()`.
unsafe fn c_utf8_path(p: *const hl::vbyte) -> Option<String> {
    if p.is_null() {
        return None;
    }
    let mut len = 0usize;
    while len < 4096 && *p.add(len) != 0 {
        len += 1;
    }
    if len == 0 {
        return None;
    }
    let bytes = std::slice::from_raw_parts(p, len);
    Some(String::from_utf8_lossy(bytes).into_owned())
}

/// Upstream hl_gc_dump_memory (gc.c): mark, then write the heap out for
/// offline analysis.
///
/// The file is NOT HashLink's `HMD1`. That format is a transcript of
/// HashLink's own allocator — `gc_pheader`, page kinds, per-page block
/// iteration — and ash's Immix heap has none of those structures to
/// transcribe. Writing ash's numbers under HashLink's magic would make
/// `hl memory` mis-parse the file instead of rejecting it, so the magic says
/// what the file actually is and the body is self-describing text.
///
/// The mark is a real one (it is what makes the per-block census mean
/// anything), but nothing is swept: a dump must not change what the program
/// can still reach. The bits it sets are left standing rather than cleared —
/// a mark bit only ever *retains* a block, and the next sweep clears them all
/// — so the cost of dumping is at most one extra conservative cycle. Clearing
/// them would be the unsafe direction: it would also drop the marks
/// allocation-time marking had already set.
#[no_mangle]
pub unsafe extern "C" fn hlp_gc_dump_memory(filename: *mut hl::vbyte) {
    use std::io::Write;

    let path = c_utf8_path(filename).unwrap_or_else(|| "hlmemory.dump".to_string());
    let Ok(file) = std::fs::File::create(&path) else {
        eprintln!("[gc] dump_memory: cannot create {path}");
        return;
    };
    let mut out = std::io::BufWriter::new(file);

    let mut gc = gc_locked_init();
    let stopped_world = stop_mutator_world();

    // Before the mutator has entered user code there is no stack to scan
    // conservatively, and marking from a partial root set would report
    // live data as garbage.
    let marked = !stopped_world.snapshots.is_empty();
    if marked {
        gc.mark_roots(&stopped_world.snapshots);
    }

    let heap_base = gc.heap.memory.as_ptr() as usize;
    let heap_len = gc.heap.memory.len;
    let roots = gc.roots.borrow();

    let mut w = |line: String| {
        let _ = writeln!(out, "{line}");
    };
    w("ASHMEM1 ash-immix".into());
    w(format!("pointer-size {}", mem::size_of::<usize>()));
    w(format!("heap-base {heap_base:#x}"));
    w(format!("heap-size {heap_len}"));
    w(format!("block-size {BLOCK_SIZE}"));
    w(format!("line-size {LINE_SIZE}"));
    w(format!("blocks-total {}", heap_len / BLOCK_SIZE));
    w(format!("blocks-used {}", gc.heap.used_blocks.len()));
    w(format!("blocks-free {}", gc.heap.free_blocks.len()));
    w(format!("blocks-reusable {}", gc.heap.reusable_blocks.len()));
    if gc.heap.tlab_blocks.is_empty() {
        w("tlab-blocks none".into());
    } else {
        let mut blocks: Vec<usize> = gc.heap.tlab_blocks.values().copied().collect();
        blocks.sort_unstable();
        let rendered: Vec<String> = blocks
            .iter()
            .map(|b| format!("{:#x}", heap_base + b))
            .collect();
        w(format!("tlab-blocks {}", rendered.join(" ")));
    }
    w(format!("alloc-count {}", gc.heap.alloc_count));
    w(format!("bytes-since-gc {}", gc.heap.bytes_since_gc));
    w(format!("external-since-gc {}", gc.heap.external_since_gc));
    w(format!("trigger-threshold {}", gc.heap.trigger_threshold));
    w(format!(
        "collector-enabled {}",
        GC_ENABLED.load(Ordering::Relaxed)
    ));
    w(format!("safepoint-mode {}", gc.heap.safepoint_mode));
    w(format!(
        "collections {}",
        GC_STATS.collections.load(Ordering::Relaxed)
    ));
    w(format!(
        "blocks-reclaimed {}",
        GC_STATS.blocks_reclaimed.load(Ordering::Relaxed)
    ));
    w(format!(
        "bytes-allocated {}",
        GC_STATS.bytes_allocated.load(Ordering::Relaxed)
    ));
    w(format!(
        "external-bytes {}",
        GC_STATS.external_bytes.load(Ordering::Relaxed)
    ));
    w(format!(
        "pause-ns-total {}",
        GC_STATS.pause_ns_total.load(Ordering::Relaxed)
    ));
    w(format!(
        "pause-ns-max {}",
        GC_STATS.pause_ns_max.load(Ordering::Relaxed)
    ));
    w(format!("roots-globals {}", roots.globals.len()));
    w(format!("roots-stack {}", roots.stack_roots.len()));
    w(format!("roots-persistent {}", roots.persistent_roots.len()));
    w(format!("scan-ranges {}", mutator_scan_range_count()));
    w(format!("marked {marked}"));

    // One line per retained block: address, live lines, live bytes at line
    // granularity. Line marks are the finest liveness ash records — reclaim
    // is whole-block, so a block's marked-line count is its real occupancy.
    w("# block <addr> <live-lines> <live-bytes>".into());
    let mut used: Vec<usize> = gc.heap.used_blocks.iter().copied().collect();
    used.sort_unstable();
    for block_addr in used {
        let live = gc.blocks[block_addr / BLOCK_SIZE].marked_line_count();
        w(format!(
            "block {:#x} {live} {}",
            heap_base + block_addr,
            live * LINE_SIZE
        ));
    }
    w("end".into());

    drop(roots);
    let _ = out.flush();
}

// ── Fiber-stack registry (crate-internal, used by fiber.rs) ─────────────────

pub(crate) unsafe fn gc_register_fiber_stack(id: u32, base: usize, size: usize) {
    let thread = thread_self_fast();
    let mut gc = gc_locked();
    // Lazily register the main-stack descriptor the first time a fiber
    // appears, so mark_roots can scan the suspended main stack.
    if !gc
        .fiber_stacks
        .iter()
        .any(|f| f.thread == thread && f.id == 0)
    {
        gc.fiber_stacks.push(FiberStackInfo {
            thread,
            id: 0,
            base: 0,
            size: 0,
            saved_sp: 0,
        });
    }
    gc.fiber_stacks.push(FiberStackInfo {
        thread,
        id,
        base,
        size,
        saved_sp: 0,
    });
}

pub(crate) unsafe fn gc_update_fiber_sp(id: u32, sp: usize) {
    let thread = thread_self_fast();
    let mut gc = gc_locked();
    if let Some(f) = gc
        .fiber_stacks
        .iter_mut()
        .find(|f| f.id == id && (id != 0 || f.thread == thread))
    {
        f.saved_sp = sp;
    }
}

/// Must be called BEFORE the fiber's stack memory is freed.
pub(crate) unsafe fn gc_unregister_fiber_stack(id: u32) {
    let thread = thread_self_fast();
    let mut gc = gc_locked();
    gc.fiber_stacks
        .retain(|f| f.id != id || (id == 0 && f.thread != thread));
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

#[cfg(test)]
mod tests {
    /// The bump allocator and the sweep must agree where a line begins.
    ///
    /// One finds the boundary from the absolute address (`p & (LINE_SIZE-1)`)
    /// and the other from the offset into the heap, so they agree only while
    /// the base is a multiple of `LINE_SIZE`. Every platform with a real
    /// `mmap` gets that from page alignment; a platform served by the plain
    /// allocator gets whatever it asked for, and asking for too little cost a
    /// wasm build a live array's contents with no crash and no diagnostic.
    #[test]
    fn the_heap_base_is_line_aligned() {
        let heap = HeapMemory::new(BLOCK_SIZE * 4);
        let base = heap.as_ptr() as usize;
        assert_eq!(
            base % LINE_SIZE,
            0,
            "heap base {base:#x} is not a multiple of LINE_SIZE ({LINE_SIZE}); \
             the bump path and the sweep would disagree about line boundaries \
             by {} bytes",
            base % LINE_SIZE
        );
    }

    use super::*;
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::Arc;

    #[test]
    fn collector_rendezvous_with_registered_os_mutator() {
        unsafe { hlp_gc_init() };
        let main_stack_anchor = 0usize;
        unsafe {
            hlp_gc_set_stack_top(
                (&main_stack_anchor as *const usize as usize) + mem::size_of::<usize>(),
            )
        };

        let ready = Arc::new(AtomicBool::new(false));
        let finish = Arc::new(AtomicBool::new(false));
        let worker_ready = Arc::clone(&ready);
        let worker_finish = Arc::clone(&finish);
        let worker = std::thread::spawn(move || {
            let stack_anchor = 0usize;
            unsafe {
                hl_register_thread(
                    ((&stack_anchor as *const usize as usize) + mem::size_of::<usize>())
                        as *mut c_void,
                )
            };
            worker_ready.store(true, Ordering::Release);
            while !worker_finish.load(Ordering::Acquire) {
                gc_safepoint();
                std::hint::spin_loop();
            }
            unsafe { hl_unregister_thread() };
        });

        while !ready.load(Ordering::Acquire) {
            std::thread::yield_now();
        }
        {
            let mut gc = gc_locked();
            set_collect_origin(6);
            gc.collect_garbage();
        }
        finish.store(true, Ordering::Release);
        worker.join().unwrap();
        unsafe { hl_unregister_thread() };
    }

    /// The four `hl.Gc` reporting primitives, held to the contract their doc
    /// comments state. A Windows build reported
    /// `345 natives resolved, 1 missing: std@hlp_gc_stats` on a real program,
    /// so what these return is now something a shipped program reads.
    ///
    /// ONE test rather than four, for the reason `sys::tests` gives above its
    /// own single test: the collector is process-global and the harness runs
    /// separate `#[test]` functions on separate threads. Split up, the
    /// `gc_profile` case would be flipping a process-global flag word while
    /// the `gc_major` case collected, and each would see the other's writes.
    /// Here every assertion runs in sequence on one thread.
    ///
    /// That still leaves the other `#[test]` in this file collecting
    /// concurrently, so the body holds the GC lock for its whole length: no
    /// other thread can collect, or carve a block, between a pair of samples.
    /// The lock is reentrant, so each primitive's own `gc_locked_init` nests
    /// inside that hold rather than deadlocking — the same property
    /// [`hlp_gc_major`] documents — and a thread waiting on it parks at the
    /// safepoint in `ReentrantGcLock::acquire`, so holding it cannot starve
    /// the other test's stop-the-world.
    ///
    /// This thread registers as a mutator because the body keeps GC pointers
    /// in its own frame across a collection: only a registered mutator's
    /// stack is in the snapshot `mark_roots` scans.
    #[test]
    fn gc_reporting_prims_keep_their_documented_contract() {
        unsafe { hlp_gc_init() };
        let stack_anchor = 0usize;
        unsafe {
            hlp_gc_set_stack_top((&stack_anchor as *const usize as usize) + mem::size_of::<usize>())
        };

        // Both of these are process-global and have to be put back even when
        // an assertion below fails: a leaked Profile bit makes every later
        // collection in this binary print a census, and a mutator record left
        // behind by a finished thread makes the next stop-the-world wait for a
        // thread that will never park. The whole flag word is saved, not just
        // the Profile bit, because the body sets a neighbouring bit too, and
        // while it holds the GC lock nothing else in this binary writes them.
        let saved_flags = unsafe { hlp_gc_get_flags() };
        let outcome =
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(gc_reporting_prims_body));
        unsafe { hlp_gc_set_flags(saved_flags) };
        unsafe { hl_unregister_thread() };
        // The registered stack top points just past this local, so it has to
        // outlive every collection the body runs.
        std::hint::black_box(&stack_anchor);
        if let Err(payload) = outcome {
            std::panic::resume_unwind(payload);
        }
    }

    /// Sequenced body of
    /// [`gc_reporting_prims_keep_their_documented_contract`], split out so the
    /// caller restores the globals it touches on the unwind path too.
    ///
    /// `inline(never)` is load-bearing, not tidiness. A conservative scan
    /// covers `[sp, stack_top)`, so the registered anchor only bounds frames
    /// BELOW it — and an anchor is just one local among a frame's others.
    /// Inlined into the caller, this function's root array landed 24 bytes
    /// ABOVE the anchor and the cycle swept 7 of its 8 blocks. Its own frame
    /// is unconditionally below the caller's.
    #[inline(never)]
    fn gc_reporting_prims_body() {
        // 4KB is above `TLAB_MAX_OBJ`, so every chunk takes the locked path
        // and is charged its own bytes instead of 32KB at a time; 64 of them
        // is well under `INITIAL_TRIGGER_BYTES`, so the loop does not collect.
        const CHUNK: usize = 4096;
        const CHUNKS: usize = 64;
        const LIVE_BYTES: usize = CHUNK * CHUNKS;

        let gc = gc_locked_init();

        // ── hlp_gc_stats writes all three out-params ──────────────────────
        // NaN is a poison none of the three can produce, so `is_finite` is
        // proof the store happened rather than that the value looks plausible.
        let (mut total0, mut count0, mut current0) = (f64::NAN, f64::NAN, f64::NAN);
        unsafe { hlp_gc_stats(&mut total0, &mut count0, &mut current0) };
        assert!(
            total0.is_finite() && total0 >= 0.0,
            "total_allocated not written: {total0}"
        );
        assert!(count0.is_finite(), "allocation_count not written: {count0}");
        assert!(
            current0.is_finite() && current0 >= 0.0,
            "current_memory not written: {current0}"
        );

        // Documented as honestly unavailable: ash counts no allocations, and
        // the doc comment on `hlp_gc_stats` says why reporting
        // `heap.alloc_count` would be a worse answer than none — it shrinks at
        // every collection, so a caller differencing two samples would read
        // the allocation rate as negative. Asserted as the DOCUMENTED constant
        // so that implementing it for real fails here, and whoever does it
        // updates the contract on purpose.
        assert_eq!(
            count0, 0.0,
            "allocation_count is documented as a constant 0.0"
        );

        // ── NULL is legal for any out-param ───────────────────────────────
        // An HDLL caller that wants one field passes NULL for the others; that
        // must not be a store to address 0.
        unsafe {
            hlp_gc_stats(ptr::null_mut(), ptr::null_mut(), ptr::null_mut());
            let mut one = f64::NAN;
            hlp_gc_stats(&mut one, ptr::null_mut(), ptr::null_mut());
            assert!(one.is_finite(), "total_allocated alone: {one}");
            let mut one = f64::NAN;
            hlp_gc_stats(ptr::null_mut(), &mut one, ptr::null_mut());
            assert_eq!(one, 0.0, "allocation_count alone: {one}");
            let mut one = f64::NAN;
            hlp_gc_stats(ptr::null_mut(), ptr::null_mut(), &mut one);
            assert!(one.is_finite(), "current_memory alone: {one}");
        }

        // ── total_allocated across real allocation ────────────────────────
        // The pointers live in a stack array, not a Vec: conservative marking
        // scans this frame, and a Vec's buffer is on the malloc heap, which it
        // does not scan.
        let mut live = [ptr::null_mut::<u8>(); CHUNKS];
        for slot in live.iter_mut() {
            let p = gc_alloc(CHUNK).expect("GC heap exhausted allocating a test chunk");
            unsafe { p.as_ptr().write_bytes(0xA5, CHUNK) };
            *slot = p.as_ptr();
        }

        let (mut total1, mut count1, mut current1) = (f64::NAN, f64::NAN, f64::NAN);
        unsafe { hlp_gc_stats(&mut total1, &mut count1, &mut current1) };
        assert_eq!(
            count1, 0.0,
            "allocation_count is documented as a constant 0.0"
        );
        // The contract is non-decreasing: the counter is cumulative and never
        // reset. No exact byte total is asserted — the allocator charges in
        // 32KB regions on the TLAB path, and other threads add to the same
        // counter — only that this much allocation moved it at least this far.
        assert!(
            total1 >= total0,
            "total_allocated went backwards: {total0} -> {total1}"
        );
        assert!(
            total1 - total0 >= LIVE_BYTES as f64,
            "{LIVE_BYTES} bytes of above-TLAB_MAX_OBJ allocation moved total_allocated by only {}",
            total1 - total0
        );

        // ── current_memory: non-zero, block-granular, under the ceiling ───
        // The ceiling is the heap reservation itself; `used_blocks` is a
        // subset of it by construction, so a figure above it would mean the
        // count and the unit had come apart.
        let ceiling = gc.heap.memory.len as f64;
        assert!(
            current1 > 0.0,
            "current_memory is 0 with {LIVE_BYTES} bytes live"
        );
        assert!(
            current1 <= ceiling,
            "current_memory {current1} exceeds the heap reservation {ceiling}"
        );
        assert!(
            current1 >= LIVE_BYTES as f64,
            "current_memory {current1} is under the {LIVE_BYTES} bytes the heap is holding"
        );
        assert_eq!(
            current1 as usize % BLOCK_SIZE,
            0,
            "current_memory is used blocks, so it is a multiple of BLOCK_SIZE: {current1}"
        );

        // ── hlp_gc_major runs a real cycle ────────────────────────────────
        let collections_before = GC_STATS.collections.load(Ordering::Relaxed);
        unsafe { hlp_gc_major() };
        let collections_after = GC_STATS.collections.load(Ordering::Relaxed);
        assert!(
            collections_after > collections_before,
            "hlp_gc_major ran no cycle: collections {collections_before} -> {collections_after}"
        );
        // ...and it ran on its own account rather than riding a trigger that
        // happened to fire: 6 is `ORIGIN_NAMES`' "explicit", and nothing else
        // can have collected while this thread holds the lock.
        assert_eq!(
            COLLECT_ORIGIN.load(Ordering::Relaxed),
            6,
            "hlp_gc_major should collect with the explicit origin"
        );

        let (mut total2, mut count2, mut current2) = (f64::NAN, f64::NAN, f64::NAN);
        unsafe { hlp_gc_stats(&mut total2, &mut count2, &mut current2) };
        assert_eq!(
            count2, 0.0,
            "allocation_count is documented as a constant 0.0"
        );
        assert!(
            total2 >= total1,
            "a collection must not reset total_allocated: {total1} -> {total2}"
        );
        // Nothing can have carved a block while this thread holds the GC lock,
        // so a cycle can only hand blocks back.
        assert!(
            current2 <= current1,
            "current_memory grew across a collection: {current1} -> {current2}"
        );
        // And the chunks are still referenced from this frame on a registered
        // mutator, so the cycle keeps their blocks.
        assert!(
            current2 >= LIVE_BYTES as f64,
            "a collection dropped below the {LIVE_BYTES} still-reachable bytes: {current2}"
        );
        std::hint::black_box(&live);

        // ── hlp_gc_profile sets and clears exactly one bit ────────────────
        // The neighbouring bit is what makes this a test rather than a
        // tautology. Upstream's `hl_gc_profile(false)` writes
        // `gc_flags &= GC_PROFILE` — a missing `~` — which leaves profiling ON
        // and clears every OTHER flag. Ash clears the one bit instead, per the
        // note on `hlp_gc_profile`, so both halves are asserted: Profile goes
        // away, the neighbour stays.
        const NEIGHBOUR: i32 = 1 << 4; // a bit `hlp_gc_set_flags` stores without acting on
        unsafe { hlp_gc_set_flags(NEIGHBOUR) };
        assert_eq!(
            unsafe { hlp_gc_get_flags() } & GC_FLAG_PROFILE,
            0,
            "test setup left Profile set"
        );

        unsafe { hlp_gc_profile(true) };
        assert_ne!(
            unsafe { hlp_gc_get_flags() } & GC_FLAG_PROFILE,
            0,
            "gc_profile(true) did not set Profile"
        );
        assert_ne!(
            unsafe { hlp_gc_get_flags() } & NEIGHBOUR,
            0,
            "gc_profile(true) disturbed a flag other than Profile"
        );

        unsafe { hlp_gc_profile(false) };
        assert_eq!(
            unsafe { hlp_gc_get_flags() } & GC_FLAG_PROFILE,
            0,
            "gc_profile(false) left Profile set, which is upstream's missing `~`"
        );
        assert_ne!(
            unsafe { hlp_gc_get_flags() } & NEIGHBOUR,
            0,
            "gc_profile(false) cleared a flag other than Profile, which is upstream's `&= GC_PROFILE`"
        );

        // ── hlp_gc_get_live_objects cannot answer, and says so ────────────
        // -1 is upstream's "cannot answer". 0 is the wrong answer: it asserts
        // "none of this type are live", a claim about a heap ash cannot
        // enumerate, because nothing records an allocation's type and a marked
        // line is not an object address.
        let mut ty: hl_type = unsafe { mem::zeroed() };
        let mut arr: hl::varray = unsafe { mem::zeroed() };
        assert_eq!(
            unsafe { hlp_gc_get_live_objects(&mut ty, &mut arr) },
            -1,
            "gc_get_live_objects must report -1 (cannot answer), never 0 (none live)"
        );
        // `arr` is documented as left untouched, so it is never partially
        // filled behind a -1.
        assert_eq!(arr.size, 0, "gc_get_live_objects wrote into arr");
        // The arguments are not dereferenced, so a caller passing NULL for the
        // array it did not bother to allocate gets the same answer.
        assert_eq!(
            unsafe { hlp_gc_get_live_objects(ptr::null_mut(), ptr::null_mut()) },
            -1,
            "gc_get_live_objects must tolerate NULL arguments"
        );

        drop(gc);
    }
}
