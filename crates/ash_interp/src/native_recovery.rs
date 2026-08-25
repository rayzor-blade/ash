//! Native call crash recovery via sigsetjmp/siglongjmp.
//!
//! When a native FFI call triggers SIGSEGV/SIGBUS (e.g., macOS GL driver bugs),
//! the signal handler can siglongjmp back to the call site instead of crashing.
//! The interpreter then returns a default value and continues execution.
//!
//! Recovery is THREAD-GATED: each recovery slot records the pthread that armed
//! it, and the signal handler only jumps into a slot owned by the faulting
//! thread. Without this gate, a fault on the tiered-JIT broker thread while
//! the main thread had recovery armed would siglongjmp the BROKER onto the
//! MAIN thread's saved stack context — two threads executing the same stack,
//! instant corruption (observed as SIGSEGV "in hlp_win_swap_window" followed
//! by a fatal SIGBUS at a garbage address).
//!
//! Two slots exist: SLOT_INTERP for the interpreter's native calls (main
//! thread) and SLOT_TIERED for the beadie broker's compile jobs.

#[cfg(unix)]
use std::cell::UnsafeCell;
#[cfg(unix)]
use std::sync::atomic::{AtomicBool, AtomicI32, AtomicUsize, Ordering};

/// Backing store for a `sigjmp_buf`.
///
/// One generously-sized, pointer-aligned buffer for every platform rather than
/// a per-target size, because `sigjmp_buf` is a libc internal and guessing it
/// per (arch, libc) is how you get a silent stack smash. Known sizes:
///
/// | target                  | bytes |
/// |-------------------------|-------|
/// | aarch64-apple-darwin    |   196 |
/// | x86_64-apple-darwin     |   152 |
/// | x86_64 linux-gnu        |   200 |
/// | aarch64 linux-gnu       |   312 |
///
/// 512 bytes covers all of them with room to spare; the cost is two slots.
///
/// The element type is `u64`, not `i32`: `sigsetjmp` saves registers and
/// pointers, so the buffer must be at least pointer-aligned. An `[i32; N]`
/// alias is only 4-byte aligned and left correct alignment to chance — it
/// happened to work on Darwin because neighbouring atomics pushed the field
/// to an 8-byte offset.
#[cfg(unix)]
pub type SigJmpBuf = [u64; 64];

#[cfg(unix)]
const SIGJMP_BUF_ZERO: SigJmpBuf = [0; 64];

#[cfg(unix)]
extern "C" {
    /// glibc does **not** export a `sigsetjmp` symbol: `<setjmp.h>` defines it
    /// as a macro over `__sigsetjmp`, so an `extern "C" { fn sigsetjmp }`
    /// declaration fails to link on linux-gnu with an undefined reference.
    /// Darwin, musl and bionic all export the plain name.
    #[cfg_attr(
        all(target_os = "linux", target_env = "gnu"),
        link_name = "__sigsetjmp"
    )]
    pub fn sigsetjmp(buf: *mut u64, savemask: i32) -> i32;
    pub fn siglongjmp(buf: *mut u64, val: i32) -> !;
}

/// One recovery point: a jump buffer plus the pthread that armed it.
#[cfg(unix)]
struct RecoverySlot {
    active: AtomicBool,
    /// pthread_t of the arming thread (0 = never armed).
    owner: AtomicUsize,
    signal: AtomicI32,
    fault_addr: AtomicUsize,
    buf: UnsafeCell<SigJmpBuf>,
}

// The buf is only written by the owning thread (arm) and only read by the
// signal handler running ON that same thread (the handler checks `owner`
// against pthread_self before touching it).
#[cfg(unix)]
unsafe impl Sync for RecoverySlot {}

#[cfg(unix)]
impl RecoverySlot {
    const fn new() -> Self {
        RecoverySlot {
            active: AtomicBool::new(false),
            owner: AtomicUsize::new(0),
            signal: AtomicI32::new(0),
            fault_addr: AtomicUsize::new(0),
            buf: UnsafeCell::new(SIGJMP_BUF_ZERO),
        }
    }
}

/// Interpreter native-call recovery (main thread).
#[cfg(unix)]
pub const SLOT_INTERP: usize = 0;
/// Tiered-JIT broker compile recovery (beadie broker thread).
#[cfg(unix)]
pub const SLOT_TIERED: usize = 1;

#[cfg(unix)]
static SLOTS: [RecoverySlot; 2] = [RecoverySlot::new(), RecoverySlot::new()];

#[cfg(unix)]
#[inline]
fn current_thread_id() -> usize {
    // pthread_self is async-signal-safe on the platforms we target.
    unsafe { libc::pthread_self() as usize }
}

/// Check if a recovery point armed BY THE FAULTING THREAD is active and
/// perform siglongjmp if so. Called from the signal handler. Returns true if
/// recovery was performed (in which case the handler should NOT re-raise).
///
/// # Safety
/// Must only be called from a signal handler context.
#[cfg(unix)]
pub unsafe fn try_recover_from_signal(sig: i32, fault_addr: usize) -> bool {
    let me = current_thread_id();
    for slot in SLOTS.iter() {
        if slot.active.load(Ordering::Relaxed) && slot.owner.load(Ordering::Relaxed) == me {
            slot.active.store(false, Ordering::Relaxed);
            slot.signal.store(sig, Ordering::Relaxed);
            slot.fault_addr.store(fault_addr, Ordering::Relaxed);
            siglongjmp(slot.buf.get() as *mut u64, 1);
            // unreachable - siglongjmp never returns
        }
    }
    false
}

#[inline(always)]
#[cfg(unix)]
unsafe fn arm_slot(index: usize) -> i32 {
    let slot = &SLOTS[index];
    let result = sigsetjmp(slot.buf.get() as *mut u64, 1);
    if result == 0 {
        slot.owner.store(current_thread_id(), Ordering::Relaxed);
        slot.active.store(true, Ordering::Relaxed);
    }
    result
}

#[inline]
#[cfg(unix)]
fn disarm_slot(index: usize) {
    SLOTS[index].active.store(false, Ordering::Relaxed);
}

/// Arm the interpreter recovery point. Returns 0 on initial call, non-zero on
/// recovery.
///
/// # Safety
/// The recovery point is only valid for the duration of the current stack
/// frame.
#[inline(always)]
#[cfg(unix)]
pub unsafe fn arm_native_recovery() -> i32 {
    arm_slot(SLOT_INTERP)
}

/// Disarm the interpreter recovery point after a successful native call.
#[cfg(unix)]
pub fn disarm_native_recovery() {
    disarm_slot(SLOT_INTERP)
}

/// Get the signal number from the last interpreter recovery.
#[cfg(unix)]
pub fn last_recovery_signal() -> i32 {
    SLOTS[SLOT_INTERP].signal.load(Ordering::Relaxed)
}

/// Get the fault address from the last interpreter recovery.
#[cfg(unix)]
pub fn last_recovery_fault_addr() -> usize {
    SLOTS[SLOT_INTERP].fault_addr.load(Ordering::Relaxed)
}

/// Arm the tiered-broker recovery point. Returns 0 on initial call, non-zero
/// on recovery.
///
/// # Safety
/// Must be called on the broker thread; valid for the current stack frame.
#[inline(always)]
#[cfg(unix)]
pub unsafe fn arm_tiered_recovery() -> i32 {
    arm_slot(SLOT_TIERED)
}

/// Disarm the tiered-broker recovery point after a successful compile.
#[cfg(unix)]
pub fn disarm_tiered_recovery() {
    disarm_slot(SLOT_TIERED)
}

/// Get the signal number from the last tiered-broker recovery.
#[cfg(unix)]
pub fn last_tiered_recovery_signal() -> i32 {
    SLOTS[SLOT_TIERED].signal.load(Ordering::Relaxed)
}

/// Get the fault address from the last tiered-broker recovery.
#[cfg(unix)]
pub fn last_tiered_recovery_fault_addr() -> usize {
    SLOTS[SLOT_TIERED].fault_addr.load(Ordering::Relaxed)
}

// Native-call recovery is Unix signal machinery. Windows must not attempt to
// longjmp through JIT frames: Win64 longjmp performs an SEH unwind and those
// frames do not carry the required unwind metadata. Keep the call-site API
// intact while allowing access violations to follow the normal Windows
// unhandled-exception path until a CONTEXT-based recovery mechanism exists.
#[cfg(not(unix))]
pub unsafe fn try_recover_from_signal(_sig: i32, _fault_addr: usize) -> bool {
    false
}

#[cfg(not(unix))]
pub unsafe fn arm_native_recovery() -> i32 {
    0
}

#[cfg(not(unix))]
pub fn disarm_native_recovery() {}

#[cfg(not(unix))]
pub fn last_recovery_signal() -> i32 {
    0
}

#[cfg(not(unix))]
pub fn last_recovery_fault_addr() -> usize {
    0
}

#[cfg(not(unix))]
pub unsafe fn arm_tiered_recovery() -> i32 {
    0
}

#[cfg(not(unix))]
pub fn disarm_tiered_recovery() {}

#[cfg(not(unix))]
pub fn last_tiered_recovery_signal() -> i32 {
    0
}

#[cfg(not(unix))]
pub fn last_tiered_recovery_fault_addr() -> usize {
    0
}
