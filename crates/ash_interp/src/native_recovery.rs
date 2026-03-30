//! Native call crash recovery via sigsetjmp/siglongjmp.
//!
//! When a native FFI call triggers SIGSEGV/SIGBUS (e.g., macOS GL driver bugs),
//! the signal handler can siglongjmp back to the call site instead of crashing.
//! The interpreter then returns a default value and continues execution.

use std::ptr::addr_of_mut;
use std::sync::atomic::{AtomicBool, Ordering};

/// sigjmp_buf on aarch64-apple-darwin is [c_int; 49]
#[cfg(all(target_arch = "aarch64", target_os = "macos"))]
pub type SigJmpBuf = [i32; 49];

/// sigjmp_buf on x86_64-apple-darwin is [c_int; 38] (actually 37+1 for save mask)
#[cfg(all(target_arch = "x86_64", target_os = "macos"))]
pub type SigJmpBuf = [i32; 38];

/// Fallback for other platforms
#[cfg(not(target_os = "macos"))]
pub type SigJmpBuf = [i32; 64]; // generous

extern "C" {
    pub fn sigsetjmp(buf: *mut i32, savemask: i32) -> i32;
    pub fn siglongjmp(buf: *mut i32, val: i32) -> !;
}

/// Whether a native call recovery point is currently active.
pub static NATIVE_RECOVERY_ACTIVE: AtomicBool = AtomicBool::new(false);

/// The sigjmp_buf for native call recovery.
/// Only accessed from the main thread (single-threaded VM execution).
#[cfg(all(target_arch = "aarch64", target_os = "macos"))]
static mut NATIVE_RECOVERY_BUF: SigJmpBuf = [0; 49];
#[cfg(all(target_arch = "x86_64", target_os = "macos"))]
static mut NATIVE_RECOVERY_BUF: SigJmpBuf = [0; 38];
#[cfg(not(target_os = "macos"))]
static mut NATIVE_RECOVERY_BUF: SigJmpBuf = [0; 64];

/// The signal number that triggered recovery (for diagnostics).
static mut NATIVE_RECOVERY_SIGNAL: i32 = 0;

/// The fault address that triggered recovery (for diagnostics).
static mut NATIVE_RECOVERY_FAULT_ADDR: usize = 0;

/// Check if recovery is active and perform siglongjmp if so.
/// Called from the signal handler. Returns true if recovery was performed
/// (in which case the signal handler should NOT re-raise).
///
/// # Safety
/// Must only be called from a signal handler context on the main thread.
pub unsafe fn try_recover_from_signal(sig: i32, fault_addr: usize) -> bool {
    if NATIVE_RECOVERY_ACTIVE.load(Ordering::Relaxed) {
        NATIVE_RECOVERY_ACTIVE.store(false, Ordering::Relaxed);
        NATIVE_RECOVERY_SIGNAL = sig;
        NATIVE_RECOVERY_FAULT_ADDR = fault_addr;
        siglongjmp(addr_of_mut!(NATIVE_RECOVERY_BUF) as *mut i32, 1);
        // unreachable - siglongjmp never returns
    }
    false
}

/// Arm the recovery point. Returns 0 on initial call, non-zero on recovery.
///
/// # Safety
/// Must be called on the main thread. The recovery point is only valid
/// for the duration of the current stack frame.
pub unsafe fn arm_native_recovery() -> i32 {
    let result = sigsetjmp(addr_of_mut!(NATIVE_RECOVERY_BUF) as *mut i32, 1);
    if result == 0 {
        NATIVE_RECOVERY_ACTIVE.store(true, Ordering::Relaxed);
    }
    result
}

/// Disarm the recovery point after a successful native call.
pub fn disarm_native_recovery() {
    NATIVE_RECOVERY_ACTIVE.store(false, Ordering::Relaxed);
}

/// Get the signal number from the last recovery.
pub fn last_recovery_signal() -> i32 {
    unsafe { NATIVE_RECOVERY_SIGNAL }
}

/// Get the fault address from the last recovery.
pub fn last_recovery_fault_addr() -> usize {
    unsafe { NATIVE_RECOVERY_FAULT_ADDR }
}
