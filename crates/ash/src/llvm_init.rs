//! LLVM's static constructors, for an executable that defers them.
//!
//! The `ash` executable links LLVM statically, and LLVM registers its
//! options, passes and targets from hundreds of static constructors. Run at
//! process start they make megabytes of code and data resident in a run that
//! never compiles anything with LLVM. So on Linux the executable's link moves
//! them out of `.init_array` (crates/ash_cli/llvm-init.ld), registers a
//! function that runs them with [`set_deferred`], and every path into LLVM
//! calls [`ensure`] first.
//!
//! In any other build nothing is registered: the constructors ran at startup
//! as usual and [`ensure`] does nothing.

use std::sync::Once;
use std::sync::atomic::{AtomicPtr, Ordering};

static RUNNER: AtomicPtr<()> = AtomicPtr::new(std::ptr::null_mut());
static DONE: Once = Once::new();

/// Register the function that runs LLVM's deferred constructors. The
/// executable calls this before anything can reach LLVM.
pub fn set_deferred(run: unsafe extern "C" fn()) {
    RUNNER.store(run as *mut (), Ordering::Release);
}

/// Run LLVM's deferred constructors if they have not run yet. Call before
/// any use of LLVM; cheap after the first call.
pub fn ensure() {
    DONE.call_once(|| {
        let run = RUNNER.load(Ordering::Acquire);
        if !run.is_null() {
            unsafe { std::mem::transmute::<*mut (), unsafe extern "C" fn()>(run)() };
        }
    });
}
