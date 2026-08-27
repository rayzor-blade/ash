//! Interpreter re-entry bridge for stub-sentinel calls from JIT code.
//!
//! In hybrid mode the interpreter fills shared `module_ctx` function-pointer
//! slots (functions_ptrs, vtables built from them, closure `fun` fields) with
//! STUB SENTINELS: `findex + 1`, always `< 0x100000`. Interpreted code decodes
//! them; native/JIT code calling one crashes (the observed deterministic
//! SIGBUS at fault_addr = findex+1 on game.hl right after tier promotion).
//!
//! Every indirect call site the JIT emits therefore guards the loaded pointer
//! (see `build_stub_guarded_indirect_call` in jit/function.rs): when it is a
//! sentinel, the compiled code spills its arguments to an i64 buffer and calls
//! [`ash_jit_call_stub`] instead, which forwards to a bridge the interpreter
//! registered at startup ([`set_stub_call_bridge`]). The bridge re-enters the
//! interpreter for that findex, marshaling raw words per the callee's declared
//! bytecode signature, and returns the result as a raw word.
//!
//! Word encoding contract (both directions, low bits significant):
//! - ints/bools: value in the low bits (encoder zero-extends, decoder
//!   truncates to the declared width)
//! - f64/f32: f64 bit pattern (f32 is extended to f64 before bitcasting)
//! - pointers: the address itself

use std::sync::atomic::{AtomicUsize, Ordering};

/// Highest address treated as an interpreter stub sentinel (exclusive).
/// Matches the interpreter's `findex + 1` encoding and the existing checks in
/// std/src/fiber.rs and the interpreter's closure runner.
pub const STUB_SENTINEL_LIMIT: u64 = 0x100000;

/// Bridge signature: (callee findex, caller findex, raw arg words, arg count)
/// -> raw result word. The caller travels explicitly because a cold stub call
/// crosses Rust frames that native stack unwinders cannot associate with the
/// generated-code caller.
pub type StubCallBridge =
    unsafe extern "C" fn(findex: i32, caller: i32, args: *const i64, nargs: i32) -> i64;

static STUB_CALL_BRIDGE: AtomicUsize = AtomicUsize::new(0);

/// Register the interpreter's re-entry bridge. Called once at startup by the
/// hybrid host (ash_interp) before any tiered code can run.
pub fn set_stub_call_bridge(f: StubCallBridge) {
    STUB_CALL_BRIDGE.store(f as usize, Ordering::Release);
}

/// Called by JIT-compiled code when an indirect call target turned out to be
/// an interpreter stub sentinel. `sentinel` is the raw pointer value
/// (findex + 1). Never inlined into JIT code — its address is embedded as an
/// inttoptr constant.
///
/// # Safety
/// `args` must point to `nargs` i64 words encoded per the module contract.
#[no_mangle]
pub unsafe extern "C" fn ash_jit_call_stub(
    sentinel: i64,
    caller: i32,
    args: *const i64,
    nargs: i32,
) -> i64 {
    let bridge = STUB_CALL_BRIDGE.load(Ordering::Acquire);
    if bridge == 0 {
        // Pure-JIT mode never stores sentinels in function tables; reaching
        // this without a registered bridge is a real bug — fail loudly
        // instead of the silent SIGBUS the sentinel call would have been.
        eprintln!(
            "[ash] FATAL: JIT code called stub sentinel {:#x} (findex {}) but no \
             interpreter bridge is registered",
            sentinel,
            sentinel - 1
        );
        std::process::abort();
    }
    let f: StubCallBridge = std::mem::transmute(bridge as *const ());
    f((sentinel - 1) as i32, caller, args, nargs)
}
