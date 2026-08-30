//! Diagnostics that make the runtime describe itself.
//!
//! Kept apart from the interpreter proper because none of it is needed to run
//! a program: every item here exists to answer a question about a run that
//! went wrong, and each is gated behind its own environment variable so a
//! normal run pays nothing.

use std::ffi::c_void;
use std::sync::atomic::{AtomicBool, Ordering};

use anyhow::Result;
use ash_core::bytecode::DecodedBytecode;

use crate::values::NanBoxedValue;

use super::{HLInterpreter, NativeFunctionResolver};

/// Set by the stall watchdog; cleared by the interpreter once it has reported.
static STALL_PING: AtomicBool = AtomicBool::new(false);

/// Start the watchdog behind `ASH_STALL_LOG=<seconds>`, if asked.
///
/// A wedged run looks identical from a native sampler whether it is stuck or
/// merely slow -- the interpreter's frames carry no Haxe identity there. This
/// makes the interpreter name the function it is in.
pub(super) fn arm_stall_watchdog() {
    static ARMED: std::sync::OnceLock<()> = std::sync::OnceLock::new();
    let Some(secs) = std::env::var("ASH_STALL_LOG")
        .ok()
        .and_then(|v| v.parse::<u64>().ok())
    else {
        return;
    };
    ARMED.get_or_init(|| {
        let _ = std::thread::Builder::new()
            .name("ash-stall-watchdog".into())
            .spawn(move || loop {
                std::thread::sleep(std::time::Duration::from_secs(secs.max(1)));
                STALL_PING.store(true, Ordering::Relaxed);
            });
    });
}

/// Tells the collector this thread will not reach a safepoint for a while.
///
/// Preparing a body runs the AIR pipeline -- lower, inline, verify, build a
/// dominator tree -- on whichever thread first calls the function, and there
/// is no poll anywhere in it. A fiber worker doing that held every world stop
/// open for its whole duration: 150-350ms on MBHaxe, sampled to
/// `Inlining::is_stack_sensitive_inner` and `lower_with`. The pipeline builds
/// Rust structures over bytecode and touches no GC object, and `hlp_blocking`
/// publishes the stack pointer and callee-saved registers first, so the
/// interpreter frames underneath stay conservatively scannable throughout.
pub(super) struct CompileBlocking(*mut c_void);

impl CompileBlocking {
    pub(super) fn enter(f: *mut c_void) -> Self {
        if !f.is_null() {
            type FnBlocking = unsafe extern "C" fn(bool);
            unsafe { (std::mem::transmute::<*mut c_void, FnBlocking>(f))(true) };
        }
        Self(f)
    }
}

impl Drop for CompileBlocking {
    fn drop(&mut self) {
        if !self.0.is_null() {
            type FnBlocking = unsafe extern "C" fn(bool);
            unsafe { (std::mem::transmute::<*mut c_void, FnBlocking>(self.0))(false) };
        }
    }
}

impl HLInterpreter {
    /// Print this interpreter's own Haxe stack when the watchdog asks.
    ///
    /// The interpreter reports itself rather than the watchdog reading it:
    /// the frame stack is owned by this thread and racing another one on it
    /// would be undefined. Polled once every 4096 calls, so with the watchdog
    /// disarmed this costs an increment and a predictable branch.
    pub(super) fn report_stall_if_asked(&mut self, bytecode: &DecodedBytecode) {
        self.stall_tick = self.stall_tick.wrapping_add(1);
        if self.stall_tick & 0xFFF != 0 || !STALL_PING.swap(false, Ordering::Relaxed) {
            return;
        }
        // Throughput as well as position: a main loop that is turning slowly
        // and one that is turning fast but rendering nothing look identical
        // from the stack alone.
        let now = std::time::Instant::now();
        let steps = self.stall_tick.wrapping_sub(self.stall_tick_reported) as f64;
        let secs = now.duration_since(self.stall_reported_at).as_secs_f64();
        self.stall_tick_reported = self.stall_tick;
        self.stall_reported_at = now;
        let stack = self.capture_call_stack(bytecode);
        eprintln!(
            "[stall] {:.0} dispatch steps/s over {:.1}s; stack innermost first, {} frames:",
            if secs > 0.0 { steps / secs } else { 0.0 },
            secs,
            stack.len()
        );
        for frame in stack.iter().take(30) {
            eprintln!("[stall]   {frame}");
        }
    }

    /// The interpreted call stack as HashLink reports it: innermost first,
    /// `Class.method(file:line)` per frame, using the debug info the bytecode
    /// already carries.
    /// Time each native and report the slow ones, when `ASH_SLOW_NATIVE_MS`
    /// asks.
    ///
    /// A native runs with no safepoint poll in it, so one that takes long
    /// enough holds up every world stop for its whole duration. The collector
    /// can say a thread reached a safepoint late but not what it was doing
    /// before it got there, and a sampler catches only what it happens to
    /// land on. This names the call and its cost directly.
    pub(super) fn call_native(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        native_idx: usize,
        args: &[NanBoxedValue],
    ) -> Result<NanBoxedValue> {
        static LIMIT: std::sync::OnceLock<Option<u128>> = std::sync::OnceLock::new();
        let limit = *LIMIT.get_or_init(|| {
            std::env::var("ASH_SLOW_NATIVE_MS")
                .ok()
                .and_then(|v| v.parse::<u128>().ok())
        });
        let Some(limit) = limit else {
            return self.call_native_inner(bytecode, native_resolver, native_idx, args);
        };
        let started = std::time::Instant::now();
        let out = self.call_native_inner(bytecode, native_resolver, native_idx, args);
        let took = started.elapsed();
        if took.as_millis() >= limit {
            let native = &bytecode.natives[native_idx];
            eprintln!(
                "[slow-native] hlp_{} lib={} took {:.1}ms on {}",
                native.name,
                native.lib,
                took.as_secs_f64() * 1e3,
                std::thread::current().name().unwrap_or("main"),
            );
        }
        out
    }
}
