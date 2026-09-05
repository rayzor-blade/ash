//! The wasm side of the runtime: what gets compiled INTO the program.
//!
//! `ash_std` depends on this module when it is built for wasm, so everything
//! here is ordinary Rust linkage rather than a wasm import -- which is the
//! point. The only things that have to cross the module boundary are the ones
//! a sandbox genuinely cannot perform, and there is currently exactly one of
//! those: suspending a fiber.
//!
//! # Fibers on a target that cannot switch its own stack
//!
//! A fiber suspends in the middle of a call and resumes there later, which on
//! a native target means saving a stack and jumping to another one. A wasm
//! module cannot do that: it has no addressable stack, and no instruction that
//! moves between two. The capability exists one level up, in the host --
//! JavaScript Promise Integration in a browser, `wasmtime`'s async support on
//! a server, an explicit scheduler anywhere else -- so this module keeps the
//! same interface `krio-fiber` gives a native build and routes the one
//! operation that must suspend to the host. `ash_std`'s scheduler is
//! unchanged and does not know which backend it has.
//!
//! That single operation is [`yield_now`], which calls the import
//! `ash_host_fiber_yield`. A host that can suspend does so there, and the
//! engine preserves the module's stack while other fibers run; from inside,
//! `yield_now` is a call that takes a while to return. A host that cannot
//! suspend returns immediately, and fibers then run to completion in the
//! order they were started -- cooperative scheduling with one scheduling
//! point, which is a limitation of that host rather than of the program.
//!
//! The host contract is one function:
//!
//! ```text
//! (import "env" "ash_host_fiber_yield" (func))
//! ```
//!
//! # Why one import rather than a topology
//!
//! There are three ways a host can make that call suspend, and they are not
//! interchangeable, so the module declines to choose:
//!
//! * **Engine suspension** -- JavaScript Promise Integration in a browser,
//!   `wasmtime`'s async support on a server. The engine saves the wasm stack
//!   and returns to the scheduler; nothing is shared, nothing is parallel, and
//!   a fiber costs what a fiber should. This is the cheapest arrangement where
//!   the engine has it.
//! * **A worker per fiber over shared memory.** Each fiber runs in its own
//!   worker with its own stack, all of them sharing one linear memory, and
//!   `yield_now` parks the worker on `Atomics.wait` until the scheduler wakes
//!   it. This suspends a real stack with no engine feature beyond threads,
//!   which is its attraction. Its costs are equally real: it needs shared
//!   memory, so `wasm32-wasip1-threads` and, in a browser, COOP/COEP headers
//!   on every response; a fiber becomes an OS thread, which a scheduler
//!   holding thousands of them cannot afford; and every collection becomes a
//!   rendezvous across workers.
//! * **Asyncify** -- a whole-module transform that unwinds and rewinds the
//!   stack through linear memory. It works on any engine and costs roughly
//!   double the code size, plus a tax on every call, whether or not it ever
//!   suspends.
//!
//! Which is right depends on where the module runs, and that is the harness's
//! knowledge rather than the program's. So the program marks the point at
//! which it can be suspended, and the harness decides how. What a host must
//! NOT do is return from a suspension on a different instance or with a
//! different memory.

use std::cell::Cell;
use std::ffi::c_void;

/// The state values the link-time transform uses, which are Asyncify's.
const UNWINDING: i32 = 1;

/// Bytes at the base of a fiber's side stack holding `{current, end}`, which
/// is what the transform's data global points at.
const HEADER: usize = 8;

/// Side stack for one fiber when the caller does not ask for a size.
///
/// A frame is the fiber's locals plus a word, so this is thousands of frames
/// deep. It is charged per fiber, so it is not free.
const DEFAULT_SIDE_STACK: usize = 256 * 1024;

/// Shadow stack for one fiber, taken from the same buffer as its side stack.
///
/// The module gets exactly one shadow stack from the linker, and a suspended
/// fiber's frames stay allocated on it. Two fibers on one pointer therefore
/// collide: the frames between the suspend and the scheduler return, restore
/// the pointer above the suspended frames, and the next allocation writes
/// over them. So each fiber brings its own region and the pointer is swapped
/// on the way in and out.
const SHADOW_STACK: usize = 64 * 1024;

/// Where a fiber is in its life, matching `krio_fiber::FiberState` so the
/// scheduler above does not care which backend it has.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FiberState {
    Ready,
    Running,
    Suspended,
    Done,
    Errored,
}

// An import, not an undefined symbol. Without naming the module the linker
// treats it as something that ought to be defined somewhere and fails; with
// it, the function becomes `(import "env" "ash_host_fiber_yield")`, which is
// exactly what a host binds.
#[link(wasm_import_module = "env")]
extern "C" {
    /// Suspend the running fiber until the host resumes it.
    fn ash_host_fiber_yield();
    /// What the transform's state global says: running, unwinding or
    /// rewinding.
    ///
    /// The guest cannot read that global directly. It is added to the module
    /// after this code has been compiled and linked, so there is no name here
    /// to refer to it by -- but the host holds the instance and can.
    fn ash_host_fiber_state() -> i32;
    /// Point the transform at `data` as the side stack, and set the state.
    ///
    /// A non-zero `rewind` starts a rewind, so the next call into an
    /// instrumented function walks back down to where it suspended; zero puts
    /// the state back to running, which is also how an unwind is cleared once
    /// the scheduler has seen it.
    ///
    /// `sp` is this fiber's shadow stack pointer, swapped in for the duration;
    /// what comes back is the pointer that was there, which is the caller's on
    /// the way in and the fiber's own on the way out. Zero leaves it alone.
    fn ash_host_fiber_arm(data: *mut c_void, rewind: i32, sp: i32) -> i32;
}

/// Where an unwind stops.
///
/// The transform instruments a function so it can unwind out of a call, and
/// an unwind travels exactly as far as that instrumentation does: a function
/// without it sees the callee return and carries on with its own locals
/// untouched. This function is named to the linker as the one deliberately
/// left out, so the unwind ends here and [`Fiber::resume`] -- and the whole
/// scheduler above it -- is still running afterwards, free to pick another
/// fiber.
///
/// It has a stable name because that is how the linker finds it; nothing else
/// about it matters.
///
/// # Safety
///
/// `body` must be callable with `arg`, and must tolerate being called a
/// second time to rewind: on that pass the instrumentation walks it back to
/// where it stopped rather than running it again from the top.
/// `inline(never)` is load-bearing, not a hint. The body is one call, so it
/// inlines into `resume` given the chance, and then there is no frame here to
/// be the edge and nothing left for the linker to find by name -- the unwind
/// would run straight through the scheduler and out of the guest.
#[inline(never)]
#[no_mangle]
pub unsafe extern "C" fn ash_fiber_enter(body: extern "C" fn(*mut c_void), arg: *mut c_void) {
    body(arg)
}

thread_local! {
    /// Whether a host yield has ever been attempted. A host that does not
    /// provide the import traps on the first call; after that the module
    /// stops asking, so a program that yields in a loop still finishes.
    static HOST_YIELD_USABLE: Cell<bool> = const { Cell::new(true) };
}

/// Give the host a chance to run something else.
///
/// Returns immediately when the host offers no suspension, which is what
/// makes a scheduler built on this degrade to run-to-completion rather than
/// deadlock.
pub fn yield_now() {
    if !HOST_YIELD_USABLE.with(|u| u.get()) {
        return;
    }
    // Safety: the import takes no arguments, returns nothing, and either
    // suspends this instance or returns immediately. A host that cannot
    // suspend binds it to a stub; a module linked without it does not load,
    // which is why a harness must bind it even when it does nothing.
    unsafe { ash_host_fiber_yield() };
}

/// Record that the host cannot suspend, so later yields stop trying.
pub fn mark_host_yield_unavailable() {
    HOST_YIELD_USABLE.with(|u| u.set(false));
}

/// A unit of work the host schedules.
///
/// Its closure runs inside [`Fiber::resume`]. Where the module has been
/// instrumented by the link-time transform, a call to [`yield_now`] inside
/// that closure unwinds every frame back to [`ash_fiber_enter`] and `resume`
/// returns with the fiber `Suspended`; the next `resume` rewinds it to
/// exactly where it stopped. Where it has not, the host's yield returns
/// immediately and the closure runs to the end, which is the same interface
/// with one scheduling point instead of many.
pub struct Fiber {
    /// `FnMut` rather than `FnOnce` because a rewind calls it again: the
    /// instrumentation walks the second call back down to the suspend point
    /// instead of running it from the top, so the body has to survive being
    /// entered more than once.
    body: Option<Box<dyn FnMut()>>,
    state: FiberState,
    /// This fiber's memory: `{current, end}`, then the side stack the
    /// transform saves frames into growing up, then this fiber's shadow stack
    /// growing down from the top. One allocation so the collector has one
    /// range to scan, and so a side stack that runs into the shadow stack is
    /// caught by the bounds check the transform already emits.
    ///
    /// Its address is what the transform's data global is pointed at, so it
    /// must not move while the fiber is suspended -- the heap buffer does not
    /// move when the `Vec` or the `Fiber` does.
    stack: Vec<u8>,
    /// Where this fiber's shadow stack pointer was when it last stopped, or
    /// the top of its region if it has not started.
    shadow_sp: i32,
    /// Whether the body has been entered, and so whether the next entry is a
    /// rewind rather than a start.
    started: bool,
}

/// Call a boxed closure through a plain function pointer.
///
/// # Safety
///
/// `arg` must be a live `*mut Box<dyn FnMut()>`.
extern "C" fn call_boxed(arg: *mut c_void) {
    // Safety: `resume` passes a pointer to its own `body`, which outlives the
    // call, and nothing else holds a reference to it meanwhile.
    let body = unsafe { &mut *(arg as *mut Box<dyn FnMut()>) };
    body();
}

impl Fiber {
    /// The stack size is the SIDE stack: where the transform saves a
    /// suspended frame's locals. It is not the wasm call stack, which the
    /// engine owns and a module cannot size from the inside.
    pub fn with_stack_size<F>(stack_size: usize, f: F) -> Self
    where
        F: FnMut() + 'static,
    {
        let bytes = if stack_size == 0 {
            DEFAULT_SIDE_STACK
        } else {
            stack_size
        };
        let stack = vec![0u8; HEADER + bytes + SHADOW_STACK];
        // The shadow stack grows down from the very top of the buffer.
        let shadow_sp = (stack.as_ptr() as usize + stack.len()) as i32;
        Self {
            body: Some(Box::new(f)),
            state: FiberState::Ready,
            stack,
            shadow_sp,
            started: false,
        }
    }

    pub fn state(&self) -> FiberState {
        self.state
    }

    /// Run the fiber's body, or rewind it to where it suspended.
    ///
    /// Returns as soon as the body suspends, leaving the fiber `Suspended`;
    /// the scheduler above is then free to resume another. A panic inside the
    /// body leaves it `Errored` rather than unwinding into the scheduler,
    /// which is what the native backend does too.
    pub fn resume(&mut self) {
        if matches!(self.state, FiberState::Done | FiberState::Errored) {
            return;
        }
        let Some(body) = self.body.as_mut() else {
            self.state = FiberState::Done;
            return;
        };
        let body: *mut Box<dyn FnMut()> = body;

        let base = self.stack.as_mut_ptr();
        if !self.started {
            // `current` starts just past the header and grows up; `end` is
            // where a push would overflow, which the transform checks in the
            // push rather than at the API edge.
            let start = base as usize + HEADER;
            // `end` stops the side stack where the shadow stack begins, so
            // one running into the other is the overflow the transform's own
            // bounds check reports rather than silent corruption.
            let end = base as usize + self.stack.len() - SHADOW_STACK;
            // Safety: `stack` is at least HEADER bytes and correctly aligned
            // for two `u32`s, being a fresh allocation.
            unsafe {
                let header = base as *mut u32;
                header.write(start as u32);
                header.add(1).write(end as u32);
            }
        }
        self.state = FiberState::Running;
        // Safety: the imports are the host's, and `base` stays valid for as
        // long as this fiber does. What comes back is the caller's shadow
        // stack pointer, to put back when this fiber stops.
        let caller_sp = unsafe {
            ash_host_fiber_arm(base as *mut c_void, i32::from(self.started), self.shadow_sp)
        };

        let outcome = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            // Safety: `body` points at this fiber's own closure, and
            // `ash_fiber_enter` only calls it.
            unsafe { ash_fiber_enter(call_boxed, body as *mut c_void) }
        }));

        // Safety: reading the host's view of the transform's state.
        let unwound = unsafe { ash_host_fiber_state() } == UNWINDING;
        // Put the state back before anything else runs, or the next
        // instrumented call would think it too was unwinding -- and give the
        // caller its shadow stack back, keeping this fiber's for the resume.
        self.shadow_sp = unsafe { ash_host_fiber_arm(std::ptr::null_mut(), 0, caller_sp) };

        self.state = if outcome.is_err() {
            FiberState::Errored
        } else if unwound {
            self.started = true;
            FiberState::Suspended
        } else {
            self.body = None;
            FiberState::Done
        };
    }

    /// The bottom of the live part of this fiber's side stack.
    ///
    /// The collector narrows its conservative scan to the window above this
    /// address. Before the transform there was nothing here to report -- a
    /// wasm fiber's frames lived in the engine, out of reach -- but a
    /// suspended fiber's locals are now written into linear memory, and any
    /// of them may be an object nothing else refers to.
    pub fn saved_sp(&self) -> *const u8 {
        if self.state == FiberState::Suspended {
            // Safety: the header is inside `stack`.
            unsafe { self.stack.as_ptr().add(HEADER) }
        } else {
            std::ptr::null()
        }
    }

    /// This fiber's whole side stack, which is what the collector is given
    /// once when the fiber is made.
    ///
    /// The collector scans `[saved_sp, base + size)`, a shape that fits a
    /// stack growing down. This one grows up, so the range reported is the
    /// whole buffer and the scan covers the unused tail as well as the live
    /// frames. That is conservative in the safe direction -- it can keep a
    /// dead object alive, never drop a live one -- and the tail is zeroes
    /// until it is first used.
    pub fn stack_range(&self) -> (*const u8, usize) {
        if self.stack.len() <= HEADER {
            return (std::ptr::null(), 0);
        }
        // Safety: the header is the first two words of `stack`.
        let start = unsafe { self.stack.as_ptr().add(HEADER) };
        (start, self.stack.len() - HEADER)
    }
}
