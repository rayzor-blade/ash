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
/// Its closure runs inside [`Fiber::resume`]. With a suspending host, the
/// closure's own calls to [`yield_now`] hand control back mid-flight and
/// `resume` returns only when the work is finished; without one, `resume`
/// runs it to the end. Either way the state afterwards is [`FiberState::Done`]
/// or [`FiberState::Errored`], which is all the scheduler above inspects.
pub struct Fiber {
    body: Option<Box<dyn FnOnce()>>,
    state: FiberState,
}

impl Fiber {
    /// The stack size is accepted and ignored: the engine owns the stack, and
    /// a wasm module cannot choose its size from the inside.
    pub fn with_stack_size<F>(_stack_size: usize, f: F) -> Self
    where
        F: FnOnce() + 'static,
    {
        Self {
            body: Some(Box::new(f)),
            state: FiberState::Ready,
        }
    }

    pub fn state(&self) -> FiberState {
        self.state
    }

    /// Run the fiber's body.
    ///
    /// A panic inside it leaves the fiber `Errored` rather than unwinding
    /// into the scheduler, which is what the native backend does too.
    pub fn resume(&mut self) {
        let Some(body) = self.body.take() else {
            return;
        };
        self.state = FiberState::Running;
        let outcome = std::panic::catch_unwind(std::panic::AssertUnwindSafe(body));
        self.state = if outcome.is_ok() {
            FiberState::Done
        } else {
            FiberState::Errored
        };
    }

    /// Where a suspended fiber's stack pointer is, which here is nowhere.
    ///
    /// The collector narrows its conservative scan to the live window above
    /// this address. A wasm fiber's stack is held by the engine, outside
    /// linear memory and outside the collector's reach, so there is no
    /// address to report and the null it gets means "scan nothing" rather
    /// than "scan everything" -- which is why roots on this target have to
    /// be explicit.
    pub fn saved_sp(&self) -> *const u8 {
        std::ptr::null()
    }

    /// No stack of its own to scan.
    ///
    /// The collector uses this range for a conservative scan of a suspended
    /// fiber's stack. A wasm module's stack lives in the engine, not in linear
    /// memory, so there is nothing here to scan and nothing to report --
    /// roots on this target have to be explicit (see docs/wasm-target.md).
    pub fn stack_range(&self) -> (*const u8, usize) {
        (std::ptr::null(), 0)
    }
}
