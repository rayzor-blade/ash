//! Stackful fiber runtime backing the sys.thread API on a single OS thread.
//!
//! HashLink's hl target assumes HL_THREADS: Haxe code spawns worker threads
//! and blocks on locks/deques they release. ash is single-threaded, so
//! `thread_create` runs each Haxe "thread" as a krio fiber, and blocking
//! primitives become yield points: the blocked context lets every runnable
//! fiber advance, then re-checks its predicate. When no fibers exist the
//! primitives keep exact upstream !HL_THREADS semantics (never block).
//!
//! GC: each fiber stack is registered with the conservative scanner
//! (krio's switch spills all callee-saved regs — including d8-d15 — onto
//! the fiber stack, so scanning [saved_sp, stack_top) covers registers).
//! Exception state (trap chain head + exception value) is per-fiber,
//! swapped into the GC singleton's cells around every resume.
//!
//! KNOWN SPIKE LIMIT: the interpreter's scan-root protocol (clear-then-add)
//! is not yet per-fiber — heavy allocation while a fiber's interpreter
//! frames are suspended can under-root them. Heaps workers are wait-loops,
//! so this is acceptable for bring-up; the hardening fix is per-fiber
//! scan_ranges.

use crate::error::TrapContext;
use crate::hl::{vclosure, vdynamic};
use krio_fiber::{Fiber, FiberState};
use std::ffi::c_void;

pub type ClosureRunner =
    unsafe extern "C" fn(*mut vclosure, *mut *mut vdynamic, i32) -> *mut vdynamic;

static mut CLOSURE_RUNNER: Option<ClosureRunner> = None;

/// Registered by the host (interpreter) so native code can run a Haxe
/// closure whose `fun` is an interpreter stub pointer (findex+1).
#[no_mangle]
pub unsafe extern "C" fn hlp_set_closure_runner(f: ClosureRunner) {
    CLOSURE_RUNNER = Some(f);
}

/// The registered interpreter re-entry runner, if any. Used by native code
/// (e.g. virtual method dispatch fallback) that encounters a stub-sentinel
/// function pointer it must not call directly.
pub(crate) unsafe fn closure_runner() -> Option<ClosureRunner> {
    CLOSURE_RUNNER
}

struct VmFiber {
    fiber: Box<Fiber>,
    id: u32,
    closure: *mut vclosure,
    /// Per-fiber exception state, swapped into the GC cells while running.
    trap_head: *mut TrapContext,
    exc_value: *mut vdynamic,
}

// Single-threaded VM: fibers are created and scheduled only on the main
// OS thread (krio Fibers are !Send by design).
static mut FIBERS: Vec<VmFiber> = Vec::new();
static mut NEXT_ID: u32 = 1; // id 0 is reserved for the main stack
static mut CURRENT: Option<u32> = None;
static mut MAIN_TRAP: *mut TrapContext = std::ptr::null_mut();
static mut MAIN_EXC: *mut vdynamic = std::ptr::null_mut();

/// wren_lift-proven default; 64 KB tripped on real workloads there.
const FIBER_STACK_SIZE: usize = 256 * 1024;

pub(crate) unsafe fn fibers_active() -> bool {
    !FIBERS.is_empty()
}

pub(crate) unsafe fn on_fiber() -> bool {
    CURRENT.is_some()
}

unsafe fn run_closure(c: *mut vclosure) {
    let fun = (*c).fun as usize;
    eprintln!("[ash] fiber: thread body starting (fun={:#x})", fun);
    if let Some(runner) = CLOSURE_RUNNER {
        runner(c, std::ptr::null_mut(), 0);
        eprintln!("[ash] fiber: thread body returned (fun={:#x})", fun);
    } else if fun >= 0x100000 && (*c).hasValue == 0 {
        // JIT mode, plain function pointer, zero args.
        let f: extern "C" fn() = std::mem::transmute(fun);
        f();
    } else {
        eprintln!(
            "[ash] fiber: cannot run closure (fun={:#x}, hasValue={}) — no closure runner set",
            fun,
            (*c).hasValue
        );
    }
}

/// Spawn a Haxe thread as a fiber. Returns an opaque non-null handle.
pub(crate) unsafe fn thread_create(c: *mut vclosure) -> *mut c_void {
    if c.is_null() {
        return std::ptr::null_mut();
    }
    eprintln!(
        "[ash] fiber: thread_create (closure fun={:#x})",
        (*c).fun as usize
    );
    let id = NEXT_ID;
    NEXT_ID += 1;

    // Root the closure while the fiber exists — it is otherwise reachable
    // only from Rust heap memory the GC cannot see.
    crate::gc::gc_add_persistent(c as *mut vdynamic);

    let c_usize = c as usize;
    let fiber = Box::new(Fiber::with_stack_size(FIBER_STACK_SIZE, move || {
        run_closure(c_usize as *mut vclosure);
    }));
    let (base, len) = fiber.stack_range();
    crate::gc::gc_register_fiber_stack(id, base as usize, len);
    // Charge the off-heap stack as GC allocation pressure so dead fibers'
    // stacks translate into collections (wren_lift core/fiber.rs:189-199).
    crate::gc::hlp_gc_track_external(len as u64);

    FIBERS.push(VmFiber {
        fiber,
        id,
        closure: c,
        trap_head: std::ptr::null_mut(),
        exc_value: std::ptr::null_mut(),
    });

    // Give the new thread a chance to run to its first blocking point,
    // matching the "starts immediately" expectation of real threads.
    schedule_step();

    // Handle = fiber id, offset so it is never null and never a valid ptr.
    ((id as usize) << 4 | 1) as *mut c_void
}

/// Resume every runnable fiber once (single round-robin pass). Runs on the
/// main context only. Returns true if any fiber was resumed.
pub(crate) unsafe fn schedule_step() -> bool {
    if CURRENT.is_some() {
        // A fiber calling this should yield instead — never nest resumes.
        return false;
    }
    let mut resumed = false;
    let mut i = 0;
    while i < FIBERS.len() {
        let state = FIBERS[i].fiber.state();
        if matches!(state, FiberState::Done | FiberState::Errored) {
            let f = FIBERS.swap_remove(i);
            crate::gc::gc_unregister_fiber_stack(f.id);
            crate::gc::gc_remove_persistent(f.closure as *mut vdynamic);
            if let FiberState::Errored = state {
                eprintln!("[ash] fiber {} terminated with a panic", f.id);
            }
            continue;
        }
        resumed = true;

        // Record where the main stack is suspended for the GC, then swap
        // this fiber's exception state into the live cells.
        let probe: usize = 0;
        crate::gc::gc_update_fiber_sp(0, &probe as *const usize as usize);
        let (mut trap, mut exc) = (FIBERS[i].trap_head, FIBERS[i].exc_value);
        crate::gc::gc_swap_exc_state(&mut trap, &mut exc);
        MAIN_TRAP = trap;
        MAIN_EXC = exc;
        CURRENT = Some(FIBERS[i].id);

        FIBERS[i].fiber.resume();

        CURRENT = None;
        let (mut trap, mut exc) = (MAIN_TRAP, MAIN_EXC);
        crate::gc::gc_swap_exc_state(&mut trap, &mut exc);
        FIBERS[i].trap_head = trap;
        FIBERS[i].exc_value = exc;
        crate::gc::gc_update_fiber_sp(FIBERS[i].id, FIBERS[i].fiber.saved_sp() as usize);

        i += 1;
    }
    resumed
}

/// Universal "I am blocked" primitive: on a fiber, yield to the scheduler;
/// on the main context, run other fibers, and keep the window alive with an
/// SDL pump + short nap when nothing is runnable.
pub(crate) unsafe fn block_yield() {
    if CURRENT.is_some() {
        krio_fiber::yield_now();
    } else {
        schedule_step();
        // Always pace after a pass: a resumed-but-still-blocked fiber yields
        // instantly, and treating that as progress spins main and every
        // blocked fiber at 100% CPU ping-pong. 1ms keeps the old cadence
        // and keeps the SDL window responsive.
        crate::thread::pump_sdl_events();
        let ts = libc::timespec {
            tv_sec: 0,
            tv_nsec: 1_000_000,
        };
        libc::nanosleep(&ts, std::ptr::null_mut());
    }
}
