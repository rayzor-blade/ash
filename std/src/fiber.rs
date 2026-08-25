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
//! The host is notified around every fiber switch so it can swap logical VM
//! state (notably interpreted AIR v2 frames). The interpreter publishes both
//! its active and suspended register files as GC roots.

// `static mut` + raw-pointer access is this module's deliberate story (the
// VM's single-threaded invariant): `static_mut_refs` demands the
// `&raw`/deref spelling, and these two style lints then flag exactly that
// spelling. The trio cannot all be satisfied at once.
#![allow(clippy::deref_addrof, dangerous_implicit_autorefs)]
use crate::error::TrapContext;
use crate::hl::{vclosure, vdynamic};
use krio_fiber::{Fiber, FiberState};
use std::ffi::c_void;

pub type ClosureRunner =
    unsafe extern "C" fn(*mut vclosure, *mut *mut vdynamic, i32) -> *mut vdynamic;
pub type FiberSwitchHook = unsafe extern "C" fn(u32, u32);

static mut CLOSURE_RUNNER: Option<ClosureRunner> = None;
static mut FIBER_SWITCH_HOOK: Option<FiberSwitchHook> = None;

/// Registered by the host (interpreter) so native code can run a Haxe
/// closure whose `fun` is an interpreter stub pointer (findex+1).
#[no_mangle]
pub unsafe extern "C" fn hlp_set_closure_runner(f: ClosureRunner) {
    CLOSURE_RUNNER = Some(f);
}

/// Registered by the host so logical VM state follows krio's stack switch.
/// Fiber id 0 denotes the scheduler/main context.
#[no_mangle]
pub unsafe extern "C" fn hlp_set_fiber_switch_hook(hook: FiberSwitchHook) {
    FIBER_SWITCH_HOOK = Some(hook);
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
    !(*(&raw const FIBERS)).is_empty()
}

/// The opaque handle used by `thread_create` for the currently running Haxe
/// fiber. The main context continues to use its native OS-thread identity.
pub(crate) unsafe fn current_handle() -> Option<*mut c_void> {
    (*(&raw const CURRENT)).map(|id| ((id as usize) << 4 | 1) as *mut c_void)
}

/// Whether `c` is the closure at the root of the currently running fiber.
///
/// Native helpers also use the closure runner to re-enter interpreted AIR V2
/// methods. Those nested calls must propagate exceptions through their active
/// native trap; only a real thread body's outermost call owns an uncaught
/// exception and may terminate the fiber.
#[no_mangle]
pub unsafe extern "C" fn hlp_fiber_is_root_closure(c: *mut vclosure) -> bool {
    let Some(id) = *(&raw const CURRENT) else {
        return false;
    };
    (*(&raw const FIBERS))
        .iter()
        .find(|fiber| fiber.id == id)
        .is_some_and(|fiber| fiber.closure == c)
}

unsafe fn notify_switch(from: u32, to: u32) {
    if let Some(hook) = FIBER_SWITCH_HOOK {
        hook(from, to);
    }
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

    (*(&raw mut FIBERS)).push(VmFiber {
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
    if (*(&raw const CURRENT)).is_some() {
        // A fiber calling this should yield instead — never nest resumes.
        return false;
    }
    let mut resumed = false;
    let mut i = 0;
    while i < (*(&raw const FIBERS)).len() {
        let state = (*(&raw const FIBERS))[i].fiber.state();
        if matches!(state, FiberState::Done | FiberState::Errored) {
            let f = (*(&raw mut FIBERS)).swap_remove(i);
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
        let (mut trap, mut exc) = (
            (*(&raw const FIBERS))[i].trap_head,
            (*(&raw const FIBERS))[i].exc_value,
        );
        crate::gc::gc_swap_exc_state(&mut trap, &mut exc);
        MAIN_TRAP = trap;
        MAIN_EXC = exc;
        let id = (*(&raw const FIBERS))[i].id;
        CURRENT = Some(id);
        notify_switch(0, id);

        (*(&raw mut FIBERS))[i].fiber.resume();

        notify_switch(id, 0);
        CURRENT = None;
        let (mut trap, mut exc) = (MAIN_TRAP, MAIN_EXC);
        crate::gc::gc_swap_exc_state(&mut trap, &mut exc);
        (*(&raw mut FIBERS))[i].trap_head = trap;
        (*(&raw mut FIBERS))[i].exc_value = exc;
        crate::gc::gc_update_fiber_sp(
            (*(&raw const FIBERS))[i].id,
            (*(&raw const FIBERS))[i].fiber.saved_sp() as usize,
        );

        i += 1;
    }
    resumed
}

/// Universal "I am blocked" primitive: on a fiber, yield to the scheduler;
/// on the main context, run other fibers, and keep the window alive with an
/// SDL pump + short nap when nothing is runnable.
pub(crate) unsafe fn block_yield() {
    if (*(&raw const CURRENT)).is_some() {
        krio_fiber::yield_now();
    } else {
        schedule_step();
        // Always pace after a pass: a resumed-but-still-blocked fiber yields
        // instantly, and treating that as progress spins main and every
        // blocked fiber at 100% CPU ping-pong. 1ms keeps the old cadence
        // and keeps the SDL window responsive.
        crate::thread::pump_sdl_events();
        // std's sleep rather than nanosleep: same syscall on unix, plus the
        // EINTR retry a pacing nap wants anyway, and no Win32 fork. Windows
        // may round the wait up to the OS timer resolution — acceptable,
        // because this is pacing and not a deadline.
        std::thread::sleep(std::time::Duration::from_millis(1));
    }
}
