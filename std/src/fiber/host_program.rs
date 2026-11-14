//! A program run as a fiber beneath a host's event loop.
//!
//! On iOS, UIKit owns the main thread and calls the app back once per pass of
//! its loop, so the program cannot be the thread's outermost frame. The program
//! is a fiber instead. The host's loop gives it turns: a turn runs the
//! scheduler until the program asks for the next one, which is when the host's
//! `wait` or `poll` has nothing to return and calls the yield registered here.
//! The program then resumes at the start of the next turn, inside that call.

use std::cell::Cell;
use std::ffi::c_void;
use std::ptr;

/// The compiled program's entry: its exit status.
type Entry = unsafe extern "C" fn() -> i64;
/// The program's turn, which answers 0 once the program has ended.
type Turn = extern "C" fn(*mut c_void) -> i32;
/// The host's loop: calls `turn(data)` for each turn until it returns 0.
type RunTurns = unsafe extern "C" fn(Turn, *mut c_void) -> i32;
/// Registers the function that suspends the program until the next turn.
type SetYield = unsafe extern "C" fn(Option<extern "C" fn()>);

#[derive(Clone, Copy)]
struct Program {
    /// The entry has returned, or the fiber ended without returning.
    done: bool,
    status: i64,
    /// The host asked for the next turn since the turn began.
    asked: bool,
}

thread_local! {
    /// The loop's thread runs every turn and every yield.
    static PROGRAM: Cell<Program> = const {
        Cell::new(Program { done: false, status: 1, asked: false })
    };
}

/// The fiber body: run the entry and record how it ended. A guard records the
/// end even when the body unwinds, so a turn never waits for a fiber that is
/// gone.
unsafe extern "C-unwind" fn run_program(ctx: *mut c_void) {
    struct Ended;
    impl Drop for Ended {
        fn drop(&mut self) {
            PROGRAM.with(|p| p.set(Program { done: true, ..p.get() }));
        }
    }
    let _ended = Ended;
    let entry = unsafe { std::mem::transmute::<*mut c_void, Entry>(ctx) };
    let status = unsafe { entry() };
    PROGRAM.with(|p| p.set(Program { status, ..p.get() }));
}

/// What the host calls when the program waits with nothing to return: hand the
/// thread back to the loop until its next turn. A call from outside a fiber has
/// nothing to suspend and returns.
extern "C" fn yield_to_turn() {
    let on_fiber = super::ACTIVE_FIBER.with(|active| active.get().is_some());
    if !on_fiber {
        return;
    }
    PROGRAM.with(|p| p.set(Program { asked: true, ..p.get() }));
    super::yield_now_backend();
}

/// One turn: run the scheduler until the program asks for the next turn or
/// ends. A yield for fairness, which compiled loops make on their own, is not
/// a request. It resumes at once, since the host is not waiting on it.
extern "C" fn turn(_: *mut c_void) -> i32 {
    PROGRAM.with(|p| p.set(Program { asked: false, ..p.get() }));
    loop {
        unsafe { super::poll_watches() };
        let ran = unsafe { crate::rt::schedule_step() };
        let program = PROGRAM.with(Cell::get);
        if program.done {
            // iOS cannot end its loop, so the program's end is the app's.
            #[cfg(target_os = "ios")]
            std::process::exit(program.status as i32);
            #[cfg(not(target_os = "ios"))]
            return 0;
        }
        if program.asked {
            return 1;
        }
        if !ran {
            // The program waits on something other than the host, a timer or
            // another thread. Pace as the main thread's own blocking does.
            std::thread::sleep(std::time::Duration::from_millis(1));
        }
    }
}

/// Run the program the way the host's loop needs. With `run_turns` null, no
/// loop is linked and the entry runs on this stack, as it does everywhere else.
/// Otherwise the entry becomes a fiber and `run_turns` drives it. Answers the
/// program's exit status, or 1 when the loop could not run.
///
/// # Safety
/// `entry` is the program's entry; `run_turns` and `set_yield`, when not null,
/// are the host's loop and its yield registration, called on this thread.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn hlp_host_run_program(
    run_turns: Option<RunTurns>,
    set_yield: Option<SetYield>,
    entry: Entry,
) -> i32 {
    unsafe {
        let Some(run_turns) = run_turns else {
            return entry() as i32;
        };
        PROGRAM.with(|p| p.set(Program { done: false, status: 1, asked: false }));
        if let Some(set_yield) = set_yield {
            set_yield(Some(yield_to_turn));
        }
        super::install_deferred(run_program, entry as usize as *mut c_void);
        if run_turns(turn, ptr::null_mut()) != 0 {
            eprintln!("[ash] the host's event loop could not run");
            return 1;
        }
        PROGRAM.with(Cell::get).status as i32
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    thread_local! {
        static STEPS: Cell<u32> = const { Cell::new(0) };
        static TURNS: Cell<u32> = const { Cell::new(0) };
    }

    /// A program that waits twice, as `wait` does under a host, and then ends.
    unsafe extern "C" fn waits_twice() -> i64 {
        for _ in 0..2 {
            STEPS.with(|s| s.set(s.get() + 1));
            yield_to_turn();
        }
        STEPS.with(|s| s.set(s.get() + 1));
        7
    }

    /// The host's loop: one program turn at a time, as a platform's callbacks
    /// would make them.
    unsafe extern "C" fn run_turns(turn: Turn, data: *mut c_void) -> i32 {
        loop {
            TURNS.with(|t| t.set(t.get() + 1));
            if turn(data) == 0 {
                return 0;
            }
        }
    }

    unsafe extern "C" fn set_yield(_: Option<extern "C" fn()>) {}

    #[test]
    fn the_program_runs_between_turns_and_ends_with_its_status() {
        // A fiber's stack is registered with the collector, so it has to exist
        // and know where this thread's stack begins. Every test thread has its
        // own scheduler, so this one has the thread to itself.
        let anchor = 0u8;
        unsafe {
            crate::rt::gc_init();
            crate::rt::gc_set_stack_top(&anchor as *const u8 as usize);
        }
        let status = unsafe { hlp_host_run_program(Some(run_turns), Some(set_yield), waits_twice) };
        assert_eq!(status, 7);
        // Each wait ends a turn, and the third turn finishes the program.
        assert_eq!(STEPS.with(Cell::get), 3);
        assert_eq!(TURNS.with(Cell::get), 3);
    }

    unsafe extern "C" fn returns_five() -> i64 {
        5
    }

    #[test]
    fn without_a_loop_the_entry_runs_on_this_stack() {
        let status = unsafe { hlp_host_run_program(None, None, returns_five) };
        assert_eq!(status, 5);
    }
}
