//! Run a call that may block for a long time on an OS thread while the
//! calling fiber is parked, so the scheduler thread keeps running its other
//! fibers.
//!
//! The closure runs on a pool thread the collector does not know, so it may
//! touch Rust-owned state and the Haxe bytes the caller holds (not moved, not
//! scanned), and nothing else on the GC heap.

#[cfg(not(target_family = "wasm"))]
pub(crate) use native::offload;

/// No threads to hand the call to: it runs where it is called.
#[cfg(target_family = "wasm")]
pub(crate) fn offload<T>(f: impl FnOnce() -> T) -> T {
    f()
}

#[cfg(not(target_family = "wasm"))]
mod native {
    use crate::rt;
    use std::collections::VecDeque;
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::{Condvar, Mutex};
    use std::time::Duration;

    type Job = Box<dyn FnOnce() + Send + 'static>;

    struct Pool {
        queue: VecDeque<Job>,
        idle: usize,
    }

    static POOL: Mutex<Pool> = Mutex::new(Pool {
        queue: VecDeque::new(),
        idle: 0,
    });
    static WORK: Condvar = Condvar::new();

    /// How long an idle pool thread waits for work before it exits.
    const IDLE_EXIT: Duration = Duration::from_secs(5);

    fn submit(job: Job) {
        let mut pool = POOL.lock().unwrap();
        pool.queue.push_back(job);
        if pool.idle >= pool.queue.len() {
            WORK.notify_one();
            return;
        }
        drop(pool);
        std::thread::Builder::new()
            .name("ash-blocking".into())
            .spawn(worker)
            .expect("spawn blocking-call thread");
    }

    fn worker() {
        let mut pool = POOL.lock().unwrap();
        loop {
            if let Some(job) = pool.queue.pop_front() {
                drop(pool);
                job();
                pool = POOL.lock().unwrap();
                continue;
            }
            pool.idle += 1;
            let (guard, wait) = WORK.wait_timeout(pool, IDLE_EXIT).unwrap();
            pool = guard;
            pool.idle -= 1;
            if wait.timed_out() && pool.queue.is_empty() {
                return;
            }
        }
    }

    pub(crate) fn offload<T: Send>(f: impl FnOnce() -> T + Send) -> T {
        struct Slot<T> {
            result: Mutex<Option<std::thread::Result<T>>>,
            done: AtomicBool,
        }
        let slot = Slot {
            result: Mutex::new(None),
            done: AtomicBool::new(false),
        };
        // SAFETY: this function does not return before `done` is set, which the
        // job does last, so everything the job borrows outlives it.
        let waiter = unsafe { rt::new_waiter() };
        let job: Box<dyn FnOnce() + Send + '_> = Box::new(|| {
            let outcome = std::panic::catch_unwind(std::panic::AssertUnwindSafe(f));
            *slot.result.lock().unwrap() = Some(outcome);
            slot.done.store(true, Ordering::Release);
            unsafe { rt::wake(waiter) };
        });
        let job: Job = unsafe { std::mem::transmute(job) };
        submit(job);
        if !slot.done.load(Ordering::Acquire) {
            unsafe { rt::park(waiter, None) };
        }
        // A wake is sent after `done`, so this only waits out a park that
        // returned early.
        while !slot.done.load(Ordering::Acquire) {
            std::thread::yield_now();
        }
        match slot.result.lock().unwrap().take().unwrap() {
            Ok(value) => value,
            Err(panic) => std::panic::resume_unwind(panic),
        }
    }
}
