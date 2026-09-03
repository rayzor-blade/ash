//! Thread, mutex, semaphore, condition, TLS, and atomic operations.
//!
//! Implements the HashLink `std/thread.c` API surface needed by Heaps.io
//! and other non-trivial Haxe programs.

use std::collections::{HashMap, HashSet, VecDeque};
use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::{AtomicI32, Ordering};
use std::time::{Duration, Instant};

use crate::fiber::Waiter;
use crate::hl::vdynamic;

fn wake_one(waiters: &mut VecDeque<Waiter>) -> bool {
    while let Some(waiter) = waiters.pop_front() {
        if unsafe { crate::fiber::wake(waiter) } {
            return true;
        }
    }
    false
}

fn remove_waiter(waiters: &mut VecDeque<Waiter>, waiter: Waiter) {
    if let Some(index) = waiters.iter().position(|candidate| *candidate == waiter) {
        waiters.remove(index);
    }
}

// ============================================================================
// Mutex
// ============================================================================

#[repr(C)]
struct HlMutex {
    state: std::sync::Mutex<MutexState>,
}

struct MutexState {
    owner: Option<u64>,
    depth: u32,
    waiters: VecDeque<Waiter>,
}

unsafe fn mutex_try_acquire_inner(mutex: *mut HlMutex) -> bool {
    let current = crate::fiber::current_owner();
    let mut state = (*mutex).state.lock().unwrap();
    match state.owner {
        None => {
            state.owner = Some(current);
            state.depth = 1;
            true
        }
        Some(owner) if owner == current => {
            state.depth = state.depth.saturating_add(1);
            true
        }
        Some(_) => false,
    }
}

unsafe fn mutex_acquire_inner(mutex: *mut HlMutex) {
    loop {
        let waiter = {
            let current = crate::fiber::current_owner();
            let mut state = (*mutex).state.lock().unwrap();
            match state.owner {
                None => {
                    state.owner = Some(current);
                    state.depth = 1;
                    return;
                }
                Some(owner) if owner == current => {
                    state.depth = state.depth.saturating_add(1);
                    return;
                }
                Some(_) => {
                    let waiter = crate::fiber::new_waiter();
                    state.waiters.push_back(waiter);
                    waiter
                }
            }
        };
        let _ = crate::fiber::park(waiter, None);
        let mut state = (*mutex).state.lock().unwrap();
        remove_waiter(&mut state.waiters, waiter);
        if state.owner.is_none() {
            state.owner = Some(crate::fiber::current_owner());
            state.depth = 1;
            return;
        }
    }
}

unsafe fn mutex_release_inner(mutex: *mut HlMutex) {
    let mut state = (*mutex).state.lock().unwrap();
    if state.owner != Some(crate::fiber::current_owner()) || state.depth == 0 {
        return;
    }
    state.depth -= 1;
    if state.depth == 0 {
        state.owner = None;
        wake_one(&mut state.waiters);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_mutex_alloc(_gc_thread: bool) -> *mut c_void {
    Box::into_raw(Box::new(HlMutex {
        state: std::sync::Mutex::new(MutexState {
            owner: None,
            depth: 0,
            waiters: VecDeque::new(),
        }),
    })) as *mut c_void
}

#[no_mangle]
pub unsafe extern "C" fn hlp_mutex_acquire(m: *mut c_void) {
    if !m.is_null() {
        mutex_acquire_inner(m as *mut HlMutex);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_mutex_try_acquire(m: *mut c_void) -> bool {
    if m.is_null() {
        return false;
    }
    mutex_try_acquire_inner(m as *mut HlMutex)
}

#[no_mangle]
pub unsafe extern "C" fn hlp_mutex_release(m: *mut c_void) {
    if !m.is_null() {
        mutex_release_inner(m as *mut HlMutex);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_mutex_free(m: *mut c_void) {
    if !m.is_null() {
        drop(Box::from_raw(m as *mut HlMutex));
    }
}

// HDLLs call HashLink's public C API names directly, while Haxe bytecode
// resolves the `hlp_` primitive names above. Export both spellings so native
// extensions and bytecode share the same synchronization objects.
#[no_mangle]
pub unsafe extern "C" fn hl_mutex_alloc(gc_thread: bool) -> *mut c_void {
    hlp_mutex_alloc(gc_thread)
}

#[no_mangle]
pub unsafe extern "C" fn hl_mutex_acquire(m: *mut c_void) {
    hlp_mutex_acquire(m);
}

#[no_mangle]
pub unsafe extern "C" fn hl_mutex_try_acquire(m: *mut c_void) -> bool {
    hlp_mutex_try_acquire(m)
}

#[no_mangle]
pub unsafe extern "C" fn hl_mutex_release(m: *mut c_void) {
    hlp_mutex_release(m);
}

#[no_mangle]
pub unsafe extern "C" fn hl_mutex_free(m: *mut c_void) {
    hlp_mutex_free(m);
}

// ============================================================================
// Semaphore (used by Lock)
// ============================================================================

#[repr(C)]
struct HlSemaphore {
    state: std::sync::Mutex<SemaphoreState>,
}

struct SemaphoreState {
    value: i32,
    waiters: VecDeque<Waiter>,
}

unsafe fn semaphore_take(s: *mut HlSemaphore) -> bool {
    let mut state = (*s).state.lock().unwrap();
    let acquired = state.value > 0;
    if acquired {
        state.value -= 1;
    }
    acquired
}

unsafe fn timeout_deadline(timeout: *mut vdynamic) -> Option<std::time::Instant> {
    if timeout.is_null() {
        return None;
    }
    let kind = if !(*timeout).t.is_null() {
        (*(*timeout).t).kind
    } else {
        0
    };
    let secs = if kind == 6 {
        (*timeout).v.d
    } else if kind == 5 {
        (*timeout).v.f as f64
    } else {
        0.0
    };
    (secs > 0.0).then(|| std::time::Instant::now() + std::time::Duration::from_secs_f64(secs))
}

#[no_mangle]
pub unsafe extern "C" fn hlp_semaphore_alloc(value: i32) -> *mut c_void {
    Box::into_raw(Box::new(HlSemaphore {
        state: std::sync::Mutex::new(SemaphoreState {
            value,
            waiters: VecDeque::new(),
        }),
    })) as *mut c_void
}

unsafe fn semaphore_wait(s: *mut HlSemaphore, deadline: Option<Instant>) -> bool {
    let waiter = {
        let mut state = (*s).state.lock().unwrap();
        if state.value > 0 {
            state.value -= 1;
            return true;
        }
        // A program with no fibers at all has nothing that could release the
        // permit while the main context waits, so it still declines rather
        // than hanging. A thread the runtime did not create is a different
        // case: something else is running and will release, so it waits.
        if deadline.is_some_and(|limit| Instant::now() >= limit)
            || (!crate::fiber::fibers_active()
                && crate::fiber::is_main_thread()
                && !crate::fiber::foreign_threads_seen())
        {
            return false;
        }
        let waiter = crate::fiber::new_waiter();
        state.waiters.push_back(waiter);
        waiter
    };
    let notified = crate::fiber::park(waiter, deadline);
    remove_waiter(&mut (*s).state.lock().unwrap().waiters, waiter);
    notified
}

#[no_mangle]
pub unsafe extern "C" fn hlp_semaphore_acquire(sem: *mut c_void) {
    if sem.is_null() {
        return;
    }
    let s = sem as *mut HlSemaphore;
    let _ = semaphore_wait(s, None);
}

#[no_mangle]
pub unsafe extern "C" fn hlp_semaphore_try_acquire(
    sem: *mut c_void,
    timeout: *mut vdynamic,
) -> bool {
    if sem.is_null() {
        return false;
    }
    let s = sem as *mut HlSemaphore;
    if semaphore_take(s) {
        return true;
    }
    let Some(deadline) = timeout_deadline(timeout) else {
        return false;
    };
    semaphore_wait(s, Some(deadline))
}

#[no_mangle]
pub unsafe extern "C" fn hlp_semaphore_release(sem: *mut c_void) {
    if sem.is_null() {
        return;
    }
    let s = sem as *mut HlSemaphore;
    let mut state = (*s).state.lock().unwrap();
    if !wake_one(&mut state.waiters) {
        state.value = state.value.saturating_add(1);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_semaphore_free(sem: *mut c_void) {
    if !sem.is_null() {
        drop(Box::from_raw(sem as *mut HlSemaphore));
    }
}

#[no_mangle]
pub unsafe extern "C" fn hl_semaphore_alloc(value: i32) -> *mut c_void {
    hlp_semaphore_alloc(value)
}

#[no_mangle]
pub unsafe extern "C" fn hl_semaphore_acquire(sem: *mut c_void) {
    hlp_semaphore_acquire(sem);
}

#[no_mangle]
pub unsafe extern "C" fn hl_semaphore_try_acquire(
    sem: *mut c_void,
    timeout: *mut vdynamic,
) -> bool {
    hlp_semaphore_try_acquire(sem, timeout)
}

#[no_mangle]
pub unsafe extern "C" fn hl_semaphore_release(sem: *mut c_void) {
    hlp_semaphore_release(sem);
}

#[no_mangle]
pub unsafe extern "C" fn hl_semaphore_free(sem: *mut c_void) {
    hlp_semaphore_free(sem);
}

// ============================================================================
// Condition Variable
// ============================================================================

#[repr(C)]
struct HlCondition {
    state: std::sync::Mutex<ConditionState>,
}

struct ConditionState {
    owner: Option<u64>,
    depth: u32,
    mutex_waiters: VecDeque<Waiter>,
    waiters: VecDeque<Waiter>,
}

unsafe fn condition_mutex_acquire(c: *mut HlCondition) {
    loop {
        let waiter = {
            let current = crate::fiber::current_owner();
            let mut state = (*c).state.lock().unwrap();
            match state.owner {
                None => {
                    state.owner = Some(current);
                    state.depth = 1;
                    return;
                }
                Some(owner) if owner == current => {
                    state.depth = state.depth.saturating_add(1);
                    return;
                }
                Some(_) => {
                    let waiter = crate::fiber::new_waiter();
                    state.mutex_waiters.push_back(waiter);
                    waiter
                }
            }
        };
        let _ = crate::fiber::park(waiter, None);
        let mut state = (*c).state.lock().unwrap();
        remove_waiter(&mut state.mutex_waiters, waiter);
    }
}

unsafe fn condition_mutex_try_acquire(c: *mut HlCondition) -> bool {
    let current = crate::fiber::current_owner();
    let mut state = (*c).state.lock().unwrap();
    match state.owner {
        None => {
            state.owner = Some(current);
            state.depth = 1;
            true
        }
        Some(owner) if owner == current => {
            state.depth = state.depth.saturating_add(1);
            true
        }
        Some(_) => false,
    }
}

unsafe fn condition_mutex_release(c: *mut HlCondition) {
    let mut state = (*c).state.lock().unwrap();
    if state.owner != Some(crate::fiber::current_owner()) || state.depth == 0 {
        return;
    }
    state.depth -= 1;
    if state.depth == 0 {
        state.owner = None;
        wake_one(&mut state.mutex_waiters);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_condition_alloc() -> *mut c_void {
    Box::into_raw(Box::new(HlCondition {
        state: std::sync::Mutex::new(ConditionState {
            owner: None,
            depth: 0,
            mutex_waiters: VecDeque::new(),
            waiters: VecDeque::new(),
        }),
    })) as *mut c_void
}

#[no_mangle]
pub unsafe extern "C" fn hlp_condition_acquire(c: *mut c_void) {
    if !c.is_null() {
        condition_mutex_acquire(c as *mut HlCondition);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_condition_try_acquire(c: *mut c_void) -> bool {
    if c.is_null() {
        return false;
    }
    condition_mutex_try_acquire(c as *mut HlCondition)
}

#[no_mangle]
pub unsafe extern "C" fn hlp_condition_release(c: *mut c_void) {
    if !c.is_null() {
        condition_mutex_release(c as *mut HlCondition);
    }
}

unsafe fn condition_wait_inner(c: *mut HlCondition, deadline: Option<Instant>) -> bool {
    if !crate::fiber::fibers_active()
        && crate::fiber::is_main_thread()
        && !crate::fiber::foreign_threads_seen()
    {
        return true;
    }
    let (waiter, depth) = {
        let current = crate::fiber::current_owner();
        let mut state = (*c).state.lock().unwrap();
        if state.owner != Some(current) {
            return false;
        }
        let waiter = crate::fiber::new_waiter();
        state.waiters.push_back(waiter);
        let depth = state.depth;
        state.owner = None;
        state.depth = 0;
        wake_one(&mut state.mutex_waiters);
        (waiter, depth)
    };
    let notified = crate::fiber::park(waiter, deadline);
    remove_waiter(&mut (*c).state.lock().unwrap().waiters, waiter);
    condition_mutex_acquire(c);
    (*c).state.lock().unwrap().depth = depth;
    notified
}

#[no_mangle]
pub unsafe extern "C" fn hlp_condition_wait(c: *mut c_void) {
    if !c.is_null() {
        let _ = condition_wait_inner(c as *mut HlCondition, None);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_condition_timed_wait(c: *mut c_void, timeout: f64) -> bool {
    if c.is_null() {
        return false;
    }
    let deadline = Instant::now() + Duration::from_secs_f64(timeout.max(0.0));
    condition_wait_inner(c as *mut HlCondition, Some(deadline))
}

#[no_mangle]
pub unsafe extern "C" fn hlp_condition_signal(c: *mut c_void) {
    if !c.is_null() {
        wake_one(&mut (*(c as *mut HlCondition)).state.lock().unwrap().waiters);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_condition_broadcast(c: *mut c_void) {
    if !c.is_null() {
        let mut state = (*(c as *mut HlCondition)).state.lock().unwrap();
        while wake_one(&mut state.waiters) {}
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_condition_free(c: *mut c_void) {
    if !c.is_null() {
        drop(Box::from_raw(c as *mut HlCondition));
    }
}

#[no_mangle]
pub unsafe extern "C" fn hl_condition_alloc() -> *mut c_void {
    hlp_condition_alloc()
}

#[no_mangle]
pub unsafe extern "C" fn hl_condition_acquire(c: *mut c_void) {
    hlp_condition_acquire(c);
}

#[no_mangle]
pub unsafe extern "C" fn hl_condition_try_acquire(c: *mut c_void) -> bool {
    hlp_condition_try_acquire(c)
}

#[no_mangle]
pub unsafe extern "C" fn hl_condition_release(c: *mut c_void) {
    hlp_condition_release(c);
}

#[no_mangle]
pub unsafe extern "C" fn hl_condition_wait(c: *mut c_void) {
    hlp_condition_wait(c);
}

#[no_mangle]
pub unsafe extern "C" fn hl_condition_timed_wait(c: *mut c_void, timeout: f64) -> bool {
    hlp_condition_timed_wait(c, timeout)
}

#[no_mangle]
pub unsafe extern "C" fn hl_condition_signal(c: *mut c_void) {
    hlp_condition_signal(c);
}

#[no_mangle]
pub unsafe extern "C" fn hl_condition_broadcast(c: *mut c_void) {
    hlp_condition_broadcast(c);
}

#[no_mangle]
pub unsafe extern "C" fn hl_condition_free(c: *mut c_void) {
    hlp_condition_free(c);
}

// ============================================================================
// Lock (semaphore-backed lock with release/wait semantics)
// ============================================================================

#[no_mangle]
pub unsafe extern "C" fn hlp_lock_create() -> *mut c_void {
    hlp_semaphore_alloc(0)
}

#[no_mangle]
pub unsafe extern "C" fn hlp_lock_release(lock: *mut c_void) {
    hlp_semaphore_release(lock);
}

#[no_mangle]
pub unsafe extern "C" fn hlp_lock_wait(lock: *mut c_void, timeout: *mut vdynamic) -> bool {
    if lock.is_null() {
        return false;
    }
    let s = lock as *mut HlSemaphore;
    if semaphore_take(s) {
        return true;
    }
    // No fibers: exact HashLink !HL_THREADS semantics — never block.
    if !crate::fiber::fibers_active() {
        return false;
    }
    // Fibers exist: HL_THREADS semantics — wait for a release, cooperatively.
    // Timeout arrives as a boxed Null<Float> (seconds); null = wait forever.
    let deadline = if timeout.is_null() {
        None
    } else {
        let t = timeout as *const vdynamic;
        let kind = if !(*t).t.is_null() { (*(*t).t).kind } else { 0 };
        let secs = if kind == 6 {
            (*t).v.d
        } else if kind == 5 {
            (*t).v.f as f64
        } else {
            0.0
        };
        if secs <= 0.0 {
            return false; // wait(0.0): pure poll, e.g. EventLoop's drain
        }
        Some(std::time::Instant::now() + std::time::Duration::from_secs_f64(secs))
    };
    semaphore_wait(s, deadline)
}

// Compatibility primitive retained for bytecode that references it. Event
// ownership belongs to the loaded UI/SDL library, so this only provides frame
// pacing and never consumes native events itself.
#[no_mangle]
pub unsafe extern "C" fn hlp_pump_and_sleep() {
    // Told to the collector: a thread asleep here reaches no safepoint, and a
    // world stop would otherwise wait out the frame.
    hlp_blocking(true);
    std::thread::sleep(std::time::Duration::from_millis(16));
    hlp_blocking(false);
}

#[no_mangle]
pub unsafe extern "C" fn hlp_lock_free(lock: *mut c_void) {
    hlp_semaphore_free(lock);
}

// ============================================================================
// Deque
// ============================================================================

struct DequeState {
    queue: VecDeque<*mut c_void>,
    waiters: VecDeque<Waiter>,
}

struct HlDeque {
    state: std::sync::Mutex<DequeState>,
}

#[no_mangle]
pub unsafe extern "C" fn hlp_deque_alloc() -> *mut c_void {
    Box::into_raw(Box::new(HlDeque {
        state: std::sync::Mutex::new(DequeState {
            queue: VecDeque::new(),
            waiters: VecDeque::new(),
        }),
    })) as *mut c_void
}

/// A queued message is a GC object whose only reference is the Vec above,
/// which lives on the malloc heap the collector never scans -- so without an
/// explicit root it is collectable the moment the sender drops its copy. That
/// is the same defect the Int/Object maps had. Persistent roots fit a deque
/// better than a GC-allocated backing array: O(1) per enqueue, and they work
/// across threads.
///
/// Rooting happens *before* the message is published, so there is no window in
/// which it is reachable only from the unscanned Vec. `persistent_roots` is a
/// set, so a pointer queued twice roots once -- pop therefore only unroots when
/// no copy remains.
unsafe fn deque_root(msg: *mut vdynamic) {
    if !msg.is_null() {
        crate::gc::gc_add_persistent(msg);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_deque_add(d: *mut c_void, msg: *mut vdynamic) {
    if d.is_null() {
        return;
    }
    deque_root(msg);
    let deque = &*(d as *const HlDeque);
    if let Ok(mut state) = deque.state.lock() {
        state.queue.push_back(msg as *mut c_void);
        wake_one(&mut state.waiters);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_deque_push(d: *mut c_void, msg: *mut vdynamic) {
    if d.is_null() {
        return;
    }
    deque_root(msg);
    let deque = &*(d as *const HlDeque);
    if let Ok(mut state) = deque.state.lock() {
        state.queue.push_front(msg as *mut c_void);
        wake_one(&mut state.waiters);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_deque_pop(d: *mut c_void, block: bool) -> *mut vdynamic {
    if d.is_null() {
        return ptr::null_mut();
    }
    let deque = &*(d as *const HlDeque);
    loop {
        let popped;
        if let Ok(mut state) = deque.state.lock() {
            popped = if state.queue.is_empty() {
                None
            } else {
                let m = state.queue.pop_front().unwrap() as *mut vdynamic;
                Some((m, state.queue.contains(&(m as *mut c_void))))
            };
        } else {
            return ptr::null_mut();
        }
        // The deque lock is released here; the GC lock is only ever taken
        // after it, never the other way round.
        if let Some((m, still_queued)) = popped {
            if !still_queued && !m.is_null() {
                crate::gc::gc_remove_persistent(m);
            }
            return m;
        }
        // Empty: blocking pop waits cooperatively while fibers exist;
        // otherwise keep the non-blocking null return (single-threaded,
        // nothing can ever push).
        if !block || !crate::fiber::fibers_active() {
            return ptr::null_mut();
        }
        let waiter = crate::fiber::new_waiter();
        if let Ok(mut state) = deque.state.lock() {
            state.waiters.push_back(waiter);
        } else {
            return ptr::null_mut();
        }
        let _ = crate::fiber::park(waiter, None);
        if let Ok(mut state) = deque.state.lock() {
            remove_waiter(&mut state.waiters, waiter);
        }
    }
}

// ============================================================================
// Thread-local storage
// ============================================================================

struct HlTls {
    gc_value: bool,
    values: std::sync::Mutex<HashMap<u64, *mut c_void>>,
}

#[no_mangle]
pub unsafe extern "C" fn hlp_tls_alloc(gc_value: bool) -> *mut c_void {
    Box::into_raw(Box::new(HlTls {
        gc_value,
        values: std::sync::Mutex::new(HashMap::new()),
    })) as *mut c_void
}

#[no_mangle]
pub unsafe extern "C" fn hlp_tls_set(tls: *mut c_void, value: *mut c_void) {
    if tls.is_null() {
        return;
    }
    let tls = tls as *mut HlTls;
    let id = crate::fiber::current_owner();
    if (*tls).gc_value && !value.is_null() {
        crate::gc::gc_add_persistent(value as *mut vdynamic);
    }
    let mut values = (*tls).values.lock().unwrap();
    let old = if value.is_null() {
        values.remove(&id)
    } else {
        values.insert(id, value)
    };
    if (*tls).gc_value {
        if let Some(old) = old {
            if !old.is_null() && !values.values().any(|candidate| *candidate == old) {
                crate::gc::gc_remove_persistent(old as *mut vdynamic);
            }
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_tls_get(tls: *mut c_void) -> *mut c_void {
    if tls.is_null() {
        return ptr::null_mut();
    }
    let tls = tls as *mut HlTls;
    (*tls)
        .values
        .lock()
        .unwrap()
        .get(&crate::fiber::current_owner())
        .copied()
        .unwrap_or(ptr::null_mut())
}

#[no_mangle]
pub unsafe extern "C" fn hlp_tls_free(tls: *mut c_void) {
    if tls.is_null() {
        return;
    }
    let tls = Box::from_raw(tls as *mut HlTls);
    if tls.gc_value {
        let values = tls.values.lock().unwrap();
        let unique: HashSet<usize> = values
            .values()
            .filter(|value| !value.is_null())
            .map(|value| *value as usize)
            .collect();
        for value in unique {
            crate::gc::gc_remove_persistent(value as *mut vdynamic);
        }
        drop(values);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hl_tls_alloc(gc_value: bool) -> *mut c_void {
    hlp_tls_alloc(gc_value)
}

#[no_mangle]
pub unsafe extern "C" fn hl_tls_set(tls: *mut c_void, value: *mut c_void) {
    hlp_tls_set(tls, value);
}

#[no_mangle]
pub unsafe extern "C" fn hl_tls_get(tls: *mut c_void) -> *mut c_void {
    hlp_tls_get(tls)
}

#[no_mangle]
pub unsafe extern "C" fn hl_tls_free(tls: *mut c_void) {
    hlp_tls_free(tls);
}

// ============================================================================
// Thread
// ============================================================================

#[no_mangle]
pub unsafe extern "C" fn hlp_thread_current() -> *mut c_void {
    if let Some(handle) = crate::fiber::current_handle() {
        return handle;
    }
    // One agent, and the identity has only to be stable and non-null: it is
    // compared, never dereferenced.
    #[cfg(not(any(unix, windows)))]
    {
        1 as *mut c_void
    }
    #[cfg(unix)]
    {
        libc::pthread_self() as usize as *mut c_void
    }
    // Only ever compared for identity, so the thread id stands in for the
    // pthread_t handle.
    #[cfg(windows)]
    {
        windows_sys::Win32::System::Threading::GetCurrentThreadId() as usize as *mut c_void
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_thread_create(callback: *mut c_void) -> *mut c_void {
    // Haxe threads run as cooperative stackful fibers (krio). AIR V2 bodies
    // that the host resolves to native code are distributed over worker OS
    // threads; pure-interpreter bodies remain on the main scheduler. Upstream
    // prim is _FUN(_VOID,_NO_ARG): callback is a vclosure*.
    crate::fiber::thread_create(callback as *mut crate::hl::vclosure)
}

#[no_mangle]
pub unsafe extern "C" fn hlp_thread_set_name(_thread: *mut c_void, _name: *const u8) {
    // No-op stub
}

#[no_mangle]
pub unsafe extern "C" fn hlp_thread_get_name(_thread: *mut c_void) -> *const u8 {
    ptr::null()
}

#[no_mangle]
pub unsafe extern "C" fn hl_thread_current() -> *mut c_void {
    hlp_thread_current()
}

/// Upstream starts an OS thread running `callback(param)`. ash has none to
/// give -- Haxe threads are krio fibers on one scheduler -- and the callbacks
/// that arrive here are not fiber-shaped either.
///
/// ui.hdll's sentinel, the only caller in reach, is an OS-thread watchdog by
/// construction: it sleeps in an unbounded loop that never yields, and when it
/// fires it calls SuspendThread/GetThreadContext on the thread it watches and
/// rewrites that thread's instruction pointer to graft a call onto it. Run as
/// a fiber it would share, and so starve, the very thread it is watching; run
/// at all against ash's main thread it would be rewriting the pc of a stack
/// carrying JIT frames.
///
/// Null is upstream's own answer for "no thread was started", and
/// `ui_start_sentinel` only stores the result -- nothing dereferences it. A
/// program that asks for a sentinel therefore runs without a watchdog, rather
/// than not running at all.
#[no_mangle]
pub unsafe extern "C" fn hl_thread_start(
    _callback: *mut c_void,
    _param: *mut c_void,
    _with_gc: bool,
) -> *mut c_void {
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn hl_thread_yield() {
    if crate::fiber::fibers_active() {
        crate::fiber::hlp_fiber_poll();
    } else {
        std::thread::yield_now();
    }
}

// ============================================================================
// Atomics
// ============================================================================

#[no_mangle]
pub unsafe extern "C" fn hlp_atomic_add32(a: *mut i32, b: i32) -> i32 {
    (*(a as *const AtomicI32)).fetch_add(b, Ordering::SeqCst)
}

#[no_mangle]
pub unsafe extern "C" fn hlp_atomic_sub32(a: *mut i32, b: i32) -> i32 {
    (*(a as *const AtomicI32)).fetch_sub(b, Ordering::SeqCst)
}

#[no_mangle]
pub unsafe extern "C" fn hlp_atomic_and32(a: *mut i32, b: i32) -> i32 {
    (*(a as *const AtomicI32)).fetch_and(b, Ordering::SeqCst)
}

#[no_mangle]
pub unsafe extern "C" fn hlp_atomic_or32(a: *mut i32, b: i32) -> i32 {
    (*(a as *const AtomicI32)).fetch_or(b, Ordering::SeqCst)
}

#[no_mangle]
pub unsafe extern "C" fn hlp_atomic_xor32(a: *mut i32, b: i32) -> i32 {
    (*(a as *const AtomicI32)).fetch_xor(b, Ordering::SeqCst)
}

#[no_mangle]
pub unsafe extern "C" fn hlp_atomic_compare_exchange32(
    a: *mut i32,
    expected: i32,
    replacement: i32,
) -> i32 {
    match (*(a as *const AtomicI32)).compare_exchange(
        expected,
        replacement,
        Ordering::SeqCst,
        Ordering::SeqCst,
    ) {
        Ok(v) => v,
        Err(v) => {
            // A failed CAS is the polling primitive used by Haxe's atomic
            // spin loops. Cooperative threads cannot make progress unless
            // the losing fiber gives the owner a turn.
            if crate::fiber::fibers_active() {
                crate::fiber::block_yield();
            }
            v
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_atomic_compare_exchange_ptr(
    a: *mut *mut c_void,
    expected: *mut c_void,
    replacement: *mut c_void,
) -> *mut c_void {
    use std::sync::atomic::AtomicPtr;
    match (*(a as *const AtomicPtr<c_void>)).compare_exchange(
        expected,
        replacement,
        Ordering::SeqCst,
        Ordering::SeqCst,
    ) {
        Ok(v) => v,
        Err(v) => {
            if crate::fiber::fibers_active() {
                crate::fiber::block_yield();
            }
            v
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_atomic_exchange32(a: *mut i32, replacement: i32) -> i32 {
    (*(a as *const AtomicI32)).swap(replacement, Ordering::SeqCst)
}

#[no_mangle]
pub unsafe extern "C" fn hlp_atomic_exchange_ptr(
    a: *mut *mut c_void,
    replacement: *mut c_void,
) -> *mut c_void {
    use std::sync::atomic::AtomicPtr;
    (*(a as *const AtomicPtr<c_void>)).swap(replacement, Ordering::SeqCst)
}

#[no_mangle]
pub unsafe extern "C" fn hlp_atomic_load32(a: *const i32) -> i32 {
    (*(a as *const AtomicI32)).load(Ordering::SeqCst)
}

#[no_mangle]
pub unsafe extern "C" fn hlp_atomic_load_ptr(a: *const *mut c_void) -> *mut c_void {
    use std::sync::atomic::AtomicPtr;
    (*(a as *const AtomicPtr<c_void>)).load(Ordering::SeqCst)
}

#[no_mangle]
pub unsafe extern "C" fn hlp_atomic_store32(a: *mut i32, value: i32) -> i32 {
    (*(a as *const AtomicI32)).store(value, Ordering::SeqCst);
    value
}

#[no_mangle]
pub unsafe extern "C" fn hlp_atomic_store_ptr(
    a: *mut *mut c_void,
    value: *mut c_void,
) -> *mut c_void {
    use std::sync::atomic::AtomicPtr;
    (*(a as *const AtomicPtr<c_void>)).store(value, Ordering::SeqCst);
    value
}

// ============================================================================
// GC blocking sections
// ============================================================================

/// Upstream hl_blocking (gc.c): enter or leave a section during which the
/// collector must not wait for this thread.
///
/// The logical nesting depth remains fiber-owned so one Haxe thread cannot
/// consume another's `Gc.blocking(false)`. The GC also publishes the current
/// OS mutator's stack/register context: once marked blocking, an HDLL promises
/// not to execute HL code until the matching leave, so collection need not
/// wait for an AIR V2 poll from that worker.
/// HashLink's `hl_thread_info`, as much of it as anything outside the runtime
/// actually reads.
///
/// ash keeps no thread registry, so `hl_get_thread` used to answer null. That
/// is not a graceful "no registry": ui.hdll's sentinel stores the pointer and
/// its loop condition reads `s->main_thread->gc_blocking` from another thread,
/// so null is an access violation the moment the sentinel starts. The tail is
/// zeroed padding out to upstream's size, so an hdll reading a field ash does
/// not keep reads a zero instead of running off the end of the allocation.
#[repr(C)]
pub struct ThreadInfo {
    pub thread_id: i32,
    pub gc_blocking: i32,
    /// `stack_top` through `exc_value` upstream: six pointers ash keeps
    /// nowhere. Named padding, so `flags` lands on upstream's offset.
    _through_exc_value: [u8; 48],
    /// Upstream's `flags`, the word `hl_set_thread_flags` edits.
    pub flags: i32,
    _rest: [u8; 2900],
}

// The padding above is load-bearing in one direction only -- an hdll indexes
// this record by upstream's offsets -- and nothing else would catch a slip.
const _: () = assert!(std::mem::offset_of!(ThreadInfo, flags) == 56);
const _: () = assert!(std::mem::size_of::<ThreadInfo>() == 2960);

thread_local! {
    static THREAD_INFO: std::cell::Cell<*mut ThreadInfo> =
        const { std::cell::Cell::new(std::ptr::null_mut()) };
}

/// This thread's record, created on first use.
///
/// Deliberately leaked: a native library may hold the pointer past the point
/// where thread-local destructors would have run, and a watchdog polling freed
/// memory is worse than one polling a stale record.
pub(crate) fn thread_info() -> *mut ThreadInfo {
    THREAD_INFO.with(|slot| {
        let mut info = slot.get();
        if info.is_null() {
            info = Box::into_raw(Box::new(ThreadInfo {
                thread_id: current_os_thread_id() as i32,
                gc_blocking: 0,
                _through_exc_value: [0; 48],
                flags: 0,
                _rest: [0; 2900],
            }));
            slot.set(info);
        }
        info
    })
}

/// This thread's OS id, in the form `hlp_thread_current` hands out.
fn current_os_thread_id() -> usize {
    #[cfg(not(any(unix, windows)))]
    {
        1
    }
    #[cfg(unix)]
    {
        unsafe { libc::pthread_self() as usize }
    }
    #[cfg(windows)]
    {
        unsafe { windows_sys::Win32::System::Threading::GetCurrentThreadId() as usize }
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_blocking(b: bool) {
    if crate::fiber::update_gc_blocking_depth(b) {
        let _ = crate::gc::gc_set_blocking(b);
    }
    // Published for anyone holding this thread's record -- see ThreadInfo.
    (*thread_info()).gc_blocking = i32::from(crate::fiber::is_gc_blocking());
}

#[no_mangle]
pub unsafe extern "C" fn hl_is_blocking() -> bool {
    crate::fiber::is_gc_blocking()
}

/// Upstream `hl_set_thread_flags` (gc.c): a read-modify-write of this
/// thread's flag word, `mask` selecting the bits `flags` supplies.
///
/// Nothing inside ash reads them. HL_THREAD_INVISIBLE steers a thread
/// registry the collector here does not keep, the profiler-pause bit belongs
/// to a profiler ash implements elsewhere, and the tracking bits above the
/// shift gate `hlp_track_call`, which is a stub. The write still has to
/// land, because the word is not private: an hdll reaching
/// `hl_get_thread()->flags` reads this slot, and that crossing is the whole
/// observable contract.
#[no_mangle]
pub unsafe extern "C" fn hlp_set_thread_flags(flags: i32, mask: i32) {
    let t = thread_info();
    (*t).flags = ((*t).flags & !mask) | flags;
}

// ============================================================================
// Memory tracking stubs
// ============================================================================

#[no_mangle]
pub unsafe extern "C" fn hlp_track_init() {}

#[no_mangle]
pub unsafe extern "C" fn hlp_track_call(_mode: i32, _data: *mut c_void) {}

// ============================================================================
// Process stubs
// ============================================================================

#[no_mangle]
pub unsafe extern "C" fn hlp_sys_exit(code: i32) {
    // ASH_DEBUG_EXIT=1 prints the exit site backtrace (debugging aid).
    if env_flag!(os "ASH_DEBUG_EXIT") {
        eprintln!("[ash] sys_exit({})", code);
        eprintln!("{}", std::backtrace::Backtrace::force_capture());
    }
    std::process::exit(code);
}

#[cfg(test)]
mod foreign_thread_tests {
    use std::sync::atomic::{AtomicU32, AtomicUsize, Ordering};
    use std::sync::Arc;

    /// Threads the runtime did not create — the ones a native library spawns
    /// for its own callbacks — must still exclude each other on an HL mutex.
    ///
    /// This is the shape hxDatachannel uses: libdatachannel's callback threads
    /// take `hl_mutex_acquire`, push onto an intrusive list, and release. If
    /// the mutex lets two of them in at once the list is linked and freed
    /// concurrently, which surfaces as "pointer being freed was not
    /// allocated" inside the consumer.
    #[test]
    fn foreign_threads_exclude_each_other_on_a_mutex() {
        unsafe {
            crate::fiber::mark_main_thread();
            let m = super::hlp_mutex_alloc(false);
            assert!(!m.is_null());

            // Held by exactly one thread at a time, checked from inside.
            let inside = Arc::new(AtomicU32::new(0));
            let overlaps = Arc::new(AtomicUsize::new(0));
            // Plain counter, written only under the mutex: a lost update is
            // the same race the intrusive list loses a node to.
            let counter = Arc::new(AtomicUsize::new(0));

            let addr = m as usize;
            let mut handles = Vec::new();
            for _ in 0..4 {
                let inside = Arc::clone(&inside);
                let overlaps = Arc::clone(&overlaps);
                let counter = Arc::clone(&counter);
                handles.push(std::thread::spawn(move || {
                    for _ in 0..2000 {
                        super::hlp_mutex_acquire(addr as *mut std::ffi::c_void);
                        if inside.fetch_add(1, Ordering::AcqRel) != 0 {
                            overlaps.fetch_add(1, Ordering::Relaxed);
                        }
                        let seen = counter.load(Ordering::Relaxed);
                        std::hint::spin_loop();
                        counter.store(seen + 1, Ordering::Relaxed);
                        inside.fetch_sub(1, Ordering::AcqRel);
                        super::hlp_mutex_release(addr as *mut std::ffi::c_void);
                    }
                }));
            }
            for h in handles {
                h.join().unwrap();
            }
            super::hlp_mutex_free(m);

            assert_eq!(
                overlaps.load(Ordering::Relaxed),
                0,
                "two runtime-external threads were inside the mutex at once"
            );
            assert_eq!(
                counter.load(Ordering::Relaxed),
                4 * 2000,
                "updates were lost under the mutex"
            );
        }
    }

    /// A blocking semaphore acquire must not return until it holds a permit.
    ///
    /// The producer side of the hxDatachannel queue signals from a library
    /// thread and the consumer drains on the main thread, so the permit count
    /// is the only thing keeping the queue length and the drain loop agreed.
    #[test]
    fn a_blocking_acquire_on_a_foreign_thread_waits_for_its_permit() {
        unsafe {
            crate::fiber::mark_main_thread();
            let sem = super::hlp_semaphore_alloc(0);
            assert!(!sem.is_null());
            let addr = sem as usize;

            let acquired = Arc::new(AtomicUsize::new(0));
            let a2 = Arc::clone(&acquired);
            let waiter = std::thread::spawn(move || {
                super::hlp_semaphore_acquire(addr as *mut std::ffi::c_void);
                a2.fetch_add(1, Ordering::Release);
            });

            // No permit has been released, so nothing may get through.
            std::thread::sleep(std::time::Duration::from_millis(150));
            let early = acquired.load(Ordering::Acquire);

            super::hlp_semaphore_release(sem);
            waiter.join().unwrap();
            super::hlp_semaphore_free(sem);

            assert_eq!(early, 0, "acquire returned before any permit was released");
        }
    }

    /// Every permit a producer releases is taken exactly once.
    #[test]
    fn semaphore_permits_are_conserved_across_foreign_threads() {
        unsafe {
            let sem = super::hlp_semaphore_alloc(0);
            let addr = sem as usize;
            const PRODUCERS: usize = 4;
            const PER: usize = 1000;

            let mut handles = Vec::new();
            for _ in 0..PRODUCERS {
                handles.push(std::thread::spawn(move || {
                    for _ in 0..PER {
                        super::hlp_semaphore_release(addr as *mut std::ffi::c_void);
                    }
                }));
            }
            for h in handles {
                h.join().unwrap();
            }

            let mut taken = 0usize;
            while super::hlp_semaphore_try_acquire(sem, std::ptr::null_mut()) {
                taken += 1;
                if taken > PRODUCERS * PER {
                    break;
                }
            }
            super::hlp_semaphore_free(sem);
            assert_eq!(taken, PRODUCERS * PER, "permits were lost or duplicated");
        }
    }

    /// The hxDatachannel queue in miniature: several library threads push
    /// onto one intrusive list under a mutex and signal a counting semaphore,
    /// while the main thread drains it with `try_acquire`.
    ///
    /// Each node carries a magic word, so a node published before its payload
    /// store is visible, or handed out twice, is caught here rather than as
    /// `pointer being freed was not allocated` inside the consumer.
    #[test]
    fn multiple_producers_one_consumer_over_a_mutex_and_semaphore() {
        const MAGIC: usize = 0x5EA1_600D;
        const PRODUCERS: usize = 4;
        const PER: usize = 500;

        struct Node {
            magic: usize,
            payload: Box<[u8; 64]>,
            seq: usize,
            next: *mut Node,
        }

        // The list head, exactly as the C shim keeps it: a plain global that
        // only the mutex protects.
        static HEAD: AtomicUsize = AtomicUsize::new(0);

        unsafe {
            crate::fiber::mark_main_thread();
            let m = super::hlp_mutex_alloc(false);
            let sem = super::hlp_semaphore_alloc(0);
            let (maddr, saddr) = (m as usize, sem as usize);
            HEAD.store(0, Ordering::Release);

            let mut handles = Vec::new();
            for p in 0..PRODUCERS {
                handles.push(std::thread::spawn(move || {
                    for i in 0..PER {
                        super::hlp_mutex_acquire(maddr as *mut std::ffi::c_void);
                        let node = Box::into_raw(Box::new(Node {
                            magic: MAGIC,
                            payload: Box::new([(p as u8).wrapping_add(i as u8); 64]),
                            seq: p * PER + i,
                            next: HEAD.load(Ordering::Relaxed) as *mut Node,
                        }));
                        HEAD.store(node as usize, Ordering::Relaxed);
                        super::hlp_semaphore_release(saddr as *mut std::ffi::c_void);
                        super::hlp_mutex_release(maddr as *mut std::ffi::c_void);
                    }
                }));
            }

            // Consumer: drain while permits last, exactly as process_events does.
            let mut seen = vec![false; PRODUCERS * PER];
            let mut drained = 0usize;
            let deadline = std::time::Instant::now() + std::time::Duration::from_secs(30);
            while drained < PRODUCERS * PER && std::time::Instant::now() < deadline {
                if !super::hlp_semaphore_try_acquire(sem, std::ptr::null_mut()) {
                    std::thread::yield_now();
                    continue;
                }
                super::hlp_mutex_acquire(m);
                let head = HEAD.load(Ordering::Relaxed) as *mut Node;
                assert!(!head.is_null(), "a permit outlived its node");
                assert_eq!((*head).magic, MAGIC, "node observed before it was published");
                assert_eq!((*head).payload.len(), 64);
                let seq = (*head).seq;
                assert!(!seen[seq], "node {seq} was drained twice");
                seen[seq] = true;
                HEAD.store((*head).next as usize, Ordering::Relaxed);
                drop(Box::from_raw(head));
                drained += 1;
                super::hlp_mutex_release(m);
            }

            for h in handles {
                h.join().unwrap();
            }
            super::hlp_semaphore_free(sem);
            super::hlp_mutex_free(m);

            assert_eq!(drained, PRODUCERS * PER, "queue lost messages");
            assert_eq!(HEAD.load(Ordering::Relaxed), 0, "queue was left non-empty");
        }
    }
}

/// hxDatachannel's callback queue, reproduced at the level that matters: the
/// exact head/end pointer protocol from `datachannel.c`, over ash's real HL
/// mutex and semaphore.
///
/// Driving libdatachannel itself would need two peers and a network, and would
/// reproduce this only by luck. The defect is in the queue bookkeeping, so the
/// queue is what is modelled -- `callback_result_alloc` linking through an end
/// pointer, and `process_events` draining under a `try_acquire` that may leave
/// early.
#[cfg(test)]
mod datachannel_queue_tests {
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::{Arc, Mutex};

    struct Node {
        next: *mut Node,
        seq: usize,
    }

    /// The globals `datachannel.c` keeps, plus a record of which nodes are
    /// still allocated. Held per run rather than in statics so two tests can
    /// run at once without sharing a queue.
    struct Queue {
        head: *mut Node,
        end: *mut Node,
        live: std::collections::HashSet<usize>,
    }
    unsafe impl Send for Queue {}

    struct Counters {
        /// Times the consumer freed a node that `end` still pointed at.
        dangling: AtomicUsize,
        /// Times a producer reached a node that was already freed.
        uaf: AtomicUsize,
        /// Sum of drained sequence numbers, so a callback dropped or handed
        /// out twice shows up even when the pointer bookkeeping survives.
        seq_sum: AtomicUsize,
    }

    /// `callback_result_alloc()`, verbatim in structure.
    fn callback_result_alloc(q: &mut Queue, c: &Counters, seq: usize) {
        let fresh = Box::into_raw(Box::new(Node {
            next: std::ptr::null_mut(),
            seq,
        }));
        if q.end.is_null() {
            q.head = fresh;
        } else if q.live.contains(&(q.end as usize)) {
            unsafe { (*q.end).next = fresh };
        } else {
            // The write upstream performs unconditionally: `end` may name a
            // node the consumer has already freed.
            c.uaf.fetch_add(1, Ordering::Relaxed);
        }
        q.end = fresh;
        q.live.insert(fresh as usize);
    }

    fn run(fixed: bool) -> (usize, usize) {
        const PRODUCERS: usize = 3;
        const PER: usize = 400;
        const TOTAL: usize = PRODUCERS * PER;

        unsafe {
            crate::fiber::mark_main_thread();
            let m = super::hlp_mutex_alloc(false);
            let sem = super::hlp_semaphore_alloc(0);
            let (ma, sa) = (m as usize, sem as usize);

            let q = Arc::new(Mutex::new(Queue {
                head: std::ptr::null_mut(),
                end: std::ptr::null_mut(),
                live: std::collections::HashSet::new(),
            }));
            let c = Arc::new(Counters {
                dangling: AtomicUsize::new(0),
                uaf: AtomicUsize::new(0),
                seq_sum: AtomicUsize::new(0),
            });

            let mut handles = Vec::new();
            for p in 0..PRODUCERS {
                let (q, c) = (Arc::clone(&q), Arc::clone(&c));
                handles.push(std::thread::spawn(move || {
                    for i in 0..PER {
                        super::hlp_mutex_acquire(ma as *mut std::ffi::c_void);
                        callback_result_alloc(&mut q.lock().unwrap(), &c, p * PER + i);
                        super::hlp_semaphore_release(sa as *mut std::ffi::c_void);
                        super::hlp_mutex_release(ma as *mut std::ffi::c_void);
                    }
                }));
            }

            // `process_events()`: drain while permits last, then reset `end`.
            let mut drained = 0usize;
            let deadline = std::time::Instant::now() + std::time::Duration::from_secs(30);
            while drained < TOTAL && std::time::Instant::now() < deadline {
                let mut looped = false;
                loop {
                    if q.lock().unwrap().head.is_null()
                        || !super::hlp_semaphore_try_acquire(sem, std::ptr::null_mut())
                    {
                        break;
                    }
                    super::hlp_mutex_acquire(m);
                    {
                        let mut g = q.lock().unwrap();
                        let res = g.head;
                        g.head = (*res).next;
                        // The fix: a node leaving the list must also leave the
                        // end pointer, under the lock that frees it.
                        if fixed && g.head.is_null() {
                            g.end = std::ptr::null_mut();
                        }
                        g.live.remove(&(res as usize));
                        // Checked as an invariant rather than as a race: if
                        // `end` still names the node being freed, the next
                        // producer writes through it. Whether one arrives in
                        // the window decides when the heap corruption
                        // surfaces, not whether it will.
                        if g.end == res {
                            c.dangling.fetch_add(1, Ordering::Relaxed);
                        }
                        c.seq_sum.fetch_add((*res).seq, Ordering::Relaxed);
                        drop(Box::from_raw(res));
                        drained += 1;
                    }
                    super::hlp_mutex_release(m);
                    looped = true;
                }
                if looped {
                    super::hlp_mutex_acquire(m);
                    q.lock().unwrap().end = std::ptr::null_mut();
                    super::hlp_mutex_release(m);
                }
                std::thread::yield_now();
            }

            for h in handles {
                h.join().unwrap();
            }
            super::hlp_semaphore_free(sem);
            super::hlp_mutex_free(m);

            assert_eq!(drained, TOTAL, "queue lost callbacks");
            // 0 + 1 + ... + (TOTAL-1), reached only if each node drained once.
            assert_eq!(
                c.seq_sum.load(Ordering::Relaxed),
                TOTAL * (TOTAL - 1) / 2,
                "a queued callback was dropped or drained twice"
            );
            (
                c.dangling.load(Ordering::Relaxed),
                c.uaf.load(Ordering::Relaxed),
            )
        }
    }

    /// As written upstream, the consumer frees nodes without ever clearing
    /// `end`, leaving it naming freed memory. The next `callback_result_alloc`
    /// writes `end->next` through it, into malloc's metadata, and the process
    /// aborts later with "pointer being freed was not allocated".
    #[test]
    fn upstream_queue_leaves_end_naming_a_freed_node() {
        let (dangling, _) = run(false);
        assert!(
            dangling > 0,
            "the upstream shape should leave a dangling end pointer"
        );
    }

    /// Clearing `end` as the list empties, under the same lock that frees the
    /// node, closes it.
    #[test]
    fn clearing_end_with_the_last_node_closes_it() {
        let (dangling, uaf) = run(true);
        assert_eq!(dangling, 0, "end still named a freed node");
        assert_eq!(uaf, 0, "a producer still reached a freed node");
    }
}

/// `hlp_set_thread_flags` writes a word an hdll reads back by upstream's
/// offset, so the two const asserts above pin `flags` to offset 56 and the
/// record to 2960 bytes. Neither of them can see where the *write* goes:
/// they would still hold if the field were correct and the store landed in
/// `_through_exc_value` or `_rest`. These tests read the slot the way the
/// hdll does -- a raw i32 load 56 bytes into the record -- so a write that
/// misses is a failure rather than a silently ignored flag.
#[cfg(test)]
mod thread_flags_tests {
    use super::{hlp_set_thread_flags, thread_info, ThreadInfo};

    /// Upstream's offset for `hl_thread_info.flags`, spelled as a raw byte
    /// count rather than as the field, because the field is what is on
    /// trial.
    const FLAGS_OFFSET: usize = 56;

    /// The hdll's view: `((char*)hl_get_thread())[56]` read as an int.
    unsafe fn flags_at_upstream_offset() -> i32 {
        let t = thread_info();
        assert!(!t.is_null(), "thread_info() handed back nothing");
        (t as *const u8).add(FLAGS_OFFSET).cast::<i32>().read()
    }

    /// Restore whatever this thread's word held, so a test that runs after
    /// another on the same thread starts from the same place.
    unsafe fn set_raw(v: i32) {
        (*thread_info()).flags = v;
    }

    #[test]
    fn a_write_lands_on_the_field_the_hdll_reads() {
        unsafe {
            set_raw(0);
            hlp_set_thread_flags(0x0000_0001, 0x0000_0001);
            assert_eq!(
                flags_at_upstream_offset(),
                1,
                "the store missed offset {FLAGS_OFFSET}"
            );
            // The named field and the raw offset have to be the same word.
            assert_eq!((*thread_info()).flags, flags_at_upstream_offset());
        }
    }

    /// The whole word, one bit at a time: a store that landed a few bytes
    /// off would still pass a single-bit check often enough to look fine.
    #[test]
    fn every_bit_of_the_word_round_trips() {
        unsafe {
            for bit in 0..32 {
                let v = 1i32 << bit;
                set_raw(0);
                hlp_set_thread_flags(v, v);
                assert_eq!(
                    flags_at_upstream_offset(),
                    v,
                    "bit {bit} did not survive the round trip"
                );
            }
            set_raw(0);
        }
    }

    /// `mask` selects what is cleared, `flags` supplies what is set --
    /// upstream's `(t->flags & ~mask) | flags`.
    #[test]
    fn mask_clears_and_flags_set_within_the_mask() {
        unsafe {
            set_raw(0b1111);
            // Clear bits 0 and 1, set bit 0 back.
            hlp_set_thread_flags(0b0001, 0b0011);
            assert_eq!(flags_at_upstream_offset(), 0b1101);

            // A bit outside the mask is left exactly as it was found.
            set_raw(0b1000_0000);
            hlp_set_thread_flags(0, 0b0000_0001);
            assert_eq!(flags_at_upstream_offset(), 0b1000_0000);

            // And a bit set in `flags` but absent from `mask` is still set:
            // upstream ORs the whole of `flags`, it does not filter it.
            set_raw(0);
            hlp_set_thread_flags(0b0100, 0b0001);
            assert_eq!(flags_at_upstream_offset(), 0b0100);

            set_raw(0);
        }
    }

    /// Successive calls accumulate rather than replace -- the read half of
    /// the read-modify-write.
    #[test]
    fn successive_calls_accumulate() {
        unsafe {
            set_raw(0);
            hlp_set_thread_flags(0b0001, 0b0001);
            hlp_set_thread_flags(0b0010, 0b0010);
            hlp_set_thread_flags(0b0100, 0b0100);
            assert_eq!(flags_at_upstream_offset(), 0b0111);
            hlp_set_thread_flags(0, 0b0010);
            assert_eq!(flags_at_upstream_offset(), 0b0101);
            set_raw(0);
        }
    }

    /// The word is thread-local, which is what lets the tests above run
    /// beside each other under the harness's thread pool. Confirmed rather
    /// than assumed: a shared word would make every one of them flaky.
    #[test]
    fn the_word_is_private_to_its_thread() {
        unsafe {
            set_raw(0);
            hlp_set_thread_flags(0x00ff_0000, -1);
            let here = thread_info() as usize;

            let (there, seen) = std::thread::spawn(|| {
                // A fresh thread starts at zero, not at the parent's value.
                let start = flags_at_upstream_offset();
                hlp_set_thread_flags(0x0000_00ff, -1);
                (thread_info() as usize, (start, flags_at_upstream_offset()))
            })
            .join()
            .unwrap();

            assert_ne!(there, here, "both threads shared one ThreadInfo");
            assert_eq!(seen.0, 0, "a new thread inherited flags");
            assert_eq!(seen.1, 0x0000_00ff);
            assert_eq!(
                flags_at_upstream_offset(),
                0x00ff_0000,
                "the child's write reached this thread's word"
            );
            set_raw(0);
        }
    }

    /// Guards the padding either side of `flags`: a store wider than the
    /// field, or one bit off, shows up as a disturbed neighbour rather than
    /// as a wrong flag value.
    #[test]
    fn the_write_does_not_spill_into_its_neighbours() {
        unsafe {
            let t = thread_info();
            let base = t as *const u8;
            // 8 bytes before and after the field, sampled before and after.
            let before: [u8; 8] = std::ptr::read(base.add(FLAGS_OFFSET - 8).cast());
            let after: [u8; 8] = std::ptr::read(base.add(FLAGS_OFFSET + 4).cast());

            set_raw(0);
            hlp_set_thread_flags(-1, -1);
            assert_eq!(flags_at_upstream_offset(), -1);

            assert_eq!(
                std::ptr::read::<[u8; 8]>(base.add(FLAGS_OFFSET - 8).cast()),
                before,
                "the store reached back before the field"
            );
            assert_eq!(
                std::ptr::read::<[u8; 8]>(base.add(FLAGS_OFFSET + 4).cast()),
                after,
                "the store ran past the end of the field"
            );
            set_raw(0);
        }
    }

    /// The two const asserts restated at runtime. They cannot fire on a host
    /// where the crate already compiled, but they name what the offsets above
    /// depend on, so a future edit that moves the field fails here with a
    /// message rather than only in a `const _: ()`.
    #[test]
    fn the_record_still_matches_upstreams_layout() {
        assert_eq!(std::mem::offset_of!(ThreadInfo, flags), FLAGS_OFFSET);
        assert_eq!(std::mem::size_of::<ThreadInfo>(), 2960);
    }

    /// The signature the resolver binds by name. A `no_mangle` prim whose
    /// arity or types drift fails at the call, not at the build; naming the
    /// type here makes that a compile error.
    #[test]
    fn the_exported_signature_is_the_one_upstream_declares() {
        let f: unsafe extern "C" fn(i32, i32) = hlp_set_thread_flags;
        unsafe {
            f(0, -1);
            assert_eq!(flags_at_upstream_offset(), 0);
        }
    }
}
