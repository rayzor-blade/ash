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
    owner: Option<u32>,
    depth: u32,
    waiters: VecDeque<Waiter>,
}

unsafe fn mutex_try_acquire_inner(mutex: *mut HlMutex) -> bool {
    let current = crate::fiber::current_id();
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
            let current = crate::fiber::current_id();
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
            state.owner = Some(crate::fiber::current_id());
            state.depth = 1;
            return;
        }
    }
}

unsafe fn mutex_release_inner(mutex: *mut HlMutex) {
    let mut state = (*mutex).state.lock().unwrap();
    if state.owner != Some(crate::fiber::current_id()) || state.depth == 0 {
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
        if deadline.is_some_and(|limit| Instant::now() >= limit)
            || !crate::fiber::fibers_active()
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
    owner: Option<u32>,
    depth: u32,
    mutex_waiters: VecDeque<Waiter>,
    waiters: VecDeque<Waiter>,
}

unsafe fn condition_mutex_acquire(c: *mut HlCondition) {
    loop {
        let waiter = {
            let current = crate::fiber::current_id();
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
    let current = crate::fiber::current_id();
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
    if state.owner != Some(crate::fiber::current_id()) || state.depth == 0 {
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
    if !crate::fiber::fibers_active() {
        return true;
    }
    let (waiter, depth) = {
        let current = crate::fiber::current_id();
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
    std::thread::sleep(std::time::Duration::from_millis(16));
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
    values: std::sync::Mutex<HashMap<u32, *mut c_void>>,
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
    let id = crate::fiber::current_id();
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
        .get(&crate::fiber::current_id())
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
#[no_mangle]
pub unsafe extern "C" fn hlp_blocking(b: bool) {
    if crate::fiber::update_gc_blocking_depth(b) {
        let _ = crate::gc::gc_set_blocking(b);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hl_is_blocking() -> bool {
    crate::fiber::is_gc_blocking()
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
