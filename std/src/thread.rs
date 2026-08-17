//! Thread, mutex, semaphore, condition, TLS, and atomic operations.
//!
//! Implements the HashLink `std/thread.c` API surface needed by Heaps.io
//! and other non-trivial Haxe programs.

use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::{AtomicI32, Ordering};

use crate::hl::vdynamic;

// ============================================================================
// SDL event pump for single-threaded mode
// ============================================================================
// When Heaps' event thread isn't running (thread_create is stubbed),
// we pump SDL events during lock_wait so the window stays responsive.

static mut SDL_POLL_EVENT_FN: Option<unsafe extern "C" fn(*mut u8) -> i32> = None;
static SDL_POLL_INIT: std::sync::Once = std::sync::Once::new();

unsafe fn get_sdl_poll_event() -> Option<unsafe extern "C" fn(*mut u8) -> i32> {
    SDL_POLL_INIT.call_once(|| {
        // SDL2 is already loaded via sdl.hdll with RTLD_GLOBAL,
        // so SDL_PollEvent should be resolvable via RTLD_DEFAULT.
        let sym = libc::dlsym(
            libc::RTLD_DEFAULT,
            b"SDL_PollEvent\0".as_ptr() as *const libc::c_char,
        );
        if !sym.is_null() {
            eprintln!("[ash] SDL_PollEvent resolved at {:p}", sym);
            SDL_POLL_EVENT_FN = Some(std::mem::transmute(sym));
        } else {
            eprintln!("[ash] WARNING: SDL_PollEvent not found via dlsym");
        }
    });
    SDL_POLL_EVENT_FN
}

/// Pump all pending SDL events (non-blocking).
/// This makes the window visible and responsive on macOS,
/// which requires event processing on the main thread.
unsafe fn pump_sdl_events() {
    if let Some(poll) = get_sdl_poll_event() {
        let mut event = [0u8; 128]; // SDL_Event is 56 bytes, 128 is plenty
        while poll(event.as_mut_ptr()) != 0 {
            // Check for SDL_QUIT (type field is first u32 = 0x100)
            let event_type = u32::from_ne_bytes([event[0], event[1], event[2], event[3]]);
            if event_type == 0x100 {
                std::process::exit(0);
            }
        }
    }
}

// ============================================================================
// Mutex
// ============================================================================

#[repr(C)]
struct HlMutex {
    inner: libc::pthread_mutex_t,
}

#[no_mangle]
pub unsafe extern "C" fn hlp_mutex_alloc(_gc_thread: bool) -> *mut c_void {
    let layout = std::alloc::Layout::new::<HlMutex>();
    let ptr = std::alloc::alloc_zeroed(layout) as *mut HlMutex;
    if ptr.is_null() {
        return ptr::null_mut();
    }
    libc::pthread_mutex_init(&mut (*ptr).inner, ptr::null());
    ptr as *mut c_void
}

#[no_mangle]
pub unsafe extern "C" fn hlp_mutex_acquire(m: *mut c_void) {
    if !m.is_null() {
        libc::pthread_mutex_lock(&mut (*(m as *mut HlMutex)).inner);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_mutex_try_acquire(m: *mut c_void) -> bool {
    if m.is_null() {
        return false;
    }
    libc::pthread_mutex_trylock(&mut (*(m as *mut HlMutex)).inner) == 0
}

#[no_mangle]
pub unsafe extern "C" fn hlp_mutex_release(m: *mut c_void) {
    if !m.is_null() {
        libc::pthread_mutex_unlock(&mut (*(m as *mut HlMutex)).inner);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_mutex_free(m: *mut c_void) {
    if !m.is_null() {
        libc::pthread_mutex_destroy(&mut (*(m as *mut HlMutex)).inner);
        std::alloc::dealloc(m as *mut u8, std::alloc::Layout::new::<HlMutex>());
    }
}

// ============================================================================
// Semaphore (used by Lock)
// ============================================================================

#[repr(C)]
struct HlSemaphore {
    mutex: libc::pthread_mutex_t,
    cond: libc::pthread_cond_t,
    value: i32,
}

#[no_mangle]
pub unsafe extern "C" fn hlp_semaphore_alloc(value: i32) -> *mut c_void {
    let layout = std::alloc::Layout::new::<HlSemaphore>();
    let ptr = std::alloc::alloc_zeroed(layout) as *mut HlSemaphore;
    if ptr.is_null() {
        return ptr::null_mut();
    }
    libc::pthread_mutex_init(&mut (*ptr).mutex, ptr::null());
    libc::pthread_cond_init(&mut (*ptr).cond, ptr::null());
    (*ptr).value = value;
    ptr as *mut c_void
}

#[no_mangle]
pub unsafe extern "C" fn hlp_semaphore_acquire(sem: *mut c_void) {
    if sem.is_null() {
        return;
    }
    let s = sem as *mut HlSemaphore;
    libc::pthread_mutex_lock(&mut (*s).mutex);
    while (*s).value <= 0 {
        libc::pthread_cond_wait(&mut (*s).cond, &mut (*s).mutex);
    }
    (*s).value -= 1;
    libc::pthread_mutex_unlock(&mut (*s).mutex);
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
    libc::pthread_mutex_lock(&mut (*s).mutex);
    if (*s).value > 0 {
        (*s).value -= 1;
        libc::pthread_mutex_unlock(&mut (*s).mutex);
        return true;
    }
    // No timeout support for now — just fail immediately
    libc::pthread_mutex_unlock(&mut (*s).mutex);
    false
}

#[no_mangle]
pub unsafe extern "C" fn hlp_semaphore_release(sem: *mut c_void) {
    if sem.is_null() {
        return;
    }
    let s = sem as *mut HlSemaphore;
    libc::pthread_mutex_lock(&mut (*s).mutex);
    (*s).value += 1;
    libc::pthread_cond_signal(&mut (*s).cond);
    libc::pthread_mutex_unlock(&mut (*s).mutex);
}

#[no_mangle]
pub unsafe extern "C" fn hlp_semaphore_free(sem: *mut c_void) {
    if !sem.is_null() {
        let s = sem as *mut HlSemaphore;
        libc::pthread_mutex_destroy(&mut (*s).mutex);
        libc::pthread_cond_destroy(&mut (*s).cond);
        std::alloc::dealloc(sem as *mut u8, std::alloc::Layout::new::<HlSemaphore>());
    }
}

// ============================================================================
// Condition Variable
// ============================================================================

#[repr(C)]
struct HlCondition {
    mutex: libc::pthread_mutex_t,
    cond: libc::pthread_cond_t,
}

#[no_mangle]
pub unsafe extern "C" fn hlp_condition_alloc() -> *mut c_void {
    let layout = std::alloc::Layout::new::<HlCondition>();
    let ptr = std::alloc::alloc_zeroed(layout) as *mut HlCondition;
    if ptr.is_null() {
        return ptr::null_mut();
    }
    libc::pthread_mutex_init(&mut (*ptr).mutex, ptr::null());
    libc::pthread_cond_init(&mut (*ptr).cond, ptr::null());
    ptr as *mut c_void
}

#[no_mangle]
pub unsafe extern "C" fn hlp_condition_acquire(c: *mut c_void) {
    if !c.is_null() {
        libc::pthread_mutex_lock(&mut (*(c as *mut HlCondition)).mutex);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_condition_try_acquire(c: *mut c_void) -> bool {
    if c.is_null() {
        return false;
    }
    libc::pthread_mutex_trylock(&mut (*(c as *mut HlCondition)).mutex) == 0
}

#[no_mangle]
pub unsafe extern "C" fn hlp_condition_release(c: *mut c_void) {
    if !c.is_null() {
        libc::pthread_mutex_unlock(&mut (*(c as *mut HlCondition)).mutex);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_condition_wait(c: *mut c_void) {
    if !c.is_null() {
        let cv = c as *mut HlCondition;
        libc::pthread_cond_wait(&mut (*cv).cond, &mut (*cv).mutex);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_condition_timed_wait(c: *mut c_void, timeout: f64) -> bool {
    if c.is_null() {
        return false;
    }
    let cv = c as *mut HlCondition;
    let mut ts: libc::timespec = std::mem::zeroed();
    libc::clock_gettime(libc::CLOCK_REALTIME, &mut ts);
    let secs = timeout as i64;
    let nsecs = ((timeout - secs as f64) * 1_000_000_000.0) as i64;
    ts.tv_sec += secs;
    ts.tv_nsec += nsecs;
    if ts.tv_nsec >= 1_000_000_000 {
        ts.tv_sec += 1;
        ts.tv_nsec -= 1_000_000_000;
    }
    libc::pthread_cond_timedwait(&mut (*cv).cond, &mut (*cv).mutex, &ts) == 0
}

#[no_mangle]
pub unsafe extern "C" fn hlp_condition_signal(c: *mut c_void) {
    if !c.is_null() {
        libc::pthread_cond_signal(&mut (*(c as *mut HlCondition)).cond);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_condition_broadcast(c: *mut c_void) {
    if !c.is_null() {
        libc::pthread_cond_broadcast(&mut (*(c as *mut HlCondition)).cond);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_condition_free(c: *mut c_void) {
    if !c.is_null() {
        let cv = c as *mut HlCondition;
        libc::pthread_mutex_destroy(&mut (*cv).mutex);
        libc::pthread_cond_destroy(&mut (*cv).cond);
        std::alloc::dealloc(c as *mut u8, std::alloc::Layout::new::<HlCondition>());
    }
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
pub unsafe extern "C" fn hlp_lock_wait(lock: *mut c_void, _timeout: *mut vdynamic) -> bool {
    if lock.is_null() {
        return false;
    }
    // Exact match of HashLink's !HL_THREADS implementation:
    //   if (l->counter == 0) return false;
    //   l->counter--;
    //   return true;
    let s = lock as *mut HlSemaphore;
    if (*s).value == 0 {
        return false;
    }
    (*s).value -= 1;
    true
}

// Separate SDL pump function called from the main loop, not from lock_wait
#[no_mangle]
pub unsafe extern "C" fn hlp_pump_and_sleep() {
    pump_sdl_events();
    let sleep_ts = libc::timespec {
        tv_sec: 0,
        tv_nsec: 16_000_000,
    };
    libc::nanosleep(&sleep_ts, std::ptr::null_mut());
}

#[no_mangle]
pub unsafe extern "C" fn hlp_lock_free(lock: *mut c_void) {
    hlp_semaphore_free(lock);
}

// ============================================================================
// Deque (simple thread-safe deque stub)
// ============================================================================

#[no_mangle]
pub unsafe extern "C" fn hlp_deque_alloc() -> *mut c_void {
    // Stub: allocate an empty deque (Vec behind a mutex)
    let deque: Box<std::sync::Mutex<Vec<*mut c_void>>> =
        Box::new(std::sync::Mutex::new(Vec::new()));
    Box::into_raw(deque) as *mut c_void
}

#[no_mangle]
pub unsafe extern "C" fn hlp_deque_add(d: *mut c_void, msg: *mut vdynamic) {
    if d.is_null() {
        return;
    }
    let deque = &*(d as *const std::sync::Mutex<Vec<*mut c_void>>);
    if let Ok(mut v) = deque.lock() {
        v.push(msg as *mut c_void);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_deque_push(d: *mut c_void, msg: *mut vdynamic) {
    if d.is_null() {
        return;
    }
    let deque = &*(d as *const std::sync::Mutex<Vec<*mut c_void>>);
    if let Ok(mut v) = deque.lock() {
        v.insert(0, msg as *mut c_void);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_deque_pop(d: *mut c_void, block: bool) -> *mut vdynamic {
    if d.is_null() {
        return ptr::null_mut();
    }
    let deque = &*(d as *const std::sync::Mutex<Vec<*mut c_void>>);
    if let Ok(mut v) = deque.lock() {
        if v.is_empty() {
            return ptr::null_mut();
        }
        return v.remove(0) as *mut vdynamic;
    }
    ptr::null_mut()
}

// ============================================================================
// Thread-local storage
// ============================================================================

#[no_mangle]
pub unsafe extern "C" fn hlp_tls_alloc() -> *mut c_void {
    let mut key: libc::pthread_key_t = 0;
    if libc::pthread_key_create(&mut key, None) == 0 {
        key as usize as *mut c_void
    } else {
        ptr::null_mut()
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_tls_set(tls: *mut c_void, value: *mut c_void) {
    libc::pthread_setspecific(tls as usize as libc::pthread_key_t, value);
}

#[no_mangle]
pub unsafe extern "C" fn hlp_tls_get(tls: *mut c_void) -> *mut c_void {
    libc::pthread_getspecific(tls as usize as libc::pthread_key_t) as *mut c_void
}

#[no_mangle]
pub unsafe extern "C" fn hlp_tls_free(tls: *mut c_void) {
    libc::pthread_key_delete(tls as usize as libc::pthread_key_t);
}

// ============================================================================
// Thread
// ============================================================================

#[no_mangle]
pub unsafe extern "C" fn hlp_thread_current() -> *mut c_void {
    libc::pthread_self() as usize as *mut c_void
}

#[no_mangle]
pub unsafe extern "C" fn hlp_thread_create(_callback: *mut c_void) -> *mut c_void {
    // Thread creation requires marshaling a HashLink closure through pthread.
    // Stub for now — returns null (thread not created).
    eprintln!("[ash] warning: hlp_thread_create not yet implemented");
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_thread_set_name(_thread: *mut c_void, _name: *const u8) {
    // No-op stub
}

#[no_mangle]
pub unsafe extern "C" fn hlp_thread_get_name(_thread: *mut c_void) -> *const u8 {
    ptr::null()
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
        Ok(v) | Err(v) => v,
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
        Ok(v) | Err(v) => v,
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
    eprintln!("[ash] sys_exit({})", code);
    // Print backtrace for debugging
    eprintln!("{}", std::backtrace::Backtrace::force_capture());
    std::process::exit(code);
}
