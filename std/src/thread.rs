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

#[cfg(unix)]
unsafe fn resolve_sdl_poll_event() -> *mut c_void {
    // SDL2 is already loaded via sdl.hdll with RTLD_GLOBAL,
    // so SDL_PollEvent should be resolvable via RTLD_DEFAULT.
    libc::dlsym(libc::RTLD_DEFAULT, c"SDL_PollEvent".as_ptr())
}

#[cfg(windows)]
unsafe fn resolve_sdl_poll_event() -> *mut c_void {
    use windows_sys::Win32::System::LibraryLoader::{GetModuleHandleA, GetProcAddress};
    // Win32 has no RTLD_DEFAULT — a symbol is only reachable through the module
    // that exports it — so the SDL runtime sdl.hdll pulled in is probed by name.
    for dll in [c"SDL2.dll", c"SDL3.dll"] {
        let module = GetModuleHandleA(dll.as_ptr() as *const u8);
        if module.is_null() {
            continue;
        }
        if let Some(sym) = GetProcAddress(module, c"SDL_PollEvent".as_ptr() as *const u8) {
            return sym as usize as *mut c_void;
        }
    }
    ptr::null_mut()
}

unsafe fn get_sdl_poll_event() -> Option<unsafe extern "C" fn(*mut u8) -> i32> {
    SDL_POLL_INIT.call_once(|| {
        let sym = resolve_sdl_poll_event();
        if !sym.is_null() {
            eprintln!("[ash] SDL_PollEvent resolved at {:p}", sym);
            SDL_POLL_EVENT_FN = Some(std::mem::transmute::<
                *mut c_void,
                unsafe extern "C" fn(*mut u8) -> i32,
            >(sym));
        } else {
            eprintln!("[ash] WARNING: SDL_PollEvent not found");
        }
    });
    SDL_POLL_EVENT_FN
}

/// Pump all pending SDL events (non-blocking).
/// This makes the window visible and responsive on macOS,
/// which requires event processing on the main thread.
pub(crate) unsafe fn pump_sdl_events() {
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
// Raw OS synchronisation primitives
// ============================================================================
// hl hands mutexes/conditions to Haxe as opaque pointers and splits every
// acquire/release across two separate C calls, so std's guard-based Mutex
// cannot express them — the OS primitives are driven directly instead.

#[cfg(unix)]
mod sys {
    pub type RawMutex = libc::pthread_mutex_t;
    pub type RawCond = libc::pthread_cond_t;

    pub unsafe fn mutex_init(m: *mut RawMutex, recursive: bool) {
        if !recursive {
            libc::pthread_mutex_init(m, std::ptr::null());
            return;
        }
        let mut attr: libc::pthread_mutexattr_t = std::mem::zeroed();
        libc::pthread_mutexattr_init(&mut attr);
        libc::pthread_mutexattr_settype(&mut attr, libc::PTHREAD_MUTEX_RECURSIVE);
        libc::pthread_mutex_init(m, &attr);
        libc::pthread_mutexattr_destroy(&mut attr);
    }

    pub unsafe fn mutex_lock(m: *mut RawMutex) {
        libc::pthread_mutex_lock(m);
    }

    pub unsafe fn mutex_try_lock(m: *mut RawMutex) -> bool {
        libc::pthread_mutex_trylock(m) == 0
    }

    pub unsafe fn mutex_unlock(m: *mut RawMutex) {
        libc::pthread_mutex_unlock(m);
    }

    pub unsafe fn mutex_destroy(m: *mut RawMutex) {
        libc::pthread_mutex_destroy(m);
    }

    pub unsafe fn cond_init(c: *mut RawCond) {
        libc::pthread_cond_init(c, std::ptr::null());
    }

    pub unsafe fn cond_signal(c: *mut RawCond) {
        libc::pthread_cond_signal(c);
    }

    pub unsafe fn cond_broadcast(c: *mut RawCond) {
        libc::pthread_cond_broadcast(c);
    }

    pub unsafe fn cond_destroy(c: *mut RawCond) {
        libc::pthread_cond_destroy(c);
    }
}

#[cfg(windows)]
mod sys {
    use windows_sys::Win32::System::Threading::{
        DeleteCriticalSection, EnterCriticalSection, InitializeConditionVariable,
        InitializeCriticalSection, LeaveCriticalSection, TryEnterCriticalSection,
        WakeAllConditionVariable, WakeConditionVariable, CONDITION_VARIABLE, CRITICAL_SECTION,
    };

    pub type RawMutex = CRITICAL_SECTION;
    pub type RawCond = CONDITION_VARIABLE;

    // A CRITICAL_SECTION is re-entrant for its owning thread and offers no way
    // to opt out, so `recursive` is unenforceable here. The one caller that
    // asks for a plain lock (the semaphore) only ever takes it in balanced
    // pairs, so the extra re-entrancy is unobservable.
    pub unsafe fn mutex_init(m: *mut RawMutex, _recursive: bool) {
        InitializeCriticalSection(m);
    }

    pub unsafe fn mutex_lock(m: *mut RawMutex) {
        EnterCriticalSection(m);
    }

    pub unsafe fn mutex_try_lock(m: *mut RawMutex) -> bool {
        TryEnterCriticalSection(m) != 0
    }

    pub unsafe fn mutex_unlock(m: *mut RawMutex) {
        LeaveCriticalSection(m);
    }

    pub unsafe fn mutex_destroy(m: *mut RawMutex) {
        DeleteCriticalSection(m);
    }

    pub unsafe fn cond_init(c: *mut RawCond) {
        InitializeConditionVariable(c);
    }

    pub unsafe fn cond_signal(c: *mut RawCond) {
        WakeConditionVariable(c);
    }

    pub unsafe fn cond_broadcast(c: *mut RawCond) {
        WakeAllConditionVariable(c);
    }

    // Win32 condition variables own no resources and have no destructor.
    pub unsafe fn cond_destroy(_c: *mut RawCond) {}
}

// ============================================================================
// Mutex
// ============================================================================

#[repr(C)]
struct HlMutex {
    inner: sys::RawMutex,
}

#[no_mangle]
pub unsafe extern "C" fn hlp_mutex_alloc(_gc_thread: bool) -> *mut c_void {
    let layout = std::alloc::Layout::new::<HlMutex>();
    let ptr = std::alloc::alloc_zeroed(layout) as *mut HlMutex;
    if ptr.is_null() {
        return ptr::null_mut();
    }
    // HashLink mutexes are RECURSIVE (thread.c uses PTHREAD_MUTEX_RECURSIVE);
    // sys.thread.EventLoop re-acquires from the same thread — a default
    // (non-recursive) mutex deadlocks progress().
    sys::mutex_init(&mut (*ptr).inner, true);
    ptr as *mut c_void
}

#[no_mangle]
pub unsafe extern "C" fn hlp_mutex_acquire(m: *mut c_void) {
    if !m.is_null() {
        sys::mutex_lock(&mut (*(m as *mut HlMutex)).inner);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_mutex_try_acquire(m: *mut c_void) -> bool {
    if m.is_null() {
        return false;
    }
    sys::mutex_try_lock(&mut (*(m as *mut HlMutex)).inner)
}

#[no_mangle]
pub unsafe extern "C" fn hlp_mutex_release(m: *mut c_void) {
    if !m.is_null() {
        sys::mutex_unlock(&mut (*(m as *mut HlMutex)).inner);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_mutex_free(m: *mut c_void) {
    if !m.is_null() {
        sys::mutex_destroy(&mut (*(m as *mut HlMutex)).inner);
        std::alloc::dealloc(m as *mut u8, std::alloc::Layout::new::<HlMutex>());
    }
}

// ============================================================================
// Semaphore (used by Lock)
// ============================================================================

#[repr(C)]
struct HlSemaphore {
    mutex: sys::RawMutex,
    cond: sys::RawCond,
    value: i32,
}

unsafe fn semaphore_take(s: *mut HlSemaphore) -> bool {
    sys::mutex_lock(&mut (*s).mutex);
    let acquired = (*s).value > 0;
    if acquired {
        (*s).value -= 1;
    }
    sys::mutex_unlock(&mut (*s).mutex);
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
    (secs > 0.0).then(|| {
        std::time::Instant::now() + std::time::Duration::from_secs_f64(secs)
    })
}

#[no_mangle]
pub unsafe extern "C" fn hlp_semaphore_alloc(value: i32) -> *mut c_void {
    let layout = std::alloc::Layout::new::<HlSemaphore>();
    let ptr = std::alloc::alloc_zeroed(layout) as *mut HlSemaphore;
    if ptr.is_null() {
        return ptr::null_mut();
    }
    sys::mutex_init(&mut (*ptr).mutex, false);
    sys::cond_init(&mut (*ptr).cond);
    (*ptr).value = value;
    ptr as *mut c_void
}

#[no_mangle]
pub unsafe extern "C" fn hlp_semaphore_acquire(sem: *mut c_void) {
    if sem.is_null() {
        return;
    }
    let s = sem as *mut HlSemaphore;
    while !semaphore_take(s) {
        // Preserve HashLink's !HL_THREADS escape hatch only when no Haxe
        // worker can possibly release the semaphore. With fibers active this
        // is a real blocking acquire and must yield cooperatively.
        if !crate::fiber::fibers_active() {
            return;
        }
        crate::fiber::block_yield();
    }
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
    loop {
        crate::fiber::block_yield();
        if semaphore_take(s) {
            return true;
        }
        if std::time::Instant::now() >= deadline {
            return false;
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_semaphore_release(sem: *mut c_void) {
    if sem.is_null() {
        return;
    }
    let s = sem as *mut HlSemaphore;
    sys::mutex_lock(&mut (*s).mutex);
    (*s).value += 1;
    sys::cond_signal(&mut (*s).cond);
    sys::mutex_unlock(&mut (*s).mutex);
}

#[no_mangle]
pub unsafe extern "C" fn hlp_semaphore_free(sem: *mut c_void) {
    if !sem.is_null() {
        let s = sem as *mut HlSemaphore;
        sys::mutex_destroy(&mut (*s).mutex);
        sys::cond_destroy(&mut (*s).cond);
        std::alloc::dealloc(sem as *mut u8, std::alloc::Layout::new::<HlSemaphore>());
    }
}

// ============================================================================
// Condition Variable
// ============================================================================

#[repr(C)]
struct HlCondition {
    mutex: sys::RawMutex,
    cond: sys::RawCond,
}

#[no_mangle]
pub unsafe extern "C" fn hlp_condition_alloc() -> *mut c_void {
    let layout = std::alloc::Layout::new::<HlCondition>();
    let ptr = std::alloc::alloc_zeroed(layout) as *mut HlCondition;
    if ptr.is_null() {
        return ptr::null_mut();
    }
    // Recursive, matching HashLink's condition mutex (thread.c:340-344).
    sys::mutex_init(&mut (*ptr).mutex, true);
    sys::cond_init(&mut (*ptr).cond);
    ptr as *mut c_void
}

#[no_mangle]
pub unsafe extern "C" fn hlp_condition_acquire(c: *mut c_void) {
    if !c.is_null() {
        sys::mutex_lock(&mut (*(c as *mut HlCondition)).mutex);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_condition_try_acquire(c: *mut c_void) -> bool {
    if c.is_null() {
        return false;
    }
    sys::mutex_try_lock(&mut (*(c as *mut HlCondition)).mutex)
}

#[no_mangle]
pub unsafe extern "C" fn hlp_condition_release(c: *mut c_void) {
    if !c.is_null() {
        sys::mutex_unlock(&mut (*(c as *mut HlCondition)).mutex);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_condition_wait(_c: *mut c_void) {
    // HashLink !HL_THREADS: condition_wait returns immediately (thread.c:380).
    // Blocking here is fatal in a single-threaded VM — no other thread can signal.
}

#[no_mangle]
pub unsafe extern "C" fn hlp_condition_timed_wait(_c: *mut c_void, _timeout: f64) -> bool {
    // HashLink !HL_THREADS: timed_wait returns true immediately (thread.c:393-394).
    true
}

#[no_mangle]
pub unsafe extern "C" fn hlp_condition_signal(c: *mut c_void) {
    if !c.is_null() {
        sys::cond_signal(&mut (*(c as *mut HlCondition)).cond);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_condition_broadcast(c: *mut c_void) {
    if !c.is_null() {
        sys::cond_broadcast(&mut (*(c as *mut HlCondition)).cond);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_condition_free(c: *mut c_void) {
    if !c.is_null() {
        let cv = c as *mut HlCondition;
        sys::mutex_destroy(&mut (*cv).mutex);
        sys::cond_destroy(&mut (*cv).cond);
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
    let s = lock as *mut HlSemaphore;
    if (*s).value > 0 {
        (*s).value -= 1;
        return true;
    }
    // No fibers: exact HashLink !HL_THREADS semantics — never block.
    if !crate::fiber::fibers_active() {
        return false;
    }
    // Fibers exist: HL_THREADS semantics — wait for a release, cooperatively.
    // Timeout arrives as a boxed Null<Float> (seconds); null = wait forever.
    let deadline = if _timeout.is_null() {
        None
    } else {
        let t = _timeout as *const vdynamic;
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
    loop {
        crate::fiber::block_yield();
        if (*s).value > 0 {
            (*s).value -= 1;
            return true;
        }
        if let Some(d) = deadline {
            if std::time::Instant::now() >= d {
                return false;
            }
        }
    }
}

// Separate SDL pump function called from the main loop, not from lock_wait
#[no_mangle]
pub unsafe extern "C" fn hlp_pump_and_sleep() {
    pump_sdl_events();
    std::thread::sleep(std::time::Duration::from_millis(16));
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
    deque_root(msg);
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
    loop {
        let popped;
        if let Ok(mut v) = deque.lock() {
            popped = if v.is_empty() {
                None
            } else {
                let m = v.remove(0) as *mut vdynamic;
                Some((m, v.contains(&(m as *mut c_void))))
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
        crate::fiber::block_yield();
    }
}

// ============================================================================
// Thread-local storage
// ============================================================================

#[cfg(unix)]
#[no_mangle]
pub unsafe extern "C" fn hlp_tls_alloc() -> *mut c_void {
    let mut key: libc::pthread_key_t = 0;
    if libc::pthread_key_create(&mut key, None) == 0 {
        key as usize as *mut c_void
    } else {
        ptr::null_mut()
    }
}

#[cfg(unix)]
#[no_mangle]
pub unsafe extern "C" fn hlp_tls_set(tls: *mut c_void, value: *mut c_void) {
    libc::pthread_setspecific(tls as usize as libc::pthread_key_t, value);
}

#[cfg(unix)]
#[no_mangle]
pub unsafe extern "C" fn hlp_tls_get(tls: *mut c_void) -> *mut c_void {
    libc::pthread_getspecific(tls as usize as libc::pthread_key_t)
}

#[cfg(unix)]
#[no_mangle]
pub unsafe extern "C" fn hlp_tls_free(tls: *mut c_void) {
    libc::pthread_key_delete(tls as usize as libc::pthread_key_t);
}

// The Win32 slot index is biased by one on the way out: index 0 is perfectly
// valid but would travel back as a null handle, which every caller reads as
// "allocation failed".
#[cfg(windows)]
#[no_mangle]
pub unsafe extern "C" fn hlp_tls_alloc() -> *mut c_void {
    use windows_sys::Win32::System::Threading::{TlsAlloc, TLS_OUT_OF_INDEXES};
    let index = TlsAlloc();
    if index == TLS_OUT_OF_INDEXES {
        ptr::null_mut()
    } else {
        (index as usize + 1) as *mut c_void
    }
}

#[cfg(windows)]
#[no_mangle]
pub unsafe extern "C" fn hlp_tls_set(tls: *mut c_void, value: *mut c_void) {
    if tls.is_null() {
        return;
    }
    windows_sys::Win32::System::Threading::TlsSetValue(tls as usize as u32 - 1, value);
}

#[cfg(windows)]
#[no_mangle]
pub unsafe extern "C" fn hlp_tls_get(tls: *mut c_void) -> *mut c_void {
    if tls.is_null() {
        return ptr::null_mut();
    }
    windows_sys::Win32::System::Threading::TlsGetValue(tls as usize as u32 - 1)
}

#[cfg(windows)]
#[no_mangle]
pub unsafe extern "C" fn hlp_tls_free(tls: *mut c_void) {
    if tls.is_null() {
        return;
    }
    windows_sys::Win32::System::Threading::TlsFree(tls as usize as u32 - 1);
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
    // Haxe threads run as cooperative stackful fibers on the main OS thread
    // (krio) — blocking primitives yield to the scheduler. Upstream prim is
    // _FUN(_VOID,_NO_ARG): callback is a vclosure*.
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

thread_local! {
    /// Nesting depth of `Gc.blocking(true)` on this OS thread.
    static GC_BLOCKING_DEPTH: std::cell::Cell<u32> = const { std::cell::Cell::new(0) };
}

/// Upstream hl_blocking (gc.c): enter or leave a section during which the
/// collector must not wait for this thread.
///
/// There is nothing here for it to wait on. ash's collector never stops the
/// world — it runs on whichever thread allocated, under the GC lock — and
/// Haxe threads are krio fibers sharing the mutator's OS thread, so a fiber
/// parked in a blocking section is simply not on the stack the conservative
/// scanner walks. The depth is therefore bookkeeping: it records the state a
/// stop-the-world protocol would need, and suspends nothing.
///
/// Upstream raises "Unblocked thread" on an unmatched `blocking(false)`. Not
/// here: interleaved fibers share this one counter, so a correctly paired
/// program could still see another fiber's decrement and die on the error.
/// The depth saturates at zero instead.
#[no_mangle]
pub unsafe extern "C" fn hlp_blocking(b: bool) {
    GC_BLOCKING_DEPTH.with(|d| {
        d.set(if b {
            d.get().saturating_add(1)
        } else {
            d.get().saturating_sub(1)
        });
    });
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
