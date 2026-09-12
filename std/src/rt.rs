//! The runtime seam: every entry point a host may replace, as one atomic
//! function-pointer slot each.
//!
//! Every slot starts as ash's own implementation (`gc.rs`, `fiber.rs`) and is
//! replaced entry by entry through [`hlp_rt_install`]; a `None` entry keeps
//! ash's. The rest of the crate reaches the heap and the scheduler only
//! through the dispatchers here, so a host that fills the table owns them
//! without ash naming the host anywhere.
//!
//! Dispatch is one relaxed load and an indirect call. That is sound only
//! because installation is refused once the heap exists: before that point
//! nothing but the installing thread runs, and every thread created later
//! observes the stores through its spawn.

use crate::error::TrapContext;
use crate::hl::{hl_type, varray, vbyte, vdynamic};
use std::ffi::c_void;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicBool, AtomicPtr, Ordering};
use std::time::{Duration, Instant};

/// Bumped whenever a slot is added, removed or changes signature.
pub const RT_VERSION: u32 = 1;

/// `timeout_ns` value meaning "no deadline" for [`RuntimeVTable::park`].
pub const RT_NO_TIMEOUT: u64 = u64::MAX;

/// [`RuntimeVTable::thread_create`] flag: the body is compiled, so it may run
/// on a worker lane without re-entering the interpreter.
pub const RT_THREAD_COMPILED: u32 = 1;

/// Word zero of a finalizable block: `void (*)(void *block)`.
pub type Finalizer = unsafe extern "C" fn(*mut c_void);

/// A fiber body as the scheduler sees it: a callback and its context.
/// `C-unwind`, so a panic inside it reaches the scheduler's catch as an
/// errored fiber rather than aborting at the boundary.
pub type FiberBody = unsafe extern "C-unwind" fn(*mut c_void);

/// Callback for [`RuntimeVTable::gc_walk_heap`].
pub type HeapVisitor = unsafe extern "C" fn(*mut vdynamic, *mut hl_type, *mut c_void);

/// A parked logical thread's identity, opaque to everything but the
/// scheduler that minted it. Compared for equality only.
#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Waiter {
    pub scheduler_id: u64,
    pub fiber_id: u32,
    pub token: u64,
}

macro_rules! runtime_table {
    ($( $name:ident ( $($arg:ident : $ty:ty),* ) $(-> $ret:ty)? = $default:expr ; )*) => {
        /// Entry points a host may replace. See the module doc.
        ///
        /// `version` and `size` say which table a host was built against;
        /// [`hlp_rt_install`] refuses either mismatching. Heap slots own
        /// allocation, roots, mutator registration, the GC lock and the
        /// collector's C surface; scheduler slots own fibers, waiters and the
        /// poll epoch. A host replacing the scheduler still calls the switch
        /// hook and closure runner ash registers ([`hlp_rt_switch_hook`],
        /// [`hlp_rt_closure_runner`]) and swaps per-fiber exception state
        /// through [`hlp_rt_exc_swap`].
        #[repr(C)]
        #[derive(Clone, Copy)]
        pub struct RuntimeVTable {
            pub version: u32,
            pub size: u32,
            $( pub $name: Option<unsafe extern "C" fn($($ty),*) $(-> $ret)?>, )*
        }

        impl RuntimeVTable {
            /// Every entry `None`: installing it changes nothing.
            pub const fn new() -> Self {
                Self {
                    version: RT_VERSION,
                    size: std::mem::size_of::<Self>() as u32,
                    $( $name: None, )*
                }
            }
        }

        impl Default for RuntimeVTable {
            fn default() -> Self {
                Self::new()
            }
        }

        mod slot {
            use super::*;
            $( pub static $name: AtomicPtr<()> = AtomicPtr::new($default as *mut ()); )*
        }

        /// The dispatchers, C-typed. The crate calls these (or the typed
        /// wrappers below) and never `gc.rs`/`fiber.rs` across a module
        /// boundary.
        pub mod call {
            use super::*;
            $(
                #[inline(always)]
                pub unsafe fn $name($($arg: $ty),*) $(-> $ret)? {
                    let f: unsafe extern "C" fn($($ty),*) $(-> $ret)? =
                        std::mem::transmute(slot::$name.load(Ordering::Relaxed));
                    f($($arg),*)
                }
            )*
        }

        unsafe fn install_entries(table: &RuntimeVTable) {
            $( if let Some(f) = table.$name {
                slot::$name.store(f as *mut (), Ordering::Release);
            } )*
        }
    };
}

runtime_table! {
    // ── Heap ────────────────────────────────────────────────────────────
    gc_alloc(size: usize) -> *mut u8 = ash::gc_alloc;
    alloc_locked(size: usize) -> *mut u8 = ash::alloc_locked;
    alloc_immortal(size: usize) -> *mut u8 = ash::alloc_immortal;
    alloc_with_finalizer(size: usize, finalize: Option<Finalizer>) -> *mut c_void = ash::alloc_with_finalizer;
    allocation_size(ptr: *const c_void) -> usize = ash::allocation_size;
    is_gc_ptr(ptr: *const c_void) -> bool = ash::is_gc_ptr;
    out_of_memory(what: *const u8, len: usize) -> ! = ash::out_of_memory;
    gc_safepoint() = ash::gc_safepoint;
    gc_set_blocking(blocking: bool) -> bool = ash::gc_set_blocking;
    mark_site(site: u64) = ash::mark_site;
    gc_register_current_os_thread() = ash::gc_register_current_os_thread;
    gc_unregister_current_os_thread() = ash::gc_unregister_current_os_thread;
    gc_register_fiber_stack(id: u32, base: usize, size: usize) = ash::gc_register_fiber_stack;
    gc_update_fiber_sp(id: u32, sp: usize) = ash::gc_update_fiber_sp;
    gc_unregister_fiber_stack(id: u32) = ash::gc_unregister_fiber_stack;
    gc_add_persistent(ptr: *mut vdynamic) = ash::gc_add_persistent;
    gc_remove_persistent(ptr: *mut vdynamic) = ash::gc_remove_persistent;
    add_root_slot(slot: usize) = ash::add_root_slot;
    remove_root_slot(slot: usize) = ash::remove_root_slot;
    gc_lock() = crate::gc::gc_lock;
    gc_unlock() = crate::gc::gc_unlock;
    gc_lock_held_depth() -> usize = ash::gc_lock_held_depth;
    gc_lock_unwind_to(depth: usize) = ash::gc_lock_unwind_to;
    gc_registered_threads(out: *mut u64, cap: usize) -> usize = crate::gc::gc_registered_threads;
    gc_print_stats() = crate::gc::gc_print_stats;
    mark_size(data_size: i32) -> i32 = crate::gc::mark_size;
    gc_walk_heap(visitor: HeapVisitor, ctx: *mut c_void) = crate::gc::gc_walk_heap;
    gc_init() = crate::gc::gc_init;
    gc_set_stack_top(top: usize) = crate::gc::gc_set_stack_top;
    register_thread(stack_top: *mut c_void) = crate::gc::register_thread;
    unregister_thread() = crate::gc::unregister_thread;
    gc_set_globals(ptr: *const *mut c_void, count: usize) = crate::gc::gc_set_globals;
    gc_scan_roots_done() = crate::gc::gc_scan_roots_done;
    gc_clear_scan_roots() = crate::gc::gc_clear_scan_roots;
    gc_add_scan_root(ptr: *const c_void, size: usize) = crate::gc::gc_add_scan_root;
    gc_set_scan_roots_live(ranges: *const (usize, usize), len: *const usize) = crate::gc::gc_set_scan_roots_live;
    gc_set_scan_roots(ranges: *const (usize, usize), count: usize) = crate::gc::gc_set_scan_roots;
    gc_track_external(bytes: u64) = crate::gc::gc_track_external;
    gc_enable(b: bool) = crate::gc::gc_enable;
    gc_get_flags() -> i32 = crate::gc::gc_get_flags;
    gc_set_flags(f: i32) = crate::gc::gc_set_flags;
    gc_major() = crate::gc::gc_major;
    gc_stats(total_allocated: *mut f64, allocation_count: *mut f64, current_memory: *mut f64) = crate::gc::gc_stats;
    gc_profile(b: bool) = crate::gc::gc_profile;
    gc_get_live_objects(t: *mut hl_type, arr: *mut varray) -> i32 = crate::gc::gc_get_live_objects;
    gc_dump_memory(filename: *mut vbyte) = crate::gc::gc_dump_memory;
    // ── Scheduler ───────────────────────────────────────────────────────
    new_waiter() -> Waiter = ash::new_waiter;
    wake(waiter: Waiter) -> bool = ash::wake;
    park(waiter: Waiter, timeout_ns: u64) -> bool = ash::park;
    sleep_ns(ns: u64) = ash::sleep_ns;
    block_yield() = ash::block_yield;
    schedule_step() -> bool = ash::schedule_step;
    thread_create(body: FiberBody, ctx: *mut c_void, flags: u32) -> *mut c_void = ash::thread_create;
    fiber_poll() = crate::fiber::fiber_poll;
    fibers_active() -> bool = ash::fibers_active;
    current_id() -> u32 = ash::current_id;
    current_handle() -> *mut c_void = ash::current_handle;
    current_owner() -> u64 = ash::current_owner;
    current_ctx() -> *mut c_void = ash::current_ctx;
    update_gc_blocking_depth(blocking: bool) -> bool = ash::update_gc_blocking_depth;
    is_gc_blocking() -> bool = ash::is_gc_blocking;
    request_fiber_poll() = ash::request_fiber_poll;
    fiber_poll_epoch_address() -> *const u64 = crate::fiber::fiber_poll_epoch_address;
    is_worker_lane() -> bool = crate::fiber::is_worker_lane;
    mark_main_thread() = ash::mark_main_thread;
    is_main_thread() -> bool = ash::is_main_thread;
    foreign_threads_seen() -> bool = ash::foreign_threads_seen;
}

pub use call::{
    add_root_slot, block_yield, current_ctx, current_handle, current_id, current_owner, fiber_poll,
    fiber_poll_epoch_address, fibers_active, gc_add_persistent, gc_add_scan_root,
    gc_clear_scan_roots, gc_dump_memory, gc_enable, gc_get_flags, gc_get_live_objects, gc_init,
    gc_lock, gc_lock_held_depth, gc_lock_unwind_to, gc_major, gc_print_stats, gc_profile,
    gc_register_fiber_stack, gc_registered_threads, gc_remove_persistent, gc_scan_roots_done,
    gc_set_flags, gc_set_globals, gc_set_scan_roots, gc_set_scan_roots_live, gc_set_stack_top,
    gc_stats, gc_track_external, gc_unlock, gc_unregister_fiber_stack, gc_update_fiber_sp,
    gc_walk_heap, is_gc_blocking, is_worker_lane, mark_size, new_waiter, register_thread,
    remove_root_slot, schedule_step, thread_create, unregister_thread, update_gc_blocking_depth,
    wake,
};

// ── Typed wrappers over the C-shaped slots ──────────────────────────────
//
// Entries with no precondition are safe to call, as ash's own were.

#[inline(always)]
pub fn gc_safepoint() {
    unsafe { call::gc_safepoint() }
}

#[inline(always)]
pub fn gc_set_blocking(blocking: bool) -> bool {
    unsafe { call::gc_set_blocking(blocking) }
}

#[inline(always)]
pub fn mark_site(site: u64) {
    unsafe { call::mark_site(site) }
}

#[inline(always)]
pub fn gc_register_current_os_thread() {
    unsafe { call::gc_register_current_os_thread() }
}

#[inline(always)]
pub fn gc_unregister_current_os_thread() {
    unsafe { call::gc_unregister_current_os_thread() }
}

#[inline(always)]
pub fn request_fiber_poll() {
    unsafe { call::request_fiber_poll() }
}

#[inline(always)]
pub fn mark_main_thread() {
    unsafe { call::mark_main_thread() }
}

#[inline(always)]
pub fn is_main_thread() -> bool {
    unsafe { call::is_main_thread() }
}

#[inline(always)]
pub fn foreign_threads_seen() -> bool {
    unsafe { call::foreign_threads_seen() }
}

/// Zeroed memory, or `None` when the heap is exhausted.
#[inline(always)]
pub fn gc_alloc(size: usize) -> Option<NonNull<u8>> {
    NonNull::new(unsafe { call::gc_alloc(size) })
}

/// Zeroed memory from the locked path.
#[inline(always)]
pub fn alloc_locked(size: usize) -> Option<NonNull<u8>> {
    NonNull::new(unsafe { call::alloc_locked(size) })
}

/// Zeroed, pinned for the life of the process.
#[inline(always)]
pub fn alloc_immortal(size: usize) -> Option<NonNull<u8>> {
    NonNull::new(unsafe { call::alloc_immortal(size) })
}

/// Allocate a finalizable block whose word zero holds `finalize`.
#[inline(always)]
pub unsafe fn alloc_with_finalizer(size: usize, finalize: Finalizer) -> *mut c_void {
    call::alloc_with_finalizer(size, Some(finalize))
}

/// Allocate a finalizable block and leave word zero to the caller.
#[inline(always)]
pub unsafe fn alloc_finalizable(size: usize) -> *mut c_void {
    call::alloc_with_finalizer(size, None)
}

#[inline(always)]
pub unsafe fn allocation_size(ptr: *const c_void) -> usize {
    call::allocation_size(ptr)
}

#[inline(always)]
pub unsafe fn is_gc_ptr(ptr: *const c_void) -> bool {
    call::is_gc_ptr(ptr)
}

/// Report an allocation that could not be satisfied, and stop.
#[cold]
#[inline(never)]
pub fn out_of_memory(what: &str) -> ! {
    unsafe { call::out_of_memory(what.as_ptr(), what.len()) }
}

/// Park until `waiter` is woken or `deadline` passes.
#[inline(always)]
pub unsafe fn park(waiter: Waiter, deadline: Option<Instant>) -> bool {
    call::park(waiter, timeout_ns(deadline))
}

#[inline(always)]
pub unsafe fn sleep_for(duration: Duration) {
    call::sleep_ns(duration_ns(duration))
}

fn timeout_ns(deadline: Option<Instant>) -> u64 {
    deadline.map_or(RT_NO_TIMEOUT, |d| {
        duration_ns(d.saturating_duration_since(Instant::now()))
    })
}

fn duration_ns(d: Duration) -> u64 {
    d.as_nanos().min(RT_NO_TIMEOUT as u128 - 1) as u64
}

fn deadline_from(timeout_ns: u64) -> Option<Instant> {
    (timeout_ns != RT_NO_TIMEOUT).then(|| Instant::now() + Duration::from_nanos(timeout_ns))
}

// ── Installation ────────────────────────────────────────────────────────

static INSTALLED: AtomicBool = AtomicBool::new(false);
/// Set by `hlp_gc_init`: the point past which a slot may be in use on any
/// thread, and installation is refused.
static SEALED: AtomicBool = AtomicBool::new(false);

pub(crate) fn seal() {
    SEALED.store(true, Ordering::Release);
}

/// Copy the `Some` entries of `table` into the dispatch slots.
///
/// Returns false, changing nothing, when `table` is null, was built against
/// another table version or size, or the heap already exists -- either
/// `hlp_gc_init` has run or ash's heap was created lazily by an allocation.
#[no_mangle]
pub unsafe extern "C" fn hlp_rt_install(table: *const RuntimeVTable) -> bool {
    if table.is_null() {
        return false;
    }
    let table = &*table;
    if table.version != RT_VERSION || table.size as usize != std::mem::size_of::<RuntimeVTable>() {
        return false;
    }
    if SEALED.load(Ordering::Acquire) || crate::gc::heap_exists() {
        return false;
    }
    install_entries(table);
    INSTALLED.store(true, Ordering::Release);
    true
}

/// Whether a host table has been installed.
#[no_mangle]
pub extern "C" fn hlp_rt_installed() -> bool {
    INSTALLED.load(Ordering::Acquire)
}

/// The fiber switch hook the host registered with `hlp_set_fiber_switch_hook`,
/// for a scheduler that replaces ash's and must call it around every switch.
#[no_mangle]
pub unsafe extern "C" fn hlp_rt_switch_hook() -> Option<crate::fiber::FiberSwitchHook> {
    crate::fiber::switch_hook()
}

/// The closure runner registered with `hlp_set_closure_runner`.
#[no_mangle]
pub unsafe extern "C" fn hlp_rt_closure_runner() -> Option<crate::fiber::ClosureRunner> {
    crate::fiber::closure_runner()
}

/// Swap this thread's live exception state (trap chain head, exception
/// value) with `*trap` and `*exc`: a replacement scheduler's per-fiber
/// switch.
#[no_mangle]
pub unsafe extern "C" fn hlp_rt_exc_swap(trap: *mut *mut TrapContext, exc: *mut *mut vdynamic) {
    crate::gc::gc_swap_exc_state(&mut *trap, &mut *exc);
}

// ── ash's own implementations, C-shaped ─────────────────────────────────

/// Adapters from the C slot signatures to `gc.rs` and `fiber.rs`. Slots whose
/// ash implementation already has the C shape point at it directly.
mod ash {
    use super::*;

    pub unsafe extern "C" fn gc_alloc(size: usize) -> *mut u8 {
        crate::gc::gc_alloc(size).map_or(std::ptr::null_mut(), NonNull::as_ptr)
    }

    pub unsafe extern "C" fn alloc_locked(size: usize) -> *mut u8 {
        crate::gc::gc_locked_init()
            .allocate(size)
            .map_or(std::ptr::null_mut(), NonNull::as_ptr)
    }

    pub unsafe extern "C" fn alloc_immortal(size: usize) -> *mut u8 {
        crate::gc::gc_locked_init()
            .allocate_immortal(size)
            .map_or(std::ptr::null_mut(), NonNull::as_ptr)
    }

    pub unsafe extern "C" fn alloc_with_finalizer(
        size: usize,
        finalize: Option<Finalizer>,
    ) -> *mut c_void {
        crate::gc::alloc_with_finalizer(size, finalize)
    }

    pub unsafe extern "C" fn allocation_size(ptr: *const c_void) -> usize {
        crate::gc::allocation_size(ptr)
    }

    pub unsafe extern "C" fn is_gc_ptr(ptr: *const c_void) -> bool {
        crate::gc::gc_locked_init().is_gc_ptr(ptr as *const vdynamic)
    }

    pub unsafe extern "C" fn out_of_memory(what: *const u8, len: usize) -> ! {
        let what = std::str::from_utf8_unchecked(std::slice::from_raw_parts(what, len));
        crate::gc::out_of_memory(what)
    }

    pub unsafe extern "C" fn gc_safepoint() {
        crate::gc::gc_safepoint();
    }

    pub unsafe extern "C" fn gc_set_blocking(blocking: bool) -> bool {
        crate::gc::gc_set_blocking(blocking)
    }

    pub unsafe extern "C" fn mark_site(site: u64) {
        crate::gc::mark_site(site);
    }

    pub unsafe extern "C" fn gc_register_current_os_thread() {
        #[cfg(any(not(target_family = "wasm"), target_feature = "atomics"))]
        crate::gc::gc_register_current_os_thread();
    }

    pub unsafe extern "C" fn gc_unregister_current_os_thread() {
        #[cfg(any(not(target_family = "wasm"), target_feature = "atomics"))]
        crate::gc::gc_unregister_current_os_thread();
    }

    pub unsafe extern "C" fn gc_register_fiber_stack(id: u32, base: usize, size: usize) {
        crate::gc::gc_register_fiber_stack(id, base, size);
    }

    pub unsafe extern "C" fn gc_update_fiber_sp(id: u32, sp: usize) {
        crate::gc::gc_update_fiber_sp(id, sp);
    }

    pub unsafe extern "C" fn gc_unregister_fiber_stack(id: u32) {
        crate::gc::gc_unregister_fiber_stack(id);
    }

    pub unsafe extern "C" fn gc_add_persistent(ptr: *mut vdynamic) {
        crate::gc::gc_add_persistent(ptr);
    }

    pub unsafe extern "C" fn gc_remove_persistent(ptr: *mut vdynamic) {
        crate::gc::gc_remove_persistent(ptr);
    }

    pub unsafe extern "C" fn add_root_slot(slot: usize) {
        crate::gc::gc_locked_init().add_root_slot(slot);
    }

    pub unsafe extern "C" fn remove_root_slot(slot: usize) {
        crate::gc::gc_locked_init().remove_root_slot(slot);
    }

    pub unsafe extern "C" fn gc_lock_held_depth() -> usize {
        crate::gc::gc_lock_held_depth()
    }

    pub unsafe extern "C" fn gc_lock_unwind_to(depth: usize) {
        crate::gc::gc_lock_unwind_to(depth);
    }

    pub unsafe extern "C" fn new_waiter() -> Waiter {
        crate::fiber::new_waiter()
    }

    pub unsafe extern "C" fn wake(waiter: Waiter) -> bool {
        crate::fiber::wake(waiter)
    }

    pub unsafe extern "C" fn park(waiter: Waiter, timeout_ns: u64) -> bool {
        crate::fiber::park(waiter, deadline_from(timeout_ns))
    }

    pub unsafe extern "C" fn sleep_ns(ns: u64) {
        crate::fiber::sleep_until(Instant::now() + Duration::from_nanos(ns));
    }

    pub unsafe extern "C" fn block_yield() {
        crate::fiber::block_yield();
    }

    pub unsafe extern "C" fn schedule_step() -> bool {
        crate::fiber::schedule_step()
    }

    pub unsafe extern "C" fn thread_create(
        body: FiberBody,
        ctx: *mut c_void,
        flags: u32,
    ) -> *mut c_void {
        crate::fiber::spawn(body, ctx, flags)
    }

    pub unsafe extern "C" fn fibers_active() -> bool {
        crate::fiber::fibers_active()
    }

    pub unsafe extern "C" fn current_id() -> u32 {
        crate::fiber::current_id()
    }

    pub unsafe extern "C" fn current_handle() -> *mut c_void {
        crate::fiber::current_handle().unwrap_or(std::ptr::null_mut())
    }

    pub unsafe extern "C" fn current_owner() -> u64 {
        crate::fiber::current_owner()
    }

    pub unsafe extern "C" fn current_ctx() -> *mut c_void {
        crate::fiber::current_ctx()
    }

    pub unsafe extern "C" fn update_gc_blocking_depth(blocking: bool) -> bool {
        crate::fiber::update_gc_blocking_depth(blocking)
    }

    pub unsafe extern "C" fn is_gc_blocking() -> bool {
        crate::fiber::is_gc_blocking()
    }

    pub unsafe extern "C" fn request_fiber_poll() {
        crate::fiber::request_fiber_poll();
    }

    pub unsafe extern "C" fn mark_main_thread() {
        crate::fiber::mark_main_thread();
    }

    pub unsafe extern "C" fn is_main_thread() -> bool {
        crate::fiber::is_main_thread()
    }

    pub unsafe extern "C" fn foreign_threads_seen() -> bool {
        crate::fiber::foreign_threads_seen()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::AtomicUsize;

    static SAFEPOINTS: AtomicUsize = AtomicUsize::new(0);

    /// Counts, then forwards to ash's.
    unsafe extern "C" fn counting_safepoint() {
        SAFEPOINTS.fetch_add(1, Ordering::SeqCst);
        ash::gc_safepoint();
    }

    const CHILD_ENV: &str = "ASH_RT_SEAM_CHILD";

    /// The table is process-global and any other test's `hlp_gc_init` seals
    /// it, so the install has to happen in a process of its own: the test
    /// re-runs itself with only this test selected and checks the exit.
    #[test]
    fn an_entry_installed_before_init_is_what_the_export_dispatches_to() {
        if std::env::var_os(CHILD_ENV).is_none() {
            let status = std::process::Command::new(std::env::current_exe().unwrap())
                .args([
                    "--exact",
                    "rt::tests::an_entry_installed_before_init_is_what_the_export_dispatches_to",
                    "--test-threads=1",
                ])
                .env(CHILD_ENV, "1")
                .status()
                .expect("re-run the test binary");
            assert!(status.success(), "child test process failed: {status}");
            return;
        }

        assert!(!hlp_rt_installed());
        let mut table = RuntimeVTable::new();
        table.gc_safepoint = Some(counting_safepoint);
        assert!(unsafe { hlp_rt_install(&table) });
        assert!(hlp_rt_installed());

        unsafe { crate::gc::hlp_gc_init() };
        let before = SAFEPOINTS.load(Ordering::SeqCst);
        unsafe {
            crate::gc::hlp_gc_safepoint();
            crate::gc::hlp_gc_safepoint();
            crate::gc::hlp_gc_safepoint();
        }
        assert_eq!(SAFEPOINTS.load(Ordering::SeqCst), before + 3);

        // Sealed by the init above; the table is untouched from here on.
        assert!(!unsafe { hlp_rt_install(&table) });
    }

    #[test]
    fn install_after_init_is_refused() {
        unsafe { crate::gc::hlp_gc_init() };
        let mut table = RuntimeVTable::new();
        table.gc_safepoint = Some(counting_safepoint);
        assert!(!unsafe { hlp_rt_install(&table) });
    }

    #[test]
    fn a_table_from_another_version_or_size_is_refused() {
        let mut table = RuntimeVTable::new();
        table.version = RT_VERSION + 1;
        assert!(!unsafe { hlp_rt_install(&table) });
        let mut table = RuntimeVTable::new();
        table.size += 8;
        assert!(!unsafe { hlp_rt_install(&table) });
        assert!(!unsafe { hlp_rt_install(std::ptr::null()) });
    }
}
