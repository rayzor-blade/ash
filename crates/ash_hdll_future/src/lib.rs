//! Stock HashLink implementation of `ash.Future<T>`.
//!
//! This library uses only the HashLink ABI. Ash keeps its own `std@future_*`
//! implementation, which parks fibers instead of blocking OS threads.

use hl_abi::{hl_type, vdynamic};
use std::ffi::c_void;
use std::ptr;
use std::sync::{Condvar, Mutex};

const MEM_KIND_FINALIZER: i32 = 3;

unsafe extern "C" {
    static mut hlt_abstract: hl_type;
    fn hl_gc_alloc_gen(t: *mut hl_type, size: i32, flags: i32) -> *mut c_void;
    fn hl_add_root(slot: *mut c_void);
    fn hl_remove_root(slot: *mut c_void);
    fn hl_blocking(enter: bool);
    fn hl_get_thread() -> *mut c_void;
    fn hl_register_thread(stack_top: *mut c_void);
    fn hl_unregister_thread();
}

/// A foreign callback thread has to join HashLink's GC before changing roots.
struct RegisteredThread(bool);

impl RegisteredThread {
    unsafe fn enter(stack_top: *mut c_void) -> Self {
        unsafe {
            if hl_get_thread().is_null() {
                hl_register_thread(stack_top);
                Self(true)
            } else {
                Self(false)
            }
        }
    }
}

impl Drop for RegisteredThread {
    fn drop(&mut self) {
        if self.0 {
            unsafe { hl_unregister_thread() };
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Status {
    Pending,
    Resolved,
    Rejected,
}

struct State {
    status: Status,
}

/// The first word is the HashLink GC finalizer. The two pointer slots remain
/// registered roots while their contents need to survive a collection.
#[repr(C)]
struct FutureHandle {
    finalize: Option<unsafe extern "C" fn(*mut c_void)>,
    pending_self: *mut c_void,
    value: *mut vdynamic,
    state: Mutex<State>,
    wake: Condvar,
}

unsafe extern "C" fn finalize_future(block: *mut c_void) {
    unsafe {
        let future = block.cast::<FutureHandle>();
        debug_assert!((*future).state.lock().unwrap().status != Status::Pending);
        hl_remove_root(ptr::addr_of_mut!((*future).value).cast());
        ptr::drop_in_place(ptr::addr_of_mut!((*future).wake));
        ptr::drop_in_place(ptr::addr_of_mut!((*future).state));
    }
}

/// Create a handle retained until its first completion, including when Haxe
/// drops its copy before the native callback runs.
///
/// # Safety
/// Call from a HashLink registered thread after its collector is initialized.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn hlp_future_create() -> *mut c_void {
    unsafe {
        let future = hl_gc_alloc_gen(
            ptr::addr_of_mut!(hlt_abstract),
            std::mem::size_of::<FutureHandle>() as i32,
            MEM_KIND_FINALIZER,
        ) as *mut FutureHandle;
        if future.is_null() {
            return ptr::null_mut();
        }
        ptr::addr_of_mut!((*future).finalize).write(Some(finalize_future));
        ptr::addr_of_mut!((*future).state).write(Mutex::new(State {
            status: Status::Pending,
        }));
        ptr::addr_of_mut!((*future).wake).write(Condvar::new());
        (*future).pending_self = future.cast();
        (*future).value = ptr::null_mut();
        hl_add_root(ptr::addr_of_mut!((*future).pending_self).cast());
        hl_add_root(ptr::addr_of_mut!((*future).value).cast());
        future.cast()
    }
}

unsafe fn complete(future: *mut FutureHandle, value: *mut vdynamic, status: Status) -> bool {
    unsafe {
        let Some(handle) = future.as_ref() else {
            return false;
        };
        let mut state = handle.state.lock().unwrap();
        if state.status != Status::Pending {
            return false;
        }
        // The caller keeps `value` rooted until this call returns. Its new
        // root slot was registered at creation and remains until finalization.
        (*future).value = value;
        state.status = status;
        handle.wake.notify_all();
        drop(state);
        // Removing this root can let the finalizer run immediately. Do not
        // access the handle again after this call.
        hl_remove_root(ptr::addr_of_mut!((*future).pending_self).cast());
        true
    }
}

/// Complete a pending future with a boxed HashLink value.
///
/// # Safety
/// `future` must be a live handle from this HDLL, and `value` must stay rooted
/// through this call. A foreign callback thread is registered temporarily.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn hlp_future_resolve(future: *mut c_void, value: *mut vdynamic) -> bool {
    unsafe {
        let stack_top = 0usize;
        let _thread = RegisteredThread::enter(ptr::addr_of!(stack_top).cast_mut().cast());
        complete(future.cast(), value, Status::Resolved)
    }
}

/// Complete a pending future with a boxed Haxe exception value.
///
/// # Safety
/// `future` must be a live handle from this HDLL, and `error` must stay rooted
/// through this call. A foreign callback thread is registered temporarily.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn hlp_future_reject(future: *mut c_void, error: *mut vdynamic) -> bool {
    unsafe {
        let stack_top = 0usize;
        let _thread = RegisteredThread::enter(ptr::addr_of!(stack_top).cast_mut().cast());
        complete(future.cast(), error, Status::Rejected)
    }
}

/// Return 0 for pending, 1 for resolved, or 2 for rejected.
///
/// # Safety
/// A non-null `future` must be a live handle from this HDLL.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn hlp_future_state(future: *mut c_void) -> i32 {
    unsafe {
        let Some(future) = (future as *mut FutureHandle).as_ref() else {
            return 0;
        };
        match future.state.lock().unwrap().status {
            Status::Pending => 0,
            Status::Resolved => 1,
            Status::Rejected => 2,
        }
    }
}

/// Wait for completion and return the stored value or rejection payload.
///
/// # Safety
/// A non-null `future` must be a live handle from this HDLL. Call from a
/// HashLink registered thread; the Haxe wrapper raises a rejection afterward.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn hlp_future_await(future: *mut c_void) -> *mut vdynamic {
    unsafe {
        let Some(future) = (future as *mut FutureHandle).as_ref() else {
            return ptr::null_mut();
        };
        let mut state = future.state.lock().unwrap();
        while state.status == Status::Pending {
            // HashLink's collector must be able to stop the waiting thread.
            hl_blocking(true);
            state = future.wake.wait(state).unwrap();
            hl_blocking(false);
        }
        let value = future.value;
        drop(state);
        // The Haxe wrapper checks `state` and throws on rejection. Calling
        // stock HashLink's longjmp-based hl_throw from Rust would skip Rust
        // stack frames and violate their cleanup contract.
        value
    }
}

// These are DEFINE_PRIM resolvers, distinct from the public C ABI above.
hl_abi::define_prim!(hlp_create, hlp_future_create, "P_Xash_future_");
hl_abi::define_prim!(hlp_resolve, hlp_future_resolve, "PXash_future_D_b");
hl_abi::define_prim!(hlp_reject, hlp_future_reject, "PXash_future_D_b");
hl_abi::define_prim!(hlp_state, hlp_future_state, "PXash_future__i");
hl_abi::define_prim!(hlp_await, hlp_future_await, "PXash_future__D");

#[cfg(feature = "stock-test-prims")]
mod test_prims {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::time::Duration;

    static HIDDEN_PENDING: AtomicUsize = AtomicUsize::new(0);

    unsafe extern "C" fn create_abandoned() -> bool {
        let future = unsafe { hlp_future_create() };
        HIDDEN_PENDING.store((future as usize) ^ usize::MAX, Ordering::SeqCst);
        !future.is_null()
    }

    unsafe extern "C" fn finish_abandoned() -> bool {
        let future = HIDDEN_PENDING.swap(0, Ordering::SeqCst) ^ usize::MAX;
        unsafe { hlp_future_resolve(future as *mut c_void, ptr::null_mut()) }
    }

    unsafe extern "C" fn create_foreign() -> *mut c_void {
        let future = unsafe { hlp_future_create() };
        let address = future as usize;
        std::thread::spawn(move || {
            std::thread::sleep(Duration::from_millis(10));
            assert!(unsafe { hlp_future_resolve(address as *mut c_void, ptr::null_mut()) });
        });
        future
    }

    hl_abi::define_prim!(hlp_test_create_abandoned, create_abandoned, "P_b");
    hl_abi::define_prim!(hlp_test_finish_abandoned, finish_abandoned, "P_b");
    hl_abi::define_prim!(hlp_test_create_foreign, create_foreign, "P_Xash_future_");
}
