//! A single-assignment carrier for native operations that finish after their
//! call returns. The C entry points are shared by native HDLLs and wasm side
//! modules; neither needs to link a second copy of the runtime.

use crate::hl::vdynamic;
use crate::rt::{self, Waiter};
use std::ffi::c_void;
use std::ptr;
use std::sync::Mutex;

#[derive(Clone, Copy, PartialEq, Eq)]
enum Status {
    Pending,
    Resolved,
    Rejected,
}

struct State {
    status: Status,
    waiters: Vec<Waiter>,
}

/// The first word is the collector's finalizer. Both root slots are addresses
/// inside the pinned GC block, so the collector can re-read their values.
/// `pending_self` keeps a handle supplied to an asynchronous native callback
/// alive until completion, even if Haxe drops its reference first.
#[repr(C)]
struct FutureHandle {
    finalize: Option<rt::Finalizer>,
    pending_self: *mut c_void,
    value: *mut vdynamic,
    state: Mutex<State>,
}

unsafe extern "C" fn finalize_future(block: *mut c_void) {
    unsafe {
        let future = block as *mut FutureHandle;
        // The pending self-root has been removed before a handle can become
        // unreachable. The value root lasts for every later await/poll.
        debug_assert!((*future).state.lock().unwrap().status != Status::Pending);
        rt::remove_root_slot(ptr::addr_of_mut!((*future).value) as usize);
        ptr::drop_in_place(ptr::addr_of_mut!((*future).state));
    }
}

/// Create a pending future. Native callers own the returned handle until
/// their one completion call; the runtime roots it during that interval.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn hlp_future_create() -> *mut c_void {
    unsafe {
        // A foreign caller may not have a stack the collector scans. Keep
        // collection out until both root slots are installed.
        rt::gc_lock();
        let future = rt::alloc_with_finalizer(std::mem::size_of::<FutureHandle>(), finalize_future)
            as *mut FutureHandle;
        if future.is_null() {
            rt::gc_unlock();
            return ptr::null_mut();
        }
        ptr::addr_of_mut!((*future).state).write(Mutex::new(State {
            status: Status::Pending,
            waiters: Vec::new(),
        }));
        (*future).pending_self = future.cast();
        rt::add_root_slot(ptr::addr_of_mut!((*future).pending_self) as usize);
        rt::add_root_slot(ptr::addr_of_mut!((*future).value) as usize);
        rt::gc_unlock();
        future.cast()
    }
}

unsafe fn complete(future: *mut FutureHandle, value: *mut vdynamic, status: Status) -> bool {
    unsafe {
        if future.is_null() {
            return false;
        }
        let mut state = (*future).state.lock().unwrap();
        if state.status != Status::Pending {
            return false;
        }
        // The root-slot registry and collection both use the GC lock. Set the
        // payload while it is held so a foreign completion thread cannot race
        // the collector's read of this pointer.
        rt::gc_lock();
        (*future).value = value;
        rt::gc_unlock();
        state.status = status;
        let waiters = std::mem::take(&mut state.waiters);
        drop(state);
        for waiter in waiters {
            rt::wake(waiter);
        }
        // Last access to the handle: removing this root may let a concurrent
        // collection finalize it if Haxe has already dropped its reference.
        rt::remove_root_slot(ptr::addr_of_mut!((*future).pending_self) as usize);
        true
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn hlp_future_resolve(future: *mut c_void, value: *mut vdynamic) -> bool {
    unsafe { complete(future.cast(), value, Status::Resolved) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn hlp_future_reject(future: *mut c_void, error: *mut vdynamic) -> bool {
    unsafe { complete(future.cast(), error, Status::Rejected) }
}

/// 0 pending, 1 resolved, 2 rejected.
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

/// Park the current Ash fiber while pending. Rejection raises the stored Haxe
/// value through the ordinary exception path after all locks are released.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn hlp_future_await(future: *mut c_void) -> *mut vdynamic {
    unsafe {
        let future = future as *mut FutureHandle;
        if future.is_null() {
            return ptr::null_mut();
        }
        loop {
            let mut state = (*future).state.lock().unwrap();
            match state.status {
                Status::Resolved => return (*future).value,
                Status::Rejected => {
                    let error = (*future).value;
                    drop(state);
                    crate::error::hlp_throw(error);
                    return ptr::null_mut();
                }
                Status::Pending => {
                    let waiter = rt::new_waiter();
                    state.waiters.push(waiter);
                    drop(state);
                    let _ = rt::park(waiter, None);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::{Arc, Barrier};

    #[test]
    fn foreign_thread_completes_after_collection_and_wakes_waiter() {
        unsafe { crate::gc::hlp_gc_init() };
        let future = unsafe { hlp_future_create() };
        assert!(!future.is_null());
        let gate = Arc::new(Barrier::new(2));
        let thread_gate = gate.clone();
        let address = future as usize;
        let worker = std::thread::spawn(move || {
            thread_gate.wait();
            // Make this a parked-await test, independent of which OS thread
            // wins the race after the barrier.
            let deadline = std::time::Instant::now() + std::time::Duration::from_secs(2);
            let mut parked = false;
            loop {
                let handle = unsafe { &*(address as *mut FutureHandle) };
                if !handle.state.lock().unwrap().waiters.is_empty() {
                    parked = true;
                    break;
                }
                if std::time::Instant::now() >= deadline {
                    break;
                }
                std::thread::yield_now();
            }
            (parked, unsafe {
                hlp_future_resolve(address as *mut c_void, ptr::null_mut())
            })
        });
        unsafe { rt::gc_major() };
        gate.wait();
        assert!(unsafe { hlp_future_await(future) }.is_null());
        assert_eq!(worker.join().unwrap(), (true, true));
        assert_eq!(unsafe { hlp_future_state(future) }, 1);
        assert!(!unsafe { hlp_future_reject(future, ptr::null_mut()) });
    }
}
