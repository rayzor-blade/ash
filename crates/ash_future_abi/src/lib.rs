//! Ash's Future extension to the `hl_abi` C imports.
//!
//! This crate declares runtime functions only. A native library or wasm side
//! module can depend on it without linking another copy of `ash_std`.

use hl_abi::vdynamic;

#[repr(C)]
pub struct AshFuture {
    _private: [u8; 0],
}

unsafe extern "C" {
    /// Create a pending, GC-owned carrier. The runtime retains it until its
    /// first completion, even if Haxe drops its reference.
    pub fn hlp_future_create() -> *mut AshFuture;
    /// Complete once with a GC-managed boxed value. Returns false if already
    /// completed. The caller must keep `value` valid until this call.
    pub fn hlp_future_resolve(future: *mut AshFuture, value: *mut vdynamic) -> bool;
    /// Complete once with a Haxe exception value.
    pub fn hlp_future_reject(future: *mut AshFuture, error: *mut vdynamic) -> bool;
    /// 0 pending, 1 resolved, 2 rejected.
    pub fn hlp_future_state(future: *mut AshFuture) -> i32;
    /// Wait for completion. Ash raises on rejection; stock HashLink returns
    /// the stored error and reports rejection through `hlp_future_state`.
    /// Use the Haxe `Future.await()` surface for consistent throwing behavior.
    pub fn hlp_future_await(future: *mut AshFuture) -> *mut vdynamic;
}
