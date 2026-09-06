//! The `sdl` HDLL for wasm.
//!
//! A demonstration that a Heaps program runs unmodified on ash's VM wherever
//! a host can answer for a window and a drawing surface: `examples/heaps_base3d`
//! reaches sixty-seven of `sdl.hdll`'s two hundred and eight primitives, and
//! those sixty-seven are what is here.
//!
//! # What this is not
//!
//! Not an SDL, and not a GL. Every function forwards, and the host decides
//! what a window and a frame actually are -- a canvas and WebGL2 in a page, a
//! recorder under a headless host. That is what makes it small enough to
//! generate and small enough to check.
//!
//! # How it is built
//!
//! [`generated`] comes from `scripts/generate_sdl_shim.py`, which reads the
//! signatures the real `sdl.hdll` reports for itself. See
//! `crates/ash/tests/native_signatures.rs`: `DEFINE_PRIM` carries the
//! signature by design, so the library is the authority on its own ABI and
//! the Haxe externs are a second-hand account of it.

#![cfg(target_family = "wasm")]

mod abi;

/// This library allocates through the program. See [`abi::ProgramAllocator`].
#[global_allocator]
static ALLOCATOR: abi::ProgramAllocator = abi::ProgramAllocator;

/// Export a primitive the way `DEFINE_PRIM` does: a resolver that reports the
/// signature through an out-parameter and returns the real function.
///
/// Never the primitive itself -- storing the resolver and calling it as the
/// primitive writes a signature string through whatever the first argument
/// happens to be.
macro_rules! define_prim {
    ($resolver:ident, $function:ident, $signature:literal) => {
        /// # Safety
        /// `sign` must be a writable pointer, which is what the caller of a
        /// `DEFINE_PRIM` resolver passes.
        #[no_mangle]
        pub unsafe extern "C" fn $resolver(
            sign: *mut *const std::ffi::c_char,
        ) -> *mut std::ffi::c_void {
            if !sign.is_null() {
                *sign = concat!($signature, "\0").as_ptr() as *const std::ffi::c_char;
            }
            $function as *mut std::ffi::c_void
        }
    };
}

mod generated;
