//! `simd.hdll`: the ash-simd primitives as a HashLink native library.
//!
//! Each primitive is exported as `hlp_<name>`, the resolver `DEFINE_PRIM`
//! writes, reporting the signature HashLink checks against the program's
//! declaration: `_FUN(ret, args)` spelled `P<args>_<ret>` in hl.h's letters. On ash the same bodies are exports of the runtime and this
//! file is never loaded.
//!
//! Build the cdylib and rename it: `libash_hdll_simd.so` (`.dylib`, `.dll`)
//! becomes `simd.hdll` beside the program or in HashLink's library directory.

use ash_simd::*;
use std::ffi::{c_char, c_void};

macro_rules! sig {
    (slot2) => {
        "PBiBi_v"
    };
    (slot3) => {
        "PBiBiBi_v"
    };
    (slot4) => {
        "PBiBiBiBi_v"
    };
    (splat_f) => {
        "PBif_v"
    };
    (splat_d) => {
        "PBid_v"
    };
    (splat_i) => {
        "PBii_v"
    };
    (shift) => {
        "PBiBii_v"
    };
    (reduce_f) => {
        "PBi_f"
    };
    (reduce_d) => {
        "PBi_d"
    };
    (reduce_i) => {
        "PBi_i"
    };
    (load_arr) => {
        "PBiAi_v"
    };
    (store_arr) => {
        "PAiBi_v"
    };
}

// `hl_abi::define_prim` spelled out, because its signature must be a literal
// and this one comes from the shape.
macro_rules! export_resolvers {
    ($( ($body:ident, $export:ident, $shape:ident), )*) => {
        $(
            /// # Safety
            /// `sign` must be writable, which is what a caller of a
            /// `DEFINE_PRIM` resolver passes.
            #[no_mangle]
            pub unsafe extern "C" fn $export(sign: *mut *const c_char) -> *mut c_void {
                if !sign.is_null() {
                    *sign = concat!(sig!($shape), "\0").as_ptr() as *const c_char;
                }
                $body as *mut c_void
            }
        )*
    };
}

for_each_prim!(export_resolvers);
