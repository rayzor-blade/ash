//! The `simd` natives: the ash-simd primitives as exports of the runtime.
//!
//! A program declares them under lib `simd`, which ash resolves the way it
//! resolves `std` -- by the `hlp_<name>` symbol -- so no `simd.hdll` is ever
//! looked for. The bodies are `ash_simd`'s, the same code `simd.hdll` gives
//! stock HashLink.

use ash_simd::*;

// One `hlp_<name>` per table line, the argument list chosen by its shape.
macro_rules! export_direct {
    ($( ($body:ident, $export:ident, $shape:ident), )*) => {
        $( export_one!($body, $export, $shape); )*
    };
}

macro_rules! export_one {
    ($body:ident, $export:ident, slot2) => {
        #[no_mangle]
        pub unsafe extern "C" fn $export(d: *mut u8, di: i32, a: *const u8, ai: i32) {
            $body(d, di, a, ai)
        }
    };
    ($body:ident, $export:ident, slot3) => {
        #[no_mangle]
        pub unsafe extern "C" fn $export(
            d: *mut u8,
            di: i32,
            a: *const u8,
            ai: i32,
            b: *const u8,
            bi: i32,
        ) {
            $body(d, di, a, ai, b, bi)
        }
    };
    ($body:ident, $export:ident, slot4) => {
        #[no_mangle]
        pub unsafe extern "C" fn $export(
            d: *mut u8,
            di: i32,
            a: *const u8,
            ai: i32,
            b: *const u8,
            bi: i32,
            c: *const u8,
            ci: i32,
        ) {
            $body(d, di, a, ai, b, bi, c, ci)
        }
    };
    ($body:ident, $export:ident, splat_f) => {
        #[no_mangle]
        pub unsafe extern "C" fn $export(d: *mut u8, di: i32, x: f32) {
            $body(d, di, x)
        }
    };
    ($body:ident, $export:ident, splat_d) => {
        #[no_mangle]
        pub unsafe extern "C" fn $export(d: *mut u8, di: i32, x: f64) {
            $body(d, di, x)
        }
    };
    ($body:ident, $export:ident, splat_i) => {
        #[no_mangle]
        pub unsafe extern "C" fn $export(d: *mut u8, di: i32, x: i32) {
            $body(d, di, x)
        }
    };
    ($body:ident, $export:ident, shift) => {
        #[no_mangle]
        pub unsafe extern "C" fn $export(d: *mut u8, di: i32, a: *const u8, ai: i32, n: i32) {
            $body(d, di, a, ai, n)
        }
    };
    ($body:ident, $export:ident, reduce_f) => {
        #[no_mangle]
        pub unsafe extern "C" fn $export(a: *const u8, ai: i32) -> f32 {
            $body(a, ai)
        }
    };
    ($body:ident, $export:ident, reduce_d) => {
        #[no_mangle]
        pub unsafe extern "C" fn $export(a: *const u8, ai: i32) -> f64 {
            $body(a, ai)
        }
    };
    ($body:ident, $export:ident, reduce_i) => {
        #[no_mangle]
        pub unsafe extern "C" fn $export(a: *const u8, ai: i32) -> i32 {
            $body(a, ai)
        }
    };
    ($body:ident, $export:ident, load_arr) => {
        #[no_mangle]
        pub unsafe extern "C" fn $export(d: *mut u8, di: i32, arr: *const u8, index: i32) {
            $body(d, di, arr, index)
        }
    };
    ($body:ident, $export:ident, store_arr) => {
        #[no_mangle]
        pub unsafe extern "C" fn $export(arr: *mut u8, index: i32, a: *const u8, ai: i32) {
            $body(arr, index, a, ai)
        }
    };
}

for_each_prim!(export_direct);
