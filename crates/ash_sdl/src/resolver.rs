//! DEFINE_PRIM resolver protocol for HashLink HDLLs.
//!
//! Each exported symbol `hlp_<name>` is a resolver function that:
//! 1. Takes `*mut *const c_char` (pointer to signature output)
//! 2. Writes the type signature string to `*sign`
//! 3. Returns the address of the actual implementation function

/// Defines an HDLL resolver entry point.
///
/// Usage: `define_prim!(hlp_init_once, "Pb", sdl_init_once);`
/// - `$resolver`: the exported symbol name (e.g., `hlp_init_once`)
/// - `$sig`: the type signature string (e.g., `"Pb"` for `() -> bool`)
/// - `$impl_fn`: the actual implementation function
macro_rules! define_prim {
    ($resolver:ident, $sig:literal, $impl_fn:expr) => {
        #[no_mangle]
        pub unsafe extern "C" fn $resolver(
            sign: *mut *const std::ffi::c_char,
        ) -> *mut std::ffi::c_void {
            static SIG: &[u8] = concat!($sig, "\0").as_bytes();
            if !sign.is_null() {
                *sign = SIG.as_ptr() as *const std::ffi::c_char;
            }
            $impl_fn as *mut std::ffi::c_void
        }
    };
}

pub(crate) use define_prim;
