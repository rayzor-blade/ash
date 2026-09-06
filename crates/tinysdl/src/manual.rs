//! The primitives the generator cannot express.
//!
//! Everything else forwards its arguments and hands back what the host
//! returns. These do not: the value crossing back is a string, which lives in
//! the guest's heap, so this side has to allocate it and the host can only
//! fill it in.

use crate::abi::*;

/// Longest string this asks the host for. GL's answers here are a vendor, a
/// renderer and two version strings; a driver name is the long one and is
/// nowhere near this.
const MAX: i32 = 256;

#[link(wasm_import_module = "env")]
extern "C" {
    /// Write GL string `name` into `into`, at most `len` bytes including its
    /// terminator, and answer how many it wrote. Zero for a name the host has
    /// no answer for.
    fn ash_host_sdl_gl_get_string(name: i32, into: i32, len: i32) -> i32;
}

/// `Pi_B`
///
/// # Safety
/// Called by the VM through the resolver below.
#[no_mangle]
pub unsafe extern "C" fn sdl_gl_get_string(name: i32) -> *mut vbyte {
    let buffer = hlp_alloc_bytes(MAX);
    if buffer.is_null() {
        return std::ptr::null_mut();
    }
    if ash_host_sdl_gl_get_string(name, buffer as i32, MAX) <= 0 {
        // Null, not an empty string: a GL that does not recognise the name
        // returns null, and Heaps tells them apart.
        return std::ptr::null_mut();
    }
    buffer
}
define_prim!(hlp_gl_get_string, sdl_gl_get_string, "Pi_B");
