//! The primitives the generator cannot express.
//!
//! Everything else forwards its arguments and hands back what the host
//! returns. These do not: the value crossing back is a string, which lives in
//! the guest's heap, so this side has to allocate it and the host can only
//! fill it in.

use std::ffi::c_void;

use crate::abi::*;

/// Longest string this asks the host for. GL's answers here are a vendor, a
/// renderer and two version strings; a driver name is the long one and is
/// nowhere near this.
const MAX: i32 = 256;

#[link(wasm_import_module = "env")]
extern "C" {
    /// End a frame, and answer nothing this side reads.
    ///
    /// It returns a value only because the host has to be able to fail here:
    /// a host function that suspends and then refuses to resume reports that
    /// as an error, and a wasm import returning nothing has nowhere to put
    /// one.
    fn ash_host_sdl_win_swap_window(window: i32) -> i32;
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

/// `PXsdl_window__v`
///
/// The end of a frame, and the point at which the host takes control back. A
/// Heaps main loop never returns, so a host that answered this and let the
/// guest carry on would not run again until the program exited -- which in a
/// page means never, and a frozen tab.
///
/// # Safety
/// Called by the VM through the resolver below.
#[no_mangle]
pub unsafe extern "C" fn sdl_win_swap_window(window: *mut c_void) {
    let _ = ash_host_sdl_win_swap_window(window as i32);
}
define_prim!(hlp_win_swap_window, sdl_win_swap_window, "PXsdl_window__v");
