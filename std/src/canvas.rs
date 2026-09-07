//! Putting a frame on screen, for a host that has somewhere to put it.
//!
//! One primitive: hand the host a rectangle of RGBA pixels out of the
//! program's own memory and let it show them. That is the whole interface,
//! and it is deliberately not SDL -- a program that wants windows, events and
//! a GL context loads an HDLL for it. This is for the case where the program
//! has already drawn the pixels itself and only needs them displayed.
//!
//! It exists because of where a wasm module runs in a browser. HashLink runs
//! in a Worker, and a Worker that is inside a call into the program never
//! returns to its event loop -- so nothing on the page can be asked to draw a
//! frame while the program is running. An `OffscreenCanvas` transferred to
//! that Worker can be drawn synchronously from inside the call, which makes
//! this one function enough.
//!
//! The pixels stay where the program put them. Nothing is copied out of
//! linear memory here; the host reads the rectangle and presents it.

use crate::hl::vbyte;

/// Show `width` by `height` RGBA pixels starting at `data`.
///
/// Byte order is what a canvas wants -- red, green, blue, alpha -- so a
/// little-endian `i32` write of `0xAABBGGRR` sets one pixel.
///
/// Returns 1 if the host displayed it, 0 if it has no display. A host without
/// one is not an error: the same program runs headless, and the frames it
/// would have shown are simply counted.
///
/// # Safety
///
/// `data` must point at `width * height * 4` readable bytes.
#[no_mangle]
pub unsafe extern "C" fn hlp_canvas_present(data: *mut vbyte, width: i32, height: i32) -> bool {
    if data.is_null() || width <= 0 || height <= 0 {
        return false;
    }
    present(data, width, height)
}

#[cfg(target_family = "wasm")]
unsafe fn present(data: *mut vbyte, width: i32, height: i32) -> bool {
    ash_host_canvas_present(data as *const u8, width, height) != 0
}

/// Nowhere to present to. A native build of a program that draws its own
/// pixels still runs; it just draws them into memory and nobody looks.
#[cfg(not(target_family = "wasm"))]
unsafe fn present(_data: *mut vbyte, _width: i32, _height: i32) -> bool {
    false
}

#[cfg(target_family = "wasm")]
#[link(wasm_import_module = "env")]
extern "C" {
    /// Show a rectangle of RGBA pixels from the module's memory. Non-zero if
    /// the host had a display for them.
    fn ash_host_canvas_present(data: *const u8, width: i32, height: i32) -> i32;
}
