//! HashLink type definitions needed by SDL/GL wrappers.

use std::ffi::c_void;

/// hl_type (minimal — only kind field needed for most operations)
#[repr(C)]
pub struct hl_type {
    pub kind: u32,
}

/// vdynamic — HashLink dynamic value
#[repr(C)]
pub struct vdynamic {
    pub t: *mut hl_type,
    pub v: vdynamic_union,
}

#[repr(C)]
pub union vdynamic_union {
    pub i: i32,
    pub b: bool,
    pub f: f32,
    pub d: f64,
    pub ptr: *mut c_void,
}

/// varray — HashLink array
#[repr(C)]
pub struct varray {
    pub t: *mut hl_type,
    pub at: *mut hl_type,
    pub size: i32,
    pub _pad: i32,
    // data follows at offset 24
}

/// vbyte = u8
pub type vbyte = u8;

/// SDL event types (matches HashLink sdl.c enum)
#[repr(i32)]
#[derive(Clone, Copy)]
pub enum EventType {
    Quit = 0,
    MouseMove = 1,
    MouseLeave = 2,
    MouseDown = 3,
    MouseUp = 4,
    MouseWheel = 5,
    WindowState = 6,
    KeyDown = 7,
    KeyUp = 8,
    TextInput = 9,
    GControllerAdded = 100,
    GControllerRemoved = 101,
    GControllerDown = 102,
    GControllerUp = 103,
    GControllerAxis = 104,
    TouchDown = 200,
    TouchUp = 201,
    TouchMove = 202,
}

/// Window state changes
#[repr(i32)]
#[derive(Clone, Copy)]
pub enum WsChange {
    Show = 0,
    Hide = 1,
    Expose = 2,
    Move = 3,
    Resize = 4,
    Minimize = 5,
    Maximize = 6,
    Restore = 7,
    Enter = 8,
    Leave = 9,
    Focus = 10,
    Blur = 11,
    Close = 12,
}

/// event_data struct — matches HashLink sdl.c layout exactly
#[repr(C)]
pub struct EventData {
    pub t: *mut hl_type,
    pub event_type: i32,
    pub mouse_x: i32,
    pub mouse_y: i32,
    pub mouse_x_rel: i32,
    pub mouse_y_rel: i32,
    pub button: i32,
    pub wheel_delta: i32,
    pub state: i32,
    pub key_code: i32,
    pub scan_code: i32,
    pub key_repeat: i32,
    pub reference: i32,
    pub value: i32,
    pub __unused: i32,
    pub window: i32,
    pub drop_file: *mut vbyte,
}

// Extern symbols from ash_std (loaded with RTLD_GLOBAL)
extern "C" {
    pub fn hl_alloc_dynamic(t: *mut c_void) -> *mut vdynamic;
    pub fn hl_to_utf8(s: *const u16) -> *const u8;
    pub fn hl_to_utf16(s: *const u8) -> *const u16;
    pub fn hl_copy_bytes(src: *const u8, size: i32) -> *mut u8;
    pub fn hl_alloc_bytes(size: i32) -> *mut u8;
    pub fn hl_gc_alloc_gen(t: *mut c_void, size: i32, flags: i32) -> *mut c_void;
    pub fn hl_blocking(enter: bool);

    pub static hlt_i32: hl_type;
    pub static hlt_dyn: hl_type;
    pub static hlt_array: hl_type;
}

/// Allocate a boxed i32 (HNULL(HI32) / vdynamic with kind=HI32)
pub unsafe fn alloc_i32(v: i32) -> *mut vdynamic {
    let d = hl_alloc_dynamic(&hlt_i32 as *const _ as *mut c_void);
    if !d.is_null() {
        (*d).v.i = v;
    }
    d
}
