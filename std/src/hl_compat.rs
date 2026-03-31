//! HashLink ABI compatibility layer.
//!
//! The HashLink HDLLs (fmt.hdll, sdl.hdll, etc.) are compiled against
//! `libhl.dylib` which exports `hl_` prefixed symbols. Ash's stdlib
//! exports `hlp_` prefixed symbols. This module re-exports ash functions
//! under their `hl_` names so HDLLs can link against ash_std directly.

use std::ffi::c_void;
use std::ptr;

// ============================================================================
// Global type singletons (hlt_*)
//
// HashLink HDLLs reference these as global variables, not functions.
// Each is a static hl_type with the correct kind field set.
// ============================================================================

// Wrapper to make hl_type Sync for static globals
#[repr(transparent)]
struct SyncHlType(hl_type);
unsafe impl Sync for SyncHlType {}

macro_rules! hlt_global {
    ($name:ident, $kind:expr) => {
        #[no_mangle]
        pub static $name: SyncHlType = SyncHlType(hl_type {
            kind: $kind,
            __bindgen_anon_1: hl_type__bindgen_ty_1 {
                obj: std::ptr::null_mut(),
            },
            vobj_proto: std::ptr::null_mut(),
            mark_bits: std::ptr::null_mut(),
        });
    };
}

hlt_global!(hlt_void, 0);      // HVOID
hlt_global!(hlt_i32, 3);      // HI32
hlt_global!(hlt_i64, 4);      // HI64
hlt_global!(hlt_f32, 5);      // HF32
hlt_global!(hlt_f64, 6);      // HF64
hlt_global!(hlt_bool, 7);     // HBOOL
hlt_global!(hlt_bytes, 8);    // HBYTES
hlt_global!(hlt_dyn, 9);      // HDYN
hlt_global!(hlt_array, 12);   // HARRAY
hlt_global!(hlt_dynobj, 16);  // HDYNOBJ
hlt_global!(hlt_abstract, 17); // HABSTRACT

use crate::gc::{ImmixAllocator, GC};
use crate::hl::{self, hl_buffer, hl_type, hl_type__bindgen_ty_1, varray, vbyte, vdynamic};
use crate::strings;

// ============================================================================
// Direct aliases: forward hl_XXX to hlp_XXX
// ============================================================================

#[no_mangle]
pub unsafe extern "C" fn hl_alloc_array(t: *mut hl_type, size: i32) -> *mut varray {
    crate::array::hlp_alloc_array(t, size)
}

#[no_mangle]
pub unsafe extern "C" fn hl_alloc_dynamic(t: *mut hl_type) -> *mut vdynamic {
    let result = crate::obj::hlp_alloc_dynamic(t);
    if std::env::var("ASH_DBG_ALLOC").is_ok() {
        let kind = if !t.is_null() { (*t).kind } else { 999 };
        eprintln!("[hl_alloc_dynamic] t={:p} kind={} -> {:p}", t, kind, result);
    }
    result
}

#[no_mangle]
pub unsafe extern "C" fn hl_alloc_dynobj() -> *mut vdynamic {
    crate::obj::hlp_alloc_dynobj() as *mut vdynamic
}

#[no_mangle]
pub unsafe extern "C" fn hl_dyn_setd(d: *mut vdynamic, hfield: i32, value: f64) {
    crate::obj::hlp_dyn_setd(d, hfield, value);
}

#[no_mangle]
pub unsafe extern "C" fn hl_dyn_seti(d: *mut vdynamic, hfield: i32, t: *mut hl_type, value: i32) {
    crate::obj::hlp_dyn_seti(d, hfield, t, value);
}

#[no_mangle]
pub unsafe extern "C" fn hl_dyn_setp(
    d: *mut vdynamic,
    hfield: i32,
    t: *mut hl_type,
    value: *mut c_void,
) {
    crate::obj::hlp_dyn_setp(d, hfield, t, value);
}

#[no_mangle]
pub unsafe extern "C" fn hl_hash_gen(name: *const hl::uchar, cache_name: bool) -> i32 {
    crate::obj::hlp_hash_gen(name, cache_name)
}

#[no_mangle]
pub unsafe extern "C" fn hl_make_dyn(data: *mut c_void, t: *mut hl_type) -> *mut vdynamic {
    crate::cast::hlp_make_dyn(data, t)
}

#[no_mangle]
pub unsafe extern "C" fn hl_throw(v: *mut vdynamic) {
    crate::error::hlp_throw(v);
}

#[no_mangle]
pub unsafe extern "C" fn hl_dyn_call(c: *mut vdynamic, args: *mut varray) -> *mut vdynamic {
    crate::fun::hlp_call_method(c, args)
}

// ============================================================================
// Buffer functions (forward to existing hlp_buffer_*)
// ============================================================================

#[no_mangle]
pub unsafe extern "C" fn hl_alloc_buffer(init: *const hl::uchar) -> *mut c_void {
    crate::buffer::hlp_alloc_buffer() as *mut c_void
}

#[no_mangle]
pub unsafe extern "C" fn hl_buffer_char(b: *mut c_void, c: u16) {
    crate::buffer::hlp_buffer_char(b as *mut hl_buffer, c);
}

#[no_mangle]
pub unsafe extern "C" fn hl_buffer_str(b: *mut c_void, s: *const hl::uchar) {
    crate::buffer::hlp_buffer_str(b as *mut hl_buffer, s);
}

#[no_mangle]
pub unsafe extern "C" fn hl_buffer_cstr(b: *mut c_void, s: *const u8) {
    if s.is_null() {
        return;
    }
    let cstr = std::ffi::CStr::from_ptr(s as *const i8);
    if let Ok(st) = cstr.to_str() {
        let utf16 = crate::strings::str_to_uchar_ptr(st);
        crate::buffer::hlp_buffer_str(b as *mut hl_buffer, utf16);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hl_buffer_content(b: *mut c_void, len: *mut i32) -> *const hl::uchar {
    crate::buffer::hlp_buffer_content(b as *mut hl_buffer, len)
}

#[no_mangle]
pub unsafe extern "C" fn hl_buffer_val(b: *mut c_void, v: *mut vdynamic) {
    crate::buffer::hlp_buffer_val(b as *mut hl_buffer, v);
}

// ============================================================================
// GC functions
// ============================================================================

#[no_mangle]
pub unsafe extern "C" fn hl_gc_alloc_gen(
    t: *mut hl_type,
    size: i32,
    _flags: i32,
) -> *mut c_void {
    let gc = GC.get_mut().expect("GC");
    if let Some(ptr) = gc.allocate(size as usize) {
        let p = ptr.as_ptr() as *mut vdynamic;
        (*p).t = t;
        p as *mut c_void
    } else {
        ptr::null_mut()
    }
}

#[no_mangle]
pub unsafe extern "C" fn hl_add_root(ptr: *mut c_void) {
    let gc = GC.get_mut().expect("GC");
    gc.register_persistent(ptr as *mut vdynamic);
}

#[no_mangle]
pub unsafe extern "C" fn hl_remove_root(ptr: *mut c_void) {
    let gc = GC.get_mut().expect("GC");
    gc.unregister_persistent(ptr as *mut vdynamic);
}

// ============================================================================
// String/encoding functions
// ============================================================================

#[no_mangle]
pub unsafe extern "C" fn hl_to_utf16(str: *const u8) -> *const hl::uchar {
    if str.is_null() {
        return ptr::null();
    }
    let cstr = std::ffi::CStr::from_ptr(str as *const i8);
    if let Ok(s) = cstr.to_str() {
        let ptr = crate::strings::str_to_uchar_ptr(s);
        return ptr;
    }
    ptr::null()
}

#[no_mangle]
pub unsafe extern "C" fn hl_to_utf8(str: *const hl::uchar) -> *const u8 {
    if std::env::var("ASH_DBG_SHADER").is_ok() {
        if !str.is_null() && (str as usize) > 0x10000 {
            let mut len = 0;
            while *str.add(len) != 0 && len < 200 { len += 1; }
            let s = String::from_utf16_lossy(std::slice::from_raw_parts(str, len));
            eprintln!("[hl_to_utf8] len={} first100={:?}", len, &s[..s.len().min(100)]);
        } else {
            eprintln!("[hl_to_utf8] str={:p} (null or invalid)", str);
        }
    }
    if str.is_null() {
        return ptr::null();
    }
    let mut len = 0;
    while *str.add(len) != 0 {
        len += 1;
    }
    let s = String::from_utf16_lossy(std::slice::from_raw_parts(str, len));
    let cstr = std::ffi::CString::new(s).unwrap_or_default();
    let ptr = cstr.as_ptr() as *const u8;
    std::mem::forget(cstr);
    ptr
}

#[no_mangle]
pub unsafe extern "C" fn hl_from_utf8(str: *const u8, len: i32) -> *const hl::uchar {
    if str.is_null() {
        return ptr::null();
    }
    let bytes = std::slice::from_raw_parts(str, len as usize);
    let s = String::from_utf8_lossy(bytes);
    let ptr = crate::strings::str_to_uchar_ptr(&s);
    ptr
}

#[no_mangle]
pub unsafe extern "C" fn hl_alloc_strbytes(len: i32) -> *mut hl::uchar {
    let gc = GC.get_mut().expect("GC");
    let size = (len as usize + 1) * 2; // u16 per char + null
    if let Some(ptr) = gc.allocate(size) {
        let p = ptr.as_ptr() as *mut hl::uchar;
        std::ptr::write_bytes(p, 0, len as usize + 1);
        p
    } else {
        ptr::null_mut()
    }
}

#[no_mangle]
pub unsafe extern "C" fn hl_hash_utf8(name: *const u8) -> i32 {
    // Hash UTF-8 bytes by first converting to UTF-16
    if name.is_null() {
        return 0;
    }
    let cstr = std::ffi::CStr::from_ptr(name as *const i8);
    if let Ok(s) = cstr.to_str() {
        let utf16 = crate::strings::str_to_uchar_ptr(s);
        crate::obj::hlp_hash_gen(utf16, true)
    } else {
        0
    }
}

// ============================================================================
// Misc
// ============================================================================

#[no_mangle]
pub unsafe extern "C" fn hl_alloc_bytes(size: i32) -> *mut u8 {
    crate::bytes::hlp_alloc_bytes(size)
}

#[no_mangle]
pub unsafe extern "C" fn hl_copy_bytes(dst: *mut u8, src: *const u8, size: i32) {
    if !dst.is_null() && !src.is_null() && size > 0 {
        std::ptr::copy_nonoverlapping(src, dst, size as usize);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hl_throw_buffer(buf: *mut c_void) {
    // Convert buffer content to a string and throw as exception
    let mut len: i32 = 0;
    let content = crate::buffer::hlp_buffer_content(buf as *mut hl_buffer, &mut len);
    if !content.is_null() {
        let gc = GC.get_mut().expect("GC");
        let d = gc
            .allocate(std::mem::size_of::<vdynamic>())
            .expect("alloc")
            .as_ptr() as *mut vdynamic;
        (*d).t = crate::types::hlt_bytes();
        (*d).v.ptr = content as *mut c_void;
        hl_throw(d);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hl_blocking(_enter: bool) {}

// ============================================================================
// UTF-16 string utilities (exported by libhl.dylib)
// ============================================================================

// ustrdup is in ucs2.rs

#[no_mangle]
pub unsafe extern "C" fn ustrlen(s: *const u16) -> usize {
    if s.is_null() { return 0; }
    let mut len = 0;
    while *s.add(len) != 0 { len += 1; }
    len
}

#[no_mangle]
pub unsafe extern "C" fn uprintf(_fmt: *const u16, ...) {
    // Stub: UTF-16 printf
}

#[no_mangle]
pub unsafe extern "C" fn usprintf(_out: *mut u16, _size: i32, _fmt: *const u16, ...) -> i32 {
    0
}

#[no_mangle]
pub unsafe extern "C" fn uvszprintf(
    _out: *mut u16, _size: i32, _fmt: *const u16, _args: *mut c_void,
) -> i32 {
    0
}
