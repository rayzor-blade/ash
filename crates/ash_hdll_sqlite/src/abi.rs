//! The HashLink C ABI, as a library sees it.
//!
//! An HDLL includes `hl.h` and links against libhl; this is the same thing in
//! Rust. Everything here is either a `#[repr(C)]` layout the runtime and this
//! library must agree on, or a function the runtime exports -- so this file
//! contributes no code of its own, and a library built on it links nothing of
//! the runtime's into itself.
//!
//! That is the whole reason it is written out rather than imported from
//! `ash_std`: depending on the runtime as a Rust crate would put a second
//! copy of its `#[no_mangle]` exports in this library's archive, and two
//! strong definitions of one name cannot be linked.

// These are `hl.h`'s names, and they are spelled its way on purpose: someone
// checking this against the header should be able to read the two side by
// side. The runtime's own bindings suppress the same lints for the same
// reason.
#![allow(non_camel_case_types)]

use std::ffi::c_void;
use std::os::raw::c_int;

/// A type descriptor. Opaque: nothing here reads one, it only passes them
/// back to the runtime that handed them over.
#[repr(C)]
pub struct hl_type {
    _private: [u8; 0],
}

/// A byte, in HashLink's spelling. `hl.Bytes` is a pointer to these, and a
/// string's bytes are UTF-16.
pub type vbyte = u8;

/// A HashLink array: a header, then the elements.
#[repr(C)]
pub struct varray {
    pub t: *mut hl_type,
    pub at: *mut hl_type,
    pub size: c_int,
    pub __pad: c_int,
}

/// A boxed value. The union is as wide as its widest member, and only the
/// field matching `t` may be read.
#[repr(C)]
pub struct vdynamic {
    pub t: *mut hl_type,
    pub v: vdynamic_value,
}

#[repr(C)]
pub union vdynamic_value {
    pub b: bool,
    pub ui8: u8,
    pub ui16: u16,
    pub i: c_int,
    pub i64: i64,
    pub f: f32,
    pub d: f64,
    pub bytes: *mut vbyte,
    pub ptr: *mut c_void,
}

/// The elements of an array, which follow its header.
///
/// # Safety
/// `a` must be an array the runtime allocated, and `T` its element type.
#[inline]
pub unsafe fn hl_aptr<T>(a: *mut varray) -> *mut T {
    (a as *mut u8).add(std::mem::size_of::<varray>()) as *mut T
}

extern "C" {
    /// GC-allocated bytes, zeroed and scanned as data.
    pub fn hlp_alloc_bytes(size: c_int) -> *mut vbyte;
    /// A GC-allocated array of `size` elements of type `at`.
    pub fn hlp_alloc_array(at: *mut hl_type, size: c_int) -> *mut varray;
    /// A GC-allocated box of type `t`, its value unset.
    pub fn hlp_alloc_dynamic(t: *mut hl_type) -> *mut vdynamic;

    /// The persistent type singletons. NOT `hl.h`'s plain `hlt_*` statics:
    /// these are the GC-registered ones every allocation above is made
    /// against.
    pub fn hlp_type_i32() -> *mut hl_type;
    pub fn hlp_type_f64() -> *mut hl_type;
    pub fn hlp_type_bytes() -> *mut hl_type;
    pub fn hlp_type_array() -> *mut hl_type;
    pub fn hlp_type_dyn() -> *mut hl_type;

    /// The program's allocator. Used as this library's own, so that one
    /// allocator owns the one heap: a side module that brought its own would
    /// carve pages out of the same memory in parallel, and anything it
    /// allocated could never be freed by the program.
    pub fn malloc(size: usize) -> *mut c_void;
    pub fn free(ptr: *mut c_void);
    pub fn realloc(ptr: *mut c_void, size: usize) -> *mut c_void;
}

/// Allocate through the program, never through a second allocator of our own.
///
/// Rust's default for wasm is its own `dlmalloc` over its own arena. Two
/// allocators on one linear memory do not corrupt each other, but they do
/// mean this library's memory is invisible to the program's and vice versa,
/// and the first pointer that crosses between them is a bug that appears far
/// from its cause.
pub struct ProgramAllocator;

unsafe impl std::alloc::GlobalAlloc for ProgramAllocator {
    unsafe fn alloc(&self, layout: std::alloc::Layout) -> *mut u8 {
        // wasi-libc's malloc aligns to 16, which covers every alignment Rust
        // asks for here; anything stricter would need posix_memalign, and
        // asking for it would be a silent mis-alignment rather than a
        // refusal.
        if layout.align() > 16 {
            return std::ptr::null_mut();
        }
        malloc(layout.size()) as *mut u8
    }

    unsafe fn dealloc(&self, ptr: *mut u8, _layout: std::alloc::Layout) {
        free(ptr as *mut c_void)
    }

    unsafe fn realloc(&self, ptr: *mut u8, layout: std::alloc::Layout, new: usize) -> *mut u8 {
        if layout.align() > 16 {
            return std::ptr::null_mut();
        }
        realloc(ptr as *mut c_void, new) as *mut u8
    }
}
