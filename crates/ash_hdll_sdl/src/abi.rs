//! The HashLink C ABI, as a library sees it.
//!
//! The same file the sqlite library has, with the string and boxing helpers
//! this one needs: sdl passes Haxe strings, and twenty-four of its primitives
//! take or return a `Null<Int>`, which is a pointer to unwrap rather than an
//! integer.
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

/// A Haxe `String` as it crosses this boundary: `_STRING` is `_OBJ(_BYTES
/// _I32)`, so the bytes and their length, and the bytes are UTF-16.
#[repr(C)]
pub struct vstring {
    pub t: *mut hl_type,
    pub bytes: *mut u16,
    pub length: c_int,
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

extern "C" {
    /// A GC-allocated box of type `t`, its value unset.
    pub fn hlp_alloc_dynamic(t: *mut hl_type) -> *mut vdynamic;

    /// The persistent type singleton for `Int`. NOT `hl.h`'s plain `hlt_i32`
    /// static: that is the plain descriptor, while the allocation above is
    /// made against the GC-registered one.
    pub fn hlp_type_i32() -> *mut hl_type;

    /// The program's allocator. Used as this library's own, so that one
    /// allocator owns the one heap: a side module that brought its own would
    /// carve pages out of the same memory in parallel, and anything it
    /// allocated could never be freed by the program.
    pub fn malloc(size: usize) -> *mut c_void;
    pub fn free(ptr: *mut c_void);
    pub fn realloc(ptr: *mut c_void, size: usize) -> *mut c_void;
}

/// The bytes of a `String`, or null.
///
/// # Safety
/// `s` must be a `vstring` the VM allocated, or null.
#[inline]
pub unsafe fn string_bytes(s: *mut vstring) -> i32 {
    if s.is_null() {
        0
    } else {
        (*s).bytes as i32
    }
}

/// Its length in UTF-16 code units, which is not its length in bytes.
///
/// # Safety
/// As [`string_bytes`].
#[inline]
pub unsafe fn string_length(s: *mut vstring) -> i32 {
    if s.is_null() {
        0
    } else {
        (*s).length
    }
}

/// What a `Null<Int>` holds.
///
/// Boxed rather than raw, so this is a pointer to unwrap and not an integer.
/// Null reads as zero, which is what every GL name of zero already means:
/// "no object".
///
/// # Safety
/// `d` must be a `vdynamic` the VM allocated, or null.
#[inline]
pub unsafe fn unbox_i32(d: *mut vdynamic) -> i32 {
    if d.is_null() {
        0
    } else {
        (*d).v.i
    }
}

/// A `Null<Int>` holding `value`.
///
/// # Safety
/// Calls the runtime's allocator, so the GC must be up -- which it is by the
/// time any primitive here is reached.
#[inline]
pub unsafe fn box_i32(value: i32) -> *mut vdynamic {
    let d = hlp_alloc_dynamic(hlp_type_i32());
    if !d.is_null() {
        (*d).v.i = value;
    }
    d
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
