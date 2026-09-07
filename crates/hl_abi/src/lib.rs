//! The HashLink C ABI, as a native library sees it.
//!
//! An HDLL includes `hl.h` and links against libhl; this is the same thing in
//! Rust, shared by every library in this repo that is one.
//!
//! **This crate must never gain a dependency on `ash_std`, or define a symbol
//! of its own.** Everything here is either a `#[repr(C)]` layout the runtime
//! and a library must agree on, or a function the runtime exports, so a
//! library built on it links nothing of the runtime into itself. Depending on
//! the runtime as a Rust crate would put a second copy of its `#[no_mangle]`
//! exports in the library's archive, and two strong definitions of one name
//! cannot be linked.

// These are `hl.h`'s names, spelled its way on purpose: someone checking this
// against the header should be able to read the two side by side.
#![allow(non_camel_case_types)]

/// Export a primitive the way `DEFINE_PRIM` does: a resolver that reports the
/// signature through an out-parameter and returns the real function.
///
/// Never the primitive itself -- storing the resolver and calling it as the
/// primitive writes a signature string through whatever the first argument
/// happens to be.
#[macro_export]
macro_rules! define_prim {
    ($resolver:ident, $function:ident, $signature:literal) => {
        /// # Safety
        /// `sign` must be writable, which is what a caller of a `DEFINE_PRIM`
        /// resolver passes.
        #[no_mangle]
        pub unsafe extern "C" fn $resolver(
            sign: *mut *const ::std::ffi::c_char,
        ) -> *mut ::std::ffi::c_void {
            if !sign.is_null() {
                *sign = concat!($signature, "\0").as_ptr() as *const ::std::ffi::c_char;
            }
            $function as *mut ::std::ffi::c_void
        }
    };
}

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

/// An array of nothing.
///
/// What a host with no screen has to say when asked to list its displays or
/// its devices. An empty array rather than null, because the caller iterates
/// it and null would be a crash where "none" is the truth.
///
/// # Safety
/// Calls the runtime's allocator, so the GC must be up -- which it is by the
/// time any primitive here is reached.
#[inline]
pub unsafe fn empty_array() -> *mut varray {
    hlp_alloc_array(hlp_type_bytes(), 0)
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
