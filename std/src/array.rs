use std::ptr;

use crate::{
    error::hlp_error,
    hl::*,
    types::{hl_aptr, hlp_type_size},
};

#[no_mangle]
pub unsafe extern "C" fn hlp_array_type(a: *mut varray) -> *mut hl_type {
    (*a).at
}

#[no_mangle]
pub unsafe extern "C" fn hlp_alloc_array(at: *mut hl_type, size: i32) -> *mut varray {
    if size < 0 {
        hlp_error("Invalid array size".as_ptr() as *const uchar);
    }

    let esize = hlp_type_size(at);
    let total_size = std::mem::size_of::<varray>() + (esize as usize) * (size as usize);

    // let flag = if hl_is_ptr(at) {
    //     MEM_KIND_DYNAMIC
    // } else {
    //     MEM_KIND_NOPTR
    // } | MEM_ZERO;

    let a = crate::gc::gc_alloc(total_size)
        .unwrap_or_else(|| crate::gc::out_of_memory("an array"))
        .as_ptr() as *mut varray;

    (*a).t = crate::types::hlt_array();
    (*a).at = at;
    (*a).size = size;

    a
}

pub fn array_blit<T: Copy>(dst: &mut [T], dpos: usize, src: &[T], spos: usize, len: usize) {
    assert!(dpos + len <= dst.len(), "Destination range out of bounds");
    assert!(spos + len <= src.len(), "Source range out of bounds");

    unsafe {
        ptr::copy(src.as_ptr().add(spos), dst.as_mut_ptr().add(dpos), len);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_array_blit(
    dst: *mut varray,
    dpos: i32,
    src: *const varray,
    spos: i32,
    len: i32,
) {
    if dst.is_null() || src.is_null() || len <= 0 {
        return;
    }
    let dst_at = (*dst).at;
    let src_at = (*src).at;
    if dst_at.is_null() || src_at.is_null() {
        return;
    }
    // Guard against misaligned or invalid type pointers
    if (dst_at as usize) < 0x10000
        || !(dst_at as usize).is_multiple_of(std::mem::align_of::<usize>())
    {
        eprintln!(
            "[WARN] array_blit: invalid dst.at={:#x} dst={:p}",
            dst_at as usize, dst
        );
        return;
    }
    if (src_at as usize) < 0x10000
        || !(src_at as usize).is_multiple_of(std::mem::align_of::<usize>())
    {
        eprintln!(
            "[WARN] array_blit: invalid src.at={:#x} src={:p}",
            src_at as usize, src
        );
        return;
    }
    let size = hlp_type_size(dst_at);

    let dst_ptr = hl_aptr::<vbyte>(dst).add((dpos as usize) * (size as usize));
    let src_ptr = hl_aptr::<vbyte>(src as *mut varray).add((spos as usize) * (size as usize));

    ptr::copy(
        src_ptr,
        dst_ptr as *mut vbyte,
        (len as usize) * (size as usize),
    );
}

// DEFINE_PRIM(_BYTES, array_bytes, _ARR)
//
// The element run itself, not a copy. Aliasing is the point of the
// primitive: the bytes view a caller gets back has to see writes made
// through the array, and the header it skips is the same 24 bytes
// `hlp_array_blit` skips.
#[no_mangle]
pub unsafe extern "C" fn hlp_array_bytes(a: *mut varray) -> *mut vbyte {
    if a.is_null() {
        return ptr::null_mut();
    }
    hl_aptr::<vbyte>(a)
}

// DEFINE_PRIM(_CARRAY, alloc_carray, _TYPE _I32)
//
// A carray is a bare run of `size` instances with no varray header, indexed
// by the class's runtime stride. Each slot is a whole object, so the field
// bindings `hlp_alloc_obj` writes have to be written per element as well:
// a bound method read out of element k otherwise dispatches through the null
// left by the zeroing allocator.
#[no_mangle]
pub unsafe extern "C" fn hlp_alloc_carray(at: *mut hl_type, size: i32) -> *mut std::ffi::c_void {
    if at.is_null() || ((*at).kind != hl_type_kind_HOBJ && (*at).kind != hl_type_kind_HSTRUCT) {
        hlp_error(crate::strings::str_to_uchar_ptr("Invalid array type"));
        return ptr::null_mut();
    }
    if size < 0 {
        hlp_error(crate::strings::str_to_uchar_ptr("Invalid array size"));
        return ptr::null_mut();
    }

    let obj = (*at).__bindgen_anon_1.obj;
    if obj.is_null() {
        return ptr::null_mut();
    }
    let mut rt = (*obj).rt;
    if rt.is_null() || (*rt).methods.is_null() {
        rt = crate::obj::hl_get_obj_proto(at);
    }
    if rt.is_null() {
        return ptr::null_mut();
    }

    let stride = (*rt).size as usize;
    // Upstream multiplies these as ints and hands the wrapped product to the
    // allocator, so an absurd size buys a small block and then writes past
    // it; refusing is the only answer that stays inside the allocation.
    let Some(total) = stride.checked_mul(size as usize) else {
        return ptr::null_mut();
    };
    let Some(arr) = crate::gc::gc_alloc(total) else {
        return ptr::null_mut();
    };
    let arr = arr.as_ptr();

    if (*at).kind == hl_type_kind_HOBJ || (*rt).nbindings > 0 {
        for k in 0..(size as usize) {
            let o = arr.add(stride * k);
            // Structs are laid out without the type header; only HOBJ carries
            // one, and writing it into a struct slot would clobber field 0.
            if (*at).kind == hl_type_kind_HOBJ {
                (*(o as *mut vobj)).t = at;
            }
            for i in 0..(*rt).nbindings as usize {
                let b = (*rt).bindings.add(i);
                let offset = *(*rt).fields_indexes.add((*b).fid as usize);
                let slot = o.add(offset as usize) as *mut *mut std::ffi::c_void;
                *slot = if (*b).closure.is_null() {
                    (*b).ptr
                } else {
                    crate::fun::hlp_alloc_closure_ptr(
                        (*b).closure,
                        (*b).ptr,
                        o as *mut std::ffi::c_void,
                    ) as *mut std::ffi::c_void
                };
            }
        }
    }

    arr as *mut std::ffi::c_void
}

/// Move `len` elements between two C arrays of `at`.
///
/// A carray holds instances inline rather than pointers, so the stride is the
/// type's runtime size and not a machine word -- which is why this cannot be
/// `hlp_array_blit`. `memmove`, not `memcpy`: upstream permits the two arrays
/// to be the same one, and a self-blit with overlapping ranges is the ordinary
/// way to open or close a gap.
#[no_mangle]
pub unsafe extern "C" fn hlp_carray_blit(
    dst: *mut std::ffi::c_void,
    at: *mut hl_type,
    dpos: i32,
    src: *mut std::ffi::c_void,
    spos: i32,
    len: i32,
) {
    if at.is_null()
        || ((*at).kind != crate::hl::hl_type_kind_HOBJ
            && (*at).kind != crate::hl::hl_type_kind_HSTRUCT)
    {
        crate::error::hlp_error(crate::strings::str_to_uchar_ptr("Invalid array type"));
        return;
    }
    if dpos < 0 || spos < 0 || len < 0 {
        crate::error::hlp_error(crate::strings::str_to_uchar_ptr(
            "Invalid array pos or length",
        ));
        return;
    }
    let rt = crate::obj::hlp_get_obj_rt(at);
    if rt.is_null() {
        return;
    }
    let size = (*rt).size as usize;
    if size == 0 || dst.is_null() || src.is_null() {
        return;
    }
    std::ptr::copy(
        (src as *const u8).add(spos as usize * size),
        (dst as *mut u8).add(dpos as usize * size),
        len as usize * size,
    );
}

#[cfg(test)]
mod array_bytes_tests {
    use super::*;
    use crate::types::{hlt_bytes, hlt_f64, hlt_i32};

    /// The header the primitive skips, named here as a raw byte count rather
    /// than as `size_of::<varray>()`, because agreeing with upstream's 24 is
    /// the thing under test. `hlp_array_blit` skips the same run.
    const HEADER: usize = 24;

    #[test]
    fn a_null_array_yields_a_null_pointer() {
        unsafe {
            assert!(hlp_array_bytes(std::ptr::null_mut()).is_null());
        }
    }

    /// One test rather than several: these allocate from the process-wide GC,
    /// and the harness runs separate `#[test]` functions on separate threads.
    #[test]
    fn array_bytes_aliases_the_element_run() {
        unsafe {
            crate::gc::hlp_gc_init();

            assert_eq!(std::mem::size_of::<varray>(), HEADER);

            let a = hlp_alloc_array(hlt_i32(), 8);
            assert!(!a.is_null());
            assert_eq!((*a).size, 8);

            // The documented offset, checked against the header rather than
            // against hl_aptr, which is the helper the implementation uses.
            let bytes = hlp_array_bytes(a);
            assert_eq!(bytes as usize, a as usize + HEADER, "wrong offset");
            assert_eq!(bytes, hl_aptr::<vbyte>(a));

            // Aliasing is the point of the primitive: this is a view, not a
            // copy, so a write through the array has to be visible through
            // the bytes and the other way round.
            let slots = hl_aptr::<i32>(a);
            for i in 0..8 {
                *slots.add(i) = 0x0a0b_0c0d + i as i32;
            }
            let view = std::slice::from_raw_parts(bytes as *const i32, 8);
            for (i, got) in view.iter().enumerate() {
                assert_eq!(*got, 0x0a0b_0c0d + i as i32, "slot {i} did not alias");
            }
            *(bytes as *mut i32).add(3) = -1;
            assert_eq!(*slots.add(3), -1, "the write did not reach the array");

            // The offset is the header's, not the element's: a wider element
            // must not move the start of the run.
            let f = hlp_alloc_array(hlt_f64(), 4);
            assert_eq!(hlp_array_bytes(f) as usize, f as usize + HEADER);
            let b = hlp_alloc_array(hlt_bytes(), 4);
            assert_eq!(hlp_array_bytes(b) as usize, b as usize + HEADER);

            // A zero-length array still names where its elements would
            // start; upstream hands back the same address rather than null.
            let empty = hlp_alloc_array(hlt_i32(), 0);
            assert!(!empty.is_null());
            assert_eq!(hlp_array_bytes(empty) as usize, empty as usize + HEADER);
        }
    }

    /// DEFINE_PRIM(_BYTES, array_bytes, _ARR).
    #[test]
    fn the_exported_signature_is_the_one_upstream_declares() {
        let f: unsafe extern "C" fn(*mut varray) -> *mut vbyte = hlp_array_bytes;
        unsafe {
            assert!(f(std::ptr::null_mut()).is_null());
        }
    }

    /// `hlp_alloc_carray` has no runtime test here, and the gap is
    /// deliberate. Every path through it past the argument checks reads
    /// `(*at).obj->rt` -- stride, `nbindings`, `fields_indexes` -- and
    /// reaches `hl_get_obj_proto`, which dereferences the type's module
    /// context and allocates a vobj_proto through the GC. ash_std has no
    /// helper that builds an `hl_type_obj`; the ones in use are read out of
    /// loaded bytecode by the JIT and interpreter crates. Hand-rolling one
    /// here would mean fabricating the exact record whose misreading would
    /// corrupt memory, so the test would be more dangerous than the code it
    /// covers. The three argument guards are no better: each ends in
    /// `hlp_error`, which throws through a trap context a unit test does not
    /// have. What can be checked without a type is checked -- the symbol
    /// exists and its ABI signature is upstream's.
    #[test]
    fn alloc_carray_is_exported_with_upstreams_signature() {
        let _: unsafe extern "C" fn(*mut hl_type, i32) -> *mut std::ffi::c_void = hlp_alloc_carray;
    }
}
