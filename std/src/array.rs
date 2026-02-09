use std::ptr;

use crate::{
    error::hlp_error,
    gc::GC,
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

    let gc = GC.get_mut().expect("expected to call GC");
    // let flag = if hl_is_ptr(at) {
    //     MEM_KIND_DYNAMIC
    // } else {
    //     MEM_KIND_NOPTR
    // } | MEM_ZERO;

    let a = gc
        .allocate(total_size)
        .expect("Failed to allocate array")
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
pub unsafe fn hlp_array_blit(dst: *mut varray, dpos: i32, src: *const varray, spos: i32, len: i32) {
    let size = hlp_type_size((*dst).at);

    let dst_ptr = hl_aptr::<vbyte>(dst).add((dpos as usize) * (size as usize));
    let src_ptr = hl_aptr::<vbyte>(src as *mut varray).add((spos as usize) * (size as usize));

    ptr::copy(
        src_ptr,
        dst_ptr as *mut vbyte,
        (len as usize) * (size as usize),
    );
}
