use std::ffi::*;
use std::cmp::min;

use crate::{gc::GC, hl, sort::hl_bsort};

use std::simd::{u16x8, u8x32};

#[no_mangle]
pub unsafe extern "C" fn hlp_bytes_blit(
    dst: *mut c_char,
    dpos: c_int,
    src: *const c_char,
    spos: c_int,
    len: c_int,
) {
    // Convert C types to Rust types
    let dst_slice = std::slice::from_raw_parts_mut(dst.add(dpos as usize) as *mut u8, len as usize);
    let src_slice = std::slice::from_raw_parts(src.add(spos as usize) as *const u8, len as usize);

    // Perform the byte copy
    dst_slice.copy_from_slice(src_slice);
}

#[no_mangle]
pub unsafe extern "C" fn hlp_alloc_bytes(size: c_int) -> *mut hl::vbyte {
    if size < 0 {
        panic!("invalid size for bytes allocation")
    }
    let _size: usize = size as usize;
    let allocator = GC.get_mut().expect("expected to get garbage collector");
    let bytes_ptr = allocator
        .allocate(_size)
        .expect("Out of memory")
        .as_ptr() as *mut hl::vbyte;

    bytes_ptr
}

#[no_mangle]
pub unsafe extern "C" fn hlp_bytes_compare(
    a: *const hl::vbyte,
    apos: c_int,
    b: *const hl::vbyte,
    bpos: c_int,
    len: c_int,
) -> c_int {
    // -- SCALAR APPROACH
    // // Check for null pointers and invalid length
    // if a.is_null() || b.is_null() || len < 0 {
    //     return 0; // Or another appropriate error value
    // }

    // let a_slice = std::slice::from_raw_parts(a.offset(apos as isize), len as usize);
    // let b_slice = std::slice::from_raw_parts(b.offset(bpos as isize), len as usize);
    // a_slice.cmp(b_slice) as c_int

    // -- SIMD APPROACH
    // Check for null pointers and invalid parameters
    if a.is_null() || b.is_null() || apos < 0 || bpos < 0 || len < 0 {
        return 0; // Or another appropriate error value
    }

    let a_ptr = a.offset(apos as isize);
    let b_ptr = b.offset(bpos as isize);

    let mut offset = 0;
    let simd_len = len as usize / 32;

    // SIMD comparison
    for _ in 0..simd_len {
        let a_simd = u8x32::from_slice(std::slice::from_raw_parts(a_ptr.add(offset), 32));
        let b_simd = u8x32::from_slice(std::slice::from_raw_parts(b_ptr.add(offset), 32));

        if a_simd < b_simd {
            return -1;
        } else if a_simd > b_simd {
            return 1;
        }

        offset += 32;
    }

    // Compare remaining elements
    for i in offset..(len as usize) {
        let byte_a = *a_ptr.add(i);
        let byte_b = *b_ptr.add(i);

        if byte_a < byte_b {
            return -1;
        } else if byte_a > byte_b {
            return 1;
        }
    }

    0 // Arrays are equal for the compared length
}

#[no_mangle]
pub unsafe extern "C" fn hlp_bytes_compare16(
    a: *const hl::vbyte,
    b: *const hl::vbyte,
    len: c_int,
) -> c_int {
    // -- SCALAR APPROACH
    // // Check for null pointers and invalid length
    // if a.is_null() || b.is_null() || len < 0 {
    //     return 0; // Or another appropriate error value
    // }

    // let a_slice16 = hlp_vbyte_to_ushort(a);
    // let b_slice16 = hlp_vbyte_to_ushort(b);

    // // Create slices of c_ushort
    // let a_slice = std::slice::from_raw_parts(a_slice16, len as usize);
    // let b_slice = std::slice::from_raw_parts(b_slice16, len as usize);

    // // Compare the slices
    // for (&char_a, &char_b) in a_slice.iter().zip(b_slice.iter()) {
    //     match char_a.cmp(&char_b) {
    //         Ordering::Less => return -1,
    //         Ordering::Greater => return 1,
    //         Ordering::Equal => continue,
    //     }
    // }

    // 0 // Arrays are equal for the compared length


    // -- SIMD APPROACH
    // Check for null pointers and invalid length
    if a.is_null() || b.is_null() || len < 0 {
        return 0; // Or another appropriate error value
    }

    let a_slice16 = a as *const c_ushort;
    let b_slice16 = b as *const c_ushort;

    let mut offset = 0;
    let simd_len = len as usize / 8;

    // SIMD comparison
    for _ in 0..simd_len {
        let a_simd = u16x8::from_slice(std::slice::from_raw_parts(a_slice16.add(offset), 8));
        let b_simd = u16x8::from_slice(std::slice::from_raw_parts(b_slice16.add(offset), 8));

        if a_simd < b_simd {
            return -1;
        } else if a_simd > b_simd {
            return 1;
        }

        offset += 8;
    }

    // Compare remaining elements
    for i in (offset * 2)..(len as usize) {
        let char_a = *a.add(i);
        let char_b = *b.add(i);

        if char_a < char_b {
            return -1;
        } else if char_a > char_b {
            return 1;
        }
    }

    0 // Arrays are equal for the compared length
}


pub struct BoyerMooreHorspool {
    shift: [usize; 256],
}

impl BoyerMooreHorspool {
    pub fn new() -> Self {
        BoyerMooreHorspool {
            shift: [0; 256],
        }
    }

    pub fn find(&mut self, block: &[u8], pattern: &[u8], repeat_find: bool) -> Option<usize> {
        if pattern.is_empty() {
            return Some(0);
        }

        if block.len() < pattern.len() {
            return None;
        }

        if !repeat_find {
            self.prepare_shift_table(pattern);
        }

        let limit = block.len() - pattern.len() + 1;
        let mut match_base = 0;

        while match_base < limit {
            let mut match_size = 0;
            while match_size < pattern.len() && 
                  block[match_base + match_size] == pattern[match_size] {
                match_size += 1;
            }

            if match_size == pattern.len() {
                return Some(match_base);
            }

            match_base += self.shift[block[min(match_base + pattern.len(), block.len() - 1)] as usize];
        }

        None
    }

    fn prepare_shift_table(&mut self, pattern: &[u8]) {
        self.shift.fill(pattern.len() + 1);

        for (i, &byte) in pattern.iter().enumerate().take(pattern.len() - 1) {
            self.shift[byte as usize] = pattern.len() - i - 1;
        }
    }
}


pub fn memfind_rb(block: &[u8], pattern: &[u8], repeat_find: &mut bool) -> Option<usize> {
    static mut BMH: Option<BoyerMooreHorspool> = None;

    let bmh = unsafe {
        BMH.get_or_insert_with(BoyerMooreHorspool::new)
    };

    let result = bmh.find(block, pattern, *repeat_find);
    *repeat_find = true;
    result
}

#[no_mangle]
pub unsafe extern "C" fn hlp_bytes_find(r#where: *const hl::vbyte, pos:c_int, len:c_int, which:*const hl::vbyte, wpos:c_int, wlen:c_int ) -> c_int {
    // Check for null pointers and invalid parameters
    if r#where.is_null() || which.is_null() || pos < 0 || len < 0 || wpos < 0 || wlen < 0 {
        return -1;
    }

    let where_slice = std::slice::from_raw_parts(r#where.offset(pos as isize), len as usize);
    let which_slice = std::slice::from_raw_parts(which.offset(wpos as isize), wlen as usize);

    let mut repeat_find = false;
    
    match memfind_rb(where_slice, which_slice, &mut repeat_find) {
        Some(found_index) => (found_index + pos as usize) as c_int,
        None => -1,
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_bytes_fill(bytes: *mut hl::vbyte, pos: c_int, len: c_int, value: c_int) {
    // Check for null pointer and invalid parameters
    if bytes.is_null() || pos < 0 || len < 0 {
        return; // Early return for invalid input
    }

    // Create a mutable slice from the input pointer
    let slice = std::slice::from_raw_parts_mut(bytes.offset(pos as isize), len as usize);

    // Fill the slice with the specified value
    slice.fill(value as u8);
}


#[no_mangle]
pub unsafe extern "C" fn hlp_bsort_i32(bytes: *mut hl::vbyte, pos: i32, len: i32, cmp: *mut hl::vclosure) {
    hl_bsort::<i32>(bytes, pos, len, cmp);
}

#[no_mangle]
pub unsafe extern "C" fn hlp_bsort_i64(bytes: *mut hl::vbyte, pos: i32, len: i32, cmp: *mut hl::vclosure) {
    hl_bsort::<i64>(bytes, pos, len, cmp);
}

#[no_mangle]
pub unsafe extern "C" fn hlp_bsort_f32(bytes: *mut hl::vbyte, pos: i32, len: i32, cmp: *mut hl::vclosure) {
    hl_bsort::<f32>(bytes, pos, len, cmp);
}

#[no_mangle]
pub unsafe extern "C" fn hlp_bsort_f64(bytes: *mut hl::vbyte, pos: i32, len: i32, cmp: *mut hl::vclosure) {
    hl_bsort::<f64>(bytes, pos, len, cmp);
}

#[no_mangle]
pub unsafe extern "C" fn hlp_bsort_bool(bytes: *mut hl::vbyte, pos: i32, len: i32, cmp: *mut hl::vclosure) {
    hl_bsort::<bool>(bytes, pos, len, cmp);
}