use std::ffi::*;

use crate::{gc::GC, hl, sort::hl_bsort};

#[no_mangle]
pub unsafe extern "C" fn hlp_bytes_blit(
    dst: *mut c_char,
    dpos: c_int,
    src: *const c_char,
    spos: c_int,
    len: c_int,
) {
    if len <= 0 || dst.is_null() || src.is_null() {
        return;
    }
    std::ptr::copy_nonoverlapping(
        src.add(spos as usize) as *const u8,
        dst.add(dpos as usize) as *mut u8,
        len as usize,
    );
}

#[no_mangle]
pub unsafe extern "C" fn hlp_alloc_bytes(size: c_int) -> *mut hl::vbyte {
    if size < 0 {
        panic!("invalid size for bytes allocation")
    }
    let _size: usize = size as usize;
    let allocator = GC.get_mut().expect("expected to get garbage collector");
    let bytes_ptr = allocator.allocate(_size).expect("Out of memory").as_ptr() as *mut hl::vbyte;

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
    if a.is_null() || b.is_null() || apos < 0 || bpos < 0 || len < 0 {
        return 0;
    }
    let a_ptr = a.add(apos as usize);
    let b_ptr = b.add(bpos as usize);
    for i in 0..(len as usize) {
        let xa = *a_ptr.add(i);
        let xb = *b_ptr.add(i);
        if xa < xb {
            return -1;
        }
        if xa > xb {
            return 1;
        }
    }
    0
}

#[no_mangle]
pub unsafe extern "C" fn hlp_bytes_compare16(
    a: *const hl::vbyte,
    b: *const hl::vbyte,
    len: c_int,
) -> c_int {
    if a.is_null() || b.is_null() || len < 0 {
        return 0;
    }
    let a16 = a as *const c_ushort;
    let b16 = b as *const c_ushort;
    for i in 0..(len as usize) {
        let xa = *a16.add(i);
        let xb = *b16.add(i);
        if xa < xb {
            return -1;
        }
        if xa > xb {
            return 1;
        }
    }
    0
}

pub struct BoyerMooreHorspool {
    shift: [usize; 256],
}

impl BoyerMooreHorspool {
    pub fn new() -> Self {
        BoyerMooreHorspool { shift: [0; 256] }
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
            while match_size < pattern.len()
                && block[match_base + match_size] == pattern[match_size]
            {
                match_size += 1;
            }

            if match_size == pattern.len() {
                return Some(match_base);
            }

            // Standard BMH: shift based on the last character of the current window
            match_base += self.shift[block[match_base + pattern.len() - 1] as usize];
        }

        None
    }

    fn prepare_shift_table(&mut self, pattern: &[u8]) {
        self.shift.fill(pattern.len());

        for (i, &byte) in pattern.iter().enumerate().take(pattern.len() - 1) {
            self.shift[byte as usize] = pattern.len() - i - 1;
        }
    }
}

pub fn memfind_rb(block: &[u8], pattern: &[u8], repeat_find: &mut bool) -> Option<usize> {
    static mut BMH: Option<BoyerMooreHorspool> = None;

    let bmh = unsafe { BMH.get_or_insert_with(BoyerMooreHorspool::new) };

    let result = bmh.find(block, pattern, *repeat_find);
    *repeat_find = true;
    result
}

#[no_mangle]
pub unsafe extern "C" fn hlp_bytes_find(
    r#where: *const hl::vbyte,
    pos: c_int,
    len: c_int,
    which: *const hl::vbyte,
    wpos: c_int,
    wlen: c_int,
) -> c_int {
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
pub unsafe extern "C" fn hlp_bytes_fill(
    bytes: *mut hl::vbyte,
    pos: c_int,
    len: c_int,
    value: c_int,
) {
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
pub unsafe extern "C" fn hlp_bsort_i32(
    bytes: *mut hl::vbyte,
    pos: i32,
    len: i32,
    cmp: *mut hl::vclosure,
) {
    hl_bsort::<i32>(bytes, pos, len, cmp);
}

#[no_mangle]
pub unsafe extern "C" fn hlp_bsort_i64(
    bytes: *mut hl::vbyte,
    pos: i32,
    len: i32,
    cmp: *mut hl::vclosure,
) {
    hl_bsort::<i64>(bytes, pos, len, cmp);
}

#[no_mangle]
pub unsafe extern "C" fn hlp_bsort_f32(
    bytes: *mut hl::vbyte,
    pos: i32,
    len: i32,
    cmp: *mut hl::vclosure,
) {
    hl_bsort::<f32>(bytes, pos, len, cmp);
}

#[no_mangle]
pub unsafe extern "C" fn hlp_bsort_f64(
    bytes: *mut hl::vbyte,
    pos: i32,
    len: i32,
    cmp: *mut hl::vclosure,
) {
    hl_bsort::<f64>(bytes, pos, len, cmp);
}

#[no_mangle]
pub unsafe extern "C" fn hlp_bsort_bool(
    bytes: *mut hl::vbyte,
    pos: i32,
    len: i32,
    cmp: *mut hl::vclosure,
) {
    hl_bsort::<bool>(bytes, pos, len, cmp);
}

unsafe fn read_utf16z(bytes: *const hl::vbyte) -> Vec<u16> {
    if bytes.is_null() {
        return Vec::new();
    }
    let mut len = 0usize;
    let ptr = bytes as *const u16;
    while *ptr.add(len) != 0 {
        len += 1;
    }
    std::slice::from_raw_parts(ptr, len).to_vec()
}

unsafe fn alloc_utf16_bytes(units: &[u16], out_size: *mut c_int) -> *mut hl::vbyte {
    if !out_size.is_null() {
        *out_size = units.len() as c_int;
    }
    let out = hlp_alloc_bytes(((units.len() + 1) * 2) as c_int) as *mut u16;
    if out.is_null() {
        return std::ptr::null_mut();
    }
    std::ptr::copy_nonoverlapping(units.as_ptr(), out, units.len());
    *out.add(units.len()) = 0;
    out as *mut hl::vbyte
}

fn url_encode_utf8(input: &[u8]) -> Vec<u8> {
    let mut out = Vec::with_capacity(input.len());
    for &b in input {
        let is_unreserved = b.is_ascii_alphanumeric() || matches!(b, b'-' | b'_' | b'.' | b'~');
        if is_unreserved {
            out.push(b);
        } else {
            out.push(b'%');
            out.push(b"0123456789ABCDEF"[(b >> 4) as usize]);
            out.push(b"0123456789ABCDEF"[(b & 0x0F) as usize]);
        }
    }
    out
}

fn from_hex_digit(b: u8) -> Option<u8> {
    match b {
        b'0'..=b'9' => Some(b - b'0'),
        b'a'..=b'f' => Some(10 + b - b'a'),
        b'A'..=b'F' => Some(10 + b - b'A'),
        _ => None,
    }
}

fn url_decode_utf8(input: &[u8]) -> Vec<u8> {
    let mut out = Vec::with_capacity(input.len());
    let mut i = 0usize;
    while i < input.len() {
        if input[i] == b'%' && i + 2 < input.len() {
            if let (Some(h1), Some(h2)) =
                (from_hex_digit(input[i + 1]), from_hex_digit(input[i + 2]))
            {
                out.push((h1 << 4) | h2);
                i += 3;
                continue;
            }
        }
        out.push(input[i]);
        i += 1;
    }
    out
}

#[no_mangle]
pub unsafe extern "C" fn hlp_url_encode(
    bytes: *const hl::vbyte,
    out_size: *mut c_int,
) -> *mut hl::vbyte {
    let units = read_utf16z(bytes);
    let input = String::from_utf16_lossy(&units);
    let encoded = url_encode_utf8(input.as_bytes());
    let encoded_ascii = String::from_utf8_lossy(&encoded);
    let out_units: Vec<u16> = encoded_ascii.encode_utf16().collect();
    alloc_utf16_bytes(&out_units, out_size)
}

#[no_mangle]
pub unsafe extern "C" fn hlp_url_decode(
    bytes: *const hl::vbyte,
    out_size: *mut c_int,
) -> *mut hl::vbyte {
    let units = read_utf16z(bytes);
    let input = String::from_utf16_lossy(&units);
    let decoded = url_decode_utf8(input.as_bytes());
    let decoded_str = String::from_utf8(decoded)
        .unwrap_or_else(|e| String::from_utf8_lossy(e.as_bytes()).into_owned());
    let out_units: Vec<u16> = decoded_str.encode_utf16().collect();
    alloc_utf16_bytes(&out_units, out_size)
}
