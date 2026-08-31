// `static mut` + raw-pointer access is this module's deliberate story (the
// VM's single-threaded invariant): `static_mut_refs` demands the
// `&raw`/deref spelling, and these two style lints then flag exactly that
// spelling. The trio cannot all be satisfied at once.
#![allow(clippy::deref_addrof, dangerous_implicit_autorefs)]
use std::ffi::*;

use crate::{hl, sort::hl_bsort};

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
    // HashLink implements this with `memmove`: `Bytes.blit` and the typed
    // vector helpers are allowed to copy overlapping ranges within the same
    // allocation. `copy_nonoverlapping` made those valid calls undefined
    // behaviour (and an immediate abort under Rust's debug UB checks).
    std::ptr::copy(
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

    crate::gc::gc_alloc(_size).expect("Out of memory").as_ptr() as *mut hl::vbyte
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

/// Upstream hl_string_compare (bytes.c): memcmp over len UTF-16 chars.
/// Byte-wise memcmp, exactly like upstream — NOT a u16-wise compare.
#[no_mangle]
pub unsafe extern "C" fn hlp_string_compare(
    a: *const hl::vbyte,
    b: *const hl::vbyte,
    len: c_int,
) -> c_int {
    if a.is_null() || b.is_null() || len <= 0 {
        return 0;
    }
    libc::memcmp(
        a as *const std::ffi::c_void,
        b as *const std::ffi::c_void,
        (len as usize) * 2,
    )
}

#[no_mangle]
pub unsafe extern "C" fn hlp_bytes_offset(bytes: *mut hl::vbyte, offset: c_int) -> *mut hl::vbyte {
    if bytes.is_null() {
        return std::ptr::null_mut();
    }
    bytes.add(offset as usize)
}

/// Upstream hl_bytes_subtract (bytes.c): `(int)(a - b)`.
#[no_mangle]
pub unsafe extern "C" fn hlp_bytes_subtract(a: *const hl::vbyte, b: *const hl::vbyte) -> c_int {
    // Deliberately NOT `offset_from`: Haxe hands this pair of pointers to us
    // with no promise they came from one allocation (hl.Bytes wraps arbitrary
    // native memory), which is exactly the precondition `offset_from` makes
    // UB to violate. Address arithmetic reproduces C's wraparound, and the
    // result is truncated to 32 bits upstream regardless.
    (a as isize).wrapping_sub(b as isize) as c_int
}

/// Upstream hl_bytes_address64 (bytes.c): the pointer as an integer.
#[no_mangle]
pub unsafe extern "C" fn hlp_bytes_address64(a: *const hl::vbyte) -> i64 {
    // Upstream returns `int_val` (intptr_t), but the DEFINE_PRIM declares
    // _I64, so the value the VM sees is 64 bits wide on every host. Going
    // through `usize` widens by zero-extension, which is the identity on a
    // 64-bit host and keeps a 32-bit host's high addresses from arriving as
    // negative i64 — hlp_bytes_from_address64 truncates back either way.
    a as usize as i64
}

/// Upstream hl_bytes_address (bytes.c): pointer split into two i32 halves,
/// low returned, high written through the `_REF(_I32)` out-parameter.
#[no_mangle]
pub unsafe extern "C" fn hlp_bytes_address(a: *const hl::vbyte, high: *mut c_int) -> c_int {
    let addr = a as usize as u64;
    if !high.is_null() {
        // The C is `#ifdef HL_64`: 64-bit hosts publish the top 32 bits,
        // 32-bit hosts store 0 because the whole address already fits in the
        // low word. Keyed off the target pointer width rather than assuming
        // a 64-bit host, so the two halves stay a faithful round-trip pair
        // with hlp_bytes_from_address on either.
        *high = if cfg!(target_pointer_width = "64") {
            (addr >> 32) as c_int
        } else {
            0
        };
    }
    addr as u32 as c_int
}

/// Upstream hl_bytes_from_address (bytes.c): rebuild a pointer from the two
/// i32 halves produced by hlp_bytes_address.
#[no_mangle]
pub unsafe extern "C" fn hlp_bytes_from_address(low: c_int, high: c_int) -> *mut hl::vbyte {
    // Upstream fills a `struct { int low; int high; }` and reinterprets it as
    // a pointer, purely to dodge an MSVC bug shifting by 32; on the
    // little-endian targets HashLink supports that struct layout IS
    // `low | high << 32`, so the arithmetic form is bit-identical. The 32-bit
    // arm ignores `high` entirely, as the `#ifdef HL_64` does.
    if cfg!(target_pointer_width = "64") {
        ((((high as u32) as u64) << 32) | ((low as u32) as u64)) as usize as *mut hl::vbyte
    } else {
        (low as u32) as usize as *mut hl::vbyte
    }
}

/// Upstream hl_bytes_from_address64 (bytes.c): integer address back to pointer.
#[no_mangle]
pub unsafe extern "C" fn hlp_bytes_from_address64(v: i64) -> *mut hl::vbyte {
    // Declared _I64 by the prim even though upstream types the parameter
    // `int_val`, so the argument arrives 64 bits wide whatever the host; the
    // `usize` hop truncates to the host pointer width the way the C cast does.
    v as usize as *mut hl::vbyte
}

pub struct BoyerMooreHorspool {
    shift: [usize; 256],
}

impl Default for BoyerMooreHorspool {
    fn default() -> Self {
        Self::new()
    }
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

    let bmh = unsafe { (*(&raw mut BMH)).get_or_insert_with(BoyerMooreHorspool::new) };

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

/// Upstream hl_bytes_rfind (bytes.c): index of the LAST occurrence of
/// `which[0..wlen]` inside `where[0..len]`, or -1. Unlike hl_bytes_find there
/// is no start offset — the scan begins at the last position where the needle
/// still fits, `len - wlen`, and walks down to 0.
#[no_mangle]
pub unsafe extern "C" fn hlp_bytes_rfind(
    r#where: *const hl::vbyte,
    len: c_int,
    which: *const hl::vbyte,
    wlen: c_int,
) -> c_int {
    // Upstream has no negative guard, so a negative `wlen` reaches memcmp as a
    // huge size_t. The siblings in this file all reject negatives up front,
    // and for the negative case C does handle (wlen > len) the answer is the
    // same -1, so nothing observable changes.
    if len < 0 || wlen < 0 {
        return -1;
    }
    // These two boundary cases are upstream's, in upstream's order: a needle
    // longer than the haystack loses before anything else, and an empty needle
    // then matches "at end" — `len`, not 0, which is what makes this the
    // mirror of hl_bytes_find rather than its duplicate. Note len == wlen == 0
    // therefore yields 0. Neither path touches memory, so both stay reachable
    // with null pointers exactly as in C; the null guard goes after them and
    // covers only the paths that actually dereference.
    if wlen > len {
        return -1;
    }
    if wlen == 0 {
        return len;
    }
    if r#where.is_null() || which.is_null() {
        return -1;
    }

    let haystack = std::slice::from_raw_parts(r#where, len as usize);
    let needle = std::slice::from_raw_parts(which, wlen as usize);
    let mut pos = (len - wlen) as usize;
    loop {
        if haystack[pos..pos + needle.len()] == *needle {
            return pos as c_int;
        }
        // `pos` is unsigned here, so C's `pos--` / `while (pos >= 0)` has to
        // terminate before the decrement wraps.
        if pos == 0 {
            return -1;
        }
        pos -= 1;
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

#[cfg(test)]
mod bytes_tests {
    use std::ffi::c_int;

    /// The VM never reaches these six through Rust. It dlsym's the exported
    /// C name — `format!("hlp_{}", native.name)`, see
    /// crates/ash_interp/src/interpreter/natives.rs — and calls the result
    /// through the signature the upstream DEFINE_PRIM declares. A renamed
    /// export, a dropped `#[no_mangle]`, or a signature that drifted from the
    /// prim is therefore a *runtime* failure, never a build error; that is the
    /// missing-symbol shape a user hit in production.
    ///
    /// Re-importing the six as C symbols puts their exported names on the
    /// linker's critical path, and spelling the prototypes straight from
    /// bytes.c's DEFINE_PRIM lines means every test below crosses the same ABI
    /// edge the VM does rather than a friendlier Rust one.
    mod prim {
        use crate::hl::vbyte;
        use std::ffi::c_int;

        extern "C" {
            /// `DEFINE_PRIM(_I32,bytes_rfind,_BYTES _I32 _BYTES _I32)`
            pub fn hlp_bytes_rfind(
                r#where: *const vbyte,
                len: c_int,
                which: *const vbyte,
                wlen: c_int,
            ) -> c_int;
            /// `DEFINE_PRIM(_I32,bytes_subtract, _BYTES _BYTES)`
            pub fn hlp_bytes_subtract(a: *const vbyte, b: *const vbyte) -> c_int;
            /// `DEFINE_PRIM(_I32,bytes_address, _BYTES _REF(_I32))`
            pub fn hlp_bytes_address(a: *const vbyte, high: *mut c_int) -> c_int;
            /// `DEFINE_PRIM(_I64,bytes_address64,_BYTES)`
            pub fn hlp_bytes_address64(a: *const vbyte) -> i64;
            /// `DEFINE_PRIM(_BYTES,bytes_from_address, _I32 _I32)`
            pub fn hlp_bytes_from_address(low: c_int, high: c_int) -> *mut vbyte;
            /// `DEFINE_PRIM(_BYTES,bytes_from_address64, _I64)`
            pub fn hlp_bytes_from_address64(v: i64) -> *mut vbyte;
        }
    }

    /// `hl_bytes_rfind` over two slices, handed over the way the VM hands them
    /// over: a base pointer plus an explicit length. Tests that need a length
    /// which disagrees with the buffer call the prim directly.
    unsafe fn rfind(hay: &[u8], needle: &[u8]) -> c_int {
        prim::hlp_bytes_rfind(
            hay.as_ptr(),
            hay.len() as c_int,
            needle.as_ptr(),
            needle.len() as c_int,
        )
    }

    // ---- hl_bytes_rfind -------------------------------------------------

    #[test]
    fn rfind_locates_the_needle_at_the_start_middle_and_end() {
        unsafe {
            let hay = b"hello world";
            assert_eq!(rfind(hay, b"hello"), 0, "at the start");
            assert_eq!(rfind(hay, b"o w"), 4, "straddling the middle");
            assert_eq!(rfind(hay, b"world"), 6, "flush against the end");
            assert_eq!(rfind(hay, b"d"), 10, "single byte at the last index");
            assert_eq!(rfind(hay, b"h"), 0, "single byte at index 0");
        }
    }

    #[test]
    fn rfind_returns_minus_one_when_the_needle_is_absent() {
        unsafe {
            assert_eq!(rfind(b"hello world", b"z"), -1);
            assert_eq!(rfind(b"hello world", b"World"), -1, "case matters");
            // A prefix that starts matching in several places but never
            // completes: the scan must not report a partial window.
            assert_eq!(rfind(b"ababab", b"abc"), -1);
            assert_eq!(rfind(b"hello", b"lloX"), -1);
        }
    }

    #[test]
    fn rfind_matches_a_needle_equal_to_the_whole_haystack() {
        unsafe {
            assert_eq!(rfind(b"hello", b"hello"), 0);
            assert_eq!(rfind(b"a", b"a"), 0);
            // One byte shorter still fits, at the end.
            assert_eq!(rfind(b"hello", b"ello"), 1);
        }
    }

    /// Upstream checks `wlen > len` FIRST and `wlen == 0` second:
    ///
    /// ```c
    /// if( wlen > len ) return -1;
    /// if( wlen == 0 ) return len; // at end
    /// ```
    ///
    /// Worth stating plainly, because the ordering is the one thing here that a
    /// test cannot pin down. The two orders disagree only when `len` is
    /// negative — `wlen == 0` and `wlen > len` are true together nowhere else —
    /// and this port rejects a negative `len` in a guard that runs ahead of
    /// both. So no input distinguishes them as written: transposing the two
    /// lines leaves every assertion below green (checked by mutation).
    ///
    /// What is pinned instead is the behaviour the ordering exists to produce.
    /// An oversized needle always loses, including where it is only just
    /// oversized, and the negative-`len` input still answers upstream's -1
    /// rather than the `-5` a port would return if it both dropped the guard
    /// and transposed the checks.
    #[test]
    fn rfind_rejects_a_needle_longer_than_the_haystack() {
        unsafe {
            let buf = b"abc";
            assert_eq!(prim::hlp_bytes_rfind(buf.as_ptr(), 3, buf.as_ptr(), 5), -1);
            assert_eq!(
                prim::hlp_bytes_rfind(buf.as_ptr(), 3, buf.as_ptr(), 4),
                -1,
                "one byte too long is still too long"
            );
            assert_eq!(prim::hlp_bytes_rfind(buf.as_ptr(), 0, buf.as_ptr(), 1), -1);
            assert_eq!(
                prim::hlp_bytes_rfind(buf.as_ptr(), -5, buf.as_ptr(), 0),
                -1,
                "upstream's answer at negative len, not the -5 of the other order"
            );
        }
    }

    /// `if( wlen == 0 ) return len; // at end` — the haystack length, not 0.
    /// That asymmetry is what makes rfind the mirror of `hl_bytes_find`
    /// (which answers 0 for an empty needle) rather than its duplicate.
    #[test]
    fn rfind_answers_the_haystack_length_for_an_empty_needle() {
        unsafe {
            assert_eq!(rfind(b"hello", b""), 5);
            assert_eq!(rfind(b"", b""), 0, "len == 0 && wlen == 0 yields 0");
            let hay = b"hello";
            assert_eq!(
                prim::hlp_bytes_rfind(hay.as_ptr(), 3, hay.as_ptr(), 0),
                3,
                "the answer follows the len argument, not the allocation"
            );
        }
    }

    #[test]
    fn rfind_returns_the_last_of_several_occurrences() {
        unsafe {
            assert_eq!(rfind(b"abcabcabc", b"abc"), 6);
            assert_eq!(rfind(b"xxabcxx", b"abc"), 2, "sole occurrence");
            // Genuinely overlapping windows, where a scan that stepped by the
            // needle length instead of by one would stop a match early.
            assert_eq!(rfind(b"aaaa", b"aa"), 2);
            assert_eq!(rfind(b"aaaaa", b"aaa"), 2);
            assert_eq!(rfind(b"abababa", b"aba"), 4);
            assert_eq!(rfind(b"aaaa", b"a"), 3);
        }
    }

    #[test]
    fn rfind_handles_a_zero_length_haystack() {
        unsafe {
            assert_eq!(rfind(b"", b"a"), -1, "any real needle is oversized");
            assert_eq!(rfind(b"", b""), 0);
        }
    }

    /// `len` is the search window, not a capacity hint: a match lying wholly
    /// or partly beyond it must not be reported, even though the bytes are
    /// there to be read.
    #[test]
    fn rfind_never_looks_past_len() {
        unsafe {
            let hay = b"abcXabc";
            let needle = b"abc";
            assert_eq!(prim::hlp_bytes_rfind(hay.as_ptr(), 7, needle.as_ptr(), 3), 4);
            assert_eq!(
                prim::hlp_bytes_rfind(hay.as_ptr(), 6, needle.as_ptr(), 3),
                0,
                "the trailing match no longer fits inside len"
            );
            assert_eq!(prim::hlp_bytes_rfind(hay.as_ptr(), 5, needle.as_ptr(), 3), 0);
            assert_eq!(prim::hlp_bytes_rfind(hay.as_ptr(), 4, needle.as_ptr(), 3), 0);
            assert_eq!(
                prim::hlp_bytes_rfind(hay.as_ptr(), 2, needle.as_ptr(), 3),
                -1,
                "the leading match no longer fits either"
            );
        }
    }

    /// `hl.Bytes` is arbitrary binary, not text: NUL is an ordinary byte and
    /// nothing here may stop at one or assume valid UTF-8.
    #[test]
    fn rfind_is_binary_safe() {
        unsafe {
            let hay = [0x00u8, 0xFF, 0x00, 0x41, 0x00, 0xFF, 0x00];
            assert_eq!(rfind(&hay, &[0x00, 0xFF, 0x00]), 4);
            assert_eq!(rfind(&hay, &[0x00]), 6);
            assert_eq!(rfind(&hay, &[0x41]), 3);
            assert_eq!(rfind(&hay, &[0xFF, 0x41]), -1);
        }
    }

    /// Guards this port adds over the C, which has none: upstream would pass a
    /// negative `wlen` to `memcmp` as a huge `size_t` and would dereference a
    /// null `where`. Both additions are documented at the implementation as
    /// deliberate, and both are placed *after* the two boundary rules so the
    /// no-memory answers stay reachable exactly as in C.
    #[test]
    fn rfind_guards_negative_lengths_and_null_pointers() {
        unsafe {
            let buf = b"abc";
            assert_eq!(prim::hlp_bytes_rfind(buf.as_ptr(), 3, buf.as_ptr(), -1), -1);
            assert_eq!(prim::hlp_bytes_rfind(buf.as_ptr(), -1, buf.as_ptr(), 1), -1);
            assert_eq!(
                prim::hlp_bytes_rfind(std::ptr::null(), 3, buf.as_ptr(), 1),
                -1
            );
            assert_eq!(
                prim::hlp_bytes_rfind(buf.as_ptr(), 3, std::ptr::null(), 1),
                -1
            );
            // ...but the two cases C answers without touching memory still
            // answer, null pointers and all.
            assert_eq!(
                prim::hlp_bytes_rfind(std::ptr::null(), 5, std::ptr::null(), 0),
                5,
                "empty needle: 'at end' needs no memory"
            );
            assert_eq!(
                prim::hlp_bytes_rfind(std::ptr::null(), 1, std::ptr::null(), 2),
                -1,
                "oversized needle: rejected before any read"
            );
        }
    }

    // ---- hl_bytes_subtract ----------------------------------------------

    /// `(int)(a - b)` over `vbyte*`: a byte distance, signed in both
    /// directions, zero for equal pointers.
    #[test]
    fn subtract_measures_a_signed_byte_distance() {
        unsafe {
            // Owned for the whole test; nothing below dereferences it.
            let buf = [0u8; 64];
            let base: *const u8 = buf.as_ptr();
            let a = base.add(10);
            let b = base.add(3);

            assert_eq!(prim::hlp_bytes_subtract(a, b), 7, "forwards");
            assert_eq!(prim::hlp_bytes_subtract(b, a), -7, "backwards");
            assert_eq!(prim::hlp_bytes_subtract(a, a), 0, "equal pointers");
            assert_eq!(prim::hlp_bytes_subtract(base, base), 0);
            // The unit is one byte, not one element of anything wider.
            assert_eq!(prim::hlp_bytes_subtract(base.add(1), base), 1);
            assert_eq!(prim::hlp_bytes_subtract(base.add(63), base), 63);
            assert_eq!(prim::hlp_bytes_subtract(base, base.add(63)), -63);
            // Null is just an address here, as it is in C.
            assert_eq!(prim::hlp_bytes_subtract(std::ptr::null(), std::ptr::null()), 0);

            std::hint::black_box(&buf); // the buffer outlives every assertion
        }
    }

    /// Two unrelated allocations are still a legal argument pair — `hl.Bytes`
    /// wraps arbitrary native memory, so the prim may not assume one object.
    /// The distance itself is whatever the allocator chose; what is pinned is
    /// that the two directions negate and a pointer minus itself is 0.
    #[test]
    fn subtract_accepts_pointers_from_different_allocations() {
        unsafe {
            let left = Box::new([0u8; 16]);
            let right = Box::new([0u8; 16]);
            let (l, r): (*const u8, *const u8) = (left.as_ptr(), right.as_ptr());

            let d = prim::hlp_bytes_subtract(l, r);
            assert_eq!(prim::hlp_bytes_subtract(r, l), d.wrapping_neg());
            assert_eq!(prim::hlp_bytes_subtract(l, l), 0);
            assert_eq!(prim::hlp_bytes_subtract(r, r), 0);

            std::hint::black_box((&left, &right)); // both buffers still live
        }
    }

    // ---- hl_bytes_address / hl_bytes_from_address ------------------------

    /// The pair splits a pointer into two i32 halves and puts it back. The
    /// high half is populated only under `#ifdef HL_64`, so assert against the
    /// host's own pointer width rather than hardcoding one.
    #[test]
    fn address_and_from_address_round_trip_a_real_pointer() {
        unsafe {
            // One on the stack and one on the heap: the two live far apart, so
            // between them they exercise more than a single high word.
            let stack = [0xABu8; 64];
            let heap = Box::new([0xABu8; 64]);
            for p in [stack.as_ptr(), heap.as_ptr()] {
                let addr = p as usize;

                let mut high: c_int = 0x5A5A_5A5A; // poisoned: must be overwritten
                let low = prim::hlp_bytes_address(p, &mut high);

                assert_eq!(low, addr as u32 as c_int, "low half is the bottom 32 bits");
                if cfg!(target_pointer_width = "64") {
                    assert_eq!(
                        high,
                        ((addr >> 32) as u32) as c_int,
                        "a 64-bit host publishes the top 32 bits"
                    );
                } else {
                    assert_eq!(high, 0, "a 32-bit host stores 0, as the #ifdef does");
                }

                let back = prim::hlp_bytes_from_address(low, high);
                assert_eq!(back as usize, addr, "the halves rebuild the same address");
                assert_eq!(*back, 0xAB, "and it is still the same live buffer");
            }

            std::hint::black_box((&stack, &heap)); // both outlive the assertions
        }
    }

    /// `_REF(_I32)` is always a real reference coming from Haxe, but the port
    /// tolerates a null out-parameter instead of faulting; the returned low
    /// half is unaffected either way.
    #[test]
    fn address_tolerates_a_null_high_out_parameter() {
        unsafe {
            let buf = [0u8; 8];
            let p: *const u8 = buf.as_ptr();
            assert_eq!(
                prim::hlp_bytes_address(p, std::ptr::null_mut()),
                p as usize as u32 as c_int
            );
            std::hint::black_box(&buf);
        }
    }

    /// The one way this pair can go wrong with no null and no wild pointer to
    /// show for it: `low` arrives as a *signed* i32, so it has to be
    /// recombined as `low as u32`. Sign-extending it instead would flood the
    /// high half with ones and hand Haxe a pointer far from the real one — and
    /// only for addresses whose bit 31 happens to be set, which a real
    /// allocation exercises perhaps half the time. So pin it with the bit
    /// pattern directly. Every value below is compared as an integer and is
    /// never dereferenced.
    #[test]
    fn from_address_zero_extends_both_halves() {
        unsafe {
            assert_eq!(prim::hlp_bytes_from_address(-1, 0) as usize as u64, 0xFFFF_FFFF);
            assert_eq!(
                prim::hlp_bytes_from_address(i32::MIN, 0) as usize as u64,
                0x8000_0000,
                "bit 31 set must not leak into the high half"
            );
            if cfg!(target_pointer_width = "64") {
                assert_eq!(
                    prim::hlp_bytes_from_address(-1, 1) as usize as u64,
                    0x1_FFFF_FFFF
                );
                assert_eq!(
                    prim::hlp_bytes_from_address(0, -1) as usize as u64,
                    0xFFFF_FFFF_0000_0000,
                    "the high half is zero-extended before the shift, too"
                );
            } else {
                assert_eq!(
                    prim::hlp_bytes_from_address(-1, 1) as usize as u64,
                    0xFFFF_FFFF,
                    "a 32-bit host ignores `high` entirely, as the #ifdef does"
                );
            }
            // hl_bytes_from_address64 truncates to the host width, as C's cast does.
            assert_eq!(
                prim::hlp_bytes_from_address64(-1) as usize,
                usize::MAX,
                "all-ones truncates to the host pointer width"
            );
        }
    }

    // ---- hl_bytes_address64 / hl_bytes_from_address64 --------------------

    #[test]
    fn address64_and_from_address64_round_trip_a_real_pointer() {
        unsafe {
            let buf = Box::new([0xCDu8; 32]);
            let p: *const u8 = buf.as_ptr();

            let v = prim::hlp_bytes_address64(p);
            assert_eq!(v as usize, p as usize, "the i64 carries the whole address");
            if cfg!(target_pointer_width = "64") {
                assert_eq!(v as u64, p as usize as u64);
            } else {
                // The zero-extension the implementation notes: a 32-bit host's
                // high addresses must not arrive as a negative i64.
                assert!(v > 0 && v <= u32::MAX as i64, "zero-extended, not sign-extended");
            }

            let back = prim::hlp_bytes_from_address64(v);
            assert_eq!(back as usize, p as usize);
            assert_eq!(*back, 0xCD, "and it is still the same live buffer");

            std::hint::black_box(&buf); // the buffer outlives every assertion
        }
    }

    /// The 32-bit-pair and the 64-bit spellings describe one pointer, so the
    /// four prims compose in every direction. Haxe relies on that when it
    /// stores an address in one form and rebuilds it through the other.
    #[test]
    fn the_two_address_spellings_agree() {
        unsafe {
            let buf = [0u8; 16];
            let p: *const u8 = buf.as_ptr();

            let mut high: c_int = 0;
            let low = prim::hlp_bytes_address(p, &mut high);
            let v = prim::hlp_bytes_address64(p);

            assert_eq!(
                prim::hlp_bytes_from_address(low, high),
                prim::hlp_bytes_from_address64(v)
            );
            assert_eq!(
                prim::hlp_bytes_address64(prim::hlp_bytes_from_address(low, high)),
                v
            );
            let mut high2: c_int = 0;
            assert_eq!(
                prim::hlp_bytes_address(prim::hlp_bytes_from_address64(v), &mut high2),
                low
            );
            assert_eq!(high2, high);

            if cfg!(target_pointer_width = "64") {
                assert_eq!(
                    (low as u32 as u64) | ((high as u32 as u64) << 32),
                    v as u64,
                    "the halves are exactly the halves of the 64-bit form"
                );
            }
            std::hint::black_box(&buf); // the buffer outlives every assertion
        }
    }

    /// A null `hl.Bytes` has to survive the trip too: Haxe compares the
    /// rebuilt pointer against null to detect one.
    #[test]
    fn a_null_pointer_round_trips_as_null() {
        unsafe {
            let mut high: c_int = 0x7FFF_FFFF; // poisoned
            assert_eq!(prim::hlp_bytes_address(std::ptr::null(), &mut high), 0);
            assert_eq!(high, 0);
            assert_eq!(prim::hlp_bytes_address64(std::ptr::null()), 0);
            assert!(prim::hlp_bytes_from_address(0, 0).is_null());
            assert!(prim::hlp_bytes_from_address64(0).is_null());
        }
    }
}
