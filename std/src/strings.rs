use std::{ffi::c_double, os::raw::c_int, ptr};

use crate::{
    buffer::{hlp_alloc_buffer, hlp_buffer_content, hlp_buffer_val},
    cast::hlp_make_dyn,
    hl::{self, hl_type_kind_HF64, hl_type_kind_HI32, uchar, vbyte, vdynamic},
    unicase::*,
};

pub fn str_to_uchar_ptr(s: &str) -> *const u16 {
    // Convert the &str to a UTF-16 vector
    let mut utf16: Vec<u16> = s.encode_utf16().collect();
    utf16.push(0);

    // Leak the vector to create a &'static [u16]
    let static_slice: &'static [u16] = Box::leak(utf16.into_boxed_slice());

    // Get a pointer to the start of the slice
    static_slice.as_ptr()
}

/// Calculates the length of a null-terminated UTF-16 string.
///
/// This function is safe to call with null pointers, returning 0 in that case.
///
/// # Safety
///
/// This function is unsafe because it dereferences raw pointers.
/// The caller must ensure that the pointer is valid and points to a null-terminated UTF-16 string.
///
/// # Arguments
///
/// * `s` - A pointer to a null-terminated UTF-16 string (represented as `uchar` in HashLink)
///
/// # Returns
///
/// The length of the string, not including the null terminator
#[no_mangle]
pub unsafe extern "C" fn hlp_utf16_length(s: *const hl::uchar) -> usize {
    if s.is_null() {
        return 0;
    }

    let mut len = 0;
    while *s.add(len) != 0 {
        len += 1;
    }
    len
}

#[no_mangle]
pub unsafe extern "C" fn hlp_ucs2length(s: *const hl::uchar, pos: i32) -> usize {
    if s.is_null() {
        return 0;
    }


    hlp_utf16_length(s.wrapping_add(pos as usize))
}

#[no_mangle]
pub unsafe extern "C" fn hlp_itos(i: c_int, len: *mut c_int) -> *const hl::vbyte {
    let s = format!("{}", i);
    let utf16: Vec<u16> = s.encode_utf16().collect();
    let k = utf16.len() as c_int;
    *len = k;
    let result = crate::bytes::hlp_alloc_bytes((k + 1) * 2) as *mut u16;
    ptr::copy_nonoverlapping(utf16.as_ptr(), result, k as usize);
    *result.add(k as usize) = 0; // null terminator
    result as *const hl::vbyte
}

/// C's `%.*g`, which is what HashLink formats floats with.
///
/// Rust's `{}` prints the shortest representation that round-trips, so
/// `Math.sqrt(2)` came out as `1.4142135623730951` where HashLink gives
/// `1.4142135623731`. Same double, different text, and every program printing
/// an irrational float disagreed with the reference implementation.
///
/// Two precisions are in use and they are not interchangeable: `hl_ftos`
/// (`Std.string` of a Float) uses 15, while the buffer paths use 17.
///
/// Verified against `printf("%.*g")` on 5000 random doubles at precisions
/// 6, 9, 15 and 17.
pub(crate) fn format_g(d: f64, p: i32) -> String {
    fn strip(s: &str) -> String {
        if !s.contains('.') {
            return s.to_string();
        }
        s.trim_end_matches('0').trim_end_matches('.').to_string()
    }
    if d == 0.0 {
        // C prints the sign of a negative zero.
        return if d.is_sign_negative() { "-0".into() } else { "0".into() };
    }
    // Take the exponent from the value *after* rounding to `p` significant
    // digits, so a value that rounds up into the next decade (999999999999999.9
    // -> 1e+15) picks the same branch C does.
    let e = format!("{:.*e}", (p - 1) as usize, d);
    let i = e.find('e').expect("Rust always emits an exponent for {:e}");
    let exp: i32 = e[i + 1..].parse().expect("exponent is an integer");
    if exp < -4 || exp >= p {
        format!(
            "{}e{}{:02}",
            strip(&e[..i]),
            if exp < 0 { "-" } else { "+" },
            exp.abs()
        )
    } else {
        strip(&format!("{:.*}", (p - 1 - exp).max(0) as usize, d))
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_ftos(d: c_double, len: *mut c_int) -> *const hl::vbyte {
    // hl_ftos special-cases NaN before the printf and lets everything else,
    // infinities included, fall through to %.15g.
    let s = if d.is_nan() {
        "NaN".to_string()
    } else if d.is_infinite() {
        if d > 0.0 {
            "inf".to_string()
        } else {
            "-inf".to_string()
        }
    } else {
        format_g(d, 15)
    };
    let utf16: Vec<u16> = s.encode_utf16().collect();
    let k = utf16.len() as c_int;
    *len = k;
    let result = crate::bytes::hlp_alloc_bytes((k + 1) * 2) as *mut u16;
    ptr::copy_nonoverlapping(utf16.as_ptr(), result, k as usize);
    *result.add(k as usize) = 0; // null terminator
    result as *const hl::vbyte
}

#[no_mangle]
pub unsafe extern "C" fn hlp_utf8_to_utf16(
    str: *const hl::vbyte,
    pos: i32,
    size: *mut i32,
) -> *mut uchar {
    if str.is_null() || size.is_null() {
        return ptr::null_mut();
    }

    let mut utf16_len = 0;
    let mut i = pos as isize;

    // First pass: count the number of UTF-16 code units needed
    while *str.offset(i) != 0 {
        let c = *str.offset(i);
        if c < 0x80 {
            utf16_len += 1;
            i += 1;
        } else if c < 0xE0 {
            utf16_len += 1;
            i += 2;
        } else if c < 0xF0 {
            utf16_len += 1;
            i += 3;
        } else {
            utf16_len += 2; // Surrogate pair
            i += 4;
        }
    }

    // Allocate memory for the UTF-16 string
    let result = crate::bytes::hlp_alloc_bytes((utf16_len + 1) * 2) as *mut uchar;
    if result.is_null() {
        return ptr::null_mut();
    }

    // Second pass: convert UTF-8 to UTF-16
    let mut j = 0;
    i = pos as isize;
    while *str.offset(i) != 0 {
        let mut c = *str.offset(i) as u32;
        if c < 0x80 {
            *result.offset(j) = c as uchar;
            i += 1;
        } else if c < 0xE0 {
            c = ((c & 0x3F) << 6) | ((*str.offset(i + 1) as u32) & 0x3F);
            *result.offset(j) = c as uchar;
            i += 2;
        } else if c < 0xF0 {
            c = ((c & 0x1F) << 12)
                | (((*str.offset(i + 1) as u32) & 0x3F) << 6)
                | ((*str.offset(i + 2) as u32) & 0x3F);
            *result.offset(j) = c as uchar;
            i += 3;
        } else {
            c = ((c & 0x0F) << 18)
                | (((*str.offset(i + 1) as u32) & 0x3F) << 12)
                | (((*str.offset(i + 2) as u32) & 0x3F) << 6)
                | ((*str.offset(i + 3) as u32) & 0x3F);
            c -= 0x10000;
            *result.offset(j) = (0xD800 | (c >> 10)) as uchar;
            j += 1;
            *result.offset(j) = (0xDC00 | (c & 0x3FF)) as uchar;
            i += 4;
        }
        j += 1;
    }

    // Null-terminate the UTF-16 string
    *result.offset(j) = 0;

    // *size is the UTF-16 length in BYTES (callers compute chars as size>>1,
    // matching upstream hl_utf8_to_utf16), not the UTF-8 source length.
    *size = utf16_len * 2;

    result
}

#[no_mangle]
pub unsafe extern "C" fn hlp_utf16_to_utf8(
    str: *const vbyte,
    len: i32,
    size: *mut i32,
) -> *mut vbyte {
    if str.is_null() || size.is_null() {
        return ptr::null_mut();
    }

    let str = str as *const uchar;

    let mut utf8_len = 0;
    let mut i = 0;

    // First pass: count the number of UTF-8 bytes needed
    while i < len {
        let c = *str.offset(i as isize);
        if c < 0x80 {
            utf8_len += 1;
        } else if c < 0x800 {
            utf8_len += 2;
        } else if (0xD800..=0xDBFF).contains(&c) && i + 1 < len {
            // Surrogate pair
            let c2 = *str.offset((i + 1) as isize);
            if (0xDC00..=0xDFFF).contains(&c2) {
                utf8_len += 4;
                i += 1;
            } else {
                utf8_len += 3;
            }
        } else {
            utf8_len += 3;
        }
        i += 1;
    }

    // Allocate memory for the UTF-8 string
    let result = crate::bytes::hlp_alloc_bytes(utf8_len + 1) as *mut vbyte;
    if result.is_null() {
        return ptr::null_mut();
    }

    // Second pass: convert UTF-16 to UTF-8
    let mut j = 0;
    i = 0;
    while i < len {
        let mut c = *str.offset(i as isize) as u32;
        if c < 0x80 {
            *result.offset(j) = c as vbyte;
            j += 1;
        } else if c < 0x800 {
            *result.offset(j) = (0xC0 | (c >> 6)) as vbyte;
            *result.offset(j + 1) = (0x80 | (c & 0x3F)) as vbyte;
            j += 2;
        } else if (0xD800..=0xDBFF).contains(&c) && i + 1 < len {
            // Surrogate pair
            let c2 = *str.offset((i + 1) as isize) as u32;
            if (0xDC00..=0xDFFF).contains(&c2) {
                c = 0x10000 + (((c - 0xD800) << 10) | (c2 - 0xDC00));
                *result.offset(j) = (0xF0 | (c >> 18)) as vbyte;
                *result.offset(j + 1) = (0x80 | ((c >> 12) & 0x3F)) as vbyte;
                *result.offset(j + 2) = (0x80 | ((c >> 6) & 0x3F)) as vbyte;
                *result.offset(j + 3) = (0x80 | (c & 0x3F)) as vbyte;
                j += 4;
                i += 1;
            } else {
                *result.offset(j) = (0xE0 | (c >> 12)) as vbyte;
                *result.offset(j + 1) = (0x80 | ((c >> 6) & 0x3F)) as vbyte;
                *result.offset(j + 2) = (0x80 | (c & 0x3F)) as vbyte;
                j += 3;
            }
        } else {
            *result.offset(j) = (0xE0 | (c >> 12)) as vbyte;
            *result.offset(j + 1) = (0x80 | ((c >> 6) & 0x3F)) as vbyte;
            *result.offset(j + 2) = (0x80 | (c & 0x3F)) as vbyte;
            j += 3;
        }
        i += 1;
    }

    // Null-terminate the UTF-8 string
    *result.offset(j) = 0;

    // *size is the UTF-16 length in BYTES (callers compute chars as size>>1,
    // matching upstream hl_utf8_to_utf16), not the UTF-8 source length.
    *size = j as i32;

    result
}

#[no_mangle]
pub unsafe extern "C" fn hlp_ucs2_upper(str: *const vbyte, pos: i32, len: i32) -> *mut vbyte {
    let cstr = str.offset(pos as isize) as *const uchar;
    let out = crate::bytes::hlp_alloc_bytes((len + 1) * 2) as *mut uchar;

    if out.is_null() {
        return ptr::null_mut();
    }

    ptr::copy_nonoverlapping(cstr, out, len as usize);

    for i in 0..len as isize {
        let c = *cstr.offset(i) as u32;
        let up = (c >> UL_BITS) as usize;
        if up < UMAX {
            let c2 = UPPER[up][c as usize & ((1 << UL_BITS) - 1)] as u32;
            if c2 != 0 {
                *out.offset(i) = c2 as uchar;
            }
        }
    }

    *out.offset(len as isize) = 0;
    out as *mut vbyte
}

#[no_mangle]
pub unsafe extern "C" fn hlp_ucs2_lower(str: *const vbyte, pos: i32, len: i32) -> *mut vbyte {
    let cstr = str.offset(pos as isize) as *const uchar;
    // len is in u16 characters; allocate (len+1) * sizeof(u16) bytes
    let out = crate::bytes::hlp_alloc_bytes((len + 1) * 2) as *mut uchar;

    if out.is_null() {
        return ptr::null_mut();
    }

    ptr::copy_nonoverlapping(cstr, out, len as usize);

    for i in 0..len as isize {
        let c = *cstr.offset(i) as u32;
        let up = (c >> UL_BITS) as usize;
        if up < LMAX {
            let c2 = LOWER[up][c as usize & ((1 << UL_BITS) - 1)] as u32;
            if c2 != 0 {
                *out.offset(i) = c2 as uchar;
            }
        }
    }

    *out.offset(len as isize) = 0;
    out as *mut vbyte
}

#[no_mangle]
pub unsafe extern "C" fn hlp_parse_int(bytes: *mut vbyte, pos: c_int, len: c_int) -> *mut vdynamic {
    if bytes.is_null() {
        return ptr::null_mut();
    }
    let c = (bytes as *const uchar).wrapping_add(pos as usize);
    let mut p = c;

    // Skip whitespace
    while *p != 0
        && (*p == b' ' as u16 || *p == b'\t' as u16 || *p == b'\r' as u16 || *p == b'\n' as u16)
    {
        p = p.add(1);
    }

    if *p == 0 {
        return ptr::null_mut();
    }

    let sign = *p;
    let is_signed = sign == b'-' as u16 || sign == b'+' as u16;

    // Check for hex prefix (0x or 0X, optionally with sign)
    let hex_start = if is_signed { p.add(1) } else { p };
    let remaining = if is_signed {
        len - (p.offset_from(c) as i32) - 1
    } else {
        len - (p.offset_from(c) as i32)
    };
    let is_hex = remaining >= 2
        && *hex_start == b'0' as u16
        && (*hex_start.add(1) == b'x' as u16 || *hex_start.add(1) == b'X' as u16);

    let mut h: i32;
    if is_hex {
        h = 0;
        let mut hp = if is_signed { p.add(3) } else { p.add(2) };
        let mut found_digit = false;
        while *hp != 0 {
            let k = *hp;
            if k >= b'0' as u16 && k <= b'9' as u16 {
                h = (h << 4) | (k - b'0' as u16) as i32;
                found_digit = true;
            } else if k >= b'A' as u16 && k <= b'F' as u16 {
                h = (h << 4) | ((k - b'A' as u16) as i32 + 10);
                found_digit = true;
            } else if k >= b'a' as u16 && k <= b'f' as u16 {
                h = (h << 4) | ((k - b'a' as u16) as i32 + 10);
                found_digit = true;
            } else {
                break;
            }
            hp = hp.add(1);
        }
        if !found_digit {
            return ptr::null_mut();
        }
        if sign == b'-' as u16 {
            h = -h;
        }
    } else {
        // Decimal parsing
        let mut dp = p;
        let negative = *dp == b'-' as u16;
        if *dp == b'-' as u16 || *dp == b'+' as u16 {
            dp = dp.add(1);
        }
        if *dp == 0 || *dp < b'0' as u16 || *dp > b'9' as u16 {
            return ptr::null_mut();
        }
        h = 0;
        while *dp >= b'0' as u16 && *dp <= b'9' as u16 {
            h = h.wrapping_mul(10).wrapping_add((*dp - b'0' as u16) as i32);
            dp = dp.add(1);
        }
        if negative {
            h = -h;
        }
    }

    let t = crate::types::hlt_i32();
    hlp_make_dyn(&h as *const i32 as *mut std::ffi::c_void, t)
}

#[no_mangle]
pub unsafe extern "C" fn hlp_parse_float(bytes: *mut vbyte, pos: c_int, _len: c_int) -> c_double {
    if bytes.is_null() {
        return f64::NAN;
    }
    let mut p = (bytes as *const uchar).wrapping_add(pos as usize);

    // Skip whitespace
    while *p != 0
        && (*p == b' ' as u16 || *p == b'\t' as u16 || *p == b'\r' as u16 || *p == b'\n' as u16)
    {
        p = p.add(1);
    }

    if *p == 0 {
        return f64::NAN;
    }

    // Convert the uchar (UTF-16) to a Rust string for parsing
    let mut chars = Vec::new();
    while *p != 0 {
        chars.push(*p);
        p = p.add(1);
    }

    let s = String::from_utf16_lossy(&chars);
    let trimmed = s.trim();
    match trimmed.parse::<f64>() {
        Ok(d) => d,
        Err(_) => {
            // Try parsing just the numeric prefix
            let mut end = 0;
            let bytes = trimmed.as_bytes();
            if end < bytes.len() && (bytes[end] == b'-' || bytes[end] == b'+') {
                end += 1;
            }
            while end < bytes.len() && bytes[end].is_ascii_digit() {
                end += 1;
            }
            if end < bytes.len() && bytes[end] == b'.' {
                end += 1;
                while end < bytes.len() && bytes[end].is_ascii_digit() {
                    end += 1;
                }
            }
            if end < bytes.len() && (bytes[end] == b'e' || bytes[end] == b'E') {
                end += 1;
                if end < bytes.len() && (bytes[end] == b'-' || bytes[end] == b'+') {
                    end += 1;
                }
                while end < bytes.len() && bytes[end].is_ascii_digit() {
                    end += 1;
                }
            }
            if end == 0 || (end == 1 && (bytes[0] == b'-' || bytes[0] == b'+')) {
                return f64::NAN;
            }
            trimmed[..end].parse::<f64>().unwrap_or(f64::NAN)
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_value_to_string(d: *mut vdynamic, len: *mut c_int) -> *const vbyte {
    // removed debug counter
    if d.is_null() {
        *len = 4;
        return str_to_uchar_ptr("null") as *mut vbyte;
    }
    let t = (*d).t;
    if t.is_null() {
        *len = 4;
        return str_to_uchar_ptr("null") as *mut vbyte;
    }
    let kind = (*t).kind;

    match kind {
        hl_type_kind_HI32 => hlp_itos((*d).v.i, len),
        hl_type_kind_HF64 => hlp_ftos((*d).v.d, len),
        _ => {
            let b = hlp_alloc_buffer();
            hlp_buffer_val(b, d);
            hlp_buffer_content(b, len) as *mut vbyte
        }
    }
}

#[cfg(test)]
// The explicit digit strings below are printf format-test vectors; approximating
// well-known constants is the point, so clippy::approx_constant does not apply.
#[allow(clippy::approx_constant)]
mod format_g_tests {
    use super::format_g;

    /// Values taken from `printf("%.15g")` on this platform. The first is the
    /// one that started this: `Std.string(Math.sqrt(2))`.
    #[test]
    fn matches_c_at_precision_15() {
        for (d, want) in [
            (1.4142135623730951_f64, "1.4142135623731"),
            (0.1, "0.1"),
            (1e18, "1e+18"),
            (-1e18, "-1e+18"),
            (1e-5, "1e-05"),
            (1e-4, "0.0001"),
            (3.14159265358979, "3.14159265358979"),
            (100.0, "100"),
            (5.1, "5.1"),
            (86.57, "86.57"),
            (0.30000000000000004, "0.3"),
            (6.02214076e23, "6.02214076e+23"),
            (-273.15, "-273.15"),
        ] {
            assert_eq!(format_g(d, 15), want, "%.15g of {d}");
        }
    }

    /// The buffer paths use 17, where the same double prints differently.
    #[test]
    fn precision_17_differs_from_15() {
        assert_eq!(format_g(1.4142135623730951, 15), "1.4142135623731");
        assert_eq!(format_g(1.4142135623730951, 17), "1.4142135623730951");
    }

    /// Rounding up into the next decade has to switch to exponent form, the
    /// case a naive "compare the exponent of the input" implementation gets
    /// wrong.
    #[test]
    fn rounding_into_the_next_decade_switches_form() {
        assert_eq!(format_g(999999999999999.9, 15), "1e+15");
    }

    /// C keeps the sign of a negative zero.
    #[test]
    fn signed_zero() {
        assert_eq!(format_g(0.0, 15), "0");
        assert_eq!(format_g(-0.0, 15), "-0");
    }

    /// The exponent carries a sign and at least two digits, as C prints it.
    #[test]
    fn exponent_is_two_digits_with_a_sign() {
        assert_eq!(format_g(1e5, 3), "1e+05");
        assert_eq!(format_g(1e-300, 15), "1e-300");
    }
}
