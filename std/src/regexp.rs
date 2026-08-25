use std::ffi::c_void;

use fancy_regex::{Regex, RegexBuilder};

use crate::{error::hlp_error, hl::vbyte, strings::str_to_uchar_ptr};

struct RegexpState {
    regex: Regex,
    last_groups: Option<Vec<Option<(i32, i32)>>>,
}

unsafe fn read_utf16z(bytes: *const vbyte) -> Vec<u16> {
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

fn utf8_byte_to_utf16_units(s: &str, byte_idx: usize) -> i32 {
    let mut units = 0i32;
    for (idx, ch) in s.char_indices() {
        if idx >= byte_idx {
            break;
        }
        units += ch.len_utf16() as i32;
    }
    units
}

fn utf16_units_to_utf8_byte(s: &str, unit_idx: usize) -> usize {
    let mut units = 0usize;
    for (byte, ch) in s.char_indices() {
        if units >= unit_idx {
            return byte;
        }
        units += ch.len_utf16();
    }
    s.len()
}

fn build_regex(pattern: &str, options: &str) -> Option<Regex> {
    let mut builder = RegexBuilder::new(pattern);
    for ch in options.chars() {
        match ch {
            'i' => {
                builder.case_insensitive(true);
            }
            'm' => {
                builder.multi_line(true);
            }
            's' => {
                builder.dot_matches_new_line(true);
            }
            'u' => {
                builder.unicode_mode(true);
            }
            _ => {}
        }
    }
    builder.build().ok()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_regexp_new_options(
    bytes: *const vbyte,
    options: *const vbyte,
) -> *mut c_void {
    let pattern = String::from_utf16_lossy(&read_utf16z(bytes));
    let opts = String::from_utf16_lossy(&read_utf16z(options));
    let Some(regex) = build_regex(&pattern, &opts) else {
        return std::ptr::null_mut();
    };
    Box::into_raw(Box::new(RegexpState {
        regex,
        last_groups: None,
    })) as *mut c_void
}

#[no_mangle]
pub unsafe extern "C" fn hlp_regexp_match(
    r: *mut c_void,
    str_bytes: *const vbyte,
    pos: i32,
    size: i32,
) -> i32 {
    if r.is_null() || str_bytes.is_null() {
        return 0;
    }
    let state = &mut *(r as *mut RegexpState);
    let full_units = read_utf16z(str_bytes);
    let total_len = full_units.len() as i32;
    let start = pos.clamp(0, total_len) as usize;
    let avail = total_len - start as i32;
    let run_len = if size < 0 {
        avail
    } else {
        size.min(avail).max(0)
    } as usize;
    let subject = String::from_utf16_lossy(&full_units);
    let start_byte = utf16_units_to_utf8_byte(&subject, start);
    let end_byte = utf16_units_to_utf8_byte(&subject, start + run_len);
    let visible_subject = &subject[..end_byte];

    // Search the original subject at an offset instead of slicing it at
    // `pos`.  Anchors are relative to the subject in PCRE2: slicing made `^`
    // spuriously match after every zero-width global match, because each new
    // offset appeared to be the start of a fresh string.
    if let Ok(Some(caps)) = state
        .regex
        .captures_from_pos(visible_subject, start_byte)
    {
        let mut groups = Vec::with_capacity(caps.len());
        for i in 0..caps.len() {
            if let Some(m) = caps.get(i) {
                let s = utf8_byte_to_utf16_units(&subject, m.start());
                let e = utf8_byte_to_utf16_units(&subject, m.end());
                groups.push(Some((s, e - s)));
            } else {
                groups.push(None);
            }
        }
        state.last_groups = Some(groups);
        1
    } else {
        state.last_groups = None;
        0
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_regexp_matched_pos(r: *mut c_void, n: i32, size: *mut i32) -> i32 {
    if r.is_null() || n < 0 {
        if !size.is_null() {
            *size = 0;
        }
        return -1;
    }
    let state = &mut *(r as *mut RegexpState);
    let Some(groups) = &state.last_groups else {
        if !size.is_null() {
            *size = 0;
        }
        hlp_error(str_to_uchar_ptr(
            "Calling regexp_matched_pos() on an unmatched regexp",
        ));
        return -1;
    };
    let Some(group) = groups.get(n as usize) else {
        if !size.is_null() {
            *size = 0;
        }
        hlp_error(str_to_uchar_ptr(&format!(
            "Matched index {n} outside bounds"
        )));
        return -1;
    };
    let Some((pos, len)) = group else {
        if !size.is_null() {
            *size = 0;
        }
        return -1;
    };
    if !size.is_null() {
        *size = *len;
    }
    *pos
}
