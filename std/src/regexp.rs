use std::ffi::c_void;

use regex::{Regex, RegexBuilder};

use crate::hl::vbyte;

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
                builder.unicode(true);
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
    let segment_units = &full_units[start..start + run_len];
    let segment = String::from_utf16_lossy(segment_units);

    if let Some(caps) = state.regex.captures(&segment) {
        let base = start as i32;
        let mut groups = Vec::with_capacity(caps.len());
        for i in 0..caps.len() {
            if let Some(m) = caps.get(i) {
                let s = utf8_byte_to_utf16_units(&segment, m.start());
                let e = utf8_byte_to_utf16_units(&segment, m.end());
                groups.push(Some((base + s, e - s)));
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
        return -1;
    };
    let Some(group) = groups.get(n as usize) else {
        if !size.is_null() {
            *size = 0;
        }
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
