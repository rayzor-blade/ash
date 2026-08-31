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

// DEFINE_PRIM(_I32, regexp_matched_num, _EREG)
#[no_mangle]
pub unsafe extern "C" fn hlp_regexp_matched_num(r: *mut c_void) -> i32 {
    if r.is_null() {
        return -1;
    }
    let state = &*(r as *mut RegexpState);
    // -1 is upstream's "no match on this regexp yet", and Haxe's EReg.matched
    // relies on it to tell that apart from a pattern with no groups. A
    // successful match stored one row per group including group 0, which is
    // the count pcre2 reports as `n_groups`.
    match &state.last_groups {
        Some(groups) => groups.len() as i32,
        None => -1,
    }
}

#[cfg(test)]
mod regexp_matched_num_tests {
    use super::*;

    /// Subjects and patterns cross this boundary as NUL-terminated UTF-16,
    /// which is what `read_utf16z` on the other side expects.
    fn u16z(s: &str) -> Vec<u16> {
        let mut v: Vec<u16> = s.encode_utf16().collect();
        v.push(0);
        v
    }

    unsafe fn new_regexp(pattern: &str) -> *mut c_void {
        let p = u16z(pattern);
        let o = u16z("");
        let r = hlp_regexp_new_options(p.as_ptr() as *const vbyte, o.as_ptr() as *const vbyte);
        assert!(!r.is_null(), "failed to build /{pattern}/");
        r
    }

    unsafe fn run_match(r: *mut c_void, subject: &str) -> i32 {
        let s = u16z(subject);
        hlp_regexp_match(r, s.as_ptr() as *const vbyte, 0, -1)
    }

    #[test]
    fn a_null_regexp_reports_no_match() {
        unsafe {
            assert_eq!(hlp_regexp_matched_num(std::ptr::null_mut()), -1);
        }
    }

    /// -1 is upstream's "no match on this regexp yet", and Haxe's EReg
    /// relies on it to tell that state apart from a pattern that matched but
    /// has no groups -- which answers 1, not 0.
    #[test]
    fn before_any_match_it_is_minus_one() {
        unsafe {
            let r = new_regexp("a(b)c");
            assert_eq!(hlp_regexp_matched_num(r), -1);
        }
    }

    /// One row per group including group 0, so a groupless pattern that
    /// matched answers 1. This is the value that must not collide with the
    /// unmatched state.
    #[test]
    fn a_match_counts_group_zero_and_every_group() {
        unsafe {
            let r = new_regexp("abc");
            assert_eq!(run_match(r, "xxabcxx"), 1);
            assert_eq!(hlp_regexp_matched_num(r), 1, "group 0 alone");

            let r = new_regexp("(a)(b)(c)");
            assert_eq!(run_match(r, "abc"), 1);
            assert_eq!(hlp_regexp_matched_num(r), 4, "group 0 plus three");

            // A group present in the pattern but not in the match still
            // occupies a row: the count is the pattern's, not the match's.
            let r = new_regexp("(a)|(b)");
            assert_eq!(run_match(r, "a"), 1);
            assert_eq!(hlp_regexp_matched_num(r), 3);
            assert_eq!(hlp_regexp_matched_pos(r, 2, std::ptr::null_mut()), -1);
        }
    }

    /// A failed match puts the regexp back into the unmatched state rather
    /// than leaving the previous match's count standing -- otherwise EReg
    /// would read groups out of a match that did not happen.
    #[test]
    fn a_failed_match_returns_to_minus_one() {
        unsafe {
            let r = new_regexp("(a)(b)");
            assert_eq!(hlp_regexp_matched_num(r), -1, "before");

            assert_eq!(run_match(r, "ab"), 1);
            assert_eq!(hlp_regexp_matched_num(r), 3, "after a match");

            assert_eq!(run_match(r, "zz"), 0);
            assert_eq!(
                hlp_regexp_matched_num(r),
                -1,
                "a failed match left the previous count in place"
            );

            // And it recovers on the next success.
            assert_eq!(run_match(r, "qqab"), 1);
            assert_eq!(hlp_regexp_matched_num(r), 3);
        }
    }

    /// The count is per regexp, not per process: two live regexps keep their
    /// own state.
    #[test]
    fn the_count_belongs_to_its_own_regexp() {
        unsafe {
            let a = new_regexp("(x)(y)(z)");
            let b = new_regexp("q");
            assert_eq!(run_match(a, "xyz"), 1);
            assert_eq!(hlp_regexp_matched_num(a), 4);
            assert_eq!(hlp_regexp_matched_num(b), -1);
            assert_eq!(run_match(b, "q"), 1);
            assert_eq!(hlp_regexp_matched_num(b), 1);
            assert_eq!(hlp_regexp_matched_num(a), 4);
        }
    }

    /// DEFINE_PRIM(_I32, regexp_matched_num, _EREG).
    #[test]
    fn the_exported_signature_is_the_one_upstream_declares() {
        let f: unsafe extern "C" fn(*mut c_void) -> i32 = hlp_regexp_matched_num;
        unsafe {
            assert_eq!(f(std::ptr::null_mut()), -1);
        }
    }
}
