use std::ffi::c_void;

use fancy_regex::{Regex, RegexBuilder};

use crate::{error::hlp_error, hl::vbyte, strings::str_to_uchar_ptr};

/// The `EReg` handle itself, in GC memory.
///
/// A compiled pattern is expensive and a program builds them in loops, so
/// leaving them to be freed at exit is not an option. Upstream's `ereg` is a
/// `hl_gc_alloc_finalizer` block for the same reason.
#[repr(C)]
struct RegexpState {
    /// Word zero, where the collector looks. Upstream names it the same.
    finalize: Option<crate::rt::Finalizer>,
    regex: Regex,
    last_groups: Option<Vec<Option<(i32, i32)>>>,
    /// The last subject, converted, so EReg's replace, split and map, which
    /// match one string at advancing positions, convert it once.
    subject: Option<Subject>,
}

/// A UTF-16 subject as fancy_regex needs it: UTF-8, with a table from each
/// UTF-16 unit index to its UTF-8 byte offset.
///
/// Recognised by its address and then by its contents, compared in one
/// pass: an address alone is not the same string once the buffer is freed
/// and reused. That compare is the only per-call cost the cache leaves.
struct Subject {
    source: *const u16,
    units: Vec<u16>,
    text: String,
    /// `units.len() + 1` entries. A unit inside a surrogate pair maps to the
    /// byte after the pair.
    to_byte: Vec<u32>,
}

impl Subject {
    fn new(source: *const u16, units: &[u16]) -> Self {
        let mut text = String::with_capacity(units.len());
        let mut to_byte = Vec::with_capacity(units.len() + 1);
        let mut i = 0;
        for decoded in char::decode_utf16(units.iter().copied()) {
            let (ch, width) = match decoded {
                Ok(ch) => (ch, ch.len_utf16()),
                Err(_) => (char::REPLACEMENT_CHARACTER, 1),
            };
            to_byte.push(text.len() as u32);
            text.push(ch);
            for _ in 1..width {
                to_byte.push(text.len() as u32);
            }
            i += width;
        }
        debug_assert_eq!(i, units.len());
        to_byte.push(text.len() as u32);
        Subject {
            source,
            units: units.to_vec(),
            text,
            to_byte,
        }
    }

    fn byte_of(&self, unit: usize) -> usize {
        self.to_byte[unit.min(self.units.len())] as usize
    }

    /// The UTF-16 units before `byte`, a char boundary.
    fn unit_of(&self, byte: usize) -> i32 {
        (self.to_byte.partition_point(|&b| b as usize <= byte) - 1) as i32
    }

    /// Whether the NUL-terminated string at `source` is this one: the same
    /// address, the same units, and its terminator where this one ends.
    unsafe fn is(&self, source: *const u16) -> bool {
        unsafe {
            source == self.source
                && std::slice::from_raw_parts(source, self.units.len()) == self.units.as_slice()
                && *source.add(self.units.len()) == 0
        }
    }
}

// The collector reads the callback out of word zero, so `finalize` has to BE
// word zero, and the block has to satisfy the struct's alignment.
const _: () = assert!(std::mem::offset_of!(RegexpState, finalize) == 0);
const _: () = assert!(std::mem::align_of::<RegexpState>() <= 16);

/// Frees the compiled pattern of an `EReg` nothing can reach any more, the way
/// upstream's `regexp_finalize` frees its `pcre16` one.
///
/// Guards on word zero and clears it, which is upstream's idiom for making a
/// free run once however it is reached.
unsafe extern "C" fn regexp_finalize(block: *mut c_void) {
    unsafe {
        let state = block as *mut RegexpState;
        if (*state).finalize.take().is_none() {
            return;
        }
        std::ptr::drop_in_place(std::ptr::addr_of_mut!((*state).regex));
        std::ptr::drop_in_place(std::ptr::addr_of_mut!((*state).last_groups));
        std::ptr::drop_in_place(std::ptr::addr_of_mut!((*state).subject));
    }
}

unsafe fn read_utf16z(bytes: *const vbyte) -> Vec<u16> {
    unsafe {
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
}

/// The units of the NUL-terminated string at `bytes`, without copying them.
unsafe fn utf16z<'a>(bytes: *const vbyte) -> &'a [u16] {
    unsafe {
        let ptr = bytes as *const u16;
        let mut len = 0usize;
        while *ptr.add(len) != 0 {
            len += 1;
        }
        std::slice::from_raw_parts(ptr, len)
    }
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

#[unsafe(no_mangle)]
pub unsafe extern "C" fn hlp_regexp_new_options(
    bytes: *const vbyte,
    options: *const vbyte,
) -> *mut c_void {
    unsafe {
        let pattern = String::from_utf16_lossy(&read_utf16z(bytes));
        let opts = String::from_utf16_lossy(&read_utf16z(options));
        let Some(regex) = build_regex(&pattern, &opts) else {
            return std::ptr::null_mut();
        };
        let state =
            crate::rt::alloc_with_finalizer(std::mem::size_of::<RegexpState>(), regexp_finalize)
                as *mut RegexpState;
        if state.is_null() {
            return std::ptr::null_mut();
        }
        // Raw memory, so write the fields rather than assigning: an assignment
        // would drop whatever the previous occupant's bytes look like.
        std::ptr::addr_of_mut!((*state).regex).write(regex);
        std::ptr::addr_of_mut!((*state).last_groups).write(None);
        std::ptr::addr_of_mut!((*state).subject).write(None);
        state as *mut c_void
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn hlp_regexp_match(
    r: *mut c_void,
    str_bytes: *const vbyte,
    pos: i32,
    size: i32,
) -> i32 {
    unsafe {
        if r.is_null() || str_bytes.is_null() {
            return 0;
        }
        let state = &mut *(r as *mut RegexpState);
        let source = str_bytes as *const u16;
        if !state.subject.as_ref().is_some_and(|s| s.is(source)) {
            state.subject = Some(Subject::new(source, utf16z(str_bytes)));
        }
        let subject = state.subject.as_ref().unwrap();
        let total_len = subject.units.len() as i32;
        let start = pos.clamp(0, total_len) as usize;
        let avail = total_len - start as i32;
        let run_len = if size < 0 {
            avail
        } else {
            size.min(avail).max(0)
        } as usize;
        let start_byte = subject.byte_of(start);
        let end_byte = subject.byte_of(start + run_len);
        let visible_subject = &subject.text[..end_byte];

        // Search the original subject at an offset instead of slicing it at
        // `pos`.  Anchors are relative to the subject in PCRE2: slicing made `^`
        // spuriously match after every zero-width global match, because each new
        // offset appeared to be the start of a fresh string.
        if let Ok(Some(caps)) = state.regex.captures_from_pos(visible_subject, start_byte) {
            let mut groups = Vec::with_capacity(caps.len());
            for i in 0..caps.len() {
                if let Some(m) = caps.get(i) {
                    let s = subject.unit_of(m.start());
                    let e = subject.unit_of(m.end());
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
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn hlp_regexp_matched_pos(r: *mut c_void, n: i32, size: *mut i32) -> i32 {
    unsafe {
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
}

// DEFINE_PRIM(_I32, regexp_matched_num, _EREG)
#[unsafe(no_mangle)]
pub unsafe extern "C" fn hlp_regexp_matched_num(r: *mut c_void) -> i32 {
    unsafe {
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
        unsafe {
            let p = u16z(pattern);
            let o = u16z("");
            let r = hlp_regexp_new_options(p.as_ptr() as *const vbyte, o.as_ptr() as *const vbyte);
            assert!(!r.is_null(), "failed to build /{pattern}/");
            r
        }
    }

    unsafe fn run_match(r: *mut c_void, subject: &str) -> i32 {
        unsafe {
            let s = u16z(subject);
            hlp_regexp_match(r, s.as_ptr() as *const vbyte, 0, -1)
        }
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

    unsafe fn pos_of(r: *mut c_void, n: i32) -> (i32, i32) {
        unsafe {
            let mut len = 0;
            let pos = hlp_regexp_matched_pos(r, n, &mut len);
            (pos, len)
        }
    }

    /// Positions are UTF-16 units: a surrogate pair before the match counts
    /// two, and a match starting after it reports the unit, not the byte.
    #[test]
    fn positions_count_utf16_units_across_a_surrogate_pair() {
        unsafe {
            let r = new_regexp("b+");
            assert_eq!(run_match(r, "a\u{1F600}bb c"), 1);
            assert_eq!(pos_of(r, 0), (3, 2));
            // A start inside the pair lands after it.
            let s = u16z("a\u{1F600}bb");
            assert_eq!(hlp_regexp_match(r, s.as_ptr() as *const vbyte, 2, -1), 1);
            assert_eq!(pos_of(r, 0), (3, 2));
        }
    }

    /// An unpaired surrogate is one unit, as upstream's UTF-16 matcher sees it.
    #[test]
    fn an_unpaired_surrogate_is_one_unit() {
        unsafe {
            let r = new_regexp("x");
            let s = [0x61u16, 0xD800, 0x78, 0];
            assert_eq!(hlp_regexp_match(r, s.as_ptr() as *const vbyte, 0, -1), 1);
            assert_eq!(pos_of(r, 0), (2, 1));
        }
    }

    /// A different string is converted afresh, not matched against the one
    /// cached before it.
    #[test]
    fn another_subject_is_not_matched_against_the_cached_one() {
        unsafe {
            let r = new_regexp("[0-9]+");
            let a = u16z("ab12cd");
            let b = u16z("7xyz");
            assert_eq!(hlp_regexp_match(r, a.as_ptr() as *const vbyte, 0, -1), 1);
            assert_eq!(pos_of(r, 0), (2, 2));
            assert_eq!(hlp_regexp_match(r, b.as_ptr() as *const vbyte, 0, -1), 1);
            assert_eq!(pos_of(r, 0), (0, 1));
            assert_eq!(hlp_regexp_match(r, a.as_ptr() as *const vbyte, 0, -1), 1);
            assert_eq!(pos_of(r, 0), (2, 2));
        }
    }

    /// The loop EReg.replace and split run: one subject, advancing positions.
    #[test]
    fn successive_matches_on_one_subject_advance() {
        unsafe {
            let r = new_regexp("[0-9]+");
            let s = u16z("a1 b22 c333");
            let mut found = Vec::new();
            let mut pos = 0;
            while hlp_regexp_match(r, s.as_ptr() as *const vbyte, pos, -1) == 1 {
                let (p, l) = pos_of(r, 0);
                found.push((p, l));
                pos = p + l;
            }
            assert_eq!(found, vec![(1, 1), (4, 2), (8, 3)]);
        }
    }
}
