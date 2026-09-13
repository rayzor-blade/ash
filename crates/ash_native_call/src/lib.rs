//! Calling a native whose signature is only known at run time.
//!
//! A native is called by transmuting its address to a function pointer, which
//! means every signature has to exist somewhere in the source. The
//! interpreter used to carry a hand-written table and gained an arm whenever a
//! library used a combination nobody had used before; three were added in one
//! afternoon once a graphics library started passing colours and viewports
//! around, and each gap was a clean error rather than a wrong answer, but a
//! wall all the same.
//!
//! `build.rs` writes them all instead. It is its own crate for two reasons:
//! the table is large enough that recompiling it on every interpreter edit
//! would be felt, and it is built unoptimised, which the profile in the
//! workspace manifest arranges.

include!(concat!(env!("OUT_DIR"), "/dispatch.rs"));

#[cfg(test)]
mod tests {
    #[test]
    fn the_table_covers_what_it_claims() {
        // Uniform-float signatures at every arity, plus mixed ones up to five
        // arguments; times three ways to return.
        assert_eq!(super::SIGNATURES, 3771);
    }

    #[test]
    fn the_word_form_places_each_kind() {
        unsafe extern "C" fn mix(a: i64, b: f64, c: f32) -> f64 {
            a as f64 + b + c as f64
        }
        let kinds = [0u8, 2, 1];
        let pattern = super::pattern_of(&kinds);
        let words = [3u64, 1.5f64.to_bits(), 0.25f32.to_bits() as u64];
        let out = unsafe { super::dispatch_by_pattern(mix as *mut _, &words, 2, pattern) };
        assert_eq!(out.map(|bits| f64::from_bits(bits as u64)), Some(4.75));
        let ints = [3i64, 0, 0];
        let f32s = [0f32, 0.0, 0.25];
        let f64s = [0f64, 1.5, 0.0];
        let same = unsafe { super::dispatch(mix as *mut _, &ints, &f32s, &f64s, &kinds, 2) };
        assert_eq!(same, out);
    }

    #[test]
    fn a_signature_that_was_not_generated_is_refused() {
        // Nine arguments is past the end of the table.
        let kinds = [0u8; 9];
        let out = unsafe {
            super::dispatch(
                std::ptr::null_mut(),
                &[0i64; 9],
                &[0f32; 9],
                &[0f64; 9],
                &kinds,
                0,
            )
        };
        assert!(out.is_none());
    }
}
