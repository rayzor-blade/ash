//! # Native primitives that are machine instructions
//!
//! A handful of the `std` natives are single instructions on every target ash
//! runs on. Calling out to `ash_std` for them costs a call, a dylib boundary,
//! and — worse than either — an optimization barrier: neither backend can hoist
//! a loop-invariant `Math.sqrt` or fold it against its neighbours across an
//! opaque call. This table is what lets both backends emit the instruction
//! instead.
//!
//! Only primitives with an *exact* instruction equivalent appear here. The
//! transcendentals (`sin`, `cos`, `exp`, `log`, `pow`, …) are deliberately
//! absent: they bottom out in libm either way, so replacing the call with an
//! intrinsic that lowers back to the same libm call buys nothing while putting
//! the result at the mercy of whichever implementation the backend picks.
//!
//! ## Matching `ash_std` exactly
//!
//! Two details decide correctness, and both are easy to get wrong:
//!
//! * **`Math.round` is not IEEE rounding.** HashLink defines it as
//!   `floor(x + 0.5)`, which differs from round-half-away-from-zero at negative
//!   halves — `round(-2.5)` is `-2`, not `-3`. It is therefore expressed as a
//!   composite here, never as a `round` instruction.
//! * **The float→int casts saturate.** `ash_std` writes `x.floor() as i32`, and
//!   Rust's `as` saturates: NaN becomes 0 and out-of-range values clamp to
//!   `i32::MIN`/`i32::MAX`. A plain `fptosi` is *poison* on those inputs, so the
//!   saturating forms (`llvm.fptosi.sat`, Cranelift's `fcvt_to_sint_sat`) are
//!   the only correct lowering.

/// What to emit in place of a call to a native primitive.
///
/// Each variant names the operation, not the instruction: the backends pick
/// their own encoding, and the `ToI32` shapes carry an implied saturating
/// float→int conversion of the result.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum NativeIntrinsic {
    /// `sqrt(x)` — f64 → f64.
    Sqrt,
    /// `|x|` — f64 → f64.
    Abs,
    /// `floor(x)` — f64 → f64.
    Floor,
    /// `ceil(x)` — f64 → f64.
    Ceil,
    /// `floor(x + 0.5)` — f64 → f64. HashLink's rounding, not IEEE's.
    RoundHalfUp,
    /// `floor(x)` saturating to i32.
    FloorToI32,
    /// `ceil(x)` saturating to i32.
    CeilToI32,
    /// `floor(x + 0.5)` saturating to i32.
    RoundHalfUpToI32,
    /// `x != x` — f64 → bool.
    IsNaN,
    /// `|x| != inf && x == x` — f64 → bool.
    IsFinite,
}

impl NativeIntrinsic {
    /// Whether the result is an HL `Int` rather than an `F64` or `Bool`.
    pub fn returns_i32(self) -> bool {
        matches!(
            self,
            NativeIntrinsic::FloorToI32
                | NativeIntrinsic::CeilToI32
                | NativeIntrinsic::RoundHalfUpToI32
        )
    }

    /// Whether the result is an HL `Bool`.
    pub fn returns_bool(self) -> bool {
        matches!(self, NativeIntrinsic::IsNaN | NativeIntrinsic::IsFinite)
    }
}

/// The intrinsic that replaces `lib@name`, if there is an exact one.
///
/// Every entry takes a single `f64`; callers rely on that to know a one-argument
/// call site is the only shape they have to intercept.
pub fn lookup(lib: &str, name: &str) -> Option<NativeIntrinsic> {
    if lib != "std" {
        return None;
    }
    Some(match name {
        "math_sqrt" => NativeIntrinsic::Sqrt,
        "math_abs" => NativeIntrinsic::Abs,
        "math_ffloor" => NativeIntrinsic::Floor,
        "math_fceil" => NativeIntrinsic::Ceil,
        "math_fround" => NativeIntrinsic::RoundHalfUp,
        "math_floor" => NativeIntrinsic::FloorToI32,
        "math_ceil" => NativeIntrinsic::CeilToI32,
        "math_round" => NativeIntrinsic::RoundHalfUpToI32,
        "math_isnan" => NativeIntrinsic::IsNaN,
        "math_isfinite" => NativeIntrinsic::IsFinite,
        _ => return None,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn only_std_natives_map() {
        assert_eq!(lookup("std", "math_sqrt"), Some(NativeIntrinsic::Sqrt));
        // A same-named primitive from an HDLL is a different function.
        assert_eq!(lookup("mylib", "math_sqrt"), None);
    }

    #[test]
    fn transcendentals_are_left_as_calls() {
        for name in ["math_sin", "math_cos", "math_pow", "math_exp", "math_log"] {
            assert_eq!(lookup("std", name), None, "{name} should stay a call");
        }
    }

    #[test]
    fn int_returning_and_float_returning_forms_are_distinct() {
        // HashLink has both: Math.floor -> Int, and math_ffloor -> Float.
        assert!(lookup("std", "math_floor").unwrap().returns_i32());
        assert!(!lookup("std", "math_ffloor").unwrap().returns_i32());
        assert!(lookup("std", "math_isnan").unwrap().returns_bool());
    }
}
