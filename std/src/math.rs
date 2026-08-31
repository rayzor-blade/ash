use std::os::raw::c_double;

#[no_mangle]
pub unsafe extern "C" fn hlp_math_abs(x: c_double) -> c_double {
    x.abs()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_math_sqrt(x: c_double) -> c_double {
    x.sqrt()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_math_floor(x: c_double) -> i32 {
    // Upstream: DEFINE_PRIM(_I32, math_floor, _F64)
    x.floor() as i32
}

#[no_mangle]
pub unsafe extern "C" fn hlp_math_ceil(x: c_double) -> i32 {
    x.ceil() as i32
}

#[no_mangle]
pub unsafe extern "C" fn hlp_math_round(x: c_double) -> i32 {
    // HashLink round = floor(x + 0.5) (half-away differs from Rust .round()
    // for negative halves).
    (x + 0.5).floor() as i32
}

#[no_mangle]
pub unsafe extern "C" fn hlp_math_sin(x: c_double) -> c_double {
    x.sin()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_math_cos(x: c_double) -> c_double {
    x.cos()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_math_tan(x: c_double) -> c_double {
    x.tan()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_math_asin(x: c_double) -> c_double {
    x.asin()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_math_acos(x: c_double) -> c_double {
    x.acos()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_math_atan(x: c_double) -> c_double {
    x.atan()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_math_atan2(y: c_double, x: c_double) -> c_double {
    y.atan2(x)
}

#[no_mangle]
pub unsafe extern "C" fn hlp_math_exp(x: c_double) -> c_double {
    x.exp()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_math_log(x: c_double) -> c_double {
    x.ln()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_math_pow(x: c_double, y: c_double) -> c_double {
    x.powf(y)
}

#[no_mangle]
pub unsafe extern "C" fn hlp_math_isnan(x: c_double) -> bool {
    x.is_nan()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_math_isfinite(x: c_double) -> bool {
    x.is_finite()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_math_ffloor(x: c_double) -> c_double {
    // Upstream: DEFINE_PRIM(_F64, math_ffloor, _F64)
    x.floor()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_math_fceil(x: c_double) -> c_double {
    x.ceil()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_math_fround(x: c_double) -> c_double {
    (x + 0.5).floor()
}

/// Upstream: DEFINE_PRIM(_F64, nan, _NO_ARG) — where `Math.NaN` comes from.
///
/// `f64::NAN` is the platform's quiet NaN, the same value upstream's `NAN`
/// macro expands to; the bit pattern spelled out there is a fallback for
/// toolchains that lack the macro, not a different number.
#[no_mangle]
pub unsafe extern "C" fn hlp_nan() -> c_double {
    f64::NAN
}

#[cfg(test)]
mod nan_tests {
    use super::*;

    /// NaN is the one value that is not equal to itself, so `== f64::NAN`
    /// would pass whatever this returned. `is_nan()` is the only check that
    /// means anything here.
    #[test]
    fn nan_returns_a_nan() {
        let x = unsafe { hlp_nan() };
        assert!(x.is_nan(), "hlp_nan returned {x}");
        #[allow(clippy::eq_op)]
        {
            assert!(x != x, "a NaN compares unequal to itself");
        }
        // Unordered against everything, which is the property that makes
        // every comparison Haxe writes against Math.NaN come out false.
        assert!(x.partial_cmp(&0.0).is_none());
        assert!(x.partial_cmp(&f64::INFINITY).is_none());
    }

    /// Quiet, not signalling: the comment claims the platform's quiet NaN,
    /// and a signalling one would trap in arithmetic a Haxe program does
    /// freely. Bit 51 (the top mantissa bit) set is what makes it quiet on
    /// every IEEE-754 binary64 target ash builds for.
    #[test]
    fn the_nan_is_quiet_and_matches_the_platforms_own() {
        let x = unsafe { hlp_nan() };
        let bits = x.to_bits();
        assert_eq!(
            bits & 0x7ff0_0000_0000_0000,
            0x7ff0_0000_0000_0000,
            "exponent is not all ones"
        );
        assert_ne!(bits & 0x000f_ffff_ffff_ffff, 0, "that is an infinity");
        assert_ne!(bits & 0x0008_0000_0000_0000, 0, "signalling NaN");
        // The same number the C `NAN` macro expands to on this platform.
        assert_eq!(bits, f64::NAN.to_bits());
    }

    /// A NaN has to stay a NaN through the arithmetic Math.NaN feeds, or
    /// `Math.isNaN` downstream reads a number.
    #[test]
    fn nan_propagates_through_arithmetic() {
        let x = unsafe { hlp_nan() };
        assert!((x + 1.0).is_nan());
        assert!((x * 0.0).is_nan());
        assert!(unsafe { hlp_math_abs(x) }.is_nan());
        assert!(unsafe { hlp_math_sqrt(x) }.is_nan());
    }

    /// DEFINE_PRIM(_F64, nan, _NO_ARG): no arguments, returns a double.
    #[test]
    fn the_exported_signature_is_the_one_upstream_declares() {
        let f: unsafe extern "C" fn() -> c_double = hlp_nan;
        assert!(unsafe { f() }.is_nan());
    }
}
