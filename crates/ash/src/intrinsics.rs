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

    /// The unary machine primitive AIR classified a call as, or None for a
    /// kind that is not one (`PtrCompare` is a two-operand compare the
    /// backends emit inline).
    pub fn of_kind(kind: air::v2::ir::IntrinsicKind) -> Option<Self> {
        use air::v2::ir::IntrinsicKind as K;
        Some(match kind {
            K::Sqrt => NativeIntrinsic::Sqrt,
            K::Abs => NativeIntrinsic::Abs,
            K::Floor => NativeIntrinsic::Floor,
            K::Ceil => NativeIntrinsic::Ceil,
            K::RoundHalfUp => NativeIntrinsic::RoundHalfUp,
            K::FloorToI32 => NativeIntrinsic::FloorToI32,
            K::CeilToI32 => NativeIntrinsic::CeilToI32,
            K::RoundHalfUpToI32 => NativeIntrinsic::RoundHalfUpToI32,
            K::IsNaN => NativeIntrinsic::IsNaN,
            K::IsFinite => NativeIntrinsic::IsFinite,
            K::PtrCompare => return None,
        })
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

/// The vector primitive behind a `simd@<name>` native, from the name's
/// `<lanes>_<op>` spelling (`f32x4_add`, `v128_select`, `f32x4_to_i32x4`).
///
/// Every name in ash_simd's table resolves here; the backends emit the lane
/// operation in place of the call, and the fixture pins the two against each
/// other.
pub fn lookup_simd(name: &str) -> Option<air::v2::ir::VecIntrinsic> {
    use air::v2::ir::{VecElem, VecIntrinsic, VecOp};
    let (ty, op) = name.split_once('_')?;
    let elem = match ty {
        "f32x4" => VecElem::F32,
        "f64x2" => VecElem::F64,
        "i32x4" => VecElem::I32,
        "i16x8" => VecElem::I16,
        "i8x16" => VecElem::I8,
        "u8x16" => VecElem::U8,
        "v128" => VecElem::I64,
        _ => return None,
    };
    let op = match op {
        "add" => VecOp::Add,
        "sub" => VecOp::Sub,
        "mul" => VecOp::Mul,
        "div" => VecOp::Div,
        "min" => VecOp::Min,
        "max" => VecOp::Max,
        "abs" => VecOp::Abs,
        "neg" => VecOp::Neg,
        "sqrt" => VecOp::Sqrt,
        "fma" => VecOp::Fma,
        "splat" => VecOp::Splat,
        "shl" => VecOp::Shl,
        "shr" => VecOp::Shr,
        "eq" => VecOp::Eq,
        "ne" => VecOp::Ne,
        "lt" => VecOp::Lt,
        "le" => VecOp::Le,
        "gt" => VecOp::Gt,
        "ge" => VecOp::Ge,
        "sum" => VecOp::Sum,
        "min_lane" => VecOp::MinLane,
        "max_lane" => VecOp::MaxLane,
        "load_array" => VecOp::LoadArray,
        "store_array" => VecOp::StoreArray,
        "and" => VecOp::And,
        "or" => VecOp::Or,
        "xor" => VecOp::Xor,
        "not" => VecOp::Not,
        "select" => VecOp::Select,
        "to_i32x4" if elem == VecElem::F32 => VecOp::ToI32,
        "to_f32x4" if elem == VecElem::I32 => VecOp::ToF32,
        "copy" if elem == VecElem::I64 => VecOp::Copy,
        _ => return None,
    };
    Some(VecIntrinsic { elem, op })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_simd_primitive_is_an_intrinsic() {
        let table = include_str!("../../ash_simd/src/prims.rs");
        let mut seen = 0;
        for line in table.lines() {
            let Some(rest) = line.trim().strip_prefix('(') else {
                continue;
            };
            let name = rest.split(',').next().unwrap().trim();
            if !rest.contains("hlp_") {
                continue;
            }
            assert!(lookup_simd(name).is_some(), "{name} has no intrinsic");
            seen += 1;
        }
        assert!(seen > 100, "read {seen} names from the primitive table");
        assert_eq!(lookup_simd("f32x4_frobnicate"), None);
        assert_eq!(lookup_simd("f64x2_to_i32x4"), None);
    }

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
