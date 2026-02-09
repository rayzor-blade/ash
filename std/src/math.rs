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
pub unsafe extern "C" fn hlp_math_floor(x: c_double) -> c_double {
    x.floor()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_math_ceil(x: c_double) -> c_double {
    x.ceil()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_math_round(x: c_double) -> c_double {
    x.round()
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
pub unsafe extern "C" fn hlp_math_ffloor(x: c_double) -> i32 {
    x.floor() as i32
}

#[no_mangle]
pub unsafe extern "C" fn hlp_math_fceil(x: c_double) -> i32 {
    x.ceil() as i32
}

#[no_mangle]
pub unsafe extern "C" fn hlp_math_fround(x: c_double) -> i32 {
    x.round() as i32
}
