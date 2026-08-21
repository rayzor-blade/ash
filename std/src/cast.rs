use std::{
    cmp::Ordering,
    ffi::{c_void, CStr},
    ptr,
};

use crate::{
    buffer::hlp_type_str,
    error::hlp_error,
    fun::hlp_make_fun_wrapper,
    hl::{
        self, hl_type, hl_type__bindgen_ty_1, hl_type_kind_HABSTRACT, hl_type_kind_HARRAY,
        hl_type_kind_HBOOL, hl_type_kind_HBYTES, hl_type_kind_HDYN, hl_type_kind_HDYNOBJ,
        hl_type_kind_HF32, hl_type_kind_HF64, hl_type_kind_HFUN, hl_type_kind_HI32,
        hl_type_kind_HI64, hl_type_kind_HNULL, hl_type_kind_HOBJ, hl_type_kind_HREF,
        hl_type_kind_HSTRUCT, hl_type_kind_HTYPE, hl_type_kind_HUI16, hl_type_kind_HUI8,
        hl_type_kind_HVIRTUAL, uchar, vclosure, vdynamic, vvirtual,
    },
    obj::hl_to_virtual,
    strings::str_to_uchar_ptr,
    types::{hlp_is_dynamic, hlp_safe_cast},
};

pub unsafe extern "C" fn invalid_cast(from: *mut hl_type, to: *mut hl_type) {
    hlp_error(str_to_uchar_ptr(&format!(
        "Can't cast {} to {}",
        CStr::from_ptr(hlp_type_str(from) as *const i8).to_string_lossy(),
        CStr::from_ptr(hlp_type_str(to) as *const i8).to_string_lossy()
    )));
}

#[no_mangle]
pub unsafe extern "C" fn hlp_make_dyn(data: *mut c_void, t: *mut hl_type) -> *mut vdynamic {
    let kind = (*t).kind;
    match kind {
        hl_type_kind_HUI8 => {
            let v = crate::gc::gc_alloc(std::mem::size_of::<vdynamic>())
                .expect("Failed to allocate vdynamic")
                .as_ptr() as *mut vdynamic;
            (*v).t = t;
            (*v).v.ui8 = *(data as *mut u8);
            v
        }
        hl_type_kind_HUI16 => {
            let v = crate::gc::gc_alloc(std::mem::size_of::<vdynamic>())
                .expect("Failed to allocate vdynamic")
                .as_ptr() as *mut vdynamic;
            (*v).t = t;
            (*v).v.ui16 = *(data as *mut uchar);
            v
        }
        hl_type_kind_HI32 => {
            let v = crate::gc::gc_alloc(std::mem::size_of::<vdynamic>())
                .expect("Failed to allocate vdynamic")
                .as_ptr() as *mut vdynamic;
            (*v).t = t;
            (*v).v.i = *(data as *mut i32);
            v
        }
        hl_type_kind_HI64 => {
            let v = crate::gc::gc_alloc(std::mem::size_of::<vdynamic>())
                .expect("Failed to allocate vdynamic")
                .as_ptr() as *mut vdynamic;
            (*v).t = t;
            (*v).v.i64_ = *(data as *mut hl::int64);
            v
        }
        hl_type_kind_HF32 => {
            let v = crate::gc::gc_alloc(std::mem::size_of::<vdynamic>())
                .expect("Failed to allocate vdynamic")
                .as_ptr() as *mut vdynamic;
            (*v).t = t;
            (*v).v.f = *(data as *mut f32);
            v
        }
        hl_type_kind_HF64 => {
            let v = crate::gc::gc_alloc(std::mem::size_of::<vdynamic>())
                .expect("Failed to allocate vdynamic")
                .as_ptr() as *mut vdynamic;
            (*v).t = t;
            (*v).v.d = *(data as *mut f64);
            v
        }
        hl_type_kind_HBOOL => {
            let b = *(data as *mut bool);
            let v = crate::gc::gc_alloc(std::mem::size_of::<vdynamic>())
                .expect("Failed to allocate vdynamic")
                .as_ptr() as *mut vdynamic;
            (*t).kind = hl_type_kind_HBOOL;
            (*v).t = t;
            (*v).v.b = b;
            v
        }
        hl_type_kind_HBYTES | hl_type_kind_HTYPE | hl_type_kind_HREF | hl_type_kind_HABSTRACT => {
            if data.is_null() {
                return std::ptr::null_mut();
            }
            let v = crate::gc::gc_alloc(std::mem::size_of::<vdynamic>())
                .expect("Failed to allocate vdynamic")
                .as_ptr() as *mut vdynamic;
            (*v).t = t;
            (*v).v.ptr = *(data as *mut *mut c_void);
            v
        }
        _ => *(data as *mut *mut vdynamic),
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_dyn_casti(
    data: *mut c_void,
    t: *mut hl_type,
    to: *mut hl_type,
) -> i32 {
    // hl_track_call(HL_TRACK_CAST, on_cast(t, to));
    let mut t = t;
    let mut data = data;

    if (*t).kind == hl::hl_type_kind_HDYN {
        let v = *(data as *mut *mut vdynamic);
        if v.is_null() {
            return 0;
        }
        t = (*v).t;
        if !hlp_is_dynamic(t) {
            data = &mut (*v).v as *mut _ as *mut c_void;
        }
    }

    match (*t).kind {
        hl::hl_type_kind_HUI8 => *(data as *mut u8) as i32,
        hl::hl_type_kind_HUI16 => *(data as *mut u16) as i32,
        hl::hl_type_kind_HI32 => *(data as *mut i32),
        hl::hl_type_kind_HI64 => *(data as *mut i64) as i32,
        hl::hl_type_kind_HF32 => *(data as *mut f32) as i32,
        hl::hl_type_kind_HF64 => *(data as *mut f64) as i32,
        hl::hl_type_kind_HBOOL => *(data as *mut bool) as i32,
        hl::hl_type_kind_HNULL => {
            let v = *(data as *mut *mut vdynamic);
            if v.is_null() {
                0
            } else {
                hlp_dyn_casti(
                    &mut (*v).v as *mut _ as *mut c_void,
                    (*t).__bindgen_anon_1.tparam,
                    to,
                )
            }
        }
        _ => {
            invalid_cast(t, to);
            0
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_dyn_castf(data: *mut c_void, t: *mut hl_type) -> f32 {
    // hl_track_call(HL_TRACK_CAST, on_cast(t, to));
    let mut t = t;
    let mut data = data;

    if (*t).kind == hl::hl_type_kind_HDYN {
        let v = *(data as *mut *mut vdynamic);
        if v.is_null() {
            return 0.0;
        }
        t = (*v).t;
        if !hlp_is_dynamic(t) {
            data = &mut (*v).v as *mut _ as *mut c_void;
        }
    }

    match (*t).kind {
        hl::hl_type_kind_HUI8 => *(data as *mut u8) as f32,
        hl::hl_type_kind_HUI16 => *(data as *mut u16) as f32,
        hl::hl_type_kind_HI32 => *(data as *mut f32),
        hl::hl_type_kind_HI64 => *(data as *mut i64) as f32,
        hl::hl_type_kind_HF32 => *(data as *mut f32),
        hl::hl_type_kind_HF64 => *(data as *mut f64) as f32,
        hl::hl_type_kind_HBOOL => {
            if *(data as *mut bool) {
                1_f32
            } else {
                0_f32
            }
        }
        hl::hl_type_kind_HNULL => {
            let v = *(data as *mut *mut vdynamic);
            if v.is_null() {
                0.0
            } else {
                hlp_dyn_castf(
                    &mut (*v).v as *mut _ as *mut c_void,
                    (*t).__bindgen_anon_1.tparam,
                )
            }
        }
        _ => {
            invalid_cast(
                t,
                &mut hl_type {
                    kind: hl_type_kind_HF32,
                    __bindgen_anon_1: hl_type__bindgen_ty_1 {
                        obj: ptr::null_mut(),
                    },
                    vobj_proto: ptr::null_mut(),
                    mark_bits: ptr::null_mut(),
                },
            );
            0.0
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_dyn_castd(data: *mut c_void, t: *mut hl_type) -> f64 {
    // hl_track_call(HL_TRACK_CAST, on_cast(t, to));
    let mut t = t;
    let mut data = data;

    if (*t).kind == hl::hl_type_kind_HDYN {
        let v = *(data as *mut *mut vdynamic);
        if v.is_null() {
            return 0.0;
        }
        t = (*v).t;
        if !hlp_is_dynamic(t) {
            data = &mut (*v).v as *mut _ as *mut c_void;
        }
    }

    match (*t).kind {
        hl::hl_type_kind_HUI8 => *(data as *mut u8) as f64,
        hl::hl_type_kind_HUI16 => *(data as *mut u16) as f64,
        hl::hl_type_kind_HI32 => *(data as *mut f64),
        hl::hl_type_kind_HI64 => *(data as *mut i64) as f64,
        hl::hl_type_kind_HF32 => *(data as *mut f32) as f64,
        hl::hl_type_kind_HF64 => *(data as *mut f64),
        hl::hl_type_kind_HBOOL => {
            if *(data as *mut bool) {
                1_f64
            } else {
                0_f64
            }
        }
        hl::hl_type_kind_HNULL => {
            let v = *(data as *mut *mut vdynamic);
            if v.is_null() {
                0.0
            } else {
                hlp_dyn_castd(
                    &mut (*v).v as *mut _ as *mut c_void,
                    (*t).__bindgen_anon_1.tparam,
                )
            }
        }
        _ => {
            invalid_cast(
                t,
                &mut hl_type {
                    kind: hl_type_kind_HF64,
                    __bindgen_anon_1: hl_type__bindgen_ty_1 {
                        obj: ptr::null_mut(),
                    },
                    vobj_proto: ptr::null_mut(),
                    mark_bits: ptr::null_mut(),
                },
            );
            0.0
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_dyn_casti64(data: *mut c_void, t: *mut hl_type) -> i64 {
    // hl_track_call(HL_TRACK_CAST, on_cast(t, &hlt_i64));
    let mut t = t;
    let mut data = data;

    if (*t).kind == hl::hl_type_kind_HDYN {
        let v = *(data as *mut *mut vdynamic);
        if v.is_null() {
            return 0;
        }
        t = (*v).t;
        if !hlp_is_dynamic(t) {
            data = &mut (*v).v as *mut _ as *mut c_void;
        }
    }

    match (*t).kind {
        hl::hl_type_kind_HUI8 => *(data as *mut u8) as i64,
        hl::hl_type_kind_HUI16 => *(data as *mut u16) as i64,
        hl::hl_type_kind_HI32 => *(data as *mut i32) as i64,
        hl::hl_type_kind_HI64 => *(data as *mut i64),
        hl::hl_type_kind_HF32 => *(data as *mut f32) as i64,
        hl::hl_type_kind_HF64 => *(data as *mut f64) as i64,
        hl::hl_type_kind_HBOOL => *(data as *mut bool) as i64,
        hl::hl_type_kind_HNULL => {
            let v = *(data as *mut *mut vdynamic);
            if v.is_null() {
                0
            } else {
                hlp_dyn_casti64(
                    &mut (*v).v as *mut _ as *mut c_void,
                    (*t).__bindgen_anon_1.tparam,
                )
            }
        }
        _ => {
            invalid_cast(
                t,
                &mut hl_type {
                    kind: hl_type_kind_HI64,
                    __bindgen_anon_1: hl_type__bindgen_ty_1 {
                        obj: std::ptr::null_mut(),
                    },
                    vobj_proto: std::ptr::null_mut(),
                    mark_bits: std::ptr::null_mut(),
                },
            );
            0
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_dyn_castp(
    data: *mut c_void,
    t: *mut hl_type,
    to: *mut hl_type,
) -> *mut c_void {
    if t.is_null() || to.is_null() {
        return ptr::null_mut();
    }
    if (*t).kind > 22 || (*to).kind > 22 {
        return ptr::null_mut();
    }

    if (*to).kind == hl_type_kind_HDYN && hlp_is_dynamic(t) {
        return *(data as *mut *mut vdynamic) as *mut c_void;
    }

    let mut t = t;
    let mut data = data;

    let gc = crate::gc::gc_locked();

    if (*t).kind == hl_type_kind_HDYN || (*t).kind == hl_type_kind_HNULL {
        let v = *(data as *mut *mut vdynamic);
        if v.is_null() {
            return ptr::null_mut();
        }
        if (*to).kind == hl_type_kind_HNULL
            && (*v).t == (*to).__bindgen_anon_1.tparam
            && gc.is_gc_ptr(v)
        {
            return v as *mut c_void;
        }
        t = (*v).t;
        if !hlp_is_dynamic(t) {
            data = &mut (*v).v as *mut _ as *mut c_void;
        }
    } else if hlp_is_dynamic(t) {
        let v = *(data as *mut *mut vdynamic);
        if v.is_null() {
            return ptr::null_mut();
        }
        t = (*v).t;
        if t.is_null() || (*t).kind > 22 {
            return ptr::null_mut();
        }
    }

    if t.is_null() || to.is_null() {
        return ptr::null_mut();
    }
    if env_flag!("ASH_DBG_SC") {
        eprintln!(
            "[dyn_castp] pre-safe_cast t={:p} k={} to={:p} k={}",
            t,
            (*t).kind,
            to,
            (*to).kind
        );
        use std::io::Write;
        std::io::stderr().flush().ok();
    }
    if t == to || hlp_safe_cast(t, to) {
        return *(data as *mut *mut c_void);
    }

    match ((*t).kind, (*to).kind) {
        (hl_type_kind_HOBJ, hl_type_kind_HOBJ) => {
            let t1_obj = (*t).__bindgen_anon_1.obj;
            let t2 = (*to).__bindgen_anon_1.obj;
            if t1_obj.is_null()
                || t2.is_null()
                || (t1_obj as usize) < 0x10000
                || (t2 as usize) < 0x10000
                || !(t1_obj as usize).is_multiple_of(std::mem::align_of::<usize>())
                || !(t2 as usize).is_multiple_of(std::mem::align_of::<usize>())
            {
                return ptr::null_mut();
            }
            let mut t1 = t1_obj;
            loop {
                if t1 == t2 {
                    return *(data as *mut *mut c_void);
                }
                if (*t1).super_.is_null() {
                    break;
                }
                let sup = (*t1).super_;
                if (sup as usize) < 0x10000 || !(sup as usize).is_multiple_of(std::mem::align_of::<usize>()) {
                    break;
                }
                if (*sup).kind != hl_type_kind_HOBJ {
                    break;
                }
                let sup_obj = (*sup).__bindgen_anon_1.obj;
                if sup_obj.is_null()
                    || (sup_obj as usize) < 0x10000
                    || !(sup_obj as usize).is_multiple_of(std::mem::align_of::<usize>())
                {
                    break;
                }
                t1 = sup_obj;
            }
            let t_obj_for_cast = (*t).__bindgen_anon_1.obj;
            if !t_obj_for_cast.is_null()
                && (t_obj_for_cast as usize) >= 0x10000
                && !(*t_obj_for_cast).rt.is_null()
                && ((*t_obj_for_cast).rt as usize) >= 0x10000
                && (*(*t_obj_for_cast).rt).castFun.is_some()
            {
                let cast_fn = (*(*t_obj_for_cast).rt).castFun.unwrap();
                let obj_val = *(data as *mut *mut vdynamic);
                if env_flag!("ASH_DBG_SC") {
                    eprintln!(
                        "[dyn_castp] castFun={:p} obj={:p} to={:p}",
                        cast_fn as *const (), obj_val, to
                    );
                    std::io::Write::flush(&mut std::io::stderr()).ok();
                }
                let v = cast_fn(obj_val, to);
                if !v.is_null() {
                    return v as *mut c_void;
                }
            }
        }
        (hl_type_kind_HSTRUCT, hl_type_kind_HSTRUCT) => {
            let mut t1 = (*t).__bindgen_anon_1.obj;
            let t2 = (*to).__bindgen_anon_1.obj;
            loop {
                if t1 == t2 {
                    return *(data as *mut *mut c_void);
                }
                if (*t1).super_.is_null() {
                    break;
                }
                t1 = (*(*t1).super_).__bindgen_anon_1.obj;
            }
            let t_obj_for_cast = (*t).__bindgen_anon_1.obj;
            if !t_obj_for_cast.is_null()
                && (t_obj_for_cast as usize) >= 0x10000
                && !(*t_obj_for_cast).rt.is_null()
                && ((*t_obj_for_cast).rt as usize) >= 0x10000
                && (*(*t_obj_for_cast).rt).castFun.is_some()
            {
                let cast_fn = (*(*t_obj_for_cast).rt).castFun.unwrap();
                let obj_val = *(data as *mut *mut vdynamic);
                if env_flag!("ASH_DBG_SC") {
                    eprintln!(
                        "[dyn_castp] castFun={:p} obj={:p} to={:p}",
                        cast_fn as *const (), obj_val, to
                    );
                    std::io::Write::flush(&mut std::io::stderr()).ok();
                }
                let v = cast_fn(obj_val, to);
                if !v.is_null() {
                    return v as *mut c_void;
                }
            }
        }
        (hl_type_kind_HFUN, hl_type_kind_HFUN) => {
            let c = *(data as *mut *mut vclosure);
            if !c.is_null() {
                let c = hlp_make_fun_wrapper(c, to);
                if !c.is_null() {
                    return c as *mut c_void;
                }
            }
        }
        (hl_type_kind_HOBJ, hl_type_kind_HVIRTUAL) => {
            return hl_to_virtual(to, *(data as *mut *mut vdynamic)) as *mut c_void;
        }
        (hl_type_kind_HDYNOBJ, hl_type_kind_HVIRTUAL) => {
            return hl_to_virtual(to, *(data as *mut *mut vdynamic)) as *mut c_void;
        }
        (hl_type_kind_HVIRTUAL, hl_type_kind_HVIRTUAL) => {
            return hl_to_virtual(to, *(data as *mut *mut vdynamic)) as *mut c_void;
        }
        (hl_type_kind_HVIRTUAL, hl_type_kind_HOBJ) => {
            let v = *(data as *mut *mut vvirtual);
            if !(*v).value.is_null() {
                return hlp_dyn_castp(
                    &mut (*(*v).value).v as *mut _ as *mut c_void,
                    (*(*v).value).t,
                    to,
                );
            }
        }
        (hl_type_kind_HOBJ, hl_type_kind_HDYN)
        | (hl_type_kind_HDYNOBJ, hl_type_kind_HDYN)
        | (hl_type_kind_HFUN, hl_type_kind_HDYN)
        | (hl_type_kind_HNULL, hl_type_kind_HDYN)
        | (hl_type_kind_HARRAY, hl_type_kind_HDYN) => {
            return *(data as *mut *mut c_void);
        }
        _ => {}
    }

    if (*to).kind == hl_type_kind_HDYN {
        return hlp_make_dyn(data, t) as *mut c_void;
    }

    if (*to).kind == hl_type_kind_HNULL {
        if (*(*to).__bindgen_anon_1.tparam).kind == (*t).kind {
            return hlp_make_dyn(data, t) as *mut c_void;
        }
        match (*(*to).__bindgen_anon_1.tparam).kind {
            hl_type_kind_HUI8 | hl_type_kind_HUI16 | hl_type_kind_HI32 | hl_type_kind_HBOOL => {
                let v = hlp_dyn_casti(data, t, (*to).__bindgen_anon_1.tparam);
                return hlp_make_dyn(&v as *const _ as *mut c_void, (*to).__bindgen_anon_1.tparam)
                    as *mut c_void;
            }
            hl_type_kind_HI64 => {
                let v = hlp_dyn_casti64(data, t);
                return hlp_make_dyn(&v as *const _ as *mut c_void, (*to).__bindgen_anon_1.tparam)
                    as *mut c_void;
            }
            hl_type_kind_HF32 => {
                let f = hlp_dyn_castf(data, t);
                return hlp_make_dyn(&f as *const _ as *mut c_void, (*to).__bindgen_anon_1.tparam)
                    as *mut c_void;
            }
            hl_type_kind_HF64 => {
                let d = hlp_dyn_castd(data, t);
                return hlp_make_dyn(&d as *const _ as *mut c_void, (*to).__bindgen_anon_1.tparam)
                    as *mut c_void;
            }
            _ => {}
        }
    }

    if (*to).kind == hl_type_kind_HREF {
        match (*(*to).__bindgen_anon_1.tparam).kind {
            hl_type_kind_HUI8 | hl_type_kind_HUI16 | hl_type_kind_HI32 | hl_type_kind_HBOOL => {
                let v = crate::gc::gc_alloc(std::mem::size_of::<i32>())
                    .expect("Out of memory") // Handle this error appropriately
                    .as_ptr() as *mut i32;
                *v = hlp_dyn_casti(data, t, (*to).__bindgen_anon_1.tparam);
                return v as *mut c_void;
            }
            hl_type_kind_HI64 => {
                let d = crate::gc::gc_alloc(std::mem::size_of::<i64>())
                    .expect("Out of memory")
                    .as_ptr() as *mut i64;
                *d = hlp_dyn_casti64(data, t);
                return d as *mut c_void;
            }
            hl_type_kind_HF32 => {
                let f = crate::gc::gc_alloc(std::mem::size_of::<f32>())
                    .expect("Out of memory")
                    .as_ptr() as *mut f32;
                *f = hlp_dyn_castf(data, t);
                return f as *mut c_void;
            }
            hl_type_kind_HF64 => {
                let d = crate::gc::gc_alloc(std::mem::size_of::<f64>())
                    .expect("Out of memory")
                    .as_ptr() as *mut f64;
                *d = hlp_dyn_castd(data, t);
                return d as *mut c_void;
            }
            _ => {
                let p = crate::gc::gc_alloc(std::mem::size_of::<*mut c_void>())
                    .expect("Out of memory")
                    .as_ptr() as *mut *mut c_void;
                *p = hlp_dyn_castp(data, t, (*to).__bindgen_anon_1.tparam);
                return p as *mut c_void;
            }
        }
    }

    invalid_cast(t, to);
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_value_cast(
    v: *mut hl::vdynamic,
    t: *mut hl::hl_type,
) -> *mut hl::vdynamic {
    // hl_track_call(HL_TRACK_CAST, {
    //     let v_type = if !v.is_null() { (*v).t } else { &hl::hlt_dyn as *const _ as *mut _ };
    //     hl::on_cast(v_type, t);
    // });

    if (*t).kind == hl::hl_type_kind_HDYN || v.is_null() || hlp_safe_cast((*v).t, t) {
        return v;
    }

    invalid_cast((*v).t, t);
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_type_safe_cast(a: *mut hl::hl_type, b: *mut hl::hl_type) -> bool {
    hlp_safe_cast(a, b)
}

#[no_mangle]
pub unsafe extern "C" fn hlp_ptr_compare(a: *const vdynamic, b: *const vdynamic) -> i32 {
    match (a as usize).cmp(&(b as usize)) {
        Ordering::Equal => 0,
        Ordering::Greater => 1,
        Ordering::Less => -1,
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_dyn_compare(a: *mut vdynamic, b: *mut vdynamic) -> i32 {
    if a == b {
        return 0;
    }
    if a.is_null() {
        return -1;
    }
    if b.is_null() {
        return 1;
    }
    if (*a).t.is_null() || (*b).t.is_null() {
        return hlp_ptr_compare(a, b);
    }

    let ka = (*(*a).t).kind;
    let kb = (*(*b).t).kind;
    let cmp_f64 = |x: f64, y: f64| -> i32 {
        if x < y {
            -1
        } else if x > y {
            1
        } else {
            0
        }
    };
    let cmp_ord = |o: Ordering| -> i32 {
        match o {
            Ordering::Less => -1,
            Ordering::Equal => 0,
            Ordering::Greater => 1,
        }
    };

    if ka == kb {
        return match ka {
            hl_type_kind_HI32 => cmp_ord((*a).v.i.cmp(&(*b).v.i)),
            hl_type_kind_HUI8 => cmp_ord((*a).v.ui8.cmp(&(*b).v.ui8)),
            hl_type_kind_HUI16 => cmp_ord((*a).v.ui16.cmp(&(*b).v.ui16)),
            hl_type_kind_HI64 => cmp_ord((*a).v.i64_.cmp(&(*b).v.i64_)),
            hl_type_kind_HBOOL => cmp_ord(((*a).v.b as u8).cmp(&((*b).v.b as u8))),
            hl_type_kind_HF32 => cmp_f64((*a).v.f as f64, (*b).v.f as f64),
            hl_type_kind_HF64 => cmp_f64((*a).v.d, (*b).v.d),
            hl_type_kind_HBYTES => {
                crate::ucs2::ucmp((*a).v.bytes as *const uchar, (*b).v.bytes as *const uchar)
                    .signum()
            }
            hl_type_kind_HOBJ | hl_type_kind_HSTRUCT => {
                let obj = (*(*a).t).__bindgen_anon_1.obj;
                if !obj.is_null() {
                    let rt = (*obj).rt;
                    if !rt.is_null() {
                        if let Some(compare_fn) = (*rt).compareFun {
                            return compare_fn(a, b);
                        }
                        // String-like objects (first field is HBYTES): compare content
                        if (*obj).nfields >= 1
                            && !(*obj).fields.is_null()
                            && (*(*(*obj).fields).t).kind == hl_type_kind_HBYTES
                        {
                            let parent_fields = (*rt).nfields as usize - (*obj).nfields as usize;
                            let offset = *(*rt).fields_indexes.add(parent_fields) as usize;
                            let a_bytes = *((a as *mut u8).add(offset) as *mut *const uchar);
                            let b_bytes = *((b as *mut u8).add(offset) as *mut *const uchar);
                            if !a_bytes.is_null() && !b_bytes.is_null() {
                                return crate::ucs2::ucmp(a_bytes, b_bytes).signum();
                            }
                        }
                    }
                }
                hlp_ptr_compare(a, b)
            }
            _ => hlp_ptr_compare((*a).v.ptr as *const vdynamic, (*b).v.ptr as *const vdynamic),
        };
    }

    let as_num = |v: *mut vdynamic, k: u32| -> Option<f64> {
        match k {
            hl_type_kind_HI32 => Some(unsafe { (*v).v.i as f64 }),
            hl_type_kind_HUI8 => Some(unsafe { (*v).v.ui8 as f64 }),
            hl_type_kind_HUI16 => Some(unsafe { (*v).v.ui16 as f64 }),
            hl_type_kind_HI64 => Some(unsafe { (*v).v.i64_ as f64 }),
            hl_type_kind_HF32 => Some(unsafe { (*v).v.f as f64 }),
            hl_type_kind_HF64 => Some(unsafe { (*v).v.d }),
            hl_type_kind_HBOOL => Some(unsafe {
                if (*v).v.b {
                    1.0
                } else {
                    0.0
                }
            }),
            _ => None,
        }
    };

    if let (Some(x), Some(y)) = (as_num(a, ka), as_num(b, kb)) {
        return cmp_f64(x, y);
    }

    hlp_ptr_compare(a, b)
}

/// Dynamic arithmetic: `a <op> b` where at least one side arrives boxed.
///
/// Mirrors `hl_dyn_op` in HashLink's std/cast.c, including the part that
/// surprises people: the arithmetic operators always yield an f64, even for
/// two boxed ints, because both operands go through `dyn_castd` on the way
/// in. Only the shifts and bitwise operators produce an i32. Matching that
/// exactly matters more than it looks — Haxe code that adds two Dynamics and
/// then checks `Std.isOfType(v, Int)` observes the difference.
#[no_mangle]
pub unsafe extern "C" fn hlp_dyn_op(
    op: i32,
    a: *mut vdynamic,
    b: *mut vdynamic,
) -> *mut vdynamic {
    const OP_ADD: i32 = 0;
    const OP_SUB: i32 = 1;
    const OP_MUL: i32 = 2;
    const OP_MOD: i32 = 3;
    const OP_DIV: i32 = 4;
    const OP_SHL: i32 = 5;
    const OP_SHR: i32 = 6;
    const OP_USHR: i32 = 7;
    const OP_AND: i32 = 8;
    const OP_OR: i32 = 9;
    const OP_XOR: i32 = 10;
    const OP_LAST: i32 = 11;

    const OP_NAMES: [&str; OP_LAST as usize] =
        ["+", "-", "*", "%", "/", "<<", ">>", ">>>", "&", "|", "^"];

    if !(0..OP_LAST).contains(&op) {
        hlp_error(str_to_uchar_ptr(&format!("Invalid op {op}")));
        return ptr::null_mut();
    }

    // Two nulls are not an error: division and modulo yield NaN, everything
    // else yields null, which is what `null + null` evaluates to in Haxe.
    if a.is_null() && b.is_null() {
        if op == OP_DIV || op == OP_MOD {
            let nan = f64::NAN;
            return hlp_make_dyn(&nan as *const f64 as *mut c_void, crate::types::hlt_f64());
        }
        return ptr::null_mut();
    }

    // A null operand counts as a number here (it casts to 0 / NaN), matching
    // upstream's `!a || is_number(a->t)`.
    let numeric = |v: *mut vdynamic| -> bool {
        if v.is_null() {
            return true;
        }
        let t = (*v).t;
        if t.is_null() {
            return false;
        }
        // HUI8 (1) through HBOOL (7) — upstream's is_number, and ash's kind
        // numbering is identical to hl.h's.
        (hl::hl_type_kind_HUI8..=hl::hl_type_kind_HBOOL).contains(&(*t).kind)
    };

    if numeric(a) && numeric(b) {
        let mut pa = a;
        let mut pb = b;
        let dyn_t = crate::types::hlt_dyn();
        // `dyn_castd`/`dyn_casti` take the address of the operand, not the
        // operand, exactly as the C does with `&a`.
        let da = &mut pa as *mut *mut vdynamic as *mut c_void;
        let db = &mut pb as *mut *mut vdynamic as *mut c_void;

        let as_f64 = |v: f64| -> *mut vdynamic {
            hlp_make_dyn(&v as *const f64 as *mut c_void, crate::types::hlt_f64())
        };
        let as_i32 = |v: i32| -> *mut vdynamic {
            hlp_make_dyn(&v as *const i32 as *mut c_void, crate::types::hlt_i32())
        };

        match op {
            OP_ADD | OP_SUB | OP_MUL | OP_DIV | OP_MOD => {
                let va = hlp_dyn_castd(da, dyn_t);
                let vb = hlp_dyn_castd(db, dyn_t);
                return as_f64(match op {
                    OP_ADD => va + vb,
                    OP_SUB => va - vb,
                    OP_MUL => va * vb,
                    OP_DIV => va / vb,
                    _ => va % vb, // Rust's % on f64 is fmod, including sign
                });
            }
            _ => {
                let i32_t = crate::types::hlt_i32();
                let va = hlp_dyn_casti(da, dyn_t, i32_t);
                let vb = hlp_dyn_casti(db, dyn_t, i32_t);
                return as_i32(match op {
                    // Upstream shifts with C's operators, which are undefined
                    // past the width; wrapping keeps the result defined
                    // without changing it for the in-range shifts real code
                    // performs.
                    OP_SHL => va.wrapping_shl(vb as u32),
                    OP_SHR => va.wrapping_shr(vb as u32),
                    OP_USHR => ((va as u32).wrapping_shr(vb as u32)) as i32,
                    OP_AND => va & vb,
                    OP_OR => va | vb,
                    OP_XOR => va ^ vb,
                    // Unreachable: the range check at the top rejected every
                    // other op. A value rather than a panic, because an
                    // unwind here would cross the FFI boundary.
                    _ => 0,
                });
            }
        }
    }

    let name = |v: *mut vdynamic| -> String {
        if v.is_null() || (*v).t.is_null() {
            return "null".to_string();
        }
        CStr::from_ptr(hlp_type_str((*v).t) as *const i8)
            .to_string_lossy()
            .into_owned()
    };
    hlp_error(str_to_uchar_ptr(&format!(
        "Can't perform dyn op {} {} {}",
        name(a),
        OP_NAMES[op as usize],
        name(b)
    )));
    ptr::null_mut()
}
