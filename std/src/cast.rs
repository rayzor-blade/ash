use std::{
    cmp::Ordering, ffi::{c_long, c_void, CStr}, ptr
};

use crate::{
    buffer::hlp_type_str, error::hlp_error, fun::hlp_make_fun_wrapper, gc::GC, hl::{
        self, hl_type, hl_type__bindgen_ty_1, hl_type_kind_HABSTRACT, hl_type_kind_HARRAY, hl_type_kind_HBOOL, hl_type_kind_HBYTES, hl_type_kind_HDYN, hl_type_kind_HDYNOBJ, hl_type_kind_HF32, hl_type_kind_HF64, hl_type_kind_HFUN, hl_type_kind_HI32, hl_type_kind_HI64, hl_type_kind_HNULL, hl_type_kind_HOBJ, hl_type_kind_HREF, hl_type_kind_HSTRUCT, hl_type_kind_HTYPE, hl_type_kind_HUI16, hl_type_kind_HUI8, hl_type_kind_HVIRTUAL, uchar, vclosure, vdynamic, vvirtual
    }, obj::hl_to_virtual, strings::str_to_uchar_ptr, types::{hlp_is_dynamic, hlp_safe_cast}
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
    let gc = GC.get_mut().expect("Expect to get GC");
    match kind {
        hl_type_kind_HUI8 => {
            let v = gc
                .allocate(std::mem::size_of::<vdynamic>())
                .expect("Failed to allocate vdynamic")
                .as_ptr() as *mut vdynamic;
            (*v).t = t;
            (*v).v.ui8 = *(data as *mut u8);
            v
        }
        hl_type_kind_HUI16 => {
            let v = gc
                .allocate(std::mem::size_of::<vdynamic>())
                .expect("Failed to allocate vdynamic")
                .as_ptr() as *mut vdynamic;
            (*v).t = t;
            (*v).v.ui16 = *(data as *mut uchar);
            v
        }
        hl_type_kind_HI32 => {
            let v = gc
                .allocate(std::mem::size_of::<vdynamic>())
                .expect("Failed to allocate vdynamic")
                .as_ptr() as *mut vdynamic;
            (*v).t = t;
            (*v).v.i = *(data as *mut i32);
            v
        }
        hl_type_kind_HI64 => {
            let v = gc
                .allocate(std::mem::size_of::<vdynamic>())
                .expect("Failed to allocate vdynamic")
                .as_ptr() as *mut vdynamic;
            (*v).t = t;
            (*v).v.i64_ = *(data as *mut c_long);
            v
        }
        hl_type_kind_HF32 => {
            let v = gc
                .allocate(std::mem::size_of::<vdynamic>())
                .expect("Failed to allocate vdynamic")
                .as_ptr() as *mut vdynamic;
            (*v).t = t;
            (*v).v.f = *(data as *mut f32);
            v
        }
        hl_type_kind_HF64 => {
            let v = gc
                .allocate(std::mem::size_of::<vdynamic>())
                .expect("Failed to allocate vdynamic")
                .as_ptr() as *mut vdynamic;
            (*v).t = t;
            (*v).v.d = *(data as *mut f64);
            v
        }
        hl_type_kind_HBOOL => {
            let b = *(data as *mut bool);
            let v = gc
                .allocate(std::mem::size_of::<vdynamic>())
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
            let v = gc
                .allocate(std::mem::size_of::<vdynamic>())
                .expect("Failed to allocate vdynamic")
                .as_ptr() as *mut vdynamic;
            (*v).v.ptr = data;
            v
        }
        _ => std::mem::transmute(data),
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
pub unsafe extern "C" fn hlp_dyn_castf(
    data: *mut c_void,
    t: *mut hl_type
) -> f32 {
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
                    (*t).__bindgen_anon_1.tparam
                )
            }
        }
        _ => {
            invalid_cast(t, &mut hl_type {
                kind: hl_type_kind_HF32,
                __bindgen_anon_1: hl_type__bindgen_ty_1 {
                    obj: ptr::null_mut(),
                },
                vobj_proto: ptr::null_mut(),
                mark_bits: ptr::null_mut(),
            });
            0.0
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_dyn_castd(
    data: *mut c_void,
    t: *mut hl_type,
) -> f64 {
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
            invalid_cast(t, &mut hl_type {
                kind: hl_type_kind_HF64,
                __bindgen_anon_1: hl_type__bindgen_ty_1 {
                    obj: ptr::null_mut(),
                },
                vobj_proto: ptr::null_mut(),
                mark_bits: ptr::null_mut(),
            });
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
    // hl_track_call(HL_TRACK_CAST, on_cast(t, to));

    if (*to).kind == hl_type_kind_HDYN && hlp_is_dynamic(t) {
        return *(data as *mut *mut vdynamic) as *mut c_void;
    }

    let mut t = t;
    let mut data = data;

    let gc = GC.get_mut().expect("Expected to get GC");

    if (*t).kind == hl_type_kind_HDYN || (*t).kind == hl_type_kind_HNULL {
        let v = *(data as *mut *mut vdynamic);
        if v.is_null() {
            return ptr::null_mut();
        }
        if (*to).kind == hl_type_kind_HNULL
            && (*v).t == (*to).__bindgen_anon_1.tparam
            &&gc.is_gc_ptr(v)
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
    }

    if t == to || hlp_safe_cast(t, to) {
        return *(data as *mut *mut c_void);
    }

    match ((*t).kind, (*to).kind) {
        (hl_type_kind_HOBJ, hl_type_kind_HOBJ) =>  {
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
            if !(*(*t).__bindgen_anon_1.obj).rt.is_null() && !(*(*(*t).__bindgen_anon_1.obj).rt).castFun.is_none() {
                let v = (*(*(*t).__bindgen_anon_1.obj).rt).castFun.unwrap()(*(data as *mut *mut vdynamic), to);
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
            if !(*(*t).__bindgen_anon_1.obj).rt.is_null() && !(*(*(*t).__bindgen_anon_1.obj).rt).castFun.is_none() {
                let v = (*(*(*t).__bindgen_anon_1.obj).rt).castFun.unwrap()(*(data as *mut *mut vdynamic), to);
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
                return hlp_make_dyn(&v as *const _ as *mut c_void, (*to).__bindgen_anon_1.tparam) as *mut c_void;
            }
            hl_type_kind_HI64 => {
                let v = hlp_dyn_casti64(data, t);
                return hlp_make_dyn(&v as *const _ as *mut c_void, (*to).__bindgen_anon_1.tparam) as *mut c_void;
            }
            hl_type_kind_HF32 => {
                let f = hlp_dyn_castf(data, t);
                return hlp_make_dyn(&f as *const _ as *mut c_void, (*to).__bindgen_anon_1.tparam) as *mut c_void;
            }
            hl_type_kind_HF64 => {
                let d = hlp_dyn_castd(data, t);
                return hlp_make_dyn(&d as *const _ as *mut c_void, (*to).__bindgen_anon_1.tparam) as *mut c_void;
            }
            _ => {}
        }
    }

    if (*to).kind == hl_type_kind_HREF {
        match (*(*to).__bindgen_anon_1.tparam).kind {
            hl_type_kind_HUI8 | hl_type_kind_HUI16 | hl_type_kind_HI32 | hl_type_kind_HBOOL => {
                let v =gc.allocate(std::mem::size_of::<i32>())
                    .expect("Out of memory") // Handle this error appropriately
                    .as_ptr() as *mut i32;
                *v = hlp_dyn_casti(data, t, (*to).__bindgen_anon_1.tparam);
                return v as *mut c_void;
            },
            hl_type_kind_HI64 => {
                let d =gc.allocate(std::mem::size_of::<i64>())
                    .expect("Out of memory")
                    .as_ptr() as *mut i64;
                *d = hlp_dyn_casti64(data, t);
                return d as *mut c_void;
            },
            hl_type_kind_HF32 => {
                let f =gc.allocate(std::mem::size_of::<f32>())
                    .expect("Out of memory")
                    .as_ptr() as *mut f32;
                *f = hlp_dyn_castf(data, t);
                return f as *mut c_void;
            },
            hl_type_kind_HF64 => {
                let d =gc.allocate(std::mem::size_of::<f64>())
                    .expect("Out of memory")
                    .as_ptr() as *mut f64;
                *d = hlp_dyn_castd(data, t);
                return d as *mut c_void;
            },
            _ => {
                let p =gc.allocate(std::mem::size_of::<*mut c_void>())
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
pub unsafe extern "C" fn hlp_value_cast(v: *mut hl::vdynamic, t: *mut hl::hl_type) -> *mut hl::vdynamic {
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