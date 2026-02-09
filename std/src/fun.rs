use std::{ffi::c_void, mem, ptr};

use crate::{cast::{hlp_dyn_castd, hlp_dyn_castf, hlp_dyn_casti, hlp_dyn_casti64, hlp_dyn_castp, hlp_make_dyn}, error::hlp_error, gc::GC, hl::{self, hl_type, hl_type__bindgen_ty_1, hl_type_kind_HARRAY, hl_type_kind_HBOOL, hl_type_kind_HDYN, hl_type_kind_HF32, hl_type_kind_HF64, hl_type_kind_HFUN, hl_type_kind_HI32, hl_type_kind_HI64, hl_type_kind_HUI16, hl_type_kind_HUI8, hl_type_kind_HVOID, varray, vclosure, vclosure_wrapper, vdynamic}, obj::{hlp_alloc_dynamic, hlp_alloc_dynbool}, strings::str_to_uchar_ptr, types::{hl_aptr, hl_is_ptr, hlp_is_dynamic, hlp_safe_cast}};

pub type HlcFunWrapperType = unsafe extern "C" fn(*mut hl_type) -> *mut c_void;
pub type HlcStaticCallType = unsafe extern "C" fn(_fun: *mut c_void, _t: *mut hl_type, _args: *mut *mut c_void, _out: *mut vdynamic)-> *mut c_void;

pub unsafe extern "C" fn empty_fun_wrapper(_t:*mut hl_type) -> *mut c_void {
    return ptr::null_mut()
}

pub unsafe extern "C" fn empty_static_call(_fun: *mut c_void, _t: *mut hl_type, _args: *mut *mut c_void, _out: *mut vdynamic)-> *mut c_void  {
    return ptr::null_mut()
}

pub static mut hlc_get_wrapper:HlcFunWrapperType=  empty_fun_wrapper;
pub static mut hlc_static_call:HlcStaticCallType=  empty_static_call;
pub static mut hlc_call_flags:i32 = 0;

#[no_mangle]
pub unsafe extern "C" fn hl_setup_callbacks2(c: *mut c_void, w:*mut c_void, flags:i32) {
    hlc_get_wrapper = mem::transmute(w);
    hlc_static_call = mem::transmute(c);
    hlc_call_flags = flags;
}

#[no_mangle]
pub unsafe extern "C" fn _fun_var_args() {
	hlp_error(str_to_uchar_ptr("Variable fun args was not cast to typed function"));
}

pub static mut fun_var_args: unsafe extern "C" fn()  = _fun_var_args;

#[no_mangle]
pub unsafe extern "C" fn hlp_make_fun_wrapper(v: *mut vclosure, to: *mut hl_type) -> *mut vclosure {
    let gc = GC.get_mut().expect("Expected to get GC");
    let wrap = hlc_get_wrapper(to);
    if wrap.is_null() {
        return ptr::null_mut();
    }

    if (*v).fun != fun_var_args as *mut ::std::os::raw::c_void && (*(*v).t).__bindgen_anon_1.fun.as_ref().unwrap().nargs != (*to).__bindgen_anon_1.fun.as_ref().unwrap().nargs {
        return ptr::null_mut();
    }

    let c = gc.allocate(std::mem::size_of::<vclosure_wrapper>()).unwrap().as_ptr() as *mut vclosure_wrapper;
    (*c).cl.t = to;
    (*c).cl.fun = wrap;
    (*c).cl.hasValue = 2;
    
    #[cfg(target_pointer_width = "64")]
    {
        (*c).cl.stackCount = 0;
    }

    (*c).cl.value = c as *mut ::std::os::raw::c_void;
    (*c).wrappedFun = v;

    c as *mut vclosure
}




const HL_MAX_ARGS: usize = 9;

#[no_mangle]
pub unsafe fn hlp_call_method(c: *mut vdynamic, args: *mut varray) -> *mut vdynamic {
    let cl = c as *mut vclosure;
    let vargs = hl_aptr(args) as *mut *mut vdynamic;
    let mut pargs: [*mut libc::c_void; HL_MAX_ARGS] = [ptr::null_mut(); HL_MAX_ARGS];
    let mut tmp: [mem::MaybeUninit<libc::c_double>; HL_MAX_ARGS] = unsafe { mem::MaybeUninit::uninit().assume_init() };
    let mut out: vdynamic = unsafe { mem::zeroed() };

    if (*args).size > HL_MAX_ARGS as i32 {
        hlp_error(str_to_uchar_ptr("Too many arguments"));
    }

    if (*cl).hasValue != 0 {
        if (*cl).fun == fun_var_args as *mut libc::c_void {
            let cl = (*cl).value as *mut vclosure;
            return if (*cl).hasValue != 0 {
                let func: unsafe extern "C" fn(*mut vdynamic, *mut varray) -> *mut vdynamic =
                    mem::transmute((*cl).fun);
                func((*cl).value as *mut vdynamic, args)
            } else {
                let func: unsafe extern "C" fn(*mut varray) -> *mut vdynamic =
                    mem::transmute((*cl).fun);
                func(args)
            };
        }
        hlp_error(str_to_uchar_ptr("Can't call closure with value"));
    }

    if (*args).size < (*(*cl).t).__bindgen_anon_1.fun.as_ref().unwrap().nargs {
        hlp_error(
            str_to_uchar_ptr(&format!("Missing arguments : {} expected but {} passed",
            (*(*cl).t).__bindgen_anon_1.fun.as_ref().unwrap().nargs,
            (*args).size))
        );
    }

    let _hlt_dyn: *mut hl_type = &mut hl_type {
        kind: hl_type_kind_HDYN,
        __bindgen_anon_1: hl_type__bindgen_ty_1 {
            obj: ptr::null_mut(),
        },
        vobj_proto: ptr::null_mut(),
        mark_bits: ptr::null_mut(),
    };

    for i in 0..(*(*cl).t).__bindgen_anon_1.fun.as_ref().unwrap().nargs as usize {
        let v = *vargs.add(i);
        let t = *(*(*cl).t).__bindgen_anon_1.fun.as_ref().unwrap().args.add(i);
        let p: *mut libc::c_void;

        if v.is_null() {
            if hl_is_ptr(t) {
                p = ptr::null_mut();
            } else {
                tmp[i] = mem::MaybeUninit::new(0.0);
                p = tmp[i].as_mut_ptr() as *mut libc::c_void;
            }
        } else {
            match (*t).kind {
                hl_type_kind_HBOOL | hl_type_kind_HUI8 | hl_type_kind_HUI16 | hl_type_kind_HI32 => {
                    tmp[i] = mem::MaybeUninit::new(hlp_dyn_casti((vargs.add(i) as *mut vdynamic) as *mut c_void, _hlt_dyn, t) as f64);
                    p = tmp[i].as_mut_ptr() as *mut libc::c_void;
                }
                hl_type_kind_HI64 => {
                    tmp[i] = mem::MaybeUninit::new(hlp_dyn_casti64((vargs.add(i) as *mut vdynamic) as *mut c_void, _hlt_dyn) as f64);
                    p = tmp[i].as_mut_ptr() as *mut libc::c_void;
                }
                hl_type_kind_HF32 => {
                    tmp[i] = mem::MaybeUninit::new(hlp_dyn_castf((vargs.add(i) as *mut vdynamic) as *mut c_void, _hlt_dyn) as f64);
                    p = tmp[i].as_mut_ptr() as *mut libc::c_void;
                }
                hl_type_kind_HF64 => {
                    tmp[i] = mem::MaybeUninit::new(hlp_dyn_castd((vargs.add(i) as *mut vdynamic) as *mut c_void, _hlt_dyn));
                    p = tmp[i].as_mut_ptr() as *mut libc::c_void;
                }
                _ => {
                    p = hlp_dyn_castp((vargs.add(i) as *mut vdynamic) as *mut c_void, _hlt_dyn, t);
                }
            }
        }
        pargs[i] = p;
    }

    let ret = hlc_static_call(
        if hlc_call_flags & 1 != 0 { &(*cl).fun as *const _ as *mut _ } else { (*cl).fun },
        (*cl).t,
        pargs.as_mut_ptr(),
        &mut out,
    );

    let tret = (*(*cl).t).__bindgen_anon_1.fun.as_ref().unwrap().ret;
    if !hl_is_ptr(tret) {
        match (*tret).kind {
            hl_type_kind_HVOID => return ptr::null_mut(),
            hl_type_kind_HBOOL => return hlp_alloc_dynbool(out.v.b),
            _ => {
                let r = hlp_alloc_dynamic(tret);
                (*r).t = tret;
                (*r).v.d = out.v.d;
                return r;
            }
        }
    }

    if ret.is_null() || hlp_is_dynamic(tret) {
        return ret as *mut vdynamic;
    }

    let dret = hlp_alloc_dynamic(tret);
    (*dret).v.ptr = ret;
    dret
}

#[no_mangle]
pub unsafe extern "C" fn hlp_get_closure_type(t: *mut hl_type) -> *mut hl_type {
    let ft = (*t).__bindgen_anon_1.fun.as_mut().expect("Type is not a function");
    
    if ft.closure_type.kind != hl_type_kind_HFUN {
        if ft.nargs == 0 {
           panic!("assert");
        }
        
        ft.closure_type.kind = hl_type_kind_HFUN;
        ft.closure_type.p = &mut ft.closure as *mut _ as *mut std::ffi::c_void;
        
        ft.closure.nargs = ft.nargs - 1;
        ft.closure.args = if ft.closure.nargs != 0 {
            ft.args.offset(1)
        } else {
            ptr::null_mut()
        };
        ft.closure.ret = ft.ret;
        ft.closure.parent = t;
    }
    
    mem::transmute(&mut ft.closure_type)
}


#[no_mangle]
pub unsafe extern "C" fn hlp_alloc_closure_void(t: *mut hl_type, fvalue: *mut libc::c_void) -> *mut vclosure {
    let size = mem::size_of::<vclosure>();
    let gc = GC.get_mut().expect("Expected to get GC");
    
    let c_ptr = gc.allocate(size)
        .expect("Failed to allocate memory for closure")
        .as_ptr() as *mut vclosure;

    ptr::write(c_ptr, vclosure {
        t,
        fun: fvalue,
        hasValue: 0,
        value: ptr::null_mut(),
        stackCount: 0,
    });

    c_ptr
}

#[no_mangle]
pub unsafe extern "C" fn hlp_alloc_closure_ptr(
    t: *mut hl_type,
    fun: *mut std::ffi::c_void,
    ptr: *mut std::ffi::c_void) -> *mut vclosure {

    let gc = GC.get_mut().expect("Expected to get GC");
    
    let c_ptr = gc.allocate_closure_ptr(t, fun, ptr);

    ptr::write(c_ptr, vclosure {
        t,
        fun,
        hasValue: 1,
        value: ptr,
        stackCount: 0,
    });

    c_ptr
}

#[no_mangle]
pub unsafe extern "C" fn hlp_no_closure(c: *mut vdynamic) -> *mut vdynamic {
    let cl = c as *mut vclosure;
    if (*cl).hasValue == 0 {
        return c;
    }
    if (*cl).hasValue == 2 {
        let wrapper = c as *mut vclosure_wrapper;
        return hlp_no_closure((*wrapper).wrappedFun as *mut vdynamic);
    }
    hlp_alloc_closure_void((*(*cl).t).__bindgen_anon_1.fun.as_ref().unwrap().parent, (*cl).fun) as *mut vdynamic
}

#[no_mangle]
pub unsafe extern "C" fn hlp_make_closure(c: *mut vdynamic, v: *mut vdynamic) -> *mut vdynamic {
    let cl = c as *mut vclosure;
    let t = if (*cl).hasValue != 0 {
        (*(*cl).t).__bindgen_anon_1.fun.as_ref().unwrap().parent
    } else {
        (*cl).t
    };

    if (*cl).hasValue == 2 {
        let wrapper = c as *mut vclosure_wrapper;
        return hlp_make_closure((*wrapper).wrappedFun as *mut vdynamic, v);
    }

    if (*(*t).__bindgen_anon_1.fun.as_ref().unwrap()).nargs == 0 || v.is_null() || !hlp_safe_cast((*v).t, *(*(*t).__bindgen_anon_1.fun.as_ref().unwrap()).args) {
        return ptr::null_mut();
    }

    hlp_alloc_closure_ptr(t, (*cl).fun, v as *mut libc::c_void) as *mut vdynamic
}

#[no_mangle]
pub unsafe extern "C" fn hlp_get_closure_value(c: *mut vdynamic) -> *mut vdynamic {
    let cl = c as *mut vclosure;
    if (*cl).hasValue == 0 {
        return ptr::null_mut();
    }
    if (*cl).hasValue == 2 {
        let wrapper = c as *mut vclosure_wrapper;
        return hlp_get_closure_value((*wrapper).wrappedFun as *mut vdynamic);
    }
    if (*cl).fun == fun_var_args as *mut libc::c_void {
        return ptr::null_mut();
    }
    hlp_make_dyn(
        &(*cl).value as *const _ as *mut libc::c_void,
        *(*(*(*cl).t).__bindgen_anon_1.fun.as_ref().unwrap().parent).__bindgen_anon_1.fun.as_ref().unwrap().args
    )
}

#[no_mangle]
pub unsafe extern "C" fn hlp_fun_compare(a: *mut vdynamic, b: *mut vdynamic) -> bool {
    if a == b {
        return true;
    }
    if a.is_null() || b.is_null() {
        return false;
    }
    if (*(*a).t).kind != (*(*b).t).kind || (*(*a).t).kind != hl_type_kind_HFUN {
        return false;
    }
    let ca = a as *mut vclosure;
    let cb = b as *mut vclosure;
    if (*ca).fun != (*cb).fun {
        return false;
    }
    if (*ca).hasValue != 0 && (*ca).value != (*cb).value {
        return false;
    }
    true
}

#[no_mangle]
pub unsafe extern "C" fn hlp_make_var_args(c: *mut vclosure) -> *mut vdynamic {
   
    let mut HLT_VAR_ARGS: hl_type = hl_type {
        kind: hl_type_kind_HFUN,
        __bindgen_anon_1: hl_type__bindgen_ty_1{
            obj: ptr::null_mut()
        },
        vobj_proto: ptr::null_mut(),
        mark_bits: ptr::null_mut(),
    };

    // Allocate and initialize the closure
    let closure = hlp_alloc_closure_ptr(
        &mut HLT_VAR_ARGS as *mut _,
        fun_var_args as *mut _,
        c as *mut _
    );


    // Cast the closure to vdynamic and return
    closure as *mut vdynamic
}


#[no_mangle]
pub unsafe extern "C" fn hlp_dyn_call(
    c: *mut vclosure,
    args: *mut *mut vdynamic,
    nargs: i32
) -> *mut vdynamic {
    #[repr(C)]
    struct TmpArray {
        a: varray,
        args: [*mut vdynamic; HL_MAX_ARGS + 1],
    }

    if nargs as usize > HL_MAX_ARGS {
        hlp_error(str_to_uchar_ptr("Too many arguments"));
    }

    let mut tmp: TmpArray = mem::zeroed();
    tmp.a.t = crate::types::hlt_array();
    tmp.a.at = crate::types::hlt_dyn();
    tmp.a.size = nargs;

    let mut ctmp: vclosure = mem::zeroed();
    let mut c_ptr = c;

    if (*c).hasValue != 0 && (*(*c).t).__bindgen_anon_1.fun.as_ref().unwrap().nargs >= 0 {
        ctmp.t = (*(*c).t).__bindgen_anon_1.fun.as_ref().unwrap().parent;
        ctmp.hasValue = 0;
        ctmp.fun = (*c).fun;
        tmp.args[0] = hlp_make_dyn(
            &(*c).value as *const _ as *mut _,
            *(*ctmp.t).__bindgen_anon_1.fun.as_ref().unwrap().args
        );
        tmp.a.size += 1;
        for i in 0..nargs as usize {
            tmp.args[i + 1] = *args.add(i);
        }
        c_ptr = &mut ctmp;
    } else {
        for i in 0..nargs as usize {
            tmp.args[i] = *args.add(i);
        }
    }

    hlp_call_method(c_ptr as *mut vdynamic, &mut tmp.a)
}

#[no_mangle]
pub extern "C" fn hlp_prim_not_loaded() {
    unsafe {
        hlp_error((b"Primitive or library is missing\0").as_ptr() as *const u16);
    }
}

#[no_mangle]
pub extern "C" fn hlp_is_prim_loaded(f: *mut hl::vdynamic) -> bool {
    if f.is_null() {
        return false;
    }

    unsafe {
        let t = (*f).t;
        if (*t).kind == hl::hl_type_kind_HFUN {
            let closure = f as *mut hl::vclosure;
            (*closure).fun != hlp_prim_not_loaded as *mut c_void
        } else {
            false
        }
    }
}