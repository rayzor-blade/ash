use std::{ffi::c_void, mem, ptr};

use crate::{
    cast::{
        hlp_dyn_castd, hlp_dyn_castf, hlp_dyn_casti, hlp_dyn_casti64, hlp_dyn_castp, hlp_make_dyn,
    },
    error::hlp_error,
    hl::{
        self, hl_type, hl_type__bindgen_ty_1, hl_type_kind_HBOOL, hl_type_kind_HDYN,
        hl_type_kind_HF32, hl_type_kind_HF64, hl_type_kind_HFUN, hl_type_kind_HI32,
        hl_type_kind_HI64, hl_type_kind_HUI16, hl_type_kind_HUI8, hl_type_kind_HVOID, varray,
        vclosure, vclosure_wrapper, vdynamic,
    },
    obj::{hlp_alloc_dynamic, hlp_alloc_dynbool},
    strings::str_to_uchar_ptr,
    types::{hl_aptr, hl_is_ptr, hlp_is_dynamic, hlp_safe_cast},
};

pub type HlcFunWrapperType = unsafe extern "C" fn(*mut hl_type) -> *mut c_void;
pub type HlcStaticCallType = unsafe extern "C" fn(
    _fun: *mut c_void,
    _t: *mut hl_type,
    _args: *mut *mut c_void,
    _out: *mut vdynamic,
) -> *mut c_void;

pub unsafe extern "C" fn empty_fun_wrapper(_t: *mut hl_type) -> *mut c_void {
    ptr::null_mut()
}

pub unsafe extern "C" fn empty_static_call(
    _fun: *mut c_void,
    _t: *mut hl_type,
    _args: *mut *mut c_void,
    _out: *mut vdynamic,
) -> *mut c_void {
    ptr::null_mut()
}

/// Dynamic calls on a target with no registers to marshal into.
///
/// The native implementations place arguments in the registers a C call
/// expects and jump. WebAssembly has neither: an indirect call names a
/// signature the validator checks, so there is no way to assemble a call
/// whose shape is only known at run time.
///
/// The real answer is for the compiler to emit one trampoline per distinct
/// signature -- it knows them all -- and for this to become a lookup. Until
/// then a dynamic call fails here rather than silently returning something,
/// which costs reflection and `Reflect.callMethod`; see docs/wasm-target.md.
///
/// # Safety
/// Mirrors the native signature; touches none of its arguments.
#[cfg(not(any(target_arch = "aarch64", target_arch = "x86_64")))]
#[no_mangle]
pub unsafe extern "C" fn ash_static_call(
    fun: *mut c_void,
    t: *mut hl_type,
    args: *mut *mut c_void,
    out: *mut vdynamic,
) -> *mut c_void {
    let _ = (fun, t, args, out);
    // Returning null would be worse than failing: the caller uses the result
    // as a value and faults somewhere else, which is how this first showed up
    // -- an out-of-bounds access at 0xffffffb0, four frames below the actual
    // problem. Say what happened, where it happened.
    panic!(
        "ash: a dynamic call cannot be made on this target. WebAssembly checks \
         the signature of every indirect call, so a call whose shape is only \
         known at run time cannot be assembled; the compiler needs to emit a \
         trampoline per signature first. Reflection, Reflect.callMethod and \
         closure-taking stdlib calls such as Array.sort reach this."
    )
}

/// Dynamic function call for aarch64 — marshals args according to function type
/// and calls the function pointer. Used by hlp_call_method for dynamic dispatch.
#[cfg(target_arch = "aarch64")]
#[no_mangle]
pub unsafe extern "C" fn ash_static_call(
    fun: *mut c_void,
    t: *mut hl_type,
    args: *mut *mut c_void,
    out: *mut vdynamic,
) -> *mut c_void {
    let ft = (*t).__bindgen_anon_1.fun.as_ref().unwrap();
    let nargs = ft.nargs as usize;

    // Separate integer/pointer and float args for register allocation
    let mut ivals = [0usize; 8];
    let mut fvals = [0.0f64; 8];
    let mut ireg = 0usize;
    let mut freg = 0usize;

    for i in 0..nargs.min(8) {
        let arg_t = *ft.args.add(i);
        let kind = (*arg_t).kind;
        let p = *args.add(i);
        match kind {
            hl_type_kind_HF32 => {
                fvals[freg] = *(p as *const f32) as f64;
                freg += 1;
            }
            hl_type_kind_HF64 => {
                fvals[freg] = *(p as *const f64);
                freg += 1;
            }
            hl_type_kind_HI32 | hl_type_kind_HBOOL | hl_type_kind_HUI8 | hl_type_kind_HUI16 => {
                // p points to a stack slot containing the value as f64
                let val = (*(p as *const f64)) as i32;
                ivals[ireg] = val as i64 as usize; // sign-extend
                ireg += 1;
            }
            hl_type_kind_HI64 => {
                let val = *(p as *const i64);
                ivals[ireg] = val as usize;
                ireg += 1;
            }
            _ => {
                // Pointer types: p IS the pointer value
                ivals[ireg] = p as usize;
                ireg += 1;
            }
        }
    }

    let result: usize;
    let fresult: f64;

    core::arch::asm!(
        "blr x9",
        in("x9") fun,
        inout("x0") ivals[0] => result,
        in("x1") ivals[1],
        in("x2") ivals[2],
        in("x3") ivals[3],
        in("x4") ivals[4],
        in("x5") ivals[5],
        in("x6") ivals[6],
        in("x7") ivals[7],
        inout("d0") fvals[0] => fresult,
        in("d1") fvals[1],
        in("d2") fvals[2],
        in("d3") fvals[3],
        in("d4") fvals[4],
        in("d5") fvals[5],
        in("d6") fvals[6],
        in("d7") fvals[7],
        clobber_abi("C"),
    );

    // Handle return value
    let ret_kind = (*ft.ret).kind;
    match ret_kind {
        hl_type_kind_HVOID => ptr::null_mut(),
        hl_type_kind_HF32 => {
            (*out).v.f = fresult as f32;
            ptr::null_mut()
        }
        hl_type_kind_HF64 => {
            (*out).v.d = fresult;
            ptr::null_mut()
        }
        hl_type_kind_HI32 | hl_type_kind_HBOOL | hl_type_kind_HUI8 | hl_type_kind_HUI16 => {
            (*out).v.i = result as i32;
            ptr::null_mut()
        }
        hl_type_kind_HI64 => {
            (*out).v.i64_ = result as i64;
            ptr::null_mut()
        }
        _ => result as *mut c_void,
    }
}

/// Dynamic function call for x86_64 — marshals args per System V AMD64 ABI
/// and calls the function pointer. Used by hlp_call_method for dynamic dispatch.
///
/// System V AMD64 ABI:
///   Integer/pointer args → rdi, rsi, rdx, rcx, r8, r9  (6 regs, independent counter)
///   Float args           → xmm0–xmm7                   (8 regs, independent counter)
///   Return               → rax (integer) / xmm0 (float)
#[cfg(target_arch = "x86_64")]
#[no_mangle]
pub unsafe extern "C" fn ash_static_call(
    fun: *mut c_void,
    t: *mut hl_type,
    args: *mut *mut c_void,
    out: *mut vdynamic,
) -> *mut c_void {
    let ft = (*t).__bindgen_anon_1.fun.as_ref().unwrap();
    let nargs = ft.nargs as usize;

    // x86_64 SysV: integer and float args use independent register banks
    let mut ivals = [0usize; 6];
    let mut fvals = [0.0f64; 8];
    let mut ireg = 0usize;
    let mut freg = 0usize;

    for i in 0..nargs.min(14) {
        let arg_t = *ft.args.add(i);
        let kind = (*arg_t).kind;
        let p = *args.add(i);
        match kind {
            hl_type_kind_HF32 => {
                if freg < 8 {
                    fvals[freg] = *(p as *const f32) as f64;
                    freg += 1;
                }
            }
            hl_type_kind_HF64 => {
                if freg < 8 {
                    fvals[freg] = *(p as *const f64);
                    freg += 1;
                }
            }
            hl_type_kind_HI32 | hl_type_kind_HBOOL | hl_type_kind_HUI8 | hl_type_kind_HUI16 => {
                if ireg < 6 {
                    let val = (*(p as *const f64)) as i32;
                    ivals[ireg] = val as i64 as usize; // sign-extend
                    ireg += 1;
                }
            }
            hl_type_kind_HI64 => {
                if ireg < 6 {
                    let val = *(p as *const i64);
                    ivals[ireg] = val as usize;
                    ireg += 1;
                }
            }
            _ => {
                if ireg < 6 {
                    ivals[ireg] = p as usize;
                    ireg += 1;
                }
            }
        }
    }

    let result: usize;
    let fresult: f64;

    core::arch::asm!(
        "call r10",
        in("r10") fun,
        out("rax") result,
        in("rdi") ivals[0],
        in("rsi") ivals[1],
        in("rdx") ivals[2],
        in("rcx") ivals[3],
        in("r8") ivals[4],
        in("r9") ivals[5],
        inout("xmm0") fvals[0] => fresult,
        in("xmm1") fvals[1],
        in("xmm2") fvals[2],
        in("xmm3") fvals[3],
        in("xmm4") fvals[4],
        in("xmm5") fvals[5],
        in("xmm6") fvals[6],
        in("xmm7") fvals[7],
        clobber_abi("C"),
    );

    // Handle return value
    let ret_kind = (*ft.ret).kind;
    match ret_kind {
        hl_type_kind_HVOID => ptr::null_mut(),
        hl_type_kind_HF32 => {
            (*out).v.f = fresult as f32;
            ptr::null_mut()
        }
        hl_type_kind_HF64 => {
            (*out).v.d = fresult;
            ptr::null_mut()
        }
        hl_type_kind_HI32 | hl_type_kind_HBOOL | hl_type_kind_HUI8 | hl_type_kind_HUI16 => {
            (*out).v.i = result as i32;
            ptr::null_mut()
        }
        hl_type_kind_HI64 => {
            (*out).v.i64_ = result as i64;
            ptr::null_mut()
        }
        _ => result as *mut c_void,
    }
}

pub static mut hlc_get_wrapper: HlcFunWrapperType = empty_fun_wrapper;
pub static mut hlc_static_call: HlcStaticCallType = empty_static_call;
pub static mut hlc_call_flags: i32 = 0;

#[no_mangle]
pub unsafe extern "C" fn hl_setup_callbacks2(c: *mut c_void, w: *mut c_void, flags: i32) {
    // Ash supplies its static-call bridge but has no HashLink wrapper-code
    // generator. Keep the null-returning defaults for callbacks it omits;
    // transmuting a null pointer into a function pointer makes the next
    // HFUN-to-HFUN SafeCast jump to address zero (notably when an SDL window
    // is restored and Heaps rebuilds its event callbacks).
    hlc_get_wrapper = if w.is_null() {
        empty_fun_wrapper
    } else {
        mem::transmute::<*mut c_void, HlcFunWrapperType>(w)
    };
    hlc_static_call = if c.is_null() {
        empty_static_call
    } else {
        mem::transmute::<*mut c_void, HlcStaticCallType>(c)
    };
    hlc_call_flags = flags;
}

#[no_mangle]
pub unsafe extern "C" fn _fun_var_args() {
    hlp_error(str_to_uchar_ptr(
        "Variable fun args was not cast to typed function",
    ));
}

pub static mut fun_var_args: unsafe extern "C" fn() = _fun_var_args;

// Every closure produced by `hlp_make_var_args` retains this type pointer for
// its entire lifetime. HashLink's descriptor is static for the same reason;
// making it a function-local value leaves each closure pointing into a dead
// native stack frame, where the kind later reads as HVOID (or arbitrary data).
static mut HLT_VAR_ARGS_TYPE: hl_type = hl_type {
    kind: hl_type_kind_HFUN,
    __bindgen_anon_1: hl_type__bindgen_ty_1 {
        obj: ptr::null_mut(),
    },
    vobj_proto: ptr::null_mut(),
    mark_bits: ptr::null_mut(),
};

// HashLink represents the variadic sentinel as an HFUN whose arity is -1.
// Keeping a real descriptor is essential: generic cast code is allowed to
// inspect `closure.t->fun` before it notices the `fun_var_args` entry point.
static mut HLT_VAR_FUN: hl::hl_type_fun = hl::hl_type_fun {
    args: ptr::null_mut(),
    ret: ptr::null_mut(),
    nargs: -1,
    parent: &raw mut HLT_VAR_ARGS_TYPE,
    closure_type: hl::hl_type_fun__bindgen_ty_1 {
        kind: hl_type_kind_HFUN,
        p: ptr::null_mut(),
    },
    closure: hl::hl_type_fun__bindgen_ty_2 {
        args: ptr::null_mut(),
        ret: ptr::null_mut(),
        nargs: -1,
        parent: &raw mut HLT_VAR_ARGS_TYPE,
    },
};

#[no_mangle]
pub unsafe extern "C" fn hlp_make_fun_wrapper(v: *mut vclosure, to: *mut hl_type) -> *mut vclosure {
    let wrap = hlc_get_wrapper(to);
    if wrap.is_null() {
        return ptr::null_mut();
    }

    if (*v).fun != fun_var_args as *mut ::std::os::raw::c_void
        && (*(*v).t).__bindgen_anon_1.fun.as_ref().unwrap().nargs
            != (*to).__bindgen_anon_1.fun.as_ref().unwrap().nargs
    {
        return ptr::null_mut();
    }

    let c = crate::gc::gc_alloc(std::mem::size_of::<vclosure_wrapper>())
        .unwrap()
        .as_ptr() as *mut vclosure_wrapper;
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

#[inline]
unsafe fn resolve_closure_ptr(c: *mut vdynamic) -> *mut vclosure {
    if c.is_null() {
        return ptr::null_mut();
    }
    let mut cl = c as *mut vclosure;
    if !(*c).t.is_null() && (*(*c).t).kind == hl_type_kind_HFUN {
        let wrapped_addr = (*c).v.ptr as usize;
        if wrapped_addr >= 0x10000 && wrapped_addr.is_multiple_of(std::mem::align_of::<usize>()) {
            let wrapped = wrapped_addr as *mut vdynamic;
            // Only dereference if it's a valid GC heap pointer, not JIT code
            let gc = crate::gc::gc_locked();
            if !wrapped.is_null()
                && gc.is_gc_ptr(wrapped)
                && !(*wrapped).t.is_null()
                && (*(*wrapped).t).kind == hl_type_kind_HFUN
            {
                if env_flag!("ASH_DBG_FUN") {
                    eprintln!(
                        "[FUN] resolve_closure_ptr unwrap c={:p} -> wrapped={:p}",
                        c, wrapped
                    );
                }
                cl = wrapped as *mut vclosure;
            }
        }
    }
    cl
}

#[no_mangle]
pub unsafe fn hlp_call_method(c: *mut vdynamic, args: *mut varray) -> *mut vdynamic {
    let cl = resolve_closure_ptr(c);
    if cl.is_null() {
        return ptr::null_mut();
    }
    let vargs = hl_aptr::<*mut vdynamic>(args);
    let mut pargs: [*mut libc::c_void; HL_MAX_ARGS] = [ptr::null_mut(); HL_MAX_ARGS];
    let mut tmp: [mem::MaybeUninit<libc::c_double>; HL_MAX_ARGS] =
        unsafe { mem::MaybeUninit::uninit().assume_init() };
    let mut out: vdynamic = unsafe { mem::zeroed() };

    if (*args).size > HL_MAX_ARGS as i32 {
        hlp_error(str_to_uchar_ptr("Too many arguments"));
    }

    // Runtime metadata can retain a closure created before its target was
    // tier-compiled. Ash represents that target as the small `findex + 1`
    // sentinel; calling it through `hlc_static_call` jumps to an address such
    // as 0x12e. Bound closures already take the registered runner below.
    // Give unbound sentinel closures the same bridge so reflection and
    // dynamic calls can re-enter AIR V2 (or observe the now-compiled slot)
    // safely.
    if (*cl).hasValue == 0 && crate::fiber::is_stub_sentinel((*cl).fun as usize) {
        if let Some(runner) = crate::fiber::closure_runner() {
            let runner_args = if (*args).size == 0 {
                ptr::null_mut()
            } else {
                vargs
            };
            return runner(cl, runner_args, (*args).size);
        }
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
        let runner_args = if (*args).size == 0 {
            ptr::null_mut()
        } else {
            hl_aptr::<*mut vdynamic>(args)
        };
        if let Some(runner) = crate::fiber::closure_runner() {
            return runner(cl, runner_args, (*args).size);
        }
        if !crate::fiber::is_stub_sentinel((*cl).fun as usize) {
            return crate::fiber::hlp_jit_closure_runner(cl, runner_args, (*args).size);
        }
        hlp_error(str_to_uchar_ptr("Can't call closure with value"));
    }

    if (*args).size < (*(*cl).t).__bindgen_anon_1.fun.as_ref().unwrap().nargs {
        hlp_error(str_to_uchar_ptr(&format!(
            "Missing arguments : {} expected but {} passed",
            (*(*cl).t).__bindgen_anon_1.fun.as_ref().unwrap().nargs,
            (*args).size
        )));
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
        let t = *(*(*cl).t)
            .__bindgen_anon_1
            .fun
            .as_ref()
            .unwrap()
            .args
            .add(i);
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
                    tmp[i] = mem::MaybeUninit::new(hlp_dyn_casti(
                        (vargs.add(i) as *mut vdynamic) as *mut c_void,
                        _hlt_dyn,
                        t,
                    ) as f64);
                    p = tmp[i].as_mut_ptr() as *mut libc::c_void;
                }
                hl_type_kind_HI64 => {
                    tmp[i] = mem::MaybeUninit::new(hlp_dyn_casti64(
                        (vargs.add(i) as *mut vdynamic) as *mut c_void,
                        _hlt_dyn,
                    ) as f64);
                    p = tmp[i].as_mut_ptr() as *mut libc::c_void;
                }
                hl_type_kind_HF32 => {
                    tmp[i] = mem::MaybeUninit::new(hlp_dyn_castf(
                        (vargs.add(i) as *mut vdynamic) as *mut c_void,
                        _hlt_dyn,
                    ) as f64);
                    p = tmp[i].as_mut_ptr() as *mut libc::c_void;
                }
                hl_type_kind_HF64 => {
                    tmp[i] = mem::MaybeUninit::new(hlp_dyn_castd(
                        (vargs.add(i) as *mut vdynamic) as *mut c_void,
                        _hlt_dyn,
                    ));
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
        if hlc_call_flags & 1 != 0 {
            &(*cl).fun as *const _ as *mut _
        } else {
            (*cl).fun
        },
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
    let ft = (*t)
        .__bindgen_anon_1
        .fun
        .as_mut()
        .expect("Type is not a function");

    if ft.closure_type.kind != hl_type_kind_HFUN {
        // Already stripped: a closure type has no `this` left to remove, so
        // stripping again is a no-op rather than an error. Upstream asserts
        // here because its callers only ever pass full method types; ash has
        // one more caller than upstream does. hlp_alloc_closure_ptr now
        // stores the stripped type on the closure (so a method closure is not
        // one argument too wide), and the JIT hands that same type back to
        // this function when it lowers InstanceClosure/VirtualClosure —
        // which turned a 1-arg method into a 0-arg type and aborted the VM
        // on test_stdlib under --mode jit.
        if ft.nargs == 0 {
            return t;
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

    &mut ft.closure_type as *mut _ as *mut hl_type
}

#[no_mangle]
pub unsafe extern "C" fn hlp_alloc_closure_void(
    t: *mut hl_type,
    fvalue: *mut libc::c_void,
) -> *mut vclosure {
    let size = mem::size_of::<vclosure>();

    let c_ptr = crate::gc::gc_alloc(size)
        .expect("Failed to allocate memory for closure")
        .as_ptr() as *mut vclosure;

    ptr::write(
        c_ptr,
        crate::types::vclosure_new_with_stack(t, fvalue, 0, ptr::null_mut(), 0),
    );

    c_ptr
}

#[no_mangle]
pub unsafe extern "C" fn hlp_alloc_closure_ptr(
    t: *mut hl_type,
    fun: *mut std::ffi::c_void,
    ptr: *mut std::ffi::c_void,
) -> *mut vclosure {
    // Upstream hl_alloc_closure_ptr stores hl_get_closure_type(fullt) — the
    // method's type WITHOUT `this`, whose fun->parent points back at the
    // full type. Storing the full type instead made every dynamically
    // fetched method closure look one arg wider than its call site's
    // declared closure type, so a plain `untyped obj.method(...)` chain
    // died in invalid_cast ("Can't cast (Content (Content,String,dynamic))
    // to (... (String,dynamic))", unit suite Issue5082) — and
    // hlp_dyn_call's parent-based this-boxing could never fire.
    // A zero-arg fun type has no `this` to strip — some of our internal
    // callers (constants init) build value-carrying closures over plain
    // functions, a shape upstream never feeds this path. Keep their type
    // untouched rather than assert like hl_get_closure_type does.
    let t = {
        let ft = (*t).__bindgen_anon_1.fun.as_ref();
        match ft {
            Some(f) if f.nargs > 0 => hlp_get_closure_type(t),
            _ => t,
        }
    };
    let mut gc = crate::gc::gc_locked();

    let c_ptr = gc.allocate_closure_ptr(t, fun, ptr);

    ptr::write(
        c_ptr,
        crate::types::vclosure_new_with_stack(t, fun, 1, ptr, 0),
    );

    c_ptr
}

#[no_mangle]
pub unsafe extern "C" fn hlp_no_closure(c: *mut vdynamic) -> *mut vdynamic {
    let cl = resolve_closure_ptr(c);
    if cl.is_null() {
        return ptr::null_mut();
    }
    if env_flag!("ASH_DBG_FUN") {
        let tk = if (*cl).t.is_null() {
            0
        } else {
            (*(*cl).t).kind
        };
        let pk = if (*cl).t.is_null() {
            0
        } else {
            let p = (*(*cl).t).__bindgen_anon_1.fun.as_ref().unwrap().parent;
            if p.is_null() {
                0
            } else {
                (*p).kind
            }
        };
        eprintln!(
            "[FUN] no_closure in c={:p} cl={:p} hasValue={} t={:p} tk={} parent_kind={} fun={:p}",
            c,
            cl,
            (*cl).hasValue,
            (*cl).t,
            tk,
            pk,
            (*cl).fun
        );
    }
    if (*cl).hasValue == 0 {
        return cl as *mut vdynamic;
    }
    if (*cl).hasValue == 2 {
        let wrapper = cl as *mut vclosure_wrapper;
        return hlp_no_closure((*wrapper).wrappedFun as *mut vdynamic);
    }
    let parent = (*(*cl).t).__bindgen_anon_1.fun.as_ref().unwrap().parent;
    let out_t = if parent.is_null() { (*cl).t } else { parent };
    let out = hlp_alloc_closure_void(out_t, (*cl).fun) as *mut vdynamic;
    if env_flag!("ASH_DBG_FUN") {
        let tk = if out.is_null() || (*out).t.is_null() {
            0
        } else {
            (*(*out).t).kind
        };
        eprintln!(
            "[FUN] no_closure out={:p} out_t={:p} tk={}",
            out,
            if out.is_null() {
                ptr::null_mut()
            } else {
                (*out).t
            },
            tk
        );
    }
    out
}

#[no_mangle]
pub unsafe extern "C" fn hlp_make_closure(c: *mut vdynamic, v: *mut vdynamic) -> *mut vdynamic {
    let cl = resolve_closure_ptr(c);
    if cl.is_null() {
        return ptr::null_mut();
    }
    let t = if (*cl).hasValue != 0 {
        (*(*cl).t).__bindgen_anon_1.fun.as_ref().unwrap().parent
    } else {
        (*cl).t
    };

    if (*cl).hasValue == 2 {
        let wrapper = cl as *mut vclosure_wrapper;
        return hlp_make_closure((*wrapper).wrappedFun as *mut vdynamic, v);
    }

    if (*t).__bindgen_anon_1.fun.as_ref().unwrap().nargs == 0
        || v.is_null()
        || !hlp_safe_cast((*v).t, *(*t).__bindgen_anon_1.fun.as_ref().unwrap().args)
    {
        return ptr::null_mut();
    }

    hlp_alloc_closure_ptr(t, (*cl).fun, v as *mut libc::c_void) as *mut vdynamic
}

#[no_mangle]
pub unsafe extern "C" fn hlp_get_closure_value(c: *mut vdynamic) -> *mut vdynamic {
    let cl = resolve_closure_ptr(c);
    if cl.is_null() {
        return ptr::null_mut();
    }
    if (*cl).hasValue == 0 {
        return ptr::null_mut();
    }
    if (*cl).hasValue == 2 {
        let wrapper = cl as *mut vclosure_wrapper;
        return hlp_get_closure_value((*wrapper).wrappedFun as *mut vdynamic);
    }
    if (*cl).fun == fun_var_args as *mut libc::c_void {
        return ptr::null_mut();
    }
    hlp_make_dyn(
        &(*cl).value as *const _ as *mut libc::c_void,
        *(*(*(*cl).t).__bindgen_anon_1.fun.as_ref().unwrap().parent)
            .__bindgen_anon_1
            .fun
            .as_ref()
            .unwrap()
            .args,
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
    HLT_VAR_FUN.ret = crate::types::hlt_void();
    HLT_VAR_FUN.closure.ret = crate::types::hlt_void();
    HLT_VAR_FUN.closure_type.p = &raw mut HLT_VAR_FUN as *mut c_void;
    HLT_VAR_ARGS_TYPE.__bindgen_anon_1.fun = &raw mut HLT_VAR_FUN;

    // Allocate and initialize the closure
    let closure = hlp_alloc_closure_ptr(
        &raw mut HLT_VAR_ARGS_TYPE,
        fun_var_args as *mut _,
        c as *mut _,
    );

    // Cast the closure to vdynamic and return
    closure as *mut vdynamic
}

#[no_mangle]
pub unsafe extern "C" fn hlp_dyn_call(
    c: *mut vclosure,
    args: *mut *mut vdynamic,
    nargs: i32,
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

    // A varargs closure deliberately uses a sentinel HFUN type without an
    // `hl_type_fun`; `hlp_call_method` recognizes its function pointer and
    // packs the incoming arguments for the wrapped closure. Do not try to
    // synthesize a full bound-method signature from that sentinel here.
    if (*c).hasValue != 0 && (*c).fun != fun_var_args as *mut libc::c_void {
        let Some(closure_fun) = (*(*c).t).__bindgen_anon_1.fun.as_ref() else {
            hlp_error(str_to_uchar_ptr("Closure has no function type"));
            return ptr::null_mut();
        };
        ctmp.t = closure_fun.parent;
        if ctmp.t.is_null() {
            hlp_error(str_to_uchar_ptr("Bound closure has no parent type"));
            return ptr::null_mut();
        }
        ctmp.hasValue = 0;
        ctmp.fun = (*c).fun;
        tmp.args[0] = hlp_make_dyn(
            &(*c).value as *const _ as *mut _,
            *(*ctmp.t).__bindgen_anon_1.fun.as_ref().unwrap().args,
        );
        tmp.a.size += 1;
        for (i, slot) in tmp.args.iter_mut().skip(1).take(nargs as usize).enumerate() {
            *slot = *args.add(i);
        }
        c_ptr = &mut ctmp;
    } else {
        for (i, slot) in tmp.args.iter_mut().take(nargs as usize).enumerate() {
            *slot = *args.add(i);
        }
    }

    hlp_call_method(c_ptr as *mut vdynamic, &mut tmp.a)
}

#[no_mangle]
pub extern "C" fn hlp_prim_not_loaded() {
    unsafe {
        hlp_error(c"Primitive or library is missing".as_ptr() as *const u16);
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

/// Install the dynamic-dispatch callbacks, without the caller naming them.
///
/// `hl_setup_callbacks2` takes `ash_static_call`'s ADDRESS, and an AOT object
/// linked against the runtime as a shared library cannot take the address of
/// one of its functions directly -- Mach-O arm64 addresses it with adrp/add
/// and the link fails with "does not have address". Calling a function works
/// (the linker makes a stub); taking its address does not. So the address is
/// taken HERE, inside the library that owns it.
#[no_mangle]
pub unsafe extern "C" fn hlp_install_static_call() {
    hl_setup_callbacks2(
        ash_static_call as *mut std::ffi::c_void,
        std::ptr::null_mut(),
        0,
    );
}

/// Install the closure runner, without the caller naming it.
///
/// Same reason as [`hlp_install_static_call`]: this is installed BY ADDRESS,
/// and a shared-runtime AOT object cannot take the address of one of the
/// library's functions. Taking it here keeps the object to calls only.
#[no_mangle]
pub unsafe extern "C" fn hlp_install_closure_runner() {
    crate::fiber::hlp_set_closure_runner(crate::fiber::hlp_jit_closure_runner);
}
