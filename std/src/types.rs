use std::{ffi::c_void, mem, ptr, sync::OnceLock};

use crate::{
    array::hlp_alloc_array,
    gc::{hlp_mark_size, hlp_zalloc, GC},
    hl::{
        self, hl_module_context, hl_type, hl_type__bindgen_ty_1, hl_type_kind_HABSTRACT,
        hl_type_kind_HARRAY, hl_type_kind_HBYTES, hl_type_kind_HDYN, hl_type_kind_HDYNOBJ,
        hl_type_kind_HENUM, hl_type_kind_HOBJ, hl_type_kind_HSTRUCT, varray, vbyte, vdynamic,
        venum,
    },
};

/// Returns a persistent `*mut hl_type` for a given type kind.
/// Uses `Box::leak` so the pointer is valid for the program's lifetime.
/// Used to avoid storing stack-local `hl_type` pointers into heap-allocated objects.
fn persistent_type(kind: u32) -> *mut hl_type {
    Box::leak(Box::new(hl_type {
        kind,
        __bindgen_anon_1: hl_type__bindgen_ty_1 {
            obj: ptr::null_mut(),
        },
        vobj_proto: ptr::null_mut(),
        mark_bits: ptr::null_mut(),
    }))
}

/// Persistent type singleton for HDYNOBJ.
pub fn hlt_dynobj() -> *mut hl_type {
    static CELL: OnceLock<usize> = OnceLock::new();
    *CELL.get_or_init(|| persistent_type(hl_type_kind_HDYNOBJ) as usize) as *mut hl_type
}

/// Persistent type singleton for HARRAY.
pub fn hlt_array() -> *mut hl_type {
    static CELL: OnceLock<usize> = OnceLock::new();
    *CELL.get_or_init(|| persistent_type(hl_type_kind_HARRAY) as usize) as *mut hl_type
}

/// Persistent type singleton for HBYTES.
pub fn hlt_bytes() -> *mut hl_type {
    static CELL: OnceLock<usize> = OnceLock::new();
    *CELL.get_or_init(|| persistent_type(hl_type_kind_HBYTES) as usize) as *mut hl_type
}

/// Persistent type singleton for HDYN.
pub fn hlt_dyn() -> *mut hl_type {
    static CELL: OnceLock<usize> = OnceLock::new();
    *CELL.get_or_init(|| persistent_type(hl_type_kind_HDYN) as usize) as *mut hl_type
}

pub static TSTR: [&'static str; 22] = [
    "void", "i8", "i16", "i32", "i64", "f32", "f64", "bool", "bytes", "dynamic", "null", "array",
    "type", "null", "null", "dynobj", "null", "null", "null", "null", "null", "null",
];

#[inline]
pub unsafe fn hl_aptr<T>(a: *mut varray) -> *mut T {
    (a as *mut u8).add(mem::size_of::<varray>()) as *mut T
}

#[inline]
pub unsafe fn hl_is_ptr(t: *mut hl_type) -> bool {
    (*t).kind >= hl_type_kind_HBYTES
}

pub const HL_WSIZE: isize = 8;

pub static T_SIZES: [isize; 23] = [
    0,        // VOID
    1,        // I8
    2,        // I16
    4,        // I32
    8,        // I64
    4,        // F32
    8,        // F64
    2,        // BOOL
    HL_WSIZE, // BYTES
    HL_WSIZE, // DYN
    HL_WSIZE, // FUN
    HL_WSIZE, // OBJ
    HL_WSIZE, // ARRAY
    HL_WSIZE, // TYPE
    HL_WSIZE, // REF
    HL_WSIZE, // VIRTUAL
    HL_WSIZE, // DYNOBJ
    HL_WSIZE, // ABSTRACT
    HL_WSIZE, // ENUM
    HL_WSIZE, // NULL
    HL_WSIZE, // METHOD
    HL_WSIZE, // STRUCT
    0,        // PACKED
];

#[no_mangle]
pub unsafe extern "C" fn hlp_type_size(t: *mut hl_type) -> isize {
    return T_SIZES[(*t).kind as usize];
}

#[no_mangle]
pub unsafe extern "C" fn hlp_pad_struct(size: i32, t: *mut hl_type) -> i32 {
    let align = match (*t).kind {
        hl::hl_type_kind_HVOID => return 0,
        hl::hl_type_kind_HUI8 => mem::align_of::<u8>(),
        hl::hl_type_kind_HUI16 => mem::align_of::<u16>(),
        hl::hl_type_kind_HI32 => mem::align_of::<i32>(),
        hl::hl_type_kind_HI64 => mem::align_of::<i64>(),
        hl::hl_type_kind_HF32 => mem::align_of::<f32>(),
        hl::hl_type_kind_HF64 => mem::align_of::<f64>(),
        hl::hl_type_kind_HBOOL => mem::align_of::<bool>(),
        _ => mem::size_of::<*mut std::os::raw::c_void>(),
    };

    (-(size as isize) & (align as isize - 1)) as i32
}

#[no_mangle]
pub unsafe extern "C" fn hlp_same_type(a: *mut hl::hl_type, b: *mut hl::hl_type) -> bool {
    if a == b {
        return true;
    }

    if (*a).kind != (*b).kind {
        return false;
    }

    match (*a).kind {
        hl::hl_type_kind_HVOID
        | hl::hl_type_kind_HUI8
        | hl::hl_type_kind_HUI16
        | hl::hl_type_kind_HI32
        | hl::hl_type_kind_HI64
        | hl::hl_type_kind_HF32
        | hl::hl_type_kind_HF64
        | hl::hl_type_kind_HBOOL
        | hl::hl_type_kind_HTYPE
        | hl::hl_type_kind_HBYTES
        | hl::hl_type_kind_HDYN
        | hl::hl_type_kind_HARRAY
        | hl::hl_type_kind_HDYNOBJ => true,

        hl::hl_type_kind_HREF | hl::hl_type_kind_HNULL | hl::hl_type_kind_HPACKED => {
            hlp_same_type((*a).__bindgen_anon_1.tparam, (*b).__bindgen_anon_1.tparam)
        }

        hl::hl_type_kind_HFUN | hl::hl_type_kind_HMETHOD => {
            let fun_a = &*(*a).__bindgen_anon_1.fun;
            let fun_b = &*(*b).__bindgen_anon_1.fun;

            if fun_a.nargs != fun_b.nargs {
                return false;
            }

            for i in 0..fun_a.nargs as usize {
                if !hlp_same_type(*fun_a.args.add(i), *fun_b.args.add(i)) {
                    return false;
                }
            }

            hlp_same_type(fun_a.ret, fun_b.ret)
        }

        hl::hl_type_kind_HOBJ | hl::hl_type_kind_HSTRUCT => {
            (*a).__bindgen_anon_1.obj == (*b).__bindgen_anon_1.obj
        }

        hl::hl_type_kind_HVIRTUAL => (*a).__bindgen_anon_1.virt == (*b).__bindgen_anon_1.virt,

        hl::hl_type_kind_HABSTRACT => {
            (*a).__bindgen_anon_1.abs_name == (*b).__bindgen_anon_1.abs_name
        }

        hl::hl_type_kind_HENUM => (*a).__bindgen_anon_1.tenum == (*b).__bindgen_anon_1.tenum,

        _ => false,
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_safe_cast(t: *mut hl::hl_type, to: *mut hl::hl_type) -> bool {
    if t == to {
        return true;
    }
    if (*to).kind == hl::hl_type_kind_HDYN {
        return hlp_is_dynamic(t);
    }
    if (*t).kind != (*to).kind {
        return false;
    }
    match (*t).kind {
        hl::hl_type_kind_HVIRTUAL => {
            let t_virt = *(*t).__bindgen_anon_1.virt;
            let to_virt = *(*to).__bindgen_anon_1.virt;
            if to_virt.nfields < t_virt.nfields {
                for i in 0..to_virt.nfields as usize {
                    let f1 = t_virt.fields.add(i);
                    let f2 = to_virt.fields.add(i);
                    if (*f1).hashed_name != (*f2).hashed_name || !hlp_same_type((*f1).t, (*f2).t) {
                        return false;
                    }
                }
                return true;
            }
        }
        hl::hl_type_kind_HOBJ | hl::hl_type_kind_HSTRUCT => {
            let mut o = (*t).__bindgen_anon_1.obj;
            let oto = (*to).__bindgen_anon_1.obj;
            loop {
                if o == oto {
                    return true;
                }
                if (*o).super_.is_null() {
                    return false;
                }
                o = (*(*o).super_).__bindgen_anon_1.obj;
            }
        }
        hl::hl_type_kind_HFUN | hl::hl_type_kind_HMETHOD => {
            let t_fun = *(*t).__bindgen_anon_1.fun;
            let to_fun = *(*to).__bindgen_anon_1.fun;
            if t_fun.nargs == to_fun.nargs {
                if !hlp_safe_cast(t_fun.ret, to_fun.ret) {
                    return false;
                }
                for i in 0..t_fun.nargs as usize {
                    let t1 = *t_fun.args.add(i);
                    let t2 = *to_fun.args.add(i);
                    if !hlp_safe_cast(t2, t1)
                        && ((*t1).kind != hl::hl_type_kind_HDYN || !hlp_is_dynamic(t2))
                    {
                        return false;
                    }
                }
                return true;
            }
        }
        hl::hl_type_kind_HPACKED => {
            return hlp_safe_cast((*t).__bindgen_anon_1.tparam, to);
        }
        _ => {}
    }
    hlp_same_type(t, to)
}

static T_IS_DYNAMIC: [bool; 23] = [
    false, // HVOID
    false, // HUI8
    false, // HUI16
    false, // HI32
    false, // HI64
    false, // HF32
    false, // HF64
    false, // HBOOL
    false, // HBYTES
    true,  // HDYN
    true,  // HFUN
    true,  // HOBJ
    true,  // HARRAY
    false, // HTYPE
    false, // HREF
    true,  // HVIRTUAL
    true,  // HDYNOBJ
    false, // HABSTRACT
    true,  // HENUM
    true,  // HNULL
    false, // HMETHOD
    false, // HSTRUCT
    false, // HPACKED
];

#[no_mangle]
pub unsafe extern "C" fn hlp_is_dynamic(t: *const hl::hl_type) -> bool {
    T_IS_DYNAMIC[(*t).kind as usize]
}

#[no_mangle]
pub unsafe extern "C" fn hlp_type_name(t: *const hl::hl_type) -> *mut vbyte {
    match (*t).kind {
        hl_type_kind_HOBJ | hl_type_kind_HSTRUCT => (*(*t).__bindgen_anon_1.obj).name as *mut vbyte,
        hl_type_kind_HENUM => (*(*t).__bindgen_anon_1.tenum).name as *mut vbyte,
        hl_type_kind_HABSTRACT => (*t).__bindgen_anon_1.abs_name as *mut vbyte,
        _ => std::ptr::null_mut(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_type_set_global(t: *const hl::hl_type, v: *mut vdynamic) -> bool {
    match (*t).kind {
        hl_type_kind_HOBJ | hl_type_kind_HSTRUCT => {
            (*(*(*t).__bindgen_anon_1.obj).global_value) = v as *mut c_void;
            true
        }
        hl_type_kind_HENUM => {
            (*(*(*t).__bindgen_anon_1.tenum).global_value) = v as *mut c_void;
            true
        }

        _ => false,
    }
}

#[no_mangle]
pub extern "C" fn hlp_type_get_global(t: *mut hl::hl_type) -> *mut hl::vdynamic {
    if t.is_null() {
        return std::ptr::null_mut();
    }

    unsafe {
        match (*t).kind {
            hl::hl_type_kind_HOBJ | hl::hl_type_kind_HSTRUCT => {
                let obj = (*t).__bindgen_anon_1.obj;
                if !obj.is_null() && !(*obj).global_value.is_null() {
                    *((*obj).global_value as *mut *mut hl::vdynamic)
                } else {
                    std::ptr::null_mut()
                }
            }
            hl::hl_type_kind_HENUM => {
                let tenum = (*t).__bindgen_anon_1.tenum;
                if !tenum.is_null() && !(*tenum).global_value.is_null() {
                    *((*tenum).global_value as *mut *mut hl::vdynamic)
                } else {
                    std::ptr::null_mut()
                }
            }
            _ => std::ptr::null_mut(),
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_alloc_enum(t: *mut hl_type, index: i32) -> *mut venum {
    let gc = GC.get_mut().expect("Expected to get GC");

    let tenum = (*t).__bindgen_anon_1.tenum;
    if tenum.is_null() {
        return ptr::null_mut();
    }

    let construct = (*tenum).constructs.offset(index as isize);
    if construct.is_null() {
        return ptr::null_mut();
    }

    let size = (*construct).size as usize;
    let has_ptr = (*construct).hasptr;

    // Allocate memory
    let ptr = gc
        .allocate(std::mem::size_of::<hl::venum>() + size)
        .expect("Out of memory");

    // Initialize the enum
    let v = ptr.as_ptr() as *mut hl::venum;
    (*v).t = t;
    (*v).index = index;

    // Zero-initialize the rest of the memory if needed
    if has_ptr {
        std::ptr::write_bytes(v.offset(1) as *mut u8, 0, size);
    }

    v
}

#[no_mangle]
pub unsafe extern "C" fn hlp_type_enum_fields(t: *mut hl::hl_type) -> *mut varray {
    let _hlt_bytes: *mut hl_type = &mut hl_type {
        kind: hl_type_kind_HBYTES,
        __bindgen_anon_1: hl_type__bindgen_ty_1 {
            obj: ptr::null_mut(),
        },
        vobj_proto: ptr::null_mut(),
        mark_bits: ptr::null_mut(),
    };

    let array = hlp_alloc_array(_hlt_bytes, (*(*t).__bindgen_anon_1.tenum).nconstructs);

    for i in 0..(*(*t).__bindgen_anon_1.tenum).nconstructs as usize {
        *(hl_aptr::<*mut vbyte>(array).add(i)) =
            (*(*t).__bindgen_anon_1.tenum).name as *mut vbyte;
    }

    array
}

#[no_mangle]
pub unsafe extern "C" fn hlp_type_enum_values(t: *mut hl::hl_type) -> *mut varray {
    let _hlt_dyn: *mut hl_type = &mut hl_type {
        kind: hl_type_kind_HDYN,
        __bindgen_anon_1: hl_type__bindgen_ty_1 {
            obj: ptr::null_mut(),
        },
        vobj_proto: ptr::null_mut(),
        mark_bits: ptr::null_mut(),
    };

    let tenum = (*t).__bindgen_anon_1.tenum;
    let nconstructs = (*tenum).nconstructs;
    let array = hlp_alloc_array(_hlt_dyn, nconstructs);

    for i in 0..nconstructs as usize {
        let e = hlp_alloc_enum(t, i as i32);
        *(hl_aptr::<*mut venum>(array).add(i)) = e;
    }

    array
}

#[no_mangle]
pub extern "C" fn hlp_mem_compact(_d:*mut vdynamic, _exclude: *mut varray, _flags:*const i32, _out_count:*mut i32) -> *mut vdynamic {
    unimplemented!()
}


#[no_mangle]
pub unsafe extern "C" fn hlp_init_enum(et: *mut hl_type, _m: *mut hl_module_context) {
    let tenum = (*et).__bindgen_anon_1.tenum;
    if tenum.is_null() {
        return;
    }

    let mut mark_size = 0;
    let constructs = std::slice::from_raw_parts_mut((*tenum).constructs, (*tenum).nconstructs as usize);

    for (i, c) in constructs.iter_mut().enumerate() {
        c.hasptr = false;
        c.size = std::mem::size_of::<*mut std::os::raw::c_void>() as i32 + std::mem::size_of::<i32>() as i32; // t + index

        let params = std::slice::from_raw_parts_mut(c.params, c.nparams as usize);
        let offsets = std::slice::from_raw_parts_mut(c.offsets, c.nparams as usize);

        for (j, &param) in params.iter().enumerate() {
            c.size += hlp_pad_struct(c.size, param);
            offsets[j] = c.size;
            if hl_is_ptr(param) {
                c.hasptr = true;
            }
            c.size += hlp_type_size(param) as i32;
        }

        if c.hasptr {
            let max_pos = i as i32 * std::mem::size_of::<i32>() as i32 + hlp_mark_size(c.size - (HL_WSIZE * 2) as i32);
            if max_pos > mark_size {
                mark_size = max_pos;
            }
        }
    }

    let mark = hlp_zalloc( mark_size) as *mut u32;
    if mark.is_null() {
        return;
    }

    for (i, c) in constructs.iter().enumerate() {
        if !c.hasptr {
            continue;
        }

        let params = std::slice::from_raw_parts(c.params, c.nparams as usize);
        let offsets = std::slice::from_raw_parts(c.offsets, c.nparams as usize);

        for (j, (&param, &offset)) in params.iter().zip(offsets.iter()).enumerate() {
            if hl_is_ptr(param) {
                let pos = (offset / HL_WSIZE as i32) - 2;
                *mark.add(i + (pos as usize >> 5)) |= 1 << (pos & 31);
            }
        }
    }

    (*et).mark_bits = mark as *mut ::std::os::raw::c_uint;
}