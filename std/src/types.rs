use std::{ffi::c_void, mem, ptr, sync::OnceLock};

use crate::{
    array::hlp_alloc_array,
    gc::{hlp_mark_size, hlp_zalloc},
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
// `hl::hl_type_kind`, not u32: bindgen maps the C enum to i32 under MSVC and
// u32 under clang, so any explicit width compiles on exactly one platform.
fn persistent_type(kind: hl::hl_type_kind) -> *mut hl_type {
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

/// Persistent type singleton for HTYPE.
pub fn hlt_type() -> *mut hl_type {
    static CELL: OnceLock<usize> = OnceLock::new();
    *CELL.get_or_init(|| persistent_type(hl::hl_type_kind_HTYPE) as usize) as *mut hl_type
}

/// Persistent type singleton for HDYN.
pub fn hlt_dyn() -> *mut hl_type {
    static CELL: OnceLock<usize> = OnceLock::new();
    *CELL.get_or_init(|| persistent_type(hl_type_kind_HDYN) as usize) as *mut hl_type
}

/// Persistent type singleton for HI32.
pub fn hlt_i32() -> *mut hl_type {
    static CELL: OnceLock<usize> = OnceLock::new();
    *CELL.get_or_init(|| persistent_type(hl::hl_type_kind_HI32) as usize) as *mut hl_type
}

/// Persistent type singleton for HF64.
pub fn hlt_f64() -> *mut hl_type {
    static CELL: OnceLock<usize> = OnceLock::new();
    *CELL.get_or_init(|| persistent_type(hl::hl_type_kind_HF64) as usize) as *mut hl_type
}

/// Persistent type singleton for HF32.
pub fn hlt_f32() -> *mut hl_type {
    static CELL: OnceLock<usize> = OnceLock::new();
    *CELL.get_or_init(|| persistent_type(hl::hl_type_kind_HF32) as usize) as *mut hl_type
}

/// Persistent type singleton for HI64.
pub fn hlt_i64() -> *mut hl_type {
    static CELL: OnceLock<usize> = OnceLock::new();
    *CELL.get_or_init(|| persistent_type(hl::hl_type_kind_HI64) as usize) as *mut hl_type
}

/// Persistent type singleton for HBOOL.
pub fn hlt_bool() -> *mut hl_type {
    static CELL: OnceLock<usize> = OnceLock::new();
    *CELL.get_or_init(|| persistent_type(hl::hl_type_kind_HBOOL) as usize) as *mut hl_type
}

/// Persistent type singleton for HVOID (upstream &hlt_void).
pub fn hlt_void() -> *mut hl_type {
    static CELL: OnceLock<usize> = OnceLock::new();
    *CELL.get_or_init(|| persistent_type(hl::hl_type_kind_HVOID) as usize) as *mut hl_type
}

// Indexed by hl_type_kind; "null" is the marker for kinds rendered
// recursively by hlp_type_str_rec (fun/obj/ref/virtual/...). This table had
// ONE slot missing after "dynamic", shifting every name from HOBJ onward:
// cast errors printed "array" for objects and "dynobj" for virtuals, which
// sent the Issue5082 diagnosis chasing phantom array types. Kind count is
// 23 (through HPACKED) — the old 22-entry table was also an
// out-of-bounds panic waiting on any packed type's name.
pub static TSTR: [&str; 23] = [
    "void", "i8", "i16", "i32", "i64", "f32", "f64", "bool", "bytes", "dynamic", "null", "null",
    "array", "type", "null", "null", "dynobj", "null", "null", "null", "null", "null", "null",
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
    T_SIZES[(*t).kind as usize]
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
    if t.is_null() || to.is_null() {
        return false;
    }
    if (t as usize) < 0x10000 || (to as usize) < 0x10000 {
        return false;
    }
    if (*t).kind > 22 || (*to).kind > 22 {
        return false;
    }
    if t == to {
        return true;
    }
    if (*to).kind == hl::hl_type_kind_HDYN {
        return hlp_is_dynamic(t);
    }
    // HFUN and HMETHOD are interchangeable for safe casting
    let t_kind = (*t).kind;
    let to_kind = (*to).kind;
    if t_kind != to_kind {
        let fun_kinds = [hl::hl_type_kind_HFUN, hl::hl_type_kind_HMETHOD];
        if !(fun_kinds.contains(&t_kind) && fun_kinds.contains(&to_kind)) {
            return false;
        }
    }
    match (*t).kind {
        hl::hl_type_kind_HVIRTUAL => {
            let t_virt = *(*t).__bindgen_anon_1.virt;
            let to_virt = *(*to).__bindgen_anon_1.virt;
            if to_virt.nfields <= t_virt.nfields {
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
            let o_init = (*t).__bindgen_anon_1.obj;
            let oto = (*to).__bindgen_anon_1.obj;
            if env_flag!("ASH_DBG_SC") {
                eprintln!(
                    "[safe_cast] t={:p} k={} obj={:p} to={:p} k={} oto={:p}",
                    t,
                    (*t).kind,
                    o_init,
                    to,
                    (*to).kind,
                    oto
                );
            }
            if o_init.is_null()
                || oto.is_null()
                || (o_init as usize) < 0x10000
                || (oto as usize) < 0x10000
                || !(o_init as usize).is_multiple_of(std::mem::align_of::<usize>())
                || !(oto as usize).is_multiple_of(std::mem::align_of::<usize>())
            {
                return false;
            }
            let mut o = o_init;
            loop {
                if o == oto {
                    return true;
                }
                if (*o).super_.is_null() {
                    return false;
                }
                let sup = (*o).super_;
                if (sup as usize) < 0x10000 || !(sup as usize).is_multiple_of(std::mem::align_of::<usize>()) {
                    return false;
                }
                if (*sup).kind != hl::hl_type_kind_HOBJ && (*sup).kind != hl::hl_type_kind_HSTRUCT {
                    return false;
                }
                let sup_obj = (*sup).__bindgen_anon_1.obj;
                if sup_obj.is_null()
                    || (sup_obj as usize) < 0x10000
                    || !(sup_obj as usize).is_multiple_of(std::mem::align_of::<usize>())
                {
                    return false;
                }
                o = sup_obj;
            }
        }
        hl::hl_type_kind_HFUN | hl::hl_type_kind_HMETHOD => {
            let t_fun = *(*t).__bindgen_anon_1.fun;
            let to_fun = *(*to).__bindgen_anon_1.fun;
            if t_fun.nargs == to_fun.nargs {
                let ret_ok = hlp_safe_cast(t_fun.ret, to_fun.ret);
                if !ret_ok {
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
    if t.is_null() {
        return false;
    }
    let kind = (*t).kind as usize;
    if kind >= T_IS_DYNAMIC.len() {
        return false;
    }
    T_IS_DYNAMIC[kind]
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

/// Names registered for GUID values, keyed by the guid itself.
///
/// Owned by ash, not by the collector: upstream parks the names in a GC'd
/// int64 map because it hands the `vbyte*` straight back out, and a raw GC
/// pointer sitting in a Rust container is a root the collector cannot see.
/// Copying the characters in sidesteps the whole question.
static GUID_NAMES: OnceLock<std::sync::Mutex<std::collections::HashMap<i64, Box<[u16]>>>> =
    OnceLock::new();

// DEFINE_PRIM(_VOID, register_guid_name, _I64 _BYTES)
//
// Upstream's only reader is `hl_guid_str`, which prints a registered name in
// place of the base64 of a guid's bits. ash's kind table stops at HPACKED —
// there is no HGUID to render — so nothing reads this back yet. Retained
// anyway: dropping a name the program handed over would make the day someone
// adds the reader look like a registration bug. A null name deregisters,
// matching upstream's hi64remove branch.
#[no_mangle]
pub unsafe extern "C" fn hlp_register_guid_name(guid: i64, name: *mut vbyte) {
    let names = GUID_NAMES.get_or_init(Default::default);
    let mut names = names.lock().unwrap_or_else(|e| e.into_inner());
    if name.is_null() {
        names.remove(&guid);
        return;
    }
    let s = name as *const u16;
    let len = crate::hl_compat::ustrlen(s);
    names.insert(guid, std::slice::from_raw_parts(s, len).into());
}

#[no_mangle]
pub unsafe extern "C" fn hlp_type_args_count(t: *mut hl::hl_type) -> i32 {
    if t.is_null() {
        return 0;
    }
    if (*t).kind == hl::hl_type_kind_HFUN {
        if let Some(fun) = (*t).__bindgen_anon_1.fun.as_ref() {
            return fun.nargs;
        }
    }
    0
}

#[no_mangle]
pub unsafe extern "C" fn hlp_alloc_enum(t: *mut hl_type, index: i32) -> *mut venum {

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
    let ptr = crate::gc::gc_alloc(std::mem::size_of::<hl::venum>() + size)
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

/// Upstream hl_type_super (types.c): super type of HOBJ/HSTRUCT, else &hlt_void.
#[no_mangle]
pub unsafe extern "C" fn hlp_type_super(t: *mut hl_type) -> *mut hl_type {
    if !t.is_null() && ((*t).kind == hl_type_kind_HOBJ || (*t).kind == hl_type_kind_HSTRUCT) {
        let obj = (*t).__bindgen_anon_1.obj;
        if !obj.is_null() && !(*obj).super_.is_null() {
            return (*obj).super_;
        }
    }
    hlt_void()
}

/// Upstream hl_type_enum_eq (types.c): structural equality of two enum
/// values — same type, same constructor, recursively equal parameters.
#[no_mangle]
pub unsafe extern "C" fn hlp_type_enum_eq(a: *mut venum, b: *mut venum) -> bool {
    if a == b {
        return true;
    }
    if a.is_null() || b.is_null() || (*a).t != (*b).t {
        return false;
    }
    if (*a).index != (*b).index {
        return false;
    }
    let tenum = (*(*a).t).__bindgen_anon_1.tenum;
    if tenum.is_null() {
        return false;
    }
    let c = (*tenum).constructs.add((*a).index as usize);
    for i in 0..(*c).nparams as usize {
        let t = *(*c).params.add(i);
        let offset = *(*c).offsets.add(i) as usize;
        if (*t).kind == hl_type_kind_HENUM {
            let pa = *((a as *mut u8).add(offset) as *mut *mut venum);
            let pb = *((b as *mut u8).add(offset) as *mut *mut venum);
            if !hlp_type_enum_eq(pa, pb) {
                return false;
            }
        } else {
            let pa = crate::cast::hlp_make_dyn((a as *mut u8).add(offset) as *mut c_void, t);
            let pb = crate::cast::hlp_make_dyn((b as *mut u8).add(offset) as *mut c_void, t);
            if !pa.is_null()
                && !pb.is_null()
                && !(*pa).t.is_null()
                && !(*pb).t.is_null()
                && (*(*pa).t).kind == hl_type_kind_HENUM
                && (*(*pb).t).kind == hl_type_kind_HENUM
            {
                if !hlp_type_enum_eq(pa as *mut venum, pb as *mut venum) {
                    return false;
                }
                continue;
            }
            if crate::cast::hlp_dyn_compare(pa, pb) != 0 {
                return false;
            }
        }
    }
    true
}

/// Upstream hl_alloc_enum_dyn (types.c): allocate an enum value from a
/// dynamic argument array, allowing missing trailing nullable params.
#[no_mangle]
pub unsafe extern "C" fn hlp_alloc_enum_dyn(
    t: *mut hl_type,
    index: i32,
    args: *mut varray,
    nargs: i32,
) -> *mut venum {
    if t.is_null() || args.is_null() {
        return ptr::null_mut();
    }
    let tenum = (*t).__bindgen_anon_1.tenum;
    if tenum.is_null() || index < 0 || index >= (*tenum).nconstructs {
        return ptr::null_mut();
    }
    let c = (*tenum).constructs.add(index as usize);
    if (*c).nparams < nargs || (*args).size < nargs {
        return ptr::null_mut();
    }
    if nargs < (*c).nparams {
        // allow missing params only if they are nullable (pointer-kinded)
        for i in (nargs as usize)..(*c).nparams as usize {
            if !hl_is_ptr(*(*c).params.add(i)) {
                return ptr::null_mut();
            }
        }
    }
    let e = hlp_alloc_enum(t, index);
    if e.is_null() {
        return ptr::null_mut();
    }
    // hlp_alloc_enum only zeroes when hasptr; upstream always MEM_ZEROs.
    // Zero the payload so missing nullable params read as null.
    let payload = (*c).size as usize;
    std::ptr::write_bytes(e.offset(1) as *mut u8, 0, payload);
    for i in 0..nargs as usize {
        crate::obj::hlp_write_dyn(
            (e as *mut u8).add(*(*c).offsets.add(i) as usize) as *mut c_void,
            *(*c).params.add(i),
            *hl_aptr::<*mut vdynamic>(args).add(i),
            false,
        );
    }
    e
}

/// Upstream hl_enum_parameters (types.c): the constructor arguments of an
/// enum value, boxed into a `Dynamic` array in declaration order.
///
/// The prim is declared `_ARR enum_parameters(_DYN)`, so a caller can hand
/// over any dynamic — including one that is not an enum. Upstream would walk
/// a garbage `tenum` there; an empty array is the answer that cannot corrupt
/// the heap.
#[no_mangle]
pub unsafe extern "C" fn hlp_enum_parameters(e: *mut venum) -> *mut varray {
    if e.is_null() || (*e).t.is_null() || (*(*e).t).kind != hl_type_kind_HENUM {
        return hlp_alloc_array(hlt_dyn(), 0);
    }
    let tenum = (*(*e).t).__bindgen_anon_1.tenum;
    if tenum.is_null() || (*e).index < 0 || (*e).index >= (*tenum).nconstructs {
        return hlp_alloc_array(hlt_dyn(), 0);
    }
    let c = (*tenum).constructs.add((*e).index as usize);
    let a = hlp_alloc_array(hlt_dyn(), (*c).nparams);
    for i in 0..(*c).nparams as usize {
        let field = (e as *mut u8).add(*(*c).offsets.add(i) as usize) as *mut c_void;
        *hl_aptr::<*mut vdynamic>(a).add(i) = crate::cast::hlp_make_dyn(field, *(*c).params.add(i));
    }
    a
}

#[no_mangle]
pub unsafe extern "C" fn hlp_type_enum_fields(t: *mut hl::hl_type) -> *mut varray {
    // Use persistent hlt_bytes() so the at pointer does not dangle after return
    let tenum = (*t).__bindgen_anon_1.tenum;
    let array = hlp_alloc_array(hlt_bytes(), (*tenum).nconstructs);

    for i in 0..(*tenum).nconstructs as usize {
        // Type.initEnum builds its constructor-name map from this array.
        // Returning tenum.name here repeated the enum TYPE name for every
        // slot, so Type.createEnum(e, "A") could never find constructor A.
        *(hl_aptr::<*mut vbyte>(array).add(i)) = (*(*tenum).constructs.add(i)).name as *mut vbyte;
    }

    array
}

#[no_mangle]
pub unsafe extern "C" fn hlp_type_enum_values(t: *mut hl::hl_type) -> *mut varray {
    let tenum = (*t).__bindgen_anon_1.tenum;
    let nconstructs = (*tenum).nconstructs;
    let array = hlp_alloc_array(hlt_dyn(), nconstructs);

    for i in 0..nconstructs as usize {
        let construct = (*tenum).constructs.add(i);
        // __evalues__ contains only constructors that can exist without
        // arguments.  Parameterized constructors stay null and are omitted
        // by Type.allEnums; allocating them here manufactured C(0,null) and
        // D(null) values from uninitialized/default payloads.
        if (*construct).nparams == 0 {
            *(hl_aptr::<*mut venum>(array).add(i)) = hlp_alloc_enum(t, i as i32);
        }
    }

    array
}

#[no_mangle]
pub extern "C" fn hlp_mem_compact(
    _d: *mut vdynamic,
    _exclude: *mut varray,
    _flags: *const i32,
    _out_count: *mut i32,
) -> *mut vdynamic {
    unimplemented!()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_init_enum(et: *mut hl_type, _m: *mut hl_module_context) {
    let tenum = (*et).__bindgen_anon_1.tenum;
    if tenum.is_null() {
        return;
    }

    let mut mark_size = 0;
    let constructs =
        std::slice::from_raw_parts_mut((*tenum).constructs, (*tenum).nconstructs as usize);

    for (i, c) in constructs.iter_mut().enumerate() {
        c.hasptr = false;
        c.size = std::mem::size_of::<*mut std::os::raw::c_void>() as i32
            + std::mem::size_of::<i32>() as i32; // t + index

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
            let max_pos = i as i32 * std::mem::size_of::<i32>() as i32
                + hlp_mark_size(c.size - (HL_WSIZE * 2) as i32);
            if max_pos > mark_size {
                mark_size = max_pos;
            }
        }
    }

    let mark = hlp_zalloc(mark_size) as *mut u32;
    if mark.is_null() {
        return;
    }

    for (i, c) in constructs.iter().enumerate() {
        if !c.hasptr {
            continue;
        }

        let params = std::slice::from_raw_parts(c.params, c.nparams as usize);
        let offsets = std::slice::from_raw_parts(c.offsets, c.nparams as usize);

        for (&param, &offset) in params.iter().zip(offsets.iter()) {
            if hl_is_ptr(param) {
                let pos = (offset / HL_WSIZE as i32) - 2;
                *mark.add(i + (pos as usize >> 5)) |= 1 << (pos & 31);
            }
        }
    }

    (*et).mark_bits = mark as *mut ::std::os::raw::c_uint;
}

/// `hlp_register_guid_name` writes into a process-global map, so these tests
/// share one table with each other and with any future caller. They key off
/// distinct guids, never assert the table's size, and remove what they added,
/// so they hold whatever else is in there and survive being run twice.
#[cfg(test)]
mod guid_name_tests {
    use super::{hlp_register_guid_name, GUID_NAMES};

    /// The map is only reachable from this module, which is the point: the
    /// doc comment says nothing reads these back yet, so the stored value is
    /// the only observable the primitive has.
    fn stored(guid: i64) -> Option<Vec<u16>> {
        let names = GUID_NAMES.get_or_init(Default::default);
        let names = names.lock().unwrap_or_else(|e| e.into_inner());
        names.get(&guid).map(|b| b.to_vec())
    }

    /// Names arrive as NUL-terminated UTF-16, the way `ustrlen` reads them.
    fn u16z(s: &str) -> Vec<u16> {
        let mut v: Vec<u16> = s.encode_utf16().collect();
        v.push(0);
        v
    }

    fn want(s: &str) -> Vec<u16> {
        s.encode_utf16().collect()
    }

    #[test]
    fn a_name_round_trips_and_a_second_one_replaces_it() {
        const GUID: i64 = 0x0a51_7e57_0000_0001;
        unsafe {
            let first = u16z("Renderer");
            hlp_register_guid_name(GUID, first.as_ptr() as *mut _);
            assert_eq!(stored(GUID).as_deref(), Some(want("Renderer").as_slice()));

            // Keyed by guid, so registering again replaces rather than adds.
            let second = u16z("Audio");
            hlp_register_guid_name(GUID, second.as_ptr() as *mut _);
            assert_eq!(stored(GUID).as_deref(), Some(want("Audio").as_slice()));

            hlp_register_guid_name(GUID, std::ptr::null_mut());
            assert_eq!(stored(GUID), None);
        }
    }

    /// The characters are copied in, not aliased. Upstream can hand the
    /// `vbyte*` straight back out because its map is GC'd; a raw GC pointer
    /// parked in a Rust container would be a root the collector cannot see,
    /// so this side copies. If it ever stopped copying, the stored name would
    /// follow the caller's buffer -- including after the buffer is freed.
    #[test]
    fn the_characters_are_copied_not_aliased() {
        const GUID: i64 = 0x0a51_7e57_0000_0002;
        unsafe {
            let mut name = u16z("Physics");
            hlp_register_guid_name(GUID, name.as_mut_ptr() as *mut _);
            assert_eq!(stored(GUID).as_deref(), Some(want("Physics").as_slice()));

            // Scribble over the caller's buffer, then drop it entirely.
            name[0] = 'X' as u16;
            name[1] = 0;
            assert_eq!(
                stored(GUID).as_deref(),
                Some(want("Physics").as_slice()),
                "the stored name followed the caller's buffer"
            );
            drop(name);
            assert_eq!(stored(GUID).as_deref(), Some(want("Physics").as_slice()));

            hlp_register_guid_name(GUID, std::ptr::null_mut());
            assert_eq!(stored(GUID), None);
        }
    }

    /// A null name deregisters, matching upstream's hi64remove branch, and
    /// deregistering something never registered is not an error.
    #[test]
    fn a_null_name_deregisters_and_is_safe_on_an_absent_guid() {
        const PRESENT: i64 = 0x0a51_7e57_0000_0003;
        const ABSENT: i64 = 0x0a51_7e57_0000_0004;
        unsafe {
            hlp_register_guid_name(ABSENT, std::ptr::null_mut());
            assert_eq!(stored(ABSENT), None);

            let n = u16z("Net");
            hlp_register_guid_name(PRESENT, n.as_ptr() as *mut _);
            assert!(stored(PRESENT).is_some());
            hlp_register_guid_name(PRESENT, std::ptr::null_mut());
            assert_eq!(stored(PRESENT), None);
            // Twice over, so a stale entry would show.
            hlp_register_guid_name(PRESENT, std::ptr::null_mut());
            assert_eq!(stored(PRESENT), None);
        }
    }

    /// Length comes from `ustrlen`, so the terminator bounds the name and
    /// nothing past it is read. An empty name is a name, not a removal.
    #[test]
    fn the_stored_length_stops_at_the_terminator() {
        const GUID: i64 = 0x0a51_7e57_0000_0005;
        unsafe {
            // "ab\0cd\0": everything after the first NUL must be ignored.
            let buf: Vec<u16> = vec![
                'a' as u16, 'b' as u16, 0, 'c' as u16, 'd' as u16, 0,
            ];
            hlp_register_guid_name(GUID, buf.as_ptr() as *mut _);
            assert_eq!(stored(GUID).as_deref(), Some(want("ab").as_slice()));

            let empty = u16z("");
            hlp_register_guid_name(GUID, empty.as_ptr() as *mut _);
            assert_eq!(
                stored(GUID).as_deref(),
                Some([].as_slice()),
                "an empty name should register, not deregister"
            );

            // Non-ASCII survives as UTF-16 code units rather than bytes.
            let uni = u16z("\u{00e9}\u{4e2d}");
            hlp_register_guid_name(GUID, uni.as_ptr() as *mut _);
            assert_eq!(stored(GUID).as_deref(), Some(want("\u{00e9}\u{4e2d}").as_slice()));

            hlp_register_guid_name(GUID, std::ptr::null_mut());
            assert_eq!(stored(GUID), None);
        }
    }

    /// The map is shared across threads, which is why it is behind a mutex.
    /// Registering from several at once must not lose an entry or poison the
    /// lock for the next caller.
    #[test]
    fn concurrent_registrations_all_land() {
        const BASE: i64 = 0x0a51_7e57_0001_0000;
        let mut handles = Vec::new();
        for t in 0..4i64 {
            handles.push(std::thread::spawn(move || unsafe {
                for i in 0..64i64 {
                    let guid = BASE + t * 1000 + i;
                    let n = u16z(&format!("t{t}n{i}"));
                    hlp_register_guid_name(guid, n.as_ptr() as *mut _);
                }
            }));
        }
        for h in handles {
            h.join().unwrap();
        }
        for t in 0..4i64 {
            for i in 0..64i64 {
                let guid = BASE + t * 1000 + i;
                assert_eq!(
                    stored(guid).as_deref(),
                    Some(want(&format!("t{t}n{i}")).as_slice()),
                    "guid {guid} went missing"
                );
                unsafe { hlp_register_guid_name(guid, std::ptr::null_mut()) };
            }
        }
    }

    /// DEFINE_PRIM(_VOID, register_guid_name, _I64 _BYTES).
    #[test]
    fn the_exported_signature_is_the_one_upstream_declares() {
        let _: unsafe extern "C" fn(i64, *mut super::vbyte) = hlp_register_guid_name;
    }
}

/// Instance size of an object type, or -1 for anything without one.
#[no_mangle]
pub unsafe extern "C" fn hlp_type_data_size(t: *mut hl_type) -> i32 {
    if t.is_null() {
        return -1;
    }
    match (*t).kind {
        hl::hl_type_kind_HOBJ | hl::hl_type_kind_HSTRUCT => {
            let rt = crate::obj::hlp_get_obj_rt(t);
            if rt.is_null() { -1 } else { (*rt).size }
        }
        _ => -1,
    }
}

#[cfg(test)]
mod data_size_tests {
    use super::*;

    /// -1 for everything that is not an object, and never a crash on null.
    /// Haxe reads this to decide whether a type can be laid out inline.
    #[test]
    fn data_size_is_negative_for_non_objects() {
        unsafe {
            assert_eq!(hlp_type_data_size(std::ptr::null_mut()), -1);
            assert_eq!(hlp_type_data_size(hlt_i32()), -1);
            assert_eq!(hlp_type_data_size(hlt_f64()), -1);
            assert_eq!(hlp_type_data_size(hlt_bytes()), -1);
        }
    }
}
