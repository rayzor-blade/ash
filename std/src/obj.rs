// `static mut` + raw-pointer access is this module's deliberate story (the
// VM's single-threaded invariant): `static_mut_refs` demands the
// `&raw`/deref spelling, and these two style lints then flag exactly that
// spelling. The trio cannot all be satisfied at once.
#![allow(clippy::deref_addrof, dangerous_implicit_autorefs)]
use std::alloc::alloc;
use std::sync::RwLock;
use std::{
    alloc::Layout,
    ffi::{c_int, c_void, CStr},
    mem, ptr,
    sync::{LazyLock, Mutex},
};

use crate::{
    buffer::hlp_type_str,
    cast::*,
    error::hlp_error,
    gc::{hlp_mark_size, HL_GLOBAL_LOCK},
    hl::{self, *},
    strings::str_to_uchar_ptr,
    types::{
        hl_is_ptr, hlp_is_dynamic, hlp_pad_struct, hlp_safe_cast, hlp_same_type, hlp_type_size,
    },
    ucs2::{ucmp, ustrdup},
};

/// Pre-computed UTF-16 null-terminated string constants for hash lookups.
/// These must be properly aligned u16 arrays, not byte strings cast to u16.
static USTR_COMPARE: &[u16] = &[
    b'_' as u16,
    b'_' as u16,
    b'c' as u16,
    b'o' as u16,
    b'm' as u16,
    b'p' as u16,
    b'a' as u16,
    b'r' as u16,
    b'e' as u16,
    0,
];
static USTR_STRING: &[u16] = &[
    b'_' as u16,
    b'_' as u16,
    b's' as u16,
    b't' as u16,
    b'r' as u16,
    b'i' as u16,
    b'n' as u16,
    b'g' as u16,
    0,
];
static USTR_CAST: &[u16] = &[
    b'_' as u16,
    b'_' as u16,
    b'c' as u16,
    b'a' as u16,
    b's' as u16,
    b't' as u16,
    0,
];
static USTR_GET_FIELD: &[u16] = &[
    b'_' as u16,
    b'_' as u16,
    b'g' as u16,
    b'e' as u16,
    b't' as u16,
    b'_' as u16,
    b'f' as u16,
    b'i' as u16,
    b'e' as u16,
    b'l' as u16,
    b'd' as u16,
    0,
];

#[derive(Clone)]
struct Cache {
    data: *mut hl_field_lookup,
    size: usize,
    capacity: usize,
}

static mut HL_CACHE: LazyLock<RwLock<Cache>> = LazyLock::new(|| {
    RwLock::new(Cache {
        data: ptr::null_mut(),
        size: 0,
        capacity: 0,
    })
});
static INITIAL_CACHE_CAPACITY: usize = 16;

pub static cache_lock: Mutex<i32> = Mutex::new(0);

#[no_mangle]
pub unsafe extern "C" fn hlp_alloc_virtual(t: *mut hl::hl_type) -> *mut hl::vvirtual {
    // Ensure the virtual type is initialized (indexes, lookup, dataSize populated).
    // The interpreter doesn't call hlp_init_virtual during setup, so the first
    // allocation of a given virtual type triggers lazy initialization here.
    let virt = (*t).__bindgen_anon_1.virt;
    if !virt.is_null() && (*virt).indexes.is_null() {
        hlp_init_virtual(t, std::ptr::null_mut());
    }
    let mut allocator = crate::gc::gc_locked();
    if let Some(virt) = allocator.alloc_virtual(t) {
        return virt.as_ptr();
    }
    std::ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_alloc_obj(t: *mut hl::hl_type) -> *mut hl::vdynamic {
    let obj = (*t).__bindgen_anon_1.obj;
    if obj.is_null() {
        return ptr::null_mut();
    }

    let mut rt = (*obj).rt;
    if rt.is_null() || (*rt).methods.is_null() {
        rt = hl_get_obj_proto(t);
    }

    if rt.is_null() {
        return ptr::null_mut();
    }

    let size = (*rt).size as usize;
    // let has_ptr = (*rt).hasPtr;

    // Allocate memory — `allocate` returns zeroed memory (it memsets the
    // region against stale data in reused blocks), so zeroing again here
    // was one of the two memsets the profiler charged to every allocation.
    let ptr = crate::gc::gc_alloc(size).expect("Out of memory");

    let o = ptr.as_ptr() as *mut hl::vobj;
    if (*t).kind != hl::hl_type_kind_HSTRUCT {
        (*o).t = t;
    }

    // Initialize bindings
    for i in 0..(*rt).nbindings {
        let binding = (*rt).bindings.offset(i as isize);
        let fid = (*binding).fid;
        let field_offset = *(*rt).fields_indexes.offset(fid as isize);
        let field_ptr = (o as *mut u8).offset(field_offset as isize) as *mut *mut std::ffi::c_void;

        if !(*binding).closure.is_null() {
            *field_ptr = crate::fun::hlp_alloc_closure_ptr(
                (*binding).closure,
                (*binding).ptr,
                o as *mut std::ffi::c_void,
            ) as *mut c_void;
        } else {
            *field_ptr = (*binding).ptr;
        }
    }

    o as *mut vdynamic
}

#[no_mangle]
pub unsafe extern "C" fn hlp_alloc_dynamic(t: *mut hl_type) -> *mut vdynamic {
    // let flags = mem_kind | MEM_ZERO;

    let d = crate::gc::gc_alloc(std::mem::size_of::<vdynamic>())
        .expect("Out of memory")
        .as_ptr() as *mut vdynamic;

    // Zero-initialize the memory
    ptr::write_bytes(d as *mut u8, 0, std::mem::size_of::<vdynamic>());

    (*d).t = t;

    d
}

pub unsafe extern "C" fn hlp_alloc_dynbool(b: bool) -> *mut vdynamic {
    let v = hlp_alloc_dynamic(crate::types::hlt_bool());
    (*v).v.b = b;
    v
}

#[no_mangle]
pub unsafe extern "C" fn hlp_write_dyn(
    data: *mut c_void,
    t: *mut hl_type,
    v: *mut vdynamic,
    is_tmp: bool,
) {
    // hl_track_call(HL_TRACK_CAST, on_cast(if !v.is_null() { (*v).t } else { hlt_dyn }, t));

    let dyn_type = crate::types::hlt_dyn();

    match (*t).kind {
        hl_type_kind_HUI8 => {
            *(data as *mut u8) = hlp_dyn_casti(&v as *const _ as *mut c_void, dyn_type, t) as u8;
        }
        hl_type_kind_HBOOL => {
            *(data as *mut bool) = hlp_dyn_casti(&v as *const _ as *mut c_void, dyn_type, t) != 0;
        }
        hl_type_kind_HUI16 => {
            *(data as *mut u16) = hlp_dyn_casti(&v as *const _ as *mut c_void, dyn_type, t) as u16;
        }
        hl_type_kind_HI32 => {
            *(data as *mut i32) = hlp_dyn_casti(&v as *const _ as *mut c_void, dyn_type, t);
        }
        hl_type_kind_HI64 => {
            *(data as *mut i64) = hlp_dyn_casti64(&v as *const _ as *mut c_void, dyn_type);
        }
        hl_type_kind_HF32 => {
            *(data as *mut f32) = hlp_dyn_castf(&v as *const _ as *mut c_void, dyn_type);
        }
        hl_type_kind_HF64 => {
            *(data as *mut f64) = hlp_dyn_castd(&v as *const _ as *mut c_void, dyn_type);
        }
        _ => {
            let mut ret = if !v.is_null() && hlp_same_type(t, (*v).t) {
                v as *mut c_void
            } else {
                hlp_dyn_castp(&v as *const _ as *mut c_void, dyn_type, t)
            };

            if is_tmp && ret == v as *mut c_void {
                let new_v = hlp_alloc_dynamic((*v).t);
                (*new_v).v = (*v).v;
                ret = new_v as *mut c_void;
            }

            *(data as *mut *mut c_void) = ret;
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_obj_lookup(
    d: *mut vdynamic,
    hfield: i32,
    t: *mut *mut hl_type,
) -> *mut c_void {
    match (*(*d).t).kind {
        hl_type_kind_HDYNOBJ => {
            let o = d as *mut vdynobj;
            let f = hlp_lookup_find((*o).lookup, (*o).nfields, hfield);
            if f.is_null() {
                return ptr::null_mut();
            }
            *t = (*f).t;
            return hlp_dynobj_field(o, f);
        }
        hl_type_kind_HOBJ => {
            let f = obj_resolve_field((*(*d).t).__bindgen_anon_1.obj, hfield);
            if f.is_null() || (*f).field_index < 0 {
                return ptr::null_mut();
            }
            *t = (*f).t;
            return (d as *mut u8).offset((*f).field_index as isize) as *mut c_void;
        }
        hl_type_kind_HSTRUCT => {
            let f = obj_resolve_field((*(*d).t).__bindgen_anon_1.obj, hfield);
            if f.is_null() || (*f).field_index < 0 {
                return ptr::null_mut();
            }
            *t = (*f).t;
            return ((*d).v.ptr as *mut u8).offset((*f).field_index as isize) as *mut c_void;
        }
        hl_type_kind_HVIRTUAL => {
            let v = (*(d as *mut vvirtual)).value;
            if !v.is_null() {
                return hlp_obj_lookup(v, hfield, t);
            }
            let f = hlp_lookup_find(
                (*(*(*d).t).__bindgen_anon_1.virt).lookup,
                (*(*(*d).t).__bindgen_anon_1.virt).nfields,
                hfield,
            );
            if f.is_null() {
                return ptr::null_mut();
            }
            *t = (*f).t;
            return (d as *mut u8).offset(
                *(*(*(*d).t).__bindgen_anon_1.virt)
                    .indexes
                    .add((*f).field_index as usize) as isize,
            ) as *mut c_void;
        }
        _ => {
            hlp_error(str_to_uchar_ptr("Invalid field access"));
        }
    }
    ptr::null_mut()
}

// Function to get the field of a dynamic object
#[inline]
pub unsafe fn hlp_dynobj_field(o: *const vdynobj, f: *const hl_field_lookup) -> *mut c_void {
    if hl_is_ptr((*f).t) {
        ((*o)
            .values
            .add(((*f).field_index & HL_DYNOBJ_INDEX_MASK as i32) as usize)) as *mut c_void
    } else {
        ((*o)
            .raw_data
            .add(((*f).field_index & HL_DYNOBJ_INDEX_MASK as i32) as usize)) as *mut c_void
    }
}

// Function to get the order of a field lookup
#[inline]
pub fn hlp_dynobj_order(f: *const hl_field_lookup) -> u32 {
    unsafe { (*f).field_index as u32 >> HL_DYNOBJ_INDEX_SHIFT }
}

// Debug function to print cache state
#[allow(dead_code)] // diagnostic kept for the commented-out cache tracing below
fn print_cache_state(cache: &Cache, msg: &str) {
    println!(
        "{}: Cache state - data: {:?}, size: {}, capacity: {}",
        msg, cache.data, cache.size, cache.capacity
    );
}

unsafe fn grow_cache(cache: &mut Cache) -> bool {
    let new_capacity = if cache.capacity == 0 {
        INITIAL_CACHE_CAPACITY
    } else {
        cache.capacity * 2
    };
    // println!("Growing cache from {} to {} entries", cache.capacity, new_capacity);

    let new_layout = Layout::array::<hl_field_lookup>(new_capacity).unwrap();
    let new_data = if cache.data.is_null() {
        alloc(new_layout) as *mut hl_field_lookup
    } else {
        let old_layout = Layout::array::<hl_field_lookup>(cache.capacity).unwrap();
        std::alloc::realloc(cache.data as *mut u8, old_layout, new_layout.size())
            as *mut hl_field_lookup
    };

    if new_data.is_null() {
        // println!("Failed to grow cache");
        return false;
    }

    cache.data = new_data;
    cache.capacity = new_capacity;
    true
}

#[no_mangle]
pub unsafe extern "C" fn hlp_hash_gen(name: *const uchar, cache_name: bool) -> i32 {
    // println!(
    //     "Entering hl_hash_gen with name: {:?}, cache_name: {}",
    //     name, cache_name
    // );

    if name.is_null() {
        return 0;
    }

    // Guard against misaligned pointers (e.g., from byte strings cast to u16*)
    if !(name as usize).is_multiple_of(2) {
        return 0;
    }

    let mut h: i32 = 0;
    let oname = name;
    let mut current = name;

    while *current != 0 {
        h = h.wrapping_mul(223).wrapping_add(*current as i32);
        current = current.offset(1);
    }

    h = h.wrapping_rem(0x1FFFFF7B);
    // println!("Computed hash: {}", h);

    if cache_name {
        // HashLink resolves a real hash collision by probing consecutive
        // hashes. Dynamic objects store only the integer, so keeping two
        // different names under the same value makes the second field alias
        // the first and also loses its name in Reflect.fields (haxe#5572).
        if let Ok(mut cache) = (*(&raw const HL_CACHE)).write() {
            loop {
                let lookup = hlp_lookup_find(cache.data, cache.size as i32, h);
                if lookup.is_null() {
                    break;
                }
                if ucmp((*lookup).t as *const uchar, oname) == 0 {
                    return h;
                }
                h = h.wrapping_add(1);
            }

            if (cache.data.is_null() || cache.size >= cache.capacity)
                && !grow_cache(&mut cache) {
                    return h;
                }

            let new_name = ustrdup(oname);
            if !new_name.is_null() {
                // Use sorted insertion (hlp_lookup_insert) so binary search works
                hlp_lookup_insert(
                    cache.data,
                    cache.size as i32,
                    h,
                    new_name as *mut hl_type,
                    0,
                );
                cache.size += 1;
            }
        }
    }

    // println!("Returning hash: {}", h);
    h
}

pub unsafe extern "C" fn hlp_get_obj_proto(ot: *mut hl_type) -> *mut hl_runtime_obj {
    if ot.is_null() {
        return ptr::null_mut();
    }
    match (*ot).kind {
        hl::hl_type_kind_HOBJ | hl::hl_type_kind_HSTRUCT => hlp_get_obj_rt(ot),
        _ => ptr::null_mut(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_lookup_insert(
    l: *mut hl_field_lookup,
    size: i32,
    hash: i32,
    t: *mut hl_type,
    index: i32,
) -> *mut hl_field_lookup {
    if l.is_null() || size < 0 {
        return ptr::null_mut();
    }

    let n = size as usize;

    // Binary search for insertion position (sorted by hashed_name)
    let mut low = 0usize;
    let mut high = n;

    while low < high {
        let mid = low + (high - low) / 2;
        let mid_hash = (*l.add(mid)).hashed_name;
        if mid_hash < hash {
            low = mid + 1;
        } else {
            high = mid;
        }
    }

    let pos = low;

    // Shift elements right to make room for the new entry.
    // The caller guarantees the array has enough capacity (pre-allocated
    // with nlookup entries in hlp_get_obj_rt).
    if pos < n {
        ptr::copy(l.add(pos), l.add(pos + 1), n - pos);
    }

    // Insert the new entry
    *l.add(pos) = hl_field_lookup {
        field_index: index,
        hashed_name: hash,
        t,
    };

    l.add(pos)
}

#[no_mangle]
pub unsafe extern "C" fn hlp_lookup_find(
    l: *mut hl_field_lookup,
    size: i32,
    hash: i32,
) -> *mut hl_field_lookup {
    if l.is_null() || size <= 0 {
        return ptr::null_mut();
    }

    let mut low = 0;
    let mut high = size as usize;

    while low < high {
        let mid = low + (high - low) / 2;
        let mid_hash = (*l.add(mid)).hashed_name;
        if mid_hash < hash {
            low = mid + 1;
        } else if mid_hash > hash {
            high = mid;
        } else {
            return l.add(mid);
        }
    }

    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_lookup_find_index(
    l: *mut hl_field_lookup,
    size: i32,
    hash: i32,
) -> i32 {
    if l.is_null() || size <= 0 {
        return 0;
    }

    let mut low = 0;
    let mut high = size as usize;

    while low < high {
        let mid = low + (high - low) / 2;
        let mid_hash = (*l.add(mid)).hashed_name;
        if mid_hash < hash {
            low = mid + 1;
        } else if mid_hash > hash {
            high = mid;
        } else {
            return mid as i32;
        }
    }

    low as i32
}
#[no_mangle]
pub unsafe extern "C" fn hlp_field_name(hash: c_int) -> *mut vbyte {
    if let Ok(cache) = (*(&raw const HL_CACHE)).read() {
        let l = hlp_lookup_find(cache.data, cache.size as i32, hash);
        if !l.is_null() {
            return (*l).t as *mut vbyte;
        }
    }
    str_to_uchar_ptr("???") as *mut vbyte
}

pub(crate) unsafe fn obj_resolve_field(o: *const hl_type_obj, hfield: i32) -> *mut hl_field_lookup {
    let mut rt = (*o).rt;
    while !rt.is_null() {
        let f = hlp_lookup_find((*rt).lookup, (*rt).nlookup, hfield);
        if !f.is_null() {
            return f;
        }
        rt = (*rt).parent;
    }
    ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn hl_get_obj_proto(ot: *mut hl_type) -> *mut hl_runtime_obj {
    let o = (*ot).__bindgen_anon_1.obj;
    let m = (*o).m;
    let t = hlp_get_obj_rt(ot);
    let mut p: *mut hl_runtime_obj = ptr::null_mut();

    if !(*ot).vobj_proto.is_null() {
        return t;
    }

    if !(*o).super_.is_null() {
        p = hl_get_obj_proto((*o).super_);
    }

    let _lock = HL_GLOBAL_LOCK
        .get_or_init(|| std::sync::Mutex::new(()))
        .lock()
        .unwrap();

    if !(*ot).vobj_proto.is_null() {
        return t;
    }

    let mut allocator = crate::gc::gc_locked();

    if (*t).nproto != 0 {
        let fptr = allocator
            .allocate_immortal(
                std::mem::size_of::<*mut std::os::raw::c_void>() * (*t).nproto as usize,
            )
            .expect("Failed to allocate memory")
            .as_ptr() as *mut *mut std::os::raw::c_void;
        (*ot).vobj_proto = fptr;

        if !p.is_null() {
            ptr::copy_nonoverlapping(
                (*(*p).t).vobj_proto as *const *mut std::os::raw::c_void,
                fptr,
                (*p).nproto as usize,
            );
        }

        if !m.is_null() && !(*m).functions_ptrs.is_null() {
            for i in 0..(*o).nproto as usize {
                let p = (*o).proto.add(i);
                if (*p).pindex >= 0 {
                    let faddr = *(*m).functions_ptrs.add((*p).findex as usize);
                    *fptr.add((*p).pindex as usize) = faddr;
                }
            }
        }
    } else {
        // HashLink's "this type has no proto" sentinel is exactly 1
        // (hashlink src/std/obj.c:312), and every reader in ash tests
        // `vobj_proto as usize > 1`. dangling_mut() is align_of::<*mut
        // c_void>() == 8, which sails through all of those guards and then
        // gets indexed as if it were a real proto array at address 8.
        // `without_provenance_mut` rather than `1 as *mut _`: this is an
        // ADDRESS used as a sentinel, never a pointer to be dereferenced, and
        // spelling it that way is both the honest description and what stops
        // clippy::manual_dangling_ptr from "helpfully" suggesting
        // dangling_mut() -- which is the 8 this line exists to replace.
        (*ot).vobj_proto = std::ptr::without_provenance_mut(1);
    }

    (*t).methods = allocator
        .allocate_immortal(
            std::mem::size_of::<*mut std::os::raw::c_void>() * (*t).nmethods as usize,
        )
        .expect("Failed to allocate memory")
        .as_ptr() as *mut *mut std::os::raw::c_void;

    if !p.is_null() {
        ptr::copy_nonoverlapping((*p).methods, (*t).methods, (*p).nmethods as usize);
    }

    let mut nmethods = if !p.is_null() { (*p).nmethods } else { 0 };

    for i in 0..(*o).nproto as usize {
        let pr = (*o).proto.add(i);
        let method_index = if !p.is_null() {
            if (*pr).pindex >= 0 && (*pr).pindex < (*p).nproto {
                let super_obj = (*(*o).super_).__bindgen_anon_1.obj;
                let lookup = obj_resolve_field(super_obj, (*pr).hashed_name);
                if lookup.is_null() {
                    // Skip this proto entry to avoid crash
                    let index = nmethods;
                    nmethods += 1;
                    index
                } else {
                    -(*lookup).field_index - 1
                }
            } else {
                let index = nmethods;
                nmethods += 1;
                index
            }
        } else {
            i as i32
        };
        if !m.is_null() && !(*m).functions_ptrs.is_null() {
            let faddr = *(*m).functions_ptrs.add((*pr).findex as usize);
            *(*t).methods.add(method_index as usize) = faddr;
        }
    }

    // Interfaces
    (*t).ninterfaces = 0;
    for i in 0..(*o).nfields {
        if (*(*o).fields.add(i as usize)).hashed_name == 0 {
            (*t).ninterfaces += 1;
        }
    }
    (*t).interfaces = allocator
        .allocate_immortal(std::mem::size_of::<i32>() * (*t).ninterfaces as usize)
        .expect("Failed to allocate memory")
        .as_ptr() as *mut i32;
    (*t).ninterfaces = 0;
    for i in 0..(*o).nfields as usize {
        if (*(*o).fields.add(i)).hashed_name == 0 {
            *(*t).interfaces.add((*t).ninterfaces as usize) = i as i32;
            (*t).ninterfaces += 1;
        }
    }

    // Bindings
    let mut nbindings = if !p.is_null() {
        ptr::copy_nonoverlapping((*p).bindings, (*t).bindings, (*p).nbindings as usize);
        (*p).nbindings
    } else {
        0
    };

    // Bindings require module metadata (m) for function pointers/types.
    // Skip binding setup when m is null (e.g., interpreter-only mode).
    if !m.is_null() && !(*m).functions_ptrs.is_null() {
        for i in 0..(*o).nbindings as usize {
            let fid = *(*o).bindings.add(i * 2);
            let mid = *(*o).bindings.add(i * 2 + 1);
            let mut b: *mut hl_runtime_binding = ptr::null_mut();

            if !p.is_null() {
                for j in 0..(*p).nbindings as usize {
                    if (*(*p).bindings.add(j)).fid == fid {
                        b = (*t).bindings.add(j);
                        break;
                    }
                }
            }

            if b.is_null() {
                b = (*t).bindings.add(nbindings as usize);
                nbindings += 1;
            }

            (*b).fid = fid;
            let field_lookup = hlp_obj_field_fetch((*t).t, fid);
            if field_lookup.is_null() {
                continue;
            }
            let ft = (*field_lookup).t;

            let _func_type_ptr = *(*m).functions_types.add(mid as usize);
            let _func_ptr = *(*m).functions_ptrs.add(mid as usize);

            match (*ft).kind {
                hl::hl_type_kind_HFUN
                    if (*(*ft).__bindgen_anon_1.fun).nargs
                        == (*(*(*(*m).functions_types.add(mid as usize)))
                            .__bindgen_anon_1
                            .fun)
                            .nargs =>
                {
                    let c = allocator
                        .allocate_immortal(std::mem::size_of::<vclosure>())
                        .expect("Failed to allocate memory")
                        .as_ptr() as *mut vclosure;
                    (*c).fun = *(*m).functions_ptrs.add(mid as usize);
                    (*c).t = *(*m).functions_types.add(mid as usize);
                    (*c).hasValue = 0;
                    (*c).value = ptr::null_mut();
                    (*b).closure = ptr::null_mut();
                    (*b).ptr = c as *mut std::os::raw::c_void;
                }
                hl::hl_type_kind_HFUN | hl::hl_type_kind_HDYN => {
                    (*b).closure = *(*m).functions_types.add(mid as usize);
                    (*b).ptr = *(*m).functions_ptrs.add(mid as usize);
                }
                _ => panic!("invalid bind field kind={}", (*ft).kind),
            }
        }
    }

    let str_hash = hlp_hash_gen(USTR_STRING.as_ptr(), false);
    let str_field = obj_resolve_field(o, str_hash);
    let cmp_field = obj_resolve_field(o, hlp_hash_gen(USTR_COMPARE.as_ptr(), false));
    let cast_field = obj_resolve_field(o, hlp_hash_gen(USTR_CAST.as_ptr(), false));
    let get_field = obj_resolve_field(o, hlp_hash_gen(USTR_GET_FIELD.as_ptr(), false));
    (*t).toStringFun = if !str_field.is_null() {
        let fptr = *(*t)
            .methods
            .offset((-((*str_field).field_index + 1)).try_into().unwrap());
        // KEEP interpreter stubs. Upstream stores the method pointer
        // unconditionally (hashlink src/std/obj.c:395); ash used to drop
        // anything below 0x10000, and in interpreter mode that is EVERY
        // __string in the program, because the function table holds findex+1
        // sentinels rather than code addresses. toStringFun was therefore
        // always None and `"" + obj` printed the class name instead of
        // calling toString -- the class-name path is the one upstream takes
        // only when there is no __string at all (buffer.c:236).
        //
        // The single reader, buffer.rs, routes a stub back through the
        // interpreter (see call_tostring_or_stub). Any future reader MUST do
        // the same rather than calling this as a raw function pointer.
        if fptr.is_null() {
            None
        } else {
            Some(mem::transmute::<
                *mut c_void,
                unsafe extern "C" fn(*mut vdynamic) -> *const u16,
            >(fptr))
        }
    } else {
        None
    };
    (*t).compareFun = if !cmp_field.is_null() {
        let fptr = *(*t)
            .methods
            .offset((-((*cmp_field).field_index + 1)).try_into().unwrap());
        if (fptr as usize) < 0x10000 {
            None
        } else {
            Some(mem::transmute::<
                *mut c_void,
                unsafe extern "C" fn(*mut vdynamic, *mut vdynamic) -> c_int,
            >(fptr))
        }
    } else {
        None
    };
    (*t).castFun = if !cast_field.is_null() {
        let fptr = *(*t)
            .methods
            .offset((-((*cast_field).field_index + 1)).try_into().unwrap());
        // Guard: stub function pointers (findex+1) from the interpreter are
        // small integers, not valid code addresses. Treat them as None.
        if (fptr as usize) < 0x10000 {
            None
        } else {
            Some(mem::transmute::<
                *mut c_void,
                unsafe extern "C" fn(*mut vdynamic, *mut hl_type) -> *mut vdynamic,
            >(fptr))
        }
    } else {
        None
    };
    (*t).getFieldFun = if !get_field.is_null() {
        let fptr = *(*t)
            .methods
            .offset((-((*get_field).field_index + 1)).try_into().unwrap());
        if (fptr as usize) < 0x10000 {
            None
        } else {
            Some(mem::transmute::<
                *mut c_void,
                unsafe extern "C" fn(*mut vdynamic, c_int) -> *mut vdynamic,
            >(fptr))
        }
    } else {
        None
    };

    if !p.is_null() && (*t).getFieldFun.is_none() {
        (*t).getFieldFun = (*p).getFieldFun;
    }

    t
}

#[no_mangle]
pub unsafe extern "C" fn hlp_obj_field_fetch(t: *mut hl_type, fid: i32) -> *mut hl_obj_field {
    if (*t).kind != hl::hl_type_kind_HOBJ && (*t).kind != hl::hl_type_kind_HSTRUCT {
        return ptr::null_mut();
    }

    let mut rt = hlp_get_obj_rt(t);

    if fid < 0 || fid >= (*rt).nfields {
        return ptr::null_mut();
    }

    while !(*rt).parent.is_null() && fid < (*(*rt).parent).nfields {
        rt = (*rt).parent;
    }

    let offset = if !(*rt).parent.is_null() {
        (*(*rt).parent).nfields
    } else {
        0
    };
    (*(*(*rt).t).__bindgen_anon_1.obj)
        .fields
        .offset((fid - offset) as isize)
}

#[no_mangle]
pub unsafe extern "C" fn hlp_get_obj_rt(ot: *mut hl_type) -> *mut hl_runtime_obj {
    let _kind = (*ot).kind;
    
    let o = (*ot).__bindgen_anon_1.obj;
    let m = (*o).m;

    if !(*o).rt.is_null() {
        return (*o).rt;
    }

    let mut p: *mut hl_runtime_obj = ptr::null_mut();
    if !(*o).super_.is_null() {
        p = hlp_get_obj_rt((*o).super_);
    }

    let _lock = HL_GLOBAL_LOCK
        .get_or_init(|| std::sync::Mutex::new(()))
        .lock()
        .unwrap();

    if !(*o).rt.is_null() {
        return (*o).rt;
    }

    let mut gc = crate::gc::gc_locked_init();
    // Runtime type structures are immortal and referenced only from type
    // memory the GC never scans — allocate_immortal pins them (and, via
    // conservative trace, everything they point to) as persistent roots.
    let t = gc
        .allocate_immortal(std::mem::size_of::<hl_runtime_obj>())
        .expect("Failed to allocate memory")
        .as_ptr() as *mut hl_runtime_obj;
    (*t).t = ot;
    (*t).nfields = (*o).nfields + if !p.is_null() { (*p).nfields } else { 0 };
    (*t).nproto = if !p.is_null() { (*p).nproto } else { 0 };
    (*t).nlookup = (*o).nfields;
    (*t).nbindings = if !p.is_null() { (*p).nbindings } else { 0 };
    (*t).hasPtr = if !p.is_null() { (*p).hasPtr } else { false };

    if p.is_null() {
        (*t).nlookup += (*o).nproto;
        (*t).nbindings += (*o).nbindings;
    } else {
        for i in 0..(*o).nproto as usize {
            let pr = (*o).proto.add(i);
            if (*pr).pindex >= 0 && (*pr).pindex < (*p).nproto {
                continue;
            }
            (*t).nlookup += 1;
        }
        for i in 0..(*o).nbindings as usize {
            let fid = *(*o).bindings.add(i * 2);
            let mut found = false;
            let mut pp = if !p.is_null() {
                (*(*p).t).__bindgen_anon_1.obj
            } else {
                ptr::null_mut()
            };
            while !pp.is_null() && !found {
                for j in 0..(*pp).nbindings as usize {
                    if *(*pp).bindings.add(j * 2) == fid {
                        found = true;
                        break;
                    }
                }
                pp = if !(*pp).super_.is_null() {
                    (*(*pp).super_).__bindgen_anon_1.obj
                } else {
                    ptr::null_mut()
                };
            }
            if !found {
                (*t).nbindings += 1;
            }
        }
    }

    (*t).lookup = gc
        .allocate_immortal(std::mem::size_of::<hl_field_lookup>() * (*t).nlookup as usize)
        .expect("Failed to allocate memory")
        .as_ptr() as *mut hl_field_lookup;
    (*t).fields_indexes = gc
        .allocate_immortal(std::mem::size_of::<i32>() * (*t).nfields as usize)
        .expect("Failed to allocate memory")
        .as_ptr() as *mut i32;
    (*t).bindings = gc
        .allocate_immortal(std::mem::size_of::<hl_runtime_binding>() * (*t).nbindings as usize)
        .expect("Failed to allocate memory")
        .as_ptr() as *mut hl_runtime_binding;
    (*t).toStringFun = None;
    (*t).compareFun = None;
    (*t).castFun = None;
    (*t).getFieldFun = None;
    (*t).parent = p;

    // Fields indexes
    let mut start = 0;
    let mut size = if !p.is_null() {
        (*p).size - (*p).pad_size as i32
    } else {
        if (*ot).kind == hl::hl_type_kind_HSTRUCT {
            0
        } else {
            std::mem::size_of::<*mut hl_type>() as i32
        }
    };
    let mut nlookup = 0;
    let mut largest_field = if !p.is_null() {
        (*p).largest_field
    } else {
        size as u8
    };

    if !p.is_null() {
        start = (*p).nfields;
        ptr::copy_nonoverlapping(
            (*p).fields_indexes,
            (*t).fields_indexes,
            (*p).nfields as usize,
        );
    }

    for i in 0..(*o).nfields as usize {
        let ft = (*(*o).fields.add(i)).t;
        if (*ft).kind == hl::hl_type_kind_HPACKED {
            let large = (*hlp_get_obj_rt((*ft).__bindgen_anon_1.tparam)).largest_field;
            let pad = size as u8 % large;
            if pad != 0 {
                size += (large - pad) as i32;
            }
            if large > largest_field {
                largest_field = large;
            }
        } else {
            size += hlp_pad_struct(size, ft);
        }
        *(*t).fields_indexes.add(i + start as usize) = size;
        if *(*(*o).fields.add(i)).name != 0 {
            hlp_lookup_insert(
                (*t).lookup,
                nlookup,
                (*(*o).fields.add(i)).hashed_name,
                (*(*o).fields.add(i)).t,
                size,
            );
            nlookup += 1;
        } else {
            (*t).nlookup -= 1;
        }
        if (*ft).kind == hl::hl_type_kind_HPACKED {
            let rts = hlp_get_obj_rt((*ft).__bindgen_anon_1.tparam);
            size += (*rts).size;
            if (*rts).hasPtr {
                (*t).hasPtr = true;
            }
            continue;
        }
        let sz = hlp_type_size(ft);
        size += sz as i32;
        if sz > largest_field.into() {
            largest_field = sz as u8;
        }
        if !(*t).hasPtr && hl_is_ptr(ft) {
            (*t).hasPtr = true;
        }
    }

    (*t).size = size;
    (*t).pad_size = 0;
    if largest_field > 0 {
        let pad = size % largest_field as i32;
        if pad != 0 {
            (*t).pad_size = largest_field - pad as u8;
            (*t).size += (*t).pad_size as i32;
        }
    }
    (*t).largest_field = largest_field;
    (*t).nmethods = if !p.is_null() {
        (*p).nmethods
    } else {
        (*o).nproto
    };
    (*t).methods = ptr::null_mut();
    (*o).rt = t;
    (*ot).vobj_proto = ptr::null_mut();

    let compare_hash = hlp_hash_gen(USTR_COMPARE.as_ptr(), false);
    for i in 0..(*o).nproto as usize {
        let pr = (*o).proto.add(i);
        let method_index = if !p.is_null() {
            if (*pr).pindex >= 0 && (*pr).pindex < (*p).nproto {
                continue;
            }
            let index = (*t).nmethods;
            (*t).nmethods += 1;
            index
        } else {
            i as i32
        };
        if (*pr).pindex >= (*t).nproto {
            (*t).nproto = (*pr).pindex + 1;
        }
        // m (hl_module) may be null if CTypeFactory doesn't have module metadata.
        // In that case, use the hl_type itself as a placeholder for the lookup entry.
        let mt = if !m.is_null() && !(*m).functions_types.is_null() {
            *(*m).functions_types.add((*pr).findex as usize)
        } else {
            ot // fallback: use the object's own type
        };
        hlp_lookup_insert(
            (*t).lookup,
            nlookup,
            (*pr).hashed_name,
            mt,
            -(method_index + 1),
        );
        nlookup += 1;
        if !m.is_null()
            && !(*m).functions_types.is_null()
            && (*pr).hashed_name == compare_hash
            && (*(*mt).__bindgen_anon_1.fun).nargs == 2
            && (*(*(*(*mt).__bindgen_anon_1.fun).args.add(1))).kind == hl::hl_type_kind_HDYN
            && (*(*(*mt).__bindgen_anon_1.fun).ret).kind == hl::hl_type_kind_HI32
        {
            // Look up the actual function pointer from the module's functions table
            let fptr = *(*m).functions_ptrs.add((*pr).findex as usize);
            if !fptr.is_null() {
                (*t).compareFun = Some(mem::transmute::<
                    *mut c_void,
                    unsafe extern "C" fn(*mut vdynamic, *mut vdynamic) -> c_int,
                >(fptr));
            }
        }
    }

    // Mark bits
    if (*t).hasPtr {
        let mark_size = hlp_mark_size((*t).size) as usize;
        let mark = gc
            .allocate_immortal(mark_size)
            .expect("Failed to allocate memory")
            .as_ptr() as *mut u32;
        ptr::write_bytes(mark as *mut u8, 0, mark_size);
        (*ot).mark_bits = mark;
        if !p.is_null() && !(*(*p).t).mark_bits.is_null() {
            ptr::copy_nonoverlapping(
                (*(*p).t).mark_bits,
                mark,
                hlp_mark_size((*p).size) as usize / std::mem::size_of::<u32>(),
            );
        }
        for i in 0..(*o).nfields as usize {
            let ft = (*(*o).fields.add(i)).t;
            if hl_is_ptr(ft) {
                let pos = *(*t).fields_indexes.add(i + start as usize)
                    / std::mem::size_of::<*mut std::os::raw::c_void>() as i32;
                if (*ft).kind == hl::hl_type_kind_HPACKED {
                    let rts = hlp_get_obj_rt((*ft).__bindgen_anon_1.tparam);
                    if !(*(*rts).t).mark_bits.is_null() {
                        ptr::copy_nonoverlapping(
                            (*(*rts).t).mark_bits,
                            mark.add((pos >> 5) as usize),
                            hlp_mark_size((*rts).size) as usize / std::mem::size_of::<u32>(),
                        );
                    }
                    continue;
                }
                *mark.add((pos >> 5) as usize) |= 1 << (pos & 31);
            }
        }
    }

    t
}

#[no_mangle]
pub unsafe extern "C" fn hlp_obj_lookup_set(
    d: *mut vdynamic,
    hfield: i32,
    t: *mut hl_type,
    ft: *mut *mut hl_type,
) -> *mut c_void {
    match (*(*d).t).kind {
        hl::hl_type_kind_HDYNOBJ => {
            let o = d as *mut vdynobj;
            let mut f = hlp_lookup_find((*o).lookup, (*o).nfields, hfield);
            if f.is_null() {
                f = hlp_dynobj_add_field(o, hfield, t);
            } else if !hlp_same_type(t, (*f).t) {
                if hl_is_ptr(t) != hl_is_ptr((*f).t) || hlp_type_size(t) != hlp_type_size((*f).t) {
                    hlp_dynobj_delete_field(o, f);
                    f = hlp_dynobj_add_field(o, hfield, t);
                } else {
                    (*f).t = t;
                    hlp_dynobj_remap_virtuals(o, f, 0);
                }
            }
            *ft = (*f).t;
            hlp_dynobj_field(o, f)
        }
        hl::hl_type_kind_HOBJ => {
            let f = obj_resolve_field((*(*d).t).__bindgen_anon_1.obj, hfield);
            if f.is_null() || (*f).field_index < 0 {
                let name = CStr::from_ptr((*(*(*d).t).__bindgen_anon_1.obj).name as *const i8);
                let field = CStr::from_ptr(hlp_field_name(hfield) as *const i8);

                hlp_error(str_to_uchar_ptr(
                    format!(
                        "{}  not have field {}",
                        name.to_string_lossy(),
                        field.to_string_lossy()
                    )
                    .as_str(),
                ));
            }
            *ft = (*f).t;
            (d as *mut u8).offset((*f).field_index as isize) as *mut c_void
        }
        hl::hl_type_kind_HSTRUCT => {
            let f = obj_resolve_field((*(*d).t).__bindgen_anon_1.obj, hfield);

            if f.is_null() || (*f).field_index < 0 {
                let name = CStr::from_ptr((*(*(*d).t).__bindgen_anon_1.obj).name as *const i8);
                let field = CStr::from_ptr(hlp_field_name(hfield) as *const i8);
                hlp_error(str_to_uchar_ptr(
                    format!(
                        "{}  not have field {}",
                        name.to_string_lossy(),
                        field.to_string_lossy()
                    )
                    .as_str(),
                ));
            }
            *ft = (*f).t;
            ((*d).v.ptr as *mut u8).offset((*f).field_index as isize) as *mut c_void
        }
        hl::hl_type_kind_HVIRTUAL => {
            let v = d as *mut vvirtual;
            if !(*v).value.is_null() {
                return hlp_obj_lookup_set((*v).value, hfield, t, ft);
            }
            let f = hlp_lookup_find(
                (*(*(*v).t).__bindgen_anon_1.virt).lookup,
                (*(*(*v).t).__bindgen_anon_1.virt).nfields,
                hfield,
            );
            if f.is_null() || !hlp_safe_cast(t, (*f).t) {
                return hlp_obj_lookup_set(hlp_virtual_make_value(v), hfield, t, ft);
            }
            *ft = (*f).t;
            (v as *mut u8).offset(
                *(*(*(*v).t).__bindgen_anon_1.virt)
                    .indexes
                    .offset((*f).field_index as isize) as isize,
            ) as *mut c_void
        }
        _ => {
            hlp_error(str_to_uchar_ptr("Invalid field access"));
            ptr::null_mut()
        }
    }
}

unsafe fn hlp_obj_lookup_extra(d: *mut vdynamic, hfield: i32) -> *mut vdynamic {
    match (*(*d).t).kind {
        hl::hl_type_kind_HOBJ | hl::hl_type_kind_HSTRUCT => {
            let obj = (*(*d).t).__bindgen_anon_1.obj;
            let f = obj_resolve_field(obj, hfield);
            if !f.is_null() && (*f).field_index < 0 {
                // Through the canonical allocator so the closure gets the
                // method's CLOSURE type (this stripped, parent set) — the
                // raw allocate_closure_ptr call here kept the full type and
                // made `untyped obj.method(...)` chains fail their fun->fun
                // cast one arg too wide (Issue5082).
                let closure = crate::fun::hlp_alloc_closure_ptr(
                    (*f).t,
                    *(*(*obj).rt).methods.offset(-(*f).field_index as isize - 1),
                    d as *mut std::ffi::c_void,
                );
                return closure as *mut vdynamic;
            }
            if f.is_null() {
                let rt = (*obj).rt;
                if (*rt).getFieldFun.is_some() {
                    return (*rt).getFieldFun.unwrap()(d, hfield);
                }
                return get_field_via_stub(d, hfield);
            }
            ptr::null_mut()
        }
        hl::hl_type_kind_HVIRTUAL => {
            let v = (*(d as *mut vvirtual)).value;
            if !v.is_null() {
                return hlp_obj_lookup_extra(v, hfield);
            }
            ptr::null_mut()
        }
        _ => ptr::null_mut(),
    }
}

/// Invoke an object's `__get_field` when its runtime slot could not retain it.
///
/// `getFieldFun`, like `castFun`, is a bare C function pointer.  In interpreter
/// mode the method table contains findex+1 sentinels instead, so
/// `hlp_get_obj_rt` must leave that slot empty.  Dynamic property reads still
/// have to run `__get_field`, though: ArrayDyn exposes its computed `length`
/// exclusively through this hook.  Re-resolve the method and route a stub
/// through the registered closure runner, boxing the field hash because that
/// bridge carries `vdynamic*` arguments.
unsafe fn get_field_via_stub(d: *mut vdynamic, hfield: i32) -> *mut vdynamic {
    if d.is_null() || (*d).t.is_null() {
        return ptr::null_mut();
    }
    let t = (*d).t;
    if (*t).kind != hl_type_kind_HOBJ && (*t).kind != hl_type_kind_HSTRUCT {
        return ptr::null_mut();
    }
    let obj = (*t).__bindgen_anon_1.obj;
    if obj.is_null() {
        return ptr::null_mut();
    }

    // obj_resolve_field walks the runtime parent chain, so initialize it
    // before asking for an inherited __get_field.
    let mut rt = (*obj).rt;
    if rt.is_null() || (*rt).methods.is_null() {
        rt = hl_get_obj_proto(t);
    }
    if rt.is_null() || (*rt).methods.is_null() {
        return ptr::null_mut();
    }
    let f = obj_resolve_field(obj, hlp_hash_gen(USTR_GET_FIELD.as_ptr(), false));
    if f.is_null() || (*f).field_index >= 0 {
        return ptr::null_mut();
    }
    let idx = (-(*f).field_index - 1) as usize;
    if idx >= (*rt).nmethods as usize {
        return ptr::null_mut();
    }
    let fptr = *(*rt).methods.add(idx);
    let addr = fptr as usize;
    if addr == 0 || addr >= 0x100000 {
        return ptr::null_mut(); // real code is handled by getFieldFun
    }
    let Some(runner) = crate::fiber::closure_runner() else {
        return ptr::null_mut();
    };

    let mut cl = vclosure {
        t: (*f).t,
        fun: fptr,
        hasValue: 1,
        stackCount: 0,
        value: d as *mut c_void,
    };
    let mut hash = hfield;
    let boxed_hash =
        crate::cast::hlp_make_dyn((&mut hash as *mut i32).cast(), crate::types::hlt_i32());
    if boxed_hash.is_null() {
        return ptr::null_mut();
    }
    let mut arg = boxed_hash;
    runner(&mut cl, &mut arg, 1)
}

unsafe fn hlp_dynobj_remap_virtuals(
    o: *mut vdynobj,
    f: *mut hl_field_lookup,
    address_offset: isize,
) {
    let mut v = (*o).virtuals;
    let is_ptr = hl_is_ptr((*f).t);

    while !v.is_null() {
        let vf = hlp_lookup_find(
            (*(*(*v).t).__bindgen_anon_1.virt).lookup,
            (*(*(*v).t).__bindgen_anon_1.virt).nfields,
            (*f).hashed_name,
        );

        if address_offset != 0 {
            for i in 0..(*(*(*v).t).__bindgen_anon_1.virt).nfields as usize {
                let vfields = hl_vfields(v);
                if !(*vfields.add(i)).is_null()
                    && hl_is_ptr((*(*(*(*v).t).__bindgen_anon_1.virt).fields.add(i)).t) == is_ptr
                {
                    *(vfields.add(i) as *mut *mut u8) =
                        (*vfields.add(i) as *mut u8).offset(address_offset);
                }
            }
        }

        if !vf.is_null() {
            let vfields = hl_vfields(v);
            *vfields.add((*vf).field_index as usize) = if hlp_same_type((*vf).t, (*f).t) {
                hlp_dynobj_field(o, f)
            } else {
                ptr::null_mut()
            };
        }

        v = (*v).next;
    }
}

// Helper function to get the virtual fields of a vvirtual
unsafe fn hl_vfields(v: *mut vvirtual) -> *mut *mut std::ffi::c_void {
    (v as *mut u8).add(mem::size_of::<vvirtual>()) as *mut *mut std::ffi::c_void
}

#[no_mangle]
pub unsafe extern "C" fn hlp_dynobj_add_field(
    o: *mut vdynobj,
    hfield: i32,
    t: *mut hl_type,
) -> *mut hl_field_lookup {
    let index: i32;
    let address_offset: isize;


    // expand data
    if hl_is_ptr(t) {
        index = (*o).nvalues;
        if index > HL_DYNOBJ_INDEX_MASK as i32 {
            hlp_error(str_to_uchar_ptr("Too many dynobj values\0"));
        }
        let nvalues = crate::gc::gc_alloc(((*o).nvalues as usize + 1) * mem::size_of::<*mut c_void>())
            .expect("Failed to allocate memory")
            .as_ptr() as *mut *mut c_void;
        ptr::copy_nonoverlapping((*o).values, nvalues, (*o).nvalues as usize);
        *nvalues.add(index as usize) = ptr::null_mut();
        address_offset = (nvalues as *mut i8).offset_from((*o).values as *mut i8);
        (*o).values = nvalues;
        (*o).nvalues += 1;
    } else {
        let mut raw_size = 0;
        for i in 0..(*o).nfields as usize {
            let f = (*o).lookup.add(i);
            if hl_is_ptr((*f).t) {
                continue;
            }
            raw_size += hlp_pad_size(raw_size, (*f).t);
            raw_size += hlp_type_size((*f).t) as i32;
        }
        if raw_size > (*o).raw_size {
            raw_size = (*o).raw_size;
        }
        let pad = hlp_pad_size(raw_size, t) as usize;
        let size = hlp_type_size(t) as usize;

        if raw_size as usize + pad > HL_DYNOBJ_INDEX_MASK as usize {
            hlp_error(str_to_uchar_ptr("Too many dynobj values\0"));
        }

        let new_data = crate::gc::gc_alloc(raw_size as usize + pad + size)
            .expect("Failed to allocate memory")
            .as_ptr() as *mut i8;
        if raw_size == (*o).raw_size {
            ptr::copy_nonoverlapping((*o).raw_data, new_data, (*o).raw_size as usize);
        } else {
            raw_size = 0;
            for i in 0..(*o).nfields as usize {
                let f = (*o).lookup.add(i);
                let index = (*f).field_index & HL_DYNOBJ_INDEX_MASK as i32;
                if hl_is_ptr((*f).t) {
                    continue;
                }
                raw_size += hlp_pad_size(raw_size, (*f).t);
                ptr::copy_nonoverlapping(
                    (*o).raw_data.add(index as usize),
                    new_data.add(raw_size as usize),
                    hlp_type_size((*f).t) as usize,
                );
                (*f).field_index =
                    raw_size | ((hlp_dynobj_order(f) << HL_DYNOBJ_INDEX_SHIFT) as i32);
                if index != raw_size {
                    hlp_dynobj_remap_virtuals(o, f, 0);
                }
                raw_size += hlp_type_size((*f).t) as i32;
            }
            (*o).raw_size = raw_size;
        }
        address_offset = new_data.offset_from((*o).raw_data);
        (*o).raw_data = new_data;
        (*o).raw_size += pad as i32;
        index = (*o).raw_size;
        (*o).raw_size += size as i32;
    }

    // update field table
    let new_lookup = crate::gc::gc_alloc(mem::size_of::<hl_field_lookup>() * ((*o).nfields as usize + 1))
        .expect("Failed to allocate memory")
        .as_ptr() as *mut hl_field_lookup;
    let field_pos = hlp_lookup_find_index((*o).lookup, (*o).nfields, hfield);
    ptr::copy_nonoverlapping((*o).lookup, new_lookup, field_pos as usize);
    let f = new_lookup.add(field_pos as usize);
    (*f).t = t;
    (*f).hashed_name = hfield;
    (*f).field_index = index | ((*o).nfields << HL_DYNOBJ_INDEX_SHIFT);
    ptr::copy_nonoverlapping(
        (*o).lookup.add(field_pos as usize),
        new_lookup.add((field_pos + 1) as usize),
        ((*o).nfields - field_pos) as usize,
    );
    (*o).nfields += 1;
    (*o).lookup = new_lookup;

    hlp_dynobj_remap_virtuals(o, f, address_offset);
    f
}

unsafe fn hlp_dynobj_delete_field(o: *mut vdynobj, f: *mut hl_field_lookup) {
    let order = hlp_dynobj_order(f);
    let index = (*f).field_index & HL_DYNOBJ_INDEX_MASK as i32;
    let is_ptr = hl_is_ptr((*f).t);

    // erase data
    if is_ptr {
        ptr::copy(
            (*o).values.add(index as usize + 1),
            (*o).values.add(index as usize),
            (*o).nvalues as usize - (index as usize + 1),
        );
        (*o).nvalues -= 1;
        *(*o).values.add((*o).nvalues as usize) = ptr::null_mut();
        for i in 0..(*o).nfields as usize {
            let f = (*o).lookup.add(i);
            if hl_is_ptr((*f).t) && ((*f).field_index & HL_DYNOBJ_INDEX_MASK as i32) > index {
                (*f).field_index -= 1;
            }
        }
    } else {
        // no erase needed, compaction will be performed on next add
    }

    // remove from virtuals
    let mut v = (*o).virtuals;
    while !v.is_null() {
        let vf = hlp_lookup_find(
            (*(*(*v).t).__bindgen_anon_1.virt).lookup,
            (*(*(*v).t).__bindgen_anon_1.virt).nfields,
            (*f).hashed_name,
        );
        if !vf.is_null() {
            *hl_vfields(v).add((*vf).field_index as usize) = ptr::null_mut();
        }
        // remap pointers that were moved
        if is_ptr {
            for i in 0..(*(*(*v).t).__bindgen_anon_1.virt).nfields as usize {
                let vf = (*(*(*v).t).__bindgen_anon_1.virt).lookup.add(i);
                if hl_is_ptr((*vf).t) {
                    let pf = hl_vfields(v).add((*vf).field_index as usize) as *mut *mut *mut c_void;
                    if !(*pf).is_null() && *pf > (*o).values.add(index as usize) {
                        *pf = (*pf).sub(1);
                    }
                }
            }
        }
        v = (*v).next;
    }

    // remove from lookup
    let field = (f as usize - (*o).lookup as usize) / mem::size_of::<hl_field_lookup>();
    ptr::copy(
        (*o).lookup.add(field + 1),
        (*o).lookup.add(field),
        (*o).nfields as usize - (field + 1),
    );
    (*o).nfields -= 1;

    // remap order indexes
    for i in 0..(*o).nfields as usize {
        let f = (*o).lookup.add(i);
        if hlp_dynobj_order(f) > order {
            (*f).field_index -= 1 << HL_DYNOBJ_INDEX_SHIFT;
        }
    }
}

#[inline]
pub fn hlp_pad_size(size: i32, t: *mut hl::hl_type) -> i32 {
    unsafe {
        if (*t).kind == hl::hl_type_kind_HVOID {
            0
        } else {
            (-size) & (hlp_type_size(t) as i32 - 1)
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_alloc_dynobj() -> *mut vdynobj {

    // Allocate memory for the vdynobj structure
    let obj = crate::gc::gc_alloc(mem::size_of::<vdynobj>())
        .expect("Failed to allocate memory for vdynobj")
        .as_ptr() as *mut vdynobj;

    // Initialize the fields
    ptr::write(
        obj,
        vdynobj {
            t: crate::types::hlt_dynobj(),
            lookup: ptr::null_mut(),
            raw_data: ptr::null_mut(),
            values: ptr::null_mut(),
            nfields: 0,
            raw_size: 0,
            nvalues: 0,
            virtuals: ptr::null_mut(),
        },
    );

    // Allocate initial memory for lookup table (we'll start with a small size)
    let initial_lookup_size = 4; // Starting with space for 4 fields
    (*obj).lookup = crate::gc::gc_alloc(mem::size_of::<hl_field_lookup>() * initial_lookup_size)
        .expect("Failed to allocate memory for lookup table")
        .as_ptr() as *mut hl_field_lookup;

    // Allocate initial memory for values (we'll start with a small size)
    let initial_values_size = 4; // Starting with space for 4 values
    (*obj).values = crate::gc::gc_alloc(mem::size_of::<*mut std::ffi::c_void>() * initial_values_size)
        .expect("Failed to allocate memory for values")
        .as_ptr() as *mut *mut std::ffi::c_void;

    // We don't allocate raw_data yet, as its size depends on the fields that will be added

    obj
}

#[no_mangle]
pub unsafe extern "C" fn hlp_virtual_make_value(v: *mut vvirtual) -> *mut vdynamic {
    if !(*v).value.is_null() {
        return (*v).value;
    }

    let nfields = (*(*(*v).t).__bindgen_anon_1.virt).nfields;
    let o = hlp_alloc_dynobj();
    let mut raw_size = 0;
    let mut nvalues = 0;

    // Copy the lookup table
    (*o).lookup = crate::gc::gc_alloc(mem::size_of::<hl_field_lookup>() * nfields as usize)
        .expect("Failed to allocate memory")
        .as_ptr() as *mut hl_field_lookup;
    (*o).nfields = nfields;
    ptr::copy_nonoverlapping(
        (*(*(*v).t).__bindgen_anon_1.virt).lookup,
        (*o).lookup,
        nfields as usize,
    );

    for i in 0..nfields as usize {
        let f = (*o).lookup.add(i);
        if hl_is_ptr((*f).t) {
            (*f).field_index = nvalues;
            nvalues += 1;
        } else {
            raw_size += hlp_pad_size(raw_size, (*f).t);
            (*f).field_index = raw_size;
            raw_size += hlp_type_size((*f).t) as i32;
        }
        if (*f).field_index > HL_DYNOBJ_INDEX_MASK as i32 {
            hlp_error(str_to_uchar_ptr("Too many dynobj fields\0"));
        }
        (*f).field_index |= (i as i32) << HL_DYNOBJ_INDEX_SHIFT;
    }

    // Copy the data & rebind virtual addresses
    (*o).raw_data = crate::gc::gc_alloc(raw_size as usize)
        .expect("Failed to allocate memory")
        .as_ptr() as *mut i8;
    (*o).raw_size = raw_size;
    (*o).values = crate::gc::gc_alloc(nvalues as usize * mem::size_of::<*mut std::ffi::c_void>())
        .expect("Failed to allocate memory")
        .as_ptr() as *mut *mut std::ffi::c_void;
    (*o).nvalues = nvalues;

    for i in 0..nfields as usize {
        let f = (*o).lookup.add(i);
        let vf = (*(*(*v).t).__bindgen_anon_1.virt).lookup.add(i);
        let vaddr = hl_vfields(v).add((*vf).field_index as usize);
        ptr::copy_nonoverlapping(
            *vaddr as *const u8,
            hlp_dynobj_field(o, f) as *mut u8,
            hlp_type_size((*f).t) as usize,
        );
        *vaddr = hlp_dynobj_field(o, f);
    }

    // Erase virtual data
    ptr::write_bytes(
        hl_vfields(v).add(nfields as usize) as *mut u8,
        0,
        (*(*(*v).t).__bindgen_anon_1.virt).dataSize as usize,
    );
    (*o).virtuals = v;
    (*v).value = o as *mut vdynamic;
    (*v).value
}

#[no_mangle]
pub unsafe extern "C" fn hlp_dyn_getp(
    d: *mut vdynamic,
    hfield: i32,
    t: *mut hl_type,
) -> *mut std::ffi::c_void {
    let mut ft: *mut hl_type = ptr::null_mut();

    // Assuming hl_track_call and on_dynfield are defined elsewhere
    // hl_track_call(HL_TRACK_DYNFIELD, on_dynfield(d, hfield));

    let dyn_type = crate::types::hlt_dyn();

    let addr = hlp_obj_lookup(d, hfield, &mut ft);

    if addr.is_null() {
        let d = hlp_obj_lookup_extra(d, hfield);
        if d.is_null() {
            return ptr::null_mut();
        } else {
            return hlp_dyn_castp(&d as *const _ as *mut std::ffi::c_void, dyn_type, t);
        }
    }

    if hlp_same_type(t, ft) {
        *(addr as *mut *mut std::ffi::c_void)
    } else {
        hlp_dyn_castp(addr, ft, t)
    }
}

#[no_mangle]
pub unsafe extern "C" fn hl_to_virtual(vt: *mut hl_type, obj: *mut vdynamic) -> *mut vvirtual {
    if obj.is_null() {
        return ptr::null_mut();
    }
    if (*obj).t.is_null() {
        return ptr::null_mut();
    }
    let _obj_kind = (*(*obj).t).kind;
    let _vt_nfields = (*vt).__bindgen_anon_1.virt.as_ref().unwrap().nfields;

    // Lazily initialize the virtual type (lookup + indexes + dataSize) —
    // same pattern as hlp_alloc_virtual above. The interpreter initializes
    // virtual types lazily, so promoted JIT code can reach one first (this
    // used to be a debug panic "virtual not initialized", aborting the VM
    // from JIT frames that cannot unwind). hlp_init_virtual ignores its
    // module-context argument and is self-contained.
    {
        let virt = (*vt).__bindgen_anon_1.virt.as_ref().unwrap();
        if virt.nfields != 0 && (virt.lookup.is_null() || virt.indexes.is_null()) {
            hlp_init_virtual(vt, ptr::null_mut());
        }
    }


    match (*obj).t.as_ref().unwrap().kind {
        hl::hl_type_kind_HOBJ => {
            let mut v: *mut vvirtual;
            let mut interface_address: *mut *mut vvirtual = ptr::null_mut();

            let mut rt = (*(*obj).t).__bindgen_anon_1.obj.as_ref().unwrap().rt;
            while !rt.is_null() {
                for i in 0..(*rt).ninterfaces as usize {
                    let fi = *(*rt).interfaces.add(i) as usize;
                    let tobj = (*(*rt).t).__bindgen_anon_1.obj;
                    if (*(*tobj).fields.add(fi)).t == vt {
                        let start = if (*rt).parent.is_null() {
                            0
                        } else {
                            (*(*rt).parent).nfields as usize
                        };
                        let offset = *(*rt).fields_indexes.add(fi + start) as usize;
                        interface_address = (obj as *mut u8).add(offset) as *mut *mut vvirtual;
                        break;
                    }
                }
                rt = (*rt).parent;
            }

            if !interface_address.is_null() {
                v = *interface_address;
                if !v.is_null() {
                    return v;
                }
            }

            v = crate::gc::gc_alloc(
                    mem::size_of::<vvirtual>()
                        + mem::size_of::<*mut std::ffi::c_void>()
                            * (*vt).__bindgen_anon_1.virt.as_ref().unwrap().nfields as usize,
                )
                .expect("Memory allocation failed")
                .as_ptr() as *mut vvirtual;
            (*v).t = vt;
            (*v).value = obj as *mut _;
            (*v).next = ptr::null_mut();

            let nf = (*vt).__bindgen_anon_1.virt.as_ref().unwrap().nfields as usize;
            for i in 0..nf {
                let virt_field = (*vt)
                    .__bindgen_anon_1
                    .virt
                    .as_ref()
                    .unwrap()
                    .fields
                    .add(i)
                    .as_ref()
                    .unwrap();
                let f = obj_resolve_field(
                    (*(*obj).t).__bindgen_anon_1.obj.as_ref().unwrap(),
                    virt_field.hashed_name,
                );
                if !f.is_null() && (*f).field_index < 0 {
                    let ft = (*vt)
                        .__bindgen_anon_1
                        .virt
                        .as_ref()
                        .unwrap()
                        .fields
                        .add(i)
                        .as_ref()
                        .unwrap()
                        .t;
                    let mut tmp = hl_type {
                        kind: hl::hl_type_kind_HMETHOD,
                        __bindgen_anon_1: hl_type__bindgen_ty_1 {
                            fun: ptr::null_mut(),
                        },
                        vobj_proto: ptr::null_mut(),
                        mark_bits: ptr::null_mut(),
                    };
                    let mut tf = hl::hl_type_fun {
                        args: (*(*f).t).__bindgen_anon_1.fun.as_ref().unwrap().args.add(1),
                        nargs: (*(*f).t).__bindgen_anon_1.fun.as_ref().unwrap().nargs - 1,
                        ret: (*(*f).t).__bindgen_anon_1.fun.as_ref().unwrap().ret,
                        parent: ptr::null_mut(),
                        closure_type: hl_type_fun__bindgen_ty_1 {
                            kind: hl_type_kind_HDYN,
                            p: ptr::null_mut(),
                        },
                        closure: hl_type_fun__bindgen_ty_2 {
                            args: ptr::null_mut(),
                            ret: ptr::null_mut(),
                            nargs: 0,
                            parent: ptr::null_mut(),
                        },
                    };
                    tmp.__bindgen_anon_1.fun = &mut tf;
                    let cast_ok = hlp_safe_cast(&mut tmp, ft);
                    if cast_ok {
                        let method_idx = (-(*f).field_index - 1) as usize;
                        let rt = (*(*obj).t).__bindgen_anon_1.obj.as_ref().unwrap().rt;
                        *(hl_vfields(v).add(i)) = *(*rt).methods.wrapping_add(method_idx);
                    } else {
                        *(hl_vfields(v).add(i)) = ptr::null_mut();
                    }
                } else {
                    *(hl_vfields(v).add(i)) = if f.is_null()
                        || !hlp_same_type(
                            (*f).t,
                            (*vt)
                                .__bindgen_anon_1
                                .virt
                                .as_ref()
                                .unwrap()
                                .fields
                                .add(i)
                                .as_ref()
                                .unwrap()
                                .t,
                        ) {
                        ptr::null_mut()
                    } else {
                        (obj as *mut u8).add((*f).field_index as usize) as *mut _
                    };
                }
            }

            if !interface_address.is_null() {
                *interface_address = v;
            }

            v
        }
        hl::hl_type_kind_HDYNOBJ => {
            let o = obj as *mut vdynobj;
            let mut v = (*o).virtuals;
            while !v.is_null() {
                if (*(*v).t).__bindgen_anon_1.virt == (*vt).__bindgen_anon_1.virt {
                    return v;
                }
                v = (*v).next;
            }

            let mut need_recast: i64 = 0;
            v = crate::gc::gc_alloc(
                    mem::size_of::<vvirtual>()
                        + mem::size_of::<*mut std::ffi::c_void>()
                            * (*vt).__bindgen_anon_1.virt.as_ref().unwrap().nfields as usize,
                )
                .unwrap()
                .as_ptr() as *mut vvirtual;
            (*v).t = vt;
            (*v).value = obj as *mut _;

            for i in 0..(*vt).__bindgen_anon_1.virt.as_ref().unwrap().nfields as usize {
                let f = hlp_lookup_find(
                    (*o).lookup,
                    (*o).nfields,
                    (*vt)
                        .__bindgen_anon_1
                        .virt
                        .as_ref()
                        .unwrap()
                        .fields
                        .add(i)
                        .as_ref()
                        .unwrap()
                        .hashed_name,
                );
                let vft = (*vt)
                    .__bindgen_anon_1
                    .virt
                    .as_ref()
                    .unwrap()
                    .fields
                    .add(i)
                    .as_ref()
                    .unwrap()
                    .t;
                let addr = if f.is_null() || !hlp_same_type((*f).t, vft) {
                    ptr::null_mut()
                } else {
                    hlp_dynobj_field(o, f)
                };

                if addr.is_null()
                    && !f.is_null()
                    && (*o).virtuals.is_null()
                    && should_recast((*f).t, vft)
                {
                    need_recast |= 1 << i;
                }
                *(hl_vfields(v).add(i)) = addr;
            }

            (*v).next = (*o).virtuals;
            (*o).virtuals = v;

            if need_recast != 0 {
                let extra_check = (*vt).__bindgen_anon_1.virt.as_ref().unwrap().nfields > 63;
                for i in 0..(*vt).__bindgen_anon_1.virt.as_ref().unwrap().nfields as usize {
                    if (need_recast & (1 << i)) != 0 {
                        let f = (*vt).__bindgen_anon_1.virt.as_ref().unwrap().fields.add(i);
                        if extra_check
                            && hlp_lookup_find((*o).lookup, (*o).nfields, (*f).hashed_name)
                                .is_null()
                        {
                            continue;
                        }
                        if hl_is_ptr((*f).t) {
                            hlp_dyn_setp(
                                obj,
                                (*f).hashed_name,
                                (*f).t,
                                hlp_dyn_getp(obj, (*f).hashed_name, (*f).t),
                            );
                        } else if (*(*f).t).kind == hl::hl_type_kind_HF64 {
                            hlp_dyn_setd(
                                obj,
                                (*f).hashed_name,
                                hlp_dyn_getd(obj, (*f).hashed_name),
                            );
                        }
                    }
                }
            }

            v
        }
        hl::hl_type_kind_HVIRTUAL => {
            if hlp_safe_cast((*obj).t, vt) {
                obj as *mut vvirtual
            } else {
                hl_to_virtual(vt, hlp_virtual_make_value(obj as *mut vvirtual))
            }
        }
        _ => {
            hlp_error(str_to_uchar_ptr(&format!(
                "Can't cast {} to {}",
                CStr::from_ptr(hlp_type_str((*obj).t) as *const i8).to_string_lossy(),
                CStr::from_ptr(hlp_type_str(vt) as *const i8).to_string_lossy(),
            )));
            ptr::null_mut()
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_get_virtual_value(v: *mut vdynamic) -> *mut vdynamic {
    // Reached from Reflect with whatever a Dynamic slot held, which is not
    // always a virtual (nor always a box). Two dereferences deep with no
    // check aborted the VM on TestJson and TestReflect.
    if v.is_null() {
        return ptr::null_mut();
    }
    let inner = (*v).v.ptr as *mut vvirtual;
    if inner.is_null()
        || (inner as usize) < 0x10000
        || !(inner as usize).is_multiple_of(std::mem::align_of::<usize>())
    {
        return ptr::null_mut();
    }
    (*inner).value
}

/// Invoke a resolved method pointer with `this` as the only argument.
/// Stub sentinels (interpreter findex+1, < 0x100000) are routed through the
/// registered closure runner (interpreter re-entry) instead of being called —
/// calling one from native code is a guaranteed SIGBUS.
unsafe fn vcall_fn_or_stub(fun: *mut c_void, this: *mut vdynamic) -> *mut vdynamic {
    let addr = fun as usize;
    if addr == 0 {
        return ptr::null_mut();
    }
    if addr < 0x100000 {
        if let Some(runner) = crate::fiber::closure_runner() {
            let mut cl = vclosure {
                t: crate::types::hlt_dyn(),
                fun,
                hasValue: 1,
                stackCount: 0,
                value: this as *mut c_void,
            };
            return runner(&mut cl, ptr::null_mut(), 0);
        }
        return ptr::null_mut();
    }
    let method_fn: unsafe extern "C" fn(*mut vdynamic) -> *mut vdynamic = std::mem::transmute(fun);
    method_fn(this)
}

/// Resolve a virtual method call target by field hash, for JIT code.
///
/// `target` is whatever an HVIRTUAL-typed register held at runtime — a
/// vvirtual, or (hybrid interpreter boundary) a plain HOBJ/HDYNOBJ. Unwraps
/// vvirtual wrappers, resolves the method on the concrete value, writes the
/// `this` pointer to `out_this`, and returns the raw method pointer — which
/// may be an interpreter stub sentinel (findex+1): the JIT calls the result
/// through its stub-guarded indirect call, so sentinels re-enter the
/// interpreter with full, correctly-typed arguments.
#[no_mangle]
pub unsafe extern "C" fn hlp_vresolve_method_hashed(
    target: *mut vdynamic,
    hfield: i32,
    out_this: *mut *mut vdynamic,
) -> *mut c_void {
    if !out_this.is_null() {
        *out_this = ptr::null_mut();
    }
    // Unwrap vvirtual wrappers to the concrete backing value.
    let mut cur = target;
    loop {
        if cur.is_null() {
            return ptr::null_mut();
        }
        let t = (*cur).t;
        if t.is_null() {
            return ptr::null_mut();
        }
        if (*t).kind == hl::hl_type_kind_HVIRTUAL {
            cur = (*(cur as *mut vvirtual)).value;
            continue;
        }
        break;
    }
    let t = (*cur).t;
    match (*t).kind {
        hl::hl_type_kind_HOBJ => {
            let Some(tobj) = (*t).__bindgen_anon_1.obj.as_ref() else {
                return ptr::null_mut();
            };
            let f = obj_resolve_field(tobj, hfield);
            if f.is_null() || (*f).field_index >= 0 {
                return ptr::null_mut();
            }
            let mut rt = tobj.rt;
            if rt.is_null() || (*rt).methods.is_null() {
                rt = hl_get_obj_proto(t);
            }
            if rt.is_null() || (*rt).methods.is_null() {
                return ptr::null_mut();
            }
            let method_idx = (-(*f).field_index - 1) as usize;
            if method_idx >= (*rt).nmethods as usize {
                return ptr::null_mut();
            }
            if !out_this.is_null() {
                *out_this = cur;
            }
            *(*rt).methods.add(method_idx)
        }
        hl::hl_type_kind_HDYNOBJ => {
            // Dynamic object: the "method" is a closure-valued field.
            let cl = hlp_dyn_getp(cur, hfield, crate::types::hlt_dyn()) as *mut vclosure;
            if cl.is_null() {
                return ptr::null_mut();
            }
            if !out_this.is_null() && (*cl).hasValue != 0 {
                *out_this = (*cl).value as *mut vdynamic;
            }
            (*cl).fun
        }
        _ => ptr::null_mut(),
    }
}

/// Invoke a type's `__cast` when the runtime slot could not hold it.
///
/// `hl_runtime_obj.castFun` is a bare C function pointer, so it can only
/// hold real code — an interpreter stub sentinel (findex+1) stored there
/// would be called as an address and SIGBUS. hlp_get_obj_rt therefore
/// stores None for a stub, which is correct AND means `__cast` never runs
/// under the interpreter at all: every method is a stub there. Casts that
/// Haxe defines entirely in terms of __cast — Array<Dynamic> to Array<Int>
/// and friends, via ArrayBase.__cast — then failed with invalid_cast.
///
/// This re-resolves `__cast` on the object's own type and calls it through
/// the interpreter bridge when it is a stub. Returns null when the type has
/// no __cast, when it is real code (the caller's castFun path already
/// handled that), or when no bridge is registered.
pub(crate) unsafe fn cast_via_stub_castfun(
    t: *mut hl_type,
    obj: *mut vdynamic,
    to: *mut hl_type,
) -> *mut vdynamic {
    if t.is_null() || obj.is_null() {
        return ptr::null_mut();
    }
    let Some(o) = (*t).__bindgen_anon_1.obj.as_ref() else {
        return ptr::null_mut();
    };
    // Build the runtime object FIRST: obj_resolve_field walks rt->parent, so
    // with a null rt it answers "no such field" for every field there is.
    let mut rt = o.rt;
    if rt.is_null() || (*rt).methods.is_null() {
        rt = hl_get_obj_proto(t);
    }
    if rt.is_null() || (*rt).methods.is_null() {
        return ptr::null_mut();
    }
    let f = obj_resolve_field(o, hlp_hash_gen(USTR_CAST.as_ptr(), false));
    if f.is_null() || (*f).field_index >= 0 {
        if env_flag!("ASH_DBG_SC") {
            eprintln!("[stub-cast] no __cast on type {:p}", t);
        }
        return ptr::null_mut();
    }
    let idx = (-(*f).field_index - 1) as usize;
    if idx >= (*rt).nmethods as usize {
        return ptr::null_mut();
    }
    let fptr: *mut c_void = *(*rt).methods.add(idx);
    let addr = fptr as usize;
    if addr == 0 || addr >= 0x100000 {
        return ptr::null_mut(); // real code: the castFun path owns it
    }
    let Some(runner) = crate::fiber::closure_runner() else {
        if env_flag!("ASH_DBG_SC") {
            eprintln!("[stub-cast] no closure runner registered");
        }
        return ptr::null_mut();
    };
    if env_flag!("ASH_DBG_SC") {
        eprintln!("[stub-cast] invoking __cast stub {:#x} to={:p}", addr, to);
    }
    // __cast(this, toType) -> Dynamic. `this` rides in the closure, the
    // target type is the single argument; hl_type* is passed as the opaque
    // pointer the callee expects.
    let mut cl = vclosure {
        t: (*f).t,
        fun: fptr,
        hasValue: 1,
        stackCount: 0,
        value: obj as *mut c_void,
    };
    // The bridge's contract is an array of vdynamic*, so the target type is
    // boxed rather than passed raw: an hl_type* handed over as if it were a
    // box gets its `kind` word read as a type pointer.
    let mut t_slot = to;
    let boxed = crate::cast::hlp_make_dyn(
        &mut t_slot as *mut _ as *mut c_void,
        crate::types::hlt_type(),
    );
    let mut arg: *mut vdynamic = boxed;
    runner(&mut cl, &mut arg as *mut *mut vdynamic, 1)
}

/// A varray of HDYN elements, for callers (JIT-emitted code) that need to
/// stage boxed arguments without holding a dyn type pointer of their own.
#[no_mangle]
pub unsafe extern "C" fn hlp_alloc_dyn_array(n: i32) -> *mut varray {
    crate::array::hlp_alloc_array(crate::types::hlt_dyn(), n)
}

/// Boxed virtual-method call: resolve by field hash, call the method through
/// its OWN runtime type, return the result boxed.
///
/// This is the semantic HashLink's hl_dyn_call_obj implements, and the part
/// that matters is whose signature the call uses. A virtual declares the
/// field's type as the INTERFACE sees it — Iterator<Int>.next is () -> i32 —
/// but the implementation behind it may be generic, compiled as () -> Dynamic.
/// The JIT used to call the resolved pointer with the DECLARED signature,
/// which read the low 32 bits of a returned vdynamic* as the Int: map
/// iteration over Int values returned truncated box addresses. Here the
/// method's own hl_type comes from the module context every hl_type_obj
/// carries (tobj.m -> functions_types[findex]), and hlp_call_method's
/// marshaller does the rest; the caller unboxes the result to the declared
/// kind, which is a defined dyn cast rather than an ABI pun.
///
/// Returns null when the method cannot be resolved or typed; the caller
/// stores the declared kind's zero, same as before.
#[no_mangle]
pub unsafe extern "C" fn hlp_vcall_dyn(
    target: *mut vdynamic,
    hfield: i32,
    args: *mut varray,
) -> *mut vdynamic {
    // Unwrap vvirtual wrappers to the concrete backing value.
    let mut cur = target;
    loop {
        if cur.is_null() {
            return ptr::null_mut();
        }
        let t = (*cur).t;
        if t.is_null() {
            return ptr::null_mut();
        }
        if (*t).kind == hl::hl_type_kind_HVIRTUAL {
            cur = (*(cur as *mut vvirtual)).value;
            continue;
        }
        break;
    }
    let t = (*cur).t;
    let nargs = if args.is_null() { 0 } else { (*args).size };
    match (*t).kind {
        hl::hl_type_kind_HOBJ => {
            let Some(tobj) = (*t).__bindgen_anon_1.obj.as_ref() else {
                return ptr::null_mut();
            };
            let f = obj_resolve_field(tobj, hfield);
            if f.is_null() || (*f).field_index >= 0 {
                return ptr::null_mut();
            }
            let mut rt = tobj.rt;
            if rt.is_null() || (*rt).methods.is_null() {
                rt = hl_get_obj_proto(t);
            }
            if rt.is_null() || (*rt).methods.is_null() {
                return ptr::null_mut();
            }
            let method_idx = (-(*f).field_index - 1) as usize;
            if method_idx >= (*rt).nmethods as usize {
                return ptr::null_mut();
            }
            let method_ptr: *mut c_void = *(*rt).methods.add(method_idx);
            // The findex, for the method's own type: walk the declaring chain
            // for the proto entry with this hash. obj_resolve_field already
            // proved one exists somewhere on the chain.
            let mut findex: Option<usize> = None;
            let mut scan_t = t;
            'chain: while !scan_t.is_null() {
                let Some(so) = (*scan_t).__bindgen_anon_1.obj.as_ref() else {
                    break;
                };
                for i in 0..so.nproto as usize {
                    let pr = &*so.proto.add(i);
                    if pr.hashed_name == hfield {
                        findex = Some(pr.findex as usize);
                        break 'chain;
                    }
                }
                scan_t = so.super_;
            }
            let Some(findex) = findex else {
                return ptr::null_mut();
            };
            let m = tobj.m;
            if m.is_null() || (*m).functions_types.is_null() {
                return ptr::null_mut();
            }
            let fun_type = *(*m).functions_types.add(findex);
            if fun_type.is_null() || (*fun_type).kind != hl::hl_type_kind_HFUN {
                return ptr::null_mut();
            }
            let addr = method_ptr as usize;
            if addr == 0 {
                return ptr::null_mut();
            }
            if addr < 0x100000 {
                // Interpreter stub sentinel: the bridge decodes the findex and
                // runs the interpreter with its own typing, boxed both ways.
                if let Some(runner) = crate::fiber::closure_runner() {
                    let mut cl = vclosure {
                        t: fun_type,
                        fun: method_ptr,
                        hasValue: 1,
                        stackCount: 0,
                        value: cur as *mut c_void,
                    };
                    let aptr = if nargs == 0 {
                        ptr::null_mut()
                    } else {
                        crate::types::hl_aptr::<*mut vdynamic>(args)
                    };
                    return runner(&mut cl, aptr, nargs);
                }
                return ptr::null_mut();
            }
            // hlp_call_method marshals per the closure's own type but
            // rejects value-carrying closures, and functions_types[findex]
            // includes `this` as arg0 anyway — so `this` rides in the args
            // array itself (an HOBJ pointer is its own dynamic) and the
            // closure carries no value.
            let call_args = hlp_alloc_dyn_array(nargs + 1);
            let dst = crate::types::hl_aptr::<*mut vdynamic>(call_args);
            *dst = cur;
            if nargs > 0 {
                let src = crate::types::hl_aptr::<*mut vdynamic>(args);
                for i in 0..nargs as usize {
                    *dst.add(i + 1) = *src.add(i);
                }
            }
            let mut cl = vclosure {
                t: fun_type,
                fun: method_ptr,
                hasValue: 0,
                stackCount: 0,
                value: ptr::null_mut(),
            };
            crate::fun::hlp_call_method(&mut cl as *mut vclosure as *mut vdynamic, call_args)
        }
        hl::hl_type_kind_HDYNOBJ => {
            // A closure-valued field carries its own type; hlp_dyn_call
            // handles both plain and value-carrying closures, which is what
            // HL's hl_dyn_call_obj does here.
            let cl = hlp_dyn_getp(cur, hfield, crate::types::hlt_dyn());
            if cl.is_null() {
                return ptr::null_mut();
            }
            let aptr = if nargs == 0 {
                ptr::null_mut()
            } else {
                crate::types::hl_aptr::<*mut vdynamic>(args)
            };
            crate::fun::hlp_dyn_call(cl as *mut vclosure, aptr, nargs)
        }
        _ => ptr::null_mut(),
    }
}

/// Virtual method dispatch fallback used by JIT code, keyed by field hash.
///
/// `target` is whatever the HVIRTUAL-typed register held at runtime. At the
/// hybrid interpreter/JIT boundary that is NOT guaranteed to be a vvirtual:
/// the interpreter is dynamically typed and passes plain HOBJ (or HDYNOBJ)
/// pointers through HVIRTUAL-typed slots. Dispatch on the runtime kind
/// instead of trusting the static type (trusting it was a deterministic
/// SIGBUS on game.hl: hl_type_obj's nfields/nproto ints read as a "fields"
/// pointer, fault_addr 0x2d00000058).
#[no_mangle]
pub unsafe extern "C" fn hlp_vcall_virtual_hashed(
    target: *mut vdynamic,
    hfield: i32,
) -> *mut vdynamic {
    if target.is_null() {
        return ptr::null_mut();
    }
    let t = (*target).t;
    if t.is_null() {
        return ptr::null_mut();
    }
    match (*t).kind {
        hl::hl_type_kind_HVIRTUAL => {
            let v = target as *mut vvirtual;
            let obj = (*v).value;
            if obj.is_null() {
                ptr::null_mut()
            } else {
                hlp_vcall_virtual_hashed(obj, hfield)
            }
        }
        hl::hl_type_kind_HOBJ => {
            let Some(tobj) = (*t).__bindgen_anon_1.obj.as_ref() else {
                return ptr::null_mut();
            };
            let f = obj_resolve_field(tobj, hfield);
            if f.is_null() || (*f).field_index >= 0 {
                return ptr::null_mut();
            }
            let mut rt = tobj.rt;
            if rt.is_null() || (*rt).methods.is_null() {
                rt = hl_get_obj_proto(t);
            }
            if rt.is_null() || (*rt).methods.is_null() {
                return ptr::null_mut();
            }
            let method_idx = (-(*f).field_index - 1) as usize;
            if method_idx >= (*rt).nmethods as usize {
                return ptr::null_mut();
            }
            let method_ptr: *mut c_void = *(*rt).methods.add(method_idx);
            vcall_fn_or_stub(method_ptr, target)
        }
        hl::hl_type_kind_HDYNOBJ => {
            // Dynamic object: the "method" is a closure-valued field.
            let cl = hlp_dyn_getp(target, hfield, crate::types::hlt_dyn()) as *mut vclosure;
            if cl.is_null() {
                return ptr::null_mut();
            }
            let this = if (*cl).hasValue != 0 {
                (*cl).value as *mut vdynamic
            } else {
                ptr::null_mut()
            };
            vcall_fn_or_stub((*cl).fun, this)
        }
        _ => ptr::null_mut(),
    }
}

/// Runtime helper for virtual method dispatch when vfields[field] is NULL.
/// Resolves the method from the underlying object's vtable, calls it (with only
/// the `this` arg — nargs=0 methods like hasNext/next), and returns the result
/// as a vdynamic* (boxed). The JIT unboxes as needed.
#[no_mangle]
pub unsafe extern "C" fn hlp_vcall_virtual_0(virt: *mut vvirtual, field: i32) -> *mut vdynamic {
    let obj = (*virt).value;
    let vt = (*virt).t;
    let hfield = (*vt)
        .__bindgen_anon_1
        .virt
        .as_ref()
        .unwrap()
        .fields
        .add(field as usize)
        .as_ref()
        .unwrap()
        .hashed_name;

    // Resolve the method from the underlying object
    let obj_type = (*obj).t;
    let rt = (*obj_type).__bindgen_anon_1.obj.as_ref().unwrap().rt;
    let f = obj_resolve_field((*obj_type).__bindgen_anon_1.obj.as_ref().unwrap(), hfield);
    if f.is_null() || (*f).field_index >= 0 {
        return ptr::null_mut();
    }

    let method_idx = (-(*f).field_index - 1) as usize;
    let method_ptr: *mut std::ffi::c_void = *(*rt).methods.wrapping_add(method_idx);

    // Call the method with this=obj, no other args
    // The method signature is fn(this: *obj) -> result
    let method_fn: unsafe extern "C" fn(*mut vdynamic) -> *mut vdynamic =
        std::mem::transmute(method_ptr);
    
    method_fn(obj)
}

unsafe fn should_recast(t: *mut hl_type, vt: *mut hl_type) -> bool {
    if (*vt).kind == hl::hl_type_kind_HF64 && (*t).kind == hl::hl_type_kind_HI32 {
        return true;
    }
    if (*vt).kind == hl::hl_type_kind_HNULL && (*(*vt).__bindgen_anon_1.tparam).kind == (*t).kind {
        return true;
    }
    if (*vt).kind == hl::hl_type_kind_HNULL
        && (*(*vt).__bindgen_anon_1.tparam).kind == hl::hl_type_kind_HF64
        && (*t).kind == hl::hl_type_kind_HI32
    {
        return true;
    }
    if (*vt).kind == hl::hl_type_kind_HVIRTUAL && (*t).kind == hl::hl_type_kind_HDYNOBJ {
        return true;
    }
    if (*vt).kind == hl::hl_type_kind_HOBJ
        && (*t).kind == hl::hl_type_kind_HOBJ
        && !(*(*vt).__bindgen_anon_1.obj).rt.is_null()
        && (*(*(*vt).__bindgen_anon_1.obj).rt).castFun.is_some()
    {
        return true;
    }
    false
}

#[no_mangle]
pub unsafe extern "C" fn hlp_dyn_setp(
    d: *mut vdynamic,
    hfield: i32,
    t: *mut hl_type,
    value: *mut c_void,
) {
    let mut ft: *mut hl_type = ptr::null_mut();
    // hl_track_call(HL_TRACK_DYNFIELD, on_dynfield(d, hfield));

    let addr = hlp_obj_lookup_set(d, hfield, t, &mut ft);

    if hlp_same_type(t, ft) || (hl_is_ptr(ft) && value.is_null()) {
        *(addr as *mut *mut c_void) = value;
    } else if hlp_is_dynamic(t) {
        hlp_write_dyn(addr, ft, value as *mut vdynamic, false);
    } else {
        let mut tmp = vdynamic {
            t,
            v: *std::mem::ManuallyDrop::new(vdynamic__bindgen_ty_1 { ptr: value }),
        };
        hlp_write_dyn(addr, ft, &mut tmp as *mut vdynamic, true);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_dyn_setd(d: *mut vdynamic, hfield: i32, value: f64) {
    let mut t: *mut hl_type = ptr::null_mut();
    // hl_track_call(HL_TRACK_DYNFIELD, on_dynfield(d, hfield));
    let f64_type = crate::types::hlt_f64();
    let addr = hlp_obj_lookup_set(d, hfield, f64_type, &mut t);

    if (*t).kind == hl_type_kind_HF64 {
        *(addr as *mut f64) = value;
    } else {
        let mut tmp = vdynamic {
            t: f64_type,
            v: *std::mem::ManuallyDrop::new(vdynamic__bindgen_ty_1 { d: value }),
        };
        hlp_write_dyn(addr, t, &mut tmp, true);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_dyn_setf(d: *mut vdynamic, hfield: i32, value: f32) {
    let mut t: *mut hl_type = ptr::null_mut();
    // hl_track_call(HL_TRACK_DYNFIELD, on_dynfield(d, hfield));
    let f32_type = crate::types::hlt_f32();
    let addr = hlp_obj_lookup_set(d, hfield, f32_type, &mut t);

    if (*t).kind == hl_type_kind_HF32 {
        *(addr as *mut f32) = value;
    } else {
        let mut tmp = vdynamic {
            t: f32_type,
            v: *std::mem::ManuallyDrop::new(vdynamic__bindgen_ty_1 { f: value }),
        };
        hlp_write_dyn(addr, t, &mut tmp, true);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_dyn_seti64(d: *mut vdynamic, hfield: i32, value: i64) {
    let mut ft: *mut hl_type = ptr::null_mut();
    // hl_track_call(HL_TRACK_DYNFIELD, on_dynfield(d, hfield));
    let i64_type = crate::types::hlt_i64();
    let addr = hlp_obj_lookup_set(d, hfield, i64_type, &mut ft);

    match (*ft).kind {
        hl_type_kind_HUI8 => *(addr as *mut u8) = value as u8,
        hl_type_kind_HUI16 => *(addr as *mut u16) = value as u16,
        hl_type_kind_HI32 => *(addr as *mut i32) = value as i32,
        hl_type_kind_HI64 => *(addr as *mut i64) = value,
        hl_type_kind_HBOOL => *(addr as *mut bool) = value != 0,
        hl_type_kind_HF32 => *(addr as *mut f32) = value as f32,
        hl_type_kind_HF64 => *(addr as *mut f64) = value as f64,
        _ => {
            let mut tmp = vdynamic {
                t: i64_type,
                v: *std::mem::ManuallyDrop::new(vdynamic__bindgen_ty_1 { i64_: value }),
            };
            hlp_write_dyn(addr, ft, &mut tmp, true);
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_dyn_seti(d: *mut vdynamic, hfield: i32, t: *mut hl_type, value: i32) {
    let mut ft: *mut hl_type = ptr::null_mut();
    // hl_track_call(HL_TRACK_DYNFIELD, on_dynfield(d, hfield));
    let addr = hlp_obj_lookup_set(d, hfield, t, &mut ft);
    if env_flag!("ASH_DYN_TRACE") {
        eprintln!(
            "[dyn-seti] d={:#x} dkind={} hfield={hfield} value={value} addr={:#x} ftkind={}",
            d as usize,
            if d.is_null() { 999 } else { (*(*d).t).kind },
            addr as usize,
            if ft.is_null() { 999 } else { (*ft).kind }
        );
    }

    match (*ft).kind {
        hl_type_kind_HUI8 => *(addr as *mut u8) = value as u8,
        hl_type_kind_HUI16 => *(addr as *mut u16) = value as u16,
        hl_type_kind_HI32 => *(addr as *mut i32) = value,
        hl_type_kind_HI64 => *(addr as *mut i64) = value as i64,
        hl_type_kind_HBOOL => *(addr as *mut bool) = value != 0,
        hl_type_kind_HF32 => *(addr as *mut f32) = value as f32,
        hl_type_kind_HF64 => *(addr as *mut f64) = value as f64,
        _ => {
            let mut tmp = vdynamic {
                t,
                v: *std::mem::ManuallyDrop::new(vdynamic__bindgen_ty_1 { i: value }),
            };
            hlp_write_dyn(addr, ft, &mut tmp, true);
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_dyn_getf(d: *mut vdynamic, hfield: i32) -> f32 {
    let mut ft: *mut hl_type = ptr::null_mut();
    // hl_track_call(HL_TRACK_DYNFIELD, on_dynfield(d, hfield));
    let dyn_type = crate::types::hlt_dyn();
    let addr = hlp_obj_lookup(d, hfield, &mut ft);

    if addr.is_null() {
        let d = hlp_obj_lookup_extra(d, hfield);
        if d.is_null() {
            return 0.0;
        } else {
            return hlp_dyn_castf(&d as *const _ as *mut c_void, dyn_type);
        }
    }

    if (*ft).kind == hl_type_kind_HF32 {
        *(addr as *mut f32)
    } else {
        hlp_dyn_castf(addr, ft)
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_dyn_getd(d: *mut vdynamic, hfield: i32) -> f64 {
    let mut ft: *mut hl_type = ptr::null_mut();
    // hl_track_call(HL_TRACK_DYNFIELD, on_dynfield(d, hfield));
    let dyn_type = crate::types::hlt_dyn();
    let addr = hlp_obj_lookup(d, hfield, &mut ft);

    if addr.is_null() {
        let d = hlp_obj_lookup_extra(d, hfield);
        if d.is_null() {
            return 0.0;
        } else {
            return hlp_dyn_castd(&d as *const _ as *mut c_void, dyn_type);
        }
    }

    if (*ft).kind == hl_type_kind_HF64 {
        *(addr as *mut f64)
    } else {
        hlp_dyn_castd(addr, ft)
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_dyn_geti(d: *mut vdynamic, hfield: i32, t: *mut hl_type) -> i32 {
    let dyn_type = crate::types::hlt_dyn();

    let mut ft: *mut hl_type = std::ptr::null_mut();
    // hl_track_call(HL_TRACK_DYNFIELD, on_dynfield(d, hfield));
    let addr = hlp_obj_lookup(d, hfield, &mut ft);
    if env_flag!("ASH_DYN_TRACE") {
        eprintln!(
            "[dyn-geti] d={:#x} dkind={} hfield={hfield} addr={:#x} ftkind={}",
            d as usize,
            if d.is_null() { 999 } else { (*(*d).t).kind },
            addr as usize,
            if ft.is_null() { 999 } else { (*ft).kind }
        );
    }
    if addr.is_null() {
        let d = hlp_obj_lookup_extra(d, hfield);
        return if d.is_null() {
            0
        } else {
            hlp_dyn_casti(&d as *const _ as *mut _, dyn_type, t)
        };
    }
    match (*ft).kind {
        hl_type_kind_HUI8 => *(addr as *const u8) as i32,
        hl_type_kind_HUI16 => *(addr as *const u16) as i32,
        hl_type_kind_HI32 => *(addr as *const i32),
        hl_type_kind_HI64 => *(addr as *const i64) as i32,
        hl_type_kind_HF32 => *(addr as *const f32) as i32,
        hl_type_kind_HF64 => *(addr as *const f64) as i32,
        hl_type_kind_HBOOL => *(addr as *const bool) as i32,
        _ => hlp_dyn_casti(addr, ft, t),
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_dyn_geti64(d: *mut vdynamic, hfield: i32) -> i64 {
    let dyn_type = crate::types::hlt_dyn();

    let mut ft: *mut hl_type = std::ptr::null_mut();
    // hl_track_call(HL_TRACK_DYNFIELD, on_dynfield(d, hfield));
    let addr = hlp_obj_lookup(d, hfield, &mut ft);
    if addr.is_null() {
        let d = hlp_obj_lookup_extra(d, hfield);
        return if d.is_null() {
            0
        } else {
            hlp_dyn_casti64(&d as *const _ as *mut _, dyn_type)
        };
    }
    match (*ft).kind {
        hl_type_kind_HUI8 => *(addr as *const u8) as i64,
        hl_type_kind_HUI16 => *(addr as *const u16) as i64,
        hl_type_kind_HI32 => *(addr as *const i32) as i64,
        hl_type_kind_HI64 => *(addr as *const i64),
        hl_type_kind_HF32 => *(addr as *const f32) as i64,
        hl_type_kind_HF64 => *(addr as *const f64) as i64,
        hl_type_kind_HBOOL => *(addr as *const bool) as i64,
        _ => hlp_dyn_casti64(addr, ft),
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_get_dynset(t: *mut hl_type) -> *mut c_void {
    unsafe {
        match (*t).kind {
            hl_type_kind_HF32 => hlp_dyn_setf as *mut c_void,
            hl_type_kind_HF64 => hlp_dyn_setd as *mut c_void,
            hl_type_kind_HI64 => hlp_dyn_seti64 as *mut c_void,
            hl_type_kind_HI32 | hl_type_kind_HUI16 | hl_type_kind_HUI8 | hl_type_kind_HBOOL => {
                hlp_dyn_seti as *mut c_void
            }
            _ => hlp_dyn_setp as *mut c_void,
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_get_dynget(t: *mut hl_type) -> *mut c_void {
    unsafe {
        match (*t).kind {
            hl_type_kind_HF32 => hlp_dyn_getf as *mut c_void,
            hl_type_kind_HF64 => hlp_dyn_getd as *mut c_void,
            hl_type_kind_HI64 => hlp_dyn_geti64 as *mut c_void,
            hl_type_kind_HI32 | hl_type_kind_HUI16 | hl_type_kind_HUI8 | hl_type_kind_HBOOL => {
                hlp_dyn_geti as *mut c_void
            }
            _ => hlp_dyn_getp as *mut c_void,
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_obj_get_field(obj: *mut vdynamic, hfield: i32) -> *mut vdynamic {
    if obj.is_null() {
        return ptr::null_mut();
    }
    if env_flag!("ASH_DYN_TRACE") {
        eprintln!(
            "[obj-get] obj={:#x} kind={} hfield={hfield}",
            obj as usize, (*(*obj).t).kind
        );
    }

    let dyn_type = crate::types::hlt_dyn();

    

    match (*(*obj).t).kind {
        hl_type_kind_HOBJ | hl_type_kind_HVIRTUAL | hl_type_kind_HDYNOBJ | hl_type_kind_HSTRUCT => {
            hlp_dyn_getp(obj, hfield, dyn_type) as *mut vdynamic
        }
        _ => ptr::null_mut(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_obj_set_field(obj: *mut vdynamic, hfield: i32, v: *mut vdynamic) {
    if obj.is_null() {
        hlp_error(str_to_uchar_ptr("Null access"));
        return;
    }
    if env_flag!("ASH_DYN_TRACE") {
        eprintln!(
            "[obj-set] obj={:#x} kind={} hfield={hfield} v={:#x}",
            obj as usize, (*(*obj).t).kind, v as usize
        );
    }

    let dyn_type = crate::types::hlt_dyn();

    if v.is_null() {
        hlp_dyn_setp(obj, hfield, dyn_type, ptr::null_mut());
        return;
    }

    // hl_track_call(HL_TRACK_DYNFIELD, on_dynfield(obj, hfield));

    let mut ft: *mut hl_type = ptr::null_mut();
    let addr = hlp_obj_lookup_set(obj, hfield, (*v).t, &mut ft);

    hlp_write_dyn(addr, ft, v, false);
}

#[no_mangle]
pub unsafe extern "C" fn hlp_obj_has_field(obj: *mut vdynamic, hfield: i32) -> bool {
    if obj.is_null() {
        return false;
    }

    match (*(*obj).t).kind {
        hl_type_kind_HOBJ | hl_type_kind_HSTRUCT => {
            let l = obj_resolve_field((*(*obj).t).__bindgen_anon_1.obj, hfield);
            !l.is_null() && (*l).field_index >= 0
        }
        hl_type_kind_HDYNOBJ => {
            let d = obj as *mut vdynobj;
            let f = hlp_lookup_find((*d).lookup, (*d).nfields, hfield);
            !f.is_null()
        }
        hl_type_kind_HVIRTUAL => {
            let v = obj as *mut vvirtual;
            if !(*v).value.is_null() {
                return hlp_obj_has_field((*v).value, hfield);
            }
            let f = hlp_lookup_find(
                (*(*(*v).t).__bindgen_anon_1.virt).lookup,
                (*(*(*v).t).__bindgen_anon_1.virt).nfields,
                hfield,
            );
            !f.is_null()
        }
        _ => false,
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_obj_delete_field(obj: *mut vdynamic, hfield: i32) -> bool {
    if obj.is_null() {
        return false;
    }

    match (*(*obj).t).kind {
        hl_type_kind_HDYNOBJ => {
            let d = obj as *mut vdynobj;
            let f = hlp_lookup_find((*d).lookup, (*d).nfields, hfield);
            if f.is_null() {
                return false;
            }
            hlp_dynobj_delete_field(d, f);
            true
        }
        hl_type_kind_HVIRTUAL => {
            let v = obj as *mut vvirtual;
            if !(*v).value.is_null() {
                return hlp_obj_delete_field((*v).value, hfield);
            }
            if hlp_lookup_find(
                (*(*(*v).t).__bindgen_anon_1.virt).lookup,
                (*(*(*v).t).__bindgen_anon_1.virt).nfields,
                hfield,
            )
            .is_null()
            {
                return false;
            }
            hlp_obj_delete_field(hlp_virtual_make_value(v), hfield)
        }
        _ => false,
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_hash(name: *mut vbyte) -> i32 {
    hlp_hash_gen(name as *const uchar, true)
}

#[no_mangle]
pub unsafe extern "C" fn hlp_obj_fields(obj: *mut vdynamic) -> *mut varray {
    use crate::{
        array::hlp_alloc_array,
        types::{hl_aptr, hlt_bytes},
    };

    if obj.is_null() {
        return ptr::null_mut();
    }
    match (*(*obj).t).kind {
        hl_type_kind_HDYNOBJ => {
            let o = obj as *mut vdynobj;
            let nf = (*o).nfields;
            let a = hlp_alloc_array(hlt_bytes(), nf);
            for i in 0..nf as usize {
                let f = (*o).lookup.add(i);
                let order = hlp_dynobj_order(f) as usize;
                *(hl_aptr::<*mut vbyte>(a)).add(order) = hlp_field_name((*f).hashed_name);
            }
            a
        }
        hl_type_kind_HOBJ | hl_type_kind_HSTRUCT => {
            let mut tobj = (*(*obj).t).__bindgen_anon_1.obj;
            if tobj.is_null() {
                return ptr::null_mut();
            }
            let rt = (*tobj).rt;
            if rt.is_null() {
                return ptr::null_mut();
            }
            let a = hlp_alloc_array(hlt_bytes(), (*rt).nfields);
            let mut p = 0usize;
            loop {
                for i in 0..(*tobj).nfields as usize {
                    let f = (*tobj).fields.add(i);
                    let name = (*f).name;
                    if name.is_null() || *name == 0 {
                        (*a).size -= 1;
                        continue;
                    }
                    *(hl_aptr::<*mut vbyte>(a)).add(p) = name as *mut vbyte;
                    p += 1;
                }
                let sup = (*tobj).super_;
                if sup.is_null() {
                    break;
                }
                tobj = (*sup).__bindgen_anon_1.obj;
            }
            a
        }
        hl_type_kind_HVIRTUAL => {
            let v = obj as *mut vvirtual;
            if !(*v).value.is_null() {
                return hlp_obj_fields((*v).value);
            }
            let virt = (*(*v).t).__bindgen_anon_1.virt;
            let a = hlp_alloc_array(hlt_bytes(), (*virt).nfields);
            for i in 0..(*virt).nfields as usize {
                *(hl_aptr::<*mut vbyte>(a)).add(i) = (*(*virt).fields.add(i)).name as *mut vbyte;
            }
            a
        }
        _ => ptr::null_mut(),
    }
}

/// Shallow copy of a dynamic object, `hl_obj_copy` in upstream's obj.c.
///
/// Only the two kinds that own their field storage answer: HDYNOBJ, whose
/// three arrays are duplicated, and HVIRTUAL, whose data block is. An HOBJ
/// has no copy semantics to give -- upstream returns NULL for it, and
/// `Reflect.copy` reads that as "not copyable".
// DEFINE_PRIM(_DYN, obj_copy, _DYN)
#[no_mangle]
pub unsafe extern "C" fn hlp_obj_copy(obj: *mut vdynamic) -> *mut vdynamic {
    if obj.is_null() || (*obj).t.is_null() {
        return ptr::null_mut();
    }
    match (*(*obj).t).kind {
        hl_type_kind_HDYNOBJ => {
            let o = obj as *mut vdynobj;
            let c = hlp_alloc_dynobj();
            if c.is_null() {
                return ptr::null_mut();
            }
            let nfields = (*o).nfields.max(0) as usize;
            let nvalues = (*o).nvalues.max(0) as usize;
            let raw_size = (*o).raw_size.max(0) as usize;

            (*c).raw_size = raw_size as c_int;
            (*c).nfields = nfields as c_int;
            (*c).nvalues = nvalues as c_int;
            // The virtual views bound to the original keep pointing at it;
            // the copy starts with none, as upstream's NULL does.
            (*c).virtuals = ptr::null_mut();

            let lsize = mem::size_of::<hl_field_lookup>() * nfields;
            (*c).lookup = crate::gc::gc_alloc(lsize)
                .expect("Failed to allocate dynobj lookup copy")
                .as_ptr() as *mut hl_field_lookup;
            (*c).raw_data = crate::gc::gc_alloc(raw_size)
                .expect("Failed to allocate dynobj raw_data copy")
                .as_ptr() as *mut std::os::raw::c_char;
            (*c).values = crate::gc::gc_alloc(nvalues * mem::size_of::<*mut c_void>())
                .expect("Failed to allocate dynobj values copy")
                .as_ptr() as *mut *mut c_void;

            if nfields > 0 && !(*o).lookup.is_null() {
                ptr::copy_nonoverlapping((*o).lookup, (*c).lookup, nfields);
            }
            if raw_size > 0 && !(*o).raw_data.is_null() {
                ptr::copy_nonoverlapping(
                    (*o).raw_data as *const u8,
                    (*c).raw_data as *mut u8,
                    raw_size,
                );
            }
            if nvalues > 0 && !(*o).values.is_null() {
                ptr::copy_nonoverlapping((*o).values, (*c).values, nvalues);
            }
            c as *mut vdynamic
        }
        hl_type_kind_HVIRTUAL => {
            let v = obj as *mut vvirtual;
            // A virtual backed by a dynobj is a view of it, so the copy is
            // of what it views, not of the view.
            if !(*v).value.is_null() {
                return hlp_obj_copy((*v).value);
            }
            let virt = (*(*v).t).__bindgen_anon_1.virt;
            if virt.is_null() {
                return ptr::null_mut();
            }
            let v2 = hlp_alloc_virtual((*v).t);
            if v2.is_null() {
                return ptr::null_mut();
            }
            // vfields[0..nfields] are addresses into this object; only the
            // data block past them is copyable, and hlp_alloc_virtual has
            // already pointed the new object's slots at its own copy.
            let nfields = (*virt).nfields.max(0) as usize;
            let data_size = (*virt).dataSize.max(0) as usize;
            if data_size > 0 {
                ptr::copy_nonoverlapping(
                    hl_vfields(v).add(nfields) as *const u8,
                    hl_vfields(v2).add(nfields) as *mut u8,
                    data_size,
                );
            }
            v2 as *mut vdynamic
        }
        _ => ptr::null_mut(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_type_instance_fields(t: *mut hl_type) -> *mut varray {
    use crate::{
        array::hlp_alloc_array,
        types::{hl_aptr, hlt_bytes},
    };

    if t.is_null() {
        return ptr::null_mut();
    }

    if (*t).kind == hl_type_kind_HVIRTUAL {
        let virt = (*t).__bindgen_anon_1.virt;
        let a = hlp_alloc_array(hlt_bytes(), (*virt).nfields);
        for i in 0..(*virt).nfields as usize {
            *(hl_aptr::<*const uchar>(a)).add(i) = (*(*virt).fields.add(i)).name;
        }
        return a;
    }

    if (*t).kind != hl_type_kind_HOBJ && (*t).kind != hl_type_kind_HSTRUCT {
        return ptr::null_mut();
    }

    let mut o = (*t).__bindgen_anon_1.obj;
    if o.is_null() {
        return ptr::null_mut();
    }

    // Count methods (proto entries with pindex < 0)
    let mut mcount = 0;
    let mut oc = o;
    loop {
        for i in 0..(*oc).nproto as usize {
            let p = (*oc).proto.add(i);
            if (*p).pindex < 0 {
                mcount += 1;
            }
        }
        let sup = (*oc).super_;
        if sup.is_null() {
            break;
        }
        oc = (*sup).__bindgen_anon_1.obj;
    }

    let rt = hlp_get_obj_rt(t);
    if rt.is_null() {
        return ptr::null_mut();
    }
    let total = mcount + (*rt).nproto + (*rt).nfields;
    let a = hlp_alloc_array(hlt_bytes(), total);
    let mut out = 0usize;
    let mut current_rt = rt;

    o = (*t).__bindgen_anon_1.obj;
    loop {
        let pproto = if !(*current_rt).parent.is_null() {
            (*(*current_rt).parent).nproto
        } else {
            0
        };
        for i in 0..(*o).nproto as usize {
            let p = (*o).proto.add(i);
            if (*p).pindex < 0 || (*p).pindex >= pproto {
                *(hl_aptr::<*const uchar>(a)).add(out) = (*p).name;
                out += 1;
            }
        }
        for i in 0..(*o).nfields as usize {
            let f = (*o).fields.add(i);
            *(hl_aptr::<*const uchar>(a)).add(out) = (*f).name;
            out += 1;
        }
        let sup = (*o).super_;
        if sup.is_null() {
            break;
        }
        o = (*sup).__bindgen_anon_1.obj;
        current_rt = (*o).rt;
    }

    a
}

#[no_mangle]
/// Flush a type's cached vtable/proto so it gets re-populated from `functions_ptrs`
/// on the next method dispatch. Used during hot-reload after function pointers are updated.
pub unsafe extern "C" fn hlp_flush_proto(ot: *mut hl_type) {
    if ot.is_null() {
        return;
    }
    // Reset the cached method dispatch table.
    // On next CallMethod, hlp_get_obj_proto / hlp_get_obj_rt will re-read
    // from functions_ptrs and rebuild the proto array.
    (*ot).vobj_proto = ptr::null_mut();
    let kind = (*ot).kind;
    if kind == hl_type_kind_HOBJ || kind == hl_type_kind_HSTRUCT {
        let obj = (*ot).__bindgen_anon_1.obj;
        if !obj.is_null() && !(*obj).rt.is_null() {
            // Clear the runtime binding's cached method pointers
            (*(*obj).rt).methods = ptr::null_mut();
        }
    }
}

#[no_mangle]
pub extern "C" fn hlp_init_virtual(vt: *mut hl_type, _ctx: *mut hl_module_context) {
    unsafe {
        let virt = (*vt).__bindgen_anon_1.virt.as_mut().unwrap();

        let vsize = mem::size_of::<vvirtual>()
            + mem::size_of::<*mut std::os::raw::c_void>() * virt.nfields as usize;
        let mut size = vsize;

        let mut allocator = crate::gc::gc_locked_init();

        // Immortal: stored only into the (never-scanned) type's virt data.
        let l = allocator
            .allocate_immortal(mem::size_of::<hl_field_lookup>() * virt.nfields as usize)
            .unwrap()
            .cast::<hl_field_lookup>()
            .as_ptr();

        let indexes = allocator
            .allocate_immortal(mem::size_of::<i32>() * virt.nfields as usize)
            .unwrap()
            .cast::<i32>()
            .as_ptr();

        for i in 0..virt.nfields as usize {
            let f = &*virt.fields.add(i);
            hlp_lookup_insert(l, i as i32, f.hashed_name, f.t, i as i32);
            size += hlp_pad_struct(size as i32, f.t) as usize;
            *indexes.add(i) = size as i32;
            size += hlp_type_size(f.t) as usize;
        }

        virt.lookup = l;
        virt.indexes = indexes;
        virt.dataSize = (size - vsize) as i32;

        let mark_size = hlp_mark_size(size as i32);
        let mark = allocator
            .allocate_immortal(mark_size as usize)
            .unwrap()
            .cast::<u32>()
            .as_ptr();
        ptr::write_bytes(mark, 0, mark_size as usize / mem::size_of::<u32>());

        (*vt).mark_bits = mark;
        *mark = 2 | 4; // value | next

        for i in 0..virt.nfields as usize {
            let f = &*virt.fields.add(i);
            if hl_is_ptr(f.t) {
                let pos = *indexes.add(i) as u32 / HL_WSIZE;
                *mark.add((pos >> 5) as usize) |= 1 << (pos & 31);
            }
        }
    }
}
