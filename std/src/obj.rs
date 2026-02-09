use core::slice;
use std::alloc::alloc;
use std::rc::Rc;
use std::sync::{MutexGuard, RwLock};
use std::{
    alloc::Layout,
    cmp::Ordering,
    ffi::{c_int, c_void, CStr},
    mem, ptr,
    sync::{LazyLock, Mutex, OnceLock},
};

use crate::gc::ImmixAllocator;
use crate::strings::{hlp_utf16_length, hlp_utf16_to_utf8};
use crate::{
    buffer::hlp_type_str,
    cast::{self, *},
    error::hlp_error,
    gc::{hlp_mark_size, hlp_zalloc, GC, HL_GLOBAL_LOCK},
    hl::{self, *},
    strings::str_to_uchar_ptr,
    types::{
        hl_is_ptr, hlp_is_dynamic, hlp_pad_struct, hlp_safe_cast, hlp_same_type, hlp_type_size,
    },
    ucs2::{ucmp, ustrdup},
};

/// Pre-computed UTF-16 null-terminated string constants for hash lookups.
/// These must be properly aligned u16 arrays, not byte strings cast to u16.
static USTR_COMPARE: &[u16] = &[b'_' as u16, b'_' as u16, b'c' as u16, b'o' as u16, b'm' as u16, b'p' as u16, b'a' as u16, b'r' as u16, b'e' as u16, 0];
static USTR_STRING: &[u16] = &[b'_' as u16, b'_' as u16, b's' as u16, b't' as u16, b'r' as u16, b'i' as u16, b'n' as u16, b'g' as u16, 0];
static USTR_CAST: &[u16] = &[b'_' as u16, b'_' as u16, b'c' as u16, b'a' as u16, b's' as u16, b't' as u16, 0];
static USTR_GET_FIELD: &[u16] = &[b'_' as u16, b'_' as u16, b'g' as u16, b'e' as u16, b't' as u16, b'_' as u16, b'f' as u16, b'i' as u16, b'e' as u16, b'l' as u16, b'd' as u16, 0];

static HL_CACHE_LOCK: OnceLock<Mutex<()>> = OnceLock::new();
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
static mut HL_CACHE_COUNT: i32 = 0;
static mut HL_CACHE_SIZE: i32 = 0;
static INITIAL_CACHE_CAPACITY: usize = 16;

pub static cache_lock: Mutex<i32> = Mutex::new(0);

#[no_mangle]
pub unsafe extern "C" fn hlp_alloc_virtual(t: *mut hl::hl_type) -> *mut hl::vvirtual {
    let allocator = GC.get_mut().expect("Expected to get GC");
    if let Some(virt) = allocator.alloc_virtual(t) {
        return virt.as_ptr()
    }
    return std::ptr::null_mut();
}

#[no_mangle]
pub unsafe extern "C" fn hlp_alloc_obj(t: *mut hl::hl_type) -> *mut hl::vdynamic {
    let allocator = GC.get_mut().expect("Expected to get GC");

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

    // Allocate memory
    let ptr = allocator.allocate(size).expect("Out of memory");

    // Zero-initialize memory first, THEN set type header
    let o = ptr.as_ptr() as *mut hl::vobj;
    std::ptr::write_bytes(o as *mut u8, 0, size);
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
    let gc = GC.get_mut().expect("Expected to get GC");

    let d = gc
        .allocate(std::mem::size_of::<vdynamic>())
        .expect("Out of memory")
        .as_ptr() as *mut vdynamic;

    // Zero-initialize the memory
    ptr::write_bytes(d as *mut u8, 0, std::mem::size_of::<vdynamic>());

    (*d).t = t;

    d
}

pub unsafe extern "C" fn hlp_alloc_dynbool(b: bool) -> *mut vdynamic {
    let _hlt_bool: *mut hl_type = &mut hl_type {
        kind: hl_type_kind_HBOOL,
        __bindgen_anon_1: hl_type__bindgen_ty_1 {
            obj: ptr::null_mut(),
        },
        vobj_proto: ptr::null_mut(),
        mark_bits: ptr::null_mut(),
    };
    let v = hlp_alloc_dynamic(_hlt_bool);
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
    // hl_track_call(HL_TRACK_CAST, on_cast(if !v.is_null() { (*v).t } else { _hlt_dyn }, t));

    let _hlt_dyn: *mut hl_type = &mut hl_type {
        kind: hl_type_kind_HDYN,
        __bindgen_anon_1: hl_type__bindgen_ty_1 {
            obj: ptr::null_mut(),
        },
        vobj_proto: ptr::null_mut(),
        mark_bits: ptr::null_mut(),
    };

    match std::mem::transmute::<u32, hl_type_kind>((*t).kind) {
        hl_type_kind_HUI8 => {
            *(data as *mut u8) = hlp_dyn_casti(&v as *const _ as *mut c_void, _hlt_dyn, t) as u8;
        }
        hl_type_kind_HBOOL => {
            *(data as *mut bool) = hlp_dyn_casti(&v as *const _ as *mut c_void, _hlt_dyn, t) != 0;
        }
        hl_type_kind_HUI16 => {
            *(data as *mut u16) = hlp_dyn_casti(&v as *const _ as *mut c_void, _hlt_dyn, t) as u16;
        }
        hl_type_kind_HI32 => {
            *(data as *mut i32) = hlp_dyn_casti(&v as *const _ as *mut c_void, _hlt_dyn, t);
        }
        hl_type_kind_HI64 => {
            *(data as *mut i64) = hlp_dyn_casti64(&v as *const _ as *mut c_void, _hlt_dyn);
        }
        hl_type_kind_HF32 => {
            *(data as *mut f32) = hlp_dyn_castf(&v as *const _ as *mut c_void, _hlt_dyn);
        }
        hl_type_kind_HF64 => {
            *(data as *mut f64) = hlp_dyn_castd(&v as *const _ as *mut c_void, _hlt_dyn);
        }
        _ => {
            let mut ret = if !v.is_null() && hlp_same_type(t, (*v).t) {
                v as *mut c_void
            } else {
                hlp_dyn_castp(&v as *const _ as *mut c_void, _hlt_dyn, t)
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
    match std::mem::transmute::<u32, hl_type_kind>((*(*d).t).kind) {
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
                (*(*(*d).t).__bindgen_anon_1.virt)
                    .indexes
                    .wrapping_add((*f).field_index as usize) as isize,
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
    if (name as usize) % 2 != 0 {
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
        // println!("Attempting to cache the name");

        // First, try to read from the cache
        if let Ok(cache) = HL_CACHE.read() {
            // print_cache_state(&cache, "Read lock acquired");

            if !cache.data.is_null() {
                let mut low = 0;
                let mut high = cache.size;

                while low < high {
                    let mid = low + (high - low) / 2;
                    // println!("Binary search - low: {}, high: {}, mid: {}", low, high, mid);

                    if mid >= cache.capacity {
                        // println!(
                        //     "Error: mid ({}) is out of bounds. Cache capacity: {}",
                        //     mid, cache.capacity
                        // );
                        return h;
                    }

                    let lookup = &*cache.data.add(mid);
                    // println!("Comparing hash {} with {}", lookup.hashed_name, h);

                    let cmp = lookup.hashed_name.cmp(&h).then_with(|| {
                        let cmp_result = ucmp(lookup.t as *const uchar, oname);
                        // println!("String comparison result: {}", cmp_result);
                        if cmp_result < 0 {
                            Ordering::Less
                        } else if cmp_result > 0 {
                            Ordering::Greater
                        } else {
                            Ordering::Equal
                        }
                    });

                    match cmp {
                        Ordering::Equal => {
                            // println!("Found exact match, returning hash");
                            return h;
                        }
                        Ordering::Less => low = mid + 1,
                        Ordering::Greater => high = mid,
                    }
                }
            }
        } else {
            // println!("Failed to acquire read lock");
        }

        // If we're here, we need to write to the cache
        if let Ok(mut cache) = HL_CACHE.write() {
            // print_cache_state(&cache, "Write lock acquired");

            if cache.data.is_null() || cache.size >= cache.capacity {
                if !grow_cache(&mut cache) {
                    // println!("Failed to grow cache, returning computed hash");
                    return h;
                }
            }

            let new_name = ustrdup(oname);
            if !new_name.is_null() {
                let insert_pos = cache.size;
                // println!("Inserting new entry at position {}", insert_pos);
                ptr::write(
                    cache.data.add(insert_pos),
                    hl_field_lookup {
                        field_index: 0,
                        hashed_name: h,
                        t: new_name as *mut hl_type,
                    },
                );
                cache.size += 1;
                // print_cache_state(&cache, "After insertion");
            } else {
                // println!("Failed to duplicate string, not inserting");
            }
        } else {
            // println!("Failed to acquire write lock");
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
    if let Ok(cache) = HL_CACHE.read() {
        let l = hlp_lookup_find(cache.data, HL_CACHE_COUNT, hash);
        if !l.is_null() {
            return (*l).t as *mut vbyte;
        }
    }
    return str_to_uchar_ptr("???") as *mut vbyte;
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

    let allocator = GC.get_mut().expect("expected to get garbage collector");

    if (*t).nproto != 0 {
        let fptr = allocator
            .allocate(std::mem::size_of::<*mut std::os::raw::c_void>() * (*t).nproto as usize)
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
                    *fptr.add((*p).pindex as usize) = *(*m).functions_ptrs.add((*p).findex as usize);
                }
            }
        }
    } else {
        (*ot).vobj_proto = 1 as *mut *mut c_void;
    }

    (*t).methods = allocator
        .allocate(std::mem::size_of::<*mut std::os::raw::c_void>() * (*t).nmethods as usize)
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
            *(*t).methods.add(method_index as usize) = *(*m).functions_ptrs.add((*pr).findex as usize);
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
        .allocate(std::mem::size_of::<i32>() * (*t).ninterfaces as usize)
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

            let func_type_ptr = *(*m).functions_types.add(mid as usize);
            let func_ptr = *(*m).functions_ptrs.add(mid as usize);

            match (*ft).kind {
                hl::hl_type_kind_HFUN
                    if (*(*ft).__bindgen_anon_1.fun).nargs
                        == (*(*(*(*m).functions_types.add(mid as usize)))
                            .__bindgen_anon_1
                            .fun)
                            .nargs =>
                {
                    let c = allocator
                        .allocate(std::mem::size_of::<vclosure>())
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
    let cmp_field = obj_resolve_field(
        o,
        hlp_hash_gen(USTR_COMPARE.as_ptr(), false),
    );
    let cast_field = obj_resolve_field(
        o,
        hlp_hash_gen(USTR_CAST.as_ptr(), false),
    );
    let get_field = obj_resolve_field(
        o,
        hlp_hash_gen(USTR_GET_FIELD.as_ptr(), false),
    );
    (*t).toStringFun = if !str_field.is_null() {
        Some(mem::transmute::<
            *const c_void,
            unsafe extern "C" fn(*mut vdynamic) -> *const u16,
        >(*(*t).methods.offset(
            (-((*str_field).field_index + 1)).try_into().unwrap(),
        )))
    } else {
        None
    };
    (*t).compareFun = if !cmp_field.is_null() && !(*t).compareFun.is_none() {
        Some(mem::transmute(*(*t).methods.offset(
            (-((*cmp_field).field_index + 1)).try_into().unwrap(),
        )))
    } else {
        None
    };
    (*t).castFun = if !cast_field.is_null() {
        Some(mem::transmute(*(*t).methods.offset(
            (-((*cast_field).field_index + 1)).try_into().unwrap(),
        )))
    } else {
        None
    };
    (*t).getFieldFun = if !get_field.is_null() {
        Some(mem::transmute(*(*t).methods.offset(
            (-((*get_field).field_index + 1)).try_into().unwrap(),
        )))
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

    let gc = GC.get_mut_or_init(|| ImmixAllocator::new());
    let t = gc
        .allocate(std::mem::size_of::<hl_runtime_obj>())
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
        .allocate(std::mem::size_of::<hl_field_lookup>() * (*t).nlookup as usize)
        .expect("Failed to allocate memory")
        .as_ptr() as *mut hl_field_lookup;
    (*t).fields_indexes = gc
        .allocate(std::mem::size_of::<i32>() * (*t).nfields as usize)
        .expect("Failed to allocate memory")
        .as_ptr() as *mut i32;
    (*t).bindings = gc
        .allocate(std::mem::size_of::<hl_runtime_binding>() * (*t).nbindings as usize)
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
        if !m.is_null() && !(*m).functions_types.is_null()
            && (*pr).hashed_name == compare_hash
            && (*(*mt).__bindgen_anon_1.fun).nargs == 2
            && (*(*(*(*mt).__bindgen_anon_1.fun).args.add(1))).kind == hl::hl_type_kind_HDYN
            && (*(*(*mt).__bindgen_anon_1.fun).ret).kind == hl::hl_type_kind_HI32
        {
            //TODO: Need to make sure findex is really a pointer to a function or figure out how to look up the function table
            (*t).compareFun = Some(mem::transmute((*pr).findex as *mut std::os::raw::c_void));
        }
    }

    // Mark bits
    if (*t).hasPtr {
        let mark_size = hlp_mark_size((*t).size) as usize;
        let mark = hlp_zalloc(mark_size.try_into().unwrap()) as *mut u32;
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
                let allocator = GC.get_mut().expect("Failed to get GC");
                let closure = allocator.allocate_closure_ptr(
                    (*f).t,
                    *(*(*obj).rt).methods.offset(-(*f).field_index as isize - 1),
                    d as *mut std::ffi::c_void,
                );
                return closure as *mut vdynamic;
            }
            if f.is_null() {
                let rt = (*obj).rt;
                if !(*rt).getFieldFun.is_none() {
                    return (*rt).getFieldFun.unwrap()(d, hfield);
                }
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

    let allocator = GC.get_mut().expect("Failed to get GC");

    // expand data
    if hl_is_ptr(t) {
        index = (*o).nvalues;
        if index > HL_DYNOBJ_INDEX_MASK as i32 {
            hlp_error(str_to_uchar_ptr("Too many dynobj values\0"));
        }
        let nvalues = allocator
            .allocate(((*o).nvalues as usize + 1) * mem::size_of::<*mut c_void>())
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
            raw_size += hlp_pad_size(raw_size as i32, (*f).t);
            raw_size += hlp_type_size((*f).t) as i32;
        }
        if raw_size > (*o).raw_size {
            raw_size = (*o).raw_size;
        }
        let pad = hlp_pad_size(raw_size as i32, t) as usize;
        let size = hlp_type_size(t) as usize;

        if raw_size as usize + pad > HL_DYNOBJ_INDEX_MASK as usize {
            hlp_error(str_to_uchar_ptr("Too many dynobj values\0"));
        }

        let new_data = allocator
            .allocate(raw_size as usize + pad + size)
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
                raw_size += hlp_pad_size(raw_size as i32, (*f).t);
                ptr::copy_nonoverlapping(
                    (*o).raw_data.add(index as usize),
                    new_data.add(raw_size as usize),
                    hlp_type_size((*f).t) as usize,
                );
                (*f).field_index =
                    (raw_size as i32) | ((hlp_dynobj_order(f) << HL_DYNOBJ_INDEX_SHIFT) as i32);
                if index != raw_size {
                    hlp_dynobj_remap_virtuals(o, f, 0);
                }
                raw_size += hlp_type_size((*f).t) as i32;
            }
            (*o).raw_size = raw_size as i32;
        }
        address_offset = new_data.offset_from((*o).raw_data);
        (*o).raw_data = new_data;
        (*o).raw_size += pad as i32;
        index = (*o).raw_size;
        (*o).raw_size += size as i32;
    }

    // update field table
    let new_lookup = allocator
        .allocate(mem::size_of::<hl_field_lookup>() * ((*o).nfields as usize + 1))
        .expect("Failed to allocate memory")
        .as_ptr() as *mut hl_field_lookup;
    let field_pos = hlp_lookup_find_index((*o).lookup, (*o).nfields, hfield);
    ptr::copy_nonoverlapping((*o).lookup, new_lookup, field_pos as usize);
    let f = new_lookup.add(field_pos as usize);
    (*f).t = t;
    (*f).hashed_name = hfield;
    (*f).field_index = index | (((*o).nfields << HL_DYNOBJ_INDEX_SHIFT) as i32);
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
            ((*o).nvalues as usize - (index as usize + 1)) * mem::size_of::<*mut c_void>(),
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
        ((*o).nfields as usize - (field + 1)) * mem::size_of::<hl_field_lookup>(),
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
    let allocator = GC.get_mut().expect("Failed to get GC");

    // Allocate memory for the vdynobj structure
    let obj = allocator
        .allocate(mem::size_of::<vdynobj>())
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
    (*obj).lookup = allocator
        .allocate(mem::size_of::<hl_field_lookup>() * initial_lookup_size)
        .expect("Failed to allocate memory for lookup table")
        .as_ptr() as *mut hl_field_lookup;

    // Allocate initial memory for values (we'll start with a small size)
    let initial_values_size = 4; // Starting with space for 4 values
    (*obj).values = allocator
        .allocate(mem::size_of::<*mut std::ffi::c_void>() * initial_values_size)
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
    let allocator = GC.get_mut().expect("Failed to get GC");
    (*o).lookup = allocator
        .allocate(mem::size_of::<hl_field_lookup>() * nfields as usize)
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
    (*o).raw_data = allocator
        .allocate(raw_size as usize)
        .expect("Failed to allocate memory")
        .as_ptr() as *mut i8;
    (*o).raw_size = raw_size;
    (*o).values = allocator
        .allocate(nvalues as usize * mem::size_of::<*mut std::ffi::c_void>())
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

    let _hlt_dyn: *mut hl_type = &mut hl_type {
        kind: hl_type_kind_HDYN,
        __bindgen_anon_1: hl_type__bindgen_ty_1 {
            obj: ptr::null_mut(),
        },
        vobj_proto: ptr::null_mut(),
        mark_bits: ptr::null_mut(),
    };

    let addr = hlp_obj_lookup(d, hfield, &mut ft);

    if addr.is_null() {
        let d = hlp_obj_lookup_extra(d, hfield);
        if d.is_null() {
            return ptr::null_mut();
        } else {
            return hlp_dyn_castp(&d as *const _ as *mut std::ffi::c_void, _hlt_dyn, t);
        }
    }

    if hlp_same_type(t, ft) {
        *(addr as *mut *mut std::ffi::c_void)
    } else {
        hlp_dyn_castp(addr, ft, t)
    }
}

pub unsafe fn hl_to_virtual(vt: *mut hl_type, obj: *mut vdynamic) -> *mut vvirtual {
    if obj.is_null() {
        return ptr::null_mut();
    }

    #[cfg(debug_assertions)]
    {
        if (*vt).__bindgen_anon_1.virt.as_ref().unwrap().nfields != 0
            && (*vt)
                .__bindgen_anon_1
                .virt
                .as_ref()
                .unwrap()
                .lookup
                .is_null()
        {
            panic!("virtual not initialized");
        }
    }

    let gc = GC.get_mut().expect("Expected to get GC");

    match (*obj).t.as_ref().unwrap().kind {
        hl::hl_type_kind_HOBJ => {
            let mut v: *mut vvirtual = ptr::null_mut();
            let mut interface_address: *mut *mut vvirtual = ptr::null_mut();

            let mut rt = (*(*obj).t).__bindgen_anon_1.obj.as_ref().unwrap().rt;
            while !rt.is_null() {
                for i in 0..(*rt).ninterfaces as usize {
                    if (*(*rt).t)
                        .__bindgen_anon_1
                        .obj
                        .as_ref()
                        .unwrap()
                        .fields
                        .add((*rt).interfaces.add(i as usize) as usize)
                        .as_ref()
                        .unwrap()
                        .t
                        == vt
                    {
                        let start = if (*rt).parent.is_null() {
                            0
                        } else {
                            (*(*rt).parent).nfields as usize
                        };
                        interface_address = (obj as *mut u8).add(
                            (*rt)
                                .fields_indexes
                                .wrapping_add((*rt).interfaces.add(i as usize) as usize + start)
                                as usize,
                        ) as *mut *mut vvirtual;
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

            v = gc
                .allocate(
                    mem::size_of::<vvirtual>()
                        + mem::size_of::<*mut std::ffi::c_void>()
                            * (*vt).__bindgen_anon_1.virt.as_ref().unwrap().nfields as usize,
                )
                .expect("Memory allocation failed")
                .as_ptr() as *mut vvirtual;
            (*v).t = vt;
            (*v).value = obj as *mut _;
            (*v).next = ptr::null_mut();

            for i in 0..(*vt).__bindgen_anon_1.virt.as_ref().unwrap().nfields as usize {
                let f = obj_resolve_field(
                    (*(*obj).t).__bindgen_anon_1.obj.as_ref().unwrap(),
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
                    if hlp_safe_cast(&mut tmp, ft) {
                        *(hl_vfields(v).add(i)) =
                            *(*(*(*obj).t).__bindgen_anon_1.obj.as_ref().unwrap().rt)
                                .methods
                                .wrapping_add((-(*f).field_index - 1) as usize);
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
            v = gc
                .allocate(
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
    (*((*v).v.ptr as *mut vvirtual)).value
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
        && !(*(*(*vt).__bindgen_anon_1.obj).rt).castFun.is_none()
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
            t: t,
            v: *std::mem::ManuallyDrop::new(vdynamic__bindgen_ty_1 { ptr: value }),
        };
        hlp_write_dyn(addr, ft, &mut tmp as *mut vdynamic, true);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_dyn_setd(d: *mut vdynamic, hfield: i32, value: f64) {
    let mut t: *mut hl_type = ptr::null_mut();
    // hl_track_call(HL_TRACK_DYNFIELD, on_dynfield(d, hfield));
    let mut HLT_F64: hl_type = hl_type {
        kind: hl_type_kind_HF64 as u32,
        __bindgen_anon_1: hl_type__bindgen_ty_1 {
            obj: ptr::null_mut(),
        },
        vobj_proto: ptr::null_mut(),
        mark_bits: ptr::null_mut(),
    };
    let addr = hlp_obj_lookup_set(d, hfield, &mut HLT_F64, &mut t);

    if (*t).kind == hl_type_kind_HF64 as u32 {
        *(addr as *mut f64) = value;
    } else {
        let mut tmp = vdynamic {
            t: &mut HLT_F64,
            v: *std::mem::ManuallyDrop::new(vdynamic__bindgen_ty_1 { d: value }),
        };
        hlp_write_dyn(addr, t, &mut tmp, true);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_dyn_setf(d: *mut vdynamic, hfield: i32, value: f32) {
    let mut t: *mut hl_type = ptr::null_mut();
    // hl_track_call(HL_TRACK_DYNFIELD, on_dynfield(d, hfield));
    let mut HLT_F32: hl_type = hl_type {
        kind: hl_type_kind_HF32 as u32,
        __bindgen_anon_1: hl_type__bindgen_ty_1 {
            obj: ptr::null_mut(),
        },
        vobj_proto: ptr::null_mut(),
        mark_bits: ptr::null_mut(),
    };
    let addr = hlp_obj_lookup_set(d, hfield, &mut HLT_F32, &mut t);

    if (*t).kind == hl_type_kind_HF32 as u32 {
        *(addr as *mut f32) = value;
    } else {
        let mut tmp = vdynamic {
            t: &mut HLT_F32,
            v: *std::mem::ManuallyDrop::new(vdynamic__bindgen_ty_1 { f: value }),
        };
        hlp_write_dyn(addr, t, &mut tmp, true);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_dyn_seti64(d: *mut vdynamic, hfield: i32, value: i64) {
    let mut ft: *mut hl_type = ptr::null_mut();
    // hl_track_call(HL_TRACK_DYNFIELD, on_dynfield(d, hfield));
    let mut HLT_I64: hl_type = hl_type {
        kind: hl_type_kind_HI64 as u32,
        __bindgen_anon_1: hl_type__bindgen_ty_1 {
            obj: ptr::null_mut(),
        },
        vobj_proto: ptr::null_mut(),
        mark_bits: ptr::null_mut(),
    };
    let addr = hlp_obj_lookup_set(d, hfield, &mut HLT_I64, &mut ft);

    match std::mem::transmute::<u32, hl_type_kind>((*ft).kind) {
        hl_type_kind_HUI8 => *(addr as *mut u8) = value as u8,
        hl_type_kind_HUI16 => *(addr as *mut u16) = value as u16,
        hl_type_kind_HI32 => *(addr as *mut i32) = value as i32,
        hl_type_kind_HI64 => *(addr as *mut i64) = value,
        hl_type_kind_HBOOL => *(addr as *mut bool) = value != 0,
        hl_type_kind_HF32 => *(addr as *mut f32) = value as f32,
        hl_type_kind_HF64 => *(addr as *mut f64) = value as f64,
        _ => {
            let mut tmp = vdynamic {
                t: &mut HLT_I64,
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

    match std::mem::transmute::<u32, hl_type_kind>((*ft).kind) {
        hl_type_kind_HUI8 => *(addr as *mut u8) = value as u8,
        hl_type_kind_HUI16 => *(addr as *mut u16) = value as u16,
        hl_type_kind_HI32 => *(addr as *mut i32) = value,
        hl_type_kind_HI64 => *(addr as *mut i64) = value as i64,
        hl_type_kind_HBOOL => *(addr as *mut bool) = value != 0,
        hl_type_kind_HF32 => *(addr as *mut f32) = value as f32,
        hl_type_kind_HF64 => *(addr as *mut f64) = value as f64,
        _ => {
            let mut tmp = vdynamic {
                t: t,
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
    let _hlt_dyn: *mut hl_type = &mut hl_type {
        kind: hl_type_kind_HDYN,
        __bindgen_anon_1: hl_type__bindgen_ty_1 {
            obj: ptr::null_mut(),
        },
        vobj_proto: ptr::null_mut(),
        mark_bits: ptr::null_mut(),
    };
    let addr = hlp_obj_lookup(d, hfield, &mut ft);

    if addr.is_null() {
        let d = hlp_obj_lookup_extra(d, hfield);
        if d.is_null() {
            return 0.0;
        } else {
            return hlp_dyn_castf(&d as *const _ as *mut c_void, _hlt_dyn);
        }
    }

    if (*ft).kind == hl_type_kind_HF32 as u32 {
        *(addr as *mut f32)
    } else {
        hlp_dyn_castf(addr, ft)
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_dyn_getd(d: *mut vdynamic, hfield: i32) -> f64 {
    let mut ft: *mut hl_type = ptr::null_mut();
    // hl_track_call(HL_TRACK_DYNFIELD, on_dynfield(d, hfield));
    let _hlt_dyn: *mut hl_type = &mut hl_type {
        kind: hl_type_kind_HDYN,
        __bindgen_anon_1: hl_type__bindgen_ty_1 {
            obj: ptr::null_mut(),
        },
        vobj_proto: ptr::null_mut(),
        mark_bits: ptr::null_mut(),
    };
    let addr = hlp_obj_lookup(d, hfield, &mut ft);

    if addr.is_null() {
        let d = hlp_obj_lookup_extra(d, hfield);
        if d.is_null() {
            return 0.0;
        } else {
            return hlp_dyn_castd(&d as *const _ as *mut c_void, _hlt_dyn);
        }
    }

    if (*ft).kind == hl_type_kind_HF64 as u32 {
        *(addr as *mut f64)
    } else {
        hlp_dyn_castd(addr, ft)
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_dyn_geti(d: *mut vdynamic, hfield: i32, t: *mut hl_type) -> i32 {
    let _hlt_dyn: *mut hl_type = &mut hl_type {
        kind: hl_type_kind_HDYN,
        __bindgen_anon_1: hl_type__bindgen_ty_1 {
            obj: ptr::null_mut(),
        },
        vobj_proto: ptr::null_mut(),
        mark_bits: ptr::null_mut(),
    };

    let mut ft: *mut hl_type = std::ptr::null_mut();
    // hl_track_call(HL_TRACK_DYNFIELD, on_dynfield(d, hfield));
    let addr = hlp_obj_lookup(d, hfield, &mut ft);
    if addr.is_null() {
        let d = hlp_obj_lookup_extra(d, hfield);
        return if d.is_null() {
            0
        } else {
            hlp_dyn_casti(&d as *const _ as *mut _, _hlt_dyn, t)
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
    let _hlt_dyn: *mut hl_type = &mut hl_type {
        kind: hl_type_kind_HDYN,
        __bindgen_anon_1: hl_type__bindgen_ty_1 {
            obj: ptr::null_mut(),
        },
        vobj_proto: ptr::null_mut(),
        mark_bits: ptr::null_mut(),
    };

    let mut ft: *mut hl_type = std::ptr::null_mut();
    // hl_track_call(HL_TRACK_DYNFIELD, on_dynfield(d, hfield));
    let addr = hlp_obj_lookup(d, hfield, &mut ft);
    if addr.is_null() {
        let d = hlp_obj_lookup_extra(d, hfield);
        return if d.is_null() {
            0
        } else {
            hlp_dyn_casti64(&d as *const _ as *mut _, _hlt_dyn)
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

    let _hlt_dyn: *mut hl_type = &mut hl_type {
        kind: hl_type_kind_HDYN,
        __bindgen_anon_1: hl_type__bindgen_ty_1 {
            obj: ptr::null_mut(),
        },
        vobj_proto: ptr::null_mut(),
        mark_bits: ptr::null_mut(),
    };

    match std::mem::transmute::<u32, hl_type_kind>((*(*obj).t).kind) {
        hl_type_kind_HOBJ | hl_type_kind_HVIRTUAL | hl_type_kind_HDYNOBJ | hl_type_kind_HSTRUCT => {
            hlp_dyn_getp(obj, hfield, _hlt_dyn) as *mut vdynamic
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

    let _hlt_dyn: *mut hl_type = &mut hl_type {
        kind: hl_type_kind_HDYN,
        __bindgen_anon_1: hl_type__bindgen_ty_1 {
            obj: ptr::null_mut(),
        },
        vobj_proto: ptr::null_mut(),
        mark_bits: ptr::null_mut(),
    };

    if v.is_null() {
        hlp_dyn_setp(obj, hfield, _hlt_dyn, ptr::null_mut());
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

    match std::mem::transmute::<u32, hl_type_kind>((*(*obj).t).kind) {
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

    match std::mem::transmute::<u32, hl_type_kind>((*(*obj).t).kind) {
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
pub extern "C" fn hlp_init_virtual(vt: *mut hl_type, _ctx: *mut hl_module_context) {
    unsafe {
        let virt = (*vt).__bindgen_anon_1.virt.as_mut().unwrap();

        let vsize = mem::size_of::<vvirtual>()
            + mem::size_of::<*mut std::os::raw::c_void>() * virt.nfields as usize;
        let mut size = vsize;

        let allocator = GC.get_mut_or_init(|| ImmixAllocator::new());

        let l = allocator
            .allocate(mem::size_of::<hl_field_lookup>() * virt.nfields as usize)
            .unwrap()
            .cast::<hl_field_lookup>()
            .as_ptr();

        let indexes = allocator
            .allocate(mem::size_of::<i32>() * virt.nfields as usize)
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
            .allocate(mark_size as usize)
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
