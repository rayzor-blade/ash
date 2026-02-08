use crate::gc::GC;
use crate::hl::{
    self, _stringitem, hl_buffer, hl_type, hl_type_kind, hl_type_kind_HABSTRACT, hl_type_kind_HARRAY, hl_type_kind_HBOOL, hl_type_kind_HBYTES, hl_type_kind_HDYNOBJ, hl_type_kind_HENUM, hl_type_kind_HF32, hl_type_kind_HF64, hl_type_kind_HFUN, hl_type_kind_HI32, hl_type_kind_HI64, hl_type_kind_HMETHOD, hl_type_kind_HNULL, hl_type_kind_HOBJ, hl_type_kind_HPACKED, hl_type_kind_HREF, hl_type_kind_HSTRUCT, hl_type_kind_HTYPE, hl_type_kind_HUI16, hl_type_kind_HUI8, hl_type_kind_HVIRTUAL, hl_type_kind_HVOID, stringitem, tlist, uchar, varray, vclosure, vdynamic, vdynamic__bindgen_ty_1, vdynobj, venum, vlist, vvirtual, HL_DYNOBJ_INDEX_MASK, HL_DYNOBJ_INDEX_SHIFT
};
use crate::obj::{hlp_field_name, hlp_get_obj_proto, hlp_hash_gen, hlp_lookup_find};
use crate::strings::{hlp_utf16_length, str_to_uchar_ptr};
use crate::types::{hl_aptr, hl_is_ptr, hlp_type_size, TSTR};

#[no_mangle]
pub unsafe extern "C" fn hlp_alloc_buffer() -> *mut hl_buffer {
    // Get the global GC instance
    let gc = GC.get_mut().expect("GC not initialized");

    // Allocate memory for the hl_buffer struct
    let buffer_ptr = match gc.allocate(std::mem::size_of::<hl_buffer>()) {
        Some(ptr) => ptr.as_ptr() as *mut hl_buffer,
        None => return std::ptr::null_mut(), // Return null if allocation fails
    };

    // Initialize the buffer
    (*buffer_ptr).totlen = 0;
    (*buffer_ptr).blen = 16;
    (*buffer_ptr).data = std::ptr::null_mut();

    // Register the buffer as a root to prevent it from being collected
    gc.register_persistent(buffer_ptr as *mut vdynamic);

    buffer_ptr
}

#[no_mangle]
pub unsafe extern "C" fn buffer_append_new(b: *mut hl_buffer, s: *const uchar, len: i32) {
    // Get the global GC instance
    let gc = GC.get_mut().expect("GC not initialized");

    // Adjust buffer length if necessary
    while (*b).totlen >= ((*b).blen << 2) {
        (*b).blen <<= 1;
    }

    // Determine the size to allocate
    let size = if len < (*b).blen { (*b).blen } else { len };

    // Allocate memory for the _stringitem struct (NOT the pointer typedef)
    let it: stringitem = match gc.allocate(std::mem::size_of::<_stringitem>()) {
        Some(ptr) => ptr.as_ptr() as stringitem,
        None => return, // Return if allocation fails
    };

    // Allocate memory for the string data
    let str_ptr = match gc.allocate((size << 1) as usize) {
        Some(ptr) => ptr.as_ptr() as *mut uchar,
        None => return, // Return if allocation fails
    };

    // Copy the string data
    std::ptr::copy_nonoverlapping(s, str_ptr, len as usize);

    // Initialize the stringitem
    (*it).str_ = str_ptr;
    (*it).size = size;
    (*it).len = len;
    (*it).next = (*b).data;

    // Update the buffer
    (*b).data = it;

    // Update total length
    (*b).totlen += len;

    // Register the stringitem as a root
    gc.register_persistent(it as *mut vdynamic);
}

#[no_mangle]
pub unsafe extern "C" fn hlp_buffer_str_sub(b: *mut hl_buffer, mut s: *const uchar, mut len: i32) {
    // Check for null pointer or non-positive length
    if s.is_null() || len <= 0 {
        return;
    }

    // Get the first item in the buffer
    let it = (*b).data;
    if !it.is_null() {
        let free = (*it).size - (*it).len;
        if free >= len {
            // If there's enough space in the current item, copy the whole string
            std::ptr::copy_nonoverlapping(s, (*it).str_.add((*it).len as usize), len as usize);
            (*it).len += len;
            (*b).totlen += len;
            return;
        } else if free > 0 {
            // If there's some space, fill it and continue with the rest
            std::ptr::copy_nonoverlapping(s, (*it).str_.add((*it).len as usize), free as usize);
            (*it).len += free;
            (*b).totlen += free;
            // Adjust s and len for the remaining part
            s = s.add(free as usize);
            len -= free;
        }
    }

    // Append the remaining part as a new item (buffer_append_new updates totlen)
    buffer_append_new(b, s, len);
}

pub unsafe extern "C" fn hlp_buffer_str(b: *mut hl_buffer, s: *const uchar) {
    if !s.is_null() {
        let len = hlp_utf16_length(s) as i32;
        hlp_buffer_str_sub(b, s, len);
    } else {
        hlp_buffer_str_sub(b, str_to_uchar_ptr("null"), 4);
    }
}

pub unsafe extern "C" fn hlp_buffer_char(b: *mut hl_buffer, c: hl::uchar) {
    // Get the first item in the buffer
    let it = (*b).data;

    // Check if there's an existing item and if it has space
    if !it.is_null() && (*it).len != (*it).size {
        // Add the character to the existing item
        *(*it).str_.add((*it).len as usize) = c;
        (*it).len += 1;
        (*b).totlen += 1;
    } else {
        // Create a new item for the character (buffer_append_new updates totlen)
        let c_ptr: *const uchar = &c;
        buffer_append_new(b, c_ptr, 1);
    }
}

use std::ffi::c_void;
use std::ptr;

pub unsafe extern "C" fn hlp_buffer_content(b: *mut hl_buffer, len: *mut i32) -> *mut hl::uchar {
    // Get the global GC instance
    let gc = GC.get_mut().expect("GC not initialized");

    // Allocate memory for the buffer content
    let buf = match gc.allocate((((*b).totlen + 1) << 1) as usize) {
        Some(ptr) => ptr.as_ptr() as *mut hl::uchar,
        None => return ptr::null_mut(), // Return null if allocation fails
    };

    // Start from the end of the buffer
    let mut s = buf.add((*b).totlen as usize);

    // Null-terminate the string
    *s = 0;

    // Iterate through the stringitems
    let mut it = (*b).data;
    while !it.is_null() {
        // Move the pointer back by the length of the current item
        s = s.sub((*it).len as usize);

        // Copy the content of the current item
        ptr::copy_nonoverlapping((*it).str_, s, (*it).len as usize);

        // Move to the next item
        it = (*it).next;
    }

    // Set the length if the len pointer is not null
    if !len.is_null() {
        *len = (*b).totlen;
    }

    buf
}

pub unsafe extern "C" fn hlp_type_str_rec(b: *mut hl_buffer, t: *mut hl_type, parents: *mut tlist) {
    let c = TSTR[(*t).kind as usize];
    if c != "null" {
        hlp_buffer_str(b, str_to_uchar_ptr(c));
        return;
    }

    let mut l = parents;
    while !l.is_null() {
        if (*l).t == t {
            hlp_buffer_str(b, str_to_uchar_ptr("<...>"));
            return;
        }
        l = (*l).next;
    }

    let mut cur = tlist {
        t: t,
        next: parents,
    };
    let l = &mut cur as *mut tlist;

    match (*t).kind {
        hl_type_kind_HFUN | hl_type_kind_HMETHOD => {
            hlp_buffer_char(b, '(' as u16);
            hlp_type_str_rec(b, (*(*t).__bindgen_anon_1.fun).ret, l);
            hlp_buffer_char(b, ' ' as u16);
            hlp_buffer_char(b, '(' as u16);
            for i in 0..(*(*t).__bindgen_anon_1.fun).nargs as usize {
                if i > 0 {
                    hlp_buffer_char(b, ',' as u16);
                }
                hlp_type_str_rec(b, *(*(*t).__bindgen_anon_1.fun).args.add(i), l);
            }
            hlp_buffer_char(b, ')' as u16);
            hlp_buffer_char(b, ')' as u16);
        }
        hl_type_kind_HSTRUCT => {
            hlp_buffer_char(b, '@' as u16);
            hlp_buffer_str(b, (*(*t).__bindgen_anon_1.obj).name);
        }
        hl_type_kind_HOBJ => {
            hlp_buffer_str(b, (*(*t).__bindgen_anon_1.obj).name);
        }
        hl_type_kind_HREF => {
            hlp_buffer_str(b, str_to_uchar_ptr("ref<"));
            hlp_type_str_rec(b, (*t).__bindgen_anon_1.tparam, l);
            hlp_buffer_char(b, '>' as u16);
        }
        hl_type_kind_HVIRTUAL => {
            hlp_buffer_str(b, str_to_uchar_ptr("virtual<"));
            for i in 0..(*(*t).__bindgen_anon_1.virt).nfields as usize {
                let f = (*(*t).__bindgen_anon_1.virt).fields.add(i);
                if i > 0 {
                    hlp_buffer_char(b, ',' as u16);
                }
                hlp_buffer_str(b, (*f).name);
                hlp_buffer_char(b, ':' as u16);
                hlp_type_str_rec(b, (*f).t, l);
            }
            hlp_buffer_char(b, '>' as u16);
        }
        hl_type_kind_HABSTRACT => {
            hlp_buffer_str(b, (*t).__bindgen_anon_1.abs_name);
        }
        hl_type_kind_HENUM => {
            hlp_buffer_str(b, str_to_uchar_ptr("enum"));
            if !(*(*t).__bindgen_anon_1.tenum).name.is_null() {
                hlp_buffer_char(b, '<' as u16);
                hlp_buffer_str(b, (*(*t).__bindgen_anon_1.tenum).name);
                hlp_buffer_char(b, '>' as u16);
            }
        }
        hl_type_kind_HNULL => {
            hlp_buffer_str(b, str_to_uchar_ptr("null<"));
            hlp_type_str_rec(b, (*t).__bindgen_anon_1.tparam, l);
            hlp_buffer_char(b, '>' as u16);
        }
        hl_type_kind_HPACKED => {
            hlp_buffer_str(b, str_to_uchar_ptr("packed<"));
            hlp_type_str_rec(b, (*t).__bindgen_anon_1.tparam, l);
            hlp_buffer_char(b, '>' as u16);
        }
        _ => {
            hlp_buffer_str(b, str_to_uchar_ptr("???"));
        }
    }
}

pub unsafe extern "C" fn hlp_type_str(t: *mut hl_type) -> *const uchar {
    let _c = TSTR[(*t).kind as usize];
    let c = str_to_uchar_ptr(_c);
    if _c != "null" {
        return c;
    }
    let b = hlp_alloc_buffer();
    hlp_type_str_rec(b, t, std::ptr::null_mut());
    return hlp_buffer_content(b, std::ptr::null_mut());
}

pub unsafe extern "C" fn hlp_buffer_addr(
    b: *mut hl_buffer,
    data: *mut c_void,
    t: *mut hl_type,
    stack: *mut vlist,
) {
    match (*t).kind {
        hl_type_kind_HUI8 => {
            let value = *(data as *mut u8);
            let s = str_to_uchar_ptr(&format!("{}", value));
            hlp_buffer_str(b, s);
        }
        hl_type_kind_HUI16 => {
            let value = *(data as *mut u16);
            let s = str_to_uchar_ptr(&format!("{}", value));
            hlp_buffer_str(b, s);
        }
        hl_type_kind_HI32 => {
            let value = *(data as *mut i32);
            let s = str_to_uchar_ptr(&format!("{}", value));
            hlp_buffer_str(b, s);
        }
        hl_type_kind_HI64 => {
            let value = *(data as *mut i64);
            let s = str_to_uchar_ptr(&format!("{}", value));
            hlp_buffer_str(b, s);
        }
        hl_type_kind_HF32 => {
            let value = *(data as *mut f32);
            let s = str_to_uchar_ptr(&format!("{:.9}", value));
            hlp_buffer_str(b, s);
        }
        hl_type_kind_HF64 => {
            let value = *(data as *mut f64);
            let s = str_to_uchar_ptr(&format!("{}", value));
            hlp_buffer_str(b, s);
        }
        hl_type_kind_HBYTES => {
            let bytes_ptr = *(data as *mut *mut uchar);
            hlp_buffer_str(b, bytes_ptr);
        }
        hl_type_kind_HTYPE => {
            let mut tmp = vdynamic {
                t: t,
                v: vdynamic__bindgen_ty_1 {
                    ptr: *(data as *mut *mut c_void),
                },
            };
            hlp_buffer_rec(
                b,
                if !tmp.v.ptr.is_null() {
                    &mut tmp
                } else {
                    ptr::null_mut()
                },
                stack,
            );
        }
        hl_type_kind_HREF => {
            let mut tmp = vdynamic {
                t: t,
                v: vdynamic__bindgen_ty_1 {
                    ptr: *(data as *mut *mut c_void),
                },
            };
            hlp_buffer_rec(
                b,
                if !tmp.v.ptr.is_null() {
                    &mut tmp
                } else {
                    ptr::null_mut()
                },
                stack,
            );
        }
        hl_type_kind_HABSTRACT => {
            let mut tmp = vdynamic {
                t: t,
                v: vdynamic__bindgen_ty_1 {
                    ptr: *(data as *mut *mut c_void),
                },
            };
            hlp_buffer_rec(
                b,
                if !tmp.v.ptr.is_null() {
                    &mut tmp
                } else {
                    ptr::null_mut()
                },
                stack,
            );
        }
        hl_type_kind_HBOOL => {
            let value = *(data as *mut bool);
            if value {
                hlp_buffer_str_sub(b, str_to_uchar_ptr("true"), 4);
            } else {
                hlp_buffer_str_sub(b, str_to_uchar_ptr("false"), 5);
            }
        }
        _ => {
            let dyn_ptr = *(data as *mut *mut vdynamic);
            hlp_buffer_rec(b, dyn_ptr, stack);
        }
    }
}

pub unsafe extern "C" fn hlp_buffer_rec(b: *mut hl_buffer, v: *mut vdynamic, stack: *mut vlist) {
    if v.is_null() {
        hlp_buffer_str_sub(b, str_to_uchar_ptr("null"), 4);
        return;
    }
    let kind: hl_type_kind = (*(*v).t).kind;
    match kind {
        hl_type_kind_HVOID => {
            hlp_buffer_str_sub(b, str_to_uchar_ptr("void"), 4);
            return;
        }
        hl_type_kind_HUI8 => {
            let _str = format!("{}", (*v).v.ui8);
            let s = str_to_uchar_ptr(_str.as_str());
            let len = hlp_utf16_length(s);
            hlp_buffer_str_sub(b, s, len as i32);
            return;
        }
        hl_type_kind_HUI16 => {
            let _str = format!("{}", (*v).v.ui16);
            let s = str_to_uchar_ptr(_str.as_str());
            let len = hlp_utf16_length(s);
            hlp_buffer_str_sub(b, s, len as i32);
            return;
        }
        hl_type_kind_HI32 => {
            let _str = format!("{}", (*v).v.i);
            let s = str_to_uchar_ptr(_str.as_str());
            let len = hlp_utf16_length(s);
            hlp_buffer_str_sub(b, s, len as i32);
            return;
        }

        hl_type_kind_HI64 => {
            let _str = format!("{}", (*v).v.i64_);
            let s = str_to_uchar_ptr(_str.as_str());
            let len = hlp_utf16_length(s);
            hlp_buffer_str_sub(b, s, len as i32);
            return;
        }
        hl_type_kind_HF64 => {
            let _str = format!("{}", (*v).v.d);
            let s = str_to_uchar_ptr(_str.as_str());
            let len = hlp_utf16_length(s);
            hlp_buffer_str_sub(b, s, len as i32);
            return;
        }
        hl_type_kind_HBOOL => {
            if (*v).v.b {
                hlp_buffer_str_sub(b, str_to_uchar_ptr("true"), 4);
            } else {
                hlp_buffer_str_sub(b, str_to_uchar_ptr("false"), 5);
            }
            return;
        }
        hl_type_kind_HF32 => {
            let _str = format!("{:.9}", (*v).v.f);
            let s = str_to_uchar_ptr(_str.as_str());
            let len = hlp_utf16_length(s);
            hlp_buffer_str_sub(b, s, len as i32);
            return;
        }
        hl_type_kind_HBYTES => {
            hlp_buffer_str(b, (*v).v.bytes as *const uchar);
            return;
        }
        hl_type_kind_HFUN => {
            hlp_buffer_str_sub(b, str_to_uchar_ptr("function#"), 9);
            let _str = format!("{:p}", v);
            let s = str_to_uchar_ptr(_str.as_str());
            let len = hlp_utf16_length(s);
            hlp_buffer_str_sub(b, s, len as i32);
            return;
        }
        hl_type_kind_HMETHOD => {
            hlp_buffer_str_sub(b, str_to_uchar_ptr("method#"), 7);
            let _str = format!("{:p}", (*v).v.ptr);
            let s = str_to_uchar_ptr(_str.as_str());
            let len = hlp_utf16_length(s);
            hlp_buffer_str_sub(b, s, len as i32);
            return;
        }
        hl_type_kind_HOBJ | hl_type_kind_HSTRUCT => {
            let o = (*(*v).t).__bindgen_anon_1.obj;
            let rt = (*o).rt;

            // Check if toStringFun exists and is a real callable pointer
            // (not an interpreter stub where findex+1 is stored as a small integer)
            let to_string_fn = if !rt.is_null() {
                let proto = hlp_get_obj_proto((*v).t);
                if let Some(f) = (*proto).toStringFun {
                    if (f as usize) > 0x100000 {
                        Some(f)
                    } else {
                        None // Interpreter stub, not callable
                    }
                } else {
                    None
                }
            } else {
                None
            };

            if let Some(f) = to_string_fn {
                hlp_buffer_str(
                    b,
                    f(if kind == hl_type_kind_HSTRUCT {
                        (*v).v.ptr as *mut vdynamic
                    } else {
                        v
                    }),
                );
            } else if !rt.is_null() {
                // toStringFun is unavailable (interpreter stub or missing).
                // For String objects (field 0 is HBYTES), read the bytes pointer directly.
                let fi = (*rt).fields_indexes;
                let nfields = (*o).nfields;
                let is_string_like = nfields >= 1 && !fi.is_null() && {
                    let field0_type = (*(*o).fields.offset(0)).t;
                    !field0_type.is_null() && (*field0_type).kind == hl_type_kind_HBYTES
                };
                if is_string_like {
                    let field0_offset = *fi.offset(0) as usize;
                    let bytes_ptr = *((v as *const u8).add(field0_offset) as *const *const uchar);
                    if !bytes_ptr.is_null() {
                        hlp_buffer_str(b, bytes_ptr);
                    } else {
                        hlp_buffer_str(b, (*o).name);
                    }
                } else {
                    if kind == hl_type_kind_HSTRUCT {
                        hlp_buffer_char(b, '@' as u16);
                    }
                    hlp_buffer_str(b, (*o).name);
                }
            } else {
                if kind == hl_type_kind_HSTRUCT {
                    hlp_buffer_char(b, '@' as u16);
                }
                hlp_buffer_str(b, (*o).name);
            }
            return;
        }
        hl_type_kind_HTYPE => {
            hlp_buffer_str(b, hlp_type_str((*v).v.ptr as *mut hl::hl_type));
            return;
        }
        hl_type_kind_HREF => {
            hlp_buffer_str_sub(b, str_to_uchar_ptr("ref"), 3);
        }
        hl_type_kind_HARRAY => {
            let a = v as *mut varray;
            let at = (*a).at;
            let stride = hlp_type_size(at);
            let mut l = vlist { v, next: stack };

            let mut vtmp = stack;
            while !vtmp.is_null() {
                if (*vtmp).v == v {
                    hlp_buffer_str_sub(b, str_to_uchar_ptr("..."), 3);
                    return;
                }
                vtmp = (*vtmp).next;
            }

            hlp_buffer_char(b, '[' as u16);
            for i in 0..(*a).size as usize {
                if i > 0 {
                    hlp_buffer_str_sub(b, str_to_uchar_ptr(", "), 2);
                }
                hlp_buffer_addr(
                    b,
                    (hl_aptr(a) as *mut c_void).add(i * stride as usize),
                    at,
                    &mut l as *mut vlist,
                );
            }
            hlp_buffer_char(b, ']' as u16);
        }

        hl_type_kind_HVIRTUAL => {
            let vv = v as *mut vvirtual;
            if !(*vv).value.is_null() {
                hlp_buffer_rec(b, (*vv).value, stack);
                return;
            }

            let mut vtmp = stack;
            while !vtmp.is_null() {
                if (*vtmp).v == v {
                    hlp_buffer_str_sub(b, str_to_uchar_ptr("..."), 3);
                    return;
                }
                vtmp = (*vtmp).next;
            }

            let mut l = vlist { v: v, next: stack };

            hlp_buffer_char(b, '{' as u16);
            for i in 0..(*(*vv).t).__bindgen_anon_1.virt.as_ref().unwrap().nfields as usize {
                let f = (*(*vv).t)
                    .__bindgen_anon_1
                    .virt
                    .as_ref()
                    .unwrap()
                    .lookup
                    .add(i);
                if i > 0 {
                    hlp_buffer_str_sub(b, str_to_uchar_ptr(", "), 2);
                }
                hlp_buffer_str(b, hlp_field_name((*f).hashed_name) as *const uchar);
                hlp_buffer_str_sub(b, str_to_uchar_ptr(" : "), 3);
                hlp_buffer_addr(
                    b,
                    (v as *mut c_void).add(
                        (*(*vv).t)
                            .__bindgen_anon_1
                            .virt
                            .as_ref()
                            .unwrap()
                            .indexes
                            .wrapping_add((*f).field_index as usize)
                            as usize,
                    ),
                    (*f).t,
                    &mut l as *mut vlist,
                );
            }
            hlp_buffer_char(b, '}' as u16);
        }

        hl_type_kind_HDYNOBJ => {
            let o = v as *mut vdynobj;
            let mut vtmp = stack;
            while !vtmp.is_null() {
                if (*vtmp).v == v {
                    hlp_buffer_str_sub(b, str_to_uchar_ptr("..."), 3);
                    return;
                }
                vtmp = (*vtmp).next;
            }

            let mut l = vlist { v: v, next: stack };

            let f = hlp_lookup_find(
                (*o).lookup,
                (*o).nfields,
                hlp_hash_gen(str_to_uchar_ptr("__string"), false),
            );
            if !f.is_null()
                && (*(*f).t).kind == hl_type_kind_HFUN
                && (*(*f).t).__bindgen_anon_1.fun.as_ref().unwrap().nargs == 0
                && (*(*(*f).t).__bindgen_anon_1.fun.as_ref().unwrap().ret).kind
                    == hl_type_kind_HBYTES
            {
                let v = (*o)
                    .values
                    .add((*f).field_index as usize & HL_DYNOBJ_INDEX_MASK as usize)
                    as *mut vclosure;
                if !v.is_null() {
                    if (*v).hasValue != 0 {
                        let fun: unsafe extern "C" fn(*mut c_void) -> *mut uchar =
                            std::mem::transmute((*v).fun);
                        hlp_buffer_str(b, fun((*v).value));
                    } else {
                        let fun: unsafe extern "C" fn() -> *mut uchar =
                            std::mem::transmute((*v).fun);
                        hlp_buffer_str(b, fun());
                    }
                    return;
                }
            }

            hlp_buffer_char(b, '{' as u16);
            let mut indexes = [0i32; 128];
            let indexes_ptr = if (*o).nfields <= 128 {
                indexes.as_mut_ptr()
            } else {
                let gc = GC.get_mut().expect("GC not initialized");
                let size = ((*o).nfields as usize * std::mem::size_of::<i32>()) as usize;
                match gc.allocate(size) {
                    Some(ptr) => ptr.as_ptr() as *mut i32,
                    None => return, // Handle allocation failure
                }
            };

            for i in 0..(*o).nfields as usize {
                let f = (*o).lookup.add(i);
                *indexes_ptr.add(((*f).field_index as usize >> HL_DYNOBJ_INDEX_SHIFT) as usize) =
                    i as i32;
            }

            for i in 0..(*o).nfields as usize {
                let f = (*o).lookup.add(*indexes_ptr.add(i) as usize);
                if i > 0 {
                    hlp_buffer_str_sub(b, str_to_uchar_ptr(", "), 2);
                }
                hlp_buffer_str(b, hlp_field_name((*f).hashed_name) as *const uchar);
                hlp_buffer_str_sub(b, str_to_uchar_ptr(" : "), 3);
                let ptr = if hl_is_ptr((*f).t) {
                    (*o).values
                        .add((*f).field_index as usize & HL_DYNOBJ_INDEX_MASK as usize)
                        as *mut c_void
                } else {
                    (*o).raw_data
                        .add((*f).field_index as usize & HL_DYNOBJ_INDEX_MASK as usize)
                        as *mut c_void
                };
                hlp_buffer_addr(b, ptr, (*f).t, &mut l as *mut vlist);
            }

            if (*o).nfields > 128 {
                // Instead of hl_gc_free, we don't need to explicitly free
                // Our GC will handle this automatically
            }

            hlp_buffer_char(b, '}' as u16);
        }

        hl_type_kind_HENUM => {
            let e = v as *mut venum;
            let c = (*(*(*v).t).__bindgen_anon_1.tenum)
                .constructs
                .add((*e).index as usize);
            if (*c).nparams == 0 {
                hlp_buffer_str(b, (*c).name);
            } else {
                let mut vtmp = stack;
                while !vtmp.is_null() {
                    if (*vtmp).v == v {
                        hlp_buffer_str_sub(b, str_to_uchar_ptr("..."), 3);
                        return;
                    }
                    vtmp = (*vtmp).next;
                }

                let mut l = vlist { v: v, next: stack };

                hlp_buffer_str(b, (*c).name);
                hlp_buffer_char(b, '(' as u16);
                for i in 0..(*c).nparams as usize {
                    if i > 0 {
                        hlp_buffer_char(b, ',' as u16);
                    }
                    hlp_buffer_addr(
                        b,
                        (v as *mut c_void).add(*(*c).offsets.add(i) as usize),
                        *(*c).params.add(i),
                        &mut l as *mut vlist,
                    );
                }
                hlp_buffer_char(b, ')' as u16);
            }
        }
        hl_type_kind_HABSTRACT => {
            hlp_buffer_char(b, '~' as u16);
            hlp_buffer_str(b, (*(*v).t).__bindgen_anon_1.abs_name);
            hlp_buffer_char(b, ':' as u16);
            let ptr_str = format!("{:p}", (*v).v.ptr as *const c_void);
            let uchar_ptr = str_to_uchar_ptr(&ptr_str);
            hlp_buffer_str(b, uchar_ptr);
        }
        _ => {
            let ptr_str = format!("{:p}H", v as *const c_void);
            let uchar_ptr = str_to_uchar_ptr(&ptr_str);
            hlp_buffer_str(b, uchar_ptr);
        }
    }
}


#[no_mangle]
pub unsafe extern "C" fn hlp_buffer_val(b:*mut hl_buffer, v: *mut vdynamic) {
    return hlp_buffer_rec(b, v, std::ptr::null_mut())
}