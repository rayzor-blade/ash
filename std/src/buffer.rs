use crate::hl::{
    self, _stringitem, hl_buffer, hl_type, hl_type_kind, hl_type_kind_HABSTRACT,
    hl_type_kind_HARRAY, hl_type_kind_HBOOL, hl_type_kind_HBYTES, hl_type_kind_HDYNOBJ,
    hl_type_kind_HENUM, hl_type_kind_HF32, hl_type_kind_HF64, hl_type_kind_HFUN, hl_type_kind_HI32,
    hl_type_kind_HI64, hl_type_kind_HMETHOD, hl_type_kind_HNULL, hl_type_kind_HOBJ,
    hl_type_kind_HPACKED, hl_type_kind_HREF, hl_type_kind_HSTRUCT, hl_type_kind_HTYPE,
    hl_type_kind_HUI16, hl_type_kind_HUI8, hl_type_kind_HVIRTUAL, hl_type_kind_HVOID, stringitem,
    tlist, uchar, varray, vclosure, vdynamic, vdynamic__bindgen_ty_1, vdynobj, venum, vlist,
    vvirtual, HL_DYNOBJ_INDEX_MASK, HL_DYNOBJ_INDEX_SHIFT,
};
use crate::obj::{hlp_field_name, hlp_get_obj_proto, hlp_hash_gen, hlp_lookup_find};
use crate::strings::{hlp_utf16_length, str_to_uchar_ptr};
use crate::types::{hl_aptr, hl_is_ptr, hlp_type_size, TSTR};

#[no_mangle]
pub unsafe extern "C" fn hlp_alloc_buffer() -> *mut hl_buffer {
    // Get the global GC instance
    let mut gc = crate::gc::gc_locked();

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
    let mut gc = crate::gc::gc_locked();

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

/// `toStringFun` may be an INTERPRETER STUB — a findex+1 sentinel stored as a
/// small integer — rather than a real code pointer, and under `--mode interp`
/// it always is: the module's function table is filled with sentinels, so no
/// `__string` in the program has a native address. Route a stub back through
/// the interpreter, exactly as `obj::vcall_fn_or_stub` already does for every
/// sibling method.
///
/// Returns null when there is no callable at all, which is the ONLY case
/// upstream prints the class name for (hashlink src/std/buffer.c:236).
unsafe fn call_tostring_or_stub(f: *mut c_void, this: *mut vdynamic) -> *const uchar {
    let addr = f as usize;
    if addr == 0 {
        return ptr::null();
    }
    if addr < 0x100000 {
        let Some(runner) = crate::fiber::closure_runner() else {
            return ptr::null();
        };
        let mut cl = crate::types::vclosure_new(crate::types::hlt_dyn(), f, 1, this as *mut c_void);
        return runner(&mut cl, ptr::null_mut(), 0) as *const uchar;
    }
    let g: unsafe extern "C" fn(*mut vdynamic) -> *const uchar = std::mem::transmute(f);
    g(this)
}

/// Invoke a zero-argument closure-valued `__string` field. Dynamic-object
/// values store a pointer to the closure in their pointer-slot array; the
/// closure's function can be either native code or an interpreter sentinel.
unsafe fn call_closure_tostring_or_stub(c: *mut vclosure) -> *const uchar {
    if c.is_null() || (*c).fun.is_null() {
        return ptr::null();
    }
    let addr = (*c).fun as usize;
    if addr < 0x100000 {
        let Some(runner) = crate::fiber::closure_runner() else {
            return ptr::null();
        };
        return runner(c, ptr::null_mut(), 0) as *const uchar;
    }
    if (*c).hasValue != 0 {
        let f: unsafe extern "C" fn(*mut c_void) -> *const uchar =
            std::mem::transmute((*c).fun);
        f((*c).value)
    } else {
        let f: unsafe extern "C" fn() -> *const uchar = std::mem::transmute((*c).fun);
        f()
    }
}

pub unsafe extern "C" fn hlp_buffer_content(b: *mut hl_buffer, len: *mut i32) -> *mut hl::uchar {
    // Get the global GC instance
    let mut gc = crate::gc::gc_locked();

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
    // Same guard as hlp_type_str: describing a corrupt type must not panic.
    if t.is_null() || (*t).kind as usize >= TSTR.len() {
        hlp_buffer_str(b, str_to_uchar_ptr("?"));
        return;
    }
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

    let mut cur = tlist { t, next: parents };
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

// DEFINE_PRIM(_BYTES, type_str, _TYPE) — bytecode asks for `std@type_str` by
// name, so this needs an export as well as the internal callers in cast.rs
// and obj.rs; without one the resolver fails the whole module load.
#[no_mangle]
pub unsafe extern "C" fn hlp_type_str(t: *mut hl_type) -> *const uchar {
    // A kind outside the table is a corrupt or non-type pointer; naming it
    // "?" beats indexing out of bounds, which panics inside the very
    // routine that exists to describe what went wrong (TestMisc,
    // Issue2937).
    if t.is_null() {
        return str_to_uchar_ptr("?");
    }
    let kind = (*t).kind as usize;
    if kind >= TSTR.len() {
        return str_to_uchar_ptr("?");
    }
    let _c = TSTR[kind];
    let c = str_to_uchar_ptr(_c);
    if _c != "null" {
        return c;
    }
    let b = hlp_alloc_buffer();
    hlp_type_str_rec(b, t, std::ptr::null_mut());
    hlp_buffer_content(b, std::ptr::null_mut())
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
            // hl_buffer_addr uses %.17g here, not the %.15g that Std.string
            // goes through. The two precisions are deliberate upstream.
            let s = str_to_uchar_ptr(&crate::strings::format_g(value, 17));
            hlp_buffer_str(b, s);
        }
        hl_type_kind_HBYTES => {
            let bytes_ptr = *(data as *mut *mut uchar);
            hlp_buffer_str(b, bytes_ptr);
        }
        hl_type_kind_HTYPE => {
            let mut tmp = crate::types::vdynamic_new(t, vdynamic__bindgen_ty_1 {
                    ptr: *(data as *mut *mut c_void),
                });
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
            let mut tmp = crate::types::vdynamic_new(t, vdynamic__bindgen_ty_1 {
                    ptr: *(data as *mut *mut c_void),
                });
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
            let mut tmp = crate::types::vdynamic_new(t, vdynamic__bindgen_ty_1 {
                    ptr: *(data as *mut *mut c_void),
                });
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
        }
        hl_type_kind_HUI8 => {
            let _str = format!("{}", (*v).v.ui8);
            let s = str_to_uchar_ptr(_str.as_str());
            let len = hlp_utf16_length(s);
            hlp_buffer_str_sub(b, s, len as i32);
        }
        hl_type_kind_HUI16 => {
            let _str = format!("{}", (*v).v.ui16);
            let s = str_to_uchar_ptr(_str.as_str());
            let len = hlp_utf16_length(s);
            hlp_buffer_str_sub(b, s, len as i32);
        }
        hl_type_kind_HI32 => {
            let _str = format!("{}", (*v).v.i);
            let s = str_to_uchar_ptr(_str.as_str());
            let len = hlp_utf16_length(s);
            hlp_buffer_str_sub(b, s, len as i32);
        }

        hl_type_kind_HI64 => {
            let _str = format!("{}", (*v).v.i64_);
            let s = str_to_uchar_ptr(_str.as_str());
            let len = hlp_utf16_length(s);
            hlp_buffer_str_sub(b, s, len as i32);
        }
        hl_type_kind_HF64 => {
            let _str = crate::strings::format_g((*v).v.d, 17);
            let s = str_to_uchar_ptr(_str.as_str());
            let len = hlp_utf16_length(s);
            hlp_buffer_str_sub(b, s, len as i32);
        }
        hl_type_kind_HBOOL => {
            if (*v).v.b {
                hlp_buffer_str_sub(b, str_to_uchar_ptr("true"), 4);
            } else {
                hlp_buffer_str_sub(b, str_to_uchar_ptr("false"), 5);
            }
        }
        hl_type_kind_HF32 => {
            let _str = format!("{:.9}", (*v).v.f);
            let s = str_to_uchar_ptr(_str.as_str());
            let len = hlp_utf16_length(s);
            hlp_buffer_str_sub(b, s, len as i32);
        }
        hl_type_kind_HBYTES => {
            hlp_buffer_str(b, (*v).v.bytes as *const uchar);
        }
        hl_type_kind_HFUN => {
            hlp_buffer_str_sub(b, str_to_uchar_ptr("function#"), 9);
            let _str = format!("{:p}", v);
            let s = str_to_uchar_ptr(_str.as_str());
            let len = hlp_utf16_length(s);
            hlp_buffer_str_sub(b, s, len as i32);
        }
        hl_type_kind_HMETHOD => {
            hlp_buffer_str_sub(b, str_to_uchar_ptr("method#"), 7);
            let _str = format!("{:p}", (*v).v.ptr);
            let s = str_to_uchar_ptr(_str.as_str());
            let len = hlp_utf16_length(s);
            hlp_buffer_str_sub(b, s, len as i32);
        }
        hl_type_kind_HOBJ | hl_type_kind_HSTRUCT => {
            let o = (*(*v).t).__bindgen_anon_1.obj;
            let rt = (*o).rt;

            // Do NOT filter out interpreter stubs here. The old
            // `.filter(|&f| (f as usize) > 0x100000)` discarded every
            // `__string` in an interpreted program -- all of them are
            // sentinels in that mode -- and silently fell through to the
            // class-name path below, so `"" + obj` printed the class name
            // instead of calling toString. Upstream reaches the name path
            // only when there is no toStringFun at all (buffer.c:236).
            let to_string_fn = if !rt.is_null() {
                let proto = hlp_get_obj_proto((*v).t);
                (*proto).toStringFun
            } else {
                None
            };
            // A String is field-0-is-HBYTES shaped, and its bytes ARE its
            // string form: upstream's String.__string hands back this.bytes
            // with no allocation, so calling it is free there. ash's is a
            // bytecode function that BUILDS a new string, and running it from
            // in here re-enters the buffer machinery this call is already
            // inside. Measured: the receiver was intact (this.bytes = "loop
            // a...", len 19) while the returned pointer was a fresh
            // allocation 0x50 further on holding garbage, which is how
            // "loop acc=2147483647" printed as two mojibake characters under
            // --jit-threshold 1. So string-likes take the direct-bytes path
            // below and never re-enter; every other object gets its toString.
            // Match String by NAME, not by shape. "field 0 is HBYTES" also
            // matches hl.types.ArrayBytes_* — so arrays took the raw-bytes
            // path and `a + ''` printed garbage instead of "[2,3,4]",
            // regressing Issue6349 and Issue6722. String is the one type whose
            // __string upstream returns this.bytes for; everything else,
            // arrays included, must get its real toString.
            let is_string_type = !(*o).name.is_null() && {
                const S: [u16; 6] = [
                    'S' as u16, 't' as u16, 'r' as u16, 'i' as u16, 'n' as u16, 'g' as u16,
                ];
                let n = (*o).name;
                (0..6).all(|i| *n.add(i) == S[i]) && *n.add(6) == 0
            };
            let string_like_fast_path = is_string_type;
            let this_ptr = if kind == hl_type_kind_HSTRUCT {
                (*v).v.ptr as *mut vdynamic
            } else {
                v
            };
            let to_string_out = match to_string_fn {
                Some(f) if !string_like_fast_path => {
                    call_tostring_or_stub(f as *mut c_void, this_ptr)
                }
                _ => ptr::null(),
            };

            if !to_string_out.is_null() {
                hlp_buffer_str(b, to_string_out);
            } else if !rt.is_null() {
                // No callable toString (or it produced nothing).
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
        }
        hl_type_kind_HTYPE => {
            hlp_buffer_str(b, hlp_type_str((*v).v.ptr as *mut hl::hl_type));
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
                    hl_aptr::<c_void>(a).add(i * stride as usize),
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

            let mut l = vlist { v, next: stack };

            hlp_buffer_char(b, '{' as u16);
            // An uninitialised virtual has no lookup table, and this is the
            // one consumer of it with no defence: the two in obj.rs return
            // null on a null table, while dereferencing it here abends the
            // process. Printing nothing is the wrong answer; aborting is a
            // worse one.
            if (*(*vv).t).__bindgen_anon_1.virt.as_ref().unwrap().lookup.is_null() {
                hlp_buffer_str_sub(b, str_to_uchar_ptr("}"), 1);
                return;
            }
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
                        *(*(*vv).t)
                            .__bindgen_anon_1
                            .virt
                            .as_ref()
                            .unwrap()
                            .indexes
                            .add((*f).field_index as usize) as usize,
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

            let mut l = vlist { v, next: stack };

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
                let slot = (*o)
                    .values
                    .add((*f).field_index as usize & HL_DYNOBJ_INDEX_MASK as usize);
                let closure = *slot as *mut vclosure;
                if !closure.is_null() {
                    hlp_buffer_str(b, call_closure_tostring_or_stub(closure));
                    return;
                }
            }

            hlp_buffer_char(b, '{' as u16);
            let mut indexes = [0i32; 128];
            let indexes_ptr = if (*o).nfields <= 128 {
                indexes.as_mut_ptr()
            } else {
                let mut gc = crate::gc::gc_locked();
                let size = ((*o).nfields as usize * std::mem::size_of::<i32>()) as usize;
                match gc.allocate(size) {
                    Some(ptr) => ptr.as_ptr() as *mut i32,
                    None => return, // Handle allocation failure
                }
            };

            for i in 0..(*o).nfields as usize {
                let f = (*o).lookup.add(i);
                *indexes_ptr.add((*f).field_index as usize >> HL_DYNOBJ_INDEX_SHIFT) = i as i32;
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

                let mut l = vlist { v, next: stack };

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
pub unsafe extern "C" fn hlp_buffer_val(b: *mut hl_buffer, v: *mut vdynamic) {
    hlp_buffer_rec(b, v, std::ptr::null_mut())
}

#[cfg(test)]
mod type_str_tests {
    use super::*;
    use crate::types::{
        hlt_array, hlt_bool, hlt_bytes, hlt_dyn, hlt_dynobj, hlt_f32, hlt_f64, hlt_i32, hlt_i64,
        hlt_type, hlt_void,
    };

    unsafe fn name_of(t: *mut hl_type) -> String {
        let p = hlp_type_str(t);
        assert!(!p.is_null(), "hlp_type_str handed back nothing");
        let mut len = 0usize;
        while *p.add(len) != 0 {
            len += 1;
        }
        String::from_utf16_lossy(std::slice::from_raw_parts(p, len))
    }

    /// The kinds TSTR names outright, each read through the crate's own
    /// persistent singleton rather than a hand-built hl_type. The table was
    /// once a slot short after "dynamic", which shifted every name from HOBJ
    /// on and printed "array" for objects; pinning the concrete names is what
    /// catches that class of slip.
    #[test]
    fn the_flat_kinds_get_their_documented_names() {
        unsafe {
            for (t, want) in [
                (hlt_void(), "void"),
                (hlt_i32(), "i32"),
                (hlt_i64(), "i64"),
                (hlt_f32(), "f32"),
                (hlt_f64(), "f64"),
                (hlt_bool(), "bool"),
                (hlt_bytes(), "bytes"),
                (hlt_dyn(), "dynamic"),
                (hlt_array(), "array"),
                (hlt_type(), "type"),
                (hlt_dynobj(), "dynobj"),
            ] {
                assert_eq!(name_of(t), want, "kind {}", (*t).kind);
            }
        }
    }

    /// Every name is indexed by kind, so the table has to be as long as the
    /// kind enum. It was 22 entries once, one short, which made any packed
    /// type's name an out-of-bounds panic inside the routine that exists to
    /// describe what went wrong.
    #[test]
    fn the_name_table_covers_every_kind() {
        assert_eq!(TSTR.len(), hl::hl_type_kind_HLAST as usize);
        assert_eq!(TSTR.len(), 23);
        // The recursive kinds are marked, not named, and the marker has to
        // stay spelled the way hlp_type_str tests for it.
        for k in [
            hl_type_kind_HFUN,
            hl_type_kind_HOBJ,
            hl_type_kind_HREF,
            hl_type_kind_HVIRTUAL,
            hl_type_kind_HABSTRACT,
            hl_type_kind_HENUM,
            hl_type_kind_HNULL,
            hl_type_kind_HMETHOD,
            hl_type_kind_HSTRUCT,
            hl_type_kind_HPACKED,
        ] {
            assert_eq!(TSTR[k as usize], "null", "kind {k} should recurse");
        }
    }

    /// A null type names itself "?" instead of faulting. hlp_type_str is
    /// reached from cast-error reporting, so it is called precisely when the
    /// pointer in hand is already suspect (Issue2937).
    #[test]
    fn a_null_type_is_named_rather_than_dereferenced() {
        unsafe {
            assert_eq!(name_of(std::ptr::null_mut()), "?");
        }
    }

    /// The returned string is NUL-terminated UTF-16, which is what the
    /// bytecode side reads it as. A UTF-8 pointer cast to `*const uchar`
    /// would still be non-null and would still terminate, but every
    /// character after the first would be wrong.
    #[test]
    fn the_result_is_nul_terminated_utf16() {
        unsafe {
            let p = hlp_type_str(hlt_dynobj());
            let want: Vec<u16> = "dynobj".encode_utf16().collect();
            for (i, u) in want.iter().enumerate() {
                assert_eq!(*p.add(i), *u, "unit {i}");
            }
            assert_eq!(*p.add(want.len()), 0, "missing terminator");
        }
    }

    /// DEFINE_PRIM(_BYTES, type_str, _TYPE). The export exists because the
    /// resolver looks the symbol up by name and fails the whole module load
    /// when it is absent -- the failure this test is here to prevent.
    #[test]
    fn the_exported_signature_is_the_one_upstream_declares() {
        let f: unsafe extern "C" fn(*mut hl_type) -> *const uchar = hlp_type_str;
        unsafe {
            assert!(!f(hlt_i32()).is_null());
        }
    }
}
