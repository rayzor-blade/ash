//! HashLink ABI compatibility layer.
//!
//! The HashLink HDLLs (fmt.hdll, sdl.hdll, etc.) are compiled against
//! `libhl.dylib` which exports `hl_` prefixed symbols. Ash's stdlib
//! exports `hlp_` prefixed symbols. This module re-exports ash functions
//! under their `hl_` names so HDLLs can link against ash_std directly.

use std::ffi::{c_void, CString};
use std::ptr;

// ============================================================================
// Global type singletons (hlt_*)
//
// HashLink HDLLs reference these as global variables, not functions.
// Each is a static hl_type with the correct kind field set.
// ============================================================================

// Wrapper to make hl_type Sync for static globals.
// Public because the `pub static` hlt_* globals below expose it in their type.
#[repr(transparent)]
pub struct SyncHlType(hl_type);
unsafe impl Sync for SyncHlType {}

macro_rules! hlt_global {
    ($name:ident, $kind:expr) => {
        #[no_mangle]
        pub static $name: SyncHlType = SyncHlType(hl_type {
            kind: $kind,
            __bindgen_anon_1: hl_type__bindgen_ty_1 {
                obj: std::ptr::null_mut(),
            },
            vobj_proto: std::ptr::null_mut(),
            mark_bits: std::ptr::null_mut(),
        });
    };
}

hlt_global!(hlt_void, 0); // HVOID
hlt_global!(hlt_i32, 3); // HI32
hlt_global!(hlt_i64, 4); // HI64
hlt_global!(hlt_f32, 5); // HF32
hlt_global!(hlt_f64, 6); // HF64
hlt_global!(hlt_bool, 7); // HBOOL
hlt_global!(hlt_bytes, 8); // HBYTES
hlt_global!(hlt_dyn, 9); // HDYN
hlt_global!(hlt_array, 12); // HARRAY
hlt_global!(hlt_dynobj, 16); // HDYNOBJ
hlt_global!(hlt_abstract, 17); // HABSTRACT

use crate::hl::{self, hl_buffer, hl_type, hl_type__bindgen_ty_1, varray, vdynamic};

// ============================================================================
// Direct aliases: forward hl_XXX to hlp_XXX
// ============================================================================

#[no_mangle]
pub unsafe extern "C" fn hl_alloc_array(t: *mut hl_type, size: i32) -> *mut varray {
    crate::array::hlp_alloc_array(t, size)
}

#[no_mangle]
pub unsafe extern "C" fn hl_alloc_dynamic(t: *mut hl_type) -> *mut vdynamic {
    let result = crate::obj::hlp_alloc_dynamic(t);
    if env_flag!("ASH_DBG_ALLOC") {
        let kind = if !t.is_null() { (*t).kind } else { 999 };
        eprintln!("[hl_alloc_dynamic] t={:p} kind={} -> {:p}", t, kind, result);
    }
    result
}

#[no_mangle]
pub unsafe extern "C" fn hl_alloc_dynobj() -> *mut vdynamic {
    crate::obj::hlp_alloc_dynobj() as *mut vdynamic
}

#[no_mangle]
pub unsafe extern "C" fn hl_dyn_setd(d: *mut vdynamic, hfield: i32, value: f64) {
    crate::obj::hlp_dyn_setd(d, hfield, value);
}

#[no_mangle]
pub unsafe extern "C" fn hl_dyn_seti(d: *mut vdynamic, hfield: i32, t: *mut hl_type, value: i32) {
    crate::obj::hlp_dyn_seti(d, hfield, t, value);
}

#[no_mangle]
pub unsafe extern "C" fn hl_dyn_setp(
    d: *mut vdynamic,
    hfield: i32,
    t: *mut hl_type,
    value: *mut c_void,
) {
    crate::obj::hlp_dyn_setp(d, hfield, t, value);
}

#[no_mangle]
pub unsafe extern "C" fn hl_dyn_setf(d: *mut vdynamic, hfield: i32, value: f32) {
    crate::obj::hlp_dyn_setf(d, hfield, value);
}

#[no_mangle]
pub unsafe extern "C" fn hl_dyn_seti64(d: *mut vdynamic, hfield: i32, value: i64) {
    crate::obj::hlp_dyn_seti64(d, hfield, value);
}

// The reading half of the same family. A Windows loader resolves every import
// before it will map a module at all, so ui.hdll's hl_dyn_geti/hl_dyn_getp
// failed the whole load rather than the call that needed them.

#[no_mangle]
pub unsafe extern "C" fn hl_dyn_geti(d: *mut vdynamic, hfield: i32, t: *mut hl_type) -> i32 {
    crate::obj::hlp_dyn_geti(d, hfield, t)
}

#[no_mangle]
pub unsafe extern "C" fn hl_dyn_geti64(d: *mut vdynamic, hfield: i32) -> i64 {
    crate::obj::hlp_dyn_geti64(d, hfield)
}

#[no_mangle]
pub unsafe extern "C" fn hl_dyn_getf(d: *mut vdynamic, hfield: i32) -> f32 {
    crate::obj::hlp_dyn_getf(d, hfield)
}

#[no_mangle]
pub unsafe extern "C" fn hl_dyn_getd(d: *mut vdynamic, hfield: i32) -> f64 {
    crate::obj::hlp_dyn_getd(d, hfield)
}

#[no_mangle]
pub unsafe extern "C" fn hl_dyn_getp(
    d: *mut vdynamic,
    hfield: i32,
    t: *mut hl_type,
) -> *mut c_void {
    crate::obj::hlp_dyn_getp(d, hfield, t)
}

/// Upstream hands back this thread's `hl_thread_info`. ash keeps no such
/// registry, so the answer is the null `hlp_get_thread_info` documents — but
/// the symbol has to exist, because ui.hdll imports it whether or not the
/// program ever builds a `ui.Sentinel`.
#[no_mangle]
pub unsafe extern "C" fn hl_get_thread() -> *mut c_void {
    crate::sys::hlp_get_thread_info()
}

#[no_mangle]
pub unsafe extern "C" fn hl_hash_gen(name: *const hl::uchar, cache_name: bool) -> i32 {
    crate::obj::hlp_hash_gen(name, cache_name)
}

#[no_mangle]
pub unsafe extern "C" fn hl_make_dyn(data: *mut c_void, t: *mut hl_type) -> *mut vdynamic {
    crate::cast::hlp_make_dyn(data, t)
}

#[no_mangle]
pub unsafe extern "C" fn hl_throw(v: *mut vdynamic) {
    crate::error::hlp_throw(v);
}

#[no_mangle]
pub unsafe extern "C" fn hl_rethrow(v: *mut vdynamic) {
    crate::error::hlp_rethrow(v);
}

/// Upstream `hl.h:678`: `vdynamic *hl_dyn_call( vclosure *c, vdynamic **args, int nargs )`.
///
/// This took `(vdynamic*, varray*)` and forwarded to `hlp_call_method` — two
/// parameters where the ABI has three, and the wrong two. Every hdll calls the
/// C spelling, so `hl_dyn_call(closure, args, 1)` had the closure read as a
/// `vdynamic*`, the raw `vdynamic**` read as a `varray*` whose header is
/// whatever happened to precede it, and `nargs` dropped on the floor. The size
/// field came out of unrelated memory, which is why the symptom ranged from
/// "Too many arguments" to silence to corruption depending on the engine and
/// the allocation history.
///
/// `hlp_dyn_call` is already a faithful port of upstream's `std/fun.c:223`,
/// including the part `hlp_call_method` alone does not do: for a BOUND
/// closure it rebuilds an unbound one from the parent type and boxes
/// `c->value` as argument zero. Forwarding is therefore the whole fix — the
/// implementation was never missing, only unreachable from C.
#[no_mangle]
pub unsafe extern "C" fn hl_dyn_call(
    c: *mut hl::vclosure,
    args: *mut *mut vdynamic,
    nargs: i32,
) -> *mut vdynamic {
    crate::fun::hlp_dyn_call(c, args, nargs)
}

// ============================================================================
// Buffer functions (forward to existing hlp_buffer_*)
// ============================================================================

#[no_mangle]
pub unsafe extern "C" fn hl_alloc_buffer(_init: *const hl::uchar) -> *mut c_void {
    crate::buffer::hlp_alloc_buffer() as *mut c_void
}

#[no_mangle]
pub unsafe extern "C" fn hl_buffer_char(b: *mut c_void, c: u16) {
    crate::buffer::hlp_buffer_char(b as *mut hl_buffer, c);
}

#[no_mangle]
pub unsafe extern "C" fn hl_buffer_str(b: *mut c_void, s: *const hl::uchar) {
    crate::buffer::hlp_buffer_str(b as *mut hl_buffer, s);
}

#[no_mangle]
pub unsafe extern "C" fn hl_buffer_cstr(b: *mut c_void, s: *const u8) {
    if s.is_null() {
        return;
    }
    let cstr = std::ffi::CStr::from_ptr(s as *const std::ffi::c_char);
    if let Ok(st) = cstr.to_str() {
        let utf16 = crate::strings::str_to_uchar_ptr(st);
        crate::buffer::hlp_buffer_str(b as *mut hl_buffer, utf16);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hl_buffer_content(b: *mut c_void, len: *mut i32) -> *const hl::uchar {
    crate::buffer::hlp_buffer_content(b as *mut hl_buffer, len)
}

#[no_mangle]
pub unsafe extern "C" fn hl_buffer_val(b: *mut c_void, v: *mut vdynamic) {
    crate::buffer::hlp_buffer_val(b as *mut hl_buffer, v);
}

// ============================================================================
// GC functions
// ============================================================================

#[no_mangle]
pub unsafe extern "C" fn hl_gc_alloc_gen(t: *mut hl_type, size: i32, flags: i32) -> *mut c_void {
    // A Mach-O HDLL binds these `hl_*` imports to its `libhl.dylib`
    // dependency by install-name ordinal, even when ash exports the same ABI
    // from the main executable. That compatibility image can therefore see
    // its first allocation before ash's ordinary stdlib initializer reaches
    // it. HashLink treats allocation as an initialization boundary; do the
    // same here instead of aborting an otherwise valid fmt/ssl call with
    // "GC not initialized".
    let mut gc = crate::gc::gc_locked_init();
    let Some(ptr) = gc.allocate(size as usize) else {
        return ptr::null_mut();
    };
    let p = ptr.as_ptr();

    // Word zero belongs to the CALLER for every kind but one.
    //
    // Upstream's hl_gc_alloc_gen hands the block back untouched (gc.c:495-560
    // ends in a bare `return ptr`); anything that wants a type there writes it
    // itself, the way hl_alloc_dynamic does at gc.c:1255-1257. ash wrote the
    // type into word zero unconditionally, which silently overwrote the first
    // eight bytes of every block whose kind reserves them for something else:
    // the payload of an hl_gc_alloc_noptr byte buffer (MEM_KIND_NOPTR), an
    // hl_gc_alloc_raw struct's first field (MEM_KIND_RAW), and -- the reason
    // this was found -- the finalizer pointer that MEM_KIND_FINALIZER puts
    // there, which a collector would later call.
    //
    // `gc.allocate` zeroes, so leaving it alone gives upstream's semantics: a
    // null finalizer slot, and a buffer that starts as the caller expects.
    //
    // The write is kept for MEM_KIND_DYNAMIC, whose blocks are the only ones
    // conventionally shaped like a vdynamic. Upstream does not write even
    // there; narrowing it further is a separate change with its own blast
    // radius, and no reported defect turns on it.
    const PAGE_KIND_MASK: i32 = 3; // gc.c:76-77, (1 << PAGE_KIND_BITS) - 1
    const MEM_KIND_DYNAMIC: i32 = 0; // hl.h:745
    if flags & PAGE_KIND_MASK == MEM_KIND_DYNAMIC {
        (*(p as *mut vdynamic)).t = t;
    }
    p as *mut c_void
}

/// Upstream `hl_add_root` takes the address of a POINTER SLOT, not an object.
///
/// HashLink's root table is a `void***` and its mark phase does
/// `void *p = *gc_roots[i]`, re-reading the slot every cycle, which is why
/// every hdll is written as `hl_add_root(&h->data)` (uv) or
/// `hl_add_root(&on_dx_error)` (directx). We registered the ARGUMENT as an
/// object instead, and `mark_roots` only bounds-checked it against the Immix
/// arena, so a slot living in malloc'd memory was dropped with no diagnostic:
/// the closure went unmarked, line recycling handed its 32 bytes to the next
/// Haxe string, and `hlp_dyn_call` read the vclosure's type field as UTF-16
/// text -- a crash whose fault address spells the string that overwrote it.
///
/// That reached only slots OUTSIDE the arena. A slot inside it -- and
/// `hl_gc_alloc_finalizer` is an arena allocation, so `&struct->field` on a
/// struct an hdll allocated that way is an interior arena address -- passed
/// the bounds check, marked the containing allocation, and was reached anyway
/// by the transitive conservative trace. Such callers were never broken, and
/// are not what this path fixed.
///
/// So this is unconditionally a slot registration, exactly as upstream is.
/// An earlier version discriminated on `is_gc_ptr` to keep pinning objects for
/// callers that passed one directly, which was wrong twice over: `is_gc_ptr`
/// additionally requires the address's line MARK BIT -- set only while the
/// collector marks, cleared at sweep -- so during mutator execution, when every
/// `hl_add_root` call actually arrives, a live object fails it and took the
/// slot branch regardless. The discrimination never fired, and because the two
/// functions evaluated it independently at different times, an address could in
/// principle be filed in one set and looked up in the other, stranding the
/// entry. Dropping it makes the pair symmetric by construction.
///
/// Nothing internal regressed, because nothing internal came through here:
/// ash pins its own objects with `hlp_gc_register_root` (gc.rs) and
/// `register_persistent` (buffer.rs), never with `hl_add_root`.
///
/// Registering a slot costs nothing if it holds null or a non-heap value --
/// the collector's conservative read ignores those exactly as upstream does.
#[no_mangle]
pub unsafe extern "C" fn hl_add_root(ptr: *mut c_void) {
    if ptr.is_null() {
        return;
    }
    crate::gc::gc_locked_init().add_root_slot(ptr as usize);
}

#[no_mangle]
pub unsafe extern "C" fn hl_remove_root(ptr: *mut c_void) {
    if ptr.is_null() {
        return;
    }
    crate::gc::gc_locked_init().remove_root_slot(ptr as usize);
}

// ============================================================================
// String/encoding functions
// ============================================================================

#[no_mangle]
pub unsafe extern "C" fn hl_to_utf16(str: *const u8) -> *const hl::uchar {
    if str.is_null() {
        return ptr::null();
    }
    let cstr = std::ffi::CStr::from_ptr(str as *const std::ffi::c_char);
    if let Ok(s) = cstr.to_str() {
        let ptr = crate::strings::str_to_uchar_ptr(s);
        return ptr;
    }
    ptr::null()
}

#[no_mangle]
pub unsafe extern "C" fn hl_to_utf8(str: *const hl::uchar) -> *const u8 {
    if env_flag!("ASH_DBG_SHADER") {
        if !str.is_null() && (str as usize) > 0x10000 {
            let mut len = 0;
            while *str.add(len) != 0 && len < 200 {
                len += 1;
            }
            let s = String::from_utf16_lossy(std::slice::from_raw_parts(str, len));
            eprintln!(
                "[hl_to_utf8] len={} first100={:?}",
                len,
                &s[..s.len().min(100)]
            );
        } else {
            eprintln!("[hl_to_utf8] str={:p} (null or invalid)", str);
        }
    }
    if str.is_null() {
        return ptr::null();
    }
    let mut len = 0;
    while *str.add(len) != 0 {
        len += 1;
    }
    let s = String::from_utf16_lossy(std::slice::from_raw_parts(str, len));
    let cstr = std::ffi::CString::new(s).unwrap_or_default();
    let ptr = cstr.as_ptr() as *const u8;
    std::mem::forget(cstr);
    ptr
}

#[no_mangle]
pub unsafe extern "C" fn hl_from_utf8(str: *const u8, len: i32) -> *const hl::uchar {
    if str.is_null() {
        return ptr::null();
    }
    let bytes = std::slice::from_raw_parts(str, len as usize);
    let s = String::from_utf8_lossy(bytes);

    crate::strings::str_to_uchar_ptr(&s)
}

/// Format an HDLL `hl_error(...)` message and box it as HashLink `bytes`.
///
/// This is HashLink's public C ABI, not a raw UTF-16 allocator.  In
/// particular, hlsdl expands `hl_error("... %s", value)` to
/// `hl_throw(hl_alloc_strbytes(...))`, so changing either the arguments or
/// return type turns an ordinary catchable exception into memory corruption.
#[no_mangle]
pub unsafe extern "C" fn hl_alloc_strbytes(fmt: *const hl::uchar, mut args: ...) -> *mut vdynamic {
    if fmt.is_null() {
        return ptr::null_mut();
    }

    let mut units = Vec::<u16>::new();
    let mut pos = 0usize;
    while *fmt.add(pos) != 0 {
        let ch = *fmt.add(pos);
        pos += 1;
        if ch != b'%' as u16 {
            units.push(ch);
            continue;
        }

        let mut cfmt = String::from("%");
        let conversion = loop {
            let spec = *fmt.add(pos);
            pos += 1;
            if spec == 0 {
                break '\0';
            }
            let spec = char::from_u32(spec as u32).unwrap_or('\0');
            cfmt.push(spec);
            if matches!(spec, 'd' | 'f' | 'g' | 'x' | 'X' | 's' | '%') {
                break spec;
            }
        };

        if conversion == 's' {
            let value = args.next_arg::<*const hl::uchar>();
            if value.is_null() {
                units.extend("null".encode_utf16());
            } else {
                let mut i = 0usize;
                while *value.add(i) != 0 {
                    units.push(*value.add(i));
                    i += 1;
                }
            }
            continue;
        }
        if conversion == '%' {
            units.push(b'%' as u16);
            continue;
        }
        if conversion == '\0' {
            break;
        }

        let cfmt = CString::new(cfmt).expect("printf format contains NUL");
        let mut rendered = [0 as std::ffi::c_char; 128];
        let written = match conversion {
            'd' if cfmt.as_bytes().contains(&b'l') => libc::snprintf(
                rendered.as_mut_ptr(),
                rendered.len(),
                cfmt.as_ptr(),
                args.next_arg::<i64>(),
            ),
            'd' => libc::snprintf(
                rendered.as_mut_ptr(),
                rendered.len(),
                cfmt.as_ptr(),
                args.next_arg::<i32>(),
            ),
            'f' | 'g' => libc::snprintf(
                rendered.as_mut_ptr(),
                rendered.len(),
                cfmt.as_ptr(),
                args.next_arg::<f64>(),
            ),
            'x' | 'X' if cfmt.as_bytes().contains(&b'I') => libc::snprintf(
                rendered.as_mut_ptr(),
                rendered.len(),
                cfmt.as_ptr(),
                args.next_arg::<usize>(),
            ),
            'x' | 'X' if cfmt.as_bytes().contains(&b'l') => libc::snprintf(
                rendered.as_mut_ptr(),
                rendered.len(),
                cfmt.as_ptr(),
                args.next_arg::<*const c_void>(),
            ),
            'x' | 'X' => libc::snprintf(
                rendered.as_mut_ptr(),
                rendered.len(),
                cfmt.as_ptr(),
                args.next_arg::<i32>(),
            ),
            _ => 0,
        };
        let count = written.max(0) as usize;
        units.extend(
            rendered[..count.min(rendered.len().saturating_sub(1))]
                .iter()
                .map(|&byte| byte as u8 as u16),
        );
    }

    units.push(0);
    let d = crate::obj::hlp_alloc_dynamic(crate::types::hlt_bytes());
    if d.is_null() {
        return ptr::null_mut();
    }
    let bytes = crate::bytes::hlp_alloc_bytes((units.len() * 2) as i32);
    if bytes.is_null() {
        return ptr::null_mut();
    }
    std::ptr::copy_nonoverlapping(units.as_ptr().cast::<u8>(), bytes, units.len() * 2);
    (*d).v.ptr = bytes.cast();
    d
}

#[no_mangle]
pub unsafe extern "C" fn hl_hash_utf8(name: *const u8) -> i32 {
    // Hash UTF-8 bytes by first converting to UTF-16
    if name.is_null() {
        return 0;
    }
    let cstr = std::ffi::CStr::from_ptr(name as *const std::ffi::c_char);
    if let Ok(s) = cstr.to_str() {
        let utf16 = crate::strings::str_to_uchar_ptr(s);
        crate::obj::hlp_hash_gen(utf16, true)
    } else {
        0
    }
}

// ============================================================================
// Misc
// ============================================================================

#[no_mangle]
pub unsafe extern "C" fn hl_alloc_bytes(size: i32) -> *mut u8 {
    crate::bytes::hlp_alloc_bytes(size)
}

#[no_mangle]
pub unsafe extern "C" fn hl_copy_bytes(src: *const u8, size: i32) -> *mut u8 {
    let dst = hl_alloc_bytes(size);
    if size > 0 {
        std::ptr::copy_nonoverlapping(src, dst, size as usize);
    }
    dst
}

#[no_mangle]
pub unsafe extern "C" fn hl_throw_buffer(buf: *mut c_void) {
    // Convert buffer content to a string and throw as exception
    let mut len: i32 = 0;
    let content = crate::buffer::hlp_buffer_content(buf as *mut hl_buffer, &mut len);
    if !content.is_null() {
        let mut gc = crate::gc::gc_locked();
        let d = gc
            .allocate(std::mem::size_of::<vdynamic>())
            .expect("alloc")
            .as_ptr() as *mut vdynamic;
        (*d).t = crate::types::hlt_bytes();
        (*d).v.ptr = content as *mut c_void;
        hl_throw(d);
    }
}

#[no_mangle]
pub unsafe extern "C" fn hl_blocking(enter: bool) {
    crate::thread::hlp_blocking(enter);
}

// ============================================================================
// UTF-16 string utilities (exported by libhl.dylib)
// ============================================================================

// ustrdup is in ucs2.rs

#[no_mangle]
pub unsafe extern "C" fn ustrlen(s: *const u16) -> usize {
    if s.is_null() {
        return 0;
    }
    let mut len = 0;
    while *s.add(len) != 0 {
        len += 1;
    }
    len
}

#[no_mangle]
pub unsafe extern "C" fn uprintf(_fmt: *const u16, _: ...) {
    // Stub: UTF-16 printf
}

#[no_mangle]
pub unsafe extern "C" fn usprintf(_out: *mut u16, _size: i32, _fmt: *const u16, _: ...) -> i32 {
    0
}

#[no_mangle]
pub unsafe extern "C" fn uvszprintf(
    _out: *mut u16,
    _size: i32,
    _fmt: *const u16,
    _args: *mut c_void,
) -> i32 {
    0
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hl::vdynamic;

    // Both of these are pure type assertions: they never run anything, they
    // just fail to COMPILE if a signature drifts. That is the right shape of
    // test here, because the failure they guard against is not a wrong answer
    // -- it is a correctly-linked call with the arguments in the wrong places,
    // which no amount of exercising the Rust side can detect.

    #[test]
    fn hl_dyn_call_keeps_the_abi_hdlls_are_compiled_against() {
        // libhl's hl.h declares:
        //   vdynamic *hl_dyn_call( vclosure *c, vdynamic **args, int nargs );
        // ash shipped `(vdynamic *, varray *)` instead. An HDLL compiled
        // against the real header links against that happily and then passes a
        // closure where a vdynamic is expected, so the callee reads the arg
        // count out of an object header. Binding the symbol to the declared
        // type is what catches it.
        let _: unsafe extern "C" fn(*mut hl::vclosure, *mut *mut vdynamic, i32) -> *mut vdynamic =
            hl_dyn_call;
    }

    #[test]
    fn root_registration_keeps_the_abi_hdlls_are_compiled_against() {
        let _: unsafe extern "C" fn(*mut c_void) = hl_add_root;
        let _: unsafe extern "C" fn(*mut c_void) = hl_remove_root;
    }

    /// `hl_add_root` is handed the ADDRESS OF A SLOT, not the object in it --
    /// upstream keeps `void ***gc_roots` and marks `*gc_roots[i]`. Filing that
    /// address as an object instead marks the stack address itself (which is
    /// not in the heap, so it marks nothing) and leaves the real object
    /// unreachable, freeing it out from under a native library that is holding
    /// a reference it correctly registered.
    #[test]
    fn add_root_files_a_slot_address_as_a_slot() {
        unsafe {
            crate::gc::hlp_gc_init();
            let obj = crate::obj::hlp_alloc_dynamic(crate::types::hlt_i32());
            assert!(!obj.is_null(), "allocation failed, test cannot conclude");

            let mut slot: *mut vdynamic = obj;
            let slot_addr = &mut slot as *mut *mut vdynamic as *mut c_void;
            hl_add_root(slot_addr);

            let gc = crate::gc::gc_locked_init();
            assert!(
                gc.has_root_slot(slot_addr as usize),
                "the slot address was not registered as a slot"
            );
            assert!(
                !gc.has_persistent(slot_addr as *mut vdynamic),
                "the slot address was filed as an object -- this is the bug: \
                 marking would trace the stack address instead of the object"
            );
            drop(gc);

            hl_remove_root(slot_addr);
            assert!(!crate::gc::gc_locked_init().has_root_slot(slot_addr as usize));
        }
    }

    /// Every address is a slot, whatever it points at. Upstream has no
    /// object-pinning form of `hl_add_root`, and ash's own pinning goes through
    /// `hlp_gc_register_root` instead, so there is nothing to discriminate for.
    #[test]
    fn add_root_files_every_address_as_a_slot() {
        unsafe {
            crate::gc::hlp_gc_init();
            let obj = crate::obj::hlp_alloc_dynamic(crate::types::hlt_i32());
            assert!(!obj.is_null(), "allocation failed, test cannot conclude");

            hl_add_root(obj as *mut c_void);

            let gc = crate::gc::gc_locked_init();
            assert!(gc.has_root_slot(obj as usize));
            assert!(
                !gc.has_persistent(obj),
                "hl_add_root pinned an object -- upstream has no such form, and \
                 a caller wanting it should use hlp_gc_register_root"
            );
            drop(gc);

            hl_remove_root(obj as *mut c_void);
            assert!(!crate::gc::gc_locked_init().has_root_slot(obj as usize));
        }
    }

    /// The pair must agree whatever else is happening: add and remove have to
    /// name the same set by construction, not because they happened to be
    /// asked at a moment when some predicate answered the same way twice.
    ///
    /// This deliberately does NOT drive a collection to prove it. An earlier
    /// version called `collect_garbage()` here and hung the whole test binary:
    /// a collection stops the mutator world and waits for every registered
    /// mutator to park at a safepoint, and the sibling threads of a cargo test
    /// binary never do, so it spun on 6 cores until killed. The routing has no
    /// state-dependent branch left in it, so there is nothing a collection
    /// would add beyond that hazard.
    #[test]
    fn add_and_remove_name_the_same_set() {
        unsafe {
            crate::gc::hlp_gc_init();
            let mut slot: *mut vdynamic = crate::obj::hlp_alloc_dynamic(crate::types::hlt_i32());
            let addr = &mut slot as *mut *mut vdynamic as *mut c_void;

            hl_add_root(addr);
            assert!(crate::gc::gc_locked_init().has_root_slot(addr as usize));

            hl_remove_root(addr);
            assert!(
                !crate::gc::gc_locked_init().has_root_slot(addr as usize),
                "the root survived its own removal"
            );
        }
    }

    /// Upstream returns early on NULL rather than registering it; a null slot
    /// in the root set would be dereferenced on every mark.
    #[test]
    fn a_null_root_is_ignored_by_both_directions() {
        unsafe {
            crate::gc::hlp_gc_init();
            hl_add_root(ptr::null_mut());
            hl_remove_root(ptr::null_mut());
            assert!(!crate::gc::gc_locked_init().has_root_slot(0));
        }
    }
}
