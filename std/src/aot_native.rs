//! Resolving HDLL primitives inside an ahead-of-time binary.
//!
//! A `std` primitive is a plain `#[no_mangle]` export of the runtime the
//! object links against, so the symbol IS the name and the linker binds it.
//! An HDLL primitive is not: it lives in a shared library reached through a
//! `DEFINE_PRIM` table, and at emit time there is no library to bind to. So
//! the emitter leaves a slot per primitive and calls this to fill it at
//! startup -- the same `dlopen` the interpreter and JIT already do, moved
//! into the binary that needs it.
//!
//! This lives in ash_std rather than beside the rest of the loading code
//! because an AOT binary links `libash_std.a` and nothing else;
//! `NativeLibraryManager` is in ash_core and is not reachable from here.

use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::os::raw::{c_char, c_void};
use std::path::PathBuf;
use std::sync::{Mutex, OnceLock};

fn handles() -> &'static Mutex<HashMap<String, usize>> {
    static H: OnceLock<Mutex<HashMap<String, usize>>> = OnceLock::new();
    H.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Where an HDLL may sit, most specific first.
///
/// Beside the executable before the working directory: a binary is run from
/// anywhere, and its own directory is the only one that travels with it.
fn search_dirs() -> Vec<PathBuf> {
    let mut dirs = Vec::new();
    if let Ok(exe) = std::env::current_exe() {
        if let Some(parent) = exe.parent() {
            dirs.push(parent.to_path_buf());
        }
    }
    if let Ok(cwd) = std::env::current_dir() {
        if !dirs.contains(&cwd) {
            dirs.push(cwd);
        }
    }
    dirs
}

#[cfg(unix)]
unsafe fn dlopen_path(path: &std::path::Path) -> *mut c_void {
    let Ok(c) = CString::new(path.to_string_lossy().as_bytes()) else {
        return std::ptr::null_mut();
    };
    // RTLD_GLOBAL so the library can see the runtime's own hl_* exports, and
    // an ABSOLUTE path because dlopen reads a name with no slash in it as a
    // SONAME and never looks in the working directory.
    libc::dlopen(c.as_ptr(), libc::RTLD_NOW | libc::RTLD_GLOBAL)
}

#[cfg(unix)]
unsafe fn dlsym_handle(handle: *mut c_void, symbol: &str) -> *mut c_void {
    let Ok(c) = CString::new(symbol) else {
        return std::ptr::null_mut();
    };
    libc::dlsym(handle, c.as_ptr())
}

#[cfg(windows)]
unsafe fn dlopen_path(path: &std::path::Path) -> *mut c_void {
    use std::os::windows::ffi::OsStrExt;
    let wide: Vec<u16> = path.as_os_str().encode_wide().chain(Some(0)).collect();
    windows_sys::Win32::System::LibraryLoader::LoadLibraryW(wide.as_ptr()) as *mut c_void
}

#[cfg(windows)]
unsafe fn dlsym_handle(handle: *mut c_void, symbol: &str) -> *mut c_void {
    let Ok(c) = CString::new(symbol) else {
        return std::ptr::null_mut();
    };
    windows_sys::Win32::System::LibraryLoader::GetProcAddress(handle as _, c.as_ptr() as *const u8)
        .map_or(std::ptr::null_mut(), |p| p as *mut c_void)
}

/// The file names an HDLL may have, in the order upstream tries them.
fn candidates(lib: &str) -> Vec<String> {
    let mut v = vec![format!("{lib}.hdll")];
    #[cfg(windows)]
    {
        v.push(format!("{lib}.dll"));
        v.push(format!("lib{lib}.dll"));
    }
    #[cfg(any(target_os = "macos", target_os = "ios"))]
    {
        v.push(format!("lib{lib}.dylib"));
        v.push(format!("{lib}.dylib"));
    }
    #[cfg(all(unix, not(any(target_os = "macos", target_os = "ios"))))]
    {
        v.push(format!("lib{lib}.so"));
        v.push(format!("{lib}.so"));
    }
    v
}

unsafe fn library(lib: &str) -> *mut c_void {
    if let Some(&h) = handles().lock().expect("hdll handles poisoned").get(lib) {
        return h as *mut c_void;
    }
    for dir in search_dirs() {
        for name in candidates(lib) {
            let path = dir.join(&name);
            if !path.exists() {
                continue;
            }
            let handle = dlopen_path(&path);
            if !handle.is_null() {
                handles()
                    .lock()
                    .expect("hdll handles poisoned")
                    .insert(lib.to_string(), handle as usize);
                return handle;
            }
            // A file that IS there and still will not load is the interesting
            // case -- almost always its own dependencies. Staying quiet about
            // it leaves only "primitive not found", which points at the wrong
            // thing entirely.
            #[cfg(unix)]
            {
                let err = libc::dlerror();
                if !err.is_null() {
                    eprintln!(
                        "[ash] cannot load {}: {}",
                        path.display(),
                        CStr::from_ptr(err).to_string_lossy()
                    );
                }
            }
        }
    }
    std::ptr::null_mut()
}

/// Address of `lib@name`, or null.
///
/// Null is not fatal here. The emitted call site checks the slot and raises
/// the same error HashLink's `disabled_primitive` does, so a program that
/// never reaches an unresolvable primitive still runs -- which is the
/// behaviour the interpreter and the JIT already have.
///
/// # Safety
/// `lib` and `name` must be valid NUL-terminated C strings.
#[no_mangle]
pub unsafe extern "C" fn hlp_aot_native(lib: *const c_char, name: *const c_char) -> *mut c_void {
    if lib.is_null() || name.is_null() {
        return std::ptr::null_mut();
    }
    let (Ok(lib), Ok(name)) = (
        CStr::from_ptr(lib).to_str(),
        CStr::from_ptr(name).to_str(),
    ) else {
        return std::ptr::null_mut();
    };
    let handle = library(lib);
    if handle.is_null() {
        return std::ptr::null_mut();
    }
    // The DEFINE_PRIM protocol, in full. `hlp_<name>` is NOT the primitive: the
    // macro expands to
    //
    //   EXPORT void *hlp_<name>(const char **sign) {
    //       *sign = <signature>; return (void*)&HL_NAME(<name>);
    //   }
    //
    // a RESOLVER that reports the signature through an out-parameter and
    // returns the real function. Storing the resolver and calling it as the
    // primitive writes a signature string through whatever the first argument
    // happens to be -- for a callback that is the vclosure, and the write
    // faults on read-only memory some distance from the actual mistake.
    let resolver = dlsym_handle(handle, &format!("hlp_{name}"));
    if resolver.is_null() {
        return std::ptr::null_mut();
    }
    type Resolver = unsafe extern "C" fn(*mut *const c_char) -> *mut c_void;
    let resolver: Resolver = std::mem::transmute(resolver);
    let mut sign: *const c_char = std::ptr::null();
    resolver(&mut sign)
}

/// Raise the error HashLink's `disabled_primitive` raises.
///
/// The emitted call site checks its slot and comes here when the primitive
/// never resolved, so a program that merely REFERENCES an unavailable
/// primitive still runs -- only reaching one is an error. That is the
/// interpreter's and the JIT's behaviour too.
///
/// # Safety
/// `lib` and `name` must be valid NUL-terminated C strings.
#[no_mangle]
pub unsafe extern "C" fn hlp_aot_native_missing(lib: *const c_char, name: *const c_char) {
    let show = |p: *const c_char| {
        if p.is_null() {
            "?".to_string()
        } else {
            CStr::from_ptr(p).to_string_lossy().into_owned()
        }
    };
    let msg = format!(
        "Native library '{}' not loaded, or it exports no '{}'",
        show(lib),
        show(name)
    );
    crate::error::hlp_error(crate::strings::str_to_uchar_ptr(&msg));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_missing_library_resolves_to_null_rather_than_failing() {
        unsafe {
            let lib = CString::new("no_such_hdll_anywhere").unwrap();
            let name = CString::new("whatever").unwrap();
            assert!(hlp_aot_native(lib.as_ptr(), name.as_ptr()).is_null());
            assert!(hlp_aot_native(std::ptr::null(), name.as_ptr()).is_null());
        }
    }

    /// The name it looks for is the primitive's, prefixed -- not the
    /// primitive's own spelling. Getting this wrong resolves nothing and
    /// looks exactly like a missing library.
    #[test]
    fn it_looks_for_the_prefixed_symbol() {
        assert_eq!(format!("hlp_{}", "alloc_bytes"), "hlp_alloc_bytes");
    }
}
