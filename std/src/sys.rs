use crate::hl::vbyte;

#[no_mangle]
pub extern "C" fn hlp_sys_utf8_path() -> bool {
    true
}

#[no_mangle]
pub unsafe extern "C" fn hlp_sys_print(msg: *const vbyte) {
    if msg.is_null() {
        return;
    }

    // msg points to UTF-16 data (pairs of u16 little-endian) cast as *const u8.
    // Read u16 values until null terminator.
    let p = msg as *const u16;
    let mut len = 0usize;
    while *p.add(len) != 0 {
        len += 1;
    }
    let slice = std::slice::from_raw_parts(p, len);
    let s = String::from_utf16_lossy(slice);
    print!("{}", s);
    use std::io::Write;
    std::io::stdout().flush().ok();
}

#[no_mangle]
pub extern "C" fn hlp_sys_is64() -> bool {
    #[cfg(target_pointer_width = "64")]
    {
        true
    }
    #[cfg(not(target_pointer_width = "64"))]
    {
        false
    }
}

use std::ffi::c_void;
use std::path::PathBuf;
use std::sync::Mutex;
use std::time::SystemTime;

struct ReloadState {
    bytecode_path: PathBuf,
    last_mtime: SystemTime,
}

static RELOAD_STATE: Mutex<Option<ReloadState>> = Mutex::new(None);

/// Callback invoked when a file change is detected.
/// The runtime sets this to `perform_reload` during initialization.
/// Signature: fn(path: *const u16) -> bool (returns true if reload succeeded)
type ReloadCallback = unsafe extern "C" fn(*const u16) -> bool;
static RELOAD_CALLBACK: Mutex<Option<ReloadCallback>> = Mutex::new(None);

/// Register a callback to be invoked when a bytecode file change is detected.
#[no_mangle]
pub extern "C" fn hlp_set_reload_callback(cb: ReloadCallback) {
    *RELOAD_CALLBACK.lock().unwrap() = Some(cb);
}

/// Register the bytecode file path for reload monitoring.
/// Called once during runtime initialization.
#[no_mangle]
pub extern "C" fn hlp_setup_reload_check(path_utf16: *const u16) {
    if path_utf16.is_null() {
        return;
    }
    let mut len = 0;
    unsafe {
        while *path_utf16.add(len) != 0 {
            len += 1;
        }
    }
    let slice = unsafe { std::slice::from_raw_parts(path_utf16, len) };
    let path = PathBuf::from(String::from_utf16_lossy(slice));

    let mtime = std::fs::metadata(&path)
        .and_then(|m| m.modified())
        .unwrap_or(SystemTime::UNIX_EPOCH);

    if env_flag!("ASH_DBG_RELOAD") {
        eprintln!("[reload] registered path: {:?} mtime={:?}", path, mtime);
    }

    *RELOAD_STATE.lock().unwrap() = Some(ReloadState {
        bytecode_path: path,
        last_mtime: mtime,
    });
}

/// Check if the bytecode file has been modified since the last check.
///
/// Called per-frame by user code via `hl.Api.checkReload()`.
/// If `debug_alt_file` is non-null, uses that path instead of the registered one.
/// Returns `true` if the file changed (caller should trigger reload).
#[no_mangle]
pub extern "C" fn hlp_sys_check_reload(debug_alt_file: *const vbyte) -> bool {
    let mut guard = match RELOAD_STATE.lock() {
        Ok(g) => g,
        Err(_) => return false,
    };

    let state = match guard.as_mut() {
        Some(s) => s,
        None => return false, // Not initialized — reload not enabled
    };

    // Use debug_alt_file if provided, otherwise the registered path
    let check_path = if !debug_alt_file.is_null() {
        let p = debug_alt_file as *const u16;
        let mut len = 0;
        unsafe {
            while *p.add(len) != 0 {
                len += 1;
            }
        }
        let slice = unsafe { std::slice::from_raw_parts(p, len) };
        PathBuf::from(String::from_utf16_lossy(slice))
    } else {
        state.bytecode_path.clone()
    };

    let current_mtime = match std::fs::metadata(&check_path).and_then(|m| m.modified()) {
        Ok(t) => t,
        Err(_) => return false,
    };

    if current_mtime != state.last_mtime {
        state.last_mtime = current_mtime;
        if env_flag!("ASH_DBG_RELOAD") {
            eprintln!(
                "[reload] file changed: {:?} (old={:?} new={:?})",
                check_path, state.last_mtime, current_mtime
            );
        }
        // Invoke the reload callback if registered
        drop(guard); // release RELOAD_STATE lock before calling back
        if let Ok(cb_guard) = RELOAD_CALLBACK.lock() {
            if let Some(cb) = *cb_guard {
                let path_str: String = check_path.to_string_lossy().into();
                let mut utf16: Vec<u16> = path_str.encode_utf16().collect();
                utf16.push(0);
                drop(cb_guard); // release callback lock before invoking
                unsafe { cb(utf16.as_ptr()) };
            }
        }
        true
    } else {
        false
    }
}

// ============================================================================
// Additional sys functions needed by Heaps.io / real-world programs
// ============================================================================

/// Returns the path to the current .hl bytecode file (UTF-16).
#[no_mangle]
pub extern "C" fn hlp_sys_hl_file() -> *const u16 {
    if let Ok(guard) = RELOAD_STATE.lock() {
        if let Some(state) = guard.as_ref() {
            let s = state.bytecode_path.to_string_lossy();
            let mut utf16: Vec<u16> = s.encode_utf16().collect();
            utf16.push(0);
            let ptr = utf16.as_ptr();
            std::mem::forget(utf16);
            return ptr;
        }
    }
    // Return empty string if no path registered
    static EMPTY: [u16; 1] = [0];
    EMPTY.as_ptr()
}

#[no_mangle]
pub extern "C" fn hlp_sys_time() -> f64 {
    use std::time::{SystemTime, UNIX_EPOCH};
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs_f64()
}

#[no_mangle]
pub extern "C" fn hlp_sys_cpu_time() -> f64 {
    hlp_sys_time() // Simplified: use wall time
}

#[no_mangle]
pub extern "C" fn hlp_sys_get_env(_name: *const vbyte) -> *const vbyte {
    std::ptr::null()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_sys_string(_v: *mut c_void) -> *const vbyte {
    std::ptr::null()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_sys_set_flags(_v: i32) {}

// The VM event loop function, set by haxe.MainLoop via @:hlNative("std","sys_set_loop").
// After main() returns, the interpreter should call this in a loop.
static mut SYS_LOOP_FUNC: *mut std::ffi::c_void = std::ptr::null_mut();

#[no_mangle]
pub unsafe extern "C" fn hlp_sys_set_loop(func: *mut std::ffi::c_void) {
    eprintln!("[ash] hlp_sys_set_loop called with {:p}", func);
    SYS_LOOP_FUNC = func;
}

/// Returns the registered loop function (for the interpreter to call after main).
#[no_mangle]
pub unsafe extern "C" fn hlp_sys_get_loop() -> *mut std::ffi::c_void {
    SYS_LOOP_FUNC
}

#[no_mangle]
pub unsafe extern "C" fn hlp_resolve_symbol(
    _name: *const u8,
    _lib: *const u8,
    _p: *mut std::ffi::c_void,
) -> *mut std::ffi::c_void {
    // Runtime symbol resolution — used by JIT for dynamic linking.
    if _name.is_null() {
        return std::ptr::null_mut();
    }

    #[cfg(unix)]
    {
        // Resolve via dlsym since all libs are loaded with RTLD_GLOBAL.
        let sym = libc::dlsym(libc::RTLD_DEFAULT, _name as *const libc::c_char);
        if !sym.is_null() {
            return sym;
        }
    }

    #[cfg(windows)]
    {
        // Win32 has no RTLD_DEFAULT: GetProcAddress only ever searches the one
        // module it is handed, so reaching dlsym's "anything already mapped"
        // reach means walking the process module list ourselves. Enumeration
        // order is load order, which is the order dlsym would have searched.
        use windows_sys::Win32::Foundation::HMODULE;
        use windows_sys::Win32::System::LibraryLoader::GetProcAddress;
        use windows_sys::Win32::System::ProcessStatus::EnumProcessModules;
        use windows_sys::Win32::System::Threading::GetCurrentProcess;

        let process = GetCurrentProcess();
        let mut modules: Vec<HMODULE> = vec![std::ptr::null_mut(); 256];
        loop {
            let capacity = (modules.len() * std::mem::size_of::<HMODULE>()) as u32;
            let mut needed: u32 = 0;
            if EnumProcessModules(process, modules.as_mut_ptr(), capacity, &mut needed) == 0 {
                return std::ptr::null_mut();
            }
            let count = needed as usize / std::mem::size_of::<HMODULE>();
            // EnumProcessModules silently truncates rather than failing, and a
            // library loaded between two calls can grow the list, so keep
            // re-asking until the answer fits in the buffer we passed.
            if needed <= capacity {
                modules.truncate(count);
                break;
            }
            modules.resize(count, std::ptr::null_mut());
        }

        for module in modules {
            if let Some(sym) = GetProcAddress(module, _name) {
                return sym as *mut std::ffi::c_void;
            }
        }
    }

    std::ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_get_thread_info() -> *mut c_void {
    std::ptr::null_mut()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_sys_full_path(path: *const vbyte) -> *const vbyte {
    // Return the path unchanged (simplified: no canonical resolution)
    path
}

/// Upstream hl_sys_exe_path (sys.c). On macOS upstream uses
/// _NSGetExecutablePath + pstrdup; with HL_UTF8PATH (all non-Windows
/// targets, matching our hlp_sys_utf8_path() == true) the returned bytes
/// are NUL-terminated UTF-8, GC-allocated.
#[no_mangle]
pub unsafe extern "C" fn hlp_sys_exe_path() -> *mut vbyte {
    let Ok(exe) = std::env::current_exe() else {
        return std::ptr::null_mut();
    };
    let bytes = exe.to_string_lossy().into_owned().into_bytes();
    let out = crate::bytes::hlp_alloc_bytes(bytes.len() as i32 + 1);
    if out.is_null() {
        return std::ptr::null_mut();
    }
    std::ptr::copy_nonoverlapping(bytes.as_ptr(), out, bytes.len());
    *out.add(bytes.len()) = 0;
    out
}

#[no_mangle]
pub unsafe extern "C" fn hlp_sys_exists(path: *const vbyte) -> bool {
    if path.is_null() {
        return false;
    }
    let p = path as *const u16;
    let mut len = 0;
    while *p.add(len) != 0 {
        len += 1;
    }
    let s = String::from_utf16_lossy(std::slice::from_raw_parts(p, len));
    std::path::Path::new(&s).exists()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_sys_is_dir(path: *const vbyte) -> bool {
    if path.is_null() {
        return false;
    }
    let p = path as *const u16;
    let mut len = 0;
    while *p.add(len) != 0 {
        len += 1;
    }
    let s = String::from_utf16_lossy(std::slice::from_raw_parts(p, len));
    std::path::Path::new(&s).is_dir()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_file_open(_name: *const vbyte, _mode: *const vbyte) -> *mut c_void {
    std::ptr::null_mut() // Stub
}

#[no_mangle]
pub unsafe extern "C" fn hlp_file_close(_f: *mut c_void) {}

#[no_mangle]
pub unsafe extern "C" fn hlp_file_contents(_name: *const vbyte, _len: *mut i32) -> *const vbyte {
    std::ptr::null()
}

#[no_mangle]
pub unsafe extern "C" fn hlp_sys_sleep(seconds: f64) {
    // With fibers alive, sleeping is a scheduling window: let workers run
    // until the deadline instead of stalling the whole VM.
    if crate::fiber::fibers_active() {
        let deadline =
            std::time::Instant::now() + std::time::Duration::from_secs_f64(seconds.max(0.0));
        while std::time::Instant::now() < deadline {
            crate::fiber::block_yield();
        }
        return;
    }
    std::thread::sleep(std::time::Duration::from_secs_f64(seconds));
}
