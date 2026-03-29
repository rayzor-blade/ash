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

use std::path::PathBuf;
use std::sync::Mutex;
use std::time::SystemTime;

struct ReloadState {
    bytecode_path: PathBuf,
    last_mtime: SystemTime,
}

static RELOAD_STATE: Mutex<Option<ReloadState>> = Mutex::new(None);

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
    let path = PathBuf::from(String::from_utf16_lossy(slice).into_owned());

    let mtime = std::fs::metadata(&path)
        .and_then(|m| m.modified())
        .unwrap_or(SystemTime::UNIX_EPOCH);

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
        PathBuf::from(String::from_utf16_lossy(slice).into_owned())
    } else {
        state.bytecode_path.clone()
    };

    let current_mtime = match std::fs::metadata(&check_path).and_then(|m| m.modified()) {
        Ok(t) => t,
        Err(_) => return false,
    };

    if current_mtime != state.last_mtime {
        state.last_mtime = current_mtime;
        true
    } else {
        false
    }
}
