//! `sys_*` natives: process, environment and filesystem access.
//!
//! Path/environment strings crossing this boundary are `pchar` in upstream's
//! sys.c: NUL-terminated UTF-8 where `HL_UTF8PATH` is defined, UTF-16 where it
//! is not. Which one the VM sends is decided entirely by what `sys_utf8_path`
//! answers, and `hlp_sys_utf8_path` below answers `true` on every target ash
//! builds for — so every `pchar` here is UTF-8, Windows included. That is one
//! target's worth of divergence from upstream (which uses UTF-16 paths on
//! Windows) and it is deliberate: `std::fs` re-encodes UTF-8 to UTF-16 for the
//! Win32 call anyway, so a single decode path serves all three platforms with
//! no loss of Unicode range. `uchar` (UTF-16) still appears where the C says
//! `uchar` regardless of platform — `sys_print` and `sys_string`.

use crate::array::hlp_alloc_array;
use crate::bytes::hlp_alloc_bytes;
use crate::hl::{varray, vbyte};
use crate::types::{hl_aptr, hlt_bytes, hlt_i32};
use std::ffi::{c_void, OsStr, OsString};
use std::io::Write;
use std::path::PathBuf;
use std::sync::atomic::{AtomicI32, AtomicPtr, Ordering};
use std::sync::Mutex;
use std::time::SystemTime;

// ============================================================================
// pchar / uchar marshalling
// ============================================================================

/// Bytes of a NUL-terminated `pchar*`, excluding the terminator.
unsafe fn pchar_slice<'a>(p: *const vbyte) -> &'a [u8] {
    let mut len = 0usize;
    while *p.add(len) != 0 {
        len += 1;
    }
    std::slice::from_raw_parts(p, len)
}

/// Units of a NUL-terminated `uchar*`, excluding the terminator.
unsafe fn uchar_slice<'a>(p: *const u16) -> &'a [u16] {
    let mut len = 0usize;
    while *p.add(len) != 0 {
        len += 1;
    }
    std::slice::from_raw_parts(p, len)
}

/// The OS's own encoding for a `pchar*`. On unix the bytes are already the
/// filesystem encoding and are taken verbatim, so a path the OS accepts but
/// UTF-8 cannot describe still round-trips.
unsafe fn pchar_to_os(p: *const vbyte) -> Option<OsString> {
    if p.is_null() {
        return None;
    }
    let bytes = pchar_slice(p);
    #[cfg(unix)]
    {
        use std::os::unix::ffi::OsStringExt;
        Some(OsString::from_vec(bytes.to_vec()))
    }
    #[cfg(windows)]
    {
        Some(OsString::from(String::from_utf8_lossy(bytes).into_owned()))
    }
}

unsafe fn pchar_to_path(p: *const vbyte) -> Option<PathBuf> {
    pchar_to_os(p).map(PathBuf::from)
}

/// Inverse of `pchar_to_os`.
fn os_to_pbytes(s: &OsStr) -> Vec<u8> {
    #[cfg(unix)]
    {
        use std::os::unix::ffi::OsStrExt;
        s.as_bytes().to_vec()
    }
    #[cfg(windows)]
    {
        s.to_string_lossy().into_owned().into_bytes()
    }
}

/// GC-allocated NUL-terminated copy, the `pstrdup` of sys.c. The VM owns the
/// result, so it cannot come from the Rust heap.
unsafe fn alloc_pbytes(data: &[u8]) -> *mut vbyte {
    let out = hlp_alloc_bytes(data.len() as i32 + 1);
    if out.is_null() {
        return std::ptr::null_mut();
    }
    std::ptr::copy_nonoverlapping(data.as_ptr(), out, data.len());
    *out.add(data.len()) = 0;
    out
}

/// GC-allocated NUL-terminated UTF-16, for the `uchar*`-returning prims.
unsafe fn alloc_ubytes(s: &str) -> *mut vbyte {
    let units: Vec<u16> = s.encode_utf16().collect();
    let out = hlp_alloc_bytes(((units.len() + 1) * 2) as i32);
    if out.is_null() {
        return std::ptr::null_mut();
    }
    let u = out as *mut u16;
    std::ptr::copy_nonoverlapping(units.as_ptr(), u, units.len());
    *u.add(units.len()) = 0;
    out
}

/// Build an `_ARR` of `_BYTES` from already-marshalled entries.
unsafe fn alloc_bytes_array(entries: &[Vec<u8>]) -> *mut varray {
    let a = hlp_alloc_array(hlt_bytes(), entries.len() as i32);
    if a.is_null() {
        return std::ptr::null_mut();
    }
    let slots = hl_aptr::<*mut vbyte>(a);
    for (i, e) in entries.iter().enumerate() {
        *slots.add(i) = alloc_pbytes(e);
    }
    a
}

// ============================================================================
// Platform identity
// ============================================================================

#[no_mangle]
pub extern "C" fn hlp_sys_utf8_path() -> bool {
    true
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

/// UTF-16 always: Sys.systemName decodes this with `String.fromUCS2`,
/// on every platform.
#[no_mangle]
pub unsafe extern "C" fn hlp_sys_string() -> *mut vbyte {
    let name = if cfg!(target_os = "windows") {
        "Windows"
    } else if cfg!(target_os = "macos") {
        "Mac"
    } else if cfg!(target_os = "ios") {
        "iOS"
    } else if cfg!(target_os = "tvos") {
        "tvOS"
    } else if cfg!(target_os = "android") {
        "Android"
    } else if cfg!(target_os = "linux") {
        "Linux"
    } else if cfg!(any(
        target_os = "freebsd",
        target_os = "openbsd",
        target_os = "netbsd",
        target_os = "dragonfly"
    )) {
        "BSD"
    } else {
        // Upstream #errors here. Reporting the target's own name is more
        // useful than a wrong-but-familiar one; the Haxe callers that branch
        // on this only ever test for "Windows".
        std::env::consts::OS
    };
    alloc_ubytes(name)
}

/// `getenv("LANG")` upstream; the system default locale name on Windows,
/// where there is no LANG. Returns null when neither is set — heaps and
/// friends fall back to "en" on null.
#[no_mangle]
pub unsafe extern "C" fn hlp_sys_locale() -> *mut vbyte {
    #[cfg(not(windows))]
    {
        match std::env::var_os("LANG") {
            Some(v) => alloc_pbytes(&os_to_pbytes(&v)),
            None => std::ptr::null_mut(),
        }
    }
    #[cfg(windows)]
    {
        use windows_sys::Win32::Globalization::GetSystemDefaultLocaleName;
        // LOCALE_NAME_MAX_LENGTH
        let mut buf = [0u16; 85];
        let len = GetSystemDefaultLocaleName(buf.as_mut_ptr(), buf.len() as i32);
        if len <= 0 {
            return std::ptr::null_mut();
        }
        // Upstream hands back the UTF-16 directly because Windows pchar is
        // uchar there; ours is UTF-8 (see the module note), so re-encode.
        let s = String::from_utf16_lossy(&buf[..(len as usize - 1)]);
        alloc_pbytes(s.as_bytes())
    }
}

// ============================================================================
// stdout
// ============================================================================

/// PR_AUTO_FLUSH. PR_WIN_UTF8 (1) has no analogue here: `hlp_sys_print`
/// always writes UTF-8 bytes rather than going through the Windows CRT's
/// `_setmode` wide-character mode.
const PR_AUTO_FLUSH: i32 = 2;
static PRINT_FLAGS: AtomicI32 = AtomicI32::new(PR_AUTO_FLUSH);

#[no_mangle]
pub unsafe extern "C" fn hlp_sys_set_flags(flags: i32) -> i32 {
    PRINT_FLAGS.store(flags, Ordering::Relaxed);
    flags
}

#[no_mangle]
pub unsafe extern "C" fn hlp_sys_print(msg: *const vbyte) {
    if msg.is_null() {
        return;
    }
    let s = String::from_utf16_lossy(uchar_slice(msg as *const u16));
    // print! panics on a write error; a panic crossing back into VM frames is
    // undefined behaviour, so the error is swallowed the way fputs would.
    let mut out = std::io::stdout().lock();
    let _ = out.write_all(s.as_bytes());
    if PRINT_FLAGS.load(Ordering::Relaxed) & PR_AUTO_FLUSH != 0 {
        let _ = out.flush();
    }
}

// ============================================================================
// Process lifetime, profiling hooks
// ============================================================================

static PROFILE_EVENT: AtomicPtr<c_void> = AtomicPtr::new(std::ptr::null_mut());
static BEFORE_EXIT: AtomicPtr<c_void> = AtomicPtr::new(std::ptr::null_mut());

/// Upstream `hl_setup_profiler`. `before_exit` is recorded but nothing calls
/// it yet: `hlp_sys_exit` lives in thread.rs, which this file does not own.
#[no_mangle]
pub unsafe extern "C" fn hlp_setup_profiler(profile_event: *mut c_void, before_exit: *mut c_void) {
    PROFILE_EVENT.store(profile_event, Ordering::Relaxed);
    BEFORE_EXIT.store(before_exit, Ordering::Relaxed);
}

#[no_mangle]
pub unsafe extern "C" fn hlp_sys_profile_event(code: i32, data: *mut vbyte, data_len: i32) {
    let f = PROFILE_EVENT.load(Ordering::Relaxed);
    if f.is_null() {
        return;
    }
    let f = std::mem::transmute::<*mut c_void, unsafe extern "C" fn(i32, *mut vbyte, i32)>(f);
    f(code, data, data_len);
}

#[no_mangle]
pub extern "C" fn hlp_sys_getpid() -> i32 {
    std::process::id() as i32
}

/// Upstream errors out with "Unknown sys_special key" for every key on
/// non-mobile targets; ash has no mobile target, so every key errors.
#[no_mangle]
pub unsafe extern "C" fn hlp_sys_special(_key: *const vbyte) -> *mut vbyte {
    crate::error::hlp_error(crate::strings::str_to_uchar_ptr("Unknown sys_special key"));
    std::ptr::null_mut()
}

// ============================================================================
// Time
// ============================================================================

#[no_mangle]
pub extern "C" fn hlp_sys_time() -> f64 {
    use std::time::UNIX_EPOCH;
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs_f64()
}

/// Process CPU time, user + system.
#[no_mangle]
pub extern "C" fn hlp_sys_cpu_time() -> f64 {
    #[cfg(unix)]
    unsafe {
        // Upstream uses times()/CLK_TCK, whose resolution is 10ms; getrusage
        // reports the same quantity to the microsecond.
        let mut ru: libc::rusage = std::mem::zeroed();
        if libc::getrusage(libc::RUSAGE_SELF, &mut ru) != 0 {
            return 0.0;
        }
        let secs = |t: &libc::timeval| t.tv_sec as f64 + t.tv_usec as f64 * 1e-6;
        secs(&ru.ru_utime) + secs(&ru.ru_stime)
    }
    #[cfg(windows)]
    unsafe {
        use windows_sys::Win32::Foundation::FILETIME;
        use windows_sys::Win32::System::Threading::{GetCurrentProcess, GetProcessTimes};
        let mut creation: FILETIME = std::mem::zeroed();
        let mut exit: FILETIME = std::mem::zeroed();
        let mut kernel: FILETIME = std::mem::zeroed();
        let mut user: FILETIME = std::mem::zeroed();
        if GetProcessTimes(
            GetCurrentProcess(),
            &mut creation,
            &mut exit,
            &mut kernel,
            &mut user,
        ) == 0
        {
            return 0.0;
        }
        (filetime_ticks(&kernel) + filetime_ticks(&user)) as f64 * 1e-7
    }
}

/// Current thread's CPU time.
///
/// Upstream refuses on macOS ("not implemented on this platform"); that branch
/// predates macOS 10.12, which added CLOCK_THREAD_CPUTIME_ID. Answering is
/// strictly better than throwing, so this returns a real number everywhere.
#[no_mangle]
pub extern "C" fn hlp_sys_thread_cpu_time() -> f64 {
    #[cfg(unix)]
    unsafe {
        let mut t: libc::timespec = std::mem::zeroed();
        if libc::clock_gettime(libc::CLOCK_THREAD_CPUTIME_ID, &mut t) != 0 {
            return 0.0;
        }
        t.tv_sec as f64 + t.tv_nsec as f64 * 1e-9
    }
    #[cfg(windows)]
    unsafe {
        use windows_sys::Win32::Foundation::FILETIME;
        use windows_sys::Win32::System::Threading::{GetCurrentThread, GetThreadTimes};
        let mut creation: FILETIME = std::mem::zeroed();
        let mut exit: FILETIME = std::mem::zeroed();
        let mut kernel: FILETIME = std::mem::zeroed();
        let mut user: FILETIME = std::mem::zeroed();
        if GetThreadTimes(
            GetCurrentThread(),
            &mut creation,
            &mut exit,
            &mut kernel,
            &mut user,
        ) == 0
        {
            return 0.0;
        }
        filetime_ticks(&user) as f64 * 1e-7
    }
}

#[cfg(windows)]
fn filetime_ticks(ft: &windows_sys::Win32::Foundation::FILETIME) -> u64 {
    ((ft.dwHighDateTime as u64) << 32) | ft.dwLowDateTime as u64
}

#[no_mangle]
pub unsafe extern "C" fn hlp_sys_sleep(seconds: f64) {
    let duration = std::time::Duration::from_secs_f64(seconds.max(0.0));
    // With fibers alive, park the logical thread on a scheduler timer instead
    // of repeatedly making every sleeping fiber runnable.
    if crate::fiber::fibers_active() {
        crate::fiber::sleep_until(std::time::Instant::now() + duration);
        return;
    }
    std::thread::sleep(duration);
}

/// Upstream installs an LC_TIME locale for `strftime`. ash's date formatting
/// (date.rs) goes through chrono and never consults the C locale, so a
/// successful call here changes nothing ash prints — the C locale is set for
/// the benefit of hdll natives that do call strftime.
#[no_mangle]
pub unsafe extern "C" fn hlp_sys_set_time_locale(l: *const vbyte) -> bool {
    if l.is_null() {
        return false;
    }
    let name = std::ffi::CString::new(pchar_slice(l).to_vec());
    let Ok(name) = name else {
        return false;
    };
    #[cfg(unix)]
    {
        // LC_GLOBAL_LOCALE, which libc does not name.
        let lc_global = usize::MAX as libc::locale_t;
        let lc = libc::newlocale(libc::LC_TIME_MASK, name.as_ptr(), std::ptr::null_mut());
        if lc.is_null() {
            return false;
        }
        let old = libc::uselocale(lc);
        if old.is_null() {
            libc::freelocale(lc);
            return false;
        }
        if old != lc_global {
            libc::freelocale(old);
        }
        true
    }
    #[cfg(windows)]
    {
        !libc::setlocale(libc::LC_TIME, name.as_ptr()).is_null()
    }
}

// ============================================================================
// Environment
// ============================================================================

#[no_mangle]
pub unsafe extern "C" fn hlp_sys_get_env(name: *const vbyte) -> *mut vbyte {
    let Some(key) = pchar_to_os(name) else {
        return std::ptr::null_mut();
    };
    match std::env::var_os(&key) {
        Some(v) => alloc_pbytes(&os_to_pbytes(&v)),
        None => std::ptr::null_mut(),
    }
}

/// A null value unsets, matching upstream's `unsetenv` branch.
#[no_mangle]
pub unsafe extern "C" fn hlp_sys_put_env(name: *const vbyte, value: *const vbyte) -> bool {
    if name.is_null() {
        return false;
    }
    #[cfg(unix)]
    {
        // setenv/unsetenv rather than std::env, so the C `environ` that
        // std::env::var_os and any loaded hdll both read stays the one
        // authority on this process's environment.
        let Ok(key) = std::ffi::CString::new(pchar_slice(name).to_vec()) else {
            return false;
        };
        if value.is_null() {
            return libc::unsetenv(key.as_ptr()) == 0;
        }
        let Ok(val) = std::ffi::CString::new(pchar_slice(value).to_vec()) else {
            return false;
        };
        libc::setenv(key.as_ptr(), val.as_ptr(), 1) == 0
    }
    #[cfg(windows)]
    {
        use windows_sys::Win32::System::Environment::SetEnvironmentVariableW;
        let wide = |s: &[u8]| -> Vec<u16> {
            String::from_utf8_lossy(s)
                .encode_utf16()
                .chain(std::iter::once(0))
                .collect()
        };
        let wkey = wide(pchar_slice(name));
        if value.is_null() {
            return SetEnvironmentVariableW(wkey.as_ptr(), std::ptr::null()) != 0;
        }
        let wval = wide(pchar_slice(value));
        SetEnvironmentVariableW(wkey.as_ptr(), wval.as_ptr()) != 0
    }
}

/// Flat key/value array: 2*n entries, key at 2i, value at 2i+1.
#[no_mangle]
pub unsafe extern "C" fn hlp_sys_env() -> *mut varray {
    let mut entries: Vec<Vec<u8>> = Vec::new();
    for (k, v) in std::env::vars_os() {
        entries.push(os_to_pbytes(&k));
        entries.push(os_to_pbytes(&v));
    }
    alloc_bytes_array(&entries)
}

// ============================================================================
// Process arguments and the running bytecode file
// ============================================================================

static SYS_ARGS: Mutex<Option<Vec<Vec<u8>>>> = Mutex::new(None);
static HL_FILE: Mutex<Option<Vec<u8>>> = Mutex::new(None);

/// Upstream `hl_sys_init`, for a host that wants to hand over the exact argv
/// slice and bytecode path it parsed. Nothing in ash calls it yet, so both
/// `hlp_sys_args` and `hlp_sys_hl_file` fall back to reading the real argv.
#[no_mangle]
pub unsafe extern "C" fn hlp_sys_init(args: *mut *mut vbyte, nargs: i32, hlfile: *mut vbyte) {
    // Before any native library has had a chance to start a thread of its own.
    crate::fiber::mark_main_thread();
    let mut collected = Vec::new();
    if !args.is_null() {
        for i in 0..nargs.max(0) as usize {
            let a = *args.add(i);
            if a.is_null() {
                continue;
            }
            collected.push(pchar_slice(a).to_vec());
        }
    }
    if let Ok(mut g) = SYS_ARGS.lock() {
        *g = Some(collected);
    }
    if let Ok(mut g) = HL_FILE.lock() {
        *g = if hlfile.is_null() {
            None
        } else {
            Some(pchar_slice(hlfile).to_vec())
        };
    }
}

/// Upstream returns argv from `hl_sys_init`, which the host sets to the
/// arguments *after* the .hl file. Reconstructed here by dropping everything
/// up to and including the first `.hl` argument, mirroring how the ash CLIs
/// pick the bytecode out of their own argv.
#[no_mangle]
pub unsafe extern "C" fn hlp_sys_args() -> *mut varray {
    if let Ok(g) = SYS_ARGS.lock() {
        if let Some(args) = g.as_ref() {
            return alloc_bytes_array(args);
        }
    }
    let argv: Vec<OsString> = std::env::args_os().collect();
    let start = argv
        .iter()
        .position(|a| a.to_string_lossy().to_ascii_lowercase().ends_with(".hl"))
        .map(|i| i + 1)
        .unwrap_or(1);
    let entries: Vec<Vec<u8>> = argv
        .iter()
        .skip(start.min(argv.len()))
        .map(|a| os_to_pbytes(a))
        .collect();
    alloc_bytes_array(&entries)
}

/// Null when no bytecode path is known — Sys.programPath treats that as
/// "use the executable path", which is the right answer for an embedded run.
#[no_mangle]
pub unsafe extern "C" fn hlp_sys_hl_file() -> *mut vbyte {
    if let Ok(g) = HL_FILE.lock() {
        if let Some(f) = g.as_ref() {
            return alloc_pbytes(f);
        }
    }
    // The hot-reload path registers the bytecode file for mtime watching; if
    // it ran, it knows the same thing hl_sys_init would have been told.
    if let Ok(g) = RELOAD_STATE.lock() {
        if let Some(state) = g.as_ref() {
            return alloc_pbytes(&os_to_pbytes(state.bytecode_path.as_os_str()));
        }
    }
    std::ptr::null_mut()
}

/// Upstream reads `_NSGetExecutablePath` / `GetModuleFileNameW` / `/proc`.
#[no_mangle]
pub unsafe extern "C" fn hlp_sys_exe_path() -> *mut vbyte {
    let Ok(exe) = std::env::current_exe() else {
        return std::ptr::null_mut();
    };
    alloc_pbytes(&os_to_pbytes(exe.as_os_str()))
}

// ============================================================================
// Filesystem
// ============================================================================

#[no_mangle]
pub unsafe extern "C" fn hlp_sys_exists(path: *const vbyte) -> bool {
    match pchar_to_path(path) {
        // stat(), not lstat(): a symlink to a live target exists.
        Some(p) => std::fs::metadata(p).is_ok(),
        None => false,
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_sys_is_dir(path: *const vbyte) -> bool {
    match pchar_to_path(path) {
        Some(p) => std::fs::metadata(p).map(|m| m.is_dir()).unwrap_or(false),
        None => false,
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_sys_delete(path: *const vbyte) -> bool {
    match pchar_to_path(path) {
        Some(p) => std::fs::remove_file(p).is_ok(),
        None => false,
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_sys_remove_dir(path: *const vbyte) -> bool {
    match pchar_to_path(path) {
        Some(p) => std::fs::remove_dir(p).is_ok(),
        None => false,
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_sys_rename(path: *const vbyte, newname: *const vbyte) -> bool {
    let (Some(from), Some(to)) = (pchar_to_path(path), pchar_to_path(newname)) else {
        return false;
    };
    std::fs::rename(from, to).is_ok()
}

/// `mode` is the POSIX permission word; Windows has no equivalent and
/// `_wmkdir` drops it, as this does.
#[no_mangle]
pub unsafe extern "C" fn hlp_sys_create_dir(path: *const vbyte, mode: i32) -> bool {
    let Some(p) = pchar_to_path(path) else {
        return false;
    };
    #[cfg(unix)]
    {
        use std::os::unix::fs::DirBuilderExt;
        std::fs::DirBuilder::new()
            .mode(mode as u32)
            .create(p)
            .is_ok()
    }
    #[cfg(windows)]
    {
        let _ = mode;
        std::fs::create_dir(p).is_ok()
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_sys_get_cwd() -> *mut vbyte {
    let Ok(dir) = std::env::current_dir() else {
        return std::ptr::null_mut();
    };
    let mut bytes = os_to_pbytes(dir.as_os_str());
    // Callers concatenate onto this without inserting a separator.
    if !matches!(bytes.last(), Some(b'/') | Some(b'\\')) {
        bytes.push(b'/');
    }
    alloc_pbytes(&bytes)
}

#[no_mangle]
pub unsafe extern "C" fn hlp_sys_set_cwd(dir: *const vbyte) -> bool {
    match pchar_to_path(dir) {
        Some(p) => std::env::set_current_dir(p).is_ok(),
        None => false,
    }
}

/// `realpath` / `GetFullPathNameW`: the target must exist, and null means it
/// did not resolve.
#[no_mangle]
pub unsafe extern "C" fn hlp_sys_full_path(path: *const vbyte) -> *mut vbyte {
    let Some(p) = pchar_to_path(path) else {
        return std::ptr::null_mut();
    };
    let Ok(full) = std::fs::canonicalize(&p) else {
        return std::ptr::null_mut();
    };
    #[cfg_attr(not(windows), allow(unused_mut))]
    let mut bytes = os_to_pbytes(full.as_os_str());
    #[cfg(windows)]
    {
        // canonicalize returns an extended-length path; upstream's
        // GetFullPathNameW does not, and Haxe code splits these on ':'.
        if bytes.starts_with(br"\\?\UNC\") {
            bytes.splice(0..8, br"\\".iter().copied());
        } else if bytes.starts_with(br"\\?\") {
            bytes.drain(0..4);
        }
    }
    alloc_pbytes(&bytes)
}

/// Twelve i32 slots, of which upstream fills eleven and leaves the last
/// zeroed: gid, uid, atime, mtime, ctime, size, dev, ino, nlink, rdev, mode.
/// Null when the path cannot be stat'ed — sys.FileSystem.stat turns that into
/// a SysError.
#[no_mangle]
pub unsafe extern "C" fn hlp_sys_stat(path: *const vbyte) -> *mut varray {
    let Some(p) = pchar_to_path(path) else {
        return std::ptr::null_mut();
    };
    let Ok(md) = std::fs::metadata(&p) else {
        return std::ptr::null_mut();
    };

    #[cfg(unix)]
    let fields: [i32; 11] = {
        use std::os::unix::fs::MetadataExt;
        [
            md.gid() as i32,
            md.uid() as i32,
            md.atime() as i32,
            md.mtime() as i32,
            md.ctime() as i32,
            md.size() as i32,
            md.dev() as i32,
            md.ino() as i32,
            md.nlink() as i32,
            md.rdev() as i32,
            md.mode() as i32,
        ]
    };

    #[cfg(windows)]
    let fields: [i32; 11] = {
        use std::os::windows::fs::MetadataExt;
        // _wstat32's view of a Windows file: no owner, no inode, one link,
        // times as 32-bit unix seconds, dev/rdev the 0-based drive index.
        let unix_time = |ft: u64| -> i32 {
            if ft == 0 {
                0
            } else {
                ((ft / 10_000_000) as i64 - 11_644_473_600) as i32
            }
        };
        let drive = p
            .to_string_lossy()
            .as_bytes()
            .first()
            .filter(|_| p.to_string_lossy().as_bytes().get(1) == Some(&b':'))
            .map(|c| (c.to_ascii_uppercase() - b'A') as i32)
            .unwrap_or(0);
        // The CRT's _S_* bits, then the owner triplet mirrored into group
        // and other exactly as _wstat32 does.
        let mut mode: i32 = if md.is_dir() { 0x4000 } else { 0x8000 };
        mode |= 0o400;
        // FILE_ATTRIBUTE_READONLY
        if md.file_attributes() & 0x1 == 0 {
            mode |= 0o200;
        }
        let executable = md.is_dir()
            || matches!(
                p.extension()
                    .map(|e| e.to_string_lossy().to_ascii_lowercase())
                    .as_deref(),
                Some("exe") | Some("com") | Some("bat") | Some("cmd")
            );
        if executable {
            mode |= 0o100;
        }
        mode |= (mode & 0o700) >> 3 | (mode & 0o700) >> 6;
        [
            0,
            0,
            unix_time(md.last_access_time()),
            unix_time(md.last_write_time()),
            unix_time(md.creation_time()),
            md.file_size() as i32,
            drive,
            0,
            1,
            drive,
            mode,
        ]
    };

    let a = hlp_alloc_array(hlt_i32(), 12);
    if a.is_null() {
        return std::ptr::null_mut();
    }
    let slots = hl_aptr::<i32>(a);
    for (i, v) in fields.iter().enumerate() {
        *slots.add(i) = *v;
    }
    a
}

/// Entry names only, "." and ".." excluded. Null when the directory cannot be
/// opened — sys.FileSystem.readDirectory turns that into a SysError.
#[no_mangle]
pub unsafe extern "C" fn hlp_sys_read_dir(path: *const vbyte) -> *mut varray {
    let Some(p) = pchar_to_path(path) else {
        return std::ptr::null_mut();
    };
    let Ok(dir) = std::fs::read_dir(&p) else {
        return std::ptr::null_mut();
    };
    // Marshalled up front: every alloc_pbytes below can collect, and a name
    // read after that point would come from an iterator holding an open
    // directory handle across the collection.
    let mut entries: Vec<Vec<u8>> = Vec::new();
    for e in dir.flatten() {
        entries.push(os_to_pbytes(&e.file_name()));
    }
    alloc_bytes_array(&entries)
}

// ============================================================================
// Subprocesses and console input
// ============================================================================

/// `system()`: the whole string goes to the platform shell, already quoted by
/// the Haxe side. The result packs the exit code in the low byte and the
/// terminating signal in the next, as upstream's
/// `WEXITSTATUS(status) | (WTERMSIG(status) << 8)` does. -1 means the shell
/// could not be started at all, where C's system() would have returned -1 and
/// upstream would have decoded that -1 as a status word.
#[no_mangle]
pub unsafe extern "C" fn hlp_sys_command(cmd: *const vbyte) -> i32 {
    let Some(cmdline) = pchar_to_os(cmd) else {
        return -1;
    };

    #[cfg(unix)]
    {
        let status = std::process::Command::new("/bin/sh")
            .arg("-c")
            .arg(&cmdline)
            .status();
        let Ok(status) = status else {
            return -1;
        };
        use std::os::unix::process::ExitStatusExt;
        status.code().unwrap_or(0) | (status.signal().unwrap_or(0) << 8)
    }
    #[cfg(windows)]
    {
        use std::os::windows::process::CommandExt;
        let shell = std::env::var_os("COMSPEC").unwrap_or_else(|| OsString::from("cmd.exe"));
        // raw_arg, not arg: cmd.exe does not parse its command line by the
        // MSVC rules Command::arg quotes for, so anything the Haxe side
        // quoted would be re-quoted into a different command.
        let status = std::process::Command::new(shell)
            .raw_arg("/C")
            .raw_arg(&cmdline)
            .status();
        match status {
            Ok(s) => s.code().unwrap_or(-1),
            Err(_) => -1,
        }
    }
}

#[cfg(windows)]
extern "C" {
    fn _getch() -> std::ffi::c_int;
    fn _getche() -> std::ffi::c_int;
}

/// One raw byte from stdin, -1 at end of input. `echo` writes it back out.
#[no_mangle]
pub unsafe extern "C" fn hlp_sys_get_char(echo: bool) -> i32 {
    #[cfg(unix)]
    {
        let fd = libc::STDIN_FILENO;
        let mut old: libc::termios = std::mem::zeroed();
        // Not a terminal (a pipe, say): no mode to change, just read. Upstream
        // reaches the same behaviour by ignoring the failure and letting the
        // tcsetattr calls fail too.
        let is_tty = libc::tcgetattr(fd, &mut old) == 0;
        if is_tty {
            let mut term = old;
            libc::cfmakeraw(&mut term);
            libc::tcsetattr(fd, libc::TCSANOW, &term);
        }
        let c = libc::getchar();
        if is_tty {
            libc::tcsetattr(fd, libc::TCSANOW, &old);
        }
        if echo && c >= 0 {
            let mut out = std::io::stdout().lock();
            let _ = out.write_all(&[c as u8]);
            let _ = out.flush();
        }
        c
    }
    #[cfg(windows)]
    {
        if echo {
            _getche() as i32
        } else {
            _getch() as i32
        }
    }
}

// ============================================================================
// Hot reload (ash extension, not part of upstream sys.c)
// ============================================================================

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
    let slice = unsafe { uchar_slice(path_utf16) };
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
    let check_path = match unsafe { pchar_to_path(debug_alt_file) } {
        Some(p) => p,
        None => state.bytecode_path.clone(),
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
// VM plumbing (ash extensions, not part of upstream sys.c)
// ============================================================================

// The VM event loop function, set by haxe.MainLoop via @:hlNative("std","sys_set_loop").
// After main() returns, the interpreter should call this in a loop.
static mut SYS_LOOP_FUNC: *mut std::ffi::c_void = std::ptr::null_mut();

#[no_mangle]
pub unsafe extern "C" fn hlp_sys_set_loop(func: *mut std::ffi::c_void) {
    if env_flag!("ASH_DBG_LOOP") {
        eprintln!("[ash] hlp_sys_set_loop called with {:p}", func);
    }
    SYS_LOOP_FUNC = func;
}

/// Returns the registered loop function (for the interpreter to call after main).
#[no_mangle]
pub unsafe extern "C" fn hlp_sys_get_loop() -> *mut std::ffi::c_void {
    SYS_LOOP_FUNC
}

/// Upstream returns this thread's `hl_thread_info`. ash keeps no thread
/// registry, but it does keep the one field native code reads -- see
/// `thread::ThreadInfo` for why null is not an option here.
#[no_mangle]
pub unsafe extern "C" fn hlp_get_thread_info() -> *mut c_void {
    crate::thread::thread_info() as *mut c_void
}

/// Whether a debugger is attached.
///
/// Not in HashLink 1.15 — the bytecode a 4.3.6 Haxe emits asks for it anyway,
/// and an unresolved native is a hard failure at startup rather than at the
/// call. ash has no debugger protocol, so the honest answer is false; see
/// std/src/debugger.rs for what does exist.
#[no_mangle]
pub unsafe extern "C" fn hlp_sys_has_debugger() -> bool {
    false
}

/// Resident memory of this process, in bytes.
///
/// Upstream answers 0.0 everywhere but Windows and the consoles, and a
/// caller cannot tell that apart from a process holding nothing. Each
/// platform here reports what its own accounting calls the working set:
/// `pti_resident_size` on Darwin, the resident column of /proc/self/statm on
/// Linux. That is pages the kernel has actually backed, not the GC heap,
/// which is the figure a profiler weighs against the machine's RAM.
#[no_mangle]
pub extern "C" fn hlp_sys_process_memory() -> f64 {
    #[cfg(any(target_os = "macos", target_os = "ios"))]
    unsafe {
        let mut ti: libc::proc_taskinfo = std::mem::zeroed();
        let want = std::mem::size_of::<libc::proc_taskinfo>() as i32;
        let got = libc::proc_pidinfo(
            libc::getpid(),
            libc::PROC_PIDTASKINFO,
            0,
            &mut ti as *mut _ as *mut c_void,
            want,
        );
        if got != want {
            return 0.0;
        }
        ti.pti_resident_size as f64
    }
    #[cfg(target_os = "linux")]
    {
        // Field 2 of statm is the resident set in pages. Reading the file is
        // a syscall pair, not a /proc directory walk, so it is cheap enough
        // to call from a per-frame profiler.
        let Ok(statm) = std::fs::read_to_string("/proc/self/statm") else {
            return 0.0;
        };
        let Some(pages) = statm.split_whitespace().nth(1) else {
            return 0.0;
        };
        let Ok(pages) = pages.parse::<f64>() else {
            return 0.0;
        };
        let page = unsafe { libc::sysconf(libc::_SC_PAGESIZE) };
        if page <= 0 {
            return 0.0;
        }
        pages * page as f64
    }
    #[cfg(windows)]
    unsafe {
        use windows_sys::Win32::System::ProcessStatus::{
            GetProcessMemoryInfo, PROCESS_MEMORY_COUNTERS,
        };
        use windows_sys::Win32::System::Threading::GetCurrentProcess;
        let mut inf: PROCESS_MEMORY_COUNTERS = std::mem::zeroed();
        let size = std::mem::size_of::<PROCESS_MEMORY_COUNTERS>() as u32;
        if GetProcessMemoryInfo(GetCurrentProcess(), &mut inf, size) == 0 {
            return 0.0;
        }
        inf.WorkingSetSize as f64
    }
    #[cfg(not(any(target_os = "macos", target_os = "ios", target_os = "linux", windows)))]
    {
        // No portable accounting call on the remaining targets; upstream
        // returns 0.0 here too.
        0.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Marshal a Rust string the way the VM does: NUL-terminated pchar.
    fn pc(s: &str) -> Vec<u8> {
        let mut v = s.as_bytes().to_vec();
        v.push(0);
        v
    }

    /// Read back a pchar the prim handed us.
    unsafe fn unpc(p: *const vbyte) -> Option<String> {
        if p.is_null() {
            return None;
        }
        Some(String::from_utf8_lossy(pchar_slice(p)).into_owned())
    }

    unsafe fn array_strings(a: *mut varray) -> Vec<String> {
        assert!(!a.is_null());
        let slots = hl_aptr::<*mut vbyte>(a);
        (0..(*a).size as usize)
            .map(|i| unpc(*slots.add(i)).unwrap())
            .collect()
    }

    /// One test, not several: these all allocate from the process-wide GC,
    /// and the harness runs separate #[test] functions on separate threads.
    #[test]
    fn sys_prims_round_trip() {
        unsafe {
            crate::gc::hlp_gc_init();

            let cwd = unpc(hlp_sys_get_cwd()).expect("cwd");
            assert!(cwd.ends_with('/') || cwd.ends_with('\\'), "cwd={cwd}");
            assert!(std::path::Path::new(&cwd).is_dir());

            let key = pc("ASH_SYS_RS_TEST_VAR");
            assert!(hlp_sys_get_env(key.as_ptr()).is_null());
            assert!(hlp_sys_put_env(key.as_ptr(), pc("hello").as_ptr()));
            assert_eq!(
                unpc(hlp_sys_get_env(key.as_ptr())).as_deref(),
                Some("hello")
            );
            let env = array_strings(hlp_sys_env());
            assert!(env.len().is_multiple_of(2) && !env.is_empty());
            assert!(env.chunks(2).any(|kv| kv[0] == "ASH_SYS_RS_TEST_VAR"));
            assert!(hlp_sys_put_env(key.as_ptr(), std::ptr::null()));
            assert!(hlp_sys_get_env(key.as_ptr()).is_null());

            let root = std::env::temp_dir().join(format!("ash_sys_rs_{}", std::process::id()));
            let _ = std::fs::remove_dir_all(&root);
            let root_p = pc(root.to_str().unwrap());
            assert!(hlp_sys_create_dir(root_p.as_ptr(), 0o755));
            assert!(hlp_sys_exists(root_p.as_ptr()));
            assert!(hlp_sys_is_dir(root_p.as_ptr()));

            let file = root.join("a.txt");
            std::fs::write(&file, b"0123456789").unwrap();
            let file_p = pc(file.to_str().unwrap());
            assert!(hlp_sys_exists(file_p.as_ptr()));
            assert!(!hlp_sys_is_dir(file_p.as_ptr()));
            assert_eq!(array_strings(hlp_sys_read_dir(root_p.as_ptr())), ["a.txt"]);

            let st = hlp_sys_stat(file_p.as_ptr());
            assert!(!st.is_null());
            assert_eq!((*st).size, 12);
            let f = hl_aptr::<i32>(st);
            assert_eq!(*f.add(5), 10, "st_size");
            assert!(*f.add(3) > 1_600_000_000, "st_mtime");
            assert_eq!(*f.add(10) & 0xF000, 0x8000, "S_IFREG");
            assert_eq!(*f.add(11), 0, "12th slot stays zero");
            assert_eq!(
                hlp_sys_stat(pc(root.join("nope").to_str().unwrap()).as_ptr()),
                std::ptr::null_mut()
            );

            let full = unpc(hlp_sys_full_path(file_p.as_ptr())).expect("full_path");
            assert!(full.ends_with("a.txt"));
            assert!(hlp_sys_full_path(pc("/no/such/path/at/all").as_ptr()).is_null());

            let moved = root.join("b.txt");
            let moved_p = pc(moved.to_str().unwrap());
            assert!(hlp_sys_rename(file_p.as_ptr(), moved_p.as_ptr()));
            assert!(!hlp_sys_exists(file_p.as_ptr()));
            assert!(hlp_sys_delete(moved_p.as_ptr()));
            assert!(hlp_sys_remove_dir(root_p.as_ptr()));
            assert!(!hlp_sys_exists(root_p.as_ptr()));

            let name = hlp_sys_string();
            assert!(!name.is_null());
            assert!(!String::from_utf16_lossy(uchar_slice(name as *const u16)).is_empty());

            assert!(hlp_sys_getpid() > 0);
            assert!(hlp_sys_cpu_time() > 0.0);
            assert!(hlp_sys_thread_cpu_time() > 0.0);
            assert_eq!(hlp_sys_command(pc("exit 3").as_ptr()), 3);

            assert_eq!(hlp_sys_set_flags(0), 0);
            hlp_sys_set_flags(PR_AUTO_FLUSH);
        }
    }
}

#[cfg(test)]
mod process_memory_tests {
    use super::*;

    const MIB: f64 = 1024.0 * 1024.0;

    /// A plausible resident size, asserted as a range rather than a value:
    /// the exact figure is the kernel's and moves between runs. Upstream
    /// answers 0.0 on this platform, and 0.0 is also what a caller sees when
    /// the accounting call fails, so the floor is the interesting half --
    /// a test process that has loaded LLVM cannot be resident in under a
    /// megabyte.
    #[test]
    fn it_reports_a_plausible_resident_size() {
        let rss = hlp_sys_process_memory();
        assert!(rss.is_finite(), "rss={rss}");
        assert!(rss > 0.0, "reported nothing resident; the query failed");
        assert!(rss > MIB, "rss={rss} is below a megabyte");
        assert!(rss < 64.0 * 1024.0 * MIB, "rss={rss} exceeds 64GiB");
        // A byte count, not a page count: a resident set under a megabyte
        // would mean the Linux arm forgot to multiply by the page size.
        assert_eq!(rss, rss.trunc(), "rss={rss} is not a whole number of bytes");
    }

    /// Residency, not the GC heap and not a constant. Touching every page of
    /// a fresh mapping is what makes the kernel back it, so the figure has to
    /// move; a stub returning a fixed number would pass the range check above
    /// and fail here.
    #[test]
    fn it_tracks_pages_the_kernel_has_actually_backed() {
        let before = hlp_sys_process_memory();
        assert!(before > 0.0);

        // 64MiB, written a page at a time. Kept alive across the second
        // reading, or the allocator may hand it straight back.
        let mut block = vec![0u8; 64 * 1024 * 1024];
        for i in (0..block.len()).step_by(4096) {
            block[i] = 1;
        }
        let after = hlp_sys_process_memory();
        assert!(
            after - before > 8.0 * MIB,
            "touching 64MiB moved the resident set by {} bytes",
            after - before
        );
        assert_eq!(block[0], 1);
        drop(block);
    }

    /// DEFINE_PRIM(_F64, sys_process_memory, _NO_ARG) -- no arguments, and a
    /// double rather than an int, because a resident set outgrows i32 on any
    /// machine that matters.
    #[test]
    fn the_exported_signature_is_the_one_upstream_declares() {
        let f: extern "C" fn() -> f64 = hlp_sys_process_memory;
        assert!(f() > 0.0);
    }
}
