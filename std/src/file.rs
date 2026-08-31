//! Buffered file I/O, the `std@hlp_file_*` natives.
//!
//! Upstream is `hashlink/src/std/file.c`, where a handle is a C `FILE*` and
//! the buffering belongs to the C library. `std::fs` buffers nothing, so the
//! staging below is part of the contract rather than a speed-up:
//! `sys.io.FileInput.readByte` is one `file_read_char` per byte, and every
//! `haxe.io.Input` helper built on it (`readLine`, `readUntil`) walks a file
//! that way.
//!
//! Handle lifetime differs from upstream in one respect. `hl_file_open`
//! allocates through `hl_gc_alloc_finalizer`, so a handle dropped without
//! `file_close` still has its `FILE*` closed when the GC reaps it. ash has no
//! finalizer hook, so the OS handle and the Rust-side buffers behind an
//! abandoned descriptor stay alive until the process exits. Every path in
//! Haxe's `sys.io` closes explicitly, so this only bites code that leaks a
//! `FileInput`/`FileOutput` on purpose.

use std::ffi::{c_int, c_void};
use std::fs::{File, OpenOptions};
use std::io::{self, Read, Seek, SeekFrom, Write};
use std::ptr;
use std::sync::{Mutex, MutexGuard};

use crate::bytes::hlp_alloc_bytes;
use crate::hl::vbyte;

/// Matches the C library's default block size closely enough that a
/// per-byte `file_read_char` loop costs one `read(2)` per 64K, not per byte.
const BUF_SIZE: usize = 64 * 1024;

/// Tag stored in every handle. `hl.Abstract<"hl_fdesc">` is an untyped
/// pointer on the VM side, so a slot that was never opened, was zeroed, or
/// holds an unrelated abstract arrives here indistinguishable from a live
/// descriptor; upstream tolerates that by being a GC object with a NULL
/// `FILE*`, and this is the equivalent guard.
const FDESC_MAGIC: u64 = 0x4153_485f_4644_5343;

/// The VM-visible handle: GC memory, like upstream's `hl_fdesc`, holding a
/// pointer to Rust-owned state that `file_close` releases.
#[repr(C)]
struct Fdesc {
    magic: u64,
    state: *mut Mutex<FileState>,
}

enum Backing {
    File(File),
    Stdin,
    Stdout,
    Stderr,
}

struct FileState {
    backing: Backing,
    /// Read-ahead staging. Only one of `rbuf`/`wbuf` ever holds data: C
    /// stdio requires a seek or a flush between switching direction on a
    /// read-write stream, and the direction switches below enforce it.
    rbuf: Vec<u8>,
    rpos: usize,
    rend: usize,
    wbuf: Vec<u8>,
    /// `feof` is set by a read that reached the end, never by arriving at
    /// it; `file_eof` reports this flag and not the current offset.
    eof: bool,
}

/// `File::read` surfaces `EINTR` to the caller where `fread` retries.
fn read_retry<R: Read>(r: &mut R, buf: &mut [u8]) -> io::Result<usize> {
    loop {
        match r.read(buf) {
            Err(e) if e.kind() == io::ErrorKind::Interrupted => continue,
            other => return other,
        }
    }
}

/// A poisoned handle stays usable: every mutation here is a single field
/// write, so a panic elsewhere cannot leave the buffers half-updated.
fn lock(m: &Mutex<FileState>) -> MutexGuard<'_, FileState> {
    m.lock().unwrap_or_else(|e| e.into_inner())
}

impl FileState {
    fn new(backing: Backing) -> Self {
        FileState {
            backing,
            rbuf: Vec::new(),
            rpos: 0,
            rend: 0,
            wbuf: Vec::new(),
            eof: false,
        }
    }

    fn read_ahead(&self) -> usize {
        self.rend - self.rpos
    }

    fn flush_writes(&mut self) -> io::Result<()> {
        if self.wbuf.is_empty() {
            return Ok(());
        }
        let res = if let Backing::File(f) = &mut self.backing {
            f.write_all(&self.wbuf)
        } else {
            Ok(())
        };
        self.wbuf.clear();
        res
    }

    /// Hand back bytes pulled in ahead of the caller's position, so the OS
    /// offset agrees with the offset `file_tell` last reported.
    fn rewind_read_ahead(&mut self) -> io::Result<()> {
        let back = self.read_ahead();
        self.rpos = 0;
        self.rend = 0;
        if back == 0 {
            return Ok(());
        }
        if let Backing::File(f) = &mut self.backing {
            f.seek(SeekFrom::Current(-(back as i64)))?;
        }
        Ok(())
    }

    fn read(&mut self, out: &mut [u8]) -> io::Result<usize> {
        if out.is_empty() {
            return Ok(0);
        }
        match self.backing {
            Backing::Stdin => {
                // Already buffered by std, and not seekable, so it bypasses
                // the staging below entirely.
                let n = read_retry(&mut io::stdin(), out)?;
                if n == 0 {
                    self.eof = true;
                }
                Ok(n)
            }
            // fread on a write-only stream sets the error flag and returns 0.
            Backing::Stdout | Backing::Stderr => Err(io::ErrorKind::InvalidInput.into()),
            Backing::File(_) => self.read_file(out),
        }
    }

    fn read_file(&mut self, out: &mut [u8]) -> io::Result<usize> {
        self.flush_writes()?;
        if self.rbuf.is_empty() {
            self.rbuf.resize(BUF_SIZE, 0);
        }
        let Backing::File(f) = &mut self.backing else {
            return Ok(0);
        };
        let mut total = 0usize;
        while total < out.len() {
            if self.rpos == self.rend {
                // Reads at or past the staging size would only be copied
                // twice, so they go straight to the caller's buffer.
                if out.len() - total >= BUF_SIZE {
                    let n = read_retry(f, &mut out[total..])?;
                    if n == 0 {
                        self.eof = true;
                        break;
                    }
                    total += n;
                    continue;
                }
                let n = read_retry(f, &mut self.rbuf)?;
                self.rpos = 0;
                self.rend = n;
                if n == 0 {
                    self.eof = true;
                    break;
                }
            }
            let n = (self.rend - self.rpos).min(out.len() - total);
            out[total..total + n].copy_from_slice(&self.rbuf[self.rpos..self.rpos + n]);
            self.rpos += n;
            total += n;
        }
        Ok(total)
    }

    fn write(&mut self, data: &[u8]) -> io::Result<usize> {
        if data.is_empty() {
            return Ok(0);
        }
        match self.backing {
            // Routed through std's own handles rather than fd 1/2 so writes
            // interleave with hlp_sys_print in the order they were issued,
            // and so a Windows console still gets the UTF-8 re-encoding that
            // upstream open-codes with _O_U8TEXT.
            Backing::Stdout => {
                io::stdout().lock().write_all(data)?;
                Ok(data.len())
            }
            Backing::Stderr => {
                io::stderr().lock().write_all(data)?;
                Ok(data.len())
            }
            Backing::Stdin => Err(io::ErrorKind::InvalidInput.into()),
            Backing::File(_) => self.write_file(data),
        }
    }

    fn write_file(&mut self, data: &[u8]) -> io::Result<usize> {
        self.rewind_read_ahead()?;
        self.eof = false;
        if self.wbuf.len() + data.len() > BUF_SIZE {
            self.flush_writes()?;
        }
        if data.len() >= BUF_SIZE {
            if let Backing::File(f) = &mut self.backing {
                f.write_all(data)?;
            }
        } else {
            self.wbuf.extend_from_slice(data);
        }
        Ok(data.len())
    }

    fn seek(&mut self, pos: i64, kind: c_int) -> bool {
        if !matches!(self.backing, Backing::File(_)) {
            return false;
        }
        if self.flush_writes().is_err() {
            return false;
        }
        let ahead = self.read_ahead() as i64;
        self.rpos = 0;
        self.rend = 0;
        let target = match kind {
            0 => {
                if pos < 0 {
                    return false;
                }
                SeekFrom::Start(pos as u64)
            }
            // SEEK_CUR is relative to the position the caller believes it is
            // at, which is behind the OS offset by the unconsumed read-ahead.
            1 => SeekFrom::Current(pos - ahead),
            2 => SeekFrom::End(pos),
            _ => return false,
        };
        let Backing::File(f) = &mut self.backing else {
            return false;
        };
        if f.seek(target).is_err() {
            return false;
        }
        self.eof = false;
        true
    }

    fn tell(&mut self) -> Option<i64> {
        let ahead = self.read_ahead() as i64;
        let pending = self.wbuf.len() as i64;
        let Backing::File(f) = &mut self.backing else {
            return None;
        };
        let cur = f.stream_position().ok()? as i64;
        Some(cur - ahead + pending)
    }

    fn flush(&mut self) -> bool {
        if self.flush_writes().is_err() {
            return false;
        }
        match &mut self.backing {
            Backing::File(f) => f.flush().is_ok(),
            Backing::Stdout => io::stdout().flush().is_ok(),
            Backing::Stderr => io::stderr().flush().is_ok(),
            Backing::Stdin => true,
        }
    }
}

impl Drop for FileState {
    fn drop(&mut self) {
        let _ = self.flush_writes();
    }
}

/// Paths cross this boundary in whatever encoding `hlp_sys_utf8_path()`
/// advertises, and ash answers `true` on every target. `Sys.getPath`
/// therefore hands over NUL-terminated UTF-8 even on Windows, where upstream
/// would have sent UTF-16 and gone through `_wfopen`.
unsafe fn path_from_bytes(name: *const vbyte) -> Option<String> {
    if name.is_null() {
        return None;
    }
    let mut len = 0usize;
    while *name.add(len) != 0 {
        len += 1;
    }
    let raw = std::slice::from_raw_parts(name, len);
    Some(String::from_utf8_lossy(raw).into_owned())
}

unsafe fn alloc_handle(state: FileState) -> *mut c_void {
    let d = hlp_alloc_bytes(std::mem::size_of::<Fdesc>() as c_int) as *mut Fdesc;
    if d.is_null() {
        return ptr::null_mut();
    }
    (*d).magic = FDESC_MAGIC;
    (*d).state = Box::into_raw(Box::new(Mutex::new(state)));
    d as *mut c_void
}

/// Lifetime is bounded by `file_close`, which the VM only reaches through a
/// handle it is no longer using.
unsafe fn state_of<'a>(f: *mut c_void) -> Option<&'a Mutex<FileState>> {
    if f.is_null() {
        return None;
    }
    let d = f as *const Fdesc;
    if (*d).magic != FDESC_MAGIC || (*d).state.is_null() {
        return None;
    }
    Some(&*(*d).state)
}

// DEFINE_PRIM(_FILE, file_open, _BYTES _I32 _BOOL)
/// Run a blocking file operation with the collector told to expect it.
///
/// A thread inside a read is not going to reach a safepoint until the kernel
/// returns, and the collector would otherwise wait for it: one measured world
/// stop spent 352ms waiting on a fiber worker. `gc_set_blocking` publishes the
/// thread's stack pointer and callee-saved registers first, so the stack stays
/// conservatively scannable for the whole call — the same contract the socket
/// primitives already use.
#[inline]
fn blocking_io<T>(f: impl FnOnce() -> T) -> T {
    // SAFETY: the primitive only touches this thread's mutator record.
    unsafe { crate::thread::hlp_blocking(true) };
    let out = f();
    unsafe { crate::thread::hlp_blocking(false) };
    out
}

#[no_mangle]
pub unsafe extern "C" fn hlp_file_open(
    name: *const vbyte,
    mode: c_int,
    binary: bool,
) -> *mut c_void {
    // Upstream indexes MODES[mode|(binary?4:0)] over
    // { "r", "w", "a", "r+", "rb", "wb", "ab", "rb+" }; the `b` half differs
    // only on Windows, where it suppresses a CRLF translation std::fs never
    // performs in either direction.
    let _ = binary;
    let Some(path) = path_from_bytes(name) else {
        return ptr::null_mut();
    };
    let mut opts = OpenOptions::new();
    match mode {
        0 => opts.read(true),
        1 => opts.write(true).create(true).truncate(true),
        2 => opts.append(true).create(true),
        3 => opts.read(true).write(true),
        // Out of range indexes MODES past its end upstream; there is no
        // behaviour there to be compatible with.
        _ => return ptr::null_mut(),
    };
    let Ok(file) = opts.open(&path) else {
        return ptr::null_mut();
    };
    alloc_handle(FileState::new(Backing::File(file)))
}

// DEFINE_PRIM(_VOID, file_close, _FILE)
#[no_mangle]
pub unsafe extern "C" fn hlp_file_close(f: *mut c_void) {
    if f.is_null() {
        return;
    }
    let d = f as *mut Fdesc;
    if (*d).magic != FDESC_MAGIC || (*d).state.is_null() {
        return;
    }
    // Cleared before the drop, so a second close - or a call that outlives
    // the Haxe wrapper nulling its field - finds a spent handle rather than
    // a freed box. Upstream gets the same property from NULLing `f->f`.
    let state = (*d).state;
    (*d).state = ptr::null_mut();
    drop(Box::from_raw(state));
}

// DEFINE_PRIM(_I32, file_write, _FILE _BYTES _I32 _I32)
#[no_mangle]
pub unsafe extern "C" fn hlp_file_write(
    f: *mut c_void,
    buf: *mut vbyte,
    pos: c_int,
    len: c_int,
) -> c_int {
    let Some(m) = state_of(f) else {
        return -1;
    };
    if buf.is_null() || pos < 0 || len <= 0 {
        return 0;
    }
    let data = std::slice::from_raw_parts(buf.add(pos as usize), len as usize);
    // fwrite reports the count that made it; a partial write behind the
    // staging buffer cannot be attributed, so a failure reports none, which
    // is the short count Haxe already turns into an Eof.
    match blocking_io(|| lock(m).write(data)) {
        Ok(n) => n as c_int,
        Err(_) => 0,
    }
}

// DEFINE_PRIM(_I32, file_read, _FILE _BYTES _I32 _I32)
#[no_mangle]
pub unsafe extern "C" fn hlp_file_read(
    f: *mut c_void,
    buf: *mut vbyte,
    pos: c_int,
    len: c_int,
) -> c_int {
    let Some(m) = state_of(f) else {
        return -1;
    };
    if buf.is_null() || pos < 0 || len <= 0 {
        return 0;
    }
    let out = std::slice::from_raw_parts_mut(buf.add(pos as usize), len as usize);
    match blocking_io(|| lock(m).read(out)) {
        Ok(n) => n as c_int,
        Err(_) => 0,
    }
}

// DEFINE_PRIM(_BOOL, file_write_char, _FILE _I32)
#[no_mangle]
pub unsafe extern "C" fn hlp_file_write_char(f: *mut c_void, c: c_int) -> bool {
    let Some(m) = state_of(f) else {
        return false;
    };
    let byte = [c as u8];
    matches!(blocking_io(|| lock(m).write(&byte)), Ok(1))
}

// DEFINE_PRIM(_I32, file_read_char, _FILE)
#[no_mangle]
pub unsafe extern "C" fn hlp_file_read_char(f: *mut c_void) -> c_int {
    let Some(m) = state_of(f) else {
        return -2;
    };
    let mut byte = [0u8; 1];
    match blocking_io(|| lock(m).read(&mut byte)) {
        Ok(1) => byte[0] as c_int,
        _ => -2,
    }
}

// DEFINE_PRIM(_BOOL, file_seek, _FILE _I32 _I32)
#[no_mangle]
pub unsafe extern "C" fn hlp_file_seek(f: *mut c_void, pos: c_int, kind: c_int) -> bool {
    let Some(m) = state_of(f) else {
        return false;
    };
    lock(m).seek(pos as i64, kind)
}

// DEFINE_PRIM(_I32, file_tell, _FILE)
#[no_mangle]
pub unsafe extern "C" fn hlp_file_tell(f: *mut c_void) -> c_int {
    let Some(m) = state_of(f) else {
        return -1;
    };
    // Upstream truncates ftell to int and hands back a wrong offset past
    // 2GB; Haxe reads any negative as "tell() failure", so report that
    // instead of a plausible-looking wrap.
    match lock(m).tell() {
        Some(p) if p >= 0 && p <= c_int::MAX as i64 => p as c_int,
        _ => -1,
    }
}

// DEFINE_PRIM(_BOOL, file_seek2, _FILE _F64 _I32)
#[no_mangle]
pub unsafe extern "C" fn hlp_file_seek2(f: *mut c_void, pos: f64, kind: c_int) -> bool {
    let Some(m) = state_of(f) else {
        return false;
    };
    if !pos.is_finite() {
        return false;
    }
    lock(m).seek(pos as i64, kind)
}

// DEFINE_PRIM(_F64, file_tell2, _FILE)
#[no_mangle]
pub unsafe extern "C" fn hlp_file_tell2(f: *mut c_void) -> f64 {
    let Some(m) = state_of(f) else {
        return -1.0;
    };
    match lock(m).tell() {
        Some(p) => p as f64,
        None => -1.0,
    }
}

// DEFINE_PRIM(_BOOL, file_eof, _FILE)
#[no_mangle]
pub unsafe extern "C" fn hlp_file_eof(f: *mut c_void) -> bool {
    match state_of(f) {
        Some(m) => lock(m).eof,
        None => true,
    }
}

// DEFINE_PRIM(_BOOL, file_flush, _FILE)
#[no_mangle]
pub unsafe extern "C" fn hlp_file_flush(f: *mut c_void) -> bool {
    let Some(m) = state_of(f) else {
        return false;
    };
    lock(m).flush()
}

// DEFINE_PRIM(_FILE, file_stdin, _NO_ARG)
#[no_mangle]
pub unsafe extern "C" fn hlp_file_stdin() -> *mut c_void {
    alloc_handle(FileState::new(Backing::Stdin))
}

// DEFINE_PRIM(_FILE, file_stdout, _NO_ARG)
#[no_mangle]
pub unsafe extern "C" fn hlp_file_stdout() -> *mut c_void {
    alloc_handle(FileState::new(Backing::Stdout))
}

// DEFINE_PRIM(_FILE, file_stderr, _NO_ARG)
#[no_mangle]
pub unsafe extern "C" fn hlp_file_stderr() -> *mut c_void {
    alloc_handle(FileState::new(Backing::Stderr))
}

// DEFINE_PRIM(_BYTES, file_contents, _BYTES _REF(_I32))
#[no_mangle]
pub unsafe extern "C" fn hlp_file_contents(name: *const vbyte, size: *mut c_int) -> *mut vbyte {
    let Some(path) = path_from_bytes(name) else {
        return ptr::null_mut();
    };
    let Ok(mut f) = File::open(&path) else {
        return ptr::null_mut();
    };
    // Sized by fseek/ftell rather than by stat, matching upstream: a stream
    // whose metadata length lies (procfs, sysfs) reads as empty here exactly
    // as it does there.
    let Ok(end) = f.seek(SeekFrom::End(0)) else {
        return ptr::null_mut();
    };
    if end > c_int::MAX as u64 {
        return ptr::null_mut();
    }
    let len = end as usize;
    if f.rewind().is_err() {
        return ptr::null_mut();
    }
    if !size.is_null() {
        *size = len as c_int;
    }
    // Without an out-param the caller is String.fromUTF8, which needs the
    // trailing 0 upstream appends here.
    let alloc = if size.is_null() { len + 1 } else { len.max(1) };
    let content = hlp_alloc_bytes(alloc as c_int);
    if content.is_null() {
        return ptr::null_mut();
    }
    if size.is_null() {
        *content.add(len) = 0;
    }
    let out = std::slice::from_raw_parts_mut(content, len);
    let mut p = 0usize;
    while p < len {
        match f.read(&mut out[p..]) {
            Ok(0) => return ptr::null_mut(),
            Ok(d) => p += d,
            Err(e) if e.kind() == io::ErrorKind::Interrupted => {}
            Err(_) => return ptr::null_mut(),
        }
    }
    content
}

/// Upstream asks Windows for the file with `dwShareMode` 0 and
/// `OPEN_EXISTING`, so a file another process holds and a file that is not
/// there both answer "locked". `OpenOptions::share_mode` is that same
/// `CreateFileW` call.
#[cfg(windows)]
fn file_locked(path: &str) -> bool {
    use std::os::windows::fs::OpenOptionsExt;
    OpenOptions::new()
        .read(true)
        .share_mode(0)
        .open(path)
        .is_err()
}

/// Upstream has no POSIX branch here: mandatory locking is not a thing to
/// query, and advisory locks are not what this asks about.
#[cfg(not(windows))]
fn file_locked(_path: &str) -> bool {
    false
}

// DEFINE_PRIM(_BOOL, file_is_locked, _BYTES)
#[no_mangle]
pub unsafe extern "C" fn hlp_file_is_locked(name: *const vbyte) -> bool {
    match path_from_bytes(name) {
        Some(path) => file_locked(&path),
        None => cfg!(windows),
    }
}

// DEFINE_PRIM(_I32, file_error_code, _NO_ARG)
//
// Upstream returns the raw `errno` left behind by the last failing stdio
// call, and `std::fs` reaches the same syscalls through the same libc, so
// this is the same number for the same reason. What it is not is a latch:
// errno belongs to the thread, not to the handle, and the staging above can
// interpose a call between a failed read and this query. Haxe only asks
// after a primitive already reported failure, which is where the two agree.
#[no_mangle]
pub unsafe extern "C" fn hlp_file_error_code() -> c_int {
    io::Error::last_os_error().raw_os_error().unwrap_or(0)
}

#[cfg(test)]
mod file_error_code_tests {
    use super::*;

    /// Fail a syscall with a known errno and read it straight back, with no
    /// intervening call that could set errno itself. libc rather than
    /// `std::fs`, because std may allocate or retry on the way out and the
    /// point here is what the *last* failing call left behind.
    unsafe fn errno_after_failed_open(path: &str) -> c_int {
        let mut p = path.as_bytes().to_vec();
        p.push(0);
        let fd = libc::open(p.as_ptr() as *const libc::c_char, libc::O_RDONLY);
        assert_eq!(fd, -1, "{path} unexpectedly opened");
        hlp_file_error_code()
    }

    fn missing_path(tag: &str) -> String {
        let p = std::env::temp_dir().join(format!(
            "ash_file_error_{tag}_{}",
            std::process::id()
        ));
        let _ = std::fs::remove_file(&p);
        p.to_str().unwrap().to_string()
    }

    /// The documented contract: the raw errno the last failing call left, the
    /// same number upstream reads out of the C library.
    #[test]
    fn it_reports_the_errno_of_the_last_failing_call() {
        unsafe {
            assert_eq!(errno_after_failed_open(&missing_path("enoent")), libc::ENOENT);

            // A different failure gives a different number, so the value is
            // being read rather than returned as a constant. Opening a
            // directory for writing fails on every platform ash targets.
            let dir = std::env::temp_dir();
            let mut p = dir.to_str().unwrap().as_bytes().to_vec();
            p.push(0);
            let fd = libc::open(p.as_ptr() as *const libc::c_char, libc::O_WRONLY);
            if fd == -1 {
                let code = hlp_file_error_code();
                assert_ne!(code, libc::ENOENT, "the temp directory does not exist");
                assert_ne!(code, 0, "a failed open left errno clear");
            } else {
                libc::close(fd);
            }

            // Closing a descriptor that was never open is EBADF everywhere.
            assert_eq!(libc::close(-1), -1);
            assert_eq!(hlp_file_error_code(), libc::EBADF);
        }
    }

    /// The comment is explicit that this is not a latch: errno belongs to the
    /// thread, so a later failure overwrites an earlier one and the value is
    /// only meaningful straight after a primitive reported failure. Both
    /// halves are asserted, so the honest limitation is recorded rather than
    /// quietly assumed away.
    #[test]
    fn it_is_a_thread_errno_read_not_a_per_handle_latch() {
        unsafe {
            assert_eq!(libc::close(-1), -1);
            assert_eq!(hlp_file_error_code(), libc::EBADF);

            // A second, different failure replaces it -- nothing is held on
            // behalf of the first, and no handle is consulted.
            assert_eq!(errno_after_failed_open(&missing_path("latch")), libc::ENOENT);

            // Reading does not consume the value either.
            assert_eq!(hlp_file_error_code(), libc::ENOENT);
            assert_eq!(hlp_file_error_code(), libc::ENOENT);
        }
    }

    /// errno is per-thread, which is what lets the query take no handle
    /// argument. An implementation that cached the last error in a process
    /// global would pass every test above and fail this one: the child would
    /// open onto the parent's EBADF.
    ///
    /// The child checks before it has failed at anything, because that is the
    /// only moment a leaked global would be visible. It asserts the parent's
    /// value is absent rather than that errno is exactly zero -- a thread's
    /// startup path is entitled to leave something behind, and the bug this
    /// is aimed at leaves EBADF specifically.
    #[test]
    fn the_code_belongs_to_the_calling_thread() {
        unsafe {
            assert_eq!(libc::close(-1), -1);
            assert_eq!(hlp_file_error_code(), libc::EBADF);

            let child = std::thread::spawn(|| {
                let leaked = hlp_file_error_code();
                let own = errno_after_failed_open(&missing_path("child"));
                (leaked, own)
            })
            .join()
            .unwrap();

            assert_ne!(
                child.0,
                libc::EBADF,
                "the parent thread's error was visible on a fresh thread"
            );
            assert_eq!(child.1, libc::ENOENT);
        }
    }

    /// DEFINE_PRIM(_I32, file_error_code, _NO_ARG).
    #[test]
    fn the_exported_signature_is_the_one_upstream_declares() {
        let f: unsafe extern "C" fn() -> c_int = hlp_file_error_code;
        unsafe {
            let _ = f();
        }
    }
}
