//! Child processes, the `std@hlp_process_*` natives.
//!
//! Upstream is `hashlink/src/std/process.c`, which open-codes
//! `pipe`/`fork`/`execvp` on unix and `CreatePipe`/`CreateProcess` on
//! Windows. `std::process` performs the same sequence, so the handle here
//! wraps a `Child` plus the three pipe ends rather than raw descriptors --
//! that is what buys the Windows half without a second hand-written spawner.
//!
//! Two structural consequences of that choice are load-bearing:
//!
//! * The stdio handles are moved out of the `Child` at spawn time.
//!   `Child::wait` closes whatever stdin it still owns before waiting;
//!   upstream's `waitpid` closes nothing, and a Haxe caller blocking in
//!   `exitCode()` while the child still reads stdin has to see the same
//!   stall rather than a silent EOF.
//! * `Command::spawn` reports a failed `execvp` to the *parent*, where
//!   upstream only discovers it inside the already-forked child -- which
//!   prints `Command not found : <cmd>` and exits 1, leaving the handle it
//!   had already returned perfectly valid. `ChildSlot::ExecFailed` below
//!   reconstructs that child so `new Process(...)` keeps succeeding for a
//!   command that does not exist, as it does upstream.
//!
//! Handle lifetime differs from upstream the same way `file.rs` does.
//! `hl_process_run` allocates through `hl_gc_alloc_finalizer`, so an
//! abandoned handle still has its pipes closed when the GC reaps it. ash has
//! no finalizer hook, so a `Process` that is never `close()`d holds its pipes
//! and its unreaped child until exit. Upstream leaks the zombie in that case
//! too (it forks without ever waiting); only the three descriptors are extra.

use std::ffi::{c_int, c_void, OsString};
use std::io::{self, Read, Write};
use std::process::{Child, ChildStderr, ChildStdin, ChildStdout, Command, ExitStatus, Stdio};
use std::ptr;
use std::sync::{Mutex, MutexGuard};

use crate::bytes::hlp_alloc_bytes;
use crate::hl::{hl_type_kind_HBYTES, varray, vbyte};
use crate::types::hl_aptr;

/// Tag stored in every handle. `hl.Abstract<"hl_process">` is an untyped
/// pointer on the VM side, so a slot that was never run, was zeroed, or holds
/// an unrelated abstract arrives here indistinguishable from a live process;
/// upstream tolerates that by being a GC object whose descriptors are 0, and
/// this is the equivalent guard.
const PROC_MAGIC: u64 = 0x4153_485f_5052_4f43;

/// The VM-visible handle: GC memory, like upstream's `vprocess`, holding a
/// pointer to Rust-owned state.
#[repr(C)]
struct VProcess {
    magic: u64,
    state: *mut ProcState,
}

/// One lock per pipe rather than one per process. `process_exit` with a NULL
/// `running` blocks until the child is gone, and the idiomatic Haxe shape is
/// a second thread draining stdout meanwhile; a single lock would stall that
/// drain, the child would block on a full pipe, and neither side would move.
struct ProcState {
    child: Mutex<ChildSlot>,
    stdin: Mutex<Option<ChildStdin>>,
    stdout: Mutex<Option<ChildStdout>>,
    stderr: Mutex<StderrSrc>,
}

enum ChildSlot {
    Live(Child),
    /// The fork happened, the exec did not. See the module header.
    ExecFailed,
}

enum StderrSrc {
    Pipe(ChildStderr),
    /// What upstream's doomed child writes to fd 2 before `exit(1)`.
    Message(io::Cursor<Vec<u8>>),
    Closed,
}

/// A poisoned handle stays usable: every mutation here is a single field
/// write, so a panic elsewhere cannot leave the state half-updated.
fn lock<T>(m: &Mutex<T>) -> MutexGuard<'_, T> {
    m.lock().unwrap_or_else(|e| e.into_inner())
}

/// Bytes of a NUL-terminated `vbyte*`, excluding the terminator.
unsafe fn pchar_slice<'a>(p: *const vbyte) -> &'a [u8] {
    let mut len = 0usize;
    while *p.add(len) != 0 {
        len += 1;
    }
    std::slice::from_raw_parts(p, len)
}

/// Commands and arguments cross this boundary in whatever encoding
/// `hlp_sys_utf8_path()` advertises, and ash answers `true` on every target.
/// `Sys.getPath` therefore hands over NUL-terminated UTF-8 even on Windows,
/// where upstream would have sent UTF-16 straight to `CreateProcessW`.
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
    // WASI paths are UTF-8, so the lossy conversion loses nothing the host
    // would have accepted. Nothing on this target spawns anything anyway.
    #[cfg(not(any(unix, windows)))]
    {
        Some(OsString::from(String::from_utf8_lossy(bytes).into_owned()))
    }
}

unsafe fn state_of<'a>(p: *mut c_void) -> Option<&'a ProcState> {
    if p.is_null() {
        return None;
    }
    let h = p as *const VProcess;
    if (*h).magic != PROC_MAGIC || (*h).state.is_null() {
        return None;
    }
    Some(&*(*h).state)
}

/// `read` reaching end-of-file is an error to this API, not a short count:
/// upstream returns -1 for any `nbytes <= 0`, and `haxe.io.Input` turns that
/// into the `Eof` its callers loop on.
///
/// EINTR is retried, where upstream's bare `read` would surface it as that
/// same Eof. A signal arriving mid-read is not end of stream, and ash
/// installs handlers (GC, crash recovery) upstream does not.
fn read_pipe<R: Read>(r: &mut R, out: &mut [u8]) -> c_int {
    loop {
        match r.read(out) {
            Ok(0) => return -1,
            Ok(n) => return n as c_int,
            Err(e) if e.kind() == io::ErrorKind::Interrupted => continue,
            Err(_) => return -1,
        }
    }
}

/// True when `spawn` failed at the exec rather than at the fork or the pipes.
/// Upstream reaches `execvp` only inside the child, so an exec failure still
/// leaves a valid handle behind; a `pipe` or `fork` failure is what makes
/// `hl_process_run` answer NULL. These four errnos are the ones `fork` and
/// `pipe` raise -- everything else came from the exec.
/// Nothing spawns here, so nothing can fail during spawn setup.
#[cfg(not(any(unix, windows)))]
fn spawn_setup_failed(_e: &io::Error) -> bool {
    true
}

#[cfg(unix)]
fn spawn_setup_failed(e: &io::Error) -> bool {
    matches!(
        e.raw_os_error(),
        Some(libc::EAGAIN) | Some(libc::ENOMEM) | Some(libc::EMFILE) | Some(libc::ENFILE)
    )
}

/// `CreateProcess` reports every failure to the caller, and upstream answers
/// all of them with NULL.
#[cfg(windows)]
fn spawn_setup_failed(_e: &io::Error) -> bool {
    true
}

/// `CreateProcess` with a NULL application name takes the executable off the
/// front of the command line; `Command` wants it separately. This splits that
/// first token for the quoted shape `sys.io.Process` always builds on
/// Windows. Unlike `CreateProcess`, an *unquoted* first token containing
/// spaces is not retried against successive prefixes -- nothing in the Haxe
/// std produces one.
#[cfg(windows)]
fn split_command_line(cmd: &str) -> Option<(String, String)> {
    let s = cmd.trim_start();
    if s.is_empty() {
        return None;
    }
    if let Some(rest) = s.strip_prefix('"') {
        let end = rest.find('"')?;
        Some((
            rest[..end].to_string(),
            rest[end + 1..].trim_start().to_string(),
        ))
    } else {
        match s.find(' ') {
            Some(i) => Some((s[..i].to_string(), s[i + 1..].trim_start().to_string())),
            None => Some((s.to_string(), String::new())),
        }
    }
}

/// No child, so no status. -1 is what the natives report when there is no
/// process to ask.
#[cfg(not(any(unix, windows)))]
fn status_code(_s: ExitStatus) -> c_int {
    -1
}

#[cfg(unix)]
fn status_code(s: ExitStatus) -> c_int {
    use std::os::unix::process::ExitStatusExt;
    if let Some(c) = s.code() {
        c
    } else if let Some(sig) = s.signal() {
        // WIFSIGNALED: upstream tags the signal number with bit 30 so a
        // Haxe caller can tell it from an ordinary small exit code.
        0x4000_0000 | sig
    } else {
        -2
    }
}

/// `GetExitCodeProcess`'s DWORD, reinterpreted as the int upstream returns.
#[cfg(windows)]
fn status_code(s: ExitStatus) -> c_int {
    s.code().unwrap_or(-1)
}

// DEFINE_PRIM(_PROCESS, process_run, _BYTES _ARR _BOOL)
#[no_mangle]
pub unsafe extern "C" fn hlp_process_run(
    cmd: *mut vbyte,
    vargs: *mut varray,
    detached: bool,
) -> *mut c_void {
    let Some(cmdline) = pchar_to_os(cmd) else {
        return ptr::null_mut();
    };
    let raw_cmd = pchar_slice(cmd).to_vec();

    let mut args: Option<Vec<OsString>> = None;
    if !vargs.is_null() {
        let at = (*vargs).at;
        if at.is_null() || (*at).kind != hl_type_kind_HBYTES {
            return ptr::null_mut();
        }
        let n = (*vargs).size;
        if n < 0 {
            return ptr::null_mut();
        }
        let slots = hl_aptr::<*mut vbyte>(vargs);
        let mut v: Vec<OsString> = Vec::with_capacity(n as usize);
        for i in 0..n as usize {
            // A NULL entry lands in upstream's argv, where execvp reads it as
            // the terminator and drops everything after it.
            match pchar_to_os(*slots.add(i)) {
                Some(s) => v.push(s),
                None => break,
            }
        }
        args = Some(v);
    }

    // Only the Windows half of hl_process_run reads `detached`; the unix half
    // never mentions it and pipes and forks either way.
    let detached = detached && cfg!(windows);

    #[cfg(unix)]
    let mut command = match &args {
        // No argv means the command is a shell line, exactly as upstream's
        // hand-built { "/bin/sh", "-c", cmd, NULL }.
        None => {
            let mut c = Command::new("/bin/sh");
            c.arg("-c").arg(&cmdline);
            c
        }
        // With argv, cmd is both the program and argv[0], which is what
        // execvp(argv[0], argv) does upstream.
        Some(a) => {
            let mut c = Command::new(&cmdline);
            c.args(a);
            c
        }
    };

    // A sandbox has no subprocesses. The natives still exist and still link;
    // this one reports the failure upstream reports when a spawn is refused,
    // and the read/write prims below already answer -1 with no child.
    #[cfg(not(any(unix, windows)))]
    let mut command = {
        let _ = (&args, detached);
        Command::new("")
    };

    #[cfg(windows)]
    let mut command = {
        if args.is_some() {
            // Upstream: "should have been pre-processed by toplevel".
            // sys.io.Process folds argv into the command line on Windows.
            return ptr::null_mut();
        }
        let line = cmdline.to_string_lossy().into_owned();
        let Some((program, rest)) = split_command_line(&line) else {
            return ptr::null_mut();
        };
        let mut c = Command::new(program);
        if !rest.is_empty() {
            // raw_arg, not arg: the line was already quoted by the Haxe side
            // for CreateProcess, and Command::arg would quote it again.
            use std::os::windows::process::CommandExt;
            c.raw_arg(rest);
        }
        c
    };

    if detached {
        // Upstream gives a detached child no inherited handles at all and a
        // console of its own. Stdio::null is the nearest std offers; the
        // difference is the child's std handles being NUL rather than its
        // new console. Either way this process holds no pipe to it, so the
        // read/write prims below report -1 as upstream's NULL handles do.
        command
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null());
        #[cfg(windows)]
        {
            use std::os::windows::process::CommandExt;
            command.creation_flags(windows_sys::Win32::System::Threading::CREATE_NEW_CONSOLE);
        }
    } else {
        command
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());
    }

    let (slot, sin, sout, serr) = match command.spawn() {
        Ok(mut c) => {
            // Taken out of the Child so Child::wait has no stdin left to
            // close. See the module header.
            let sin = c.stdin.take();
            let sout = c.stdout.take();
            let serr = c.stderr.take().map_or(StderrSrc::Closed, StderrSrc::Pipe);
            (ChildSlot::Live(c), sin, sout, serr)
        }
        Err(e) => {
            if spawn_setup_failed(&e) {
                return ptr::null_mut();
            }
            let mut msg = b"Command not found : ".to_vec();
            msg.extend_from_slice(&raw_cmd);
            msg.push(b'\n');
            (
                ChildSlot::ExecFailed,
                None,
                None,
                StderrSrc::Message(io::Cursor::new(msg)),
            )
        }
    };

    let state = Box::into_raw(Box::new(ProcState {
        child: Mutex::new(slot),
        stdin: Mutex::new(sin),
        stdout: Mutex::new(sout),
        stderr: Mutex::new(serr),
    }));
    let h = hlp_alloc_bytes(std::mem::size_of::<VProcess>() as c_int) as *mut VProcess;
    if h.is_null() {
        drop(Box::from_raw(state));
        return ptr::null_mut();
    }
    (*h).magic = PROC_MAGIC;
    (*h).state = state;
    h as *mut c_void
}

// DEFINE_PRIM(_I32, process_stdout_read, _PROCESS _BYTES _I32 _I32)
#[no_mangle]
pub unsafe extern "C" fn hlp_process_stdout_read(
    p: *mut c_void,
    str: *mut vbyte,
    pos: c_int,
    len: c_int,
) -> c_int {
    let Some(st) = state_of(p) else {
        return -1;
    };
    if str.is_null() || pos < 0 || len <= 0 {
        return -1;
    }
    let out = std::slice::from_raw_parts_mut(str.add(pos as usize), len as usize);
    let mut g = lock(&st.stdout);
    match g.as_mut() {
        Some(r) => read_pipe(r, out),
        None => -1,
    }
}

// DEFINE_PRIM(_I32, process_stderr_read, _PROCESS _BYTES _I32 _I32)
#[no_mangle]
pub unsafe extern "C" fn hlp_process_stderr_read(
    p: *mut c_void,
    str: *mut vbyte,
    pos: c_int,
    len: c_int,
) -> c_int {
    let Some(st) = state_of(p) else {
        return -1;
    };
    if str.is_null() || pos < 0 || len <= 0 {
        return -1;
    }
    let out = std::slice::from_raw_parts_mut(str.add(pos as usize), len as usize);
    let mut g = lock(&st.stderr);
    match &mut *g {
        StderrSrc::Pipe(r) => read_pipe(r, out),
        StderrSrc::Message(c) => read_pipe(c, out),
        StderrSrc::Closed => -1,
    }
}

// DEFINE_PRIM(_I32, process_stdin_write, _PROCESS _BYTES _I32 _I32)
#[no_mangle]
pub unsafe extern "C" fn hlp_process_stdin_write(
    p: *mut c_void,
    str: *mut vbyte,
    pos: c_int,
    len: c_int,
) -> c_int {
    let Some(st) = state_of(p) else {
        return -1;
    };
    if str.is_null() || pos < 0 || len < 0 {
        return -1;
    }
    let mut g = lock(&st.stdin);
    // A closed or absent write end is upstream's fd -1, where write() fails
    // with EBADF whatever the length.
    let Some(w) = g.as_mut() else {
        return -1;
    };
    if len == 0 {
        return 0;
    }
    let data = std::slice::from_raw_parts(str.add(pos as usize), len as usize);
    loop {
        match w.write(data) {
            Ok(n) => return n as c_int,
            Err(e) if e.kind() == io::ErrorKind::Interrupted => continue,
            Err(_) => return -1,
        }
    }
}

// DEFINE_PRIM(_BOOL, process_stdin_close, _PROCESS)
#[no_mangle]
pub unsafe extern "C" fn hlp_process_stdin_close(p: *mut c_void) -> bool {
    let Some(st) = state_of(p) else {
        return false;
    };
    // Upstream reports the return of close(), so a second call - which lands
    // on the fd -1 it left behind - answers false.
    lock(&st.stdin).take().is_some()
}

// DEFINE_PRIM(_I32, process_exit, _PROCESS _REF(_BOOL))
#[no_mangle]
pub unsafe extern "C" fn hlp_process_exit(p: *mut c_void, running: *mut bool) -> c_int {
    if !running.is_null() {
        *running = false;
    }
    let Some(st) = state_of(p) else {
        return -1;
    };
    let mut slot = lock(&st.child);
    let ChildSlot::Live(child) = &mut *slot else {
        // Upstream's doomed child called exit(1) before this could be asked.
        return 1;
    };
    if running.is_null() {
        match child.wait() {
            Ok(s) => status_code(s),
            Err(_) => -1,
        }
    } else {
        match child.try_wait() {
            Ok(Some(s)) => status_code(s),
            Ok(None) => {
                *running = true;
                0
            }
            // waitpid failing while `running` was asked for reports 0 with
            // *running left false, not -1.
            Err(_) => 0,
        }
    }
}

// DEFINE_PRIM(_I32, process_pid, _PROCESS)
#[no_mangle]
pub unsafe extern "C" fn hlp_process_pid(p: *mut c_void) -> c_int {
    let Some(st) = state_of(p) else {
        return -1;
    };
    match &*lock(&st.child) {
        ChildSlot::Live(c) => c.id() as c_int,
        // Upstream would report the pid of the fork that failed to exec.
        // There is no fork here to name, and inventing one would be worse
        // than the -1 a caller can at least test.
        ChildSlot::ExecFailed => -1,
    }
}

// DEFINE_PRIM(_VOID, process_close, _PROCESS)
#[no_mangle]
pub unsafe extern "C" fn hlp_process_close(p: *mut c_void) {
    let Some(st) = state_of(p) else {
        return;
    };
    // Upstream's finalizer body: the three pipe ends, nothing else. Idempotent
    // because a second call finds them already taken. Windows additionally
    // closes hProcess/hThread here, which makes a later process_exit fail
    // there but not on unix; ash keeps the child handle so both behave like
    // the unix side.
    drop(lock(&st.stdin).take());
    drop(lock(&st.stdout).take());
    drop(std::mem::replace(&mut *lock(&st.stderr), StderrSrc::Closed));
}

// DEFINE_PRIM(_VOID, process_kill, _PROCESS)
#[no_mangle]
pub unsafe extern "C" fn hlp_process_kill(p: *mut c_void) {
    let Some(st) = state_of(p) else {
        return;
    };
    let slot = lock(&st.child);
    let ChildSlot::Live(child) = &*slot else {
        return;
    };
    #[cfg(unix)]
    {
        // libc::kill, not Child::kill: the latter refuses once the child has
        // been reaped, where upstream's kill() just returns ESRCH.
        libc::kill(child.id() as libc::pid_t, libc::SIGKILL);
    }
    #[cfg(windows)]
    {
        use std::os::windows::io::AsRawHandle;
        // 0xCDCDCDCD, not Child::kill's 1: it is the exit code a later
        // process_exit reports, and upstream picked this one.
        windows_sys::Win32::System::Threading::TerminateProcess(
            child.as_raw_handle() as _,
            0xCDCD_CDCD,
        );
    }
}
