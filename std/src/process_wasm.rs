//! `sys.io.Process` on a target that cannot fork.
//!
//! The native module next door is built on `std::process::Child` and live
//! pipes. A wasm module has neither, so this provides the same nine
//! primitives by asking the host, the way [`crate::sys::hlp_sys_command`]
//! does -- and, like it, the host refuses unless it has been told to allow
//! it, because spawning is the one thing here that leaves the sandbox.
//!
//! # It runs to completion, and that is visible
//!
//! Upstream's `Process` is a live child: you may write its input while
//! reading its output, and a program that streams both is normal. Nothing
//! here can do that, because a host function call is not concurrent with the
//! guest that made it -- the guest is stopped for its duration.
//!
//! So the child is not started at `run`. Its command is recorded, `stdin`
//! writes accumulate, and the first read of its output or its exit code runs
//! it once, to completion, with everything written so far as its input. That
//! is exactly right for the shape almost every program uses -- start
//! something, feed it, wait, read what it said -- and wrong for one that
//! expects to interleave. A program that needs interleaving needs a host that
//! can suspend the guest mid-call, which is the fiber transform's business
//! and not this module's.
//!
//! The one other visible difference: a command that cannot be started is
//! reported when it is run rather than when it is created, so the failure
//! arrives at `exitCode` instead of at `new Process`.

use std::ffi::c_void;
use std::os::raw::c_int;

use crate::hl::{varray, vbyte};
use crate::types::hl_aptr;

// The host side of a spawn. It refuses unless the operator allowed it, and
// answers exactly as a native `Process` would once it has.
#[link(wasm_import_module = "env")]
extern "C" {
    /// Run `argv` -- NUL-separated, command first -- to completion with
    /// `stdin` as its input, and keep what it said. With `shell` set there
    /// are no separators and the whole of `argv` is one line for the
    /// platform's shell. Returns a handle, or -1 if it could not be started
    /// or the host does not allow this.
    fn ash_host_process_start(
        argv: *const u8,
        argv_len: i32,
        stdin: *const u8,
        stdin_len: i32,
        shell: i32,
    ) -> i32;
    /// Bytes of `which` -- 0 for stdout, 1 for stderr -- still unread.
    fn ash_host_process_len(handle: i32, which: i32) -> i32;
    /// Copy up to `len` unread bytes of `which` into the guest.
    fn ash_host_process_read(handle: i32, which: i32, into: *mut u8, len: i32) -> i32;
    /// What it exited with.
    fn ash_host_process_code(handle: i32) -> i32;
    /// Let the host forget it.
    fn ash_host_process_free(handle: i32);
}

/// Stamped into the handle. `hl.Abstract` is an untyped pointer on the VM
/// side, so a slot that was never run, was closed, or holds something else
/// arrives here indistinguishable from a live one.
const MAGIC: u64 = 0x4153_485f_5057_5350;

struct Proc {
    magic: u64,
    /// Command and arguments, NUL-separated, as the host wants them.
    argv: Vec<u8>,
    /// What has been written to its input but not yet handed over.
    stdin: Vec<u8>,
    /// The host's handle once it has run, or -1 if it could not be started.
    started: Option<i32>,
    /// Whether `argv` is a shell line rather than a command and arguments.
    shell: bool,
}

unsafe fn proc_of<'a>(p: *mut c_void) -> Option<&'a mut Proc> {
    let q = p as *mut Proc;
    if q.is_null() || (*q).magic != MAGIC {
        return None;
    }
    Some(&mut *q)
}

/// Run it, if it has not run already.
///
/// Every accessor goes through here, so the child starts at whichever of them
/// the caller reaches first and never more than once.
unsafe fn ensure_started(p: &mut Proc) -> i32 {
    if let Some(h) = p.started {
        return h;
    }
    let h = ash_host_process_start(
        p.argv.as_ptr(),
        p.argv.len() as i32,
        p.stdin.as_ptr(),
        p.stdin.len() as i32,
        p.shell as i32,
    );
    p.started = Some(h);
    h
}

unsafe fn pchar_bytes(p: *const vbyte) -> Vec<u8> {
    if p.is_null() {
        return Vec::new();
    }
    let mut n = 0usize;
    while *p.add(n) != 0 {
        n += 1;
    }
    std::slice::from_raw_parts(p, n).to_vec()
}

// DEFINE_PRIM(_PROCESS, process_run, _BYTES _ARR _BOOL)
#[no_mangle]
pub unsafe extern "C" fn hlp_process_run(
    cmd: *mut vbyte,
    vargs: *mut varray,
    _detached: bool,
) -> *mut c_void {
    if cmd.is_null() {
        return std::ptr::null_mut();
    }
    let mut argv = pchar_bytes(cmd);
    if argv.is_empty() {
        return std::ptr::null_mut();
    }
    // No argument array at all means the command is a shell line, which is
    // what `Process` is documented to do and what the native side does.
    let shell = vargs.is_null();
    if !vargs.is_null() {
        let n = (*vargs).size;
        if n > 0 {
            let slots = hl_aptr::<*mut vbyte>(vargs);
            for i in 0..n as usize {
                let a = *slots.add(i);
                // A NULL entry is upstream's argv terminator: everything
                // after it is dropped.
                if a.is_null() {
                    break;
                }
                argv.push(0);
                argv.extend_from_slice(&pchar_bytes(a));
            }
        }
    }
    Box::into_raw(Box::new(Proc {
        magic: MAGIC,
        argv,
        stdin: Vec::new(),
        started: None,
        shell,
    })) as *mut c_void
}

unsafe fn read_into(p: *mut c_void, which: i32, str: *mut vbyte, pos: c_int, len: c_int) -> c_int {
    let Some(proc) = proc_of(p) else {
        return -1;
    };
    if str.is_null() || pos < 0 || len <= 0 {
        return -1;
    }
    let h = ensure_started(proc);
    if h < 0 {
        return -1;
    }
    // Upstream reports end of stream as -1, which is what `haxe.io.Input`
    // turns into an Eof.
    if ash_host_process_len(h, which) <= 0 {
        return -1;
    }
    let got = ash_host_process_read(h, which, str.add(pos as usize), len);
    if got <= 0 {
        -1
    } else {
        got
    }
}

// DEFINE_PRIM(_I32, process_stdout_read, _PROCESS _BYTES _I32 _I32)
#[no_mangle]
pub unsafe extern "C" fn hlp_process_stdout_read(
    p: *mut c_void,
    str: *mut vbyte,
    pos: c_int,
    len: c_int,
) -> c_int {
    read_into(p, 0, str, pos, len)
}

// DEFINE_PRIM(_I32, process_stderr_read, _PROCESS _BYTES _I32 _I32)
#[no_mangle]
pub unsafe extern "C" fn hlp_process_stderr_read(
    p: *mut c_void,
    str: *mut vbyte,
    pos: c_int,
    len: c_int,
) -> c_int {
    read_into(p, 1, str, pos, len)
}

// DEFINE_PRIM(_I32, process_stdin_write, _PROCESS _BYTES _I32 _I32)
#[no_mangle]
pub unsafe extern "C" fn hlp_process_stdin_write(
    p: *mut c_void,
    str: *mut vbyte,
    pos: c_int,
    len: c_int,
) -> c_int {
    let Some(proc) = proc_of(p) else {
        return -1;
    };
    if str.is_null() || pos < 0 || len <= 0 {
        return -1;
    }
    // Once it has run, its input is closed: there is nobody left to read.
    if proc.started.is_some() {
        return -1;
    }
    let src = std::slice::from_raw_parts(str.add(pos as usize), len as usize);
    proc.stdin.extend_from_slice(src);
    len
}

// DEFINE_PRIM(_BOOL, process_stdin_close, _PROCESS)
#[no_mangle]
pub unsafe extern "C" fn hlp_process_stdin_close(p: *mut c_void) -> bool {
    let Some(proc) = proc_of(p) else {
        return false;
    };
    // Closing input is the usual signal that the child may now finish, so it
    // is as good a moment as any to run it.
    ensure_started(proc) >= 0
}

// DEFINE_PRIM(_I32, process_exit, _PROCESS _REF(_BOOL))
#[no_mangle]
pub unsafe extern "C" fn hlp_process_exit(p: *mut c_void, running: *mut bool) -> c_int {
    let Some(proc) = proc_of(p) else {
        if !running.is_null() {
            *running = false;
        }
        return -1;
    };
    let h = ensure_started(proc);
    // It ran to completion inside `ensure_started`, so it is never still
    // going by the time anyone can ask.
    if !running.is_null() {
        *running = false;
    }
    if h < 0 {
        return -1;
    }
    ash_host_process_code(h)
}

// DEFINE_PRIM(_I32, process_pid, _PROCESS)
#[no_mangle]
pub unsafe extern "C" fn hlp_process_pid(p: *mut c_void) -> c_int {
    // The child is gone by the time anything could use its id, and inventing
    // one would be worse than saying so.
    let _ = p;
    -1
}

// DEFINE_PRIM(_VOID, process_close, _PROCESS)
#[no_mangle]
pub unsafe extern "C" fn hlp_process_close(p: *mut c_void) {
    let Some(proc) = proc_of(p) else {
        return;
    };
    if let Some(h) = proc.started {
        if h >= 0 {
            ash_host_process_free(h);
        }
    }
    proc.magic = 0;
    drop(Box::from_raw(p as *mut Proc));
}

// DEFINE_PRIM(_VOID, process_kill, _PROCESS)
#[no_mangle]
pub unsafe extern "C" fn hlp_process_kill(p: *mut c_void) {
    // Nothing to signal: it has either not started or already finished.
    let _ = p;
}
