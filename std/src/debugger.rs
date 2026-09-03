use std::sync::atomic::{AtomicBool, Ordering};

static DEBUGGER_PRESENT: AtomicBool = AtomicBool::new(false);

#[no_mangle]
#[inline(never)]
#[cold]
pub extern "C" fn hlp_breakpoint() {
    hl_debug_break();
}

pub fn hl_debug_break() {
    if hl_detect_debugger() {
        unsafe {
            #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
            {
                #[cfg(target_os = "windows")]
                {
                    core::arch::asm!("int3");
                }
                #[cfg(target_os = "macos")]
                {
                    core::arch::asm!("int3");
                }
                #[cfg(all(not(target_os = "windows"), not(target_os = "macos")))]
                {
                    core::arch::asm!(
                        "int3",
                        ".pushsection embed-breakpoints, \"aw\", @progbits",
                        ".quad .",
                        ".popsection"
                    );
                }
            }
            #[cfg(target_arch = "aarch64")]
            {
                #[cfg(target_os = "macos")]
                {
                    // On macOS ARM64, we'll just use the brk instruction without the extra section
                    core::arch::asm!("brk #0");
                }
                #[cfg(not(target_os = "macos"))]
                {
                    core::arch::asm!(
                        "brk #0",
                        ".pushsection .debug_gdb_scripts, \"MS\",@progbits,1",
                        ".byte 1",
                        ".asciz \"breakpoint {{ . }}\"",
                        ".popsection"
                    );
                }
            }
        }
    }
}

/// Whether a debugger is attached.
///
/// Exported under its HashLink name as well: ui.hdll imports
/// `hl_detect_debugger`, and on Windows every import has to resolve before the
/// library will map at all.
#[no_mangle]
pub extern "C" fn hl_detect_debugger() -> bool {
    if DEBUGGER_PRESENT.load(Ordering::Relaxed) {
        return true;
    }

    // The macOS arm (sysctl) needs the unsafe block; the Linux arm does not.
    #[allow(unused_unsafe)]
    let debugger_present = unsafe {
        #[cfg(target_os = "windows")]
        {
            use winapi::um::debugapi::IsDebuggerPresent;
            IsDebuggerPresent() != 0
        }
        #[cfg(target_os = "linux")]
        {
            use std::fs::File;
            use std::io::Read;
            let mut status = String::new();
            File::open("/proc/self/status")
                .and_then(|mut f| f.read_to_string(&mut status))
                .map(|_| !status.contains("TracerPid:\t0"))
                .unwrap_or(false)
        }
        #[cfg(target_os = "macos")]
        {
            use libc::{ptrace, PT_DENY_ATTACH};

            // Try to deny debugger attachment
            let result = ptrace(PT_DENY_ATTACH, 0, std::ptr::null_mut(), 0);

            // If ptrace returns -1, it means a debugger is already attached
            result == -1
        }
        #[cfg(not(any(target_os = "windows", target_os = "linux", target_os = "macos")))]
        {
            false // Unsupported OS, assume no debugger
        }
    };

    DEBUGGER_PRESENT.store(debugger_present, Ordering::Relaxed);
    debugger_present
}

// ---------------------------------------------------------------------------
// Unsupported tooling primitives.
//
// ash does NOT implement the HashLink debugger-attach protocol (ptrace /
// mach_vm / WaitForDebugEvent process control), the allocation-tracking
// protocol (`HL_TRACK_*` bucket sampling), or VTune JIT profiling. Nothing
// below inspects, controls or samples anything.
//
// They exist only so a program that *references* them LOADS instead of dying
// at startup with "natives resolved, N missing". Every one is a stub, and each
// stub answers the way HashLink itself answers on a build where the feature is
// compiled out:
//
//   * queries report emptiness — `false`, `0`, `NULL`, never invented data;
//   * actions report failure where the signature has room for it, so a
//     debugger or profiler learns immediately that it is unsupported instead
//     of attaching to a session that will never produce an event.
//
// A caller should expect `debug_start` to refuse, `debug_wait` to report the
// session gone, register reads to be null, and `track_count` to report zero
// live buckets. Signatures are taken from HashLink's `DEFINE_PRIM` lines
// (`std/debug.c`, `std/track.c`, `gc.c`, `std/sys.c`) — the bytecode-visible
// types — not from the C function prototypes, which differ for `debug_stop`
// (declared `bool` in C, `_VOID` in its `DEFINE_PRIM`).
//
// Set `ASH_TRACE_DEBUGGER=1` for a single one-line warning the first time any
// of them is reached. These are called in loops; do not log per call.
// ---------------------------------------------------------------------------

use crate::hl::{hl_type, varray, vbyte, vdynamic};

/// `ASH_TRACE_DEBUGGER=1`: warn once when unsupported tooling is reached.
fn debugger_trace_enabled() -> bool {
    static V: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *V.get_or_init(|| std::env::var("ASH_TRACE_DEBUGGER").is_ok())
}

/// One line per process, not per call: `debug_wait` and `track_entry` are
/// polled in tight loops by the tools that call them.
fn note_unsupported(what: &str) {
    if !debugger_trace_enabled() {
        return;
    }
    static ONCE: std::sync::Once = std::sync::Once::new();
    ONCE.call_once(|| {
        eprintln!("[ash] {what}: ash does not implement this protocol; reporting unavailable");
    });
}

// --- debugger attach API ---------------------------------------------------
// std/debug.c. `pid` is the *target* process; ash never attaches to one.

/// `DEFINE_PRIM(_BOOL, debug_start, _I32)` — refuse to attach.
#[no_mangle]
pub unsafe extern "C" fn hlp_debug_start(_pid: i32) -> bool {
    note_unsupported("debug_start");
    false
}

/// `DEFINE_PRIM(_VOID, debug_stop, _I32)` — no session to detach from.
#[no_mangle]
pub unsafe extern "C" fn hlp_debug_stop(_pid: i32) {
    note_unsupported("debug_stop");
}

/// `DEFINE_PRIM(_BOOL, debug_breakpoint, _I32)` — could not interrupt.
#[no_mangle]
pub unsafe extern "C" fn hlp_debug_breakpoint(_pid: i32) -> bool {
    note_unsupported("debug_breakpoint");
    false
}

/// `DEFINE_PRIM(_BOOL, debug_read, _I32 _BYTES _BYTES _I32)` — read nothing.
///
/// `buffer` is deliberately left untouched: a caller that ignores the `false`
/// and reads it anyway sees its own uninitialised bytes rather than zeros we
/// invented and passed off as the target's memory.
#[no_mangle]
pub unsafe extern "C" fn hlp_debug_read(
    _pid: i32,
    _addr: *mut vbyte,
    _buffer: *mut vbyte,
    _size: i32,
) -> bool {
    note_unsupported("debug_read");
    false
}

/// `DEFINE_PRIM(_BOOL, debug_write, _I32 _BYTES _BYTES _I32)` — wrote nothing.
#[no_mangle]
pub unsafe extern "C" fn hlp_debug_write(
    _pid: i32,
    _addr: *mut vbyte,
    _buffer: *mut vbyte,
    _size: i32,
) -> bool {
    note_unsupported("debug_write");
    false
}

/// `DEFINE_PRIM(_BOOL, debug_flush, _I32 _BYTES _I32)` — no i-cache to flush.
#[no_mangle]
pub unsafe extern "C" fn hlp_debug_flush(_pid: i32, _addr: *mut vbyte, _size: i32) -> bool {
    note_unsupported("debug_flush");
    false
}

/// `DEFINE_PRIM(_I32, debug_wait, _I32 _REF(_I32) _I32)` — no session.
///
/// Returns `0`, which HashLink's own no-debug-support build returns and which
/// the protocol reads as "target exited". That ends a debugger's event loop
/// instead of spinning it forever on a session that never existed.
#[no_mangle]
pub unsafe extern "C" fn hlp_debug_wait(_pid: i32, thread: *mut i32, _timeout: i32) -> i32 {
    note_unsupported("debug_wait");
    if !thread.is_null() {
        *thread = 0;
    }
    0
}

/// `DEFINE_PRIM(_BOOL, debug_resume, _I32 _I32)` — nothing was ever stopped.
#[no_mangle]
pub unsafe extern "C" fn hlp_debug_resume(_pid: i32, _thread: i32) -> bool {
    note_unsupported("debug_resume");
    false
}

/// `DEFINE_PRIM(_BYTES, debug_read_register, _I32 _I32 _I32 _BOOL)` — no value.
#[no_mangle]
pub unsafe extern "C" fn hlp_debug_read_register(
    _pid: i32,
    _thread: i32,
    _reg: i32,
    _is64: bool,
) -> *mut vbyte {
    note_unsupported("debug_read_register");
    std::ptr::null_mut()
}

/// `DEFINE_PRIM(_BOOL, debug_write_register, _I32 _I32 _I32 _BYTES _BOOL)`.
#[no_mangle]
pub unsafe extern "C" fn hlp_debug_write_register(
    _pid: i32,
    _thread: i32,
    _reg: i32,
    _value: *mut vbyte,
    _is64: bool,
) -> bool {
    note_unsupported("debug_write_register");
    false
}

/// `DEFINE_PRIM(_DYN, debug_call, _I32 _DYN)` — the debugger's in-process
/// callback trampoline. HashLink's own body is `return NULL;`; so is ours.
#[no_mangle]
pub unsafe extern "C" fn hlp_debug_call(_mode: i32, _v: *mut vdynamic) -> *mut vdynamic {
    note_unsupported("debug_call");
    std::ptr::null_mut()
}

// --- allocation tracking ---------------------------------------------------
// std/track.c. ash's GC records no `HL_TRACK_*` buckets, so every query
// reports an empty table and every setter is dropped.

/// `DEFINE_PRIM(_I32, track_count, _REF(_I32))` — zero buckets, zero depth.
#[no_mangle]
pub unsafe extern "C" fn hlp_track_count(depth: *mut i32) -> i32 {
    note_unsupported("track_count");
    if !depth.is_null() {
        *depth = 0;
    }
    0
}

/// `DEFINE_PRIM(_I32, track_entry, _I32 _REF(_TYPE) _REF(_I32) _REF(_I32) _ARR)`
///
/// `-1` is HashLink's "id out of range" answer, and upstream returns it before
/// touching any out-parameter — so neither do we. With `track_count` at 0
/// every id is out of range.
#[no_mangle]
pub unsafe extern "C" fn hlp_track_entry(
    _id: i32,
    _t: *mut *mut hl_type,
    _count: *mut i32,
    _info: *mut i32,
    _stack: *mut varray,
) -> i32 {
    note_unsupported("track_entry");
    -1
}

/// `DEFINE_PRIM(_I32, track_get_bits, _BOOL)` — no flags set, exactly what
/// HashLink returns when `HL_TRACK_ENABLE` is off.
#[no_mangle]
pub unsafe extern "C" fn hlp_track_get_bits(_thread: bool) -> i32 {
    note_unsupported("track_get_bits");
    0
}

/// `DEFINE_PRIM(_VOID, track_lock, _BOOL)` — no bucket table to guard.
#[no_mangle]
pub unsafe extern "C" fn hlp_track_lock(_lock: bool) {
    note_unsupported("track_lock");
}

/// `DEFINE_PRIM(_VOID, track_reset, _NO_ARG)` — nothing recorded to clear.
#[no_mangle]
pub unsafe extern "C" fn hlp_track_reset() {
    note_unsupported("track_reset");
}

/// `DEFINE_PRIM(_VOID, track_set_bits, _I32 _BOOL)` — dropped. A following
/// `track_get_bits` still reports 0, so a caller sees the request did not take.
#[no_mangle]
pub unsafe extern "C" fn hlp_track_set_bits(_flags: i32, _thread: bool) {
    note_unsupported("track_set_bits");
}

/// `DEFINE_PRIM(_VOID, track_set_depth, _I32)` — dropped; `track_count`
/// keeps reporting depth 0.
#[no_mangle]
pub unsafe extern "C" fn hlp_track_set_depth(_d: i32) {
    note_unsupported("track_set_depth");
}

// --- profiler --------------------------------------------------------------

/// `DEFINE_PRIM(_VOID, sys_vtune_init, _NO_ARG)` — no VTune JIT API is wired
/// up. HashLink's body is also a no-op unless `hl_setup_vtune` ran first.
#[no_mangle]
pub unsafe extern "C" fn hlp_sys_vtune_init() {
    note_unsupported("sys_vtune_init");
}

// ---------------------------------------------------------------------------

#[cfg(test)]
mod unsupported_tooling_tests {
    //! Contract tests for the 19 stubs above (11 `debug_*`, 7 `track_*`,
    //! `sys_vtune_init`).
    //!
    //! These primitives implement nothing. They exist so a program that
    //! *references* the HashLink debugger-attach, allocation-tracking or VTune
    //! protocols LOADS instead of dying at startup with "natives resolved, N
    //! missing". So the job here is not to protect behaviour we want to keep
    //! working — it is to keep a later change from quietly making one of them
    //! *claim success*.
    //!
    //! A stub that answers "attached" is strictly worse than one that answers
    //! "unavailable": the caller is a debugger or a profiler and it will act on
    //! the answer — attaching to a session that will never produce an event, or
    //! reporting allocation buckets that were never sampled. Every assertion
    //! below therefore pins the honest answer:
    //!
    //!   * a QUERY returns the value that truthfully means unavailable —
    //!     `false`, `0`, `NULL`, or `-1` for `track_entry`, which is HashLink's
    //!     own "id out of range";
    //!   * an ACTION returns FAILURE wherever its `DEFINE_PRIM` line leaves room
    //!     for a result;
    //!   * a `_VOID` setter, which cannot signal at all, is pinned by its
    //!     read-back instead: the following query must still report 0, so the
    //!     caller can see the request did not take.
    //!
    //! Two things are deliberately never touched from here:
    //!
    //!   * `hlp_breakpoint` / `hl_detect_debugger`, which are real, not stubs.
    //!     On macOS the detector issues `ptrace(PT_DENY_ATTACH)` for the whole
    //!     process and the breakpoint traps into `brk #0`.
    //!   * `ASH_TRACE_DEBUGGER`. `debugger_trace_enabled` caches the lookup in a
    //!     `OnceLock`, so setting the variable inside one `#[test]` would race
    //!     every other test in this process. The only global state any of these
    //!     stubs reaches is that cache and the `Once` behind the warning it
    //!     gates, and neither feeds a return value — so no answer should depend
    //!     on what ran before it. `stubs_are_stateless_under_concurrent_use`
    //!     checks that rather than assuming it.

    use super::*;

    /// Memory we own, standing in for the addresses a debugger would hand these
    /// primitives. Filled with a recognisable pattern so that a stub which
    /// started writing into a caller's buffer would be visible; nothing is
    /// dereferenced by the implementation.
    const PATTERN: u8 = 0xAB;

    /// Neither the count nor the depth a real tracker would ever report, so a
    /// stub that leaves an out-parameter alone is distinguishable from one that
    /// writes a plausible-looking zero.
    const SENTINEL: i32 = 0x5EED_BEEF_u32 as i32;

    /// A `_ARR` argument for `track_entry`: a real, owned, empty array header.
    fn empty_varray() -> varray {
        varray {
            t: std::ptr::null_mut(),
            at: std::ptr::null_mut(),
            size: 0,
            __pad: 0,
        }
    }

    /// Every stub, coerced to the fn pointer type its `DEFINE_PRIM` line
    /// implies. This is a compile-time assertion: changing a return type or an
    /// argument list stops this test building.
    ///
    /// `debug_stop` is the one worth spelling out. HashLink's C prototype
    /// declares it `bool`, but its `DEFINE_PRIM` is `_VOID`, and `DEFINE_PRIM`
    /// is what a Haxe extern is validated against — so `_VOID` is the shape ash
    /// exports, and this test pins it. It is not a mistake to be "fixed".
    #[test]
    fn signatures_match_the_define_prim_lines() {
        let prims: [*const (); 19] = [
            // std/debug.c
            {
                // DEFINE_PRIM(_BOOL, debug_start, _I32)
                let f: unsafe extern "C" fn(i32) -> bool = hlp_debug_start;
                f as *const ()
            },
            {
                // DEFINE_PRIM(_VOID, debug_stop, _I32) — _VOID, not the C bool.
                let f: unsafe extern "C" fn(i32) = hlp_debug_stop;
                f as *const ()
            },
            {
                // DEFINE_PRIM(_BOOL, debug_breakpoint, _I32)
                let f: unsafe extern "C" fn(i32) -> bool = hlp_debug_breakpoint;
                f as *const ()
            },
            {
                // DEFINE_PRIM(_BOOL, debug_read, _I32 _BYTES _BYTES _I32)
                let f: unsafe extern "C" fn(i32, *mut vbyte, *mut vbyte, i32) -> bool =
                    hlp_debug_read;
                f as *const ()
            },
            {
                // DEFINE_PRIM(_BOOL, debug_write, _I32 _BYTES _BYTES _I32)
                let f: unsafe extern "C" fn(i32, *mut vbyte, *mut vbyte, i32) -> bool =
                    hlp_debug_write;
                f as *const ()
            },
            {
                // DEFINE_PRIM(_BOOL, debug_flush, _I32 _BYTES _I32)
                let f: unsafe extern "C" fn(i32, *mut vbyte, i32) -> bool = hlp_debug_flush;
                f as *const ()
            },
            {
                // DEFINE_PRIM(_I32, debug_wait, _I32 _REF(_I32) _I32)
                let f: unsafe extern "C" fn(i32, *mut i32, i32) -> i32 = hlp_debug_wait;
                f as *const ()
            },
            {
                // DEFINE_PRIM(_BOOL, debug_resume, _I32 _I32)
                let f: unsafe extern "C" fn(i32, i32) -> bool = hlp_debug_resume;
                f as *const ()
            },
            {
                // DEFINE_PRIM(_BYTES, debug_read_register, _I32 _I32 _I32 _BOOL)
                let f: unsafe extern "C" fn(i32, i32, i32, bool) -> *mut vbyte =
                    hlp_debug_read_register;
                f as *const ()
            },
            {
                // DEFINE_PRIM(_BOOL, debug_write_register, _I32 _I32 _I32 _BYTES _BOOL)
                let f: unsafe extern "C" fn(i32, i32, i32, *mut vbyte, bool) -> bool =
                    hlp_debug_write_register;
                f as *const ()
            },
            {
                // DEFINE_PRIM(_DYN, debug_call, _I32 _DYN)
                let f: unsafe extern "C" fn(i32, *mut vdynamic) -> *mut vdynamic = hlp_debug_call;
                f as *const ()
            },
            // std/track.c
            {
                // DEFINE_PRIM(_I32, track_count, _REF(_I32))
                let f: unsafe extern "C" fn(*mut i32) -> i32 = hlp_track_count;
                f as *const ()
            },
            {
                // DEFINE_PRIM(_I32, track_entry, _I32 _REF(_TYPE) _REF(_I32) _REF(_I32) _ARR)
                let f: unsafe extern "C" fn(
                    i32,
                    *mut *mut hl_type,
                    *mut i32,
                    *mut i32,
                    *mut varray,
                ) -> i32 = hlp_track_entry;
                f as *const ()
            },
            {
                // DEFINE_PRIM(_I32, track_get_bits, _BOOL)
                let f: unsafe extern "C" fn(bool) -> i32 = hlp_track_get_bits;
                f as *const ()
            },
            {
                // DEFINE_PRIM(_VOID, track_lock, _BOOL)
                let f: unsafe extern "C" fn(bool) = hlp_track_lock;
                f as *const ()
            },
            {
                // DEFINE_PRIM(_VOID, track_reset, _NO_ARG)
                let f: unsafe extern "C" fn() = hlp_track_reset;
                f as *const ()
            },
            {
                // DEFINE_PRIM(_VOID, track_set_bits, _I32 _BOOL)
                let f: unsafe extern "C" fn(i32, bool) = hlp_track_set_bits;
                f as *const ()
            },
            {
                // DEFINE_PRIM(_VOID, track_set_depth, _I32)
                let f: unsafe extern "C" fn(i32) = hlp_track_set_depth;
                f as *const ()
            },
            // std/sys.c
            {
                // DEFINE_PRIM(_VOID, sys_vtune_init, _NO_ARG)
                let f: unsafe extern "C" fn() = hlp_sys_vtune_init;
                f as *const ()
            },
        ];

        assert!(
            prims.iter().all(|p| !p.is_null()),
            "all 19 unsupported-tooling stubs must be real exported symbols; \
             the point of them is that native resolution finds them"
        );
    }

    /// The four `debug_*` actions a caller could otherwise believe succeeded.
    /// Each has room in its `DEFINE_PRIM` for a result, so each must report
    /// failure — a debugger that reads `true` here starts a session that will
    /// never produce an event.
    #[test]
    fn debug_actions_report_failure() {
        // Real memory of our own, standing in for the target-process addresses
        // a debugger would pass. Nothing below dereferences them.
        let mut addr = [PATTERN; 64];
        let mut payload = [PATTERN; 64];
        let addr_ptr: *mut vbyte = addr.as_mut_ptr();
        let payload_ptr: *mut vbyte = payload.as_mut_ptr();
        let pid = crate::sys::process_id() as i32;

        unsafe {
            assert!(
                !hlp_debug_start(pid),
                "debug_start must refuse to attach; ash implements no attach protocol"
            );
            assert!(
                !hlp_debug_breakpoint(pid),
                "debug_breakpoint must report it could not interrupt the target"
            );
            assert!(
                !hlp_debug_write(pid, addr_ptr, payload_ptr, payload.len() as i32),
                "debug_write must report that nothing was written"
            );
            assert!(
                !hlp_debug_flush(pid, addr_ptr, addr.len() as i32),
                "debug_flush must report failure; there is no target i-cache to flush"
            );
            assert!(
                !hlp_debug_resume(pid, 0),
                "debug_resume must report failure; nothing was ever stopped"
            );
            assert!(
                !hlp_debug_write_register(pid, 0, 0, payload_ptr, true),
                "debug_write_register must report that the register was not written"
            );
            assert!(
                !hlp_debug_write_register(pid, 0, 0, payload_ptr, false),
                "debug_write_register must fail for 32-bit registers too"
            );
        }
    }

    /// `debug_read` reports failure and says nothing about the buffer.
    ///
    /// The implementation deliberately leaves `buffer` untouched: zeroing it
    /// would hand the caller invented bytes as if they were the target's
    /// memory. So the contract this test pins is the return value only — a
    /// caller that ignores `false` and reads the buffer anyway is reading its
    /// own bytes, and that is the point.
    #[test]
    fn debug_read_reports_failure() {
        let mut addr = [PATTERN; 32];
        let mut buffer = [PATTERN; 32];
        let addr_ptr: *mut vbyte = addr.as_mut_ptr();
        let buffer_ptr: *mut vbyte = buffer.as_mut_ptr();

        unsafe {
            assert!(
                !hlp_debug_read(1, addr_ptr, buffer_ptr, buffer.len() as i32),
                "debug_read must report that nothing was read"
            );
            // A zero-length read is still a read: it must not become the one
            // call that succeeds.
            assert!(
                !hlp_debug_read(1, addr_ptr, buffer_ptr, 0),
                "debug_read must fail for a zero-length request too"
            );
        }
    }

    /// The `debug_*` queries: no session, no register value, no callback result.
    #[test]
    fn debug_queries_report_nothing_available() {
        unsafe {
            assert!(
                hlp_debug_read_register(1, 0, 0, true).is_null(),
                "debug_read_register must return NULL, never an invented register value"
            );
            assert!(
                hlp_debug_read_register(1, 0, 0, false).is_null(),
                "debug_read_register must return NULL for 32-bit registers too"
            );

            // _DYN in, _DYN out. A real (owned, zeroed) vdynamic goes in so the
            // argument is a genuine object; the stub never dereferences it.
            let mut v: vdynamic = std::mem::zeroed();
            assert!(
                hlp_debug_call(0, &mut v).is_null(),
                "debug_call must return NULL, as HashLink's own body does"
            );
            assert!(
                hlp_debug_call(0, std::ptr::null_mut()).is_null(),
                "debug_call must return NULL for a null argument too"
            );
        }
    }

    /// `debug_wait` reports the session gone (`0` — "target exited"), which ends
    /// a debugger's event loop instead of spinning it forever, and clears the
    /// `_REF(_I32)` thread out-parameter it was handed.
    #[test]
    fn debug_wait_reports_the_session_gone() {
        let mut thread = SENTINEL;

        unsafe {
            assert_eq!(
                hlp_debug_wait(1, &mut thread, 0),
                0,
                "debug_wait must return 0 (target exited) so the caller's event loop ends"
            );
            assert_eq!(
                thread, 0,
                "debug_wait must clear the thread out-parameter, not leave a stale id"
            );

            // Polled in a loop by the tools that call it; a non-zero timeout is
            // not a different answer.
            thread = SENTINEL;
            assert_eq!(hlp_debug_wait(1, &mut thread, 1000), 0);
            assert_eq!(thread, 0);

            // The out-parameter is guarded, so a caller passing none is answered
            // rather than crashed.
            assert_eq!(hlp_debug_wait(1, std::ptr::null_mut(), 0), 0);
        }
    }

    /// `debug_stop` is `_VOID`: it has no way to report anything, so all this
    /// can pin is that it returns normally and detaches from nothing. The
    /// signature itself is pinned by `signatures_match_the_define_prim_lines`.
    #[test]
    fn debug_stop_is_void_and_returns() {
        unsafe {
            hlp_debug_stop(1);
            hlp_debug_stop(std::process::id() as i32);
        }

        // Nothing was attached, so nothing detaches: the queries answer exactly
        // as they did before.
        unsafe {
            assert!(!hlp_debug_start(1), "debug_start still refuses after a stop");
            assert_eq!(
                hlp_debug_wait(1, std::ptr::null_mut(), 0),
                0,
                "debug_wait still reports the session gone after a stop"
            );
        }
    }

    /// The `track_*` queries: an empty bucket table, zero depth, no flags.
    #[test]
    fn track_queries_report_an_empty_table() {
        unsafe {
            let mut depth = SENTINEL;
            assert_eq!(
                hlp_track_count(&mut depth),
                0,
                "track_count must report zero live buckets; ash's GC records none"
            );
            assert_eq!(depth, 0, "track_count must report depth 0");

            // _REF(_I32) is always a real ref from the VM, but the guard is
            // there, so a caller passing none is answered rather than crashed.
            assert_eq!(hlp_track_count(std::ptr::null_mut()), 0);

            assert_eq!(
                hlp_track_get_bits(false),
                0,
                "track_get_bits must report no flags set, as HashLink does with HL_TRACK_ENABLE off"
            );
            assert_eq!(
                hlp_track_get_bits(true),
                0,
                "track_get_bits must report no flags set for the per-thread mask too"
            );
        }
    }

    /// `track_entry` answers `-1` — HashLink's "id out of range" — and, like
    /// upstream, returns before touching any out-parameter. With `track_count`
    /// at 0 every id is out of range, including 0.
    #[test]
    fn track_entry_reports_every_id_out_of_range() {
        unsafe {
            let mut stack = empty_varray();

            for id in [0i32, 1, 7, i32::MAX, -1] {
                let mut t: *mut hl_type = std::ptr::null_mut();
                // Sentinels: upstream returns -1 before writing these, so they
                // must still hold the caller's values afterwards.
                let mut count = SENTINEL;
                let mut info = SENTINEL;

                assert_eq!(
                    hlp_track_entry(id, &mut t, &mut count, &mut info, &mut stack),
                    -1,
                    "track_entry({id}) must answer -1 (id out of range)"
                );
                assert!(
                    t.is_null(),
                    "track_entry must not write the type out-parameter"
                );
                assert_eq!(
                    count, SENTINEL,
                    "track_entry must return before touching the count out-parameter"
                );
                assert_eq!(
                    info, SENTINEL,
                    "track_entry must return before touching the info out-parameter"
                );
            }

            // The array header it was handed is the caller's; leave it alone.
            assert_eq!(stack.size, 0, "track_entry must not write into the _ARR");
            assert!(stack.t.is_null());
            assert!(stack.at.is_null());
        }
    }

    /// `track_set_bits` and `track_set_depth` are `_VOID` and so cannot report
    /// that they were dropped. The read-back is the report: whatever is asked
    /// for, the following `track_get_bits` / `track_count` must still say 0, so
    /// a caller can see the request did not take.
    #[test]
    fn track_setters_do_not_take() {
        unsafe {
            for flags in [1i32, 0x0F, i32::MAX, -1] {
                hlp_track_set_bits(flags, false);
                assert_eq!(
                    hlp_track_get_bits(false),
                    0,
                    "track_set_bits({flags}) must not take: get_bits still reports 0"
                );

                hlp_track_set_bits(flags, true);
                assert_eq!(
                    hlp_track_get_bits(true),
                    0,
                    "track_set_bits({flags}, thread) must not take either"
                );
            }

            for d in [1i32, 16, 64, i32::MAX] {
                hlp_track_set_depth(d);
                let mut depth = SENTINEL;
                assert_eq!(
                    hlp_track_count(&mut depth),
                    0,
                    "track_set_depth({d}) must not conjure buckets"
                );
                assert_eq!(
                    depth, 0,
                    "track_set_depth({d}) must not take: track_count still reports depth 0"
                );
            }
        }
    }

    /// The remaining `_VOID` no-ops: there is no bucket table to guard and
    /// nothing recorded to clear, and neither may become the call that makes the
    /// table look populated afterwards.
    #[test]
    fn track_lock_and_reset_change_nothing() {
        unsafe {
            hlp_track_lock(true);
            hlp_track_reset();
            hlp_track_lock(false);

            let mut depth = SENTINEL;
            assert_eq!(hlp_track_count(&mut depth), 0);
            assert_eq!(depth, 0);
            assert_eq!(hlp_track_get_bits(false), 0);

            let mut stack = empty_varray();
            let mut t: *mut hl_type = std::ptr::null_mut();
            let mut count = SENTINEL;
            let mut info = SENTINEL;
            assert_eq!(
                hlp_track_entry(0, &mut t, &mut count, &mut info, &mut stack),
                -1
            );
        }
    }

    /// No VTune JIT API is wired up, and `sys_vtune_init` is `_NO_ARG` `_VOID`:
    /// it can only be a no-op that returns, including when called more than once.
    #[test]
    fn vtune_init_is_a_no_op() {
        unsafe {
            hlp_sys_vtune_init();
            hlp_sys_vtune_init();
        }
    }

    /// The stubs must be stateless.
    ///
    /// `cargo test` runs `#[test]` functions on parallel threads in one process,
    /// so `track_setters_do_not_take` above is only meaningful if a setter
    /// cannot latch anything for another thread to observe. Rather than assume
    /// that, hammer the whole set concurrently and require every answer to be
    /// the unavailable one — which also rules out the other shape of the bug
    /// this file guards against: a stub that succeeds once (on first call, on
    /// one thread) and fails afterwards.
    #[test]
    fn stubs_are_stateless_under_concurrent_use() {
        const THREADS: usize = 4;
        const ROUNDS: usize = 500;

        let mut handles = Vec::with_capacity(THREADS);
        for n in 0..THREADS {
            handles.push(std::thread::spawn(move || {
                let mut addr = [PATTERN; 16];
                let mut payload = [PATTERN; 16];
                let addr_ptr: *mut vbyte = addr.as_mut_ptr();
                let payload_ptr: *mut vbyte = payload.as_mut_ptr();
                let pid = n as i32 + 1;

                for round in 0..ROUNDS {
                    let bits = round as i32 | 1;
                    unsafe {
                        // Ask, loudly, for everything to be turned on...
                        hlp_track_set_bits(bits, round % 2 == 0);
                        hlp_track_set_depth(bits);
                        hlp_track_lock(true);
                        hlp_track_reset();
                        hlp_track_lock(false);
                        hlp_sys_vtune_init();
                        hlp_debug_stop(pid);

                        // ...and get "unavailable" every time regardless.
                        assert!(!hlp_debug_start(pid));
                        assert!(!hlp_debug_breakpoint(pid));
                        assert!(!hlp_debug_read(pid, addr_ptr, payload_ptr, 16));
                        assert!(!hlp_debug_write(pid, addr_ptr, payload_ptr, 16));
                        assert!(!hlp_debug_flush(pid, addr_ptr, 16));
                        assert!(!hlp_debug_resume(pid, round as i32));
                        assert!(!hlp_debug_write_register(pid, 0, 0, payload_ptr, true));
                        assert!(hlp_debug_read_register(pid, 0, 0, true).is_null());
                        assert!(hlp_debug_call(round as i32, std::ptr::null_mut()).is_null());

                        let mut thread = SENTINEL;
                        assert_eq!(hlp_debug_wait(pid, &mut thread, 0), 0);
                        assert_eq!(thread, 0);

                        let mut depth = SENTINEL;
                        assert_eq!(hlp_track_count(&mut depth), 0);
                        assert_eq!(depth, 0);
                        assert_eq!(hlp_track_get_bits(false), 0);
                        assert_eq!(hlp_track_get_bits(true), 0);

                        let mut stack = empty_varray();
                        let mut t: *mut hl_type = std::ptr::null_mut();
                        let mut count = SENTINEL;
                        let mut info = SENTINEL;
                        assert_eq!(
                            hlp_track_entry(round as i32, &mut t, &mut count, &mut info, &mut stack),
                            -1
                        );
                        assert!(t.is_null());
                        assert_eq!(count, SENTINEL);
                        assert_eq!(info, SENTINEL);
                    }
                }
            }));
        }

        for h in handles {
            h.join().expect("a stub answered something other than \"unavailable\"");
        }
    }
}
