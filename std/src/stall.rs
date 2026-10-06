//! Where a thread that a world stop is waiting on is running.
//!
//! The collector cannot walk a running thread's stack, so it interrupts the
//! thread with a signal: the handler copies the interrupted pc, link register
//! and frame-pointer chain into a static buffer, and the collector names the
//! frames once the handler is done. Used only to explain a slow stop under
//! `ASH_GC_STATS`; the handler is installed the first time it is needed.

use std::ffi::c_void;
use std::sync::OnceLock;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::time::{Duration, Instant};

const DEPTH: usize = 16;
/// Ignored by default, so a thread that somehow sees it without the handler
/// is not killed. Installed with `SA_RESTART`: an interrupted system call
/// carries on rather than failing with `EINTR` in a library's code.
const SIGNAL: libc::c_int = libc::SIGURG;

static STACK_TOP: AtomicUsize = AtomicUsize::new(0);
static FRAMES: [AtomicUsize; DEPTH] = [const { AtomicUsize::new(0) }; DEPTH];
static COUNT: AtomicUsize = AtomicUsize::new(0);
static DONE: AtomicBool = AtomicBool::new(false);

/// `(pc, lr, fp, sp)` from a signal context; `lr` is 0 where there is none.
#[cfg(all(target_os = "macos", target_arch = "aarch64"))]
unsafe fn registers(ctx: *mut c_void) -> Option<(usize, usize, usize, usize)> {
    #[repr(C)]
    struct ThreadState {
        x: [u64; 29],
        fp: u64,
        lr: u64,
        sp: u64,
        pc: u64,
    }
    #[repr(C)]
    struct Mcontext {
        far: u64,
        esr: u32,
        exception: u32,
        ss: ThreadState,
    }
    #[repr(C)]
    struct UContext {
        uc_onstack: i32,
        uc_sigmask: u32,
        uc_stack_sp: *mut c_void,
        uc_stack_size: usize,
        uc_stack_flags: i32,
        _pad: i32,
        uc_link: *mut c_void,
        uc_mcsize: usize,
        uc_mcontext: *mut Mcontext,
    }
    unsafe {
        let mc = (*(ctx as *const UContext)).uc_mcontext;
        if mc.is_null() {
            return None;
        }
        let ss = &(*mc).ss;
        Some((ss.pc as usize, ss.lr as usize, ss.fp as usize, ss.sp as usize))
    }
}

#[cfg(all(target_os = "linux", target_arch = "aarch64"))]
unsafe fn registers(ctx: *mut c_void) -> Option<(usize, usize, usize, usize)> {
    let mc = unsafe { &(*(ctx as *const libc::ucontext_t)).uc_mcontext };
    Some((mc.pc as usize, mc.regs[30] as usize, mc.regs[29] as usize, mc.sp as usize))
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
unsafe fn registers(ctx: *mut c_void) -> Option<(usize, usize, usize, usize)> {
    let gregs = unsafe { &(*(ctx as *const libc::ucontext_t)).uc_mcontext.gregs };
    Some((
        gregs[libc::REG_RIP as usize] as usize,
        0,
        gregs[libc::REG_RBP as usize] as usize,
        gregs[libc::REG_RSP as usize] as usize,
    ))
}

#[cfg(not(any(
    all(target_os = "macos", target_arch = "aarch64"),
    all(target_os = "linux", any(target_arch = "aarch64", target_arch = "x86_64"))
)))]
unsafe fn registers(_ctx: *mut c_void) -> Option<(usize, usize, usize, usize)> {
    None
}

/// Async-signal-safe: loads and stores only. The frame chain is followed
/// only while each frame lies between the interrupted sp and the thread's
/// stack top, so a frame without a frame pointer ends the walk instead of
/// faulting.
extern "C" fn on_signal(_sig: libc::c_int, _info: *mut libc::siginfo_t, ctx: *mut c_void) {
    let mut n = 0;
    let mut push = |pc: usize| {
        if n < DEPTH {
            FRAMES[n].store(pc, Ordering::Relaxed);
            n += 1;
        }
    };
    if let Some((pc, lr, mut fp, sp)) = unsafe { registers(ctx) } {
        push(pc);
        if lr != 0 && lr != pc {
            push(lr);
        }
        let top = STACK_TOP.load(Ordering::Relaxed);
        while fp >= sp && fp.saturating_add(16) <= top && fp.is_multiple_of(16) {
            let saved = unsafe { *(fp as *const usize) };
            let ra = unsafe { *((fp + 8) as *const usize) };
            if ra < 0x1000 || saved <= fp {
                break;
            }
            push(ra);
            fp = saved;
        }
    }
    COUNT.store(n, Ordering::Relaxed);
    DONE.store(true, Ordering::Release);
}

fn install() -> bool {
    static OK: OnceLock<bool> = OnceLock::new();
    *OK.get_or_init(|| unsafe {
        let mut action: libc::sigaction = std::mem::zeroed();
        action.sa_sigaction = on_signal as *const () as usize;
        action.sa_flags = libc::SA_SIGINFO | libc::SA_RESTART;
        libc::sigemptyset(&mut action.sa_mask);
        libc::sigaction(SIGNAL, &action, std::ptr::null_mut()) == 0
    })
}

/// The return addresses of `thread`, innermost first, interrupting it once.
/// Empty when it does not answer within a few milliseconds or the platform
/// has no reader for the signal context. One caller at a time: the
/// collector, holding the world lock.
pub(crate) fn sample(thread: libc::pthread_t, stack_top: usize) -> Vec<usize> {
    if !install() {
        return Vec::new();
    }
    DONE.store(false, Ordering::Relaxed);
    STACK_TOP.store(stack_top, Ordering::Relaxed);
    if unsafe { libc::pthread_kill(thread, SIGNAL) } != 0 {
        return Vec::new();
    }
    let began = Instant::now();
    while !DONE.load(Ordering::Acquire) {
        if began.elapsed() > Duration::from_millis(20) {
            return Vec::new();
        }
        std::thread::sleep(Duration::from_micros(100));
    }
    (0..COUNT.load(Ordering::Relaxed))
        .map(|i| FRAMES[i].load(Ordering::Relaxed))
        .collect()
}

/// `pc` as a symbol where `dladdr` or the AOT table can name it, else as an
/// offset into its library (glibc's `memset` variants, say, are local
/// symbols), else hex: JIT bodies have neither.
pub(crate) fn name(pc: usize) -> String {
    if let Some(name) = unsafe { crate::error::aot_symbol_via_dladdr(pc) } {
        return format!("{pc:#x} {name}");
    }
    let mut info: libc::Dl_info = unsafe { std::mem::zeroed() };
    if unsafe { libc::dladdr(pc as *const c_void, &mut info) } != 0 && !info.dli_fname.is_null() {
        let path = unsafe { std::ffi::CStr::from_ptr(info.dli_fname) }.to_string_lossy();
        let file = path.rsplit('/').next().unwrap_or(&path);
        return format!("{pc:#x} {file}+{:#x}", pc - info.dli_fbase as usize);
    }
    format!("{pc:#x}")
}
