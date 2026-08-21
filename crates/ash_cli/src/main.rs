use anyhow::Result;
use ash_core::bytecode::BytecodeDecoder;
use ash_core::native_lib::{init_std_library, NativeFunctionResolver};
use ash_interp::interpreter::{HLInterpreter, TierMode, TieredConfig};
use clap::{Parser, ValueEnum};
use std::path::PathBuf;
use std::process;
// Only the unix crash handler reads a OnceLock (the ASH_CRASH_BACKTRACE
// latch); on Windows there is no handler and no latch.
#[cfg(unix)]
use std::sync::OnceLock;

#[derive(Parser)]
#[command(name = "ash", about = "ASH - HashLink bytecode runtime (interp | hybrid | jit)")]
struct Cli {
    /// Path to a HashLink bytecode (.hl) file
    file: Option<PathBuf>,

    /// Everything after the file belongs to the PROGRAM, exactly as the
    /// stock `hl` CLI behaves: `ash [options] file.hl [args...]`. Without
    /// the trailing capture, clap rejected the program's own arguments as
    /// unknown options and ash refused to start at all.
    #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
    program_args: Vec<String>,

    /// Execution mode
    #[arg(long, value_enum, default_value_t = Mode::Interp)]
    mode: Mode,

    /// Hot-call threshold for JIT promotion in hybrid mode
    #[arg(long, default_value_t = 100)]
    jit_threshold: u64,

    /// Enable tiered runtime promotion logs
    #[arg(long, default_value_t = false)]
    jit_log: bool,

    /// Max argument count for promoted calls
    #[arg(long, default_value_t = 8)]
    jit_max_args: usize,

    /// Optional static opcode-size gate before promotion (0 disables, call-count only)
    #[arg(long, default_value_t = 0)]
    jit_min_ops: usize,

    /// Suppress non-program output (useful for parity testing)
    #[arg(long, default_value_t = false)]
    quiet: bool,

    /// Enable hot-reload support (converts direct calls to indirect dispatch)
    #[arg(long, default_value_t = false)]
    hot_reload: bool,

    /// Which rungs of the interpreter -> Cranelift -> LLVM ladder to use.
    /// `auto` (default) runs both JIT tiers; `cranelift` and `llvm` pin a
    /// single tier for testing; `off` disables promotion. Overridden by the
    /// ASH_TIER environment variable when this flag is left at its default.
    #[arg(long, value_name = "auto|cranelift|llvm|off")]
    jit_tier: Option<String>,
}

#[derive(Clone, ValueEnum)]
enum Mode {
    /// Run using the bytecode interpreter
    Interp,
    /// Run using the JIT compiler
    Jit,
    /// Hybrid mode (interpreter with JIT tier promotion)
    Hybrid,
}

/// Whether the crash handler should attempt a (deliberately unsafe) backtrace.
///
/// Read from `ASH_CRASH_BACKTRACE` exactly once, during `main`, before any
/// handler can fire: `getenv` is not async-signal-safe, and neither is the
/// capture itself (see [`crash_handler_siginfo`]).
#[cfg(unix)]
static CRASH_BACKTRACE: OnceLock<bool> = OnceLock::new();


// See Cargo.toml: glibc frees lazily; jemalloc is the wren_lift-proven fix.
#[cfg(target_os = "linux")]
#[global_allocator]
static GLOBAL: tikv_jemallocator::Jemalloc = tikv_jemallocator::Jemalloc;

fn main() {
    // The crash-reporting complex below is unix signal machinery
    // (sigaction/SA_SIGINFO, siginfo_t, a signal-context register walk), so
    // it installs as one unit on unix only. Windows runs without a crash
    // handler for now: the counterpart is an SEH/vectored exception filter —
    // a different architecture, not a per-line substitution — tracked in
    // docs/windows-port.md. An access violation there dies with the OS
    // default (exit code 0xC0000005), exactly as any unhandled fault does.
    #[cfg(unix)]
    {
        // Resolve the crash-time options before installing the handler, so
        // the handler itself never touches the environment (getenv is not
        // async-signal-safe and may be mid-mutation when the fault lands).
        let _ = CRASH_BACKTRACE.set(
            std::env::var("ASH_CRASH_BACKTRACE")
                .map(|v| v != "0" && !v.is_empty())
                .unwrap_or(false),
        );

        // Install signal handlers with sigaction for faulting address info
        unsafe {
            let mut sa: libc::sigaction = std::mem::zeroed();
            sa.sa_sigaction = crash_handler_siginfo as *const () as usize;
            sa.sa_flags = libc::SA_SIGINFO;
            libc::sigaction(libc::SIGSEGV, &sa, std::ptr::null_mut());
            libc::sigaction(libc::SIGBUS, &sa, std::ptr::null_mut());
            libc::sigaction(libc::SIGABRT, &sa, std::ptr::null_mut());
        }
    }

    // Start profiling before any work happens, and on this thread: the sampler
    // interrupts whichever thread calls init, and that must be the one that
    // runs the program.
    ash_core::profile::init();

    let result = run();
    ash_core::profile::report();
    if let Err(e) = result {
        eprintln!("Error: {:#}", e);
        process::exit(1);
    }
}

/// `errno` for the current thread. A plain read of thread-local storage, so
/// it is safe to consult from a signal handler.
#[cfg(any(target_os = "macos", target_os = "ios"))]
unsafe fn errno() -> i32 {
    *libc::__error()
}

#[cfg(all(unix, not(any(target_os = "macos", target_os = "ios"))))]
unsafe fn errno() -> i32 {
    *libc::__errno_location()
}

/// Write a byte slice straight to fd 2, retrying short writes and `EINTR`.
///
/// `write(2)` is async-signal-safe; `eprintln!` is not (it takes a lock and
/// the formatting machinery can allocate).
///
/// unix-only, like every one of its callers: raw fd 2 and `EINTR` are the
/// unix contract, and the only consumer is the signal handler above/below.
/// A Windows crash handler would write via
/// `WriteFile(GetStdHandle(STD_ERROR_HANDLE))` and lands with it.
#[cfg(unix)]
unsafe fn write_stderr(bytes: &[u8]) {
    let mut off = 0usize;
    while off < bytes.len() {
        let n = libc::write(
            libc::STDERR_FILENO,
            bytes.as_ptr().add(off) as *const std::ffi::c_void,
            bytes.len() - off,
        );
        if n > 0 {
            off += n as usize;
        } else if n < 0 && errno() == libc::EINTR {
            continue;
        } else {
            return;
        }
    }
}

/// Append `src` to `buf` at `len`, truncating at the buffer's end.
#[cfg(unix)]
fn push_bytes(buf: &mut [u8], len: &mut usize, src: &[u8]) {
    for &b in src {
        if *len >= buf.len() {
            return;
        }
        buf[*len] = b;
        *len += 1;
    }
}

/// Append `v` in decimal. Manual formatting: `format!` allocates.
#[cfg(unix)]
fn push_dec(buf: &mut [u8], len: &mut usize, v: u64) {
    let mut digits = [0u8; 20];
    let mut n = 0;
    let mut v = v;
    loop {
        digits[n] = b'0' + (v % 10) as u8;
        n += 1;
        v /= 10;
        if v == 0 {
            break;
        }
    }
    while n > 0 {
        n -= 1;
        push_bytes(buf, len, &[digits[n]]);
    }
}

/// Append `v` in hex, no `0x` prefix. Manual formatting: `format!` allocates.
#[cfg(unix)]
fn push_hex(buf: &mut [u8], len: &mut usize, v: u64) {
    const HEX: &[u8; 16] = b"0123456789abcdef";
    let mut digits = [0u8; 16];
    let mut n = 0;
    let mut v = v;
    loop {
        digits[n] = HEX[(v & 0xf) as usize];
        n += 1;
        v >>= 4;
        if v == 0 {
            break;
        }
    }
    while n > 0 {
        n -= 1;
        push_bytes(buf, len, &[digits[n]]);
    }
}

// The `ucontext_t` a `SA_SIGINFO` handler receives on arm64 macOS. The `libc`
// crate does not define these for Apple targets, so mirror the (stable)
// `<sys/_types/_ucontext.h>` / `<mach/arm/_structs.h>` layout. Only the
// register file is read, and only inside the crash handler.
#[cfg(all(target_os = "macos", target_arch = "aarch64"))]
#[repr(C)]
struct ArmThreadState64 {
    x: [u64; 29],
    fp: u64,
    lr: u64,
    sp: u64,
    pc: u64,
    cpsr: u32,
    _pad: u32,
}

#[cfg(all(target_os = "macos", target_arch = "aarch64"))]
#[repr(C)]
struct ArmExceptionState64 {
    far: u64,
    esr: u32,
    exception: u32,
}

#[cfg(all(target_os = "macos", target_arch = "aarch64"))]
#[repr(C)]
struct McontextArm64 {
    es: ArmExceptionState64,
    ss: ArmThreadState64,
    // __darwin_arm_neon_state64 follows; unused here.
}

#[cfg(all(target_os = "macos", target_arch = "aarch64"))]
#[repr(C)]
struct UContext64 {
    uc_onstack: i32,
    uc_sigmask: u32,
    uc_stack_sp: *mut std::ffi::c_void,
    uc_stack_size: usize,
    uc_stack_flags: i32,
    _pad: i32,
    uc_link: *mut UContext64,
    uc_mcsize: usize,
    uc_mcontext: *mut McontextArm64,
}

/// Read `(pc, lr, fp, sp)` out of a signal context. Plain memory loads, so
/// this is async-signal-safe.
#[cfg(all(target_os = "macos", target_arch = "aarch64"))]
unsafe fn signal_registers(ctx: *mut std::ffi::c_void) -> Option<(u64, u64, u64, u64)> {
    if ctx.is_null() {
        return None;
    }
    let mc = (*(ctx as *const UContext64)).uc_mcontext;
    if mc.is_null() {
        return None;
    }
    let ss = &(*mc).ss;
    Some((ss.pc, ss.lr, ss.fp, ss.sp))
}

/// x86_64 Linux: the register file lives in `ucontext_t.uc_mcontext.gregs`,
/// indexed by the `REG_*` constants from `<sys/ucontext.h>`. `libc` models
/// this one, so no hand-rolled layout is needed.
///
/// x86_64 has no link register — the return address sits on the stack at
/// `[rbp+8]` once a frame is set up — so `lr` is reported as 0 rather than
/// dereferencing a possibly-garbage frame pointer inside a signal handler.
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
unsafe fn signal_registers(ctx: *mut std::ffi::c_void) -> Option<(u64, u64, u64, u64)> {
    if ctx.is_null() {
        return None;
    }
    const REG_RBP: usize = 10;
    const REG_RSP: usize = 15;
    const REG_RIP: usize = 16;
    let gregs = &(*(ctx as *const libc::ucontext_t)).uc_mcontext.gregs;
    Some((
        gregs[REG_RIP] as u64,
        0,
        gregs[REG_RBP] as u64,
        gregs[REG_RSP] as u64,
    ))
}

#[cfg(all(
    unix,
    not(any(
        all(target_os = "macos", target_arch = "aarch64"),
        all(target_os = "linux", target_arch = "x86_64")
    ))
))]
unsafe fn signal_registers(_ctx: *mut std::ffi::c_void) -> Option<(u64, u64, u64, u64)> {
    None
}

/// Fatal-signal handler. **Everything here must be async-signal-safe.**
///
/// The handler runs on a thread that was interrupted at an arbitrary
/// instruction — possibly inside `malloc` while it holds its own lock. So it
/// allocates nothing and formats nothing: the report is assembled in a stack
/// buffer with manual integer formatting and pushed out with a single
/// `write(2)`, then the default disposition is restored and the signal
/// re-raised so the process dies with the right status and the real faulting
/// frame is still on top for a debugger or crash reporter.
///
/// This replaces an earlier version that called
/// `std::backtrace::Backtrace::force_capture()` plus `eprintln!`: both
/// allocate, so the handler reliably faulted a second time inside its own
/// `RawVec` teardown and printed *that* stack — hiding the actual crash site.
/// A backtrace is still available under `ASH_CRASH_BACKTRACE=1`, but it is
/// strictly best-effort and formally unsafe (it can deadlock or double-fault
/// exactly as before); use it only when the fault is known not to involve the
/// allocator.
#[cfg(unix)]
unsafe extern "C" fn crash_handler_siginfo(
    sig: i32,
    info: *mut libc::siginfo_t,
    ctx: *mut std::ffi::c_void,
) {
    // `si_addr` is a plain field in Apple's `siginfo_t`, but on Linux the
    // `libc` crate models the sigaction union with an accessor method, so the
    // two spellings are not interchangeable.
    let fault_addr = if !info.is_null() {
        #[cfg(any(target_os = "macos", target_os = "ios"))]
        {
            (*info).si_addr as usize
        }
        #[cfg(not(any(target_os = "macos", target_os = "ios")))]
        {
            (*info).si_addr() as usize
        }
    } else {
        0
    };

    // If a native call recovery point is armed, siglongjmp back to it
    // instead of crashing. This handles cases like macOS GL driver bugs
    // where native code triggers SIGSEGV during normal operation.
    if ash_interp::native_recovery::try_recover_from_signal(sig, fault_addr) {
        return; // unreachable — siglongjmp never returns
    }

    let name: &[u8] = match sig {
        libc::SIGSEGV => b"SIGSEGV",
        libc::SIGBUS => b"SIGBUS",
        libc::SIGABRT => b"SIGABRT",
        _ => b"UNKNOWN",
    };

    let mut buf = [0u8; 256];
    let mut len = 0usize;
    push_bytes(&mut buf, &mut len, b"\n=== CRASH: ");
    push_bytes(&mut buf, &mut len, name);
    push_bytes(&mut buf, &mut len, b" (signal ");
    push_dec(&mut buf, &mut len, sig as u64);
    push_bytes(&mut buf, &mut len, b") fault_addr=0x");
    push_hex(&mut buf, &mut len, fault_addr as u64);

    // The faulting PC/LR/FP straight out of the signal context. Reading the
    // context is just a memory load, so it is signal-safe — and it is the only
    // way to name the faulting frame here, since symbolization is not. JIT
    // frames have no symbol at all: cross-check the PC against the
    // `[tiered] promoted findex=… addr=…` lines that `--jit-log` prints.
    if let Some((pc, lr, fp, sp)) = signal_registers(ctx) {
        push_bytes(&mut buf, &mut len, b" pc=0x");
        push_hex(&mut buf, &mut len, pc);
        push_bytes(&mut buf, &mut len, b" lr=0x");
        push_hex(&mut buf, &mut len, lr);
        push_bytes(&mut buf, &mut len, b" fp=0x");
        push_hex(&mut buf, &mut len, fp);
        push_bytes(&mut buf, &mut len, b" sp=0x");
        push_hex(&mut buf, &mut len, sp);
    }
    push_bytes(&mut buf, &mut len, b" ===\n");
    write_stderr(&buf[..len]);

    // Frame-pointer walk, default-on. Bounded stack reads plus a try_lock
    // registry lookup — nothing here allocates or takes a lock it could
    // deadlock on, unlike the opt-in Rust backtrace below. Both arm64 and
    // x86_64 store [saved_fp, return_addr] at fp, so one walk serves both.
    // JIT frames are named from the promotion registry (findex + tier);
    // everything else goes through dladdr on unix.
    if let Some((pc0, lr, mut fp, _sp)) = signal_registers(ctx) {
        write_stderr(b"[ash] frames (innermost first):\n");
        let mut frame = 0usize;
        let emit = |pc: u64, frame: usize| {
            let mut b = [0u8; 192];
            let mut l = 0usize;
            push_bytes(&mut b, &mut l, b"  #");
            push_dec(&mut b, &mut l, frame as u64);
            push_bytes(&mut b, &mut l, b" 0x");
            push_hex(&mut b, &mut l, pc);
            if let Some((findex, tier, off)) =
                ash_core::profile::describe_jit_pc(pc as usize)
            {
                push_bytes(&mut b, &mut l, b" jit findex=");
                push_dec(&mut b, &mut l, findex as u64);
                push_bytes(&mut b, &mut l, b" (");
                push_bytes(&mut b, &mut l, tier.as_bytes());
                push_bytes(&mut b, &mut l, b"+0x");
                push_hex(&mut b, &mut l, off as u64);
                push_bytes(&mut b, &mut l, b")");
            } else {
                #[cfg(unix)]
                unsafe {
                    let mut info: libc::Dl_info = std::mem::zeroed();
                    if libc::dladdr(pc as *const std::ffi::c_void, &mut info) != 0
                        && !info.dli_sname.is_null()
                    {
                        let name = std::ffi::CStr::from_ptr(info.dli_sname);
                        push_bytes(&mut b, &mut l, b" ");
                        // Demangle into the fixed buffer — rustc_demangle
                        // formats lazily, so a stack fmt::Write sink keeps
                        // this allocation-free, which the signal context
                        // requires.
                        struct Sink<'a> {
                            buf: &'a mut [u8; 192],
                            len: &'a mut usize,
                        }
                        impl std::fmt::Write for Sink<'_> {
                            fn write_str(&mut self, s: &str) -> std::fmt::Result {
                                push_bytes(self.buf, self.len, s.as_bytes());
                                Ok(())
                            }
                        }
                        if let Ok(sym) = name.to_str() {
                            let _ = std::fmt::write(
                                &mut Sink { buf: &mut b, len: &mut l },
                                format_args!("{:#}", rustc_demangle::demangle(sym)),
                            );
                        } else {
                            let bytes = name.to_bytes();
                            let take = bytes.len().min(120);
                            push_bytes(&mut b, &mut l, &bytes[..take]);
                        }
                    }
                }
            }
            push_bytes(&mut b, &mut l, b"\n");
            write_stderr(&b[..l]);
        };
        emit(pc0, frame);
        frame += 1;
        if lr != 0 && lr != pc0 {
            emit(lr, frame);
            frame += 1;
        }
        // The chain itself: each fp points at [saved_fp, return_addr].
        // Sanity bounds keep a corrupted fp from turning the report into a
        // second fault: alignment, monotonic growth, and a page of slack
        // below 48 bits of address space.
        while frame < 32 {
            if fp == 0 || fp & 0xF != 0 || fp > 0x7FFF_FFFF_F000 {
                break;
            }
            let saved_fp = unsafe { *(fp as *const u64) };
            let ra = unsafe { *((fp + 8) as *const u64) };
            if ra < 0x1000 {
                break;
            }
            emit(ra, frame);
            frame += 1;
            if saved_fp <= fp {
                break;
            }
            fp = saved_fp;
        }
    }

    // Opt-in, best-effort, and NOT async-signal-safe — see the doc comment.
    if *CRASH_BACKTRACE.get().unwrap_or(&false) {
        write_stderr(b"[ash] ASH_CRASH_BACKTRACE=1: capturing (unsafe in a signal handler)\n");
        let bt = std::backtrace::Backtrace::force_capture();
        let text = bt.to_string();
        write_stderr(text.as_bytes());
        write_stderr(b"\n");
    }

    // Restore the default disposition and re-raise, so the process dies from
    // the original signal with the faulting frame intact.
    libc::signal(sig, libc::SIG_DFL);
    libc::raise(sig);
}

fn run() -> Result<()> {
    let cli = Cli::parse();
    // Startup diagnostics go to stderr, which the parity harness compares
    // against an oracle's. --quiet has to reach them.
    ash_core::native_lib::set_quiet(cli.quiet);

    let hl_path = cli.file.unwrap_or_else(|| {
        let mut cwd = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        cwd.push("../../crates/ash/test/test.hl");
        cwd
    });

    if !hl_path.exists() {
        anyhow::bail!("Bytecode file not found: {}", hl_path.display());
    }

    // Hand the program its argv before any mode runs. The runtime side
    // (hlp_sys_init in ash_std) has existed for a while with no caller, so
    // Sys.args() answered from nothing. The linkage choice and init are
    // idempotent (Once-guarded), so doing them here is safe for the jit
    // path, which repeats them inside JITModule::new.
    {
        ash_core::native_lib::choose_std_linkage(&hl_path);
        init_std_library()?;
        let addr = ash_core::native_lib::std_symbol_addr("hlp_sys_init")
            .ok_or_else(|| anyhow::anyhow!("hlp_sys_init not found in ash_std"))?;
        type SysInit = unsafe extern "C" fn(*mut *mut u8, i32, *mut u8);
        let sys_init: SysInit = unsafe { std::mem::transmute(addr) };
        // NUL-terminated UTF-8, the pchar contract sys.rs documents.
        let mut bufs: Vec<Vec<u8>> = cli
            .program_args
            .iter()
            .map(|a| {
                let mut b = a.as_bytes().to_vec();
                b.push(0);
                b
            })
            .collect();
        let mut ptrs: Vec<*mut u8> = bufs.iter_mut().map(|b| b.as_mut_ptr()).collect();
        let mut file_buf = hl_path.to_string_lossy().as_bytes().to_vec();
        file_buf.push(0);
        unsafe {
            sys_init(ptrs.as_mut_ptr(), ptrs.len() as i32, file_buf.as_mut_ptr());
        }
        // hlp_sys_init copies everything out, so the temporaries may drop.
    }

    // The whole-module JIT is its own world: it decodes, initializes the
    // stdlib and compiles inside `run_whole_module`, exactly as the old
    // standalone `ash` binary did — so it branches off before the
    // interpreter-oriented prep below.
    if matches!(cli.mode, Mode::Jit) {
        return ash_core::jit::run_whole_module(&hl_path);
    }

    {
        let _p = ash_core::profile::scope("init stdlib");
        // Before decode: the decoder calls into ash_std itself.
        ash_core::native_lib::choose_std_linkage(&hl_path);
        init_std_library()?;
    }

    let bytecode = {
        let _p = ash_core::profile::scope("decode bytecode");
        BytecodeDecoder::decode(&hl_path)?
    };
    let mut native_resolver = NativeFunctionResolver::new();

    // Give the profiler findex → name so sampled JIT frames and hot
    // interpreted functions print as names. Built once, only when profiling.
    if ash_core::profile::enabled() {
        let mut names: std::collections::HashMap<u32, String> = std::collections::HashMap::new();
        for f in &bytecode.functions {
            names.insert(f.findex as u32, f.name().to_string());
        }
        for n in &bytecode.natives {
            names.insert(n.findex as u32, format!("{}@{}", n.lib, n.name));
        }
        ash_core::profile::set_name_resolver(move |fx| names.get(&fx).cloned());
    }

    // Discover and load external HDLL libraries from the .hl file's directory
    let search_dir = hl_path
        .parent()
        .unwrap_or_else(|| std::path::Path::new("."));
    {
        let _p = ash_core::profile::scope("load hdlls");
        native_resolver.discover_and_load_libraries(search_dir, &bytecode.natives)?;
    }

    // Debug: dump type info for Heaps investigation
    if std::env::var("ASH_DUMP_TYPES").is_ok() {
        eprintln!(
            "=== Bytecode: {} types, {} globals, {} functions, {} natives ===",
            bytecode.types.len(),
            bytecode.globals.len(),
            bytecode.functions.len(),
            bytecode.natives.len()
        );
        for (i, t) in bytecode.types.iter().enumerate() {
            if i < 50 || i == 39 || i == 203 {
                let name = t.obj.as_ref().map(|o| o.name.as_str()).unwrap_or("");
                let super_idx = t.obj.as_ref().and_then(|o| o.super_.as_ref()).map(|s| s.0);
                let field_names: Vec<_> = t
                    .obj
                    .as_ref()
                    .map(|o| {
                        o.fields
                            .iter()
                            .map(|f| format!("{}(k={})", f.name, bytecode.types[f.type_.0].kind))
                            .collect()
                    })
                    .unwrap_or_default();
                let vname = t
                    .virt
                    .as_ref()
                    .map(|v| format!("virt({} fields)", v.fields.len()))
                    .unwrap_or_default();
                eprintln!(
                    "  type[{}] kind={} nfields={} super={:?} name={} fields={:?} {}",
                    i,
                    t.kind,
                    t.obj.as_ref().map(|o| o.fields.len()).unwrap_or(0),
                    super_idx,
                    name,
                    field_names,
                    vname
                );
            }
        }
        if bytecode.globals.len() > 58 {
            eprintln!("  global[58] type_idx={}", bytecode.globals[58].0);
        }
        // Find Fun_5483 and Fun_2360
        for f in &bytecode.functions {
            if f.name() == "Fun_5483"
                || f.findex == 5483
                || f.name() == "Fun_2360"
                || f.findex == 2360
            {
                eprintln!(
                    "  Fun_5483: findex={} type_idx={} nregs={}",
                    f.findex,
                    f.type_.0,
                    f.regs.len()
                );
                for (ri, reg) in f.regs.iter().enumerate().take(16) {
                    eprintln!(
                        "    r{}: type_idx={} kind={}",
                        ri, reg.0, bytecode.types[reg.0].kind
                    );
                }
            }
        }
        // Dump natives around findex 2250
        for n in &bytecode.natives {
            if n.findex >= 2245 && n.findex <= 2260 {
                eprintln!("  native findex={} lib={} name={}", n.findex, n.lib, n.name);
            }
        }
        std::process::exit(0);
    }

    // Cross-check the compile-time field-offset oracle against the runtime
    // before running anything. A disagreement would be silent corruption in
    // compiled code, so it is worth being able to ask on any program.
    //
    // `ASH_VERIFY_LAYOUT=only` checks and exits, which is how the whole test
    // corpus can be swept — several of those programs take minutes to
    // interpret, and running them proves nothing about the type graph that
    // decoding them has not already proved.
    match std::env::var("ASH_VERIFY_LAYOUT").ok().as_deref() {
        None | Some("") | Some("0") => {}
        Some(mode) => {
            let interpreter = HLInterpreter::new(&bytecode, &native_resolver);
            let mismatches = interpreter.verify_layout_oracle(&bytecode, &native_resolver)?;
            let objects = bytecode
                .types
                .iter()
                .filter(|t| {
                    t.kind == ash_core::hl_bindings::hl_type_kind_HOBJ
                        || t.kind == ash_core::hl_bindings::hl_type_kind_HSTRUCT
                })
                .count();
            if mismatches.is_empty() {
                eprintln!(
                    "[layout] oracle agrees with hlp_get_obj_rt across {objects} object types"
                );
            } else {
                for m in &mismatches {
                    eprintln!(
                        "[layout] MISMATCH type[{}] {}: {}",
                        m.type_index, m.name, m.detail
                    );
                }
                anyhow::bail!("{} layout mismatches; refusing to run", mismatches.len());
            }
            if mode == "only" {
                return Ok(());
            }
        }
    }

    // `ASH_VERIFY_AIR=only` pushes every function through the AIR v2 pipeline
    // and reports what round-trips; `ASH_VERIFY_AIR=dump:<findex>` prints one
    // function's IR before and after optimization, which is how a pass that
    // fires zero times gets diagnosed.
    if let Ok(mode) = std::env::var("ASH_VERIFY_AIR") {
        if !mode.is_empty() && mode != "0" {
            let level = match std::env::var("ASH_AIR_LEVEL").ok().as_deref() {
                Some("O0") => ash_core::air_pipeline::AirOptLevel::O0,
                Some("O1") => ash_core::air_pipeline::AirOptLevel::O1,
                Some("O3") => ash_core::air_pipeline::AirOptLevel::O3,
                _ => ash_core::air_pipeline::AirOptLevel::O2,
            };
            let opts = ash_core::air_pipeline::AirPassOptions::default();
            if let Some(want) = mode
                .strip_prefix("dump:")
                .and_then(|s| s.parse::<i32>().ok())
            {
                for line in ash_core::air_pipeline::dump(&bytecode, want, level, &opts) {
                    eprintln!("[air] {line}");
                }
                return Ok(());
            }
            for line in ash_core::air_pipeline::report(&bytecode, level, &opts) {
                eprintln!("[air] {line}");
            }
            if mode == "only" {
                return Ok(());
            }
        }
    }

    // `ASH_VERIFY_TRAPS=only` reports where exception handlers are active and
    // how many call sites an explicit-edge lowering would have to check. The
    // handler map comes from AIR v2's Block::handler, which derives it by
    // dataflow over the CFG — the only form that resolves a region with more
    // than one normal exit.
    if let Ok(mode) = std::env::var("ASH_VERIFY_TRAPS") {
        if !mode.is_empty() && mode != "0" {
            let level = match std::env::var("ASH_AIR_LEVEL").ok().as_deref() {
                Some("O0") => ash_core::air_pipeline::AirOptLevel::O0,
                Some("O1") => ash_core::air_pipeline::AirOptLevel::O1,
                Some("O3") => ash_core::air_pipeline::AirOptLevel::O3,
                _ => ash_core::air_pipeline::AirOptLevel::O2,
            };
            let (funcs, sites, covered) = ash_core::air_pipeline::trap_report(&bytecode, level);
            eprintln!("[traps] {funcs} functions have a block under a handler");
            eprintln!(
                "[traps] {sites} may-throw sites, {covered} inside a handler ({:.1}%)",
                if sites == 0 {
                    0.0
                } else {
                    covered as f64 * 100.0 / sites as f64
                }
            );
            if mode == "only" {
                return Ok(());
            }
        }
    }

    if let Ok(mode) = std::env::var("ASH_ESCAPE") {
        if !mode.is_empty() && mode != "0" {
            let level = match std::env::var("ASH_AIR_LEVEL").ok().as_deref() {
                Some("O0") => ash_core::air_pipeline::AirOptLevel::O0,
                Some("O1") => ash_core::air_pipeline::AirOptLevel::O1,
                Some("O3") => ash_core::air_pipeline::AirOptLevel::O3,
                _ => ash_core::air_pipeline::AirOptLevel::O2,
            };
            for line in ash_core::air_pipeline::escape_report(&bytecode, level) {
                eprintln!("[escape] {line}");
            }
            if mode == "only" {
                return Ok(());
            }
        }
    }

    // `ASH_REACH=only` reports how much of the module can actually be entered.
    // A `.hl` links the whole stdlib, so any sweep that iterates bc.functions
    // is mostly measuring code that cannot run.
    if let Ok(mode) = std::env::var("ASH_REACH") {
        if !mode.is_empty() && mode != "0" {
            for line in ash_core::reachable::report(&bytecode) {
                eprintln!("[reach] {line}");
            }
            if let Some(fx) = std::env::var("ASH_REACH_WHY")
                .ok()
                .and_then(|v| v.trim().parse::<i32>().ok())
            {
                for line in ash_core::reachable::why(&bytecode, fx) {
                    eprintln!("[reach] why({fx}) {line}");
                }
            }
            if mode == "only" {
                return Ok(());
            }
        }
    }

    // `ASH_VERIFY_OSR=only` reports which loops on-stack replacement would
    // accept. Computed over AIR, so loop discovery, dominance and the
    // address-escape question all come from the IR that already models them.
    if let Ok(mode) = std::env::var("ASH_VERIFY_OSR") {
        if !mode.is_empty() && mode != "0" {
            let level = match std::env::var("ASH_AIR_LEVEL").ok().as_deref() {
                Some("O0") => ash_core::air_pipeline::AirOptLevel::O0,
                Some("O1") => ash_core::air_pipeline::AirOptLevel::O1,
                Some("O3") => ash_core::air_pipeline::AirOptLevel::O3,
                _ => ash_core::air_pipeline::AirOptLevel::O2,
            };
            for line in ash_core::air_pipeline::osr_report(&bytecode, level) {
                eprintln!("[osr] {line}");
            }
            if mode == "only" {
                return Ok(());
            }
        }
    }

    match cli.mode {
        Mode::Interp => {
            let mut interpreter = HLInterpreter::new(&bytecode, &native_resolver);
            let result = {
                let _p = ash_core::profile::scope("run");
                interpreter.execute_entrypoint(&bytecode, &native_resolver)?
            };
            if !cli.quiet {
                eprintln!("Interpreter returned: {:?}", result);
            }
        }
        Mode::Hybrid => {
            // --jit-tier wins; ASH_TIER is the env fallback.
            let tier_spec = cli
                .jit_tier
                .clone()
                .or_else(|| std::env::var("ASH_TIER").ok());
            let tier_mode = match tier_spec {
                Some(s) => match TierMode::parse(&s) {
                    Some(m) => m,
                    None => anyhow::bail!(
                        "invalid --jit-tier/ASH_TIER value '{}' (expected auto|cranelift|llvm|off)",
                        s
                    ),
                },
                None => TierMode::default(),
            };
            let mut interpreter = HLInterpreter::new(&bytecode, &native_resolver);
            let cfg = TieredConfig {
                enabled: true,
                jit_threshold: cli.jit_threshold,
                max_jit_args: cli.jit_max_args,
                min_ops_for_promotion: cli.jit_min_ops,
                log_promotions: cli.jit_log,
                strict_mode: true,
                hot_reload: cli.hot_reload,
                tier_mode,
            };
            {
                let _p = ash_core::profile::scope("tiered prewarm");
                interpreter.enable_tiered(&hl_path, &native_resolver, cfg)?;
            }
            let result = {
                let _p = ash_core::profile::scope("run");
                interpreter.execute_entrypoint(&bytecode, &native_resolver)?
            };
            // Any tier-chase thread still compiling touches the shared JIT
            // module; letting one outlive this scope is a use-after-free.
            ash_interp::interpreter::retier_chase_join();
            if let Some(stats) = interpreter.tiered_stats() {
                if cli.jit_log {
                    eprintln!(
                        "[tiered] attempted={} succeeded={} failed={} compiled_calls={} fallbacks={} cranelift={} llvm={}",
                        stats.attempted_promotions,
                        stats.successful_promotions,
                        stats.failed_promotions,
                        stats.compiled_calls,
                        stats.fallback_calls,
                        stats.cranelift_promotions,
                        stats.llvm_promotions
                    );
                }
            }
            if !cli.quiet {
                eprintln!("Interpreter returned: {:?}", result);
            }
        }
        Mode::Jit => unreachable!("handled before the interpreter prep"),
    }

    Ok(())
}
