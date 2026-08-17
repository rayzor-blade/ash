use anyhow::Result;
use ash::bytecode::BytecodeDecoder;
use ash::native_lib::{init_std_library, NativeFunctionResolver};
use ash_interp::interpreter::{HLInterpreter, TierMode, TieredConfig};
use clap::{Parser, ValueEnum};
use std::path::PathBuf;
use std::process;
use std::sync::OnceLock;

#[derive(Parser)]
#[command(name = "ash_cli", about = "ASH - HashLink VM Interpreter")]
struct Cli {
    /// Path to a HashLink bytecode (.hl) file
    file: Option<PathBuf>,

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
static CRASH_BACKTRACE: OnceLock<bool> = OnceLock::new();

fn main() {
    // Resolve the crash-time options before installing the handler, so the
    // handler itself never touches the environment (getenv is not
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

    if let Err(e) = run() {
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

#[cfg(not(any(target_os = "macos", target_os = "ios")))]
unsafe fn errno() -> i32 {
    *libc::__errno_location()
}

/// Write a byte slice straight to fd 2, retrying short writes and `EINTR`.
///
/// `write(2)` is async-signal-safe; `eprintln!` is not (it takes a lock and
/// the formatting machinery can allocate).
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

#[cfg(not(all(target_os = "macos", target_arch = "aarch64")))]
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

    let hl_path = cli.file.unwrap_or_else(|| {
        let mut cwd = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        cwd.push("../../crates/ash/test/test.hl");
        cwd
    });

    if !hl_path.exists() {
        anyhow::bail!("Bytecode file not found: {}", hl_path.display());
    }

    init_std_library()?;

    let bytecode = BytecodeDecoder::decode(&hl_path)?;
    let mut native_resolver = NativeFunctionResolver::new();

    // Discover and load external HDLL libraries from the .hl file's directory
    let search_dir = hl_path
        .parent()
        .unwrap_or_else(|| std::path::Path::new("."));
    native_resolver.discover_and_load_libraries(search_dir, &bytecode.natives)?;

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

    match cli.mode {
        Mode::Interp => {
            let mut interpreter = HLInterpreter::new(&bytecode, &native_resolver);
            let result = interpreter.execute_entrypoint(&bytecode, &native_resolver)?;
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
            interpreter.enable_tiered(&hl_path, &native_resolver, cfg)?;
            let result = interpreter.execute_entrypoint(&bytecode, &native_resolver)?;
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
        Mode::Jit => {
            if !cli.quiet {
                eprintln!("JIT-only mode not yet fully implemented, falling back to interpreter");
            }
            let mut interpreter = HLInterpreter::new(&bytecode, &native_resolver);
            let result = interpreter.execute_entrypoint(&bytecode, &native_resolver)?;
            if !cli.quiet {
                eprintln!("Interpreter returned: {:?}", result);
            }
        }
    }

    Ok(())
}
