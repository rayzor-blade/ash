use anyhow::Result;
use ash_core::bytecode::BytecodeDecoder;
use ash_core::native_lib::{init_std_library, NativeFunctionResolver};
use ash_interp::interpreter::{HLInterpreter, TierMode, TierPreset, TieredConfig};
use clap::{ArgGroup, Parser, ValueEnum};
use std::path::PathBuf;
// Only the unix crash handler reads a OnceLock (the ASH_CRASH_BACKTRACE
// latch); on Windows there is no handler and no latch.
#[cfg(unix)]
use std::sync::OnceLock;

/// What `ash` can do besides run bytecode.
#[derive(clap::Subcommand)]
enum Command {
    /// Read a WebAssembly module: what it is, and what it still needs.
    ///
    /// With no flag it reports. `--validate` answers one question instead:
    /// is this a module a host could run, exiting non-zero when it is not,
    /// which is what a build gate wants.
    Wasm {
        /// The `.wasm` module, or a relocatable object.
        module: PathBuf,
        /// Answer runnable-or-not, and fail if not.
        #[arg(long)]
        validate: bool,
        /// The full report. The default when neither flag is given.
        #[arg(long, visible_alias = "analyze")]
        analyse: bool,
    },
}

#[derive(Parser)]
#[command(
    name = "ash",
    about = "ASH - HashLink bytecode runtime (interp | hybrid | jit)"
)]
#[command(group(
    ArgGroup::new("aot_output")
        .args(["emit_aot", "build"])
        .multiple(true)
))]
struct Cli {
    /// Read a WebAssembly module instead of running bytecode.
    #[command(subcommand)]
    command: Option<Command>,

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

    /// Compile to a native object file instead of running.
    ///
    /// The object still has to be linked; `--build` does both and is what
    /// most callers want.
    #[arg(long, value_name = "OUT.o")]
    emit_aot: Option<PathBuf>,

    /// Compile the program to a native binary: emit and link, in one step.
    ///
    /// The usual way to build. `--emit-aot` is the same compile stopping at
    /// the object file, for a caller doing its own linking.
    #[arg(long, value_name = "OUT")]
    build: Option<PathBuf>,

    /// Runtime to link against (--build).
    ///
    /// By default the one beside `ash`, then the usual library directories,
    /// and for a program that loads HDLLs, failing all that, the copy `ash`
    /// carries inside itself.
    #[arg(long, value_name = "PATH", requires = "build")]
    runtime: Option<PathBuf>,

    /// Target triple for --build/--emit-aot. Defaults to this machine.
    ///
    /// A non-native triple is cross-compiled for a generic CPU, because the
    /// host's features cannot be assumed of a machine we cannot ask.
    #[arg(long, value_name = "TRIPLE")]
    target: Option<String>,

    /// Emit even when some functions could not be lowered (--build/--emit-aot).
    ///
    /// A refused function becomes a throw. Without this, emitting stops rather
    /// than writing a binary that aborts the moment one is reached.
    #[arg(long)]
    allow_refused: bool,

    /// Devirtualise from a callsite profile when emitting (--build/--emit-aot).
    ///
    /// With no value, reads <file>.prof beside the bytecode. Produce one by
    /// running the program once with ASH_AOT_PROFILE_OUT set. The profile is
    /// advisory: every guard it produces re-checks its target at run time, so
    /// a stale one costs a compare and never an answer.
    /// A value must be attached with `=`. Without that, `--pgo prog.hl` reads
    /// as "the profile is prog.hl" and the program argument disappears, which
    /// is a silent wrong answer rather than an error.
    #[arg(long, value_name = "PROFILE", num_args = 0..=1, require_equals = true,
          default_missing_value = "", requires = "aot_output")]
    pgo: Option<String>,

    /// Hot-call threshold for Cranelift promotion in hybrid mode
    #[arg(long, default_value_t = 100)]
    jit_threshold: u64,

    /// Threshold preset for the program's shape.
    ///
    /// Explicit --jit-threshold / --opt-threshold override the preset.
    #[arg(long, value_enum)]
    preset: Option<Preset>,

    /// Invocations before promoting to the optimising tier in hybrid mode.
    ///
    /// Reached by INTERPRETED calls only, so a value far above
    /// --jit-threshold is unreachable once a function's callers compile.
    #[arg(long, default_value_t = 1_000)]
    opt_threshold: u64,

    /// Enable tiered runtime promotion logs
    #[arg(long, default_value_t = false)]
    jit_log: bool,

    /// Max argument count for promoted calls. Wider signatures are reached
    /// through a backend-emitted uniform entry, which is not yet proven on
    /// real signatures — raise this to exercise it.
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

    /// Run the AIR pipeline over every function and write the result as a
    /// HashLink bytecode file, then exit without running the program.
    ///
    /// The output is an ordinary `.hl` that stock `hl`, HL/C or hl2 will run —
    /// ash is used here as an optimizer rather than a runtime. Debug info is
    /// not carried across: the decoder drops the `assigns` table, and after
    /// inlining a pc->line mapping describes a function the source no longer
    /// has. Use --air-level to choose how much optimization runs.
    #[arg(long, value_name = "PATH")]
    emit_optimized: Option<std::path::PathBuf>,

    /// Which rungs of the interpreter -> Cranelift -> LLVM ladder to use.
    /// `auto` (default) runs both JIT tiers; `cranelift` and `llvm` pin a
    /// single tier for testing; `off` disables promotion. Overridden by the
    /// ASH_TIER environment variable when this flag is left at its default.
    #[arg(long, value_name = "auto|cranelift|llvm|off")]
    jit_tier: Option<String>,
}

#[derive(Clone, Copy, ValueEnum)]
enum Mode {
    /// Run using the bytecode interpreter
    Interp,
    /// Compile reached functions with Cranelift, then promote them to LLVM
    Jit,
    /// Hybrid mode (interpreter with JIT tier promotion)
    Hybrid,
}

/// The named presets of [`TierPreset`], as a command-line value.
///
/// This list is not free-standing: `every_preset_is_offered` below matches
/// exhaustively over `TierPreset`, so a preset added there stops compiling
/// until it is offered here too. That is deliberate. `game` existed in
/// `TierPreset` and in `TierPreset::names()` for some time while `--preset`
/// took a bare String and documented its own hand-written list, so the one
/// preset shaped for a frame budget was unreachable to anyone reading --help.
#[derive(Clone, Copy, ValueEnum)]
enum Preset {
    /// Short-lived programs: promote early, optimise late
    Script,
    /// General applications: the default shape
    #[value(alias = "app")]
    Application,
    /// Frame budgets: compile during load, not during play
    Game,
    /// Long-lived processes, where compile cost amortises
    Server,
    /// Drive everything through every tier; slower on short programs
    #[value(alias = "bench")]
    Benchmark,
    /// Favours iteration over peak performance
    #[value(alias = "dev")]
    Development,
    /// No JIT at all
    #[value(alias = "interp", alias = "none")]
    Interpreter,
}

impl Preset {
    fn to_tier(self) -> TierPreset {
        match self {
            Preset::Script => TierPreset::Script,
            Preset::Application => TierPreset::Application,
            Preset::Game => TierPreset::Game,
            Preset::Server => TierPreset::Server,
            Preset::Benchmark => TierPreset::Benchmark,
            Preset::Development => TierPreset::Development,
            Preset::Interpreter => TierPreset::Interpreter,
        }
    }
}

/// Compile-time guard, never called: adding a named variant to `TierPreset`
/// fails this match until `Preset` offers it on the command line.
#[allow(dead_code)]
fn every_preset_is_offered(t: TierPreset) -> Option<Preset> {
    match t {
        TierPreset::Script => Some(Preset::Script),
        TierPreset::Application => Some(Preset::Application),
        TierPreset::Game => Some(Preset::Game),
        TierPreset::Server => Some(Preset::Server),
        TierPreset::Benchmark => Some(Preset::Benchmark),
        TierPreset::Development => Some(Preset::Development),
        TierPreset::Interpreter => Some(Preset::Interpreter),
        // Built directly from a TieredConfig; it has no command-line spelling.
        TierPreset::Custom(_) => None,
    }
}

/// Compile the bytecode to a native object instead of running it.
///
/// The same lowering the JIT uses, stopped one step earlier: everything up to
/// the middle end is target-independent, and only the tail differs -- an
/// address to jump to for the JIT, a file for this. The object defines `main`
/// and `ash_module_init` and imports the runtime by symbol, so linking it
/// against libash_std.a produces a binary with no bytecode in it.
/// What to build, and where to put it.
struct AotRequest<'a> {
    /// The bytecode to compile.
    file: &'a std::path::Path,
    /// The object file. Scratch, and removed, when `exe` is set.
    out: &'a std::path::Path,
    /// The binary to link, if this is a one-command build.
    exe: Option<&'a std::path::Path>,
    /// The runtime to link against, if the caller named one.
    runtime: Option<&'a std::path::Path>,
    /// Target triple; this machine's if absent.
    target: Option<String>,
    /// Call-site profile to devirtualize from.
    pgo: Option<String>,
    /// Emit even if some functions could not be lowered.
    allow_refused: bool,
    quiet: bool,
}

fn emit_aot(request: AotRequest<'_>) -> anyhow::Result<()> {
    let AotRequest {
        file,
        out,
        exe,
        runtime,
        target,
        pgo,
        allow_refused,
        quiet,
    } = request;
    // A profile, if one was asked for. Advisory: a stale file costs a compare.
    if let Some(pgo) = &pgo {
        let path = if pgo.is_empty() {
            file.with_extension("prof")
        } else {
            PathBuf::from(pgo)
        };
        match std::fs::read_to_string(&path) {
            Ok(text) => {
                let n = ash_core::callsite_profile::load_profile(&text);
                if !quiet {
                    eprintln!("[ash] pgo: loaded {n} caller(s) from {}", path.display());
                }
            }
            Err(e) => eprintln!("[ash] pgo: could not read {}: {e}", path.display()),
        }
    }

    let triple = target.unwrap_or_else(|| {
        inkwell::targets::TargetMachine::get_default_triple()
            .as_str()
            .to_string_lossy()
            .into_owned()
    });

    // Leaked on purpose: the module borrows the context for as long as it
    // exists, and this one lives until the process ends.
    let context: &'static inkwell::context::Context =
        Box::leak(Box::new(inkwell::context::Context::create()));
    let mut jit =
        ash_core::llvm::module::JITModule::new_aot_for_target(context, file, &triple)?;

    let findexes: Vec<usize> = jit
        .bytecode_functions()
        .iter()
        .map(|f| f.findex as usize)
        .collect();
    // A build is a minute of silence otherwise, and silence is
    // indistinguishable from a hang.
    ash_core::progress::enable(!quiet);
    ash_core::progress::begin("lowering", findexes.len() as u64);
    let (mut lowered, mut refused) = (0usize, Vec::new());
    // Per-function timing, on the same terms the tier log reports a JIT
    // compile. Without it the only cost figure AOT produces is the total, and a
    // single pathological function is invisible inside it: one measured at 47s
    // in the JIT could not be attributed here at all, and a whole-emit A/B
    // (O1 274s vs O2 298s over 8577 functions) cannot separate one outlier
    // from a broad shift. `ASH_AOT_SLOW_MS` sets the threshold.
    let slow_ms: u128 = std::env::var("ASH_AOT_SLOW_MS")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(500);
    let mut slowest: Vec<(u128, usize)> = Vec::new();
    for fx in &findexes {
        let began = std::time::Instant::now();
        let outcome = jit.promote_function_strict(*fx);
        let took = began.elapsed().as_millis();
        ash_core::progress::advance(1);
        if took >= slow_ms {
            ash_core::progress::detail(&format!("[aot] findex={fx} took {took}ms"));
        }
        slowest.push((took, *fx));
        match outcome {
            Ok(_) => lowered += 1,
            Err(e) => refused.push((*fx, format!("{e}"))),
        }
    }
    slowest.sort_unstable_by_key(|&(t, _)| std::cmp::Reverse(t));
    let total: u128 = slowest.iter().map(|(t, _)| t).sum();
    ash_core::progress::detail(&format!("[aot] lowering {}ms total; slowest:", total));
    for (took, fx) in slowest.iter().take(5) {
        ash_core::progress::detail(&format!(
            "[aot]   findex={fx} {took}ms ({:.1}% of lowering)",
            100.0 * *took as f64 / total.max(1) as f64
        ));
    }

    // A refused function is not a warning: it lowers to a throw, so a binary
    // containing one aborts the moment that path is reached -- and exiting 0
    // after writing it means the failure surfaces at run time, on a machine
    // that may not be this one. The commonest cause is an HDLL primitive,
    // which resolves through dlopen and has no symbol to bind ahead of time.
    if !refused.is_empty() && !allow_refused {
        let mut msg = format!(
            "{} function(s) could not be lowered; refusing to write a binary \
             that would abort when one is reached:",
            refused.len()
        );
        for (fx, e) in refused.iter().take(10) {
            msg.push_str(&format!(
                "\n    findex={fx}: {}",
                e.lines().next().unwrap_or("")
            ));
        }
        if refused.len() > 10 {
            msg.push_str(&format!("\n    ... and {} more", refused.len() - 10));
        }
        msg.push_str("\n  --allow-refused emits anyway, if those paths are never taken.");
        anyhow::bail!(msg);
    }

    jit.finalize_aot_data()?;
    jit.emit_late_init()?;
    jit.emit_main()?;
    let shared_runtime = jit.aot_needs_shared_runtime();
    // More than one shard splits the middle end and codegen across threads
    // (see `aot_shard`); one shard is the single-module path, which also
    // honours ASH_AOT_NO_OPT and ASH_AOT_DUMP_IR itself.
    let shards = ash_core::llvm::aot_shard::shard_count_for(&triple);
    if !quiet && shards == 1 && ash_core::llvm::aot_shard::shard_count() > 1 {
        eprintln!(
            "[aot] cross-compiling to {triple}: one module, since `ld -r` joins the shards and reads the host's object format only"
        );
    }
    // Heading for a binary, the parts go straight to the linker: it takes any
    // number of objects, so joining them into one first would rewrite every
    // byte for nothing.
    let mut objects: Vec<PathBuf> = vec![out.to_path_buf()];
    let bytes = if shards > 1 && exe.is_some() {
        let parts = jit.emit_object_parts(&triple, out, shards, quiet)?;
        let bytes = parts
            .iter()
            .filter_map(|p| std::fs::metadata(p).ok().map(|m| m.len()))
            .sum();
        objects = parts;
        bytes
    } else if shards > 1 {
        jit.emit_object_sharded(&triple, out, shards, quiet)?
    } else {
        // ASH_AOT_NO_OPT leaves the module unoptimised. O3 inlines
        // ash_module_init straight into main, so a fault during startup
        // surfaces with no frame naming the routine it happened in -- which
        // is exactly when you most want one.
        if std::env::var("ASH_AOT_NO_OPT").is_err() {
            let began = std::time::Instant::now();
            jit.optimize_module()?;
            if !quiet {
                ash_core::progress::detail(&format!(
                    "[aot] middle end {}ms",
                    began.elapsed().as_millis()
                ));
            }
        }
        // ASH_AOT_DUMP_IR=<path> writes the module beside the object. Reading
        // the IR is how every AOT defect in this file was actually found;
        // making it reachable only from an example was a false economy.
        if let Ok(dir) = std::env::var("ASH_AOT_DUMP_IR") {
            let p = std::path::Path::new(&dir);
            let dest = if p.is_dir() {
                p.join("module.ll")
            } else {
                p.to_path_buf()
            };
            jit.write_ir(&dest)?;
            if !quiet {
                ash_core::progress::note(&format!("[ash] wrote IR to {}", dest.display()));
            }
        }
        let began = std::time::Instant::now();
        let bytes = jit.emit_object(&triple, out)?;
        if !quiet {
            ash_core::progress::detail(&format!("[aot] codegen {}ms", began.elapsed().as_millis()));
        }
        bytes
    };

    // The stages are over; what follows is the summary, and a bar under it
    // would be a finished stage left on screen.
    ash_core::progress::finish();
    if !quiet {
        ash_core::progress::note(&format!(
            "[ash] lowered {lowered}/{} functions, {} refused",
            findexes.len(),
            refused.len()
        ));
        for (fx, e) in refused.iter().take(5) {
            ash_core::progress::note(&format!(
                "[ash]   findex={fx}: {}",
                e.lines().next().unwrap_or("")
            ));
        }
        if pgo.is_some() {
            let loaded = ash_core::callsite_profile::aot_profile_size();
            let hits = ash_core::callsite_profile::aot_profile_hits();
            if loaded > 0 && hits == 0 {
                ash_core::progress::note(&format!(
                    "[ash] pgo: WARNING none of the {loaded} profiled caller(s) matched \
                     this bytecode -- the profile is stale, regenerate it"
                ));
            } else if loaded > 0 {
                ash_core::progress::note(&format!(
                    "[ash] pgo: {hits} of {loaded} profiled caller(s) matched"
                ));
            }
        }
        if exe.is_some() {
            ash_core::progress::note(&format!(
                "[ash] compiled {} object bytes for {triple}",
                bytes
            ));
        } else {
            ash_core::progress::note(&format!(
                "[ash] wrote {} ({bytes} bytes) for {triple}",
                out.display()
            ));
        }
        if shared_runtime {
            // An HDLL brings its own copy of the runtime unless the binary
            // shares one, and two collectors in a process crash as soon as one
            // meets the other's objects. So this object must take the runtime
            // as a library, and the HDLLs must sit beside the binary.
            ash_core::progress::note(
                "[ash] this program loads HDLLs, so it takes the SHARED runtime, \
                 staged beside the binary. The .hdll files must sit there too. \
                 Do not link the static runtime into a program that loads HDLLs: \
                 it builds, and then the HDLL loads a second copy of the runtime \
                 and the two collectors meet."
            );
        } else if exe.is_none() {
            ash_core::progress::note(&format!(
                "[ash] this is an object; `--build {}` would have linked it too",
                out.with_extension("").display()
            ));
        }
    }

    if let Some(exe) = exe {
        let kind = if shared_runtime {
            ash_core::llvm::aot_link::Runtime::Shared
        } else {
            ash_core::llvm::aot_link::Runtime::Static
        };
        let linked = ash_core::llvm::aot_link::link_executable(&objects, exe, kind, runtime, quiet);
        // The objects are scratch either way. Keeping them after a failure
        // only helps if someone is told they exist.
        if linked.is_err() {
            ash_core::progress::note(&format!(
                "[ash] the objects are left beside {} for inspection",
                exe.display()
            ));
        } else {
            for part in &objects {
                let _ = std::fs::remove_file(part);
            }
        }
        linked?;
    }
    Ok(())
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
    ash_core::profile::report_on_termination();

    let result = run();
    ash_core::profile::report();
    let code = if let Err(e) = result {
        // An uncaught HL exception is the PROGRAM's failure and already reads
        // as HashLink prints it ("Uncaught exception: ..." plus its stack);
        // prefixing it with "Error:" would frame it as an ash malfunction.
        let text = format!("{e:#}");
        if text.starts_with("Uncaught exception:") {
            eprintln!("{text}");
        } else {
            eprintln!("Error: {text}");
        }
        1
    } else {
        0
    };
    exit_without_atexit(code);
}

/// Leave without running the process's `atexit` handlers.
///
/// LLVM registers its own, and they tear down JIT state a promotion still
/// running on the broker is using -- a SIGSEGV in `SelectionDAGISel` against
/// `~GDBJITRegistrationListener` on the main thread. Waiting for that
/// promotion also works, but it costs whatever the compile has left to run
/// and buys nothing: the program has produced its answer and nothing can call
/// the code being compiled. deltablue paid 22ms of a 45ms run that way.
///
/// Leaving without the handlers wins the same race for free. Nothing is being
/// freed -- the interpreter is already leaked, for the raw handles the
/// brokers hold -- so a compile still in flight is simply ended with the
/// process. The buffers have to be flushed by hand, since that is one of the
/// things `exit` would have done.
#[cfg(unix)]
fn exit_without_atexit(code: i32) -> ! {
    use std::io::Write;
    // Anything the skipped handlers would have printed has to be printed
    // here instead. `ASH_GC_STATS` registers one; it is gated internally, so
    // this is a no-op when it was not asked for.
    ash_std::gc::print_stats_if_enabled();
    let _ = std::io::stdout().flush();
    let _ = std::io::stderr().flush();
    unsafe { libc::_exit(code) }
}

/// Windows counterpart to the Unix `_exit` path above.
///
/// `ExitProcess` terminates all threads and does not run the MSVC CRT's
/// `atexit` table, which is the property this shutdown path needs.  The
/// embedded ash_std DLL owns the GC state on Windows, so linking a second
/// ash_std rlib here merely to print its (different) counters would be wrong.
#[cfg(windows)]
fn exit_without_atexit(code: i32) -> ! {
    use std::io::Write;
    let _ = std::io::stdout().flush();
    let _ = std::io::stderr().flush();
    unsafe { windows_sys::Win32::System::Threading::ExitProcess(code as u32) }
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
            if let Some((findex, tier, off)) = ash_core::profile::describe_jit_pc(pc as usize) {
                push_bytes(&mut b, &mut l, b" jit findex=");
                push_dec(&mut b, &mut l, findex as u64);
                push_bytes(&mut b, &mut l, b" (");
                push_bytes(&mut b, &mut l, tier.as_bytes());
                push_bytes(&mut b, &mut l, b"+0x");
                push_hex(&mut b, &mut l, off as u64);
                push_bytes(&mut b, &mut l, b")");
                // Signal-safe: an index into a leaked table, no allocation.
                if let Some(name) = ash_core::profile::static_name(findex) {
                    push_bytes(&mut b, &mut l, b" ");
                    push_bytes(&mut b, &mut l, name.as_bytes());
                }
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
                                &mut Sink {
                                    buf: &mut b,
                                    len: &mut l,
                                },
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

/// `ash wasm`: read a module and say what it is.
///
/// Separate from the bytecode path because it shares nothing with it -- no
/// runtime, no interpreter, no LLVM. It reads a file and prints.
fn run_wasm(module: &std::path::Path, validate: bool, _analyse: bool) -> Result<()> {
    let bytes = std::fs::read(module)
        .map_err(|e| anyhow::anyhow!("reading {}: {e}", module.display()))?;
    let report = ash_wasm_runtime::validate::inspect(&bytes);

    if validate {
        if let Some(why) = &report.invalid {
            anyhow::bail!("{} is not a valid WebAssembly module: {why}", module.display());
        }
        if report.relocatable {
            anyhow::bail!(
                "{} is a relocatable object, not a module: link it first",
                module.display()
            );
        }
        let unsatisfied = report.unsatisfied();
        if !unsatisfied.is_empty() {
            let names: Vec<String> = unsatisfied
                .iter()
                .take(8)
                .map(|i| format!("{}.{}", i.module, i.name))
                .collect();
            anyhow::bail!(
                "{} imports {} symbol(s) no host can supply: {}{}",
                module.display(),
                unsatisfied.len(),
                names.join(", "),
                if unsatisfied.len() > names.len() { ", ..." } else { "" }
            );
        }
        println!("{}: runnable", module.display());
        return Ok(());
    }

    // The report. Everything a reader needs to see where the port stands.
    println!("{}", module.display());
    match &report.invalid {
        Some(why) => println!("  invalid: {why}"),
        None => println!(
            "  valid {}",
            if report.relocatable {
                "relocatable object (needs linking)"
            } else {
                "module"
            }
        ),
    }
    println!("  functions          {}", report.functions);
    println!("  indirect calls     {}", report.call_indirect_sites);
    println!("  data segments      {}", report.data_segments);
    for table in &report.tables {
        println!(
            "  table              {} x{}{}",
            table.element_type,
            table.initial,
            table
                .maximum
                .map(|m| format!(" (max {m})"))
                .unwrap_or_default()
        );
    }

    let unsatisfied = report.unsatisfied();
    let host_supplied = report.imports.len() - unsatisfied.len();
    println!(
        "  imports            {} ({host_supplied} a host supplies, {} it cannot)",
        report.imports.len(),
        unsatisfied.len()
    );
    // Grouped by module, because "72 of them are env.hlp_*" is the useful
    // shape of that list rather than 72 lines.
    let mut groups: std::collections::BTreeMap<&str, Vec<&str>> = Default::default();
    for import in &unsatisfied {
        groups
            .entry(import.module.as_str())
            .or_default()
            .push(import.name.as_str());
    }
    for (module_name, names) in &groups {
        println!("    {module_name}: {} unsatisfied", names.len());
        for name in names.iter().take(6) {
            println!("      {name}");
        }
        if names.len() > 6 {
            println!("      ... and {} more", names.len() - 6);
        }
    }
    if !report.exports.is_empty() {
        println!("  exports            {}", report.exports.join(", "));
    }
    if report.runnable() {
        println!("  -> runnable: it needs only WASI and the fiber import");
    }
    Ok(())
}

fn run() -> Result<()> {
    let cli = Cli::parse();

    if let Some(Command::Wasm {
        module,
        validate,
        analyse,
    }) = &cli.command
    {
        return run_wasm(module, *validate, *analyse);
    }

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

    {
        let _p = ash_core::profile::scope("init stdlib");
        // Before decode: the decoder calls into ash_std itself.
        ash_core::native_lib::choose_std_linkage(&hl_path);
        init_std_library()?;
    }

    // Arc, not a bare value: the tiered brokers lower from this on their own
    // threads and must own a share of it. Publishing a raw pointer instead
    // (as this used to) put the lifetime in a comment, and the comment was
    // wrong — see TieredSharedCtx::bytecode.
    let bytecode = {
        let _p = ash_core::profile::scope("decode bytecode");
        std::sync::Arc::new(BytecodeDecoder::decode(&hl_path)?)
    };

    if let Some(out) = cli.emit_optimized.clone() {
        return emit_optimized(&bytecode, &out, cli.quiet);
    }
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

    // The same mapping, flat and leaked, for the crash handler -- which cannot
    // allocate and so cannot use the resolver above. Unconditional, unlike the
    // profiler's copy: a crash does not announce itself in advance, and a
    // report that says `findex=7450` and nothing else costs whoever reads it
    // the one fact they most need. One &str per function, built once.
    ash_core::profile::set_static_names(
        bytecode
            .functions
            .iter()
            .map(|f| (f.findex as u32, f.name().to_string()))
            .chain(
                bytecode
                    .natives
                    .iter()
                    .map(|n| (n.findex as u32, format!("{}@{}", n.lib, n.name))),
            ),
    );

    // Discover and load external HDLL libraries from the .hl file's directory
    let search_dir = hl_path
        .parent()
        .unwrap_or_else(|| std::path::Path::new("."));
    {
        let _p = ash_core::profile::scope("load hdlls");
        native_resolver.discover_and_load_libraries(search_dir, &bytecode.natives)?;
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

    if cli.emit_aot.is_some() || cli.build.is_some() {
        // With only `--build`, the object is scratch: it is named after the
        // binary, beside it, and removed once linked.
        let exe = cli.build.clone();
        let out = cli.emit_aot.clone().unwrap_or_else(|| {
            let exe = exe.as_ref().expect("one of the two is set");
            let mut name = exe.file_name().unwrap_or_default().to_os_string();
            name.push(".o");
            exe.with_file_name(name)
        });
        return emit_aot(AotRequest {
            file: &hl_path,
            out: &out,
            exe: exe.as_deref(),
            runtime: cli.runtime.as_deref(),
            target: cli.target.clone(),
            pgo: cli.pgo.clone(),
            allow_refused: cli.allow_refused,
            quiet: cli.quiet,
        });
    }

    match cli.mode {
        Mode::Interp => {
            let mut interpreter = HLInterpreter::new(&bytecode, &native_resolver);
            let result = {
                let _p = ash_core::profile::scope("run");
                interpreter.execute_entrypoint(&bytecode, &native_resolver)?
            };
            for line in ash_core::air_pipeline::pass_time_report() {
                eprintln!("{line}");
            }
            if ash_core::jit_map::dump_wanted() {
                for line in ash_core::jit_map::dump(|f| {
                    ash_core::profile::static_name(f).map(str::to_owned)
                }) {
                    eprintln!("{line}");
                }
            }
            if !cli.quiet {
                eprintln!("Interpreter returned: {:?}", result);
            }
        }
        mode @ (Mode::Hybrid | Mode::Jit) => {
            let compiled_only = matches!(mode, Mode::Jit);
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
            if compiled_only && tier_mode == TierMode::Off {
                anyhow::bail!("--mode jit cannot be combined with --jit-tier=off");
            }
            let mut interpreter = HLInterpreter::new(&bytecode, &native_resolver);
            // A preset supplies the thresholds; a flag the operator actually
            // typed still wins over it.
            let preset_cfg = cli.preset.map(|p| p.to_tier().to_config());
            let arg_given = |flag: &str| {
                std::env::args().any(|a| a == flag || a.starts_with(&format!("{flag}=")))
            };
            let cfg = TieredConfig {
                enabled: true,
                compiled_only,
                jit_threshold: match &preset_cfg {
                    Some(p) if !arg_given("--jit-threshold") => p.jit_threshold,
                    _ => cli.jit_threshold,
                },
                opt_threshold: match &preset_cfg {
                    Some(p) if !arg_given("--opt-threshold") => p.opt_threshold,
                    _ => cli.opt_threshold,
                },
                max_jit_args: cli.jit_max_args,
                min_ops_for_promotion: cli.jit_min_ops,
                log_promotions: cli.jit_log,
                strict_mode: true,
                hot_reload: cli.hot_reload,
                tier_mode,
            };
            {
                let _p = ash_core::profile::scope("tiered prewarm");
                interpreter.enable_tiered(&hl_path, &native_resolver, &bytecode, cfg)?;
            }
            let result = {
                let _p = ash_core::profile::scope("run");
                interpreter.execute_entrypoint(&bytecode, &native_resolver)?
            };
            // Any tier-chase thread still compiling touches the shared JIT
            // module; letting one outlive this scope is a use-after-free.
            // Do not wait for speculative compiles the program will never
            // call. An in-flight LLVM compile cannot be interrupted, so the
            // only way not to wait is to not free what it is reading: the
            // interpreter is leaked below, deliberately, and the thread dies
            // with the process. deltablue answered at 65ms and exited at
            // ~290ms purely because of this wait.
            // Abandon without joining. The flag retires every chase that has
            // not begun its compile; an in-flight one is left to finish and
            // die with the process, because nothing it reads is freed
            // underneath it: TieredSharedCtx now OWNS the bytecode through an
            // Arc that every compile thread holds a share of.
            //
            // Two earlier attempts at this were wrong and are worth naming.
            // Not joining while the ctx published a RAW pointer to the
            // decode was a use-after-free (551347e). Leaking the decode by
            // hand fixed the first crash site and CI found the next
            // (f61799e), because the set of state a broker can reach is not
            // something a caller can enumerate. Ownership is the fix; the
            // join was the symptom-level workaround.
            interpreter.quiesce_promotions();
            // Where the AIR pipeline spent its time, when asked. Printed
            // whatever the mode, because the interpreter prepares on the
            // mutator and a pinned-interpreter run is exactly where that cost
            // is easiest to see.
            for line in ash_core::air_pipeline::pass_time_report() {
                eprintln!("{line}");
            }
            if ash_core::jit_map::dump_wanted() {
                for line in ash_core::jit_map::dump(|f| {
                    ash_core::profile::static_name(f).map(str::to_owned)
                }) {
                    eprintln!("{line}");
                }
            }
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
                    if let Some(report) = ash_interp::interpreter::decline_report() {
                        eprint!("{report}");
                    }
                    // Did the promotions repay themselves? A compile that
                    // lands after the function stops being called cost its
                    // middle end and returned nothing, which on a short
                    // program is most of what the top tier does.
                    let installs = ash_interp::interpreter::install_call_counts();
                    if !installs.is_empty() && std::env::var_os("ASH_TIER_LOG").is_some() {
                        eprintln!(
                            "[tiered] llvm installs, calls at install time ({} functions):",
                            installs.len()
                        );
                        for (findex, calls) in installs.iter().take(24) {
                            eprintln!(
                                "[tiered]   findex={findex:<6} name={:<28} calls_at_install={calls}",
                                ash_core::profile::static_name(*findex as u32).unwrap_or("?")
                            );
                        }
                    }
                }
            }
            if !cli.quiet {
                match mode {
                    Mode::Jit => eprintln!("JIT returned: {:?}", result),
                    Mode::Hybrid => eprintln!("Interpreter returned: {:?}", result),
                    Mode::Interp => unreachable!(),
                }
            }
            // Hand the ahead-of-time compiler what the interpreter watched.
            // Only these modes have a tiered runtime, and the record site is
            // gated on one, so `--mode interp` observes nothing to write.
            if let Ok(path) = std::env::var("ASH_AOT_PROFILE_OUT") {
                // Name functions by their semantic hash, not their index: an
                // index is a position, and adding an unrelated class moves it.
                let by_findex =
                    ash_core::types::function_keys(&bytecode.types, &bytecode.functions);
                let observed = ash_core::callsite_profile::monomorphic_callers();
                let unnamed: Vec<u32> = observed
                    .iter()
                    .flat_map(|&(c, t)| [c, t])
                    .filter(|fx| !by_findex.contains_key(fx))
                    .collect();
                let text =
                    ash_core::callsite_profile::render_profile(|fx| by_findex.get(&fx).cloned());
                if !unnamed.is_empty() && !cli.quiet {
                    eprintln!(
                        "[ash] {} of {} named; {} function(s) have no Class.method name \
                         and were skipped: {:?}",
                        by_findex.len(),
                        bytecode.functions.len(),
                        unnamed.len(),
                        &unnamed[..unnamed.len().min(6)]
                    );
                }
                match std::fs::write(&path, &text) {
                    Ok(()) => {
                        if !cli.quiet {
                            eprintln!(
                                "[ash] wrote {} monomorphic caller(s) to {path}",
                                text.lines().filter(|l| l.starts_with("m ")).count()
                            );
                        }
                    }
                    Err(e) => eprintln!("[ash] could not write {path}: {e}"),
                }
            }
            // The interpreter still hands the brokers raw handles for the
            // globals and functions_ptrs arrays (SharedArrayHandles), so it
            // is leaked rather than dropped while a chase may still be
            // reading. The bytecode no longer needs this — the context owns
            // it — but giving those handles the same Arc treatment is the
            // remaining half of this cleanup.
            std::mem::forget(interpreter);
        }
    }

    Ok(())
}

/// `--emit-optimized`: run the AIR pipeline over every function and write the
/// module back out as HashLink bytecode.
///
/// The AIR pipeline already produces exactly what this needs. `optimized()`
/// returns a serialized form whose `ops` and `reg_types` ARE HL opcodes and
/// register types — that is the form the interpreter walks — so writing an
/// optimized module is a matter of swapping each function's body for its
/// serialized AIR and handing the result to the encoder.
///
/// A function the pipeline refuses is left exactly as it was rather than
/// failing the whole file: an unoptimized body is still a correct body.
fn emit_optimized(
    bytecode: &ash_core::bytecode::DecodedBytecode,
    out: &std::path::Path,
    quiet: bool,
) -> anyhow::Result<()> {
    use ash_core::types::TypeRef;

    let module = ash_core::air_pipeline::AshModule::new(bytecode);
    let mut optimized_bc = bytecode.clone();

    // Nothing here executes the program: each function is lowered, optimized
    // and serialized independently, and `optimized()` is already called
    // concurrently by the tier brokers -- its cache takes a lock only to look
    // up and insert, never across the pipeline itself. So this is a parallel
    // map, and on a module of a few hundred functions it is the whole cost of
    // the command.
    //
    // `std::thread::scope` rather than a data-parallel crate: rayon is not a
    // dependency of this workspace, and a chunked scope is the same thing for
    // a map over a slice.
    enum Outcome {
        Optimized(Vec<ash_core::opcodes::Opcode>, Vec<TypeRef>),
        /// The pipeline refused this function; its original body stands.
        Refused,
        /// The optimized body cannot be encoded -- see `backward_switch`.
        Unencodable,
        /// The function takes the address of a register, and ash and stock
        /// HashLink do not agree on the optimized form -- see `takes_ref`.
        Pinned,
    }
    let threads = std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(4)
        .min(optimized_bc.functions.len().max(1));
    let chunk = optimized_bc.functions.len().div_ceil(threads.max(1));
    let outcomes: Vec<Outcome> = std::thread::scope(|scope| {
        let module = &module;
        let handles: Vec<_> = bytecode
            .functions
            .chunks(chunk.max(1))
            .map(|part| {
                scope.spawn(move || {
                    part.iter()
                        .map(|f| match ash_core::air_pipeline::optimized(module, f) {
                            Ok(o) => {
                                if f.ops.iter().any(takes_ref) {
                                    Outcome::Pinned
                                } else if o.ser.ops.iter().any(backward_switch) {
                                    Outcome::Unencodable
                                } else {
                                    Outcome::Optimized(
                                        o.ser.ops.clone(),
                                        o.ser
                                            .reg_types
                                            .iter()
                                            .map(|t| TypeRef(t.0 as usize))
                                            .collect(),
                                    )
                                }
                            }
                            Err(_) => Outcome::Refused,
                        })
                        .collect::<Vec<_>>()
                })
            })
            .collect();
        handles
            .into_iter()
            .flat_map(|h| h.join().expect("optimize worker panicked"))
            .collect()
    });

    let (mut done, mut refused, mut unencodable, mut pinned) = (0usize, 0usize, 0usize, 0usize);
    for (f, outcome) in optimized_bc.functions.iter_mut().zip(outcomes) {
        match outcome {
            Outcome::Optimized(ops, regs) => {
                f.ops = ops;
                f.regs = regs;
                // The mapping the old body carried does not describe the new
                // one, and the encoder writes no debug section regardless.
                f.debug.clear();
                done += 1;
            }
            Outcome::Refused => refused += 1,
            Outcome::Unencodable => unencodable += 1,
            Outcome::Pinned => pinned += 1,
        }
    }
    let version = 5;
    let bytes = ash_core::bytecode_encode::encode(&optimized_bc, version)?;
    std::fs::write(out, &bytes)?;
    if !quiet {
        eprintln!(
            "[emit] {} optimized, {} refused, {} kept (backward switch), \
             {} kept (ref-taken) -> {} ({} bytes, HLB v{})",
            done,
            refused,
            unencodable,
            pinned,
            out.display(),
            bytes.len(),
            version
        );
    }
    Ok(())
}

/// Whether `op` is a `Switch` the bytecode format cannot represent, because
/// one of its offsets points backwards and the format reads them unsigned.
fn backward_switch(op: &ash_core::opcodes::Opcode) -> bool {
    match op {
        ash_core::opcodes::Opcode::Switch { offsets, end, .. } => {
            *end < 0 || offsets.iter().any(|o| *o < 0)
        }
        _ => false,
    }
}

/// Whether `op` takes the address of a register.
///
/// A register whose address is taken is *pinned*: every definition of it must
/// keep the same register id, because `Unref`/`SetRef` read and write through
/// the pointer rather than the register. The AIR pipeline honours that for its
/// own consumers, but the emitted bytecode is not portable to stock HashLink
/// -- `test_ref_cells` prints `loop=9` under both ash and `hl` unoptimized,
/// and under ash optimized, yet `loop=6.75` under `hl` optimized. Until that
/// divergence is understood, a function that takes a reference keeps the body
/// it came with.
fn takes_ref(op: &ash_core::opcodes::Opcode) -> bool {
    use ash_core::opcodes::Opcode;
    matches!(
        op,
        Opcode::Ref { .. } | Opcode::RefData { .. } | Opcode::RefOffset { .. }
    )
}
