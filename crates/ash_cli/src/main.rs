use anyhow::Result;
use ash::bytecode::BytecodeDecoder;
use ash::native_lib::{init_std_library, NativeFunctionResolver};
use ash_interp::interpreter::{HLInterpreter, TieredConfig};
use clap::{Parser, ValueEnum};
use std::path::PathBuf;
use std::process;

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

fn main() {
    // Install SIGSEGV handler for debugging HDLL crashes
    unsafe {
        libc::signal(libc::SIGSEGV, crash_handler as libc::sighandler_t);
        libc::signal(libc::SIGBUS, crash_handler as libc::sighandler_t);
        libc::signal(libc::SIGABRT, crash_handler as libc::sighandler_t);
    }

    if let Err(e) = run() {
        eprintln!("Error: {:#}", e);
        process::exit(1);
    }
}

extern "C" fn crash_handler(sig: i32) {
    let name = match sig {
        libc::SIGSEGV => "SIGSEGV",
        libc::SIGBUS => "SIGBUS",
        libc::SIGABRT => "SIGABRT",
        _ => "UNKNOWN",
    };
    eprintln!("\n=== CRASH: {} (signal {}) ===", name, sig);

    // Print backtrace
    let bt = std::backtrace::Backtrace::force_capture();
    eprintln!("{}", bt);

    // Re-raise to get core dump
    unsafe {
        libc::signal(sig, libc::SIG_DFL);
        libc::raise(sig);
    }
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
    let search_dir = hl_path.parent().unwrap_or_else(|| std::path::Path::new("."));
    native_resolver.discover_and_load_libraries(search_dir, &bytecode.natives)?;

    match cli.mode {
        Mode::Interp => {
            let mut interpreter = HLInterpreter::new(&bytecode, &native_resolver);
            let result = interpreter.execute_entrypoint(&bytecode, &native_resolver)?;
            if !cli.quiet {
                eprintln!("Interpreter returned: {:?}", result);
            }
        }
        Mode::Hybrid => {
            let mut interpreter = HLInterpreter::new(&bytecode, &native_resolver);
            let cfg = TieredConfig {
                enabled: true,
                jit_threshold: cli.jit_threshold,
                max_jit_args: cli.jit_max_args,
                min_ops_for_promotion: cli.jit_min_ops,
                log_promotions: cli.jit_log,
                strict_mode: true,
                hot_reload: cli.hot_reload,
            };
            interpreter.enable_tiered(&hl_path, &native_resolver, cfg)?;
            let result = interpreter.execute_entrypoint(&bytecode, &native_resolver)?;
            if let Some(stats) = interpreter.tiered_stats() {
                if cli.jit_log {
                    eprintln!(
                        "[tiered] attempted={} succeeded={} failed={} compiled_calls={} fallbacks={}",
                        stats.attempted_promotions,
                        stats.successful_promotions,
                        stats.failed_promotions,
                        stats.compiled_calls,
                        stats.fallback_calls
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
