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

    /// Minimum opcode count before a function is promotion-eligible
    #[arg(long, default_value_t = 16)]
    jit_min_ops: usize,
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
    if let Err(e) = run() {
        eprintln!("Error: {:#}", e);
        process::exit(1);
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
    let native_resolver = NativeFunctionResolver::new();

    match cli.mode {
        Mode::Interp => {
            let mut interpreter = HLInterpreter::new(&bytecode, &native_resolver);
            let result = interpreter.execute_entrypoint(&bytecode, &native_resolver)?;
            eprintln!("Interpreter returned: {:?}", result);
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
            eprintln!("Interpreter returned: {:?}", result);
        }
        Mode::Jit => {
            eprintln!("JIT-only mode not yet fully implemented, falling back to interpreter");
            let mut interpreter = HLInterpreter::new(&bytecode, &native_resolver);
            let result = interpreter.execute_entrypoint(&bytecode, &native_resolver)?;
            eprintln!("Interpreter returned: {:?}", result);
        }
    }

    Ok(())
}
