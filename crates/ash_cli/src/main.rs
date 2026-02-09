use anyhow::Result;
use ash::bytecode::BytecodeDecoder;
use ash::native_lib::{init_std_library, NativeFunctionResolver};
use ash_interp::interpreter::HLInterpreter;
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
        Mode::Interp | Mode::Hybrid => {
            let mut interpreter = HLInterpreter::new(&bytecode, &native_resolver);
            let result = interpreter.execute_entrypoint(&bytecode, &native_resolver)?;
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
