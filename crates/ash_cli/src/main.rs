use anyhow::Result;
use ash::bytecode::BytecodeDecoder;
use ash::native_lib::{init_std_library, NativeFunctionResolver};
use ash_interp::interpreter::HLInterpreter;
use std::env;
use std::path::PathBuf;
use std::process;

fn main() {
    if let Err(e) = run() {
        eprintln!("Error: {:#}", e);
        process::exit(1);
    }
}

fn run() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    let hl_path = if args.len() > 1 && !args[1].starts_with("--") {
        PathBuf::from(&args[1])
    } else {
        // Default to test file for development
        let mut cwd = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        cwd.push("../../crates/ash/test/test.hl");
        cwd
    };

    if !hl_path.exists() {
        anyhow::bail!("Bytecode file not found: {}", hl_path.display());
    }

    // Parse mode from args: --jit, --interp, or default hybrid
    let mode = if args.iter().any(|a| a == "--jit") {
        ExecutionMode::JitOnly
    } else if args.iter().any(|a| a == "--interp") {
        ExecutionMode::InterpreterOnly
    } else {
        ExecutionMode::Hybrid
    };

    // Initialize the standard library (loads libash_std)
    init_std_library()?;

    // Decode bytecode
    let bytecode = BytecodeDecoder::decode(&hl_path)?;

    // Create native function resolver for FFI calls
    let native_resolver = NativeFunctionResolver::new();

    match mode {
        ExecutionMode::InterpreterOnly | ExecutionMode::Hybrid => {
            // For now, both hybrid and interp-only use the interpreter
            // JIT tier promotion will be added in later phases
            let mut interpreter = HLInterpreter::new(&bytecode, &native_resolver);
            let result = interpreter.execute_entrypoint(&bytecode, &native_resolver)?;
            eprintln!("Interpreter returned: {:?}", result);
        }
        ExecutionMode::JitOnly => {
            // Full JIT path via the existing JITModule
            eprintln!("JIT-only mode not yet fully implemented, falling back to interpreter");
            let mut interpreter = HLInterpreter::new(&bytecode, &native_resolver);
            let result = interpreter.execute_entrypoint(&bytecode, &native_resolver)?;
            eprintln!("Interpreter returned: {:?}", result);
        }
    }

    Ok(())
}

enum ExecutionMode {
    InterpreterOnly,
    JitOnly,
    Hybrid,
}
