//! Run an ash program compiled to WebAssembly, under wasmtime.
//!
//! `ash-wasm-run prog.wasm [args...]`. The exit status is the program's.
//!
//! `--imports` lists what the module still needs instead of running it, which
//! is the question worth asking while the runtime is being ported.

use std::path::PathBuf;

use anyhow::Result;
use ash_wasm_runtime::native::{Outcome, Program};
use clap::Parser;

#[derive(Parser)]
#[command(
    name = "ash-wasm-run",
    about = "Run an ash wasm program under wasmtime"
)]
struct Args {
    /// The module to run.
    module: PathBuf,
    /// Report the imports no host can satisfy, then stop.
    #[arg(long)]
    imports: bool,
    /// Everything after the module belongs to the program.
    #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
    program_args: Vec<String>,
}

#[tokio::main]
async fn main() -> Result<()> {
    let args = Args::parse();
    let program = Program::load(&args.module)?;

    if args.imports {
        let missing = program.missing();
        if missing.is_empty() {
            println!("nothing missing: this module needs only WASI and the fiber import");
        } else {
            println!("{} import(s) no host can supply:", missing.len());
            for name in missing {
                println!("  {name}");
            }
        }
        return Ok(());
    }

    let mut argv = vec![args
        .module
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_else(|| "program".to_string())];
    argv.extend(args.program_args);

    match program.run(&argv).await? {
        Outcome::Exited(code) => std::process::exit(code),
        Outcome::Trapped(trap) => {
            eprintln!("{trap}");
            std::process::exit(70)
        }
    }
}
