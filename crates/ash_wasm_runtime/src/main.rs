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
    /// A directory to make visible to the program, beyond its own.
    ///
    /// The working directory is always given. Anything else the program is
    /// meant to reach has to be named, because a wasm module can open only
    /// what the host has opened for it -- there is no path out of a sandbox
    /// that the host did not build.
    #[arg(long = "dir", value_name = "PATH")]
    dirs: Vec<PathBuf>,
    /// Everything after the module belongs to the program.
    #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
    program_args: Vec<String>,
}

/// The module's path as the PROGRAM will see it.
///
/// argv[0] is what `Sys.programPath` reports, and a guest that is handed a
/// bare file name cannot tell whether the path is absolute or find the file
/// again. Its root is the directory the host preopened, so the module's path
/// relative to that, with a leading separator, names the same file on both
/// sides. A module from outside that directory has no name the guest could
/// use, so it keeps the bare one.
fn guest_visible_path(module: &std::path::Path) -> String {
    let bare = || {
        module
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_else(|| "program".to_string())
    };
    let (Ok(cwd), Ok(full)) = (std::env::current_dir(), module.canonicalize()) else {
        return bare();
    };
    match full.strip_prefix(&cwd) {
        Ok(rest) => format!("/{}", rest.to_string_lossy()),
        Err(_) => bare(),
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    let args = Args::parse();
    let program = Program::load(&args.module)?;

    if args.imports {
        let missing = program.missing();
        if missing.is_empty() {
            println!("nothing missing: this module needs only WASI, the fiber import and the socket imports");
        } else {
            println!("{} import(s) no host can supply:", missing.len());
            for name in missing {
                println!("  {name}");
            }
        }
        return Ok(());
    }

    let mut argv = vec![guest_visible_path(&args.module)];
    argv.extend(args.program_args);

    match program.run(&argv, &args.dirs).await? {
        Outcome::Exited(code) => std::process::exit(code),
        Outcome::Trapped(trap) => {
            eprintln!("{trap}");
            std::process::exit(70)
        }
    }
}
