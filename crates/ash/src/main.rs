use ash::jit::module::JITModule;
use clap::Parser;
use inkwell::context::Context;
use std::path::PathBuf;
use std::str::FromStr;

#[derive(Parser)]
#[command(name = "ash", about = "ASH - HashLink VM JIT Compiler")]
struct Cli {
    /// Path to a HashLink bytecode (.hl) file
    file: Option<PathBuf>,
}

pub fn main() {
    let cli = Cli::parse();

    let hl_path = cli.file.unwrap_or_else(|| {
        PathBuf::from_str(env!("CARGO_MANIFEST_DIR"))
            .unwrap()
            .join("test/test.hl")
    });

    let context = Context::create();
    let mut module = JITModule::new(&context, &hl_path);
    module.execute_main().expect("Failed to execute main");
}
