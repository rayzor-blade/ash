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

// glibc frees lazily on Linux; jemalloc frees on time (wren_lift-proven).
#[cfg(target_os = "linux")]
#[global_allocator]
static GLOBAL: tikv_jemallocator::Jemalloc = tikv_jemallocator::Jemalloc;

pub fn main() {
    let cli = Cli::parse();
    ash::profile::init();

    let hl_path = cli.file.unwrap_or_else(|| {
        PathBuf::from_str(env!("CARGO_MANIFEST_DIR"))
            .unwrap()
            .join("test/test.hl")
    });

    let context = Context::create();
    let mut module = {
        let _p = ash::profile::scope("jit init");
        JITModule::new(&context, &hl_path)
    };
    // The whole-module compile happens inside execute_main, so the two are
    // separated here rather than in the callee.
    {
        let _p = ash::profile::scope("compile + setup + run");
        module.execute_main().expect("Failed to execute main");
    }
    ash::profile::report();
}
