//! How much of a program the interpreter may prepare cheaply.
//!
//! `air_pipeline::interpreter_config_for` splits every function on one rule:
//! a back edge means OSR can enter it, so its AIR has to stay positionally
//! identical to the body a tier compiles, and it keeps the shared
//! configuration. Everything else is the interpreter's alone.
//!
//! That split is what bounds a cheaper interpreter pipeline, so it is worth
//! counting rather than assuming -- if loop-free functions were a tenth of a
//! program there would be nothing to win.
//!
//! Usage: air_levels <file.hl> [<file.hl>...]

use ash_core::air_pipeline::{self, AirConfigKey};

fn main() -> anyhow::Result<()> {
    let files: Vec<String> = std::env::args().skip(1).collect();
    if files.is_empty() {
        eprintln!("usage: air_levels <file.hl> [...]");
        return Ok(());
    }
    ash_core::native_lib::init_std_library()?;
    println!(
        "interpreter level = {:?}  (pipeline default {:?})",
        air_pipeline::interpreter_level(),
        air_pipeline::default_level()
    );
    for path in &files {
        let bc = ash_core::bytecode::BytecodeDecoder::decode(std::path::Path::new(path))?;
        let (mut loops, mut flat, mut loop_ops, mut flat_ops) = (0usize, 0usize, 0usize, 0usize);
        for f in &bc.functions {
            if air_pipeline::interpreter_config_for(f) == AirConfigKey::standard() {
                loops += 1;
                loop_ops += f.ops.len();
            } else {
                flat += 1;
                flat_ops += f.ops.len();
            }
        }
        let total = loops + flat;
        let ops = loop_ops + flat_ops;
        println!("== {path} ==");
        println!(
            "  with a back edge (stays O3) : {loops:5} fns ({:4.1}%)  {loop_ops:7} ops ({:4.1}%)",
            100.0 * loops as f64 / total.max(1) as f64,
            100.0 * loop_ops as f64 / ops.max(1) as f64,
        );
        println!(
            "  loop-free (interpreter's)   : {flat:5} fns ({:4.1}%)  {flat_ops:7} ops ({:4.1}%)",
            100.0 * flat as f64 / total.max(1) as f64,
            100.0 * flat_ops as f64 / ops.max(1) as f64,
        );
    }
    Ok(())
}
