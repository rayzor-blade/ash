//! Enumerate AIR pipeline refusals over whole modules.
//!
//! Runs [`ash_core::air_pipeline::trip`] — lower, verify, identity
//! round-trip, optimize, verify, serialize — over EVERY declared function of
//! each `.hl` file given, and lists each refusal with the stage that refused,
//! the error, and whether the function contains the opcodes that force
//! register pinning (`Ref` / `Incr` / `Decr` / `Trap`). A refusal is then
//! re-run with `verify_each` so the pass that broke the IR names itself.
//!
//! This is the ground-truth work queue for field refusals (MBHaxe's
//! marblegame was the first): the pipeline alone, no execution, no window.
//!
//! Usage: air_refusals [--level O0|O1|O2|O3] <file.hl> [<file.hl>...]
//!
//! The level defaults to the production one (`ASH_AIR_LEVEL`, else O3).

use air::v2::{OptLevel, PassOptions};
use ash_core::air_pipeline::{self, AshModule};
use ash_core::opcodes::Opcode;

fn main() -> anyhow::Result<()> {
    let mut level = air_pipeline::default_level();
    let mut files: Vec<String> = Vec::new();
    let mut args = std::env::args().skip(1);
    while let Some(a) = args.next() {
        if a == "--level" {
            let l = args.next().expect("--level needs O0|O1|O2|O3");
            level = match l.as_str() {
                "O0" | "o0" | "0" => OptLevel::O0,
                "O1" | "o1" | "1" => OptLevel::O1,
                "O2" | "o2" | "2" => OptLevel::O2,
                "O3" | "o3" | "3" => OptLevel::O3,
                other => anyhow::bail!("unknown level '{other}' (expected O0|O1|O2|O3)"),
            };
        } else {
            files.push(a);
        }
    }
    if files.is_empty() {
        anyhow::bail!("usage: air_refusals [--level O0|O1|O2|O3] <file.hl>...");
    }

    ash_core::native_lib::init_std_library()?;

    // The pipeline catches per-stage panics and records them as refusals;
    // the default hook would still print each backtrace mid-sweep.
    std::panic::set_hook(Box::new(|_| {}));

    let mut grand_total = 0usize;
    let mut grand_refused = 0usize;
    for path in &files {
        let bc = ash_core::bytecode::BytecodeDecoder::decode(std::path::Path::new(path))?;
        let m = AshModule::new(&bc);
        let mut refused = 0usize;
        for f in &bc.functions {
            let t = air_pipeline::trip(&m, f, level, &PassOptions::default());
            let Some(e) = t.failure else { continue };
            refused += 1;

            let (mut r, mut inc, mut dec, mut trap) = (false, false, false, false);
            for op in &f.ops {
                match op {
                    Opcode::Ref { .. } => r = true,
                    Opcode::Incr { .. } => inc = true,
                    Opcode::Decr { .. } => dec = true,
                    Opcode::Trap { .. } => trap = true,
                    _ => {}
                }
            }
            println!(
                "REFUSED f{} {} ops={} stage={} [ref={} incr={} decr={} trap={}] {}",
                e.findex,
                e.name,
                f.ops.len(),
                e.stage,
                r,
                inc,
                dec,
                trap,
                e.brief()
            );
            // Attribute the damage to a pass: verify after each one.
            let t2 = air_pipeline::trip(
                &m,
                f,
                level,
                &PassOptions {
                    verify_each: true,
                    ..PassOptions::default()
                },
            );
            match t2.failure {
                Some(e2) => println!("        verify_each: stage={} {}", e2.stage, e2.brief()),
                None => println!("        verify_each: passes (refusal not reproduced?)"),
            }
        }
        println!(
            "{path}: total={} refused={refused} level={level:?}",
            bc.functions.len()
        );
        grand_total += bc.functions.len();
        grand_refused += refused;
    }
    if files.len() > 1 {
        println!(
            "ALL: files={} total={grand_total} refused={grand_refused} level={level:?}",
            files.len()
        );
    }
    Ok(())
}
