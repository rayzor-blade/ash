//! What fraction of a program can the Cranelift tier compile, and what stops
//! the rest?
//!
//! Runs the tier's own pre-flight (`reject_reason`) over every reachable
//! function's optimized AIR. The histogram is the coverage work queue.
use std::collections::HashMap;

fn main() -> anyhow::Result<()> {
    ash_core::native_lib::init_std_library()?;
    let mut hist: HashMap<String, usize> = HashMap::new();
    let (mut ok, mut total) = (0usize, 0usize);
    for path in std::env::args().skip(1).filter(|a| a.ends_with(".hl")) {
        let bc = ash_core::bytecode::BytecodeDecoder::decode(std::path::Path::new(&path))?;
        let m = ash_core::air_pipeline::AshModule::new(&bc);
        for f in &bc.functions {
            let Ok(opt) = ash_core::air_pipeline::optimized(&m, f) else { continue };
            total += 1;
            match ash_core::cranelift::codegen::reject_reason(&opt.ir) {
                None => ok += 1,
                Some(why) => *hist.entry(why).or_default() += 1,
            }
        }
    }
    println!("{ok}/{total} functions compilable by the Cranelift tier ({:.1}%)",
             100.0 * ok as f64 / total.max(1) as f64);
    let mut rows: Vec<_> = hist.into_iter().collect();
    rows.sort_by_key(|(_, c)| std::cmp::Reverse(*c));
    for (why, n) in rows.iter().take(20) {
        println!("  {n:5}  {why}");
    }
    Ok(())
}
