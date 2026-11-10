//! How much of a program the direct wasm backend takes, and what stops the rest.
//!
//! Usage: wasm_direct_coverage <file.hl> [<file.hl> ...] [--top N]
//!
//! Runs the emitter over every function with no LLVM involved, and groups the
//! reasons it declined by what they say with numbers removed.
use std::collections::{BTreeMap, HashMap};

fn shape(reason: &str) -> String {
    // "a call to f123 with ..." and "a call to f9 with ..." are one reason.
    let mut out = String::new();
    let mut digits = false;
    for c in reason.chars() {
        if c.is_ascii_digit() {
            if !digits {
                out.push('N');
            }
            digits = true;
        } else {
            digits = false;
            out.push(c);
        }
    }
    out
}

fn main() -> anyhow::Result<()> {
    let mut paths = Vec::new();
    let mut top = 25usize;
    let mut args = std::env::args().skip(1);
    while let Some(a) = args.next() {
        if a == "--top" {
            top = args.next().expect("--top N").parse()?;
        } else {
            paths.push(a);
        }
    }
    anyhow::ensure!(!paths.is_empty(), "usage: wasm_direct_coverage <file.hl>...");
    ash_core::native_lib::init_std_library()?;
    for path in paths {
        let bc = ash_core::bytecode::BytecodeDecoder::decode(std::path::Path::new(&path))?;
        let began = std::time::Instant::now();
        // Placeholder names: coverage counts what could be emitted, and there
        // is no LLVM module here to name the real data.
        let data = ash_core::wasm_direct::ProgramData {
            globals: Some("ash_globals".to_string()),
            types: (0..bc.types.len())
                .map(|i| (i, format!("ash_type_{i}")))
                .collect(),
            sized_alloc: true,
            object_tails: HashMap::new(),
            object_drops: std::collections::HashSet::new(),
            barriers: true,
        };
        let emitted = ash_core::wasm_direct::emit_object(
            &bc,
            &HashMap::new(),
            &data,
            ash_wasm_emit::Options {
                polls: true,
                shadow: true,
            },
        );
        let total = bc.functions.len();
        println!("=== {path}");
        println!(
            "{} of {total} functions direct ({:.1}%), {} bytes, {:.0} ms",
            emitted.functions.len(),
            100.0 * emitted.functions.len() as f64 / total.max(1) as f64,
            emitted.bytes.len(),
            began.elapsed().as_secs_f64() * 1e3
        );
        let mut by_reason: BTreeMap<String, usize> = BTreeMap::new();
        for (_, why) in &emitted.skipped {
            *by_reason.entry(shape(why)).or_default() += 1;
        }
        let mut rows: Vec<_> = by_reason.into_iter().collect();
        rows.sort_by_key(|(_, n)| std::cmp::Reverse(*n));
        for (why, n) in rows.iter().take(top) {
            println!("  {n:6}  {why}");
        }
    }
    Ok(())
}
