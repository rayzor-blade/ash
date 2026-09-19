//! What each AIR pass did over a whole program: its statistics summed over
//! every function, and the functions it changed.
//!
//! A pass earns its place in the pipeline by firing; this is where that is
//! counted rather than assumed. The pipeline runs to a fixed point, so the
//! sums are over every round.
//!
//! Usage: air_pass_stats [--level O0|O1|O2|O3] <file.hl> [<file.hl>...]
//!
//! The level defaults to the production one (`ASH_AIR_LEVEL`, else O3).

use air::v2::passes::{OptLevel, PassOptions, PassStats};
use ash_core::air_pipeline::{self, AshModule};
use std::collections::BTreeMap;

/// The non-zero counters of `s`, named.
fn counts(s: &PassStats) -> String {
    let fields = [
        ("eliminated", s.eliminated),
        ("hoisted", s.hoisted),
        ("fused", s.fused),
        ("replaced", s.replaced),
        ("tail_calls", s.tail_calls),
        ("inlined", s.inlined),
        ("added", s.added),
        ("allocs_removed", s.allocs_removed),
        ("fields_scalarized", s.fields_scalarized),
    ];
    fields
        .iter()
        .filter(|(_, n)| *n > 0)
        .map(|(k, n)| format!("{k}={n}"))
        .collect::<Vec<_>>()
        .join(" ")
}

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
        anyhow::bail!("usage: air_pass_stats [--level O0|O1|O2|O3] <file.hl>...");
    }

    ash_core::native_lib::init_std_library()?;
    std::panic::set_hook(Box::new(|_| {}));

    for path in &files {
        let bc = ash_core::bytecode::BytecodeDecoder::decode(std::path::Path::new(path))?;
        let m = AshModule::new(&bc);
        // Per pass: the summed statistics and the functions it changed.
        let mut per_pass: BTreeMap<&'static str, (PassStats, Vec<usize>)> = BTreeMap::new();
        let mut functions = 0usize;
        for f in &bc.functions {
            let t = air_pipeline::trip(&m, f, level, &PassOptions::default());
            if t.failure.is_some() {
                continue;
            }
            functions += 1;
            for (name, stats) in &t.passes {
                let entry = per_pass.entry(name).or_default();
                entry.0.merge(*stats);
                if stats.changed() {
                    entry.1.push(f.findex as usize);
                }
            }
        }
        println!("== {path} ({functions} functions at {level:?})");
        for (name, (stats, fns)) in &per_pass {
            if !stats.changed() {
                continue;
            }
            let shown: Vec<String> = fns.iter().take(12).map(|f| f.to_string()).collect();
            let more = if fns.len() > 12 {
                format!(" +{}", fns.len() - 12)
            } else {
                String::new()
            };
            println!(
                "  {name:<24} {}\n  {:<24} in findex {}{more}",
                counts(stats),
                "",
                shown.join(",")
            );
        }
    }
    Ok(())
}
