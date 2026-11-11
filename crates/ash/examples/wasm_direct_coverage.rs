//! How much of a program the direct wasm backend takes, and what stops the rest.
//!
//! Usage: wasm_direct_coverage <file.hl> [<file.hl> ...] [--top N] [--class NAME] [--exact]
//!
//! `--exact` keeps the numbers in each reason instead of folding them.
//!
//! With `--class`, also says for each static method of class NAME whether it
//! went direct and, if not, why.
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
    let mut class_filter: Option<String> = None;
    let mut exact = false;
    let mut args = std::env::args().skip(1);
    while let Some(a) = args.next() {
        if a == "--top" {
            top = args.next().expect("--top N").parse()?;
        } else if a == "--exact" {
            exact = true;
        } else if a == "--class" {
            class_filter = Some(args.next().expect("--class NAME"));
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
            functions: Some("ash_functions".to_string()),
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
        if let Some(class) = &class_filter {
            let want = format!("${class}");
            let statics = bc
                .types
                .iter()
                .filter_map(|t| t.obj.as_ref())
                .find(|o| o.name == want);
            if let Some(o) = statics {
                // A binding names its field by absolute index, which counts
                // the fields of every supertype first.
                let mut inherited = 0;
                let mut up = o.super_.clone();
                while let Some(sup) = up {
                    let Some(parent) = bc.types[sup.0].obj.as_ref() else { break };
                    inherited += parent.fields.len();
                    up = parent.super_.clone();
                }
                for pair in o.bindings.chunks(2) {
                    let (field, findex) = (pair[0] as usize, pair[1] as usize);
                    let name = field
                        .checked_sub(inherited)
                        .and_then(|i| o.fields.get(i))
                        .map_or("?", |f| f.name.as_str());
                    let verdict = match emitted.skipped.iter().find(|(i, _)| *i == findex) {
                        Some((_, why)) => why.clone(),
                        None if emitted.functions.contains(&findex) => "direct".to_string(),
                        None => "not offered".to_string(),
                    };
                    println!("  f{findex} {name}: {verdict}");
                }
            }
        }
        let mut by_reason: BTreeMap<String, usize> = BTreeMap::new();
        for (_, why) in &emitted.skipped {
            *by_reason
                .entry(if exact { why.clone() } else { shape(why) })
                .or_default() += 1;
        }
        if exact {
            // Name the natives behind "no known signature".
            let mut by_lib: BTreeMap<String, usize> = BTreeMap::new();
            for (why, n) in &by_reason {
                let Some(rest) = why.strip_prefix("a call to f") else { continue };
                let Some(findex) = rest.split(',').next().and_then(|d| d.parse::<usize>().ok())
                else {
                    continue;
                };
                if !why.contains("no known signature") {
                    continue;
                }
                let label = match bc.natives.iter().find(|n| n.findex as usize == findex) {
                    Some(nat) => format!("native {}", nat.lib),
                    None => "a bytecode function".to_string(),
                };
                *by_lib.entry(label).or_default() += n;
            }
            for (label, n) in by_lib {
                println!("  callers of {label}: {n}");
            }
        }
        let mut rows: Vec<_> = by_reason.into_iter().collect();
        rows.sort_by_key(|(_, n)| std::cmp::Reverse(*n));
        for (why, n) in rows.iter().take(top) {
            println!("  {n:6}  {why}");
        }
    }
    Ok(())
}
