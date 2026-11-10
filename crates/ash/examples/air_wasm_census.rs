//! What a direct AIR-to-wasm emitter would have to handle.
//!
//! Usage: air_wasm_census <file.hl> [<file.hl> ...]
//!
//! For every function with a body, takes the optimized AIR the backends read
//! and counts the instruction and terminator kinds, the loop shapes, and the
//! features that need more than a plain emitter: irreducible control flow
//! (wasm has only structured control flow), trap regions (wasm exception
//! handling), and vector or inline-assembly instructions.
use air::v2::ir::{Function, Terminator};
use air::v2::{CfgInfo, LoopForest};
use std::collections::BTreeMap;

fn variant(debug: &str) -> String {
    debug
        .chars()
        .take_while(|c| c.is_ascii_alphanumeric())
        .collect()
}

/// A retreating edge whose target does not dominate its source: the mark of
/// an irreducible CFG.
fn irreducible(f: &Function, cfg: &CfgInfo) -> bool {
    let n = f.blocks.len();
    // 0 = unvisited, 1 = on the DFS stack, 2 = done.
    let mut state = vec![0u8; n];
    let mut stack: Vec<(usize, usize)> = vec![(0, 0)];
    state[0] = 1;
    while let Some(&mut (b, ref mut next)) = stack.last_mut() {
        if let Some(&s) = cfg.succs[b].get(*next) {
            *next += 1;
            let s = s.idx();
            match state[s] {
                0 => {
                    state[s] = 1;
                    stack.push((s, 0));
                }
                1 if !cfg.dominates(
                    air::v2::ir::BlockId(s as u32),
                    air::v2::ir::BlockId(b as u32),
                ) =>
                {
                    return true;
                }
                _ => {}
            }
        } else {
            state[b] = 2;
            stack.pop();
        }
    }
    false
}

#[derive(Default)]
struct Census {
    functions: usize,
    refused: usize,
    instrs: usize,
    blocks: usize,
    kinds: BTreeMap<String, (usize, usize)>,
    terms: BTreeMap<String, usize>,
    irreducible: usize,
    multi_latch_loops: usize,
    loops: usize,
    max_loop_depth: usize,
    with_trap: usize,
    with_vector: usize,
    with_asm: usize,
    plain: usize,
    plain_with_traps: usize,
    largest: (usize, i32),
}

fn main() -> anyhow::Result<()> {
    let paths: Vec<String> = std::env::args().skip(1).collect();
    anyhow::ensure!(!paths.is_empty(), "usage: air_wasm_census <file.hl>...");
    ash_core::native_lib::init_std_library()?;
    for path in paths {
        let bc = ash_core::bytecode::BytecodeDecoder::decode(std::path::Path::new(&path))?;
        let m = ash_core::air_pipeline::AshModule::new(&bc);
        let mut c = Census::default();
        for f in &bc.functions {
            let opt = match ash_core::air_pipeline::optimized(&m, f) {
                Ok(o) => o,
                Err(_) => {
                    c.refused += 1;
                    continue;
                }
            };
            let ir = &opt.ir;
            c.functions += 1;
            c.blocks += ir.blocks.len();
            let mut used = std::collections::BTreeSet::new();
            let mut n_instrs = 0;
            let (mut trap, mut vector, mut asm) = (false, false, false);
            for b in &ir.blocks {
                n_instrs += b.instrs.len();
                trap |= b.handler.is_some() || matches!(b.term, Terminator::Trap { .. });
                for i in &b.instrs {
                    let k = variant(&format!("{i:?}"));
                    vector |= k.starts_with("Vec");
                    asm |= k == "Asm";
                    c.kinds.entry(k.clone()).or_default().0 += 1;
                    used.insert(k);
                }
                *c.terms.entry(variant(&format!("{:?}", b.term))).or_default() += 1;
            }
            for k in used {
                c.kinds.entry(k).or_default().1 += 1;
            }
            c.instrs += n_instrs;
            if n_instrs > c.largest.0 {
                c.largest = (n_instrs, f.findex);
            }
            let cfg = CfgInfo::build(ir);
            let irr = irreducible(ir, &cfg);
            let forest = LoopForest::analyze(ir, &cfg);
            c.loops += forest.len();
            c.multi_latch_loops += forest.loops.iter().filter(|l| l.latches.len() > 1).count();
            c.max_loop_depth = c
                .max_loop_depth
                .max(forest.loops.iter().map(|l| l.depth + 1).max().unwrap_or(0));
            c.irreducible += irr as usize;
            c.with_trap += trap as usize;
            c.with_vector += vector as usize;
            c.with_asm += asm as usize;
            let simple = !irr && !vector && !asm;
            c.plain_with_traps += simple as usize;
            c.plain += (simple && !trap) as usize;
        }
        report(&path, &bc.functions.len(), &c);
    }
    Ok(())
}

fn pct(n: usize, of: usize) -> String {
    format!("{n} ({:.1}%)", 100.0 * n as f64 / of.max(1) as f64)
}

fn report(path: &str, total: &usize, c: &Census) {
    println!("=== {path}");
    println!(
        "functions in file {total}; lowered to AIR {}; pipeline refused {}",
        c.functions, c.refused
    );
    println!(
        "blocks {}, instructions {}, largest function {} instrs (findex {})",
        c.blocks, c.instrs, c.largest.0, c.largest.1
    );
    println!(
        "loops {} (multi-latch {}, max nesting {}); irreducible CFG in {} functions",
        c.loops,
        c.multi_latch_loops,
        c.max_loop_depth,
        pct(c.irreducible, c.functions)
    );
    println!(
        "functions with a trap region {}; with vector instrs {}; with inline asm {}",
        pct(c.with_trap, c.functions),
        pct(c.with_vector, c.functions),
        pct(c.with_asm, c.functions)
    );
    println!(
        "plain (reducible, no vector/asm, no trap): {}",
        pct(c.plain, c.functions)
    );
    println!(
        "plain if traps are supported:              {}",
        pct(c.plain_with_traps, c.functions)
    );
    println!("terminators: {:?}", c.terms);
    let mut kinds: Vec<_> = c.kinds.iter().collect();
    kinds.sort_by_key(|k| std::cmp::Reverse(k.1.0));
    println!("instruction kinds (total, functions using):");
    for (k, (n, f)) in kinds {
        println!("  {k:<14} {n:>8} {f:>6}");
    }
}
