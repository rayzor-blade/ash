//! Which stdlib FFI calls could have been AIR instruction blocks?
//!
//! Walks every reachable function's optimized AIR, finds `Call`s whose
//! target is a native, and reports them grouped by symbol with the deepest
//! loop nesting any call site sits at — a call at depth 2 runs O(n²) times,
//! so static site counts alone say nothing.
//!
//! Usage: ffi_report <file.hl> [<file.hl> ...]
use std::collections::HashMap;

fn main() -> anyhow::Result<()> {
    ash::native_lib::init_std_library()?;
    // symbol -> (sites, max loop depth, sites at depth >= 1)
    let mut agg: HashMap<String, (usize, usize, usize)> = HashMap::new();

    for path in std::env::args().skip(1) {
        let bc = ash::bytecode::BytecodeDecoder::decode(std::path::Path::new(&path))?;
        let m = ash::air_pipeline::AshModule::new(&bc);
        let natives: HashMap<usize, String> = bc
            .natives
            .iter()
            .map(|n| (n.findex as usize, format!("{}@{}", n.lib, n.name)))
            .collect();
        for f in &bc.functions {
            let Ok(opt) = ash::air_pipeline::optimized(&m, f) else {
                continue;
            };
            let ir = &opt.ir;
            let cfg = air::v2::CfgInfo::build(ir);
            let forest = air::v2::LoopForest::analyze(ir, &cfg);
            // depth per block
            let mut depth = vec![0usize; ir.blocks.len()];
            for l in forest.innermost_first() {
                let lp = forest.get(l);
                for b in &lp.blocks {
                    depth[b.idx()] += 1;
                }
            }
            for (bi, blk) in ir.blocks.iter().enumerate() {
                for ins in &blk.instrs {
                    let air::v2::Instr::Call { fun, .. } = ins else {
                        continue;
                    };
                    let Some(sym) = natives.get(fun) else { continue };
                    let e = agg.entry(sym.clone()).or_default();
                    e.0 += 1;
                    e.1 = e.1.max(depth[bi]);
                    if depth[bi] >= 1 {
                        e.2 += 1;
                    }
                }
            }
        }
    }

    let mut rows: Vec<_> = agg.into_iter().collect();
    rows.sort_by(|a, b| (b.1 .1, b.1 .2, b.1 .0).cmp(&(a.1 .1, a.1 .2, a.1 .0)));
    println!("{:<40} {:>6} {:>9} {:>10}", "native", "sites", "in-loop", "max-depth");
    for (sym, (sites, maxd, inloop)) in rows {
        println!("{sym:<40} {sites:>6} {inloop:>9} {maxd:>10}");
    }
    Ok(())
}
