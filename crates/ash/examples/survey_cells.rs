//! Count how often a `CellSet` is dead — overwritten on every path before the
//! cell is read — and how many of those sit in a loop.
//!
//! `cellfwd` removes the redundant *load* after a store; the store itself
//! stays, because proving it dead needs more than one block. The call
//! benchmarks show a store per iteration for an accumulator whose cell is only
//! read after the loop. The question this answers is whether that shape is
//! general or peculiar to those benchmarks, which decides whether a
//! dead-cell-store pass is worth writing at all.
//!
//! Usage: survey_cells <file.hl>
use std::collections::{HashMap, HashSet};

fn main() -> anyhow::Result<()> {
    let path = std::env::args().nth(1).expect("usage: survey_cells <file.hl>");
    ash_core::native_lib::init_std_library()?;
    let bc = ash_core::bytecode::BytecodeDecoder::decode(std::path::Path::new(&path))?;
    let module = ash_core::air_pipeline::AshModule::new(&bc);

    let (mut fns, mut sets, mut dead, mut dead_in_loop, mut refused) = (0, 0, 0, 0, 0);
    let mut dead_fns: HashSet<usize> = HashSet::new();

    for f in &bc.functions {
        let Ok(opt) = ash_core::air_pipeline::optimized(&module, f) else {
            refused += 1;
            continue;
        };
        let ir = &opt.ir;
        fns += 1;

        // Cells read anywhere in the function, by any means.
        let mut read: HashSet<u32> = HashSet::new();
        for b in &ir.blocks {
            for i in &b.instrs {
                match i {
                    air::v2::ir::Instr::CellGet { cell, .. }
                    | air::v2::ir::Instr::CellRef { cell, .. } => {
                        read.insert(cell.0);
                    }
                    _ => {}
                }
            }
        }

        let cfg = air::v2::CfgInfo::build(ir);
        let forest = air::v2::LoopForest::analyze(ir, &cfg);
        let mut in_loop: HashSet<u32> = HashSet::new();
        for l in forest.innermost_first() {
            for b in &forest.get(l).blocks {
                in_loop.insert(b.idx() as u32);
            }
        }

        // Count each CellSet once. It is dead if the same cell is stored
        // again later in the same block with no read between, or if it sits
        // in a loop and nothing in the function ever reads that cell.
        for (bi, b) in ir.blocks.iter().enumerate() {
            let loop_block = in_loop.contains(&(bi as u32));
            let mut pending: HashMap<u32, usize> = HashMap::new();
            let mut dead_here: HashSet<usize> = HashSet::new();
            for (ii, i) in b.instrs.iter().enumerate() {
                match i {
                    air::v2::ir::Instr::CellSet { cell, .. } => {
                        if let Some(prev) = pending.insert(cell.0, ii) {
                            dead_here.insert(prev);
                        }
                    }
                    air::v2::ir::Instr::CellGet { cell, .. }
                    | air::v2::ir::Instr::CellRef { cell, .. } => {
                        pending.remove(&cell.0);
                    }
                    _ => {}
                }
            }
            for (ii, i) in b.instrs.iter().enumerate() {
                let air::v2::ir::Instr::CellSet { cell, .. } = i else {
                    continue;
                };
                sets += 1;
                let never_read = !read.contains(&cell.0);
                if dead_here.contains(&ii) || (loop_block && never_read) {
                    dead += 1;
                    dead_fns.insert(f.findex as usize);
                    if loop_block {
                        dead_in_loop += 1;
                    }
                }
            }
        }
    }

    println!("functions analysed : {fns}  (AIR refused {refused})");
    println!("CellSet total      : {sets}");
    println!("  dead (any rule)  : {dead}");
    println!("  dead in a loop   : {dead_in_loop}   <- the ones that cost per iteration");
    println!("functions affected : {}", dead_fns.len());
    Ok(())
}
