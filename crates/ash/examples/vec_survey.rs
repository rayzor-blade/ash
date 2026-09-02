//! Where a program's loops stand with the vectorizer, and why the rest refuse.
//!
//! Runs the loop analysis over EVERY function of each `.hl` file and tallies
//! the outcome per loop: widened, widenable-but-declined, or refused by the
//! analysis with a named obstacle. The refusals are the work queue — the same
//! role `air_refusals` plays for the pipeline.
//!
//! HL arrays are the interesting case. A `varray` is a contiguous element
//! block after a 24-byte header, so `GetArray`/`SetArray` index in ELEMENTS
//! and unit stride is the natural shape. Whether real Haxe array code reaches
//! that form — rather than a call into the stdlib, which refuses — is what
//! this measures rather than assumes.
//!
//! Usage: vec_survey <file.hl> [<file.hl>...]

use air::v2::vectorize::{self, Refusal, VecOptions};
use ash_core::air_pipeline::{self, AshModule};
use std::collections::BTreeMap;

fn main() -> anyhow::Result<()> {
    let files: Vec<String> = std::env::args().skip(1).collect();
    if files.is_empty() {
        eprintln!("usage: vec_survey <file.hl> [...]");
        return Ok(());
    }
    ash_core::native_lib::init_std_library()?;
    for path in &files {
        let bc = ash_core::bytecode::BytecodeDecoder::decode(std::path::Path::new(path))?;
        let module = AshModule::new(&bc);
        let opts = VecOptions::default();

        let mut loops = 0usize;
        let mut widenable = 0usize;
        let mut refusals: BTreeMap<String, usize> = BTreeMap::new();
        let mut array_loops = 0usize;
        let mut array_widenable = 0usize;
        let mut fns_with_loops = 0usize;
        // What the calls inside array-touching loops actually are. "Call in
        // body" is the top refusal; naming the callees turns it into a queue.
        let mut array_blockers: BTreeMap<String, usize> = BTreeMap::new();

        for f in &bc.functions {
            let Ok(opt) = air_pipeline::optimized(&module, f) else {
                continue;
            };
            let ir = &opt.ir;
            let plans = vectorize::analyze_with(ir, &opts, &|i| bc.ints.get(i).copied());
            if plans.is_empty() {
                continue;
            }
            // Blocks of each loop, so "touches an array" and "calls what"
            // are answered about the LOOP and not merely the function it
            // sits in. Scanning the whole function overstates both.
            let cfg = air::v2::analysis::CfgInfo::build(ir);
            let forest = air::v2::analysis::LoopForest::analyze(ir, &cfg);
            let blocks_of = |header: air::v2::ir::BlockId| -> Vec<air::v2::ir::BlockId> {
                forest
                    .innermost_first()
                    .into_iter()
                    .map(|l| forest.get(l))
                    .find(|nl| nl.header == header)
                    .map(|nl| nl.blocks.clone())
                    .unwrap_or_default()
            };
            fns_with_loops += 1;
            for p in &plans {
                loops += 1;
                // Does this loop touch an HL array at all?
                let lp_blocks = blocks_of(p.header);
                let touches_array = lp_blocks.iter().any(|bid| {
                    ir.blocks[bid.idx()].instrs.iter().any(|i| {
                        matches!(
                            i,
                            air::v2::ir::Instr::MemGet {
                                kind: air::v2::ir::MemAccess::Array,
                                ..
                            } | air::v2::ir::Instr::MemSet {
                                kind: air::v2::ir::MemAccess::Array,
                                ..
                            }
                        )
                    })
                });
                if touches_array {
                    array_loops += 1;
                }
                if p.vectorizable() {
                    widenable += 1;
                    if touches_array {
                        array_widenable += 1;
                    }
                } else {
                    for r in &p.refusals {
                        *refusals.entry(name_of(r)).or_default() += 1;
                    }
                    if touches_array && p.refusals.contains(&Refusal::Call) {
                        for bid in &lp_blocks {
                            for i in &ir.blocks[bid.idx()].instrs {
                                let who = match i {
                                    air::v2::ir::Instr::Call { fun, .. } => bc
                                        .natives
                                        .iter()
                                        .find(|n| n.findex as usize == *fun)
                                        .map(|n| format!("native {}@{}", n.lib, n.name))
                                        .or_else(|| {
                                            bc.functions
                                                .iter()
                                                .find(|g| g.findex as usize == *fun)
                                                .map(|g| format!("fn {}", g.name()))
                                        })
                                        .unwrap_or_else(|| format!("fn #{fun}")),
                                    air::v2::ir::Instr::CallMethod { .. } => "method (vtable)".into(),
                                    air::v2::ir::Instr::CallClosure { .. } => "closure".into(),
                                    _ => continue,
                                };
                                *array_blockers.entry(who).or_default() += 1;
                            }
                        }
                    }
                }
            }
        }

        println!("== {path} ==");
        println!("  functions with loops : {fns_with_loops}");
        println!("  loops                : {loops}");
        println!("  widenable            : {widenable}");
        println!("  loops touching arrays: {array_loops}  (widenable: {array_widenable})");
        println!("  refusals, most common first:");
        let mut rows: Vec<_> = refusals.into_iter().collect();
        rows.sort_by_key(|(_, n)| std::cmp::Reverse(*n));
        for (why, n) in rows.iter().take(14) {
            println!("    {n:>6}  {why}");
        }
        if !array_blockers.is_empty() {
            println!("  calls inside array loops:");
            let mut b: Vec<_> = array_blockers.into_iter().collect();
            b.sort_by_key(|(_, n)| std::cmp::Reverse(*n));
            for (who, n) in b.iter().take(12) {
                println!("    {n:>6}  {who}");
            }
        }
    }
    Ok(())
}

fn name_of(r: &Refusal) -> String {
    match r {
        Refusal::NotInnermost => "not innermost".into(),
        Refusal::MultipleExits => "multiple exits".into(),
        Refusal::NoInductionVariable => "no induction variable".into(),
        Refusal::MultipleInductionVariables => "multiple induction variables".into(),
        Refusal::NonConstantStride => "non-constant stride".into(),
        Refusal::UnknownTripCount => "unknown trip count".into(),
        Refusal::UnwidenableInstr(k) => format!("unwidenable instruction: {k}"),
        Refusal::Call => "call in body".into(),
        Refusal::Allocation => "allocation in body".into(),
        Refusal::LoopCarried(_) => "loop-carried value".into(),
        Refusal::NonAffineAccess(_) => "non-affine access".into(),
        Refusal::MayAlias { .. } => "may alias".into(),
        Refusal::FloatReductionNeedsReassoc(_) => "float reduction (needs reassoc)".into(),
    }
}
