//! What would a loop vectorizer find in this bytecode?
//!
//! Runs [`air::v2::vectorize::analyze`] over every reachable function's
//! optimized AIR and reports, per loop, either the plan or the exact reason it
//! was refused. The refusal histogram at the end is the work queue: it says
//! which single obstacle, if removed, would unlock the most loops.
//!
//! Usage: vec_report <file.hl> [<file.hl> ...] [--verbose]

use air::v2::vectorize::{analyze, Refusal, VecOptions};
use std::collections::HashMap;

fn refusal_name(r: &Refusal) -> String {
    match r {
        Refusal::NotInnermost => "not innermost".into(),
        Refusal::MultipleExits => "multiple exits".into(),
        Refusal::NoInductionVariable => "no induction variable".into(),
        Refusal::MultipleInductionVariables => "multiple induction variables".into(),
        Refusal::NonConstantStride => "non-constant stride".into(),
        Refusal::UnknownTripCount => "unknown trip count".into(),
        Refusal::UnwidenableInstr(k) => format!("unwidenable instr: {k}"),
        Refusal::Call => "call in body".into(),
        Refusal::Allocation => "allocation in body".into(),
        Refusal::LoopCarried(_) => "loop-carried dependence".into(),
        Refusal::NonAffineAccess(_) => "non-affine memory access".into(),
        Refusal::MayAlias { .. } => "may alias".into(),
        Refusal::FloatReductionNeedsReassoc(_) => "float reduction (needs reassoc)".into(),
    }
}

fn main() -> anyhow::Result<()> {
    ash_core::native_lib::init_std_library()?;
    let args: Vec<String> = std::env::args().skip(1).collect();
    let verbose = args.iter().any(|a| a == "--verbose");
    let opts = VecOptions::default();

    let mut hist: HashMap<String, usize> = HashMap::new();
    let mut loops = 0usize;
    let mut ok = 0usize;
    // A loop stopped by exactly one thing is the cheapest to unlock.
    let mut sole: HashMap<String, usize> = HashMap::new();
    let mut exitshape: HashMap<String, usize> = HashMap::new();

    for path in args.iter().filter(|a| a.ends_with(".hl")) {
        let bc = ash_core::bytecode::BytecodeDecoder::decode(std::path::Path::new(path))?;
        let m = ash_core::air_pipeline::AshModule::new(&bc);
        println!("== {path}");
        for f in &bc.functions {
            let Ok(opt) = ash_core::air_pipeline::optimized(&m, f) else {
                continue;
            };
            for plan in analyze(&opt.ir, &opts) {
                loops += 1;
                if plan.vectorizable() {
                    ok += 1;
                    println!(
                        "  VECTORIZABLE findex={} header=b{} body={} iv={:?} reductions={} accesses={}",
                        f.findex,
                        plan.header.0,
                        plan.body_size,
                        plan.induction,
                        plan.reductions.len(),
                        plan.accesses.len()
                    );
                    continue;
                }
                let names: Vec<String> = plan.refusals.iter().map(refusal_name).collect();
                let mut uniq: Vec<String> = names.clone();
                uniq.sort();
                uniq.dedup();
                for n in &uniq {
                    *hist.entry(n.clone()).or_default() += 1;
                }
                if uniq.len() == 1 {
                    *sole.entry(uniq[0].clone()).or_default() += 1;
                }
                if !plan.exit_terms.is_empty() {
                    let terms: Vec<String> = plan
                        .exit_terms
                        .iter()
                        .map(|(b, t)| format!("b{}:{}", b.0, t))
                        .collect();
                    *exitshape.entry(terms.join("+")).or_default() += 1;
                }
                if verbose {
                    println!(
                        "  refused  findex={} header=b{} body={} : {}",
                        f.findex,
                        plan.header.0,
                        plan.body_size,
                        uniq.join(", ")
                    );
                }
            }
        }
    }

    println!("\n{loops} loops, {ok} vectorizable");
    let mut rows: Vec<_> = hist.into_iter().collect();
    rows.sort_by_key(|(_, c)| std::cmp::Reverse(*c));
    println!("\nrefusals (a loop may hit several):");
    for (name, count) in &rows {
        println!("  {count:5}  {name}");
    }
    let mut srows: Vec<_> = sole.into_iter().collect();
    srows.sort_by_key(|(_, c)| std::cmp::Reverse(*c));
    let mut eshapes: Vec<_> = exitshape.into_iter().collect();
    eshapes.sort_by_key(|(_, c)| std::cmp::Reverse(*c));
    println!("\nmulti-exit loops, by what their exiting blocks end with:");
    for (shape, count) in eshapes.iter().take(8) {
        println!("  {count:5}  {shape}");
    }
    println!("\nloops blocked by exactly ONE thing (the work queue):");
    for (name, count) in srows.iter().take(10) {
        println!("  {count:5}  {name}");
    }
    Ok(())
}
