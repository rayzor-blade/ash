//! The shape of a wasm module's code: how many functions, locals, call sites
//! and branch tables it has, and where the largest products of locals and
//! call sites are.
//!
//! Usage: wasm_stats <module.wasm> [--top N]
use wasmparser::{Operator, Parser, Payload};

fn main() -> anyhow::Result<()> {
    let mut path = None;
    let mut top = 8usize;
    let mut args = std::env::args().skip(1);
    while let Some(a) = args.next() {
        if a == "--top" {
            top = args.next().expect("--top N").parse()?;
        } else {
            path = Some(a);
        }
    }
    let bytes = std::fs::read(path.expect("usage: wasm_stats <module.wasm>"))?;
    let (mut funcs, mut locals, mut calls, mut indirect, mut tables, mut ops) = (0u64, 0u64, 0u64, 0u64, 0u64, 0u64);
    let mut rows: Vec<(u64, u32, u64, u64)> = Vec::new(); // (locals*calls, locals, calls, ops)
    let mut index = 0u32;
    for payload in Parser::new(0).parse_all(&bytes) {
        if let Payload::CodeSectionEntry(body) = payload? {
            let mut n_locals = 0u64;
            for l in body.get_locals_reader()? {
                n_locals += u64::from(l?.0);
            }
            let (mut c, mut ci, mut bt, mut n) = (0u64, 0u64, 0u64, 0u64);
            for op in body.get_operators_reader()? {
                n += 1;
                match op? {
                    Operator::Call { .. } => c += 1,
                    Operator::CallIndirect { .. } => ci += 1,
                    Operator::BrTable { .. } => bt += 1,
                    _ => {}
                }
            }
            funcs += 1;
            locals += n_locals;
            calls += c;
            indirect += ci;
            tables += bt;
            ops += n;
            rows.push((n_locals * (c + ci), n_locals as u32, c + ci, n));
            let _ = index;
            index += 1;
        }
    }
    println!(
        "{funcs} functions, {ops} operators, {locals} locals ({:.1} per function), {calls} calls, {indirect} indirect calls, {tables} br_tables",
        locals as f64 / funcs.max(1) as f64
    );
    println!("sum of locals x call sites: {}", rows.iter().map(|r| r.0).sum::<u64>());
    rows.sort_by_key(|r| std::cmp::Reverse(r.0));
    for (product, l, c, n) in rows.iter().take(top) {
        println!("  locals {l:5} x calls {c:6} = {product:9}   ({n} operators)");
    }
    Ok(())
}
