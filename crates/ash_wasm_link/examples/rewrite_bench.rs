//! Times the rewrite passes a host runs on every module it loads.
//!
//! Usage: rewrite_bench <module.wasm> [repeat]
use std::time::Instant;

fn main() -> anyhow::Result<()> {
    let mut args = std::env::args().skip(1);
    let path = args.next().expect("usage: rewrite_bench <module.wasm> [repeat]");
    let repeat: usize = args.next().map(|n| n.parse()).transpose()?.unwrap_or(3);
    let bytes = std::fs::read(&path)?;
    println!("{path}: {} bytes", bytes.len());
    let time = |what: &str, f: &dyn Fn() -> anyhow::Result<usize>| -> anyhow::Result<()> {
        let mut best = f64::MAX;
        let mut size = 0;
        for _ in 0..repeat {
            let began = Instant::now();
            size = f()?;
            best = best.min(began.elapsed().as_secs_f64() * 1e3);
        }
        println!("  {what:<28} {best:8.1} ms  ({size} bytes out)");
        Ok(())
    };
    time("instrument", &|| Ok(ash_wasm_link::waits::instrument(&bytes)?.len()))?;
    time("read_side_module", &|| {
        Ok(ash_wasm_link::read_side_module(&bytes)?.map_or(0, |s| s.functions.len()))
    })?;
    time("expose_tls_base", &|| Ok(ash_wasm_link::dylink::expose_tls_base(&bytes)?.len()))?;
    time("without_initial_data", &|| {
        Ok(ash_wasm_link::dylink::without_initial_data(&bytes)?.len())
    })?;
    // The two orders a library's thread variant can be made in.
    let before = ash_wasm_link::dylink::expose_tls_base(&ash_wasm_link::waits::instrument(
        &ash_wasm_link::dylink::without_initial_data(&bytes)?,
    )?)?;
    let after = ash_wasm_link::dylink::expose_tls_base(&ash_wasm_link::dylink::without_initial_data(
        &ash_wasm_link::waits::instrument(&bytes)?,
    )?)?;
    println!("  thread variant, either order: {}", if before == after { "same bytes" } else { "DIFFERENT" });
    Ok(())
}
