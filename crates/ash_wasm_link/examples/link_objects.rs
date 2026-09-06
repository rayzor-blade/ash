//! Link objects from the command line, for testing against a known linker.
//!
//! `cargo run --example link_objects -- [--shared-memory] out.wasm a.o b.o`
use anyhow::{bail, Result};

fn main() -> Result<()> {
    let mut args: Vec<String> = std::env::args().skip(1).collect();
    let shared_memory = args.iter().any(|a| a == "--shared-memory");
    args.retain(|a| a != "--shared-memory");
    if args.len() < 2 {
        bail!("usage: link_objects [--shared-memory] <out.wasm> <object>...");
    }
    let out = &args[0];
    let mut objects = Vec::new();
    for path in &args[1..] {
        let bytes = std::fs::read(path)?;
        objects.push(ash_wasm_link::read(path, &bytes)?);
    }
    let module = ash_wasm_link::link(
        objects,
        &ash_wasm_link::LinkOptions {
            shared_memory,
            ..Default::default()
        },
    )?;
    std::fs::write(out, &module)?;
    eprintln!("linked {out} ({} bytes)", module.len());
    Ok(())
}
