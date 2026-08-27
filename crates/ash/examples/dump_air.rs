//! Print a function's optimized AIR — the first panel of the AIR / CLIF /
//! LLVM side-by-side latency investigations read (CLIF: `ASH_CL_DUMP`,
//! LLVM: `ASH_DUMP_FN_IR`).
//!
//! Usage: dump_air <file.hl> <findex> [--ops]
//!
//! `--ops` also prints the serialized opcode array — what the interpreter
//! actually executes — with block-start pcs marked, so an interp-bucket
//! sample can be read against the same numbering an OSR site uses.
fn main() -> anyhow::Result<()> {
    let mut args = std::env::args().skip(1);
    let path = args
        .next()
        .expect("usage: dump_air <file.hl> <findex> [--ops]");
    let selection = args.next().expect("findex or type:<index>");
    if let Some(index) = selection.strip_prefix("type:") {
        let index: usize = index.parse()?;
        println!("type[{index}] = {:#?}", bc_type(&path, index)?);
        return Ok(());
    }
    if let Some(needle) = selection.strip_prefix("debug:") {
        ash_core::native_lib::init_std_library()?;
        let bc = ash_core::bytecode::BytecodeDecoder::decode(std::path::Path::new(&path))?;
        for f in &bc.functions {
            let mut locations = Vec::new();
            for pair in f.debug.as_chunks::<2>().0 {
                let file = bc
                    .debug_files
                    .get(pair[0] as usize)
                    .map(String::as_str)
                    .unwrap_or("?");
                if !file.contains(needle) {
                    continue;
                }
                let location = format!("{file}:{}", pair[1]);
                if locations.last() != Some(&location) {
                    locations.push(location);
                }
            }
            if !locations.is_empty() {
                println!(
                    "findex={} {} — {}",
                    f.findex,
                    f.name(),
                    locations.join(", ")
                );
            }
        }
        return Ok(());
    }
    let want: i32 = selection.parse()?;
    let flags: Vec<String> = args.collect();
    let with_ops = flags.iter().any(|a| a == "--ops");
    let with_values = flags.iter().any(|a| a == "--values");
    let with_debug = flags.iter().any(|a| a == "--debug");

    ash_core::native_lib::init_std_library()?;
    let bc = ash_core::bytecode::BytecodeDecoder::decode(std::path::Path::new(&path))?;
    let m = ash_core::air_pipeline::AshModule::new(&bc);
    for f in &bc.functions {
        if f.findex != want {
            continue;
        }
        let opt = ash_core::air_pipeline::optimized(&m, f)
            .map_err(|e| anyhow::anyhow!("pipeline: {}", e.brief()))?;
        println!("=== AIR (optimized) findex={want} {} ===", f.name());
        println!("signature type[{}] = {:#?}", f.type_.0, bc.types[f.type_.0]);
        if with_debug {
            let mut locations = Vec::new();
            for pair in f.debug.as_chunks::<2>().0 {
                let file = bc
                    .debug_files
                    .get(pair[0] as usize)
                    .map(String::as_str)
                    .unwrap_or("?");
                let location = format!("{file}:{}", pair[1]);
                if locations.last() != Some(&location) {
                    locations.push(location);
                }
            }
            println!("debug locations: {}", locations.join(", "));
        }
        if with_values {
            println!("=== values (AIR value: HL register, type, kind) ===");
            for (index, value) in opt.ir.values.iter().enumerate() {
                println!(
                    "v{index}: r{} type[{}] kind={}",
                    value.reg, value.ty.0, bc.types[value.ty.0 as usize].kind
                );
            }
        }
        println!("{}", opt.ir.dump());
        if with_ops {
            println!("=== serialized ops (what the interpreter runs) ===");
            let starts: std::collections::HashMap<usize, usize> = opt
                .ser
                .block_pcs
                .iter()
                .enumerate()
                .map(|(b, &pc)| (pc, b))
                .collect();
            for (pc, op) in opt.ser.ops.iter().enumerate() {
                match starts.get(&pc) {
                    Some(b) => println!("b{b:<3} {pc:4}: {op:?}"),
                    None => println!("     {pc:4}: {op:?}"),
                }
            }
        }
        return Ok(());
    }
    if let Some(native) = bc.natives.iter().find(|native| native.findex == want) {
        println!(
            "findex={want} native {}@{} type={}",
            native.lib, native.name, native.type_.0
        );
        return Ok(());
    }
    anyhow::bail!("findex {want} not found")
}

fn bc_type(path: &str, index: usize) -> anyhow::Result<ash_core::types::HLType> {
    ash_core::native_lib::init_std_library()?;
    let bc = ash_core::bytecode::BytecodeDecoder::decode(std::path::Path::new(path))?;
    bc.types
        .get(index)
        .cloned()
        .ok_or_else(|| anyhow::anyhow!("type {index} out of range"))
}
