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
    let path = args.next().expect("usage: dump_air <file.hl> <findex> [--ops]");
    let want: i32 = args.next().expect("findex").parse()?;
    let with_ops = args.any(|a| a == "--ops");

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
    anyhow::bail!("findex {want} not found (or is a native)")
}
