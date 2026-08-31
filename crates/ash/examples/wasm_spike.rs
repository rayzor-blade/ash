//! Phase 0 of docs/wasm-target.md: can LLVM's WebAssembly backend emit an
//! object from the IR our AIR lowering produces?
//!
//! Deliberately minimal. No runtime, no GC, no linking — this exists to find
//! out, in a day rather than after a runtime port, whether the lowering says
//! anything the wasm backend refuses.
//!
//! Usage: wasm_spike <file.hl> [out.o]
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::OptimizationLevel;

fn main() -> anyhow::Result<()> {
    let mut args = std::env::args().skip(1);
    let path = args.next().expect("usage: wasm_spike <file.hl> [out.o]");
    let out = args.next().unwrap_or_else(|| "/tmp/spike.o".to_string());

    Target::initialize_webassembly(&InitializationConfig::default());
    let triple = TargetMachine::get_default_triple();
    println!("host triple : {}", triple.as_str().to_string_lossy());

    let wasm = inkwell::targets::TargetTriple::create("wasm32-unknown-unknown");
    let target = Target::from_triple(&wasm)
        .map_err(|e| anyhow::anyhow!("no wasm32 target: {e}"))?;
    println!("wasm target : {} ({})", target.get_name().to_string_lossy(),
             target.get_description().to_string_lossy());

    let machine = target
        .create_target_machine(
            &wasm,
            "generic",
            "",
            OptimizationLevel::Aggressive,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .ok_or_else(|| anyhow::anyhow!("could not create a wasm32 TargetMachine"))?;
    println!("machine     : created");

    // A module of our own making first: this isolates "can the backend emit"
    // from "does OUR lowering emit something it accepts".
    let ctx = inkwell::context::Context::create();
    let module = ctx.create_module("spike");
    module.set_triple(&wasm);
    let i32t = ctx.i32_type();
    let f = module.add_function("add", i32t.fn_type(&[i32t.into(), i32t.into()], false), None);
    let bb = ctx.append_basic_block(f, "entry");
    let b = ctx.create_builder();
    b.position_at_end(bb);
    let sum = b.build_int_add(
        f.get_nth_param(0).unwrap().into_int_value(),
        f.get_nth_param(1).unwrap().into_int_value(),
        "sum",
    )?;
    b.build_return(Some(&sum))?;

    machine
        .write_to_file(&module, FileType::Object, std::path::Path::new(&out))
        .map_err(|e| anyhow::anyhow!("emit failed: {e}"))?;
    let bytes = std::fs::metadata(&out)?.len();
    println!("emitted     : {out} ({bytes} bytes)");

    // Now the real question: what does OUR pipeline produce for a real
    // function, and does the backend accept it?
    ash_core::native_lib::choose_std_linkage(std::path::Path::new(&path));
    ash_core::native_lib::init_std_library()?;
    let bc = ash_core::bytecode::BytecodeDecoder::decode(std::path::Path::new(&path))?;
    let m = ash_core::air_pipeline::AshModule::new(&bc);
    let mut ok = 0usize;
    let mut refused = 0usize;
    for f in bc.functions.iter().take(20) {
        match ash_core::air_pipeline::optimized(&m, f) {
            Ok(_) => ok += 1,
            Err(_) => refused += 1,
        }
    }
    println!("air         : {ok} lowered, {refused} refused (first 20 functions)");
    println!();
    println!("NEXT: lower one of those to LLVM IR through llvm/function.rs and");
    println!("emit THAT. Blocked on JITModule owning an MCJIT engine — see");
    println!("docs/wasm-target.md Phase 2.");
    Ok(())
}
