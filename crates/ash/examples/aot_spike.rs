//! MVP for ahead-of-time compilation: AIR -> LLVM -> object file, with the
//! runtime referenced by symbol rather than by baked address.
//!
//! This is the fork described in docs/wasm-target.md: everything up to
//! `run_middle_end` is target-independent, and only the tail differs between
//! JIT (`get_function_address`) and AOT (`emit_object`).
//!
//! Usage: aot_spike <file.hl> <out.o> [triple]
use anyhow::Result;

fn main() -> Result<()> {
    let mut args = std::env::args().skip(1);
    let path = args.next().expect("usage: aot_spike <file.hl> <out.o> [triple]");
    let out = args.next().expect("usage: aot_spike <file.hl> <out.o> [triple]");
    let triple = args.next().unwrap_or_else(|| {
        inkwell::targets::TargetMachine::get_default_triple()
            .as_str()
            .to_string_lossy()
            .into_owned()
    });

    // A profile from an earlier run, if one was left. Advisory: every guard it
    // produces re-checks at run time, so a stale file costs a compare.
    if let Ok(profile) = std::env::var("ASH_AOT_PROFILE") {
        match std::fs::read_to_string(&profile) {
            Ok(text) => {
                let n = ash_core::callsite_profile::load_profile(&text);
                println!("loaded {n} profiled method site(s) from {profile}");
            }
            Err(e) => eprintln!("could not read {profile}: {e}"),
        }
    }

    let p = std::path::Path::new(&path);
    ash_core::native_lib::choose_std_linkage(p);
    ash_core::native_lib::init_std_library()?;

    let context: &'static inkwell::context::Context =
        Box::leak(Box::new(inkwell::context::Context::create()));
    let mut jit = ash_core::llvm::module::JITModule::new_aot(context, p)?;

    let mut ok = 0usize;
    let mut failed: Vec<(usize, String)> = Vec::new();
    let findexes: Vec<usize> = jit
        .bytecode_functions()
        .iter()
        .map(|f| f.findex as usize)
        .collect();
    let limit: usize = std::env::var("AOT_LIMIT").ok().and_then(|v| v.parse().ok()).unwrap_or(usize::MAX);
    for fx in findexes.iter().take(limit) {
        if std::env::var("AOT_TRACE").is_ok() {
            eprintln!("lowering findex={fx}");
        }
        if let Ok(dir) = std::env::var("AOT_DUMP_IR") {
            jit.write_ir(std::path::Path::new(&dir).join("before.ll").as_path())?;
        }
        match jit.promote_function_strict(*fx) {
            Ok(_) => ok += 1,
            Err(e) => failed.push((*fx, format!("{e}"))),
        }
    }
    println!("lowered {ok}/{} functions ({} refused)", findexes.len(), failed.len());
    for (fx, e) in failed.iter().take(5) {
        println!("   findex={fx}: {}", e.lines().next().unwrap_or(""));
    }

    jit.finalize_aot_data()?;
    jit.emit_main()?;
    if std::env::var("AOT_NO_OPT").is_err() {
        jit.optimize_module()?;
    }

    jit.write_ir(std::path::Path::new(&out).with_extension("ll").as_path())?;
    let bytes = jit.emit_object(&triple, std::path::Path::new(&out))?;
    println!("emitted {out} for {triple} ({bytes} bytes)");
    println!("entrypoint findex = {}  symbol = {}",
             jit.entrypoint_findex(), jit.entrypoint_symbol());
    Ok(())
}
