//! MVP for ahead-of-time compilation: AIR -> LLVM -> object file, with the
//! runtime referenced by symbol rather than by baked address.
//!
//! This is the fork described in docs/wasm-target.md: everything up to
//! `run_middle_end` is target-independent, and only the tail differs between
//! JIT (`get_function_address`) and AOT (`emit_object`).
//!
//! Usage: aot_spike <file.hl> <out.o> [triple]
use anyhow::Result;

const USAGE: &str = "usage: aot_spike <file.hl> <out.o> [triple] [--pgo[=<profile>]]";

fn main() -> Result<()> {
    // --pgo, with or without a path. Bare --pgo takes the profile beside the
    // bytecode, so the common case needs no path at all.
    let mut pgo: Option<Option<String>> = None;
    let mut args = Vec::new();
    for a in std::env::args().skip(1) {
        if a == "--pgo" {
            pgo = Some(None);
        } else if let Some(rest) = a.strip_prefix("--pgo=") {
            pgo = Some(Some(rest.to_string()));
        } else if a == "-h" || a == "--help" {
            println!("{USAGE}");
            return Ok(());
        } else {
            args.push(a);
        }
    }
    let mut args = args.into_iter();
    let path = args.next().expect(USAGE);
    let out = args.next().expect(USAGE);
    let triple = args.next().unwrap_or_else(|| {
        inkwell::targets::TargetMachine::get_default_triple()
            .as_str()
            .to_string_lossy()
            .into_owned()
    });

    // A profile from an earlier run. Advisory: every guard it produces
    // re-checks its target at run time, so a stale or wrong file costs a
    // compare and never an answer.
    let profile_path = match &pgo {
        Some(Some(p)) => Some(p.clone()),
        // Bare --pgo: the profile beside the bytecode, the way Go looks for
        // default.pgo beside main.
        Some(None) => Some(
            std::path::Path::new(&path)
                .with_extension("prof")
                .to_string_lossy()
                .into_owned(),
        ),
        None => std::env::var("ASH_AOT_PROFILE").ok(),
    };
    let mut loaded = 0usize;
    if let Some(profile) = &profile_path {
        match std::fs::read_to_string(profile) {
            Ok(text) => {
                loaded = ash_core::callsite_profile::load_profile(&text);
                println!("pgo: loaded {loaded} caller(s) from {profile}");
            }
            Err(e) if pgo.is_some() => {
                // Asked for explicitly and not there: say so. Silently
                // emitting an unoptimised object is how a build loses a
                // measured win without anyone noticing.
                eprintln!("pgo: could not read {profile}: {e}");
            }
            Err(_) => {}
        }
    }

    let p = std::path::Path::new(&path);
    ash_core::native_lib::choose_std_linkage(p);
    ash_core::native_lib::init_std_library()?;

    let context: &'static inkwell::context::Context =
        Box::leak(Box::new(inkwell::context::Context::create()));
    let mut jit = ash_core::llvm::module::JITModule::new_aot_for_target(context, p, &triple)?;

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

    // Whether the profile actually described THIS program. Zero matches
    // against a non-empty profile means every entry is stale -- the failure
    // that used to be silent, and that costs the whole optimisation.
    if loaded > 0 {
        let hits = ash_core::callsite_profile::aot_profile_hits();
        if hits == 0 {
            eprintln!(
                "pgo: WARNING none of the {loaded} profiled caller(s) matched this \
                 bytecode -- the profile is stale. Regenerate it with \
                 ASH_AOT_PROFILE_OUT=<file> ash --mode hybrid <program>"
            );
        } else {
            println!("pgo: {hits} of {loaded} profiled caller(s) matched");
        }
    }
    Ok(())
}
