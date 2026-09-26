//! A one-command build: bytecode to an object, and, when a binary is asked
//! for, the object linked against a runtime. Ash's CLI is one caller; a host
//! that runs Ash as a library builds the same way, with its own runtime.

use std::path::PathBuf;

/// What to build, and where to put it.
pub struct AotRequest<'a> {
    /// The bytecode to compile.
    pub file: &'a std::path::Path,
    /// The object file. Scratch, and removed, when `exe` is set.
    pub out: &'a std::path::Path,
    /// The binary to link, if this is a one-command build.
    pub exe: Option<&'a std::path::Path>,
    /// The runtime to link against, if the caller named one.
    pub runtime: Option<&'a std::path::Path>,
    /// Target triple; this machine's if absent.
    pub target: Option<String>,
    /// Call-site profile to devirtualize from.
    pub pgo: Option<String>,
    /// Emit even if some functions could not be lowered.
    pub allow_refused: bool,
    /// HashLink generation the staged runtime must answer to.
    pub abi_version: u32,
    pub quiet: bool,
}

/// Compile the bytecode to a native object instead of running it.
///
/// The same lowering the JIT uses, stopped one step earlier: everything up to
/// the middle end is target-independent, and only the tail differs -- an
/// address to jump to for the JIT, a file for this. The object defines `main`
/// and `ash_module_init` and imports the runtime by symbol, so linking it
/// against libash_std.a produces a binary with no bytecode in it.
pub fn emit_aot(request: AotRequest<'_>) -> anyhow::Result<()> {
    let AotRequest {
        file,
        out,
        exe,
        runtime,
        target,
        pgo,
        allow_refused,
        abi_version,
        quiet,
    } = request;
    // A profile, if one was asked for. Advisory: a stale file costs a compare.
    if let Some(pgo) = &pgo {
        let path = if pgo.is_empty() {
            file.with_extension("prof")
        } else {
            PathBuf::from(pgo)
        };
        match std::fs::read_to_string(&path) {
            Ok(text) => {
                let n = crate::callsite_profile::load_profile(&text);
                if !quiet {
                    eprintln!("[ash] pgo: loaded {n} caller(s) from {}", path.display());
                }
            }
            Err(e) => eprintln!("[ash] pgo: could not read {}: {e}", path.display()),
        }
    }

    let triple = target.unwrap_or_else(|| {
        inkwell::targets::TargetMachine::get_default_triple()
            .as_str()
            .to_string_lossy()
            .into_owned()
    });

    // Leaked on purpose: the module borrows the context for as long as it
    // exists, and this one lives until the process ends.
    let context: &'static inkwell::context::Context =
        Box::leak(Box::new(inkwell::context::Context::create()));
    // Decided before anything is lowered: a function's AIR is built once,
    // through the shared cache, and on a target whose frames record their own
    // positions every body needs the markers from its first lowering.
    crate::air_pipeline::set_shadow_frames(
        crate::target_abi::TargetAbi::for_triple(&triple)?.shadow_call_stack,
    );
    let mut jit = crate::llvm::module::JITModule::new_aot_for_target(context, file, &triple)?;

    let findexes: Vec<usize> = jit
        .bytecode_functions()
        .iter()
        .map(|f| f.findex as usize)
        .collect();
    // A build is a minute of silence otherwise, and silence is
    // indistinguishable from a hang.
    crate::progress::enable(!quiet);
    crate::progress::begin("lowering", findexes.len() as u64);
    let (mut lowered, mut refused) = (0usize, Vec::new());
    // Per-function timing, on the same terms the tier log reports a JIT
    // compile. Without it the only cost figure AOT produces is the total, and a
    // single pathological function is invisible inside it: one measured at 47s
    // in the JIT could not be attributed here at all, and a whole-emit A/B
    // (O1 274s vs O2 298s over 8577 functions) cannot separate one outlier
    // from a broad shift. `ASH_AOT_SLOW_MS` sets the threshold.
    let slow_ms: u128 = std::env::var("ASH_AOT_SLOW_MS")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(500);
    let mut slowest: Vec<(u128, usize)> = Vec::new();
    for fx in &findexes {
        let began = std::time::Instant::now();
        let outcome = jit.promote_function_strict(*fx);
        let took = began.elapsed().as_millis();
        crate::progress::advance(1);
        if took >= slow_ms {
            crate::progress::detail(&format!("[aot] findex={fx} took {took}ms"));
        }
        slowest.push((took, *fx));
        match outcome {
            Ok(_) => lowered += 1,
            Err(e) => refused.push((*fx, format!("{e}"))),
        }
    }
    slowest.sort_unstable_by_key(|&(t, _)| std::cmp::Reverse(t));
    let total: u128 = slowest.iter().map(|(t, _)| t).sum();
    crate::progress::detail(&format!("[aot] lowering {}ms total; slowest:", total));
    for (took, fx) in slowest.iter().take(5) {
        crate::progress::detail(&format!(
            "[aot]   findex={fx} {took}ms ({:.1}% of lowering)",
            100.0 * *took as f64 / total.max(1) as f64
        ));
    }

    // A refused function is not a warning: it lowers to a throw, so a binary
    // containing one aborts the moment that path is reached -- and exiting 0
    // after writing it means the failure surfaces at run time, on a machine
    // that may not be this one. The commonest cause is an HDLL primitive,
    // which resolves through dlopen and has no symbol to bind ahead of time.
    if !refused.is_empty() && !allow_refused {
        let mut msg = format!(
            "{} function(s) could not be lowered; refusing to write a binary \
             that would abort when one is reached:",
            refused.len()
        );
        for (fx, e) in refused.iter().take(10) {
            msg.push_str(&format!(
                "\n    findex={fx}: {}",
                e.lines().next().unwrap_or("")
            ));
        }
        if refused.len() > 10 {
            msg.push_str(&format!("\n    ... and {} more", refused.len() - 10));
        }
        msg.push_str("\n  --allow-refused emits anyway, if those paths are never taken.");
        anyhow::bail!(msg);
    }

    // Every function type the program contains gets a trampoline, so a
    // dynamic call can be looked up rather than assembled. Only a target that
    // cannot assemble one needs them; see `aot_trampoline`.
    if triple.to_ascii_lowercase().starts_with("wasm") {
        let emitted = jit.emit_call_trampolines()?;
        if !quiet {
            eprintln!("[aot] {emitted} call trampolines");
        }
    }

    jit.finalize_aot_data()?;
    jit.emit_late_init()?;
    jit.emit_main()?;
    let shared_runtime = jit.aot_needs_shared_runtime();
    // More than one shard splits the middle end and codegen across threads
    // (see `aot_shard`); one shard is the single-module path, which also
    // honours ASH_AOT_NO_OPT and ASH_AOT_DUMP_IR itself.
    // A wasm OBJECT is the one output the shards cannot make: ash's linker
    // produces a finished module, not a relocatable one, and `ld -r` does
    // not read wasm. Only a `--build` shards there.
    let wasm = crate::llvm::aot_link::is_wasm_triple(&triple);
    let shards = if wasm && exe.is_none() {
        1
    } else {
        crate::llvm::aot_shard::shard_count_for(&triple)
    };
    if !quiet && shards == 1 && crate::llvm::aot_shard::shard_count() > 1 {
        if wasm {
            eprintln!(
                "[aot] one module: a wasm object cannot be sharded, since ash's linker emits a module and `ld -r` does not read wasm; `--build` would shard"
            );
        } else {
            eprintln!(
                "[aot] cross-compiling to {triple}: one module, since `ld -r` joins the shards and reads the host's object format only"
            );
        }
    }
    // Heading for a binary, the parts go straight to the linker: it takes any
    // number of objects, so joining them into one first would rewrite every
    // byte for nothing.
    let mut objects: Vec<PathBuf> = vec![out.to_path_buf()];
    let bytes = if shards > 1 && exe.is_some() {
        let parts = jit.emit_object_parts(&triple, out, shards, quiet)?;
        let bytes = parts
            .iter()
            .filter_map(|p| std::fs::metadata(p).ok().map(|m| m.len()))
            .sum();
        objects = parts;
        bytes
    } else if shards > 1 {
        jit.emit_object_sharded(&triple, out, shards, quiet)?
    } else {
        // ASH_AOT_NO_OPT leaves the module unoptimised. O3 inlines
        // ash_module_init straight into main, so a fault during startup
        // surfaces with no frame naming the routine it happened in -- which
        // is exactly when you most want one.
        if std::env::var("ASH_AOT_NO_OPT").is_err() {
            let began = std::time::Instant::now();
            jit.optimize_module()?;
            if !quiet {
                crate::progress::detail(&format!(
                    "[aot] middle end {}ms",
                    began.elapsed().as_millis()
                ));
            }
        }
        // ASH_AOT_DUMP_IR=<path> writes the module beside the object. Reading
        // the IR is how every AOT defect in this file was actually found;
        // making it reachable only from an example was a false economy.
        if let Ok(dir) = std::env::var("ASH_AOT_DUMP_IR") {
            let p = std::path::Path::new(&dir);
            let dest = if p.is_dir() {
                p.join("module.ll")
            } else {
                p.to_path_buf()
            };
            jit.write_ir(&dest)?;
            if !quiet {
                crate::progress::note(&format!("[ash] wrote IR to {}", dest.display()));
            }
        }
        let began = std::time::Instant::now();
        let bytes = jit.emit_object(&triple, out)?;
        if !quiet {
            crate::progress::detail(&format!("[aot] codegen {}ms", began.elapsed().as_millis()));
        }
        bytes
    };

    // The stages are over; what follows is the summary, and a bar under it
    // would be a finished stage left on screen.
    crate::progress::finish();
    if !quiet {
        crate::progress::note(&format!(
            "[ash] lowered {lowered}/{} functions, {} refused",
            findexes.len(),
            refused.len()
        ));
        for (fx, e) in refused.iter().take(5) {
            crate::progress::note(&format!(
                "[ash]   findex={fx}: {}",
                e.lines().next().unwrap_or("")
            ));
        }
        if pgo.is_some() {
            let loaded = crate::callsite_profile::aot_profile_size();
            let hits = crate::callsite_profile::aot_profile_hits();
            if loaded > 0 && hits == 0 {
                crate::progress::note(&format!(
                    "[ash] pgo: WARNING none of the {loaded} profiled caller(s) matched \
                     this bytecode -- the profile is stale, regenerate it"
                ));
            } else if loaded > 0 {
                crate::progress::note(&format!(
                    "[ash] pgo: {hits} of {loaded} profiled caller(s) matched"
                ));
            }
        }
        if exe.is_some() {
            crate::progress::note(&format!(
                "[ash] compiled {} object bytes for {triple}",
                bytes
            ));
        } else {
            crate::progress::note(&format!(
                "[ash] wrote {} ({bytes} bytes) for {triple}",
                out.display()
            ));
        }
        if shared_runtime {
            // An HDLL brings its own copy of the runtime unless the binary
            // shares one, and two collectors in a process crash as soon as one
            // meets the other's objects. So this object must take the runtime
            // as a library, and the HDLLs must sit beside the binary.
            crate::progress::note(
                "[ash] this program loads HDLLs, so it takes the SHARED runtime, \
                 staged beside the binary. The .hdll files must sit there too. \
                 Do not link the static runtime into a program that loads HDLLs: \
                 it builds, and then the HDLL loads a second copy of the runtime \
                 and the two collectors meet.",
            );
        } else if exe.is_none() {
            crate::progress::note(&format!(
                "[ash] this is an object; `--build {}` would have linked it too",
                out.with_extension("").display()
            ));
        }
    }

    if let Some(exe) = exe {
        let kind = if shared_runtime {
            crate::llvm::aot_link::Runtime::Shared
        } else {
            crate::llvm::aot_link::Runtime::Static
        };
        let linked = crate::llvm::aot_link::link_executable(
            &objects,
            exe,
            &triple,
            kind,
            runtime,
            abi_version,
            quiet,
        );
        // The objects are scratch either way. Keeping them after a failure
        // only helps if someone is told they exist.
        if linked.is_err() {
            crate::progress::note(&format!(
                "[ash] the objects are left beside {} for inspection",
                exe.display()
            ));
        } else {
            for part in &objects {
                let _ = std::fs::remove_file(part);
            }
        }
        linked?;
    }
    Ok(())
}
