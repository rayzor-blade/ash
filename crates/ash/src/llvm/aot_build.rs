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
    /// The host's natives it links directly, by `(lib, name)`: each called
    /// by its symbol, casting at the boundary. Empty for a program that
    /// uses none.
    pub links: std::collections::HashMap<(String, String), crate::native_lib::HostLink>,
    /// Relocatable objects emitted by other language frontends. They join the
    /// final executable through Ash's linker and remain owned by the caller.
    pub objects: Vec<PathBuf>,
    /// Instrument a wasm build so a fiber can suspend inside it, as
    /// `ASH_WASM_FIBERS=1` does; either one turns it on. Ignored for other
    /// targets.
    pub wasm_fibers: bool,
    /// Program members to define C functions for, which the host's other
    /// objects call by symbol; see `host_export`.
    pub exports: Vec<crate::host_export::HostExport>,
    /// Function types whose values the host supplies; see
    /// `host_export::HostClosure`.
    pub closures: Vec<crate::host_export::HostClosure>,
    /// Classes, by bytecode name, whose instances (subclasses' too) the host
    /// wants this many bytes of its own after: allocated, never read,
    /// written or copied by the program.
    pub object_tails: Vec<(String, usize)>,
    /// Classes, by bytecode name, whose host attachment needs notification
    /// when an instance dies. Subclasses inherit the policy.
    pub object_drops: Vec<String>,
}

/// Compile the bytecode to a native object instead of running it.
///
/// The same lowering the JIT uses, stopped one step earlier: everything up to
/// the middle end is target-independent, and only the tail differs -- an
/// address to jump to for the JIT, a file for this. The object defines `main`
/// and `ash_module_init` and imports the runtime by symbol, so linking it
/// against libash_std.a produces a binary with no bytecode in it.
pub fn emit_aot(request: AotRequest<'_>) -> anyhow::Result<()> {
    emit_aot_with_options(request, crate::aot_options::AotOptions::default())
}

pub fn emit_aot_with_options(request: AotRequest<'_>, options: crate::aot_options::AotOptions) -> anyhow::Result<()> {
    let _options = crate::aot_options::enter(options)?;
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
        links,
        objects: language_objects,
        wasm_fibers,
        exports,
        closures,
        object_tails,
        object_drops,
    } = request;
    // A process may build several programs. These caches are keyed by
    // findex or caller, which name different code in each, so a build
    // starts with them empty.
    crate::air_pipeline::invalidate_optimized();
    crate::llvm::air::invalidate_ceilings();
    crate::callsite_profile::clear_profile();
    crate::native_lib::set_host_links(&links);
    // A profile, if one was asked for. Advisory: a stale file costs a compare.
    let mut profile_identity = None;
    if let Some(pgo) = &pgo {
        let path = if pgo.is_empty() {
            file.with_extension("prof")
        } else {
            PathBuf::from(pgo)
        };
        match std::fs::read_to_string(&path) {
            Ok(text) => {
                profile_identity = Some(crate::air_disk_cache::hash_debug(&text));
                let n = crate::callsite_profile::load_profile(&text);
                if !quiet {
                    eprintln!("[ash] pgo: loaded {n} caller(s) from {}", path.display());
                }
            }
            Err(e) => eprintln!("[ash] pgo: could not read {}: {e}", path.display()),
        }
    }

    // A target the caller named is built for a generic CPU, the host's own
    // triple included; with none named, the host CPU is the point of building
    // here. Reset on every call: a process may build several programs.
    let named = target.is_some();
    let _cpu_policy = crate::target_abi::CpuPolicy::enter(named);
    let triple = target
        .map(|t| crate::target_abi::normalize_triple(&t))
        .unwrap_or_else(|| {
            inkwell::targets::TargetMachine::get_default_triple()
                .as_str()
                .to_string_lossy()
                .into_owned()
        });
    let _air_cache = crate::air_disk_cache::enter(
        &(&triple, abi_version, named, wasm_fibers,
            std::env::var_os("ASH_WASM_FIBERS"), profile_identity),
        runtime.is_none() && links.is_empty() && language_objects.is_empty()
            && exports.is_empty() && closures.is_empty() && object_tails.is_empty() && object_drops.is_empty(),
    );

    // Decided before anything is lowered: a function's AIR is built once,
    // through the shared cache, and on a target whose frames record their own
    // positions every body needs the markers from its first lowering.
    // A wasm triple's ABI comes from its name: nothing of LLVM's is made for it.
    let abi = if crate::llvm::aot_link::is_wasm_triple(&triple) {
        crate::target_abi::TargetAbi::for_triple_str(&triple)?
    } else {
        crate::target_abi::TargetAbi::for_triple(&triple)?
    };
    crate::air_pipeline::set_shadow_frames(abi.shadow_call_stack);
    // A wasm program is built by the direct AIR-to-wasm backend: no LLVM
    // context is ever made for it.
    if crate::llvm::aot_link::is_wasm_triple(&triple) {
        return emit_wasm(
            WasmBuild {
                file,
                out,
                exe,
                runtime,
                triple: &triple,
                allow_refused,
                abi_version,
                quiet,
                wasm_fibers,
                profiled: pgo.is_some(),
            },
            &crate::wasm_program::Host {
                exports: &exports,
                closures: &closures,
                object_tails: &object_tails,
                object_drops: &object_drops,
            },
            language_objects,
        );
    }
    if !quiet {
        eprintln!(
            "[aot] optimization: air={:?} llvm={} codegen={}",
            crate::air_pipeline::default_level(),
            crate::aot_options::pass_pipeline("default<O3>"),
            crate::aot_options::codegen_level()?
        );
    }

    // Leaked on purpose: the module borrows the context for as long as it
    // exists, and this one lives until the process ends.
    crate::llvm_init::ensure();
    let context: &'static inkwell::context::Context =
        Box::leak(Box::new(inkwell::context::Context::create()));
    // A one-command build of a native executable nothing else registers into
    // is a fresh standalone program, which is what tree fusion's contract
    // needs; an object a host links, or a wasm module, is not.
    let standalone = exe.is_some()
        && runtime.is_none()
        && links.is_empty()
        && language_objects.is_empty()
        && exports.is_empty()
        && closures.is_empty()
        && object_tails.is_empty()
        && object_drops.is_empty()
        && crate::tree_fusion::enabled();
    let began = std::time::Instant::now();
    let mut jit = if standalone {
        crate::llvm::module::JITModule::new_aot_standalone(context, file, &triple)?
    } else {
        crate::llvm::module::JITModule::new_aot_with_host(
            context,
            file,
            &triple,
            &exports,
            &closures,
            &object_tails,
            &object_drops,
        )?
    };

    crate::air_disk_cache::stage("module_setup", began.elapsed());
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
    let began = std::time::Instant::now();
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

    jit.emit_host_exports()?;
    jit.finalize_aot_data()?;
    jit.emit_late_init()?;
    jit.emit_main()?;
    // The resolvers a phone program weakly references; the link names them.
    let native_symbols: Vec<String> = if jit.target_abi.static_natives {
        jit.aot_hdll_natives
            .iter()
            .map(|(_, prim)| format!("hlp_{prim}"))
            .collect()
    } else {
        Vec::new()
    };
    crate::air_disk_cache::stage("llvm_ir", began.elapsed());
    let backend_began = std::time::Instant::now();
    // A mobile program links the runtime statically whatever it names.
    let shared_runtime =
        jit.aot_needs_shared_runtime() && crate::llvm::mobile_sdk::os(&triple).is_none();
    // More than one shard splits the middle end and codegen across threads
    // (see `aot_shard`); one shard is the single-module path, which also
    // honours ASH_AOT_NO_OPT and ASH_AOT_DUMP_IR itself.
    let shards = crate::llvm::aot_shard::shard_count_for(&triple);
    if !quiet && shards == 1 && crate::llvm::aot_shard::shard_count() > 1 {
        eprintln!(
            "[aot] cross-compiling to {triple}: one module, since `ld -r` joins the shards and reads the host's object format only"
        );
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
    crate::air_disk_cache::stage("llvm_backend", backend_began.elapsed());
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
                "[ash] compiled {} object bytes for {triple}, cpu {}",
                bytes,
                crate::target_abi::cpu_for(&triple)
            ));
        } else {
            crate::progress::note(&format!(
                "[ash] wrote {} ({bytes} bytes) for {triple}, cpu {}",
                out.display(),
                crate::target_abi::cpu_for(&triple)
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
        let scratch_objects = objects.clone();
        objects.extend(language_objects);
        let began = std::time::Instant::now();
        let linked = crate::llvm::aot_link::link_executable(
            &objects,
            exe,
            &triple,
            kind,
            runtime,
            abi_version,
            wasm_fibers,
            quiet,
            &native_symbols,
        );
        // The objects are scratch either way. Keeping them after a failure
        // only helps if someone is told they exist.
        if linked.is_err() {
            crate::progress::note(&format!(
                "[ash] the objects are left beside {} for inspection",
                exe.display()
            ));
        } else {
            for part in &scratch_objects {
                let _ = std::fs::remove_file(part);
            }
        }
        linked?;
        crate::air_disk_cache::stage("link", began.elapsed());
    }
    Ok(())
}

/// What a wasm build needs beyond the host's additions.
struct WasmBuild<'a> {
    file: &'a std::path::Path,
    out: &'a std::path::Path,
    exe: Option<&'a std::path::Path>,
    runtime: Option<&'a std::path::Path>,
    triple: &'a str,
    allow_refused: bool,
    abi_version: u32,
    quiet: bool,
    wasm_fibers: bool,
    /// Whether a call-site profile was asked for.
    profiled: bool,
}

/// `ASH_WASM_DIRECT` holds measurement switches for a wasm build: `nopolls`
/// leaves out the safepoint test at loop heads, `noshadow` the shadow frames.
/// A build with either is not sound to run. A wasm build is always the direct
/// backend, so a value that once chose another is refused.
fn wasm_switches(raw: &str) -> anyhow::Result<(bool, bool)> {
    let (mut polls, mut shadow) = (true, true);
    for word in raw.split(',').map(str::trim).filter(|w| !w.is_empty()) {
        match word {
            "nopolls" => polls = false,
            "noshadow" => shadow = false,
            other => anyhow::bail!(
                "ASH_WASM_DIRECT={other}: a wasm build is always the direct backend; \
                 the switches are `nopolls` and `noshadow`"
            ),
        }
    }
    Ok((polls, shadow))
}

/// A wasm program, written as one relocatable object and, for a binary,
/// linked with the runtime.
fn emit_wasm(
    build: WasmBuild<'_>,
    host: &crate::wasm_program::Host,
    language_objects: Vec<PathBuf>,
) -> anyhow::Result<()> {
    let WasmBuild {
        file,
        out,
        exe,
        runtime,
        triple,
        allow_refused,
        abi_version,
        quiet,
        wasm_fibers,
        profiled,
    } = build;
    let (switch_polls, switch_shadow) =
        wasm_switches(&std::env::var("ASH_WASM_DIRECT").unwrap_or_default())?;
    let polls = switch_polls && !matches!(std::env::var("ASH_FIBER_POLLS").as_deref(), Ok("0"));
    let shadow = switch_shadow && crate::target_abi::TargetAbi::for_triple_str(triple)?.shadow_call_stack;

    if !quiet {
        eprintln!("[aot] optimization: air={:?}", crate::air_pipeline::default_level());
    }
    let began = std::time::Instant::now();
    let bytes = crate::wasm_program::build(file, triple, polls, shadow, allow_refused, host, quiet)?;
    crate::air_disk_cache::stage("direct_program", began.elapsed());
    if profiled && !quiet {
        let loaded = crate::callsite_profile::aot_profile_size();
        let hits = crate::callsite_profile::aot_profile_hits();
        if loaded > 0 && hits == 0 {
            eprintln!(
                "[ash] pgo: WARNING none of the {loaded} profiled caller(s) matched \
                 this bytecode -- the profile is stale, regenerate it"
            );
        } else if loaded > 0 {
            eprintln!("[ash] pgo: {hits} of {loaded} profiled caller(s) matched");
        }
    }

    let Some(exe) = exe else {
        std::fs::write(out, &bytes)?;
        if !quiet {
            eprintln!(
                "[ash] wrote {} ({} bytes) for {triple}\n\
                 [ash] this is an object; `--build {}` would have linked it too",
                out.display(),
                bytes.len(),
                out.with_extension("").display()
            );
        }
        return Ok(());
    };

    let object = out.with_extension("direct.o");
    std::fs::write(&object, &bytes)?;
    // Any native outside the runtime that the host did not link is an HDLL,
    // which brings its own copy of the runtime unless this program shares one.
    let host_libraries = crate::bytecode::BytecodeDecoder::decode_for_abi(
        file,
        &crate::target_abi::TargetAbi::for_triple_str(triple)?,
    )?
    .natives
    .iter()
    .any(|n| {
        !crate::native_lib::is_runtime_lib(&n.lib) && crate::native_lib::host_link(&n.lib, &n.name).is_none()
    });
    let kind = if host_libraries {
        crate::llvm::aot_link::Runtime::Shared
    } else {
        crate::llvm::aot_link::Runtime::Static
    };
    let mut objects = vec![object.clone()];
    objects.extend(language_objects);
    let began = std::time::Instant::now();
    let linked = crate::llvm::aot_link::link_executable(
        &objects,
        exe,
        triple,
        kind,
        runtime,
        abi_version,
        wasm_fibers,
        quiet,
        &[],
    );
    if linked.is_ok() {
        let _ = std::fs::remove_file(&object);
    }
    crate::air_disk_cache::stage("link", began.elapsed());
    linked
}

#[cfg(test)]
mod wasm_switch_tests {
    use super::wasm_switches;

    #[test]
    fn a_wasm_build_takes_only_measurement_switches() {
        assert_eq!(wasm_switches("").unwrap(), (true, true));
        assert_eq!(wasm_switches("nopolls").unwrap(), (false, true));
        assert_eq!(wasm_switches("noshadow, nopolls").unwrap(), (false, false));
        for removed in ["0", "off", "llvm", "1", "only", "log"] {
            assert!(wasm_switches(removed).is_err(), "{removed}");
        }
    }
}
