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
    if !request.quiet {
        eprintln!("[aot] optimization: air={:?} llvm={} codegen={}",
            crate::air_pipeline::default_level(), crate::aot_options::pass_pipeline("default<O3>"), crate::aot_options::codegen_level()?);
    }
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

    // Leaked on purpose: the module borrows the context for as long as it
    // exists, and this one lives until the process ends.
    crate::llvm_init::ensure();
    let context: &'static inkwell::context::Context =
        Box::leak(Box::new(inkwell::context::Context::create()));
    // Decided before anything is lowered: a function's AIR is built once,
    // through the shared cache, and on a target whose frames record their own
    // positions every body needs the markers from its first lowering.
    crate::air_pipeline::set_shadow_frames(
        crate::target_abi::TargetAbi::for_triple(&triple)?.shadow_call_stack,
    );
    // The direct wasm backend on its own, with no LLVM module at all, when it
    // can take the whole program. It is the default for a wasm build; a
    // program it cannot take goes the ordinary way.
    let (direct_mode, mode) = wasm_direct_mode();
    if crate::llvm::aot_link::is_wasm_triple(&triple)
        && let Some(exe) = exe
        && direct_mode == WasmDirect::Only
        && pgo.is_none()
    {
        let polls = !matches!(std::env::var("ASH_FIBER_POLLS").as_deref(), Ok("0"))
            && !mode.contains("nopolls");
        let shadow = crate::target_abi::TargetAbi::for_triple(&triple)?.shadow_call_stack
            && !mode.contains("noshadow");
        let began = std::time::Instant::now();
        match crate::wasm_program::build(
            file,
            &triple,
            polls,
            shadow,
            allow_refused,
            &crate::wasm_program::Host {
                exports: &exports,
                closures: &closures,
                object_tails: &object_tails,
                object_drops: &object_drops,
            },
            quiet,
        )? {
            crate::wasm_program::Outcome::Built(bytes) => {
                crate::air_disk_cache::stage("direct_program", began.elapsed());
                let object = out.with_extension("direct.o");
                std::fs::write(&object, &bytes)?;
                let host_libraries = crate::bytecode::BytecodeDecoder::decode_for_abi(
                    file,
                    &crate::target_abi::TargetAbi::for_triple(&triple)?,
                )?
                .natives
                .iter()
                .any(|n| {
                    !crate::native_lib::is_runtime_lib(&n.lib)
                        && crate::native_lib::host_link(&n.lib, &n.name).is_none()
                });
                let kind = if host_libraries {
                    crate::llvm::aot_link::Runtime::Shared
                } else {
                    crate::llvm::aot_link::Runtime::Static
                };
                let mut objects = vec![object.clone()];
                objects.extend(language_objects.iter().cloned());
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
                );
                if linked.is_ok() {
                    let _ = std::fs::remove_file(&object);
                }
                crate::air_disk_cache::stage("link", began.elapsed());
                return linked;
            }
            crate::wasm_program::Outcome::Declined(why) => {
                // Asked for by name, the backend is not a preference.
                if std::env::var_os("ASH_WASM_DIRECT").is_some_and(|v| !v.is_empty()) {
                    anyhow::bail!("the direct wasm backend declined the whole program: {why}");
                }
                if !quiet {
                    eprintln!("[aot] the direct wasm backend declined the whole program: {why}");
                }
            }
        }
    }

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
        && !triple.starts_with("wasm")
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
    // The direct wasm backend takes the functions it can and leaves the rest to
    // LLVM. Reached by `ASH_WASM_DIRECT=1`; a program the default mode
    // declined stays on LLVM.
    let direct = if crate::llvm::aot_link::is_wasm_triple(&triple) && direct_mode == WasmDirect::Mixed {
        Some(emit_direct_object(&mut jit, &triple, &findexes, out, quiet)?)
    } else {
        None
    };
    let direct_set: std::collections::HashSet<usize> = direct
        .as_ref()
        .map(|d| d.functions.iter().copied().collect())
        .unwrap_or_default();
    let began = std::time::Instant::now();
    for fx in &findexes {
        if direct_set.contains(fx) {
            lowered += 1;
            crate::progress::advance(1);
            continue;
        }
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

    jit.emit_host_exports()?;
    jit.finalize_aot_data()?;
    jit.emit_late_init()?;
    jit.emit_main()?;
    crate::air_disk_cache::stage("llvm_ir", began.elapsed());
    let backend_began = std::time::Instant::now();
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
        if shared_runtime && !triple.starts_with("wasm") {
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
        if let Some(d) = &direct {
            objects.push(d.path.clone());
        }
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

/// What the direct backend emitted for this build.
struct DirectObject {
    path: PathBuf,
    functions: Vec<usize>,
}

/// How a wasm build uses the direct backend.
#[derive(Clone, Copy, PartialEq, Eq)]
enum WasmDirect {
    /// No direct backend: LLVM emits everything.
    Llvm,
    /// Direct bodies where the backend covers them, LLVM for the rest.
    Mixed,
    /// A whole program with no LLVM module.
    Only,
}

/// `ASH_WASM_DIRECT`: unset, empty or containing `only` is the whole-program
/// build; `0`, `off` or `llvm` is LLVM alone; any other value is the mixed
/// build. `log`, `nopolls` and `noshadow` are measurement switches that may
/// be combined with the others, and a build with either of the last two is not
/// sound to run. Returns the raw value too.
fn wasm_direct_mode() -> (WasmDirect, String) {
    let raw = std::env::var("ASH_WASM_DIRECT").unwrap_or_default();
    (WasmDirect::parse(&raw), raw)
}

impl WasmDirect {
    fn parse(raw: &str) -> Self {
        if raw.is_empty() || raw.contains("only") {
            WasmDirect::Only
        } else if matches!(raw, "0" | "off" | "llvm") {
            WasmDirect::Llvm
        } else {
            WasmDirect::Mixed
        }
    }
}

#[cfg(test)]
mod wasm_direct_tests {
    use super::WasmDirect;

    #[test]
    fn a_wasm_build_is_whole_program_direct_unless_told_otherwise() {
        assert!(WasmDirect::parse("") == WasmDirect::Only);
        assert!(WasmDirect::parse("only") == WasmDirect::Only);
        assert!(WasmDirect::parse("log,only") == WasmDirect::Only);
        assert!(WasmDirect::parse("1") == WasmDirect::Mixed);
        assert!(WasmDirect::parse("log") == WasmDirect::Mixed);
        for off in ["0", "off", "llvm"] {
            assert!(WasmDirect::parse(off) == WasmDirect::Llvm);
        }
    }
}

/// Emit the functions the direct wasm backend covers as an object beside `out`,
/// and arrange the LLVM module so the two link: every direct function is
/// declared (which puts it in the dynamic-call table), and the functions the
/// object calls stay visible to the linker.
fn emit_direct_object(
    jit: &mut crate::llvm::module::JITModule<'_>,
    triple: &str,
    findexes: &[usize],
    out: &std::path::Path,
    quiet: bool,
) -> anyhow::Result<DirectObject> {
    let began = std::time::Instant::now();
    let mut symbols = std::collections::HashMap::new();
    for fx in findexes {
        symbols.insert(*fx, jit.function_symbol(*fx)?);
    }
    // `ASH_WASM_DIRECT` takes any value to turn the backend on; `noshadow`,
    // `nopolls` and `log` in it are measurement switches, and a build with
    // either of the first two is not sound to run.
    let mode = std::env::var("ASH_WASM_DIRECT").unwrap_or_default();
    let polls = !matches!(std::env::var("ASH_FIBER_POLLS").as_deref(), Ok("0"))
        && !mode.contains("nopolls");
    let shadow = crate::target_abi::TargetAbi::for_triple(triple)?.shadow_call_stack
        && !mode.contains("noshadow");
    let data = jit.aot_data_symbols();
    let emitted = crate::wasm_direct::emit_object(
        &jit.bytecode,
        &symbols,
        &data,
        ash_wasm_emit::Options {
            polls,
            shadow,
            stub_skipped: false,
        },
    );
    // Natives of host libraries are reached through slots the LLVM module
    // owns and fills at start-up.
    let wanted: Vec<(String, String)> = jit
        .bytecode
        .natives
        .iter()
        .filter_map(|n| {
            let lib = n.lib.strip_prefix('?').unwrap_or(&n.lib);
            emitted
                .data
                .contains(&format!("ash_native_{lib}_{}", n.name))
                .then(|| (lib.to_string(), n.name.clone()))
        })
        .collect();
    for (lib, prim) in wanted {
        jit.ensure_native_slot(&lib, &prim);
    }
    jit.keep_external(emitted.imports.iter().filter_map(|f| symbols.get(f).cloned()));
    jit.keep_data_external(emitted.data.iter().cloned());
    jit.define_elsewhere(emitted.functions.iter().copied());
    let path = out.with_extension("direct.o");
    std::fs::write(&path, &emitted.bytes)?;
    if mode.contains("log") {
        eprintln!("[aot] direct: {:?}", emitted.functions);
        for (f, why) in emitted.skipped.iter().take(40) {
            eprintln!("[aot] not direct: f{f}: {why}");
        }
    }
    if !quiet {
        eprintln!(
            "[aot] direct wasm backend: {} of {} functions, {} bytes",
            emitted.functions.len(),
            findexes.len(),
            emitted.bytes.len()
        );
    }
    crate::air_disk_cache::stage("direct_emit", began.elapsed());
    Ok(DirectObject {
        path,
        functions: emitted.functions,
    })
}
