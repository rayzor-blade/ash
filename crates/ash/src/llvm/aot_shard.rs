//! Parallel AOT emission: one lowered module, N object files, one link.
//!
//! LLVM optimizes and emits a module on one thread, and a game's module is
//! six minutes of O3 and codegen on one core while the other nine idle. An
//! `LLVMContext` owns everything in its module, so the work cannot be split
//! inside the context; it can be split *across* contexts. The lowered module
//! is serialized to bitcode once, each worker parses its own copy into a
//! context of its own, keeps the bodies in its shard, reduces every other body
//! to a declaration, and optimizes and emits only what it kept. The N objects
//! are then joined with `ld -r` into the single object the rest of the
//! toolchain already expects, so the link script and every recipe built on it
//! are untouched.
//!
//! Two things make a body reachable from another shard's object:
//!
//! - **Symbols.** A body is `internal` after `finalize_aot_data`, for a
//!   reason that still holds: a Haxe method called `write` would otherwise
//!   take libc's symbol at link time. Across objects a symbol has to be
//!   external, so every body and every named data global is promoted to
//!   external with **hidden** visibility, under a name no library can own:
//!   `ash_f<findex>.<name>` for bodies, `ash_h<n>.` for helpers with no
//!   findex, `ash_d<n>.` for data. Hidden keeps them out of the dynamic
//!   symbol table; the prefix keeps them out of libc's way.
//! - **Inlining.** The inliner cannot see across a declaration, and with N
//!   shards most call edges cross one. ThinLTO's answer is used here: a small
//!   body travels to every shard as `available_externally`, which the inliner
//!   may consume and codegen never emits, so the callees O3 would actually
//!   have inlined are still inlined. Large bodies are what the split is for
//!   and are stripped.
//!
//! Data lives once, in a shard of its own that owns no bodies and runs no
//! middle end. The optimizer's cost over data is what made the first cut
//! lopsided: the game's module carries 97,731 global definitions, and the
//! shard that owned them spent 90 s in O3 where every other shard spent 6 s,
//! on the small programs the same shard was 3x the rest. Nothing in that
//! walk was worth having -- data needs codegen, not optimization. In a body
//! shard a named mutable global becomes a declaration, and a named
//! `constant` rides along as `available_externally`, which the optimizer may
//! fold a load from (an `Int_7` is an immediate, not a load, exactly as in
//! the single-module build) and codegen never emits. `private` globals,
//! which have no symbol to declare, stay wherever they are referenced --
//! they are literal constants, and a duplicate costs bytes, not correctness.
//!
//! Shards are contiguous findex ranges of equal instruction weight rather than
//! round-robin: the compiler emits a class's methods together, and callers
//! sit beside their callees more often than not, so a contiguous cut keeps
//! more inlining inside a shard than any interleaving would.
//!
//! `ASH_AOT_SHARDS=1` is the old single-module path exactly, with no bitcode
//! round-trip; it is the bisect switch if a sharded build ever misbehaves.

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use anyhow::{anyhow, Result};
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::context::Context;
use inkwell::llvm_sys;
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::{Linkage, Module};
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
};
use inkwell::values::{AsValueRef, FunctionValue};
use inkwell::GlobalVisibility;

use super::module::JITModule;

/// A body at or under this many instructions is carried into every shard as
/// an `available_externally` definition so the inliner can still consume it.
/// O3's inline threshold is measured in a cost model, not instructions, but
/// anything larger than this it would rarely take, and every body carried is
/// parsed and simplified once per shard.
const INLINE_CANDIDATE_INSTRUCTIONS: usize = 80;

/// Stack for a shard's thread. The middle end recurses -- ScalarEvolution's
/// `createSCEV` walks an expression tree one frame per node -- and on the
/// game's largest bodies it went past the 2 MB a spawned thread gets by
/// default, faulting on the guard page (address 0x17000fe7c, the thread
/// stack region). The single-module path never saw this because it ran on
/// the main thread's 8 MB. The reservation is virtual; only touched pages
/// cost anything.
const SHARD_STACK_BYTES: usize = 64 << 20;

/// How many shards to emit. `ASH_AOT_SHARDS` wins; otherwise one per core,
/// capped at 8: every shard parses the whole module before it strips, so
/// memory scales with the count, and three parallel LLVM builds have already
/// put this machine into swap once.
pub fn shard_count() -> usize {
    if let Some(v) = std::env::var_os("ASH_AOT_SHARDS") {
        if let Ok(n) = v.to_string_lossy().trim().parse::<usize>() {
            return n.max(1);
        }
    }
    std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(1)
        .clamp(1, 8)
}

/// The back end for an object file targeting `triple`.
///
/// Emitting for THIS machine means naming it. `generic` on x86-64 is baseline
/// SSE2 -- no FMA3, no AVX2 -- and codegen for it cost nbody 3.9x against the
/// same IR run by the JIT, which stamps the host CPU on every function. The
/// middle end already optimizes for the host, so a generic back end also
/// leaves the two halves disagreeing about what the machine can do.
/// Cross-compiling keeps `generic`, which is the only safe answer for a
/// machine we cannot ask.
pub(crate) fn object_target_machine(triple: &str) -> Result<(TargetTriple, TargetMachine)> {
    Target::initialize_all(&InitializationConfig::default());
    let tt = TargetTriple::create(triple);
    let target = Target::from_triple(&tt).map_err(|e| anyhow!("no target for {triple}: {e}"))?;
    let host = TargetMachine::get_default_triple();
    let native = tt == host;
    let cpu = if native {
        TargetMachine::get_host_cpu_name().to_string()
    } else {
        "generic".to_string()
    };
    let features = if native {
        TargetMachine::get_host_cpu_features().to_string()
    } else {
        String::new()
    };
    let machine = target
        .create_target_machine(
            &tt,
            &cpu,
            &features,
            inkwell::OptimizationLevel::Aggressive,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .ok_or_else(|| anyhow!("could not create a TargetMachine for {triple}"))?;
    Ok((tt, machine))
}

fn instruction_count(f: FunctionValue<'_>) -> usize {
    f.get_basic_block_iter()
        .map(|bb| bb.get_instructions().count())
        .sum()
}

/// Reduce `f` to a declaration.
///
/// The C API has no `deleteBody`. Erasing instructions block by block is not
/// safe either: a branch in a later block still uses an earlier block, and a
/// value with live uses must not be destroyed. So every value is first
/// replaced by poison, which severs every instruction-to-instruction use;
/// then every instruction goes, which severs every use of a block; only then
/// do the blocks.
fn strip_body(f: FunctionValue<'_>) {
    use llvm_sys::core::*;
    use llvm_sys::LLVMTypeKind;
    unsafe {
        let fref = f.as_value_ref();
        let mut bb = LLVMGetFirstBasicBlock(fref);
        while !bb.is_null() {
            let mut inst = LLVMGetFirstInstruction(bb);
            while !inst.is_null() {
                let ty = LLVMTypeOf(inst);
                if LLVMGetTypeKind(ty) != LLVMTypeKind::LLVMVoidTypeKind {
                    LLVMReplaceAllUsesWith(inst, LLVMGetPoison(ty));
                }
                inst = LLVMGetNextInstruction(inst);
            }
            bb = LLVMGetNextBasicBlock(bb);
        }
        let mut bb = LLVMGetFirstBasicBlock(fref);
        while !bb.is_null() {
            loop {
                let inst = LLVMGetFirstInstruction(bb);
                if inst.is_null() {
                    break;
                }
                LLVMInstructionEraseFromParent(inst);
            }
            bb = LLVMGetNextBasicBlock(bb);
        }
        loop {
            let bb = LLVMGetFirstBasicBlock(fref);
            if bb.is_null() {
                break;
            }
            LLVMDeleteBasicBlock(bb);
        }
    }
    f.set_linkage(Linkage::External);
}

fn has_attribute(f: FunctionValue<'_>, name: &str) -> bool {
    let kind = Attribute::get_named_enum_kind_id(name);
    f.get_enum_attribute(AttributeLoc::Function, kind).is_some()
}

/// Cut `weights`, in order, into `shards` contiguous ranges of about equal
/// total weight.
fn partition(weights: &[(String, usize)], shards: usize) -> Vec<HashSet<String>> {
    let total: u128 = weights.iter().map(|(_, w)| *w as u128).sum::<u128>().max(1);
    let mut owned: Vec<HashSet<String>> = (0..shards).map(|_| HashSet::new()).collect();
    let mut cumulative: u128 = 0;
    for (name, weight) in weights {
        let shard = ((cumulative * shards as u128) / total) as usize;
        owned[shard.min(shards - 1)].insert(name.clone());
        cumulative += *weight as u128;
    }
    owned
}

impl<'ctx> JITModule<'ctx> {
    /// Give every body and every named data global a symbol another object
    /// can reference, and return the bodies in findex order with their
    /// instruction weights.
    fn promote_for_sharding(&self) -> Vec<(String, usize)> {
        let mut ordered: Vec<(String, usize)> = Vec::new();
        let mut recorded: HashSet<String> = HashSet::new();

        let mut by_findex: Vec<(usize, FunctionValue<'ctx>)> =
            self.func_cache.iter().map(|(k, v)| (*k, *v)).collect();
        by_findex.sort_by_key(|(k, _)| *k);
        for (findex, f) in by_findex {
            if f.count_basic_blocks() == 0 {
                continue;
            }
            let old = f.get_name().to_string_lossy().into_owned();
            if f.get_linkage() == Linkage::Internal {
                let gv = f.as_global_value();
                gv.set_name(&format!("ash_f{findex}.{old}"));
                gv.set_linkage(Linkage::External);
                gv.set_visibility(GlobalVisibility::Hidden);
            }
            let name = f.get_name().to_string_lossy().into_owned();
            if recorded.insert(name.clone()) {
                ordered.push((name, instruction_count(f)));
            }
        }
        let mut helper = 0usize;
        for f in self.module.get_functions() {
            if f.count_basic_blocks() == 0 {
                continue;
            }
            let old = f.get_name().to_string_lossy().into_owned();
            if recorded.contains(&old) {
                continue;
            }
            if f.get_linkage() == Linkage::Internal {
                let gv = f.as_global_value();
                gv.set_name(&format!("ash_h{helper}.{old}"));
                gv.set_linkage(Linkage::External);
                gv.set_visibility(GlobalVisibility::Hidden);
                helper += 1;
            }
            let name = f.get_name().to_string_lossy().into_owned();
            if recorded.insert(name.clone()) {
                ordered.push((name, instruction_count(f)));
            }
        }

        let mut data = 0usize;
        let mut global = self.module.get_first_global();
        while let Some(g) = global {
            let old = g.get_name().to_string_lossy().into_owned();
            if g.get_initializer().is_some()
                && g.get_linkage() == Linkage::Internal
                && !old.is_empty()
            {
                g.set_name(&format!("ash_d{data}.{old}"));
                g.set_linkage(Linkage::External);
                g.set_visibility(GlobalVisibility::Hidden);
                data += 1;
            }
            global = g.get_next_global();
        }
        ordered
    }

    /// Optimize and emit the module as `shards` objects on as many threads,
    /// joined into `out`. Replaces `optimize_module` + `emit_object` for the
    /// AOT path when more than one shard is asked for.
    pub fn emit_object_sharded(
        &self,
        triple: &str,
        out: &Path,
        shards: usize,
        quiet: bool,
    ) -> Result<u64> {
        let shards = shards.max(1);
        let no_opt = std::env::var_os("ASH_AOT_NO_OPT").is_some();
        let dump_dir: Option<PathBuf> = std::env::var_os("ASH_AOT_DUMP_IR").map(|d| {
            let p = PathBuf::from(d);
            if p.is_dir() {
                p
            } else {
                p.parent()
                    .map(Path::to_path_buf)
                    .unwrap_or_else(|| PathBuf::from("."))
            }
        });

        let began = std::time::Instant::now();
        // The same shield `optimize_module` raises: a trap is `setjmp`, and a
        // local promoted out of memory has an indeterminate value after the
        // jump. Attributes survive the bitcode round-trip.
        let shielded = self.shield_trap_functions_from_optimization();
        let weights = self.promote_for_sharding();
        let owned = partition(&weights, shards);
        if let Some(dir) = &dump_dir {
            let dest = dir.join("module.ll");
            self.module
                .print_to_file(&dest)
                .map_err(|e| anyhow!("write {}: {}", dest.display(), e.to_string()))?;
            if !quiet {
                eprintln!("[ash] wrote IR to {}", dest.display());
            }
        }
        let bitcode: Vec<u8> = self.module.write_bitcode_to_memory().as_slice().to_vec();
        // Registering targets is not something two threads should race on;
        // once here, every worker's own call finds them registered.
        Target::initialize_all(&InitializationConfig::default());
        if !quiet {
            eprintln!(
                "[aot] sharding: {} bodies, {} shards, shielded {shielded}, bitcode {} MB, prepared in {}ms",
                weights.len(),
                shards,
                bitcode.len() >> 20,
                began.elapsed().as_millis()
            );
        }

        let stem = out
            .file_name()
            .map(|s| s.to_string_lossy().into_owned())
            .unwrap_or_else(|| "aot.o".to_string());
        let dir = out
            .parent()
            .map(Path::to_path_buf)
            .unwrap_or_else(|| PathBuf::from("."));
        // Shard `shards` is the data shard: no bodies, no middle end.
        let empty: HashSet<String> = HashSet::new();
        let parts: Vec<PathBuf> = (0..=shards)
            .map(|k| dir.join(format!("{stem}.shard{k}.o")))
            .collect();

        let results: Vec<Result<()>> = std::thread::scope(|scope| {
            let handles: Vec<_> = (0..=shards)
                .map(|k| {
                    let bitcode = &bitcode;
                    let data_shard = k == shards;
                    let owned = if data_shard { &empty } else { &owned[k] };
                    let part = parts[k].clone();
                    let dump_dir = dump_dir.clone();
                    std::thread::Builder::new()
                        .name(format!("ash-aot-shard-{k}"))
                        .stack_size(SHARD_STACK_BYTES)
                        .spawn_scoped(scope, move || -> Result<()> {
                            emit_shard(
                                k,
                                data_shard,
                                triple,
                                bitcode,
                                owned,
                                &part,
                                no_opt,
                                dump_dir.as_deref(),
                                quiet,
                            )
                        })
                })
                .collect();
            handles
                .into_iter()
                .map(|h| match h {
                    Ok(h) => h
                        .join()
                        .unwrap_or_else(|_| Err(anyhow!("shard thread panicked"))),
                    Err(e) => Err(anyhow!("spawn shard thread: {e}")),
                })
                .collect()
        });
        for (k, r) in results.into_iter().enumerate() {
            r.map_err(|e| anyhow!("shard {k}: {e}"))?;
        }

        let began = std::time::Instant::now();
        let status = std::process::Command::new("ld")
            .arg("-r")
            .args(&parts)
            .arg("-o")
            .arg(out)
            .output()
            .map_err(|e| anyhow!("ld -r: {e} (parts left in {})", dir.display()))?;
        if !status.status.success() {
            anyhow::bail!(
                "ld -r failed ({}): {}\nparts left in {}",
                status.status,
                String::from_utf8_lossy(&status.stderr),
                dir.display()
            );
        }
        for p in &parts {
            let _ = std::fs::remove_file(p);
        }
        if !quiet {
            eprintln!(
                "[aot] joined {} body shards + data in {}ms",
                shards,
                began.elapsed().as_millis()
            );
        }
        Ok(std::fs::metadata(out)?.len())
    }
}

#[allow(clippy::too_many_arguments)]
fn emit_shard(
    k: usize,
    data_shard: bool,
    triple: &str,
    bitcode: &[u8],
    owned: &HashSet<String>,
    part: &Path,
    no_opt: bool,
    dump_dir: Option<&Path>,
    quiet: bool,
) -> Result<()> {
    let began = std::time::Instant::now();
    let context = Context::create();
    let buffer = MemoryBuffer::create_from_memory_range_copy(bitcode, "ash-aot");
    let module: Module<'_> = Module::parse_bitcode_from_buffer(&buffer, &context)
        .map_err(|e| anyhow!("parse bitcode: {}", e.to_string()))?;

    let mut kept = 0usize;
    let mut carried = 0usize;
    let mut stripped = 0usize;
    for f in module.get_functions() {
        if f.count_basic_blocks() == 0 {
            continue;
        }
        let name = f.get_name().to_string_lossy();
        if owned.contains(name.as_ref()) {
            kept += 1;
            continue;
        }
        // A shielded body is `optnone`: it will never be inlined, so carrying
        // it buys nothing. Everything else small enough rides along for the
        // inliner and is discarded by codegen. The data shard runs no
        // middle end, so it carries nothing.
        if !no_opt
            && !data_shard
            && instruction_count(f) <= INLINE_CANDIDATE_INSTRUCTIONS
            && !has_attribute(f, "optnone")
        {
            f.set_linkage(Linkage::AvailableExternally);
            carried += 1;
        } else {
            strip_body(f);
            stripped += 1;
        }
    }
    let mut folded = 0usize;
    if !data_shard {
        // Data is defined once, in the data shard. Private globals have no
        // symbol to declare and stay; a named constant stays readable for
        // the optimizer without being emitted; everything else named
        // becomes a declaration.
        let mut global = module.get_first_global();
        while let Some(g) = global {
            let named = !g.get_name().to_bytes().is_empty();
            if named && g.get_initializer().is_some() && g.get_linkage() != Linkage::Private {
                if g.is_constant() && !no_opt {
                    g.set_linkage(Linkage::AvailableExternally);
                    folded += 1;
                } else {
                    unsafe {
                        llvm_sys::core::LLVMSetInitializer(g.as_value_ref(), std::ptr::null_mut());
                    }
                }
            }
            global = g.get_next_global();
        }
    }
    let prepared_ms = began.elapsed().as_millis();

    let began = std::time::Instant::now();
    if !no_opt && !data_shard {
        super::module::run_middle_end_at(&module, "default<O3>")?;
    }
    let opt_ms = began.elapsed().as_millis();
    if let Some(dir) = dump_dir {
        let dest = dir.join(format!("module.shard{k}.ll"));
        module
            .print_to_file(&dest)
            .map_err(|e| anyhow!("write {}: {}", dest.display(), e.to_string()))?;
    }

    let began = std::time::Instant::now();
    let (tt, machine) = object_target_machine(triple)?;
    module.set_triple(&tt);
    machine
        .write_to_file(&module, FileType::Object, part)
        .map_err(|e| anyhow!("emit {}: {e}", part.display()))?;
    let codegen_ms = began.elapsed().as_millis();
    if !quiet {
        let role = if data_shard { "data" } else { "body" };
        eprintln!(
            "[aot] shard {k} ({role}): kept {kept}, carried {carried}, stripped {stripped}, constants {folded}; prepare {prepared_ms}ms, middle end {opt_ms}ms, codegen {codegen_ms}ms"
        );
    }
    Ok(())
}
