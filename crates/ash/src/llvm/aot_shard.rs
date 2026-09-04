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
//! Data is emitted once, by the emitter itself, on the thread that lowered
//! it. Its module already holds every initializer, so a shard that parsed a
//! copy of the program to reach them would be a second whole module in
//! memory and a second full parse in time -- measured at about 1.5 GB and
//! 11 s on a game, for globals that were already in hand. Instead, once the
//! stream is bytes, the emitter strips its own bodies and codegens what is
//! left: the data object. It then empties the module, while the shards are
//! still working, so the memory goes back before they need it.
//!
//! A body shard therefore sees the data only as something to reference. Its
//! mutable globals become declarations, its unreferenced ones are deleted,
//! and a named `constant` its own code still reads rides along as
//! `available_externally`, which the optimizer may fold a load from (an
//! `Int_7` is an immediate, not a load, exactly as in the single-module
//! build) and codegen never emits. `private` globals, which have no symbol
//! to declare, stay wherever they are referenced -- they are literal
//! constants, and a duplicate costs bytes, not correctness.
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

/// How many shards to emit. `ASH_AOT_SHARDS` wins.
///
/// Half the cores, never fewer than two or more than six, because the curve
/// is not the one you would guess. Measured on a game of 8,577 functions on
/// ten cores: one shard 350 s, two 57 s, four 42 s, eight 45 s. The collapse
/// from one to two is not parallelism -- it is that the optimizer's cost
/// grows faster than the module does, so half a module is far less than half
/// the work. Past four, each extra shard adds a whole copy of the program in
/// memory (2.9 GB at two shards, 4.9 GB at eight) and buys nothing, because
/// every shard first reads the program in full whatever it keeps.
pub fn shard_count() -> usize {
    if let Some(v) = std::env::var_os("ASH_AOT_SHARDS") {
        if let Ok(n) = v.to_string_lossy().trim().parse::<usize>() {
            return n.max(1);
        }
    }
    let cores = std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(1);
    (cores / 2).clamp(2, 6)
}

/// How many shards to emit for `triple`.
///
/// One when cross-compiling: the shards are joined by the HOST linker, and
/// `ld -r` reads one object format only -- Apple's reads Mach-O, GNU's reads
/// ELF. Every other step here honours the requested triple, so a cross build
/// stays on the single-module path, which needs no linker at all.
///
/// wasm is the exception, because ash links wasm itself: `ash_wasm_link`
/// takes any number of objects, so the shards go to it as they would to a
/// native link. Without this a wasm build of the Haxe unit suite ran the
/// optimizer over 1.7MB of bytecode as one module on one core, forty
/// minutes against the sharded native build's four.
pub fn shard_count_for(triple: &str) -> usize {
    if TargetTriple::create(triple) != TargetMachine::get_default_triple()
        && !super::aot_link::is_wasm_triple(triple)
    {
        return 1;
    }
    shard_count()
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
    crate::target_abi::target_machine(triple, inkwell::OptimizationLevel::Aggressive)
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

/// A global whose initializer is a plain scalar -- an `Int_7`, a `Float_`.
/// These are what constant folding turns into immediates, they are a
/// rounding error in memory, and they are the only data a body shard has any
/// reason to carry.
fn has_scalar_initializer(g: inkwell::values::GlobalValue<'_>) -> bool {
    match g.get_initializer() {
        Some(v) => v.is_int_value() || v.is_float_value(),
        None => false,
    }
}

fn has_uses(v: llvm_sys::prelude::LLVMValueRef) -> bool {
    unsafe { !llvm_sys::core::LLVMGetFirstUse(v).is_null() }
}

/// Every global value an initializer or attribute names, so that deleting
/// one thing tells us what to look at next.
fn referenced_globals(v: llvm_sys::prelude::LLVMValueRef) -> Vec<llvm_sys::prelude::LLVMValueRef> {
    use llvm_sys::core::*;
    let mut found = Vec::new();
    let mut stack = vec![v];
    let mut depth = 0usize;
    unsafe {
        while let Some(cur) = stack.pop() {
            depth += 1;
            if depth > 10_000 {
                break;
            }
            let operands = LLVMGetNumOperands(cur);
            for i in 0..operands {
                let op = LLVMGetOperand(cur, i as u32);
                if op.is_null() {
                    continue;
                }
                if !LLVMIsAGlobalVariable(op).is_null() || !LLVMIsAFunction(op).is_null() {
                    found.push(op);
                } else if !LLVMIsAConstant(op).is_null() && LLVMGetNumOperands(op) > 0 {
                    // A constant expression: the global is inside it.
                    stack.push(op);
                }
            }
        }
    }
    found
}

/// Delete everything this shard will neither emit nor read: globals nothing
/// references, declarations nothing calls, and carried bodies nothing calls.
///
/// Deleting one thing orphans the things it named -- a table's initializer
/// names a hundred strings, a carried body calls other carried bodies -- so
/// this is a worklist, seeded with everything and refilled from whatever a
/// deletion just released. It was a fixpoint over the whole module once, and
/// on a game that cost more time than the memory was worth: 97,731 globals
/// re-examined per round, for as many rounds as the longest chain.
///
/// Carried bodies are the bulk of what goes: a shard admits every body under
/// 80 instructions in case the inliner wants it, which on a game is some
/// 3,700 functions, and the ones its own code never calls are pure weight.
/// LLVM's own GlobalDCE would find them, but only from inside the O3
/// pipeline -- far too late, since the point is to be holding less before O3
/// starts.
fn sweep_unreachable(module: &Module<'_>) -> usize {
    use std::collections::HashSet;
    let mut work: Vec<llvm_sys::prelude::LLVMValueRef> = Vec::new();
    let mut global = module.get_first_global();
    while let Some(g) = global {
        work.push(g.as_value_ref());
        global = g.get_next_global();
    }
    for f in module.get_functions() {
        if f.count_basic_blocks() == 0 || f.get_linkage() == Linkage::AvailableExternally {
            work.push(f.as_value_ref());
        }
    }

    // A value can be reached twice -- with uses the first time, without them
    // the second -- so "seen" cannot gate the visit. What must not happen is
    // deleting the same pointer twice; nothing new is created here, so no
    // address is ever reused.
    let mut deleted: HashSet<usize> = HashSet::new();
    let mut removed = 0usize;
    while let Some(v) = work.pop() {
        if deleted.contains(&(v as usize)) || has_uses(v) {
            continue;
        }
        unsafe {
            let name = std::ffi::CStr::from_ptr(llvm_sys::core::LLVMGetValueName2(
                v,
                &mut 0usize as *mut usize,
            ))
            .to_string_lossy()
            .into_owned();
            if name.starts_with("llvm.") {
                continue;
            }
            let orphans = referenced_globals(v);
            if !llvm_sys::core::LLVMIsAFunction(v).is_null() {
                llvm_sys::core::LLVMDeleteFunction(v);
            } else if !llvm_sys::core::LLVMIsAGlobalVariable(v).is_null() {
                llvm_sys::core::LLVMDeleteGlobal(v);
            } else {
                continue;
            }
            deleted.insert(v as usize);
            removed += 1;
            work.extend(orphans);
        }
    }
    removed
}

/// Empty the module: every body stripped, every initializer dropped, every
/// global and declaration nothing references deleted.
///
/// Called once the data object is written. Holding a program's worth of IR
/// that nothing will read again is a gigabyte the shards could be using, and
/// each of them works from its own copy of the stream.
fn release_module(module: &Module<'_>) {
    for f in module.get_functions() {
        if f.count_basic_blocks() > 0 {
            strip_body(f);
        }
    }
    let mut global = module.get_first_global();
    while let Some(g) = global {
        let next = g.get_next_global();
        if g.get_initializer().is_some() {
            unsafe { llvm_sys::core::LLVMSetInitializer(g.as_value_ref(), std::ptr::null_mut()) };
        }
        global = next;
    }
    sweep_unreachable(module);
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

    /// Write the object that defines the program's data, from the emitter's
    /// own module.
    ///
    /// Every body becomes a declaration first, so this object defines data
    /// and nothing else; the shards define the bodies. No middle end runs:
    /// the optimizer's only business with 97,731 initializers is to walk
    /// them, which cost 90 s the one time it was tried.
    fn emit_data_object(&self, triple: &str, part: &Path, quiet: bool) -> Result<()> {
        let began = std::time::Instant::now();
        let mut stripped = 0usize;
        for f in self.module.get_functions() {
            if f.count_basic_blocks() > 0 {
                strip_body(f);
                stripped += 1;
            }
        }
        let prepared_ms = began.elapsed().as_millis();
        let began = std::time::Instant::now();
        let (tt, machine) = object_target_machine(triple)?;
        self.module.set_triple(&tt);
        self.module
            .set_data_layout(&machine.get_target_data().get_data_layout());
        // The data object holds every constant and the function table, and it
        // is the one artifact `ASH_AOT_DUMP_IR` did not cover: the shards carry
        // bodies, this carries the data those bodies read. Comparing a program
        // across two targets is comparing this.
        if let Some(spec) = std::env::var_os("ASH_AOT_DUMP_IR") {
            let spec = spec.to_string_lossy().into_owned();
            if !spec.is_empty() && spec != "0" {
                let base = std::path::PathBuf::from(if spec == "1" {
                    "/tmp/ash_aot.ll".to_string()
                } else {
                    spec
                });
                let stem = base
                    .file_stem()
                    .map(|s| s.to_string_lossy().into_owned())
                    .unwrap_or_else(|| "module".to_string());
                let dest = base.with_file_name(format!("{stem}.data.ll"));
                match self.module.print_to_file(&dest) {
                    Ok(()) => eprintln!("[ash] LLVM IR written to {}", dest.display()),
                    Err(e) => eprintln!("[ash] could not write {}: {e}", dest.display()),
                }
            }
        }
        machine
            .write_to_file(&self.module, FileType::Object, part)
            .map_err(|e| anyhow!("emit {}: {e}", part.display()))?;
        let codegen_ms = began.elapsed().as_millis();
        // The shards are still running, and this module is now dead weight.
        release_module(&self.module);
        crate::progress::advance(1);
        if !quiet {
            crate::progress::detail(&format!(
                "[aot] data object: {stripped} bodies stripped; prepare {prepared_ms}ms, codegen {codegen_ms}ms"
            ));
        }
        Ok(())
    }

    /// Optimize and emit the module as `shards` objects on as many threads,
    /// joined into `out`. Replaces `optimize_module` + `emit_object` for the
    /// AOT path when more than one shard is asked for.
    /// One object, joined from the parts. A caller heading straight for a
    /// binary wants [`Self::emit_object_parts`] instead: a linker takes any
    /// number of objects, so joining them first rewrites every byte for
    /// nothing.
    pub fn emit_object_sharded(
        &self,
        triple: &str,
        out: &Path,
        shards: usize,
        quiet: bool,
    ) -> Result<u64> {
        let parts = self.emit_object_parts(triple, out, shards, quiet)?;
        let began = std::time::Instant::now();
        let status = std::process::Command::new("ld")
            .arg("-r")
            .args(&parts)
            .arg("-o")
            .arg(out)
            .output()
            .map_err(|e| anyhow!("ld -r: {e} (parts left beside {})", out.display()))?;
        if !status.status.success() {
            anyhow::bail!(
                "ld -r failed ({}): {}\nparts left beside {}",
                status.status,
                String::from_utf8_lossy(&status.stderr),
                out.display()
            );
        }
        for p in &parts {
            let _ = std::fs::remove_file(p);
        }
        if !quiet {
            crate::progress::detail(&format!(
                "[aot] joined {} shards + data in {}ms",
                shards,
                began.elapsed().as_millis()
            ));
        }
        Ok(std::fs::metadata(out)?.len())
    }

    /// Optimize and emit the module as one object per shard plus the data
    /// object, named after `out`, and return their paths.
    pub fn emit_object_parts(
        &self,
        triple: &str,
        out: &Path,
        shards: usize,
        quiet: bool,
    ) -> Result<Vec<PathBuf>> {
        let shards = shards.max(1);
        let no_opt = std::env::var_os("ASH_AOT_NO_OPT").is_some();
        // Same meaning as on the single-module path: a directory takes
        // `module.ll`, anything else IS the file the caller named. Each
        // shard's own IR lands beside it under the same stem.
        let dump_ir: Option<PathBuf> = std::env::var_os("ASH_AOT_DUMP_IR").map(|d| {
            let p = PathBuf::from(d);
            if p.is_dir() {
                p.join("module.ll")
            } else {
                p
            }
        });

        let began = std::time::Instant::now();
        // The same shield `optimize_module` raises: a trap is `setjmp`, and a
        // local promoted out of memory has an indeterminate value after the
        // jump. Attributes survive the bitcode round-trip.
        let shielded = self.shield_trap_functions_from_optimization();
        let weights = self.promote_for_sharding();
        let owned = partition(&weights, shards);
        if let Some(dest) = &dump_ir {
            self.module
                .print_to_file(dest)
                .map_err(|e| anyhow!("write {}: {}", dest.display(), e.to_string()))?;
            if !quiet {
                eprintln!("[ash] wrote IR to {}", dest.display());
            }
        }
        // One stream. Slimming it first -- dropping the data a body shard
        // will not read -- was measured and kept only 4 MB of 61, because
        // the module is bodies, not data; what it cost was the emitter's own
        // copy of the initializers, which is exactly what emits the data
        // object below.
        let bitcode: Vec<u8> = self.module.write_bitcode_to_memory().as_slice().to_vec();
        // Registering targets is not something two threads should race on;
        // once here, every worker's own call finds them registered.
        Target::initialize_all(&InitializationConfig::default());
        if !quiet {
            crate::progress::detail(&format!(
                "[aot] sharding: {} bodies, {} shards, shielded {shielded}, stream {} MB, prepared in {}ms",
                weights.len(),
                shards,
                bitcode.len() >> 20,
                began.elapsed().as_millis()
            ));
        }
        // One unit per shard, plus the data object this thread writes.
        crate::progress::begin("compiling", shards as u64 + 1);

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
            let handles: Vec<_> = (0..shards)
                .map(|k| {
                    let bitcode = &bitcode;
                    let owned = &owned[k];
                    let part = parts[k].clone();
                    let dump_ir = dump_ir.clone();
                    std::thread::Builder::new()
                        .name(format!("ash-aot-shard-{k}"))
                        .stack_size(SHARD_STACK_BYTES)
                        .spawn_scoped(scope, move || -> Result<()> {
                            emit_shard(
                                k,
                                triple,
                                bitcode,
                                owned,
                                &part,
                                no_opt,
                                dump_ir.as_deref(),
                                quiet,
                            )
                        })
                })
                .collect();
            // This thread takes the data object while those run. Nothing is
            // parsed for it: the module in hand is the one that was lowered.
            let data = self.emit_data_object(triple, &parts[shards], quiet);
            let mut results: Vec<Result<()>> = handles
                .into_iter()
                .map(|h| match h {
                    Ok(h) => h
                        .join()
                        .unwrap_or_else(|_| Err(anyhow!("shard thread panicked"))),
                    Err(e) => Err(anyhow!("spawn shard thread: {e}")),
                })
                .collect();
            results.push(data);
            results
        });
        for (k, r) in results.into_iter().enumerate() {
            r.map_err(|e| {
                if k == shards {
                    anyhow!("data object: {e}")
                } else {
                    anyhow!("shard {k}: {e}")
                }
            })?;
        }

        Ok(parts)
    }
}

#[allow(clippy::too_many_arguments)]
fn emit_shard(
    k: usize,
    triple: &str,
    bitcode: &[u8],
    owned: &HashSet<String>,
    part: &Path,
    no_opt: bool,
    dump_ir: Option<&Path>,
    quiet: bool,
) -> Result<()> {
    let began = std::time::Instant::now();
    let context = Context::create();
    // Borrowed, not copied: nine shards copying a 61 MB stream is half a
    // gigabyte of nothing. The parse reads the buffer to completion and the
    // module keeps no reference to it, and `thread::scope` already guarantees
    // the bytes outlive every shard.
    let buffer = MemoryBuffer::create_from_memory_range(bitcode, "ash-aot");
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
        // inliner, and the sweep below drops the ones this shard never calls.
        if !no_opt
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
    let mut declared = 0usize;
    let swept = {
        // The stream arrived carrying every body; this shard emits a
        // fraction of them, and the bodies it does not emit are now empty.
        // Whatever only they referenced is unreachable, so it goes -- a
        // declaration nothing names is still a symbol, a type and a use
        // list to carry through O3.
        let swept = sweep_unreachable(&module);
        // What survives is reachable from code this shard emits, and the
        // data object defines all of it -- so no shard may define any of it
        // too, or the link fails on duplicate symbols. A constant keeps its
        // initializer under `available_externally`, which means exactly
        // "read this, do not emit it", so a load of it can still fold to an
        // immediate. Everything else becomes a declaration.
        let mut global = module.get_first_global();
        while let Some(g) = global {
            let next = g.get_next_global();
            if !g.get_name().to_bytes().is_empty()
                && g.get_initializer().is_some()
                && g.get_linkage() != Linkage::Private
            {
                if g.is_constant() && !no_opt {
                    g.set_linkage(Linkage::AvailableExternally);
                    folded += 1;
                } else {
                    unsafe {
                        llvm_sys::core::LLVMSetInitializer(g.as_value_ref(), std::ptr::null_mut())
                    };
                    declared += 1;
                }
            }
            global = next;
        }
        swept
    };
    let prepared_ms = began.elapsed().as_millis();

    let began = std::time::Instant::now();
    if !no_opt {
        super::module::run_middle_end_at(&module, "default<O3>")?;
    }
    let opt_ms = began.elapsed().as_millis();
    if let Some(base) = dump_ir {
        let stem = base
            .file_stem()
            .map(|s| s.to_string_lossy().into_owned())
            .unwrap_or_else(|| "module".to_string());
        let dest = base.with_file_name(format!("{stem}.shard{k}.ll"));
        module
            .print_to_file(&dest)
            .map_err(|e| anyhow!("write {}: {}", dest.display(), e.to_string()))?;
    }

    let began = std::time::Instant::now();
    let (tt, machine) = object_target_machine(triple)?;
    module.set_triple(&tt);
    module.set_data_layout(&machine.get_target_data().get_data_layout());
    machine
        .write_to_file(&module, FileType::Object, part)
        .map_err(|e| anyhow!("emit {}: {e}", part.display()))?;
    let codegen_ms = began.elapsed().as_millis();
    crate::progress::advance(1);
    if !quiet {
        crate::progress::detail(&format!(
            "[aot] shard {k}: kept {kept}, carried {carried}, stripped {stripped}, swept {swept}, folded {folded}, declared {declared}; prepare {prepared_ms}ms, middle end {opt_ms}ms, codegen {codegen_ms}ms"
        ));
    }
    Ok(())
}
