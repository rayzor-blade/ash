use std::ffi::c_void;

use air::v2::ir::{
    BinOp as AirBinOp, BlockId as AirBlockId, CastKind as AirCastKind, CondKind as AirCondKind,
    Function as AirFunction, Instr as AirInstr, MemAccess as AirMemAccess,
    Terminator as AirTerminator, UnOp as AirUnOp, ValueId,
};
use ash_macro::to_llvm;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::types::{
    AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType,
};
use inkwell::values::{
    AnyValue, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, IntValue,
    PointerValue,
};
use inkwell::{
    basic_block::BasicBlock, builder::Builder, AddressSpace, AtomicOrdering, FloatPredicate,
    IntPredicate,
};

use super::module::{CompiledFunctionMeta, JITModule};
use crate::hl::{
    hl_obj_field, hl_runtime_obj, hl_type, hl_type_kind_HABSTRACT, hl_type_kind_HBOOL,
    hl_type_kind_HBYTES, hl_type_kind_HDYN, hl_type_kind_HDYNOBJ, hl_type_kind_HF32,
    hl_type_kind_HF64, hl_type_kind_HI32, hl_type_kind_HI64, hl_type_kind_HNULL, hl_type_kind_HOBJ,
    hl_type_kind_HSTRUCT, hl_type_kind_HTYPE, hl_type_kind_HUI16, hl_type_kind_HUI8,
    hl_type_kind_HVIRTUAL, hl_type_kind_HVOID, vdynamic, vdynobj, vvirtual,
};
use crate::opcodes::{
    Opcode, RefBytes, RefEnumConstruct, RefField, RefFloat, RefFun, RefGlobal, RefInt, RefString,
    RefType, Reg,
};
use crate::types::{HLNative, HLTypeFun, Str, TypeRef};
use crate::{
    hl::{hl_type_kind_HFUN, hl_type_kind_HMETHOD},
    types::HLFunction,
};
use anyhow::{anyhow, Result};

mod air_emit;
mod calls;
mod casts;
mod enums;
mod memory;
mod natives;
mod objects;

/// Compile unresolved natives to call-time trap stubs instead of failing the
/// whole function compile — matching HashLink's disabled_primitive semantics
/// (errors when called, not when compiled) and the interpreter's lazy
/// resolution. Unlocks tier promotion of functions that merely reference
/// unimplemented natives. Default ON since JIT indirect-call sites guard
/// against interpreter stub sentinels (see build_stub_guarded_indirect_call);
/// opt out with ASH_JIT_NATIVE_TRAPS=0 to restore compile-time failure.
pub(super) fn native_traps_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| {
        !matches!(
            std::env::var("ASH_JIT_NATIVE_TRAPS").as_deref(),
            Ok("0") | Ok("false")
        )
    })
}

/// Compute HashLink field hash at compile time (same algorithm as hlp_hash_gen).
///
/// Lives in [`crate::layout`] now that the Cranelift tier bakes the same
/// number into its own code: two copies of a hash both tiers embed is the
/// shape of bug that only surfaces when they disagree about one field.
pub(super) fn hl_hash_utf8(s: &str) -> i32 {
    crate::layout::field_name_hash(s)
}

#[to_llvm]
unsafe extern "C" {
    fn hlp_get_dynset(d: *mut vdynamic, hfield: i32) -> *mut c_void;
    fn hlp_get_dynget(t: *mut hl_type) -> *mut c_void;
    fn hlp_get_obj_rt(ot: *mut hl_type) -> *mut hl_runtime_obj;
    fn hlp_obj_field_fetch(t: *mut hl_type, fid: i32) -> *mut hl_obj_field;
    fn hlp_alloc_dynobj() -> *mut vdynobj;
    fn hlp_alloc_virtual(t: *mut hl_type) -> *mut vvirtual;
}

/// Reference to a function or a native object
#[derive(Debug, Clone)]
pub enum FuncPtr {
    Fun(HLFunction),
    Native(HLNative),
}

/// The word a pinned register's address is stored to, so that the address
/// escapes and the middle end leaves the register in memory. Nothing reads
/// it; see [`FunctionCompiler::pin_register`].
const GC_REGISTER_PIN: &str = "ash_gc_register_pin";

/// Whether compiled loops carry a fiber/GC safepoint poll at their headers.
/// `ASH_FIBER_POLLS=0` turns them off; see the call site for why that is a
/// measurement switch and not a tuning knob.
pub(super) fn fiber_polls_enabled() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| !matches!(std::env::var("ASH_FIBER_POLLS").as_deref(), Ok("0")))
}

/// Whether `New` allocates through the sized entry. `ASH_ALLOC_SIZED=0` sends
/// every allocation back through `hlp_alloc_obj`, which is how the two are
/// compared without a rebuild.
/// Memory effects for `hlp_fiber_poll`, as the LLVM `memory(...)` attribute's
/// encoded value. `None` declares nothing, which LLVM reads as
/// `memory(readwrite)` -- correct, because past a yield another fiber runs
/// Haxe code and writes the same heap.
///
/// `ASH_POLL_MEMORY=inaccessible` narrows it, dropping the barrier that stops
/// LICM hoisting loads out of a polling loop while keeping the call itself.
/// For measurement only; not sound to run with.
pub(super) fn poll_memory_effects() -> Option<u64> {
    static VALUE: std::sync::OnceLock<Option<u64>> = std::sync::OnceLock::new();
    *VALUE.get_or_init(|| match std::env::var("ASH_POLL_MEMORY").as_deref() {
        // `MemoryEffects` packs two bits of ModRef per location, indexed
        // ArgMem, InaccessibleMem, ErrnoMem, Other. ModRef is 3, so
        // inaccessible-only readwrite is 3 << 2.
        Ok("inaccessible") => Some(3 << 2),
        Ok("none") => Some(0),
        _ => None,
    })
}

pub(super) fn sized_alloc_enabled() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| !matches!(std::env::var("ASH_ALLOC_SIZED").as_deref(), Ok("0")))
}

impl<'ctx> JITModule<'ctx> {
    /// Tag an access as touching the object field at `field_index` of
    /// `type_index`. Keyed by byte offset — see [`super::tbaa`] for why that
    /// is the sound key under inheritance.
    fn tbaa_field(
        &self,
        inst: Option<inkwell::values::InstructionValue<'ctx>>,
        type_index: usize,
        field_index: usize,
    ) {
        if let Some(inst) = inst {
            if let Some(off) = crate::layout::field_offset_for(
                &self.types_,
                type_index,
                field_index,
                self.target_abi.pointer_bytes() as i32,
            ) {
                self.tbaa.tag(inst, self.tbaa.obj_field(self.context, off));
            }
        }
    }

    #[inline(always)]
    fn current_stack_addr() -> usize {
        // Portable stack probe: address of a local variable approximates current SP.
        let marker = 0u8;
        (&marker as *const u8) as usize
    }

    /// The loop safe point's epoch word.
    ///
    /// One word, reached two ways. A JIT asks the runtime for its address and
    /// bakes it in. An object file cannot: it names the runtime's
    /// `ash_fiber_poll_epoch` and lets the linker place it, which is the same
    /// word at a different time.
    fn fiber_poll_epoch_ptr(&self) -> Result<PointerValue<'ctx>> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        if self.aot {
            // Indirect wherever the target cannot resolve a reference to an
            // undefined data symbol. Two targets cannot, for the same reason
            // and with different words for it: a Mach-O arm64 dylib addresses
            // data through the GOT, so a direct `ARM64_RELOC_PAGE21` fails the
            // link with "does not have address"; wasm has no relocation that
            // reaches an undefined data symbol at all, and `--allow-undefined`
            // covers functions only. Fibers themselves are not in question --
            // on wasm they are driven by the host, and this word is what the
            // host's scheduler ticks.
            if self.aot_shared_runtime || !self.target_abi.direct_data_relocations {
                // Naming the runtime's `ash_fiber_poll_epoch` is right for a
                // static link and impossible for a dynamic one on Mach-O
                // arm64: the object addresses it directly (ARM64_RELOC_PAGE21)
                // and a dylib's data needs a GOT access, so the link fails
                // with "does not have address". It is the ONLY data symbol
                // this object imports -- the other 74 are functions, which
                // cross the boundary fine -- so route it through the getter
                // the runtime already exports and the JIT already calls.
                // ash_module_init fills this once; the poll pays one extra
                // load, and only in a build that loads HDLLs.
                let slot = match self.module.get_global("ash_fiber_poll_epoch_ptr") {
                    Some(existing) => existing,
                    None => {
                        let g = self
                            .module
                            .add_global(ptr_type, None, "ash_fiber_poll_epoch_ptr");
                        g.set_initializer(&ptr_type.const_null());
                        g.set_linkage(inkwell::module::Linkage::Internal);
                        g
                    }
                };
                return Ok(self
                    .builder
                    .build_load(ptr_type, slot.as_pointer_value(), "poll_epoch")?
                    .into_pointer_value());
            }
            let global = self.aot_runtime_global("ash_fiber_poll_epoch", self.context.i64_type());
            return Ok(global.as_pointer_value());
        }
        Ok(self
            .context
            .i64_type()
            .const_int(self.fiber_poll_epoch_address()? as u64, false)
            .const_to_pointer(ptr_type))
    }

    fn fiber_poll_epoch_address(&self) -> Result<usize> {
        let getter = self
            .native_function_resolver
            .resolve_function("std", "hlp_fiber_poll_epoch_address")?;
        let getter: unsafe extern "C" fn() -> *const u64 = unsafe { std::mem::transmute(getter) };
        let address = unsafe { getter() } as usize;
        if address == 0 {
            return Err(anyhow!("hlp_fiber_poll_epoch_address returned null"));
        }
        Ok(address)
    }

    /// Declare an external native function and create a caller wrapper.
    /// Embeds the native function's address directly as inttoptr constant
    /// to avoid MCJIT symbol resolution issues with add_global_mapping.
    fn declare_native(
        &self,
        name: &str,
        param_types: &[BasicMetadataTypeEnum<'ctx>],
        ret_type: Option<BasicTypeEnum<'ctx>>,
    ) -> FunctionValue<'ctx> {
        let caller_name = format!("__native_{}_caller", name);
        if let Some(f) = self.module.get_function(&caller_name) {
            // A cached stub for a helper that never resolved still poisons the
            // function being compiled, so the record has to happen on the hit
            // as well as on the miss.
            if self.poisoned_natives.borrow().contains(name) {
                self.natives_missing_in_compile
                    .borrow_mut()
                    .push(name.to_string());
            }
            return f;
        }

        let fn_type = match ret_type {
            Some(BasicTypeEnum::IntType(t)) => t.fn_type(param_types, false),
            Some(BasicTypeEnum::FloatType(t)) => t.fn_type(param_types, false),
            Some(BasicTypeEnum::PointerType(t)) => t.fn_type(param_types, false),
            Some(BasicTypeEnum::StructType(t)) => t.fn_type(param_types, false),
            Some(BasicTypeEnum::ArrayType(t)) => t.fn_type(param_types, false),
            Some(BasicTypeEnum::VectorType(t)) => t.fn_type(param_types, false),
            Some(BasicTypeEnum::ScalableVectorType(t)) => t.fn_type(param_types, false),
            None => self.context.void_type().fn_type(param_types, false),
        };

        // AOT: reference the native by NAME and let the linker resolve it
        // against libash_std.a. An address resolved in this process is
        // meaningless in the one that runs the object, and there is no dlopen
        // in a wasm sandbox at all. `External` here says only "not defined in
        // this module" -- it is orthogonal to static vs dynamic linking, and
        // the AOT link is static.
        if self.aot {
            return self.aot_runtime_fn(name, fn_type);
        }

        // A helper the loaded runtime does not export -- a staged `libhl` older
        // than the `ash` beside it is how that happens. Record it and return a
        // stub, so this stays infallible for its callers;
        // `promote_function_strict` refuses any function that reached one and
        // it keeps running on the tier below.
        let func_addr = match self.resolve_runtime_helper(name) {
            Ok(addr) => addr as usize,
            Err(err) => {
                self.poisoned_natives.borrow_mut().insert(name.to_string());
                self.natives_missing_in_compile
                    .borrow_mut()
                    .push(name.to_string());
                eprintln!(
                    "[ash] runtime helper {name} unresolved ({err}); refusing to compile callers"
                );
                return self.trapping_stub(&caller_name, fn_type);
            }
        };

        self.generate_native_caller_with_addr(&caller_name, fn_type, func_addr)
            .unwrap_or_else(|e| panic!("Failed to generate caller for {}: {}", name, e))
    }

    /// Resolve one of ash's own runtime helpers.
    ///
    /// `ASH_TEST_UNRESOLVED_NATIVE=<name>[,<name>...]` reports the listed
    /// helpers as missing however the runtime actually answers, which is the
    /// only way to reach the refusal path without staging a runtime older than
    /// the compiler.
    fn resolve_runtime_helper(&self, name: &str) -> Result<*mut std::ffi::c_void> {
        static FORCED: std::sync::OnceLock<Vec<String>> = std::sync::OnceLock::new();
        let forced = FORCED.get_or_init(|| {
            std::env::var("ASH_TEST_UNRESOLVED_NATIVE")
                .map(|v| v.split(',').map(|n| n.trim().to_string()).collect())
                .unwrap_or_default()
        });
        if forced.iter().any(|n| n == name) {
            return Err(anyhow!("forced missing by ASH_TEST_UNRESOLVED_NATIVE"));
        }
        self.native_function_resolver.resolve_function("std", name)
    }

    /// Split control flow on `ptr` being null, for an opcode HL defines on
    /// null where the lowering would otherwise dereference.
    ///
    /// Returns `(null_block, load_block, cont_block)` with the conditional
    /// branch already emitted and the builder left on `null_block`. Each arm
    /// stores its own answer into the destination and branches to `cont`,
    /// which is why this hands back blocks rather than taking closures: the
    /// arms differ in what they store, and several need the register table
    /// the caller already holds borrowed.
    fn null_guard(
        &self,
        tag: &str,
        ptr: inkwell::values::PointerValue<'ctx>,
    ) -> Result<(
        inkwell::basic_block::BasicBlock<'ctx>,
        inkwell::basic_block::BasicBlock<'ctx>,
        inkwell::basic_block::BasicBlock<'ctx>,
    )> {
        let current_fn = self
            .builder
            .get_insert_block()
            .and_then(|b| b.get_parent())
            .ok_or_else(|| anyhow!("{tag}: builder is not inside a function"))?;
        let null_block = self
            .context
            .append_basic_block(current_fn, &format!("{tag}_null"));
        let load_block = self
            .context
            .append_basic_block(current_fn, &format!("{tag}_load"));
        let cont_block = self
            .context
            .append_basic_block(current_fn, &format!("{tag}_cont"));
        let is_null = self.builder.build_is_null(ptr, &format!("{tag}_is_null"))?;
        self.builder
            .build_conditional_branch(is_null, null_block, load_block)?;
        self.builder.position_at_end(null_block);
        Ok((null_block, load_block, cont_block))
    }

    /// A body-shaped placeholder for a helper that did not resolve.
    ///
    /// `llvm.trap` and not a call to `hlp_error`: building an error path needs
    /// `hlp_error` to resolve, which is no more certain than the helper that
    /// just did not. The promote path refuses any function reaching one, but
    /// the whole-module path has no such gate, so the body aborts on a defined
    /// signal rather than leaving bare `unreachable` for something to execute.
    fn trapping_stub(&self, name: &str, fn_type: FunctionType<'ctx>) -> FunctionValue<'ctx> {
        let saved = self.builder.get_insert_block();
        let f = self.module.add_function(name, fn_type, None);
        let entry = self.context.append_basic_block(f, "entry");
        self.builder.position_at_end(entry);
        if let Some(trap) = inkwell::intrinsics::Intrinsic::find("llvm.trap")
            .and_then(|t| t.get_declaration(&self.module, &[]))
        {
            let _ = self.builder.build_call(trap, &[], "trap");
        }
        let _ = self.builder.build_unreachable();
        if let Some(block) = saved {
            self.builder.position_at_end(block);
        }
        f
    }

    /// Get or declare an external native function, avoiding builder position clobber.
    /// The `_to_llvm` macro functions reposition the builder, so we save/restore it.
    fn get_or_declare_native(
        &self,
        name: &str,
        declare_fn: impl FnOnce(
            &'ctx inkwell::context::Context,
            &inkwell::module::Module<'ctx>,
            &inkwell::builder::Builder<'ctx>,
        ) -> Result<FunctionValue<'ctx>>,
    ) -> Result<FunctionValue<'ctx>> {
        if let Some(f) = self.module.get_function(name) {
            return Ok(f);
        }
        let saved_block = self.builder.get_insert_block();
        let func = declare_fn(self.context, &self.module, &self.builder)?;
        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }
        Ok(func)
    }

    fn create_function_placeholder(
        &self,
        name: &str,
        func_type: FunctionType<'ctx>,
    ) -> FunctionValue<'ctx> {
        self.add_body_function(name, func_type)
    }

    /// Create the LLVM function for one bytecode body.
    ///
    /// Linkage stays external here even under AOT, because this is also how a
    /// not-yet-lowered callee is declared, and an internal declaration is
    /// invalid IR. `finalize_aot_data` internalizes the ones that ended up
    /// with a body.
    fn add_body_function(&self, name: &str, func_type: FunctionType<'ctx>) -> FunctionValue<'ctx> {
        let f = self
            .module
            .add_function(name, func_type, Some(inkwell::module::Linkage::External));
        // Under AOT the object may be built for a machine that is not this
        // one, and a host-CPU stamp would make it crash there rather than run
        // slower. The target machine handed to `emit_object` decides instead.
        // Always: the frame-pointer attribute applies to every body, and the
        // CPU stamp inside skips itself under AOT.
        self.stamp_host_cpu(f);
        f
    }

    /// Pin `f`'s codegen to the HOST CPU.
    ///
    /// MCJIT's engine compiles for a GENERIC target CPU — on x86-64 that is
    /// SSE2 with no FMA3 and no AVX2, which is why the NUC produced the
    /// unfused mandelbrot checksum and lost to an M1 on FP kernels it should
    /// win. aarch64 never felt it because the base ISA already has fmadd.
    /// Codegen honors per-FUNCTION `target-cpu`/`target-features` attributes
    /// regardless of the engine's machine, so every function gets stamped at
    /// creation — the one choke point both the whole-module and the tiered
    /// promote paths pass through.
    fn stamp_host_cpu(&self, f: FunctionValue<'ctx>) {
        // Every body keeps a frame pointer. LLVM's default for a raw module
        // is to omit it, and an emitted prologue then saves x29/x30 without
        // ever pointing x29 at the frame, which breaks the chain a stack walk
        // follows: an exception's stack ended at the first AOT body. Apple's
        // ABI expects it anyway; the JIT's own walker uses its code map but
        // crash reports read the chain too.
        let loc = inkwell::attributes::AttributeLoc::Function;
        f.add_attribute(
            loc,
            self.context.create_string_attribute("frame-pointer", "all"),
        );
        // An object file names its CPU once, in `emit_object`, from the
        // requested triple; stamping THIS host onto a function would bake
        // the build machine into a cross-compiled binary.
        if self.aot {
            return;
        }
        use std::sync::OnceLock;
        static HOST: OnceLock<(String, String)> = OnceLock::new();
        let (cpu, feats) = HOST.get_or_init(|| {
            (
                inkwell::targets::TargetMachine::get_host_cpu_name()
                    .to_string_lossy()
                    .into_owned(),
                inkwell::targets::TargetMachine::get_host_cpu_features()
                    .to_string_lossy()
                    .into_owned(),
            )
        });
        let loc = inkwell::attributes::AttributeLoc::Function;
        if !cpu.is_empty() {
            f.add_attribute(loc, self.context.create_string_attribute("target-cpu", cpu));
        }
        if !feats.is_empty() {
            f.add_attribute(
                loc,
                self.context
                    .create_string_attribute("target-features", feats),
            );
        }
    }

    pub(crate) fn get_or_create_function_value(
        &mut self,
        index: usize,
    ) -> Result<(FunctionValue<'ctx>, bool)> {
        if let Some(f_v) = self.func_cache.get(&index) {
            let is_placeholder = f_v.count_basic_blocks() == 0
                || f_v
                    .get_first_basic_block()
                    .map_or(true, |bb| bb.get_first_instruction().is_none());
            return Ok((*f_v, is_placeholder));
        }

        let fun_ptr = self
            .findexes
            .get(&index)
            .ok_or_else(|| anyhow!("Function not found at index {}", index))?
            .clone();

        match fun_ptr {
            FuncPtr::Fun(f) => {
                let name = f.name();
                let type_fun = self.bytecode.types[f.type_.0]
                    .fun
                    .clone()
                    .expect("expect to get function type");
                let func_type = self.create_function_type(&type_fun)?;
                let placeholder = self.create_function_placeholder(&name, func_type);
                self.func_cache.insert(index, placeholder);
                Ok((placeholder, true))
            }
            FuncPtr::Native(native) => {
                let index = native.findex;
                let func = self.init_native_func(&native)?;
                self.func_cache.insert(index as usize, func);
                Ok((func, false))
            }
        }
    }

    pub(crate) fn create_function_type(
        &mut self,
        type_fun: &HLTypeFun,
    ) -> Result<FunctionType<'ctx>> {
        let ret_type = self.get_or_create_any_type(type_fun.ret.0)?;

        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let mut param_types: Vec<BasicMetadataTypeEnum<'ctx>> = type_fun
            .args
            .iter()
            .map(|arg| {
                let arg_type = self.get_or_create_any_type(arg.0).unwrap();
                (match arg_type {
                    AnyTypeEnum::FloatType(t) => t.as_basic_type_enum(),
                    AnyTypeEnum::IntType(t) => t.as_basic_type_enum(),
                    AnyTypeEnum::PointerType(t) => t.as_basic_type_enum(),
                    // Heap-allocated types are always passed by pointer
                    AnyTypeEnum::StructType(_)
                    | AnyTypeEnum::ArrayType(_)
                    | AnyTypeEnum::FunctionType(_)
                    | AnyTypeEnum::VectorType(_)
                    | AnyTypeEnum::ScalableVectorType(_)
                    | AnyTypeEnum::VoidType(_) => ptr_type.as_basic_type_enum(),
                })
                .into()
            })
            .collect();

        let function_type = match ret_type {
            AnyTypeEnum::FloatType(t) => t.fn_type(&param_types, false),
            AnyTypeEnum::IntType(t) => t.fn_type(&param_types, false),
            AnyTypeEnum::PointerType(t) => t.fn_type(&param_types, false),
            AnyTypeEnum::VoidType(_) => self.context.void_type().fn_type(&param_types, false),
            // Heap-allocated types are always returned by pointer
            AnyTypeEnum::StructType(_)
            | AnyTypeEnum::ArrayType(_)
            | AnyTypeEnum::FunctionType(_)
            | AnyTypeEnum::VectorType(_)
            | AnyTypeEnum::ScalableVectorType(_) => ptr_type.fn_type(&param_types, false),
        };

        Ok(function_type)
    }

    fn add_pending_compilation(&mut self, index: usize) {
        self.pending_compilations.push(index);
    }

    /// Drop any queued transitive compilations. Called by the tiered broker
    /// after a recovered hardware fault: the queue may still hold the findex
    /// whose translation faulted, and re-popping it on the next job would
    /// fault again, poisoning every subsequent promotion.
    pub fn clear_pending_compilations(&mut self) {
        self.pending_compilations.clear();
    }

    /// The findexes that resolve to natives, which the hot-reload rewrite must
    /// leave as direct calls: a native's address never changes, so there is
    /// nothing to patch and an indirect hop would only cost.
    fn native_findexes(&self) -> std::collections::HashSet<usize> {
        self.findexes
            .iter()
            .filter_map(|(k, v)| matches!(v, FuncPtr::Native(_)).then_some(*k))
            .collect()
    }

    fn compile_pending_functions(&mut self) -> Result<()> {
        while let Some(index) = self.pending_compilations.pop() {
            if let Err(e) = self.compile_function(index) {
                // Compilation failure is non-fatal — stub will be used
            }
        }
        Ok(())
    }

    fn compile_pending_functions_strict(&mut self) -> Result<()> {
        while let Some(index) = self.pending_compilations.pop() {
            self.compile_function(index)?;
            let f = self.func_cache.get(&index).ok_or_else(|| {
                anyhow!(
                    "Pending function {} missing from cache after compile",
                    index
                )
            })?;
            // `verify(true)`: on failure LLVM prints WHY to stderr
            // (LLVMPrintMessageAction); `false` reduced every verifier error
            // to an undiagnosed boolean.
            if !f.verify(true) {
                return Err(anyhow!(
                    "Strict promotion failed: function {} did not verify (diagnostic above)",
                    index
                ));
            }
        }
        Ok(())
    }

    pub(crate) fn compile_function(&mut self, index: usize) -> Result<()> {
        let _phase = crate::profile::scope("AIR v2 -> LLVM");
        // Skip if already compiled (has entry block with instructions)
        if let Some(func) = self.func_cache.get(&index) {
            if func.count_basic_blocks() > 0
                && func
                    .get_first_basic_block()
                    .map_or(false, |bb| bb.get_first_instruction().is_some())
            {
                return Ok(());
            }
        }

        let fun_ptr = self
            .findexes
            .get(&index)
            .ok_or_else(|| anyhow!("Function not found at index {}", index))?
            .clone();

        if let FuncPtr::Fun(f) = fun_ptr {
            // LLVM consumes AIR v2 directly. Serializing the verified SSA
            // function back into HashLink opcodes here made the old bytecode
            // translator the real backend and discarded AIR's phis, cells,
            // resolved fields and effects before code generation.
            // A shadow-stack target isolates callees too: Haxe's stack
            // arithmetic (`__skipStack`, the `sub(1)` in callStack) counts
            // one frame per Haxe function, and an inlined callee has none.
            let air = crate::llvm::air::prepare_llvm(
                &self.bytecode,
                &f,
                self.hot_reload,
                self.lazy_compilation || self.shadow_frames(),
            )
            .map_err(|e| anyhow!("AIR v2 refused findex {}: {e}", f.findex))?;

            // Create declaration if not in cache yet
            let function = if let Some(func) = self.func_cache.get(&index) {
                *func
            } else {
                let decl = self.create_function_declaration(&f)?;
                self.func_cache.insert(index, decl);
                decl
            };

            let basic_block = self.context.append_basic_block(function, "entry");
            self.builder.position_at_end(basic_block);
            self.emit_purity_barrier()?;

            self.translate_air_v2(&f, &air, function)?;

            if self
                .builder
                .get_insert_block()
                .unwrap()
                .get_terminator()
                .is_none()
            {
                let ret_type = function.get_type().get_return_type();
                if let Some(ret_type) = ret_type {
                    self.builder.build_return(Some(&ret_type.const_zero()))?;
                } else {
                    self.builder.build_return(None)?;
                }
            }

            if !function.verify(true) {
                // Function verification failed (non-fatal) — stub will be used
                // Function has invalid IR. We can't delete its blocks safely
                // (would create dangling references). Leave it as-is — the module
                // verification will catch it, but MCJIT may still compile valid
                // functions correctly.
            }
        } else if let FuncPtr::Native(native) = fun_ptr {
            // Ensure native function is initialized and in func_cache
            if !self.func_cache.contains_key(&index) {
                let func = self.init_native_func(&native)?;
                self.func_cache.insert(index, func);
            }
        }

        Ok(())
    }

    /// Which module a promotion compiles into.
    ///
    /// `ASH_PROMOTE_MODULE=0` forces the shared module for every promotion,
    /// `=1` forces a private one; unset lets
    /// `air::promotion_wants_full_module` decide per function, which is the
    /// shipped behaviour.
    fn promote_module_override() -> Option<bool> {
        static SPEC: std::sync::OnceLock<Option<bool>> = std::sync::OnceLock::new();
        *SPEC.get_or_init(|| std::env::var("ASH_PROMOTE_MODULE").ok().map(|v| v != "0"))
    }

    /// How many function bodies the shared module may hold before promotions
    /// stop being compiled into it. `ASH_PROMOTE_MODULE_CAP=off` removes it.
    ///
    /// This was off by default because the game hung on the capped arm and
    /// the cause was not known. It is known now: raising promotion throughput
    /// reached the audio path sooner, and `ToSFloat` was storing an f64 into
    /// a 4-byte HF32 register, which sent junk to `alSourcef(AL_GAIN)` and
    /// threw out through the main loop (e865132). The cap was the trigger,
    /// never the bug. With that fixed the same configuration ran a full
    /// session clean.
    fn promote_module_cap() -> Option<usize> {
        /// Bodies the shared module may hold before promotions stop joining it.
        ///
        /// The shared path re-optimizes every body already in the module, so
        /// its cost is the module's size and not the promoted function's:
        /// measured on deltablue, 4ms at 10 bodies and 22-30ms at 38-39, for
        /// roots of 16 to 44 AIR instructions with no relationship between
        /// size and time. On a game, where the module reaches thousands, one
        /// promotion cost 60s and the run spent 128.9s of its 170s compiling.
        /// Forcing every promotion into its own module there took that to 1.0s
        /// while optimizing MORE functions (55 against 37), and a player felt
        /// the difference.
        ///
        /// `promotion_wants_full_module` asks whether a function would benefit
        /// from being lowered beside its callees; it has no way to know what
        /// that costs today. This is the other half of that decision. Below
        /// the cap the module is small enough that the walk is cheap and the
        /// inlining is worth having; above it, it is not.
        const DEFAULT_CAP: usize = 256;
        static CAP: std::sync::OnceLock<Option<usize>> = std::sync::OnceLock::new();
        *CAP.get_or_init(|| match std::env::var("ASH_PROMOTE_MODULE_CAP") {
            Ok(v) if v == "off" => None,
            Ok(v) => v.parse().ok(),
            Err(_) => Some(DEFAULT_CAP),
        })
    }

    /// Function bodies currently in the shared module -- what a promotion into
    /// it pays the middle end to walk.
    fn shared_module_bodies(&self) -> usize {
        self.module
            .get_functions()
            .filter(|f| f.count_basic_blocks() > 0)
            .count()
    }

    /// Whether `findex` compiles into a module of its own.
    fn promote_uses_own_module(&self, findex: usize) -> bool {
        if self.lazy_compilation {
            return true;
        }
        if let Some(forced) = Self::promote_module_override() {
            return forced;
        }
        // A promotion into the shared module re-optimizes the whole module, so
        // its cost tracks the module's size rather than the promoted function:
        // measured on a large program, cost correlates +0.93 with body count and -0.07
        // with the root's own size, and a 4-instruction root cost 2.3s. Once
        // the module is large enough for that walk to outweigh the inlining
        // the shared path buys, promote alone instead. Benchmarks reach 19
        // bodies, so the cap only engages on program-sized workloads.
        if let Some(cap) = Self::promote_module_cap() {
            if self.shared_module_bodies() >= cap {
                return true;
            }
        }
        let Some(raw) = self
            .bytecode
            .functions
            .iter()
            .find(|f| f.findex as usize == findex)
        else {
            return false;
        };
        !super::air::promotion_wants_full_module(&self.bytecode, raw, self.hot_reload)
    }

    /// Lower `findex` into a private module and hand that to the engine.
    ///
    /// Mirrors `compile_osr_entry`: swap the module and every by-index cache
    /// out, build, resolve whatever the module leaves undefined against the
    /// addresses the host already holds, and swap back. Returns the address.
    fn promote_in_own_module(&mut self, findex: usize) -> Result<usize> {
        let modname = format!("promote_{findex}");
        let promo_module = self.context.create_module(&modname);
        // A fresh module names no target, and the middle end reads the triple
        // back off the module to choose one. Leaving it unset does not produce
        // slower code -- it fails the promotion outright, and the caller reads
        // a tier failure as a reason to blacklist the function for the rest of
        // the run. Every hot function, permanently, on the tier that is
        // supposed to be the fast one.
        self.target_abi.apply_to_module(&promo_module)?;
        self.builder.clear_insertion_position();

        let host_module = std::mem::replace(&mut self.module, promo_module);
        let host_funcs = std::mem::take(&mut self.func_cache);
        let host_ints = std::mem::take(&mut self.int_globals);
        let host_floats = std::mem::take(&mut self.float_globals);
        let host_strings = std::mem::take(&mut self.string_globals);
        let host_bytes = std::mem::take(&mut self.bytes_globals);
        let host_types = std::mem::take(&mut self.type_info_globals);
        // Seed with this function's own constants and let anything else
        // materialize on demand (`ensure_*_global`). Cloning the entire pool
        // instead put a game's whole constant table into a module holding ONE
        // function, and the module-level passes then walk all of it: a game
        // spent 40s compiling a 94-instruction function that way. The seed is
        // only an optimization now -- correctness no longer depends on it
        // predicting which constants the optimized body will reference, which
        // a raw-opcode scan cannot do once AIR V2 inlines a callee.
        self.create_constant_pool_globals_for(findex);

        let built: Result<()> = (|| {
            self.compile_function(findex)?;
            // Callees stay declarations, bound below to the addresses the host
            // already holds. Lowering copies of them here costs as much as the
            // shared module and buys nothing measurable -- a function that
            // needs them is sent down the shared path instead.
            self.pending_compilations.clear();
            Ok(())
        })();

        self.builder.clear_insertion_position();
        let promo_module = std::mem::replace(&mut self.module, host_module);
        // Kept, not dropped: this is the module's findex -> value map, and it
        // is the only exact identity for the callees it left as declarations.
        let promo_funcs = std::mem::replace(&mut self.func_cache, host_funcs);
        let target = promo_funcs.get(&findex).copied();
        self.int_globals = host_ints;
        self.float_globals = host_floats;
        self.string_globals = host_strings;
        self.bytes_globals = host_bytes;
        self.type_info_globals = host_types;
        built?;

        let target =
            target.ok_or_else(|| anyhow!("promote module {modname}: no function built"))?;
        // Rename to something only this promotion can answer to. The address
        // below is fetched from the engine BY NAME, and `HLFunction::name` is
        // the bare Haxe field name -- half the functions in a real program
        // share one (810 are called `_`). Asking MCJIT for `dispose` returns
        // whichever module defined that symbol first, so the promotion could
        // install a different class's method for this findex and nothing
        // would say so. Nothing refers to this symbol by name except the
        // lookup two lines down: callers hold addresses, and everything the
        // module leaves undefined is bound by findex.
        target
            .as_global_value()
            .set_name(&format!("{modname}$entry"));
        let name = target
            .get_name()
            .to_str()
            .map_err(|_| anyhow!("promote module {modname}: invalid symbol name"))?
            .to_string();

        {
            let _phase = crate::profile::scope("llvm middle-end (promote)");
            // The module holds this promotion and nothing else, so there is
            // nothing here to park.
            let excluded = self.shield_trap_functions_in(&promo_funcs);
            crate::profile::count("middle-end functions excluded (trap)", excluded as u64);
            let n = promo_module
                .get_functions()
                .filter(|f| f.count_basic_blocks() > 0)
                .count();
            crate::profile::count("middle-end functions processed", n as u64);
            crate::profile::count("middle-end functions in module", n as u64);
            let me_t0 = std::time::Instant::now();
            let me_result = super::module::run_middle_end(&promo_module);
            report_slow_promote(findex, "own", n, me_t0.elapsed().as_secs_f64() * 1e3);
            me_result?;
            if std::env::var_os("ASH_MIDDLE_END_LOG").is_some() {
                let globals = promo_module.get_globals().count();
                let decls = promo_module
                    .get_functions()
                    .filter(|f| f.count_basic_blocks() == 0)
                    .count();
                eprintln!(
                    "[me] findex={findex} bodies={n} decls={decls} globals={globals} \
                     took={:.0}ms",
                    me_t0.elapsed().as_secs_f64() * 1e3
                );
            }
        }

        if let Err(e) = promo_module.verify() {
            return Err(anyhow!("promote module {modname} failed verification: {e}"));
        }

        self.bind_module_declarations(
            &promo_module,
            &promo_funcs,
            &format!("promote module {modname}"),
        )?;

        self.execution_engine
            .add_module(&promo_module)
            .map_err(|()| anyhow!("promote module {modname} rejected by the engine"))?;
        let addr = {
            let _phase = crate::profile::scope("mcjit codegen");
            self.execution_engine
                .get_function_address(&name)
                .map_err(|e| anyhow!("promote module {modname}: {e}"))?
        };
        if addr == 0 {
            return Err(anyhow!("promote module {modname}: zero address"));
        }
        // Every body in the module, under ITS OWN findex. This used to label
        // each inlined callee body with the promoted function's findex, which
        // is the small version of the hole the shared path had: a crash in a
        // copied callee was reported as the function that inlined it.
        let mut found: Vec<(usize, usize)> = Vec::new();
        for (&fi, f) in &promo_funcs {
            if f.count_basic_blocks() == 0 {
                continue;
            }
            if let Ok(sym) = f.get_name().to_str() {
                if let Ok(a) = self.execution_engine.get_function_address(sym) {
                    if a != 0 {
                        found.push((fi, a as usize));
                    }
                }
            }
        }
        register_batch(found, "own");
        Ok(addr)
    }

    /// Findexes `ASH_NO_PROMOTE` withholds from the optimising tier.
    ///
    /// A bisection tool, not a policy: when a program misbehaves only once
    /// some function reaches LLVM, the question "which one" has no answer
    /// short of trying, and the tier log names the candidates. Comma-separated
    /// findexes; unset denies nothing.
    fn promotion_denied(findex: usize) -> bool {
        static DENY: std::sync::OnceLock<Vec<usize>> = std::sync::OnceLock::new();
        let deny = DENY.get_or_init(|| {
            std::env::var("ASH_NO_PROMOTE")
                .ok()
                .map(|spec| {
                    spec.split(',')
                        .filter_map(|w| w.trim().parse().ok())
                        .collect()
                })
                .unwrap_or_default()
        });
        deny.contains(&findex)
    }

    pub fn promote_function_strict(&mut self, findex: usize) -> Result<CompiledFunctionMeta> {
        let _phase = crate::profile::scope("llvm promote");
        crate::profile::count("llvm promotions", 1);
        if Self::promotion_denied(findex) {
            return Err(anyhow!(
                "promotion of findex {findex} denied by ASH_NO_PROMOTE"
            ));
        }
        self.natives_missing_in_compile.borrow_mut().clear();
        // Promotion currently targets bytecode functions only.
        if !self.findexes.contains_key(&findex) {
            return Err(anyhow!(
                "Strict promotion failed: unknown findex {}",
                findex
            ));
        }

        // AOT lowers everything into ONE module: the per-function promo module
        // exists to hand MCJIT a small unit to codegen and is added to the
        // engine, which an AOT build has no use for and cannot satisfy —
        // resolving a symbol in this process is exactly what AOT must not do.
        if !self.aot && self.promote_uses_own_module(findex) {
            // A private module leaves its callees as declarations and binds
            // them to code the host already has. When a callee has none --
            // no lower tier compiled it, which is the norm with the ladder
            // pinned to one rung -- there is nothing to bind and the
            // promotion refuses. Fall through to the shared path, which
            // lowers the callees beside the root, rather than propagating:
            // the caller treats a tier failure as a reason to blacklist, so
            // refusing here retired the function permanently and it
            // interpreted for the rest of the run. That is what made
            // `--jit-tier llvm` run `free_call` and `inlined_call` at
            // interpreter speed, 66x slower than the same program on
            // Cranelift, while reporting the LLVM tier as the engine.
            match self.promote_in_own_module(findex) {
                Ok(fn_addr) => {
                    self.install_function_address(findex, fn_addr as *mut c_void);
                    return self.compiled_meta_for(findex, fn_addr);
                }
                Err(e) => {
                    if std::env::var_os("ASH_TIER_LOG").is_some() {
                        eprintln!(
                            "[tier] findex={findex} own-module refused ({e:#}); \
                             retrying through the shared module"
                        );
                    }
                    crate::profile::count("llvm promotions retried in shared module", 1);
                }
            }
        }

        // Reaching here means the shared module: either the own-module path
        // was refused -- an unresolved callee symbol, most often -- or it was
        // not attempted. The shared path charges for the whole module, and a
        // ceiling below High cannot repay that. Declining leaves the function
        // on its Cranelift code, which is what it was running on already.
        // A reload's recompile has no such fallback: the body it replaces is
        // the wrong one.
        if !self.aot && !self.reload_recompile {
            if let Some(raw) = self
                .bytecode
                .functions
                .iter()
                .find(|f| f.findex as usize == findex)
            {
                let ceiling = super::air::llvm_ceiling(&self.bytecode, raw);
                if !super::air::shared_promote_allows(ceiling) {
                    return Err(anyhow!(
                        "declined: {ceiling:?} ceiling is not worth the shared module \
                         ({} bodies)",
                        self.shared_module_bodies()
                    ));
                }
            }
        }
        let (_function, is_placeholder) = self.get_or_create_function_value(findex)?;
        if is_placeholder {
            self.add_pending_compilation(findex);
        }

        let lowered = self
            .compile_pending_functions_strict()
            .and_then(|()| self.compile_function(findex))
            .and_then(|()| self.compile_pending_functions_strict());
        if let Err(error) = lowered {
            // A refused body is not an absent one. The emitter stops where it
            // failed and leaves behind blocks with no terminator, which are
            // invisible until the next middle-end run walks the module and
            // dies inside SimplifyCFG -- far from the function that caused
            // it. Sealing keeps the refusal local to the body that earned it.
            self.seal_partial_bodies()?;
            return Err(error);
        }

        // Optimize before asking for the address, because asking is what
        // forces codegen. Without this the tiered LLVM tier shipped raw
        // lowering output -- no mem2reg, no inlining, no GVN, no LICM -- and
        // lost to Cranelift on nbody by 1.5s, which is not a thing a top tier
        // should do. Only the whole-module path ran the middle end.
        //
        // Scoped to this function and its callees: a promotion pays for the
        // function it is promoting, not for the whole module again.
        //
        // Not under AOT. There is no address to ask for, so nothing forces
        // codegen per function, and running the module pipeline once per
        // lowered body is both quadratic and destructive: it deletes the
        // emitted data that no lowered body happens to reference YET, and the
        // next body to want that type finds a handle pointing at freed
        // memory. AOT optimizes once, after everything exists -- see
        // `optimize_module`.
        if !self.aot {
            let _phase = crate::profile::scope("llvm middle-end (promote)");
            let excluded = self.shield_trap_functions_from_optimization();
            crate::profile::count("middle-end functions excluded (trap)", excluded as u64);
            let target = *self.func_cache.get(&findex).ok_or_else(|| {
                anyhow!(
                    "Strict promotion failed: function {} missing from cache",
                    findex
                )
            })?;
            // Which bodies this promote is paying to optimize; the middle-end
            // cost lives or dies by this list.
            if std::env::var_os("ASH_PROMOTE_FNS").is_some() {
                for func in self.module.get_functions() {
                    let bbs = func.count_basic_blocks();
                    if bbs > 0 {
                        eprintln!(
                            "[promote-fns] findex={findex} {} blocks={bbs}",
                            func.get_name().to_str().unwrap_or("?")
                        );
                    }
                }
            }
            let parked = self.park_optimized_functions(target);
            // What the shared path is actually buying. It exists so the
            // inliner has callee bodies to work with, and it pays for the
            // whole transitive closure to get them -- 263 functions per
            // promotion in a large program. Inlining removes calls from the root and
            // grows it, so measuring the root either side of the middle end
            // says how many of those bodies were worth lowering.
            let root_shape = |f: inkwell::values::FunctionValue<'ctx>| -> (usize, usize) {
                let mut calls = 0;
                let mut instrs = 0;
                for bb in f.get_basic_blocks() {
                    let mut i = bb.get_first_instruction();
                    while let Some(ins) = i {
                        instrs += 1;
                        if matches!(ins.get_opcode(), inkwell::values::InstructionOpcode::Call) {
                            calls += 1;
                        }
                        i = ins.get_next_instruction();
                    }
                }
                (calls, instrs)
            };
            let before = std::env::var_os("ASH_INLINE_LOG")
                .is_some()
                .then(|| root_shape(target));
            let me_t0 = std::time::Instant::now();
            let result = super::module::run_middle_end(&self.module);
            report_slow_promote(
                findex,
                "shared",
                self.module
                    .get_functions()
                    .filter(|f| f.count_basic_blocks() > 0)
                    .count(),
                me_t0.elapsed().as_secs_f64() * 1e3,
            );
            if let Some((calls_before, instrs_before)) = before {
                let (calls_after, instrs_after) = root_shape(target);
                eprintln!(
                    "[inline] findex={findex} lowered={} parked={} root_calls={}->{} root_instrs={}->{} me={:.0}ms",
                    self.module.get_functions().filter(|f| f.count_basic_blocks() > 0).count(),
                    parked.len(),
                    calls_before,
                    calls_after,
                    instrs_before,
                    instrs_after,
                    me_t0.elapsed().as_secs_f64() * 1e3,
                );
            }
            if std::env::var_os("ASH_MIDDLE_END_LOG").is_some() {
                let bodies = self
                    .module
                    .get_functions()
                    .filter(|f| f.count_basic_blocks() > 0)
                    .count();
                let globals = self.module.get_globals().count();
                eprintln!(
                    "[me-shared] findex={findex} bodies={bodies} parked={} globals={globals} took={:.0}ms",
                    parked.len(),
                    me_t0.elapsed().as_secs_f64() * 1e3
                );
            }
            self.release_parked_functions(&parked);
            result?;
            self.record_optimized_functions(&parked);
        }

        // A body that reached an unresolved helper carries a stub where the
        // call belongs, so it must not install. The caller reads a tier
        // failure as a reason to blacklist, which leaves the findex on the
        // tier below.
        {
            let missing = self.natives_missing_in_compile.borrow();
            if !missing.is_empty() {
                let mut names: Vec<&str> = missing.iter().map(String::as_str).collect();
                names.sort_unstable();
                names.dedup();
                return Err(anyhow!(
                    "Strict promotion failed: findex {} calls unresolved runtime {} ({})",
                    findex,
                    if names.len() == 1 {
                        "helper"
                    } else {
                        "helpers"
                    },
                    names.join(", ")
                ));
            }
        }

        let function = *self.func_cache.get(&findex).ok_or_else(|| {
            anyhow!(
                "Strict promotion failed: function {} missing from cache",
                findex
            )
        })?;
        // As above: print the diagnostic, do not swallow it into a bool.
        if !function.verify(true) {
            return Err(anyhow!(
                "Strict promotion failed: function {} did not verify (diagnostic above)",
                findex
            ));
        }

        // `ASH_DUMP_FN_IR=<findex,...|all>`: print this function's LLVM IR as
        // it will execute — post middle-end, the exact input to codegen. The
        // third panel of the AIR / CLIF / LLVM side-by-side that latency
        // investigations read (AIR: `Function::dump`, CLIF: `ASH_CL_DUMP`).
        if Self::fn_ir_dump_wanted_impl(findex) {
            eprintln!(
                "=== LLVM IR (promote) findex={findex} ===\n{}",
                function.print_to_string().to_string()
            );
        }

        let name = function.get_name().to_str().map_err(|_| {
            anyhow!(
                "Strict promotion failed: invalid symbol name for {}",
                findex
            )
        })?;
        // The JIT/AOT fork. Everything above is target-independent IR
        // construction; only this tail differs. In AOT there is no address to
        // return -- the function exists as a symbol in the module, and
        // `emit_object` turns the whole module into relocatable code later.
        if self.aot {
            return Ok(CompiledFunctionMeta::aot_placeholder(findex));
        }
        // Where MCJIT actually emits machine code: the address request is what
        // forces codegen and relocation for everything reachable.
        let fn_addr = {
            let _phase = crate::profile::scope("mcjit codegen");
            self.execution_engine
                .get_function_address(name)
                .map_err(|e| {
                    anyhow!(
                        "Strict promotion failed: get_function_address({}) -> {}",
                        name,
                        e
                    )
                })?
        };
        if fn_addr == 0 {
            return Err(anyhow!(
                "Strict promotion failed: zero function address for {}",
                findex
            ));
        }
        self.install_function_address(findex, fn_addr as *mut c_void);
        // The shared module just emitted every pending body -- thousands on a
        // game -- and only this one entry was registered. A crash in any of
        // the others was attributed to the nearest registered entry below it:
        // the 2026-09-02 escape crash named a 12-opcode accessor for a pc
        // 43,320 bytes past its entry. Register them all, once each, with the
        // size each body has by construction -- bodies of one batch are laid
        // out back to back, so a body ends where the next one starts.
        self.register_shared_bodies();

        self.compiled_meta_for(findex, fn_addr)
    }

    /// The signature the tiered caller marshals through, read from the
    /// bytecode rather than the compiled function: it is tier-independent.
    fn compiled_meta_for(&self, findex: usize, fn_addr: usize) -> Result<CompiledFunctionMeta> {
        let fidx = self
            .bytecode
            .functions
            .iter()
            .position(|f| f.findex as usize == findex)
            .ok_or_else(|| {
                anyhow!(
                    "Strict promotion failed: {} is not a bytecode function",
                    findex
                )
            })?;
        let f = &self.bytecode.functions[fidx];
        let tf = self.bytecode.types[f.type_.0].fun.as_ref().ok_or_else(|| {
            anyhow!(
                "Strict promotion failed: missing function type for {}",
                findex
            )
        })?;
        let arg_kinds = tf
            .args
            .iter()
            .map(|a| self.bytecode.types[a.0].kind)
            .collect::<Vec<_>>();
        let ret_kind = self.bytecode.types[tf.ret.0].kind;

        Ok(CompiledFunctionMeta {
            findex,
            fn_addr,
            arg_kinds,
            ret_kind,
        })
    }

    pub(crate) fn create_function_value(&mut self, index: usize) -> Result<FunctionValue<'ctx>> {
        if let Some(f_v) = self.func_cache.get(&index) {
            return Ok(*f_v);
        }
        let findexes = self.findexes.clone();
        let fun_ptr = findexes
            .get(&index)
            .ok_or_else(|| anyhow!("Function not found at index {}", index))?;

        match fun_ptr {
            FuncPtr::Fun(f) => {
                let f = f.clone();
                let air = crate::llvm::air::prepare_llvm(
                    &self.bytecode,
                    &f,
                    self.hot_reload,
                    self.lazy_compilation,
                )
                .map_err(|e| anyhow!("AIR v2 refused findex {}: {e}", f.findex))?;

                let function = self.create_function_declaration(&f)?;
                let basic_block = self.context.append_basic_block(function, "entry");
                self.builder.position_at_end(basic_block);

                self.translate_air_v2(&f, &air, function)?;

                if self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_terminator()
                    .is_none()
                {
                    let ret_type = function.get_type().get_return_type();
                    if let Some(ret_type) = ret_type {
                        self.builder.build_return(Some(&ret_type.const_zero()))?;
                    } else {
                        self.builder.build_return(None)?;
                    }
                }

                if !function.verify(true) {
                    function.print_to_stderr();
                    return Err(anyhow!(
                        "Function verification failed for findex {}",
                        f.findex
                    ));
                }

                self.func_cache.insert(f.findex as usize, function);
                Ok(function)
            }
            FuncPtr::Native(native) => {
                let func = self.init_native_func(native)?;
                self.func_cache.insert(native.findex as usize, func);
                Ok(func)
            }
        }
    }

    /// Compile an entry point that begins at `header_pc` instead of at the
    /// top of the function, taking the live register file as a buffer.
    ///
    /// This is what lets a loop be entered while an interpreter frame for its
    /// function is already running. Promotion counts calls, so a loop inside a
    /// function called once is invisible to it -- nbody's `main` runs ten
    /// million iterations in a single invocation, and without this the loop
    /// stays interpreted and pays a boundary crossing per call it makes.
    ///
    /// `body` is the caller's own opcode array, not one this module derives.
    /// Both sides run AIR, which renumbers opcodes, so a `header_pc` computed
    /// against the interpreter's body would name a different instruction in a
    /// separately-optimized copy. Taking the body removes the question.
    ///
    /// The buffer holds one 64-bit slot per register in the interpreter's
    /// representation, which is what `value_to_i64` already produces for the
    /// ordinary call boundary.
    pub fn compile_osr_entry(
        &mut self,
        findex: usize,
        header_pc: usize,
        optimized: &crate::air_pipeline::Optimized,
    ) -> Result<u64> {
        self.compile_snapshot_entry(findex, header_pc, optimized, None)
    }

    /// A separate ABI: slots name exact SSA inputs, not de-SSA registers.
    pub fn compile_retier_entry(&mut self, layout: &crate::retier::Layout) -> Result<u64> {
        self.compile_snapshot_entry(layout.findex, layout.pc, &layout.air, Some(layout))
    }

    fn compile_snapshot_entry(
        &mut self,
        findex: usize,
        header_pc: usize,
        optimized: &crate::air_pipeline::Optimized,
        snapshot: Option<&crate::retier::Layout>,
    ) -> Result<u64> {
        let _phase = crate::profile::scope("llvm osr entry");
        let header = if let Some(layout) = snapshot {
            // Empty blocks can share a serialized pc. A typed snapshot names
            // its exact AIR header, not the first block with that pc.
            layout.header.idx()
        } else {
            optimized
                .ser
                .block_pcs
                .iter()
                .position(|&pc| pc == header_pc)
                .ok_or_else(|| {
                    anyhow!("osr header pc {header_pc} is not an AIR block in findex {findex}")
                })?
        };
        let source = self
            .bytecode
            .functions
            .iter()
            .find(|f| f.findex as usize == findex)
            .cloned()
            .ok_or_else(|| anyhow!("osr findex {findex} is not a bytecode function"))?;
        let name = match snapshot {
            Some(layout) => format!("retier_{}_{}", findex, layout.id),
            None => format!("osr_{findex}_{header_pc}"),
        };
        if let Ok(addr) = self.execution_engine.get_function_address(&name) {
            if addr != 0 {
                return Ok(addr as u64);
            }
        }

        // Build into a module of its own, and hand that to the engine.
        //
        // The first attempt added the entry to the main module, which MCJIT
        // had already emitted -- a module's object is produced once, so the
        // new function had no address and `get_function_address` answered "not
        // found". That is not a limitation of MCJIT: it holds several modules
        // and resolves symbols across them. The entry just has to arrive as a
        // module rather than as an afterthought to a finished one.
        //
        // `func_cache` is emptied for the duration so calls in the body become
        // declarations in this module, which the engine resolves by name
        // against the module that defines them. The constant pools are rebuilt
        // rather than cleared: they are read by index and hold immutable
        // values, so a private copy is correct and a missing one is not.
        let osr_module = self.context.create_module(&name);
        // See `promote_in_own_module`: a module with no triple cannot be
        // optimized, and this one holds the body a hot loop actually runs.
        self.target_abi.apply_to_module(&osr_module)?;
        // The builder is shared, and whatever it last pointed at belongs to the
        // module about to be swapped out. `generate_native_caller_with_addr`
        // opens by saving `get_insert_block()`, which asserts on anything that
        // is not a live block, so leave it pointing at nothing.
        self.builder.clear_insertion_position();
        let host_module = std::mem::replace(&mut self.module, osr_module);
        let host_funcs = std::mem::take(&mut self.func_cache);
        let host_ints = std::mem::take(&mut self.int_globals);
        let host_floats = std::mem::take(&mut self.float_globals);
        let host_strings = std::mem::take(&mut self.string_globals);
        let host_bytes = std::mem::take(&mut self.bytes_globals);
        let host_types = std::mem::take(&mut self.type_info_globals);
        // Seed with this entry's own constants, as the promote path does.
        // Anything the optimized body turns out to reference materializes on
        // demand through `ensure_*_global`, so the seed is an optimization and
        // not a prediction it has to get right.
        //
        // The `else` arm here took the WHOLE pool, and `lazy_compilation` is
        // false in --mode hybrid, so every OSR entry a game built carried the
        // program's entire constant table: measured on a game, 1194 ints + 717
        // floats + 17998 strings = 19909 globals and ~708KB of UTF-16 rodata,
        // each string re-encoded with encode_utf16().collect() as it was
        // added. They are emitted with external linkage, so GlobalDCE and
        // GlobalOpt in the default<O2> run below cannot drop a single one, and
        // no module is ever removed from the engine.
        self.create_constant_pool_globals_for(findex);

        if std::env::var_os("ASH_OSR_LOG").is_some() {
            eprintln!("[osr] LLVM AIR build begin findex={findex} pc={header_pc}");
        }
        let built = self.build_air_osr_body(
            &source,
            &optimized.ir,
            AirBlockId(header as u32),
            header_pc,
            &name,
            snapshot,
        );

        self.builder.clear_insertion_position();
        let osr_module = std::mem::replace(&mut self.module, host_module);
        let osr_funcs = std::mem::replace(&mut self.func_cache, host_funcs);
        self.int_globals = host_ints;
        self.float_globals = host_floats;
        self.string_globals = host_strings;
        self.bytes_globals = host_bytes;
        self.type_info_globals = host_types;
        built?;
        if std::env::var_os("ASH_OSR_LOG").is_some() {
            eprintln!("[osr] LLVM AIR build done findex={findex} pc={header_pc}");
        }

        // The verifier catches a reference to a value left behind in the host
        // module, which is the failure this swap could produce.
        if let Err(e) = osr_module.verify() {
            return Err(anyhow!("osr module {name} failed verification: {}", e));
        }
        // The OSR entry is the body the hot loop actually executes, so IR
        // questions about steady-state code are questions about THIS module,
        // not the ordinary one ASH_DUMP_IR writes.
        if let Ok(dir) = std::env::var("ASH_DUMP_OSR_IR") {
            if !dir.is_empty() && dir != "0" {
                let path = format!("{dir}/{name}.ll");
                match osr_module.print_to_file(&path) {
                    Ok(()) => eprintln!("[ash] OSR IR written to {path}"),
                    Err(e) => eprintln!("[ash] could not write {path}: {e}"),
                }
            }
        }
        // Bind every symbol this module leaves undefined to the address the
        // host already has for it. MCJIT resolves across the modules it holds,
        // but only for symbols that are actually defined somewhere it can see;
        // a bytecode function that was never compiled has no definition, and
        // the call lands on a null pointer. Resolving them explicitly is the
        // only way a fresh module reaches the runtime symbols.
        self.bind_module_declarations(&osr_module, &osr_funcs, &format!("osr module {name}"))?;

        self.execution_engine
            .add_module(&osr_module)
            .map_err(|()| anyhow!("osr module {name} rejected by the engine"))?;
        if std::env::var_os("ASH_OSR_LOG").is_some() {
            eprintln!("[osr] LLVM module attached findex={findex} pc={header_pc}");
        }
        let addr = self
            .execution_engine
            .get_function_address(&name)
            .map_err(|e| anyhow!("osr entry {name}: get_function_address failed: {e}"))?;
        if addr == 0 {
            return Err(anyhow!("osr entry {name}: zero address"));
        }
        crate::profile::count("osr entries compiled", 1);
        // Register EVERY function this module defines, not just the entry.
        // The module carries its own copies of the native-caller thunks and
        // constant plumbing; on a NUC mandelbrot profile 72% of samples sat
        // in those unregistered ranges, filed under `unknown` while the
        // entry itself attributed fine.
        for f in osr_module.get_functions() {
            if f.count_basic_blocks() == 0 {
                continue; // declaration, defined elsewhere
            }
            if let Ok(sym) = f.get_name().to_str() {
                if let Ok(a) = self.execution_engine.get_function_address(sym) {
                    if a != 0 && a as u64 != addr as u64 {
                        crate::jit_map::register(
                            findex as u32,
                            crate::profile::Tier::Llvm,
                            crate::jit_map::CodeKind::OsrEntry,
                            a as usize,
                            0,
                        );
                    }
                }
            }
        }
        // Register the entry so samples inside it are charged to the function
        // it belongs to. Without this the sampler has no symbol for the
        // address range and reports the time as `unknown` -- which on nbody was
        // 59.5% of the run, i.e. all of the work OSR had just moved into
        // compiled code.
        crate::jit_map::register(
            findex as u32,
            crate::profile::Tier::Llvm,
            crate::jit_map::CodeKind::OsrEntry,
            addr as usize,
            0,
        );
        return Ok(addr as u64);
    }

    /// Register every compiled body the shared module holds that has not
    /// been registered yet, each under its own findex.
    ///
    /// Idempotent across promotions: the set below remembers what is done,
    /// so a batch of 3,300 bodies costs 3,300 hash probes and only the new
    /// ones ask the engine for an address.
    fn register_shared_bodies(&mut self) {
        let mut fresh: Vec<(usize, String)> = Vec::new();
        {
            let done = registered_bodies()
                .lock()
                .expect("registered bodies poisoned");
            for (&fi, f) in &self.func_cache {
                if done.contains(&fi) || f.count_basic_blocks() == 0 {
                    continue;
                }
                if let Ok(sym) = f.get_name().to_str() {
                    fresh.push((fi, sym.to_owned()));
                }
            }
        }
        let mut found: Vec<(usize, usize)> = Vec::new();
        for (fi, sym) in fresh {
            if let Ok(a) = self.execution_engine.get_function_address(&sym) {
                if a != 0 {
                    found.push((fi, a as usize));
                }
            }
        }
        register_batch(found, "shared");
    }

    /// Emit an AIR V2 OSR entry into whatever module is current.
    ///
    /// Cranelift spills the de-SSA register image described by the shared
    /// optimized AIR cache. Restoring that image directly into AIR values and
    /// cells keeps the transition in the typed IR.
    fn build_air_osr_body(
        &mut self,
        source: &HLFunction,
        air: &AirFunction,
        header: AirBlockId,
        header_pc: usize,
        name: &str,
        snapshot: Option<&crate::retier::Layout>,
    ) -> Result<()> {
        // `(ptr) -> ret`. Compiled snapshots match Cranelift's entry ABI:
        // sub-word integer returns are zero-extended to i32. Keep the
        // interpreter OSR signature unchanged.
        let type_fun = self.bytecode.types[source.type_.0]
            .fun
            .clone()
            .ok_or_else(|| anyhow!("findex {} has no function type", source.findex))?;
        let ptr_ty = self.context.ptr_type(AddressSpace::default());
        let ret_any = self.get_or_create_any_type(type_fun.ret.0)?;
        let fn_ty = match ret_any {
            AnyTypeEnum::VoidType(t) => t.fn_type(&[ptr_ty.into()], false),
            AnyTypeEnum::IntType(t) if snapshot.is_some() && t.get_bit_width() < 32 => {
                self.context.i32_type().fn_type(&[ptr_ty.into()], false)
            }
            AnyTypeEnum::IntType(t) => t.fn_type(&[ptr_ty.into()], false),
            AnyTypeEnum::FloatType(t) => t.fn_type(&[ptr_ty.into()], false),
            AnyTypeEnum::PointerType(t) => t.fn_type(&[ptr_ty.into()], false),
            _ => ptr_ty.fn_type(&[ptr_ty.into()], false),
        };
        let function = self.module.add_function(name, fn_ty, None);
        self.stamp_host_cpu(function);

        let entry = self.context.append_basic_block(function, "osr_entry");
        self.builder.position_at_end(entry);
        let lowering = Self::air_lowering_table(source, air);
        let (registers, reg_types) = self.allocate_air_registers(air, &lowering)?;
        let cell_base = air.values.len();

        // Reconstruct the header state using the selected transfer ABI.
        // Compiled snapshots seed exact SSA inputs; interpreter entries
        // retain the original de-SSA register-image convention.
        let buf = function
            .get_nth_param(0)
            .ok_or_else(|| anyhow!("osr entry has no buffer parameter"))?
            .into_pointer_value();
        if let Some(layout) = snapshot {
            for (offset, input) in layout.slots.iter().enumerate() {
                let index = match input.input {
                    crate::retier::Input::Value(v) => v.idx(),
                    crate::retier::Input::Cell(c) => cell_base + c.idx(),
                };
                let restored = self.load_air_osr_slot(buf, offset as u32, reg_types[index])?;
                self.builder.build_store(registers[index], restored)?;
            }
        } else {
            // Preserve the interpreter ABI. Re-tier entries never read an
            // unspecified slot or seed multiple SSA values from one register.
            for (i, value) in air.values.iter().enumerate() {
                let restored = self.load_air_osr_slot(buf, value.reg, reg_types[i])?;
                self.builder.build_store(registers[i], restored)?;
            }
            for (ci, cell) in air.cells.iter().enumerate() {
                let slot = cell_base + ci;
                let restored = self.load_air_osr_slot(buf, cell.reg, reg_types[slot])?;
                self.builder.build_store(registers[slot], restored)?;
            }
        }

        let mut included = vec![false; air.blocks.len()];
        let mut stack = vec![header];
        while let Some(block) = stack.pop() {
            if included[block.idx()] {
                continue;
            }
            included[block.idx()] = true;
            stack.extend(air.blocks[block.idx()].term.successors());
        }

        self.emit_air_v2_cfg(
            source,
            air,
            function,
            &lowering,
            &registers,
            &reg_types,
            cell_base,
            &included,
            header,
        )?;
        if std::env::var_os("ASH_OSR_LOG").is_some() {
            eprintln!(
                "[osr] LLVM AIR CFG emitted findex={} pc={header_pc}",
                source.findex
            );
        }

        // `emit_air_v2_cfg` leaves the builder in an unreachable convenience
        // block, matching ordinary AIR lowering. Close it for verification.
        if self
            .builder
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_none()
        {
            match function.get_type().get_return_type() {
                Some(ret_type) => {
                    self.builder.build_return(Some(&ret_type.const_zero()))?;
                }
                None => {
                    self.builder.build_return(None)?;
                }
            }
        }

        if !function.verify(true) {
            unsafe { function.delete() };
            return Err(anyhow!(
                "AIR OSR entry for findex {} pc {header_pc} failed verification",
                source.findex
            ));
        }

        // Bring in whatever the body calls. Lowering queues each callee it
        // could not find in the (deliberately empty) cache, and without this
        // they stay declarations that resolve to nothing -- the first version
        // left `Fun_16`, `Fun_20` and `Fun_23` undefined and jumped through a
        // null pointer. Compiling them here duplicates their code into this
        // module, which is the price of the module being self-contained.
        // Duplicate the callee closure into this module only when the body
        // actually needs it, using the same question the promote path asks:
        // does a loop on the hot path still contain a call the AIR inliner did
        // not remove.
        //
        // Lowering them unconditionally made an OSR entry carry the transitive
        // direct-call closure of its body and run default<O2> over all of it.
        // On bench_free_call that was 83.13ms of a 106ms run -- 78.6% --
        // against 3.50ms for the promotion the entry belongs to, and dropping
        // it cost that benchmark nothing. But bench_method_call and
        // bench_closure_call lost 21% and 38%: their loops still call, so the
        // entry needs the callee present to inline it.
        //
        // Under lazy compilation every bytecode call dispatches through the
        // live runtime table, so nothing is needed either way. Closure
        // construction may also have queued a declaration merely to ask for
        // its ABI; compiling that body would rebuild a transitive mini-module
        // and, on a declined callee, leave invalid half-emitted IR behind.
        let wants_callees = !self.lazy_compilation
            && self
                .bytecode
                .functions
                .iter()
                .find(|f| f.findex == source.findex)
                .is_some_and(|raw| {
                    super::air::promotion_wants_full_module(&self.bytecode, raw, self.hot_reload)
                });
        if wants_callees {
            self.compile_pending_functions()?;
        } else {
            self.clear_pending_compilations();
        }
        if std::env::var_os("ASH_OSR_LOG").is_some() {
            eprintln!(
                "[osr] LLVM AIR callees ready findex={} pc={header_pc}",
                source.findex
            );
        }

        // Unscoped, unlike the promote path: `compile_osr_entry` swapped in a
        // module of its own, so everything here is new and needs the one run
        // it is about to get. `optimized_fns` belongs to the host module and
        // is deliberately left out of this — its entries do not name functions
        // in this module, and recording this module's into it would leave
        // dangling keys behind once the module is handed to the engine.
        {
            let _p = crate::profile::scope("llvm middle-end (osr)");
            // The callees duplicated above arrive as full bodies, traps and
            // all, and this module gets the same `default<O2>` the promote
            // path gets -- so it needs the same shield, or mem2reg promotes
            // allocas across a callee's setjmp and longjmp reverts them.
            // `func_cache` holds this module's own functions by now and
            // `findexes` was never swapped, so the shield reads the right
            // bodies.
            let excluded = self.shield_trap_functions_from_optimization();
            crate::profile::count("middle-end functions excluded (trap)", excluded as u64);
            super::module::run_middle_end(&self.module)?;
        }
        if std::env::var_os("ASH_OSR_LOG").is_some() {
            eprintln!(
                "[osr] LLVM AIR middle-end done findex={} pc={header_pc}",
                source.findex
            );
        }
        Ok(())
    }

    /// Load one typed AIR value from Cranelift's 64-bit de-SSA transfer slot.
    fn load_air_osr_slot(
        &self,
        buf: PointerValue<'ctx>,
        reg: u32,
        ty: BasicTypeEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let i64_ty = self.context.i64_type();
        let slot = unsafe {
            self.builder.build_gep(
                i64_ty,
                buf,
                &[i64_ty.const_int(u64::from(reg), false)],
                "air_osr_slot",
            )?
        };
        let raw = self
            .builder
            .build_load(i64_ty, slot, "air_osr_raw")?
            .into_int_value();
        Ok(match ty {
            BasicTypeEnum::IntType(t) if t.get_bit_width() < 64 => self
                .builder
                .build_int_truncate(raw, t, "air_osr_int")?
                .into(),
            BasicTypeEnum::IntType(_) => raw.into(),
            BasicTypeEnum::FloatType(t) if t == self.context.f64_type() => {
                self.builder.build_bit_cast(raw, t, "air_osr_f64")?
            }
            BasicTypeEnum::FloatType(t) => {
                let bits = self.builder.build_int_truncate(
                    raw,
                    self.context.i32_type(),
                    "air_osr_f32_bits",
                )?;
                self.builder.build_bit_cast(bits, t, "air_osr_f32")?
            }
            BasicTypeEnum::PointerType(t) => {
                self.builder.build_int_to_ptr(raw, t, "air_osr_ptr")?.into()
            }
            _ => return Err(anyhow!("unsupported AIR OSR slot type")),
        })
    }

    fn create_function_declaration(&mut self, f: &HLFunction) -> Result<FunctionValue<'ctx>> {
        let type_fun = self.bytecode.types[f.type_.0]
            .fun
            .clone()
            .expect("expect to get function type");
        let func_type = self.create_function_type(&type_fun)?;

        Ok(self.add_body_function(&f.name(), func_type))
    }

    fn load_function_arguments(
        &self,
        f: &HLFunction,
        function: &FunctionValue<'ctx>,
        registers: &[PointerValue<'ctx>],
    ) -> Result<()> {
        let fun_type = self.bytecode.types[f.type_.0]
            .fun
            .as_ref()
            .expect("expected function type");
        let args_count = fun_type.args.len();

        for i in 0..args_count {
            let param = function
                .get_nth_param(i as u32)
                .ok_or_else(|| anyhow!("Missing function parameter {}", i))?;
            self.builder.build_store(registers[i], param)?;
        }

        Ok(())
    }

    /// Keep one register in memory, where the collector can see what it holds.
    ///
    /// These start as `alloca`s on every target, and the middle end promotes
    /// the ones whose address never escapes -- into machine registers
    /// natively, which the mutator publishes at a safepoint, and into wasm
    /// LOCALS on WebAssembly, which live in the engine's frame storage and are
    /// not addressable by anything. An object whose only reference is there is
    /// collected while it is in use. Measured: with the middle end off the
    /// same program is correct, and with it on an array held by a live frame
    /// comes back holding another object's bytes.
    ///
    /// So the address is made to escape, once, and promotion cannot happen.
    /// A volatile store is what does it: the value stored is the slot's
    /// address, the store is to a word nothing reads, and volatile is what
    /// stops the optimiser removing a store whose result is unused. The slot
    /// then stays on the shadow stack, which is memory, which is scanned.
    ///
    /// Only pointer-typed registers. An integer register holds nothing the
    /// collector needs to find, and pinning it would pay the cost for
    /// nothing.
    fn pin_register(&mut self, slot: PointerValue<'ctx>) -> Result<()> {
        let sink = match self.module.get_global(GC_REGISTER_PIN) {
            Some(global) => global,
            None => {
                let ty = self.context.ptr_type(AddressSpace::default());
                let global = self.module.add_global(ty, None, GC_REGISTER_PIN);
                global.set_initializer(&ty.const_null());
                global
            }
        };
        let store = self.builder.build_store(sink.as_pointer_value(), slot)?;
        store
            .set_volatile(true)
            .map_err(|e| anyhow!("marking the register pin volatile: {e:?}"))?;

        // And null, because moving a register out of a wasm local moves it
        // out of something the specification zeroes. A local starts at zero;
        // a slot on the shadow stack starts holding whatever the last call to
        // use those bytes left there. A register read before it is written
        // was harmless when that read `undef` and folded away, and reads
        // `0xffffffff` from a stale frame once the slot is real memory --
        // which is a fault at the top of the address space, and which the
        // collector would otherwise have traced as a pointer.
        self.builder.build_store(
            slot,
            self.context.ptr_type(AddressSpace::default()).const_null(),
        )?;
        Ok(())
    }

    /// Whether the bodies being emitted keep the runtime's shadow call stack
    /// (`TargetAbi::shadow_call_stack`). Only an ahead-of-time target can:
    /// the JIT's frames are machine frames the runtime walks.
    fn shadow_frames(&self) -> bool {
        self.aot && self.target_abi.shadow_call_stack
    }

    /// Open the function's shadow frame: `hlp_shadow_push(findex)` returns
    /// the frame's position slot, and every `Pos` in the body stores into it.
    ///
    /// The slot is memory an external call handed back, so LLVM keeps every
    /// store that a later call or the return could observe and may drop only
    /// one that another position overwrites first -- exactly the ones no
    /// trace can see. It must NOT be marked `noalias`: dead-store elimination
    /// treats a non-escaping `noalias` allocation as private and would delete
    /// every position store as unread.
    fn emit_shadow_push(&self, findex: usize) -> Result<PointerValue<'ctx>> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let i32_type = self.context.i32_type();
        let push =
            self.declare_native("hlp_shadow_push", &[i32_type.into()], Some(ptr_type.into()));
        let slot = self
            .builder
            .build_call(
                push,
                &[i32_type.const_int(findex as u64, false).into()],
                "shadow_frame",
            )?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| anyhow!("hlp_shadow_push returned void"))?
            .into_pointer_value();
        Ok(slot)
    }

    /// Close the frame `emit_shadow_push` opened.
    fn emit_shadow_pop(&self) -> Result<()> {
        let pop = self.declare_native("hlp_shadow_pop", &[], None);
        self.builder.build_call(pop, &[], "")?;
        Ok(())
    }

    fn get_register_type(&mut self, type_index: usize) -> Result<BasicTypeEnum<'ctx>> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        Ok(match self.get_or_create_any_type(type_index)? {
            AnyTypeEnum::FloatType(t) => t.as_basic_type_enum(),
            AnyTypeEnum::IntType(t) => t.as_basic_type_enum(),
            AnyTypeEnum::PointerType(t) => t.as_basic_type_enum(),
            // Heap-allocated types are held as pointers in registers
            AnyTypeEnum::StructType(_)
            | AnyTypeEnum::ArrayType(_)
            | AnyTypeEnum::FunctionType(_)
            | AnyTypeEnum::VectorType(_)
            | AnyTypeEnum::ScalableVectorType(_)
            | AnyTypeEnum::VoidType(_) => ptr_type.as_basic_type_enum(),
        })
    }

    /// Cast a value to match a target function's expected parameter type.
    /// In HashLink, all values are passed as machine-word-sized values regardless of
    /// declared type. When the bytecode register type differs from the target function's
    /// parameter type (e.g., i32 register passed to a function expecting ptr/Dynamic),
    /// we cast to preserve the bit pattern — matching the C calling convention behavior.
    /// An integer the runtime or memory handed back, shaped for the slot of
    /// AIR value `dst`: a Bool slot holds `value != 0` -- any nonzero byte
    /// is true, as the interpreter, Cranelift and HashLink read it -- and a
    /// narrower or wider int slot takes the width-converted value.
    pub(super) fn int_for_slot(
        &self,
        value: IntValue<'ctx>,
        lowering: &HLFunction,
        reg_types: &[BasicTypeEnum<'ctx>],
        dst: ValueId,
    ) -> Result<BasicValueEnum<'ctx>> {
        let slot = reg_types[dst.idx()];
        if self.types_[lowering.regs[dst.idx()].0].kind == crate::hl::hl_type_kind_HBOOL {
            let truth = self.builder.build_int_compare(
                inkwell::IntPredicate::NE,
                value,
                value.get_type().const_zero(),
                "slot_bool",
            )?;
            return self.cast_for_call(truth.into(), slot);
        }
        self.cast_for_call(value.into(), slot)
    }

    fn cast_for_call(
        &self,
        value: BasicValueEnum<'ctx>,
        target: BasicTypeEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let i64_type = self.context.i64_type();

        match (value.get_type(), target) {
            // int → ptr: zero-extend to i64 then inttoptr
            (BasicTypeEnum::IntType(_), BasicTypeEnum::PointerType(_)) => {
                let int_val = value.into_int_value();
                let i64_val = if int_val.get_type().get_bit_width() < 64 {
                    self.builder.build_int_z_extend(int_val, i64_type, "zext")?
                } else {
                    int_val
                };
                Ok(self
                    .builder
                    .build_int_to_ptr(i64_val, ptr_type, "cast_itoptr")?
                    .into())
            }
            // ptr → int: ptrtoint then truncate if needed
            (BasicTypeEnum::PointerType(_), BasicTypeEnum::IntType(int_type)) => {
                let ptr_val = value.into_pointer_value();
                let i64_val = self
                    .builder
                    .build_ptr_to_int(ptr_val, i64_type, "cast_ptrtoi")?;
                if int_type.get_bit_width() < 64 {
                    Ok(self
                        .builder
                        .build_int_truncate(i64_val, int_type, "cast_trunc")?
                        .into())
                } else {
                    Ok(i64_val.into())
                }
            }
            // float → ptr: bitcast to i64, then inttoptr
            (BasicTypeEnum::FloatType(_), BasicTypeEnum::PointerType(_)) => {
                let float_val = value.into_float_value();
                let i64_val = self
                    .builder
                    .build_bit_cast(float_val, i64_type, "cast_ftoi64")?
                    .into_int_value();
                Ok(self
                    .builder
                    .build_int_to_ptr(i64_val, ptr_type, "cast_ftoptr")?
                    .into())
            }
            // ptr → float: ptrtoint then bitcast
            (BasicTypeEnum::PointerType(_), BasicTypeEnum::FloatType(float_type)) => {
                let ptr_val = value.into_pointer_value();
                let i64_val = self
                    .builder
                    .build_ptr_to_int(ptr_val, i64_type, "cast_ptrtoi")?;
                Ok(self
                    .builder
                    .build_bit_cast(i64_val, float_type, "cast_itof")?
                    .into())
            }
            // int widths differ: zext or trunc
            (BasicTypeEnum::IntType(from), BasicTypeEnum::IntType(to)) => {
                let int_val = value.into_int_value();
                if from.get_bit_width() < to.get_bit_width() {
                    Ok(self
                        .builder
                        .build_int_z_extend(int_val, to, "cast_zext")?
                        .into())
                } else if from.get_bit_width() > to.get_bit_width() {
                    Ok(self
                        .builder
                        .build_int_truncate(int_val, to, "cast_trunc")?
                        .into())
                } else {
                    Ok(value)
                }
            }
            // float widths differ: fptrunc or fpext. Without this the arm
            // below returned an f64 for an f32 target unchanged, and storing
            // that into a 4-byte slot wrote 8 bytes and read back the low
            // half -- see `audit_register_stores`.
            (BasicTypeEnum::FloatType(from), BasicTypeEnum::FloatType(to)) if from != to => {
                let fv = value.into_float_value();
                if from == self.context.f64_type() {
                    Ok(self
                        .builder
                        .build_float_trunc(fv, to, "cast_fptrunc")?
                        .into())
                } else {
                    Ok(self.builder.build_float_ext(fv, to, "cast_fpext")?.into())
                }
            }
            // Same or compatible types: no conversion
            _ => Ok(value),
        }
    }

    fn get_initialized_type(&mut self, type_index: usize) -> Result<BasicValueEnum<'ctx>> {
        if let Some(type_) = self.initialized_type_cache.get(&type_index) {
            return Ok(*type_);
        }
        let kind = self.types_[type_index].clone().kind;

        if self.aot {
            // `emit_aot_data` converted every type index before any body was
            // lowered, so the descriptor exists; what must not happen is the
            // fallback below, which would fabricate one in this process's heap
            // and bake its address into an object that runs elsewhere.
            // The canonical descriptor for the index. A scan of
            // `c_ptr_to_type_index` by value answers in hash order, and the
            // pick is baked into the object.
            let descriptor = self
                .type_index_to_c_ptr
                .get(&type_index)
                .map(|&p| p as *mut hl_type)
                .ok_or_else(|| anyhow!("type {type_index} has no emitted descriptor"))?;
            let value = self.aot_type_ptr(descriptor)?;
            self.initialized_type_cache.insert(type_index, value.into());
            return Ok(value.into());
        }

        // Function, nullable, reference, and packed descriptors contain a
        // type-specific pointer in their union. Reusing the descriptor built
        // by the C-type graph is essential: fabricating only the kind leaves
        // `hlp_safe_cast` with a null `fun`/`tparam` pointer.
        if let Some(c_type_ptr) = self
            .type_index_to_c_ptr
            .get(&type_index)
            .map(|&p| p as *mut hl_type)
        {
            let ptr_type = self.context.ptr_type(AddressSpace::default());
            let value = self
                .context
                .i64_type()
                .const_int(c_type_ptr as u64, false)
                .const_to_pointer(ptr_type);
            self.initialized_type_cache.insert(type_index, value.into());
            return Ok(value.into());
        }

        if matches!(
            kind,
            hl_type_kind_HFUN
                | hl_type_kind_HMETHOD
                | crate::hl::hl_type_kind_HPACKED
                | hl_type_kind_HNULL
                | crate::hl::hl_type_kind_HREF
        ) {
            let cache = std::rc::Rc::new(std::cell::RefCell::new(std::collections::HashMap::new()));
            let c_type_ptr =
                self.convert_type_ref_to_c_cached(&crate::types::TypeRef(type_index), cache)?;
            let ptr_type = self.context.ptr_type(AddressSpace::default());
            let value = self
                .context
                .i64_type()
                .const_int(c_type_ptr as u64, false)
                .const_to_pointer(ptr_type);
            self.initialized_type_cache.insert(type_index, value.into());
            return Ok(value.into());
        }

        // For primitive types (kind <= HDYN), create a real C-side hl_type and store its pointer
        // This matches what HOBJ/HSTRUCT/HENUM/HVIRTUAL already do in init_indexes
        let c_type_ptr = unsafe {
            Box::into_raw(Box::new(hl_type {
                kind,
                __bindgen_anon_1: std::mem::zeroed(),
                vobj_proto: std::ptr::null_mut(),
                mark_bits: std::ptr::null_mut(),
            }))
        };

        let ptr_as_int = self.context.i64_type().const_int(c_type_ptr as u64, false);
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let ptr_to_type = ptr_as_int.const_to_pointer(ptr_type);

        self.initialized_type_cache
            .insert(type_index, ptr_to_type.into());

        Ok(ptr_to_type.into())
    }

    fn declare_native_function(
        &mut self,
        lib: &str,
        name: &str,
        native_func: &HLNative,
    ) -> Result<FunctionValue<'ctx>> {
        let type_fun = self.bytecode.types[native_func.type_.0]
            .fun
            .clone()
            .expect("expected to get function type");
        let func_type = self.create_function_type(&type_fun)?;
        let f_v = self.module.add_function(name, func_type, None);
        self.stamp_host_cpu(f_v);
        Ok(f_v)
    }

    /// Generate a caller function that embeds the native function's address directly
    /// as an inttoptr constant, avoiding reliance on add_global_mapping symbol resolution.
    fn generate_native_caller_with_addr(
        &self,
        name: &str,
        fn_type: FunctionType<'ctx>,
        func_addr: usize,
    ) -> Result<FunctionValue<'ctx>> {
        let saved_block = self.builder.get_insert_block();

        let function = self.module.add_function(name, fn_type, None);
        self.stamp_host_cpu(function);
        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);

        // Embed the function address directly as inttoptr constant
        self.reject_in_aot("a native caller thunk")?;
        let addr_int = self.context.i64_type().const_int(func_addr as u64, false);
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let func_ptr = self.builder.build_int_to_ptr(addr_int, ptr_type, "fptr")?;

        let args: Vec<BasicMetadataValueEnum> =
            function.get_param_iter().map(|arg| arg.into()).collect();

        let call_site = self
            .builder
            .build_indirect_call(fn_type, func_ptr, &args, "call")?;

        if let Some(result) = call_site.try_as_basic_value().basic() {
            self.builder.build_return(Some(&result))?;
        } else {
            self.builder.build_return(None)?;
        }

        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }

        Ok(function)
    }

    /// Float to integer with a defined result for every input.
    ///
    /// LLVM's `fptosi` is poison when the value does not fit, and O3 is
    /// entitled to reason backwards from that. `Std.int(f) == f` is how
    /// `haxe.format.JsonParser.parseNumber` and `Std.isOfType(_, Int)` ask
    /// whether a Float is integral; assuming the conversion was in range, the
    /// optimizer folded it to `true` for 1e10, and `Json.parse("10000000000")`
    /// came back as the Int -2147483648 (TestJson, TestReflect). It only
    /// surfaced once boxing slots moved to the entry block -- a body with a
    /// dynamic alloca is never inlined, and the inlined shape is what the
    /// fold needed -- but the poison was there all along.
    ///
    /// The saturating intrinsic clamps and maps NaN to zero: exactly what
    /// the interpreter's Rust `as` does, so both engines agree by
    /// construction. It is the one `fcvtzs` HashLink's own JIT emits on
    /// aarch64 and a `cvttsd2si` plus fixups on x86-64. (x86-64 HashLink
    /// answers INT_MIN for every out-of-range value; no suite depends on
    /// that, and the interpreter never did it either.) The math intrinsics
    /// went saturating for the same reason -- see `emit_native_intrinsic`.
    fn build_float_to_int_saturating(
        &self,
        x: inkwell::values::FloatValue<'ctx>,
        dst: inkwell::types::IntType<'ctx>,
        name: &str,
    ) -> Result<inkwell::values::IntValue<'ctx>> {
        use inkwell::intrinsics::Intrinsic;
        let decl = Intrinsic::find("llvm.fptosi.sat")
            .and_then(|sat| sat.get_declaration(&self.module, &[dst.into(), x.get_type().into()]));
        let Some(decl) = decl else {
            // Every LLVM this crate builds against has the intrinsic; the
            // plain conversion is kept only so a missing declaration reads
            // as the old behaviour rather than a refused body.
            return Ok(self.builder.build_float_to_signed_int(x, dst, name)?);
        };
        let call = self.builder.build_call(decl, &[x.into()], name)?;
        Ok(call
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| anyhow!("llvm.fptosi.sat returned void"))?
            .into_int_value())
    }

    /// A scratch slot for one site, allocated in the function's entry block.
    ///
    /// An `alloca` anywhere else is a DYNAMIC allocation: LLVM moves the
    /// stack pointer every time control passes it, and nothing gives the
    /// space back before the function returns. Every boxing site used to
    /// allocate at its own position, so a loop that boxed a primitive per
    /// iteration -- `waitLock.wait(0.0)` in `sys.thread.EventLoop.loop`, a
    /// `Null<Float>` argument -- grew the stack by a slot per pass, and the
    /// event loops of the threads and eventLoop suites overflowed the main
    /// thread's 8 MB in about a second on Linux, SEVEN frames deep: gdb showed
    /// `loop`'s frame spanning the whole stack. macOS ran the same code and
    /// merely spun fewer times before the loop ended. `mem2reg` cannot
    /// rescue such a slot either: its address is handed to a native.
    ///
    /// An entry-block alloca is static -- one slot per site per activation,
    /// reserved in the prologue -- and that is enough here, because every
    /// caller passes the slot to a runtime helper that copies out of it
    /// before returning (`hlp_make_dyn`, `hlp_dyn_castp`, the stub bridge)
    /// and keeps nothing past the call. A second builder does the placing so
    /// the emitting builder's insertion point is never disturbed.
    fn entry_alloca<T: BasicType<'ctx>>(&self, ty: T, name: &str) -> Result<PointerValue<'ctx>> {
        let function = self
            .builder
            .get_insert_block()
            .and_then(|block| block.get_parent())
            .ok_or_else(|| anyhow!("entry_alloca: builder is not inside a function"))?;
        let entry = function
            .get_first_basic_block()
            .ok_or_else(|| anyhow!("entry_alloca: function has no entry block"))?;
        let at_entry = self.context.create_builder();
        match entry.get_first_instruction() {
            Some(first) => at_entry.position_before(&first),
            None => at_entry.position_at_end(entry),
        }
        Ok(at_entry.build_alloca(ty, name)?)
    }

    /// Emit an indirect call guarded against interpreter stub sentinels.
    ///
    /// In hybrid mode, shared function-pointer slots (functions_ptrs, vtables,
    /// closure `fun` fields) may still hold the interpreter's stub sentinel
    /// (findex + 1, always < 0x100000). Calling one from native code is the
    /// deterministic SIGBUS observed on game.hl right after tier promotion.
    /// This wraps every JIT indirect call site: pointers below the sentinel
    /// limit are routed to `ash_jit_call_stub`, which re-enters the
    /// interpreter for that findex with the same arguments (raw i64 word
    /// encoding; see jit/stub_bridge.rs for the contract).
    ///
    /// Returns the merged call result (None for void returns).
    fn build_stub_guarded_indirect_call(
        &self,
        fn_type: FunctionType<'ctx>,
        fn_ptr: PointerValue<'ctx>,
        args: &[BasicMetadataValueEnum<'ctx>],
        name: &str,
    ) -> Result<Option<BasicValueEnum<'ctx>>> {
        if self.aot {
            // The guard exists to catch an interpreter stub sentinel reaching
            // compiled code. An AOT binary has no interpreter to fall back
            // to and no sentinels to catch: `ash_functions` holds emitted
            // symbols or null, and a null is a body the compiler refused --
            // faulting on it is the honest outcome, where routing it into a
            // bridge that is not in the binary is not. Emitting the guard
            // would also bake this compiler's own addresses into the object.
            let call = self
                .builder
                .build_indirect_call(fn_type, fn_ptr, args, name)?;
            return Ok(call.try_as_basic_value().basic());
        }

        let i64_type = self.context.i64_type();
        let i32_type = self.context.i32_type();
        let f64_type = self.context.f64_type();
        let f32_type = self.context.f32_type();
        let ptr_type = self.context.ptr_type(AddressSpace::default());

        let function = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();

        let addr = self
            .builder
            .build_ptr_to_int(fn_ptr, i64_type, &format!("{}_addr", name))?;
        // Null also takes the stub path: the bridge fails it gracefully
        // (findex -1 lookup miss) instead of a null-call crash.
        let is_stub = self.builder.build_int_compare(
            IntPredicate::ULT,
            addr,
            i64_type.const_int(crate::llvm::stub_bridge::STUB_SENTINEL_LIMIT, false),
            &format!("{}_is_stub", name),
        )?;

        let direct_bb = self
            .context
            .append_basic_block(function, &format!("{}_direct", name));
        let stub_bb = self
            .context
            .append_basic_block(function, &format!("{}_stub", name));
        let heal_bb = self
            .context
            .append_basic_block(function, &format!("{}_heal", name));
        let bridge_bb = self
            .context
            .append_basic_block(function, &format!("{}_bridge", name));
        let resolved_bb = self
            .context
            .append_basic_block(function, &format!("{}_resolved", name));
        let interpreter_bb = self
            .context
            .append_basic_block(function, &format!("{}_interp", name));
        let merge_bb = self
            .context
            .append_basic_block(function, &format!("{}_merge", name));
        self.builder
            .build_conditional_branch(is_stub, stub_bb, direct_bb)?;

        // --- Direct path: a real code pointer, call it as before ---
        self.builder.position_at_end(direct_bb);
        let direct = self
            .builder
            .build_indirect_call(fn_type, fn_ptr, args, name)?;
        let direct_val = direct.try_as_basic_value().basic();
        self.builder.build_unconditional_branch(merge_bb)?;

        // --- Stub probe: has the sentinel's findex been promoted since this
        // pointer was captured? ---
        //
        // A sentinel encodes `findex + 1`, captured by value — into a
        // closure's `fun` field, a vtable row, a stored function pointer —
        // at a time when the findex was interpreted. Promotion updates
        // `functions_ptrs[findex]`, not the captures, so a hot loop calling
        // through an old capture paid the full bridge (a malloc, an
        // interpreter re-entry and a marshal) for every call: 100M closure
        // calls spent 63.9% of the run in `call_function` and 8% in the
        // bridge's malloc/free. One load from the (hot, cached) slot turns
        // all of those into direct calls.
        self.builder.position_at_end(stub_bb);
        let ptrs_base = self
            .shared_runtime
            .as_ref()
            .filter(|sh| !sh.module_ctx.is_null())
            .map(|sh| unsafe { (*sh.module_ctx).functions_ptrs })
            .filter(|p| !p.is_null());
        let healed: Option<inkwell::values::BasicValueEnum> = match ptrs_base {
            Some(base) => {
                let zero = i64_type.const_zero();
                let is_null = self.builder.build_int_compare(
                    IntPredicate::EQ,
                    addr,
                    zero,
                    &format!("{}_is_null", name),
                )?;
                // Null probes slot 0 harmlessly instead of slot -1.
                let fx_raw = self.builder.build_int_sub(
                    addr,
                    i64_type.const_int(1, false),
                    &format!("{}_fx_raw", name),
                )?;
                let fx = self
                    .builder
                    .build_select(is_null, zero, fx_raw, &format!("{}_fx", name))?
                    .into_int_value();
                let base_ptr = i64_type
                    .const_int(base as usize as u64, false)
                    .const_to_pointer(ptr_type);
                let slot_gep = unsafe {
                    self.builder.build_gep(
                        ptr_type,
                        base_ptr,
                        &[fx],
                        &format!("{}_slot_gep", name),
                    )?
                };
                let slot = self
                    .builder
                    .build_load(ptr_type, slot_gep, &format!("{}_slot", name))?
                    .into_pointer_value();
                let slot_addr = self.builder.build_ptr_to_int(
                    slot,
                    i64_type,
                    &format!("{}_slot_addr", name),
                )?;
                let slot_real = self.builder.build_int_compare(
                    IntPredicate::UGE,
                    slot_addr,
                    i64_type.const_int(crate::llvm::stub_bridge::STUB_SENTINEL_LIMIT, false),
                    &format!("{}_slot_real", name),
                )?;
                let not_null = self.builder.build_not(is_null, &format!("{}_nn", name))?;
                let can_heal =
                    self.builder
                        .build_and(slot_real, not_null, &format!("{}_can_heal", name))?;
                self.builder
                    .build_conditional_branch(can_heal, heal_bb, bridge_bb)?;

                self.builder.position_at_end(heal_bb);
                let call = self.builder.build_indirect_call(
                    fn_type,
                    slot,
                    args,
                    &format!("{}_healed", name),
                )?;
                let v = call.try_as_basic_value().basic();
                self.builder.build_unconditional_branch(merge_bb)?;
                v
            }
            None => {
                // No runtime handles (whole-module JIT: nothing is ever a
                // sentinel there anyway). Keep the single-path shape. The
                // heal block still needs a terminator to satisfy the
                // verifier, even with zero predecessors.
                self.builder.build_unconditional_branch(bridge_bb)?;
                self.builder.position_at_end(heal_bb);
                self.builder.build_unreachable()?;
                None
            }
        };

        // --- Lazy compiled-only path: resolve one AIR V2 body and call it
        // using the exact typed signature already present at this site. ---
        self.builder.position_at_end(bridge_bb);
        let resolver_type = i64_type.fn_type(&[i64_type.into()], false);
        let resolver_ptr = i64_type
            .const_int(
                crate::llvm::stub_bridge::ash_jit_resolve_stub as usize as u64,
                false,
            )
            .const_to_pointer(ptr_type);
        let resolved_addr = self
            .builder
            .build_indirect_call(
                resolver_type,
                resolver_ptr,
                &[addr.into()],
                &format!("{}_resolve", name),
            )?
            .try_as_basic_value()
            .basic()
            .unwrap()
            .into_int_value();
        let resolved_real = self.builder.build_int_compare(
            IntPredicate::UGE,
            resolved_addr,
            i64_type.const_int(crate::llvm::stub_bridge::STUB_SENTINEL_LIMIT, false),
            &format!("{}_resolved_real", name),
        )?;
        self.builder
            .build_conditional_branch(resolved_real, resolved_bb, interpreter_bb)?;

        self.builder.position_at_end(resolved_bb);
        let resolved_ptr = self.builder.build_int_to_ptr(
            resolved_addr,
            ptr_type,
            &format!("{}_resolved_ptr", name),
        )?;
        let resolved_call = self.builder.build_indirect_call(
            fn_type,
            resolved_ptr,
            args,
            &format!("{}_resolved_call", name),
        )?;
        let resolved_val = resolved_call.try_as_basic_value().basic();
        self.builder.build_unconditional_branch(merge_bb)?;

        // --- Hybrid fallback: spill raw words and re-enter the interpreter. ---
        self.builder.position_at_end(interpreter_bb);
        let nargs = args.len() as u32;
        let buf = self.entry_alloca(
            i64_type.array_type(nargs.max(1)),
            &format!("{}_argbuf", name),
        )?;
        for (i, arg) in args.iter().enumerate() {
            let val = BasicValueEnum::try_from(*arg)
                .map_err(|_| anyhow!("non-basic argument in stub-guarded call"))?;
            let word = match val {
                BasicValueEnum::IntValue(iv) => {
                    if iv.get_type().get_bit_width() < 64 {
                        self.builder
                            .build_int_z_extend(iv, i64_type, "stub_arg_zext")?
                    } else {
                        iv
                    }
                }
                BasicValueEnum::FloatValue(fv) => {
                    let as_f64 = if fv.get_type() == f32_type {
                        self.builder.build_float_ext(fv, f64_type, "stub_arg_ext")?
                    } else {
                        fv
                    };
                    self.builder
                        .build_bit_cast(as_f64, i64_type, "stub_arg_bits")?
                        .into_int_value()
                }
                BasicValueEnum::PointerValue(pv) => {
                    self.builder
                        .build_ptr_to_int(pv, i64_type, "stub_arg_ptr")?
                }
                other => {
                    return Err(anyhow!(
                        "unsupported argument value {:?} in stub-guarded call",
                        other
                    ))
                }
            };
            let slot = unsafe {
                self.builder.build_gep(
                    i64_type,
                    buf,
                    &[i64_type.const_int(i as u64, false)],
                    "stub_arg_slot",
                )?
            };
            self.builder.build_store(slot, word)?;
        }

        let stub_fn_type = i64_type.fn_type(
            &[
                i64_type.into(),
                i32_type.into(),
                ptr_type.into(),
                i32_type.into(),
            ],
            false,
        );
        let stub_fn_ptr = i64_type
            .const_int(
                crate::llvm::stub_bridge::ash_jit_call_stub as usize as u64,
                false,
            )
            .const_to_pointer(ptr_type);
        let raw = self
            .builder
            .build_indirect_call(
                stub_fn_type,
                stub_fn_ptr,
                &[
                    addr.into(),
                    i32_type.const_int(self.current_findex as u64, false).into(),
                    buf.into(),
                    i32_type.const_int(nargs as u64, false).into(),
                ],
                &format!("{}_stub_call", name),
            )?
            .try_as_basic_value()
            .basic()
            .unwrap()
            .into_int_value();

        // Decode the raw word back into the call's return type.
        let stub_val: Option<BasicValueEnum> = match fn_type.get_return_type() {
            None => None,
            Some(BasicTypeEnum::IntType(t)) => Some(if t.get_bit_width() < 64 {
                self.builder
                    .build_int_truncate(raw, t, "stub_ret_trunc")?
                    .into()
            } else {
                raw.into()
            }),
            Some(BasicTypeEnum::FloatType(t)) => {
                let as_f64 = self
                    .builder
                    .build_bit_cast(raw, f64_type, "stub_ret_bits")?
                    .into_float_value();
                Some(if t == f32_type {
                    self.builder
                        .build_float_trunc(as_f64, f32_type, "stub_ret_f32")?
                        .into()
                } else {
                    as_f64.into()
                })
            }
            Some(BasicTypeEnum::PointerType(t)) => Some(
                self.builder
                    .build_int_to_ptr(raw, t, "stub_ret_ptr")?
                    .into(),
            ),
            Some(other) => {
                return Err(anyhow!(
                    "unsupported return type {:?} in stub-guarded call",
                    other
                ))
            }
        };
        self.builder.build_unconditional_branch(merge_bb)?;

        // --- Merge ---
        self.builder.position_at_end(merge_bb);
        match (direct_val, resolved_val, stub_val) {
            (Some(d), Some(r), Some(s)) => {
                let phi = self
                    .builder
                    .build_phi(d.get_type(), &format!("{}_result", name))?;
                phi.add_incoming(&[(&d, direct_bb), (&r, resolved_bb), (&s, interpreter_bb)]);
                if let Some(h) = healed {
                    phi.add_incoming(&[(&h, heal_bb)]);
                }
                Ok(Some(phi.as_basic_value()))
            }
            _ => Ok(None),
        }
    }

    /// Address of the live runtime function-pointer slot for `findex`.
    ///
    /// Per-function modules must not bake the address of their private
    /// snapshot vector: Cranelift and later LLVM installs update the shared
    /// `hl_module_context`. Loading this slot lets compiled callers and
    /// closures observe tier changes without being rebuilt.
    /// Give every unterminated block in the module a terminator that reports
    /// rather than one that guesses.
    ///
    /// Only a body whose lowering was abandoned has such a block, so this is
    /// a no-op for every function that compiled. The sealed body traps
    /// through `hlp_error`: a caller that reaches it says which function was
    /// refused, where a bare `unreachable` would let the optimizer conclude
    /// the call never happens and delete the code around it.
    fn seal_partial_bodies(&mut self) -> Result<()> {
        let saved = self.builder.get_insert_block();
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let error_type = self.context.void_type().fn_type(&[ptr_type.into()], true);

        let unsealed: Vec<(
            FunctionValue<'ctx>,
            Vec<inkwell::basic_block::BasicBlock<'ctx>>,
        )> = self
            .module
            .get_functions()
            .filter_map(|function| {
                let open: Vec<_> = function
                    .get_basic_blocks()
                    .into_iter()
                    .filter(|block| block.get_terminator().is_none())
                    .collect();
                (!open.is_empty()).then_some((function, open))
            })
            .collect();

        for (function, blocks) in unsealed {
            let name = function.get_name().to_string_lossy().into_owned();
            let message = self.utf16_message(&format!("Refused at compile time: {name}"))?;
            let error = self.error_function_ptr()?;
            for block in blocks {
                self.builder.position_at_end(block);
                self.builder
                    .build_indirect_call(error_type, error, &[message.into()], "")?;
                self.builder.build_unreachable()?;
            }
        }

        if let Some(block) = saved {
            self.builder.position_at_end(block);
        }
        Ok(())
    }

    /// `hlp_error`, as an address under the JIT and as a symbol under AOT.
    fn error_function_ptr(&self) -> Result<PointerValue<'ctx>> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        if self.aot {
            let signature = self.context.void_type().fn_type(&[ptr_type.into()], true);
            return Ok(self
                .aot_runtime_fn("hlp_error", signature)
                .as_global_value()
                .as_pointer_value());
        }
        let address = self
            .native_function_resolver
            .resolve_function("std", "hlp_error")
            .map_err(|e| anyhow!("cannot seal a refused body (no hlp_error): {}", e))?
            as u64;
        Ok(self
            .context
            .i64_type()
            .const_int(address, false)
            .const_to_pointer(ptr_type))
    }

    /// A NUL-terminated UTF-16 message the emitted code can hand to
    /// `hlp_error`: object data under AOT, a leaked buffer under the JIT.
    pub(crate) fn utf16_message(&self, message: &str) -> Result<PointerValue<'ctx>> {
        let text: Vec<u16> = message.encode_utf16().chain(std::iter::once(0)).collect();
        if self.aot {
            let bytes: Vec<u8> = text.iter().flat_map(|unit| unit.to_le_bytes()).collect();
            let global = self.module.add_global(
                self.context.i8_type().array_type(bytes.len() as u32),
                None,
                "ash_message",
            );
            global.set_initializer(&self.context.const_string(&bytes, false));
            global.set_linkage(inkwell::module::Linkage::Internal);
            global.set_constant(true);
            global.set_alignment(2);
            return Ok(global.as_pointer_value());
        }
        let address = Box::leak(text.into_boxed_slice()).as_ptr() as u64;
        Ok(self
            .context
            .i64_type()
            .const_int(address, false)
            .const_to_pointer(self.context.ptr_type(AddressSpace::default())))
    }

    /// `setjmp`, as an address under the JIT and as a symbol under AOT.
    ///
    /// The C function is spelled `_setjmp`, and the IR name `_setjmp` reaches
    /// it on both object formats that matter here: Mach-O prepends an
    /// underscore, giving `__setjmp`, which is what libSystem exports, and
    /// ELF does not, giving `_setjmp`, which is what libc exports.
    /// Whether the emitted call passes Win64's frame argument.
    fn emits_setjmp_frame(&self) -> bool {
        self.target_abi.setjmp_takes_frame
    }

    /// `_setjmp`'s type for this target.
    ///
    /// Win64 spells it `_setjmp(env, frame)` and its `longjmp` reads that
    /// frame to decide whether to unwind with SEH. Everywhere else it takes
    /// the buffer alone.
    fn setjmp_signature(&self) -> inkwell::types::FunctionType<'ctx> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        if self.emits_setjmp_frame() {
            self.context
                .i32_type()
                .fn_type(&[ptr_type.into(), ptr_type.into()], false)
        } else {
            self.context.i32_type().fn_type(&[ptr_type.into()], false)
        }
    }

    /// Arm `buf` and answer 0 on the way in, non-zero when a throw lands here.
    ///
    /// The null frame is Win64's own spelling for "do not unwind": ash
    /// abandons the frames between the trap and the throw on purpose, having
    /// restored the GC lock depth and the shadow stack itself.
    fn build_setjmp_call(
        &self,
        buf: PointerValue<'ctx>,
        name: &str,
    ) -> Result<inkwell::values::IntValue<'ctx>> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let setjmp_ptr = self.setjmp_ptr()?;
        let args: Vec<inkwell::values::BasicMetadataValueEnum<'ctx>> =
            if self.emits_setjmp_frame() {
                vec![buf.into(), ptr_type.const_null().into()]
            } else {
                vec![buf.into()]
            };
        let call =
            self.builder
                .build_indirect_call(self.setjmp_signature(), setjmp_ptr, &args, name)?;
        // Without this every pass that asks "does this call return twice"
        // answers no, and a value live across the jump ends up in a register
        // the longjmp path never restores.
        let returns_twice = self.context.create_enum_attribute(
            inkwell::attributes::Attribute::get_named_enum_kind_id("returns_twice"),
            0,
        );
        call.add_attribute(inkwell::attributes::AttributeLoc::Function, returns_twice);
        Ok(call
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| anyhow!("_setjmp returned void"))?
            .into_int_value())
    }

    fn setjmp_ptr(&self) -> Result<PointerValue<'ctx>> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        if self.aot {
            const SYMBOL: &str = "_setjmp";
            let signature = self.setjmp_signature();
            let function = self.module.get_function(SYMBOL).unwrap_or_else(|| {
                let declared =
                    self.module
                        .add_function(SYMBOL, signature, Some(inkwell::module::Linkage::External));
                // The same attribute a C header gives `setjmp`. Call sites
                // carry it too, but the declaration is what makes every pass
                // that asks "does this function call something that returns
                // twice" answer yes -- which is what keeps a value that is
                // live across the jump in memory instead of in a register
                // the longjmp path never restores.
                declared.add_attribute(
                    inkwell::attributes::AttributeLoc::Function,
                    self.context.create_enum_attribute(
                        inkwell::attributes::Attribute::get_named_enum_kind_id("returns_twice"),
                        0,
                    ),
                );
                declared
            });
            return Ok(function.as_global_value().as_pointer_value());
        }
        Ok(self
            .context
            .i64_type()
            .const_int(crate::hl::_setjmp as usize as u64, false)
            .const_to_pointer(ptr_type))
    }

    /// Under `ASH_NO_PURE_CSE`, stop LLVM from proving a body effect-free.
    ///
    /// This exists for one measurement, and it is worth saying exactly which.
    /// `fib` is a call benchmark that a good compiler does not run: LLVM
    /// infers `memory(none)`, and once AIR's recursive inliner has exposed two
    /// calls with the same argument, GVN collapses the tree. The reported
    /// number is then optimizer visibility, not call cost -- clang and gcc do
    /// the same to a C `fib`.
    ///
    /// The honest row needs the CSE suppressed and nothing else. It would be
    /// easy and wrong to reach for `ASH_AIR_NO_INLINE` instead: inlining is a
    /// separate optimization that drops the recurrence base on its own, and
    /// turning it off would UNDERSTATE call performance rather than isolate
    /// the elimination. An empty side-effecting asm is inert -- it emits no
    /// instruction -- but a function containing one cannot be `memory(none)`,
    /// so identical calls stop being redundant while every other pass,
    /// inlining included, carries on unchanged.
    ///
    /// Never on by default. A build that sets it is measuring, not shipping.
    fn emit_purity_barrier(&self) -> Result<()> {
        static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
        if !*ENABLED.get_or_init(|| std::env::var_os("ASH_NO_PURE_CSE").is_some()) {
            return Ok(());
        }
        let barrier = self.context.void_type().fn_type(&[], false);
        let asm = self.context.create_inline_asm(
            barrier,
            String::new(),
            String::new(),
            true,  // has side effects
            false, // not align-stack
            None,
            false,
        );
        self.builder
            .build_indirect_call(barrier, asm, &[], "ash_purity_barrier")?;
        Ok(())
    }

    /// The concrete type's vtable, without a runtime call per dispatch.
    ///
    /// `hl_get_obj_proto` fills `vobj_proto` on first use and returns the
    /// cached table on every call after that -- so all but the first call per
    /// type does nothing except cost a call. It was emitted unconditionally
    /// on every dispatch, which put `callq hl_get_obj_proto` inside
    /// bench_method_call's 100M-iteration loop, immediately followed by the
    /// load of `t->vobj_proto` it had just guaranteed. Being opaque, it also
    /// barred the optimizer from moving anything across it, so the cost was
    /// larger than the call itself.
    ///
    /// The same guard hoisted to the call site is a load and a branch that
    /// predicts perfectly after the first dispatch, and the slow path runs
    /// once per type for the life of the process. `layout.rs` already does
    /// this for field ACCESS -- it computes offsets at compile time rather
    /// than calling `hlp_get_obj_rt` -- but a vtable pointer has no such
    /// oracle, because the table is built at run time.
    /// Resolve a profile's `Class.method` back to a findex in THIS bytecode.
    /// Built once. A name that no longer exists resolves to nothing, which
    /// costs the guard and never correctness.
    fn findex_for_name(&mut self, name: &str) -> Option<u32> {
        self.ensure_name_map();
        self.name_to_findex.as_ref().unwrap().get(name).copied()
    }

    /// `Class.method` for a findex, or `None` for a closure or the entrypoint.
    pub(crate) fn function_name(&mut self, findex: u32) -> Option<String> {
        self.ensure_name_map();
        self.findex_to_name.as_ref().unwrap().get(&findex).cloned()
    }

    fn ensure_name_map(&mut self) {
        if self.name_to_findex.is_some() {
            return;
        }
        let by_findex = crate::types::function_keys(&self.bytecode.types, &self.bytecode.functions);
        let mut by_name = std::collections::HashMap::new();
        for (fx, n) in &by_findex {
            by_name.entry(n.clone()).or_insert(*fx);
        }
        self.name_to_findex = Some(by_name);
        self.findex_to_name = Some(by_findex);
    }

    fn vobj_proto_ptr(&mut self, type_ptr: PointerValue<'ctx>) -> Result<PointerValue<'ctx>> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let function = self
            .builder
            .get_insert_block()
            .and_then(|block| block.get_parent())
            .ok_or_else(|| anyhow!("vobj_proto_ptr outside a function"))?;

        let slot = unsafe {
            self.builder.build_gep(
                self.context.i8_type(),
                type_ptr,
                &[self
                    .context
                    .i64_type()
                    .const_int(self.target_abi.hl_type_vobj_proto_offset(), false)],
                "vobj_proto_gep",
            )?
        };
        let cached = self
            .builder
            .build_load(ptr_type, slot, "vobj_proto_cached")?
            .into_pointer_value();
        let missing = self.builder.build_is_null(cached, "vobj_proto_missing")?;

        let init_bb = self.context.append_basic_block(function, "vobj_proto_init");
        let done_bb = self.context.append_basic_block(function, "vobj_proto_done");
        let cached_bb = self
            .builder
            .get_insert_block()
            .ok_or_else(|| anyhow!("vobj_proto_ptr lost its block"))?;
        self.builder
            .build_conditional_branch(missing, init_bb, done_bb)?;

        self.builder.position_at_end(init_bb);
        let get_obj_proto = self.declare_native(
            "hl_get_obj_proto",
            &[ptr_type.into()],
            Some(ptr_type.into()),
        );
        self.builder
            .build_call(get_obj_proto, &[type_ptr.into()], "init_obj_proto")?;
        let filled = self
            .builder
            .build_load(ptr_type, slot, "vobj_proto_filled")?
            .into_pointer_value();
        let init_end = self
            .builder
            .get_insert_block()
            .ok_or_else(|| anyhow!("vobj_proto_ptr lost its init block"))?;
        self.builder.build_unconditional_branch(done_bb)?;

        self.builder.position_at_end(done_bb);
        let merged = self.builder.build_phi(ptr_type, "vobj_proto")?;
        merged.add_incoming(&[(&cached, cached_bb), (&filled, init_end)]);
        Ok(merged.as_basic_value().into_pointer_value())
    }

    /// An unbound closure as object data, rather than an allocation.
    ///
    /// A `StaticClosure` over a known function captures nothing: its `t` and
    /// `fun` are compile-time constants and its `hasValue` is zero. Allocating
    /// one at run time hides all three behind an opaque call, and the call
    /// site then cannot fold anything it reads back -- so bench_closure_call
    /// re-derived the whole closure (read `hasValue`, test for a wrapper,
    /// unwrap, read `fun`, compare the type) on each of 100M iterations, for
    /// an object that never changed.
    ///
    /// Emitting it as a global instead makes every one of those a constant,
    /// and the decode folds. This is what HL/C does -- `static vclosure cl$0 =
    /// { &type, fn, 0 }` -- and measuring its output settled that the gap was
    /// never the optimizer: the same C is 0.09s under gcc and 0.11s under
    /// clang, against 0.22s for what we were emitting through LLVM.
    ///
    /// Static storage is sound here because a `vclosure` is written once, by
    /// whatever builds it, and never again -- `stackCount` included -- and an
    /// unbound one holds a type pointer, a code pointer and a null. Nothing
    /// GC-owned, so it never needs scanning or collecting.
    ///
    /// One global per lowered site, which is the identity HL/C gives: the same
    /// site evaluated twice yields the same closure, where allocating gave two.
    /// AOT only -- a JIT cannot bake `fun`, because the callee may not be
    /// compiled yet when its address is needed.
    fn emit_static_closure(
        &mut self,
        findex: usize,
        type_ptr: PointerValue<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let i32_type = self.context.i32_type();
        let (target, _) = self.get_or_create_function_value(findex)?;

        let mut fields: Vec<BasicValueEnum<'ctx>> = vec![
            type_ptr.into(),
            target.as_global_value().as_pointer_value().into(),
            i32_type.const_zero().into(), // hasValue: unbound
        ];
        if self.target_abi.pointer_bytes() == 8 {
            fields.push(i32_type.const_zero().into()); // HL_64 stackCount
        }
        fields.push(ptr_type.const_null().into()); // value
        let value = self.context.const_struct(&fields, false);
        let global = self
            .module
            .add_global(value.get_type(), None, "ash_closure");
        global.set_initializer(&value);
        global.set_linkage(inkwell::module::Linkage::Internal);
        // Constant, and that is the half that makes it pay: without it LLVM
        // must assume something writes the closure, so `hasValue` stays a
        // load and the wrapper test survives. Every `vclosure` field --
        // `stackCount` included -- is written by whatever constructs the
        // object and never afterwards, so an emitted one is genuinely
        // read-only for its whole life.
        global.set_constant(true);
        global.set_alignment(self.target_abi.pointer_align());
        Ok(global.as_pointer_value().into())
    }

    /// Refuse to bake an address of this process into an object file.
    ///
    /// These are the sites that still resolve a pointer at compile time and
    /// have no symbol to name it by. Under the JIT they are correct; under
    /// AOT the address means nothing in the process that runs, and the
    /// failure would be an object that links, runs, and reads whatever
    /// happens to live there. Refusing the function instead leaves it out of
    /// `ash_functions`, where a call through it faults immediately and the
    /// compile reports which construct it could not lower.
    fn reject_in_aot(&self, what: &str) -> Result<()> {
        if self.aot {
            return Err(anyhow!(
                "cannot emit {what} ahead of time: it has no symbol, only an address in this process"
            ));
        }
        Ok(())
    }

    /// The address of `functions_ptrs[findex]`, as a value the emitted code
    /// can use.
    ///
    /// Under the JIT that is this process's address for the slot. Under AOT
    /// it is a constant offset into the emitted `ash_functions` table, since
    /// nothing about this process's heap survives into the object file.
    fn function_slot_ptr(&self, findex: usize) -> Result<PointerValue<'ctx>> {
        if self.aot {
            return self.aot_function_slot(findex);
        }
        let address = self.function_slot_address(findex)?;
        Ok(self
            .context
            .i64_type()
            .const_int(address, false)
            .const_to_pointer(self.context.ptr_type(AddressSpace::default())))
    }

    /// The per-findex `HFUN` descriptor, likewise as an address under the JIT
    /// and as emitted object data under AOT.
    fn func_type_ptr(&mut self, findex: usize) -> Result<PointerValue<'ctx>> {
        if self.aot {
            // Use the TYPE TABLE's descriptor, not `func_types[findex]`.
            //
            // They describe the same function type and are two different
            // allocations of it, which is invisible until something compares
            // them by pointer. A closure call does exactly that: it checks
            // the closure's own type against `get_initialized_type(type_idx)`
            // and only falls back to the structural `hlp_same_type` walk when
            // they differ. Building the closure from the other descriptor
            // made that check fail every time, so bench_closure_call ran a
            // recursive type comparison inside a 100M-iteration loop.
            let type_index = match self.findexes.get(&findex) {
                Some(FuncPtr::Fun(f)) => f.type_.0,
                Some(FuncPtr::Native(n)) => n.type_.0,
                None => return Err(anyhow!("no function type for findex {findex}")),
            };
            return Ok(self.get_initialized_type(type_index)?.into_pointer_value());
        }
        let descriptor = *self
            .func_types
            .get(findex)
            .ok_or_else(|| anyhow!("no function type for findex {findex}"))?;
        Ok(self
            .context
            .i64_type()
            .const_int(descriptor as u64, false)
            .const_to_pointer(self.context.ptr_type(AddressSpace::default())))
    }

    fn function_slot_address(&self, findex: usize) -> Result<u64> {
        if let Some(shared) = self.shared_runtime.as_ref() {
            if !shared.module_ctx.is_null() {
                let base = unsafe { (*shared.module_ctx).functions_ptrs };
                if !base.is_null() {
                    return Ok(unsafe { base.add(findex) } as u64);
                }
            }
        }
        self.functions_ptrs
            .get(findex)
            .map(|slot| slot as *const *mut c_void as u64)
            .ok_or_else(|| anyhow!("function slot {findex} is out of range"))
    }

    fn live_function_address(&self, findex: usize) -> Option<usize> {
        let addr = if let Some(shared) = self.shared_runtime.as_ref() {
            if shared.module_ctx.is_null() {
                std::ptr::null_mut()
            } else {
                let base = unsafe { (*shared.module_ctx).functions_ptrs };
                if base.is_null() {
                    std::ptr::null_mut()
                } else {
                    unsafe { *base.add(findex) }
                }
            }
        } else {
            self.functions_ptrs.get(findex).copied()?
        };
        let addr = addr as usize;
        (addr >= crate::llvm::stub_bridge::STUB_SENTINEL_LIMIT as usize).then_some(addr)
    }

    /// Bind declarations in an isolated MCJIT module to code already
    /// installed by either tier.
    ///
    /// `module_funcs` is the module's own findex -> value map, and it is what
    /// makes a bytecode callee resolvable at all: `HLFunction::name` is the
    /// bare Haxe field name, so `update`, `new` and `dispose` name dozens of
    /// unrelated functions in one program. Resolving those by symbol picked
    /// whichever the engine happened to hold first, and a promotion then
    /// called a different class's method with no diagnostic anywhere -- the
    /// body was correct, only the edge was wrong. A findex is the identity the
    /// name is not, so bytecode declarations bind through it and never fall
    /// back to a name. Only runtime symbols -- natives, `hlp_*` helpers --
    /// still resolve by symbol, which for them is a unique C name.
    fn bind_module_declarations(
        &self,
        module: &inkwell::module::Module<'ctx>,
        module_funcs: &std::collections::HashMap<usize, FunctionValue<'ctx>>,
        label: &str,
    ) -> Result<()> {
        // Symbol -> findex for the bytecode functions this module declares.
        // Names are unique within a module, so this is injective; LLVM's own
        // uniquifying suffix rides along because the key is taken from the
        // value that is actually in the module.
        let mut bytecode_findex: std::collections::HashMap<&str, usize> =
            std::collections::HashMap::new();
        for (&findex, value) in module_funcs {
            if !matches!(self.findexes.get(&findex), Some(FuncPtr::Fun(_))) {
                continue;
            }
            if let Ok(symbol) = value.get_name().to_str() {
                bytecode_findex.insert(symbol, findex);
            }
        }

        let mut unresolved = Vec::new();
        for declaration in module.get_functions() {
            if declaration.count_basic_blocks() != 0 {
                continue;
            }
            let Ok(symbol) = declaration.get_name().to_str() else {
                continue;
            };
            if symbol.starts_with("llvm.") {
                continue;
            }
            let addr = match bytecode_findex.get(symbol) {
                // A bytecode callee: exactly this findex's installed code, or
                // nothing. `live_function_address` already rejects a stub
                // sentinel, so an uncompiled callee refuses the promotion
                // instead of calling a small integer.
                Some(&findex) => {
                    let exact = self.live_function_address(findex);
                    // `ASH_BIND_AUDIT=1` reports what resolving this callee by
                    // symbol would have produced, which is what this code did
                    // before. Read the output carefully: a DIFFERENT address is
                    // not by itself a wrong target. A `Fun_<findex>` name is
                    // unique, so a difference there is only the same function
                    // at another tier -- the engine's copy in the shared module
                    // versus whatever `functions_ptrs` currently holds. Only a
                    // difference on a bare Haxe field name is a wrong callee.
                    // Measured on a game: 46 differences in 90s, all of them on
                    // `Fun_` names and none on a collidable one, so the old
                    // resolution was ambiguous by construction but was not
                    // actually mis-resolving that workload.
                    if std::env::var_os("ASH_BIND_AUDIT").is_some() {
                        let by_name = self
                            .execution_engine
                            .get_function_address(symbol)
                            .ok()
                            .filter(|&a| a != 0)
                            .or_else(|| {
                                self.bytecode
                                    .functions
                                    .iter()
                                    .find(|f| f.name() == symbol)
                                    .and_then(|f| self.live_function_address(f.findex as usize))
                            });
                        if by_name != exact {
                            eprintln!(
                                "[bind] {label}: callee {symbol} findex={findex} \
                                 exact={exact:?} by_name={by_name:?} DIFFERS"
                            );
                        }
                    }
                    exact
                }
                None => self
                    .execution_engine
                    .get_function_address(symbol)
                    .ok()
                    .filter(|&addr| addr != 0),
            };
            match addr {
                Some(addr) => self.execution_engine.add_global_mapping(&declaration, addr),
                None => unresolved.push(symbol.to_string()),
            }
        }
        if unresolved.is_empty() {
            Ok(())
        } else {
            Err(anyhow!(
                "{label} has {} unresolved symbol(s): {}",
                unresolved.len(),
                unresolved.join(", ")
            ))
        }
    }

    /// Address of `field_index` inside `obj_ptr`, whose static type is
    /// `obj_type_index`.
    ///
    /// Prefers a constant offset from [`crate::layout`], which turns the whole
    /// access into one `getelementptr` on a known constant. The fallback — load
    /// the object's `hl_type*`, call `hlp_get_obj_rt`, load `fields_indexes`,
    /// then load the offset out of it — costs a call and three dependent loads
    /// per field access, and the call is opaque, so it also stops LLVM hoisting
    /// or CSE-ing anything across it.
    ///
    /// Reading the offset from the object's *dynamic* type is what the fallback
    /// does, and replacing that with a constant is sound because a subclass
    /// inherits its parent's `fields_indexes` verbatim — see the module docs on
    /// [`crate::layout`]. The oracle returns `None` for anything it cannot
    /// reproduce exactly (packed fields), which lands back on the fallback.
    fn build_field_ptr(
        &self,
        obj_type_index: usize,
        field_index: usize,
        obj_ptr: PointerValue<'ctx>,
    ) -> Result<PointerValue<'ctx>> {
        let i8_ty = self.context.i8_type();

        if let Some(offset) = crate::layout::field_offset_for(
            &self.types_,
            obj_type_index,
            field_index,
            self.target_abi.pointer_bytes() as i32,
        ) {
            let off = self.context.i64_type().const_int(offset as u64, false);
            return Ok(unsafe {
                self.builder
                    .build_gep(i8_ty, obj_ptr, &[off], "field_ptr")?
            });
        }

        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let type_ptr = self
            .builder
            .build_load(ptr_type, obj_ptr, "obj_type_ptr")?
            .into_pointer_value();
        let hl_get_obj_rt =
            self.declare_native("hlp_get_obj_rt", &[ptr_type.into()], Some(ptr_type.into()));
        let rt_obj = self
            .builder
            .build_call(hl_get_obj_rt, &[type_ptr.into()], "rt_obj")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| anyhow!("hlp_get_obj_rt returned void"))?;

        // hl_runtime_obj::fields_indexes sits at byte offset 40.
        let fields_indexes_gep = unsafe {
            self.builder.build_gep(
                i8_ty,
                rt_obj.into_pointer_value(),
                &[self.context.i64_type().const_int(
                    self.target_abi.hl_runtime_obj_fields_indexes_offset(),
                    false,
                )],
                "fields_indexes_gep",
            )?
        };
        let fields_indexes = self
            .builder
            .build_load(ptr_type, fields_indexes_gep, "fields_indexes")?
            .into_pointer_value();
        let field_offset_ptr = unsafe {
            self.builder.build_gep(
                self.context.i32_type(),
                fields_indexes,
                &[self.context.i32_type().const_int(field_index as u64, false)],
                "field_offset_ptr",
            )?
        };
        let field_offset_i32 = self
            .builder
            .build_load(
                self.context.i32_type(),
                field_offset_ptr,
                "field_offset_i32",
            )?
            .into_int_value();
        let field_offset = self.builder.build_int_z_extend(
            field_offset_i32,
            self.context.i64_type(),
            "field_offset",
        )?;
        Ok(unsafe {
            self.builder
                .build_gep(i8_ty, obj_ptr, &[field_offset], "field_ptr")?
        })
    }

    /// Compile all remaining bytecode functions not yet compiled.
    /// Functions only reachable through virtual dispatch (CallMethod on HVIRTUAL)
    /// are not discovered during the main compilation pass, so we compile them here.
    /// Any function that cannot be compiled gets a stub returning zero/null.
    fn compile_remaining_functions(&mut self) -> Result<()> {
        let uncompiled: Vec<usize> = self
            .findexes
            .iter()
            .filter_map(|(&findex, fp)| {
                if !self.func_cache.contains_key(&findex) {
                    if let FuncPtr::Fun(_) = fp {
                        return Some(findex);
                    }
                }
                None
            })
            .collect();

        for findex in &uncompiled {
            if let Err(_e) = self.compile_function(*findex) {
                // Compilation failure: create a stub so functions_ptrs has a valid address
                if !self.func_cache.contains_key(findex) {
                    let saved_block = self.builder.get_insert_block();
                    // Clone the function data to avoid borrow conflict with self
                    let f_clone = if let Some(FuncPtr::Fun(f)) = self.findexes.get(findex) {
                        Some(f.clone())
                    } else {
                        None
                    };
                    if let Some(f) = f_clone {
                        if let Ok(decl) = self.create_function_declaration(&f) {
                            let stub_block = self.context.append_basic_block(decl, "stub");
                            self.builder.position_at_end(stub_block);
                            let ret_type = decl.get_type().get_return_type();
                            if let Some(ret_type) = ret_type {
                                self.builder.build_return(Some(&ret_type.const_zero())).ok();
                            } else {
                                self.builder.build_return(None).ok();
                            }
                            self.func_cache.insert(*findex, decl);
                        }
                    }
                    if let Some(block) = saved_block {
                        self.builder.position_at_end(block);
                    }
                }
            }
        }

        // Also compile any functions that were discovered during the above compilation
        self.compile_pending_functions()?;

        Ok(())
    }

    /// Park the functions the middle end has already optimized, so a promotion
    /// pays for the function it is promoting rather than for the whole module.
    ///
    /// `run_passes` is a module operation: unscoped, every promotion optimizes
    /// every function compiled so far, tying promotion latency to the size of
    /// the module instead of the size of the function — 128ms of a 491ms fib
    /// run. The work is wasted twice over, since MCJIT has already emitted
    /// those functions and will not re-emit them.
    ///
    /// `optnone` makes the pass pipeline skip a body, the same lever
    /// `shield_trap_functions_from_optimization` uses. Two sets are exempt:
    ///
    /// - Anything not yet optimized, whatever reaches it. A callee is not
    ///   always a call operand — `StaticClosure` takes an address out of
    ///   `functions_ptrs` at runtime, `CallMethod` dispatches through a vtable,
    ///   and under `hot_reload` every direct call is rewritten to an indirect
    ///   one — so a keep set built from the call graph alone would leave those
    ///   functions emitted but never optimized. Reachability decides when a
    ///   function is compiled; this only decides when it is optimized, and
    ///   every function compiled into the module gets that exactly once.
    /// - The promoted function's direct callees, even when already optimized,
    ///   because `optnone` also blocks inlining and parking a callee would
    ///   quietly cost the promoted function the inline it was promoted to get.
    ///
    /// Returns the parked functions for `release_parked_functions`: a function
    /// parked for this promotion is an inlining candidate in the next one.
    fn park_optimized_functions(
        &self,
        root: inkwell::values::FunctionValue<'ctx>,
    ) -> Vec<inkwell::values::FunctionValue<'ctx>> {
        use inkwell::attributes::{Attribute, AttributeLoc};
        use inkwell::values::{CallSiteValue, FunctionValue};

        let noinline_id = Attribute::get_named_enum_kind_id("noinline");
        let optnone_id = Attribute::get_named_enum_kind_id("optnone");
        let noinline = self.context.create_enum_attribute(noinline_id, 0);
        let optnone = self.context.create_enum_attribute(optnone_id, 0);

        // Direct callees of the root, transitively: the inliner's working set.
        // Indirect targets are not inlining candidates, so missing them here
        // costs nothing — being unoptimized is what would cost, and the
        // not-yet-optimized rule below already covers that.
        let mut keep: std::collections::HashSet<FunctionValue<'ctx>> =
            std::collections::HashSet::new();
        let mut work: Vec<FunctionValue<'ctx>> = vec![root];
        keep.insert(root);
        while let Some(f) = work.pop() {
            for bb in f.get_basic_blocks() {
                let mut inst = bb.get_first_instruction();
                while let Some(i) = inst {
                    if let Ok(call) = CallSiteValue::try_from(i) {
                        if let Some(callee) = call.get_called_fn_value() {
                            // A declaration has no body to optimize or inline.
                            if callee.count_basic_blocks() > 0 && keep.insert(callee) {
                                work.push(callee);
                            }
                        }
                    }
                    inst = i.get_next_instruction();
                }
            }
        }

        let mut parked = Vec::new();
        for f in self.module.get_functions() {
            if f.count_basic_blocks() == 0 || keep.contains(&f) || !self.optimized_fns.contains(&f)
            {
                continue;
            }
            // Park only what carries neither mark, so releasing restores the
            // function exactly as it was and cannot lift a trap shield that
            // has to stay down.
            if f.get_enum_attribute(AttributeLoc::Function, optnone_id)
                .is_some()
                || f.get_enum_attribute(AttributeLoc::Function, noinline_id)
                    .is_some()
            {
                continue;
            }
            f.add_attribute(AttributeLoc::Function, noinline);
            f.add_attribute(AttributeLoc::Function, optnone);
            parked.push(f);
        }

        let with_body = self
            .module
            .get_functions()
            .filter(|f| f.count_basic_blocks() > 0)
            .count();
        // Work this run does, against the work an unscoped run would do. The
        // ratio between the two is the whole point of the scoping.
        crate::profile::count(
            "middle-end functions processed",
            (with_body - parked.len()) as u64,
        );
        crate::profile::count("middle-end functions in module", with_body as u64);
        crate::profile::count("middle-end functions parked", parked.len() as u64);
        // Distinct against total answers whether the scoped runs are doing the
        // same work repeatedly: a callee reachable from several hot functions
        // is re-optimised once per promotion, and if that is where the time
        // goes the fix is to keep the result rather than to optimise less.
        {
            use std::collections::HashSet;
            use std::sync::Mutex;
            static SEEN: Mutex<Option<HashSet<String>>> = Mutex::new(None);
            let mut seen = SEEN.lock().expect("middle-end seen set poisoned");
            let seen = seen.get_or_insert_with(HashSet::new);
            let before = seen.len();
            for f in self.module.get_functions() {
                if f.count_basic_blocks() == 0
                    || f.get_enum_attribute(AttributeLoc::Function, optnone_id)
                        .is_some()
                {
                    continue;
                }
                if let Ok(name) = f.get_name().to_str() {
                    seen.insert(name.to_string());
                }
            }
            // Only the delta: `count` accumulates, so summing it over every
            // promotion gives the number of functions optimised once and only
            // once. Reporting the running total instead would sum a running
            // total, which means nothing.
            crate::profile::count(
                "middle-end functions optimised for the first time",
                (seen.len() - before) as u64,
            );
        }
        parked
    }

    fn release_parked_functions(&self, parked: &[inkwell::values::FunctionValue<'ctx>]) {
        use inkwell::attributes::{Attribute, AttributeLoc};

        let noinline_id = Attribute::get_named_enum_kind_id("noinline");
        let optnone_id = Attribute::get_named_enum_kind_id("optnone");
        for f in parked {
            f.remove_enum_attribute(AttributeLoc::Function, optnone_id);
            f.remove_enum_attribute(AttributeLoc::Function, noinline_id);
        }
    }

    /// Record everything this run optimized, so the next one can park it.
    ///
    /// Trap-shielded functions are recorded too: the pipeline skips them under
    /// their own permanent `optnone`, so there is nothing for a later run to
    /// gain by keeping them in the working set.
    fn record_optimized_functions(&mut self, parked: &[inkwell::values::FunctionValue<'ctx>]) {
        let parked: std::collections::HashSet<_> = parked.iter().copied().collect();
        let mut optimized = std::mem::take(&mut self.optimized_fns);
        for f in self.module.get_functions() {
            if f.count_basic_blocks() > 0 && !parked.contains(&f) {
                optimized.insert(f);
            }
        }
        self.optimized_fns = optimized;
    }

    /// Opt functions containing `Trap` out of the LLVM middle-end.
    ///
    /// HL exceptions are setjmp/longjmp, and `longjmp` restores the machine
    /// registers to their state at the `setjmp`. A value the optimizer has
    /// promoted out of its alloca into an SSA value therefore reverts on the
    /// exceptional return, while one left in memory survives — the same reason
    /// C requires `volatile` on locals modified between `setjmp` and `longjmp`.
    /// Marking the setjmp call `returns_twice` (which this backend does) tells
    /// LLVM the call has two returns; it does not stop `mem2reg` promoting the
    /// allocas around it, which is the transform that actually breaks HL
    /// semantics. Observed as a dropped statement inside a `try` and a lost
    /// inner catch once the pipeline was switched on.
    ///
    /// `optnone` is the narrow fix: a function that can catch keeps its
    /// registers in memory, and every function that cannot — measured at ~99%
    /// of the corpus — is optimized normally. The alternative, making the
    /// register allocas volatile, would cost the same functions the same
    /// optimization while being harder to reason about.
    ///
    /// Returns how many functions were excluded.
    pub(crate) fn shield_trap_functions_from_optimization(&self) -> usize {
        self.shield_trap_functions_in(&self.func_cache)
    }

    /// Store a freshly computed float into `dst`, narrowing to that register's
    /// declared width first.
    ///
    /// The conversion opcodes all build their result as `f64`, but an HF32
    /// register is a 4-byte alloca. Storing the `f64` into it did two things
    /// at once: the next `load float` read the LOW half of the double's bit
    /// pattern -- `1.0` came back as `0.0f`, and roughly half of all values
    /// came back with the sign bit set -- and the 8-byte store ran 4 bytes off
    /// the end of the slot, corrupting whatever register LLVM had placed
    /// after it. On a game this reached OpenAL as `alSourcef(AL_GAIN, <junk>)`,
    /// which answers a negative or non-finite gain with AL_INVALID_VALUE, and
    /// the clobbered neighbour broke unrelated float state in the same frame.
    fn store_float_as_reg(
        &self,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        dst: usize,
        value: inkwell::values::FloatValue<'ctx>,
    ) -> Result<()> {
        let want = reg_types[dst];
        let value = if want.is_float_type()
            && want.into_float_type() == self.context.f32_type()
            && value.get_type() == self.context.f64_type()
        {
            self.builder
                .build_float_trunc(value, self.context.f32_type(), "narrow_f32")?
        } else {
            value
        };
        self.builder.build_store(registers[dst], value)?;
        Ok(())
    }

    /// The same shield over a module's own findex -> value map.
    ///
    /// `promote_in_own_module` swaps `func_cache` back to the host's before it
    /// optimizes, so the no-argument form shielded the host module's functions
    /// while the promo module went through `default<O2>` unshielded: a
    /// promoted body holding a try/catch could lose a statement or an inner
    /// catch, and only on the own-module path.
    pub(crate) fn shield_trap_functions_in(
        &self,
        cache: &std::collections::HashMap<usize, FunctionValue<'ctx>>,
    ) -> usize {
        let noinline = self.context.create_enum_attribute(
            inkwell::attributes::Attribute::get_named_enum_kind_id("noinline"),
            0,
        );
        let optnone = self.context.create_enum_attribute(
            inkwell::attributes::Attribute::get_named_enum_kind_id("optnone"),
            0,
        );
        let mut n = 0;
        for (findex, fv) in cache {
            let has_trap = match self.findexes.get(findex) {
                Some(FuncPtr::Fun(f)) => f
                    .ops
                    .iter()
                    .any(|op| matches!(op, Opcode::Trap { .. } | Opcode::EndTrap { .. })),
                _ => false,
            };
            if has_trap {
                // LLVM's verifier requires noinline alongside optnone.
                fv.add_attribute(inkwell::attributes::AttributeLoc::Function, noinline);
                fv.add_attribute(inkwell::attributes::AttributeLoc::Function, optnone);
                n += 1;
            }
        }
        n
    }

    fn fn_ir_dump_wanted_impl(findex: usize) -> bool {
        static SPEC: std::sync::OnceLock<Option<(bool, Vec<String>)>> = std::sync::OnceLock::new();
        let spec = SPEC.get_or_init(|| {
            std::env::var("ASH_DUMP_FN_IR").ok().map(|want| {
                let all = want == "all";
                (all, want.split(',').map(|w| w.trim().to_string()).collect())
            })
        });
        match spec {
            Some((all, wanted)) => *all || wanted.iter().any(|w| *w == findex.to_string()),
            None => false,
        }
    }

    /// Wrap the bytecode entrypoint in the same outer exception boundary that
    /// HashLink's `hl_dyn_call_safe` provides.
    ///
    /// The setjmp must live in generated code: placing it in Rust and then
    /// longjmping across `ExecutionEngine::run_function` would skip Rust/C++
    /// frames.  The wrapper returns 1 after printing an uncaught exception and
    /// 0 after a normal return.
    pub(crate) fn build_safe_entry_wrapper(
        &self,
        entrypoint: FunctionValue<'ctx>,
    ) -> Result<FunctionValue<'ctx>> {
        if entrypoint.count_params() != 0 {
            return Err(anyhow!("HashLink entrypoint unexpectedly takes arguments"));
        }

        let saved_block = self.builder.get_insert_block();
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let i32_type = self.context.i32_type();
        let i64_type = self.context.i64_type();
        let wrapper =
            self.module
                .add_function("__ash_safe_entrypoint", i64_type.fn_type(&[], false), None);
        self.stamp_host_cpu(wrapper);

        // setjmp locals must remain in memory across the second return.
        let noinline = self.context.create_enum_attribute(
            inkwell::attributes::Attribute::get_named_enum_kind_id("noinline"),
            0,
        );
        let optnone = self.context.create_enum_attribute(
            inkwell::attributes::Attribute::get_named_enum_kind_id("optnone"),
            0,
        );
        wrapper.add_attribute(inkwell::attributes::AttributeLoc::Function, noinline);
        wrapper.add_attribute(inkwell::attributes::AttributeLoc::Function, optnone);

        let start = self.context.append_basic_block(wrapper, "start");
        let normal = self.context.append_basic_block(wrapper, "normal");
        let exception = self.context.append_basic_block(wrapper, "exception");
        self.builder.position_at_end(start);

        let setup = self.declare_native("hlp_setup_trap_jit", &[], Some(ptr_type.into()));
        let buf = self
            .builder
            .build_call(setup, &[], "outer_trap_buf")?
            .try_as_basic_value()
            .basic()
            .unwrap()
            .into_pointer_value();
        let jumped = self.build_setjmp_call(buf, "outer_setjmp")?;
        let is_exception = self.builder.build_int_compare(
            IntPredicate::NE,
            jumped,
            i32_type.const_zero(),
            "outer_is_exception",
        )?;
        self.builder
            .build_conditional_branch(is_exception, exception, normal)?;

        self.builder.position_at_end(normal);
        self.builder.build_call(entrypoint, &[], "")?;
        let remove = self.declare_native("hlp_remove_trap_jit", &[], None);
        self.builder.build_call(remove, &[], "")?;
        self.builder.build_return(Some(&i64_type.const_zero()))?;

        self.builder.position_at_end(exception);
        let get_exc = self.declare_native("hlp_get_exc_value", &[], Some(ptr_type.into()));
        let exc = self
            .builder
            .build_call(get_exc, &[], "uncaught_exception")?
            .try_as_basic_value()
            .basic()
            .unwrap();
        let print = self.declare_native("hlp_print_uncaught_exception", &[ptr_type.into()], None);
        self.builder.build_call(print, &[exc.into()], "")?;
        let clear = self.declare_native("hlp_clear_exc_value", &[], None);
        self.builder.build_call(clear, &[], "")?;
        self.builder
            .build_return(Some(&i64_type.const_int(1, false)))?;

        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }
        if !wrapper.verify(true) {
            return Err(anyhow!("invalid LLVM safe-entrypoint wrapper"));
        }
        Ok(wrapper)
    }

    pub fn execute_main(&mut self) -> Result<()> {
        // Everything up to `execute` is compilation, grouped so the report
        // gives one number for it rather than four the reader has to add up --
        // and so `execute` sits beside it as a sibling instead of being nested
        // inside a phase named for the thing it is not.
        let compile_phase = crate::profile::scope("compile");
        // Compile any pending functions discovered during initialization
        {
            let _phase = crate::profile::scope("compile pending");
            self.compile_pending_functions()?;
        }

        // Compile remaining bytecode functions (e.g., virtual-dispatch-only methods)
        {
            let _phase = crate::profile::scope("compile remaining");
            self.compile_remaining_functions()?;
        }

        let index = self.bytecode.entrypoint as usize;
        let function = *self
            .func_cache
            .get(&index)
            .ok_or_else(|| anyhow!("Entrypoint function not found in cache"))?;
        let safe_entrypoint = self.build_safe_entry_wrapper(function)?;

        // Optimize before anything asks for an address: requesting one forces
        // codegen, and a pass run afterwards would be too late.
        {
            let _phase = crate::profile::scope("llvm middle-end");
            let excluded = self.shield_trap_functions_from_optimization();
            crate::profile::count("middle-end functions excluded (trap)", excluded as u64);
            super::module::run_middle_end(&self.module)?;
            // Whole-module by design here — this compiles everything once. A
            // promotion later in the same process starts from that.
            self.record_optimized_functions(&[]);
        }

        // `ASH_DUMP_FN_IR` worked only on the promote path until the map-
        // iterator investigation needed the whole-module IR to diff against
        // it; same flag, same post-middle-end vantage, both pipelines.
        for (findex, fun) in self.func_cache.iter() {
            if Self::fn_ir_dump_wanted_impl(*findex) {
                eprintln!(
                    "=== LLVM IR (whole-module) findex={findex} ===\n{}",
                    fun.print_to_string().to_string()
                );
            }
        }

        // Off unless asked for. This wrote the whole module to /tmp on every
        // run -- around 940KB and 13ms of it, inside the region the profiler
        // reports as compile time, on a binary whose compile time is the thing
        // most worth measuring. `ASH_DUMP_IR=1` restores the old path,
        // `ASH_DUMP_IR=<path>` chooses another.
        if let Ok(spec) = std::env::var("ASH_DUMP_IR") {
            if !spec.is_empty() && spec != "0" {
                let _phase = crate::profile::scope("dump ir");
                let path = if spec == "1" {
                    "/tmp/ash_jit.ll"
                } else {
                    &spec
                };
                match self.module.print_to_file(path) {
                    Ok(()) => eprintln!("[ash] LLVM IR written to {path}"),
                    Err(e) => eprintln!("[ash] could not write {path}: {e}"),
                }
            }
        }

        // The whole-module verifier, before MCJIT consumes the IR. This was
        // the one LLVM path with no verification at all: the tiered promote
        // path verifies per function, the OSR module verifies on build, and
        // this — the largest module of the three — handed MCJIT whatever the
        // builder produced. Invalid IR here is undefined behaviour that tends
        // to surface as an unrelated crash long after the cause.
        //
        // An error reports and aborts the run rather than continuing:
        // executing IR the verifier rejected is not a degraded mode, it is
        // UB. `ASH_LLVM_VERIFY=0` skips the check (and its one linear pass
        // over the module) once a measurement needs the old behaviour.
        if !matches!(
            std::env::var("ASH_LLVM_VERIFY").as_deref(),
            Ok("0") | Ok("off")
        ) {
            let _phase = crate::profile::scope("llvm verify");
            if let Err(msg) = self.module.verify() {
                return Err(anyhow!(
                    "LLVM module failed verification — an ash codegen bug:\n{}",
                    msg.to_string()
                ));
            }
        }

        // Populate functions_ptrs with actual function addresses from the JIT.
        // This must happen after compilation so the execution engine has allocated code.
        // Requesting every address is what forces MCJIT to emit machine code.
        {
            let _phase = crate::profile::scope("mcjit codegen");
            self.setup_functions_ptrs()?;
        }

        // Register GC roots BEFORE init_constants (which allocates and might trigger GC)
        unsafe {
            type FnSetGlobals = unsafe extern "C" fn(*const *mut std::ffi::c_void, usize);
            let set_globals: FnSetGlobals = std::mem::transmute(
                self.native_function_resolver
                    .resolve_function("std", "hlp_gc_set_globals")
                    .map_err(|e| anyhow!("Cannot resolve hlp_gc_set_globals: {}", e))?,
            );
            set_globals(self.globals_data.as_ptr(), self.globals_data.len());

            type FnSetStackTop = unsafe extern "C" fn(usize);
            let set_stack_top: FnSetStackTop = std::mem::transmute(
                self.native_function_resolver
                    .resolve_function("std", "hlp_gc_set_stack_top")
                    .map_err(|e| anyhow!("Cannot resolve hlp_gc_set_stack_top: {}", e))?,
            );
            set_stack_top(Self::current_stack_addr());
        }

        // Materialize bytecode constants (pre-initialized globals like string literals)
        // Compilation ends here; what follows is runtime setup and the run.
        drop(compile_phase);

        {
            let _phase = crate::profile::scope("init constants");
            self.init_constants()?;
        }

        // Pre-allocate class descriptors for HOBJ globals not populated by init_constants
        {
            let _phase = crate::profile::scope("init class descriptors");
            self.init_class_descriptors()?;
        }

        {
            let _phase = crate::profile::scope("execute");
            let status = unsafe {
                self.execution_engine
                    .run_function(safe_entrypoint, &[])
                    .as_int(false)
            };
            if status != 0 {
                return Err(anyhow!(
                    "HashLink program terminated with an uncaught exception"
                ));
            }
        }

        Ok(())
    }

    /// Populate the functions_ptrs table with actual function addresses.
    /// The table was pre-allocated in init_indexes and already wired into module contexts.
    fn setup_functions_ptrs(&mut self) -> Result<()> {
        // Collect function names and findexes first to avoid borrow conflicts
        let func_entries: Vec<(usize, String)> = self
            .func_cache
            .iter()
            .map(|(&findex, func_val)| {
                (
                    findex,
                    func_val.get_name().to_str().unwrap_or("").to_string(),
                )
            })
            .collect();

        for (findex, name) in &func_entries {
            if let Ok(addr) = self.execution_engine.get_function_address(name) {
                if addr != 0 && *findex < self.functions_ptrs.len() {
                    self.install_function_address(*findex, addr as *mut c_void);
                }
            }
        }

        Ok(())
    }

    pub(crate) fn install_function_address(&mut self, findex: usize, addr: *mut c_void) {
        // Every LLVM-compiled entry point passes through here, in both the
        // whole-module and the tiered path, so this is the one place the
        // profiler needs to learn about generated code.
        crate::jit_map::register(
            findex as u32,
            crate::profile::Tier::Llvm,
            crate::jit_map::CodeKind::Entry,
            addr as usize,
            0,
        );
        if findex < self.functions_ptrs.len() {
            self.functions_ptrs[findex] = addr;
        }
        if let Some(shared) = &self.shared_runtime {
            if !shared.module_ctx.is_null() {
                unsafe {
                    let ptrs = (*shared.module_ctx).functions_ptrs;
                    if !ptrs.is_null() {
                        *ptrs.add(findex) = addr;
                    }
                }
            }
        }
    }
}

pub struct FunctionBuilder<'ctx> {
    pub(crate) builder: Builder<'ctx>,
    pub(crate) execution_engine: ExecutionEngine<'ctx>,
    pub(crate) type_: Option<FunctionType<'ctx>>,
    pub(crate) value: Option<FunctionValue<'ctx>>,
    fun: HLFunction,
}

impl<'ctx> FunctionBuilder<'ctx> {
    pub fn new(
        fun: HLFunction,
        builder: Builder<'ctx>,
        execution_engine: ExecutionEngine<'ctx>,
    ) -> Self {
        Self {
            builder,
            execution_engine,
            fun,
            type_: None,
            value: None,
        }
    }

    pub fn build(&mut self, module: &mut JITModule<'ctx>) -> Result<()> {
        let regs = &self.fun.regs;
        // One HLTypeFun, not a deep clone of every type in the module.
        let fun = module
            .types_
            .get(self.fun.type_.0)
            .expect("Unknown type")
            .fun
            .clone()
            .expect("Expected to get function type");
        self.type_ = module.create_function_type(&fun).ok();
        self.value = module.create_function_value(self.fun.findex as usize).ok();
        Ok(())
    }
}

/// Report a promotion whose middle end ran longer than `ASH_PROMOTE_SLOW_MS`
/// (default 1000; set 0 to report every promotion).
///
/// One compile per run costs about a minute on a game, and no lever found so
/// far moves it: a 200x range of promotion thresholds leaves it unchanged, and
/// so does ASH_PROMOTE_MODULE_CAP at 1024, 256 and 64 -- which should have sent
/// that promotion into a module of its own. It is ~76% of all compile time and
/// it is the once-or-twice-a-run stall a player actually feels. What is missing
/// is not another theory but the two numbers that separate them: how many
/// bodies this promotion asked the middle end to walk, and how long the walk
/// took. A shared module of ~2000 bodies and a single pathological function
/// look identical from outside and want opposite fixes.
fn report_slow_promote(findex: usize, path: &str, bodies: usize, middle_end_ms: f64) {
    static THRESHOLD: std::sync::OnceLock<f64> = std::sync::OnceLock::new();
    let limit = *THRESHOLD.get_or_init(|| {
        std::env::var("ASH_PROMOTE_SLOW_MS")
            .ok()
            .and_then(|v| v.parse().ok())
            .unwrap_or(1000.0)
    });
    if middle_end_ms >= limit {
        eprintln!(
            "[promote] findex={findex} path={path} bodies={bodies} middle_end={middle_end_ms:.0}ms"
        );
    }
}

/// Findexes whose compiled LLVM body has been registered with the profiler,
/// so a batch is walked once per body rather than once per promotion.
fn registered_bodies() -> &'static std::sync::Mutex<std::collections::HashSet<usize>> {
    static SET: std::sync::OnceLock<std::sync::Mutex<std::collections::HashSet<usize>>> =
        std::sync::OnceLock::new();
    SET.get_or_init(|| std::sync::Mutex::new(std::collections::HashSet::new()))
}

/// Register a batch of bodies under their own findexes, each sized by where
/// the next one starts.
///
/// One module's bodies are emitted back to back, so `next.start - start` is
/// the body's size up to alignment padding -- close enough that a pc is
/// attributed by containment rather than by the nearest entry below it. The
/// last body of a batch has no successor here and is left unsized; the map
/// bounds it by whatever is registered above it.
fn register_batch(mut found: Vec<(usize, usize)>, how: &str) {
    found.sort_unstable_by_key(|&(_, a)| a);
    for i in 0..found.len() {
        let (fi, a) = found[i];
        let size = found.get(i + 1).map_or(0, |&(_, n)| n - a);
        register_body(fi, a, size, how);
    }
}

/// Register one compiled body under its own findex, and say so when the tier
/// log is on -- a run can then show which bodies a crash report can name.
fn register_body(findex: usize, addr: usize, size: usize, how: &str) {
    if !registered_bodies()
        .lock()
        .expect("registered bodies poisoned")
        .insert(findex)
    {
        return;
    }
    crate::jit_map::register(
        findex as u32,
        crate::profile::Tier::Llvm,
        crate::jit_map::CodeKind::Body,
        addr,
        size,
    );
    if std::env::var_os("ASH_TIER_LOG").is_some() {
        eprintln!("[tier] body findex={findex} tier=llvm addr={addr:#x} size={size} ({how})");
    }
}
