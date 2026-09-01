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
    AnyValue, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue,
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

/// Compile unresolved natives to call-time trap stubs instead of failing the
/// whole function compile — matching HashLink's disabled_primitive semantics
/// (errors when called, not when compiled) and the interpreter's lazy
/// resolution. Unlocks tier promotion of functions that merely reference
/// unimplemented natives. Default ON since JIT indirect-call sites guard
/// against interpreter stub sentinels (see build_stub_guarded_indirect_call);
/// opt out with ASH_JIT_NATIVE_TRAPS=0 to restore compile-time failure.
fn native_traps_enabled() -> bool {
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
fn hl_hash_utf8(s: &str) -> i32 {
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
            if let Some(off) = crate::layout::field_offset(&self.types_, type_index, field_index) {
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
            let global =
                self.aot_runtime_global("ash_fiber_poll_epoch", self.context.i64_type());
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

        let func_addr = self
            .native_function_resolver
            .resolve_function("std", name)
            .unwrap_or_else(|_| panic!("Failed to resolve native function: {}", name))
            as usize;

        self.generate_native_caller_with_addr(&caller_name, fn_type, func_addr)
            .unwrap_or_else(|e| panic!("Failed to generate caller for {}: {}", name, e))
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
    fn add_body_function(
        &self,
        name: &str,
        func_type: FunctionType<'ctx>,
    ) -> FunctionValue<'ctx> {
        let f = self
            .module
            .add_function(name, func_type, Some(inkwell::module::Linkage::External));
        // Under AOT the object may be built for a machine that is not this
        // one, and a host-CPU stamp would make it crash there rather than run
        // slower. The target machine handed to `emit_object` decides instead.
        if !self.aot {
            self.stamp_host_cpu(f);
        }
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
            let air = crate::llvm::air::prepare_llvm(
                &self.bytecode,
                &f,
                self.hot_reload,
                self.lazy_compilation,
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
    /// stop being compiled into it, or `None` to let it grow without bound.
    ///
    /// Off by default: capping at 1024 cut a game's shared middle end from
    /// 61.9s to 24.4s, but the game hung twice on the capped arm and the
    /// cause was never pinned down -- both hangs were also a second launch
    /// seconds after a SIGKILL, with a person playing the run being timed, so
    /// the evidence does not separate the cap from the confounds. Turning it
    /// on wants an A/B where the game is driven the same way in both arms.
    fn promote_module_cap() -> Option<usize> {
        static CAP: std::sync::OnceLock<Option<usize>> = std::sync::OnceLock::new();
        *CAP.get_or_init(|| {
            std::env::var("ASH_PROMOTE_MODULE_CAP")
                .ok()
                .and_then(|v| v.parse().ok())
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

        let built = self.init_required_natives().and_then(|()| {
            self.compile_function(findex)?;
            // Callees stay declarations, bound below to the addresses the host
            // already holds. Lowering copies of them here costs as much as the
            // shared module and buys nothing measurable -- a function that
            // needs them is sent down the shared path instead.
            self.pending_compilations.clear();
            Ok(())
        });

        self.builder.clear_insertion_position();
        let promo_module = std::mem::replace(&mut self.module, host_module);
        let target = self.func_cache.get(&findex).copied();
        self.func_cache = host_funcs;
        self.int_globals = host_ints;
        self.float_globals = host_floats;
        self.string_globals = host_strings;
        self.bytes_globals = host_bytes;
        self.type_info_globals = host_types;
        built?;

        let target =
            target.ok_or_else(|| anyhow!("promote module {modname}: no function built"))?;
        let name = target
            .get_name()
            .to_str()
            .map_err(|_| anyhow!("promote module {modname}: invalid symbol name"))?
            .to_string();

        {
            let _phase = crate::profile::scope("llvm middle-end (promote)");
            // The module holds this promotion and nothing else, so there is
            // nothing here to park.
            let excluded = self.shield_trap_functions_from_optimization();
            crate::profile::count("middle-end functions excluded (trap)", excluded as u64);
            let n = promo_module
                .get_functions()
                .filter(|f| f.count_basic_blocks() > 0)
                .count();
            crate::profile::count("middle-end functions processed", n as u64);
            crate::profile::count("middle-end functions in module", n as u64);
            let me_t0 = std::time::Instant::now();
            super::module::run_middle_end(&promo_module)?;
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

        self.bind_module_declarations(&promo_module, &format!("promote module {modname}"))?;

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
        for f in promo_module.get_functions() {
            if f.count_basic_blocks() == 0 {
                continue;
            }
            if let Ok(sym) = f.get_name().to_str() {
                if let Ok(a) = self.execution_engine.get_function_address(sym) {
                    if a != 0 {
                        crate::profile::register_jit_code(
                            findex as u32,
                            crate::profile::Tier::Llvm,
                            a as usize,
                        );
                    }
                }
            }
        }
        Ok(addr)
    }

    pub fn promote_function_strict(&mut self, findex: usize) -> Result<CompiledFunctionMeta> {
        let _phase = crate::profile::scope("llvm promote");
        crate::profile::count("llvm promotions", 1);
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
            let fn_addr = self.promote_in_own_module(findex)?;
            self.install_function_address(findex, fn_addr as *mut c_void);
            return self.compiled_meta_for(findex, fn_addr);
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
                        if matches!(
                            ins.get_opcode(),
                            inkwell::values::InstructionOpcode::Call
                        ) {
                            calls += 1;
                        }
                        i = ins.get_next_instruction();
                    }
                }
                (calls, instrs)
            };
            let before = std::env::var_os("ASH_INLINE_LOG").is_some().then(|| root_shape(target));
            let me_t0 = std::time::Instant::now();
            let result = super::module::run_middle_end(&self.module);
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
        let _phase = crate::profile::scope("llvm osr entry");
        let header = optimized
            .ser
            .block_pcs
            .iter()
            .position(|&pc| pc == header_pc)
            .ok_or_else(|| {
                anyhow!("osr header pc {header_pc} is not an AIR block in findex {findex}")
            })?;
        let source = self
            .bytecode
            .functions
            .iter()
            .find(|f| f.findex as usize == findex)
            .cloned()
            .ok_or_else(|| anyhow!("osr findex {findex} is not a bytecode function"))?;
        let name = format!("osr_{findex}_{header_pc}");
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
        // `Opcode::New` fetches a pre-created native caller out of `func_cache`
        // by generated name, so emptying the cache is not enough on its own --
        // the new module needs its own copy of those declarations.
        let natives_ready = self.init_required_natives();

        if std::env::var_os("ASH_OSR_LOG").is_some() {
            eprintln!("[osr] LLVM AIR build begin findex={findex} pc={header_pc}");
        }
        let built = natives_ready.and_then(|()| {
            self.build_air_osr_body(
                &source,
                &optimized.ir,
                AirBlockId(header as u32),
                header_pc,
                &name,
            )
        });

        self.builder.clear_insertion_position();
        let osr_module = std::mem::replace(&mut self.module, host_module);
        self.func_cache = host_funcs;
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
        self.bind_module_declarations(&osr_module, &format!("osr module {name}"))?;

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
                        crate::profile::register_jit_code(
                            findex as u32,
                            crate::profile::Tier::Llvm,
                            a as usize,
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
        crate::profile::register_jit_code(findex as u32, crate::profile::Tier::Llvm, addr as usize);
        return Ok(addr as u64);
    }

    /// Emit an AIR V2 OSR entry into whatever module is current.
    ///
    /// Cranelift spills the de-SSA register image described by the shared
    /// optimized AIR cache. Restoring that image directly into AIR values and
    /// cells keeps the transition in the typed IR; serializing it back into
    /// HashLink opcodes here would make the legacy bytecode translator the
    /// real LLVM OSR backend again.
    fn build_air_osr_body(
        &mut self,
        source: &HLFunction,
        air: &AirFunction,
        header: AirBlockId,
        header_pc: usize,
        name: &str,
    ) -> Result<()> {
        // `(ptr) -> ret`, where ret is the function's own return type.
        let type_fun = self.bytecode.types[source.type_.0]
            .fun
            .clone()
            .ok_or_else(|| anyhow!("findex {} has no function type", source.findex))?;
        let ptr_ty = self.context.ptr_type(AddressSpace::default());
        let ret_any = self.get_or_create_any_type(type_fun.ret.0)?;
        let fn_ty = match ret_any {
            AnyTypeEnum::VoidType(t) => t.fn_type(&[ptr_ty.into()], false),
            AnyTypeEnum::IntType(t) => t.fn_type(&[ptr_ty.into()], false),
            AnyTypeEnum::FloatType(t) => t.fn_type(&[ptr_ty.into()], false),
            AnyTypeEnum::PointerType(t) => t.fn_type(&[ptr_ty.into()], false),
            _ => ptr_ty.fn_type(&[ptr_ty.into()], false),
        };
        let function = self.module.add_function(name, fn_ty, None);
        self.stamp_host_cpu(function);

        let entry = self.context.append_basic_block(function, "osr_entry");
        self.builder.position_at_end(entry);
        let mut lowering = source.clone();
        lowering.regs = air
            .values
            .iter()
            .map(|v| TypeRef(v.ty.0 as usize))
            .chain(air.cells.iter().map(|c| TypeRef(c.ty.0 as usize)))
            .collect();
        lowering.ops.clear();
        let (registers, reg_types) = self.allocate_registers(&lowering)?;
        let cell_base = air.values.len();

        // Reconstruct every AIR value and pinned cell from the de-SSA
        // register image. Definitions inside the selected region overwrite
        // their seed before use; live-ins and header phis retain the value
        // Cranelift spilled for their original HashLink register.
        let buf = function
            .get_nth_param(0)
            .ok_or_else(|| anyhow!("osr entry has no buffer parameter"))?
            .into_pointer_value();
        for (i, value) in air.values.iter().enumerate() {
            let restored = self.load_air_osr_slot(buf, value.reg, reg_types[i])?;
            self.builder.build_store(registers[i], restored)?;
        }
        for (ci, cell) in air.cells.iter().enumerate() {
            let slot = cell_base + ci;
            let restored = self.load_air_osr_slot(buf, cell.reg, reg_types[slot])?;
            self.builder.build_store(registers[slot], restored)?;
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
            &mut lowering,
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

    fn allocate_registers(
        &mut self,
        f: &HLFunction,
    ) -> Result<(Vec<PointerValue<'ctx>>, Vec<BasicTypeEnum<'ctx>>)> {
        let mut ptrs = Vec::with_capacity(f.regs.len());
        let mut types = Vec::with_capacity(f.regs.len());
        for (i, reg) in f.regs.iter().enumerate() {
            let reg_type = self
                .get_register_type(reg.0)
                .expect("expected to get register type");
            types.push(reg_type);
            ptrs.push(self.builder.build_alloca(reg_type, &format!("reg_{}", i))?);
        }
        Ok((ptrs, types))
    }

    /// Lower a verified AIR v2 function directly to LLVM.
    ///
    /// Values and pinned cells get distinct stack slots. LLVM's mem2reg pass
    /// promotes the SSA value slots; cells deliberately remain addressable.
    /// AIR blocks, phi edges and terminators drive the CFG. The small opcode
    /// emitter below is reused for individual machine operations only -- it
    /// never sees or walks a serialized HashLink opcode array.
    fn translate_air_v2(
        &mut self,
        source: &HLFunction,
        air: &AirFunction,
        function: FunctionValue<'ctx>,
    ) -> Result<()> {
        self.current_findex = source.findex as usize;
        let mut lowering = source.clone();
        lowering.regs = air
            .values
            .iter()
            .map(|v| TypeRef(v.ty.0 as usize))
            .chain(air.cells.iter().map(|c| TypeRef(c.ty.0 as usize)))
            .collect();
        lowering.ops.clear();

        let (registers, reg_types) = self.allocate_registers(&lowering)?;
        let cell_base = air.values.len();
        let nargs = self.bytecode.types[source.type_.0]
            .fun
            .as_ref()
            .ok_or_else(|| anyhow!("findex {} has no function type", source.findex))?
            .args
            .len();

        // AIR does not emit Param values for pinned registers. Seed argument
        // cells here and give local cells HashLink's zero initialization.
        for (ci, cell) in air.cells.iter().enumerate() {
            let slot = cell_base + ci;
            let init = if (cell.reg as usize) < nargs {
                let param = function
                    .get_nth_param(cell.reg)
                    .ok_or_else(|| anyhow!("missing argument r{}", cell.reg))?;
                self.cast_for_call(param, reg_types[slot])?
            } else {
                reg_types[slot].const_zero()
            };
            self.builder.build_store(registers[slot], init)?;
        }

        let included = vec![true; air.blocks.len()];
        self.emit_air_v2_cfg(
            source,
            air,
            function,
            &mut lowering,
            &registers,
            &reg_types,
            cell_base,
            &included,
            AirBlockId(0),
        )
    }

    /// Emit the selected AIR CFG region, starting at `entry_target`.
    ///
    /// Ordinary functions select every block and start at b0. OSR entries
    /// select only blocks reachable from a loop header, after their entry
    /// block has restored the de-SSA register image.
    #[allow(clippy::too_many_arguments)]
    fn emit_air_v2_cfg(
        &mut self,
        source: &HLFunction,
        air: &AirFunction,
        function: FunctionValue<'ctx>,
        lowering: &mut HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        included: &[bool],
        entry_target: AirBlockId,
    ) -> Result<()> {
        let entry = self
            .builder
            .get_insert_block()
            .ok_or_else(|| anyhow!("AIR LLVM lowering has no entry block"))?;
        let nargs = self.bytecode.types[source.type_.0]
            .fun
            .as_ref()
            .ok_or_else(|| anyhow!("findex {} has no function type", source.findex))?
            .args
            .len();

        // One continuation block per selected AIR instruction plus its
        // terminator. NullCheck and Trap need an explicit continuation, and
        // keeping that shape for every instruction makes primitive-emitter
        // reuse exact.
        let mut blocks: Vec<Vec<BasicBlock<'ctx>>> =
            (0..air.blocks.len()).map(|_| Vec::new()).collect();
        for (bi, block) in air.blocks.iter().enumerate() {
            if !included.get(bi).copied().unwrap_or(false) {
                continue;
            }
            let mut seq = Vec::with_capacity(block.instrs.len() + 1);
            for ii in 0..=block.instrs.len() {
                seq.push(self.context.append_basic_block(
                    function,
                    &format!(
                        "air_b{bi}_{}",
                        if ii == block.instrs.len() {
                            "term".into()
                        } else {
                            ii.to_string()
                        }
                    ),
                ));
            }
            blocks[bi] = seq;
        }

        // Compiled fibers need safe points even in CPU-only loops. Derive
        // those points from AIR V2's natural-loop analysis. Each selected
        // header compares the runtime poll epoch; only a new scheduling/GC
        // request enters the cold helper block. All CFG edges target
        // `entries`, so phi copies still happen on the original predecessor
        // edge before the safe point.
        let cfg = air::v2::CfgInfo::build(air);
        let loops = air::v2::LoopForest::analyze(air, &cfg);
        let mut poll_headers = vec![false; air.blocks.len()];
        for lp in &loops.loops {
            if included.get(lp.header.idx()).copied().unwrap_or(false) {
                poll_headers[lp.header.idx()] = true;
            }
        }
        let has_polls = poll_headers.iter().any(|poll| *poll);
        let mut entries = vec![None; air.blocks.len()];
        for bi in 0..air.blocks.len() {
            if blocks[bi].is_empty() {
                continue;
            }
            entries[bi] = if poll_headers[bi] {
                Some(
                    self.context
                        .append_basic_block(function, &format!("air_b{bi}_fiber_poll")),
                )
            } else {
                blocks[bi].first().copied()
            };
        }

        // `cold`: the poll fires when a fiber is due to yield, which on a hot
        // loop is approximately never. Without the hint LLVM lays the call out
        // as an equally likely successor, and the whole dispatch chain --
        // obj->t, t->vobj_proto, the null test, the slot load -- is stranded in
        // the loop behind a call it must assume writes all memory.
        //
        // Deliberately a function attribute rather than branch weights on the
        // poll test. Weights encode a POLARITY: rewrite the branch to test the
        // negated condition and forget to swap them, and the hint silently
        // inverts into a pessimisation no test would catch. `cold` has no
        // polarity, applies at one site instead of every poll branch, and
        // survives block splitting and jump threading.
        let fiber_poll = has_polls.then(|| {
            let f = self.declare_native("hlp_fiber_poll", &[], None);
            let cold = self.context.create_enum_attribute(
                inkwell::attributes::Attribute::get_named_enum_kind_id("cold"),
                0,
            );
            f.add_attribute(inkwell::attributes::AttributeLoc::Function, cold);
            f
        });
        self.builder.position_at_end(entry);
        let poll_epoch = if has_polls {
            let slot = self
                .builder
                .build_alloca(self.context.i64_type(), "air_fiber_poll_epoch")?;
            let pointer = self.fiber_poll_epoch_ptr()?;
            let initial = self.builder.build_load(
                self.context.i64_type(),
                pointer,
                "air_fiber_poll_epoch_initial",
            )?;
            initial
                .as_instruction_value()
                .expect("epoch load is an instruction")
                .set_atomic_ordering(AtomicOrdering::Monotonic)
                .map_err(|error| anyhow!("failed to mark epoch load atomic: {error:?}"))?;
            self.builder.build_store(slot, initial)?;
            Some((slot, pointer))
        } else {
            None
        };

        let first = entries
            .get(entry_target.idx())
            .copied()
            .flatten()
            .ok_or_else(|| anyhow!("AIR entry block b{} is not selected", entry_target.0))?;
        self.builder.position_at_end(entry);
        self.builder.build_unconditional_branch(first)?;

        for bi in 0..air.blocks.len() {
            let Some(poll_entry) = entries[bi].filter(|_| poll_headers[bi]) else {
                continue;
            };
            let (handled_slot, epoch_pointer) = poll_epoch.expect("poll headers have an epoch");
            let poll_call = self
                .context
                .append_basic_block(function, &format!("air_b{bi}_fiber_poll_call"));
            let body = blocks[bi][0];

            self.builder.position_at_end(poll_entry);
            let handled = self
                .builder
                .build_load(
                    self.context.i64_type(),
                    handled_slot,
                    "air_fiber_poll_handled_epoch",
                )?
                .into_int_value();
            let current = self.builder.build_load(
                self.context.i64_type(),
                epoch_pointer,
                "air_fiber_poll_current_epoch",
            )?;
            current
                .as_instruction_value()
                .expect("epoch load is an instruction")
                .set_atomic_ordering(AtomicOrdering::Monotonic)
                .map_err(|error| anyhow!("failed to mark epoch load atomic: {error:?}"))?;
            let current = current.into_int_value();
            let due = self.builder.build_int_compare(
                IntPredicate::NE,
                current,
                handled,
                "air_fiber_poll_due",
            )?;
            self.builder
                .build_conditional_branch(due, poll_call, body)?;

            self.builder.position_at_end(poll_call);
            self.builder.build_store(handled_slot, current)?;
            self.builder.build_call(
                fiber_poll.expect("poll headers have a helper"),
                &[],
                "air_fiber_poll",
            )?;
            self.builder.build_unconditional_branch(body)?;
        }

        for (bi, block) in air.blocks.iter().enumerate() {
            if !included.get(bi).copied().unwrap_or(false) {
                continue;
            }
            for (ii, instr) in block.instrs.iter().enumerate() {
                let current = blocks[bi][ii];
                let next = blocks[bi][ii + 1];
                self.builder.position_at_end(current);

                match instr {
                    AirInstr::Param { dst, reg } => {
                        let dst = dst.idx();
                        let value = if (*reg as usize) < nargs {
                            let param = function
                                .get_nth_param(*reg)
                                .ok_or_else(|| anyhow!("missing argument r{reg}"))?;
                            self.cast_for_call(param, reg_types[dst])?
                        } else {
                            reg_types[dst].const_zero()
                        };
                        self.builder.build_store(registers[dst], value)?;
                    }
                    AirInstr::UnOp { op, dst, src }
                        if matches!(op, AirUnOp::Incr | AirUnOp::Decr) =>
                    {
                        // AIR models Incr/Decr as an SSA definition from the
                        // old value. The legacy opcode mutates its destination
                        // in place, so adapting it directly would read an
                        // uninitialized destination alloca.
                        let value = self.builder.build_load(
                            reg_types[src.idx()],
                            registers[src.idx()],
                            "air_step_src",
                        )?;
                        let value = value.into_int_value();
                        let one = value.get_type().const_int(1, false);
                        let result = if matches!(op, AirUnOp::Incr) {
                            self.builder.build_int_add(value, one, "air_incr")?
                        } else {
                            self.builder.build_int_sub(value, one, "air_decr")?
                        };
                        self.builder.build_store(registers[dst.idx()], result)?;
                    }
                    AirInstr::Fma { dst, a, b, c } => {
                        self.emit_air_fma(*dst, *a, *b, *c, &registers, &reg_types)?;
                    }
                    AirInstr::FieldGet { obj, obj_ty, .. }
                    | AirInstr::FieldSet { obj, obj_ty, .. } => {
                        // AIR resolved the field's declaring object type once.
                        // Let the reused field primitive see that answer rather
                        // than re-deriving it from the value's declared type.
                        let ri = obj.idx();
                        let saved = lowering.regs[ri].clone();
                        lowering.regs[ri] = TypeRef(obj_ty.0 as usize);
                        let op = self
                            .air_instr_opcode(instr, cell_base)?
                            .ok_or_else(|| anyhow!("AIR field instruction produced no opcode"))?;
                        let dummy = [current, next];
                        self.translate_opcode(lowering, &op, registers, reg_types, 0, &dummy)?;
                        lowering.regs[ri] = saved;
                    }
                    AirInstr::SetEnumField {
                        value,
                        construct,
                        field,
                        src,
                    } => {
                        // The legacy primitive historically discovered the
                        // construct by scanning preceding opcodes. AIR carries
                        // it explicitly, so provide only that local fact.
                        let alloc = Opcode::EnumAlloc {
                            dst: Reg(value.0),
                            construct: RefEnumConstruct(*construct),
                        };
                        let set = Opcode::SetEnumField {
                            value: Reg(value.0),
                            field: RefField(*field),
                            src: Reg(src.0),
                        };
                        lowering.ops = vec![alloc, set.clone()];
                        let dummy = [current, current, next];
                        self.translate_opcode(lowering, &set, registers, reg_types, 1, &dummy)?;
                        lowering.ops.clear();
                    }
                    _ => {
                        if let Some(op) = self.air_instr_opcode(instr, cell_base)? {
                            let dummy = [current, next];
                            self.translate_opcode(lowering, &op, registers, reg_types, 0, &dummy)?;
                        }
                    }
                }

                if self
                    .builder
                    .get_insert_block()
                    .is_some_and(|b| b.get_terminator().is_none())
                {
                    self.builder.build_unconditional_branch(next)?;
                }
            }

            let term_block = blocks[bi][block.instrs.len()];
            self.builder.position_at_end(term_block);
            self.emit_air_terminator(
                source,
                air,
                AirBlockId(bi as u32),
                &block.term,
                &entries,
                lowering,
                registers,
                reg_types,
                cell_base,
            )?;
        }

        // Leave the caller in a valid insertion block. It is unreachable;
        // every verified AIR block already has a terminator.
        let exit = self.context.append_basic_block(function, "air_exit");
        self.builder.position_at_end(exit);
        Ok(())
    }

    fn emit_air_fma(
        &self,
        dst: ValueId,
        a: ValueId,
        b: ValueId,
        c: ValueId,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
    ) -> Result<()> {
        use inkwell::intrinsics::Intrinsic;
        let ty = reg_types[dst.idx()];
        let BasicTypeEnum::FloatType(float_ty) = ty else {
            return Err(anyhow!("AIR Fma destination is not a float"));
        };
        let load = |v: ValueId, name: &str| {
            self.builder
                .build_load(reg_types[v.idx()], registers[v.idx()], name)
                .map(|v| v.into_float_value())
        };
        let av = load(a, "fma_a")?;
        let bv = load(b, "fma_b")?;
        let cv = load(c, "fma_c")?;
        let intr = Intrinsic::find("llvm.fma").ok_or_else(|| anyhow!("LLVM fma unavailable"))?;
        let decl = intr
            .get_declaration(&self.module, &[float_ty.into()])
            .ok_or_else(|| anyhow!("no LLVM fma declaration"))?;
        let value = self
            .builder
            .build_call(decl, &[av.into(), bv.into(), cv.into()], "air_fma")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| anyhow!("LLVM fma returned void"))?;
        self.builder.build_store(registers[dst.idx()], value)?;
        Ok(())
    }

    /// Adapt one non-terminating AIR instruction to the existing primitive
    /// emitter. The adapter only supplies operands; AIR still owns the CFG,
    /// SSA joins, and resolved type information.
    fn air_instr_opcode(&self, instr: &AirInstr, cell_base: usize) -> Result<Option<Opcode>> {
        let reg = |v: ValueId| Reg(v.0);
        let cell = |c: air::v2::ir::CellId| Reg((cell_base + c.idx()) as u32);
        let call = |dst: ValueId, fun: usize, args: &[ValueId]| -> Result<Opcode> {
            let dst = reg(dst);
            let fun = RefFun(fun);
            let args: Vec<Reg> = args.iter().copied().map(reg).collect();
            Ok(match args.as_slice() {
                [] => Opcode::Call0 { dst, fun },
                [arg0] => Opcode::Call1 {
                    dst,
                    fun,
                    arg0: *arg0,
                },
                [arg0, arg1] => Opcode::Call2 {
                    dst,
                    fun,
                    arg0: *arg0,
                    arg1: *arg1,
                },
                [arg0, arg1, arg2] => Opcode::Call3 {
                    dst,
                    fun,
                    arg0: *arg0,
                    arg1: *arg1,
                    arg2: *arg2,
                },
                [arg0, arg1, arg2, arg3] => Opcode::Call4 {
                    dst,
                    fun,
                    arg0: *arg0,
                    arg1: *arg1,
                    arg2: *arg2,
                    arg3: *arg3,
                },
                _ => Opcode::CallN { dst, fun, args },
            })
        };

        let opcode = match instr {
            AirInstr::Param { .. } | AirInstr::Fma { .. } => return Ok(None),
            AirInstr::Copy { dst, src } => Opcode::Mov {
                dst: reg(*dst),
                src: reg(*src),
            },
            AirInstr::Int { dst, idx } => Opcode::Int {
                dst: reg(*dst),
                ptr: RefInt(*idx),
            },
            AirInstr::Float { dst, idx } => Opcode::Float {
                dst: reg(*dst),
                ptr: RefFloat(*idx),
            },
            AirInstr::Bool { dst, value } => Opcode::Bool {
                dst: reg(*dst),
                value: *value,
            },
            AirInstr::Bytes { dst, idx } => Opcode::Bytes {
                dst: reg(*dst),
                ptr: RefBytes(*idx),
            },
            AirInstr::String { dst, idx } => Opcode::String {
                dst: reg(*dst),
                ptr: RefString(*idx),
            },
            AirInstr::Null { dst } => Opcode::Null { dst: reg(*dst) },
            AirInstr::BinOp { op, dst, a, b } => {
                let dst = reg(*dst);
                let a = reg(*a);
                let b = reg(*b);
                match op {
                    AirBinOp::Add => Opcode::Add { dst, a, b },
                    AirBinOp::Sub => Opcode::Sub { dst, a, b },
                    AirBinOp::Mul => Opcode::Mul { dst, a, b },
                    AirBinOp::SDiv => Opcode::SDiv { dst, a, b },
                    AirBinOp::UDiv => Opcode::UDiv { dst, a, b },
                    AirBinOp::SMod => Opcode::SMod { dst, a, b },
                    AirBinOp::UMod => Opcode::UMod { dst, a, b },
                    AirBinOp::Shl => Opcode::Shl { dst, a, b },
                    AirBinOp::SShr => Opcode::SShr { dst, a, b },
                    AirBinOp::UShr => Opcode::UShr { dst, a, b },
                    AirBinOp::And => Opcode::And { dst, a, b },
                    AirBinOp::Or => Opcode::Or { dst, a, b },
                    AirBinOp::Xor => Opcode::Xor { dst, a, b },
                }
            }
            AirInstr::UnOp { op, dst, src } => {
                let dst = reg(*dst);
                let src = reg(*src);
                match op {
                    AirUnOp::Neg => Opcode::Neg { dst, src },
                    AirUnOp::Not => Opcode::Not { dst, src },
                    AirUnOp::Incr => Opcode::Incr { dst },
                    AirUnOp::Decr => Opcode::Decr { dst },
                }
            }
            AirInstr::Intrinsic { fun, dst, args, .. } => return Ok(Some(call(*dst, *fun, args)?)),
            AirInstr::Call { dst, fun, args } => {
                if self.lazy_compilation && matches!(self.findexes.get(fun), Some(FuncPtr::Fun(_)))
                {
                    Opcode::IndirectCall {
                        dst: reg(*dst),
                        fun: RefFun(*fun),
                        args: args.iter().copied().map(reg).collect(),
                    }
                } else {
                    return Ok(Some(call(*dst, *fun, args)?));
                }
            }
            AirInstr::CallMethod { dst, field, args } => Opcode::CallMethod {
                dst: reg(*dst),
                field: RefField(*field),
                args: args.iter().copied().map(reg).collect(),
            },
            AirInstr::CallClosure { dst, fun, args } => Opcode::CallClosure {
                dst: reg(*dst),
                fun: reg(*fun),
                args: args.iter().copied().map(reg).collect(),
            },
            AirInstr::StaticClosure { dst, fun } => Opcode::StaticClosure {
                dst: reg(*dst),
                fun: RefFun(*fun),
            },
            AirInstr::InstanceClosure { dst, fun, obj } => Opcode::InstanceClosure {
                dst: reg(*dst),
                fun: RefFun(*fun),
                obj: reg(*obj),
            },
            AirInstr::VirtualClosure { dst, obj, field } => Opcode::VirtualClosure {
                dst: reg(*dst),
                obj: reg(*obj),
                field: Reg(*field as u32),
            },
            AirInstr::GetGlobal { dst, global } => Opcode::GetGlobal {
                dst: reg(*dst),
                global: RefGlobal(*global),
            },
            AirInstr::SetGlobal { global, src } => Opcode::SetGlobal {
                global: RefGlobal(*global),
                src: reg(*src),
            },
            AirInstr::FieldGet {
                dst, obj, field, ..
            } => Opcode::Field {
                dst: reg(*dst),
                obj: reg(*obj),
                field: RefField(*field),
            },
            AirInstr::FieldSet {
                obj, field, src, ..
            } => Opcode::SetField {
                obj: reg(*obj),
                field: RefField(*field),
                src: reg(*src),
            },
            AirInstr::DynGet { dst, obj, field } => Opcode::DynGet {
                dst: reg(*dst),
                obj: reg(*obj),
                field: RefString(*field),
            },
            AirInstr::DynSet { obj, field, src } => Opcode::DynSet {
                obj: reg(*obj),
                field: RefString(*field),
                src: reg(*src),
            },
            AirInstr::Cast { kind, dst, src } => {
                let dst = reg(*dst);
                let src = reg(*src);
                match kind {
                    AirCastKind::ToDyn => Opcode::ToDyn { dst, src },
                    AirCastKind::ToSFloat => Opcode::ToSFloat { dst, src },
                    AirCastKind::ToUFloat => Opcode::ToUFloat { dst, src },
                    AirCastKind::ToInt => Opcode::ToInt { dst, src },
                    AirCastKind::SafeCast => Opcode::SafeCast { dst, src },
                    AirCastKind::UnsafeCast => Opcode::UnsafeCast { dst, src },
                    AirCastKind::ToVirtual => Opcode::ToVirtual { dst, src },
                }
            }
            AirInstr::NullCheck { value } => Opcode::NullCheck { reg: reg(*value) },
            AirInstr::EndTrap { flag, .. } => Opcode::EndTrap {
                // OEndTrap's operand is a boolean flag, not the exception cell.
                exc: Reg(*flag as u32),
            },
            AirInstr::MemGet {
                kind,
                dst,
                base,
                index,
            } => {
                let dst = reg(*dst);
                let base = reg(*base);
                let index = reg(*index);
                match kind {
                    AirMemAccess::I8 => Opcode::GetI8 {
                        dst,
                        bytes: base,
                        index,
                    },
                    AirMemAccess::I16 => Opcode::GetI16 {
                        dst,
                        bytes: base,
                        index,
                    },
                    AirMemAccess::Mem => Opcode::GetMem {
                        dst,
                        bytes: base,
                        index,
                    },
                    AirMemAccess::Array => Opcode::GetArray {
                        dst,
                        array: base,
                        index,
                    },
                }
            }
            AirInstr::MemSet {
                kind,
                base,
                index,
                src,
            } => {
                let base = reg(*base);
                let index = reg(*index);
                let src = reg(*src);
                match kind {
                    AirMemAccess::I8 => Opcode::SetI8 {
                        bytes: base,
                        index,
                        src,
                    },
                    AirMemAccess::I16 => Opcode::SetI16 {
                        bytes: base,
                        index,
                        src,
                    },
                    AirMemAccess::Mem => Opcode::SetMem {
                        bytes: base,
                        index,
                        src,
                    },
                    AirMemAccess::Array => Opcode::SetArray {
                        array: base,
                        index,
                        src,
                    },
                }
            }
            AirInstr::New { dst } => Opcode::New { dst: reg(*dst) },
            AirInstr::ArraySize { dst, array } => Opcode::ArraySize {
                dst: reg(*dst),
                array: reg(*array),
            },
            AirInstr::TypeConst { dst, ty } => Opcode::Type {
                dst: reg(*dst),
                ty: RefType(ty.0 as usize),
            },
            AirInstr::GetType { dst, src } => Opcode::GetType {
                dst: reg(*dst),
                src: reg(*src),
            },
            AirInstr::GetTID { dst, src } => Opcode::GetTID {
                dst: reg(*dst),
                src: reg(*src),
            },
            AirInstr::Unref { dst, src } => Opcode::Unref {
                dst: reg(*dst),
                src: reg(*src),
            },
            AirInstr::SetRef { r, value } => Opcode::Setref {
                dst: reg(*r),
                value: reg(*value),
            },
            AirInstr::RefData { dst, src } => Opcode::RefData {
                dst: reg(*dst),
                src: reg(*src),
            },
            AirInstr::RefOffset { dst, base, offset } => Opcode::RefOffset {
                dst: reg(*dst),
                reg: reg(*base),
                offset: reg(*offset),
            },
            AirInstr::MakeEnum {
                dst,
                construct,
                args,
            } => Opcode::MakeEnum {
                dst: reg(*dst),
                construct: RefEnumConstruct(*construct),
                args: args.iter().copied().map(reg).collect(),
            },
            AirInstr::EnumAlloc { dst, construct } => Opcode::EnumAlloc {
                dst: reg(*dst),
                construct: RefEnumConstruct(*construct),
            },
            AirInstr::EnumIndex { dst, value } => Opcode::EnumIndex {
                dst: reg(*dst),
                value: reg(*value),
            },
            AirInstr::EnumField {
                dst,
                value,
                construct,
                field,
            } => Opcode::EnumField {
                dst: reg(*dst),
                value: reg(*value),
                construct: RefEnumConstruct(*construct),
                field: RefField(*field),
            },
            AirInstr::SetEnumField {
                value, field, src, ..
            } => Opcode::SetEnumField {
                value: reg(*value),
                field: RefField(*field),
                src: reg(*src),
            },
            AirInstr::CellGet { dst, cell: c } => Opcode::Mov {
                dst: reg(*dst),
                src: cell(*c),
            },
            AirInstr::CellSet { cell: c, src } => Opcode::Mov {
                dst: cell(*c),
                src: reg(*src),
            },
            AirInstr::CellIncr { cell: c } => Opcode::Incr { dst: cell(*c) },
            AirInstr::CellDecr { cell: c } => Opcode::Decr { dst: cell(*c) },
            AirInstr::CellRef { dst, cell: c } => Opcode::Ref {
                dst: reg(*dst),
                src: cell(*c),
            },
            AirInstr::Assert => Opcode::Assert,
            AirInstr::Prefetch { value, field, mode } => Opcode::Prefetch {
                value: reg(*value),
                field: RefField(*field),
                mode: *mode,
            },
            AirInstr::Asm {
                mode,
                value,
                reg: r,
            } => Opcode::Asm {
                mode: *mode,
                value: *value,
                reg: Reg(*r),
            },
        };
        Ok(Some(opcode))
    }

    /// Emit copies for one ordinary CFG edge's phi nodes.
    fn emit_air_phi_edge(
        &mut self,
        air: &AirFunction,
        from: AirBlockId,
        to: AirBlockId,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
    ) -> Result<()> {
        for phi in &air.blocks[to.idx()].phis {
            let src = phi
                .incoming
                .iter()
                .find(|(pred, _)| *pred == from)
                .map(|(_, value)| *value)
                .ok_or_else(|| {
                    anyhow!(
                        "AIR phi in b{} has no incoming value from b{}",
                        to.0,
                        from.0
                    )
                })?;
            let loaded = self.builder.build_load(
                reg_types[src.idx()],
                registers[src.idx()],
                "air_phi_src",
            )?;
            let value = if loaded.get_type() == reg_types[phi.dst.idx()] {
                loaded
            } else {
                self.cast_for_call(loaded, reg_types[phi.dst.idx()])?
            };
            self.builder.build_store(registers[phi.dst.idx()], value)?;
        }
        Ok(())
    }

    /// Emit an AIR V2 conditional as an LLVM branch condition.
    fn emit_air_condition(
        &mut self,
        lowering: &HLFunction,
        cond: AirCondKind,
        a: ValueId,
        b: Option<ValueId>,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
    ) -> Result<inkwell::values::IntValue<'ctx>> {
        let av = self
            .builder
            .build_load(reg_types[a.idx()], registers[a.idx()], "air_cond_a")?;
        if cond.is_unary() {
            return match cond {
                AirCondKind::True => Ok(av.into_int_value()),
                AirCondKind::False => {
                    let value = av.into_int_value();
                    Ok(self.builder.build_int_compare(
                        IntPredicate::EQ,
                        value,
                        value.get_type().const_zero(),
                        "air_cond_false",
                    )?)
                }
                AirCondKind::Null | AirCondKind::NotNull => {
                    if av.is_pointer_value() {
                        let is_null = self
                            .builder
                            .build_is_null(av.into_pointer_value(), "air_cond_null")?;
                        Ok(if cond == AirCondKind::Null {
                            is_null
                        } else {
                            self.builder.build_not(is_null, "air_cond_not_null")?
                        })
                    } else {
                        Ok(self
                            .context
                            .bool_type()
                            .const_int((cond == AirCondKind::NotNull) as u64, false))
                    }
                }
                _ => unreachable!("CondKind::is_unary only admits unary conditions"),
            };
        }

        let b = b.ok_or_else(|| anyhow!("binary AIR condition has no rhs"))?;
        let bv = self
            .builder
            .build_load(reg_types[b.idx()], registers[b.idx()], "air_cond_b")?;
        let bv = if av.get_type() == bv.get_type() {
            bv
        } else {
            self.cast_for_call(bv, av.get_type())?
        };
        let a_kind = self.types_[lowering.regs[a.idx()].0].kind;
        let (int_pred, float_pred) = match cond {
            AirCondKind::SLt => (IntPredicate::SLT, FloatPredicate::OLT),
            AirCondKind::SGte => (IntPredicate::SGE, FloatPredicate::OGE),
            AirCondKind::SGt => (IntPredicate::SGT, FloatPredicate::OGT),
            AirCondKind::SLte => (IntPredicate::SLE, FloatPredicate::OLE),
            AirCondKind::ULt => (IntPredicate::ULT, FloatPredicate::OLT),
            AirCondKind::UGte => (IntPredicate::UGE, FloatPredicate::OGE),
            AirCondKind::NotLt => (IntPredicate::SGE, FloatPredicate::OGE),
            AirCondKind::NotGte => (IntPredicate::SLT, FloatPredicate::OLT),
            AirCondKind::Eq => (IntPredicate::EQ, FloatPredicate::OEQ),
            AirCondKind::NotEq => (IntPredicate::NE, FloatPredicate::ONE),
            _ => unreachable!("unary conditions returned above"),
        };

        Ok(match av.get_type().as_any_type_enum() {
            AnyTypeEnum::IntType(_) => self.builder.build_int_compare(
                int_pred,
                av.into_int_value(),
                bv.into_int_value(),
                "air_cond_int",
            )?,
            AnyTypeEnum::FloatType(_) => self.builder.build_float_compare(
                float_pred,
                av.into_float_value(),
                bv.into_float_value(),
                "air_cond_float",
            )?,
            AnyTypeEnum::PointerType(_) => {
                if a_kind == hl_type_kind_HDYN
                    || a_kind == hl_type_kind_HNULL
                    || a_kind == hl_type_kind_HOBJ
                {
                    let ptr_type = self.context.ptr_type(AddressSpace::default());
                    let compare = self.declare_native(
                        "hlp_dyn_compare",
                        &[ptr_type.into(), ptr_type.into()],
                        Some(self.context.i32_type().into()),
                    );
                    let result = self
                        .builder
                        .build_call(compare, &[av.into(), bv.into()], "air_dyn_compare")?
                        .try_as_basic_value()
                        .basic()
                        .ok_or_else(|| anyhow!("hlp_dyn_compare returned void"))?
                        .into_int_value();
                    self.builder.build_int_compare(
                        int_pred,
                        result,
                        self.context.i32_type().const_zero(),
                        "air_dyn_condition",
                    )?
                } else {
                    let ai = self.builder.build_ptr_to_int(
                        av.into_pointer_value(),
                        self.context.i64_type(),
                        "air_ptr_a",
                    )?;
                    let bi = self.builder.build_ptr_to_int(
                        bv.into_pointer_value(),
                        self.context.i64_type(),
                        "air_ptr_b",
                    )?;
                    self.builder
                        .build_int_compare(int_pred, ai, bi, "air_ptr_condition")?
                }
            }
            _ => return Err(anyhow!("unsupported AIR condition operand type")),
        })
    }

    /// Emit one AIR V2 terminator and its CFG edges.
    #[allow(clippy::too_many_arguments)]
    fn emit_air_terminator(
        &mut self,
        _source: &HLFunction,
        air: &AirFunction,
        bid: AirBlockId,
        term: &AirTerminator,
        entries: &[Option<BasicBlock<'ctx>>],
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
    ) -> Result<()> {
        let block = |id: AirBlockId| -> Result<BasicBlock<'ctx>> {
            entries
                .get(id.idx())
                .copied()
                .flatten()
                .ok_or_else(|| anyhow!("AIR branch to missing block b{}", id.0))
        };
        match term {
            AirTerminator::Ret { value } => {
                let function = self
                    .builder
                    .get_insert_block()
                    .and_then(|b| b.get_parent())
                    .ok_or_else(|| anyhow!("AIR terminator has no parent function"))?;
                match function.get_type().get_return_type() {
                    None => {
                        self.builder.build_return(None)?;
                    }
                    Some(ret_type) => {
                        let loaded = self.builder.build_load(
                            reg_types[value.idx()],
                            registers[value.idx()],
                            "air_ret",
                        )?;
                        let value = if loaded.get_type() == ret_type {
                            loaded
                        } else {
                            self.cast_for_call(loaded, ret_type)?
                        };
                        self.builder.build_return(Some(&value))?;
                    }
                }
            }
            AirTerminator::Jump { target } => {
                self.emit_air_phi_edge(air, bid, *target, registers, reg_types)?;
                self.builder.build_unconditional_branch(block(*target)?)?;
            }
            AirTerminator::CondJump {
                cond,
                a,
                b,
                if_true,
                if_false,
            } => {
                let condition =
                    self.emit_air_condition(lowering, *cond, *a, *b, registers, reg_types)?;
                let function = self
                    .builder
                    .get_insert_block()
                    .and_then(|b| b.get_parent())
                    .ok_or_else(|| anyhow!("AIR terminator has no parent function"))?;
                let true_edge = self.context.append_basic_block(function, "air_true_edge");
                let false_edge = self.context.append_basic_block(function, "air_false_edge");
                self.builder
                    .build_conditional_branch(condition, true_edge, false_edge)?;

                self.builder.position_at_end(true_edge);
                self.emit_air_phi_edge(air, bid, *if_true, registers, reg_types)?;
                self.builder.build_unconditional_branch(block(*if_true)?)?;

                self.builder.position_at_end(false_edge);
                self.emit_air_phi_edge(air, bid, *if_false, registers, reg_types)?;
                self.builder.build_unconditional_branch(block(*if_false)?)?;
            }
            AirTerminator::Switch {
                value,
                targets,
                default,
            } => {
                let raw = self.builder.build_load(
                    reg_types[value.idx()],
                    registers[value.idx()],
                    "air_switch",
                )?;
                let value = raw.into_int_value();
                let function = self
                    .builder
                    .get_insert_block()
                    .and_then(|b| b.get_parent())
                    .ok_or_else(|| anyhow!("AIR terminator has no parent function"))?;
                let default_edge = self
                    .context
                    .append_basic_block(function, "air_switch_default");
                let mut case_edges = Vec::with_capacity(targets.len());
                for target in targets {
                    case_edges.push((
                        self.context.append_basic_block(function, "air_switch_case"),
                        *target,
                    ));
                }
                let cases: Vec<_> = case_edges
                    .iter()
                    .enumerate()
                    .map(|(index, (edge, _))| {
                        (value.get_type().const_int(index as u64, false), *edge)
                    })
                    .collect();
                self.builder.build_switch(value, default_edge, &cases)?;

                for (edge, target) in case_edges {
                    self.builder.position_at_end(edge);
                    self.emit_air_phi_edge(air, bid, target, registers, reg_types)?;
                    self.builder.build_unconditional_branch(block(target)?)?;
                }
                self.builder.position_at_end(default_edge);
                self.emit_air_phi_edge(air, bid, *default, registers, reg_types)?;
                self.builder.build_unconditional_branch(block(*default)?)?;
            }
            AirTerminator::Throw { exc } | AirTerminator::Rethrow { exc } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let value = self.builder.build_load(
                    reg_types[exc.idx()],
                    registers[exc.idx()],
                    "air_throw",
                )?;
                let value = if value.get_type() == ptr_type.as_basic_type_enum() {
                    value.into_pointer_value()
                } else {
                    self.cast_for_call(value, ptr_type.into())?
                        .into_pointer_value()
                };
                let throw = self.declare_native("hlp_throw", &[ptr_type.into()], None);
                self.builder
                    .build_call(throw, &[value.into()], "air_throw_call")?;
                self.builder.build_unreachable()?;
            }
            AirTerminator::Trap {
                exc_cell,
                handler,
                normal,
            } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let i32_type = self.context.i32_type();
                let function = self
                    .builder
                    .get_insert_block()
                    .and_then(|b| b.get_parent())
                    .ok_or_else(|| anyhow!("AIR terminator has no parent function"))?;
                let normal_edge = self.context.append_basic_block(function, "air_trap_normal");
                let handler_entry = self
                    .context
                    .append_basic_block(function, "air_trap_handler");

                let setup = self.declare_native("hlp_setup_trap_jit", &[], Some(ptr_type.into()));
                let buf = self
                    .builder
                    .build_call(setup, &[], "air_trap_buf")?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| anyhow!("hlp_setup_trap_jit returned void"))?
                    .into_pointer_value();
                let setjmp_ptr = self.setjmp_ptr()?;
                let setjmp = self.builder.build_indirect_call(
                    i32_type.fn_type(&[ptr_type.into()], false),
                    setjmp_ptr,
                    &[buf.into()],
                    "air_setjmp",
                )?;
                let returns_twice = self.context.create_enum_attribute(
                    inkwell::attributes::Attribute::get_named_enum_kind_id("returns_twice"),
                    0,
                );
                setjmp.add_attribute(inkwell::attributes::AttributeLoc::Function, returns_twice);
                let jumped = setjmp
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| anyhow!("_setjmp returned void"))?
                    .into_int_value();
                let is_exception = self.builder.build_int_compare(
                    IntPredicate::NE,
                    jumped,
                    i32_type.const_zero(),
                    "air_trap_exception",
                )?;
                self.builder
                    .build_conditional_branch(is_exception, handler_entry, normal_edge)?;

                self.builder.position_at_end(normal_edge);
                self.emit_air_phi_edge(air, bid, *normal, registers, reg_types)?;
                self.builder.build_unconditional_branch(block(*normal)?)?;

                self.builder.position_at_end(handler_entry);
                let get_exc = self.declare_native("hlp_get_exc_value", &[], Some(ptr_type.into()));
                let exc = self
                    .builder
                    .build_call(get_exc, &[], "air_exception")?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| anyhow!("hlp_get_exc_value returned void"))?;
                let exc_reg = Reg((cell_base + exc_cell.idx()) as u32);
                let exc_index = exc_reg.0 as usize;
                let exc = if exc.get_type() == reg_types[exc_index] {
                    exc
                } else {
                    self.cast_for_call(exc, reg_types[exc_index])?
                };
                self.builder.build_store(registers[exc_index], exc)?;
                let clear = self.declare_native("hlp_clear_exc_value", &[], None);
                self.builder.build_call(clear, &[], "air_clear_exception")?;
                self.emit_air_phi_edge(air, bid, *handler, registers, reg_types)?;
                self.builder.build_unconditional_branch(block(*handler)?)?;
            }
        }
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
            let descriptor = self
                .c_ptr_to_type_index
                .iter()
                .find_map(|(&ptr, &index)| (index == type_index).then_some(ptr as *mut hl_type))
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
            .c_ptr_to_type_index
            .iter()
            .find_map(|(&ptr, &index)| (index == type_index).then_some(ptr as *mut hl_type))
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

    /// Emit one primitive operation selected by AIR V2.
    ///
    /// This is an instruction emitter, not a bytecode-function lowering
    /// route: AIR owns the CFG, phi edges, values, cells and terminators, and
    /// there is intentionally no method that walks an `HLFunction::ops` body.
    fn translate_opcode(
        &mut self,
        f: &HLFunction,
        op: &Opcode,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        i: usize,
        opcode_blocks: &[BasicBlock<'ctx>],
    ) -> Result<()> {
        match op {
            Opcode::Mov { dst, src } => {
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "src_val",
                )?;
                self.builder.build_store(registers[dst.0 as usize], src_val);
            }
            Opcode::Int { dst, ptr } => {
                let int_val = self
                    .ensure_int_global(ptr.0)
                    .ok_or_else(|| anyhow!("Int constant not found"))?;
                let loaded_int = self.builder.build_load(
                    self.context.i32_type(),
                    int_val.as_pointer_value(),
                    "int_val",
                )?;
                self.builder
                    .build_store(registers[dst.0 as usize], loaded_int);
            }
            Opcode::Float { dst, ptr } => {
                let float_val = self
                    .ensure_float_global(ptr.0)
                    .ok_or_else(|| anyhow!("Float constant not found"))?;
                let loaded_float = self.builder.build_load(
                    self.context.f64_type(),
                    float_val.as_pointer_value(),
                    "float_val",
                )?;
                self.builder
                    .build_store(registers[dst.0 as usize], loaded_float);
            }
            Opcode::Bool { dst, value } => {
                let bool_val = self.context.bool_type().const_int(*value as u64, false);
                self.builder
                    .build_store(registers[dst.0 as usize], bool_val);
            }
            Opcode::String { dst, ptr } => {
                let string_val = self
                    .ensure_string_global(ptr.0)
                    .ok_or_else(|| anyhow!("String constant not found"))?;
                // Store the ADDRESS of the string constant (pointer to first byte)
                self.builder
                    .build_store(registers[dst.0 as usize], string_val.as_pointer_value());
            }
            Opcode::Null { dst } => {
                let null_val = self.context.ptr_type(AddressSpace::default()).const_null();
                self.builder
                    .build_store(registers[dst.0 as usize], null_val);
            }
            Opcode::Add { dst, a, b } => {
                let a_val = self.builder.build_load(
                    reg_types[a.0 as usize],
                    registers[a.0 as usize],
                    "a_val",
                )?;
                let b_val = self.builder.build_load(
                    reg_types[b.0 as usize],
                    registers[b.0 as usize],
                    "b_val",
                )?;
                let result = match (
                    a_val.get_type().as_any_type_enum(),
                    b_val.get_type().as_any_type_enum(),
                ) {
                    (AnyTypeEnum::IntType(_), AnyTypeEnum::IntType(_)) => self
                        .builder
                        .build_int_add(a_val.into_int_value(), b_val.into_int_value(), "add")?
                        .as_any_value_enum()
                        .into_int_value()
                        .as_basic_value_enum(),
                    (AnyTypeEnum::FloatType(_), AnyTypeEnum::FloatType(_)) => {
                        let fv = self.builder.build_float_add(
                            a_val.into_float_value(),
                            b_val.into_float_value(),
                            "add",
                        )?;
                        if let Some(inst) = fv.as_instruction() {
                            inst.set_fast_math_flags(1 << 5);
                        }
                        fv.as_basic_value_enum()
                    }
                    _ => return Err(anyhow!("Unsupported types for Add operation")),
                };
                self.builder.build_store(registers[dst.0 as usize], result);
            }
            Opcode::Sub { dst, a, b } => {
                let a_val = self.builder.build_load(
                    reg_types[a.0 as usize],
                    registers[a.0 as usize],
                    "a_val",
                )?;
                let b_val = self.builder.build_load(
                    reg_types[b.0 as usize],
                    registers[b.0 as usize],
                    "b_val",
                )?;
                let result = match (
                    a_val.get_type().as_any_type_enum(),
                    b_val.get_type().as_any_type_enum(),
                ) {
                    (AnyTypeEnum::IntType(_), AnyTypeEnum::IntType(_)) => self
                        .builder
                        .build_int_sub(a_val.into_int_value(), b_val.into_int_value(), "sub")?
                        .as_basic_value_enum(),
                    (AnyTypeEnum::FloatType(_), AnyTypeEnum::FloatType(_)) => {
                        let fv = self.builder.build_float_sub(
                            a_val.into_float_value(),
                            b_val.into_float_value(),
                            "sub",
                        )?;
                        if let Some(inst) = fv.as_instruction() {
                            inst.set_fast_math_flags(1 << 5);
                        }
                        fv.as_basic_value_enum()
                    }
                    _ => return Err(anyhow!("Unsupported types for Sub operation")),
                };
                self.builder.build_store(registers[dst.0 as usize], result);
            }
            Opcode::Mul { dst, a, b } => {
                let a_val = self.builder.build_load(
                    reg_types[a.0 as usize],
                    registers[a.0 as usize],
                    "a_val",
                )?;
                let b_val = self.builder.build_load(
                    reg_types[b.0 as usize],
                    registers[b.0 as usize],
                    "b_val",
                )?;
                let result = match (
                    a_val.get_type().as_any_type_enum(),
                    b_val.get_type().as_any_type_enum(),
                ) {
                    (AnyTypeEnum::IntType(_), AnyTypeEnum::IntType(_)) => self
                        .builder
                        .build_int_mul(a_val.into_int_value(), b_val.into_int_value(), "mul")?
                        .as_basic_value_enum(),
                    (AnyTypeEnum::FloatType(_), AnyTypeEnum::FloatType(_)) => {
                        let fv = self.builder.build_float_mul(
                            a_val.into_float_value(),
                            b_val.into_float_value(),
                            "mul",
                        )?;
                        if let Some(inst) = fv.as_instruction() {
                            inst.set_fast_math_flags(1 << 5);
                        }
                        fv.as_basic_value_enum()
                    }
                    _ => return Err(anyhow!("Unsupported types for Mul operation")),
                };
                self.builder.build_store(registers[dst.0 as usize], result);
            }
            Opcode::Call0 { dst, fun } => {
                let (function, is_placeholder) = self.get_or_create_function_value(fun.0)?;
                let result = self.builder.build_call(function, &[], "call")?;

                if result.try_as_basic_value().basic().is_some() {
                    self.builder.build_store(
                        registers[dst.0 as usize],
                        result.try_as_basic_value().basic().unwrap(),
                    );
                }

                if is_placeholder {
                    self.add_pending_compilation(fun.0);
                }
            }

            Opcode::Call1 { dst, fun, arg0 } => {
                let arg0_val = self.builder.build_load(
                    reg_types[arg0.0 as usize],
                    registers[arg0.0 as usize],
                    "arg0_val",
                )?;

                // Machine-instruction primitives (Math.sqrt and friends) are
                // emitted here rather than called. Every entry in the table is
                // unary, which is why this is the only call arity that has to
                // check. See crate::intrinsics.
                let inlined = match self.native_intrinsic_for(fun.0) {
                    Some(intr) => self.emit_native_intrinsic(intr, arg0_val)?,
                    None => None,
                };

                if let Some(v) = inlined {
                    self.builder.build_store(registers[dst.0 as usize], v)?;
                } else {
                    let (function, is_placeholder) = self.get_or_create_function_value(fun.0)?;
                    let result = self
                        .builder
                        .build_call(function, &[arg0_val.into()], "call")?;

                    if result.try_as_basic_value().basic().is_some() {
                        self.builder.build_store(
                            registers[dst.0 as usize],
                            result.try_as_basic_value().basic().unwrap(),
                        );
                    }

                    if is_placeholder {
                        self.add_pending_compilation(fun.0);
                    }
                }
            }
            Opcode::Call2 {
                dst,
                fun,
                arg0,
                arg1,
            } => {
                let (function, is_placeholder) = self.get_or_create_function_value(fun.0)?;
                let arg0_val = self.builder.build_load(
                    reg_types[arg0.0 as usize],
                    registers[arg0.0 as usize],
                    "arg0_val",
                )?;
                let arg1_val = self.builder.build_load(
                    reg_types[arg1.0 as usize],
                    registers[arg1.0 as usize],
                    "arg1_val",
                )?;

                let result = self.builder.build_call(
                    function,
                    &[arg0_val.into(), arg1_val.into()],
                    "call",
                )?;

                if result.try_as_basic_value().basic().is_some() {
                    self.builder.build_store(
                        registers[dst.0 as usize],
                        result.try_as_basic_value().basic().unwrap(),
                    );
                }

                if is_placeholder {
                    self.add_pending_compilation(fun.0);
                }
            }
            Opcode::Call3 {
                dst,
                fun,
                arg0,
                arg1,
                arg2,
            } => {
                let (function, is_placeholder) = self.get_or_create_function_value(fun.0)?;
                let arg0_val = self.builder.build_load(
                    reg_types[arg0.0 as usize],
                    registers[arg0.0 as usize],
                    "arg0_val",
                )?;
                let arg1_val = self.builder.build_load(
                    reg_types[arg1.0 as usize],
                    registers[arg1.0 as usize],
                    "arg1_val",
                )?;
                let arg2_val = self.builder.build_load(
                    reg_types[arg2.0 as usize],
                    registers[arg2.0 as usize],
                    "arg2_val",
                )?;
                let result = self.builder.build_call(
                    function,
                    &[arg0_val.into(), arg1_val.into(), arg2_val.into()],
                    "call",
                )?;

                if result.try_as_basic_value().basic().is_some() {
                    self.builder.build_store(
                        registers[dst.0 as usize],
                        result.try_as_basic_value().basic().unwrap(),
                    );
                }

                if is_placeholder {
                    self.add_pending_compilation(fun.0);
                }
            }
            Opcode::Ret { ret } => {
                if let Some(t) = self.types_.get(f.regs[ret.0 as usize].0) {
                    if t.kind == hl_type_kind_HVOID {
                        self.builder.build_return(None);
                        return Ok(());
                    }
                }
                let ret_val = self.builder.build_load(
                    reg_types[ret.0 as usize],
                    registers[ret.0 as usize],
                    "ret_val",
                )?;
                self.builder.build_return(Some(&ret_val));
            }
            Opcode::JTrue { cond, offset } => {
                let cond_val = self.builder.build_load(
                    reg_types[cond.0 as usize],
                    registers[cond.0 as usize],
                    "cond_val",
                )?;
                let target = opcode_blocks[(i as i32 + 1 + *offset) as usize];
                let next = opcode_blocks[i + 1];
                self.builder
                    .build_conditional_branch(cond_val.into_int_value(), target, next)?;
            }
            Opcode::JFalse { cond, offset } => {
                let cond_val = self.builder.build_load(
                    reg_types[cond.0 as usize],
                    registers[cond.0 as usize],
                    "cond_val",
                )?;
                let target = opcode_blocks[(i as i32 + 1 + *offset) as usize];
                let next = opcode_blocks[i + 1];
                self.builder
                    .build_conditional_branch(cond_val.into_int_value(), next, target)?;
            }
            Opcode::JAlways { offset } => {
                let target = opcode_blocks[(i as i32 + 1 + *offset) as usize];
                self.builder.build_unconditional_branch(target)?;
            }
            Opcode::GetType { dst, src } => {
                // GetType reads the runtime hl_type* from the value's ->t field (offset 0)
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "gettype_src",
                )?;
                let obj_ptr = src_val.into_pointer_value();
                // obj->t is the first field (offset 0) of vdynamic/vobj, a pointer to hl_type
                let t_ptr = self
                    .builder
                    .build_load(ptr_type, obj_ptr, "gettype_t")?
                    .into_pointer_value();
                self.builder.build_store(registers[dst.0 as usize], t_ptr)?;
            }

            Opcode::Type { dst, ty } => {
                let typ: BasicValueEnum<'ctx> = self.get_initialized_type(ty.0)?;
                // Store the type info in the destination register
                self.builder.build_store(registers[dst.0 as usize], typ);
            }

            Opcode::New { dst } => {
                let type_index = f.regs.clone()[dst.0 as usize].0;
                // `kind` is Copy; cloning the whole type table to read it was pure waste.
                let type_kind = self.types_[type_index].kind;

                match type_kind {
                    hl_type_kind_HSTRUCT | hl_type_kind_HOBJ => {
                        let type_ = self
                            .initialized_type_cache
                            .get(&type_index)
                            .expect("Expected to get type");
                        let fun = self
                            .func_cache
                            .iter()
                            .find(|(_, f)| {
                                f.get_name().to_string_lossy() == "std_hlp_alloc_obj_caller"
                            })
                            .expect("Expected to find native function hlp_alloc_obj")
                            .1;

                        // type_ is already a pointer constant (inttoptr), pass directly
                        let type_ptr = type_.into_pointer_value();
                        let result = self.builder.build_call(*fun, &[type_ptr.into()], "call")?;
                        self.builder.build_store(
                            registers[dst.0 as usize],
                            result.try_as_basic_value().basic().unwrap(),
                        );
                    }
                    hl_type_kind_HDYNOBJ => {
                        let fun = self.declare_native(
                            "hlp_alloc_dynobj",
                            &[],
                            Some(self.context.ptr_type(AddressSpace::default()).into()),
                        );

                        let result = self.builder.build_call(fun, &[], "call")?;
                        self.builder.build_store(
                            registers[dst.0 as usize],
                            result.try_as_basic_value().basic().unwrap(),
                        );
                    }
                    hl_type_kind_HVIRTUAL => {
                        let type_ = self
                            .initialized_type_cache
                            .get(&type_index)
                            .expect("Expected to get type");
                        let fun = self.declare_native(
                            "hlp_alloc_virtual",
                            &[self.context.ptr_type(AddressSpace::default()).into()],
                            Some(self.context.ptr_type(AddressSpace::default()).into()),
                        );

                        // type_ is already a pointer constant, pass directly
                        let type_ptr = type_.into_pointer_value();
                        let result = self.builder.build_call(fun, &[type_ptr.into()], "call")?;
                        self.builder.build_store(
                            registers[dst.0 as usize],
                            result.try_as_basic_value().basic().unwrap(),
                        );
                    }
                    _ => return Err(anyhow!("Can't call constructor on invalid type")),
                }
            }
            Opcode::SetField { obj, field, src } => {
                let obj_type_ = &self.types_[f.regs[obj.0 as usize].0];
                let obj_val = self
                    .builder
                    .build_load(
                        reg_types[obj.0 as usize],
                        registers[obj.0 as usize],
                        "obj_val",
                    )?
                    .into_pointer_value();
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "src_val",
                )?;

                match obj_type_.kind {
                    hl_type_kind_HSTRUCT | hl_type_kind_HOBJ => {
                        let field_ptr =
                            self.build_field_ptr(f.regs[obj.0 as usize].0, field.0, obj_val)?;
                        let st = self.builder.build_store(field_ptr, src_val)?;
                        self.tbaa_field(Some(st), f.regs[obj.0 as usize].0, field.0);
                    }
                    hl_type_kind_HVIRTUAL => {
                        let ptr_type = self.context.ptr_type(AddressSpace::default());
                        let vvirt_ptr = obj_val;

                        // vfields array starts at offset sizeof(vvirtual) = 24
                        // from the vvirtual pointer (after t, value, next fields)
                        let vfields_ptr = unsafe {
                            self.builder.build_gep(
                                self.context.i8_type(),
                                vvirt_ptr,
                                &[self.context.i64_type().const_int(24, false)],
                                "vfields_ptr",
                            )?
                        };

                        // Get the field pointer: vfields[field] (array of pointers)
                        let field_ptr = unsafe {
                            self.builder.build_gep(
                                ptr_type,
                                vfields_ptr,
                                &[self.context.i32_type().const_int(field.0 as u64, false)],
                                "field_ptr",
                            )?
                        };

                        // Check if the field exists
                        let field_value_ptr =
                            self.builder
                                .build_load(ptr_type, field_ptr, "field_value_ptr")?;
                        let field_exists = self.builder.build_is_not_null(
                            field_value_ptr.into_pointer_value(),
                            "field_exists",
                        )?;

                        let current_fn = self
                            .builder
                            .get_insert_block()
                            .unwrap()
                            .get_parent()
                            .unwrap();
                        let then_block =
                            self.context.append_basic_block(current_fn, "field_exists");
                        let else_block = self
                            .context
                            .append_basic_block(current_fn, "field_not_exists");
                        let cont_block = self.context.append_basic_block(current_fn, "cont");

                        self.builder.build_conditional_branch(
                            field_exists,
                            then_block,
                            else_block,
                        )?;

                        // Field exists: *hl_vfields(o)[f] = v
                        self.builder.position_at_end(then_block);
                        self.builder
                            .build_store(field_value_ptr.into_pointer_value(), src_val)?;
                        self.builder.build_unconditional_branch(cont_block)?;

                        // Field doesn't exist: box value + call hlp_obj_set_field
                        self.builder.position_at_end(else_block);
                        let hashed_name = obj_type_
                            .virt
                            .as_ref()
                            .map(|v| v.fields.get(field.0).map(|f| f.hashed_name).unwrap_or(0))
                            .unwrap_or(0);
                        let field_hash =
                            self.context.i32_type().const_int(hashed_name as u64, false);
                        let src_type_idx = f.regs[src.0 as usize].0;
                        let src_kind = self.types_[src_type_idx].kind;

                        // Box the value to a vdynamic* via hlp_make_dyn
                        let boxed_val = if src_kind == hl_type_kind_HI32
                            || src_kind == hl_type_kind_HBOOL
                            || src_kind == hl_type_kind_HUI8
                            || src_kind == hl_type_kind_HUI16
                            || src_kind == hl_type_kind_HF32
                            || src_kind == hl_type_kind_HF64
                            || src_kind == hl_type_kind_HI64
                        {
                            // Store value to a temp alloca, pass its address
                            let tmp = self
                                .builder
                                .build_alloca(reg_types[src.0 as usize], "tmp_box")?;
                            self.builder.build_store(tmp, src_val)?;
                            let type_ptr_val = self
                                .get_initialized_type(src_type_idx)?
                                .into_pointer_value();
                            let make_dyn = self.declare_native(
                                "hlp_make_dyn",
                                &[ptr_type.into(), ptr_type.into()],
                                Some(ptr_type.into()),
                            );
                            self.builder
                                .build_call(
                                    make_dyn,
                                    &[tmp.into(), type_ptr_val.into()],
                                    "boxed_val",
                                )?
                                .try_as_basic_value()
                                .basic()
                                .unwrap()
                                .into_pointer_value()
                        } else {
                            src_val.into_pointer_value()
                        };

                        // Load value (underlying object) from vvirtual offset 8
                        let fb_value_gep = unsafe {
                            self.builder.build_gep(
                                self.context.i8_type(),
                                vvirt_ptr,
                                &[self.context.i64_type().const_int(8, false)],
                                "sf_fb_value_gep",
                            )?
                        };
                        let fb_value_obj =
                            self.builder
                                .build_load(ptr_type, fb_value_gep, "sf_fb_value")?;

                        let obj_set_field = self.declare_native(
                            "hlp_obj_set_field",
                            &[
                                ptr_type.into(),
                                self.context.i32_type().into(),
                                ptr_type.into(),
                            ],
                            None,
                        );
                        self.builder.build_call(
                            obj_set_field,
                            &[fb_value_obj.into(), field_hash.into(), boxed_val.into()],
                            "dyn_set_result",
                        )?;
                        self.builder.build_unconditional_branch(cont_block)?;

                        // Continue
                        self.builder.position_at_end(cont_block);
                    }
                    _ => return Err(anyhow!("Could not set field of non-object type")),
                }
            }
            Opcode::Field { dst, obj, field } => {
                let obj_type_ = &self.types_[f.regs[obj.0 as usize].0];
                let obj_val = self.builder.build_load(
                    reg_types[obj.0 as usize],
                    registers[obj.0 as usize],
                    "obj_val",
                )?;
                match obj_type_.kind {
                    hl_type_kind_HSTRUCT | hl_type_kind_HOBJ => {
                        let field_ptr = self.build_field_ptr(
                            f.regs[obj.0 as usize].0,
                            field.0,
                            obj_val.into_pointer_value(),
                        )?;

                        // Load the field value using destination register type
                        let load_type = self.get_register_type(f.regs[dst.0 as usize].0)?;
                        let field_val =
                            self.builder.build_load(load_type, field_ptr, "field_val")?;
                        self.tbaa_field(
                            field_val.as_instruction_value(),
                            f.regs[obj.0 as usize].0,
                            field.0,
                        );

                        self.builder
                            .build_store(registers[dst.0 as usize], field_val)?;
                    }
                    hl_type_kind_HVIRTUAL => {
                        let ptr_type = self.context.ptr_type(AddressSpace::default());
                        let vvirt_ptr = obj_val.into_pointer_value();

                        // vfields array starts at offset sizeof(vvirtual) = 24
                        // from the vvirtual pointer (after t, value, next fields)
                        let vfields_ptr = unsafe {
                            self.builder.build_gep(
                                self.context.i8_type(),
                                vvirt_ptr,
                                &[self.context.i64_type().const_int(24, false)],
                                "vfields_ptr",
                            )?
                        };

                        // Get the field pointer: vfields[field] (array of pointers)
                        let field_ptr = unsafe {
                            self.builder.build_gep(
                                ptr_type,
                                vfields_ptr,
                                &[self.context.i32_type().const_int(field.0 as u64, false)],
                                "field_ptr",
                            )?
                        };

                        // Check if the field exists
                        let field_value_check =
                            self.builder
                                .build_load(ptr_type, field_ptr, "field_value_ptr")?;
                        let field_exists = self.builder.build_is_not_null(
                            field_value_check.into_pointer_value(),
                            "field_exists",
                        )?;

                        let current_fn = self
                            .builder
                            .get_insert_block()
                            .unwrap()
                            .get_parent()
                            .unwrap();
                        let then_block =
                            self.context.append_basic_block(current_fn, "field_exists");
                        let else_block = self
                            .context
                            .append_basic_block(current_fn, "field_not_exists");
                        let cont_block = self.context.append_basic_block(current_fn, "cont");

                        self.builder.build_conditional_branch(
                            field_exists,
                            then_block,
                            else_block,
                        )?;

                        // Field exists: r = *hl_vfields(o)[f]
                        self.builder.position_at_end(then_block);
                        let field_value_ptr =
                            self.builder
                                .build_load(ptr_type, field_ptr, "field_value_ptr")?;
                        // Load with the destination register's type, not ptr,
                        // to avoid reading more bytes than the field actually holds.
                        let dst_load_type = reg_types[dst.0 as usize];
                        let field_value = self.builder.build_load(
                            dst_load_type,
                            field_value_ptr.into_pointer_value(),
                            "field_value",
                        )?;
                        self.builder
                            .build_store(registers[dst.0 as usize], field_value)?;
                        self.builder.build_unconditional_branch(cont_block)?;

                        // Field doesn't exist in vfields: fall back to dynamic
                        // field access on the underlying value object
                        self.builder.position_at_end(else_block);
                        let i32_type = self.context.i32_type();
                        // Load value (underlying object) from vvirtual offset 8
                        let value_gep = unsafe {
                            self.builder.build_gep(
                                self.context.i8_type(),
                                vvirt_ptr,
                                &[self.context.i64_type().const_int(8, false)],
                                "fb_value_gep",
                            )?
                        };
                        let value_obj = self.builder.build_load(ptr_type, value_gep, "fb_value")?;
                        let hashed_name = obj_type_
                            .virt
                            .as_ref()
                            .map(|v| v.fields.get(field.0).map(|f| f.hashed_name).unwrap_or(0))
                            .unwrap_or(0);
                        let field_hash = i32_type.const_int(hashed_name as u64, true);
                        let dst_type_idx = f.regs[dst.0 as usize].0;
                        let dst_kind = self.types_[dst_type_idx].kind;
                        let type_ptr = self
                            .get_initialized_type(dst_type_idx)?
                            .into_pointer_value();
                        let getter = self.declare_native(
                            "hlp_dyn_getp",
                            &[ptr_type.into(), i32_type.into(), ptr_type.into()],
                            Some(ptr_type.into()),
                        );
                        let result = self.builder.build_call(
                            getter,
                            &[value_obj.into(), field_hash.into(), type_ptr.into()],
                            "dyn_get_fb",
                        )?;
                        let dyn_field_value = result.try_as_basic_value().basic().unwrap();
                        self.builder
                            .build_store(registers[dst.0 as usize], dyn_field_value)?;
                        self.builder.build_unconditional_branch(cont_block)?;

                        // Continue
                        self.builder.position_at_end(cont_block);
                    }
                    _ => return Err(anyhow!("Could not get field of non-object type")),
                }
            }

            Opcode::GetGlobal { dst, global } => {
                let global_ptr = *self
                    .globals
                    .get(&global.0)
                    .expect("Expected to get global value");

                // All globals are pointer-sized slots, load as ptr
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let val = self
                    .builder
                    .build_load(ptr_type, global_ptr, "global_load")?;
                self.builder.build_store(registers[dst.0 as usize], val);
            }
            Opcode::SetGlobal { global, src } => {
                let global_ptr = *self
                    .globals
                    .get(&global.0)
                    .expect("Expected to get global value");

                // Load the value from the register, then store into global
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "src_val",
                )?;
                self.builder.build_store(global_ptr, src_val);
            }
            Opcode::GetArray { dst, array, index } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let i32_type = self.context.i32_type();
                let i8_type = self.context.i8_type();

                let arr = self
                    .builder
                    .build_load(ptr_type, registers[array.0 as usize], "getarr_ptr")?
                    .into_pointer_value();
                let idx = self
                    .builder
                    .build_load(i32_type, registers[index.0 as usize], "getarr_idx")?
                    .into_int_value();

                // Data starts at offset 24 (sizeof(varray))
                let data_ptr = unsafe {
                    self.builder.build_gep(
                        i8_type,
                        arr,
                        &[self.context.i64_type().const_int(24, false)],
                        "getarr_data",
                    )?
                };

                // Element size from the destination register's
                // kind, via the table `crate::layout` shares with the Cranelift
                // tier so the two cannot index an array differently.
                let dst_type_idx = f.regs[dst.0 as usize].0;
                let dst_kind = self.types_[dst_type_idx].kind;
                let elem_size: u64 = crate::layout::array_elem_size(dst_kind) as u64;

                let elem_size_val = i32_type.const_int(elem_size, false);
                let byte_offset =
                    self.builder
                        .build_int_mul(idx, elem_size_val, "getarr_offset")?;
                let slot = unsafe {
                    self.builder
                        .build_gep(i8_type, data_ptr, &[byte_offset], "getarr_slot")?
                };
                let element_val =
                    self.builder
                        .build_load(reg_types[dst.0 as usize], slot, "getarr_val")?;
                if let Some(i) = element_val.as_instruction_value() {
                    self.tbaa.tag(i, self.tbaa.payload());
                }
                self.builder
                    .build_store(registers[dst.0 as usize], element_val)?;
            }

            // --- Control flow: Label, Nop ---
            Opcode::Label | Opcode::Nop => {
                // No-op: fallthrough handled by outer loop
            }

            // --- NullCheck ---
            Opcode::NullCheck { reg } => {
                let val = self.builder.build_load(
                    reg_types[reg.0 as usize],
                    registers[reg.0 as usize],
                    "null_check",
                )?;
                if val.is_pointer_value() {
                    let is_null = self
                        .builder
                        .build_is_null(val.into_pointer_value(), "is_null")?;
                    let function = self
                        .builder
                        .get_insert_block()
                        .unwrap()
                        .get_parent()
                        .unwrap();
                    let throw_block = self.context.append_basic_block(function, "null_throw");
                    self.builder.build_conditional_branch(
                        is_null,
                        throw_block,
                        opcode_blocks[i + 1],
                    )?;
                    self.builder.position_at_end(throw_block);
                    self.builder.build_unreachable()?;
                }
                // Non-pointer types are never null, fall through
            }

            // --- Null/NotNull jumps ---
            Opcode::JNull { reg, offset } => {
                let val = self.builder.build_load(
                    reg_types[reg.0 as usize],
                    registers[reg.0 as usize],
                    "jnull_val",
                )?;
                let target = opcode_blocks[(i as i32 + 1 + *offset) as usize];
                let next = opcode_blocks[i + 1];
                if val.is_pointer_value() {
                    let is_null = self
                        .builder
                        .build_is_null(val.into_pointer_value(), "is_null")?;
                    self.builder
                        .build_conditional_branch(is_null, target, next)?;
                } else {
                    // Non-pointer types are never null
                    self.builder.build_unconditional_branch(next)?;
                }
            }
            Opcode::JNotNull { reg, offset } => {
                let val = self.builder.build_load(
                    reg_types[reg.0 as usize],
                    registers[reg.0 as usize],
                    "jnotnull_val",
                )?;
                let target = opcode_blocks[(i as i32 + 1 + *offset) as usize];
                let next = opcode_blocks[i + 1];
                if val.is_pointer_value() {
                    let is_not_null = self
                        .builder
                        .build_is_not_null(val.into_pointer_value(), "is_not_null")?;
                    self.builder
                        .build_conditional_branch(is_not_null, target, next)?;
                } else {
                    // Non-pointer types are always not-null
                    self.builder.build_unconditional_branch(target)?;
                }
            }

            // --- Comparison jumps ---
            Opcode::JSLt { a, b, offset } => {
                let a_kind = self.types_[f.regs[a.0 as usize].0].kind;
                self.emit_comparison_jump(
                    registers,
                    reg_types,
                    a,
                    b,
                    a_kind,
                    IntPredicate::SLT,
                    FloatPredicate::OLT,
                    i,
                    *offset,
                    opcode_blocks,
                )?;
            }
            Opcode::JSGte { a, b, offset } => {
                let a_kind = self.types_[f.regs[a.0 as usize].0].kind;
                self.emit_comparison_jump(
                    registers,
                    reg_types,
                    a,
                    b,
                    a_kind,
                    IntPredicate::SGE,
                    FloatPredicate::OGE,
                    i,
                    *offset,
                    opcode_blocks,
                )?;
            }
            Opcode::JSGt { a, b, offset } => {
                let a_kind = self.types_[f.regs[a.0 as usize].0].kind;
                self.emit_comparison_jump(
                    registers,
                    reg_types,
                    a,
                    b,
                    a_kind,
                    IntPredicate::SGT,
                    FloatPredicate::OGT,
                    i,
                    *offset,
                    opcode_blocks,
                )?;
            }
            Opcode::JSLte { a, b, offset } => {
                let a_kind = self.types_[f.regs[a.0 as usize].0].kind;
                self.emit_comparison_jump(
                    registers,
                    reg_types,
                    a,
                    b,
                    a_kind,
                    IntPredicate::SLE,
                    FloatPredicate::OLE,
                    i,
                    *offset,
                    opcode_blocks,
                )?;
            }
            Opcode::JULt { a, b, offset } => {
                let a_kind = self.types_[f.regs[a.0 as usize].0].kind;
                self.emit_comparison_jump(
                    registers,
                    reg_types,
                    a,
                    b,
                    a_kind,
                    IntPredicate::ULT,
                    FloatPredicate::OLT,
                    i,
                    *offset,
                    opcode_blocks,
                )?;
            }
            Opcode::JUGte { a, b, offset } => {
                let a_kind = self.types_[f.regs[a.0 as usize].0].kind;
                self.emit_comparison_jump(
                    registers,
                    reg_types,
                    a,
                    b,
                    a_kind,
                    IntPredicate::UGE,
                    FloatPredicate::OGE,
                    i,
                    *offset,
                    opcode_blocks,
                )?;
            }
            Opcode::JNotLt { a, b, offset } => {
                // !(a < b) is the same as a >= b
                let a_kind = self.types_[f.regs[a.0 as usize].0].kind;
                self.emit_comparison_jump(
                    registers,
                    reg_types,
                    a,
                    b,
                    a_kind,
                    IntPredicate::SGE,
                    FloatPredicate::OGE,
                    i,
                    *offset,
                    opcode_blocks,
                )?;
            }
            Opcode::JNotGte { a, b, offset } => {
                // !(a >= b) is the same as a < b
                let a_kind = self.types_[f.regs[a.0 as usize].0].kind;
                self.emit_comparison_jump(
                    registers,
                    reg_types,
                    a,
                    b,
                    a_kind,
                    IntPredicate::SLT,
                    FloatPredicate::OLT,
                    i,
                    *offset,
                    opcode_blocks,
                )?;
            }
            Opcode::JEq { a, b, offset } => {
                let a_kind = self.types_[f.regs[a.0 as usize].0].kind;
                self.emit_comparison_jump(
                    registers,
                    reg_types,
                    a,
                    b,
                    a_kind,
                    IntPredicate::EQ,
                    FloatPredicate::OEQ,
                    i,
                    *offset,
                    opcode_blocks,
                )?;
            }
            Opcode::JNotEq { a, b, offset } => {
                let a_kind = self.types_[f.regs[a.0 as usize].0].kind;
                self.emit_comparison_jump(
                    registers,
                    reg_types,
                    a,
                    b,
                    a_kind,
                    IntPredicate::NE,
                    FloatPredicate::ONE,
                    i,
                    *offset,
                    opcode_blocks,
                )?;
            }

            // --- Switch ---
            Opcode::Switch { reg, offsets, end } => {
                let val = self
                    .builder
                    .build_load(
                        reg_types[reg.0 as usize],
                        registers[reg.0 as usize],
                        "switch_val",
                    )?
                    .into_int_value();
                let default_target = opcode_blocks[i + 1];
                let cases: Vec<(inkwell::values::IntValue<'ctx>, BasicBlock<'ctx>)> = offsets
                    .iter()
                    .enumerate()
                    .map(|(case_idx, off)| {
                        let case_val = self.context.i32_type().const_int(case_idx as u64, false);
                        let target = opcode_blocks[(i as i32 + 1 + *off) as usize];
                        (case_val, target)
                    })
                    .collect();
                self.builder.build_switch(val, default_target, &cases)?;
            }

            // --- Remaining arithmetic ---
            Opcode::SDiv { dst, a, b } => {
                self.emit_binary_op(registers, reg_types, dst, a, b, "sdiv", |b, av, bv| match (
                    av.get_type().as_any_type_enum(),
                    bv.get_type().as_any_type_enum(),
                ) {
                    (AnyTypeEnum::IntType(_), AnyTypeEnum::IntType(_)) => Ok(b
                        .build_int_signed_div(av.into_int_value(), bv.into_int_value(), "sdiv")?
                        .as_basic_value_enum()),
                    (AnyTypeEnum::FloatType(_), AnyTypeEnum::FloatType(_)) => {
                        let fv = b.build_float_div(
                            av.into_float_value(),
                            bv.into_float_value(),
                            "sdiv",
                        )?;
                        if let Some(inst) = fv.as_instruction() {
                            inst.set_fast_math_flags(1 << 5);
                        }
                        Ok(fv.as_basic_value_enum())
                    }
                    _ => Err(anyhow!("Unsupported types for SDiv")),
                })?;
            }
            Opcode::UDiv { dst, a, b } => {
                self.emit_binary_op(registers, reg_types, dst, a, b, "udiv", |b, av, bv| match (
                    av.get_type().as_any_type_enum(),
                    bv.get_type().as_any_type_enum(),
                ) {
                    (AnyTypeEnum::IntType(_), AnyTypeEnum::IntType(_)) => Ok(b
                        .build_int_unsigned_div(av.into_int_value(), bv.into_int_value(), "udiv")?
                        .as_basic_value_enum()),
                    _ => Err(anyhow!("Unsupported types for UDiv")),
                })?;
            }
            Opcode::SMod { dst, a, b } => {
                self.emit_binary_op(registers, reg_types, dst, a, b, "smod", |b, av, bv| match (
                    av.get_type().as_any_type_enum(),
                    bv.get_type().as_any_type_enum(),
                ) {
                    (AnyTypeEnum::IntType(_), AnyTypeEnum::IntType(_)) => Ok(b
                        .build_int_signed_rem(av.into_int_value(), bv.into_int_value(), "smod")?
                        .as_basic_value_enum()),
                    (AnyTypeEnum::FloatType(_), AnyTypeEnum::FloatType(_)) => {
                        let fv = b.build_float_rem(
                            av.into_float_value(),
                            bv.into_float_value(),
                            "smod",
                        )?;
                        if let Some(inst) = fv.as_instruction() {
                            inst.set_fast_math_flags(1 << 5);
                        }
                        Ok(fv.as_basic_value_enum())
                    }
                    _ => Err(anyhow!("Unsupported types for SMod")),
                })?;
            }
            Opcode::UMod { dst, a, b } => {
                self.emit_binary_op(registers, reg_types, dst, a, b, "umod", |b, av, bv| match (
                    av.get_type().as_any_type_enum(),
                    bv.get_type().as_any_type_enum(),
                ) {
                    (AnyTypeEnum::IntType(_), AnyTypeEnum::IntType(_)) => Ok(b
                        .build_int_unsigned_rem(av.into_int_value(), bv.into_int_value(), "umod")?
                        .as_basic_value_enum()),
                    _ => Err(anyhow!("Unsupported types for UMod")),
                })?;
            }
            Opcode::Shl { dst, a, b } => {
                self.emit_binary_op(registers, reg_types, dst, a, b, "shl", |b, av, bv| {
                    Ok(
                        b.build_left_shift(av.into_int_value(), bv.into_int_value(), "shl")?
                            .as_basic_value_enum(),
                    )
                })?;
            }
            Opcode::SShr { dst, a, b } => {
                self.emit_binary_op(registers, reg_types, dst, a, b, "sshr", |b, av, bv| {
                    Ok(
                        b.build_right_shift(
                            av.into_int_value(),
                            bv.into_int_value(),
                            true,
                            "sshr",
                        )?
                        .as_basic_value_enum(),
                    )
                })?;
            }
            Opcode::UShr { dst, a, b } => {
                self.emit_binary_op(registers, reg_types, dst, a, b, "ushr", |b, av, bv| {
                    Ok(b.build_right_shift(
                        av.into_int_value(),
                        bv.into_int_value(),
                        false,
                        "ushr",
                    )?
                    .as_basic_value_enum())
                })?;
            }
            Opcode::And { dst, a, b } => {
                self.emit_binary_op(registers, reg_types, dst, a, b, "and", |b, av, bv| {
                    Ok(
                        b.build_and(av.into_int_value(), bv.into_int_value(), "and")?
                            .as_basic_value_enum(),
                    )
                })?;
            }
            Opcode::Or { dst, a, b } => {
                self.emit_binary_op(registers, reg_types, dst, a, b, "or", |b, av, bv| {
                    Ok(b.build_or(av.into_int_value(), bv.into_int_value(), "or")?
                        .as_basic_value_enum())
                })?;
            }
            Opcode::Xor { dst, a, b } => {
                self.emit_binary_op(registers, reg_types, dst, a, b, "xor", |b, av, bv| {
                    Ok(
                        b.build_xor(av.into_int_value(), bv.into_int_value(), "xor")?
                            .as_basic_value_enum(),
                    )
                })?;
            }
            Opcode::Neg { dst, src } => {
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "neg_src",
                )?;
                let result = match src_val.get_type().as_any_type_enum() {
                    AnyTypeEnum::IntType(_) => self
                        .builder
                        .build_int_neg(src_val.into_int_value(), "neg")?
                        .as_basic_value_enum(),
                    AnyTypeEnum::FloatType(_) => self
                        .builder
                        .build_float_neg(src_val.into_float_value(), "neg")?
                        .as_basic_value_enum(),
                    _ => return Err(anyhow!("Unsupported type for Neg")),
                };
                self.builder
                    .build_store(registers[dst.0 as usize], result)?;
            }
            Opcode::Not { dst, src } => {
                let src_val = self
                    .builder
                    .build_load(
                        reg_types[src.0 as usize],
                        registers[src.0 as usize],
                        "not_src",
                    )?
                    .into_int_value();
                let result = self.builder.build_not(src_val, "not")?;
                self.builder
                    .build_store(registers[dst.0 as usize], result)?;
            }
            Opcode::Incr { dst } => {
                let val = self
                    .builder
                    .build_load(
                        reg_types[dst.0 as usize],
                        registers[dst.0 as usize],
                        "incr_val",
                    )?
                    .into_int_value();
                let one = val.get_type().const_int(1, false);
                let result = self.builder.build_int_add(val, one, "incr")?;
                self.builder
                    .build_store(registers[dst.0 as usize], result)?;
            }
            Opcode::Decr { dst } => {
                let val = self
                    .builder
                    .build_load(
                        reg_types[dst.0 as usize],
                        registers[dst.0 as usize],
                        "decr_val",
                    )?
                    .into_int_value();
                let one = val.get_type().const_int(1, false);
                let result = self.builder.build_int_sub(val, one, "decr")?;
                self.builder
                    .build_store(registers[dst.0 as usize], result)?;
            }

            // --- Call4, CallN ---
            Opcode::Call4 {
                dst,
                fun,
                arg0,
                arg1,
                arg2,
                arg3,
            } => {
                let (function, is_placeholder) = self.get_or_create_function_value(fun.0)?;
                let args: Vec<BasicMetadataValueEnum> = [arg0, arg1, arg2, arg3]
                    .iter()
                    .map(|arg| {
                        self.builder
                            .build_load(
                                reg_types[arg.0 as usize],
                                registers[arg.0 as usize],
                                "arg_val",
                            )
                            .unwrap()
                            .into()
                    })
                    .collect();
                let result = self.builder.build_call(function, &args, "call")?;
                if let Some(ret_val) = result.try_as_basic_value().basic() {
                    self.builder
                        .build_store(registers[dst.0 as usize], ret_val)?;
                }
                if is_placeholder {
                    self.add_pending_compilation(fun.0);
                }
            }
            Opcode::CallN { dst, fun, args } => {
                let (function, is_placeholder) = self.get_or_create_function_value(fun.0)?;
                let arg_vals: Vec<BasicMetadataValueEnum> = args
                    .iter()
                    .map(|arg| {
                        self.builder
                            .build_load(
                                reg_types[arg.0 as usize],
                                registers[arg.0 as usize],
                                "arg_val",
                            )
                            .unwrap()
                            .into()
                    })
                    .collect();
                let result = self.builder.build_call(function, &arg_vals, "call")?;
                if let Some(ret_val) = result.try_as_basic_value().basic() {
                    self.builder
                        .build_store(registers[dst.0 as usize], ret_val)?;
                }
                if is_placeholder {
                    self.add_pending_compilation(fun.0);
                }
            }

            // --- IndirectCall: dispatch through functions_ptrs[findex] ---
            //
            // Emitted by the AIR IndirectCallRewritePass for hot-reload support.
            // Loads the callee address from the mutable function pointer table at
            // runtime, so recompiled functions are picked up without recompiling
            // the caller.
            Opcode::IndirectCall { dst, fun, args } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());

                // Build the ABI type directly from bytecode. Creating an LLVM
                // declaration here makes an isolated lazy module report an
                // unresolved Fun_* symbol even though the call itself goes
                // exclusively through functions_ptrs.
                let callee = self
                    .bytecode
                    .functions
                    .iter()
                    .find(|f| f.findex as usize == fun.0)
                    .ok_or_else(|| anyhow!("IndirectCall target {} is not bytecode", fun.0))?;
                let type_fun = self.bytecode.types[callee.type_.0]
                    .fun
                    .clone()
                    .ok_or_else(|| anyhow!("IndirectCall target {} has no function type", fun.0))?;
                let fn_type = self.create_function_type(&type_fun)?;

                // Load callee address from functions_ptrs[findex] at runtime
                let findex = fun.0;
                let fun_addr_ptr = self.function_slot_ptr(findex)?;
                let fun_addr = self
                    .builder
                    .build_load(ptr_type, fun_addr_ptr, "indirect_call_fn")?
                    .into_pointer_value();

                // Build argument values
                let arg_vals: Vec<BasicMetadataValueEnum> = args
                    .iter()
                    .map(|arg| {
                        self.builder
                            .build_load(
                                reg_types[arg.0 as usize],
                                registers[arg.0 as usize],
                                "arg_val",
                            )
                            .unwrap()
                            .into()
                    })
                    .collect();

                // Indirect call through the loaded pointer (stub-guarded:
                // functions_ptrs may hold interpreter sentinels in hybrid mode)
                if let Some(ret_val) =
                    self.build_stub_guarded_indirect_call(fn_type, fun_addr, &arg_vals, "icall")?
                {
                    self.builder
                        .build_store(registers[dst.0 as usize], ret_val)?;
                }
            }

            // --- GetThis / SetThis (delegate to Field/SetField with obj = reg 0) ---
            Opcode::GetThis { dst, field } => {
                let rewritten = Opcode::Field {
                    dst: *dst,
                    obj: crate::opcodes::Reg(0),
                    field: *field,
                };
                self.translate_opcode(f, &rewritten, registers, reg_types, i, opcode_blocks)?;
            }
            Opcode::SetThis { field, src } => {
                let rewritten = Opcode::SetField {
                    obj: crate::opcodes::Reg(0),
                    field: *field,
                    src: *src,
                };
                self.translate_opcode(f, &rewritten, registers, reg_types, i, opcode_blocks)?;
            }

            // --- CallMethod (compile-time proto resolution, runtime vtable for virtuals) ---
            Opcode::CallMethod { dst, field, args } => {
                let obj_type_idx = f.regs[args[0].0 as usize].0;
                let obj_type = &self.types_[obj_type_idx];
                let ptr_type = self.context.ptr_type(AddressSpace::default());

                if obj_type.kind == hl_type_kind_HVIRTUAL {
                    // Compile-time hash of the virtual field name, for the
                    // dynamic fallback (which resolves by hash, not slot).
                    let field_hash = obj_type
                        .virt
                        .as_ref()
                        .and_then(|v| v.fields.get(field.0))
                        .map(|fld| hl_hash_utf8(&fld.name))
                        .unwrap_or(0);

                    // HVIRTUAL dispatch: load function pointer from vfields[field]
                    let vvirt = self
                        .builder
                        .build_load(ptr_type, registers[args[0].0 as usize], "vvirt")?
                        .into_pointer_value();

                    let function = self
                        .builder
                        .get_insert_block()
                        .unwrap()
                        .get_parent()
                        .unwrap();
                    let nonnull_block = self.context.append_basic_block(function, "vcall_nonnull");
                    let vfields_block = self.context.append_basic_block(function, "vcall_vfields");
                    let direct_block = self.context.append_basic_block(function, "vcall_direct");
                    let fallback_block =
                        self.context.append_basic_block(function, "vcall_fallback");
                    let merge_block = self.context.append_basic_block(function, "vcall_merge");

                    // Runtime guard: at the hybrid interpreter/JIT boundary an
                    // HVIRTUAL-typed register can hold a plain HOBJ/HDYNOBJ
                    // pointer (the interpreter is dynamically typed). Only
                    // trust the vvirtual layout after checking the header's
                    // type kind; null or non-virtual goes to the hash-based
                    // fallback helper. Trusting the static type here read
                    // hl_type_obj ints as a vfields pointer — a deterministic
                    // SIGBUS at 0x2d00000058 on game.hl.
                    let vvirt_null = self.builder.build_is_null(vvirt, "vvirt_null")?;
                    self.builder.build_conditional_branch(
                        vvirt_null,
                        fallback_block,
                        nonnull_block,
                    )?;

                    self.builder.position_at_end(nonnull_block);
                    let hdr_type = self
                        .builder
                        .build_load(ptr_type, vvirt, "vvirt_type")?
                        .into_pointer_value();
                    let hdr_kind = self
                        .builder
                        .build_load(self.context.i32_type(), hdr_type, "vvirt_kind")?
                        .into_int_value();
                    let is_virt = self.builder.build_int_compare(
                        IntPredicate::EQ,
                        hdr_kind,
                        self.context
                            .i32_type()
                            .const_int(hl_type_kind_HVIRTUAL as u64, false),
                        "vvirt_is_virtual",
                    )?;
                    self.builder.build_conditional_branch(
                        is_virt,
                        vfields_block,
                        fallback_block,
                    )?;

                    // --- vfields path: a real vvirtual; try the resolved slot ---
                    self.builder.position_at_end(vfields_block);
                    // Load value (underlying object) from vvirtual offset 8
                    let value_gep = unsafe {
                        self.builder.build_gep(
                            self.context.i8_type(),
                            vvirt,
                            &[self.context.i64_type().const_int(8, false)],
                            "vvirt_value_gep",
                        )?
                    };
                    let value = self
                        .builder
                        .build_load(ptr_type, value_gep, "vvirt_value")?
                        .into_pointer_value();

                    // Load vfields[field] from vvirtual offset 24 + field*8
                    let vfield_offset = 24 + field.0 as u64 * 8;
                    let vfield_gep = unsafe {
                        self.builder.build_gep(
                            self.context.i8_type(),
                            vvirt,
                            &[self.context.i64_type().const_int(vfield_offset, false)],
                            "vfield_gep",
                        )?
                    };
                    let fn_ptr = self
                        .builder
                        .build_load(ptr_type, vfield_gep, "vfield_fn")?
                        .into_pointer_value();

                    // Check if vfield is null (type mismatch — need dynamic fallback)
                    let is_null = self.builder.build_is_null(fn_ptr, "vfield_null")?;
                    self.builder
                        .build_conditional_branch(is_null, fallback_block, direct_block)?;

                    // Look up virtual field's declared function type to get correct param types.
                    // Extract type indices first to avoid borrow conflicts with self.
                    let virt_fn_info: Option<(Vec<usize>, usize)> = obj_type
                        .virt
                        .as_ref()
                        .and_then(|v| v.fields.get(field.0))
                        .and_then(|fld| {
                            let ft = &self.types_[fld.type_.0];
                            if ft.kind == hl_type_kind_HFUN {
                                ft.fun.as_ref().map(|fun| {
                                    let arg_indices: Vec<usize> =
                                        fun.args.iter().map(|a| a.0).collect();
                                    (arg_indices, fun.ret.0)
                                })
                            } else {
                                None
                            }
                        });

                    // Convert type indices to LLVM types (now safe to call get_register_type)
                    let virt_fn_args: Option<Vec<BasicTypeEnum>> =
                        if let Some((ref arg_indices, _)) = virt_fn_info {
                            let mut types = vec![ptr_type.as_basic_type_enum()];
                            for &idx in arg_indices {
                                types.push(self.get_register_type(idx).unwrap_or(ptr_type.into()));
                            }
                            Some(types)
                        } else {
                            None
                        };
                    let virt_ret_type: Option<BasicTypeEnum> =
                        if let Some((_, ret_idx)) = virt_fn_info {
                            Some(self.get_register_type(ret_idx).unwrap_or(ptr_type.into()))
                        } else {
                            None
                        };

                    // Build fn_type from the virtual's declared types (not register types)
                    let mut arg_types: Vec<BasicMetadataTypeEnum> = Vec::with_capacity(args.len());
                    if let Some(ref fn_args) = virt_fn_args {
                        for t in fn_args.iter() {
                            arg_types.push((*t).into());
                        }
                    } else {
                        arg_types.push(ptr_type.into());
                        for arg in &args[1..] {
                            arg_types.push(reg_types[arg.0 as usize].into());
                        }
                    }
                    let dst_kind = self.types_[f.regs[dst.0 as usize].0].kind;
                    let ret_type = virt_ret_type;
                    let fn_type = if dst_kind == hl_type_kind_HVOID {
                        self.context.void_type().fn_type(&arg_types, false)
                    } else if let Some(rt) = ret_type {
                        rt.fn_type(&arg_types, false)
                    } else {
                        reg_types[dst.0 as usize].fn_type(&arg_types, false)
                    };

                    // Emit the tail (non-this) argument loads with casts to the
                    // declared param types, in the current insert block.
                    let build_tail_args =
                        |this: &JITModule<'ctx>| -> Result<Vec<BasicMetadataValueEnum<'ctx>>> {
                            let mut vals: Vec<BasicMetadataValueEnum> =
                                Vec::with_capacity(args.len().saturating_sub(1));
                            for (idx, arg) in args[1..].iter().enumerate() {
                                let loaded = this.builder.build_load(
                                    reg_types[arg.0 as usize],
                                    registers[arg.0 as usize],
                                    "arg_val",
                                )?;
                                // Cast to match the declared function param type
                                if let Some(ref fn_args) = virt_fn_args {
                                    let param_idx = idx + 1; // +1 for 'this'
                                    if param_idx < fn_args.len() {
                                        let expected = fn_args[param_idx];
                                        if loaded.get_type() != expected {
                                            let casted = this.cast_for_call(loaded, expected)?;
                                            vals.push(casted.into());
                                            continue;
                                        }
                                    }
                                }
                                vals.push(loaded.into());
                            }
                            Ok(vals)
                        };

                    // --- Direct path: vfield is resolved, call it (stub-guarded) ---
                    self.builder.position_at_end(direct_block);
                    let mut arg_vals: Vec<BasicMetadataValueEnum> = Vec::with_capacity(args.len());
                    arg_vals.push(value.into());
                    arg_vals.extend(build_tail_args(self)?);
                    if let Some(ret_val) = self.build_stub_guarded_indirect_call(
                        fn_type,
                        fn_ptr,
                        &arg_vals,
                        "vcall_virt",
                    )? {
                        let store_val = if ret_val.get_type() != reg_types[dst.0 as usize] {
                            self.cast_for_call(ret_val, reg_types[dst.0 as usize])?
                        } else {
                            ret_val
                        };
                        self.builder
                            .build_store(registers[dst.0 as usize], store_val)?;
                    }
                    self.builder.build_unconditional_branch(merge_block)?;

                    // --- Fallback path: not a vvirtual, or vfield is null ---
                    // The vfield being null means the DECLARED signature is
                    // not the implementation's: the interface says
                    // Iterator<Int>.next is () -> i32 while the generic
                    // implementation behind it was compiled () -> Dynamic.
                    // Calling the resolved pointer through the declared ABI
                    // reads the low 32 bits of a returned box pointer as the
                    // value (map iteration over Ints yielded truncated
                    // vdynamic addresses). So this path never guesses an ABI:
                    // box every argument by its static register type, let
                    // hlp_vcall_dyn call the method through its OWN runtime
                    // type, and dyn-cast the boxed result to the declared
                    // kind.
                    self.builder.position_at_end(fallback_block);
                    let i32_type = self.context.i32_type();
                    let n_tail = args.len() - 1;
                    let arr_val: BasicValueEnum = if n_tail == 0 {
                        ptr_type.const_null().into()
                    } else {
                        let alloc_arr = self.declare_native(
                            "hlp_alloc_dyn_array",
                            &[i32_type.into()],
                            Some(ptr_type.into()),
                        );
                        let arr = self
                            .builder
                            .build_call(
                                alloc_arr,
                                &[i32_type.const_int(n_tail as u64, false).into()],
                                "vcall_args_arr",
                            )?
                            .try_as_basic_value()
                            .basic()
                            .unwrap()
                            .into_pointer_value();
                        let make_dyn = self.declare_native(
                            "hlp_make_dyn",
                            &[ptr_type.into(), ptr_type.into()],
                            Some(ptr_type.into()),
                        );
                        for (i, arg) in args[1..].iter().enumerate() {
                            let src_type_idx = f.regs[arg.0 as usize].0;
                            let loaded = self.builder.build_load(
                                reg_types[arg.0 as usize],
                                registers[arg.0 as usize],
                                "vcall_arg",
                            )?;
                            // Same boxing rule as ToDyn: pointers are already
                            // dyn-compatible (except HABSTRACT), primitives go
                            // through hlp_make_dyn with their static type.
                            let src_is_abstract =
                                self.types_[src_type_idx].kind == hl_type_kind_HABSTRACT;
                            let boxed: BasicValueEnum =
                                if loaded.is_pointer_value() && !src_is_abstract {
                                    loaded
                                } else {
                                    let temp = self
                                        .builder
                                        .build_alloca(loaded.get_type(), "vcall_box_slot")?;
                                    self.builder.build_store(temp, loaded)?;
                                    let type_ptr = self
                                        .get_initialized_type(src_type_idx)?
                                        .into_pointer_value();
                                    self.builder
                                        .build_call(
                                            make_dyn,
                                            &[temp.into(), type_ptr.into()],
                                            "vcall_box",
                                        )?
                                        .try_as_basic_value()
                                        .basic()
                                        .unwrap()
                                };
                            // varray data starts at offset 24.
                            let slot_gep = unsafe {
                                self.builder.build_gep(
                                    self.context.i8_type(),
                                    arr,
                                    &[self.context.i64_type().const_int(24 + i as u64 * 8, false)],
                                    "vcall_arg_gep",
                                )?
                            };
                            self.builder.build_store(slot_gep, boxed)?;
                        }
                        arr.into()
                    };
                    let vcall = self.declare_native(
                        "hlp_vcall_dyn",
                        &[ptr_type.into(), i32_type.into(), ptr_type.into()],
                        Some(ptr_type.into()),
                    );
                    let hash_val = i32_type.const_int(field_hash as u32 as u64, false);
                    let ret_dyn = self
                        .builder
                        .build_call(
                            vcall,
                            &[vvirt.into(), hash_val.into(), arr_val.into()],
                            "vcall_dyn",
                        )?
                        .try_as_basic_value()
                        .basic()
                        .unwrap()
                        .into_pointer_value();
                    if dst_kind != hl_type_kind_HVOID {
                        let dst_ty = reg_types[dst.0 as usize];
                        let store_val: BasicValueEnum = if dst_ty.is_pointer_type() {
                            // Objects/strings/dynamics ARE their own box; an
                            // unresolved call's null return is dst's null.
                            ret_dyn.into()
                        } else {
                            // Primitive dst: dyn-cast the box (null -> zero,
                            // numeric coercion when the box holds a wider
                            // kind), then narrow to the register width.
                            let (helper, helper_ret): (&str, BasicTypeEnum) =
                                if dst_kind == hl_type_kind_HF64 {
                                    ("hlp_dyn_todouble", self.context.f64_type().into())
                                } else if dst_kind == hl_type_kind_HF32 {
                                    ("hlp_dyn_tofloat", self.context.f32_type().into())
                                } else if dst_kind == hl_type_kind_HI64 {
                                    ("hlp_dyn_toi64", self.context.i64_type().into())
                                } else {
                                    ("hlp_dyn_toint", i32_type.into())
                                };
                            let unbox =
                                self.declare_native(helper, &[ptr_type.into()], Some(helper_ret));
                            let raw = self
                                .builder
                                .build_call(unbox, &[ret_dyn.into()], "vcall_unbox")?
                                .try_as_basic_value()
                                .basic()
                                .unwrap();
                            if raw.get_type() != dst_ty {
                                self.cast_for_call(raw, dst_ty)?
                            } else {
                                raw
                            }
                        };
                        self.builder
                            .build_store(registers[dst.0 as usize], store_val)?;
                    }
                    self.builder.build_unconditional_branch(merge_block)?;

                    // Continue at merge
                    self.builder.position_at_end(merge_block);
                } else if let Some(findex) = obj_type.obj.as_ref().and_then(|obj| {
                    // field.0 is the vtable slot index (vobj_proto index).
                    // Find the proto entry whose pindex matches field.0
                    // to get the findex for the function signature.
                    for p in &obj.proto {
                        if p.pindex as usize == field.0 {
                            return Some(p.findex as usize);
                        }
                    }
                    None
                }) {
                    // Runtime vtable dispatch for HOBJ/HSTRUCT.
                    // field.0 is the vobj_proto slot index.
                    let vtable_slot = field.0 as u64;

                    // Get base function type for constructing the indirect call fn_type
                    let (function, is_placeholder) = self.get_or_create_function_value(findex)?;
                    let param_types: Vec<BasicTypeEnum> = function
                        .get_type()
                        .get_param_types()
                        .into_iter()
                        .map(|t| {
                            BasicTypeEnum::try_from(t)
                                .expect("unsupported metadata param type in method call")
                        })
                        .collect();
                    let fn_type = function.get_type();

                    // Load object pointer
                    let obj_val = self
                        .builder
                        .build_load(ptr_type, registers[args[0].0 as usize], "cm_obj")?
                        .into_pointer_value();

                    // Load hl_type* from object (offset 0)
                    let type_ptr = self
                        .builder
                        .build_load(ptr_type, obj_val, "cm_type")?
                        .into_pointer_value();

                    // Build arg values with type casting (shared by both the
                    // devirtualised and the vtable arm below).
                    let expected_params = function.count_params() as usize;
                    let mut arg_vals: Vec<BasicMetadataValueEnum> =
                        Vec::with_capacity(expected_params);
                    for (idx, arg) in args.iter().enumerate() {
                        if idx >= expected_params {
                            break;
                        }
                        let loaded = self.builder.build_load(
                            reg_types[arg.0 as usize],
                            registers[arg.0 as usize],
                            "arg_val",
                        )?;
                        if idx < param_types.len() {
                            let expected = param_types[idx];
                            if loaded.get_type() != expected {
                                let casted = self.cast_for_call(loaded, expected)?;
                                arg_vals.push(casted.into());
                            } else {
                                arg_vals.push(loaded.into());
                            }
                        } else {
                            arg_vals.push(loaded.into());
                        }
                    }
                    while arg_vals.len() < expected_params {
                        let param_type = param_types[arg_vals.len()];
                        arg_vals.push(param_type.const_zero().into());
                    }

                    // Guarded devirtualisation, same reasoning as CallClosure
                    // below: the interpreter watched this site dispatch, and a
                    // site that only ever saw one receiver type gets a
                    // type-header compare -- the header pointer never moves,
                    // unlike the vtable SLOT, which promotion patches -- and a
                    // direct call the inliner can take. A different receiver
                    // falls into the vtable path unchanged.
                    let devirt = if self.hot_reload {
                        None
                    } else {
                        let caller = f.findex as u32;
                        crate::callsite_profile::method_receiver(caller, i as u32)
                            .or_else(|| crate::callsite_profile::uniform_method_receiver(caller))
                            .and_then(|(type_ptr_c, target)| {
                                match self.get_or_create_function_value(target as usize) {
                                    Ok((callee, ph)) => {
                                        if ph {
                                            self.add_pending_compilation(target as usize);
                                        }
                                        (callee.get_type() == fn_type)
                                            .then_some((callee, type_ptr_c))
                                    }
                                    Err(_) => None,
                                }
                            })
                    };

                    let cm_function = self
                        .builder
                        .get_insert_block()
                        .unwrap()
                        .get_parent()
                        .unwrap();
                    let cm_done_bb = self.context.append_basic_block(cm_function, "cm_done");

                    if let Some((callee, type_ptr_c)) = devirt {
                        crate::profile::count("devirt method fast-arm", 1);
                        // An object's type header is written once, at
                        // allocation, and the object is live for as long as
                        // anything can load through this pointer -- so the
                        // load is `!invariant.load`, which is what lets LICM
                        // hoist the whole guard out of the loop and leave the
                        // fast arm's inlined body running guard-free.
                        if let Some(inst) = type_ptr.as_instruction() {
                            let _ = inst.set_metadata(
                                self.context.metadata_node(&[]),
                                self.context.get_kind_id("invariant.load"),
                            );
                        }
                        let hit_bb = self
                            .context
                            .append_basic_block(cm_function, "cm_devirt_hit");
                        let miss_bb = self
                            .context
                            .append_basic_block(cm_function, "cm_devirt_miss");
                        let type_int = self.builder.build_ptr_to_int(
                            type_ptr,
                            self.context.i64_type(),
                            "cm_type_int",
                        )?;
                        let guard = self.builder.build_int_compare(
                            IntPredicate::EQ,
                            type_int,
                            self.context.i64_type().const_int(type_ptr_c, false),
                            "cm_devirt_guard",
                        )?;
                        self.builder
                            .build_conditional_branch(guard, hit_bb, miss_bb)?;

                        self.builder.position_at_end(hit_bb);
                        let ret = self
                            .builder
                            .build_call(callee, &arg_vals, "cm_devirt_call")?
                            .try_as_basic_value();
                        if let Some(rv) = ret.basic() {
                            self.builder.build_store(registers[dst.0 as usize], rv)?;
                        }
                        self.builder.build_unconditional_branch(cm_done_bb)?;

                        self.builder.position_at_end(miss_bb);
                    }

                    let vobj_proto = self.vobj_proto_ptr(type_ptr)?;

                    // Load method pointer from vobj_proto[field.0]
                    let method_gep = unsafe {
                        self.builder.build_gep(
                            ptr_type,
                            vobj_proto,
                            &[self.context.i32_type().const_int(vtable_slot, false)],
                            "method_gep",
                        )?
                    };
                    let method_ptr = self
                        .builder
                        .build_load(ptr_type, method_gep, "method_ptr")?
                        .into_pointer_value();

                    // Ahead-of-time devirtualisation, from a profile a
                    // previous run left behind.
                    //
                    // The JIT arm above anchors its guard on the receiver's
                    // type header, which it can do because it compiles inside
                    // the process that watched the dispatch. AOT has no such
                    // address, so it guards on the slot it just loaded: if the
                    // vtable resolves to the function the profile named, call
                    // that function directly and let the inliner take it.
                    // Wrong or stale profiles cost one compare.
                    //
                    // The load is `!invariant.load` here and not in the JIT
                    // for a real reason: promotion patches vtable SLOTS, so
                    // there the value genuinely changes. Nothing patches
                    // anything in a finished object, which is what lets LICM
                    // lift the whole guard out of a dispatch loop and leave
                    // the inlined body running without it.
                    let aot_devirt = if self.aot {
                        self.function_name(f.findex as u32)
                            .and_then(|caller| {
                                crate::callsite_profile::aot_target_for(&caller)
                            })
                            .and_then(|target_name| self.findex_for_name(&target_name))
                            .and_then(|target| {
                                match self.get_or_create_function_value(target as usize) {
                                    Ok((callee, ph)) => {
                                        if ph {
                                            self.add_pending_compilation(target as usize);
                                        }
                                        (callee.get_type() == fn_type).then_some(callee)
                                    }
                                    Err(_) => None,
                                }
                            })
                    } else {
                        None
                    };

                    if let Some(callee) = aot_devirt {
                        crate::profile::count("devirt method aot-arm", 1);
                        if let Some(inst) = method_ptr.as_instruction() {
                            let _ = inst.set_metadata(
                                self.context.metadata_node(&[]),
                                self.context.get_kind_id("invariant.load"),
                            );
                        }
                        let hit_bb = self
                            .context
                            .append_basic_block(cm_function, "cm_aot_devirt_hit");
                        let miss_bb = self
                            .context
                            .append_basic_block(cm_function, "cm_aot_devirt_miss");
                        let want = callee.as_global_value().as_pointer_value();
                        let guard = self.builder.build_int_compare(
                            IntPredicate::EQ,
                            self.builder.build_ptr_to_int(
                                method_ptr,
                                self.context.i64_type(),
                                "cm_aot_slot",
                            )?,
                            self.builder.build_ptr_to_int(
                                want,
                                self.context.i64_type(),
                                "cm_aot_want",
                            )?,
                            "cm_aot_devirt_guard",
                        )?;
                        self.builder
                            .build_conditional_branch(guard, hit_bb, miss_bb)?;

                        self.builder.position_at_end(hit_bb);
                        let ret = self
                            .builder
                            .build_call(callee, &arg_vals, "cm_aot_devirt_call")?
                            .try_as_basic_value();
                        if let Some(rv) = ret.basic() {
                            self.builder.build_store(registers[dst.0 as usize], rv)?;
                        }
                        self.builder.build_unconditional_branch(cm_done_bb)?;
                        self.builder.position_at_end(miss_bb);
                    }

                    // Indirect call through the vtable method pointer
                    // (stub-guarded: vobj_proto slots may hold interpreter
                    // sentinels in hybrid mode)
                    if let Some(ret_val) = self.build_stub_guarded_indirect_call(
                        fn_type,
                        method_ptr,
                        &arg_vals,
                        "call_method",
                    )? {
                        self.builder
                            .build_store(registers[dst.0 as usize], ret_val)?;
                    }
                    if is_placeholder {
                        self.add_pending_compilation(findex);
                    }
                    self.builder.build_unconditional_branch(cm_done_bb)?;
                    self.builder.position_at_end(cm_done_bb);
                } else {
                    // Runtime dispatch via hl_runtime_obj.methods table
                    let obj_val = self
                        .builder
                        .build_load(ptr_type, registers[args[0].0 as usize], "vobj")?
                        .into_pointer_value();

                    // Load hl_type* from obj (offset 0)
                    let obj_type_ptr = self
                        .builder
                        .build_load(ptr_type, obj_val, "obj_type")?
                        .into_pointer_value();

                    // Call hlp_get_obj_rt to get hl_runtime_obj*
                    let hl_get_obj_rt = self.declare_native(
                        "hlp_get_obj_rt",
                        &[ptr_type.into()],
                        Some(ptr_type.into()),
                    );
                    let rt_obj = self
                        .builder
                        .build_call(hl_get_obj_rt, &[obj_type_ptr.into()], "rt_obj")?
                        .try_as_basic_value()
                        .basic()
                        .unwrap()
                        .into_pointer_value();

                    // Load methods pointer from hl_runtime_obj (offset 32)
                    let methods_gep = unsafe {
                        self.builder.build_gep(
                            self.context.i8_type(),
                            rt_obj,
                            &[self.context.i64_type().const_int(32, false)],
                            "methods_gep",
                        )?
                    };
                    let methods_ptr = self
                        .builder
                        .build_load(ptr_type, methods_gep, "methods")?
                        .into_pointer_value();

                    // Load function pointer from methods[field]
                    let fn_ptr_gep = unsafe {
                        self.builder.build_gep(
                            ptr_type,
                            methods_ptr,
                            &[self.context.i32_type().const_int(field.0 as u64, false)],
                            "fn_ptr_gep",
                        )?
                    };
                    let fn_ptr = self
                        .builder
                        .build_load(ptr_type, fn_ptr_gep, "fn_ptr")?
                        .into_pointer_value();

                    // Build args and function type
                    let arg_vals: Vec<BasicMetadataValueEnum> = args
                        .iter()
                        .map(|arg| {
                            self.builder
                                .build_load(
                                    reg_types[arg.0 as usize],
                                    registers[arg.0 as usize],
                                    "arg_val",
                                )
                                .unwrap()
                                .into()
                        })
                        .collect();

                    let arg_types: Vec<BasicMetadataTypeEnum> = args
                        .iter()
                        .map(|arg| reg_types[arg.0 as usize].into())
                        .collect();

                    let dst_kind = self.types_[f.regs[dst.0 as usize].0].kind;
                    let fn_type = if dst_kind == hl_type_kind_HVOID {
                        self.context.void_type().fn_type(&arg_types, false)
                    } else {
                        reg_types[dst.0 as usize].fn_type(&arg_types, false)
                    };

                    if let Some(ret_val) =
                        self.build_stub_guarded_indirect_call(fn_type, fn_ptr, &arg_vals, "vcall")?
                    {
                        self.builder
                            .build_store(registers[dst.0 as usize], ret_val)?;
                    }
                }
            }
            // --- CallThis (same as CallMethod HOBJ vtable dispatch, this = reg 0) ---
            Opcode::CallThis { dst, field, args } => {
                let obj_type_idx = f.regs[0].0;
                let ptr_type = self.context.ptr_type(AddressSpace::default());

                // field.0 is the vtable slot index (vobj_proto index).
                // Find the proto entry whose pindex matches field.0 to get the
                // findex for the function signature, walking the super chain
                // since this-calls often resolve to ancestor methods.
                let findex = {
                    let mut found: Option<usize> = None;
                    let mut cur_obj = self.types_[obj_type_idx].obj.as_ref();
                    while let Some(obj) = cur_obj {
                        if let Some(p) = obj.proto.iter().find(|p| p.pindex as usize == field.0) {
                            found = Some(p.findex as usize);
                            break;
                        }
                        cur_obj = obj
                            .super_
                            .as_ref()
                            .and_then(|s| self.types_[s.0].obj.as_ref());
                    }
                    found.ok_or_else(|| {
                        anyhow!(
                            "CallThis: cannot resolve vtable slot {} on type {}",
                            field.0,
                            obj_type_idx
                        )
                    })?
                };

                // Runtime vtable dispatch for HOBJ/HSTRUCT.
                // field.0 is the vobj_proto slot index.
                let vtable_slot = field.0 as u64;

                // Get base function type for constructing the indirect call fn_type
                let (function, is_placeholder) = self.get_or_create_function_value(findex)?;
                let param_types: Vec<BasicTypeEnum> = function
                    .get_type()
                    .get_param_types()
                    .into_iter()
                    .map(|t| {
                        BasicTypeEnum::try_from(t)
                            .expect("unsupported metadata param type in method call")
                    })
                    .collect();
                let fn_type = function.get_type();

                // Load `this` object pointer (reg 0)
                let obj_val = self
                    .builder
                    .build_load(ptr_type, registers[0], "ct_obj")?
                    .into_pointer_value();

                // Load hl_type* from object (offset 0)
                let type_ptr = self
                    .builder
                    .build_load(ptr_type, obj_val, "ct_type")?
                    .into_pointer_value();

                let vobj_proto = self.vobj_proto_ptr(type_ptr)?;

                // Load method pointer from vobj_proto[field.0]
                let method_gep = unsafe {
                    self.builder.build_gep(
                        ptr_type,
                        vobj_proto,
                        &[self.context.i32_type().const_int(vtable_slot, false)],
                        "method_gep",
                    )?
                };
                let method_ptr = self
                    .builder
                    .build_load(ptr_type, method_gep, "method_ptr")?
                    .into_pointer_value();

                // Build arg values with type casting; this (reg 0) comes first
                let expected_params = function.count_params() as usize;
                let mut arg_vals: Vec<BasicMetadataValueEnum> = Vec::with_capacity(expected_params);
                for (idx, reg_idx) in std::iter::once(0usize)
                    .chain(args.iter().map(|arg| arg.0 as usize))
                    .enumerate()
                {
                    if idx >= expected_params {
                        break;
                    }
                    let loaded = self.builder.build_load(
                        reg_types[reg_idx],
                        registers[reg_idx],
                        "arg_val",
                    )?;
                    if idx < param_types.len() {
                        let expected = param_types[idx];
                        if loaded.get_type() != expected {
                            let casted = self.cast_for_call(loaded, expected)?;
                            arg_vals.push(casted.into());
                        } else {
                            arg_vals.push(loaded.into());
                        }
                    } else {
                        arg_vals.push(loaded.into());
                    }
                }
                while arg_vals.len() < expected_params {
                    let param_type = param_types[arg_vals.len()];
                    arg_vals.push(param_type.const_zero().into());
                }

                // Indirect call through the vtable method pointer
                // (stub-guarded: vobj_proto slots may hold interpreter
                // sentinels in hybrid mode)
                if let Some(ret_val) = self.build_stub_guarded_indirect_call(
                    fn_type,
                    method_ptr,
                    &arg_vals,
                    "call_this",
                )? {
                    self.builder
                        .build_store(registers[dst.0 as usize], ret_val)?;
                }
                if is_placeholder {
                    self.add_pending_compilation(findex);
                }
            }

            // --- ToDyn ---
            Opcode::ToDyn { dst, src } => {
                let src_type_idx = f.regs[src.0 as usize].0;
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "todyn_src",
                )?;
                // For pointer types (objects, strings, etc.), just copy the pointer.
                // HABSTRACT is excepted: it is a pointer whose target has no
                // hl_type header, so a Dynamic holding it raw makes the
                // hl_dyn_castp on the way back out read the payload as a type.
                let src_is_abstract = self.types_[src_type_idx].kind == hl_type_kind_HABSTRACT;
                if src_val.is_pointer_value() && !src_is_abstract {
                    self.builder
                        .build_store(registers[dst.0 as usize], src_val)?;
                } else {
                    // Primitives: alloca temp, store value, call hlp_make_dyn(&temp, type_ptr)
                    let ptr_type = self.context.ptr_type(AddressSpace::default());
                    let temp = self
                        .builder
                        .build_alloca(reg_types[src.0 as usize], "todyn_temp")?;
                    self.builder.build_store(temp, src_val)?;

                    let type_ptr = self
                        .get_initialized_type(src_type_idx)?
                        .into_pointer_value();
                    let make_dyn = self.declare_native(
                        "hlp_make_dyn",
                        &[ptr_type.into(), ptr_type.into()],
                        Some(ptr_type.into()),
                    );
                    let result = self.builder.build_call(
                        make_dyn,
                        &[temp.into(), type_ptr.into()],
                        "todyn",
                    )?;
                    self.builder.build_store(
                        registers[dst.0 as usize],
                        result.try_as_basic_value().basic().unwrap(),
                    )?;
                }
            }

            // --- UnsafeCast ---
            Opcode::UnsafeCast { dst, src } => {
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "unsafe_cast_src",
                )?;
                self.builder
                    .build_store(registers[dst.0 as usize], src_val)?;
            }

            // --- ToSFloat ---
            Opcode::ToSFloat { dst, src } => {
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "tosfloat_src",
                )?;
                let f64_type = self.context.f64_type();
                let result: BasicValueEnum = if src_val.is_int_value() {
                    self.builder
                        .build_signed_int_to_float(src_val.into_int_value(), f64_type, "tosfloat")?
                        .into()
                } else if src_val.is_float_value() {
                    // Already float — just ensure it's f64
                    let fv = src_val.into_float_value();
                    if fv.get_type() == self.context.f32_type() {
                        self.builder
                            .build_float_ext(fv, f64_type, "tosfloat_ext")?
                            .into()
                    } else {
                        fv.into()
                    }
                } else {
                    return Err(anyhow!("ToSFloat: unexpected source type"));
                };
                self.builder
                    .build_store(registers[dst.0 as usize], result)?;
            }

            // --- ToUFloat ---
            Opcode::ToUFloat { dst, src } => {
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "toufloat_src",
                )?;
                let f64_type = self.context.f64_type();
                let result: BasicValueEnum = if src_val.is_int_value() {
                    self.builder
                        .build_unsigned_int_to_float(
                            src_val.into_int_value(),
                            f64_type,
                            "toufloat",
                        )?
                        .into()
                } else if src_val.is_float_value() {
                    let fv = src_val.into_float_value();
                    if fv.get_type() == self.context.f32_type() {
                        self.builder
                            .build_float_ext(fv, f64_type, "toufloat_ext")?
                            .into()
                    } else {
                        fv.into()
                    }
                } else {
                    return Err(anyhow!("ToUFloat: unexpected source type"));
                };
                self.builder
                    .build_store(registers[dst.0 as usize], result)?;
            }

            // --- ToInt ---
            Opcode::ToInt { dst, src } => {
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "toint_src",
                )?;
                let i32_type = self.context.i32_type();
                let result: BasicValueEnum = if src_val.is_float_value() {
                    self.builder
                        .build_float_to_signed_int(src_val.into_float_value(), i32_type, "toint")?
                        .into()
                } else if src_val.is_int_value() {
                    // Already int — truncate or extend to i32
                    let iv = src_val.into_int_value();
                    if iv.get_type().get_bit_width() > 32 {
                        self.builder
                            .build_int_truncate(iv, i32_type, "toint_trunc")?
                            .into()
                    } else if iv.get_type().get_bit_width() < 32 {
                        self.builder
                            .build_int_s_extend(iv, i32_type, "toint_ext")?
                            .into()
                    } else {
                        iv.into()
                    }
                } else {
                    return Err(anyhow!("ToInt: unexpected source type"));
                };
                self.builder
                    .build_store(registers[dst.0 as usize], result)?;
            }

            // --- StaticClosure: allocate a vclosure wrapping the function ---
            Opcode::StaticClosure { dst, fun } => {
                if !self.lazy_compilation {
                    let (_function, is_placeholder) = self.get_or_create_function_value(fun.0)?;
                    if is_placeholder {
                        self.add_pending_compilation(fun.0);
                    }
                }

                let ptr_type = self.context.ptr_type(AddressSpace::default());

                // Load function address from functions_ptrs[findex] at runtime
                let findex = fun.0 as usize;
                let fun_addr_ptr = self.function_slot_ptr(findex)?;
                let fun_addr = self
                    .builder
                    .build_load(ptr_type, fun_addr_ptr, "static_closure_fun")?
                    .into_pointer_value();

                // Get function type pointer (compile-time constant from func_types)
                let type_ptr = self.func_type_ptr(findex)?;

                let closure = if self.aot {
                    self.emit_static_closure(findex, type_ptr)?
                } else {
                    let alloc_closure = self.declare_native(
                        "hlp_alloc_closure_void",
                        &[ptr_type.into(), ptr_type.into()],
                        Some(ptr_type.into()),
                    );
                    self.builder
                        .build_call(
                            alloc_closure,
                            &[type_ptr.into(), fun_addr.into()],
                            "static_closure",
                        )?
                        .try_as_basic_value()
                        .basic()
                        .unwrap()
                };
                self.builder
                    .build_store(registers[dst.0 as usize], closure)?;
            }

            // --- CallClosure ---
            Opcode::CallClosure { dst, fun, args } => {
                let raw_closure_ptr = self
                    .builder
                    .build_load(
                        reg_types[fun.0 as usize],
                        registers[fun.0 as usize],
                        "closure_ptr",
                    )?
                    .into_pointer_value();

                let i8_type = self.context.i8_type();
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let i32_type = self.context.i32_type();

                // HashLink represents a signature-adapted bound closure as a
                // vclosure_wrapper: its public vclosure has hasValue == 2 and
                // the original closure lives after it at offset 32. Hybrid's
                // interpreter runner unwraps this object; LLVM must do the
                // same before reading the callable fields.
                let raw_has_value_gep = unsafe {
                    self.builder.build_gep(
                        i8_type,
                        raw_closure_ptr,
                        &[self.context.i64_type().const_int(16, false)],
                        "closure_raw_hasvalue_gep",
                    )?
                };
                let raw_has_value = self
                    .builder
                    .build_load(i32_type, raw_has_value_gep, "closure_raw_hasvalue")?
                    .into_int_value();
                let is_wrapper = self.builder.build_int_compare(
                    IntPredicate::EQ,
                    raw_has_value,
                    i32_type.const_int(2, false),
                    "closure_is_wrapper",
                )?;
                // Branch rather than load-then-select. `wrappedFun` lives at
                // offset 32, which exists only on a `vclosure_wrapper` (40
                // bytes); a plain `vclosure` is 32, and that is what
                // `hlp_alloc_closure_void`/`_ptr` allocate. Loading it
                // unconditionally and discarding it in a `select` read 8 bytes
                // past the end of every ordinary closure, on every call --
                // harmless inside an Immix block, a fault when the closure is
                // the last object before an unmapped page. Guarding the load
                // also keeps the common path within the 32 bytes the object is
                // known to have.
                let unwrap_function = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();
                let unwrap_bb = self
                    .context
                    .append_basic_block(unwrap_function, "closure_unwrap");
                let unwrap_done_bb = self
                    .context
                    .append_basic_block(unwrap_function, "closure_unwrap_done");
                let unwrap_from_bb = self.builder.get_insert_block().unwrap();
                self.builder
                    .build_conditional_branch(is_wrapper, unwrap_bb, unwrap_done_bb)?;

                self.builder.position_at_end(unwrap_bb);
                let wrapped_fun_gep = unsafe {
                    self.builder.build_gep(
                        i8_type,
                        raw_closure_ptr,
                        &[self.context.i64_type().const_int(32, false)],
                        "closure_wrapped_fun_gep",
                    )?
                };
                let wrapped_fun = self
                    .builder
                    .build_load(ptr_type, wrapped_fun_gep, "closure_wrapped_fun")?
                    .into_pointer_value();
                self.builder.build_unconditional_branch(unwrap_done_bb)?;

                self.builder.position_at_end(unwrap_done_bb);
                let closure_phi = self.builder.build_phi(ptr_type, "closure_unwrapped")?;
                closure_phi.add_incoming(&[
                    (&wrapped_fun, unwrap_bb),
                    (&raw_closure_ptr, unwrap_from_bb),
                ]);
                let closure_ptr = closure_phi.as_basic_value().into_pointer_value();

                // vclosure.fun at offset 8
                let fun_field_gep = unsafe {
                    self.builder.build_gep(
                        i8_type,
                        closure_ptr,
                        &[self.context.i64_type().const_int(8, false)],
                        "closure_fun_gep",
                    )?
                };
                let fun_ptr = self
                    .builder
                    .build_load(ptr_type, fun_field_gep, "closure_fun")?
                    .into_pointer_value();

                // vclosure.hasValue at offset 16
                let has_value_gep = unsafe {
                    self.builder.build_gep(
                        i8_type,
                        closure_ptr,
                        &[self.context.i64_type().const_int(16, false)],
                        "closure_hasvalue_gep",
                    )?
                };
                let has_value = self
                    .builder
                    .build_load(i32_type, has_value_gep, "has_value")?
                    .into_int_value();

                // vclosure.value at offset 24
                let value_gep = unsafe {
                    self.builder.build_gep(
                        i8_type,
                        closure_ptr,
                        &[self.context.i64_type().const_int(24, false)],
                        "closure_value_gep",
                    )?
                };
                let closure_value = self
                    .builder
                    .build_load(ptr_type, value_gep, "closure_value")?
                    .into_pointer_value();

                // Load all explicit args
                let arg_vals: Vec<BasicMetadataValueEnum> = args
                    .iter()
                    .map(|arg| {
                        self.builder
                            .build_load(
                                reg_types[arg.0 as usize],
                                registers[arg.0 as usize],
                                "arg_val",
                            )
                            .unwrap()
                            .into()
                    })
                    .collect();

                // Determine function type from register's type info
                let fun_type_idx = f.regs[fun.0 as usize].0;
                let base_fn_type = if let Some(fun_type) = self.types_[fun_type_idx].fun.clone() {
                    self.create_function_type(&fun_type)?
                } else {
                    // Dynamic-typed closure: infer from args (all ptrs) with ptr return
                    let dyn_params: Vec<BasicMetadataTypeEnum> =
                        args.iter().map(|_| ptr_type.into()).collect();
                    // Determine return type from dst register
                    let dst_type = reg_types[dst.0 as usize];
                    match dst_type {
                        BasicTypeEnum::IntType(t) => t.fn_type(&dyn_params, false),
                        BasicTypeEnum::FloatType(t) => t.fn_type(&dyn_params, false),
                        _ => ptr_type.fn_type(&dyn_params, false),
                    }
                };

                // Build extended function type (with value prepended as first arg)
                let mut extended_params: Vec<BasicMetadataTypeEnum> = vec![ptr_type.into()];
                extended_params.extend(base_fn_type.get_param_types().iter().map(|t| {
                    let bmt: BasicMetadataTypeEnum = (*t).into();
                    bmt
                }));
                let extended_fn_type = if base_fn_type.get_return_type().is_some() {
                    let ret = base_fn_type.get_return_type().unwrap();
                    match ret {
                        BasicTypeEnum::FloatType(t) => t.fn_type(&extended_params, false),
                        BasicTypeEnum::IntType(t) => t.fn_type(&extended_params, false),
                        BasicTypeEnum::PointerType(t) => t.fn_type(&extended_params, false),
                        BasicTypeEnum::ArrayType(t) => t.fn_type(&extended_params, false),
                        BasicTypeEnum::StructType(t) => t.fn_type(&extended_params, false),
                        BasicTypeEnum::VectorType(t) => t.fn_type(&extended_params, false),
                        BasicTypeEnum::ScalableVectorType(t) => t.fn_type(&extended_params, false),
                    }
                } else {
                    self.context.void_type().fn_type(&extended_params, false)
                };

                let function = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();
                let call_done_bb = self.context.append_basic_block(function, "call_done");

                // Guarded devirtualisation comes before the generic runtime
                // signature check. The target's real LLVM signature was
                // checked while constructing `devirt`, so a guard hit is
                // already ABI-safe. Signature-adapted closures whose target
                // does not have this call site's ABI cannot enter this arm;
                // they continue to the dynamic marshaller below. Keeping the
                // common monomorphic arm first avoids putting the Issue2889
                // safety branch inside every iteration of closure_call.
                let devirt = if self.hot_reload {
                    None
                } else {
                    let caller = f.findex as u32;
                    crate::callsite_profile::closure_target(caller, i as u32)
                        .or_else(|| crate::callsite_profile::uniform_closure_target(caller))
                        .and_then(|(target, exp_hv)| {
                            let expected_ty = if exp_hv {
                                extended_fn_type
                            } else {
                                base_fn_type
                            };
                            match self.get_or_create_function_value(target as usize) {
                                Ok((callee, is_placeholder)) => {
                                    if is_placeholder {
                                        self.add_pending_compilation(target as usize);
                                    }
                                    (callee.get_type() == expected_ty)
                                        .then_some((callee, target, exp_hv))
                                }
                                Err(_) => None,
                            }
                        })
                };

                if let Some((callee, target, exp_hv)) = devirt {
                    crate::profile::count("devirt closure fast-arm", 1);
                    // Closure header fields are immutable after allocation.
                    // This lets LICM hoist the target guard when the closure
                    // value itself is loop invariant.
                    for lv in [
                        raw_has_value.as_instruction(),
                        wrapped_fun.as_instruction(),
                        fun_ptr.as_instruction(),
                        has_value.as_instruction(),
                        closure_value.as_instruction(),
                    ]
                    .into_iter()
                    .flatten()
                    {
                        let _ = lv.set_metadata(
                            self.context.metadata_node(&[]),
                            self.context.get_kind_id("invariant.load"),
                        );
                    }

                    let devirt_bb = self.context.append_basic_block(function, "devirt_hit");
                    let signature_bb = self
                        .context
                        .append_basic_block(function, "devirt_miss_signature");
                    let fun_int = self.builder.build_ptr_to_int(
                        fun_ptr,
                        self.context.i64_type(),
                        "closure_fun_int",
                    )?;
                    let is_target = self.builder.build_int_compare(
                        IntPredicate::EQ,
                        fun_int,
                        self.context.i64_type().const_int(target as u64 + 1, false),
                        "devirt_is_target",
                    )?;
                    let hv_matches = self.builder.build_int_compare(
                        if exp_hv {
                            IntPredicate::NE
                        } else {
                            IntPredicate::EQ
                        },
                        has_value,
                        i32_type.const_zero(),
                        "devirt_hv",
                    )?;
                    let guard = self
                        .builder
                        .build_and(is_target, hv_matches, "devirt_guard")?;
                    self.builder
                        .build_conditional_branch(guard, devirt_bb, signature_bb)?;

                    self.builder.position_at_end(devirt_bb);
                    let direct_args: Vec<BasicMetadataValueEnum> = if exp_hv {
                        let mut values: Vec<BasicMetadataValueEnum> = vec![closure_value.into()];
                        values.extend(arg_vals.iter().cloned());
                        values
                    } else {
                        arg_vals.clone()
                    };
                    let ret = self
                        .builder
                        .build_call(callee, &direct_args, "devirt_call")?
                        .try_as_basic_value();
                    if let Some(value) = ret.basic() {
                        self.builder
                            .build_store(registers[dst.0 as usize], value)?;
                    }
                    self.builder.build_unconditional_branch(call_done_bb)?;
                    self.builder.position_at_end(signature_bb);
                }

                // The register's HFUN is only the call-site contract. A
                // signature-adapted closure can carry a different runtime
                // HFUN and a wrapper body with that runtime ABI. Calling it
                // through `base_fn_type` turns scalar arguments into tiny
                // pointers (Issue2889 passed Int(1) as vdynamic* 0x1). Match
                // Cranelift's AIR V2 lowering: retain the typed fast path for
                // equal signatures, otherwise let the runtime marshal using
                // the closure's own type.
                let runtime_type = unsafe {
                    let gep = self.builder.build_gep(
                        i8_type,
                        raw_closure_ptr,
                        &[self.context.i64_type().const_zero()],
                        "closure_runtime_type_gep",
                    )?;
                    self.builder
                        .build_load(ptr_type, gep, "closure_runtime_type")?
                        .into_pointer_value()
                };
                let expected_type = self
                    .get_initialized_type(fun_type_idx)?
                    .into_pointer_value();
                let pointer_exact = self.builder.build_int_compare(
                    IntPredicate::EQ,
                    runtime_type,
                    expected_type,
                    "closure_type_pointer_exact",
                )?;
                let structural_bb = self
                    .context
                    .append_basic_block(function, "closure_type_structural");
                let typed_bb = self
                    .context
                    .append_basic_block(function, "closure_type_typed");
                let dynamic_bb = self
                    .context
                    .append_basic_block(function, "closure_type_dynamic");
                self.builder
                    .build_conditional_branch(pointer_exact, typed_bb, structural_bb)?;

                self.builder.position_at_end(structural_bb);
                let same_type = self.declare_native(
                    "hlp_same_type",
                    &[ptr_type.into(), ptr_type.into()],
                    Some(self.context.bool_type().into()),
                );
                let structurally_exact = self
                    .builder
                    .build_call(
                        same_type,
                        &[runtime_type.into(), expected_type.into()],
                        "closure_same_type",
                    )?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| anyhow!("hlp_same_type returned void"))?
                    .into_int_value();
                self.builder
                    .build_conditional_branch(structurally_exact, typed_bb, dynamic_bb)?;

                self.builder.position_at_end(dynamic_bb);
                let nargs = args.len();
                let argv = self.builder.build_array_alloca(
                    ptr_type,
                    i32_type.const_int(nargs.max(1) as u64, false),
                    "closure_dyn_argv",
                )?;
                let make_dyn = self.declare_native(
                    "hlp_make_dyn",
                    &[ptr_type.into(), ptr_type.into()],
                    Some(ptr_type.into()),
                );
                for (index, arg) in args.iter().enumerate() {
                    let type_index = f.regs[arg.0 as usize].0;
                    let kind = self.types_[type_index].kind;
                    let loaded = self.builder.build_load(
                        reg_types[arg.0 as usize],
                        registers[arg.0 as usize],
                        "closure_dyn_arg",
                    )?;
                    let self_describing = matches!(
                        kind,
                        hl_type_kind_HDYN
                            | hl_type_kind_HFUN
                            | hl_type_kind_HOBJ
                            | crate::hl::hl_type_kind_HARRAY
                            | hl_type_kind_HVIRTUAL
                            | hl_type_kind_HDYNOBJ
                            | crate::hl::hl_type_kind_HENUM
                            | hl_type_kind_HNULL
                    );
                    let boxed = if self_describing {
                        loaded
                    } else {
                        let slot = self
                            .builder
                            .build_alloca(loaded.get_type(), "closure_dyn_box_slot")?;
                        self.builder.build_store(slot, loaded)?;
                        let type_ptr = self.get_initialized_type(type_index)?.into_pointer_value();
                        self.builder
                            .build_call(
                                make_dyn,
                                &[slot.into(), type_ptr.into()],
                                "closure_dyn_box",
                            )?
                            .try_as_basic_value()
                            .basic()
                            .ok_or_else(|| anyhow!("hlp_make_dyn returned void"))?
                    };
                    let boxed = if boxed.get_type() == ptr_type.as_basic_type_enum() {
                        boxed
                    } else {
                        self.cast_for_call(boxed, ptr_type.into())?
                    };
                    let argv_slot = unsafe {
                        self.builder.build_gep(
                            ptr_type,
                            argv,
                            &[i32_type.const_int(index as u64, false)],
                            "closure_dyn_argv_slot",
                        )?
                    };
                    self.builder.build_store(argv_slot, boxed)?;
                }
                let dyn_call = self.declare_native(
                    "hlp_dyn_call",
                    &[ptr_type.into(), ptr_type.into(), i32_type.into()],
                    Some(ptr_type.into()),
                );
                let dyn_result = self
                    .builder
                    .build_call(
                        dyn_call,
                        &[
                            raw_closure_ptr.into(),
                            argv.into(),
                            i32_type.const_int(nargs as u64, false).into(),
                        ],
                        "closure_dyn_call",
                    )?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| anyhow!("hlp_dyn_call returned void"))?
                    .into_pointer_value();
                let dst_type_index = f.regs[dst.0 as usize].0;
                let dst_kind = self.types_[dst_type_index].kind;
                if dst_kind != hl_type_kind_HVOID {
                    let dst_type = reg_types[dst.0 as usize];
                    let value: BasicValueEnum = if dst_kind == hl_type_kind_HDYN {
                        dyn_result.into()
                    } else if dst_type.is_pointer_type() {
                        let dyn_type_index = self
                            .types_
                            .iter()
                            .position(|ty| ty.kind == hl_type_kind_HDYN)
                            .ok_or_else(|| anyhow!("module has no HDYN runtime type"))?;
                        let dyn_type = self
                            .get_initialized_type(dyn_type_index)?
                            .into_pointer_value();
                        let dst_runtime_type = self
                            .get_initialized_type(dst_type_index)?
                            .into_pointer_value();
                        let result_slot = self
                            .builder
                            .build_alloca(ptr_type, "closure_dyn_result_slot")?;
                        self.builder.build_store(result_slot, dyn_result)?;
                        let castp = self.declare_native(
                            "hlp_dyn_castp",
                            &[ptr_type.into(), ptr_type.into(), ptr_type.into()],
                            Some(ptr_type.into()),
                        );
                        self.builder
                            .build_call(
                                castp,
                                &[result_slot.into(), dyn_type.into(), dst_runtime_type.into()],
                                "closure_dyn_result_cast",
                            )?
                            .try_as_basic_value()
                            .basic()
                            .ok_or_else(|| anyhow!("hlp_dyn_castp returned void"))?
                    } else {
                        let (helper, helper_ret): (&str, BasicTypeEnum) =
                            if dst_kind == hl_type_kind_HF64 {
                                ("hlp_dyn_todouble", self.context.f64_type().into())
                            } else if dst_kind == hl_type_kind_HF32 {
                                ("hlp_dyn_tofloat", self.context.f32_type().into())
                            } else if dst_kind == hl_type_kind_HI64 {
                                ("hlp_dyn_toi64", self.context.i64_type().into())
                            } else {
                                ("hlp_dyn_toint", i32_type.into())
                            };
                        let unbox =
                            self.declare_native(helper, &[ptr_type.into()], Some(helper_ret));
                        let raw = self
                            .builder
                            .build_call(unbox, &[dyn_result.into()], "closure_dyn_unbox")?
                            .try_as_basic_value()
                            .basic()
                            .ok_or_else(|| anyhow!("dynamic unbox helper returned void"))?;
                        if raw.get_type() == dst_type {
                            raw
                        } else {
                            self.cast_for_call(raw, dst_type)?
                        }
                    };
                    self.builder.build_store(registers[dst.0 as usize], value)?;
                }
                self.builder.build_unconditional_branch(call_done_bb)?;

                self.builder.position_at_end(typed_bb);

                // Branch based on hasValue
                let has_value_cmp = self.builder.build_int_compare(
                    IntPredicate::NE,
                    has_value,
                    i32_type.const_zero(),
                    "has_value_cmp",
                )?;

                let call_with_value_bb =
                    self.context.append_basic_block(function, "call_with_value");
                let call_without_value_bb = self
                    .context
                    .append_basic_block(function, "call_without_value");

                self.builder.build_conditional_branch(
                    has_value_cmp,
                    call_with_value_bb,
                    call_without_value_bb,
                )?;

                // --- Call WITH value (hasValue != 0) ---
                self.builder.position_at_end(call_with_value_bb);
                let mut args_with_value: Vec<BasicMetadataValueEnum> = vec![closure_value.into()];
                args_with_value.extend(arg_vals.iter().cloned());
                if let Some(ret_val) = self.build_stub_guarded_indirect_call(
                    extended_fn_type,
                    fun_ptr,
                    &args_with_value,
                    "call_closure_hv",
                )? {
                    self.builder
                        .build_store(registers[dst.0 as usize], ret_val)?;
                }
                self.builder.build_unconditional_branch(call_done_bb)?;

                // --- Call WITHOUT value (hasValue == 0) ---
                self.builder.position_at_end(call_without_value_bb);
                if let Some(ret_val) = self.build_stub_guarded_indirect_call(
                    base_fn_type,
                    fun_ptr,
                    &arg_vals,
                    "call_closure",
                )? {
                    self.builder
                        .build_store(registers[dst.0 as usize], ret_val)?;
                }
                self.builder.build_unconditional_branch(call_done_bb)?;

                // Continue from call_done
                self.builder.position_at_end(call_done_bb);
            }

            // --- SafeCast: unbox HNULL(T)/HDYN -> primitive T, otherwise copy ---
            Opcode::SafeCast { dst, src } => {
                let src_type_idx = f.regs[src.0 as usize].0;
                let dst_type_idx = f.regs[dst.0 as usize].0;
                let src_kind = self.types_[src_type_idx].kind;
                let dst_kind = self.types_[dst_type_idx].kind;

                // Unboxing needed when casting from a heap-boxed type (HNULL/HDYN)
                // to a primitive type. The primitive value lives at offset 8 inside
                // the vdynamic struct (the `v` union field).
                let needs_unbox = (src_kind == hl_type_kind_HNULL || src_kind == hl_type_kind_HDYN)
                    && matches!(dst_kind,
                        k if k == hl_type_kind_HBOOL || k == hl_type_kind_HI32
                            || k == hl_type_kind_HF64 || k == hl_type_kind_HF32
                            || k == hl_type_kind_HI64 || k == hl_type_kind_HUI8
                            || k == hl_type_kind_HUI16);

                if needs_unbox {
                    let ptr_type = self.context.ptr_type(AddressSpace::default());
                    let src_ptr = self
                        .builder
                        .build_load(ptr_type, registers[src.0 as usize], "safecast_src")?
                        .into_pointer_value();

                    let is_null = self.builder.build_is_null(src_ptr, "safecast_null")?;

                    let function = self
                        .builder
                        .get_insert_block()
                        .unwrap()
                        .get_parent()
                        .unwrap();
                    let null_bb = self
                        .context
                        .append_basic_block(function, "safecast_null_path");
                    let unbox_bb = self.context.append_basic_block(function, "safecast_unbox");
                    let done_bb = self.context.append_basic_block(function, "safecast_done");

                    self.builder
                        .build_conditional_branch(is_null, null_bb, unbox_bb)?;

                    // Unbox path. A raw load of vdynamic.v at offset 8 is only
                    // right when the box's runtime kind IS the destination kind
                    // — an HDYN register can hold any numeric box, and reading
                    // an Int box as f64 yields its bits as a denormal (Dynamic
                    // subtraction of 7 and 2 printed 2.47e-323, div was exact
                    // because the 2^-1074 scales cancelled). Coerce through the
                    // dyn-cast helpers instead; they switch on the box's own
                    // runtime type and match the interpreter and upstream.
                    self.builder.position_at_end(unbox_bb);
                    let dst_llvm_type = reg_types[dst.0 as usize];
                    let (helper, helper_ret): (&str, BasicTypeEnum) =
                        if dst_kind == hl_type_kind_HF64 {
                            ("hlp_dyn_todouble", self.context.f64_type().into())
                        } else if dst_kind == hl_type_kind_HF32 {
                            ("hlp_dyn_tofloat", self.context.f32_type().into())
                        } else if dst_kind == hl_type_kind_HI64 {
                            ("hlp_dyn_toi64", self.context.i64_type().into())
                        } else {
                            ("hlp_dyn_toint", self.context.i32_type().into())
                        };
                    let unbox_fn =
                        self.declare_native(helper, &[ptr_type.into()], Some(helper_ret));
                    let raw = self
                        .builder
                        .build_call(unbox_fn, &[src_ptr.into()], "safecast_unbox_call")?
                        .try_as_basic_value()
                        .basic()
                        .unwrap();
                    let unboxed = if raw.get_type() != dst_llvm_type {
                        self.cast_for_call(raw, dst_llvm_type)?
                    } else {
                        raw
                    };
                    self.builder
                        .build_store(registers[dst.0 as usize], unboxed)?;
                    self.builder.build_unconditional_branch(done_bb)?;

                    // Null path: store default value (0/false/0.0)
                    self.builder.position_at_end(null_bb);
                    let default_val = dst_llvm_type.const_zero();
                    self.builder
                        .build_store(registers[dst.0 as usize], default_val)?;
                    self.builder.build_unconditional_branch(done_bb)?;

                    self.builder.position_at_end(done_bb);
                } else if src_kind == hl_type_kind_HDYN || src_kind == hl_type_kind_HNULL {
                    // Dynamic-to-concrete non-primitive cast: call hlp_dyn_castp to
                    // properly extract the inner value from the vdynamic wrapper.
                    // A simple pointer copy would pass the vdynamic header address
                    // instead of the actual data (e.g. bytes pointer for HBYTES).
                    let ptr_type = self.context.ptr_type(AddressSpace::default());
                    let src_type_ptr = self
                        .get_initialized_type(src_type_idx)?
                        .into_pointer_value();
                    let dst_type_ptr = self
                        .get_initialized_type(dst_type_idx)?
                        .into_pointer_value();
                    let dyn_castp = self.declare_native(
                        "hlp_dyn_castp",
                        &[ptr_type.into(), ptr_type.into(), ptr_type.into()],
                        Some(ptr_type.into()),
                    );
                    // hlp_dyn_castp expects double-indirection: data points to a slot
                    // containing the *mut vdynamic, which is exactly what the alloca is.
                    let result = self.builder.build_call(
                        dyn_castp,
                        &[
                            registers[src.0 as usize].into(),
                            src_type_ptr.into(),
                            dst_type_ptr.into(),
                        ],
                        "dyn_castp",
                    )?;
                    self.builder.build_store(
                        registers[dst.0 as usize],
                        result.try_as_basic_value().basic().unwrap(),
                    )?;
                } else if src_type_idx != dst_type_idx
                    && ((src_kind == hl_type_kind_HOBJ && dst_kind == hl_type_kind_HOBJ)
                        || (src_kind == hl_type_kind_HSTRUCT && dst_kind == hl_type_kind_HSTRUCT))
                {
                    let ptr_type = self.context.ptr_type(AddressSpace::default());
                    let src_type_ptr = self
                        .get_initialized_type(src_type_idx)?
                        .into_pointer_value();
                    let dst_type_ptr = self
                        .get_initialized_type(dst_type_idx)?
                        .into_pointer_value();
                    let dyn_castp = self.declare_native(
                        "hlp_dyn_castp",
                        &[ptr_type.into(), ptr_type.into(), ptr_type.into()],
                        Some(ptr_type.into()),
                    );
                    let result = self.builder.build_call(
                        dyn_castp,
                        &[
                            registers[src.0 as usize].into(),
                            src_type_ptr.into(),
                            dst_type_ptr.into(),
                        ],
                        "dyn_castp_obj",
                    )?;
                    self.builder.build_store(
                        registers[dst.0 as usize],
                        result.try_as_basic_value().basic().unwrap(),
                    )?;
                } else {
                    // Same type or non-dynamic: simple pointer copy
                    let src_val = self.builder.build_load(
                        reg_types[src.0 as usize],
                        registers[src.0 as usize],
                        "safecast_src",
                    )?;
                    self.builder
                        .build_store(registers[dst.0 as usize], src_val)?;
                }
            }

            // --- ToVirtual: wrap object in a vvirtual with resolved field/method pointers ---
            Opcode::ToVirtual { dst, src } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let dst_type_idx = f.regs[dst.0 as usize].0;
                let dst_kind = self.types_[dst_type_idx].kind;

                if dst_kind == hl_type_kind_HVIRTUAL {
                    // Get the full C-side hl_type pointer for the virtual type
                    let vt_ptr = self
                        .get_initialized_type(dst_type_idx)?
                        .into_pointer_value();
                    let src_val = self
                        .builder
                        .build_load(ptr_type, registers[src.0 as usize], "tovirt_src")?
                        .into_pointer_value();

                    let hl_to_virtual = self.declare_native(
                        "hl_to_virtual",
                        &[ptr_type.into(), ptr_type.into()],
                        Some(ptr_type.into()),
                    );
                    let result = self.builder.build_call(
                        hl_to_virtual,
                        &[vt_ptr.into(), src_val.into()],
                        "tovirt",
                    )?;
                    self.builder.build_store(
                        registers[dst.0 as usize],
                        result.try_as_basic_value().basic().unwrap(),
                    )?;
                } else {
                    // Non-virtual dst: simple pointer copy
                    let src_val = self.builder.build_load(
                        reg_types[src.0 as usize],
                        registers[src.0 as usize],
                        "tovirt_src",
                    )?;
                    self.builder
                        .build_store(registers[dst.0 as usize], src_val)?;
                }
            }

            // --- Trap: setjmp-based exception handling ---
            Opcode::Trap { exc, offset } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let i32_type = self.context.i32_type();

                // 1. Call hlp_setup_trap_jit() → returns *mut c_int (jmp_buf pointer)
                let setup = self.declare_native("hlp_setup_trap_jit", &[], Some(ptr_type.into()));
                let buf_ptr = self
                    .builder
                    .build_call(setup, &[], "trap_buf")?
                    .try_as_basic_value()
                    .basic()
                    .unwrap()
                    .into_pointer_value();

                // 2. Call _setjmp(buf_ptr) via indirect call (system function, not in stdlib)
                let setjmp_ptr = self.setjmp_ptr()?;
                let setjmp_fn_type = i32_type.fn_type(&[ptr_type.into()], false);
                let setjmp_call = self.builder.build_indirect_call(
                    setjmp_fn_type,
                    setjmp_ptr,
                    &[buf_ptr.into()],
                    "setjmp_ret",
                )?;
                // Mark as returns_twice so LLVM doesn't misoptimize around setjmp at O3
                let rt_kind =
                    inkwell::attributes::Attribute::get_named_enum_kind_id("returns_twice");
                let rt_attr = self.context.create_enum_attribute(rt_kind, 0);
                setjmp_call.add_attribute(inkwell::attributes::AttributeLoc::Function, rt_attr);
                let setjmp_result = setjmp_call
                    .try_as_basic_value()
                    .basic()
                    .unwrap()
                    .into_int_value();

                // 3. Branch: 0 → normal (protected code), non-zero → handler
                let is_exception = self.builder.build_int_compare(
                    IntPredicate::NE,
                    setjmp_result,
                    i32_type.const_zero(),
                    "is_exc",
                )?;

                let handler_block = opcode_blocks[(i as i32 + 1 + *offset) as usize];
                let normal_block = opcode_blocks[i + 1];

                // Create handler_entry block to load exc value before jumping to handler
                let function = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();
                let handler_entry = self
                    .context
                    .append_basic_block(function, &format!("trap_handler_{}", i));

                self.builder
                    .build_conditional_branch(is_exception, handler_entry, normal_block)?;

                // Emit handler entry: load exc value into exc register, then branch to handler
                self.builder.position_at_end(handler_entry);
                let get_exc = self.declare_native("hlp_get_exc_value", &[], Some(ptr_type.into()));
                let exc_val = self
                    .builder
                    .build_call(get_exc, &[], "exc_val")?
                    .try_as_basic_value()
                    .basic()
                    .unwrap();
                self.builder
                    .build_store(registers[exc.0 as usize], exc_val)?;
                // Clear the global exc_value to prevent stale values
                // contaminating nested exception handlers.
                let clear_exc = self.declare_native("hlp_clear_exc_value", &[], None);
                self.builder.build_call(clear_exc, &[], "")?;
                self.builder.build_unconditional_branch(handler_block)?;
            }

            // --- EndTrap: remove trap context ---
            Opcode::EndTrap { exc: _ } => {
                let remove = self.declare_native("hlp_remove_trap_jit", &[], None);
                self.builder.build_call(remove, &[], "")?;
            }

            // --- Throw: call hlp_throw (diverging) ---
            Opcode::Throw { exc } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let exc_val =
                    self.builder
                        .build_load(ptr_type, registers[exc.0 as usize], "throw_val")?;
                let throw_fn = self.declare_native("hlp_throw", &[ptr_type.into()], None);
                self.builder.build_call(throw_fn, &[exc_val.into()], "")?;
                self.builder.build_unreachable()?;
            }

            // --- Rethrow: same as Throw ---
            Opcode::Rethrow { exc } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let exc_val =
                    self.builder
                        .build_load(ptr_type, registers[exc.0 as usize], "rethrow_val")?;
                let throw_fn = self.declare_native("hlp_throw", &[ptr_type.into()], None);
                self.builder.build_call(throw_fn, &[exc_val.into()], "")?;
                self.builder.build_unreachable()?;
            }

            // --- Ref: take address of register ---
            Opcode::Ref { dst, src } => {
                // dst = &src (pointer to the register's alloca)
                self.builder
                    .build_store(registers[dst.0 as usize], registers[src.0 as usize])?;
            }

            // --- Unref: dereference pointer ---
            Opcode::Unref { dst, src } => {
                let ptr = self
                    .builder
                    .build_load(
                        reg_types[src.0 as usize],
                        registers[src.0 as usize],
                        "unref_ptr",
                    )?
                    .into_pointer_value();
                let val = self
                    .builder
                    .build_load(reg_types[dst.0 as usize], ptr, "unref_val")?;
                self.builder.build_store(registers[dst.0 as usize], val)?;
            }

            // --- Setref: store through pointer ---
            Opcode::Setref { dst, value } => {
                let ptr = self
                    .builder
                    .build_load(
                        reg_types[dst.0 as usize],
                        registers[dst.0 as usize],
                        "setref_ptr",
                    )?
                    .into_pointer_value();
                let val = self.builder.build_load(
                    reg_types[value.0 as usize],
                    registers[value.0 as usize],
                    "setref_val",
                )?;
                self.builder.build_store(ptr, val)?;
            }

            // --- InstanceClosure: allocate closure binding obj as first arg ---
            Opcode::InstanceClosure { dst, fun, obj } => {
                if !self.lazy_compilation {
                    let (_function, is_placeholder) = self.get_or_create_function_value(fun.0)?;
                    if is_placeholder {
                        self.add_pending_compilation(fun.0);
                    }
                }

                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let findex = fun.0 as usize;

                // Load function address from functions_ptrs[findex]
                let fun_addr_ptr = self.function_slot_ptr(findex)?;
                let fun_addr = self
                    .builder
                    .build_load(ptr_type, fun_addr_ptr, "inst_closure_fun")?
                    .into_pointer_value();

                // This removes the first param (bound obj's type) from the fn signature
                let func_type_const = self.func_type_ptr(findex)?;
                // The METHOD's full type, unstripped: `hlp_alloc_closure_ptr`
                // does the single strip itself, exactly as upstream's
                // OInstanceClosure does (jit_emit.c passes
                // `functions[functions_indexes[fun]].type`) and as the
                // Cranelift tier does. Stripping here first made the
                // allocator strip a SECOND time, walking off the end of the
                // `hl_type_fun` it was handed.
                let closure_type: inkwell::values::BasicValueEnum = func_type_const.into();

                // Load bound object
                let obj_val =
                    self.builder
                        .build_load(ptr_type, registers[obj.0 as usize], "inst_obj")?;

                // Call hlp_alloc_closure_ptr(closure_type, fun_addr, obj_ptr)
                let alloc = self.declare_native(
                    "hlp_alloc_closure_ptr",
                    &[ptr_type.into(), ptr_type.into(), ptr_type.into()],
                    Some(ptr_type.into()),
                );
                let closure = self
                    .builder
                    .build_call(
                        alloc,
                        &[closure_type.into(), fun_addr.into(), obj_val.into()],
                        "inst_closure",
                    )?
                    .try_as_basic_value()
                    .basic()
                    .unwrap();
                self.builder
                    .build_store(registers[dst.0 as usize], closure)?;
            }

            // --- VirtualClosure: resolve proto method, create bound closure ---
            Opcode::VirtualClosure { dst, obj, field } => {
                let i8_type = self.context.i8_type();
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let obj_type_idx = f.regs[obj.0 as usize].0;
                let obj_type_info = self.types_[obj_type_idx].clone();

                // Resolve findex from proto table at compile time
                let findex = if let Some(ref obj_data) = obj_type_info.obj {
                    obj_data.proto[field.0 as usize].findex as usize
                } else {
                    return Err(anyhow!(
                        "VirtualClosure: obj register type has no proto table"
                    ));
                };

                if !self.lazy_compilation {
                    let (_function, is_placeholder) = self.get_or_create_function_value(findex)?;
                    if is_placeholder {
                        self.add_pending_compilation(findex);
                    }
                }

                // Load obj pointer
                let obj_val =
                    self.builder
                        .build_load(ptr_type, registers[obj.0 as usize], "vclos_obj")?;

                // Load function address from functions_ptrs[findex]
                let fun_addr_ptr = self.function_slot_ptr(findex)?;
                let fun_addr = self
                    .builder
                    .build_load(ptr_type, fun_addr_ptr, "vclos_fun")?
                    .into_pointer_value();

                // Virtual closures dispatch through the concrete object's
                // runtime proto. The static proto entry may name a base
                // implementation even when the object overrides it.
                let obj_ptr = obj_val.into_pointer_value();
                let obj_type_ptr = self
                    .builder
                    .build_load(ptr_type, obj_ptr, "vclos_obj_type")?
                    .into_pointer_value();
                // The concrete type's proto table, indexed by pindex.
                let vobj_proto = self.vobj_proto_ptr(obj_type_ptr)?;
                let runtime_fun_ptr = unsafe {
                    self.builder.build_gep(
                        ptr_type,
                        vobj_proto,
                        &[self.context.i32_type().const_int(field.0 as u64, false)],
                        "vclos_runtime_fun_gep",
                    )?
                };
                let runtime_fun = self
                    .builder
                    .build_load(ptr_type, runtime_fun_ptr, "vclos_runtime_fun")?
                    .into_pointer_value();
                let runtime_fun_present = self
                    .builder
                    .build_is_not_null(runtime_fun, "vclos_has_runtime_fun")?;
                let fun_addr = self
                    .builder
                    .build_select(
                        runtime_fun_present,
                        runtime_fun,
                        fun_addr,
                        "vclos_selected_fun",
                    )?
                    .into_pointer_value();

                let func_type_const = self.func_type_ptr(findex)?;
                // Full method type; the allocator strips once. See
                // OInstanceClosure above.
                let closure_type: inkwell::values::BasicValueEnum = func_type_const.into();

                // Call hlp_alloc_closure_ptr(closure_type, fun_addr, obj_ptr)
                let alloc = self.declare_native(
                    "hlp_alloc_closure_ptr",
                    &[ptr_type.into(), ptr_type.into(), ptr_type.into()],
                    Some(ptr_type.into()),
                );
                let closure = self
                    .builder
                    .build_call(
                        alloc,
                        &[closure_type.into(), fun_addr.into(), obj_ptr.into()],
                        "vclos",
                    )?
                    .try_as_basic_value()
                    .basic()
                    .unwrap();
                self.builder
                    .build_store(registers[dst.0 as usize], closure)?;
            }

            // --- DynGet: dynamic field access (stub) ---
            Opcode::DynGet { dst, obj, field } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let i32_type = self.context.i32_type();
                let i64_type = self.context.i64_type();

                let obj_val =
                    self.builder
                        .build_load(ptr_type, registers[obj.0 as usize], "dynget_obj")?;
                let field_name = &self.bytecode.strings[field.0].clone();
                let hfield = hl_hash_utf8(field_name);
                let hfield_val = i32_type.const_int(hfield as u64, true);

                let dst_type_idx = f.regs[dst.0 as usize].0;
                let dst_kind = self.types_[dst_type_idx].kind;

                match dst_kind {
                    hl_type_kind_HF64 => {
                        let getter = self.declare_native(
                            "hlp_dyn_getd",
                            &[ptr_type.into(), i32_type.into()],
                            Some(self.context.f64_type().into()),
                        );
                        let result = self.builder.build_call(
                            getter,
                            &[obj_val.into(), hfield_val.into()],
                            "dynget_d",
                        )?;
                        self.builder.build_store(
                            registers[dst.0 as usize],
                            result.try_as_basic_value().basic().unwrap(),
                        )?;
                    }
                    hl_type_kind_HF32 => {
                        let getter = self.declare_native(
                            "hlp_dyn_getf",
                            &[ptr_type.into(), i32_type.into()],
                            Some(self.context.f32_type().into()),
                        );
                        let result = self.builder.build_call(
                            getter,
                            &[obj_val.into(), hfield_val.into()],
                            "dynget_f",
                        )?;
                        self.builder.build_store(
                            registers[dst.0 as usize],
                            result.try_as_basic_value().basic().unwrap(),
                        )?;
                    }
                    hl_type_kind_HI64 => {
                        let getter = self.declare_native(
                            "hlp_dyn_geti64",
                            &[ptr_type.into(), i32_type.into()],
                            Some(i64_type.into()),
                        );
                        let result = self.builder.build_call(
                            getter,
                            &[obj_val.into(), hfield_val.into()],
                            "dynget_i64",
                        )?;
                        self.builder.build_store(
                            registers[dst.0 as usize],
                            result.try_as_basic_value().basic().unwrap(),
                        )?;
                    }
                    hl_type_kind_HI32 | hl_type_kind_HBOOL | hl_type_kind_HUI8
                    | hl_type_kind_HUI16 => {
                        let type_ptr = self
                            .get_initialized_type(dst_type_idx)?
                            .into_pointer_value();
                        let getter = self.declare_native(
                            "hlp_dyn_geti",
                            &[ptr_type.into(), i32_type.into(), ptr_type.into()],
                            Some(i32_type.into()),
                        );
                        let result = self.builder.build_call(
                            getter,
                            &[obj_val.into(), hfield_val.into(), type_ptr.into()],
                            "dynget_i",
                        )?;
                        self.builder.build_store(
                            registers[dst.0 as usize],
                            result.try_as_basic_value().basic().unwrap(),
                        )?;
                    }
                    _ => {
                        // Pointer types: hlp_dyn_getp(obj, hfield, dst_type)
                        let type_ptr = self
                            .get_initialized_type(dst_type_idx)?
                            .into_pointer_value();
                        let getter = self.declare_native(
                            "hlp_dyn_getp",
                            &[ptr_type.into(), i32_type.into(), ptr_type.into()],
                            Some(ptr_type.into()),
                        );
                        let result = self.builder.build_call(
                            getter,
                            &[obj_val.into(), hfield_val.into(), type_ptr.into()],
                            "dynget_p",
                        )?;
                        self.builder.build_store(
                            registers[dst.0 as usize],
                            result.try_as_basic_value().basic().unwrap(),
                        )?;
                    }
                }
            }

            // --- DynSet: dynamic field set ---
            Opcode::DynSet { obj, field, src } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let i32_type = self.context.i32_type();

                let obj_val =
                    self.builder
                        .build_load(ptr_type, registers[obj.0 as usize], "dynset_obj")?;
                let field_name = &self.bytecode.strings[field.0].clone();
                let hfield = hl_hash_utf8(field_name);
                let hfield_val = i32_type.const_int(hfield as u64, true);

                let src_type_idx = f.regs[src.0 as usize].0;
                let src_kind = self.types_[src_type_idx].kind;
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "dynset_src",
                )?;

                match src_kind {
                    hl_type_kind_HF64 => {
                        let setter = self.declare_native(
                            "hlp_dyn_setd",
                            &[
                                ptr_type.into(),
                                i32_type.into(),
                                self.context.f64_type().into(),
                            ],
                            None,
                        );
                        self.builder.build_call(
                            setter,
                            &[obj_val.into(), hfield_val.into(), src_val.into()],
                            "dynset_d",
                        )?;
                    }
                    hl_type_kind_HF32 => {
                        let setter = self.declare_native(
                            "hlp_dyn_setf",
                            &[
                                ptr_type.into(),
                                i32_type.into(),
                                self.context.f32_type().into(),
                            ],
                            None,
                        );
                        self.builder.build_call(
                            setter,
                            &[obj_val.into(), hfield_val.into(), src_val.into()],
                            "dynset_f",
                        )?;
                    }
                    hl_type_kind_HI64 => {
                        let setter = self.declare_native(
                            "hlp_dyn_seti64",
                            &[
                                ptr_type.into(),
                                i32_type.into(),
                                self.context.i64_type().into(),
                            ],
                            None,
                        );
                        self.builder.build_call(
                            setter,
                            &[obj_val.into(), hfield_val.into(), src_val.into()],
                            "dynset_i64",
                        )?;
                    }
                    hl_type_kind_HI32 | hl_type_kind_HBOOL | hl_type_kind_HUI8
                    | hl_type_kind_HUI16 => {
                        let type_ptr = self
                            .get_initialized_type(src_type_idx)?
                            .into_pointer_value();
                        // All four kinds share one setter, whose value
                        // parameter is i32 — but their registers are not: HBOOL
                        // loads as i1, HUI8 as i8, HUI16 as i16. Passing those
                        // straight through builds a call the LLVM verifier
                        // rejects ("Call parameter type does not match function
                        // signature"), which fails the whole module and drops
                        // the program back to the interpreter. Widen first;
                        // all three narrow kinds are unsigned, so zero-extend.
                        let src_int = src_val.into_int_value();
                        let src_i32 = if src_int.get_type().get_bit_width() < 32 {
                            self.builder
                                .build_int_z_extend(src_int, i32_type, "dynset_src_i32")?
                        } else {
                            src_int
                        };
                        let setter = self.declare_native(
                            "hlp_dyn_seti",
                            &[
                                ptr_type.into(),
                                i32_type.into(),
                                ptr_type.into(),
                                i32_type.into(),
                            ],
                            None,
                        );
                        self.builder.build_call(
                            setter,
                            &[
                                obj_val.into(),
                                hfield_val.into(),
                                type_ptr.into(),
                                src_i32.into(),
                            ],
                            "dynset_i",
                        )?;
                    }
                    _ => {
                        // Pointer types: hlp_dyn_setp(obj, hfield, type, value)
                        let type_ptr = self
                            .get_initialized_type(src_type_idx)?
                            .into_pointer_value();
                        let setter = self.declare_native(
                            "hlp_dyn_setp",
                            &[
                                ptr_type.into(),
                                i32_type.into(),
                                ptr_type.into(),
                                ptr_type.into(),
                            ],
                            None,
                        );
                        self.builder.build_call(
                            setter,
                            &[
                                obj_val.into(),
                                hfield_val.into(),
                                type_ptr.into(),
                                src_val.into(),
                            ],
                            "dynset_p",
                        )?;
                    }
                }
            }

            // --- Bytes: load bytes constant ---
            Opcode::Bytes { dst, ptr } => {
                if let Some(bytes_global) = self.ensure_bytes_global(ptr.0) {
                    self.builder
                        .build_store(registers[dst.0 as usize], bytes_global.as_pointer_value())?;
                } else {
                    let null_ptr = self.context.ptr_type(AddressSpace::default()).const_null();
                    self.builder
                        .build_store(registers[dst.0 as usize], null_ptr)?;
                }
            }

            // --- Enum opcodes ---
            Opcode::EnumAlloc { dst, construct } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let i32_type = self.context.i32_type();
                let type_index = f.regs[dst.0 as usize].0;
                let type_ptr = self.get_initialized_type(type_index)?.into_pointer_value();

                let alloc_enum = self.declare_native(
                    "hlp_alloc_enum",
                    &[ptr_type.into(), i32_type.into()],
                    Some(ptr_type.into()),
                );
                let construct_val = i32_type.const_int(construct.0 as u64, false);
                let result = self.builder.build_call(
                    alloc_enum,
                    &[type_ptr.into(), construct_val.into()],
                    "enum_alloc",
                )?;
                self.builder.build_store(
                    registers[dst.0 as usize],
                    result.try_as_basic_value().basic().unwrap(),
                )?;
            }
            Opcode::MakeEnum {
                dst,
                construct,
                args,
            } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let i32_type = self.context.i32_type();
                let i8_type = self.context.i8_type();
                let type_index = f.regs[dst.0 as usize].0;
                let type_ptr = self.get_initialized_type(type_index)?.into_pointer_value();

                // Allocate the enum
                let alloc_enum = self.declare_native(
                    "hlp_alloc_enum",
                    &[ptr_type.into(), i32_type.into()],
                    Some(ptr_type.into()),
                );
                let construct_val = i32_type.const_int(construct.0 as u64, false);
                let venum_ptr = self
                    .builder
                    .build_call(
                        alloc_enum,
                        &[type_ptr.into(), construct_val.into()],
                        "make_enum",
                    )?
                    .try_as_basic_value()
                    .basic()
                    .unwrap()
                    .into_pointer_value();

                // Write each arg at its pre-computed offset
                let tenum = self.types_[type_index]
                    .tenum
                    .as_ref()
                    .ok_or_else(|| anyhow!("MakeEnum: type {} is not an enum", type_index))?;
                let construct_info = &tenum.constructs[construct.0];

                for (j, arg) in args.iter().enumerate() {
                    let arg_val = self.builder.build_load(
                        reg_types[arg.0 as usize],
                        registers[arg.0 as usize],
                        &format!("make_enum_arg_{}", j),
                    )?;
                    let offset = construct_info.offsets[j] as u64;
                    let param_ptr = unsafe {
                        self.builder.build_gep(
                            i8_type,
                            venum_ptr,
                            &[self.context.i64_type().const_int(offset, false)],
                            &format!("make_enum_param_{}", j),
                        )?
                    };
                    self.builder.build_store(param_ptr, arg_val)?;
                }

                self.builder
                    .build_store(registers[dst.0 as usize], venum_ptr)?;
            }
            Opcode::EnumIndex { dst, value } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let venum_ptr = self
                    .builder
                    .build_load(ptr_type, registers[value.0 as usize], "enumidx_ptr")?
                    .into_pointer_value();
                // venum.index is i32 at offset 8
                let index_gep = unsafe {
                    self.builder.build_gep(
                        self.context.i8_type(),
                        venum_ptr,
                        &[self.context.i64_type().const_int(8, false)],
                        "enumidx_gep",
                    )?
                };
                let index_val =
                    self.builder
                        .build_load(self.context.i32_type(), index_gep, "enumidx_val")?;
                self.builder
                    .build_store(registers[dst.0 as usize], index_val)?;
            }
            Opcode::EnumField {
                dst,
                value,
                construct,
                field,
            } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let venum_ptr = self
                    .builder
                    .build_load(ptr_type, registers[value.0 as usize], "enumfield_ptr")?
                    .into_pointer_value();

                let value_type_idx = f.regs[value.0 as usize].0;
                let tenum = self.types_[value_type_idx]
                    .tenum
                    .as_ref()
                    .ok_or_else(|| anyhow!("EnumField: type {} is not an enum", value_type_idx))?;
                let construct_info = &tenum.constructs[construct.0];
                let offset = construct_info.offsets[field.0] as u64;

                let param_ptr = unsafe {
                    self.builder.build_gep(
                        self.context.i8_type(),
                        venum_ptr,
                        &[self.context.i64_type().const_int(offset, false)],
                        "enumfield_gep",
                    )?
                };
                let val = self.builder.build_load(
                    reg_types[dst.0 as usize],
                    param_ptr,
                    "enumfield_val",
                )?;
                self.builder.build_store(registers[dst.0 as usize], val)?;
            }
            Opcode::SetEnumField { value, field, src } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let venum_ptr = self
                    .builder
                    .build_load(ptr_type, registers[value.0 as usize], "setenumfield_ptr")?
                    .into_pointer_value();
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "setenumfield_val",
                )?;

                // Scan backwards to find the preceding EnumAlloc targeting the same register
                let value_type_idx = f.regs[value.0 as usize].0;
                let tenum = self.types_[value_type_idx].tenum.as_ref().ok_or_else(|| {
                    anyhow!("SetEnumField: type {} is not an enum", value_type_idx)
                })?;

                // Find construct index from preceding opcodes
                let mut construct_idx = 0usize; // default to 0
                for prev_i in (0..i).rev() {
                    match &f.ops[prev_i] {
                        Opcode::EnumAlloc { dst, construct } if dst.0 == value.0 => {
                            construct_idx = construct.0;
                            break;
                        }
                        Opcode::MakeEnum { dst, construct, .. } if dst.0 == value.0 => {
                            construct_idx = construct.0;
                            break;
                        }
                        _ => {}
                    }
                }

                let construct_info = &tenum.constructs[construct_idx];
                let offset = construct_info.offsets[field.0] as u64;
                let param_ptr = unsafe {
                    self.builder.build_gep(
                        self.context.i8_type(),
                        venum_ptr,
                        &[self.context.i64_type().const_int(offset, false)],
                        "setenumfield_gep",
                    )?
                };
                self.builder.build_store(param_ptr, src_val)?;
            }

            // --- Memory access ---
            Opcode::GetI8 { dst, bytes, index } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let base = self
                    .builder
                    .build_load(ptr_type, registers[bytes.0 as usize], "geti8_base")?
                    .into_pointer_value();
                let idx = self
                    .builder
                    .build_load(
                        self.context.i32_type(),
                        registers[index.0 as usize],
                        "geti8_idx",
                    )?
                    .into_int_value();
                let addr = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), base, &[idx], "geti8_addr")?
                };
                let val = self
                    .builder
                    .build_load(self.context.i8_type(), addr, "geti8_val")?
                    .into_int_value();
                let ext =
                    self.builder
                        .build_int_z_extend(val, self.context.i32_type(), "geti8_zext")?;
                self.builder.build_store(registers[dst.0 as usize], ext)?;
            }
            Opcode::GetI16 { dst, bytes, index } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let base = self
                    .builder
                    .build_load(ptr_type, registers[bytes.0 as usize], "geti16_base")?
                    .into_pointer_value();
                let idx = self
                    .builder
                    .build_load(
                        self.context.i32_type(),
                        registers[index.0 as usize],
                        "geti16_idx",
                    )?
                    .into_int_value();
                let addr = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), base, &[idx], "geti16_addr")?
                };
                let val = self
                    .builder
                    .build_load(self.context.i16_type(), addr, "geti16_val")?
                    .into_int_value();
                let ext =
                    self.builder
                        .build_int_z_extend(val, self.context.i32_type(), "geti16_zext")?;
                self.builder.build_store(registers[dst.0 as usize], ext)?;
            }
            Opcode::GetMem { dst, bytes, index } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let base = self
                    .builder
                    .build_load(ptr_type, registers[bytes.0 as usize], "getmem_base")?
                    .into_pointer_value();
                let idx = self
                    .builder
                    .build_load(
                        self.context.i32_type(),
                        registers[index.0 as usize],
                        "getmem_idx",
                    )?
                    .into_int_value();
                let addr = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), base, &[idx], "getmem_addr")?
                };
                let val = self
                    .builder
                    .build_load(reg_types[dst.0 as usize], addr, "getmem_val")?;
                if let Some(i) = val.as_instruction_value() {
                    self.tbaa.tag(i, self.tbaa.payload());
                }
                self.builder.build_store(registers[dst.0 as usize], val)?;
            }
            Opcode::SetI8 { bytes, index, src } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let base = self
                    .builder
                    .build_load(ptr_type, registers[bytes.0 as usize], "seti8_base")?
                    .into_pointer_value();
                let idx = self
                    .builder
                    .build_load(
                        self.context.i32_type(),
                        registers[index.0 as usize],
                        "seti8_idx",
                    )?
                    .into_int_value();
                let src_val = self
                    .builder
                    .build_load(
                        reg_types[src.0 as usize],
                        registers[src.0 as usize],
                        "seti8_src",
                    )?
                    .into_int_value();
                let addr = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), base, &[idx], "seti8_addr")?
                };
                let trunc = self.builder.build_int_truncate(
                    src_val,
                    self.context.i8_type(),
                    "seti8_trunc",
                )?;
                self.builder.build_store(addr, trunc)?;
            }
            Opcode::SetI16 { bytes, index, src } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let base = self
                    .builder
                    .build_load(ptr_type, registers[bytes.0 as usize], "seti16_base")?
                    .into_pointer_value();
                let idx = self
                    .builder
                    .build_load(
                        self.context.i32_type(),
                        registers[index.0 as usize],
                        "seti16_idx",
                    )?
                    .into_int_value();
                let src_val = self
                    .builder
                    .build_load(
                        reg_types[src.0 as usize],
                        registers[src.0 as usize],
                        "seti16_src",
                    )?
                    .into_int_value();
                let addr = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), base, &[idx], "seti16_addr")?
                };
                let trunc = self.builder.build_int_truncate(
                    src_val,
                    self.context.i16_type(),
                    "seti16_trunc",
                )?;
                self.builder.build_store(addr, trunc)?;
            }
            Opcode::SetMem { bytes, index, src } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let base = self
                    .builder
                    .build_load(ptr_type, registers[bytes.0 as usize], "setmem_base")?
                    .into_pointer_value();
                let idx = self
                    .builder
                    .build_load(
                        self.context.i32_type(),
                        registers[index.0 as usize],
                        "setmem_idx",
                    )?
                    .into_int_value();
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "setmem_src",
                )?;
                let addr = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), base, &[idx], "setmem_addr")?
                };
                let st = self.builder.build_store(addr, src_val)?;
                self.tbaa.tag(st, self.tbaa.payload());
            }

            // --- Array operations ---
            Opcode::SetArray { array, index, src } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let i32_type = self.context.i32_type();
                let i8_type = self.context.i8_type();

                let arr = self
                    .builder
                    .build_load(ptr_type, registers[array.0 as usize], "setarr_ptr")?
                    .into_pointer_value();
                let idx = self
                    .builder
                    .build_load(i32_type, registers[index.0 as usize], "setarr_idx")?
                    .into_int_value();
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "setarr_val",
                )?;

                // Data starts at offset 24 (sizeof(varray))
                let data_ptr = unsafe {
                    self.builder.build_gep(
                        i8_type,
                        arr,
                        &[self.context.i64_type().const_int(24, false)],
                        "setarr_data",
                    )?
                };

                // Element size from the source register's
                // kind, via the table `crate::layout` shares with the Cranelift
                // tier so the two cannot index an array differently.
                let src_type_idx = f.regs[src.0 as usize].0;
                let src_kind = self.types_[src_type_idx].kind;
                let elem_size: u64 = crate::layout::array_elem_size(src_kind) as u64;

                let elem_size_val = i32_type.const_int(elem_size, false);
                let byte_offset =
                    self.builder
                        .build_int_mul(idx, elem_size_val, "setarr_offset")?;
                let slot = unsafe {
                    self.builder
                        .build_gep(i8_type, data_ptr, &[byte_offset], "setarr_slot")?
                };
                let st = self.builder.build_store(slot, src_val)?;
                self.tbaa.tag(st, self.tbaa.payload());
            }
            Opcode::ArraySize { dst, array } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let arr = self
                    .builder
                    .build_load(ptr_type, registers[array.0 as usize], "arrsize_ptr")?
                    .into_pointer_value();
                // varray.size is at offset 16
                let size_gep = unsafe {
                    self.builder.build_gep(
                        self.context.i8_type(),
                        arr,
                        &[self.context.i64_type().const_int(16, false)],
                        "arrsize_gep",
                    )?
                };
                let size =
                    self.builder
                        .build_load(self.context.i32_type(), size_gep, "arrsize_val")?;
                if let Some(i) = size.as_instruction_value() {
                    self.tbaa.tag(i, self.tbaa.array_len());
                }
                self.builder.build_store(registers[dst.0 as usize], size)?;
            }

            // --- GetTID: get type kind ---
            Opcode::GetTID { dst, src } => {
                let src_val = self.builder.build_load(
                    reg_types[src.0 as usize],
                    registers[src.0 as usize],
                    "gettid_src",
                )?;
                let src_type_kind = self.types_[f.regs[src.0 as usize].0].kind;
                if src_val.is_pointer_value() {
                    let obj = src_val.into_pointer_value();
                    if src_type_kind == hl_type_kind_HTYPE {
                        // Source is hl_type* — kind is directly at offset 0
                        let kind =
                            self.builder
                                .build_load(self.context.i32_type(), obj, "gettid_kind")?;
                        self.builder.build_store(registers[dst.0 as usize], kind)?;
                    } else {
                        // Source is an object — load obj->t (offset 0), then t->kind (offset 0)
                        let ptr_type = self.context.ptr_type(AddressSpace::default());
                        let t_ptr = self
                            .builder
                            .build_load(ptr_type, obj, "gettid_type")?
                            .into_pointer_value();
                        let kind = self.builder.build_load(
                            self.context.i32_type(),
                            t_ptr,
                            "gettid_kind",
                        )?;
                        self.builder.build_store(registers[dst.0 as usize], kind)?;
                    }
                } else {
                    // Compile-time: type kind is known
                    let type_idx = f.regs[src.0 as usize].0;
                    let kind = self.types_[type_idx].kind;
                    let kind_val = self.context.i32_type().const_int(kind as u64, false);
                    self.builder
                        .build_store(registers[dst.0 as usize], kind_val)?;
                }
            }

            // --- Assert: throw "assert", catchably ---
            // Upstream OAssert calls hl_assert() -> hl_error("assert"). The
            // unit suite EXECUTES this opcode on purpose (assert-testing
            // cases), so `unreachable` here was a licence to miscompile a
            // path that runs. hlp_error longjmps to the active trap.
            Opcode::Assert => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let err_fn_type = self.context.void_type().fn_type(&[ptr_type.into()], true);
                let err_ptr = self.error_function_ptr()?;
                let msg_ptr = self.utf16_message("assert")?;
                self.builder.build_indirect_call(
                    err_fn_type,
                    err_ptr,
                    &[msg_ptr.into()],
                    "assert_throw",
                )?;
                self.builder.build_unreachable()?;
            }

            // --- Prefetch: emit target-specific cache hint via inline asm ---
            Opcode::Prefetch { value, field, mode } => {
                let _ = field; // field offset elision is safe; prefetch is purely a hint
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let base = self
                    .builder
                    .build_load(ptr_type, registers[value.0 as usize], "prefetch_ptr")?
                    .into_pointer_value();
                let void_type = self.context.void_type();
                let fn_type = void_type.fn_type(&[ptr_type.into()], false);

                #[cfg(target_arch = "x86_64")]
                let hint = match mode {
                    0 => "prefetcht0 ($0)",
                    1 => "prefetcht1 ($0)",
                    2 => "prefetcht2 ($0)",
                    _ => "prefetchnta ($0)",
                };
                #[cfg(target_arch = "aarch64")]
                let hint = match mode {
                    0 => "prfm pldl1keep, [$0]",
                    1 => "prfm pldl2keep, [$0]",
                    2 => "prfm pldl3keep, [$0]",
                    _ => "prfm pldl1strm, [$0]",
                };
                // Fallback for other architectures: no-op
                #[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
                let hint = {
                    let _ = mode;
                    ""
                };

                if !hint.is_empty() {
                    let asm_val = self.context.create_inline_asm(
                        fn_type,
                        hint.to_string(),
                        "r".to_string(),
                        true,
                        false,
                        Some(inkwell::InlineAsmDialect::ATT),
                        false,
                    );
                    self.builder.build_indirect_call(
                        fn_type,
                        asm_val,
                        &[base.into()],
                        "prefetch",
                    )?;
                }
            }
            // --- Asm: inline assembly byte emission ---
            //
            // HashLink OAsm modes:
            //   0 → emit raw byte (p2) into code stream
            //   1 → mark physical register (p2) as clobbered
            //   2 → load VM register into physical register (p2)
            //   3 → store physical register (p2) into VM register
            //   4 → naked function (strip prologue; must be first opcode)
            //
            // Modes 1-3 are register-allocator directives for HashLink's custom JIT;
            // LLVM handles register allocation automatically so these are no-ops.
            // Mode 0 emits raw bytes via `.byte` — works on all LLVM targets.
            Opcode::Asm { mode, value, reg } => {
                let _ = reg;
                match mode {
                    0 => {
                        let byte = *value as u8;
                        let void_type = self.context.void_type();
                        let fn_type = void_type.fn_type(&[], false);
                        let asm_val = self.context.create_inline_asm(
                            fn_type,
                            format!(".byte 0x{byte:02x}"),
                            String::new(),
                            true,  // side effects
                            false, // align stack
                            Some(inkwell::InlineAsmDialect::ATT),
                            false, // can_throw
                        );
                        self.builder
                            .build_indirect_call(fn_type, asm_val, &[], "")?;
                    }
                    1 | 2 | 3 | 4 => {
                        // Register hints / naked: LLVM handles allocation automatically.
                    }
                    _ => {}
                }
            }
            // --- RefData: extract value pointer from vdynamic (offset 8) ---
            Opcode::RefData { dst, src } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let obj = self
                    .builder
                    .build_load(ptr_type, registers[src.0 as usize], "refdata_src")?
                    .into_pointer_value();
                let data_gep = unsafe {
                    self.builder.build_gep(
                        self.context.i8_type(),
                        obj,
                        &[self.context.i64_type().const_int(8, false)],
                        "refdata_gep",
                    )?
                };
                let data = self.builder.build_load(ptr_type, data_gep, "refdata_val")?;
                self.builder.build_store(registers[dst.0 as usize], data)?;
            }
            // --- RefOffset: pointer + byte offset ---
            Opcode::RefOffset { dst, reg, offset } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let base = self
                    .builder
                    .build_load(ptr_type, registers[reg.0 as usize], "refoff_base")?
                    .into_pointer_value();
                let off = self
                    .builder
                    .build_load(
                        self.context.i32_type(),
                        registers[offset.0 as usize],
                        "refoff_off",
                    )?
                    .into_int_value();
                let result = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), base, &[off], "refoff_result")?
                };
                self.builder
                    .build_store(registers[dst.0 as usize], result)?;
            }

            _ => return Err(anyhow!("Opcode {:?} not yet implemented in JIT", op)),
        }
        Ok(())
    }

    /// Helper: emit a comparison jump (used by JSLt, JSGte, JEq, JNotEq, etc.)
    fn emit_comparison_jump(
        &self,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        a: &crate::opcodes::Reg,
        b: &crate::opcodes::Reg,
        // The bindgen alias, not a bare integer: MSVC types the C enum i32
        // where clang types it u32, so only the alias compiles on both.
        a_kind: crate::hl::hl_type_kind,
        int_pred: IntPredicate,
        float_pred: FloatPredicate,
        i: usize,
        offset: i32,
        opcode_blocks: &[BasicBlock<'ctx>],
    ) -> Result<()> {
        let a_val =
            self.builder
                .build_load(reg_types[a.0 as usize], registers[a.0 as usize], "cmp_a")?;
        let b_val =
            self.builder
                .build_load(reg_types[b.0 as usize], registers[b.0 as usize], "cmp_b")?;
        let cmp = match a_val.get_type().as_any_type_enum() {
            AnyTypeEnum::IntType(_) => self.builder.build_int_compare(
                int_pred,
                a_val.into_int_value(),
                b_val.into_int_value(),
                "cmp",
            )?,
            AnyTypeEnum::FloatType(_) => self.builder.build_float_compare(
                float_pred,
                a_val.into_float_value(),
                b_val.into_float_value(),
                "cmp",
            )?,
            AnyTypeEnum::PointerType(_) => {
                // A String's identity is not its value, and hlp_dyn_compare is the
                // one place that knows the difference: it uses the type's compareFun
                // when there is one, then compares the UTF-16 payload of
                // String-shaped objects, and only then falls back to pointers — so
                // routing HOBJ through it fixes `a == b` on strings while leaving
                // identity semantics intact for every other object.
                //
                // Passing an object pointer as a vdynamic* is sound because an
                // object's first word IS its hl_type*, which is all dyn_compare
                // reads of it. HBYTES and HSTRUCT must NOT come here: a raw byte
                // buffer and a struct both lack that header, so dyn_compare would
                // read their payload as a type.
                if a_kind == hl_type_kind_HDYN
                    || a_kind == hl_type_kind_HNULL
                    || a_kind == hl_type_kind_HOBJ
                {
                    let ptr_type = self.context.ptr_type(AddressSpace::default());
                    let i32_type = self.context.i32_type();
                    let dyn_compare = self.declare_native(
                        "hlp_dyn_compare",
                        &[ptr_type.into(), ptr_type.into()],
                        Some(i32_type.into()),
                    );
                    let result = self
                        .builder
                        .build_call(dyn_compare, &[a_val.into(), b_val.into()], "dyn_cmp")?
                        .try_as_basic_value()
                        .basic()
                        .unwrap()
                        .into_int_value();
                    let zero = i32_type.const_int(0, false);
                    self.builder
                        .build_int_compare(int_pred, result, zero, "cmp")?
                } else {
                    // Non-dynamic pointer: identity comparison
                    let a_int = self.builder.build_ptr_to_int(
                        a_val.into_pointer_value(),
                        self.context.i64_type(),
                        "a_int",
                    )?;
                    let b_int = self.builder.build_ptr_to_int(
                        b_val.into_pointer_value(),
                        self.context.i64_type(),
                        "b_int",
                    )?;
                    self.builder
                        .build_int_compare(int_pred, a_int, b_int, "cmp")?
                }
            }
            _ => return Err(anyhow!("Unsupported types for comparison jump")),
        };
        let target = opcode_blocks[(i as i32 + 1 + offset) as usize];
        let next = opcode_blocks[i + 1];
        self.builder.build_conditional_branch(cmp, target, next)?;
        Ok(())
    }

    /// Helper: emit a binary arithmetic operation
    fn emit_binary_op<F>(
        &self,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        dst: &crate::opcodes::Reg,
        a: &crate::opcodes::Reg,
        b: &crate::opcodes::Reg,
        _name: &str,
        op_fn: F,
    ) -> Result<()>
    where
        F: FnOnce(
            &Builder<'ctx>,
            BasicValueEnum<'ctx>,
            BasicValueEnum<'ctx>,
        ) -> Result<BasicValueEnum<'ctx>>,
    {
        let a_val =
            self.builder
                .build_load(reg_types[a.0 as usize], registers[a.0 as usize], "a_val")?;
        let b_val =
            self.builder
                .build_load(reg_types[b.0 as usize], registers[b.0 as usize], "b_val")?;
        let result = op_fn(&self.builder, a_val, b_val)?;
        self.builder
            .build_store(registers[dst.0 as usize], result)?;
        Ok(())
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
            let call = self.builder.build_indirect_call(fn_type, fn_ptr, args, name)?;
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
        // Hoist the spill buffer to the entry block so a guarded call inside
        // a hot loop does not grow the stack per iteration.
        let buf = {
            let saved = self.builder.get_insert_block();
            let entry = function.get_first_basic_block().unwrap();
            match entry.get_first_instruction() {
                Some(first) => self.builder.position_before(&first),
                None => self.builder.position_at_end(entry),
            }
            let buf = self.builder.build_alloca(
                i64_type.array_type(nargs.max(1)),
                &format!("{}_argbuf", name),
            )?;
            if let Some(block) = saved {
                self.builder.position_at_end(block);
            }
            buf
        };
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

        let unsealed: Vec<(FunctionValue<'ctx>, Vec<inkwell::basic_block::BasicBlock<'ctx>>)> =
            self.module
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
    fn utf16_message(&self, message: &str) -> Result<PointerValue<'ctx>> {
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
    fn setjmp_ptr(&self) -> Result<PointerValue<'ctx>> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        if self.aot {
            const SYMBOL: &str = "_setjmp";
            let function = self.module.get_function(SYMBOL).unwrap_or_else(|| {
                let declared = self.module.add_function(
                    SYMBOL,
                    self.context.i32_type().fn_type(&[ptr_type.into()], false),
                    Some(inkwell::module::Linkage::External),
                );
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
    fn function_name(&mut self, findex: u32) -> Option<String> {
        self.ensure_name_map();
        self.findex_to_name.as_ref().unwrap().get(&findex).cloned()
    }

    fn ensure_name_map(&mut self) {
        if self.name_to_findex.is_some() {
            return;
        }
        let by_findex =
            crate::types::function_keys(&self.bytecode.types, &self.bytecode.functions);
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
                &[self.context.i64_type().const_int(16, false)],
                "vobj_proto_gep",
            )?
        };
        let cached = self
            .builder
            .build_load(ptr_type, slot, "vobj_proto_cached")?
            .into_pointer_value();
        let missing = self.builder.build_is_null(cached, "vobj_proto_missing")?;

        let init_bb = self
            .context
            .append_basic_block(function, "vobj_proto_init");
        let done_bb = self
            .context
            .append_basic_block(function, "vobj_proto_done");
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
        const _: () = {
            assert!(std::mem::size_of::<crate::hl::vclosure>() == 32);
            assert!(std::mem::offset_of!(crate::hl::_vclosure, t) == 0);
            assert!(std::mem::offset_of!(crate::hl::_vclosure, fun) == 8);
            assert!(std::mem::offset_of!(crate::hl::_vclosure, hasValue) == 16);
            assert!(std::mem::offset_of!(crate::hl::_vclosure, stackCount) == 20);
            assert!(std::mem::offset_of!(crate::hl::_vclosure, value) == 24);
        };
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let i32_type = self.context.i32_type();
        let (target, _) = self.get_or_create_function_value(findex)?;

        let value = self.context.const_struct(
            &[
                type_ptr.into(),
                target.as_global_value().as_pointer_value().into(),
                i32_type.const_zero().into(), // hasValue: unbound
                i32_type.const_zero().into(), // stackCount
                ptr_type.const_null().into(), // value
            ],
            false,
        );
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
        global.set_alignment(8);
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
    fn bind_module_declarations(
        &self,
        module: &inkwell::module::Module<'ctx>,
        label: &str,
    ) -> Result<()> {
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
            let engine_addr = self.execution_engine.get_function_address(symbol).ok();
            let tier_addr = self
                .bytecode
                .functions
                .iter()
                .find(|f| f.name() == symbol)
                .and_then(|f| self.live_function_address(f.findex as usize));
            match engine_addr.filter(|&addr| addr != 0).or(tier_addr) {
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

        if let Some(offset) = crate::layout::field_offset(&self.types_, obj_type_index, field_index)
        {
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
                &[self.context.i64_type().const_int(40, false)],
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

    /// Emit `intr` inline over `arg`, or `Ok(None)` if the intrinsic
    /// declaration could not be obtained.
    ///
    /// The saturating float→int conversions are not an optimization but a
    /// correctness requirement: `ash_std` casts with Rust's `as`, which clamps
    /// out-of-range values and maps NaN to zero, whereas `fptosi` is poison on
    /// exactly those inputs. See [`crate::intrinsics`].
    fn emit_native_intrinsic(
        &self,
        intr: crate::intrinsics::NativeIntrinsic,
        arg: BasicValueEnum<'ctx>,
    ) -> Result<Option<BasicValueEnum<'ctx>>> {
        use crate::intrinsics::NativeIntrinsic as NI;
        use inkwell::intrinsics::Intrinsic;

        let f64_ty = self.context.f64_type();
        let x = arg.into_float_value();

        // `math_isnan` / `math_isfinite` are comparisons, not intrinsic calls.
        match intr {
            NI::IsNaN | NI::IsFinite => {
                let pred = match intr {
                    // x != x is true only for NaN.
                    NI::IsNaN => FloatPredicate::UNO,
                    // ORD additionally excludes NaN, which is what `is_finite`
                    // means on top of the magnitude test below.
                    _ => FloatPredicate::ONE,
                };
                let bit = if intr == NI::IsNaN {
                    self.builder.build_float_compare(pred, x, x, "isnan")?
                } else {
                    let abs = self.call_float_intrinsic("llvm.fabs", x, "fabs")?;
                    let inf = f64_ty.const_float(f64::INFINITY);
                    let finite = self.builder.build_float_compare(
                        FloatPredicate::ONE,
                        abs,
                        inf,
                        "notinf",
                    )?;
                    let ord = self
                        .builder
                        .build_float_compare(FloatPredicate::ORD, x, x, "ord")?;
                    self.builder.build_and(finite, ord, "isfinite")?
                };
                // HL bools are byte-wide in the ABI; the comparison yields i1.
                let b = self
                    .builder
                    .build_int_z_extend(bit, self.context.bool_type(), "b")?;
                return Ok(Some(b.into()));
            }
            _ => {}
        }

        // Everything else is floor/ceil/sqrt/fabs, optionally over x + 0.5, and
        // optionally converted to i32 afterwards.
        let base = match intr {
            NI::Sqrt => self.call_float_intrinsic("llvm.sqrt", x, "sqrt")?,
            NI::Abs => self.call_float_intrinsic("llvm.fabs", x, "fabs")?,
            NI::Floor | NI::FloorToI32 => self.call_float_intrinsic("llvm.floor", x, "floor")?,
            NI::Ceil | NI::CeilToI32 => self.call_float_intrinsic("llvm.ceil", x, "ceil")?,
            NI::RoundHalfUp | NI::RoundHalfUpToI32 => {
                let half = f64_ty.const_float(0.5);
                let shifted = self.builder.build_float_add(x, half, "half")?;
                self.call_float_intrinsic("llvm.floor", shifted, "floor")?
            }
            NI::IsNaN | NI::IsFinite => unreachable!("handled above"),
        };

        if !intr.returns_i32() {
            return Ok(Some(base.into()));
        }

        let i32_ty = self.context.i32_type();
        let Some(sat) = Intrinsic::find("llvm.fptosi.sat") else {
            return Ok(None);
        };
        let Some(decl) = sat.get_declaration(&self.module, &[i32_ty.into(), f64_ty.into()]) else {
            return Ok(None);
        };
        let call = self
            .builder
            .build_call(decl, &[base.into()], "fptosi_sat")?;
        Ok(call.try_as_basic_value().basic())
    }

    /// Call a unary `f64 -> f64` LLVM intrinsic by name.
    fn call_float_intrinsic(
        &self,
        name: &str,
        x: inkwell::values::FloatValue<'ctx>,
        label: &str,
    ) -> Result<inkwell::values::FloatValue<'ctx>> {
        use inkwell::intrinsics::Intrinsic;
        let f64_ty = self.context.f64_type();
        let intr =
            Intrinsic::find(name).ok_or_else(|| anyhow!("LLVM intrinsic {name} not found"))?;
        let decl = intr
            .get_declaration(&self.module, &[f64_ty.into()])
            .ok_or_else(|| anyhow!("no declaration for {name}"))?;
        let call = self.builder.build_call(decl, &[x.into()], label)?;
        Ok(call
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| anyhow!("{name} returned void"))?
            .into_float_value())
    }

    /// The intrinsic replacing a one-argument call to `findex`, if that findex
    /// is a native primitive this backend can emit directly.
    fn native_intrinsic_for(&self, findex: usize) -> Option<crate::intrinsics::NativeIntrinsic> {
        let FuncPtr::Native(native) = self.findexes.get(&findex)? else {
            return None;
        };
        crate::intrinsics::lookup(native.lib.as_str(), native.name.as_str())
    }

    pub(crate) fn init_native_func(
        &mut self,
        native_func: &HLNative,
    ) -> Result<FunctionValue<'ctx>> {
        let lib = native_func.lib.as_str();
        let name: String = format!("hlp_{}", native_func.name);

        let type_fun = self.bytecode.types[native_func.type_.0]
            .fun
            .clone()
            .expect("expected to get function type");
        let func_type = self.create_function_type(&type_fun)?;

        if self.aot {
            // `std` primitives are plain `#[no_mangle]` exports of the runtime
            // this object links against, so the symbol IS the name. An HDLL
            // primitive is not: it is reached through a DEFINE_PRIM resolver
            // in a shared library, and there is no shared library to load.
            let clean = lib.strip_prefix('?').unwrap_or(lib);
            if clean != "std" {
                return Err(anyhow!(
                    "cannot emit native {lib}@{} ahead of time: HDLL primitives resolve through dlopen",
                    native_func.name
                ));
            }
            let caller_name = format!("{}_{}_caller", lib, name);
            return self.generate_native_caller_to_symbol(&caller_name, func_type, &name);
        }

        let func_addr = match self.native_function_resolver.resolve_function(lib, &name) {
            Ok(addr) => addr as usize,
            Err(resolve_err) => {
                // Unresolved native. HashLink maps these to a stub that errors
                // at CALL time (disabled_primitive in hl's module.c), and the
                // interpreter resolves natives lazily per call — so failing
                // the whole compile here blacklists every hot function that
                // merely references (but never executes) an unimplemented
                // native. With ASH_JIT_NATIVE_TRAPS=1, generate a trap that
                // throws via hlp_error if the code path is actually taken,
                // letting such functions promote.
                //
                // Default is OFF: on game.hl the promotions this unlocks are
                // the FIRST ever, and the promoted code promptly dies with
                // SIGBUS at fault_addr = findex+1 — a call through a function
                // pointer slot still holding the interpreter's stub sentinel
                // (vtables/closures built from the shared module_ctx
                // functions_ptrs). Until JIT call sites guard against stub
                // sentinels, keeping the compile-time failure preserves the
                // previous stable blacklist behavior.
                if native_traps_enabled() {
                    eprintln!(
                        "[ash] native {}@{} unresolved ({}); generating call-time trap",
                        lib, name, resolve_err
                    );
                    return self.generate_missing_native_trap(lib, &name, func_type);
                }
                return Err(resolve_err);
            }
        };

        let caller_name = format!("{}_{}_caller", lib, name);
        let native_caller =
            self.generate_native_caller_with_addr(&caller_name, func_type, func_addr)?;

        debug_assert!(native_caller.verify(true));

        Ok(native_caller)
    }

    /// Build a caller-shaped function for a native that failed to resolve.
    /// Invoking it throws an HL error ("Unresolved native lib@name") via
    /// hlp_error — matching interpreter semantics, where native resolution
    /// happens lazily at call time and only an executed call can fail.
    /// The AOT counterpart of `generate_native_caller_with_addr`: the same
    /// forwarding thunk, but calling a symbol instead of an address.
    ///
    /// A bare declaration would be simpler and is what this did first. It
    /// does not survive: an unused declaration is dead, the module cleanup
    /// deletes it, and the handle cached in `func_cache` is left dangling --
    /// which surfaces as a fault inside `LLVMCountBasicBlocks` several
    /// functions later. A thunk with external linkage is never dead, and the
    /// inliner folds it into its callers anyway.
    fn generate_native_caller_to_symbol(
        &self,
        caller_name: &str,
        fn_type: FunctionType<'ctx>,
        symbol: &str,
    ) -> Result<FunctionValue<'ctx>> {
        if let Some(existing) = self.module.get_function(caller_name) {
            return Ok(existing);
        }
        let callee = self.aot_runtime_fn(symbol, fn_type);

        let saved_block = self.builder.get_insert_block();
        let function = self.module.add_function(caller_name, fn_type, None);
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        let args: Vec<BasicMetadataValueEnum> =
            function.get_param_iter().map(|arg| arg.into()).collect();
        let call = self.builder.build_call(callee, &args, "call")?;
        match call.try_as_basic_value().basic() {
            Some(value) => self.builder.build_return(Some(&value))?,
            None => self.builder.build_return(None)?,
        };

        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }
        Ok(function)
    }

    fn generate_missing_native_trap(
        &self,
        lib: &str,
        name: &str,
        fn_type: FunctionType<'ctx>,
    ) -> Result<FunctionValue<'ctx>> {
        let hlp_error_addr = self
            .native_function_resolver
            .resolve_function("std", "hlp_error")
            .map_err(|e| anyhow!("cannot build missing-native trap (no hlp_error): {}", e))?
            as usize;

        // Leak a NUL-terminated UTF-16 message; the JIT code embeds its address.
        let msg: Vec<u16> = format!("Unresolved native {}@{}", lib, name)
            .encode_utf16()
            .chain(std::iter::once(0))
            .collect();
        let msg_addr = Box::leak(msg.into_boxed_slice()).as_ptr() as u64;

        let saved_block = self.builder.get_insert_block();
        let function =
            self.module
                .add_function(&format!("{}_{}_missing", lib, name), fn_type, None);
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        let ptr_type = self.context.ptr_type(AddressSpace::default());
        // hlp_error(msg, ...) is variadic; we pass only the named arg.
        let err_fn_type = self.context.void_type().fn_type(&[ptr_type.into()], true);
        let err_ptr = self.builder.build_int_to_ptr(
            self.context
                .i64_type()
                .const_int(hlp_error_addr as u64, false),
            ptr_type,
            "hlp_error",
        )?;
        let msg_ptr = self.builder.build_int_to_ptr(
            self.context.i64_type().const_int(msg_addr, false),
            ptr_type,
            "msg",
        )?;
        self.builder
            .build_indirect_call(err_fn_type, err_ptr, &[msg_ptr.into()], "trap")?;
        // hlp_error longjmps to the active trap (or aborts); never returns.
        self.builder.build_unreachable()?;

        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }

        debug_assert!(function.verify(true));

        Ok(function)
    }

    // fn generate_std_lib_func(&mut self, lib: &str, name: &str) -> Result<FunctionValue<'ctx>> {
    //     let name: String = format!("hlp_{}", name);

    //     let func_value = self.declare_native_function(lib, name.as_str(), native_func)?;

    //     let func_ptr = self
    //         .native_function_resolver
    //         .resolve_function(lib, name.as_str())?;

    //     // Add function mapping
    //     self.execution_engine
    //         .add_global_mapping(&func_value, func_ptr as usize);

    //     let native_caller =
    //         self.generate_native_caller_function(&format!("{}_{}_caller", lib, name), func_value)?;

    //     debug_assert!(native_caller.verify(true));
    //     // println!("{}", native_caller.print_to_string().to_string());

    //     Ok(native_caller)
    // }

    fn get_native_func(&self, native: &HLNative) -> Result<&FunctionValue<'ctx>> {
        if let Some(func) = self.func_cache.get(&(native.findex as usize)) {
            return Ok(func);
        }

        Err(anyhow!(
            "Native function not found '{}::{}'",
            native.lib,
            native.name
        ))
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
        let noinline = self.context.create_enum_attribute(
            inkwell::attributes::Attribute::get_named_enum_kind_id("noinline"),
            0,
        );
        let optnone = self.context.create_enum_attribute(
            inkwell::attributes::Attribute::get_named_enum_kind_id("optnone"),
            0,
        );
        let mut n = 0;
        for (findex, fv) in &self.func_cache {
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
    fn build_safe_entry_wrapper(
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
        let setjmp_ptr = self.setjmp_ptr()?;
        let setjmp_call = self.builder.build_indirect_call(
            i32_type.fn_type(&[ptr_type.into()], false),
            setjmp_ptr,
            &[buf.into()],
            "outer_setjmp",
        )?;
        let returns_twice = self.context.create_enum_attribute(
            inkwell::attributes::Attribute::get_named_enum_kind_id("returns_twice"),
            0,
        );
        setjmp_call.add_attribute(inkwell::attributes::AttributeLoc::Function, returns_twice);
        let jumped = setjmp_call
            .try_as_basic_value()
            .basic()
            .unwrap()
            .into_int_value();
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
        crate::profile::register_jit_code(findex as u32, crate::profile::Tier::Llvm, addr as usize);
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
