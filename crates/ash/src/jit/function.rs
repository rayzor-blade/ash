use std::ffi::c_void;

use ash_macro::to_llvm;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::types::{
    AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType,
};
use inkwell::values::{
    AnyValue, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue,
};
use inkwell::{
    basic_block::BasicBlock, builder::Builder, AddressSpace, FloatPredicate, IntPredicate,
};

use super::module::{CompiledFunctionMeta, JITModule};
use crate::hl::{
    hl_obj_field, hl_runtime_obj, hl_type, hl_type_kind_HABSTRACT, hl_type_kind_HBOOL,
    hl_type_kind_HBYTES, hl_type_kind_HDYN, hl_type_kind_HDYNOBJ, hl_type_kind_HF32,
    hl_type_kind_HF64, hl_type_kind_HI32, hl_type_kind_HI64, hl_type_kind_HNULL,
    hl_type_kind_HOBJ, hl_type_kind_HSTRUCT, hl_type_kind_HTYPE, hl_type_kind_HUI16,
    hl_type_kind_HUI8, hl_type_kind_HVIRTUAL, hl_type_kind_HVOID, vdynamic, vdynobj, vvirtual,
};
use crate::opcodes::Opcode;
use crate::types::{HLNative, HLTypeFun, Str};
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
        let f = self
            .module
            .add_function(name, func_type, Some(inkwell::module::Linkage::External));
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
        let _phase = crate::profile::scope("llvm lower");
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

        if let FuncPtr::Fun(mut f) = fun_ptr {
            // Bytecode optimization before LLVM emission, plus the hot-reload
            // rewrite that turns direct calls to bytecode functions into
            // IndirectCall dispatch through functions_ptrs[findex]. Which
            // pipeline runs, and where the rewrite sits relative to it, is
            // decided in jit::air.
            let natives = self.hot_reload.then(|| self.native_findexes());
            crate::jit::air::optimize(&self.bytecode, &mut f, natives.as_ref());

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

            let (registers, reg_types) = self.allocate_registers(&f)?;
            self.load_function_arguments(&f, &function, &registers)?;

            self.translate_opcodes(&f, &registers, &reg_types)?;

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

        let (_function, is_placeholder) = self.get_or_create_function_value(findex)?;
        if is_placeholder {
            self.add_pending_compilation(findex);
        }

        self.compile_pending_functions_strict()?;
        self.compile_function(findex)?;
        self.compile_pending_functions_strict()?;

        // Optimize before asking for the address, because asking is what
        // forces codegen. Without this the tiered LLVM tier shipped raw
        // lowering output -- no mem2reg, no inlining, no GVN, no LICM -- and
        // lost to Cranelift on nbody by 1.5s, which is not a thing a top tier
        // should do. Only the whole-module path ran the middle end.
        {
            let _phase = crate::profile::scope("llvm middle-end (promote)");
            let excluded = self.shield_trap_functions_from_optimization();
            crate::profile::count("middle-end functions excluded (trap)", excluded as u64);
            super::module::run_middle_end(&self.module)?;
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

        // Resolve arg/return kinds from bytecode signature.
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
                let mut f = f.clone();

                // Same optimize-then-emit sequence as compile_function.
                let natives = self.hot_reload.then(|| self.native_findexes());
                crate::jit::air::optimize(&self.bytecode, &mut f, natives.as_ref());

                let function = self.create_function_declaration(&f)?;
                let basic_block = self.context.append_basic_block(function, "entry");
                self.builder.position_at_end(basic_block);

                let (registers, reg_types) = self.allocate_registers(&f)?;
                self.load_function_arguments(&f, &function, &registers)?;
                self.translate_opcodes(&f, &registers, &reg_types)?;

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
        body: &HLFunction,
    ) -> Result<u64> {
        let _phase = crate::profile::scope("llvm osr entry");
        if header_pc >= body.ops.len() {
            return Err(anyhow!("osr header {header_pc} past end of findex {findex}"));
        }
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
        self.create_constant_pool_globals();
        // `Opcode::New` fetches a pre-created native caller out of `func_cache`
        // by generated name, so emptying the cache is not enough on its own --
        // the new module needs its own copy of those declarations.
        let natives_ready = self.init_required_natives();

        let built = natives_ready.and_then(|()| self.build_osr_body(findex, header_pc, body, &name));

        self.builder.clear_insertion_position();
        let osr_module = std::mem::replace(&mut self.module, host_module);
        self.func_cache = host_funcs;
        self.int_globals = host_ints;
        self.float_globals = host_floats;
        self.string_globals = host_strings;
        self.bytes_globals = host_bytes;
        self.type_info_globals = host_types;
        built?;

        // The verifier catches a reference to a value left behind in the host
        // module, which is the failure this swap could produce.
        if let Err(e) = osr_module.verify() {
            return Err(anyhow!("osr module {name} failed verification: {}", e));
        }
        // Bind every symbol this module leaves undefined to the address the
        // host already has for it. MCJIT resolves across the modules it holds,
        // but only for symbols that are actually defined somewhere it can see;
        // a bytecode function that was never compiled has no definition, and
        // the call lands on a null pointer. Resolving them explicitly is what
        // rayzor does with its runtime symbols.
        let mut unresolved: Vec<String> = Vec::new();
        for f in osr_module.get_functions() {
            if f.count_basic_blocks() != 0 {
                continue; // defined here
            }
            let Ok(sym) = f.get_name().to_str() else {
                continue;
            };
            if sym.starts_with("llvm.") {
                continue; // intrinsic, lowered by the backend
            }
            match self.execution_engine.get_function_address(sym) {
                Ok(a) if a != 0 => self.execution_engine.add_global_mapping(&f, a as usize),
                _ => unresolved.push(sym.to_string()),
            }
        }
        if !unresolved.is_empty() {
            return Err(anyhow!(
                "osr module {name} has {} unresolved symbol(s): {}",
                unresolved.len(),
                unresolved.join(", ")
            ));
        }

        self.execution_engine
            .add_module(&osr_module)
            .map_err(|()| anyhow!("osr module {name} rejected by the engine"))?;
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
        crate::profile::register_jit_code(
            findex as u32,
            crate::profile::Tier::Llvm,
            addr as usize,
        );
        return Ok(addr as u64);
    }

    /// Emit the OSR entry function itself, into whatever module is current.
    fn build_osr_body(
        &mut self,
        findex: usize,
        header_pc: usize,
        body: &HLFunction,
        name: &str,
    ) -> Result<()> {

        // `(ptr) -> ret`, where ret is the function's own return type.
        let type_fun = self.bytecode.types[body.type_.0]
            .fun
            .clone()
            .ok_or_else(|| anyhow!("findex {findex} has no function type"))?;
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
        let (registers, reg_types) = self.allocate_registers(body)?;

        // Reconstruct the register file from the transferred buffer. Every
        // register is restored, not just the ones the analysis calls live: a
        // slot the loop never reads costs one load, and deciding wrongly which
        // those are costs correctness.
        let buf = function
            .get_nth_param(0)
            .ok_or_else(|| anyhow!("osr entry has no buffer parameter"))?
            .into_pointer_value();
        let i64_ty = self.context.i64_type();
        for (i, slot_ty) in reg_types.iter().enumerate() {
            let slot = unsafe {
                self.builder.build_gep(
                    i64_ty,
                    buf,
                    &[i64_ty.const_int(i as u64, false)],
                    "osr_slot",
                )?
            };
            let raw = self
                .builder
                .build_load(i64_ty, slot, "osr_raw")?
                .into_int_value();
            let v: BasicValueEnum<'ctx> = match *slot_ty {
                BasicTypeEnum::IntType(t) => {
                    if t.get_bit_width() >= 64 {
                        raw.into()
                    } else {
                        self.builder
                            .build_int_truncate(raw, t, "osr_trunc")?
                            .into()
                    }
                }
                BasicTypeEnum::FloatType(t) => {
                    if t == self.context.f64_type() {
                        self.builder.build_bit_cast(raw, t, "osr_f64")?
                    } else {
                        let n = self.builder.build_int_truncate(
                            raw,
                            self.context.i32_type(),
                            "osr_f32bits",
                        )?;
                        self.builder.build_bit_cast(n, t, "osr_f32")?
                    }
                }
                BasicTypeEnum::PointerType(t) => {
                    self.builder.build_int_to_ptr(raw, t, "osr_ptr")?.into()
                }
                _ => continue,
            };
            self.builder.build_store(registers[i], v)?;
        }

        self.translate_opcodes_from(body, &registers, &reg_types, header_pc)?;

        // The fall-through exit block is left unterminated by lowering; the
        // ordinary path closes it the same way. Without this the function
        // fails verification for a block with no terminator, which is what an
        // OSR entry hit first.
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
                "osr entry for findex {findex} pc {header_pc} failed verification"
            ));
        }

        // Bring in whatever the body calls. Lowering queues each callee it
        // could not find in the (deliberately empty) cache, and without this
        // they stay declarations that resolve to nothing -- the first version
        // left `Fun_16`, `Fun_20` and `Fun_23` undefined and jumped through a
        // null pointer. Compiling them here duplicates their code into this
        // module, which is the price of the module being self-contained.
        self.compile_pending_functions()?;

        {
            let _p = crate::profile::scope("llvm middle-end (osr)");
            super::module::run_middle_end(&self.module)?;
        }
        Ok(())
    }

    fn create_function_declaration(&mut self, f: &HLFunction) -> Result<FunctionValue<'ctx>> {
        let type_fun = self.bytecode.types[f.type_.0]
            .fun
            .clone()
            .expect("expect to get function type");
        let func_type = self.create_function_type(&type_fun)?;

        let f_v = self.module.add_function(&f.name(), func_type, None);
        self.stamp_host_cpu(f_v);
        Ok(f_v)
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

    fn translate_opcodes(
        &mut self,
        f: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
    ) -> Result<()> {
        self.translate_opcodes_from(f, registers, reg_types, 0)
    }

    /// Lower `f`, entering at opcode `entry_pc` rather than at the top.
    ///
    /// Every block is still emitted, so a jump backwards from inside the loop
    /// to code above `entry_pc` lands somewhere real; only the branch out of
    /// the entry block changes. That is what makes an OSR entry a normal
    /// compile with one edge moved, rather than a second lowering path that
    /// could disagree with the first.
    fn translate_opcodes_from(
        &mut self,
        f: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        entry_pc: usize,
    ) -> Result<()> {
        let function = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();
        let num_ops = f.ops.len();

        // Pre-create all basic blocks (two-pass approach for jump resolution)
        let mut opcode_blocks: Vec<BasicBlock<'ctx>> = Vec::with_capacity(num_ops + 1);
        for i in 0..num_ops {
            opcode_blocks.push(
                self.context
                    .append_basic_block(function, &format!("op_{}", i)),
            );
        }
        // Exit block (fallthrough after last opcode)
        opcode_blocks.push(self.context.append_basic_block(function, "exit"));

        // Branch from the entry block to wherever this compilation starts.
        self.builder
            .build_unconditional_branch(opcode_blocks[entry_pc.min(num_ops)])?;

        // Emit IR for each opcode
        for (i, op) in f.ops.iter().enumerate() {
            self.builder.position_at_end(opcode_blocks[i]);

            self.translate_opcode(f, op, registers, reg_types, i, &opcode_blocks)?;

            // If the current block has no terminator, add fallthrough to next block
            let current = self.builder.get_insert_block().unwrap();
            if current.get_terminator().is_none() {
                self.builder
                    .build_unconditional_branch(opcode_blocks[i + 1])?;
            }
        }

        // Position builder at exit block for caller to add default return if needed
        self.builder.position_at_end(opcode_blocks[num_ops]);

        Ok(())
    }

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
                    .get_int_global(ptr.0)
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
                    .get_float_global(ptr.0)
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
                    .get_string_global(ptr.0)
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
                let type_kind = self.types_.clone()[type_index].kind;

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

                // Ensure the callee is declared so we have its LLVM function type
                let (function, is_placeholder) = self.get_or_create_function_value(fun.0)?;
                let fn_type = function.get_type();

                // Load callee address from functions_ptrs[findex] at runtime
                let findex = fun.0;
                let fun_addr_slot = unsafe { self.functions_ptrs.as_ptr().add(findex) } as u64;
                let fun_addr_ptr = self
                    .context
                    .i64_type()
                    .const_int(fun_addr_slot, false)
                    .const_to_pointer(ptr_type);
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
                if is_placeholder {
                    self.add_pending_compilation(fun.0);
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
                                    &[self
                                        .context
                                        .i64_type()
                                        .const_int(24 + i as u64 * 8, false)],
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
                            let unbox = self.declare_native(
                                helper,
                                &[ptr_type.into()],
                                Some(helper_ret),
                            );
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

                    // Load vobj_proto from hl_type (offset 16)
                    let vobj_proto_gep = unsafe {
                        self.builder.build_gep(
                            self.context.i8_type(),
                            type_ptr,
                            &[self.context.i64_type().const_int(16, false)],
                            "vobj_proto_gep",
                        )?
                    };
                    let vobj_proto = self
                        .builder
                        .build_load(ptr_type, vobj_proto_gep, "vobj_proto")?
                        .into_pointer_value();

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

                    // Build arg values with type casting
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

                // Load vobj_proto from hl_type (offset 16)
                let vobj_proto_gep = unsafe {
                    self.builder.build_gep(
                        self.context.i8_type(),
                        type_ptr,
                        &[self.context.i64_type().const_int(16, false)],
                        "vobj_proto_gep",
                    )?
                };
                let vobj_proto = self
                    .builder
                    .build_load(ptr_type, vobj_proto_gep, "vobj_proto")?
                    .into_pointer_value();

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
                let (_function, is_placeholder) = self.get_or_create_function_value(fun.0)?;
                if is_placeholder {
                    self.add_pending_compilation(fun.0);
                }

                let ptr_type = self.context.ptr_type(AddressSpace::default());

                // Load function address from functions_ptrs[findex] at runtime
                let findex = fun.0 as usize;
                let fun_addr_slot = unsafe { self.functions_ptrs.as_ptr().add(findex) } as u64;
                let fun_addr_ptr = self
                    .context
                    .i64_type()
                    .const_int(fun_addr_slot, false)
                    .const_to_pointer(ptr_type);
                let fun_addr = self
                    .builder
                    .build_load(ptr_type, fun_addr_ptr, "static_closure_fun")?
                    .into_pointer_value();

                // Get function type pointer (compile-time constant from func_types)
                let type_ptr_val = self.func_types[findex] as u64;
                let type_ptr = self
                    .context
                    .i64_type()
                    .const_int(type_ptr_val, false)
                    .const_to_pointer(ptr_type);

                // Call hlp_alloc_closure_void(type, fun_addr) -> *mut vclosure
                let alloc_closure = self.declare_native(
                    "hlp_alloc_closure_void",
                    &[ptr_type.into(), ptr_type.into()],
                    Some(ptr_type.into()),
                );
                let closure = self
                    .builder
                    .build_call(
                        alloc_closure,
                        &[type_ptr.into(), fun_addr.into()],
                        "static_closure",
                    )?
                    .try_as_basic_value()
                    .basic()
                    .unwrap();
                self.builder
                    .build_store(registers[dst.0 as usize], closure)?;
            }

            // --- CallClosure ---
            Opcode::CallClosure { dst, fun, args } => {
                let closure_ptr = self
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

                // Branch based on hasValue
                let has_value_cmp = self.builder.build_int_compare(
                    IntPredicate::NE,
                    has_value,
                    i32_type.const_zero(),
                    "has_value_cmp",
                )?;

                let function = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();
                let call_with_value_bb =
                    self.context.append_basic_block(function, "call_with_value");
                let call_without_value_bb = self
                    .context
                    .append_basic_block(function, "call_without_value");
                let call_done_bb = self.context.append_basic_block(function, "call_done");

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
                        || (src_kind == hl_type_kind_HSTRUCT
                            && dst_kind == hl_type_kind_HSTRUCT))
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
                let setjmp_addr = crate::hl::_setjmp as usize as u64;
                let setjmp_ptr = self
                    .context
                    .i64_type()
                    .const_int(setjmp_addr, false)
                    .const_to_pointer(ptr_type);
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
                let (_function, is_placeholder) = self.get_or_create_function_value(fun.0)?;
                if is_placeholder {
                    self.add_pending_compilation(fun.0);
                }

                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let findex = fun.0 as usize;

                // Load function address from functions_ptrs[findex]
                let fun_addr_slot = unsafe { self.functions_ptrs.as_ptr().add(findex) } as u64;
                let fun_addr_ptr = self
                    .context
                    .i64_type()
                    .const_int(fun_addr_slot, false)
                    .const_to_pointer(ptr_type);
                let fun_addr = self
                    .builder
                    .build_load(ptr_type, fun_addr_ptr, "inst_closure_fun")?
                    .into_pointer_value();

                // Get closure type via hlp_get_closure_type(func_type)
                // This removes the first param (bound obj's type) from the fn signature
                let func_type_ptr = self.func_types[findex] as u64;
                let func_type_const = self
                    .context
                    .i64_type()
                    .const_int(func_type_ptr, false)
                    .const_to_pointer(ptr_type);
                let get_closure_type = self.declare_native(
                    "hlp_get_closure_type",
                    &[ptr_type.into()],
                    Some(ptr_type.into()),
                );
                let closure_type = self
                    .builder
                    .build_call(get_closure_type, &[func_type_const.into()], "closure_type")?
                    .try_as_basic_value()
                    .basic()
                    .unwrap();

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

                let (_function, is_placeholder) = self.get_or_create_function_value(findex)?;
                if is_placeholder {
                    self.add_pending_compilation(findex);
                }

                // Load obj pointer
                let obj_val =
                    self.builder
                        .build_load(ptr_type, registers[obj.0 as usize], "vclos_obj")?;

                // Load function address from functions_ptrs[findex]
                let fun_addr_slot = unsafe { self.functions_ptrs.as_ptr().add(findex) } as u64;
                let fun_addr_ptr = self
                    .context
                    .i64_type()
                    .const_int(fun_addr_slot, false)
                    .const_to_pointer(ptr_type);
                let fun_addr = self
                    .builder
                    .build_load(ptr_type, fun_addr_ptr, "vclos_fun")?
                    .into_pointer_value();

                // Get closure type via hlp_get_closure_type(func_type)
                let func_type_ptr = self.func_types[findex] as u64;
                let func_type_const = self
                    .context
                    .i64_type()
                    .const_int(func_type_ptr, false)
                    .const_to_pointer(ptr_type);
                let get_closure_type = self.declare_native(
                    "hlp_get_closure_type",
                    &[ptr_type.into()],
                    Some(ptr_type.into()),
                );
                let closure_type = self
                    .builder
                    .build_call(get_closure_type, &[func_type_const.into()], "vclos_type")?
                    .try_as_basic_value()
                    .basic()
                    .unwrap();

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
                if let Some(bytes_global) = self.get_bytes_global(ptr.0) {
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

            // --- Assert: unreachable ---
            Opcode::Assert => {
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
            i64_type.const_int(crate::jit::stub_bridge::STUB_SENTINEL_LIMIT, false),
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
                    i64_type.const_int(crate::jit::stub_bridge::STUB_SENTINEL_LIMIT, false),
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

        // --- Bridge path: spill args as raw i64 words, re-enter interpreter ---
        self.builder.position_at_end(bridge_bb);
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

        let stub_fn_type =
            i64_type.fn_type(&[i64_type.into(), ptr_type.into(), i32_type.into()], false);
        let stub_fn_ptr = i64_type
            .const_int(
                crate::jit::stub_bridge::ash_jit_call_stub as usize as u64,
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
        match (direct_val, stub_val) {
            (Some(d), Some(s)) => {
                let phi = self
                    .builder
                    .build_phi(d.get_type(), &format!("{}_result", name))?;
                phi.add_incoming(&[(&d, direct_bb), (&s, bridge_bb)]);
                if let Some(h) = healed {
                    phi.add_incoming(&[(&h, heal_bb)]);
                }
                Ok(Some(phi.as_basic_value()))
            }
            _ => Ok(None),
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
    fn shield_trap_functions_from_optimization(&self) -> usize {
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

        // Optimize before anything asks for an address: requesting one forces
        // codegen, and a pass run afterwards would be too late.
        {
            let _phase = crate::profile::scope("llvm middle-end");
            let excluded = self.shield_trap_functions_from_optimization();
            crate::profile::count("middle-end functions excluded (trap)", excluded as u64);
            super::module::run_middle_end(&self.module)?;
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
                let path = if spec == "1" { "/tmp/ash_jit.ll" } else { &spec };
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
            unsafe {
                self.execution_engine.run_function(function, &[]);
            };
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
        let types = module.types_.clone();
        let regs = &self.fun.regs;
        let type_ = types.get(self.fun.type_.0).expect("Unknown type");
        let fun = type_.fun.as_ref().expect("Expected to get function type");
        self.type_ = module.create_function_type(fun).ok();
        self.value = module.create_function_value(self.fun.findex as usize).ok();
        Ok(())
    }
}
