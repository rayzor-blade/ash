//! Reaching ash_std from generated code: intrinsics that become instructions,
//! and the callers for the natives that stay calls.
//!
//! Split out of `function/mod.rs`. Same `impl` block on `JITModule`, moved
//! verbatim.

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
    AddressSpace, AtomicOrdering, FloatPredicate, IntPredicate, basic_block::BasicBlock,
    builder::Builder,
};

use crate::hl::{
    hl_obj_field, hl_runtime_obj, hl_type, hl_type_kind_HABSTRACT, hl_type_kind_HBOOL,
    hl_type_kind_HBYTES, hl_type_kind_HDYN, hl_type_kind_HDYNOBJ, hl_type_kind_HF32,
    hl_type_kind_HF64, hl_type_kind_HI32, hl_type_kind_HI64, hl_type_kind_HNULL, hl_type_kind_HOBJ,
    hl_type_kind_HSTRUCT, hl_type_kind_HTYPE, hl_type_kind_HUI8, hl_type_kind_HUI16,
    hl_type_kind_HVIRTUAL, hl_type_kind_HVOID, vdynamic, vdynobj, vvirtual,
};
use crate::llvm::module::{CompiledFunctionMeta, JITModule};
use crate::opcodes::{
    Opcode, RefBytes, RefEnumConstruct, RefField, RefFloat, RefFun, RefGlobal, RefInt, RefString,
    RefType, Reg,
};
use crate::types::{HLNative, HLTypeFun, Str, TypeRef};
use crate::{
    hl::{hl_type_kind_HFUN, hl_type_kind_HMETHOD},
    types::HLFunction,
};
use anyhow::{Result, anyhow};

use super::{FuncPtr, GC_REGISTER_PIN, native_traps_enabled};

impl<'ctx> JITModule<'ctx> {
    /// Emit `intr` inline over `arg`, or `Ok(None)` if the intrinsic
    /// declaration could not be obtained.
    ///
    /// The saturating float→int conversions are not an optimization but a
    /// correctness requirement: `ash_std` casts with Rust's `as`, which clamps
    /// out-of-range values and maps NaN to zero, whereas `fptosi` is poison on
    /// exactly those inputs. See [`crate::intrinsics`].
    pub(super) fn emit_native_intrinsic(
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
    pub(super) fn native_intrinsic_for(
        &self,
        findex: usize,
    ) -> Option<crate::intrinsics::NativeIntrinsic> {
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
        let context = crate::native_lib::host_native_context(lib, &native_func.name);
        let record = crate::native_lib::host_native_record(lib, &native_func.name);

        if self.aot {
            // A host's native it links ahead of time: its callee by name,
            // with the casts the host chose between the two sides' types.
            if let Some(link) = crate::native_lib::host_link(lib, &native_func.name) {
                let caller_name = format!("{}_{}_linked", lib, name);
                let arg_types: Vec<usize> = type_fun.args.iter().map(|t| t.0).collect();
                return self.generate_native_caller_linked(
                    &caller_name,
                    func_type,
                    &link,
                    &arg_types,
                    type_fun.ret.0,
                );
            }
            if context != 0 || record {
                return Err(anyhow!(
                    "host native {lib}@{name} carries a context word, which cannot be linked ahead of time"
                ));
            }
            // The runtime's own primitives are plain `#[no_mangle]` exports
            // of the library this object links against, so the symbol IS the
            // name. An HDLL primitive is not: it is reached through a
            // DEFINE_PRIM resolver in a shared library, and there is no
            // shared library to load.
            let clean = lib.strip_prefix('?').unwrap_or(lib);
            if !crate::native_lib::is_runtime_lib(clean) {
                // No symbol to bind: an HDLL primitive lives behind a
                // DEFINE_PRIM table in a shared library that does not exist
                // yet. So bind a SLOT instead and let the startup routine fill
                // it with the same dlopen/dlsym the interpreter and the JIT
                // do -- the indirection an HDLL call has anyway, moved from
                // link time to load time.
                let slot_name = format!("ash_native_{clean}_{}", native_func.name);
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let slot = match self.module.get_global(&slot_name) {
                    Some(existing) => existing,
                    None => {
                        let g = self.module.add_global(ptr_type, None, &slot_name);
                        g.set_initializer(&ptr_type.const_null());
                        g.set_linkage(inkwell::module::Linkage::Internal);
                        self.aot_hdll_natives
                            .push((clean.to_string(), native_func.name.clone()));
                        g
                    }
                };
                let caller_name = format!("{}_{}_caller", lib, name);
                return self.generate_native_caller_through_slot(
                    &caller_name,
                    func_type,
                    slot,
                    clean,
                    &native_func.name,
                );
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
        let native_caller = if record {
            self.generate_native_caller_by_record(&caller_name, func_type, func_addr, context)?
        } else {
            self.generate_native_caller_with_context(&caller_name, func_type, func_addr, context)?
        };

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
    /// A caller that reaches its primitive through a slot the startup routine
    /// filled, raising HashLink's own error if it never resolved.
    ///
    /// Referencing an unavailable primitive is not itself an error -- only
    /// calling one is. Failing at emit time instead would refuse whole
    /// programs over a primitive they never reach, which is neither what the
    /// interpreter does nor what upstream does.
    fn generate_native_caller_through_slot(
        &self,
        caller_name: &str,
        fn_type: FunctionType<'ctx>,
        slot: inkwell::values::GlobalValue<'ctx>,
        lib: &str,
        prim: &str,
    ) -> Result<FunctionValue<'ctx>> {
        if let Some(existing) = self.module.get_function(caller_name) {
            return Ok(existing);
        }
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let saved_block = self.builder.get_insert_block();
        let function = self.module.add_function(caller_name, fn_type, None);
        let entry = self.context.append_basic_block(function, "entry");
        let resolved = self.context.append_basic_block(function, "native_resolved");
        let missing = self.context.append_basic_block(function, "native_missing");

        self.builder.position_at_end(entry);
        let target = self
            .builder
            .build_load(ptr_type, slot.as_pointer_value(), "native")?
            .into_pointer_value();
        let is_null = self.builder.build_is_null(target, "native_missing_p")?;
        self.builder
            .build_conditional_branch(is_null, missing, resolved)?;

        self.builder.position_at_end(missing);
        let void_type = self.context.void_type();
        let reporter = self.aot_runtime_fn(
            "hlp_aot_native_missing",
            void_type.fn_type(&[ptr_type.into(), ptr_type.into()], false),
        );
        let lib_s = self.builder.build_global_string_ptr(lib, "aot_lib")?;
        let prim_s = self.builder.build_global_string_ptr(prim, "aot_prim")?;
        self.builder.build_call(
            reporter,
            &[
                lib_s.as_pointer_value().into(),
                prim_s.as_pointer_value().into(),
            ],
            "",
        )?;
        self.builder.build_unreachable()?;

        self.builder.position_at_end(resolved);
        let args: Vec<BasicMetadataValueEnum> =
            function.get_param_iter().map(|arg| arg.into()).collect();
        let call = self
            .builder
            .build_indirect_call(fn_type, target, &args, "call")?;
        match call.try_as_basic_value().basic() {
            Some(value) => self.builder.build_return(Some(&value))?,
            None => self.builder.build_return(None)?,
        };
        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }
        Ok(function)
    }

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

    /// A caller for a native the host links: each argument cast to the
    /// callee's type where the host named a cast, the callee called by its
    /// symbol with its own signature, the result cast back.
    fn generate_native_caller_linked(
        &mut self,
        caller_name: &str,
        fn_type: FunctionType<'ctx>,
        link: &crate::native_lib::HostLink,
        arg_types: &[usize],
        ret_type: usize,
    ) -> Result<FunctionValue<'ctx>> {
        if let Some(existing) = self.module.get_function(caller_name) {
            return Ok(existing);
        }
        // An initialiser's first argument is the program's object, which
        // the callee never sees.
        let skip = usize::from(link.init.is_some());
        let caller_params = fn_type.get_param_types();
        if caller_params.len() != link.params.len() + skip
            || link.arg_casts.len() != link.params.len()
        {
            return Err(anyhow!(
                "{}: the program declares {} arguments, the link {}",
                link.symbol,
                caller_params.len(),
                link.params.len()
            ));
        }
        let word = |w: crate::native_lib::Word| -> BasicTypeEnum<'ctx> {
            use crate::native_lib::Word;
            match w {
                Word::Bool => self.context.bool_type().into(),
                Word::I32 => self.context.i32_type().into(),
                Word::I64 => self.context.i64_type().into(),
                Word::F32 => self.context.f32_type().into(),
                Word::F64 => self.context.f64_type().into(),
                Word::Ptr => self.context.ptr_type(AddressSpace::default()).into(),
            }
        };
        let callee_params: Vec<BasicMetadataTypeEnum> =
            link.params.iter().map(|&w| word(w).into()).collect();
        let callee_type = match link.ret {
            Some(w) => word(w).fn_type(&callee_params, false),
            None => self.context.void_type().fn_type(&callee_params, false),
        };
        // A callee in a side module is found by its symbol at startup, into a
        // slot; any other is linked by it.
        let slot = link.library.as_deref().map(|lib| {
            let slot_name = format!("ash_link_{lib}_{}", link.symbol);
            match self.module.get_global(&slot_name) {
                Some(existing) => existing,
                None => {
                    let ptr_type = self.context.ptr_type(AddressSpace::default());
                    let g = self.module.add_global(ptr_type, None, &slot_name);
                    g.set_initializer(&ptr_type.const_null());
                    g.set_linkage(inkwell::module::Linkage::Internal);
                    self.aot_link_slots
                        .push((lib.to_owned(), link.symbol.clone()));
                    g
                }
            }
        });
        let callee = slot
            .is_none()
            .then(|| self.aot_runtime_fn(&link.symbol, callee_type));
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        // The program's type of each value a cast sees, as the constant the
        // object holds for it; resolved before the body is positioned.
        let mut arg_descs = Vec::with_capacity(arg_types.len());
        for &t in arg_types {
            arg_descs.push(self.get_initialized_type(t)?);
        }
        let ret_desc = self.get_initialized_type(ret_type)?;

        let saved_block = self.builder.get_insert_block();
        let function = self.module.add_function(caller_name, fn_type, None);
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        let mut args: Vec<BasicMetadataValueEnum> = Vec::with_capacity(link.params.len());
        for (i, param) in function.get_param_iter().enumerate().skip(skip) {
            let j = i - skip;
            let target = word(link.params[j]);
            let value = match &link.arg_casts[j] {
                Some(cast) => {
                    self.emit_host_cast(cast, param, arg_descs[i], target, arg_types[i])?
                }
                None if param.get_type() == target => param,
                None => self.widen_or_narrow(param, target).ok_or_else(|| {
                    anyhow!(
                        "{}: argument {i} is {:?} to the program and {:?} to the callee, with no cast",
                        link.symbol,
                        param.get_type(),
                        link.params[j]
                    )
                })??,
            };
            args.push(value.into());
        }
        let call = match (callee, slot) {
            (Some(callee), _) => self.builder.build_call(callee, &args, "call")?,
            (None, Some(slot)) => {
                // Unresolved, it raises as an HDLL primitive does: only a
                // call is an error, not a reference.
                let lib = link.library.as_deref().unwrap_or_default();
                let target = self
                    .builder
                    .build_load(ptr_type, slot.as_pointer_value(), "callee")?
                    .into_pointer_value();
                let resolved = self.context.append_basic_block(function, "callee_resolved");
                let missing = self.context.append_basic_block(function, "callee_missing");
                let is_null = self.builder.build_is_null(target, "callee_missing_p")?;
                self.builder
                    .build_conditional_branch(is_null, missing, resolved)?;
                self.builder.position_at_end(missing);
                let reporter = self.aot_runtime_fn(
                    "hlp_aot_native_missing",
                    self.context
                        .void_type()
                        .fn_type(&[ptr_type.into(), ptr_type.into()], false),
                );
                let lib_s = self.builder.build_global_string_ptr(lib, "link_lib")?;
                let sym_s = self
                    .builder
                    .build_global_string_ptr(&link.symbol, "link_sym")?;
                self.builder.build_call(
                    reporter,
                    &[
                        lib_s.as_pointer_value().into(),
                        sym_s.as_pointer_value().into(),
                    ],
                    "",
                )?;
                self.builder.build_unreachable()?;
                self.builder.position_at_end(resolved);
                self.builder
                    .build_indirect_call(callee_type, target, &args, "call")?
            }
            (None, None) => unreachable!("a link is either linked or found in a library"),
        };
        if let Some(after) = &link.after {
            let after_fn = self.aot_runtime_fn(after, self.context.void_type().fn_type(&[], false));
            self.builder.build_call(after_fn, &[], "")?;
        }
        // An initialiser binds the program's object to what the callee made.
        // The object stays live across the call: it is init's first argument,
        // and the program's frame that allocated it holds it too. A callee
        // that throws skips init, leaving the object unbound, as a throwing
        // constructor leaves it.
        if let Some(init) = &link.init {
            let object = function
                .get_nth_param(0)
                .ok_or_else(|| anyhow!("{init}: no object"))?;
            let made = call.try_as_basic_value().basic().ok_or_else(|| {
                anyhow!("{}: an initialiser's callee returns nothing", link.symbol)
            })?;
            let init_fn = self.aot_runtime_fn(
                init,
                self.context.void_type().fn_type(
                    &[
                        object.get_type().into(),
                        made.get_type().into(),
                        ptr_type.into(),
                    ],
                    false,
                ),
            );
            self.builder.build_call(
                init_fn,
                &[object.into(), made.into(), arg_descs[0].into()],
                "",
            )?;
            self.builder.build_return(None)?;
            if let Some(block) = saved_block {
                self.builder.position_at_end(block);
            }
            return Ok(function);
        }
        match (call.try_as_basic_value().basic(), fn_type.get_return_type()) {
            (Some(value), Some(want)) => {
                let value = match &link.ret_cast {
                    Some(cast) => self.emit_host_cast(cast, value, ret_desc, want, ret_type)?,
                    None if value.get_type() == want => value,
                    None => self.widen_or_narrow(value, want).ok_or_else(|| {
                        anyhow!(
                            "{}: the result is {:?} to the callee and {:?} to the program, with no cast",
                            link.symbol,
                            value.get_type(),
                            want
                        )
                    })??,
                };
                self.builder.build_return(Some(&value))?
            }
            // A result the program declares none for is dropped.
            (_, None) => self.builder.build_return(None)?,
            (None, want) => {
                return Err(anyhow!(
                    "{}: the callee returns {:?}, the program declares {:?}",
                    link.symbol,
                    link.ret,
                    want
                ));
            }
        };

        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }
        Ok(function)
    }

    /// `value` as the integer type `target` when both are integers (a bool
    /// is one bit): sign-extended, zero-extended from a bool, or truncated.
    /// `None` when either is not an integer.
    fn widen_or_narrow(
        &self,
        value: BasicValueEnum<'ctx>,
        target: BasicTypeEnum<'ctx>,
    ) -> Option<Result<BasicValueEnum<'ctx>>> {
        let (BasicValueEnum::IntValue(v), BasicTypeEnum::IntType(t)) = (value, target) else {
            return None;
        };
        let from = v.get_type().get_bit_width();
        let to = t.get_bit_width();
        let cast = if to > from && from == 1 {
            self.builder.build_int_z_extend(v, t, "widen")
        } else if to > from {
            self.builder.build_int_s_extend(v, t, "widen")
        } else if to == 1 {
            self.builder.build_int_compare(
                inkwell::IntPredicate::NE,
                v,
                v.get_type().const_zero(),
                "truth",
            )
        } else {
            self.builder.build_int_truncate(v, t, "narrow")
        };
        Some(cast.map(Into::into).map_err(Into::into))
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
}
