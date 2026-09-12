//! Emitting the AIR v2 call and closure instructions: direct calls by
//! findex, method calls through a vtable slot, closure calls, and the three
//! closure constructors.
//!
//! Each emitter takes the AIR instruction's own fields. A value's slot is
//! `registers[v.idx()]` (typed `reg_types[v.idx()]`) and its HL type index
//! is `lowering.regs[v.idx()].0`; `lowering.findex` is the caller.

use air::v2::ir::{IntrinsicKind, ValueId};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, PointerValue};
use inkwell::{AddressSpace, IntPredicate};

use crate::hl::{
    hl_type_kind_HABSTRACT, hl_type_kind_HDYN, hl_type_kind_HDYNOBJ, hl_type_kind_HF32,
    hl_type_kind_HF64, hl_type_kind_HFUN, hl_type_kind_HI64, hl_type_kind_HNULL,
    hl_type_kind_HOBJ, hl_type_kind_HVIRTUAL, hl_type_kind_HVOID,
};
use crate::llvm::module::JITModule;
use crate::types::HLFunction;
use anyhow::{anyhow, Result};

use super::{hl_hash_utf8, FuncPtr};

impl<'ctx> JITModule<'ctx> {
    /// Direct call by findex.
    ///
    /// A bytecode callee under lazy compilation is reached through its
    /// functions_ptrs slot instead, so a recompiled body is picked up
    /// without recompiling the caller. Natives are always called directly.
    pub(super) fn emit_air_call(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        dst: ValueId,
        fun: usize,
        args: &[ValueId],
    ) -> Result<()> {
        if self.lazy_compilation && matches!(self.findexes.get(&fun), Some(FuncPtr::Fun(_))) {
            return self.emit_air_indirect_call(
                lowering, registers, reg_types, cell_base, dst, fun, args,
            );
        }
        self.emit_air_direct_call(lowering, registers, reg_types, cell_base, dst, fun, args)
    }

    /// A recognised stdlib native. `kind` is not consulted: the native is
    /// called like any other direct call of its arity, and the unary
    /// machine-instruction primitives are picked up by the arity-1 path.
    pub(super) fn emit_air_intrinsic(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        kind: IntrinsicKind,
        fun: usize,
        dst: ValueId,
        args: &[ValueId],
    ) -> Result<()> {
        self.emit_air_direct_call(lowering, registers, reg_types, cell_base, dst, fun, args)
    }

    /// Direct call through the callee's declaration. Each arity keeps its
    /// own order of callee resolution and argument loads, and its own value
    /// names.
    fn emit_air_direct_call(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        dst: ValueId,
        fun: usize,
        args: &[ValueId],
    ) -> Result<()> {
        match args {
            [] => {
                let (function, is_placeholder) = self.get_or_create_function_value(fun)?;
                let result = self.builder.build_call(function, &[], "call")?;

                if result.try_as_basic_value().basic().is_some() {
                    self.builder.build_store(
                        registers[dst.idx()],
                        result.try_as_basic_value().basic().unwrap(),
                    );
                }

                if is_placeholder {
                    self.add_pending_compilation(fun);
                }
            }
            [arg0] => {
                let arg0_val = self.builder.build_load(
                    reg_types[arg0.idx()],
                    registers[arg0.idx()],
                    "arg0_val",
                )?;

                // Machine-instruction primitives (Math.sqrt and friends) are
                // emitted here rather than called. Every entry in the table is
                // unary, which is why this is the only call arity that has to
                // check. See crate::intrinsics.
                let inlined = match self.native_intrinsic_for(fun) {
                    Some(intr) => self.emit_native_intrinsic(intr, arg0_val)?,
                    None => None,
                };

                if let Some(v) = inlined {
                    self.builder.build_store(registers[dst.idx()], v)?;
                } else {
                    let (function, is_placeholder) = self.get_or_create_function_value(fun)?;
                    let result = self
                        .builder
                        .build_call(function, &[arg0_val.into()], "call")?;

                    if result.try_as_basic_value().basic().is_some() {
                        self.builder.build_store(
                            registers[dst.idx()],
                            result.try_as_basic_value().basic().unwrap(),
                        );
                    }

                    if is_placeholder {
                        self.add_pending_compilation(fun);
                    }
                }
            }
            [arg0, arg1] => {
                let (function, is_placeholder) = self.get_or_create_function_value(fun)?;
                let arg0_val = self.builder.build_load(
                    reg_types[arg0.idx()],
                    registers[arg0.idx()],
                    "arg0_val",
                )?;
                let arg1_val = self.builder.build_load(
                    reg_types[arg1.idx()],
                    registers[arg1.idx()],
                    "arg1_val",
                )?;

                let result = self.builder.build_call(
                    function,
                    &[arg0_val.into(), arg1_val.into()],
                    "call",
                )?;

                if result.try_as_basic_value().basic().is_some() {
                    self.builder.build_store(
                        registers[dst.idx()],
                        result.try_as_basic_value().basic().unwrap(),
                    );
                }

                if is_placeholder {
                    self.add_pending_compilation(fun);
                }
            }
            [arg0, arg1, arg2] => {
                let (function, is_placeholder) = self.get_or_create_function_value(fun)?;
                let arg0_val = self.builder.build_load(
                    reg_types[arg0.idx()],
                    registers[arg0.idx()],
                    "arg0_val",
                )?;
                let arg1_val = self.builder.build_load(
                    reg_types[arg1.idx()],
                    registers[arg1.idx()],
                    "arg1_val",
                )?;
                let arg2_val = self.builder.build_load(
                    reg_types[arg2.idx()],
                    registers[arg2.idx()],
                    "arg2_val",
                )?;
                let result = self.builder.build_call(
                    function,
                    &[arg0_val.into(), arg1_val.into(), arg2_val.into()],
                    "call",
                )?;

                if result.try_as_basic_value().basic().is_some() {
                    self.builder.build_store(
                        registers[dst.idx()],
                        result.try_as_basic_value().basic().unwrap(),
                    );
                }

                if is_placeholder {
                    self.add_pending_compilation(fun);
                }
            }
            [arg0, arg1, arg2, arg3] => {
                let (function, is_placeholder) = self.get_or_create_function_value(fun)?;
                let args: Vec<BasicMetadataValueEnum> = [arg0, arg1, arg2, arg3]
                    .iter()
                    .map(|arg| {
                        self.builder
                            .build_load(reg_types[arg.idx()], registers[arg.idx()], "arg_val")
                            .unwrap()
                            .into()
                    })
                    .collect();
                let result = self.builder.build_call(function, &args, "call")?;
                if let Some(ret_val) = result.try_as_basic_value().basic() {
                    self.builder.build_store(registers[dst.idx()], ret_val)?;
                }
                if is_placeholder {
                    self.add_pending_compilation(fun);
                }
            }
            _ => {
                let (function, is_placeholder) = self.get_or_create_function_value(fun)?;
                let arg_vals: Vec<BasicMetadataValueEnum> = args
                    .iter()
                    .map(|arg| {
                        self.builder
                            .build_load(reg_types[arg.idx()], registers[arg.idx()], "arg_val")
                            .unwrap()
                            .into()
                    })
                    .collect();
                let result = self.builder.build_call(function, &arg_vals, "call")?;
                if let Some(ret_val) = result.try_as_basic_value().basic() {
                    self.builder.build_store(registers[dst.idx()], ret_val)?;
                }
                if is_placeholder {
                    self.add_pending_compilation(fun);
                }
            }
        }
        Ok(())
    }

    /// Call a bytecode function through functions_ptrs[findex], loaded at
    /// run time, so the caller keeps working after the callee is recompiled.
    fn emit_air_indirect_call(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        dst: ValueId,
        fun: usize,
        args: &[ValueId],
    ) -> Result<()> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());

        // Build the ABI type directly from bytecode. Creating an LLVM
        // declaration here makes an isolated lazy module report an
        // unresolved Fun_* symbol even though the call itself goes
        // exclusively through functions_ptrs.
        let callee = self
            .bytecode
            .functions
            .iter()
            .find(|f| f.findex as usize == fun)
            .ok_or_else(|| anyhow!("IndirectCall target {} is not bytecode", fun))?;
        let type_fun = self.bytecode.types[callee.type_.0]
            .fun
            .clone()
            .ok_or_else(|| anyhow!("IndirectCall target {} has no function type", fun))?;
        let fn_type = self.create_function_type(&type_fun)?;

        // Load callee address from functions_ptrs[findex] at runtime
        let findex = fun;
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
                    .build_load(reg_types[arg.idx()], registers[arg.idx()], "arg_val")
                    .unwrap()
                    .into()
            })
            .collect();

        // Indirect call through the loaded pointer (stub-guarded:
        // functions_ptrs may hold interpreter sentinels in hybrid mode)
        if let Some(ret_val) =
            self.build_stub_guarded_indirect_call(fn_type, fun_addr, &arg_vals, "icall")?
        {
            self.builder.build_store(registers[dst.idx()], ret_val)?;
        }
        Ok(())
    }

    /// Method call through vtable slot `field`; `args[0]` is the receiver.
    /// Compile-time proto resolution for HOBJ/HSTRUCT, runtime vfields for
    /// HVIRTUAL, the runtime methods table otherwise.
    pub(super) fn emit_air_call_method(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        dst: ValueId,
        field: usize,
        args: &[ValueId],
    ) -> Result<()> {
        let obj_type_idx = lowering.regs[args[0].idx()].0;
        let obj_type = &self.types_[obj_type_idx];
        let ptr_type = self.context.ptr_type(AddressSpace::default());

        if obj_type.kind == hl_type_kind_HVIRTUAL {
            // Compile-time hash of the virtual field name, for the
            // dynamic fallback (which resolves by hash, not slot).
            let field_hash = obj_type
                .virt
                .as_ref()
                .and_then(|v| v.fields.get(field))
                .map(|fld| hl_hash_utf8(&fld.name))
                .unwrap_or(0);

            // HVIRTUAL dispatch: load function pointer from vfields[field]
            let vvirt = self
                .builder
                .build_load(ptr_type, registers[args[0].idx()], "vvirt")?
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
            let fallback_block = self.context.append_basic_block(function, "vcall_fallback");
            let merge_block = self.context.append_basic_block(function, "vcall_merge");

            // Runtime guard: at the hybrid interpreter/JIT boundary an
            // HVIRTUAL-typed register can hold a plain HOBJ/HDYNOBJ
            // pointer (the interpreter is dynamically typed). Only
            // trust the vvirtual layout after checking the header's
            // type kind; null or non-virtual goes to the hash-based
            // fallback helper.
            let vvirt_null = self.builder.build_is_null(vvirt, "vvirt_null")?;
            self.builder
                .build_conditional_branch(vvirt_null, fallback_block, nonnull_block)?;

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
            self.builder
                .build_conditional_branch(is_virt, vfields_block, fallback_block)?;

            // --- vfields path: a real vvirtual; try the resolved slot ---
            self.builder.position_at_end(vfields_block);
            // Load value (underlying object) from the vvirtual
            let value_gep = unsafe {
                self.builder.build_gep(
                    self.context.i8_type(),
                    vvirt,
                    &[self
                        .context
                        .i64_type()
                        .const_int(self.target_abi.vvirtual_value_offset(), false)],
                    "vvirt_value_gep",
                )?
            };
            let value = self
                .builder
                .build_load(ptr_type, value_gep, "vvirt_value")?
                .into_pointer_value();

            // Load vfields[field]
            let vfield_offset = self.target_abi.vvirtual_fields_offset()
                + field as u64 * self.target_abi.pointer_bytes() as u64;
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
                .and_then(|v| v.fields.get(field))
                .and_then(|fld| {
                    let ft = &self.types_[fld.type_.0];
                    if ft.kind == hl_type_kind_HFUN {
                        ft.fun.as_ref().map(|fun| {
                            let arg_indices: Vec<usize> = fun.args.iter().map(|a| a.0).collect();
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
            let virt_ret_type: Option<BasicTypeEnum> = if let Some((_, ret_idx)) = virt_fn_info {
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
                    arg_types.push(reg_types[arg.idx()].into());
                }
            }
            let dst_kind = self.types_[lowering.regs[dst.idx()].0].kind;
            let ret_type = virt_ret_type;
            let fn_type = if dst_kind == hl_type_kind_HVOID {
                self.context.void_type().fn_type(&arg_types, false)
            } else if let Some(rt) = ret_type {
                rt.fn_type(&arg_types, false)
            } else {
                reg_types[dst.idx()].fn_type(&arg_types, false)
            };

            // Emit the tail (non-this) argument loads with casts to the
            // declared param types, in the current insert block.
            let build_tail_args =
                |this: &JITModule<'ctx>| -> Result<Vec<BasicMetadataValueEnum<'ctx>>> {
                    let mut vals: Vec<BasicMetadataValueEnum> =
                        Vec::with_capacity(args.len().saturating_sub(1));
                    for (idx, arg) in args[1..].iter().enumerate() {
                        let loaded = this.builder.build_load(
                            reg_types[arg.idx()],
                            registers[arg.idx()],
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
            if let Some(ret_val) =
                self.build_stub_guarded_indirect_call(fn_type, fn_ptr, &arg_vals, "vcall_virt")?
            {
                let store_val = if ret_val.get_type() != reg_types[dst.idx()] {
                    self.cast_for_call(ret_val, reg_types[dst.idx()])?
                } else {
                    ret_val
                };
                self.builder.build_store(registers[dst.idx()], store_val)?;
            }
            self.builder.build_unconditional_branch(merge_block)?;

            // --- Fallback path: not a vvirtual, or vfield is null ---
            // The vfield being null means the DECLARED signature is
            // not the implementation's: the interface says
            // Iterator<Int>.next is () -> i32 while the generic
            // implementation behind it was compiled () -> Dynamic.
            // Calling the resolved pointer through the declared ABI
            // would read the low 32 bits of a returned box pointer as
            // the value. So this path never guesses an ABI: box every
            // argument by its static register type, let hlp_vcall_dyn
            // call the method through its OWN runtime type, and
            // dyn-cast the boxed result to the declared kind.
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
                    let src_type_idx = lowering.regs[arg.idx()].0;
                    let loaded = self.builder.build_load(
                        reg_types[arg.idx()],
                        registers[arg.idx()],
                        "vcall_arg",
                    )?;
                    // Same boxing rule as ToDyn: pointers are already
                    // dyn-compatible (except HABSTRACT), primitives go
                    // through hlp_make_dyn with their static type.
                    let src_is_abstract =
                        self.types_[src_type_idx].kind == hl_type_kind_HABSTRACT;
                    let boxed: BasicValueEnum = if loaded.is_pointer_value() && !src_is_abstract
                    {
                        loaded
                    } else {
                        let temp = self.entry_alloca(loaded.get_type(), "vcall_box_slot")?;
                        self.builder.build_store(temp, loaded)?;
                        let type_ptr = self
                            .get_initialized_type(src_type_idx)?
                            .into_pointer_value();
                        self.builder
                            .build_call(make_dyn, &[temp.into(), type_ptr.into()], "vcall_box")?
                            .try_as_basic_value()
                            .basic()
                            .unwrap()
                    };
                    // varray data follows the header.
                    let slot_gep = unsafe {
                        self.builder.build_gep(
                            self.context.i8_type(),
                            arr,
                            &[self.context.i64_type().const_int(
                                self.target_abi.varray_data_offset()
                                    + i as u64 * self.target_abi.pointer_bytes() as u64,
                                false,
                            )],
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
                let dst_ty = reg_types[dst.idx()];
                let store_val: BasicValueEnum = if dst_kind == hl_type_kind_HVIRTUAL {
                    // The callee returns ITS declared type. When that is
                    // a view of another virtual type (IntMap.keys hands
                    // back an Iterator<Int> where the caller's erased
                    // field is Iterator<Dynamic>), storing it as-is
                    // leaves a view whose method slots were resolved
                    // for the other type. HashLink's dynamic call casts
                    // the result to the caller's type; do the same.
                    // hl_to_virtual returns a same-typed view unchanged
                    // and null for null.
                    let dst_vt = self
                        .get_initialized_type(lowering.regs[dst.idx()].0)?
                        .into_pointer_value();
                    let to_virtual = self.declare_native(
                        "hl_to_virtual",
                        &[ptr_type.into(), ptr_type.into()],
                        Some(ptr_type.into()),
                    );
                    self.builder
                        .build_call(
                            to_virtual,
                            &[dst_vt.into(), ret_dyn.into()],
                            "vcall_ret_view",
                        )?
                        .try_as_basic_value()
                        .basic()
                        .unwrap()
                } else if dst_kind == hl_type_kind_HDYN {
                    // A Dynamic destination: the box is the value.
                    ret_dyn.into()
                } else if dst_ty.is_pointer_type() {
                    // The callee returned ITS declared type, which is
                    // not necessarily the caller's (`ArrayBytes_Int.map`
                    // hands back an ArrayDyn where the structural type
                    // says Array<Int>). Run the runtime's dynamic cast:
                    // it walks the super chain and otherwise asks the
                    // object's own __cast, which for ArrayDyn
                    // reinterprets into the typed array. Same as the
                    // CallClosure dynamic path. Null passes through as
                    // null.
                    let dyn_type_index = self
                        .types_
                        .iter()
                        .position(|ty| ty.kind == hl_type_kind_HDYN)
                        .ok_or_else(|| anyhow!("module has no HDYN runtime type"))?;
                    let dyn_type = self
                        .get_initialized_type(dyn_type_index)?
                        .into_pointer_value();
                    let dst_runtime_type = self
                        .get_initialized_type(lowering.regs[dst.idx()].0)?
                        .into_pointer_value();
                    let result_slot = self.entry_alloca(ptr_type, "vcall_dyn_result_slot")?;
                    self.builder.build_store(result_slot, ret_dyn)?;
                    let castp = self.declare_native(
                        "hlp_dyn_castp",
                        &[ptr_type.into(), ptr_type.into(), ptr_type.into()],
                        Some(ptr_type.into()),
                    );
                    self.builder
                        .build_call(
                            castp,
                            &[result_slot.into(), dyn_type.into(), dst_runtime_type.into()],
                            "vcall_dyn_result_cast",
                        )?
                        .try_as_basic_value()
                        .basic()
                        .ok_or_else(|| anyhow!("hlp_dyn_castp returned void"))?
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
                    let unbox = self.declare_native(helper, &[ptr_type.into()], Some(helper_ret));
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
                self.builder.build_store(registers[dst.idx()], store_val)?;
            }
            self.builder.build_unconditional_branch(merge_block)?;

            // Continue at merge
            self.builder.position_at_end(merge_block);
        } else if let Some(findex) = {
            // `field` is the vtable slot index (vobj_proto index). Find
            // the proto entry whose pindex matches it to get the findex
            // for the function signature -- walking the SUPER chain: a
            // subclass's own proto list holds only the methods it
            // declares, so an inherited method called through a
            // subclass-typed receiver is only found further up.
            let mut found: Option<usize> = None;
            let mut cur = Some(obj_type_idx);
            while let Some(ti) = cur {
                let Some(obj) = self.types_[ti].obj.as_ref() else {
                    break;
                };
                if let Some(p) = obj.proto.iter().find(|p| p.pindex as usize == field) {
                    found = Some(p.findex as usize);
                    break;
                }
                cur = obj.super_.as_ref().map(|t| t.0);
            }
            found
        } {
            // Runtime vtable dispatch for HOBJ/HSTRUCT.
            // `field` is the vobj_proto slot index.
            let vtable_slot = field as u64;

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
                .build_load(ptr_type, registers[args[0].idx()], "cm_obj")?
                .into_pointer_value();

            // Load hl_type* from object (offset 0)
            let type_ptr = self
                .builder
                .build_load(ptr_type, obj_val, "cm_type")?
                .into_pointer_value();

            // Build arg values with type casting (shared by both the
            // devirtualised and the vtable arm below).
            let expected_params = function.count_params() as usize;
            let mut arg_vals: Vec<BasicMetadataValueEnum> = Vec::with_capacity(expected_params);
            for (idx, arg) in args.iter().enumerate() {
                if idx >= expected_params {
                    break;
                }
                let loaded = self.builder.build_load(
                    reg_types[arg.idx()],
                    registers[arg.idx()],
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
            // falls into the vtable path unchanged. The per-site key is
            // pc 0 on this path; the caller-wide lookup is what fires.
            let devirt = if self.hot_reload {
                None
            } else {
                let caller = lowering.findex as u32;
                crate::callsite_profile::method_receiver(caller, 0)
                    .or_else(|| crate::callsite_profile::uniform_method_receiver(caller))
                    .and_then(|(type_ptr_c, target)| {
                        match self.get_or_create_function_value(target as usize) {
                            Ok((callee, ph)) => {
                                if ph {
                                    self.add_pending_compilation(target as usize);
                                }
                                (callee.get_type() == fn_type).then_some((callee, type_ptr_c))
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
                self.builder.build_conditional_branch(guard, hit_bb, miss_bb)?;

                self.builder.position_at_end(hit_bb);
                let ret = self
                    .builder
                    .build_call(callee, &arg_vals, "cm_devirt_call")?
                    .try_as_basic_value();
                if let Some(rv) = ret.basic() {
                    self.builder.build_store(registers[dst.idx()], rv)?;
                }
                self.builder.build_unconditional_branch(cm_done_bb)?;

                self.builder.position_at_end(miss_bb);
            }

            let vobj_proto = self.vobj_proto_ptr(type_ptr)?;

            // Load method pointer from vobj_proto[field]
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
                self.function_name(lowering.findex as u32)
                    .and_then(|caller| crate::callsite_profile::aot_target_for(&caller))
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
                    self.builder
                        .build_ptr_to_int(want, self.context.i64_type(), "cm_aot_want")?,
                    "cm_aot_devirt_guard",
                )?;
                self.builder.build_conditional_branch(guard, hit_bb, miss_bb)?;

                self.builder.position_at_end(hit_bb);
                let ret = self
                    .builder
                    .build_call(callee, &arg_vals, "cm_aot_devirt_call")?
                    .try_as_basic_value();
                if let Some(rv) = ret.basic() {
                    self.builder.build_store(registers[dst.idx()], rv)?;
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
                self.builder.build_store(registers[dst.idx()], ret_val)?;
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
                .build_load(ptr_type, registers[args[0].idx()], "vobj")?
                .into_pointer_value();

            // Load hl_type* from obj (offset 0)
            let obj_type_ptr = self
                .builder
                .build_load(ptr_type, obj_val, "obj_type")?
                .into_pointer_value();

            // Call hlp_get_obj_rt to get hl_runtime_obj*
            let hl_get_obj_rt =
                self.declare_native("hlp_get_obj_rt", &[ptr_type.into()], Some(ptr_type.into()));
            let rt_obj = self
                .builder
                .build_call(hl_get_obj_rt, &[obj_type_ptr.into()], "rt_obj")?
                .try_as_basic_value()
                .basic()
                .unwrap()
                .into_pointer_value();

            // Load the methods pointer from the target's C layout.
            let methods_gep = unsafe {
                self.builder.build_gep(
                    self.context.i8_type(),
                    rt_obj,
                    &[self
                        .context
                        .i64_type()
                        .const_int(self.target_abi.hl_runtime_obj_methods_offset(), false)],
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
                    &[self.context.i32_type().const_int(field as u64, false)],
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
                        .build_load(reg_types[arg.idx()], registers[arg.idx()], "arg_val")
                        .unwrap()
                        .into()
                })
                .collect();

            let arg_types: Vec<BasicMetadataTypeEnum> = args
                .iter()
                .map(|arg| reg_types[arg.idx()].into())
                .collect();

            let dst_kind = self.types_[lowering.regs[dst.idx()].0].kind;
            let fn_type = if dst_kind == hl_type_kind_HVOID {
                self.context.void_type().fn_type(&arg_types, false)
            } else {
                reg_types[dst.idx()].fn_type(&arg_types, false)
            };

            if let Some(ret_val) =
                self.build_stub_guarded_indirect_call(fn_type, fn_ptr, &arg_vals, "vcall")?
            {
                self.builder.build_store(registers[dst.idx()], ret_val)?;
            }
        }
        Ok(())
    }

    /// Call through a closure value: unwrap a signature-adapted wrapper,
    /// try the profiled target, then the typed path when the closure's
    /// runtime HFUN is the call site's, else the runtime marshaller.
    pub(super) fn emit_air_call_closure(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        dst: ValueId,
        fun: ValueId,
        args: &[ValueId],
    ) -> Result<()> {
        let raw_closure_ptr = self
            .builder
            .build_load(reg_types[fun.idx()], registers[fun.idx()], "closure_ptr")?
            .into_pointer_value();

        let i8_type = self.context.i8_type();
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let i32_type = self.context.i32_type();

        // HashLink represents a signature-adapted bound closure as a
        // vclosure_wrapper: its public vclosure has hasValue == 2 and
        // the original closure lives after it. Hybrid's interpreter
        // runner unwraps this object; LLVM must do the same before
        // reading the callable fields.
        let raw_has_value_gep = unsafe {
            self.builder.build_gep(
                i8_type,
                raw_closure_ptr,
                &[self
                    .context
                    .i64_type()
                    .const_int(self.target_abi.vclosure_has_value_offset(), false)],
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
        // Branch rather than load-then-select. `wrappedFun` exists only
        // on a `vclosure_wrapper`; a plain `vclosure` is shorter, and
        // that is what `hlp_alloc_closure_void`/`_ptr` allocate. Loading
        // it unconditionally would read past the end of every ordinary
        // closure -- a fault when the closure is the last object before
        // an unmapped page. Guarding the load also keeps the common path
        // within the bytes the object is known to have.
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
                &[self
                    .context
                    .i64_type()
                    .const_int(self.target_abi.vclosure_wrapper_fun_offset(), false)],
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

        // vclosure.fun
        let fun_field_gep = unsafe {
            self.builder.build_gep(
                i8_type,
                closure_ptr,
                &[self
                    .context
                    .i64_type()
                    .const_int(self.target_abi.vclosure_fun_offset(), false)],
                "closure_fun_gep",
            )?
        };
        let fun_ptr = self
            .builder
            .build_load(ptr_type, fun_field_gep, "closure_fun")?
            .into_pointer_value();

        // vclosure.hasValue
        let has_value_gep = unsafe {
            self.builder.build_gep(
                i8_type,
                closure_ptr,
                &[self
                    .context
                    .i64_type()
                    .const_int(self.target_abi.vclosure_has_value_offset(), false)],
                "closure_hasvalue_gep",
            )?
        };
        let has_value = self
            .builder
            .build_load(i32_type, has_value_gep, "has_value")?
            .into_int_value();

        // vclosure.value
        let value_gep = unsafe {
            self.builder.build_gep(
                i8_type,
                closure_ptr,
                &[self
                    .context
                    .i64_type()
                    .const_int(self.target_abi.vclosure_value_offset(), false)],
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
                    .build_load(reg_types[arg.idx()], registers[arg.idx()], "arg_val")
                    .unwrap()
                    .into()
            })
            .collect();

        // Determine function type from the closure value's type info
        let fun_type_idx = lowering.regs[fun.idx()].0;
        let base_fn_type = if let Some(fun_type) = self.types_[fun_type_idx].fun.clone() {
            self.create_function_type(&fun_type)?
        } else {
            // Dynamic-typed closure: infer from args (all ptrs) with ptr return
            let dyn_params: Vec<BasicMetadataTypeEnum> =
                args.iter().map(|_| ptr_type.into()).collect();
            // Determine return type from dst
            let dst_type = reg_types[dst.idx()];
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
        // common monomorphic arm first keeps the safety branch out of
        // the hot loop. The per-site key is pc 0 on this path; the
        // caller-wide lookup is what fires.
        let devirt = if self.hot_reload {
            None
        } else {
            let caller = lowering.findex as u32;
            crate::callsite_profile::closure_target(caller, 0)
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
                            (callee.get_type() == expected_ty).then_some((callee, target, exp_hv))
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
            // The profiled target is the interpreter's stub sentinel
            // (findex + 1), which is what a closure the interpreter
            // allocated holds in `fun`.
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
                self.builder.build_store(registers[dst.idx()], value)?;
            }
            self.builder.build_unconditional_branch(call_done_bb)?;
            self.builder.position_at_end(signature_bb);
        }

        // The value's HFUN is only the call-site contract. A
        // signature-adapted closure can carry a different runtime HFUN
        // and a wrapper body with that runtime ABI; calling it through
        // `base_fn_type` would turn scalar arguments into tiny pointers.
        // Same as Cranelift's lowering: keep the typed fast path for
        // equal signatures, otherwise let the runtime marshal using the
        // closure's own type.
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
        // In the entry block, and an array type rather than
        // `build_array_alloca`: the count is a constant here, and an
        // alloca in this conditional block would be a DYNAMIC one that
        // moves the stack pointer once per loop iteration and never
        // gives it back. Same for the boxing slots below. The GEP
        // indexes it by pointer, which is what an array of pointers is.
        let argv = self.entry_alloca(ptr_type.array_type(nargs.max(1) as u32), "closure_dyn_argv")?;
        let make_dyn = self.declare_native(
            "hlp_make_dyn",
            &[ptr_type.into(), ptr_type.into()],
            Some(ptr_type.into()),
        );
        for (index, arg) in args.iter().enumerate() {
            let type_index = lowering.regs[arg.idx()].0;
            let kind = self.types_[type_index].kind;
            let loaded = self.builder.build_load(
                reg_types[arg.idx()],
                registers[arg.idx()],
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
                let slot = self.entry_alloca(loaded.get_type(), "closure_dyn_box_slot")?;
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
        let dst_type_index = lowering.regs[dst.idx()].0;
        let dst_kind = self.types_[dst_type_index].kind;
        if dst_kind != hl_type_kind_HVOID {
            let dst_type = reg_types[dst.idx()];
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
                let result_slot = self.entry_alloca(ptr_type, "closure_dyn_result_slot")?;
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
                let (helper, helper_ret): (&str, BasicTypeEnum) = if dst_kind == hl_type_kind_HF64
                {
                    ("hlp_dyn_todouble", self.context.f64_type().into())
                } else if dst_kind == hl_type_kind_HF32 {
                    ("hlp_dyn_tofloat", self.context.f32_type().into())
                } else if dst_kind == hl_type_kind_HI64 {
                    ("hlp_dyn_toi64", self.context.i64_type().into())
                } else {
                    ("hlp_dyn_toint", i32_type.into())
                };
                let unbox = self.declare_native(helper, &[ptr_type.into()], Some(helper_ret));
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
            self.builder.build_store(registers[dst.idx()], value)?;
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

        let call_with_value_bb = self.context.append_basic_block(function, "call_with_value");
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
            self.builder.build_store(registers[dst.idx()], ret_val)?;
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
            self.builder.build_store(registers[dst.idx()], ret_val)?;
        }
        self.builder.build_unconditional_branch(call_done_bb)?;

        // Continue from call_done
        self.builder.position_at_end(call_done_bb);
        Ok(())
    }

    /// Closure over a function with no bound value. AOT emits a constant
    /// global; the JIT allocates one at run time.
    pub(super) fn emit_air_static_closure(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        dst: ValueId,
        fun: usize,
    ) -> Result<()> {
        // Declare the callee so it gets compiled; under lazy compilation
        // the slot is resolved at run time instead.
        if !self.lazy_compilation {
            let (_function, is_placeholder) = self.get_or_create_function_value(fun)?;
            if is_placeholder {
                self.add_pending_compilation(fun);
            }
        }

        let ptr_type = self.context.ptr_type(AddressSpace::default());

        // Load function address from functions_ptrs[findex] at runtime
        let findex = fun;
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
        self.builder.build_store(registers[dst.idx()], closure)?;
        Ok(())
    }

    /// Closure binding `obj` as the first argument of `fun`.
    pub(super) fn emit_air_instance_closure(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        dst: ValueId,
        fun: usize,
        obj: ValueId,
    ) -> Result<()> {
        if !self.lazy_compilation {
            let (_function, is_placeholder) = self.get_or_create_function_value(fun)?;
            if is_placeholder {
                self.add_pending_compilation(fun);
            }
        }

        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let findex = fun;

        // Load function address from functions_ptrs[findex]
        let fun_addr_ptr = self.function_slot_ptr(findex)?;
        let fun_addr = self
            .builder
            .build_load(ptr_type, fun_addr_ptr, "inst_closure_fun")?
            .into_pointer_value();

        let func_type_const = self.func_type_ptr(findex)?;
        // The METHOD's full type, unstripped: `hlp_alloc_closure_ptr`
        // strips the bound parameter itself, exactly as upstream's
        // OInstanceClosure does (jit_emit.c passes
        // `functions[functions_indexes[fun]].type`) and as the
        // Cranelift tier does. Stripping here too would make the
        // allocator strip a second time, walking off the end of the
        // `hl_type_fun` it was handed.
        let closure_type: inkwell::values::BasicValueEnum = func_type_const.into();

        // Load bound object
        let obj_val = self
            .builder
            .build_load(ptr_type, registers[obj.idx()], "inst_obj")?;

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
        self.builder.build_store(registers[dst.idx()], closure)?;
        Ok(())
    }

    /// Closure over proto method `field` of `obj`, bound to `obj`. The
    /// method is resolved from the static type's proto table at compile
    /// time and overridden by the concrete object's runtime proto.
    pub(super) fn emit_air_virtual_closure(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        dst: ValueId,
        obj: ValueId,
        field: usize,
    ) -> Result<()> {
        let i8_type = self.context.i8_type();
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let obj_type_idx = lowering.regs[obj.idx()].0;
        let obj_type_info = self.types_[obj_type_idx].clone();

        // Resolve findex from proto table at compile time
        let findex = if let Some(ref obj_data) = obj_type_info.obj {
            obj_data.proto[field].findex as usize
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
        let obj_val = self
            .builder
            .build_load(ptr_type, registers[obj.idx()], "vclos_obj")?;

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
                &[self.context.i32_type().const_int(field as u64, false)],
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
        // emit_air_instance_closure.
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
        self.builder.build_store(registers[dst.idx()], closure)?;
        Ok(())
    }
}
