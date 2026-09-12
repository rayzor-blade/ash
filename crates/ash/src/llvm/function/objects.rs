//! Emitting the object-model AIR instructions: field access, dynamic field
//! access, allocation, null checks and the type queries.
//!
//! Each emitter takes the AIR instruction's own fields. A value slot is
//! `registers[v.idx()]` / `reg_types[v.idx()]` and its HL type is
//! `lowering.regs[v.idx()].0`; `FieldGet`/`FieldSet` read the object type
//! from the instruction's `obj_ty`, the type AIR resolved the field against.

use air::v2::ir::{TypeRef as AirTypeRef, ValueId};
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, PointerValue};
use inkwell::{basic_block::BasicBlock, AddressSpace};

use crate::llvm::module::JITModule;

use super::{hl_hash_utf8, sized_alloc_enabled};
use crate::hl::{
    hl_type_kind_HBOOL, hl_type_kind_HDYNOBJ, hl_type_kind_HF32, hl_type_kind_HF64,
    hl_type_kind_HI32, hl_type_kind_HI64, hl_type_kind_HOBJ, hl_type_kind_HSTRUCT,
    hl_type_kind_HTYPE, hl_type_kind_HUI16, hl_type_kind_HUI8, hl_type_kind_HVIRTUAL,
    hl_type_kind_HVOID,
};
use crate::types::HLFunction;
use anyhow::{anyhow, Result};

impl<'ctx> JITModule<'ctx> {
    /// `dst = obj.field`, with `obj_ty` as the object's type.
    pub(super) fn emit_air_field_get(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        dst: ValueId,
        obj: ValueId,
        obj_ty: AirTypeRef,
        field: usize,
    ) -> Result<()> {
        let obj_type_idx = obj_ty.0 as usize;
        let obj_type_ = &self.types_[obj_type_idx];
        let obj_val =
            self.builder
                .build_load(reg_types[obj.idx()], registers[obj.idx()], "obj_val")?;
        match obj_type_.kind {
            hl_type_kind_HSTRUCT | hl_type_kind_HOBJ => {
                let field_ptr =
                    self.build_field_ptr(obj_type_idx, field, obj_val.into_pointer_value())?;

                // Load the field value using destination register type
                let load_type = self.get_register_type(lowering.regs[dst.idx()].0)?;
                let field_val = self.builder.build_load(load_type, field_ptr, "field_val")?;
                self.tbaa_field(field_val.as_instruction_value(), obj_type_idx, field);

                self.builder.build_store(registers[dst.idx()], field_val)?;
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
                        &[self
                            .context
                            .i64_type()
                            .const_int(self.target_abi.vvirtual_fields_offset(), false)],
                        "vfields_ptr",
                    )?
                };

                // Get the field pointer: vfields[field] (array of pointers)
                let field_ptr = unsafe {
                    self.builder.build_gep(
                        ptr_type,
                        vfields_ptr,
                        &[self.context.i32_type().const_int(field as u64, false)],
                        "field_ptr",
                    )?
                };

                // Check if the field exists
                let field_value_check =
                    self.builder
                        .build_load(ptr_type, field_ptr, "field_value_ptr")?;
                let field_exists = self
                    .builder
                    .build_is_not_null(field_value_check.into_pointer_value(), "field_exists")?;

                let current_fn = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();
                let then_block = self.context.append_basic_block(current_fn, "field_exists");
                let else_block = self
                    .context
                    .append_basic_block(current_fn, "field_not_exists");
                let cont_block = self.context.append_basic_block(current_fn, "cont");

                self.builder
                    .build_conditional_branch(field_exists, then_block, else_block)?;

                // Field exists: r = *hl_vfields(o)[f]
                self.builder.position_at_end(then_block);
                let field_value_ptr =
                    self.builder
                        .build_load(ptr_type, field_ptr, "field_value_ptr")?;
                // Load with the destination register's type, not ptr,
                // to avoid reading more bytes than the field actually holds.
                let dst_load_type = reg_types[dst.idx()];
                let field_value = self.builder.build_load(
                    dst_load_type,
                    field_value_ptr.into_pointer_value(),
                    "field_value",
                )?;
                self.builder
                    .build_store(registers[dst.idx()], field_value)?;
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
                        &[self
                            .context
                            .i64_type()
                            .const_int(self.target_abi.vvirtual_value_offset(), false)],
                        "fb_value_gep",
                    )?
                };
                let value_obj = self.builder.build_load(ptr_type, value_gep, "fb_value")?;
                let hashed_name = obj_type_
                    .virt
                    .as_ref()
                    .map(|v| v.fields.get(field).map(|f| f.hashed_name).unwrap_or(0))
                    .unwrap_or(0);
                let field_hash = i32_type.const_int(hashed_name as u64, true);
                let dst_type_idx = lowering.regs[dst.idx()].0;
                let dst_kind = self.types_[dst_type_idx].kind;
                // Pick the getter by the DESTINATION's kind, the way
                // `DynGet` does: the helper's return width has to match
                // the slot it is stored into.
                let type_ptr = self
                    .get_initialized_type(dst_type_idx)?
                    .into_pointer_value();
                let f32_type = self.context.f32_type();
                let f64_type = self.context.f64_type();
                let i64_type = self.context.i64_type();
                let (getter, args, ret): (&str, Vec<_>, BasicTypeEnum) = match dst_kind {
                    hl_type_kind_HF64 => (
                        "hlp_dyn_getd",
                        vec![ptr_type.into(), i32_type.into()],
                        f64_type.into(),
                    ),
                    hl_type_kind_HF32 => (
                        "hlp_dyn_getf",
                        vec![ptr_type.into(), i32_type.into()],
                        f32_type.into(),
                    ),
                    hl_type_kind_HI64 => (
                        "hlp_dyn_geti64",
                        vec![ptr_type.into(), i32_type.into()],
                        i64_type.into(),
                    ),
                    hl_type_kind_HI32 | hl_type_kind_HBOOL | hl_type_kind_HUI8
                    | hl_type_kind_HUI16 => (
                        "hlp_dyn_geti",
                        vec![ptr_type.into(), i32_type.into(), ptr_type.into()],
                        i32_type.into(),
                    ),
                    _ => (
                        "hlp_dyn_getp",
                        vec![ptr_type.into(), i32_type.into(), ptr_type.into()],
                        ptr_type.into(),
                    ),
                };
                let getter = self.declare_native(getter, &args, Some(ret.into()));
                let mut call_args: Vec<inkwell::values::BasicMetadataValueEnum> =
                    vec![value_obj.into(), field_hash.into()];
                if args.len() == 3 {
                    call_args.push(type_ptr.into());
                }
                let result = self.builder.build_call(getter, &call_args, "dyn_get_fb")?;
                let dyn_field_value = result.try_as_basic_value().basic().unwrap();
                // `hlp_dyn_geti` answers every narrow integer kind as
                // i32, so a HBOOL/HUI8/HUI16 slot still needs the
                // truncation its width implies.
                let dyn_field_value = self.cast_for_call(dyn_field_value, reg_types[dst.idx()])?;
                self.builder
                    .build_store(registers[dst.idx()], dyn_field_value)?;
                self.builder.build_unconditional_branch(cont_block)?;

                // Continue
                self.builder.position_at_end(cont_block);
            }
            _ => return Err(anyhow!("Could not get field of non-object type")),
        }
        Ok(())
    }

    /// `obj.field = src`, with `obj_ty` as the object's type.
    pub(super) fn emit_air_field_set(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        obj: ValueId,
        obj_ty: AirTypeRef,
        field: usize,
        src: ValueId,
    ) -> Result<()> {
        let obj_type_idx = obj_ty.0 as usize;
        let obj_type_ = &self.types_[obj_type_idx];
        let obj_val = self
            .builder
            .build_load(reg_types[obj.idx()], registers[obj.idx()], "obj_val")?
            .into_pointer_value();
        let src_val =
            self.builder
                .build_load(reg_types[src.idx()], registers[src.idx()], "src_val")?;

        match obj_type_.kind {
            hl_type_kind_HSTRUCT | hl_type_kind_HOBJ => {
                let field_ptr = self.build_field_ptr(obj_type_idx, field, obj_val)?;
                let st = self.builder.build_store(field_ptr, src_val)?;
                self.tbaa_field(Some(st), obj_type_idx, field);
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
                        &[self
                            .context
                            .i64_type()
                            .const_int(self.target_abi.vvirtual_fields_offset(), false)],
                        "vfields_ptr",
                    )?
                };

                // Get the field pointer: vfields[field] (array of pointers)
                let field_ptr = unsafe {
                    self.builder.build_gep(
                        ptr_type,
                        vfields_ptr,
                        &[self.context.i32_type().const_int(field as u64, false)],
                        "field_ptr",
                    )?
                };

                // Check if the field exists
                let field_value_ptr =
                    self.builder
                        .build_load(ptr_type, field_ptr, "field_value_ptr")?;
                let field_exists = self
                    .builder
                    .build_is_not_null(field_value_ptr.into_pointer_value(), "field_exists")?;

                let current_fn = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();
                let then_block = self.context.append_basic_block(current_fn, "field_exists");
                let else_block = self
                    .context
                    .append_basic_block(current_fn, "field_not_exists");
                let cont_block = self.context.append_basic_block(current_fn, "cont");

                self.builder
                    .build_conditional_branch(field_exists, then_block, else_block)?;

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
                    .map(|v| v.fields.get(field).map(|f| f.hashed_name).unwrap_or(0))
                    .unwrap_or(0);
                let field_hash = self.context.i32_type().const_int(hashed_name as u64, false);
                let src_type_idx = lowering.regs[src.idx()].0;
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
                    let tmp = self.entry_alloca(reg_types[src.idx()], "tmp_box")?;
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
                        .build_call(make_dyn, &[tmp.into(), type_ptr_val.into()], "boxed_val")?
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
                        &[self
                            .context
                            .i64_type()
                            .const_int(self.target_abi.vvirtual_value_offset(), false)],
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
        Ok(())
    }

    /// `dst = obj.<field>` through the runtime; `field` is a string-pool index.
    pub(super) fn emit_air_dyn_get(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        dst: ValueId,
        obj: ValueId,
        field: usize,
    ) -> Result<()> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let i32_type = self.context.i32_type();
        let i64_type = self.context.i64_type();

        let obj_val = self
            .builder
            .build_load(ptr_type, registers[obj.idx()], "dynget_obj")?;
        let field_name = &self.bytecode.strings[field].clone();
        let hfield = hl_hash_utf8(field_name);
        let hfield_val = i32_type.const_int(hfield as u64, true);

        let dst_type_idx = lowering.regs[dst.idx()].0;
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
                    registers[dst.idx()],
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
                    registers[dst.idx()],
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
                    registers[dst.idx()],
                    result.try_as_basic_value().basic().unwrap(),
                )?;
            }
            hl_type_kind_HI32 | hl_type_kind_HBOOL | hl_type_kind_HUI8 | hl_type_kind_HUI16 => {
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
                    registers[dst.idx()],
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
                    registers[dst.idx()],
                    result.try_as_basic_value().basic().unwrap(),
                )?;
            }
        }
        Ok(())
    }

    /// `obj.<field> = src` through the runtime; `field` is a string-pool index.
    pub(super) fn emit_air_dyn_set(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        obj: ValueId,
        field: usize,
        src: ValueId,
    ) -> Result<()> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let i32_type = self.context.i32_type();

        let obj_val = self
            .builder
            .build_load(ptr_type, registers[obj.idx()], "dynset_obj")?;
        let field_name = &self.bytecode.strings[field].clone();
        let hfield = hl_hash_utf8(field_name);
        let hfield_val = i32_type.const_int(hfield as u64, true);

        let src_type_idx = lowering.regs[src.idx()].0;
        let src_kind = self.types_[src_type_idx].kind;
        let src_val =
            self.builder
                .build_load(reg_types[src.idx()], registers[src.idx()], "dynset_src")?;

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
            hl_type_kind_HI32 | hl_type_kind_HBOOL | hl_type_kind_HUI8 | hl_type_kind_HUI16 => {
                let type_ptr = self
                    .get_initialized_type(src_type_idx)?
                    .into_pointer_value();
                // All four kinds share one setter whose value parameter is
                // i32, but their registers are not: HBOOL loads as i1, HUI8
                // as i8, HUI16 as i16. Widen first; all three narrow kinds
                // are unsigned, so zero-extend.
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
        Ok(())
    }

    /// Allocate an object of `dst`'s type.
    pub(super) fn emit_air_new(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        dst: ValueId,
    ) -> Result<()> {
        let type_index = lowering.regs[dst.idx()].0;
        let type_kind = self.types_[type_index].kind;

        match type_kind {
            hl_type_kind_HSTRUCT | hl_type_kind_HOBJ => {
                let type_ptr = self.get_initialized_type(type_index)?.into_pointer_value();

                // What `hlp_alloc_obj` re-derives per allocation is
                // fixed for a type: the size, and whether the class
                // binds closures into fields at construction. Both are
                // known here, so an ordinary class allocates through
                // the sized entry and skips them. A struct keeps the
                // general path -- it has no type header to stamp.
                let sized = (type_kind == hl_type_kind_HOBJ && sized_alloc_enabled())
                    .then(|| {
                        let no_bindings = self.types_[type_index]
                            .obj
                            .as_ref()
                            .is_some_and(|o| o.bindings.is_empty());
                        let size = crate::layout::object_layout_for(
                            &self.types_,
                            type_index,
                            self.target_abi.pointer_bytes() as i32,
                        )
                        .map(|l| l.size);
                        match (no_bindings, size) {
                            (true, Some(size)) if size > 0 => Some(size as u64),
                            _ => None,
                        }
                    })
                    .flatten();

                let result = if let Some(size) = sized {
                    // `size` is a `usize` in the runtime, so it is the
                    // target's pointer width -- i32 on wasm32, where a
                    // hardcoded i64 makes the module fail validation
                    // at the call.
                    let size_type = self.target_abi.pointer_int_type(self.context);
                    let fun = self.declare_native(
                        "hlp_alloc_obj_sized",
                        &[
                            self.context.ptr_type(AddressSpace::default()).into(),
                            size_type.into(),
                        ],
                        Some(self.context.ptr_type(AddressSpace::default()).into()),
                    );
                    self.builder.build_call(
                        fun,
                        &[type_ptr.into(), size_type.const_int(size, false).into()],
                        "call",
                    )?
                } else {
                    let ptr_type = self.context.ptr_type(AddressSpace::default());
                    let fun = self.declare_native(
                        "hlp_alloc_obj",
                        &[ptr_type.into()],
                        Some(ptr_type.into()),
                    );
                    self.builder.build_call(fun, &[type_ptr.into()], "call")?
                };
                self.builder.build_store(
                    registers[dst.idx()],
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
                    registers[dst.idx()],
                    result.try_as_basic_value().basic().unwrap(),
                );
            }
            hl_type_kind_HVIRTUAL => {
                let type_ptr = self.get_initialized_type(type_index)?.into_pointer_value();
                let fun = self.declare_native(
                    "hlp_alloc_virtual",
                    &[self.context.ptr_type(AddressSpace::default()).into()],
                    Some(self.context.ptr_type(AddressSpace::default()).into()),
                );
                let result = self.builder.build_call(fun, &[type_ptr.into()], "call")?;
                self.builder.build_store(
                    registers[dst.idx()],
                    result.try_as_basic_value().basic().unwrap(),
                );
            }
            _ => return Err(anyhow!("Can't call constructor on invalid type")),
        }
        Ok(())
    }

    /// Throw "Null access" if `value` is null, else branch to `next`, the
    /// block of the following instruction. A pointer slot leaves the builder
    /// in the terminated throw block; a non-pointer slot emits nothing.
    pub(super) fn emit_air_null_check(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        value: ValueId,
        next: BasicBlock<'ctx>,
    ) -> Result<()> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let val = self.builder.build_load(
            reg_types[value.idx()],
            registers[value.idx()],
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
            self.builder
                .build_conditional_branch(is_null, throw_block, next)?;
            self.builder.position_at_end(throw_block);
            // A null here is a catchable HashLink exception, "Null
            // access" -- what the interpreter throws and what Haxe
            // code (and the unit suite) catches. A bare `unreachable`
            // would make it a `brk` at run time and, worse, tell the
            // optimizer the pointer is never null, so O3 could drop
            // the test and everything guarded by it.
            let err_fn_type = self.context.void_type().fn_type(&[ptr_type.into()], true);
            let err_ptr = self.error_function_ptr()?;
            let msg_ptr = self.utf16_message("Null access")?;
            self.builder.build_indirect_call(
                err_fn_type,
                err_ptr,
                &[msg_ptr.into()],
                "null_access_throw",
            )?;
            self.builder.build_unreachable()?;
        }
        // Non-pointer types are never null, fall through
        Ok(())
    }

    /// `dst = src->t`; the void type for a null `src`.
    pub(super) fn emit_air_get_type(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        dst: ValueId,
        src: ValueId,
    ) -> Result<()> {
        // GetType reads the runtime hl_type* from the value's ->t field (offset 0)
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let src_val =
            self.builder
                .build_load(reg_types[src.idx()], registers[src.idx()], "gettype_src")?;
        let obj_ptr = src_val.into_pointer_value();
        // `hl_typeof(NULL)` is the void type, and `Type.typeof(null)`,
        // `Reflect.isFunction(null)` and a JSON printer walking an
        // object with a null field all rely on it.
        let current_fn = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();
        let null_block = self.context.append_basic_block(current_fn, "gettype_null");
        let load_block = self.context.append_basic_block(current_fn, "gettype_load");
        let cont_block = self.context.append_basic_block(current_fn, "gettype_cont");
        let is_null = self.builder.build_is_null(obj_ptr, "gettype_is_null")?;
        self.builder
            .build_conditional_branch(is_null, null_block, load_block)?;
        self.builder.position_at_end(null_block);
        let void_index = self
            .types_
            .iter()
            .position(|t| t.kind == hl_type_kind_HVOID)
            .ok_or_else(|| anyhow!("GetType: no void type in the type table"))?;
        let void_type = self.get_initialized_type(void_index)?;
        self.builder.build_store(registers[dst.idx()], void_type)?;
        self.builder.build_unconditional_branch(cont_block)?;
        self.builder.position_at_end(load_block);
        // obj->t is the first field (offset 0) of vdynamic/vobj, a pointer to hl_type
        let t_ptr = self
            .builder
            .build_load(ptr_type, obj_ptr, "gettype_t")?
            .into_pointer_value();
        self.builder.build_store(registers[dst.idx()], t_ptr)?;
        self.builder.build_unconditional_branch(cont_block)?;
        self.builder.position_at_end(cont_block);
        Ok(())
    }

    /// `dst = kind of src`: the static kind for a non-pointer or null `src`,
    /// otherwise read through the header.
    pub(super) fn emit_air_get_tid(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        dst: ValueId,
        src: ValueId,
    ) -> Result<()> {
        let src_val =
            self.builder
                .build_load(reg_types[src.idx()], registers[src.idx()], "gettid_src")?;
        let src_type_kind = self.types_[lowering.regs[src.idx()].0].kind;
        if src_val.is_pointer_value() {
            let obj = src_val.into_pointer_value();
            // A null source answers with the register's STATIC kind,
            // which is what the interpreter returns and is a constant
            // here. Both shapes below dereference, so the guard wraps
            // them rather than each load.
            let current_fn = self
                .builder
                .get_insert_block()
                .and_then(|b| b.get_parent())
                .ok_or_else(|| anyhow!("GetTID: builder is not inside a function"))?;
            let null_block = self.context.append_basic_block(current_fn, "gettid_null");
            let load_block = self.context.append_basic_block(current_fn, "gettid_load");
            let cont_block = self.context.append_basic_block(current_fn, "gettid_cont");
            let is_null = self.builder.build_is_null(obj, "gettid_is_null")?;
            self.builder
                .build_conditional_branch(is_null, null_block, load_block)?;

            self.builder.position_at_end(null_block);
            self.builder.build_store(
                registers[dst.idx()],
                self.context
                    .i32_type()
                    .const_int(src_type_kind as u64, false),
            )?;
            self.builder.build_unconditional_branch(cont_block)?;

            self.builder.position_at_end(load_block);
            if src_type_kind == hl_type_kind_HTYPE {
                // Source is hl_type* — kind is directly at offset 0
                let kind = self
                    .builder
                    .build_load(self.context.i32_type(), obj, "gettid_kind")?;
                self.builder.build_store(registers[dst.idx()], kind)?;
            } else {
                // Source is an object — load obj->t (offset 0), then t->kind (offset 0)
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let t_ptr = self
                    .builder
                    .build_load(ptr_type, obj, "gettid_type")?
                    .into_pointer_value();
                let kind =
                    self.builder
                        .build_load(self.context.i32_type(), t_ptr, "gettid_kind")?;
                self.builder.build_store(registers[dst.idx()], kind)?;
            }
            self.builder.build_unconditional_branch(cont_block)?;
            self.builder.position_at_end(cont_block);
        } else {
            // Compile-time: type kind is known
            let type_idx = lowering.regs[src.idx()].0;
            let kind = self.types_[type_idx].kind;
            let kind_val = self.context.i32_type().const_int(kind as u64, false);
            self.builder.build_store(registers[dst.idx()], kind_val)?;
        }
        Ok(())
    }

    /// `dst = array.size`; 0 for a null array.
    pub(super) fn emit_air_array_size(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        dst: ValueId,
        array: ValueId,
    ) -> Result<()> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let arr = self
            .builder
            .build_load(ptr_type, registers[array.idx()], "arrsize_ptr")?
            .into_pointer_value();
        // The size of no array is 0, which is what the interpreter
        // answers; loading it would fault. Same diamond as `GetType`.
        let current_fn = self
            .builder
            .get_insert_block()
            .and_then(|b| b.get_parent())
            .ok_or_else(|| anyhow!("ArraySize: builder is not inside a function"))?;
        let null_block = self.context.append_basic_block(current_fn, "arrsize_null");
        let load_block = self.context.append_basic_block(current_fn, "arrsize_load");
        let cont_block = self.context.append_basic_block(current_fn, "arrsize_cont");
        let is_null = self.builder.build_is_null(arr, "arrsize_is_null")?;
        self.builder
            .build_conditional_branch(is_null, null_block, load_block)?;

        self.builder.position_at_end(null_block);
        self.builder
            .build_store(registers[dst.idx()], self.context.i32_type().const_zero())?;
        self.builder.build_unconditional_branch(cont_block)?;

        self.builder.position_at_end(load_block);
        // varray.size is at offset 16
        let size_gep = unsafe {
            self.builder.build_gep(
                self.context.i8_type(),
                arr,
                &[self
                    .context
                    .i64_type()
                    .const_int(self.target_abi.varray_size_offset(), false)],
                "arrsize_gep",
            )?
        };
        let size = self
            .builder
            .build_load(self.context.i32_type(), size_gep, "arrsize_val")?;
        if let Some(i) = size.as_instruction_value() {
            self.tbaa.tag(i, self.tbaa.array_len());
        }
        self.builder.build_store(registers[dst.idx()], size)?;
        self.builder.build_unconditional_branch(cont_block)?;

        self.builder.position_at_end(cont_block);
        Ok(())
    }
}
