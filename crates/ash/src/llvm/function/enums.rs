//! Emitting the AIR enum instructions: allocation, construction, index and
//! field access.
//!
//! Each emitter takes the AIR instruction's own fields. A value's slot is
//! `registers[v.idx()]` / `reg_types[v.idx()]`, its HL type index
//! `lowering.regs[v.idx()].0`; a cell's slot sits at `cell_base + c.idx()`.
//! Payload offsets come from the loader's `tenum.constructs[c].offsets`.

use air::v2::ir::ValueId;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::PointerValue;
use inkwell::AddressSpace;

use crate::llvm::module::JITModule;
use crate::types::HLFunction;
use anyhow::{anyhow, Result};

impl<'ctx> JITModule<'ctx> {
    pub(super) fn emit_air_enum_alloc(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        dst: ValueId,
        construct: usize,
    ) -> Result<()> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let i32_type = self.context.i32_type();
        let type_index = lowering.regs[dst.idx()].0;
        let type_ptr = self.get_initialized_type(type_index)?.into_pointer_value();

        let alloc_enum = self.declare_native(
            "hlp_alloc_enum",
            &[ptr_type.into(), i32_type.into()],
            Some(ptr_type.into()),
        );
        let construct_val = i32_type.const_int(construct as u64, false);
        let result = self.builder.build_call(
            alloc_enum,
            &[type_ptr.into(), construct_val.into()],
            "enum_alloc",
        )?;
        self.builder.build_store(
            registers[dst.idx()],
            result.try_as_basic_value().basic().unwrap(),
        )?;
        Ok(())
    }

    pub(super) fn emit_air_make_enum(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        dst: ValueId,
        construct: usize,
        args: &[ValueId],
    ) -> Result<()> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let i32_type = self.context.i32_type();
        let i8_type = self.context.i8_type();
        let type_index = lowering.regs[dst.idx()].0;
        let type_ptr = self.get_initialized_type(type_index)?.into_pointer_value();

        // Allocate the enum
        let alloc_enum = self.declare_native(
            "hlp_alloc_enum",
            &[ptr_type.into(), i32_type.into()],
            Some(ptr_type.into()),
        );
        let construct_val = i32_type.const_int(construct as u64, false);
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
        let construct_info = &tenum.constructs[construct];

        for (j, arg) in args.iter().enumerate() {
            let arg_val = self.builder.build_load(
                reg_types[arg.idx()],
                registers[arg.idx()],
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

        self.builder.build_store(registers[dst.idx()], venum_ptr)?;
        Ok(())
    }

    pub(super) fn emit_air_enum_index(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        dst: ValueId,
        value: ValueId,
    ) -> Result<()> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let venum_ptr = self
            .builder
            .build_load(ptr_type, registers[value.idx()], "enumidx_ptr")?
            .into_pointer_value();
        // The index of no enum value is 0, as the interpreter answers.
        let (_, load_block, cont_block) = self.null_guard("enumidx", venum_ptr)?;
        self.builder.build_store(
            registers[dst.idx()],
            self.context.i32_type().const_zero(),
        )?;
        self.builder.build_unconditional_branch(cont_block)?;
        self.builder.position_at_end(load_block);
        // venum.index is i32 at offset 8
        let index_gep = unsafe {
            self.builder.build_gep(
                self.context.i8_type(),
                venum_ptr,
                &[self
                    .context
                    .i64_type()
                    .const_int(self.target_abi.venum_index_offset(), false)],
                "enumidx_gep",
            )?
        };
        let index_val =
            self.builder
                .build_load(self.context.i32_type(), index_gep, "enumidx_val")?;
        self.builder.build_store(registers[dst.idx()], index_val)?;
        self.builder.build_unconditional_branch(cont_block)?;
        self.builder.position_at_end(cont_block);
        Ok(())
    }

    pub(super) fn emit_air_enum_field(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        dst: ValueId,
        value: ValueId,
        construct: usize,
        field: usize,
    ) -> Result<()> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let venum_ptr = self
            .builder
            .build_load(ptr_type, registers[value.idx()], "enumfield_ptr")?
            .into_pointer_value();
        // A field of no enum value is null, as the interpreter answers.
        let (_, load_block, cont_block) = self.null_guard("enumfield", venum_ptr)?;
        self.builder
            .build_store(registers[dst.idx()], reg_types[dst.idx()].const_zero())?;
        self.builder.build_unconditional_branch(cont_block)?;
        self.builder.position_at_end(load_block);

        let value_type_idx = lowering.regs[value.idx()].0;
        let tenum = self.types_[value_type_idx]
            .tenum
            .as_ref()
            .ok_or_else(|| anyhow!("EnumField: type {} is not an enum", value_type_idx))?;
        let construct_info = &tenum.constructs[construct];
        let offset = construct_info.offsets[field] as u64;

        let param_ptr = unsafe {
            self.builder.build_gep(
                self.context.i8_type(),
                venum_ptr,
                &[self.context.i64_type().const_int(offset, false)],
                "enumfield_gep",
            )?
        };
        let val = self
            .builder
            .build_load(reg_types[dst.idx()], param_ptr, "enumfield_val")?;
        self.builder.build_store(registers[dst.idx()], val)?;
        self.builder.build_unconditional_branch(cont_block)?;
        self.builder.position_at_end(cont_block);
        Ok(())
    }

    /// The construct is the instruction's own; AIR resolved it when lowering
    /// from the preceding EnumAlloc/MakeEnum of the same register.
    pub(super) fn emit_air_set_enum_field(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        value: ValueId,
        construct: usize,
        field: usize,
        src: ValueId,
    ) -> Result<()> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let venum_ptr = self
            .builder
            .build_load(ptr_type, registers[value.idx()], "setenumfield_ptr")?
            .into_pointer_value();
        // Setting a field of no enum value does nothing, as in the
        // interpreter -- the null arm falls straight through.
        let (_, load_block, cont_block) = self.null_guard("setenumfield", venum_ptr)?;
        self.builder.build_unconditional_branch(cont_block)?;
        self.builder.position_at_end(load_block);
        let src_val = self.builder.build_load(
            reg_types[src.idx()],
            registers[src.idx()],
            "setenumfield_val",
        )?;

        let value_type_idx = lowering.regs[value.idx()].0;
        let tenum = self.types_[value_type_idx].tenum.as_ref().ok_or_else(|| {
            anyhow!("SetEnumField: type {} is not an enum", value_type_idx)
        })?;

        let construct_info = &tenum.constructs[construct];
        let offset = construct_info.offsets[field] as u64;
        let param_ptr = unsafe {
            self.builder.build_gep(
                self.context.i8_type(),
                venum_ptr,
                &[self.context.i64_type().const_int(offset, false)],
                "setenumfield_gep",
            )?
        };
        self.builder.build_store(param_ptr, src_val)?;
        self.builder.build_unconditional_branch(cont_block)?;
        self.builder.position_at_end(cont_block);
        Ok(())
    }
}
