//! Emitting the AIR memory family: raw and array loads and stores, pinned
//! register cells, references, bytes constants, and the assert / prefetch /
//! inline-asm / end-trap leftovers.
//!
//! Each emitter takes the AIR instruction's own fields. A value lives in
//! `registers[v.idx()]` typed `reg_types[v.idx()]`; a cell in
//! `registers[cell_base + c.idx()]`. `lowering.regs` carries the HL type of
//! every value and cell slot in the same order.

use air::v2::ir::{CellId, MemAccess as AirMemAccess, ValueId};
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, PointerValue};
use inkwell::AddressSpace;

use crate::llvm::module::JITModule;
use crate::types::HLFunction;
use anyhow::{anyhow, Result};

impl<'ctx> JITModule<'ctx> {
    /// `dst = base[index]` at the width selected by `kind`.
    ///
    /// I8/I16 load a byte or half-word and shape it for the destination
    /// slot; Mem and Array load at the destination slot's own type. Array
    /// indexes past the varray header and scales the index by the
    /// destination register's element size.
    pub(super) fn emit_air_mem_get(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        kind: AirMemAccess,
        dst: ValueId,
        base: ValueId,
        index: ValueId,
    ) -> Result<()> {
        match kind {
            AirMemAccess::I8 => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let base = self
                    .builder
                    .build_load(ptr_type, registers[base.idx()], "geti8_base")?
                    .into_pointer_value();
                let idx = self
                    .builder
                    .build_load(self.context.i32_type(), registers[index.idx()], "geti8_idx")?
                    .into_int_value();
                let addr = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), base, &[idx], "geti8_addr")?
                };
                let val = self
                    .builder
                    .build_load(self.context.i8_type(), addr, "geti8_val")?
                    .into_int_value();
                // Shaped for the destination's own slot, which HL types as
                // Int but a rewritten program may type narrower or as Bool.
                let ext = self.int_for_slot(val, lowering, reg_types, dst)?;
                self.builder.build_store(registers[dst.idx()], ext)?;
            }
            AirMemAccess::I16 => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let base = self
                    .builder
                    .build_load(ptr_type, registers[base.idx()], "geti16_base")?
                    .into_pointer_value();
                let idx = self
                    .builder
                    .build_load(
                        self.context.i32_type(),
                        registers[index.idx()],
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
                let ext = self.int_for_slot(val, lowering, reg_types, dst)?;
                self.builder.build_store(registers[dst.idx()], ext)?;
            }
            AirMemAccess::Mem => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let base = self
                    .builder
                    .build_load(ptr_type, registers[base.idx()], "getmem_base")?
                    .into_pointer_value();
                let idx = self
                    .builder
                    .build_load(
                        self.context.i32_type(),
                        registers[index.idx()],
                        "getmem_idx",
                    )?
                    .into_int_value();
                let addr = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), base, &[idx], "getmem_addr")?
                };
                let val = self
                    .builder
                    .build_load(reg_types[dst.idx()], addr, "getmem_val")?;
                if let Some(i) = val.as_instruction_value() {
                    self.tbaa.tag(i, self.tbaa.payload());
                }
                self.builder.build_store(registers[dst.idx()], val)?;
            }
            AirMemAccess::Array => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let i32_type = self.context.i32_type();
                let i8_type = self.context.i8_type();

                let arr = self
                    .builder
                    .build_load(ptr_type, registers[base.idx()], "getarr_ptr")?
                    .into_pointer_value();
                let idx = self
                    .builder
                    .build_load(i32_type, registers[index.idx()], "getarr_idx")?
                    .into_int_value();

                // Data starts at offset 24 (sizeof(varray))
                let data_ptr = unsafe {
                    self.builder.build_gep(
                        i8_type,
                        arr,
                        &[self
                            .context
                            .i64_type()
                            .const_int(self.target_abi.varray_data_offset(), false)],
                        "getarr_data",
                    )?
                };

                // Element size from the destination register's
                // kind, via the table `crate::layout` shares with the Cranelift
                // tier so the two cannot index an array differently.
                let dst_type_idx = lowering.regs[dst.idx()].0;
                let dst_kind = self.types_[dst_type_idx].kind;
                let elem_size = crate::layout::array_elem_size_for(
                    dst_kind,
                    self.target_abi.pointer_bytes() as i32,
                ) as u64;

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
                        .build_load(reg_types[dst.idx()], slot, "getarr_val")?;
                if let Some(i) = element_val.as_instruction_value() {
                    self.tbaa.tag(i, self.tbaa.payload());
                }
                self.builder
                    .build_store(registers[dst.idx()], element_val)?;
            }
        }
        Ok(())
    }

    /// `base[index] = src` at the width selected by `kind`.
    ///
    /// I8/I16 truncate the source; Mem and Array store the source slot's
    /// value as is. Array indexes past the varray header and scales the
    /// index by the source register's element size.
    pub(super) fn emit_air_mem_set(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        kind: AirMemAccess,
        base: ValueId,
        index: ValueId,
        src: ValueId,
    ) -> Result<()> {
        match kind {
            AirMemAccess::I8 => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let base = self
                    .builder
                    .build_load(ptr_type, registers[base.idx()], "seti8_base")?
                    .into_pointer_value();
                let idx = self
                    .builder
                    .build_load(self.context.i32_type(), registers[index.idx()], "seti8_idx")?
                    .into_int_value();
                let src_val = self
                    .builder
                    .build_load(reg_types[src.idx()], registers[src.idx()], "seti8_src")?
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
            AirMemAccess::I16 => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let base = self
                    .builder
                    .build_load(ptr_type, registers[base.idx()], "seti16_base")?
                    .into_pointer_value();
                let idx = self
                    .builder
                    .build_load(
                        self.context.i32_type(),
                        registers[index.idx()],
                        "seti16_idx",
                    )?
                    .into_int_value();
                let src_val = self
                    .builder
                    .build_load(reg_types[src.idx()], registers[src.idx()], "seti16_src")?
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
            AirMemAccess::Mem => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let base = self
                    .builder
                    .build_load(ptr_type, registers[base.idx()], "setmem_base")?
                    .into_pointer_value();
                let idx = self
                    .builder
                    .build_load(
                        self.context.i32_type(),
                        registers[index.idx()],
                        "setmem_idx",
                    )?
                    .into_int_value();
                let src_val = self.builder.build_load(
                    reg_types[src.idx()],
                    registers[src.idx()],
                    "setmem_src",
                )?;
                let addr = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), base, &[idx], "setmem_addr")?
                };
                let st = self.builder.build_store(addr, src_val)?;
                self.tbaa.tag(st, self.tbaa.payload());
            }
            AirMemAccess::Array => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let i32_type = self.context.i32_type();
                let i8_type = self.context.i8_type();

                let arr = self
                    .builder
                    .build_load(ptr_type, registers[base.idx()], "setarr_ptr")?
                    .into_pointer_value();
                let idx = self
                    .builder
                    .build_load(i32_type, registers[index.idx()], "setarr_idx")?
                    .into_int_value();
                let src_val = self.builder.build_load(
                    reg_types[src.idx()],
                    registers[src.idx()],
                    "setarr_val",
                )?;

                // Data starts at offset 24 (sizeof(varray))
                let data_ptr = unsafe {
                    self.builder.build_gep(
                        i8_type,
                        arr,
                        &[self
                            .context
                            .i64_type()
                            .const_int(self.target_abi.varray_data_offset(), false)],
                        "setarr_data",
                    )?
                };

                // Element size from the source register's
                // kind, via the table `crate::layout` shares with the Cranelift
                // tier so the two cannot index an array differently.
                let src_type_idx = lowering.regs[src.idx()].0;
                let src_kind = self.types_[src_type_idx].kind;
                let elem_size = crate::layout::array_elem_size_for(
                    src_kind,
                    self.target_abi.pointer_bytes() as i32,
                ) as u64;

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
        }
        Ok(())
    }

    /// `dst = cell`: copy the cell slot into the value slot at the cell's
    /// LLVM type. AIR types the value the same as the cell, so no cast.
    pub(super) fn emit_air_cell_get(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        dst: ValueId,
        cell: CellId,
    ) -> Result<()> {
        let src = cell_base + cell.idx();
        let src_val = self
            .builder
            .build_load(reg_types[src], registers[src], "src_val")?;
        self.builder.build_store(registers[dst.idx()], src_val);
        Ok(())
    }

    /// `cell = src`: copy the value slot into the cell slot, no cast.
    pub(super) fn emit_air_cell_set(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        cell: CellId,
        src: ValueId,
    ) -> Result<()> {
        let dst = cell_base + cell.idx();
        let src_val =
            self.builder
                .build_load(reg_types[src.idx()], registers[src.idx()], "src_val")?;
        self.builder.build_store(registers[dst], src_val);
        Ok(())
    }

    /// `cell += 1` in place, at the cell slot's integer width.
    pub(super) fn emit_air_cell_incr(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        cell: CellId,
    ) -> Result<()> {
        let dst = cell_base + cell.idx();
        let val = self
            .builder
            .build_load(reg_types[dst], registers[dst], "incr_val")?
            .into_int_value();
        let one = val.get_type().const_int(1, false);
        let result = self.builder.build_int_add(val, one, "incr")?;
        self.builder.build_store(registers[dst], result)?;
        Ok(())
    }

    /// `cell -= 1` in place, at the cell slot's integer width.
    pub(super) fn emit_air_cell_decr(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        cell: CellId,
    ) -> Result<()> {
        let dst = cell_base + cell.idx();
        let val = self
            .builder
            .build_load(reg_types[dst], registers[dst], "decr_val")?
            .into_int_value();
        let one = val.get_type().const_int(1, false);
        let result = self.builder.build_int_sub(val, one, "decr")?;
        self.builder.build_store(registers[dst], result)?;
        Ok(())
    }

    /// `dst = &cell`: the cell alloca's address. This is what keeps the cell
    /// addressable through mem2reg.
    pub(super) fn emit_air_cell_ref(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        dst: ValueId,
        cell: CellId,
    ) -> Result<()> {
        // dst = &src (pointer to the register's alloca)
        self.builder
            .build_store(registers[dst.idx()], registers[cell_base + cell.idx()])?;
        Ok(())
    }

    /// `dst = src` between two value slots of the same type.
    pub(super) fn emit_air_copy(
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
                .build_load(reg_types[src.idx()], registers[src.idx()], "src_val")?;
        self.builder.build_store(registers[dst.idx()], src_val);
        Ok(())
    }

    /// `dst = *src`, null-guarded.
    pub(super) fn emit_air_unref(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        dst: ValueId,
        src: ValueId,
    ) -> Result<()> {
        let ptr = self
            .builder
            .build_load(reg_types[src.idx()], registers[src.idx()], "unref_ptr")?
            .into_pointer_value();
        // Dereferencing no reference yields null, as the interpreter
        // answers, rather than faulting.
        let (_, load_block, cont_block) = self.null_guard("unref", ptr)?;
        self.builder
            .build_store(registers[dst.idx()], reg_types[dst.idx()].const_zero())?;
        self.builder.build_unconditional_branch(cont_block)?;
        self.builder.position_at_end(load_block);
        let val = self
            .builder
            .build_load(reg_types[dst.idx()], ptr, "unref_val")?;
        self.builder.build_store(registers[dst.idx()], val)?;
        self.builder.build_unconditional_branch(cont_block)?;
        self.builder.position_at_end(cont_block);
        Ok(())
    }

    /// `*r = value`, null-guarded.
    pub(super) fn emit_air_set_ref(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        r: ValueId,
        value: ValueId,
    ) -> Result<()> {
        let ptr = self
            .builder
            .build_load(reg_types[r.idx()], registers[r.idx()], "setref_ptr")?
            .into_pointer_value();
        // Storing through no reference does nothing, as in the
        // interpreter -- the null arm falls straight through.
        let (_, load_block, cont_block) = self.null_guard("setref", ptr)?;
        self.builder.build_unconditional_branch(cont_block)?;
        self.builder.position_at_end(load_block);
        let val = self.builder.build_load(
            reg_types[value.idx()],
            registers[value.idx()],
            "setref_val",
        )?;
        self.builder.build_store(ptr, val)?;
        self.builder.build_unconditional_branch(cont_block)?;
        self.builder.position_at_end(cont_block);
        Ok(())
    }

    /// `dst = &src->data`: the address of an array's first element.
    pub(super) fn emit_air_ref_data(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        dst: ValueId,
        src: ValueId,
    ) -> Result<()> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let obj = self
            .builder
            .build_load(ptr_type, registers[src.idx()], "refdata_src")?
            .into_pointer_value();
        // ORefData is the address of the first element, past the whole
        // varray header {t, at, size, pad}; the element type descriptor
        // sits inside that header.
        let header = self.target_abi.varray_data_offset();
        let data_gep = unsafe {
            self.builder.build_gep(
                self.context.i8_type(),
                obj,
                &[self.context.i64_type().const_int(header, false)],
                "refdata_gep",
            )?
        };
        let _ = ptr_type;
        self.builder.build_store(registers[dst.idx()], data_gep)?;
        Ok(())
    }

    /// `dst = base + offset * size_of(param)`: the offset counts elements of
    /// the destination ref's parameter type, as HashLink's
    /// `hl_type_size(dst->t->tparam)` does.
    pub(super) fn emit_air_ref_offset(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        dst: ValueId,
        base: ValueId,
        offset: ValueId,
    ) -> Result<()> {
        let dst_ty = lowering.regs[dst.idx()].0;
        let href = &self.types_[dst_ty];
        if href.kind != crate::hl::hl_type_kind_HREF {
            return Err(anyhow!(
                "RefOffset destination type kind {} is not HREF",
                href.kind
            ));
        }
        let inner = href
            .tparam
            .as_ref()
            .ok_or_else(|| anyhow!("RefOffset HREF type {dst_ty} has no parameter"))?;
        let stride = crate::layout::array_elem_size_for(
            self.types_[inner.0].kind,
            self.target_abi.pointer_bytes() as i32,
        );
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let base = self
            .builder
            .build_load(ptr_type, registers[base.idx()], "refoff_base")?
            .into_pointer_value();
        // Scaled at pointer width, so a large index does not wrap in i32.
        let i32t = self.context.i32_type();
        let i64t = self.context.i64_type();
        let index = self
            .builder
            .build_load(i32t, registers[offset.idx()], "refoff_off")?
            .into_int_value();
        let index = self
            .builder
            .build_int_s_extend(index, i64t, "refoff_idx")?;
        let off = if stride == 1 {
            index
        } else {
            self.builder.build_int_mul(
                index,
                i64t.const_int(stride as u64, false),
                "refoff_scaled",
            )?
        };
        let result = unsafe {
            self.builder
                .build_gep(self.context.i8_type(), base, &[off], "refoff_result")?
        };
        self.builder.build_store(registers[dst.idx()], result)?;
        Ok(())
    }

    /// `dst = &bytes_pool[idx]`; null when the index names no constant.
    pub(super) fn emit_air_bytes(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        dst: ValueId,
        idx: usize,
    ) -> Result<()> {
        if let Some(bytes_global) = self.ensure_bytes_global(idx) {
            self.builder
                .build_store(registers[dst.idx()], bytes_global.as_pointer_value())?;
        } else {
            let null_ptr = self.context.ptr_type(AddressSpace::default()).const_null();
            self.builder.build_store(registers[dst.idx()], null_ptr)?;
        }
        Ok(())
    }

    /// Throw "assert", catchably.
    ///
    /// Upstream OAssert calls hl_assert() -> hl_error("assert"). The unit
    /// suite EXECUTES this instruction on purpose (assert-testing cases), so
    /// the `unreachable` relies on hlp_error longjmp'ing to the active trap.
    pub(super) fn emit_air_assert(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
    ) -> Result<()> {
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
        Ok(())
    }

    /// Cache hint on `value` via inline asm chosen for the host
    /// architecture; a no-op elsewhere.
    pub(super) fn emit_air_prefetch(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        value: ValueId,
        field: usize,
        mode: i32,
    ) -> Result<()> {
        let _ = field; // field offset elision is safe; prefetch is purely a hint
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let base = self
            .builder
            .build_load(ptr_type, registers[value.idx()], "prefetch_ptr")?
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
            self.builder
                .build_indirect_call(fn_type, asm_val, &[base.into()], "prefetch")?;
        }
        Ok(())
    }

    /// Inline assembly byte emission.
    ///
    /// HashLink OAsm modes:
    ///   0 → emit raw byte (p2) into code stream
    ///   1 → mark physical register (p2) as clobbered
    ///   2 → load VM register into physical register (p2)
    ///   3 → store physical register (p2) into VM register
    ///   4 → naked function (strip prologue; must be first opcode)
    ///
    /// Modes 1-3 are register-allocator directives for HashLink's custom JIT;
    /// LLVM handles register allocation automatically so these are no-ops.
    /// Mode 0 emits raw bytes via `.byte` — works on all LLVM targets.
    pub(super) fn emit_air_asm(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        mode: i32,
        value: i32,
        reg: u32,
    ) -> Result<()> {
        let _ = reg;
        match mode {
            0 => {
                let byte = value as u8;
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
        Ok(())
    }

    /// Pop the innermost trap context. The exception cell and Haxe's flag
    /// operand are not read: the runtime tracks the trap stack itself.
    pub(super) fn emit_air_end_trap(
        &mut self,
        lowering: &HLFunction,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
        cell_base: usize,
        cell: CellId,
        flag: bool,
    ) -> Result<()> {
        let remove = self.declare_native("hlp_remove_trap_jit", &[], None);
        self.builder.build_call(remove, &[], "")?;
        Ok(())
    }
}
