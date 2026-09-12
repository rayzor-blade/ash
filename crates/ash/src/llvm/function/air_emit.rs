//! Lowering AIR v2 to LLVM: register allocation, the CFG, phi edges,
//! terminators, conditions, arithmetic, constants, vectors and the FMA
//! peephole. Every other instruction is emitted by the per-family sibling
//! modules (`calls`, `objects`, `casts`, `enums`, `memory`), which the
//! instruction match here dispatches to.

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

use crate::hl::{
    hl_obj_field, hl_runtime_obj, hl_type, hl_type_kind_HABSTRACT, hl_type_kind_HBOOL,
    hl_type_kind_HBYTES, hl_type_kind_HDYN, hl_type_kind_HDYNOBJ, hl_type_kind_HF32,
    hl_type_kind_HF64, hl_type_kind_HI32, hl_type_kind_HI64, hl_type_kind_HNULL, hl_type_kind_HOBJ,
    hl_type_kind_HSTRUCT, hl_type_kind_HTYPE, hl_type_kind_HUI16, hl_type_kind_HUI8,
    hl_type_kind_HVIRTUAL, hl_type_kind_HVOID, vdynamic, vdynobj, vvirtual,
};
use crate::llvm::module::{CompiledFunctionMeta, JITModule};
use crate::types::{HLNative, HLTypeFun, Str, TypeRef};
use crate::{
    hl::{hl_type_kind_HFUN, hl_type_kind_HMETHOD},
    types::HLFunction,
};
use anyhow::{anyhow, Result};

use super::{fiber_polls_enabled, hl_hash_utf8, native_traps_enabled, FuncPtr};

impl<'ctx> JITModule<'ctx> {
    /// Lower a verified AIR v2 function directly to LLVM.
    ///
    /// Values and pinned cells get distinct stack slots. LLVM's mem2reg pass
    /// promotes the SSA value slots; cells deliberately remain addressable.
    /// AIR blocks, phi edges and terminators drive the CFG, and every
    /// instruction is emitted from its own fields by the per-family emitters
    /// in the sibling modules; nothing here rebuilds a HashLink opcode. The
    /// `lowering` table is the source function with its register types
    /// replaced by one type per AIR value and cell, which is what the
    /// emitters index.
    pub(super) fn translate_air_v2(
        &mut self,
        source: &HLFunction,
        air: &AirFunction,
        function: FunctionValue<'ctx>,
    ) -> Result<()> {
        self.current_findex = source.findex as usize;
        let lowering = Self::air_lowering_table(source, air);
        let (registers, reg_types) = self.allocate_air_registers(air, &lowering)?;
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

        // The frame opens once the registers are seeded and before any block
        // runs; each `Ret` closes it. Throws need nothing here: the runtime
        // records the depth when a trap is armed and unwinds to it, the way
        // it restores the GC lock depth.
        self.shadow_slot = if self.shadow_frames() {
            Some(self.emit_shadow_push(source.findex as usize)?)
        } else {
            None
        };

        let included = vec![true; air.blocks.len()];
        self.emit_air_v2_cfg(
            source,
            air,
            function,
            &lowering,
            &registers,
            &reg_types,
            cell_base,
            &included,
            AirBlockId(0),
        )?;
        self.shadow_slot = None;
        self.audit_register_stores(source.findex as usize, function, &registers, &reg_types);
        Ok(())
    }

    /// The source function with its register table replaced by one HL type
    /// per AIR value followed by one per cell: what every emitter indexes a
    /// value's or a cell's type by. Its opcodes are dropped; nothing reads
    /// them.
    pub(super) fn air_lowering_table(source: &HLFunction, air: &AirFunction) -> HLFunction {
        let mut lowering = source.clone();
        lowering.regs = air
            .values
            .iter()
            .map(|v| TypeRef(v.ty.0 as usize))
            .chain(air.cells.iter().map(|c| TypeRef(c.ty.0 as usize)))
            .collect();
        lowering.ops.clear();
        lowering
    }

    /// One stack slot per AIR value and per cell.
    ///
    /// A slot is typed by the value's HL type, except that a widened value
    /// gets a vector of its lane count: the bytecode's type table has no
    /// vector entries, so the lane count on the value is the only place that
    /// width exists. Constants a pass minted have no pool entry either, so
    /// their globals are materialised first; after that `ensure_int_global`
    /// cannot tell them from pooled constants. Both entry points -- a whole
    /// function and an OSR body -- allocate here, so a widened function
    /// lowers the same way through either.
    pub(super) fn allocate_air_registers(
        &mut self,
        air: &AirFunction,
        lowering: &HLFunction,
    ) -> Result<(Vec<PointerValue<'ctx>>, Vec<BasicTypeEnum<'ctx>>)> {
        // A minted constant's index is function-local (the pool size plus
        // its position in this function's list), and `int_globals` is the
        // module's, so the slot is overwritten for every function rather
        // than kept: the previous function's global stays referenced by
        // its own body, and this one's body reads the slot next.
        for (i, v) in air.pending_ints.iter().enumerate() {
            let idx = air.int_pool_base + i;
            let g = self.module.add_global(
                self.context.i32_type(),
                None,
                &format!("Int_{idx}_f{}", lowering.findex),
            );
            g.set_initializer(&self.context.i32_type().const_int(*v as u64, true));
            g.set_constant(true);
            if self.int_globals.len() <= idx {
                self.int_globals.resize(idx + 1, None);
            }
            self.int_globals[idx] = Some(g);
        }

        let mut ptrs = Vec::with_capacity(lowering.regs.len());
        let mut types = Vec::with_capacity(lowering.regs.len());
        for (i, reg) in lowering.regs.iter().enumerate() {
            let scalar = self.get_register_type(reg.0)?;
            let lanes = air.values.get(i).map_or(1, |v| v.lanes);
            let (ty, name) = if lanes >= 2 {
                let vec_ty: BasicTypeEnum = match scalar {
                    BasicTypeEnum::IntType(t) => t.vec_type(lanes as u32).into(),
                    BasicTypeEnum::FloatType(t) => t.vec_type(lanes as u32).into(),
                    other => {
                        return Err(anyhow!(
                            "AIR value v{i} is {lanes} lanes of a non-scalar type {other:?}"
                        ))
                    }
                };
                (vec_ty, format!("vreg_{i}"))
            } else {
                (scalar, format!("reg_{i}"))
            };
            types.push(ty);
            ptrs.push(self.builder.build_alloca(ty, &name)?);
        }
        if self.target_abi.pointer_registers_in_memory {
            for (slot, ty) in types.iter().enumerate() {
                if ty.is_pointer_type() {
                    self.pin_register(ptrs[slot])?;
                }
            }
        }
        Ok((ptrs, types))
    }

    /// Report any store into a register slot whose value is the wrong width.
    ///
    /// LLVM cannot catch this. With opaque pointers `store double, ptr %reg`
    /// is valid IR whatever `%reg` was allocated as, so an 8-byte value going
    /// into a 4-byte HF32 slot verifies clean, silently reads back as the low
    /// half of the double, and writes four bytes past the end of the slot into
    /// whatever register LLVM placed next. That shipped as `ToSFloat` storing
    /// its always-f64 result into an f32 register, and it reached a game as
    /// `alSourcef(AL_GAIN, <junk>)` plus a clobbered neighbour.
    ///
    /// There are ~70 places that store into a register, so this checks the
    /// emitted IR rather than trusting each of them. `ASH_CHECK_REG_STORES=1`.
    fn audit_register_stores(
        &self,
        findex: usize,
        function: FunctionValue<'ctx>,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
    ) {
        if std::env::var_os("ASH_CHECK_REG_STORES").is_none() {
            return;
        }
        use inkwell::values::InstructionOpcode;
        for block in function.get_basic_blocks() {
            let mut inst = block.get_first_instruction();
            while let Some(i) = inst {
                inst = i.get_next_instruction();
                if i.get_opcode() != InstructionOpcode::Store {
                    continue;
                }
                let (Some(val), Some(dest)) = (i.get_operand(0), i.get_operand(1)) else {
                    continue;
                };
                use inkwell::values::Operand;
                let (Operand::Value(val), Operand::Value(dest)) = (val, dest) else {
                    continue;
                };
                if !dest.is_pointer_value() {
                    continue;
                }
                let dest = dest.into_pointer_value();
                let Some(slot) = registers.iter().position(|r| *r == dest) else {
                    continue;
                };
                let got = val.get_type();
                let want = reg_types[slot];
                if got == want {
                    continue;
                }
                // A mismatch only matters two ways. Either the store is WIDER
                // than the slot, which writes past it into whatever register
                // LLVM placed next; or something later loads the slot at its
                // declared type and gets a reinterpreted value. A slot that is
                // consistently written and read as one wrong type is only bad
                // bookkeeping in `reg_types`, and says nothing about codegen.
                let size = |t: BasicTypeEnum<'ctx>| -> u32 {
                    match t {
                        BasicTypeEnum::IntType(i) => i.get_bit_width().div_ceil(8),
                        BasicTypeEnum::FloatType(f) => {
                            if f == self.context.f32_type() {
                                4
                            } else {
                                8
                            }
                        }
                        _ => 8,
                    }
                };
                let overruns = size(got) > size(want);
                let mut read_as_declared = false;
                for b2 in function.get_basic_blocks() {
                    let mut it = b2.get_first_instruction();
                    while let Some(l) = it {
                        it = l.get_next_instruction();
                        if l.get_opcode() != InstructionOpcode::Load {
                            continue;
                        }
                        if let Some(Operand::Value(src)) = l.get_operand(0) {
                            if src.is_pointer_value()
                                && src.into_pointer_value() == dest
                                && l.get_type() != got.as_any_type_enum()
                            {
                                read_as_declared = true;
                            }
                        }
                    }
                }
                if overruns || read_as_declared {
                    eprintln!(
                        "[regstore] findex={findex} r{slot}: storing {got} into a {want} slot\
                         {}{}",
                        if overruns { " OVERRUN" } else { "" },
                        if read_as_declared { " READBACK" } else { "" },
                    );
                }
            }
        }
    }

    /// Emit the selected AIR CFG region, starting at `entry_target`.
    ///
    /// Ordinary functions select every block and start at b0. OSR entries
    /// select only blocks reachable from a loop header, after their entry
    /// block has restored the de-SSA register image.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn emit_air_v2_cfg(
        &mut self,
        source: &HLFunction,
        air: &AirFunction,
        function: FunctionValue<'ctx>,
        lowering: &HLFunction,
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
        if fiber_polls_enabled() {
            for lp in &loops.loops {
                if included.get(lp.header.idx()).copied().unwrap_or(false) {
                    poll_headers[lp.header.idx()] = true;
                }
            }
        }
        // `hlp_fiber_poll` calls `gc_safepoint`, so a loop whose header has no
        // poll never reaches one: the collector cannot stop the world, and a
        // fiber spinning in that loop never yields. `ASH_FIBER_POLLS=0` emits
        // none, and `ASH_POLL_MEMORY` relaxes what the helper may write; both
        // are for measuring what a poll costs, neither is sound to run with.
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
            if let Some(effects) = super::poll_memory_effects() {
                f.add_attribute(
                    inkwell::attributes::AttributeLoc::Function,
                    self.context.create_enum_attribute(
                        inkwell::attributes::Attribute::get_named_enum_kind_id("memory"),
                        effects,
                    ),
                );
            }
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
                        // old value: read `src`, write `dst`. The in-place
                        // form is `CellIncr`/`CellDecr`, over a cell slot.
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
                    // Arithmetic and bitwise, translated from AIR. All
                    // thirteen: `can_trap` marks the ops GVN may not reuse, not
                    // ops needing emitted trap code -- division lowers to a
                    // plain sdiv here as it does everywhere else.
                    AirInstr::BinOp { op, dst, a, b } => {
                        let av = self.builder.build_load(
                            reg_types[a.idx()],
                            registers[a.idx()],
                            "air_a",
                        )?;
                        let bv = self.builder.build_load(
                            reg_types[b.idx()],
                            registers[b.idx()],
                            "air_b",
                        )?;
                        let out: BasicValueEnum = match (av, bv) {
                            (BasicValueEnum::IntValue(x), BasicValueEnum::IntValue(y)) => {
                                let bd = &self.builder;
                                match op {
                                    AirBinOp::Add => bd.build_int_add(x, y, "air_add")?,
                                    AirBinOp::Sub => bd.build_int_sub(x, y, "air_sub")?,
                                    AirBinOp::Mul => bd.build_int_mul(x, y, "air_mul")?,
                                    // A shift count of a different width from
                                    // the value, or at or past that width, is
                                    // ordinary HL. `shift_operands` fits it and
                                    // masks it, because an LLVM shift is poison
                                    // from the value's width up.
                                    AirBinOp::Shl => {
                                        let (x, y) = Self::shift_operands(bd, x, y)?;
                                        bd.build_left_shift(x, y, "air_shl")?
                                    }
                                    AirBinOp::SShr => {
                                        let (x, y) = Self::shift_operands(bd, x, y)?;
                                        bd.build_right_shift(x, y, true, "air_sshr")?
                                    }
                                    AirBinOp::UShr => {
                                        let (x, y) = Self::shift_operands(bd, x, y)?;
                                        bd.build_right_shift(x, y, false, "air_ushr")?
                                    }
                                    AirBinOp::And => bd.build_and(x, y, "air_and")?,
                                    AirBinOp::Or => bd.build_or(x, y, "air_or")?,
                                    AirBinOp::Xor => bd.build_xor(x, y, "air_xor")?,
                                    AirBinOp::SDiv => bd.build_int_signed_div(x, y, "air_sdiv")?,
                                    AirBinOp::UDiv => bd.build_int_unsigned_div(x, y, "air_udiv")?,
                                    AirBinOp::SMod => bd.build_int_signed_rem(x, y, "air_smod")?,
                                    AirBinOp::UMod => bd.build_int_unsigned_rem(x, y, "air_umod")?,
                                }
                                .into()
                            }
                            (BasicValueEnum::FloatValue(x), BasicValueEnum::FloatValue(y)) => {
                                let fv = match op {
                                    AirBinOp::Add => {
                                        self.builder.build_float_add(x, y, "air_fadd")?
                                    }
                                    AirBinOp::Sub => {
                                        self.builder.build_float_sub(x, y, "air_fsub")?
                                    }
                                    AirBinOp::Mul => {
                                        self.builder.build_float_mul(x, y, "air_fmul")?
                                    }
                                    AirBinOp::SDiv => {
                                        self.builder.build_float_div(x, y, "air_fdiv")?
                                    }
                                    AirBinOp::SMod => {
                                        self.builder.build_float_rem(x, y, "air_frem")?
                                    }
                                    _ => return Err(anyhow!("AIR BinOp {op:?} on floats")),
                                };
                                // `contract`, so the FMA peephole's pairs can
                                // still fuse -- the same flag the opcode path
                                // sets on these three.
                                if let Some(inst) = fv.as_instruction() {
                                    inst.set_fast_math_flags(1 << 5);
                                }
                                fv.into()
                            }
                            _ => {
                                return Err(anyhow!("AIR BinOp {op:?} on mismatched operand types"))
                            }
                        };
                        self.builder.build_store(registers[dst.idx()], out)?;
                    }
                    // Constants, translated from AIR rather than rebuilt as
                    // HL opcodes first. AIR is the IR the backend consumes;
                    // reconstructing bytecode to reach the same emitter is
                    // work the compiler should not be doing, and it is where
                    // a decision made twice drifts apart.
                    AirInstr::Int { dst, idx } => {
                        let global = self
                            .ensure_int_global(*idx)
                            .ok_or_else(|| anyhow!("AIR Int names no constant: {idx}"))?;
                        let v = self.builder.build_load(
                            self.context.i32_type(),
                            global.as_pointer_value(),
                            "air_int",
                        )?;
                        let v = self.cast_for_call(v, reg_types[dst.idx()])?;
                        self.builder.build_store(registers[dst.idx()], v)?;
                    }
                    AirInstr::Float { dst, idx } => {
                        let global = self
                            .ensure_float_global(*idx)
                            .ok_or_else(|| anyhow!("AIR Float names no constant: {idx}"))?;
                        let v = self.builder.build_load(
                            self.context.f64_type(),
                            global.as_pointer_value(),
                            "air_float",
                        )?;
                        // The pool is f64; an HF32 destination is a 4-byte slot.
                        self.store_float_as_reg(
                            &registers,
                            &reg_types,
                            dst.idx(),
                            v.into_float_value(),
                        )?;
                    }
                    AirInstr::Bool { dst, value } => {
                        let v = self.context.bool_type().const_int(*value as u64, false);
                        self.builder.build_store(registers[dst.idx()], v)?;
                    }
                    AirInstr::Null { dst } => {
                        let v = self.context.ptr_type(AddressSpace::default()).const_null();
                        self.builder.build_store(registers[dst.idx()], v)?;
                    }
                    AirInstr::String { dst, idx } => {
                        // The register holds the ADDRESS of the constant.
                        let g = self
                            .ensure_string_global(*idx)
                            .ok_or_else(|| anyhow!("AIR String: no constant {idx}"))?;
                        self.builder
                            .build_store(registers[dst.idx()], g.as_pointer_value())?;
                    }
                    AirInstr::TypeConst { dst, ty } => {
                        let v = self.get_initialized_type(ty.0 as usize)?;
                        self.builder.build_store(registers[dst.idx()], v)?;
                    }
                    AirInstr::GetGlobal { dst, global } => {
                        let slot = *self
                            .globals
                            .get(global)
                            .ok_or_else(|| anyhow!("AIR GetGlobal: no global {global}"))?;
                        // Every global is a pointer-sized slot.
                        let ptr_type = self.context.ptr_type(AddressSpace::default());
                        let v = self.builder.build_load(ptr_type, slot, "air_global")?;
                        self.builder.build_store(registers[dst.idx()], v)?;
                    }
                    AirInstr::SetGlobal { global, src } => {
                        let slot = *self
                            .globals
                            .get(global)
                            .ok_or_else(|| anyhow!("AIR SetGlobal: no global {global}"))?;
                        let v = self.builder.build_load(
                            reg_types[src.idx()],
                            registers[src.idx()],
                            "air_global_src",
                        )?;
                        self.builder.build_store(slot, v)?;
                    }
                    AirInstr::Fma { dst, a, b, c } => {
                        self.emit_air_fma(*dst, *a, *b, *c, &registers, &reg_types)?;
                    }
                    AirInstr::VecLoad { .. }
                    | AirInstr::VecStore { .. }
                    | AirInstr::VecSplat { .. }
                    | AirInstr::VecBinOp { .. }
                    | AirInstr::VecReduce { .. } => {
                        self.emit_air_vector(instr, &registers, &reg_types)?;
                    }
                    AirInstr::FieldGet { dst, obj, obj_ty, field } => {
                        self.emit_air_field_get(
                            lowering, registers, reg_types, cell_base, *dst, *obj, *obj_ty, *field,
                        )?;
                    }
                    AirInstr::FieldSet { obj, obj_ty, field, src } => {
                        self.emit_air_field_set(
                            lowering, registers, reg_types, cell_base, *obj, *obj_ty, *field, *src,
                        )?;
                    }
                    AirInstr::DynGet { dst, obj, field } => {
                        self.emit_air_dyn_get(lowering, registers, reg_types, cell_base, *dst, *obj, *field)?;
                    }
                    AirInstr::DynSet { obj, field, src } => {
                        self.emit_air_dyn_set(lowering, registers, reg_types, cell_base, *obj, *field, *src)?;
                    }
                    AirInstr::New { dst } => {
                        self.emit_air_new(lowering, registers, reg_types, cell_base, *dst)?;
                    }
                    AirInstr::NullCheck { value } => {
                        self.emit_air_null_check(lowering, registers, reg_types, cell_base, *value, next)?;
                    }
                    AirInstr::GetType { dst, src } => {
                        self.emit_air_get_type(lowering, registers, reg_types, cell_base, *dst, *src)?;
                    }
                    AirInstr::GetTID { dst, src } => {
                        self.emit_air_get_tid(lowering, registers, reg_types, cell_base, *dst, *src)?;
                    }
                    AirInstr::ArraySize { dst, array } => {
                        self.emit_air_array_size(lowering, registers, reg_types, cell_base, *dst, *array)?;
                    }
                    AirInstr::Call { dst, fun, args } => {
                        self.emit_air_call(lowering, registers, reg_types, cell_base, *dst, *fun, args)?;
                    }
                    AirInstr::Intrinsic { kind, fun, dst, args } => {
                        self.emit_air_intrinsic(lowering, registers, reg_types, cell_base, *kind, *fun, *dst, args)?;
                    }
                    AirInstr::CallMethod { dst, field, args } => {
                        self.emit_air_call_method(lowering, registers, reg_types, cell_base, *dst, *field, args)?;
                    }
                    AirInstr::CallClosure { dst, fun, args } => {
                        self.emit_air_call_closure(lowering, registers, reg_types, cell_base, *dst, *fun, args)?;
                    }
                    AirInstr::StaticClosure { dst, fun } => {
                        self.emit_air_static_closure(lowering, registers, reg_types, cell_base, *dst, *fun)?;
                    }
                    AirInstr::InstanceClosure { dst, fun, obj } => {
                        self.emit_air_instance_closure(lowering, registers, reg_types, cell_base, *dst, *fun, *obj)?;
                    }
                    AirInstr::VirtualClosure { dst, obj, field } => {
                        self.emit_air_virtual_closure(lowering, registers, reg_types, cell_base, *dst, *obj, *field)?;
                    }
                    AirInstr::Cast { kind, dst, src } => {
                        self.emit_air_cast(lowering, registers, reg_types, cell_base, *kind, *dst, *src)?;
                    }
                    // After the guarded Incr/Decr arm above: this one refuses those.
                    AirInstr::UnOp { op, dst, src } => {
                        self.emit_air_un_op(lowering, registers, reg_types, cell_base, *op, *dst, *src)?;
                    }
                    AirInstr::MemGet { kind, dst, base, index } => {
                        self.emit_air_mem_get(lowering, registers, reg_types, cell_base, *kind, *dst, *base, *index)?;
                    }
                    AirInstr::MemSet { kind, base, index, src } => {
                        self.emit_air_mem_set(lowering, registers, reg_types, cell_base, *kind, *base, *index, *src)?;
                    }
                    AirInstr::CellGet { dst, cell } => {
                        self.emit_air_cell_get(lowering, registers, reg_types, cell_base, *dst, *cell)?;
                    }
                    AirInstr::CellSet { cell, src } => {
                        self.emit_air_cell_set(lowering, registers, reg_types, cell_base, *cell, *src)?;
                    }
                    AirInstr::CellIncr { cell } => {
                        self.emit_air_cell_incr(lowering, registers, reg_types, cell_base, *cell)?;
                    }
                    AirInstr::CellDecr { cell } => {
                        self.emit_air_cell_decr(lowering, registers, reg_types, cell_base, *cell)?;
                    }
                    AirInstr::CellRef { dst, cell } => {
                        self.emit_air_cell_ref(lowering, registers, reg_types, cell_base, *dst, *cell)?;
                    }
                    AirInstr::Copy { dst, src } => {
                        self.emit_air_copy(lowering, registers, reg_types, cell_base, *dst, *src)?;
                    }
                    AirInstr::Unref { dst, src } => {
                        self.emit_air_unref(lowering, registers, reg_types, cell_base, *dst, *src)?;
                    }
                    AirInstr::SetRef { r, value } => {
                        self.emit_air_set_ref(lowering, registers, reg_types, cell_base, *r, *value)?;
                    }
                    AirInstr::RefData { dst, src } => {
                        self.emit_air_ref_data(lowering, registers, reg_types, cell_base, *dst, *src)?;
                    }
                    AirInstr::RefOffset { dst, base, offset } => {
                        self.emit_air_ref_offset(lowering, registers, reg_types, cell_base, *dst, *base, *offset)?;
                    }
                    AirInstr::Bytes { dst, idx } => {
                        self.emit_air_bytes(lowering, registers, reg_types, cell_base, *dst, *idx)?;
                    }
                    AirInstr::Assert => {
                        self.emit_air_assert(lowering, registers, reg_types, cell_base)?;
                    }
                    AirInstr::Prefetch { value, field, mode } => {
                        self.emit_air_prefetch(lowering, registers, reg_types, cell_base, *value, *field, *mode)?;
                    }
                    AirInstr::Asm { mode, value, reg } => {
                        self.emit_air_asm(lowering, registers, reg_types, cell_base, *mode, *value, *reg)?;
                    }
                    AirInstr::EndTrap { cell, flag } => {
                        self.emit_air_end_trap(lowering, registers, reg_types, cell_base, *cell, *flag)?;
                    }
                    AirInstr::EnumAlloc { dst, construct } => {
                        self.emit_air_enum_alloc(lowering, registers, reg_types, cell_base, *dst, *construct)?;
                    }
                    AirInstr::MakeEnum { dst, construct, args } => {
                        self.emit_air_make_enum(lowering, registers, reg_types, cell_base, *dst, *construct, args)?;
                    }
                    AirInstr::EnumIndex { dst, value } => {
                        self.emit_air_enum_index(lowering, registers, reg_types, cell_base, *dst, *value)?;
                    }
                    AirInstr::EnumField { dst, value, construct, field } => {
                        self.emit_air_enum_field(
                            lowering, registers, reg_types, cell_base, *dst, *value, *construct, *field,
                        )?;
                    }
                    AirInstr::SetEnumField { value, construct, field, src } => {
                        self.emit_air_set_enum_field(
                            lowering, registers, reg_types, cell_base, *value, *construct, *field, *src,
                        )?;
                    }
                    AirInstr::Pos { file, line } => {
                        // The frame's position, as the runtime reads it back:
                        // `(file << 32) | line`. Only a shadow-stack body has a
                        // slot; lowering emits the marker for no other target.
                        if let Some(slot) = self.shadow_slot {
                            let pos = (u64::from(*file) << 32) | u64::from(*line);
                            self.builder
                                .build_store(slot, self.context.i64_type().const_int(pos, false))?;
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

    /// Emit one of the vector forms.
    ///
    /// Lane width comes from the destination slot, which `translate_air_v2`
    /// already re-typed from the value's lane count -- so the machine width is
    /// whatever the transform decided, and a disagreement between operands was
    /// rejected by the AIR verifier before reaching here.
    fn emit_air_vector(
        &self,
        instr: &AirInstr,
        registers: &[PointerValue<'ctx>],
        reg_types: &[BasicTypeEnum<'ctx>],
    ) -> Result<()> {
        let load = |v: ValueId, name: &str| -> Result<BasicValueEnum<'ctx>> {
            Ok(self
                .builder
                .build_load(reg_types[v.idx()], registers[v.idx()], name)?)
        };
        match instr {
            AirInstr::VecLoad {
                dst, base, index, ..
            } => {
                let base_ptr = load(*base, "vec_base")?.into_pointer_value();
                let idx = load(*index, "vec_idx")?.into_int_value();
                let addr = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), base_ptr, &[idx], "vec_addr")?
                };
                let v = self
                    .builder
                    .build_load(reg_types[dst.idx()], addr, "vec_load")?;
                self.builder.build_store(registers[dst.idx()], v)?;
            }
            AirInstr::VecStore {
                base, index, src, ..
            } => {
                let base_ptr = load(*base, "vec_base")?.into_pointer_value();
                let idx = load(*index, "vec_idx")?.into_int_value();
                let addr = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), base_ptr, &[idx], "vec_addr")?
                };
                let v = load(*src, "vec_val")?;
                self.builder.build_store(addr, v)?;
            }
            AirInstr::VecSplat { dst, src } => {
                // insertelement into lane 0, then a zero shuffle to fill.
                let scalar = load(*src, "splat_src")?;
                let BasicTypeEnum::VectorType(vec_ty) = reg_types[dst.idx()] else {
                    return Err(anyhow!("VecSplat destination is not a vector"));
                };
                let lanes = vec_ty.get_size();
                let zero = self.context.i32_type().const_zero();
                let one = self.builder.build_insert_element(
                    vec_ty.get_undef(),
                    scalar,
                    zero,
                    "splat_ins",
                )?;
                let mask = self.context.i32_type().vec_type(lanes).const_zero();
                let out =
                    self.builder
                        .build_shuffle_vector(one, vec_ty.get_undef(), mask, "splat")?;
                self.builder.build_store(registers[dst.idx()], out)?;
            }
            AirInstr::VecBinOp { op, dst, a, b } => {
                let lhs = load(*a, "vec_a")?;
                let rhs = load(*b, "vec_b")?;
                let out = self.emit_vector_binop(*op, lhs, rhs)?;
                self.builder.build_store(registers[dst.idx()], out)?;
            }
            AirInstr::VecReduce { op, dst, src } => {
                // Extract-and-combine rather than the reduce intrinsics: it
                // is correct for every element type without asking whether
                // the target has a horizontal instruction, and the backend
                // pattern-matches the tree anyway.
                let v = load(*src, "red_src")?;
                let BasicTypeEnum::VectorType(vec_ty) = reg_types[src.idx()] else {
                    return Err(anyhow!("VecReduce source is not a vector"));
                };
                let lanes = vec_ty.get_size();
                if lanes == 0 {
                    return Err(anyhow!("VecReduce over an empty vector"));
                }
                let i32t = self.context.i32_type();
                let mut acc = self.builder.build_extract_element(
                    v.into_vector_value(),
                    i32t.const_zero(),
                    "red0",
                )?;
                for lane in 1..lanes {
                    let e = self.builder.build_extract_element(
                        v.into_vector_value(),
                        i32t.const_int(lane as u64, false),
                        "rede",
                    )?;
                    acc = self.emit_vector_binop(*op, acc, e)?;
                }
                self.builder.build_store(registers[dst.idx()], acc)?;
            }
            other => return Err(anyhow!("not a vector instruction: {other:?}")),
        }
        Ok(())
    }

    /// The elementwise operation behind `VecBinOp` and the reduce's combine.
    /// LLVM applies a scalar opcode lane-wise on vector operands, so this is
    /// the same dispatch either way.
    fn emit_vector_binop(
        &self,
        op: air::v2::ir::BinOp,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>> {
        use air::v2::ir::BinOp as B;
        let float = a.is_float_value()
            || a.is_vector_value() && b.is_vector_value() && {
                matches!(
                    a.into_vector_value().get_type().get_element_type(),
                    inkwell::types::BasicTypeEnum::FloatType(_)
                )
            };
        Ok(if float {
            let (x, y) = (a.into_float_value(), b.into_float_value());
            match op {
                B::Add => self.builder.build_float_add(x, y, "vfadd")?.into(),
                B::Sub => self.builder.build_float_sub(x, y, "vfsub")?.into(),
                B::Mul => self.builder.build_float_mul(x, y, "vfmul")?.into(),
                // HL's division is the signed integer opcode reused for
                // floats; there is no separate float variant.
                B::SDiv => self.builder.build_float_div(x, y, "vfdiv")?.into(),
                _ => return Err(anyhow!("unsupported float vector op {op:?}")),
            }
        } else {
            let (x, y) = (a.into_int_value(), b.into_int_value());
            match op {
                B::Add => self.builder.build_int_add(x, y, "viadd")?.into(),
                B::Sub => self.builder.build_int_sub(x, y, "visub")?.into(),
                B::Mul => self.builder.build_int_mul(x, y, "vimul")?.into(),
                B::And => self.builder.build_and(x, y, "viand")?.into(),
                B::Or => self.builder.build_or(x, y, "vior")?.into(),
                B::Xor => self.builder.build_xor(x, y, "vixor")?.into(),
                B::Shl => self.builder.build_left_shift(x, y, "vishl")?.into(),
                B::SShr => self.builder.build_right_shift(x, y, true, "vishr")?.into(),
                B::UShr => self.builder.build_right_shift(x, y, false, "vushr")?.into(),
                _ => return Err(anyhow!("unsupported int vector op {op:?}")),
            }
        })
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

    /// A shift count in HashLink is masked to the operand width (x86 `shl cl`
    /// and arm64 `lslv` both do; the interpreter does with `wrapping_shl`).
    /// LLVM's `shl`/`lshr`/`ashr` are POISON for a count >= the width, so
    /// `1 << 32`, `x << -1` or `Int64 << (i32 count)` constant-folded to
    /// arbitrary values whenever the optimizer could see the count. Bring the
    /// count to the value's width first (Int64 shifts carry an I32 count),
    /// then mask it.
    pub(super) fn shift_operands(
        b: &inkwell::builder::Builder<'ctx>,
        x: inkwell::values::IntValue<'ctx>,
        y: inkwell::values::IntValue<'ctx>,
    ) -> Result<(
        inkwell::values::IntValue<'ctx>,
        inkwell::values::IntValue<'ctx>,
    )> {
        let width = x.get_type().get_bit_width();
        let y = if y.get_type().get_bit_width() > width {
            b.build_int_truncate(y, x.get_type(), "shift_count")?
        } else if y.get_type().get_bit_width() < width {
            b.build_int_z_extend(y, x.get_type(), "shift_count")?
        } else {
            y
        };
        let mask = x.get_type().const_int(u64::from(width - 1), false);
        Ok((x, b.build_and(y, mask, "shift_mask")?))
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
        // The phis of one block are a PARALLEL copy: every source is the
        // value at the end of the predecessor, before any phi has written.
        // After GVN folds `prev = sl; sl = sl.next` the header reads
        // `sl = phi [.. sl.next]`, `prev = phi [.. sl]` -- one phi's source is
        // another's destination -- and a sequential store-then-load handed
        // `prev` the advanced cursor. Load every source first, then store.
        let phis = &air.blocks[to.idx()].phis;
        let mut loaded = Vec::with_capacity(phis.len());
        for phi in phis {
            let src = phi
                .incoming
                .iter()
                .find(|(b, _)| *b == from)
                .map(|(_, v)| *v)
                .ok_or_else(|| {
                    anyhow!(
                        "AIR phi in b{} has no incoming value from b{}",
                        to.idx(),
                        from.idx()
                    )
                })?;
            let value = self.builder.build_load(
                reg_types[src.idx()],
                registers[src.idx()],
                "air_phi_src",
            )?;
            let value = if value.get_type() == reg_types[phi.dst.idx()] {
                value
            } else {
                self.cast_for_call(value, reg_types[phi.dst.idx()])?
            };
            loaded.push(value);
        }
        for (phi, value) in phis.iter().zip(loaded) {
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
            // NotLt/NotGte are Haxe's inverted float tests and must jump on
            // NaN, and `nan != nan` is true.
            AirCondKind::NotLt => (IntPredicate::SGE, FloatPredicate::UGE),
            AirCondKind::NotGte => (IntPredicate::SLT, FloatPredicate::ULT),
            AirCondKind::Eq => (IntPredicate::EQ, FloatPredicate::OEQ),
            AirCondKind::NotEq => (IntPredicate::NE, FloatPredicate::UNE),
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
                    || a_kind == hl_type_kind_HVIRTUAL
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
                if self.shadow_slot.is_some() {
                    self.emit_shadow_pop()?;
                }
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
                // A rethrow keeps the exception's captured stack; a throw
                // captures a new one. The runtime tells them apart by entry
                // point, as the other tiers do.
                let helper = if matches!(term, AirTerminator::Rethrow { .. }) {
                    "hlp_rethrow"
                } else {
                    "hlp_throw"
                };
                let throw = self.declare_native(helper, &[ptr_type.into()], None);
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
                let jumped = self.build_setjmp_call(buf, "air_setjmp")?;
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
                let exc_index = cell_base + exc_cell.idx();
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
}
