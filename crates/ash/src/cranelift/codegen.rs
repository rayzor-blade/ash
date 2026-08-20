//! AIR v2 → CLIF codegen.
//!
//! [`super::lower`] compiles the flat opcode array — either the bytecode's own
//! or AIR's serialization of it. This module compiles the **IR**: it walks
//! [`air::v2::ir::Function`] and composes CLIF from the typed, block-structured
//! form directly, with no opcode array in between.
//!
//! That is not a reorganization for its own sake. Three properties fall out of
//! the IR that the opcode array cannot express, and each one is a thing this
//! module does that [`super::lower`] structurally cannot:
//!
//! * **Phis are given, not reconstructed.** The opcode lowerer hands every HL
//!   register to `cranelift_frontend` as a `Variable` and lets it re-run SSA
//!   construction — work AIR already did, and did with type information the
//!   frontend does not have. Here a phi becomes a block parameter and a
//!   [`ValueId`] becomes a CLIF `Value`, one to one.
//! * **Address-taken registers become stack slots.** Because the opcode
//!   lowerer holds every register in a `Variable`, taking a register's address
//!   is impossible by construction — its own doc comment says so, and `Ref` /
//!   `Unref` / `Setref` are refused for that reason alone. AIR has already
//!   separated exactly those registers out as [`CellData`], so here they are
//!   `StackSlot`s and the three opcodes are ordinary loads and stores.
//! * **Field accesses carry their resolved type.** [`Instr::FieldGet`] holds
//!   the object type AIR resolved at lowering time, so the offset does not
//!   depend on the destination register's declared type being accurate.
//!
//! ## What is not here yet
//!
//! Exceptions. `Trap` is a terminator with a normal and a handler successor,
//! which is the shape a backend wants — but HL's runtime side is setjmp /
//! longjmp, and a longjmp back into a Cranelift frame needs the same
//! treatment the LLVM tier spells `returns_twice` plus `optnone`. Until that
//! is designed, [`reject_reason`] declines any function containing `Trap`,
//! `EndTrap`, `Throw` or `Rethrow`, exactly as the opcode gate does.
//!
//! The object model beyond fields and arrays — `New`, closures, enums, the
//! dynamic accessors — needs runtime `hl_type*` pointers and allocation
//! helpers that [`CraneliftTierContext`] does not carry. Declined for now, and
//! named individually by [`reject_reason`] so the report says which.

use anyhow::{anyhow, bail, Result};
use std::collections::HashMap;

use beadie::CraneliftFunctionDef;
use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::{
    types, AbiParam, Block, BlockArg, BlockCall, FuncRef, InstBuilder, JumpTableData, MemFlags,
    SigRef, Signature, StackSlot, StackSlotData, StackSlotKind, Type, Value,
};
use cranelift_frontend::FunctionBuilder;

use air::v2::ir::{
    BinOp, BlockId, CastKind, CellId, CondKind, Function as AirFunction, Instr, MemAccess,
    Terminator, TypeRef as AirTypeRef, UnOp, ValueId,
};

use super::backend::{AshCraneliftBackend, CraneliftTierContext};
use super::lower::LoweredFunction;
use super::{abi_class, entry_return_class, AbiClass};
use crate::hl_bindings as hl;
use crate::jit::stub_bridge::{ash_jit_call_stub, STUB_SENTINEL_LIMIT};

// ─────────────────────────────────────────────────────────────────────────────
// Pre-flight gate
// ─────────────────────────────────────────────────────────────────────────────

/// Why this tier cannot compile `f` from AIR, or `None` when it can be
/// attempted.
///
/// Side-effect free and linear in the function, so it is cheap enough to run
/// before paying for a lowering attempt — the same contract
/// [`super::lower::lowering_reject_reason`] holds to.
pub fn reject_reason(f: &AirFunction) -> Option<String> {
    for b in &f.blocks {
        for i in &b.instrs {
            if let Some(why) = instr_reject(i) {
                return Some(format!("air_instr {why}"));
            }
        }
        let why = match &b.term {
            Terminator::Ret { .. }
            | Terminator::Jump { .. }
            | Terminator::CondJump { .. }
            | Terminator::Switch { .. } => None,
            Terminator::Throw { .. } => Some("Throw"),
            Terminator::Rethrow { .. } => Some("Rethrow"),
            Terminator::Trap { .. } => Some("Trap"),
        };
        if let Some(why) = why {
            return Some(format!("air_term {why}"));
        }
    }
    None
}

/// Name of the instruction if this module cannot emit it.
///
/// Exhaustive on purpose: a new [`Instr`] variant must be classified here
/// before it can reach the emitter, so adding one to the IR fails the build
/// rather than silently miscompiling.
fn instr_reject(i: &Instr) -> Option<&'static str> {
    match i {
        Instr::Param { .. }
        | Instr::Copy { .. }
        | Instr::Int { .. }
        | Instr::Float { .. }
        | Instr::Bool { .. }
        | Instr::String { .. }
        | Instr::Null { .. }
        | Instr::BinOp { .. }
        | Instr::Fma { .. }
        | Instr::UnOp { .. }
        | Instr::Call { .. }
        | Instr::GetGlobal { .. }
        | Instr::SetGlobal { .. }
        | Instr::FieldGet { .. }
        | Instr::FieldSet { .. }
        | Instr::NullCheck { .. }
        | Instr::ArraySize { .. }
        | Instr::MemGet { .. }
        | Instr::MemSet { .. }
        | Instr::New { .. }
        | Instr::TypeConst { .. }
        | Instr::CallMethod { .. }
        | Instr::Intrinsic { .. }
        | Instr::Unref { .. }
        | Instr::SetRef { .. }
        | Instr::CellGet { .. }
        | Instr::CellSet { .. }
        | Instr::CellIncr { .. }
        | Instr::CellDecr { .. }
        | Instr::CellRef { .. } => None,

        Instr::Cast { kind, .. } => match kind {
            CastKind::ToSFloat | CastKind::ToUFloat | CastKind::ToInt | CastKind::UnsafeCast => {
                None
            }
            CastKind::ToDyn => Some("Cast::ToDyn"),
            CastKind::SafeCast => Some("Cast::SafeCast"),
            CastKind::ToVirtual => Some("Cast::ToVirtual"),
        },

        Instr::Bytes { .. } => Some("Bytes"),
        Instr::CallClosure { .. } => Some("CallClosure"),
        Instr::StaticClosure { .. } => Some("StaticClosure"),
        Instr::InstanceClosure { .. } => Some("InstanceClosure"),
        Instr::VirtualClosure { .. } => Some("VirtualClosure"),
        Instr::DynGet { .. } => Some("DynGet"),
        Instr::DynSet { .. } => Some("DynSet"),
        Instr::EndTrap { .. } => Some("EndTrap"),
        Instr::GetType { .. } => Some("GetType"),
        Instr::GetTID { .. } => Some("GetTID"),
        Instr::RefData { .. } => Some("RefData"),
        Instr::RefOffset { .. } => Some("RefOffset"),
        Instr::MakeEnum { .. } => Some("MakeEnum"),
        Instr::EnumAlloc { .. } => Some("EnumAlloc"),
        Instr::EnumIndex { .. } => Some("EnumIndex"),
        Instr::EnumField { .. } => Some("EnumField"),
        Instr::SetEnumField { .. } => Some("SetEnumField"),
        Instr::Assert => Some("Assert"),
        Instr::Prefetch { .. } => Some("Prefetch"),
        Instr::Asm { .. } => Some("Asm"),
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Entry point
// ─────────────────────────────────────────────────────────────────────────────

/// Compile `air` — the optimized IR for `findex` — to CLIF.
///
/// `tf` is the HL function type of `findex`, which fixes the entry signature;
/// AIR does not carry it, and it is not something optimization can change.
pub fn lower_air_function(
    backend: &AshCraneliftBackend,
    ctx: &CraneliftTierContext,
    findex: usize,
    air: &AirFunction,
) -> Result<LoweredFunction> {
    let bytecode = ctx.bytecode();
    let func_idx = ctx
        .func_index(findex)
        .ok_or_else(|| anyhow!("findex {findex} is not a bytecode function"))?;
    let func = &bytecode.functions[func_idx];
    let tf = bytecode.types[func.type_.0]
        .fun
        .as_ref()
        .ok_or_else(|| anyhow!("no function type"))?;

    if let Some(reason) = reject_reason(air) {
        bail!("{reason}");
    }

    let arg_kinds: Vec<u32> = tf.args.iter().map(|a| bytecode.types[a.0].kind).collect();
    let ret_kind = bytecode.types[tf.ret.0].kind;

    let mut sig = backend.make_signature();
    for a in &tf.args {
        let ty = abi_class(ctx.type_kind(a.0)?)
            .clif_type()
            .ok_or_else(|| anyhow!("void argument in entry signature"))?;
        sig.params.push(AbiParam::new(ty));
    }
    if let Some(ty) = entry_return_class(ret_kind).clif_type() {
        sig.returns.push(AbiParam::new(ty));
    }

    let name = backend.unique_name(findex, &func.name());
    let mut def = backend
        .new_def(sig, &name)
        .map_err(|e| anyhow!("declare_function({name}): {e}"))?;

    // Imports must be declared before a FunctionBuilder borrows `ctx.func`.
    // AIR recorded every native this function references at lowering time, so
    // this reads the declarations rather than re-deriving them by scanning.
    let native_refs = import_natives(backend, ctx, air, &mut def)?;

    let n_instrs: usize = air.blocks.iter().map(|b| b.instrs.len()).sum();

    {
        let mut cg = AirCodegen {
            ctx,
            f: air,
            b: def.builder(),
            vals: vec![None; air.values.len()],
            cells: Vec::new(),
            blocks: vec![None; air.blocks.len()],
            native_refs,
            nargs: tf.args.len(),
            ret_class: entry_return_class(ret_kind),
        };
        cg.run()?;
        cg.finish();
    }

    if super::lower::clif_dump_wanted(findex) {
        eprintln!(
            "=== CLIF (from AIR) findex={findex} {} ===\n{}",
            func.name(),
            def.ctx.func.display()
        );
    }

    Ok(LoweredFunction {
        def,
        arg_kinds,
        ret_kind,
        num_ops: n_instrs,
    })
}

/// Compile a Cranelift OSR entry for `findex` at serialized pc `site`.
///
/// This is what makes the ladder's middle tier reachable from a frame that
/// is already running: the LLVM entry costs a promote-sized compile
/// (~90ms), this one costs a tier-0 compile (~1ms), so the interpreter can
/// leave a hot loop within milliseconds of noticing it and tier up to LLVM
/// later through the ordinary swap.
///
/// Returns the entry address, ABI `extern "C" fn(*mut u64) -> ret` — the
/// same contract `try_osr_transfer` already speaks for LLVM entries.
pub fn compile_osr_entry(
    backend: &AshCraneliftBackend,
    ctx: &CraneliftTierContext,
    bead: &std::sync::Arc<beadie::Bead>,
    findex: usize,
    opt: &crate::air_pipeline::Optimized,
    site: usize,
) -> Result<usize> {
    let air = &opt.ir;
    let header = opt
        .ser
        .block_pcs
        .iter()
        .position(|&pc| pc == site)
        .ok_or_else(|| anyhow!("site pc {site} is not a block start"))?;

    if let Some(reason) = reject_reason(air) {
        bail!("{reason}");
    }
    // The transfer buffer holds f64 bits for float registers; an f32
    // register would need a narrowing rule nothing else exercises.
    for tr in &air.reg_types {
        if ctx.type_kind(tr.0 as usize)? == hl::hl_type_kind_HF32 {
            bail!("f32 register in an OSR frame");
        }
    }

    let bytecode = ctx.bytecode();
    let func_idx = ctx
        .func_index(findex)
        .ok_or_else(|| anyhow!("findex {findex} is not a bytecode function"))?;
    let func = &bytecode.functions[func_idx];
    let tf = bytecode.types[func.type_.0]
        .fun
        .as_ref()
        .ok_or_else(|| anyhow!("no function type"))?;
    let ret_kind = bytecode.types[tf.ret.0].kind;

    let mut sig = backend.make_signature();
    sig.params.push(AbiParam::new(types::I64)); // the transfer buffer
    if let Some(ty) = entry_return_class(ret_kind).clif_type() {
        sig.returns.push(AbiParam::new(ty));
    }
    let name = backend.unique_name(findex, &format!("osr_{site}"));
    let mut def = backend
        .new_def(sig, &name)
        .map_err(|e| anyhow!("declare_function({name}): {e}"))?;
    let native_refs = import_natives(backend, ctx, air, &mut def)?;

    {
        let mut cg = AirCodegen {
            ctx,
            f: air,
            b: def.builder(),
            vals: vec![None; air.values.len()],
            cells: Vec::new(),
            blocks: vec![None; air.blocks.len()],
            native_refs,
            nargs: 0, // parameters are dead in an OSR body; values come from buf
            ret_class: entry_return_class(ret_kind),
        };
        cg.run_osr(header)?;
        cg.finish();
    }

    if super::lower::clif_dump_wanted(findex) {
        eprintln!(
            "=== CLIF (osr entry) findex={findex} site={site} ===\n{}",
            def.ctx.func.display()
        );
    }

    let code = backend
        .compile_def(bead, def)
        .map_err(|e| anyhow!("osr entry compile: {e}"))?;
    Ok(code as usize)
}

/// Declare every native this function calls, from AIR's own forward
/// declarations. The signature is built out of [`NativeImport`]'s recorded
/// argument and return types, so nothing here re-reads the bytecode's native
/// table.
///
/// [`NativeImport`]: air::v2::module::NativeImport
fn import_natives(
    backend: &AshCraneliftBackend,
    ctx: &CraneliftTierContext,
    air: &AirFunction,
    def: &mut CraneliftFunctionDef,
) -> Result<HashMap<usize, FuncRef>> {
    let mut refs: HashMap<usize, FuncRef> = HashMap::new();
    for imp in air.natives.iter() {
        let Some(native_idx) = ctx.native_index(imp.findex) else {
            continue;
        };
        let mut sig = backend.make_signature();
        for a in &imp.args {
            let ty = abi_class(ctx.type_kind(a.0 as usize)?)
                .clif_type()
                .ok_or_else(|| anyhow!("void argument to native {}", imp.symbol()))?;
            sig.params.push(AbiParam::new(ty));
        }
        if let Some(ty) = abi_class(ctx.type_kind(imp.ret.0 as usize)?).clif_type() {
            sig.returns.push(AbiParam::new(ty));
        }
        let key = ctx
            .native_symbol_key(native_idx)
            .ok_or_else(|| anyhow!("native {} unresolved", imp.symbol()))?;
        let fref = backend
            .import_function(&key, &sig, &mut def.ctx.func)
            .map_err(|e| anyhow!("import {key}: {e}"))?;
        refs.insert(imp.findex, fref);
    }
    Ok(refs)
}

// ─────────────────────────────────────────────────────────────────────────────
// The emitter
// ─────────────────────────────────────────────────────────────────────────────

struct AirCodegen<'a, 'b> {
    ctx: &'a CraneliftTierContext,
    f: &'a AirFunction,
    b: FunctionBuilder<'b>,
    /// One CLIF value per AIR value. `None` for values of void type, which
    /// have no machine representation and are never legally used.
    vals: Vec<Option<Value>>,
    /// One stack slot per AIR cell.
    cells: Vec<StackSlot>,
    /// CLIF block per *reachable* AIR block. Unreachable AIR blocks get none;
    /// `cranelift_frontend` rejects a block that is created and never filled,
    /// and an unreachable block has nothing to fill it with.
    blocks: Vec<Option<Block>>,
    native_refs: HashMap<usize, FuncRef>,
    nargs: usize,
    ret_class: AbiClass,
}

impl AirCodegen<'_, '_> {
    fn run(&mut self) -> Result<()> {
        let order = self.reverse_postorder();

        for &bid in &order {
            let blk = self.b.create_block();
            self.blocks[bid.idx()] = Some(blk);
        }

        // Phis become block parameters, in the order the block lists them.
        // Every jump into the block supplies its arguments in that same order.
        for &bid in &order {
            let blk = self.blocks[bid.idx()].expect("block in order has a CLIF block");
            for pi in 0..self.f.blocks[bid.idx()].phis.len() {
                let dst = self.f.blocks[bid.idx()].phis[pi].dst;
                // A void phi (AIR merges void call results across arms) has
                // no machine representation and no legal use; it gets no
                // block parameter. This was mandelbrot's whole bottleneck:
                // declining the kernel for it closed the Cranelift OSR door,
                // and the loop interpreted while a promote-sized LLVM
                // compile ran.
                if self.is_void(dst) {
                    continue;
                }
                let ty = self.value_clif_ty(dst)?;
                let v = self.b.append_block_param(blk, ty);
                self.vals[dst.idx()] = Some(v);
            }
        }

        let entry = self.blocks[0].ok_or_else(|| anyhow!("entry block is unreachable"))?;
        self.b.append_block_params_for_function_params(entry);
        self.b.switch_to_block(entry);

        self.bind_entry()?;

        for &bid in &order {
            let blk = self.blocks[bid.idx()].expect("block in order has a CLIF block");
            if bid.0 != 0 {
                self.b.switch_to_block(blk);
            }
            for ii in 0..self.f.blocks[bid.idx()].instrs.len() {
                let instr = self.f.blocks[bid.idx()].instrs[ii].clone();
                self.emit(&instr)?;
            }
            let term = self.f.blocks[bid.idx()].term.clone();
            self.emit_term(bid, &term)?;
        }
        self.b.seal_all_blocks();
        Ok(())
    }

    /// Consume the builder — this is what runs cranelift-frontend's own
    /// block and seal invariant checks.
    fn finish(self) {
        self.b.finalize();
    }

    /// Emit an ON-STACK-REPLACEMENT entry: `fn(buf: *mut u64) -> ret`.
    ///
    /// `buf` is the interpreter's transfer buffer — one 64-bit slot per HL
    /// register, floats as raw bits (`value_to_i64`). The entry block loads
    /// every value that is live into the region reachable from `header`,
    /// initializes the cells the same way, and jumps to the header supplying
    /// its phi arguments from the same buffer (a phi's destination register
    /// IS its de-SSA slot, which is what makes that correct). Blocks the
    /// header cannot reach are never emitted.
    ///
    /// SSA soundness: a value defined before the loop and used inside it has
    /// no definition in the emitted subgraph, so every such value is
    /// re-defined in the entry block by a load — and the entry block
    /// dominates everything emitted, which restores the dominance the
    /// original definition provided.
    fn run_osr(&mut self, header: usize) -> Result<()> {
        let order = self.rpo_from(header);

        for &bid in &order {
            let blk = self.b.create_block();
            self.blocks[bid.idx()] = Some(blk);
        }
        for &bid in &order {
            let blk = self.blocks[bid.idx()].expect("block in order has a CLIF block");
            for pi in 0..self.f.blocks[bid.idx()].phis.len() {
                let dst = self.f.blocks[bid.idx()].phis[pi].dst;
                if self.is_void(dst) {
                    continue; // no machine representation — see run()
                }
                let ty = self.value_clif_ty(dst)?;
                let v = self.b.append_block_param(blk, ty);
                self.vals[dst.idx()] = Some(v);
            }
        }

        let entry = self.b.create_block();
        self.b.append_block_params_for_function_params(entry);
        self.b.switch_to_block(entry);
        let buf = self.b.block_params(entry)[0];

        // Cells are registers too; their current values are in the buffer.
        for ci in 0..self.f.cells.len() {
            let cell = self.f.cells[ci].clone();
            let ty = self.clif_ty(cell.ty)?;
            let slot = self.b.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                8,
                3,
            ));
            self.cells.push(slot);
            let v = self.load_osr_slot(buf, cell.reg, ty)?;
            self.b.ins().stack_store(v, slot, 0);
        }

        // Live-ins, in EMISSION order: any use not preceded by a definition
        // in that order loads from the transfer buffer at its de-SSA
        // register slot. Emission order is the binding constraint (a CLIF
        // value must exist before its use), and it is also semantically
        // right: `buf[value_reg(v)]` is what the interpreter's register held
        // at the transfer point, which is de-SSA truth for every value whose
        // definition the header-entry path bypasses. A mere
        // "defined-anywhere-in-region" rule missed exactly those — a def on
        // a path the header does not dominate left its uses reading an
        // unmaterialized value (mandelbrot's kernel, v83).
        let mut defined = vec![false; self.f.values.len()];
        let mut live_in = vec![false; self.f.values.len()];
        {
            let mut note_use = |u: ValueId, defined: &[bool], live_in: &mut [bool]| {
                if !defined[u.idx()] {
                    live_in[u.idx()] = true;
                }
            };
            for &bid in &order {
                let blk = &self.f.blocks[bid.idx()];
                for phi in &blk.phis {
                    defined[phi.dst.idx()] = true;
                }
                for i in &blk.instrs {
                    for u in i.uses() {
                        note_use(u, &defined, &mut live_in);
                    }
                    if let Some(d) = i.dst() {
                        defined[d.idx()] = true;
                    }
                }
                for u in blk.term.uses() {
                    note_use(u, &defined, &mut live_in);
                }
                for succ in blk.term.successors() {
                    if succ.idx() < self.blocks.len() && self.blocks[succ.idx()].is_some() {
                        for phi in &self.f.blocks[succ.idx()].phis {
                            if let Some((_, v)) = phi.incoming.iter().find(|(p, _)| *p == bid) {
                                note_use(*v, &defined, &mut live_in);
                            }
                        }
                    }
                }
            }
        }
        for v in 0..self.f.values.len() {
            if live_in[v] {
                let vid = ValueId(v as u32);
                if self.is_void(vid) {
                    continue;
                }
                // Used before its in-region definition in emission order:
                // the header-entry path can bypass the def, and whether the
                // buffer slot or the (later) definition is the right value
                // is a dataflow question this entry does not answer. The
                // first attempt answered it wrong and produced an entry
                // that looped forever. Decline; the function's ordinary
                // compiled entry still serves every future CALL, which is
                // what a frequently-invoked function actually needs.
                if defined[v] {
                    bail!(
                        "OSR entry: v{v} is used before its in-region definition; \
                         header-relative liveness is ambiguous"
                    );
                }
                let ty = self.value_clif_ty(vid)?;
                let reg = self.f.value_reg(vid);
                let loaded = self.load_osr_slot(buf, reg, ty)?;
                self.vals[v] = Some(loaded);
            }
        }

        // Into the header, phi args from the buffer.
        let mut args: Vec<BlockArg> = Vec::new();
        for phi in &self.f.blocks[header].phis {
            if self.is_void(phi.dst) {
                continue; // void phis have no block parameter
            }
            let ty = abi_class(self.ctx.type_kind(self.f.value_ty(phi.dst).0 as usize)?)
                .clif_type()
                .ok_or_else(|| anyhow!("void phi"))?;
            let reg = self.f.value_reg(phi.dst);
            let v = self.load_osr_slot(buf, reg, ty)?;
            args.push(BlockArg::Value(v));
        }
        let hblk = self.blocks[header].expect("header emitted");
        self.b.ins().jump(hblk, &args);

        for &bid in &order {
            let blk = self.blocks[bid.idx()].expect("block in order has a CLIF block");
            self.b.switch_to_block(blk);
            for ii in 0..self.f.blocks[bid.idx()].instrs.len() {
                let instr = self.f.blocks[bid.idx()].instrs[ii].clone();
                self.emit(&instr)?;
            }
            let term = self.f.blocks[bid.idx()].term.clone();
            self.emit_term(bid, &term)?;
        }
        self.b.seal_all_blocks();
        Ok(())
    }

    /// One typed load from the transfer buffer. Slot `reg` holds the
    /// register's value as the interpreter marshaled it: floats as raw f64
    /// bits (so an F64 load reads them back exactly), integers
    /// sign-extended, pointers as-is.
    fn load_osr_slot(&mut self, buf: Value, reg: u32, ty: Type) -> Result<Value> {
        let off = (reg as i32) * 8;
        if ty == types::F64 {
            return Ok(self.b.ins().load(types::F64, MemFlags::trusted(), buf, off));
        }
        if ty == types::F32 {
            bail!("f32 register in an OSR frame");
        }
        let wide = self.b.ins().load(types::I64, MemFlags::trusted(), buf, off);
        self.coerce(wide, ty)
    }

    /// Blocks reachable from the entry, in reverse postorder.
    ///
    /// RPO is what makes a single forward pass legal: every value a
    /// non-phi use reads is defined in a block that dominates the use, and
    /// every dominator precedes its dominatee here. Phi arguments are read at
    /// the *predecessor's* terminator, which the same argument covers.
    fn reverse_postorder(&self) -> Vec<BlockId> {
        self.rpo_from(0)
    }

    /// Reverse postorder over the blocks reachable from `seed`. The OSR
    /// entry path seeds at a loop header instead of block 0, and everything
    /// the header cannot reach is simply never emitted.
    fn rpo_from(&self, seed: usize) -> Vec<BlockId> {
        let n = self.f.blocks.len();
        let mut seen = vec![false; n];
        let mut post: Vec<BlockId> = Vec::with_capacity(n);
        // Iterative DFS: (block, next successor index).
        let mut stack: Vec<(usize, usize)> = vec![(seed, 0)];
        seen[seed] = true;
        while let Some((b, si)) = stack.pop() {
            let succs = self.f.blocks[b].term.successors();
            if si < succs.len() {
                stack.push((b, si + 1));
                let s = succs[si].idx();
                if s < n && !seen[s] {
                    seen[s] = true;
                    stack.push((s, 0));
                }
            } else {
                post.push(BlockId(b as u32));
            }
        }
        post.reverse();
        post
    }

    /// Bind function parameters and give every cell its initial contents.
    ///
    /// A cell backing an *argument* register must be seeded from the incoming
    /// parameter. AIR emits no `Param` for a pinned register — the serialize
    /// path does not need one, because there the register is the storage — so
    /// a backend that puts cells in stack slots and skips this reads a zero
    /// where the argument should be.
    fn bind_entry(&mut self) -> Result<()> {
        let entry = self.blocks[0].expect("entry exists");
        let params: Vec<Value> = self.b.block_params(entry).to_vec();

        for (ci, cell) in self.f.cells.iter().enumerate() {
            let ty = self.clif_ty(cell.ty)?;
            let slot = self.b.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                8,
                3,
            ));
            self.cells.push(slot);
            let init = if (cell.reg as usize) < self.nargs {
                let p = params[cell.reg as usize];
                self.coerce(p, ty)?
            } else if ty.is_float() {
                self.b.ins().f64const(0.0)
            } else {
                self.b.ins().iconst(ty, 0)
            };
            debug_assert_eq!(ci + 1, self.cells.len());
            self.b.ins().stack_store(init, slot, 0);
        }
        Ok(())
    }

    // ── Type and value plumbing ─────────────────────────────────────────────

    fn clif_ty(&self, tr: AirTypeRef) -> Result<Type> {
        abi_class(self.ctx.type_kind(tr.0 as usize)?)
            .clif_type()
            .ok_or_else(|| anyhow!("type {} has no machine type", tr.0))
    }

    fn value_clif_ty(&self, v: ValueId) -> Result<Type> {
        self.clif_ty(self.f.value_ty(v))
    }

    fn class_of(&self, v: ValueId) -> Result<AbiClass> {
        Ok(abi_class(
            self.ctx.type_kind(self.f.value_ty(v).0 as usize)?,
        ))
    }

    fn is_void(&self, v: ValueId) -> bool {
        self.ctx
            .type_kind(self.f.value_ty(v).0 as usize)
            .map(|k| abi_class(k) == AbiClass::Void)
            .unwrap_or(false)
    }

    fn get(&self, v: ValueId) -> Result<Value> {
        self.vals[v.idx()].ok_or_else(|| anyhow!("use of undefined AIR value v{}", v.0))
    }

    /// Define `dst`, coercing to its declared machine type. Void destinations
    /// are dropped: they have no representation and no legal use.
    fn def(&mut self, dst: ValueId, v: Value) -> Result<()> {
        if self.is_void(dst) {
            return Ok(());
        }
        let want = self.value_clif_ty(dst)?;
        let v = self.coerce(v, want)?;
        self.vals[dst.idx()] = Some(v);
        Ok(())
    }

    /// Width / representation change, restricted to the ones that are well
    /// defined for HashLink values. Anything else declines the function.
    fn coerce(&mut self, v: Value, want: Type) -> Result<Value> {
        let have = self.b.func.dfg.value_type(v);
        if have == want {
            return Ok(v);
        }
        if have.is_int() && want.is_int() {
            return Ok(if have.bits() > want.bits() {
                self.b.ins().ireduce(want, v)
            } else {
                self.b.ins().sextend(want, v)
            });
        }
        if have == types::F32 && want == types::F64 {
            return Ok(self.b.ins().fpromote(want, v));
        }
        if have == types::F64 && want == types::F32 {
            return Ok(self.b.ins().fdemote(want, v));
        }
        bail!("cannot coerce {have} to {want}")
    }

    // ── Instructions ────────────────────────────────────────────────────────

    fn emit(&mut self, i: &Instr) -> Result<()> {
        match i {
            // Bound in `bind_entry` for arguments; locals start at zero, the
            // same as the interpreter's register file.
            Instr::Param { dst, reg } => {
                if self.is_void(*dst) {
                    return Ok(());
                }
                let ty = self.value_clif_ty(*dst)?;
                let v = if (*reg as usize) < self.nargs {
                    let entry = self.blocks[0].expect("entry exists");
                    self.b.block_params(entry)[*reg as usize]
                } else if ty.is_float() {
                    self.b.ins().f64const(0.0)
                } else {
                    self.b.ins().iconst(ty, 0)
                };
                self.def(*dst, v)?;
            }

            Instr::Copy { dst, src } => {
                let v = self.get(*src)?;
                self.def(*dst, v)?;
            }

            Instr::Int { dst, idx } => {
                let val = *self
                    .ctx
                    .bytecode()
                    .ints
                    .get(*idx)
                    .ok_or_else(|| anyhow!("int constant {idx} out of range"))?;
                let v = self.b.ins().iconst(types::I32, val as i64);
                self.def(*dst, v)?;
            }
            Instr::Float { dst, idx } => {
                let val = *self
                    .ctx
                    .bytecode()
                    .floats
                    .get(*idx)
                    .ok_or_else(|| anyhow!("float constant {idx} out of range"))?;
                let v = self.b.ins().f64const(val);
                self.def(*dst, v)?;
            }
            Instr::Bool { dst, value } => {
                let v = self.b.ins().iconst(types::I8, i64::from(*value));
                self.def(*dst, v)?;
            }
            Instr::String { dst, idx } => {
                let addr = self.ctx.string_ptr(*idx)?;
                let v = self.b.ins().iconst(types::I64, addr as i64);
                self.def(*dst, v)?;
            }
            Instr::Null { dst } => {
                let ty = self.value_clif_ty(*dst)?;
                if ty.is_float() {
                    bail!("Null into a float value");
                }
                let v = self.b.ins().iconst(ty, 0);
                self.def(*dst, v)?;
            }

            Instr::BinOp { op, dst, a, b } => self.emit_binop(*op, *dst, *a, *b)?,

            // A real fused multiply-add. The serialize path lowers `Fma` back
            // to `Mul` + `Add` because HL bytecode has no fused opcode, so
            // this is arithmetic only a backend reading the IR can emit.
            Instr::Fma { dst, a, b, c } => {
                let (va, vb, vc) = (self.get(*a)?, self.get(*b)?, self.get(*c)?);
                let t = self.b.func.dfg.value_type(va);
                if !t.is_float() {
                    bail!("Fma on non-float operands");
                }
                let r = self.b.ins().fma(va, vb, vc);
                self.def(*dst, r)?;
            }

            Instr::UnOp { op, dst, src } => {
                let v = self.get(*src)?;
                let t = self.b.func.dfg.value_type(v);
                let r = match op {
                    UnOp::Neg => {
                        if t.is_float() {
                            self.b.ins().fneg(v)
                        } else {
                            self.b.ins().ineg(v)
                        }
                    }
                    UnOp::Not => {
                        if self.class_of(*src)? == AbiClass::Bool {
                            // Logical not on a 0/1 byte.
                            self.b.ins().bxor_imm(v, 1)
                        } else if t.is_int() {
                            self.b.ins().bnot(v)
                        } else {
                            bail!("Not on a float value");
                        }
                    }
                    UnOp::Incr => {
                        if t.is_float() {
                            bail!("Incr on a float value");
                        }
                        self.b.ins().iadd_imm(v, 1)
                    }
                    UnOp::Decr => {
                        if t.is_float() {
                            bail!("Decr on a float value");
                        }
                        self.b.ins().iadd_imm(v, -1)
                    }
                };
                self.def(*dst, r)?;
            }

            Instr::Call { dst, fun, args } => self.emit_call(*dst, *fun, args)?,

            // A stdlib operation the IR knows outright — one machine
            // sequence, no FFI, and `Effect::Pure` upstream means LICM was
            // free to move it (and the loads around it).
            Instr::Intrinsic { kind, dst, args, .. } => {
                if *kind == air::v2::ir::IntrinsicKind::PtrCompare {
                    // Three-way identity compare, ash_std's hlp_ptr_compare
                    // verbatim: (a > b) as i32 - (a < b) as i32, unsigned —
                    // it compares usize addresses, never contents.
                    let a = self.get(args[0])?;
                    let b = self.get(args[1])?;
                    let gt = self.b.ins().icmp(IntCC::UnsignedGreaterThan, a, b);
                    let lt = self.b.ins().icmp(IntCC::UnsignedLessThan, a, b);
                    let gt32 = self.b.ins().uextend(types::I32, gt);
                    let lt32 = self.b.ins().uextend(types::I32, lt);
                    let v = self.b.ins().isub(gt32, lt32);
                    self.def(*dst, v)?;
                } else {
                    let x =
                        self.get(*args.first().ok_or_else(|| anyhow!("intrinsic arity"))?)?;
                    let v = self.emit_native_intrinsic(intrinsic_to_native(*kind), x);
                    self.def(*dst, v)?;
                }
            }

            Instr::GetGlobal { dst, global } => {
                if self.class_of(*dst)? != AbiClass::Ptr {
                    bail!("GetGlobal into a non-pointer value");
                }
                let addr = self.ctx.global_slot_addr(*global)?;
                let base = self.b.ins().iconst(types::I64, addr as i64);
                let v = self.b.ins().load(types::I64, MemFlags::trusted(), base, 0);
                self.def(*dst, v)?;
            }
            Instr::SetGlobal { global, src } => {
                if self.class_of(*src)? != AbiClass::Ptr {
                    bail!("SetGlobal from a non-pointer value");
                }
                let addr = self.ctx.global_slot_addr(*global)?;
                let base = self.b.ins().iconst(types::I64, addr as i64);
                let v = self.get(*src)?;
                self.b.ins().store(MemFlags::trusted(), v, base, 0);
            }

            // The object type comes from AIR, which resolved it at lowering
            // time — not from the destination register's declared type.
            Instr::FieldGet {
                dst,
                obj,
                obj_ty,
                field,
            } => {
                let (off, fty) = self.field_offset(*obj_ty, *field)?;
                let base = self.get(*obj)?;
                let raw = self.b.ins().load(fty, MemFlags::trusted(), base, off);
                self.def(*dst, raw)?;
            }
            Instr::FieldSet {
                obj,
                obj_ty,
                field,
                src,
            } => {
                let (off, fty) = self.field_offset(*obj_ty, *field)?;
                let base = self.get(*obj)?;
                let raw = self.get(*src)?;
                // Narrow to the field's own width first, so the store cannot
                // spill into whatever is laid out next to it.
                let v = self.coerce(raw, fty)?;
                self.b.ins().store(MemFlags::trusted(), v, base, off);
            }

            Instr::MemGet {
                kind,
                dst,
                base,
                index,
            } => self.emit_mem_get(*kind, *dst, *base, *index)?,
            Instr::MemSet {
                kind,
                base,
                index,
                src,
            } => self.emit_mem_set(*kind, *base, *index, *src)?,

            Instr::ArraySize { dst, array } => {
                let base = self.get(*array)?;
                let raw = self.b.ins().load(
                    types::I32,
                    MemFlags::trusted(),
                    base,
                    crate::layout::VARRAY_SIZE_OFFSET,
                );
                self.def(*dst, raw)?;
            }

            // The runtime type identity, not the decoded description: an
            // object header holds this pointer and the allocators key on it.
            Instr::TypeConst { dst, ty } => {
                let p = self.ctx.type_ptr(ty.0 as usize)?;
                let v = self.b.ins().iconst(types::I64, p as i64);
                self.def(*dst, v)?;
            }

            // `new C()`. Which allocator runs is decided by the destination's
            // type kind, the same three-way split the LLVM tier makes.
            Instr::New { dst } => {
                let ty = self.f.value_ty(*dst);
                let kind = self.ctx.type_kind(ty.0 as usize)?;
                let (addr, takes_type) = self.ctx.alloc_helper(kind)?;
                let callee = self.b.ins().iconst(types::I64, addr as i64);
                let args: Vec<Value> = if takes_type {
                    let p = self.ctx.type_ptr(ty.0 as usize)?;
                    vec![self.b.ins().iconst(types::I64, p as i64)]
                } else {
                    vec![]
                };
                let params: Vec<Type> = args.iter().map(|_| types::I64).collect();
                let sig = self.helper_sigref(&params, Some(types::I64));
                let call = self.b.ins().call_indirect(sig, callee, &args);
                let v = self.b.inst_results(call)[0];
                self.def(*dst, v)?;
            }

            Instr::CallMethod { dst, field, args } => {
                self.emit_call_method(*dst, *field, args)?
            }

            Instr::Cast { kind, dst, src } => self.emit_cast(*kind, *dst, *src)?,

            Instr::NullCheck { value } => self.emit_null_check(*value)?,

            // `Ref` / `Unref` / `Setref`, which the opcode lowerer refuses by
            // construction: it holds registers in `Variable`s, which have no
            // address. AIR already isolated the address-taken registers as
            // cells, and a cell is a stack slot.
            Instr::CellRef { dst, cell } => {
                let slot = self.cell_slot(*cell)?;
                let v = self.b.ins().stack_addr(types::I64, slot, 0);
                self.def(*dst, v)?;
            }
            Instr::CellGet { dst, cell } => {
                let slot = self.cell_slot(*cell)?;
                let ty = self.clif_ty(self.f.cells[cell.idx()].ty)?;
                let v = self.b.ins().stack_load(ty, slot, 0);
                self.def(*dst, v)?;
            }
            Instr::CellSet { cell, src } => {
                let slot = self.cell_slot(*cell)?;
                let ty = self.clif_ty(self.f.cells[cell.idx()].ty)?;
                let raw = self.get(*src)?;
                let v = self.coerce(raw, ty)?;
                self.b.ins().stack_store(v, slot, 0);
            }
            Instr::CellIncr { cell } => self.emit_cell_step(*cell, 1)?,
            Instr::CellDecr { cell } => self.emit_cell_step(*cell, -1)?,

            Instr::Unref { dst, src } => {
                let ty = self.value_clif_ty(*dst)?;
                let p = self.get(*src)?;
                let v = self.b.ins().load(ty, MemFlags::trusted(), p, 0);
                self.def(*dst, v)?;
            }
            Instr::SetRef { r, value } => {
                let p = self.get(*r)?;
                let v = self.get(*value)?;
                self.b.ins().store(MemFlags::trusted(), v, p, 0);
            }

            other => bail!("unhandled AIR instruction {:?}", instr_reject(other)),
        }
        Ok(())
    }

    fn emit_cell_step(&mut self, cell: CellId, delta: i64) -> Result<()> {
        let slot = self.cell_slot(cell)?;
        let ty = self.clif_ty(self.f.cells[cell.idx()].ty)?;
        if ty.is_float() {
            bail!("Incr/Decr on a float cell");
        }
        let cur = self.b.ins().stack_load(ty, slot, 0);
        let next = self.b.ins().iadd_imm(cur, delta);
        self.b.ins().stack_store(next, slot, 0);
        Ok(())
    }

    fn cell_slot(&self, cell: CellId) -> Result<StackSlot> {
        self.cells
            .get(cell.idx())
            .copied()
            .ok_or_else(|| anyhow!("cell {} out of range", cell.0))
    }

    /// Compile-time byte offset and machine type of a field.
    ///
    /// Declines rather than falling back to a runtime lookup: the kinds that
    /// need one (`HVIRTUAL`, `HDYNOBJ`, packed layouts) are exactly what the
    /// LLVM tier is there for.
    fn field_offset(&self, obj_ty: AirTypeRef, field: usize) -> Result<(i32, Type)> {
        let type_index = obj_ty.0 as usize;
        let bytecode = self.ctx.bytecode();
        let kind = bytecode
            .types
            .get(type_index)
            .ok_or_else(|| anyhow!("field access on unknown type {type_index}"))?
            .kind;
        if kind != hl::hl_type_kind_HOBJ && kind != hl::hl_type_kind_HSTRUCT {
            bail!("field access needs a runtime lookup for type kind {kind}");
        }
        let (offset, field_kind) =
            crate::layout::field_offset_and_kind(&bytecode.types, type_index, field)
                .ok_or_else(|| anyhow!("no static layout for type {type_index} field {field}"))?;
        let ty = abi_class(field_kind)
            .clif_type()
            .ok_or_else(|| anyhow!("field of kind {field_kind} has no machine type"))?;
        Ok((offset, ty))
    }

    /// An index widened to pointer width. HL indices are signed 32-bit, so
    /// this sign-extends — matching the LLVM tier rather than computing a
    /// different wrong address for an out-of-range index.
    fn index_as_addr(&mut self, index: ValueId) -> Result<Value> {
        let v = self.get(index)?;
        let have = self.b.func.dfg.value_type(v);
        Ok(match have.bits().cmp(&64) {
            std::cmp::Ordering::Less => self.b.ins().sextend(types::I64, v),
            std::cmp::Ordering::Equal => v,
            std::cmp::Ordering::Greater => bail!("index wider than a pointer"),
        })
    }

    fn emit_mem_get(
        &mut self,
        kind: MemAccess,
        dst: ValueId,
        base: ValueId,
        index: ValueId,
    ) -> Result<()> {
        let vbase = self.get(base)?;
        let idx = self.index_as_addr(index)?;
        match kind {
            // Raw bytes: a byte offset with no stride and no header.
            MemAccess::Mem => {
                let ty = self.value_clif_ty(dst)?;
                let addr = self.b.ins().iadd(vbase, idx);
                let v = self.b.ins().load(ty, MemFlags::trusted(), addr, 0);
                self.def(dst, v)
            }
            MemAccess::Array => {
                let hl_kind = self.ctx.type_kind(self.f.value_ty(dst).0 as usize)?;
                let ty = abi_class(hl_kind)
                    .clif_type()
                    .ok_or_else(|| anyhow!("array element of kind {hl_kind} has no machine type"))?;
                let stride = crate::layout::array_elem_size(hl_kind) as i64;
                let byte_off = self.b.ins().imul_imm(idx, stride);
                let addr = self.b.ins().iadd(vbase, byte_off);
                let v = self.b.ins().load(
                    ty,
                    MemFlags::trusted(),
                    addr,
                    crate::layout::VARRAY_DATA_OFFSET,
                );
                self.def(dst, v)
            }
            // A raw byte offset read through an unsigned C type, then
            // widened — `*(unsigned char*)` and `*(unsigned short*)`, which
            // is what `ash_interp` does and what HashLink's own opcodes mean.
            MemAccess::I8 | MemAccess::I16 => {
                let ty = if kind == MemAccess::I8 {
                    types::I8
                } else {
                    types::I16
                };
                let addr = self.b.ins().iadd(vbase, idx);
                let raw = self.b.ins().load(ty, MemFlags::trusted(), addr, 0);
                let want = self.value_clif_ty(dst)?;
                let v = if want.bits() > ty.bits() {
                    self.b.ins().uextend(want, raw)
                } else {
                    self.coerce(raw, want)?
                };
                self.def(dst, v)
            }
        }
    }

    fn emit_mem_set(
        &mut self,
        kind: MemAccess,
        base: ValueId,
        index: ValueId,
        src: ValueId,
    ) -> Result<()> {
        let vbase = self.get(base)?;
        let idx = self.index_as_addr(index)?;
        let raw = self.get(src)?;
        match kind {
            MemAccess::Mem => {
                let addr = self.b.ins().iadd(vbase, idx);
                self.b.ins().store(MemFlags::trusted(), raw, addr, 0);
                Ok(())
            }
            MemAccess::Array => {
                let hl_kind = self.ctx.type_kind(self.f.value_ty(src).0 as usize)?;
                let ty = abi_class(hl_kind)
                    .clif_type()
                    .ok_or_else(|| anyhow!("array element of kind {hl_kind} has no machine type"))?;
                let stride = crate::layout::array_elem_size(hl_kind) as i64;
                let byte_off = self.b.ins().imul_imm(idx, stride);
                let addr = self.b.ins().iadd(vbase, byte_off);
                let v = self.coerce(raw, ty)?;
                self.b.ins().store(
                    MemFlags::trusted(),
                    v,
                    addr,
                    crate::layout::VARRAY_DATA_OFFSET,
                );
                Ok(())
            }
            MemAccess::I8 | MemAccess::I16 => {
                let ty = if kind == MemAccess::I8 {
                    types::I8
                } else {
                    types::I16
                };
                let addr = self.b.ins().iadd(vbase, idx);
                let v = self.coerce(raw, ty)?;
                self.b.ins().store(MemFlags::trusted(), v, addr, 0);
                Ok(())
            }
        }
    }

    fn emit_cast(&mut self, kind: CastKind, dst: ValueId, src: ValueId) -> Result<()> {
        let v = self.get(src)?;
        let t = self.b.func.dfg.value_type(v);
        let r = match kind {
            CastKind::UnsafeCast => v,
            // Saturating: the non-saturating conversion traps on NaN and on
            // out-of-range inputs, which would abort the process where LLVM's
            // `fptosi` is merely undefined.
            CastKind::ToInt => {
                if t.is_float() {
                    self.b.ins().fcvt_to_sint_sat(types::I32, v)
                } else {
                    self.coerce(v, types::I32)?
                }
            }
            CastKind::ToSFloat => {
                if t.is_float() {
                    self.coerce(v, types::F64)?
                } else {
                    self.b.ins().fcvt_from_sint(types::F64, v)
                }
            }
            CastKind::ToUFloat => {
                if t.is_float() {
                    self.coerce(v, types::F64)?
                } else {
                    self.b.ins().fcvt_from_uint(types::F64, v)
                }
            }
            CastKind::ToDyn | CastKind::SafeCast | CastKind::ToVirtual => {
                bail!("cast {kind:?} needs a runtime helper")
            }
        };
        self.def(dst, r)
    }

    /// `if value == null throw`.
    ///
    /// Throwing *out* of a Cranelift frame is sound: `hlp_error` longjmps to
    /// the interpreter's per-call trap, strictly outside this frame. Only
    /// resuming *into* one would need more.
    fn emit_null_check(&mut self, value: ValueId) -> Result<()> {
        if self.class_of(value)? != AbiClass::Ptr {
            return Ok(()); // scalars are never null
        }
        let v = self.get(value)?;
        let throw_block = self.b.create_block();
        let cont_block = self.b.create_block();
        self.b.ins().brif(v, cont_block, &[], throw_block, &[]);

        self.b.switch_to_block(throw_block);
        let addr = self.ctx.hl_error_addr()?;
        let msg = self.ctx.utf16_message("Null access");
        let sig = self.helper_sigref(&[types::I64], None);
        let callee = self.b.ins().iconst(types::I64, addr as i64);
        let msg_val = self.b.ins().iconst(types::I64, msg as i64);
        self.b.ins().call_indirect(sig, callee, &[msg_val]);
        self.b
            .ins()
            .trap(cranelift_codegen::ir::TrapCode::unwrap_user(1));

        self.b.switch_to_block(cont_block);
        Ok(())
    }

    fn emit_binop(&mut self, op: BinOp, dst: ValueId, a: ValueId, b: ValueId) -> Result<()> {
        let va = self.get(a)?;
        let vb = self.get(b)?;
        let ta = self.b.func.dfg.value_type(va);
        let tb = self.b.func.dfg.value_type(vb);
        if ta != tb {
            bail!("mismatched operand types {ta}/{tb}");
        }
        let r = if ta.is_float() {
            match op {
                BinOp::Add => self.b.ins().fadd(va, vb),
                BinOp::Sub => self.b.ins().fsub(va, vb),
                BinOp::Mul => self.b.ins().fmul(va, vb),
                BinOp::SDiv | BinOp::UDiv => self.b.ins().fdiv(va, vb),
                // No CLIF `frem`; declining beats emitting a libcall whose
                // ABI is not verified here.
                BinOp::SMod | BinOp::UMod => bail!("float modulo"),
                _ => bail!("bitwise op on floats"),
            }
        } else {
            match op {
                BinOp::Add => self.b.ins().iadd(va, vb),
                BinOp::Sub => self.b.ins().isub(va, vb),
                BinOp::Mul => self.b.ins().imul(va, vb),
                BinOp::And => self.b.ins().band(va, vb),
                BinOp::Or => self.b.ins().bor(va, vb),
                BinOp::Xor => self.b.ins().bxor(va, vb),
                // Cranelift masks the shift amount to the operand width, the
                // same as the aarch64 instruction the LLVM tier lowers to.
                BinOp::Shl => self.b.ins().ishl(va, vb),
                BinOp::SShr => self.b.ins().sshr(va, vb),
                BinOp::UShr => self.b.ins().ushr(va, vb),
                BinOp::SDiv | BinOp::UDiv | BinOp::SMod | BinOp::UMod => {
                    match self.int_const_of(b) {
                        Some(c) => self.const_div(va, ta, op, c),
                        None => self.guarded_div(va, vb, ta, op),
                    }
                }
            }
        };
        self.def(dst, r)
    }

    /// The literal behind `v` when its definition is `Instr::Int`. Divisions
    /// are rare enough that a scan beats carrying a map.
    fn int_const_of(&self, v: ValueId) -> Option<i64> {
        for blk in &self.f.blocks {
            for ins in &blk.instrs {
                if let Instr::Int { dst, idx } = ins {
                    if *dst == v {
                        return self.ctx.bytecode().ints.get(*idx).map(|&i| i as i64);
                    }
                }
            }
        }
        None
    }

    /// Division/remainder by a compile-time constant. The zero and INT_MIN/-1
    /// guards of [`guarded_div`] fold away, and a power-of-two divisor
    /// strength-reduces to shifts and masks — Cranelift's mid-end does not do
    /// this for `srem`/`sdiv`, and on x86-64 the difference is a ~25-cycle
    /// `idiv` per iteration in a `% 8` reduction loop. Semantics are HL's:
    /// truncated division, remainder takes the dividend's sign, and the
    /// INT_MIN edge cases wrap exactly as the guarded path's answers do.
    fn const_div(&mut self, va: Value, ty: Type, op: BinOp, c: i64) -> Value {
        let bits = i64::from(ty.bits());
        let abs = c.unsigned_abs() as i64;
        if c == 0 {
            // HL yields 0 for a zero divisor (division) and 0 for modulo.
            return self.b.ins().iconst(ty, 0);
        }
        if abs == 1 {
            return match op {
                // x / -1 = -x; `ineg` wraps, which is exactly the
                // INT_MIN / -1 answer the guarded path selects.
                BinOp::SDiv if c < 0 => self.b.ins().ineg(va),
                BinOp::SDiv | BinOp::UDiv => va,
                _ => self.b.ins().iconst(ty, 0),
            };
        }
        let pow2 = abs & (abs - 1) == 0;
        if pow2 && matches!(op, BinOp::SDiv | BinOp::SMod) {
            let k = i64::from(abs.trailing_zeros());
            // Truncated division needs the bias 2^k - 1 added for negative
            // dividends; the arithmetic shift of the sign produces it.
            let sign = self.b.ins().sshr_imm(va, bits - 1);
            let bias = self.b.ins().ushr_imm(sign, bits - k);
            let sum = self.b.ins().iadd(va, bias);
            return match op {
                BinOp::SDiv => {
                    let q = self.b.ins().sshr_imm(sum, k);
                    if c < 0 {
                        self.b.ins().ineg(q)
                    } else {
                        q
                    }
                }
                _ => {
                    // Remainder keeps the dividend's sign: mask the biased
                    // value, un-bias. |x % c| depends only on |c|.
                    let m = self.b.ins().band_imm(sum, abs - 1);
                    self.b.ins().isub(m, bias)
                }
            };
        }
        if pow2 && c > 0 {
            match op {
                BinOp::UDiv => return self.b.ins().ushr_imm(va, abs.trailing_zeros() as i64),
                BinOp::UMod => return self.b.ins().band_imm(va, abs - 1),
                _ => {}
            }
        }
        // Any other non-zero, non-(-1) constant: the guards can never fire,
        // so emit the raw operation. (-1 is handled above for signed; for
        // unsigned it is just a huge divisor and this is still exact.)
        let vb = self.b.ins().iconst(ty, c);
        match op {
            BinOp::SDiv => self.b.ins().sdiv(va, vb),
            BinOp::UDiv => self.b.ins().udiv(va, vb),
            BinOp::SMod => self.b.ins().srem(va, vb),
            _ => self.b.ins().urem(va, vb),
        }
    }

    /// Integer division and remainder without Cranelift's trapping edge cases.
    ///
    /// CLIF `sdiv`/`srem`/`udiv`/`urem` trap on a zero divisor (and, signed,
    /// on `INT_MIN / -1`) — a process abort, where the interpreter merely
    /// errors. Normalise the divisor and select instead: zero divisor yields
    /// 0, and `INT_MIN / -1` yields the `wrapping_div` / `wrapping_rem`
    /// answers. This mirrors [`super::lower`] exactly, because the two tiers
    /// must agree on the same program.
    fn guarded_div(&mut self, va: Value, vb: Value, ty: Type, op: BinOp) -> Value {
        let signed = matches!(op, BinOp::SDiv | BinOp::SMod);
        let is_div = matches!(op, BinOp::SDiv | BinOp::UDiv);

        let zero_div = self.b.ins().icmp_imm(IntCC::Equal, vb, 0);
        let bad = if signed {
            let min = 1i64 << (ty.bits() - 1);
            let a_is_min = self.b.ins().icmp_imm(IntCC::Equal, va, min.wrapping_neg());
            let b_is_neg1 = self.b.ins().icmp_imm(IntCC::Equal, vb, -1);
            let overflow = self.b.ins().band(a_is_min, b_is_neg1);
            self.b.ins().bor(zero_div, overflow)
        } else {
            zero_div
        };

        let one = self.b.ins().iconst(ty, 1);
        let safe_b = self.b.ins().select(bad, one, vb);
        let quot = match op {
            BinOp::SDiv => self.b.ins().sdiv(va, safe_b),
            BinOp::UDiv => self.b.ins().udiv(va, safe_b),
            BinOp::SMod => self.b.ins().srem(va, safe_b),
            _ => self.b.ins().urem(va, safe_b),
        };

        let zero = self.b.ins().iconst(ty, 0);
        let fallback = if is_div {
            self.b.ins().select(zero_div, zero, va)
        } else {
            zero
        };
        self.b.ins().select(bad, fallback, quot)
    }

    // ── Calls ───────────────────────────────────────────────────────────────

    fn helper_sigref(&mut self, params: &[Type], ret: Option<Type>) -> SigRef {
        let mut sig = Signature::new(self.ctx.call_conv());
        for p in params {
            sig.params.push(AbiParam::new(*p));
        }
        if let Some(r) = ret {
            sig.returns.push(AbiParam::new(r));
        }
        self.b.import_signature(sig)
    }

    fn emit_call(&mut self, dst: ValueId, target: usize, args: &[ValueId]) -> Result<()> {
        let bytecode = self.ctx.bytecode();
        let (tf, is_native) = if let Some(ni) = self.ctx.native_index(target) {
            let n = &bytecode.natives[ni];
            (
                bytecode.types[n.type_.0]
                    .fun
                    .as_ref()
                    .ok_or_else(|| anyhow!("native has no function type"))?,
                true,
            )
        } else if let Some(fi) = self.ctx.func_index(target) {
            let f = &bytecode.functions[fi];
            (
                bytecode.types[f.type_.0]
                    .fun
                    .as_ref()
                    .ok_or_else(|| anyhow!("callee has no function type"))?,
                false,
            )
        } else {
            bail!("unknown call target findex {target}");
        };

        if args.len() != tf.args.len() {
            bail!("call arity mismatch for findex {target}");
        }

        let param_classes: Vec<AbiClass> = tf
            .args
            .iter()
            .map(|a| abi_class(bytecode.types[a.0].kind))
            .collect();
        let ret_class = abi_class(bytecode.types[tf.ret.0].kind);
        let ret_ty = ret_class.clif_type();

        let mut arg_vals = Vec::with_capacity(args.len());
        for (idx, v) in args.iter().enumerate() {
            let want = param_classes[idx]
                .clif_type()
                .ok_or_else(|| anyhow!("void parameter"))?;
            let raw = self.get(*v)?;
            arg_vals.push(self.coerce(raw, want)?);
        }

        // Primitives that are single instructions rather than calls into
        // ash_std. Every entry in the table is unary. See crate::intrinsics.
        if is_native && arg_vals.len() == 1 {
            if let Some(intr) = self
                .ctx
                .native_index(target)
                .map(|ni| &bytecode.natives[ni])
                .and_then(|n| crate::intrinsics::lookup(n.lib.as_str(), n.name.as_str()))
            {
                let v = self.emit_native_intrinsic(intr, arg_vals[0]);
                return self.def(dst, v);
            }
        }

        let result = if is_native {
            let fref = *self
                .native_refs
                .get(&target)
                .ok_or_else(|| anyhow!("native findex {target} not declared by AIR"))?;
            let call = self.b.ins().call(fref, &arg_vals);
            ret_ty.map(|_| self.b.inst_results(call)[0])
        } else {
            self.stub_guarded_call(target, &arg_vals, &param_classes, ret_class)?
        };

        if let Some(v) = result {
            // Normalise a boolean result to 0/1: the callee may be an
            // LLVM-tier `i1` return whose upper bits are undefined.
            let v = if ret_class == AbiClass::Bool {
                self.b.ins().band_imm(v, 1)
            } else {
                v
            };
            self.def(dst, v)?;
        }
        Ok(())
    }

    fn emit_native_intrinsic(
        &mut self,
        intr: crate::intrinsics::NativeIntrinsic,
        x: Value,
    ) -> Value {
        use crate::intrinsics::NativeIntrinsic as NI;
        match intr {
            NI::IsNaN => self.b.ins().fcmp(FloatCC::Unordered, x, x),
            NI::IsFinite => {
                let abs = self.b.ins().fabs(x);
                let inf = self.b.ins().f64const(f64::INFINITY);
                self.b.ins().fcmp(FloatCC::LessThan, abs, inf)
            }
            _ => {
                let base = match intr {
                    NI::Sqrt => self.b.ins().sqrt(x),
                    NI::Abs => self.b.ins().fabs(x),
                    NI::Floor | NI::FloorToI32 => self.b.ins().floor(x),
                    NI::Ceil | NI::CeilToI32 => self.b.ins().ceil(x),
                    NI::RoundHalfUp | NI::RoundHalfUpToI32 => {
                        let half = self.b.ins().f64const(0.5);
                        let shifted = self.b.ins().fadd(x, half);
                        self.b.ins().floor(shifted)
                    }
                    NI::IsNaN | NI::IsFinite => unreachable!("handled above"),
                };
                if intr.returns_i32() {
                    self.b.ins().fcvt_to_sint_sat(types::I32, base)
                } else {
                    base
                }
            }
        }
    }

    /// Indirect call through `functions_ptrs[findex]`, guarded against the
    /// interpreter's stub sentinels — the same protocol
    /// [`super::lower::Lowerer::stub_guarded_call`] implements and the same
    /// one the LLVM tier's `build_stub_guarded_indirect_call` does.
    fn stub_guarded_call(
        &mut self,
        target: usize,
        args: &[Value],
        param_classes: &[AbiClass],
        ret_class: AbiClass,
    ) -> Result<Option<Value>> {
        let slot_addr = self.ctx.function_slot_addr(target)?;
        let slot_base = self.b.ins().iconst(types::I64, slot_addr as i64);
        let fn_addr = self
            .b
            .ins()
            .load(types::I64, MemFlags::trusted(), slot_base, 0);
        self.stub_guarded_indirect(fn_addr, args, param_classes, ret_class)
    }

    /// The same protocol against a callee address that is already in a
    /// register — a vtable slot, rather than a `functions_ptrs` entry.
    fn stub_guarded_indirect(
        &mut self,
        fn_addr: Value,
        args: &[Value],
        param_classes: &[AbiClass],
        ret_class: AbiClass,
    ) -> Result<Option<Value>> {
        let ret_ty = ret_class.clif_type();
        let is_stub =
            self.b
                .ins()
                .icmp_imm(IntCC::UnsignedLessThan, fn_addr, STUB_SENTINEL_LIMIT as i64);

        let direct_bb = self.b.create_block();
        let stub_bb = self.b.create_block();
        let merge_bb = self.b.create_block();
        if let Some(t) = ret_ty {
            self.b.append_block_param(merge_bb, t);
        }
        self.b.ins().brif(is_stub, stub_bb, &[], direct_bb, &[]);

        self.b.switch_to_block(direct_bb);
        let mut sig = Signature::new(self.ctx.call_conv());
        for c in param_classes {
            sig.params.push(AbiParam::new(
                c.clif_type().ok_or_else(|| anyhow!("void parameter"))?,
            ));
        }
        if let Some(t) = ret_ty {
            sig.returns.push(AbiParam::new(t));
        }
        let sigref = self.b.import_signature(sig);
        let call = self.b.ins().call_indirect(sigref, fn_addr, args);
        let direct_vals: Vec<BlockArg> = if ret_ty.is_some() {
            vec![BlockArg::Value(self.b.inst_results(call)[0])]
        } else {
            vec![]
        };
        self.b.ins().jump(merge_bb, &direct_vals);

        self.b.switch_to_block(stub_bb);
        let nargs = args.len();
        let slot = self.b.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            (nargs.max(1) * 8) as u32,
            3,
        ));
        for (idx, v) in args.iter().enumerate() {
            let ty = self.b.func.dfg.value_type(*v);
            let word = if ty == types::F64 {
                self.b.ins().bitcast(types::I64, MemFlags::new(), *v)
            } else if ty == types::F32 {
                let wide = self.b.ins().fpromote(types::F64, *v);
                self.b.ins().bitcast(types::I64, MemFlags::new(), wide)
            } else if ty.bits() < 64 {
                self.b.ins().uextend(types::I64, *v)
            } else {
                *v
            };
            self.b.ins().stack_store(word, slot, (idx * 8) as i32);
        }
        let buf = self.b.ins().stack_addr(types::I64, slot, 0);
        let stub_sig = self.helper_sigref(&[types::I64, types::I64, types::I32], Some(types::I64));
        let stub_addr = self
            .b
            .ins()
            .iconst(types::I64, ash_jit_call_stub as usize as i64);
        let nargs_val = self.b.ins().iconst(types::I32, nargs as i64);
        let stub_call = self
            .b
            .ins()
            .call_indirect(stub_sig, stub_addr, &[fn_addr, buf, nargs_val]);
        let stub_vals: Vec<BlockArg> = match ret_ty {
            None => vec![],
            Some(t) => {
                let raw = self.b.inst_results(stub_call)[0];
                let decoded = if t == types::F64 {
                    self.b.ins().bitcast(types::F64, MemFlags::new(), raw)
                } else if t == types::F32 {
                    let wide = self.b.ins().bitcast(types::F64, MemFlags::new(), raw);
                    self.b.ins().fdemote(types::F32, wide)
                } else if t.bits() < 64 {
                    self.b.ins().ireduce(t, raw)
                } else {
                    raw
                };
                vec![BlockArg::Value(decoded)]
            }
        };
        self.b.ins().jump(merge_bb, &stub_vals);

        self.b.switch_to_block(merge_bb);
        Ok(ret_ty.map(|_| self.b.block_params(merge_bb)[0]))
    }

    /// Virtual dispatch through the receiver's vtable slot.
    ///
    /// `field` is the `vobj_proto` slot index directly — not an index into the
    /// proto array. The proto array is only consulted to find the entry whose
    /// `pindex` matches, because that entry names the findex whose declared
    /// type gives the call signature. Getting this backwards once produced
    /// reads at `vobj_proto[-1]`; see the note in MEMORY.md.
    ///
    /// Layout: `obj->t` at offset 0, `t->vobj_proto` at offset 16,
    /// `vobj_proto[slot]` — the same three loads the LLVM tier emits.
    fn emit_call_method(&mut self, dst: ValueId, field: usize, args: &[ValueId]) -> Result<()> {
        let recv = *args
            .first()
            .ok_or_else(|| anyhow!("CallMethod with no receiver"))?;
        let obj_type_idx = self.f.value_ty(recv).0 as usize;
        let bytecode = self.ctx.bytecode();
        let kind = self.ctx.type_kind(obj_type_idx)?;
        if kind != hl::hl_type_kind_HOBJ && kind != hl::hl_type_kind_HSTRUCT {
            // HVIRTUAL dispatch needs the hash-based resolver and a runtime
            // kind check; that is what the LLVM tier is for.
            bail!("CallMethod on type kind {kind}");
        }

        // Walk the super chain: a call often resolves to an ancestor's method.
        let findex = {
            let mut found: Option<usize> = None;
            let mut cur = bytecode.types[obj_type_idx].obj.as_ref();
            while let Some(obj) = cur {
                if let Some(p) = obj.proto.iter().find(|p| p.pindex as usize == field) {
                    found = Some(p.findex as usize);
                    break;
                }
                cur = obj
                    .super_
                    .as_ref()
                    .and_then(|s| bytecode.types[s.0].obj.as_ref());
            }
            found.ok_or_else(|| {
                anyhow!("CallMethod: no proto entry with pindex {field} on type {obj_type_idx}")
            })?
        };

        let fi = self
            .ctx
            .func_index(findex)
            .ok_or_else(|| anyhow!("vtable slot {field} names findex {findex}, not a function"))?;
        let tf = bytecode.types[bytecode.functions[fi].type_.0]
            .fun
            .as_ref()
            .ok_or_else(|| anyhow!("method findex {findex} has no function type"))?;
        if args.len() != tf.args.len() {
            bail!("CallMethod arity mismatch for findex {findex}");
        }

        let param_classes: Vec<AbiClass> = tf
            .args
            .iter()
            .map(|a| abi_class(bytecode.types[a.0].kind))
            .collect();
        let ret_class = abi_class(bytecode.types[tf.ret.0].kind);

        let mut arg_vals = Vec::with_capacity(args.len());
        for (idx, v) in args.iter().enumerate() {
            let want = param_classes[idx]
                .clif_type()
                .ok_or_else(|| anyhow!("void parameter"))?;
            let raw = self.get(*v)?;
            arg_vals.push(self.coerce(raw, want)?);
        }

        let obj = self.get(recv)?;
        let type_ptr = self.b.ins().load(types::I64, MemFlags::trusted(), obj, 0);
        let proto = self
            .b
            .ins()
            .load(types::I64, MemFlags::trusted(), type_ptr, 16);
        let method = self.b.ins().load(
            types::I64,
            MemFlags::trusted(),
            proto,
            (field * std::mem::size_of::<usize>()) as i32,
        );

        // Vtable slots can hold interpreter stub sentinels exactly like
        // `functions_ptrs` entries do, so the same guard applies.
        let result = self.stub_guarded_indirect(method, &arg_vals, &param_classes, ret_class)?;
        if let Some(v) = result {
            let v = if ret_class == AbiClass::Bool {
                self.b.ins().band_imm(v, 1)
            } else {
                v
            };
            self.def(dst, v)?;
        }
        Ok(())
    }

    // ── Terminators ─────────────────────────────────────────────────────────

    /// The arguments block `to` expects from predecessor `from`, in the order
    /// its phis are listed.
    fn phi_args(&mut self, from: BlockId, to: BlockId) -> Result<Vec<BlockArg>> {
        let n = self.f.blocks[to.idx()].phis.len();
        let mut out = Vec::with_capacity(n);
        for pi in 0..n {
            let phi = &self.f.blocks[to.idx()].phis[pi];
            let dst = phi.dst;
            if self.is_void(dst) {
                continue; // void phis have no block parameter
            }
            let src = phi
                .incoming
                .iter()
                .find(|(p, _)| *p == from)
                .map(|(_, v)| *v)
                .ok_or_else(|| {
                    anyhow!(
                        "phi in b{} has no incoming value for predecessor b{}",
                        to.0,
                        from.0
                    )
                })?;
            let want = self.value_clif_ty(dst)?;
            let raw = self.get(src)?;
            let v = self.coerce(raw, want)?;
            out.push(BlockArg::Value(v));
        }
        Ok(out)
    }

    fn clif_block(&self, b: BlockId) -> Result<Block> {
        self.blocks
            .get(b.idx())
            .copied()
            .flatten()
            .ok_or_else(|| anyhow!("branch to unreachable block b{}", b.0))
    }

    fn emit_term(&mut self, bid: BlockId, term: &Terminator) -> Result<()> {
        match term {
            Terminator::Ret { value } => self.emit_ret(*value),

            Terminator::Jump { target } => {
                let args = self.phi_args(bid, *target)?;
                let t = self.clif_block(*target)?;
                self.b.ins().jump(t, &args);
                Ok(())
            }

            Terminator::CondJump {
                cond,
                a,
                b,
                if_true,
                if_false,
            } => {
                let c = self.emit_cond(*cond, *a, *b)?;
                // Both argument lists are computed before the branch, in this
                // block, which dominates both successors.
                let t_args = self.phi_args(bid, *if_true)?;
                let f_args = self.phi_args(bid, *if_false)?;
                let (tb, fb) = (self.clif_block(*if_true)?, self.clif_block(*if_false)?);
                match c {
                    Cond::Value(v) => {
                        self.b.ins().brif(v, tb, &t_args, fb, &f_args);
                    }
                    // A condition the type system settles statically, e.g.
                    // `JNull` on a non-pointer.
                    Cond::Always(true) => {
                        self.b.ins().jump(tb, &t_args);
                    }
                    Cond::Always(false) => {
                        self.b.ins().jump(fb, &f_args);
                    }
                }
                Ok(())
            }

            Terminator::Switch {
                value,
                targets,
                default,
            } => self.emit_switch(bid, *value, targets, *default),

            Terminator::Throw { .. } | Terminator::Rethrow { .. } | Terminator::Trap { .. } => {
                bail!("exception terminator reached the emitter")
            }
        }
    }

    fn emit_ret(&mut self, value: ValueId) -> Result<()> {
        if self.ret_class == AbiClass::Void {
            self.b.ins().return_(&[]);
            return Ok(());
        }
        let want = self
            .ret_class
            .clif_type()
            .ok_or_else(|| anyhow!("void return class with a value"))?;
        let src_class = self.class_of(value)?;
        let v = self.get(value)?;
        // Sub-word results are zero-extended, not sign-extended: the
        // interpreter reads the raw word and tests `!= 0` (HBOOL) or
        // truncates (HUI8/HUI16).
        let v = if matches!(src_class, AbiClass::Bool | AbiClass::I8 | AbiClass::I16) {
            let have = self.b.func.dfg.value_type(v);
            if have.bits() < want.bits() {
                self.b.ins().uextend(want, v)
            } else {
                self.coerce(v, want)?
            }
        } else {
            self.coerce(v, want)?
        };
        self.b.ins().return_(&[v]);
        Ok(())
    }

    fn emit_switch(
        &mut self,
        bid: BlockId,
        value: ValueId,
        targets: &[BlockId],
        default: BlockId,
    ) -> Result<()> {
        let raw = self.get(value)?;
        let idx = self.coerce(raw, types::I32)?;

        // Jump-table entries carry no arguments, so any target with phis gets
        // a trampoline that jumps on with them. The arguments are computed
        // here, in the block that dominates every trampoline.
        let mut entries: Vec<Block> = Vec::with_capacity(targets.len());
        let mut trampolines: Vec<(Block, Block, Vec<BlockArg>)> = Vec::new();
        let mut route = |cg: &mut Self,
                         to: BlockId,
                         tramps: &mut Vec<(Block, Block, Vec<BlockArg>)>|
         -> Result<Block> {
            let args = cg.phi_args(bid, to)?;
            let dest = cg.clif_block(to)?;
            if args.is_empty() {
                return Ok(dest);
            }
            let tramp = cg.b.create_block();
            tramps.push((tramp, dest, args));
            Ok(tramp)
        };

        for t in targets {
            let blk = route(self, *t, &mut trampolines)?;
            entries.push(blk);
        }
        let def_blk = route(self, default, &mut trampolines)?;

        let calls: Vec<BlockCall> = entries
            .iter()
            .map(|blk| BlockCall::new(*blk, [], &mut self.b.func.dfg.value_lists))
            .collect();
        let def_call = BlockCall::new(def_blk, [], &mut self.b.func.dfg.value_lists);
        let jt = self
            .b
            .create_jump_table(JumpTableData::new(def_call, &calls));
        self.b.ins().br_table(idx, jt);

        for (tramp, dest, args) in trampolines {
            self.b.switch_to_block(tramp);
            self.b.ins().jump(dest, &args);
        }
        Ok(())
    }

    /// A branch condition, or the constant the type system already settles.
    fn emit_cond(&mut self, kind: CondKind, a: ValueId, b: Option<ValueId>) -> Result<Cond> {
        let class = self.class_of(a)?;
        let va = self.get(a)?;

        if kind.is_unary() {
            return Ok(match kind {
                CondKind::True => Cond::Value(va),
                CondKind::False => {
                    let z = self.b.ins().icmp_imm(IntCC::Equal, va, 0);
                    Cond::Value(z)
                }
                // Non-pointer values are never null, so the branch is decided
                // without a comparison.
                CondKind::Null if class != AbiClass::Ptr => Cond::Always(false),
                CondKind::NotNull if class != AbiClass::Ptr => Cond::Always(true),
                CondKind::Null => {
                    let z = self.b.ins().icmp_imm(IntCC::Equal, va, 0);
                    Cond::Value(z)
                }
                CondKind::NotNull => {
                    let z = self.b.ins().icmp_imm(IntCC::NotEqual, va, 0);
                    Cond::Value(z)
                }
                _ => unreachable!("is_unary covers exactly these"),
            });
        }

        let b = b.ok_or_else(|| anyhow!("binary condition {kind:?} without a second operand"))?;
        let vb = self.get(b)?;
        let ta = self.b.func.dfg.value_type(va);
        let tb = self.b.func.dfg.value_type(vb);
        if ta != tb {
            bail!("mismatched comparison operands {ta}/{tb}");
        }

        // `NotLt` / `NotGte` are rewritten to `>=` / `<` — which is what the
        // interpreter and the LLVM tier both do, so all three agree. It is
        // NOT what HashLink means by them: `!(a < b)` is true when either
        // operand is NaN, and `a >= b` is false. Fixing it in one engine
        // alone would break cross-tier parity, so it is recorded in
        // BACKLOG.md as a single change to all three.
        let (icc, fcc) = match kind {
            CondKind::SLt => (IntCC::SignedLessThan, FloatCC::LessThan),
            CondKind::SGte => (
                IntCC::SignedGreaterThanOrEqual,
                FloatCC::GreaterThanOrEqual,
            ),
            CondKind::SGt => (IntCC::SignedGreaterThan, FloatCC::GreaterThan),
            CondKind::SLte => (IntCC::SignedLessThanOrEqual, FloatCC::LessThanOrEqual),
            CondKind::ULt => (IntCC::UnsignedLessThan, FloatCC::LessThan),
            CondKind::UGte => (
                IntCC::UnsignedGreaterThanOrEqual,
                FloatCC::GreaterThanOrEqual,
            ),
            CondKind::NotLt => (
                IntCC::SignedGreaterThanOrEqual,
                FloatCC::GreaterThanOrEqual,
            ),
            CondKind::NotGte => (IntCC::SignedLessThan, FloatCC::LessThan),
            CondKind::Eq => (IntCC::Equal, FloatCC::Equal),
            CondKind::NotEq => (IntCC::NotEqual, FloatCC::NotEqual),
            _ => unreachable!("unary conditions returned above"),
        };

        let cond = if ta.is_float() {
            self.b.ins().fcmp(fcc, va, vb)
        } else if class == AbiClass::Ptr {
            // Boxed values compare by content through hlp_dyn_compare; every
            // other pointer kind compares by identity — the same split the
            // LLVM tier makes.
            let hl_kind = self.ctx.type_kind(self.f.value_ty(a).0 as usize)?;
            if hl_kind == hl::hl_type_kind_HDYN || hl_kind == hl::hl_type_kind_HNULL {
                let addr = self.ctx.dyn_compare_addr()?;
                let sig = self.helper_sigref(&[types::I64, types::I64], Some(types::I32));
                let callee = self.b.ins().iconst(types::I64, addr as i64);
                let call = self.b.ins().call_indirect(sig, callee, &[va, vb]);
                let res = self.b.inst_results(call)[0];
                self.b.ins().icmp_imm(icc, res, 0)
            } else {
                self.b.ins().icmp(icc, va, vb)
            }
        } else {
            self.b.ins().icmp(icc, va, vb)
        };
        Ok(Cond::Value(cond))
    }
}

/// [`air::v2::ir::IntrinsicKind`] → the backend emitter's own enum. A match
/// so a new kind fails the build here instead of silently declining.
fn intrinsic_to_native(k: air::v2::ir::IntrinsicKind) -> crate::intrinsics::NativeIntrinsic {
    use air::v2::ir::IntrinsicKind as K;
    use crate::intrinsics::NativeIntrinsic as NI;
    match k {
        K::Sqrt => NI::Sqrt,
        K::Abs => NI::Abs,
        K::Floor => NI::Floor,
        K::Ceil => NI::Ceil,
        K::RoundHalfUp => NI::RoundHalfUp,
        K::FloorToI32 => NI::FloorToI32,
        K::CeilToI32 => NI::CeilToI32,
        K::RoundHalfUpToI32 => NI::RoundHalfUpToI32,
        K::IsNaN => NI::IsNaN,
        K::IsFinite => NI::IsFinite,
        K::PtrCompare => unreachable!("two-arg kinds are emitted inline at the call site"),
    }
}

/// A lowered branch condition.
enum Cond {
    Value(Value),
    /// Decided at compile time — a null test on a value that cannot be null.
    Always(bool),
}
