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
//! The emitter is exhaustive over AIR V2's instruction and terminator enums.
//! [`reject_reason`] remains as a compile-time tripwire: adding a new AIR V2
//! instruction requires classifying it before this backend will build.

use anyhow::{anyhow, bail, Context, Result};
use std::collections::HashMap;

use beadie::CraneliftFunctionDef;
use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::{
    types, AbiParam, Block, BlockArg, BlockCall, FuncRef, InstBuilder, JumpTableData, MemFlagsData,
    SigRef, Signature, StackSlot, StackSlotData, StackSlotKind, Type, Value,
};
use cranelift_frontend::FunctionBuilder;

use air::v2::ir::{
    BinOp, BlockId, CastKind, CellId, CondKind, Function as AirFunction, Instr, MemAccess,
    Terminator, TypeRef as AirTypeRef, UnOp, ValueId,
};

use super::backend::{AshCraneliftBackend, CraneliftTierContext, DynShape};
use super::lower::LoweredFunction;
use super::{abi_class, argument_abi_class, entry_return_class, AbiClass};
use crate::hl_bindings as hl;
use crate::llvm::stub_bridge::{ash_jit_call_stub, ash_jit_resolve_stub, STUB_SENTINEL_LIMIT};

// Cranelift has no floating remainder instruction. Keeping these helpers in
// Rust avoids depending on a platform-specific libm symbol name while still
// giving generated code the exact IEEE remainder operation Rust/LLVM use.
extern "C" fn ash_fmod_f32(a: f32, b: f32) -> f32 {
    a % b
}

extern "C" fn ash_fmod_f64(a: f64, b: f64) -> f64 {
    a % b
}

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
        Instr::VecLoad { .. }
        | Instr::VecStore { .. }
        | Instr::VecSplat { .. }
        | Instr::VecBinOp { .. }
        | Instr::VecReduce { .. }
        | Instr::Param { .. }
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
        | Instr::CellRef { .. }
        // Lowered here since the closure family landed; see
        // `emit_static_closure` / `emit_call_closure`.
        | Instr::StaticClosure { .. }
        | Instr::InstanceClosure { .. }
        | Instr::CallClosure { .. }
        // The name is a bytecode string constant, so its hash is a compile-
        // time constant and the accessor is picked by the value's declared
        // kind; see `emit_dyn_get` / `emit_dyn_set`.
        | Instr::DynGet { .. }
        | Instr::DynSet { .. }
        // The enum family. A venum's payload layout is the loader's, and AIR
        // carries the construct on every instruction that needs one, so all
        // five are `hlp_alloc_enum` plus stores and loads at known offsets;
        // see `emit_enum_alloc` / `enum_param`.
        | Instr::MakeEnum { .. }
        | Instr::EnumAlloc { .. }
        | Instr::EnumIndex { .. }
        | Instr::EnumField { .. }
        | Instr::SetEnumField { .. }
        // Constant-pool loads and header reads: no allocation, no runtime
        // lookup. `GetType` / `GetTID` still decline the source kinds the
        // two reference tiers read differently — see their arms.
        | Instr::Bytes { .. }
        | Instr::GetType { .. }
        | Instr::GetTID { .. }
        | Instr::RefOffset { .. }
        | Instr::Assert
        | Instr::Prefetch { .. }
        | Instr::Pos { .. } => None,

        Instr::Cast { kind, .. } => match kind {
            CastKind::ToSFloat | CastKind::ToUFloat | CastKind::ToInt | CastKind::UnsafeCast => {
                None
            }
            // Helper-backed now; `emit_cast` still declines the one shape
            // it will not guess at (SafeCast unboxing into a primitive).
            CastKind::ToDyn | CastKind::SafeCast | CastKind::ToVirtual => None,
        },

        Instr::VirtualClosure { .. }
        | Instr::EndTrap { .. }
        | Instr::RefData { .. }
        | Instr::Asm { .. } => None,
    }
}

/// Kinds whose run-time values begin with the `hl_type*` read by OGetType.
/// Raw pointer families such as bytes, refs, abstracts, and type pointers do
/// not have that header. Closures and arrays do; omitting them made valid AIR
/// V2 functions decline even though their layout is unambiguous.
fn header_typed(kind: hl::hl_type_kind) -> bool {
    matches!(
        kind,
        hl::hl_type_kind_HDYN
            | hl::hl_type_kind_HFUN
            | hl::hl_type_kind_HOBJ
            | hl::hl_type_kind_HARRAY
            | hl::hl_type_kind_HVIRTUAL
            | hl::hl_type_kind_HENUM
            | hl::hl_type_kind_HDYNOBJ
            | hl::hl_type_kind_HNULL
            | hl::hl_type_kind_HMETHOD
    )
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
    // Re-tier exits: AIR block id of an OSR-eligible loop header -> address
    // of a leaked AtomicU64 slot. The header polls the slot; when the broker
    // publishes an LLVM OSR entry address there, the frame spills its
    // register image into a stack slot of its own and tail-calls the entry.
    // Empty map (and 0 registers) compiles the function with no exits.
    osr_exits: &HashMap<u32, u64>,
    osr_image_regs: usize,
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

    let arg_kinds: Vec<hl::hl_type_kind> =
        tf.args.iter().map(|a| bytecode.types[a.0].kind).collect();
    let ret_kind = bytecode.types[tf.ret.0].kind;

    let mut sig = backend.make_signature();
    for a in &tf.args {
        let ty = argument_abi_class(ctx.type_kind(a.0)?)
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

    // Capture before `builder()` takes a mutable borrow of `def`.
    let fcfg = def.frontend_config();
    {
        let mut cg = AirCodegen {
            ctx,
            f: air,
            findex,
            b: def.builder(),
            fcfg,
            vals: vec![None; air.values.len()],
            use_counts: air.use_counts(),
            cells: Vec::new(),
            blocks: vec![None; air.blocks.len()],
            native_refs,
            nargs: tf.args.len(),
            ret_class: entry_return_class(ret_kind),
            osr_exits,
            osr_image_regs,
            osr_image_slot: None,
            fiber_poll_epoch_slot: None,
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

    // The frame that enters here is exactly the one a later LLVM promote
    // wants to lift out, so the entry polls the same re-tier slots the
    // function's ordinary compile allocated.
    let (osr_exits, osr_image_regs) = super::air::retier_state_for(findex, &opt.ser.block_pcs);
    // Capture before `builder()` takes a mutable borrow of `def`.
    let fcfg = def.frontend_config();
    {
        let mut cg = AirCodegen {
            ctx,
            f: air,
            findex,
            b: def.builder(),
            fcfg,
            vals: vec![None; air.values.len()],
            use_counts: air.use_counts(),
            cells: Vec::new(),
            blocks: vec![None; air.blocks.len()],
            native_refs,
            nargs: 0, // parameters are dead in an OSR body; values come from buf
            ret_class: entry_return_class(ret_kind),
            osr_exits: &osr_exits,
            osr_image_regs,
            osr_image_slot: None,
            fiber_poll_epoch_slot: None,
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

/// Compile a uniform-ABI entry for `findex`: `extern "C" fn(*const i64) -> i64`.
///
/// Rust cannot synthesize a call whose signature it only learns at runtime, so
/// the interpreter's compiled-call bridge is a ladder of fixed arities and
/// refuses anything wider. Cranelift knows the exact signature here, so it
/// emits the unpacking and an ABI-correct call instead — registers or stack,
/// whichever the platform calls for, at any arity.
///
/// The buffer encoding is the inverse of the one `stub_guarded_indirect` uses
/// when compiled code calls *out* through the stub: one 8-byte word per
/// argument, floats as their f64 bit pattern, narrower integers zero-extended.
/// `target` is the address of the already-compiled natural-ABI entry.
pub fn compile_uniform_entry(
    backend: &AshCraneliftBackend,
    ctx: &CraneliftTierContext,
    bead: &std::sync::Arc<beadie::Bead>,
    findex: usize,
    target: usize,
) -> Result<usize> {
    let bytecode = ctx.bytecode();
    let func_idx = ctx
        .func_index(findex)
        .ok_or_else(|| anyhow!("findex {findex} is not a bytecode function"))?;
    let func = &bytecode.functions[func_idx];
    let tf = bytecode.types[func.type_.0]
        .fun
        .as_ref()
        .ok_or_else(|| anyhow!("no function type"))?;
    let ret_class = entry_return_class(bytecode.types[tf.ret.0].kind);

    let mut sig = backend.make_signature();
    sig.params.push(AbiParam::new(types::I64)); // the argument buffer
    sig.returns.push(AbiParam::new(types::I64)); // always one raw word
    let name = backend.unique_name(findex, "uniform");
    let mut def = backend
        .new_def(sig, &name)
        .map_err(|e| anyhow!("declare_function({name}): {e}"))?;

    // The callee's real signature, built from the same classes the ordinary
    // entry is compiled with, so the two agree by construction.
    let mut target_sig = backend.make_signature();
    let mut arg_types = Vec::with_capacity(tf.args.len());
    for a in &tf.args {
        let ty = argument_abi_class(ctx.type_kind(a.0)?)
            .clif_type()
            .ok_or_else(|| anyhow!("void argument in uniform entry signature"))?;
        target_sig.params.push(AbiParam::new(ty));
        arg_types.push(ty);
    }
    if let Some(ty) = ret_class.clif_type() {
        target_sig.returns.push(AbiParam::new(ty));
    }

    // Capture before `builder()` takes a mutable borrow of `def`.
    let fcfg = def.frontend_config();
    {
        let mut b = def.builder();
        let sig_ref = b.import_signature(target_sig);
        let entry = b.create_block();
        b.append_block_params_for_function_params(entry);
        b.switch_to_block(entry);
        b.seal_block(entry);
        let buf = b.block_params(entry)[0];

        let mut args = Vec::with_capacity(arg_types.len());
        for (i, &ty) in arg_types.iter().enumerate() {
            let off = (i * 8) as i32;
            let v = if ty == types::F32 {
                // Packed as an f64 bit pattern, matching the outbound path.
                let raw = b.ins().load(types::I64, MemFlagsData::trusted(), buf, off);
                let wide = b.ins().bitcast(types::F64, MemFlagsData::new(), raw);
                b.ins().fdemote(types::F32, wide)
            } else {
                // F64 and every integer width load directly: the word holds
                // the f64 bits, and a narrow integer sits in the low bytes.
                b.ins().load(ty, MemFlagsData::trusted(), buf, off)
            };
            args.push(v);
        }

        let addr = b.ins().iconst(types::I64, target as i64);
        let call = b.ins().call_indirect(sig_ref, addr, &args);
        let out = match b.inst_results(call).first().copied() {
            None => b.ins().iconst(types::I64, 0),
            Some(r) => {
                let ty = b.func.dfg.value_type(r);
                if ty == types::F64 {
                    b.ins().bitcast(types::I64, MemFlagsData::new(), r)
                } else if ty == types::F32 {
                    let wide = b.ins().fpromote(types::F64, r);
                    b.ins().bitcast(types::I64, MemFlagsData::new(), wide)
                } else if ty.bits() < 64 {
                    b.ins().uextend(types::I64, r)
                } else {
                    r
                }
            }
        };
        b.ins().return_(&[out]);
        b.finalize(fcfg);
    }

    if super::lower::clif_dump_wanted(findex) {
        eprintln!(
            "=== CLIF (uniform entry) findex={findex} nargs={} ===\n{}",
            arg_types.len(),
            def.ctx.func.display()
        );
    }

    let code = backend
        .compile_def(bead, def)
        .map_err(|e| anyhow!("uniform entry compile: {e}"))?;
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
            let ty = argument_abi_class(ctx.type_kind(a.0 as usize)?)
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
    findex: usize,
    b: FunctionBuilder<'b>,
    /// Captured from the def before `builder()` borrows it; `finalize` needs
    /// it to size pointer-width operations.
    fcfg: cranelift_codegen::isa::TargetFrontendConfig,
    /// One CLIF value per AIR value. Most values use their declared machine
    /// representation. An HVOID value that survives de-SSA bookkeeping uses
    /// a zero I64 word; see `has_machine_value`.
    vals: Vec<Option<Value>>,
    /// HVOID normally has no machine representation, but de-SSA cell writes
    /// can still read the destination of a void call. Those bookkeeping uses
    /// receive a zero word so AIR storage remains defined.
    use_counts: Vec<usize>,
    /// One stack slot per AIR cell.
    cells: Vec<StackSlot>,
    /// CLIF block per *reachable* AIR block. Unreachable AIR blocks get none;
    /// `cranelift_frontend` rejects a block that is created and never filled,
    /// and an unreachable block has nothing to fill it with.
    blocks: Vec<Option<Block>>,
    native_refs: HashMap<usize, FuncRef>,
    nargs: usize,
    ret_class: AbiClass,
    /// See [`lower_air_function`]: loop-header re-tier exits.
    osr_exits: &'a HashMap<u32, u64>,
    /// Slots a spilled register image needs. The image itself lives in
    /// [`Self::osr_image_slot`], per frame — see [`RetierState`].
    osr_image_regs: usize,
    /// Lazily created stack slot holding this frame's spilled image.
    osr_image_slot: Option<StackSlot>,
    /// Last runtime poll generation handled by this compiled activation.
    fiber_poll_epoch_slot: Option<StackSlot>,
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
                if !self.has_machine_value(dst) {
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

        let poll_headers = self.fiber_poll_headers();
        if poll_headers.iter().any(|poll| *poll) {
            self.init_fiber_poll_epoch()?;
        }

        // Re-tier exits: each participating loop header gets a body block the
        // poll falls through to, and a cold exit block that hands the frame
        // to the LLVM OSR entry. The register image a header must spill is
        // decided by dominance, so the tree is built once, lazily.
        let dom_cfg = if self.osr_exits.is_empty() {
            None
        } else {
            Some(air::v2::CfgInfo::build(self.f))
        };

        for &bid in &order {
            let blk = self.blocks[bid.idx()].expect("block in order has a CLIF block");
            if bid.0 != 0 {
                self.b.switch_to_block(blk);
            }
            if let (Some(&slot), Some(cfg)) = (self.osr_exits.get(&bid.0), dom_cfg.as_ref()) {
                self.emit_retier_poll(bid, slot, cfg)?;
            }
            if poll_headers[bid.idx()] {
                self.emit_fiber_poll()?;
            }
            for ii in 0..self.f.blocks[bid.idx()].instrs.len() {
                let instr = self.f.blocks[bid.idx()].instrs[ii].clone();
                self.emit(&instr)
                    .with_context(|| format!("AIR b{} instruction {}: {instr:?}", bid.0, ii))?;
            }
            let term = self.f.blocks[bid.idx()].term.clone();
            self.emit_term(bid, &term)
                .with_context(|| format!("AIR b{} terminator: {term:?}", bid.0))?;
        }
        self.b.seal_all_blocks();
        Ok(())
    }

    /// Poll the re-tier slot at a loop header; on a published LLVM OSR entry,
    /// spill the register image and tail into it.
    ///
    /// The image is the value each serialized register holds at the header:
    /// the header's own phis first (they are the live-in joins), then the
    /// nearest dominating definition of every other register. Registers with
    /// no dominating definition are left as whatever the per-function buffer
    /// already holds — the entry restores every register, but the compiled
    /// region only reads the live ones, and a live register always has a
    /// dominating definition or a header phi.
    ///
    /// Single-threaded by design, like the interpreter that feeds this tier:
    /// the spill buffer is one leaked allocation per compiled function.
    /// This frame's register-image spill slot, created on first use.
    ///
    /// Sized from the function's register count so `reg * 8` addressing
    /// matches what the OSR entry loads.
    fn osr_image_slot(&mut self) -> StackSlot {
        if let Some(slot) = self.osr_image_slot {
            return slot;
        }
        let bytes = (self.osr_image_regs.max(1) * 8) as u32;
        let slot = self.b.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            bytes,
            3,
        ));
        self.osr_image_slot = Some(slot);
        slot
    }

    fn emit_retier_poll(
        &mut self,
        header: BlockId,
        slot: u64,
        cfg: &air::v2::CfgInfo,
    ) -> Result<()> {
        let body = self.b.create_block();
        let exit = self.b.create_block();
        self.b.set_cold_block(exit);

        let slot_addr = self.b.ins().iconst(types::I64, slot as i64);
        // A PLAIN load, deliberately: an acquire load is a barrier, and one
        // barrier per iteration serialized mandelbrot's FP pipeline (371ms
        // -> 619ms measured). A naturally-aligned pointer-width load is
        // indivisible on both targets, the publisher stores a pointer to
        // code that was finalized before the store, and the consumer's only
        // use of the value is as a branch target — an address dependency,
        // which no reordering can break. Missing a publication for a few
        // iterations is also harmless: the next iteration takes the exit.
        let target = self
            .b
            .ins()
            .load(types::I64, MemFlagsData::trusted(), slot_addr, 0);
        self.b.ins().brif(target, exit, &[], body, &[]);

        // ---- cold exit: spill the image, call the entry, return ----------
        self.b.switch_to_block(exit);

        let mut image: HashMap<u32, ValueId> = HashMap::new();
        for phi in &self.f.blocks[header.idx()].phis {
            if self.has_machine_value(phi.dst) {
                image.entry(self.f.value_reg(phi.dst)).or_insert(phi.dst);
            }
        }
        let mut b = header.idx();
        while cfg.dom.idom[b] != b {
            b = cfg.dom.idom[b];
            for ins in self.f.blocks[b].instrs.iter().rev() {
                if let Some(d) = ins.dst() {
                    if self.has_machine_value(d) && self.vals[d.idx()].is_some() {
                        image.entry(self.f.value_reg(d)).or_insert(d);
                    }
                }
            }
            for phi in &self.f.blocks[b].phis {
                if self.has_machine_value(phi.dst) {
                    image.entry(self.f.value_reg(phi.dst)).or_insert(phi.dst);
                }
            }
        }

        // Deterministic emission order.
        let mut spill: Vec<(u32, ValueId)> = image.into_iter().collect();
        spill.sort_unstable_by_key(|&(r, _)| r);
        // The image belongs to THIS frame. It used to be one leaked buffer per
        // findex, which every activation on every thread spilled into: with
        // more than one VM worker, two fibers running the same function raced
        // on the same addresses and one entered the OSR body carrying the
        // other's live registers.
        let image = self.osr_image_slot();
        for (reg, vid) in spill {
            let v = self.get(vid)?;
            // Narrow stores leave the slot's high bytes stale; the entry
            // truncates every load to the register's width, so that is fine.
            self.b
                .ins()
                .stack_store(types::I64, v, image, (u32::from(reg) * 8) as i32);
        }

        let mut call_sig = Signature::new(self.ctx.call_conv());
        call_sig.params.push(AbiParam::new(types::I64));
        if let Some(ty) = self.ret_class.clif_type() {
            call_sig.returns.push(AbiParam::new(ty));
        }
        let sig_ref = self.b.import_signature(call_sig);
        let buf_addr = self.b.ins().stack_addr(types::I64, image, 0);
        let call = self.b.ins().call_indirect(sig_ref, target, &[buf_addr]);
        let results: Vec<Value> = self.b.inst_results(call).to_vec();
        match results.first() {
            Some(&r) => {
                self.b.ins().return_(&[r]);
            }
            None => {
                self.b.ins().return_(&[]);
            }
        }

        // ---- warm fall-through -------------------------------------------
        self.b.switch_to_block(body);
        Ok(())
    }

    /// Consume the builder — this is what runs cranelift-frontend's own
    /// block and seal invariant checks.
    fn finish(self) {
        self.b.finalize(self.fcfg);
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
                if !self.has_machine_value(dst) {
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

        let poll_headers = self.fiber_poll_headers();
        if poll_headers.iter().any(|poll| *poll) {
            self.init_fiber_poll_epoch()?;
        }

        // Cells are registers too; their current values are in the buffer.
        for ci in 0..self.f.cells.len() {
            let cell = self.f.cells[ci].clone();
            let ty = self.cell_clif_ty(cell.ty)?;
            let slot = self.b.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                8,
                3,
            ));
            self.cells.push(slot);
            let v = self.load_osr_slot(buf, cell.reg, ty)?;
            self.b.ins().stack_store(types::I64, v, slot, 0);
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
                if !self.has_machine_value(vid) {
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
            if !self.has_machine_value(phi.dst) {
                continue; // void phis have no block parameter
            }
            let ty = self.value_clif_ty(phi.dst)?;
            let reg = self.f.value_reg(phi.dst);
            let v = self.load_osr_slot(buf, reg, ty)?;
            args.push(BlockArg::Value(v));
        }
        let hblk = self.blocks[header].expect("header emitted");
        self.b.ins().jump(hblk, &args);

        let dom_cfg = if self.osr_exits.is_empty() {
            None
        } else {
            Some(air::v2::CfgInfo::build(self.f))
        };
        for &bid in &order {
            let blk = self.blocks[bid.idx()].expect("block in order has a CLIF block");
            self.b.switch_to_block(blk);
            if let (Some(&slot), Some(cfg)) = (self.osr_exits.get(&bid.0), dom_cfg.as_ref()) {
                self.emit_retier_poll(bid, slot, cfg)?;
            }
            if poll_headers[bid.idx()] {
                self.emit_fiber_poll()?;
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

    /// Mark every natural AIR V2 loop header. Polling all headers, rather
    /// than only loops that contain calls, is required for a tight numeric
    /// loop to make progress alongside timers and sibling Haxe threads.
    fn fiber_poll_headers(&self) -> Vec<bool> {
        let cfg = air::v2::CfgInfo::build(self.f);
        let loops = air::v2::LoopForest::analyze(self.f, &cfg);
        let mut headers = vec![false; self.f.blocks.len()];
        for lp in &loops.loops {
            headers[lp.header.idx()] = true;
        }
        headers
    }

    fn init_fiber_poll_epoch(&mut self) -> Result<()> {
        let slot = self.b.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            8,
            3,
        ));
        let epoch_addr = self
            .b
            .ins()
            .iconst(types::I64, self.ctx.fiber_poll_epoch_address()? as i64);
        let initial = self
            .b
            .ins()
            .load(types::I64, MemFlagsData::new(), epoch_addr, 0);
        self.b.ins().stack_store(types::I64, initial, slot, 0);
        self.fiber_poll_epoch_slot = Some(slot);
        Ok(())
    }

    /// Event-driven cooperative safe point inserted directly into the AIR V2
    /// CFG. Ordinary iterations only compare the runtime's poll generation;
    /// the cold helper edge runs when work, a timer quantum, or GC asks for it.
    fn emit_fiber_poll(&mut self) -> Result<()> {
        let slot = self
            .fiber_poll_epoch_slot
            .ok_or_else(|| anyhow!("fiber poll epoch was not initialized"))?;
        let poll = self.b.create_block();
        let body = self.b.create_block();
        self.b.set_cold_block(poll);

        let handled = self
            .b
            .ins()
            .stack_load(types::I64, types::I64, slot, 0);
        let epoch_addr = self
            .b
            .ins()
            .iconst(types::I64, self.ctx.fiber_poll_epoch_address()? as i64);
        let current = self
            .b
            .ins()
            .load(types::I64, MemFlagsData::new(), epoch_addr, 0);
        let due = self.b.ins().icmp(IntCC::NotEqual, current, handled);
        self.b.ins().brif(due, poll, &[], body, &[]);

        self.b.switch_to_block(poll);
        self.b.ins().stack_store(types::I64, current, slot, 0);
        let sig_ref = self
            .b
            .import_signature(Signature::new(self.ctx.call_conv()));
        let target = self
            .b
            .ins()
            .iconst(types::I64, self.ctx.fiber_poll_helper()? as i64);
        self.b.ins().call_indirect(sig_ref, target, &[]);
        self.b.ins().jump(body, &[]);

        self.b.switch_to_block(body);
        Ok(())
    }

    /// One typed load from the transfer buffer. Slot `reg` holds the
    /// register's value as the interpreter marshaled it: floats as raw f64
    /// bits (so an F64 load reads them back exactly), integers
    /// sign-extended, pointers as-is.
    fn load_osr_slot(&mut self, buf: Value, reg: u32, ty: Type) -> Result<Value> {
        let off = (reg as i32) * 8;
        if ty == types::F64 {
            return Ok(self
                .b
                .ins()
                .load(types::F64, MemFlagsData::trusted(), buf, off));
        }
        if ty == types::F32 {
            bail!("f32 register in an OSR frame");
        }
        let wide = self
            .b
            .ins()
            .load(types::I64, MemFlagsData::trusted(), buf, off);
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
            let ty = self.cell_clif_ty(cell.ty)?;
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
            self.b.ins().stack_store(types::I64, init, slot, 0);
        }
        Ok(())
    }

    // ── Type and value plumbing ─────────────────────────────────────────────

    fn clif_ty(&self, tr: AirTypeRef) -> Result<Type> {
        abi_class(self.ctx.type_kind(tr.0 as usize)?)
            .clif_type()
            .ok_or_else(|| anyhow!("type {} has no machine type", tr.0))
    }

    /// Trap exception registers are typed HVOID in some bytecode even though
    /// the trap edge writes a `vdynamic*` into them before the handler reads
    /// the cell. Values of HVOID still have no representation; only pinned
    /// storage needs this word-shaped escape hatch.
    fn cell_clif_ty(&self, tr: AirTypeRef) -> Result<Type> {
        Ok(abi_class(self.ctx.type_kind(tr.0 as usize)?)
            .clif_type()
            .unwrap_or(types::I64))
    }

    fn value_clif_ty(&self, v: ValueId) -> Result<Type> {
        if self.is_void(v) && self.use_counts[v.idx()] != 0 {
            return Ok(types::I64);
        }
        let lane = self.clif_ty(self.f.value_ty(v))?;
        // A vector value's machine type is its element type by its lane
        // count. `ValueData.lanes` is 1 for everything the vectorizer has not
        // widened, so this is the scalar answer everywhere else.
        match self.f.value_lanes(v) {
            0 | 1 => Ok(lane),
            n => lane.by(n as u32).ok_or_else(|| {
                anyhow!("no CLIF vector type for {n} lanes of {lane}")
            }),
        }
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

    /// HVOID is normally representation-less. AIR V2 can nevertheless keep
    /// a void call result alive long enough to write its de-SSA cell (notably
    /// around trap regions). Give only those actually-used values a raw zero
    /// word so every AIR use has a definition without changing the function
    /// ABI or the semantics of a void result.
    fn has_machine_value(&self, v: ValueId) -> bool {
        !self.is_void(v) || self.use_counts[v.idx()] != 0
    }

    fn define_void_word_if_used(&mut self, dst: ValueId) {
        if self.is_void(dst) && self.use_counts[dst.idx()] != 0 {
            let zero = self.b.ins().iconst(types::I64, 0);
            self.vals[dst.idx()] = Some(zero);
        }
    }

    fn get(&self, v: ValueId) -> Result<Value> {
        self.vals[v.idx()].ok_or_else(|| {
            anyhow!(
                "use of undefined AIR value v{} (type {}, reg {}, uses {})",
                v.0,
                self.f.value_ty(v).0,
                self.f.value_reg(v),
                self.use_counts[v.idx()]
            )
        })
    }

    /// Define `dst`, coercing to its declared machine type. Used HVOID
    /// bookkeeping destinations receive their canonical zero word.
    fn def(&mut self, dst: ValueId, v: Value) -> Result<()> {
        if self.is_void(dst) {
            self.define_void_word_if_used(dst);
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
                if !self.has_machine_value(*dst) {
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
                // Past the pool means a pass minted it; `int_at` answers for
                // both so a minted constant is indistinguishable here.
                let bc = self.ctx.bytecode();
                let val = self
                    .f
                    .int_at(*idx, |i| bc.ints.get(i).copied())
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
            // The same shape as `String`, over the raw byte blob instead of
            // the UTF-16 one. The interpreter puts `bytes_pos[idx]` — an
            // offset, not an address — in the register, so the LLVM tier's
            // constant global is the reference here.
            Instr::Bytes { dst, idx } => {
                let addr = self.ctx.bytes_ptr(*idx)?;
                let v = self.b.ins().iconst(types::I64, addr as i64);
                self.def(*dst, v)?;
            }
            Instr::Null { dst } => {
                // Haxe constructors commonly carry an unused HVOID Null, but
                // trap handlers also use an HVOID-typed null as the raw zero
                // argument of an untyped call. Materialize that latter shape
                // as one word; dropping every HVOID Null left its handler use
                // undefined (Base2D findex 4422).
                if self.is_void(*dst) {
                    self.define_void_word_if_used(*dst);
                    return Ok(());
                }
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
            Instr::Intrinsic {
                kind, dst, args, ..
            } => {
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
                    let x = self.get(*args.first().ok_or_else(|| anyhow!("intrinsic arity"))?)?;
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
                let v = self
                    .b
                    .ins()
                    .load(types::I64, MemFlagsData::trusted(), base, 0);
                self.def(*dst, v)?;
            }
            Instr::SetGlobal { global, src } => {
                if self.class_of(*src)? != AbiClass::Ptr {
                    bail!("SetGlobal from a non-pointer value");
                }
                let addr = self.ctx.global_slot_addr(*global)?;
                let base = self.b.ins().iconst(types::I64, addr as i64);
                let v = self.get(*src)?;
                self.b.ins().store(MemFlagsData::trusted(), v, base, 0);
            }

            // The object type comes from AIR, which resolved it at lowering
            // time — not from the destination register's declared type.
            Instr::FieldGet {
                dst,
                obj,
                obj_ty,
                field,
            } => {
                self.emit_field_get(*dst, *obj, *obj_ty, *field)?;
            }
            Instr::FieldSet {
                obj,
                obj_ty,
                field,
                src,
            } => {
                self.emit_field_set(*obj, *obj_ty, *field, *src)?;
            }

            Instr::VecLoad {
                kind,
                dst,
                base,
                index,
                stride,
            } => self.emit_vec_load(*kind, *dst, *base, *index, *stride)?,
            Instr::VecStore {
                kind,
                base,
                index,
                src,
                stride,
            } => self.emit_vec_store(*kind, *base, *index, *src, *stride)?,
            Instr::VecSplat { dst, src } => {
                let ty = self.value_clif_ty(*dst)?;
                let scalar = self.get(*src)?;
                let v = self.b.ins().splat(ty, scalar);
                self.def(*dst, v)?;
            }
            Instr::VecBinOp { op, dst, a, b } => self.emit_binop(*op, *dst, *a, *b)?,
            Instr::VecReduce { op, dst, src } => self.emit_vec_reduce(*op, *dst, *src)?,
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
                    MemFlagsData::trusted(),
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

            // The type a value carries at run time. Header-bearing values
            // yield word zero, raw pointer/scalar families yield their static
            // type, and a null header-bearing value is HashLink's HVOID type.
            Instr::GetType { dst, src } => {
                let src_ty = self.f.value_ty(*src).0 as usize;
                let kind = self.ctx.type_kind(src_ty)?;
                let stat = self.ctx.type_ptr(src_ty)?;
                let stat = self.b.ins().iconst(types::I64, stat as i64);
                let v = if header_typed(kind) {
                    let p = self.get(*src)?;
                    let void_ty = self
                        .ctx
                        .bytecode()
                        .types
                        .iter()
                        .position(|t| t.kind == hl::hl_type_kind_HVOID)
                        .ok_or_else(|| anyhow!("bytecode has no HVOID type"))?;
                    let void_ty = self.ctx.type_ptr(void_ty)?;
                    let void_ty = self.b.ins().iconst(types::I64, void_ty as i64);
                    self.guarded_header_load(p, types::I64, void_ty)?
                } else {
                    stat
                };
                self.def(*dst, v)?;
            }

            // `kind` is `hl_type`'s first field, so this is a single load
            // from the type pointer the opcode is handed — and it is handed
            // one: the compiler types `OGetTID`'s operand `HTYPE`. A source
            // of any other pointer kind is declined rather than guessed,
            // since the LLVM tier walks that value's header first and the
            // interpreters read its word 0 straight.
            Instr::GetTID { dst, src } => {
                let src_ty = self.f.value_ty(*src).0 as usize;
                let kind = self.ctx.type_kind(src_ty)?;
                if kind != hl::hl_type_kind_HTYPE && abi_class(kind) == AbiClass::Ptr {
                    bail!("GetTID on type kind {kind}: reference tiers disagree");
                }
                let stat = self.b.ins().iconst(types::I32, i64::from(kind));
                let v = if kind == hl::hl_type_kind_HTYPE {
                    let p = self.get(*src)?;
                    self.guarded_header_load(p, types::I32, stat)?
                } else {
                    stat
                };
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

            Instr::CallMethod { dst, field, args } => self.emit_call_method(*dst, *field, args)?,

            Instr::StaticClosure { dst, fun } => self.emit_static_closure(*dst, *fun, None)?,
            Instr::InstanceClosure { dst, fun, obj } => {
                self.emit_static_closure(*dst, *fun, Some(*obj))?
            }
            Instr::VirtualClosure { dst, obj, field } => {
                self.emit_virtual_closure(*dst, *obj, *field)?
            }
            Instr::CallClosure { dst, fun, args } => self.emit_call_closure(*dst, *fun, args)?,

            Instr::DynGet { dst, obj, field } => self.emit_dyn_get(*dst, *obj, *field)?,
            Instr::DynSet { obj, field, src } => self.emit_dyn_set(*obj, *field, *src)?,

            // venum: `t` at 0, `index` at 8, then the live construct's
            // parameters. `MakeEnum` is the allocation plus one store per
            // parameter; `EnumAlloc` is the allocation alone, with the stores
            // left to the `SetEnumField`s that follow it.
            Instr::EnumAlloc { dst, construct } => {
                let ty = self.f.value_ty(*dst);
                let v = self.emit_enum_alloc(ty, *construct)?;
                self.def(*dst, v)?;
            }
            Instr::MakeEnum {
                dst,
                construct,
                args,
            } => {
                let ty = self.f.value_ty(*dst);
                let e = self.emit_enum_alloc(ty, *construct)?;
                for (j, a) in args.iter().enumerate() {
                    let (off, pty) = self.enum_param(ty, *construct, j)?;
                    let raw = self.get(*a)?;
                    // Narrowed to the parameter's own width for the reason
                    // `FieldSet` narrows: the next parameter is laid out
                    // immediately after this one.
                    let v = self.coerce(raw, pty)?;
                    self.b.ins().store(MemFlagsData::trusted(), v, e, off);
                }
                self.def(*dst, e)?;
            }
            Instr::EnumIndex { dst, value } => {
                let e = self.get(*value)?;
                let v = self.b.ins().load(types::I32, MemFlagsData::trusted(), e, 8);
                self.def(*dst, v)?;
            }
            Instr::EnumField {
                dst,
                value,
                construct,
                field,
            } => {
                let (off, pty) = self.enum_param(self.f.value_ty(*value), *construct, *field)?;
                let e = self.get(*value)?;
                let v = self.b.ins().load(pty, MemFlagsData::trusted(), e, off);
                self.def(*dst, v)?;
            }
            // The construct comes off the instruction, where AIR put it once
            // at lowering time — it is the result of the same backward scan
            // the LLVM tier re-runs per opcode, so both tiers place the store
            // at the same offset.
            Instr::SetEnumField {
                value,
                construct,
                field,
                src,
            } => {
                let (off, pty) = self.enum_param(self.f.value_ty(*value), *construct, *field)?;
                let e = self.get(*value)?;
                let raw = self.get(*src)?;
                let v = self.coerce(raw, pty)?;
                self.b.ins().store(MemFlagsData::trusted(), v, e, off);
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
                let ty = self.cell_clif_ty(self.f.cells[cell.idx()].ty)?;
                let v = self.b.ins().stack_load(types::I64, ty, slot, 0);
                self.def(*dst, v)?;
            }
            Instr::CellSet { cell, src } => {
                let slot = self.cell_slot(*cell)?;
                let ty = self.cell_clif_ty(self.f.cells[cell.idx()].ty)?;
                let raw = self.get(*src)?;
                let v = self.coerce(raw, ty)?;
                self.b.ins().stack_store(types::I64, v, slot, 0);
            }
            Instr::CellIncr { cell } => self.emit_cell_step(*cell, 1)?,
            Instr::CellDecr { cell } => self.emit_cell_step(*cell, -1)?,

            // The trap terminator arms the runtime context. Reaching EndTrap
            // on the normal path pops it; the exception path was already
            // popped by `hlp_throw` before its longjmp.
            Instr::EndTrap { .. } => {
                let helper = self
                    .b
                    .ins()
                    .iconst(types::I64, self.ctx.remove_trap_helper()? as i64);
                let sig = self.helper_sigref(&[], None);
                self.b.ins().call_indirect(sig, helper, &[]);
            }

            Instr::Unref { dst, src } => {
                let ty = self.value_clif_ty(*dst)?;
                let p = self.get(*src)?;
                let v = self.b.ins().load(ty, MemFlagsData::trusted(), p, 0);
                self.def(*dst, v)?;
            }
            Instr::SetRef { r, value } => {
                let p = self.get(*r)?;
                let v = self.get(*value)?;
                self.b.ins().store(MemFlagsData::trusted(), v, p, 0);
            }

            // HashLink defines ORefOffset in elements of the destination
            // HREF's parameter type (`hl_type_size(dst->t->tparam)`), not in
            // bytes. This distinction was hidden while every Ash engine made
            // the same raw-byte mistake; pointer and i64 refs happened to be
            // the only common cases where a factor of eight exposed it.
            Instr::RefOffset { dst, base, offset } => {
                let raw = self.get(*base)?;
                let p = self.coerce(raw, types::I64)?;
                let index = self.index_as_addr(*offset)?;
                let dst_ty = self.f.value_ty(*dst).0 as usize;
                let href =
                    self.ctx.bytecode().types.get(dst_ty).ok_or_else(|| {
                        anyhow!("RefOffset destination type {dst_ty} out of range")
                    })?;
                if href.kind != hl::hl_type_kind_HREF {
                    bail!("RefOffset destination type kind {} is not HREF", href.kind);
                }
                let inner = href
                    .tparam
                    .as_ref()
                    .ok_or_else(|| anyhow!("RefOffset HREF type {dst_ty} has no parameter"))?;
                let inner_kind = self.ctx.type_kind(inner.0)?;
                let stride = crate::layout::array_elem_size(inner_kind) as i64;
                let off = if stride == 1 {
                    index
                } else {
                    self.b.ins().imul_imm(index, stride)
                };
                let v = self.b.ins().iadd(p, off);
                self.def(*dst, v)?;
            }

            // ORefData points at the first element after the varray header.
            // It does not dereference offset 8 (the former LLVM behavior),
            // which is part of the header and corrupted pointer atomics.
            Instr::RefData { dst, src } => {
                let base = self.coerce(self.get(*src)?, types::I64)?;
                let data = self
                    .b
                    .ins()
                    .iadd_imm(base, std::mem::size_of::<hl::varray>() as i64);
                self.def(*dst, data)?;
            }

            // Upstream OAssert is hl_assert(): hl_error("assert"), a
            // catchable exception the unit suite triggers on purpose — a
            // debug break here took the whole process down instead of the
            // one test. Same emission as emit_null_check's throw block;
            // hlp_error longjmps out of this frame, so nothing after the
            // call executes and the block's remaining instructions keep
            // their dominator.
            Instr::Assert => {
                let addr = self.ctx.hl_error_addr()?;
                let msg = self.ctx.utf16_message("assert");
                let sig = self.helper_sigref(&[types::I64], None);
                let callee = self.b.ins().iconst(types::I64, addr as i64);
                let msg_val = self.b.ins().iconst(types::I64, msg as i64);
                self.b.ins().call_indirect(sig, callee, &[msg_val]);
            }

            // A cache hint with no CLIF spelling — Cranelift has neither a
            // prefetch instruction nor inline assembly, and AIR classifies
            // this `Effect::Pure`, so dropping it changes nothing an observer
            // can see. The interpreter drops it too.
            Instr::Prefetch { .. } => {}

            // A source position for a shadow call stack. This tier keeps
            // none -- its frames are machine frames the runtime can walk --
            // and lowering only emits the marker for a target that does.
            Instr::Pos { .. } => {}

            // Haxe uses the non-zero OAsm modes as backend register hints;
            // neither interpreter observes them. Cranelift owns its register
            // allocation, so those hints are intentionally inert here. Mode
            // zero embeds target-specific instruction bytes in HashLink's x86
            // JIT and has no portable AIR meaning; treating it as inert is the
            // only architecture-independent behavior and matches interp.
            Instr::Asm { .. } => {}

            other => bail!("unhandled AIR instruction {:?}", instr_reject(other)),
        }
        Ok(())
    }

    fn emit_cell_step(&mut self, cell: CellId, delta: i64) -> Result<()> {
        let slot = self.cell_slot(cell)?;
        let ty = self.cell_clif_ty(self.f.cells[cell.idx()].ty)?;
        if ty.is_float() {
            bail!("Incr/Decr on a float cell");
        }
        let cur = self.b.ins().stack_load(types::I64, ty, slot, 0);
        let next = self.b.ins().iadd_imm(cur, delta);
        self.b.ins().stack_store(types::I64, next, slot, 0);
        Ok(())
    }

    fn cell_slot(&self, cell: CellId) -> Result<StackSlot> {
        self.cells
            .get(cell.idx())
            .copied()
            .ok_or_else(|| anyhow!("cell {} out of range", cell.0))
    }

    /// Emit an AIR field read. Ordinary objects use their compile-time
    /// layout; virtuals use their resolved field pointer and fall back to a
    /// name-hash lookup on the wrapped value, matching HashLink's vvirtual
    /// representation and the LLVM tier.
    fn emit_field_get(
        &mut self,
        dst: ValueId,
        obj: ValueId,
        obj_ty: AirTypeRef,
        field: usize,
    ) -> Result<()> {
        let type_index = obj_ty.0 as usize;
        match self.ctx.type_kind(type_index)? {
            hl::hl_type_kind_HOBJ | hl::hl_type_kind_HSTRUCT => {
                let (off, fty) = self.field_offset(obj_ty, field)?;
                let base = self.get(obj)?;
                let raw = self.b.ins().load(fty, MemFlagsData::trusted(), base, off);
                self.def(dst, raw)
            }
            hl::hl_type_kind_HVIRTUAL => {
                let base = self.get(obj)?;
                let field_ptr = self.b.ins().load(
                    types::I64,
                    MemFlagsData::trusted(),
                    base,
                    (24 + field * std::mem::size_of::<usize>()) as i32,
                );
                let direct_bb = self.b.create_block();
                let fallback_bb = self.b.create_block();
                let join_bb = self.b.create_block();
                let result_ty = self.value_clif_ty(dst)?;
                let result = self.b.append_block_param(join_bb, result_ty);
                let field_kind = self.ctx.virtual_field_kind(type_index, field)?;
                let function_field =
                    matches!(field_kind, hl::hl_type_kind_HFUN | hl::hl_type_kind_HMETHOD);
                if function_field {
                    // A view over an object stores a raw method entry here,
                    // not an address to an HFUN slot. In Ash it can also be a
                    // lazy findex sentinel. Dynamic lookup materializes the
                    // bound vclosure in both cases. A self-backed virtual has
                    // `value == null` and its vfield really is a data address.
                    let wrapped = self
                        .b
                        .ins()
                        .load(types::I64, MemFlagsData::trusted(), base, 8);
                    self.b.ins().brif(wrapped, fallback_bb, &[], direct_bb, &[]);
                } else {
                    self.b
                        .ins()
                        .brif(field_ptr, direct_bb, &[], fallback_bb, &[]);
                }

                self.b.switch_to_block(direct_bb);
                let direct = self
                    .b
                    .ins()
                    .load(result_ty, MemFlagsData::trusted(), field_ptr, 0);
                self.b.ins().jump(join_bb, &[BlockArg::Value(direct)]);

                self.b.switch_to_block(fallback_bb);
                let wrapped = self
                    .b
                    .ins()
                    .load(types::I64, MemFlagsData::trusted(), base, 8);
                let hash = self.ctx.virtual_field_hash(type_index, field)?;
                let hash = self.b.ins().iconst(types::I32, hash as i64);
                // The dynamic integer getter returns i32 for UI8/UI16/Bool,
                // while the AIR value and the direct field load retain their
                // declared narrow width. Block parameters require exact CLIF
                // types, so narrow before joining the two paths.
                let fallback = self.emit_dyn_get_value(dst, wrapped, hash)?;
                let fallback = self.coerce(fallback, result_ty)?;
                self.b.ins().jump(join_bb, &[BlockArg::Value(fallback)]);

                self.b.switch_to_block(join_bb);
                self.def(dst, result)
            }
            kind => bail!("field access needs a runtime lookup for type kind {kind}"),
        }
    }

    /// Emit the write half of [`Self::emit_field_get`].
    fn emit_field_set(
        &mut self,
        obj: ValueId,
        obj_ty: AirTypeRef,
        field: usize,
        src: ValueId,
    ) -> Result<()> {
        let type_index = obj_ty.0 as usize;
        match self.ctx.type_kind(type_index)? {
            hl::hl_type_kind_HOBJ | hl::hl_type_kind_HSTRUCT => {
                let (off, fty) = self.field_offset(obj_ty, field)?;
                let base = self.get(obj)?;
                let raw = self.get(src)?;
                // Narrow to the field's own width first, so the store cannot
                // spill into whatever is laid out next to it.
                let v = self.coerce(raw, fty)?;
                self.b.ins().store(MemFlagsData::trusted(), v, base, off);
                Ok(())
            }
            hl::hl_type_kind_HVIRTUAL => {
                let base = self.get(obj)?;
                let field_ptr = self.b.ins().load(
                    types::I64,
                    MemFlagsData::trusted(),
                    base,
                    (24 + field * std::mem::size_of::<usize>()) as i32,
                );
                let direct_bb = self.b.create_block();
                let fallback_bb = self.b.create_block();
                let join_bb = self.b.create_block();
                self.b
                    .ins()
                    .brif(field_ptr, direct_bb, &[], fallback_bb, &[]);

                self.b.switch_to_block(direct_bb);
                let value = self.get(src)?;
                self.b
                    .ins()
                    .store(MemFlagsData::trusted(), value, field_ptr, 0);
                self.b.ins().jump(join_bb, &[]);

                self.b.switch_to_block(fallback_bb);
                let wrapped = self
                    .b
                    .ins()
                    .load(types::I64, MemFlagsData::trusted(), base, 8);
                let hash = self.ctx.virtual_field_hash(type_index, field)?;
                let hash = self.b.ins().iconst(types::I32, hash as i64);
                self.emit_dyn_set_value(src, wrapped, hash)?;
                self.b.ins().jump(join_bb, &[]);

                self.b.switch_to_block(join_bb);
                Ok(())
            }
            kind => bail!("field access needs a runtime lookup for type kind {kind}"),
        }
    }

    /// Compile-time byte offset and machine type of a statically laid-out
    /// object field.
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

    /// Address of the first lane, shared by the vector load and store.
    ///
    /// The vector is contiguous -- the analysis refuses anything else, and
    /// `stride` records the step it proved -- so one wide access at the base
    /// address covers every lane.
    fn vec_addr(
        &mut self,
        kind: MemAccess,
        base: ValueId,
        index: ValueId,
        elem_bits: u32,
        stride: u32,
    ) -> Result<(Value, i32)> {
        let vbase = self.get(base)?;
        let idx = self.index_as_addr(index)?;
        Ok(match kind {
            MemAccess::Mem | MemAccess::I8 | MemAccess::I16 => {
                if stride as u32 != elem_bits / 8 {
                    bail!("vector access stride {stride} is not contiguous for {elem_bits}-bit lanes");
                }
                (self.b.ins().iadd(vbase, idx), 0)
            }
            MemAccess::Array => {
                if stride != 1 {
                    bail!("vector array access stride {stride} is not contiguous");
                }
                let byte = self.b.ins().imul_imm(idx, (elem_bits / 8) as i64);
                (
                    self.b.ins().iadd(vbase, byte),
                    crate::layout::VARRAY_DATA_OFFSET,
                )
            }
        })
    }

    fn emit_vec_load(
        &mut self,
        kind: MemAccess,
        dst: ValueId,
        base: ValueId,
        index: ValueId,
        stride: u32,
    ) -> Result<()> {
        let ty = self.value_clif_ty(dst)?;
        let (addr, off) = self.vec_addr(kind, base, index, ty.lane_bits(), stride)?;
        let v = self.b.ins().load(ty, MemFlagsData::trusted(), addr, off);
        self.def(dst, v)
    }

    fn emit_vec_store(
        &mut self,
        kind: MemAccess,
        base: ValueId,
        index: ValueId,
        src: ValueId,
        stride: u32,
    ) -> Result<()> {
        let ty = self.value_clif_ty(src)?;
        let (addr, off) = self.vec_addr(kind, base, index, ty.lane_bits(), stride)?;
        let v = self.get(src)?;
        self.b.ins().store(MemFlagsData::trusted(), v, addr, off);
        Ok(())
    }

    /// Fold a vector's lanes into a scalar by extracting and combining.
    ///
    /// Cranelift has no general horizontal reduction, and the extract-and-
    /// combine tree is what its backends pattern-match anyway.
    fn emit_vec_reduce(&mut self, op: BinOp, dst: ValueId, src: ValueId) -> Result<()> {
        let vty = self.value_clif_ty(src)?;
        let lanes = vty.lane_count();
        if lanes == 0 {
            bail!("VecReduce over an empty vector");
        }
        let v = self.get(src)?;
        let mut acc = self.b.ins().extractlane(v, 0);
        for lane in 1..lanes {
            let e = self.b.ins().extractlane(v, lane as u8);
            acc = self.combine_scalar(op, acc, e)?;
        }
        self.def(dst, acc)
    }

    /// One combine step of a reduction, on scalars.
    fn combine_scalar(&mut self, op: BinOp, a: Value, b: Value) -> Result<Value> {
        let ty = self.b.func.dfg.value_type(a);
        Ok(if ty.is_float() {
            match op {
                BinOp::Add => self.b.ins().fadd(a, b),
                BinOp::Sub => self.b.ins().fsub(a, b),
                BinOp::Mul => self.b.ins().fmul(a, b),
                BinOp::SDiv | BinOp::UDiv => self.b.ins().fdiv(a, b),
                _ => bail!("unsupported float reduction {op:?}"),
            }
        } else {
            match op {
                BinOp::Add => self.b.ins().iadd(a, b),
                BinOp::Sub => self.b.ins().isub(a, b),
                BinOp::Mul => self.b.ins().imul(a, b),
                BinOp::And => self.b.ins().band(a, b),
                BinOp::Or => self.b.ins().bor(a, b),
                BinOp::Xor => self.b.ins().bxor(a, b),
                _ => bail!("unsupported integer reduction {op:?}"),
            }
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
                let v = self.b.ins().load(ty, MemFlagsData::trusted(), addr, 0);
                self.def(dst, v)
            }
            MemAccess::Array => {
                let hl_kind = self.ctx.type_kind(self.f.value_ty(dst).0 as usize)?;
                let ty = abi_class(hl_kind).clif_type().ok_or_else(|| {
                    anyhow!("array element of kind {hl_kind} has no machine type")
                })?;
                let stride = crate::layout::array_elem_size(hl_kind) as i64;
                let byte_off = self.b.ins().imul_imm(idx, stride);
                let addr = self.b.ins().iadd(vbase, byte_off);
                let v = self.b.ins().load(
                    ty,
                    MemFlagsData::trusted(),
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
                let raw = self.b.ins().load(ty, MemFlagsData::trusted(), addr, 0);
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
                self.b.ins().store(MemFlagsData::trusted(), raw, addr, 0);
                Ok(())
            }
            MemAccess::Array => {
                let hl_kind = self.ctx.type_kind(self.f.value_ty(src).0 as usize)?;
                let ty = abi_class(hl_kind).clif_type().ok_or_else(|| {
                    anyhow!("array element of kind {hl_kind} has no machine type")
                })?;
                let stride = crate::layout::array_elem_size(hl_kind) as i64;
                let byte_off = self.b.ins().imul_imm(idx, stride);
                let addr = self.b.ins().iadd(vbase, byte_off);
                let v = self.coerce(raw, ty)?;
                self.b.ins().store(
                    MemFlagsData::trusted(),
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
                self.b.ins().store(MemFlagsData::trusted(), v, addr, 0);
                Ok(())
            }
        }
    }

    /// `DynGet`: read a field of a dynamic value, resolved by name hash.
    ///
    /// The accessor is chosen by the *destination's* declared kind, not by
    /// anything about the object: the helper reads whatever the field really
    /// holds and casts it to the type asked for, so the choice is a static
    /// property of the instruction. That is why this needs no runtime lookup
    /// and `FieldGet` on an `HVIRTUAL`/`HDYNOBJ` still does.
    ///
    /// No null guard on the object, matching the LLVM tier and HashLink's own
    /// JIT: both call the accessor unconditionally, and the accessor reads
    /// `obj->t->kind` before anything else. `ash_interp` is the odd one out —
    /// it answers null (and no-ops on a set) rather than faulting — so a
    /// program that reaches here with null is a crash under either compiled
    /// tier and a silent null under the interpreter. Adding the branch here
    /// alone would just move the divergence.
    fn emit_dyn_get(&mut self, dst: ValueId, obj: ValueId, field: usize) -> Result<()> {
        let obj_val = self.dyn_obj(obj)?;
        let hash = self.field_hash(field)?;

        let v = self.emit_dyn_get_value(dst, obj_val, hash)?;
        // A void destination still runs the call: the lookup can fall through
        // to a user `__get_field`, which is a side effect.
        self.def(dst, v)
    }

    /// Dynamic getter once the object and field hash have already been
    /// resolved. Virtual field fallback uses the same accessor family.
    fn emit_dyn_get_value(&mut self, dst: ValueId, obj_val: Value, hash: Value) -> Result<Value> {
        let ty = self.f.value_ty(dst);
        let kind = self.ctx.type_kind(ty.0 as usize)?;
        let (addr, shape) = self.ctx.dyn_get_helper(kind)?;

        let mut args = vec![obj_val, hash];
        let mut params = vec![types::I64, types::I32];
        if shape.takes_type() {
            let p = self.ctx.type_ptr(ty.0 as usize)?;
            args.push(self.b.ins().iconst(types::I64, p as i64));
            params.push(types::I64);
        }

        let sig = self.helper_sigref(&params, Some(dyn_value_ty(shape)));
        let callee = self.b.ins().iconst(types::I64, addr as i64);
        let call = self.b.ins().call_indirect(sig, callee, &args);
        Ok(self.b.inst_results(call)[0])
    }

    /// `DynSet`: write a field of a dynamic value, resolved by name hash.
    /// The accessor is chosen by the source's declared kind, the mirror of
    /// [`Self::emit_dyn_get`].
    fn emit_dyn_set(&mut self, obj: ValueId, field: usize, src: ValueId) -> Result<()> {
        let obj_val = self.dyn_obj(obj)?;
        let hash = self.field_hash(field)?;

        self.emit_dyn_set_value(src, obj_val, hash)
    }

    /// Dynamic setter once the object and field hash have already been
    /// resolved. Virtual field fallback uses the same accessor family.
    fn emit_dyn_set_value(&mut self, src: ValueId, obj_val: Value, hash: Value) -> Result<()> {
        let ty = self.f.value_ty(src);
        let kind = self.ctx.type_kind(ty.0 as usize)?;
        let (addr, shape) = self.ctx.dyn_set_helper(kind)?;

        let want = dyn_value_ty(shape);
        let raw = self.get(src)?;
        let have = self.b.func.dfg.value_type(raw);
        // Every narrow kind reaching the `int` accessor — HBOOL, HUI8, HUI16
        // — is unsigned in HashLink, so widening to its `int` parameter is a
        // zero-extension. `coerce` sign-extends, which would turn a byte of
        // 0x80..0xFF into a negative field value.
        let src_val = if have.is_int() && want.is_int() && have.bits() < want.bits() {
            self.b.ins().uextend(want, raw)
        } else {
            self.coerce(raw, want)?
        };

        let mut args = vec![obj_val, hash];
        let mut params = vec![types::I64, types::I32];
        if shape.takes_type() {
            let p = self.ctx.type_ptr(ty.0 as usize)?;
            args.push(self.b.ins().iconst(types::I64, p as i64));
            params.push(types::I64);
        }
        args.push(src_val);
        params.push(want);

        let sig = self.helper_sigref(&params, None);
        let callee = self.b.ins().iconst(types::I64, addr as i64);
        self.b.ins().call_indirect(sig, callee, &args);
        Ok(())
    }

    /// The object operand of a dynamic accessor.
    ///
    /// Declines a non-pointer class outright instead of widening it: the
    /// accessors dereference this before looking at anything else, so a
    /// sign-extended scalar would be a wild pointer rather than a wrong
    /// answer.
    fn dyn_obj(&mut self, obj: ValueId) -> Result<Value> {
        if self.class_of(obj)? != AbiClass::Ptr {
            bail!("dynamic field access on a non-pointer object");
        }
        self.get(obj)
    }

    /// Turn one statically typed AIR value into the `vdynamic*` representation
    /// native dynamic helpers consume. Heap values already carry an
    /// `hl_type*` header and are their own box; every non-dynamic kind needs
    /// `hlp_make_dyn` over an addressable copy. Pointer-shaped does not imply
    /// dynamically self-describing: HREF, HBYTES, HTYPE, HABSTRACT, HMETHOD,
    /// HSTRUCT, and HPACKED all carry raw pointers without a `vdynamic`
    /// header. Passing one of those through unchanged makes `hlp_dyn_castp`
    /// interpret the pointed-to payload as an `hl_type*`.
    fn box_dynamic(&mut self, value: ValueId) -> Result<Value> {
        let raw = self.get(value)?;
        let ty = self.f.value_ty(value);
        let kind = self.ctx.type_kind(ty.0 as usize)?;
        if is_dynamically_self_describing(kind) {
            return self.coerce(raw, types::I64);
        }

        let slot =
            self.b
                .create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 8, 3));
        self.b.ins().stack_store(types::I64, raw, slot, 0);
        let data = self.b.ins().stack_addr(types::I64, slot, 0);
        let type_ptr = self.ctx.type_ptr(ty.0 as usize)?;
        let type_val = self.b.ins().iconst(types::I64, type_ptr as i64);
        let helper = self
            .b
            .ins()
            .iconst(types::I64, self.ctx.make_dyn_helper()? as i64);
        let sig = self.helper_sigref(&[types::I64, types::I64], Some(types::I64));
        let call = self.b.ins().call_indirect(sig, helper, &[data, type_val]);
        Ok(self.b.inst_results(call)[0])
    }

    /// Convert the boxed result of a dynamic call to the destination's
    /// declared machine representation.
    ///
    /// `hlp_dyn_call` and `hlp_vcall_dyn` always return `vdynamic*`. Heap
    /// values may be their own dynamic box, but that does not make the box
    /// pointer interchangeable with every pointer-shaped destination. In
    /// particular, a generic method can return an HOBJ wrapped in an HDYN;
    /// using the wrapper as the object makes its payload look like object
    /// fields. Route reference results through the ordinary checked dynamic
    /// cast so exact object/array types and virtual wrappers are recovered.
    fn unbox_dynamic_result(&mut self, dst: ValueId, boxed: Value) -> Result<Value> {
        let dst_ty = self.f.value_ty(dst);
        let dst_kind = self.ctx.type_kind(dst_ty.0 as usize)?;
        if abi_class(dst_kind) != AbiClass::Ptr {
            let (addr, shape) = self.ctx.dyn_unbox_helper(dst_kind)?;
            let callee = self.b.ins().iconst(types::I64, addr as i64);
            let sig = self.helper_sigref(&[types::I64], Some(dyn_value_ty(shape)));
            let call = self.b.ins().call_indirect(sig, callee, &[boxed]);
            return Ok(self.b.inst_results(call)[0]);
        }

        if dst_kind == hl::hl_type_kind_HDYN {
            return Ok(boxed);
        }

        let dyn_type_index = self
            .ctx
            .bytecode()
            .types
            .iter()
            .position(|ty| ty.kind == hl::hl_type_kind_HDYN)
            .ok_or_else(|| anyhow!("module has no HDYN runtime type"))?;
        let src_type = self.ctx.type_ptr(dyn_type_index)?;
        let dst_type = self.ctx.type_ptr(dst_ty.0 as usize)?;
        let slot =
            self.b
                .create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 8, 3));
        self.b.ins().stack_store(types::I64, boxed, slot, 0);
        let data = self.b.ins().stack_addr(types::I64, slot, 0);
        let src_type = self.b.ins().iconst(types::I64, src_type as i64);
        let dst_type = self.b.ins().iconst(types::I64, dst_type as i64);
        let helper = self
            .b
            .ins()
            .iconst(types::I64, self.ctx.dyn_castp_helper()? as i64);
        let sig = self.helper_sigref(&[types::I64, types::I64, types::I64], Some(types::I64));
        let call = self
            .b
            .ins()
            .call_indirect(sig, helper, &[data, src_type, dst_type]);
        Ok(self.b.inst_results(call)[0])
    }

    /// The collision-resolved field-name hash as an immediate. Resolving it
    /// also caches the UTF-16 spelling in ash_std, which Reflect.fields needs
    /// to map dynamic-object hashes back to names.
    fn field_hash(&mut self, field: usize) -> Result<Value> {
        let h = self.ctx.field_name_hash(field)?;
        Ok(self.b.ins().iconst(types::I32, h as i64))
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
            // The three helper-backed casts. Each mirrors the LLVM tier's
            // choice of helper; the argument each one wants is the reason
            // they are not one shared shape.
            CastKind::ToDyn => {
                // Pointer-shaped heap values are already dynamic values;
                // primitives and abstracts are boxed by the shared helper.
                self.box_dynamic(src)?
            }
            CastKind::SafeCast => {
                let dst_kind = self.ctx.type_kind(self.f.value_ty(dst).0 as usize)?;
                let src_ty = self.f.value_ty(src);
                let src_kind = self.ctx.type_kind(src_ty.0 as usize)?;
                if abi_class(dst_kind) != AbiClass::Ptr {
                    // SafeCast is checked even when the source's static type
                    // is not Dynamic. Returning `v` here used to turn
                    // `cast("foo", Int)` into the low 32 bits of the String
                    // object's address instead of raising. Materialize the
                    // source exactly as HashLink's helpers expect and let
                    // them either coerce a numeric/null value or throw.
                    let slot = self.b.create_sized_stack_slot(StackSlotData::new(
                        StackSlotKind::ExplicitSlot,
                        8,
                        3,
                    ));
                    self.b.ins().stack_store(t, v, slot, 0);
                    let data = self.b.ins().stack_addr(types::I64, slot, 0);
                    let src_type = self.ctx.type_ptr(src_ty.0 as usize)?;
                    let dst_type = self.ctx.type_ptr(self.f.value_ty(dst).0 as usize)?;
                    let src_type = self.b.ins().iconst(types::I64, src_type as i64);
                    let dst_type = self.b.ins().iconst(types::I64, dst_type as i64);
                    let (addr, shape, takes_dst_type) = self.ctx.scalar_cast_helper(dst_kind)?;
                    let callee = self.b.ins().iconst(types::I64, addr as i64);
                    let (sig, call) = if takes_dst_type {
                        let sig = self.helper_sigref(
                            &[types::I64, types::I64, types::I64],
                            Some(dyn_value_ty(shape)),
                        );
                        let call =
                            self.b
                                .ins()
                                .call_indirect(sig, callee, &[data, src_type, dst_type]);
                        (sig, call)
                    } else {
                        let sig = self
                            .helper_sigref(&[types::I64, types::I64], Some(dyn_value_ty(shape)));
                        let call = self.b.ins().call_indirect(sig, callee, &[data, src_type]);
                        (sig, call)
                    };
                    let _ = sig;
                    self.b.inst_results(call)[0]
                } else {
                    let sp = self.ctx.type_ptr(src_ty.0 as usize)?;
                    let dp = self.ctx.type_ptr(self.f.value_ty(dst).0 as usize)?;
                    // `hlp_dyn_castp` takes `void *data`, where `data` points
                    // to the source value in its native representation.
                    // Passing a pointer value itself makes the helper read an
                    // object header as the slot; coercing a float to i64 is
                    // not a legal representation conversion. LLVM passes its
                    // register alloca here, so materialize the equivalent
                    // Cranelift slot for every source class.
                    let slot = self.b.create_sized_stack_slot(StackSlotData::new(
                        StackSlotKind::ExplicitSlot,
                        8,
                        3,
                    ));
                    self.b.ins().stack_store(types::I64, v, slot, 0);
                    let data = self.b.ins().stack_addr(types::I64, slot, 0);
                    let stv = self.b.ins().iconst(types::I64, sp as i64);
                    let dtv = self.b.ins().iconst(types::I64, dp as i64);
                    let callee = self
                        .b
                        .ins()
                        .iconst(types::I64, self.ctx.dyn_castp_helper()? as i64);
                    let sig =
                        self.helper_sigref(&[types::I64, types::I64, types::I64], Some(types::I64));
                    let call = self.b.ins().call_indirect(sig, callee, &[data, stv, dtv]);
                    self.b.inst_results(call)[0]
                }
            }
            CastKind::ToVirtual => {
                let dp = self.ctx.type_ptr(self.f.value_ty(dst).0 as usize)?;
                let vt = self.b.ins().iconst(types::I64, dp as i64);
                let obj = self.coerce(v, types::I64)?;
                let callee = self
                    .b
                    .ins()
                    .iconst(types::I64, self.ctx.to_virtual_helper()? as i64);
                let sig = self.helper_sigref(&[types::I64, types::I64], Some(types::I64));
                let call = self.b.ins().call_indirect(sig, callee, &[vt, obj]);
                self.b.inst_results(call)[0]
            }
        };
        self.def(dst, r)
    }

    /// Read word 0 of `p`, answering `fallback` when `p` is null.
    ///
    /// The branch is the interpreters' behaviour, not decoration: a null
    /// receiver reaching `GetType` / `GetTID` gets the register's declared
    /// type there, and this tier is checked against them. The LLVM tier
    /// faults on that input instead.
    fn guarded_header_load(&mut self, p: Value, ty: Type, fallback: Value) -> Result<Value> {
        let load_bb = self.b.create_block();
        let null_bb = self.b.create_block();
        let join_bb = self.b.create_block();
        let out = self.b.append_block_param(join_bb, ty);

        self.b.ins().brif(p, load_bb, &[], null_bb, &[]);

        self.b.switch_to_block(load_bb);
        let loaded = self.b.ins().load(ty, MemFlagsData::trusted(), p, 0);
        self.b.ins().jump(join_bb, &[BlockArg::Value(loaded)]);

        self.b.switch_to_block(null_bb);
        self.b.ins().jump(join_bb, &[BlockArg::Value(fallback)]);

        self.b.switch_to_block(join_bb);
        Ok(out)
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
        let r = if ta.lane_type().is_float() {
            match op {
                BinOp::Add => self.b.ins().fadd(va, vb),
                BinOp::Sub => self.b.ins().fsub(va, vb),
                BinOp::Mul => self.b.ins().fmul(va, vb),
                BinOp::SDiv | BinOp::UDiv => self.b.ins().fdiv(va, vb),
                BinOp::SMod | BinOp::UMod => {
                    let addr = if ta == types::F32 {
                        ash_fmod_f32 as usize
                    } else {
                        ash_fmod_f64 as usize
                    };
                    let callee = self.b.ins().iconst(types::I64, addr as i64);
                    let sig = self.helper_sigref(&[ta, ta], Some(ta));
                    let call = self.b.ins().call_indirect(sig, callee, &[va, vb]);
                    self.b.inst_results(call)[0]
                }
                _ => bail!("bitwise op on floats"),
            }
        } else {
            match op {
                BinOp::Add => self.b.ins().iadd(va, vb),
                BinOp::Sub => self.b.ins().isub(va, vb),
                BinOp::Mul => match self.int_const_of(b) {
                    Some(c) => self.const_mul(va, ta, c),
                    None => self.b.ins().imul(va, vb),
                },
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

    /// Multiplication by a compile-time constant, strength-reduced.
    ///
    /// `imul` is a 3-cycle-latency instruction, and when the multiply sits on
    /// a loop-carried dependence every one of those cycles is the loop's
    /// period. Shifts and adds are one cycle each, so a constant one away
    /// from a power of two costs two cycles instead of three — which is the
    /// whole gap on the call benchmarks, whose accumulator is `sum * 31`:
    /// gcc emits `(sum << 5) - sum` there and runs the loop in 3 cycles
    /// against our 4, a 1.33x ratio that matched the measured 99ms against
    /// 76ms almost exactly.
    ///
    /// Only the forms that are unambiguously cheaper are taken. A general
    /// decomposition into shift-add chains can cost more than the multiply it
    /// replaces once it needs three or more terms, and Cranelift's own
    /// lowering already handles the plain powers of two.
    fn const_mul(&mut self, va: Value, ty: Type, c: i64) -> Value {
        let bits = i64::from(ty.bits());
        let pow2 = |v: i64| v > 0 && v & (v - 1) == 0;
        // Shifting by the full width is undefined; leave those to `imul`.
        let fits = |k: u32| i64::from(k) < bits;

        match c {
            0 => self.b.ins().iconst(ty, 0),
            1 => va,
            -1 => self.b.ins().ineg(va),
            _ if pow2(c) && fits(c.trailing_zeros()) => {
                self.b.ins().ishl_imm(va, i64::from(c.trailing_zeros()))
            }
            // 2^k - 1, e.g. 31 => (x << 5) - x
            _ if c > 0 && pow2(c + 1) && fits((c + 1).trailing_zeros()) => {
                let sh = self
                    .b
                    .ins()
                    .ishl_imm(va, i64::from((c + 1).trailing_zeros()));
                self.b.ins().isub(sh, va)
            }
            // 2^k + 1, e.g. 33 => (x << 5) + x
            _ if c > 1 && pow2(c - 1) && fits((c - 1).trailing_zeros()) => {
                let sh = self
                    .b
                    .ins()
                    .ishl_imm(va, i64::from((c - 1).trailing_zeros()));
                self.b.ins().iadd(sh, va)
            }
            _ => {
                let vb = self.b.ins().iconst(ty, c);
                self.b.ins().imul(va, vb)
            }
        }
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

        let zero_div = self.b.ins().icmp_imm_s(IntCC::Equal, vb, 0);
        let bad = if signed {
            let min = 1i64 << (ty.bits() - 1);
            let a_is_min = self
                .b
                .ins()
                .icmp_imm_s(IntCC::Equal, va, min.wrapping_neg());
            let b_is_neg1 = self.b.ins().icmp_imm_s(IntCC::Equal, vb, -1);
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

    // ── Enums ───────────────────────────────────────────────────────────────

    /// The decoded description of one enum construct.
    ///
    /// Looking it up also proves the construct index is in range, which
    /// `hlp_alloc_enum` does not check for itself — it indexes the construct
    /// array to size the allocation, so a bad index there is a wild read.
    fn enum_construct(
        &self,
        enum_ty: AirTypeRef,
        construct: usize,
    ) -> Result<&crate::types::HLEnumConstruct> {
        let idx = enum_ty.0 as usize;
        let tenum = self
            .ctx
            .bytecode()
            .types
            .get(idx)
            .and_then(|t| t.tenum.as_ref())
            .ok_or_else(|| anyhow!("enum access on type {idx}, which is not an enum"))?;
        tenum
            .constructs
            .get(construct)
            .ok_or_else(|| anyhow!("enum type {idx} has no construct {construct}"))
    }

    /// Byte offset and machine type of one parameter of an enum construct.
    ///
    /// The offsets are the loader's: HL bytecode names a construct's
    /// parameter types and nothing else, so `DecodedBytecode` computes the
    /// venum layout at load time and every tier reads it from there rather
    /// than laying the payload out again.
    fn enum_param(
        &self,
        enum_ty: AirTypeRef,
        construct: usize,
        field: usize,
    ) -> Result<(i32, Type)> {
        let c = self.enum_construct(enum_ty, construct)?;
        let (off, param) = c
            .offsets
            .get(field)
            .zip(c.params.get(field))
            .map(|(off, param)| (*off, param.0))
            .ok_or_else(|| {
                anyhow!(
                    "construct {construct} of enum {} has no parameter {field}",
                    enum_ty.0
                )
            })?;
        let kind = self.ctx.type_kind(param)?;
        let ty = abi_class(kind)
            .clif_type()
            .ok_or_else(|| anyhow!("enum parameter of kind {kind} has no machine type"))?;
        Ok((off, ty))
    }

    /// The allocation `EnumAlloc` and `MakeEnum` share:
    /// `hlp_alloc_enum(t, construct)`, which sizes the venum from the
    /// construct and zeroes the payload when it holds pointers.
    ///
    /// `t` is the runtime `hl_type*`, not a description of one — the
    /// allocator reads `tenum` off it, and the header keeps it for
    /// `EnumIndex` and the dynamic side to read back.
    fn emit_enum_alloc(&mut self, enum_ty: AirTypeRef, construct: usize) -> Result<Value> {
        let idx = enum_ty.0 as usize;
        let kind = self.ctx.type_kind(idx)?;
        if kind != hl::hl_type_kind_HENUM {
            bail!("enum allocation on type kind {kind}");
        }
        self.enum_construct(enum_ty, construct)?;
        let helper = self.ctx.alloc_enum_helper()?;
        let type_ptr = self.ctx.type_ptr(idx)?;

        let callee = self.b.ins().iconst(types::I64, helper as i64);
        let t = self.b.ins().iconst(types::I64, type_ptr as i64);
        let ci = self.b.ins().iconst(types::I32, construct as i64);
        let sig = self.helper_sigref(&[types::I64, types::I32], Some(types::I64));
        let call = self.b.ins().call_indirect(sig, callee, &[t, ci]);
        Ok(self.b.inst_results(call)[0])
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

    /// `StaticClosure` / `InstanceClosure`: allocate a `vclosure` over a
    /// bytecode function, optionally binding a receiver.
    ///
    /// The function address is LOADED from `functions_ptrs[findex]` rather
    /// than baked in, for the same reason the LLVM tier loads it: the slot
    /// holds a stub sentinel until that function is promoted, and a closure
    /// built from a baked-in stale address would keep calling the stub after
    /// the real code exists. Loading at construction time also means a
    /// closure made after promotion carries the compiled address directly.
    fn emit_static_closure(
        &mut self,
        dst: ValueId,
        fun: usize,
        value: Option<ValueId>,
    ) -> Result<()> {
        let slot_addr = self.ctx.function_slot_addr(fun)?;
        let slot = self.b.ins().iconst(types::I64, slot_addr as i64);
        let fn_addr = self
            .b
            .ins()
            .load(types::I64, MemFlagsData::trusted(), slot, 0);

        let type_ptr = self.ctx.func_type_ptr(fun)?;
        let ty = self.b.ins().iconst(types::I64, type_ptr as i64);

        let helper = self.ctx.closure_helper(value.is_some())?;
        let callee = self.b.ins().iconst(types::I64, helper as i64);

        let mut args = vec![ty, fn_addr];
        let mut params = vec![types::I64, types::I64];
        if let Some(v) = value {
            let raw = self.get(v)?;
            args.push(self.coerce(raw, types::I64)?);
            params.push(types::I64);
        }
        let sig = self.helper_sigref(&params, Some(types::I64));
        let call = self.b.ins().call_indirect(sig, callee, &args);
        let out = self.b.inst_results(call)[0];
        self.def(dst, out)
    }

    /// `VirtualClosure`: bind the receiver to the implementation currently
    /// installed in its runtime vtable. `field` is the vtable `pindex`, not a
    /// position in the static proto array; inherited methods therefore require
    /// the same super-chain search as `CallMethod`.
    fn emit_virtual_closure(&mut self, dst: ValueId, obj: ValueId, field: usize) -> Result<()> {
        let obj_type_idx = self.f.value_ty(obj).0 as usize;
        let bytecode = self.ctx.bytecode();
        let kind = self.ctx.type_kind(obj_type_idx)?;
        if kind != hl::hl_type_kind_HOBJ && kind != hl::hl_type_kind_HSTRUCT {
            bail!("VirtualClosure on type kind {kind}");
        }

        let findex = {
            let mut current = bytecode.types[obj_type_idx].obj.as_ref();
            let mut found = None;
            while let Some(info) = current {
                if let Some(proto) = info.proto.iter().find(|p| p.pindex as usize == field) {
                    found = Some(proto.findex as usize);
                    break;
                }
                current = info
                    .super_
                    .as_ref()
                    .and_then(|s| bytecode.types[s.0].obj.as_ref());
            }
            found.ok_or_else(|| {
                anyhow!("VirtualClosure: no proto pindex {field} on type {obj_type_idx}")
            })?
        };

        let receiver = self.coerce(self.get(obj)?, types::I64)?;
        let runtime_type = self
            .b
            .ins()
            .load(types::I64, MemFlagsData::trusted(), receiver, 0);
        let init = self
            .b
            .ins()
            .iconst(types::I64, self.ctx.get_obj_proto_helper()? as i64);
        let init_sig = self.helper_sigref(&[types::I64], Some(types::I64));
        self.b.ins().call_indirect(init_sig, init, &[runtime_type]);
        let vtable = self
            .b
            .ins()
            .load(types::I64, MemFlagsData::trusted(), runtime_type, 16);
        let method = self.b.ins().load(
            types::I64,
            MemFlagsData::trusted(),
            vtable,
            (field * std::mem::size_of::<usize>()) as i32,
        );

        let full_type = self.ctx.func_type_ptr(findex)?;
        let type_value = self.b.ins().iconst(types::I64, full_type as i64);
        let alloc = self
            .b
            .ins()
            .iconst(types::I64, self.ctx.closure_helper(true)? as i64);
        let alloc_sig = self.helper_sigref(&[types::I64, types::I64, types::I64], Some(types::I64));
        let call = self
            .b
            .ins()
            .call_indirect(alloc_sig, alloc, &[type_value, method, receiver]);
        let closure = self.b.inst_results(call)[0];
        self.def(dst, closure)
    }

    /// `CallClosure`: call through a `vclosure`, prepending its bound value
    /// when it has one.
    ///
    /// vclosure layout, which this reads directly rather than through a
    /// helper: `t` at 0, `fun` at 8, `hasValue` (i32) at 16, `value` at 24.
    /// `hasValue` is a runtime property of the closure, not of the call
    /// site, so both shapes are emitted and selected at run time — a bound
    /// closure takes the receiver as its first argument.
    fn emit_call_closure(&mut self, dst: ValueId, fun: ValueId, args: &[ValueId]) -> Result<()> {
        let fun_kind = self.ctx.type_kind(self.f.value_ty(fun).0 as usize)?;
        if !matches!(fun_kind, hl::hl_type_kind_HFUN | hl::hl_type_kind_HMETHOD) {
            return self.emit_dynamic_call_closure(dst, fun, args);
        }

        let closure = self.coerce(self.get(fun)?, types::I64)?;
        let runtime_type = self
            .b
            .ins()
            .load(types::I64, MemFlagsData::trusted(), closure, 0);
        let expected_type = self.ctx.type_ptr(self.f.value_ty(fun).0 as usize)?;
        let expected_type = self.b.ins().iconst(types::I64, expected_type as i64);
        let pointer_exact = self
            .b
            .ins()
            .icmp(IntCC::Equal, runtime_type, expected_type);
        // A bound method carries the closure type embedded in its full
        // function type, while AIR's register can refer to an independently
        // interned but structurally identical HFUN. Pointer identity sent
        // those ordinary calls through `hlp_dyn_call`; on AArch64 that also
        // exposed the generic marshaller to I64 arguments unnecessarily.
        // HashLink type equality is structural for function signatures, so
        // use the runtime's own predicate to choose the direct ABI path.
        let structural_bb = self.b.create_block();
        let direct_bb = self.b.create_block();
        let dynamic_bb = self.b.create_block();
        let merge_bb = self.b.create_block();
        let result_ty = if self.is_void(dst) {
            None
        } else {
            Some(self.value_clif_ty(dst)?)
        };
        if let Some(ty) = result_ty {
            self.b.append_block_param(merge_bb, ty);
        }
        self.b
            .ins()
            .brif(pointer_exact, direct_bb, &[], structural_bb, &[]);

        // Pointer identity is overwhelmingly the ordinary case and costs one
        // compare. Only independently interned but structurally equivalent
        // signatures need the runtime equality helper.
        self.b.switch_to_block(structural_bb);
        let same_type = self
            .b
            .ins()
            .iconst(types::I64, self.ctx.same_type_addr()? as i64);
        let same_type_sig = self.helper_sigref(&[types::I64, types::I64], Some(types::I8));
        let same_type_call =
            self.b
                .ins()
                .call_indirect(same_type_sig, same_type, &[runtime_type, expected_type]);
        let structurally_exact = self.b.inst_results(same_type_call)[0];
        self.b
            .ins()
            .brif(structurally_exact, direct_bb, &[], dynamic_bb, &[]);

        self.b.switch_to_block(direct_bb);
        let direct = self.emit_direct_call_closure(closure, dst, args)?;
        let direct_args: Vec<BlockArg> = direct.into_iter().map(BlockArg::Value).collect();
        self.b.ins().jump(merge_bb, &direct_args);

        self.b.switch_to_block(dynamic_bb);
        let dynamic = self.emit_dynamic_call_closure_value(dst, closure, args)?;
        let dynamic_args: Vec<BlockArg> = dynamic.into_iter().map(BlockArg::Value).collect();
        self.b.ins().jump(merge_bb, &dynamic_args);

        self.b.switch_to_block(merge_bb);
        if result_ty.is_some() {
            self.def(dst, self.b.block_params(merge_bb)[0])?;
        } else {
            self.define_void_word_if_used(dst);
        }
        Ok(())
    }

    /// Fast closure call for an exact runtime/static function-type match.
    fn emit_direct_call_closure(
        &mut self,
        closure: Value,
        dst: ValueId,
        args: &[ValueId],
    ) -> Result<Option<Value>> {
        let fn_addr = self
            .b
            .ins()
            .load(types::I64, MemFlagsData::trusted(), closure, 8);
        let has_value = self
            .b
            .ins()
            .load(types::I32, MemFlagsData::trusted(), closure, 16);
        let bound_value = self
            .b
            .ins()
            .load(types::I64, MemFlagsData::trusted(), closure, 24);

        // Argument classes come from the closure's own signature in AIR: the
        // destination's type gives the return, the argument values give the
        // parameters.
        let mut arg_vals = Vec::with_capacity(args.len() + 1);
        let mut classes = Vec::with_capacity(args.len() + 1);
        for a in args {
            let c = argument_abi_class(self.ctx.type_kind(self.f.value_ty(*a).0 as usize)?);
            let want = c
                .clif_type()
                .ok_or_else(|| anyhow!("void argument to closure call"))?;
            let raw = self.get(*a)?;
            arg_vals.push(self.coerce(raw, want)?);
            classes.push(c);
        }
        let ret_class = if self.is_void(dst) {
            AbiClass::Void
        } else {
            self.class_of(dst)?
        };

        let bound_bb = self.b.create_block();
        let plain_bb = self.b.create_block();
        let merge_bb = self.b.create_block();
        if let Some(t) = ret_class.clif_type() {
            self.b.append_block_param(merge_bb, t);
        }
        self.b.ins().brif(has_value, bound_bb, &[], plain_bb, &[]);

        // Bound: the receiver is argument zero.
        self.b.switch_to_block(bound_bb);
        let mut bound_args = vec![bound_value];
        bound_args.extend_from_slice(&arg_vals);
        let mut bound_classes = vec![AbiClass::Ptr];
        bound_classes.extend_from_slice(&classes);
        let r = self.stub_guarded_indirect(fn_addr, &bound_args, &bound_classes, ret_class)?;
        let bound_out: Vec<BlockArg> = r.into_iter().map(BlockArg::Value).collect();
        self.b.ins().jump(merge_bb, &bound_out);

        self.b.switch_to_block(plain_bb);
        let r = self.stub_guarded_indirect(fn_addr, &arg_vals, &classes, ret_class)?;
        let plain_out: Vec<BlockArg> = r.into_iter().map(BlockArg::Value).collect();
        self.b.ins().jump(merge_bb, &plain_out);

        self.b.switch_to_block(merge_bb);
        Ok(ret_class
            .clif_type()
            .map(|_| self.b.block_params(merge_bb)[0]))
    }

    /// Call a closure whose AIR type does not carry its concrete function
    /// signature. A direct indirect-call cannot be correct here: a getter
    /// fetched through `Reflect.getProperty`, for example, is `Dynamic` at
    /// the call site but may return an unboxed `Int`. Treating that machine
    /// result as the site's `vdynamic*` gives pointers such as `0x5`.
    ///
    /// The runtime helper reads the closure's actual `hl_type`, boxes each
    /// argument according to that signature, and always returns a boxed
    /// value. We only pay for this path at an explicitly dynamic boundary;
    /// ordinary `HFUN`/`HMETHOD` calls retain the direct fast path above.
    fn emit_dynamic_call_closure(
        &mut self,
        dst: ValueId,
        fun: ValueId,
        args: &[ValueId],
    ) -> Result<()> {
        let closure = self.coerce(self.get(fun)?, types::I64)?;
        let value = self.emit_dynamic_call_closure_value(dst, closure, args)?;
        if let Some(value) = value {
            self.def(dst, value)
        } else {
            self.define_void_word_if_used(dst);
            Ok(())
        }
    }

    fn emit_dynamic_call_closure_value(
        &mut self,
        dst: ValueId,
        closure: Value,
        args: &[ValueId],
    ) -> Result<Option<Value>> {
        let slot = self.b.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            (args.len().max(1) * std::mem::size_of::<usize>()) as u32,
            3,
        ));
        for (index, arg) in args.iter().copied().enumerate() {
            let boxed = self.box_dynamic(arg)?;
            self.b
                .ins()
                .stack_store(types::I64, boxed, slot, (index * 8) as i32);
        }
        let argv = self.b.ins().stack_addr(types::I64, slot, 0);
        let nargs = self.b.ins().iconst(types::I32, args.len() as i64);
        let helper = self
            .b
            .ins()
            .iconst(types::I64, self.ctx.dyn_call_helper()? as i64);
        let sig = self.helper_sigref(&[types::I64, types::I64, types::I32], Some(types::I64));
        let call = self
            .b
            .ins()
            .call_indirect(sig, helper, &[closure, argv, nargs]);
        let boxed = self.b.inst_results(call)[0];

        if self.is_void(dst) {
            return Ok(None);
        }

        let value = self.unbox_dynamic_result(dst, boxed)?;
        Ok(Some(self.coerce(value, self.value_clif_ty(dst)?)?))
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
            .map(|a| argument_abi_class(bytecode.types[a.0].kind))
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
        } else {
            self.define_void_word_if_used(dst);
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
            .load(types::I64, MemFlagsData::trusted(), slot_base, 0);
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
                .icmp_imm_s(IntCC::UnsignedLessThan, fn_addr, STUB_SENTINEL_LIMIT as i64);

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

        // The stub path, with the LLVM tier's self-heal in front of it.
        //
        // A sentinel means "this findex was not compiled when the caller
        // captured its address" — which for a closure is decided once, at
        // construction, and then never revisited. Without this, a closure
        // built before its target promoted sends every later call through the
        // interpreter bridge for the life of the program: measured at 8.2s
        // against 0.39s on bench_closure_call. Re-read the slot instead; the
        // sentinel encodes `findex + 1`, so the real address is one load
        // away, and calls made after promotion go direct.
        self.b.switch_to_block(stub_bb);
        let ptrs_base = self.ctx.functions_ptrs_base();
        if ptrs_base != 0 {
            let heal_bb = self.b.create_block();
            let bridge_bb = self.b.create_block();
            let findex = self.b.ins().iadd_imm(fn_addr, -1);
            let off = self.b.ins().imul_imm(findex, 8);
            let base = self.b.ins().iconst(types::I64, ptrs_base as i64);
            let slot = self.b.ins().iadd(base, off);
            let real = self
                .b
                .ins()
                .load(types::I64, MemFlagsData::trusted(), slot, 0);
            let healed = self.b.ins().icmp_imm_s(
                IntCC::UnsignedGreaterThanOrEqual,
                real,
                STUB_SENTINEL_LIMIT as i64,
            );
            self.b.ins().brif(healed, heal_bb, &[], bridge_bb, &[]);

            self.b.switch_to_block(heal_bb);
            let mut hsig = Signature::new(self.ctx.call_conv());
            for c in param_classes {
                hsig.params.push(AbiParam::new(
                    c.clif_type().ok_or_else(|| anyhow!("void parameter"))?,
                ));
            }
            if let Some(t) = ret_ty {
                hsig.returns.push(AbiParam::new(t));
            }
            let hsigref = self.b.import_signature(hsig);
            let hcall = self.b.ins().call_indirect(hsigref, real, args);
            let hvals: Vec<BlockArg> = if ret_ty.is_some() {
                vec![BlockArg::Value(self.b.inst_results(hcall)[0])]
            } else {
                vec![]
            };
            self.b.ins().jump(merge_bb, &hvals);

            self.b.switch_to_block(bridge_bb);
        }

        // Compiled-only workers must never re-enter the single main-thread
        // interpreter. Ask the shared AIR V2 compiler for this one findex,
        // then invoke the resolved pointer with this call site's exact ABI.
        // Hybrid mode returns null and continues to the interpreter bridge.
        let resolved_bb = self.b.create_block();
        let interpreter_bb = self.b.create_block();
        let resolve_sig = self.helper_sigref(&[types::I64], Some(types::I64));
        let resolve_addr = self
            .b
            .ins()
            .iconst(types::I64, ash_jit_resolve_stub as usize as i64);
        let resolve_call =
            self.b
                .ins()
                .call_indirect(resolve_sig, resolve_addr, &[fn_addr]);
        let resolved = self.b.inst_results(resolve_call)[0];
        let resolved_real = self.b.ins().icmp_imm_s(
            IntCC::UnsignedGreaterThanOrEqual,
            resolved,
            STUB_SENTINEL_LIMIT as i64,
        );
        self.b
            .ins()
            .brif(resolved_real, resolved_bb, &[], interpreter_bb, &[]);

        self.b.switch_to_block(resolved_bb);
        let resolved_call = self.b.ins().call_indirect(sigref, resolved, args);
        let resolved_vals: Vec<BlockArg> = if ret_ty.is_some() {
            vec![BlockArg::Value(self.b.inst_results(resolved_call)[0])]
        } else {
            vec![]
        };
        self.b.ins().jump(merge_bb, &resolved_vals);

        self.b.switch_to_block(interpreter_bb);
        let nargs = args.len();
        let slot = self.b.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            (nargs.max(1) * 8) as u32,
            3,
        ));
        for (idx, v) in args.iter().enumerate() {
            let ty = self.b.func.dfg.value_type(*v);
            let word = if ty == types::F64 {
                self.b.ins().bitcast(types::I64, MemFlagsData::new(), *v)
            } else if ty == types::F32 {
                let wide = self.b.ins().fpromote(types::F64, *v);
                self.b.ins().bitcast(types::I64, MemFlagsData::new(), wide)
            } else if ty.bits() < 64 {
                self.b.ins().uextend(types::I64, *v)
            } else {
                *v
            };
            self.b
                .ins()
                .stack_store(types::I64, word, slot, (idx * 8) as i32);
        }
        let buf = self.b.ins().stack_addr(types::I64, slot, 0);
        let stub_sig = self.helper_sigref(
            &[types::I64, types::I32, types::I64, types::I32],
            Some(types::I64),
        );
        let stub_addr = self
            .b
            .ins()
            .iconst(types::I64, ash_jit_call_stub as usize as i64);
        let nargs_val = self.b.ins().iconst(types::I32, nargs as i64);
        let caller = self.b.ins().iconst(types::I32, self.findex as i64);
        let stub_call =
            self.b
                .ins()
                .call_indirect(stub_sig, stub_addr, &[fn_addr, caller, buf, nargs_val]);
        let stub_vals: Vec<BlockArg> = match ret_ty {
            None => vec![],
            Some(t) => {
                let raw = self.b.inst_results(stub_call)[0];
                let decoded = if t == types::F64 {
                    self.b.ins().bitcast(types::F64, MemFlagsData::new(), raw)
                } else if t == types::F32 {
                    let wide = self.b.ins().bitcast(types::F64, MemFlagsData::new(), raw);
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

    /// HVIRTUAL dispatch through the runtime helper that owns the concrete
    /// method ABI. The interface signature can differ from the implementation
    /// signature after generic specialization, so calling a resolved pointer
    /// with AIR's declared types would be an ABI pun. Boxed dispatch is the
    /// same fallback the LLVM tier uses for this case.
    fn emit_virtual_call(
        &mut self,
        dst: ValueId,
        obj_type_idx: usize,
        field: usize,
        args: &[ValueId],
    ) -> Result<()> {
        let receiver = self.dyn_obj(args[0])?;
        let hash = self.ctx.virtual_field_hash(obj_type_idx, field)?;
        let hash = self.b.ins().iconst(types::I32, i64::from(hash));
        let tail = &args[1..];

        let array = if tail.is_empty() {
            self.b.ins().iconst(types::I64, 0)
        } else {
            let helper = self
                .b
                .ins()
                .iconst(types::I64, self.ctx.alloc_dyn_array_helper()? as i64);
            let count = self.b.ins().iconst(types::I32, tail.len() as i64);
            let sig = self.helper_sigref(&[types::I32], Some(types::I64));
            let call = self.b.ins().call_indirect(sig, helper, &[count]);
            self.b.inst_results(call)[0]
        };

        for (idx, arg) in tail.iter().copied().enumerate() {
            let boxed = self.box_dynamic(arg)?;
            let offset = std::mem::size_of::<hl::varray>() + idx * std::mem::size_of::<usize>();
            self.b
                .ins()
                .store(MemFlagsData::trusted(), boxed, array, offset as i32);
        }

        let helper = self
            .b
            .ins()
            .iconst(types::I64, self.ctx.vcall_dyn_helper()? as i64);
        let sig = self.helper_sigref(&[types::I64, types::I32, types::I64], Some(types::I64));
        let call = self
            .b
            .ins()
            .call_indirect(sig, helper, &[receiver, hash, array]);
        let boxed = self.b.inst_results(call)[0];
        if self.is_void(dst) {
            self.define_void_word_if_used(dst);
            return Ok(());
        }

        let value = self.unbox_dynamic_result(dst, boxed)?;
        let want = self.value_clif_ty(dst)?;
        let value = self.coerce(value, want)?;
        self.def(dst, value)
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
        if kind == hl::hl_type_kind_HVIRTUAL {
            return self.emit_virtual_call(dst, obj_type_idx, field, args);
        }
        if kind != hl::hl_type_kind_HOBJ && kind != hl::hl_type_kind_HSTRUCT {
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
            .map(|a| argument_abi_class(bytecode.types[a.0].kind))
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
        let type_ptr = self
            .b
            .ins()
            .load(types::I64, MemFlagsData::trusted(), obj, 0);
        let proto = self
            .b
            .ins()
            .load(types::I64, MemFlagsData::trusted(), type_ptr, 16);
        let method = self.b.ins().load(
            types::I64,
            MemFlagsData::trusted(),
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
        } else {
            self.define_void_word_if_used(dst);
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
            if !self.has_machine_value(dst) {
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

            // `hlp_throw` does not return: it longjmps to the nearest armed
            // trap, or exits. The block still needs a terminator for the
            // verifier, and `trap` is the honest one — reaching it would mean
            // the helper returned, which it cannot.
            Terminator::Throw { exc } | Terminator::Rethrow { exc } => {
                let is_rethrow = matches!(term, Terminator::Rethrow { .. });
                let v = self.get(*exc)?;
                let v = self.coerce(v, types::I64)?;
                let callee = self
                    .b
                    .ins()
                    .iconst(types::I64, self.ctx.throw_helper(is_rethrow)? as i64);
                let sig = self.helper_sigref(&[types::I64], None);
                self.b.ins().call_indirect(sig, callee, &[v]);
                self.b
                    .ins()
                    .trap(cranelift_codegen::ir::TrapCode::unwrap_user(2));
                Ok(())
            }

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

            Terminator::Trap {
                exc_cell,
                handler,
                normal,
            } => {
                // Arm ash_std's thread-local trap chain, then establish the
                // jmp_buf in this generated frame. `hlp_throw` pops that trap
                // before longjmp returns here with a non-zero result.
                let setup = self
                    .b
                    .ins()
                    .iconst(types::I64, self.ctx.setup_trap_helper()? as i64);
                let setup_sig = self.helper_sigref(&[], Some(types::I64));
                let setup_call = self.b.ins().call_indirect(setup_sig, setup, &[]);
                let buf = self.b.inst_results(setup_call)[0];

                let setjmp = self
                    .b
                    .ins()
                    .iconst(types::I64, crate::hl::_setjmp as usize as i64);
                let setjmp_sig = self.helper_sigref(&[types::I64], Some(types::I32));
                let setjmp_call = self.b.ins().call_indirect(setjmp_sig, setjmp, &[buf]);
                let jumped = self.b.inst_results(setjmp_call)[0];
                let caught = self.b.ins().icmp_imm(IntCC::NotEqual, jumped, 0);

                let caught_bb = self.b.create_block();
                let normal_bb = self.b.create_block();
                self.b.ins().brif(caught, caught_bb, &[], normal_bb, &[]);

                self.b.switch_to_block(normal_bb);
                let normal_args = self.phi_args(bid, *normal)?;
                let normal_target = self.clif_block(*normal)?;
                self.b.ins().jump(normal_target, &normal_args);

                self.b.switch_to_block(caught_bb);
                let get = self
                    .b
                    .ins()
                    .iconst(types::I64, self.ctx.get_exc_helper()? as i64);
                let get_sig = self.helper_sigref(&[], Some(types::I64));
                let get_call = self.b.ins().call_indirect(get_sig, get, &[]);
                let exception = self.b.inst_results(get_call)[0];
                let cell = self.cell_slot(*exc_cell)?;
                self.b.ins().stack_store(types::I64, exception, cell, 0);

                let clear = self
                    .b
                    .ins()
                    .iconst(types::I64, self.ctx.clear_exc_helper()? as i64);
                let clear_sig = self.helper_sigref(&[], None);
                self.b.ins().call_indirect(clear_sig, clear, &[]);

                let handler_args = self.phi_args(bid, *handler)?;
                let handler_target = self.clif_block(*handler)?;
                self.b.ins().jump(handler_target, &handler_args);
                Ok(())
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
                    let z = self.b.ins().icmp_imm_s(IntCC::Equal, va, 0);
                    Cond::Value(z)
                }
                // Non-pointer values are never null, so the branch is decided
                // without a comparison.
                CondKind::Null if class != AbiClass::Ptr => Cond::Always(false),
                CondKind::NotNull if class != AbiClass::Ptr => Cond::Always(true),
                CondKind::Null => {
                    let z = self.b.ins().icmp_imm_s(IntCC::Equal, va, 0);
                    Cond::Value(z)
                }
                CondKind::NotNull => {
                    let z = self.b.ins().icmp_imm_s(IntCC::NotEqual, va, 0);
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

        // HashLink's NotLt/NotGte are logical negations, not ordered >=/<
        // aliases. Their float forms therefore include unordered (NaN), while
        // their integer forms remain the ordinary complementary predicates.
        let (icc, fcc) = match kind {
            CondKind::SLt => (IntCC::SignedLessThan, FloatCC::LessThan),
            CondKind::SGte => (IntCC::SignedGreaterThanOrEqual, FloatCC::GreaterThanOrEqual),
            CondKind::SGt => (IntCC::SignedGreaterThan, FloatCC::GreaterThan),
            CondKind::SLte => (IntCC::SignedLessThanOrEqual, FloatCC::LessThanOrEqual),
            CondKind::ULt => (IntCC::UnsignedLessThan, FloatCC::LessThan),
            CondKind::UGte => (
                IntCC::UnsignedGreaterThanOrEqual,
                FloatCC::GreaterThanOrEqual,
            ),
            CondKind::NotLt => (
                IntCC::SignedGreaterThanOrEqual,
                FloatCC::UnorderedOrGreaterThanOrEqual,
            ),
            CondKind::NotGte => (IntCC::SignedLessThan, FloatCC::UnorderedOrLessThan),
            CondKind::Eq => (IntCC::Equal, FloatCC::Equal),
            CondKind::NotEq => (IntCC::NotEqual, FloatCC::NotEqual),
            _ => unreachable!("unary conditions returned above"),
        };

        let cond = if ta.is_float() {
            self.b.ins().fcmp(fcc, va, vb)
        } else if class == AbiClass::Ptr {
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
            let hl_kind = self.ctx.type_kind(self.f.value_ty(a).0 as usize)?;
            if hl_kind == hl::hl_type_kind_HDYN
                || hl_kind == hl::hl_type_kind_HNULL
                || hl_kind == hl::hl_type_kind_HOBJ
                || hl_kind == hl::hl_type_kind_HVIRTUAL
            {
                let addr = self.ctx.dyn_compare_addr()?;
                let sig = self.helper_sigref(&[types::I64, types::I64], Some(types::I32));
                let callee = self.b.ins().iconst(types::I64, addr as i64);
                let call = self.b.ins().call_indirect(sig, callee, &[va, vb]);
                let res = self.b.inst_results(call)[0];
                self.b.ins().icmp_imm_s(icc, res, 0)
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
/// Machine type of the value a `DynShape`'s accessor returns or takes.
fn dyn_value_ty(shape: DynShape) -> Type {
    match shape {
        DynShape::F64 => types::F64,
        DynShape::F32 => types::F32,
        DynShape::I64 | DynShape::Ptr => types::I64,
        DynShape::Int => types::I32,
    }
}

/// Mirror HashLink's `hl_is_dynamic`: these values carry an `hl_type*` in
/// their first machine word and can therefore be passed as `vdynamic*`
/// without allocating a wrapper.
fn is_dynamically_self_describing(kind: hl::hl_type_kind) -> bool {
    matches!(
        kind,
        hl::hl_type_kind_HDYN
            | hl::hl_type_kind_HFUN
            | hl::hl_type_kind_HOBJ
            | hl::hl_type_kind_HARRAY
            | hl::hl_type_kind_HVIRTUAL
            | hl::hl_type_kind_HDYNOBJ
            | hl::hl_type_kind_HENUM
            | hl::hl_type_kind_HNULL
    )
}

fn intrinsic_to_native(k: air::v2::ir::IntrinsicKind) -> crate::intrinsics::NativeIntrinsic {
    use crate::intrinsics::NativeIntrinsic as NI;
    use air::v2::ir::IntrinsicKind as K;
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
