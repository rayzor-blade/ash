//! HL bytecode → CLIF lowering for the Cranelift middle tier.
//!
//! Scope is deliberately the *mechanical* opcode subset (see
//! [`super::is_cranelift_lowerable`]): constants, arithmetic, control flow,
//! conversions, globals, and calls. Nothing here makes an object-model layout
//! decision — no field offsets, vtable slots, enum construct offsets or unbox
//! choices — so there is no second copy of the compile-time layout logic to
//! drift from the LLVM tier.
//!
//! Registers become Cranelift `Variable`s (the frontend does SSA construction
//! and block-parameter insertion), so `Ref`/`Unref`/`Setref` — which take the
//! address of a register — are out of scope by construction.
//!
//! Calls:
//! - **native** targets go straight to the implementation address, imported by
//!   its canonical `lib@symbol` name (every entry of
//!   `NativeFunctionResolver::symbol_table_entries()` is registered with the
//!   `JITBuilder` when the backend is built);
//! - **bytecode** targets load `functions_ptrs[findex]` at run time and guard
//!   the loaded pointer against the interpreter's stub sentinel (`findex + 1`,
//!   always `< STUB_SENTINEL_LIMIT`), routing sentinels through
//!   [`crate::jit::stub_bridge::ash_jit_call_stub`] exactly like the LLVM
//!   tier's `build_stub_guarded_indirect_call`.

use anyhow::{anyhow, bail, Result};
use std::collections::HashMap;

use beadie::CraneliftFunctionDef;
use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::{
    types, AbiParam, Block, BlockArg, BlockCall, FuncRef, InstBuilder, JumpTableData, MemFlags,
    SigRef, Signature, StackSlotData, StackSlotKind, Type, Value,
};
use cranelift_frontend::{FunctionBuilder, Variable};

use super::backend::{AshCraneliftBackend, CraneliftTierContext};
use super::{abi_class, entry_return_class, first_unsupported_opcode, AbiClass};
use crate::hl_bindings as hl;
use crate::jit::stub_bridge::{ash_jit_call_stub, STUB_SENTINEL_LIMIT};
use crate::opcodes::{Opcode, Reg};
use crate::types::{HLFunction, HLTypeFun};

/// A lowered function plus the marshaling metadata the interpreter needs.
pub struct LoweredFunction {
    pub def: CraneliftFunctionDef,
    pub arg_kinds: Vec<u32>,
    pub ret_kind: u32,
    /// Opcode count, for the promotion log.
    pub num_ops: usize,
}

/// Static pre-flight check: why this function cannot be lowered by the
/// Cranelift tier, or `None` when it can be attempted.
///
/// Cheap and side-effect free — run it before paying for a lowering attempt.
pub fn lowering_reject_reason(
    bytecode: &crate::bytecode::DecodedBytecode,
    func: &HLFunction,
) -> Option<String> {
    let tf = match bytecode
        .types
        .get(func.type_.0)
        .and_then(|t| t.fun.as_ref())
    {
        Some(tf) => tf,
        None => return Some("no_function_type".to_string()),
    };
    if tf.args.len() > 8 {
        return Some("arg_count_over_8".to_string());
    }
    // The interpreter marshals float arguments and results as f64
    // (`dispatch_float_native` passes `NanBoxedValue::as_f64()`), so an entry
    // point declaring f32 would read the wrong half of the register.
    for a in &tf.args {
        let kind = bytecode.types[a.0].kind;
        if kind == hl::hl_type_kind_HF32 {
            return Some("f32_in_signature".to_string());
        }
        if kind == hl::hl_type_kind_HVOID {
            return Some("void_argument".to_string());
        }
    }
    if bytecode.types[tf.ret.0].kind == hl::hl_type_kind_HF32 {
        return Some("f32_in_signature".to_string());
    }
    if let Some(op) = first_unsupported_opcode(&func.ops) {
        return Some(format!("unsupported_opcode {}", opcode_name(op)));
    }
    None
}

/// Short opcode name for logs (the full `Debug` render is far too noisy).
fn opcode_name(op: &Opcode) -> &'static str {
    match op {
        Opcode::Trap { .. } => "Trap",
        Opcode::EndTrap { .. } => "EndTrap",
        Opcode::Throw { .. } => "Throw",
        Opcode::Rethrow { .. } => "Rethrow",
        Opcode::Field { .. } => "Field",
        Opcode::SetField { .. } => "SetField",
        Opcode::GetThis { .. } => "GetThis",
        Opcode::SetThis { .. } => "SetThis",
        Opcode::CallMethod { .. } => "CallMethod",
        Opcode::CallThis { .. } => "CallThis",
        Opcode::CallClosure { .. } => "CallClosure",
        Opcode::StaticClosure { .. } => "StaticClosure",
        Opcode::InstanceClosure { .. } => "InstanceClosure",
        Opcode::VirtualClosure { .. } => "VirtualClosure",
        Opcode::DynGet { .. } => "DynGet",
        Opcode::DynSet { .. } => "DynSet",
        Opcode::ToDyn { .. } => "ToDyn",
        Opcode::SafeCast { .. } => "SafeCast",
        Opcode::ToVirtual { .. } => "ToVirtual",
        Opcode::New { .. } => "New",
        Opcode::EnumAlloc { .. } => "EnumAlloc",
        Opcode::MakeEnum { .. } => "MakeEnum",
        Opcode::EnumIndex { .. } => "EnumIndex",
        Opcode::EnumField { .. } => "EnumField",
        Opcode::SetEnumField { .. } => "SetEnumField",
        Opcode::GetArray { .. } => "GetArray",
        Opcode::SetArray { .. } => "SetArray",
        Opcode::ArraySize { .. } => "ArraySize",
        Opcode::GetI8 { .. } => "GetI8",
        Opcode::GetI16 { .. } => "GetI16",
        Opcode::GetMem { .. } => "GetMem",
        Opcode::SetI8 { .. } => "SetI8",
        Opcode::SetI16 { .. } => "SetI16",
        Opcode::SetMem { .. } => "SetMem",
        Opcode::Ref { .. } => "Ref",
        Opcode::Unref { .. } => "Unref",
        Opcode::Setref { .. } => "Setref",
        Opcode::RefData { .. } => "RefData",
        Opcode::RefOffset { .. } => "RefOffset",
        Opcode::Type { .. } => "Type",
        Opcode::GetType { .. } => "GetType",
        Opcode::GetTID { .. } => "GetTID",
        Opcode::Bytes { .. } => "Bytes",
        Opcode::Prefetch { .. } => "Prefetch",
        Opcode::Asm { .. } => "Asm",
        Opcode::Assert => "Assert",
        Opcode::IndirectCall { .. } => "IndirectCall",
        _ => "other",
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Signatures
// ─────────────────────────────────────────────────────────────────────────────

/// Signature used when this tier *calls* a function (native or bytecode).
///
/// Mirrors the LLVM tier's `create_function_type` byte for byte, because a
/// bytecode target may already have been promoted by the LLVM tier and
/// installed into `functions_ptrs`: sub-word kinds keep their declared width
/// (`HBOOL` is `i1` on the LLVM side, `I8` here — both only ever carry 0/1).
fn call_signature(
    backend: &AshCraneliftBackend,
    ctx: &CraneliftTierContext,
    tf: &HLTypeFun,
) -> Result<Signature> {
    let mut sig = backend.make_signature();
    for a in &tf.args {
        let class = abi_class(ctx.type_kind(a.0)?);
        let ty = class
            .clif_type()
            .ok_or_else(|| anyhow!("void argument in call signature"))?;
        sig.params.push(AbiParam::new(ty));
    }
    if let Some(ty) = abi_class(ctx.type_kind(tf.ret.0)?).clif_type() {
        sig.returns.push(AbiParam::new(ty));
    }
    Ok(sig)
}

/// Signature of the compiled ENTRY point, as the interpreter's
/// `call_compiled_function` will transmute and call it.
///
/// Same parameter mapping as [`call_signature`]; the result is widened to
/// `I32` for `HBOOL`/`HUI8`/`HUI16` so the interpreter's raw-`i64` read never
/// sees undefined upper bits (see [`entry_return_class`]).
fn entry_signature(
    backend: &AshCraneliftBackend,
    ctx: &CraneliftTierContext,
    tf: &HLTypeFun,
) -> Result<Signature> {
    let mut sig = backend.make_signature();
    for a in &tf.args {
        let class = abi_class(ctx.type_kind(a.0)?);
        let ty = class
            .clif_type()
            .ok_or_else(|| anyhow!("void argument in entry signature"))?;
        sig.params.push(AbiParam::new(ty));
    }
    if let Some(ty) = entry_return_class(ctx.type_kind(tf.ret.0)?).clif_type() {
        sig.returns.push(AbiParam::new(ty));
    }
    Ok(sig)
}

// ─────────────────────────────────────────────────────────────────────────────
// Lowering
// ─────────────────────────────────────────────────────────────────────────────

/// Lower one bytecode function to CLIF. Errors mean "this tier declines" —
/// the caller falls back to the LLVM tier rather than blacklisting the bead.
pub fn lower_function(
    backend: &AshCraneliftBackend,
    ctx: &CraneliftTierContext,
    findex: usize,
) -> Result<LoweredFunction> {
    let bytecode = ctx.bytecode();
    let func_idx = ctx
        .func_index(findex)
        .ok_or_else(|| anyhow!("findex {findex} is not a bytecode function"))?;
    let func = &bytecode.functions[func_idx];

    if let Some(reason) = lowering_reject_reason(bytecode, func) {
        bail!("{reason}");
    }

    let tf = bytecode.types[func.type_.0]
        .fun
        .as_ref()
        .ok_or_else(|| anyhow!("no function type"))?;

    let arg_kinds: Vec<u32> = tf.args.iter().map(|a| bytecode.types[a.0].kind).collect();
    let ret_kind = bytecode.types[tf.ret.0].kind;

    let sig = entry_signature(backend, ctx, tf)?;
    let name = backend.unique_name(findex, &func.name());
    let mut def = backend
        .new_def(sig, &name)
        .map_err(|e| anyhow!("declare_function({name}): {e}"))?;

    // Imports must be declared before a FunctionBuilder borrows `ctx.func`.
    let native_refs = import_native_targets(backend, ctx, func, &mut def)?;

    {
        let mut lo = Lowerer {
            ctx,
            func,
            b: def.builder(),
            vars: Vec::new(),
            reg_ty: Vec::new(),
            reg_class: Vec::new(),
            blocks: HashMap::new(),
            native_refs,
            ret_class: entry_return_class(ret_kind),
            terminated: false,
        };
        lo.run()?;
        lo.finish();
    }

    // `ASH_CL_DUMP=<findex>` (or `all`) prints the lowered CLIF. Reading the IR
    // is the only practical way to tell a bad offset from a bad width from a
    // bad base pointer once a function miscompiles.
    if let Ok(want) = std::env::var("ASH_CL_DUMP") {
        if want == "all" || want.split(',').any(|w| w.trim() == findex.to_string()) {
            eprintln!(
                "=== CLIF findex={findex} {} ===\n{}",
                func.name(),
                def.ctx.func.display()
            );
        }
    }

    Ok(LoweredFunction {
        def,
        arg_kinds,
        ret_kind,
        num_ops: func.ops.len(),
    })
}

/// Declare every native call target this function references, by its
/// canonical `lib@symbol` key, and return `findex -> FuncRef`.
fn import_native_targets(
    backend: &AshCraneliftBackend,
    ctx: &CraneliftTierContext,
    func: &HLFunction,
    def: &mut CraneliftFunctionDef,
) -> Result<HashMap<usize, FuncRef>> {
    let bytecode = ctx.bytecode();
    let mut refs: HashMap<usize, FuncRef> = HashMap::new();
    for op in &func.ops {
        let target = match op {
            Opcode::Call0 { fun, .. }
            | Opcode::Call1 { fun, .. }
            | Opcode::Call2 { fun, .. }
            | Opcode::Call3 { fun, .. }
            | Opcode::Call4 { fun, .. }
            | Opcode::CallN { fun, .. } => fun.0,
            _ => continue,
        };
        let Some(native_idx) = ctx.native_index(target) else {
            continue;
        };
        if refs.contains_key(&target) {
            continue;
        }
        let native = &bytecode.natives[native_idx];
        let tf = bytecode.types[native.type_.0]
            .fun
            .as_ref()
            .ok_or_else(|| anyhow!("native {} has no function type", native.name))?;
        let sig = call_signature(backend, ctx, tf)?;
        let key = ctx
            .native_symbol_key(native_idx)
            .ok_or_else(|| anyhow!("native {}@{} unresolved", native.lib, native.name))?;
        let fref = backend
            .import_function(&key, &sig, &mut def.ctx.func)
            .map_err(|e| anyhow!("import {key}: {e}"))?;
        refs.insert(target, fref);
    }
    Ok(refs)
}

struct Lowerer<'a, 'b> {
    ctx: &'a CraneliftTierContext,
    func: &'a HLFunction,
    b: FunctionBuilder<'b>,
    /// One Cranelift variable per HL register.
    vars: Vec<Variable>,
    reg_ty: Vec<Type>,
    reg_class: Vec<AbiClass>,
    /// Opcode index → block, for every opcode that starts a basic block.
    blocks: HashMap<usize, Block>,
    native_refs: HashMap<usize, FuncRef>,
    ret_class: AbiClass,
    /// Whether the block currently being filled already has a terminator.
    terminated: bool,
}

impl Lowerer<'_, '_> {
    fn run(&mut self) -> Result<()> {
        let ops = &self.func.ops;
        self.prepare_registers()?;
        self.prepare_blocks();

        let entry = self.b.create_block();
        self.b.append_block_params_for_function_params(entry);
        self.b.switch_to_block(entry);
        self.terminated = false;

        // Bind parameters, then zero-initialise every other register so no
        // `use_var` can ever hit an undefined variable (the interpreter also
        // starts registers at null/0).
        let params: Vec<Value> = self.b.block_params(entry).to_vec();
        let nargs = params.len();
        if nargs > self.vars.len() {
            bail!(
                "function declares fewer registers ({}) than arguments ({nargs})",
                self.vars.len()
            );
        }
        for (i, p) in params.into_iter().enumerate() {
            // Argument registers are declared with the same kind as the
            // signature, but coerce defensively rather than tripping
            // cranelift's type assertion on malformed bytecode.
            let want = self.reg_ty[i];
            let v = self.coerce(p, want)?;
            self.b.def_var(self.vars[i], v);
        }
        for i in nargs..self.vars.len() {
            let ty = self.reg_ty[i];
            let zero = if ty.is_float() {
                if ty == types::F32 {
                    self.b.ins().f32const(0.0)
                } else {
                    self.b.ins().f64const(0.0)
                }
            } else {
                self.b.ins().iconst(ty, 0)
            };
            self.b.def_var(self.vars[i], zero);
        }

        // Fall into the first opcode's block (opcode 0 always starts one).
        let first = self.blocks[&0];
        self.b.ins().jump(first, &[]);
        self.terminated = true;

        for i in 0..ops.len() {
            if let Some(&block) = self.blocks.get(&i) {
                if !self.terminated {
                    self.b.ins().jump(block, &[]);
                }
                self.b.switch_to_block(block);
                self.terminated = false;
            }
            if self.terminated {
                // Unreachable straight-line code after a terminator that is
                // not a block start — cannot happen (prepare_blocks marks
                // every post-terminator index), but stay defensive.
                continue;
            }
            self.lower_op(i)?;
        }

        // A function whose last opcode falls off the end: emit a default
        // return, matching the LLVM tier's `const_zero` tail. A jump target
        // one past the last opcode (legal in HL) lands in the same block.
        if let Some(&tail) = self.blocks.get(&ops.len()) {
            if !self.terminated {
                self.b.ins().jump(tail, &[]);
            }
            self.b.switch_to_block(tail);
            self.terminated = false;
        }
        if !self.terminated {
            self.emit_default_return();
        }

        self.b.seal_all_blocks();
        Ok(())
    }

    /// Consume the builder — runs cranelift-frontend's own block/seal
    /// invariant checks (debug builds) and finishes SSA construction.
    fn finish(self) {
        self.b.finalize();
    }

    fn prepare_registers(&mut self) -> Result<()> {
        for (i, r) in self.func.regs.iter().enumerate() {
            let kind = self.ctx.type_kind(r.0)?;
            let class = abi_class(kind);
            // HVOID registers exist in bytecode but never carry a value; give
            // them an integer slot so nothing can read an undefined variable.
            let ty = class.clif_type().unwrap_or(types::I64);
            let var = self.b.declare_var(ty);
            debug_assert_eq!(var, Variable::from_u32(i as u32));
            self.vars.push(var);
            self.reg_ty.push(ty);
            self.reg_class.push(class);
        }
        Ok(())
    }

    /// A block starts at opcode 0, at every jump target, and immediately
    /// after every terminator.
    fn prepare_blocks(&mut self) {
        let ops = &self.func.ops;
        let n = ops.len();
        let mut starts = vec![false; n + 1];
        starts[0] = true;
        let mut mark = |idx: i64, starts: &mut Vec<bool>| {
            if idx >= 0 && (idx as usize) < starts.len() {
                starts[idx as usize] = true;
            }
        };
        for (i, op) in ops.iter().enumerate() {
            let i64i = i as i64;
            match op {
                Opcode::JTrue { offset, .. }
                | Opcode::JFalse { offset, .. }
                | Opcode::JNull { offset, .. }
                | Opcode::JNotNull { offset, .. }
                | Opcode::JSLt { offset, .. }
                | Opcode::JSGte { offset, .. }
                | Opcode::JSGt { offset, .. }
                | Opcode::JSLte { offset, .. }
                | Opcode::JULt { offset, .. }
                | Opcode::JUGte { offset, .. }
                | Opcode::JNotLt { offset, .. }
                | Opcode::JNotGte { offset, .. }
                | Opcode::JEq { offset, .. }
                | Opcode::JNotEq { offset, .. } => {
                    mark(i64i + 1 + *offset as i64, &mut starts);
                    mark(i64i + 1, &mut starts);
                }
                Opcode::JAlways { offset } => {
                    mark(i64i + 1 + *offset as i64, &mut starts);
                    mark(i64i + 1, &mut starts);
                }
                Opcode::Switch { offsets, end, .. } => {
                    for off in offsets {
                        mark(i64i + 1 + *off as i64, &mut starts);
                    }
                    mark(i64i + 1 + *end as i64, &mut starts);
                    mark(i64i + 1, &mut starts);
                }
                Opcode::Ret { .. } => mark(i64i + 1, &mut starts),
                // NullCheck splits its own block internally but continues
                // straight-line, so no start needed here.
                _ => {}
            }
        }
        for (i, &is_start) in starts.iter().enumerate() {
            if is_start {
                let block = self.b.create_block();
                self.blocks.insert(i, block);
            }
        }
    }

    // ── Value helpers ────────────────────────────────────────────────────────

    fn reg_val(&mut self, r: Reg) -> Value {
        self.b.use_var(self.vars[r.0 as usize])
    }

    fn set_reg(&mut self, r: Reg, v: Value) -> Result<()> {
        let want = self.reg_ty[r.0 as usize];
        let v = self.coerce(v, want)?;
        self.b.def_var(self.vars[r.0 as usize], v);
        Ok(())
    }

    /// Coerce a value to `want`. Only width/representation changes that are
    /// well defined for HashLink values are accepted; anything else declines
    /// the function.
    fn coerce(&mut self, v: Value, want: Type) -> Result<Value> {
        let have = self.b.func.dfg.value_type(v);
        if have == want {
            return Ok(v);
        }
        if have.is_int() && want.is_int() {
            return Ok(if have.bits() > want.bits() {
                self.b.ins().ireduce(want, v)
            } else {
                // HashLink integers are signed; widening preserves the value.
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

    fn class_of(&self, r: Reg) -> AbiClass {
        self.reg_class[r.0 as usize]
    }

    /// `dst = obj.field`, loading exactly as many bytes as the field holds and
    /// then widening to whatever the destination register is.
    fn lower_field_load(&mut self, dst: Reg, obj: Reg, field_index: usize) -> Result<()> {
        let (off, field_ty) = self.static_field_offset(obj, field_index)?;
        let base = self.reg_val(obj);
        let raw = self.b.ins().load(field_ty, MemFlags::trusted(), base, off);
        let want = self.ty_of(dst);
        let v = if field_ty == want {
            raw
        } else {
            self.coerce(raw, want)?
        };
        self.set_reg(dst, v)
    }

    /// `obj.field = src`, narrowing to the field's width first so the store
    /// cannot spill into the neighbouring field.
    fn lower_field_store(&mut self, obj: Reg, field_index: usize, src: Reg) -> Result<()> {
        let (off, field_ty) = self.static_field_offset(obj, field_index)?;
        let base = self.reg_val(obj);
        let raw = self.reg_val(src);
        let v = if self.b.func.dfg.value_type(raw) == field_ty {
            raw
        } else {
            self.coerce(raw, field_ty)?
        };
        self.b.ins().store(MemFlags::trusted(), v, base, off);
        Ok(())
    }

    /// Compile-time byte offset and machine type of `field_index` within the
    /// object held in `obj`.
    ///
    /// Errors decline the function rather than falling back to a runtime
    /// lookup: this tier exists to be cheap, and the cases that need one
    /// (`HVIRTUAL`, `HDYNOBJ`, packed fields) are exactly the cases the LLVM
    /// tier is there to handle.
    fn static_field_offset(&self, obj: Reg, field_index: usize) -> Result<(i32, Type)> {
        let type_index = self
            .func
            .regs
            .get(obj.0 as usize)
            .ok_or_else(|| anyhow!("field access on out-of-range register {}", obj.0))?
            .0;
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
            crate::layout::field_offset_and_kind(&bytecode.types, type_index, field_index)
                .ok_or_else(|| {
                    anyhow!("no static layout for type {type_index} field {field_index}")
                })?;
        // The access must be as wide as the FIELD, not as wide as the register
        // the value lives in. Where those differ, sizing from the register
        // writes past the field and corrupts whatever is laid out next to it.
        let ty = abi_class(field_kind)
            .clif_type()
            .ok_or_else(|| anyhow!("field of kind {field_kind} has no machine type"))?;
        Ok((offset, ty))
    }

    fn ty_of(&self, r: Reg) -> Type {
        self.reg_ty[r.0 as usize]
    }

    fn block_at(&self, idx: usize) -> Result<Block> {
        self.blocks
            .get(&idx)
            .copied()
            .ok_or_else(|| anyhow!("jump target {idx} is not a block start"))
    }

    fn target(&self, i: usize, offset: i32) -> Result<Block> {
        let idx = i as i64 + 1 + offset as i64;
        if idx < 0 {
            bail!("negative jump target");
        }
        self.block_at(idx as usize)
    }

    fn emit_default_return(&mut self) {
        match self.ret_class.clif_type() {
            None => {
                self.b.ins().return_(&[]);
            }
            Some(ty) => {
                let zero = if ty == types::F64 {
                    self.b.ins().f64const(0.0)
                } else if ty == types::F32 {
                    self.b.ins().f32const(0.0)
                } else {
                    self.b.ins().iconst(ty, 0)
                };
                self.b.ins().return_(&[zero]);
            }
        }
        self.terminated = true;
    }

    // ── Opcode dispatch ──────────────────────────────────────────────────────

    fn lower_op(&mut self, i: usize) -> Result<()> {
        let op = self.func.ops[i].clone();
        match &op {
            Opcode::Label | Opcode::Nop => {}

            Opcode::Mov { dst, src } | Opcode::UnsafeCast { dst, src } => {
                let v = self.reg_val(*src);
                self.set_reg(*dst, v)?;
            }

            Opcode::Int { dst, ptr } => {
                let val = *self
                    .ctx
                    .bytecode()
                    .ints
                    .get(ptr.0)
                    .ok_or_else(|| anyhow!("int constant {} out of range", ptr.0))?;
                let v = self.b.ins().iconst(types::I32, val as i64);
                self.set_reg(*dst, v)?;
            }
            Opcode::Float { dst, ptr } => {
                let val = *self
                    .ctx
                    .bytecode()
                    .floats
                    .get(ptr.0)
                    .ok_or_else(|| anyhow!("float constant {} out of range", ptr.0))?;
                let v = self.b.ins().f64const(val);
                self.set_reg(*dst, v)?;
            }
            Opcode::Bool { dst, value } => {
                let v = self.b.ins().iconst(types::I8, i64::from(*value));
                self.set_reg(*dst, v)?;
            }
            Opcode::String { dst, ptr } => {
                let addr = self.ctx.string_ptr(ptr.0)?;
                let v = self.b.ins().iconst(types::I64, addr as i64);
                self.set_reg(*dst, v)?;
            }
            Opcode::Null { dst } => {
                let ty = self.ty_of(*dst);
                if ty.is_float() {
                    bail!("Null into float register");
                }
                let v = self.b.ins().iconst(ty, 0);
                self.set_reg(*dst, v)?;
            }

            Opcode::Add { dst, a, b } => self.arith(*dst, *a, *b, ArithOp::Add)?,
            Opcode::Sub { dst, a, b } => self.arith(*dst, *a, *b, ArithOp::Sub)?,
            Opcode::Mul { dst, a, b } => self.arith(*dst, *a, *b, ArithOp::Mul)?,
            Opcode::SDiv { dst, a, b } => self.arith(*dst, *a, *b, ArithOp::SDiv)?,
            Opcode::UDiv { dst, a, b } => self.arith(*dst, *a, *b, ArithOp::UDiv)?,
            Opcode::SMod { dst, a, b } => self.arith(*dst, *a, *b, ArithOp::SMod)?,
            Opcode::UMod { dst, a, b } => self.arith(*dst, *a, *b, ArithOp::UMod)?,
            Opcode::Shl { dst, a, b } => self.arith(*dst, *a, *b, ArithOp::Shl)?,
            Opcode::SShr { dst, a, b } => self.arith(*dst, *a, *b, ArithOp::SShr)?,
            Opcode::UShr { dst, a, b } => self.arith(*dst, *a, *b, ArithOp::UShr)?,
            Opcode::And { dst, a, b } => self.arith(*dst, *a, *b, ArithOp::And)?,
            Opcode::Or { dst, a, b } => self.arith(*dst, *a, *b, ArithOp::Or)?,
            Opcode::Xor { dst, a, b } => self.arith(*dst, *a, *b, ArithOp::Xor)?,

            Opcode::Neg { dst, src } => {
                let v = self.reg_val(*src);
                let ty = self.b.func.dfg.value_type(v);
                let r = if ty.is_float() {
                    self.b.ins().fneg(v)
                } else {
                    self.b.ins().ineg(v)
                };
                self.set_reg(*dst, r)?;
            }
            Opcode::Not { dst, src } => {
                let class = self.class_of(*src);
                let v = self.reg_val(*src);
                let r = if class == AbiClass::Bool {
                    // Logical not on a 0/1 byte (LLVM models bools as i1, so
                    // its bitwise `not` is already logical there).
                    self.b.ins().bxor_imm(v, 1)
                } else if self.b.func.dfg.value_type(v).is_int() {
                    self.b.ins().bnot(v)
                } else {
                    bail!("Not on float register");
                };
                self.set_reg(*dst, r)?;
            }
            Opcode::Incr { dst } => {
                let v = self.reg_val(*dst);
                if self.b.func.dfg.value_type(v).is_float() {
                    bail!("Incr on float register");
                }
                let r = self.b.ins().iadd_imm(v, 1);
                self.set_reg(*dst, r)?;
            }
            Opcode::Decr { dst } => {
                let v = self.reg_val(*dst);
                if self.b.func.dfg.value_type(v).is_float() {
                    bail!("Decr on float register");
                }
                let r = self.b.ins().iadd_imm(v, -1);
                self.set_reg(*dst, r)?;
            }

            // Field access, but only where the layout oracle can resolve the
            // offset at compile time — which is what makes it lowerable here at
            // all. This tier deliberately makes no object-model layout
            // decisions of its own; it consumes the same offsets the LLVM tier
            // does. Anything the oracle declines (HPACKED) or that needs the
            // dynamic type (HVIRTUAL, HDYNOBJ) declines the whole function, and
            // it keeps interpreting until the LLVM tier takes it.
            Opcode::Field { dst, obj, field } => self.lower_field_load(*dst, *obj, field.0)?,
            Opcode::SetField { obj, field, src } => self.lower_field_store(*obj, field.0, *src)?,
            // `this` is register 0 by construction.
            Opcode::GetThis { dst, field } => self.lower_field_load(*dst, Reg(0), field.0)?,
            Opcode::SetThis { field, src } => self.lower_field_store(Reg(0), field.0, *src)?,

            Opcode::GetGlobal { dst, global } => {
                if self.class_of(*dst) != AbiClass::Ptr {
                    bail!("GetGlobal into non-pointer register");
                }
                let addr = self.ctx.global_slot_addr(global.0)?;
                let base = self.b.ins().iconst(types::I64, addr as i64);
                let v = self.b.ins().load(types::I64, MemFlags::trusted(), base, 0);
                self.set_reg(*dst, v)?;
            }
            Opcode::SetGlobal { global, src } => {
                if self.class_of(*src) != AbiClass::Ptr {
                    bail!("SetGlobal from non-pointer register");
                }
                let addr = self.ctx.global_slot_addr(global.0)?;
                let base = self.b.ins().iconst(types::I64, addr as i64);
                let v = self.reg_val(*src);
                self.b.ins().store(MemFlags::trusted(), v, base, 0);
            }

            Opcode::ToInt { dst, src } => {
                let v = self.reg_val(*src);
                let ty = self.b.func.dfg.value_type(v);
                let r = if ty.is_float() {
                    // Saturating: Cranelift's non-saturating fcvt traps on
                    // NaN / out-of-range, which would abort the process where
                    // LLVM's fptosi is merely undefined.
                    self.b.ins().fcvt_to_sint_sat(types::I32, v)
                } else {
                    self.coerce(v, types::I32)?
                };
                self.set_reg(*dst, r)?;
            }
            Opcode::ToSFloat { dst, src } => {
                let v = self.reg_val(*src);
                let ty = self.b.func.dfg.value_type(v);
                let r = if ty.is_float() {
                    self.coerce(v, types::F64)?
                } else {
                    self.b.ins().fcvt_from_sint(types::F64, v)
                };
                self.set_reg(*dst, r)?;
            }
            Opcode::ToUFloat { dst, src } => {
                let v = self.reg_val(*src);
                let ty = self.b.func.dfg.value_type(v);
                let r = if ty.is_float() {
                    self.coerce(v, types::F64)?
                } else {
                    self.b.ins().fcvt_from_uint(types::F64, v)
                };
                self.set_reg(*dst, r)?;
            }

            Opcode::NullCheck { reg } => self.lower_null_check(i, *reg)?,

            Opcode::Ret { ret } => {
                if self.ret_class == AbiClass::Void {
                    self.b.ins().return_(&[]);
                } else {
                    let v = self.reg_val(*ret);
                    let want = self
                        .ret_class
                        .clif_type()
                        .ok_or_else(|| anyhow!("void return class with value"))?;
                    let src_class = self.class_of(*ret);
                    // Sub-word results are zero-extended, not sign-extended:
                    // the interpreter reads the raw word and tests `!= 0`
                    // (HBOOL) or truncates (HUI8/HUI16).
                    let v = if src_class == AbiClass::Bool
                        || src_class == AbiClass::I8
                        || src_class == AbiClass::I16
                    {
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
                }
                self.terminated = true;
            }

            Opcode::JAlways { offset } => {
                let t = self.target(i, *offset)?;
                self.b.ins().jump(t, &[]);
                self.terminated = true;
            }
            Opcode::JTrue { cond, offset } => {
                let v = self.reg_val(*cond);
                let t = self.target(i, *offset)?;
                let n = self.block_at(i + 1)?;
                self.b.ins().brif(v, t, &[], n, &[]);
                self.terminated = true;
            }
            Opcode::JFalse { cond, offset } => {
                let v = self.reg_val(*cond);
                let t = self.target(i, *offset)?;
                let n = self.block_at(i + 1)?;
                self.b.ins().brif(v, n, &[], t, &[]);
                self.terminated = true;
            }
            Opcode::JNull { reg, offset } => self.lower_null_jump(i, *reg, *offset, true)?,
            Opcode::JNotNull { reg, offset } => self.lower_null_jump(i, *reg, *offset, false)?,

            Opcode::JSLt { a, b, offset } => {
                self.cmp_jump(i, *a, *b, IntCC::SignedLessThan, FloatCC::LessThan, *offset)?
            }
            Opcode::JSGte { a, b, offset } => self.cmp_jump(
                i,
                *a,
                *b,
                IntCC::SignedGreaterThanOrEqual,
                FloatCC::GreaterThanOrEqual,
                *offset,
            )?,
            Opcode::JSGt { a, b, offset } => self.cmp_jump(
                i,
                *a,
                *b,
                IntCC::SignedGreaterThan,
                FloatCC::GreaterThan,
                *offset,
            )?,
            Opcode::JSLte { a, b, offset } => self.cmp_jump(
                i,
                *a,
                *b,
                IntCC::SignedLessThanOrEqual,
                FloatCC::LessThanOrEqual,
                *offset,
            )?,
            Opcode::JULt { a, b, offset } => self.cmp_jump(
                i,
                *a,
                *b,
                IntCC::UnsignedLessThan,
                FloatCC::LessThan,
                *offset,
            )?,
            Opcode::JUGte { a, b, offset } => self.cmp_jump(
                i,
                *a,
                *b,
                IntCC::UnsignedGreaterThanOrEqual,
                FloatCC::GreaterThanOrEqual,
                *offset,
            )?,
            // !(a < b) is a >= b; !(a >= b) is a < b — same rewrite the LLVM
            // tier applies.
            Opcode::JNotLt { a, b, offset } => self.cmp_jump(
                i,
                *a,
                *b,
                IntCC::SignedGreaterThanOrEqual,
                FloatCC::GreaterThanOrEqual,
                *offset,
            )?,
            Opcode::JNotGte { a, b, offset } => {
                self.cmp_jump(i, *a, *b, IntCC::SignedLessThan, FloatCC::LessThan, *offset)?
            }
            Opcode::JEq { a, b, offset } => {
                self.cmp_jump(i, *a, *b, IntCC::Equal, FloatCC::Equal, *offset)?
            }
            Opcode::JNotEq { a, b, offset } => {
                self.cmp_jump(i, *a, *b, IntCC::NotEqual, FloatCC::NotEqual, *offset)?
            }

            Opcode::Switch { reg, offsets, .. } => {
                let v = self.reg_val(*reg);
                let idx = self.coerce(v, types::I32)?;
                // Out-of-range values fall through to the next opcode — NOT
                // to the switch's `end` label (a bug the LLVM tier already
                // paid for once).
                let default = self.block_at(i + 1)?;
                let mut calls: Vec<BlockCall> = Vec::with_capacity(offsets.len());
                for off in offsets {
                    let t = self.target(i, *off)?;
                    calls.push(BlockCall::new(t, [], &mut self.b.func.dfg.value_lists));
                }
                let def_call = BlockCall::new(default, [], &mut self.b.func.dfg.value_lists);
                let jt = self
                    .b
                    .create_jump_table(JumpTableData::new(def_call, &calls));
                self.b.ins().br_table(idx, jt);
                self.terminated = true;
            }

            Opcode::Call0 { dst, fun } => self.lower_call(*dst, fun.0, &[])?,
            Opcode::Call1 { dst, fun, arg0 } => self.lower_call(*dst, fun.0, &[*arg0])?,
            Opcode::Call2 {
                dst,
                fun,
                arg0,
                arg1,
            } => self.lower_call(*dst, fun.0, &[*arg0, *arg1])?,
            Opcode::Call3 {
                dst,
                fun,
                arg0,
                arg1,
                arg2,
            } => self.lower_call(*dst, fun.0, &[*arg0, *arg1, *arg2])?,
            Opcode::Call4 {
                dst,
                fun,
                arg0,
                arg1,
                arg2,
                arg3,
            } => self.lower_call(*dst, fun.0, &[*arg0, *arg1, *arg2, *arg3])?,
            Opcode::CallN { dst, fun, args } => self.lower_call(*dst, fun.0, args)?,

            other => bail!("opcode {} not lowerable", opcode_name(other)),
        }
        Ok(())
    }

    // ── Arithmetic ───────────────────────────────────────────────────────────

    fn arith(&mut self, dst: Reg, a: Reg, b: Reg, op: ArithOp) -> Result<()> {
        let va = self.reg_val(a);
        let vb = self.reg_val(b);
        let ta = self.b.func.dfg.value_type(va);
        let tb = self.b.func.dfg.value_type(vb);
        if ta != tb {
            bail!("mismatched operand types {ta}/{tb}");
        }
        let r = if ta.is_float() {
            match op {
                ArithOp::Add => self.b.ins().fadd(va, vb),
                ArithOp::Sub => self.b.ins().fsub(va, vb),
                ArithOp::Mul => self.b.ins().fmul(va, vb),
                ArithOp::SDiv | ArithOp::UDiv => self.b.ins().fdiv(va, vb),
                // No CLIF `frem`; fall back to the LLVM tier rather than
                // emitting a libcall whose ABI we have not verified.
                ArithOp::SMod | ArithOp::UMod => bail!("float modulo"),
                _ => bail!("bitwise op on floats"),
            }
        } else {
            match op {
                ArithOp::Add => self.b.ins().iadd(va, vb),
                ArithOp::Sub => self.b.ins().isub(va, vb),
                ArithOp::Mul => self.b.ins().imul(va, vb),
                ArithOp::And => self.b.ins().band(va, vb),
                ArithOp::Or => self.b.ins().bor(va, vb),
                ArithOp::Xor => self.b.ins().bxor(va, vb),
                // Cranelift masks the shift amount to the operand width, the
                // same as the aarch64 instruction the LLVM tier lowers to.
                ArithOp::Shl => self.b.ins().ishl(va, vb),
                ArithOp::SShr => self.b.ins().sshr(va, vb),
                ArithOp::UShr => self.b.ins().ushr(va, vb),
                ArithOp::SDiv | ArithOp::UDiv | ArithOp::SMod | ArithOp::UMod => {
                    self.guarded_div(va, vb, ta, op)
                }
            }
        };
        self.set_reg(dst, r)
    }

    /// Integer division/remainder without Cranelift's trapping edge cases.
    ///
    /// CLIF `sdiv`/`srem`/`udiv`/`urem` emit an explicit trap for a zero
    /// divisor (and, signed, for `INT_MIN / -1`) — a process abort, where the
    /// interpreter merely errors and the LLVM tier has undefined behaviour.
    /// Normalise the divisor and select the result instead: zero divisor
    /// yields 0, and `INT_MIN / -1` yields Rust's `wrapping_div`/`wrapping_rem`
    /// answers (`INT_MIN` and 0).
    fn guarded_div(&mut self, va: Value, vb: Value, ty: Type, op: ArithOp) -> Value {
        let signed = matches!(op, ArithOp::SDiv | ArithOp::SMod);
        let is_div = matches!(op, ArithOp::SDiv | ArithOp::UDiv);

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
            ArithOp::SDiv => self.b.ins().sdiv(va, safe_b),
            ArithOp::UDiv => self.b.ins().udiv(va, safe_b),
            ArithOp::SMod => self.b.ins().srem(va, safe_b),
            _ => self.b.ins().urem(va, safe_b),
        };

        let zero = self.b.ins().iconst(ty, 0);
        // Division fallback: 0 for a zero divisor, `a` (== INT_MIN) for the
        // signed overflow case. Remainder is 0 in both.
        let fallback = if is_div {
            self.b.ins().select(zero_div, zero, va)
        } else {
            zero
        };
        self.b.ins().select(bad, fallback, quot)
    }

    // ── Branches ─────────────────────────────────────────────────────────────

    fn cmp_jump(
        &mut self,
        i: usize,
        a: Reg,
        b: Reg,
        icc: IntCC,
        fcc: FloatCC,
        offset: i32,
    ) -> Result<()> {
        let class = self.class_of(a);
        let va = self.reg_val(a);
        let vb = self.reg_val(b);
        let ta = self.b.func.dfg.value_type(va);
        let tb = self.b.func.dfg.value_type(vb);
        if ta != tb {
            bail!("mismatched comparison operands {ta}/{tb}");
        }
        let cond = if ta.is_float() {
            self.b.ins().fcmp(fcc, va, vb)
        } else if class == AbiClass::Ptr {
            // Boxed values compare by content via hlp_dyn_compare; every
            // other pointer kind compares by identity — same split the LLVM
            // tier makes.
            let kind = self.ctx.reg_kind(self.func, a)?;
            if kind == hl::hl_type_kind_HDYN || kind == hl::hl_type_kind_HNULL {
                let res = self.call_dyn_compare(va, vb)?;
                self.b.ins().icmp_imm(icc, res, 0)
            } else {
                self.b.ins().icmp(icc, va, vb)
            }
        } else {
            self.b.ins().icmp(icc, va, vb)
        };
        let t = self.target(i, offset)?;
        let n = self.block_at(i + 1)?;
        self.b.ins().brif(cond, t, &[], n, &[]);
        self.terminated = true;
        Ok(())
    }

    fn lower_null_jump(&mut self, i: usize, reg: Reg, offset: i32, on_null: bool) -> Result<()> {
        let t = self.target(i, offset)?;
        let n = self.block_at(i + 1)?;
        if self.class_of(reg) != AbiClass::Ptr {
            // Non-pointer registers are never null: JNull never taken,
            // JNotNull always taken.
            let dest = if on_null { n } else { t };
            self.b.ins().jump(dest, &[]);
        } else {
            let v = self.reg_val(reg);
            let is_null = self.b.ins().icmp_imm(IntCC::Equal, v, 0);
            if on_null {
                self.b.ins().brif(is_null, t, &[], n, &[]);
            } else {
                self.b.ins().brif(is_null, n, &[], t, &[]);
            }
        }
        self.terminated = true;
        Ok(())
    }

    /// `if reg == null throw`. Throwing *out* of a Cranelift frame is sound —
    /// `hlp_error` longjmps to the interpreter's per-call trap, which is
    /// strictly outside this frame; only resuming *into* one would need
    /// `returns_twice`.
    fn lower_null_check(&mut self, _i: usize, reg: Reg) -> Result<()> {
        if self.class_of(reg) != AbiClass::Ptr {
            return Ok(()); // scalars are never null
        }
        let v = self.reg_val(reg);
        let throw_block = self.b.create_block();
        let cont_block = self.b.create_block();
        self.b.ins().brif(v, cont_block, &[], throw_block, &[]);

        self.b.switch_to_block(throw_block);
        self.call_hl_error("Null access")?;
        self.b
            .ins()
            .trap(cranelift_codegen::ir::TrapCode::unwrap_user(1));

        self.b.switch_to_block(cont_block);
        self.terminated = false;
        Ok(())
    }

    // ── Runtime helper calls ─────────────────────────────────────────────────

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

    fn call_dyn_compare(&mut self, a: Value, b: Value) -> Result<Value> {
        let addr = self.ctx.dyn_compare_addr()?;
        let sig = self.helper_sigref(&[types::I64, types::I64], Some(types::I32));
        let callee = self.b.ins().iconst(types::I64, addr as i64);
        let call = self.b.ins().call_indirect(sig, callee, &[a, b]);
        Ok(self.b.inst_results(call)[0])
    }

    fn call_hl_error(&mut self, message: &str) -> Result<()> {
        let addr = self.ctx.hl_error_addr()?;
        let msg = self.ctx.utf16_message(message);
        let sig = self.helper_sigref(&[types::I64], None);
        let callee = self.b.ins().iconst(types::I64, addr as i64);
        let msg_val = self.b.ins().iconst(types::I64, msg as i64);
        self.b.ins().call_indirect(sig, callee, &[msg_val]);
        Ok(())
    }

    // ── Calls ────────────────────────────────────────────────────────────────

    fn lower_call(&mut self, dst: Reg, target: usize, args: &[Reg]) -> Result<()> {
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
        for (idx, r) in args.iter().enumerate() {
            let want = param_classes[idx]
                .clif_type()
                .ok_or_else(|| anyhow!("void parameter"))?;
            let v = self.reg_val(*r);
            arg_vals.push(self.coerce(v, want)?);
        }

        // Primitives that are single instructions here rather than calls into
        // ash_std. Every entry in the table is unary, so a one-argument native
        // call is the only shape worth checking. See crate::intrinsics.
        if is_native && arg_vals.len() == 1 {
            if let Some(intr) = self
                .ctx
                .native_index(target)
                .map(|ni| &bytecode.natives[ni])
                .and_then(|n| crate::intrinsics::lookup(n.lib.as_str(), n.name.as_str()))
            {
                let v = self.emit_native_intrinsic(intr, arg_vals[0]);
                if self.class_of(dst) != AbiClass::Void {
                    self.set_reg(dst, v)?;
                }
                return Ok(());
            }
        }

        let result = if is_native {
            let fref = *self
                .native_refs
                .get(&target)
                .ok_or_else(|| anyhow!("native findex {target} not imported"))?;
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
            if self.class_of(dst) != AbiClass::Void {
                self.set_reg(dst, v)?;
            }
        }
        Ok(())
    }

    /// Emit a native primitive as CLIF instructions.
    ///
    /// `fcvt_to_sint_sat`, not `fcvt_to_sint`: `ash_std` converts with Rust's
    /// `as`, which clamps out-of-range values and maps NaN to zero, while the
    /// non-saturating instruction traps on exactly those inputs. Likewise
    /// `RoundHalfUp` is spelled `floor(x + 0.5)` rather than `nearest`, because
    /// HashLink's rounding disagrees with IEEE's at negative halves. See
    /// [`crate::intrinsics`].
    fn emit_native_intrinsic(
        &mut self,
        intr: crate::intrinsics::NativeIntrinsic,
        x: Value,
    ) -> Value {
        use crate::intrinsics::NativeIntrinsic as NI;

        match intr {
            NI::IsNaN => {
                // Unordered is true only when an operand is NaN.
                self.b.ins().fcmp(FloatCC::Unordered, x, x)
            }
            NI::IsFinite => {
                let abs = self.b.ins().fabs(x);
                let inf = self.b.ins().f64const(f64::INFINITY);
                // Ordered-and-less-than is false for both NaN and infinity, so
                // this single compare covers what is_finite means.
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
    /// interpreter's stub sentinels.
    ///
    /// In hybrid mode those slots hold `findex + 1` until (and unless) the
    /// LLVM tier installs a real address, so an unguarded call is the
    /// deterministic SIGBUS seen on game.hl. Sentinels are routed to
    /// `ash_jit_call_stub`, which re-enters the interpreter with the
    /// arguments spilled as raw i64 words.
    fn stub_guarded_call(
        &mut self,
        target: usize,
        args: &[Value],
        param_classes: &[AbiClass],
        ret_class: AbiClass,
    ) -> Result<Option<Value>> {
        let slot_addr = self.ctx.function_slot_addr(target)?;
        let ret_ty = ret_class.clif_type();

        let slot_base = self.b.ins().iconst(types::I64, slot_addr as i64);
        let fn_addr = self
            .b
            .ins()
            .load(types::I64, MemFlags::trusted(), slot_base, 0);
        // Null takes the stub path too: the bridge reports a lookup miss
        // rather than jumping to address zero.
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

        // Direct: a real code pointer.
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

        // Stub: spill args as raw i64 words and re-enter the interpreter.
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
        self.terminated = false;
        Ok(ret_ty.map(|_| self.b.block_params(merge_bb)[0]))
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum ArithOp {
    Add,
    Sub,
    Mul,
    SDiv,
    UDiv,
    SMod,
    UMod,
    Shl,
    SShr,
    UShr,
    And,
    Or,
    Xor,
}
