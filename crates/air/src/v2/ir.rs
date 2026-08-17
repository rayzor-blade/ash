//! Core AIR v2 data structures: typed SSA values, blocks, instructions,
//! terminators, and the effect lattice.

use std::fmt;

/// Reference to a type in the module type table.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeRef(pub u32);

/// Dense index into [`Function::values`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ValueId(pub u32);

impl ValueId {
    #[inline]
    pub fn idx(self) -> usize {
        self.0 as usize
    }
}

/// Index into [`Function::blocks`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BlockId(pub u32);

impl BlockId {
    #[inline]
    pub fn idx(self) -> usize {
        self.0 as usize
    }
}

/// Index into [`Function::cells`]. A cell is a pinned HL register that keeps
/// memory semantics (multiple assignments, address-taken) instead of being
/// promoted to SSA values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct CellId(pub u32);

impl CellId {
    #[inline]
    pub fn idx(self) -> usize {
        self.0 as usize
    }
}

/// Why a register was pinned to a cell instead of being SSA-promoted.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PinReason {
    /// Its address is taken by `Ref`.
    RefTaken,
    /// It is the target of `Incr`/`Decr`.
    IncrDecr,
    /// It receives the exception value of a `Trap`.
    TrapExc,
    /// It is written inside a trap region (HL registers are memory slots
    /// across exceptional control flow).
    TrapWritten,
}

/// Per-value metadata: the HL type of the value and the HL register it
/// originates from (which is also its de-SSA register assignment).
#[derive(Debug, Clone)]
pub struct ValueData {
    pub ty: TypeRef,
    /// Originating HL register. `serialize` assigns the value back to this
    /// register.
    pub reg: u32,
}

/// A pinned register modeled as a memory cell.
#[derive(Debug, Clone)]
pub struct CellData {
    /// The HL register backing this cell.
    pub reg: u32,
    pub ty: TypeRef,
    pub reason: PinReason,
}

/// Binary arithmetic / bitwise operations (all same-typed in HL).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
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

impl BinOp {
    /// Integer division/modulo can raise a division-by-zero error in HL.
    pub fn can_trap(self) -> bool {
        matches!(self, BinOp::SDiv | BinOp::UDiv | BinOp::SMod | BinOp::UMod)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    Neg,
    Not,
}

/// Conversion / cast kinds, mirroring the HL conversion opcodes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CastKind {
    ToDyn,
    ToSFloat,
    ToUFloat,
    ToInt,
    SafeCast,
    UnsafeCast,
    ToVirtual,
}

/// Raw memory / array access widths.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemAccess {
    I8,
    I16,
    /// `GetMem`/`SetMem`: element width given by the value register type.
    Mem,
    /// `GetArray`/`SetArray`.
    Array,
}

/// Conditional-jump condition kinds, mirroring the HL `J*` opcodes exactly
/// (including the NaN-aware `NotLt`/`NotGte` forms).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CondKind {
    True,
    False,
    Null,
    NotNull,
    SLt,
    SGte,
    SGt,
    SLte,
    ULt,
    UGte,
    NotLt,
    NotGte,
    Eq,
    NotEq,
}

impl CondKind {
    /// Unary conditions take a single operand (`b` is `None`).
    pub fn is_unary(self) -> bool {
        matches!(
            self,
            CondKind::True | CondKind::False | CondKind::Null | CondKind::NotNull
        )
    }
}

/// Effect classification lattice. Declared in increasing severity; each level
/// subsumes the capabilities of the levels below it:
///
/// `Pure < ReadMem < Alloc < WriteMem < MayThrow < ClobberAll`
///
/// * `Pure` — value depends only on operands; freely removable if dead.
/// * `ReadMem` — reads heap/global state; removable if dead, not movable
///   across writes.
/// * `Alloc` — allocates GC memory (observable via GC pressure only).
/// * `WriteMem` — writes heap/global/cell state.
/// * `MayThrow` — may raise an HL exception (ordering with trap regions
///   matters).
/// * `ClobberAll` — may read/write anything and throw (calls, region markers,
///   inline asm).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Effect {
    Pure,
    ReadMem,
    Alloc,
    WriteMem,
    MayThrow,
    ClobberAll,
}

/// A phi instruction at a block head: `dst = phi[(pred, value), ...]`.
/// `incoming` has exactly one entry per predecessor block (set semantics;
/// order is not significant).
#[derive(Debug, Clone)]
pub struct Phi {
    pub dst: ValueId,
    pub incoming: Vec<(BlockId, ValueId)>,
}

/// Non-terminator instructions. These mirror HL opcode semantics but with
/// explicit operands for everything v1 re-derived:
///
/// * `SetEnumField` carries its construct index.
/// * `FieldGet`/`FieldSet` carry the resolved `(object type, field slot)`.
/// * `CallThis`/`GetThis`/`SetThis` are normalized to `CallMethod`/
///   `FieldGet`/`FieldSet` with the `this` value explicit.
/// * `VirtualClosure.field` is an immediate proto index (the v1
///   `opcode_info::reads` treats it as a register read, which is wrong).
#[derive(Debug, Clone)]
pub enum Instr {
    /// Value of HL register `reg` at function entry (argument or default
    /// initialized). Entry block only; emits no code.
    Param {
        dst: ValueId,
        reg: u32,
    },
    /// `dst = src` (HL `Mov`).
    Copy {
        dst: ValueId,
        src: ValueId,
    },
    /// i32 constant pool load.
    Int {
        dst: ValueId,
        idx: usize,
    },
    /// f64 constant pool load.
    Float {
        dst: ValueId,
        idx: usize,
    },
    Bool {
        dst: ValueId,
        value: bool,
    },
    Bytes {
        dst: ValueId,
        idx: usize,
    },
    String {
        dst: ValueId,
        idx: usize,
    },
    Null {
        dst: ValueId,
    },
    BinOp {
        op: BinOp,
        dst: ValueId,
        a: ValueId,
        b: ValueId,
    },
    UnOp {
        op: UnOp,
        dst: ValueId,
        src: ValueId,
    },
    /// Direct call by findex (arity re-derived at serialization).
    Call {
        dst: ValueId,
        fun: usize,
        args: Vec<ValueId>,
    },
    /// Method call through the receiver's vtable slot `field`; `args[0]` is
    /// the receiver. `CallThis` lowers to this with `this` prepended.
    CallMethod {
        dst: ValueId,
        field: usize,
        args: Vec<ValueId>,
    },
    CallClosure {
        dst: ValueId,
        fun: ValueId,
        args: Vec<ValueId>,
    },
    StaticClosure {
        dst: ValueId,
        fun: usize,
    },
    InstanceClosure {
        dst: ValueId,
        fun: usize,
        obj: ValueId,
    },
    /// `field` is an immediate proto index, not a register.
    VirtualClosure {
        dst: ValueId,
        obj: ValueId,
        field: usize,
    },
    GetGlobal {
        dst: ValueId,
        global: usize,
    },
    SetGlobal {
        global: usize,
        src: ValueId,
    },
    /// `dst = obj.field` with the object type resolved at lowering time.
    FieldGet {
        dst: ValueId,
        obj: ValueId,
        obj_ty: TypeRef,
        field: usize,
    },
    FieldSet {
        obj: ValueId,
        obj_ty: TypeRef,
        field: usize,
        src: ValueId,
    },
    /// `field` is a string-pool index.
    DynGet {
        dst: ValueId,
        obj: ValueId,
        field: usize,
    },
    DynSet {
        obj: ValueId,
        field: usize,
        src: ValueId,
    },
    Cast {
        kind: CastKind,
        dst: ValueId,
        src: ValueId,
    },
    NullCheck {
        value: ValueId,
    },
    /// Ends the innermost trap region. Writes null into the trap's exception
    /// cell (matching `ash_interp`). Must be the last instruction of its
    /// block.
    EndTrap {
        cell: CellId,
    },
    MemGet {
        kind: MemAccess,
        dst: ValueId,
        base: ValueId,
        index: ValueId,
    },
    MemSet {
        kind: MemAccess,
        base: ValueId,
        index: ValueId,
        src: ValueId,
    },
    /// Allocate an object of the dst register's type.
    New {
        dst: ValueId,
    },
    ArraySize {
        dst: ValueId,
        array: ValueId,
    },
    /// `dst = type ty` (HL `Type`).
    TypeConst {
        dst: ValueId,
        ty: TypeRef,
    },
    GetType {
        dst: ValueId,
        src: ValueId,
    },
    GetTID {
        dst: ValueId,
        src: ValueId,
    },
    /// `dst = *src`.
    Unref {
        dst: ValueId,
        src: ValueId,
    },
    /// `*r = value` (HL `Setref`).
    SetRef {
        r: ValueId,
        value: ValueId,
    },
    RefData {
        dst: ValueId,
        src: ValueId,
    },
    RefOffset {
        dst: ValueId,
        base: ValueId,
        offset: ValueId,
    },
    MakeEnum {
        dst: ValueId,
        construct: usize,
        args: Vec<ValueId>,
    },
    EnumAlloc {
        dst: ValueId,
        construct: usize,
    },
    EnumIndex {
        dst: ValueId,
        value: ValueId,
    },
    EnumField {
        dst: ValueId,
        value: ValueId,
        construct: usize,
        field: usize,
    },
    /// Carries the construct index explicitly (resolved once during lowering
    /// via the EnumAlloc/MakeEnum backward scan).
    SetEnumField {
        value: ValueId,
        construct: usize,
        field: usize,
        src: ValueId,
    },
    /// Read the current value of a pinned register cell.
    CellGet {
        dst: ValueId,
        cell: CellId,
    },
    /// Write a value into a pinned register cell.
    CellSet {
        cell: CellId,
        src: ValueId,
    },
    /// `cell++` (HL `Incr`).
    CellIncr {
        cell: CellId,
    },
    /// `cell--` (HL `Decr`).
    CellDecr {
        cell: CellId,
    },
    /// `dst = &cell` (HL `Ref`).
    CellRef {
        dst: ValueId,
        cell: CellId,
    },
    Assert,
    Prefetch {
        value: ValueId,
        field: usize,
        mode: i32,
    },
    Asm {
        mode: i32,
        value: i32,
        reg: u32,
    },
}

impl Instr {
    /// The value defined by this instruction, if any.
    pub fn dst(&self) -> Option<ValueId> {
        match self {
            Instr::Param { dst, .. }
            | Instr::Copy { dst, .. }
            | Instr::Int { dst, .. }
            | Instr::Float { dst, .. }
            | Instr::Bool { dst, .. }
            | Instr::Bytes { dst, .. }
            | Instr::String { dst, .. }
            | Instr::Null { dst }
            | Instr::BinOp { dst, .. }
            | Instr::UnOp { dst, .. }
            | Instr::Call { dst, .. }
            | Instr::CallMethod { dst, .. }
            | Instr::CallClosure { dst, .. }
            | Instr::StaticClosure { dst, .. }
            | Instr::InstanceClosure { dst, .. }
            | Instr::VirtualClosure { dst, .. }
            | Instr::GetGlobal { dst, .. }
            | Instr::FieldGet { dst, .. }
            | Instr::DynGet { dst, .. }
            | Instr::Cast { dst, .. }
            | Instr::MemGet { dst, .. }
            | Instr::New { dst }
            | Instr::ArraySize { dst, .. }
            | Instr::TypeConst { dst, .. }
            | Instr::GetType { dst, .. }
            | Instr::GetTID { dst, .. }
            | Instr::Unref { dst, .. }
            | Instr::RefData { dst, .. }
            | Instr::RefOffset { dst, .. }
            | Instr::MakeEnum { dst, .. }
            | Instr::EnumAlloc { dst, .. }
            | Instr::EnumIndex { dst, .. }
            | Instr::EnumField { dst, .. }
            | Instr::CellGet { dst, .. }
            | Instr::CellRef { dst, .. } => Some(*dst),
            Instr::SetGlobal { .. }
            | Instr::FieldSet { .. }
            | Instr::DynSet { .. }
            | Instr::NullCheck { .. }
            | Instr::EndTrap { .. }
            | Instr::MemSet { .. }
            | Instr::SetRef { .. }
            | Instr::SetEnumField { .. }
            | Instr::CellSet { .. }
            | Instr::CellIncr { .. }
            | Instr::CellDecr { .. }
            | Instr::Assert
            | Instr::Prefetch { .. }
            | Instr::Asm { .. } => None,
        }
    }

    /// The values read by this instruction.
    pub fn uses(&self) -> Vec<ValueId> {
        match self {
            Instr::Param { .. }
            | Instr::Int { .. }
            | Instr::Float { .. }
            | Instr::Bool { .. }
            | Instr::Bytes { .. }
            | Instr::String { .. }
            | Instr::Null { .. }
            | Instr::StaticClosure { .. }
            | Instr::GetGlobal { .. }
            | Instr::New { .. }
            | Instr::TypeConst { .. }
            | Instr::EnumAlloc { .. }
            | Instr::EndTrap { .. }
            | Instr::CellGet { .. }
            | Instr::CellIncr { .. }
            | Instr::CellDecr { .. }
            | Instr::CellRef { .. }
            | Instr::Assert
            | Instr::Asm { .. } => vec![],
            Instr::Copy { src, .. }
            | Instr::UnOp { src, .. }
            | Instr::Cast { src, .. }
            | Instr::GetType { src, .. }
            | Instr::GetTID { src, .. }
            | Instr::Unref { src, .. }
            | Instr::RefData { src, .. }
            | Instr::SetGlobal { src, .. }
            | Instr::CellSet { src, .. } => vec![*src],
            Instr::BinOp { a, b, .. } => vec![*a, *b],
            Instr::Call { args, .. } | Instr::CallMethod { args, .. } => args.clone(),
            Instr::CallClosure { fun, args, .. } => {
                let mut v = vec![*fun];
                v.extend(args.iter().copied());
                v
            }
            Instr::InstanceClosure { obj, .. } => vec![*obj],
            Instr::VirtualClosure { obj, .. } => vec![*obj],
            Instr::FieldGet { obj, .. } | Instr::DynGet { obj, .. } => vec![*obj],
            Instr::FieldSet { obj, src, .. } | Instr::DynSet { obj, src, .. } => {
                vec![*obj, *src]
            }
            Instr::NullCheck { value } => vec![*value],
            Instr::MemGet { base, index, .. } => vec![*base, *index],
            Instr::MemSet {
                base, index, src, ..
            } => vec![*base, *index, *src],
            Instr::ArraySize { array, .. } => vec![*array],
            Instr::SetRef { r, value } => vec![*r, *value],
            Instr::RefOffset { base, offset, .. } => vec![*base, *offset],
            Instr::MakeEnum { args, .. } => args.clone(),
            Instr::EnumIndex { value, .. } | Instr::EnumField { value, .. } => vec![*value],
            Instr::SetEnumField { value, src, .. } => vec![*value, *src],
            Instr::Prefetch { value, .. } => vec![*value],
        }
    }

    /// Effect classification (see [`Effect`]).
    pub fn effect(&self) -> Effect {
        match self {
            Instr::Param { .. }
            | Instr::Copy { .. }
            | Instr::Int { .. }
            | Instr::Float { .. }
            | Instr::Bool { .. }
            | Instr::Bytes { .. }
            | Instr::String { .. }
            | Instr::Null { .. }
            | Instr::UnOp { .. }
            | Instr::TypeConst { .. }
            | Instr::CellRef { .. }
            | Instr::RefOffset { .. }
            | Instr::Prefetch { .. } => Effect::Pure,
            Instr::BinOp { op, .. } => {
                if op.can_trap() {
                    Effect::MayThrow
                } else {
                    Effect::Pure
                }
            }
            Instr::Cast { kind, .. } => match kind {
                CastKind::ToSFloat
                | CastKind::ToUFloat
                | CastKind::ToInt
                | CastKind::UnsafeCast => Effect::Pure,
                CastKind::ToDyn | CastKind::ToVirtual => Effect::Alloc,
                CastKind::SafeCast => Effect::MayThrow,
            },
            Instr::GetGlobal { .. }
            | Instr::FieldGet { .. }
            | Instr::ArraySize { .. }
            | Instr::MemGet { .. }
            | Instr::GetType { .. }
            | Instr::GetTID { .. }
            | Instr::Unref { .. }
            | Instr::RefData { .. }
            | Instr::EnumIndex { .. }
            | Instr::EnumField { .. }
            | Instr::CellGet { .. } => Effect::ReadMem,
            Instr::New { .. }
            | Instr::MakeEnum { .. }
            | Instr::EnumAlloc { .. }
            | Instr::StaticClosure { .. }
            | Instr::InstanceClosure { .. }
            | Instr::VirtualClosure { .. } => Effect::Alloc,
            Instr::SetGlobal { .. }
            | Instr::FieldSet { .. }
            | Instr::MemSet { .. }
            | Instr::SetRef { .. }
            | Instr::SetEnumField { .. }
            | Instr::CellSet { .. }
            | Instr::CellIncr { .. }
            | Instr::CellDecr { .. } => Effect::WriteMem,
            Instr::NullCheck { .. }
            | Instr::DynGet { .. }
            | Instr::DynSet { .. }
            | Instr::Assert => Effect::MayThrow,
            Instr::Call { .. }
            | Instr::CallMethod { .. }
            | Instr::CallClosure { .. }
            | Instr::EndTrap { .. }
            | Instr::Asm { .. } => Effect::ClobberAll,
        }
    }

    /// True if this instruction can raise an HL exception (drives exceptional
    /// CFG edges into trap handlers). Note `EndTrap` is `ClobberAll` for
    /// motion purposes but never throws.
    pub fn may_throw(&self) -> bool {
        match self {
            Instr::Call { .. }
            | Instr::CallMethod { .. }
            | Instr::CallClosure { .. }
            | Instr::NullCheck { .. }
            | Instr::DynGet { .. }
            | Instr::DynSet { .. }
            | Instr::Assert
            | Instr::Asm { .. } => true,
            Instr::BinOp { op, .. } => op.can_trap(),
            Instr::Cast { kind, .. } => *kind == CastKind::SafeCast,
            _ => false,
        }
    }
}

/// Block terminators. Every block ends with exactly one.
#[derive(Debug, Clone)]
pub enum Terminator {
    Ret {
        value: ValueId,
    },
    Jump {
        target: BlockId,
    },
    /// `if cond(a[, b]) goto if_true else goto if_false`.
    CondJump {
        cond: CondKind,
        a: ValueId,
        b: Option<ValueId>,
        if_true: BlockId,
        if_false: BlockId,
    },
    /// `goto targets[value]`, out-of-range values go to `default`
    /// (HL `Switch` falls through; the serializer guarantees `default` is the
    /// fall-through, inserting a `JAlways` trampoline if necessary).
    Switch {
        value: ValueId,
        targets: Vec<BlockId>,
        default: BlockId,
    },
    Throw {
        exc: ValueId,
    },
    Rethrow {
        exc: ValueId,
    },
    /// Open a trap region: control continues at `normal`; if an exception is
    /// raised inside the region it is caught at `handler` with the exception
    /// value written into `exc_cell`.
    Trap {
        exc_cell: CellId,
        handler: BlockId,
        normal: BlockId,
    },
}

impl Terminator {
    /// Ordered successor list (may contain duplicates for degenerate
    /// branches; use [`Function::succ_blocks`] for the deduplicated CFG view).
    pub fn successors(&self) -> Vec<BlockId> {
        match self {
            Terminator::Ret { .. } | Terminator::Throw { .. } | Terminator::Rethrow { .. } => {
                vec![]
            }
            Terminator::Jump { target } => vec![*target],
            Terminator::CondJump {
                if_true, if_false, ..
            } => vec![*if_true, *if_false],
            Terminator::Switch {
                targets, default, ..
            } => {
                let mut v = targets.clone();
                v.push(*default);
                v
            }
            Terminator::Trap {
                normal, handler, ..
            } => vec![*normal, *handler],
        }
    }

    /// Values read by the terminator.
    pub fn uses(&self) -> Vec<ValueId> {
        match self {
            Terminator::Ret { value } => vec![*value],
            Terminator::Jump { .. } | Terminator::Trap { .. } => vec![],
            Terminator::CondJump { a, b, .. } => {
                let mut v = vec![*a];
                if let Some(b) = b {
                    v.push(*b);
                }
                v
            }
            Terminator::Switch { value, .. } => vec![*value],
            Terminator::Throw { exc } | Terminator::Rethrow { exc } => vec![*exc],
        }
    }
}

/// A basic block owning its phis, instructions and terminator.
#[derive(Debug, Clone)]
pub struct Block {
    pub phis: Vec<Phi>,
    pub instrs: Vec<Instr>,
    pub term: Terminator,
    /// Innermost active trap handler covering this block, if any. Together
    /// with [`Instr::may_throw`] this induces the exceptional CFG edges.
    pub handler: Option<BlockId>,
}

/// An AIR v2 function in SSA form.
///
/// Block 0 is the entry block (synthetic: `Param` definitions plus a `Jump`
/// to the first lowered block; it emits no code).
#[derive(Debug, Clone)]
pub struct Function {
    pub values: Vec<ValueData>,
    pub cells: Vec<CellData>,
    pub blocks: Vec<Block>,
    /// The original HL register-type table, preserved verbatim for the
    /// serialize path.
    pub reg_types: Vec<TypeRef>,
}

impl Function {
    pub fn new_value(&mut self, ty: TypeRef, reg: u32) -> ValueId {
        let id = ValueId(self.values.len() as u32);
        self.values.push(ValueData { ty, reg });
        id
    }

    #[inline]
    pub fn value_ty(&self, v: ValueId) -> TypeRef {
        self.values[v.idx()].ty
    }

    /// The de-SSA register assignment of a value.
    #[inline]
    pub fn value_reg(&self, v: ValueId) -> u32 {
        self.values[v.idx()].reg
    }

    /// True if the block contains an instruction that may raise, or ends in
    /// `Throw`/`Rethrow`.
    pub fn block_may_throw(&self, b: BlockId) -> bool {
        let blk = &self.blocks[b.idx()];
        blk.instrs.iter().any(|i| i.may_throw())
            || matches!(
                blk.term,
                Terminator::Throw { .. } | Terminator::Rethrow { .. }
            )
    }

    /// Ordered, deduplicated successor list of a block, including the
    /// exceptional edge to the innermost handler when the block can throw.
    pub fn succ_blocks(&self, b: BlockId) -> Vec<BlockId> {
        let blk = &self.blocks[b.idx()];
        let mut out: Vec<BlockId> = Vec::new();
        for s in blk.term.successors() {
            if !out.contains(&s) {
                out.push(s);
            }
        }
        if let Some(h) = blk.handler {
            if self.block_may_throw(b) && !out.contains(&h) {
                out.push(h);
            }
        }
        out
    }

    /// Deduplicated predecessor lists for all blocks.
    pub fn preds(&self) -> Vec<Vec<BlockId>> {
        let mut preds: Vec<Vec<BlockId>> = vec![vec![]; self.blocks.len()];
        for b in 0..self.blocks.len() {
            let bid = BlockId(b as u32);
            for s in self.succ_blocks(bid) {
                if !preds[s.idx()].contains(&bid) {
                    preds[s.idx()].push(bid);
                }
            }
        }
        preds
    }

    /// Human-readable dump for debugging and test failure diagnostics.
    pub fn dump(&self) -> String {
        use fmt::Write;
        let mut s = String::new();
        for (bi, blk) in self.blocks.iter().enumerate() {
            let _ = write!(s, "b{}:", bi);
            if let Some(h) = blk.handler {
                let _ = write!(s, "  [handler b{}]", h.0);
            }
            let _ = writeln!(s);
            for phi in &blk.phis {
                let _ = write!(
                    s,
                    "  v{}(r{}) = phi",
                    phi.dst.0,
                    self.values[phi.dst.idx()].reg
                );
                for (p, v) in &phi.incoming {
                    let _ = write!(s, " [b{}: v{}]", p.0, v.0);
                }
                let _ = writeln!(s);
            }
            for ins in &blk.instrs {
                let _ = writeln!(s, "  {:?}", ins);
            }
            let _ = writeln!(s, "  {:?}", blk.term);
        }
        s
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn effect_lattice_order() {
        assert!(Effect::Pure < Effect::ReadMem);
        assert!(Effect::ReadMem < Effect::Alloc);
        assert!(Effect::Alloc < Effect::WriteMem);
        assert!(Effect::WriteMem < Effect::MayThrow);
        assert!(Effect::MayThrow < Effect::ClobberAll);
    }

    #[test]
    fn effect_classification() {
        let call = Instr::Call {
            dst: ValueId(0),
            fun: 3,
            args: vec![],
        };
        assert_eq!(call.effect(), Effect::ClobberAll);
        assert!(call.may_throw());

        let add = Instr::BinOp {
            op: BinOp::Add,
            dst: ValueId(0),
            a: ValueId(1),
            b: ValueId(2),
        };
        assert_eq!(add.effect(), Effect::Pure);
        assert!(!add.may_throw());

        let sdiv = Instr::BinOp {
            op: BinOp::SDiv,
            dst: ValueId(0),
            a: ValueId(1),
            b: ValueId(2),
        };
        assert_eq!(sdiv.effect(), Effect::MayThrow);
        assert!(sdiv.may_throw());

        let cg = Instr::CellGet {
            dst: ValueId(0),
            cell: CellId(0),
        };
        assert_eq!(cg.effect(), Effect::ReadMem);
        assert!(!cg.may_throw());

        let et = Instr::EndTrap { cell: CellId(0) };
        assert_eq!(et.effect(), Effect::ClobberAll);
        assert!(!et.may_throw(), "EndTrap clobbers but never throws");

        let sc = Instr::Cast {
            kind: CastKind::SafeCast,
            dst: ValueId(0),
            src: ValueId(1),
        };
        assert!(sc.may_throw());
    }

    #[test]
    fn terminator_successor_order() {
        let t = Terminator::CondJump {
            cond: CondKind::SLt,
            a: ValueId(0),
            b: Some(ValueId(1)),
            if_true: BlockId(4),
            if_false: BlockId(2),
        };
        assert_eq!(t.successors(), vec![BlockId(4), BlockId(2)]);

        let sw = Terminator::Switch {
            value: ValueId(0),
            targets: vec![BlockId(1), BlockId(2)],
            default: BlockId(3),
        };
        assert_eq!(sw.successors(), vec![BlockId(1), BlockId(2), BlockId(3)]);

        let trap = Terminator::Trap {
            exc_cell: CellId(0),
            handler: BlockId(5),
            normal: BlockId(1),
        };
        assert_eq!(trap.successors(), vec![BlockId(1), BlockId(5)]);
    }

    #[test]
    fn condkind_unary() {
        assert!(CondKind::True.is_unary());
        assert!(CondKind::NotNull.is_unary());
        assert!(!CondKind::SLt.is_unary());
        assert!(!CondKind::Eq.is_unary());
    }

    #[test]
    fn uses_and_dst() {
        let i = Instr::SetEnumField {
            value: ValueId(3),
            construct: 1,
            field: 0,
            src: ValueId(7),
        };
        assert_eq!(i.uses(), vec![ValueId(3), ValueId(7)]);
        assert_eq!(i.dst(), None);

        let f = Instr::FieldGet {
            dst: ValueId(1),
            obj: ValueId(2),
            obj_ty: TypeRef(9),
            field: 4,
        };
        assert_eq!(f.uses(), vec![ValueId(2)]);
        assert_eq!(f.dst(), Some(ValueId(1)));
    }
}
