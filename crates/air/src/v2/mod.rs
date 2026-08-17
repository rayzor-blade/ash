//! # AIR v2 — typed phi-SSA mid-level IR for HashLink bytecode
//!
//! AIR v2 replaces the v1 "annotate opcodes in place, never move anything"
//! model (frozen jump offsets, Nop-based elimination, re-derived operands)
//! with a real mid-level IR:
//!
//! * **Typed values** — every SSA value carries its HL type ([`ir::TypeRef`],
//!   an index into the module type table). Values live in a dense `u32`-indexed
//!   table ([`ir::Function::values`]).
//! * **Real basic blocks** that own their instructions. Branch targets are
//!   block ids, and phi *instructions* ([`ir::Phi`]) sit at block heads.
//! * **Explicit operands** for everything v1 re-derived by pattern matching:
//!   `SetEnumField` carries its construct index (resolved once during lowering
//!   via the EnumAlloc/MakeEnum backward scan), field accesses carry the
//!   resolved `(object type, field slot)` pair.
//! * **First-class trap regions** — `Trap` is a terminator with a normal and
//!   an exceptional (handler) successor; `EndTrap` is a region-end marker that
//!   always terminates its block. Every block records the innermost active
//!   handler, and every block containing a may-throw instruction gets an
//!   exceptional CFG edge to that handler.
//! * **Effect classification** — [`ir::Effect`] `{Pure < ReadMem < Alloc <
//!   WriteMem < MayThrow < ClobberAll}` as a method on every instruction,
//!   extending v1 `opcode_info::is_pure` into a real lattice.
//! * **De-SSA with parallel-copy insertion** — non-trivial phis are supported;
//!   copy cycles are broken with fresh temporary registers, critical edges are
//!   split with dedicated copy blocks.
//! * **Pinned registers as explicit cells** — registers whose address is taken
//!   (`Ref`), that are targets of `Incr`/`Decr`, that receive trap exceptions,
//!   or that are written inside a trap region (HL registers behave like memory
//!   slots across exceptional control flow) become [`ir::CellData`] cells with
//!   explicit `CellGet`/`CellSet`/`CellIncr`/`CellDecr`/`CellRef` accesses.
//!
//! ## Pipeline
//!
//! ```text
//! Vec<Opcode> + reg types --lower()--> ir::Function (CFG + SSA)
//!                                          |  verify()
//! ir::Function --serialize()--> Vec<Opcode> + reg types (standard HL bytecode)
//! ```
//!
//! ## Invariants that hold for the serialize path
//!
//! * **Jump-offset recomputation**: the serializer never reuses input offsets.
//!   All offsets are recomputed from the final block layout; this is what
//!   breaks the v1 frozen-offset identity. Any negative (backward or self)
//!   offset targets a `Label` opcode, which the serializer inserts itself if
//!   the target block does not already start with one.
//! * **Register-type table preservation**: original registers keep their ids
//!   and types in the output register-type table verbatim; temporaries needed
//!   for parallel-copy cycle breaking are appended after them with the type of
//!   the register they shadow.
//! * **Interpreter-compatible output opcodes only**: the serializer emits only
//!   standard HL opcodes that `ash_interp` executes. Normalizations performed:
//!   `CallThis` becomes `CallMethod` with the receiver (`reg0`) prepended,
//!   `GetThis`/`SetThis` become `Field`/`SetField` on `reg0`, `Call*` arity is
//!   re-derived from the argument count, `Switch` defaults are guaranteed to
//!   be the fall-through (with a `JAlways` trampoline if the layout demands
//!   it), `Nop` and unreachable code are dropped, and `Label`s are re-derived
//!   from actual backward edges.
//!
//! ## Current limitations (documented, checked with errors — not UB)
//!
//! * Parallel copies cannot be inserted on exceptional edges (there is no
//!   program point between "throw" and "handler entry"); `serialize` rejects
//!   non-trivial phis at handler blocks. Plain `lower` output never produces
//!   them because in-region-written registers are pinned to cells.
//! * In-region pinning is conservative: *every* register written inside a trap
//!   region becomes a cell. Liveness analysis can refine this later.
//! * `IndirectCall` (a v1 hot-reload pass artifact, not a real bytecode op)
//!   is rejected by `lower`.

pub mod ir;
pub mod lower;
pub mod serialize;
pub mod verify;

#[cfg(test)]
mod tests;

pub use ir::{
    BinOp, Block, BlockId, CastKind, CellData, CellId, CondKind, Effect, Function, Instr,
    MemAccess, Phi, PinReason, Terminator, TypeRef, UnOp, ValueData, ValueId,
};
pub use lower::lower;
pub use serialize::{serialize, Serialized};
pub use verify::verify;
