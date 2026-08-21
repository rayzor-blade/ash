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
//! ## Exception-safe values
//!
//! HL registers behave like memory slots across exceptional control flow, so
//! every register written inside a trap region — together with `Ref`-taken
//! registers, `Incr`/`Decr` targets, and registers that receive the caught
//! exception — is pinned to a cell. Handler blocks therefore read their
//! values through cells rather than phis, which is what keeps SSA sound
//! across edges that have no program point for a copy (there is none between
//! "throw" and "handler entry").
//!
//! Both directions of that invariant are enforced with errors rather than
//! assumed: `serialize` rejects non-trivial phis at handler blocks, and
//! `lower` rejects `IndirectCall`, which is a v1 hot-reload pass artifact
//! rather than a bytecode opcode.
//!
//! ## Module declarations
//!
//! Lowering is handed a [`module::ModuleInfo`] — the embedder's view of the
//! bytecode module — and records what it learns onto the function:
//! [`ir::Function::natives`] holds one [`module::NativeImport`] per native the
//! function references, and [`ir::Function::float_types`] holds the type refs
//! that denote HL floats. Backends consume those declarations instead of
//! re-deriving them, which is what lets Cranelift call `import_function`
//! before its `FunctionBuilder` exists. [`lower::ModuleBuilder`] keeps the
//! union of the declarations across every function it lowers.
//!
//! ## Passes
//!
//! [`passes::PassManager`] sequences the optimizations by opt level or from an
//! explicit list, running to a fixed point and reporting per-pass statistics.
//! [`passes::OptLevel::O2`] is the pipeline that cannot grow a function —
//! null-check elimination, GVN/CSE, LICM, the FMA peephole and dead-code
//! elimination. [`passes::OptLevel::O3`] runs tail-recursion elimination,
//! inlining and scalar replacement of aggregates ahead of it, in that order:
//! HL's `new C(...)` hands the fresh object to its constructor, so nothing
//! stops escaping until the constructor is inlined.
//!
//! Inlining is the one pass that needs to see past the function it is given,
//! so it asks the embedder for callee bodies through
//! [`module::ModuleInfo::callee`] and is inert without them.
//!
//! Every pass preserves [`verify`], the trap-region invariants and the cell
//! invariants; the rules each one commits to are documented on the pass
//! itself.

pub mod analysis;
pub mod ir;
pub mod lower;
pub mod module;
pub mod passes;
pub mod serialize;
pub mod vectorize;
pub mod verify;

#[cfg(test)]
mod tests;

pub use analysis::{AliasClass, CfgInfo, LoopForest, LoopId, NaturalLoop};
pub use ir::{
    BinOp, Block, BlockId, CastKind, CellData, CellId, CondKind, Effect, Function, Instr,
    MemAccess, Phi, PinReason, Terminator, TypeRef, UnOp, ValueData, ValueId,
};
pub use lower::{lower, lower_with, ModuleBuilder};
pub use module::{
    CalleeBody, ModuleInfo, ModuleTables, NativeImport, NativeTable, NoModuleInfo, NO_MODULE_INFO,
};
pub use passes::{OptLevel, Pass, PassManager, PassOptions, PassReport, PassStats};
pub use serialize::{serialize, Serialized};
pub use verify::verify;
