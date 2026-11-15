//! nAIR: compiles Haxe to AIR directly, without going through HashLink bytecode.
//!
//! The input is the typed program in a [TAST](ash_tast) file. So far the crate computes the
//! program's type table: the layout of every class, enum and interface (field order, method
//! slots, interface fields). Lowering function bodies to AIR is not written yet.
//!
//! The runtime builds objects from these layouts, and nAIR feeds the same runtime and tiers the
//! bytecode frontend does, so the table is built to match. [`hlfile`] reads a `.hl` so a test can
//! check that it does; the test checks agreement with the existing frontend and does not define
//! the table. What nAIR adds is information the bytecode never had, which later passes use.

pub mod build;
pub mod compare;
pub mod hl;
pub mod hlfile;
