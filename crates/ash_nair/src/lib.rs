//! nAIR: compiles Haxe to AIR directly, without going through HashLink bytecode.
//!
//! The input is the typed program in a [TAST](ash_tast) file. The crate computes the program's
//! type table ([`build`]: the layout of every class, enum and interface), finds where generic
//! code is used at concrete types ([`collect`]), and lowers function bodies to AIR ([`lower`]).
//! The lowering covers numeric code so far: integers, floats and bools, locals, arithmetic,
//! `if`, loops and direct calls. [`eval`] is a small interpreter for that AIR, so a test can
//! check what a lowered function computes.
//!
//! The runtime builds objects from the layouts, and nAIR feeds the same runtime and tiers the
//! bytecode frontend does, so the table is built to match. [`hlfile`] reads a `.hl` so a test can
//! check that it does; the test checks agreement with the existing frontend and does not define
//! the table. What nAIR adds is information the bytecode never had, which later passes use.

pub mod build;
pub mod collect;
pub mod compare;
pub mod eval;
pub mod hl;
pub mod hlfile;
pub mod info;
pub mod lower;
pub mod ty;
