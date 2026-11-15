//! Lowers the typed program in a [TAST](ash_tast) file to AIR v2.
//!
//! Terms. The TAST is the input: what the Haxe compiler knows. nAIR is AIR v2 extended with what
//! the TAST adds and the bytecode never had (call effects, constructor shape, immutable fields,
//! non-null, exact receivers, ranges, specializations), so that the AIR passes can use it. This
//! crate is the lowering from one to the other. What it emits today is plain AIR v2 with none of
//! that enrichment: the slots for it are not in the `air` crate yet, so it is not nAIR yet.
//!
//! What is here: the program's type table ([`build`]: the layout of every class, enum and
//! interface), the generic code used at concrete types ([`collect`]), and function bodies lowered
//! to AIR ([`lower`]) that [`info`] runs the AIR passes over. The lowering covers numeric code so
//! far: integers, floats and bools, locals, arithmetic, `if`, loops and direct calls. [`eval`] is
//! a small interpreter for that AIR, so a test can check what a lowered function computes.
//!
//! The runtime builds objects from the layouts, so the table is built to match them. [`hlfile`]
//! reads a `.hl` so a test can check that it does; that test checks agreement with the existing
//! frontend and does not define the table.

pub mod build;
pub mod collect;
pub mod compare;
pub mod eval;
pub mod hl;
pub mod hlfile;
pub mod info;
pub mod lower;
pub mod ty;
