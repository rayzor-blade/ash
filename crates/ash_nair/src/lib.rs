//! Lowers the typed program in a [TAST](ash_tast) file to AIR v2.
//!
//! Terms. The TAST is the input: what the Haxe compiler knows. nAIR is AIR v2 extended with what
//! the TAST adds and the bytecode never had (call effects, constructor shape, immutable fields,
//! non-null, exact receivers, ranges, specializations), so that the AIR passes can use it. This
//! crate is the lowering from one to the other. So far one kind of enrichment exists: what a
//! direct call may write (`air::v2::facts`), which [`info`] solves over the lowered functions and
//! attaches to them. The other slots are not in the `air` crate yet, so most of nAIR is still to
//! come.
//!
//! What is here: the program's type table ([`build`]: the layout of every class, enum and
//! interface), the generic code used at concrete types ([`collect`]), and function bodies lowered
//! to AIR ([`lower`]) that [`info`] runs the AIR passes over. The lowering covers numeric code and
//! objects of non-generic classes so far: integers, floats and bools, locals, arithmetic, `if`,
//! loops, direct calls, `new`, instance variables, method calls, `null`, and arrays read, written and
//! made empty. [`eval`] is a small
//! interpreter for that AIR, so a test can check what a lowered function computes. The
//! `lower_report` binary lowers each function of a TAST file on its own and counts why the ones
//! that fail do, which is how the next construct to lower is chosen.
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
pub mod immutable;
pub mod info;
pub mod lower;
pub mod ty;
