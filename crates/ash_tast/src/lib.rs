//! TAST: Haxe's typed AST as a flat file that ash reads without copying it.
//!
//! The Haxe side (a macro) writes the file once. This crate defines its
//! layout ([`format`]), reads and validates it in place ([`reader`]), builds
//! files for tests and other producers ([`builder`]), and generates the Haxe
//! writer from the same record descriptions ([`haxe`]), so the two sides
//! cannot drift apart.
//!
//! Every record is a run of little-endian words with no alignment
//! requirement, so a memory-mapped file is read as `&[Record]` directly. One
//! validation pass checks every cross-reference; after it, ids are trusted.

pub mod builder;
pub mod dump;
pub mod format;
pub mod haxe;
pub mod reader;
pub mod stack;

pub use reader::{Tast, TastError, lineage};
pub use stack::Stack;
