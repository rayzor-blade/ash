//! # Telling LLVM what cannot alias what
//!
//! Every load and store the JIT emitted was untagged, so LLVM had to assume
//! any store might clobber any load. That is the difference between a loop
//! that reloads its invariants and one that does not. For
//! `a[0] = a[0] * 31 + (i % 8)` over 100M iterations, the optimized body was
//!
//! ```text
//!   %data = load ptr, ptr %datap     ; a.bytes, reloaded every iteration
//!   %v    = load i32, ptr %data      ; a[0]
//!   %m    = mul i32 %v, 31
//!   store i32 %m, ptr %data
//! ```
//!
//! and with the metadata below it is
//!
//! ```text
//!   %m = mul i32 %m1, 31             ; that is the whole loop
//! ```
//!
//! — `a.bytes` hoisted to the preheader, the element promoted to a register,
//! the store sunk past the exit. Three memory operations an iteration became
//! none, from metadata alone.
//!
//! ## The tree
//!
//! Flat, with one type descriptor per disjoint storage kind under a single
//! root. Two accesses alias when their descriptors are equal or one descends
//! from the other, so siblings are declared disjoint and anything left
//! untagged aliases everything.
//!
//! Object fields are keyed by **byte offset**, not by `(type, field index)`.
//! That is what makes it sound under inheritance: a subclass keeps its
//! parent's field offsets, so `a.x` through a base-typed register and `b.x`
//! through a derived one are the same storage and must get the same tag. Two
//! unrelated classes with different fields at one offset then share a tag and
//! are treated as aliasing, which is merely conservative.
//!
//! ## What is deliberately left untagged
//!
//! `DynGet`/`DynSet` resolve a field by name at run time and can land on any
//! object field, so they get no tag and therefore alias everything —
//! [`air::v2::analysis::AliasClass`] records the same exception. Calls,
//! `Setref` through a taken reference and inline asm are likewise untagged.
//! Array payloads and raw byte buffers share one descriptor rather than the
//! two the IR distinguishes: a `bytes` pointer obtained from an array and
//! indexed directly is the same memory reached two ways, and the win here
//! comes from separating payloads from *headers*, not from separating the two
//! kinds of payload.

use std::cell::RefCell;
use std::collections::HashMap;

use inkwell::context::Context;
use inkwell::values::{InstructionValue, MetadataValue};

/// Whether to emit the metadata at all (`ASH_TBAA=0` to suppress).
///
/// A kill switch rather than a build flag so the two can be compared in one
/// binary: alias metadata changes what the optimizer may do, not what the
/// program means, so an A/B that also rebuilds is measuring two things.
fn emit_enabled() -> bool {
    static CELL: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *CELL.get_or_init(|| !matches!(std::env::var("ASH_TBAA").as_deref(), Ok("0") | Ok("off")))
}

pub struct TbaaTree<'ctx> {
    kind_id: u32,
    root: MetadataValue<'ctx>,
    /// Access tags for object fields, by byte offset.
    obj_fields: RefCell<HashMap<i32, MetadataValue<'ctx>>>,
    /// Array elements and raw byte buffers.
    payload: MetadataValue<'ctx>,
    /// A `varray`'s element count, which lives in the header beside the data.
    array_len: MetadataValue<'ctx>,
}

impl<'ctx> TbaaTree<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let root = context.metadata_node(&[context.metadata_string("ash").into()]);
        let payload = Self::access_tag(context, root, "payload");
        let array_len = Self::access_tag(context, root, "arraylen");
        Self {
            kind_id: context.get_kind_id("tbaa"),
            root,
            obj_fields: RefCell::new(HashMap::new()),
            payload,
            array_len,
        }
    }

    /// A descriptor `!{name, parent}` plus the access tag `!{desc, desc, 0}`
    /// that instructions actually carry.
    fn access_tag(
        context: &'ctx Context,
        root: MetadataValue<'ctx>,
        name: &str,
    ) -> MetadataValue<'ctx> {
        let desc = context.metadata_node(&[context.metadata_string(name).into(), root.into()]);
        context.metadata_node(&[
            desc.into(),
            desc.into(),
            context.i64_type().const_int(0, false).into(),
        ])
    }

    /// The tag for an object field at `offset` bytes, created on first use.
    pub fn obj_field(&self, context: &'ctx Context, offset: i32) -> MetadataValue<'ctx> {
        if let Some(md) = self.obj_fields.borrow().get(&offset) {
            return *md;
        }
        let md = Self::access_tag(context, self.root, &format!("objfield.{offset}"));
        self.obj_fields.borrow_mut().insert(offset, md);
        md
    }

    pub fn payload(&self) -> MetadataValue<'ctx> {
        self.payload
    }

    pub fn array_len(&self) -> MetadataValue<'ctx> {
        self.array_len
    }

    /// Attach a tag. Silently does nothing for a value that is not an
    /// instruction, which is what a folded constant load comes back as.
    pub fn tag(&self, inst: InstructionValue<'ctx>, md: MetadataValue<'ctx>) {
        if !emit_enabled() {
            return;
        }
        let _ = inst.set_metadata(md, self.kind_id);
    }
}
