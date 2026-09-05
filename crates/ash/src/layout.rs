//! # Object layout oracle
//!
//! Computes, at compile time, the byte offsets `hlp_get_obj_rt` would compute
//! at run time. Both JIT tiers use it to turn a field access into a
//! constant-offset load instead of a call.
//!
//! ## Why this is worth having
//!
//! The generated code for `o.x` currently loads the object's `hl_type*`, calls
//! `hlp_get_obj_rt`, loads `fields_indexes` out of the result, loads the offset
//! out of *that*, and only then touches the field. The callee is about five
//! instructions behind a cached pointer, so the cost is not the work it does —
//! it is the call itself, of which the profiler counts 1.76 billion in one
//! mandelbrot run (18 per inner iteration), 43% of nbody and 14% of mandelbrot.
//! An opaque call is also a barrier: neither backend can hoist, CSE or reorder
//! anything across it, so the real cost is larger than the samples show.
//!
//! ## Why a static offset is sound
//!
//! The offset is read from the object's *dynamic* type, so replacing it with a
//! constant is only valid if every subclass agrees. It does:
//! `hlp_get_obj_rt` builds a subclass's `fields_indexes` by `memcpy`ing the
//! parent's array verbatim and appending, so entry `k` means the same byte
//! offset in every type descended from the one that introduced it. A field
//! index valid for the static type is therefore valid, with the same offset,
//! for any dynamic type that can appear in that register.
//!
//! ## Exactness
//!
//! This must agree with `ash_std` bit for bit — a disagreement is silent memory
//! corruption, not a wrong answer. Two safeguards:
//!
//! * Anything this module cannot reproduce exactly returns `None`, and the
//!   caller falls back to the runtime call. `HPACKED` fields are the main case:
//!   their layout recurses through another type's *runtime* object.
//! * `verify_against_runtime` cross-checks every computed offset against what
//!   `hlp_get_obj_rt` actually produced, and is wired to `ASH_VERIFY_LAYOUT=1`.
//!
//! The size and alignment rules mirror `std/src/types.rs` (`T_SIZES`,
//! `hlp_pad_struct`) and the field walk mirrors `std/src/obj.rs`
//! (`hlp_get_obj_rt`). Note that `HBOOL` occupies two bytes but aligns to one —
//! that asymmetry is HashLink's, and it is load-bearing here.

use std::collections::HashMap;

use crate::hl::{
    hl_type_kind, hl_type_kind_HBOOL, hl_type_kind_HF32, hl_type_kind_HF64, hl_type_kind_HI32,
    hl_type_kind_HI64, hl_type_kind_HOBJ, hl_type_kind_HPACKED, hl_type_kind_HSTRUCT,
    hl_type_kind_HUI16, hl_type_kind_HUI8, hl_type_kind_HVOID,
};
use crate::types::HLType;

const HOST_WORD_SIZE: i32 = std::mem::size_of::<usize>() as i32;

/// Byte size of a value of `kind`, mirroring `T_SIZES` in `std/src/types.rs`.
///
/// `HBOOL` is 2, not 1. `HVOID` and `HPACKED` are 0 — the latter because a
/// packed field's size comes from the referenced type's runtime object, which
/// is why [`object_layout`] refuses to lay out types containing one.
pub(crate) fn type_size(kind: hl_type_kind) -> i32 {
    type_size_for(kind, HOST_WORD_SIZE)
}

pub(crate) fn type_size_for(kind: hl_type_kind, word_size: i32) -> i32 {
    match kind {
        hl_type_kind_HVOID => 0,
        hl_type_kind_HUI8 => 1,
        hl_type_kind_HUI16 => 2,
        hl_type_kind_HI32 => 4,
        hl_type_kind_HI64 => 8,
        hl_type_kind_HF32 => 4,
        hl_type_kind_HF64 => 8,
        hl_type_kind_HBOOL => 2,
        hl_type_kind_HPACKED => 0,
        _ => word_size,
    }
}

/// Byte stride between elements of a `varray` holding values of `kind`.
///
/// The same sizes as [`type_size`] for every kind that can actually be an
/// array element — `HBOOL` is 2 here too — and a machine word for the pointer
/// kinds. It differs only for `HVOID` and `HPACKED`, which `type_size` calls
/// zero and which cannot be array elements at all; a stride is meaningless
/// there, and answering with a word keeps a nonsense program from computing
/// every index as the same address.
///
/// Shared so the two compiled tiers cannot drift: each used to carry its own
/// copy of this table, which is the shape of bug that only shows up once the
/// tiers disagree about one element kind on one program.
pub fn array_elem_size(kind: hl_type_kind) -> i32 {
    array_elem_size_for(kind, HOST_WORD_SIZE)
}

pub fn array_elem_size_for(kind: hl_type_kind, word_size: i32) -> i32 {
    match kind {
        hl_type_kind_HVOID | hl_type_kind_HPACKED => word_size,
        _ => type_size_for(kind, word_size),
    }
}

/// Byte offset of a `varray`'s first element.
///
/// `varray` is `{ hl_type *t; hl_type *at; int size; int pad; }`, so the data
/// begins after 24 bytes.
pub const VARRAY_DATA_OFFSET: i32 = 24;

/// Byte offset of a `varray`'s element count.
pub const VARRAY_SIZE_OFFSET: i32 = 16;

/// The hash `hlp_hash_gen` computes for a field name, computed here instead.
///
/// Field names reaching this are bytecode string constants, so the hash is a
/// compile-time constant and the accessor helpers (`hlp_dyn_get*` /
/// `hlp_dyn_set*`, which take the hash and never the name) can be called with
/// an immediate. It must reproduce `hlp_hash_gen` exactly — the runtime keys
/// its field lookup tables on this number, so an off-by-anything resolves to
/// the wrong field or to none at all.
///
/// Shared for the same reason [`array_elem_size`] is: both compiled tiers
/// bake the result into code, and a disagreement between them would be a
/// wrong field rather than a compile failure.
///
/// Note this only *computes* the hash; it does not intern the name in the
/// runtime's hash-to-name table the way calling `hlp_hash_gen` would. A field
/// that only ever exists because compiled code created it is therefore
/// reachable by hash but not recoverable by name through reflection.
pub fn field_name_hash(name: &str) -> i32 {
    let mut h: i32 = 0;
    for c in name.encode_utf16() {
        h = h.wrapping_mul(223).wrapping_add(c as i32);
    }
    h.wrapping_rem(0x1FFF_FF7B)
}

/// Padding to insert before a field of `kind` at byte `size`, mirroring
/// `hlp_pad_struct`.
fn pad_struct(size: i32, kind: hl_type_kind, word_size: i32) -> i32 {
    let align: i32 = match kind {
        hl_type_kind_HVOID => return 0,
        hl_type_kind_HUI8 => 1,
        hl_type_kind_HUI16 => 2,
        hl_type_kind_HI32 => 4,
        hl_type_kind_HI64 => 8,
        hl_type_kind_HF32 => 4,
        hl_type_kind_HF64 => 8,
        // `align_of::<bool>()` is 1, even though HBOOL occupies 2 bytes.
        hl_type_kind_HBOOL => 1,
        _ => word_size,
    };
    (-(size as i64) & (align as i64 - 1)) as i32
}

/// The resolved layout of one `HOBJ`/`HSTRUCT`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ObjLayout {
    /// Byte offset of each field, indexed exactly as `hl_runtime_obj`'s
    /// `fields_indexes`: inherited fields first, in super-chain order, then
    /// this type's own.
    pub field_offsets: Vec<i32>,
    /// The declared `hl_type_kind` of each field, in the same order.
    ///
    /// A backend must size the load or store from *this*, not from the width of
    /// the register the value happens to live in. The two usually agree, but
    /// where they do not, using the register's width writes past the field and
    /// silently corrupts its neighbour.
    pub field_kinds: Vec<hl_type_kind>,
    /// Instance size including trailing padding.
    pub size: i32,
    /// Trailing padding included in `size`. A subclass resumes laying out at
    /// `size - pad_size`.
    pub pad_size: u8,
    /// Widest field seen along the chain; sets the trailing alignment.
    pub largest_field: u8,
}

/// Layout of `type_index`, or `None` if it cannot be reproduced exactly.
///
/// `None` means "ask the runtime", never "no fields": callers must fall back to
/// `hlp_get_obj_rt` rather than assuming an empty layout.
pub fn object_layout(types: &[HLType], type_index: usize) -> Option<ObjLayout> {
    object_layout_for(types, type_index, HOST_WORD_SIZE)
}

pub fn object_layout_for(types: &[HLType], type_index: usize, word_size: i32) -> Option<ObjLayout> {
    let mut memo = HashMap::new();
    layout_memo(types, type_index, word_size, &mut memo, 0)
}

/// Byte offset of `field_index` within `type_index`, or `None` to fall back.
pub fn field_offset(types: &[HLType], type_index: usize, field_index: usize) -> Option<i32> {
    field_offset_for(types, type_index, field_index, HOST_WORD_SIZE)
}

pub fn field_offset_for(
    types: &[HLType],
    type_index: usize,
    field_index: usize,
    word_size: i32,
) -> Option<i32> {
    object_layout_for(types, type_index, word_size)?
        .field_offsets
        .get(field_index)
        .copied()
}

/// Byte offset *and* declared kind of `field_index` within `type_index`.
///
/// Prefer this over [`field_offset`] when emitting the access: the kind is what
/// the load or store must be sized from.
pub fn field_offset_and_kind(
    types: &[HLType],
    type_index: usize,
    field_index: usize,
) -> Option<(i32, hl_type_kind)> {
    field_offset_and_kind_for(types, type_index, field_index, HOST_WORD_SIZE)
}

pub fn field_offset_and_kind_for(
    types: &[HLType],
    type_index: usize,
    field_index: usize,
    word_size: i32,
) -> Option<(i32, hl_type_kind)> {
    let l = object_layout_for(types, type_index, word_size)?;
    Some((
        *l.field_offsets.get(field_index)?,
        *l.field_kinds.get(field_index)?,
    ))
}

/// Super chains are shallow in practice; this only exists so a malformed or
/// hostile bytecode file cannot drive this into unbounded recursion.
const MAX_SUPER_DEPTH: usize = 256;

fn layout_memo(
    types: &[HLType],
    type_index: usize,
    word_size: i32,
    memo: &mut HashMap<usize, Option<ObjLayout>>,
    depth: usize,
) -> Option<ObjLayout> {
    if depth > MAX_SUPER_DEPTH {
        return None;
    }
    if let Some(cached) = memo.get(&type_index) {
        return cached.clone();
    }
    let computed = compute(types, type_index, word_size, memo, depth);
    memo.insert(type_index, computed.clone());
    computed
}

fn compute(
    types: &[HLType],
    type_index: usize,
    word_size: i32,
    memo: &mut HashMap<usize, Option<ObjLayout>>,
    depth: usize,
) -> Option<ObjLayout> {
    let t = types.get(type_index)?;
    if t.kind != hl_type_kind_HOBJ && t.kind != hl_type_kind_HSTRUCT {
        return None;
    }
    let obj = t.obj.as_ref()?;

    // A super whose layout is unknown makes this one unknown too: the child's
    // fields start where the parent's end.
    let parent = match obj.super_.as_ref() {
        Some(s) => Some(layout_memo(types, s.0, word_size, memo, depth + 1)?),
        None => None,
    };

    // An HOBJ begins with its `hl_type*`; an HSTRUCT does not. A subclass
    // resumes at the parent's size with the parent's trailing padding removed.
    let mut size = match &parent {
        Some(p) => p.size - p.pad_size as i32,
        None => {
            if t.kind == hl_type_kind_HSTRUCT {
                0
            } else {
                word_size
            }
        }
    };
    let mut largest_field = match &parent {
        Some(p) => p.largest_field,
        None => size as u8,
    };
    let mut field_offsets = match &parent {
        Some(p) => p.field_offsets.clone(),
        None => Vec::new(),
    };
    let mut field_kinds = match &parent {
        Some(p) => p.field_kinds.clone(),
        None => Vec::new(),
    };

    for f in &obj.fields {
        let ft = types.get(f.type_.0)?;
        // A packed field's size and alignment come from another type's runtime
        // object. Reproducing that here would mean reimplementing the recursive
        // case as well; refuse instead, and let the runtime answer.
        if ft.kind == hl_type_kind_HPACKED {
            return None;
        }
        size += pad_struct(size, ft.kind, word_size);
        field_offsets.push(size);
        field_kinds.push(ft.kind);
        let sz = type_size_for(ft.kind, word_size);
        size += sz;
        if sz > largest_field as i32 {
            largest_field = sz as u8;
        }
    }

    let mut pad_size = 0u8;
    if largest_field > 0 {
        let pad = size % largest_field as i32;
        if pad != 0 {
            pad_size = largest_field - pad as u8;
            size += pad_size as i32;
        }
    }

    Some(ObjLayout {
        field_offsets,
        field_kinds,
        size,
        pad_size,
        largest_field,
    })
}

/// One type whose computed layout disagreed with the runtime's.
#[derive(Debug)]
pub struct LayoutMismatch {
    pub type_index: usize,
    pub name: String,
    pub detail: String,
}

/// Cross-check every computed layout against what `hlp_get_obj_rt` produced.
///
/// This is the safety net for the whole module: a wrong offset is silent memory
/// corruption, so the claim that the static walk reproduces the runtime one is
/// checked rather than asserted. `resolve_rt` is given a type index and returns
/// that type's `hl_runtime_obj*`, or `None` if the runtime has not built one —
/// unbuilt types are skipped, not reported, since there is nothing to compare.
///
/// # Safety
///
/// Every non-null pointer `resolve_rt` returns must be a live `hl_runtime_obj`
/// with `nfields` valid entries in `fields_indexes`.
pub unsafe fn verify_against_runtime(
    types: &[HLType],
    mut resolve_rt: impl FnMut(usize) -> Option<*mut crate::hl::hl_runtime_obj>,
) -> Vec<LayoutMismatch> {
    let mut out = Vec::new();
    for (ti, t) in types.iter().enumerate() {
        if t.kind != hl_type_kind_HOBJ && t.kind != hl_type_kind_HSTRUCT {
            continue;
        }
        let Some(computed) = object_layout(types, ti) else {
            continue;
        };
        let Some(rt) = resolve_rt(ti) else { continue };
        if rt.is_null() {
            continue;
        }
        let name = t
            .obj
            .as_ref()
            .map(|o| o.name.clone())
            .unwrap_or_else(|| format!("type[{ti}]"));

        let nfields = (*rt).nfields as usize;
        if nfields != computed.field_offsets.len() {
            out.push(LayoutMismatch {
                type_index: ti,
                name,
                detail: format!(
                    "field count: runtime {} vs computed {}",
                    nfields,
                    computed.field_offsets.len()
                ),
            });
            continue;
        }
        if (*rt).size != computed.size {
            out.push(LayoutMismatch {
                type_index: ti,
                name: name.clone(),
                detail: format!("size: runtime {} vs computed {}", (*rt).size, computed.size),
            });
            continue;
        }
        let indexes = (*rt).fields_indexes;
        if indexes.is_null() {
            continue;
        }
        for (i, &want) in computed.field_offsets.iter().enumerate() {
            let got = *indexes.add(i);
            if got != want {
                out.push(LayoutMismatch {
                    type_index: ti,
                    name: name.clone(),
                    detail: format!("field {i}: runtime offset {got} vs computed {want}"),
                });
            }
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{HLObjField, HLTypeObj, TypeRef};

    fn scalar(kind: hl_type_kind) -> HLType {
        HLType {
            kind,
            ..Default::default()
        }
    }

    fn object(name: &str, super_: Option<usize>, fields: &[(&str, usize)]) -> HLType {
        HLType {
            kind: hl_type_kind_HOBJ,
            obj: Some(HLTypeObj {
                name: name.to_string(),
                super_: super_.map(TypeRef),
                fields: fields
                    .iter()
                    .map(|(n, t)| HLObjField {
                        name: n.to_string(),
                        type_: TypeRef(*t),
                        hashed_name: 0,
                    })
                    .collect(),
                ..Default::default()
            }),
            ..Default::default()
        }
    }

    /// Index layout: 0=F64, 1=I32, 2=BOOL, 3=OBJ-ish pointer (HDYN), 4=UI8.
    fn base_types() -> Vec<HLType> {
        vec![
            scalar(hl_type_kind_HF64),
            scalar(hl_type_kind_HI32),
            scalar(hl_type_kind_HBOOL),
            scalar(crate::hl::hl_type_kind_HDYN),
            scalar(hl_type_kind_HUI8),
        ]
    }

    #[test]
    fn an_object_starts_after_its_type_pointer() {
        let mut types = base_types();
        types.push(object("A", None, &[("x", 0)])); // one F64
        let l = object_layout(&types, 5).unwrap();
        // Offset 0 holds the hl_type*, so the first field lands at 8.
        assert_eq!(l.field_offsets, vec![8]);
        assert_eq!(l.size, 16);
    }

    #[test]
    fn a_struct_has_no_type_header() {
        let mut types = base_types();
        let mut s = object("S", None, &[("x", 0)]);
        s.kind = hl_type_kind_HSTRUCT;
        types.push(s);
        let l = object_layout(&types, 5).unwrap();
        assert_eq!(l.field_offsets, vec![0]);
    }

    #[test]
    fn fields_are_aligned_to_their_own_width() {
        let mut types = base_types();
        // header(8) | i32 @8 | pad 4 | f64 @16
        types.push(object("A", None, &[("i", 1), ("f", 0)]));
        let l = object_layout(&types, 5).unwrap();
        assert_eq!(l.field_offsets, vec![8, 16]);
        assert_eq!(l.size, 24);
    }

    #[test]
    fn bool_is_two_bytes_but_aligns_to_one() {
        let mut types = base_types();
        // header(8) | u8 @8 | bool @9 (align 1, size 2) | i32 @12 (align 4)
        types.push(object("A", None, &[("b", 4), ("t", 2), ("i", 1)]));
        let l = object_layout(&types, 5).unwrap();
        assert_eq!(l.field_offsets, vec![8, 9, 12]);
    }

    /// The invariant the whole optimization rests on: a subclass's offsets for
    /// inherited fields are identical to the parent's.
    #[test]
    fn subclass_preserves_parent_field_offsets() {
        let mut types = base_types();
        types.push(object("Base", None, &[("x", 0), ("y", 0)])); // 5
        types.push(object("Derived", Some(5), &[("z", 0)])); // 6

        let base = object_layout(&types, 5).unwrap();
        let derived = object_layout(&types, 6).unwrap();
        assert_eq!(base.field_offsets, vec![8, 16]);
        assert_eq!(derived.field_offsets[..2], base.field_offsets[..]);
        assert_eq!(derived.field_offsets, vec![8, 16, 24]);
    }

    /// A subclass resumes at the parent's size minus the parent's tail padding,
    /// so trailing padding is reclaimed rather than inherited as a hole.
    #[test]
    fn subclass_reuses_parent_tail_padding() {
        let mut types = base_types();
        // Base: header(8) | u8 @8, size 9 -> padded to 16 (largest_field 8)
        types.push(object("Base", None, &[("b", 4)])); // 5
        types.push(object("Derived", Some(5), &[("c", 4)])); // 6
        let base = object_layout(&types, 5).unwrap();
        assert_eq!(base.size, 16);
        assert_eq!(base.pad_size, 7);
        let derived = object_layout(&types, 6).unwrap();
        // Resumes at 16 - 7 = 9, not at 16.
        assert_eq!(derived.field_offsets, vec![8, 9]);
    }

    #[test]
    fn packed_fields_refuse_rather_than_guess() {
        let mut types = base_types();
        types.push(scalar(hl_type_kind_HPACKED)); // 5
        types.push(object("A", None, &[("p", 5)])); // 6
        assert_eq!(object_layout(&types, 6), None);
        // And a subclass of an unknowable type is itself unknowable.
        types.push(object("B", Some(6), &[("x", 0)])); // 7
        assert_eq!(object_layout(&types, 7), None);
    }

    #[test]
    fn non_object_kinds_have_no_layout() {
        let types = base_types();
        assert_eq!(object_layout(&types, 0), None);
        assert_eq!(field_offset(&types, 0, 0), None);
    }

    #[test]
    fn out_of_range_field_index_is_none_not_a_panic() {
        let mut types = base_types();
        types.push(object("A", None, &[("x", 0)]));
        assert_eq!(field_offset(&types, 5, 0), Some(8));
        assert_eq!(field_offset(&types, 5, 1), None);
    }

    /// The stride table is shared by both compiled tiers, so its values are
    /// load-bearing in two places at once. `HBOOL` is the one that surprises:
    /// two bytes, not one.
    #[test]
    fn array_strides_match_the_runtime() {
        assert_eq!(array_elem_size(hl_type_kind_HUI8), 1);
        assert_eq!(array_elem_size(hl_type_kind_HUI16), 2);
        assert_eq!(array_elem_size(hl_type_kind_HBOOL), 2);
        assert_eq!(array_elem_size(hl_type_kind_HI32), 4);
        assert_eq!(array_elem_size(hl_type_kind_HF32), 4);
        assert_eq!(array_elem_size(hl_type_kind_HI64), 8);
        assert_eq!(array_elem_size(hl_type_kind_HF64), 8);
    }

    /// A pointer element is a machine word, and so are the two kinds that
    /// cannot be array elements at all -- a zero stride would make every index
    /// name the same address.
    #[test]
    fn array_strides_are_never_zero() {
        for kind in [hl_type_kind_HVOID, hl_type_kind_HPACKED, hl_type_kind_HOBJ] {
            assert_eq!(array_elem_size(kind), 8, "kind {kind}");
        }
    }

    /// The header offsets the tiers index against.
    #[test]
    fn varray_header_offsets() {
        assert_eq!(VARRAY_SIZE_OFFSET, 16);
        assert_eq!(VARRAY_DATA_OFFSET, 24);
    }

    #[test]
    fn wasm32_object_layout_uses_four_byte_words() {
        let mut types = base_types();
        // header(4) | pointer @4 | i32 @8 | pad 4 | f64 @16
        types.push(object("Wasm", None, &[("p", 3), ("i", 1), ("f", 0)]));
        let l = object_layout_for(&types, 5, 4).unwrap();
        assert_eq!(l.field_offsets, vec![4, 8, 16]);
        assert_eq!(l.size, 24);
        assert_eq!(array_elem_size_for(hl_type_kind_HOBJ, 4), 4);
    }
}
