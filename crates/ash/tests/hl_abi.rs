//! The ABI ash promises to HDLLs built against upstream HashLink.
//!
//! bindgen already asserts that ash's Rust types match `std/hl.h` — but it
//! asserts them against whatever that header currently says, so editing or
//! bumping the header moves the goalposts and the check still passes. This
//! table is the other half: it pins what upstream's ABI actually is, so a
//! change to the vendored header has to be made deliberately rather than
//! noticed after an HDLL reads the wrong offset.
//!
//! Measured from upstream `src/hl.h` on arm64. Every value here is identical
//! in HashLink 1.14 (what ash vendors), 1.16 and master/2.0 — the layouts have
//! not moved across those releases.
//!
//! # If this fails
//!
//! Re-measure against the upstream tag you intend to support and update the
//! table in the same commit, saying which release it came from. Do not adjust
//! a number to make the test pass.

use std::mem::{offset_of, size_of};

use ash_core::hl_bindings as hl;

#[test]
fn struct_sizes_match_upstream() {
    assert_eq!(size_of::<hl::vdynamic>(), 16, "vdynamic");
    assert_eq!(size_of::<hl::hl_type>(), 32, "hl_type");
    assert_eq!(size_of::<hl::varray>(), 24, "varray");
    assert_eq!(size_of::<hl::vstring>(), 24, "vstring");
    assert_eq!(size_of::<hl::vclosure>(), 32, "vclosure");
    assert_eq!(size_of::<hl::vvirtual>(), 24, "vvirtual");
    assert_eq!(size_of::<hl::hl_type_obj>(), 80, "hl_type_obj");
    assert_eq!(size_of::<hl::hl_field_lookup>(), 16, "hl_field_lookup");
    assert_eq!(size_of::<hl::hl_type_fun>(), 80, "hl_type_fun");
    assert_eq!(size_of::<hl::hl_type_enum>(), 32, "hl_type_enum");
}

#[test]
fn field_offsets_match_upstream() {
    // `v` at 8 is the one every boxed-value path depends on: a null vdynamic
    // dereferenced for its payload faults at exactly 0x8, which is how a lost
    // box is recognised in a crash report.
    assert_eq!(offset_of!(hl::vdynamic, t), 0);
    assert_eq!(offset_of!(hl::vdynamic, v), 8);

    assert_eq!(offset_of!(hl::hl_type, kind), 0);
    assert_eq!(offset_of!(hl::hl_type, mark_bits), 24);

    assert_eq!(offset_of!(hl::varray, at), 8);
    assert_eq!(offset_of!(hl::varray, size), 16);

    assert_eq!(offset_of!(hl::vclosure, t), 0);
    assert_eq!(offset_of!(hl::vclosure, fun), 8);
    assert_eq!(offset_of!(hl::vclosure, value), 24);

    assert_eq!(offset_of!(hl::vvirtual, t), 0);
    assert_eq!(offset_of!(hl::vvirtual, value), 8);
    assert_eq!(offset_of!(hl::vvirtual, next), 16);

    assert_eq!(offset_of!(hl::hl_type_obj, nfields), 0);
    assert_eq!(offset_of!(hl::hl_type_obj, super_), 24);
    assert_eq!(offset_of!(hl::hl_type_obj, fields), 32);
    assert_eq!(offset_of!(hl::hl_type_obj, rt), 72);

    assert_eq!(offset_of!(hl::hl_field_lookup, t), 0);
    assert_eq!(offset_of!(hl::hl_field_lookup, hashed_name), 8);
    assert_eq!(offset_of!(hl::hl_field_lookup, field_index), 12);
}

/// Type kinds are a wire format: bytecode stores them as integers.
///
/// They may only ever be appended to. 1.16 added `HGUID = 23`, moving `HLAST`
/// from 23 to 24; ash still targets 23 and the decoder rejects anything at or
/// past its own `HLAST`, so a program using a kind ash does not know fails to
/// load instead of being read as some other kind.
#[test]
fn type_kinds_have_not_been_renumbered() {
    assert_eq!(hl::hl_type_kind_HVOID, 0);
    assert_eq!(hl::hl_type_kind_HI32, 3);
    assert_eq!(hl::hl_type_kind_HBYTES, 8);
    assert_eq!(hl::hl_type_kind_HDYN, 9);
    assert_eq!(hl::hl_type_kind_HFUN, 10);
    assert_eq!(hl::hl_type_kind_HOBJ, 11);
    assert_eq!(hl::hl_type_kind_HVIRTUAL, 15);
    assert_eq!(hl::hl_type_kind_HDYNOBJ, 16);
    assert_eq!(hl::hl_type_kind_HENUM, 18);
    assert_eq!(hl::hl_type_kind_HNULL, 19);
    assert_eq!(hl::hl_type_kind_HSTRUCT, 21);
    assert_eq!(hl::hl_type_kind_HLAST, 23);
}
