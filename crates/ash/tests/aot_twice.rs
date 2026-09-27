//! Two AOT builds in one process must not share compiled bodies.
//!
//! A host such as a build server calls `emit_aot` once per rebuild without
//! restarting. Anything cached per function across calls would hand the
//! second program the first one's code, linked against its own data. This
//! builds B, then A, then B again, and requires the two objects for B to be
//! the same bytes.
//!
//! Its own test binary, so no other test has compiled anything first.

#![cfg(feature = "llvm")]

use std::path::{Path, PathBuf};

use ash_core::llvm::aot_build::{AotRequest, emit_aot};

fn fixture(name: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("test/tests")
        .join(name)
}

fn build(program: &Path, out: &Path) -> Vec<u8> {
    emit_aot(AotRequest {
        file: program,
        out,
        exe: None,
        runtime: None,
        target: None,
        pgo: None,
        allow_refused: true,
        abi_version: 1,
        quiet: true,
        links: Default::default(),
        objects: Vec::new(),
        exports: Vec::new(),
        wasm_fibers: false,
    })
    .unwrap_or_else(|e| panic!("emit {}: {e:#}", program.display()));
    std::fs::read(out).unwrap_or_else(|e| panic!("read {}: {e}", out.display()))
}

#[test]
fn a_second_build_in_the_process_compiles_its_own_program() {
    let dir = std::env::temp_dir().join(format!("ash-aot-twice-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let a = fixture("test_array_push.hl");
    let b = fixture("test_arrayobj_sort.hl");

    let first = build(&b, &dir.join("b1.o"));
    build(&a, &dir.join("a.o"));
    let second = build(&b, &dir.join("b2.o"));

    let _ = std::fs::remove_dir_all(&dir);
    assert!(
        first == second,
        "B built after A differs from B built first ({} vs {} bytes)",
        first.len(),
        second.len()
    );
}
