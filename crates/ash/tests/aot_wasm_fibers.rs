//! `AotRequest::wasm_fibers` turns the fiber transform on without the
//! environment variable.
//!
//! Needs the wasm runtime object; skipped, not failed, when it is absent.

#![cfg(feature = "llvm")]

use std::path::{Path, PathBuf};

use ash_core::llvm::aot_build::{AotRequest, emit_aot};

fn runtime() -> Option<PathBuf> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../target");
    ["release", "debug"]
        .iter()
        .map(|p| root.join(p).join("wasm32-wasip1/ash_runtime.o"))
        .find(|p| p.is_file())
}

fn build(runtime: &Path, dir: &Path, name: &str, wasm_fibers: bool) -> Vec<u8> {
    let program = Path::new(env!("CARGO_MANIFEST_DIR")).join("test/tests/test_array_push.hl");
    let exe = dir.join(format!("{name}.wasm"));
    emit_aot(AotRequest {
        file: &program,
        out: &dir.join(format!("{name}.wasm.o")),
        exe: Some(&exe),
        runtime: Some(runtime),
        target: Some("wasm32-wasip1".to_string()),
        pgo: None,
        allow_refused: true,
        abi_version: 1,
        quiet: true,
        links: Default::default(),
        objects: Vec::new(),
        exports: Vec::new(),
        closures: Vec::new(),
        object_tails: Vec::new(),
        wasm_fibers,
    })
    .unwrap_or_else(|e| panic!("build {name}: {e:#}"));
    std::fs::read(&exe).unwrap()
}

#[test]
fn the_request_field_instruments_the_module() {
    let Some(runtime) = runtime() else {
        eprintln!("skipped: no wasm32-wasip1 ash_runtime.o under target/");
        return;
    };
    if std::env::var_os("ASH_WASM_FIBERS").is_some() {
        eprintln!("skipped: ASH_WASM_FIBERS is set, so both builds would have fibers");
        return;
    }
    let dir = std::env::temp_dir().join(format!("ash-aot-fibers-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let plain = build(&runtime, &dir, "plain", false);
    let fibers = build(&runtime, &dir, "fibers", true);
    let _ = std::fs::remove_dir_all(&dir);
    assert!(
        fibers.len() > plain.len(),
        "wasm_fibers did not instrument the module ({} vs {} bytes)",
        fibers.len(),
        plain.len()
    );
}
