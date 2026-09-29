//! The lazy LLVM tier must inline SIMD value helpers before lowering them.
//! Otherwise a local vector add allocates a 16-byte result every iteration.
mod common;

use common::{ash_cli_bin, haxe_available, repo_root, tests_dir};
use std::path::PathBuf;
use std::process::Command;

fn fixture(main: &str, compiled: &str) -> PathBuf {
    if !haxe_available() {
        let path = tests_dir().join(compiled);
        assert!(
            path.exists(),
            "missing compiled fixture: {}",
            path.display()
        );
        return path;
    }
    let dir = std::env::temp_dir().join(format!("ash-simd-alloc-tests-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join(compiled);
    let out = Command::new("haxe")
        .arg("-cp")
        .arg(tests_dir())
        .arg("-cp")
        .arg(repo_root().join("haxelib/ash-simd"))
        .args(["-main", main, "-hl"])
        .arg(&path)
        .output()
        .unwrap();
    assert!(
        out.status.success(),
        "haxe: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    path
}

fn run_bytes(fixture: PathBuf, args: &[&str]) -> usize {
    let out = Command::new(ash_cli_bin())
        .args(["--mode", "jit", "--jit-tier", "llvm"])
        .arg(fixture)
        .args(args)
        .env("ASH_AIR", "v2")
        .env("ASH_AIR_LEVEL", "O3")
        .env_remove("ASH_AIR_NO_INLINE")
        .env("ASH_GC_TLAB", "0")
        .output()
        .unwrap();
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(out.status.success(), "{}\n{stdout}\n{stderr}", out.status);
    stdout
        .split_whitespace()
        .find_map(|field| field.strip_prefix("gc_bytes="))
        .expect("benchmark printed gc_bytes")
        .parse()
        .unwrap()
}

#[test]
fn llvm_jit_keeps_local_simd_values_in_registers() {
    let bytes = run_bytes(
        fixture("BenchSimdVsScalar", "bench_simd_vs_scalar.hl"),
        &["value-local", "100000"],
    );
    assert!(bytes < 4096, "local SIMD additions allocated {bytes} bytes");
}

#[test]
fn llvm_jit_elides_multiple_vector_temporaries() {
    let bytes = run_bytes(
        fixture("SimdMultiBufferAlloc", "simd_multi_buffer_alloc.hl"),
        &["wrapped", "100"],
    );
    assert!(
        bytes < 4096,
        "multi-buffer SIMD updates allocated {bytes} bytes"
    );
}

#[test]
fn llvm_jit_elides_haxe_inlined_vector_temporaries() {
    let bytes = run_bytes(
        fixture("SimdMultiBufferAlloc", "simd_multi_buffer_alloc.hl"),
        &["integration", "100"],
    );
    assert!(
        bytes < 4096,
        "Haxe-inlined multi-buffer SIMD updates allocated {bytes} bytes"
    );
}
