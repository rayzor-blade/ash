//! The lazy LLVM tier must inline SIMD value helpers before lowering them.
//! Otherwise a local vector add allocates a 16-byte result every iteration.
mod common;

use common::{ash_cli_bin, haxe_available, repo_root, tests_dir};
use std::path::PathBuf;
use std::process::Command;

fn fixture() -> PathBuf {
    if !haxe_available() {
        let path = tests_dir().join("bench_simd_vs_scalar.hl");
        assert!(
            path.exists(),
            "missing compiled fixture: {}",
            path.display()
        );
        return path;
    }
    let dir = std::env::temp_dir().join(format!("ash-simd-alloc-tests-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("BenchSimdVsScalar.hl");
    let out = Command::new("haxe")
        .arg("-cp")
        .arg(tests_dir())
        .arg("-cp")
        .arg(repo_root().join("haxelib/ash-simd"))
        .args(["-main", "BenchSimdVsScalar", "-hl"])
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

#[test]
fn llvm_jit_keeps_local_simd_values_in_registers() {
    let out = Command::new(ash_cli_bin())
        .args(["--mode", "jit", "--jit-tier", "llvm"])
        .arg(fixture())
        .args(["value-local", "100000"])
        .env("ASH_AIR", "v2")
        .env("ASH_AIR_LEVEL", "O3")
        .env_remove("ASH_AIR_NO_INLINE")
        .env("ASH_GC_TLAB", "0")
        .output()
        .unwrap();
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(out.status.success(), "{}\n{stderr}", out.status);
    let bytes: usize = stdout
        .split_whitespace()
        .find_map(|field| field.strip_prefix("gc_bytes="))
        .expect("benchmark printed gc_bytes")
        .parse()
        .unwrap();
    assert!(
        bytes < 4096,
        "local SIMD additions allocated {bytes} bytes:\n{stdout}\n{stderr}"
    );
}
