//! A native that takes more words than the integer registers hold is called
//! with each argument where the platform's calling convention puts it.
//!
//! `test_wide_native.hl` calls natives of 9, 12 and 14 integer arguments, one
//! that mixes booleans in among them, one that interleaves integers, floats and
//! doubles, and one that returns a float from a call with an argument on the
//! stack. Each native weights its arguments by position, so a value in the
//! wrong slot changes the answer. The interpreter, the hybrid VM and the JIT
//! must all print the same lines.
#![cfg(all(feature = "llvm", unix))]
mod common;
use common::{ash_cli_bin, render_output, run_with_timeout, tests_dir};
use std::process::Command;
use std::time::Duration;

const EXPECTED: &str = "w9=285\nw12=650\nw14=1015\nflags=13936\nmixed=1015\nsingle=112.500000000\n";

/// The library the program names, built as an HDLL next to a copy of the
/// program, and the program run in `mode`.
fn run(mode: &str) -> std::process::Output {
    let dir = tempfile::tempdir().unwrap();
    let source = tests_dir().join("wide_native/wide.c");
    let hdll = dir.path().join("wide.hdll");
    let shared = if cfg!(target_os = "macos") {
        "-dynamiclib"
    } else {
        "-shared"
    };
    let cc = std::env::var("CC").unwrap_or_else(|_| "cc".into());
    let built = Command::new(cc)
        .args([shared, "-fPIC", "-o"])
        .arg(&hdll)
        .arg(&source)
        .status()
        .expect("no C compiler");
    assert!(built.success(), "compiling {}", source.display());
    let program = dir.path().join("test_wide_native.hl");
    std::fs::copy(tests_dir().join("test_wide_native.hl"), &program).unwrap();

    let mut cmd = Command::new(ash_cli_bin());
    cmd.args(["--mode", mode]).arg(&program);
    let result = run_with_timeout(cmd, Duration::from_secs(300));
    assert!(!result.timed_out, "--mode {mode} timed out");
    result.output
}

fn assert_prints_expected(mode: &str) {
    let out = run(mode);
    assert!(
        out.status.success(),
        "--mode {mode}: {}",
        render_output(&out)
    );
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        EXPECTED,
        "--mode {mode}: {}",
        render_output(&out)
    );
}

#[test]
fn the_interpreter_passes_wide_native_arguments_where_the_callee_reads_them() {
    assert_prints_expected("interp");
}

#[test]
fn the_hybrid_vm_passes_wide_native_arguments_where_the_callee_reads_them() {
    assert_prints_expected("hybrid");
}

#[test]
fn the_jit_passes_wide_native_arguments_where_the_callee_reads_them() {
    assert_prints_expected("jit");
}
