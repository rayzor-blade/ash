//! LLVM codegen reached from a Haxe fiber must have enough native stack.
#![cfg(feature = "llvm")]
mod common;

use common::{ash_cli_bin, haxe_available, render_output, run_with_timeout, tests_dir};
use std::process::Command;
use std::time::Duration;

fn output_for(tier: &str, workers: usize, fixture: &std::path::Path) -> Vec<String> {
    let mut command = Command::new(ash_cli_bin());
    command
        .args(["--mode", "jit", "--jit-tier", tier])
        .arg(fixture)
        .env("ASH_WORKERS", workers.to_string())
        .env_remove("ASH_NO_PROMOTE")
        .env_remove("ASH_STD_LINKAGE");
    let result = run_with_timeout(command, Duration::from_secs(60));
    assert!(
        !result.timed_out,
        "{tier} workers={workers} timed out: {}",
        render_output(&result.output)
    );
    assert!(
        result.output.status.success(),
        "{tier} workers={workers} failed: {}",
        render_output(&result.output)
    );
    String::from_utf8_lossy(&result.output.stdout)
        .lines()
        .filter(|line| line.starts_with("pool "))
        .map(str::to_string)
        .collect()
}

#[test]
fn llvm_worker_pool_completes_and_matches_cranelift() {
    let dir = tempfile::tempdir().unwrap();
    let fixture = dir.path().join("pool.hl");
    if haxe_available() {
        let output = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", "TestLlvmWorkerPool", "-hl"])
            .arg(&fixture)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "haxe: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    } else {
        std::fs::copy(tests_dir().join("test_llvm_worker_pool.hl"), &fixture)
            .expect("committed pool bytecode fixture");
    }
    for workers in [0, 4] {
        let expected = output_for("cranelift", workers, &fixture);
        assert_eq!(
            expected.len(),
            3,
            "pool fixture did not finish all worker counts"
        );
        assert_eq!(output_for("llvm", workers, &fixture), expected);
    }
}
