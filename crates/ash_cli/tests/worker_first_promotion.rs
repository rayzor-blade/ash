//! A function first reached on a pool worker is still promoted to the top tier.
#![cfg(feature = "llvm")]
mod common;

use common::{ash_cli_bin, haxe_available, render_output, run_with_timeout, tests_dir};
use std::collections::HashSet;
use std::process::Command;
use std::time::Duration;

#[test]
fn a_function_first_reached_on_a_worker_reaches_llvm() {
    let dir = tempfile::tempdir().unwrap();
    let fixture = dir.path().join("worker_promote.hl");
    if haxe_available() {
        let output = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", "TestWorkerPromote", "-hl"])
            .arg(&fixture)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "haxe: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    } else {
        std::fs::copy(tests_dir().join("test_worker_promote.hl"), &fixture)
            .expect("committed worker promotion bytecode fixture");
    }

    let mut command = Command::new(ash_cli_bin());
    command
        .args(["--mode", "jit"])
        .arg(&fixture)
        .env("ASH_TIER_LOG", "1")
        .env_remove("ASH_NO_PROMOTE")
        .env_remove("ASH_STD_LINKAGE");
    let result = run_with_timeout(command, Duration::from_secs(120));
    assert!(
        !result.timed_out && result.output.status.success(),
        "{}",
        render_output(&result.output)
    );
    let stdout = String::from_utf8_lossy(&result.output.stdout);
    assert!(stdout.contains("worker promote ok"), "{stdout}");

    // `[tier] install findex=N ... tier=<tier> ... on <thread> t=..ms`
    let stderr = String::from_utf8_lossy(&result.output.stderr);
    let mut on_worker = HashSet::new();
    let mut at_llvm = HashSet::new();
    for line in stderr.lines().filter(|l| l.starts_with("[tier] install ")) {
        let field = |key: &str| {
            line.split_whitespace()
                .find_map(|word| word.strip_prefix(key))
        };
        let Some(findex) = field("findex=") else {
            continue;
        };
        match field("tier=") {
            Some("cranelift") if line.contains(" on ash-vm-") => {
                on_worker.insert(findex.to_string());
            }
            Some("llvm") => {
                at_llvm.insert(findex.to_string());
            }
            _ => {}
        }
    }
    assert!(
        on_worker.intersection(&at_llvm).next().is_some(),
        "no function installed on a worker reached LLVM\nworker baselines: {on_worker:?}\nllvm: {at_llvm:?}\n{stderr}"
    );
}
