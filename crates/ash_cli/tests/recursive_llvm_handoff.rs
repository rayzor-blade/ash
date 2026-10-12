//! Recursive calls should still reach LLVM after Cranelift takes the calls
//! away from the interpreter's tier-1 counter.
#![cfg(feature = "llvm")]
mod common;

use common::{ash_cli_bin, render_output, run_with_timeout, tests_dir};
use std::process::Command;
use std::time::Duration;

#[test]
fn recursive_cranelift_entry_requests_llvm_with_real_calls() {
    let mut command = Command::new(ash_cli_bin());
    command
        .args(["--mode", "hybrid", "--jit-tier", "auto", "--jit-log", "--quiet"])
        .arg(tests_dir().join("bench_fib.hl"))
        // Put the ordinary interpreter counter out of reach. The only way
        // to reach LLVM is the live recursive frame crossing into Cranelift.
        .env("ASH_TIER1", "1000000000")
        .env("ASH_NO_PURE_CSE", "1")
        .env("ASH_TIER_LOG", "1")
        .env_remove("ASH_NO_PROMOTE");
    let result = run_with_timeout(command, Duration::from_secs(30));
    assert!(
        !result.timed_out && result.output.status.success(),
        "{}",
        render_output(&result.output)
    );
    let stdout = String::from_utf8_lossy(&result.output.stdout);
    let stderr = String::from_utf8_lossy(&result.output.stderr);
    assert!(stdout.contains("BenchFib 102334155"), "{stdout}");
    assert!(stderr.contains("recursive-entry proposes"), "{stderr}");
    assert!(stderr.contains("tier=llvm"), "{stderr}");
    assert!(stderr.contains("llvm=1"), "{stderr}");
}
