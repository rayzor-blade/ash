//! A function whose time is one loop of heavy iterations is compiled while it runs.
#![cfg(feature = "llvm")]
mod common;

use common::{ash_cli_bin, render_output, run_with_timeout, tests_dir};
use std::process::Command;
use std::time::Duration;

/// pidigits' `main` runs a few thousand iterations of a loop whose body calls
/// compiled arithmetic, in a single invocation. Counting only edges that close
/// a loop leaves it below the promotion threshold and it stays interpreted for
/// the whole run.
#[test]
fn a_loop_owner_with_few_heavy_iterations_is_compiled() {
    let mut command = Command::new(ash_cli_bin());
    command
        .args(["--mode", "hybrid"])
        .arg(tests_dir().join("bench_pidigits.hl"))
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
    assert!(stdout.contains("checksum 882142"), "{stdout}");

    let stderr = String::from_utf8_lossy(&result.output.stderr);
    assert!(
        stderr
            .lines()
            .any(|l| l.starts_with("[tier] install ") && l.contains("BMPidigitsCode.main ")),
        "main was never compiled\n{stderr}"
    );
}
