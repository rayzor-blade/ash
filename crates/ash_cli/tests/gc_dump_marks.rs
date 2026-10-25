//! A memory dump must not change what a later collection keeps alive.
mod common;

use common::{ash_cli_bin, render_output, run_with_timeout, tests_dir};
use std::process::Command;
use std::time::Duration;

/// Objects reachable from a marked object were lost when the dump's marks
/// outlived it and the collector ran without generations.
fn run(generational: bool) {
    let dir = tempfile::tempdir().unwrap();
    let mut command = Command::new(ash_cli_bin());
    command
        .args(["--mode", "interp"])
        .arg(tests_dir().join("dump_marks.hl"))
        .env("ASH_TEST_DUMP", dir.path().join("dump"));
    if !generational {
        command.env("ASH_GC_GEN", "0");
    }
    let result = run_with_timeout(command, Duration::from_secs(120));
    assert!(
        !result.timed_out && result.output.status.success(),
        "{}",
        render_output(&result.output)
    );
    let stdout = String::from_utf8_lossy(&result.output.stdout);
    assert!(stdout.contains("bad=0"), "{stdout}");
}

#[test]
fn a_dump_does_not_cost_the_program_its_objects() {
    run(true);
}

#[test]
fn a_dump_does_not_cost_the_program_its_objects_without_generations() {
    run(false);
}
