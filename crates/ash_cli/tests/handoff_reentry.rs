//! A function whose running frame handed off to the top tier has its promote
//! postponed. Entered again later from the interpreter, or called from a
//! compiled caller, it is promoted.
mod common;

use common::{ash_cli_bin, haxe_available, render_output, run_with_timeout, tests_dir};
use std::process::Command;
use std::time::Duration;

/// Runs `main` from the Haxe source, or from the committed bytecode `fixture`
/// when haxe is absent, with `--jit-log`. Returns stdout and the log.
fn run_logged(main: &str, fixture: &str, args: &[&str]) -> (String, String) {
    let dir = tempfile::tempdir().unwrap();
    let hl = dir.path().join("program.hl");
    if haxe_available() {
        let output = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", main, "-hl"])
            .arg(&hl)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "haxe: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    } else {
        std::fs::copy(tests_dir().join(fixture), &hl).expect("committed bytecode fixture");
    }
    let mut command = Command::new(ash_cli_bin());
    command.arg("--jit-log").arg(&hl).args(args);
    let result = run_with_timeout(command, Duration::from_secs(120));
    assert!(
        !result.timed_out && result.output.status.success(),
        "{}",
        render_output(&result.output)
    );
    (
        String::from_utf8_lossy(&result.output.stdout).into_owned(),
        String::from_utf8_lossy(&result.output.stderr).into_owned(),
    )
}

fn installed_at_llvm(log: &str, function: &str) -> bool {
    log.lines().any(|l| {
        l.starts_with("[tier] install ") && l.contains(function) && l.contains("tier=llvm")
    })
}

#[test]
fn a_function_entered_again_after_its_hand_off_reaches_the_top_tier() {
    let (stdout, log) = run_logged("TestHandoffReentry", "test_handoff_reentry.hl", &[]);
    assert!(stdout.contains("done true"), "{stdout}");
    assert!(
        installed_at_llvm(&log, "TestHandoffReentry.kernel"),
        "the kernel never reached the top tier:\n{log}"
    );
}

#[test]
fn a_function_called_from_a_compiled_caller_reaches_the_top_tier() {
    let (stdout, log) = run_logged("TestCompiledCaller", "test_compiled_caller.hl", &[]);
    assert!(stdout.contains("done true"), "{stdout}");
    assert!(
        installed_at_llvm(&log, "CallerWorld.step"),
        "the step never reached the top tier:\n{log}"
    );
}

#[test]
fn a_function_entered_by_a_compiled_caller_reaches_the_top_tier() {
    let (stdout, log) = run_logged("TestCompiledCaller", "test_compiled_caller.hl", &["wait"]);
    assert!(stdout.contains("done true"), "{stdout}");
    assert!(
        installed_at_llvm(&log, "CallerWorld.step"),
        "the step never reached the top tier:\n{log}"
    );
}
