//! A function whose running frame handed off to the top tier has its promote
//! postponed. Entered again later from the interpreter, it is promoted.
mod common;

use common::{ash_cli_bin, haxe_available, render_output, run_with_timeout, tests_dir};
use std::process::Command;
use std::time::Duration;

#[test]
fn a_function_entered_again_after_its_hand_off_reaches_the_top_tier() {
    let dir = tempfile::tempdir().unwrap();
    let hl = dir.path().join("handoff_reentry.hl");
    if haxe_available() {
        let output = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", "TestHandoffReentry", "-hl"])
            .arg(&hl)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "haxe: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    } else {
        std::fs::copy(tests_dir().join("test_handoff_reentry.hl"), &hl)
            .expect("committed bytecode fixture");
    }
    let mut command = Command::new(ash_cli_bin());
    command.arg("--jit-log").arg(&hl);
    let result = run_with_timeout(command, Duration::from_secs(120));
    assert!(
        !result.timed_out && result.output.status.success(),
        "{}",
        render_output(&result.output)
    );
    let stdout = String::from_utf8_lossy(&result.output.stdout);
    let log = String::from_utf8_lossy(&result.output.stderr);
    assert!(
        stdout.contains("done true"),
        "{}",
        render_output(&result.output)
    );
    assert!(
        log.lines().any(|l| l.starts_with("[tier] install ")
            && l.contains("TestHandoffReentry.kernel")
            && l.contains("tier=llvm")),
        "the kernel never reached the top tier:\n{log}"
    );
}
