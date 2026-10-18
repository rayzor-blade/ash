//! The regexp's subject cache recognises a string by address and terminator,
//! not by comparing it whole on every call.
mod common;

use common::{ash_cli_bin, haxe_available, render_output, run_with_timeout, tests_dir};
use std::process::Command;
use std::time::Duration;

const EXPECTED: &str = "1 false\n2 true\n3 3\nmatches 200000\nfast\n";

#[test]
fn a_grown_string_buffer_is_matched_afresh_and_advancing_matches_stay_linear() {
    let dir = tempfile::tempdir().unwrap();
    let fixture = dir.path().join("regex_subject.hl");
    if haxe_available() {
        let output = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", "TestRegexSubject", "-hl"])
            .arg(&fixture)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "haxe: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    } else {
        std::fs::copy(tests_dir().join("test_regex_subject.hl"), &fixture)
            .expect("committed regexp subject bytecode fixture");
    }
    for mode in ["interp", "hybrid"] {
        let mut command = Command::new(ash_cli_bin());
        command.args(["--mode", mode]).arg(&fixture);
        let result = run_with_timeout(command, Duration::from_secs(120));
        assert!(
            !result.timed_out && result.output.status.success(),
            "{mode}: {}",
            render_output(&result.output)
        );
        let stdout = String::from_utf8_lossy(&result.output.stdout);
        let lines: String = stdout
            .lines()
            .filter(|l| !l.ends_with("returned: Void"))
            .map(|l| format!("{l}\n"))
            .collect();
        assert_eq!(lines, EXPECTED, "{mode}");
    }
}
