//! A closure that captures null still receives it as its first argument, in every mode.
mod common;

use common::{ash_cli_bin, haxe_available, render_output, run_with_timeout, tests_dir};
use std::process::Command;
use std::time::Duration;

// The output of Haxe's evaluator and of stock HashLink 1.15 for TestNullBoundClosure.
const EXPECTED: &str = "\
direct false true
exists null true
exists absent false
count 3
map bone0,bone1,bone2,n
filter 1
object false
bound object true
";

#[test]
fn a_closure_bound_to_null_keeps_its_arguments_aligned() {
    let dir = tempfile::tempdir().unwrap();
    let fixture = dir.path().join("null_bound_closure.hl");
    if haxe_available() {
        let output = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", "TestNullBoundClosure", "-hl"])
            .arg(&fixture)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "haxe: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    } else {
        std::fs::copy(tests_dir().join("test_null_bound_closure.hl"), &fixture)
            .expect("committed null bound closure bytecode fixture");
    }
    for mode in [
        &["--mode", "interp"][..],
        &["--mode", "hybrid", "--jit-threshold", "1"][..],
        &["--mode", "jit"][..],
    ] {
        let mut command = Command::new(ash_cli_bin());
        command.args(mode).arg(&fixture);
        let result = run_with_timeout(command, Duration::from_secs(60));
        assert!(
            !result.timed_out && result.output.status.success(),
            "{mode:?}: {}",
            render_output(&result.output)
        );
        let stdout = String::from_utf8_lossy(&result.output.stdout);
        let lines: String = stdout
            .lines()
            .filter(|l| !l.ends_with("returned: Void"))
            .map(|l| format!("{l}\n"))
            .collect();
        assert_eq!(lines, EXPECTED, "{mode:?}");
    }
}
