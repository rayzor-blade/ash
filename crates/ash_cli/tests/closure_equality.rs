//! Closures compared through Dynamic are equal only when they run the same
//! function over the same bound value, in every execution mode.
mod common;

use common::{ash_cli_bin, haxe_available, render_output, run_with_timeout, tests_dir};
use std::process::Command;
use std::time::Duration;

// The output of stock HashLink 1.15 for TestClosureEquality.
const EXPECTED: &str = "\
capture remaining=1 seen=1
same object 0 other object 1
other method -1
method remaining=1 a=1 b=0
static 0 capture 1 missing -1
compareMethods false true true false
";

#[test]
fn closures_compare_by_function_and_bound_value() {
    let dir = tempfile::tempdir().unwrap();
    let fixture = dir.path().join("closure_equality.hl");
    if haxe_available() {
        let output = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", "TestClosureEquality", "-hl"])
            .arg(&fixture)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "haxe: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    } else {
        std::fs::copy(tests_dir().join("test_closure_equality.hl"), &fixture)
            .expect("committed closure equality bytecode fixture");
    }
    for mode in ["interp", "hybrid", "jit"] {
        let mut command = Command::new(ash_cli_bin());
        command.args(["--mode", mode]).arg(&fixture);
        let result = run_with_timeout(command, Duration::from_secs(60));
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
