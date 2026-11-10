//! A primitive missing from an optional library (`?lib`) raises when called;
//! it does not stop the program from loading or running. HashLink binds it to
//! a stub that raises "Primitive or library is missing".
mod common;

use common::{ash_cli_bin, haxe_available, render_output, run_with_timeout, tests_dir};
use std::process::Command;
use std::time::Duration;

#[test]
fn missing_optional_primitive_raises_when_called() {
    let dir = tempfile::tempdir().unwrap();
    let hl = dir.path().join("optional_prim.hl");
    if haxe_available() {
        let output = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", "TestOptionalPrim", "-hl"])
            .arg(&hl)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "haxe: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    } else {
        std::fs::copy(tests_dir().join("test_optional_prim.hl"), &hl)
            .expect("committed bytecode fixture");
    }
    for mode in ["interp", "hybrid", "jit"] {
        let mut command = Command::new(ash_cli_bin());
        command.args(["--mode", mode]).arg(&hl);
        let result = run_with_timeout(command, Duration::from_secs(30));
        assert!(
            !result.timed_out && result.output.status.success(),
            "{mode}: {}",
            render_output(&result.output)
        );
        let stdout = String::from_utf8_lossy(&result.output.stdout);
        let lines: Vec<&str> = stdout
            .lines()
            .filter(|l| !l.contains(" returned: "))
            .collect();
        assert_eq!(
            lines,
            [
                "start",
                "after-skip",
                "caught: Primitive or library is missing",
                "end"
            ],
            "{mode}: {}",
            render_output(&result.output)
        );
    }
}
