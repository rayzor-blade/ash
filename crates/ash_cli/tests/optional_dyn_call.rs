//! A method called through Dynamic without its optional parameters reads them
//! as null in every mode, including once the caller is compiled.
mod common;

use common::{ash_cli_bin, haxe_available, render_output, run_with_timeout, tests_dir};
use std::process::Command;
use std::time::Duration;

// The output of Haxe's own evaluator for TestOptionalDynCall.
const EXPECTED: &str = "\
hot indexOf total: 120000
indexOf: 6
indexOf from: 26
substr: ror SCENE_BODY_SCALE at entities
substr len: ror 
lastIndexOf: 32
split: 3
";

#[test]
fn omitted_optional_arguments_read_as_null() {
    let dir = tempfile::tempdir().unwrap();
    let fixture = dir.path().join("optional_dyn_call.hl");
    if haxe_available() {
        let output = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", "TestOptionalDynCall", "-hl"])
            .arg(&fixture)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "haxe: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    } else {
        std::fs::copy(tests_dir().join("test_optional_dyn_call.hl"), &fixture)
            .expect("committed optional argument bytecode fixture");
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
