//! Dynamic comparisons with NaN are IEEE in every engine: NaN equals nothing
//! (itself included) and satisfies no ordering test.
mod common;

use common::{ash_cli_bin, haxe_available, render_output, run_with_timeout, tests_dir};
use std::process::Command;
use std::time::Duration;

// The output of Haxe's evaluator for TestDynNaN.
const EXPECTED: &str = "\
nan,nan == false != true < false <= false > false >= false
nan,1.0 == false != true < false <= false > false >= false
1.0,nan == false != true < false <= false > false >= false
nan,2 == false != true < false <= false > false >= false
2,nan == false != true < false <= false > false >= false
1.0,1.0 == true != false < false <= true > false >= true
1.0,2 == false != true < true <= true > false >= false
";

#[test]
fn nan_is_unordered_in_dynamic_comparisons() {
    let dir = tempfile::tempdir().unwrap();
    let fixture = dir.path().join("dyn_nan.hl");
    if haxe_available() {
        let output = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", "TestDynNaN", "-hl"])
            .arg(&fixture)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "haxe: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    } else {
        std::fs::copy(tests_dir().join("test_dyn_nan.hl"), &fixture)
            .expect("committed dynamic NaN bytecode fixture");
    }
    let mut modes: Vec<&[&str]> = vec![
        &["--mode", "interp"],
        &["--mode", "hybrid", "--jit-threshold", "1"],
        &["--mode", "jit"],
    ];
    if cfg!(feature = "llvm") {
        modes.push(&["--mode", "jit", "--jit-tier", "llvm"]);
    }
    for mode in modes {
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
