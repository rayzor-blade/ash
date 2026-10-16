//! A failed cast raised while reading a dynamic field is catchable in every mode.
mod common;

use common::{ash_cli_bin, haxe_available, render_output, run_with_timeout, tests_dir};
use std::process::Command;
use std::time::Duration;

const EXPECTED: &str = "\
caught Can't cast hl.types.ArrayBytes_Int to hl.types.ArrayBytes_Float
90
caught Can't cast hl.types.ArrayBytes_Int to hl.types.ArrayBytes_Float
90
caught Can't cast hl.types.ArrayBytes_Int to hl.types.ArrayBytes_Float
90
";

#[test]
fn a_failed_cast_reading_a_dynamic_field_reaches_the_catch() {
    let dir = tempfile::tempdir().unwrap();
    let fixture = dir.path().join("dyn_field_cast.hl");
    if haxe_available() {
        let output = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", "TestDynFieldCast", "-hl"])
            .arg(&fixture)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "haxe: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    } else {
        std::fs::copy(tests_dir().join("test_dyn_field_cast.hl"), &fixture)
            .expect("committed dynamic field cast bytecode fixture");
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
