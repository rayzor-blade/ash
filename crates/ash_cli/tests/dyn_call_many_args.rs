//! A dynamic call with nine arguments passes the ninth on the stack, in every mode.
mod common;

use common::{ash_cli_bin, haxe_available, render_output, run_with_timeout, tests_dir};
use std::process::Command;
use std::time::Duration;

// The output of Haxe's evaluator and of stock HashLink 1.15 for TestDynCallManyArgs.
const EXPECTED: &str = "\
dyn sum8: 304
dyn mixed8: 155
dyn floats8: 322
dyn strs8: abcdefgh
reflect sum8: 304
dyn static9: 285
reflect static9: 285
dyn staticf9: 307.5
reflect staticf9: 307.5
dyn staticm9: 80
dyn statics9: abcdefghi
";

#[test]
fn the_ninth_argument_of_a_dynamic_call_arrives() {
    let dir = tempfile::tempdir().unwrap();
    let fixture = dir.path().join("dyn_call_many_args.hl");
    if haxe_available() {
        let output = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", "TestDynCallManyArgs", "-hl"])
            .arg(&fixture)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "haxe: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    } else {
        std::fs::copy(tests_dir().join("test_dyn_call_many_args.hl"), &fixture)
            .expect("committed many-argument bytecode fixture");
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
