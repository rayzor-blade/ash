//! A boxed primitive reaching a compiled callee that declares Bool/Int/Float
//! must arrive unboxed, as it does for an interpreted callee. Hybrid is the
//! mode that matters: an interpreted caller hands the box to compiled code.
//!
//! Every mode runs before the assertion, so a failure lists all of them.

mod common;

use common::{ash_cli_bin, haxe_available, render_output, run_with_timeout, tests_dir};
use std::process::Command;
use std::time::Duration;

const MODES: &[&[&str]] = &[
    &["--mode", "interp"],
    &["--mode", "jit"],
    &["--mode", "hybrid"],
];

#[test]
fn boxed_primitive_reaches_compiled_callee_unboxed() {
    let dir = tempfile::tempdir().unwrap();
    let hl = dir.path().join("boxed_primitive_arg.hl");
    if haxe_available() {
        let compile = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", "TestBoxedPrimitiveArg", "-hl"])
            .arg(&hl)
            .output()
            .unwrap();
        assert!(compile.status.success(), "{}", render_output(&compile));
    } else {
        std::fs::copy(tests_dir().join("test_boxed_primitive_arg.hl"), &hl)
            .expect("committed boxed-primitive bytecode fixture");
    }

    let expected = "wrong bool=0 int=0 float=0\n";
    let mut failures = Vec::new();
    for args in MODES {
        for osr in ["1", "0"] {
            let mut command = Command::new(ash_cli_bin());
            command
                .args(*args)
                .arg(&hl)
                .env("ASH_WORKERS", "0")
                .env("ASH_OSR", osr);
            let result = run_with_timeout(command, Duration::from_secs(60));
            let stdout = String::from_utf8_lossy(&result.output.stdout);
            if result.timed_out || !result.output.status.success() || !stdout.contains(expected) {
                failures.push(format!(
                    "{args:?} ASH_OSR={osr}{}:\n{}",
                    if result.timed_out { " (timed out)" } else { "" },
                    render_output(&result.output)
                ));
            }
        }
    }
    assert!(
        failures.is_empty(),
        "expected stdout to contain:\n{expected}\n{}",
        failures.join("\n")
    );
}
