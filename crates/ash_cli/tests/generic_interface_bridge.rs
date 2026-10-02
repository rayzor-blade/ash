//! Calls through a generic interface slot must behave as the same calls made
//! directly: an f32 return keeps its value, and an exception reaches the
//! caller's `try`. The expected output is stock HashLink's.
//!
//! Every mode runs before the assertion, so a failure lists all of them.

mod common;

use common::{ash_cli_bin, haxe_available, render_output, run_with_timeout, tests_dir};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Duration;

const MODES: &[&[&str]] = &[
    &["--mode", "interp"],
    &["--mode", "jit"],
    &["--mode", "hybrid"],
    &["--mode", "hybrid", "--jit-threshold", "1"],
];

fn fixture(dir: &Path) -> PathBuf {
    let hl = dir.join("generic_interface_bridge.hl");
    if haxe_available() {
        let compile = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", "TestGenericInterfaceBridge", "-hl"])
            .arg(&hl)
            .output()
            .unwrap();
        assert!(compile.status.success(), "{}", render_output(&compile));
    } else {
        std::fs::copy(tests_dir().join("test_generic_interface_bridge.hl"), &hl)
            .expect("committed generic interface bytecode fixture");
    }
    hl
}

fn run_section(section: &str, expected: &str) {
    let dir = tempfile::tempdir().unwrap();
    let hl = fixture(dir.path());
    let mut failures = Vec::new();
    for args in MODES {
        let mut command = Command::new(ash_cli_bin());
        command
            .args(*args)
            .arg(&hl)
            .arg(section)
            .env("ASH_WORKERS", "0");
        let result = run_with_timeout(command, Duration::from_secs(60));
        let stdout = String::from_utf8_lossy(&result.output.stdout);
        if result.timed_out || !result.output.status.success() || !stdout.contains(expected) {
            failures.push(format!(
                "{args:?}{}:\n{}",
                if result.timed_out { " (timed out)" } else { "" },
                render_output(&result.output)
            ));
        }
    }
    assert!(
        failures.is_empty(),
        "expected stdout to contain:\n{expected}\n{}",
        failures.join("\n")
    );
}

#[test]
fn single_returned_through_generic_interface_keeps_its_value() {
    run_section(
        "single",
        "direct Single = 200.500000000\n\
         IGet<Single> = 200.500000000\n\
         IGet<Int> = 200\n\
         IGet<Float> = 200.5\n",
    );
}

#[test]
fn exception_through_generic_interface_reaches_callers_try() {
    run_section(
        "throw",
        "direct: caught boom\n\
         IFailInt: caught boom\n\
         IFail<Int>: caught boom\n\
         returned\n",
    );
}
