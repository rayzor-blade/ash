//! A cast that fails inside `try` must be caught, not abort the process. The
//! interpreter raises it from `hlp_dyn_castp`, which needs the same setjmp
//! boundary a native call has.
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
];

fn fixture(dir: &Path) -> PathBuf {
    let hl = dir.join("cast_in_try.hl");
    if haxe_available() {
        let compile = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", "TestCastInTry", "-hl"])
            .arg(&hl)
            .output()
            .unwrap();
        assert!(compile.status.success(), "{}", render_output(&compile));
    } else {
        std::fs::copy(tests_dir().join("test_cast_in_try.hl"), &hl)
            .expect("committed cast-in-try bytecode fixture");
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
fn failed_safe_cast_reaches_catch() {
    run_section("safecast", "caught: Can't cast i32 to hl.BaseType\nafter\n");
}

#[test]
fn failed_call_method_argument_cast_reaches_catch() {
    run_section("callmethod", "caught: Can't cast Other to Box\nafter\n");
}

#[test]
fn array_cast_rejected_by_cast_reaches_catch() {
    run_section(
        "variance",
        "caught: Can't cast hl.types.ArrayBytes_Float to hl.types.ArrayObj\nafter\n",
    );
}
