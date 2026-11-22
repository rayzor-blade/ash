//! A value array starts inside its allocation so its element data is 16-byte
//! aligned. Collections must keep such an array and its contents intact.
mod common;

use common::{ash_cli_bin, haxe_available, render_output, run_with_timeout, tests_dir};
use std::path::PathBuf;
use std::process::Command;
use std::time::Duration;

/// The program built from source, or the committed bytecode when haxe is absent.
fn program(dir: &std::path::Path) -> PathBuf {
    let hl = dir.join("program.hl");
    if haxe_available() {
        let output = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", "TestValueArrays", "-hl"])
            .arg(&hl)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "haxe: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    } else {
        std::fs::copy(tests_dir().join("test_value_arrays.hl"), &hl)
            .expect("committed bytecode fixture");
    }
    hl
}

fn run(args: &[&str], stress: bool) {
    let dir = tempfile::tempdir().unwrap();
    let mut command = Command::new(ash_cli_bin());
    command.args(args).arg(program(dir.path()));
    if stress {
        command.env("ASH_GC_STRESS", "1");
    }
    let result = run_with_timeout(command, Duration::from_secs(120));
    assert!(
        !result.timed_out && result.output.status.success(),
        "{}",
        render_output(&result.output)
    );
    let stdout = String::from_utf8_lossy(&result.output.stdout);
    assert!(stdout.contains("done true"), "{stdout}");
}

#[test]
fn value_arrays_survive_collections_in_the_default_mode() {
    run(&[], false);
}

#[test]
fn value_arrays_survive_collections_in_the_jit() {
    run(&["--mode", "jit"], false);
}

#[test]
fn value_arrays_survive_a_collection_at_every_allocation() {
    run(&[], true);
}
