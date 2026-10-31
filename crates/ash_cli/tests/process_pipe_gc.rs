//! A thread blocked reading a child's pipe, standard input or a socket lets a
//! collection and the other fibers proceed.
#![cfg(unix)]
mod common;

use common::{ash_cli_bin, haxe_available, render_output, run_with_timeout, tests_dir};
use std::process::Command;
use std::time::Duration;

#[test]
fn blocked_pipe_reader_does_not_stall_collection() {
    let dir = tempfile::tempdir().unwrap();
    let fixture = dir.path().join("process_pipe_gc.hl");
    if haxe_available() {
        let output = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", "TestProcessPipeGc", "-hl"])
            .arg(&fixture)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "haxe: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    } else {
        std::fs::copy(tests_dir().join("test_process_pipe_gc.hl"), &fixture)
            .expect("committed process pipe bytecode fixture");
    }
    for mode in [
        &["--mode", "interp"][..],
        &["--mode", "hybrid"][..],
        &["--mode", "jit"][..],
    ] {
        let mut command = Command::new(ash_cli_bin());
        command.args(mode).arg(&fixture);
        let result = run_with_timeout(command, Duration::from_secs(40));
        assert!(
            !result.timed_out && result.output.status.success(),
            "{mode:?}: {}",
            render_output(&result.output)
        );
        let stdout = String::from_utf8_lossy(&result.output.stdout);
        assert!(stdout.contains("collected"), "{mode:?}: {stdout}");
        let stderr = String::from_utf8_lossy(&result.output.stderr);
        assert!(
            !stderr.contains("gave up stopping the world"),
            "{mode:?}: {stderr}"
        );
    }
}

/// Build or fetch the committed bytecode for `main`, named `fixture`.
fn fixture(dir: &std::path::Path, main: &str, committed: &str) -> std::path::PathBuf {
    let out = dir.join(committed);
    if haxe_available() {
        let output = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", main, "-hl"])
            .arg(&out)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "haxe: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    } else {
        std::fs::copy(tests_dir().join(committed), &out).expect("committed bytecode fixture");
    }
    out
}

/// All fibers on one scheduler thread, so a blocked read that held its thread
/// would starve the sibling.
fn assert_sibling_runs(main: &str, committed: &str) {
    let dir = tempfile::tempdir().unwrap();
    let hl = fixture(dir.path(), main, committed);
    for mode in ["interp", "hybrid", "jit"] {
        let mut command = Command::new(ash_cli_bin());
        command
            .env("ASH_WORKERS", "0")
            .args(["--mode", mode])
            .arg(&hl)
            .stdin(std::process::Stdio::piped());
        let result = run_with_timeout(command, Duration::from_secs(20));
        assert!(
            !result.timed_out && result.output.status.success(),
            "{mode}: {}",
            render_output(&result.output)
        );
        let stdout = String::from_utf8_lossy(&result.output.stdout);
        assert!(stdout.contains("ran"), "{mode}: {stdout}");
    }
}

#[test]
fn blocked_pipe_reader_leaves_sibling_fiber_running() {
    assert_sibling_runs("TestProcessPipeFibers", "test_process_pipe_fibers.hl");
}

#[test]
fn blocked_stdin_reader_leaves_sibling_fiber_running() {
    assert_sibling_runs("TestStdinFibers", "test_stdin_fibers.hl");
}

#[test]
fn blocked_socket_calls_leave_sibling_fiber_running() {
    assert_sibling_runs("TestSocketFibers", "test_socket_fibers.hl");
}
