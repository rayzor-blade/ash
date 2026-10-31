//! A thread blocked reading a child's pipe lets a collection by another
//! thread proceed.
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
    // Interpreted threads share one OS thread, so only compiled modes apply.
    for mode in [&["--mode", "hybrid"][..], &["--mode", "jit"][..]] {
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
