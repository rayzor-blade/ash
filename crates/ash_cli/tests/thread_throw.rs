//! An exception escaping a thread body ends that thread; the program goes on.
mod common;

use common::{ash_cli_bin, haxe_available, render_output, run_with_timeout, tests_dir};
use std::process::Command;
use std::time::Duration;

#[test]
fn thread_body_throw_does_not_abort_the_process() {
    let dir = tempfile::tempdir().unwrap();
    let hl = dir.path().join("thread_throw.hl");
    if haxe_available() {
        let output = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", "TestThreadThrow", "-hl"])
            .arg(&hl)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "haxe: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    } else {
        std::fs::copy(tests_dir().join("test_thread_throw.hl"), &hl)
            .expect("committed bytecode fixture");
    }
    for mode in ["interp", "hybrid", "jit"] {
        for workers in ["0", "4"] {
            let mut command = Command::new(ash_cli_bin());
            command
                .env("ASH_WORKERS", workers)
                .args(["--mode", mode])
                .arg(&hl);
            let result = run_with_timeout(command, Duration::from_secs(20));
            assert!(
                !result.timed_out && result.output.status.success(),
                "{mode} workers={workers}: {}",
                render_output(&result.output)
            );
            let stdout = String::from_utf8_lossy(&result.output.stdout);
            assert!(
                stdout.contains("survived"),
                "{mode} workers={workers}: {stdout}"
            );
        }
    }

    // A compiled program carries no interpreter to supply the frame, so it
    // brings its own.
    let binary = dir.path().join("thread_throw_aot");
    let mut build = Command::new(ash_cli_bin());
    build.arg("--quiet").arg("--build").arg(&binary).arg(&hl);
    let built = run_with_timeout(build, Duration::from_secs(300));
    assert!(
        !built.timed_out && built.output.status.success(),
        "aot build: {}",
        render_output(&built.output)
    );
    let ran = run_with_timeout(Command::new(&binary), Duration::from_secs(20));
    assert!(
        !ran.timed_out && ran.output.status.success(),
        "aot: {}",
        render_output(&ran.output)
    );
    assert!(
        String::from_utf8_lossy(&ran.output.stdout).contains("survived"),
        "aot: {}",
        render_output(&ran.output)
    );
}
