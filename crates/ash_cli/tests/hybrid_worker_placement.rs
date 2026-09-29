mod common;

use common::{ash_cli_bin, haxe_available, render_output, run_with_timeout, tests_dir};
use std::process::Command;
use std::time::Duration;

#[test]
fn cold_thread_roots_dispatch_from_hybrid_main() {
    let dir = tempfile::tempdir().unwrap();
    let fixture = dir.path().join("barriers.hl");
    if haxe_available() {
        let compile = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", "BenchWorkerBarriers", "-hl"])
            .arg(&fixture)
            .output()
            .unwrap();
        assert!(
            compile.status.success(),
            "haxe: {}",
            render_output(&compile)
        );
    } else {
        std::fs::copy(tests_dir().join("bench_worker_barriers.hl"), &fixture)
            .expect("committed worker barrier bytecode fixture");
    }

    let mut command = Command::new(ash_cli_bin());
    command
        .args(["--mode", "hybrid", "--jit-tier", "cranelift", "--jit-log"])
        .arg(&fixture)
        .args(["16", "20"])
        .env("ASH_WORKERS", "4")
        .env("ASH_FIBER_TRACE", "1")
        .env_remove("ASH_STUB_COMPILE");
    let run = run_with_timeout(command, Duration::from_secs(90));
    assert!(!run.timed_out, "hybrid worker test timed out");
    assert!(
        run.output.status.success(),
        "{}",
        render_output(&run.output)
    );
    let stdout = String::from_utf8_lossy(&run.output.stdout);
    let stderr = String::from_utf8_lossy(&run.output.stderr);
    assert!(stdout.contains("done=320"), "{stdout}");
    assert!(!stderr.contains("resolve-failed"), "{stderr}");
    let dispatches = stderr
        .lines()
        .filter(|line| line.contains(" dispatch id="))
        .count();
    assert_eq!(dispatches, 4, "{stderr}");
}
