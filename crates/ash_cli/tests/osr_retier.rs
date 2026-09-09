//! Re-tier snapshots must preserve the answer AND actually be exercised.
mod common;

use common::{ash_cli_bin, haxe_available, tests_dir};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{Duration, Instant};

fn scratch() -> PathBuf {
    let dir = std::env::temp_dir().join(format!("ash-retier-tests-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

fn fixture(main: &str) -> PathBuf {
    if !haxe_available() {
        let name = match main {
            "TestOsrRetier" => "test_osr_retier.hl",
            "TestRetierSnapshot" => "test_retier_snapshot.hl",
            _ => panic!("unknown fixture"),
        };
        let path = tests_dir().join(name);
        assert!(
            path.exists(),
            "missing compiled fixture: {}",
            path.display()
        );
        return path;
    }
    let path = scratch().join(format!("{main}.hl"));
    let out = Command::new("haxe")
        .arg("-cp")
        .arg(tests_dir())
        .args(["-main", main, "-hl"])
        .arg(&path)
        .output()
        .unwrap();
    assert!(
        out.status.success(),
        "haxe: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    path
}

fn run(hl: &Path, mode: &[&str], extra: &[(&str, &str)]) -> (String, String) {
    static NEXT: AtomicU64 = AtomicU64::new(0);
    let id = NEXT.fetch_add(1, Ordering::Relaxed);
    let stdout = scratch().join(format!("{id}.stdout"));
    let stderr = scratch().join(format!("{id}.stderr"));
    let mut cmd = Command::new(ash_cli_bin());
    cmd.args(mode)
        .arg(hl)
        .env_remove("ASH_TEST_RETIER_AFTER")
        .env_remove("ASH_RETIER_TEST_PLAIN")
        .env("ASH_OSR", "1")
        // Not pinned: the first test below is here to check the shipped
        // default, and pinning it meant that default was never exercised.
        .env_remove("ASH_CL_RETIER")
        .stdout(Stdio::from(std::fs::File::create(&stdout).unwrap()))
        .stderr(Stdio::from(std::fs::File::create(&stderr).unwrap()));
    for (k, v) in extra {
        cmd.env(k, v);
    }
    let mut child = cmd.spawn().unwrap();
    let deadline = Instant::now() + Duration::from_secs(120);
    let status = loop {
        if let Some(status) = child.try_wait().unwrap() {
            break status;
        }
        if Instant::now() >= deadline {
            child.kill().unwrap();
            child.wait().unwrap();
            panic!("guest timed out; {}", stderr.display());
        }
        std::thread::sleep(Duration::from_millis(10));
    };
    let log = std::fs::read_to_string(stderr).unwrap();
    assert!(status.success(), "guest failed: {status}\n{log}");
    let out = std::fs::read_to_string(stdout).unwrap();
    let line = out
        .lines()
        .find(|l| l.starts_with("iters="))
        .unwrap_or_else(|| panic!("missing result line: {out}\n{log}"))
        .to_owned();
    (line, log)
}

/// The divergence that `f042e3e` mitigated by refusing the hand-off, run
/// against whatever the default is now. It reported 249,960 iterations on one
/// run and about 40,000 on others before `37adeba` transferred the loop's live
/// registers; the attempts are repeated because it never failed every time.
#[test]
fn a_hot_loop_keeps_its_counters_across_a_re_tier() {
    let hl = fixture("TestOsrRetier");
    let (interp, _) = run(&hl, &["--mode", "interp"], &[]);
    for attempt in 0..6 {
        let (hybrid, log) = run(&hl, &["--mode", "hybrid"], &[]);
        assert_eq!(hybrid, interp, "attempt {attempt}\n{log}");
    }
}

#[test]
fn forced_snapshots_from_ordinary_and_osr_entries_preserve_live_state() {
    let hl = fixture("TestRetierSnapshot");
    for plain in ["0", "1"] {
        let (interp, _) = run(
            &hl,
            &["--mode", "interp"],
            &[("ASH_RETIER_TEST_PLAIN", plain)],
        );
        for (mode, source) in [("jit", "ordinary"), ("hybrid", "osr")] {
            let (answer, log) = run(
                &hl,
                &[
                    "--mode",
                    mode,
                    "--jit-threshold",
                    "1",
                    "--opt-threshold",
                    "10000",
                ],
                &[
                    ("ASH_CL_RETIER", "1"),
                    ("ASH_TEST_RETIER_AFTER", "4096"),
                    ("ASH_OSR_LOG", "1"),
                    ("ASH_RETIER_TEST_PLAIN", plain),
                ],
            );
            assert_eq!(answer, interp, "{mode}, plain={plain}\n{log}");
            assert!(
                log.lines().any(|l| l.starts_with("[retier] taken ")
                    && l.ends_with(&format!("source={source}"))),
                "{mode}, plain={plain} never took the intended compiled hand-off\n{log}"
            );
        }
    }
}
