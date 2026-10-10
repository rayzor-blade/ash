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
            "TestOsrRefBeforeLoop" => "test_osr_ref_before_loop.hl",
            "TestOsrF32" => "test_osr_f32.hl",
            "TestOsrNested" => "test_osr_nested.hl",
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
    run_for(hl, mode, extra, "iters=")
}

/// Runs `hl` and returns its first stdout line starting with `result`, and the log.
fn run_for(
    hl: &Path,
    mode: &[&str],
    extra: &[(&str, &str)],
    result: &str,
) -> (String, String) {
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
        .find(|l| l.starts_with(result))
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

/// A pointer passed to a call is held by the frame alone, so a loop behind it
/// can be entered mid-flight; a loop with two back edges refuses only itself.
/// `carried` writes through a ref on every iteration, so the entry has to
/// point it at its own cell.
#[test]
fn a_loop_behind_a_ref_is_entered_mid_flight() {
    let hl = fixture("TestOsrRefBeforeLoop");
    let (interp, _) = run_for(&hl, &["--mode", "interp"], &[], "total=");
    // An LLVM entry takes long enough to build that a short loop can be over
    // before it arrives, so only the Cranelift tier is held to a count.
    for (tier, at_least) in [("cranelift", 2), ("llvm", 0)] {
        let (answer, log) = run_for(
            &hl,
            &["--mode", "hybrid", "--jit-tier", tier, "--jit-threshold", "1"],
            &[("ASH_OSR_LOG", "1"), ("ASH_OSR_ENTRY_SYNC", "1")],
            "total=",
        );
        assert_eq!(answer, interp, "{tier}\n{log}");
        let entered = log
            .lines()
            .filter(|l| l.starts_with("[osr] entering findex="))
            .count();
        assert!(
            entered >= at_least,
            "{tier} entered compiled code {entered} times\n{log}"
        );
    }
    let (answer, log) = run_for(&hl, &["--mode", "jit"], &[], "total=");
    assert_eq!(answer, interp, "{log}");
}

/// The interpreter holds an f32 register as the f64 it widens to; both
/// compiled entries have to narrow it again.
#[test]
fn an_f32_loop_is_entered_mid_flight() {
    let hl = fixture("TestOsrF32");
    let (interp, _) = run_for(&hl, &["--mode", "interp"], &[], "f32=");
    for tier in ["cranelift", "llvm"] {
        let (answer, log) = run_for(
            &hl,
            &["--mode", "hybrid", "--jit-tier", tier, "--jit-threshold", "1"],
            &[("ASH_OSR_LOG", "1"), ("ASH_OSR_ENTRY_SYNC", "1")],
            "f32=",
        );
        assert_eq!(answer, interp, "{tier}\n{log}");
        if tier == "cranelift" {
            assert!(
                log.lines().any(|l| l.starts_with("[osr] entering findex=")),
                "{tier} never entered compiled code mid-loop\n{log}"
            );
        }
    }
}

/// An inner loop reads values its outer loop defines on every pass: the entry
/// takes them from the interpreter on the first pass and from the outer body
/// on the later ones, and no entry is refused over it.
#[test]
fn a_loop_inside_a_loop_is_entered_mid_flight() {
    let hl = fixture("TestOsrNested");
    let (interp, _) = run_for(&hl, &["--mode", "interp"], &[], "nested=");
    for tier in ["cranelift", "llvm"] {
        let (answer, log) = run_for(
            &hl,
            &["--mode", "hybrid", "--jit-tier", tier, "--jit-threshold", "1"],
            &[("ASH_OSR_LOG", "1"), ("ASH_OSR_ENTRY_SYNC", "1")],
            "nested=",
        );
        assert_eq!(answer, interp, "{tier}\n{log}");
        if tier == "cranelift" {
            let entered = log
                .lines()
                .filter(|l| l.starts_with("[osr] entering findex="))
                .count();
            assert!(
                entered >= 3,
                "{tier} entered compiled code {entered} times\n{log}"
            );
            assert!(!log.contains("declined"), "{tier} refused an entry\n{log}");
        }
    }
}
