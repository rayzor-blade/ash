//! A reload that adds, removes, moves or edits closures runs end to end.
//!
//! Each case runs `from` under `--hot-reload`, replaces its file with `to`
//! once it says `ready`, and compares what it printed afterwards. The program
//! calls its closures from the frame that began before the reload, through
//! `Reflect`, and from compiled code, which are the three ways a closure of
//! the new program reaches a caller of the old one.
mod common;

use common::{ash_cli_bin, haxe_available, repo_root};
use std::io::{BufRead, BufReader, Read};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Mutex;
use std::time::{Duration, Instant};

fn scratch() -> PathBuf {
    let dir = std::env::temp_dir().join(format!("ash-hot-reload-tests-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

fn fixtures() -> PathBuf {
    repo_root().join("crates/ash/test/hot_reload/closures")
}

/// The compiled program of version `v`: built with haxe when it is there,
/// else the one committed beside the sources. Built once, whichever test asks
/// first.
fn version(v: &str) -> PathBuf {
    static BUILT: Mutex<Vec<(String, PathBuf)>> = Mutex::new(Vec::new());
    let mut built = BUILT.lock().unwrap_or_else(|e| e.into_inner());
    if let Some((_, path)) = built.iter().find(|(name, _)| name == v) {
        return path.clone();
    }
    let path = if haxe_available() {
        let path = scratch().join(format!("closures-{v}.hl"));
        let out = Command::new("haxe")
            .arg("--cwd")
            .arg(fixtures().join(v))
            .args(["-main", "TestHotReloadClosures", "-hl"])
            .arg(&path)
            .output()
            .unwrap();
        assert!(
            out.status.success(),
            "haxe: {}",
            String::from_utf8_lossy(&out.stderr)
        );
        path
    } else {
        let path = fixtures().join(format!("{v}.hl"));
        assert!(path.exists(), "missing compiled fixture: {}", path.display());
        path
    };
    built.push((v.to_string(), path.clone()));
    path
}

/// Runs `from`, swaps in `to`, and returns the lines the program printed
/// after `ready`, with ash's own stderr.
fn reload(from: &str, to: &str, flags: &[&str]) -> (Vec<String>, String) {
    static NEXT: AtomicU64 = AtomicU64::new(0);
    let dir = scratch().join(format!(
        "{from}-{to}-{}",
        NEXT.fetch_add(1, Ordering::Relaxed)
    ));
    std::fs::create_dir_all(&dir).unwrap();
    let program = dir.join("program.hl");
    std::fs::copy(version(from), &program).unwrap();
    let stderr_path = dir.join("stderr");
    let mut child = Command::new(ash_cli_bin())
        .args(["--mode", "hybrid", "--hot-reload", "--quiet"])
        .args(flags)
        .arg(&program)
        .stdout(Stdio::piped())
        .stderr(Stdio::from(std::fs::File::create(&stderr_path).unwrap()))
        .spawn()
        .unwrap();
    let mut lines = BufReader::new(child.stdout.take().unwrap()).lines();
    let stderr = || std::fs::read_to_string(&stderr_path).unwrap_or_default();
    loop {
        match lines.next() {
            Some(Ok(line)) if line == "ready" => break,
            Some(Ok(_)) => {}
            _ => panic!("{from} -> {to} {flags:?}: never said ready\n{}", stderr()),
        }
    }
    // The reload is found by the file's modification time.
    std::thread::sleep(Duration::from_millis(1100));
    let next = dir.join("next.hl");
    std::fs::copy(version(to), &next).unwrap();
    std::fs::rename(&next, &program).unwrap();

    let deadline = Instant::now() + Duration::from_secs(120);
    let mut printed = Vec::new();
    let reader = std::thread::spawn(move || {
        for line in lines.map_while(Result::ok) {
            printed.push(line);
        }
        printed
    });
    while child.try_wait().unwrap().is_none() {
        if Instant::now() > deadline {
            child.kill().ok();
            panic!("{from} -> {to} {flags:?}: timed out\n{}", stderr());
        }
        std::thread::sleep(Duration::from_millis(50));
    }
    let printed = reader.join().unwrap();
    let mut rest = String::new();
    std::fs::File::open(&stderr_path)
        .unwrap()
        .read_to_string(&mut rest)
        .unwrap();
    (printed, rest)
}

/// The tier arrangements a reload has to work under: the interpreter alone
/// until the thresholds are met, then each compiled tier from the first call.
const ARRANGEMENTS: &[&[&str]] = &[
    &[],
    &["--jit-threshold", "1"],
    &["--jit-tier", "cranelift", "--jit-threshold", "1"],
    &["--jit-tier", "llvm", "--jit-threshold", "1"],
];

fn check(from: &str, to: &str, expected: &[&str], log: &str) {
    for flags in ARRANGEMENTS {
        let (printed, stderr) = reload(from, to, flags);
        assert_eq!(
            printed, expected,
            "{from} -> {to} {flags:?}\n--- ash stderr\n{stderr}"
        );
        assert!(
            stderr.contains(log),
            "{from} -> {to} {flags:?}: expected `{log}` in\n{stderr}"
        );
    }
}

/// A closure added between two others: the two keep running, and the new one
/// is called from every kind of caller.
#[test]
fn a_closure_added_between_two_others() {
    check(
        "v1",
        "v2",
        &[
            "reloaded",
            "old 6,10",
            "new 6,105,10",
            "reflect 6,105,10",
            "apply 60,1050,100",
            "done",
        ],
        "1 added",
    );
}

/// A closure removed: one made before the reload still runs its old body.
#[test]
fn a_closure_removed() {
    check(
        "v1",
        "v3",
        &[
            "reloaded",
            "old 6,10",
            "new 10",
            "reflect 10",
            "apply 100",
            "done",
        ],
        "reloaded",
    );
}

/// Closures moved and one added in front of them.
#[test]
fn closures_moved_and_one_added() {
    check(
        "v1",
        "v4",
        &[
            "reloaded",
            "old 6,10",
            "new -2,10,6",
            "reflect -2,10,6",
            "apply -20,100,60",
            "done",
        ],
        "1 added",
    );
}

/// A closure edited in place: one made before the reload runs the new body.
#[test]
fn a_closure_edited_in_place() {
    check(
        "v1",
        "v5",
        &[
            "reloaded",
            "old 6,15",
            "new 6,15",
            "reflect 6,15",
            "apply 60,150",
            "done",
        ],
        "reloaded",
    );
}

#[test]
fn the_fixtures_are_where_the_test_expects() {
    for v in ["v1", "v2", "v3", "v4", "v5"] {
        assert!(Path::new(&fixtures().join(v).join("TestHotReloadClosures.hx")).exists());
    }
}
