//! A counted loop keeps its fiber poll but holds it in an outer loop, so the
//! inner loop vectorizes. The answers do not change, with or without it.
mod common;

use common::{ash_cli_bin, haxe_available, render_output, run_with_timeout, tests_dir};
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

/// Arguments and environment of one run.
type Configuration<'a> = (&'a [&'a str], &'a [(&'a str, &'a str)]);

fn bytecode(dir: &tempfile::TempDir) -> PathBuf {
    let hl = dir.path().join("nested_strip.hl");
    if haxe_available() {
        let output = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", "TestNestedStrip", "-hl"])
            .arg(&hl)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "haxe: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    } else {
        std::fs::copy(tests_dir().join("test_nested_strip.hl"), &hl)
            .expect("committed bytecode fixture");
    }
    hl
}

/// Standard output and standard error of one run.
fn run(hl: &PathBuf, args: &[&str], env: &[(&str, &str)]) -> (String, String) {
    let mut command = Command::new(ash_cli_bin());
    command.args(args).arg(hl);
    for (key, value) in env {
        command.env(key, value);
    }
    let result = run_with_timeout(command, Duration::from_secs(120));
    assert!(
        !result.timed_out && result.output.status.success(),
        "{args:?} {env:?}: {}",
        render_output(&result.output)
    );
    (
        String::from_utf8_lossy(&result.output.stdout).into_owned(),
        String::from_utf8_lossy(&result.output.stderr).into_owned(),
    )
}

#[test]
fn every_engine_prints_what_the_interpreter_prints() {
    let dir = tempfile::tempdir().unwrap();
    let hl = bytecode(&dir);
    let (expected, _) = run(&hl, &["--mode", "interp"], &[]);
    assert!(expected.contains("between 747 300 648 0 0"), "{expected}");

    let llvm = ["--mode", "jit", "--jit-tier", "llvm"];
    let every = ["--mode", "hybrid", "--jit-tier", "llvm", "--jit-threshold", "1"];
    let configurations: [Configuration; 5] = [
        (&[], &[]),
        (&llvm, &[]),
        (&every, &[]),
        (&llvm, &[("ASH_NESTED_STRIP", "0")]),
        (&["--mode", "jit", "--jit-tier", "cranelift"], &[]),
    ];
    for (args, env) in configurations {
        let (stdout, _) = run(&hl, args, env);
        assert_eq!(stdout, expected, "{args:?} {env:?}");
    }
}

/// The LLVM IR of every function the program runs, as it will execute. Dumped
/// to a file: the text is far past what a pipe holds while the test waits.
fn ir_dump(hl: &PathBuf, env: &[(&str, &str)]) -> String {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("ir.txt");
    let mut command = Command::new(ash_cli_bin());
    command
        .args(["--mode", "jit", "--jit-tier", "llvm"])
        .arg(hl)
        .env("ASH_DUMP_FN_IR", "all")
        .stdout(Stdio::null())
        .stderr(Stdio::from(std::fs::File::create(&path).unwrap()));
    for (key, value) in env {
        command.env(key, value);
    }
    let mut child = command.spawn().unwrap();
    let start = Instant::now();
    loop {
        if let Some(status) = child.try_wait().unwrap() {
            assert!(status.success(), "{env:?}: {status}");
            break;
        }
        if start.elapsed() > Duration::from_secs(120) {
            let _ = child.kill();
            panic!("{env:?}: timed out");
        }
        std::thread::sleep(Duration::from_millis(20));
    }
    String::from_utf8_lossy(&std::fs::read(&path).unwrap()).into_owned()
}

#[test]
fn a_counted_loop_vectorizes_with_its_poll_in_place() {
    let dir = tempfile::tempdir().unwrap();
    let hl = bytecode(&dir);
    let count = |env: &[(&str, &str)]| {
        let ir = ir_dump(&hl, env);
        (
            ir.matches("vector.body").count(),
            ir.matches("fiber_poll_call").count(),
        )
    };
    let (nested, polls) = count(&[]);
    let (flat, flat_polls) = count(&[("ASH_NESTED_STRIP", "0")]);
    assert!(
        nested > flat,
        "nested strips vectorized {nested} loops, single polled loops {flat}"
    );
    // The poll is still emitted, as often as before.
    assert!(polls >= flat_polls, "{polls} polls against {flat_polls}");
}
