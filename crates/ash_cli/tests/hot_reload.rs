//! A reload that adds, removes, moves or edits closures, adds classes and
//! types, and adds globals, runs end to end.
//!
//! Each case runs `from` under `--hot-reload`, replaces its file with `to`
//! once it says `ready`, and compares what it printed afterwards; a case with
//! several steps replaces it again each time the program says `next`. The
//! closure programs call their closures from the frame that began before the
//! reload, through `Reflect`, and from compiled code, which are the three ways
//! a closure of the new program reaches a caller of the old one.
mod common;

use common::{ash_cli_bin, haxe_available, repo_root};
use std::io::{BufRead, BufReader, Read};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::mpsc::{self, RecvTimeoutError};
use std::sync::Mutex;
use std::time::{Duration, Instant};

fn scratch() -> PathBuf {
    let dir = std::env::temp_dir().join(format!("ash-hot-reload-tests-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

/// A family of programs that differ by version: the directory under
/// `crates/ash/test/hot_reload` that holds them and the class that is their
/// main.
#[derive(Clone, Copy)]
struct Family {
    dir: &'static str,
    main: &'static str,
}

const CLOSURES: Family = Family {
    dir: "closures",
    main: "TestHotReloadClosures",
};

const CLASSES: Family = Family {
    dir: "classes",
    main: "TestHotReloadClasses",
};

const METHODS: Family = Family {
    dir: "methods",
    main: "TestHotReloadMethods",
};

impl Family {
    fn fixtures(self) -> PathBuf {
        repo_root().join("crates/ash/test/hot_reload").join(self.dir)
    }

    /// The compiled program of version `v`: built with haxe when it is there,
    /// else the one committed beside the sources. Built once, whichever test
    /// asks first.
    fn version(self, v: &str) -> PathBuf {
        static BUILT: Mutex<Vec<(String, PathBuf)>> = Mutex::new(Vec::new());
        let key = format!("{}/{v}", self.dir);
        let mut built = BUILT.lock().unwrap_or_else(|e| e.into_inner());
        if let Some((_, path)) = built.iter().find(|(name, _)| *name == key) {
            return path.clone();
        }
        let path = if haxe_available() {
            let path = scratch().join(format!("{}-{v}.hl", self.dir));
            let out = Command::new("haxe")
                .arg("--cwd")
                .arg(self.fixtures().join(v))
                .args(["-main", self.main, "-hl"])
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
            let path = self.fixtures().join(format!("{v}.hl"));
            assert!(path.exists(), "missing compiled fixture: {}", path.display());
            path
        };
        built.push((key, path.clone()));
        path
    }
}

/// Runs `from` and swaps in each of `steps` in turn, the first once the
/// program says `ready` and each later one once it says `next`. Returns the
/// lines the program printed after `ready`, with ash's own stderr. The
/// program is told how many reloads to wait for in `ROUNDS`.
fn reload_through(
    family: Family,
    from: &str,
    steps: &[&str],
    flags: &[&str],
) -> (Vec<String>, String) {
    static NEXT: AtomicU64 = AtomicU64::new(0);
    let route = format!("{from} -> {}", steps.join(" -> "));
    let dir = scratch().join(format!(
        "{}-{from}-{}-{}",
        family.dir,
        steps.join("-"),
        NEXT.fetch_add(1, Ordering::Relaxed)
    ));
    std::fs::create_dir_all(&dir).unwrap();
    let program = dir.join("program.hl");
    std::fs::copy(family.version(from), &program).unwrap();
    let stderr_path = dir.join("stderr");
    let mut child = Command::new(ash_cli_bin())
        .args(["--mode", "hybrid", "--hot-reload", "--quiet"])
        .args(flags)
        .arg(&program)
        .env("ROUNDS", steps.len().to_string())
        .stdout(Stdio::piped())
        .stderr(Stdio::from(std::fs::File::create(&stderr_path).unwrap()))
        .spawn()
        .unwrap();
    let stderr = || std::fs::read_to_string(&stderr_path).unwrap_or_default();
    let (sender, lines) = mpsc::channel();
    let stdout = child.stdout.take().unwrap();
    std::thread::spawn(move || {
        for line in BufReader::new(stdout).lines().map_while(Result::ok) {
            if sender.send(line).is_err() {
                break;
            }
        }
    });
    let deadline = Instant::now() + Duration::from_secs(120);
    // The next line the program prints, or `None` once it has closed its output.
    let read_line = |child: &mut Child| {
        match lines.recv_timeout(deadline.saturating_duration_since(Instant::now())) {
            Ok(line) => Some(line),
            Err(RecvTimeoutError::Disconnected) => None,
            Err(RecvTimeoutError::Timeout) => {
                child.kill().ok();
                panic!("{route} {flags:?}: timed out\n{}", stderr());
            }
        }
    };
    loop {
        match read_line(&mut child) {
            Some(line) if line == "ready" => break,
            Some(_) => {}
            None => panic!("{route} {flags:?}: never said ready\n{}", stderr()),
        }
    }
    let mut printed = Vec::new();
    for step in steps {
        // The reload is found by the file's modification time.
        std::thread::sleep(Duration::from_millis(1100));
        let next = dir.join("next.hl");
        std::fs::copy(family.version(step), &next).unwrap();
        std::fs::rename(&next, &program).unwrap();
        while let Some(line) = read_line(&mut child) {
            let waiting = line == "next";
            printed.push(line);
            if waiting {
                break;
            }
        }
    }
    while child.try_wait().unwrap().is_none() {
        if Instant::now() > deadline {
            child.kill().ok();
            panic!("{route} {flags:?}: timed out\n{}", stderr());
        }
        std::thread::sleep(Duration::from_millis(50));
    }
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

fn check(family: Family, from: &str, to: &str, expected: &[&str], log: &str) {
    check_through(family, from, &[to], expected, log);
}

fn check_through(family: Family, from: &str, steps: &[&str], expected: &[&str], log: &str) {
    for flags in ARRANGEMENTS {
        let (printed, stderr) = reload_through(family, from, steps, flags);
        assert_eq!(
            printed, expected,
            "{from} -> {steps:?} {flags:?}\n--- ash stderr\n{stderr}"
        );
        assert!(
            stderr.contains(log),
            "{from} -> {steps:?} {flags:?}: expected `{log}` in\n{stderr}"
        );
    }
}

/// A closure added between two others: the two keep running, and the new one
/// is called from every kind of caller.
#[test]
fn a_closure_added_between_two_others() {
    check(
        CLOSURES,
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
        CLOSURES,
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
        CLOSURES,
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
        CLOSURES,
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

/// A closure that reads a string literal the running program does not have:
/// the literal gets a global of its own.
#[test]
fn a_closure_with_a_new_literal() {
    check(
        CLOSURES,
        "v1",
        "v6",
        &[
            "reloaded",
            "old 6,10",
            "new 6,10,8",
            "reflect 6,10,8",
            "apply 60,100,80",
            "done",
        ],
        "1 added",
    );
}

/// A closure whose function type no running closure has: the type goes
/// after the running types, and the ones that were after it in the new
/// program's table are found where they were.
#[test]
fn a_closure_of_a_new_function_type() {
    check(
        CLOSURES,
        "v1",
        "v7",
        &[
            "reloaded",
            "old 6,10",
            "new 6,10,7",
            "reflect 6,10,7",
            "apply 60,100,70",
            "done",
        ],
        "2 added",
    );
}

#[test]
fn the_fixtures_are_where_the_test_expects() {
    for (family, versions) in [
        (CLOSURES, &["v1", "v2", "v3", "v4", "v5", "v6", "v7"][..]),
        (CLASSES, &["v1", "v2", "v3"][..]),
        (METHODS, &["v1", "v2", "v3", "v4", "v5", "v6", "v7"][..]),
    ] {
        for v in versions {
            let source = format!("{}.hx", family.main);
            assert!(Path::new(&family.fixtures().join(v).join(source)).exists());
        }
    }
}

/// A class the running program does not have, with a static and a method,
/// is made by the new `render`, and is a class like any other: its static
/// counts and an object of it is a `View`. `main`, which began before the
/// reload, also calls its method through a structural type.
#[test]
fn a_class_added() {
    check(
        CLASSES,
        "v1",
        "v2",
        &[
            "reloaded",
            "render child 5,extra x #1,total 1",
            "again child 5,extra x #2,total 2",
            "source e:x",
            "done",
        ],
        "added",
    );
}

/// A class whose statics are computed when the program starts: the new
/// program's setup of it runs once, after the reload.
#[test]
fn a_class_with_statics_to_compute() {
    check(
        CLASSES,
        "v1",
        "v3",
        &[
            "reloaded",
            "render child 5,t:3abc",
            "again child 5,t:3abc",
            "source t:3",
            "done",
        ],
        "added",
    );
}

/// Reloads one after another. A class that one program drops and a later one
/// has again is the class that was running, so its statics keep their values.
#[test]
fn a_class_dropped_and_added_again() {
    check_through(
        CLASSES,
        "v1",
        &["v2", "v3", "v2"],
        &[
            "reloaded",
            "render child 5,extra x #1,total 1",
            "again child 5,extra x #2,total 2",
            "source e:x",
            "next",
            "reloaded",
            "render child 5,t:3abc",
            "again child 5,t:3abc",
            "source t:3",
            "next",
            "reloaded",
            "render child 5,extra x #4,total 4",
            "again child 5,extra x #5,total 5",
            "source e:x",
            "done",
        ],
        "added",
    );
}

/// A helper the leaf's own `value` calls and the program shows: a method
/// nothing overrides has no slot, only a function. It is reached through a
/// structural type and by name too, which look a method up in the class.
#[test]
fn a_helper_method_added() {
    check(
        METHODS,
        "v1",
        "v2",
        &[
            "reloaded",
            "render virtual 6,field true,name leaf5,describe [leaf5],value 16,bonus 15",
            "other other",
            "done",
        ],
        "added",
    );
}

/// `describe` had no override, so it was no virtual method; the leaf's
/// override gives it a slot, and `base.describe()` reaches the override.
#[test]
fn an_override_added_to_a_method_nothing_overrode() {
    check(
        METHODS,
        "v1",
        "v3",
        &[
            "reloaded",
            "render name leaf5,describe <leaf5>,value 10",
            "other other",
            "done",
        ],
        "1 added",
    );
}

#[test]
fn a_method_removed() {
    check(
        METHODS,
        "v1",
        "v4",
        &[
            "reloaded",
            "render name leaf5,describe [leaf5]",
            "other other",
            "done",
        ],
        "reloaded",
    );
}

/// The leaf drops its `name`: it has the base class's, in its vtable too.
#[test]
fn an_override_removed() {
    check(
        METHODS,
        "v1",
        "v5",
        &[
            "reloaded",
            "render name base,describe [base],value 10",
            "other other",
            "done",
        ],
        "reloaded",
    );
}

/// The compiler numbers `tag` before `name`, which moves `name` to slot 1.
/// `main`, which began before the reload, still calls `other.name()` by
/// slot 0.
#[test]
fn a_virtual_method_declared_before_the_others() {
    check(
        METHODS,
        "v1",
        "v6",
        &[
            "reloaded",
            "render name leaf5,describe [leaf5],value 10,tag T5",
            "other other",
            "done",
        ],
        "added",
    );
}

/// Methods come and go over several reloads, and a method that was removed
/// comes back.
#[test]
fn methods_added_removed_and_added_again() {
    check_through(
        METHODS,
        "v1",
        &["v3", "v5", "v6", "v1"],
        &[
            "reloaded",
            "render name leaf5,describe <leaf5>,value 10",
            "other other",
            "next",
            "reloaded",
            "render name base,describe [base],value 10",
            "other other",
            "next",
            "reloaded",
            "render name leaf5,describe [leaf5],value 10,tag T5",
            "other other",
            "next",
            "reloaded",
            "render name leaf5,describe [leaf5],value 10",
            "other other",
            "done",
        ],
        "reloaded",
    );
}

/// A class the program did not have, below one it did: its overrides share the
/// slots of the methods they override, and `describe`, which nothing overrode,
/// gains a slot from its override.
#[test]
fn a_class_added_below_a_class_with_methods() {
    check(
        METHODS,
        "v1",
        "v7",
        &[
            "reloaded",
            "render third third{third}6,name leaf5,describe [leaf5],value 10",
            "other other",
            "done",
        ],
        "added",
    );
}
