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

const LADDER: Family = Family {
    dir: "ladder",
    main: "TestHotReloadLadder",
};

const FIELDS: Family = Family {
    dir: "fields",
    main: "TestHotReloadFields",
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

/// Runs `from`, replaces it with `to` once it says `ready`, and returns what
/// ash said on stderr when it refused the reload.
fn refusal(family: Family, from: &str, to: &str) -> String {
    static NEXT: AtomicU64 = AtomicU64::new(0);
    let dir = scratch().join(format!(
        "{}-{from}-{to}-refused-{}",
        family.dir,
        NEXT.fetch_add(1, Ordering::Relaxed)
    ));
    std::fs::create_dir_all(&dir).unwrap();
    let program = dir.join("program.hl");
    std::fs::copy(family.version(from), &program).unwrap();
    let stderr_path = dir.join("stderr");
    let mut child = Command::new(ash_cli_bin())
        .args(["--mode", "hybrid", "--hot-reload", "--quiet"])
        .arg(&program)
        .stdout(Stdio::piped())
        .stderr(Stdio::from(std::fs::File::create(&stderr_path).unwrap()))
        .spawn()
        .unwrap();
    let stdout = child.stdout.take().unwrap();
    let (sender, lines) = mpsc::channel();
    std::thread::spawn(move || {
        for line in BufReader::new(stdout).lines().map_while(Result::ok) {
            if sender.send(line).is_err() {
                break;
            }
        }
    });
    let deadline = Instant::now() + Duration::from_secs(60);
    let stderr = || std::fs::read_to_string(&stderr_path).unwrap_or_default();
    loop {
        match lines.recv_timeout(deadline.saturating_duration_since(Instant::now())) {
            Ok(line) if line == "ready" => break,
            Ok(_) => {}
            Err(_) => {
                child.kill().ok();
                panic!("{from} -> {to}: never said ready\n{}", stderr());
            }
        }
    }
    std::thread::sleep(Duration::from_millis(1100));
    let next = dir.join("next.hl");
    std::fs::copy(family.version(to), &next).unwrap();
    std::fs::rename(&next, &program).unwrap();
    while !stderr().contains("reload refused") {
        if Instant::now() > deadline {
            child.kill().ok();
            panic!("{from} -> {to}: not refused\n{}", stderr());
        }
        std::thread::sleep(Duration::from_millis(50));
    }
    child.kill().ok();
    child.wait().ok();
    stderr()
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
        (METHODS, &["v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8"][..]),
        (LADDER, &["v1", "v2", "v3", "v4"][..]),
        (FIELDS, &["v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9"][..]),
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
            "render virtual 6,field true,name leaf5,describe [leaf5],value 16,bonus 15,sibling other",
            "other other",
            "hot 1000",
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
            "render name leaf5,describe <leaf5>,value 10,sibling other",
            "other other",
            "hot 1000",
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
            "render name leaf5,describe [leaf5],sibling other",
            "other other",
            "hot 1000",
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
            "render name base,describe [base],value 10,sibling other",
            "other other",
            "hot 1000",
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
            "render name leaf5,describe [leaf5],value 10,tag T5,sibling other",
            "other other",
            "hot 1000",
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
            "render name leaf5,describe <leaf5>,value 10,sibling other",
            "other other",
            "hot 1000",
            "next",
            "reloaded",
            "render name base,describe [base],value 10,sibling other",
            "other other",
            "hot 1000",
            "next",
            "reloaded",
            "render name leaf5,describe [leaf5],value 10,tag T5,sibling other",
            "other other",
            "hot 1000",
            "next",
            "reloaded",
            "render name leaf5,describe [leaf5],value 10,sibling other",
            "other other",
            "hot 1000",
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
            "render third third{third}6,name leaf5,describe [leaf5],value 10,sibling other",
            "other other",
            "hot 1000",
            "done",
        ],
        "added",
    );
}

/// A `toString` the class did not have is called when the object is turned
/// into a string.
#[test]
fn a_to_string_method_added() {
    check(
        METHODS,
        "v1",
        "v8",
        &[
            "reloaded",
            "render name leaf5,describe [leaf5],value 10,str Leaf(5),sibling other",
            "other other",
            "hot 1000",
            "done",
        ],
        "reloaded",
    );
}

/// `B` has a virtual method of its own (`h`, which `C` overrides) in slot 1.
/// `A` gains a virtual method, which takes slot 2, above `h`: the slots of the
/// subclasses are not the slots the parent counts, and a lookup by name,
/// `toString` and a call by slot must all still find their methods.
#[test]
fn a_root_gains_a_slot_above_the_slots_of_its_subclasses() {
    check(
        LADDER,
        "v1",
        "v2",
        &[
            "reloaded",
            "render f b.f,h c.h,helper b.helper,byname b.helper,describe B,str B!",
            "oldh c.h",
            "d d.h",
            "done",
        ],
        "reloaded",
    );
}

/// `h` is removed from `B` and `C`, though a frame that began before the
/// reload calls its slot: the class keeps it.
#[test]
fn a_removed_method_whose_slot_an_older_frame_calls() {
    check(
        LADDER,
        "v1",
        "v3",
        &[
            "reloaded",
            "render f b.f,helper b.helper,byname b.helper,describe B,str B!",
            "oldh b.h",
            "d b.h",
            "done",
        ],
        "reloaded",
    );
}

/// The same two edits one after the other: the slot `describe` took above
/// `h` is the running program's by the time `h` is removed.
#[test]
fn a_slot_taken_above_a_subclass_and_the_subclass_method_removed() {
    check_through(
        LADDER,
        "v1",
        &["v2", "v3"],
        &[
            "reloaded",
            "render f b.f,h c.h,helper b.helper,byname b.helper,describe B,str B!",
            "oldh c.h",
            "d d.h",
            "next",
            "reloaded",
            "render f b.f,helper b.helper,byname b.helper,describe B,str B!",
            "oldh b.h",
            "d b.h",
            "done",
        ],
        "reloaded",
    );
}

/// The only change is that `C` stops overriding `h`, which `D` still
/// overrides: no function body changes, only the class's methods. The reload
/// still happens, and `C` has `B`'s.
#[test]
fn a_reload_that_only_removes_an_override() {
    check_through(
        LADDER,
        "v1",
        &["v2", "v4"],
        &[
            "reloaded",
            "render f b.f,h c.h,helper b.helper,byname b.helper,describe B,str B!",
            "oldh c.h",
            "d d.h",
            "next",
            "reloaded",
            "render f b.f,h b.h,helper b.helper,byname b.helper,describe B,str B!",
            "oldh b.h",
            "d d.h",
            "done",
        ],
        "reloaded",
    );
}

/// A reference and a float after the fields `Widget` has. The widget made
/// before the reload has room for them: they read as null and zero, take
/// writes, and compiled code reads them too. A widget made after the reload
/// has them as well.
#[test]
fn fields_added_to_a_class() {
    check(
        FIELDS,
        "v1",
        "v2",
        &[
            "reloaded",
            "render id 1;label w;hits 1;note null,scale 0;after viaReflect,2.5;reflect viaReflect hits/id/label/note/scale",
            "hot 800",
            "many 95700 bad 0",
            "fresh 2,f,0,null,0",
            "done",
        ],
        "reloaded",
    );
}

/// An int declared between two the class has: the bodies of the new program
/// name `hits` by a number the running class does not give it.
#[test]
fn a_field_declared_between_the_others() {
    check(
        FIELDS,
        "v1",
        "v3",
        &[
            "reloaded",
            "render id 1;label w;hits 1;tag 0;after 7",
            "hot 1800",
            "many 95700 bad 0",
            "fresh 2,f,0,0",
            "done",
        ],
        "reloaded",
    );
}

/// A constructor sets the field to a constant: a widget made after the
/// reload has it, and the one made before holds zero.
#[test]
fn a_field_a_constructor_sets_to_a_constant() {
    check(
        FIELDS,
        "v1",
        "v4",
        &[
            "reloaded",
            "render id 1;label w;hits 1;step 0",
            "hot 400",
            "many 47850 bad 0",
            "fresh 2,f,0,3",
            "done",
        ],
        "reloaded",
    );
}

/// Fields over two reloads. The second class is larger than the first, and
/// the widget is still the one made before either.
#[test]
fn fields_added_over_two_reloads() {
    check_through(
        FIELDS,
        "v1",
        &["v2", "v5"],
        &[
            "reloaded",
            "render id 1;label w;hits 1;note null,scale 0;after viaReflect,2.5;reflect viaReflect hits/id/label/note/scale",
            "hot 800",
            "many 95700 bad 0",
            "fresh 2,f,0,null,0",
            "next",
            "reloaded",
            "render id 1;label w;hits 2;note viaReflect,scale 2.5;after viaReflect,2.5;reflect viaReflect extra/hits/id/label/note/scale;extra 10",
            "hot 3000",
            "many 143550 bad 0",
            "fresh 2,f,0,null,0",
            "done",
        ],
        "reloaded",
    );
}

/// The constructor of the new program builds an array for the field, which
/// does not run for the widget made before the reload, so the reload says no.
#[test]
fn a_field_set_to_a_computation_is_refused() {
    let said = refusal(FIELDS, "v1", "v6");
    assert!(
        said.contains("Widget.items is set by a constructor with something that is not a constant"),
        "{said}"
    );
}

/// Nine references are 72 bytes, and a widget made before the reload has 64
/// spare: the reload says so instead of writing past the object.
#[test]
fn a_class_that_outgrows_its_room_is_refused() {
    let said = refusal(FIELDS, "v1", "v7");
    assert!(
        said.contains("an object made before the reload has 96 bytes where the class now needs 104"),
        "{said}"
    );
}

/// Null on one path and an array on the other is not a constant.
#[test]
fn a_field_a_branch_sets_to_null_or_a_computation_is_refused() {
    let said = refusal(FIELDS, "v1", "v8");
    assert!(
        said.contains("Widget.cache is set by a constructor with something that is not a constant"),
        "{said}"
    );
}

/// A field the next version drops stays in the class, unused: the widget
/// keeps its value, the field after it is numbered as before in the bodies
/// that began earlier, and a version that declares it again finds it as it
/// was.
#[test]
fn a_field_dropped_stays_and_comes_back() {
    check_through(
        FIELDS,
        "v1",
        &["v2", "v9", "v2"],
        &[
            "reloaded",
            "render id 1;label w;hits 1;note null,scale 0;after viaReflect,2.5;reflect viaReflect hits/id/label/note/scale",
            "hot 800",
            "many 95700 bad 0",
            "fresh 2,f,0,null,0",
            "next",
            "reloaded",
            "render id 1;label w;hits 2;scale 2.5;after 3.5;reflect viaReflect hits/id/label/note/scale",
            "hot 1200",
            "many 95700 bad 0",
            "fresh 2,f,0,0",
            "next",
            "reloaded",
            "render id 1;label w;hits 3;note viaReflect,scale 3.5;after viaReflect,2.5;reflect viaReflect hits/id/label/note/scale",
            "hot 1200",
            "many 95700 bad 0",
            "fresh 2,f,0,null,0",
            "done",
        ],
        "reloaded",
    );
}
