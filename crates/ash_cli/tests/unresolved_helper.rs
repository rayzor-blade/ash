//! A runtime helper that will not resolve refuses the promotion; it does not
//! kill the promoter thread.
//!
//! `declare_native` cannot return an error — 62 call sites take its
//! `FunctionValue` directly — so it used to panic, on `beadie-promoter-N`,
//! over a symbol the program might never reach. A staged `libhl` older than
//! the `ash` beside it is how that happens in practice.
//!
//! `ASH_TEST_UNRESOLVED_NATIVE` forces the condition without staging an old
//! runtime. The program must still finish with the right answer, on whatever
//! tier is left.

mod common;

use common::{ash_cli_bin, tests_dir};
use std::process::Command;

/// A helper the LLVM tier emits for `New`. The fixture has to promote an
/// allocating body to reach it: mandelbrot builds two `Complex` per inner
/// iteration and promotes early, where a short program never gets there.
const HELPER: &str = "hlp_alloc_obj_sized";
const FIXTURE: &str = "test_mandelbrot_small.hl";

fn run(hl: &str, forced: Option<&str>) -> (String, String, Option<i32>) {
    let path = tests_dir().join(hl);
    assert!(path.exists(), "fixture not built: {}", path.display());
    let mut cmd = Command::new(ash_cli_bin());
    cmd.arg("--mode").arg("hybrid").arg(&path);
    if let Some(names) = forced {
        cmd.env("ASH_TEST_UNRESOLVED_NATIVE", names);
    }
    let out = cmd.output().expect("failed to run ash");
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.code(),
    )
}

#[test]
fn an_unresolved_runtime_helper_refuses_the_promotion_instead_of_panicking() {
    let (baseline, _, base_code) = run(FIXTURE, None);
    assert_eq!(base_code, Some(0), "baseline run did not exit cleanly");

    let (stdout, stderr, code) = run(FIXTURE, Some(HELPER));

    assert!(
        !stderr.contains("panicked"),
        "a missing helper still panics a thread:\n{stderr}"
    );
    assert_eq!(code, Some(0), "run did not exit cleanly:\n{stderr}");
    assert_eq!(
        stdout, baseline,
        "answer changed when {HELPER} was made unresolvable"
    );
    assert!(
        stderr.contains(HELPER),
        "the refusal should name the helper it could not resolve:\n{stderr}"
    );
}

#[test]
fn an_unknown_forced_name_leaves_the_run_untouched() {
    // Guards the seam itself: matching is by exact name, so a helper that does
    // not exist must not make every resolution fail.
    let (baseline, _, _) = run(FIXTURE, None);
    let (stdout, stderr, code) = run(FIXTURE, Some("hlp_not_a_real_helper"));
    assert_eq!(code, Some(0), "run did not exit cleanly:\n{stderr}");
    assert_eq!(stdout, baseline);
}
