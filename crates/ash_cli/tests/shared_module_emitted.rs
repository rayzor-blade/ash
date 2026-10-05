//! A promotion that arrives after the shared LLVM module has been emitted
//! still gets code.
//!
//! MCJIT generates a module's code once, at the first address asked of it.
//! A body lowered into the shared module after that never becomes code, so
//! the promotion must take a module of its own. `ASH_PROMOTE_MODULE=0` sends
//! every promotion to the shared path and `--jit-tier llvm` makes every
//! promotion an LLVM one; the fixture's phases make functions hot one after
//! another, so all but the first arrive after emission.

mod common;

use common::{ash_cli_bin, tests_dir};
use std::process::Command;

const FIXTURE: &str = "test_promote_phases.hl";

fn run(args: &[&str], env: &[(&str, &str)]) -> (String, String, Option<i32>) {
    let path = tests_dir().join(FIXTURE);
    assert!(path.exists(), "fixture not built: {}", path.display());
    let mut cmd = Command::new(ash_cli_bin());
    cmd.args(args).arg(&path);
    for (k, v) in env {
        cmd.env(k, v);
    }
    let out = cmd.output().expect("failed to run ash");
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.code(),
    )
}

#[test]
fn promotions_after_the_shared_module_is_emitted_get_code() {
    let (expected, _, code) = run(&["--mode", "interp"], &[]);
    assert_eq!(code, Some(0), "interpreter run did not exit cleanly");

    let (stdout, stderr, code) = run(
        &["--jit-log", "--jit-tier", "llvm"],
        &[("ASH_PROMOTE_MODULE", "0"), ("ASH_TIER_LOG", "1")],
    );
    assert_eq!(code, Some(0), "run did not exit cleanly:\n{stderr}");
    let answer = |s: &str| s.lines().find(|l| l.parse::<i64>().is_ok()).map(str::to_owned);
    assert_eq!(answer(&stdout), answer(&expected), "answer changed:\n{stderr}");
    assert!(
        !stderr.contains("not found in ExecutionEngine"),
        "a promotion was lowered into the already-emitted shared module:\n{stderr}"
    );
    let installs = stderr
        .lines()
        .filter(|l| l.starts_with("[tier] install") && l.contains("tier=llvm"))
        .count();
    assert!(
        installs >= 2,
        "only {installs} LLVM install(s); later phases never reached code:\n{stderr}"
    );
}
