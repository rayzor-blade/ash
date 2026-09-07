//! An abandoned file handle gets its descriptor back.
//!
//! A `MEM_KIND_FINALIZER` block carries a callback in word zero that the
//! collector calls once nothing can reach the block. ash allocated such
//! blocks correctly for a long time but never called the callback, so a
//! `FileInput` dropped without `close()` held its descriptor until the
//! process exited.
//!
//! The check is a low `RLIMIT_NOFILE` and more opens than it allows. Nothing
//! in Haxe's `sys.io` closes them, so the run either finalizes or dies with
//! `SysError(Can't open ...)`.

mod common;

use common::{ash_cli_bin, haxe_available, tests_dir};
use std::process::Command;

/// Well above the limit below, so the run cannot pass by fitting inside it.
const EXPECTED: &str = "opened 4000";
const FD_LIMIT: u32 = 256;

#[cfg(unix)]
fn run_under_fd_limit(mode: &[&str]) -> (String, String) {
    let hl = tests_dir().join("test_finalizers.hl");
    assert!(hl.exists(), "fixture not built: {}", hl.display());
    let probe = std::env::temp_dir().join(format!("ash_finalizers_{}.txt", std::process::id()));

    // `ulimit` rather than a `pre_exec` setrlimit, so the limit is visible in
    // the command if this ever has to be reproduced by hand.
    let script = format!(
        "ulimit -n {FD_LIMIT}; exec '{}' {} '{}' '{}'",
        ash_cli_bin().display(),
        mode.join(" "),
        hl.display(),
        probe.display(),
    );
    let out = Command::new("sh")
        .arg("-c")
        .arg(&script)
        .output()
        .expect("failed to run ash");
    let _ = std::fs::remove_file(&probe);
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
    )
}

#[cfg(unix)]
fn assert_finalizes(mode: &[&str], label: &str) {
    if !haxe_available() {
        eprintln!("skipping {label}: haxe not on PATH");
        return;
    }
    let (stdout, stderr) = run_under_fd_limit(mode);
    assert!(
        stdout.contains(EXPECTED),
        "{label}: the collector did not close abandoned file handles.\n\
         Under a {FD_LIMIT}-descriptor limit the loop can only finish if it \
         did.\n--- stdout ---\n{stdout}\n--- stderr ---\n{stderr}"
    );
}

#[cfg(unix)]
#[test]
fn abandoned_file_handles_are_closed_by_the_collector_interp() {
    assert_finalizes(&["--mode", "interp"], "interp");
}

#[cfg(unix)]
#[test]
fn abandoned_file_handles_are_closed_by_the_collector_hybrid() {
    assert_finalizes(&[], "hybrid");
}
