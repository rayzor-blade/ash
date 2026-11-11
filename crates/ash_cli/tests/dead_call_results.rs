//! A call whose result nobody reads does not keep its object alive.
//!
//! The interpreter holds each SSA value in a frame slot of its own, and the
//! collector scans every slot of a live frame. Inlining and unrolling leave a
//! long-lived frame with one result slot per call site, so objects returned
//! by calls whose results are discarded stayed reachable until the frame
//! returned, and a finalizer on any of them never ran.
//!
//! The check is a low `RLIMIT_NOFILE` and more handles opened than it allows
//! if every round's handles were kept: the run finishes only when the
//! collector closes each round's handles before the next one opens.

mod common;

use common::{ash_cli_bin, haxe_available, tests_dir};
use std::process::Command;

const EXPECTED: &str = "rounds 5, held 4";
/// Under the 200 handles the five rounds open between them, well over the
/// 44 that are live at once when each round is collected.
const FD_LIMIT: u32 = 160;

#[cfg(unix)]
fn run_under_fd_limit(mode: &[&str], env: &[(&str, &str)], label: &str) -> (String, String) {
    let hl = tests_dir().join("test_dead_call_results.hl");
    assert!(hl.exists(), "fixture not built: {}", hl.display());
    let probe = std::env::temp_dir().join(format!(
        "ash_dead_call_results_{}_{label}.txt",
        std::process::id()
    ));
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
        .envs(env.iter().copied())
        .output()
        .expect("failed to run ash");
    let _ = std::fs::remove_file(&probe);
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
    )
}

#[cfg(unix)]
fn assert_collected(mode: &[&str], env: &[(&str, &str)], label: &str) {
    if !haxe_available() {
        eprintln!("skipping {label}: haxe not on PATH");
        return;
    }
    let (stdout, stderr) = run_under_fd_limit(mode, env, label);
    assert!(
        stdout.contains(EXPECTED),
        "{label}: handles returned by discarded calls were still reachable.\n\
         Under a {FD_LIMIT}-descriptor limit the rounds can only finish if \
         each round's handles were collected.\n--- stdout ---\n{stdout}\n\
         --- stderr ---\n{stderr}"
    );
}

#[cfg(unix)]
#[test]
fn discarded_call_results_are_collected_interp() {
    assert_collected(&["--mode", "interp"], &[], "interp");
}

#[cfg(unix)]
#[test]
fn discarded_call_results_are_collected_hybrid() {
    assert_collected(&[], &[], "hybrid");
}
