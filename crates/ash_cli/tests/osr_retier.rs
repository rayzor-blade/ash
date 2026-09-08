//! A hot loop must survive being handed to another tier mid-flight.
//!
//! Tier-1 code polls a re-tier slot at its loop headers, and when a tier-2 OSR
//! entry is published the running loop jumps to it, spilling a register image
//! on the way. If that image is not the one the entry reads, the loop resumes
//! holding values that are not its own: it restarts, or exits early, and every
//! run differs. Nothing crashes.
//!
//! The fixture's counters are all loop-carried, so any of that shows up as a
//! wrong total. Compared against the interpreter rather than a fixed number,
//! so the test says "the tiers disagree" rather than restating the loop bound.

mod common;

use common::{ash_cli_bin, haxe_available, tests_dir};
use std::process::Command;

fn run(mode: &[&str], extra: &[(&str, &str)]) -> String {
    let hl = tests_dir().join("test_osr_retier.hl");
    assert!(hl.exists(), "fixture not built: {}", hl.display());
    let mut cmd = Command::new(ash_cli_bin());
    cmd.args(mode).arg(&hl);
    for (k, v) in extra {
        cmd.env(k, v);
    }
    let out = cmd.output().expect("failed to run ash");
    String::from_utf8_lossy(&out.stdout)
        .lines()
        .find(|l| l.starts_with("iters="))
        .unwrap_or("<no result line>")
        .to_string()
}

#[test]
fn a_hot_loop_keeps_its_counters_across_a_tier_handoff() {
    if !haxe_available() {
        eprintln!("skipping: haxe not on PATH");
        return;
    }
    let interp = run(&["--mode", "interp"], &[]);

    // Repeated because the failure depends on when the tier-2 compile lands:
    // one run in the original defect restarted the loop, another stopped
    // early, and a third could have been correct by luck.
    for attempt in 0..3 {
        let hybrid = run(&["--mode", "hybrid"], &[]);
        assert_eq!(
            hybrid, interp,
            "attempt {attempt}: hybrid disagrees with the interpreter.\n\
             A loop handed to another tier mid-flight resumed with the wrong \
             registers; see ASH_CL_RETIER in crates/ash/src/cranelift/air.rs."
        );
    }
}
