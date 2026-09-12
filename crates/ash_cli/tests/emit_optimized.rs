//! `--emit-optimized` writes an ordinary `.hl`: a widened loop is
//! scalarized back to opcodes, and the constants the serializer minted for
//! that have to land in the emitted pool. Run the result with AIR off, so
//! the emitted opcodes are what executes, and compare with the source.

mod common;

use std::process::Command;
use std::time::Duration;

use common::{ash_cli_bin, run_with_timeout, tests_dir};

fn run(args: &[&str], air: &str) -> String {
    let mut cmd = Command::new(ash_cli_bin());
    cmd.args(args).env("ASH_AIR", air);
    let r = run_with_timeout(cmd, Duration::from_secs(300));
    assert!(!r.timed_out, "{args:?} hung");
    assert!(
        r.output.status.success(),
        "{args:?} failed:\n{}{}",
        String::from_utf8_lossy(&r.output.stdout),
        String::from_utf8_lossy(&r.output.stderr)
    );
    String::from_utf8_lossy(&r.output.stdout)
        .lines()
        .filter(|l| !l.contains("returned: Void"))
        .collect::<Vec<_>>()
        .join("\n")
}

#[test]
fn emitted_widened_program_runs_with_air_off() {
    let source = tests_dir().join("test_vec_array_addr.hl");
    let source = source.to_string_lossy().into_owned();
    let out = std::env::temp_dir().join("ash-emit-optimized-vec.hl");
    let out = out.to_string_lossy().into_owned();
    let want = run(&["--mode", "interp", &source], "off");
    run(&["--emit-optimized", &out, &source], "v2");
    let got = run(&["--mode", "interp", &out], "off");
    assert_eq!(got, want);
}
