mod common;

use common::{ash_cli_bin, haxe_available, render_output, run_with_timeout, tests_dir};
use std::process::Command;
use std::time::Duration;

#[test]
fn f32_registers_round_the_same_across_tiers() {
    let dir = tempfile::tempdir().unwrap();
    let fixture = dir.path().join("f32_tier_parity.hl");
    if haxe_available() {
        let compile = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", "TestF32TierParity", "-hl"])
            .arg(&fixture)
            .output()
            .unwrap();
        assert!(compile.status.success(), "{}", render_output(&compile));
    } else {
        std::fs::copy(tests_dir().join("test_f32_tier_parity.hl"), &fixture)
            .expect("committed f32 parity bytecode fixture");
    }

    for args in [
        vec!["--mode", "interp"],
        vec!["--mode", "jit"],
        vec!["--mode", "hybrid", "--jit-threshold", "1"],
        vec!["--mode", "hybrid", "--jit-threshold", "1", "--no-fma"],
    ] {
        let mut command = Command::new(ash_cli_bin());
        command.args(&args).arg(&fixture).env("ASH_WORKERS", "0");
        let result = run_with_timeout(command, Duration::from_secs(60));
        assert!(!result.timed_out, "{args:?} timed out");
        assert!(
            result.output.status.success(),
            "{args:?}: {}",
            render_output(&result.output)
        );
        let stdout = String::from_utf8_lossy(&result.output.stdout);
        assert!(stdout.contains("f32=1.513353109"), "{args:?}: {stdout}");
    }
}
