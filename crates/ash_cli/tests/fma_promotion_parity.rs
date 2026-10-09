mod common;

use common::{ash_cli_bin, haxe_available, render_output, run_with_timeout, tests_dir};
use std::process::Command;
use std::time::Duration;

/// A loop-free function's multiply-add rounds as one fused operation in
/// every engine, before the function is compiled and after, and as two
/// roundings everywhere under `--no-fma`.
#[test]
fn loop_free_multiply_add_rounds_the_same_before_and_after_compilation() {
    let dir = tempfile::tempdir().unwrap();
    let fixture = dir.path().join("fma_promotion_parity.hl");
    if haxe_available() {
        let compile = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", "TestFmaPromotionParity", "-hl"])
            .arg(&fixture)
            .output()
            .unwrap();
        assert!(compile.status.success(), "{}", render_output(&compile));
    } else {
        std::fs::copy(tests_dir().join("test_fma_promotion_parity.hl"), &fixture)
            .expect("committed fma parity bytecode fixture");
    }

    let fused = "before=5.55111512312578e-17 after=5.55111512312578e-17 same=true";
    for (args, want) in [
        (vec!["--mode", "interp"], fused),
        (vec!["--mode", "jit"], fused),
        (vec!["--mode", "hybrid"], fused),
        (
            vec!["--mode", "hybrid", "--no-fma"],
            "before=0 after=0 same=true",
        ),
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
        assert!(stdout.contains(want), "{args:?}: {stdout}");
    }
}
