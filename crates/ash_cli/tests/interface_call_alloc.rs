mod common;

use common::{ash_cli_bin, haxe_available, render_output, run_with_timeout, tests_dir};
use std::process::Command;
use std::time::Duration;

fn amount(json: &str, key: &str) -> u64 {
    let marker = format!("\"{key}\":");
    let value = json
        .split(&marker)
        .nth(1)
        .unwrap_or_else(|| panic!("missing {key} in {json}"))
        .split([',', '}'])
        .next()
        .unwrap();
    value
        .parse::<u64>()
        .unwrap_or_else(|_| panic!("invalid {key} in {json}"))
}

#[test]
fn interface_calls_do_not_box_arguments_in_cranelift() {
    let dir = tempfile::tempdir().unwrap();
    let hl = dir.path().join("interface_call_alloc.hl");
    if haxe_available() {
        let compile = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", "TestInterfaceCallAlloc", "-hl"])
            .arg(&hl)
            .output()
            .unwrap();
        assert!(compile.status.success(), "{}", render_output(&compile));
    } else {
        std::fs::copy(tests_dir().join("test_interface_call_alloc.hl"), &hl)
            .expect("committed interface allocation bytecode fixture");
    }

    for mode in [
        &["--mode", "jit", "--jit-tier", "cranelift"][..],
        &[
            "--mode",
            "hybrid",
            "--jit-tier",
            "cranelift",
            "--jit-threshold",
            "1",
        ][..],
        &["--mode", "jit", "--jit-tier", "llvm"][..],
        &["--mode", "interp"][..],
    ] {
        let mut command = Command::new(ash_cli_bin());
        command.args(mode).arg(&hl).env("ASH_GC_TLAB", "0");
        let result = run_with_timeout(command, Duration::from_secs(30));
        assert!(
            !result.timed_out && result.output.status.success(),
            "{mode:?}: {}",
            render_output(&result.output)
        );
        let stdout = String::from_utf8_lossy(&result.output.stdout);
        let json = stdout
            .lines()
            .find(|line| line.starts_with('{'))
            .unwrap_or_else(|| panic!("{mode:?}: no result in {}", render_output(&result.output)));
        assert_eq!(amount(json, "calls"), 8192, "{mode:?}: {json}");
        assert_eq!(amount(json, "total"), 230912, "{mode:?}: {json}");
        assert_eq!(amount(json, "dynamicTotal"), 7, "{mode:?}: {json}");
        for key in ["direct", "base", "viaInterface", "cachedCall", "switched"] {
            assert_eq!(amount(json, key), 0, "{mode:?}: {json}");
        }
        assert!(amount(json, "readMethod") > 0, "{mode:?}: {json}");
    }
}
