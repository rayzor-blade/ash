mod common;

use common::{ash_cli_bin, haxe_available, render_output, run_with_timeout, tests_dir};
use std::process::Command;
use std::time::Duration;

fn amount(json: &str, key: &str) -> u64 {
    let marker = format!("\"{key}\":");
    json.split(&marker)
        .nth(1)
        .unwrap_or_else(|| panic!("missing {key} in {json}"))
        .split([',', '}'])
        .next()
        .unwrap()
        .parse()
        .unwrap_or_else(|_| panic!("invalid {key} in {json}"))
}

#[test]
fn inlined_bytes_data_wrapper_does_not_allocate() {
    let dir = tempfile::tempdir().unwrap();
    let hl = dir.path().join("bytes_data_alloc.hl");
    if haxe_available() {
        let compile = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", "TestBytesDataAlloc", "-hl"])
            .arg(&hl)
            .output()
            .unwrap();
        assert!(compile.status.success(), "{}", render_output(&compile));
    } else {
        std::fs::copy(tests_dir().join("test_bytes_data_alloc.hl"), &hl)
            .expect("committed byte data allocation fixture");
    }

    for mode in [
        &["--mode", "jit", "--jit-tier", "cranelift"][..],
        &["--mode", "jit", "--jit-tier", "llvm"][..],
        &["--mode", "hybrid", "--preset", "game"][..],
        &["--mode", "interp"][..],
        &["--mode", "jit", "--jit-tier", "cranelift", "--hot-reload"][..],
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
        for key in ["getData", "cached", "blit"] {
            assert_eq!(amount(json, key), 0, "{mode:?}: {json}");
        }
    }
}
