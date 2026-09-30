mod common;

use common::{ash_cli_bin, haxe_available, tests_dir};
use std::path::PathBuf;
use std::process::Command;

fn fixture() -> PathBuf {
    let checked_in = tests_dir().join("test_wide_hybrid.hl");
    if !haxe_available() {
        assert!(
            checked_in.exists(),
            "missing fixture: {}",
            checked_in.display()
        );
        return checked_in;
    }
    let dir = std::env::temp_dir().join(format!("ash-wide-hybrid-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("TestWideHybrid.hl");
    let out = Command::new("haxe")
        .arg("-cp")
        .arg(tests_dir())
        .args(["-main", "TestWideHybrid", "-hl"])
        .arg(&path)
        .output()
        .unwrap();
    assert!(
        out.status.success(),
        "haxe: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    path
}

#[test]
fn default_hybrid_promotes_wide_calls() {
    let out = Command::new(ash_cli_bin())
        .args([
            "--mode",
            "hybrid",
            "--jit-tier",
            "cranelift",
            "--jit-threshold",
            "1",
            "--jit-log",
        ])
        .arg(fixture())
        .output()
        .unwrap();
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(out.status.success(), "{}\n{stderr}", out.status);
    assert!(
        stdout.lines().any(|line| line == "wide-hybrid bad=0"),
        "{stdout}\n{stderr}"
    );
    assert!(
        stderr
            .lines()
            .any(|line| line.contains("uniform entry") && line.contains("nargs=16")),
        "wide function stayed interpreted or has no callable entry:\n{stderr}"
    );
}
