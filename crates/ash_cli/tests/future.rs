//! Haxe-created Future values run through the interpreter and Cranelift JIT.
mod common;

use common::{ash_cli_bin, haxe_available, repo_root, tests_dir};
use std::process::Command;

#[test]
fn haxe_future_surface() {
    let dir = tempfile::tempdir().unwrap();
    let file = dir.path().join("future.hl");
    if haxe_available() {
        let output = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .arg("-cp")
            .arg(repo_root().join("haxelib/ash-future"))
            .args(["-main", "TestAshFuture", "-hl"])
            .arg(&file)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "haxe: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    } else {
        std::fs::copy(tests_dir().join("test_ash_future.hl"), &file)
            .expect("committed Future bytecode fixture");
    }
    for args in [
        vec!["--mode", "interp"],
        vec!["--mode", "jit", "--jit-tier", "cranelift"],
    ] {
        let output = Command::new(ash_cli_bin())
            .args(&args)
            .arg(&file)
            .output()
            .unwrap();
        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(output.status.success(), "{args:?}: {stdout}\n{stderr}");
        assert!(
            stdout.lines().any(|line| line == "future ok"),
            "{args:?}: {stdout}\n{stderr}"
        );
    }
}
