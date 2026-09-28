//! A ref an interpreted caller makes to one of its registers must read and
//! write correctly in a compiled callee.
mod common;

use common::{ash_cli_bin, haxe_available, tests_dir};
use std::path::PathBuf;
use std::process::Command;

fn fixture() -> PathBuf {
    if !haxe_available() {
        let path = tests_dir().join("test_ref_args.hl");
        assert!(
            path.exists(),
            "missing compiled fixture: {}",
            path.display()
        );
        return path;
    }
    let dir = std::env::temp_dir().join(format!("ash-ref-args-tests-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("TestRefArgs.hl");
    let out = Command::new("haxe")
        .arg("-cp")
        .arg(tests_dir())
        .args(["-main", "TestRefArgs", "-hl"])
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

/// `drive` takes seven arguments, so `--jit-max-args 6` keeps it on the
/// interpreter while its callees promote at the first call. `ASH_AIR=0` keeps
/// the interpreted caller's own lowering out of the result: this test is about
/// the call boundary.
#[test]
fn register_refs_cross_into_compiled_callees() {
    let hl = fixture();
    for tier in ["cranelift", "llvm"] {
        let out = Command::new(ash_cli_bin())
            .args([
                "--mode",
                "hybrid",
                "--jit-tier",
                tier,
                "--jit-threshold",
                "1",
                "--jit-max-args",
                "6",
            ])
            .arg(&hl)
            .env("ASH_AIR", "0")
            .output()
            .unwrap();
        let stdout = String::from_utf8_lossy(&out.stdout);
        let stderr = String::from_utf8_lossy(&out.stderr);
        assert!(out.status.success(), "{tier}: {}\n{stderr}", out.status);
        assert!(
            stdout.lines().any(|l| l == "ref-args bad=0"),
            "{tier}:\n{stdout}\n{stderr}"
        );
    }
}
