//! The direct wasm backend inside the real build: numeric functions emitted by
//! `ash_wasm_emit`, everything else by LLVM, linked by `ash_wasm_link` with the
//! runtime. The program must print what the interpreter prints, with and
//! without the fiber transform.
mod common;

use common::{ash_cli_bin, haxe_available, render_output, run_with_timeout, tests_dir};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Duration;

const TARGET: &str = "wasm32-wasip1-threads";

fn runtime_object() -> Option<PathBuf> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../target");
    ["release", "debug"]
        .iter()
        .map(|p| root.join(p).join(TARGET).join("ash_runtime.o"))
        .find(|p| p.is_file())
}

fn lines(stdout: &[u8]) -> Vec<String> {
    String::from_utf8_lossy(stdout)
        .lines()
        .filter(|l| l.contains('=') && !l.starts_with('['))
        .map(str::to_owned)
        .collect()
}

#[test]
fn mixed_direct_and_llvm_wasm_matches_the_interpreter() {
    if runtime_object().is_none() {
        eprintln!("skipped: no wasm runtime object for {TARGET}");
        return;
    }
    let dir = tempfile::tempdir().unwrap();
    let hl = dir.path().join("numeric.hl");
    if haxe_available() {
        let out = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", "TestWasmNumeric", "-hl"])
            .arg(&hl)
            .output()
            .unwrap();
        assert!(out.status.success(), "{}", String::from_utf8_lossy(&out.stderr));
    } else {
        std::fs::copy(tests_dir().join("test_wasm_numeric.hl"), &hl).unwrap();
    }

    let mut interp = Command::new(ash_cli_bin());
    interp.args(["--mode", "interp"]).arg(&hl);
    let reference = run_with_timeout(interp, Duration::from_secs(60));
    assert!(!reference.timed_out && reference.output.status.success());
    let expected = lines(&reference.output.stdout);
    assert!(expected.len() > 30, "the reference printed {} lines", expected.len());

    for fibers in [false, true] {
        let wasm = dir.path().join(format!("numeric_{fibers}.wasm"));
        let mut build = Command::new(ash_cli_bin());
        build
            .env("ASH_WASM_DIRECT", "log")
            .args(["--quiet", "--target", TARGET, "--build"])
            .arg(&wasm)
            .arg(&hl);
        if fibers {
            build.env("ASH_WASM_FIBERS", "1");
        }
        let built = run_with_timeout(build, Duration::from_secs(600));
        assert!(
            !built.timed_out && built.output.status.success(),
            "fibers={fibers}: {}",
            render_output(&built.output)
        );
        // It must have taken functions, or this tests nothing.
        let log = String::from_utf8_lossy(&built.output.stderr).into_owned();
        let direct = log
            .lines()
            .find(|l| l.starts_with("[aot] direct: ["))
            .unwrap_or_else(|| panic!("no direct-backend log line: {log}"));
        assert!(
            direct.matches(',').count() >= 5,
            "fibers={fibers}: too few functions went direct: {direct}"
        );

        let mut run = Command::new(ash_cli_bin());
        run.arg("run").arg(&wasm);
        let ran = run_with_timeout(run, Duration::from_secs(120));
        assert!(
            !ran.timed_out && ran.output.status.success(),
            "fibers={fibers}: {}",
            render_output(&ran.output)
        );
        assert_eq!(
            lines(&ran.output.stdout),
            expected,
            "fibers={fibers}: the mixed module disagrees with the interpreter"
        );
    }
}
