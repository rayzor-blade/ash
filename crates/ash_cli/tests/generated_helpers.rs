mod common;

use common::{ash_cli_bin, render_output, run_with_timeout, tests_dir};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Duration;

fn input() -> (PathBuf, usize, usize) {
    let input = tests_dir().join("test_generated_helper.hl");
    ash_core::native_lib::choose_std_linkage(&input);
    ash_core::native_lib::init_std_library().unwrap();
    let bc = ash_core::bytecode::BytecodeDecoder::decode(&input).unwrap();
    let source = bc
        .bound_static("TestGeneratedHelper", "sum")
        .expect("fixture recursive function");
    (input, source, bc.functions.len() + bc.natives.len())
}

fn run(mut cmd: Command) -> String {
    // Keep the recursive dispatch boundary visible in every tier.
    cmd.env("ASH_AIR_NO_INLINE", "1");
    let out = run_with_timeout(cmd, Duration::from_secs(90));
    assert!(
        !out.timed_out && out.output.status.success(),
        "{}",
        render_output(&out.output)
    );
    render_output(&out.output)
}

fn check_output(text: &str) {
    assert!(
        text.lines().any(|l| l == "generated-check=1313310000"),
        "{text}"
    );
}

#[test]
fn registered_recursion_runs_on_interpreter_and_native_tiers() {
    let (input, source, helper) = input();
    let modes = [
        vec!["--mode", "interp"],
        vec!["--mode", "jit", "--jit-tier", "cranelift"],
        #[cfg(feature = "llvm")]
        vec!["--mode", "jit", "--jit-tier", "llvm"],
    ];
    for mode in modes {
        let mut cmd = Command::new(ash_cli_bin());
        cmd.args(&mode)
            .args(["--dev-generated-helper", &source.to_string(), "--jit-log"])
            .arg(&input);
        let text = run(cmd);
        check_output(&text);
        if mode.contains(&"jit") {
            assert!(
                text.lines()
                    .any(|l| l.contains(&format!("findex={helper} ")) && l.contains("install")),
                "helper did not compile: {text}"
            );
        }
    }
}

#[test]
fn hybrid_installs_the_same_helper_across_tiers() {
    let (input, source, helper) = input();
    let mut cmd = Command::new(ash_cli_bin());
    cmd.args([
        "--mode",
        "hybrid",
        "--jit-threshold",
        "1",
        "--opt-threshold",
        "1",
        "--jit-min-ops",
        "0",
        "--jit-log",
        "--dev-generated-helper",
        &source.to_string(),
    ])
    .arg(input);
    let text = run(cmd);
    check_output(&text);
    for tier in if cfg!(feature = "llvm") {
        vec!["cranelift", "llvm"]
    } else {
        vec!["cranelift"]
    } {
        assert!(
            text.lines().any(|l| l.contains("install")
                && l.contains(&format!("findex={helper} "))
                && l.contains(&format!("tier={tier}"))),
            "helper not installed in {tier}: {text}"
        );
    }
}

fn emitted(dir: &Path) -> PathBuf {
    let (input, source, helper) = input();
    let output = dir.join("generated.hl");
    let mut cmd = Command::new(ash_cli_bin());
    cmd.args([
        "--dev-generated-helper",
        &source.to_string(),
        "--emit-optimized",
    ])
    .arg(&output)
    .arg(input);
    run(cmd);
    let decoded = ash_core::bytecode::BytecodeDecoder::decode(&output).unwrap();
    assert!(
        decoded
            .functions
            .iter()
            .any(|f| f.findex as usize == helper)
    );
    assert!(ash_core::reachable::analyze(&decoded).is_live(helper as i32));
    output
}

#[test]
fn emitted_helpers_run_without_development_option() {
    let dir = tempfile::tempdir().unwrap();
    let path = emitted(dir.path());
    let mut cmd = Command::new(ash_cli_bin());
    cmd.args(["--mode", "interp"]).arg(path);
    check_output(&run(cmd));
}

#[cfg(feature = "llvm")]
#[test]
fn emitted_helpers_are_reachable_in_native_and_wasm_aot() {
    let dir = tempfile::tempdir().unwrap();
    let path = emitted(dir.path());
    let exe = dir.path().join(if cfg!(windows) {
        "generated.exe"
    } else {
        "generated"
    });
    let mut cmd = Command::new(ash_cli_bin());
    cmd.arg("--build").arg(&exe).arg(&path);
    run(cmd);
    check_output(&run(Command::new(exe)));
    let wasm = dir.path().join("generated.o");
    let mut cmd = Command::new(ash_cli_bin());
    cmd.args(["--target", "wasm32-wasip1", "--emit-aot"])
        .arg(&wasm)
        .arg(path);
    run(cmd);
    let report = ash_wasm_runtime::validate::inspect(&std::fs::read(wasm).unwrap());
    assert!(report.invalid.is_none(), "{:?}", report.invalid);
    assert!(report.relocatable);
}

#[test]
fn unsupported_preparation_modes_fail_explicitly() {
    let (input, source, _) = input();
    for flag in ["--hot-reload", "--build", "--emit-aot"] {
        let mut cmd = Command::new(ash_cli_bin());
        cmd.args(["--dev-generated-helper", &source.to_string(), flag]);
        if flag != "--hot-reload" {
            cmd.arg("unused-output");
        }
        cmd.arg(&input);
        let result = run_with_timeout(cmd, Duration::from_secs(10));
        assert!(
            !result.timed_out && !result.output.status.success(),
            "{}",
            render_output(&result.output)
        );
        assert!(String::from_utf8_lossy(&result.output.stderr).contains("cannot be used with"));
    }
}
