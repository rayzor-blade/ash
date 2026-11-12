//! The direct wasm backend inside the real build: every function emitted by
//! `ash_wasm_emit` and linked by `ash_wasm_link` with the runtime, with no LLVM
//! module. The program must print what the interpreter prints, with and
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
fn numeric_code_matches_the_interpreter() {
    check("TestWasmNumeric", "test_wasm_numeric.hl");
}

/// References: equality by value, catchable null checks, fields of every kind,
/// global slots and type constants.
#[test]
fn references_through_the_direct_backend_match_the_interpreter() {
    check("TestWasmDynamic", "test_wasm_dynamic.hl");
}

/// Fields of structural types: found by pointer, and by name when the object
/// behind the structure lacks them.
#[test]
fn virtual_fields_through_the_direct_backend_match_the_interpreter() {
    check("TestWasmVirtual", "test_wasm_virtual.hl");
}

/// Calls into the runtime: math and string natives, and the stdlib operations
/// emitted as single instructions.
#[test]
fn natives_through_the_direct_backend_match_the_interpreter() {
    check("TestWasmNative", "test_wasm_native.hl");
}

/// Arrays and raw memory at every width, and reference stores into them.
#[test]
fn arrays_through_the_direct_backend_match_the_interpreter() {
    check("TestWasmArray", "test_wasm_array.hl");
}

/// Closures made by direct code: over a static function, a bound method and
/// a lambda, then called by code the direct backend did not emit.
#[test]
fn closures_through_the_direct_backend_match_the_interpreter() {
    check("TestWasmClosure", "test_wasm_closure.hl");
}

/// Boxing and unboxing, nullable numbers, and objects viewed as a structure.
#[test]
fn casts_through_the_direct_backend_match_the_interpreter() {
    check("TestWasmCast", "test_wasm_cast.hl");
}

/// Calls through a pointer: methods of classes, interfaces and structural
/// types, and closures with the signature they were made with and with
/// another.
#[test]
fn dispatch_through_the_direct_backend_matches_the_interpreter() {
    check("TestWasmDispatch", "test_wasm_dispatch.hl");
}

/// Exceptions, enums, references to locals, narrow integers and fields of a
/// Dynamic.
#[test]
fn flow_through_the_direct_backend_matches_the_interpreter() {
    check("TestWasmFlow", "test_wasm_flow.hl");
}

/// A wasm build has no LLVM module, and a value that once asked for one is
/// refused rather than ignored.
#[test]
fn a_wasm_build_is_direct_and_refuses_the_removed_modes() {
    if runtime_object().is_none() {
        eprintln!("skipped: no wasm runtime object for {TARGET}");
        return;
    }
    let dir = tempfile::tempdir().unwrap();
    let hl = dir.path().join("program.hl");
    std::fs::copy(tests_dir().join("test_wasm_flow.hl"), &hl).unwrap();

    let mut interp = Command::new(ash_cli_bin());
    interp.args(["--mode", "interp"]).arg(&hl);
    let reference = run_with_timeout(interp, Duration::from_secs(60));
    assert!(!reference.timed_out && reference.output.status.success());
    let expected = lines(&reference.output.stdout);

    let build = |mode: Option<&str>| {
        let wasm = dir.path().join(format!("program_{}.wasm", mode.unwrap_or("default")));
        let mut build = Command::new(ash_cli_bin());
        build.env_remove("ASH_WASM_DIRECT");
        if let Some(mode) = mode {
            build.env("ASH_WASM_DIRECT", mode);
        }
        build
            .args(["--target", TARGET, "--build"])
            .arg(&wasm)
            .arg(&hl);
        (wasm, run_with_timeout(build, Duration::from_secs(600)))
    };

    let (wasm, built) = build(None);
    let log = String::from_utf8_lossy(&built.output.stderr).into_owned();
    assert!(!built.timed_out && built.output.status.success(), "{log}");
    assert!(log.contains("functions, no LLVM"), "{log}");
    let mut run = Command::new(ash_cli_bin());
    run.arg("run").arg(&wasm);
    let ran = run_with_timeout(run, Duration::from_secs(120));
    assert!(
        !ran.timed_out && ran.output.status.success(),
        "{}",
        render_output(&ran.output)
    );
    assert_eq!(lines(&ran.output.stdout), expected);

    for removed in ["llvm", "off", "0", "1"] {
        let (wasm, built) = build(Some(removed));
        let log = String::from_utf8_lossy(&built.output.stderr).into_owned();
        assert!(!built.timed_out && !built.output.status.success(), "{removed}: {log}");
        assert!(log.contains("always the direct backend"), "{removed}: {log}");
        assert!(!wasm.exists(), "{removed}");
    }
}

/// A function the backend cannot emit fails the build and is named, as it does
/// under LLVM; `--allow-refused` writes a module whose call to it raises.
#[test]
fn a_function_the_backend_cannot_emit_is_an_error_or_a_raising_stub() {
    if runtime_object().is_none() {
        eprintln!("skipped: no wasm runtime object for {TARGET}");
        return;
    }
    let dir = tempfile::tempdir().unwrap();
    let hl = dir.path().join("program.hl");
    std::fs::copy(tests_dir().join("asm_add.hl"), &hl).unwrap();
    let wasm = dir.path().join("program.wasm");

    let mut refused = Command::new(ash_cli_bin());
    refused.env_remove("ASH_WASM_DIRECT");
    refused
        .args(["--target", TARGET, "--build"])
        .arg(&wasm)
        .arg(&hl);
    let built = run_with_timeout(refused, Duration::from_secs(600));
    let log = String::from_utf8_lossy(&built.output.stderr).into_owned();
    assert!(!built.timed_out && !built.output.status.success(), "{log}");
    assert!(log.contains("could not be lowered") && log.contains("findex="), "{log}");
    assert!(!wasm.exists());

    let mut allowed = Command::new(ash_cli_bin());
    allowed.env_remove("ASH_WASM_DIRECT");
    allowed
        .args(["--target", TARGET, "--allow-refused", "--build"])
        .arg(&wasm)
        .arg(&hl);
    let built = run_with_timeout(allowed, Duration::from_secs(600));
    let log = String::from_utf8_lossy(&built.output.stderr).into_owned();
    assert!(!built.timed_out && built.output.status.success(), "{log}");
    assert!(log.contains("no LLVM"), "{log}");

    let mut run = Command::new(ash_cli_bin());
    run.arg("run").arg(&wasm);
    let ran = run_with_timeout(run, Duration::from_secs(120));
    let out = render_output(&ran.output);
    assert!(!ran.timed_out && !ran.output.status.success(), "{out}");
    assert!(out.contains("Refused at compile time"), "{out}");
}

/// A call-site profile names the function a caller's method calls reach. The
/// direct backend calls it behind a test of the slot, so a receiver of another
/// class still goes through the slot: the program prints what it does without
/// the profile.
#[test]
fn a_profiled_method_call_is_guarded_and_matches_the_interpreter() {
    if runtime_object().is_none() {
        eprintln!("skipped: no wasm runtime object for {TARGET}");
        return;
    }
    let dir = tempfile::tempdir().unwrap();
    let hl = dir.path().join("program.hl");
    std::fs::copy(tests_dir().join("test_wasm_dispatch.hl"), &hl).unwrap();
    // `Base.scale` calls `get`, which `Derived` overrides: the profile saw it
    // reach the override, and `main` also calls it on a `Base`.
    let profile = dir.path().join("dispatch.prof");
    std::fs::write(&profile, "m Base.scale Derived.get\n").unwrap();

    let mut interp = Command::new(ash_cli_bin());
    interp.args(["--mode", "interp"]).arg(&hl);
    let reference = run_with_timeout(interp, Duration::from_secs(60));
    assert!(!reference.timed_out && reference.output.status.success());
    let expected = lines(&reference.output.stdout);
    assert!(expected.len() >= 5, "the reference printed {} lines", expected.len());

    for fibers in [false, true] {
        let build = |name: &str, pgo: bool| {
            let wasm = dir.path().join(format!("{name}_{fibers}.wasm"));
            let mut cmd = Command::new(ash_cli_bin());
            if fibers {
                cmd.env("ASH_WASM_FIBERS", "1");
            }
            cmd.args(["--target", TARGET, "--build"]).arg(&wasm);
            if pgo {
                cmd.arg(format!("--pgo={}", profile.display()));
            }
            cmd.arg(&hl);
            let built = run_with_timeout(cmd, Duration::from_secs(600));
            let log = String::from_utf8_lossy(&built.output.stderr).into_owned();
            assert!(!built.timed_out && built.output.status.success(), "{log}");
            assert!(log.contains("functions, no LLVM"), "{log}");
            (wasm, log)
        };
        let (plain, _) = build("plain", false);
        let (guarded, log) = build("guarded", true);
        assert!(log.contains("pgo: 1 of 1 profiled caller(s) matched"), "{log}");
        assert_ne!(
            std::fs::read(&plain).unwrap(),
            std::fs::read(&guarded).unwrap(),
            "fibers={fibers}: the profile changed nothing"
        );

        let mut run = Command::new(ash_cli_bin());
        run.arg("run").arg(&guarded);
        let ran = run_with_timeout(run, Duration::from_secs(120));
        assert!(
            !ran.timed_out && ran.output.status.success(),
            "{}",
            render_output(&ran.output)
        );
        assert_eq!(lines(&ran.output.stdout), expected, "fibers={fibers}");
    }
}

/// The `simd` primitives and the vector values they promote to, against a
/// scalar reference: wasm `v128` operations. Built from the committed
/// bytecode, since the fixture needs the library's class path.
#[test]
fn simd_through_the_direct_backend_matches_the_interpreter() {
    check_with("TestSimd", "test_simd.hl", false);
}

/// Build `main_class` and run it, built with and without the fiber transform.
fn check(main_class: &str, fallback: &str) {
    check_with(main_class, fallback, true);
}

fn check_with(main_class: &str, fallback: &str, compile: bool) {
    if runtime_object().is_none() {
        eprintln!("skipped: no wasm runtime object for {TARGET}");
        return;
    }
    let dir = tempfile::tempdir().unwrap();
    let hl = dir.path().join("program.hl");
    if compile && haxe_available() {
        let out = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", main_class, "-hl"])
            .arg(&hl)
            .output()
            .unwrap();
        assert!(out.status.success(), "{}", String::from_utf8_lossy(&out.stderr));
    } else {
        std::fs::copy(tests_dir().join(fallback), &hl).unwrap();
    }

    let mut interp = Command::new(ash_cli_bin());
    interp.args(["--mode", "interp"]).arg(&hl);
    let reference = run_with_timeout(interp, Duration::from_secs(60));
    assert!(!reference.timed_out && reference.output.status.success());
    let expected = lines(&reference.output.stdout);
    assert!(expected.len() >= 5, "the reference printed {} lines", expected.len());

    for fibers in [false, true] {
        let wasm = dir.path().join(format!("program_{fibers}.wasm"));
        let mut build = Command::new(ash_cli_bin());
        build
            .args(["--target", TARGET, "--build"])
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
        let log = String::from_utf8_lossy(&built.output.stderr).into_owned();
        assert!(
            log.contains("functions, no LLVM"),
            "fibers={fibers}: the program did not go through the direct backend: {log}"
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
            "fibers={fibers}: the module disagrees with the interpreter"
        );
    }
}
