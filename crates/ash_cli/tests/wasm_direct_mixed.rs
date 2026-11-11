//! The direct wasm backend inside the real build: numeric functions emitted by
//! `ash_wasm_emit`, everything else by LLVM, linked by `ash_wasm_link` with the
//! runtime. The program must print what the interpreter prints, with and
//! without the fiber transform.
mod common;

use common::{ash_cli_bin, haxe_available, render_output, run_with_timeout, static_findex, tests_dir};
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

/// The function indices the build log says went direct.
fn direct_list(log: &str) -> Vec<usize> {
    let line = log.lines().find(|l| l.starts_with("[aot] direct: [")).unwrap_or("");
    line.trim_start_matches("[aot] direct: [")
        .trim_end_matches(']')
        .split(',')
        .filter_map(|n| n.trim().parse().ok())
        .collect()
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
    check("TestWasmNumeric", "test_wasm_numeric.hl", &[]);
}

/// References: equality by value, catchable null checks, fields of every kind,
/// global slots and type constants.
#[test]
fn references_through_the_direct_backend_match_the_interpreter() {
    check("TestWasmDynamic", "test_wasm_dynamic.hl", &[]);
}

/// Fields of structural types: found by pointer, and by name when the object
/// behind the structure lacks them.
#[test]
fn virtual_fields_through_the_direct_backend_match_the_interpreter() {
    check(
        "TestWasmVirtual",
        "test_wasm_virtual.hl",
        &["total", "assign", "link"],
    );
}

/// Calls into the runtime: math and string natives, and the stdlib operations
/// emitted as single instructions.
#[test]
fn natives_through_the_direct_backend_match_the_interpreter() {
    check(
        "TestWasmNative",
        "test_wasm_native.hl",
        &["rounding", "classify", "powers", "curve", "where"],
    );
}

/// Arrays and raw memory at every width, and reference stores into them.
#[test]
fn arrays_through_the_direct_backend_match_the_interpreter() {
    check(
        "TestWasmArray",
        "test_wasm_array.hl",
        &[
            "sumInts",
            "scale",
            "nativeSum",
            "nativeFill",
            "nativeSize",
            "swapItems",
            "bytesRoundTrip",
            "bytesWide",
        ],
    );
}

/// Closures made by direct code: over a static function, a bound method and
/// a lambda, then called by code the direct backend did not emit.
#[test]
fn closures_through_the_direct_backend_match_the_interpreter() {
    check(
        "TestWasmClosure",
        "test_wasm_closure.hl",
        &["twice", "pick", "makeBumper"],
    );
}

/// Boxing and unboxing, nullable numbers, and objects viewed as a structure.
#[test]
fn casts_through_the_direct_backend_match_the_interpreter() {
    check(
        "TestWasmCast",
        "test_wasm_cast.hl",
        &[
            "boxInt",
            "boxFloat",
            "boxBool",
            "boxObject",
            "boxBytes",
            "unboxInt",
            "unboxFloat",
            "unboxBool",
            "orMinusOne",
            "maybe",
            "wide",
            "view",
        ],
    );
}

/// Calls through a pointer: methods of classes, interfaces and structural
/// types, and closures with the signature they were made with and with
/// another.
#[test]
fn dispatch_through_the_direct_backend_matches_the_interpreter() {
    check(
        "TestWasmDispatch",
        "test_wasm_dispatch.hl",
        &[
            "sumAreas",
            "labels",
            "baseTwice",
            "baseScale",
            "viaStructure",
            "applyInt",
            "applyFloat",
            "applyTwo",
            "applyBool",
            "applyVoid",
            "callDyn",
        ],
    );
}

/// Exceptions, enums, references to locals, narrow integers and fields of a
/// Dynamic.
#[test]
fn flow_through_the_direct_backend_matches_the_interpreter() {
    check(
        "TestWasmFlow",
        "test_wasm_flow.hl",
        &[
            "sum",
            "build",
            "describe",
            "thrower",
            "guarded",
            "nested",
            "rethrown",
            "afterwards",
            "inLoop",
            "bump",
            "counter",
            "narrow",
            "dynGet",
            "dynSet",
        ],
    );
}

/// Whole programs through the backend alone, with no LLVM module: the data,
/// the start-up routine and `main` are written beside the bodies.
#[test]
fn whole_programs_without_llvm_match_the_interpreter() {
    check_only("TestWasmNumeric", "test_wasm_numeric.hl");
    check_only("TestWasmDispatch", "test_wasm_dispatch.hl");
    check_only("TestWasmFlow", "test_wasm_flow.hl");
}

/// The `simd` primitives and the vector values they promote to, against a
/// scalar reference: wasm `v128` operations. Built from the committed
/// bytecode, since the fixture needs the library's class path.
#[test]
fn simd_through_the_direct_backend_matches_the_interpreter() {
    check_with("TestSimd", "test_simd.hl", &[], false, false);
    check_with("TestSimd", "test_simd.hl", &[], true, false);
}

/// Build `main_class` and run it. `must_be_direct` names static methods of the class
/// that must have gone through the direct backend.
fn check(main_class: &str, fallback: &str, must_be_direct: &[&str]) {
    check_in(main_class, fallback, must_be_direct, false);
}

/// The same, with the direct backend taking the whole program: no LLVM module
/// is built at all.
fn check_only(main_class: &str, fallback: &str) {
    check_in(main_class, fallback, &[], true);
}

fn check_in(main_class: &str, fallback: &str, must_be_direct: &[&str], only: bool) {
    check_with(main_class, fallback, must_be_direct, only, true);
}

fn check_with(main_class: &str, fallback: &str, must_be_direct: &[&str], only: bool, compile: bool) {
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
            .env("ASH_WASM_DIRECT", if only { "log,only" } else { "log" })
            .args(["--target", TARGET, "--build"])
            .arg(&wasm)
            .arg(&hl);
        if !only {
            build.arg("--quiet");
        }
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
        if only {
            assert!(
                log.contains("functions, no LLVM"),
                "fibers={fibers}: the program did not go through the backend alone: {log}"
            );
        }
        // It must have taken functions, or this tests nothing.
        if !only {
        let direct = log
            .lines()
            .find(|l| l.starts_with("[aot] direct: ["))
            .unwrap_or_else(|| panic!("no direct-backend log line: {log}"));
        assert!(
            direct.matches(',').count() >= 5,
            "fibers={fibers}: too few functions went direct: {direct}"
        );
        }

        ash_core::native_lib::init_std_library().unwrap();
        let bc = ash_core::bytecode::BytecodeDecoder::decode(&hl).unwrap();
        for name in must_be_direct {
            let findex = static_findex(&bc, main_class, name)
                .unwrap_or_else(|| panic!("no static {name} in {main_class}"));
            assert!(
                direct_list(&log).contains(&findex),
                "fibers={fibers}: {name} (f{findex}) was not emitted directly: {log}"
            );
        }

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
