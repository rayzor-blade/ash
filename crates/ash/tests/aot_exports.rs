//! Program members exported to a host's object, called from C.
//!
//! `test/exports/Exports.hx` calls a native the host links, whose C body
//! (`exports_driver.c`) calls back into the program through each kind of
//! export and answers one bit per check. Each test needs a C compiler for
//! its target and Ash's runtime for it, and is skipped, not failed, without
//! them; the wasm one also runs the module with a built `ash`.

#![cfg(feature = "llvm")]

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use ash_core::host_export::{ExportKind, HostExport};
use ash_core::llvm::aot_build::{AotRequest, emit_aot};
use ash_core::native_lib::{HostLink, Word};

const ALL_CHECKS: u32 = 524287;
const UNIT: u64 = 7;

fn export(symbol: &str, class: &str, member: &str, kind: ExportKind, args: usize) -> HostExport {
    HostExport {
        symbol: symbol.to_string(),
        class: class.to_string(),
        member: member.to_string(),
        kind,
        arg_casts: vec![None; args],
        ret_cast: None,
        raise: "exports_test_raise".to_string(),
        unit: UNIT,
        casts_nothrow: false,
    }
}

/// The first of `names` this checkout built, under `target/`.
fn built(names: &[&str]) -> Option<PathBuf> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../target");
    let root = &root;
    ["release", "debug"]
        .iter()
        .flat_map(|p| names.iter().map(move |n| root.join(p).join(n)))
        .find(|p| p.is_file())
}

fn fixture() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("test/exports")
}

/// Compile the driver with `cc`, build the program around it for `target`,
/// and return the built file; `None` when the compiler is missing.
fn build(
    name: &str,
    cc: &mut Command,
    target: Option<&str>,
    runtime: &Path,
    exe_name: &str,
) -> Option<(PathBuf, PathBuf)> {
    let dir = std::env::temp_dir().join(format!("ash-aot-exports-{name}-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let driver = dir.join("exports_driver.o");
    let compiled = cc
        .arg("-c")
        .arg(fixture().join("exports_driver.c"))
        .arg("-o")
        .arg(&driver)
        .status();
    if !compiled.is_ok_and(|s| s.success()) {
        let _ = std::fs::remove_dir_all(&dir);
        return None;
    }
    let links = HashMap::from([(
        ("exports_test".to_string(), "drive".to_string()),
        HostLink {
            symbol: "exports_test_drive".to_string(),
            params: Vec::new(),
            ret: Some(Word::I32),
            arg_casts: Vec::new(),
            ret_cast: None,
            after: None,
            init: None,
            library: None,
        },
    )]);
    let exports = vec![
        export("exports_test_add", "Counter", "add", ExportKind::Static, 2),
        export(
            "exports_test_new_counter",
            "Counter",
            "new",
            ExportKind::Constructor,
            1,
        ),
        export(
            "exports_test_new_twice",
            "Twice",
            "new",
            ExportKind::Constructor,
            1,
        ),
        export(
            "exports_test_bump",
            "Counter",
            "bump",
            ExportKind::Method,
            2,
        ),
        export(
            "exports_test_get_count",
            "Counter",
            "count",
            ExportKind::Getter,
            1,
        ),
        export(
            "exports_test_set_count",
            "Counter",
            "count",
            ExportKind::Setter,
            2,
        ),
        export(
            "exports_test_get_made",
            "Counter",
            "made",
            ExportKind::Getter,
            0,
        ),
        export(
            "exports_test_fail",
            "Counter",
            "fail",
            ExportKind::Static,
            1,
        ),
        export(
            "exports_test_half",
            "Counter",
            "half",
            ExportKind::Static,
            1,
        ),
        export(
            "exports_test_grow",
            "Counter",
            "grow",
            ExportKind::Method,
            2,
        ),
        export(
            "exports_test_adder",
            "Counter",
            "adder",
            ExportKind::Static,
            1,
        ),
        export(
            "exports_test_thrower",
            "Counter",
            "thrower",
            ExportKind::Static,
            0,
        ),
        export(
            "exports_test_grow_of",
            "Counter",
            "growOf",
            ExportKind::Static,
            1,
        ),
        HostExport {
            arg_casts: vec![None, Some("ash:unbox_f64".to_string())],
            ret_cast: Some("ash:box_f64".to_string()),
            ..export(
                "exports_test_call_ff",
                "Float->Float",
                "",
                ExportKind::Call,
                2,
            )
        },
        export(
            "exports_test_call_ii",
            "(Int)->Int",
            "",
            ExportKind::Call,
            2,
        ),
        HostExport {
            arg_casts: vec![Some("ash:unbox_f64".to_string())],
            ret_cast: Some("ash:box_f64".to_string()),
            ..export(
                "exports_test_half_boxed",
                "Counter",
                "half",
                ExportKind::Static,
                1,
            )
        },
    ];
    let exe = dir.join(exe_name);
    emit_aot(AotRequest {
        file: &fixture().join("test_exports.hl"),
        out: &dir.join(format!("{exe_name}.o")),
        exe: Some(&exe),
        runtime: Some(runtime),
        target: target.map(str::to_string),
        pgo: None,
        allow_refused: false,
        abi_version: 1,
        quiet: true,
        links,
        objects: vec![driver],
        wasm_fibers: false,
        exports,
    })
    .unwrap_or_else(|e| panic!("build for {name}: {e:#}"));
    Some((dir, exe))
}

fn assert_all_checks(run: Output) {
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert!(
        run.status.success(),
        "exit {:?}\nstdout: {stdout}\nstderr: {}",
        run.status,
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(stdout.trim(), format!("drive {ALL_CHECKS}"));
}

#[test]
fn a_host_object_calls_program_members_by_symbol() {
    let Some(runtime) = built(&["libash_std.a"]) else {
        eprintln!("skipped: no libash_std.a under target/");
        return;
    };
    let Some((dir, exe)) = build("native", &mut Command::new("cc"), None, &runtime, "exports")
    else {
        eprintln!("skipped: no C compiler");
        return;
    };
    let run = Command::new(&exe).output().expect("run the built program");
    let _ = std::fs::remove_dir_all(&dir);
    assert_all_checks(run);
}

#[test]
fn a_host_object_calls_program_members_by_symbol_on_wasm() {
    let (Some(runtime), Some(ash)) = (
        built(&["wasm32-wasip1/ash_runtime.o"]),
        built(&["ash", "ash.exe"]),
    ) else {
        eprintln!("skipped: no wasm32-wasip1 runtime or ash binary under target/");
        return;
    };
    // A clang that targets wasm: ASH_WASM_CC, else the one on PATH.
    let clang = std::env::var_os("ASH_WASM_CC").unwrap_or_else(|| "clang".into());
    let mut cc = Command::new(clang);
    cc.arg("--target=wasm32-wasip1");
    let Some((dir, module)) = build(
        "wasm",
        &mut cc,
        Some("wasm32-wasip1"),
        &runtime,
        "exports.wasm",
    ) else {
        eprintln!("skipped: no clang that targets wasm32 (set ASH_WASM_CC)");
        return;
    };
    let run = Command::new(ash)
        .arg("run")
        .arg(&module)
        .output()
        .expect("run the module");
    let _ = std::fs::remove_dir_all(&dir);
    assert_all_checks(run);
}
