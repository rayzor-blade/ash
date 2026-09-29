//! A C library uses the public Future ABI and hands the carrier to Haxe.
//! The same driver is linked beside native and wasm programs.
#![cfg(feature = "llvm")]

use ash_core::llvm::aot_build::{AotRequest, emit_aot};
use ash_core::native_lib::{HostLink, Word};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::Command;

fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../..")
}

fn runtime(name: &str) -> Option<PathBuf> {
    ["debug", "release"]
        .iter()
        .map(|profile| root().join("target").join(profile).join(name))
        .find(|path| path.is_file())
}

fn links(side_module: bool) -> HashMap<(String, String), HostLink> {
    [
        ("create", vec![], Word::Ptr),
        ("resolve", vec![Word::Ptr, Word::Ptr], Word::Bool),
        ("reject", vec![Word::Ptr, Word::Ptr], Word::Bool),
        ("create_abandoned", vec![], Word::Bool),
        ("finish_abandoned", vec![], Word::Bool),
    ]
    .into_iter()
    .map(|(name, params, ret)| {
        let arg_casts = vec![None; params.len()];
        (
            ("future_test".into(), name.into()),
            HostLink {
                symbol: format!("future_test_{name}"),
                params,
                ret: Some(ret),
                arg_casts,
                ret_cast: None,
                after: None,
                after_flag: None,
                init: None,
                library: side_module.then(|| "future_test".into()),
            },
        )
    })
    .collect()
}

fn fixture(dir: &Path) -> PathBuf {
    let file = dir.join("future.hl");
    if Command::new("haxe").arg("-version").output().is_ok() {
        let output = Command::new("haxe")
            .arg("-cp")
            .arg(root().join("crates/ash/test/tests"))
            .arg("-cp")
            .arg(root().join("haxelib/ash-future"))
            .args(["-main", "TestAshFutureNative", "-hl"])
            .arg(&file)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "haxe: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    } else {
        std::fs::copy(
            root().join("crates/ash/test/tests/test_ash_future_native.hl"),
            &file,
        )
        .expect("committed Future bytecode fixture");
    }
    file
}

fn run_case(dir: &Path, file: &Path, target: Option<&str>, runtime_path: &Path, wasm_fibers: bool) {
    let name = match (target.is_some(), wasm_fibers) {
        (false, _) => "native",
        (true, false) => "wasm_plain",
        (true, true) => "wasm_fibers",
    };
    let object = dir.join(format!("future_driver_{name}.o"));
    let mut cc = Command::new("clang");
    if let Some(target) = target {
        cc.arg(format!("--target={target}"));
        cc.arg("-fPIC");
    }
    let output = cc
        .arg("-c")
        .arg(root().join("crates/ash/test/future/future_driver.c"))
        .arg("-I")
        .arg(root().join("std"))
        .arg("-o")
        .arg(&object)
        .output();
    let Ok(output) = output else {
        eprintln!("skipped {name}: clang unavailable");
        return;
    };
    assert!(
        output.status.success(),
        "clang: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    // A wasm plugin is a separate dylink.0 module. Its imports must be
    // exported by the main module and resolved by the side-module loader.
    if target.is_some() {
        let target_libdir = Command::new("rustc")
            .args(["--print", "target-libdir"])
            .output()
            .unwrap();
        assert!(target_libdir.status.success());
        let libdir = String::from_utf8(target_libdir.stdout).unwrap();
        let lld = Path::new(libdir.trim())
            .parent()
            .unwrap()
            .join("bin/gcc-ld/wasm-ld");
        let output = Command::new(lld)
            .args(["-shared", "--allow-undefined", "--no-entry"])
            .args([
                "--export=future_test_create",
                "--export=future_test_resolve",
                "--export=future_test_reject",
                "--export=future_test_create_abandoned",
                "--export=future_test_finish_abandoned",
            ])
            .arg(&object)
            .arg("-o")
            .arg(dir.join("future_test.wasm"))
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "wasm side-module link: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    let executable = dir.join(if target.is_some() {
        format!("future_{name}.wasm")
    } else {
        "future".to_string()
    });
    emit_aot(AotRequest {
        file,
        out: &dir.join(format!("future_{name}.o")),
        exe: Some(&executable),
        runtime: Some(runtime_path),
        target: target.map(str::to_string),
        pgo: None,
        allow_refused: false,
        abi_version: 1,
        quiet: true,
        links: links(target.is_some()),
        objects: if target.is_some() {
            Vec::new()
        } else {
            vec![object]
        },
        exports: Vec::new(),
        closures: Vec::new(),
        object_tails: Vec::new(),
        object_drops: Vec::new(),
        wasm_fibers,
    })
    .unwrap_or_else(|e| panic!("build {name}: {e:#}"));

    let output = if target.is_some() {
        let Some(ash) = runtime("ash") else {
            eprintln!("skipped wasm run: ash CLI unavailable");
            return;
        };
        Command::new(ash)
            .arg("run")
            .arg(&executable)
            .output()
            .unwrap()
    } else {
        Command::new(&executable).output().unwrap()
    };
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(output.status.success(), "{name}: {stdout}\n{stderr}");
    assert!(
        stdout.lines().any(|line| line == "future C ABI ok"),
        "{name}: {stdout}\n{stderr}"
    );
}

#[test]
fn c_future_carrier_works_in_native_and_wasm_programs() {
    let dir = tempfile::tempdir().unwrap();
    let file = fixture(dir.path());
    let native = runtime("libash_std.a").expect("native ash_std runtime archive");
    run_case(dir.path(), &file, None, &native, false);
    if let Some(wasm) = runtime("wasm32-wasip1/ash_runtime.o") {
        run_case(dir.path(), &file, Some("wasm32-wasip1"), &wasm, false);
        run_case(dir.path(), &file, Some("wasm32-wasip1"), &wasm, true);
    } else {
        eprintln!("skipped wasm: no prelinked runtime object");
    }
}
