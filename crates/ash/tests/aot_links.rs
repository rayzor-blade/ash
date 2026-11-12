//! Natives a host links ahead of time, with every kind of cast the link can
//! ask for: integers widened and narrowed, a bool made of an integer, the
//! built-in number boxing, a host cast that is handed the program's type, a
//! call after the native, the same behind a data flag, and an initialiser.
//! The same program runs natively and as wasm, with and without fibers, and
//! must print the same lines.
#![cfg(feature = "llvm")]

use ash_core::llvm::aot_build::{AotRequest, emit_aot};
use ash_core::native_lib::{HostLink, Word};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::Command;

const EXPECTED: &[&str] = &[
    "add 42",
    "twice64 -42 -2",
    "negate false true",
    "half 1.5",
    "boxed 1.5",
    // The canonical NaN: no tag bits, no payload.
    "boxed-nan 2146959360 0",
    // 1, cast to 101 going in, and by 100 again coming out.
    "bump 201",
    "ticks 0",
    "counted 7 ticks 1",
    "flagged-off 9 ticks 1",
    "flagged-on 11 ticks 2",
    "bind 12 true true",
];

fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../..")
}

fn runtime(name: &str) -> Option<PathBuf> {
    ["debug", "release"]
        .iter()
        .map(|profile| root().join("target").join(profile).join(name))
        .find(|path| path.is_file())
}

fn link(symbol: &str, params: &[Word], ret: Option<Word>) -> HostLink {
    HostLink {
        symbol: format!("links_test_{symbol}"),
        params: params.to_vec(),
        ret,
        arg_casts: vec![None; params.len()],
        ret_cast: None,
        after: None,
        after_flag: None,
        init: None,
        library: None,
    }
}

fn links() -> HashMap<(String, String), HostLink> {
    use Word::{Bool, F64, I32, I64};
    let mut table = vec![
        ("add", link("add", &[I32, I32], Some(I32))),
        ("twice64", link("twice64", &[I64], Some(I64))),
        ("negate", link("negate", &[Bool], Some(Bool))),
        ("half", link("half", &[F64], Some(F64))),
        ("ticks", link("ticks", &[], Some(I32))),
        ("set_pending", link("set_pending", &[I32], None)),
        ("bound_made", link("bound_made", &[], Some(I32))),
        ("bound_is", link("bound_is", &[Word::Ptr], Some(Bool))),
        ("bound_typed", link("bound_typed", &[], Some(Bool))),
    ];
    // The same callee as `half`, taking and answering the program's number
    // words.
    table.push((
        "boxed_half",
        HostLink {
            arg_casts: vec![Some("ash:unbox_f64".into())],
            ret_cast: Some("ash:box_f64".into()),
            ..link("half", &[F64], Some(F64))
        },
    ));
    // A host cast on the way in and again on the way out.
    table.push((
        "bump",
        HostLink {
            arg_casts: vec![Some("links_test_adjust".into())],
            ret_cast: Some("links_test_adjust".into()),
            ..link("id", &[I32], Some(I32))
        },
    ));
    table.push((
        "add_counted",
        HostLink {
            after: Some("links_test_tick".into()),
            ..link("add", &[I32, I32], Some(I32))
        },
    ));
    table.push((
        "add_flagged",
        HostLink {
            after: Some("links_test_tick".into()),
            after_flag: Some("links_test_pending".into()),
            ..link("add", &[I32, I32], Some(I32))
        },
    ));
    // The program's object is the first argument; the callee never sees it.
    table.push((
        "bind",
        HostLink {
            init: Some("links_test_bind".into()),
            ..link("make", &[I32], Some(I32))
        },
    ));
    table
        .into_iter()
        .map(|(name, link)| (("links_test".to_string(), name.to_string()), link))
        .collect()
}

fn fixture(dir: &Path) -> PathBuf {
    let file = dir.join("links.hl");
    if Command::new("haxe").arg("-version").output().is_ok() {
        let output = Command::new("haxe")
            .arg("-cp")
            .arg(root().join("crates/ash/test/tests"))
            .args(["-main", "TestWasmLinks", "-hl"])
            .arg(&file)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "haxe: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    } else {
        std::fs::copy(root().join("crates/ash/test/tests/test_wasm_links.hl"), &file)
            .expect("committed links bytecode fixture");
    }
    file
}

fn run_case(dir: &Path, file: &Path, target: Option<&str>, runtime_path: &Path, wasm_fibers: bool) {
    let name = match (target.is_some(), wasm_fibers) {
        (false, _) => "native",
        (true, false) => "wasm",
        (true, true) => "wasm_fibers",
    };
    let object = dir.join(format!("links_driver_{name}.o"));
    let mut cc = Command::new("clang");
    if let Some(target) = target {
        cc.arg(format!("--target={target}"));
    }
    let output = cc
        .arg("-c")
        .arg(root().join("crates/ash/test/links/links_driver.c"))
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

    let executable = dir.join(if target.is_some() {
        format!("links_{name}.wasm")
    } else {
        "links".to_string()
    });
    emit_aot(AotRequest {
        file,
        out: &dir.join(format!("links_{name}.o")),
        exe: Some(&executable),
        runtime: Some(runtime_path),
        target: target.map(str::to_string),
        pgo: None,
        allow_refused: false,
        abi_version: 1,
        quiet: true,
        links: links(),
        objects: vec![object],
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
        Command::new(ash).arg("run").arg(&executable).output().unwrap()
    } else {
        Command::new(&executable).output().unwrap()
    };
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(output.status.success(), "{name}: {stdout}\n{stderr}");
    let lines: Vec<&str> = stdout.lines().collect();
    assert_eq!(lines, EXPECTED, "{name}: {stderr}");
}

#[test]
fn linked_natives_cast_the_same_natively_and_on_wasm() {
    let dir = tempfile::tempdir().unwrap();
    let file = fixture(dir.path());
    let native = runtime("libash_std.a").expect("native ash_std runtime archive");
    // The native build is LLVM's, which is what the wasm builds are held to.
    run_case(dir.path(), &file, None, &native, false);
    if let Some(wasm) = runtime("wasm32-wasip1/ash_runtime.o") {
        run_case(dir.path(), &file, Some("wasm32-wasip1"), &wasm, false);
        run_case(dir.path(), &file, Some("wasm32-wasip1"), &wasm, true);
    } else {
        eprintln!("skipped wasm: no prelinked runtime object");
    }
}
