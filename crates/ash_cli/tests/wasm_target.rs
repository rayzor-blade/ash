//! The wasm32 emit must produce a module a WebAssembly toolchain accepts.
//!
//! An object file that merely exists proves nothing: the interesting failures
//! are structural, and they are exactly what a validator reports. So this
//! compiles a real program for `wasm32-wasip1`, validates the object, links it
//! into a core module, validates that, and then checks the module's shape --
//! that it exports the entry points, that its function table and indirect
//! calls survived, and that every unresolved import is a runtime symbol rather
//! than something the host ABI leaked in.
//!
//! The last check is the one that catches a cross-compile regression. Until
//! the wasm runtime exists (docs/wasm-target.md, phase 2) the module is linked
//! permissively, so the imports ARE the program's runtime boundary, listed.
//! Anything appearing there that is not an `hlp_*`, `hl_*` or `setjmp` means
//! generated code called something only a native host provides.
//!
//! External tools do the validating: `wasm-tools` and, for the link, the
//! Rust toolchain's own `rust-lld`. Homebrew's `wasm-ld` is not usable here --
//! it is lld 20 resolving against LLVM 21's libLLVM and aborts on a missing
//! symbol. Where a tool is absent the check it would have done is reported as
//! skipped rather than passing quietly.

use std::path::{Path, PathBuf};
use std::process::Command;

const TRIPLE: &str = "wasm32-wasip1";

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("the crate lives two levels below the root")
        .to_path_buf()
}

fn tool(name: &str) -> Option<PathBuf> {
    let found = Command::new("command")
        .args(["-v", name])
        .output()
        .ok()
        .filter(|o| o.status.success())
        .map(|o| PathBuf::from(String::from_utf8_lossy(&o.stdout).trim()));
    found.filter(|p| p.exists()).or_else(|| {
        // `command -v` is a shell builtin in some environments; fall back to
        // the paths a developer machine actually uses.
        ["/opt/homebrew/bin", "/usr/local/bin", "/usr/bin"]
            .iter()
            .map(|d| Path::new(d).join(name))
            .find(|p| p.exists())
    })
}

/// The Rust toolchain ships an lld built against its own LLVM, which is the
/// one linker on this machine guaranteed to match.
fn rust_lld() -> Option<PathBuf> {
    let home = std::env::var_os("HOME")?;
    let toolchains = Path::new(&home).join(".rustup/toolchains");
    let mut found = None;
    for entry in std::fs::read_dir(toolchains).ok()? {
        let Ok(entry) = entry else { continue };
        for target in std::fs::read_dir(entry.path().join("lib/rustlib"))
            .into_iter()
            .flatten()
        {
            let Ok(target) = target else { continue };
            let candidate = target.path().join("bin/rust-lld");
            if candidate.exists() {
                found = Some(candidate);
            }
        }
    }
    found
}

fn run(program: &Path, args: &[&str]) -> (String, bool) {
    let out = Command::new(program)
        .args(args)
        .output()
        .unwrap_or_else(|e| panic!("running {}: {e}", program.display()));
    let mut text = String::from_utf8_lossy(&out.stdout).into_owned();
    text.push_str(&String::from_utf8_lossy(&out.stderr));
    (text, out.status.success())
}

#[test]
fn wasm32_object_is_a_valid_module_with_the_expected_boundary() {
    let ash = PathBuf::from(env!("CARGO_BIN_EXE_ash"));
    let program = repo_root().join("crates/ash/test/tests/bench_fib.hl");
    let scratch = std::env::temp_dir().join("ash-wasm-target");
    std::fs::create_dir_all(&scratch).expect("scratch directory");
    let object = scratch.join("bench_fib.o");
    let module = scratch.join("bench_fib.wasm");
    let _ = std::fs::remove_file(&object);
    let _ = std::fs::remove_file(&module);

    let (emit, ok) = run(
        &ash,
        &[
            "--emit-aot",
            &object.to_string_lossy(),
            "--target",
            TRIPLE,
            "--quiet",
            &program.to_string_lossy(),
        ],
    );
    assert!(ok, "emitting for {TRIPLE} failed:\n{emit}");

    // The magic bytes are the one check that needs no tool at all.
    let bytes = std::fs::read(&object).expect("the object was written");
    assert_eq!(
        &bytes[..4],
        b"\0asm",
        "emitted object is not WebAssembly ({} bytes)",
        bytes.len()
    );

    let Some(wasm_tools) = tool("wasm-tools") else {
        eprintln!("wasm-tools not installed: validation skipped");
        return;
    };
    let (out, ok) = run(&wasm_tools, &["validate", &object.to_string_lossy()]);
    assert!(ok, "the emitted object does not validate:\n{out}");

    let Some(lld) = rust_lld() else {
        eprintln!("no rust-lld found: link and module checks skipped");
        return;
    };
    // Permissive on purpose: there is no wasm runtime to link against yet, so
    // the runtime's symbols stay unresolved and become this module's imports.
    let (out, ok) = run(
        &lld,
        &[
            "-flavor",
            "wasm",
            "--no-entry",
            "--export-dynamic",
            "--allow-undefined",
            "-o",
            &module.to_string_lossy(),
            &object.to_string_lossy(),
        ],
    );
    assert!(ok, "linking the wasm module failed:\n{out}");
    let (out, ok) = run(&wasm_tools, &["validate", &module.to_string_lossy()]);
    assert!(ok, "the linked module does not validate:\n{out}");

    // `wasm-tools print` gives the module as text, which is enough to see the
    // boundary and the entry points without a second tool.
    let (wat, ok) = run(&wasm_tools, &["print", &module.to_string_lossy()]);
    assert!(ok, "printing the module failed:\n{wat}");

    for entry in ["ash_module_init", "main"] {
        assert!(
            wat.contains(&format!("(export \"{entry}\"")),
            "the module does not export {entry}"
        );
    }
    assert!(
        wat.contains("(table") && wat.contains("funcref"),
        "the module has no function table, so indirect calls cannot work"
    );
    assert!(
        wat.contains("call_indirect"),
        "the module makes no indirect calls, which bench_fib does"
    );

    // Every import must be a runtime symbol. Anything else is the host ABI
    // reaching into a cross build.
    let mut unexpected: Vec<String> = Vec::new();
    for line in wat.lines() {
        let line = line.trim();
        if !line.starts_with("(import ") {
            continue;
        }
        let Some(name) = line.split('"').nth(3) else {
            continue;
        };
        let bare = name.trim_start_matches('_');
        if !(bare.starts_with("hlp_")
            || bare.starts_with("hl_")
            || bare.starts_with("setjmp")
            || bare.starts_with("longjmp"))
        {
            unexpected.push(name.to_string());
        }
    }
    assert!(
        unexpected.is_empty(),
        "the module imports symbols that are not the ash runtime: {unexpected:?}"
    );
}
