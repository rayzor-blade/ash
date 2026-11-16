//! Compiles a fixture with the Haxe that has the `ash_tast` plugin, for the tests that need a
//! typed program. Set `ASH_HAXE_NATIVE` to that `haxe`; the tests skip without it.

// Each test crate that includes this module uses some of the helpers.
#![allow(dead_code)]

use std::path::{Path, PathBuf};
use std::process::Command;

pub fn tast_crate() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../ash_tast")
}

pub fn native_haxe() -> Option<String> {
    let name = std::env::var("ASH_HAXE_NATIVE").ok()?;
    let system = match std::env::consts::OS {
        "macos" => "Mac",
        "linux" => "Linux",
        other => other,
    };
    tast_crate()
        .join(format!("ocaml/ash_tast/cmxs/{system}/plugin.cmxs"))
        .exists()
        .then_some(name)
}

pub fn haxe(compiler: &str, fixture: &str, args: &[&str]) {
    let root = tast_crate();
    let output = Command::new(compiler)
        .arg("-cp")
        .arg(root.join("ocaml/ash_tast/hx"))
        .arg("-cp")
        .arg(root.join(fixture))
        .args(["-D", "retain-untyped-meta", "-main", "Main"])
        .args(args)
        .output()
        .expect("run haxe");
    assert!(
        output.status.success(),
        "haxe failed:\n{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

/// The program's TAST file, exported after the filters.
pub fn export(compiler: &str, fixture: &str, tag: &str) -> Vec<u8> {
    let path = std::env::temp_dir().join(format!(
        "ash_nair_{}_{}_{}.tast",
        std::process::id(),
        fixture.replace('/', "_"),
        tag
    ));
    haxe(
        compiler,
        fixture,
        &[
            "--macro",
            &format!("AshTastNative.exportFinal(\"{}\")", path.display()),
            "--hl",
            "unused.hl",
            "--no-output",
        ],
    );
    let bytes = std::fs::read(&path).unwrap();
    let _ = std::fs::remove_file(&path);
    bytes
}

/// The `ash` binary the compiled files are run with: `ASH_BIN`, else the release build.
pub fn ash_binary() -> Option<PathBuf> {
    if let Ok(p) = std::env::var("ASH_BIN") {
        return Some(PathBuf::from(p));
    }
    let release = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../target/release/ash");
    release.exists().then_some(release)
}

/// genhl's own `.hl` for the fixture.
pub fn genhl_file(compiler: &str, fixture: &str, tag: &str) -> Vec<u8> {
    let path = std::env::temp_dir().join(format!(
        "ash_nair_hl_{}_{}_{}.hl",
        std::process::id(),
        fixture.replace('/', "_"),
        tag
    ));
    haxe(compiler, fixture, &["--hl", &path.display().to_string()]);
    let bytes = std::fs::read(&path).unwrap();
    let _ = std::fs::remove_file(&path);
    bytes
}
