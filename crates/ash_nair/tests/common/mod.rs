//! Compiles a fixture with the Haxe that has the `ash_tast` plugin, for the tests that need a
//! typed program. Set `ASH_HAXE_NATIVE` to that `haxe`; the tests skip without it.

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
