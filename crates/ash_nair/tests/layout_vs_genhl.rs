//! The layouts `ash_nair` computes from the typed program are the ones the HashLink generator
//! writes into a `.hl` for the same program: every class, enum and interface, field for field and
//! slot for slot.
//!
//! Needs the Haxe built from source with the `ash_tast` plugin; set `ASH_HAXE_NATIVE` to that
//! `haxe`. Skipped without it.

use std::path::{Path, PathBuf};
use std::process::Command;

use ash_nair::build::TypeBuilder;
use ash_nair::compare::{differences, program_names};
use ash_nair::hlfile::read_types;
use ash_tast::Tast;

fn tast_crate() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../ash_tast")
}

fn native_haxe() -> Option<String> {
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

fn haxe(haxe: &str, fixture: &str, args: &[&str]) {
    let root = tast_crate();
    let output = Command::new(haxe)
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

fn check(fixture: &str) {
    let Some(compiler) = native_haxe() else {
        eprintln!("skipped: set ASH_HAXE_NATIVE to a haxe with the ash_tast plugin built");
        return;
    };
    let dir = std::env::temp_dir();
    let id = std::process::id();
    let name = fixture.replace('/', "_");
    let hl = dir.join(format!("ash_nair_{id}_{name}.hl"));
    let tast = dir.join(format!("ash_nair_{id}_{name}.tast"));
    haxe(&compiler, fixture, &["--hl", &hl.display().to_string()]);
    haxe(
        &compiler,
        fixture,
        &[
            "--macro",
            &format!("AshTastNative.exportFinal(\"{}\")", tast.display()),
            "--hl",
            "unused.hl",
            "--no-output",
        ],
    );
    let (hl_bytes, tast_bytes) = (std::fs::read(&hl).unwrap(), std::fs::read(&tast).unwrap());
    let _ = std::fs::remove_file(&hl);
    let _ = std::fs::remove_file(&tast);

    let theirs = read_types(&hl_bytes).expect("a readable .hl");
    let file = Tast::parse(&tast_bytes).expect("a valid TAST file");
    let mut builder = TypeBuilder::new(file);
    builder.build_program().expect("the program lays out");
    let (compared, lazy, problems) = differences(&builder.table, &theirs, &program_names(&file));
    assert!(compared > 50, "compared only {compared} types");
    assert!(
        problems.is_empty(),
        "{} of {compared} types differ from genhl's; the first:\n{}",
        problems.len(),
        problems
            .iter()
            .take(12)
            .cloned()
            .collect::<Vec<_>>()
            .join("\n")
    );
    eprintln!(
        "{fixture}: {compared} classes and enums match genhl's; {lazy} more are created only while compiling bodies"
    );
}

#[test]
fn the_export_fixture_has_genhls_layouts() {
    check("tests/export");
}

#[test]
fn the_native_fixture_has_genhls_layouts() {
    check("tests/native");
}
