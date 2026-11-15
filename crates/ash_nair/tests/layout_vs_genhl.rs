//! The layouts `ash_nair` computes from the typed program are the ones the HashLink generator
//! writes into a `.hl` for the same program: every class, enum and interface, field for field and
//! slot for slot.
//!
//! Needs the Haxe built from source with the `ash_tast` plugin; set `ASH_HAXE_NATIVE` to that
//! `haxe`. Skipped without it.

mod common;

use ash_nair::build::TypeBuilder;
use ash_nair::compare::{differences, program_names};
use ash_nair::hlfile::read_types;
use ash_tast::Tast;

fn check(fixture: &str) {
    let Some(compiler) = common::native_haxe() else {
        eprintln!("skipped: set ASH_HAXE_NATIVE to a haxe with the ash_tast plugin built");
        return;
    };
    let hl = std::env::temp_dir().join(format!(
        "ash_nair_{}_{}.hl",
        std::process::id(),
        fixture.replace('/', "_")
    ));
    common::haxe(&compiler, fixture, &["--hl", &hl.display().to_string()]);
    let hl_bytes = std::fs::read(&hl).unwrap();
    let _ = std::fs::remove_file(&hl);
    let tast_bytes = common::export(&compiler, fixture, "layout");

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
