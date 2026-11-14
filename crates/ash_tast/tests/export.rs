//! The Haxe exporter, run by the real Haxe compiler, read by this crate.
//!
//! Skipped when `haxe` is not installed.

use std::collections::BTreeSet;
use std::path::PathBuf;
use std::process::Command;
use std::sync::atomic::{AtomicUsize, Ordering};

use ash_tast::Tast;
use ash_tast::format::*;

fn haxe() -> Option<String> {
    let name = std::env::var("HAXE").unwrap_or_else(|_| "haxe".to_owned());
    Command::new(&name)
        .arg("--version")
        .output()
        .ok()
        .filter(|o| o.status.success())?;
    Some(name)
}

/// Tests run in parallel, so every export gets its own file.
static EXPORTS: AtomicUsize = AtomicUsize::new(0);

/// Export `tests/export/Main.hx` to a TAST file and answer its bytes.
fn export(extra: &[&str]) -> Option<Vec<u8>> {
    let haxe = haxe().or_else(|| {
        eprintln!("skipped: no haxe on the path (set HAXE to name one)");
        None
    })?;
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out = std::env::temp_dir().join(format!(
        "ash_tast_export_{}_{}.tast",
        std::process::id(),
        EXPORTS.fetch_add(1, Ordering::Relaxed)
    ));
    let output = Command::new(haxe)
        .arg("-cp")
        .arg(root.join("haxe"))
        .arg("-cp")
        .arg(root.join("tests/export"))
        .arg("--macro")
        .arg(format!("ash.tast.Export.init(\"{}\")", out.display()))
        .args(extra)
        .args(["-main", "Main", "--hl", "unused.hl", "--no-output"])
        .output()
        .expect("run haxe");
    assert!(
        output.status.success(),
        "haxe failed:\n{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let bytes = std::fs::read(&out).expect("the exporter wrote its file");
    let _ = std::fs::remove_file(&out);
    Some(bytes)
}

#[test]
fn the_exporter_writes_a_file_this_crate_accepts() {
    let Some(bytes) = export(&[]) else { return };
    let tast = Tast::parse(&bytes).expect("the exported file is valid");
    assert_eq!(tast.haxe_version() / 10000, 4, "Haxe 4");

    // The entry is a function whose body is the main expression.
    let entry = &tast.funcs()[tast.entry().expect("an entry") as usize];
    let body = tast.body(entry).expect("the entry has a body");
    assert_eq!(body.kind, node_kind::CALL);

    // `Main.main` and `Main.area` are static methods with bodies.
    let main_class = tast
        .classes()
        .iter()
        .position(|c| tast.opt_string(c.name) == Some("Main"))
        .expect("class Main");
    let methods: BTreeSet<&str> = tast
        .fields()
        .iter()
        .filter(|f| f.owner.get() as usize == main_class && f.func.get() != NONE)
        .filter_map(|f| tast.opt_string(f.name))
        .collect();
    assert!(
        methods.contains("main") && methods.contains("area"),
        "{methods:?}"
    );
}

#[test]
fn the_program_shows_up_as_the_node_kinds_it_uses() {
    let Some(bytes) = export(&[]) else { return };
    let tast = Tast::parse(&bytes).unwrap();
    let kinds: BTreeSet<u8> = tast.nodes().iter().map(|n| n.kind).collect();
    for (name, kind) in [
        ("SWITCH", node_kind::SWITCH),
        ("CASE", node_kind::CASE),
        ("ENUM_PARAMETER", node_kind::ENUM_PARAMETER),
        ("TRY", node_kind::TRY),
        ("CATCH", node_kind::CATCH),
        ("THROW", node_kind::THROW),
        ("OBJECT_DECL", node_kind::OBJECT_DECL),
        ("OBJECT_FIELD", node_kind::OBJECT_FIELD),
        ("FUNCTION", node_kind::FUNCTION),
        // `for` over an array is already a counting `while` loop when the typer hands it over.
        ("WHILE", node_kind::WHILE),
        ("IF", node_kind::IF),
        ("ARRAY_DECL", node_kind::ARRAY_DECL),
        ("BINOP", node_kind::BINOP),
        ("UNOP", node_kind::UNOP),
    ] {
        assert!(
            kinds.contains(&kind),
            "no {name} node in the exported program"
        );
    }
}

#[test]
fn nullability_and_structure_types_survive() {
    let Some(bytes) = export(&[]) else { return };
    let tast = Tast::parse(&bytes).unwrap();
    // `Null<Int>` stays a type of its own, not a bare Int.
    let has_null = tast
        .abstracts()
        .iter()
        .any(|a| tast.opt_string(a.name) == Some("Null"));
    assert!(has_null, "the Null abstract is in the table");
    let null_types = tast
        .types()
        .iter()
        .filter(|t| t.kind == type_kind::ABSTRACT)
        .filter(|t| {
            let def = &tast.abstracts()[t.def.get() as usize];
            tast.opt_string(def.name) == Some("Null") && t.args_count.get() == 1
        })
        .count();
    assert!(null_types >= 1, "a Null<T> type is present");
    // The self-referencing structure has one anonymous type that names its own id.
    let anon = tast
        .types()
        .iter()
        .filter(|t| t.kind == type_kind::ANON)
        .count();
    assert!(anon >= 2, "anonymous structure types: {anon}");
}

#[test]
fn positions_are_added_on_request() {
    let (Some(plain), Some(with)) = (export(&[]), export(&["-D", "ash_tast_positions"])) else {
        return;
    };
    let plain = Tast::parse(&plain).unwrap();
    let with = Tast::parse(&with).unwrap();
    assert!(plain.positions().is_empty());
    assert!(!with.positions().is_empty());
    assert!(
        with.nodes()
            .iter()
            .all(|n| n.pos.get() != NONE || n.kind >= node_kind::CASE)
    );
}
