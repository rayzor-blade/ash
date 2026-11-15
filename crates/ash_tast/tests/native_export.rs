//! The OCaml plugin's export is the same program as the Haxe exporter's: every function
//! has the same text, and the entry is the same function.
//!
//! Needs a Haxe built from source with the plugin (`make plugin PLUGIN=ash_tast` in a
//! Haxe 4.3.6 tree with `ocaml/ash_tast` linked into its `plugins/`); set
//! `ASH_HAXE_NATIVE` to that `haxe`. Skipped without it.

use std::path::PathBuf;
use std::process::Command;

use ash_tast::Stack;
use ash_tast::dump::Dumper;

fn root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

/// The Haxe binary the plugin was built into, when there is one and the plugin is built.
fn native_haxe() -> Option<String> {
    let name = std::env::var("ASH_HAXE_NATIVE").ok()?;
    let system = match std::env::consts::OS {
        "macos" => "Mac",
        "linux" => "Linux",
        other => other,
    };
    let plugin = root().join(format!("ocaml/ash_tast/cmxs/{system}/plugin.cmxs"));
    plugin.exists().then_some(name)
}

fn export(haxe: &str, init: &str, out: &std::path::Path) {
    let root = root();
    let output = Command::new(haxe)
        .arg("-cp")
        .arg(root.join("haxe"))
        .arg("-cp")
        .arg(root.join("ocaml/ash_tast/hx"))
        .arg("-cp")
        .arg(root.join("tests/export"))
        .arg("--macro")
        .arg(init.replace("OUT", &out.display().to_string()))
        .args(["-main", "Main", "--hl", "unused.hl", "--no-output"])
        .output()
        .expect("run haxe");
    assert!(
        output.status.success(),
        "haxe failed:\n{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn the_native_writer_exports_the_same_program() {
    let Some(haxe) = native_haxe() else {
        eprintln!("skipped: set ASH_HAXE_NATIVE to a haxe with the ash_tast plugin built");
        return;
    };
    let dir = std::env::temp_dir();
    let id = std::process::id();
    let (from_haxe, from_plugin) = (
        dir.join(format!("ash_tast_native_{id}_haxe.tast")),
        dir.join(format!("ash_tast_native_{id}_plugin.tast")),
    );
    export(&haxe, "ash.tast.Export.init(\"OUT\")", &from_haxe);
    export(&haxe, "AshTastNative.export(\"OUT\")", &from_plugin);

    let (a, b) = (
        std::fs::read(&from_haxe).unwrap(),
        std::fs::read(&from_plugin).unwrap(),
    );
    let _ = std::fs::remove_file(&from_haxe);
    let _ = std::fs::remove_file(&from_plugin);
    let one = Stack::parse(&[&a]).expect("the Haxe export is valid");
    let two = Stack::parse(&[&b]).expect("the native export is valid");

    let expected = Dumper::new(&one).own_functions(0);
    let actual = Dumper::new(&two).own_functions(0);
    assert!(expected.len() > 100, "the program has its functions");
    let missing: Vec<_> = expected
        .keys()
        .filter(|k| !actual.contains_key(*k))
        .collect();
    let extra: Vec<_> = actual
        .keys()
        .filter(|k| !expected.contains_key(*k))
        .collect();
    assert!(
        missing.is_empty() && extra.is_empty(),
        "missing {missing:?}, extra {extra:?}"
    );
    for (key, text) in &expected {
        assert_eq!(&actual[key], text, "{key} differs");
    }

    let entry = |stack: &Stack| {
        let tast = &stack.layers()[0];
        Dumper::new(stack).function_text(0, tast.entry().expect("an entry") as usize)
    };
    assert_eq!(entry(&one), entry(&two));
}
