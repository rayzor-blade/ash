//! A program exported on top of a standard-library layer is the same program as one
//! exported whole: every function has the same text, and the layer supplies exactly
//! the standard-library functions the compiler kept.
//!
//! Needs `haxe`; skipped without it.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Mutex, OnceLock};

use ash_tast::Stack;
use ash_tast::dump::Dumper;
use ash_tast::format::*;

/// The HL version the standard library is typed for, and so the layer is built for.
const HL: [&str; 2] = ["-D", "hl-ver=1.13.0"];

fn haxe() -> Option<String> {
    let name = std::env::var("HAXE").unwrap_or_else(|_| "haxe".to_owned());
    Command::new(&name)
        .arg("--version")
        .output()
        .ok()
        .filter(|o| o.status.success())?;
    Some(name)
}

fn root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

static FILES: AtomicUsize = AtomicUsize::new(0);

fn scratch(name: &str) -> PathBuf {
    std::env::temp_dir().join(format!(
        "ash_tast_layers_{}_{}_{name}",
        std::process::id(),
        FILES.fetch_add(1, Ordering::Relaxed)
    ))
}

/// Run the Haxe compiler on `tests/export/Main.hx` with the exporter, answering what it printed.
fn export(haxe: &str, out: &Path, init: &str, extra: &[&str]) -> Output {
    let root = root();
    let output = Command::new(haxe)
        .arg("-cp")
        .arg(root.join("haxe"))
        .arg("-cp")
        .arg(root.join("tests/export"))
        .args(HL)
        .arg("--macro")
        .arg(init.replace("OUT", &out.display().to_string()))
        .args(extra)
        .args(["--hl", "unused.hl", "--no-output"])
        .output()
        .expect("run haxe");
    assert!(
        output.status.success(),
        "haxe failed:\n{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    output
}

/// A standard-library layer built with `flags`, built once per flag set for the whole run.
fn std_layer(flags: &[&str]) -> Option<PathBuf> {
    static LAYERS: OnceLock<Mutex<HashMap<String, Option<PathBuf>>>> = OnceLock::new();
    let cache = LAYERS.get_or_init(Default::default);
    let key = flags.join(" ");
    let mut cache = cache.lock().unwrap();
    if let Some(found) = cache.get(&key) {
        return found.clone();
    }
    let built = build_std_layer(flags);
    cache.insert(key, built.clone());
    built
}

fn build_std_layer(flags: &[&str]) -> Option<PathBuf> {
    let haxe = haxe()?;
    let out = scratch("std.tast");
    // Every class the HL target can type, with nothing removed.
    let include = "include('', true, ['cpp','cs','flash','java','jvm','js','lua','neko','php','python','eval','hl._std','haxe.macro.ExampleJSGenerator','ash.tast'])";
    let root = root();
    // `include('')` takes every module under the working directory too, so run it in an empty one.
    let here = scratch("std_cwd");
    std::fs::create_dir_all(&here).expect("a scratch directory");
    let output = Command::new(&haxe)
        .current_dir(&here)
        .arg("-cp")
        .arg(root.join("haxe"))
        .args(HL)
        .args(flags)
        .arg("--macro")
        .arg(include)
        .arg("--macro")
        .arg(format!(
            "ash.tast.Export.init(\"{}\", [], [\"<std>\"])",
            out.display()
        ))
        .args(["-dce", "no", "--hl", "unused.hl", "--no-output"])
        .output()
        .expect("run haxe");
    assert!(
        output.status.success(),
        "the std layer did not build:\n{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    Some(out)
}

/// Export `tests/export/Main.hx` whole and on a layer built with `layer_flags`, with
/// `program_flags` on the program, and check they are the same program.
fn check_equivalence(layer_flags: &[&str], program_flags: &[&str]) {
    let (Some(haxe), Some(layer)) = (haxe(), std_layer(layer_flags)) else {
        eprintln!("skipped: no haxe on the path");
        return;
    };
    let whole = scratch("whole.tast");
    let layered = scratch("layered.tast");
    let mut args = vec!["-main", "Main"];
    args.extend_from_slice(program_flags);
    export(&haxe, &whole, "ash.tast.Export.init(\"OUT\")", &args);
    let out = export(
        &haxe,
        &layered,
        &format!("ash.tast.Export.init(\"OUT\", [\"{}\"])", layer.display()),
        &args,
    );

    let (whole_bytes, layer_bytes, layered_bytes) = (
        std::fs::read(&whole).unwrap(),
        std::fs::read(&layer).unwrap(),
        std::fs::read(&layered).unwrap(),
    );
    let one = Stack::parse(&[&whole_bytes]).expect("the whole export is valid");
    let two = Stack::parse(&[&layer_bytes, &layered_bytes]).unwrap_or_else(|e| {
        panic!(
            "the layers do not stack: {e}\n{}",
            String::from_utf8_lossy(&out.stdout)
        )
    });
    assert_eq!(
        two.layers().len(),
        2,
        "the program was not exported on the layer:\n{}",
        String::from_utf8_lossy(&out.stdout)
    );
    assert!(
        layered_bytes.len() * 5 < whole_bytes.len(),
        "the program on the layer is {} bytes, exported whole {}",
        layered_bytes.len(),
        whole_bytes.len()
    );

    // The functions the program has, whole against layered: its own plus what it kept.
    let whole_texts = Dumper::new(&one).own_functions(0);
    let mut layered_texts = Dumper::new(&two).own_functions(1);
    let kept = Dumper::new(&two).kept_functions(1);
    assert!(!kept.is_empty(), "the program keeps part of the layer");
    for (key, text) in kept {
        assert!(
            !layered_texts.contains_key(&key),
            "{key} is in both the program and the layer"
        );
        layered_texts.insert(key, text);
    }
    let missing: Vec<_> = whole_texts
        .keys()
        .filter(|k| !layered_texts.contains_key(*k))
        .collect();
    let extra: Vec<_> = layered_texts
        .keys()
        .filter(|k| !whole_texts.contains_key(*k))
        .collect();
    assert!(
        missing.is_empty() && extra.is_empty(),
        "missing {missing:?}, extra {extra:?}"
    );
    for (key, text) in &whole_texts {
        assert_eq!(&layered_texts[key], text, "{key} differs");
    }

    // The entry is the same function.
    let entry = |stack: &Stack, layer: usize| {
        let tast = &stack.layers()[layer];
        Dumper::new(stack).function_text(layer, tast.entry().expect("an entry") as usize)
    };
    assert_eq!(entry(&one, 0), entry(&two, 1));
}

#[test]
fn a_program_on_the_std_layer_is_the_program_exported_whole() {
    check_equivalence(&[], &[]);
}

/// Dead-code removal at its most thorough, and the analyzer on: it rewrites function
/// bodies before a macro sees them, so the layer is built with the same analyzer flag.
#[test]
fn the_same_holds_with_full_dead_code_removal_and_the_analyzer_on() {
    check_equivalence(
        &["-D", "analyzer-optimize"],
        &["-dce", "full", "-D", "analyzer-optimize"],
    );
}

/// The exporter's own switch is not part of what a layer was built with: a layer built
/// verbosely serves a program that is not.
#[test]
fn the_exporters_own_defines_do_not_count() {
    check_equivalence(&["-D", "ash_tast_verbose"], &[]);
}

/// A define nothing in the layer's sources tests, and the compiler does not type by,
/// leaves the layer usable.
#[test]
fn an_unrelated_define_does_not_matter() {
    check_equivalence(&[], &["-D", "an_extra_define"]);
}

/// What the layer was built with and what the program sets must agree wherever the
/// standard library's sources look; otherwise the layer is ignored and the program is
/// exported whole.
#[test]
fn a_layer_is_ignored_when_a_define_its_sources_test_differs() {
    let (Some(haxe), Some(layer)) = (haxe(), std_layer(&[])) else {
        eprintln!("skipped: no haxe on the path");
        return;
    };
    let out = scratch("stale.tast");
    // The standard library tests `no_ssl`, which the layer was not built with.
    let output = export(
        &haxe,
        &out,
        &format!("ash.tast.Export.init(\"OUT\", [\"{}\"])", layer.display()),
        &["-main", "Main", "-D", "no_ssl"],
    );
    let said = String::from_utf8_lossy(&output.stdout);
    assert!(said.contains("out of date"), "no warning: {said}");
    let bytes = std::fs::read(&out).unwrap();
    // Exported whole: a file that stands alone.
    let stack = Stack::parse(&[&bytes]).expect("a whole export needs no layer");
    assert_eq!(stack.top().parent_ids().count(), 0);
}

/// A library is a layer of its own, built on the standard library's; a program stacks on both.
/// When the library's source files change the layer no longer describes them.
#[test]
fn a_library_layer_stacks_and_is_ignored_when_its_sources_changed() {
    let (Some(haxe), Some(std)) = (haxe(), std_layer(&[])) else {
        eprintln!("skipped: no haxe on the path");
        return;
    };
    let root = root();
    let lib = scratch("lib");
    std::fs::create_dir_all(lib.join("greet")).unwrap();
    let greeter = |text: &str| {
        format!(
            "package greet;\nclass Greeter {{ public static function hello():String return \"{text}\"; }}\n"
        )
    };
    std::fs::write(lib.join("greet/Greeter.hx"), greeter("hi")).unwrap();
    let lib_layer = scratch("lib.tast");
    let built = Command::new(&haxe)
        .current_dir(&lib)
        .arg("-cp")
        .arg(root.join("haxe"))
        .arg("-cp")
        .arg(&lib)
        .args(HL)
        .arg("--macro")
        .arg("include('greet')")
        .arg("--macro")
        .arg(format!(
            "ash.tast.Export.init(\"{}\", [\"{}\"], [\"{}\"])",
            lib_layer.display(),
            std.display(),
            lib.display()
        ))
        .args(["-dce", "no", "--hl", "unused.hl", "--no-output"])
        .output()
        .expect("run haxe");
    assert!(
        built.status.success(),
        "{}{}",
        String::from_utf8_lossy(&built.stdout),
        String::from_utf8_lossy(&built.stderr)
    );

    let program = |into: &Path| {
        let dir = scratch("program");
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(
            dir.join("Use.hx"),
            "class Use { static function main() { trace(greet.Greeter.hello()); } }\n",
        )
        .unwrap();
        Command::new(&haxe)
            .current_dir(&dir)
            .arg("-cp")
            .arg(root.join("haxe"))
            .arg("-cp")
            .arg(&dir)
            .arg("-cp")
            .arg(&lib)
            .args(HL)
            .arg("--macro")
            .arg(format!(
                "ash.tast.Export.init(\"{}\", [\"{}\", \"{}\"])",
                into.display(),
                std.display(),
                lib_layer.display()
            ))
            .args(["-main", "Use", "--hl", "unused.hl", "--no-output"])
            .output()
            .expect("run haxe")
    };

    // Unchanged: the program is exported on both layers.
    let first = scratch("use1.tast");
    let said = program(&first);
    assert!(
        said.status.success(),
        "{}",
        String::from_utf8_lossy(&said.stdout)
    );
    let (std_bytes, lib_bytes, first_bytes) = (
        std::fs::read(&std).unwrap(),
        std::fs::read(&lib_layer).unwrap(),
        std::fs::read(&first).unwrap(),
    );
    let stack = Stack::parse(&[&std_bytes, &lib_bytes, &first_bytes])
        .unwrap_or_else(|e| panic!("{e}\n{}", String::from_utf8_lossy(&said.stdout)));
    assert_eq!(stack.layers().len(), 3);
    // The program names the library's function by a tag into layer 1, and the standard
    // library's by a tag into layer 0.
    let kept: Vec<(usize, String)> = stack
        .top()
        .kept()
        .iter()
        .filter(|k| k.kind.get() == kept_kind::CLASS)
        .map(|k| {
            let (layer, class) = stack.class_rec(2, k.id);
            (
                layer,
                stack.layers()[layer]
                    .opt_string(class.name)
                    .unwrap_or("?")
                    .to_owned(),
            )
        })
        .collect();
    assert!(kept.contains(&(1, "greet.Greeter".to_owned())), "{kept:?}");
    assert!(kept.iter().any(|(layer, _)| *layer == 0), "{kept:?}");
    // The library layer holds only the library's classes: the standard library's are the layer below.
    assert_eq!(stack.layers()[1].classes().len(), 1);

    // The library's source changes: the layer no longer describes it.
    std::fs::write(lib.join("greet/Greeter.hx"), greeter("hello there")).unwrap();
    let second = scratch("use2.tast");
    let output = program(&second);
    assert!(output.status.success());
    let said = String::from_utf8_lossy(&output.stdout);
    assert!(said.contains("source files changed"), "{said}");
    let bytes = std::fs::read(&second).unwrap();
    assert_eq!(
        Stack::parse(&[&bytes]).unwrap().top().parent_ids().count(),
        0
    );
}
