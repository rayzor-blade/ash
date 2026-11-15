//! The OCaml plugin's export is the same program as the Haxe exporter's: every function
//! has the same text, and the entry is the same function.
//!
//! Needs a Haxe built from source with the plugin (`make plugin PLUGIN=ash_tast` in a
//! Haxe 4.3.6 tree with `ocaml/ash_tast` linked into its `plugins/`); set
//! `ASH_HAXE_NATIVE` to that `haxe`. Skipped without it.

use std::path::PathBuf;
use std::process::Command;

use zerocopy::little_endian::U32;

use ash_tast::dump::Dumper;
use ash_tast::format::*;
use ash_tast::{Stack, Tast};

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

fn export(haxe: &str, init: &str, out: &std::path::Path, extra: &[&str]) {
    export_in("tests/export", haxe, init, out, extra)
}

/// Compiles `dir`'s `Main` with `--no-output` and the macro `init`, `OUT` standing for `out`.
fn export_in(dir: &str, haxe: &str, init: &str, out: &std::path::Path, extra: &[&str]) {
    let root = root();
    let output = Command::new(haxe)
        .arg("-cp")
        .arg(root.join("haxe"))
        .arg("-cp")
        .arg(root.join("ocaml/ash_tast/hx"))
        .arg("-cp")
        .arg(root.join(dir))
        .arg("--macro")
        .arg(init.replace("OUT", &out.display().to_string()))
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
    export(&haxe, "ash.tast.Export.init(\"OUT\")", &from_haxe, &[]);
    export(&haxe, "AshTastNative.export(\"OUT\")", &from_plugin, &[]);

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

/// `exportFinal` writes the program after DCE and the type filters, which is what the HashLink
/// generator receives: smaller than the `onGenerate` export, with field initializers already
/// moved into constructors.
#[test]
fn the_final_export_is_the_program_after_dce() {
    let Some(haxe) = native_haxe() else {
        eprintln!("skipped: set ASH_HAXE_NATIVE to a haxe with the ash_tast plugin built");
        return;
    };
    let dir = std::env::temp_dir();
    let id = std::process::id();
    let (early, late) = (
        dir.join(format!("ash_tast_native_final_{id}_early.tast")),
        dir.join(format!("ash_tast_native_final_{id}_late.tast")),
    );
    let dce = ["-dce", "full"];
    export(&haxe, "AshTastNative.export(\"OUT\")", &early, &dce);
    export(&haxe, "AshTastNative.exportFinal(\"OUT\")", &late, &dce);
    let (a, b) = (
        std::fs::read(&early).unwrap(),
        std::fs::read(&late).unwrap(),
    );
    let _ = std::fs::remove_file(&early);
    let _ = std::fs::remove_file(&late);
    let (one, two) = (Tast::parse(&a).unwrap(), Tast::parse(&b).unwrap());

    assert!(two.entry().is_some(), "the final program has an entry");
    assert!(
        two.nodes().len() < one.nodes().len(),
        "DCE removed code: {} nodes against {}",
        two.nodes().len(),
        one.nodes().len()
    );
    let (early_fns, late_fns) = (
        Dumper::new(&Stack::parse(&[&a]).unwrap()).own_functions(0),
        Dumper::new(&Stack::parse(&[&b]).unwrap()).own_functions(0),
    );
    assert!(late_fns.len() < early_fns.len());
    assert!(late_fns.len() > 100, "the program keeps what main reaches");
}

/// With `-D ash_tast_positions` both writers record the same positions, on the same nodes.
#[test]
fn the_native_writer_records_the_same_positions() {
    let Some(haxe) = native_haxe() else {
        eprintln!("skipped: set ASH_HAXE_NATIVE to a haxe with the ash_tast plugin built");
        return;
    };
    let dir = std::env::temp_dir();
    let id = std::process::id();
    let (from_haxe, from_plugin) = (
        dir.join(format!("ash_tast_native_pos_{id}_haxe.tast")),
        dir.join(format!("ash_tast_native_pos_{id}_plugin.tast")),
    );
    let flags = ["-D", "ash_tast_positions"];
    export(&haxe, "ash.tast.Export.init(\"OUT\")", &from_haxe, &flags);
    export(&haxe, "AshTastNative.export(\"OUT\")", &from_plugin, &flags);
    let (a, b) = (
        std::fs::read(&from_haxe).unwrap(),
        std::fs::read(&from_plugin).unwrap(),
    );
    let _ = std::fs::remove_file(&from_haxe);
    let _ = std::fs::remove_file(&from_plugin);
    let (one, two) = (Tast::parse(&a).unwrap(), Tast::parse(&b).unwrap());

    // The plugin also writes what the Haxe exporter cannot reach (`__init__` blocks, metadata), so
    // the comparison is over the program's own file.
    let own = |t: &Tast, pos: U32| {
        t.opt_string(t.positions()[pos.get() as usize].file)
            .is_some_and(|f| f.ends_with("export/Main.hx"))
    };
    let spans = |t: &Tast| {
        let mut all: Vec<(u32, u32)> = t
            .positions()
            .iter()
            .enumerate()
            .filter(|(i, _)| own(t, U32::new(*i as u32)))
            .map(|(_, p)| (p.start.get(), p.end.get()))
            .collect();
        all.sort();
        all
    };
    assert!(!spans(&one).is_empty());
    assert_eq!(spans(&one), spans(&two));
    let placed = |t: &Tast| {
        t.nodes()
            .iter()
            .filter(|n| n.pos.get() != NONE && own(t, n.pos))
            .count()
    };
    assert_eq!(placed(&one), placed(&two));
}

/// A metadata argument as source text.
fn show(t: &Tast, n: &NodeRec) -> String {
    let join = |nodes: Vec<String>| nodes.join(", ");
    match n.kind {
        node_kind::CONST => match n.flags {
            const_kind::STRING => format!("{:?}", t.string(n.a.get())),
            const_kind::INT => (n.a.get() as i32).to_string(),
            const_kind::FLOAT => t.string(n.a.get()).to_owned(),
            const_kind::BOOL => (n.a.get() == 1).to_string(),
            const_kind::NULL => "null".to_owned(),
            other => panic!("constant kind {other}"),
        },
        node_kind::IDENT => t.string(n.a.get()).to_owned(),
        node_kind::ARRAY_DECL => {
            format!("[{}]", join(t.kids(n).iter().map(|k| show(t, k)).collect()))
        }
        node_kind::OBJECT_DECL => format!(
            "{{{}}}",
            join(
                t.kids(n)
                    .iter()
                    .map(|f| format!("{}: {}", t.string(f.a.get()), show(t, &t.kids(f)[0])))
                    .collect()
            )
        ),
        other => panic!("node kind {other}"),
    }
}

/// What the plugin writes beyond the code: the order, `__init__` blocks, declaration and
/// expression metadata, field flags, optional arguments, constraints, runtime metadata, defines
/// and resources.
#[test]
fn the_plugin_writes_what_the_lowering_reads() {
    let Some(haxe) = native_haxe() else {
        eprintln!("skipped: set ASH_HAXE_NATIVE to a haxe with the ash_tast plugin built");
        return;
    };
    let out = std::env::temp_dir().join(format!(
        "ash_tast_native_content_{}.tast",
        std::process::id()
    ));
    let resource = format!("{}@hello", root().join("tests/native/hello.txt").display());
    export_in(
        "tests/native",
        &haxe,
        "AshTastNative.exportFinal(\"OUT\")",
        &out,
        &[
            "-D",
            "my_flag=value",
            "-D",
            "retain-untyped-meta",
            "-resource",
            &resource,
        ],
    );
    let bytes = std::fs::read(&out).unwrap();
    let _ = std::fs::remove_file(&out);
    let t = Tast::parse(&bytes).expect("a valid file");

    let class_id = |name: &str| {
        t.classes()
            .iter()
            .position(|c| t.opt_string(c.name) == Some(name))
            .unwrap_or_else(|| panic!("class {name}")) as u32
    };
    let field_id = |class: &str, name: &str| {
        let c = &t.classes()[class_id(class) as usize];
        let first = c.fields_first.get();
        (first..first + c.fields_count.get())
            .find(|f| t.opt_string(t.fields()[*f as usize].name) == Some(name))
            .unwrap_or_else(|| panic!("field {class}.{name}")) as u32
    };
    let metas = |kind: u32, owner: u32| -> Vec<(String, String)> {
        t.metas_of(kind, owner)
            .map(|m| {
                let args: Vec<String> = t.meta_args(m).iter().map(|a| show(&t, a)).collect();
                (t.string(m.name.get()).to_owned(), args.join(", "))
            })
            .collect()
    };
    let has = |list: &[(String, String)], name: &str, args: &str| {
        assert!(
            list.iter().any(|(n, a)| n == name && a == args),
            "{name}({args}) in {list:?}"
        )
    };
    let (main, point, shape) = (class_id("Main"), class_id("Point"), class_id("Shape"));

    // Metadata of our own namespace on a class, with every argument shape.
    let on_main = metas(meta_owner::CLASS, main);
    has(&on_main, ":ash.pure", "\"a\", -1, 2.5, true, null, foo.bar");
    has(
        &metas(meta_owner::CLASS, point),
        ":ash.layout",
        "{align: 16, fields: [\"x\", \"y\"]}",
    );
    // Runtime metadata (`@author`) is not declaration metadata: it is an object for reflection.
    assert!(on_main.iter().all(|(n, _)| n != "author"));
    // Metadata on a field and on a local.
    has(
        &metas(meta_owner::FIELD, field_id("Point", "x")),
        ":ash.noThrow",
        "",
    );
    has(
        &metas(meta_owner::FIELD, field_id("Main", "counter")),
        ":ash.hot",
        "",
    );
    let local = t
        .locals()
        .iter()
        .position(|l| t.opt_string(l.name) == Some("local"))
        .expect("the local") as u32;
    has(&metas(meta_owner::LOCAL, local), ":ash.keep", "");
    // Metadata on an expression keeps its arguments after the expression.
    let on_expr = t
        .nodes()
        .iter()
        .find(|n| n.kind == node_kind::META && t.string(n.a.get()) == ":ash.unchecked")
        .expect("the :ash.unchecked node");
    let kids: Vec<String> = t
        .kids(on_expr)
        .iter()
        .skip(1)
        .map(|k| show(&t, k))
        .collect();
    assert_eq!(kids, ["1", "2"]);

    // `__init__`, and the compiler's order: an interface before the class that implements it.
    let main_rec = &t.classes()[main as usize];
    assert_ne!(main_rec.init_func.get(), NONE);
    assert_eq!(t.classes()[point as usize].init_func.get(), NONE);
    let at = |class: u32| {
        t.order()
            .iter()
            .position(|o| o.kind.get() == u32::from(module_kind::CLASS) && o.id.get() == class)
            .expect("listed in the order")
    };
    assert!(at(shape) < at(point));

    // Field kinds.
    let flags = |class: &str, name: &str| t.fields()[field_id(class, name) as usize].flags.get();
    assert_ne!(flags("Point", "y") & field_flag::NOT_PHYSICAL, 0);
    assert_eq!(flags("Point", "x") & field_flag::NOT_PHYSICAL, 0);
    assert_ne!(flags("Main", "replaceable") & field_flag::DYNAMIC, 0);
    assert_eq!(flags("Main", "opt") & field_flag::DYNAMIC, 0);

    // `?b` and `c = 3` are optional, `a` is not.
    let opt = &t.types()[t.fields()[field_id("Main", "opt") as usize].ty.get() as usize];
    assert_eq!(opt.kind, type_kind::FUN);
    assert_eq!(opt.opt_mask.get(), 0b110);

    // A type parameter's constraint takes the place of its interfaces.
    let constrained = t.classes().iter().any(|c| {
        c.flags.get() & class_flag::TYPE_PARAM != 0
            && t.type_args(c.interfaces_first, c.interfaces_count)
                .iter()
                .any(|ty| {
                    let ty = &t.types()[ty.get() as usize];
                    ty.kind == type_kind::CLASS && ty.def.get() == shape
                })
    });
    assert!(constrained, "T:Shape has its constraint");

    // The runtime metadata object `{obj: {author: ["native"]}}`.
    let root = t.node(main_rec.meta_root.get());
    assert_eq!(root.kind, node_kind::OBJECT_DECL);
    assert_eq!(show(&t, root), "{obj: {author: [\"native\"]}}");

    // Defines and resources.
    assert_eq!(t.define("my_flag"), Some("value"));
    assert!(t.define("hl").is_some());
    assert_eq!(t.define("no_such_define"), None);
    let [resource] = t.resources() else {
        panic!("one resource")
    };
    assert_eq!(t.string(resource.name.get()), "hello");
    assert_eq!(t.resource_data(resource), b"hello from the fixture\n");
    assert!(t.entry().is_some());
}
