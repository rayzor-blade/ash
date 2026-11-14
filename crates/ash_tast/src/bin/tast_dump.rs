//! Checks TAST files and prints what is in them.
//!
//!     cargo run -p ash_tast --bin tast_dump -- [--tree] [--keys] [--against=whole.tast [--allow=Class#s:name]] [layer.tast ...] program.tast
//!
//! The files are a stack, bottom first. The last one is the program; with `--tree`
//! its entry function is printed with names in place of ids.

use ash_tast::Stack;
use ash_tast::dump::Dumper;
use ash_tast::format::*;

fn main() {
    let mut tree = false;
    let mut keys = false;
    let mut against: Option<String> = None;
    let mut allowed: Vec<String> = Vec::new();
    let mut paths = Vec::new();
    for arg in std::env::args().skip(1) {
        if arg == "--tree" {
            tree = true;
        } else if arg == "--keys" {
            keys = true;
        } else if let Some(key) = arg.strip_prefix("--allow=") {
            allowed.push(key.to_owned());
        } else if let Some(path) = arg.strip_prefix("--against=") {
            against = Some(path.to_owned());
        } else {
            paths.push(arg);
        }
    }
    if paths.is_empty() {
        eprintln!("usage: tast_dump [--tree] [layer.tast ...] program.tast");
        std::process::exit(2);
    }
    let files: Vec<Vec<u8>> = paths
        .iter()
        .map(|path| {
            std::fs::read(path).unwrap_or_else(|e| {
                eprintln!("{path}: {e}");
                std::process::exit(1);
            })
        })
        .collect();
    let slices: Vec<&[u8]> = files.iter().map(Vec::as_slice).collect();

    let started = std::time::Instant::now();
    let parsed = Stack::parse(&slices);
    let elapsed = started.elapsed();
    let stack = match parsed {
        Ok(stack) => stack,
        Err(e) => {
            eprintln!("{e}");
            std::process::exit(1);
        }
    };
    println!(
        "valid, {} layer(s), {} bytes, opened and checked in {elapsed:?}",
        stack.layers().len(),
        files.iter().map(Vec::len).sum::<usize>()
    );
    for (i, (layer, path)) in stack.layers().iter().zip(&paths).enumerate() {
        println!(
            "layer {i} {path}: Haxe {}, id {:#018x}, strings {}  types {}  classes {}  fields {}  enums {}  locals {}  funcs {}  nodes {}  keys {}  kept {}",
            layer.haxe_version(),
            layer.layer_id(),
            layer.strings_len(),
            layer.types().len(),
            layer.classes().len(),
            layer.fields().len(),
            layer.enums().len(),
            layer.locals().len(),
            layer.funcs().len(),
            layer.nodes().len(),
            layer.keys().len(),
            layer.kept().len()
        );
    }
    if keys {
        for (i, layer) in stack.layers().iter().enumerate() {
            for kind in [
                key_kind::ROOT,
                key_kind::DIGEST,
                key_kind::DEFINE,
                key_kind::USED_DEFINE,
            ] {
                let name = KEY_KINDS
                    .iter()
                    .find(|(_, v)| *v == kind)
                    .map_or("?", |(n, _)| *n);
                let texts: Vec<&str> = layer
                    .keys()
                    .iter()
                    .filter(|k| k.kind.get() == kind)
                    .filter_map(|k| layer.opt_string(k.key))
                    .collect();
                println!("layer {i} {name} ({}): {}", texts.len(), texts.join(" "));
            }
        }
    }
    if let Some(path) = &against {
        compare(&stack, path, &allowed);
    }
    let top = stack.top();
    let at = stack.layers().len() - 1;
    match top.entry() {
        Some(entry) => {
            let owner = top
                .fields()
                .iter()
                .find(|f| f.func.get() == entry)
                .map(|f| {
                    let class = &top.classes()[f.owner.get() as usize];
                    format!(
                        "{}.{}",
                        top.opt_string(class.name).unwrap_or("?"),
                        top.opt_string(f.name).unwrap_or("?")
                    )
                });
            println!(
                "entry function {entry}: {}",
                owner.as_deref().unwrap_or("the main expression")
            );
            if tree && top.funcs()[entry as usize].root.get() != NONE {
                print!("{}", Dumper::new(&stack).function_text(at, entry as usize));
            }
        }
        None => println!("entry none"),
    }
}

/// Check that `stack` is the program in `whole` (the same program exported without
/// layers): the same functions with the same text, the stack's own plus what it kept.
fn compare(stack: &Stack, whole: &str, allowed: &[String]) {
    let bytes = std::fs::read(whole).unwrap_or_else(|e| {
        eprintln!("{whole}: {e}");
        std::process::exit(1);
    });
    let one = Stack::parse(&[&bytes]).unwrap_or_else(|e| {
        eprintln!("{whole}: {e}");
        std::process::exit(1);
    });
    let top = stack.layers().len() - 1;
    let expected = Dumper::new(&one).own_functions(0);
    let dumper = Dumper::new(stack);
    let mut actual = dumper.own_functions(top);
    for (key, text) in dumper.kept_functions(top) {
        if actual.insert(key.clone(), text).is_some() {
            println!("DUPLICATE {key}: in the program and in a layer");
        }
    }
    let missing = expected.keys().filter(|k| !actual.contains_key(*k)).count();
    let extra = actual.keys().filter(|k| !expected.contains_key(*k)).count();
    let differing: Vec<&String> = expected
        .iter()
        .filter(|(k, v)| actual.get(*k).is_some_and(|a| a != *v) && !allowed.contains(*k))
        .map(|(k, _)| k)
        .collect();
    println!(
        "against {whole}: {} functions whole, {} on the layers; missing {missing}, extra {extra}, differing {}",
        expected.len(),
        actual.len(),
        differing.len()
    );
    for key in expected.keys().filter(|k| !actual.contains_key(*k)).take(8) {
        println!("  missing: {key}");
    }
    for key in actual.keys().filter(|k| !expected.contains_key(*k)).take(8) {
        println!("  extra: {key}");
    }
    for key in differing.iter().take(8) {
        println!("  differs: {key}");
        let (a, b) = (&expected[*key], &actual[*key]);
        let line = a.lines().zip(b.lines()).position(|(x, y)| x != y);
        if let Some(at) = line {
            println!("    whole:   {}", a.lines().nth(at).unwrap_or(""));
            println!("    layered: {}", b.lines().nth(at).unwrap_or(""));
        } else {
            println!(
                "    one is a prefix of the other ({} and {} lines)",
                a.lines().count(),
                b.lines().count()
            );
        }
    }
    if missing + extra + differing.len() > 0 {
        std::process::exit(1);
    }
    println!("same program");
}
