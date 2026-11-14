//! Checks a TAST file and prints what is in it.
//!
//!     cargo run -p ash_tast --bin tast_dump -- program.tast [--tree]

use ash_tast::Tast;
use ash_tast::format::*;

fn kind_name(kind: u8) -> &'static str {
    NODE_KINDS
        .iter()
        .find(|(_, k)| *k == kind)
        .map_or("?", |(name, _)| name)
}

fn print_tree(tast: &Tast, id: u32, depth: usize) {
    let node = tast.node(id);
    println!(
        "{:indent$}{} flags={} a={} b={}",
        "",
        kind_name(node.kind),
        node.flags,
        node.a.get(),
        node.b.get(),
        indent = depth * 2
    );
    for kid in tast.kid_ids(node) {
        print_tree(tast, kid, depth + 1);
    }
}

fn main() {
    let mut args = std::env::args().skip(1);
    let Some(path) = args.next() else {
        eprintln!("usage: tast_dump <file> [--tree]");
        std::process::exit(2);
    };
    let tree = args.next().as_deref() == Some("--tree");
    let bytes = std::fs::read(&path).unwrap_or_else(|e| {
        eprintln!("{path}: {e}");
        std::process::exit(1);
    });
    let tast = match Tast::parse(&bytes) {
        Ok(tast) => tast,
        Err(e) => {
            eprintln!("{path}: {e}");
            std::process::exit(1);
        }
    };
    println!(
        "{path}: valid, {} bytes, Haxe {}",
        bytes.len(),
        tast.haxe_version()
    );
    println!(
        "strings {}  types {}  classes {}  fields {}  enums {}  locals {}  funcs {}  nodes {}",
        tast.strings_len(),
        tast.types().len(),
        tast.classes().len(),
        tast.fields().len(),
        tast.enums().len(),
        tast.locals().len(),
        tast.funcs().len(),
        tast.nodes().len()
    );
    println!("entry {:?}", tast.entry());
    if tree && let Some(entry) = tast.entry() {
        let root = tast.funcs()[entry as usize].root.get();
        if root != NONE {
            print_tree(&tast, root, 0);
        }
    }
}
