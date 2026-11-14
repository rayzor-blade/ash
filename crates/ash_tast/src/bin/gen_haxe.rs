//! Prints the generated Haxe writer, or writes it to the path given.
//!
//!     cargo run -p ash_tast --bin gen_haxe -- crates/ash_tast/haxe/ash/tast/Format.hx

fn main() {
    let text = ash_tast::haxe::generate();
    match std::env::args().nth(1) {
        Some(path) => std::fs::write(&path, text).unwrap_or_else(|e| {
            eprintln!("could not write {path}: {e}");
            std::process::exit(1);
        }),
        None => print!("{text}"),
    }
}
