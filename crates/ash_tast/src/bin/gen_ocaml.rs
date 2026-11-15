//! Prints the generated OCaml writer, or writes it to the path given.
//!
//!     cargo run -p ash_tast --bin gen_ocaml -- crates/ash_tast/ocaml/ash_tast/ml/tast_format.ml

fn main() {
    let text = ash_tast::ocaml::generate();
    match std::env::args().nth(1) {
        Some(path) => std::fs::write(&path, text).unwrap_or_else(|e| {
            eprintln!("could not write {path}: {e}");
            std::process::exit(1);
        }),
        None => print!("{text}"),
    }
}
