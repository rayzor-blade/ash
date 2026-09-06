//! What a program asks of its native libraries.
//!
//! Porting a library to a target that cannot load the real one starts with
//! knowing which primitives the program actually names -- which is a
//! different and much smaller set than the library's exports. `sdl.hdll`
//! exports 208; a given game references a fraction of them, and the
//! difference is the difference between a demonstration and a
//! reimplementation.
//!
//! Env-gated rather than run by default: it needs a program to look at, and
//! there is no one program this repository is about.
//!
//!     ASH_NATIVES_PROGRAM=examples/heaps_base2d/bin/game.hl \
//!       cargo test -p ash --test native_surface -- --nocapture

use std::collections::BTreeMap;
use std::path::PathBuf;

#[test]
fn report_the_native_surface() {
    let Ok(path) = std::env::var("ASH_NATIVES_PROGRAM") else {
        eprintln!("set ASH_NATIVES_PROGRAM to a .hl to run this");
        return;
    };
    let path = PathBuf::from(path);
    // The decoder hashes field names through the runtime, so the runtime has
    // to be there before it can read anything.
    ash_core::native_lib::init_std_library().expect("starting the runtime");
    let code = ash_core::bytecode::BytecodeDecoder::decode(&path).expect("decoding the bytecode");

    let mut by_lib: BTreeMap<&str, Vec<&str>> = BTreeMap::new();
    for native in &code.natives {
        by_lib
            .entry(native.lib.as_str())
            .or_default()
            .push(native.name.as_str());
    }

    println!("{} references {} natives:", path.display(), code.natives.len());
    for (lib, mut names) in by_lib {
        names.sort_unstable();
        println!("\n  {lib}: {}", names.len());
        for chunk in names.chunks(6) {
            println!("    {}", chunk.join(" "));
        }
    }
}
