//! The reader, checked against the objects ash actually emits.
//!
//! The assertion that matters is not "it parsed" but that every function
//! patch site already holds the index its symbol resolves to. That is what
//! proves relocation offsets are measured from where this linker thinks they
//! are, and it is the one mistake that would otherwise surface as a module
//! that validates, runs, and computes wrong answers.

use std::path::PathBuf;

fn object(name: &str) -> Option<Vec<u8>> {
    let p = PathBuf::from(std::env::var("ASH_LINK_TEST_OBJECTS").ok()?).join(name);
    std::fs::read(p).ok()
}

#[test]
fn function_patch_sites_agree_with_the_symbol_table() {
    for name in ["program.o", "runtime.o"] {
        let Some(bytes) = object(name) else {
            eprintln!("skipping {name}: set ASH_LINK_TEST_OBJECTS to a directory holding it");
            continue;
        };
        let obj = ash_wasm_link::read(name, &bytes).expect("read");
        eprintln!("{}", ash_wasm_link::describe(&obj));
        let wrong = ash_wasm_link::check_function_sites(&obj).expect("check");
        assert!(
            wrong.is_empty(),
            "{name}: {} of {} function patch sites disagree with the symbol table; \
             first few: {:?}",
            wrong.len(),
            obj.code_relocs.len(),
            &wrong[..wrong.len().min(5)]
        );
    }
}
