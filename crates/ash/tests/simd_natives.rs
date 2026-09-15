//! Every `simd` native a program declares resolves from the runtime itself,
//! with no library file: the primitive table in `ash_simd` is exported by
//! `std/src/simd.rs` and read into the static symbol table by `build.rs`,
//! and this is where a name dropped from either would show.

use std::path::PathBuf;

#[test]
fn simd_natives_resolve_from_the_runtime() {
    let fixture = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test/tests/test_simd.hl");
    // The runtime under test, not a libhl installed system-wide.
    std::env::set_var("ASH_LIBHL", "embedded");
    ash_core::native_lib::init_std_library().expect("starting the runtime");
    let code = ash_core::bytecode::BytecodeDecoder::decode(&fixture).expect("decoding the fixture");
    let resolver = ash_core::native_lib::NativeFunctionResolver::new();

    let simd: Vec<_> = code
        .natives
        .iter()
        .filter(|n| n.lib.trim_start_matches('?') == "simd")
        .collect();
    assert!(
        simd.len() > 100,
        "the fixture declares {} simd natives; expected the whole surface",
        simd.len()
    );
    let mut missing = Vec::new();
    for native in &simd {
        let symbol = format!("hlp_{}", native.name);
        match resolver.resolve_function(&native.lib, &symbol) {
            Ok(addr) if !addr.is_null() => {}
            _ => missing.push(symbol),
        }
    }
    assert!(missing.is_empty(), "unresolved simd natives: {missing:?}");
}
