//! Link the two real objects and check the result is a module.
//!
//! Validation is the floor, not the bar: the failure this linker is exposed
//! to writes a wrong value into a correctly shaped slot, which validates. The
//! bar is running the module and comparing its output, which the ash test
//! suite does.

use std::path::PathBuf;

fn dir() -> Option<PathBuf> {
    std::env::var("ASH_LINK_TEST_OBJECTS")
        .ok()
        .map(PathBuf::from)
}

#[test]
fn links_program_and_runtime() {
    let Some(d) = dir() else {
        eprintln!("set ASH_LINK_TEST_OBJECTS to a directory holding program.o and runtime.o");
        return;
    };
    let program = std::fs::read(d.join("program.o")).expect("program.o");
    let runtime = std::fs::read(d.join("runtime.o")).expect("runtime.o");

    let objects = vec![
        ash_wasm_link::read("program.o", &program).expect("read program"),
        ash_wasm_link::read("runtime.o", &runtime).expect("read runtime"),
    ];
    let module =
        ash_wasm_link::link(objects, &ash_wasm_link::LinkOptions::default()).expect("link");
    eprintln!("linked {} bytes", module.len());

    if let Ok(out) = std::env::var("ASH_LINK_TEST_OUT") {
        std::fs::write(&out, &module).expect("write");
        eprintln!("wrote {out}");
    }

    let mut features = wasmparser::WasmFeatures::default();
    features.insert(wasmparser::WasmFeatures::EXCEPTIONS);
    let mut validator = wasmparser::Validator::new_with_features(features);
    validator
        .validate_all(&module)
        .expect("the output validates");
}
