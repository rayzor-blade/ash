use std::env;
use std::fs;
use std::path::{Path, PathBuf};

fn generate_hl_bindings(out_dir: &Path) {
    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR"));
    let wrapper = manifest_dir.join("../../std/wrapper.h");
    let hl_header = manifest_dir.join("../../std/hl.h");

    println!("cargo:rerun-if-changed={}", wrapper.display());
    println!("cargo:rerun-if-changed={}", hl_header.display());

    let bindings = bindgen::Builder::default()
        .header(wrapper.to_string_lossy().into_owned())
        .parse_callbacks(Box::new(bindgen::CargoCallbacks::new()))
        .generate()
        .expect("Unable to generate HL bindings");

    bindings
        .write_to_file(out_dir.join("hl_bindings.rs"))
        .expect("Couldn't write HL bindings");
}

fn main() {
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    let target = env::var("TARGET").unwrap();

    // Generate target-specific HL C bindings in this crate's OUT_DIR.
    generate_hl_bindings(&out_dir);

    let mut lib_dir = PathBuf::from(env::current_dir().unwrap().as_path());

    let lib_filename = format!(
        "libash_std.{}",
        if target.contains("apple") {
            "dylib"
        } else if target.contains("windows") {
            "dll"
        } else {
            "so"
        }
    );

    lib_dir.push("../../target");

    let lib_path = PathBuf::from(lib_dir.as_path())
        .join(&target)
        .join("debug")
        .join(&lib_filename);

    println!("lib_path {:?}", lib_path);
    println!("cargo:rerun-if-changed={}", lib_path.display());

    // Read the library file into a byte array
    let lib_bytes = fs::read(&lib_path).expect("Failed to read cdylib file");

    // Write the byte array to a file in OUT_DIR
    let mut output_path = out_dir.join(lib_filename);
    output_path.set_extension("a");

    fs::write(&output_path, &lib_bytes).expect("Failed to write cdylib binary file");
}
