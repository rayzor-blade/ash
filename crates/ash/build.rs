use std::env;
use std::fs;
use std::path::PathBuf;

fn main() {
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    let target = env::var("TARGET").unwrap();

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
