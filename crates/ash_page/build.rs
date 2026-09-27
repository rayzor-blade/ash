//! Build the browser host into this crate: `ash_browser` for
//! wasm32-unknown-unknown, then wasm-bindgen over it, so neither a
//! wasm-bindgen CLI nor files beside a binary are needed at run time.
//!
//! `ASH_PAGE_HOST_DIR` names a directory holding a prebuilt `ash_browser.js`
//! and `ash_browser_bg.wasm` to use instead; safe, as long as they were built
//! from this tree.

use std::path::{Path, PathBuf};
use std::process::Command;
use std::{env, fs};

const FILES: [&str; 2] = ["ash_browser.js", "ash_browser_bg.wasm"];

fn main() {
    let out = PathBuf::from(env::var_os("OUT_DIR").expect("OUT_DIR"));
    let host = out.join("host");
    fs::create_dir_all(&host).expect("creating the host directory");

    println!("cargo:rerun-if-env-changed=ASH_PAGE_HOST_DIR");
    if let Some(dir) = env::var_os("ASH_PAGE_HOST_DIR") {
        let dir = PathBuf::from(dir);
        for file in FILES {
            let from = dir.join(file);
            println!("cargo:rerun-if-changed={}", from.display());
            fs::copy(&from, host.join(file))
                .unwrap_or_else(|e| panic!("ASH_PAGE_HOST_DIR: copying {}: {e}", from.display()));
        }
        return;
    }

    let manifest = PathBuf::from(env::var_os("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR"));
    let root = manifest.join("../..");
    for input in [
        "crates/ash_browser",
        "crates/ash_wasm_runtime/src",
        "crates/ash_wasm_runtime/Cargo.toml",
        "crates/ash_wasm_link/src",
        "crates/ash_wasm_link/Cargo.toml",
        "Cargo.lock",
    ] {
        println!("cargo:rerun-if-changed={}", root.join(input).display());
    }

    // Its own target directory: the outer build holds the lock on the
    // workspace's, and a second cargo waiting for it would wait forever.
    let target_dir = out.join("host-target");
    let cargo = env::var_os("CARGO").unwrap_or_else(|| "cargo".into());
    let status = Command::new(cargo)
        .current_dir(&root)
        .args(["build", "--release", "--locked", "-p", "ash_browser"])
        .args(["--target", "wasm32-unknown-unknown", "--target-dir"])
        .arg(&target_dir)
        // The outer build's flags are for the machine ash runs on, and an
        // environment RUSTFLAGS would replace the wasm target's own.
        .env_remove("RUSTFLAGS")
        .env_remove("CARGO_ENCODED_RUSTFLAGS")
        .env_remove("CARGO_TARGET_DIR")
        .env_remove("CARGO_BUILD_TARGET")
        .status()
        .expect("running cargo to build the browser host");
    if !status.success() {
        panic!(
            "building the browser host (ash_browser for wasm32-unknown-unknown) failed. \
             `rustup target add wasm32-unknown-unknown`, or set ASH_PAGE_HOST_DIR to a \
             directory holding ash_browser.js and ash_browser_bg.wasm"
        );
    }
    bind(
        &target_dir.join("wasm32-unknown-unknown/release/ash_browser.wasm"),
        &host,
    );
}

fn bind(wasm: &Path, host: &Path) {
    let mut bindgen = wasm_bindgen_cli_support::Bindgen::new();
    bindgen.input_path(wasm);
    bindgen
        .web(true)
        .expect("selecting wasm-bindgen's web output");
    // What the CLI does by default and the library does not: `init()` with no
    // argument loads ash_browser_bg.wasm from beside the script.
    bindgen.omit_default_module_path(false);
    bindgen
        .generate(host)
        .unwrap_or_else(|e| panic!("wasm-bindgen over {}: {e}", wasm.display()));
}
