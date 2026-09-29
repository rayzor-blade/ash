use std::env;
use std::fs;
use std::path::PathBuf;
use std::process::Command;

fn main() {
    let target = env::var("CARGO_CFG_TARGET_OS").unwrap();
    if target == "macos" {
        // The stock CLI supplies these symbols through its own libhl image.
        println!("cargo:rustc-link-arg=-Wl,-undefined,dynamic_lookup");
    } else if target == "windows" {
        // An import library carries the DLL name, not its implementation.
        // Generate one for stock libhl.dll without linking ash_std into the
        // HDLL or requiring a stock HashLink SDK on the build runner.
        let out = PathBuf::from(env::var("OUT_DIR").unwrap());
        let def = out.join("libhl.def");
        fs::write(
            &def,
            "LIBRARY libhl.dll\nEXPORTS\n  hl_gc_alloc_gen\n  hl_add_root\n  hl_remove_root\n  hl_blocking\n  hl_get_thread\n  hl_register_thread\n  hl_unregister_thread\n  hlt_abstract DATA\n",
        )
        .unwrap();
        let status = Command::new("lib.exe")
            .arg(format!("/def:{}", def.display()))
            .arg("/machine:x64")
            .arg(format!("/out:{}", out.join("libhl.lib").display()))
            .status()
            .expect("Visual Studio lib.exe");
        assert!(status.success(), "failed to generate libhl import library");
        println!("cargo:rustc-link-search=native={}", out.display());
        println!("cargo:rustc-link-lib=dylib=libhl");
    }
}
