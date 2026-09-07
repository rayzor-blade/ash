fn main() {
    hl_native_gen::generate("wgpu.api").expect("generating the wgpu bindings");

    // `hlp_alloc_bytes` and its siblings belong to the VM and are resolved
    // when it loads this library. Unix tolerates that in a shared object; the
    // Apple linker calls it "Undefined symbols for architecture" unless told.
    println!("cargo:rerun-if-changed=build.rs");
    if std::env::var("CARGO_CFG_TARGET_OS").as_deref() == Ok("macos") {
        println!("cargo:rustc-cdylib-link-arg=-Wl,-undefined,dynamic_lookup");
    }
}
