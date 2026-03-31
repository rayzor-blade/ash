fn main() {
    if cfg!(target_os = "macos") {
        println!("cargo:rustc-link-search=/opt/homebrew/lib");
        println!("cargo:rustc-link-lib=SDL2");
        println!("cargo:rustc-link-lib=framework=OpenGL");
        // Allow hl_* symbols to be resolved at runtime from ash_std (RTLD_GLOBAL)
        println!("cargo:rustc-cdylib-link-arg=-Wl,-undefined,dynamic_lookup");
    } else if cfg!(target_os = "linux") {
        println!("cargo:rustc-link-lib=SDL2");
        println!("cargo:rustc-link-lib=GL");
    }
}
