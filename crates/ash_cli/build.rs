use std::{env, fs, path::PathBuf};

fn main() {
    // HDLLs are ordinary shared objects with undefined HashLink ABI symbols
    // such as `hl_blocking`.  ash provides those symbols from the statically
    // linked ash_std compatibility layer, but ELF executables do not place
    // their globals in .dynsym unless the final link asks for it.  Without
    // these targeted exports a Linux HDLL follows its DT_NEEDED edge to a
    // stock libhl.so instead, giving one process two runtime states; fmt's
    // first hl_blocking(true) then dereferences the uninitialised stock state.
    //
    // Keep this narrower than --export-dynamic: ash links LLVM statically and
    // exporting every global would needlessly expose a very large symbol set.
    if env::var("CARGO_CFG_TARGET_OS").as_deref() == Ok("linux") {
        println!("cargo:rustc-link-arg=-Wl,--export-dynamic-symbol=hl_*");
        println!("cargo:rustc-link-arg=-Wl,--export-dynamic-symbol=hlt_*");
    }

    // Mach-O HDLLs import the HashLink ABI from a file named libhl.dylib.
    // ash_std is already built separately before ash (the core crate embeds
    // that exact artifact), so keep a compatibility-named copy beside the
    // executable. native_lib selects it whenever the bytecode directory has
    // HDLLs, giving the interpreter and extensions one runtime state.
    if env::var("CARGO_CFG_TARGET_OS").as_deref() == Ok("macos") {
        let manifest = PathBuf::from(env::var_os("CARGO_MANIFEST_DIR").unwrap());
        let target_dir = env::var_os("CARGO_TARGET_DIR")
            .map(PathBuf::from)
            .unwrap_or_else(|| manifest.join("../../target"));
        let target = env::var("TARGET").unwrap();
        let profile = env::var("PROFILE").unwrap();
        let candidates = [
            target_dir
                .join(&target)
                .join(&profile)
                .join("libash_std.dylib"),
            target_dir.join(&profile).join("libash_std.dylib"),
        ];
        for candidate in &candidates {
            println!("cargo:rerun-if-changed={}", candidate.display());
        }
        if let Some(runtime) = candidates.iter().find(|path| path.is_file()) {
            let compat = runtime.parent().unwrap().join("libhl.dylib");
            fs::copy(runtime, &compat).unwrap_or_else(|err| {
                panic!(
                    "could not stage {} as {}: {err}",
                    runtime.display(),
                    compat.display()
                )
            });
        }
    }
}
