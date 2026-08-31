use std::env;
use std::path::PathBuf;

/// Pin bindgen to the same LLVM the crate graph already requires.
///
/// bindgen loads whatever `libclang` clang-sys finds first, and clang-sys
/// prefers the HIGHEST version installed. A libclang newer than the bindgen
/// release does not fail — it silently degrades every struct to an opaque
/// `{ _address: u8 }` plus a size assertion, which surfaces hundreds of
/// downstream `no field ...` / E0080 errors with nothing pointing at the real
/// cause. (Observed on Ubuntu with libclang-21 and libclang-22 co-installed:
/// bindgen 0.70 + libclang-22 produced 738 errors; pinning to 21 produced 5.)
///
/// inkwell already demands LLVM 21 via LLVM_SYS_211_PREFIX, so reuse it and
/// keep the two halves of the build on one toolchain.
pub fn pin_libclang() {
    println!("cargo:rerun-if-env-changed=LIBCLANG_PATH");
    println!("cargo:rerun-if-env-changed=LLVM_SYS_211_PREFIX");

    if env::var_os("LIBCLANG_PATH").is_some() {
        return; // explicit operator override wins
    }
    let Some(prefix) = env::var_os("LLVM_SYS_211_PREFIX") else {
        return; // not set: leave clang-sys to its default search
    };
    let prefix = PathBuf::from(prefix);
    // Windows LLVM ships libclang.dll in bin\, everything else uses lib/.
    let libdir = if cfg!(windows) && prefix.join("bin").join("libclang.dll").exists() {
        prefix.join("bin")
    } else {
        prefix.join("lib")
    };
    let has_libclang = ["libclang.so", "libclang.dylib", "libclang.dll"]
        .iter()
        .any(|n| libdir.join(n).exists())
        || std::fs::read_dir(&libdir).is_ok_and(|entries| {
            entries.flatten().any(|e| {
                e.file_name()
                    .to_string_lossy()
                    .starts_with("libclang-21.so")
            })
        });
    if has_libclang {
        #[allow(unused_unsafe)]
        unsafe {
            env::set_var("LIBCLANG_PATH", &libdir);
        }
    }
}

/// Fail loudly if bindgen degraded the core HL types to opaque blobs.
/// Without this the failure mode is hundreds of unrelated-looking type errors
/// in files nobody touched.
pub fn assert_bindings_usable(bindings: &str) {
    if !bindings.contains("pub kind: hl_type_kind") {
        panic!(
            "bindgen produced an opaque `hl_type` (no `kind` field).\n\
             This means the loaded libclang is incompatible with the pinned \
             bindgen release — bindgen degrades structs to `{{ _address: u8 }}` \
             instead of erroring.\n\
             Set LIBCLANG_PATH to an LLVM 21 lib directory, e.g.\n\
             \x20   export LIBCLANG_PATH=/usr/lib/llvm-21/lib   (Linux)\n\
             \x20   export LIBCLANG_PATH=/opt/homebrew/opt/llvm/lib   (macOS)"
        );
    }
}

fn main() {
    pin_libclang();

    let target = env::var("TARGET").unwrap_or_default();
    if target.starts_with("x86_64") && target.contains("linux") {
        cc::Build::new()
            .file("src/stack_boundary.c")
            .flag_if_supported("-fno-omit-frame-pointer")
            .flag_if_supported("-fno-optimize-sibling-calls")
            .warnings(true)
            .compile("ash_std_stack_boundary");
        println!("cargo:rerun-if-changed=src/stack_boundary.c");
    }

    // // Tell cargo to look for shared libraries in the specified directory
    // println!("cargo:rustc-link-search=/path/to/lib");

    // // Tell cargo to tell rustc to link the system bzip2
    // // shared library.
    // println!("cargo:rustc-link-lib=bz2");

    // The bindgen::Builder is the main entry point
    // to bindgen, and lets you build up options for
    // the resulting bindings.
    // Layout tests only make sense when the headers being parsed are the
    // target's own. On a native build they are a real safety net — they catch
    // bindgen mis-sizing an HL struct, which the whole ABI leans on. But the
    // Windows cross-check from macOS has no Windows SDK headers, so it lends
    // bindgen the darwin ones (BINDGEN_EXTRA_CLANG_ARGS=--target=...-apple-
    // darwin) and every assertion then measures a darwin struct under MSVC
    // layout rules: 15 guaranteed E0080s about pthread_t and __darwin_*
    // types that exist on no Windows machine. Host != target is exactly the
    // condition under which the headers are borrowed, so it is the gate.
    //
    // Borrowing them is not automatic: without the variable, clang takes the
    // Windows triple, finds no sysroot, and stops at `'stdlib.h' file not
    // found` -- which reads like a missing libc rather than a missing SDK.
    // From macOS the cross-check is:
    //
    //     BINDGEN_EXTRA_CLANG_ARGS="--target=aarch64-apple-darwin \
    //       -isysroot $(xcrun --show-sdk-path)" \
    //       cargo check --target x86_64-pc-windows-msvc -p ash_std
    //
    // CI does not need it: the windows-check job builds natively on a
    // windows-2022 runner, where the headers are the target's own.
    let cross = env::var("HOST").unwrap_or_default() != env::var("TARGET").unwrap_or_default();

    let bindings = bindgen::Builder::default()
        // The input header we would like to generate
        // bindings for.
        .header("wrapper.h")
        .layout_tests(!cross)
        // Tell cargo to invalidate the built crate whenever any of the
        // included header files changed.
        .parse_callbacks(Box::new(bindgen::CargoCallbacks::new()))
        // Finish the builder and generate the bindings.
        .generate()
        // Unwrap the Result and panic on failure.
        .expect("Unable to generate bindings");

    assert_bindings_usable(&bindings.to_string());

    // Write the bindings to the $OUT_DIR/bindings.rs file.
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("hl_bindings.rs"))
        .expect("Couldn't write bindings!");
}
