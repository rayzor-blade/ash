use std::env;
use std::fs;
use std::path::{Path, PathBuf};

/// See `std/build.rs::pin_libclang` — a libclang newer than the pinned bindgen
/// silently degrades every struct to an opaque blob rather than erroring, so
/// tie bindgen to the same LLVM 21 inkwell already requires.
fn pin_libclang() {
    println!("cargo:rerun-if-env-changed=LIBCLANG_PATH");
    println!("cargo:rerun-if-env-changed=LLVM_SYS_211_PREFIX");

    if env::var_os("LIBCLANG_PATH").is_some() {
        return;
    }
    let Some(prefix) = env::var_os("LLVM_SYS_211_PREFIX") else {
        return;
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
        || fs::read_dir(&libdir).is_ok_and(|entries| {
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

    if !bindings.to_string().contains("pub kind: hl_type_kind") {
        panic!(
            "bindgen produced an opaque `hl_type` (no `kind` field) — the loaded \
             libclang is incompatible with the pinned bindgen release. Set \
             LIBCLANG_PATH to an LLVM 21 lib directory."
        );
    }

    bindings
        .write_to_file(out_dir.join("hl_bindings.rs"))
        .expect("Couldn't write HL bindings");
}

fn main() {
    pin_libclang();
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    let target = env::var("TARGET").unwrap();

    // Generate target-specific HL C bindings in this crate's OUT_DIR.
    generate_hl_bindings(&out_dir);

    // MSVC drops the `lib` prefix on cdylibs: ash_std.dll, not libash_std.dll.
    let lib_filename = if target.contains("windows") {
        "ash_std.dll".to_string()
    } else if target.contains("apple") {
        "libash_std.dylib".to_string()
    } else {
        "libash_std.so".to_string()
    };

    // Where cargo puts the ash_std cdylib depends on whether a target triple
    // was requested. `cargo build` writes <target-dir>/debug/, while
    // `--target <triple>` (or a build.target in .cargo/config.toml) writes
    // <target-dir>/<triple>/debug/. The repo config is platform-neutral and
    // sets no target, so the triple-qualified path only exists when the
    // operator asks for it explicitly. Try both rather than assuming one.
    let target_dir = match env::var_os("CARGO_TARGET_DIR") {
        Some(dir) => PathBuf::from(dir),
        None => env::current_dir().unwrap().join("../../target"),
    };
    // PROFILE is "debug" or "release"; ash_std may only have been built in
    // debug, so fall back to it.
    let profile = env::var("PROFILE").unwrap_or_else(|_| "debug".to_string());
    let mut candidates = vec![
        target_dir.join(&target).join(&profile).join(&lib_filename),
        target_dir.join(&profile).join(&lib_filename),
    ];
    if profile != "debug" {
        candidates.push(target_dir.join(&target).join("debug").join(&lib_filename));
        candidates.push(target_dir.join("debug").join(&lib_filename));
    }

    for c in &candidates {
        println!("cargo:rerun-if-changed={}", c.display());
    }

    let lib_path = candidates.iter().find(|p| p.exists()).unwrap_or_else(|| {
        panic!(
            "Could not find the ash_std cdylib to embed. Build it first:\n\
                 \x20   cargo build -p ash_std\n\
                 Tried:\n{}",
            candidates
                .iter()
                .map(|p| format!("  {}\n", p.display()))
                .collect::<String>()
        )
    });

    // Falling back to a debug cdylib in a release build is legal but almost
    // never intended: the embedded runtime is everything the JIT calls into —
    // the GC, hlp_get_obj_rt, string handling — so a "release" binary built
    // this way runs its whole runtime at the dev profile's opt-level while
    // looking like an optimized build. It has to say so.
    let embedded_profile = if lib_path.components().any(|c| c.as_os_str() == "release") {
        "release"
    } else {
        "debug"
    };
    if profile == "release" && embedded_profile != "release" {
        println!(
            "cargo:warning=embedding a DEBUG ash_std into a release build of ash ({}). \
             The runtime the JIT calls into will not be optimized. Build the runtime \
             first: cargo build --release -p ash_std",
            lib_path.display()
        );
    }

    println!("lib_path {:?}", lib_path);

    // Read the library file into a byte array
    let lib_bytes = fs::read(lib_path).expect("Failed to read cdylib file");

    // Write the byte array to a file in OUT_DIR
    let mut output_path = out_dir.join(lib_filename);
    output_path.set_extension("a");

    fs::write(&output_path, &lib_bytes).expect("Failed to write cdylib binary file");
}
