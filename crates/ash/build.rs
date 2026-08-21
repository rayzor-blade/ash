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

    // Same gate as std/build.rs: layout tests only when the headers being
    // parsed are the target's own. The macOS->Windows cross-check borrows
    // darwin headers, and asserting darwin struct sizes under MSVC layout
    // rules yields guaranteed E0080s that assert nothing about ash.
    let cross = env::var("HOST").unwrap_or_default() != env::var("TARGET").unwrap_or_default();

    let bindings = bindgen::Builder::default()
        .header(wrapper.to_string_lossy().into_owned())
        .layout_tests(!cross)
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


/// Emit a name -> address table for every native ash_std exports.
///
/// ash_std is linked into this binary as an rlib, but an rlib is an archive:
/// the linker pulls in only the members that satisfy an undefined symbol, so
/// 380-odd `#[no_mangle]` functions nothing references would simply not be
/// linked. Declaring each one in an `extern "C"` block and taking its address
/// creates that reference, which is what puts the object in the binary and
/// lets `std@` natives resolve without a dlopen.
///
/// The placeholder `fn hlp_x();` signatures are deliberate — the table only
/// ever takes addresses, never calls through them, and the real signature is
/// applied by the caller that already knows it from the bytecode.
fn generate_std_symbol_table(out_dir: &Path) {
    let std_src = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap()).join("../../std/src");

    // build.rs cannot ask rustc whether a cfg holds, but the four predicates
    // that actually guard an export are all decidable from the target.
    let target_os = env::var("CARGO_CFG_TARGET_OS").unwrap_or_default();
    let target_family = env::var("CARGO_CFG_TARGET_FAMILY").unwrap_or_default();
    let target_arch = env::var("CARGO_CFG_TARGET_ARCH").unwrap_or_default();
    let cfg_holds = |pred: &str| -> bool {
        let p = pred.trim();
        if p == "unix" {
            return target_family.split(',').any(|f| f == "unix") || target_os != "windows";
        }
        if p == "windows" {
            return target_os == "windows";
        }
        if let Some(v) = p.strip_prefix("target_arch") {
            return v.trim_start_matches([' ', '=']).trim().trim_matches('"') == target_arch;
        }
        if let Some(v) = p.strip_prefix("target_os") {
            return v.trim_start_matches([' ', '=']).trim().trim_matches('"') == target_os;
        }
        // An unrecognized guard is not silently dropped: including the symbol
        // makes the link fail loudly, which is the outcome that gets noticed.
        true
    };

    let mut names: Vec<String> = Vec::new();
    let mut files: Vec<PathBuf> = fs::read_dir(&std_src)
        .expect("ash_std source directory not found")
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|p| p.extension().is_some_and(|e| e == "rs"))
        .collect();
    files.sort();

    for f in &files {
        println!("cargo:rerun-if-changed={}", f.display());
        let text = fs::read_to_string(f).unwrap_or_default();
        let lines: Vec<&str> = text.lines().collect();
        for (i, line) in lines.iter().enumerate() {
            if line.trim() != "#[no_mangle]" {
                continue;
            }
            // Find the signature line. Other attributes may sit between —
            // #[inline(never)], #[cold] — and those contain parentheses, so
            // the scan skips attribute lines rather than stopping at the
            // first '('. Not every export is `extern "C"` either: a few are
            // plain `pub unsafe fn` carrying #[no_mangle], and they are just
            // as much part of the symbol table.
            let mut sig = String::new();
            for l in lines.iter().skip(i + 1).take(8) {
                let t = l.trim();
                if t.starts_with("#[") || t.starts_with("//") || t.is_empty() {
                    continue;
                }
                sig.push_str(t);
                sig.push(' ');
                if t.contains('(') {
                    break;
                }
            }
            let Some(rest) = sig.split(" fn ").nth(1) else {
                continue; // a #[no_mangle] static, not a function
            };
            let name: String = rest
                .chars()
                .take_while(|c| c.is_alphanumeric() || *c == '_')
                .collect();
            if name.is_empty() {
                continue;
            }
            // Walk back over the attributes attached to this item.
            let mut ok = true;
            for back in lines[..i].iter().rev().take(6) {
                let b = back.trim();
                if let Some(inner) = b.strip_prefix("#[cfg(").and_then(|x| x.strip_suffix(")]")) {
                    if !cfg_holds(inner) {
                        ok = false;
                    }
                } else if !b.starts_with("#[") && !b.starts_with("///") && !b.starts_with("//") {
                    break;
                }
            }
            if ok {
                names.push(name);
            }
        }
    }
    names.sort();
    names.dedup();

    let mut out = String::from("// @generated by build.rs from std/src/*.rs — do not edit.\n");
    out.push_str("extern \"C\" {\n");
    for n in &names {
        out.push_str(&format!("    fn {n}();\n"));
    }
    out.push_str("}\n\n");
    out.push_str(&format!(
        "/// The {} natives ash_std exports, resolvable without a dlopen.\n",
        names.len()
    ));
    out.push_str("pub fn std_symbol_table() -> &'static std::collections::HashMap<&'static str, usize> {\n");
    out.push_str("    static T: std::sync::OnceLock<std::collections::HashMap<&'static str, usize>> =\n");
    out.push_str("        std::sync::OnceLock::new();\n");
    out.push_str("    T.get_or_init(|| {\n");
    out.push_str(&format!(
        "        let mut m = std::collections::HashMap::with_capacity({});\n",
        names.len()
    ));
    for n in &names {
        out.push_str(&format!(
            "        m.insert(\"{n}\", {n} as *const () as usize);\n"
        ));
    }
    out.push_str("        m\n    })\n}\n");

    // An empty table would mean every `std@` native silently failing to
    // resolve, which is worth a hard stop rather than a warning nobody reads.
    assert!(
        names.len() > 100,
        "only {} ash_std natives found — the scanner in build.rs has stopped \
         matching how std/src declares its exports",
        names.len()
    );
    fs::write(out_dir.join("std_symbols.rs"), out).expect("write std_symbols.rs");
}

fn main() {
    pin_libclang();
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    let target = env::var("TARGET").unwrap();

    // Generate target-specific HL C bindings in this crate's OUT_DIR.
    generate_hl_bindings(&out_dir);

    // Static resolution for std@ natives; see the function's own comment.
    generate_std_symbol_table(&out_dir);

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

    // Write the byte array to a file in OUT_DIR. The name is FIXED:
    // native_lib.rs does include_bytes!(concat!(env!("OUT_DIR"),
    // "/libash_std.a")), and deriving the name from the platform's dylib
    // filename broke Windows — MSVC's cdylib is ash_std.dll (no lib prefix),
    // so this wrote ash_std.a and the include failed to resolve.
    let output_path = out_dir.join("libash_std.a");

    fs::write(&output_path, &lib_bytes).expect("Failed to write cdylib binary file");
}
