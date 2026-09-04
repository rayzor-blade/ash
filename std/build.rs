use std::env;
use std::path::{Path, PathBuf};

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
    // Match with whitespace removed. bindgen only formats its output when it
    // can run rustfmt, and falls back to raw token spacing (`pub kind :
    // hl_type_kind`) with nothing worse than a warning when it cannot. Matching
    // the formatted spelling therefore turned "rustfmt is not installed" into a
    // confident report that libclang was incompatible -- a two-hour wrong lead,
    // and one that only appears on machines whose toolchain omits the
    // component, which is to say CI and not the machine writing the check.
    let squashed: String = bindings.chars().filter(|c| !c.is_whitespace()).collect();
    if !squashed.contains("pubkind:hl_type_kind") {
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

/// Every prefix on PATH, so a toolchain is found where it was installed
/// rather than where this file guessed.
fn path_prefixes() -> Vec<PathBuf> {
    let Some(path) = env::var_os("PATH") else {
        return Vec::new();
    };
    env::split_paths(&path)
        .filter_map(|dir| dir.parent().map(Path::to_path_buf))
        .collect()
}

/// Ask a compiler where its sysroot is.
///
/// A cross gcc packaged the Debian way answers `/`, which means "no sysroot,
/// use the normal paths" -- and its normal paths are `<prefix>/<triple>`, not
/// the host's. Taking that answer at face value points bindgen at THIS
/// machine's headers, which is worse than finding none: it parses, and the
/// struct layouts it produces are the host's.
fn print_sysroot(program: &str) -> Option<String> {
    let out = std::process::Command::new(program)
        .arg("-print-sysroot")
        .output()
        .ok()?;
    if !out.status.success() {
        return None;
    }
    let path = String::from_utf8_lossy(&out.stdout).trim().to_string();
    if path.is_empty() || path == "/" {
        return None;
    }
    Path::new(&path).is_dir().then_some(path)
}

/// Where `name` is on PATH.
fn on_path(name: &str) -> Option<PathBuf> {
    env::split_paths(&env::var_os("PATH")?).find_map(|dir| {
        let candidate = dir.join(name);
        candidate.is_file().then_some(candidate)
    })
}

/// The sysroot a Debian-style cross toolchain keeps beside itself:
/// `<prefix>/bin/<triple>-gcc` puts its headers in `<prefix>/<triple>`.
///
/// Derived from where the compiler actually is, so it holds for a package
/// manager's `/usr` and for a toolchain unpacked into a home directory
/// alike.
fn sysroot_beside(compiler: &Path, triple: &str) -> Option<String> {
    let prefix = compiler.parent()?.parent()?;
    let sysroot = prefix.join(triple);
    sysroot
        .join("include/stdlib.h")
        .is_file()
        .then(|| sysroot.display().to_string())
}

/// The clang arguments bindgen needs to read a C header written for a machine
/// that is not this one.
///
/// bindgen parses `hl.h` with libclang, and libclang looks for `stdlib.h`
/// where THIS machine keeps it. Cross-compiling, that is the wrong place or
/// no place, and the failure is `fatal error: 'stdlib.h' file not found` from
/// inside a build script, which reads as ash being broken rather than as a
/// toolchain being absent.
///
/// So the sysroot is asked for rather than assumed: the target's own compiler
/// is the thing that knows where its headers are, and naming directories here
/// would be right on one distribution and wrong on the rest.
fn cross_clang_args(target: &str, host: &str) -> Vec<String> {
    if target.is_empty() || target == host {
        return Vec::new();
    }

    if target.starts_with("wasm") {
        println!("cargo:rerun-if-env-changed=WASI_SYSROOT");
        let sysroot = env::var("WASI_SYSROOT").ok().or_else(|| {
            // Under every prefix on PATH, which covers whichever package
            // manager or SDK actually installed one.
            path_prefixes()
                .into_iter()
                .map(|p| p.join("share/wasi-sysroot"))
                .chain(["/opt/wasi-sdk/share/wasi-sysroot".into()])
                .find(|p| p.is_dir())
                .map(|p| p.display().to_string())
        });
        let sysroot = sysroot.unwrap_or_else(|| {
            panic!(
                "building ash_std for {target} needs WASI libc headers. Install a \
                 wasi sysroot (the WASI SDK, or a wasi-libc package) and set \
                 WASI_SYSROOT to it"
            )
        });
        return vec![
            format!("--target={target}"),
            format!("--sysroot={sysroot}"),
            // WASI's `setjmp.h` refuses to be included unless exception
            // handling is on, because that is what its setjmp lowers to.
            // ash's trap model IS setjmp, so neither is optional.
            "-mexception-handling".to_string(),
        ];
    }

    // 32-bit x86 on a 64-bit x86 host is not really a cross build: the same
    // headers serve both, and the compiler only needs telling which word size
    // to read them as.
    if (target.starts_with("i686") || target.starts_with("i586")) && host.starts_with("x86_64") {
        return vec![format!("--target={target}"), "-m32".to_string()];
    }

    // An operator who has already said how to reach the headers is not
    // second-guessed: bindgen reads these itself, so the right move is to add
    // nothing and let it. Panicking here instead ignored a correct answer and
    // demanded it again in a different form.
    for var in [
        format!("BINDGEN_EXTRA_CLANG_ARGS_{target}"),
        format!("BINDGEN_EXTRA_CLANG_ARGS_{}", target.replace('-', "_")),
        "BINDGEN_EXTRA_CLANG_ARGS".to_string(),
    ] {
        if env::var_os(&var).is_some() {
            return Vec::new();
        }
    }

    // Otherwise ask the target's own gcc. Two things separate its name from
    // the Rust triple: Debian drops the vendor component
    // (`aarch64-linux-gnu-gcc` for `aarch64-unknown-linux-gnu`), and Rust's
    // architecture names carry an ISA profile the GNU ones do not --
    // `riscv64gc` is `riscv64` to gcc, `armv7` is `arm`. Without that second
    // rule the search looks for a compiler nobody ships.
    let parts: Vec<&str> = target.split('-').collect();
    let gnu_arch = match parts.first().copied().unwrap_or("") {
        a if a.starts_with("riscv64") => "riscv64",
        a if a.starts_with("riscv32") => "riscv32",
        a if a.starts_with("armv") || a.starts_with("thumbv") => "arm",
        other => other,
    };
    let mut names = vec![format!("{target}-gcc")];
    if parts.len() == 4 {
        names.push(format!("{}-{}-{}-gcc", gnu_arch, parts[2], parts[3]));
        if gnu_arch != parts[0] {
            names.push(format!("{}-{}-{}-gcc", parts[0], parts[2], parts[3]));
        }
    }
    // The gcc triple, which is the Rust one without its vendor component.
    let gcc_triple = if parts.len() == 4 {
        format!("{}-{}-{}", gnu_arch, parts[2], parts[3])
    } else {
        target.to_string()
    };
    for name in &names {
        if let Some(sysroot) = print_sysroot(name) {
            return vec![format!("--target={target}"), format!("--sysroot={sysroot}")];
        }
        if let Some(compiler) = on_path(name) {
            if let Some(sysroot) = sysroot_beside(&compiler, &gcc_triple) {
                return vec![format!("--target={target}"), format!("--sysroot={sysroot}")];
            }
        }
    }

    panic!(
        "building ash_std for {target} needs that target's C headers, and no \
         cross compiler was found to ask where they are (tried {}). Install one \
         -- on Debian and Ubuntu that is the gcc-<target> package -- or set \
         BINDGEN_EXTRA_CLANG_ARGS_{} to the clang arguments that reach them",
        names.join(", "),
        target.replace('-', "_")
    );
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

    // The shared runtime answers to HashLink's name, decided here rather than
    // rewritten later.
    //
    // An HDLL's import table names `@rpath/libhl.dylib` (`libhl.so`), and the
    // loader binds by that name: a binary that linked this library under any
    // other identity ends up in a process with TWO runtimes and two garbage
    // collectors, which crash as soon as one meets the other's objects. The
    // identity is a property of the library, so stamping it at build time is
    // the whole fix -- staging a copy is then `fs::copy` and nothing else. The
    // alternative, patching each copy afterwards with `install_name_tool`,
    // also invalidates its signature and so drags in `codesign` behind it.
    //
    // Loading by an absolute path still works: `dlopen` takes the path it is
    // given, and dyld only uses the install name to notice that an image is
    // already loaded -- which is exactly the deduplication wanted here, since
    // the binary and its HDLLs must share one image.
    if target.contains("apple") {
        println!("cargo:rustc-cdylib-link-arg=-Wl,-install_name,@rpath/libhl.dylib");
    } else if target.contains("linux") || target.contains("bsd") {
        println!("cargo:rustc-cdylib-link-arg=-Wl,-soname,libhl.so");
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

    // A wasm target needs a C library to parse the header against, and the
    // host's will not do: clang stops at `'stdlib.h' file not found`, which
    // reads like a missing libc rather than a missing sysroot. WASI's libc
    // supplies one. `WASI_SYSROOT` names it; otherwise the usual install
    // locations are tried, and a clear failure beats bindings built against
    // the wrong platform.
    let target = env::var("TARGET").unwrap_or_default();
    let host = env::var("HOST").unwrap_or_default();
    let cross_clang_args = cross_clang_args(&target, &host);

    let bindings = bindgen::Builder::default()
        // The input header we would like to generate
        // bindings for.
        .header("wrapper.h")
        .clang_args(&cross_clang_args)
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
