use std::{env, fs, path::PathBuf};

fn main() {
    // `ASH_VERSION` is set by the release workflow: the tag for a versioned
    // release, a nightly label otherwise. A local build reports the crate's
    // own version.
    println!("cargo:rerun-if-env-changed=ASH_VERSION");
    let version = std::env::var("ASH_VERSION")
        .ok()
        .filter(|v| !v.is_empty())
        .map(|v| v.trim_start_matches('v').to_string())
        .unwrap_or_else(|| std::env::var("CARGO_PKG_VERSION").unwrap());
    println!("cargo:rustc-env=ASH_BUILD_VERSION={version}");

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
        // ash's own entry points too: a native that looks one up in the
        // process (`dlsym(dlopen(NULL))`) must find the runtime the program
        // runs on, and on ELF that is this copy.
        println!("cargo:rustc-link-arg=-Wl,--export-dynamic-symbol=hlp_*");
        // Keep `.text.startup` and friends as their own output sections, so
        // the static constructors of the statically linked LLVM sit together
        // instead of spread across the code; running them at startup then
        // faults in a few pages rather than one page cluster per constructor.
        println!("cargo:rustc-link-arg=-Wl,-z,keep-text-section-prefix");
        // Move LLVM's static constructors out of `.init_array`; ash runs them
        // the first time it needs LLVM (ash_core::llvm_init), so a run that
        // never compiles with LLVM never pays for them.
        if env::var_os("CARGO_FEATURE_LLVM").is_some() {
            let script = PathBuf::from(env::var_os("CARGO_MANIFEST_DIR").unwrap())
                .join("llvm-init.ld");
            println!("cargo:rerun-if-changed={}", script.display());
            println!("cargo:rustc-link-arg=-Wl,-T,{}", script.display());
        }
        // And lay out the functions startup and the first compiles run, in
        // the order they run, so they share pages too. lld only: Rust links
        // x86_64 Linux with it and other Linux targets with the system
        // linker. A name the build no longer has is skipped. Regenerate with
        // scripts/startup_order.py.
        if env::var("TARGET").as_deref() == Ok("x86_64-unknown-linux-gnu") {
            let order = PathBuf::from(env::var_os("CARGO_MANIFEST_DIR").unwrap())
                .join("startup-order.txt");
            println!("cargo:rerun-if-changed={}", order.display());
            println!(
                "cargo:rustc-link-arg=-Wl,--symbol-ordering-file={}",
                order.display()
            );
            println!("cargo:rustc-link-arg=-Wl,--no-warn-symbol-ordering");
        }
        // Pack the relative relocations (DT_RELR): the loader reads every
        // entry of the unpacked table at startup, megabytes of it with LLVM
        // linked in. Needs glibc 2.36 to run, so only when this machine's
        // glibc has it -- a binary built here needs this glibc's symbol
        // versions anyway -- and never when cross-compiling.
        if env::var("TARGET") == env::var("HOST") && host_glibc_at_least(2, 36) {
            println!("cargo:rustc-link-arg=-Wl,-z,pack-relative-relocs");
        }
    }

    // On Mach-O the runtime a program with HDLLs runs on is the libhl.dylib
    // ash loads, and this executable's linked-in copy is never started. A
    // native that looks a runtime symbol up in the process searches the
    // executable first, so the copy here must not answer: unexported, the
    // lookup reaches libhl.dylib. ash itself resolves these by address.
    if env::var("CARGO_CFG_TARGET_OS").as_deref() == Ok("macos") {
        for pattern in ["_hl_*", "_hlp_*", "_hlt_*"] {
            println!("cargo:rustc-link-arg=-Wl,-unexported_symbol,{pattern}");
        }
    }

    // PE HDLLs import the HashLink ABI from a DLL named libhl.dll -- the name
    // is baked into their import table, and a Windows loader binds it by that
    // name at load time. Keep a compatibility-named copy of the runtime beside
    // the executable, which is the first directory the loader searches, so an
    // hdll dropped next to the bytecode finds ash's runtime and not some other
    // HashLink install.
    if env::var("CARGO_CFG_TARGET_OS").as_deref() == Ok("windows") {
        let manifest = PathBuf::from(env::var_os("CARGO_MANIFEST_DIR").unwrap());
        let target_dir = env::var_os("CARGO_TARGET_DIR")
            .map(PathBuf::from)
            .unwrap_or_else(|| manifest.join("../../target"));
        let target = env::var("TARGET").unwrap();
        let profile = env::var("PROFILE").unwrap();
        let candidates = [
            target_dir.join(&target).join(&profile).join("ash_std.dll"),
            target_dir.join(&profile).join("ash_std.dll"),
        ];
        for candidate in &candidates {
            println!("cargo:rerun-if-changed={}", candidate.display());
        }
        if let Some(runtime) = candidates.iter().find(|path| path.is_file()) {
            let runtime_dir = runtime.parent().unwrap();
            // Windows has no symlink to spend here -- creating one needs a
            // privilege an ordinary build does not have -- so both spellings
            // are copies. HashLink 1.x CMake builds name the versioned one.
            for name in ["libhl.dll", "libhl.1.dll"] {
                let compat = runtime_dir.join(name);
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

    // Mach-O HDLLs import the HashLink ABI from a file named libhl.dylib.
    // ash_std is already built separately before ash (the core crate embeds
    // that exact artifact), so keep a compatibility-named copy beside the
    // executable. native_lib selects it whenever the bytecode directory has
    // HDLLs, giving the interpreter and extensions one runtime state.
    if env::var("CARGO_CFG_TARGET_OS").as_deref() == Ok("macos") {
        // Current HashLink HDLLs use @rpath/libhl.1.dylib while older builds
        // use @rpath/libhl.dylib. Resolve both against Ash's sibling runtime;
        // never let dyld find and initialize a second, system HashLink GC.
        println!("cargo:rustc-link-arg=-Wl,-rpath,@executable_path");

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
            let runtime_dir = runtime.parent().unwrap();
            let compat = runtime_dir.join("libhl.dylib");
            // A new file, never an overwrite: macOS caches a binary's code
            // signature by inode, and a signed dylib rewritten in place gets
            // the next process that maps it killed for an invalid page.
            let _ = fs::remove_file(&compat);
            fs::copy(runtime, &compat).unwrap_or_else(|err| {
                panic!(
                    "could not stage {} as {}: {err}",
                    runtime.display(),
                    compat.display()
                )
            });

            // A symlink makes dyld see both dependency spellings as the same
            // image/inode, preserving the one-runtime invariant even when a
            // game mixes HDLLs built by different HashLink releases.
            let versioned = runtime_dir.join("libhl.1.dylib");
            let _ = fs::remove_file(&versioned);
            #[cfg(unix)]
            std::os::unix::fs::symlink("libhl.dylib", &versioned).unwrap_or_else(|err| {
                panic!(
                    "could not stage {} as an alias of {}: {err}",
                    versioned.display(),
                    compat.display()
                )
            });
        }
    }
}

/// Whether this machine's glibc is at least `major.minor`, from
/// `getconf GNU_LIBC_VERSION` ("glibc 2.39"). False without glibc.
fn host_glibc_at_least(major: u32, minor: u32) -> bool {
    let Ok(out) = std::process::Command::new("getconf")
        .arg("GNU_LIBC_VERSION")
        .output()
    else {
        return false;
    };
    let text = String::from_utf8_lossy(&out.stdout);
    let Some(version) = text.trim().strip_prefix("glibc ") else {
        return false;
    };
    let mut parts = version.split('.').map(|p| p.parse::<u32>().unwrap_or(0));
    let found = (parts.next().unwrap_or(0), parts.next().unwrap_or(0));
    found >= (major, minor)
}
