//! Turn the objects the emitter wrote into a native binary.
//!
//! There is no C compiler in this path and no C source. A C driver is used
//! purely because it knows where the platform's startup files and libc live;
//! it is the one subprocess here, because linking is what a linker does.
//! Everything else -- finding the runtime, staging it, naming it -- is done
//! in this file. In particular nothing shells out to `otool`,
//! `install_name_tool` or `codesign`: the runtime is built already carrying
//! the identity an HDLL expects (see `std/build.rs`), so staging a copy is
//! `fs::copy` and no signature is ever invalidated.
//!
//! Which runtime to link is not a preference. A program that loads an HDLL
//! must share one runtime with it: two copies in a process means two garbage
//! collectors, and they crash as soon as one meets the other's objects. So an
//! HDLL program links the runtime dynamically and finds it at `@rpath`, and a
//! program with no HDLL links the archive -- and could not link the shared
//! library anyway, since it references the runtime directly.

use std::path::{Path, PathBuf};
use std::process::Command;

use anyhow::{anyhow, bail, Result};

/// How the runtime is linked, which follows from whether the program loads an
/// HDLL rather than from anyone's choice.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Runtime {
    /// The archive, linked into the binary.
    Static,
    /// The shared library, staged beside the binary under HashLink's name.
    Shared,
}

/// Which command-line dialect the driver speaks. MSVC's differs in every
/// particular: `/Fe:` instead of `-o`, bare `.lib` names instead of `-l`, and
/// no notion of an rpath at all.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Dialect {
    Unix,
    Msvc,
}

struct Driver {
    program: String,
    dialect: Dialect,
}

fn newest(paths: impl IntoIterator<Item = PathBuf>) -> Option<PathBuf> {
    paths
        .into_iter()
        .filter(|p| p.is_file())
        .filter_map(|p| {
            let t = std::fs::metadata(&p).and_then(|m| m.modified()).ok()?;
            Some((t, p))
        })
        .max_by_key(|(t, _)| *t)
        .map(|(_, p)| p)
}

/// What cargo calls the runtime here. MSVC produces `ash_std.lib` and, for the
/// shared build, an import library beside the DLL; everyone else uses the
/// `lib` prefix.
fn runtime_file_names(kind: Runtime) -> Vec<&'static str> {
    match (kind, std::env::consts::OS) {
        (Runtime::Static, "windows") => vec!["ash_std.lib", "libash_std.a"],
        (Runtime::Static, _) => vec!["libash_std.a"],
        (Runtime::Shared, "windows") => vec!["ash_std.dll.lib", "libash_std.dll.a"],
        (Runtime::Shared, "macos") => vec!["libash_std.dylib"],
        (Runtime::Shared, _) => vec!["libash_std.so"],
    }
}

/// Where the runtime may be, in the order a user would expect.
///
/// Beside the executable first: that is where an install puts it, and it is
/// also where cargo puts it, so one rule covers both. Then the usual library
/// prefixes for a system-wide install. No build-tree paths are guessed --
/// somebody compiling a Haxe program has no `target/` directory, and the one
/// case where that layout matters is already covered by "beside the
/// executable".
fn runtime_candidates(kind: Runtime) -> Vec<PathBuf> {
    let mut roots: Vec<PathBuf> = Vec::new();
    if let Ok(exe) = std::env::current_exe() {
        if let Some(dir) = exe.parent() {
            roots.push(dir.to_path_buf());
            roots.push(dir.join("../lib"));
        }
    }
    if cfg!(windows) {
        if let Some(pf) = std::env::var_os("ProgramFiles") {
            roots.push(PathBuf::from(pf).join("ash"));
        }
    } else {
        roots.push(PathBuf::from("/usr/local/lib"));
        roots.push(PathBuf::from("/usr/lib"));
    }
    roots
        .into_iter()
        .flat_map(|r| runtime_file_names(kind).into_iter().map(move |n| r.join(n)))
        .collect()
}

/// Where to keep a runtime unpacked from this binary, so it is written once
/// rather than per build. Beside the binary that will load it, which is what
/// `@rpath` resolves to anyway.
fn embedded_runtime_path(beside: &Path) -> PathBuf {
    let name = match std::env::consts::OS {
        "macos" => "libhl.dylib",
        "windows" => "libhl.dll",
        _ => "libhl.so",
    };
    beside.parent().unwrap_or(Path::new(".")).join(name)
}

/// The runtime to link against.
///
/// For a program that loads HDLLs there is always an answer: this binary
/// carries the shared runtime, so if none is installed it writes its own out
/// beside the executable being linked. The static archive cannot be produced
/// that way -- it is not embedded, because it would double the size of every
/// `ash` -- so a missing one is reported with the command that builds it.
pub fn find_runtime(kind: Runtime) -> Result<PathBuf> {
    if let Some(explicit) = std::env::var_os("ASH_RUNTIME") {
        let p = PathBuf::from(explicit);
        if p.is_file() {
            return Ok(p);
        }
        bail!("ASH_RUNTIME points at {}, which is not a file", p.display());
    }
    newest(runtime_candidates(kind)).ok_or_else(|| {
        anyhow!(
            "no {} runtime found beside {}, or in the usual library directories; \
             build one with `cargo build --release -p ash_std`, or name it with \
             ASH_RUNTIME",
            match kind {
                Runtime::Static => "static",
                Runtime::Shared => "shared",
            },
            std::env::current_exe()
                .ok()
                .and_then(|e| e.parent().map(|d| d.display().to_string()))
                .unwrap_or_else(|| ".".to_string())
        )
    })
}

fn driver() -> Result<Driver> {
    let mut tried: Vec<String> = Vec::new();
    let preferred: Vec<String> = if cfg!(target_os = "windows") {
        vec!["clang-cl".into(), "cl".into(), "clang".into(), "gcc".into()]
    } else {
        vec!["cc".into(), "clang".into(), "gcc".into()]
    };
    for candidate in std::env::var("CC").ok().into_iter().chain(preferred) {
        if candidate.is_empty() {
            continue;
        }
        let msvc_style = candidate.ends_with("cl") && !candidate.ends_with("clang-cl")
            || candidate.ends_with("clang-cl");
        // `cl` has no --version; it prints its banner and fails on no input.
        let probe = if msvc_style { "/?" } else { "--version" };
        let ok = Command::new(&candidate)
            .arg(probe)
            .output()
            .map(|o| o.status.success())
            .unwrap_or(false);
        if ok {
            return Ok(Driver {
                program: candidate,
                dialect: if msvc_style {
                    Dialect::Msvc
                } else {
                    Dialect::Unix
                },
            });
        }
        tried.push(candidate);
    }
    bail!(
        "no C driver found (tried {}); set CC to one",
        tried.join(", ")
    )
}

/// The system libraries a Rust staticlib needs, and the search paths an HDLL
/// program needs.
///
/// An HDLL imports the runtime by HashLink's name, so a binary that loads one
/// needs an rpath pointing at its own directory. Without it `dlopen` refuses
/// the HDLL with "no LC_RPATH's found", which reads as the HDLL being missing
/// when it is sitting right there. A binary with no HDLL is unaffected: an
/// rpath nobody consults costs nothing. Windows has no rpath and needs none --
/// the loader searches the executable's own directory first.
///
/// `ASH_LINK_ARGS` is appended verbatim, for a platform whose system libraries
/// have moved since this was written.
fn platform_args(dialect: Dialect) -> Vec<String> {
    let base: &[&str] = match (dialect, std::env::consts::OS) {
        (Dialect::Msvc, _) => &[
            "kernel32.lib",
            "advapi32.lib",
            "bcrypt.lib",
            "ntdll.lib",
            "userenv.lib",
            "ws2_32.lib",
            "synchronization.lib",
            "dbghelp.lib",
            "legacy_stdio_definitions.lib",
        ],
        (Dialect::Unix, "windows") => &[
            "-lws2_32",
            "-luserenv",
            "-lbcrypt",
            "-lntdll",
            "-ladvapi32",
            "-lkernel32",
            "-ldbghelp",
        ],
        (Dialect::Unix, "macos") => &[
            "-framework",
            "CoreFoundation",
            "-framework",
            "Security",
            "-liconv",
            "-lm",
            "-Wl,-rpath,@executable_path",
            "-Wl,-rpath,@loader_path",
        ],
        (Dialect::Unix, _) => &[
            "-lpthread",
            "-ldl",
            "-lm",
            "-Wl,-rpath,$ORIGIN",
            "-Wl,--export-dynamic",
        ],
    };
    let mut args: Vec<String> = base.iter().map(|s| (*s).to_string()).collect();
    if let Ok(extra) = std::env::var("ASH_LINK_ARGS") {
        args.extend(extra.split_whitespace().map(str::to_string));
    }
    args
}

fn run(command: &mut Command, what: &str) -> Result<()> {
    let output = command
        .output()
        .map_err(|e| anyhow!("could not run {what}: {e}"))?;
    if !output.status.success() {
        bail!(
            "{what} failed ({}):\n{}{}",
            output.status,
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }
    Ok(())
}

/// The names an HDLL may import the runtime by, on this platform.
///
/// Both are needed on Unix: upstream HDLLs link the versioned name, ash's own
/// `sdl.hdll` links the bare one.
fn staged_runtime_names() -> &'static [&'static str] {
    match std::env::consts::OS {
        "macos" => &["libhl.dylib", "libhl.1.dylib"],
        "windows" => &["libhl.dll", "libhl.1.dll"],
        _ => &["libhl.so", "libhl.so.1"],
    }
}

/// Copy the runtime beside the binary under the names an HDLL may import.
///
/// A copy, not a symlink: creating one on Windows needs Developer Mode or
/// administrator rights. Nothing is patched afterwards, because the library
/// was built carrying `@rpath/libhl.dylib` (`libhl.so`) as its own identity,
/// so every copy already answers to the name the loader will ask for, and
/// dyld treats them as the one image they are.
fn stage_runtime(runtime: &Path, beside: &Path, quiet: bool) -> Result<()> {
    // On Windows the linker was given an import library; the library to stage
    // is the DLL beside it.
    let source = if cfg!(target_os = "windows") {
        let dll = runtime.with_extension("").with_extension("dll");
        if dll.is_file() {
            dll
        } else {
            runtime.to_path_buf()
        }
    } else {
        runtime.to_path_buf()
    };
    let dir = beside.parent().unwrap_or(Path::new("."));
    for name in staged_runtime_names() {
        let dest = dir.join(name);
        if dest == source {
            continue;
        }
        std::fs::copy(&source, &dest)
            .map_err(|e| anyhow!("stage {} beside the binary: {e}", dest.display()))?;
    }
    if !quiet {
        eprintln!(
            "\n[ash] staged {} beside the binary as {}",
            source.display(),
            staged_runtime_names().join(" and ")
        );
    }
    Ok(())
}

/// Link `objects` into the executable `out`.
///
/// With [`Runtime::Shared`] the runtime is also staged beside `out`, so the
/// result runs from its own directory with the HDLLs next to it and nothing
/// else to arrange.

/// Whether this triple wants the wasm linker rather than a C driver.
pub fn is_wasm_triple(triple: &str) -> bool {
    let lower = triple.to_ascii_lowercase();
    lower.starts_with("wasm32") || lower.starts_with("wasm64")
}

/// A wasm link needs three things a native link does not, and each can be
/// missing on its own: a linker that speaks wasm, a libc built for it, and an
/// ash runtime compiled for it. They are looked up separately so the error
/// says which one is absent.
/// The ash runtime built for wasm: one relocatable object holding `ash_std`,
/// a wasi libc and `libsetjmp`, joined when ash was built.
///
/// Same places as the native runtime, plus a subdirectory named for the
/// target, because a machine that builds for both has two files that mean the
/// same thing.
pub fn find_wasm_runtime(triple: &str) -> Result<PathBuf> {
    if let Some(explicit) = std::env::var_os("ASH_RUNTIME") {
        let p = PathBuf::from(explicit);
        if p.is_file() {
            return Ok(p);
        }
        bail!("ASH_RUNTIME points at {}, which is not a file", p.display());
    }
    let mut roots: Vec<PathBuf> = Vec::new();
    if let Ok(exe) = std::env::current_exe() {
        if let Some(dir) = exe.parent() {
            roots.push(dir.join(triple));
            roots.push(dir.to_path_buf());
            roots.push(dir.join("../lib").join(triple));
            roots.push(dir.join("../lib"));
        }
    }
    roots.push(PathBuf::from("/usr/local/lib").join(triple));
    roots.push(PathBuf::from("/usr/lib").join(triple));
    newest(roots.into_iter().map(|r| r.join(RUNTIME_OBJECT))).ok_or_else(|| {
        anyhow!(
            "no {RUNTIME_OBJECT} for {triple} found beside {}, or in the usual library \
             directories. It is built when ash is; put it in a directory named {triple} \
             beside ash, or name it with ASH_RUNTIME",
            std::env::current_exe()
                .ok()
                .and_then(|e| e.parent().map(|d| d.display().to_string()))
                .unwrap_or_else(|| ".".to_string())
        )
    })
}

/// What the prelinked wasm runtime is called.
const RUNTIME_OBJECT: &str = "ash_runtime.o";

/// Link a wasm module.
///
/// No linker is spawned and nothing is searched for on the machine: ash
/// carries its own linker (`ash_wasm_link`) and links the program against one
/// runtime object. That is the whole reason the linker exists -- a Haxe
/// developer should not have to install LLVM or a wasi-sdk to build.
///
/// The result is a library rather than a command: it exports `main` and
/// `ash_module_init` and imports what only a host can answer, because a wasm
/// ash program cannot start itself. It has no fibers until a host lends it
/// suspension, and no sockets until a host lends it those.
fn link_wasm_module(
    objects: &[PathBuf],
    out: &Path,
    triple: &str,
    kind: Runtime,
    runtime: Option<&Path>,
    quiet: bool,
) -> Result<()> {
    // A program that names an HDLL is not refused. There is no dlopen in the
    // sandbox and no second module to open, so its primitives resolve to the
    // stub that raises when one is REACHED -- which is the same treatment a
    // native binary gives a primitive whose library is missing, and it lets a
    // program that merely mentions `haxe.Http` run.
    //
    // Saying so is still worth it: the failure, if it comes, arrives at a call
    // deep in a library rather than here.
    if kind == Runtime::Shared && !quiet {
        crate::progress::note(&format!(
            "[ash] this program names HDLLs and {triple} cannot load one. It will \
             build and run; a primitive that is actually reached raises there."
        ));
    }
    let runtime = match runtime {
        Some(p) if p.is_file() => p.to_path_buf(),
        Some(p) => bail!("runtime {} does not exist", p.display()),
        None => find_wasm_runtime(triple)?,
    };

    crate::progress::begin("linking", 0);
    let mut inputs = Vec::with_capacity(objects.len() + 1);
    for path in objects.iter().chain(std::iter::once(&runtime)) {
        let bytes = std::fs::read(path).map_err(|e| anyhow!("reading {}: {e}", path.display()))?;
        let name = path
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_else(|| path.display().to_string());
        inputs.push(ash_wasm_link::read(&name, &bytes)?);
    }
    let module = ash_wasm_link::link(inputs, &ash_wasm_link::LinkOptions::default())?;
    std::fs::write(out, &module).map_err(|e| anyhow!("writing {}: {e}", out.display()))?;

    if !quiet {
        eprintln!(
            "[ash] linked {} ({} bytes) against {}",
            out.display(),
            module.len(),
            runtime.display()
        );
        crate::progress::note(
            "[ash] this is a wasm module, not a command: it exports `main` and \
             `ash_module_init`, and imports WASI plus the two things a sandbox \
             cannot do for itself, suspending a fiber and opening a socket. \
             Whatever embeds it supplies those.",
        );
    }
    Ok(())
}

pub fn link_executable(
    objects: &[PathBuf],
    out: &Path,
    triple: &str,
    kind: Runtime,
    runtime: Option<&Path>,
    quiet: bool,
) -> Result<()> {
    if objects.is_empty() {
        bail!("nothing to link");
    }
    if is_wasm_triple(triple) {
        return link_wasm_module(objects, out, triple, kind, runtime, quiet);
    }
    let runtime = match (runtime, kind) {
        (Some(p), _) if p.is_file() => p.to_path_buf(),
        (Some(p), _) => bail!("runtime {} does not exist", p.display()),
        (None, Runtime::Static) => find_runtime(kind)?,
        // Nothing to install and nothing to build: this binary carries the
        // shared runtime, so if the machine has none, unpack ours.
        (None, Runtime::Shared) => match find_runtime(kind) {
            Ok(found) => found,
            Err(_) => {
                let dest = embedded_runtime_path(out);
                crate::native_lib::write_embedded_runtime(&dest).map_err(|e| {
                    anyhow!(
                        "unpack the runtime this binary carries to {}: {e}",
                        dest.display()
                    )
                })?;
                if !quiet {
                    crate::progress::note(&format!(
                        "[ash] no runtime installed; wrote the embedded one to {}",
                        dest.display()
                    ));
                }
                dest
            }
        },
    };
    let driver = driver()?;

    // On Windows an executable names the DLL it imports from, and that name
    // comes from the import library it was linked against. An HDLL's imports
    // name `libhl.dll`, so linking against `ash_std.dll.lib` would put two
    // runtimes in the process -- the very thing the shared runtime exists to
    // prevent. Say so rather than produce it.
    if kind == Runtime::Shared
        && cfg!(target_os = "windows")
        && !runtime
            .file_name()
            .map(|n| n.to_string_lossy().starts_with("libhl"))
            .unwrap_or(false)
    {
        bail!(
            "on Windows an HDLL program must link an import library for libhl.dll, \
             not {}: the executable would import the runtime under a second name and \
             the process would hold two collectors. Stage the runtime as libhl.dll, \
             generate its import library, and point ASH_RUNTIME at it.",
            runtime.display()
        );
    }

    crate::progress::begin("linking", 0);
    let mut cmd = Command::new(&driver.program);
    cmd.args(objects).arg(&runtime);
    match driver.dialect {
        Dialect::Unix => {
            cmd.arg("-o").arg(out);
        }
        Dialect::Msvc => {
            cmd.arg(format!("/Fe:{}", out.display()));
        }
    }
    cmd.args(platform_args(driver.dialect));
    run(&mut cmd, &format!("{} (link)", driver.program))?;

    if kind == Runtime::Shared {
        stage_runtime(&runtime, out, quiet)?;
    }
    if !quiet {
        let bytes = std::fs::metadata(out).map(|m| m.len()).unwrap_or(0);
        eprintln!(
            "[ash] linked {} ({bytes} bytes) against {}",
            out.display(),
            runtime.display()
        );
    }
    Ok(())
}
