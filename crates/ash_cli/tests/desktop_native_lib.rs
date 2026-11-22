//! A desktop `ash --build` links the libraries it is given as `--native-lib`
//! archives, as a phone build does, instead of loading an HDLL at run time.
//!
//! When the archives stand in for every library the program names, the
//! executable takes the static runtime and has no `.hdll` or `libhl` beside it.
//! A library no archive touches still has to be found as an HDLL.
#![cfg(all(feature = "llvm", unix))]
mod common;
use common::{ash_cli_bin, render_output, repo_root, run_with_timeout, tests_dir};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Duration;

/// `source` compiled with the host C compiler, given `flags`, and archived
/// as `lib<name>.a`.
fn c_archive(dir: &Path, name: &str, source: &Path, flags: &[&str]) -> PathBuf {
    let cc = std::env::var("CC").unwrap_or_else(|_| "cc".into());
    let object = dir.join(format!("{name}.o"));
    let archive = dir.join(format!("lib{name}.a"));
    let compiled = Command::new(cc)
        .args(flags)
        .arg("-c")
        .arg(source)
        .arg("-o")
        .arg(&object)
        .status()
        .expect("no C compiler");
    assert!(compiled.success(), "compiling {}", source.display());
    let archived = Command::new("ar")
        .arg("rcs")
        .arg(&archive)
        .arg(&object)
        .status()
        .expect("no ar");
    assert!(archived.success(), "archiving {name}");
    archive
}

/// `ash --build` of the toy program, for `cross` = (triple, runtime) when
/// the target is not this machine.
fn build(out: &Path, archive: &Path, cross: Option<(&str, &Path)>) -> Result<(), String> {
    let mut cmd = Command::new(ash_cli_bin());
    cmd.arg("--quiet").arg("--build").arg(out);
    if let Some((triple, runtime)) = cross {
        cmd.args(["--target", triple]).arg("--runtime").arg(runtime);
    }
    cmd.arg("--native-lib")
        .arg(archive)
        .arg(tests_dir().join("test_static_native.hl"));
    let result = run_with_timeout(cmd, Duration::from_secs(300));
    if !result.timed_out && result.output.status.success() {
        Ok(())
    } else {
        Err(render_output(&result.output))
    }
}

#[test]
fn a_program_takes_its_library_from_the_archive_and_needs_no_hdll() {
    let dir = tempfile::tempdir().unwrap();
    let toy = tests_dir().join("static_native/toy.c");
    let archive = c_archive(dir.path(), "toy", &toy, &[]);
    let out = dir.path().join("native");
    build(&out, &archive, None).unwrap_or_else(|e| panic!("{e}"));

    // One executable: no HDLL, and no shared runtime staged beside it.
    let beside: Vec<String> = std::fs::read_dir(dir.path())
        .unwrap()
        .map(|e| e.unwrap().file_name().to_string_lossy().into_owned())
        .filter(|n| n.starts_with("libhl") || n.ends_with(".hdll"))
        .collect();
    assert!(beside.is_empty(), "files beside the executable: {beside:?}");

    let run = Command::new(&out).output().unwrap();
    assert!(run.status.success(), "{}", render_output(&run));
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "add=42\nscale=6\nsum=500500\n"
    );

    // A primitive the archive lacks stays unresolved and raises when reached.
    let run = Command::new(&out).arg("x").output().unwrap();
    assert!(
        String::from_utf8_lossy(&run.stderr).contains("exports no 'toy_absent'"),
        "{}",
        render_output(&run)
    );
}

#[test]
fn a_library_no_archive_touches_is_still_required_as_an_hdll() {
    let dir = tempfile::tempdir().unwrap();
    let other = dir.path().join("other.c");
    std::fs::write(
        &other,
        "const void *hlp_other_thing(const char **sign) { *sign = \"_i\"; return 0; }\n",
    )
    .unwrap();
    let archive = c_archive(dir.path(), "other", &other, &[]);
    let err = build(&dir.path().join("native"), &archive, None).unwrap_err();
    assert!(
        err.contains("Required library 'toy' not found"),
        "an archive that defines none of the library's primitives must not stand in for it:\n{err}"
    );
}

/// A macOS program for the other architecture links with Apple's linker from
/// an archive built for that architecture: nothing of it is loaded at run
/// time, and there is no HDLL for a triple the library was never built as.
/// Skipped without that architecture's runtime, which is a separate build:
/// `cargo build --release -p ash_std --target T`.
#[cfg(target_os = "macos")]
#[test]
fn a_macos_program_cross_builds_for_the_other_architecture_from_an_archive() {
    let (triple, arch, cpu_type) = if cfg!(target_arch = "aarch64") {
        ("x86_64-apple-darwin", "x86_64", 0x0100_0007u32)
    } else {
        ("aarch64-apple-darwin", "arm64", 0x0100_000cu32)
    };
    let runtime = repo_root()
        .join("target")
        .join(triple)
        .join("release/libash_std.a");
    if !runtime.is_file() {
        return eprintln!("skipped {triple}: no {}", runtime.display());
    }
    let dir = tempfile::tempdir().unwrap();
    let toy = tests_dir().join("static_native/toy.c");
    let archive = c_archive(dir.path(), "toy", &toy, &["-arch", arch]);
    let out = dir.path().join("native");
    build(&out, &archive, Some((triple, &runtime))).unwrap_or_else(|e| panic!("{e}"));

    let bytes = std::fs::read(&out).unwrap();
    let word = |at: usize| u32::from_le_bytes(bytes[at..at + 4].try_into().unwrap());
    assert_eq!(word(0), 0xfeed_facf, "not a 64-bit Mach-O");
    assert_eq!(word(4), cpu_type, "built for the wrong architecture");
    assert!(
        bytes
            .windows(b"hlp_toy_add".len())
            .any(|w| w == b"hlp_toy_add"),
        "the archive's primitive is not in the executable"
    );
}
