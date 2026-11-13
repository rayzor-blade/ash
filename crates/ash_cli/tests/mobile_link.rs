//! `ash --build` links a program for a phone with only the platform SDK.
//!
//! No SDK variable is set, and `CC` is set to the host's compiler, which a
//! phone's link must look past. Each case is skipped, and says so, when the
//! SDK or the target's runtime archive is not on the machine: building the
//! archive is a separate step (`cargo build --release -p ash_std --target T`).
#![cfg(feature = "llvm")]
mod common;
use common::{ash_cli_bin, render_output, repo_root, run_with_timeout, tests_dir};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Duration;

fn runtime(triple: &str) -> Option<PathBuf> {
    let path = repo_root().join("target").join(triple).join("release/libash_std.a");
    if path.is_file() {
        Some(path)
    } else {
        eprintln!("skipped {triple}: no {}", path.display());
        None
    }
}

/// `ash --build` for `triple`, the way a developer with only the SDK runs it.
/// `Err` carries the CLI's complaint.
fn build(triple: &str, runtime: &Path, out: &Path) -> Result<(), String> {
    build_program(triple, runtime, out, "test_basic.hl", &[])
}

/// The same build of a named test program, with `extra` arguments before it.
fn build_program(
    triple: &str,
    runtime: &Path,
    out: &Path,
    program: &str,
    extra: &[&Path],
) -> Result<(), String> {
    let mut cmd = Command::new(ash_cli_bin());
    cmd.arg("--build")
        .arg(out)
        .args(["--target", triple])
        .arg("--runtime")
        .arg(runtime);
    for path in extra {
        cmd.arg("--native-lib").arg(path);
    }
    cmd.arg(tests_dir().join(program))
        .env("CC", "/usr/bin/clang");
    for var in [
        "ASH_LINKER",
        "ASH_SYSROOT",
        "ASH_LINK_ARGS",
        "ASH_ANDROID_API",
        "BINDGEN_EXTRA_CLANG_ARGS",
    ] {
        cmd.env_remove(var);
    }
    let result = run_with_timeout(cmd, Duration::from_secs(300));
    if !result.timed_out && result.output.status.success() {
        Ok(())
    } else {
        Err(render_output(&result.output))
    }
}

/// The platform recorded in a 64-bit Mach-O's `LC_BUILD_VERSION`.
fn macho_platform(bytes: &[u8]) -> Option<u32> {
    let word = |at: usize| Some(u32::from_le_bytes(bytes.get(at..at + 4)?.try_into().ok()?));
    if word(0)? != 0xfeed_facf {
        return None;
    }
    let mut at = 32;
    for _ in 0..word(16)? {
        let (cmd, size) = (word(at)?, word(at + 4)? as usize);
        if cmd == 0x32 {
            return word(at + 8);
        }
        at += size;
    }
    None
}

/// Each iOS target links and records the platform it is for: the device is a
/// different output from either simulator.
#[cfg(target_os = "macos")]
#[test]
fn an_ios_program_links_and_says_which_platform_it_is_for() {
    const IOS: u32 = 2;
    const IOS_SIMULATOR: u32 = 7;
    for (triple, platform) in [
        ("aarch64-apple-ios", IOS),
        ("aarch64-apple-ios-sim", IOS_SIMULATOR),
        ("x86_64-apple-ios", IOS_SIMULATOR),
    ] {
        let Some(runtime) = runtime(triple) else {
            continue;
        };
        let dir = tempfile::tempdir().unwrap();
        let out = dir.path().join("basic");
        match build(triple, &runtime, &out) {
            Ok(()) => {}
            Err(e) if e.contains("xcrun") => return eprintln!("skipped: no Xcode\n{e}"),
            Err(e) => panic!("{triple}: {e}"),
        }
        let bytes = std::fs::read(&out).unwrap();
        assert_eq!(macho_platform(&bytes), Some(platform), "{triple}");
    }
}

/// `static_native/toy.c`, compiled for the iOS simulator and archived.
#[cfg(target_os = "macos")]
fn toy_archive(dir: &Path) -> Option<PathBuf> {
    let xcrun = |args: &[&str]| {
        let out = Command::new("xcrun").args(args).output().ok()?;
        out.status.success().then_some(out)
    };
    let sdk = xcrun(&["--sdk", "iphonesimulator", "--show-sdk-path"])?;
    let sdk = String::from_utf8_lossy(&sdk.stdout).trim().to_string();
    let object = dir.join("toy.o");
    let archive = dir.join("libtoy.a");
    let source = tests_dir().join("static_native/toy.c");
    xcrun(&[
        "--sdk",
        "iphonesimulator",
        "clang",
        "-target",
        "arm64-apple-ios14.0-simulator",
        "-isysroot",
        &sdk,
        "-c",
        source.to_str()?,
        "-o",
        object.to_str()?,
    ])?;
    xcrun(&["ar", "rcs", archive.to_str()?, object.to_str()?])?;
    Some(archive)
}

/// A phone app cannot open an HDLL, so the natives it calls come from the
/// archive it links: the program carries the archive's resolvers, and the
/// primitive nothing defines stays unresolved.
#[cfg(target_os = "macos")]
#[test]
fn an_ios_program_links_the_natives_it_calls_from_an_archive() {
    let triple = "aarch64-apple-ios-sim";
    let Some(runtime) = runtime(triple) else {
        return;
    };
    let dir = tempfile::tempdir().unwrap();
    let Some(archive) = toy_archive(dir.path()) else {
        return eprintln!("skipped: no Xcode");
    };
    let out = dir.path().join("native");
    if let Err(e) = build_program(
        triple,
        &runtime,
        &out,
        "test_static_native.hl",
        &[archive.as_path()],
    ) {
        panic!("{e}");
    }
    let nm = Command::new("xcrun")
        .args(["nm", "-gU"])
        .arg(&out)
        .output()
        .unwrap();
    let defined = String::from_utf8_lossy(&nm.stdout);
    assert!(defined.contains("_hlp_toy_add"), "{defined}");
    assert!(defined.contains("_hlp_toy_scale"), "{defined}");
    assert!(!defined.contains("_hlp_toy_absent"), "{defined}");
}

/// The release makes the runtime's Rust-internal symbols local, so it links
/// beside another Rust static library, which carries its own `std`. Apple's
/// linker cannot be told to take a duplicate definition, so the runtime must
/// not export them; the program still links against the edited archive.
#[cfg(target_os = "macos")]
#[test]
fn a_runtime_with_local_rust_internals_still_links_a_program() {
    let triple = "aarch64-apple-ios-sim";
    let Some(runtime) = runtime(triple) else {
        return;
    };
    let dir = tempfile::tempdir().unwrap();
    let copy = dir.path().join("libash_std.a");
    std::fs::copy(&runtime, &copy).unwrap();
    let script = repo_root().join("scripts/localize_runtime.sh");
    let edited = Command::new(&script).arg(&copy).arg("ios").output().unwrap();
    if !edited.status.success() {
        let why = String::from_utf8_lossy(&edited.stderr);
        if why.contains("xcrun") {
            return eprintln!("skipped: no Xcode\n{why}");
        }
        panic!("{why}");
    }
    let nm = Command::new("xcrun")
        .args(["nm", "-g", "--defined-only"])
        .arg(&copy)
        .output()
        .unwrap();
    let exported = String::from_utf8_lossy(&nm.stdout);
    assert!(!exported.contains("_rust_eh_personality"), "still exported");
    assert!(!exported.contains("EMPTY_PANIC"), "still exported");
    let out = dir.path().join("basic");
    if let Err(e) = build(triple, &copy, &out) {
        panic!("{e}");
    }
}

#[test]
fn an_android_program_links_with_only_the_ndk() {
    let Some(runtime) = runtime("aarch64-linux-android") else {
        return;
    };
    let dir = tempfile::tempdir().unwrap();
    let out = dir.path().join("basic");
    match build("aarch64-linux-android", &runtime, &out) {
        Ok(()) => {}
        Err(e) if e.contains("no Android NDK found") => return eprintln!("skipped: no NDK"),
        Err(e) => panic!("{e}"),
    }
    const EM_AARCH64: u16 = 183;
    let bytes = std::fs::read(&out).unwrap();
    assert_eq!(&bytes[..4], b"\x7fELF");
    assert_eq!(bytes[4], 2, "64-bit");
    assert_eq!(u16::from_le_bytes([bytes[18], bytes[19]]), EM_AARCH64);
}

#[test]
fn a_macho_without_a_build_version_has_no_platform() {
    assert_eq!(macho_platform(b"not a macho"), None);
}
