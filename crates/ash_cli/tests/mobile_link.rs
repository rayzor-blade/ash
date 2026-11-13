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
    let mut cmd = Command::new(ash_cli_bin());
    cmd.arg("--build")
        .arg(out)
        .args(["--target", triple])
        .arg("--runtime")
        .arg(runtime)
        .arg(tests_dir().join("test_basic.hl"))
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

#[cfg(target_os = "macos")]
#[test]
fn an_ios_simulator_program_links_and_says_it_is_for_the_simulator() {
    let Some(runtime) = runtime("aarch64-apple-ios-sim") else {
        return;
    };
    let dir = tempfile::tempdir().unwrap();
    let out = dir.path().join("basic");
    match build("aarch64-apple-ios-sim", &runtime, &out) {
        Ok(()) => {}
        Err(e) if e.contains("xcrun") => return eprintln!("skipped: no Xcode\n{e}"),
        Err(e) => panic!("{e}"),
    }
    const IOS_SIMULATOR: u32 = 7;
    let bytes = std::fs::read(&out).unwrap();
    assert_eq!(macho_platform(&bytes), Some(IOS_SIMULATOR));
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
