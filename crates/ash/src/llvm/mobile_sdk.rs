//! Finding the SDK a mobile link needs, where the platform's own tools put it.
//!
//! A developer who has Xcode, or the Android SDK with an NDK, has everything a
//! link needs; nothing here asks for a variable to be set. Each variable that
//! is read only overrides what was found: `ASH_LINKER_<triple>` and
//! `ASH_ANDROID_API`, plus the names the Android tooling already uses for the
//! NDK.

use std::ffi::OsString;
use std::path::{Path, PathBuf};
use std::process::Command;

use anyhow::{Result, anyhow, bail};

/// The operating system a mobile triple names, or `None` for any other.
///
/// Android is tested first: its triples contain `linux`.
pub(crate) fn os(triple: &str) -> Option<&'static str> {
    if triple.contains("android") {
        Some("android")
    } else if triple.contains("apple-ios") {
        Some("ios")
    } else {
        None
    }
}

/// A C driver that links for a mobile target: the program, then the arguments
/// that point it at the target and its SDK.
pub(crate) struct MobileDriver {
    pub program: String,
    pub args: Vec<String>,
}

pub(crate) fn driver(triple: &str) -> Result<MobileDriver> {
    match os(triple) {
        Some("ios") => ios_driver(triple),
        Some(_) => android_driver(triple),
        None => bail!("{triple} is not a mobile target"),
    }
}

// ---- iOS ----------------------------------------------------------------

/// What clang calls an iOS Rust triple, and which SDK it links against.
#[derive(Debug, PartialEq, Eq)]
pub(crate) struct IosTarget {
    pub clang: String,
    pub sdk: &'static str,
}

/// `aarch64-apple-ios` is a device; `aarch64-apple-ios-sim` and
/// `x86_64-apple-ios` are the simulator. They are different outputs: the
/// object records the platform it was built for, and the linker refuses to
/// treat one as the other.
pub(crate) fn ios_target(triple: &str) -> Result<IosTarget> {
    if triple.contains("macabi") {
        bail!("{triple} is Mac Catalyst, which is not an iOS target");
    }
    let arch = match triple.split('-').next().unwrap_or("") {
        "aarch64" | "arm64" => "arm64",
        "x86_64" => "x86_64",
        other => bail!("no iOS target for the architecture `{other}` in {triple}"),
    };
    let simulator = triple.ends_with("-sim") || triple.contains("simulator") || arch == "x86_64";
    Ok(if simulator {
        IosTarget {
            clang: format!("{arch}-apple-ios-simulator"),
            sdk: "iphonesimulator",
        }
    } else {
        IosTarget {
            clang: format!("{arch}-apple-ios"),
            sdk: "iphoneos",
        }
    })
}

/// The oldest iOS an object is built for when nothing says otherwise. Arm64
/// simulators begin at 14.0, so a lower number would be raised to it anyway.
const DEFAULT_IOS_VERSION: &str = "14.0";

/// The triple LLVM is given for an iOS target. Rust's `-sim` suffix is not an
/// environment LLVM knows, so a simulator object written under that name
/// carries no platform tag and is indistinguishable from a device one. This
/// spelling makes the object say which it is, and for what minimum version.
pub(crate) fn ios_llvm_triple(triple: &str) -> Option<String> {
    if os(triple) != Some("ios") {
        return None;
    }
    let target = ios_target(triple).ok()?;
    let version = std::env::var("IPHONEOS_DEPLOYMENT_TARGET")
        .ok()
        .filter(|v| !v.is_empty())
        .unwrap_or_else(|| DEFAULT_IOS_VERSION.to_string());
    Some(match target.clang.strip_suffix("-simulator") {
        Some(base) => base
            .replacen("-ios", &format!("-ios{version}"), 1)
            .to_string()
            + "-simulator",
        None => target.clang.replacen("-ios", &format!("-ios{version}"), 1),
    })
}

fn xcrun(args: &[&str]) -> Result<String> {
    let out = Command::new("xcrun").args(args).output().map_err(|e| {
        anyhow!(
            "building for iOS needs Xcode on a Mac, and `xcrun` could not be run: {e}"
        )
    })?;
    let text = String::from_utf8_lossy(&out.stdout).trim().to_string();
    if !out.status.success() || text.is_empty() {
        bail!(
            "`xcrun {}` found nothing: {}. Install Xcode, open it once, and \
             select it with `sudo xcode-select -s /Applications/Xcode.app`",
            args.join(" "),
            String::from_utf8_lossy(&out.stderr).trim()
        );
    }
    Ok(text)
}

fn ios_driver(triple: &str) -> Result<MobileDriver> {
    let target = ios_target(triple)?;
    let sdk = xcrun(&["--sdk", target.sdk, "--show-sdk-path"])?;
    let clang = xcrun(&["--sdk", target.sdk, "--find", "clang"])?;
    Ok(MobileDriver {
        program: clang,
        args: vec!["-target".into(), target.clang, "-isysroot".into(), sdk],
    })
}

// ---- Android ------------------------------------------------------------

/// The API level the program is linked for when nothing says otherwise. It is
/// the oldest level the graphics stack ash programs use supports.
const DEFAULT_API: u32 = 24;

/// The NDK names a clang wrapper `<triple><api>-clang`, and spells the 32-bit
/// ARM triple differently from Rust.
pub(crate) fn android_clang_triple(triple: &str, api: u32) -> Result<String> {
    let base = match triple.split('-').next().unwrap_or("") {
        "aarch64" => "aarch64-linux-android",
        "x86_64" => "x86_64-linux-android",
        "i686" => "i686-linux-android",
        a if a.starts_with("armv7") || a.starts_with("thumbv7") => "armv7a-linux-androideabi",
        other => bail!("no Android target for the architecture `{other}` in {triple}"),
    };
    Ok(format!("{base}{api}"))
}

fn android_api() -> Result<u32> {
    match std::env::var("ASH_ANDROID_API") {
        Ok(v) if !v.is_empty() => v
            .parse()
            .map_err(|_| anyhow!("ASH_ANDROID_API must be an API level such as 24, not `{v}`")),
        _ => Ok(DEFAULT_API),
    }
}

/// The NDK's own host directory name. The macOS one holds universal binaries,
/// so it is the right name on Apple silicon too.
fn host_tag() -> &'static str {
    if cfg!(windows) {
        "windows-x86_64"
    } else if cfg!(target_os = "macos") {
        "darwin-x86_64"
    } else {
        "linux-x86_64"
    }
}

/// Where Android Studio puts the SDK when nobody chose.
fn default_sdk_roots(home: Option<&Path>) -> Vec<PathBuf> {
    let mut roots = Vec::new();
    if cfg!(windows) {
        if let Some(local) = std::env::var_os("LOCALAPPDATA") {
            roots.push(PathBuf::from(local).join("Android").join("Sdk"));
        }
    } else if let Some(home) = home {
        roots.push(home.join("Library/Android/sdk"));
        roots.push(home.join("Android/Sdk"));
    }
    roots
}

/// `ndk/<version>` directories, newest first, comparing the numbers in the
/// name rather than the text (`29.0.1` is newer than `4.0.1`).
fn versions_newest_first(dir: &Path) -> Vec<PathBuf> {
    let mut found: Vec<(Vec<u64>, PathBuf)> = std::fs::read_dir(dir)
        .into_iter()
        .flatten()
        .flatten()
        .filter(|e| e.path().is_dir())
        .map(|e| {
            let key = e
                .file_name()
                .to_string_lossy()
                .split(|c: char| !c.is_ascii_digit())
                .filter_map(|n| n.parse().ok())
                .collect();
            (key, e.path())
        })
        .collect();
    found.sort_by(|a, b| b.0.cmp(&a.0));
    found.into_iter().map(|(_, p)| p).collect()
}

fn is_ndk(dir: &Path) -> bool {
    dir.join("toolchains/llvm/prebuilt").is_dir()
}

/// The NDK to link with: a variable that names one, then the newest under an
/// SDK the environment names, then the newest under Android Studio's default
/// SDK location.
pub(crate) fn find_ndk(env: &dyn Fn(&str) -> Option<OsString>, home: Option<&Path>) -> Option<PathBuf> {
    let named = ["ANDROID_NDK_HOME", "ANDROID_NDK_ROOT", "NDK_HOME"]
        .iter()
        .filter_map(|v| env(v))
        .map(PathBuf::from);
    let mut sdks: Vec<PathBuf> = ["ANDROID_HOME", "ANDROID_SDK_ROOT"]
        .iter()
        .filter_map(|v| env(v))
        .map(PathBuf::from)
        .collect();
    sdks.extend(default_sdk_roots(home));
    let installed = sdks.into_iter().flat_map(|sdk| {
        let mut found = versions_newest_first(&sdk.join("ndk"));
        found.push(sdk.join("ndk-bundle"));
        found
    });
    named.chain(installed).find(|dir| is_ndk(dir))
}

/// The directory of the NDK's host toolchain, `.../prebuilt/<tag>`. The
/// expected tag first; any other the NDK ships second, which covers a Linux
/// NDK on a machine that names its architecture differently.
fn prebuilt(ndk: &Path) -> Option<PathBuf> {
    let root = ndk.join("toolchains/llvm/prebuilt");
    let expected = root.join(host_tag());
    if expected.is_dir() {
        return Some(expected);
    }
    std::fs::read_dir(&root)
        .ok()?
        .flatten()
        .map(|e| e.path())
        .find(|p| p.is_dir())
}

fn android_driver(triple: &str) -> Result<MobileDriver> {
    let api = android_api()?;
    let clang_triple = android_clang_triple(triple, api)?;
    let home = std::env::var_os("HOME").or_else(|| std::env::var_os("USERPROFILE")).map(PathBuf::from);
    let ndk = find_ndk(&|v| std::env::var_os(v).filter(|s| !s.is_empty()), home.as_deref())
        .ok_or_else(|| {
            anyhow!(
                "no Android NDK found. Install one from Android Studio's SDK Manager \
                 (SDK Tools, NDK), or set ANDROID_NDK_HOME to it"
            )
        })?;
    let tools = prebuilt(&ndk)
        .ok_or_else(|| anyhow!("the NDK at {} has no host toolchain", ndk.display()))?;
    let wrapper = tools.join("bin").join(format!(
        "{clang_triple}-clang{}",
        if cfg!(windows) { ".cmd" } else { "" }
    ));
    if !wrapper.is_file() {
        bail!(
            "the NDK at {} has no {} (API level {api} is not supported by it; \
             name another with ASH_ANDROID_API)",
            ndk.display(),
            wrapper.file_name().unwrap_or_default().to_string_lossy()
        );
    }
    // The wrapper already carries the target and sysroot.
    Ok(MobileDriver {
        program: wrapper.display().to_string(),
        args: Vec::new(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn android_comes_before_linux() {
        assert_eq!(os("aarch64-linux-android"), Some("android"));
        assert_eq!(os("armv7-linux-androideabi"), Some("android"));
        assert_eq!(os("aarch64-apple-ios-sim"), Some("ios"));
        assert_eq!(os("x86_64-unknown-linux-gnu"), None);
        assert_eq!(os("aarch64-apple-darwin"), None);
    }

    #[test]
    fn a_device_and_a_simulator_are_different_targets() {
        let device = ios_target("aarch64-apple-ios").unwrap();
        assert_eq!(device.clang, "arm64-apple-ios");
        assert_eq!(device.sdk, "iphoneos");
        let sim = ios_target("aarch64-apple-ios-sim").unwrap();
        assert_eq!(sim.clang, "arm64-apple-ios-simulator");
        assert_eq!(sim.sdk, "iphonesimulator");
        // Rust's `x86_64-apple-ios` has only ever been the simulator.
        let intel = ios_target("x86_64-apple-ios").unwrap();
        assert_eq!(intel.clang, "x86_64-apple-ios-simulator");
        assert_eq!(intel.sdk, "iphonesimulator");
        assert!(ios_target("aarch64-apple-ios-macabi").is_err());
        assert!(ios_target("riscv64-apple-ios").is_err());
    }

    #[test]
    fn an_ios_object_is_written_under_a_triple_that_names_its_platform() {
        // Not run with a deployment target set by the environment.
        if std::env::var_os("IPHONEOS_DEPLOYMENT_TARGET").is_some() {
            return;
        }
        assert_eq!(
            ios_llvm_triple("aarch64-apple-ios-sim").unwrap(),
            "arm64-apple-ios14.0-simulator"
        );
        assert_eq!(
            ios_llvm_triple("aarch64-apple-ios").unwrap(),
            "arm64-apple-ios14.0"
        );
        assert_eq!(ios_llvm_triple("aarch64-linux-android"), None);
    }

    #[test]
    fn the_ndk_spells_32_bit_arm_its_own_way() {
        assert_eq!(
            android_clang_triple("aarch64-linux-android", 24).unwrap(),
            "aarch64-linux-android24"
        );
        assert_eq!(
            android_clang_triple("armv7-linux-androideabi", 24).unwrap(),
            "armv7a-linux-androideabi24"
        );
        assert_eq!(
            android_clang_triple("thumbv7neon-linux-androideabi", 21).unwrap(),
            "armv7a-linux-androideabi21"
        );
        assert_eq!(
            android_clang_triple("x86_64-linux-android", 30).unwrap(),
            "x86_64-linux-android30"
        );
        assert!(android_clang_triple("riscv64-linux-android", 24).is_err());
    }

    fn fake_ndk(dir: &Path) {
        std::fs::create_dir_all(dir.join("toolchains/llvm/prebuilt/darwin-x86_64/bin")).unwrap();
    }

    #[test]
    fn the_newest_installed_ndk_wins_by_number() {
        let sdk = tempfile::tempdir().unwrap();
        for version in ["4.0.1", "29.0.14206865", "27.0.12077973"] {
            fake_ndk(&sdk.path().join("ndk").join(version));
        }
        let home = sdk.path().to_path_buf();
        let found = find_ndk(
            &|v| (v == "ANDROID_HOME").then(|| sdk.path().into()),
            Some(&home),
        )
        .unwrap();
        assert!(found.ends_with("ndk/29.0.14206865"), "{}", found.display());
    }

    #[test]
    fn a_named_ndk_beats_an_installed_one_and_a_bad_name_is_skipped() {
        let sdk = tempfile::tempdir().unwrap();
        fake_ndk(&sdk.path().join("ndk/29.0.1"));
        let chosen = tempfile::tempdir().unwrap();
        fake_ndk(chosen.path());
        let env = |v: &str| match v {
            "ANDROID_HOME" => Some(sdk.path().into()),
            "ANDROID_NDK_HOME" => Some(chosen.path().into()),
            _ => None,
        };
        assert_eq!(find_ndk(&env, None).unwrap(), chosen.path());
        // A variable that names something that is not an NDK falls through.
        let wrong = |v: &str| match v {
            "ANDROID_HOME" => Some(sdk.path().into()),
            "ANDROID_NDK_HOME" => Some("/nonexistent".into()),
            _ => None,
        };
        assert!(find_ndk(&wrong, None).unwrap().ends_with("ndk/29.0.1"));
        assert!(find_ndk(&|_| None, None).is_none());
    }

    #[test]
    fn the_default_sdk_location_is_searched_without_any_variable() {
        let home = tempfile::tempdir().unwrap();
        let sdk = if cfg!(windows) {
            return;
        } else {
            home.path().join("Library/Android/sdk")
        };
        fake_ndk(&sdk.join("ndk/27.0.1"));
        assert!(
            find_ndk(&|_| None, Some(home.path()))
                .unwrap()
                .ends_with("ndk/27.0.1")
        );
    }
}
