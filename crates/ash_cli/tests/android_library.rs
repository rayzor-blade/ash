//! `ash --build libmain.so` for an Android target writes the shared library a
//! NativeActivity app loads.
//!
//! The window host is the NDK's own `native_app_glue`, which defines
//! `ANativeActivity_onCreate`, with an `android_main` that calls the program's
//! `xwindow_main`. The library must export what Android looks up by name and
//! nothing else, and name the platform libraries it needs.
//!
//! Skipped without an NDK and an Android runtime, which is a separate build:
//! `cargo rustc -p ash_std --target T --release --crate-type staticlib`.
//! `ASH_ANDROID_TEST=required` makes that a failure.
#![cfg(all(feature = "llvm", unix))]
mod common;
use common::{ash_cli_bin, render_output, repo_root, run_with_timeout, tests_dir};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Duration;

/// The NDK's host tools, a target it can compile for and that target's runtime.
struct Android {
    tools: PathBuf,
    ndk: PathBuf,
    triple: &'static str,
    runtime: PathBuf,
}

fn ndk() -> Option<PathBuf> {
    let var = |name: &str| {
        std::env::var_os(name)
            .filter(|v| !v.is_empty())
            .map(PathBuf::from)
    };
    let home = var("HOME");
    let sdks = [
        var("ANDROID_HOME"),
        var("ANDROID_SDK_ROOT"),
        home.as_ref().map(|h| h.join("Library/Android/sdk")),
        home.as_ref().map(|h| h.join("Android/Sdk")),
    ];
    let installed = sdks.into_iter().flatten().filter_map(|sdk| {
        let mut versions: Vec<PathBuf> = std::fs::read_dir(sdk.join("ndk"))
            .ok()?
            .flatten()
            .map(|e| e.path())
            .collect();
        versions.sort();
        versions.pop()
    });
    [var("ANDROID_NDK_HOME"), var("ANDROID_NDK_ROOT")]
        .into_iter()
        .flatten()
        .chain(installed)
        .find(|dir| dir.join("sources/android/native_app_glue").is_dir())
}

fn android() -> Option<Android> {
    let found = ndk().and_then(|ndk| {
        let host = if cfg!(target_os = "macos") {
            "darwin-x86_64"
        } else {
            "linux-x86_64"
        };
        let tools = ndk.join("toolchains/llvm/prebuilt").join(host).join("bin");
        ["x86_64-linux-android", "aarch64-linux-android"]
            .into_iter()
            .map(|triple| {
                let runtime = repo_root()
                    .join("target")
                    .join(triple)
                    .join("release/libash_std.a");
                (triple, runtime)
            })
            .find(|(_, runtime)| runtime.is_file())
            .map(|(triple, runtime)| Android {
                tools,
                ndk,
                triple,
                runtime,
            })
    });
    if found.is_none() {
        let required = std::env::var("ASH_ANDROID_TEST").is_ok_and(|v| v == "required");
        assert!(!required, "no Android NDK with a built ash_std runtime");
        eprintln!("skipped: no Android NDK with a built ash_std runtime");
    }
    found
}

fn output_of(command: &mut Command) -> String {
    let out = command.output().expect("the NDK tool did not start");
    assert!(out.status.success(), "{}", render_output(&out));
    String::from_utf8_lossy(&out.stdout).into_owned()
}

impl Android {
    /// `sources` compiled for the target and archived as `lib<name>.a`.
    fn archive(&self, dir: &Path, name: &str, sources: &[PathBuf]) -> PathBuf {
        let glue = self.ndk.join("sources/android/native_app_glue");
        let archive = dir.join(format!("lib{name}.a"));
        let mut objects = Vec::new();
        for (i, source) in sources.iter().enumerate() {
            let object = dir.join(format!("{name}{i}.o"));
            output_of(
                Command::new(self.tools.join("clang"))
                    .arg(format!("--target={}24", self.triple))
                    .args(["-fPIC", "-c"])
                    .arg("-I")
                    .arg(&glue)
                    .arg(source)
                    .arg("-o")
                    .arg(&object),
            );
            objects.push(object);
        }
        output_of(
            Command::new(self.tools.join("llvm-ar"))
                .arg("rcs")
                .arg(&archive)
                .args(&objects),
        );
        archive
    }

    fn host(&self, dir: &Path) -> PathBuf {
        let glue = self
            .ndk
            .join("sources/android/native_app_glue/android_native_app_glue.c");
        self.archive(
            dir,
            "host",
            &[glue, tests_dir().join("android_host/host.c")],
        )
    }

    fn toy(&self, dir: &Path) -> PathBuf {
        self.archive(dir, "toy", &[tests_dir().join("static_native/toy.c")])
    }

    fn build(&self, out: &Path, archives: &[PathBuf]) -> Result<(), String> {
        let mut cmd = Command::new(ash_cli_bin());
        cmd.arg("--quiet")
            .arg("--build")
            .arg(out)
            .args(["--target", self.triple])
            .arg("--runtime")
            .arg(&self.runtime)
            .env("ANDROID_NDK_HOME", &self.ndk);
        for archive in archives {
            cmd.arg("--native-lib").arg(archive);
        }
        cmd.arg(tests_dir().join("test_static_native.hl"));
        let result = run_with_timeout(cmd, Duration::from_secs(600));
        if !result.timed_out && result.output.status.success() {
            Ok(())
        } else {
            Err(render_output(&result.output))
        }
    }

    fn readelf(&self, flag: &str, library: &Path) -> String {
        output_of(
            Command::new(self.tools.join("llvm-readelf"))
                .args(["--wide", flag])
                .arg(library),
        )
    }
}

#[test]
fn an_android_output_named_so_is_a_library_a_native_activity_can_load() {
    let Some(android) = android() else { return };
    let dir = tempfile::tempdir().unwrap();
    let archives = [android.host(dir.path()), android.toy(dir.path())];
    let library = dir.path().join("libmain.so");
    android
        .build(&library, &archives)
        .unwrap_or_else(|e| panic!("{e}"));

    let header = android.readelf("--file-header", &library);
    assert!(header.contains("DYN (Shared object file)"), "{header}");
    let dynamic = android.readelf("--dynamic-table", &library);
    for wanted in [
        "Library soname: [libmain.so]",
        "[liblog.so]",
        "[libandroid.so]",
    ] {
        assert!(dynamic.contains(wanted), "{wanted} not in\n{dynamic}");
    }

    // Columns: number, value, size, type, binding, visibility, section, name.
    let symbols = android.readelf("--dyn-symbols", &library);
    let exported: Vec<&str> = symbols
        .lines()
        .map(|line| line.split_whitespace().collect::<Vec<_>>())
        .filter(|c| c.len() == 8 && matches!(c[4], "GLOBAL" | "WEAK") && c[6] != "UND")
        .map(|c| c[7])
        .collect();
    assert!(
        exported.contains(&"ANativeActivity_onCreate"),
        "the activity's entry is not exported: {exported:?}"
    );
    let extra: Vec<&&str> = exported
        .iter()
        .filter(|name| {
            !["ANativeActivity_", "JNI_On", "Java_"]
                .iter()
                .any(|p| name.starts_with(p))
        })
        .collect();
    assert!(
        extra.is_empty(),
        "exported beside what Android calls: {extra:?}"
    );
}

#[test]
fn an_android_library_with_nothing_to_call_is_refused() {
    let Some(android) = android() else { return };
    let dir = tempfile::tempdir().unwrap();
    let library = dir.path().join("libmain.so");
    let err = android
        .build(&library, &[android.toy(dir.path())])
        .unwrap_err();
    assert!(err.contains("exports nothing Android can call"), "{err}");
    assert!(!library.exists(), "an unusable library was left behind");
}
