#![cfg(feature = "llvm")]
mod common;

use common::{ash_cli_bin, haxe_available, render_output, repo_root, run_with_timeout};
use std::path::Path;
use std::process::{Command, Output};
use std::time::Duration;

fn run(mut command: Command) -> Output {
    command.env("ASH_AOT_SHARDS", "1");
    let result = run_with_timeout(command, Duration::from_secs(120));
    assert!(
        !result.timed_out && result.output.status.success(),
        "{}",
        render_output(&result.output)
    );
    result.output
}
fn count(output: &Output, name: &str) -> usize {
    let log = String::from_utf8_lossy(&output.stderr);
    let line = log
        .lines()
        .find(|s| s.starts_with("[air-disk-cache]"))
        .expect(&log);
    line.split_whitespace()
        .find_map(|word| word.strip_prefix(&format!("{name}=")))
        .unwrap_or_else(|| panic!("missing {name}: {line}"))
        .parse()
        .unwrap()
}
fn build(hl: &Path, out: &Path, cache: &Path, extra: &[&str]) -> Output {
    let mut command = Command::new(ash_cli_bin());
    command
        .args([
            "--quiet",
            "--no-tree-fusion",
            "--air-cache-stats",
            "--air-cache-dir",
        ])
        .arg(cache)
        .arg("--build")
        .arg(out)
        .args(extra)
        .arg(hl);
    run(command)
}
fn compile(dir: &Path, op: &str, out: &Path) {
    std::fs::write(
        dir.join("AirCacheFixture.hx"),
        format!(
            r#"
class AirCacheFixture {{
    @:neverInline static function helper(x:Int):Int return x {op} x;
    static function main() {{ var sum = 0; for (i in 0...5) sum += helper(i); Sys.println(sum); }}
}}
"#
        ),
    )
    .unwrap();
    let mut command = Command::new("haxe");
    command
        .arg("-cp")
        .arg(dir)
        .args(["-main", "AirCacheFixture", "-hl"])
        .arg(out);
    run(command);
}

#[test]
fn persistent_cache_across_processes_edits_corruption_and_disable() {
    if !haxe_available() {
        return;
    }
    let dir = tempfile::tempdir().unwrap();
    let hl = dir.path().join("fixture.hl");
    let binary = dir.path().join("fixture");
    let cache = dir.path().join("cache");
    compile(dir.path(), "+", &hl);
    let cold = build(&hl, &binary, &cache, &[]);
    assert_eq!(count(&cold, "optimized_hits"), 0);
    assert!(count(&cold, "written") > 0);
    let warm = build(&hl, &binary, &cache, &[]);
    assert!(count(&warm, "optimized_hits") > 0);
    assert_eq!(
        String::from_utf8_lossy(&run(Command::new(&binary)).stdout).trim(),
        "20"
    );
    let disabled = build(&hl, &binary, &cache, &["--no-air-cache"]);
    assert_eq!(count(&disabled, "optimized_hits"), 0);
    assert_eq!(count(&disabled, "written"), 0);
    compile(dir.path(), "*", &hl);
    let edit = build(&hl, &binary, &cache, &[]);
    assert!(count(&edit, "optimized_hits") > 0);
    assert!(count(&edit, "misses") > 0);
    assert_eq!(
        String::from_utf8_lossy(&run(Command::new(&binary)).stdout).trim(),
        "30"
    );
    for entry in std::fs::read_dir(cache.join("entries")).unwrap() {
        std::fs::write(entry.unwrap().path(), b"interrupted or corrupt").unwrap();
    }
    let recovered = build(&hl, &binary, &cache, &[]);
    assert!(count(&recovered, "invalid") > 0);
    assert_eq!(
        String::from_utf8_lossy(&run(Command::new(&binary)).stdout).trim(),
        "30"
    );
    let mut command = Command::new(ash_cli_bin());
    command
        .arg("--air-cache-dir")
        .arg(&cache)
        .args(["air-cache", "--clear"]);
    let cleared = run(command);
    assert!(String::from_utf8_lossy(&cleared.stdout).contains("entries=0 bytes=0"));
}

#[test]
fn text_only_edits_reuse_air_and_codegen_uses_current_literals() {
    if !haxe_available() {
        return;
    }
    let dir = tempfile::tempdir().unwrap();
    let source = dir.path().join("AirCacheText.hx");
    let hl = dir.path().join("text.hl");
    let binary = dir.path().join("text");
    let wasm = dir.path().join("text.wasm");
    let runtime = repo_root().join("target/release/wasm32-wasip1/ash_runtime.o");
    let cache = dir.path().join("cache");
    for message in ["old label", "new label"] {
        std::fs::write(
            &source,
            format!(
                "class AirCacheText {{ static function main() Sys.println(\"{message}\"); }}\n"
            ),
        )
        .unwrap();
        let mut command = Command::new("haxe");
        command
            .arg("-cp")
            .arg(dir.path())
            .args(["-main", "AirCacheText", "-hl"])
            .arg(&hl);
        run(command);
        let result = build(&hl, &binary, &cache, &[]);
        if message == "new label" {
            assert!(count(&result, "optimized_hits") > 0);
        }
        assert_eq!(
            String::from_utf8_lossy(&run(Command::new(&binary)).stdout).trim(),
            message
        );
        if runtime.is_file() {
            let mut command = Command::new(ash_cli_bin());
            command
                .args(["--quiet", "--air-cache-stats", "--air-cache-dir"])
                .arg(&cache)
                .arg("--build")
                .arg(&wasm)
                .args(["--target", "wasm32-wasip1"])
                .arg(&hl)
                .env("ASH_RUNTIME", &runtime);
            let result = run(command);
            if message == "new label" {
                assert!(count(&result, "optimized_hits") > 0);
            }
            let mut command = Command::new(ash_cli_bin());
            command.arg("run").arg(&wasm);
            assert_eq!(
                String::from_utf8_lossy(&run(command).stdout).trim(),
                message
            );
        }
    }
}

#[test]
fn wasm_cache_is_separate_from_native_and_preserves_output() {
    if !haxe_available() {
        return;
    }
    let runtime = repo_root().join("target/release/wasm32-wasip1/ash_runtime.o");
    if !runtime.is_file() {
        eprintln!("skipping: no wasm runtime at {}", runtime.display());
        return;
    }
    let dir = tempfile::tempdir().unwrap();
    let hl = dir.path().join("fixture.hl");
    let binary = dir.path().join("fixture");
    let wasm = dir.path().join("fixture.wasm");
    let cache = dir.path().join("cache");
    compile(dir.path(), "+", &hl);
    build(&hl, &binary, &cache, &[]);
    let args = ["--target", "wasm32-wasip1"];
    let wasm_build = || {
        let mut command = Command::new(ash_cli_bin());
        command
            .args([
                "--quiet",
                "--no-tree-fusion",
                "--air-cache-stats",
                "--air-cache-dir",
            ])
            .arg(&cache)
            .arg("--build")
            .arg(&wasm)
            .args(args)
            .arg(&hl)
            .env("ASH_RUNTIME", &runtime);
        run(command)
    };
    let cold = wasm_build();
    assert_eq!(count(&cold, "optimized_hits"), 0);
    assert!(count(&wasm_build(), "optimized_hits") > 0);
    let mut command = Command::new(ash_cli_bin());
    command.arg("run").arg(&wasm);
    assert_eq!(String::from_utf8_lossy(&run(command).stdout).trim(), "20");
    compile(dir.path(), "*", &hl);
    assert!(count(&wasm_build(), "optimized_hits") > 0);
    let mut command = Command::new(ash_cli_bin());
    command.arg("run").arg(&wasm);
    assert_eq!(String::from_utf8_lossy(&run(command).stdout).trim(), "30");
}
