#![cfg(feature = "llvm")]
mod common;

use common::{ash_cli_bin, haxe_available, render_output, repo_root, run_with_timeout};
use std::process::{Command, Output};
use std::time::Duration;

fn run(command: Command) -> Output {
    let result = run_with_timeout(command, Duration::from_secs(90));
    assert!(
        !result.timed_out && result.output.status.success(),
        "{}",
        render_output(&result.output)
    );
    result.output
}

#[test]
fn wasm_air_levels_preserve_catches() {
    if !haxe_available() {
        return;
    }
    let runtime = repo_root().join("target/release/wasm32-wasip1/ash_runtime.o");
    if !runtime.is_file() {
        eprintln!("skipping: no WASM runtime");
        return;
    }
    let dir = tempfile::tempdir().unwrap();
    let hl = dir.path().join("opt.hl");
    let wasm = dir.path().join("opt.wasm");
    std::fs::write(
        dir.path().join("OptFixture.hx"),
        r#"
class OptFixture {
    @:neverInline static function twice(x:Int):Int return x + x;
    @:neverInline static function risky(x:Int):Int { if (x < 0) throw "expected"; return x; }
    static function main() {
        var sum = 0;
        for (i in 0...5) sum += twice(i);
        try { risky(-1); } catch (e:Dynamic) { sum += 3; }
        Sys.println(sum);
    }
}
"#,
    )
    .unwrap();
    let mut haxe = Command::new("haxe");
    haxe.arg("-cp")
        .arg(dir.path())
        .args(["-main", "OptFixture", "-hl"])
        .arg(&hl);
    run(haxe);
    // Flags that choose LLVM's pipeline have nothing to act on in a wasm
    // build, and are accepted; the AIR level is the one that applies.
    let variants: &[&[&str]] = &[
        &[],
        &["-O0"],
        &["-O3"],
        &["-Os"],
        &["-O3", "--codegen-opt-level", "0"],
        &["-O2", "--air-opt-level", "0"],
    ];
    for flags in variants {
        let mut build = Command::new(ash_cli_bin());
        build
            .arg("--air-cache-dir")
            .arg(dir.path().join("cache"))
            .arg("--build")
            .arg(&wasm)
            .args(["--target", "wasm32-wasip1"])
            .args(*flags)
            .arg(&hl)
            .env("ASH_RUNTIME", &runtime)
            .env("ASH_AIR_LEVEL", "3");
        let output = run(build);
        let log = String::from_utf8_lossy(&output.stderr);
        let air = if flags.contains(&"--air-opt-level") {
            "O0"
        } else {
            "O3"
        };
        assert!(
            log.contains(&format!("optimization: air={air}")),
            "{flags:?}: {log}"
        );
        assert!(log.contains("functions, no LLVM"), "{flags:?}: {log}");
        let mut execute = Command::new(ash_cli_bin());
        execute.arg("run").arg(&wasm);
        let result = run(execute);
        assert_eq!(
            String::from_utf8_lossy(&result.stdout).trim(),
            "23",
            "{flags:?}: {}",
            render_output(&result)
        );
    }
}
