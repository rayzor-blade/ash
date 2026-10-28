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
fn wasm_optimization_levels_and_independent_overrides_preserve_catches() {
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
    let variants: &[(&[&str], &str, u8)] = &[
        (&["-O0"], "default<O0>", 0),
        (&["-O1"], "default<O1>", 1),
        (&["-O2"], "default<O2>", 2),
        (&["-O3"], "default<O3>", 3),
        (&["-Os"], "default<Os>", 2),
        (&["-Oz"], "default<Oz>", 2),
        (&["-O3", "--codegen-opt-level", "0"], "default<O3>", 0),
        (
            &["-O2", "--air-opt-level", "0", "--codegen-opt-level", "0"],
            "default<O2>",
            0,
        ),
    ];
    for (flags, pipeline, codegen) in variants {
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
            .env("ASH_AOT_SHARDS", "2")
            .env("ASH_LLVM_PASSES", "off")
            .env("ASH_AIR_LEVEL", "3")
            .env("ASH_AOT_CODEGEN_LEVEL", "3");
        let output = run(build);
        let log = String::from_utf8_lossy(&output.stderr);
        assert!(
            log.contains(&format!("llvm={pipeline} codegen={codegen}")),
            "{flags:?}: {log}"
        );
        let air = if flags.contains(&"--air-opt-level") {
            "O0"
        } else {
            "O3"
        };
        assert!(
            log.contains(&format!("optimization: air={air}")),
            "{flags:?}: {log}"
        );
        let workers: Vec<_> = log
            .lines()
            .filter(|line| line.contains("[aot] shard "))
            .collect();
        assert_eq!(workers.len(), 2, "{flags:?}: {log}");
        assert!(
            workers
                .iter()
                .all(|line| line.contains(&format!("codegen_level={codegen}"))),
            "{flags:?}: {log}"
        );
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
