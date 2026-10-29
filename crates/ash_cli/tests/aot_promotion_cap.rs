//! AOT must not inherit the JIT's shared-module budget or table trampolines.
#![cfg(feature = "llvm")]
mod common;

use common::{ash_cli_bin, haxe_available, render_output, repo_root, run_with_timeout};
use std::process::{Command, Output};
use std::time::Duration;

fn run(command: Command) -> Output {
    let result = run_with_timeout(command, Duration::from_secs(120));
    assert!(
        !result.timed_out && result.output.status.success(),
        "{}",
        render_output(&result.output)
    );
    result.output
}

#[test]
fn aot_ignores_jit_promotion_caps_and_preserves_calls() {
    if !haxe_available() {
        return;
    }
    let dir = tempfile::tempdir().unwrap();
    let hl = dir.path().join("calls.hl");
    std::fs::write(dir.path().join("Calls.hx"), r#"
class Parent {
    public function new() {}
    public function apply(x:Int):Int return x + 1;
    public var score(get, never):Int;
    function get_score():Int return 7;
}
class Child extends Parent {
    override public function apply(x:Int):Int return x + 2;
}
class Calls {
    @:neverInline static function helper(x:Int):Int {
        if (x < 0) throw "expected";
        return x * 3;
    }
    static function main() {
        var receiver:Parent = new Child();
        var method = receiver.apply;
        var sum = receiver.score;
        for (i in 0...4) sum += helper(method(i));
        try { helper(-1); } catch (e:Dynamic) { sum++; }
        Sys.println(sum);
    }
}
"#).unwrap();
    let mut haxe = Command::new("haxe");
    haxe.arg("-cp").arg(dir.path()).args(["-main", "Calls", "-hl"]).arg(&hl);
    run(haxe);

    let runtime = repo_root().join("target/release/wasm32-wasip1-threads/ash_runtime.o");
    for wasm in [false, true] {
        if wasm && !runtime.is_file() {
            eprintln!("skipping WASM: no threads runtime");
            continue;
        }
        let output = dir.path().join(if wasm { "calls.wasm" } else if cfg!(windows) { "calls.exe" } else { "calls" });
        let mut first_wasm = None;
        for cap in ["1", "256", "off"] {
            let mut build = Command::new(ash_cli_bin());
            build.arg("--build").arg(&output).arg(&hl)
                .env("ASH_PROMOTE_MODULE_CAP", cap)
                .env("ASH_AOT_SHARDS", "2")
                .env("ASH_PROFILE", "phases")
                .env_remove("ASH_PROFILE_OUT")
                .env_remove("ASH_RUNTIME");
            if wasm {
                build.args(["--target", "wasm32-wasip1-threads"])
                    .env("ASH_RUNTIME", &runtime);
            }
            let built = run(build);
            let log = String::from_utf8_lossy(&built.stderr);
            assert!(!log.contains("table trampolines"), "cap={cap}, wasm={wasm}: {log}");
            if wasm {
                let bytes = std::fs::read(&output).unwrap();
                if let Some(first) = &first_wasm {
                    assert_eq!(&bytes, first, "cap={cap} changed the WASM artifact");
                } else {
                    first_wasm = Some(bytes);
                }
            }
            let mut execute = if wasm {
                let mut command = Command::new(ash_cli_bin());
                command.arg("run").arg(&output);
                command
            } else {
                Command::new(&output)
            };
            execute.env_remove("ASH_PROFILE").env_remove("ASH_PROFILE_OUT");
            assert_eq!(String::from_utf8_lossy(&run(execute).stdout).trim(), "50");
        }
    }
}
