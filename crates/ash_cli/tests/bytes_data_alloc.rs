mod common;

use common::{ash_cli_bin, haxe_available, render_output, run_with_timeout, tests_dir};
use std::io::{BufRead, BufReader};
use std::process::Command;
use std::process::Stdio;
use std::sync::mpsc;
use std::time::Duration;

fn amount(json: &str, key: &str) -> u64 {
    let marker = format!("\"{key}\":");
    json.split(&marker)
        .nth(1)
        .unwrap_or_else(|| panic!("missing {key} in {json}"))
        .split([',', '}'])
        .next()
        .unwrap()
        .parse()
        .unwrap_or_else(|_| panic!("invalid {key} in {json}"))
}

#[test]
fn inlined_bytes_data_wrapper_does_not_allocate() {
    let dir = tempfile::tempdir().unwrap();
    let hl = dir.path().join("bytes_data_alloc.hl");
    if haxe_available() {
        let compile = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", "TestBytesDataAlloc", "-hl"])
            .arg(&hl)
            .output()
            .unwrap();
        assert!(compile.status.success(), "{}", render_output(&compile));
    } else {
        std::fs::copy(tests_dir().join("test_bytes_data_alloc.hl"), &hl)
            .expect("committed byte data allocation fixture");
    }

    for mode in [
        &["--mode", "jit", "--jit-tier", "cranelift"][..],
        &["--mode", "jit", "--jit-tier", "llvm"][..],
        &["--mode", "hybrid", "--preset", "game"][..],
        &["--mode", "interp"][..],
        &["--mode", "jit", "--jit-tier", "cranelift", "--hot-reload"][..],
        &["--mode", "jit", "--jit-tier", "llvm", "--hot-reload"][..],
    ] {
        let mut command = Command::new(ash_cli_bin());
        command.args(mode).arg(&hl).env("ASH_GC_TLAB", "0");
        let result = run_with_timeout(command, Duration::from_secs(30));
        assert!(
            !result.timed_out && result.output.status.success(),
            "{mode:?}: {}",
            render_output(&result.output)
        );
        let stdout = String::from_utf8_lossy(&result.output.stdout);
        let json = stdout
            .lines()
            .find(|line| line.starts_with('{'))
            .unwrap_or_else(|| panic!("{mode:?}: no result in {}", render_output(&result.output)));
        assert_eq!(amount(json, "calls"), 8192, "{mode:?}: {json}");
        for key in ["getData", "cached", "blit"] {
            assert_eq!(amount(json, key), 0, "{mode:?}: {json}");
        }
    }
}

#[test]
fn llvm_reload_keeps_calls_from_an_active_frame_current() {
    if !haxe_available() {
        return;
    }
    const SOURCE: &str = r#"
class TestInlineReload {
    static function helper():Int return HELPER_RESULT;
    static function work(wait:Bool):Int {
        if (!wait) return 0;
        var deadline = Sys.time() + 10.0;
        while (Sys.time() < deadline) {
            if (hl.Api.checkReload()) return helper();
            Sys.sleep(0.001);
        }
        return -1;
    }
    static function main() {
        for (i in 0...200) work(false);
        Sys.println("ready");
        Sys.println("value " + work(true));
    }
}
"#;
    let dir = tempfile::tempdir().unwrap();
    let live = dir.path().join("live.hl");
    let mut versions = Vec::new();
    for value in [11, 22] {
        let src = dir.path().join(format!("v{value}"));
        std::fs::create_dir(&src).unwrap();
        std::fs::write(
            src.join("TestInlineReload.hx"),
            SOURCE.replace("HELPER_RESULT", &value.to_string()),
        )
        .unwrap();
        let bytecode = dir.path().join(format!("v{value}.hl"));
        let out = Command::new("haxe")
            .arg("-cp")
            .arg(&src)
            .args(["-main", "TestInlineReload", "-hl"])
            .arg(&bytecode)
            .output()
            .unwrap();
        assert!(out.status.success(), "{}", render_output(&out));
        versions.push(bytecode);
    }
    std::fs::copy(&versions[0], &live).unwrap();
    let stderr_path = dir.path().join("stderr");
    let mut child = Command::new(ash_cli_bin())
        .args([
            "--mode",
            "jit",
            "--jit-tier",
            "llvm",
            "--hot-reload",
            "--quiet",
        ])
        .arg(&live)
        .stdout(Stdio::piped())
        .stderr(Stdio::from(std::fs::File::create(&stderr_path).unwrap()))
        .spawn()
        .unwrap();
    let (sender, receiver) = mpsc::channel();
    let stdout = child.stdout.take().unwrap();
    std::thread::spawn(move || {
        for line in BufReader::new(stdout).lines().map_while(Result::ok) {
            if sender.send(line).is_err() {
                break;
            }
        }
    });
    let ready = receiver.recv_timeout(Duration::from_secs(15)).unwrap();
    assert_eq!(ready, "ready");
    std::thread::sleep(Duration::from_millis(1100));
    let replacement = dir.path().join("replacement.hl");
    std::fs::copy(&versions[1], &replacement).unwrap();
    std::fs::rename(&replacement, &live).unwrap();
    let value = receiver
        .recv_timeout(Duration::from_secs(15))
        .unwrap_or_else(|e| {
            child.kill().ok();
            panic!("LLVM reload timed out: {e}");
        });
    assert_eq!(
        value,
        "value 22",
        "{}",
        std::fs::read_to_string(&stderr_path).unwrap()
    );
    assert!(child.wait().unwrap().success());
}
