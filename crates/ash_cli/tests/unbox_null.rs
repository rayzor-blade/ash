//! `charCodeAt` answers `Null<Int>`; the pass that carries it unboxed has to
//! read exactly what the boxed code read, at the edges where null, 0 and the
//! largest unit meet.
mod common;

use common::{ash_cli_bin, haxe_available, render_output, run_with_timeout, tests_dir};
use std::process::Command;
use std::time::Duration;

// The output of Haxe's evaluator for TestUnboxNull.
const EXPECTED: &str = "\
null,A,B,null,null\n\
null,null,null\n\
null,A,nul,#65536,#123,null,null\n\
12345\n\
0\n\
2\n\
2\n\
true\n\
false\n\
65536\n";

#[test]
fn a_nullable_char_code_reads_the_same_unboxed() {
    let dir = tempfile::tempdir().unwrap();
    let fixture = dir.path().join("unbox_null.hl");
    if haxe_available() {
        let output = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", "TestUnboxNull", "-hl"])
            .arg(&fixture)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "haxe: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    } else {
        std::fs::copy(tests_dir().join("test_unbox_null.hl"), &fixture)
            .expect("committed unbox bytecode fixture");
    }
    let mut modes: Vec<&[&str]> = vec![
        &["--mode", "interp"],
        &["--mode", "hybrid", "--jit-threshold", "1"],
        &["--mode", "jit"],
        &["--mode", "jit", "--jit-tier", "cranelift"],
    ];
    if cfg!(feature = "llvm") {
        modes.push(&["--mode", "jit", "--jit-tier", "llvm"]);
    }
    for mode in modes {
        let mut command = Command::new(ash_cli_bin());
        command.args(mode).arg(&fixture);
        let result = run_with_timeout(command, Duration::from_secs(60));
        assert!(
            !result.timed_out && result.output.status.success(),
            "{mode:?}: {}",
            render_output(&result.output)
        );
        let stdout = String::from_utf8_lossy(&result.output.stdout);
        let lines: String = stdout
            .lines()
            .filter(|l| !l.ends_with("returned: Void"))
            .map(|l| format!("{l}\n"))
            .collect();
        assert_eq!(lines, EXPECTED, "{mode:?}");
    }
}
