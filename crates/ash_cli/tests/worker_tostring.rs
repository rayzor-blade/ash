//! A worker thread can turn an object into a string before its `toString`
//! has been compiled.
//!
//! The runtime finds `toString` as a stub sentinel. A worker lane cannot hand
//! it to the interpreter, so the runtime compiles it and calls the code.

mod common;

use common::{ash_cli_bin, tests_dir};
use std::process::Command;

const FIXTURE: &str = "test_worker_tostring.hl";

#[test]
fn a_worker_stringifies_an_object_whose_tostring_is_not_compiled() {
    let path = tests_dir().join(FIXTURE);
    assert!(path.exists(), "fixture not built: {}", path.display());
    for mode in ["interp", "hybrid", "jit"] {
        let out = Command::new(ash_cli_bin())
            .args(["--mode", mode])
            .arg(&path)
            .output()
            .expect("failed to run ash");
        let stdout = String::from_utf8_lossy(&out.stdout);
        let stderr = String::from_utf8_lossy(&out.stderr);
        assert_eq!(out.status.code(), Some(0), "{mode}: run failed:\n{stderr}");
        assert!(
            stdout.lines().next() == Some("Named(worker)"),
            "{mode}: wrong output:\n{stdout}\n{stderr}"
        );
    }
}
