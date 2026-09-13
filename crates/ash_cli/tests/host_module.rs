//! With nothing registered, a program that names a host library is refused
//! at load, by name: the default is ash unchanged, and the only way a
//! `host@` native resolves is a registration made before the run.

mod common;

use common::{ash_cli_bin, repo_root};
use std::process::Command;

#[test]
fn an_unregistered_host_library_is_refused_by_name() {
    let hl = repo_root().join("crates/ash_interp/tests/fixtures/host_module/main.hl");
    assert!(hl.exists(), "fixture not built: {}", hl.display());
    for mode in ["interp", "hybrid"] {
        let out = Command::new(ash_cli_bin())
            .args(["--mode", mode])
            .arg(&hl)
            .output()
            .expect("failed to run ash");
        let stdout = String::from_utf8_lossy(&out.stdout);
        let stderr = String::from_utf8_lossy(&out.stderr);
        assert!(
            !out.status.success(),
            "{mode}: ran without the host library:\n{stdout}"
        );
        assert!(
            stderr.contains("'host'"),
            "{mode}: the refusal should name the library:\n{stderr}"
        );
        assert!(
            stdout.trim().is_empty(),
            "{mode}: nothing should run:\n{stdout}"
        );
    }
}
