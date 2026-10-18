//! The sampler names a hot interpreted function `Class.method` from the type
//! table, for instance methods and statics alike, instead of `Fun_<findex>`.
mod common;

use common::{ash_cli_bin, haxe_available, render_output, run_with_timeout, tests_dir};
use std::process::Command;
use std::time::Duration;

#[test]
fn hot_functions_are_named_by_class_and_method() {
    let dir = tempfile::tempdir().unwrap();
    let fixture = dir.path().join("sampler_names.hl");
    if haxe_available() {
        let output = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", "TestSamplerNames", "-hl"])
            .arg(&fixture)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "haxe: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    } else {
        std::fs::copy(tests_dir().join("test_sampler_names.hl"), &fixture)
            .expect("committed sampler names bytecode fixture");
    }

    let report = dir.path().join("profile.txt");
    let mut command = Command::new(ash_cli_bin());
    command
        .args(["--mode", "interp"])
        .arg(&fixture)
        .env("ASH_PROFILE", "sample")
        .env("ASH_PROFILE_OUT", &report);
    let result = run_with_timeout(command, Duration::from_secs(120));
    assert!(
        !result.timed_out && result.output.status.success(),
        "{}",
        render_output(&result.output)
    );

    let text = std::fs::read_to_string(&report).unwrap();
    let section = text
        .split_once("by interpreted function")
        .unwrap_or_else(|| panic!("no interpreted-function section:\n{text}"))
        .1;
    for name in ["Spinner.spin", "TestSamplerNames.hot"] {
        assert!(
            section.lines().any(|l| l.contains(name)),
            "{name} not named in:\n{section}"
        );
    }
}
