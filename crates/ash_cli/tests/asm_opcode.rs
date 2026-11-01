//! HashLink `$asm` machine code runs only in compiled code, on x86-64.
//!
//! The fixtures are `AsmBase`-style bytecode whose `add(a, b)` body was
//! rewritten to `$asm` ops by `crates/ash/examples/asm_fixture.rs`: load the
//! arguments into eax and ecx, run `add` or `imul`, store eax.
mod common;

use common::{ash_cli_bin, render_output, run_with_timeout, tests_dir};
use std::process::Command;
use std::time::Duration;

fn run(mode: &str, fixture: &str) -> (bool, String) {
    let mut command = Command::new(ash_cli_bin());
    command
        .args(["--mode", mode])
        .arg(tests_dir().join(fixture));
    let result = run_with_timeout(command, Duration::from_secs(60));
    assert!(
        !result.timed_out,
        "{mode} {fixture}: {}",
        render_output(&result.output)
    );
    let text = format!(
        "{}{}",
        String::from_utf8_lossy(&result.output.stdout),
        String::from_utf8_lossy(&result.output.stderr)
    );
    (result.output.status.success(), text)
}

#[test]
fn the_interpreter_refuses_asm_instead_of_skipping_it() {
    let (_, text) = run("interp", "asm_add.hl");
    assert!(text.contains("cannot run in the interpreter"), "{text}");
}

#[cfg(target_arch = "x86_64")]
#[test]
fn compiled_tiers_run_the_asm_bytes() {
    for (fixture, expected) in [
        ("asm_add.hl", "add 5 42 sum -1474336480"),
        ("asm_imul.hl", "add 6 80 sum -129842144"),
    ] {
        for mode in ["hybrid", "jit"] {
            let (_, text) = run(mode, fixture);
            assert!(text.contains(expected), "{mode} {fixture}: {text}");
        }
    }
}

#[cfg(not(target_arch = "x86_64"))]
#[test]
fn other_hosts_refuse_asm_rather_than_ignore_it() {
    for mode in ["hybrid", "jit"] {
        let (_, text) = run(mode, "asm_add.hl");
        assert!(!text.contains("add 5 42"), "{mode}: {text}");
    }
}
