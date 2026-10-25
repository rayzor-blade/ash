//! Distribution builds select the named CPU/format and can use a foreign toolchain.
#![cfg(feature = "llvm")]
mod common;
use common::{ash_cli_bin, render_output, run_with_timeout, tests_dir};
use inkwell::targets::TargetMachine;
use std::process::{Command, Output};
use std::time::Duration;

fn run(cmd: Command) -> Output {
    let r = run_with_timeout(cmd, Duration::from_secs(120));
    assert!(
        !r.timed_out && r.output.status.success(),
        "{}",
        render_output(&r.output)
    );
    r.output
}

#[test]
fn explicitly_named_host_build_is_generic_and_runs() {
    let dir = tempfile::tempdir().unwrap();
    let host = TargetMachine::get_default_triple()
        .as_str()
        .to_string_lossy()
        .into_owned();
    let exe = dir.path().join(if cfg!(windows) {
        "distribution.exe"
    } else {
        "distribution"
    });
    let input = tests_dir().join("test_basic.hl");
    let alias = match (std::env::consts::OS, std::env::consts::ARCH) {
        ("macos", "aarch64") => "macos-aarch64",
        ("macos", "x86_64") => "macos-x86_64",
        ("linux", "aarch64") => "linux-aarch64",
        ("linux", "x86_64") => "linux-x86_64",
        ("windows", "aarch64") => "windows-aarch64",
        ("windows", "x86_64") => "windows-x86_64",
        _ => &host,
    };
    for target in [host.as_str(), alias] {
        let mut build = Command::new(ash_cli_bin());
        build
            .arg("--build")
            .arg(&exe)
            .args(["--target", target])
            .arg(&input);
        let built = run(build);
        assert!(
            String::from_utf8_lossy(&built.stderr).contains("cpu generic"),
            "{}",
            render_output(&built)
        );
        let actual = run(Command::new(&exe));
        let mut reference = Command::new(ash_cli_bin());
        reference.args(["--quiet", "--mode", "interp"]).arg(&input);
        assert_eq!(actual.stdout, run(reference).stdout);
    }
}

#[test]
fn distribution_names_emit_elf_macho_and_coff() {
    let dir = tempfile::tempdir().unwrap();
    for (target, magic, machine) in [
        ("windows-x86_64", vec![0x64, 0x86], None),
        ("windows-aarch64", vec![0x64, 0xaa], None),
        (
            "macos-x86_64",
            vec![0xcf, 0xfa, 0xed, 0xfe],
            Some(0x01000007u32),
        ),
        (
            "macos-aarch64",
            vec![0xcf, 0xfa, 0xed, 0xfe],
            Some(0x0100000cu32),
        ),
        (
            "riscv64gc-unknown-linux-gnu",
            vec![0x7f, b'E', b'L', b'F'],
            None,
        ),
    ] {
        let object = dir.path().join(format!("{target}.o"));
        let mut cmd = Command::new(ash_cli_bin());
        cmd.arg("--emit-aot")
            .arg(&object)
            .args(["--target", target])
            .arg(tests_dir().join("test_basic.hl"));
        let output = run(cmd);
        assert!(String::from_utf8_lossy(&output.stderr).contains("cpu generic"));
        let bytes = std::fs::read(object).unwrap();
        assert!(
            bytes.starts_with(&magic),
            "wrong object format for {target}"
        );
        if let Some(machine) = machine {
            assert_eq!(u32::from_le_bytes(bytes[4..8].try_into().unwrap()), machine);
        }
        if target.starts_with("riscv") {
            assert_eq!(u16::from_le_bytes(bytes[18..20].try_into().unwrap()), 0xf3);
        }
    }
}

#[test]
fn configured_cross_executable_matches_the_reference() {
    let Some(target) = std::env::var_os("ASH_TEST_CROSS_TARGET") else {
        return;
    };
    let target = target.to_str().unwrap();
    let runner = std::env::var("ASH_TEST_CROSS_RUNNER")
        .expect("ASH_TEST_CROSS_RUNNER must run the target executable");
    let words = runner.split_whitespace().collect::<Vec<_>>();
    assert!(!words.is_empty());
    let dir = tempfile::tempdir().unwrap();
    for fixture in ["test_basic.hl", "test_stdlib.hl"] {
        let exe = dir.path().join(fixture);
        let mut build = Command::new(ash_cli_bin());
        build
            .arg("--build")
            .arg(&exe)
            .args(["--target", target])
            .arg(tests_dir().join(fixture));
        run(build);
        let mut target_run = Command::new(words[0]);
        target_run.args(&words[1..]).arg(&exe);
        let actual = run(target_run);
        let mut reference = Command::new(ash_cli_bin());
        reference
            .args(["--quiet", "--mode", "interp"])
            .arg(tests_dir().join(fixture));
        let expected = run(reference).stdout;
        assert_eq!(actual.stdout, expected, "{fixture}");
        if let Some(runtime) = std::env::var_os("ASH_TEST_CROSS_SHARED_RUNTIME") {
            let input = tests_dir().join(fixture);
            ash_core::native_lib::choose_std_linkage(&input);
            ash_core::native_lib::init_std_library().unwrap();
            let mut bc = ash_core::bytecode::BytecodeDecoder::decode(&input).unwrap();
            // An unused optional HDLL declaration selects shared-runtime code generation.
            let mut native = bc.natives[0].clone();
            native.lib = "?ash_cross_unused".into();
            native.name = "unused".into();
            native.findex = (bc.functions.len() + bc.natives.len()) as i32;
            bc.strings.extend([native.lib.clone(), native.name.clone()]);
            bc.natives.push(native);
            let input = dir.path().join(format!("shared-{fixture}"));
            std::fs::write(&input, ash_core::bytecode_encode::encode(&bc, 5).unwrap()).unwrap();
            let mut build = Command::new(ash_cli_bin());
            build
                .arg("--build")
                .arg(&exe)
                .args(["--target", target])
                .arg("--runtime")
                .arg(runtime)
                .arg(input);
            run(build);
            let mut shared = Command::new(words[0]);
            shared.args(&words[1..]).arg(&exe);
            assert_eq!(run(shared).stdout, expected, "shared runtime: {fixture}");
        }
    }
}
