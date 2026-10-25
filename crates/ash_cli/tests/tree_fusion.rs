mod common;
use common::{ash_cli_bin, render_output, run_with_timeout, tests_dir};
use std::process::Command;
use std::time::Duration;

fn run(file: &str, mode: &[&str], fused: bool) -> std::process::Output {
    let mut cmd = Command::new(ash_cli_bin());
    cmd.args(mode).arg("--quiet");
    if fused {
        cmd.arg("--tree-fusion-log");
    } else {
        cmd.arg("--no-tree-fusion");
    }
    cmd.arg(tests_dir().join(file))
        .env("ASH_GC_STATS", "1")
        .env("ASH_GC_TLAB", "0");
    let result = run_with_timeout(cmd, Duration::from_secs(120));
    assert!(
        !result.timed_out && result.output.status.success(),
        "{}",
        render_output(&result.output)
    );
    result.output
}
fn allocation(out: &std::process::Output) -> f64 {
    let text = String::from_utf8_lossy(&out.stderr);
    let line = text
        .lines()
        .find(|l| l.starts_with("[gc] bytes allocated:"))
        .expect("allocation report");
    line.split_whitespace()
        .nth(3)
        .unwrap()
        .trim_end_matches("MB")
        .parse()
        .unwrap()
}

#[test]
fn unchanged_binary_trees_retains_checksum_and_only_long_lived_tree_allocation() {
    let input = tests_dir().join("bench_binary_trees.hl");
    let before = std::fs::read(&input).unwrap();
    let original = run("bench_binary_trees.hl", &["--mode", "hybrid"], false);
    let fused = run("bench_binary_trees.hl", &["--mode", "hybrid"], true);
    assert_eq!(
        String::from_utf8_lossy(&original.stdout).trim(),
        "Checksum: -104864"
    );
    assert_eq!(original.stdout, fused.stdout);
    assert!(
        allocation(&original) > 800.0,
        "{}",
        render_output(&original)
    );
    // ~4 MiB for depth-16's long-lived tree, plus startup. Cumulative bytes,
    // measured with the same no-TLAB policy on both sides, not peak heap/RSS.
    assert!(
        (3.9..5.0).contains(&allocation(&fused)),
        "{}",
        render_output(&fused)
    );
    let log = String::from_utf8_lossy(&fused.stderr);
    assert!(log.contains("transformed_sites=3 helpers=1"), "{log}");
    assert!(log.contains("node_allocations=0"), "{log}");
    assert!(
        log.contains("caller=25 producer=24 consumer=None refused"),
        "{log}"
    );
    assert_eq!(before, std::fs::read(input).unwrap());
}

#[test]
fn renamed_reordered_fields_wrapping_values_and_effectful_constructors() {
    let original = run("test_tree_fusion.hl", &["--mode", "interp"], false);
    assert_eq!(
        String::from_utf8_lossy(&original.stdout).trim(),
        "tree-fusion check=-3824 made=31"
    );
    let modes = [
        vec!["--mode", "interp"],
        vec!["--mode", "jit", "--jit-tier", "cranelift"],
        vec![
            "--mode",
            "hybrid",
            "--jit-threshold",
            "1",
            "--opt-threshold",
            "1",
        ],
        #[cfg(feature = "llvm")]
        vec!["--mode", "jit", "--jit-tier", "llvm"],
    ];
    for mode in modes {
        let fused = run("test_tree_fusion.hl", &mode, true);
        assert_eq!(
            original.stdout,
            fused.stdout,
            "{mode:?}: {}",
            render_output(&fused)
        );
        let log = String::from_utf8_lossy(&fused.stderr);
        assert!(log.contains("transformed_sites=6 helpers=1"), "{log}");
        assert!(
            log.contains("unsupported operation/effect: GetGlobal"),
            "{log}"
        );
    }
}

#[test]
fn runtime_dependent_fusion_cannot_be_serialized_or_hot_reloaded() {
    let dir = tempfile::tempdir().unwrap();
    for flag in ["--emit-optimized", "--build", "--emit-aot", "--hot-reload"] {
        let mut cmd = Command::new(ash_cli_bin());
        cmd.args(["--dev-tree-fusion", flag]);
        if flag != "--hot-reload" {
            cmd.arg(dir.path().join("output"));
        }
        cmd.arg(tests_dir().join("test_tree_fusion.hl"));
        let result = run_with_timeout(cmd, Duration::from_secs(10));
        assert!(!result.timed_out && !result.output.status.success());
        assert!(String::from_utf8_lossy(&result.output.stderr).contains("cannot be used with"));
        assert!(!dir.path().join("output").exists());
    }
}

#[test]
fn default_execution_is_quiet_and_export_uses_original_contract() {
    let dir = tempfile::tempdir().unwrap();
    let exported = dir.path().join("optimized.hl");
    let mut export = Command::new(ash_cli_bin());
    export
        .args(["--quiet", "--tree-fusion-log", "--emit-optimized"])
        .arg(&exported)
        .arg(tests_dir().join("test_tree_fusion.hl"));
    let result = run_with_timeout(export, Duration::from_secs(30));
    assert!(
        !result.timed_out && result.output.status.success(),
        "{}",
        render_output(&result.output)
    );
    assert!(!String::from_utf8_lossy(&result.output.stderr).contains("[tree-fusion]"));
    for input in [tests_dir().join("test_tree_fusion.hl"), exported] {
        let mut cmd = Command::new(ash_cli_bin());
        cmd.arg("--quiet").arg(input);
        let result = run_with_timeout(cmd, Duration::from_secs(30));
        assert!(
            !result.timed_out && result.output.status.success(),
            "{}",
            render_output(&result.output)
        );
        assert_eq!(
            String::from_utf8_lossy(&result.output.stdout).trim(),
            "tree-fusion check=-3824 made=31"
        );
        assert!(!String::from_utf8_lossy(&result.output.stderr).contains("[tree-fusion]"));
    }
}

/// A standalone executable `--build` writes is the program in its final form,
/// so it gets the same fusion a run does; `--no-tree-fusion` leaves it as is.
#[cfg(feature = "llvm")]
#[test]
fn a_built_executable_is_fused_unless_fusion_is_turned_off() {
    let dir = tempfile::tempdir().unwrap();
    let input = tests_dir().join("bench_binary_trees.hl");
    let mut outputs = Vec::new();
    for (name, flags) in [("fused", &[][..]), ("plain", &["--no-tree-fusion"][..])] {
        let exe = dir.path().join(name);
        let mut build = Command::new(ash_cli_bin());
        build
            .arg("--quiet")
            .args(flags)
            .arg("--build")
            .arg(&exe)
            .arg(&input);
        let built = run_with_timeout(build, Duration::from_secs(300));
        assert!(
            !built.timed_out && built.output.status.success(),
            "{}",
            render_output(&built.output)
        );
        let mut run = Command::new(&exe);
        run.env("ASH_GC_STATS", "1").env("ASH_GC_TLAB", "0");
        let ran = run_with_timeout(run, Duration::from_secs(120));
        assert!(
            !ran.timed_out && ran.output.status.success(),
            "{}",
            render_output(&ran.output)
        );
        outputs.push(ran.output);
    }
    assert_eq!(
        String::from_utf8_lossy(&outputs[0].stdout).trim(),
        "Checksum: -104864"
    );
    assert_eq!(outputs[0].stdout, outputs[1].stdout);
    assert!(
        (3.9..5.0).contains(&allocation(&outputs[0])),
        "{}",
        render_output(&outputs[0])
    );
    assert!(
        allocation(&outputs[1]) > 800.0,
        "{}",
        render_output(&outputs[1])
    );
}
