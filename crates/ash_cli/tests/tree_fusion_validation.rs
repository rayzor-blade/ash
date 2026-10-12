//! Checkpoint 5: semantic parity, exercised tier transitions, and retained roots.
mod common;
use common::{ash_cli_bin, render_output, run_with_timeout, tests_dir};
use std::process::{Command, Output};
use std::time::Duration;

fn modes() -> Vec<Vec<&'static str>> {
    vec![
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
    ]
}
fn command(file: &str, mode: &[&str], fused: bool, args: &[&str], env: &[(&str, &str)]) -> Command {
    let mut cmd = Command::new(ash_cli_bin());
    cmd.args(mode).arg("--quiet");
    if fused {
        cmd.arg("--dev-tree-fusion");
    }
    cmd.arg(tests_dir().join(file))
        .args(args)
        .env("ASH_GC_STATS", "1")
        .env("ASH_GC_TLAB", "0")
        .env("ASH_VERIFY_AIR", "1");
    for (k, v) in env {
        cmd.env(k, v);
    }
    cmd
}
fn run(cmd: Command) -> Output {
    let out = run_with_timeout(cmd, Duration::from_secs(90));
    assert!(
        !out.timed_out && out.output.status.success(),
        "{}",
        render_output(&out.output)
    );
    out.output
}
fn log(out: &Output) -> String {
    String::from_utf8_lossy(&out.stderr).into_owned()
}
fn collections(out: &Output) -> usize {
    log(out)
        .lines()
        .find(|l| l.starts_with("[gc] collections:"))
        .expect("GC statistics")
        .split_whitespace()
        .last()
        .unwrap()
        .parse()
        .unwrap()
}

#[test]
fn unary_and_ternary_folds_match_with_gc_stress_and_reordered_consumption() {
    let file = "test_tree_fusion_matrix.hl";
    let stress = [("ASH_GC_STRESS", "17"), ("ASH_GC_VERIFY_HEAP", "1")];
    let expected = run(command(file, &["--mode", "interp"], false, &[], &stress));
    assert_eq!(
        String::from_utf8_lossy(&expected.stdout).trim(),
        "tree-matrix=-663667236"
    );
    for mode in modes() {
        for fused in [false, true] {
            let out = run(command(file, &mode, fused, &[], &stress));
            assert_eq!(
                out.stdout,
                expected.stdout,
                "{mode:?}, fused={fused}\n{}",
                log(&out)
            );
            // Eight explicit collections alone cannot satisfy this assertion.
            assert!(
                collections(&out) > 8,
                "stress never collected: {}",
                log(&out)
            );
            if fused {
                assert!(
                    log(&out).contains("transformed_sites=4 helpers=2"),
                    "{}",
                    log(&out)
                );
            }
        }
    }
}

#[test]
fn observable_effects_aliases_exceptions_and_dynamic_calls_stay_original() {
    let file = "test_tree_fusion_refusals.hl";
    let expected = run(command(file, &["--mode", "interp"], false, &[], &[]));
    assert_eq!(
        String::from_utf8_lossy(&expected.stdout),
        concat!(
            "observed=4 events=-2,0,-1,0,2,1,0,\n",
            "caught=constructor events=-3,\ncaught=leaf\ncaught=Null access\n",
            "escape=5/7\nshared=3 identity=2\nskip=2 mutate=12 dynamic=0\n"
        )
    );
    for mode in modes() {
        let out = run(command(file, &mode, true, &[], &[]));
        assert_eq!(out.stdout, expected.stdout, "{mode:?}\n{}", log(&out));
        for refusal in [
            "transformed_sites=0 helpers=0",
            "receiver escapes",
            "child is shared",
            "consumer does not visit every produced child",
            "unsupported operation/effect: FieldSet",
        ] {
            assert!(
                log(&out).contains(refusal),
                "missing {refusal}: {}",
                log(&out)
            );
        }
    }
}

#[test]
fn a_skipped_divergent_subtree_is_not_eliminated() {
    for fused in [false, true] {
        let out = run_with_timeout(
            command(
                "test_tree_fusion_refusals.hl",
                &["--mode", "interp"],
                fused,
                &["diverge"],
                &[],
            ),
            Duration::from_secs(2),
        );
        assert!(
            out.timed_out,
            "divergent producer returned: {}",
            render_output(&out.output)
        );
        assert!(
            String::from_utf8_lossy(&out.output.stdout).contains("starting divergent producer")
        );
        if fused {
            assert!(
                log(&out.output).contains("transformed_sites=0 helpers=0"),
                "{}",
                log(&out.output)
            );
        }
    }
}

#[test]
fn fused_helper_is_installed_in_both_hybrid_tiers_with_inlining_disabled() {
    let file = "test_tree_fusion_matrix.hl";
    let expected = run(command(file, &["--mode", "interp"], false, &["hot"], &[]));
    let probe = run(command(
        file,
        &["--mode", "interp"],
        true,
        &[],
        &[("ASH_AIR_NO_INLINE", "1")],
    ));
    let report = log(&probe);
    let helper = report
        .lines()
        .find_map(|l| l.strip_prefix("[tree-fusion] helper="))
        .expect("disabling inlining must not disable fusion")
        .split_whitespace()
        .next()
        .unwrap();
    let out = run(command(
        file,
        &[
            "--mode",
            "hybrid",
            "--jit-threshold",
            "1",
            "--opt-threshold",
            "1",
            "--jit-min-ops",
            "0",
            "--jit-log",
        ],
        true,
        &["hot"],
        &[
            ("ASH_AIR_NO_INLINE", "1"),
            ("ASH_TIERED_ONLY_FINDEXES", helper),
        ],
    ));
    assert_eq!(out.stdout, expected.stdout, "{}", log(&out));
    for tier in if cfg!(feature = "llvm") {
        vec!["cranelift", "llvm"]
    } else {
        vec!["cranelift"]
    } {
        assert!(
            log(&out).lines().any(|l| l.contains("install")
                && l.contains(&format!("findex={helper} "))
                && l.contains(&format!("tier={tier}"))),
            "missing helper {tier} install: {}",
            log(&out)
        );
    }
}

#[test]
fn rewritten_caller_enters_osr_with_live_reference_and_scalar_state() {
    let file = "test_tree_fusion_matrix.hl";
    let expected = run(command(file, &["--mode", "interp"], false, &["hot"], &[]));
    let out = run(command(
        file,
        &[
            "--mode",
            "hybrid",
            "--jit-tier",
            "cranelift",
            "--jit-threshold",
            "1",
            "--jit-min-ops",
            "0",
        ],
        true,
        &["hot"],
        &[
            ("ASH_OSR", "1"),
            ("ASH_OSR_LOG", "1"),
            ("ASH_OSR_ENTRY_SYNC", "1"),
        ],
    ));
    assert_eq!(out.stdout, expected.stdout, "{}", log(&out));
    // Resolve the caller by name from the fixture; avoid baking in a function ID.
    let path = tests_dir().join(file);
    ash_core::native_lib::choose_std_linkage(&path);
    ash_core::native_lib::init_std_library().unwrap();
    let bc = ash_core::bytecode::BytecodeDecoder::decode(&path).unwrap();
    let caller = bc.bound_static("TestTreeFusionMatrix", "hot").unwrap();
    assert!(
        log(&out).contains(&format!("[osr] entering findex={caller} ")),
        "OSR not exercised: {}",
        log(&out)
    );
    assert!(log(&out).contains("transformed_sites=4 helpers=2"));
}

#[test]
fn retained_tree_survives_worker_suspension_and_collection() {
    let file = "test_tree_fusion_suspended.hl";
    for mode in modes() {
        for workers in ["0", "2"] {
            for fused in [false, true] {
                let out = run(command(
                    file,
                    &mode,
                    fused,
                    &[],
                    &[
                        ("ASH_WORKERS", workers),
                        ("ASH_GC_STRESS", "17"),
                        ("ASH_GC_VERIFY_HEAP", "1"),
                    ],
                ));
                assert_eq!(
                    String::from_utf8_lossy(&out.stdout).trim(),
                    "tree-suspended=57",
                    "{}",
                    log(&out)
                );
                assert!(collections(&out) >= 2, "{}", log(&out));
                if fused {
                    assert!(
                        log(&out).contains("transformed_sites=1 helpers=1"),
                        "{}",
                        log(&out)
                    );
                }
            }
        }
    }
}
