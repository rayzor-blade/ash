use std::fmt::Write as _;
use std::path::{Path, PathBuf};
use std::process::{Command, Output, Stdio};
use std::sync::{Mutex, OnceLock};
use std::thread;
use std::time::{Duration, Instant};

#[derive(Clone, Copy)]
enum AshExpectation {
    Pass,
}

struct Case {
    main: &'static str,
    hl: &'static str,
    expectation: AshExpectation,
    slow: bool,
}

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .expect("repo root")
}

fn tests_dir() -> PathBuf {
    repo_root().join("crates/ash/test/tests")
}

fn ash_cli_bin() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_ash_cli"))
}

fn run(mut cmd: Command) -> Output {
    cmd.output().unwrap_or_else(|e| {
        panic!("failed to run command {:?}: {}", cmd, e);
    })
}

struct RunResult {
    output: Output,
    timed_out: bool,
}

#[derive(Clone, Copy)]
enum AshMode {
    Interp,
    Hybrid {
        jit_threshold: u64,
        jit_max_args: usize,
        jit_min_ops: usize,
        jit_log: bool,
    },
}

fn run_with_timeout(mut cmd: Command, timeout: Duration) -> RunResult {
    cmd.stdout(Stdio::piped()).stderr(Stdio::piped());
    let mut child = cmd.spawn().unwrap_or_else(|e| {
        panic!("failed to spawn command {:?}: {}", cmd, e);
    });

    let start = Instant::now();
    loop {
        match child.try_wait() {
            Ok(Some(_)) => {
                let output = child.wait_with_output().unwrap_or_else(|e| {
                    panic!("failed to collect command output: {}", e);
                });
                return RunResult {
                    output,
                    timed_out: false,
                };
            }
            Ok(None) => {
                if start.elapsed() >= timeout {
                    let _ = child.kill();
                    let output = child.wait_with_output().unwrap_or_else(|e| {
                        panic!("failed to collect timed-out command output: {}", e);
                    });
                    return RunResult {
                        output,
                        timed_out: true,
                    };
                }
                thread::sleep(Duration::from_millis(20));
            }
            Err(e) => {
                panic!("failed while waiting for command {:?}: {}", cmd, e);
            }
        }
    }
}

fn render_output(output: &Output) -> String {
    let mut s = String::new();
    let _ = writeln!(&mut s, "status: {}", output.status);
    let stdout = String::from_utf8_lossy(&output.stdout);
    if !stdout.trim().is_empty() {
        let _ = writeln!(&mut s, "stdout:\n{}", stdout);
    }
    let stderr = String::from_utf8_lossy(&output.stderr);
    if !stderr.trim().is_empty() {
        let _ = writeln!(&mut s, "stderr:\n{}", stderr);
    }
    s
}

fn compile_haxe_main(tests_dir: &Path, main: &str, hl_file: &str) -> Output {
    let mut cmd = Command::new("haxe");
    cmd.arg("--cwd")
        .arg(tests_dir)
        .arg("-main")
        .arg(main)
        .arg("-hl")
        .arg(hl_file);
    run(cmd)
}

fn run_haxe_interp(tests_dir: &Path, main: &str) -> Output {
    let mut cmd = Command::new("haxe");
    cmd.arg("--cwd")
        .arg(tests_dir)
        .arg("-main")
        .arg(main)
        .arg("--interp");
    run(cmd)
}

fn run_ash(
    ash_cli: &Path,
    hl_path: &Path,
    mode: AshMode,
    timeout: Option<Duration>,
) -> RunResult {
    let mut cmd = Command::new(ash_cli);
    match mode {
        AshMode::Interp => {
            cmd.arg("--mode").arg("interp");
        }
        AshMode::Hybrid {
            jit_threshold,
            jit_max_args,
            jit_min_ops,
            jit_log,
        } => {
            cmd.arg("--mode")
                .arg("hybrid")
                .arg("--jit-threshold")
                .arg(jit_threshold.to_string())
                .arg("--jit-max-args")
                .arg(jit_max_args.to_string())
                .arg("--jit-min-ops")
                .arg(jit_min_ops.to_string());
            if jit_log {
                cmd.arg("--jit-log");
            }
        }
    }
    cmd.arg(hl_path);
    if let Some(timeout) = timeout {
        run_with_timeout(cmd, timeout)
    } else {
        RunResult {
            output: run(cmd),
            timed_out: false,
        }
    }
}

fn run_matrix(mode: AshMode) {
    let mode_name = match mode {
        AshMode::Interp => "interp",
        AshMode::Hybrid { .. } => "hybrid",
    };

    let tests_dir = tests_dir();
    let ash_cli = ash_cli_bin();
    assert!(ash_cli.exists(), "ash_cli binary not found at {}", ash_cli.display());

    let include_slow = std::env::var("ASH_STDLIB_INCLUDE_SLOW")
        .map(|v| v == "1" || v.eq_ignore_ascii_case("true"))
        .unwrap_or(false);
    let slow_timeout_secs = std::env::var("ASH_STDLIB_SLOW_TIMEOUT_SECS")
        .ok()
        .and_then(|s| s.parse::<u64>().ok())
        .unwrap_or(120);

    let mut unexpected = Vec::new();

    for case in default_cases() {
        if case.slow && !include_slow {
            continue;
        }

        let compile = compile_haxe_main(&tests_dir, case.main, case.hl);
        if !compile.status.success() {
            unexpected.push(format!(
                "[COMPILE FAIL][{}] {} -> {}\n{}",
                mode_name,
                case.main,
                case.hl,
                render_output(&compile)
            ));
            continue;
        }

        let baseline = run_haxe_interp(&tests_dir, case.main);
        if !baseline.status.success() {
            unexpected.push(format!(
                "[BASELINE FAIL][{}] haxe --interp {}\n{}",
                mode_name,
                case.main,
                render_output(&baseline)
            ));
            continue;
        }

        let hl_path = tests_dir.join(case.hl);
        let timeout = if case.slow {
            Some(Duration::from_secs(slow_timeout_secs))
        } else {
            None
        };
        let ash_run = run_ash(&ash_cli, &hl_path, mode, timeout);
        if ash_run.timed_out {
            unexpected.push(format!(
                "[ASH TIMEOUT][{}] {} ({}) exceeded {}s",
                mode_name, case.main, case.hl, slow_timeout_secs
            ));
            continue;
        }
        let ash_output = ash_run.output;
        match case.expectation {
            AshExpectation::Pass => {
                if !ash_output.status.success() {
                    unexpected.push(format!(
                        "[ASH FAIL][{}] {} ({}) expected pass\n{}",
                        mode_name,
                        case.main,
                        case.hl,
                        render_output(&ash_output)
                    ));
                }
            }
        }
    }

    if !unexpected.is_empty() {
        let mut msg = format!("stdlib_matrix_{} had unexpected results:\n", mode_name);
        for item in &unexpected {
            let _ = writeln!(&mut msg, "\n{}", item);
        }
        panic!("{}", msg);
    }
}

fn default_cases() -> Vec<Case> {
    vec![
        Case {
            main: "TestStdlib",
            hl: "test_stdlib.hl",
            expectation: AshExpectation::Pass,
            slow: false,
        },
        Case {
            main: "TestJsonParse",
            hl: "test_jsonparse.hl",
            expectation: AshExpectation::Pass,
            slow: false,
        },
        Case {
            main: "TestJsonMin",
            hl: "test_jsonmin.hl",
            expectation: AshExpectation::Pass,
            slow: false,
        },
        Case {
            main: "TestStdStringTools",
            hl: "test_std_stringtools.hl",
            expectation: AshExpectation::Pass,
            slow: false,
        },
        Case {
            main: "TestStdBytes",
            hl: "test_std_bytes.hl",
            expectation: AshExpectation::Pass,
            slow: false,
        },
        Case {
            main: "TestStdReflectType",
            hl: "test_std_reflect_type.hl",
            expectation: AshExpectation::Pass,
            slow: false,
        },
        Case {
            main: "TestStdDate",
            hl: "test_std_date.hl",
            expectation: AshExpectation::Pass,
            slow: false,
        },
        Case {
            main: "TestStdEReg",
            hl: "test_std_ereg.hl",
            expectation: AshExpectation::Pass,
            slow: false,
        },
        Case {
            main: "TestFeatureOO",
            hl: "test_feature_oo.hl",
            expectation: AshExpectation::Pass,
            slow: false,
        },
        Case {
            main: "TestFeatureEnumsPattern",
            hl: "test_feature_enums_pattern.hl",
            expectation: AshExpectation::Pass,
            slow: false,
        },
        Case {
            main: "TestFeatureGenerics",
            hl: "test_feature_generics.hl",
            expectation: AshExpectation::Pass,
            slow: false,
        },
        Case {
            main: "TestFeatureAbstracts",
            hl: "test_feature_abstracts.hl",
            expectation: AshExpectation::Pass,
            slow: false,
        },
        Case {
            main: "TestFeatureIterators",
            hl: "test_feature_iterators.hl",
            expectation: AshExpectation::Pass,
            slow: false,
        },
        Case {
            main: "TestFeatureTypedefAnon",
            hl: "test_feature_typedef_anon.hl",
            expectation: AshExpectation::Pass,
            slow: false,
        },
        Case {
            main: "TestFeatureNullCasts",
            hl: "test_feature_null_casts.hl",
            expectation: AshExpectation::Pass,
            slow: false,
        },
        Case {
            main: "TestTieredHotLoop",
            hl: "test_tiered_hotloop.hl",
            expectation: AshExpectation::Pass,
            slow: false,
        },
        Case {
            main: "Mandelbrot",
            hl: "test_mandelbrot.hl",
            expectation: AshExpectation::Pass,
            slow: true,
        },
    ]
}

fn matrix_lock() -> &'static Mutex<()> {
    static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
    LOCK.get_or_init(|| Mutex::new(()))
}

fn lock_matrix() -> std::sync::MutexGuard<'static, ()> {
    match matrix_lock().lock() {
        Ok(g) => g,
        Err(poisoned) => poisoned.into_inner(),
    }
}

#[test]
fn stdlib_matrix_interp() {
    let _guard = lock_matrix();
    run_matrix(AshMode::Interp);
}

#[test]
fn stdlib_matrix_hybrid() {
    let _guard = lock_matrix();
    run_matrix(AshMode::Hybrid {
        jit_threshold: 5,
        jit_max_args: 8,
        jit_min_ops: 16,
        jit_log: false,
    });
}

fn parse_metric(stderr: &str, key: &str) -> Option<u64> {
    let needle = format!("{}=", key);
    stderr
        .split_whitespace()
        .find_map(|tok| tok.strip_prefix(&needle))
        .and_then(|v| v.parse::<u64>().ok())
}

#[test]
fn hybrid_promotions_observable() {
    let _guard = lock_matrix();
    let tests_dir = tests_dir();
    let ash_cli = ash_cli_bin();
    let case_main = "TestTieredHotLoop";
    let case_hl = "test_tiered_hotloop.hl";

    let compile = compile_haxe_main(&tests_dir, case_main, case_hl);
    assert!(
        compile.status.success(),
        "failed to compile hybrid observability case:\n{}",
        render_output(&compile)
    );

    let hl_path = tests_dir.join(case_hl);
    let run = run_ash(
        &ash_cli,
        &hl_path,
        AshMode::Hybrid {
            jit_threshold: 1,
            jit_max_args: 8,
            jit_min_ops: 1,
            jit_log: true,
        },
        Some(Duration::from_secs(120)),
    );
    assert!(!run.timed_out, "hybrid observability run timed out");
    assert!(
        run.output.status.success(),
        "hybrid observability run failed:\n{}",
        render_output(&run.output)
    );

    let stderr = String::from_utf8_lossy(&run.output.stderr);
    let attempted = parse_metric(&stderr, "attempted").unwrap_or(0);
    let succeeded = parse_metric(&stderr, "succeeded").unwrap_or(0);
    let compiled_calls = parse_metric(&stderr, "compiled_calls").unwrap_or(0);
    assert!(
        attempted > 0 && succeeded > 0 && compiled_calls > 0,
        "expected visible hybrid promotions, got attempted={} succeeded={} compiled_calls={}\nstderr:\n{}",
        attempted,
        succeeded,
        compiled_calls,
        stderr
    );
}
