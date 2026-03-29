mod common;

use common::{
    ash_cli_bin, compile_haxe_case, extract_checksum, load_parity_cases, normalize_text,
    parity_cases_file, parse_bool_env, parse_u64_env, render_output, repo_root, run_ash,
    run_hashlink, run_haxe_interp, tests_dir, unified_diff, AshMode, ExpectationKind,
    NormalizeKind, ParityCase, RunResult,
};
use std::fmt::Write as _;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::{Mutex, OnceLock};
use std::time::Duration;

#[derive(Clone)]
struct OracleRecord {
    source: String,
    stdout: String,
    stderr: String,
    exit_code: i32,
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

fn is_success(code: i32) -> bool {
    code == 0
}

fn status_code(output: &std::process::Output) -> i32 {
    output.status.code().unwrap_or(-1)
}

fn command_exists(name: &str) -> bool {
    Command::new("sh")
        .arg("-c")
        .arg(format!("command -v {} >/dev/null 2>&1", name))
        .status()
        .map(|s| s.success())
        .unwrap_or(false)
}

fn resolve_oracle_dir(path: PathBuf) -> Option<PathBuf> {
    if path.join("parity_manifest.json").exists() && path.join("cases").exists() {
        return Some(path);
    }
    let entries = std::fs::read_dir(&path).ok()?;
    for entry in entries.flatten() {
        let child = entry.path();
        if child.is_dir()
            && child.join("parity_manifest.json").exists()
            && child.join("cases").exists()
        {
            return Some(child);
        }
    }
    None
}

fn load_oracle_record(oracle_dir: &Path, case: &ParityCase) -> Option<OracleRecord> {
    let base = oracle_dir.join("cases").join(&case.name);
    let stdout_path = base.join("hashlink.stdout");
    let stderr_path = base.join("hashlink.stderr");
    let exit_path = base.join("exit_code.txt");
    if !stdout_path.exists() || !stderr_path.exists() || !exit_path.exists() {
        return None;
    }
    let stdout = std::fs::read_to_string(&stdout_path).ok()?;
    let stderr = std::fs::read_to_string(&stderr_path).ok()?;
    let exit_code = std::fs::read_to_string(&exit_path)
        .ok()?
        .trim()
        .parse::<i32>()
        .ok()?;
    Some(OracleRecord {
        source: "oracle-artifact".to_string(),
        stdout,
        stderr,
        exit_code,
    })
}

fn validate_output(
    case: &ParityCase,
    mode_name: &str,
    oracle: &OracleRecord,
    ash: &RunResult,
) -> Option<String> {
    let expectation = if oracle.source == "haxe-interp" {
        case.fallback_expectation.unwrap_or(case.expectation)
    } else {
        case.expectation
    };
    let ash_code = status_code(&ash.output);
    let ash_stdout = String::from_utf8_lossy(&ash.output.stdout).to_string();
    let ash_stderr = String::from_utf8_lossy(&ash.output.stderr).to_string();

    let expected_exit = case.expected_exit.unwrap_or(oracle.exit_code);
    if ash_code != expected_exit {
        return Some(format!(
            "[EXIT MISMATCH][{}][{}] source={} expected={} actual={}\nash:\n{}",
            mode_name,
            case.name,
            oracle.source,
            expected_exit,
            ash_code,
            render_output(&ash.output)
        ));
    }

    match expectation {
        ExpectationKind::ExitOnly => None,
        ExpectationKind::Checksum => {
            let exp = extract_checksum(&oracle.stdout);
            let got = extract_checksum(&ash_stdout);
            if exp != got {
                Some(format!(
                    "[CHECKSUM MISMATCH][{}][{}] source={} expected={:?} actual={:?}\nash:\n{}",
                    mode_name,
                    case.name,
                    oracle.source,
                    exp,
                    got,
                    render_output(&ash.output)
                ))
            } else {
                None
            }
        }
        ExpectationKind::Exact => {
            let exp_stdout = normalize_text(&oracle.stdout, case.normalize);
            let got_stdout = normalize_text(&ash_stdout, case.normalize);
            if exp_stdout != got_stdout {
                return Some(format!(
                    "[STDOUT MISMATCH][{}][{}] source={} normalize={:?}\n{}",
                    mode_name,
                    case.name,
                    oracle.source,
                    case.normalize,
                    unified_diff(&exp_stdout, &got_stdout)
                ));
            }

            let stderr_norm = match case.normalize {
                NormalizeKind::JsonWs => NormalizeKind::Default,
                other => other,
            };
            let exp_stderr = normalize_text(&oracle.stderr, stderr_norm);
            let got_stderr = normalize_text(&ash_stderr, stderr_norm);
            if exp_stderr != got_stderr {
                return Some(format!(
                    "[STDERR MISMATCH][{}][{}] source={} normalize={:?}\n{}",
                    mode_name,
                    case.name,
                    oracle.source,
                    stderr_norm,
                    unified_diff(&exp_stderr, &got_stderr)
                ));
            }

            None
        }
    }
}

fn run_parity_matrix(mode: AshMode) {
    let mode_name = match mode {
        AshMode::Interp => "interp",
        AshMode::Hybrid { .. } => "hybrid",
    };

    let tests_dir = tests_dir();
    let ash_cli = ash_cli_bin();
    assert!(
        ash_cli.exists(),
        "ash_cli binary not found at {}",
        ash_cli.display()
    );

    let include_slow = parse_bool_env("ASH_PARITY_INCLUDE_SLOW", false);
    let slow_timeout_secs = parse_u64_env("ASH_PARITY_SLOW_TIMEOUT_SECS", 240);
    let require_oracle = parse_bool_env("ASH_PARITY_REQUIRE_ORACLE", false);
    let use_local_hl = parse_bool_env("ASH_PARITY_USE_LOCAL_HL", false);
    let oracle_dir = std::env::var("ASH_PARITY_ORACLE_DIR")
        .ok()
        .map(|s| {
            let p = PathBuf::from(s);
            if p.is_absolute() {
                p
            } else {
                repo_root().join(p)
            }
        })
        .or_else(|| {
            let d = repo_root().join("target/parity-oracle");
            if d.exists() {
                Some(d)
            } else {
                None
            }
        })
        .and_then(resolve_oracle_dir);

    let mut failures = Vec::new();

    for case in load_parity_cases(&parity_cases_file()) {
        if case.slow && !include_slow {
            continue;
        }

        let compile = compile_haxe_case(&tests_dir, &case);
        if !compile.status.success() {
            failures.push(format!(
                "[COMPILE FAIL][{}][{}] {}\n{}",
                mode_name,
                case.name,
                case.hl,
                render_output(&compile)
            ));
            continue;
        }

        let sanity = run_haxe_interp(&tests_dir, &case);
        if let Some(sanity_out) = &sanity {
            if !sanity_out.status.success() {
                failures.push(format!(
                    "[HAXE INTERP FAIL][{}][{}]\n{}",
                    mode_name,
                    case.name,
                    render_output(sanity_out)
                ));
                continue;
            }
        }

        let hl_path = tests_dir.join(&case.hl);
        let timeout = if case.slow {
            Some(Duration::from_secs(slow_timeout_secs))
        } else {
            Some(Duration::from_secs(case.timeout_secs))
        };

        let ash_run = run_ash(&ash_cli, &hl_path, mode, timeout);
        if ash_run.timed_out {
            let actual_timeout = if case.slow {
                slow_timeout_secs
            } else {
                case.timeout_secs
            };
            failures.push(format!(
                "[ASH TIMEOUT][{}][{}] exceeded {}s",
                mode_name, case.name, actual_timeout
            ));
            continue;
        }

        let mut oracle = oracle_dir
            .as_ref()
            .and_then(|d| load_oracle_record(d, &case));

        if oracle.is_none() && use_local_hl && command_exists("hl") {
            let local_hl = run_hashlink(&hl_path, timeout);
            if local_hl.timed_out {
                failures.push(format!(
                    "[HL TIMEOUT][{}][{}] exceeded {}s",
                    mode_name, case.name, case.timeout_secs
                ));
                continue;
            }
            oracle = Some(OracleRecord {
                source: "local-hl".to_string(),
                stdout: String::from_utf8_lossy(&local_hl.output.stdout).to_string(),
                stderr: String::from_utf8_lossy(&local_hl.output.stderr).to_string(),
                exit_code: status_code(&local_hl.output),
            });
        }

        if oracle.is_none() && require_oracle {
            failures.push(format!(
                "[ORACLE MISSING][{}][{}] set ASH_PARITY_ORACLE_DIR to artifact bundle",
                mode_name, case.name
            ));
            continue;
        }

        if oracle.is_none() {
            if let Some(sanity_out) = sanity {
                oracle = Some(OracleRecord {
                    source: "haxe-interp".to_string(),
                    stdout: String::from_utf8_lossy(&sanity_out.stdout).to_string(),
                    stderr: String::from_utf8_lossy(&sanity_out.stderr).to_string(),
                    exit_code: status_code(&sanity_out),
                });
            }
        }

        if let Some(oracle_record) = oracle {
            if let Some(err) = validate_output(&case, mode_name, &oracle_record, &ash_run) {
                failures.push(err);
            }
        } else {
            let code = status_code(&ash_run.output);
            if !is_success(code) {
                failures.push(format!(
                    "[ASH FAIL][{}][{}] (no oracle available)\n{}",
                    mode_name,
                    case.name,
                    render_output(&ash_run.output)
                ));
            }
        }
    }

    if !failures.is_empty() {
        let mut msg = format!(
            "parity_matrix_{} had {} failure(s):\n",
            mode_name,
            failures.len()
        );
        for item in &failures {
            let _ = writeln!(&mut msg, "\n{}", item);
        }
        panic!("{}", msg);
    }
}

#[test]
fn parity_matrix_interp() {
    let _guard = lock_matrix();
    run_parity_matrix(AshMode::Interp);
}

#[test]
fn parity_matrix_hybrid_default() {
    let _guard = lock_matrix();
    run_parity_matrix(AshMode::Hybrid {
        jit_threshold: 100,
        jit_max_args: 8,
        jit_min_ops: 0,
        jit_log: false,
    });
}

#[test]
fn parity_matrix_hybrid_promotion_heavy() {
    let _guard = lock_matrix();
    run_parity_matrix(AshMode::Hybrid {
        jit_threshold: 1,
        jit_max_args: 8,
        jit_min_ops: 0,
        jit_log: false,
    });
}
