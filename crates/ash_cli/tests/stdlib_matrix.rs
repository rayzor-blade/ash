mod common;

use common::{
    ash_cli_bin, compile_haxe_case, load_parity_cases, parity_cases_file, parse_bool_env,
    parse_u64_env, render_output, run_ash, run_haxe_interp, AshMode, ParityCase,
};
use std::collections::HashSet;
use std::fmt::Write as _;
use std::sync::{Mutex, OnceLock};
use std::time::Duration;

fn smoke_case_names() -> HashSet<&'static str> {
    [
        "TestStdlib",
        "TestJsonParse",
        "TestJsonMin",
        "TestStdStringTools",
        "TestStdBytes",
        "TestStdReflectType",
        "TestStdDate",
        "TestStdEReg",
        "TestFeatureOO",
        "TestFeatureEnumsPattern",
        "TestFeatureGenerics",
        "TestFeatureAbstracts",
        "TestFeatureIterators",
        "TestFeatureTypedefAnon",
        "TestFeatureNullCasts",
        "TestTieredHotLoop",
        "Mandelbrot",
    ]
    .into_iter()
    .collect()
}

fn load_smoke_cases() -> Vec<ParityCase> {
    let selected = smoke_case_names();
    let mut cases = load_parity_cases(&parity_cases_file())
        .into_iter()
        .filter(|c| selected.contains(c.name.as_str()))
        .collect::<Vec<_>>();
    cases.sort_by(|a, b| a.name.cmp(&b.name));
    cases
}

fn run_matrix(mode: AshMode) {
    let mode_name = match mode {
        AshMode::Interp => "interp",
        AshMode::Hybrid { .. } => "hybrid",
    };

    let tests_dir = common::tests_dir();
    let ash_cli = ash_cli_bin();
    assert!(
        ash_cli.exists(),
        "ash_cli binary not found at {}",
        ash_cli.display()
    );

    let include_slow = parse_bool_env("ASH_STDLIB_INCLUDE_SLOW", false);
    let slow_timeout_secs = parse_u64_env("ASH_STDLIB_SLOW_TIMEOUT_SECS", 120);

    let mut unexpected = Vec::new();

    for case in load_smoke_cases() {
        if case.slow && !include_slow {
            continue;
        }

        let compile = compile_haxe_case(&tests_dir, &case);
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

        if let Some(baseline) = run_haxe_interp(&tests_dir, &case) {
            if !baseline.status.success() {
                unexpected.push(format!(
                    "[BASELINE FAIL][{}] haxe --interp {}\n{}",
                    mode_name,
                    case.main,
                    render_output(&baseline)
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
            unexpected.push(format!(
                "[ASH TIMEOUT][{}] {} ({}) exceeded {}s",
                mode_name, case.name, case.hl, slow_timeout_secs
            ));
            continue;
        }

        if !ash_run.output.status.success() {
            unexpected.push(format!(
                "[ASH FAIL][{}] {} ({}) expected pass\n{}",
                mode_name,
                case.name,
                case.hl,
                render_output(&ash_run.output)
            ));
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
        jit_min_ops: 0,
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
    let tests_dir = common::tests_dir();
    let ash_cli = ash_cli_bin();

    let case = load_parity_cases(&parity_cases_file())
        .into_iter()
        .find(|c| c.name == "TestTieredHotLoop")
        .expect("TestTieredHotLoop case missing from parity_cases.toml");

    let compile = compile_haxe_case(&tests_dir, &case);
    assert!(
        compile.status.success(),
        "failed to compile hybrid observability case:\n{}",
        render_output(&compile)
    );

    let hl_path = tests_dir.join(&case.hl);
    let run = run_ash(
        &ash_cli,
        &hl_path,
        AshMode::Hybrid {
            jit_threshold: 1,
            jit_max_args: 8,
            jit_min_ops: 0,
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
