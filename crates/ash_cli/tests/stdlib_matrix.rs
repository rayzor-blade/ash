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
    // Promotion is ASYNC: an install can land after the program's final
    // call, in which case compiled_calls is legitimately zero for that run —
    // the counters are a race by design. This probe asserts the machinery
    // CAN be observed working, so it retries a bounded number of times; a
    // real regression still fails every attempt.
    let mut last: Option<(u64, u64, u64, String)> = None;
    for _ in 0..3 {
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
        let stderr = String::from_utf8_lossy(&run.output.stderr).into_owned();
        let attempted = parse_metric(&stderr, "attempted").unwrap_or(0);

        let compiled_calls = parse_metric(&stderr, "compiled_calls").unwrap_or(0);
        // `compiled_calls` counts INTERPRETER -> compiled dispatches, and it
        // legitimately reaches zero now that the Cranelift tier compiles
        // 99.4% of a program: once the entrypoint itself is compiled, the
        // rest of the run happens inside compiled code and the interpreter
        // never dispatches into it again. The promotion counters are what
        // still observe the ladder, so assert on those; `compiled_calls` is
        // reported for the failure message but no longer gates.
        let cranelift = parse_metric(&stderr, "cranelift").unwrap_or(0);
        let llvm = parse_metric(&stderr, "llvm").unwrap_or(0);
        if attempted > 0 && cranelift + llvm > 0 {
            last = None;
            break;
        }
        last = Some((attempted, cranelift + llvm, compiled_calls, stderr));
    }
    if let Some((attempted, succeeded, compiled_calls, stderr)) = last {
        panic!(
            "expected visible hybrid promotions in 3 attempts, last got attempted={attempted} promotions={succeeded} compiled_calls={compiled_calls}\nstderr:\n{stderr}"
        );
    }
}
