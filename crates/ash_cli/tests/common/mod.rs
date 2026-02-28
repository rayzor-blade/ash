use std::fmt::Write as _;
use std::path::{Path, PathBuf};
use std::process::{Command, Output, Stdio};
use std::thread;
use std::time::{Duration, Instant};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ExpectationKind {
    Exact,
    Checksum,
    ExitOnly,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum NormalizeKind {
    Default,
    JsonWs,
    None,
}

#[derive(Clone, Debug)]
pub struct ParityCase {
    pub name: String,
    pub main: String,
    pub hl: String,
    pub slow: bool,
    pub timeout_secs: u64,
    pub expectation: ExpectationKind,
    pub fallback_expectation: Option<ExpectationKind>,
    pub normalize: NormalizeKind,
    pub compile: bool,
    pub sanity_interp: bool,
    /// Override the oracle's exit code for comparison (e.g. when HL is known to fail).
    pub expected_exit: Option<i32>,
}

#[derive(Clone, Copy, Debug)]
pub enum AshMode {
    Interp,
    Hybrid {
        jit_threshold: u64,
        jit_max_args: usize,
        jit_min_ops: usize,
        jit_log: bool,
    },
}

pub struct RunResult {
    pub output: Output,
    pub timed_out: bool,
}

pub fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .expect("repo root")
}

pub fn tests_dir() -> PathBuf {
    repo_root().join("crates/ash/test/tests")
}

pub fn parity_cases_file() -> PathBuf {
    tests_dir().join("parity_cases.toml")
}

pub fn ash_cli_bin() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_ash_cli"))
}

pub fn parse_bool_env(name: &str, default: bool) -> bool {
    std::env::var(name)
        .map(|v| v == "1" || v.eq_ignore_ascii_case("true") || v.eq_ignore_ascii_case("yes"))
        .unwrap_or(default)
}

pub fn parse_u64_env(name: &str, default: u64) -> u64 {
    std::env::var(name)
        .ok()
        .and_then(|v| v.parse::<u64>().ok())
        .unwrap_or(default)
}

pub fn run(mut cmd: Command) -> Output {
    cmd.output().unwrap_or_else(|e| {
        panic!("failed to run command {:?}: {}", cmd, e);
    })
}

pub fn run_with_timeout(mut cmd: Command, timeout: Duration) -> RunResult {
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

pub fn run_with_optional_timeout(cmd: Command, timeout: Option<Duration>) -> RunResult {
    if let Some(t) = timeout {
        run_with_timeout(cmd, t)
    } else {
        RunResult {
            output: run(cmd),
            timed_out: false,
        }
    }
}

pub fn render_output(output: &Output) -> String {
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

pub fn compile_haxe_case(tests_dir: &Path, case: &ParityCase) -> Output {
    if !case.compile {
        let mut cmd = Command::new("sh");
        cmd.arg("-c").arg("true");
        return run(cmd);
    }
    let mut cmd = Command::new("haxe");
    cmd.arg("--cwd")
        .arg(tests_dir)
        .arg("-main")
        .arg(&case.main)
        .arg("-hl")
        .arg(&case.hl);
    run(cmd)
}

pub fn run_haxe_interp(tests_dir: &Path, case: &ParityCase) -> Option<Output> {
    if !case.sanity_interp {
        return None;
    }
    let mut cmd = Command::new("haxe");
    cmd.arg("--cwd")
        .arg(tests_dir)
        .arg("-main")
        .arg(&case.main)
        .arg("--interp");
    Some(run(cmd))
}

pub fn run_hashlink(hl_path: &Path, timeout: Option<Duration>) -> RunResult {
    let mut cmd = Command::new("hl");
    cmd.arg(hl_path);
    run_with_optional_timeout(cmd, timeout)
}

pub fn run_ash(
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
    cmd.arg("--quiet");
    cmd.arg(hl_path);
    run_with_optional_timeout(cmd, timeout)
}

fn parse_string(raw: &str) -> String {
    let trimmed = raw.trim();
    if trimmed.starts_with('"') && trimmed.ends_with('"') && trimmed.len() >= 2 {
        trimmed[1..trimmed.len() - 1].replace("\\\"", "\"")
    } else {
        trimmed.to_string()
    }
}

fn parse_bool(raw: &str) -> bool {
    matches!(raw.trim(), "true" | "True" | "TRUE")
}

fn parse_u64(raw: &str) -> u64 {
    raw.trim().parse::<u64>().unwrap_or(0)
}

pub fn load_parity_cases(path: &Path) -> Vec<ParityCase> {
    let text = std::fs::read_to_string(path)
        .unwrap_or_else(|e| panic!("failed to read {}: {}", path.display(), e));
    let mut out = Vec::<ParityCase>::new();
    let mut cur: Option<ParityCase> = None;

    for (lineno, raw) in text.lines().enumerate() {
        let line = raw.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        if line == "[[case]]" {
            if let Some(c) = cur.take() {
                out.push(c);
            }
            cur = Some(ParityCase {
                name: String::new(),
                main: String::new(),
                hl: String::new(),
                slow: false,
                timeout_secs: 60,
                expectation: ExpectationKind::Exact,
                fallback_expectation: None,
                normalize: NormalizeKind::Default,
                compile: true,
                sanity_interp: true,
                expected_exit: None,
            });
            continue;
        }
        if cur.is_none() {
            // Global keys (e.g. schema_version) are ignored for now.
            continue;
        }

        let Some((k, v)) = line.split_once('=') else {
            panic!("invalid parity_cases.toml line {}: {}", lineno + 1, raw);
        };
        let key = k.trim();
        let value = v.trim();
        let c = cur.as_mut().expect("case should exist");
        match key {
            "name" => c.name = parse_string(value),
            "main" => c.main = parse_string(value),
            "hl" => c.hl = parse_string(value),
            "slow" => c.slow = parse_bool(value),
            "timeout_secs" => c.timeout_secs = parse_u64(value),
            "expectation" => {
                c.expectation = match parse_string(value).as_str() {
                    "exact" => ExpectationKind::Exact,
                    "checksum" => ExpectationKind::Checksum,
                    "exit_only" => ExpectationKind::ExitOnly,
                    other => panic!("invalid expectation '{}', line {}", other, lineno + 1),
                }
            }
            "fallback_expectation" => {
                c.fallback_expectation = Some(match parse_string(value).as_str() {
                    "exact" => ExpectationKind::Exact,
                    "checksum" => ExpectationKind::Checksum,
                    "exit_only" => ExpectationKind::ExitOnly,
                    other => panic!(
                        "invalid fallback_expectation '{}', line {}",
                        other,
                        lineno + 1
                    ),
                })
            }
            "normalize" => {
                c.normalize = match parse_string(value).as_str() {
                    "default" => NormalizeKind::Default,
                    "json_ws" => NormalizeKind::JsonWs,
                    "none" => NormalizeKind::None,
                    other => panic!("invalid normalize '{}', line {}", other, lineno + 1),
                }
            }
            "compile" => c.compile = parse_bool(value),
            "sanity_interp" => c.sanity_interp = parse_bool(value),
            "expected_exit" => {
                c.expected_exit = Some(value.trim().parse::<i32>().unwrap_or_else(|_| {
                    panic!("invalid expected_exit '{}', line {}", value, lineno + 1)
                }))
            }
            "schema_version" => {}
            _ => panic!(
                "unknown key '{}' in parity_cases.toml line {}",
                key,
                lineno + 1
            ),
        }
    }

    if let Some(c) = cur.take() {
        out.push(c);
    }

    for c in &out {
        assert!(
            !c.name.is_empty(),
            "case missing name in {}",
            path.display()
        );
        assert!(!c.hl.is_empty(), "case {} missing hl", c.name);
        if c.compile || c.sanity_interp {
            assert!(
                !c.main.is_empty(),
                "case {} requires main when compile/sanity are enabled",
                c.name
            );
        }
    }

    out
}

pub fn normalize_text(s: &str, mode: NormalizeKind) -> String {
    let unix = s.replace("\r\n", "\n");
    match mode {
        NormalizeKind::None => unix,
        NormalizeKind::Default => unix
            .lines()
            .map(str::trim_end)
            .collect::<Vec<_>>()
            .join("\n")
            .trim_end_matches('\n')
            .to_string(),
        NormalizeKind::JsonWs => unix.split_whitespace().collect::<Vec<_>>().join(" "),
    }
}

pub fn extract_checksum(s: &str) -> Option<String> {
    for line in s.lines() {
        if let Some(rest) = line.split_once("Checksum:") {
            return Some(rest.1.trim().to_string());
        }
    }
    None
}

pub fn unified_diff(expected: &str, actual: &str) -> String {
    let exp_lines: Vec<&str> = expected.lines().collect();
    let act_lines: Vec<&str> = actual.lines().collect();
    let max = exp_lines.len().max(act_lines.len());
    let mut out = String::new();
    let _ = writeln!(&mut out, "--- expected");
    let _ = writeln!(&mut out, "+++ actual");
    for i in 0..max {
        let e = exp_lines.get(i).copied();
        let a = act_lines.get(i).copied();
        if e == a {
            continue;
        }
        let ln = i + 1;
        if let Some(ev) = e {
            let _ = writeln!(&mut out, "-{:04}: {}", ln, ev);
        }
        if let Some(av) = a {
            let _ = writeln!(&mut out, "+{:04}: {}", ln, av);
        }
    }
    out
}
