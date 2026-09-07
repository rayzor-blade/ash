//! A dead worker must terminate the program, not leave main in a futex.
#![cfg(feature = "native")]
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

fn run(wat: &str) -> std::process::Output {
    let mut command = Command::new(env!("CARGO_BIN_EXE_ash-wasm-run"));
    command.arg("--threads");
    run_with(wat, command)
}

fn run_with(wat: &str, mut command: Command) -> std::process::Output {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("threads.wasm");
    std::fs::write(&path, wat::parse_str(wat).unwrap()).unwrap();
    let mut child = command
        .arg(path)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    let deadline = Instant::now() + Duration::from_secs(15);
    loop {
        if child.try_wait().unwrap().is_some() {
            return child.wait_with_output().unwrap();
        }
        if Instant::now() >= deadline {
            child.kill().unwrap();
            let output = child.wait_with_output().unwrap();
            panic!(
                "guest did not terminate: {}",
                String::from_utf8_lossy(&output.stderr)
            );
        }
        std::thread::sleep(Duration::from_millis(10));
    }
}

fn program(main_wait: &str, fatal: &str) -> String {
    format!(
        r#"(module
      (import "env" "memory" (memory 1 1 shared))
      (import "wasi" "thread-spawn" (func $spawn (param i32) (result i32)))
      (import "wasi_snapshot_preview1" "proc_exit" (func $exit (param i32)))
      (export "memory" (memory 0))
      (func (export "wasi_thread_start") (param $tid i32) (param $arg i32)
        local.get $arg i32.eqz
        if
          i32.const 8 i64.const 0 i64.const -1 memory.atomic.wait64 drop
        else {fatal} end)
      (func (export "main") (param i32 i32) (result i32)
        i32.const 0 call $spawn drop
        i32.const 1 call $spawn drop
        {main_wait}
        i32.const 99))"#
    )
}

#[test]
fn worker_proc_exit_wakes_main_and_siblings_in_atomic_waits() {
    for status in [0, 23] {
        let output = run(&program(
            "i32.const 0 i32.const 0 i64.const -1 memory.atomic.wait32 offset=4 drop",
            &format!("i32.const {status} call $exit unreachable"),
        ));
        assert_eq!(
            output.status.code(),
            Some(status),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
    }
}

#[test]
fn worker_trap_wakes_main_and_reports_the_original_failure() {
    let output = run(&program(
        "i32.const 0 i32.const 0 i64.const -1 memory.atomic.wait32 drop",
        "unreachable",
    ));
    assert_eq!(output.status.code(), Some(70));
    let error = String::from_utf8_lossy(&output.stderr);
    assert!(error.contains("unreachable"), "{error}");
    assert!(error.contains("thread"), "{error}");
}

#[test]
fn worker_exit_interrupts_main_in_a_compute_loop() {
    let output = run(&program(
        "loop br 0 end",
        "i32.const 24 call $exit unreachable",
    ));
    assert_eq!(
        output.status.code(),
        Some(24),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn wait_timeouts_mismatches_offsets_and_indirect_calls_are_preserved() {
    let output = run(WAIT_SEMANTICS);
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn waits_also_work_with_a_defined_shared_memory() {
    let program = WAIT_SEMANTICS.replace(
        "(import \"env\" \"memory\" (memory 1 1 shared))",
        "(memory 1 1 shared)",
    );
    let output = run(&program);
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
}

// Build the nodejs browser bindings and copy examples/browser/run-node*.js
// alongside them, then set ASH_BROWSER_HOST_DIR to that directory and run
// `cargo test -p ash_wasm_runtime --test thread_exit -- --ignored`.
#[test]
#[ignore = "requires Node and generated browser bindings in ASH_BROWSER_HOST_DIR"]
fn browser_worker_failures_wake_waits_and_preserve_wait_semantics() {
    let host = std::path::PathBuf::from(std::env::var_os("ASH_BROWSER_HOST_DIR").expect(
        "set ASH_BROWSER_HOST_DIR to the directory containing run-node.js and ash_browser.js",
    ));
    let run_browser = |wat: &str| {
        let mut command = Command::new("node");
        command
            .arg("--experimental-wasm-exnref")
            .arg(host.join("run-node.js"))
            .args(["--agents", "2"]);
        run_with(wat, command)
    };
    for status in [0, 23] {
        let output = run_browser(&program(
            "i32.const 0 i32.const 0 i64.const -1 memory.atomic.wait32 offset=4 drop",
            &format!("i32.const {status} call $exit unreachable"),
        ));
        assert_eq!(
            output.status.code(),
            Some(status),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
    }
    let output = run_browser(&program(
        "i32.const 0 i32.const 0 i64.const -1 memory.atomic.wait32 drop",
        "unreachable",
    ));
    assert_eq!(output.status.code(), Some(1));
    let error = String::from_utf8_lossy(&output.stderr);
    assert!(
        error.contains("thread") && error.contains("unreachable"),
        "{error}"
    );
    let output = run_browser(WAIT_SEMANTICS);
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    // Browsers have no epoch interrupt: an arbitrary loop without host calls
    // needs its owner to terminate the Worker, unlike the native test above.
}

const WAIT_SEMANTICS: &str = r#"(module
      (type $result (func (result i32)))
      (import "env" "memory" (memory 1 1 shared))
      (export "memory" (memory 0))
      (table 1 funcref) (elem (i32.const 0) $answer)
      (func $answer (result i32) i32.const 17)
      (func (export "main") (param i32 i32) (result i32)
        i32.const 0 i32.const 1 i64.const -1 memory.atomic.wait32 offset=4
        i32.const 1 i32.ne if unreachable end
        i32.const 0 i64.const 0 i64.const 0 memory.atomic.wait64 offset=8
        i32.const 2 i32.ne if unreachable end
        i32.const 0 call_indirect (type $result) i32.const 17 i32.ne if unreachable end
        i32.const 0))"#;
