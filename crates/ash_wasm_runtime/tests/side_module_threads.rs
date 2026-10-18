//! A side module's functions are callable from a thread, not only from the
//! thread that loaded it.
//!
//! A thread is a second instance of the program, and a table belongs to an
//! instance. The index a primitive was given in the first instance's table
//! is kept in shared memory and used by whichever thread calls it, so the
//! thread's own table has to hold the same function at the same index. The
//! library below counts in its own data: the thread's call must see the
//! count the main thread left, which a second instantiation that wrote the
//! library's initial data again would reset.
#![cfg(feature = "native")]
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

const PROGRAM: &str = r#"(module
  (import "env" "memory" (memory 1 1 shared))
  (import "env" "ash_host_dlsym"
    (func $dlsym (param i32 i32 i32 i32) (result i32)))
  (import "wasi" "thread-spawn" (func $spawn (param i32) (result i32)))
  (export "memory" (memory 0))
  (table (export "__indirect_function_table") 1 funcref)
  (type $answer (func (result i32)))
  (global $brk (mut i32) (i32.const 4096))
  (data (i32.const 64) "library")
  (data (i32.const 96) "answer")
  (func (export "malloc") (param $n i32) (result i32)
    (local $p i32)
    (local.set $p (global.get $brk))
    (global.set $brk (i32.add (global.get $brk) (i32.const 64)))
    (local.get $p))
  ;; The thread calls the index main stored at 256, records what it
  ;; returned at 260, and sets the flag at 264.
  (func (export "wasi_thread_start") (param $tid i32) (param $arg i32)
    (i32.store (i32.const 260)
      (call_indirect (type $answer) (i32.load (i32.const 256))))
    (i32.atomic.store (i32.const 264) (i32.const 1))
    (drop (memory.atomic.notify (i32.const 264) (i32.const 1))))
  (func (export "main") (param i32 i32) (result i32)
    (local $index i32)
    (local.set $index
      (call $dlsym (i32.const 64) (i32.const 7) (i32.const 96) (i32.const 6)))
    (if (i32.eqz (local.get $index)) (then (return (i32.const 10))))
    (i32.store (i32.const 256) (local.get $index))
    (if (i32.ne (call_indirect (type $answer) (local.get $index)) (i32.const 42))
      (then (return (i32.const 11))))
    (drop (call $spawn (i32.const 0)))
    (drop (memory.atomic.wait32 (i32.const 264) (i32.const 0) (i64.const 10000000000)))
    (if (i32.ne (i32.load (i32.const 260)) (i32.const 43))
      (then (return (i32.const 12))))
    (i32.const 0)))"#;

/// `dylink.0` memory info: 4 bytes of data, aligned to 2^2, no table.
const LIBRARY: &str = r#"(module
  (@custom "dylink.0" (before first) "\01\04\04\02\00\00")
  (import "env" "memory" (memory 1 1 shared))
  (import "env" "__memory_base" (global $base i32))
  (data (global.get $base) "\29\00\00\00")
  (func (export "answer") (result i32)
    (i32.store (global.get $base)
      (i32.add (i32.load (global.get $base)) (i32.const 1)))
    (i32.load (global.get $base))))"#;

/// Write the program and its library into a fresh directory.
fn files() -> (tempfile::TempDir, std::path::PathBuf, std::path::PathBuf) {
    let dir = tempfile::tempdir().unwrap();
    let program = dir.path().join("program.wasm");
    let library = dir.path().join("library.wasm");
    std::fs::write(&program, wat::parse_str(PROGRAM).unwrap()).unwrap();
    std::fs::write(&library, wat::parse_str(LIBRARY).unwrap()).unwrap();
    (dir, program, library)
}

#[test]
fn a_thread_calls_a_primitive_by_the_index_another_thread_resolved() {
    let (_dir, program, _library) = files();
    let mut command = Command::new(env!("CARGO_BIN_EXE_ash-wasm-run"));
    command.arg("--threads").arg(&program);
    finish(command);
}

// Build the nodejs browser bindings and copy examples/browser/run-node*.js
// alongside them, then set ASH_BROWSER_HOST_DIR to that directory and run
// `cargo test -p ash_wasm_runtime --test side_module_threads -- --ignored`.
#[test]
#[ignore = "requires Node and generated browser bindings in ASH_BROWSER_HOST_DIR"]
fn browser_threads_load_the_same_libraries() {
    let host = std::path::PathBuf::from(std::env::var_os("ASH_BROWSER_HOST_DIR").expect(
        "set ASH_BROWSER_HOST_DIR to the directory containing run-node.js and ash_browser.js",
    ));
    let (dir, program, library) = files();
    let mut command = Command::new("node");
    command
        .arg("--experimental-wasm-exnref")
        .arg(host.join("run-node.js"))
        .args(["--agents", "2"])
        .arg("--lib")
        .arg(format!("library={}", library.display()))
        .arg(&program);
    finish(command);
    drop(dir);
}

/// Run the command to completion and require a clean exit.
fn finish(mut command: Command) {
    let mut child = command
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    let deadline = Instant::now() + Duration::from_secs(30);
    loop {
        if child.try_wait().unwrap().is_some() {
            break;
        }
        if Instant::now() >= deadline {
            child.kill().unwrap();
            panic!("the host did not finish");
        }
        std::thread::sleep(Duration::from_millis(10));
    }
    let output = child.wait_with_output().unwrap();
    let stderr = String::from_utf8_lossy(&output.stderr);
    // 11: the main thread's own call; 12: the thread's. Anything else is a
    // trap, and the message says which.
    assert_eq!(output.status.code(), Some(0), "{stderr}");
}
