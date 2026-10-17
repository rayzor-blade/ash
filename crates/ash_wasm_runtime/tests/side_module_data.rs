//! A side module's own data symbols are addressed relative to its memory base.
//!
//! A position-independent module exports each data symbol as a global holding
//! an offset from `__memory_base`, and reaches it through a `GOT.mem` import
//! that the loader must fill with the ABSOLUTE address. The library below
//! keeps a value at the start of its data and reads it back through `GOT.mem`
//! in its constructor, which traps unless it finds it; with the offset loaded
//! as an address it reads the program's own memory instead, the host reports
//! "cannot load", and the program carries on regardless.
#![cfg(feature = "native")]
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

const PROGRAM: &str = r#"(module
  (memory (export "memory") 2)
  (table (export "__indirect_function_table") 1 funcref)
  (global $brk (mut i32) (i32.const 4096))
  (func (export "malloc") (param $n i32) (result i32)
    (local $p i32)
    (local.set $p (global.get $brk))
    (global.set $brk (i32.add (global.get $brk) (i32.const 64)))
    (local.get $p))
  (func (export "main") (param i32 i32) (result i32) i32.const 0))"#;

/// `dylink.0` memory info: 8 bytes of data, aligned to 2^3, no table.
const LIBRARY: &str = r#"(module
  (@custom "dylink.0" (before first) "\01\04\08\03\00\00")
  (import "env" "memory" (memory 1))
  (import "env" "__memory_base" (global $base i32))
  (import "GOT.mem" "value" (global $got (mut i32)))
  (global (export "value") i32 (i32.const 0))
  (data (global.get $base) "\2a\00\00\00")
  (func (export "__wasm_call_ctors")
    (if (i32.ne (i32.load (global.get $got)) (i32.const 42))
      (then unreachable))))"#;

#[test]
fn a_data_symbol_the_library_defines_resolves_to_its_own_data() {
    let dir = tempfile::tempdir().unwrap();
    let program = dir.path().join("program.wasm");
    std::fs::write(&program, wat::parse_str(PROGRAM).unwrap()).unwrap();
    std::fs::write(
        dir.path().join("library.wasm"),
        wat::parse_str(LIBRARY).unwrap(),
    )
    .unwrap();
    let mut child = Command::new(env!("CARGO_BIN_EXE_ash-wasm-run"))
        .arg(&program)
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
    assert!(
        stderr.contains("loaded native library: library") && !stderr.contains("cannot load"),
        "GOT.mem held the offset, not the address\n{stderr}"
    );
}
