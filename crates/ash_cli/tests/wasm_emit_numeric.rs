//! The direct wasm emitter, end to end: compile a Haxe program, emit its
//! numeric functions to wasm without LLVM, run them in wasmtime, and check
//! every result against the interpreter's output for the same program.
mod common;

use ash_core::bytecode::{BytecodeDecoder, DecodedBytecode};
use ash_core::wasm_direct::BytecodeProgram;
use ash_wasm_emit::{Input, Options, Program, Sig, emit_module_with};
use common::{ash_cli_bin, haxe_available, render_output, run_with_timeout, static_findex, tests_dir};
use std::process::Command;
use std::time::Duration;

/// The module the emitter makes from every numeric function in `hl`.
fn emit(hl: &std::path::Path, opts: Options) -> (DecodedBytecode, ash_wasm_emit::Emitted) {
    ash_core::native_lib::init_std_library().unwrap();
    let bc = BytecodeDecoder::decode(hl).unwrap();
    let emitted = {
        let module = ash_core::air_pipeline::AshModule::new(&bc);
        let symbols = std::collections::HashMap::new();
        let data = ash_core::wasm_direct::ProgramData::default();
        let prog = BytecodeProgram {
            bc: &bc,
            symbols: &symbols,
            data: &data,
        };
        let mut optimized = Vec::new();
        for f in &bc.functions {
            if let Ok(o) = ash_core::air_pipeline::optimized(&module, f) {
                let fun = bc.types[f.type_.0].fun.as_ref().unwrap();
                let sig = Sig {
                    params: fun
                        .args
                        .iter()
                        .map(|a| prog.ty(air::v2::ir::TypeRef(a.0 as u32)))
                        .collect(),
                    ret: prog.ty(air::v2::ir::TypeRef(fun.ret.0 as u32)),
                };
                optimized.push((f.findex as usize, sig, o));
            }
        }
        let inputs: Vec<Input> = optimized
            .iter()
            .map(|(findex, sig, o)| Input {
                findex: *findex,
                sig: sig.clone(),
                body: &o.ir,
            })
            .collect();
        emit_module_with(&prog, &inputs, opts)
    };
    (bc, emitted)
}

/// Instantiate an emitted module with the host's exact fused multiply-add.
fn instantiate(bytes: &[u8]) -> (wasmtime::Store<()>, wasmtime::Instance) {
    let engine = wasmtime::Engine::default();
    let wasm = wasmtime::Module::new(&engine, bytes).expect("the module validates");
    let mut store = wasmtime::Store::new(&engine, ());
    let mut linker = wasmtime::Linker::new(&engine);
    linker
        .func_wrap("ash", "fma_f64", |a: f64, b: f64, c: f64| a.mul_add(b, c))
        .unwrap();
    linker
        .func_wrap("ash", "fma_f32", |a: f32, b: f32, c: f32| a.mul_add(b, c))
        .unwrap();
    linker.func_wrap("ash", "poll", || {}).unwrap();
    let instance = linker.instantiate(&mut store, &wasm).unwrap();
    (store, instance)
}

fn compile(dir: &std::path::Path) -> std::path::PathBuf {
    let hl = dir.join("numeric.hl");
    if haxe_available() {
        let output = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", "TestWasmNumeric", "-hl"])
            .arg(&hl)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "haxe: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    } else {
        std::fs::copy(tests_dir().join("test_wasm_numeric.hl"), &hl)
            .expect("committed bytecode fixture");
    }
    hl
}

#[test]
fn emitted_wasm_matches_the_interpreter() {
    let dir = tempfile::tempdir().unwrap();
    let hl = compile(dir.path());

    // The reference: what the interpreter prints.
    let mut command = Command::new(ash_cli_bin());
    command.args(["--mode", "interp"]).arg(&hl);
    let run = run_with_timeout(command, Duration::from_secs(60));
    assert!(
        !run.timed_out && run.output.status.success(),
        "{}",
        render_output(&run.output)
    );
    let expected: Vec<String> = String::from_utf8_lossy(&run.output.stdout)
        .lines()
        .filter(|l| l.contains('=') && !l.starts_with('['))
        .map(str::to_owned)
        .collect();

    let opts = Options { polls: true, shadow: true, stub_skipped: false };
    let (bc, emitted) = emit(&hl, opts);

    let (mut store, instance) = instantiate(&emitted.bytes);

    let find = |name: &str| {
        let findex = static_findex(&bc, "TestWasmNumeric", name)
            .unwrap_or_else(|| panic!("no static {name}"));
        assert!(
            emitted.functions.contains(&findex),
            "{name} (f{findex}) was not emitted: {:?}",
            emitted.skipped.iter().find(|(f, _)| *f == findex)
        );
        format!("f{findex}")
    };
    let i1 = |name: &str, store: &mut wasmtime::Store<()>, a: i32| -> i32 {
        instance
            .get_typed_func::<i32, i32>(&mut *store, &find(name))
            .unwrap()
            .call(&mut *store, a)
            .unwrap()
    };
    let i2 = |name: &str, store: &mut wasmtime::Store<()>, a: i32, b: i32| -> i32 {
        instance
            .get_typed_func::<(i32, i32), i32>(&mut *store, &find(name))
            .unwrap()
            .call(&mut *store, (a, b))
            .unwrap()
    };
    let mandel = |store: &mut wasmtime::Store<()>, x: f64, y: f64, n: i32| -> i32 {
        instance
            .get_typed_func::<(f64, f64, i32), i32>(&mut *store, &find("mandel"))
            .unwrap()
            .call(&mut *store, (x, y, n))
            .unwrap()
    };
    let scaled = |store: &mut wasmtime::Store<()>, a: i32, b: f64| -> i32 {
        instance
            .get_typed_func::<(i32, f64), i32>(&mut *store, &find("scaled"))
            .unwrap()
            .call(&mut *store, (a, b))
            .unwrap()
    };

    let mut got: Vec<String> = Vec::new();
    let s = &mut store;
    for n in [0, 1, 2, 10, 20, 25] {
        got.push(format!("fib={}", i1("fib", s, n)));
    }
    for n in [0, 1, 10, 100, 1000] {
        got.push(format!("sumSquares={}", i1("sumSquares", s, n)));
    }
    for n in [1, 6, 27, 97] {
        got.push(format!("collatz={}", i1("collatz", s, n)));
    }
    for (a, b) in [(12, 18), (17, 5), (0, 9), (270, 192)] {
        got.push(format!("gcd={}", i2("gcd", s, a, b)));
    }
    for (x, y, n) in [(0.0, 0.0, 100), (1.0, 1.0, 100), (-0.5, 0.5, 200), (0.3, 0.5, 500)] {
        got.push(format!("mandel={}", mandel(s, x, y, n)));
    }
    for (a, b) in [(5, 9), (1234, 5678), (-7, 3)] {
        got.push(format!("bits={}", i2("bits", s, a, b)));
    }
    for n in [5, -5, 0] {
        got.push(format!("neg={}", i1("neg", s, n)));
    }
    for n in [0, 1, 2, 3, -1] {
        got.push(format!("pick={}", i1("pick", s, n)));
    }
    got.push(format!("scaled={}", scaled(s, 4, 0.5)));
    got.push(format!("scaled={}", scaled(s, -7, 2.25)));
    got.push(format!("quotient={}", i2("quotient", s, 17, 5)));
    got.push(format!("quotient={}", i2("quotient", s, -17, 5)));

    // The 64-bit function is compared separately: its result is not an i32.
    let wide = instance
        .get_typed_func::<(i64, i64), i64>(&mut store, &find("wide"))
        .unwrap()
        .call(&mut store, (123_456_789, 1000))
        .unwrap();
    got.push(format!("wide={wide}"));

    assert_eq!(got, expected, "emitted wasm disagrees with the interpreter");
}

/// Speed of the emitted code against LLVM's, on the same functions. Not part of
/// the normal run: `cargo test --release -p ash --test wasm_emit_numeric -- --ignored --nocapture`
/// prints times to compare with `ash run` of the same program built by LLVM.
#[test]
#[ignore = "a benchmark"]
fn emitted_wasm_speed() {
    let dir = tempfile::tempdir().unwrap();
    let hl = dir.path().join("bench.hl");
    let out = Command::new("haxe")
        .arg("-cp")
        .arg(tests_dir())
        .args(["-main", "TestWasmBench", "-hl"])
        .arg(&hl)
        .output()
        .unwrap();
    assert!(out.status.success(), "{}", String::from_utf8_lossy(&out.stderr));
    for (label, opts) in [
        ("plain", Options::default()),
        ("polls+shadow", Options { polls: true, shadow: true, stub_skipped: false }),
    ] {
        let (bc, emitted) = emit(&hl, opts);
        let (mut store, instance) = instantiate(&emitted.bytes);
        let mut func = |name: &str| {
            let findex = static_findex(&bc, "TestWasmBench", name).expect(name);
            assert!(emitted.functions.contains(&findex), "{name} was not emitted");
            instance
                .get_typed_func::<i32, i32>(&mut store, &format!("f{findex}"))
                .unwrap()
        };
        let (fib, sum, grid) = (func("fib"), func("loopSum"), func("grid"));
        for (name, f, arg) in [("fib", fib, 35), ("loopSum", sum, 200_000_000), ("grid", grid, 400)] {
            let mut ms = Vec::new();
            let mut result = 0;
            for _ in 0..5 {
                let began = std::time::Instant::now();
                result = f.call(&mut store, arg).unwrap();
                ms.push(began.elapsed().as_secs_f64() * 1e3);
            }
            ms.sort_by(f64::total_cmp);
            println!("EMITTED[{label}] {name} {result} median {:.0}ms", ms[2]);
        }
    }
}
