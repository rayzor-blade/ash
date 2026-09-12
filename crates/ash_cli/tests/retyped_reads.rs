//! Register shapes HashLink's JIT accepts but Haxe never emits.
//!
//! A DynGet, GetI8 or GetI16 whose destination register is Bool, UI8 or
//! UI16 has to store at that register's width; an Int constant whose
//! destination is an I64 register has to sign-extend. Haxe reads a Dynamic
//! field into a Dynamic register and casts, reads bytes into an Int register
//! and narrows, and builds an I64 constant from an Int one with OToInt, so a
//! compiled program never reaches either shape. These tests make the shapes
//! themselves: each takes a compiled fixture, retypes the read-then-convert
//! pairs so the read lands in the final register directly, writes the result
//! back out as bytecode, and runs it through every engine with the register
//! store audit on. The engines have to agree with the unmodified program, and
//! the audit has to stay silent.

mod common;

use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Duration;

use ash_core::bytecode::{BytecodeDecoder, DecodedBytecode};
use ash_core::hl_bindings::{
    hl_type_kind_HBOOL, hl_type_kind_HI64, hl_type_kind_HUI16, hl_type_kind_HUI8,
};
use ash_core::opcodes::Opcode;
use common::{ash_cli_bin, run_with_timeout, tests_dir};

/// Rewrite every `DynGet`/`GetI8`/`GetI16` followed by a `SafeCast`/`ToInt`
/// into a Bool, UI8 or UI16 register so the read writes that register
/// itself and the conversion becomes a `Nop`. Returns how many pairs changed.
fn retype_narrow_reads(bc: &mut DecodedBytecode) -> usize {
    let mut changed = 0;
    let is_narrow = |kind: u32| {
        kind == hl_type_kind_HBOOL || kind == hl_type_kind_HUI8 || kind == hl_type_kind_HUI16
    };
    for f in &mut bc.functions {
        for i in 0..f.ops.len().saturating_sub(1) {
            let read_dst = match &f.ops[i] {
                Opcode::DynGet { dst, .. } => *dst,
                Opcode::GetI8 { dst, .. } | Opcode::GetI16 { dst, .. } => *dst,
                _ => continue,
            };
            let narrow = match &f.ops[i + 1] {
                Opcode::SafeCast { dst, src } | Opcode::ToInt { dst, src }
                    if *src == read_dst =>
                {
                    *dst
                }
                _ => continue,
            };
            if !is_narrow(bc.types[f.regs[narrow.0 as usize].0].kind) {
                continue;
            }
            // Haxe reuses registers, so the wide one keeps its type and its
            // other uses; the read simply targets the narrow register.
            match &mut f.ops[i] {
                Opcode::DynGet { dst, .. }
                | Opcode::GetI8 { dst, .. }
                | Opcode::GetI16 { dst, .. } => *dst = narrow,
                _ => unreachable!(),
            }
            f.ops[i + 1] = Opcode::Nop;
            changed += 1;
        }
    }
    changed
}

/// Rewrite every `Int` followed by a `ToInt` into an I64 register so the
/// constant lands in the I64 register itself and the conversion becomes a
/// `Nop`. Returns how many pairs changed.
fn retype_int_into_i64(bc: &mut DecodedBytecode) -> usize {
    let mut changed = 0;
    for f in &mut bc.functions {
        for i in 0..f.ops.len().saturating_sub(1) {
            let Opcode::Int { dst: int_dst, .. } = &f.ops[i] else {
                continue;
            };
            let int_dst = *int_dst;
            let Opcode::ToInt { dst: wide, src } = &f.ops[i + 1] else {
                continue;
            };
            let (wide, src) = (*wide, *src);
            if src != int_dst {
                continue;
            }
            if bc.types[f.regs[wide.0 as usize].0].kind != hl_type_kind_HI64 {
                continue;
            }
            if let Opcode::Int { dst, .. } = &mut f.ops[i] {
                *dst = wide;
            }
            f.ops[i + 1] = Opcode::Nop;
            changed += 1;
        }
    }
    changed
}

struct Ran {
    stdout: String,
    stderr: String,
    ok: bool,
}

fn run(binary: &Path, args: &[&str]) -> Ran {
    let mut cmd = Command::new(binary);
    cmd.args(args).env("ASH_CHECK_REG_STORES", "1");
    let r = run_with_timeout(cmd, Duration::from_secs(300));
    assert!(!r.timed_out, "{} {:?} hung", binary.display(), args);
    Ran {
        stdout: String::from_utf8_lossy(&r.output.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&r.output.stderr).into_owned(),
        ok: r.output.status.success(),
    }
}

/// The interpreter announces its own return value; nothing else does.
fn program_lines(s: &str) -> String {
    s.lines()
        .filter(|l| !l.contains("returned: Void"))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Retype `fixture` with `retype`, then run the result through every engine
/// against the unmodified program's interpreter output.
fn check_retyped(fixture: &str, retype: fn(&mut DecodedBytecode) -> usize, min_changed: usize) {
    let ash = ash_cli_bin();
    let scratch = std::env::temp_dir().join("ash-retyped-reads");
    std::fs::create_dir_all(&scratch).expect("scratch directory");

    let source = tests_dir().join(format!("{fixture}.hl"));
    ash_core::native_lib::choose_std_linkage(&source);
    ash_core::native_lib::init_std_library().expect("starting the runtime");
    let mut bc = BytecodeDecoder::decode(&source).expect("decode the fixture");
    let changed = retype(&mut bc);
    assert!(
        changed >= min_changed,
        "{fixture}: expected at least {min_changed} pairs to retype, got {changed}"
    );
    let bytes = ash_core::bytecode_encode::encode(&bc, 5).expect("encode");
    let retyped: PathBuf = scratch.join(format!("{fixture}_retyped.hl"));
    std::fs::write(&retyped, bytes).expect("write the retyped bytecode");
    let retyped = retyped.to_string_lossy().into_owned();

    let reference = run(&ash, &["--mode", "interp", &source.to_string_lossy()]);
    assert!(reference.ok, "the unmodified fixture failed:\n{}", reference.stdout);
    let want = program_lines(&reference.stdout);

    let aot_bin = scratch.join(format!("{fixture}_retyped"));
    let aot_bin = aot_bin.to_string_lossy().into_owned();
    let build = run(&ash, &["--build", &aot_bin, &retyped]);
    assert!(build.ok, "AOT build failed:\n{}", build.stderr);

    // Compiled-only mode installs Cranelift code for every function at its
    // first call, on the calling thread, so it is the arm that runs the
    // Cranelift lowering of these shapes; a hybrid arm on a program this
    // short would exit before any promotion landed. The LLVM lowering is
    // the AOT build.
    let arms: Vec<(&str, Ran)> = vec![
        ("interp", run(&ash, &["--mode", "interp", &retyped])),
        ("jit", run(&ash, &["--mode", "jit", &retyped])),
        ("aot-build", build),
        ("aot", run(Path::new(&aot_bin), &[])),
    ];

    let mut failures = Vec::new();
    for (name, ran) in &arms {
        let audit: Vec<&str> = ran
            .stderr
            .lines()
            .filter(|l| l.contains("[regstore]"))
            .collect();
        if !audit.is_empty() {
            failures.push(format!("{name}: register store audit\n{}", audit.join("\n")));
        }
        if *name == "aot-build" {
            continue;
        }
        if !ran.ok {
            failures.push(format!("{name}: exited non-zero\n{}", ran.stdout));
        }
        let got = program_lines(&ran.stdout);
        if got != want {
            failures.push(format!("{name}: output differs\nwant:\n{want}\ngot:\n{got}"));
        }
    }
    assert!(failures.is_empty(), "{}", failures.join("\n\n"));
}

#[test]
fn narrow_reads_store_at_register_width() {
    check_retyped("test_narrow_dynget", retype_narrow_reads, 5);
}

#[test]
fn int_into_i64_is_signed() {
    check_retyped("test_int_into_i64", retype_int_into_i64, 5);
}
