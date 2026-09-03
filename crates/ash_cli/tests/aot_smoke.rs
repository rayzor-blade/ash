//! Every AOT binary must print exactly what the JIT prints.
//!
//! This is the gate on the emitter. It compiles a corpus with `--emit-exe`,
//! the same one command a user runs, and compares the binary's output against
//! the same program under the JIT, byte for byte. A difference here is a
//! lowering bug: the two tiers ran the same bytecode and disagreed.
//!
//! It replaced a shell script, which could only run where that shell did and
//! was invisible to `cargo test`.
//!
//! `cargo test -p ash --test aot_smoke -- --nocapture` to watch it work.
//! `ASH_SMOKE_PROGRAMS="a.hl b.hl"` swaps the corpus.

use std::path::{Path, PathBuf};
use std::process::Command;

/// The corpus, chosen so each entry once caught something.
const PROGRAMS: &[&str] = &[
    "bench_fib.hl",
    "test_basic.hl",
    "test_stdlib.hl",
    "bench_deltablue.hl",
    // A virtual's `lookup` was baked null while `indexes` was baked, so the
    // lazy-init guard never fired and every hash-keyed field access failed --
    // `Reflect.field` and plain `dyn.name` returning null, `hasField` false,
    // `Std.string` aborting -- while the interpreter and the JIT both ran it
    // correctly. Nothing else in this list touches a virtual.
    "test_feature_typedef_anon.hl",
    "test_safe_cast_virtual.hl",
    "test_gettype_null.hl",
    "test_std_reflect_type.hl",
];

fn repo_root() -> PathBuf {
    // CARGO_MANIFEST_DIR is crates/ash_cli.
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("the crate lives two levels below the root")
        .to_path_buf()
}

fn run(binary: &Path, args: &[&str]) -> (String, bool) {
    let out = Command::new(binary)
        .args(args)
        .output()
        .unwrap_or_else(|e| panic!("running {}: {e}", binary.display()));
    let mut text = String::from_utf8_lossy(&out.stdout).into_owned();
    text.push_str(&String::from_utf8_lossy(&out.stderr));
    (text, out.status.success())
}

/// The interpreter announces its own return value; nothing else does.
fn normalize(text: &str) -> String {
    text.lines()
        .filter(|l| !l.contains("returned: Void"))
        .collect::<Vec<_>>()
        .join("\n")
}

#[test]
fn every_aot_binary_matches_the_jit() {
    let ash = PathBuf::from(env!("CARGO_BIN_EXE_ash"));
    let root = repo_root();
    let tests = root.join("crates/ash/test/tests");
    let scratch = std::env::temp_dir().join("ash-aot-smoke");
    std::fs::create_dir_all(&scratch).expect("scratch directory");

    let programs: Vec<PathBuf> = match std::env::var("ASH_SMOKE_PROGRAMS") {
        Ok(list) => list.split_whitespace().map(PathBuf::from).collect(),
        Err(_) => PROGRAMS.iter().map(|p| tests.join(p)).collect(),
    };

    let mut failures: Vec<String> = Vec::new();
    for program in &programs {
        let name = program
            .file_stem()
            .map(|s| s.to_string_lossy().into_owned())
            .unwrap_or_default();
        if !program.exists() {
            failures.push(format!("{name}: {} does not exist", program.display()));
            continue;
        }
        let binary = scratch.join(&name);
        let (emit, ok) = run(
            &ash,
            &[
                "--emit-exe",
                &binary.to_string_lossy(),
                &program.to_string_lossy(),
            ],
        );
        if !ok {
            failures.push(format!(
                "{name}: build failed\n{}",
                emit.lines().rev().take(4).collect::<Vec<_>>().join("\n")
            ));
            continue;
        }

        let (jit, _) = run(&ash, &["--mode", "jit", &program.to_string_lossy()]);
        let (aot, _) = run(&binary, &[]);
        if normalize(&jit) != normalize(&aot) {
            let jit = normalize(&jit);
            let aot = normalize(&aot);
            let first = jit
                .lines()
                .zip(aot.lines())
                .find(|(a, b)| a != b)
                .map(|(a, b)| format!("  jit: {a}\n  aot: {b}"))
                .unwrap_or_else(|| {
                    format!(
                        "  jit has {} lines, aot {}",
                        jit.lines().count(),
                        aot.lines().count()
                    )
                });
            failures.push(format!("{name}: differs from the JIT\n{first}"));
        } else {
            println!("{name}: ok");
        }
    }

    assert!(
        failures.is_empty(),
        "AOT binaries disagreed with the JIT:\n{}",
        failures.join("\n")
    );
}
