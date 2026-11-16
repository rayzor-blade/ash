//! A whole program, with objects, arrays, strings and printing, goes from the typed program to a
//! `.hl` file and runs on each of the real engines; what it prints is what genhl's file prints.
//!
//! Needs the Haxe with the `ash_tast` plugin (`ASH_HAXE_NATIVE`) and an `ash` binary
//! (`ASH_BIN`, or `target/release/ash`). Skipped without them.

mod common;

use std::path::Path;
use std::process::Command;

use ash_nair::lower::Module;
use ash_tast::Tast;

/// What `file` prints on `mode`, and its exit code.
fn run(ash: &Path, mode: &str, file: &Path) -> (String, i32) {
    let (stdout, _, code) = run_with(ash, mode, &[], file);
    (stdout, code)
}

/// What `file` prints to standard output and to standard error, and its exit code, with `flags`.
fn run_with(ash: &Path, mode: &str, flags: &[&str], file: &Path) -> (String, String, i32) {
    let out = Command::new(ash)
        .args(["--mode", mode])
        .args(flags)
        .arg(file)
        .output()
        .expect("run ash");
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.code().unwrap_or(-1),
    )
}

/// The `.hl` file of the program in `tast_bytes`, which starts in the function that sets the
/// classes up and then calls `Main.main`.
fn nair_file(tast_bytes: &[u8]) -> Vec<u8> {
    let tast = Tast::parse(tast_bytes).expect("a valid TAST file");
    let mut module = Module::new(tast);
    let class = module.types.class_id("Main").expect("the class");
    let main = module.types.declared_field(class, "main").expect("Main.main");
    let classes = module.program_classes();
    let init = module
        .lower_init(&classes, Some(main))
        .expect("the init function lowers");
    let mut lowered = module.lower_all(&[main]).expect("the program lowers");
    let entry = init.findex;
    lowered.push(init);
    // the method tables name methods the program never calls: they are stubs in the file
    let stubs = module
        .stubs_for_layouts(&lowered)
        .expect("the method tables are stubbed");
    lowered.extend(stubs);
    let bc = ash_nair_hl::assemble(&module, &lowered, entry).expect("the module assembles");
    ash_nair_hl::encode(&bc).expect("the module encodes")
}

#[test]
fn a_whole_program_runs_on_the_engines_and_prints_what_genhls_file_prints() {
    let (Some(compiler), Some(ash)) = (common::native_haxe(), common::ash_binary()) else {
        eprintln!("skipped: set ASH_HAXE_NATIVE and ASH_BIN");
        return;
    };
    let dir = std::env::temp_dir();
    let id = std::process::id();
    let genhl = dir.join(format!("ash_nair_hl_{id}_program_genhl.hl"));
    let nair = dir.join(format!("ash_nair_hl_{id}_program_nair.hl"));
    std::fs::write(&genhl, common::genhl_file(&compiler, "tests/program", "pg")).unwrap();
    let tast = common::export(&compiler, "tests/program", "pn");
    std::fs::write(&nair, nair_file(&tast)).unwrap();

    for mode in ["interp", "jit", "hybrid"] {
        let (expected, code) = run(&ash, mode, &genhl);
        assert_eq!(
            expected, "fib 610\ncounter 10\nsum 13 of 4\nsqrt 1.5\n",
            "genhl's file on {mode} (exit {code})"
        );
        let (actual, nair_code) = run(&ash, mode, &nair);
        assert_eq!(actual, expected, "nAIR's file on {mode} (exit {nair_code})");
        assert_eq!(nair_code, code, "nAIR's exit code on {mode}");
    }
    let _ = std::fs::remove_file(&genhl);
    let _ = std::fs::remove_file(&nair);
}

/// The helper line of tree fusion's log: `ops=` and `node_allocations=`, and how many sites.
fn fusion(stderr: &str) -> Option<(String, String, String)> {
    let helper = stderr.lines().find(|l| l.contains("helper="))?;
    let field = |name: &str| {
        helper
            .split_whitespace()
            .find_map(|w| w.strip_prefix(name))
            .map(str::to_owned)
    };
    let sites = stderr
        .lines()
        .find_map(|l| l.split_whitespace().find_map(|w| w.strip_prefix("transformed_sites=")))?;
    Some((field("ops=")?, field("node_allocations=")?, sites.to_owned()))
}

#[test]
fn a_tree_built_and_checked_at_once_is_fused_in_nairs_file_as_in_genhls() {
    let (Some(compiler), Some(ash)) = (common::native_haxe(), common::ash_binary()) else {
        eprintln!("skipped: set ASH_HAXE_NATIVE and ASH_BIN");
        return;
    };
    let dir = std::env::temp_dir();
    let id = std::process::id();
    let genhl = dir.join(format!("ash_nair_hl_{id}_trees_genhl.hl"));
    let nair = dir.join(format!("ash_nair_hl_{id}_trees_nair.hl"));
    std::fs::write(&genhl, common::genhl_file(&compiler, "tests/trees", "tg")).unwrap();
    let tast = common::export(&compiler, "tests/trees", "tn");
    std::fs::write(&nair, nair_file(&tast)).unwrap();

    let (expected, genhl_log, code) = run_with(&ash, "jit", &["--tree-fusion-log"], &genhl);
    let (actual, nair_log, nair_code) = run_with(&ash, "jit", &["--tree-fusion-log"], &nair);
    assert_eq!(actual, expected, "what the files print");
    assert_eq!(nair_code, code);
    // the same helper is built, which allocates no nodes, at the same sites
    let theirs = fusion(&genhl_log).unwrap_or_else(|| panic!("genhl's file is fused:\n{genhl_log}"));
    let ours = fusion(&nair_log).unwrap_or_else(|| panic!("nAIR's file is fused:\n{nair_log}"));
    assert_eq!(ours, theirs, "nAIR's file:\n{nair_log}\ngenhl's file:\n{genhl_log}");
    assert_eq!(ours.1, "0", "the helper allocates no tree nodes");
    let _ = std::fs::remove_file(&genhl);
    let _ = std::fs::remove_file(&nair);
}

#[test]
fn dynamic_arrays_and_trace_print_on_the_engines_what_genhls_file_prints() {
    let (Some(compiler), Some(ash)) = (common::native_haxe(), common::ash_binary()) else {
        eprintln!("skipped: set ASH_HAXE_NATIVE and ASH_BIN");
        return;
    };
    let dir = std::env::temp_dir();
    let id = std::process::id();
    let genhl = dir.join(format!("ash_nair_hl_{id}_dyn_genhl.hl"));
    let nair = dir.join(format!("ash_nair_hl_{id}_dyn_nair.hl"));
    std::fs::write(&genhl, common::genhl_file(&compiler, "tests/dyn", "dg")).unwrap();
    let tast = common::export(&compiler, "tests/dyn", "dn");
    std::fs::write(&nair, nair_file(&tast)).unwrap();

    for mode in ["interp", "jit", "hybrid"] {
        let (expected, code) = run(&ash, mode, &genhl);
        assert!(
            expected.starts_with("dyn 2 5 7\n") && expected.contains(": traced 5\n"),
            "genhl's file on {mode} (exit {code}): {expected:?}"
        );
        let (actual, nair_code) = run(&ash, mode, &nair);
        assert_eq!(actual, expected, "nAIR's file on {mode} (exit {nair_code})");
        assert_eq!(nair_code, code, "nAIR's exit code on {mode}");
    }
    let _ = std::fs::remove_file(&genhl);
    let _ = std::fs::remove_file(&nair);
}
