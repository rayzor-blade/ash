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
    let out = Command::new(ash)
        .args(["--mode", mode])
        .arg(file)
        .output()
        .expect("run ash");
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
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
