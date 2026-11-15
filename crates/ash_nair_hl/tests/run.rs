//! What nAIR lowers runs on the real engines and gives the answer genhl's file gives.
//!
//! Needs the Haxe with the `ash_tast` plugin (`ASH_HAXE_NATIVE`) and an `ash` binary
//! (`ASH_BIN`, or `target/release/ash`). Skipped without them.

mod common;

use std::path::Path;
use std::process::Command;
use std::time::Instant;

use ash_nair::lower::Module;
use ash_tast::Tast;
use ash_tast::format::{NONE, field_flag};

/// The exit code of running `file` on `mode`, and how long it took.
fn run(ash: &Path, mode: &str, file: &Path) -> (i32, f64) {
    let start = Instant::now();
    let status = Command::new(ash)
        .args(["--mode", mode])
        .arg(file)
        .status()
        .expect("run ash");
    (status.code().unwrap_or(-1), start.elapsed().as_secs_f64())
}

fn nair_file(tast_bytes: &[u8], entry: &str) -> Vec<u8> {
    let tast = Tast::parse(tast_bytes).expect("a valid TAST file");
    let (class, method) = entry.rsplit_once('.').unwrap();
    let mut module = Module::new(tast);
    let c = module.types.class_id(class).expect("the class");
    let rec = tast.classes()[c as usize];
    let field = (rec.fields_first.get()..rec.fields_first.get() + rec.fields_count.get())
        .find(|f| {
            let fr = &tast.fields()[*f as usize];
            tast.opt_string(fr.name) == Some(method)
                && fr.flags.get() & field_flag::STATIC != 0
                && fr.func.get() != NONE
        })
        .expect("the entry function");
    let lowered = module.lower_program(field).expect("the program lowers");
    let entry = lowered.iter().find(|l| l.name == entry).unwrap().findex;
    let bc = ash_nair_hl::assemble(&module, &lowered, entry).expect("the module assembles");
    ash_nair_hl::encode(&bc).expect("the module encodes")
}

#[test]
fn fib_runs_on_the_tiers_and_agrees_with_genhl() {
    let (Some(compiler), Some(ash)) = (common::native_haxe(), common::ash_binary()) else {
        eprintln!("skipped: set ASH_HAXE_NATIVE and ASH_BIN");
        return;
    };
    let dir = std::env::temp_dir();
    let id = std::process::id();
    let genhl = dir.join(format!("ash_nair_hl_{id}_genhl.hl"));
    let nair = dir.join(format!("ash_nair_hl_{id}_nair.hl"));
    std::fs::write(&genhl, common::genhl_file(&compiler, "tests/run", "g")).unwrap();
    let tast = common::export(&compiler, "tests/run", "n");
    std::fs::write(&nair, nair_file(&tast, "Main.main")).unwrap();

    for mode in ["interp", "jit"] {
        let (expected, t_genhl) = run(&ash, mode, &genhl);
        let (actual, t_nair) = run(&ash, mode, &nair);
        eprintln!(
            "fib(32) on {mode}: genhl {t_genhl:.3} s, nAIR {t_nair:.3} s (exit {expected}, {actual})"
        );
        assert_eq!(expected, 5, "genhl's file returns fib(32) & 255");
        assert_eq!(actual, expected, "nAIR's file on {mode}");
    }
    let _ = std::fs::remove_file(&genhl);
    let _ = std::fs::remove_file(&nair);
}
