//! A whole program: `main` is lowered with everything it reaches, the std's printing and string
//! code included, and run by the reference interpreter, which collects what it prints. Nothing
//! of the program is written for the test but its Haxe source.

mod common;

use std::collections::HashMap;

use air::v2::ir::Function;
use air::v2::passes::OptLevel;
use air::v2::verify::verify;
use ash_nair::eval::{Heap, Program, Val, run_in};
use ash_nair::hl::HlType;
use ash_nair::info::{NairModule, optimize};
use ash_nair::lower::Module;
use ash_tast::Tast;

#[test]
fn a_program_lowers_whole_from_main_and_prints_what_it_should() {
    let Some(compiler) = common::native_haxe() else {
        eprintln!("skipped: set ASH_HAXE_NATIVE to a haxe with the ash_tast plugin built");
        return;
    };
    let bytes = common::export(&compiler, "tests/program", "program");
    let tast = Tast::parse(&bytes).expect("a valid TAST file");
    let mut module = Module::new(tast);
    let class = module.types.class_id("Main").expect("the class");
    let main = module.types.declared_field(class, "main").expect("Main.main");
    let done = module.lower_program(main).expect("the program lowers whole");
    for l in &done {
        verify(&l.function)
            .unwrap_or_else(|e| panic!("{} does not verify: {e}\n{}", l.name, l.function.dump()));
    }
    let entry = done
        .iter()
        .find(|l| l.name == "Main.main")
        .expect("main was lowered")
        .findex as usize;

    let string_ty = module
        .types
        .table
        .types
        .iter()
        .position(|t| {
            matches!(t, HlType::Obj(o) if module.types.table.objs[*o].name == "String")
        })
        .expect("String has a type");
    let bytes_at = module.types.slot_in(string_ty, "bytes").unwrap().index as usize;
    let length_at = module.types.slot_in(string_ty, "length").unwrap().index as usize;

    let expected = "fib 610\ncounter 10\nsum 13 of 4\nsqrt 1.5\n";
    let check = |what: &str, functions: HashMap<usize, &Function>| {
        let program = Program::new(functions, &module.ints, &module.floats, &module.types.table)
            .with_strings(&module.strings)
            .with_constants(&module.constants)
            .with_natives(&module.natives);
        let mut heap = Heap::default();
        heap.init_constants(&program, string_ty, bytes_at, length_at)
            .expect("the constants are built");
        run_in(&program, entry, &[], &mut heap)
            .unwrap_or_else(|e| panic!("{what}: main: {e}"));
        assert_eq!(heap.output, expected, "{what}");
        let _ = Val::Void;
    };
    check(
        "as lowered",
        done.iter()
            .map(|l| (l.findex as usize, &l.function))
            .collect(),
    );

    let info = NairModule::new(&module, &done);
    for level in [OptLevel::O2, OptLevel::O3] {
        let mut optimized: Vec<(usize, Function)> = Vec::new();
        for l in &done {
            let mut f = l.function.clone().with_findex(l.findex as usize);
            optimize(&info, &mut f, level, false).expect("the passes run");
            verify(&f).unwrap_or_else(|e| panic!("{} after {level:?}: {e}", l.name));
            optimized.push((l.findex as usize, f));
        }
        check(
            &format!("after {level:?}"),
            optimized.iter().map(|(i, f)| (*i, f)).collect(),
        );
    }
}
