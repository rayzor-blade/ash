//! `null` lowered from the typed program: the constant, and `==` and `!=` against it. Linked
//! lists and trees are built and walked by the lowered code, run by the reference interpreter,
//! and run again after the AIR passes.

mod common;

use std::collections::HashMap;

use air::v2::ir::{CondKind, Function, Instr, Terminator};
use air::v2::passes::OptLevel;
use air::v2::verify::verify;
use ash_nair::eval::{Program, Val, run};
use ash_nair::info::{NairModule, optimize};
use ash_nair::lower::{Lowered, Module};
use ash_tast::Tast;

fn by_name<'l>(done: &'l [Lowered], name: &str) -> &'l Lowered {
    done.iter()
        .find(|l| l.name == name)
        .unwrap_or_else(|| panic!("{name} was lowered"))
}

fn null_tests(f: &Function) -> (usize, usize) {
    let mut null = 0;
    let mut not_null = 0;
    for b in &f.blocks {
        if let Terminator::CondJump { cond, .. } = &b.term {
            match cond {
                CondKind::Null => null += 1,
                CondKind::NotNull => not_null += 1,
                _ => {}
            }
        }
    }
    (null, not_null)
}

#[test]
fn null_constants_and_tests_lower_and_the_lists_and_trees_come_out_right() {
    let Some(compiler) = common::native_haxe() else {
        eprintln!("skipped: set ASH_HAXE_NATIVE to a haxe with the ash_tast plugin built");
        return;
    };
    let bytes = common::export(&compiler, "tests/nulls", "nulls");
    let tast = Tast::parse(&bytes).expect("a valid TAST file");
    let mut module = Module::new(tast);
    let all: Vec<u32> = {
        let tast = module.types.tast();
        let c = module.types.class_id("Main").expect("the class");
        let rec = tast.classes()[c as usize];
        (rec.fields_first.get()..rec.fields_first.get() + rec.fields_count.get())
            .filter(|f| {
                tast.fields()[*f as usize].func.get() != ash_tast::format::NONE
                    && module.types.field_name(*f) != "main"
            })
            .collect()
    };
    let done = module.lower_all(&all).expect("the program lowers");
    for l in &done {
        verify(&l.function)
            .unwrap_or_else(|e| panic!("{} does not verify: {e}\n{}", l.name, l.function.dump()));
    }

    // a loop that runs while a node is not null, and a recursion that stops at null
    assert_eq!(null_tests(&by_name(&done, "Main.sum").function), (0, 1));
    assert_eq!(null_tests(&by_name(&done, "Main.length").function), (1, 0));
    // `t.left == null && t.right == null` tests two values
    assert_eq!(null_tests(&by_name(&done, "Main.leaves").function), (2, 0));
    // `null` as a value: the head of the list, and the children of a leaf
    let nulls = |name: &str| {
        by_name(&done, name)
            .function
            .blocks
            .iter()
            .flat_map(|b| b.instrs.iter())
            .filter(|i| matches!(i, Instr::Null { .. }))
            .count()
    };
    assert!(nulls("Main.build") >= 1);
    assert!(nulls("Main.make") >= 2);

    let check = |functions: HashMap<usize, &Function>| {
        let program = Program::new(functions, &module.ints, &module.floats, &module.types.table);
        let call = |name: &str, n: i32| {
            run(&program, by_name(&done, name).findex as usize, &[Val::I(n)])
                .unwrap_or_else(|e| panic!("{name}({n}): {e}"))
        };
        assert_eq!(call("Main.sumOf", 0), Val::I(0));
        assert_eq!(call("Main.sumOf", 5), Val::I(15));
        assert_eq!(call("Main.lengthOf", 0), Val::I(0));
        assert_eq!(call("Main.lengthOf", 7), Val::I(7));
        assert_eq!(call("Main.nodes", 0), Val::I(1));
        assert_eq!(call("Main.nodes", 3), Val::I(15));
        assert_eq!(call("Main.leafCount", 3), Val::I(8));
    };
    check(
        done.iter()
            .map(|l| (l.findex as usize, &l.function))
            .collect(),
    );

    // and the optimized functions still compute the same
    let info = NairModule::new(&module, &done);
    let mut optimized: Vec<(usize, Function)> = Vec::new();
    for l in &done {
        let mut f = l.function.clone().with_findex(l.findex as usize);
        optimize(&info, &mut f, OptLevel::O2, false).expect("the passes run");
        verify(&f).unwrap_or_else(|e| panic!("{} after the passes: {e}", l.name));
        optimized.push((l.findex as usize, f));
    }
    check(optimized.iter().map(|(i, f)| (*i, f)).collect());
}
