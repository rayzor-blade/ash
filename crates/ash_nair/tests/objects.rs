//! Objects of non-generic classes lowered from the typed program: `new`, instance variables,
//! `this` and calls of methods no subclass overrides. The lowered code is run by the reference
//! interpreter, and the call summaries are checked on the loop they exist for.

mod common;

use std::collections::HashMap;

use air::v2::analysis::{AliasClass, CfgInfo, LoopForest};
use air::v2::ir::{Function, Instr};
use air::v2::passes::{OptLevel, PassManager, PassOptions};
use air::v2::verify::verify;
use ash_nair::eval::{Program, Val, run};
use ash_nair::hl::HlType;
use ash_nair::info::{NairModule, optimize};
use ash_nair::lower::{Lowered, Module};
use ash_tast::Tast;

fn entries(module: &Module, class: &str) -> Vec<u32> {
    let tast = module.types.tast();
    let c = module.types.class_id(class).expect("the class");
    let rec = tast.classes()[c as usize];
    (rec.fields_first.get()..rec.fields_first.get() + rec.fields_count.get())
        .filter(|f| {
            let field = tast.fields()[*f as usize];
            field.func.get() != ash_tast::format::NONE && module.types.field_name(*f) != "main"
        })
        .collect()
}

fn by_name<'l>(done: &'l [Lowered], name: &str) -> &'l Lowered {
    done.iter()
        .find(|l| l.name == name)
        .unwrap_or_else(|| panic!("{name} was lowered"))
}

/// The loads that sit in a loop of `f`.
fn loads_in_loops(f: &Function) -> usize {
    let cfg = CfgInfo::build(f);
    let forest = LoopForest::analyze(f, &cfg);
    let mut in_loop = std::collections::HashSet::new();
    for l in forest.innermost_first() {
        in_loop.extend(forest.get(l).blocks.iter().copied());
    }
    in_loop
        .iter()
        .flat_map(|b| f.blocks[b.idx()].instrs.iter())
        .filter(|i| matches!(i, Instr::FieldGet { .. }))
        .count()
}

#[test]
fn objects_lower_run_and_let_a_summarized_call_keep_a_load_out_of_the_loop() {
    let Some(compiler) = common::native_haxe() else {
        eprintln!("skipped: set ASH_HAXE_NATIVE to a haxe with the ash_tast plugin built");
        return;
    };
    let bytes = common::export(&compiler, "tests/objects", "objects");
    let tast = Tast::parse(&bytes).expect("a valid TAST file");
    let mut module = Module::new(tast);
    let all = entries(&module, "Main");
    let done = module.lower_all(&all).expect("the program lowers");
    for name in [
        "Acc.new",
        "Acc.bump",
        "Acc.run",
        "Acc.reset",
        "Point.new",
        "Point.dist2",
        "Main.accumulate",
        "Main.twice",
        "Main.norm",
    ] {
        by_name(&done, name);
    }
    for l in &done {
        verify(&l.function)
            .unwrap_or_else(|e| panic!("{} does not verify: {e}\n{}", l.name, l.function.dump()));
    }

    let check = |functions: HashMap<usize, &Function>| {
        let program = Program::new(functions, &module.ints, &module.floats, &module.types.table);
        let call = |name: &str, args: &[Val]| {
            run(&program, by_name(&done, name).findex as usize, args)
                .unwrap_or_else(|e| panic!("{name}{args:?}: {e}"))
        };
        assert_eq!(
            call("Main.accumulate", &[Val::I(10), Val::I(3)]),
            Val::I(135)
        );
        assert_eq!(call("Main.accumulate", &[Val::I(0), Val::I(5)]), Val::I(0));
        assert_eq!(call("Main.twice", &[Val::I(10), Val::I(3)]), Val::I(270));
        assert_eq!(call("Main.norm", &[Val::F(3.0), Val::F(4.0)]), Val::F(25.0));
    };
    check(
        done.iter()
            .map(|l| (l.findex as usize, &l.function))
            .collect(),
    );

    // what `bump` may write is the one field it stores to, named by the class that declares it
    let info = NairModule::new(&module, &done);
    let bump = by_name(&done, "Acc.bump");
    let stored = bump
        .function
        .blocks
        .iter()
        .flat_map(|b| b.instrs.iter())
        .find_map(|i| match i {
            Instr::FieldSet { obj_ty, field, .. } => Some(AliasClass::ObjField {
                ty: *obj_ty,
                field: *field,
            }),
            _ => None,
        })
        .expect("bump stores a field");
    let summary = info
        .summary(bump.findex as usize)
        .expect("bump has a summary");
    assert_eq!(summary.writes.classes(), &[stored]);
    let AliasClass::ObjField { ty, .. } = stored else {
        unreachable!()
    };
    let acc = module
        .types
        .table
        .types
        .iter()
        .position(|t| matches!(t, HlType::Obj(o) if module.types.table.objs[*o].name == "Acc"));
    assert_eq!(Some(ty.0 as usize), acc);

    // the loop in `run` reads `limit` through a call that writes only `total`
    let run_fn = by_name(&done, "Acc.run");
    assert_eq!(loads_in_loops(&run_fn.function), 1, "before the passes");
    let mut with = run_fn.function.clone().with_findex(run_fn.findex as usize);
    optimize(&info, &mut with, OptLevel::O2, false).expect("the passes run");
    verify(&with).expect("it verifies after the passes");
    let mut without = run_fn.function.clone().with_findex(run_fn.findex as usize);
    PassManager::with_module(OptLevel::O2, &info)
        .with_options(PassOptions {
            verify_each: true,
            ..PassOptions::default()
        })
        .run(&mut without)
        .expect("the passes run");
    assert_eq!(
        loads_in_loops(&without),
        1,
        "without the summary the call blocks the load"
    );
    assert_eq!(loads_in_loops(&with), 0, "with it the load leaves the loop");

    // and the optimized functions still compute the same
    let mut optimized: Vec<(usize, Function)> = Vec::new();
    for l in &done {
        let mut f = l.function.clone().with_findex(l.findex as usize);
        optimize(&info, &mut f, OptLevel::O2, false).expect("the passes run");
        verify(&f).unwrap_or_else(|e| panic!("{} after the passes: {e}", l.name));
        optimized.push((l.findex as usize, f));
    }
    check(optimized.iter().map(|(i, f)| (*i, f)).collect());
}
