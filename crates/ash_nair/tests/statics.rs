//! Static variables: read and written as fields of a class's statics object, and set up by the
//! init function in program order, each initializer seeing the ones before it. The lowered code
//! is run by the reference interpreter, as lowered and after the AIR passes.

mod common;

use std::collections::HashMap;

use air::v2::ir::{Function, Instr};
use air::v2::passes::OptLevel;
use air::v2::verify::verify;
use ash_nair::eval::{Heap, Program, Val, run_in};
use ash_nair::info::{NairModule, optimize};
use ash_nair::lower::{Lowered, Module};
use ash_tast::Tast;

fn by_name<'l>(done: &'l [Lowered], name: &str) -> &'l Lowered {
    done.iter()
        .find(|l| l.name == name)
        .unwrap_or_else(|| panic!("{name} was lowered"))
}

#[test]
fn static_variables_are_set_up_in_program_order_and_read_and_written_through_their_class() {
    let Some(compiler) = common::native_haxe() else {
        eprintln!("skipped: set ASH_HAXE_NATIVE to a haxe with the ash_tast plugin built");
        return;
    };
    let bytes = common::export(&compiler, "tests/statics", "statics");
    let tast = Tast::parse(&bytes).expect("a valid TAST file");
    let mut module = Module::new(tast);

    let config = module.types.class_id("Config").expect("Config");
    let main = module.types.class_id("Main").expect("Main");
    let in_order: Vec<u32> = module
        .program_classes()
        .into_iter()
        .filter(|c| *c == config || *c == main)
        .collect();
    assert_eq!(in_order, vec![config, main], "Config is generated first: Main reaches it");

    let mut entries = Vec::new();
    for class in [config, main] {
        let rec = tast.classes()[class as usize];
        for f in rec.fields_first.get()..rec.fields_first.get() + rec.fields_count.get() {
            let field = tast.fields()[f as usize];
            // the methods with bodies, not `main` and not the variables' initializers
            if field.func.get() != ash_tast::format::NONE
                && field.flags.get() & ash_tast::format::field_flag::METHOD != 0
                && module.types.field_name(f) != "main"
            {
                entries.push(f);
            }
        }
    }
    let init = module.lower_init(&in_order, None).expect("the init function lowers");
    let mut done = module.lower_all(&entries).expect("the program lowers");
    done.push(init);
    for l in &done {
        verify(&l.function)
            .unwrap_or_else(|e| panic!("{} does not verify: {e}\n{}", l.name, l.function.dump()));
    }

    // a static variable is a read of its class's global, then a field
    let reads_a_global = |name: &str| {
        by_name(&done, name)
            .function
            .blocks
            .iter()
            .flat_map(|b| b.instrs.iter())
            .any(|i| matches!(i, Instr::GetGlobal { .. }))
    };
    assert!(reads_a_global("Main.work"));
    assert!(reads_a_global("Config.scaled"));
    assert!(reads_a_global("Config.bump"));

    let check = |what: &str, functions: HashMap<usize, &Function>| {
        let program = Program::new(functions, &module.ints, &module.floats, &module.types.table);
        let mut heap = Heap::default();
        let findex = |name: &str| by_name(&done, name).findex as usize;
        // set the program up: each class's statics object, then the initializers in order
        run_in(&program, findex("$init"), &[], &mut heap).unwrap_or_else(|e| panic!("{what}: {e}"));
        let call = |heap: &mut Heap, name: &str, args: &[Val]| {
            run_in(&program, findex(name), args, heap).unwrap_or_else(|e| panic!("{what}: {e}"))
        };
        // total starts at 7; table is base * 3 = 30, so the initializers ran in order; the
        // counter starts at 0 and bump adds one
        assert_eq!(call(&mut heap, "Main.work", &[Val::I(5)]), Val::I(12 + 30 + 1), "{what}");
        assert_eq!(call(&mut heap, "Main.work", &[Val::I(5)]), Val::I(17 + 30 + 2), "{what}");
        assert_eq!(call(&mut heap, "Config.scaled", &[Val::I(4)]), Val::F(10.0), "{what}");
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
