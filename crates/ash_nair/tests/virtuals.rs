//! Method calls resolved from the class hierarchy: one function where no class below the
//! receiver's overrides the method, the method table where one does, and `super` calls.

mod common;

use air::v2::analysis::AliasClass;
use air::v2::ir::{Function, Instr};
use air::v2::passes::{OptLevel, PassManager, PassOptions};
use air::v2::verify::verify;
use ash_nair::eval::{Program, Val, run};
use ash_nair::hl::HlType;
use ash_nair::info::{NairModule, optimize};
use ash_nair::lower::{Lowered, Module};
use ash_tast::Tast;

fn by_name<'l>(done: &'l [Lowered], name: &str) -> &'l Lowered {
    done.iter()
        .find(|l| l.name == name)
        .unwrap_or_else(|| panic!("{name} was lowered"))
}

/// The functions `f` calls directly.
fn direct_calls(f: &Function) -> Vec<usize> {
    f.blocks
        .iter()
        .flat_map(|b| b.instrs.iter())
        .filter_map(|i| match i {
            Instr::Call { fun, .. } => Some(*fun),
            _ => None,
        })
        .collect()
}

/// How many calls of `f` go through a method table.
fn table_calls(f: &Function) -> usize {
    f.blocks
        .iter()
        .flat_map(|b| b.instrs.iter())
        .filter(|i| matches!(i, Instr::CallMethod { .. }))
        .count()
}

#[test]
fn a_method_call_is_one_function_unless_a_class_below_the_receiver_overrides_it() {
    let Some(compiler) = common::native_haxe() else {
        eprintln!("skipped: set ASH_HAXE_NATIVE to a haxe with the ash_tast plugin built");
        return;
    };
    let bytes = common::export(&compiler, "tests/virtuals", "virtuals");
    let tast = Tast::parse(&bytes).expect("a valid TAST file");
    let mut module = Module::new(tast);
    let main = module.types.class_id("Main").expect("Main");
    let rec = tast.classes()[main as usize];
    let entries: Vec<u32> = (rec.fields_first.get()
        ..rec.fields_first.get() + rec.fields_count.get())
        .filter(|f| {
            tast.fields()[*f as usize].func.get() != ash_tast::format::NONE
                && module.types.field_name(*f) != "main"
        })
        .collect();
    let done = module.lower_all(&entries).expect("the program lowers");
    for l in &done {
        verify(&l.function)
            .unwrap_or_else(|e| panic!("{} does not verify: {e}\n{}", l.name, l.function.dump()));
    }
    let f = |name: &str| &by_name(&done, name).function;
    let id = |name: &str| by_name(&done, name).findex as usize;

    // what the program computes
    let program = Program::new(
        done.iter()
            .map(|l| (l.findex as usize, &l.function))
            .collect(),
        &module.ints,
        &module.floats,
        &module.types.table,
    );
    let call = |name: &str, args: &[Val]| {
        run(&program, id(name), args).unwrap_or_else(|e| panic!("{name}{args:?}: {e}"))
    };
    assert_eq!(call("Main.square", &[Val::I(3)]), Val::I(9));
    assert_eq!(call("Main.plain", &[]), Val::I(0));
    // Square 16, Tri 8, Shape 0, Plain 0
    assert_eq!(call("Main.mixed", &[Val::I(4)]), Val::I(24));
    // Shape.twice calls area through the table, and the object is a Square
    assert_eq!(call("Main.twiceSquare", &[Val::I(3)]), Val::I(18));

    // a Square receiver: nothing below it overrides area, so it is Square.area itself
    assert_eq!(
        direct_calls(f("Main.square")),
        [id("Square.new"), id("Square.area")]
    );
    assert_eq!(table_calls(f("Main.square")), 0);
    // Plain overrides nothing, and nothing is below it: the implementation it inherits
    assert_eq!(
        direct_calls(f("Main.plain")),
        [id("Plain.new"), id("Shape.area")]
    );
    assert_eq!(table_calls(f("Main.plain")), 0);
    // a Shape receiver may be any of four classes
    assert_eq!(table_calls(f("Main.areaOf")), 1);
    assert!(direct_calls(f("Main.areaOf")).is_empty());
    // inside twice, `this` may be any Shape
    assert_eq!(table_calls(f("Shape.twice")), 1);
    // twice itself is one function whatever the receiver
    assert_eq!(
        direct_calls(f("Main.twiceSquare")),
        [id("Square.new"), id("Shape.twice")]
    );
    // `super(k)` runs the superclass constructor on the same object
    assert_eq!(direct_calls(f("Square.new")), [id("Shape.new")]);
    assert_eq!(direct_calls(f("Plain.new")), [id("Shape.new")]);
}

#[test]
fn a_method_table_call_writes_what_the_functions_its_slot_may_run_write() {
    let Some(compiler) = common::native_haxe() else {
        eprintln!("skipped: set ASH_HAXE_NATIVE to a haxe with the ash_tast plugin built");
        return;
    };
    let bytes = common::export(&compiler, "tests/virtuals", "virtuals_summaries");
    let tast = Tast::parse(&bytes).expect("a valid TAST file");
    let mut module = Module::new(tast);
    let main = module.types.class_id("Main").expect("Main");
    let rec = tast.classes()[main as usize];
    let entries: Vec<u32> = (rec.fields_first.get()
        ..rec.fields_first.get() + rec.fields_count.get())
        .filter(|f| {
            tast.fields()[*f as usize].func.get() != ash_tast::format::NONE
                && module.types.field_name(*f) != "main"
        })
        .collect();
    let done = module.lower_all(&entries).expect("the program lowers");
    let info = NairModule::new(&module, &done);
    let id = |name: &str| by_name(&done, name).findex as usize;

    // `hits` is declared in Counter, so a write through Loud is a write of Counter's slot
    let counter = module
        .types
        .table
        .types
        .iter()
        .position(|t| matches!(t, HlType::Obj(o) if module.types.table.objs[*o].name == "Counter"))
        .expect("Counter has a type");
    let stored = by_name(&done, "Loud.note")
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
        .expect("Loud.note stores a field");
    assert!(matches!(stored, AliasClass::ObjField { ty, .. } if ty.0 as usize == counter));
    assert_eq!(
        info.summary(id("Loud.note")).unwrap().writes.classes(),
        &[stored]
    );
    assert!(info.summary(id("Quiet.note")).unwrap().writes.is_none());
    assert!(info.summary(id("Counter.note")).unwrap().writes.is_none());
    // `drive` calls the table, so it writes what the three may write together
    assert_eq!(
        info.summary(id("Main.drive")).unwrap().writes.classes(),
        &[stored]
    );

    // the loop reads `limit` through the table call, and only `hits` can change under it
    let drive = by_name(&done, "Main.drive");
    assert_eq!(
        common::loads_in_loops(&drive.function),
        1,
        "before the passes"
    );
    let mut with = drive.function.clone().with_findex(drive.findex as usize);
    optimize(&info, &mut with, OptLevel::O2, false).expect("the passes run");
    verify(&with).expect("it verifies after the passes");
    let mut without = drive.function.clone().with_findex(drive.findex as usize);
    PassManager::with_module(OptLevel::O2, &info)
        .with_options(PassOptions {
            verify_each: true,
            ..PassOptions::default()
        })
        .run(&mut without)
        .expect("the passes run");
    assert_eq!(
        common::loads_in_loops(&without),
        1,
        "without a summary the table call blocks it"
    );
    assert_eq!(
        common::loads_in_loops(&with),
        0,
        "with one the load leaves the loop"
    );

    // and the optimized program still computes the same
    let mut optimized: Vec<(usize, Function)> = Vec::new();
    for l in &done {
        let mut f = l.function.clone().with_findex(l.findex as usize);
        optimize(&info, &mut f, OptLevel::O2, false).expect("the passes run");
        verify(&f).unwrap_or_else(|e| panic!("{} after the passes: {e}", l.name));
        optimized.push((l.findex as usize, f));
    }
    let program = Program::new(
        optimized.iter().map(|(i, f)| (*i, f)).collect(),
        &module.ints,
        &module.floats,
        &module.types.table,
    );
    let call = |name: &str, n: i32| run(&program, id(name), &[Val::I(n)]).expect(name);
    assert_eq!(call("Main.loud", 5), Val::I(10));
    assert_eq!(call("Main.quiet", 5), Val::I(0));
}
