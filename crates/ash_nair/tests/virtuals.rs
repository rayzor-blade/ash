//! Method calls resolved from the class hierarchy: one function where no class below the
//! receiver's overrides the method, the method table where one does, and `super` calls.

mod common;

use air::v2::ir::{Function, Instr};
use air::v2::verify::verify;
use ash_nair::eval::{Program, Val, run};
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
