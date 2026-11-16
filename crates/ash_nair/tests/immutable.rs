//! Fields nothing writes once their object is built: what the analysis over the typed program
//! decides, what reaches each lowered function, and what the passes then do with a loop that
//! calls something they cannot see into.

mod common;

use std::collections::HashMap;

use air::v2::ir::{Function, TypeRef};
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

/// Field `field` of class `class`, as the instructions name it.
fn key(module: &Module, class: &str, field: &str) -> (TypeRef, usize) {
    let t = &module.types.table;
    let o = t
        .objs
        .iter()
        .position(|o| o.name == class)
        .unwrap_or_else(|| panic!("{class} has a layout"));
    let ty = t
        .types
        .iter()
        .position(|ty| matches!(ty, HlType::Obj(i) if *i == o))
        .unwrap_or_else(|| panic!("{class} has a type"));
    (TypeRef(ty as u32), t.objs[o].index[field].0 as usize)
}

#[test]
fn a_field_written_only_while_its_object_is_built_is_fixed_everywhere_but_where_it_is_built() {
    let Some(compiler) = common::native_haxe() else {
        eprintln!("skipped: set ASH_HAXE_NATIVE to a haxe with the ash_tast plugin built");
        return;
    };
    let bytes = common::export(&compiler, "tests/immutable", "immutable");
    let tast = Tast::parse(&bytes).expect("a valid TAST file");
    let mut module = Module::new(tast);
    let mut entries = Vec::new();
    for class in ["Main", "Config", "Counter", "Leaky", "Base", "Derived"] {
        let c = module.types.class_id(class).expect("the class");
        let rec = tast.classes()[c as usize];
        for f in rec.fields_first.get()..rec.fields_first.get() + rec.fields_count.get() {
            if tast.fields()[f as usize].func.get() != ash_tast::format::NONE
                && module.types.field_name(f) != "main"
            {
                entries.push(f);
            }
        }
    }
    let done = module.lower_all(&entries).expect("the program lowers");
    let info = NairModule::new(&module, &done);

    let fixed = |function: &str, class: &str, field: &str| {
        info.immutable_fields_of(by_name(&done, function).findex as usize)
            .expect("the function has facts")
            .contains(&key(&module, class, field))
    };

    // code that is not a constructor may rely on what no one writes after construction
    assert!(fixed("Main.run", "Config", "scale"));
    assert!(fixed("Main.run", "Config", "limit"));
    assert!(fixed("Main.run", "Counter", "step"));
    assert!(fixed("Main.make", "Base", "id"));
    assert!(fixed("Main.make", "Derived", "extra"));
    assert!(fixed("Counter.add", "Counter", "step"));
    // `total` is written by a method
    assert!(!fixed("Main.run", "Counter", "total"));
    assert!(!fixed("Counter.add", "Counter", "total"));
    // a constructor that calls a method on `this` lets a callee see the fields before they are set
    assert!(!fixed("Main.make", "Leaky", "b"));
    assert!(!fixed("Main.make", "Leaky", "seen"));

    // a constructor writes the fields of its class and the classes above it, so it may not rely
    // on them; it may rely on the others
    assert!(!fixed("Config.new", "Config", "scale"));
    assert!(!fixed("Config.new", "Config", "limit"));
    assert!(fixed("Config.new", "Counter", "step"));
    assert!(!fixed("Derived.new", "Base", "id"));
    assert!(!fixed("Derived.new", "Derived", "extra"));
    assert!(fixed("Derived.new", "Config", "scale"));
    assert!(!fixed("Base.new", "Base", "id"));
    assert!(fixed("Base.new", "Derived", "extra"));

    // a loop that calls something the passes cannot see into: `limit` is read on every pass, and
    // only the fact lets it leave the loop
    let findex = by_name(&done, "Main.run").findex as usize;
    let loop_loads = |with_the_fact: bool| {
        let mut f: Function = by_name(&done, "Main.run").function.clone().with_findex(findex);
        assert_eq!(common::loads_in_loops(&f), 1, "before the passes");
        f.facts.calls.clear();
        f.facts.methods.clear();
        if with_the_fact {
            f.facts.immutable = info.immutable_fields_of(findex).unwrap().clone();
        }
        PassManager::with_module(OptLevel::O2, &info)
            .with_options(PassOptions {
                verify_each: true,
                ..PassOptions::default()
            })
            .run(&mut f)
            .expect("the passes run");
        verify(&f).expect("it verifies after the passes");
        common::loads_in_loops(&f)
    };
    assert_eq!(loop_loads(false), 1, "an unknown call may write the field");
    assert_eq!(loop_loads(true), 0, "nothing writes it, so the load leaves the loop");

    // and the program still computes what the source means, as lowered and after the passes
    let check = |functions: HashMap<usize, &Function>| {
        let program = Program::new(functions, &module.ints, &module.floats, &module.types.table);
        let call = |name: &str, n: i32| {
            run(&program, by_name(&done, name).findex as usize, &[Val::I(n)])
                .unwrap_or_else(|e| panic!("{name}({n}): {e}"))
        };
        // run: n iterations adding 3, then times the scale of 2
        assert_eq!(call("Main.go", 5), Val::I(30));
        assert_eq!(call("Main.go", 0), Val::I(0));
        // b is 7, seen is 0 (it read b before it was set), id and extra are n and n + 1
        assert_eq!(call("Main.make", 5), Val::I(7 + 5 + 6));
    };
    check(
        done.iter()
            .map(|l| (l.findex as usize, &l.function))
            .collect(),
    );
    let mut optimized: Vec<(usize, Function)> = Vec::new();
    for l in &done {
        let mut f = l.function.clone().with_findex(l.findex as usize);
        optimize(&info, &mut f, OptLevel::O2, false).expect("the passes run");
        verify(&f).unwrap_or_else(|e| panic!("{} after the passes: {e}", l.name));
        optimized.push((l.findex as usize, f));
    }
    check(optimized.iter().map(|(i, f)| (*i, f)).collect());
}
