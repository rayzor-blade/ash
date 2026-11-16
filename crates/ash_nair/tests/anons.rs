//! Object literals. A literal whose fields suit the type the program expects is that structure,
//! its fields set by position; one that does not, such as the position `trace` passes, is a
//! dynamic object whose fields are set by name and which is then converted to the structure.
//! The lowered code is run by the reference interpreter, as lowered and after the AIR passes.

mod common;

use std::collections::HashMap;

use air::v2::ir::{CastKind, Function, Instr};
use air::v2::passes::OptLevel;
use air::v2::verify::verify;
use ash_nair::eval::{Heap, Program, Val, run_in};
use ash_nair::hl::HlType;
use ash_nair::info::{NairModule, optimize};
use ash_nair::lower::{Lowered, Module};
use ash_tast::Tast;

fn by_name<'l>(done: &'l [Lowered], name: &str) -> &'l Lowered {
    done.iter()
        .find(|l| l.name == name)
        .unwrap_or_else(|| panic!("{name} was lowered"))
}

#[test]
fn object_literals_are_structures_or_dynamic_objects_converted_to_one() {
    let Some(compiler) = common::native_haxe() else {
        eprintln!("skipped: set ASH_HAXE_NATIVE to a haxe with the ash_tast plugin built");
        return;
    };
    let bytes = common::export(&compiler, "tests/anons", "anons");
    let tast = Tast::parse(&bytes).expect("a valid TAST file");
    let mut module = Module::new(tast);
    let class = module.types.class_id("Main").expect("the class");
    let field = |module: &Module, name: &str| {
        module
            .types
            .declared_field(class, name)
            .unwrap_or_else(|| panic!("Main.{name}"))
    };
    let entries: Vec<u32> = ["make", "loose", "where", "total", "shift"]
        .iter()
        .map(|n| field(&module, n))
        .collect();
    let mut done = module.lower_all(&entries).expect("the program lowers");
    // trace's callee is the std's logging, which is not what is tested: the call is
    done.push(module.lower_field(field(&module, "shout")).expect("shout lowers"));
    for l in &done {
        verify(&l.function)
            .unwrap_or_else(|e| panic!("{} does not verify: {e}\n{}", l.name, l.function.dump()));
    }

    let instrs = |name: &str| -> Vec<Instr> {
        by_name(&done, name)
            .function
            .blocks
            .iter()
            .flat_map(|b| b.instrs.iter().cloned())
            .collect()
    };
    // a structure is allocated and its fields set by position, and not converted by name
    if std::env::var("ASH_NAIR_DUMP").is_ok() {
        for l in &done {
            eprintln!("== {}\n{}", l.name, l.function.dump());
        }
    }
    let make = instrs("Main.make");
    assert_eq!(make.iter().filter(|i| matches!(i, Instr::New { .. })).count(), 1);
    assert_eq!(make.iter().filter(|i| matches!(i, Instr::FieldSet { .. })).count(), 2);
    assert!(!make.iter().any(|i| matches!(
        i,
        Instr::DynSet { .. }
            | Instr::Cast {
                kind: CastKind::ToVirtual,
                ..
            }
    )));
    // trace's position: one dynamic object, four fields by name, and the conversion
    let shout = instrs("Main.shout");
    assert_eq!(shout.iter().filter(|i| matches!(i, Instr::DynSet { .. })).count(), 4);
    assert_eq!(
        shout
            .iter()
            .filter(|i| matches!(i, Instr::Cast { kind: CastKind::ToVirtual, .. }))
            .count(),
        1
    );

    let find_type = |name: &str| {
        module
            .types
            .table
            .types
            .iter()
            .position(|t| {
                matches!(t, HlType::Obj(o) if module.types.table.objs[*o].name == name)
            })
            .unwrap_or_else(|| panic!("a type {name}"))
    };
    let string_ty = find_type("String");
    let bytes_at = module.types.slot_in(string_ty, "bytes").unwrap().index as usize;
    let length_at = module.types.slot_in(string_ty, "length").unwrap().index as usize;

    let check = |what: &str, functions: HashMap<usize, &Function>| {
        let program = Program::new(functions, &module.ints, &module.floats, &module.types.table)
            .with_strings(&module.strings)
            .with_constants(&module.constants)
            .with_natives(&module.natives);
        let mut heap = Heap::default();
        heap.init_constants(&program, string_ty, bytes_at, length_at)
            .expect("the constants are built");
        let findex = |name: &str| by_name(&done, name).findex as usize;

        // fields are in the order of their names
        let p = run_in(&program, findex("Main.make"), &[Val::I(3), Val::I(4)], &mut heap)
            .unwrap_or_else(|e| panic!("{what}: make: {e}"));
        assert_eq!(heap.field(p, 0), Some(Val::I(3)), "{what}: x");
        assert_eq!(heap.field(p, 1), Some(Val::I(4)), "{what}: y");
        // a field of a structure is read and written through its place in the structure
        assert_eq!(
            run_in(&program, findex("Main.total"), &[p], &mut heap),
            Ok(Val::I(7)),
            "{what}: total"
        );
        run_in(&program, findex("Main.shift"), &[p, Val::I(10)], &mut heap)
            .unwrap_or_else(|e| panic!("{what}: shift: {e}"));
        assert_eq!(heap.field(p, 0), Some(Val::I(13)), "{what}: shifted x");
        assert_eq!(
            run_in(&program, findex("Main.total"), &[p], &mut heap),
            Ok(Val::I(17)),
            "{what}: total after shift"
        );
        let d = run_in(&program, findex("Main.loose"), &[Val::I(5)], &mut heap)
            .unwrap_or_else(|e| panic!("{what}: loose: {e}"));
        assert_eq!(heap.field(d, 0), Some(Val::I(5)), "{what}: loose x");
        assert_eq!(heap.field(d, 1), Some(Val::I(5)), "{what}: loose y");

        // the five fields of haxe.PosInfos: className, customParams, fileName, lineNumber,
        // methodName
        let w = run_in(&program, findex("Main.where"), &[], &mut heap)
            .unwrap_or_else(|e| panic!("{what}: where: {e}"));
        let text = |heap: &Heap, at: usize| {
            heap.field(w, at)
                .and_then(|v| heap.read_string(v, bytes_at, length_at))
        };
        assert_eq!(text(&heap, 0).as_deref(), Some("C"), "{what}: className");
        assert_eq!(heap.field(w, 1), Some(Val::Void), "{what}: customParams");
        assert_eq!(text(&heap, 2).as_deref(), Some("a.hx"), "{what}: fileName");
        assert_eq!(heap.field(w, 3), Some(Val::I(7)), "{what}: lineNumber");
        assert_eq!(text(&heap, 4).as_deref(), Some("m"), "{what}: methodName");
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
