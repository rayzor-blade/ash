//! `Array<Int>` and `Array<Float>` read by index and measured by `length`, lowered inline over
//! the array class's fields as genhl does. The arrays are built by the test, as the runtime would
//! lay them out, and the lowered code reads them in the reference interpreter, as lowered and
//! after the AIR passes.

mod common;

use std::collections::HashMap;

use air::v2::ir::{Function, Instr};
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
fn array_reads_and_length_lower_inline_and_read_what_the_source_means() {
    let Some(compiler) = common::native_haxe() else {
        eprintln!("skipped: set ASH_HAXE_NATIVE to a haxe with the ash_tast plugin built");
        return;
    };
    let bytes = common::export(&compiler, "tests/arrays", "arrays");
    let tast = Tast::parse(&bytes).expect("a valid TAST file");
    let mut module = Module::new(tast);
    let class = module.types.class_id("Main").expect("the class");
    let rec = tast.classes()[class as usize];
    let mut entries = Vec::new();
    for f in rec.fields_first.get()..rec.fields_first.get() + rec.fields_count.get() {
        if tast.fields()[f as usize].func.get() != ash_tast::format::NONE
            && module.types.field_name(f) != "main"
        {
            entries.push(f);
        }
    }
    let done = module.lower_all(&entries).expect("the program lowers");
    for l in &done {
        verify(&l.function)
            .unwrap_or_else(|e| panic!("{} does not verify: {e}\n{}", l.name, l.function.dump()));
    }

    // `length` is named by the class that declares it, as every other field is
    let reads_length = done
        .iter()
        .flat_map(|l| l.function.blocks.iter().flat_map(|b| b.instrs.iter()))
        .filter(|i| matches!(i, Instr::FieldGet { .. }))
        .count();
    assert!(reads_length >= 6, "length and bytes are read, not called");

    // the array classes and where their `length` and `bytes` sit
    let array_class = |name: &str| {
        let o = module
            .types
            .table
            .objs
            .iter()
            .position(|o| o.name == name)
            .unwrap_or_else(|| panic!("{name} has a layout"));
        let ty = module
            .types
            .table
            .types
            .iter()
            .position(|t| matches!(t, HlType::Obj(i) if *i == o))
            .unwrap();
        let length = module.types.slot_in(ty, "length").expect("length").index as usize;
        let buffer = module.types.slot_in(ty, "bytes").expect("bytes").index as usize;
        (ty, length, buffer)
    };
    let ints = array_class("hl.types.ArrayBytes_Int");
    let floats = array_class("hl.types.ArrayBytes_Float");

    let check = |functions: HashMap<usize, &Function>| {
        let program = Program::new(functions, &module.ints, &module.floats, &module.types.table);
        let mut heap = Heap::default();
        let int_array = |heap: &mut Heap, xs: &[i32]| {
            let buf = heap.bytes(xs.iter().flat_map(|x| x.to_le_bytes()).collect());
            heap.object(&program, ints.0, &[(ints.1, Val::I(xs.len() as i32)), (ints.2, buf)])
                .unwrap()
        };
        let float_array = |heap: &mut Heap, xs: &[f64]| {
            let buf = heap.bytes(xs.iter().flat_map(|x| x.to_le_bytes()).collect());
            heap.object(
                &program,
                floats.0,
                &[(floats.1, Val::I(xs.len() as i32)), (floats.2, buf)],
            )
            .unwrap()
        };
        let findex = |name: &str| by_name(&done, name).findex as usize;
        let a = int_array(&mut heap, &[1, 2, 3, 4]);
        let empty = int_array(&mut heap, &[]);
        let sum = |heap: &mut Heap, a: Val| {
            run_in(&program, findex("Main.sum"), &[a], heap).expect("sum runs")
        };
        assert_eq!(sum(&mut heap, a), Val::I(10));
        assert_eq!(sum(&mut heap, empty), Val::I(0));
        let x = float_array(&mut heap, &[1.5, 2.0, 3.0]);
        let y = float_array(&mut heap, &[2.0, 4.0, 0.5]);
        assert_eq!(
            run_in(&program, findex("Main.dot"), &[x, y], &mut heap).expect("dot runs"),
            Val::F(12.5)
        );
        let r = int_array(&mut heap, &[5, 6, 7]);
        let at = |heap: &mut Heap, i: i32| {
            run_in(&program, findex("Main.at"), &[r, Val::I(i)], heap).expect("at runs")
        };
        assert_eq!(at(&mut heap, 1), Val::I(6));
        assert_eq!(at(&mut heap, 2), Val::I(7));
        // past the end, and negative, read the default
        assert_eq!(at(&mut heap, 3), Val::I(0));
        assert_eq!(at(&mut heap, -1), Val::I(0));
        // a null array is an error
        assert!(run_in(&program, findex("Main.sum"), &[Val::Void], &mut heap).is_err());
    };
    check(
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
        check(optimized.iter().map(|(i, f)| (*i, f)).collect());
    }
}
