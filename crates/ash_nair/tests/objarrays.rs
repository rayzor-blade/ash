//! Arrays of objects and writes to array elements. The functions of a program that index and
//! build arrays are lowered, and the std's `ArrayObj` methods with them, then run on arrays laid
//! out as the runtime would. A write past the length calls the std's `__expand`, which is lowered
//! with them.

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
fn object_arrays_are_read_written_and_built() {
    let Some(compiler) = common::native_haxe() else {
        eprintln!("skipped: set ASH_HAXE_NATIVE to a haxe with the ash_tast plugin built");
        return;
    };
    let bytes = common::export(&compiler, "tests/objarrays", "objarrays");
    let tast = Tast::parse(&bytes).expect("a valid TAST file");
    let mut module = Module::new(tast);

    let wanted = [
        ("Main", "total"),
        ("Main", "at"),
        ("Main", "size"),
        ("Main", "put"),
        ("Main", "bump"),
        ("Main", "make"),
        ("Main", "empty"),
        ("Main", "ints"),
        ("hl.types.ArrayObj", "pop"),
        ("hl.types.ArrayObj", "reverse"),
    ];
    let entries: Vec<u32> = wanted
        .iter()
        .map(|(class, name)| {
            let c = module
                .types
                .class_id(class)
                .unwrap_or_else(|| panic!("{class}"));
            module
                .types
                .declared_field(c, name)
                .unwrap_or_else(|| panic!("{class}.{name}"))
        })
        .collect();
    // the program of those, with what they call: the constructor, `push`, and `__expand`
    let done = module.lower_all(&entries).expect("the program lowers");
    for l in &done {
        verify(&l.function)
            .unwrap_or_else(|e| panic!("{} does not verify: {e}\n{}", l.name, l.function.dump()));
    }

    // a write may grow the array first, so its body calls `__expand`
    let put = &by_name(&done, "Main.put").function;
    let calls = put
        .blocks
        .iter()
        .flat_map(|b| b.instrs.iter())
        .filter(|i| matches!(i, Instr::Call { .. }))
        .count();
    assert_eq!(calls, 1, "{}", put.dump());
    // an empty array is a new object and its constructor
    let empty = &by_name(&done, "Main.empty").function;
    let kinds: Vec<_> = empty
        .blocks
        .iter()
        .flat_map(|b| b.instrs.iter())
        .filter(|i| matches!(i, Instr::New { .. } | Instr::Call { .. }))
        .collect();
    assert_eq!(kinds.len(), 2, "{}", empty.dump());

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
    let (string_bytes_at, string_length_at) = (
        module.types.slot_in(string_ty, "bytes").unwrap().index as usize,
        module.types.slot_in(string_ty, "length").unwrap().index as usize,
    );
    let (body_ty, refs_ty, floats_ty) = (
        find_type("Body"),
        find_type("hl.types.ArrayObj"),
        find_type("hl.types.ArrayBytes_Float"),
    );
    let at = |ty: usize, name: &str| module.types.slot_in(ty, name).unwrap().index as usize;
    let (x_at, length_at, array_at, bytes_at) = (
        at(body_ty, "x"),
        at(refs_ty, "length"),
        at(refs_ty, "array"),
        at(floats_ty, "bytes"),
    );
    let floats_length_at = at(floats_ty, "length");

    let run = |what: &str, functions: HashMap<usize, &Function>| {
        let program = Program::new(functions, &module.ints, &module.floats, &module.types.table)
            .with_strings(&module.strings)
            .with_constants(&module.constants)
            .with_natives(&module.natives);
        let mut heap = Heap::default();
        heap.init_constants(&program, string_ty, string_bytes_at, string_length_at)
            .expect("the constants are built");
        let findex = |name: &str| by_name(&done, name).findex as usize;
        let body = |heap: &mut Heap, x: f64| {
            heap.object(&program, body_ty, &[(x_at, Val::F(x))]).unwrap()
        };
        let refs = |heap: &mut Heap, items: Vec<Val>, length: i32| {
            let held = heap.array(items);
            heap.object(
                &program,
                refs_ty,
                &[(length_at, Val::I(length)), (array_at, held)],
            )
            .unwrap()
        };

        let (b0, b1, b2) = (body(&mut heap, 1.5), body(&mut heap, 2.5), body(&mut heap, 4.0));
        let a = refs(&mut heap, vec![b0, b1, b2], 3);
        assert_eq!(
            run_in(&program, findex("Main.total"), &[a], &mut heap),
            Ok(Val::F(8.0)),
            "{what}: total"
        );
        assert_eq!(
            run_in(&program, findex("Main.size"), &[a], &mut heap),
            Ok(Val::I(3)),
            "{what}: size"
        );
        // inside the length the element is read; past it, null
        assert_eq!(
            run_in(&program, findex("Main.at"), &[a, Val::I(1)], &mut heap),
            Ok(b1),
            "{what}: at(1)"
        );
        assert_eq!(
            run_in(&program, findex("Main.at"), &[a, Val::I(7)], &mut heap),
            Ok(Val::Void),
            "{what}: at(7)"
        );
        assert_eq!(
            run_in(&program, findex("Main.at"), &[a, Val::I(-1)], &mut heap),
            Ok(Val::Void),
            "{what}: at(-1)"
        );

        // a write inside the length replaces the element
        let b3 = body(&mut heap, 10.0);
        run_in(&program, findex("Main.put"), &[a, Val::I(1), b3], &mut heap)
            .unwrap_or_else(|e| panic!("{what}: put: {e}"));
        let held = heap.field(a, array_at).unwrap();
        assert_eq!(heap.array_items(held).unwrap(), &[b0, b3, b2], "{what}: put");
        assert_eq!(
            run_in(&program, findex("Main.total"), &[a], &mut heap),
            Ok(Val::F(15.5)),
            "{what}: total after put"
        );

        // pop returns the last element and clears its slot; reverse swaps the ends
        let popped = run_in(&program, findex("hl.types.ArrayObj.pop"), &[a], &mut heap)
            .unwrap_or_else(|e| panic!("{what}: pop: {e}"));
        assert_eq!(popped, b2, "{what}: pop");
        assert_eq!(heap.field(a, length_at), Some(Val::I(2)), "{what}: length after pop");
        assert_eq!(heap.array_items(held).unwrap(), &[b0, b3, Val::Void], "{what}: pop slot");
        run_in(&program, findex("hl.types.ArrayObj.reverse"), &[a], &mut heap)
            .unwrap_or_else(|e| panic!("{what}: reverse: {e}"));
        assert_eq!(heap.array_items(held).unwrap(), &[b3, b0, Val::Void], "{what}: reverse");

        // push into spare capacity writes at the old length
        let b4 = body(&mut heap, 7.0);
        assert_eq!(
            run_in(&program, findex("hl.types.ArrayObj.push"), &[a, b4], &mut heap),
            Ok(Val::I(3)),
            "{what}: push"
        );
        assert_eq!(heap.array_items(held).unwrap(), &[b3, b0, b4], "{what}: push slot");

        // a write past the length grows the array: the elements stay, the gap is null
        let grown_from = refs(&mut heap, vec![b0, b3, b4], 3);
        run_in(&program, findex("Main.put"), &[grown_from, Val::I(5), b1], &mut heap)
            .unwrap_or_else(|e| panic!("{what}: put past the end: {e}"));
        assert_eq!(heap.field(grown_from, length_at), Some(Val::I(6)), "{what}: grown length");
        let grown = heap.field(grown_from, array_at).unwrap();
        let items = heap.array_items(grown).unwrap();
        assert!(items.len() >= 6, "{what}: grown capacity {}", items.len());
        assert_eq!(
            &items[..6],
            &[b0, b3, b4, Val::Void, Val::Void, b1],
            "{what}: grown elements"
        );
        // and a negative index is thrown
        let refused = run_in(&program, findex("Main.put"), &[a, Val::I(-1), b1], &mut heap);
        assert!(
            refused.as_ref().is_err_and(|e| e.contains("exception was thrown")),
            "{what}: {refused:?}"
        );

        // `new Array`, then pushes that grow it from nothing
        let made = run_in(&program, findex("Main.make"), &[Val::I(3)], &mut heap)
            .unwrap_or_else(|e| panic!("{what}: make: {e}"));
        assert_eq!(heap.field(made, length_at), Some(Val::I(3)), "{what}: made length");
        assert_eq!(
            run_in(&program, findex("Main.total"), &[made], &mut heap),
            Ok(Val::F(3.0)),
            "{what}: total of made"
        );

        // an empty array is a constructed object with a native array of no elements
        let fresh = run_in(&program, findex("Main.empty"), &[], &mut heap)
            .unwrap_or_else(|e| panic!("{what}: empty: {e}"));
        assert_eq!(heap.field(fresh, length_at), Some(Val::I(0)), "{what}: empty length");
        let held = heap.field(fresh, array_at).unwrap();
        assert_eq!(heap.array_items(held).unwrap(), &[], "{what}: empty elements");

        // compound assignment reads the element, adds, and writes it back
        let buf = heap.bytes(
            [1.0f64, 2.0, 3.0]
                .iter()
                .flat_map(|x| x.to_le_bytes())
                .collect(),
        );
        let floats = heap
            .object(
                &program,
                floats_ty,
                &[(floats_length_at, Val::I(3)), (bytes_at, buf)],
            )
            .unwrap();
        run_in(&program, findex("Main.bump"), &[floats, Val::I(1)], &mut heap)
            .unwrap_or_else(|e| panic!("{what}: bump: {e}"));
        let read: Vec<f64> = heap
            .memory(buf)
            .unwrap()
            .as_chunks::<8>()
            .0
            .iter()
            .map(|c| f64::from_le_bytes(*c))
            .collect();
        assert_eq!(read, vec![1.0, 3.5, 3.0], "{what}: bump");
    };
    run(
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
        run(
            &format!("after {level:?}"),
            optimized.iter().map(|(i, f)| (*i, f)).collect(),
        );
    }
}
