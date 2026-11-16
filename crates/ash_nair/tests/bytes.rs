//! The compiler's own byte operations (`$bget`, `$bset`, `$bytes_sizebits`) and `throw`, which the
//! std's array classes are written in. The std's `ArrayBytes_Int.reverse` is lowered from the
//! program and run on an array laid out as the runtime would, and a function of
//! the program's own that throws is run on both sides of its condition.

mod common;

use std::collections::HashMap;

use air::v2::ir::{CastKind, Function, Instr, Terminator};
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
fn byte_operations_and_throw_lower_and_run_the_stds_array_code() {
    let Some(compiler) = common::native_haxe() else {
        eprintln!("skipped: set ASH_HAXE_NATIVE to a haxe with the ash_tast plugin built");
        return;
    };
    let bytes = common::export(&compiler, "tests/bytes", "bytes");
    let tast = Tast::parse(&bytes).expect("a valid TAST file");
    let mut module = Module::new(tast);

    let field_of = |module: &Module, class: &str, name: &str| {
        let c = module.types.class_id(class).unwrap_or_else(|| panic!("{class}"));
        let rec = tast.classes()[c as usize];
        (rec.fields_first.get()..rec.fields_first.get() + rec.fields_count.get())
            .find(|f| module.types.field_name(*f) == name)
            .unwrap_or_else(|| panic!("{class}.{name}"))
    };
    let entries = [
        field_of(&module, "hl.types.ArrayBytes_Int", "reverse"),
        field_of(&module, "Main", "check"),
    ];
    let done = module.lower_all(&entries).expect("the program lowers");
    for l in &done {
        verify(&l.function)
            .unwrap_or_else(|e| panic!("{} does not verify: {e}\n{}", l.name, l.function.dump()));
    }

    // a throw ends its block with the value as a Dynamic
    let check = &by_name(&done, "Main.check").function;
    assert!(check.blocks.iter().any(|b| matches!(b.term, Terminator::Throw { .. })));
    assert!(check.blocks.iter().flat_map(|b| b.instrs.iter()).any(|i| matches!(
        i,
        Instr::Cast {
            kind: CastKind::UnsafeCast,
            ..
        }
    )));

    let array_ty = module
        .types
        .table
        .types
        .iter()
        .position(|t| {
            matches!(t, HlType::Obj(o)
                if module.types.table.objs[*o].name == "hl.types.ArrayBytes_Int")
        })
        .expect("the Int array class has a type");
    let length_at = module.types.slot_in(array_ty, "length").unwrap().index as usize;
    let bytes_at = module.types.slot_in(array_ty, "bytes").unwrap().index as usize;

    let run = |what: &str, functions: HashMap<usize, &Function>| {
        let program = Program::new(functions, &module.ints, &module.floats, &module.types.table)
            .with_strings(&module.strings)
            .with_constants(&module.constants);
        let mut heap = Heap::default();
        let findex = |name: &str| by_name(&done, name).findex as usize;
        let ints = |heap: &mut Heap, xs: &[i32]| {
            let buf = heap.bytes(xs.iter().flat_map(|x| x.to_le_bytes()).collect());
            heap.object(
                &program,
                array_ty,
                &[(length_at, Val::I(xs.len() as i32)), (bytes_at, buf)],
            )
            .unwrap()
        };
        let a = ints(&mut heap, &[1, 2, 3, 4, 5]);
        run_in(&program, findex("hl.types.ArrayBytes_Int.reverse"), &[a], &mut heap)
            .unwrap_or_else(|e| panic!("{what}: reverse: {e}"));
        let buf = heap.field(a, bytes_at).unwrap();
        let read: Vec<i32> = heap
            .memory(buf)
            .unwrap()
            .as_chunks::<4>()
            .0
            .iter()
            .map(|c| i32::from_le_bytes(*c))
            .collect();
        assert_eq!(read, vec![5, 4, 3, 2, 1], "{what}");
        assert_eq!(
            run_in(&program, findex("Main.check"), &[Val::I(3)], &mut heap),
            Ok(Val::I(6)),
            "{what}"
        );
        let thrown = run_in(&program, findex("Main.check"), &[Val::I(-1)], &mut heap);
        assert!(
            thrown.as_ref().is_err_and(|e| e.contains("exception was thrown")),
            "{what}: {thrown:?}"
        );
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
