//! `+` on strings with a number on one side. A number is written to a string by the runtime's
//! `itos` and `ftos` into a buffer whose length comes back through a reference to a register, and
//! the two strings are joined by the std's `String.__add__`. The lowered code is run by the
//! reference interpreter, as lowered and after the AIR passes.

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
fn strings_and_numbers_are_joined_by_the_stds_string_code() {
    let Some(compiler) = common::native_haxe() else {
        eprintln!("skipped: set ASH_HAXE_NATIVE to a haxe with the ash_tast plugin built");
        return;
    };
    let bytes = common::export(&compiler, "tests/concat", "concat");
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

    // the number is written into a buffer through the address of a register: one reference, and
    // the length is read back from that register
    let label = &by_name(&done, "Main.label").function;
    let instrs: Vec<_> = label.blocks.iter().flat_map(|b| b.instrs.iter()).collect();
    assert_eq!(
        instrs
            .iter()
            .filter(|i| matches!(i, Instr::CellRef { .. }))
            .count(),
        1,
        "{}",
        label.dump()
    );
    assert_eq!(
        instrs
            .iter()
            .filter(|i| matches!(i, Instr::CellGet { .. }))
            .count(),
        1
    );
    // itos, String.__alloc__ and String.__add__
    assert_eq!(
        instrs
            .iter()
            .filter(|i| matches!(i, Instr::Call { .. }))
            .count(),
        3
    );
    assert!(module.natives.iter().any(|n| n.name == "itos"));
    assert!(module.natives.iter().any(|n| n.name == "ftos"));

    let string_ty = module
        .types
        .table
        .types
        .iter()
        .position(|t| {
            matches!(t, HlType::Obj(o) if module.types.table.objs[*o].name == "String")
        })
        .expect("String has a type");
    let bytes_at = module.types.slot_in(string_ty, "bytes").expect("bytes").index as usize;
    let length_at = module.types.slot_in(string_ty, "length").expect("length").index as usize;

    let check = |what: &str, functions: HashMap<usize, &Function>| {
        let program = Program::new(functions, &module.ints, &module.floats, &module.types.table)
            .with_strings(&module.strings)
            .with_constants(&module.constants)
            .with_natives(&module.natives);
        let mut heap = Heap::default();
        heap.init_constants(&program, string_ty, bytes_at, length_at)
            .expect("the constants are built");
        let findex = |name: &str| by_name(&done, name).findex as usize;
        let string = |heap: &mut Heap, text: &str| {
            let units: Vec<u16> = text.encode_utf16().collect();
            let buf = heap.bytes(
                units
                    .iter()
                    .chain(std::iter::once(&0))
                    .flat_map(|u| u.to_le_bytes())
                    .collect(),
            );
            heap.object(
                &program,
                string_ty,
                &[(bytes_at, buf), (length_at, Val::I(units.len() as i32))],
            )
            .unwrap()
        };
        let text = |heap: &mut Heap, name: &str, args: &[Val]| {
            let v = run_in(&program, findex(name), args, heap)
                .unwrap_or_else(|e| panic!("{what}: {name}{args:?}: {e}"));
            heap.read_string(v, bytes_at, length_at)
        };
        for (n, expect) in [(0, "n=0"), (42, "n=42"), (-7, "n=-7"), (i32::MAX, "n=2147483647")] {
            assert_eq!(text(&mut heap, "Main.label", &[Val::I(n)]).as_deref(), Some(expect));
        }
        for (x, expect) in [(2.5, "r=2.5"), (3.0, "r=3"), (-0.25, "r=-0.25")] {
            assert_eq!(text(&mut heap, "Main.ratio", &[Val::F(x)]).as_deref(), Some(expect));
        }
        let (a, b) = (string(&mut heap, "hé"), string(&mut heap, "llo"));
        assert_eq!(text(&mut heap, "Main.both", &[a, b]).as_deref(), Some("héllo"), "{what}");
        // a null on either side is the text "null", as the std's __add__ has it
        assert_eq!(
            text(&mut heap, "Main.both", &[Val::Void, b]).as_deref(),
            Some("nullllo"),
            "{what}"
        );
        // `+=` with a number, then with a literal
        assert_eq!(text(&mut heap, "Main.join", &[Val::I(12)]).as_deref(), Some("12!"), "{what}");
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
