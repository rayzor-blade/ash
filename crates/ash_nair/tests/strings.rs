//! String constants lowered from the typed program. A literal is a read of a global that holds one
//! shared `String` object, which the module builds when it starts, as genhl does; the same text
//! in two places is one constant. The lowered code is read by the reference interpreter, as
//! lowered and after the AIR passes.

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
fn string_constants_lower_to_a_utf16_buffer_in_a_string_object() {
    let Some(compiler) = common::native_haxe() else {
        eprintln!("skipped: set ASH_HAXE_NATIVE to a haxe with the ash_tast plugin built");
        return;
    };
    let bytes = common::export(&compiler, "tests/strings", "strings");
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
    // each distinct text once, held in a global of its own
    assert_eq!(module.strings.len(), 3, "{:?}", module.strings);
    for text in ["héllo", "ab", "xyz"] {
        assert!(module.strings.iter().any(|s| s == text), "{text}");
    }
    assert_eq!(module.constants.len(), 3);
    let globals_read = |name: &str| -> Vec<usize> {
        by_name(&done, name)
            .function
            .blocks
            .iter()
            .flat_map(|b| b.instrs.iter())
            .filter_map(|i| match i {
                Instr::GetGlobal { global, .. } => Some(*global),
                _ => None,
            })
            .collect()
    };
    // a literal is a read, not an allocation
    for l in &done {
        let allocates = l
            .function
            .blocks
            .iter()
            .flat_map(|b| b.instrs.iter())
            .any(|i| matches!(i, Instr::New { .. } | Instr::String { .. }));
        assert!(!allocates, "{} allocates", l.name);
    }
    // the literal "ab" is one global, wherever it is written
    let in_pick = globals_read("Main.pick");
    assert_eq!(in_pick.len(), 2);
    assert_eq!(globals_read("Main.again"), vec![in_pick[0]]);

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

    if std::env::var("ASH_NAIR_DUMP").is_ok() {
        for l in &done {
            eprintln!("== {} (pool {:?})\n{}", l.name, module.ints, l.function.dump());
        }
    }
    let check = |what: &str, functions: HashMap<usize, &Function>| {
        let program =
            Program::new(functions, &module.ints, &module.floats, &module.types.table)
                .with_strings(&module.strings)
                .with_constants(&module.constants);
        let mut heap = Heap::default();
        heap.init_constants(&program, string_ty, bytes_at, length_at)
            .expect("the constants are built");
        let findex = |name: &str| by_name(&done, name).findex as usize;
        let text = |heap: &mut Heap, name: &str, args: &[Val]| {
            let v = run_in(&program, findex(name), args, heap).expect("it runs");
            heap.read_string(v, bytes_at, length_at)
        };
        assert_eq!(text(&mut heap, "Main.greeting", &[]).as_deref(), Some("héllo"));
        assert_eq!(text(&mut heap, "Main.pick", &[Val::I(0)]).as_deref(), Some("ab"));
        assert_eq!(text(&mut heap, "Main.pick", &[Val::I(1)]).as_deref(), Some("xyz"));
        assert_eq!(text(&mut heap, "Main.again", &[]).as_deref(), Some("ab"));
        // UTF-16 units: five for "héllo"
        assert_eq!(
            run_in(&program, findex("Main.size"), &[Val::I(0)], &mut heap),
            Ok(Val::I(7)),
            "{what}"
        );
        assert_eq!(
            run_in(&program, findex("Main.size"), &[Val::I(1)], &mut heap),
            Ok(Val::I(8)),
            "{what}"
        );
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
            if std::env::var("ASH_NAIR_DUMP").is_ok() && l.name == "Main.size" {
                eprintln!(
                    "== size after {level:?}: base {} pending {:?}\n{}",
                    f.int_pool_base,
                    f.pending_ints,
                    f.dump()
                );
            }
            optimized.push((l.findex as usize, f));
        }
        check(&format!("after {level:?}"), optimized.iter().map(|(i, f)| (*i, f)).collect());
    }
}
