//! A module answers the same questions about types whichever way it was built: from the typed
//! program by nAIR, or from the file genhl writes for the same Haxe. Each function nAIR lowers is
//! matched by name with genhl's, and the kind of every argument and the return type must agree.
//!
//! Needs the Haxe with the `ash_tast` plugin (`ASH_HAXE_NATIVE`). Skipped without it.

mod common;

use std::collections::HashMap;

use air::v2::ir::Instr;
use air::v2::{CalleeBody, FunSig, ModuleInfo, TypeKind};
use ash_core::air_pipeline::AshModule;
use ash_core::bytecode::BytecodeDecoder;
use ash_nair::info::NairModule;
use ash_nair::lower::Module;
use ash_tast::Tast;
use ash_tast::format::NONE;

type Kinds = (Vec<Option<TypeKind>>, Option<TypeKind>);

/// The distinct values of the float constants a function's body uses, in order.
fn floats(m: &dyn ModuleInfo, findex: usize) -> Vec<u64> {
    let Some(CalleeBody::Air(body)) = m.callee(findex) else {
        return Vec::new();
    };
    let mut out: Vec<u64> = body
        .blocks
        .iter()
        .flat_map(|b| b.instrs.iter())
        .filter_map(|i| match i {
            Instr::Float { idx, .. } => m.float_value(*idx).map(f64::to_bits),
            _ => None,
        })
        .collect();
    out.sort_unstable();
    out.dedup();
    out
}

/// What each argument of a signature wraps, by kind; an argument that wraps nothing gives `None`.
fn wrapped(m: &dyn ModuleInfo, sig: &FunSig) -> Vec<Option<TypeKind>> {
    sig.args
        .iter()
        .map(|t| m.type_param(*t).and_then(|p| m.type_kind(p)))
        .collect()
}

fn kinds(m: &dyn ModuleInfo, sig: &FunSig) -> Kinds {
    (
        sig.args.iter().map(|t| m.type_kind(*t)).collect(),
        m.type_kind(sig.ret),
    )
}

/// genhl names a constructor `__constructor__`, the typed program `new`.
fn genhl_name(name: &str) -> String {
    match name.strip_suffix(".new") {
        Some(class) => format!("{class}.__constructor__"),
        None => name.to_owned(),
    }
}

fn agree(fixture: &str, classes: &[&str], at_least: usize) -> (usize, usize, usize) {
    let Some(compiler) = common::native_haxe() else {
        eprintln!("skipped: set ASH_HAXE_NATIVE to a haxe with the ash_tast plugin built");
        return (0, 0, 0);
    };
    let tag = fixture.replace('/', "_");

    let file = std::env::temp_dir().join(format!("ash_nair_hl_{}_{tag}.hl", std::process::id()));
    std::fs::write(&file, common::genhl_file(&compiler, fixture, &tag)).unwrap();
    // decoding hashes field names through the runtime
    ash_core::native_lib::choose_std_linkage(&file);
    ash_core::native_lib::init_std_library().expect("the runtime starts");
    let bc = BytecodeDecoder::decode(&file).expect("genhl's file decodes");
    let _ = std::fs::remove_file(&file);
    let genhl = AshModule::new(&bc);
    let by_name: HashMap<String, usize> = bc
        .method_names()
        .into_iter()
        .map(|(findex, name)| (name, findex))
        .collect();

    let bytes = common::export(&compiler, fixture, &tag);
    let tast = Tast::parse(&bytes).expect("a valid TAST file");
    let mut module = Module::new(tast);
    let mut entries = Vec::new();
    for class in classes {
        let c = module.types.class_id(class).expect("the class");
        let rec = tast.classes()[c as usize];
        for f in rec.fields_first.get()..rec.fields_first.get() + rec.fields_count.get() {
            if tast.fields()[f as usize].func.get() != NONE && module.types.field_name(f) != "main"
            {
                entries.push(f);
            }
        }
    }
    let done = module.lower_all(&entries).expect("the program lowers");
    let nair = NairModule::new(&module, &done);

    let mut matched = 0;
    let (mut wrapped_seen, mut floats_seen) = (0, 0);
    let mut differ = Vec::new();
    for l in &done {
        let Some(&other) = by_name.get(&genhl_name(&l.name)) else {
            continue;
        };
        let (a, b) = (nair.fun_sig(l.findex as usize), genhl.fun_sig(other));
        let (Some(sa), Some(sb)) = (&a, &b) else {
            differ.push(format!("{}: a signature is missing ({a:?}, {b:?})", l.name));
            continue;
        };
        matched += 1;
        let (ka, kb) = (kinds(&nair, sa), kinds(&genhl, sb));
        if ka != kb {
            differ.push(format!("{}: nAIR {ka:?}, genhl {kb:?}", l.name));
        }
        let (wa, wb) = (wrapped(&nair, sa), wrapped(&genhl, sb));
        if wa != wb {
            differ.push(format!("{}: wrapped types nAIR {wa:?}, genhl {wb:?}", l.name));
        }
        let (fa, fb) = (floats(&nair, l.findex as usize), floats(&genhl, other));
        if fa != fb {
            differ.push(format!("{}: float constants nAIR {fa:?}, genhl {fb:?}", l.name));
        }
        if wa.iter().any(Option::is_some) {
            wrapped_seen += 1;
        }
        if !fa.is_empty() {
            floats_seen += 1;
        }
    }
    assert!(differ.is_empty(), "{fixture}:\n{}", differ.join("\n"));
    assert!(
        matched >= at_least,
        "{fixture}: only {matched} of {} lowered functions matched a genhl function",
        done.len()
    );
    (matched, wrapped_seen, floats_seen)
}

#[test]
fn numeric_functions_have_the_same_kinds_either_way() {
    let (matched, _, floats) = agree("tests/lower", &["Main"], 5);
    assert!(matched == 0 || floats > 0, "no function compared float constants");
}

#[test]
fn methods_and_constructors_have_the_same_kinds_either_way() {
    agree("tests/objects", &["Main", "Acc", "Point"], 5);
}

#[test]
fn object_arguments_and_results_have_the_same_kinds_either_way() {
    let (matched, wrapped, _) = agree("tests/nulls", &["Main", "Node", "Tree"], 5);
    assert!(matched == 0 || wrapped > 0, "no function compared a Null<T> parameter");
}
