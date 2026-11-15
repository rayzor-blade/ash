//! What a function can know about the calls it makes: a summary of what each callee may write,
//! solved over direct calls and used by the passes that move or reuse loads.

use super::*;
use crate::v2::facts::{CallSummary, Footprint, attach, summarize};

fn summary(classes: &[AliasClass]) -> CallSummary {
    CallSummary {
        writes: Footprint::of(classes),
    }
}

/// The class of the first load in `f`.
fn load_class(f: &Function) -> AliasClass {
    f.blocks
        .iter()
        .flat_map(|b| b.instrs.iter())
        .find_map(read_class)
        .expect("the function loads something")
}

/// The same object type, the next field slot.
fn next_field(c: AliasClass) -> AliasClass {
    match c {
        AliasClass::ObjField { ty, field } => AliasClass::ObjField {
            ty,
            field: field + 1,
        },
        other => panic!("not a field class: {other:?}"),
    }
}

/// `do { x = o.f0; g() } while (i < n)`, where `g` is function 3.
fn loop_with_load_and_call() -> Function {
    let ops = vec![
        Opcode::Label,
        Opcode::Field {
            dst: Reg(2),
            obj: Reg(1),
            field: RefField(0),
        },
        Opcode::Call0 {
            dst: Reg(4),
            fun: RefFun(3),
        },
        Opcode::JSLt {
            a: Reg(3),
            b: Reg(0),
            offset: -4,
        },
        Opcode::Ret { ret: Reg(2) },
    ];
    lower(&ops, &[t(0), t(5), t(0), t(0), t(0)]).unwrap()
}

fn hoisted_with(writes: Option<&[AliasClass]>) -> usize {
    let mut f = loop_with_load_and_call();
    if let Some(w) = writes {
        let w: Vec<AliasClass> = w.to_vec();
        f.facts.calls.insert(3, summary(&w));
    }
    run_pass(&mut f, &LoopInvariantCodeMotion, PassOptions::default()).hoisted
}

#[test]
fn licm_hoists_a_load_across_a_call_that_writes_elsewhere() {
    let class = load_class(&loop_with_load_and_call());
    assert_eq!(hoisted_with(None), 0, "an unsummarized call clobbers all");
    assert_eq!(hoisted_with(Some(&[next_field(class)])), 1);
    assert_eq!(hoisted_with(Some(&[])), 1, "a call that writes nothing");
}

#[test]
fn licm_refuses_a_load_across_a_call_that_may_write_it() {
    let class = load_class(&loop_with_load_and_call());
    assert_eq!(hoisted_with(Some(&[class])), 0);
    assert_eq!(hoisted_with(Some(&[next_field(class), class])), 0);
    assert_eq!(hoisted_with(Some(&[AliasClass::Any])), 0);
    // Reflective writes may reach any field.
    assert_eq!(hoisted_with(Some(&[AliasClass::DynBox])), 0);
}

fn gvn_across_call(writes: Option<&[AliasClass]>) -> usize {
    let ops = vec![
        Opcode::Field {
            dst: Reg(1),
            obj: Reg(0),
            field: RefField(0),
        },
        Opcode::Call0 {
            dst: Reg(2),
            fun: RefFun(3),
        },
        Opcode::Field {
            dst: Reg(3),
            obj: Reg(0),
            field: RefField(0),
        },
        Opcode::Add {
            dst: Reg(4),
            a: Reg(1),
            b: Reg(3),
        },
        Opcode::Ret { ret: Reg(4) },
    ];
    let mut f = lower(&ops, &[t(5), t(0), t(0), t(0), t(0)]).unwrap();
    if let Some(w) = writes {
        f.facts.calls.insert(3, summary(w));
    }
    run_pass(&mut f, &GlobalValueNumbering, PassOptions::default()).eliminated
}

#[test]
fn gvn_reuses_a_load_across_a_call_that_writes_elsewhere() {
    let ops = vec![
        Opcode::Field {
            dst: Reg(1),
            obj: Reg(0),
            field: RefField(0),
        },
        Opcode::Ret { ret: Reg(1) },
    ];
    let class = load_class(&lower(&ops, &[t(5), t(0)]).unwrap());
    assert_eq!(gvn_across_call(None), 0);
    assert_eq!(gvn_across_call(Some(&[next_field(class)])), 1);
    assert_eq!(gvn_across_call(Some(&[class])), 0);
    assert_eq!(gvn_across_call(Some(&[AliasClass::Any])), 0);
}

#[test]
fn a_summary_never_protects_a_cell_load() {
    // r4 is the address of r0 and goes to the callee, which may store through it.
    let ops = vec![
        Opcode::Ref {
            dst: Reg(4),
            src: Reg(0),
        },
        Opcode::Int {
            dst: Reg(0),
            ptr: RefInt(0),
        },
        Opcode::Mov {
            dst: Reg(1),
            src: Reg(0),
        },
        Opcode::Call1 {
            dst: Reg(5),
            fun: RefFun(3),
            arg0: Reg(4),
        },
        Opcode::Mov {
            dst: Reg(2),
            src: Reg(0),
        },
        Opcode::Add {
            dst: Reg(3),
            a: Reg(1),
            b: Reg(2),
        },
        Opcode::Ret { ret: Reg(3) },
    ];
    let mut f = lower(&ops, &[t(0); 6]).unwrap();
    f.facts.calls.insert(3, summary(&[]));
    run_pass(&mut f, &GlobalValueNumbering, PassOptions::default());
    assert_eq!(
        count_instrs(&f, |i| matches!(i, Instr::CellGet { .. })),
        2,
        "{}",
        f.dump()
    );
}

/// A function whose body is `ops`, as function `findex`.
fn body(findex: usize, ops: &[Opcode], tys: &[TypeRef]) -> Function {
    lower(ops, tys).unwrap().with_findex(findex)
}

fn writes_field(field: usize) -> Vec<Opcode> {
    vec![
        Opcode::SetField {
            obj: Reg(0),
            field: RefField(field),
            src: Reg(1),
        },
        Opcode::Ret { ret: Reg(1) },
    ]
}

fn calls(fun: usize) -> Vec<Opcode> {
    vec![
        Opcode::Call0 {
            dst: Reg(1),
            fun: RefFun(fun),
        },
        Opcode::Ret { ret: Reg(1) },
    ]
}

#[test]
fn a_summary_follows_direct_calls() {
    let tys = [t(5), t(0)];
    let leaf = body(2, &writes_field(1), &tys);
    let mid = body(1, &calls(2), &tys);
    let top = body(0, &calls(1), &tys);
    let class = AliasClass::ObjField { ty: t(5), field: 1 };
    let all = summarize([(0, &top), (1, &mid), (2, &leaf)]);
    for findex in [0, 1, 2] {
        assert_eq!(all[&findex], summary(&[class]), "function {findex}");
    }
}

#[test]
fn a_summary_of_a_recursive_function_settles() {
    let tys = [t(5), t(0)];
    let a = body(0, &calls(1), &tys);
    let b = body(1, &calls(0), &tys);
    let all = summarize([(0, &a), (1, &b)]);
    assert!(all[&0].writes.is_none() && all[&1].writes.is_none());

    // One writer anywhere in the cycle reaches every member.
    let mut ops = calls(0);
    ops.insert(0, writes_field(2)[0].clone());
    let w = body(1, &ops, &tys);
    let all = summarize([(0, &a), (1, &w)]);
    let class = AliasClass::ObjField { ty: t(5), field: 2 };
    assert_eq!(all[&0], summary(&[class]));
    assert_eq!(all[&1], summary(&[class]));
}

#[test]
fn a_summary_gives_up_on_what_it_cannot_see() {
    let tys = [t(5), t(0)];
    // A call outside the set, a method call and a closure call may run anything.
    let outside = body(0, &calls(9), &tys);
    assert!(summarize([(0, &outside)])[&0].writes.is_any());

    let method = body(
        0,
        &[
            Opcode::CallMethod {
                dst: Reg(1),
                field: RefField(0),
                args: vec![Reg(0)],
            },
            Opcode::Ret { ret: Reg(1) },
        ],
        &tys,
    );
    assert!(summarize([(0, &method)])[&0].writes.is_any());

    // A summary the function already carries stands in for a body it was not given.
    let mut known = body(0, &calls(9), &tys);
    known.facts.calls.insert(9, summary(&[]));
    assert!(summarize([(0, &known)])[&0].writes.is_none());
}

#[test]
fn a_summary_leaves_out_the_callees_own_cells() {
    // r0's address is taken, so its stores are cell writes in the callee's own frame.
    let ops = vec![
        Opcode::Ref {
            dst: Reg(1),
            src: Reg(0),
        },
        Opcode::Int {
            dst: Reg(0),
            ptr: RefInt(0),
        },
        Opcode::Ret { ret: Reg(0) },
    ];
    let f = body(0, &ops, &[t(0), t(0)]);
    assert!(
        count_instrs(&f, |i| matches!(i, Instr::CellSet { .. })) > 0,
        "{}",
        f.dump()
    );
    assert!(summarize([(0, &f)])[&0].writes.is_none());
}

#[test]
fn a_footprint_that_outgrows_its_bound_means_anything() {
    let mut fp = Footprint::none();
    for field in 0..64 {
        fp.add(AliasClass::ObjField { ty: t(1), field });
    }
    assert!(fp.is_any());
    assert!(fp.may_alias(AliasClass::Global(7)));

    let a = Footprint::of(&[AliasClass::Global(1), AliasClass::Global(2)]);
    let b = Footprint::of(&[AliasClass::Global(2), AliasClass::Global(1)]);
    assert_eq!(a, b, "order is not part of a footprint");
}

#[test]
fn attach_gives_a_function_the_summaries_of_what_it_calls() {
    let tys = [t(5), t(0)];
    let leaf = body(2, &writes_field(1), &tys);
    let mut top = body(0, &calls(2), &tys);
    let all = summarize([(2, &leaf)]);
    assert_eq!(attach(&mut top, &all), 1);
    assert_eq!(top.facts.calls.len(), 1);
    assert_eq!(attach(&mut top, &all), 0, "already there");
}

#[test]
fn inlining_keeps_the_summaries_of_the_calls_it_copies_in() {
    let mut callee = body(7, &calls(9), &[t(0), t(0)]);
    callee
        .facts
        .calls
        .insert(9, summary(&[AliasClass::Global(4)]));
    let info = ModuleTables::new().with_callee(7, CalleeBody::Air(Box::new(callee)));
    let mut f = lower(&calls(7), &[t(0), t(0)]).unwrap().with_findex(3);
    assert!(f.facts.is_empty());
    let stats = run_pass(&mut f, &Inlining::new(&info), PassOptions::default());
    assert_eq!(stats.inlined, 1, "{}", f.dump());
    assert_eq!(f.facts.calls[&9], summary(&[AliasClass::Global(4)]));
}
