use super::*;
use crate::opcodes::*;
use crate::v2::module::CalleeBody;

const INT: TypeRef = TypeRef(0);
const TREE: TypeRef = TypeRef(1);
const VOID: TypeRef = TypeRef(2);
const CTOR: usize = 9;
const BUILD: usize = 40;
const FOLD: usize = 57;

struct Body {
    ops: Vec<Opcode>,
    regs: Vec<TypeRef>,
    args: Vec<TypeRef>,
    ret: TypeRef,
}

struct Module {
    bodies: BTreeMap<usize, Body>,
    policy: AllocationElision,
    stable: bool,
    layout: Vec<TypeRef>,
}

impl ModuleInfo for Module {
    fn int_value(&self, i: usize) -> Option<i32> {
        [0, 2, 1].get(i).copied()
    }
    fn int_pool_len(&self) -> usize {
        3
    }
    fn callee_ops(&self, i: usize) -> Option<usize> {
        Some(self.bodies.get(&i)?.ops.len())
    }
    fn callee(&self, i: usize) -> Option<CalleeBody> {
        let b = self.bodies.get(&i)?;
        Some(CalleeBody::Bytecode {
            ops: b.ops.clone(),
            reg_types: b.regs.clone(),
        })
    }
}

impl TreeModule for Module {
    fn signature(&self, i: usize) -> Option<(Vec<TypeRef>, TypeRef)> {
        let b = self.bodies.get(&i)?;
        Some((b.args.clone(), b.ret))
    }
    fn is_i32(&self, ty: TypeRef) -> bool {
        ty == INT
    }
    fn is_void(&self, ty: TypeRef) -> bool {
        ty == VOID
    }
    fn plain_fields(&self, ty: TypeRef) -> Option<Vec<TypeRef>> {
        (ty == TREE).then(|| self.layout.clone())
    }
    fn allocation_elision(&self, _: TypeRef) -> AllocationElision {
        self.policy
    }
    fn stable_code_and_layout(&self) -> bool {
        self.stable
    }
}

fn fixture() -> Module {
    use Opcode::*;
    Module {
        policy: AllocationElision::Permitted,
        stable: true,
        layout: vec![TREE, TREE, INT],
        bodies: BTreeMap::from([
            (
                CTOR,
                Body {
                    args: vec![TREE, TREE, TREE, INT],
                    ret: VOID,
                    regs: vec![TREE, TREE, TREE, INT, VOID],
                    ops: vec![
                        SetThis {
                            field: RefField(0),
                            src: Reg(1),
                        },
                        SetThis {
                            field: RefField(1),
                            src: Reg(2),
                        },
                        SetThis {
                            field: RefField(2),
                            src: Reg(3),
                        },
                        Ret { ret: Reg(4) },
                    ],
                },
            ),
            (
                BUILD,
                Body {
                    args: vec![INT, INT],
                    ret: TREE,
                    regs: vec![INT, INT, VOID, INT, INT, TREE, TREE, INT, TREE],
                    ops: vec![
                        Int {
                            dst: Reg(4),
                            ptr: RefInt(0),
                        },
                        JSGte {
                            a: Reg(4),
                            b: Reg(1),
                            offset: 15,
                        },
                        New { dst: Reg(5) },
                        Int {
                            dst: Reg(3),
                            ptr: RefInt(1),
                        },
                        Mul {
                            dst: Reg(3),
                            a: Reg(3),
                            b: Reg(0),
                        },
                        Int {
                            dst: Reg(4),
                            ptr: RefInt(2),
                        },
                        Sub {
                            dst: Reg(3),
                            a: Reg(3),
                            b: Reg(4),
                        },
                        Int {
                            dst: Reg(7),
                            ptr: RefInt(2),
                        },
                        Sub {
                            dst: Reg(4),
                            a: Reg(1),
                            b: Reg(7),
                        },
                        Call2 {
                            dst: Reg(6),
                            fun: RefFun(BUILD),
                            arg0: Reg(3),
                            arg1: Reg(4),
                        },
                        Int {
                            dst: Reg(3),
                            ptr: RefInt(1),
                        },
                        Mul {
                            dst: Reg(3),
                            a: Reg(3),
                            b: Reg(0),
                        },
                        Int {
                            dst: Reg(7),
                            ptr: RefInt(2),
                        },
                        Sub {
                            dst: Reg(4),
                            a: Reg(1),
                            b: Reg(7),
                        },
                        Call2 {
                            dst: Reg(8),
                            fun: RefFun(BUILD),
                            arg0: Reg(3),
                            arg1: Reg(4),
                        },
                        Call4 {
                            dst: Reg(2),
                            fun: RefFun(CTOR),
                            arg0: Reg(5),
                            arg1: Reg(6),
                            arg2: Reg(8),
                            arg3: Reg(0),
                        },
                        Ret { ret: Reg(5) },
                        New { dst: Reg(5) },
                        Null { dst: Reg(6) },
                        Null { dst: Reg(8) },
                        Call4 {
                            dst: Reg(2),
                            fun: RefFun(CTOR),
                            arg0: Reg(5),
                            arg1: Reg(6),
                            arg2: Reg(8),
                            arg3: Reg(0),
                        },
                        Ret { ret: Reg(5) },
                    ],
                },
            ),
            (
                FOLD,
                Body {
                    args: vec![TREE],
                    ret: INT,
                    regs: vec![TREE, TREE, INT, INT],
                    ops: vec![
                        GetThis {
                            dst: Reg(1),
                            field: RefField(0),
                        },
                        JNotNull {
                            reg: Reg(1),
                            offset: 2,
                        },
                        GetThis {
                            dst: Reg(2),
                            field: RefField(2),
                        },
                        Ret { ret: Reg(2) },
                        GetThis {
                            dst: Reg(2),
                            field: RefField(2),
                        },
                        GetThis {
                            dst: Reg(1),
                            field: RefField(0),
                        },
                        NullCheck { reg: Reg(1) },
                        Call1 {
                            dst: Reg(3),
                            fun: RefFun(FOLD),
                            arg0: Reg(1),
                        },
                        Add {
                            dst: Reg(2),
                            a: Reg(2),
                            b: Reg(3),
                        },
                        GetThis {
                            dst: Reg(1),
                            field: RefField(1),
                        },
                        NullCheck { reg: Reg(1) },
                        Call1 {
                            dst: Reg(3),
                            fun: RefFun(FOLD),
                            arg0: Reg(1),
                        },
                        Sub {
                            dst: Reg(2),
                            a: Reg(2),
                            b: Reg(3),
                        },
                        Ret { ret: Reg(2) },
                    ],
                },
            ),
        ]),
    }
}

fn caller(m: &Module, extra: Vec<Opcode>) -> Function {
    use Opcode::*;
    let mut ops = vec![
        Int {
            dst: Reg(0),
            ptr: RefInt(0),
        },
        Int {
            dst: Reg(1),
            ptr: RefInt(1),
        },
        Call2 {
            dst: Reg(2),
            fun: RefFun(BUILD),
            arg0: Reg(0),
            arg1: Reg(1),
        },
        NullCheck { reg: Reg(2) },
        Call1 {
            dst: Reg(3),
            fun: RefFun(FOLD),
            arg0: Reg(2),
        },
    ];
    ops.extend(extra);
    ops.push(Ret { ret: Reg(3) });
    super::super::lower::lower_with(&ops, &[INT, INT, TREE, INT, TREE], m).unwrap()
}

fn refuse(m: &Module, needle: &str) {
    let error = analyze_pair(m, BUILD, FOLD, Limits::default()).unwrap_err();
    assert!(error.contains(needle), "expected {needle:?}: {error}");
}

#[test]
fn proves_both_shapes_without_changing_ir() {
    let m = fixture();
    let f = caller(&m, vec![]);
    let before = f.dump();
    let sites = survey(&m, &f, Limits::default()).sites;
    assert_eq!(sites.len(), 1);
    assert!(sites[0].blockers.is_empty(), "{sites:?}");
    assert_eq!(
        sites[0].shape,
        Some(Shape {
            tree_type: TREE,
            depth_arg: 1,
            child_fields: vec![0, 1],
            scalar_fields: vec![2],
        })
    );
    assert_eq!(before, f.dump());
    // The summary does not depend on the particular integer fold formula.
    let mut other = fixture();
    other.bodies.get_mut(&FOLD).unwrap().ops[12] = Opcode::Xor {
        dst: Reg(2),
        a: Reg(2),
        b: Reg(3),
    };
    assert!(analyze_pair(&other, BUILD, FOLD, Limits::default()).is_ok());
}

#[test]
fn allocation_and_reload_contracts_are_separate_blockers() {
    let mut m = fixture();
    m.policy = AllocationElision::Unknown;
    m.stable = false;
    let sites = survey(&m, &caller(&m, vec![]), Limits::default()).sites;
    assert!(sites[0].shape.is_some());
    assert_eq!(sites[0].blockers.len(), 2);
    m.policy = AllocationElision::Observable;
    assert!(
        survey(&m, &caller(&m, vec![]), Limits::default()).sites[0].blockers[0]
            .contains("observable")
    );
}

#[test]
fn refuses_sharing_cycles_and_uninitialized_fields() {
    let mut m = fixture();
    if let Opcode::Call4 { arg2, .. } = &mut m.bodies.get_mut(&BUILD).unwrap().ops[15] {
        *arg2 = Reg(6);
    }
    refuse(&m, "shared");
    let mut m = fixture();
    m.bodies.get_mut(&CTOR).unwrap().ops[0] = Opcode::SetThis {
        field: RefField(0),
        src: Reg(0),
    };
    refuse(&m, "alias, cycle");
    let mut m = fixture();
    m.bodies.get_mut(&CTOR).unwrap().ops[1] = Opcode::Nop;
    refuse(&m, "completely initialized");
}

#[test]
fn refuses_missing_duplicate_and_null_child_visits() {
    let mut m = fixture();
    m.bodies.get_mut(&FOLD).unwrap().ops[11] = Opcode::Int {
        dst: Reg(3),
        ptr: RefInt(0),
    };
    refuse(&m, "every produced child");
    let mut m = fixture();
    m.bodies.get_mut(&FOLD).unwrap().ops[9] = Opcode::GetThis {
        dst: Reg(1),
        field: RefField(0),
    };
    refuse(&m, "more than once");
    let mut m = fixture();
    m.bodies.get_mut(&FOLD).unwrap().ops[1] = Opcode::Nop;
    m.bodies.get_mut(&FOLD).unwrap().ops[2] = Opcode::NullCheck { reg: Reg(1) };
    refuse(&m, "null check can throw");
}

#[test]
fn refuses_effects_mutation_dynamic_calls_and_throwing_arithmetic() {
    for replacement in [
        Opcode::SetThis {
            field: RefField(2),
            src: Reg(2),
        },
        Opcode::SDiv {
            dst: Reg(2),
            a: Reg(2),
            b: Reg(3),
        },
        Opcode::GetGlobal {
            dst: Reg(2),
            global: RefGlobal(0),
        },
        Opcode::CallMethod {
            dst: Reg(3),
            field: RefField(0),
            args: vec![Reg(0)],
        },
        Opcode::GetType {
            dst: Reg(2),
            src: Reg(0),
        },
    ] {
        let mut m = fixture();
        m.bodies.get_mut(&FOLD).unwrap().ops[8] = replacement;
        refuse(&m, "unsupported operation");
    }
    let mut m = fixture();
    m.bodies.get_mut(&CTOR).unwrap().ops[2] = Opcode::SetGlobal {
        global: RefGlobal(0),
        src: Reg(3),
    };
    refuse(&m, "unsupported operation");
}

#[test]
fn refuses_unproved_termination_and_mutual_recursion() {
    let mut m = fixture();
    m.bodies.get_mut(&BUILD).unwrap().ops[7] = Opcode::Int {
        dst: Reg(7),
        ptr: RefInt(1),
    };
    refuse(&m, "does not decrease");
    let mut m = fixture();
    m.bodies.get_mut(&BUILD).unwrap().ops[1] = Opcode::JSGte {
        a: Reg(1),
        b: Reg(4),
        offset: 15,
    };
    refuse(&m, "positive depth guard");
    let mut m = fixture();
    let body = m.bodies.get(&BUILD).unwrap();
    m.bodies.insert(
        41,
        Body {
            ops: body.ops.clone(),
            regs: body.regs.clone(),
            args: body.args.clone(),
            ret: TREE,
        },
    );
    if let Opcode::Call2 { fun, .. } = &mut m.bodies.get_mut(&BUILD).unwrap().ops[9] {
        *fun = RefFun(41);
    }
    refuse(&m, "mutual recursion");
}

#[test]
fn audits_receiver_uses_after_the_consumer() {
    let m = fixture();
    let f = caller(
        &m,
        vec![Opcode::SetGlobal {
            global: RefGlobal(0),
            src: Reg(2),
        }],
    );
    let sites = survey(&m, &f, Limits::default()).sites;
    assert!(sites[0].shape.is_none());
    assert!(sites[0].blockers[0].contains("escapes"));
}

#[test]
fn delayed_consumption_is_refused() {
    let m = fixture();
    let mut f = caller(&m, vec![]);
    let unused = f.new_value(VOID, 99);
    let block = f
        .blocks
        .iter_mut()
        .find(|b| {
            b.instrs
                .iter()
                .any(|i| matches!(i, Instr::NullCheck { .. }))
        })
        .unwrap();
    let pos = block
        .instrs
        .iter()
        .position(|i| matches!(i, Instr::NullCheck { .. }))
        .unwrap();
    block.instrs.insert(
        pos,
        Instr::Call {
            dst: unused,
            fun: 999,
            args: vec![],
        },
    );
    assert!(survey(&m, &f, Limits::default()).sites[0].blockers[0].contains("intervening effect"));
}

#[test]
fn bounded_analysis_declines_instead_of_approximating() {
    let m = fixture();
    let limited = survey(
        &m,
        &caller(&m, vec![]),
        Limits {
            sites: 0,
            ..Limits::default()
        },
    );
    assert!(limited.truncated);
    assert!(limited.sites.is_empty());
    assert!(
        analyze_pair(
            &m,
            BUILD,
            FOLD,
            Limits {
                steps: 1,
                ..Limits::default()
            }
        )
        .unwrap_err()
        .contains("budget")
    );
    assert!(
        analyze_pair(
            &m,
            BUILD,
            FOLD,
            Limits {
                body_ops: 1,
                ..Limits::default()
            }
        )
        .unwrap_err()
        .contains("budget")
    );
}

#[test]
fn field_positions_are_discovered_from_the_module() {
    let mut m = fixture();
    m.layout = vec![INT, TREE, TREE];
    for index in [CTOR, FOLD] {
        for op in &mut m.bodies.get_mut(&index).unwrap().ops {
            match op {
                Opcode::SetThis { field, .. } | Opcode::GetThis { field, .. } => {
                    field.0 = (field.0 + 1) % 3;
                }
                _ => {}
            }
        }
    }
    let shape = analyze_pair(&m, BUILD, FOLD, Limits::default()).unwrap();
    assert_eq!(shape.child_fields, [1, 2]);
    assert_eq!(shape.scalar_fields, [0]);
}

#[test]
fn unused_loop_phi_cycles_do_not_escape_but_observed_ones_do() {
    let m = fixture();
    let mut f = caller(&m, vec![]);
    let root = f.blocks[1]
        .instrs
        .iter()
        .find_map(|op| match op {
            Instr::Call {
                dst, fun: BUILD, ..
            } => Some(*dst),
            _ => None,
        })
        .unwrap();
    let old_return = f.blocks[1].term.clone();
    let phi = f.new_value(TREE, 4);
    f.blocks[1].term = Terminator::Jump { target: BlockId(2) };
    f.blocks.push(Block {
        phis: vec![Phi {
            dst: phi,
            incoming: vec![(BlockId(1), root), (BlockId(2), phi)],
        }],
        instrs: vec![],
        handler: None,
        term: Terminator::CondJump {
            cond: CondKind::Null,
            a: root,
            b: None,
            if_true: BlockId(2),
            if_false: BlockId(3),
        },
    });
    // Use an independent integer condition, so only the phi carries the root.
    let scalar = f.blocks[1]
        .instrs
        .iter()
        .find_map(|op| match op {
            Instr::Int { dst, .. } => Some(*dst),
            _ => None,
        })
        .unwrap();
    f.blocks[2].term = Terminator::CondJump {
        cond: CondKind::Eq,
        a: scalar,
        b: Some(scalar),
        if_true: BlockId(2),
        if_false: BlockId(3),
    };
    f.blocks.push(Block {
        phis: vec![],
        instrs: vec![],
        term: old_return,
        handler: None,
    });
    crate::v2::verify::verify(&f).unwrap();
    assert!(survey(&m, &f, Limits::default()).sites[0].shape.is_some());
    f.blocks[3].instrs.push(Instr::SetGlobal {
        global: 0,
        src: phi,
    });
    crate::v2::verify::verify(&f).unwrap();
    assert!(survey(&m, &f, Limits::default()).sites[0].shape.is_none());
}

#[test]
fn copies_are_transparent_only_until_the_unique_consumer() {
    let m = fixture();
    let mut f = caller(&m, vec![]);
    let alias = f.new_value(TREE, 4);
    let pos = f.blocks[1]
        .instrs
        .iter()
        .position(|op| matches!(op, Instr::NullCheck { .. }))
        .unwrap();
    let Instr::NullCheck { value: root } = f.blocks[1].instrs[pos] else {
        unreachable!()
    };
    f.blocks[1].instrs.insert(
        pos,
        Instr::Copy {
            dst: alias,
            src: root,
        },
    );
    for op in &mut f.blocks[1].instrs[pos + 1..] {
        op.map_uses(&mut |v| if v == root { alias } else { v });
    }
    crate::v2::verify::verify(&f).unwrap();
    assert!(survey(&m, &f, Limits::default()).sites[0].shape.is_some());
    f.blocks[1].instrs.push(Instr::SetGlobal {
        global: 0,
        src: alias,
    });
    assert!(survey(&m, &f, Limits::default()).sites[0].blockers[0].contains("escapes"));
}

#[test]
fn unsigned_or_wrapping_depth_guards_are_not_a_termination_proof() {
    let mut m = fixture();
    m.bodies.get_mut(&BUILD).unwrap().ops[1] = Opcode::JUGte {
        a: Reg(4),
        b: Reg(1),
        offset: 15,
    };
    refuse(&m, "positive depth guard");
    // A negative i32 must take the leaf, never unsigned recursion through
    // wraparound. The supported signed proof covers both extreme inputs.
    for d in [i32::MIN, -1, 0, 1, i32::MAX] {
        let positive = d > 0;
        assert_eq!(
            branch(
                CondKind::SGte,
                Value::Int(Integer::Constant(0)),
                Some(Value::Int(Integer::Param(1))),
                1,
                positive
            )
            .unwrap(),
            0 >= d
        );
        if positive {
            assert!(d.checked_sub(1).unwrap() < d);
        }
    }
}

#[test]
fn constructor_loops_and_exceptions_are_not_summarized_as_initialization() {
    let mut m = fixture();
    m.bodies.get_mut(&CTOR).unwrap().ops = vec![Opcode::Label, Opcode::JAlways { offset: -2 }];
    refuse(&m, "CFG loop");
    let mut m = fixture();
    m.bodies.get_mut(&CTOR).unwrap().ops = vec![Opcode::Throw { exc: Reg(0) }];
    refuse(&m, "exception");
    let mut m = fixture();
    let ctor = m.bodies.get_mut(&CTOR).unwrap();
    ctor.regs.push(TREE);
    ctor.ops = vec![
        Opcode::Trap {
            exc: Reg(5),
            offset: 5,
        },
        Opcode::SetThis {
            field: RefField(0),
            src: Reg(1),
        },
        Opcode::SetThis {
            field: RefField(1),
            src: Reg(2),
        },
        Opcode::SetThis {
            field: RefField(2),
            src: Reg(3),
        },
        Opcode::EndTrap { exc: Reg(5) },
        Opcode::Ret { ret: Reg(4) },
        Opcode::Ret { ret: Reg(4) },
    ];
    refuse(&m, "exception regions");
}

#[test]
fn fusion_builds_scalar_recursion_and_replaces_only_proven_sites() {
    let m = fixture();
    let helper = build_helper(&m, BUILD, FOLD, 100, Limits::default()).unwrap();
    super::super::verify::verify(&helper).unwrap();
    assert_eq!(helper.findex, Some(100));
    assert!(helper.reg_types.iter().all(|t| *t == INT));
    let calls: Vec<_> = helper
        .blocks
        .iter()
        .flat_map(|b| &b.instrs)
        .filter_map(|i| match i {
            Instr::Call { fun, .. } => Some(*fun),
            _ => None,
        })
        .collect();
    assert_eq!(calls, vec![100, 100]);
    assert!(
        !helper
            .blocks
            .iter()
            .flat_map(|b| &b.instrs)
            .any(|i| matches!(
                i,
                Instr::New { .. } | Instr::FieldGet { .. } | Instr::FieldSet { .. }
            ))
    );
    let raw = caller(&m, vec![]);
    let before = raw.dump();
    let (fused, n) = rewrite_caller(
        &m,
        &raw,
        &BTreeMap::from([((BUILD, FOLD), 100)]),
        Limits::default(),
    )
    .unwrap();
    assert_eq!(n, 1);
    assert_eq!(before, raw.dump());
    let calls: Vec<_> = fused
        .blocks
        .iter()
        .flat_map(|b| &b.instrs)
        .filter_map(|i| match i {
            Instr::Call { fun, .. } => Some(*fun),
            _ => None,
        })
        .collect();
    assert_eq!(calls, vec![100]);
}

#[test]
fn fusion_rechecks_policy_escape_and_budget() {
    for policy in [AllocationElision::Unknown, AllocationElision::Observable] {
        let mut m = fixture();
        m.policy = policy;
        assert!(build_helper(&m, BUILD, FOLD, 100, Limits::default()).is_err());
        assert_eq!(
            rewrite_caller(
                &m,
                &caller(&m, vec![]),
                &BTreeMap::from([((BUILD, FOLD), 100)]),
                Limits::default()
            )
            .unwrap()
            .1,
            0
        );
    }
    let mut m = fixture();
    m.stable = false;
    assert!(build_helper(&m, BUILD, FOLD, 100, Limits::default()).is_err());
    let m = fixture();
    let raw = caller(&m, vec![Opcode::NullCheck { reg: Reg(2) }]);
    assert_eq!(
        rewrite_caller(
            &m,
            &raw,
            &BTreeMap::from([((BUILD, FOLD), 100)]),
            Limits::default()
        )
        .unwrap()
        .1,
        0
    );
    assert!(
        build_helper(
            &m,
            BUILD,
            FOLD,
            100,
            Limits {
                steps: 1,
                ..Limits::default()
            }
        )
        .is_err()
    );
}
