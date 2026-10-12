use super::*;
use crate::bytecode::BytecodeDecoder;
use crate::opcodes::RefInt;

fn fixture() -> DecodedBytecode {
    let mut f = HLFunction::with_body(
        vec![
            Opcode::Int {
                dst: Reg(1),
                ptr: RefInt(0),
            },
            Opcode::JSLte {
                a: Reg(0),
                b: Reg(1),
                offset: 5,
            },
            Opcode::Int {
                dst: Reg(1),
                ptr: RefInt(1),
            },
            Opcode::Sub {
                dst: Reg(2),
                a: Reg(0),
                b: Reg(1),
            },
            Opcode::Call1 {
                dst: Reg(3),
                fun: RefFun(0),
                arg0: Reg(2),
            },
            Opcode::Add {
                dst: Reg(3),
                a: Reg(3),
                b: Reg(0),
            },
            Opcode::Ret { ret: Reg(3) },
            Opcode::Int {
                dst: Reg(3),
                ptr: RefInt(2),
            },
            Opcode::Ret { ret: Reg(3) },
        ],
        [0, 12].repeat(9),
    );
    f.type_ = TypeRef(1);
    f.regs = vec![TypeRef(0); 4];
    DecodedBytecode {
        ints: vec![0, 1, 3],
        types: vec![
            HLType {
                kind: hl_type_kind_HI32,
                ..Default::default()
            },
            HLType {
                kind: hl_type_kind_HFUN,
                fun: Some(HLTypeFun {
                    args: vec![TypeRef(0)],
                    ret: TypeRef(0),
                    ..Default::default()
                }),
                ..Default::default()
            },
        ],
        functions: vec![f],
        debug_files: vec!["generated-test.hx".into()],
        has_debug: true,
        ..Default::default()
    }
}

fn decl(name: &str) -> Declaration {
    Declaration {
        key: Key {
            pass: name.into(),
            version: 1,
            sources: vec![0],
            specialization: vec![],
        },
        args: vec![0],
        ret: 0,
    }
}

fn body(bc: &DecodedBytecode, id: usize, call: usize) -> Function {
    let f = &bc.functions[0];
    let mut ir = air::v2::lower_with_positions(
        f.ops(),
        &vec![air::v2::TypeRef(0); 4],
        &AshModule::new(bc),
        Some(f.debug()),
    )
    .unwrap()
    .with_findex(id);
    for b in &mut ir.blocks {
        for i in &mut b.instrs {
            if let Instr::Call { fun, .. } = i {
                *fun = call;
            }
        }
    }
    ir
}

#[test]
fn deterministic_deduplicated_recursive_reservations_and_serialization() {
    let bc = fixture();
    // Force BodyFacts to be populated before registering the new functions.
    bc.body_facts();
    let make = |requests| {
        prepare(&bc, requests, Limits::default(), |d, ids| {
            let other = if d.key.pass == "a" {
                decl("b").key
            } else {
                decl("a").key
            };
            Ok(body(&bc, ids[&d.key], ids[&other]))
        })
        .unwrap()
    };
    let a = make(vec![decl("b"), decl("a"), decl("a")]);
    let b = make(vec![decl("a"), decl("b")]);
    assert_eq!(a.generated_helpers.len(), 2);
    assert_eq!(a.generated_helpers[0].findex, 1);
    assert_eq!(a.generated_helpers[0].declaration.key.pass, "a");
    assert!(a.body_facts().called.contains(&2));
    assert!(a.body_facts().called.contains(&1));
    assert_eq!(a.functions[0].debug(), bc.functions[0].debug());
    assert!(a.functions[1].debug().chunks_exact(2).any(|p| p == [0, 12]));
    let encoded = crate::bytecode_encode::encode(&a, 5).unwrap();
    assert_eq!(encoded, crate::bytecode_encode::encode(&b, 5).unwrap());
    let decoded = BytecodeDecoder::decode_bytes(&encoded).unwrap();
    assert_eq!(decoded.functions.len(), 3);
    assert!(
        decoded.functions[1]
            .ops()
            .iter()
            .any(|op| matches!(op, Opcode::Call1 { fun: RefFun(2), .. }))
    );
}

#[test]
fn atomic_fallback_for_limits_conflicts_and_invalid_references() {
    let bc = fixture();
    let original = crate::bytecode_encode::encode(&bc, 5).unwrap();
    let make = |limits| {
        prepare(&bc, [decl("a")], limits, |d, ids| {
            Ok(body(&bc, ids[&d.key], ids[&d.key]))
        })
    };
    for limits in [
        Limits {
            helpers: 0,
            ..Limits::default()
        },
        Limits {
            body_ops: 1,
            ..Limits::default()
        },
        Limits {
            total_ops: 1,
            ..Limits::default()
        },
        Limits {
            air_bytes: 1,
            ..Limits::default()
        },
    ] {
        assert!(make(limits).is_err());
    }
    let mut conflict = decl("a");
    conflict.args.clear();
    assert!(
        prepare(
            &bc,
            [decl("a"), conflict],
            Limits::default(),
            |_, _| unreachable!()
        )
        .is_err()
    );
    assert!(
        prepare(&bc, [decl("a")], Limits::default(), |d, ids| Ok(body(
            &bc,
            ids[&d.key],
            999
        )))
        .is_err()
    );
    assert!(
        prepare(&bc, [decl("a")], Limits::default(), |_, _| Ok(body(
            &bc, 999, 0
        )))
        .is_err()
    );
    assert_eq!(original, crate::bytecode_encode::encode(&bc, 5).unwrap());
}

#[test]
fn fresh_snapshots_do_not_reuse_stale_code_pools_or_specializations() {
    let bc = fixture();
    let a = probe(&bc, 0).unwrap();
    let mut changed = bc.clone();
    changed.ints[2] = 99;
    let b = probe(&changed, 0).unwrap();
    assert_ne!(
        a.compute_function_hashes()[&1],
        b.compute_function_hashes()[&1]
    );
    let mut d = decl("a");
    d.key.specialization.push(42);
    let mut e = d.clone();
    e.key.version += 1;
    let result = prepare(&bc, [decl("a"), d, e], Limits::default(), |d, ids| {
        Ok(body(&bc, ids[&d.key], ids[&d.key]))
    })
    .unwrap();
    assert_eq!(result.generated_helpers.len(), 3);
    assert!(probe(&a, 0).is_err());
    assert!(!crate::reload::diff_bytecode(&a, &changed).is_safe());
    let mut incompatible = bc.clone();
    incompatible.types[0].kind = crate::hl::hl_type_kind_HF64;
    assert!(probe(&incompatible, 0).is_err());
    assert!(a.body_facts().called.contains(&1));
    assert!(crate::reachable::analyze(&a).is_live(1));
}

#[test]
fn private_constants_are_rebased_once_for_each_helper() {
    let bc = fixture();
    let result = prepare(&bc, [decl("a"), decl("b")], Limits::default(), |d, ids| {
        let mut f = body(&bc, ids[&d.key], ids[&d.key]);
        f.pending_ints.push(if d.key.pass == "a" { 77 } else { 88 });
        for block in &mut f.blocks {
            for i in &mut block.instrs {
                if let Instr::Int { idx, .. } = i
                    && *idx == 2
                {
                    *idx = bc.ints.len();
                }
            }
        }
        Ok(f)
    })
    .unwrap();
    assert_eq!(result.ints, vec![0, 1, 3, 77, 88]);
    assert!(
        result.functions[1]
            .ops()
            .iter()
            .any(|o| matches!(o, Opcode::Int { ptr: RefInt(3), .. }))
    );
    assert!(
        result.functions[2]
            .ops()
            .iter()
            .any(|o| matches!(o, Opcode::Int { ptr: RefInt(4), .. }))
    );
}

#[test]
fn preparing_at_a_reused_address_does_not_reuse_canonical_air() {
    let mut source = fixture();
    let mut slot = probe(&source, 0).unwrap();
    let first = crate::air_pipeline::optimized(&AshModule::new(&slot), &slot.functions[1]).unwrap();
    let address = &slot as *const _;
    source.ints[2] = 99;
    slot = probe(&source, 0).unwrap();
    assert_eq!(address, &slot as *const _);
    let next = crate::air_pipeline::optimized(&AshModule::new(&slot), &slot.functions[1]).unwrap();
    assert!(!std::sync::Arc::ptr_eq(&first, &next));
    let again = crate::air_pipeline::optimized(&AshModule::new(&slot), &slot.functions[1]).unwrap();
    assert!(std::sync::Arc::ptr_eq(&next, &again));
}

#[test]
fn prepared_modules_refuse_host_metadata_changes() {
    let mut prepared = probe(&fixture(), 0).unwrap();
    let host = crate::host_module::HostModule {
        lib: "test".into(),
        classes: vec![],
    };
    assert!(
        prepared
            .register_host_module(&host)
            .unwrap_err()
            .to_string()
            .contains("before generated helper preparation")
    );
}

#[test]
fn malformed_generated_registers_are_refused_before_serialization() {
    let bc = fixture();
    for reg in [99, 1] {
        let result = prepare(&bc, [decl("a")], Limits::default(), |d, ids| {
            let mut f = body(&bc, ids[&d.key], ids[&d.key]);
            let dst = f.blocks[0]
                .instrs
                .iter()
                .find_map(|op| match op {
                    Instr::Param { dst, reg: 0 } => Some(*dst),
                    _ => None,
                })
                .unwrap();
            f.values[dst.idx()].reg = reg;
            Ok(f)
        });
        assert!(result.is_err());
    }
}
