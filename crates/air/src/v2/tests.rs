//! AIR v2 test suite: lowering shape tests, verifier negative tests, and the
//! round-trip property over a fixture corpus.
//!
//! Round-trip property: for every fixture, `serialize(lower(ops))` must
//! produce a valid executable opcode sequence with identical structural
//! semantics — proven by re-lowering the output, verifying it, and checking
//! condensed-CFG isomorphism + dominance equivalence against the original,
//! plus (where the mini interpreter covers the ops) identical input/output
//! behavior, plus register-type-table preservation.

use super::analysis::{read_class, write_class, AliasClass, CfgInfo, LoopForest};
use super::ir::*;
use super::lower::{lower, lower_with, ModuleBuilder};
use super::module::{CalleeBody, ModuleTables, NativeImport, NativeTable, NoModuleInfo};
use super::passes::{
    DeadCodeElim, FmaPeephole, GlobalValueNumbering, Inlining, LoopInvariantCodeMotion,
    NullCheckElim, OptLevel, Pass, PassManager, PassOptions, PassStats, TailRecursionElim,
};
use super::serialize::{serialize, Serialized};
use super::verify::{check_cfg_equivalent, condense_cfg, verify};
use crate::opcodes::*;
use std::collections::HashMap;

fn t(n: u32) -> TypeRef {
    TypeRef(n)
}

fn ops_text(ops: &[Opcode]) -> String {
    format!("{:?}", ops)
}

/// The round-trip harness: lower -> verify -> serialize -> re-lower ->
/// verify -> CFG isomorphism + dominance equivalence + reg-type preservation.
fn round_trip(ops: &[Opcode], reg_types: &[TypeRef]) -> (Function, Serialized, Function) {
    let f1 = lower(ops, reg_types).expect("lower failed");
    verify(&f1).unwrap_or_else(|e| panic!("verify(f1): {e}\n{}", f1.dump()));
    let out = serialize(&f1).expect("serialize failed");
    assert_eq!(
        &out.reg_types[..reg_types.len()],
        reg_types,
        "original register types must be preserved verbatim"
    );
    assert_eq!(out.num_regs, out.reg_types.len());
    for op in &out.ops {
        assert!(
            !matches!(op, Opcode::Nop | Opcode::IndirectCall { .. }),
            "serializer must emit interpreter-compatible opcodes only, got {:?}",
            op
        );
    }
    let f2 = lower(&out.ops, &out.reg_types)
        .unwrap_or_else(|e| panic!("re-lower failed: {e}\nops: {}", ops_text(&out.ops)));
    verify(&f2).unwrap_or_else(|e| panic!("verify(f2): {e}\n{}", f2.dump()));
    check_cfg_equivalent(&f1, &f2).unwrap_or_else(|e| {
        panic!(
            "CFG equivalence failed: {e}\nf1:\n{}\nf2:\n{}\nout: {}",
            f1.dump(),
            f2.dump(),
            ops_text(&out.ops)
        )
    });
    (f1, out, f2)
}

fn assert_exact(ops: &[Opcode], reg_types: &[TypeRef]) -> Serialized {
    let (_, out, _) = round_trip(ops, reg_types);
    assert_eq!(
        ops_text(&out.ops),
        ops_text(ops),
        "expected byte-identical round trip"
    );
    out
}

// ---------------------------------------------------------------------------
// mini interpreter over the serializable opcode subset used by fixtures
// ---------------------------------------------------------------------------

/// The functions a mini-interpreter run can call, keyed by findex.
#[derive(Default)]
struct MiniModule<'a> {
    ints: Vec<i32>,
    funs: HashMap<usize, (&'a [Opcode], usize)>,
}

impl<'a> MiniModule<'a> {
    fn new(ints: &[i32]) -> Self {
        MiniModule {
            ints: ints.to_vec(),
            funs: HashMap::new(),
        }
    }
    fn with_fun(mut self, findex: usize, ops: &'a [Opcode], num_regs: usize) -> Self {
        self.funs.insert(findex, (ops, num_regs));
        self
    }
    fn call(&self, findex: usize, args: &[i64], fuel: &mut usize) -> i64 {
        let (ops, num_regs) = *self
            .funs
            .get(&findex)
            .unwrap_or_else(|| panic!("mini_eval: no body for findex {}", findex));
        self.run(ops, args, num_regs, fuel)
    }

    fn run(&self, ops: &[Opcode], args: &[i64], num_regs: usize, fuel: &mut usize) -> i64 {
        mini_run(self, ops, args, num_regs, fuel)
    }
}

fn mini_eval(ops: &[Opcode], ints: &[i32], args: &[i64], num_regs: usize) -> i64 {
    let m = MiniModule::new(ints);
    let mut fuel = 100_000usize;
    mini_run(&m, ops, args, num_regs, &mut fuel)
}

/// `mini_eval` over a module, so `Call*` opcodes (self-recursion included) can
/// be executed.
fn mini_eval_in(m: &MiniModule, ops: &[Opcode], args: &[i64], num_regs: usize) -> i64 {
    let mut fuel = 100_000usize;
    mini_run(m, ops, args, num_regs, &mut fuel)
}

fn mini_run(
    m: &MiniModule,
    ops: &[Opcode],
    args: &[i64],
    num_regs: usize,
    fuel: &mut usize,
) -> i64 {
    let ints = &m.ints;
    let mut regs = vec![0i64; num_regs];
    regs[..args.len()].copy_from_slice(args);
    let mut pc = 0usize;
    loop {
        *fuel = fuel.checked_sub(1).expect("mini_eval: step limit exceeded");
        assert!(pc < ops.len(), "mini_eval: pc {} out of bounds", pc);
        let jump = |off: i32| (pc as i64 + 1 + off as i64) as usize;
        match &ops[pc] {
            Opcode::Call0 { dst, fun } => regs[dst.0 as usize] = m.call(fun.0, &[], fuel),
            Opcode::Call1 { dst, fun, arg0 } => {
                regs[dst.0 as usize] = m.call(fun.0, &[regs[arg0.0 as usize]], fuel)
            }
            Opcode::Call2 {
                dst,
                fun,
                arg0,
                arg1,
            } => {
                let a = [regs[arg0.0 as usize], regs[arg1.0 as usize]];
                regs[dst.0 as usize] = m.call(fun.0, &a, fuel);
            }
            Opcode::Call3 {
                dst,
                fun,
                arg0,
                arg1,
                arg2,
            } => {
                let a = [
                    regs[arg0.0 as usize],
                    regs[arg1.0 as usize],
                    regs[arg2.0 as usize],
                ];
                regs[dst.0 as usize] = m.call(fun.0, &a, fuel);
            }
            Opcode::CallN { dst, fun, args } => {
                let a: Vec<i64> = args.iter().map(|r| regs[r.0 as usize]).collect();
                regs[dst.0 as usize] = m.call(fun.0, &a, fuel);
            }
            Opcode::Int { dst, ptr } => regs[dst.0 as usize] = ints[ptr.0] as i64,
            Opcode::Bool { dst, value } => regs[dst.0 as usize] = *value as i64,
            Opcode::Mov { dst, src } => regs[dst.0 as usize] = regs[src.0 as usize],
            Opcode::Add { dst, a, b } => {
                regs[dst.0 as usize] = regs[a.0 as usize] + regs[b.0 as usize]
            }
            Opcode::Sub { dst, a, b } => {
                regs[dst.0 as usize] = regs[a.0 as usize] - regs[b.0 as usize]
            }
            Opcode::Mul { dst, a, b } => {
                regs[dst.0 as usize] = regs[a.0 as usize] * regs[b.0 as usize]
            }
            Opcode::Neg { dst, src } => regs[dst.0 as usize] = -regs[src.0 as usize],
            Opcode::Incr { dst } => regs[dst.0 as usize] += 1,
            Opcode::Decr { dst } => regs[dst.0 as usize] -= 1,
            Opcode::JTrue { cond, offset } => {
                if regs[cond.0 as usize] != 0 {
                    pc = jump(*offset);
                    continue;
                }
            }
            Opcode::JFalse { cond, offset } => {
                if regs[cond.0 as usize] == 0 {
                    pc = jump(*offset);
                    continue;
                }
            }
            Opcode::JSLt { a, b, offset } => {
                if regs[a.0 as usize] < regs[b.0 as usize] {
                    pc = jump(*offset);
                    continue;
                }
            }
            Opcode::JSGte { a, b, offset } => {
                if regs[a.0 as usize] >= regs[b.0 as usize] {
                    pc = jump(*offset);
                    continue;
                }
            }
            Opcode::JSGt { a, b, offset } => {
                if regs[a.0 as usize] > regs[b.0 as usize] {
                    pc = jump(*offset);
                    continue;
                }
            }
            Opcode::JSLte { a, b, offset } => {
                if regs[a.0 as usize] <= regs[b.0 as usize] {
                    pc = jump(*offset);
                    continue;
                }
            }
            Opcode::JEq { a, b, offset } => {
                if regs[a.0 as usize] == regs[b.0 as usize] {
                    pc = jump(*offset);
                    continue;
                }
            }
            Opcode::JNotEq { a, b, offset } => {
                if regs[a.0 as usize] != regs[b.0 as usize] {
                    pc = jump(*offset);
                    continue;
                }
            }
            Opcode::JAlways { offset } => {
                pc = jump(*offset);
                continue;
            }
            Opcode::Switch { reg, offsets, .. } => {
                let v = regs[reg.0 as usize];
                if v >= 0 && (v as usize) < offsets.len() {
                    pc = jump(offsets[v as usize]);
                    continue;
                }
            }
            Opcode::Label | Opcode::Nop => {}
            Opcode::Ret { ret } => return regs[ret.0 as usize],
            other => panic!("mini_eval: unsupported opcode {:?}", other),
        }
        pc += 1;
    }
}

// ---------------------------------------------------------------------------
// fixtures
// ---------------------------------------------------------------------------

fn fix_straight_line() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::Int {
                dst: Reg(0),
                ptr: RefInt(0),
            },
            Opcode::Int {
                dst: Reg(1),
                ptr: RefInt(1),
            },
            Opcode::Add {
                dst: Reg(2),
                a: Reg(0),
                b: Reg(1),
            },
            Opcode::Ret { ret: Reg(2) },
        ],
        vec![t(0); 3],
    )
}

fn fix_diamond() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::JTrue {
                cond: Reg(0),
                offset: 2,
            },
            Opcode::Int {
                dst: Reg(1),
                ptr: RefInt(0),
            },
            Opcode::JAlways { offset: 1 },
            Opcode::Int {
                dst: Reg(1),
                ptr: RefInt(1),
            },
            Opcode::Ret { ret: Reg(1) },
        ],
        vec![t(0); 2],
    )
}

/// sum 0..n with an explicit loop Label (as hlc emits).
fn fix_loop() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::Int {
                dst: Reg(1),
                ptr: RefInt(0),
            }, // sum = 0
            Opcode::Int {
                dst: Reg(2),
                ptr: RefInt(0),
            }, // i = 0
            Opcode::Int {
                dst: Reg(3),
                ptr: RefInt(1),
            }, // one = 1
            Opcode::Label,
            Opcode::JSGte {
                a: Reg(2),
                b: Reg(0),
                offset: 3,
            }, // while i < n
            Opcode::Add {
                dst: Reg(1),
                a: Reg(1),
                b: Reg(2),
            },
            Opcode::Add {
                dst: Reg(2),
                a: Reg(2),
                b: Reg(3),
            },
            Opcode::JAlways { offset: -5 },
            Opcode::Ret { ret: Reg(1) },
        ],
        vec![t(0); 4],
    )
}

/// Same loop but without the Label — the serializer must add one.
fn fix_loop_no_label() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::Int {
                dst: Reg(1),
                ptr: RefInt(0),
            },
            Opcode::Int {
                dst: Reg(2),
                ptr: RefInt(0),
            },
            Opcode::Int {
                dst: Reg(3),
                ptr: RefInt(1),
            },
            Opcode::JSGte {
                a: Reg(2),
                b: Reg(0),
                offset: 3,
            },
            Opcode::Add {
                dst: Reg(1),
                a: Reg(1),
                b: Reg(2),
            },
            Opcode::Add {
                dst: Reg(2),
                a: Reg(2),
                b: Reg(3),
            },
            Opcode::JAlways { offset: -4 },
            Opcode::Ret { ret: Reg(1) },
        ],
        vec![t(0); 4],
    )
}

/// Switch with fall-through default and a redundant trailing JAlways +0.
fn fix_switch() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::Switch {
                reg: Reg(0),
                offsets: vec![2, 4],
                end: 0,
            },
            Opcode::Int {
                dst: Reg(1),
                ptr: RefInt(0),
            }, // default arm (fallthrough)
            Opcode::JAlways { offset: 4 },
            Opcode::Int {
                dst: Reg(1),
                ptr: RefInt(1),
            }, // case 0
            Opcode::JAlways { offset: 2 },
            Opcode::Int {
                dst: Reg(1),
                ptr: RefInt(2),
            }, // case 1
            Opcode::JAlways { offset: 0 },
            Opcode::Ret { ret: Reg(1) },
        ],
        vec![t(0); 2],
    )
}

fn fix_trap() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::Trap {
                exc: Reg(1),
                offset: 4,
            }, // handler at 5
            Opcode::Null { dst: Reg(2) },
            Opcode::NullCheck { reg: Reg(2) },
            Opcode::EndTrap { exc: Reg(1) },
            Opcode::JAlways { offset: 1 }, // skip handler
            Opcode::Ret { ret: Reg(0) },   // handler
            Opcode::Ret { ret: Reg(0) },   // normal exit
        ],
        vec![t(0), t(2), t(2)],
    )
}

fn fix_nested_traps() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::Trap {
                exc: Reg(1),
                offset: 7,
            }, // outer handler at 8
            Opcode::Trap {
                exc: Reg(2),
                offset: 3,
            }, // inner handler at 5
            Opcode::NullCheck { reg: Reg(3) },
            Opcode::EndTrap { exc: Reg(2) },
            Opcode::JAlways { offset: 1 },   // -> 6
            Opcode::Rethrow { exc: Reg(2) }, // inner handler, rethrows to outer
            Opcode::EndTrap { exc: Reg(1) },
            Opcode::JAlways { offset: 1 }, // -> 9
            Opcode::Ret { ret: Reg(0) },   // outer handler
            Opcode::Ret { ret: Reg(0) },
        ],
        vec![t(0), t(2), t(2), t(2)],
    )
}

/// One Trap with two EndTraps on different paths (early return in a try).
fn fix_multi_endtrap() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::Trap {
                exc: Reg(1),
                offset: 5,
            }, // handler at 6
            Opcode::JTrue {
                cond: Reg(0),
                offset: 2,
            }, // -> 4
            Opcode::EndTrap { exc: Reg(1) },
            Opcode::Ret { ret: Reg(2) },
            Opcode::EndTrap { exc: Reg(1) },
            Opcode::Ret { ret: Reg(2) },
            Opcode::Ret { ret: Reg(2) }, // handler
        ],
        vec![t(1), t(2), t(0)],
    )
}

fn fix_incr() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::Int {
                dst: Reg(0),
                ptr: RefInt(0),
            },
            Opcode::Incr { dst: Reg(0) },
            Opcode::Incr { dst: Reg(0) },
            Opcode::Ret { ret: Reg(0) },
        ],
        vec![t(0)],
    )
}

fn fix_ref() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::Int {
                dst: Reg(0),
                ptr: RefInt(0),
            },
            Opcode::Ref {
                dst: Reg(1),
                src: Reg(0),
            },
            Opcode::Int {
                dst: Reg(2),
                ptr: RefInt(1),
            },
            Opcode::Setref {
                dst: Reg(1),
                value: Reg(2),
            },
            Opcode::Unref {
                dst: Reg(3),
                src: Reg(1),
            },
            Opcode::Ret { ret: Reg(3) },
        ],
        vec![t(0), t(9), t(0), t(0)],
    )
}

fn fix_setenumfield() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::EnumAlloc {
                dst: Reg(0),
                construct: RefEnumConstruct(1),
            },
            Opcode::Int {
                dst: Reg(1),
                ptr: RefInt(0),
            },
            Opcode::SetEnumField {
                value: Reg(0),
                field: RefField(0),
                src: Reg(1),
            },
            Opcode::Ret { ret: Reg(0) },
        ],
        vec![t(3), t(0)],
    )
}

// ---------------------------------------------------------------------------
// lowering shape tests
// ---------------------------------------------------------------------------

#[test]
fn lower_straight_line_shape() {
    let (ops, tys) = fix_straight_line();
    let f = lower(&ops, &tys).unwrap();
    verify(&f).unwrap();
    // synthetic entry + one real block
    assert_eq!(f.blocks.len(), 2);
    assert!(f.blocks.iter().all(|b| b.phis.is_empty()));
    assert!(f.cells.is_empty());
    // Params carry register types
    let params: Vec<_> = f.blocks[0]
        .instrs
        .iter()
        .filter(|i| matches!(i, Instr::Param { .. }))
        .collect();
    assert_eq!(params.len(), 3);
    for v in &f.values {
        assert_eq!(v.ty, t(0));
    }
}

#[test]
fn lower_diamond_phi() {
    let (ops, tys) = fix_diamond();
    let f = lower(&ops, &tys).unwrap();
    verify(&f).unwrap();
    // exactly one phi (for r1) at the join block, with 2 sources
    let phis: Vec<_> = f.blocks.iter().flat_map(|b| b.phis.iter()).collect();
    assert_eq!(phis.len(), 1);
    assert_eq!(phis[0].incoming.len(), 2);
    assert_eq!(f.value_reg(phis[0].dst), 1);
}

#[test]
fn lower_loop_phi_at_header() {
    let (ops, tys) = fix_loop();
    let f = lower(&ops, &tys).unwrap();
    verify(&f).unwrap();
    // loop header (block of the Label/JSGte run) has phis for sum and i
    let header = f
        .blocks
        .iter()
        .find(|b| !b.phis.is_empty())
        .expect("expected a block with phis");
    let mut regs: Vec<u32> = header.phis.iter().map(|p| f.value_reg(p.dst)).collect();
    regs.sort_unstable();
    assert_eq!(regs, vec![1, 2]);
    for p in &header.phis {
        assert_eq!(p.incoming.len(), 2, "entry edge + back edge");
    }
}

#[test]
fn lower_pinned_incr_cells() {
    let (ops, tys) = fix_incr();
    let f = lower(&ops, &tys).unwrap();
    verify(&f).unwrap();
    assert_eq!(f.cells.len(), 1);
    assert_eq!(f.cells[0].reg, 0);
    assert_eq!(f.cells[0].reason, PinReason::IncrDecr);
    let incrs = f.blocks[1]
        .instrs
        .iter()
        .filter(|i| matches!(i, Instr::CellIncr { .. }))
        .count();
    assert_eq!(incrs, 2);
    // pinned registers get no phis and no Params
    assert!(f.blocks.iter().all(|b| b.phis.is_empty()));
    assert!(!f.blocks[0]
        .instrs
        .iter()
        .any(|i| matches!(i, Instr::Param { reg: 0, .. })));
}

#[test]
fn lower_pinned_ref_cell() {
    let (ops, tys) = fix_ref();
    let f = lower(&ops, &tys).unwrap();
    verify(&f).unwrap();
    assert_eq!(f.cells.len(), 1);
    assert_eq!(f.cells[0].reg, 0);
    assert_eq!(f.cells[0].reason, PinReason::RefTaken);
    assert!(f.blocks[1]
        .instrs
        .iter()
        .any(|i| matches!(i, Instr::CellRef { .. })));
}

#[test]
fn lower_trap_regions_and_pinning() {
    let (ops, tys) = fix_trap();
    let f = lower(&ops, &tys).unwrap();
    verify(&f).unwrap();
    // r1 pinned as trap exception, r2 pinned as written-in-region
    let mut pins: Vec<(u32, PinReason)> = f.cells.iter().map(|c| (c.reg, c.reason)).collect();
    pins.sort_by_key(|p| p.0);
    assert_eq!(
        pins,
        vec![(1, PinReason::TrapExc), (2, PinReason::TrapWritten)]
    );
    // the protected block has a handler and an exceptional successor
    let trap_block = f
        .blocks
        .iter()
        .enumerate()
        .find(|(_, b)| matches!(b.term, Terminator::Trap { .. }))
        .map(|(i, _)| i)
        .unwrap();
    let (handler, normal) = match f.blocks[trap_block].term {
        Terminator::Trap {
            handler, normal, ..
        } => (handler, normal),
        _ => unreachable!(),
    };
    assert_eq!(f.blocks[normal.idx()].handler, Some(handler));
    assert!(
        f.succ_blocks(normal).contains(&handler),
        "may-throw block must have an exceptional edge"
    );
}

#[test]
fn lower_nested_traps() {
    let (ops, tys) = fix_nested_traps();
    let f = lower(&ops, &tys).unwrap();
    verify(&f).unwrap();
    assert_eq!(f.cells.len(), 2); // both exception registers pinned
                                  // the inner handler (Rethrow block) sits inside the outer region
    let rethrow_block = f
        .blocks
        .iter()
        .find(|b| matches!(b.term, Terminator::Rethrow { .. }))
        .unwrap();
    assert!(
        rethrow_block.handler.is_some(),
        "inner handler is covered by the outer trap"
    );
}

#[test]
fn lower_multi_endtrap_paths() {
    let (ops, tys) = fix_multi_endtrap();
    let f = lower(&ops, &tys).unwrap();
    verify(&f).unwrap();
    let endtraps = f
        .blocks
        .iter()
        .flat_map(|b| b.instrs.iter())
        .filter(|i| matches!(i, Instr::EndTrap { .. }))
        .count();
    assert_eq!(endtraps, 2, "one EndTrap per exit path");
}

#[test]
fn lower_setenumfield_construct() {
    let (ops, tys) = fix_setenumfield();
    let f = lower(&ops, &tys).unwrap();
    verify(&f).unwrap();
    let sef = f
        .blocks
        .iter()
        .flat_map(|b| b.instrs.iter())
        .find_map(|i| match i {
            Instr::SetEnumField { construct, .. } => Some(*construct),
            _ => None,
        })
        .expect("SetEnumField lowered");
    assert_eq!(sef, 1, "construct resolved from the preceding EnumAlloc");

    // MakeEnum variant
    let ops2 = vec![
        Opcode::Int {
            dst: Reg(1),
            ptr: RefInt(0),
        },
        Opcode::MakeEnum {
            dst: Reg(0),
            construct: RefEnumConstruct(2),
            args: vec![Reg(1)],
        },
        Opcode::SetEnumField {
            value: Reg(0),
            field: RefField(0),
            src: Reg(1),
        },
        Opcode::Ret { ret: Reg(0) },
    ];
    let f2 = lower(&ops2, &[t(3), t(0)]).unwrap();
    let sef2 = f2
        .blocks
        .iter()
        .flat_map(|b| b.instrs.iter())
        .find_map(|i| match i {
            Instr::SetEnumField { construct, .. } => Some(*construct),
            _ => None,
        })
        .unwrap();
    assert_eq!(sef2, 2, "construct resolved from the preceding MakeEnum");
}

#[test]
fn lower_callthis_normalized() {
    let ops = vec![
        Opcode::CallThis {
            dst: Reg(1),
            field: RefField(2),
            args: vec![Reg(2)],
        },
        Opcode::Ret { ret: Reg(1) },
    ];
    let tys = vec![t(5), t(0), t(0)];
    let f = lower(&ops, &tys).unwrap();
    verify(&f).unwrap();
    let cm = f
        .blocks
        .iter()
        .flat_map(|b| b.instrs.iter())
        .find_map(|i| match i {
            Instr::CallMethod { field, args, .. } => Some((*field, args.clone())),
            _ => None,
        })
        .expect("CallThis lowers to CallMethod");
    assert_eq!(cm.0, 2);
    assert_eq!(cm.1.len(), 2, "receiver prepended");
    assert_eq!(f.value_reg(cm.1[0]), 0, "receiver is reg0");
}

#[test]
fn lower_virtualclosure_immediate_field() {
    // field index 7 must NOT be treated as a register read (only 2 regs).
    let ops = vec![
        Opcode::VirtualClosure {
            dst: Reg(1),
            obj: Reg(0),
            field: Reg(7),
        },
        Opcode::Ret { ret: Reg(1) },
    ];
    let tys = vec![t(5), t(6)];
    let f = lower(&ops, &tys).unwrap();
    verify(&f).unwrap();
    let field = f
        .blocks
        .iter()
        .flat_map(|b| b.instrs.iter())
        .find_map(|i| match i {
            Instr::VirtualClosure { field, .. } => Some(*field),
            _ => None,
        })
        .unwrap();
    assert_eq!(field, 7);
    let out = serialize(&f).unwrap();
    assert!(out
        .ops
        .iter()
        .any(|o| matches!(o, Opcode::VirtualClosure { field, .. } if field.0 == 7)));
}

#[test]
fn lower_drops_unreachable() {
    let ops = vec![
        Opcode::Ret { ret: Reg(0) },
        Opcode::Int {
            dst: Reg(0),
            ptr: RefInt(0),
        },
        Opcode::Ret { ret: Reg(0) },
    ];
    let f = lower(&ops, &[t(0)]).unwrap();
    verify(&f).unwrap();
    assert_eq!(f.blocks.len(), 2); // entry + one live block
    let out = serialize(&f).unwrap();
    assert_eq!(ops_text(&out.ops), ops_text(&[Opcode::Ret { ret: Reg(0) }]));
}

#[test]
fn lower_switch_default_fallthrough() {
    let (ops, tys) = fix_switch();
    let f = lower(&ops, &tys).unwrap();
    verify(&f).unwrap();
    let (targets, default) = f
        .blocks
        .iter()
        .find_map(|b| match &b.term {
            Terminator::Switch {
                targets, default, ..
            } => Some((targets.clone(), *default)),
            _ => None,
        })
        .expect("switch lowered");
    assert_eq!(targets.len(), 2);
    // default is the block of the opcode right after the Switch
    assert!(f.blocks[default.idx()]
        .instrs
        .iter()
        .any(|i| matches!(i, Instr::Int { idx: 0, .. })));
}

#[test]
fn lower_rejects_endtrap_without_trap() {
    let ops = vec![Opcode::EndTrap { exc: Reg(0) }, Opcode::Ret { ret: Reg(0) }];
    let err = lower(&ops, &[t(0)]).unwrap_err();
    assert!(err.to_string().contains("no open trap region"), "{err}");
}

#[test]
fn lower_rejects_ret_inside_trap() {
    let ops = vec![
        Opcode::Trap {
            exc: Reg(1),
            offset: 2,
        },
        Opcode::Ret { ret: Reg(0) },
        Opcode::EndTrap { exc: Reg(1) },
        Opcode::Ret { ret: Reg(0) },
    ];
    let err = lower(&ops, &[t(0), t(2)]).unwrap_err();
    assert!(
        err.to_string().contains("inside an open trap region"),
        "{err}"
    );
}

#[test]
fn lower_rejects_indirect_call() {
    let ops = vec![
        Opcode::IndirectCall {
            dst: Reg(0),
            fun: RefFun(1),
            args: vec![],
        },
        Opcode::Ret { ret: Reg(0) },
    ];
    assert!(lower(&ops, &[t(0)]).is_err());
}

// ---------------------------------------------------------------------------
// verifier negative tests (hand-built malformed IR)
// ---------------------------------------------------------------------------

fn empty_func(reg_types: Vec<TypeRef>) -> Function {
    Function::new(reg_types)
}

#[test]
fn verify_rejects_double_def() {
    let mut f = empty_func(vec![t(0)]);
    let v0 = f.new_value(t(0), 0);
    f.blocks.push(Block {
        phis: vec![],
        instrs: vec![
            Instr::Int { dst: v0, idx: 0 },
            Instr::Int { dst: v0, idx: 1 },
        ],
        term: Terminator::Ret { value: v0 },
        handler: None,
    });
    let err = verify(&f).unwrap_err();
    assert!(err.to_string().contains("defined more than once"), "{err}");
}

#[test]
fn verify_rejects_phi_arity() {
    let mut f = empty_func(vec![t(0)]);
    let p = f.new_value(t(0), 0);
    f.blocks.push(Block {
        phis: vec![],
        instrs: vec![],
        term: Terminator::Jump { target: BlockId(1) },
        handler: None,
    });
    f.blocks.push(Block {
        phis: vec![Phi {
            dst: p,
            incoming: vec![],
        }],
        instrs: vec![],
        term: Terminator::Ret { value: p },
        handler: None,
    });
    let err = verify(&f).unwrap_err();
    assert!(err.to_string().contains("arity"), "{err}");
}

#[test]
fn verify_rejects_copy_type_mismatch() {
    let mut f = empty_func(vec![t(0), t(1)]);
    let a = f.new_value(t(0), 0);
    let b = f.new_value(t(1), 1);
    f.blocks.push(Block {
        phis: vec![],
        instrs: vec![
            Instr::Int { dst: a, idx: 0 },
            Instr::Copy { dst: b, src: a },
        ],
        term: Terminator::Ret { value: b },
        handler: None,
    });
    let err = verify(&f).unwrap_err();
    assert!(err.to_string().contains("Copy"), "{err}");
}

#[test]
fn verify_rejects_use_before_def() {
    let mut f = empty_func(vec![t(0), t(0)]);
    let a = f.new_value(t(0), 0);
    let b = f.new_value(t(0), 1);
    f.blocks.push(Block {
        phis: vec![],
        instrs: vec![
            Instr::Copy { dst: b, src: a }, // uses a before its def
            Instr::Int { dst: a, idx: 0 },
        ],
        term: Terminator::Ret { value: b },
        handler: None,
    });
    let err = verify(&f).unwrap_err();
    assert!(err.to_string().contains("not dominated"), "{err}");
}

#[test]
fn verify_rejects_unreachable_block() {
    let mut f = empty_func(vec![t(0)]);
    let v = f.new_value(t(0), 0);
    f.blocks.push(Block {
        phis: vec![],
        instrs: vec![Instr::Int { dst: v, idx: 0 }],
        term: Terminator::Ret { value: v },
        handler: None,
    });
    f.blocks.push(Block {
        phis: vec![],
        instrs: vec![],
        term: Terminator::Ret { value: v },
        handler: None,
    });
    let err = verify(&f).unwrap_err();
    assert!(err.to_string().contains("unreachable"), "{err}");
}

#[test]
fn verify_rejects_param_outside_entry() {
    let mut f = empty_func(vec![t(0)]);
    let v = f.new_value(t(0), 0);
    f.blocks.push(Block {
        phis: vec![],
        instrs: vec![],
        term: Terminator::Jump { target: BlockId(1) },
        handler: None,
    });
    f.blocks.push(Block {
        phis: vec![],
        instrs: vec![Instr::Param { dst: v, reg: 0 }],
        term: Terminator::Ret { value: v },
        handler: None,
    });
    let err = verify(&f).unwrap_err();
    assert!(err.to_string().contains("non-entry"), "{err}");
}

#[test]
fn verify_rejects_unbalanced_endtrap() {
    let mut f = empty_func(vec![t(0), t(2)]);
    f.cells.push(CellData {
        reg: 1,
        ty: t(2),
        reason: PinReason::TrapExc,
    });
    let v = f.new_value(t(0), 0);
    f.blocks.push(Block {
        phis: vec![],
        instrs: vec![
            Instr::Int { dst: v, idx: 0 },
            Instr::EndTrap { cell: CellId(0) },
        ],
        term: Terminator::Ret { value: v },
        handler: None,
    });
    let err = verify(&f).unwrap_err();
    assert!(err.to_string().contains("no open trap region"), "{err}");
}

// ---------------------------------------------------------------------------
// round-trip property tests
// ---------------------------------------------------------------------------

#[test]
fn roundtrip_straight_line_exact() {
    let (ops, tys) = fix_straight_line();
    let out = assert_exact(&ops, &tys);
    let ints = [7, 35];
    assert_eq!(
        mini_eval(&ops, &ints, &[], tys.len()),
        mini_eval(&out.ops, &ints, &[], out.num_regs)
    );
}

#[test]
fn roundtrip_diamond_exact() {
    let (ops, tys) = fix_diamond();
    let out = assert_exact(&ops, &tys);
    let ints = [10, 20];
    for cond in [0i64, 1] {
        assert_eq!(
            mini_eval(&ops, &ints, &[cond], tys.len()),
            mini_eval(&out.ops, &ints, &[cond], out.num_regs),
            "cond={}",
            cond
        );
    }
}

#[test]
fn roundtrip_loop_exact() {
    let (ops, tys) = fix_loop();
    let out = assert_exact(&ops, &tys);
    let ints = [0, 1];
    for n in [0i64, 1, 5, 10] {
        assert_eq!(
            mini_eval(&out.ops, &ints, &[n], out.num_regs),
            (0..n).sum::<i64>(),
            "sum 0..{}",
            n
        );
    }
    assert!(
        out.ops.iter().any(|o| matches!(o, Opcode::Label)),
        "backward jump target keeps its Label"
    );
}

#[test]
fn roundtrip_label_added_for_backward_jump() {
    let (ops, tys) = fix_loop_no_label();
    let (_, out, _) = round_trip(&ops, &tys);
    assert!(
        out.ops.iter().any(|o| matches!(o, Opcode::Label)),
        "serializer must add a Label for the backward jump: {}",
        ops_text(&out.ops)
    );
    let ints = [0, 1];
    for n in [0i64, 3, 7] {
        assert_eq!(
            mini_eval(&out.ops, &ints, &[n], out.num_regs),
            (0..n).sum::<i64>()
        );
    }
}

#[test]
fn roundtrip_switch_semantics() {
    let (ops, tys) = fix_switch();
    let (_, out, _) = round_trip(&ops, &tys);
    let ints = [100, 200, 300];
    for v in [0i64, 1, 2, 5, -1] {
        assert_eq!(
            mini_eval(&ops, &ints, &[v], tys.len()),
            mini_eval(&out.ops, &ints, &[v], out.num_regs),
            "switch value {}",
            v
        );
    }
    // Switch default must remain the fall-through in the output.
    let sw_idx = out
        .ops
        .iter()
        .position(|o| matches!(o, Opcode::Switch { .. }))
        .unwrap();
    assert!(
        matches!(out.ops[sw_idx + 1], Opcode::Int { ptr: RefInt(0), .. }),
        "default arm must follow the Switch"
    );
}

#[test]
fn roundtrip_trap_exact() {
    let (ops, tys) = fix_trap();
    assert_exact(&ops, &tys);
}

#[test]
fn roundtrip_nested_trap_exact() {
    let (ops, tys) = fix_nested_traps();
    assert_exact(&ops, &tys);
}

#[test]
fn roundtrip_multi_endtrap_exact() {
    let (ops, tys) = fix_multi_endtrap();
    assert_exact(&ops, &tys);
}

#[test]
fn roundtrip_incr_exact() {
    let (ops, tys) = fix_incr();
    let out = assert_exact(&ops, &tys);
    let ints = [40];
    assert_eq!(mini_eval(&out.ops, &ints, &[], out.num_regs), 42);
}

#[test]
fn roundtrip_ref_exact() {
    let (ops, tys) = fix_ref();
    assert_exact(&ops, &tys);
}

#[test]
fn roundtrip_setenumfield_exact() {
    let (ops, tys) = fix_setenumfield();
    assert_exact(&ops, &tys);
}

#[test]
fn roundtrip_callthis_serializes_callmethod() {
    let ops = vec![
        Opcode::CallThis {
            dst: Reg(1),
            field: RefField(2),
            args: vec![Reg(2)],
        },
        Opcode::Ret { ret: Reg(1) },
    ];
    let tys = vec![t(5), t(0), t(0)];
    let (_, out, _) = round_trip(&ops, &tys);
    let expected = vec![
        Opcode::CallMethod {
            dst: Reg(1),
            field: RefField(2),
            args: vec![Reg(0), Reg(2)],
        },
        Opcode::Ret { ret: Reg(1) },
    ];
    assert_eq!(ops_text(&out.ops), ops_text(&expected));
}

#[test]
fn roundtrip_getthis_setthis_field() {
    let ops = vec![
        Opcode::GetThis {
            dst: Reg(1),
            field: RefField(3),
        },
        Opcode::SetThis {
            field: RefField(4),
            src: Reg(1),
        },
        Opcode::Ret { ret: Reg(1) },
    ];
    let tys = vec![t(5), t(0)];
    let (_, out, _) = round_trip(&ops, &tys);
    let expected = vec![
        Opcode::Field {
            dst: Reg(1),
            obj: Reg(0),
            field: RefField(3),
        },
        Opcode::SetField {
            obj: Reg(0),
            field: RefField(4),
            src: Reg(1),
        },
        Opcode::Ret { ret: Reg(1) },
    ];
    assert_eq!(ops_text(&out.ops), ops_text(&expected));
}

#[test]
fn roundtrip_idempotent() {
    for (ops, tys) in [fix_diamond(), fix_loop(), fix_switch(), fix_trap()] {
        let (_, out1, _) = round_trip(&ops, &tys);
        let (_, out2, _) = round_trip(&out1.ops, &out1.reg_types);
        assert_eq!(
            ops_text(&out1.ops),
            ops_text(&out2.ops),
            "serialize(lower(.)) must be a fixpoint after one iteration"
        );
        assert_eq!(out1.reg_types, out2.reg_types);
    }
}

// ---------------------------------------------------------------------------
// de-SSA / parallel-copy tests (hand-built IR with non-trivial phis)
// ---------------------------------------------------------------------------

#[test]
fn dessa_nontrivial_phi_copies() {
    // b0: v0=param r0; c1=Int(r1); if v0 -> b1 else b2
    // b1: Jump b3      b2: d=Int(r2); Jump b3
    // b3: p(r4) = phi[(b1,c1),(b2,d)]; Ret p       (both sources non-trivial)
    let mut f = empty_func(vec![t(0); 5]);
    let v0 = f.new_value(t(0), 0);
    let c1 = f.new_value(t(0), 1);
    let d = f.new_value(t(0), 2);
    let p = f.new_value(t(0), 4);
    f.blocks.push(Block {
        phis: vec![],
        instrs: vec![
            Instr::Param { dst: v0, reg: 0 },
            Instr::Int { dst: c1, idx: 0 },
        ],
        term: Terminator::CondJump {
            cond: CondKind::True,
            a: v0,
            b: None,
            if_true: BlockId(1),
            if_false: BlockId(2),
        },
        handler: None,
    });
    f.blocks.push(Block {
        phis: vec![],
        instrs: vec![],
        term: Terminator::Jump { target: BlockId(3) },
        handler: None,
    });
    f.blocks.push(Block {
        phis: vec![],
        instrs: vec![Instr::Int { dst: d, idx: 1 }],
        term: Terminator::Jump { target: BlockId(3) },
        handler: None,
    });
    f.blocks.push(Block {
        phis: vec![Phi {
            dst: p,
            incoming: vec![(BlockId(1), c1), (BlockId(2), d)],
        }],
        instrs: vec![],
        term: Terminator::Ret { value: p },
        handler: None,
    });
    verify(&f).unwrap_or_else(|e| panic!("{e}\n{}", f.dump()));
    let out = serialize(&f).unwrap();
    let movs = out
        .ops
        .iter()
        .filter(|o| matches!(o, Opcode::Mov { .. }))
        .count();
    assert_eq!(
        movs,
        2,
        "one copy per incoming edge: {}",
        ops_text(&out.ops)
    );
    let ints = [11, 22];
    for cond in [0i64, 1] {
        let expect = if cond != 0 { 11 } else { 22 };
        assert_eq!(mini_eval(&out.ops, &ints, &[cond], out.num_regs), expect);
    }
}

#[test]
fn dessa_swap_cycle_temp() {
    // while i < n { (a, b) = (b, a); i++ } — a phi cycle needing a temp.
    let mut f = empty_func(vec![t(0); 5]);
    let vn = f.new_value(t(0), 0);
    let va = f.new_value(t(0), 1);
    let vb = f.new_value(t(0), 2);
    let vi = f.new_value(t(0), 3);
    let vone = f.new_value(t(0), 4);
    let pa = f.new_value(t(0), 1);
    let pb = f.new_value(t(0), 2);
    let pi = f.new_value(t(0), 3);
    let pi2 = f.new_value(t(0), 3);
    f.blocks.push(Block {
        phis: vec![],
        instrs: vec![
            Instr::Param { dst: vn, reg: 0 },
            Instr::Int { dst: va, idx: 1 },
            Instr::Int { dst: vb, idx: 2 },
            Instr::Int { dst: vi, idx: 0 },
            Instr::Int { dst: vone, idx: 3 },
        ],
        term: Terminator::Jump { target: BlockId(1) },
        handler: None,
    });
    f.blocks.push(Block {
        phis: vec![
            Phi {
                dst: pa,
                incoming: vec![(BlockId(0), va), (BlockId(2), pb)],
            },
            Phi {
                dst: pb,
                incoming: vec![(BlockId(0), vb), (BlockId(2), pa)],
            },
            Phi {
                dst: pi,
                incoming: vec![(BlockId(0), vi), (BlockId(2), pi2)],
            },
        ],
        instrs: vec![],
        term: Terminator::CondJump {
            cond: CondKind::SLt,
            a: pi,
            b: Some(vn),
            if_true: BlockId(2),
            if_false: BlockId(3),
        },
        handler: None,
    });
    f.blocks.push(Block {
        phis: vec![],
        instrs: vec![Instr::BinOp {
            op: BinOp::Add,
            dst: pi2,
            a: pi,
            b: vone,
        }],
        term: Terminator::Jump { target: BlockId(1) },
        handler: None,
    });
    f.blocks.push(Block {
        phis: vec![],
        instrs: vec![],
        term: Terminator::Ret { value: pa },
        handler: None,
    });
    verify(&f).unwrap_or_else(|e| panic!("{e}\n{}", f.dump()));
    let out = serialize(&f).unwrap();
    // A temp register was appended, typed like the register it shadows.
    assert_eq!(
        out.num_regs,
        6,
        "swap cycle needs one temp: {}",
        ops_text(&out.ops)
    );
    assert_eq!(out.reg_types[5], t(0));
    // (a0, b0) = (7, 9): even iteration counts return a0, odd return b0.
    let ints = [0, 7, 9, 1];
    for (n, expect) in [(0i64, 7i64), (1, 9), (2, 7), (3, 9)] {
        assert_eq!(
            mini_eval(&out.ops, &ints, &[n], out.num_regs),
            expect,
            "n={}",
            n
        );
    }
}

#[test]
fn dessa_critical_edge_split() {
    // b0 has two successors and b1 (the phi block) has two predecessors:
    // the b0->b1 edge is critical and must get a dedicated copy block.
    let mut f = empty_func(vec![t(0); 5]);
    let v0 = f.new_value(t(0), 0);
    let c1 = f.new_value(t(0), 1);
    let d = f.new_value(t(0), 3);
    let p = f.new_value(t(0), 4);
    f.blocks.push(Block {
        phis: vec![],
        instrs: vec![
            Instr::Param { dst: v0, reg: 0 },
            Instr::Int { dst: c1, idx: 0 },
        ],
        term: Terminator::CondJump {
            cond: CondKind::True,
            a: v0,
            b: None,
            if_true: BlockId(1),
            if_false: BlockId(2),
        },
        handler: None,
    });
    f.blocks.push(Block {
        phis: vec![Phi {
            dst: p,
            incoming: vec![(BlockId(0), c1), (BlockId(2), d)],
        }],
        instrs: vec![],
        term: Terminator::Ret { value: p },
        handler: None,
    });
    f.blocks.push(Block {
        phis: vec![],
        instrs: vec![Instr::Int { dst: d, idx: 1 }],
        term: Terminator::Jump { target: BlockId(1) },
        handler: None,
    });
    verify(&f).unwrap_or_else(|e| panic!("{e}\n{}", f.dump()));
    let out = serialize(&f).unwrap();
    let ints = [51, 62];
    for (cond, expect) in [(1i64, 51i64), (0, 62)] {
        assert_eq!(
            mini_eval(&out.ops, &ints, &[cond], out.num_regs),
            expect,
            "cond={}: {}",
            cond,
            ops_text(&out.ops)
        );
    }
}

#[test]
fn dessa_rejects_handler_phi_copies() {
    // A non-trivial phi at a trap handler block cannot be materialized:
    // there is no program point on the exceptional edge.
    let mut f = empty_func(vec![t(0); 6]);
    f.cells.push(CellData {
        reg: 5,
        ty: t(0),
        reason: PinReason::TrapExc,
    });
    let v0 = f.new_value(t(0), 0);
    let v1 = f.new_value(t(0), 1);
    let p = f.new_value(t(0), 3);
    f.blocks.push(Block {
        phis: vec![],
        instrs: vec![
            Instr::Param { dst: v0, reg: 0 },
            Instr::Param { dst: v1, reg: 1 },
        ],
        term: Terminator::Trap {
            exc_cell: CellId(0),
            handler: BlockId(2),
            normal: BlockId(1),
        },
        handler: None,
    });
    f.blocks.push(Block {
        phis: vec![],
        instrs: vec![Instr::EndTrap { cell: CellId(0) }],
        term: Terminator::Jump { target: BlockId(3) },
        handler: Some(BlockId(2)),
    });
    f.blocks.push(Block {
        phis: vec![Phi {
            dst: p,
            incoming: vec![(BlockId(0), v1)], // r3 != r1: needs a copy
        }],
        instrs: vec![],
        term: Terminator::Ret { value: p },
        handler: None,
    });
    f.blocks.push(Block {
        phis: vec![],
        instrs: vec![],
        term: Terminator::Ret { value: v0 },
        handler: None,
    });
    verify(&f).unwrap_or_else(|e| panic!("{e}\n{}", f.dump()));
    let err = serialize(&f).unwrap_err();
    assert!(err.to_string().contains("handler"), "{err}");
}

// ---------------------------------------------------------------------------
// infrastructure self-tests
// ---------------------------------------------------------------------------

#[test]
fn mini_interp_sanity() {
    let (ops, tys) = fix_loop();
    let ints = [0, 1];
    assert_eq!(mini_eval(&ops, &ints, &[5], tys.len()), 10);
    let (ops, tys) = fix_switch();
    let ints = [100, 200, 300];
    assert_eq!(mini_eval(&ops, &ints, &[0], tys.len()), 200);
    assert_eq!(mini_eval(&ops, &ints, &[1], tys.len()), 300);
    assert_eq!(mini_eval(&ops, &ints, &[9], tys.len()), 100);
}

#[test]
fn cfg_equiv_detects_difference() {
    let (a_ops, a_tys) = fix_straight_line();
    let (b_ops, b_tys) = fix_diamond();
    let fa = lower(&a_ops, &a_tys).unwrap();
    let fb = lower(&b_ops, &b_tys).unwrap();
    assert!(check_cfg_equivalent(&fa, &fb).is_err());
}

#[test]
fn condense_contracts_chains() {
    let (ops, tys) = fix_straight_line();
    let f = lower(&ops, &tys).unwrap();
    let c = condense_cfg(&f);
    assert_eq!(
        c.num_nodes, 1,
        "entry -> body chain contracts to a single node"
    );

    let (ops, tys) = fix_diamond();
    let f = lower(&ops, &tys).unwrap();
    let c = condense_cfg(&f);
    // entry+cond contract; then/else/join stay: 4 nodes
    assert_eq!(c.num_nodes, 4);
}

// ---------------------------------------------------------------------------
// module declarations: native imports and float types
// ---------------------------------------------------------------------------

/// Module info describing two natives (findex 5 and 7) and one float type.
fn demo_module() -> ModuleTables {
    let mut natives = NativeTable::new();
    natives
        .declare(NativeImport::new(5, "std", "alloc_bytes", vec![t(0)], t(2)))
        .unwrap();
    natives
        .declare(NativeImport::new(7, "std", "sin", vec![t(1)], t(1)))
        .unwrap();
    ModuleTables::new()
        .with_natives(natives)
        .with_float_types([t(1)])
}

fn fix_calls_natives() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            // findex 5 is a native, findex 6 is a bytecode function.
            Opcode::Call1 {
                dst: Reg(2),
                fun: RefFun(5),
                arg0: Reg(0),
            },
            Opcode::Call1 {
                dst: Reg(2),
                fun: RefFun(6),
                arg0: Reg(0),
            },
            Opcode::Call1 {
                dst: Reg(2),
                fun: RefFun(5),
                arg0: Reg(0),
            },
            Opcode::Ret { ret: Reg(2) },
        ],
        vec![t(0), t(0), t(2)],
    )
}

#[test]
fn lower_records_native_imports() {
    let (ops, tys) = fix_calls_natives();
    let f = lower_with(&ops, &tys, &demo_module()).unwrap();
    verify(&f).unwrap();
    assert_eq!(f.natives.len(), 1, "only the referenced native is declared");
    let imp = f.natives.get(5).expect("findex 5 declared");
    assert_eq!(imp.lib, "std");
    assert_eq!(imp.name, "alloc_bytes");
    assert_eq!(imp.args, vec![t(0)]);
    assert_eq!(imp.ret, t(2));
    assert_eq!(imp.symbol(), "std@alloc_bytes");
    assert!(
        f.natives.get(6).is_none(),
        "bytecode functions are not native imports"
    );
    assert!(!f.natives.get(5).unwrap().name.is_empty());
}

#[test]
fn lower_records_closure_targets_as_imports() {
    let ops = vec![
        Opcode::StaticClosure {
            dst: Reg(1),
            fun: RefFun(7),
        },
        Opcode::Ret { ret: Reg(1) },
    ];
    let f = lower_with(&ops, &[t(0), t(4)], &demo_module()).unwrap();
    assert_eq!(f.natives.len(), 1);
    assert_eq!(f.natives.get(7).unwrap().name, "sin");
}

#[test]
fn lower_without_module_info_declares_nothing() {
    let (ops, tys) = fix_calls_natives();
    let f = lower(&ops, &tys).unwrap();
    assert!(f.natives.is_empty());
    assert!(f.float_types.is_empty());
    let f2 = lower_with(&ops, &tys, &NoModuleInfo).unwrap();
    assert!(f2.natives.is_empty());
}

#[test]
fn lower_records_float_types() {
    let (ops, tys) = fix_float_mul_add();
    let f = lower_with(&ops, &tys, &demo_module()).unwrap();
    assert_eq!(f.float_types, vec![t(1)]);
    assert!(f.is_float(t(1)));
    assert!(!f.is_float(t(0)));
}

#[test]
fn module_builder_remembers_declarations_across_lowerings() {
    let mut mb = ModuleBuilder::new(demo_module());
    let (ops, tys) = fix_calls_natives();
    let f1 = mb.lower(&ops, &tys).unwrap();
    assert_eq!(mb.natives().len(), 1);

    let ops2 = vec![
        Opcode::Call1 {
            dst: Reg(1),
            fun: RefFun(7),
            arg0: Reg(0),
        },
        Opcode::Ret { ret: Reg(1) },
    ];
    let f2 = mb.lower(&ops2, &[t(1), t(1)]).unwrap();
    assert_eq!(mb.lowered(), 2);
    assert_eq!(
        mb.natives().len(),
        2,
        "the module table is the union of both functions"
    );
    // Each function still carries only what it references.
    assert_eq!(f1.natives.len(), 1);
    assert_eq!(f2.natives.len(), 1);
    assert!(mb.natives().contains(5) && mb.natives().contains(7));
    let order: Vec<usize> = mb.natives().iter().map(|i| i.findex).collect();
    assert_eq!(order, vec![5, 7], "declaration order is deterministic");
}

#[test]
fn native_declarations_survive_the_round_trip() {
    let (ops, tys) = fix_calls_natives();
    let info = demo_module();
    let f1 = lower_with(&ops, &tys, &info).unwrap();
    verify(&f1).unwrap();
    let out = serialize(&f1).unwrap();
    let f2 = lower_with(&out.ops, &out.reg_types, &info).unwrap();
    verify(&f2).unwrap();
    assert_eq!(f1.natives, f2.natives);
    assert_eq!(f1.float_types, f2.float_types);
}

#[test]
fn verify_rejects_unsorted_float_types() {
    let mut f = empty_func(vec![t(0)]);
    let v = f.new_value(t(0), 0);
    f.blocks.push(Block {
        phis: vec![],
        instrs: vec![Instr::Int { dst: v, idx: 0 }],
        term: Terminator::Ret { value: v },
        handler: None,
    });
    f.float_types = vec![t(3), t(1)];
    let err = verify(&f).unwrap_err();
    assert!(err.to_string().contains("float type table"), "{err}");
}

// ---------------------------------------------------------------------------
// float fixtures + IR-level float evaluator (the FMA rewrite is only visible
// before serialization, which un-fuses it again)
// ---------------------------------------------------------------------------

/// `r3 = r0 * r1; r4 = r3 + r2; ret r4`
fn fix_float_mul_add() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::Mul {
                dst: Reg(3),
                a: Reg(0),
                b: Reg(1),
            },
            Opcode::Add {
                dst: Reg(4),
                a: Reg(3),
                b: Reg(2),
            },
            Opcode::Ret { ret: Reg(4) },
        ],
        vec![t(1); 5],
    )
}

fn float_fn(ops: &[Opcode], tys: &[TypeRef]) -> Function {
    let f = lower_with(ops, tys, &demo_module()).expect("lower");
    verify(&f).expect("verify");
    f
}

fn run_pass(f: &mut Function, pass: &dyn Pass, opts: PassOptions) -> PassStats {
    let stats = pass.run(f, &opts).expect("pass failed");
    verify(f).unwrap_or_else(|e| panic!("{} broke the IR: {e}\n{}", pass.name(), f.dump()));
    stats
}

fn count_instrs(f: &Function, pred: impl Fn(&Instr) -> bool) -> usize {
    f.blocks
        .iter()
        .flat_map(|b| b.instrs.iter())
        .filter(|i| pred(i))
        .count()
}

/// Evaluate a float function over the IR, so `Fma` semantics are observable.
fn eval_f64(f: &Function, floats: &[f64], args: &[f64]) -> f64 {
    let mut vals: HashMap<ValueId, f64> = HashMap::new();
    let mut cur = BlockId(0);
    let mut prev: Option<BlockId> = None;
    for _ in 0..10_000 {
        let blk = &f.blocks[cur.idx()];
        let mut committed: Vec<(ValueId, f64)> = Vec::new();
        for phi in &blk.phis {
            let p = prev.expect("phi outside the entry block");
            let src = phi
                .incoming
                .iter()
                .find(|(b, _)| *b == p)
                .expect("phi source for predecessor")
                .1;
            committed.push((phi.dst, vals[&src]));
        }
        for (d, v) in committed {
            vals.insert(d, v);
        }
        for ins in &blk.instrs {
            match ins {
                Instr::Param { dst, reg } => {
                    vals.insert(*dst, args.get(*reg as usize).copied().unwrap_or(0.0));
                }
                Instr::Float { dst, idx } => {
                    vals.insert(*dst, floats[*idx]);
                }
                Instr::Copy { dst, src } => {
                    let v = vals[src];
                    vals.insert(*dst, v);
                }
                Instr::BinOp { op, dst, a, b } => {
                    let (x, y) = (vals[a], vals[b]);
                    let r = match op {
                        BinOp::Add => x + y,
                        BinOp::Sub => x - y,
                        BinOp::Mul => x * y,
                        BinOp::SDiv => x / y,
                        other => panic!("eval_f64: unsupported op {:?}", other),
                    };
                    vals.insert(*dst, r);
                }
                Instr::UnOp {
                    op: UnOp::Neg,
                    dst,
                    src,
                } => {
                    let v = -vals[src];
                    vals.insert(*dst, v);
                }
                Instr::Fma { dst, a, b, c } => {
                    let r = vals[a].mul_add(vals[b], vals[c]);
                    vals.insert(*dst, r);
                }
                other => panic!("eval_f64: unsupported instruction {:?}", other),
            }
        }
        match &blk.term {
            Terminator::Ret { value } => return vals[value],
            Terminator::Jump { target } => {
                prev = Some(cur);
                cur = *target;
            }
            Terminator::CondJump {
                cond,
                a,
                b,
                if_true,
                if_false,
            } => {
                let x = vals[a];
                let taken = match cond {
                    CondKind::True => x != 0.0,
                    CondKind::False => x == 0.0,
                    CondKind::SLt => x < vals[&b.unwrap()],
                    CondKind::SGte => x >= vals[&b.unwrap()],
                    other => panic!("eval_f64: unsupported cond {:?}", other),
                };
                prev = Some(cur);
                cur = if taken { *if_true } else { *if_false };
            }
            other => panic!("eval_f64: unsupported terminator {:?}", other),
        }
    }
    panic!("eval_f64: step limit exceeded");
}

/// Float mini-interpreter over serialized opcodes.
fn mini_eval_f64(ops: &[Opcode], args: &[f64], num_regs: usize) -> f64 {
    let mut regs = vec![0f64; num_regs];
    regs[..args.len()].copy_from_slice(args);
    let mut pc = 0usize;
    loop {
        assert!(pc < ops.len(), "mini_eval_f64: pc out of bounds");
        match &ops[pc] {
            Opcode::Mov { dst, src } => regs[dst.0 as usize] = regs[src.0 as usize],
            Opcode::Add { dst, a, b } => {
                regs[dst.0 as usize] = regs[a.0 as usize] + regs[b.0 as usize]
            }
            Opcode::Sub { dst, a, b } => {
                regs[dst.0 as usize] = regs[a.0 as usize] - regs[b.0 as usize]
            }
            Opcode::Mul { dst, a, b } => {
                regs[dst.0 as usize] = regs[a.0 as usize] * regs[b.0 as usize]
            }
            Opcode::Neg { dst, src } => regs[dst.0 as usize] = -regs[src.0 as usize],
            Opcode::Label | Opcode::Nop => {}
            Opcode::Ret { ret } => return regs[ret.0 as usize],
            other => panic!("mini_eval_f64: unsupported opcode {:?}", other),
        }
        pc += 1;
    }
}

// ---------------------------------------------------------------------------
// FMA peephole
// ---------------------------------------------------------------------------

#[test]
fn fma_fuses_mul_into_add() {
    let (ops, tys) = fix_float_mul_add();
    let mut f = float_fn(&ops, &tys);
    let stats = run_pass(&mut f, &FmaPeephole, PassOptions::default());
    assert_eq!(stats.fused, 1);
    assert_eq!(count_instrs(&f, |i| matches!(i, Instr::Fma { .. })), 1);
    assert_eq!(
        count_instrs(&f, |i| matches!(i, Instr::BinOp { op: BinOp::Mul, .. })),
        0,
        "the multiply is consumed"
    );
}

#[test]
fn fma_fuses_when_product_is_the_right_addend() {
    // r4 = r2 + (r0 * r1): the multiply feeds the second operand.
    let ops = vec![
        Opcode::Mul {
            dst: Reg(3),
            a: Reg(0),
            b: Reg(1),
        },
        Opcode::Add {
            dst: Reg(4),
            a: Reg(2),
            b: Reg(3),
        },
        Opcode::Ret { ret: Reg(4) },
    ];
    let mut f = float_fn(&ops, &vec![t(1); 5]);
    let stats = run_pass(&mut f, &FmaPeephole, PassOptions::default());
    assert_eq!(stats.fused, 1);
    let fma = f.blocks[1]
        .instrs
        .iter()
        .find_map(|i| match i {
            Instr::Fma { a, b, c, .. } => Some((*a, *b, *c)),
            _ => None,
        })
        .expect("fused");
    assert_eq!(f.value_reg(fma.0), 0);
    assert_eq!(f.value_reg(fma.1), 1);
    assert_eq!(f.value_reg(fma.2), 2, "the addend is the other operand");
}

#[test]
fn fma_sub_with_product_as_minuend_negates_the_addend() {
    // r4 = (r0 * r1) - r2  ->  n = -r2; r4 = fma(r0, r1, n)
    let ops = vec![
        Opcode::Mul {
            dst: Reg(3),
            a: Reg(0),
            b: Reg(1),
        },
        Opcode::Sub {
            dst: Reg(4),
            a: Reg(3),
            b: Reg(2),
        },
        Opcode::Ret { ret: Reg(4) },
    ];
    let mut f = float_fn(&ops, &vec![t(1); 5]);
    let stats = run_pass(&mut f, &FmaPeephole, PassOptions::default());
    assert_eq!(stats.fused, 1);
    let body = &f.blocks[1].instrs;
    assert!(
        matches!(body[0], Instr::UnOp { op: UnOp::Neg, .. }),
        "negation of the subtrahend comes first: {:?}",
        body
    );
    assert!(matches!(body[1], Instr::Fma { .. }));
    // Semantics: a*b - c, one rounding.
    let args = [3.0e16, 3.0, 1.0];
    assert_eq!(eval_f64(&f, &[], &args), args[0].mul_add(args[1], -args[2]));
}

#[test]
fn fma_sub_with_product_as_subtrahend_negates_a_multiplicand() {
    // r4 = r2 - (r0 * r1)  ->  n = -r0; r4 = fma(n, r1, r2)
    let ops = vec![
        Opcode::Mul {
            dst: Reg(3),
            a: Reg(0),
            b: Reg(1),
        },
        Opcode::Sub {
            dst: Reg(4),
            a: Reg(2),
            b: Reg(3),
        },
        Opcode::Ret { ret: Reg(4) },
    ];
    let mut f = float_fn(&ops, &vec![t(1); 5]);
    let stats = run_pass(&mut f, &FmaPeephole, PassOptions::default());
    assert_eq!(stats.fused, 1);
    let body = &f.blocks[1].instrs;
    assert!(
        matches!(body[0], Instr::UnOp { op: UnOp::Neg, .. }),
        "the multiply is replaced in place by the negation: {:?}",
        body
    );
    let neg_dst = body[0].dst().unwrap();
    assert!(
        matches!(body[1], Instr::Fma { a, .. } if a == neg_dst),
        "the negated multiplicand feeds the fma"
    );
    let args = [3.0e16, 3.0, 1.0];
    assert_eq!(
        eval_f64(&f, &[], &args),
        (-args[0]).mul_add(args[1], args[2])
    );
}

#[test]
fn fma_refuses_when_the_product_has_two_uses() {
    // r3 = r0 * r1; r4 = r3 + r2; r5 = r3 + r4 — r3 is used twice.
    let ops = vec![
        Opcode::Mul {
            dst: Reg(3),
            a: Reg(0),
            b: Reg(1),
        },
        Opcode::Add {
            dst: Reg(4),
            a: Reg(3),
            b: Reg(2),
        },
        Opcode::Add {
            dst: Reg(5),
            a: Reg(3),
            b: Reg(4),
        },
        Opcode::Ret { ret: Reg(5) },
    ];
    let mut f = float_fn(&ops, &vec![t(1); 6]);
    let stats = run_pass(&mut f, &FmaPeephole, PassOptions::default());
    assert_eq!(stats.fused, 0, "a two-use product must not be fused");
    assert_eq!(count_instrs(&f, |i| matches!(i, Instr::Fma { .. })), 0);
}

#[test]
fn fma_refuses_non_float_types() {
    let (ops, _) = fix_float_mul_add();
    let tys = vec![t(0); 5]; // t(0) is not a declared float type
    let mut f = lower_with(&ops, &tys, &demo_module()).unwrap();
    let stats = run_pass(&mut f, &FmaPeephole, PassOptions::default());
    assert_eq!(stats.fused, 0);
}

#[test]
fn fma_refuses_across_blocks() {
    // The multiply and the add live in different blocks.
    let ops = vec![
        Opcode::Mul {
            dst: Reg(3),
            a: Reg(0),
            b: Reg(1),
        },
        Opcode::JAlways { offset: 0 },
        Opcode::Add {
            dst: Reg(4),
            a: Reg(3),
            b: Reg(2),
        },
        Opcode::Ret { ret: Reg(4) },
    ];
    let mut f = float_fn(&ops, &vec![t(1); 5]);
    let stats = run_pass(&mut f, &FmaPeephole, PassOptions::default());
    assert_eq!(stats.fused, 0, "cross-block fusion is refused");
}

#[test]
fn fma_refuses_when_an_operand_register_is_overwritten() {
    // r3 = r0 * r1; r0 = r2 (clobbers an operand); r4 = r3 + r2
    let ops = vec![
        Opcode::Mul {
            dst: Reg(3),
            a: Reg(0),
            b: Reg(1),
        },
        Opcode::Mov {
            dst: Reg(0),
            src: Reg(2),
        },
        Opcode::Add {
            dst: Reg(4),
            a: Reg(3),
            b: Reg(2),
        },
        Opcode::Ret { ret: Reg(4) },
    ];
    let mut f = float_fn(&ops, &vec![t(1); 5]);
    let stats = run_pass(&mut f, &FmaPeephole, PassOptions::default());
    assert_eq!(
        stats.fused, 0,
        "operand register no longer holds the operand"
    );
}

#[test]
fn fma_is_inert_when_disabled() {
    let (ops, tys) = fix_float_mul_add();
    let mut f = float_fn(&ops, &tys);
    let before = f.dump();
    let opts = PassOptions {
        fma: false,
        ..PassOptions::default()
    };
    let stats = run_pass(&mut f, &FmaPeephole, opts);
    assert_eq!(stats, PassStats::default());
    assert_eq!(f.dump(), before, "strict-IEEE mode leaves the IR alone");

    // The same pipeline through the manager.
    let mut f2 = float_fn(&ops, &tys);
    let pm = PassManager::new(OptLevel::O2).with_options(opts);
    let report = pm.run(&mut f2).unwrap();
    assert_eq!(report.stats_for("fma").fused, 0);
    assert_eq!(count_instrs(&f2, |i| matches!(i, Instr::Fma { .. })), 0);
}

#[test]
fn fma_result_matches_mul_add_and_differs_from_unfused() {
    let (ops, tys) = fix_float_mul_add();
    let mut f = float_fn(&ops, &tys);
    // (1+e)^2 rounds to 1+2e, so the unfused sum cancels to zero while the
    // fused one keeps the exact product's tail.
    let args = [
        1.0 + f64::EPSILON,
        1.0 + f64::EPSILON,
        -(1.0 + 2.0 * f64::EPSILON),
    ];
    let unfused = eval_f64(&f, &[], &args);
    run_pass(&mut f, &FmaPeephole, PassOptions::default());
    let fused = eval_f64(&f, &[], &args);
    assert_eq!(fused, args[0].mul_add(args[1], args[2]));
    assert_ne!(
        fused, unfused,
        "the fixture must actually distinguish one rounding from two"
    );
    assert_eq!(unfused, args[0] * args[1] + args[2]);
}

#[test]
fn fma_serializes_back_to_mul_add() {
    let (ops, tys) = fix_float_mul_add();
    let mut f = float_fn(&ops, &tys);
    run_pass(&mut f, &FmaPeephole, PassOptions::default());
    let out = serialize(&f).unwrap();
    assert_eq!(
        out.ops
            .iter()
            .filter(|o| matches!(o, Opcode::Mul { .. }))
            .count(),
        1,
        "HL has no fma opcode: {}",
        ops_text(&out.ops)
    );
    assert_eq!(
        out.ops
            .iter()
            .filter(|o| matches!(o, Opcode::Add { .. }))
            .count(),
        1
    );
    assert!(
        out.num_regs > tys.len(),
        "a scratch register carries the product"
    );
    // The un-fused output reproduces the original bytecode's arithmetic.
    let args = [
        1.0 + f64::EPSILON,
        1.0 + f64::EPSILON,
        -(1.0 + 2.0 * f64::EPSILON),
    ];
    assert_eq!(
        mini_eval_f64(&out.ops, &args, out.num_regs),
        args[0] * args[1] + args[2]
    );
    // And it re-lowers to a verifiable function.
    let f2 = lower_with(&out.ops, &out.reg_types, &demo_module()).unwrap();
    verify(&f2).unwrap();
    check_cfg_equivalent(&f, &f2).unwrap();
}

#[test]
fn fma_sub_forms_serialize_to_the_original_arithmetic() {
    for (sub_ops, expect) in [
        (
            vec![
                Opcode::Mul {
                    dst: Reg(3),
                    a: Reg(0),
                    b: Reg(1),
                },
                Opcode::Sub {
                    dst: Reg(4),
                    a: Reg(3),
                    b: Reg(2),
                },
                Opcode::Ret { ret: Reg(4) },
            ],
            0,
        ),
        (
            vec![
                Opcode::Mul {
                    dst: Reg(3),
                    a: Reg(0),
                    b: Reg(1),
                },
                Opcode::Sub {
                    dst: Reg(4),
                    a: Reg(2),
                    b: Reg(3),
                },
                Opcode::Ret { ret: Reg(4) },
            ],
            1,
        ),
    ] {
        let args = [1.0 + f64::EPSILON, 1.0 + f64::EPSILON, 2.0];
        let mut f = float_fn(&sub_ops, &vec![t(1); 5]);
        let unfused_ir = eval_f64(&f, &[], &args);
        run_pass(&mut f, &FmaPeephole, PassOptions::default());
        let out = serialize(&f).unwrap();
        assert_eq!(
            mini_eval_f64(&out.ops, &args, out.num_regs),
            unfused_ir,
            "serialization restores the unfused arithmetic (form {})",
            expect
        );
        let f2 = lower_with(&out.ops, &out.reg_types, &demo_module()).unwrap();
        verify(&f2).unwrap();
    }
}

#[test]
fn fma_fuses_one_product_per_add() {
    // r4 = r0*r1; r5 = r2*r2; r6 = r4 + r5 — one fusion, the other multiply
    // survives as the addend. This is the `i*i + j*j` shape.
    let ops = vec![
        Opcode::Mul {
            dst: Reg(4),
            a: Reg(0),
            b: Reg(1),
        },
        Opcode::Mul {
            dst: Reg(5),
            a: Reg(2),
            b: Reg(2),
        },
        Opcode::Add {
            dst: Reg(6),
            a: Reg(4),
            b: Reg(5),
        },
        Opcode::Ret { ret: Reg(6) },
    ];
    let mut f = float_fn(&ops, &vec![t(1); 7]);
    let stats = run_pass(&mut f, &FmaPeephole, PassOptions::default());
    assert_eq!(stats.fused, 1);
    assert_eq!(
        count_instrs(&f, |i| matches!(i, Instr::BinOp { op: BinOp::Mul, .. })),
        1,
        "the addend multiply stays"
    );
    let args = [3.0, 4.0, 5.0];
    assert_eq!(
        eval_f64(&f, &[], &args),
        args[0].mul_add(args[1], args[2] * args[2])
    );
}

// ---------------------------------------------------------------------------
// analysis: natural loops and alias classes
// ---------------------------------------------------------------------------

#[test]
fn loop_forest_finds_the_natural_loop() {
    let (ops, tys) = fix_loop();
    let f = lower(&ops, &tys).unwrap();
    let cfg = CfgInfo::build(&f);
    let forest = LoopForest::analyze(&f, &cfg);
    assert_eq!(forest.len(), 1);
    let l = forest.roots[0];
    let lp = forest.get(l);
    assert_eq!(lp.latches.len(), 1);
    assert!(lp.contains(lp.header));
    assert_eq!(lp.depth, 0);
    assert_eq!(forest.depth_of(lp.header), 1);
    assert_eq!(forest.innermost_of(lp.header), Some(l));
    // Entry predecessors are outside the loop, exits leave it.
    let entries = forest.entry_preds(&cfg, l);
    assert_eq!(entries.len(), 1);
    assert!(!lp.contains(entries[0]));
    let exits = forest.exiting_blocks(&cfg, l);
    assert!(!exits.is_empty());
    // Blocks outside the loop have depth 0.
    assert_eq!(forest.depth_of(BlockId(0)), 0);
}

/// Nested counting loops: `for i in 0..n { for j in 0..n {} }`.
fn fix_nested_loops() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::Int {
                dst: Reg(1),
                ptr: RefInt(0),
            }, // i = 0
            Opcode::Int {
                dst: Reg(3),
                ptr: RefInt(1),
            }, // one = 1
            Opcode::Int {
                dst: Reg(4),
                ptr: RefInt(0),
            }, // acc = 0
            Opcode::Label, // 3: outer header
            Opcode::JSGte {
                a: Reg(1),
                b: Reg(0),
                offset: 7,
            }, // 4: exit outer -> 12
            Opcode::Int {
                dst: Reg(2),
                ptr: RefInt(0),
            }, // 5: j = 0
            Opcode::Label, // 6: inner header
            Opcode::JSGte {
                a: Reg(2),
                b: Reg(0),
                offset: 3,
            }, // 7: exit inner -> 11
            Opcode::Add {
                dst: Reg(4),
                a: Reg(4),
                b: Reg(3),
            }, // 8
            Opcode::Add {
                dst: Reg(2),
                a: Reg(2),
                b: Reg(3),
            }, // 9: j++
            Opcode::JAlways { offset: -4 }, // 10 -> 6
            Opcode::Add {
                dst: Reg(1),
                a: Reg(1),
                b: Reg(3),
            }, // 11: i++
            Opcode::JAlways { offset: -10 }, // 12 -> wrong; patched below
            Opcode::Ret { ret: Reg(4) },
        ],
        vec![t(0); 5],
    )
}

#[test]
fn loop_forest_nests_inner_loops() {
    let (mut ops, tys) = fix_nested_loops();
    // op 12 jumps back to the outer header (op 3).
    ops[12] = Opcode::JAlways { offset: -10 };
    let f = lower(&ops, &tys).unwrap();
    verify(&f).unwrap();
    let cfg = CfgInfo::build(&f);
    let forest = LoopForest::analyze(&f, &cfg);
    assert_eq!(forest.len(), 2, "one outer and one inner loop");
    let inner = forest.innermost_first()[0];
    let outer = forest.get(inner).parent.expect("inner loop has a parent");
    assert_eq!(forest.get(inner).depth, 1);
    assert_eq!(forest.get(outer).depth, 0);
    assert!(forest.get(outer).children.contains(&inner));
    assert!(forest.roots.contains(&outer));
    assert!(forest
        .get(inner)
        .blocks
        .iter()
        .all(|&b| forest.get(outer).contains(b)));
    assert_eq!(forest.depth_of(forest.get(inner).header), 2);
}

#[test]
fn alias_classes_separate_storage() {
    let of0 = AliasClass::ObjField { ty: t(5), field: 0 };
    let of1 = AliasClass::ObjField { ty: t(5), field: 1 };
    let other_ty = AliasClass::ObjField { ty: t(6), field: 0 };
    assert!(of0.may_alias(of0));
    assert!(!of0.may_alias(of1), "different field slots are disjoint");
    assert!(!of0.may_alias(other_ty), "different types are disjoint");
    assert!(!AliasClass::ArrayData.may_alias(AliasClass::ArrayLen));
    assert!(!AliasClass::Global(1).may_alias(AliasClass::Global(2)));
    assert!(AliasClass::Global(1).may_alias(AliasClass::Global(1)));
    assert!(!AliasClass::RawBytes.may_alias(AliasClass::ArrayData));
    assert!(!AliasClass::Cell(CellId(0)).may_alias(AliasClass::Cell(CellId(1))));
    assert!(!AliasClass::EnumParam {
        construct: 0,
        field: 0
    }
    .may_alias(AliasClass::EnumParam {
        construct: 1,
        field: 0
    }));
    // Reflective access can resolve to an object field.
    assert!(AliasClass::DynBox.may_alias(of0));
    assert!(of0.may_alias(AliasClass::DynBox));
    // Unknown storage aliases everything.
    for c in [
        of0,
        AliasClass::ArrayData,
        AliasClass::ArrayLen,
        AliasClass::RawBytes,
        AliasClass::DynBox,
        AliasClass::Cell(CellId(3)),
        AliasClass::Global(9),
    ] {
        assert!(c.may_alias(AliasClass::Any));
        assert!(AliasClass::Any.may_alias(c));
    }
}

#[test]
fn alias_classification_of_instructions() {
    let v = ValueId(0);
    assert_eq!(
        read_class(&Instr::FieldGet {
            dst: v,
            obj: v,
            obj_ty: t(5),
            field: 3
        }),
        Some(AliasClass::ObjField { ty: t(5), field: 3 })
    );
    assert_eq!(
        read_class(&Instr::ArraySize { dst: v, array: v }),
        Some(AliasClass::ArrayLen)
    );
    assert_eq!(
        read_class(&Instr::MemGet {
            kind: MemAccess::Array,
            dst: v,
            base: v,
            index: v
        }),
        Some(AliasClass::ArrayData)
    );
    assert_eq!(
        read_class(&Instr::MemGet {
            kind: MemAccess::I8,
            dst: v,
            base: v,
            index: v
        }),
        Some(AliasClass::RawBytes)
    );
    // Allocations write only storage nothing else can reach yet.
    assert_eq!(write_class(&Instr::New { dst: v }), None);
    assert_eq!(
        write_class(&Instr::Call {
            dst: v,
            fun: 0,
            args: vec![]
        }),
        Some(AliasClass::Any)
    );
    assert_eq!(
        write_class(&Instr::CellIncr { cell: CellId(2) }),
        Some(AliasClass::Cell(CellId(2)))
    );
    assert_eq!(read_class(&Instr::Null { dst: v }), None);
}

// ---------------------------------------------------------------------------
// null-check elimination
// ---------------------------------------------------------------------------

#[test]
fn nullcheck_elim_removes_dominated_check() {
    let ops = vec![
        Opcode::NullCheck { reg: Reg(0) },
        Opcode::NullCheck { reg: Reg(0) },
        Opcode::Ret { ret: Reg(0) },
    ];
    let mut f = lower(&ops, &[t(5)]).unwrap();
    let stats = run_pass(&mut f, &NullCheckElim, PassOptions::default());
    assert_eq!(stats.eliminated, 1);
    assert_eq!(
        count_instrs(&f, |i| matches!(i, Instr::NullCheck { .. })),
        1
    );
}

#[test]
fn nullcheck_elim_knows_allocations_are_non_null() {
    let ops = vec![
        Opcode::New { dst: Reg(1) },
        Opcode::NullCheck { reg: Reg(1) },
        Opcode::Ret { ret: Reg(1) },
    ];
    let mut f = lower(&ops, &[t(5), t(5)]).unwrap();
    let stats = run_pass(&mut f, &NullCheckElim, PassOptions::default());
    assert_eq!(stats.eliminated, 1);
    assert_eq!(
        count_instrs(&f, |i| matches!(i, Instr::NullCheck { .. })),
        0
    );
}

#[test]
fn nullcheck_elim_follows_copies() {
    let ops = vec![
        Opcode::New { dst: Reg(1) },
        Opcode::Mov {
            dst: Reg(2),
            src: Reg(1),
        },
        Opcode::NullCheck { reg: Reg(2) },
        Opcode::Ret { ret: Reg(2) },
    ];
    let mut f = lower(&ops, &[t(5); 3]).unwrap();
    let stats = run_pass(&mut f, &NullCheckElim, PassOptions::default());
    assert_eq!(stats.eliminated, 1);
}

#[test]
fn nullcheck_elim_uses_guard_edges() {
    // if (r0 != null) { nullcheck r0 }  — the check is redundant.
    let ops = vec![
        Opcode::JNull {
            reg: Reg(0),
            offset: 1,
        },
        Opcode::NullCheck { reg: Reg(0) },
        Opcode::Ret { ret: Reg(0) },
    ];
    let mut f = lower(&ops, &[t(5)]).unwrap();
    let stats = run_pass(&mut f, &NullCheckElim, PassOptions::default());
    assert_eq!(stats.eliminated, 1, "{}", f.dump());
}

#[test]
fn nullcheck_elim_keeps_unproven_checks() {
    let ops = vec![
        Opcode::NullCheck { reg: Reg(0) },
        Opcode::NullCheck { reg: Reg(1) },
        Opcode::Ret { ret: Reg(0) },
    ];
    let mut f = lower(&ops, &[t(5), t(5)]).unwrap();
    let stats = run_pass(&mut f, &NullCheckElim, PassOptions::default());
    assert_eq!(stats.eliminated, 0, "different values, nothing proven");
    assert_eq!(
        count_instrs(&f, |i| matches!(i, Instr::NullCheck { .. })),
        2
    );
}

#[test]
fn nullcheck_elim_keeps_the_last_thrower_of_a_trapped_block() {
    // Inside the trap region the only may-throw instruction is the second
    // NullCheck: removing it would delete the handler's incoming edge.
    let ops = vec![
        Opcode::Trap {
            exc: Reg(1),
            offset: 4,
        },
        Opcode::NullCheck { reg: Reg(2) },
        Opcode::NullCheck { reg: Reg(2) },
        Opcode::EndTrap { exc: Reg(1) },
        Opcode::JAlways { offset: 1 },
        Opcode::Ret { ret: Reg(0) },
        Opcode::Ret { ret: Reg(0) },
    ];
    let tys = vec![t(0), t(2), t(5)];
    let f0 = lower(&ops, &tys).unwrap();
    let handler_preds = f0.preds();
    let mut f = f0.clone();
    let stats = run_pass(&mut f, &NullCheckElim, PassOptions::default());
    assert_eq!(
        stats.eliminated, 1,
        "the redundant check goes, one thrower stays"
    );
    assert_eq!(
        count_instrs(&f, |i| matches!(i, Instr::NullCheck { .. })),
        1
    );
    assert_eq!(
        f.preds(),
        handler_preds,
        "the exceptional edge and every phi arity survive"
    );

    // With only one check in the region, nothing is removable at all.
    let ops2 = vec![
        Opcode::Trap {
            exc: Reg(1),
            offset: 4,
        },
        Opcode::New { dst: Reg(2) },
        Opcode::NullCheck { reg: Reg(2) },
        Opcode::EndTrap { exc: Reg(1) },
        Opcode::JAlways { offset: 1 },
        Opcode::Ret { ret: Reg(0) },
        Opcode::Ret { ret: Reg(0) },
    ];
    let mut f2 = lower(&ops2, &tys).unwrap();
    let stats2 = run_pass(&mut f2, &NullCheckElim, PassOptions::default());
    assert_eq!(
        stats2.eliminated, 0,
        "provably non-null, but it is the block's only exceptional edge"
    );
}

// ---------------------------------------------------------------------------
// GVN / CSE
// ---------------------------------------------------------------------------

#[test]
fn gvn_numbers_pure_arithmetic() {
    let ops = vec![
        Opcode::Add {
            dst: Reg(2),
            a: Reg(0),
            b: Reg(1),
        },
        Opcode::Add {
            dst: Reg(3),
            a: Reg(0),
            b: Reg(1),
        },
        Opcode::Add {
            dst: Reg(4),
            a: Reg(2),
            b: Reg(3),
        },
        Opcode::Ret { ret: Reg(4) },
    ];
    let tys = vec![t(0); 5];
    let mut f = lower(&ops, &tys).unwrap();
    let stats = run_pass(&mut f, &GlobalValueNumbering, PassOptions::default());
    assert_eq!(stats.eliminated, 1);
    assert!(stats.replaced >= 1);
    // Semantics survive serialization.
    let out = serialize(&f).unwrap();
    let ints = [0];
    for (a, b) in [(3i64, 4i64), (-1, 9)] {
        assert_eq!(
            mini_eval(&out.ops, &ints, &[a, b], out.num_regs),
            mini_eval(&ops, &ints, &[a, b], tys.len())
        );
    }
}

#[test]
fn gvn_numbers_commutative_operands_up_to_order() {
    let ops = vec![
        Opcode::Add {
            dst: Reg(2),
            a: Reg(0),
            b: Reg(1),
        },
        Opcode::Add {
            dst: Reg(3),
            a: Reg(1),
            b: Reg(0),
        },
        Opcode::Add {
            dst: Reg(4),
            a: Reg(2),
            b: Reg(3),
        },
        Opcode::Ret { ret: Reg(4) },
    ];
    let mut f = lower(&ops, &[t(0); 5]).unwrap();
    let stats = run_pass(&mut f, &GlobalValueNumbering, PassOptions::default());
    assert_eq!(stats.eliminated, 1);
}

#[test]
fn gvn_refuses_trapping_arithmetic() {
    let ops = vec![
        Opcode::SDiv {
            dst: Reg(2),
            a: Reg(0),
            b: Reg(1),
        },
        Opcode::SDiv {
            dst: Reg(3),
            a: Reg(0),
            b: Reg(1),
        },
        Opcode::Add {
            dst: Reg(4),
            a: Reg(2),
            b: Reg(3),
        },
        Opcode::Ret { ret: Reg(4) },
    ];
    let mut f = lower(&ops, &[t(0); 5]).unwrap();
    let stats = run_pass(&mut f, &GlobalValueNumbering, PassOptions::default());
    assert_eq!(stats.eliminated, 0, "division may throw");
}

#[test]
fn gvn_reuses_a_load_with_no_intervening_write() {
    let ops = vec![
        Opcode::Field {
            dst: Reg(1),
            obj: Reg(0),
            field: RefField(0),
        },
        Opcode::Field {
            dst: Reg(2),
            obj: Reg(0),
            field: RefField(0),
        },
        Opcode::Add {
            dst: Reg(3),
            a: Reg(1),
            b: Reg(2),
        },
        Opcode::Ret { ret: Reg(3) },
    ];
    let mut f = lower(&ops, &[t(5), t(0), t(0), t(0)]).unwrap();
    let stats = run_pass(&mut f, &GlobalValueNumbering, PassOptions::default());
    assert_eq!(stats.eliminated, 1);
    assert_eq!(count_instrs(&f, |i| matches!(i, Instr::FieldGet { .. })), 1);
}

#[test]
fn gvn_refuses_a_load_across_an_aliasing_write() {
    let ops = vec![
        Opcode::Field {
            dst: Reg(1),
            obj: Reg(0),
            field: RefField(0),
        },
        Opcode::SetField {
            obj: Reg(0),
            field: RefField(0),
            src: Reg(1),
        },
        Opcode::Field {
            dst: Reg(2),
            obj: Reg(0),
            field: RefField(0),
        },
        Opcode::Add {
            dst: Reg(3),
            a: Reg(1),
            b: Reg(2),
        },
        Opcode::Ret { ret: Reg(3) },
    ];
    let mut f = lower(&ops, &[t(5), t(0), t(0), t(0)]).unwrap();
    let stats = run_pass(&mut f, &GlobalValueNumbering, PassOptions::default());
    assert_eq!(stats.eliminated, 0, "the field was written in between");
    assert_eq!(count_instrs(&f, |i| matches!(i, Instr::FieldGet { .. })), 2);
}

#[test]
fn gvn_allows_a_load_across_a_disjoint_write() {
    let ops = vec![
        Opcode::Field {
            dst: Reg(1),
            obj: Reg(0),
            field: RefField(0),
        },
        Opcode::SetField {
            obj: Reg(0),
            field: RefField(1),
            src: Reg(1),
        },
        Opcode::Field {
            dst: Reg(2),
            obj: Reg(0),
            field: RefField(0),
        },
        Opcode::Add {
            dst: Reg(3),
            a: Reg(1),
            b: Reg(2),
        },
        Opcode::Ret { ret: Reg(3) },
    ];
    let mut f = lower(&ops, &[t(5), t(0), t(0), t(0)]).unwrap();
    let stats = run_pass(&mut f, &GlobalValueNumbering, PassOptions::default());
    assert_eq!(stats.eliminated, 1, "a different field slot cannot alias");
}

#[test]
fn gvn_refuses_a_load_across_a_call() {
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
    let stats = run_pass(&mut f, &GlobalValueNumbering, PassOptions::default());
    assert_eq!(stats.eliminated, 0, "a call clobbers all memory");
}

#[test]
fn gvn_refuses_a_load_clobbered_on_the_loop_back_edge() {
    // The dominating load is in the preheader, the write happens after the
    // second load inside the loop: only the back edge exposes it.
    let ops = vec![
        Opcode::Field {
            dst: Reg(2),
            obj: Reg(1),
            field: RefField(0),
        }, // 0: dominating load
        Opcode::Label, // 1
        Opcode::Field {
            dst: Reg(4),
            obj: Reg(1),
            field: RefField(0),
        }, // 2: candidate
        Opcode::SetField {
            obj: Reg(1),
            field: RefField(0),
            src: Reg(3),
        }, // 3: clobber
        Opcode::JSLt {
            a: Reg(3),
            b: Reg(0),
            offset: -4,
        }, // 4 -> 1
        Opcode::Ret { ret: Reg(4) },
    ];
    let tys = vec![t(0), t(5), t(0), t(0), t(0)];
    let mut f = lower(&ops, &tys).unwrap();
    let stats = run_pass(&mut f, &GlobalValueNumbering, PassOptions::default());
    assert_eq!(
        stats.eliminated,
        0,
        "the write reaches the candidate around the back edge:\n{}",
        f.dump()
    );

    // Control: the identical loop without the write does reuse the load.
    let mut ops2 = ops.clone();
    ops2[3] = Opcode::Nop;
    let mut g = lower(&ops2, &tys).unwrap();
    let gs = run_pass(&mut g, &GlobalValueNumbering, PassOptions::default());
    assert_eq!(
        gs.eliminated,
        1,
        "without the clobber the load is redundant:\n{}",
        g.dump()
    );
}

#[test]
fn gvn_refuses_a_cell_load_across_a_cell_write() {
    let ops = vec![
        Opcode::Int {
            dst: Reg(0),
            ptr: RefInt(0),
        },
        Opcode::Incr { dst: Reg(0) },
        Opcode::Mov {
            dst: Reg(1),
            src: Reg(0),
        },
        Opcode::Incr { dst: Reg(0) },
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
    let tys = vec![t(0); 4];
    let base_ops = ops.clone();
    let mut f = lower(&ops, &tys).unwrap();
    run_pass(&mut f, &GlobalValueNumbering, PassOptions::default());
    assert_eq!(
        count_instrs(&f, |i| matches!(i, Instr::CellGet { .. })),
        2,
        "cells are memory: an Incr between two reads blocks reuse"
    );
    let out = serialize(&f).unwrap();
    let ints = [40];
    assert_eq!(
        mini_eval(&out.ops, &ints, &[], out.num_regs),
        mini_eval(&base_ops, &ints, &[], tys.len())
    );
}

#[test]
fn gvn_propagates_copies() {
    let ops = vec![
        Opcode::Mov {
            dst: Reg(1),
            src: Reg(0),
        },
        Opcode::Add {
            dst: Reg(2),
            a: Reg(1),
            b: Reg(1),
        },
        Opcode::Ret { ret: Reg(2) },
    ];
    let tys = vec![t(0); 3];
    let mut f = lower(&ops, &tys).unwrap();
    let stats = run_pass(&mut f, &GlobalValueNumbering, PassOptions::default());
    assert_eq!(stats.eliminated, 1, "the copy is gone");
    assert_eq!(count_instrs(&f, |i| matches!(i, Instr::Copy { .. })), 0);
    let out = serialize(&f).unwrap();
    for a in [3i64, -7] {
        assert_eq!(
            mini_eval(&out.ops, &[0], &[a], out.num_regs),
            mini_eval(&ops, &[0], &[a], tys.len())
        );
    }
}

// ---------------------------------------------------------------------------
// LICM
// ---------------------------------------------------------------------------

/// `while (i < n) { t = a * a; i = i + 1 } return t` — `t` is invariant.
fn fix_loop_invariant() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::Int {
                dst: Reg(2),
                ptr: RefInt(0),
            }, // i = 0
            Opcode::Int {
                dst: Reg(3),
                ptr: RefInt(1),
            }, // one = 1
            Opcode::Label,
            Opcode::JSGte {
                a: Reg(2),
                b: Reg(0),
                offset: 3,
            },
            Opcode::Mul {
                dst: Reg(4),
                a: Reg(1),
                b: Reg(1),
            }, // invariant
            Opcode::Add {
                dst: Reg(2),
                a: Reg(2),
                b: Reg(3),
            },
            Opcode::JAlways { offset: -4 },
            Opcode::Ret { ret: Reg(4) },
        ],
        vec![t(0); 5],
    )
}

#[test]
fn licm_hoists_invariant_arithmetic() {
    let (ops, tys) = fix_loop_invariant();
    let mut f = lower(&ops, &tys).unwrap();
    let cfg = CfgInfo::build(&f);
    let forest = LoopForest::analyze(&f, &cfg);
    let body: Vec<BlockId> = forest.get(forest.roots[0]).blocks.clone();
    let stats = run_pass(&mut f, &LoopInvariantCodeMotion, PassOptions::default());
    assert_eq!(stats.hoisted, 1, "{}", f.dump());
    let still_in_loop = body.iter().any(|&b| {
        f.blocks[b.idx()]
            .instrs
            .iter()
            .any(|i| matches!(i, Instr::BinOp { op: BinOp::Mul, .. }))
    });
    assert!(!still_in_loop, "the multiply left the loop:\n{}", f.dump());
    // Behaviour is unchanged.
    let out = serialize(&f).unwrap();
    let ints = [0, 1];
    for n in [0i64, 1, 4] {
        assert_eq!(
            mini_eval(&out.ops, &ints, &[n, 6], out.num_regs),
            mini_eval(&ops, &ints, &[n, 6], tys.len()),
            "n={}",
            n
        );
    }
}

#[test]
fn licm_creates_a_preheader_when_the_header_has_several_entries() {
    // Two paths reach the loop header, so no existing block qualifies.
    let ops = vec![
        Opcode::JTrue {
            cond: Reg(5),
            offset: 2,
        }, // 0
        Opcode::Int {
            dst: Reg(2),
            ptr: RefInt(0),
        }, // 1: i = 0
        Opcode::JAlways { offset: 1 }, // 2 -> 4
        Opcode::Int {
            dst: Reg(2),
            ptr: RefInt(1),
        }, // 3: i = 1
        Opcode::Label,                 // 4: header, two entry edges
        Opcode::JSGte {
            a: Reg(2),
            b: Reg(0),
            offset: 3,
        }, // 5
        Opcode::Mul {
            dst: Reg(4),
            a: Reg(1),
            b: Reg(1),
        }, // 6: invariant
        Opcode::Add {
            dst: Reg(2),
            a: Reg(2),
            b: Reg(3),
        }, // 7
        Opcode::JAlways { offset: -5 }, // 8 -> 4
        Opcode::Ret { ret: Reg(4) },
    ];
    let tys = vec![t(0); 6];
    let mut f = lower(&ops, &tys).unwrap();
    let before_blocks = f.blocks.len();
    let stats = run_pass(&mut f, &LoopInvariantCodeMotion, PassOptions::default());
    assert_eq!(stats.hoisted, 1, "{}", f.dump());
    assert_eq!(
        f.blocks.len(),
        before_blocks + 1,
        "a preheader was inserted"
    );
    let out = serialize(&f).unwrap();
    let ints = [0, 1];
    for cond in [0i64, 1] {
        for n in [0i64, 3] {
            assert_eq!(
                mini_eval(&out.ops, &ints, &[n, 5, 0, 1, 0, cond], out.num_regs),
                mini_eval(&ops, &ints, &[n, 5, 0, 1, 0, cond], tys.len()),
                "cond={} n={}",
                cond,
                n
            );
        }
    }
}

#[test]
fn licm_refuses_to_hoist_out_of_a_trap_region() {
    // The same invariant multiply, but the loop sits inside a try block: the
    // preheader would be outside the region.
    let ops = vec![
        Opcode::Trap {
            exc: Reg(1),
            offset: 5,
        }, // 0 -> handler at 6
        Opcode::Label, // 1: header
        Opcode::Mul {
            dst: Reg(3),
            a: Reg(4),
            b: Reg(4),
        }, // 2: invariant
        Opcode::JSLt {
            a: Reg(2),
            b: Reg(0),
            offset: -3,
        }, // 3 -> 1
        Opcode::EndTrap { exc: Reg(1) }, // 4
        Opcode::JAlways { offset: 1 }, // 5 -> 7
        Opcode::Ret { ret: Reg(0) }, // 6: handler
        Opcode::Ret { ret: Reg(0) }, // 7
    ];
    let tys = vec![t(0), t(2), t(0), t(0), t(0)];
    let mut f = lower(&ops, &tys).unwrap();
    let before = f.dump();
    let stats = run_pass(&mut f, &LoopInvariantCodeMotion, PassOptions::default());
    assert_eq!(
        stats.hoisted, 0,
        "hoisting would cross the trap-region boundary"
    );
    assert_eq!(f.dump(), before);

    // Control: the identical loop outside a trap region does hoist.
    let plain = vec![
        Opcode::Label,
        Opcode::Mul {
            dst: Reg(3),
            a: Reg(4),
            b: Reg(4),
        },
        Opcode::JSLt {
            a: Reg(2),
            b: Reg(0),
            offset: -3,
        },
        Opcode::Ret { ret: Reg(0) },
    ];
    let mut g = lower(&plain, &tys).unwrap();
    let gs = run_pass(&mut g, &LoopInvariantCodeMotion, PassOptions::default());
    assert_eq!(gs.hoisted, 1, "control case must hoist:\n{}", g.dump());
}

#[test]
fn licm_refuses_a_load_written_inside_the_loop() {
    let ops = vec![
        Opcode::Label,
        Opcode::Field {
            dst: Reg(2),
            obj: Reg(1),
            field: RefField(0),
        },
        Opcode::SetField {
            obj: Reg(1),
            field: RefField(0),
            src: Reg(2),
        },
        Opcode::JSLt {
            a: Reg(3),
            b: Reg(0),
            offset: -4,
        },
        Opcode::Ret { ret: Reg(2) },
    ];
    let tys = vec![t(0), t(5), t(0), t(0)];
    let mut f = lower(&ops, &tys).unwrap();
    let stats = run_pass(&mut f, &LoopInvariantCodeMotion, PassOptions::default());
    assert_eq!(stats.hoisted, 0, "the loop writes the field it loads");

    // Control: with the write on a different field slot the load hoists.
    let ops2 = vec![
        Opcode::Label,
        Opcode::Field {
            dst: Reg(2),
            obj: Reg(1),
            field: RefField(0),
        },
        Opcode::SetField {
            obj: Reg(1),
            field: RefField(1),
            src: Reg(2),
        },
        Opcode::JSLt {
            a: Reg(3),
            b: Reg(0),
            offset: -4,
        },
        Opcode::Ret { ret: Reg(2) },
    ];
    let mut g = lower(&ops2, &tys).unwrap();
    let gs = run_pass(&mut g, &LoopInvariantCodeMotion, PassOptions::default());
    assert_eq!(gs.hoisted, 1, "disjoint field slot:\n{}", g.dump());
}

#[test]
fn licm_refuses_a_load_that_the_loop_can_skip() {
    // The load sits on a conditional path, so it does not dominate the loop
    // exit and hoisting it would speculate a dereference.
    let ops = vec![
        Opcode::Label, // 0: header
        Opcode::JTrue {
            cond: Reg(3),
            offset: 1,
        }, // 1
        Opcode::Field {
            dst: Reg(2),
            obj: Reg(1),
            field: RefField(0),
        }, // 2: conditional load
        Opcode::JSLt {
            a: Reg(3),
            b: Reg(0),
            offset: -4,
        }, // 3 -> 0
        Opcode::Ret { ret: Reg(2) },
    ];
    let tys = vec![t(0), t(5), t(0), t(0)];
    let mut f = lower(&ops, &tys).unwrap();
    let stats = run_pass(&mut f, &LoopInvariantCodeMotion, PassOptions::default());
    assert_eq!(
        stats.hoisted,
        0,
        "the load is not guaranteed to execute:\n{}",
        f.dump()
    );
}

// ---------------------------------------------------------------------------
// DCE
// ---------------------------------------------------------------------------

#[test]
fn dce_removes_unused_pure_chains() {
    let ops = vec![
        Opcode::Int {
            dst: Reg(0),
            ptr: RefInt(0),
        },
        Opcode::Int {
            dst: Reg(1),
            ptr: RefInt(1),
        },
        Opcode::Add {
            dst: Reg(2),
            a: Reg(0),
            b: Reg(1),
        },
        Opcode::Int {
            dst: Reg(3),
            ptr: RefInt(2),
        },
        Opcode::Ret { ret: Reg(3) },
    ];
    let mut f = lower(&ops, &[t(0); 4]).unwrap();
    let stats = run_pass(&mut f, &DeadCodeElim, PassOptions::default());
    assert_eq!(stats.eliminated, 3, "the add and both of its inputs");
    assert_eq!(count_instrs(&f, |i| matches!(i, Instr::BinOp { .. })), 0);
    assert_eq!(count_instrs(&f, |i| matches!(i, Instr::Int { .. })), 1);
    // Params are the entry-register surface and stay.
    assert_eq!(count_instrs(&f, |i| matches!(i, Instr::Param { .. })), 4);
}

#[test]
fn dce_keeps_side_effecting_instructions() {
    let ops = vec![
        Opcode::Call0 {
            dst: Reg(1),
            fun: RefFun(2),
        }, // unused result, but a call
        Opcode::SetField {
            obj: Reg(0),
            field: RefField(0),
            src: Reg(1),
        },
        Opcode::NullCheck { reg: Reg(0) },
        Opcode::Ret { ret: Reg(0) },
    ];
    let mut f = lower(&ops, &[t(5), t(0)]).unwrap();
    let stats = run_pass(&mut f, &DeadCodeElim, PassOptions::default());
    assert_eq!(stats.eliminated, 0);
    assert_eq!(count_instrs(&f, |i| matches!(i, Instr::Call { .. })), 1);
    assert_eq!(count_instrs(&f, |i| matches!(i, Instr::FieldSet { .. })), 1);
    assert_eq!(
        count_instrs(&f, |i| matches!(i, Instr::NullCheck { .. })),
        1
    );
}

#[test]
fn dce_removes_dead_phis() {
    let ops = vec![
        Opcode::JTrue {
            cond: Reg(0),
            offset: 1,
        },
        Opcode::Int {
            dst: Reg(1),
            ptr: RefInt(0),
        },
        Opcode::Ret { ret: Reg(0) },
    ];
    let mut f = lower(&ops, &[t(0), t(0)]).unwrap();
    let phis_before: usize = f.blocks.iter().map(|b| b.phis.len()).sum();
    assert_eq!(phis_before, 1, "r1 joins at the merge block");
    let stats = run_pass(&mut f, &DeadCodeElim, PassOptions::default());
    assert!(stats.eliminated >= 2, "the phi and its source");
    assert_eq!(f.blocks.iter().map(|b| b.phis.len()).sum::<usize>(), 0);
}

// ---------------------------------------------------------------------------
// pass manager
// ---------------------------------------------------------------------------

#[test]
fn pass_manager_o0_is_inert() {
    let (ops, tys) = fix_loop();
    let mut f = lower(&ops, &tys).unwrap();
    let before = f.dump();
    let report = PassManager::new(OptLevel::O0).run(&mut f).unwrap();
    assert!(report.per_pass.is_empty());
    assert!(!report.changed());
    assert_eq!(f.dump(), before);
}

#[test]
fn pass_manager_reports_per_pass_statistics() {
    let ops = vec![
        Opcode::New { dst: Reg(1) },
        Opcode::NullCheck { reg: Reg(1) },
        Opcode::Field {
            dst: Reg(2),
            obj: Reg(1),
            field: RefField(0),
        },
        Opcode::Field {
            dst: Reg(3),
            obj: Reg(1),
            field: RefField(0),
        },
        Opcode::Add {
            dst: Reg(4),
            a: Reg(2),
            b: Reg(3),
        },
        Opcode::Ret { ret: Reg(4) },
    ];
    let mut f = lower(&ops, &[t(5), t(5), t(0), t(0), t(0)]).unwrap();
    let pm = PassManager::new(OptLevel::O2).with_options(PassOptions {
        verify_each: true,
        ..PassOptions::default()
    });
    let report = pm.run(&mut f).unwrap();
    verify(&f).unwrap();
    assert_eq!(
        pm.pass_names(),
        vec!["null-check-elim", "gvn", "licm", "fma", "dce"]
    );
    assert_eq!(report.stats_for("null-check-elim").eliminated, 1);
    assert_eq!(report.stats_for("gvn").eliminated, 1);
    assert_eq!(report.stats_for("fma").fused, 0, "no float types here");
    assert!(report.rounds >= 1 && report.rounds <= pm.options().max_rounds);
    assert!(report.total().eliminated >= 2);
    assert!(report.changed());
}

#[test]
fn pass_manager_accepts_an_explicit_pass_list() {
    let ops = vec![
        Opcode::Int {
            dst: Reg(0),
            ptr: RefInt(0),
        },
        Opcode::Add {
            dst: Reg(1),
            a: Reg(0),
            b: Reg(0),
        },
        Opcode::Ret { ret: Reg(0) },
    ];
    let mut f = lower(&ops, &[t(0), t(0)]).unwrap();
    let pm = PassManager::with_passes(vec![Box::new(DeadCodeElim)]);
    assert_eq!(pm.pass_names(), vec!["dce"]);
    let report = pm.run(&mut f).unwrap();
    assert_eq!(report.stats_for("dce").eliminated, 1);
    assert_eq!(count_instrs(&f, |i| matches!(i, Instr::BinOp { .. })), 0);
}

#[test]
fn pass_manager_runs_to_a_fixed_point() {
    // GVN exposes dead code, DCE removes it, and the next round finds
    // nothing: the manager must stop rather than spin.
    let (ops, tys) = fix_loop_invariant();
    let mut f = lower(&ops, &tys).unwrap();
    let pm = PassManager::new(OptLevel::O2).with_options(PassOptions {
        verify_each: true,
        ..PassOptions::default()
    });
    let report = pm.run(&mut f).unwrap();
    assert!(report.rounds <= pm.options().max_rounds);
    // A second run finds nothing left to do.
    let again = pm.run(&mut f).unwrap();
    assert!(!again.changed(), "pipeline is not at a fixed point");
}

// ---------------------------------------------------------------------------
// optimized round-trip property + semantic equivalence
// ---------------------------------------------------------------------------

fn optimized_round_trip(ops: &[Opcode], tys: &[TypeRef]) -> Serialized {
    let mut f1 = lower(ops, tys).expect("lower");
    verify(&f1).unwrap_or_else(|e| panic!("verify(lowered): {e}\n{}", f1.dump()));
    let pm = PassManager::new(OptLevel::O2).with_options(PassOptions {
        verify_each: true,
        ..PassOptions::default()
    });
    pm.run(&mut f1).expect("passes");
    verify(&f1).unwrap_or_else(|e| panic!("verify(optimized): {e}\n{}", f1.dump()));
    let out = serialize(&f1).expect("serialize");
    assert_eq!(
        &out.reg_types[..tys.len()],
        tys,
        "original register types survive optimization"
    );
    for op in &out.ops {
        assert!(
            !matches!(op, Opcode::Nop | Opcode::IndirectCall { .. }),
            "interpreter-compatible opcodes only, got {:?}",
            op
        );
    }
    let f2 = lower(&out.ops, &out.reg_types)
        .unwrap_or_else(|e| panic!("re-lower: {e}\nops: {}", ops_text(&out.ops)));
    verify(&f2).unwrap_or_else(|e| panic!("verify(re-lowered): {e}\n{}", f2.dump()));
    check_cfg_equivalent(&f1, &f2).unwrap_or_else(|e| {
        panic!(
            "optimized CFG equivalence failed: {e}\nf1:\n{}\nf2:\n{}",
            f1.dump(),
            f2.dump()
        )
    });
    out
}

#[test]
fn optimized_round_trip_over_every_fixture() {
    for (name, (ops, tys)) in [
        ("straight_line", fix_straight_line()),
        ("diamond", fix_diamond()),
        ("loop", fix_loop()),
        ("loop_no_label", fix_loop_no_label()),
        ("loop_invariant", fix_loop_invariant()),
        ("switch", fix_switch()),
        ("trap", fix_trap()),
        ("nested_traps", fix_nested_traps()),
        ("multi_endtrap", fix_multi_endtrap()),
        ("incr", fix_incr()),
        ("ref", fix_ref()),
        ("setenumfield", fix_setenumfield()),
    ] {
        let out = optimized_round_trip(&ops, &tys);
        // Optimizing the optimized output is stable.
        let _ = optimized_round_trip(&out.ops, &out.reg_types);
        assert!(!out.ops.is_empty(), "{} produced no opcodes", name);
    }
}

#[test]
fn passes_preserve_semantics() {
    struct Case {
        name: &'static str,
        ops: Vec<Opcode>,
        tys: Vec<TypeRef>,
        ints: Vec<i32>,
        inputs: Vec<Vec<i64>>,
    }
    let (sl_ops, sl_tys) = fix_straight_line();
    let (d_ops, d_tys) = fix_diamond();
    let (l_ops, l_tys) = fix_loop();
    let (ln_ops, ln_tys) = fix_loop_no_label();
    let (li_ops, li_tys) = fix_loop_invariant();
    let (sw_ops, sw_tys) = fix_switch();
    let (in_ops, in_tys) = fix_incr();
    let cases = vec![
        Case {
            name: "straight_line",
            ops: sl_ops,
            tys: sl_tys,
            ints: vec![7, 35],
            inputs: vec![vec![]],
        },
        Case {
            name: "diamond",
            ops: d_ops,
            tys: d_tys,
            ints: vec![10, 20],
            inputs: vec![vec![0], vec![1]],
        },
        Case {
            name: "loop",
            ops: l_ops,
            tys: l_tys,
            ints: vec![0, 1],
            inputs: vec![vec![0], vec![1], vec![5], vec![10]],
        },
        Case {
            name: "loop_no_label",
            ops: ln_ops,
            tys: ln_tys,
            ints: vec![0, 1],
            inputs: vec![vec![0], vec![3], vec![7]],
        },
        Case {
            name: "loop_invariant",
            ops: li_ops,
            tys: li_tys,
            ints: vec![0, 1],
            inputs: vec![vec![0, 3], vec![4, 5]],
        },
        Case {
            name: "switch",
            ops: sw_ops,
            tys: sw_tys,
            ints: vec![100, 200, 300],
            inputs: vec![vec![0], vec![1], vec![2], vec![5], vec![-1]],
        },
        Case {
            name: "incr",
            ops: in_ops,
            tys: in_tys,
            ints: vec![40],
            inputs: vec![vec![]],
        },
    ];
    for c in cases {
        let out = optimized_round_trip(&c.ops, &c.tys);
        for input in &c.inputs {
            let before = mini_eval(&c.ops, &c.ints, input, c.tys.len());
            let after = mini_eval(&out.ops, &c.ints, input, out.num_regs);
            assert_eq!(
                before,
                after,
                "{}: optimization changed the result for {:?}\n{}",
                c.name,
                input,
                ops_text(&out.ops)
            );
        }
    }
}

// ---------------------------------------------------------------------------
// tail-recursion elimination
// ---------------------------------------------------------------------------

/// `sum(n, acc) = n > 0 ? sum(n - 1, acc + n) : acc`, self-recursive at findex 0.
fn fix_tail_sum() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::Int {
                dst: Reg(2),
                ptr: RefInt(0),
            }, // zero
            Opcode::JSGt {
                a: Reg(0),
                b: Reg(2),
                offset: 1,
            },
            Opcode::Ret { ret: Reg(1) },
            Opcode::Add {
                dst: Reg(1),
                a: Reg(1),
                b: Reg(0),
            }, // acc += n
            Opcode::Int {
                dst: Reg(3),
                ptr: RefInt(1),
            }, // one
            Opcode::Sub {
                dst: Reg(0),
                a: Reg(0),
                b: Reg(3),
            }, // n -= 1
            Opcode::Call2 {
                dst: Reg(4),
                fun: RefFun(0),
                arg0: Reg(0),
                arg1: Reg(1),
            },
            Opcode::Ret { ret: Reg(4) },
        ],
        vec![t(0); 5],
    )
}

/// `swap(a, b, n) = n > 0 ? swap(b, a, n - 1) : a`. The recursive call permutes
/// its arguments, so de-SSA has to break a copy cycle on the back edge.
fn fix_tail_swap() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::Int {
                dst: Reg(3),
                ptr: RefInt(0),
            },
            Opcode::JSGt {
                a: Reg(2),
                b: Reg(3),
                offset: 1,
            },
            Opcode::Ret { ret: Reg(0) },
            Opcode::Int {
                dst: Reg(4),
                ptr: RefInt(1),
            },
            Opcode::Sub {
                dst: Reg(2),
                a: Reg(2),
                b: Reg(4),
            },
            Opcode::Call3 {
                dst: Reg(5),
                fun: RefFun(0),
                arg0: Reg(1),
                arg1: Reg(0),
                arg2: Reg(2),
            },
            Opcode::Ret { ret: Reg(5) },
        ],
        vec![t(0); 6],
    )
}

/// Same recursion, but `acc` is an `Incr` target, so it is a cell.
fn fix_tail_cell_param() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::Int {
                dst: Reg(2),
                ptr: RefInt(0),
            },
            Opcode::JSGt {
                a: Reg(0),
                b: Reg(2),
                offset: 1,
            },
            Opcode::Ret { ret: Reg(1) },
            Opcode::Incr { dst: Reg(1) },
            Opcode::Int {
                dst: Reg(3),
                ptr: RefInt(1),
            },
            Opcode::Sub {
                dst: Reg(0),
                a: Reg(0),
                b: Reg(3),
            },
            Opcode::Call2 {
                dst: Reg(4),
                fun: RefFun(0),
                arg0: Reg(0),
                arg1: Reg(1),
            },
            Opcode::Ret { ret: Reg(4) },
        ],
        vec![t(0); 5],
    )
}

/// The recursive call sits inside a `try`, so the handler has to stay live
/// while the callee runs: it is not in tail position.
fn fix_tail_in_trap() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::Trap {
                exc: Reg(1),
                offset: 3,
            }, // handler at op 4
            Opcode::Call1 {
                dst: Reg(2),
                fun: RefFun(0),
                arg0: Reg(0),
            },
            Opcode::EndTrap { exc: Reg(1) },
            Opcode::Ret { ret: Reg(2) },
            Opcode::Ret { ret: Reg(0) }, // handler
        ],
        vec![t(0), t(2), t(0)],
    )
}

fn tre_stats(ops: &[Opcode], tys: &[TypeRef], findex: Option<usize>) -> (Function, PassStats) {
    let mut f = lower(ops, tys).expect("lower");
    if let Some(fx) = findex {
        f.findex = Some(fx);
    }
    let stats = run_pass(&mut f, &TailRecursionElim, PassOptions::default());
    verify(&f).unwrap_or_else(|e| panic!("verify after tre: {e}\n{}", f.dump()));
    (f, stats)
}

fn count_calls(f: &Function, findex: usize) -> usize {
    count_instrs(
        f,
        |i| matches!(i, Instr::Call { fun, .. } if *fun == findex),
    )
}

#[test]
fn tre_turns_a_self_tail_call_into_a_back_edge() {
    let (ops, tys) = fix_tail_sum();
    let (f, stats) = tre_stats(&ops, &tys, Some(0));
    assert_eq!(stats.tail_calls, 1, "{}", f.dump());
    assert_eq!(
        count_calls(&f, 0),
        0,
        "the recursive call is gone\n{}",
        f.dump()
    );
    // The header carries one phi per argument register, each on its own
    // register, and the entry still defines the parameters.
    let header = f
        .blocks
        .iter()
        .find(|b| b.phis.len() == 2)
        .unwrap_or_else(|| panic!("no loop header with two phis\n{}", f.dump()));
    let mut regs: Vec<u32> = header.phis.iter().map(|p| f.value_reg(p.dst)).collect();
    regs.sort_unstable();
    assert_eq!(regs, vec![0, 1]);
    // A back edge exists: some block now jumps to the header.
    let hid = f
        .blocks
        .iter()
        .position(|b| std::ptr::eq(b, header))
        .expect("header is one of the blocks");
    let preds = f.preds();
    assert!(
        preds[hid].len() >= 2,
        "header should be entered from the entry and the back edge\n{}",
        f.dump()
    );
}

#[test]
fn tre_is_inert_without_a_function_identity() {
    let (ops, tys) = fix_tail_sum();
    let (f, stats) = tre_stats(&ops, &tys, None);
    assert_eq!(stats, PassStats::default());
    assert_eq!(count_calls(&f, 0), 1);
}

#[test]
fn tre_refuses_a_call_to_another_function() {
    let (ops, tys) = fix_tail_sum();
    // The same body, but this function is findex 9: the call is not recursive.
    let (f, stats) = tre_stats(&ops, &tys, Some(9));
    assert_eq!(stats, PassStats::default());
    assert_eq!(count_calls(&f, 0), 1);
}

#[test]
fn tre_refuses_a_cell_parameter() {
    let (ops, tys) = fix_tail_cell_param();
    let (f, stats) = tre_stats(&ops, &tys, Some(0));
    assert!(
        f.cells.iter().any(|c| c.reg == 1),
        "r1 must be pinned for this to be the negative control it claims to be"
    );
    assert_eq!(stats, PassStats::default(), "{}", f.dump());
    assert_eq!(count_calls(&f, 0), 1);
}

#[test]
fn tre_refuses_a_call_inside_a_trap_region() {
    let (ops, tys) = fix_tail_in_trap();
    let (f, stats) = tre_stats(&ops, &tys, Some(0));
    assert!(
        f.blocks.iter().any(|b| b.handler.is_some()),
        "the fixture must actually open a trap region"
    );
    assert_eq!(stats, PassStats::default(), "{}", f.dump());
    assert_eq!(count_calls(&f, 0), 1);
}

#[test]
fn tre_fires_only_on_the_self_recursive_fixtures() {
    let corpus: Vec<(&str, (Vec<Opcode>, Vec<TypeRef>))> = vec![
        ("straight_line", fix_straight_line()),
        ("diamond", fix_diamond()),
        ("loop", fix_loop()),
        ("loop_no_label", fix_loop_no_label()),
        ("loop_invariant", fix_loop_invariant()),
        ("switch", fix_switch()),
        ("trap", fix_trap()),
        ("nested_traps", fix_nested_traps()),
        ("multi_endtrap", fix_multi_endtrap()),
        ("incr", fix_incr()),
        ("ref", fix_ref()),
        ("setenumfield", fix_setenumfield()),
        ("tail_sum", fix_tail_sum()),
        ("tail_swap", fix_tail_swap()),
        ("tail_cell_param", fix_tail_cell_param()),
        ("tail_in_trap", fix_tail_in_trap()),
    ];
    let mut fired: Vec<&str> = Vec::new();
    for (name, (ops, tys)) in &corpus {
        let (_, stats) = tre_stats(ops, tys, Some(0));
        if stats.tail_calls > 0 {
            fired.push(name);
        }
    }
    assert_eq!(
        fired,
        vec!["tail_sum", "tail_swap"],
        "tail-recursion elimination fired on an unexpected set of fixtures"
    );
}

#[test]
fn tre_preserves_semantics_and_removes_the_recursion() {
    for (name, (ops, tys), ints, inputs) in [
        (
            "tail_sum",
            fix_tail_sum(),
            vec![0, 1],
            vec![vec![0i64, 0], vec![1, 0], vec![5, 100], vec![9, -3]],
        ),
        (
            "tail_swap",
            fix_tail_swap(),
            vec![0, 1],
            vec![
                vec![7i64, 11, 0],
                vec![7, 11, 1],
                vec![7, 11, 2],
                vec![7, 11, 5],
            ],
        ),
    ] {
        let mut f = lower(&ops, &tys).expect("lower");
        f.findex = Some(0);
        let stats = run_pass(&mut f, &TailRecursionElim, PassOptions::default());
        assert_eq!(stats.tail_calls, 1, "{}: {}", name, f.dump());
        verify(&f).unwrap_or_else(|e| panic!("{name}: verify: {e}\n{}", f.dump()));
        let out = serialize(&f).expect("serialize");
        assert!(
            !out.ops.iter().any(|o| matches!(
                o,
                Opcode::Call0 { .. }
                    | Opcode::Call1 { .. }
                    | Opcode::Call2 { .. }
                    | Opcode::Call3 { .. }
                    | Opcode::Call4 { .. }
                    | Opcode::CallN { .. }
            )),
            "{}: the serialized output still calls\n{}",
            name,
            ops_text(&out.ops)
        );
        // Re-lowering the output must still be valid IR.
        let f2 = lower(&out.ops, &out.reg_types)
            .unwrap_or_else(|e| panic!("{name}: re-lower: {e}\n{}", ops_text(&out.ops)));
        verify(&f2).unwrap();

        let m = MiniModule::new(&ints).with_fun(0, &ops, tys.len());
        for input in &inputs {
            let before = m.call(0, input, &mut 100_000);
            let after = mini_eval(&out.ops, &ints, input, out.num_regs);
            assert_eq!(
                before,
                after,
                "{}: TRE changed the result for {:?}\n{}",
                name,
                input,
                ops_text(&out.ops)
            );
        }
    }
}

// ---------------------------------------------------------------------------
// inlining
// ---------------------------------------------------------------------------

/// `add(a, b) = a + b` — findex 7.
fn fix_callee_add() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::Add {
                dst: Reg(2),
                a: Reg(0),
                b: Reg(1),
            },
            Opcode::Ret { ret: Reg(2) },
        ],
        vec![t(0); 3],
    )
}

/// `poly(a, b) = (a + b) * a - b` — three instructions, for budget tests.
fn fix_callee_poly() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::Add {
                dst: Reg(2),
                a: Reg(0),
                b: Reg(1),
            },
            Opcode::Mul {
                dst: Reg(2),
                a: Reg(2),
                b: Reg(0),
            },
            Opcode::Sub {
                dst: Reg(2),
                a: Reg(2),
                b: Reg(1),
            },
            Opcode::Ret { ret: Reg(2) },
        ],
        vec![t(0); 3],
    )
}

/// `max(a, b)` — two `Ret`s, so the continuation needs a phi.
fn fix_callee_max() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::JSGt {
                a: Reg(0),
                b: Reg(1),
                offset: 1,
            },
            Opcode::Ret { ret: Reg(1) },
            Opcode::Ret { ret: Reg(0) },
        ],
        vec![t(0); 2],
    )
}

/// A callee that opens a trap region, so lowering pins its exception register
/// to a cell.
fn fix_callee_trap() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::Trap {
                exc: Reg(1),
                offset: 2,
            },
            Opcode::NullCheck { reg: Reg(0) },
            Opcode::EndTrap { exc: Reg(1) },
            Opcode::Ret { ret: Reg(0) },
        ],
        vec![t(0), t(2)],
    )
}

/// `outer(a) = inner(a) + a` — findex 7, calls findex 8.
fn fix_callee_outer() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::Call1 {
                dst: Reg(1),
                fun: RefFun(8),
                arg0: Reg(0),
            },
            Opcode::Add {
                dst: Reg(2),
                a: Reg(1),
                b: Reg(0),
            },
            Opcode::Ret { ret: Reg(2) },
        ],
        vec![t(0); 3],
    )
}

/// `inner(a) = a + a` — findex 8.
fn fix_callee_inner() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::Add {
                dst: Reg(1),
                a: Reg(0),
                b: Reg(0),
            },
            Opcode::Ret { ret: Reg(1) },
        ],
        vec![t(0); 2],
    )
}

/// `caller(a) = f7(a, 10)`.
fn fix_caller_call2() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::Int {
                dst: Reg(1),
                ptr: RefInt(0),
            },
            Opcode::Call2 {
                dst: Reg(2),
                fun: RefFun(7),
                arg0: Reg(0),
                arg1: Reg(1),
            },
            Opcode::Ret { ret: Reg(2) },
        ],
        vec![t(0); 3],
    )
}

/// `caller(a) = f7(a)`, one argument.
fn fix_caller_call1() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::Call1 {
                dst: Reg(1),
                fun: RefFun(7),
                arg0: Reg(0),
            },
            Opcode::Ret { ret: Reg(1) },
        ],
        vec![t(0); 2],
    )
}

/// The call sits inside a `try`.
fn fix_caller_call_in_trap() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::Trap {
                exc: Reg(1),
                offset: 3,
            }, // handler at op 4
            Opcode::Call2 {
                dst: Reg(2),
                fun: RefFun(7),
                arg0: Reg(0),
                arg1: Reg(0),
            },
            Opcode::EndTrap { exc: Reg(1) },
            Opcode::Ret { ret: Reg(2) },
            Opcode::Ret { ret: Reg(0) }, // handler
        ],
        vec![t(0), t(2), t(0)],
    )
}

fn bodies(entries: &[(usize, (Vec<Opcode>, Vec<TypeRef>))]) -> ModuleTables {
    let mut m = ModuleTables::new();
    for (findex, (ops, reg_types)) in entries {
        m = m.with_callee(
            *findex,
            CalleeBody::Bytecode {
                ops: ops.clone(),
                reg_types: reg_types.clone(),
            },
        );
    }
    m
}

fn any_call(f: &Function) -> usize {
    count_instrs(f, |i| matches!(i, Instr::Call { .. }))
}

#[test]
fn inline_replaces_a_direct_call_with_the_callee_body() {
    let (ops, tys) = fix_caller_call2();
    let info = bodies(&[(7, fix_callee_add())]);
    let mut f = lower(&ops, &tys).expect("lower");
    let stats = run_pass(&mut f, &Inlining::new(&info), PassOptions::default());
    assert_eq!(stats.inlined, 1, "{}", f.dump());
    assert!(stats.added > 0);
    assert_eq!(any_call(&f), 0, "the call must be gone\n{}", f.dump());
    assert_eq!(
        count_instrs(&f, |i| matches!(i, Instr::BinOp { op: BinOp::Add, .. })),
        1,
        "the callee's arithmetic is now the caller's\n{}",
        f.dump()
    );
}

#[test]
fn inline_is_inert_without_module_info() {
    let (ops, tys) = fix_caller_call2();
    let mut f = lower(&ops, &tys).expect("lower");
    let stats = run_pass(
        &mut f,
        &Inlining::new(&NoModuleInfo),
        PassOptions::default(),
    );
    assert_eq!(stats, PassStats::default());
    assert_eq!(any_call(&f), 1);
}

#[test]
fn inline_builds_a_phi_for_a_callee_with_several_returns() {
    let (ops, tys) = fix_caller_call2();
    let info = bodies(&[(7, fix_callee_max())]);
    let mut f = lower(&ops, &tys).expect("lower");
    let stats = run_pass(&mut f, &Inlining::new(&info), PassOptions::default());
    assert_eq!(stats.inlined, 1, "{}", f.dump());
    assert_eq!(any_call(&f), 0);
    let phis: usize = f.blocks.iter().map(|b| b.phis.len()).sum();
    assert_eq!(
        phis,
        1,
        "the continuation merges the two returns\n{}",
        f.dump()
    );
}

#[test]
fn inline_refuses_a_callee_past_the_budget() {
    let (ops, tys) = fix_caller_call2();
    let info = bodies(&[(7, fix_callee_poly())]);
    for (budget, expected) in [(2usize, 0usize), (3, 1)] {
        let mut f = lower(&ops, &tys).expect("lower");
        let stats = run_pass(
            &mut f,
            &Inlining::new(&info),
            PassOptions {
                inline_max_callee: budget,
                ..PassOptions::default()
            },
        );
        assert_eq!(
            stats.inlined,
            expected,
            "budget {} should give {} inlines\n{}",
            budget,
            expected,
            f.dump()
        );
    }
}

#[test]
fn inline_refuses_to_grow_the_caller_past_its_ceiling() {
    let (ops, tys) = fix_caller_call2();
    let info = bodies(&[(7, fix_callee_poly())]);
    let mut f = lower(&ops, &tys).expect("lower");
    let stats = run_pass(
        &mut f,
        &Inlining::new(&info),
        PassOptions {
            inline_max_function: 2,
            ..PassOptions::default()
        },
    );
    assert_eq!(stats.inlined, 0, "{}", f.dump());
    assert_eq!(any_call(&f), 1);
}

#[test]
fn inline_refuses_a_callee_whose_trap_regions_cannot_be_preserved() {
    let (ops, tys) = fix_caller_call1();
    let callee = fix_callee_trap();
    // The fixture must really contain a trap region.
    let g = lower(&callee.0, &callee.1).expect("lower callee");
    assert!(
        !g.cells.is_empty() && g.blocks.iter().any(|b| b.handler.is_some()),
        "the callee fixture must open a trap region"
    );
    let info = bodies(&[(7, callee)]);
    let mut f = lower(&ops, &tys).expect("lower");
    let stats = run_pass(&mut f, &Inlining::new(&info), PassOptions::default());
    assert_eq!(stats, PassStats::default(), "{}", f.dump());
    assert_eq!(any_call(&f), 1);
}

#[test]
fn inline_refuses_a_call_site_inside_a_trap_region() {
    let (ops, tys) = fix_caller_call_in_trap();
    let info = bodies(&[(7, fix_callee_add())]);
    let mut f = lower(&ops, &tys).expect("lower");
    assert!(
        f.blocks.iter().any(|b| b.handler.is_some()),
        "the caller fixture must open a trap region"
    );
    let stats = run_pass(&mut f, &Inlining::new(&info), PassOptions::default());
    assert_eq!(stats, PassStats::default(), "{}", f.dump());
    assert_eq!(any_call(&f), 1);
}

#[test]
fn inline_caps_recursive_nesting_at_the_depth_budget() {
    let (ops, tys) = fix_caller_call1();
    let info = bodies(&[(7, fix_callee_outer()), (8, fix_callee_inner())]);
    for (depth, expected_inlines, expected_calls) in [(1usize, 1usize, 1usize), (2, 2, 0)] {
        let mut f = lower(&ops, &tys).expect("lower");
        let stats = run_pass(
            &mut f,
            &Inlining::new(&info),
            PassOptions {
                inline_max_depth: depth,
                ..PassOptions::default()
            },
        );
        assert_eq!(
            (stats.inlined, any_call(&f)),
            (expected_inlines, expected_calls),
            "depth {}\n{}",
            depth,
            f.dump()
        );
    }
}

#[test]
fn inline_merges_the_callee_native_declarations() {
    let (ops, tys) = fix_caller_call1();
    // The callee calls a native, so inlining it makes the caller reference it.
    let callee = (
        vec![
            Opcode::Call1 {
                dst: Reg(1),
                fun: RefFun(3),
                arg0: Reg(0),
            },
            Opcode::Ret { ret: Reg(1) },
        ],
        vec![t(0); 2],
    );
    let mut nt = NativeTable::new();
    nt.declare(NativeImport::new(3, "std", "abs", vec![t(0)], t(0)))
        .unwrap();
    let info = bodies(&[(7, callee)]).with_natives(nt);
    let mut f = lower(&ops, &tys).expect("lower");
    assert!(f.natives.is_empty());
    run_pass(&mut f, &Inlining::new(&info), PassOptions::default());
    assert_eq!(
        f.natives.get(3).map(|i| i.symbol()),
        Some("std@abs".to_string()),
        "the caller now declares what the callee called\n{}",
        f.dump()
    );
}

#[test]
fn inline_preserves_semantics() {
    let cases: Vec<(
        &str,
        (Vec<Opcode>, Vec<TypeRef>),
        Vec<(usize, (Vec<Opcode>, Vec<TypeRef>))>,
    )> = vec![
        ("add", fix_caller_call2(), vec![(7usize, fix_callee_add())]),
        ("max", fix_caller_call2(), vec![(7, fix_callee_max())]),
        ("poly", fix_caller_call2(), vec![(7, fix_callee_poly())]),
        (
            "nested",
            fix_caller_call1(),
            vec![(7, fix_callee_outer()), (8, fix_callee_inner())],
        ),
    ];
    let ints = vec![10, 1];
    for (name, (ops, tys), callees) in cases {
        let info = bodies(&callees);
        let mut f = lower(&ops, &tys).expect("lower");
        let stats = run_pass(&mut f, &Inlining::new(&info), PassOptions::default());
        assert!(stats.inlined > 0, "{}: nothing inlined\n{}", name, f.dump());
        let out = serialize(&f).expect("serialize");
        let f2 = lower(&out.ops, &out.reg_types)
            .unwrap_or_else(|e| panic!("{name}: re-lower: {e}\n{}", ops_text(&out.ops)));
        verify(&f2).unwrap();

        let mut m = MiniModule::new(&ints);
        for (findex, (cops, ctys)) in &callees {
            m = m.with_fun(*findex, cops, ctys.len());
        }
        for input in [vec![0i64], vec![3], vec![-4], vec![25]] {
            let before = mini_eval_in(&m, &ops, &input, tys.len());
            let after = mini_eval_in(&m, &out.ops, &input, out.num_regs);
            assert_eq!(
                before,
                after,
                "{}: inlining changed the result for {:?}\n{}",
                name,
                input,
                ops_text(&out.ops)
            );
        }
    }
}
