//! AIR v2 test suite: lowering shape tests, verifier negative tests, and the
//! round-trip property over a fixture corpus.
//!
//! Round-trip property: for every fixture, `serialize(lower(ops))` must
//! produce a valid executable opcode sequence with identical structural
//! semantics — proven by re-lowering the output, verifying it, and checking
//! condensed-CFG isomorphism + dominance equivalence against the original,
//! plus (where the mini interpreter covers the ops) identical input/output
//! behavior, plus register-type-table preservation.

// Test-local closures and expectation tables use big ad-hoc tuple types.
#![allow(clippy::type_complexity)]

use super::analysis::{read_class, write_class, AliasClass, CfgInfo, LoopForest};
use super::ir::*;
use super::lower::{lower, lower_with, ModuleBuilder};
use super::module::{
    CalleeBody, ModuleInfo, ModuleTables, NativeImport, NativeTable, NoModuleInfo,
};
use super::passes::{
    DeadCodeElim, FmaPeephole, GlobalValueNumbering, Inlining, LoopInvariantCodeMotion,
    NullCheckElim, OptLevel, Pass, PassManager, PassOptions, PassStats, ScalarReplacement,
    TailRecursionElim,
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
    // `(catch pc, exception register)` per open region, innermost last.
    let mut traps: Vec<(usize, usize)> = Vec::new();
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
            // The widener's vector trip count is `(limit - start) & ~(VF-1)`,
            // and its remainder is entered from that.
            Opcode::And { dst, a, b } => {
                regs[dst.0 as usize] = regs[a.0 as usize] & regs[b.0 as usize]
            }
            Opcode::Or { dst, a, b } => {
                regs[dst.0 as usize] = regs[a.0 as usize] | regs[b.0 as usize]
            }
            Opcode::Xor { dst, a, b } => {
                regs[dst.0 as usize] = regs[a.0 as usize] ^ regs[b.0 as usize]
            }
            Opcode::Shl { dst, a, b } => {
                regs[dst.0 as usize] = regs[a.0 as usize] << regs[b.0 as usize]
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
            // A register's address is just its index here, since the frame is a
            // flat array. Enough to model pinning faithfully, which is what the
            // cell tests need — Incr no longer pins on its own.
            Opcode::Ref { dst, src } => regs[dst.0 as usize] = src.0 as i64,
            Opcode::Setref { dst, value } => {
                let slot = regs[dst.0 as usize] as usize;
                regs[slot] = regs[value.0 as usize];
            }
            Opcode::Unref { dst, src } => {
                let slot = regs[src.0 as usize] as usize;
                regs[dst.0 as usize] = regs[slot];
            }
            Opcode::Trap { exc, offset } => traps.push((jump(*offset), exc.0 as usize)),
            // Only pops the region. The operand is Haxe's `OEndTrap of bool`,
            // a flag rather than a register, so writing through it here would
            // clobber whichever local the flag's value collides with — the
            // defect this models.
            Opcode::EndTrap { .. } => {
                traps.pop().expect("mini_eval: EndTrap with no open region");
            }
            Opcode::Throw { exc } | Opcode::Rethrow { exc } => {
                let v = regs[exc.0 as usize];
                let (catch_pc, exc_reg) = traps
                    .pop()
                    .expect("mini_eval: uncaught throw (no open trap region)");
                regs[exc_reg] = v;
                pc = catch_pc;
                continue;
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
            // Both EndTraps carry the flag genhl emits, not an exception
            // register: which region each one closes is fixed by the trap
            // stack, so the inner and outer operands are identical.
            Opcode::EndTrap { exc: Reg(1) },
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
fn lower_incr_stays_ssa_when_nothing_else_pins() {
    let (ops, tys) = fix_incr();
    let f = lower(&ops, &tys).unwrap();
    verify(&f).unwrap();
    // Incr reads and writes one register, which is an ordinary SSA def. It
    // used to pin, which made every counted loop's induction variable a memory
    // cell carrying a CellGet and a CellIncr per iteration that no pass could
    // reason about.
    assert!(f.cells.is_empty(), "Incr alone must not pin: {:?}", f.cells);
    let incrs = f
        .blocks
        .iter()
        .flat_map(|b| b.instrs.iter())
        .filter(|i| matches!(i, Instr::UnOp { op: UnOp::Incr, .. }))
        .count();
    assert_eq!(incrs, 2);
    assert!(f
        .blocks
        .iter()
        .flat_map(|b| b.instrs.iter())
        .all(|i| !matches!(i, Instr::CellIncr { .. })));
}

/// A counter whose address is taken is still a cell, and still uses the fused
/// read-modify-write rather than a CellGet/UnOp/CellSet triple.
#[test]
fn lower_incr_on_a_ref_taken_register_keeps_the_fused_cell_op() {
    let (mut ops, tys) = fix_incr();
    // Take the register's address after the increments.
    ops.insert(
        3,
        Opcode::Ref {
            dst: Reg(0),
            src: Reg(0),
        },
    );
    let f = lower(&ops, &tys).unwrap();
    verify(&f).unwrap();
    assert_eq!(f.cells.len(), 1);
    assert_eq!(f.cells[0].reason, PinReason::RefTaken);
    let incrs = f
        .blocks
        .iter()
        .flat_map(|b| b.instrs.iter())
        .filter(|i| matches!(i, Instr::CellIncr { .. }))
        .count();
    assert_eq!(incrs, 2, "pinned Incr must stay a fused CellIncr");
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
            Instr::EndTrap {
                cell: CellId(0),
                flag: true,
            },
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
fn endtrap_closes_the_region_the_trap_stack_names_not_its_operand() {
    // `OEndTrap`'s operand is Haxe's `OEndTrap of bool` — HashLink's jit.c
    // never reads it, it just pops `trap_current`. Lowering must therefore
    // take the region being closed from the trap stack. Reading it as a
    // register instead made lowering fail outright on functions where the
    // operand named an unpinned register, and mis-pair the regions where it
    // happened to name a pinned one.
    let (ops, tys) = fix_nested_traps();
    let f = lower(&ops, &tys).unwrap();
    verify(&f).unwrap();

    // The inner region's exception register is r2, the outer's is r1. The
    // first EndTrap closes the inner one even though its operand says `1`.
    let cell_reg = |c: CellId| f.cells[c.idx()].reg;
    let closed: Vec<u32> = f
        .blocks
        .iter()
        .flat_map(|b| b.instrs.iter())
        .filter_map(|i| match i {
            Instr::EndTrap { cell, .. } => Some(cell_reg(*cell)),
            _ => None,
        })
        .collect();
    assert_eq!(
        closed,
        vec![2, 1],
        "inner region (r2) closes first, then the outer (r1):\n{}",
        f.dump()
    );

    // The flag survives serialization unchanged, so a round trip is
    // byte-exact against the genhl input. Substituting the exception register
    // would also re-arm the interpreter defect this operand once triggered:
    // see `endtrap_must_not_write_through_its_flag_operand`.
    let out = serialize(&f).unwrap();
    let flags: Vec<u32> = out
        .ops
        .iter()
        .filter_map(|o| match o {
            Opcode::EndTrap { exc } => Some(exc.0),
            _ => None,
        })
        .collect();
    assert_eq!(flags, vec![1, 1], "both EndTraps re-emit the flag verbatim");
}

#[test]
fn endtrap_must_not_write_through_its_flag_operand() {
    // `ash_interp` used to execute `OEndTrap` as "pop the region, then null
    // `registers[exc]`". Because that operand is Haxe's `OEndTrap of bool` it
    // is 0 or 1, so the clear landed on r0/r1 — whichever local happened to
    // live there. A try that assigned a value and then exited *normally* lost
    // it: `var acc = "s"; try { acc += "|a"; } catch (e) {}` printed `null`.
    //
    // The shape below is that bug reduced to arithmetic. r1 holds a running
    // value across a trap region that never throws, and `EndTrap`'s flag
    // operand is `1` — pointing straight at it.
    let ops = vec![
        Opcode::Int {
            dst: Reg(1),
            ptr: RefInt(0),
        },
        Opcode::Trap {
            exc: Reg(2),
            offset: 2,
        },
        Opcode::Int {
            dst: Reg(3),
            ptr: RefInt(1),
        },
        // Flag operand `1` collides with the live accumulator in r1.
        Opcode::EndTrap { exc: Reg(1) },
        Opcode::Add {
            dst: Reg(0),
            a: Reg(1),
            b: Reg(3),
        },
        Opcode::Ret { ret: Reg(0) },
    ];
    let tys = vec![t(0), t(0), t(2), t(0)];
    let ints = [40, 2];

    // 40 + 2. Nulling r1 at EndTrap would yield 2.
    assert_eq!(
        mini_eval(&ops, &ints, &[], 4),
        42,
        "EndTrap cleared the live register its flag operand collided with"
    );

    // And the value survives the round trip, so no backend can reintroduce it
    // by rewriting the operand into a real register.
    let out = assert_exact(&ops, &tys);
    assert_eq!(mini_eval(&out.ops, &ints, &[], out.num_regs), 42);
}

#[test]
fn endtrap_lowers_when_its_operand_names_an_unpinned_register() {
    // The shape that made lowering bail with "r1 expected to be pinned":
    // a function whose only trap pins r2, with `EndTrap`'s flag operand
    // pointing at r1, which nothing pins.
    let ops = vec![
        Opcode::Trap {
            exc: Reg(2),
            offset: 2,
        },
        Opcode::NullCheck { reg: Reg(3) },
        Opcode::EndTrap { exc: Reg(1) },
        Opcode::Ret { ret: Reg(0) },
    ];
    let tys = vec![t(0), t(0), t(2), t(2)];
    let f = lower(&ops, &tys).unwrap();
    verify(&f).unwrap();
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
fn mov_between_different_types_lowers_to_an_unsafe_cast() {
    // Haxe emits `OMov` across reference types (HOBJ -> HDYN and friends);
    // every mismatched Mov in the ash corpus is one of those. HL treats OMov
    // as a raw register move — hashlink's jit.c runs it through the same arm
    // as OUnsafeCast — so the honest model is a reinterpreting cast. Calling
    // it a Copy asserts src and dst share a type, which the verifier rejects
    // and which would let copy propagation hand every use of dst a value of
    // the wrong type.
    let ops = vec![
        Opcode::Mov {
            dst: Reg(1),
            src: Reg(0),
        },
        Opcode::Ret { ret: Reg(1) },
    ];
    // r0 and r1 are different types; r2 shares r0's.
    let tys = vec![t(5), t(9), t(5)];
    let f = lower(&ops, &tys).unwrap();
    verify(&f).unwrap();
    assert_eq!(
        count_instrs(&f, |i| matches!(
            i,
            Instr::Cast {
                kind: CastKind::UnsafeCast,
                ..
            }
        )),
        1,
        "the type-changing Mov became a cast:\n{}",
        f.dump()
    );
    assert_eq!(
        count_instrs(&f, |i| matches!(i, Instr::Copy { .. })),
        0,
        "and is not also a copy:\n{}",
        f.dump()
    );
    let out = serialize(&f).unwrap();
    assert_eq!(
        ops_text(&out.ops),
        ops_text(&[
            Opcode::UnsafeCast {
                dst: Reg(1),
                src: Reg(0)
            },
            Opcode::Ret { ret: Reg(1) },
        ]),
        "serializes as the cast HL already treats it as"
    );

    // A same-type Mov is still a plain Copy, and still round-trips verbatim.
    let same = vec![
        Opcode::Mov {
            dst: Reg(2),
            src: Reg(0),
        },
        Opcode::Ret { ret: Reg(2) },
    ];
    let g = lower(&same, &tys).unwrap();
    assert_eq!(
        count_instrs(&g, |i| matches!(i, Instr::Copy { .. })),
        1,
        "same-type Mov stays a copy:\n{}",
        g.dump()
    );
    assert_exact(&same, &tys);
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
        instrs: vec![Instr::EndTrap {
            cell: CellId(0),
            flag: true,
        }],
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
    let mut f = float_fn(&ops, &[t(1); 5]);
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
    let mut f = float_fn(&ops, &[t(1); 5]);
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
    let mut f = float_fn(&ops, &[t(1); 5]);
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
    let mut f = float_fn(&ops, &[t(1); 6]);
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
    let mut f = float_fn(&ops, &[t(1); 5]);
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
    let mut f = float_fn(&ops, &[t(1); 5]);
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
        let mut f = float_fn(&sub_ops, &[t(1); 5]);
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
    let mut f = float_fn(&ops, &[t(1); 7]);
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
fn gvn_does_not_leak_a_binding_into_a_sibling_subtree() {
    // A diamond. The dominating load is in the head; the *left* arm clobbers
    // the field and reloads, so its reload cannot reuse the head's value —
    // but it does become the better candidate for the rest of the left arm,
    // so it takes over the table entry. The right arm is a sibling in the
    // dominator tree and the left arm's value does not reach it: it must fall
    // back to the head's load, never the left arm's.
    let ops = vec![
        Opcode::Field {
            dst: Reg(2),
            obj: Reg(1),
            field: RefField(0),
        }, // 0: head, dominates both arms
        Opcode::JSLt {
            a: Reg(3),
            b: Reg(0),
            offset: 3,
        }, // 1 -> 5 (right arm); falls through to the left arm
        Opcode::SetField {
            obj: Reg(1),
            field: RefField(0),
            src: Reg(3),
        }, // 2: left arm clobbers
        Opcode::Field {
            dst: Reg(4),
            obj: Reg(1),
            field: RefField(0),
        }, // 3: left reload, cannot reuse op 0
        Opcode::Ret { ret: Reg(4) }, // 4
        Opcode::Field {
            dst: Reg(5),
            obj: Reg(1),
            field: RefField(0),
        }, // 5: right arm, redundant with op 0 only
        Opcode::Ret { ret: Reg(5) }, // 6
    ];
    let tys = vec![t(0), t(5), t(0), t(0), t(0), t(0)];
    let mut f = lower(&ops, &tys).unwrap();
    // run_pass verifies, which is the assertion that matters: rewriting the
    // right arm's load to the left arm's value is a dominance violation.
    let stats = run_pass(&mut f, &GlobalValueNumbering, PassOptions::default());
    assert_eq!(
        stats.eliminated,
        1,
        "only the right arm's load is redundant:\n{}",
        f.dump()
    );
    assert_eq!(
        count_instrs(&f, |i| matches!(i, Instr::FieldGet { .. })),
        2,
        "the head's load and the left arm's reload both survive:\n{}",
        f.dump()
    );
    serialize(&f).unwrap();
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
    // r0's address is taken, which is what makes it a cell. Incr alone no
    // longer pins — it is an ordinary SSA def — so the pin has to be stated.
    let mut ops = ops;
    ops.insert(
        0,
        Opcode::Ref {
            dst: Reg(4),
            src: Reg(0),
        },
    );
    let tys = vec![t(0); 5];
    let base_ops = ops.clone();
    let mut f = lower(&ops, &tys).unwrap();
    run_pass(&mut f, &GlobalValueNumbering, PassOptions::default());
    assert_eq!(
        count_instrs(&f, |i| matches!(i, Instr::CellGet { .. })),
        2,
        "cells are memory: a write between two reads blocks reuse"
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
        vec!["cellfwd", "celldse", "null-check-elim", "gvn", "licm", "fma", "dce"]
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
    optimized_round_trip_at(OptLevel::O2, &NoModuleInfo, None, ops, tys)
}

/// The round-trip property at an arbitrary opt level, with the module info the
/// inliner reads and the function identity tail-recursion elimination needs.
fn optimized_round_trip_at(
    level: OptLevel,
    info: &dyn super::module::ModuleInfo,
    findex: Option<usize>,
    ops: &[Opcode],
    tys: &[TypeRef],
) -> Serialized {
    let mut f1 = lower_with(ops, tys, info).expect("lower");
    f1.findex = findex;
    verify(&f1).unwrap_or_else(|e| panic!("verify(lowered): {e}\n{}", f1.dump()));
    let pm = PassManager::with_module(level, info).with_options(PassOptions {
        verify_each: true,
        ..PassOptions::default()
    });
    let report = pm.run(&mut f1).expect("passes");
    assert!(
        report.rounds <= pm.options().max_rounds,
        "the pipeline did not reach a fixed point within its round cap"
    );
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
            // r1 must be a cell for this negative control to mean anything,
            // and Incr no longer pins on its own.
            Opcode::Ref {
                dst: Reg(4),
                src: Reg(1),
            },
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

/// The fib shape: `fib(n) = n < 2 ? n : fib(n-1) + fib(n-2)`, as a direct
/// self-recursive body. Registers: r0 = n (arg), r1 = temp, r2 = temp.
fn fix_fib(findex: usize) -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::Int {
                dst: Reg(1),
                ptr: RefInt(0),
            },
            Opcode::JSLt {
                a: Reg(0),
                b: Reg(1),
                offset: 6,
            },
            Opcode::Int {
                dst: Reg(1),
                ptr: RefInt(0),
            },
            Opcode::Sub {
                dst: Reg(1),
                a: Reg(0),
                b: Reg(1),
            },
            Opcode::Call1 {
                dst: Reg(2),
                fun: RefFun(findex),
                arg0: Reg(1),
            },
            Opcode::Call1 {
                dst: Reg(1),
                fun: RefFun(findex),
                arg0: Reg(1),
            },
            Opcode::Add {
                dst: Reg(2),
                a: Reg(2),
                b: Reg(1),
            },
            Opcode::Ret { ret: Reg(2) },
            Opcode::Ret { ret: Reg(0) }, // base case: n < 2 returns n
        ],
        vec![t(0), t(0), t(0)],
    )
}

/// A DIRECT self-call is expanded — that is deliberate (it lowers the
/// recurrence base; GCC -O2 does the same) — but ONLY under its own
/// budget, held across manager rounds. Before the budget existed, the depth
/// vector reset every round and fib's 11-instruction body compounded to 319:
/// an optimizer whose output was 29x its input. The invariant pinned here is
/// the user-facing one: optimized output stays within [`growth_cap`] of its
/// input, no matter how many rounds run.
#[test]
fn inline_bounds_direct_self_recursion_across_rounds() {
    let (ops, tys) = fix_fib(9);
    let info = bodies(&[(9, fix_fib(9))]);
    let mut f = lower(&ops, &tys).expect("lower").with_findex(9);
    let original: usize = f
        .blocks
        .iter()
        .flat_map(|b| b.instrs.iter())
        .filter(|i| !matches!(i, Instr::Param { .. }))
        .count();

    let inliner = Inlining::new(&info);
    // Many more rounds than the manager would ever run: the budget must hold
    // regardless, because it is the only bound that survives the reset of the
    // per-run depth vector.
    let mut total_inlined = 0;
    for _ in 0..12 {
        total_inlined += run_pass(&mut f, &inliner, PassOptions::default()).inlined;
    }

    let size: usize = f
        .blocks
        .iter()
        .flat_map(|b| b.instrs.iter())
        .filter(|i| !matches!(i, Instr::Param { .. }))
        .count();
    assert!(
        total_inlined >= 1,
        "a direct self-call within budget is worth one expansion\n{}",
        f.dump()
    );
    assert!(
        total_inlined <= 4,
        "self expansions must stop at the per-pipeline budget, got {total_inlined}\n{}",
        f.dump()
    );
    assert!(
        size <= original * 3 + 80,
        "output must stay within the growth cap of its input: {original} -> {size}\n{}",
        f.dump()
    );
}

/// Direct mutual recursion (A calls B, B calls A) is never expanded: each
/// round would re-open the other function's call site, and no per-site
/// budget bounds that. Reachability is what decides it.
#[test]
fn inline_refuses_direct_mutual_recursion() {
    // Function 9 calls 10; the body offered for 10 calls 9.
    let caller = fix_fib(10);
    let callee = fix_fib(9);
    let info = bodies(&[(10, callee)]);
    let mut f = lower(&caller.0, &caller.1).expect("lower").with_findex(9);
    let stats = run_pass(&mut f, &Inlining::new(&info), PassOptions::default());
    assert_eq!(
        stats.inlined,
        0,
        "a callee that calls back into the caller must be refused\n{}",
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
fn inline_retains_frames_that_transitively_capture_a_stack() {
    let (ops, tys) = fix_caller_call1();
    let wrapper = (
        vec![
            Opcode::Call1 {
                dst: Reg(1),
                fun: RefFun(8),
                arg0: Reg(0),
            },
            Opcode::Ret { ret: Reg(1) },
        ],
        vec![t(0); 2],
    );
    let capture = (
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
    nt.declare(NativeImport::new(
        3,
        "std",
        "call_stack_raw",
        vec![t(0)],
        t(0),
    ))
    .unwrap();
    let info = bodies(&[(7, wrapper), (8, capture)]).with_natives(nt);
    let mut f = lower(&ops, &tys).expect("lower");
    let stats = run_pass(&mut f, &Inlining::new(&info), PassOptions::default());
    assert_eq!(stats.inlined, 0, "{}", f.dump());
    assert_eq!(any_call(&f), 1, "stack-sensitive call must remain\n{}", f.dump());
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

// ---------------------------------------------------------------------------
// escape analysis + scalar replacement of aggregates
// ---------------------------------------------------------------------------

/// Object type used by the SROA fixtures.
const OBJ: u32 = 5;
/// Enum type used by the SROA fixtures.
const ENUM: u32 = 3;

/// Allocate, fill two fields, read both back. The pointer never leaves.
fn fix_sroa_local_object() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::New { dst: Reg(1) },
            Opcode::SetField {
                obj: Reg(1),
                field: RefField(0),
                src: Reg(0),
            },
            Opcode::Int {
                dst: Reg(2),
                ptr: RefInt(0),
            },
            Opcode::SetField {
                obj: Reg(1),
                field: RefField(1),
                src: Reg(2),
            },
            Opcode::Field {
                dst: Reg(3),
                obj: Reg(1),
                field: RefField(0),
            },
            Opcode::Field {
                dst: Reg(4),
                obj: Reg(1),
                field: RefField(1),
            },
            Opcode::Add {
                dst: Reg(5),
                a: Reg(3),
                b: Reg(4),
            },
            Opcode::Ret { ret: Reg(5) },
        ],
        vec![t(0), t(OBJ), t(0), t(0), t(0), t(0)],
    )
}

/// The same object, but one field is written on each arm of a branch, so the
/// read needs a phi.
fn fix_sroa_merged_field() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::New { dst: Reg(1) },
            Opcode::JTrue {
                cond: Reg(0),
                offset: 3,
            },
            Opcode::Int {
                dst: Reg(2),
                ptr: RefInt(0),
            },
            Opcode::SetField {
                obj: Reg(1),
                field: RefField(0),
                src: Reg(2),
            },
            Opcode::JAlways { offset: 2 },
            Opcode::Int {
                dst: Reg(2),
                ptr: RefInt(1),
            },
            Opcode::SetField {
                obj: Reg(1),
                field: RefField(0),
                src: Reg(2),
            },
            Opcode::Field {
                dst: Reg(3),
                obj: Reg(1),
                field: RefField(0),
            },
            Opcode::Ret { ret: Reg(3) },
        ],
        vec![t(0), t(OBJ), t(0), t(0)],
    )
}

/// Reading a field the program never wrote: the object's initial state cannot
/// be named, so the allocation stays.
fn fix_sroa_read_before_write() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::New { dst: Reg(1) },
            Opcode::Field {
                dst: Reg(2),
                obj: Reg(1),
                field: RefField(0),
            },
            Opcode::Ret { ret: Reg(2) },
        ],
        vec![t(0), t(OBJ), t(0)],
    )
}

/// Body of `fix_sroa_local_object` with the final read replaced by `escape`.
fn sroa_escape(escape: Vec<Opcode>, extra_tys: Vec<TypeRef>) -> (Vec<Opcode>, Vec<TypeRef>) {
    let mut ops = vec![
        Opcode::New { dst: Reg(1) },
        Opcode::SetField {
            obj: Reg(1),
            field: RefField(0),
            src: Reg(0),
        },
    ];
    ops.extend(escape);
    let mut tys = vec![t(0), t(OBJ), t(0), t(0)];
    tys.extend(extra_tys);
    (ops, tys)
}

/// `MakeEnum` initializes its payload; reading it back never needs the box.
fn fix_sroa_enum_payload() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::Int {
                dst: Reg(2),
                ptr: RefInt(0),
            },
            Opcode::MakeEnum {
                dst: Reg(1),
                construct: RefEnumConstruct(0),
                args: vec![Reg(0), Reg(2)],
            },
            Opcode::EnumField {
                dst: Reg(3),
                value: Reg(1),
                construct: RefEnumConstruct(0),
                field: RefField(1),
            },
            Opcode::Ret { ret: Reg(3) },
        ],
        vec![t(0), t(ENUM), t(0), t(0)],
    )
}

/// Same enum, but the construct tag is read: folding it would need an integer
/// constant-pool index the IR cannot mint.
fn fix_sroa_enum_index() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::MakeEnum {
                dst: Reg(1),
                construct: RefEnumConstruct(0),
                args: vec![Reg(0)],
            },
            Opcode::EnumIndex {
                dst: Reg(2),
                value: Reg(1),
            },
            Opcode::Ret { ret: Reg(2) },
        ],
        vec![t(0), t(ENUM), t(0)],
    )
}

/// The allocation happens inside a `try`, so lowering pins it to a cell.
fn fix_sroa_in_trap() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::Trap {
                exc: Reg(2),
                offset: 5,
            }, // handler at op 6
            Opcode::New { dst: Reg(1) },
            Opcode::SetField {
                obj: Reg(1),
                field: RefField(0),
                src: Reg(0),
            },
            Opcode::Field {
                dst: Reg(3),
                obj: Reg(1),
                field: RefField(0),
            },
            Opcode::EndTrap { exc: Reg(2) },
            Opcode::Ret { ret: Reg(3) },
            Opcode::Ret { ret: Reg(0) }, // handler
        ],
        vec![t(0), t(OBJ), t(2), t(0)],
    )
}

fn sroa_on(ops: &[Opcode], tys: &[TypeRef]) -> (Function, PassStats) {
    let mut f = lower(ops, tys).expect("lower");
    let stats = run_pass(&mut f, &ScalarReplacement, PassOptions::default());
    (f, stats)
}

fn count_allocs(f: &Function) -> usize {
    count_instrs(f, |i| {
        matches!(
            i,
            Instr::New { .. } | Instr::EnumAlloc { .. } | Instr::MakeEnum { .. }
        )
    })
}

#[test]
fn sroa_removes_a_non_escaping_object() {
    let (ops, tys) = fix_sroa_local_object();
    let (f, stats) = sroa_on(&ops, &tys);
    assert_eq!(stats.allocs_removed, 1, "{}", f.dump());
    assert_eq!(stats.fields_scalarized, 2);
    assert_eq!(count_allocs(&f), 0, "{}", f.dump());
    assert_eq!(
        count_instrs(&f, |i| matches!(
            i,
            Instr::FieldGet { .. } | Instr::FieldSet { .. }
        )),
        0,
        "every field access became an SSA value\n{}",
        f.dump()
    );
}

#[test]
fn sroa_builds_a_phi_for_a_field_written_on_both_arms() {
    let (ops, tys) = fix_sroa_merged_field();
    let (f, stats) = sroa_on(&ops, &tys);
    assert_eq!(stats.allocs_removed, 1, "{}", f.dump());
    assert_eq!(count_allocs(&f), 0);
    let phis: usize = f.blocks.iter().map(|b| b.phis.len()).sum();
    assert!(
        phis >= 1,
        "the merged field needs a phi at the join\n{}",
        f.dump()
    );
}

#[test]
fn sroa_scalarizes_an_enum_payload() {
    let (ops, tys) = fix_sroa_enum_payload();
    let (f, stats) = sroa_on(&ops, &tys);
    assert_eq!(stats.allocs_removed, 1, "{}", f.dump());
    assert_eq!(count_allocs(&f), 0, "{}", f.dump());
}

#[test]
fn sroa_refuses_an_escaping_object() {
    let cases: Vec<(&str, (Vec<Opcode>, Vec<TypeRef>))> = vec![
        (
            "returned",
            sroa_escape(vec![Opcode::Ret { ret: Reg(1) }], vec![]),
        ),
        (
            "passed to a call",
            sroa_escape(
                vec![
                    Opcode::Call1 {
                        dst: Reg(2),
                        fun: RefFun(7),
                        arg0: Reg(1),
                    },
                    Opcode::Ret { ret: Reg(2) },
                ],
                vec![],
            ),
        ),
        (
            "stored into memory",
            sroa_escape(
                vec![
                    Opcode::New { dst: Reg(4) },
                    Opcode::SetField {
                        obj: Reg(4),
                        field: RefField(2),
                        src: Reg(1),
                    },
                    Opcode::Ret { ret: Reg(4) },
                ],
                vec![t(OBJ)],
            ),
        ),
        (
            "address taken",
            sroa_escape(
                vec![
                    Opcode::Ref {
                        dst: Reg(4),
                        src: Reg(1),
                    },
                    Opcode::Ret { ret: Reg(0) },
                ],
                vec![t(9)],
            ),
        ),
        (
            "identity compared",
            sroa_escape(
                vec![
                    Opcode::Null { dst: Reg(4) },
                    Opcode::JEq {
                        a: Reg(1),
                        b: Reg(4),
                        offset: 0,
                    },
                    Opcode::Ret { ret: Reg(0) },
                ],
                vec![t(OBJ)],
            ),
        ),
        (
            "boxed",
            sroa_escape(
                vec![
                    Opcode::ToDyn {
                        dst: Reg(4),
                        src: Reg(1),
                    },
                    Opcode::Ret { ret: Reg(0) },
                ],
                vec![t(7)],
            ),
        ),
        ("enum tag observed", fix_sroa_enum_index()),
        ("read before write", fix_sroa_read_before_write()),
        ("inside a try", fix_sroa_in_trap()),
    ];
    for (name, (ops, tys)) in cases {
        let (f, stats) = sroa_on(&ops, &tys);
        assert_eq!(
            stats,
            PassStats::default(),
            "{}: the allocation must not be scalarized\n{}",
            name,
            f.dump()
        );
        assert!(count_allocs(&f) >= 1, "{}: {}", name, f.dump());
    }
}

#[test]
fn sroa_refuses_a_phi_merged_allocation() {
    // Two allocations merged into one register, then a field read: neither
    // pointer is single-valued at the read.
    let ops = vec![
        Opcode::JTrue {
            cond: Reg(0),
            offset: 2,
        },
        Opcode::New { dst: Reg(1) },
        Opcode::JAlways { offset: 1 },
        Opcode::New { dst: Reg(1) },
        Opcode::SetField {
            obj: Reg(1),
            field: RefField(0),
            src: Reg(0),
        },
        Opcode::Field {
            dst: Reg(2),
            obj: Reg(1),
            field: RefField(0),
        },
        Opcode::Ret { ret: Reg(2) },
    ];
    let tys = vec![t(0), t(OBJ), t(0)];
    let (f, stats) = sroa_on(&ops, &tys);
    assert!(
        f.blocks.iter().any(|b| !b.phis.is_empty()),
        "the fixture must actually merge the two allocations\n{}",
        f.dump()
    );
    assert_eq!(stats, PassStats::default(), "{}", f.dump());
    assert_eq!(count_allocs(&f), 2);
}

#[test]
fn sroa_preserves_semantics_on_the_scalarizable_fixtures() {
    // The mini interpreter has no heap, so equivalence is checked on the
    // post-SROA arithmetic: the fixtures are written so the result is a pure
    // function of the arguments.
    for (name, (ops, tys), ints, inputs, expect) in [
        (
            "local_object",
            fix_sroa_local_object(),
            vec![7],
            vec![vec![3i64], vec![-2]],
            vec![10i64, 5],
        ),
        (
            "merged_field",
            fix_sroa_merged_field(),
            vec![11, 22],
            vec![vec![0i64], vec![1]],
            vec![11, 22],
        ),
    ] {
        let mut f = lower(&ops, &tys).expect("lower");
        let stats = run_pass(&mut f, &ScalarReplacement, PassOptions::default());
        assert_eq!(stats.allocs_removed, 1, "{}: {}", name, f.dump());
        let out = serialize(&f).expect("serialize");
        assert!(
            !out.ops
                .iter()
                .any(|o| matches!(o, Opcode::New { .. } | Opcode::Field { .. })),
            "{}: the heap traffic is gone\n{}",
            name,
            ops_text(&out.ops)
        );
        let f2 = lower(&out.ops, &out.reg_types)
            .unwrap_or_else(|e| panic!("{name}: re-lower: {e}\n{}", ops_text(&out.ops)));
        verify(&f2).unwrap();
        for (input, want) in inputs.iter().zip(expect) {
            assert_eq!(
                mini_eval(&out.ops, &ints, input, out.num_regs),
                want,
                "{}: wrong result for {:?}\n{}",
                name,
                input,
                ops_text(&out.ops)
            );
        }
    }
}

// ---------------------------------------------------------------------------
// the pass chain the mandelbrot inner loop needs
// ---------------------------------------------------------------------------

/// `Complex(this, re, im)`: the constructor shape every HL `new` goes through
/// — the fresh object is passed straight in, which is why nothing stops
/// escaping until this call is inlined. findex 7.
fn fix_complex_ctor() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::SetField {
                obj: Reg(0),
                field: RefField(0),
                src: Reg(1),
            },
            Opcode::SetField {
                obj: Reg(0),
                field: RefField(1),
                src: Reg(2),
            },
            Opcode::Ret { ret: Reg(0) },
        ],
        vec![t(OBJ), t(0), t(0)],
    )
}

/// A loop whose body allocates a small aggregate, hands it to its constructor
/// and reads the fields back — the mandelbrot inner-loop shape.
fn fix_mandelbrot_shaped() -> (Vec<Opcode>, Vec<TypeRef>) {
    (
        vec![
            Opcode::Int {
                dst: Reg(6),
                ptr: RefInt(0),
            }, // acc = 0
            Opcode::Int {
                dst: Reg(1),
                ptr: RefInt(0),
            }, // i = 0
            Opcode::Int {
                dst: Reg(2),
                ptr: RefInt(1),
            }, // one = 1
            Opcode::Label,
            Opcode::JSGte {
                a: Reg(1),
                b: Reg(0),
                offset: 10,
            }, // while i < n
            Opcode::New { dst: Reg(3) },
            Opcode::Mov {
                dst: Reg(4),
                src: Reg(1),
            },
            Opcode::Mov {
                dst: Reg(5),
                src: Reg(1),
            },
            Opcode::Call3 {
                dst: Reg(8),
                fun: RefFun(7),
                arg0: Reg(3),
                arg1: Reg(4),
                arg2: Reg(5),
            },
            Opcode::Field {
                dst: Reg(7),
                obj: Reg(8),
                field: RefField(0),
            },
            Opcode::Add {
                dst: Reg(6),
                a: Reg(6),
                b: Reg(7),
            },
            Opcode::Field {
                dst: Reg(7),
                obj: Reg(8),
                field: RefField(1),
            },
            Opcode::Add {
                dst: Reg(6),
                a: Reg(6),
                b: Reg(7),
            },
            Opcode::Add {
                dst: Reg(1),
                a: Reg(1),
                b: Reg(2),
            },
            Opcode::JAlways { offset: -12 },
            Opcode::Ret { ret: Reg(6) },
        ],
        vec![t(0), t(0), t(0), t(OBJ), t(0), t(0), t(0), t(0), t(OBJ)],
    )
}

#[test]
fn inlining_then_sroa_removes_the_per_iteration_allocation() {
    let (ops, tys) = fix_mandelbrot_shaped();
    let info = bodies(&[(7, fix_complex_ctor())]);
    let mut f = lower_with(&ops, &tys, &info).expect("lower");
    assert_eq!(
        count_allocs(&f),
        1,
        "the fixture allocates once per iteration"
    );

    // Escape analysis alone finds nothing: the object is handed to the
    // constructor call.
    let before = run_pass(&mut f, &ScalarReplacement, PassOptions::default());
    assert_eq!(
        before,
        PassStats::default(),
        "SROA before inlining must find nothing\n{}",
        f.dump()
    );

    let pm = PassManager::with_module(OptLevel::O3, &info).with_options(PassOptions {
        verify_each: true,
        ..PassOptions::default()
    });
    let report = pm.run(&mut f).expect("O3");
    verify(&f).unwrap_or_else(|e| panic!("verify after O3: {e}\n{}", f.dump()));

    assert_eq!(report.stats_for("inline").inlined, 1, "{}", f.dump());
    // The allocation is written to the object register inside the loop, so the
    // loop header carries a phi over it until DCE removes that dead phi. SROA
    // therefore fires on a later manager round, not the first.
    assert!(
        report.rounds > 1,
        "the chain needs a second round to see past the loop-carried phi"
    );
    assert_eq!(
        report.stats_for("sroa").allocs_removed,
        1,
        "the allocation must be gone\n{}",
        f.dump()
    );
    assert_eq!(count_allocs(&f), 0, "{}", f.dump());
    assert_eq!(
        any_call(&f),
        0,
        "the constructor call is gone\n{}",
        f.dump()
    );
    assert_eq!(
        count_instrs(&f, |i| matches!(
            i,
            Instr::FieldGet { .. } | Instr::FieldSet { .. }
        )),
        0,
        "no field traffic survives\n{}",
        f.dump()
    );

    // And the loop still computes the same thing.
    let out = serialize(&f).expect("serialize");
    let ints = vec![0, 1];
    let m = MiniModule::new(&ints);
    for n in [0i64, 1, 4, 9] {
        let expect: i64 = (0..n).map(|i| 2 * i).sum();
        assert_eq!(
            mini_eval_in(&m, &out.ops, &[n], out.num_regs),
            expect,
            "n = {}\n{}",
            n,
            ops_text(&out.ops)
        );
    }
}

// ---------------------------------------------------------------------------
// O3: round-trip property and semantic equivalence
// ---------------------------------------------------------------------------

/// Every fixture in the corpus, with what O3 needs to act on it: the module
/// info the inliner reads and the identity tail-recursion elimination needs.
fn o3_corpus() -> Vec<(
    &'static str,
    Vec<Opcode>,
    Vec<TypeRef>,
    Option<usize>,
    ModuleTables,
)> {
    let none = ModuleTables::new;
    let mut out: Vec<(
        &'static str,
        Vec<Opcode>,
        Vec<TypeRef>,
        Option<usize>,
        ModuleTables,
    )> = Vec::new();
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
        ("sroa_local_object", fix_sroa_local_object()),
        ("sroa_merged_field", fix_sroa_merged_field()),
        ("sroa_enum_payload", fix_sroa_enum_payload()),
        ("sroa_enum_index", fix_sroa_enum_index()),
        ("sroa_read_before_write", fix_sroa_read_before_write()),
        ("sroa_in_trap", fix_sroa_in_trap()),
    ] {
        out.push((name, ops, tys, None, none()));
    }
    for (name, (ops, tys)) in [
        ("tail_sum", fix_tail_sum()),
        ("tail_swap", fix_tail_swap()),
        ("tail_cell_param", fix_tail_cell_param()),
        ("tail_in_trap", fix_tail_in_trap()),
    ] {
        out.push((name, ops, tys, Some(0), none()));
    }
    let (ops, tys) = fix_caller_call2();
    out.push((
        "caller_add",
        ops.clone(),
        tys.clone(),
        None,
        bodies(&[(7, fix_callee_add())]),
    ));
    out.push((
        "caller_max",
        ops,
        tys,
        None,
        bodies(&[(7, fix_callee_max())]),
    ));
    let (ops, tys) = fix_caller_call1();
    out.push((
        "caller_nested",
        ops,
        tys,
        None,
        bodies(&[(7, fix_callee_outer()), (8, fix_callee_inner())]),
    ));
    let (ops, tys) = fix_mandelbrot_shaped();
    out.push((
        "mandelbrot_shaped",
        ops,
        tys,
        None,
        bodies(&[(7, fix_complex_ctor())]),
    ));
    out
}

#[test]
fn optimized_round_trip_at_o3_over_every_fixture() {
    for (name, ops, tys, findex, info) in o3_corpus() {
        let out = optimized_round_trip_at(OptLevel::O3, &info, findex, &ops, &tys);
        assert!(!out.ops.is_empty(), "{} produced no opcodes", name);
        // Optimizing the optimized output is stable. The output is a different
        // function, so it carries neither the identity nor the call sites.
        let again = optimized_round_trip_at(
            OptLevel::O3,
            &ModuleTables::new(),
            None,
            &out.ops,
            &out.reg_types,
        );
        assert!(!again.ops.is_empty(), "{} was not stable", name);
    }
}

#[test]
fn o3_reaches_a_fixed_point() {
    // Inlining grows a function and DCE shrinks it, so the pair could oscillate
    // without the caller ceiling.
    let (ops, tys) = fix_caller_call1();
    let info = bodies(&[(7, fix_callee_outer()), (8, fix_callee_inner())]);
    let mut f = lower_with(&ops, &tys, &info).expect("lower");
    let pm = PassManager::with_module(OptLevel::O3, &info).with_options(PassOptions {
        verify_each: true,
        ..PassOptions::default()
    });
    pm.run(&mut f).expect("O3");
    let again = pm.run(&mut f).expect("O3 again");
    assert!(!again.changed(), "O3 is not at a fixed point\n{}", f.dump());
}

#[test]
fn o3_preserves_semantics() {
    struct Case {
        name: &'static str,
        ops: Vec<Opcode>,
        tys: Vec<TypeRef>,
        findex: Option<usize>,
        info: ModuleTables,
        callees: Vec<(usize, (Vec<Opcode>, Vec<TypeRef>))>,
        ints: Vec<i32>,
        inputs: Vec<Vec<i64>>,
    }
    let plain = |name, (ops, tys): (Vec<Opcode>, Vec<TypeRef>), ints: Vec<i32>, inputs| Case {
        name,
        ops,
        tys,
        findex: None,
        info: ModuleTables::new(),
        callees: vec![],
        ints,
        inputs,
    };
    let cases = vec![
        plain(
            "straight_line",
            fix_straight_line(),
            vec![7, 35],
            vec![vec![]],
        ),
        plain(
            "diamond",
            fix_diamond(),
            vec![10, 20],
            vec![vec![0], vec![1]],
        ),
        plain(
            "loop",
            fix_loop(),
            vec![0, 1],
            vec![vec![0], vec![1], vec![5], vec![10]],
        ),
        plain(
            "loop_no_label",
            fix_loop_no_label(),
            vec![0, 1],
            vec![vec![0], vec![3], vec![7]],
        ),
        plain(
            "loop_invariant",
            fix_loop_invariant(),
            vec![0, 1],
            vec![vec![0, 3], vec![4, 5]],
        ),
        plain(
            "switch",
            fix_switch(),
            vec![100, 200, 300],
            vec![vec![0], vec![1], vec![2], vec![5], vec![-1]],
        ),
        plain("incr", fix_incr(), vec![40], vec![vec![]]),
        Case {
            name: "tail_sum",
            ops: fix_tail_sum().0,
            tys: fix_tail_sum().1,
            findex: Some(0),
            info: ModuleTables::new(),
            callees: vec![(0, fix_tail_sum())],
            ints: vec![0, 1],
            inputs: vec![vec![0, 0], vec![1, 0], vec![6, 2], vec![9, -3]],
        },
        Case {
            name: "tail_swap",
            ops: fix_tail_swap().0,
            tys: fix_tail_swap().1,
            findex: Some(0),
            info: ModuleTables::new(),
            callees: vec![(0, fix_tail_swap())],
            ints: vec![0, 1],
            inputs: vec![vec![7, 11, 0], vec![7, 11, 1], vec![7, 11, 4]],
        },
        Case {
            name: "caller_max",
            ops: fix_caller_call2().0,
            tys: fix_caller_call2().1,
            findex: None,
            info: bodies(&[(7, fix_callee_max())]),
            callees: vec![(7, fix_callee_max())],
            ints: vec![10, 1],
            inputs: vec![vec![0], vec![3], vec![25], vec![-8]],
        },
        Case {
            name: "caller_nested",
            ops: fix_caller_call1().0,
            tys: fix_caller_call1().1,
            findex: None,
            info: bodies(&[(7, fix_callee_outer()), (8, fix_callee_inner())]),
            callees: vec![(7, fix_callee_outer()), (8, fix_callee_inner())],
            ints: vec![10, 1],
            inputs: vec![vec![0], vec![5], vec![-2]],
        },
    ];
    for c in cases {
        let out = optimized_round_trip_at(OptLevel::O3, &c.info, c.findex, &c.ops, &c.tys);
        // A pass may mint a constant the module's pool does not hold -- the
        // widener's identity and its `& ~(VF-1)` mask both do -- and the
        // serializer hands those back in `new_ints`, indexed after the pool.
        // Without appending them the evaluator reads past the end of its own
        // table and the fixture fails on an index, not on a wrong answer.
        let mut ints = c.ints.clone();
        ints.extend(out.new_ints.iter().copied());
        let mut m = MiniModule::new(&ints);
        for (findex, (cops, ctys)) in &c.callees {
            m = m.with_fun(*findex, cops, ctys.len());
        }
        for input in &c.inputs {
            let before = mini_eval_in(&m, &c.ops, input, c.tys.len());
            let after = mini_eval_in(&m, &out.ops, input, out.num_regs);
            assert_eq!(
                before,
                after,
                "{}: O3 changed the result for {:?}\n{}",
                c.name,
                input,
                ops_text(&out.ops)
            );
        }
    }
}

/// The motivating shape: an object built and dropped inside one iteration is
/// hoistable, and one whose result becomes the next iteration's input is not.
/// mandelbrot's inner loop contains exactly one of each —
/// `val = complexAdd(complexSquare(val), offset)` — which is why the sound
/// analysis reaches half of its 196.5M allocations rather than all of them.
#[test]
fn escape_separates_iteration_local_allocations_from_carried_ones() {
    use crate::v2::analysis::{CfgInfo, LoopForest};
    use crate::v2::passes::escape::analyze_alloc_escapes;

    // Build the loop by hand: two allocations, one purely local, one whose
    // value feeds a phi at the header.
    let ops = vec![
        Opcode::Int {
            dst: Reg(0),
            ptr: RefInt(0),
        },
        Opcode::Label,
        // local: allocated, written, read, dropped
        Opcode::New { dst: Reg(1) },
        Opcode::SetField {
            obj: Reg(1),
            field: RefField(0),
            src: Reg(0),
        },
        Opcode::Field {
            dst: Reg(2),
            obj: Reg(1),
            field: RefField(0),
        },
        Opcode::Incr { dst: Reg(0) },
        Opcode::JSLt {
            a: Reg(0),
            b: Reg(0),
            offset: -5,
        },
        Opcode::Ret { ret: Reg(2) },
    ];
    let tys = vec![t(0); 3];
    let f = lower(&ops, &tys).unwrap();
    verify(&f).unwrap();
    let cfg = CfgInfo::build(&f);
    let forest = LoopForest::analyze(&f, &cfg);
    assert!(!forest.is_empty(), "fixture must contain a loop");

    let l = forest.innermost_first()[0];
    let infos = analyze_alloc_escapes(&f, &forest, l);
    assert_eq!(infos.len(), 1, "one New in the loop: {infos:?}");
    assert!(
        !infos[0].escapes,
        "an object built and dropped in one iteration must be hoistable: {:?}",
        infos[0].reason
    );
}

// ---------------------------------------------------------------------------
// cellfwd regressions (refusals found in a large program)
// ---------------------------------------------------------------------------

/// f310 `init` from a game fixture, minimized: an in-place redefine of a pinned
/// register (`Not r, r`) whose `CellGet` cellfwd forwards and deletes. The
/// pass used to skip `compact_values`, leaving the deleted load's dst as an
/// undefined value-table entry that nothing else cleaned up when no later
/// pass removed anything — verify then refused the whole function.
#[test]
fn cellfwd_compacts_after_deleting_forwarded_loads() {
    let ops = vec![
        Opcode::Bool {
            dst: Reg(0),
            value: false,
        },
        Opcode::Not {
            dst: Reg(0),
            src: Reg(0),
        },
        Opcode::Ref {
            dst: Reg(1),
            src: Reg(0),
        },
        Opcode::Ret { ret: Reg(1) },
    ];
    let mut f = lower(&ops, &[t(0), t(1)]).unwrap();
    verify(&f).unwrap();
    let pm = PassManager::new(OptLevel::O2).with_options(PassOptions {
        verify_each: true,
        ..PassOptions::default()
    });
    pm.run(&mut f)
        .unwrap_or_else(|e| panic!("pipeline: {e}\n{}", f.dump()));
    verify(&f).unwrap_or_else(|e| panic!("verify: {e}\n{}", f.dump()));
}

/// f6227 `initFromScene` from a game fixture, minimized: a `CellSet` whose stored
/// value is itself the dst of a forwarded (hence deleted) `CellGet`. The
/// forward tuples are collected before any rewriting, so applying
/// `(v2 -> v1)` after `(v1 -> v0)` must chase the chain to `v0`; pointing
/// uses at `v1` leaves them dangling once both loads are removed.
#[test]
fn cellfwd_resolves_forwarding_chains() {
    use super::passes::CellForwarding;
    let mut f = empty_func(vec![t(0), t(0), t(1)]);
    let c0 = CellId(0);
    let c1 = CellId(1);
    f.cells = vec![
        CellData {
            reg: 0,
            ty: t(0),
            reason: PinReason::RefTaken,
        },
        CellData {
            reg: 1,
            ty: t(0),
            reason: PinReason::RefTaken,
        },
    ];
    let v0 = f.new_value(t(0), 0);
    let v1 = f.new_value(t(0), 0);
    let v2 = f.new_value(t(0), 1);
    let v3 = f.new_value(t(0), 1);
    let v4 = f.new_value(t(1), 2);
    let v5 = f.new_value(t(1), 2);
    f.blocks.push(Block {
        phis: vec![],
        instrs: vec![
            Instr::Int { dst: v0, idx: 0 },
            Instr::CellSet { cell: c0, src: v0 },
            Instr::CellGet { dst: v1, cell: c0 },
            Instr::CellSet { cell: c1, src: v1 },
            Instr::CellGet { dst: v2, cell: c1 },
            Instr::UnOp {
                op: UnOp::Not,
                dst: v3,
                src: v2,
            },
            Instr::CellSet { cell: c0, src: v3 },
            Instr::CellRef { dst: v4, cell: c0 },
            Instr::CellRef { dst: v5, cell: c1 },
        ],
        term: Terminator::Ret { value: v4 },
        handler: None,
    });
    verify(&f).unwrap_or_else(|e| panic!("fixture must verify: {e}\n{}", f.dump()));

    let stats = CellForwarding
        .run(&mut f, &PassOptions::default())
        .unwrap_or_else(|e| panic!("cellfwd: {e}\n{}", f.dump()));
    assert_eq!(stats.eliminated, 2, "both loads forwarded:\n{}", f.dump());
    verify(&f).unwrap_or_else(|e| panic!("verify after cellfwd: {e}\n{}", f.dump()));

    // The Not must now read the originally stored value, not the deleted
    // intermediate load.
    let not_src = f
        .blocks
        .iter()
        .flat_map(|b| b.instrs.iter())
        .find_map(|i| match i {
            Instr::UnOp {
                op: UnOp::Not, src, ..
            } => Some(*src),
            _ => None,
        })
        .expect("Not survives");
    let src_def = f
        .blocks
        .iter()
        .flat_map(|b| b.instrs.iter())
        .find(|i| i.dst() == Some(not_src))
        .expect("Not's operand is defined");
    assert!(
        matches!(src_def, Instr::Int { .. }),
        "chain must resolve to the stored Int, got {src_def:?}\n{}",
        f.dump()
    );
}

// ── Liveness ────────────────────────────────────────────────────────────────

/// Every value live on entry to a block is defined in a block that dominates
/// it. This is the SSA property the analysis exists to respect, and the one an
/// OSR entry depends on: a value handed over at a header must actually hold
/// something on every path that reaches it.
#[test]
fn liveness_live_in_is_dominated_by_its_definition() {
    use super::analysis::CfgInfo;
    use super::ir::BlockId;
    use super::liveness::Liveness;

    for (name, (ops, tys)) in [
        ("loop", fix_loop()),
        ("diamond", fix_diamond()),
        ("switch", fix_switch()),
        ("calls", fix_calls_natives()),
    ] {
        let f = lower(&ops, &tys).unwrap_or_else(|e| panic!("{name}: lower: {e}"));
        let cfg = CfgInfo::build(&f);
        let live = Liveness::analyze(&f, &cfg);

        let mut def_block = vec![usize::MAX; f.values.len()];
        for (bi, b) in f.blocks.iter().enumerate() {
            for phi in &b.phis {
                def_block[phi.dst.idx()] = bi;
            }
            for ins in &b.instrs {
                if let Some(d) = ins.dst() {
                    def_block[d.idx()] = bi;
                }
            }
        }

        let mut checked = 0usize;
        for bi in 0..f.blocks.len() {
            for v in live.live_in(BlockId(bi as u32)) {
                checked += 1;
                let d = def_block[v.idx()];
                assert!(
                    d != usize::MAX,
                    "{name}: {v:?} is live-in at b{bi} but nothing defines it"
                );
                assert!(
                    cfg.dom.dominates(d, bi),
                    "{name}: {v:?} is live-in at b{bi} but its definition in b{d} \
                     does not dominate it"
                );
            }
        }
        // Or the assertions above hold because there was nothing to assert.
        assert!(checked > 0, "{name}: no value was live anywhere");
    }
}

/// Nothing is live before the function starts.
#[test]
fn liveness_entry_has_nothing_live_in() {
    use super::analysis::CfgInfo;
    use super::ir::BlockId;
    use super::liveness::Liveness;

    let (ops, tys) = fix_loop();
    let f = lower(&ops, &tys).unwrap();
    let cfg = CfgInfo::build(&f);
    let live = Liveness::analyze(&f, &cfg);
    assert!(
        live.live_in(BlockId(0)).is_empty(),
        "entry live-in should be empty, got {:?}",
        live.live_in(BlockId(0))
    );
}

/// A loop carries values across its back edge, so a block inside one has
/// something live on entry -- the accumulator and the induction variable at
/// minimum. A liveness that reported nothing here would look like it worked
/// while telling an OSR entry to resume with an empty frame.
#[test]
fn liveness_carries_values_around_a_loop() {
    use super::analysis::CfgInfo;
    use super::ir::BlockId;
    use super::liveness::Liveness;

    let (ops, tys) = fix_loop();
    let f = lower(&ops, &tys).unwrap();
    let cfg = CfgInfo::build(&f);
    let live = Liveness::analyze(&f, &cfg);

    let looping = (0..f.blocks.len())
        .filter(|&b| cfg.succs[b].iter().any(|s| s.idx() <= b))
        .collect::<Vec<_>>();
    assert!(!looping.is_empty(), "fixture has no back edge");
    for b in looping {
        assert!(
            !live.live_in(BlockId(b as u32)).is_empty(),
            "b{b} closes a loop but reports nothing live on entry"
        );
    }
}

// ---------------------------------------------------------------------------
// Vectorization: analysis -> widening -> both backends' IR forms
// ---------------------------------------------------------------------------

/// `for (i = 0; i < 16; i++) dst[i] = k;` with `k` loop-invariant.
///
/// An ARRAY store: `SetMem` indexes in bytes, so stepping `i` by one would
/// write overlapping 4-byte stores and the analysis reports a stride of 2
/// rather than a contiguous 1. Array access indexes in elements.
///
/// One store and no load, so there is no pair for the alias check to refuse;
/// no register live across the back edge except the induction variable, so
/// there are no loop-carried values; a constant trip count of 16, which is
/// four vectors of four lanes with no remainder.
///
/// The obvious `dst[i] = a[i] + a[i]` fixture is REFUSED, and correctly: the
/// load and store are different `bytes` pointers the analysis cannot prove
/// disjoint, and it declines rather than assume. Widening that shape needs
/// alias analysis to separate the bases, not a change here.
fn widen_fixture() -> (Vec<Opcode>, Vec<TypeRef>) {
    // r0 dst(bytes) r1 i r2 limit r3 step r4 k
    let regs = vec![t(13), t(3), t(3), t(3), t(3)];
    let ops = vec![
        Opcode::Int { dst: Reg(1), ptr: RefInt(0) },   // i = 0
        Opcode::Int { dst: Reg(2), ptr: RefInt(1) },   // limit = 16
        Opcode::Int { dst: Reg(3), ptr: RefInt(2) },   // step = 1
        Opcode::Int { dst: Reg(4), ptr: RefInt(3) },   // k = 7
        Opcode::Label,
        Opcode::JSGte { a: Reg(1), b: Reg(2), offset: 3 },
        Opcode::SetArray { array: Reg(0), index: Reg(1), src: Reg(4) },
        Opcode::Add { dst: Reg(1), a: Reg(1), b: Reg(3) },
        Opcode::JAlways { offset: -4 },
        Opcode::Ret { ret: Reg(1) },
    ];
    (ops, regs)
}

/// The int pool the fixture's `RefInt`s name.
struct WidenInfo;
impl ModuleInfo for WidenInfo {
    fn int_value(&self, idx: usize) -> Option<i32> {
        [0i32, 16, 1, 7, 2].get(idx).copied()
    }
    fn int_pool_len(&self) -> usize {
        5
    }
    /// Every type in these fixtures is `t(3)`, HL's i32.
    fn type_size(&self, _ty: TypeRef) -> Option<u32> {
        Some(4)
    }
}

#[test]
fn widen_reports_a_plan_for_a_constant_trip_count_loop() {
    let (ops, regs) = widen_fixture();
    let f = lower_with(&ops, &regs, &WidenInfo).expect("lower");
    let plans = super::vectorize::analyze_with(
        &f,
        &super::vectorize::VecOptions::default(),
        &|i| WidenInfo.int_value(i),
    );
    for p in &plans {
        eprintln!(
            "loop at b{}: refusals {:?} induction {:?} accesses {:?} body {}",
            p.header.0, p.refusals, p.induction, p.accesses, p.body_size
        );
    }
    let report = super::passes::widen::explain(&f, &WidenInfo);
    assert!(
        !report.is_empty(),
        "no widenable loop found; the analysis refused before the transform \
         had a say:\n{}",
        f.dump()
    );
}

#[test]
fn widening_emits_vector_instructions_that_verify() {
    let (ops, regs) = widen_fixture();
    let mut f = lower_with(&ops, &regs, &WidenInfo).expect("lower");
    let before = f.dump();
    let pass = super::passes::widen::Widen { info: &WidenInfo };
    let stats = pass.run(&mut f, &PassOptions::default()).expect("widen");
    if stats.replaced == 0 {
        // Not a failure of correctness, but the reason has to be visible:
        // a transform that silently does nothing is the thing this whole
        // exercise is meant to avoid.
        let why = super::passes::widen::explain(&f, &WidenInfo);
        panic!("widened nothing; declines: {why:?}\n{before}");
    }
    verify(&f).unwrap_or_else(|e| panic!("verify after widen: {e}\n{}", f.dump()));
    let has_vec = f.blocks.iter().any(|b| {
        b.instrs.iter().any(|i| {
            matches!(
                i,
                Instr::VecLoad { .. }
                    | Instr::VecStore { .. }
                    | Instr::VecBinOp { .. }
                    | Instr::VecSplat { .. }
            )
        })
    });
    assert!(has_vec, "widen reported work but emitted no vector instruction");
}

#[test]
fn a_widened_function_scalarizes_back_to_runnable_bytecode() {
    let (ops, regs) = widen_fixture();
    let mut f = lower_with(&ops, &regs, &WidenInfo).expect("lower");
    let pass = super::passes::widen::Widen { info: &WidenInfo };
    if pass.run(&mut f, &PassOptions::default()).expect("widen").replaced == 0 {
        return; // covered by the test above
    }
    // `--emit-optimized` has to keep working on a function the vectorizer
    // touched: serialization unrolls the lanes back to scalar opcodes.
    let out = serialize(&f).expect("a widened function must still serialize");
    assert!(
        !out.ops.is_empty(),
        "scalarization produced no opcodes"
    );
    // Lane offsets are minted constants, named by index past the pool.
    for op in &out.ops {
        if let Opcode::Int { ptr, .. } = op {
            let idx = ptr.0;
            assert!(
                idx < WidenInfo.int_pool_len() + out.new_ints.len(),
                "opcode names int index {idx} with only {} pooled + {} minted",
                WidenInfo.int_pool_len(),
                out.new_ints.len()
            );
        }
    }
}

/// `for (i = 0; i < 16; i++) { if (i >= len) throw; dst[i] = k; }`
///
/// A bounds-checked store: the guard leaves the loop only to a throw, which
/// the analysis classifies as a guard rather than a second exit. A lane
/// executes whether or not the scalar loop would have reached it, so the
/// check has to be proven for the whole vector range before the loop and
/// removed from the body — testing it against an induction variable that now
/// steps by four proves nothing about lanes 1..3.
fn widen_guarded_fixture() -> (Vec<Opcode>, Vec<TypeRef>) {
    // r0 dst(bytes) r1 i r2 limit r3 step r4 k r5 len
    let regs = vec![t(13), t(3), t(3), t(3), t(3), t(3)];
    let ops = vec![
        Opcode::Int { dst: Reg(1), ptr: RefInt(0) },   // i = 0
        Opcode::Int { dst: Reg(2), ptr: RefInt(1) },   // limit = 16
        Opcode::Int { dst: Reg(3), ptr: RefInt(2) },   // step = 1
        Opcode::Int { dst: Reg(4), ptr: RefInt(3) },   // k = 7
        Opcode::Int { dst: Reg(5), ptr: RefInt(1) },   // len = 16
        Opcode::Label,
        Opcode::JSGte { a: Reg(1), b: Reg(2), offset: 5 },  // normal exit
        Opcode::JSLt { a: Reg(1), b: Reg(5), offset: 1 },   // guard: i < len -> ok
        Opcode::Throw { exc: Reg(4) },                       // else throw
        Opcode::SetArray { array: Reg(0), index: Reg(1), src: Reg(4) },
        Opcode::Add { dst: Reg(1), a: Reg(1), b: Reg(3) },
        Opcode::JAlways { offset: -6 },
        Opcode::Ret { ret: Reg(1) },
    ];
    (ops, regs)
}

#[test]
fn a_bounds_checked_loop_is_widenable_once_the_guard_is_hoisted() {
    let (ops, regs) = widen_guarded_fixture();
    let mut f = lower_with(&ops, &regs, &WidenInfo).expect("lower");
    let plans = super::vectorize::analyze_with(
        &f,
        &super::vectorize::VecOptions::default(),
        &|i| WidenInfo.int_value(i),
    );
    let guarded: Vec<_> = plans.iter().filter(|p| !p.guard_exits.is_empty()).collect();
    assert!(
        !guarded.is_empty(),
        "no guard recognised; the analysis saw the throw edge as an ordinary \
         exit:\n{}",
        f.dump()
    );
    let before = f.dump();
    let pass = super::passes::widen::Widen { info: &WidenInfo };
    let stats = pass.run(&mut f, &PassOptions::default()).expect("widen");
    if stats.replaced == 0 {
        let why = super::passes::widen::explain(&f, &WidenInfo);
        panic!("guarded loop not widened; declines: {why:?}\n{before}");
    }
    verify(&f).unwrap_or_else(|e| panic!("verify after guarded widen: {e}\n{}", f.dump()));
    // The guard must be GONE from the body: left in, it would be tested
    // against an induction variable stepping by a whole vector.
    let hoisted = f.blocks.iter().any(|b| {
        matches!(&b.term, Terminator::CondJump { .. }) && b.instrs.is_empty()
    });
    assert!(hoisted, "no hoisted pre-loop check was created:\n{}", f.dump());
}

/// `for (i = 0; i < len; i++) dst[i] = k;` — a RUNTIME length.
///
/// The common shape, and the one a compile-time trip count cannot cover: the
/// loop runs `len` times, `len` is not known here, so the widened loop takes
/// `len & ~3` iterations and a scalar copy finishes the remainder.
fn widen_runtime_fixture() -> (Vec<Opcode>, Vec<TypeRef>) {
    // r0 dst(array) r1 i r2 len r3 step r4 k
    let regs = vec![t(13), t(3), t(3), t(3), t(3)];
    let ops = vec![
        Opcode::Int { dst: Reg(1), ptr: RefInt(0) },   // i = 0
        Opcode::Int { dst: Reg(3), ptr: RefInt(2) },   // step = 1
        Opcode::Int { dst: Reg(4), ptr: RefInt(3) },   // k = 7
        Opcode::Label,
        Opcode::JSGte { a: Reg(1), b: Reg(2), offset: 3 },  // i >= len -> exit
        Opcode::SetArray { array: Reg(0), index: Reg(1), src: Reg(4) },
        Opcode::Add { dst: Reg(1), a: Reg(1), b: Reg(3) },
        Opcode::JAlways { offset: -4 },
        Opcode::Ret { ret: Reg(1) },
    ];
    (ops, regs)
}

/// The value a widened loop leaves behind is the REMAINDER's, not the vector
/// loop's.
///
/// `fill` returns its induction variable, which after widening stops at
/// `start + (n & ~3)` in the vector loop and only reaches the real limit in
/// the scalar copy. A use past the loop that still names the original phi
/// reads the wrong one of those two, and reads it silently -- the IR verifies
/// either way, because both values are defined and in scope.
#[test]
fn the_value_after_a_widened_loop_comes_from_the_remainder() {
    let (ops, regs) = widen_runtime_fixture();
    let mut f = lower_with(&ops, &regs, &WidenInfo).expect("lower");
    let iv_phi = f.blocks.iter().find_map(|b| {
        b.phis.first().map(|p| p.dst)
    });
    let pass = super::passes::widen::Widen { info: &WidenInfo };
    let stats = pass.run(&mut f, &PassOptions::default()).expect("widen");
    assert_eq!(stats.replaced, 1, "not widened:\n{}", f.dump());
    verify(&f).unwrap_or_else(|e| panic!("verify: {e}\n{}", f.dump()));

    let Some(iv_phi) = iv_phi else {
        panic!("fixture has no induction phi:\n{}", f.dump())
    };
    // The Ret must not still be reading the vector loop's induction.
    let returned = f
        .blocks
        .iter()
        .find_map(|b| match b.term {
            Terminator::Ret { value } => Some(value),
            _ => None,
        })
        .unwrap_or_else(|| panic!("no return:\n{}", f.dump()));
    assert_ne!(
        returned,
        iv_phi,
        "the return still names the vector loop's induction:\n{}",
        f.dump()
    );
}

#[test]
fn a_runtime_length_loop_widens_with_a_scalar_epilogue() {
    let (ops, regs) = widen_runtime_fixture();
    let mut f = lower_with(&ops, &regs, &WidenInfo).expect("lower");
    let blocks_before = f.blocks.len();
    let before = f.dump();
    let pass = super::passes::widen::Widen { info: &WidenInfo };
    let stats = pass.run(&mut f, &PassOptions::default()).expect("widen");
    if stats.replaced == 0 {
        let why = super::passes::widen::explain(&f, &WidenInfo);
        panic!("runtime-length loop not widened; declines: {why:?}\n{before}");
    }
    verify(&f).unwrap_or_else(|e| panic!("verify after epilogue: {e}\n{}", f.dump()));
    assert!(
        f.blocks.len() > blocks_before,
        "no epilogue blocks were added:\n{}",
        f.dump()
    );
    // Both forms must be present: vectors in the widened loop, the original
    // scalar store in the remainder.
    let has_vec = f.blocks.iter().any(|b| {
        b.instrs.iter().any(|i| matches!(i, Instr::VecStore { .. }))
    });
    let has_scalar = f.blocks.iter().any(|b| {
        b.instrs.iter().any(|i| matches!(i, Instr::MemSet { .. }))
    });
    assert!(has_vec, "widened loop has no vector store:\n{}", f.dump());
    assert!(has_scalar, "no scalar remainder survived:\n{}", f.dump());
}

/// `for (i = 0; i < 16; i++) bytes[i << 2] = k;` — a BYTE-indexed store.
///
/// This is what an HL array write actually looks like: the index is scaled to
/// bytes before it reaches memory, so the address steps by 4 and the value
/// that indexes it is not the induction variable but a shift of it.
fn widen_byte_index_fixture() -> (Vec<Opcode>, Vec<TypeRef>) {
    // r0 bytes r1 i r2 limit r3 step r4 k r5 shift r6 addr
    let regs = vec![t(9), t(3), t(3), t(3), t(3), t(3), t(3)];
    let ops = vec![
        Opcode::Int { dst: Reg(1), ptr: RefInt(0) },   // i = 0
        Opcode::Int { dst: Reg(2), ptr: RefInt(1) },   // limit = 16
        Opcode::Int { dst: Reg(3), ptr: RefInt(2) },   // step = 1
        Opcode::Int { dst: Reg(4), ptr: RefInt(3) },   // k = 7
        Opcode::Int { dst: Reg(5), ptr: RefInt(4) },   // shift = 2
        // r6 is written every iteration and read in the same one, but a
        // register with no definition before the loop still gets a header phi
        // -- which reads as a loop-carried value.
        Opcode::Int { dst: Reg(6), ptr: RefInt(0) },
        Opcode::Label,
        Opcode::JSGte { a: Reg(1), b: Reg(2), offset: 4 },
        Opcode::Shl { dst: Reg(6), a: Reg(1), b: Reg(5) },
        Opcode::SetMem { bytes: Reg(0), index: Reg(6), src: Reg(4) },
        Opcode::Add { dst: Reg(1), a: Reg(1), b: Reg(3) },
        Opcode::JAlways { offset: -5 },
        Opcode::Ret { ret: Reg(1) },
    ];
    (ops, regs)
}

/// Lower and clear dead code first. `r6` holds the scaled index, which SSA
/// construction gives a header phi because the register is defined in the
/// loop -- a phi nothing reads, and one the real pipeline deletes long before
/// the widener sees it. Without that step this fixture measures the absence
/// of DCE rather than the affine analysis.
fn lowered_and_cleaned(ops: &[Opcode], regs: &[TypeRef]) -> Function {
    let mut f = lower_with(ops, regs, &WidenInfo).expect("lower");
    let pm = super::passes::PassManager::with_passes(vec![Box::new(
        super::passes::dce::DeadCodeElim,
    )]);
    pm.run(&mut f).expect("dce");
    f
}

#[test]
fn a_byte_scaled_index_is_affine() {
    let (ops, regs) = widen_byte_index_fixture();
    let f = lowered_and_cleaned(&ops, &regs);
    let plans = super::vectorize::analyze_with(
        &f,
        &super::vectorize::VecOptions::default(),
        &|i| WidenInfo.int_value(i),
    );
    let p = plans
        .iter()
        .find(|p| !p.accesses.is_empty())
        .unwrap_or_else(|| panic!("no loop with an access:\n{}", f.dump()));
    // `i << 2` steps by 4 bytes, which for a 4-byte element is contiguous.
    assert_eq!(p.accesses[0].stride, 4, "{:?}", p.accesses);
    assert_eq!(
        p.accesses[0].contiguous_stride(Some(4)),
        Some(4),
        "a 4-byte element walked 4 bytes at a time is contiguous"
    );
    assert!(p.vectorizable(), "refusals: {:?}", p.refusals);
}

#[test]
fn a_byte_scaled_loop_widens_to_the_element_stride() {
    let (ops, regs) = widen_byte_index_fixture();
    let mut f = lowered_and_cleaned(&ops, &regs);
    let pass = super::passes::widen::Widen { info: &WidenInfo };
    let stats = pass.run(&mut f, &PassOptions::default()).expect("widen");
    assert_eq!(
        stats.replaced,
        1,
        "not widened; declines: {:?}\n{}",
        super::passes::widen::explain(&f, &WidenInfo),
        f.dump()
    );
    verify(&f).unwrap_or_else(|e| panic!("verify: {e}\n{}", f.dump()));
    let stride = f
        .blocks
        .iter()
        .flat_map(|b| b.instrs.iter())
        .find_map(|i| match i {
            Instr::VecStore { stride, .. } => Some(*stride),
            _ => None,
        })
        .unwrap_or_else(|| panic!("no vector store:\n{}", f.dump()));
    // The emitted stride is the element width, not a table keyed on the kind.
    assert_eq!(stride, 4, "{}", f.dump());
}

/// `sum = 0; for (i = 0; i < 16; i++) sum += a[i]; return sum;`
///
/// The most common vectorizable loop there is, and the one the transform
/// used to refuse outright: the accumulator is loop-carried, so widening it
/// means VF partial sums that collapse afterwards.
fn widen_reduction_fixture() -> (Vec<Opcode>, Vec<TypeRef>) {
    // r0 src(array) r1 i r2 limit r3 step r4 sum r5 elem
    let regs = vec![t(13), t(3), t(3), t(3), t(3), t(3)];
    let ops = vec![
        Opcode::Int { dst: Reg(1), ptr: RefInt(0) },   // i = 0
        Opcode::Int { dst: Reg(2), ptr: RefInt(1) },   // limit = 16
        Opcode::Int { dst: Reg(3), ptr: RefInt(2) },   // step = 1
        Opcode::Int { dst: Reg(4), ptr: RefInt(0) },   // sum = 0
        Opcode::Int { dst: Reg(5), ptr: RefInt(0) },   // elem = 0
        Opcode::Label,
        Opcode::JSGte { a: Reg(1), b: Reg(2), offset: 4 },
        Opcode::GetArray { dst: Reg(5), array: Reg(0), index: Reg(1) },
        Opcode::Add { dst: Reg(4), a: Reg(4), b: Reg(5) },
        Opcode::Add { dst: Reg(1), a: Reg(1), b: Reg(3) },
        Opcode::JAlways { offset: -5 },
        Opcode::Ret { ret: Reg(4) },
    ];
    (ops, regs)
}

#[test]
fn a_sum_over_an_array_widens_into_lane_partials() {
    let (ops, regs) = widen_reduction_fixture();
    let mut f = lowered_and_cleaned(&ops, &regs);
    for p in super::vectorize::analyze_with(
        &f,
        &super::vectorize::VecOptions::default(),
        &|i| WidenInfo.int_value(i),
    ) {
        eprintln!(
            "loop@b{} vectorizable={} refusals={:?} reductions={:?}",
            p.header.0,
            p.vectorizable(),
            p.refusals,
            p.reductions
        );
    }
    let pass = super::passes::widen::Widen { info: &WidenInfo };
    let stats = pass.run(&mut f, &PassOptions::default()).expect("widen");
    assert_eq!(
        stats.replaced,
        1,
        "not widened; declines: {:?}\n{}",
        super::passes::widen::explain(&f, &WidenInfo),
        f.dump()
    );
    verify(&f).unwrap_or_else(|e| panic!("verify: {e}\n{}", f.dump()));

    let has = |pred: fn(&Instr) -> bool| f.blocks.iter().any(|b| b.instrs.iter().any(pred));
    assert!(
        has(|i| matches!(i, Instr::VecLoad { .. })),
        "no vector load:\n{}",
        f.dump()
    );
    assert!(
        has(|i| matches!(i, Instr::VecSplat { .. })),
        "the lanes were never seeded with the identity:\n{}",
        f.dump()
    );
    assert!(
        has(|i| matches!(i, Instr::VecReduce { op: BinOp::Add, .. })),
        "the partials were never collapsed:\n{}",
        f.dump()
    );
    // The collapse feeds the return; the vector accumulator must not.
    let returned = f
        .blocks
        .iter()
        .find_map(|b| match b.term {
            Terminator::Ret { value } => Some(value),
            _ => None,
        })
        .unwrap_or_else(|| panic!("no return:\n{}", f.dump()));
    assert!(
        f.value_lanes(returned) <= 1,
        "the return is a vector:\n{}",
        f.dump()
    );
}

/// `acc += i * 3 + 1` — a reduction whose ADDEND varies with the induction.
///
/// The term is a scalar the widener has nothing to widen it from, so the only
/// way to feed it to a vector accumulator is to broadcast it -- and four
/// copies of the term at the lane-0 index is not the sum of the four terms.
/// TestTieredHotLoop returned 497032704 instead of 1198000000 exactly this
/// way, and the IR verified.
fn widen_varying_addend_fixture() -> (Vec<Opcode>, Vec<TypeRef>) {
    // r0 n r1 i r2 acc r3 step r4 three r5 one r6 tmp
    let regs = vec![t(3), t(3), t(3), t(3), t(3), t(3), t(3)];
    let ops = vec![
        Opcode::Int { dst: Reg(1), ptr: RefInt(0) },   // i = 0
        Opcode::Int { dst: Reg(2), ptr: RefInt(0) },   // acc = 0
        Opcode::Int { dst: Reg(3), ptr: RefInt(2) },   // step = 1
        Opcode::Int { dst: Reg(4), ptr: RefInt(4) },   // three... (2 here)
        Opcode::Int { dst: Reg(5), ptr: RefInt(2) },   // one
        Opcode::Int { dst: Reg(6), ptr: RefInt(0) },   // tmp = 0
        Opcode::Label,
        Opcode::JSGte { a: Reg(1), b: Reg(0), offset: 5 },
        Opcode::Mul { dst: Reg(6), a: Reg(1), b: Reg(4) },
        Opcode::Add { dst: Reg(6), a: Reg(6), b: Reg(5) },
        Opcode::Add { dst: Reg(2), a: Reg(2), b: Reg(6) },
        Opcode::Add { dst: Reg(1), a: Reg(1), b: Reg(3) },
        Opcode::JAlways { offset: -6 },
        Opcode::Ret { ret: Reg(2) },
    ];
    (ops, regs)
}

#[test]
fn a_reduction_over_a_varying_term_is_refused() {
    let (ops, regs) = widen_varying_addend_fixture();
    let mut f = lowered_and_cleaned(&ops, &regs);
    let pass = super::passes::widen::Widen { info: &WidenInfo };
    let stats = pass.run(&mut f, &PassOptions::default()).expect("widen");
    assert_eq!(
        stats.replaced,
        0,
        "widened a reduction whose addend changes every iteration:\n{}",
        f.dump()
    );
    assert!(
        !f.blocks
            .iter()
            .any(|b| b.instrs.iter().any(|i| matches!(i, Instr::VecSplat { .. }))),
        "a varying scalar was broadcast anyway:\n{}",
        f.dump()
    );
}

/// An element two machine-vectors wide, by four lanes, is not a vector any
/// backend can name -- and a backend handed one refuses the whole function,
/// which the ladder re-proposes forever. The game froze on `i64x4`. The
/// widener must decline it, whatever else about the loop is fine.
struct WideInfo;
impl ModuleInfo for WideInfo {
    fn int_value(&self, idx: usize) -> Option<i32> {
        WidenInfo.int_value(idx)
    }
    fn int_pool_len(&self) -> usize {
        WidenInfo.int_pool_len()
    }
    fn type_size(&self, _ty: TypeRef) -> Option<u32> {
        Some(8)
    }
}

#[test]
fn a_lane_wider_than_the_machine_vector_is_refused() {
    let (ops, regs) = widen_fixture();
    let mut f = lower_with(&ops, &regs, &WideInfo).expect("lower");
    let before = f.dump();
    let pass = super::passes::widen::Widen { info: &WideInfo };
    let stats = pass.run(&mut f, &PassOptions::default()).expect("widen");
    assert_eq!(stats.replaced, 0, "widened an 8-byte element by 4:\n{}", f.dump());
    assert_eq!(f.dump(), before, "a refusal must leave the function untouched");
    let why = super::passes::widen::take_outcomes();
    assert!(
        why.iter().any(|(_, r)| matches!(r, Err(super::passes::widen::Decline::LaneTooWide(_)))),
        "declined, but not for the width: {why:?}"
    );
}
