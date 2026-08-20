//! Serialization: [`Function`] -> standard HL bytecode (`Vec<Opcode>`).
//!
//! This is the path that breaks the v1 frozen-offset identity:
//!
//! 1. **De-SSA**: every value goes back to its originating HL register
//!    (`ValueData::reg`); phis become parallel copies on incoming edges.
//!    Non-trivial phis are supported — copy cycles are broken with fresh
//!    temporary registers appended to the register-type table, and critical
//!    edges are split with dedicated copy blocks.
//! 2. **Block layout**: blocks are emitted in id order; fall-through-
//!    constrained edges (`Trap` normal, `Switch` default) get their copy
//!    blocks placed immediately after the branching block, all other copy
//!    blocks are appended at the end and the branch retargeted.
//! 3. **Offset recomputation**: all jump offsets are recomputed from the
//!    final layout. Blocks targeted by backward (or self) jumps receive a
//!    `Label` opcode, as HL requires for negative offsets.
//!
//! The emitted opcodes are interpreter-compatible only: `CellGet`/`CellSet`
//! become `Mov`s (elided when source and destination registers coincide,
//! which is always the case for plain lowered functions), `CallMethod`
//! covers former `CallThis`, `FieldGet`/`FieldSet` cover former
//! `GetThis`/`SetThis`.
//!
//! ## `Fma` is asymmetric by design
//!
//! HL bytecode has no fused multiply-add opcode, so `Fma { dst, a, b, c }` is
//! emitted as `Mul tmp, a, b` + `Add dst, tmp, c` through a per-type
//! temporary register appended to the register-type table. That restores the
//! *unfused* arithmetic the input bytecode had — the interpreter path is
//! numerically identical to the pre-fusion program — while backends that
//! consume the IR directly emit a hardware FMA. `serialize(lower(ops))` is
//! therefore behaviour-preserving but not instruction-identical once the FMA
//! peephole has run.

use super::ir::*;
use crate::opcodes::{
    Opcode, RefBytes, RefEnumConstruct, RefField, RefFloat, RefFun, RefGlobal, RefInt, RefString,
    RefType, Reg,
};
use anyhow::{bail, Result};
use std::collections::{BTreeMap, HashSet};

/// Serialization result: standard HL bytecode plus the register-type table
/// (original registers preserved verbatim, copy temporaries appended).
#[derive(Debug, Clone)]
pub struct Serialized {
    pub ops: Vec<Opcode>,
    pub reg_types: Vec<TypeRef>,
    pub num_regs: usize,
    /// Opcode index where each block's emission begins, indexed by
    /// [`BlockId`]. For a loop header this is the pc its back-edges target
    /// (the `Label`, when one was required) — i.e. exactly the pc an
    /// interpreter running `ops` observes as the jump destination. This is
    /// what lets an OSR producer turn [`crate::v2::analysis`] block ids into
    /// entry sites without re-discovering headers by probe timing.
    pub block_pcs: Vec<usize>,
}

#[derive(Debug, Clone)]
enum Entry {
    /// A real block.
    Real(usize),
    /// An edge-copy block: sequential Movs then a jump to `target`.
    Copy {
        movs: Vec<(u32, u32)>,
        target: usize,
    },
}

pub fn serialize(f: &Function) -> Result<Serialized> {
    let nb = f.blocks.len();
    let mut reg_types = f.reg_types.clone();

    // ---- 1. collect parallel copies per (pred, succ) edge -----------------
    let mut edge_copies: BTreeMap<(usize, usize), Vec<(u32, u32)>> = BTreeMap::new();
    for (bi, blk) in f.blocks.iter().enumerate() {
        for phi in &blk.phis {
            let dreg = f.value_reg(phi.dst);
            for &(p, v) in &phi.incoming {
                let sreg = f.value_reg(v);
                if sreg != dreg {
                    edge_copies
                        .entry((p.idx(), bi))
                        .or_default()
                        .push((dreg, sreg));
                }
            }
        }
    }

    // Handler blocks can be entered exceptionally: no program point exists to
    // run copies there.
    let mut handler_blocks: HashSet<usize> = HashSet::new();
    for blk in &f.blocks {
        if let Some(h) = blk.handler {
            handler_blocks.insert(h.idx());
        }
        if let Terminator::Trap { handler, .. } = blk.term {
            handler_blocks.insert(handler.idx());
        }
    }
    for (&(p, s), copies) in &edge_copies {
        if !copies.is_empty() && handler_blocks.contains(&s) {
            bail!(
                "non-trivial phi at handler block b{} (edge from b{}): copies cannot \
                 be inserted on exceptional edges",
                s,
                p
            );
        }
    }

    // ---- 2. sequentialize parallel copies ---------------------------------
    let mut seq_copies: BTreeMap<(usize, usize), Vec<(u32, u32)>> = BTreeMap::new();
    for (&edge, par) in &edge_copies {
        let mut dsts: Vec<u32> = par.iter().map(|&(d, _)| d).collect();
        dsts.sort_unstable();
        dsts.dedup();
        if dsts.len() != par.len() {
            bail!(
                "edge b{} -> b{}: two phis assign the same register",
                edge.0,
                edge.1
            );
        }
        seq_copies.insert(edge, sequentialize(par, &mut reg_types));
    }

    // ---- 3. placement decisions -------------------------------------------
    let mut inline: Vec<Option<Vec<(u32, u32)>>> = vec![None; nb];
    // Fall-through-constrained copy block right after block p: (movs, succ).
    let mut fall_copy: Vec<Option<(Vec<(u32, u32)>, usize)>> = vec![None; nb];
    // Copy blocks appended at the end, keyed by edge.
    let mut end_copy: BTreeMap<(usize, usize), Vec<(u32, u32)>> = BTreeMap::new();

    for (&(p, s), movs) in &seq_copies {
        let term = &f.blocks[p].term;
        let distinct: HashSet<usize> = term.successors().iter().map(|b| b.idx()).collect();
        if distinct.len() == 1 {
            // Single successor: run copies at the end of p itself.
            inline[p] = Some(movs.clone());
            continue;
        }
        match term {
            Terminator::Trap { normal, .. } if normal.idx() == s => {
                fall_copy[p] = Some((movs.clone(), s));
            }
            Terminator::Switch {
                targets, default, ..
            } => {
                if default.idx() == s {
                    fall_copy[p] = Some((movs.clone(), s));
                }
                if targets.iter().any(|t| t.idx() == s) {
                    end_copy.insert((p, s), movs.clone());
                }
            }
            _ => {
                end_copy.insert((p, s), movs.clone());
            }
        }
    }

    // ---- 4. layout ---------------------------------------------------------
    let mut entries: Vec<Entry> = Vec::new();
    let mut entry_of_block: Vec<usize> = vec![usize::MAX; nb];
    let mut fall_entry: Vec<Option<usize>> = vec![None; nb];
    for b in 0..nb {
        entry_of_block[b] = entries.len();
        entries.push(Entry::Real(b));
        if let Some((movs, target)) = fall_copy[b].take() {
            fall_entry[b] = Some(entries.len());
            entries.push(Entry::Copy { movs, target });
        }
    }
    let mut end_entry: BTreeMap<(usize, usize), usize> = BTreeMap::new();
    for (&(p, s), movs) in &end_copy {
        end_entry.insert((p, s), entries.len());
        entries.push(Entry::Copy {
            movs: movs.clone(),
            target: s,
        });
    }
    let ne = entries.len();

    // Resolve the emission target of an edge.
    let resolve_jump = |p: usize, s: usize| -> usize {
        end_entry.get(&(p, s)).copied().unwrap_or(entry_of_block[s])
    };
    let resolve_fall = |p: usize, s: usize| -> usize {
        match fall_entry[p] {
            Some(e) => {
                if let Entry::Copy { target, .. } = &entries[e] {
                    if *target == s {
                        return e;
                    }
                }
                entry_of_block[s]
            }
            None => entry_of_block[s],
        }
    };

    // ---- 5. label pre-pass -------------------------------------------------
    // An entry needs a Label iff any edge into it originates at the same or a
    // later layout position (such edges are necessarily emitted as jumps with
    // offset <= -1... or are backward). Forward jumps never need one, but HL
    // requires every negative offset to target a Label.
    let mut need_label = vec![false; ne];
    {
        let mut edges: Vec<(usize, usize)> = Vec::new(); // (source pos, target entry)
        for (pos, entry) in entries.iter().enumerate() {
            match entry {
                Entry::Real(b) => match &f.blocks[*b].term {
                    Terminator::Jump { target } => {
                        edges.push((pos, resolve_jump(*b, target.idx())))
                    }
                    Terminator::CondJump {
                        if_true, if_false, ..
                    } => {
                        edges.push((pos, resolve_jump(*b, if_true.idx())));
                        edges.push((pos, resolve_jump(*b, if_false.idx())));
                    }
                    Terminator::Switch {
                        targets, default, ..
                    } => {
                        for t in targets {
                            edges.push((pos, resolve_jump(*b, t.idx())));
                        }
                        edges.push((pos, resolve_fall(*b, default.idx())));
                    }
                    Terminator::Trap {
                        handler, normal, ..
                    } => {
                        edges.push((pos, entry_of_block[handler.idx()]));
                        edges.push((pos, resolve_fall(*b, normal.idx())));
                    }
                    Terminator::Ret { .. }
                    | Terminator::Throw { .. }
                    | Terminator::Rethrow { .. } => {}
                },
                Entry::Copy { target, .. } => edges.push((pos, entry_of_block[*target])),
            }
        }
        for (src_pos, tgt) in edges {
            if src_pos >= tgt {
                need_label[tgt] = true;
            }
        }
    }

    // ---- 6. emission -------------------------------------------------------
    enum Site {
        /// Single-offset opcode at `idx` targeting `entry`.
        Simple {
            idx: usize,
            entry: usize,
        },
        SwitchCase {
            idx: usize,
            case: usize,
            entry: usize,
        },
        SwitchEnd {
            idx: usize,
            entry: usize,
        },
    }
    let mut ops: Vec<Opcode> = Vec::new();
    let mut starts = vec![0usize; ne];
    let mut sites: Vec<Site> = Vec::new();

    let rg = |v: ValueId| Reg(f.value_reg(v));
    let cr = |c: CellId| Reg(f.cells[c.idx()].reg);
    // One scratch register per float type, holding the product while `Fma` is
    // split back into `Mul` + `Add`. Fresh registers are never read by
    // anything else, so the split is correct for every operand aliasing.
    let mut fma_temps: BTreeMap<TypeRef, u32> = BTreeMap::new();

    for (pos, entry) in entries.iter().enumerate() {
        starts[pos] = ops.len();
        if need_label[pos] {
            ops.push(Opcode::Label);
        }
        let next = pos + 1;
        match entry {
            Entry::Copy { movs, target } => {
                for &(d, s) in movs {
                    if d != s {
                        ops.push(Opcode::Mov {
                            dst: Reg(d),
                            src: Reg(s),
                        });
                    }
                }
                let e = entry_of_block[*target];
                if e != next {
                    sites.push(Site::Simple {
                        idx: ops.len(),
                        entry: e,
                    });
                    ops.push(Opcode::JAlways { offset: 0 });
                }
            }
            Entry::Real(b) => {
                let blk = &f.blocks[*b];
                for ins in &blk.instrs {
                    emit_instr(f, ins, &rg, &cr, &mut ops, &mut reg_types, &mut fma_temps)?;
                }
                if let Some(movs) = &inline[*b] {
                    for &(d, s) in movs {
                        if d != s {
                            ops.push(Opcode::Mov {
                                dst: Reg(d),
                                src: Reg(s),
                            });
                        }
                    }
                }
                match &blk.term {
                    Terminator::Ret { value } => ops.push(Opcode::Ret { ret: rg(*value) }),
                    Terminator::Throw { exc } => ops.push(Opcode::Throw { exc: rg(*exc) }),
                    Terminator::Rethrow { exc } => ops.push(Opcode::Rethrow { exc: rg(*exc) }),
                    Terminator::Jump { target } => {
                        let e = resolve_jump(*b, target.idx());
                        if e != next {
                            sites.push(Site::Simple {
                                idx: ops.len(),
                                entry: e,
                            });
                            ops.push(Opcode::JAlways { offset: 0 });
                        }
                    }
                    Terminator::CondJump {
                        cond,
                        a,
                        b: rhs,
                        if_true,
                        if_false,
                    } => {
                        let et = resolve_jump(*b, if_true.idx());
                        sites.push(Site::Simple {
                            idx: ops.len(),
                            entry: et,
                        });
                        ops.push(cond_opcode(*cond, rg(*a), (*rhs).map(&rg))?);
                        let ef = resolve_jump(*b, if_false.idx());
                        if ef != next {
                            sites.push(Site::Simple {
                                idx: ops.len(),
                                entry: ef,
                            });
                            ops.push(Opcode::JAlways { offset: 0 });
                        }
                    }
                    Terminator::Switch {
                        value,
                        targets,
                        default,
                    } => {
                        let idx = ops.len();
                        for (k, t) in targets.iter().enumerate() {
                            sites.push(Site::SwitchCase {
                                idx,
                                case: k,
                                entry: resolve_jump(*b, t.idx()),
                            });
                        }
                        let ed = resolve_fall(*b, default.idx());
                        sites.push(Site::SwitchEnd { idx, entry: ed });
                        ops.push(Opcode::Switch {
                            reg: rg(*value),
                            offsets: vec![0; targets.len()],
                            end: 0,
                        });
                        // HL Switch falls through on out-of-range values: the
                        // default must be the next opcode. Trampoline if the
                        // layout says otherwise.
                        if ed != next {
                            sites.push(Site::Simple {
                                idx: ops.len(),
                                entry: ed,
                            });
                            ops.push(Opcode::JAlways { offset: 0 });
                        }
                    }
                    Terminator::Trap {
                        exc_cell,
                        handler,
                        normal,
                    } => {
                        sites.push(Site::Simple {
                            idx: ops.len(),
                            entry: entry_of_block[handler.idx()],
                        });
                        ops.push(Opcode::Trap {
                            exc: cr(*exc_cell),
                            offset: 0,
                        });
                        let en = resolve_fall(*b, normal.idx());
                        if en != next {
                            sites.push(Site::Simple {
                                idx: ops.len(),
                                entry: en,
                            });
                            ops.push(Opcode::JAlways { offset: 0 });
                        }
                    }
                }
            }
        }
    }

    // ---- 7. offset patching ------------------------------------------------
    for site in sites {
        match site {
            Site::Simple { idx, entry } => {
                let off = starts[entry] as i32 - (idx as i32 + 1);
                match &mut ops[idx] {
                    Opcode::JAlways { offset }
                    | Opcode::JTrue { offset, .. }
                    | Opcode::JFalse { offset, .. }
                    | Opcode::JNull { offset, .. }
                    | Opcode::JNotNull { offset, .. }
                    | Opcode::JSLt { offset, .. }
                    | Opcode::JSGte { offset, .. }
                    | Opcode::JSGt { offset, .. }
                    | Opcode::JSLte { offset, .. }
                    | Opcode::JULt { offset, .. }
                    | Opcode::JUGte { offset, .. }
                    | Opcode::JNotLt { offset, .. }
                    | Opcode::JNotGte { offset, .. }
                    | Opcode::JEq { offset, .. }
                    | Opcode::JNotEq { offset, .. }
                    | Opcode::Trap { offset, .. } => *offset = off,
                    other => bail!("patch site {} is not a jump opcode: {:?}", idx, other),
                }
            }
            Site::SwitchCase { idx, case, entry } => {
                let off = starts[entry] as i32 - (idx as i32 + 1);
                match &mut ops[idx] {
                    Opcode::Switch { offsets, .. } => offsets[case] = off,
                    other => bail!("patch site {} is not a Switch: {:?}", idx, other),
                }
            }
            Site::SwitchEnd { idx, entry } => {
                let off = starts[entry] as i32 - (idx as i32 + 1);
                match &mut ops[idx] {
                    Opcode::Switch { end, .. } => *end = off,
                    other => bail!("patch site {} is not a Switch: {:?}", idx, other),
                }
            }
        }
    }

    let num_regs = reg_types.len();
    let block_pcs = entry_of_block.iter().map(|&e| starts[e]).collect();
    Ok(Serialized {
        ops,
        reg_types,
        num_regs,
        block_pcs,
    })
}

/// Sequentialize a parallel copy set. Cycles are broken with a fresh
/// temporary register carrying the shadowed register's type.
fn sequentialize(par: &[(u32, u32)], reg_types: &mut Vec<TypeRef>) -> Vec<(u32, u32)> {
    let mut pending: Vec<(u32, u32)> = par.to_vec();
    let mut out = Vec::with_capacity(pending.len());
    while !pending.is_empty() {
        if let Some(i) = pending
            .iter()
            .position(|&(d, _)| !pending.iter().any(|&(_, s)| s == d))
        {
            let (d, s) = pending.remove(i);
            out.push((d, s));
        } else {
            // Every destination is still needed as a source: break a cycle by
            // saving one destination into a temp.
            let (d, _) = pending[0];
            let t = reg_types.len() as u32;
            reg_types.push(reg_types[d as usize]);
            out.push((t, d));
            for (_, s) in pending.iter_mut() {
                if *s == d {
                    *s = t;
                }
            }
        }
    }
    out
}

fn cond_opcode(cond: CondKind, a: Reg, b: Option<Reg>) -> Result<Opcode> {
    let bin = |b: Option<Reg>| -> Result<Reg> {
        b.ok_or_else(|| anyhow::anyhow!("binary CondKind missing second operand"))
    };
    Ok(match cond {
        CondKind::True => Opcode::JTrue { cond: a, offset: 0 },
        CondKind::False => Opcode::JFalse { cond: a, offset: 0 },
        CondKind::Null => Opcode::JNull { reg: a, offset: 0 },
        CondKind::NotNull => Opcode::JNotNull { reg: a, offset: 0 },
        CondKind::SLt => Opcode::JSLt {
            a,
            b: bin(b)?,
            offset: 0,
        },
        CondKind::SGte => Opcode::JSGte {
            a,
            b: bin(b)?,
            offset: 0,
        },
        CondKind::SGt => Opcode::JSGt {
            a,
            b: bin(b)?,
            offset: 0,
        },
        CondKind::SLte => Opcode::JSLte {
            a,
            b: bin(b)?,
            offset: 0,
        },
        CondKind::ULt => Opcode::JULt {
            a,
            b: bin(b)?,
            offset: 0,
        },
        CondKind::UGte => Opcode::JUGte {
            a,
            b: bin(b)?,
            offset: 0,
        },
        CondKind::NotLt => Opcode::JNotLt {
            a,
            b: bin(b)?,
            offset: 0,
        },
        CondKind::NotGte => Opcode::JNotGte {
            a,
            b: bin(b)?,
            offset: 0,
        },
        CondKind::Eq => Opcode::JEq {
            a,
            b: bin(b)?,
            offset: 0,
        },
        CondKind::NotEq => Opcode::JNotEq {
            a,
            b: bin(b)?,
            offset: 0,
        },
    })
}

fn emit_instr(
    f: &Function,
    ins: &Instr,
    rg: &dyn Fn(ValueId) -> Reg,
    cr: &dyn Fn(CellId) -> Reg,
    ops: &mut Vec<Opcode>,
    reg_types: &mut Vec<TypeRef>,
    fma_temps: &mut BTreeMap<TypeRef, u32>,
) -> Result<()> {
    match ins {
        Instr::Param { .. } => {}
        // Back to the direct native call the bytecode had — the flat form
        // has no intrinsic opcode, the same round-trip Fma takes.
        Instr::Intrinsic { fun, dst, args, .. } => {
            let d = rg(*dst);
            match args.len() {
                1 => ops.push(Opcode::Call1 {
                    dst: d,
                    fun: crate::opcodes::RefFun(*fun),
                    arg0: rg(args[0]),
                }),
                2 => ops.push(Opcode::Call2 {
                    dst: d,
                    fun: crate::opcodes::RefFun(*fun),
                    arg0: rg(args[0]),
                    arg1: rg(args[1]),
                }),
                n => bail!("intrinsic with {n} args has no serialization"),
            }
        }
        Instr::Copy { dst, src } => {
            let (d, s) = (rg(*dst), rg(*src));
            if d != s {
                ops.push(Opcode::Mov { dst: d, src: s });
            }
        }
        Instr::Int { dst, idx } => ops.push(Opcode::Int {
            dst: rg(*dst),
            ptr: RefInt(*idx),
        }),
        Instr::Float { dst, idx } => ops.push(Opcode::Float {
            dst: rg(*dst),
            ptr: RefFloat(*idx),
        }),
        Instr::Bool { dst, value } => ops.push(Opcode::Bool {
            dst: rg(*dst),
            value: *value,
        }),
        Instr::Bytes { dst, idx } => ops.push(Opcode::Bytes {
            dst: rg(*dst),
            ptr: RefBytes(*idx),
        }),
        Instr::String { dst, idx } => ops.push(Opcode::String {
            dst: rg(*dst),
            ptr: RefString(*idx),
        }),
        Instr::Null { dst } => ops.push(Opcode::Null { dst: rg(*dst) }),
        Instr::BinOp { op, dst, a, b } => {
            let (dst, a, b) = (rg(*dst), rg(*a), rg(*b));
            ops.push(match op {
                BinOp::Add => Opcode::Add { dst, a, b },
                BinOp::Sub => Opcode::Sub { dst, a, b },
                BinOp::Mul => Opcode::Mul { dst, a, b },
                BinOp::SDiv => Opcode::SDiv { dst, a, b },
                BinOp::UDiv => Opcode::UDiv { dst, a, b },
                BinOp::SMod => Opcode::SMod { dst, a, b },
                BinOp::UMod => Opcode::UMod { dst, a, b },
                BinOp::Shl => Opcode::Shl { dst, a, b },
                BinOp::SShr => Opcode::SShr { dst, a, b },
                BinOp::UShr => Opcode::UShr { dst, a, b },
                BinOp::And => Opcode::And { dst, a, b },
                BinOp::Or => Opcode::Or { dst, a, b },
                BinOp::Xor => Opcode::Xor { dst, a, b },
            });
        }
        Instr::Fma { dst, a, b, c } => {
            let ty = f.value_ty(*dst);
            let tmp = *fma_temps.entry(ty).or_insert_with(|| {
                let r = reg_types.len() as u32;
                reg_types.push(ty);
                r
            });
            ops.push(Opcode::Mul {
                dst: Reg(tmp),
                a: rg(*a),
                b: rg(*b),
            });
            ops.push(Opcode::Add {
                dst: rg(*dst),
                a: Reg(tmp),
                b: rg(*c),
            });
        }
        Instr::UnOp { op, dst, src } => {
            let (dst, src) = (rg(*dst), rg(*src));
            match op {
                UnOp::Neg => ops.push(Opcode::Neg { dst, src }),
                UnOp::Not => ops.push(Opcode::Not { dst, src }),
                // HL's Incr/Decr read and write one register, so they can only
                // be emitted directly when de-SSA landed both ends in the same
                // one. Otherwise the move has to be made explicit first.
                UnOp::Incr | UnOp::Decr => {
                    if dst != src {
                        ops.push(Opcode::Mov { dst, src });
                    }
                    ops.push(if matches!(op, UnOp::Incr) {
                        Opcode::Incr { dst }
                    } else {
                        Opcode::Decr { dst }
                    });
                }
            }
        }
        Instr::Call { dst, fun, args } => {
            let dst = rg(*dst);
            let fun = RefFun(*fun);
            let a: Vec<Reg> = args.iter().map(|v| rg(*v)).collect();
            ops.push(match a.len() {
                0 => Opcode::Call0 { dst, fun },
                1 => Opcode::Call1 {
                    dst,
                    fun,
                    arg0: a[0],
                },
                2 => Opcode::Call2 {
                    dst,
                    fun,
                    arg0: a[0],
                    arg1: a[1],
                },
                3 => Opcode::Call3 {
                    dst,
                    fun,
                    arg0: a[0],
                    arg1: a[1],
                    arg2: a[2],
                },
                4 => Opcode::Call4 {
                    dst,
                    fun,
                    arg0: a[0],
                    arg1: a[1],
                    arg2: a[2],
                    arg3: a[3],
                },
                _ => Opcode::CallN { dst, fun, args: a },
            });
        }
        Instr::CallMethod { dst, field, args } => ops.push(Opcode::CallMethod {
            dst: rg(*dst),
            field: RefField(*field),
            args: args.iter().map(|v| rg(*v)).collect(),
        }),
        Instr::CallClosure { dst, fun, args } => ops.push(Opcode::CallClosure {
            dst: rg(*dst),
            fun: rg(*fun),
            args: args.iter().map(|v| rg(*v)).collect(),
        }),
        Instr::StaticClosure { dst, fun } => ops.push(Opcode::StaticClosure {
            dst: rg(*dst),
            fun: RefFun(*fun),
        }),
        Instr::InstanceClosure { dst, fun, obj } => ops.push(Opcode::InstanceClosure {
            dst: rg(*dst),
            fun: RefFun(*fun),
            obj: rg(*obj),
        }),
        Instr::VirtualClosure { dst, obj, field } => ops.push(Opcode::VirtualClosure {
            dst: rg(*dst),
            obj: rg(*obj),
            field: Reg(*field as u32),
        }),
        Instr::GetGlobal { dst, global } => ops.push(Opcode::GetGlobal {
            dst: rg(*dst),
            global: RefGlobal(*global),
        }),
        Instr::SetGlobal { global, src } => ops.push(Opcode::SetGlobal {
            global: RefGlobal(*global),
            src: rg(*src),
        }),
        Instr::FieldGet {
            dst, obj, field, ..
        } => ops.push(Opcode::Field {
            dst: rg(*dst),
            obj: rg(*obj),
            field: RefField(*field),
        }),
        Instr::FieldSet {
            obj, field, src, ..
        } => ops.push(Opcode::SetField {
            obj: rg(*obj),
            field: RefField(*field),
            src: rg(*src),
        }),
        Instr::DynGet { dst, obj, field } => ops.push(Opcode::DynGet {
            dst: rg(*dst),
            obj: rg(*obj),
            field: RefString(*field),
        }),
        Instr::DynSet { obj, field, src } => ops.push(Opcode::DynSet {
            obj: rg(*obj),
            field: RefString(*field),
            src: rg(*src),
        }),
        Instr::Cast { kind, dst, src } => {
            let (dst, src) = (rg(*dst), rg(*src));
            ops.push(match kind {
                CastKind::ToDyn => Opcode::ToDyn { dst, src },
                CastKind::ToSFloat => Opcode::ToSFloat { dst, src },
                CastKind::ToUFloat => Opcode::ToUFloat { dst, src },
                CastKind::ToInt => Opcode::ToInt { dst, src },
                CastKind::SafeCast => Opcode::SafeCast { dst, src },
                CastKind::UnsafeCast => Opcode::UnsafeCast { dst, src },
                CastKind::ToVirtual => Opcode::ToVirtual { dst, src },
            });
        }
        Instr::NullCheck { value } => ops.push(Opcode::NullCheck { reg: rg(*value) }),
        // The operand is the bool flag it came in as, never a register: see
        // `Instr::EndTrap`. `cell` is IR bookkeeping and is not encoded.
        Instr::EndTrap { flag, .. } => ops.push(Opcode::EndTrap {
            exc: Reg(*flag as u32),
        }),
        Instr::MemGet {
            kind,
            dst,
            base,
            index,
        } => {
            let (dst, base, index) = (rg(*dst), rg(*base), rg(*index));
            ops.push(match kind {
                MemAccess::I8 => Opcode::GetI8 {
                    dst,
                    bytes: base,
                    index,
                },
                MemAccess::I16 => Opcode::GetI16 {
                    dst,
                    bytes: base,
                    index,
                },
                MemAccess::Mem => Opcode::GetMem {
                    dst,
                    bytes: base,
                    index,
                },
                MemAccess::Array => Opcode::GetArray {
                    dst,
                    array: base,
                    index,
                },
            });
        }
        Instr::MemSet {
            kind,
            base,
            index,
            src,
        } => {
            let (base, index, src) = (rg(*base), rg(*index), rg(*src));
            ops.push(match kind {
                MemAccess::I8 => Opcode::SetI8 {
                    bytes: base,
                    index,
                    src,
                },
                MemAccess::I16 => Opcode::SetI16 {
                    bytes: base,
                    index,
                    src,
                },
                MemAccess::Mem => Opcode::SetMem {
                    bytes: base,
                    index,
                    src,
                },
                MemAccess::Array => Opcode::SetArray {
                    array: base,
                    index,
                    src,
                },
            });
        }
        Instr::New { dst } => ops.push(Opcode::New { dst: rg(*dst) }),
        Instr::ArraySize { dst, array } => ops.push(Opcode::ArraySize {
            dst: rg(*dst),
            array: rg(*array),
        }),
        Instr::TypeConst { dst, ty } => ops.push(Opcode::Type {
            dst: rg(*dst),
            ty: RefType(ty.0 as usize),
        }),
        Instr::GetType { dst, src } => ops.push(Opcode::GetType {
            dst: rg(*dst),
            src: rg(*src),
        }),
        Instr::GetTID { dst, src } => ops.push(Opcode::GetTID {
            dst: rg(*dst),
            src: rg(*src),
        }),
        Instr::Unref { dst, src } => ops.push(Opcode::Unref {
            dst: rg(*dst),
            src: rg(*src),
        }),
        Instr::SetRef { r, value } => ops.push(Opcode::Setref {
            dst: rg(*r),
            value: rg(*value),
        }),
        Instr::RefData { dst, src } => ops.push(Opcode::RefData {
            dst: rg(*dst),
            src: rg(*src),
        }),
        Instr::RefOffset { dst, base, offset } => ops.push(Opcode::RefOffset {
            dst: rg(*dst),
            reg: rg(*base),
            offset: rg(*offset),
        }),
        Instr::MakeEnum {
            dst,
            construct,
            args,
        } => ops.push(Opcode::MakeEnum {
            dst: rg(*dst),
            construct: RefEnumConstruct(*construct),
            args: args.iter().map(|v| rg(*v)).collect(),
        }),
        Instr::EnumAlloc { dst, construct } => ops.push(Opcode::EnumAlloc {
            dst: rg(*dst),
            construct: RefEnumConstruct(*construct),
        }),
        Instr::EnumIndex { dst, value } => ops.push(Opcode::EnumIndex {
            dst: rg(*dst),
            value: rg(*value),
        }),
        Instr::EnumField {
            dst,
            value,
            construct,
            field,
        } => ops.push(Opcode::EnumField {
            dst: rg(*dst),
            value: rg(*value),
            construct: RefEnumConstruct(*construct),
            field: RefField(*field),
        }),
        Instr::SetEnumField {
            value, field, src, ..
        } => ops.push(Opcode::SetEnumField {
            value: rg(*value),
            field: RefField(*field),
            src: rg(*src),
        }),
        Instr::CellGet { dst, cell } => {
            let (d, s) = (rg(*dst), cr(*cell));
            if d != s {
                ops.push(Opcode::Mov { dst: d, src: s });
            }
        }
        Instr::CellSet { cell, src } => {
            let (d, s) = (cr(*cell), rg(*src));
            if d != s {
                ops.push(Opcode::Mov { dst: d, src: s });
            }
        }
        Instr::CellIncr { cell } => ops.push(Opcode::Incr { dst: cr(*cell) }),
        Instr::CellDecr { cell } => ops.push(Opcode::Decr { dst: cr(*cell) }),
        Instr::CellRef { dst, cell } => ops.push(Opcode::Ref {
            dst: rg(*dst),
            src: cr(*cell),
        }),
        Instr::Assert => ops.push(Opcode::Assert),
        Instr::Prefetch { value, field, mode } => ops.push(Opcode::Prefetch {
            value: rg(*value),
            field: RefField(*field),
            mode: *mode,
        }),
        Instr::Asm { mode, value, reg } => ops.push(Opcode::Asm {
            mode: *mode,
            value: *value,
            reg: Reg(*reg),
        }),
    }
    Ok(())
}

impl Function {
    /// Convenience: `serialize(self)`.
    pub fn to_opcodes(&self) -> Result<Serialized> {
        serialize(self)
    }
}
