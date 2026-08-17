//! Dominator-scoped global value numbering / common-subexpression
//! elimination.

use super::{
    clobber_free, compact_values, def_sites, feeds_handler_phi, handler_blocks, param_values,
    privatize, replace_all_uses, DefSite, Pass, PassOptions, PassStats, RegClaims,
};
use crate::v2::analysis::{read_class, AliasClass, CfgInfo};
use crate::v2::ir::*;
use anyhow::Result;
use std::collections::{HashMap, HashSet};

/// Replaces recomputations with the dominating value that already holds the
/// result.
///
/// Guarantees:
/// * only `Pure` instructions and `ReadMem` loads are numbered; anything that
///   writes, allocates, throws or clobbers is left alone;
/// * a `ReadMem` load is reused only when no instruction on **any** path from
///   the dominating load to the candidate — loop back edges included — may
///   write a storage class that aliases it, and nothing on those paths
///   clobbers all memory (see [`clobber_free`]);
/// * commutative arithmetic is numbered up to operand order, which is exact
///   for IEEE addition and multiplication as well as for the bitwise
///   operations;
/// * a value whose uses are extended first gets a private register
///   ([`privatize`]); when that is impossible the replacement is refused;
/// * values feeding a phi of a handler block are never rewritten, keeping
///   those phis trivial.
///
/// `Copy` is folded here too: a copy's destination is numbered as its source,
/// so copy propagation falls out of the same walk.
pub struct GlobalValueNumbering;

type Key = (u32, Vec<u64>);

const TAG_INT: u32 = 1;
const TAG_FLOAT: u32 = 2;
const TAG_BOOL: u32 = 3;
const TAG_BYTES: u32 = 4;
const TAG_STRING: u32 = 5;
const TAG_NULL: u32 = 6;
const TAG_BINOP: u32 = 7;
const TAG_UNOP: u32 = 8;
const TAG_CAST: u32 = 9;
const TAG_TYPE: u32 = 10;
const TAG_REFOFFSET: u32 = 11;
const TAG_FMA: u32 = 12;
const TAG_CELLREF: u32 = 13;
const TAG_FIELDGET: u32 = 20;
const TAG_MEMGET: u32 = 21;
const TAG_ARRAYSIZE: u32 = 22;
const TAG_GETGLOBAL: u32 = 23;
const TAG_ENUMFIELD: u32 = 24;
const TAG_CELLGET: u32 = 25;
const TAG_ENUMINDEX: u32 = 26;
const TAG_GETTYPE: u32 = 27;
const TAG_GETTID: u32 = 28;

fn commutative(op: BinOp) -> bool {
    matches!(
        op,
        BinOp::Add | BinOp::Mul | BinOp::And | BinOp::Or | BinOp::Xor
    )
}

/// The value-number key of an instruction, or `None` when it must not be
/// numbered.
fn key_of(ins: &Instr) -> Option<Key> {
    let v = |x: ValueId| x.0 as u64;
    Some(match ins {
        Instr::Int { idx, .. } => (TAG_INT, vec![*idx as u64]),
        Instr::Float { idx, .. } => (TAG_FLOAT, vec![*idx as u64]),
        Instr::Bool { value, .. } => (TAG_BOOL, vec![*value as u64]),
        Instr::Bytes { idx, .. } => (TAG_BYTES, vec![*idx as u64]),
        Instr::String { idx, .. } => (TAG_STRING, vec![*idx as u64]),
        Instr::Null { .. } => (TAG_NULL, vec![]),
        Instr::BinOp { op, a, b, .. } => {
            if op.can_trap() {
                return None;
            }
            let (x, y) = if commutative(*op) && v(*a) > v(*b) {
                (v(*b), v(*a))
            } else {
                (v(*a), v(*b))
            };
            (TAG_BINOP, vec![*op as u64, x, y])
        }
        Instr::UnOp { op, src, .. } => (TAG_UNOP, vec![*op as u64, v(*src)]),
        Instr::Fma { a, b, c, .. } => {
            let (x, y) = if v(*a) > v(*b) {
                (v(*b), v(*a))
            } else {
                (v(*a), v(*b))
            };
            (TAG_FMA, vec![x, y, v(*c)])
        }
        Instr::Cast { kind, src, .. } => match kind {
            CastKind::ToSFloat | CastKind::ToUFloat | CastKind::ToInt | CastKind::UnsafeCast => {
                (TAG_CAST, vec![*kind as u64, v(*src)])
            }
            _ => return None,
        },
        Instr::TypeConst { ty, .. } => (TAG_TYPE, vec![ty.0 as u64]),
        Instr::RefOffset { base, offset, .. } => (TAG_REFOFFSET, vec![v(*base), v(*offset)]),
        Instr::CellRef { cell, .. } => (TAG_CELLREF, vec![cell.0 as u64]),
        Instr::FieldGet {
            obj, obj_ty, field, ..
        } => (TAG_FIELDGET, vec![v(*obj), obj_ty.0 as u64, *field as u64]),
        Instr::MemGet {
            kind, base, index, ..
        } => (TAG_MEMGET, vec![*kind as u64, v(*base), v(*index)]),
        Instr::ArraySize { array, .. } => (TAG_ARRAYSIZE, vec![v(*array)]),
        Instr::GetGlobal { global, .. } => (TAG_GETGLOBAL, vec![*global as u64]),
        Instr::EnumField {
            value,
            construct,
            field,
            ..
        } => (
            TAG_ENUMFIELD,
            vec![v(*value), *construct as u64, *field as u64],
        ),
        Instr::EnumIndex { value, .. } => (TAG_ENUMINDEX, vec![v(*value)]),
        Instr::CellGet { cell, .. } => (TAG_CELLGET, vec![cell.0 as u64]),
        Instr::GetType { src, .. } => (TAG_GETTYPE, vec![v(*src)]),
        Instr::GetTID { src, .. } => (TAG_GETTID, vec![v(*src)]),
        _ => return None,
    })
}

impl Pass for GlobalValueNumbering {
    fn name(&self) -> &'static str {
        "gvn"
    }

    fn run(&self, f: &mut Function, _opts: &PassOptions) -> Result<PassStats> {
        let mut stats = PassStats::default();
        let cfg = CfgInfo::build(f);
        let defs = def_sites(f);
        let handlers = handler_blocks(f);
        let is_param = param_values(f);
        let mut claims = RegClaims::build(f);

        // Replacements to apply, and the instructions they make redundant.
        let mut rewrites: Vec<(ValueId, ValueId)> = Vec::new();
        let mut remove: HashSet<(usize, usize)> = HashSet::new();
        // Value substitutions decided so far (for chained copies).
        let mut subst: HashMap<ValueId, ValueId> = HashMap::new();
        let resolve = |subst: &HashMap<ValueId, ValueId>, mut v: ValueId| -> ValueId {
            for _ in 0..64 {
                match subst.get(&v) {
                    Some(&n) if n != v => v = n,
                    _ => break,
                }
            }
            v
        };

        let mut table: HashMap<Key, ValueId> = HashMap::new();
        enum Item {
            Visit(usize),
            Undo(Vec<Key>),
        }
        let mut walk = vec![Item::Visit(cfg.dom.rpo[0])];
        while let Some(item) = walk.pop() {
            let b = match item {
                Item::Undo(keys) => {
                    for k in keys {
                        table.remove(&k);
                    }
                    continue;
                }
                Item::Visit(b) => b,
            };
            let mut added: Vec<Key> = Vec::new();
            for k in 0..f.blocks[b].instrs.len() {
                let ins = f.blocks[b].instrs[k].clone();
                let Some(dst) = ins.dst() else { continue };

                // Copy propagation: the destination is just another name.
                if let Instr::Copy { src, .. } = ins {
                    let src = resolve(&subst, src);
                    if can_replace(f, &handlers, dst)
                        && can_replace(f, &handlers, src)
                        && privatize(f, src, &mut claims, is_param[src.idx()])
                    {
                        subst.insert(dst, src);
                        rewrites.push((dst, src));
                        remove.insert((b, k));
                    }
                    continue;
                }

                let eff = ins.effect();
                if eff > Effect::ReadMem || ins.may_throw() {
                    continue;
                }
                // Operands may already be scheduled for substitution.
                let mut normalized = ins.clone();
                normalized.map_uses(&mut |v| resolve(&subst, v));
                let Some(key) = key_of(&normalized) else {
                    continue;
                };

                if let Some(&prev) = table.get(&key) {
                    if f.value_ty(prev) == f.value_ty(dst)
                        && can_replace(f, &handlers, dst)
                        && can_replace(f, &handlers, prev)
                    {
                        let ok = match (eff, read_class(&normalized)) {
                            (Effect::ReadMem, Some(class)) => {
                                dominating_load_is_live(f, &cfg, &defs, class, prev, (b, k))
                            }
                            (Effect::ReadMem, None) => false,
                            _ => true,
                        };
                        if ok && privatize(f, prev, &mut claims, is_param[prev.idx()]) {
                            subst.insert(dst, prev);
                            rewrites.push((dst, prev));
                            remove.insert((b, k));
                            continue;
                        }
                    }
                }
                if table.insert(key.clone(), dst).is_none() {
                    added.push(key);
                }
            }
            walk.push(Item::Undo(added));
            for &child in cfg.dom.dom_children[b].iter().rev() {
                walk.push(Item::Visit(child));
            }
        }

        if rewrites.is_empty() {
            return Ok(stats);
        }
        for (from, to) in rewrites {
            let to = resolve(&subst, to);
            stats.replaced += replace_all_uses(f, from, to);
        }
        for b in 0..f.blocks.len() {
            let mut k = 0usize;
            f.blocks[b].instrs.retain(|_| {
                let keep = !remove.contains(&(b, k));
                k += 1;
                keep
            });
        }
        stats.eliminated = remove.len();
        compact_values(f)?;
        Ok(stats)
    }
}

fn can_replace(f: &Function, handlers: &[bool], v: ValueId) -> bool {
    !feeds_handler_phi(f, handlers, v)
}

/// True when the value produced by the dominating load still holds at the
/// candidate's position.
fn dominating_load_is_live(
    f: &Function,
    cfg: &CfgInfo,
    defs: &[Option<DefSite>],
    class: AliasClass,
    prev: ValueId,
    at: (usize, usize),
) -> bool {
    let Some(site) = defs[prev.idx()] else {
        return false;
    };
    let Some(pk) = site.instr_idx() else {
        return false;
    };
    clobber_free(
        f,
        cfg,
        class,
        (site.block, pk),
        (BlockId(at.0 as u32), at.1),
    )
}
