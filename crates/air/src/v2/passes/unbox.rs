//! Unboxing of a nullable integer that is only compared and read back.
//!
//! `String.charCodeAt` answers `Null<Int>`: a boxed integer (read as an
//! unsigned 16-bit unit), or null past the end. Inlined into a scanning loop
//! it leaves
//!
//! ```text
//! c = phi [ null, ToDyn(x) ]
//! ... Eq c, ToDyn(62) ...      ... SafeCast c -> int ...
//! ```
//!
//! and every iteration allocates a box, calls the dynamic comparison and
//! calls the dynamic cast back. When nothing else reads the phi, the box is
//! pure overhead: the phi can carry the integer itself, with null stood for
//! by -1, a value no `x` can take.
//!
//! * `Eq`/`NotEq` against `ToDyn(k)` becomes an integer compare against `k`.
//!   It is exact only if `k` is not -1, so `k` has to be a constant that is
//!   known and different.
//! * A `SafeCast` to int becomes `q & !(q >> 31)`: the integer, or 0 for the
//!   -1 that stands for null, which is what the dynamic cast answers for
//!   null.
//! * `Null`/`NotNull` becomes a compare against -1.
//! * A source narrower than i32 is widened by `ToInt` right after its
//!   definition; the unsigned 8/16-bit values that can be boxed here are
//!   never -1 either.
//!
//! A phi with no null edge (the bounds check was already proven away, which
//! leaves a phi of one box) needs no sentinel: it carries the integer, compares
//! against any integer, and a cast back is a copy.
//!
//! Anything else reading the phi (a store, a call, another phi) leaves it as
//! it is. The values the new phi carries are given registers of their own,
//! since their live ranges now reach the phi.
//!
//! `ASH_NO_UNBOX=1` leaves the pass inert.

use super::{
    Pass, PassOptions, PassStats, RegClaims, def_sites, handler_blocks, param_values, privatize,
};
use crate::v2::ir::*;
use crate::v2::module::ModuleInfo;
use anyhow::Result;
use std::collections::HashMap;

pub struct UnboxNullableInt<'a> {
    pub info: &'a dyn ModuleInfo,
}

fn enabled() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var_os("ASH_NO_UNBOX").is_none())
}

/// What one incoming edge of the phi carries.
enum Edge {
    Null,
    /// An i32 box: its source goes into the new phi as it is.
    Value(ValueId),
    /// An unsigned 8/16-bit box: its source is widened first.
    Widen(ValueId),
}

/// One read of the phi that the rewrite knows how to redo on integers.
enum Read {
    /// `Cast SafeCast` to an int, naming the value it defines.
    Int { block: usize, dst: ValueId },
    /// `Eq`/`NotEq` against a boxed constant `k`.
    Compare {
        block: usize,
        k: ValueId,
        phi_first: bool,
    },
    /// `Null`/`NotNull` on the phi.
    IsNull { block: usize },
}

struct Candidate {
    block: usize,
    phi: ValueId,
    /// Whether any edge is null, and so whether -1 stands for something.
    has_null: bool,
    edges: Vec<(BlockId, Edge)>,
    reads: Vec<Read>,
    rejected: bool,
}

impl Candidate {
    /// The i32 type of the new phi, taken from a read that produces or
    /// consumes one.
    fn int_type(&self, f: &Function) -> Option<TypeRef> {
        self.reads.iter().find_map(|r| match r {
            Read::Int { dst, .. } => Some(f.value_ty(*dst)),
            Read::Compare { k, .. } => Some(f.value_ty(*k)),
            Read::IsNull { .. } => None,
        })
    }
}

impl Pass for UnboxNullableInt<'_> {
    fn name(&self) -> &'static str {
        "unbox-nullable-int"
    }

    fn run(&self, f: &mut Function, _opts: &PassOptions) -> Result<PassStats> {
        let mut stats = PassStats::default();
        if !enabled() {
            return Ok(stats);
        }
        let info = self.info;
        let mut candidates = find_candidates(f, info);
        if candidates.is_empty() {
            return Ok(stats);
        }
        collect_reads(f, info, &mut candidates);

        let mut claims = RegClaims::build(f);
        let is_param = param_values(f);
        for c in candidates
            .iter()
            .filter(|c| !c.rejected && !c.reads.is_empty())
        {
            let Some(ty) = c.int_type(f) else { continue };
            if !info.value_is_i32(ty) {
                continue;
            }
            // A widened source needs a place to put its cast, and an i32
            // source needs a register of its own for its longer live range;
            // the compared constants live on to the compare as well.
            let mut extended: Vec<ValueId> = Vec::new();
            let mut feasible = true;
            for (_, edge) in &c.edges {
                match edge {
                    Edge::Value(v) => extended.push(*v),
                    Edge::Widen(v) => {
                        feasible &= widening_site(f, *v).is_some();
                    }
                    Edge::Null => {}
                }
            }
            if !feasible {
                continue;
            }
            extended.extend(c.reads.iter().filter_map(|r| match r {
                Read::Compare { k, .. } => Some(*k),
                _ => None,
            }));
            if !extended
                .iter()
                .all(|&v| privatize(f, v, &mut claims, is_param[v.idx()]))
            {
                continue;
            }
            rewrite(f, info, c, ty, &mut stats);
        }
        Ok(stats)
    }
}

/// Where a widening of `v` can go: the block and the index just past the
/// instruction that defines it, or past the entry block's parameters when it
/// is one.
fn widening_site(f: &Function, v: ValueId) -> Option<(usize, usize)> {
    for (b, blk) in f.blocks.iter().enumerate() {
        if blk.phis.iter().any(|p| p.dst == v) {
            return Some((b, 0));
        }
        if let Some(k) = blk.instrs.iter().position(|i| i.dst() == Some(v)) {
            if matches!(blk.instrs[k], Instr::Param { .. }) {
                let params = blk
                    .instrs
                    .iter()
                    .take_while(|i| matches!(i, Instr::Param { .. }))
                    .count();
                return Some((b, params));
            }
            return Some((b, k + 1));
        }
    }
    None
}

/// Phis made only of null and `ToDyn` of a small integer.
fn find_candidates(f: &Function, info: &dyn ModuleInfo) -> Vec<Candidate> {
    let defs = def_sites(f);
    let handlers = handler_blocks(f);
    let instr_of = |v: ValueId| -> Option<&Instr> {
        let d = defs[v.idx()]?;
        Some(&f.blocks[d.block.idx()].instrs[d.instr_idx()?])
    };
    let mut out = Vec::new();
    for (b, blk) in f.blocks.iter().enumerate() {
        if b == 0 || handlers[b] {
            continue;
        }
        'phis: for phi in &blk.phis {
            if f.values[phi.dst.idx()].is_vector() {
                continue;
            }
            let (mut boxed, mut nulls) = (0, 0);
            let mut edges = Vec::new();
            for &(pred, v) in &phi.incoming {
                // The constant standing for null goes at the top of the
                // predecessor, which the entry block cannot take.
                if pred.idx() == 0 {
                    continue 'phis;
                }
                match instr_of(v) {
                    Some(Instr::Null { .. }) => {
                        nulls += 1;
                        edges.push((pred, Edge::Null));
                    }
                    Some(Instr::Cast {
                        kind: CastKind::ToDyn,
                        src,
                        ..
                    }) if !f.values[src.idx()].is_vector() => {
                        let ty = f.value_ty(*src);
                        if info.value_is_i32(ty) {
                            boxed += 1;
                            edges.push((pred, Edge::Value(*src)));
                        } else if info.value_is_small_uint(ty) {
                            boxed += 1;
                            edges.push((pred, Edge::Widen(*src)));
                        } else {
                            continue 'phis;
                        }
                    }
                    _ => continue 'phis,
                }
            }
            if boxed == 0 {
                continue;
            }
            out.push(Candidate {
                block: b,
                phi: phi.dst,
                has_null: nulls > 0,
                edges,
                reads: Vec::new(),
                rejected: false,
            });
        }
    }
    out
}

/// Sort every read of every candidate into a form the rewrite handles, or
/// reject the candidate.
fn collect_reads(f: &Function, info: &dyn ModuleInfo, candidates: &mut [Candidate]) {
    let defs = def_sites(f);
    let index: HashMap<ValueId, usize> = candidates
        .iter()
        .enumerate()
        .map(|(i, c)| (c.phi, i))
        .collect();
    // `ToDyn` of an i32: `k`, and whether it is a known constant other
    // than -1, which is what a compare needs when -1 stands for null.
    let boxed_int = |v: ValueId| -> Option<(ValueId, bool)> {
        let d = defs[v.idx()]?;
        let Instr::Cast {
            kind: CastKind::ToDyn,
            src,
            ..
        } = &f.blocks[d.block.idx()].instrs[d.instr_idx()?]
        else {
            return None;
        };
        if !info.value_is_i32(f.value_ty(*src)) || f.values[src.idx()].is_vector() {
            return None;
        }
        let known = defs[src.idx()]
            .and_then(
                |sd| match &f.blocks[sd.block.idx()].instrs[sd.instr_idx()?] {
                    Instr::Int { idx, .. } => f.int_at(*idx, |i| info.int_value(i)),
                    _ => None,
                },
            )
            .is_some_and(|c| c != -1);
        Some((*src, known))
    };
    for (b, blk) in f.blocks.iter().enumerate() {
        for phi in &blk.phis {
            for &(_, v) in &phi.incoming {
                if let Some(&c) = index.get(&v) {
                    candidates[c].rejected = true;
                }
            }
        }
        for ins in &blk.instrs {
            for u in ins.uses() {
                let Some(&c) = index.get(&u) else { continue };
                match ins {
                    Instr::Cast {
                        kind: CastKind::SafeCast,
                        dst,
                        src,
                    } if *src == u && info.value_is_i32(f.value_ty(*dst)) => {
                        candidates[c].reads.push(Read::Int {
                            block: b,
                            dst: *dst,
                        })
                    }
                    _ => candidates[c].rejected = true,
                }
            }
        }
        for u in blk.term.uses() {
            let Some(&c) = index.get(&u) else { continue };
            let Terminator::CondJump { cond, a, b: bb, .. } = &blk.term else {
                candidates[c].rejected = true;
                continue;
            };
            match (cond, bb) {
                (CondKind::Null | CondKind::NotNull, None) if *a == u => {
                    if candidates[c].has_null {
                        candidates[c].reads.push(Read::IsNull { block: b })
                    } else {
                        candidates[c].rejected = true;
                    }
                }
                (CondKind::Eq | CondKind::NotEq, Some(bb)) if (*a == u) != (*bb == u) => {
                    let phi_first = *a == u;
                    let other = if phi_first { *bb } else { *a };
                    match boxed_int(other) {
                        Some((k, known)) if known || !candidates[c].has_null => {
                            candidates[c].reads.push(Read::Compare {
                                block: b,
                                k,
                                phi_first,
                            })
                        }
                        _ => candidates[c].rejected = true,
                    }
                }
                _ => candidates[c].rejected = true,
            }
        }
    }
}

/// A fresh value of `ty` with a register of its own.
fn fresh(f: &mut Function, ty: TypeRef) -> ValueId {
    let reg = f.new_reg(ty);
    f.new_value(ty, reg)
}

fn rewrite(
    f: &mut Function,
    info: &dyn ModuleInfo,
    c: &Candidate,
    ty: TypeRef,
    stats: &mut PassStats,
) {
    let pool = |i: usize| info.int_value(i);
    let q = fresh(f, ty);
    let minus_one = f.intern_int(-1, pool);

    // The widenings first, each placed by what its source is defined by.
    let mut widened: HashMap<ValueId, ValueId> = HashMap::new();
    for (_, edge) in &c.edges {
        let Edge::Widen(src) = edge else { continue };
        if widened.contains_key(src) {
            continue;
        }
        let Some((block, at)) = widening_site(f, *src) else {
            continue;
        };
        let w = fresh(f, ty);
        f.blocks[block].instrs.insert(
            at,
            Instr::Cast {
                kind: CastKind::ToInt,
                dst: w,
                src: *src,
            },
        );
        widened.insert(*src, w);
        stats.added += 1;
    }

    // The reads next: they find their instruction by what it defines, so the
    // insertions below cannot move it.
    for read in &c.reads {
        match *read {
            Read::Int { block, dst } => {
                let Some(pos) = f.blocks[block]
                    .instrs
                    .iter()
                    .position(|i| matches!(i, Instr::Cast { dst: d, .. } if *d == dst))
                else {
                    continue;
                };
                if !c.has_null {
                    f.blocks[block].instrs[pos] = Instr::Copy { dst, src: q };
                    stats.replaced += 1;
                    continue;
                }
                let (thirty_one, shifted, mask) = (fresh(f, ty), fresh(f, ty), fresh(f, ty));
                let idx = f.intern_int(31, pool);
                f.blocks[block].instrs.splice(
                    pos..=pos,
                    [
                        Instr::Int {
                            dst: thirty_one,
                            idx,
                        },
                        Instr::BinOp {
                            op: BinOp::SShr,
                            dst: shifted,
                            a: q,
                            b: thirty_one,
                        },
                        Instr::UnOp {
                            op: UnOp::Not,
                            dst: mask,
                            src: shifted,
                        },
                        Instr::BinOp {
                            op: BinOp::And,
                            dst,
                            a: q,
                            b: mask,
                        },
                    ],
                );
                stats.added += 3;
            }
            Read::Compare {
                block,
                k,
                phi_first,
            } => {
                if let Terminator::CondJump { a, b, .. } = &mut f.blocks[block].term {
                    if phi_first {
                        (*a, *b) = (q, Some(k));
                    } else {
                        (*a, *b) = (k, Some(q));
                    }
                    stats.replaced += 1;
                }
            }
            Read::IsNull { block } => {
                let m = fresh(f, ty);
                f.blocks[block].instrs.insert(
                    0,
                    Instr::Int {
                        dst: m,
                        idx: minus_one,
                    },
                );
                if let Terminator::CondJump { cond, a, b, .. } = &mut f.blocks[block].term {
                    *cond = if *cond == CondKind::Null {
                        CondKind::Eq
                    } else {
                        CondKind::NotEq
                    };
                    (*a, *b) = (q, Some(m));
                    stats.replaced += 1;
                }
            }
        }
    }

    let mut incoming = Vec::with_capacity(c.edges.len());
    for (pred, edge) in &c.edges {
        match edge {
            Edge::Value(v) => incoming.push((*pred, *v)),
            Edge::Widen(v) => incoming.push((*pred, widened[v])),
            Edge::Null => {
                let m = fresh(f, ty);
                f.blocks[pred.idx()].instrs.insert(
                    0,
                    Instr::Int {
                        dst: m,
                        idx: minus_one,
                    },
                );
                stats.added += 1;
                incoming.push((*pred, m));
            }
        }
    }
    f.blocks[c.block].phis.push(Phi { dst: q, incoming });
    stats.added += 1;
}
