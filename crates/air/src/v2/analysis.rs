//! Analyses the optimization passes run on: natural loops with a nesting
//! forest, and the HL alias-class model derived from the typed values.

use super::ir::*;
use super::lower::build_domtree_from_succs;
use crate::dominance::DominatorTree;

// ---------------------------------------------------------------------------
// CFG snapshot
// ---------------------------------------------------------------------------

/// Successors, predecessors and dominance of a function, computed once.
///
/// Successor lists are [`Function::succ_blocks`], so exceptional edges into
/// trap handlers are part of the graph: an analysis built here sees the same
/// CFG the verifier does.
pub struct CfgInfo {
    pub succs: Vec<Vec<BlockId>>,
    pub preds: Vec<Vec<BlockId>>,
    pub dom: DominatorTree,
}

impl CfgInfo {
    pub fn build(f: &Function) -> Self {
        let nb = f.blocks.len();
        let succs: Vec<Vec<BlockId>> = (0..nb).map(|b| f.succ_blocks(BlockId(b as u32))).collect();
        let preds = f.preds();
        let raw: Vec<Vec<usize>> = succs
            .iter()
            .map(|ss| ss.iter().map(|s| s.idx()).collect())
            .collect();
        let dom = build_domtree_from_succs(&raw);
        CfgInfo { succs, preds, dom }
    }

    #[inline]
    pub fn dominates(&self, a: BlockId, b: BlockId) -> bool {
        self.dom.dominates(a.idx(), b.idx())
    }

    /// Blocks in reverse postorder. Unreachable blocks do not appear.
    pub fn rpo(&self) -> Vec<BlockId> {
        self.dom.rpo.iter().map(|&b| BlockId(b as u32)).collect()
    }
}

// ---------------------------------------------------------------------------
// natural loops
// ---------------------------------------------------------------------------

/// Index into [`LoopForest::loops`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LoopId(pub u32);

impl LoopId {
    #[inline]
    pub fn idx(self) -> usize {
        self.0 as usize
    }
}

/// A natural loop: the set of blocks that can reach a latch without leaving
/// through the header, plus the header itself.
#[derive(Debug, Clone)]
pub struct NaturalLoop {
    pub header: BlockId,
    /// Blocks with a back edge to the header, ascending.
    pub latches: Vec<BlockId>,
    /// Every block of the loop including the header, ascending.
    pub blocks: Vec<BlockId>,
    pub parent: Option<LoopId>,
    pub children: Vec<LoopId>,
    /// 0 for an outermost loop.
    pub depth: usize,
}

impl NaturalLoop {
    #[inline]
    pub fn contains(&self, b: BlockId) -> bool {
        self.blocks.binary_search(&b).is_ok()
    }
}

/// Natural loops of a function and how they nest.
///
/// A back edge is an edge `b -> h` where `h` dominates `b`; loops sharing a
/// header are merged into one. Loop `A` is a child of the smallest loop `B`
/// whose block set strictly contains `A`'s, which makes the relation a forest
/// for reducible CFGs and leaves irreducible fragments as separate roots.
pub struct LoopForest {
    pub loops: Vec<NaturalLoop>,
    pub roots: Vec<LoopId>,
    innermost: Vec<Option<LoopId>>,
}

impl LoopForest {
    pub fn analyze(f: &Function, cfg: &CfgInfo) -> Self {
        let nb = f.blocks.len();

        // Back edges grouped by header.
        let mut headers: Vec<(BlockId, Vec<BlockId>)> = Vec::new();
        for b in 0..nb {
            if cfg.dom.rpo_number[b] == usize::MAX {
                continue;
            }
            let bid = BlockId(b as u32);
            for &s in &cfg.succs[b] {
                if cfg.dominates(s, bid) {
                    match headers.iter_mut().find(|(h, _)| *h == s) {
                        Some((_, latches)) => latches.push(bid),
                        None => headers.push((s, vec![bid])),
                    }
                }
            }
        }

        let mut loops: Vec<NaturalLoop> = headers
            .into_iter()
            .map(|(header, mut latches)| {
                latches.sort_unstable();
                latches.dedup();
                // Body: backward reachability from the latches, stopping at
                // the header.
                let mut in_loop = vec![false; nb];
                in_loop[header.idx()] = true;
                let mut stack: Vec<BlockId> = latches.clone();
                while let Some(x) = stack.pop() {
                    if in_loop[x.idx()] {
                        continue;
                    }
                    in_loop[x.idx()] = true;
                    for &p in &cfg.preds[x.idx()] {
                        if !in_loop[p.idx()] {
                            stack.push(p);
                        }
                    }
                }
                let blocks: Vec<BlockId> = (0..nb)
                    .filter(|&b| in_loop[b])
                    .map(|b| BlockId(b as u32))
                    .collect();
                NaturalLoop {
                    header,
                    latches,
                    blocks,
                    parent: None,
                    children: vec![],
                    depth: 0,
                }
            })
            .collect();

        // Nesting: parent = smallest strictly-containing loop.
        let n = loops.len();
        for i in 0..n {
            let mut best: Option<usize> = None;
            for j in 0..n {
                if i == j || loops[j].blocks.len() <= loops[i].blocks.len() {
                    continue;
                }
                if !loops[i].blocks.iter().all(|&b| loops[j].contains(b)) {
                    continue;
                }
                if best.is_none_or(|k| loops[j].blocks.len() < loops[k].blocks.len()) {
                    best = Some(j);
                }
            }
            loops[i].parent = best.map(|j| LoopId(j as u32));
        }
        let mut roots: Vec<LoopId> = Vec::new();
        for i in 0..n {
            match loops[i].parent {
                Some(p) => loops[p.idx()].children.push(LoopId(i as u32)),
                None => roots.push(LoopId(i as u32)),
            }
        }
        // Depths, walking the forest from the roots.
        let mut stack: Vec<(LoopId, usize)> = roots.iter().map(|&r| (r, 0)).collect();
        while let Some((l, d)) = stack.pop() {
            loops[l.idx()].depth = d;
            for &c in &loops[l.idx()].children.clone() {
                stack.push((c, d + 1));
            }
        }

        // Innermost loop per block: the containing loop with the fewest blocks.
        let mut innermost: Vec<Option<LoopId>> = vec![None; nb];
        for (i, lp) in loops.iter().enumerate() {
            for &b in &lp.blocks {
                let cur = innermost[b.idx()];
                let better = match cur {
                    None => true,
                    Some(c) => lp.blocks.len() < loops[c.idx()].blocks.len(),
                };
                if better {
                    innermost[b.idx()] = Some(LoopId(i as u32));
                }
            }
        }

        LoopForest {
            loops,
            roots,
            innermost,
        }
    }

    #[inline]
    pub fn get(&self, l: LoopId) -> &NaturalLoop {
        &self.loops[l.idx()]
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.loops.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.loops.is_empty()
    }

    /// The innermost loop containing `b`, if any.
    #[inline]
    pub fn innermost_of(&self, b: BlockId) -> Option<LoopId> {
        self.innermost[b.idx()]
    }

    /// Nesting depth of `b`: 0 outside every loop, 1 in an outermost loop.
    pub fn depth_of(&self, b: BlockId) -> usize {
        match self.innermost_of(b) {
            Some(l) => self.get(l).depth + 1,
            None => 0,
        }
    }

    /// Loop ids ordered innermost-first (deepest, then smallest).
    pub fn innermost_first(&self) -> Vec<LoopId> {
        let mut ids: Vec<LoopId> = (0..self.loops.len() as u32).map(LoopId).collect();
        ids.sort_by_key(|&l| {
            let lp = self.get(l);
            (std::cmp::Reverse(lp.depth), lp.blocks.len())
        });
        ids
    }

    /// Blocks of the loop with at least one successor outside it.
    pub fn exiting_blocks(&self, cfg: &CfgInfo, l: LoopId) -> Vec<BlockId> {
        let lp = self.get(l);
        lp.blocks
            .iter()
            .copied()
            .filter(|b| cfg.succs[b.idx()].iter().any(|s| !lp.contains(*s)))
            .collect()
    }

    /// Predecessors of the header that lie outside the loop — the edges a
    /// preheader would absorb.
    pub fn entry_preds(&self, cfg: &CfgInfo, l: LoopId) -> Vec<BlockId> {
        let lp = self.get(l);
        cfg.preds[lp.header.idx()]
            .iter()
            .copied()
            .filter(|p| !lp.contains(*p))
            .collect()
    }
}

// ---------------------------------------------------------------------------
// alias classes
// ---------------------------------------------------------------------------

/// Disjoint classes of HL memory. Two accesses in different classes provably
/// touch different storage, so a load may be reused across a write to another
/// class.
///
/// The classes are derived from the typed IR:
///
/// * `ObjField` — one class per `(object type, field slot)` pair, which is
///   what makes field loads of different fields (or of the same slot on
///   unrelated types) independent.
/// * `ArrayData` / `ArrayLen` — a `varray`'s elements and its length are
///   separate: `ArraySize` is not invalidated by `SetArray`.
/// * `EnumParam` — one class per `(construct, field)`.
/// * `Global` — one class per global slot.
/// * `RawBytes` — everything reached through byte pointers (`GetI8`,
///   `GetI16`, `GetMem` and their setters).
/// * `DynBox` — reflective access by field name. This is the one pair that is
///   *not* disjoint: a `DynGet`/`DynSet` can resolve to an object field, so
///   `DynBox` and `ObjField` may alias each other.
/// * `Cell` — one class per pinned-register cell.
/// * `Any` — unknown storage: calls, `Setref` through a taken reference,
///   `Unref`, inline asm, region markers. Aliases everything.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AliasClass {
    ObjField { ty: TypeRef, field: usize },
    ArrayData,
    ArrayLen,
    EnumParam { construct: usize, field: usize },
    Global(usize),
    RawBytes,
    DynBox,
    Cell(CellId),
    Any,
}

impl AliasClass {
    /// True when the two classes may denote the same storage.
    pub fn may_alias(self, other: AliasClass) -> bool {
        use AliasClass::*;
        match (self, other) {
            (Any, _) | (_, Any) => true,
            (DynBox, ObjField { .. }) | (ObjField { .. }, DynBox) => true,
            (a, b) => a == b,
        }
    }
}

/// The storage an instruction reads, or `None` when it reads no memory.
pub fn read_class(ins: &Instr) -> Option<AliasClass> {
    match ins {
        Instr::FieldGet { obj_ty, field, .. } => Some(AliasClass::ObjField {
            ty: *obj_ty,
            field: *field,
        }),
        Instr::MemGet { kind, .. } => Some(match kind {
            MemAccess::Array => AliasClass::ArrayData,
            _ => AliasClass::RawBytes,
        }),
        Instr::ArraySize { .. } => Some(AliasClass::ArrayLen),
        Instr::GetGlobal { global, .. } => Some(AliasClass::Global(*global)),
        Instr::EnumField {
            construct, field, ..
        } => Some(AliasClass::EnumParam {
            construct: *construct,
            field: *field,
        }),
        Instr::EnumIndex { .. } => Some(AliasClass::Any),
        Instr::CellGet { cell, .. } => Some(AliasClass::Cell(*cell)),
        Instr::DynGet { .. } => Some(AliasClass::DynBox),
        Instr::GetType { .. }
        | Instr::GetTID { .. }
        | Instr::Unref { .. }
        | Instr::RefData { .. } => Some(AliasClass::Any),
        _ => None,
    }
}

/// The storage an instruction writes, or `None` when it writes no memory
/// observable by another instruction.
///
/// Allocations (`New`, `MakeEnum`, `EnumAlloc`, the closure builders,
/// `ToDyn`/`ToVirtual`) write only storage they just created, which nothing
/// else can hold a reference to yet, so they write `None`.
pub fn write_class(ins: &Instr) -> Option<AliasClass> {
    match ins {
        Instr::FieldSet { obj_ty, field, .. } => Some(AliasClass::ObjField {
            ty: *obj_ty,
            field: *field,
        }),
        Instr::MemSet { kind, .. } => Some(match kind {
            MemAccess::Array => AliasClass::ArrayData,
            _ => AliasClass::RawBytes,
        }),
        Instr::SetGlobal { global, .. } => Some(AliasClass::Global(*global)),
        Instr::SetEnumField {
            construct, field, ..
        } => Some(AliasClass::EnumParam {
            construct: *construct,
            field: *field,
        }),
        Instr::CellSet { cell, .. } | Instr::CellIncr { cell } | Instr::CellDecr { cell } => {
            Some(AliasClass::Cell(*cell))
        }
        Instr::DynSet { .. } => Some(AliasClass::DynBox),
        Instr::SetRef { .. }
        | Instr::Call { .. }
        | Instr::CallMethod { .. }
        | Instr::CallClosure { .. }
        | Instr::Asm { .. }
        | Instr::EndTrap { .. } => Some(AliasClass::Any),
        _ => None,
    }
}

/// True when the instruction is a barrier for every load: it may write any
/// storage at all.
pub fn clobbers_all(ins: &Instr) -> bool {
    ins.effect() == Effect::ClobberAll || write_class(ins) == Some(AliasClass::Any)
}

/// True when `ins` may write storage that a load of `class` reads.
pub fn writes_may_alias(ins: &Instr, class: AliasClass) -> bool {
    match write_class(ins) {
        Some(w) => w.may_alias(class),
        None => false,
    }
}

/// True when the instruction produces a value that is provably not null: a
/// fresh allocation, a constant with an address, or the address of a cell.
pub fn is_non_null_result(ins: &Instr) -> bool {
    matches!(
        ins,
        Instr::New { .. }
            | Instr::MakeEnum { .. }
            | Instr::EnumAlloc { .. }
            | Instr::StaticClosure { .. }
            | Instr::InstanceClosure { .. }
            | Instr::VirtualClosure { .. }
            | Instr::String { .. }
            | Instr::Bytes { .. }
            | Instr::TypeConst { .. }
            | Instr::CellRef { .. }
    )
}
