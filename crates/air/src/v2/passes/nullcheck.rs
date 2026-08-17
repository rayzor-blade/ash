//! Dominance-based null-check elimination.

use super::{compact_values, def_sites, DefSite, Pass, PassOptions, PassStats};
use crate::v2::analysis::{is_non_null_result, CfgInfo};
use crate::v2::ir::*;
use anyhow::Result;
use std::collections::HashSet;

/// Removes `NullCheck`s whose operand is already known to be non-null.
///
/// A value is known non-null at a program point when any of these holds:
/// * it is defined by an instruction that cannot produce null — an
///   allocation, a string/bytes/type constant, or the address of a cell;
/// * a `NullCheck` on the same value (following `Copy` chains) dominates the
///   point and was itself kept;
/// * the point is dominated by the block on the proving side of a
///   `CondJump` on `Null`/`NotNull` of the value, and that block is entered
///   only through that edge.
///
/// The last may-throw instruction of a block covered by a trap handler is
/// never removed: the block's exceptional edge — and with it a predecessor of
/// the handler and the arity of the handler's phis — has to survive the pass.
pub struct NullCheckElim;

/// Follow `Copy` chains to the value that actually holds the pointer.
fn copy_root(f: &Function, defs: &[Option<DefSite>], mut v: ValueId) -> ValueId {
    for _ in 0..f.values.len() {
        let Some(d) = defs[v.idx()] else { return v };
        let Some(k) = d.instr_idx() else { return v };
        match &f.blocks[d.block.idx()].instrs[k] {
            Instr::Copy { src, .. } => v = *src,
            _ => return v,
        }
    }
    v
}

/// Values whose defining instruction cannot produce null.
fn non_null_defs(f: &Function, defs: &[Option<DefSite>]) -> Vec<bool> {
    let mut out = vec![false; f.values.len()];
    for (v, d) in defs.iter().enumerate() {
        let Some(d) = d else { continue };
        let Some(k) = d.instr_idx() else { continue };
        if is_non_null_result(&f.blocks[d.block.idx()].instrs[k]) {
            out[v] = true;
        }
    }
    out
}

/// `(value, block)` pairs where entering `block` proves `value` non-null.
fn guard_anchors(f: &Function, cfg: &CfgInfo, defs: &[Option<DefSite>]) -> Vec<(ValueId, BlockId)> {
    let mut out = Vec::new();
    for (b, blk) in f.blocks.iter().enumerate() {
        let Terminator::CondJump {
            cond,
            a,
            if_true,
            if_false,
            ..
        } = &blk.term
        else {
            continue;
        };
        let proving = match cond {
            CondKind::NotNull => *if_true,
            CondKind::Null => *if_false,
            _ => continue,
        };
        // Only when the proving block is entered exclusively through this
        // edge; otherwise another predecessor could carry a null.
        if cfg.preds[proving.idx()].len() == 1 && cfg.preds[proving.idx()][0].idx() == b {
            out.push((copy_root(f, defs, *a), proving));
        }
    }
    out
}

impl Pass for NullCheckElim {
    fn name(&self) -> &'static str {
        "null-check-elim"
    }

    fn run(&self, f: &mut Function, _opts: &PassOptions) -> Result<PassStats> {
        let mut stats = PassStats::default();
        let cfg = CfgInfo::build(f);
        let defs = def_sites(f);
        let nonnull = non_null_defs(f, &defs);
        let anchors = guard_anchors(f, &cfg, &defs);

        // Remaining may-throw instructions per block; a handled block must
        // keep at least one.
        let mut throwers: Vec<usize> = f
            .blocks
            .iter()
            .map(|b| b.instrs.iter().filter(|i| i.may_throw()).count())
            .collect();

        // Scoped dominator-tree walk: facts established in a block are visible
        // in the blocks it dominates and nowhere else.
        let mut known: HashSet<ValueId> = HashSet::new();
        let mut remove: Vec<(usize, usize)> = Vec::new();
        enum Item {
            Visit(usize),
            Undo(Vec<ValueId>),
        }
        let mut walk = vec![Item::Visit(cfg.dom.rpo[0])];
        while let Some(item) = walk.pop() {
            let b = match item {
                Item::Undo(vs) => {
                    for v in vs {
                        known.remove(&v);
                    }
                    continue;
                }
                Item::Visit(b) => b,
            };
            let mut added: Vec<ValueId> = Vec::new();
            let bid = BlockId(b as u32);
            for &(v, anchor) in &anchors {
                if cfg.dominates(anchor, bid) && known.insert(v) {
                    added.push(v);
                }
            }
            let terminator_throws = matches!(
                f.blocks[b].term,
                Terminator::Throw { .. } | Terminator::Rethrow { .. }
            );
            for (k, ins) in f.blocks[b].instrs.iter().enumerate() {
                let Instr::NullCheck { value } = ins else {
                    continue;
                };
                let root = copy_root(f, &defs, *value);
                let proven = nonnull[root.idx()] || known.contains(&root);
                if proven {
                    let last_thrower = throwers[b] == 1 && !terminator_throws;
                    if last_thrower && f.blocks[b].handler.is_some() {
                        continue; // keep the block's exceptional edge alive
                    }
                    throwers[b] -= 1;
                    remove.push((b, k));
                } else if known.insert(root) {
                    added.push(root);
                }
            }
            walk.push(Item::Undo(added));
            for &child in cfg.dom.dom_children[b].iter().rev() {
                walk.push(Item::Visit(child));
            }
        }

        if remove.is_empty() {
            return Ok(stats);
        }
        for b in 0..f.blocks.len() {
            let drop: HashSet<usize> = remove
                .iter()
                .filter(|(bb, _)| *bb == b)
                .map(|(_, k)| *k)
                .collect();
            if drop.is_empty() {
                continue;
            }
            let mut k = 0usize;
            f.blocks[b].instrs.retain(|_| {
                let keep = !drop.contains(&k);
                k += 1;
                keep
            });
        }
        stats.eliminated = remove.len();
        compact_values(f)?;
        Ok(stats)
    }
}
