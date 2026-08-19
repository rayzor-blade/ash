//! Loop-scoped escape analysis, so a non-escaping allocation can be hoisted
//! out of the loop that repeats it.
//!
//! # Why this is worth doing
//!
//! `new C(...)` inside a loop allocates once per iteration. mandelbrot builds
//! two `Complex` values per inner iteration and runs 196.5 million of them,
//! churning 24 GiB for a live set that never exceeds 3.7 MB. Profiling puts
//! **52% of that program in the collector** — mark, sweep, the allocator's
//! reentrant mutex, and the `madvise` traffic from handing pages back and
//! taking them again. Only ~16% is generated code, so no amount of backend
//! work moves it.
//!
//! # Why this rather than scalar replacement
//!
//! [`super::sroa`] asks whether an object can be taken apart into scalars,
//! which needs the whole object to be understood. This asks a much weaker
//! question — *does this pointer outlive the iteration?* — and when the answer
//! is no, one allocation in the preheader serves every iteration. SROA is
//! scheduled at O3 today and fires zero times on mandelbrot; this catches the
//! case it misses, because "the object is rebuilt from scratch every iteration
//! and then dropped" is exactly the shape it cannot promote but this can.
//!
//! # What counts as escaping
//!
//! Conservative by construction: the allocation is hoisted only when every use
//! inside the loop is a field access on the object itself. Anything else is an
//! escape, including all of:
//!
//! * being passed to a call — the callee may store it anywhere;
//! * being stored into memory — a field, array slot, global, cell or enum;
//! * appearing in a phi — that is how a value crosses the back edge, and it is
//!   precisely what disqualifies mandelbrot's *second* allocation, whose result
//!   becomes `val` and is read by the next iteration;
//! * being returned or thrown;
//! * being used anywhere outside the loop.
//!
//! # The reuse condition
//!
//! Hoisting means one object serves every iteration, so an iteration must not
//! be able to observe the previous one's contents. `hlp_alloc_obj` zeroes, and
//! that zeroing is what disappears when the allocation moves out. The pass
//! therefore also requires every field the loop *reads* to be written first, in
//! the allocation's own block, before any branch — which is exactly the shape
//! HL emits for `new C(a, b)` once the constructor has been inlined, and is why
//! this runs after [`super::inline`].

use std::collections::{HashMap, HashSet};

use super::super::analysis::{CfgInfo, LoopForest, LoopId};
use super::super::ir::{BlockId, Function, Instr, ValueId};

/// One allocation considered for hoisting, and the verdict.
#[derive(Debug, Clone)]
pub struct AllocEscapeInfo {
    /// Value produced by the `New`.
    pub value: ValueId,
    /// Where the `New` sits.
    pub location: (BlockId, usize),
    /// True when the pointer can outlive the iteration, so it must stay put.
    pub escapes: bool,
    /// Why, for diagnostics. `None` when it does not escape.
    pub reason: Option<&'static str>,
}

/// Analyze every `New` in `l` for whether its result outlives one iteration.
pub fn analyze_alloc_escapes(f: &Function, forest: &LoopForest, l: LoopId) -> Vec<AllocEscapeInfo> {
    let cfg = CfgInfo::build(f);
    let lp = forest.get(l);
    let in_loop: HashSet<BlockId> = (0..f.blocks.len())
        .map(|i| BlockId(i as u32))
        .filter(|&b| lp.contains(b))
        .collect();

    // SSA construction here is not pruned: a register defined anywhere in the
    // loop gets a header phi whether or not the value is live on entry, and DCE
    // only runs at the end of the pipeline. Treating those dead phis as escapes
    // would reject every allocation in every loop, so ask which phis actually
    // feed something.
    let live = transitively_used(f);

    let mut out = Vec::new();
    for &b in &in_loop {
        for (k, ins) in f.blocks[b.idx()].instrs.iter().enumerate() {
            let Instr::New { dst } = ins else { continue };
            let dst = *dst;
            let reason = escape_reason(f, &cfg, &in_loop, &live, dst, b, k);
            out.push(AllocEscapeInfo {
                value: dst,
                location: (b, k),
                escapes: reason.is_some(),
                reason,
            });
        }
    }
    out.sort_by_key(|i| (i.location.0.idx(), i.location.1));
    out
}

/// Values that some real instruction or terminator eventually consumes.
///
/// Phis are followed transitively, so a chain of phis feeding nothing is
/// correctly reported as feeding nothing.
pub(crate) fn transitively_used(f: &Function) -> HashSet<ValueId> {
    let mut live: HashSet<ValueId> = HashSet::new();
    for block in &f.blocks {
        for ins in &block.instrs {
            live.extend(ins.uses());
        }
        live.extend(block.term.uses());
    }
    loop {
        let before = live.len();
        for block in &f.blocks {
            for phi in &block.phis {
                if live.contains(&phi.dst) {
                    live.extend(phi.incoming.iter().map(|(_, s)| *s));
                }
            }
        }
        if live.len() == before {
            break;
        }
    }
    live
}

/// `Some(reason)` when `v` may outlive one iteration of the loop.
fn escape_reason(
    f: &Function,
    cfg: &CfgInfo,
    in_loop: &HashSet<BlockId>,
    live: &HashSet<ValueId>,
    v: ValueId,
    def_block: BlockId,
    def_idx: usize,
) -> Option<&'static str> {
    // Fields the loop reads, and fields written in a block that dominates every
    // read. The read set must be covered by the written set, or a later
    // iteration could see what the previous one left behind.
    let mut read_fields: HashSet<usize> = HashSet::new();
    let mut written_dominating: HashSet<usize> = HashSet::new();
    let mut read_blocks: HashMap<usize, Vec<BlockId>> = HashMap::new();
    let mut write_blocks: HashMap<usize, Vec<BlockId>> = HashMap::new();

    for b in 0..f.blocks.len() {
        let bid = BlockId(b as u32);
        let block = &f.blocks[b];

        // A phi naming the value carries it across an edge — the back edge
        // included, which is how a result becomes the next iteration's input.
        for phi in &block.phis {
            // A dead phi carries nothing anywhere.
            if !live.contains(&phi.dst) {
                continue;
            }
            if phi.incoming.iter().any(|(_, s)| *s == v) || phi.dst == v {
                return Some("carried by a phi");
            }
        }

        for (k, ins) in block.instrs.iter().enumerate() {
            let mentions = ins.uses().contains(&v);
            if !mentions {
                continue;
            }
            if !in_loop.contains(&bid) {
                return Some("used outside the loop");
            }
            match ins {
                // Reading a field of the object is fine — that is the object
                // being used as an object.
                Instr::FieldGet { obj, field, .. } if *obj == v => {
                    read_fields.insert(*field);
                    read_blocks.entry(*field).or_default().push(bid);
                }
                // Writing a field is fine as long as the value being stored is
                // not the object itself, which would publish it.
                Instr::FieldSet {
                    obj, field, src, ..
                } if *obj == v && *src != v => {
                    let _ = (def_block, def_idx, k);
                    write_blocks.entry(*field).or_default().push(bid);
                }
                _ => return Some("passed to a call, stored, or otherwise published"),
            }
        }

        if block.term.uses().contains(&v) {
            return Some("returned, thrown, or tested by a terminator");
        }
    }

    for &fd in &read_fields {
        let writes = write_blocks.get(&fd).map(|v| v.as_slice()).unwrap_or(&[]);
        let reads = read_blocks.get(&fd).map(|v| v.as_slice()).unwrap_or(&[]);
        // Every read must be dominated by some write of the same field. A write
        // in the same block counts, since the classifier only admits reads and
        // writes of this object and program order within a block is total.
        let covered = reads
            .iter()
            .all(|&r| writes.iter().any(|&w| w == r || cfg.dominates(w, r)));
        if covered {
            written_dominating.insert(fd);
        }
    }
    if !read_fields.is_subset(&written_dominating) {
        return Some("a field is read that no write dominates");
    }
    None
}

/// Values whose `New` may be hoisted out of `l`, in program order.
pub fn hoistable_allocs(f: &Function, forest: &LoopForest, l: LoopId) -> Vec<(BlockId, usize)> {
    analyze_alloc_escapes(f, forest, l)
        .into_iter()
        .filter(|i| !i.escapes)
        .map(|i| i.location)
        .collect()
}

/// Per-loop counts, for reporting how much of a program this can reach.
pub fn summarize(f: &Function) -> HashMap<&'static str, usize> {
    let cfg = CfgInfo::build(f);
    let forest = LoopForest::analyze(f, &cfg);
    let mut counts: HashMap<&'static str, usize> = HashMap::new();
    for l in forest.innermost_first() {
        for info in analyze_alloc_escapes(f, &forest, l) {
            *counts
                .entry(info.reason.unwrap_or("HOISTABLE"))
                .or_insert(0) += 1;
        }
    }
    counts
}
