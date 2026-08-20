//! Inlining of direct calls to bytecode functions.

use super::{compact_values, Pass, PassOptions, PassStats};
use crate::v2::ir::*;
use crate::v2::module::ModuleInfo;
use crate::v2::verify::verify;
use anyhow::Result;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;

/// Replaces a direct `Call` with the callee's body.
///
/// `air` is handed one function at a time and has no view of the module's
/// function pool, so the callee comes from the embedder through
/// [`ModuleInfo::callee`]. Without module info nothing is inlinable and the
/// pass is inert.
///
/// The rewrite splits the call's block in two: the part before the call jumps
/// to the callee's entry, the callee's `Ret`s jump to a continuation block
/// holding the rest of the original block, and the call's destination is
/// defined at the head of that continuation — from the single returned value,
/// or from a phi over the returning blocks. The callee's values, blocks and
/// registers are all renumbered into fresh caller ones, so the callee's own
/// de-SSA register assignment stays conflict-free inside the copy.
///
/// Guarantees:
/// * **only direct calls** — `CallMethod` and `CallClosure` name a runtime
///   target, not a `findex`;
/// * **inlining never crosses a trap region in either direction.** A call site
///   covered by a [`Block::handler`] is refused, because the inlined blocks
///   would become new exceptional predecessors of a handler that may carry
///   phis, and those phis have no program point to be resolved on. A callee
///   with cells is refused, which covers callees containing trap regions (a
///   `Trap` always pins its exception register), callees with `Ref`-taken
///   registers, and callees using `Incr`/`Decr`: a cell is a frame slot, and
///   materializing it as a caller register would both share one slot across
///   activations and lose the frame's initial value;
/// * **entry binding is total** — parameters of argument registers become
///   copies of the argument values, parameters of the callee's other registers
///   are dropped; a dropped parameter that is still read would observe its
///   frame's initial value, so the inline is refused instead;
/// * **the callee must return** — a body that only throws would leave the
///   continuation unreachable;
/// * **types must line up** — every returned value and every argument must
///   have the type the caller's call expects;
/// * **recursion is a policy, not an accident.** A DIRECT self-call may be
///   expanded — it replaces two calls with the work of three smaller ones, so
///   the recurrence base drops (naive fib: 1.618 → 1.380; GCC's recursive
///   inliner does the same at -O2) — but only under its own budget:
///   [`SELF_INLINE_MAX_BODY`], [`SELF_INLINE_MAX_SITES`] expansions per
///   function *per pipeline*, held in [`Inlining::self_expanded`] so manager
///   rounds cannot compound it. A callee that calls back into this function
///   (direct mutual recursion) is never inlined: no per-site budget bounds
///   what re-running the rounds would re-open. This is rayzor's
///   `InliningCostModel::should_inline` policy and zyntax's
///   `MAX_RECURSIVE_INLINE_*` caps; before it existed, fib's 11-instruction
///   body reached `inline_max_function` at 319 instructions — an optimizer
///   whose output was 29x its input;
/// * **budgeted** — [`PassOptions::inline_max_callee`] bounds the callee,
///   [`PassOptions::inline_max_depth`] bounds how deep one run nests inlines,
///   [`PassOptions::inline_max_function`] bounds the caller absolutely, and
///   [`GROWTH_LIMIT`] bounds it *relative to its own original size* — the
///   invariant that optimized output does not dwarf its input, enforced
///   rather than hoped for. The relative bound is the one that holds across
///   manager rounds; the depth vector starts over each round.
pub struct Inlining<'m> {
    info: &'m dyn ModuleInfo,
    /// Lowered callee bodies, so a callee inlined at several sites is lowered
    /// once. `None` records "asked, not inlinable".
    cache: RefCell<HashMap<usize, Option<Function>>>,
    /// The function's instruction count before the first inline, recorded on
    /// the first `run`. One `Inlining` serves one function's pipeline, so this
    /// is the baseline [`GROWTH_LIMIT`] measures against — unlike the per-run
    /// depth vector, it survives manager rounds.
    original_size: Cell<Option<usize>>,
    /// Direct self-call expansions performed so far, across every round.
    self_expanded: Cell<usize>,
}

impl<'m> Inlining<'m> {
    pub fn new(info: &'m dyn ModuleInfo) -> Self {
        Inlining {
            info,
            cache: RefCell::new(HashMap::new()),
            original_size: Cell::new(None),
            self_expanded: Cell::new(0),
        }
    }

    /// The lowered, verified body of `findex`, or `None` when the embedder has
    /// none or it does not survive verification.
    fn body(&self, findex: usize) -> Option<Function> {
        if let Some(hit) = self.cache.borrow().get(&findex) {
            return hit.clone();
        }
        let lowered = self
            .info
            .callee(findex)
            .and_then(|b| b.into_function(findex, self.info).ok())
            .filter(|g| verify(g).is_ok() && !g.blocks.is_empty());
        self.cache.borrow_mut().insert(findex, lowered.clone());
        lowered
    }
}

/// Bound on inlines performed by one run, independent of the budgets.
const MAX_SITES: usize = 64;

/// A self-recursive body larger than this is not worth duplicating: every
/// copy is a full body behind a live argument, so later passes cannot shrink
/// it back. Naive fib is ~11 instructions; zyntax draws the same line at 96.
const SELF_INLINE_MAX_BODY: usize = 96;

/// Direct self-call sites expanded per function per PIPELINE — not per round.
/// Naive fib has 2. Expanding an already-expanded body compounds the size
/// exponentially for a recurrence-base gain that only the first expansion
/// buys, which is why this lives in a [`Cell`] on the pass rather than in the
/// per-run depth vector.
const SELF_INLINE_MAX_SITES: usize = 4;

/// Ceiling on the function's size as a multiple of what it started the
/// pipeline at. `inline_max_function` (400) alone let an 11-instruction
/// function grow 29x; rayzor bounds the same way (`max_growth_percent`).
/// The additive floor keeps tiny functions — constructors are ~5
/// instructions — able to absorb a helper at all.
fn growth_cap(original: usize, opts: &PassOptions) -> usize {
    (original * 3).max(original + 2 * opts.inline_max_callee)
}

impl Pass for Inlining<'_> {
    fn name(&self) -> &'static str {
        "inline"
    }

    fn run(&self, f: &mut Function, opts: &PassOptions) -> Result<PassStats> {
        let mut stats = PassStats::default();
        if self.original_size.get().is_none() {
            self.original_size.set(Some(instr_count(f)));
        }
        // How many inlines deep each block already is, within this run.
        let mut depth: Vec<usize> = vec![0; f.blocks.len()];
        for _ in 0..MAX_SITES {
            let Some((b, k, g)) = self.pick(f, opts, &depth) else {
                break;
            };
            let before = instr_count(f);
            let first_new = f.blocks.len();
            let body_blocks = g.blocks.len();
            let site_depth = depth[b.idx()];
            inline_at(f, b, k, &g)?;
            // The copied body is one level deeper; the continuation is caller
            // code and stays at the call site's level.
            depth.resize(f.blocks.len(), site_depth);
            for d in depth.iter_mut().skip(first_new).take(body_blocks) {
                *d = site_depth + 1;
            }
            compact_values(f)?;
            stats.inlined += 1;
            stats.added += instr_count(f).saturating_sub(before);
        }
        Ok(stats)
    }
}

/// Code size in instructions. `Param` is the function's entry-register
/// surface, not computation, and emits nothing, so it does not count against
/// any budget.
fn instr_count(f: &Function) -> usize {
    f.blocks
        .iter()
        .flat_map(|b| b.instrs.iter())
        .filter(|i| !matches!(i, Instr::Param { .. }))
        .count()
}

impl Inlining<'_> {
    /// The next call site worth inlining, with the callee body to use.
    fn pick(
        &self,
        f: &Function,
        opts: &PassOptions,
        depth: &[usize],
    ) -> Option<(BlockId, usize, Function)> {
        let caller_size = instr_count(f);
        for (b, blk) in f.blocks.iter().enumerate() {
            // A handler covering the call site would gain the inlined blocks
            // as exceptional predecessors.
            if blk.handler.is_some() || depth[b] >= opts.inline_max_depth {
                continue;
            }
            for (k, ins) in blk.instrs.iter().enumerate() {
                let Instr::Call { dst, fun, args } = ins else {
                    continue;
                };
                let self_call = f.findex == Some(*fun);
                if self_call
                    && (self.self_expanded.get() >= SELF_INLINE_MAX_SITES
                        || caller_size > SELF_INLINE_MAX_BODY)
                {
                    continue;
                }
                let Some(g) = self.body(*fun) else { continue };
                // Direct mutual recursion: a callee that calls back into this
                // function re-opens on every round, and no per-site budget
                // bounds that. Never inlined.
                if !self_call && calls_into(&g, f.findex) {
                    continue;
                }
                let cap = self
                    .original_size
                    .get()
                    .map(|o| growth_cap(o, opts))
                    .unwrap_or(opts.inline_max_function)
                    .min(opts.inline_max_function);
                if !fits(f, &g, *dst, args, opts, caller_size, cap) {
                    continue;
                }
                if self_call {
                    self.self_expanded.set(self.self_expanded.get() + 1);
                }
                return Some((BlockId(b as u32), k, g));
            }
        }
        None
    }
}

/// True when `g` contains a direct call to `findex`.
fn calls_into(g: &Function, findex: Option<usize>) -> bool {
    let Some(target) = findex else { return false };
    g.blocks
        .iter()
        .flat_map(|b| b.instrs.iter())
        .any(|i| matches!(i, Instr::Call { fun, .. } if *fun == target))
}

/// True when every use of `v` is a `Ret` returning it.
///
/// This is the shape HL constructors have. `new C(...)` emits `New` then a call
/// to a constructor that returns whatever sits in some register it never wrote,
/// and the caller discards it — so the value being returned is the frame's
/// initial value, and nothing observes it. Refusing to inline on account of
/// that read is what kept every allocation escaping into its constructor, which
/// in turn is why scalar replacement and loop-escape analysis both found
/// nothing to do.
fn only_returned(g: &Function, v: ValueId) -> bool {
    for blk in &g.blocks {
        if blk.instrs.iter().any(|i| i.uses().contains(&v)) {
            return false;
        }
        for phi in &blk.phis {
            if phi.dst == v || phi.incoming.iter().any(|&(_, s)| s == v) {
                return false;
            }
        }
        match &blk.term {
            Terminator::Ret { value } if *value == v => {}
            other if other.uses().contains(&v) => return false,
            _ => {}
        }
    }
    true
}

/// Whether `v`'s type has null as its default, so a dropped parameter can be
/// materialized as `Null` rather than refused.
fn is_nullable(g: &Function, v: ValueId) -> bool {
    // Conservative: only kinds AIR itself models as pointers. A numeric
    // default would need a constant-pool entry the pass cannot mint.
    !g.is_float(g.value_ty(v))
}

/// Every check that does not depend on the rewrite itself.
fn fits(
    f: &Function,
    g: &Function,
    dst: ValueId,
    args: &[ValueId],
    opts: &PassOptions,
    caller_size: usize,
    size_cap: usize,
) -> bool {
    let size = instr_count(g);
    if size > opts.inline_max_callee || caller_size + size > size_cap {
        return false;
    }
    // Cells are frame slots; see the pass documentation.
    if !g.cells.is_empty() || g.blocks.iter().any(|b| b.handler.is_some()) {
        return false;
    }
    if args.len() > g.reg_types.len() {
        return false;
    }
    // The continuation needs at least one returning path, and every returned
    // value must have the type the caller's destination has.
    let mut returns = 0usize;
    for blk in &g.blocks {
        if let Terminator::Ret { value } = blk.term {
            returns += 1;
            if g.value_ty(value) != f.value_ty(dst) {
                return false;
            }
        }
    }
    if returns == 0 {
        return false;
    }
    // Entry binding: arguments must match their parameters, and parameters of
    // the callee's other registers must be dead — or dead enough, see below.
    let counts = g.use_counts();
    let dst_dead = f.use_counts()[dst.idx()] == 0;
    for ins in &g.blocks[0].instrs {
        let Instr::Param { dst: pv, reg } = ins else {
            continue;
        };
        match args.get(*reg as usize) {
            Some(&a) => {
                if f.value_ty(a) != g.value_ty(*pv) {
                    return false;
                }
            }
            None => {
                if counts[pv.idx()] > 0 && !only_returned(g, *pv) {
                    return false;
                }
                // Read only by `Ret`: the callee hands back its frame's
                // initial value for that register, which is the HL default.
                // Inlining it is sound as long as the caller cannot observe
                // the difference, so the result must be dead and the type must
                // be one whose default is null.
                if counts[pv.idx()] > 0 && (!dst_dead || !is_nullable(g, *pv)) {
                    return false;
                }
            }
        }
    }
    true
}

/// Splice `g` into `f` at instruction `k` of block `b`.
fn inline_at(f: &mut Function, b: BlockId, k: usize, g: &Function) -> Result<()> {
    let (dst, args) = match &f.blocks[b.idx()].instrs[k] {
        Instr::Call { dst, args, .. } => (*dst, args.clone()),
        other => anyhow::bail!("inline site is not a Call: {:?}", other),
    };

    let base = f.blocks.len();
    let map_block = |gb: BlockId| BlockId((base + gb.idx()) as u32);
    let cont = BlockId((base + g.blocks.len()) as u32);

    // Fresh caller registers for the callee's frame, then fresh caller values
    // carrying them.
    let reg_map: Vec<u32> = g.reg_types.iter().map(|&ty| f.new_reg(ty)).collect();
    let val_map: Vec<ValueId> = g
        .values
        .iter()
        .map(|v| {
            let reg = reg_map
                .get(v.reg as usize)
                .copied()
                .unwrap_or_else(|| f.new_reg(v.ty));
            f.new_value(v.ty, reg)
        })
        .collect();
    let map_val = |v: ValueId| val_map[v.idx()];

    // ---- callee blocks ----------------------------------------------------
    let mut ret_sites: Vec<(BlockId, ValueId)> = Vec::new();
    let mut blocks: Vec<Block> = Vec::with_capacity(g.blocks.len());
    for (gb, gblk) in g.blocks.iter().enumerate() {
        let phis = gblk
            .phis
            .iter()
            .map(|p| Phi {
                dst: map_val(p.dst),
                incoming: p
                    .incoming
                    .iter()
                    .map(|&(pb, v)| (map_block(pb), map_val(v)))
                    .collect(),
            })
            .collect();

        let mut instrs: Vec<Instr> = Vec::with_capacity(gblk.instrs.len());
        for ins in &gblk.instrs {
            if let Instr::Param { dst: pv, reg } = ins {
                match args.get(*reg as usize) {
                    // Bind an argument register.
                    Some(&a) => instrs.push(Instr::Copy {
                        dst: map_val(*pv),
                        src: a,
                    }),
                    // A parameter of a non-argument register. `fits` proved
                    // either that nothing reads it, or that the only reader is
                    // a `Ret` whose value the caller discards — in which case
                    // it still needs a definition for the continuation phi to
                    // name, and the callee's own semantics say that value is
                    // the frame default.
                    None => {
                        if g.use_counts()[pv.idx()] > 0 {
                            instrs.push(Instr::Null { dst: map_val(*pv) });
                        }
                    }
                }
                continue;
            }
            let mut ins = ins.clone();
            ins.map_dst(&mut |v| map_val(v));
            ins.map_uses(&mut |v| map_val(v));
            instrs.push(ins);
        }

        let term = match &gblk.term {
            Terminator::Ret { value } => {
                ret_sites.push((map_block(BlockId(gb as u32)), map_val(*value)));
                Terminator::Jump { target: cont }
            }
            other => {
                let mut t = other.clone();
                t.map_uses(&mut |v| map_val(v));
                t.map_targets(&mut |b| map_block(b));
                t
            }
        };

        blocks.push(Block {
            phis,
            instrs,
            term,
            handler: None,
        });
    }

    // ---- continuation ------------------------------------------------------
    let tail: Vec<Instr> = f.blocks[b.idx()].instrs.split_off(k + 1);
    f.blocks[b.idx()].instrs.pop(); // the Call itself
    let old_term = std::mem::replace(
        &mut f.blocks[b.idx()].term,
        Terminator::Jump {
            target: map_block(BlockId(0)),
        },
    );

    // The call's destination keeps its identity and its register: a single
    // return copies into it directly, several return through a phi that also
    // carries that register, so de-SSA elides the copy either way.
    let (cont_phis, ret_value) = if ret_sites.len() == 1 {
        (Vec::new(), ret_sites[0].1)
    } else {
        let pv = f.new_value(f.value_ty(dst), f.value_reg(dst));
        (
            vec![Phi {
                dst: pv,
                incoming: ret_sites.clone(),
            }],
            pv,
        )
    };
    let mut cont_instrs = vec![Instr::Copy {
        dst,
        src: ret_value,
    }];
    cont_instrs.extend(tail);

    // Successors of the original terminator now see the continuation, not the
    // block the call was in.
    for s in old_term.successors() {
        for phi in f.blocks[s.idx()].phis.iter_mut() {
            for (p, _) in phi.incoming.iter_mut() {
                if *p == b {
                    *p = cont;
                }
            }
        }
    }

    f.blocks.extend(blocks);
    f.blocks.push(Block {
        phis: cont_phis,
        instrs: cont_instrs,
        term: old_term,
        handler: None,
    });

    // The caller now references whatever the callee did.
    let _ = f.natives.merge(&g.natives)?;
    if !g.float_types.is_empty() {
        f.float_types.extend_from_slice(&g.float_types);
        f.float_types.sort_unstable();
        f.float_types.dedup();
    }
    Ok(())
}
