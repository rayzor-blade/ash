//! Optimization passes over the typed IR, and the manager that sequences
//! them.
//!
//! # Invariants every pass preserves
//!
//! * [`verify`](super::verify::verify) still succeeds afterwards. In
//!   particular the value table stays dense and fully defined: a pass that
//!   deletes instructions calls [`compact_values`] before returning.
//! * **Trap regions are never crossed.** No instruction moves between blocks
//!   with different [`Block::handler`] fields, no may-throw instruction moves
//!   at all, and no pass removes the last may-throw instruction of a block
//!   covered by a handler — that would delete the block's exceptional edge
//!   and with it a predecessor of the handler.
//! * **`ClobberAll` is a hard barrier.** No load is reused, and nothing is
//!   hoisted, across a call, `Asm`, or region marker.
//! * **Cells are memory.** `CellGet`/`CellSet`/`CellIncr`/`CellDecr` carry
//!   [`AliasClass::Cell`](super::analysis::AliasClass::Cell) and take part in
//!   the same aliasing rules as heap accesses.
//! * **Live ranges stay register-correct.** De-SSA assigns every value to
//!   [`ValueData::reg`], and lowering guarantees that assignment is
//!   conflict-free only for the live ranges it built. Any rewrite that
//!   lengthens a value's live range therefore first gives that value a
//!   private register through [`privatize`], and refuses the rewrite when
//!   that is impossible (the value is a `Param`, whose register is fixed by
//!   the calling convention).
//!
//! # Passes that grow the function
//!
//! [`TailRecursionElim`], [`Inlining`] and [`ScalarReplacement`] make up
//! [`OptLevel::O3`] and run in that order, ahead of the O2 pipeline. The
//! ordering is forced by HL's object protocol rather than chosen: `new C(...)`
//! passes the fresh object straight into the constructor call, so every
//! allocation escapes until that call has been inlined, and escape analysis
//! run before inlining finds nothing at all.

use super::analysis::{clobbers_all, write_class, AliasClass, CfgInfo};
use super::ir::*;
use super::module::{ModuleInfo, NO_MODULE_INFO};
use anyhow::{bail, Result};

pub mod celldse;
pub mod cellfwd;
pub mod dce;
pub mod escape;
pub mod fma;
pub mod gvn;
pub mod inline;
pub mod licm;
pub mod nullcheck;
pub mod sroa;
pub mod tre;

pub use celldse::DeadCellStoreElim;
pub use cellfwd::CellForwarding;
pub use dce::DeadCodeElim;
pub use fma::FmaPeephole;
pub use gvn::GlobalValueNumbering;
pub use inline::Inlining;
pub use licm::{LoopAllocHoisting, LoopInvariantCodeMotion};
pub use nullcheck::NullCheckElim;
pub use sroa::ScalarReplacement;
pub use tre::TailRecursionElim;

// ---------------------------------------------------------------------------
// pass interface
// ---------------------------------------------------------------------------

/// What a pass did, for observability. All counters are cumulative over the
/// pass's own fixed-point iterations.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct PassStats {
    /// Instructions or phis removed from the function.
    pub eliminated: usize,
    /// Instructions moved into a loop preheader.
    pub hoisted: usize,
    /// Instruction pairs merged into one (`Mul` + `Add` -> `Fma`).
    pub fused: usize,
    /// Uses redirected to a different, equivalent value.
    pub replaced: usize,
    /// Self-recursive tail calls turned into back edges.
    pub tail_calls: usize,
    /// Call sites replaced by the callee's body.
    pub inlined: usize,
    /// Instructions added to the function.
    pub added: usize,
    /// Allocations replaced by SSA values for their fields.
    pub allocs_removed: usize,
    /// Field slots promoted to SSA values.
    pub fields_scalarized: usize,
}

impl PassStats {
    pub fn changed(&self) -> bool {
        *self != PassStats::default()
    }

    pub fn merge(&mut self, other: PassStats) {
        self.eliminated += other.eliminated;
        self.hoisted += other.hoisted;
        self.fused += other.fused;
        self.replaced += other.replaced;
        self.tail_calls += other.tail_calls;
        self.inlined += other.inlined;
        self.added += other.added;
        self.allocs_removed += other.allocs_removed;
        self.fields_scalarized += other.fields_scalarized;
    }
}

/// Knobs the manager hands to every pass.
#[derive(Debug, Clone, Copy)]
pub struct PassOptions {
    /// Form [`Instr::Fma`] from float `Mul` + `Add`/`Sub` pairs. On by
    /// default: fusion is what every reference implementation of the HL
    /// numerics does. Turn it off for a strict-IEEE, per-operation-rounding
    /// pipeline.
    pub fma: bool,
    /// Run [`verify`](super::verify::verify) after every pass and report the
    /// offending pass by name.
    pub verify_each: bool,
    /// Upper bound on manager rounds when running to a fixed point.
    pub max_rounds: usize,
    /// Largest callee, in instructions, that [`Inlining`] will copy. The
    /// default (40) covers the shapes inlining exists for in HL — constructors,
    /// field accessors, small arithmetic helpers — without pulling in whole
    /// methods.
    pub inline_max_callee: usize,
    /// How many inlines deep one [`Inlining`] run nests. This is what caps
    /// recursive inlining; the default (2) is one level of callee plus one
    /// level of what that callee calls.
    pub inline_max_depth: usize,
    /// Ceiling on the caller's instruction count, checked before every inline.
    /// Because it bounds the *function* rather than one step, repeated manager
    /// rounds cannot grow a function without bound even though each round
    /// starts its depth count over — which is what keeps the
    /// inlining-versus-DCE fixed point terminating. Default 400.
    pub inline_max_function: usize,
}

impl Default for PassOptions {
    fn default() -> Self {
        PassOptions {
            fma: true,
            verify_each: false,
            max_rounds: 4,
            inline_max_callee: 40,
            inline_max_depth: 2,
            inline_max_function: 400,
        }
    }
}

/// An AIR v2 optimization pass.
/// Diagnostic: how resolvable is each `CallClosure` target, by reaching
/// definitions? Counts only -- `ASH_DEVIRT_SURVEY=1` prints them.
///
/// Answers whether a static devirtualisation pass is worth writing, and in
/// which form: a single reaching `StaticClosure` can be rewritten to a direct
/// call for free, several need a guard, and anything else is out of reach
/// without runtime feedback.
pub fn survey_closure_targets(f: &Function, m: &dyn crate::v2::module::ModuleInfo) -> (usize, usize, usize) {
    use crate::v2::ir::Instr;
    if std::env::var("ASH_DEVIRT_SURVEY").is_err() {
        return (0, 0, 0);
    }
    // ValueId -> the instruction that defines it, and whether it is a phi.
    let mut def_static: std::collections::HashMap<u32, usize> = Default::default();
    let mut phi_incoming: std::collections::HashMap<u32, Vec<u32>> = Default::default();
    for b in &f.blocks {
        for phi in &b.phis {
            phi_incoming.insert(phi.dst.0, phi.incoming.iter().map(|(_, v)| v.0).collect());
        }
        for ins in &b.instrs {
            if let Instr::StaticClosure { dst, fun } = ins {
                def_static.insert(dst.0, *fun);
            }
        }
    }
    // CallMethod does not need a reaching-definitions walk: the receiver's
    // static type plus the field slot names the target, provided no subtype
    // overrides that slot. Counted here by distinct (receiver type, slot) so
    // the size of that opportunity is visible next to the closure one.
    let mut methods: std::collections::HashSet<(u32, usize)> = Default::default();
    let mut method_sites = 0usize;
    let mut method_resolved = 0usize;
    for b in &f.blocks {
        for ins in &b.instrs {
            if let Instr::CallMethod { field, args, .. } = ins {
                method_sites += 1;
                if let Some(recv) = args.first() {
                    let ty = f.values[recv.0 as usize].ty;
                    methods.insert((ty.0, *field));
                    if m.method_target(ty, *field).is_some() {
                        method_resolved += 1;
                    }
                }
            }
        }
    }

    let (mut single, mut multi, mut unknown) = (0, 0, 0);
    for b in &f.blocks {
        for ins in &b.instrs {
            let Instr::CallClosure { fun, .. } = ins else { continue };
            if def_static.contains_key(&fun.0) {
                single += 1;
            } else if let Some(inc) = phi_incoming.get(&fun.0) {
                if !inc.is_empty() && inc.iter().all(|v| def_static.contains_key(v)) {
                    multi += 1;
                } else {
                    unknown += 1;
                }
            } else {
                unknown += 1;
            }
        }
    }
    if single + multi + unknown + method_sites > 0 {
        eprintln!(
            "[devirt-survey] fn={} single={single} phi-all-static={multi} \
             unresolved={unknown} method-sites={method_sites} \
             method-resolved={method_resolved} method-distinct={}",
            f.findex.map(|n| n.to_string()).unwrap_or_else(|| "?".into()),
            methods.len()
        );
    }
    (single, multi, unknown)
}

pub trait Pass {
    fn name(&self) -> &'static str;
    /// Run once over the function, to the pass's own fixed point where that
    /// is cheaper than another manager round.
    fn run(&self, f: &mut Function, opts: &PassOptions) -> Result<PassStats>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OptLevel {
    /// Nothing runs; the IR is left exactly as lowered.
    O0,
    /// Cleanups that cannot grow the function: null-check elimination and
    /// dead-code elimination.
    O1,
    /// Rewrites that never grow the function: null-check elimination, GVN/CSE,
    /// LICM, the FMA peephole, and dead-code elimination, run to a fixed
    /// point.
    O2,
    /// Everything in [`OptLevel::O2`], preceded by the three passes that can
    /// grow a function: tail-recursion elimination, inlining, and scalar
    /// replacement of aggregates, in that order.
    ///
    /// The order is the whole point. In HL `new C(...)` hands the fresh object
    /// straight to its constructor, so escape analysis finds nothing until
    /// inlining has removed that call; and the O2 passes run *after* SROA so
    /// that GVN, LICM and DCE clean up the arithmetic it exposes.
    ///
    /// Inlining needs callee bodies, which only [`PassManager::with_module`]
    /// can supply — `PassManager::new(OptLevel::O3)` still builds the full
    /// pipeline, but its inliner finds no callee and does nothing.
    O3,
}

/// Per-pass statistics of one [`PassManager::run`].
#[derive(Debug, Clone, Default)]
pub struct PassReport {
    /// Manager rounds executed before reaching the fixed point.
    pub rounds: usize,
    /// One entry per pass, in pipeline order, summed over all rounds.
    pub per_pass: Vec<(&'static str, PassStats)>,
}

impl PassReport {
    pub fn total(&self) -> PassStats {
        let mut t = PassStats::default();
        for (_, s) in &self.per_pass {
            t.merge(*s);
        }
        t
    }

    pub fn stats_for(&self, name: &str) -> PassStats {
        self.per_pass
            .iter()
            .find(|(n, _)| *n == name)
            .map(|(_, s)| *s)
            .unwrap_or_default()
    }

    pub fn changed(&self) -> bool {
        self.total().changed()
    }
}

/// Sequences passes over a function, either by opt level or from an explicit
/// pass list, and reports what each one did.
///
/// The lifetime is the module info the inliner borrows; a pipeline built
/// without one is `PassManager<'static>`.
pub struct PassManager<'m> {
    passes: Vec<Box<dyn Pass + 'm>>,
    /// Passes run once, after the main pipeline reaches its fixed point.
    ///
    /// For a pass that must only see what the others have already declined.
    /// Allocation hoisting is the case: it competes with scalar replacement
    /// for the same allocations, and SROA's answer — remove the object —
    /// beats hoisting's — stop re-creating it. SROA needs a later round to see
    /// past a loop's dead header phi, so a hoister running inside the loop
    /// would take the allocation before SROA got there and quietly settle for
    /// the weaker result.
    final_passes: Vec<Box<dyn Pass + 'm>>,
    opts: PassOptions,
}

impl PassManager<'static> {
    /// The pipeline for an optimization level, without module info. At
    /// [`OptLevel::O3`] the inliner is present but inert.
    pub fn new(level: OptLevel) -> Self {
        PassManager::with_module(level, &NO_MODULE_INFO)
    }
}

impl<'m> PassManager<'m> {
    /// The pipeline for an optimization level, with the module info the
    /// inliner asks for callee bodies.
    pub fn with_module(level: OptLevel, info: &'m dyn ModuleInfo) -> Self {
        let passes: Vec<Box<dyn Pass + 'm>> = match level {
            OptLevel::O0 => vec![],
            OptLevel::O1 => vec![Box::new(NullCheckElim), Box::new(DeadCodeElim)],
            OptLevel::O2 => vec![
                Box::new(CellForwarding),
                Box::new(DeadCellStoreElim),
                Box::new(NullCheckElim),
                Box::new(GlobalValueNumbering),
                Box::new(LoopInvariantCodeMotion),
                Box::new(FmaPeephole),
                Box::new(DeadCodeElim),
            ],
            OptLevel::O3 => vec![
                Box::new(TailRecursionElim),
                Box::new(Inlining::new(info)),
                // SROA stays directly after the inliner. Moving it behind
                // NullCheckElim and GVN was tried and changed nothing:
                // measured with ASH_SROA_WHY, every refusal on this corpus is
                // "phi merge", and those phis are real loop-carried merges
                // rather than inliner artifacts the cleanup could remove.
                Box::new(ScalarReplacement),
                Box::new(CellForwarding),
                Box::new(DeadCellStoreElim),
                Box::new(NullCheckElim),
                Box::new(GlobalValueNumbering),
                Box::new(LoopInvariantCodeMotion),
                Box::new(FmaPeephole),
                Box::new(DeadCodeElim),
            ],
        };
        // Allocation hoisting runs after the fixed point, never inside it —
        // see `PassManager::final_passes`.
        let final_passes: Vec<Box<dyn Pass + 'm>> = if matches!(level, OptLevel::O3) {
            vec![Box::new(LoopAllocHoisting)]
        } else {
            Vec::new()
        };
        PassManager {
            passes,
            final_passes,
            opts: PassOptions::default(),
        }
    }

    /// An explicit pipeline, run in the order given.
    pub fn with_passes(passes: Vec<Box<dyn Pass + 'm>>) -> Self {
        PassManager {
            passes,
            final_passes: Vec::new(),
            opts: PassOptions::default(),
        }
    }

    pub fn with_options(mut self, opts: PassOptions) -> Self {
        self.opts = opts;
        self
    }

    pub fn options(&self) -> &PassOptions {
        &self.opts
    }

    pub fn options_mut(&mut self) -> &mut PassOptions {
        &mut self.opts
    }

    pub fn pass_names(&self) -> Vec<&'static str> {
        self.passes.iter().map(|p| p.name()).collect()
    }

    /// Run the pipeline until no pass reports a change, capped by
    /// [`PassOptions::max_rounds`].
    pub fn run(&self, f: &mut Function) -> Result<PassReport> {
        let mut report = PassReport {
            rounds: 0,
            per_pass: self
                .passes
                .iter()
                .map(|p| (p.name(), PassStats::default()))
                .collect(),
        };
        if self.passes.is_empty() {
            return Ok(report);
        }
        for _ in 0..self.opts.max_rounds.max(1) {
            report.rounds += 1;
            let mut round_changed = false;
            for (i, pass) in self.passes.iter().enumerate() {
                let stats = pass.run(f, &self.opts)?;
                if self.opts.verify_each {
                    super::verify::verify(f).map_err(|e| {
                        anyhow::anyhow!("{} broke the IR: {e}\n{}", pass.name(), f.dump())
                    })?;
                }
                round_changed |= stats.changed();
                report.per_pass[i].1.merge(stats);
            }
            if !round_changed {
                break;
            }
        }

        // The fixed point is reached; now the last-resort passes get their turn.
        for pass in &self.final_passes {
            let stats = pass.run(f, &self.opts)?;
            if self.opts.verify_each {
                super::verify::verify(f).map_err(|e| {
                    anyhow::anyhow!("{} broke the IR: {e}\n{}", pass.name(), f.dump())
                })?;
            }
            match report.per_pass.iter_mut().find(|(n, _)| *n == pass.name()) {
                Some((_, acc)) => acc.merge(stats),
                None => report.per_pass.push((pass.name(), stats)),
            }
        }
        Ok(report)
    }
}

// ---------------------------------------------------------------------------
// shared utilities
// ---------------------------------------------------------------------------

/// Where a value is defined. `slot` is 0 for a phi and `k + 1` for
/// instruction `k`, matching the verifier's position encoding.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DefSite {
    pub block: BlockId,
    pub slot: usize,
}

impl DefSite {
    /// Instruction index, or `None` for a phi.
    #[inline]
    pub fn instr_idx(&self) -> Option<usize> {
        self.slot.checked_sub(1)
    }
}

/// Definition site of every value.
pub fn def_sites(f: &Function) -> Vec<Option<DefSite>> {
    let mut out = vec![None; f.values.len()];
    for (b, blk) in f.blocks.iter().enumerate() {
        let bid = BlockId(b as u32);
        for phi in &blk.phis {
            out[phi.dst.idx()] = Some(DefSite {
                block: bid,
                slot: 0,
            });
        }
        for (k, ins) in blk.instrs.iter().enumerate() {
            if let Some(d) = ins.dst() {
                out[d.idx()] = Some(DefSite {
                    block: bid,
                    slot: k + 1,
                });
            }
        }
    }
    out
}

/// Values defined by a `Param`, whose register is fixed by the calling
/// convention and therefore cannot be privatized.
pub fn param_values(f: &Function) -> Vec<bool> {
    let mut out = vec![false; f.values.len()];
    for blk in &f.blocks {
        for ins in &blk.instrs {
            if let Instr::Param { dst, .. } = ins {
                out[dst.idx()] = true;
            }
        }
    }
    out
}

/// How many distinct things claim each HL register: values assigned to it by
/// de-SSA, cells backed by it, and raw `Asm` register operands.
///
/// A register claimed exactly once holds one value for the whole function, so
/// that value's live range can be extended freely.
pub struct RegClaims {
    counts: Vec<usize>,
}

impl RegClaims {
    pub fn build(f: &Function) -> Self {
        let mut counts = vec![0usize; f.reg_types.len()];
        let bump = |r: u32, counts: &mut Vec<usize>| {
            let i = r as usize;
            if i >= counts.len() {
                counts.resize(i + 1, 0);
            }
            counts[i] += 1;
        };
        for v in &f.values {
            bump(v.reg, &mut counts);
        }
        for c in &f.cells {
            bump(c.reg, &mut counts);
        }
        for blk in &f.blocks {
            for ins in &blk.instrs {
                if let Instr::Asm { reg, .. } = ins {
                    bump(*reg, &mut counts);
                }
            }
        }
        RegClaims { counts }
    }

    /// True when `v` is the only claimant of its register.
    pub fn is_exclusive(&self, f: &Function, v: ValueId) -> bool {
        self.counts
            .get(f.value_reg(v) as usize)
            .copied()
            .unwrap_or(0)
            <= 1
    }
}

/// Give `v` a register of its own so its live range may be extended.
///
/// Returns `false` — leaving the function untouched — when `v` is a `Param`
/// that shares its register: an argument must be read from the register the
/// caller passed it in, so the transformation that wanted the longer live
/// range has to be refused.
pub fn privatize(f: &mut Function, v: ValueId, claims: &mut RegClaims, is_param: bool) -> bool {
    if claims.is_exclusive(f, v) {
        return true;
    }
    if is_param {
        return false;
    }
    let ty = f.value_ty(v);
    let old = f.value_reg(v) as usize;
    let new = f.new_reg(ty);
    claims.counts[old] -= 1;
    if new as usize >= claims.counts.len() {
        claims.counts.resize(new as usize + 1, 0);
    }
    claims.counts[new as usize] = 1;
    f.values[v.idx()].reg = new;
    true
}

/// Blocks that can be entered through an exceptional edge.
pub fn handler_blocks(f: &Function) -> Vec<bool> {
    let mut out = vec![false; f.blocks.len()];
    for blk in &f.blocks {
        if let Some(h) = blk.handler {
            out[h.idx()] = true;
        }
        if let Terminator::Trap { handler, .. } = blk.term {
            out[handler.idx()] = true;
        }
    }
    out
}

/// True when `v` feeds a phi of a handler block.
///
/// Such phis must stay trivial — there is no program point on an exceptional
/// edge to run a copy on — so no pass rewrites the value or its register.
pub fn feeds_handler_phi(f: &Function, handlers: &[bool], v: ValueId) -> bool {
    f.blocks.iter().enumerate().any(|(b, blk)| {
        handlers[b]
            && blk
                .phis
                .iter()
                .any(|p| p.incoming.iter().any(|&(_, s)| s == v))
    })
}

/// Redirect every use of `from` to `to`. Returns the number of uses rewritten.
pub fn replace_all_uses(f: &mut Function, from: ValueId, to: ValueId) -> usize {
    let mut n = 0;
    for blk in f.blocks.iter_mut() {
        for phi in blk.phis.iter_mut() {
            for (_, v) in phi.incoming.iter_mut() {
                if *v == from {
                    *v = to;
                    n += 1;
                }
            }
        }
        for ins in blk.instrs.iter_mut() {
            ins.map_uses(&mut |v| {
                if v == from {
                    n += 1;
                    to
                } else {
                    v
                }
            });
        }
        blk.term.map_uses(&mut |v| {
            if v == from {
                n += 1;
                to
            } else {
                v
            }
        });
    }
    n
}

/// Rebuild the value table so that it contains exactly the defined values,
/// renumbering every reference.
///
/// Passes that delete instructions call this before returning, which is what
/// keeps `verify`'s "every value is defined" invariant true.
pub fn compact_values(f: &mut Function) -> Result<()> {
    let n = f.values.len();
    let mut keep = vec![false; n];
    for blk in &f.blocks {
        for phi in &blk.phis {
            keep[phi.dst.idx()] = true;
        }
        for ins in &blk.instrs {
            if let Some(d) = ins.dst() {
                keep[d.idx()] = true;
            }
        }
    }
    if keep.iter().all(|&k| k) {
        return Ok(());
    }
    let mut map = vec![u32::MAX; n];
    let mut values = Vec::with_capacity(n);
    for (v, &k) in keep.iter().enumerate() {
        if k {
            map[v] = values.len() as u32;
            values.push(f.values[v].clone());
        }
    }
    let mut dangling: Option<u32> = None;
    {
        let mut remap = |v: ValueId| -> ValueId {
            let m = map[v.idx()];
            if m == u32::MAX {
                dangling.get_or_insert(v.0);
                v
            } else {
                ValueId(m)
            }
        };
        for blk in f.blocks.iter_mut() {
            for phi in blk.phis.iter_mut() {
                phi.dst = remap(phi.dst);
                for (_, v) in phi.incoming.iter_mut() {
                    *v = remap(*v);
                }
            }
            for ins in blk.instrs.iter_mut() {
                ins.map_dst(&mut remap);
                ins.map_uses(&mut remap);
            }
            blk.term.map_uses(&mut remap);
        }
    }
    if let Some(v) = dangling {
        bail!("value v{} is used after its definition was removed", v);
    }
    f.values = values;
    Ok(())
}

/// True when no instruction between the two program points may write storage
/// aliasing `class`, on **any** path from `from` to `to` — loops included.
///
/// `from` and `to` are instruction indices; the scan is exclusive at both
/// ends. The region is the set of blocks reachable from `from`'s block that
/// can also reach `to`'s block; a block that a path may re-enter is scanned in
/// full rather than from (or up to) the anchor position.
pub fn clobber_free(
    f: &Function,
    cfg: &CfgInfo,
    class: AliasClass,
    from: (BlockId, usize),
    to: (BlockId, usize),
) -> bool {
    let nb = f.blocks.len();
    let (db, dk) = from;
    let (ub, uk) = to;

    // Blocks reachable from db via at least one edge.
    let mut fwd = vec![false; nb];
    let mut stack: Vec<BlockId> = cfg.succs[db.idx()].clone();
    while let Some(x) = stack.pop() {
        if fwd[x.idx()] {
            continue;
        }
        fwd[x.idx()] = true;
        stack.extend(cfg.succs[x.idx()].iter().copied());
    }
    // Blocks that reach ub via at least one edge.
    let mut bwd = vec![false; nb];
    let mut stack: Vec<BlockId> = cfg.preds[ub.idx()].clone();
    while let Some(x) = stack.pop() {
        if bwd[x.idx()] {
            continue;
        }
        bwd[x.idx()] = true;
        stack.extend(cfg.preds[x.idx()].iter().copied());
    }

    let db_reentered = fwd[db.idx()];
    let ub_reentered = bwd[ub.idx()];

    for b in 0..nb {
        let bid = BlockId(b as u32);
        let in_region = (bid == db || fwd[b]) && (bid == ub || bwd[b]);
        if !in_region {
            continue;
        }
        let blk = &f.blocks[b];
        let start = if bid == db && !db_reentered {
            dk + 1
        } else {
            0
        };
        let end = if bid == ub && !ub_reentered {
            uk
        } else {
            blk.instrs.len()
        };
        if start >= end {
            continue;
        }
        for ins in &blk.instrs[start..end] {
            if clobbers_all(ins) {
                return false;
            }
            if let Some(w) = write_class(ins) {
                if w.may_alias(class) {
                    return false;
                }
            }
        }
    }
    true
}
