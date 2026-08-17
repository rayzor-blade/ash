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
//! * **Live ranges stay register-correct.** De-SSA assigns every value to
//!   [`ValueData::reg`], and lowering guarantees that assignment is
//!   conflict-free only for the live ranges it built. Any rewrite that
//!   lengthens a value's live range therefore first gives that value a
//!   private register through [`privatize`], and refuses the rewrite when
//!   that is impossible (the value is a `Param`, whose register is fixed by
//!   the calling convention).

use super::ir::*;
use anyhow::{bail, Result};

pub mod fma;

pub use fma::FmaPeephole;

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
}

impl Default for PassOptions {
    fn default() -> Self {
        PassOptions {
            fma: true,
            verify_each: false,
            max_rounds: 4,
        }
    }
}

/// An AIR v2 optimization pass.
pub trait Pass {
    fn name(&self) -> &'static str;
    /// Run once over the function, to the pass's own fixed point where that
    /// is cheaper than another manager round.
    fn run(&self, f: &mut Function, opts: &PassOptions) -> Result<PassStats>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptLevel {
    /// Nothing runs; the IR is left exactly as lowered.
    O0,
    /// Cleanups that cannot grow the function.
    O1,
    /// The full pipeline, run to a fixed point.
    O2,
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
pub struct PassManager {
    passes: Vec<Box<dyn Pass>>,
    opts: PassOptions,
}

impl PassManager {
    /// The pipeline for an optimization level.
    pub fn new(level: OptLevel) -> Self {
        let passes: Vec<Box<dyn Pass>> = match level {
            OptLevel::O0 => vec![],
            OptLevel::O1 => vec![],
            OptLevel::O2 => vec![Box::new(FmaPeephole)],
        };
        PassManager {
            passes,
            opts: PassOptions::default(),
        }
    }

    /// An explicit pipeline, run in the order given.
    pub fn with_passes(passes: Vec<Box<dyn Pass>>) -> Self {
        PassManager {
            passes,
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
