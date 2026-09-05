//! Loop vectorization opportunity analysis.
//!
//! Answers one question per loop: *could this loop's iterations run several at
//! a time, and if not, what exactly stops it?* Nothing here rewrites the IR —
//! the transform is a separate step, and it should not be written until this
//! analysis is right, because every unsound vectorizer in the wild is one that
//! transformed on an assumption this file is supposed to prove.
//!
//! The refusal is as much the product as the plan. A pass that silently
//! vectorizes nothing is indistinguishable from one that is not running, so
//! every loop this declines carries a [`Refusal`] naming the instruction or
//! the dependence that stopped it. That list is the work queue.
//!
//! # What it looks for
//!
//! A loop is a candidate when it has a single **affine induction variable**
//! (`i = i + c` around the back edge), a **guard** that bounds it, and a body
//! whose every instruction is either
//!
//! * elementwise arithmetic on values that vary per iteration,
//! * loop-invariant (the same in every iteration, so broadcastable),
//! * a memory access whose address is affine in the induction variable, or
//! * an accumulation into a loop-carried phi — a **reduction**.
//!
//! Anything else — a call, an allocation, a field access on an object that
//! moves, a second induction variable, a nested loop — refuses the loop and
//! says so.
//!
//! # What it deliberately does not assume
//!
//! Two memory accesses in the same loop may be to the same address in
//! different iterations. Vectorizing then reorders reads and writes that the
//! scalar loop ordered, so this analysis refuses unless it can prove
//! independence: same base and provably different affine offsets, or bases
//! that alias analysis separates. "The programmer surely did not alias those"
//! is not a proof and is not accepted here.

use crate::v2::analysis::{CfgInfo, LoopForest, LoopId};
use crate::v2::ir::*;
use std::collections::{HashMap, HashSet};

/// Why a loop cannot be vectorized. One per obstacle, in discovery order.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Refusal {
    /// The loop contains another loop. Vectorize the inner one first.
    NotInnermost,
    /// More than one block can leave the loop, so lanes would exit at
    /// different iterations. Legal with masking; not attempted here.
    MultipleExits,
    /// No `i = i + c` phi around the back edge.
    NoInductionVariable,
    /// Two or more values step independently per iteration.
    MultipleInductionVariables,
    /// The induction step is not a compile-time constant.
    NonConstantStride,
    /// The loop's exit test does not compare the induction variable against
    /// a loop-invariant bound, so the trip count is unknown.
    UnknownTripCount,
    /// An instruction whose semantics this analysis will not widen.
    /// The string names the instruction kind.
    UnwidenableInstr(&'static str),
    /// A call of any kind. Widening would need the callee vectorized too.
    Call,
    /// Allocation inside the body: each lane would need its own object.
    Allocation,
    /// A value crosses iterations in a way that is not a recognized
    /// reduction — lane `n` would need lane `n-1`'s result.
    LoopCarried(ValueId),
    /// A memory access whose address is not affine in the induction
    /// variable, so its lanes are not contiguous.
    NonAffineAccess(ValueId),
    /// A store and another access that this analysis cannot prove
    /// independent across iterations.
    MayAlias { store: ValueId, other: ValueId },
    /// A reduction over floats. Vectorizing reassociates the accumulation,
    /// which changes the result — allowed only when the caller opts in.
    FloatReductionNeedsReassoc(ValueId),
}

/// How a value behaves across the iterations of one loop.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Evolution {
    /// Same value in every iteration: broadcast it once.
    Invariant,
    /// The induction variable itself: `base + stride * i`.
    Induction { stride: i64 },
    /// Varies per iteration with no closed form — fine for elementwise work,
    /// which is what a lane holds.
    Varying,
}

/// A recognized accumulator: `acc = acc op x` around the back edge.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Reduction {
    /// The header phi carrying the accumulator.
    pub phi: ValueId,
    /// The value the back edge feeds in.
    pub next: ValueId,
    /// The combining operation, which is also how the lanes collapse after
    /// the loop.
    pub op: BinOp,
    /// Whether the accumulator is a float, which makes the collapse a
    /// reassociation.
    pub is_float: bool,
}

/// A memory access classified against the induction variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Access {
    /// The instruction's destination (loads) or stored value (stores) — an
    /// identity for reporting.
    pub at: ValueId,
    pub base: ValueId,
    /// Address step per iteration, in whatever unit `kind` indexes: elements
    /// for [`MemAccess::Array`], BYTES for the rest. Contiguity is therefore
    /// a question about the kind and the element width, not about the number
    /// 1 — see [`Access::contiguous_stride`].
    pub stride: i64,
    pub is_store: bool,
    /// How the address is formed, which is what says whether `stride` counts
    /// elements or bytes.
    pub kind: MemAccess,
    /// The type of the value loaded or stored, for its byte width.
    pub elem: TypeRef,
}

impl Access {
    /// The stride this access would have if it walked its elements back to
    /// back, given their width in bytes. `None` when the width is unknown,
    /// which is a refusal rather than a guess: reading it wrong means a
    /// vector that covers memory the loop never touched.
    pub fn contiguous_stride(&self, elem_bytes: Option<u32>) -> Option<i64> {
        match self.kind {
            MemAccess::Array => Some(1),
            MemAccess::I8 => Some(1),
            MemAccess::I16 => Some(2),
            MemAccess::Mem => elem_bytes.map(i64::from),
        }
    }
}

/// The exit test that bounds the induction variable.
///
/// The analysis has to find this to rule out [`Refusal::UnknownTripCount`];
/// reporting it costs nothing and is what a transform needs to build a vector
/// loop, which otherwise has to re-derive the same fact.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Bound {
    /// The block whose terminator leaves the loop.
    pub exit: BlockId,
    /// The loop-invariant value the induction variable is compared against.
    pub limit: ValueId,
    /// True when the induction variable is the first operand of the test.
    pub iv_first: bool,
}

/// The verdict for one loop.
#[derive(Debug, Clone)]
pub struct LoopPlan {
    pub loop_id: LoopId,
    pub header: BlockId,
    /// Empty when the loop can be vectorized.
    pub refusals: Vec<Refusal>,
    /// The induction variable, when there is exactly one.
    pub induction: Option<(ValueId, i64)>,
    /// The exit test bounding it, when the trip count is knowable.
    pub bound: Option<Bound>,
    pub reductions: Vec<Reduction>,
    pub accesses: Vec<Access>,
    /// Instructions in the body, as a first cost signal: widening a loop
    /// whose body is two instructions rarely repays the prologue.
    pub body_size: usize,
    /// For a multi-exit refusal: what each exiting block ends with. A second
    /// exit that throws is a guard (a bounds check), not real divergence.
    pub exit_terms: Vec<(BlockId, &'static str)>,
    /// Blocks whose only way out of the loop leads to a throw — bounds
    /// checks and null checks. The loop is vectorizable *provided* the
    /// transform hoists each of these to a single pre-loop check covering
    /// the whole vector range; a plan with these non-empty is not a plan the
    /// transform may take without doing that.
    pub guard_exits: Vec<BlockId>,
}

impl LoopPlan {
    pub fn vectorizable(&self) -> bool {
        self.refusals.is_empty()
    }
}

/// Options that change what counts as legal.
#[derive(Debug, Clone, Copy)]
pub struct VecOptions {
    /// Permit float reductions, which reassociate and therefore change
    /// results. Off by default: the same policy question FMA contraction
    /// already documents, and the answer belongs to the embedder.
    pub allow_float_reassoc: bool,
    /// Loops with fewer body instructions than this are reported as
    /// vectorizable but flagged by [`LoopPlan::body_size`]; the transform
    /// decides. Kept here so the threshold has one home.
    pub min_body: usize,
}

impl Default for VecOptions {
    fn default() -> Self {
        VecOptions {
            allow_float_reassoc: false,
            min_body: 2,
        }
    }
}

/// Analyze every loop in `f`.
pub fn analyze(f: &Function, opts: &VecOptions) -> Vec<LoopPlan> {
    analyze_with(f, opts, &|_| None)
}

/// [`analyze`] with the embedder's i32 pool, which is what makes constant
/// magnitudes — strides, bounds — readable rather than guessed from indices.
pub fn analyze_with(
    f: &Function,
    opts: &VecOptions,
    pool: &dyn Fn(usize) -> Option<i32>,
) -> Vec<LoopPlan> {
    let cfg = CfgInfo::build(f);
    let forest = LoopForest::analyze(f, &cfg);
    forest
        .innermost_first()
        .into_iter()
        .map(|l| analyze_loop(f, &cfg, &forest, l, opts, pool))
        .collect()
}

fn analyze_loop(
    f: &Function,
    _cfg: &CfgInfo,
    forest: &LoopForest,
    l: LoopId,
    opts: &VecOptions,
    pool: &dyn Fn(usize) -> Option<i32>,
) -> LoopPlan {
    let lp = forest.get(l);
    let header = lp.header;
    let in_loop: HashSet<BlockId> = lp.blocks.iter().copied().collect();
    let body_size: usize = lp
        .blocks
        .iter()
        .map(|b| f.blocks[b.idx()].instrs.len())
        .sum();

    let mut plan = LoopPlan {
        loop_id: l,
        header,
        refusals: Vec::new(),
        induction: None,
        bound: None,
        reductions: Vec::new(),
        accesses: Vec::new(),
        body_size,
        exit_terms: Vec::new(),
        guard_exits: Vec::new(),
    };

    if !lp.children.is_empty() {
        plan.refusals.push(Refusal::NotInnermost);
        return plan;
    }

    // Which blocks can leave, and whether each leaves NORMALLY.
    //
    // An exit whose out-of-loop target only ever reaches a `Throw` is not
    // divergence — it is a guard, and in HL that is overwhelmingly an array
    // bounds check. The scalar loop never takes it on a run that completes,
    // so it does not make lanes exit at different iterations; what it does
    // require is that the check be proven for the whole vector range before
    // the loop, which is a hoist the transform must perform. Counting those
    // as ordinary exits refused most loops in the corpus for a branch no
    // completing run takes.
    let mut exits: Vec<BlockId> = Vec::new();
    let mut guards: Vec<BlockId> = Vec::new();
    for b in &lp.blocks {
        let leaves: Vec<BlockId> = f.blocks[b.idx()]
            .term
            .successors()
            .into_iter()
            .filter(|s| !in_loop.contains(s))
            .collect();
        if leaves.is_empty() {
            continue;
        }
        if leaves.iter().all(|&t| always_throws(f, t, &in_loop)) {
            guards.push(*b);
        } else {
            exits.push(*b);
        }
    }
    plan.guard_exits = guards;
    if exits.len() > 1 {
        plan.refusals.push(Refusal::MultipleExits);
        plan.exit_terms = exits
            .iter()
            .map(|b| (*b, term_name(&f.blocks[b.idx()].term)))
            .collect();
        return plan;
    }
    if exits.is_empty() {
        // Every way out throws: the loop has no normal exit at all.
        plan.refusals.push(Refusal::MultipleExits);
        return plan;
    }

    // ---- induction variables and reductions --------------------------------
    // Both live as header phis whose back-edge incoming is defined in the
    // loop. An IV's incoming is `phi + constant`; anything else that closes
    // the cycle is a reduction if it is `phi op x`, and a barrier otherwise.
    let def_block = definition_blocks(f);
    let consts = int_constants(f, pool);
    let mut inductions: Vec<(ValueId, i64)> = Vec::new();

    for phi in &f.blocks[header.idx()].phis {
        let Some(&(_, back)) = phi
            .incoming
            .iter()
            .find(|(p, _)| in_loop.contains(p))
        else {
            continue; // no back-edge source: not loop-carried
        };
        match classify_cycle(f, phi.dst, back, &def_block, &in_loop, &consts) {
            Cycle::Induction(stride) => inductions.push((phi.dst, stride)),
            Cycle::Reduction(op) => {
                let is_float = f.is_float(f.value_ty(phi.dst));
                if is_float && !opts.allow_float_reassoc {
                    plan.refusals
                        .push(Refusal::FloatReductionNeedsReassoc(phi.dst));
                } else {
                    plan.reductions.push(Reduction {
                        phi: phi.dst,
                        next: back,
                        op,
                        is_float,
                    });
                }
            }
            Cycle::NonConstantStride => plan.refusals.push(Refusal::NonConstantStride),
            Cycle::Opaque => plan.refusals.push(Refusal::LoopCarried(phi.dst)),
        }
    }

    match inductions.len() {
        0 => plan.refusals.push(Refusal::NoInductionVariable),
        1 => plan.induction = Some(inductions[0]),
        _ => plan
            .refusals
            .push(Refusal::MultipleInductionVariables),
    }

    // ---- the exit test must bound the induction variable -------------------
    if let (Some((iv, _)), Some(&exit)) = (plan.induction, exits.first()) {
        match bounds_induction(f, exit, iv, &in_loop) {
            Some(b) => plan.bound = Some(b),
            None => plan.refusals.push(Refusal::UnknownTripCount),
        }
    }

    // ---- body instructions -------------------------------------------------
    let evo = evolutions(f, &in_loop, &plan, &consts);
    for b in &lp.blocks {
        for ins in &f.blocks[b.idx()].instrs {
            classify_instr(f, ins, &evo, &consts, &mut plan);
        }
    }

    // ---- memory dependence -------------------------------------------------
    check_memory(&mut plan);

    plan
}

enum Cycle {
    Induction(i64),
    Reduction(BinOp),
    NonConstantStride,
    Opaque,
}

/// What closes the cycle from a header phi back to itself.
fn classify_cycle(
    f: &Function,
    phi: ValueId,
    back: ValueId,
    def_block: &HashMap<ValueId, (usize, usize)>,
    in_loop: &HashSet<BlockId>,
    consts: &HashMap<ValueId, i64>,
) -> Cycle {
    let Some(&(b, k)) = def_block.get(&back) else {
        return Cycle::Opaque;
    };
    if !in_loop.contains(&BlockId(b as u32)) {
        return Cycle::Opaque;
    }
    match &f.blocks[b].instrs[k] {
        // `i = i + c` / `i = i - c`
        Instr::BinOp { op, a, b: rhs, .. } if matches!(op, BinOp::Add | BinOp::Sub) => {
            let (other, phi_side) = if *a == phi {
                (*rhs, true)
            } else if *rhs == phi {
                (*a, matches!(op, BinOp::Add))
            } else {
                return reduction_or_opaque(f, phi, b, k);
            };
            match consts.get(&other) {
                Some(&c) if phi_side => Cycle::Induction(if *op == BinOp::Sub { -c } else { c }),
                // `c - phi` alternates rather than steps.
                Some(_) => Cycle::NonConstantStride,
                // Not a step this can measure -- but it is still a cycle that
                // combines the phi with something under an associative
                // operation, which is a REDUCTION. `sum += a[i]` arrives here,
                // and calling it a broken induction is why the corpus appeared
                // to contain almost none: the single most common vectorizable
                // loop there is was being reported as an unmeasurable stride.
                None => reduction_or_opaque(f, phi, b, k),
            }
        }
        Instr::UnOp {
            op: UnOp::Incr,
            src,
            ..
        } if *src == phi => Cycle::Induction(1),
        Instr::UnOp {
            op: UnOp::Decr,
            src,
            ..
        } if *src == phi => Cycle::Induction(-1),
        _ => reduction_or_opaque(f, phi, b, k),
    }
}

/// A cycle that is not an induction variable is a reduction when the
/// closing instruction combines the phi with something else under an
/// associative operation.
fn reduction_or_opaque(f: &Function, phi: ValueId, b: usize, k: usize) -> Cycle {
    match &f.blocks[b].instrs[k] {
        Instr::BinOp { op, a, b: rhs, .. }
            if (*a == phi || *rhs == phi) && is_associative(*op) =>
        {
            Cycle::Reduction(*op)
        }
        _ => Cycle::Opaque,
    }
}

/// Operations whose lanes can be combined in any order — which is what makes
/// a partial-sum-per-lane collapse legal. Integer only by construction here;
/// float associativity is the caller's policy decision, checked separately.
fn is_associative(op: BinOp) -> bool {
    matches!(
        op,
        BinOp::Add | BinOp::Mul | BinOp::And | BinOp::Or | BinOp::Xor
    )
}

/// Whether `exit`'s terminator compares `iv` against something the loop does
/// not change.
fn bounds_induction(
    f: &Function,
    exit: BlockId,
    iv: ValueId,
    in_loop: &HashSet<BlockId>,
) -> Option<Bound> {
    let Terminator::CondJump { a, b, .. } = &f.blocks[exit.idx()].term else {
        return None;
    };
    let (other, iv_first) = match (*a == iv, b) {
        (true, Some(o)) => (*o, true),
        (false, Some(o)) if *o == iv => (*a, false),
        _ => return None,
    };
    // The bound must not be redefined inside the loop.
    let redefined = f.blocks.iter().enumerate().any(|(bi, blk)| {
        in_loop.contains(&BlockId(bi as u32))
            && (blk.instrs.iter().any(|i| i.dst() == Some(other))
                || blk.phis.iter().any(|p| p.dst == other))
    });
    if redefined {
        return None;
    }
    Some(Bound {
        exit,
        limit: other,
        iv_first,
    })
}

/// Per-value evolution across the loop.
fn evolutions(
    f: &Function,
    in_loop: &HashSet<BlockId>,
    plan: &LoopPlan,
    consts: &HashMap<ValueId, i64>,
) -> HashMap<ValueId, Evolution> {
    let mut evo: HashMap<ValueId, Evolution> = HashMap::new();
    if let Some((iv, stride)) = plan.induction {
        evo.insert(iv, Evolution::Induction { stride });
    }
    for r in &plan.reductions {
        evo.insert(r.phi, Evolution::Varying);
    }
    // Anything defined inside the loop varies; everything else is invariant.
    for (bi, blk) in f.blocks.iter().enumerate() {
        if !in_loop.contains(&BlockId(bi as u32)) {
            continue;
        }
        for phi in &blk.phis {
            evo.entry(phi.dst).or_insert(Evolution::Varying);
        }
        for ins in &blk.instrs {
            if let Some(d) = ins.dst() {
                evo.entry(d).or_insert(Evolution::Varying);
            }
        }
    }

    // Then walk the affine ones back out of Varying. An index is almost never
    // the induction variable itself: HL scales it to bytes (`i << 2`) or
    // offsets it (`i + k`), and treating the result as varying refused every
    // byte-indexed array access in the corpus -- the second most common
    // refusal after a call in the body.
    //
    // Only forms whose step is EXACTLY derivable are taken, and only from a
    // step already known, so this cannot invent an affine value: repeated to
    // a fixed point because a chain like `(i + k) << 2` needs its inner term
    // classified first.
    let order: Vec<(BlockId, usize)> = f
        .blocks
        .iter()
        .enumerate()
        .filter(|(bi, _)| in_loop.contains(&BlockId(*bi as u32)))
        .flat_map(|(bi, blk)| (0..blk.instrs.len()).map(move |k| (BlockId(bi as u32), k)))
        .collect();
    loop {
        let mut grew = false;
        for &(b, k) in &order {
            let ins = &f.blocks[b.idx()].instrs[k];
            let Some(dst) = ins.dst() else { continue };
            if !matches!(evo_of(&evo, dst), Evolution::Varying) {
                continue;
            }
            let Some(step) = affine_step(ins, &evo, consts) else {
                continue;
            };
            evo.insert(dst, Evolution::Induction { stride: step });
            grew = true;
        }
        if !grew {
            break;
        }
    }
    evo
}

/// The per-iteration step of `ins`'s result, when it is an exact affine
/// function of something whose step is already known.
///
/// A step of 0 is invariant and a non-zero one is an induction; either way it
/// is a value whose address arithmetic the widener can reason about. Anything
/// not listed -- a multiply by a value that is not a known constant, a shift
/// by a varying amount, division -- has no exact step and stays varying.
fn affine_step(
    ins: &Instr,
    evo: &HashMap<ValueId, Evolution>,
    consts: &HashMap<ValueId, i64>,
) -> Option<i64> {
    // The step of an operand, and its value when it is a known constant.
    let step = |v: ValueId| match evo_of(evo, v) {
        Evolution::Induction { stride } => Some(stride),
        Evolution::Invariant => Some(0),
        Evolution::Varying => None,
    };
    let konst = |v: ValueId| consts.get(&v).copied();
    match ins {
        Instr::Copy { src, .. } => step(*src),
        Instr::UnOp {
            op: UnOp::Incr,
            src,
            ..
        } => step(*src),
        Instr::UnOp {
            op: UnOp::Decr,
            src,
            ..
        } => step(*src),
        Instr::UnOp {
            op: UnOp::Neg,
            src,
            ..
        } => step(*src).and_then(|s| s.checked_neg()),
        Instr::BinOp { op, a, b, .. } => match op {
            BinOp::Add => step(*a)?.checked_add(step(*b)?),
            BinOp::Sub => step(*a)?.checked_sub(step(*b)?),
            // A scale is affine only against a constant: `i * k` steps by
            // `stride * k`, but `i * j` for a varying `j` is quadratic.
            BinOp::Mul => match (step(*a), konst(*a), step(*b), konst(*b)) {
                (Some(sa), _, Some(0), Some(k)) => sa.checked_mul(k),
                (Some(0), Some(k), Some(sb), _) => sb.checked_mul(k),
                _ => None,
            },
            BinOp::Shl => match (step(*a), step(*b), konst(*b)) {
                (Some(sa), Some(0), Some(k)) if (0..63).contains(&k) => {
                    sa.checked_mul(1i64 << k)
                }
                _ => None,
            },
            _ => None,
        },
        _ => None,
    }
}

fn evo_of(evo: &HashMap<ValueId, Evolution>, v: ValueId) -> Evolution {
    *evo.get(&v).unwrap_or(&Evolution::Invariant)
}

/// Classify one body instruction, pushing refusals for anything unwidenable.
fn classify_instr(
    f: &Function,
    ins: &Instr,
    evo: &HashMap<ValueId, Evolution>,
    consts: &HashMap<ValueId, i64>,
    plan: &mut LoopPlan,
) {
    match ins {
        // Elementwise arithmetic and the constants/copies feeding it.
        Instr::BinOp { .. }
        | Instr::UnOp { .. }
        | Instr::Fma { .. }
        | Instr::Copy { .. }
        | Instr::Int { .. }
        | Instr::Float { .. }
        | Instr::Bool { .. }
        | Instr::Null { .. }
        | Instr::Intrinsic { .. }
        | Instr::Param { .. }
        | Instr::NullCheck { .. }
        | Instr::Assert
        | Instr::Prefetch { .. }
        // A position marker names the line the body runs at; widened or
        // not, it is the same line.
        | Instr::Pos { .. } => {}

        // Numeric conversions are elementwise, and `UnsafeCast` is a pointer
        // reinterpretation the machine does not execute at all. The rest —
        // `ToDyn` boxes, `SafeCast` may throw, `ToVirtual` allocates — are
        // not.
        Instr::Cast {
            kind:
                CastKind::ToSFloat
                | CastKind::ToUFloat
                | CastKind::ToInt
                | CastKind::UnsafeCast,
            ..
        } => {}

        // Memory: widenable exactly when the address walks a constant stride.
        Instr::MemGet {
            kind,
            dst,
            base,
            index,
        } => {
            match address_stride(*index, evo, consts) {
                Some(stride) => plan.accesses.push(Access {
                    at: *dst,
                    base: *base,
                    stride,
                    is_store: false,
                    kind: *kind,
                    elem: f.value_ty(*dst),
                }),
                None => plan.refusals.push(Refusal::NonAffineAccess(*dst)),
            }
        }
        Instr::MemSet {
            kind,
            base,
            index,
            src,
        } => match address_stride(*index, evo, consts) {
            Some(stride) => plan.accesses.push(Access {
                at: *src,
                base: *base,
                stride,
                is_store: true,
                kind: *kind,
                elem: f.value_ty(*src),
            }),
            None => plan.refusals.push(Refusal::NonAffineAccess(*src)),
        },

        // A field access is a load at a fixed offset from a pointer. It
        // widens only if the pointer itself is invariant — a per-iteration
        // object means gathering, which this analysis does not model.
        Instr::FieldGet { dst, obj, .. } => {
            if !matches!(evo_of(evo, *obj), Evolution::Invariant) {
                plan.refusals.push(Refusal::NonAffineAccess(*dst));
            }
        }
        Instr::FieldSet { obj, src, .. } => {
            if !matches!(evo_of(evo, *obj), Evolution::Invariant) {
                plan.refusals.push(Refusal::NonAffineAccess(*src));
            }
        }

        Instr::Call { .. } | Instr::CallMethod { .. } | Instr::CallClosure { .. } => {
            plan.refusals.push(Refusal::Call)
        }
        Instr::New { .. } | Instr::EnumAlloc { .. } | Instr::MakeEnum { .. } => {
            plan.refusals.push(Refusal::Allocation)
        }

        other => plan
            .refusals
            .push(Refusal::UnwidenableInstr(instr_name(other))),
    }
}

/// The per-iteration step of an address, when it has one.
///
/// `index` is affine when it is the induction variable (stride = its step) or
/// a constant (stride 0 — the same slot every iteration, which is a broadcast
/// for loads and a conflict for stores).
fn address_stride(
    index: ValueId,
    evo: &HashMap<ValueId, Evolution>,
    consts: &HashMap<ValueId, i64>,
) -> Option<i64> {
    if consts.contains_key(&index) {
        return Some(0);
    }
    match evo_of(evo, index) {
        Evolution::Induction { stride } => Some(stride),
        Evolution::Invariant => Some(0),
        Evolution::Varying => None,
    }
}

/// Reject any store this analysis cannot separate from another access.
///
/// Two accesses are independent when their bases differ AND alias analysis
/// separates those bases — which AIR cannot yet answer for two arbitrary
/// pointers. So the rule is deliberately strict: a store may share its base
/// only with accesses at the same stride (lane `n` touches element `n` in
/// both, so the pairing is preserved), and a store to a fixed slot
/// (`stride == 0`) conflicts with itself across lanes.
fn check_memory(plan: &mut LoopPlan) {
    for (i, s) in plan.accesses.iter().enumerate() {
        if !s.is_store {
            continue;
        }
        if s.stride == 0 {
            plan.refusals.push(Refusal::MayAlias {
                store: s.at,
                other: s.at,
            });
            continue;
        }
        for (j, o) in plan.accesses.iter().enumerate() {
            if i == j {
                continue;
            }
            if s.base == o.base && s.stride != o.stride {
                plan.refusals.push(Refusal::MayAlias {
                    store: s.at,
                    other: o.at,
                });
            }
            // Different bases: unprovable today. A store next to any other
            // access through a base this analysis cannot separate is refused.
            if s.base != o.base {
                plan.refusals.push(Refusal::MayAlias {
                    store: s.at,
                    other: o.at,
                });
            }
        }
    }
    plan.refusals.dedup();
}

// ── small helpers ───────────────────────────────────────────────────────────

/// `value -> (block, instruction index)` for every instruction-defined value.
fn definition_blocks(f: &Function) -> HashMap<ValueId, (usize, usize)> {
    let mut m = HashMap::new();
    for (b, blk) in f.blocks.iter().enumerate() {
        for (k, ins) in blk.instrs.iter().enumerate() {
            if let Some(d) = ins.dst() {
                m.insert(d, (b, k));
            }
        }
    }
    m
}

/// Integer constants, by the value they define. The pool index is resolved by
/// the embedder, so a stride is only known when the constant is one the IR
/// itself carries — which `Instr::Int` does not (it holds a pool index). The
/// map therefore records *which* values are integer constants, and callers
/// treat an unknown magnitude as a non-constant stride.
fn int_constants(f: &Function, pool: &dyn Fn(usize) -> Option<i32>) -> HashMap<ValueId, i64> {
    let mut m = HashMap::new();
    for blk in &f.blocks {
        for ins in &blk.instrs {
            if let Instr::Int { dst, idx } = ins {
                // The real value when the caller supplied a pool, and the
                // index as a stand-in when it did not. The stand-in is what
                // this did unconditionally, and it is wrong for any constant
                // whose index differs from its value -- a step of 1 stored at
                // index 2 reads as a stride of 2, and the loop is then
                // refused for a non-unit stride it does not have.
                let v = f.int_at(*idx, pool).map(|x| x as i64);
                m.insert(*dst, v.unwrap_or(*idx as i64));
            }
        }
    }
    m
}

fn instr_name(i: &Instr) -> &'static str {
    match i {
        Instr::DynGet { .. } => "DynGet",
        Instr::DynSet { .. } => "DynSet",
        Instr::Cast { .. } => "Cast",
        Instr::GetGlobal { .. } => "GetGlobal",
        Instr::SetGlobal { .. } => "SetGlobal",
        Instr::EnumIndex { .. } => "EnumIndex",
        Instr::EnumField { .. } => "EnumField",
        Instr::SetEnumField { .. } => "SetEnumField",
        Instr::CellGet { .. } => "CellGet",
        Instr::CellSet { .. } => "CellSet",
        Instr::CellRef { .. } => "CellRef",
        Instr::CellIncr { .. } => "CellIncr",
        Instr::CellDecr { .. } => "CellDecr",
        Instr::Unref { .. } => "Unref",
        Instr::SetRef { .. } => "SetRef",
        Instr::RefData { .. } => "RefData",
        Instr::RefOffset { .. } => "RefOffset",
        Instr::ArraySize { .. } => "ArraySize",
        Instr::TypeConst { .. } => "TypeConst",
        Instr::GetType { .. } => "GetType",
        Instr::GetTID { .. } => "GetTID",
        Instr::Bytes { .. } => "Bytes",
        Instr::String { .. } => "String",
        Instr::StaticClosure { .. } => "StaticClosure",
        Instr::InstanceClosure { .. } => "InstanceClosure",
        Instr::VirtualClosure { .. } => "VirtualClosure",
        Instr::EndTrap { .. } => "EndTrap",
        Instr::Asm { .. } => "Asm",
        _ => "other",
    }
}

fn term_name(t: &Terminator) -> &'static str {
    match t {
        Terminator::Ret { .. } => "Ret",
        Terminator::Jump { .. } => "Jump",
        Terminator::CondJump { .. } => "CondJump",
        Terminator::Switch { .. } => "Switch",
        Terminator::Throw { .. } => "Throw",
        Terminator::Rethrow { .. } => "Rethrow",
        Terminator::Trap { .. } => "Trap",
    }
}

/// Whether `start`, and everything reachable from it without re-entering the
/// loop, ends in a throw. Bounded by the block count; a cycle outside the
/// loop that never throws simply answers `false`.
fn always_throws(f: &Function, start: BlockId, in_loop: &HashSet<BlockId>) -> bool {
    let mut seen: HashSet<BlockId> = HashSet::new();
    let mut work = vec![start];
    let mut saw_throw = false;
    while let Some(b) = work.pop() {
        if in_loop.contains(&b) || !seen.insert(b) {
            continue;
        }
        match &f.blocks[b.idx()].term {
            Terminator::Throw { .. } | Terminator::Rethrow { .. } => saw_throw = true,
            // Anything that returns or traps is a normal way out of the
            // function, so this path is not a guard.
            Terminator::Ret { .. } | Terminator::Trap { .. } => return false,
            t => work.extend(t.successors()),
        }
    }
    saw_throw
}
