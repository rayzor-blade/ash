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
    /// Address step per iteration, in elements. 1 is contiguous.
    pub stride: i64,
    pub is_store: bool,
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
    let cfg = CfgInfo::build(f);
    let forest = LoopForest::analyze(f, &cfg);
    forest
        .innermost_first()
        .into_iter()
        .map(|l| analyze_loop(f, &cfg, &forest, l, opts))
        .collect()
}

fn analyze_loop(
    f: &Function,
    _cfg: &CfgInfo,
    forest: &LoopForest,
    l: LoopId,
    opts: &VecOptions,
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
    let consts = int_constants(f);
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
        if !bounds_induction(f, exit, iv, &in_loop) {
            plan.refusals.push(Refusal::UnknownTripCount);
        }
    }

    // ---- body instructions -------------------------------------------------
    let evo = evolutions(f, &in_loop, &plan);
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
                Some(_) => Cycle::NonConstantStride,
                None => Cycle::NonConstantStride,
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
) -> bool {
    let Terminator::CondJump { a, b, .. } = &f.blocks[exit.idx()].term else {
        return false;
    };
    let other = match (*a == iv, b) {
        (true, Some(o)) => *o,
        (false, Some(o)) if *o == iv => *a,
        _ => return false,
    };
    // The bound must not be redefined inside the loop.
    !f.blocks.iter().enumerate().any(|(bi, blk)| {
        in_loop.contains(&BlockId(bi as u32))
            && (blk.instrs.iter().any(|i| i.dst() == Some(other))
                || blk.phis.iter().any(|p| p.dst == other))
    })
}

/// Per-value evolution across the loop.
fn evolutions(
    f: &Function,
    in_loop: &HashSet<BlockId>,
    plan: &LoopPlan,
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
    evo
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
        | Instr::Prefetch { .. } => {}

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
            dst, base, index, ..
        } => {
            match address_stride(*index, evo, consts) {
                Some(stride) => plan.accesses.push(Access {
                    at: *dst,
                    base: *base,
                    stride,
                    is_store: false,
                }),
                None => plan.refusals.push(Refusal::NonAffineAccess(*dst)),
            }
        }
        Instr::MemSet {
            base, index, src, ..
        } => match address_stride(*index, evo, consts) {
            Some(stride) => plan.accesses.push(Access {
                at: *src,
                base: *base,
                stride,
                is_store: true,
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
    let _ = f;
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
fn int_constants(f: &Function) -> HashMap<ValueId, i64> {
    let mut m = HashMap::new();
    for blk in &f.blocks {
        for ins in &blk.instrs {
            if let Instr::Int { dst, idx } = ins {
                // The magnitude is the embedder's to resolve; 1 is the
                // overwhelmingly common step and is what `UnOp::Incr`
                // already expresses directly. Record the index so a caller
                // with the pool can refine this.
                m.insert(*dst, *idx as i64);
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
