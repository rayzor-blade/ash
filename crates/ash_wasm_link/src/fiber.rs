//! The fiber transform, built one checkable piece at a time.
//!
//! The design is `docs/wasm-fibers.md`. This module holds the rewrite; what
//! is here so far is its foundation, which is the part with no precedent we
//! can copy.
//!
//! # Emptying the operand stack across a call
//!
//! To unwind out of a call and later rewind back into it, every value that is
//! live at that call has to be saved somewhere addressable. Locals are;
//! the operand stack is not -- a suspended frame cannot enumerate it, and a
//! resumed one cannot rebuild it.
//!
//! Binaryen solves this with a separate `Flatten` pass that rewrites the whole
//! function into a form where nothing is ever on the stack across a call.
//! That pass aborts on `try_table`, which every module ash links contains, so
//! it is not available to us (`docs/wasm-fibers.md` has the stack trace).
//!
//! [`empty_stack_at_calls`] does the same job locally and without a tree. At
//! each call site the operand stack is popped into locals and pushed straight
//! back: the values are unchanged and in the same order, and each one now also
//! sits in a local that an unwind can spill and a rewind can restore. The
//! validator running alongside the decoder is what makes this possible, since
//! it is what knows the type of each operand.
//!
//! Two things bound it, both of which the transform reports rather than
//! assumes:
//!
//! - A call whose stack holds nothing but its own arguments needs no locals at
//!   all: the arguments are consumed by the call, so there is nothing to
//!   preserve. This is the overwhelmingly common case in ash's output, so the
//!   transform is close to free.
//! - A value pushed *before* the current control frame began belongs to an
//!   enclosing frame and cannot be popped: wasm validation forbids a frame
//!   from touching operands below its base. Flattening those is exactly the
//!   global restructure this module exists to avoid, so a function containing
//!   one is refused rather than half-instrumented.

use std::collections::{BTreeMap, BTreeSet};

use anyhow::{anyhow, bail, Result};
use wasm_encoder::{Instruction, ValType};
use wasmparser::Operator;

use crate::cursor::{rewrite_module, Cursor};

/// What emptying the stack cost, over a whole module.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Spills {
    /// Call sites seen, direct and indirect.
    pub calls: usize,
    /// Call sites that needed no locals because the stack held only the
    /// call's own arguments.
    pub already_empty: usize,
    /// Values pushed through a local at a call site, summed over the module.
    /// This is the cost: two instructions each.
    pub moved: usize,
    /// Of those, the ones that actually needed preserving -- the values that
    /// were on the stack under the call's own arguments.
    pub live: usize,
    /// Locals added, summed over every function.
    pub locals: usize,
    /// Functions left alone because something in them cannot be instrumented:
    /// a value belonging to an enclosing frame live across a call, or a tail
    /// call, whose frame is gone before the callee returns.
    pub refused: usize,
}

/// Rewrite `bytes` so that at every call site in `instrument`, every live
/// operand also lives in a local.
///
/// `instrument` decides which functions are touched; a fiber transform passes
/// the suspend set from [`crate::suspend`]. Functions outside it are copied
/// through unchanged, as is every section but the code section.
///
/// The result is behaviour-preserving on its own: this is the preparation an
/// unwind needs, not the unwind.
pub fn empty_stack_at_calls(
    bytes: &[u8],
    instrument: &dyn Fn(u32) -> bool,
) -> Result<(Vec<u8>, Spills)> {
    // Which functions cannot be instrumented is settled before anything is
    // emitted. Discovering it mid-body would leave a function half done --
    // valid, and wrong in exactly the silent way this crate is written
    // against -- so the walk runs twice and the first result is thrown away.
    let mut refused: std::collections::BTreeSet<u32> = std::collections::BTreeSet::new();
    rewrite_module(bytes, |index, c, op| {
        if instrument(index) && !refused.contains(&index) && !can_instrument(c, op)? {
            refused.insert(index);
        }
        c.emit(op)
    })?;

    let mut spills = Spills {
        refused: refused.len(),
        ..Spills::default()
    };
    // Scratch locals, reused across the call sites of one function: a value
    // is only in one at a time, so `n` slots of a type serve every site that
    // needs at most `n` of it.
    let mut pool: BTreeMap<ValType, Vec<u32>> = BTreeMap::new();
    let mut current = u32::MAX;

    let out = rewrite_module(bytes, |index, c, op| {
        if index != current {
            current = index;
            pool.clear();
        }
        if !instrument(index) || refused.contains(&index) {
            return c.emit(op);
        }
        let arity = match op {
            Operator::Call { .. } | Operator::CallIndirect { .. } => call_arity(c, op)?,
            _ => return c.emit(op),
        };
        spills.calls += 1;

        // Everything the current frame owns, minus the arguments the call is
        // about to consume. The first pass guaranteed the frame's base is 0,
        // so the frame owns the whole stack.
        let owned = c.stack_height();
        if owned.saturating_sub(arity) == 0 {
            spills.already_empty += 1;
            return c.emit(op);
        }

        // Pop the whole stack, top first, then push it back. The arguments
        // have to go through locals too -- they are above the values we are
        // after, and wasm has no way to reach past them.
        let total = owned as usize;
        let mut used: BTreeMap<ValType, usize> = BTreeMap::new();
        let mut slots = Vec::with_capacity(total);
        for depth in 0..total {
            let ty = c
                .operand(depth)
                .ok_or_else(|| anyhow!("operand {depth} is past the bottom of the stack"))?
                .ok_or_else(|| {
                    anyhow!("operand {depth} has no type, so this code is unreachable")
                })?;
            let ty = encode_val_type(ty)?;
            let nth = used.entry(ty).or_default();
            let at = *nth;
            *nth += 1;
            let have = pool.entry(ty).or_default();
            while have.len() <= at {
                have.push(c.reserve_local(ty));
                spills.locals += 1;
            }
            slots.push(have[at]);
        }
        for &l in &slots {
            c.emit_new(&Instruction::LocalSet(l));
        }
        for &l in slots.iter().rev() {
            c.emit_new(&Instruction::LocalGet(l));
        }
        spills.moved += total;
        spills.live += (owned - arity) as usize;
        c.emit(op)
    })?;
    Ok((out, spills))
}

/// Whether this operator is one the transform can handle in this function.
///
/// Two shapes it cannot. A value that was on the stack before the current
/// control frame began belongs to an enclosing frame, and wasm forbids a
/// frame from popping below its own base -- reaching those is the global
/// restructure this module exists to avoid. And a tail call's frame is gone
/// before the callee returns, so there is no frame to unwind out of or rewind
/// back into; the suspend analysis records the edge, the rewrite declines it.
fn can_instrument(c: &Cursor, op: &Operator<'_>) -> Result<bool> {
    match op {
        Operator::ReturnCall { .. } | Operator::ReturnCallIndirect { .. } => Ok(false),
        Operator::Call { .. } | Operator::CallIndirect { .. } => {
            // A non-zero base is exactly the bad case: those values are live
            // across the call and this frame may not pop them.
            let base = c
                .frame(0)
                .ok_or_else(|| anyhow!("no control frame at a call"))?
                .height;
            Ok(base == 0)
        }
        _ => Ok(true),
    }
}

/// How many values the call about to be emitted will consume.
fn call_arity(c: &Cursor, op: &Operator<'_>) -> Result<u32> {
    use wasmparser::WasmModuleResources as _;
    let r = c.resources();
    let type_index = match op {
        Operator::Call { function_index } => r
            .type_index_of_function(*function_index)
            .ok_or_else(|| anyhow!("callee {function_index} has no type"))?,
        // An indirect call also pops the table index, which is not an
        // argument but is on the stack and so has to be counted.
        Operator::CallIndirect { type_index, .. } => *type_index,
        other => bail!("{other:?} is not a call"),
    };
    let sub = r
        .sub_type_at(type_index)
        .ok_or_else(|| anyhow!("type {type_index} is not in the module"))?;
    let params = match &sub.composite_type.inner {
        wasmparser::CompositeInnerType::Func(f) => f.params().len() as u32,
        other => bail!("a call names a non-function type {other:?}"),
    };
    Ok(match op {
        Operator::CallIndirect { .. } => params + 1,
        _ => params,
    })
}

fn encode_val_type(ty: wasmparser::ValType) -> Result<ValType> {
    use wasm_encoder::reencode::Reencode as _;
    wasm_encoder::reencode::RoundtripReencoder
        .val_type(ty)
        .map_err(|e| anyhow!("re-encoding an operand type: {e}"))
}

// ------------------------------------------------------- resuming a frame

/// The globals the transform runs on.
///
/// All three are `i32`. `state` is 0 while running, 1 while unwinding and 2
/// while rewinding, which is Asyncify's encoding and is kept because the
/// runtime side is easier to read against a published one. `data` points at a
/// two-word record in linear memory holding the side stack's current and end
/// pointers. `resume` carries the call ordinal a rewind is heading for, and is
/// cleared where it lands.
#[derive(Debug, Clone, Copy)]
pub struct Machine {
    pub state: u32,
    pub data: u32,
    pub resume: u32,
}

/// Running while unwinding out of a suspend.
const UNWINDING: i32 = 1;
/// Running while rewinding back into one.
const REWINDING: i32 = 2;

/// How much of the transform to apply.
#[derive(Debug, Clone, Copy)]
pub enum Drive {
    /// Jump machinery only, with the resume value set from outside. The state
    /// machine reads and writes linear memory, which makes it useless for
    /// testing the jumps on their own -- so the jumps keep a mode where they
    /// can be driven directly, and are tested that way.
    Resume(u32),
    /// Prologue, ladders and epilogues: a function that can suspend and be
    /// resumed.
    Full(Machine),
}

impl Drive {
    fn resume(&self) -> u32 {
        match self {
            Drive::Resume(g) => *g,
            Drive::Full(m) => m.resume,
        }
    }

    fn machine(&self) -> Option<&Machine> {
        match self {
            Drive::Resume(_) => None,
            Drive::Full(m) => Some(m),
        }
    }
}

/// What the dispatch cost.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Dispatch {
    /// Functions given a dispatch.
    pub functions: usize,
    /// Functions left alone, for the reasons in [`can_instrument`] or because
    /// a jump target could not be given an empty operand stack.
    pub refused: usize,
    /// Of those, the ones holding a value that cannot be written to linear
    /// memory -- a reference. LLVM's setjmp lowering is not supposed to leave
    /// an `exnref` live across a call, and this is the count that says so.
    pub unsavable: usize,
    /// Ladders emitted, one per control frame that contains a call.
    pub ladders: usize,
    /// Blocks added by those ladders.
    pub blocks: usize,
    /// `br_table` entries emitted, summed.
    pub table_entries: usize,
    /// Resume points whose operands had to go through locals because they
    /// were not all `local.get`s and constants.
    pub spilled: usize,
    /// Resume points that needed no spill at all.
    pub free: usize,
    /// Checks emitted on the way back from a call that could suspend.
    pub epilogues: usize,
    /// Traps emitted in functions that can be on the stack at a suspend but
    /// could not be instrumented. Each one is a place a fiber must not
    /// suspend under; a trap is what turns that from a wrong answer into a
    /// report.
    pub traps: usize,
    /// Side-stack bytes one frame of each instrumented function takes,
    /// summed. The deepest call chain, not this, is what a fiber has to have
    /// room for; this says how heavy an average frame is.
    pub saved_bytes: usize,
}

/// The function frame, which has no opening instruction of its own.
const FUNCTION_FRAME: usize = usize::MAX;

/// The first operator inside a frame, which is where its ladder opens.
///
/// A split there needs no ladder arm: landing on it is landing where ordinary
/// entry already lands, so the arm and the default would go to the same
/// place.
fn first_operator_of(key: usize) -> usize {
    if key == FUNCTION_FRAME {
        0
    } else {
        key + 1
    }
}

/// One entry of the frame chain during a walk.
#[derive(Debug, Clone, Copy)]
struct Enclosing {
    /// Operator index this frame's ladder is keyed by.
    key: usize,
    /// Split position in the enclosing frame that re-enters this one, and
    /// what landing there costs. For a `block` or `loop` that is the opening
    /// itself with an empty stack; for an `if` it is wherever the condition
    /// can be left on the stack.
    entry: Boundary,
    /// Where that boundary is.
    entry_at: usize,
    /// Ladder blocks of this frame still to close.
    open: usize,
    /// Splits of this frame already passed.
    passed: usize,
}

impl Enclosing {
    fn function() -> Self {
        Enclosing {
            key: FUNCTION_FRAME,
            entry: Boundary::default(),
            entry_at: FUNCTION_FRAME,
            open: 0,
            passed: 0,
        }
    }
}

/// A point execution can be resumed at, and what it costs to land there.
#[derive(Debug, Clone, Copy, Default)]
struct Boundary {
    /// Operands to push through locals so the stack is empty here. Zero when
    /// everything above is a `local.get` or a constant, which a rewind can
    /// simply re-execute.
    spill: u32,
    /// Whether landing here means the resumed call is next, so the resume
    /// value must be cleared.
    is_call: bool,
}

/// Where one frame has to be able to jump to, and what reaches each place.
#[derive(Debug, Default, Clone)]
struct Frame {
    /// Split positions in this frame's own sequence, ascending.
    splits: Vec<usize>,
    /// Call ordinals reachable through each split, in the same order.
    through: Vec<Vec<u32>>,
}

impl Frame {
    fn add(&mut self, at: usize, ordinal: u32) {
        match self.splits.binary_search(&at) {
            Ok(i) => self.through[i].push(ordinal),
            Err(i) => {
                self.splits.insert(i, at);
                self.through.insert(i, vec![ordinal]);
            }
        }
    }
}

/// The span of the body over which a reference-typed local might hold a
/// value someone still wants.
///
/// A reference cannot be written to linear memory, so a fiber cannot suspend
/// while one is live. Deciding that exactly needs liveness over the control
/// flow graph; this is the cheap over-approximation that a value is only live
/// somewhere between the first place it is written and the last place it is
/// read. Anything genuinely live across a resume point falls inside that
/// span, so refusing on the span refuses at least as much as the truth --
/// which is the safe direction.
#[derive(Debug, Clone, Copy)]
struct RefSpan {
    first_write: Option<usize>,
    last_read: Option<usize>,
}

impl RefSpan {
    fn covers(&self, at: usize) -> bool {
        match (self.first_write, self.last_read) {
            (Some(w), Some(r)) => w <= at && at <= r,
            _ => false,
        }
    }
}

/// What one function's rewind dispatch has to be able to do.
#[derive(Debug, Default)]
struct Plan {
    /// How many call sites are resume points. An ordinal indexes this range.
    calls: usize,
    /// Every frame that contains a resume point, keyed by its opening.
    frames: BTreeMap<usize, Frame>,
    /// Every split position in the function, with what landing there costs.
    boundaries: BTreeMap<usize, Boundary>,
    /// How many spill locals of each type the function will need, which is
    /// the most any one boundary asks for. Reserved up front rather than on
    /// demand, so the local list -- and therefore the layout of a saved frame
    /// -- is settled before the prologue is emitted.
    pool: BTreeMap<ValType, usize>,
}

/// Work out, for every instrumented function, where a rewind has to jump.
///
/// This runs as its own walk and throws away what it emits. Two things make
/// that worth the second pass: a function that turns out to be uninstrumentable
/// must never have been half rewritten, and a frame's ladder has to be emitted
/// at the top of the frame, before the calls that tell it how many arms it
/// needs have been seen.
fn plan(
    bytes: &[u8],
    instrument: &dyn Fn(u32) -> bool,
    machine: bool,
) -> Result<(BTreeMap<u32, Plan>, usize, usize)> {
    // Where each function's reference-typed locals might still hold
    // something. Its own pass, because a span is only known once the whole
    // body has been seen and a resume point is decided while walking it.
    let spans = if machine {
        ref_spans(bytes, instrument)?
    } else {
        BTreeMap::new()
    };
    let mut plans: BTreeMap<u32, Plan> = BTreeMap::new();
    let mut refused: BTreeSet<u32> = BTreeSet::new();
    let mut unsavable = 0usize;
    let mut chain: Vec<Enclosing> = Vec::new();
    let mut at = 0usize;
    let mut current = u32::MAX;
    // How many operators ending here are a `local.get` or a constant, each of
    // which pushes one value a rewind can recompute for free.
    let mut trivial = 0usize;

    rewrite_module(bytes, |index, c, op| {
        if index != current {
            current = index;
            chain.clear();
            chain.push(Enclosing::function());
            at = 0;
            trivial = 0;
        }
        let watching = instrument(index) && !refused.contains(&index);
        if watching {
            if !can_instrument(c, op)? {
                refused.insert(index);
                plans.remove(&index);
            } else if suspends(op, instrument) && !unreachable_here(c) {
                let (split, spill) = boundary(at, c.stack_height(), trivial);
                let need = spill_types(c, spill)?;
                // A reference cannot go to linear memory, so a fiber must not
                // stop where one is still holding something -- nor where one
                // is on the operand stack.
                let ref_live =
                    machine && spans.get(&index).is_some_and(|f| ref_local_live_at(f, at));
                if ref_live || (machine && need.keys().any(|t| value_size(*t).is_err())) {
                    unsavable += 1;
                    refused.insert(index);
                    plans.remove(&index);
                    at += 1;
                    trivial = 0;
                    return c.emit(op);
                }
                let plan = plans.entry(index).or_default();
                for (ty, n) in need {
                    let have = plan.pool.entry(ty).or_default();
                    *have = (*have).max(n);
                }
                let ordinal = plan.calls as u32;
                plan.calls += 1;
                plan.boundaries.insert(
                    split,
                    Boundary {
                        spill,
                        is_call: true,
                    },
                );
                // The innermost frame jumps to the call; each enclosing one
                // jumps to where the frame below it is re-entered. Those
                // positions were settled when each frame was opened.
                let mut target = split;
                for f in chain.iter().rev() {
                    plan.frames.entry(f.key).or_default().add(target, ordinal);
                    if f.key == FUNCTION_FRAME {
                        break;
                    }
                    plan.boundaries.insert(f.entry_at, f.entry);
                    target = f.entry_at;
                }
            }
        }

        match op {
            Operator::Block { .. } | Operator::Loop { .. } | Operator::TryTable { .. } => {
                // The stack is empty at these, or a call inside would have
                // refused the function for having an enclosing frame's value
                // live across it.
                chain.push(Enclosing {
                    key: at,
                    entry: Boundary::default(),
                    entry_at: at,
                    open: 0,
                    passed: 0,
                });
            }
            Operator::If { .. } => {
                // An `if` pops its condition, so re-entering it means having
                // that value on the stack again -- the same problem as a
                // call's arguments, and the same answer.
                let (entry_at, spill) = boundary(at, c.stack_height(), trivial);
                chain.push(Enclosing {
                    key: at,
                    entry: Boundary {
                        spill,
                        is_call: false,
                    },
                    entry_at,
                    open: 0,
                    passed: 0,
                });
            }
            // The two arms of an `if` are separate sequences and each needs
            // its own ladder, but both are re-entered through the `if`, so
            // they share an entry and not a key.
            Operator::Else => {
                let was = chain.pop().unwrap_or_else(Enclosing::function);
                chain.push(Enclosing { key: at, ..was });
            }
            Operator::End => {
                chain.pop();
            }
            _ => {}
        }
        trivial = if is_trivial(op) { trivial + 1 } else { 0 };
        at += 1;
        c.emit(op)
    })?;

    plans.retain(|i, _| !refused.contains(i));
    Ok((plans, refused.len(), unsavable))
}

/// Where every function's reference-typed locals are written and read.
fn ref_spans(
    bytes: &[u8],
    instrument: &dyn Fn(u32) -> bool,
) -> Result<BTreeMap<u32, BTreeMap<u32, RefSpan>>> {
    let mut all: BTreeMap<u32, BTreeMap<u32, RefSpan>> = BTreeMap::new();
    let mut at = 0usize;
    let mut current = u32::MAX;
    rewrite_module(bytes, |index, c, op| {
        if index != current {
            current = index;
            at = 0;
            if instrument(index) {
                let mut f = BTreeMap::new();
                for (i, ty) in c.local_types()?.iter().enumerate() {
                    if value_size(*ty).is_err() {
                        f.insert(
                            i as u32,
                            RefSpan {
                                first_write: None,
                                last_read: None,
                            },
                        );
                    }
                }
                if !f.is_empty() {
                    all.insert(index, f);
                }
            }
        }
        if let Some(f) = all.get_mut(&index) {
            match op {
                Operator::LocalSet { local_index } | Operator::LocalTee { local_index } => {
                    if let Some(sp) = f.get_mut(local_index) {
                        sp.first_write.get_or_insert(at);
                    }
                }
                Operator::LocalGet { local_index } => {
                    if let Some(sp) = f.get_mut(local_index) {
                        sp.last_read = Some(at);
                    }
                }
                _ => {}
            }
        }
        at += 1;
        c.emit(op)
    })?;
    Ok(all)
}

/// A reference-typed local that might still hold something at `at`.
///
/// Reference-typed locals are not saved -- they cannot be -- so a resume point
/// inside one's span is a place the fiber must not stop.
fn ref_local_live_at(spans: &BTreeMap<u32, RefSpan>, at: usize) -> bool {
    spans.values().any(|sp| sp.covers(at))
}

/// Where to put a jump target for an operator that consumes `height` values.
///
/// If everything on the stack is a `local.get` or a constant, the target goes
/// before them and a rewind re-executes them, which costs nothing. Otherwise
/// it goes at the operator itself and the operands go through locals.
fn boundary(at: usize, height: u32, trivial: usize) -> (usize, u32) {
    if height == 0 {
        (at, 0)
    } else if trivial >= height as usize {
        (at - height as usize, 0)
    } else {
        (at, height)
    }
}

/// Whether a call could be the one a fiber suspends inside.
///
/// A direct call to a function outside the suspend set cannot unwind, so it
/// needs neither a resume point nor a check on the way back. An indirect one
/// could reach anything the analysis had to assume it could.
fn suspends(op: &Operator<'_>, instrument: &dyn Fn(u32) -> bool) -> bool {
    match op {
        Operator::Call { function_index } => instrument(*function_index),
        Operator::CallIndirect { .. } => true,
        _ => false,
    }
}

/// How many locals of each type spilling the top `count` operands needs.
fn spill_types(c: &Cursor, count: u32) -> Result<BTreeMap<ValType, usize>> {
    let mut need: BTreeMap<ValType, usize> = BTreeMap::new();
    for depth in 0..count as usize {
        let ty = c
            .operand(depth)
            .ok_or_else(|| anyhow!("operand {depth} is past the bottom of the stack"))?
            .ok_or_else(|| anyhow!("operand {depth} has no type"))?;
        *need.entry(encode_val_type(ty)?).or_default() += 1;
    }
    Ok(need)
}

/// A value a rewind can recompute rather than restore.
fn is_trivial(op: &Operator<'_>) -> bool {
    matches!(
        op,
        Operator::LocalGet { .. }
            | Operator::I32Const { .. }
            | Operator::I64Const { .. }
            | Operator::F32Const { .. }
            | Operator::F64Const { .. }
    )
}

fn unreachable_here(c: &Cursor) -> bool {
    c.frame(0).map(|f| f.unreachable).unwrap_or(false)
}

/// Give every instrumented function a way to resume at any of its call sites.
///
/// A rewind cannot jump into the middle of a wasm function -- there is no
/// `goto` -- so each control frame gets a ladder: a run of nested blocks whose
/// ends sit at the frame's resume points, and a `br_table` at the top that
/// leaves the ladder at the right one. Jumping into a nested frame is the same
/// problem one level down, so the ladders compose: the outer one jumps to just
/// before the inner frame, the inner one takes over from there. A `loop` needs
/// no special handling, because the locals a rewind restores are what say
/// which iteration this is.
///
/// The operand stack has to be empty where a ladder ends, and that is why
/// [`empty_stack_at_calls`] came first. Where a call's operands are all
/// `local.get`s and constants the target simply goes before them and a rewind
/// re-executes them; otherwise they go through locals, and the target sits
/// between the stores and the loads.
///
/// This is the jump machinery only. Nothing here decides *whether* to rewind:
/// that is [`Drive`], which carries either a value set from outside or the
/// state machine that computes one.
pub fn add_rewind_dispatch(
    bytes: &[u8],
    instrument: &dyn Fn(u32) -> bool,
    drive: Drive,
) -> Result<(Vec<u8>, Dispatch)> {
    let (plans, refused, unsavable) = plan(bytes, instrument, drive.machine().is_some())?;
    let mut report = Dispatch {
        functions: plans.len(),
        refused,
        unsavable,
        ..Dispatch::default()
    };

    let mut chain: Vec<Enclosing> = Vec::new();
    let mut at = 0usize;
    let mut current = u32::MAX;
    let mut pool: BTreeMap<ValType, Vec<u32>> = BTreeMap::new();
    // A ladder opens inside the frame it serves, so it cannot be emitted in
    // the same step as the frame's opening instruction: the cursor's depth
    // only advances once that instruction has been validated.
    let mut pending: Option<usize> = None;
    let mut ordinals_seen = 0u32;
    // Set once per instrumented function, when its locals are settled.
    let mut frame: Option<Saved> = None;

    let out = rewrite_module(bytes, |index, c, op| {
        if index != current {
            current = index;
            chain.clear();
            chain.push(Enclosing::function());
            at = 0;
            pool.clear();
            frame = None;
            ordinals_seen = 0;
            pending = plans.get(&index).map(|_| FUNCTION_FRAME);
        }
        let Some(p) = plans.get(&index) else {
            c.emit(op)?;
            // A function the analysis says can be on the stack at a suspend,
            // but which the rewrite could not instrument, is not safe -- it
            // would carry on running as though the call had returned
            // normally. Refusing it silently is exactly the failure this
            // crate is written against, so it traps instead, in the frame
            // that would have been wrong rather than somewhere later.
            if let (Some(m), true) = (
                drive.machine(),
                instrument(index) && suspends(op, instrument) && !unreachable_here(c),
            ) {
                c.emit_new(&Instruction::GlobalGet(m.state));
                c.emit_new(&Instruction::I32Const(UNWINDING));
                c.emit_new(&Instruction::I32Eq);
                c.emit_new(&Instruction::If(wasm_encoder::BlockType::Empty));
                c.emit_new(&Instruction::Unreachable);
                c.emit_new(&Instruction::End);
                report.traps += 1;
            }
            return Ok(());
        };

        if let Some(key) = pending.take() {
            if key == FUNCTION_FRAME {
                // Reserving every spill local before anything is emitted is
                // what makes the saved-frame layout knowable here, and the
                // same list is what the epilogue writes.
                pool.clear();
                for (&ty, &n) in &p.pool {
                    let slots = pool.entry(ty).or_default();
                    for _ in 0..n {
                        slots.push(c.reserve_local(ty));
                    }
                }
                frame = match drive.machine() {
                    Some(m) => {
                        let scratch = c.reserve_local(ValType::I32);
                        let ordinal = c.reserve_local(ValType::I32);
                        let saved = Saved::new(c, scratch, ordinal)?;
                        // Everything the body does happens inside this, so
                        // any call site can leave by branching to its end.
                        c.open_block(wasm_encoder::BlockType::Empty);
                        emit_prologue(c, m, &saved)?;
                        report.saved_bytes += saved.size as usize;
                        Some(saved)
                    }
                    None => None,
                };
            }
            let opened = open_ladder(c, p, key, drive, &mut report)?;
            if let Some(f) = chain.last_mut() {
                f.open = opened;
            }
        }

        // Landing here means a rewind jumped to this point, so the ladder
        // block whose end is here has to close before the operator is
        // emitted.
        let land = chain
            .last()
            .and_then(|f| p.frames.get(&f.key))
            .filter(|frame| {
                let passed = chain.last().map(|f| f.passed).unwrap_or(0);
                frame.splits.get(passed) == Some(&at)
            })
            .is_some();
        if land {
            let b = p.boundaries.get(&at).copied().unwrap_or_default();
            // A split at the frame's own entry has no ladder arm, so there is
            // no boundary to make an empty stack for -- but the resume value
            // still has to be cleared there, since a rewind reaches it by
            // entering the frame rather than by jumping.
            let key = chain.last().map(|f| f.key).unwrap_or(FUNCTION_FRAME);
            let has_block = at != first_operator_of(key);
            let slots = if has_block && b.spill > 0 {
                spill_stack(c, &pool, b.spill)?
            } else {
                Vec::new()
            };
            if has_block {
                c.close_block()?;
            }
            if b.is_call {
                // Later calls in this function must run normally.
                c.emit_new(&Instruction::I32Const(0));
                c.emit_new(&Instruction::GlobalSet(drive.resume()));
                if b.spill > 0 {
                    report.spilled += 1;
                } else {
                    report.free += 1;
                }
            }
            for &l in slots.iter().rev() {
                c.emit_new(&Instruction::LocalGet(l));
            }
            if let Some(f) = chain.last_mut() {
                f.passed += 1;
                if has_block {
                    f.open -= 1;
                }
            }
        }

        // The body's own last `End`. Everything before it is inside the block
        // a suspending call site branches out of, so this is where that block
        // closes and where the one copy of the save sequence goes.
        if matches!(op, Operator::End) && c.depth() == 1 {
            if let (Some(m), Some(saved)) = (drive.machine(), &frame) {
                // Reaching here normally means returning normally; falling
                // through into the save sequence would be wrong.
                c.emit_new(&Instruction::Return);
                c.close_block()?;
                emit_unwind_exit(c, m, saved)?;
            }
        }

        c.emit(op)?;

        // Coming back from a call that could have suspended, the state says
        // whether this frame is on its way out.
        if let (Some(m), true) = (drive.machine(), suspends(op, instrument)) {
            if let Some(saved) = &frame {
                if !unreachable_here(c) {
                    let ordinal = ordinals_seen;
                    emit_epilogue(c, m, saved, ordinal)?;
                    report.epilogues += 1;
                }
            }
        }
        if suspends(op, instrument) && !unreachable_here(c) {
            ordinals_seen += 1;
        }

        match op {
            Operator::Block { .. } | Operator::Loop { .. } | Operator::TryTable { .. } => {
                chain.push(Enclosing {
                    key: at,
                    entry: Boundary::default(),
                    entry_at: at,
                    open: 0,
                    passed: 0,
                });
                pending = p.frames.contains_key(&at).then_some(at);
            }
            Operator::If { .. } => {
                chain.push(Enclosing {
                    key: at,
                    entry: Boundary::default(),
                    entry_at: at,
                    open: 0,
                    passed: 0,
                });
                pending = p.frames.contains_key(&at).then_some(at);
            }
            Operator::Else => {
                let was = chain.pop().unwrap_or_else(Enclosing::function);
                if was.open != 0 {
                    bail!("a ladder was still open at the `else` of frame {}", was.key);
                }
                chain.push(Enclosing {
                    key: at,
                    open: 0,
                    passed: 0,
                    ..was
                });
                pending = p.frames.contains_key(&at).then_some(at);
            }
            Operator::End => {
                if let Some(f) = chain.pop() {
                    if f.open != 0 {
                        bail!(
                            "{} ladder block(s) still open at the end of a frame",
                            f.open
                        );
                    }
                }
                pending = None;
            }
            _ => {}
        }
        at += 1;
        Ok(())
    })?;
    Ok((out, report))
}

/// Open one frame's ladder and return how many of its blocks remain to close.
///
/// The blocks nest outermost-first, so the innermost holds only the dispatch
/// and its end is where ordinary execution begins. Leaving block `i` lands at
/// split `i - 1`.
fn open_ladder(
    c: &mut Cursor,
    plan: &Plan,
    key: usize,
    drive: Drive,
    report: &mut Dispatch,
) -> Result<usize> {
    let Some(frame) = plan.frames.get(&key) else {
        return Ok(0);
    };
    // Splits are ascending and distinct, so at most the first can sit at the
    // frame's own entry, and an arm for it would duplicate the default.
    let leading = usize::from(frame.splits.first() == Some(&first_operator_of(key)));
    let splits = &frame.splits[leading..];
    let through = &frame.through[leading..];
    if splits.is_empty() {
        // Every way into this frame is the way in it already had.
        return Ok(0);
    }
    let m = splits.len();
    for _ in 0..=m {
        c.open_block(wasm_encoder::BlockType::Empty);
    }
    report.ladders += 1;
    report.blocks += m + 1;

    // Ordinals are handed out in body order and a frame is a contiguous run
    // of the body, so the ordinals reachable inside one are a contiguous
    // range. The table only has to span that range: subtracting its start
    // sends everything outside it, in either direction, past the end of an
    // unsigned index and so to the default arm, which runs the frame
    // normally. Sizing the table to the whole function instead costs an
    // entry per call per enclosing frame, which is quadratic in a way this
    // is not.
    let (lo, hi) = match (
        through.iter().flatten().copied().min(),
        through.iter().flatten().copied().max(),
    ) {
        (Some(lo), Some(hi)) => (lo, hi),
        _ => bail!("a frame has a ladder but no call reaches it"),
    };
    let mut targets = vec![1u32; (hi - lo + 1) as usize];
    for (i, ordinals) in through.iter().enumerate() {
        for &o in ordinals {
            targets[(o - lo) as usize] = i as u32 + 2;
        }
    }
    report.table_entries += targets.len();
    let g = drive.resume();
    c.emit_new(&Instruction::GlobalGet(g));
    c.emit_new(&Instruction::If(wasm_encoder::BlockType::Empty));
    c.emit_new(&Instruction::GlobalGet(g));
    c.emit_new(&Instruction::I32Const(1 + lo as i32));
    c.emit_new(&Instruction::I32Sub);
    // Labels are relative to the `if` frame, so the innermost ladder block is
    // 1 and split `i` is `i + 2`.
    c.emit_new(&Instruction::BrTable(targets.into(), 1));
    c.emit_new(&Instruction::End);
    c.close_block()?;
    Ok(m)
}

/// Move the top `count` operands into locals, leaving the stack that much
/// shorter, and return the locals holding them, top first.
fn spill_stack(c: &mut Cursor, pool: &BTreeMap<ValType, Vec<u32>>, count: u32) -> Result<Vec<u32>> {
    let mut used: BTreeMap<ValType, usize> = BTreeMap::new();
    let mut slots = Vec::with_capacity(count as usize);
    for depth in 0..count as usize {
        let ty = c
            .operand(depth)
            .ok_or_else(|| anyhow!("operand {depth} is past the bottom of the stack"))?
            .ok_or_else(|| anyhow!("operand {depth} has no type"))?;
        let ty = encode_val_type(ty)?;
        let nth = used.entry(ty).or_default();
        let at = *nth;
        *nth += 1;
        let have = pool.get(&ty).map(Vec::as_slice).unwrap_or_default();
        slots.push(*have.get(at).ok_or_else(|| {
            anyhow!(
                "the plan reserved {} locals of {ty:?} and the rewrite wants {}",
                have.len(),
                at + 1
            )
        })?);
    }
    for &l in &slots {
        c.emit_new(&Instruction::LocalSet(l));
    }
    Ok(slots)
}
/// Names the transform gives the globals it adds.
///
/// Exported, because the host is what starts an unwind and a rewind: ash owns
/// both sides, so three globals do what Asyncify needs five exported
/// functions for.
pub const GLOBALS: [&str; 3] = ["ash_fiber_state", "ash_fiber_data", "ash_fiber_resume"];

/// The import a fiber suspends through, and the seed of the whole analysis.
pub const YIELD_IMPORT: &str = "ash_host_fiber_yield";

/// Where an unwind stops.
///
/// The runtime defines this with a stable name for exactly this lookup. It is
/// the function that calls a fiber's body, left uninstrumented so the unwind
/// ends there and the scheduler above it is still running afterwards. A
/// module without it is instrumented all the way up, and an unwind then
/// leaves the guest entirely.
pub const BARRIER: &str = "ash_fiber_enter";

/// The defined function with this name, from the name section.
///
/// `None` when the module has no name section or nothing is called that,
/// which is not an error here: a program that never makes a fiber has no
/// barrier to find.
pub fn function_named(bytes: &[u8], want: &str) -> Result<Option<u32>> {
    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        let wasmparser::Payload::CustomSection(c) =
            payload.map_err(|e| anyhow!("parsing the module: {e}"))?
        else {
            continue;
        };
        if c.name() != "name" {
            continue;
        }
        let reader = wasmparser::NameSectionReader::new(wasmparser::BinaryReader::new_features(
            c.data(),
            c.data_offset(),
            wasmparser::WasmFeatures::all(),
        ));
        for sub in reader {
            if let Ok(wasmparser::Name::Function(map)) = sub {
                for entry in map {
                    let entry = entry.map_err(|e| anyhow!("reading a name: {e}"))?;
                    if entry.name == want {
                        return Ok(Some(entry.index));
                    }
                }
            }
        }
    }
    Ok(None)
}

/// Instrument a linked module so a fiber can suspend inside it and be
/// resumed.
///
/// This is the whole transform, in the order the pieces have to run: add the
/// state globals, close the suspend set over the call graph from the yield
/// import, then rewrite every function in that set. `link` gates it behind
/// `LinkOptions::fibers` and does nothing else.
///
/// It refuses a module that does not import the yield rather than
/// instrumenting it. Such a module has no suspend point, so the rewrite would
/// cost every function in it to produce a program that can never suspend --
/// a build worth stopping, not one worth shipping quietly.
pub fn instrument(bytes: &[u8]) -> Result<(Vec<u8>, Dispatch)> {
    let seeds = imports_named(bytes, &[YIELD_IMPORT])?;
    if seeds.is_empty() {
        bail!(
            "fibers are on but the module does not import `env.{YIELD_IMPORT}`, so it has no \
             suspend point: instrumenting it would cost every function in the module and \
             produce a program that can never suspend"
        );
    }
    // Stop the closure at the runtime's barrier, so the scheduler that has to
    // pick the next fiber is still standing when this one suspends.
    let barriers: std::collections::BTreeSet<u32> =
        function_named(bytes, BARRIER)?.into_iter().collect();
    let (module, globals) = add_exported_i32_globals(bytes, &GLOBALS)?;
    // The globals are appended past the GOT block, whose indices the linker
    // has already written into patch sites, so nothing is renumbered and the
    // analysis sees the module the rewrite will actually run over.
    let program = crate::suspend::program_from_module(&module)?;
    let set = program.suspend_closure_with_barriers(
        &seeds,
        crate::suspend::Policy::TypedTable,
        &barriers,
    );
    add_rewind_dispatch(
        &module,
        &|f| set.contains(&f),
        Drive::Full(Machine {
            state: globals[0],
            data: globals[1],
            resume: globals[2],
        }),
    )
}

/// Function imports whose name is in `names`, by index.
///
/// Imported functions take the low indices in order, so this counts only
/// function imports, the same way [`crate::suspend::program_from_module`]
/// assigns them.
pub fn imports_named(bytes: &[u8], names: &[&str]) -> Result<std::collections::BTreeSet<u32>> {
    let mut found = std::collections::BTreeSet::new();
    let mut next = 0u32;
    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        let wasmparser::Payload::ImportSection(r) =
            payload.map_err(|e| anyhow!("parsing the module: {e}"))?
        else {
            continue;
        };
        for group in r {
            for import in group.map_err(|e| anyhow!("reading imports: {e}"))? {
                let (_, import) = import.map_err(|e| anyhow!("reading an import: {e}"))?;
                if matches!(import.ty, wasmparser::TypeRef::Func(_)) {
                    if names.contains(&import.name) {
                        found.insert(next);
                    }
                    next += 1;
                }
            }
        }
    }
    Ok(found)
}

/// Append mutable `i32` globals to a module, exported under `names`, and
/// return their indices.
///
/// The fiber state lives in globals, and a linked module has none of them
/// yet. Appending leaves every existing global index alone, so nothing that
/// refers to one has to be rewritten -- which is the only reason this can be
/// a small function rather than an index-space renumbering. They are exported
/// because the host is what starts an unwind and a rewind; ash owns both
/// sides, so three globals do what Asyncify needs five exported functions for.
pub fn add_exported_i32_globals(bytes: &[u8], names: &[&str]) -> Result<(Vec<u8>, Vec<u32>)> {
    use wasm_encoder::reencode::{Reencode, RoundtripReencoder};

    let mut out = wasm_encoder::Module::new();
    let mut index = 0u32;
    let mut added = Vec::new();
    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        let payload = payload.map_err(|e| anyhow!("parsing the module: {e}"))?;
        // Imported globals take the low indices, so they have to be counted
        // even though this section is copied through untouched.
        if let wasmparser::Payload::ImportSection(r) = &payload {
            for group in r.clone() {
                for import in group.map_err(|e| anyhow!("reading imports: {e}"))? {
                    let (_, import) = import.map_err(|e| anyhow!("reading an import: {e}"))?;
                    if matches!(import.ty, wasmparser::TypeRef::Global(_)) {
                        index += 1;
                    }
                }
            }
        }
        if let wasmparser::Payload::GlobalSection(r) = &payload {
            let mut section = wasm_encoder::GlobalSection::new();
            for g in r.clone() {
                let g = g.map_err(|e| anyhow!("reading a global: {e}"))?;
                index += 1;
                section.global(
                    RoundtripReencoder
                        .global_type(g.ty)
                        .map_err(|e| anyhow!("re-encoding a global type: {e}"))?,
                    &RoundtripReencoder
                        .const_expr(g.init_expr)
                        .map_err(|e| anyhow!("re-encoding a global initialiser: {e}"))?,
                );
            }
            for _ in names {
                section.global(
                    wasm_encoder::GlobalType {
                        val_type: ValType::I32,
                        mutable: true,
                        shared: false,
                    },
                    &wasm_encoder::ConstExpr::i32_const(0),
                );
                added.push(index);
                index += 1;
            }
            out.section(&section);
            continue;
        }
        if let wasmparser::Payload::ExportSection(r) = &payload {
            let mut section = wasm_encoder::ExportSection::new();
            for e in r.clone() {
                let e = e.map_err(|e| anyhow!("reading an export: {e}"))?;
                // Two exports of one name is invalid, and the module would be
                // rejected at instantiation with nothing pointing here.
                if names.contains(&e.name) {
                    bail!("the module already exports `{}`", e.name);
                }
                section.export(
                    e.name,
                    RoundtripReencoder
                        .export_kind(e.kind)
                        .map_err(|e| anyhow!("re-encoding an export kind: {e}"))?,
                    e.index,
                );
            }
            if added.len() != names.len() {
                bail!("the export section came before the global section");
            }
            for (name, &g) in names.iter().zip(&added) {
                section.export(name, wasm_encoder::ExportKind::Global, g);
            }
            out.section(&section);
            continue;
        }
        if let Some((id, range)) = payload.as_section() {
            out.section(&wasm_encoder::RawSection {
                id,
                data: &bytes[range.start as usize..range.end as usize],
            });
        }
    }
    if added.len() != names.len() {
        bail!("the module has no global section to append to");
    }
    Ok((out.finish(), added))
}

/// Where one function's frame goes when it is suspended.
///
/// The record starts with the call ordinal so a rewind knows where it was,
/// then every local in index order. Prologue and epilogue read this one list,
/// so they cannot disagree about the order: a frame that saved its locals in
/// one order and restored them in another would resume with them permuted,
/// in a module that validates and runs.
struct Saved {
    /// Byte offset of each saved local within the record.
    offsets: Vec<u32>,
    /// Local index of each saved local, in the same order.
    indices: Vec<u32>,
    /// Type of each saved local, in the same order.
    types: Vec<ValType>,
    /// The record's size, a multiple of eight.
    size: u32,
    /// A scratch `i32` holding the record's address. Deliberately not saved:
    /// the prologue needs it before the locals are restored, so restoring it
    /// would overwrite the pointer it is reading through.
    scratch: u32,
    /// A scratch `i32` a call site puts its ordinal in before branching to
    /// the function's one copy of the save sequence. Not saved either: it is
    /// written into the record explicitly.
    ordinal: u32,
    /// What the function returns, which an unwinding frame has to leave on
    /// the stack even though nobody will look at it.
    results: Vec<ValType>,
}

impl Saved {
    fn new(c: &Cursor, scratch: u32, ordinal: u32) -> Result<Self> {
        let locals = c.local_types()?;
        let mut offsets = Vec::new();
        let mut indices = Vec::new();
        let mut types = Vec::new();
        // The ordinal occupies the first word.
        let mut at = 4u32;
        for (i, &ty) in locals.iter().enumerate() {
            let i = i as u32;
            if i == scratch || i == ordinal {
                continue;
            }
            // A reference cannot be written to linear memory at all, so it is
            // left out of the record. The planner is what makes that safe: it
            // refuses a resume point where one might still hold something.
            let Ok(size) = value_size(ty) else {
                continue;
            };
            at = at.next_multiple_of(size);
            offsets.push(at);
            indices.push(i);
            types.push(ty);
            at += size;
        }
        let results = c
            .results()?
            .into_iter()
            .map(encode_val_type)
            .collect::<Result<Vec<_>>>()?;
        Ok(Saved {
            offsets,
            indices,
            types,
            size: at.next_multiple_of(8),
            scratch,
            ordinal,
            results,
        })
    }
}

/// Bytes one value takes in the side stack.
fn value_size(ty: ValType) -> Result<u32> {
    Ok(match ty {
        ValType::I32 | ValType::F32 => 4,
        ValType::I64 | ValType::F64 => 8,
        ValType::V128 => 16,
        // A reference cannot be written to linear memory at all. LLVM's
        // setjmp lowering is not supposed to leave one live across a call;
        // this is where that assumption is checked rather than assumed.
        ValType::Ref(_) => bail!("a reference-typed value cannot be saved to the side stack"),
    })
}

fn load(ty: ValType, offset: u32) -> Instruction<'static> {
    let mem = wasm_encoder::MemArg {
        offset: offset as u64,
        align: value_size(ty).unwrap_or(4).trailing_zeros(),
        memory_index: 0,
    };
    match ty {
        ValType::I32 => Instruction::I32Load(mem),
        ValType::I64 => Instruction::I64Load(mem),
        ValType::F32 => Instruction::F32Load(mem),
        ValType::F64 => Instruction::F64Load(mem),
        _ => Instruction::V128Load(mem),
    }
}

fn store(ty: ValType, offset: u32) -> Instruction<'static> {
    let mem = wasm_encoder::MemArg {
        offset: offset as u64,
        align: value_size(ty).unwrap_or(4).trailing_zeros(),
        memory_index: 0,
    };
    match ty {
        ValType::I32 => Instruction::I32Store(mem),
        ValType::I64 => Instruction::I64Store(mem),
        ValType::F32 => Instruction::F32Store(mem),
        ValType::F64 => Instruction::F64Store(mem),
        _ => Instruction::V128Store(mem),
    }
}

/// Something of this type for an unwinding frame to return.
///
/// Nobody looks at it -- the frame is on its way out -- but the function's
/// signature still has to be honoured, including when it returns a reference.
fn zero(ty: ValType) -> Instruction<'static> {
    match ty {
        ValType::I32 => Instruction::I32Const(0),
        ValType::I64 => Instruction::I64Const(0),
        ValType::F32 => Instruction::F32Const(0.0f32.into()),
        ValType::F64 => Instruction::F64Const(0.0f64.into()),
        ValType::V128 => Instruction::V128Const(0),
        ValType::Ref(r) => Instruction::RefNull(r.heap_type),
    }
}

/// At the top of an instrumented function: if this is a rewind, take this
/// frame's record back off the side stack and pick up where it left off.
///
/// The records come off in the reverse of the order they went on, which is
/// what makes the outermost frame -- saved last, restored first -- line up
/// with the order a rewind re-enters them in.
fn emit_prologue(c: &mut Cursor, m: &Machine, saved: &Saved) -> Result<()> {
    c.emit_new(&Instruction::GlobalGet(m.state));
    c.emit_new(&Instruction::I32Const(REWINDING));
    c.emit_new(&Instruction::I32Eq);
    c.emit_new(&Instruction::If(wasm_encoder::BlockType::Empty));

    // current -= size, and keep the address.
    c.emit_new(&Instruction::GlobalGet(m.data));
    c.emit_new(&Instruction::GlobalGet(m.data));
    c.emit_new(&load(ValType::I32, 0));
    c.emit_new(&Instruction::I32Const(saved.size as i32));
    c.emit_new(&Instruction::I32Sub);
    c.emit_new(&Instruction::LocalTee(saved.scratch));
    c.emit_new(&store(ValType::I32, 0));

    // The ordinal the epilogue wrote becomes the resume value the ladders
    // read, offset by one so that zero can mean "run normally".
    c.emit_new(&Instruction::LocalGet(saved.scratch));
    c.emit_new(&load(ValType::I32, 0));
    c.emit_new(&Instruction::I32Const(1));
    c.emit_new(&Instruction::I32Add);
    c.emit_new(&Instruction::GlobalSet(m.resume));

    for ((&offset, &index), &ty) in saved.offsets.iter().zip(&saved.indices).zip(&saved.types) {
        c.emit_new(&Instruction::LocalGet(saved.scratch));
        c.emit_new(&load(ty, offset));
        c.emit_new(&Instruction::LocalSet(index));
    }
    c.emit_new(&Instruction::End);
    Ok(())
}

/// After a call that could have suspended: if it did, put this frame on the
/// side stack and get out of the way.
fn emit_epilogue(c: &mut Cursor, m: &Machine, saved: &Saved, ordinal: u32) -> Result<()> {
    c.emit_new(&Instruction::GlobalGet(m.state));
    c.emit_new(&Instruction::I32Const(UNWINDING));
    c.emit_new(&Instruction::I32Eq);
    c.emit_new(&Instruction::If(wasm_encoder::BlockType::Empty));
    c.emit_new(&Instruction::I32Const(ordinal as i32));
    c.emit_new(&Instruction::LocalSet(saved.ordinal));
    // Out to the block wrapping the whole body, where the one copy of the
    // save sequence lives. Inside the `if` just emitted, which the cursor
    // does not know about, that block sits one further out than
    // `emitted_depth` reports.
    c.emit_new(&Instruction::Br(c.emitted_depth() - 1));
    c.emit_new(&Instruction::End);
    Ok(())
}

/// The one copy of a function's save-and-return sequence, at the end of the
/// body, reached by a `br` out of the block that wraps everything.
///
/// Emitting this per call site instead costs three instructions per local per
/// site, which is where nearly all of the transform's code growth would
/// otherwise go.
fn emit_unwind_exit(c: &mut Cursor, m: &Machine, saved: &Saved) -> Result<()> {
    c.emit_new(&Instruction::GlobalGet(m.data));
    c.emit_new(&load(ValType::I32, 0));
    c.emit_new(&Instruction::LocalTee(saved.scratch));
    c.emit_new(&Instruction::I32Const(saved.size as i32));
    c.emit_new(&Instruction::I32Add);
    c.emit_new(&Instruction::GlobalGet(m.data));
    c.emit_new(&load(ValType::I32, 4));
    c.emit_new(&Instruction::I32GtU);
    // Reported by the frame that overran rather than at the API boundary,
    // which is a TODO left open in Binaryen's own version.
    c.emit_new(&Instruction::If(wasm_encoder::BlockType::Empty));
    c.emit_new(&Instruction::Unreachable);
    c.emit_new(&Instruction::End);

    c.emit_new(&Instruction::LocalGet(saved.scratch));
    c.emit_new(&Instruction::LocalGet(saved.ordinal));
    c.emit_new(&store(ValType::I32, 0));
    for ((&offset, &index), &ty) in saved.offsets.iter().zip(&saved.indices).zip(&saved.types) {
        c.emit_new(&Instruction::LocalGet(saved.scratch));
        c.emit_new(&Instruction::LocalGet(index));
        c.emit_new(&store(ty, offset));
    }

    c.emit_new(&Instruction::GlobalGet(m.data));
    c.emit_new(&Instruction::LocalGet(saved.scratch));
    c.emit_new(&Instruction::I32Const(saved.size as i32));
    c.emit_new(&Instruction::I32Add);
    c.emit_new(&store(ValType::I32, 0));

    // Nobody looks at what an unwinding frame returns, but the function's
    // signature still has to be honoured.
    for &ty in &saved.results {
        c.emit_new(&zero(ty));
    }
    c.emit_new(&Instruction::Return);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn validate(bytes: &[u8]) -> Result<()> {
        wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::all())
            .validate_all(bytes)
            .map(|_| ())
            .map_err(|e| anyhow!("{e}"))
    }

    /// `$g` leaves a value on the stack, calls `$f`, then adds the two. The
    /// value under the call is what has to survive.
    fn module() -> Vec<u8> {
        let mut types = wasm_encoder::TypeSection::new();
        types.ty().function([ValType::I32], [ValType::I32]);
        let mut funcs = wasm_encoder::FunctionSection::new();
        funcs.function(0);
        funcs.function(0);
        let mut exports = wasm_encoder::ExportSection::new();
        exports.export("g", wasm_encoder::ExportKind::Func, 1);

        let mut f = wasm_encoder::Function::new([]);
        f.instruction(&Instruction::LocalGet(0));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::End);

        let mut g = wasm_encoder::Function::new([]);
        g.instruction(&Instruction::LocalGet(0)); // lives across the call
        g.instruction(&Instruction::LocalGet(0));
        g.instruction(&Instruction::Call(0));
        g.instruction(&Instruction::I32Add);
        g.instruction(&Instruction::End);

        let mut code = wasm_encoder::CodeSection::new();
        code.function(&f);
        code.function(&g);
        let mut m = wasm_encoder::Module::new();
        m.section(&types);
        m.section(&funcs);
        m.section(&exports);
        m.section(&code);
        m.finish()
    }

    #[test]
    fn a_value_under_a_call_is_put_in_a_local() {
        let (out, spills) = empty_stack_at_calls(&module(), &|_| true).expect("transform");
        validate(&out).expect("the transformed module must validate");
        assert_eq!(spills.calls, 1);
        assert_eq!(spills.already_empty, 0);
        // Both the live value and the call's own argument go through a local;
        // one of the two actually needed preserving.
        assert_eq!(spills.moved, 2);
        assert_eq!(spills.live, 1);
        // Both are i32, so two slots of one type.
        assert_eq!(spills.locals, 2);
        assert_eq!(spills.refused, 0);
    }

    #[test]
    fn a_call_with_nothing_under_it_costs_nothing() {
        // `$f` has no calls; instrument only it, so the one call site in the
        // module is skipped and nothing at all is added.
        let input = module();
        let (out, spills) = empty_stack_at_calls(&input, &|i| i == 0).expect("transform");
        validate(&out).expect("must validate");
        assert_eq!(spills.calls, 0);
        assert_eq!(spills.locals, 0);
        // Only the immediate widths differ, so the module is no bigger.
        assert!(out.len() <= input.len(), "{} vs {}", out.len(), input.len());
    }

    #[test]
    fn a_tail_call_makes_a_function_refuse() {
        let mut types = wasm_encoder::TypeSection::new();
        types.ty().function([ValType::I32], [ValType::I32]);
        let mut funcs = wasm_encoder::FunctionSection::new();
        funcs.function(0);
        funcs.function(0);
        let mut f = wasm_encoder::Function::new([]);
        f.instruction(&Instruction::LocalGet(0));
        f.instruction(&Instruction::End);
        let mut g = wasm_encoder::Function::new([]);
        g.instruction(&Instruction::LocalGet(0));
        g.instruction(&Instruction::ReturnCall(0));
        g.instruction(&Instruction::End);
        let mut code = wasm_encoder::CodeSection::new();
        code.function(&f);
        code.function(&g);
        let mut m = wasm_encoder::Module::new();
        m.section(&types);
        m.section(&funcs);
        m.section(&code);
        let input = m.finish();

        let (out, spills) = empty_stack_at_calls(&input, &|_| true).expect("transform");
        validate(&out).expect("must validate");
        assert_eq!(
            spills.refused, 1,
            "the tail-calling function must be left alone"
        );
        assert_eq!(spills.locals, 0);
    }

    /// Build, transform and run: a module whose exported `f` calls `rec(i)`
    /// for a run of `i`, with the calls placed by `body`. Returns what `rec`
    /// saw for each resume value in `resumes`.
    fn run_dispatch(module: Vec<u8>, resumes: &[(i32, i32)]) -> Vec<Vec<i32>> {
        let (out, report) =
            add_rewind_dispatch(&module, &|_| true, Drive::Resume(0)).expect("dispatch");
        assert_eq!(report.refused, 0, "nothing should have been refused");
        wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::all())
            .validate_all(&out)
            .expect("the dispatched module must validate");

        let engine = wasmtime::Engine::default();
        let compiled = wasmtime::Module::new(&engine, &out).expect("wasmtime rejects the module");
        let mut answers = Vec::new();
        for &(r, arg) in resumes {
            let mut linker = wasmtime::Linker::new(&engine);
            linker
                .func_wrap(
                    "t",
                    "rec",
                    |mut caller: wasmtime::Caller<Vec<i32>>, v: i32| {
                        caller.data_mut().push(v);
                    },
                )
                .expect("defining rec");
            let mut store = wasmtime::Store::new(&engine, Vec::new());
            let instance = linker
                .instantiate(&mut store, &compiled)
                .expect("instantiating");
            instance
                .get_global(&mut store, "resume")
                .expect("the resume global")
                .set(&mut store, wasmtime::Val::I32(r))
                .expect("setting resume");
            instance
                .get_typed_func::<i32, ()>(&mut store, "f")
                .expect("f")
                .call(&mut store, arg)
                .expect("calling f");
            answers.push(store.into_data());
        }
        answers
    }

    /// Three calls in a row in the function's own frame.
    fn straight_line() -> Vec<u8> {
        let mut types = wasm_encoder::TypeSection::new();
        types.ty().function([ValType::I32], []);
        types.ty().function([ValType::I32], []);
        let mut imports = wasm_encoder::ImportSection::new();
        imports.import("t", "rec", wasm_encoder::EntityType::Function(0));
        let mut funcs = wasm_encoder::FunctionSection::new();
        funcs.function(1);
        let mut globals = wasm_encoder::GlobalSection::new();
        globals.global(
            wasm_encoder::GlobalType {
                val_type: ValType::I32,
                mutable: true,
                shared: false,
            },
            &wasm_encoder::ConstExpr::i32_const(0),
        );
        let mut exports = wasm_encoder::ExportSection::new();
        exports.export("f", wasm_encoder::ExportKind::Func, 1);
        exports.export("resume", wasm_encoder::ExportKind::Global, 0);
        let mut f = wasm_encoder::Function::new([]);
        for i in 0..3 {
            f.instruction(&Instruction::I32Const(i));
            f.instruction(&Instruction::Call(0));
        }
        f.instruction(&Instruction::End);
        let mut code = wasm_encoder::CodeSection::new();
        code.function(&f);
        let mut m = wasm_encoder::Module::new();
        m.section(&types);
        m.section(&imports);
        m.section(&funcs);
        m.section(&globals);
        m.section(&exports);
        m.section(&code);
        m.finish()
    }

    #[test]
    fn a_rewind_can_resume_at_any_call_in_a_flat_body() {
        let got = run_dispatch(straight_line(), &[(0, 0), (1, 0), (2, 0), (3, 0)]);
        assert_eq!(got[0], vec![0, 1, 2], "no resume runs everything");
        assert_eq!(
            got[1],
            vec![0, 1, 2],
            "resuming at the first call runs everything"
        );
        assert_eq!(got[2], vec![1, 2], "resuming at the second skips the first");
        assert_eq!(got[3], vec![2], "resuming at the third skips two");
    }

    /// The same three calls, each one frame deeper: the outer ladder has to
    /// jump into a block whose own ladder then takes over.
    fn nested() -> Vec<u8> {
        let mut types = wasm_encoder::TypeSection::new();
        types.ty().function([ValType::I32], []);
        types.ty().function([ValType::I32], []);
        let mut imports = wasm_encoder::ImportSection::new();
        imports.import("t", "rec", wasm_encoder::EntityType::Function(0));
        let mut funcs = wasm_encoder::FunctionSection::new();
        funcs.function(1);
        let mut globals = wasm_encoder::GlobalSection::new();
        globals.global(
            wasm_encoder::GlobalType {
                val_type: ValType::I32,
                mutable: true,
                shared: false,
            },
            &wasm_encoder::ConstExpr::i32_const(0),
        );
        let mut exports = wasm_encoder::ExportSection::new();
        exports.export("f", wasm_encoder::ExportKind::Func, 1);
        exports.export("resume", wasm_encoder::ExportKind::Global, 0);
        let mut f = wasm_encoder::Function::new([]);
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::Call(0));
        f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::Call(0));
        f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
        f.instruction(&Instruction::I32Const(2));
        f.instruction(&Instruction::Call(0));
        f.instruction(&Instruction::End);
        f.instruction(&Instruction::End);
        f.instruction(&Instruction::End);
        let mut code = wasm_encoder::CodeSection::new();
        code.function(&f);
        let mut m = wasm_encoder::Module::new();
        m.section(&types);
        m.section(&imports);
        m.section(&funcs);
        m.section(&globals);
        m.section(&exports);
        m.section(&code);
        m.finish()
    }

    #[test]
    fn a_rewind_can_resume_inside_nested_frames() {
        let got = run_dispatch(nested(), &[(0, 0), (2, 0), (3, 0)]);
        assert_eq!(got[0], vec![0, 1, 2]);
        assert_eq!(got[1], vec![1, 2], "one ladder jump into the outer block");
        assert_eq!(got[2], vec![2], "two ladder jumps, one per frame");
    }

    /// The scaffolding every dispatch test shares: an imported `rec`, a
    /// mutable `resume` global, and an exported `f` taking one `i32`.
    fn dispatch_module(locals: &[(u32, ValType)], body: &[Instruction]) -> Vec<u8> {
        let mut types = wasm_encoder::TypeSection::new();
        types.ty().function([ValType::I32], []);
        let mut imports = wasm_encoder::ImportSection::new();
        imports.import("t", "rec", wasm_encoder::EntityType::Function(0));
        let mut funcs = wasm_encoder::FunctionSection::new();
        funcs.function(0);
        let mut globals = wasm_encoder::GlobalSection::new();
        globals.global(
            wasm_encoder::GlobalType {
                val_type: ValType::I32,
                mutable: true,
                shared: false,
            },
            &wasm_encoder::ConstExpr::i32_const(0),
        );
        let mut exports = wasm_encoder::ExportSection::new();
        exports.export("f", wasm_encoder::ExportKind::Func, 1);
        exports.export("resume", wasm_encoder::ExportKind::Global, 0);
        let mut f = wasm_encoder::Function::new(locals.iter().copied());
        for i in body {
            f.instruction(i);
        }
        f.instruction(&Instruction::End);
        let mut code = wasm_encoder::CodeSection::new();
        code.function(&f);
        let mut m = wasm_encoder::Module::new();
        m.section(&types);
        m.section(&imports);
        m.section(&funcs);
        m.section(&globals);
        m.section(&exports);
        m.section(&code);
        m.finish()
    }

    #[test]
    fn a_rewind_can_resume_inside_the_arm_of_an_if() {
        // The condition is a `local.get`, so the jump target goes before it
        // and the rewind recomputes it -- which is only right because a
        // parameter is exactly the kind of value a rewind restores.
        let module = dispatch_module(
            &[],
            &[
                Instruction::LocalGet(0),
                Instruction::If(wasm_encoder::BlockType::Empty),
                Instruction::I32Const(10),
                Instruction::Call(0),
                Instruction::I32Const(11),
                Instruction::Call(0),
                Instruction::Else,
                Instruction::I32Const(20),
                Instruction::Call(0),
                Instruction::End,
            ],
        );
        let got = run_dispatch(module, &[(0, 1), (0, 0), (2, 1), (3, 0)]);
        assert_eq!(got[0], vec![10, 11], "the then arm, run normally");
        assert_eq!(got[1], vec![20], "the else arm, run normally");
        assert_eq!(
            got[2],
            vec![11],
            "resuming at the second call of the then arm"
        );
        assert_eq!(got[3], vec![20], "resuming into the else arm");
    }

    #[test]
    fn a_rewind_can_resume_inside_a_loop_and_later_iterations_are_normal() {
        // Two calls per iteration, three iterations. Resuming at the second
        // call skips the first once, and only once: the resume value is
        // cleared where it lands, so the loop's own ladder falls through
        // afterwards. Nothing records which iteration this is except the
        // counter local, which is the point -- a rewind restores locals, and
        // that is what makes a loop need no special handling.
        let module = dispatch_module(
            &[(1, ValType::I32)],
            &[
                Instruction::Loop(wasm_encoder::BlockType::Empty),
                Instruction::LocalGet(1),
                Instruction::Call(0),
                Instruction::I32Const(100),
                Instruction::Call(0),
                Instruction::LocalGet(1),
                Instruction::I32Const(1),
                Instruction::I32Add,
                Instruction::LocalTee(1),
                Instruction::I32Const(3),
                Instruction::I32LtS,
                Instruction::BrIf(0),
                Instruction::End,
            ],
        );
        let got = run_dispatch(module, &[(0, 0), (2, 0)]);
        assert_eq!(got[0], vec![0, 100, 1, 100, 2, 100], "run normally");
        assert_eq!(
            got[1],
            vec![100, 1, 100, 2, 100],
            "the first iteration's first call is skipped, and no other"
        );
    }

    /// Instantiate a transformed module and hand back the pieces a fiber
    /// test drives: the store recording what `rec` saw, the exported `f`, and
    /// the state global.
    ///
    /// The host's `yield` is the whole scheduler for these tests: running, it
    /// turns the call into an unwind; rewinding, it is the frame the unwind
    /// started from, so the rewind is over.
    #[allow(clippy::type_complexity)]
    fn drive_fiber(
        out: &[u8],
    ) -> (
        wasmtime::Store<Vec<i32>>,
        wasmtime::TypedFunc<(), ()>,
        wasmtime::Global,
    ) {
        let mut config = wasmtime::Config::new();
        // ash's own modules carry try_table and exnref, so a test that does
        // not enable this is not testing the shapes that matter.
        config.wasm_exceptions(true);
        config.wasm_function_references(true);
        let engine = wasmtime::Engine::new(&config).expect("engine");
        let compiled = wasmtime::Module::new(&engine, out).expect("wasmtime rejects it");
        let mut linker = wasmtime::Linker::new(&engine);
        linker
            .func_wrap(
                "t",
                "rec",
                |mut caller: wasmtime::Caller<Vec<i32>>, v: i32| {
                    caller.data_mut().push(v);
                },
            )
            .expect("rec");
        linker
            .func_wrap(
                "t",
                "state",
                |mut caller: wasmtime::Caller<Vec<i32>>| -> i32 {
                    match caller.get_export("ash_fiber_state") {
                        Some(wasmtime::Extern::Global(g)) => {
                            g.get(&mut caller).i32().expect("an i32 state")
                        }
                        _ => panic!("no state global"),
                    }
                },
            )
            .expect("state");
        linker
            .func_wrap("t", "yield", |mut caller: wasmtime::Caller<Vec<i32>>| {
                let state = match caller.get_export("ash_fiber_state") {
                    Some(wasmtime::Extern::Global(g)) => g,
                    _ => panic!("no state global"),
                };
                let now = state.get(&mut caller).i32().expect("an i32 state");
                let next = if now == 0 { UNWINDING } else { 0 };
                state
                    .set(&mut caller, wasmtime::Val::I32(next))
                    .expect("setting state");
            })
            .expect("yield");

        let mut store = wasmtime::Store::new(&engine, Vec::new());
        let instance = linker.instantiate(&mut store, &compiled).expect("instance");
        let memory = instance.get_memory(&mut store, "memory").expect("memory");
        // The side stack lives at 1024 and its two-word header at 16.
        memory.data_mut(&mut store)[16..20].copy_from_slice(&1024i32.to_le_bytes());
        memory.data_mut(&mut store)[20..24].copy_from_slice(&8192i32.to_le_bytes());
        instance
            .get_global(&mut store, "ash_fiber_data")
            .expect("the data global")
            .set(&mut store, wasmtime::Val::I32(16))
            .expect("data");
        let state = instance
            .get_global(&mut store, "ash_fiber_state")
            .expect("the state global");
        let f = instance
            .get_typed_func::<(), ()>(&mut store, "f")
            .expect("f");
        (store, f, state)
    }

    /// A fiber that suspends inside a nested call and is resumed.
    ///
    /// `f` calls `inner`, which yields. On the first call the host turns the
    /// yield into an unwind and every frame saves itself and returns; on the
    /// second the host rewinds, and each frame has to come back exactly where
    /// it was -- past the calls it had already made and not past the ones it
    /// had not. Nothing but running it says whether that happened.
    #[test]
    fn a_fiber_suspends_inside_a_call_and_is_resumed_where_it_stopped() {
        let mut types = wasm_encoder::TypeSection::new();
        types.ty().function([], []);
        types.ty().function([ValType::I32], []);
        let mut imports = wasm_encoder::ImportSection::new();
        imports.import("t", "yield", wasm_encoder::EntityType::Function(0));
        imports.import("t", "rec", wasm_encoder::EntityType::Function(1));
        let mut funcs = wasm_encoder::FunctionSection::new();
        funcs.function(0);
        funcs.function(0);
        let mut mems = wasm_encoder::MemorySection::new();
        mems.memory(wasm_encoder::MemoryType {
            minimum: 1,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });
        // A global the module already had, so the transform's own three are
        // appended after it and the existing index has to survive.
        let mut globals = wasm_encoder::GlobalSection::new();
        globals.global(
            wasm_encoder::GlobalType {
                val_type: ValType::I32,
                mutable: true,
                shared: false,
            },
            &wasm_encoder::ConstExpr::i32_const(0),
        );
        let mut exports = wasm_encoder::ExportSection::new();
        exports.export("f", wasm_encoder::ExportKind::Func, 3);
        exports.export("memory", wasm_encoder::ExportKind::Memory, 0);

        let mut inner = wasm_encoder::Function::new([]);
        inner.instruction(&Instruction::I32Const(1));
        inner.instruction(&Instruction::Call(1));
        inner.instruction(&Instruction::Call(0));
        inner.instruction(&Instruction::I32Const(2));
        inner.instruction(&Instruction::Call(1));
        inner.instruction(&Instruction::End);
        let mut f = wasm_encoder::Function::new([]);
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::Call(1));
        f.instruction(&Instruction::Call(2));
        f.instruction(&Instruction::I32Const(3));
        f.instruction(&Instruction::Call(1));
        f.instruction(&Instruction::End);
        let mut code = wasm_encoder::CodeSection::new();
        code.function(&inner);
        code.function(&f);

        let mut m = wasm_encoder::Module::new();
        m.section(&types);
        m.section(&imports);
        m.section(&funcs);
        m.section(&mems);
        m.section(&globals);
        m.section(&exports);
        m.section(&code);
        let module = m.finish();

        let (module, g) = add_exported_i32_globals(
            &module,
            &["ash_fiber_state", "ash_fiber_data", "ash_fiber_resume"],
        )
        .expect("state globals");
        assert_eq!(g, vec![1, 2, 3], "the module's own global keeps index 0");
        let machine = Machine {
            state: g[0],
            data: g[1],
            resume: g[2],
        };
        // The yield import, `inner` and `f`: the suspend point and everything
        // that can be on the stack when it fires.
        let (out, report) =
            add_rewind_dispatch(&module, &|i| matches!(i, 0 | 2 | 3), Drive::Full(machine))
                .expect("transform");
        assert_eq!(report.refused, 0);
        assert_eq!(report.epilogues, 2, "one per call that could suspend");
        wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::all())
            .validate_all(&out)
            .expect("the transformed module must validate");

        let engine = wasmtime::Engine::default();
        let compiled = wasmtime::Module::new(&engine, &out).expect("wasmtime rejects it");
        let mut linker = wasmtime::Linker::new(&engine);
        linker
            .func_wrap(
                "t",
                "rec",
                |mut caller: wasmtime::Caller<Vec<i32>>, v: i32| {
                    caller.data_mut().push(v);
                },
            )
            .expect("rec");
        linker
            .func_wrap(
                "t",
                "state",
                |mut caller: wasmtime::Caller<Vec<i32>>| -> i32 {
                    match caller.get_export("ash_fiber_state") {
                        Some(wasmtime::Extern::Global(g)) => {
                            g.get(&mut caller).i32().expect("an i32 state")
                        }
                        _ => panic!("no state global"),
                    }
                },
            )
            .expect("state");
        linker
            .func_wrap("t", "yield", |mut caller: wasmtime::Caller<Vec<i32>>| {
                let state = match caller.get_export("ash_fiber_state") {
                    Some(wasmtime::Extern::Global(g)) => g,
                    _ => panic!("no state global"),
                };
                let now = state.get(&mut caller).i32().expect("an i32 state");
                // Running: turn this into an unwind. Rewinding: this is the
                // frame the unwind started from, so it is over.
                let next = if now == 0 { 1 } else { 0 };
                state
                    .set(&mut caller, wasmtime::Val::I32(next))
                    .expect("setting state");
            })
            .expect("yield");

        let mut store = wasmtime::Store::new(&engine, Vec::new());
        let instance = linker.instantiate(&mut store, &compiled).expect("instance");
        let memory = instance.get_memory(&mut store, "memory").expect("memory");
        // The side stack lives at 1024 and its two-word header at 16.
        memory.data_mut(&mut store)[16..20].copy_from_slice(&1024i32.to_le_bytes());
        memory.data_mut(&mut store)[20..24].copy_from_slice(&8192i32.to_le_bytes());
        let state = instance.get_global(&mut store, "ash_fiber_state").unwrap();
        let data = instance.get_global(&mut store, "ash_fiber_data").unwrap();
        data.set(&mut store, wasmtime::Val::I32(16)).expect("data");
        let f = instance
            .get_typed_func::<(), ()>(&mut store, "f")
            .expect("f");

        f.call(&mut store, ()).expect("the first call");
        assert_eq!(
            store.data(),
            &vec![0, 1],
            "the fiber ran up to the yield and unwound"
        );
        assert_eq!(
            state.get(&mut store).i32(),
            Some(UNWINDING),
            "the unwind reached the host with the state still set"
        );
        let top = i32::from_le_bytes(memory.data(&store)[16..20].try_into().unwrap());
        assert!(top > 1024, "two frames should be on the side stack: {top}");

        state
            .set(&mut store, wasmtime::Val::I32(REWINDING))
            .expect("start the rewind");
        f.call(&mut store, ()).expect("the second call");
        assert_eq!(
            store.data(),
            &vec![0, 1, 2, 3],
            "the fiber resumed after the yield and ran to the end, \
             without repeating what it had already done"
        );
        assert_eq!(
            i32::from_le_bytes(memory.data(&store)[16..20].try_into().unwrap()),
            1024,
            "the side stack is empty again"
        );
    }

    /// Suspending inside an exception handler's protected region.
    ///
    /// `try_table` is the shape Binaryen's Flatten aborts on and the reason
    /// this transform exists, so a fiber that suspends under one is the case
    /// worth being sure about. The ladder for the try block has to sit inside
    /// it, and the rewind has to re-enter the try before jumping forward --
    /// otherwise the resumed code runs outside the handler it was protected
    /// by, which nothing would report.
    #[test]
    fn a_fiber_suspends_inside_a_try_table_and_the_handler_still_catches() {
        let mut types = wasm_encoder::TypeSection::new();
        types.ty().function([], []);
        types.ty().function([ValType::I32], []);
        let mut imports = wasm_encoder::ImportSection::new();
        imports.import("t", "yield", wasm_encoder::EntityType::Function(0));
        imports.import("t", "rec", wasm_encoder::EntityType::Function(1));
        let mut funcs = wasm_encoder::FunctionSection::new();
        funcs.function(0);
        funcs.function(0);
        let mut tags = wasm_encoder::TagSection::new();
        tags.tag(wasm_encoder::TagType {
            kind: wasm_encoder::TagKind::Exception,
            func_type_idx: 0,
        });
        let mut mems = wasm_encoder::MemorySection::new();
        mems.memory(wasm_encoder::MemoryType {
            minimum: 1,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });
        let mut globals = wasm_encoder::GlobalSection::new();
        globals.global(
            wasm_encoder::GlobalType {
                val_type: ValType::I32,
                mutable: true,
                shared: false,
            },
            &wasm_encoder::ConstExpr::i32_const(0),
        );
        let mut exports = wasm_encoder::ExportSection::new();
        exports.export("f", wasm_encoder::ExportKind::Func, 3);
        exports.export("memory", wasm_encoder::ExportKind::Memory, 0);

        // inner: record 1, yield, then throw.
        let mut inner = wasm_encoder::Function::new([]);
        inner.instruction(&Instruction::I32Const(1));
        inner.instruction(&Instruction::Call(1));
        inner.instruction(&Instruction::Call(0));
        inner.instruction(&Instruction::I32Const(2));
        inner.instruction(&Instruction::Call(1));
        inner.instruction(&Instruction::Throw(0));
        inner.instruction(&Instruction::End);

        // f: record 0, then call inner inside a try whose handler records 9.
        let mut f = wasm_encoder::Function::new([]);
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::Call(1));
        f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
        f.instruction(&Instruction::TryTable(
            wasm_encoder::BlockType::Empty,
            vec![wasm_encoder::Catch::One { tag: 0, label: 0 }].into(),
        ));
        f.instruction(&Instruction::Call(2));
        f.instruction(&Instruction::End); // try_table
        f.instruction(&Instruction::End); // block: falls here on a catch
        f.instruction(&Instruction::I32Const(9));
        f.instruction(&Instruction::Call(1));
        f.instruction(&Instruction::End);

        let mut code = wasm_encoder::CodeSection::new();
        code.function(&inner);
        code.function(&f);
        let mut m = wasm_encoder::Module::new();
        m.section(&types);
        m.section(&imports);
        m.section(&funcs);
        m.section(&mems);
        m.section(&tags);
        m.section(&globals);
        m.section(&exports);
        m.section(&code);
        let module = m.finish();

        let (module, g) = add_exported_i32_globals(
            &module,
            &["ash_fiber_state", "ash_fiber_data", "ash_fiber_resume"],
        )
        .expect("state globals");
        let machine = Machine {
            state: g[0],
            data: g[1],
            resume: g[2],
        };
        let (out, report) =
            add_rewind_dispatch(&module, &|i| matches!(i, 0 | 2 | 3), Drive::Full(machine))
                .expect("transform");
        assert_eq!(report.refused, 0, "no exnref local, so nothing to refuse");
        wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::all())
            .validate_all(&out)
            .expect("the transformed module must validate");

        let (mut store, f, state) = drive_fiber(&out);
        f.call(&mut store, ()).expect("the first call");
        assert_eq!(store.data(), &vec![0, 1], "suspended inside the try");
        state
            .set(&mut store, wasmtime::Val::I32(REWINDING))
            .expect("start the rewind");
        f.call(&mut store, ()).expect("the second call");
        assert_eq!(
            store.data(),
            &vec![0, 1, 2, 9],
            "resumed inside the try, and the throw that followed was still caught"
        );
    }

    /// A body built by a generator, resumed at every call it makes.
    ///
    /// The hand-written dispatch tests check shapes chosen because they were
    /// thought of. The failure this crate is written against is a branch
    /// label that is wrong and still in range, which produces a module that
    /// validates and jumps to the wrong place -- exactly the thing a chosen
    /// example is worst at finding. So: generate nested bodies, and for every
    /// call in one, assert that resuming there produces precisely the calls
    /// from that point on and not one more or less.
    ///
    /// Blocks and conditionals only, no loops: with a constant condition the
    /// executed call sequence is a flat list, so the expected answer for a
    /// resume is a suffix of it and needs no model. Loops are covered by
    /// their own test, where the counter is the thing being checked.
    #[test]
    fn every_call_of_a_generated_body_resumes_to_exactly_its_own_suffix() {
        struct Gen {
            seed: u64,
            body: Vec<Instruction<'static>>,
            /// Ordinal, payload and whether this call is on the path the
            /// module actually takes.
            calls: Vec<(u32, i32, bool)>,
            budget: usize,
        }
        impl Gen {
            fn bits(&mut self) -> u32 {
                self.seed = self
                    .seed
                    .wrapping_mul(6364136223846793005)
                    .wrapping_add(1442695040888963407);
                (self.seed >> 33) as u32
            }
            fn seq(&mut self, depth: usize, live: bool) {
                let items = 1 + (self.bits() % 3) as usize;
                for _ in 0..items {
                    if self.budget == 0 {
                        return;
                    }
                    let nested = depth > 0 && !self.bits().is_multiple_of(3);
                    if !nested {
                        self.budget -= 1;
                        let ordinal = self.calls.len() as u32;
                        let payload = 100 + ordinal as i32;
                        self.calls.push((ordinal, payload, live));
                        self.body.push(Instruction::I32Const(payload));
                        self.body.push(Instruction::Call(0));
                    } else if self.bits().is_multiple_of(2) {
                        self.body
                            .push(Instruction::Block(wasm_encoder::BlockType::Empty));
                        self.seq(depth - 1, live);
                        self.body.push(Instruction::End);
                    } else {
                        let taken = self.bits().is_multiple_of(2);
                        self.body.push(Instruction::I32Const(taken as i32));
                        self.body
                            .push(Instruction::If(wasm_encoder::BlockType::Empty));
                        self.seq(depth - 1, live && taken);
                        self.body.push(Instruction::Else);
                        self.seq(depth - 1, live && !taken);
                        self.body.push(Instruction::End);
                    }
                }
            }
        }

        // A generator that quietly produced nothing interesting would make
        // this test pass by doing nothing, so what it covered is asserted.
        let (mut bodies, mut resumes_checked, mut deepest) = (0usize, 0usize, 0usize);
        for seed in 1u64..=64 {
            let mut g = Gen {
                seed: seed.wrapping_mul(0x9E37_79B9_7F4A_7C15),
                body: Vec::new(),
                calls: Vec::new(),
                budget: 24,
            };
            g.seq(4, true);
            let executed: Vec<i32> = g
                .calls
                .iter()
                .filter(|(_, _, live)| *live)
                .map(|(_, p, _)| *p)
                .collect();
            if executed.len() < 3 {
                continue;
            }

            // Resume 0 runs the whole thing; resuming at the j-th executed
            // call must produce exactly the calls from j on. A call on a path
            // the module never takes is not a place it could have suspended,
            // so it is not a resume value worth asking about.
            let mut want: Vec<(i32, Vec<i32>)> = vec![(0, executed.clone())];
            let mut j = 0usize;
            for &(ordinal, _, live) in &g.calls {
                if live {
                    want.push((ordinal as i32 + 1, executed[j..].to_vec()));
                    j += 1;
                }
            }
            bodies += 1;
            resumes_checked += want.len();
            deepest = deepest.max(
                g.body
                    .iter()
                    .scan(0usize, |d, i| {
                        match i {
                            Instruction::Block(_) | Instruction::If(_) => *d += 1,
                            Instruction::End => *d = d.saturating_sub(1),
                            _ => {}
                        }
                        Some(*d)
                    })
                    .max()
                    .unwrap_or(0),
            );
            let resumes: Vec<(i32, i32)> = want.iter().map(|(r, _)| (*r, 0)).collect();
            let got = run_dispatch(dispatch_module(&[], &g.body), &resumes);
            for ((resume, expected), actual) in want.iter().zip(&got) {
                assert_eq!(
                    actual,
                    expected,
                    "seed {seed}, resume {resume}: {} calls in the body, {} on the taken path",
                    g.calls.len(),
                    executed.len()
                );
            }
        }
        eprintln!("{bodies} generated bodies, {resumes_checked} resumes, nesting to {deepest}");
        assert!(
            bodies >= 15,
            "only {bodies} bodies were interesting enough to run"
        );
        assert!(
            resumes_checked >= 150,
            "only {resumes_checked} resumes checked"
        );
        assert!(
            deepest >= 3,
            "the deepest generated body nested only {deepest} frames"
        );
    }

    #[test]
    fn a_module_with_no_suspend_point_is_refused_rather_than_instrumented() {
        // The straight-line dispatch module imports `rec`, not the yield.
        let err = instrument(&straight_line()).expect_err("must refuse");
        assert!(
            err.to_string().contains(YIELD_IMPORT),
            "the message must name the import that is missing: {err}"
        );
    }

    #[test]
    fn instrument_refuses_a_module_that_already_exports_a_state_global_name() {
        // Two exports of one name is invalid, and a module that reached
        // instantiation before failing would point at nothing.
        let mut types = wasm_encoder::TypeSection::new();
        types.ty().function([], []);
        let mut imports = wasm_encoder::ImportSection::new();
        imports.import("env", YIELD_IMPORT, wasm_encoder::EntityType::Function(0));
        let mut funcs = wasm_encoder::FunctionSection::new();
        funcs.function(0);
        let mut globals = wasm_encoder::GlobalSection::new();
        globals.global(
            wasm_encoder::GlobalType {
                val_type: ValType::I32,
                mutable: true,
                shared: false,
            },
            &wasm_encoder::ConstExpr::i32_const(0),
        );
        let mut exports = wasm_encoder::ExportSection::new();
        exports.export(GLOBALS[0], wasm_encoder::ExportKind::Func, 1);
        let mut f = wasm_encoder::Function::new([]);
        f.instruction(&Instruction::Call(0));
        f.instruction(&Instruction::End);
        let mut code = wasm_encoder::CodeSection::new();
        code.function(&f);
        let mut m = wasm_encoder::Module::new();
        m.section(&types);
        m.section(&imports);
        m.section(&funcs);
        m.section(&globals);
        m.section(&exports);
        m.section(&code);

        let err = instrument(&m.finish()).expect_err("must refuse");
        assert!(
            err.to_string().contains(GLOBALS[0]),
            "the message must name the export that collides: {err}"
        );
    }

    /// A scheduler that is still standing after the fiber under it suspends.
    ///
    /// This is the shape ash needs and the reason the transform has barriers.
    /// An unwind travels exactly as far as the instrumentation does, so a
    /// function deliberately left out of the suspend set is where it stops:
    /// the callee returns, the caller's locals were never touched, and it
    /// carries on. Everything above the scheduler -- the whole M:N scheduler
    /// in ash_std -- therefore keeps running while one fiber is parked, which
    /// an unwind that went all the way out to the host would not allow.
    ///
    /// `sched` is the barrier. It calls the fiber body, then asks the host
    /// what the state is and records it, which is how a guest with no way to
    /// name a linker-added global finds out that its callee suspended.
    #[test]
    fn an_unwind_stops_at_a_scheduler_left_uninstrumented() {
        let mut types = wasm_encoder::TypeSection::new();
        types.ty().function([], []);
        types.ty().function([ValType::I32], []);
        types.ty().function([], [ValType::I32]);
        let mut imports = wasm_encoder::ImportSection::new();
        imports.import("t", "yield", wasm_encoder::EntityType::Function(0));
        imports.import("t", "rec", wasm_encoder::EntityType::Function(1));
        imports.import("t", "state", wasm_encoder::EntityType::Function(2));
        let mut funcs = wasm_encoder::FunctionSection::new();
        funcs.function(0); // 3: body
        funcs.function(0); // 4: sched
        funcs.function(0); // 5: f
        let mut mems = wasm_encoder::MemorySection::new();
        mems.memory(wasm_encoder::MemoryType {
            minimum: 1,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });
        let mut globals = wasm_encoder::GlobalSection::new();
        globals.global(
            wasm_encoder::GlobalType {
                val_type: ValType::I32,
                mutable: true,
                shared: false,
            },
            &wasm_encoder::ConstExpr::i32_const(0),
        );
        let mut exports = wasm_encoder::ExportSection::new();
        exports.export("f", wasm_encoder::ExportKind::Func, 5);
        exports.export("memory", wasm_encoder::ExportKind::Memory, 0);

        let mut body = wasm_encoder::Function::new([]);
        body.instruction(&Instruction::I32Const(1));
        body.instruction(&Instruction::Call(1));
        body.instruction(&Instruction::Call(0)); // yield
        body.instruction(&Instruction::I32Const(2));
        body.instruction(&Instruction::Call(1));
        body.instruction(&Instruction::End);

        // The barrier: run the fiber, then report what the state is. On the
        // pass where the fiber suspends it reaches this line anyway, which is
        // the whole point.
        let mut sched = wasm_encoder::Function::new([]);
        sched.instruction(&Instruction::Call(3)); // body
        sched.instruction(&Instruction::Call(2)); // state
        sched.instruction(&Instruction::I32Const(100));
        sched.instruction(&Instruction::I32Add);
        sched.instruction(&Instruction::Call(1)); // rec(100 + state)
        sched.instruction(&Instruction::End);

        let mut f = wasm_encoder::Function::new([]);
        f.instruction(&Instruction::Call(4));
        f.instruction(&Instruction::End);

        let mut code = wasm_encoder::CodeSection::new();
        code.function(&body);
        code.function(&sched);
        code.function(&f);
        let mut m = wasm_encoder::Module::new();
        m.section(&types);
        m.section(&imports);
        m.section(&funcs);
        m.section(&mems);
        m.section(&globals);
        m.section(&exports);
        m.section(&code);

        let (module, g) = add_exported_i32_globals(
            &m.finish(),
            &["ash_fiber_state", "ash_fiber_data", "ash_fiber_resume"],
        )
        .expect("state globals");
        let machine = Machine {
            state: g[0],
            data: g[1],
            resume: g[2],
        };
        // The closure from the yield import would sweep in sched and f as
        // well; barriers stop it at sched, so f is out too.
        let program = crate::suspend::program_from_module(&module).expect("program");
        let set = program.suspend_closure_with_barriers(
            &BTreeSet::from([0]),
            crate::suspend::Policy::TypedTable,
            &BTreeSet::from([4]),
        );
        assert!(set.contains(&3), "the fiber body is instrumented");
        assert!(!set.contains(&4), "the scheduler is the barrier");
        assert!(!set.contains(&5), "and nothing above it is swept in");

        let (out, report) =
            add_rewind_dispatch(&module, &|i| set.contains(&i), Drive::Full(machine))
                .expect("transform");
        assert_eq!(
            report.traps, 0,
            "a barrier is deliberate, so it must not trap"
        );
        wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::all())
            .validate_all(&out)
            .expect("must validate");

        let (mut store, run, state) = drive_fiber(&out);
        run.call(&mut store, ()).expect("the first call");
        assert_eq!(
            store.data(),
            &vec![1, 101],
            "the fiber ran to the yield and unwound, and the scheduler was \
             still there to see the state say so"
        );

        state
            .set(&mut store, wasmtime::Val::I32(REWINDING))
            .expect("start the rewind");
        run.call(&mut store, ()).expect("the second call");
        assert_eq!(
            store.data(),
            &vec![1, 101, 2, 100],
            "the fiber resumed after the yield and finished, and the scheduler \
             saw the state back to running"
        );
    }

    #[test]
    fn a_value_from_an_enclosing_frame_makes_a_function_refuse() {
        // The `i32.const 5` is pushed before the block, so inside the block it
        // is below the frame's base and cannot be popped there.
        let mut types = wasm_encoder::TypeSection::new();
        types.ty().function([], []);
        types.ty().function([ValType::I32, ValType::I32], []);
        let mut funcs = wasm_encoder::FunctionSection::new();
        funcs.function(0);
        funcs.function(1);
        let mut f = wasm_encoder::Function::new([]);
        f.instruction(&Instruction::End);
        let mut g = wasm_encoder::Function::new([]);
        g.instruction(&Instruction::I32Const(5));
        g.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
        g.instruction(&Instruction::Call(0));
        g.instruction(&Instruction::End);
        g.instruction(&Instruction::I32Const(6));
        g.instruction(&Instruction::Call(1));
        g.instruction(&Instruction::End);
        let mut code = wasm_encoder::CodeSection::new();
        code.function(&f);
        code.function(&g);
        let mut m = wasm_encoder::Module::new();
        m.section(&types);
        m.section(&funcs);
        m.section(&code);
        let input = m.finish();

        let (out, spills) = empty_stack_at_calls(&input, &|_| true).expect("transform");
        validate(&out).expect("must validate");
        assert_eq!(
            spills.refused, 1,
            "the function with a stacked value must be left alone"
        );
        assert_eq!(spills.locals, 0);
    }
}
