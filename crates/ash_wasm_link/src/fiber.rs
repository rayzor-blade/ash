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
//! it is not available to us (`docs/wasm-fibers.md` §11 has the stack trace).
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

/// Where a rewind is told which call site to resume at.
///
/// The state machine that will drive this is not written yet. Naming the
/// source makes the jump machinery testable on its own, which is why it is
/// built before the machine rather than inside it.
#[derive(Debug, Clone, Copy)]
pub enum Resume {
    /// An `i32` global: `0` runs the function normally, `k` resumes at call
    /// site `k - 1`. The transform clears it on arrival, so the resumed
    /// function's own later calls run normally.
    Global(u32),
}

/// What the dispatch cost.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Dispatch {
    /// Functions given a dispatch.
    pub functions: usize,
    /// Functions left alone, for the reasons in [`can_instrument`] or because
    /// a jump target could not be given an empty operand stack.
    pub refused: usize,
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
}

/// The function frame, which has no opening instruction of its own.
const FUNCTION_FRAME: usize = usize::MAX;

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

/// What one function's rewind dispatch has to be able to do.
#[derive(Debug, Default)]
struct Plan {
    /// How many call sites are resume points. An ordinal indexes this range.
    calls: usize,
    /// Every frame that contains a resume point, keyed by its opening.
    frames: BTreeMap<usize, Frame>,
    /// Every split position in the function, with what landing there costs.
    boundaries: BTreeMap<usize, Boundary>,
}

/// Work out, for every instrumented function, where a rewind has to jump.
///
/// This runs as its own walk and throws away what it emits. Two things make
/// that worth the second pass: a function that turns out to be uninstrumentable
/// must never have been half rewritten, and a frame's ladder has to be emitted
/// at the top of the frame, before the calls that tell it how many arms it
/// needs have been seen.
fn plan(bytes: &[u8], instrument: &dyn Fn(u32) -> bool) -> Result<(BTreeMap<u32, Plan>, usize)> {
    let mut plans: BTreeMap<u32, Plan> = BTreeMap::new();
    let mut refused: BTreeSet<u32> = BTreeSet::new();
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
            } else if is_call(op) && !unreachable_here(c) {
                let (split, spill) = boundary(at, c.stack_height(), trivial);
                let plan = plans.entry(index).or_default();
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
    Ok((plans, refused.len()))
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

fn is_call(op: &Operator<'_>) -> bool {
    matches!(op, Operator::Call { .. } | Operator::CallIndirect { .. })
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
/// that is [`Resume`], which a state machine will later drive.
pub fn add_rewind_dispatch(
    bytes: &[u8],
    instrument: &dyn Fn(u32) -> bool,
    resume: Resume,
) -> Result<(Vec<u8>, Dispatch)> {
    let (plans, refused) = plan(bytes, instrument)?;
    let mut report = Dispatch {
        functions: plans.len(),
        refused,
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

    let out = rewrite_module(bytes, |index, c, op| {
        if index != current {
            current = index;
            chain.clear();
            chain.push(Enclosing::function());
            at = 0;
            pool.clear();
            pending = plans.get(&index).map(|_| FUNCTION_FRAME);
        }
        let Some(p) = plans.get(&index) else {
            return c.emit(op);
        };

        if let Some(key) = pending.take() {
            let opened = open_ladder(c, p, key, resume, &mut report)?;
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
            let slots = if b.spill > 0 {
                spill_stack(c, &mut pool, b.spill)?
            } else {
                Vec::new()
            };
            c.close_block()?;
            if b.is_call {
                // Later calls in this function must run normally.
                match resume {
                    Resume::Global(g) => {
                        c.emit_new(&Instruction::I32Const(0));
                        c.emit_new(&Instruction::GlobalSet(g));
                    }
                }
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
                f.open -= 1;
            }
        }

        c.emit(op)?;

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
    resume: Resume,
    report: &mut Dispatch,
) -> Result<usize> {
    let Some(frame) = plan.frames.get(&key) else {
        return Ok(0);
    };
    let m = frame.splits.len();
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
        frame.through.iter().flatten().copied().min(),
        frame.through.iter().flatten().copied().max(),
    ) {
        (Some(lo), Some(hi)) => (lo, hi),
        _ => bail!("a frame has a ladder but no call reaches it"),
    };
    let mut targets = vec![1u32; (hi - lo + 1) as usize];
    for (i, ordinals) in frame.through.iter().enumerate() {
        for &o in ordinals {
            targets[(o - lo) as usize] = i as u32 + 2;
        }
    }
    report.table_entries += targets.len();
    match resume {
        Resume::Global(g) => {
            c.emit_new(&Instruction::GlobalGet(g));
            c.emit_new(&Instruction::If(wasm_encoder::BlockType::Empty));
            c.emit_new(&Instruction::GlobalGet(g));
            c.emit_new(&Instruction::I32Const(1 + lo as i32));
            c.emit_new(&Instruction::I32Sub);
            // Labels are relative to the `if` frame, so the innermost ladder
            // block is 1 and split `i` is `i + 2`.
            c.emit_new(&Instruction::BrTable(targets.into(), 1));
            c.emit_new(&Instruction::End);
        }
    }
    c.close_block()?;
    Ok(m)
}

/// Move the top `count` operands into locals, leaving the stack that much
/// shorter, and return the locals holding them, top first.
fn spill_stack(
    c: &mut Cursor,
    pool: &mut BTreeMap<ValType, Vec<u32>>,
    count: u32,
) -> Result<Vec<u32>> {
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
        let have = pool.entry(ty).or_default();
        while have.len() <= at {
            have.push(c.reserve_local(ty));
        }
        slots.push(have[at]);
    }
    for &l in &slots {
        c.emit_new(&Instruction::LocalSet(l));
    }
    Ok(slots)
}
/// Append a mutable `i32` global to a module and return its index.
///
/// The fiber state lives in globals, and a linked module has none of them
/// yet. Appending leaves every existing global index alone, so nothing that
/// refers to one has to be rewritten -- which is the only reason this can be
/// a small function rather than an index-space renumbering.
pub fn add_i32_global(bytes: &[u8], initial: i32) -> Result<(Vec<u8>, u32)> {
    use wasm_encoder::reencode::{Reencode, RoundtripReencoder};

    let mut out = wasm_encoder::Module::new();
    let mut index = 0u32;
    let mut added = false;
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
            section.global(
                wasm_encoder::GlobalType {
                    val_type: ValType::I32,
                    mutable: true,
                    shared: false,
                },
                &wasm_encoder::ConstExpr::i32_const(initial),
            );
            out.section(&section);
            added = true;
            continue;
        }
        if let Some((id, range)) = payload.as_section() {
            out.section(&wasm_encoder::RawSection {
                id,
                data: &bytes[range.start as usize..range.end as usize],
            });
        }
    }
    if !added {
        bail!("the module has no global section to append to");
    }
    Ok((out.finish(), index))
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
            add_rewind_dispatch(&module, &|_| true, Resume::Global(0)).expect("dispatch");
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
