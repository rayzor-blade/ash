//! The interpreter's side of the AIR v2 migration.
//!
//! The interpreter executes almost everything in hybrid mode and, until now,
//! received no optimization at all — it walked the opcode array the bytecode
//! file shipped. This module lets it walk an *optimized* one instead:
//!
//! ```text
//! bytecode ops --lower--> typed SSA --passes--> verify --serialize--> ops'
//! ```
//!
//! `serialize` emits only standard HL opcodes, so `ops'` is executed by the
//! same dispatch loop as `ops` with no interpreter changes beyond resolving a
//! function's body through [`Cache::body`] instead of indexing
//! `bytecode.functions` directly.
//!
//! # Gating
//!
//! Off unless `ASH_AIR=v2-serialize`. Every backend is meant to end up consuming the
//! same serialized array, but the interpreter is the *reference* the other two
//! are checked against, so switching it over by default would move the
//! measuring stick at the same time as the thing being measured.
//! `ASH_AIR_LEVEL` picks the opt level, spelled the same way the
//! `ASH_VERIFY_AIR` sweep spells it, so the level that reports clean over a
//! whole module is the level that runs.
//!
//! # Falling back
//!
//! A function the pipeline refuses runs its raw opcodes, and the refusal is
//! recorded so the attempt is never repeated. That is per-function, not
//! per-module: one function `lower` cannot handle must not cost the rest of
//! the program its optimization.

use std::{collections::HashMap, sync::OnceLock};

use air::opcodes::Opcode;
use ash_core::air_pipeline::{optimized_with_config, AshModule};
use ash_core::bytecode::DecodedBytecode;
use ash_core::types::{HLFunction, TypeRef};

/// Whether the interpreter runs AIR-v2-optimized bodies.
///
/// Read once: this gates the function-entry path, and on macOS `getenv` takes
/// a process-wide lock (the same reason `env_flag!` exists in `interpreter`).
pub fn enabled() -> bool {
    static CELL: OnceLock<bool> = OnceLock::new();
    // Default ON, and that has to include the variable being UNSET — which is
    // how it is in every normal run. An earlier attempt to make v2 the default
    // changed only the empty-string arm, a value the environment never actually
    // produces, so the interpreter went on executing raw bytecode while the
    // gate reported itself as on. That is exactly the failure the typo arm
    // below was written to prevent, arriving through the one path it did not
    // cover.
    //
    // `v2` selects the SSA interpreter in `crate::ssa`, which executes the IR
    // instead of serializing it; it stays reachable for differential testing
    // but measured slower, so the flat serialized form is the default.
    *CELL.get_or_init(|| match std::env::var("ASH_AIR").as_deref() {
        Err(_) | Ok("") | Ok("v2-serialize") => true,
        Ok("v2") | Ok("0") | Ok("off") => false,
        Ok(other) => {
            eprintln!("[air] ignoring ASH_AIR='{other}' (expected v2|v2-serialize|off); AIR is off");
            false
        }
    })
}

/// Whether to report each function's trip through the pipeline (`ASH_AIR_LOG`).
fn logging() -> bool {
    static CELL: OnceLock<bool> = OnceLock::new();
    *CELL.get_or_init(|| std::env::var("ASH_AIR_LOG").is_ok_and(|v| v != "0" && !v.is_empty()))
}

/// A findex to print raw and optimized opcodes for (`ASH_AIR_DUMP`).
///
/// Aggregate op counts hide the thing that matters to an interpreter: which
/// *function* grew. A pipeline can shrink a module by twenty instructions and
/// still lose, if the ones it added landed in the innermost loop. This prints
/// one function's before and after so that question can be answered directly.
fn dump_findex() -> Option<i32> {
    static CELL: OnceLock<Option<i32>> = OnceLock::new();
    *CELL.get_or_init(|| std::env::var("ASH_AIR_DUMP").ok().and_then(|v| v.trim().parse().ok()))
}

/// Opcodes whose source location can become part of a Haxe stack trace.
///
/// AIR does not yet carry general debug metadata through every pass. These
/// events are nevertheless stable enough to align as a sequence: calls park
/// caller frames, while Throw/NullCheck/Assert originate exception frames.
/// LCS alignment tolerates dead calls disappearing and inlined calls appearing
/// without shifting every later source position as a simple zip would.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum StackEvent {
    DirectCall(usize),
    MethodCall(usize),
    ClosureCall,
    IndirectCall,
    Throw,
    Rethrow,
    NullCheck,
    Assert,
}

fn stack_event(op: &Opcode) -> Option<StackEvent> {
    match op {
        Opcode::Call0 { fun, .. }
        | Opcode::Call1 { fun, .. }
        | Opcode::Call2 { fun, .. }
        | Opcode::Call3 { fun, .. }
        | Opcode::Call4 { fun, .. }
        | Opcode::CallN { fun, .. } => Some(StackEvent::DirectCall(fun.0)),
        Opcode::CallMethod { field, .. } | Opcode::CallThis { field, .. } => {
            Some(StackEvent::MethodCall(field.0))
        }
        Opcode::CallClosure { .. } => Some(StackEvent::ClosureCall),
        Opcode::IndirectCall { .. } => Some(StackEvent::IndirectCall),
        Opcode::Throw { .. } => Some(StackEvent::Throw),
        Opcode::Rethrow { .. } => Some(StackEvent::Rethrow),
        Opcode::NullCheck { .. } => Some(StackEvent::NullCheck),
        Opcode::Assert => Some(StackEvent::Assert),
        _ => None,
    }
}

pub(crate) fn optimized_debug(raw: &HLFunction, ops: &[Opcode]) -> Vec<i32> {
    let before: Vec<(usize, StackEvent)> = raw
        .ops
        .iter()
        .enumerate()
        .filter_map(|(pc, op)| stack_event(op).map(|event| (pc, event)))
        .collect();
    let after: Vec<(usize, StackEvent)> = ops
        .iter()
        .enumerate()
        .filter_map(|(pc, op)| stack_event(op).map(|event| (pc, event)))
        .collect();
    let cols = after.len() + 1;
    let mut lcs = vec![0usize; (before.len() + 1) * cols];
    for i in (0..before.len()).rev() {
        for j in (0..after.len()).rev() {
            lcs[i * cols + j] = if before[i].1 == after[j].1 {
                1 + lcs[(i + 1) * cols + j + 1]
            } else {
                lcs[(i + 1) * cols + j].max(lcs[i * cols + j + 1])
            };
        }
    }

    let mut debug = vec![0i32; ops.len() * 2];
    for pc in 0..ops.len() {
        debug[pc * 2] = -1;
    }
    let (mut i, mut j) = (0usize, 0usize);
    while i < before.len() && j < after.len() {
        if before[i].1 == after[j].1 {
            let (raw_pc, opt_pc) = (before[i].0, after[j].0);
            if raw_pc * 2 + 1 < raw.debug.len() {
                debug[opt_pc * 2] = raw.debug[raw_pc * 2];
                debug[opt_pc * 2 + 1] = raw.debug[raw_pc * 2 + 1];
            }
            i += 1;
            j += 1;
        } else if lcs[(i + 1) * cols + j] >= lcs[i * cols + j + 1] {
            i += 1;
        } else {
            j += 1;
        }
    }

    // Block layout may move a whole region across another one. A global LCS
    // then has to sacrifice some otherwise exact matches to preserve order.
    // When an event class has the same cardinality before and after, no event
    // was inserted or deleted, so its occurrence order is a stronger mapping.
    let mut before_by_event: HashMap<StackEvent, Vec<usize>> = HashMap::new();
    let mut after_by_event: HashMap<StackEvent, Vec<usize>> = HashMap::new();
    for &(pc, event) in &before {
        before_by_event.entry(event).or_default().push(pc);
    }
    for &(pc, event) in &after {
        after_by_event.entry(event).or_default().push(pc);
    }
    for (event, raw_pcs) in before_by_event {
        let Some(opt_pcs) = after_by_event.get(&event) else {
            continue;
        };
        if raw_pcs.len() != opt_pcs.len() {
            continue;
        }
        for (&raw_pc, &opt_pc) in raw_pcs.iter().zip(opt_pcs) {
            if raw_pc * 2 + 1 < raw.debug.len() {
                debug[opt_pc * 2] = raw.debug[raw_pc * 2];
                debug[opt_pc * 2 + 1] = raw.debug[raw_pc * 2 + 1];
            }
        }
    }
    debug
}

#[cfg(test)]
mod tests {
    use super::*;
    use air::opcodes::{RefFun, Reg};

    #[test]
    fn stack_event_debug_survives_block_reordering() {
        let raw = HLFunction {
            ops: vec![
                Opcode::Call0 {
                    dst: Reg(0),
                    fun: RefFun(10),
                },
                Opcode::Throw { exc: Reg(0) },
                Opcode::Throw { exc: Reg(0) },
                Opcode::Call0 {
                    dst: Reg(0),
                    fun: RefFun(11),
                },
            ],
            debug: vec![1, 10, 1, 20, 1, 21, 1, 30],
            ..HLFunction::default()
        };
        let optimized = vec![
            Opcode::Throw { exc: Reg(0) },
            Opcode::Throw { exc: Reg(0) },
            Opcode::Call0 {
                dst: Reg(0),
                fun: RefFun(10),
            },
            Opcode::Call0 {
                dst: Reg(0),
                fun: RefFun(11),
            },
        ];

        assert_eq!(
            optimized_debug(&raw, &optimized),
            vec![1, 20, 1, 21, 1, 10, 1, 30]
        );
    }
}

/// What a function executes, decided once on its first call.
#[derive(Clone, Copy)]
enum Body {
    /// Never called yet.
    Untried,
    /// Optimized body.
    ///
    /// `'static` because the dispatch loop holds this reference across
    /// `&mut self` calls (`execute_opcode` takes the frame mutably while the
    /// body is live), so it cannot be a borrow of the cache that produced it.
    /// Leaking is bounded by the number of functions the program actually
    /// executes, and mirrors how hot reload leaks its swapped-in bytecode.
    Ready(&'static HLFunction),
    /// The pipeline refused this one; its raw opcodes run from here on.
    Raw,
}

/// Per-function optimized bodies, plus the module view they were lowered
/// against.
#[derive(Default)]
pub struct Cache {
    /// The two `AshModule` views AIR lowers against, keyed by the bytecode
    /// they were built from. See [`Cache::prepare`] for why the key is
    /// load-bearing.
    ///
    /// One view offers callee bodies to the inliner and one withholds them.
    /// Which a function gets is `air_pipeline::interpreter_config_for`, and
    /// the OSR sites ask the same question so their lowering matches this one.
    module: Option<(
        *const DecodedBytecode,
        &'static AshModule<'static>,
        &'static AshModule<'static>,
    )>,
    /// Indexed by index into `bytecode.functions`, like `func_idx` everywhere
    /// else in the interpreter.
    bodies: Vec<Body>,
    optimized: usize,
    refused: usize,
}


impl Cache {
    /// Decide `func_idx`'s body, if it has not been decided already.
    ///
    /// Called once per function at entry, never per call: `optimize_with` runs
    /// the whole pass pipeline, which is far more expensive than interpreting
    /// the function it is optimizing.
    /// Whether [`Self::prepare`] would do more than look up a cached body.
    /// See the note on `crate::ssa::Cache::needs_prepare`.
    pub fn needs_prepare(&self, func_idx: usize) -> bool {
        enabled() && matches!(self.bodies.get(func_idx), None | Some(Body::Untried))
    }

    pub fn prepare(&mut self, bc: &DecodedBytecode, func_idx: usize) {
        if !enabled() {
            return;
        }

        let key = bc as *const DecodedBytecode;
        if self.module.map(|(p, _, _)| p) != Some(key) {
            // `AshModule::new` builds a findex map over every function and
            // every native, so building one per function would make first
            // execution quadratic in module size. It is built once and cached
            // — and leaked with its bytecode borrow widened to 'static,
            // because `HLInterpreter` has no lifetime parameter to hang that
            // borrow on.
            //
            // The pointer key is what keeps the widened borrow honest: a hot
            // reload installs a *different* `DecodedBytecode`, and the stale
            // module — along with every body lowered from it — leaves the
            // cache here, before anything can read through it.
            let built = AshModule::new(bc);
            let without: &AshModule<'_> = Box::leak(Box::new(built.without_callees_view()));
            let with: &AshModule<'_> = Box::leak(Box::new(built));
            let with: &'static AshModule<'static> = unsafe { std::mem::transmute(with) };
            let without: &'static AshModule<'static> =
                unsafe { std::mem::transmute(without) };
            self.bodies.clear();
            self.module = Some((key, with, without));
        }

        if self.bodies.len() < bc.functions.len() {
            self.bodies.resize(bc.functions.len(), Body::Untried);
        }
        if !matches!(self.bodies[func_idx], Body::Untried) {
            return;
        }

        let (_, with_callees, without_callees) =
            self.module.expect("module cached just above");
        let raw = &bc.functions[func_idx];
        // A function OSR can enter keeps the inlined body, because that body is
        // what OSR compiles; everything else drops the inliner so its callees
        // stay compiled. See `air_pipeline::interpreter_config_for`.
        let cfg = ash_core::air_pipeline::interpreter_config_for(raw);
        let m = if cfg.callees_visible {
            with_callees
        } else {
            without_callees
        };
        // The AIR pipeline runs HERE, on the mutator, inside the execute
        // phase -- not on a broker thread like the tiers that consume it. Its
        // own phase so the profile says how much of a run is spent preparing
        // IR rather than executing: on deltablue that is ~20ms of ~51ms.
        let prepared = {
            let _phase = ash_core::profile::scope("air prepare (main thread)");
            optimized_with_config(m, raw, cfg).map(|o| o.ser.clone())
        };
        self.bodies[func_idx] = match prepared {
            Ok(ser) => {
                let mut opt = raw.clone();
                opt.ops = ser.ops;
                // air numbers types with u32, ash with usize; same indices.
                opt.regs = ser
                    .reg_types
                    .iter()
                    .map(|t| TypeRef(t.0 as usize))
                    .collect();
                opt.debug = optimized_debug(raw, &opt.ops);
                if logging() {
                    eprintln!(
                        "[air] findex={} {} ops {} -> {} regs {} -> {}",
                        raw.findex,
                        raw.name(),
                        raw.ops.len(),
                        opt.ops.len(),
                        raw.regs.len(),
                        opt.regs.len()
                    );
                }
                if dump_findex() == Some(raw.findex) {
                    eprintln!("[air] === findex={} {} raw ===", raw.findex, raw.name());
                    for (i, op) in raw.ops.iter().enumerate() {
                        eprintln!("[air] raw {i:4}  {op:?}");
                    }
                    eprintln!("[air] === findex={} {} optimized ===", raw.findex, raw.name());
                    for (i, op) in opt.ops.iter().enumerate() {
                        let file = opt.debug.get(i * 2).copied().unwrap_or(-1);
                        let line = opt.debug.get(i * 2 + 1).copied().unwrap_or(0);
                        eprintln!("[air] opt {i:4}  debug={file}:{line}  {op:?}");
                    }
                }
                self.optimized += 1;
                Body::Ready(Box::leak(Box::new(opt)))
            }
            Err(e) => {
                // Silent by default: a refusal is a missed optimization, not a
                // wrong answer, and the raw opcodes are still correct.
                if logging() {
                    eprintln!("[air] falling back to raw opcodes: {e}");
                }
                self.refused += 1;
                Body::Raw
            }
        };
    }

    /// The body `func_idx` executes.
    ///
    /// Borrow-free with respect to `self` on purpose: `execute_opcode` calls
    /// this and then takes the frame mutably, so the returned reference must
    /// borrow only `bytecode`. That is what the `'static` in [`Body::Ready`]
    /// buys.
    #[inline]
    pub fn body<'b>(&self, bytecode: &'b DecodedBytecode, func_idx: usize) -> &'b HLFunction {
        match self.bodies.get(func_idx) {
            Some(&Body::Ready(f)) => f,
            _ => &bytecode.functions[func_idx],
        }
    }

    /// Drop every cached body, e.g. after a hot reload swapped the bytecode.
    ///
    /// [`Cache::prepare`] would notice the new bytecode by itself; doing it
    /// here as well keeps the invalidation next to the reload that caused it.
    pub fn invalidate(&mut self) {
        self.module = None;
        self.bodies.clear();
    }

    /// `(optimized, refused)` function counts, for a run summary.
    pub fn counts(&self) -> (usize, usize) {
        (self.optimized, self.refused)
    }
}
