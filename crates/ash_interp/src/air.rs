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

use std::sync::OnceLock;

use ash::air_pipeline::{optimized, AshModule};
use ash::bytecode::DecodedBytecode;
use ash::types::{HLFunction, TypeRef};

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
pub struct Cache {
    /// The `AshModule` AIR lowers against, keyed by the bytecode it was built
    /// from. See [`Cache::prepare`] for why the key is load-bearing.
    module: Option<(*const DecodedBytecode, &'static AshModule<'static>)>,
    /// Indexed by index into `bytecode.functions`, like `func_idx` everywhere
    /// else in the interpreter.
    bodies: Vec<Body>,
    optimized: usize,
    refused: usize,
}

impl Default for Cache {
    fn default() -> Self {
        Cache {
            module: None,
            bodies: Vec::new(),
            optimized: 0,
            refused: 0,
        }
    }
}

impl Cache {
    /// Decide `func_idx`'s body, if it has not been decided already.
    ///
    /// Called once per function at entry, never per call: `optimize_with` runs
    /// the whole pass pipeline, which is far more expensive than interpreting
    /// the function it is optimizing.
    pub fn prepare(&mut self, bc: &DecodedBytecode, func_idx: usize) {
        if !enabled() {
            return;
        }

        let key = bc as *const DecodedBytecode;
        if self.module.map(|(p, _)| p) != Some(key) {
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
            let leaked: &AshModule<'_> = Box::leak(Box::new(AshModule::new(bc)));
            let m: &'static AshModule<'static> = unsafe { std::mem::transmute(leaked) };
            self.bodies.clear();
            self.module = Some((key, m));
        }

        if self.bodies.len() < bc.functions.len() {
            self.bodies.resize(bc.functions.len(), Body::Untried);
        }
        if !matches!(self.bodies[func_idx], Body::Untried) {
            return;
        }

        let m = self.module.expect("module cached just above").1;
        let raw = &bc.functions[func_idx];
        self.bodies[func_idx] = match optimized(m, raw).map(|o| o.ser.clone()) {
            Ok(ser) => {
                let mut opt = raw.clone();
                opt.ops = ser.ops;
                // air numbers types with u32, ash with usize; same indices.
                opt.regs = ser
                    .reg_types
                    .iter()
                    .map(|t| TypeRef(t.0 as usize))
                    .collect();
                // `debug` is indexed by opcode, and the pipeline renumbers
                // opcodes. The interpreter never reads it, so drop it rather
                // than carry a table whose indices no longer line up.
                opt.debug = Vec::new();
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
                        eprintln!("[air] opt {i:4}  {op:?}");
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
