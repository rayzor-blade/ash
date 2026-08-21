//! Which opcode array this tier compiles: the bytecode's own, or AIR v2's
//! optimized serialization of it.
//!
//! The Cranelift tier used to lower raw bytecode straight to CLIF with no
//! optimization at all. Routing it through
//!
//! ```text
//! ops + reg_types --lower_with--> Function --PassManager--> verify --serialize--> ops'
//! ```
//!
//! is what makes it consume the *same* optimized array the interpreter and the
//! LLVM tier are being moved onto — the property on-stack replacement needs,
//! since an opcode index only means the same thing in every tier if every tier
//! was handed the same array. All of that lives in [`crate::air_pipeline`];
//! this module is the tier's side of it: the `ASH_AIR` gate, the fall-back
//! decision, and the type conversion between the two `TypeRef` spellings.
//!
//! # Why the gate is not widened here
//!
//! [`super::lower::lower_function`] pre-checks the function against
//! [`super::is_cranelift_lowerable`] *before* this stage runs, and this stage
//! re-checks whatever it produces. So the set of functions the tier accepts is
//! identical whether `ASH_AIR` is on or off: optimization changes the code the
//! tier compiles, never which functions it takes. That is deliberate — the
//! opcode gate refuses the object model for a reason recorded in `BACKLOG.md`,
//! and widening it is a separate decision from wiring up the pipeline.
//! [`gate_report`] measures what widening *would* buy without taking it.
//!
//! # Why the options are not tuned for this tier
//!
//! [`AirPassOptions::default`] is used verbatim, and the level comes from the
//! same `ASH_AIR_LEVEL` every other engine reads. Per-tier options would give
//! each engine a different opcode array, which is exactly the property this
//! wiring exists to establish. Notably `fma` stays on even though this tier
//! gains nothing from it: the serializer emits `Fma` back as `Mul` + `Add`
//! through a temporary, so the arithmetic that reaches CLIF is the unfused
//! arithmetic the bytecode had.

use std::collections::HashMap;
use std::borrow::Cow;
use std::sync::OnceLock;

use crate::air_pipeline::{self, AirOptLevel, AirPassOptions, AshModule};
use crate::bytecode::DecodedBytecode;
use crate::opcodes::Opcode;
use crate::types::{HLFunction, TypeRef};

use super::backend::CraneliftTierContext;
use super::lower::{lowering_reject_reason, reject_reason_for_ops};

// ─────────────────────────────────────────────────────────────────────────────
// Gate
// ─────────────────────────────────────────────────────────────────────────────

/// Which IR this tier compiles from.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AirMode {
    /// Lower the bytecode as decoded. The default, and the behaviour this tier
    /// had before the pipeline existed.
    Off,
    /// Lower AIR v2's optimized serialization of the bytecode.
    V2,
}

/// The `ASH_AIR*` settings, resolved once.
#[derive(Debug)]
pub struct AirConfig {
    pub mode: AirMode,
    pub level: AirOptLevel,
    pub opts: AirPassOptions,
    /// `ASH_AIR_LOG`: one line per function describing what the stage did.
    pub log: bool,
}

/// Parse an `ASH_AIR` spelling. `None` is an unrecognized value, which the
/// caller reports rather than treating as off — a typo and a deliberate `off`
/// produce identical behaviour, so only a message tells them apart.
pub fn parse_mode(v: &str) -> Option<AirMode> {
    match v.to_ascii_lowercase().as_str() {
        "v2" => Some(AirMode::V2),
        "" | "0" | "off" => Some(AirMode::Off),
        _ => None,
    }
}

/// Read the environment once. Every compile asks for this, and macOS `getenv`
/// takes a process-wide lock (the same reason `clif_dump_wanted` in
/// [`super::lower`] caches its spec).
pub fn config() -> &'static AirConfig {
    static CONFIG: OnceLock<AirConfig> = OnceLock::new();
    CONFIG.get_or_init(|| {
        // One reader for this variable, in air_pipeline. This one answered
        // Off when ASH_AIR was unset, so the tier had never compiled optimized
        // AIR in a default run -- the same defect the interpreter's gate had.
        let mode = if air_pipeline::air_enabled() {
            AirMode::V2
        } else {
            AirMode::Off
        };
        // One level for the whole pipeline; see air_pipeline::default_level.
        let level = air_pipeline::default_level();
        AirConfig {
            mode,
            level,
            opts: AirPassOptions::default(),
            log: std::env::var("ASH_AIR_LOG").is_ok_and(|v| v != "0" && !v.is_empty()),
        }
    })
}

// ─────────────────────────────────────────────────────────────────────────────
// The body a compile runs on
// ─────────────────────────────────────────────────────────────────────────────

/// The opcode array and register-type table one compile lowers.
///
/// Borrows the bytecode function whenever the AIR stage is off or declines, so
/// the default path costs no allocation and no copy.
pub struct Body<'a> {
    pub ops: Cow<'a, [Opcode]>,
    /// Register types indexed by register id. The optimized array can declare
    /// *more* registers than the bytecode function did — the serializer
    /// appends temporaries for parallel-copy cycles and for `Fma` products —
    /// so this is the table to size register storage from, never `func.regs`.
    pub regs: Cow<'a, [TypeRef]>,
    /// Whether `ops` came out of the v2 pipeline.
    pub optimized: bool,
}

impl<'a> Body<'a> {
    fn bytecode(func: &'a HLFunction) -> Self {
        Body {
            ops: Cow::Borrowed(&func.ops),
            regs: Cow::Borrowed(&func.regs),
            optimized: false,
        }
    }
}

/// The body to compile for `func`.
///
/// Never fails: every way the pipeline can decline ends in the unoptimized
/// bytecode, which is what this tier compiled before the pipeline existed. A
/// per-function fallback is the whole point of the staged migration — one
/// function v2 cannot handle must cost that function's optimization, not the
/// tier.
pub fn body_for<'a>(ctx: &CraneliftTierContext, func: &'a HLFunction) -> Body<'a> {
    let cfg = config();
    if cfg.mode == AirMode::Off {
        return Body::bytecode(func);
    }

    let decline = |reason: String| -> Body<'a> {
        if cfg.log {
            eprintln!("[air] findex={} bytecode: {reason}", func.findex);
        }
        Body::bytecode(func)
    };

    // The shared cache, not a private pipeline run: one function lowers to
    // AIR once and every consumer reads that result. `optimize_with` here was
    // the tier's own third copy.
    let opt = match air_pipeline::optimized(ctx.air_module(), func) {
        Ok(o) => o,
        Err(e) => return decline(format!("{} failed: {}", e.stage, e.brief())),
    };
    let s = &opt.ser;

    // Re-gate. The passes only ever remove opcodes, but the serializer
    // *normalizes* some (`GetThis` -> `Field`, `CallThis` -> `CallMethod`) and
    // the O3 inliner can pull a callee's opcodes into this function, so the
    // optimized array is not guaranteed to stay inside the subset even though
    // the bytecode was. Falling back keeps the tier's accepted set exactly
    // what it is with AIR off.
    if let Some(reason) = reject_reason_for_ops(ctx.bytecode(), func, &s.ops) {
        // Fall back to the raw opcodes, which is what the comment above always
        // claimed and what the code did not do: `decline` refuses the function
        // outright, so a normalization the gate happens to exclude cost the
        // tier the whole function rather than just the optimization. Measured
        // on test_stdlib at --jit-threshold 1, turning AIR on took Cranelift
        // from 27 installs to 17 for exactly this reason.
        if cfg.log {
            eprintln!(
                "[air] findex={} optimized array refused ({reason}); using raw opcodes",
                func.findex
            );
        }
        return Body::bytecode(func);
    }

    if cfg.log {
        eprintln!(
            "[air] findex={} air: ops {} -> {} regs {} -> {}",
            func.findex,
            func.ops.len(),
            s.ops.len(),
            func.regs.len(),
            s.reg_types.len()
        );
    }
    Body {
        ops: Cow::Owned(s.ops.clone()),
        // v2 indexes the module type table with u32, ash with usize; the
        // values are the same indices.
        regs: Cow::Owned(s.reg_types.iter().map(|t| TypeRef(t.0 as usize)).collect()),
        optimized: true,
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Which lowering runs
// ─────────────────────────────────────────────────────────────────────────────

/// Whether this tier composes CLIF from the AIR IR ([`super::codegen`]) rather
/// than from a flat opcode array ([`super::lower`]).
///
/// On by default when AIR itself is on. `ASH_CL_CODEGEN=0` pins the tier to
/// the opcode lowerer, which is what makes "is this a codegen bug" answerable
/// by one environment variable instead of a rebuild.
// ─────────────────────────────────────────────────────────────────────────────
// Cranelift -> LLVM re-tier slots
// ─────────────────────────────────────────────────────────────────────────────

/// Per-function re-tier state: for every OSR-eligible loop header, a leaked
/// `AtomicU64` slot the compiled code polls, plus one leaked spill buffer
/// (one u64 per serialized register). Publishing an LLVM OSR entry address
/// into a slot makes any frame looping at that header — whether it entered
/// through the ordinary call path or through a Cranelift OSR entry — spill
/// its register image and tail into the top tier on its next iteration.
///
/// Keyed by findex; single-threaded execution is the tier's standing
/// invariant, so one buffer per function is enough.
struct RetierState {
    /// serialized header pc -> slot address
    slots: std::collections::HashMap<usize, u64>,
    /// spill buffer address (`reg * 8` slots)
    buf: u64,
}

static RETIER: std::sync::Mutex<Option<std::collections::HashMap<usize, RetierState>>> =
    std::sync::Mutex::new(None);

fn retier_enabled() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var("ASH_CL_RETIER").map_or(true, |v| v != "0"))
}

/// Allocate (once) and return this function's re-tier state as codegen wants
/// it: AIR header block id -> slot address, plus the buffer address.
fn retier_alloc(
    findex: usize,
    headers: &[(u32, usize)], // (block id, serialized pc)
    nregs: usize,
) -> (HashMap<u32, u64>, u64) {
    let mut guard = RETIER.lock().expect("retier mutex poisoned");
    let map = guard.get_or_insert_with(Default::default);
    let st = map.entry(findex).or_insert_with(|| {
        let mut slots = std::collections::HashMap::new();
        for &(_, pc) in headers {
            let slot: &'static std::sync::atomic::AtomicU64 =
                Box::leak(Box::new(std::sync::atomic::AtomicU64::new(0)));
            slots.insert(pc, slot as *const _ as u64);
        }
        let buf = Box::leak(vec![0u64; nregs.max(1)].into_boxed_slice()).as_mut_ptr() as u64;
        RetierState { slots, buf }
    });
    let exits = headers
        .iter()
        .filter_map(|&(b, pc)| st.slots.get(&pc).map(|&s| (b, s)))
        .collect();
    (exits, st.buf)
}

/// The already-allocated re-tier state for `findex`, mapped onto `block_pcs`
/// (for the OSR-entry compile, which runs after the main compile allocated
/// the slots). Empty when the function has none.
pub(super) fn retier_state_for(
    findex: usize,
    block_pcs: &[usize],
) -> (HashMap<u32, u64>, u64) {
    let guard = RETIER.lock().expect("retier mutex poisoned");
    let Some(st) = guard.as_ref().and_then(|m| m.get(&findex)) else {
        return (HashMap::new(), 0);
    };
    let exits = block_pcs
        .iter()
        .enumerate()
        .filter_map(|(b, pc)| st.slots.get(pc).map(|&s| (b as u32, s)))
        .collect();
    (exits, st.buf)
}

/// Publish an LLVM OSR entry address into the slot for `(findex, pc)`.
/// Returns whether a slot existed. Ordering: the code must be finalized
/// before this store; any frame can take the exit on its next iteration.
pub fn publish_retier_target(findex: usize, pc: usize, code: u64) -> bool {
    let guard = RETIER.lock().expect("retier mutex poisoned");
    let Some(&slot) = guard
        .as_ref()
        .and_then(|m| m.get(&findex))
        .and_then(|st| st.slots.get(&pc))
    else {
        return false;
    };
    // SAFETY: the slot is a leaked AtomicU64 allocated in retier_alloc.
    unsafe {
        (*(slot as *const std::sync::atomic::AtomicU64))
            .store(code, std::sync::atomic::Ordering::Release);
    }
    true
}

pub fn codegen_from_air() -> bool {
    static CELL: OnceLock<bool> = OnceLock::new();
    *CELL.get_or_init(|| {
        if !air_pipeline::air_enabled() {
            return false;
        }
        match std::env::var("ASH_CL_CODEGEN").as_deref() {
            Ok("0") | Ok("off") => false,
            _ => true,
        }
    })
}

/// Why `ASH_CL_SKIP` / `ASH_CL_ONLY` refuses this findex, if they do.
fn pinned_out(findex: usize) -> Option<&'static str> {
    fn list(var: &str) -> Vec<usize> {
        std::env::var(var)
            .ok()
            .map(|v| v.split(',').filter_map(|t| t.trim().parse().ok()).collect())
            .unwrap_or_default()
    }
    static SKIP: OnceLock<Vec<usize>> = OnceLock::new();
    static ONLY: OnceLock<Vec<usize>> = OnceLock::new();
    if SKIP.get_or_init(|| list("ASH_CL_SKIP")).contains(&findex) {
        return Some("ASH_CL_SKIP");
    }
    let only = ONLY.get_or_init(|| list("ASH_CL_ONLY"));
    if !only.is_empty() && !only.contains(&findex) {
        return Some("ASH_CL_ONLY");
    }
    None
}

/// Lower `findex` by whichever path can take it.
///
/// The AIR codegen is tried first and the opcode lowerer is the fallback, per
/// function. Both produce the same `LoweredFunction`, so a function either
/// path declines still reaches the LLVM tier exactly as before — the set of
/// functions this tier compiles can only grow.
pub fn lower_best(
    backend: &super::backend::AshCraneliftBackend,
    ctx: &CraneliftTierContext,
    findex: usize,
) -> anyhow::Result<super::lower::LoweredFunction> {
    // `ASH_CL_SKIP=<findex,...>` declines the tier for those functions, and
    // `ASH_CL_ONLY=<findex,...>` declines it for every other. Bisecting a
    // miscompile to one function is otherwise a rebuild per guess.
    if let Some(reason) = pinned_out(findex) {
        anyhow::bail!("{reason}");
    }
    if codegen_from_air() {
        if let Some(l) = try_air_codegen(backend, ctx, findex) {
            crate::profile::count("cranelift air-codegen", 1);
            return Ok(l);
        }
    }
    crate::profile::count("cranelift opcode-lower", 1);
    super::lower::lower_function(backend, ctx, findex)
}

/// The AIR codegen attempt, or `None` with the reason logged.
///
/// Every failure here is a decline, never an error the caller sees: the
/// opcode lowerer is behind it, and behind that the LLVM tier.
fn try_air_codegen(
    backend: &super::backend::AshCraneliftBackend,
    ctx: &CraneliftTierContext,
    findex: usize,
) -> Option<super::lower::LoweredFunction> {
    let cfg = config();
    let bytecode = ctx.bytecode();
    let func = ctx.func_index(findex).map(|i| &bytecode.functions[i])?;

    let decline = |reason: String| -> Option<super::lower::LoweredFunction> {
        if cfg.log {
            eprintln!("[air] findex={findex} codegen declined: {reason}");
        }
        None
    };

    // Signature checks only. The opcode gate is not the right screen here:
    // this path never compiles the serialized array, so refusing a function
    // for an opcode that only exists in a serialization it does not read
    // would decline work it can actually do.
    if let Some(r) = super::lower::signature_reject_reason(bytecode, func) {
        return decline(r);
    }

    let opt = match air_pipeline::optimized(ctx.air_module(), func) {
        Ok(o) => o,
        Err(e) => return decline(format!("{} failed: {}", e.stage, e.brief())),
    };

    // Re-tier exits: one polled slot per OSR-eligible loop header, gated on
    // the same eligibility the LLVM entry builder uses — a slot nothing can
    // ever fill would be a dead branch in a hot loop.
    let (osr_exits, osr_buf) = if retier_enabled() {
        let plan = crate::osr::analyze(&opt.ir);
        if plan.eligible() {
            let headers: Vec<(u32, usize)> = plan
                .entry_headers
                .iter()
                .filter_map(|&h| opt.ser.block_pcs.get(h as usize).map(|&pc| (h, pc)))
                .collect();
            retier_alloc(findex, &headers, opt.ser.reg_types.len())
        } else {
            (HashMap::new(), 0)
        }
    } else {
        (HashMap::new(), 0)
    };

    match super::codegen::lower_air_function(backend, ctx, findex, &opt.ir, &osr_exits, osr_buf)
    {
        Ok(l) => Some(l),
        Err(e) => decline(format!("{e:#}")),
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Gate delta report
// ─────────────────────────────────────────────────────────────────────────────

/// Whether optimized input changes which functions the tier's opcode gate
/// accepts, over a whole module.
///
/// [`body_for`] deliberately does not act on a widening (see the module docs),
/// so this is a measurement of what opening the pre-check to optimized input
/// would buy — and, in the other direction, of how often the optimized array
/// leaves the subset a bytecode function was already inside.
pub fn gate_report(bc: &DecodedBytecode, level: AirOptLevel, opts: &AirPassOptions) -> Vec<String> {
    let m = AshModule::new(bc);
    let (mut both_ok, mut both_refused, mut widened, mut narrowed, mut failed) = (0, 0, 0, 0, 0);
    // Refusal reasons, before and after, and the individual functions whose
    // verdict moved. A count alone cannot tell "optimization removed the one
    // Field access" from "optimization removed a whole dead branch".
    let mut before_reasons: Vec<(String, usize)> = Vec::new();
    let mut after_reasons: Vec<(String, usize)> = Vec::new();
    let mut moved: Vec<String> = Vec::new();

    let tally = |acc: &mut Vec<(String, usize)>, r: &str| match acc.iter_mut().find(|(k, _)| k == r)
    {
        Some((_, n)) => *n += 1,
        None => acc.push((r.to_string(), 1)),
    };

    for f in &bc.functions {
        let before = lowering_reject_reason(bc, f);
        if let Some(r) = &before {
            tally(&mut before_reasons, r);
        }
        let s = match air_pipeline::optimize_full(&m, f, level, opts) {
            Ok((s, _, _)) => s,
            Err(_) => {
                // The pipeline declining means the tier compiles the bytecode
                // unchanged, so admission is whatever `before` said.
                failed += 1;
                continue;
            }
        };
        let after = reject_reason_for_ops(bc, f, &s.ops);
        if let Some(r) = &after {
            tally(&mut after_reasons, r);
        }
        match (&before, &after) {
            (None, None) => both_ok += 1,
            (Some(_), Some(_)) => both_refused += 1,
            (Some(b), None) => {
                widened += 1;
                if moved.len() < 40 {
                    moved.push(format!(
                        "  WIDENED  findex={:<6} {:<32} was refused: {b}",
                        f.findex,
                        f.name()
                    ));
                }
            }
            (None, Some(a)) => {
                narrowed += 1;
                if moved.len() < 40 {
                    moved.push(format!(
                        "  NARROWED findex={:<6} {:<32} now refused: {a}",
                        f.findex,
                        f.name()
                    ));
                }
            }
        }
    }

    let top = |mut v: Vec<(String, usize)>| -> Vec<String> {
        v.sort_by(|a, b| b.1.cmp(&a.1).then(a.0.cmp(&b.0)));
        v.into_iter()
            .take(12)
            .map(|(r, n)| format!("  [{n:>6}x] {r}"))
            .collect()
    };

    let mut out = vec![
        format!("gate delta at {level:?} over {} functions", bc.functions.len()),
        format!(
            "accepted by both={both_ok} refused by both={both_refused} widened={widened} narrowed={narrowed} pipeline-declined={failed}"
        ),
        "refusal reasons on bytecode:".to_string(),
    ];
    out.extend(top(before_reasons));
    out.push("refusal reasons on the optimized array:".to_string());
    out.extend(top(after_reasons));
    out.extend(moved);
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Only the exact spelling turns the pipeline on, and anything else is
    /// reported rather than quietly treated as off — a tier that optimized
    /// (or failed to) because of a typo would make every bug report ambiguous.
    #[test]
    fn only_v2_enables_the_pipeline() {
        assert_eq!(parse_mode("v2"), Some(AirMode::V2));
        assert_eq!(parse_mode("V2"), Some(AirMode::V2));
        assert_eq!(parse_mode("off"), Some(AirMode::Off));
        assert_eq!(parse_mode("0"), Some(AirMode::Off));
        assert_eq!(parse_mode(""), Some(AirMode::Off));
        for typo in ["v1", "on", "true", "2", "air"] {
            assert_eq!(parse_mode(typo), None, "ASH_AIR={typo}");
        }
    }
}
