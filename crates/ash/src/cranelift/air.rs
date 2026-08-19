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

    let s = match air_pipeline::optimize_with(ctx.air_module(), func, cfg.level) {
        Ok(s) => s,
        Err(e) => return decline(format!("{} failed: {}", e.stage, e.brief())),
    };

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
        ops: Cow::Owned(s.ops),
        // v2 indexes the module type table with u32, ash with usize; the
        // values are the same indices.
        regs: Cow::Owned(s.reg_types.iter().map(|t| TypeRef(t.0 as usize)).collect()),
        optimized: true,
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
