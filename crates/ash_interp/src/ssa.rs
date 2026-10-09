//! Direct execution of AIR v2 typed SSA — no de-SSA, no round trip.
//!
//! [`crate::air`] runs the other half of the v2 migration: it lowers a function
//! to SSA, optimizes it, and *serializes it back to opcodes* for the existing
//! dispatch loop. That works, but everything the IR knows dies at the
//! serializer — the per-value types, the effect lattice, `FieldGet`'s already
//! resolved `(object type, field slot)` — and the opcode array then makes each
//! consumer re-derive it. Serializing also costs a pass over every function
//! before it runs once.
//!
//! So this module executes the IR. The pipeline stops at
//! [`ash_core::air_pipeline::prepare_ir`], and the CFG, the phis and the cells are
//! what run.
//!
//! # The frame is the register file
//!
//! `Function::values` is dense, so a `ValueId` is already a frame index. A
//! function's frame is one [`RegisterFile`](crate::frame::RegisterFile) of
//! `values.len() + cells.len()` slots: SSA values first, then the pinned-register
//! cells. Two things fall out of reusing the opcode interpreter's frame type
//! rather than inventing one:
//!
//! * **GC roots need no new plumbing.** `sync_gc_scan_roots` walks
//!   `self.stack`, and every SSA frame is an ordinary `InterpreterFrame` on that
//!   stack, so its values and its cells are scanned by the same code that scans
//!   HL registers. Nothing here can hold a live pointer the collector cannot see.
//! * **The semantics are shared, not copied.** The `op_*` methods extracted
//!   from `execute_opcode` take register operands as plain frame indices, so
//!   passing a value id where an HL register number used to go runs the *same*
//!   code — which is the only way parity with the reference interpreter is
//!   something other than a coincidence that decays.
//!
//! Those methods also read operand types out of `HLFunction::regs`. That is
//! what [`Prepared::shim`] is: an `HLFunction` whose `regs` is the IR's
//! per-value type table (values then cells) and whose `ops` is empty. It is a
//! type view handed over, not a body reconstructed — no opcode is ever built.
//!
//! # Phis
//!
//! Resolved at block entry against the id of the block control came from, and
//! every incoming value is *read before any destination is written*. A phi
//! group is a parallel copy; assigning as you go turns a swap into a
//! duplication, silently.
//!
//! # Traps
//!
//! `Trap` is a terminator with a normal and a handler successor, so the frame's
//! existing `trap_stack` carries `(handler block, exception cell slot)` where it
//! used to carry `(catch pc, exception register)`. An exception — raised here or
//! propagated out of a callee as `HLExceptionPropagation` — pops the innermost
//! entry, stores the value into that cell and resumes at the handler block. The
//! handler reads the value back with `CellGet`, which is exactly why lowering
//! pins it: there is no program point between the throw and the handler for a
//! phi copy to live at.
//!
//! # Gating and fallback
//!
//! This is the interpreter an unset `ASH_AIR` selects; `ASH_AIR=v2-serialize`
//! picks the older serialize-to-opcodes path instead, and `off` turns the
//! pipeline off entirely. A function whose IR the
//! pipeline refuses — or which uses an instruction this dispatcher does not
//! implement yet (see [`unsupported`]) — runs its raw opcodes, decided once and
//! recorded. That is per function, so an incomplete dispatcher costs coverage,
//! never correctness.
//!
//! # Hot reload
//!
//! Both walkers poll for a pending reload after every call. When one lands,
//! the reload path calls [`Cache::invalidate`] — bodies prepared from the old
//! bytecode describe functions the new bytecode may not even have at the same
//! findex. A frame already inside a prepared body keeps it: prepared bodies
//! are reference counted, so the frame finishes on the body it started with
//! and the next call resolves the new one. Retired bodies are then freed.

use std::rc::Rc;
use std::sync::{Arc, OnceLock};

use air::v2::{Function, Instr};
use ash_core::air_pipeline::AshModule;
use ash_core::bytecode::DecodedBytecode;
use ash_core::types::{HLFunction, TypeRef};

/// Whether the interpreter executes AIR v2 SSA directly.
///
/// Read once: this gates the function-entry path, and on macOS `getenv` takes a
/// process-wide lock.
pub fn enabled() -> bool {
    *mode()
}

fn mode() -> &'static bool {
    static CELL: OnceLock<bool> = OnceLock::new();
    // Default ON, unset included -- unset is how every normal run arrives.
    //
    // The alternative is not "no AIR": it is optimizing AIR and then
    // serializing it back to flat opcodes to interpret those, which discards
    // the types and the SSA form the pipeline just established and leaves the
    // interpreter the only consumer in the VM that does not read the IR the
    // other tiers read. It also makes a vector instruction something that
    // must be scalarized on the way out rather than executed. Keeping two
    // interpreters honest against each other is worth an escape hatch
    // (`ASH_AIR=v2-serialize`), not a default.
    CELL.get_or_init(|| {
        !matches!(
            std::env::var("ASH_AIR").as_deref(),
            Ok("v2-serialize") | Ok("0") | Ok("off") | Ok("none")
        )
    })
}

/// Whether to report each function's trip through the pipeline (`ASH_AIR_LOG`).
fn logging() -> bool {
    static CELL: OnceLock<bool> = OnceLock::new();
    *CELL.get_or_init(|| std::env::var("ASH_AIR_LOG").is_ok_and(|v| v != "0" && !v.is_empty()))
}

/// An IR body plus the type view the shared `op_*` semantics read operand
/// types out of.
pub struct Prepared {
    /// The canonical body, also used by compiler tiers and OSR. No IR clone.
    optimized: Arc<ash_core::air_pipeline::Optimized>,
    /// Indices of executable instructions in each canonical block. Position
    /// markers stay in the shared IR for codegen, but never enter dispatch.
    pub instructions: Vec<Box<[u32]>>,
    pub shim: HLFunction,
    pub cell_base: u32,
    pub cfg: ash_core::air_pipeline::AirConfigKey,
    pub positions: Box<[air::v2::positions::PcPosition]>,
    osr: OnceLock<OsrState>,
}

struct OsrState {
    liveness: air::v2::liveness::Liveness,
    reg_types: Box<[TypeRef]>,
}

impl Prepared {
    pub fn ir(&self) -> &Function {
        &self.optimized.ir
    }

    pub fn block_pcs(&self) -> &[usize] {
        &self.optimized.ser.block_pcs
    }

    pub fn instr_pcs(&self) -> &[Vec<usize>] {
        &self.optimized.ser.instr_pcs
    }

    pub fn term_pcs(&self) -> &[usize] {
        &self.optimized.ser.term_pcs
    }

    fn osr(&self) -> &OsrState {
        self.osr.get_or_init(|| OsrState {
            liveness: air::v2::liveness::Liveness::analyze(
                self.ir(),
                &air::v2::CfgInfo::build(self.ir()),
            ),
            reg_types: self
                .optimized
                .ser
                .reg_types
                .iter()
                .map(|t| TypeRef(t.0 as usize))
                .collect(),
        })
    }

    pub fn liveness(&self) -> &air::v2::liveness::Liveness {
        &self.osr().liveness
    }

    pub fn osr_reg_types(&self) -> &[TypeRef] {
        &self.osr().reg_types
    }

    /// Visit the frames the instruction at serialized `pc` stands for,
    /// innermost first as `(findex, file, line)`: the position itself, then
    /// the call that inlined it into each enclosing function. `false` when
    /// no marker reached the instruction, so the caller names the frame the
    /// old way.
    pub fn frames_at(&self, pc: usize, visit: impl FnMut(u32, i32, i32)) -> bool {
        let Some(&pos) = self.positions.get(pc) else {
            return false;
        };
        if pos.file < 0 && pos.site.is_none() {
            return false;
        }
        air::v2::positions::for_each_frame(
            pos,
            &self.ir().inline_sites,
            self.shim.findex as u32,
            visit,
        );
        true
    }
}

/// What a function executes, decided once on its first call.
enum Body {
    Untried,
    /// A running call owns a reference, so cache growth, invalidation and
    /// reload cannot move or destroy its body. Retired bodies are freed once
    /// their last active call returns.
    Ready(Rc<Prepared>),
    /// The pipeline refused it, or its IR uses something not implemented here.
    Raw,
}

/// Per-function prepared IR, plus the module view it was lowered against.
#[derive(Default)]
pub struct Cache {
    module: Option<(*const DecodedBytecode, Box<AshModule<'static>>)>,
    /// Indexed by index into `bytecode.functions`, like `func_idx` elsewhere.
    bodies: Vec<Body>,
    prepared: usize,
    refused: usize,
}

impl Cache {
    /// Decide `func_idx`'s body, if it has not been decided already.
    /// Whether [`Self::prepare`] would do more than look up a cached body.
    ///
    /// The caller wraps preparation in a blocking scope so the collector does
    /// not wait out a compile; that scope costs two trips through the world
    /// lock, which is far more than the early-out it would be guarding on the
    /// overwhelmingly common cached path.
    pub fn needs_prepare(&self, func_idx: usize) -> bool {
        enabled()
            && !matches!(
                self.bodies.get(func_idx),
                Some(Body::Ready(_)) | Some(Body::Raw)
            )
    }

    pub fn prepare(&mut self, bc: &DecodedBytecode, func_idx: usize) {
        if !enabled() {
            return;
        }

        let key = bc as *const DecodedBytecode;
        if self.module.as_ref().map(|(p, _)| *p) != Some(key) {
            // The bytecode borrow is widened because HLInterpreter has no
            // lifetime parameter. The pointer key invalidates this view before
            // a different bytecode can use it; unlike the old leaked module,
            // the cache owns it and releases its lowered callee cache.
            let m: AshModule<'static> = unsafe { std::mem::transmute(AshModule::new(bc)) };
            self.bodies.clear();
            self.module = Some((key, Box::new(m)));
        }

        if self.bodies.len() < bc.functions.len() {
            self.bodies
                .resize_with(bc.functions.len(), || Body::Untried);
        }
        if !matches!(self.bodies[func_idx], Body::Untried) {
            return;
        }

        let m = self
            .module
            .as_ref()
            .expect("module cached just above")
            .1
            .as_ref();
        let raw = &bc.functions[func_idx];
        // Reuse the flat walker's preparation policy: a loop-free body is
        // bounded by call count, so prepare no SSA just to execute it a few
        // times. Hot entries are optimized by the JIT; back-edge bodies
        // always prepare SSA so a single long call can still transfer by OSR.
        // ASH_AIR_ALL retains an explicit full-SSA comparison mode. A body
        // the optimizer could fuse a multiply-add in is prepared regardless;
        // see `may_fuse`.
        if crate::air::skip_loop_free()
            && !crate::air::has_back_edge(bc, raw)
            && !crate::air::may_fuse(bc, raw)
        {
            self.bodies[func_idx] = Body::Raw;
            return;
        }
        // The configuration every OSR site lowers this function under, which
        // is the whole point: the transfer is by position through
        // `ser.block_pcs`, so preparing separately produced a different body
        // -- different pass options, so different blocks -- and a site this
        // walker named then did not exist in the one the entry was built
        // from: the header reported as pc=13 was staged at pc=12. Asking
        // `interpreter_config_for` rather than for the tiers' own key keeps
        // that agreement while letting a function OSR can NEVER enter -- one
        // with no back edge -- be prepared more cheaply, which is what it is
        // for. Both go through the same cache, so neither runs twice.
        let air_cfg = ash_core::air_pipeline::interpreter_config_for(raw);
        let bare;
        let view = if air_cfg.callees == ash_core::air_pipeline::CalleeView::All {
            m
        } else {
            bare = m.view(air_cfg.callees);
            &bare
        };
        self.bodies[func_idx] =
            match ash_core::air_pipeline::optimized_with_config(view, raw, air_cfg) {
                Ok(optimized) => match unsupported(&optimized.ir) {
                    Some(what) => {
                        if logging() {
                            eprintln!(
                                "[ssa] findex={} {}: raw opcodes ({what} not implemented)",
                                raw.findex,
                                raw.name()
                            );
                        }
                        self.refused += 1;
                        Body::Raw
                    }
                    None => {
                        let ir = &optimized.ir;
                        let ser_view = &optimized.ser;
                        let cell_base = ir.values.len() as u32;
                        let positions = if air::v2::positions::has_markers(ir) {
                            air::v2::positions::positions_by_pc(ir, ser_view)
                        } else {
                            Vec::new()
                        };
                        let instructions = ir
                            .blocks
                            .iter()
                            .map(|blk| {
                                blk.instrs
                                    .iter()
                                    .enumerate()
                                    .filter(|(_, i)| !matches!(i, Instr::Pos { .. }))
                                    .map(|(i, _)| u32::try_from(i).expect("AIR instruction index"))
                                    .collect::<Box<[_]>>()
                            })
                            .collect();
                        // Construct a type view directly: cloning raw would
                        // copy its opcodes/debug table only to discard them.
                        // Positions are emitted directly from the canonical IR.
                        // A source-free body needs no duplicate debug table.
                        let debug = if positions.iter().any(|p| p.file >= 0) {
                            positions.iter().flat_map(|p| [p.file, p.line]).collect()
                        } else if !raw.debug().is_empty() {
                            // Escape-hatch runs without IR markers still
                            // need the legacy source alignment for traces.
                            match optimized.serialized() {
                                Ok(ser) => crate::air::optimized_debug(raw, &ser.ops),
                                Err(_) => Vec::new(),
                            }
                        } else {
                            Vec::new()
                        };
                        let mut shim = HLFunction::with_body(Vec::new(), debug);
                        shim.type_ = raw.type_.clone();
                        shim.findex = raw.findex;
                        shim.ref_ = raw.ref_;
                        shim.field_name = raw.field_name.clone();
                        shim.regs = ir
                            .values
                            .iter()
                            .map(|v| TypeRef(v.ty.0 as usize))
                            .chain(ir.cells.iter().map(|c| TypeRef(c.ty.0 as usize)))
                            .collect();
                        if logging() {
                            eprintln!(
                                "[ssa] findex={} {} ops {} -> {} values {} cells {} blocks",
                                raw.findex,
                                raw.name(),
                                raw.ops().len(),
                                ir.values.len(),
                                ir.cells.len(),
                                ir.blocks.len()
                            );
                        }
                        self.prepared += 1;
                        Body::Ready(Rc::new(Prepared {
                            optimized,
                            instructions,
                            shim,
                            cell_base,
                            cfg: air_cfg,
                            positions: positions.into_boxed_slice(),
                            osr: OnceLock::new(),
                        }))
                    }
                },
                Err(e) => {
                    // A refusal is a missed optimization, not a wrong answer.
                    if logging() {
                        eprintln!("[ssa] falling back to raw opcodes: {e}");
                    }
                    self.refused += 1;
                    Body::Raw
                }
            };
    }

    /// The prepared IR for `func_idx`, or `None` to run raw opcodes.
    ///
    /// Borrow-free with respect to `self`, like [`crate::air::Cache::body`]:
    /// the caller holds this across `&mut self` dispatch, including the nested
    /// calls that function makes.
    #[inline]
    pub fn body(&self, func_idx: usize) -> Option<Rc<Prepared>> {
        match self.bodies.get(func_idx) {
            Some(Body::Ready(p)) => Some(Rc::clone(p)),
            _ => None,
        }
    }

    /// Drop every prepared body, e.g. after a hot reload swapped the bytecode.
    pub fn invalidate(&mut self) {
        self.module = None;
        self.bodies.clear();
    }

    /// Drop ONE prepared body, so the next call re-prepares it.
    ///
    /// For the demand policy: a function that has just shown a hot loop needs
    /// the shared configuration from now on, and the body sitting here was
    /// prepared under the cheap one. Frames already running are unaffected --
    /// each owns its `Prepared` for the length of its call. That is also
    /// the policy's cost: the frame that reported the loop keeps walking the
    /// body it started with.
    pub fn forget(&mut self, func_idx: usize) {
        if let Some(slot) = self.bodies.get_mut(func_idx) {
            *slot = Body::Untried;
        }
    }

    /// `(prepared, refused)` function counts, for a run summary.
    pub fn counts(&self) -> (usize, usize) {
        (self.prepared, self.refused)
    }
}

/// What this IR uses that the dispatcher does not implement, if anything.
///
/// Checked once, before the first execution, rather than by failing mid-flight:
/// a function that bails out halfway has already committed side effects, and
/// there is no way back to the opcode interpreter from there.
fn unsupported(f: &Function) -> Option<&'static str> {
    for b in &f.blocks {
        for i in &b.instrs {
            if let Some(what) = unsupported_instr(i) {
                return Some(what);
            }
        }
    }
    None
}

fn unsupported_instr(i: &Instr) -> Option<&'static str> {
    match i {
        // Dispatched in `step`, a lane at a time through the frame's lane
        // map. Slower than the scalar loop the widening replaced, and that is
        // the trade: the same AIR must run on every tier, or a widened
        // function could not be deoptimized back to the interpreter.
        Instr::VecLoad { .. }
        | Instr::VecStore { .. }
        | Instr::VecSplat { .. }
        | Instr::VecBinOp { .. }
        | Instr::VecReduce { .. }
        // Listed explicitly so a new AIR instruction lands here as a fallback
        // rather than as a silent wrong answer.
        | Instr::Param { .. }
        | Instr::Copy { .. }
        | Instr::Int { .. }
        | Instr::Float { .. }
        | Instr::Bool { .. }
        | Instr::Bytes { .. }
        | Instr::String { .. }
        | Instr::Null { .. }
        | Instr::BinOp { .. }
        | Instr::Fma { .. }
        | Instr::Intrinsic { .. }
        | Instr::VecOp { .. }
        | Instr::VecExtract { .. }
        | Instr::VecInsert { .. }
        | Instr::UnOp { .. }
        | Instr::Call { .. }
        | Instr::CallMethod { .. }
        | Instr::CallClosure { .. }
        | Instr::StaticClosure { .. }
        | Instr::InstanceClosure { .. }
        | Instr::VirtualClosure { .. }
        | Instr::GetGlobal { .. }
        | Instr::SetGlobal { .. }
        | Instr::FieldGet { .. }
        | Instr::FieldSet { .. }
        | Instr::DynGet { .. }
        | Instr::DynSet { .. }
        | Instr::Cast { .. }
        | Instr::NullCheck { .. }
        | Instr::EndTrap { .. }
        | Instr::MemGet { .. }
        | Instr::MemSet { .. }
        | Instr::New { .. }
        | Instr::ArraySize { .. }
        | Instr::TypeConst { .. }
        | Instr::GetType { .. }
        | Instr::GetTID { .. }
        | Instr::Unref { .. }
        | Instr::SetRef { .. }
        | Instr::RefData { .. }
        | Instr::RefOffset { .. }
        | Instr::MakeEnum { .. }
        | Instr::EnumAlloc { .. }
        | Instr::EnumIndex { .. }
        | Instr::EnumField { .. }
        | Instr::SetEnumField { .. }
        | Instr::CellGet { .. }
        | Instr::CellSet { .. }
        | Instr::CellIncr { .. }
        | Instr::CellDecr { .. }
        | Instr::CellRef { .. }
        | Instr::Assert
        | Instr::Prefetch { .. }
        | Instr::Pos { .. }
        | Instr::Asm { .. } => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn invalidate_releases_prepared_body_after_its_active_call_returns() {
        let ir = Function::new(Vec::new());
        let ser = air::v2::serialize(&ir).unwrap();
        let canonical = Arc::new(ash_core::air_pipeline::Optimized {
            ir,
            ser: ser.into(),
        });
        let weak = Arc::downgrade(&canonical);
        let mut cache = Cache::default();
        cache.bodies.push(Body::Ready(Rc::new(Prepared {
            optimized: canonical,
            instructions: Vec::new(),
            shim: HLFunction::default(),
            cell_base: 0,
            cfg: ash_core::air_pipeline::AirConfigKey::interpreter(),
            positions: Box::new([]),
            osr: OnceLock::new(),
        })));
        let active = cache.body(0).unwrap();
        assert!(active.osr.get().is_none(), "OSR data must not be eager");
        // Entry-only promotion retires the cache's reference while an
        // activation still owns its source and transfer coordinates.
        cache.forget(0);
        assert!(cache.body(0).is_none());
        assert!(cache.needs_prepare(0));
        assert!(weak.upgrade().is_some());
        cache.invalidate();
        assert!(cache.body(0).is_none());
        assert!(weak.upgrade().is_some(), "active frame still owns its body");
        drop(active);
        assert!(weak.upgrade().is_none(), "retired body must be freed");
    }
}
