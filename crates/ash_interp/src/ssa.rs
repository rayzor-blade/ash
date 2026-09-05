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
//! # Known gap
//!
//! The opcode loop polls for a pending hot reload after every call; this one
//! does not, so a reload requested while an SSA frame is on the stack waits
//! until control is back in an opcode frame. When it does land, the reload path
//! calls [`Cache::invalidate`] — bodies prepared from the old bytecode describe
//! functions the new bytecode may not even have at the same findex.

use std::sync::OnceLock;

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
    pub ir: &'static Function,
    /// `HLFunction` with `regs` = per-value types (values then cells) and no
    /// `ops`. See the module docs: a type view, not a serialized body.
    pub shim: &'static HLFunction,
    /// Where the cell block starts in the frame, i.e. `ir.values.len()`.
    pub cell_base: u32,
    /// Bytecode pc each AIR block starts at, so a hot back edge can name its
    /// header the way the tiering and OSR machinery do -- both key on a pc,
    /// and `compile_osr_entry` finds the block by searching this same table.
    pub block_pcs: &'static [usize],
    /// Bytecode pc of each instruction, indexed by block then position, so a
    /// frame can name the instruction that raised rather than its block.
    pub instr_pcs: &'static [Vec<usize>],
    /// Which values are live where, for building an OSR entry's live state.
    pub liveness: &'static air::v2::liveness::Liveness,
    /// Type of each serialized register, for encoding that state.
    pub osr_reg_types: &'static [TypeRef],
    /// The configuration this body was lowered under.
    ///
    /// Carried rather than recomputed because the point of checking it is to
    /// catch a walker that has drifted off the configuration the OSR entries
    /// were built from -- asking `interpreter_config_for` again at the
    /// transfer would just re-derive the answer both sides were supposed to
    /// have used, which is the assumption under test.
    pub cfg: ash_core::air_pipeline::AirConfigKey,
}

/// What a function executes, decided once on its first call.
enum Body {
    Untried,
    /// Leaked rather than stored inline: a frame holds its `Prepared` for the
    /// whole call, and a nested call preparing a function it has not seen
    /// before can grow `bodies` and move everything in it. Owning the
    /// `Prepared` by reference means that growth cannot move what an outer
    /// frame is executing.
    Ready(&'static Prepared),
    /// The pipeline refused it, or its IR uses something not implemented here.
    Raw,
}

/// Per-function prepared IR, plus the module view it was lowered against.
#[derive(Default)]
pub struct Cache {
    module: Option<(*const DecodedBytecode, &'static AshModule<'static>)>,
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
        enabled() && !matches!(self.bodies.get(func_idx), Some(Body::Ready(_)) | Some(Body::Raw))
    }

    pub fn prepare(&mut self, bc: &DecodedBytecode, func_idx: usize) {
        if !enabled() {
            return;
        }

        let key = bc as *const DecodedBytecode;
        if self.module.map(|(p, _)| p) != Some(key) {
            // Built once and leaked with its bytecode borrow widened, for the
            // same reasons `crate::air::Cache::prepare` documents: `AshModule`
            // indexes every function and native, and `HLInterpreter` has no
            // lifetime to hang the borrow on. The pointer key is what keeps
            // that honest — a hot reload installs a different bytecode, and the
            // stale module leaves the cache here before anything reads it.
            let leaked: &AshModule<'_> = Box::leak(Box::new(AshModule::new(bc)));
            let m: &'static AshModule<'static> = unsafe { std::mem::transmute(leaked) };
            self.bodies.clear();
            self.module = Some((key, m));
        }

        if self.bodies.len() < bc.functions.len() {
            self.bodies
                .resize_with(bc.functions.len(), || Body::Untried);
        }
        if !matches!(self.bodies[func_idx], Body::Untried) {
            return;
        }

        let m = self.module.expect("module cached just above").1;
        let raw = &bc.functions[func_idx];
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
        let view = if air_cfg.callees_visible {
            m
        } else {
            bare = m.without_callees_view();
            &bare
        };
        self.bodies[func_idx] = match ash_core::air_pipeline::optimized_with_config(view, raw, air_cfg)
            .map(|o| (o.ir.clone(), o.ser.clone()))
        {
            Ok((ir, ser_view)) => match unsupported(&ir) {
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
                    let cell_base = ir.values.len() as u32;
                    // Serialized once, for the block -> pc table alone: the
                    // tiering and OSR machinery key a loop header on a
                    // bytecode pc, and this is where that mapping is computed.
                    // The body itself is still executed from the IR; only the
                    // table is kept.
                    let block_pcs: Vec<usize> = ser_view.block_pcs.clone();
                    let instr_pcs: Vec<Vec<usize>> = ser_view.instr_pcs.clone();
                    let cfg = air::v2::CfgInfo::build(&ir);
                    let liveness = air::v2::liveness::Liveness::analyze(&ir, &cfg);
                    let osr_reg_types: Vec<TypeRef> =
                        ser_view.reg_types.iter().map(|t| TypeRef(t.0 as usize)).collect();
                    let mut shim = raw.clone();
                    // air numbers types with u32, ash with usize; same indices.
                    shim.regs = ir
                        .values
                        .iter()
                        .map(|v| TypeRef(v.ty.0 as usize))
                        .chain(ir.cells.iter().map(|c| TypeRef(c.ty.0 as usize)))
                        .collect();
                    shim.ops = Vec::new();
                    // Indexed by opcode, and the pcs this interpreter
                    // publishes are indexed into the SERIALIZED opcodes -- so
                    // the table has to be the one built for those. Leaving it
                    // empty sent a trace to `air.body()`, which under
                    // ASH_AIR=v2 is the raw function, whose numbering the
                    // optimizer has already changed: every reported line was
                    // off by the drift between them.
                    shim.debug = crate::air::optimized_debug(raw, &ser_view.ops);
                    if logging() {
                        eprintln!(
                            "[ssa] findex={} {} ops {} -> {} values {} cells {} blocks",
                            raw.findex,
                            raw.name(),
                            raw.ops.len(),
                            ir.values.len(),
                            ir.cells.len(),
                            ir.blocks.len()
                        );
                    }
                    self.prepared += 1;
                    Body::Ready(Box::leak(Box::new(Prepared {
                        ir: Box::leak(Box::new(ir)),
                        shim: Box::leak(Box::new(shim)),
                        cell_base,
                        block_pcs: Box::leak(block_pcs.into_boxed_slice()),
                        instr_pcs: Box::leak(instr_pcs.into_boxed_slice()),
                        liveness: Box::leak(Box::new(liveness)),
                        osr_reg_types: Box::leak(osr_reg_types.into_boxed_slice()),
                        cfg: air_cfg,
                    })))
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
    pub fn body(&self, func_idx: usize) -> Option<&'static Prepared> {
        match self.bodies.get(func_idx) {
            Some(Body::Ready(p)) => Some(p),
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
    /// each holds its own leaked `Prepared` for the length of its call, which
    /// is exactly why these are leaked rather than owned here. That is also
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
