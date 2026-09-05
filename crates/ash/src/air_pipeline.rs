//! The bridge from ash's decoded bytecode to the AIR v2 typed-SSA pipeline.
//!
//! AIR v2 is written against an embedder-supplied [`ModuleInfo`] rather than
//! against `DecodedBytecode`, so that `air` stays dependency-free. This module
//! is ash's side of that contract: [`AshModule`] answers the three questions
//! v2 asks about a module, and [`optimize`] runs the one pipeline every
//! backend is meant to share:
//!
//! ```text
//! ops + reg_types --lower_with--> Function --PassManager--> verify --serialize--> ops'
//! ```
//!
//! Routing every engine through that pipeline is what makes the interpreter,
//! Cranelift and LLVM consume an *identical* optimized opcode array, which is
//! also the property on-stack replacement needs: an opcode index means the
//! same thing in every tier only if every tier was handed the same array.
//!
//! Nothing here is wired into an engine yet. [`report`] measures whether v2
//! can round-trip a program at all (`ASH_VERIFY_AIR=only`), which has to come
//! before switching anything over.
//!
//! # Why `lower_with` and never `lower`
//!
//! The bare `air::v2::lower` wrapper lowers against `NoModuleInfo`, whose
//! `is_float` answers `false` for every type — so the FMA peephole finds no
//! float arithmetic and silently does nothing, and the native table comes out
//! empty. Every path in this module goes through `lower_with(.., AshModule)`.

use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap};
use std::sync::{Arc, Mutex, OnceLock};
use std::fmt;
use std::panic::{self, AssertUnwindSafe};

use air::v2::module::{CalleeBody, ModuleInfo, NativeImport};
use air::v2::{
    lower_with_positions, serialize, verify, Function, OptLevel, PassManager, PassOptions,
    PassReport, Serialized, TypeRef,
};

use crate::bytecode::DecodedBytecode;
use crate::hl::{hl_type_kind_HF32, hl_type_kind_HF64};
use crate::types::HLFunction;

// Re-exported so callers (ash_cli) can drive the pipeline without taking their
// own dependency edge on `air`.
pub use air::v2::{OptLevel as AirOptLevel, PassOptions as AirPassOptions};

// ---------------------------------------------------------------------------
// ModuleInfo
// ---------------------------------------------------------------------------

/// ash's answer to [`ModuleInfo`]: natives by `findex`, HL float types, and
/// bytecode bodies for the inliner.
///
/// Build it once per module — the two `findex` maps are the whole cost, and
/// rebuilding them per function would make a whole-module sweep quadratic.
pub struct AshModule<'b> {
    bc: &'b DecodedBytecode,
    /// `findex` -> index into `bc.natives`. Shared, so that deriving a second
    /// view of the same module (see [`AshModule::without_callees_view`]) does
    /// not rebuild the maps this type exists to build once.
    native_at: Arc<HashMap<usize, usize>>,
    /// `findex` -> index into `bc.functions`.
    function_at: Arc<HashMap<usize, usize>>,
    /// Whether [`ModuleInfo::callee`] hands out bodies. Off means the inliner
    /// is inert, which is how a caller measures the module without letting
    /// inlining reshape the functions it is measuring.
    offer_callees: bool,
    /// Callees already lowered, by findex.
    ///
    /// The inliner asks for a body once per candidate site, and handing it
    /// `CalleeBody::Bytecode` made that a clone of the opcode array plus a
    /// fresh lowering every time. Lowering a given callee is the same work
    /// whoever asks, so it is done once: measured on deltablue, the inliner
    /// was 11.2ms of the 15.2ms this pipeline spent preparing bodies.
    lowered: Mutex<HashMap<usize, Function>>,
}

impl<'b> AshModule<'b> {
    pub fn new(bc: &'b DecodedBytecode) -> Self {
        AshModule {
            bc,
            native_at: Arc::new(
                bc.natives
                    .iter()
                    .enumerate()
                    .map(|(i, n)| (n.findex as usize, i))
                    .collect(),
            ),
            function_at: Arc::new(
                bc.functions
                    .iter()
                    .enumerate()
                    .map(|(i, f)| (f.findex as usize, i))
                    .collect(),
            ),
            offer_callees: true,
            lowered: Mutex::new(HashMap::new()),
        }
    }

    /// Withhold callee bodies, making [`air::v2::passes::Inlining`] inert even
    /// at `O3`.
    pub fn without_callees(mut self) -> Self {
        self.offer_callees = false;
        self
    }

    /// The same module with callee bodies withheld, without rebuilding the
    /// findex maps — for the callers that need both views of one module, or
    /// that hold the with-callees view by reference.
    pub fn without_callees_view(&self) -> AshModule<'b> {
        AshModule {
            bc: self.bc,
            native_at: Arc::clone(&self.native_at),
            function_at: Arc::clone(&self.function_at),
            offer_callees: false,
            lowered: Mutex::new(HashMap::new()),
        }
    }

    pub fn bytecode(&self) -> &'b DecodedBytecode {
        self.bc
    }

    /// The bytecode function with this `findex`, if it is a bytecode function
    /// (natives and unknown indices answer `None`).
    pub fn function(&self, findex: usize) -> Option<&'b HLFunction> {
        self.function_at
            .get(&findex)
            .map(|&i| &self.bc.functions[i])
    }

    /// `(args, ret)` of the `HFUN` type at `ty`.
    ///
    /// A native's type is always `HFUN` in well-formed bytecode. If the
    /// decoder ever hands over something else, declare the import with an
    /// empty signature rather than dropping it: a missing declaration would
    /// make a backend re-derive the symbol from bytecode, which is exactly the
    /// duplication the native table exists to remove.
    fn signature(&self, ty: usize) -> (Vec<TypeRef>, TypeRef) {
        match self.bc.types.get(ty).and_then(|t| t.fun.as_ref()) {
            Some(f) => (
                f.args.iter().map(|a| TypeRef(a.0 as u32)).collect(),
                TypeRef(f.ret.0 as u32),
            ),
            None => (Vec::new(), TypeRef(ty as u32)),
        }
    }
}

impl<'b> ModuleInfo for AshModule<'b> {
    fn native(&self, findex: usize) -> Option<NativeImport> {
        let n = &self.bc.natives[*self.native_at.get(&findex)?];
        let (args, ret) = self.signature(n.type_.0);
        Some(NativeImport::new(
            findex,
            n.lib.clone(),
            n.name.clone(),
            args,
            ret,
        ))
    }

    fn int_value(&self, idx: usize) -> Option<i32> {
        self.bc.ints.get(idx).copied()
    }

    fn int_pool_len(&self) -> usize {
        self.bc.ints.len()
    }

    fn type_size(&self, ty: TypeRef) -> Option<u32> {
        // The array stride, not `layout::type_size`: the two differ exactly
        // where a size is meaningless (HVOID, HPACKED, both zero there), and
        // an element width of zero would make every lane the same address.
        self.bc
            .types
            .get(ty.0 as usize)
            .map(|t| crate::layout::array_elem_size(t.kind))
            .filter(|n| *n > 0)
            .map(|n| n as u32)
    }

    fn is_float(&self, ty: TypeRef) -> bool {
        self.bc
            .types
            .get(ty.0 as usize)
            .is_some_and(|t| t.kind == hl_type_kind_HF32 || t.kind == hl_type_kind_HF64)
    }

    fn intrinsic_of(&self, findex: usize) -> Option<air::v2::ir::IntrinsicKind> {
        use air::v2::ir::IntrinsicKind as K;
        use crate::intrinsics::NativeIntrinsic as NI;
        let n = &self.bc.natives[*self.native_at.get(&findex)?];
        if n.lib != "std" && n.lib != "?std" {
            return None;
        }
        // ptr_compare is not in the backend emitter table (it is two-arg);
        // its body is `(a as usize).cmp(&b)` in std/src/cast.rs.
        if n.name == "ptr_compare" {
            return Some(K::PtrCompare);
        }
        Some(match crate::intrinsics::lookup("std", n.name.as_str())? {
            NI::Sqrt => K::Sqrt,
            NI::Abs => K::Abs,
            NI::Floor => K::Floor,
            NI::Ceil => K::Ceil,
            NI::RoundHalfUp => K::RoundHalfUp,
            NI::FloorToI32 => K::FloorToI32,
            NI::CeilToI32 => K::CeilToI32,
            NI::RoundHalfUpToI32 => K::RoundHalfUpToI32,
            NI::IsNaN => K::IsNaN,
            NI::IsFinite => K::IsFinite,
        })
    }

    /// Resolve a proto slot against the receiver's STATIC type.
    ///
    /// `slot` is the vtable index, not a position in `proto` -- the entry is
    /// the one whose `pindex` matches, which is how the JIT reads it too. A
    /// type that does not declare the slot inherits it, so the walk continues
    /// into `super_`; the first match is the implementation this static type
    /// carries.
    fn method_target(&self, ty: TypeRef, slot: usize) -> Option<usize> {
        let mut cur = Some(ty.0 as usize);
        // The chain is finite, but a malformed table must not spin.
        for _ in 0..64 {
            let obj = self.bc.types.get(cur?)?.obj.as_ref()?;
            if let Some(p) = obj.proto.iter().find(|p| p.pindex as usize == slot) {
                return usize::try_from(p.findex).ok();
            }
            cur = obj.super_.as_ref().map(|t| t.0);
        }
        None
    }

    fn callee(&self, findex: usize) -> Option<CalleeBody> {
        if !self.offer_callees {
            return None;
        }
        // Measurement knob: how much of `air prepare` is the inliner asking
        // for bodies, and what does losing it cost at run time.
        if std::env::var_os("ASH_AIR_NO_INLINE").is_some() {
            return None;
        }
        if let Some(hit) = self
            .lowered
            .lock()
            .expect("callee cache poisoned")
            .get(&findex)
        {
            return Some(CalleeBody::Air(hit.clone()));
        }
        let f = self.function(findex)?;
        // Lowered against a module that offers no callees: this is the callee's
        // own body, and letting it inline its own callees here would expand
        // the same work at every site that asks for it.
        let bare = self.without_callees_view();
        let body = air::v2::lower::lower_with(&f.ops, &reg_types_of(f), &bare).ok()?;
        self.lowered
            .lock()
            .expect("callee cache poisoned")
            .insert(findex, body.clone());
        Some(CalleeBody::Air(body))
    }
}

/// A function's register-type table as v2 type refs. ash indexes the module
/// type table with `usize`, v2 with `u32`; the values are the same indices.
pub fn reg_types_of(f: &HLFunction) -> Vec<TypeRef> {
    f.regs.iter().map(|r| TypeRef(r.0 as u32)).collect()
}

// ---------------------------------------------------------------------------
// errors
// ---------------------------------------------------------------------------

/// A step of the pipeline, so a failure can be attributed to one.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Stage {
    /// `lower_with`: bytecode -> typed SSA.
    Lower,
    /// `verify` of the freshly lowered IR, before any pass ran. Separating
    /// this from [`Stage::Verify`] is what tells "lowering is wrong" apart
    /// from "a pass broke it".
    VerifyLowered,
    /// `serialize` of the freshly lowered IR: the identity round trip.
    SerializeIdentity,
    /// `PassManager::run`.
    Optimize,
    /// `verify` of the optimized IR.
    Verify,
    /// `serialize` of the optimized IR.
    Serialize,
}

impl Stage {
    pub fn as_str(self) -> &'static str {
        match self {
            Stage::Lower => "lower",
            Stage::VerifyLowered => "verify-lowered",
            Stage::SerializeIdentity => "serialize-identity",
            Stage::Optimize => "optimize",
            Stage::Verify => "verify",
            Stage::Serialize => "serialize",
        }
    }
}

impl fmt::Display for Stage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

/// A pipeline failure, naming the function and the stage.
///
/// The next phase of the migration works from these, so the stage and the
/// `findex` are structured fields rather than text baked into one string.
#[derive(Debug)]
pub struct PipelineError {
    pub findex: i32,
    pub name: String,
    pub stage: Stage,
    /// What the stage reported. `panicked` marks the ones that came out of
    /// [`catch_unwind`](std::panic::catch_unwind) rather than a `Result`.
    pub cause: String,
    pub panicked: bool,
}

impl PipelineError {
    /// The cause on one line, capped, for a report that has thousands of rows.
    /// Verifier errors carry a full IR dump; that belongs in `dump:<findex>`.
    pub fn brief(&self) -> String {
        let first = self.cause.lines().next().unwrap_or("").trim();
        let mut s: String = first.chars().take(160).collect();
        if first.chars().count() > 160 {
            s.push('…');
        }
        if self.panicked {
            format!("panic: {s}")
        } else {
            s
        }
    }
}

impl fmt::Display for PipelineError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "AIR {} failed for findex={} {}: {}",
            self.stage,
            self.findex,
            self.name,
            if self.panicked {
                format!("panicked: {}", self.cause)
            } else {
                self.cause.clone()
            }
        )
    }
}

impl std::error::Error for PipelineError {}

// ---------------------------------------------------------------------------
// the pipeline
// ---------------------------------------------------------------------------

/// Lower, optimize, verify and serialize one function.
///
/// Verification runs *before* serialization on purpose: a serializer fed
/// invalid IR produces plausible-looking opcodes, and the resulting corruption
/// surfaces as a wrong answer at run time in whichever engine consumed it.
///
/// Builds an [`AshModule`] per call, so use [`optimize_with`] when sweeping a
/// whole module.
pub fn optimize(
    bc: &DecodedBytecode,
    f: &HLFunction,
    level: OptLevel,
) -> Result<Serialized, PipelineError> {
    optimize_with(&AshModule::new(bc), f, level)
}

/// [`optimize`] against a module view built once.
/// Whether AIR runs at all, from `ASH_AIR`. The one reader of that variable.
///
/// There were four, and they disagreed. `ash_interp::air` treated unset as on;
/// `ash_core::llvm::air` treated unset as V2; `ash_core::cranelift::air` treated unset as
/// **Off**, so the Cranelift tier had never once compiled optimized AIR in a
/// default run; and `ash_interp::ssa` matched only the exact string "v2".
/// Every one of them was a private `OnceLock` reading the same variable to a
/// different conclusion, which is how a flag ends up meaning four things.
///
/// On unless explicitly disabled. `ASH_AIR=0` and `ASH_AIR=off` turn it off.
/// The other spellings are a *consumer* choice and do not change whether AIR
/// runs: the direct-SSA interpreter in `ash_interp::ssa` is what an unset
/// variable selects, and `ASH_AIR=v2-serialize` picks the older path that
/// serializes optimized AIR back to opcodes and interprets those.
pub fn air_enabled() -> bool {
    static CELL: OnceLock<bool> = OnceLock::new();
    *CELL.get_or_init(|| match std::env::var("ASH_AIR").as_deref() {
        Ok("0") | Ok("off") | Ok("none") => false,
        Err(_) | Ok("") | Ok("v2") | Ok("2") | Ok("v2-serialize") => true,
        Ok(other) => {
            eprintln!("[air] ignoring ASH_AIR='{other}' (expected v2|v2-serialize|off); AIR is on");
            true
        }
    })
}

/// Whether lowering records source positions for a shadow call stack.
///
/// Set once by the AOT driver from the target it is building for: a
/// WebAssembly module has no machine stack the runtime could walk, so each
/// of its frames records its own position (`TargetAbi::shadow_call_stack`).
/// Process-global like the level, because every lowering in a build is for
/// that one target, and a JIT or interpreter process -- which never sets it
/// -- keeps producing AIR without markers.
static SHADOW_FRAMES: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

pub fn set_shadow_frames(on: bool) {
    SHADOW_FRAMES.store(on, std::sync::atomic::Ordering::Relaxed);
}

pub fn shadow_frames() -> bool {
    SHADOW_FRAMES.load(std::sync::atomic::Ordering::Relaxed)
}

/// The per-op positions `f` lowers with: its debug table when this process
/// builds for a shadow-stack target, nothing otherwise.
fn positions_of(f: &HLFunction) -> Option<&[i32]> {
    shadow_frames().then(|| f.debug.as_slice())
}

/// The one optimization level, from `ASH_AIR_LEVEL`.
///
/// One, because there is now one optimized AIR per function and every consumer
/// reads it. The tiers each used to pick their own -- `ash_interp::air::level`,
/// `jit::air::level`, `CraneliftAirConfig::level` -- which was only possible
/// while each was optimizing a private copy.
///
/// O3 is the default. O2 leaves the interpreter *slower* than no pipeline at
/// all (2.33s against 2.12s on mandelbrot_small, measured); the passes that
/// pay -- inlining, scalar replacement, allocation hoisting -- are the ones O3
/// adds. Shipping the level that loses was a decision nobody made.
pub fn default_level() -> OptLevel {
    static CELL: OnceLock<OptLevel> = OnceLock::new();
    *CELL.get_or_init(|| match std::env::var("ASH_AIR_LEVEL") {
        Ok(s) if !s.is_empty() => parse_level(&s).unwrap_or_else(|| {
            eprintln!("[air] ignoring ASH_AIR_LEVEL='{s}' (expected O0|O1|O2|O3); using O3");
            OptLevel::O3
        }),
        _ => OptLevel::O3,
    })
}

/// One function's AIR, lowered and optimized exactly once.
///
/// Bytecode lowers to AIR once and every consumer reads that. Before this,
/// five callers each ran the pipeline from scratch -- the interpreter, the SSA
/// interpreter, the Cranelift tier, the LLVM tier and the OSR eligibility
/// check -- so a function that ran interpreted and then promoted twice was
/// optimized three times over, into three separately renumbered opcode arrays.
/// That is why an opcode index meant different things in different tiers,
/// which is a property on-stack replacement needs to hold.
///
/// `ser` is the flat form, kept only for the consumers that still read
/// opcodes. It is a *product* of the IR, not a stage the IR passes through on
/// its way to a backend: a backend composes CLIF or LLVM IR from `ir`.
pub struct Optimized {
    pub ir: Function,
    pub ser: Serialized,
}

/// What a cached entry was produced under.
///
/// Part of the key, not an assumption. Two consumers legitimately want
/// different pipelines from the same function, and serving one the other's
/// result is silent and wrong rather than loud:
///
/// * a hot-reload build runs at O2 with callee bodies hidden, deliberately --
///   inlining across a reload boundary means an edit to a callee stops taking
///   effect between iterations (`jit::air::run_v2`);
/// * `ASH_AIR_FMA=0` turns off the FMA peephole, which is the one pass whose
///   output is observable as a different *number* rather than different code.
///
/// Keying on findex alone would have let whichever consumer asked first decide
/// both for everyone else.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct AirConfigKey {
    pub level: OptLevel,
    pub fma: bool,
    /// Whether the inliner may see callee bodies.
    pub callees_visible: bool,
}

impl AirConfigKey {
    /// The configuration a normal run uses.
    pub fn standard() -> Self {
        Self {
            level: default_level(),
            fma: PassOptions::default().fma,
            callees_visible: true,
        }
    }

    /// The configuration the *interpreter* walks: [`standard`] with the
    /// inliner held off.
    ///
    /// Not a tuning preference. Inlining is a codegen optimization, and the
    /// walker is not codegen: a callee inlined into a body the tiers have not
    /// promoted is a callee the interpreter now executes itself, instead of
    /// calling the compiled version that already exists. Inlining therefore
    /// moves work *out* of the tiers and back into the walker. Measured on
    /// binary_trees, where it costs 68ms of a 425ms run:
    ///
    /// | | inlined | not inlined |
    /// |---|---|---|
    /// | interpreter | 99ms | 31ms |
    /// | cranelift+llvm | 56ms | 72ms |
    ///
    /// Entry-point promotion keeps [`standard`], so the shapes inlining exists
    /// for still get it once they are compiled -- fib is 8.5x on that alone.
    ///
    /// **Any consumer that maps an interpreter pc onto compiled code must use
    /// this, not [`standard`].** OSR hands a live interpreter frame to a
    /// compiled body by position, through `Optimized::ser.block_pcs`; if the
    /// two were lowered under different configurations those positions
    /// describe different bodies, and the transfer silently reads the wrong
    /// state. It does not fail closed -- closure_call returned a different
    /// checksum on every run until the OSR sites were moved onto this key.
    pub fn interpreter() -> Self {
        Self {
            callees_visible: false,
            level: interpreter_level(),
            ..Self::standard()
        }
    }
}

/// The pipeline level for a function only the interpreter will ever walk.
///
/// Separate from [`default_level`] because the two consumers are asking
/// different questions. A compiler tier is buying code it will run millions of
/// times and the pipeline is the cheapest part of that; the interpreter is
/// buying a body it may run twice, and on a program that finishes in 20ms the
/// preparation IS the program. Measured on the NUC, `ASH_AIR_LEVEL=O1` took
/// test_stdlib from 48.0ms to 16.1ms and jsonarr from 25.9 to 13.7 -- and cost
/// mandelbrot_small a factor of five, because a global level also lowers what
/// the COMPILERS consume.
///
/// This is only reachable through [`AirConfigKey::interpreter`], which
/// [`interpreter_config_for`] hands out exclusively to functions with no back
/// edge. Those are precisely the functions OSR can never enter, so nothing
/// maps onto them by position and nothing else has to agree about how they
/// were lowered. A loop-carrying function keeps [`AirConfigKey::standard`] and
/// therefore keeps O3, on both sides of the transfer.
pub fn interpreter_level() -> OptLevel {
    static CELL: OnceLock<OptLevel> = OnceLock::new();
    *CELL.get_or_init(|| match std::env::var("ASH_AIR_INTERP_LEVEL") {
        Ok(s) if !s.is_empty() => parse_level(&s).unwrap_or_else(|| {
            eprintln!(
                "[air] ignoring ASH_AIR_INTERP_LEVEL='{s}' (expected O0|O1|O2|O3); \
                 using the pipeline default"
            );
            default_level()
        }),
        _ => default_level(),
    })
}

/// Which functions the interpreter prepares under the SHARED configuration,
/// rather than its own cheaper one.
///
/// The rule has to name every function OSR might enter, because the transfer
/// is by position and both sides must lower the function identically. Naming
/// more than that is pure cost: the pipeline's expense is concentrated in
/// exactly these functions -- measured with `ASH_AIR_TIME_PASSES`, the
/// inliner spends 8.30ms of its 8.31ms on the 17 loop-carrying functions of
/// test_stdlib's 66, and GVN and SROA the same way -- so every function this
/// over-names is paid for at full price and returns nothing.
///
/// Selected by `ASH_AIR_INTERP_POLICY`, so the three can be measured against
/// each other rather than argued about.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum InterpPolicy {
    /// A back edge means shared. The rule the tree shipped: correct, and the
    /// most conservative of the three -- every loop is treated as an OSR
    /// target whether or not one could ever enter it.
    BackEdge,
    /// A back edge AND nothing that makes `osr::analyze` refuse outright.
    ///
    /// A live trap region or a register whose address escaped is a permanent
    /// refusal, not a hotness question, and both are visible in the raw
    /// opcodes -- so a loop behind either can never be an OSR target and has
    /// no reason to pay for the shared configuration. mandelbrot_small is the
    /// case in hand: every one of its loops is refused for `RefTaken`, and it
    /// still prepares its hot function at full price.
    OsrEligible,
    /// Cheap until the interpreter DEMONSTRATES a hot loop, then shared.
    ///
    /// The sharpest rule and the one with a cost the others do not have: a
    /// frame holds its prepared body for the whole call, so the frame that
    /// reported the hot loop keeps walking the cheap one and cannot be
    /// transferred into -- the guard in `try_osr_transfer` refuses it on the
    /// configuration mismatch. A function whose loop runs once, in a `main`
    /// called once, therefore loses OSR entirely under this policy. Later
    /// calls get it. Whether that trade pays is what the A/B is for.
    Demand,
}

/// The policy in force, from `ASH_AIR_INTERP_POLICY` (backedge | osr-eligible
/// | demand). Default `BackEdge`, which is what the tree did before the knob.
pub fn interp_policy() -> InterpPolicy {
    static CELL: OnceLock<InterpPolicy> = OnceLock::new();
    *CELL.get_or_init(|| match std::env::var("ASH_AIR_INTERP_POLICY").as_deref() {
        Ok("osr-eligible") => InterpPolicy::OsrEligible,
        Ok("demand") => InterpPolicy::Demand,
        Ok("backedge") | Err(_) | Ok("") => InterpPolicy::BackEdge,
        Ok(other) => {
            eprintln!(
                "[air] ignoring ASH_AIR_INTERP_POLICY='{other}' \
                 (expected backedge|osr-eligible|demand); using backedge"
            );
            InterpPolicy::BackEdge
        }
    })
}

/// Findexes the interpreter has shown to carry a hot loop, under
/// [`InterpPolicy::Demand`].
///
/// Monotonic on purpose: a findex only ever moves from cheap to shared. If it
/// could move back, two consumers asking at different moments would get
/// different answers for the same function, which is the one thing the OSR
/// transfer cannot survive.
static OSR_DEMAND: OnceLock<Mutex<std::collections::HashSet<i32>>> = OnceLock::new();

fn osr_demand() -> &'static Mutex<std::collections::HashSet<i32>> {
    OSR_DEMAND.get_or_init(|| Mutex::new(std::collections::HashSet::new()))
}

/// Record that `findex` carries a hot loop, so it is prepared under the shared
/// configuration from its next preparation onward.
///
/// Returns whether this was news, which is what tells a caller it has a cached
/// body to drop.
pub fn note_osr_demand(findex: i32) -> bool {
    if interp_policy() != InterpPolicy::Demand {
        return false;
    }
    osr_demand()
        .lock()
        .expect("osr demand mutex poisoned")
        .insert(findex)
}

fn has_opcode(f: &HLFunction, pred: impl Fn(&air::opcodes::Opcode) -> bool) -> bool {
    f.ops.iter().any(pred)
}

/// The configuration the interpreter walks `f` under, and the one every OSR
/// site must lower `f` under to stay positionally compatible with it.
///
/// [`AirConfigKey::interpreter`] is the profitable choice for a function the
/// tiers will only ever replace at its entry, and the wrong one for a function
/// OSR can enter. A loop-carrying function is exactly the second case: OSR
/// enters it at a loop header, so its OSR body *is* the compiled body that
/// runs the loop, and lowering that without the inliner costs far more than
/// keeping the walker's callees compiled ever returns -- mandelbrot ran 7.5x
/// slower, and lost the cross-call FMA pairs its checksum depends on. A
/// function with no back edge is never an OSR target, so nothing maps onto it
/// by position and the walker keeps its callees compiled.
///
/// The back edge is read off the raw opcodes on purpose: choosing the
/// configuration cannot require the AIR that the configuration produces.
pub fn interpreter_config_for(f: &HLFunction) -> AirConfigKey {
    let has_back_edge = f
        .ops
        .iter()
        .any(|op| air::opcode_info::jump_offset(op).is_some_and(|d| d < 0));
    let shared = match interp_policy() {
        InterpPolicy::BackEdge => has_back_edge,
        InterpPolicy::OsrEligible => {
            // The two refusals `osr::analyze` never takes back, read off the
            // raw opcodes so that choosing a configuration does not require
            // the AIR the configuration produces. A `Ref` pins its register
            // as `RefTaken`; a `Trap` is a live catch region across the
            // transfer. Either one means no entry will ever be built here,
            // whatever the loop does.
            has_back_edge
                && !has_opcode(f, |op| matches!(op, air::opcodes::Opcode::Ref { .. }))
                && !has_opcode(f, |op| matches!(op, air::opcodes::Opcode::Trap { .. }))
        }
        InterpPolicy::Demand => {
            has_back_edge
                && osr_demand()
                    .lock()
                    .expect("osr demand mutex poisoned")
                    .contains(&f.findex)
        }
    };
    if shared {
        AirConfigKey::standard()
    } else {
        AirConfigKey::interpreter()
    }
}

type CacheKey = (i32, AirConfigKey);

static OPTIMIZED: OnceLock<Mutex<HashMap<CacheKey, Arc<Optimized>>>> = OnceLock::new();

fn optimized_cache() -> &'static Mutex<HashMap<CacheKey, Arc<Optimized>>> {
    OPTIMIZED.get_or_init(|| Mutex::new(HashMap::new()))
}

/// The optimized AIR for `f`, computing it on first ask.
///
/// Shared across threads because the tiers are: the interpreter asks on the
/// main thread while a broker compiles on another. The pipeline runs outside
/// the lock -- it is the expensive part, and holding a process-wide lock
/// across it would serialize the brokers against the interpreter.
pub fn optimized(m: &AshModule, f: &HLFunction) -> Result<Arc<Optimized>, PipelineError> {
    optimized_with_config(m, f, AirConfigKey::standard())
}

/// [`optimized`] under an explicit configuration, for the consumers that need
/// one that is not the default.
pub fn optimized_with_config(
    m: &AshModule,
    f: &HLFunction,
    cfg: AirConfigKey,
) -> Result<Arc<Optimized>, PipelineError> {
    let key = (f.findex, cfg);
    if let Some(hit) = optimized_cache()
        .lock()
        .expect("air cache poisoned")
        .get(&key)
    {
        return Ok(Arc::clone(hit));
    }
    let mut opts = PassOptions::default();
    opts.fma = cfg.fma;
    opts.time_passes = time_passes();
    // The bisect switch for a wrong answer out of a loop: the vectorizer is
    // the one O3 pass that rewrites arithmetic, so "same result with
    // ASH_AIR_NO_WIDEN=1" separates it from the inliner and SROA.
    if std::env::var_os("ASH_AIR_NO_WIDEN").is_some() {
        opts.widen = false;
    }
    // The pipeline runs outside the lock: it is the expensive part, and holding
    // a process-wide lock across it would serialize the brokers against the
    // interpreter.
    let (ser, ir, _report) = optimize_full(m, f, cfg.level, &opts)?;
    let entry = Arc::new(Optimized { ir, ser });
    let mut cache = optimized_cache().lock().expect("air cache poisoned");
    Ok(Arc::clone(
        cache.entry(key).or_insert_with(|| Arc::clone(&entry)),
    ))
}

/// Whether to time each pass, from `ASH_AIR_TIME_PASSES`.
///
/// The pipeline runs per function on the mutator, so on a short program it IS
/// most of the run -- and until this existed the only way to act on that was
/// to drop a whole optimization level, which is one lever for a bill that
/// nine passes share. This says which of them to look at.
pub fn time_passes() -> bool {
    static CELL: OnceLock<bool> = OnceLock::new();
    *CELL.get_or_init(|| {
        std::env::var("ASH_AIR_TIME_PASSES").is_ok_and(|v| v != "0" && !v.is_empty())
    })
}

static PASS_TIME: OnceLock<Mutex<BTreeMap<&'static str, (u128, usize)>>> = OnceLock::new();

fn record_pass_time(report: &PassReport) {
    if report.per_pass_ns.is_empty() {
        return;
    }
    let mut acc = PASS_TIME
        .get_or_init(|| Mutex::new(BTreeMap::new()))
        .lock()
        .expect("pass time mutex poisoned");
    for (name, ns) in &report.per_pass_ns {
        let e = acc.entry(name).or_insert((0, 0));
        e.0 += *ns;
        e.1 += 1;
    }
}

/// Where the pipeline spent its time this process, most expensive first.
///
/// Empty unless `ASH_AIR_TIME_PASSES` was set. The count is functions the
/// pass ran on, not rounds: a pass runs once per manager round and the
/// manager runs up to four, so the per-call figure is an average over
/// however many rounds each function needed.
pub fn pass_time_report() -> Vec<String> {
    let Some(acc) = PASS_TIME.get() else {
        return Vec::new();
    };
    let acc = acc.lock().expect("pass time mutex poisoned");
    let mut rows: Vec<(&'static str, u128, usize)> =
        acc.iter().map(|(n, (ns, c))| (*n, *ns, *c)).collect();
    let total: u128 = rows.iter().map(|(_, ns, _)| *ns).sum();
    rows.sort_by(|a, b| b.1.cmp(&a.1));
    let mut out = vec![format!(
        "[air] pipeline time {:.1}ms over {} function-runs",
        total as f64 / 1e6,
        rows.first().map(|(_, _, c)| *c).unwrap_or(0)
    )];
    for (name, ns, count) in rows {
        out.push(format!(
            "  {name:<28} {:>8.2}ms {:>5.1}%  over {count} fns",
            ns as f64 / 1e6,
            if total == 0 {
                0.0
            } else {
                100.0 * ns as f64 / total as f64
            },
        ));
    }
    out
}

/// Drop every cached function, e.g. after a hot reload replaced the bytecode.
pub fn invalidate_optimized() {
    optimized_cache()
        .lock()
        .expect("air cache poisoned")
        .clear();
}

pub fn optimize_with(
    m: &AshModule,
    f: &HLFunction,
    level: OptLevel,
) -> Result<Serialized, PipelineError> {
    Ok(optimize_full(m, f, level, &PassOptions::default())?.0)
}

/// [`optimize_with`], also returning the optimized IR and what each pass did.
/// The IR is what a backend that consumes the typed form directly will want
/// once the staged migration gets past the shared-opcode-array step.
pub fn optimize_full(
    m: &AshModule,
    f: &HLFunction,
    level: OptLevel,
    opts: &PassOptions,
) -> Result<(Serialized, Function, PassReport), PipelineError> {
    let fail = |stage: Stage, cause: String, panicked: bool| PipelineError {
        findex: f.findex,
        name: f.name(),
        stage,
        cause,
        panicked,
    };

    // Every stage runs under `guard`, the same way the sweep's `trip` does.
    // The point of this function returning `Result` is that a caller can fall
    // back to the unoptimized opcodes for one function and keep going; a
    // panic unwinding into the caller would take that choice away and bring
    // down the compile instead. `stage` runs one and attributes both a
    // returned error and a panic to it.
    macro_rules! stage {
        ($stage:expr, $body:expr) => {
            match guard(|| $body) {
                Ok(Ok(v)) => v,
                Ok(Err(e)) => return Err(fail($stage, format!("{e:#}"), false)),
                Err(p) => return Err(fail($stage, p, true)),
            }
        };
    }

    let mut ir = stage!(
        Stage::Lower,
        lower_with_positions(&f.ops, &reg_types_of(f), m, positions_of(f))
    )
    .with_findex(f.findex as usize);

    let pm = PassManager::with_module(level, m).with_options(*opts);
    let report = stage!(Stage::Optimize, pm.run(&mut ir));
    record_pass_time(&report);
    // Inert unless ASH_DEVIRT_SURVEY asks: how reachable is each closure
    // target AFTER the passes have run, which is the IR a devirtualisation
    // pass would actually see.
    air::v2::passes::survey_closure_targets(&ir, m);

    // Verify before serializing: a serializer fed invalid IR emits
    // plausible-looking opcodes, and the corruption only surfaces as a wrong
    // answer at run time in whichever engine consumed them.
    stage!(Stage::Verify, verify(&ir));

    let out = stage!(Stage::Serialize, serialize(&ir));
    Ok((out, ir, report))
}

/// Lower, optimize and verify one function, stopping short of serialization.
///
/// This is the entry point for a backend that *consumes* the typed SSA form —
/// the SSA interpreter in `ash_interp::ssa` is the first. Serializing for such
/// a consumer would be pure loss: the opcode array it produces has thrown away
/// the types, the effect lattice and the resolved field slots the IR carries,
/// and it costs a pass over the whole function to do it.
pub fn prepare_ir(
    m: &AshModule,
    f: &HLFunction,
    level: OptLevel,
    opts: &PassOptions,
) -> Result<(Function, PassReport), PipelineError> {
    let fail = |stage: Stage, cause: String, panicked: bool| PipelineError {
        findex: f.findex,
        name: f.name(),
        stage,
        cause,
        panicked,
    };

    macro_rules! stage {
        ($stage:expr, $body:expr) => {
            match guard(|| $body) {
                Ok(Ok(v)) => v,
                Ok(Err(e)) => return Err(fail($stage, format!("{e:#}"), false)),
                Err(p) => return Err(fail($stage, p, true)),
            }
        };
    }

    let mut ir = stage!(
        Stage::Lower,
        lower_with_positions(&f.ops, &reg_types_of(f), m, positions_of(f))
    )
    .with_findex(f.findex as usize);

    let pm = PassManager::with_module(level, m).with_options(*opts);
    let report = stage!(Stage::Optimize, pm.run(&mut ir));
    // Inert unless ASH_DEVIRT_SURVEY asks: how reachable is each closure
    // target AFTER the passes have run, which is the IR a devirtualisation
    // pass would actually see.
    air::v2::passes::survey_closure_targets(&ir, m);

    // The consumer executes this IR directly, so verification is the only
    // thing standing between a pass bug and a wrong answer at run time.
    stage!(Stage::Verify, verify(&ir));

    Ok((ir, report))
}

// ---------------------------------------------------------------------------
// the round-trip sweep
// ---------------------------------------------------------------------------

/// One function's trip through the pipeline, as the sweep records it.
#[derive(Debug)]
pub struct Trip {
    pub findex: i32,
    pub name: String,
    /// Opcode and register counts going in.
    pub ops_in: usize,
    pub regs_in: usize,
    /// Opcode and register counts of the *unoptimized* serialization. This is
    /// the number that answers "does the serializer preserve the program",
    /// since no pass has had a chance to legitimately change it.
    pub identity: Option<(usize, usize)>,
    /// Opcode and register counts after the requested opt level.
    pub optimized: Option<(usize, usize)>,
    /// Per-opcode-kind count change of the identity round trip, biggest loss
    /// first. A count alone cannot tell a dropped unreachable `EndTrap` from a
    /// dropped live `SetField`, and only the second is a bug.
    pub identity_kind_delta: Vec<(String, isize)>,
    /// What each pass did, when the optimize stage completed. Reading this
    /// over a whole module is how the [`ModuleInfo`] wiring gets checked:
    /// `fma` fusing nothing on float-heavy code means `is_float` is answering
    /// `false`, which is exactly what lowering through the bare `lower()`
    /// wrapper would cause.
    pub passes: Vec<(&'static str, air::v2::PassStats)>,
    /// The stage that failed. A trip stops at its first failure.
    pub failure: Option<PipelineError>,
}

impl Trip {
    pub fn round_tripped(&self) -> bool {
        self.optimized.is_some()
    }
}

/// Counters over a whole module.
#[derive(Debug, Default)]
pub struct SweepStats {
    pub total: usize,
    /// Functions that reached the end of each stage.
    pub lowered: usize,
    pub verified_lowered: usize,
    pub serialized_identity: usize,
    pub optimized: usize,
    pub verified: usize,
    pub serialized: usize,
    /// Failures per stage, in stage order.
    pub failures: Vec<PipelineError>,
    /// Identity round trip: opcode count went down / stayed / went up.
    pub id_ops_fewer: usize,
    pub id_ops_same: usize,
    pub id_ops_more: usize,
    /// Identity round trip: register count went down / stayed / went up.
    pub id_regs_fewer: usize,
    pub id_regs_same: usize,
    pub id_regs_more: usize,
    /// Optimized round trip, same three buckets.
    pub opt_ops_fewer: usize,
    pub opt_ops_same: usize,
    pub opt_ops_more: usize,
    pub opt_regs_fewer: usize,
    pub opt_regs_same: usize,
    pub opt_regs_more: usize,
    /// Total opcodes in, and out of the optimized serialization, over the
    /// functions that round-tripped.
    pub ops_in_total: usize,
    pub ops_out_total: usize,
    /// Functions whose *identity* serialization lost opcodes, worst first. A
    /// serializer that silently drops opcodes corrupts every engine at once,
    /// so these are named individually rather than only counted.
    pub id_losses: Vec<IdentityLoss>,
    /// Per-opcode-kind count change summed over the whole module, biggest
    /// loss first. This is the line to read first: `EndTrap` and `JAlways`
    /// losses are the serializer's documented unreachable- and no-op-elision,
    /// and a matched loss/gain pair is one of its normalizations. A bare loss
    /// of anything else is not accounted for.
    pub id_kind_delta: Vec<(String, isize)>,
    /// Per-pass totals summed over every function that got through the
    /// optimize stage, in pipeline order.
    pub passes: Vec<(&'static str, air::v2::PassStats)>,
}

/// One function's identity-round-trip opcode loss.
#[derive(Debug)]
pub struct IdentityLoss {
    pub findex: i32,
    pub name: String,
    pub ops_in: usize,
    pub ops_out: usize,
    pub kinds: Vec<(String, isize)>,
}

impl SweepStats {
    pub fn failed(&self) -> usize {
        self.failures.len()
    }
    /// Failures at one stage.
    pub fn failures_at(&self, stage: Stage) -> usize {
        self.failures.iter().filter(|e| e.stage == stage).count()
    }
}

/// Push one function through every stage, recording counts and the first
/// failure.
///
/// Each stage runs under [`catch_unwind`](std::panic::catch_unwind). AIR v2 has
/// never been run over real bytecode — only over its own tests — so an
/// out-of-bounds index in a pass is a likely outcome, and a sweep that dies on
/// the first one measures nothing. A panic is recorded as a failure of the
/// stage it happened in and the trip stops there.
pub fn trip(m: &AshModule, f: &HLFunction, level: OptLevel, opts: &PassOptions) -> Trip {
    let mut t = Trip {
        findex: f.findex,
        name: f.name(),
        ops_in: f.ops.len(),
        regs_in: f.regs.len(),
        identity: None,
        optimized: None,
        identity_kind_delta: Vec::new(),
        passes: Vec::new(),
        failure: None,
    };
    let mk = |stage: Stage, cause: String, panicked: bool| PipelineError {
        findex: f.findex,
        name: f.name(),
        stage,
        cause,
        panicked,
    };

    // 1. lower
    let lowered = match guard(|| lower_with_positions(&f.ops, &reg_types_of(f), m, positions_of(f)))
    {
        Ok(Ok(ir)) => ir.with_findex(f.findex as usize),
        Ok(Err(e)) => {
            t.failure = Some(mk(Stage::Lower, format!("{e:#}"), false));
            return t;
        }
        Err(p) => {
            t.failure = Some(mk(Stage::Lower, p, true));
            return t;
        }
    };

    // 2. verify what lowering produced, before any pass can be blamed for it
    match guard(|| verify(&lowered)) {
        Ok(Ok(())) => {}
        Ok(Err(e)) => {
            t.failure = Some(mk(Stage::VerifyLowered, format!("{e:#}"), false));
            return t;
        }
        Err(p) => {
            t.failure = Some(mk(Stage::VerifyLowered, p, true));
            return t;
        }
    }

    // 3. identity round trip: serialize with no pass in between. Any opcode
    //    the serializer loses here it lost on its own.
    match guard(|| serialize(&lowered)) {
        Ok(Ok(s)) => {
            t.identity = Some((s.ops.len(), s.num_regs));
            if s.ops.len() < f.ops.len() {
                t.identity_kind_delta = kind_delta(&f.ops, &s.ops);
            }
        }
        Ok(Err(e)) => {
            t.failure = Some(mk(Stage::SerializeIdentity, format!("{e:#}"), false));
            return t;
        }
        Err(p) => {
            t.failure = Some(mk(Stage::SerializeIdentity, p, true));
            return t;
        }
    }

    // 4. optimize
    let mut ir = lowered;
    let pm = PassManager::with_module(level, m).with_options(*opts);
    match guard(|| pm.run(&mut ir)) {
        Ok(Ok(report)) => t.passes = report.per_pass,
        Ok(Err(e)) => {
            t.failure = Some(mk(Stage::Optimize, format!("{e:#}"), false));
            return t;
        }
        Err(p) => {
            t.failure = Some(mk(Stage::Optimize, p, true));
            return t;
        }
    }

    // 5. verify before serializing
    match guard(|| verify(&ir)) {
        Ok(Ok(())) => {}
        Ok(Err(e)) => {
            t.failure = Some(mk(Stage::Verify, format!("{e:#}"), false));
            return t;
        }
        Err(p) => {
            t.failure = Some(mk(Stage::Verify, p, true));
            return t;
        }
    }

    // 6. serialize
    match guard(|| serialize(&ir)) {
        Ok(Ok(s)) => t.optimized = Some((s.ops.len(), s.num_regs)),
        Ok(Err(e)) => t.failure = Some(mk(Stage::Serialize, format!("{e:#}"), false)),
        Err(p) => t.failure = Some(mk(Stage::Serialize, p, true)),
    }
    t
}

/// Sweep every bytecode function in the module.
/// Round-trips every declared function, reachable or not.
///
/// Unlike the behavioural reports this deliberately does *not* restrict itself
/// to [`crate::reachable`]: it asks whether the IR can represent a function at
/// all, and a function this program never calls is one another program will.
/// Narrowing it would hide exactly the lowering bugs it exists to find.
pub fn sweep(bc: &DecodedBytecode, level: OptLevel, opts: &PassOptions) -> SweepStats {
    let m = AshModule::new(bc);
    let mut st = SweepStats {
        total: bc.functions.len(),
        ..Default::default()
    };
    let _silence = SilentPanics::install();

    for f in &bc.functions {
        let t = trip(&m, f, level, opts);

        // Stage counters: a trip that got past stage N passed every stage
        // before it, so count by how far it reached.
        let reached_end = t.failure.as_ref().map(|e| e.stage);
        let passed = |s: Stage| match reached_end {
            None => true,
            Some(failed_at) => s < failed_at,
        };
        if passed(Stage::Lower) {
            st.lowered += 1;
        }
        if passed(Stage::VerifyLowered) {
            st.verified_lowered += 1;
        }
        if passed(Stage::SerializeIdentity) {
            st.serialized_identity += 1;
        }
        if passed(Stage::Optimize) {
            st.optimized += 1;
        }
        if passed(Stage::Verify) {
            st.verified += 1;
        }
        if passed(Stage::Serialize) {
            st.serialized += 1;
        }

        if let Some((ops, regs)) = t.identity {
            bump(
                ops,
                t.ops_in,
                &mut st.id_ops_fewer,
                &mut st.id_ops_same,
                &mut st.id_ops_more,
            );
            bump(
                regs,
                t.regs_in,
                &mut st.id_regs_fewer,
                &mut st.id_regs_same,
                &mut st.id_regs_more,
            );
            if ops < t.ops_in {
                st.id_losses.push(IdentityLoss {
                    findex: t.findex,
                    name: t.name.clone(),
                    ops_in: t.ops_in,
                    ops_out: ops,
                    kinds: t.identity_kind_delta.clone(),
                });
            }
        }
        if let Some((ops, regs)) = t.optimized {
            bump(
                ops,
                t.ops_in,
                &mut st.opt_ops_fewer,
                &mut st.opt_ops_same,
                &mut st.opt_ops_more,
            );
            bump(
                regs,
                t.regs_in,
                &mut st.opt_regs_fewer,
                &mut st.opt_regs_same,
                &mut st.opt_regs_more,
            );
            st.ops_in_total += t.ops_in;
            st.ops_out_total += ops;
        }
        for (name, stats) in t.passes {
            match st.passes.iter_mut().find(|(n, _)| *n == name) {
                Some((_, acc)) => acc.merge(stats),
                None => st.passes.push((name, stats)),
            }
        }
        if let Some(e) = t.failure {
            st.failures.push(e);
        }
    }

    st.failures
        .sort_by(|a, b| a.stage.cmp(&b.stage).then(a.findex.cmp(&b.findex)));
    st.id_losses
        .sort_by_key(|l| std::cmp::Reverse(l.ops_in - l.ops_out));

    let mut totals: HashMap<String, isize> = HashMap::new();
    for l in &st.id_losses {
        for (k, n) in &l.kinds {
            *totals.entry(k.clone()).or_default() += n;
        }
    }
    st.id_kind_delta = totals.into_iter().filter(|(_, n)| *n != 0).collect();
    st.id_kind_delta
        .sort_by(|a, b| b.1.cmp(&a.1).then(a.0.cmp(&b.0)));
    st
}

/// The opcode variant name, e.g. `SetField`. `Opcode` has no discriminant
/// accessor, so take it off the `Debug` form up to the first `{`.
fn opcode_kind(op: &air::opcodes::Opcode) -> String {
    let s = format!("{op:?}");
    s.split(|c: char| !(c.is_alphanumeric() || c == '_'))
        .next()
        .unwrap_or("?")
        .to_string()
}

/// Per-opcode-kind count change, `before - after`, biggest loss first.
///
/// Gains are reported too, and that is the point: the serializer's documented
/// normalizations are *swaps* (`GetThis` -> `Field`, `CallThis` ->
/// `CallMethod`), which look exactly like a loss if only the losing side is
/// shown.
fn kind_delta(
    before: &[air::opcodes::Opcode],
    after: &[air::opcodes::Opcode],
) -> Vec<(String, isize)> {
    let mut counts: HashMap<String, isize> = HashMap::new();
    for op in before {
        *counts.entry(opcode_kind(op)).or_default() += 1;
    }
    for op in after {
        *counts.entry(opcode_kind(op)).or_default() -= 1;
    }
    let mut d: Vec<(String, isize)> = counts.into_iter().filter(|(_, n)| *n != 0).collect();
    d.sort_by(|a, b| b.1.cmp(&a.1).then(a.0.cmp(&b.0)));
    d
}

/// Only the counters a pass actually moved. `PassStats`'s `Debug` prints all
/// nine, which buries the one or two that a given pass ever sets.
fn fmt_stats(s: &air::v2::PassStats) -> String {
    let fields = [
        ("eliminated", s.eliminated),
        ("hoisted", s.hoisted),
        ("fused", s.fused),
        ("replaced", s.replaced),
        ("tail_calls", s.tail_calls),
        ("inlined", s.inlined),
        ("added", s.added),
        ("allocs_removed", s.allocs_removed),
        ("fields_scalarized", s.fields_scalarized),
    ];
    fields
        .iter()
        .filter(|(_, n)| *n != 0)
        .map(|(k, n)| format!("{k}={n}"))
        .collect::<Vec<_>>()
        .join(" ")
}

/// `EndTrap-4 Field+1`, the compact form both report lines use.
fn fmt_delta(d: &[(String, isize)]) -> String {
    d.iter()
        .map(|(k, n)| format!("{k}{n:+}"))
        .collect::<Vec<_>>()
        .join(" ")
}

fn bump(out: usize, inn: usize, fewer: &mut usize, same: &mut usize, more: &mut usize) {
    match out.cmp(&inn) {
        std::cmp::Ordering::Less => *fewer += 1,
        std::cmp::Ordering::Equal => *same += 1,
        std::cmp::Ordering::Greater => *more += 1,
    }
}

/// The sweep's report, one line per element, ending with the summary line.
/// Shaped like [`crate::osr::report`] so both sweeps read the same way.
pub fn report(bc: &DecodedBytecode, level: OptLevel, opts: &PassOptions) -> Vec<String> {
    let st = sweep(bc, level, opts);
    let mut out = Vec::new();

    out.push(format!(
        "level={:?} fma={} inliner={}",
        level,
        opts.fma,
        if level == OptLevel::O3 {
            "on"
        } else {
            "not in pipeline"
        }
    ));
    out.push(format!(
        "stages: lowered={}/{} verified-lowered={} serialize-identity={} optimized={} verified={} serialized={}",
        st.lowered, st.total, st.verified_lowered, st.serialized_identity, st.optimized, st.verified, st.serialized
    ));

    // Failures, grouped by stage, most common reason first inside each group.
    for stage in [
        Stage::Lower,
        Stage::VerifyLowered,
        Stage::SerializeIdentity,
        Stage::Optimize,
        Stage::Verify,
        Stage::Serialize,
    ] {
        let group: Vec<&PipelineError> = st.failures.iter().filter(|e| e.stage == stage).collect();
        if group.is_empty() {
            continue;
        }
        out.push(format!("FAILED at {}: {}", stage, group.len()));

        // Collapse identical reasons: one bad lowering rule shows up thousands
        // of times, and the count is the useful part, not the repetition.
        let mut by_reason: Vec<(String, usize, Vec<i32>)> = Vec::new();
        for e in &group {
            let r = e.brief();
            match by_reason.iter_mut().find(|(k, _, _)| *k == r) {
                Some((_, n, fx)) => {
                    *n += 1;
                    if fx.len() < 5 {
                        fx.push(e.findex);
                    }
                }
                None => by_reason.push((r, 1, vec![e.findex])),
            }
        }
        by_reason.sort_by_key(|(_, n, _)| std::cmp::Reverse(*n));
        for (reason, n, fx) in by_reason {
            out.push(format!("  [{n:>5}x] findex={fx:?}  {reason}"));
        }
    }

    // Round-trip deltas. The identity numbers are the ones that would expose a
    // serializer dropping opcodes; the optimized numbers are expected to shrink.
    out.push(format!(
        "identity round trip: ops fewer={} same={} more={} | regs fewer={} same={} more={}",
        st.id_ops_fewer,
        st.id_ops_same,
        st.id_ops_more,
        st.id_regs_fewer,
        st.id_regs_same,
        st.id_regs_more
    ));
    out.push(format!(
        "optimized round trip: ops fewer={} same={} more={} | regs fewer={} same={} more={}",
        st.opt_ops_fewer,
        st.opt_ops_same,
        st.opt_ops_more,
        st.opt_regs_fewer,
        st.opt_regs_same,
        st.opt_regs_more
    ));
    if st.ops_in_total > 0 {
        out.push(format!(
            "opcodes: {} in -> {} out ({:+.1}%) over {} round-tripped functions",
            st.ops_in_total,
            st.ops_out_total,
            (st.ops_out_total as f64 - st.ops_in_total as f64) / st.ops_in_total as f64 * 100.0,
            st.serialized
        ));
    }
    for (name, s) in &st.passes {
        if s.changed() {
            out.push(format!("pass {name:<16} {}", fmt_stats(s)));
        }
    }
    if !st.id_kind_delta.is_empty() {
        out.push(format!(
            "identity opcode delta over {} shrinking functions: {}",
            st.id_losses.len(),
            fmt_delta(&st.id_kind_delta)
        ));
    }
    for l in st.id_losses.iter().take(15) {
        out.push(format!(
            "  identity lost opcodes: findex={:<6} {:<28} {} -> {}  [{}]",
            l.findex,
            l.name,
            l.ops_in,
            l.ops_out,
            fmt_delta(&l.kinds)
        ));
    }
    if st.id_losses.len() > 15 {
        out.push(format!("  ... and {} more", st.id_losses.len() - 15));
    }

    out.push(format!(
        "{} of {} functions round-tripped",
        st.serialized, st.total
    ));
    out
}

/// Everything the sweep knows about one function, for `ASH_VERIFY_AIR=dump:N`.
/// Deciding whether a failure is the bytecode's fault or v2's needs the actual
/// IR, not a one-line reason.
pub fn dump(bc: &DecodedBytecode, findex: i32, level: OptLevel, opts: &PassOptions) -> Vec<String> {
    let m = AshModule::new(bc);
    let Some(f) = bc.functions.iter().find(|f| f.findex == findex) else {
        return vec![format!("no bytecode function with findex={findex}")];
    };
    let mut out = vec![format!(
        "findex={} {} ops={} regs={}",
        f.findex,
        f.name(),
        f.ops.len(),
        f.regs.len()
    )];
    for (i, op) in f.ops.iter().enumerate() {
        out.push(format!("  in {i:>4}  {op:?}"));
    }

    let t = trip(&m, f, level, opts);
    if let Some(e) = &t.failure {
        out.push(format!("FAILED at {}: {}", e.stage, e.cause));
    }
    match optimize_full(&m, f, level, opts) {
        Ok((s, ir, report)) => {
            out.push(format!("IR after {level:?}:"));
            for line in ir.dump().lines() {
                out.push(format!("  {line}"));
            }
            for (name, stats) in &report.per_pass {
                if stats.changed() {
                    out.push(format!("  pass {name}: {stats:?}"));
                }
            }
            out.push(format!(
                "serialized: ops={} regs={}",
                s.ops.len(),
                s.num_regs
            ));
            for (i, op) in s.ops.iter().enumerate() {
                out.push(format!("  out {i:>4}  {op:?}"));
            }
        }
        Err(e) => out.push(format!("{e}")),
    }
    out
}

/// Parse an opt level out of an environment variable spelling.
pub fn parse_level(s: &str) -> Option<OptLevel> {
    match s.to_ascii_lowercase().as_str() {
        "0" | "o0" => Some(OptLevel::O0),
        "1" | "o1" => Some(OptLevel::O1),
        "2" | "o2" => Some(OptLevel::O2),
        "3" | "o3" => Some(OptLevel::O3),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// panic capture
// ---------------------------------------------------------------------------

thread_local! {
    /// Where the last caught panic came from. `catch_unwind` yields the
    /// payload but not the location, and "index out of bounds" without a file
    /// and line names no bug.
    static PANIC_SITE: RefCell<Option<String>> = const { RefCell::new(None) };
}

/// Run `body`, turning a panic into `Err(message)`.
fn guard<T>(body: impl FnOnce() -> T) -> Result<T, String> {
    PANIC_SITE.with(|s| *s.borrow_mut() = None);
    panic::catch_unwind(AssertUnwindSafe(body)).map_err(|payload| {
        let msg = payload
            .downcast_ref::<&str>()
            .map(|s| s.to_string())
            .or_else(|| payload.downcast_ref::<String>().cloned())
            .unwrap_or_else(|| "<non-string panic payload>".to_string());
        match PANIC_SITE.with(|s| s.borrow_mut().take()) {
            Some(site) => format!("{msg} (at {site})"),
            None => msg,
        }
    })
}

/// Swaps in a panic hook that records the panic site instead of printing it,
/// and restores the previous hook on drop. Without this a sweep over a module
/// where one pass panics on every function prints thousands of backtrace
/// headers and buries the report.
struct SilentPanics(Option<Box<dyn Fn(&panic::PanicHookInfo<'_>) + Sync + Send + 'static>>);

impl SilentPanics {
    fn install() -> Self {
        let prev = panic::take_hook();
        panic::set_hook(Box::new(|info| {
            let site = info
                .location()
                .map(|l| format!("{}:{}", l.file(), l.line()))
                .unwrap_or_default();
            PANIC_SITE.with(|s| *s.borrow_mut() = Some(site));
        }));
        SilentPanics(Some(prev))
    }
}

impl Drop for SilentPanics {
    fn drop(&mut self) {
        if let Some(prev) = self.0.take() {
            panic::set_hook(prev);
        }
    }
}

/// Report, per program, how many loop allocations the escape analysis would
/// hoist and why it refuses the rest.
///
/// A measurement rather than a claim, in the same spirit as ASH_VERIFY_LAYOUT:
/// "allocation hoisting is worth building" should be answerable before the
/// hoisting itself is built.
pub fn escape_report(bc: &crate::bytecode::DecodedBytecode, level: OptLevel) -> Vec<String> {
    use std::collections::HashMap;
    let m = AshModule::new(bc);
    let opts = PassOptions::default();
    let mut totals: HashMap<String, usize> = HashMap::new();
    let mut funcs_with_loop_allocs = 0usize;
    // Only what the program can enter; see `crate::reachable`.
    let reach = crate::reachable::analyze(bc);
    for f in &bc.functions {
        if !reach.is_live(f.findex) {
            continue;
        }
        let Ok((func, _)) = prepare_ir(&m, f, level, &opts) else {
            continue;
        };
        let counts = air::v2::passes::escape::summarize(&func);
        if !counts.is_empty() {
            funcs_with_loop_allocs += 1;
            totals
                .entry(format!(
                    "  ^ findex={} {} :: {:?}",
                    f.findex,
                    f.name(),
                    counts
                ))
                .or_insert(0);
        }
        for (k, v) in counts {
            *totals.entry(k.to_string()).or_insert(0) += v;
        }
    }
    let mut out: Vec<String> = totals.iter().map(|(k, v)| format!("{v:>6}  {k}")).collect();
    out.sort_by(|a, b| b.cmp(a));
    out.push(format!(
        "{funcs_with_loop_allocs} functions contain an allocation inside a loop"
    ));
    out
}

/// Where exception handlers are active, taken from AIR v2 rather than scanned
/// off the opcode array.
///
/// `Block::handler` is derived by dataflow over the CFG, so it answers the
/// question a program-order scan cannot: a region with more than one normal
/// exit — `try { if (c) return a; ... return b; }` — is live or not depending
/// on the path taken, and five of the Heaps sample's functions are shaped that
/// way. This is the supplier an explicit-edge exception lowering should use.
///
/// Returns `(functions with handlers, may-throw sites, sites inside a handler)`.
pub fn trap_report(
    bc: &crate::bytecode::DecodedBytecode,
    level: OptLevel,
) -> (usize, usize, usize) {
    let m = AshModule::new(bc);
    let opts = PassOptions::default();
    let (mut with_handlers, mut sites, mut covered) = (0usize, 0usize, 0usize);
    // Only what the program can enter; see `crate::reachable`.
    let reach = crate::reachable::analyze(bc);
    for f in &bc.functions {
        if !reach.is_live(f.findex) {
            continue;
        }
        let Ok((func, _)) = prepare_ir(&m, f, level, &opts) else {
            continue;
        };
        let mut any = false;
        for block in &func.blocks {
            let handled = block.handler.is_some();
            any |= handled;
            for ins in &block.instrs {
                if ins.may_throw() {
                    sites += 1;
                    if handled {
                        covered += 1;
                    }
                }
            }
        }
        if any {
            with_handlers += 1;
        }
    }
    (with_handlers, sites, covered)
}

/// Per-program OSR eligibility, computed over AIR rather than the opcode array.
pub fn osr_report(bc: &crate::bytecode::DecodedBytecode, level: OptLevel) -> Vec<String> {
    use std::collections::BTreeMap;
    let m = AshModule::new(bc);
    let opts = PassOptions::default();
    let mut eligible = 0usize;
    let mut reasons: BTreeMap<String, usize> = BTreeMap::new();
    let mut considered = 0usize;
    // Restricted to what the program can actually enter. A .hl links the whole
    // stdlib, so counting every declared function put ~93% of the denominator
    // in code that never runs and made the eligibility rate meaningless.
    let reach = crate::reachable::analyze(bc);
    let mut skipped = 0usize;
    for f in &bc.functions {
        if !reach.is_live(f.findex) {
            skipped += 1;
            continue;
        }
        let Ok((func, _)) = prepare_ir(&m, f, level, &opts) else {
            continue;
        };
        considered += 1;
        let plan = crate::osr::analyze(&func);
        if plan.eligible() {
            eligible += 1;
            continue;
        }
        for r in &plan.refusals {
            // A function with no loop is the common case and says nothing.
            if matches!(r, crate::osr::OsrRefusal::NoBackEdge) {
                continue;
            }
            *reasons.entry(format!("{r:?}")).or_insert(0) += 1;
        }
    }
    let mut out: Vec<String> = reasons
        .iter()
        .map(|(k, v)| format!("{v:>6}  refused: {k}"))
        .collect();
    out.push(format!(
        "{eligible} of {considered} reachable functions eligible for OSR \
         ({skipped} unreachable skipped)"
    ));
    out
}
