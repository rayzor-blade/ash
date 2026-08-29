use anyhow::{anyhow, Context as _, Result};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::ffi::c_void;
use std::mem::ManuallyDrop;
use std::path::Path;
use std::sync::{Arc, Condvar, Mutex, OnceLock};

use beadie::{HotnessPolicy, OsrEntry, ThresholdPolicy, TieredAdapter};

use ash_core::bytecode::DecodedBytecode;
use ash_core::c_types::CTypeFactory;
use ash_core::hl_bindings::{self as hl, _vclosure, hl_runtime_obj, hl_type, hl_type_kind_HSTRUCT};
use ash_core::llvm::module::{JITModule, SharedRuntimeHandles};
use ash_core::native_lib::NativeFunctionResolver;
use ash_core::opcodes::{Opcode, Reg};
use ash_core::types::{HLFunction, ValueTypeKind};
use inkwell::context::Context;

use crate::air::Cache as AirCache;
use crate::frame::InterpreterFrame;
use crate::ssa::Cache as SsaCache;
use crate::values::{CmpOp, FloatBinOp, IntBinOp, NanBoxedValue};

/// Function pointer types for stdlib functions resolved at runtime.
type FnAllocObj = unsafe extern "C" fn(*mut c_void) -> *mut c_void;
type FnAllocDynObj = unsafe extern "C" fn() -> *mut c_void;
type FnAllocVirtual = unsafe extern "C" fn(*mut c_void) -> *mut c_void;
type FnGetObjRt = unsafe extern "C" fn(*mut c_void) -> *mut c_void;
type FnAllocClosureVoid = unsafe extern "C" fn(*mut c_void, *mut c_void) -> *mut c_void;
type FnDynGetD = unsafe extern "C" fn(*mut c_void, i32) -> f64;
type FnDynGetF = unsafe extern "C" fn(*mut c_void, i32) -> f32;
type FnDynGetI64 = unsafe extern "C" fn(*mut c_void, i32) -> i64;
type FnDynGetI = unsafe extern "C" fn(*mut c_void, i32, *mut c_void) -> i32;
type FnDynGetP = unsafe extern "C" fn(*mut c_void, i32, *mut c_void) -> *mut c_void;
type FnDynSetD = unsafe extern "C" fn(*mut c_void, i32, f64);
type FnDynSetF = unsafe extern "C" fn(*mut c_void, i32, f32);
type FnDynSetI64 = unsafe extern "C" fn(*mut c_void, i32, i64);
type FnDynSetI = unsafe extern "C" fn(*mut c_void, i32, *mut c_void, i32);
type FnDynSetP = unsafe extern "C" fn(*mut c_void, i32, *mut c_void, *mut c_void);
type FnHashGen = unsafe extern "C" fn(*const u16, bool) -> i32;
type FnObjGetField = unsafe extern "C" fn(*mut hl::vdynamic, i32) -> *mut hl::vdynamic;
type FnValueToString = unsafe extern "C" fn(*mut hl::vdynamic, *mut i32) -> *const hl::vbyte;
type FnTypeName = unsafe extern "C" fn(*const hl::hl_type) -> *mut hl::vbyte;

/// Resolve/calculate a HashLink field hash from a bytecode string index.
/// Uses std's hlp_hash_gen when available so field names are cached for reflection/JSON.
fn hash_field_name(
    bytecode: &DecodedBytecode,
    str_idx: usize,
    fn_hash_gen: *mut c_void,
    utf16_cache: &mut HashMap<usize, Vec<u16>>,
    hash_cache: &mut HashMap<usize, i32>,
) -> Result<i32> {
    if let Some(&h) = hash_cache.get(&str_idx) {
        return Ok(h);
    }
    let utf16 = if let Some(cached) = utf16_cache.get(&str_idx) {
        cached.as_ptr()
    } else {
        let s = bytecode
            .strings
            .get(str_idx)
            .ok_or_else(|| anyhow!("Dyn field string out of bounds: {}", str_idx))?;
        let mut buf: Vec<u16> = s.encode_utf16().collect();
        buf.push(0);
        utf16_cache.insert(str_idx, buf);
        utf16_cache[&str_idx].as_ptr()
    };
    let h = if !fn_hash_gen.is_null() {
        let f: FnHashGen = unsafe { std::mem::transmute(fn_hash_gen) };
        unsafe { f(utf16, true) }
    } else {
        let slice = unsafe {
            let mut len = 0;
            while *utf16.add(len) != 0 {
                len += 1;
            }
            std::slice::from_raw_parts(utf16, len)
        };
        let mut h: i32 = 0;
        for c in slice {
            h = h.wrapping_mul(223).wrapping_add(*c as i32);
        }
        h.wrapping_rem(0x1FFFFF7B)
    };
    hash_cache.insert(str_idx, h);
    Ok(h)
}

type FnTrapSetup = unsafe extern "C" fn() -> *mut c_void;
type FnTrapRemove = unsafe extern "C" fn();
type FnTrapCallback = unsafe extern "C" fn(*mut c_void);

extern "C" {
    fn ash_interp_run_with_hl_trap(
        setup: Option<FnTrapSetup>,
        remove: Option<FnTrapRemove>,
        callback: Option<FnTrapCallback>,
        context: *mut c_void,
    ) -> i32;
}

struct TrapCallbackContext<F> {
    callback: *mut F,
}

unsafe extern "C" fn invoke_trap_callback<F>(context: *mut c_void)
where
    F: FnMut(),
{
    let context = &mut *context.cast::<TrapCallbackContext<F>>();
    (&mut *context.callback)();
}

/// Invoke a callback while the C frame containing setjmp remains active.
///
/// HashLink exceptions longjmp out of generated code and native libraries.
/// Keeping setjmp in this boundary is essential: a Rust helper that merely
/// returns its setjmp result leaves the runtime holding a dead stack frame.
fn run_with_hl_trap<F>(setup: *mut c_void, remove: *mut c_void, mut callback: F) -> i32
where
    F: FnMut(),
{
    let setup = if setup.is_null() {
        None
    } else {
        Some(unsafe { std::mem::transmute::<*mut c_void, FnTrapSetup>(setup) })
    };
    let remove = if remove.is_null() {
        None
    } else {
        Some(unsafe { std::mem::transmute::<*mut c_void, FnTrapRemove>(remove) })
    };
    let mut context = TrapCallbackContext {
        callback: &mut callback,
    };
    unsafe {
        ash_interp_run_with_hl_trap(
            setup,
            remove,
            Some(invoke_trap_callback::<F>),
            (&mut context as *mut TrapCallbackContext<F>).cast(),
        )
    }
}

/// Result of executing a single opcode.
enum StepResult {
    /// Continue to next opcode (pc already incremented)
    Continue,
    /// Jump to a relative offset from current pc
    Jump(i32),
    /// Jump to an absolute opcode index (used for exception handler entry)
    JumpAbs(usize),
    /// Return a value from the current function
    Return(NanBoxedValue),
    /// Call a function by findex, with arguments and destination register
    Call {
        findex: usize,
        args: Vec<NanBoxedValue>,
        dst: u32,
    },
}

/// Primitive payloads that can live either boxed behind `vdynamic*` or
/// directly in an interpreter Dynamic register.
#[derive(Clone, Copy)]
enum DynamicScalar {
    Int(i64),
    Float(f64),
    Bool(bool),
}

/// What a findex resolves to, held in a dense table indexed by findex.
///
/// Functions and natives share one numbering, so one indexed load decides
/// both questions the call path used to ask two hash maps.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum CallTarget {
    /// No function or native carries this findex.
    Missing,
    /// Index into `bytecode.functions`.
    Func(u32),
    /// Index into `bytecode.natives`.
    Native(u32),
}

/// How many recycled buffers either pool keeps.
///
/// Pops and pushes are not balanced: the `Call0..CallN` arms take a buffer from
/// the pool, but `op_call_method` and `op_call_closure` build their own and the
/// trampoline reclaims those too, so a dispatch-heavy program returns more than
/// it borrows. Measured RSS does not move without this cap, because a real
/// program mixes the two, but nothing in the design bounds it -- the cap does,
/// and a buffer arriving at a full pool is simply dropped.
///
/// 64 is far past any sane call depth; the pool only has to cover the calls in
/// flight, not the calls ever made.
const POOL_CAP: usize = 64;

/// Back-edges one frame takes before its loop counts as hot.
///
/// Read in two places that must agree: the back-edge probe, which tests it as
/// the mask `& (HOT_LOOP_BACKEDGES - 1)` and so fires on every multiple, and
/// the demand test at a call, which asks whether the calling frame is already
/// past the first one. The mask makes a power of two a precondition of the two
/// readings agreeing, not just a nice round number.
const HOT_LOOP_BACKEDGES: u32 = 64;
const _: () = assert!(HOT_LOOP_BACKEDGES.is_power_of_two());

/// AIR V2 work between cooperative fiber scheduling turns. This is high
/// enough to stay below profiler noise on compute-only programs, while an
/// interpreted game loop still gives blocked worker fibers several turns per
/// frame. SSA dispatch charges a whole block at once; serialized AIR charges
/// one optimized opcode.
const FIBER_POLL_WORK: u32 = 16 * 1024;

/// Demand bits in `HLInterpreter::demand_local`.
const DEMAND_LIVE_FRAME: u8 = 1;
const DEMAND_UNDER_LOOP: u8 = 2;

/// Index into `bytecode.functions` for a findex, if it names one.
///
/// Free functions rather than methods on purpose: `func_of(&self.targets, ..)` would
/// borrow all of `self`, and the call path holds a `&mut` frame from
/// `self.stack` across the lookup. Taking the table alone keeps the borrow
/// field-disjoint, which is what the two HashMap fields gave for free.
#[inline(always)]
fn func_of(targets: &[CallTarget], findex: usize) -> Option<usize> {
    match targets.get(findex) {
        Some(CallTarget::Func(i)) => Some(*i as usize),
        _ => None,
    }
}

/// Index into `bytecode.natives` for a findex, if it names one.
#[inline(always)]
fn native_of(targets: &[CallTarget], findex: usize) -> Option<usize> {
    match targets.get(findex) {
        Some(CallTarget::Native(i)) => Some(*i as usize),
        _ => None,
    }
}

/// findex -> "Class.method", shared by every trace taken from one image.
type NameTable = Rc<HashMap<usize, String>>;
/// The image the cached table was built for, and the table.
type NameTableCache = RefCell<Option<(usize, NameTable)>>;

/// Carries a thrown HL exception value up through the Rust call stack.
/// Distinguishable from other errors so callers can catch it via downcast.
#[derive(Debug, Clone)]
struct HLExceptionPropagation {
    value: NanBoxedValue,
    message: Option<String>,
    /// Innermost-first call stack, captured where the exception was raised.
    ///
    /// Captured at the throw rather than rendered at the top: by the time an
    /// uncaught exception reaches the CLI the frames it came from have been
    /// popped, and "Uncaught exception: Null access" with nothing else is the
    /// report HashLink users are least able to act on.
    stack: Vec<String>,
}

impl std::fmt::Display for HLExceptionPropagation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.message {
            Some(msg) => write!(f, "Uncaught exception: {msg}")?,
            None => write!(f, "Uncaught exception: {:?}", self.value)?,
        }
        for frame in &self.stack {
            write!(f, "\nCalled from {frame}")?;
        }
        Ok(())
    }
}

impl std::error::Error for HLExceptionPropagation {}

mod ops;
mod ssa;

use crate::tiering::env_flag;
pub use crate::tiering::{TierMode, TierPreset, TieredConfig, TieredStats};
use crate::tiering::*;

struct HlpName<'a>(&'a str);
impl std::fmt::Display for HlpName<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "hlp_{}", self.0)
    }
}

pub struct HLInterpreter {
    /// Global variable store (indexed by global index)
    pub globals: Vec<NanBoxedValue>,
    /// Call stack
    stack: Vec<InterpreterFrame>,
    /// Suspended logical call stacks, keyed by krio fiber id. AIR v2 frames
    /// must follow the fiber that owns their register files rather than the
    /// single OS thread all cooperative Haxe threads share.
    fiber_stacks: HashMap<u32, Vec<InterpreterFrame>>,
    /// Generated-code callers currently suspended inside the lazy JIT stub
    /// bridge. Native unwinding stops at that Rust boundary, so retain the
    /// logical AIR V2 call chain explicitly for HashLink stack APIs.
    jit_bridge_callers: Vec<usize>,
    /// Maximum call stack depth
    max_stack_depth: usize,
    /// Findexes whose tier-1 compile this interpreter has force-proposed at
    /// an OSR transfer, so the proposal is made once per findex rather than
    /// on every future frame that enters an entry.
    osr_forced: std::collections::HashSet<usize>,
    /// Demand already published for a findex, bit 0 live-frame and bit 1
    /// reached-from-a-looping-frame.
    ///
    /// Indexed by findex rather than hashed: both facts are re-checked on
    /// every interpreted call so the shared set is written exactly once, and
    /// at that rate a hash lookup is itself the cost being avoided.
    demand_local: Vec<u8>,
    /// OSR entries currently attached per findex — the main thread's mirror
    /// of each bead's table. `swap_compiled_with_osr` REPLACES the table, so
    /// an incremental attach must resend the entries already installed; this
    /// is where they are remembered.
    osr_attached: std::collections::HashMap<usize, Vec<OsrEntry>>,
    /// Loop headers seen to be hot, as `(findex, header_pc)`.
    hot_loops: std::collections::HashSet<(usize, usize)>,
    /// Compiled-only functions whose AIR V2 closure dependencies have been
    /// installed. A closure can escape immediately into a native (sorting is
    /// the canonical case), where a stub sentinel is not a callable address;
    /// this set also breaks recursive closure-dependency cycles.
    compiled_only_deps_ready: std::collections::HashSet<usize>,

    /// Argument buffers, recycled the same way as [`Self::reg_pool`].
    ///
    /// `StepResult::Call` owns its arguments, so every call above arity zero
    /// used to malloc a `Vec` for them and free it on return. The trampoline
    /// hands the buffer back once the callee has copied the arguments into its
    /// own registers, which is the only span they need to survive.
    arg_pool: Vec<Vec<NanBoxedValue>>,

    /// Register buffers from finished frames, ready for the next call.
    ///
    /// Calls nest, so buffers come back in the order they were handed out and
    /// the depth of this stack is the depth of the call stack -- it cannot run
    /// away. Frames discarded by an exception unwind are reclaimed too; missing
    /// them would only cost a malloc, but the unwind path is where a long-lived
    /// program would otherwise leak its whole call depth of buffers.
    reg_pool: Vec<Vec<NanBoxedValue>>,

    /// findex → what to run, as a dense table.
    ///
    /// This was two `HashMap<usize, usize>`, consulted on every call: the
    /// interpreter paid a hash and a probe to answer a question about a dense
    /// integer. findexes number the functions and natives of one module
    /// consecutively, so an indexed load answers it instead. See
    /// [`CallTarget`].
    targets: Vec<CallTarget>,
    /// Compiled code address → findex, for closures built by compiled code.
    ///
    /// The JIT lowers `StaticClosure`/`InstanceClosure` by loading
    /// `functions_ptrs[findex]` at run time, so a closure a compiled function
    /// allocates carries whatever that slot held — the real entry address once
    /// the callee is promoted, not the `findex + 1` stub sentinel the
    /// interpreter stores. Handed such a closure, the interpreter has to walk
    /// back from the address to the findex, and `functions_ptrs` is the table
    /// the JIT read it from.
    ///
    /// Filled on miss by scanning that table, and never invalidated: a cached
    /// entry stays true because an address only ever belonged to one findex.
    ///
    /// The TABLE, though, overwrites rather than accumulates -- a tier-1
    /// install replaces the tier-0 address in the slot -- so an address that
    /// was never scanned before it was replaced cannot be recovered from it.
    /// Every slot is therefore indexed on the first miss, which captures the
    /// addresses installed so far, and re-promotion adds the new address on
    /// the next miss while the old one remains cached from before.
    code_addr_findex: HashMap<usize, usize>,
    /// Hot-reloaded bytecode (replaces the original for function lookup).
    /// Leaked to 'static so it can be passed to interpret_loop without borrow conflicts.
    reloaded_bytecode: Option<&'static ash_core::bytecode::DecodedBytecode>,
    /// AIR v2 optimized bodies, filled on first execution of each function.
    /// Inert unless `ASH_AIR=v2-serialize`; see `crate::air`.
    air: AirCache,
    /// AIR v2 SSA bodies executed directly, filled on first execution of each
    /// function. Inert unless `ASH_AIR=v2`; see `crate::ssa`.
    ssa: SsaCache,
    /// Per-native resolved function pointer cache (indexed by native array
    /// index). Backed by the process-global symbol table on first miss;
    /// kills the per-call format!/table-lock on the native hot path.
    native_fn_cache: Vec<*mut c_void>,
    /// C-level type structures for native function interop
    c_type_factory: CTypeFactory,
    /// Resolved stdlib function pointer: hlp_alloc_obj
    fn_alloc_obj: *mut c_void,
    /// Resolved stdlib function pointer: hlp_get_obj_rt
    fn_get_obj_rt: *mut c_void,
    /// Resolved stdlib function pointer: hlp_make_dyn
    fn_make_dyn: *mut c_void,
    /// Resolved stdlib function pointer: hlp_alloc_array
    fn_alloc_array: *mut c_void,
    /// HashLink's sentinel body for closures created by hlp_make_var_args.
    fn_fun_var_args: *mut c_void,
    /// Resolved stdlib function pointer: hlp_alloc_enum
    fn_alloc_enum: *mut c_void,
    /// Resolved stdlib function pointer: hlp_alloc_dynobj
    fn_alloc_dynobj: *mut c_void,
    /// Resolved stdlib function pointer: hlp_alloc_virtual
    fn_alloc_virtual: *mut c_void,
    /// Resolve an object to HashLink's canonical `vvirtual` representation.
    fn_to_virtual: *mut c_void,
    /// Boxed virtual dispatch for implementation/interface signature mismatches.
    fn_vcall_dyn: *mut c_void,
    /// Resolved stdlib function pointer: hlp_alloc_closure_void
    fn_alloc_closure_void: *mut c_void,
    /// Resolved stdlib function pointer: hlp_alloc_closure_ptr (bound closures)
    fn_alloc_closure_ptr: *mut c_void,
    /// Resolved stdlib function pointer: hlp_dyn_getd
    fn_dyn_getd: *mut c_void,
    /// Resolved stdlib function pointer: hlp_dyn_getf
    fn_dyn_getf: *mut c_void,
    /// Resolved stdlib function pointer: hlp_dyn_geti64
    fn_dyn_geti64: *mut c_void,
    /// Resolved stdlib function pointer: hlp_dyn_geti
    fn_dyn_geti: *mut c_void,
    /// Resolved stdlib function pointer: hlp_dyn_getp
    fn_dyn_getp: *mut c_void,
    /// Resolved stdlib function pointer: hlp_dyn_castp (for SafeCast)
    fn_dyn_castp: *mut c_void,
    /// Resolved stdlib function pointer: hlp_dyn_setd
    fn_dyn_setd: *mut c_void,
    /// Resolved stdlib function pointer: hlp_dyn_setf
    fn_dyn_setf: *mut c_void,
    /// Resolved stdlib function pointer: hlp_dyn_seti64
    fn_dyn_seti64: *mut c_void,
    /// Resolved stdlib function pointer: hlp_dyn_seti
    fn_dyn_seti: *mut c_void,
    /// Resolved stdlib function pointer: hlp_dyn_setp
    fn_dyn_setp: *mut c_void,
    /// Resolved stdlib function pointer: hlp_hash_gen
    fn_hash_gen: *mut c_void,
    /// Resolved stdlib function pointer: hlp_setup_trap_jit (setjmp trap for native throws)
    fn_setup_trap_jit: *mut c_void,
    /// Resolved stdlib function pointer: hlp_remove_trap_jit (pop native trap after success)
    fn_remove_trap_jit: *mut c_void,
    /// Resolved stdlib function pointer: hlp_get_exc_value (interpreter exception recovery)
    fn_get_exc_value: *mut c_void,
    /// Resolved stdlib function pointer: hlp_clear_exc_value (clears exc_value after recovery)
    fn_clear_exc_value: *mut c_void,
    /// Resolved stdlib function pointer: hlp_obj_get_field (reads dynamic object fields)
    fn_obj_get_field: *mut c_void,
    /// Resolved stdlib function pointer: hlp_value_to_string (for readable exception messages)
    fn_value_to_string: *mut c_void,
    /// Resolved stdlib function pointer: hlp_type_name (runtime type name lookup)
    fn_type_name: *mut c_void,
    /// Resolved stdlib function pointer: hlp_gc_clear_scan_roots
    fn_gc_clear_scan_roots: *mut c_void,
    /// Resolved stdlib function pointer: hlp_gc_add_scan_root
    fn_gc_add_scan_root: *mut c_void,
    /// Batched replacement for clear+add-per-frame+done. See
    /// `sync_gc_scan_roots`.
    fn_gc_set_scan_roots: *mut c_void,
    /// Reused across publishes so building the range list allocates once.
    scan_range_buf: Vec<(usize, usize)>,
    /// c_type pointers for the primitive kinds, resolved once, used to BOX a
    /// value entering a Dynamic slot. std's `hlt_i32()` and friends are plain
    /// Rust fns rather than `#[no_mangle]` exports, so the native resolver
    /// cannot find them; the bytecode's own type table always carries these
    /// kinds instead. Plain fields rather than a map so they can be copied
    /// out before a `frame` borrow is taken.
    prim_t_i32: *mut c_void,
    prim_t_i64: *mut c_void,
    prim_t_f64: *mut c_void,
    prim_t_bool: *mut c_void,
    prim_t_bytes: *mut c_void,
    prim_t_dyn: *mut c_void,
    /// Resolved stdlib function pointer: hlp_gc_scan_roots_done
    fn_gc_scan_roots_done: *mut c_void,
    /// Resolved stdlib function pointer: hlp_gc_set_stack_top
    fn_gc_set_stack_top: *mut c_void,
    /// Resolved stdlib function pointer: hlp_gc_set_globals
    fn_gc_set_globals: *mut c_void,
    /// Cooperative fiber scheduler safe point. Long-running Haxe event loops
    /// do not necessarily call a blocking primitive, so their worker fibers
    /// need bounded turns from the AIR V2 dispatcher itself.
    fn_fiber_poll: *mut c_void,
    /// AIR V2 work units remaining before the next fiber scheduling turn.
    fiber_poll_budget: u32,
    /// Whether GC globals/stack top were initialized for this interpreter.
    gc_runtime_initialized: bool,
    /// Scratch space for decoded raw pointer roots (from NaN-boxed registers).
    /// Cache of UTF-16 null-terminated strings (string index → owned buffer).
    /// HashLink uses UTF-16 internally; bytecode strings are stored as UTF-8 in Rust.
    utf16_strings: HashMap<usize, Vec<u16>>,
    /// Cache of field name hashes (string index → hash value).
    field_hash_cache: HashMap<usize, i32>,
    /// Fallback storage for HVIRTUAL fields when runtime virtual indexes are unavailable.
    virtual_fields: HashMap<(usize, usize), NanBoxedValue>,
    /// VM-lifetime storage backing every opaque `hl_symbol` token handed to Haxe.
    /// Exception objects resolve these lazily, long after a newer exception may
    /// have replaced the current stack snapshot.
    stack_symbol_arena: Vec<Box<[u16]>>,
    /// Opaque `hl_symbol` tokens backing the most recent call-stack query.
    call_stack_symbols: Vec<usize>,
    /// Stack captured at the most recent non-rethrow exception origin.
    exception_stack_symbols: Vec<usize>,
    /// Optional tiered runtime (hybrid mode).
    tiered_runtime: Option<TieredRuntime>,
}

impl Drop for HLInterpreter {
    fn drop(&mut self) {
        // Beadie's broker drains its whole queue on Drop (Shutdown is FIFO
        // behind any queued compile jobs). Invalidate every bead first so
        // still-queued jobs fail the mark_compiling gate and are skipped —
        // no LLVM compiles for a process that is exiting.
        if let Some(tiered) = self.tiered_runtime.as_mut() {
            for bound in tiered.beads.iter().flatten() {
                bound.bead().invalidate();
            }
        }
    }
}

impl HLInterpreter {
    pub fn new(bytecode: &DecodedBytecode, native_resolver: &NativeFunctionResolver) -> Self {
        // Build the dense findex table. Sized to the largest findex actually
        // seen rather than to functions.len() + natives.len(): the two share
        // one numbering, and nothing guarantees it has no gaps.
        let max_findex = bytecode
            .functions
            .iter()
            .map(|f| f.findex)
            .chain(bytecode.natives.iter().map(|n| n.findex))
            .max()
            .unwrap_or(-1);
        let mut targets = vec![CallTarget::Missing; (max_findex + 1).max(0) as usize];
        for (i, f) in bytecode.functions.iter().enumerate() {
            targets[f.findex as usize] = CallTarget::Func(i as u32);
        }
        for (i, n) in bytecode.natives.iter().enumerate() {
            targets[n.findex as usize] = CallTarget::Native(i as u32);
        }

        // Initialize globals
        let globals = vec![NanBoxedValue::null(); bytecode.globals.len()];

        // Create C-level type structures for native interop
        let c_type_factory = CTypeFactory::new(bytecode);
        // Resolved once: see `box_for_dynamic_slot`.
        let mut c_type_factory = c_type_factory;
        let find_prim = |f: &mut CTypeFactory, k: hl::hl_type_kind| -> usize {
            match bytecode.types.iter().position(|t| t.kind == k) {
                Some(i) => f.get(i) as usize,
                None => 0,
            }
        };
        let prim_t_i32 = find_prim(&mut c_type_factory, hl::hl_type_kind_HI32);
        let prim_t_i64 = find_prim(&mut c_type_factory, hl::hl_type_kind_HI64);
        let prim_t_f64 = find_prim(&mut c_type_factory, hl::hl_type_kind_HF64);
        let prim_t_bool = find_prim(&mut c_type_factory, hl::hl_type_kind_HBOOL);
        let prim_t_bytes = find_prim(&mut c_type_factory, hl::hl_type_kind_HBYTES);
        let prim_t_dyn = find_prim(&mut c_type_factory, hl::hl_type_kind_HDYN);

        // Resolve internal stdlib function pointers for object operations
        let fn_alloc_obj = native_resolver
            .resolve_function("std", "hlp_alloc_obj")
            .unwrap_or(std::ptr::null_mut());
        let fn_get_obj_rt = native_resolver
            .resolve_function("std", "hlp_get_obj_rt")
            .unwrap_or(std::ptr::null_mut());
        let fn_make_dyn = native_resolver
            .resolve_function("std", "hlp_make_dyn")
            .unwrap_or(std::ptr::null_mut());
        let fn_alloc_array = native_resolver
            .resolve_function("std", "hlp_alloc_array")
            .unwrap_or(std::ptr::null_mut());
        let fn_fun_var_args = native_resolver
            .resolve_function("std", "_fun_var_args")
            .unwrap_or(std::ptr::null_mut());
        let fn_alloc_enum = native_resolver
            .resolve_function("std", "hlp_alloc_enum")
            .unwrap_or(std::ptr::null_mut());
        let fn_alloc_dynobj = native_resolver
            .resolve_function("std", "hlp_alloc_dynobj")
            .unwrap_or(std::ptr::null_mut());
        let fn_alloc_virtual = native_resolver
            .resolve_function("std", "hlp_alloc_virtual")
            .unwrap_or(std::ptr::null_mut());
        let fn_to_virtual = native_resolver
            .resolve_function("std", "hl_to_virtual")
            .unwrap_or(std::ptr::null_mut());
        let fn_vcall_dyn = native_resolver
            .resolve_function("std", "hlp_vcall_dyn")
            .unwrap_or(std::ptr::null_mut());
        let fn_alloc_closure_void = native_resolver
            .resolve_function("std", "hlp_alloc_closure_void")
            .unwrap_or(std::ptr::null_mut());
        let fn_alloc_closure_ptr = native_resolver
            .resolve_function("std", "hlp_alloc_closure_ptr")
            .unwrap_or(std::ptr::null_mut());
        let fn_dyn_getd = native_resolver
            .resolve_function("std", "hlp_dyn_getd")
            .unwrap_or(std::ptr::null_mut());
        let fn_dyn_getf = native_resolver
            .resolve_function("std", "hlp_dyn_getf")
            .unwrap_or(std::ptr::null_mut());
        let fn_dyn_geti64 = native_resolver
            .resolve_function("std", "hlp_dyn_geti64")
            .unwrap_or(std::ptr::null_mut());
        let fn_dyn_geti = native_resolver
            .resolve_function("std", "hlp_dyn_geti")
            .unwrap_or(std::ptr::null_mut());
        let fn_dyn_getp = native_resolver
            .resolve_function("std", "hlp_dyn_getp")
            .unwrap_or(std::ptr::null_mut());
        let fn_dyn_castp = native_resolver
            .resolve_function("std", "hlp_dyn_castp")
            .unwrap_or(std::ptr::null_mut());
        let fn_dyn_setd = native_resolver
            .resolve_function("std", "hlp_dyn_setd")
            .unwrap_or(std::ptr::null_mut());
        let fn_dyn_setf = native_resolver
            .resolve_function("std", "hlp_dyn_setf")
            .unwrap_or(std::ptr::null_mut());
        let fn_dyn_seti64 = native_resolver
            .resolve_function("std", "hlp_dyn_seti64")
            .unwrap_or(std::ptr::null_mut());
        let fn_dyn_seti = native_resolver
            .resolve_function("std", "hlp_dyn_seti")
            .unwrap_or(std::ptr::null_mut());
        let fn_dyn_setp = native_resolver
            .resolve_function("std", "hlp_dyn_setp")
            .unwrap_or(std::ptr::null_mut());
        let fn_hash_gen = native_resolver
            .resolve_function("std", "hlp_hash_gen")
            .unwrap_or(std::ptr::null_mut());
        let fn_setup_trap_jit = native_resolver
            .resolve_function("std", "hlp_setup_trap_jit")
            .unwrap_or(std::ptr::null_mut());
        let fn_remove_trap_jit = native_resolver
            .resolve_function("std", "hlp_remove_trap_jit")
            .unwrap_or(std::ptr::null_mut());
        let fn_get_exc_value = native_resolver
            .resolve_function("std", "hlp_get_exc_value")
            .unwrap_or(std::ptr::null_mut());
        let fn_clear_exc_value = native_resolver
            .resolve_function("std", "hlp_clear_exc_value")
            .unwrap_or(std::ptr::null_mut());
        let fn_obj_get_field = native_resolver
            .resolve_function("std", "hlp_obj_get_field")
            .unwrap_or(std::ptr::null_mut());
        let fn_value_to_string = native_resolver
            .resolve_function("std", "hlp_value_to_string")
            .unwrap_or(std::ptr::null_mut());
        let fn_type_name = native_resolver
            .resolve_function("std", "hlp_type_name")
            .unwrap_or(std::ptr::null_mut());
        let fn_gc_clear_scan_roots = native_resolver
            .resolve_function("std", "hlp_gc_clear_scan_roots")
            .unwrap_or(std::ptr::null_mut());
        let fn_gc_scan_roots_done = native_resolver
            .resolve_function("std", "hlp_gc_scan_roots_done")
            .unwrap_or(std::ptr::null_mut());
        let fn_gc_add_scan_root = native_resolver
            .resolve_function("std", "hlp_gc_add_scan_root")
            .unwrap_or(std::ptr::null_mut());
        let fn_gc_set_scan_roots = native_resolver
            .resolve_function("std", "hlp_gc_set_scan_roots")
            .unwrap_or(std::ptr::null_mut());
        let fn_gc_set_stack_top = native_resolver
            .resolve_function("std", "hlp_gc_set_stack_top")
            .unwrap_or(std::ptr::null_mut());
        let fn_gc_set_globals = native_resolver
            .resolve_function("std", "hlp_gc_set_globals")
            .unwrap_or(std::ptr::null_mut());
        let fn_fiber_poll = native_resolver
            .resolve_function("std", "hlp_fiber_poll")
            .unwrap_or(std::ptr::null_mut());
        HLInterpreter {
            globals,
            stack: Vec::with_capacity(64),
            fiber_stacks: HashMap::new(),
            jit_bridge_callers: Vec::new(),
            max_stack_depth: 1000,
            osr_forced: std::collections::HashSet::new(),
            demand_local: Vec::new(),
            targets,
            code_addr_findex: HashMap::new(),
            reg_pool: Vec::new(),
            arg_pool: Vec::new(),
            osr_attached: std::collections::HashMap::new(),
            hot_loops: std::collections::HashSet::new(),
            compiled_only_deps_ready: std::collections::HashSet::new(),
            reloaded_bytecode: None,
            air: AirCache::default(),
            ssa: SsaCache::default(),
            native_fn_cache: vec![std::ptr::null_mut(); bytecode.natives.len()],
            c_type_factory,
            fn_alloc_obj,
            fn_get_obj_rt,
            fn_make_dyn,
            fn_alloc_array,
            fn_fun_var_args,
            fn_alloc_enum,
            fn_alloc_dynobj,
            fn_alloc_virtual,
            fn_to_virtual,
            fn_vcall_dyn,
            fn_alloc_closure_void,
            fn_alloc_closure_ptr,
            fn_dyn_getd,
            fn_dyn_getf,
            fn_dyn_geti64,
            fn_dyn_geti,
            fn_dyn_getp,
            fn_dyn_castp,
            fn_dyn_setd,
            fn_dyn_setf,
            fn_dyn_seti64,
            fn_dyn_seti,
            fn_dyn_setp,
            fn_hash_gen,
            fn_setup_trap_jit,
            fn_remove_trap_jit,
            fn_get_exc_value,
            fn_clear_exc_value,
            fn_obj_get_field,
            fn_value_to_string,
            fn_type_name,
            fn_gc_clear_scan_roots,
            fn_gc_add_scan_root,
            fn_gc_set_scan_roots,
            scan_range_buf: Vec::new(),
            prim_t_i32: prim_t_i32 as *mut c_void,
            prim_t_i64: prim_t_i64 as *mut c_void,
            prim_t_f64: prim_t_f64 as *mut c_void,
            prim_t_bool: prim_t_bool as *mut c_void,
            prim_t_bytes: prim_t_bytes as *mut c_void,
            prim_t_dyn: prim_t_dyn as *mut c_void,
            fn_gc_scan_roots_done,
            fn_gc_set_stack_top,
            fn_gc_set_globals,
            fn_fiber_poll,
            fiber_poll_budget: FIBER_POLL_WORK,
            gc_runtime_initialized: false,
            utf16_strings: HashMap::new(),
            field_hash_cache: HashMap::new(),
            virtual_fields: HashMap::new(),
            stack_symbol_arena: Vec::new(),
            call_stack_symbols: Vec::new(),
            exception_stack_symbols: Vec::new(),
            tiered_runtime: None,
        }
    }

    pub fn enable_tiered(
        &mut self,
        hl_path: &Path,
        _native_resolver: &NativeFunctionResolver,
        bytecode: &Arc<DecodedBytecode>,
        mut config: TieredConfig,
    ) -> Result<()> {
        if config.tier_mode == TierMode::Off {
            eprintln!("[tiered] disabled (--jit-tier=off)");
            self.tiered_runtime = None;
            return Ok(());
        }
        // Hot-reload swaps bytecode bodies underneath the interpreter; the
        // Cranelift tier lowers from a bytecode snapshot it pins for the run,
        // so it would keep executing stale code. The LLVM tier already has an
        // indirect-call rewrite for this.
        if config.hot_reload && config.tier_mode != TierMode::Llvm {
            eprintln!("[tiered] hot-reload active: forcing --jit-tier=llvm");
            config.tier_mode = TierMode::Llvm;
        }
        config.enabled = true;

        let log_promotions = config.log_promotions;
        let hot_reload = config.hot_reload;
        let hl_path = hl_path.to_path_buf();

        // Register the bytecode path for hot-reload mtime detection
        if hot_reload {
            match _native_resolver.resolve_function("std", "hlp_setup_reload_check") {
                Ok(setup_fn) => {
                    let path_str = hl_path.to_string_lossy();
                    let mut utf16: Vec<u16> = path_str.encode_utf16().collect();
                    utf16.push(0);
                    type FnSetup = unsafe extern "C" fn(*const u16);
                    let setup: FnSetup = unsafe { std::mem::transmute(setup_fn) };
                    unsafe { setup(utf16.as_ptr()) };
                    eprintln!(
                        "[hot-reload] registered bytecode path: {}",
                        hl_path.display()
                    );
                }
                Err(e) => {
                    eprintln!(
                        "[hot-reload] warning: could not register reload check: {}",
                        e
                    );
                }
            }
        }
        let (globals_data_ptr, nglobals) = self.c_type_factory.globals_data();
        let shared = SharedRuntimeHandles {
            globals_data_ptr,
            nglobals,
            c_types: self.c_type_factory.as_slice().to_vec(),
            module_ctx: self.c_type_factory.module_ctx(),
        };

        // Initialize the global reload context and register the callback
        if hot_reload {
            // Build the functions_ptrs snapshot from the module context.
            // Use bytecode function/native count to size the table.
            let old_bc = ash_core::bytecode::BytecodeDecoder::decode(&hl_path);
            let max_findex = old_bc.as_ref().map_or(0, |bc| {
                let max_fn = bc
                    .functions
                    .iter()
                    .map(|f| f.findex as usize)
                    .max()
                    .unwrap_or(0);
                let max_nat = bc
                    .natives
                    .iter()
                    .map(|n| n.findex as usize)
                    .max()
                    .unwrap_or(0);
                max_fn.max(max_nat) + 1
            });
            let fptrs: Vec<*mut std::ffi::c_void> = (0..max_findex)
                .map(|i| unsafe {
                    if !shared.module_ctx.is_null()
                        && !(*shared.module_ctx).functions_ptrs.is_null()
                    {
                        *(*shared.module_ctx).functions_ptrs.add(i)
                    } else {
                        std::ptr::null_mut()
                    }
                })
                .collect();

            if let Ok(old_bc) = old_bc {
                ash_core::reload::init_reload_context(
                    hl_path.to_path_buf(),
                    old_bc,
                    fptrs,
                    shared.clone(),
                );

                // Register the actual reload callback
                if let Ok(set_cb_fn) =
                    _native_resolver.resolve_function("std", "hlp_set_reload_callback")
                {
                    type FnSetCb = unsafe extern "C" fn(unsafe extern "C" fn(*const u16) -> bool);
                    let set_cb: FnSetCb = unsafe { std::mem::transmute(set_cb_fn) };
                    unsafe { set_cb(ash_core::reload::reload_callback) };
                    eprintln!("[hot-reload] reload callback registered");
                }
            }
        }
        // Pre-warm the tiered JIT module ON THE MAIN THREAD, before any
        // bytecode runs. Module init GC-allocates (constants via
        // hlp_alloc_obj + hlp_gc_register_root, obj runtimes via
        // hlp_get_obj_rt, enum marks via hlp_init_enum) — doing it lazily on
        // the broker thread mid-program both froze the main thread (GC lock
        // held across the whole multi-second init) and let a broker-side
        // collection scan the wrong stack, reclaiming objects live only in
        // main-thread frames. Compilation itself (pure LLVM + MCJIT
        // finalization) stays on the broker thread, same as before.
        // Startup narration, gated: stderr is compared against an oracle's by
        // the parity harness, and no oracle narrates ash's tiering.
        if config.log_promotions {
            eprintln!("[tiered] pre-warming JIT module on main thread (one-time startup cost)...");
        }
        let prewarm_start = std::time::Instant::now();
        let prewarmed = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let context: &'static Context = Box::leak(Box::new(Context::create()));
            let mut jit = JITModule::new_with_shared_runtime(context, &hl_path, shared.clone());
            jit.set_hot_reload(hot_reload);
            jit.set_lazy_compilation(config.compiled_only);
            Box::into_raw(Box::new(ManuallyDrop::new(jit)))
        })) {
            Ok(ptr) => {
                if config.log_promotions {
                    eprintln!(
                        "[tiered] JIT module ready in {:.2}s",
                        prewarm_start.elapsed().as_secs_f64()
                    );
                }
                Some(PrewarmedJit(ptr))
            }
            Err(_) => {
                eprintln!("[tiered] pre-warm panicked; tier promotion disabled");
                None
            }
        };

        // Beadie owns the per-tier hotness policies and one broker thread per
        // tier. Queue-ahead submits the tier-0 compile job slightly before the
        // threshold so code is ready by the time the function is truly hot;
        // the LLVM tier fires two orders of magnitude later, once a function
        // has proven worth the heavier compile.
        // Compiled-only mode installs tier 0 synchronously on first reach.
        // Keeping the primary broker's threshold unreachable prevents a
        // duplicate asynchronous baseline compile racing that install.
        let threshold = if config.compiled_only {
            u32::MAX
        } else {
            u32::try_from(config.jit_threshold).unwrap_or(u32::MAX)
        };
        let queue_ahead = (threshold / 5).max(1);
        // The LLVM rung was `threshold * 100`, which made the two rungs
        // impossible to tune apart: lowering the LLVM threshold to see whether
        // the top tier pays also dropped Cranelift to promoting on the first
        // call. ASH_TIER1 sets it independently; the default is unchanged, so
        // this is a knob rather than a policy change until a measurement says
        // otherwise.
        // The API value is the source of truth; ASH_TIER1 overrides it so a
        // harness can sweep the rung without rebuilding.
        let tier1 = std::env::var("ASH_TIER1")
            .ok()
            .and_then(|v| v.trim().parse::<u32>().ok())
            .unwrap_or_else(|| u32::try_from(config.opt_threshold).unwrap_or(u32::MAX));
        let tier0: Box<dyn HotnessPolicy> =
            Box::new(ThresholdPolicy::new(threshold).queue_ahead(queue_ahead));
        // tier-1 gets the same head start as tier-0. An LLVM compile is the
        // long one, so submitting it only once the count is reached means the
        // code lands after the run that wanted it -- which is what made the
        // rung look unreachable and got a speculative compile bolted on beside
        // it. Queueing ahead is the mechanism for that, and it is the reason
        // the ladder can now carry the top tier by itself.
        let tier1_ahead = (tier1 / 5).max(1);
        let policies: Vec<Box<dyn HotnessPolicy>> = match config.tier_mode {
            TierMode::Auto => vec![
                tier0,
                Box::new(ThresholdPolicy::new(tier1).queue_ahead(tier1_ahead)),
            ],
            _ => vec![tier0],
        };
        let adapter = TieredAdapter::new(policies);
        if config.log_promotions {
            eprintln!(
                "[tiered] ladder: mode={} tier0={} {}",
                config.tier_mode.name(),
                threshold,
                match config.tier_mode {
                    TierMode::Auto => format!("tier1={} (llvm)", tier1),
                    _ => "single tier".to_string(),
                }
            );
        }

        let published_bytecode = Arc::clone(bytecode);
        // Publishing the bytecode once went through a set_bytecode helper
        // (since deleted — this is now its only caller and it was dead code),
        // which also computed this. Setting the OnceLock directly skipped it, and a
        // max_findex of 0 silently disables the functions_ptrs update in
        // install_function_address — every virtual dispatch from compiled
        // code then falls back to the interpreter bridge. deltablue went from
        // 119ms to 700ms with the right answer, which is exactly how a
        // "performance only" field hides.
        let published_max_findex = published_bytecode
            .functions
            .iter()
            .map(|f| f.findex as usize)
            .chain(published_bytecode.natives.iter().map(|n| n.findex as usize))
            .max()
            .map(|m| m + 1)
            .unwrap_or(0);
        let shared_ctx = Arc::new(TieredSharedCtx {
            log_promotions,
            tier_log: log_promotions || std::env::var("ASH_TIER_LOG").is_ok(),
            mode: config.tier_mode,
            compiled_only: config.compiled_only,
            llvm: Mutex::new(match prewarmed {
                Some(pw) => LlvmState::Pending(pw),
                None => LlvmState::Unavailable,
            }),
            cranelift: Mutex::new(None),
            arrays: SharedArrayHandles {
                globals_data: globals_data_ptr as usize,
                nglobals,
                c_types: shared.c_types.iter().map(|p| *p as usize).collect(),
                functions_ptrs: unsafe {
                    if shared.module_ctx.is_null() {
                        0
                    } else {
                        (*shared.module_ctx).functions_ptrs as usize
                    }
                },
            },
            bytecode: {
                let c = OnceLock::new();
                let _ = c.set(published_bytecode);
                c
            },
            max_findex: std::sync::atomic::AtomicUsize::new(published_max_findex),
            llvm_done: Mutex::new(HashSet::new()),
            llvm_failed: Mutex::new(HashSet::new()),
            hot_loop_pcs: Mutex::new(HashMap::new()),
            live_frame: Mutex::new(std::collections::HashSet::new()),
            called_from_loop: Mutex::new(std::collections::HashSet::new()),
            vtable_slots: OnceLock::new(),
            pending_osr: Mutex::new(HashMap::new()),
            worker_beads: Mutex::new(HashMap::new()),
            worker_compile_lock: Mutex::new(()),
            worker_closure_deps: Mutex::new(HashMap::new()),
            worker_closure_deps_changed: Condvar::new(),
            attempted: std::sync::atomic::AtomicU64::new(0),
            failed: std::sync::atomic::AtomicU64::new(0),
            cranelift_promotions: std::sync::atomic::AtomicU64::new(0),
            llvm_promotions: std::sync::atomic::AtomicU64::new(0),
        });

        self.tiered_runtime = Some(TieredRuntime {
            config,
            adapter,
            beads: Vec::new(),
            gate_checked: Vec::new(),
            entries: Vec::new(),
            sigs: Vec::new(),
            shared_ctx,
            stats: TieredStats::default(),
        });
        Ok(())
    }

    /// Check the compile-time layout oracle against the runtime's own answers.
    ///
    /// [`ash_core::layout`] exists so field access can become a constant-offset load
    /// instead of a call, which is only safe if it reproduces
    /// `hlp_get_obj_rt` exactly — a disagreement is silent memory corruption
    /// rather than a wrong answer. This forces every `HOBJ`/`HSTRUCT` runtime
    /// object to be built and compares all of them.
    ///
    /// Run via `ASH_VERIFY_LAYOUT=1`. Not on by default: building every runtime
    /// object costs startup time and allocates for types the program may never
    /// touch.
    pub fn verify_layout_oracle(
        &self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
    ) -> Result<Vec<ash_core::layout::LayoutMismatch>> {
        type GetObjRt = unsafe extern "C" fn(
            *mut ash_core::hl_bindings::hl_type,
        ) -> *mut ash_core::hl_bindings::hl_runtime_obj;
        let addr = native_resolver
            .resolve_function("std", "hlp_get_obj_rt")
            .map_err(|e| anyhow!("cannot verify layout without hlp_get_obj_rt: {e}"))?;
        let get_obj_rt: GetObjRt = unsafe { std::mem::transmute(addr) };

        Ok(unsafe {
            ash_core::layout::verify_against_runtime(&bytecode.types, |ti| {
                let t = self.c_type_factory.get(ti);
                if t.is_null() {
                    return None;
                }
                Some(get_obj_rt(t))
            })
        })
    }

    /// Stop promoting, and wait out any compile already running.
    ///
    /// Call once the entrypoint has returned, before the process exits. LLVM
    /// registers its own `atexit` handlers, and they tear down state a
    /// promotion in flight is still using: observed as a SIGSEGV inside
    /// `SelectionDAGISel` on the broker while the main thread sat in
    /// `~GDBJITRegistrationListener`, roughly one free_call run in thirty.
    ///
    /// Only stops what has not started. A compile already running is left
    /// alone and ended with the process, which is sound because the caller
    /// leaves through `_exit` -- no `atexit` handler runs, so nothing is torn
    /// down underneath it. Waiting for it instead costs the rest of the
    /// compile for a result nothing can call.
    pub fn quiesce_promotions(&self) {
        retier_abandon();
    }

    pub fn tiered_stats(&self) -> Option<TieredStats> {
        use std::sync::atomic::Ordering;
        self.tiered_runtime.as_ref().map(|t| {
            let mut stats = t.stats.clone();
            stats.attempted_promotions += t.shared_ctx.attempted.load(Ordering::Relaxed);
            stats.failed_promotions += t.shared_ctx.failed.load(Ordering::Relaxed);
            stats.cranelift_promotions = t.shared_ctx.cranelift_promotions.load(Ordering::Relaxed);
            stats.llvm_promotions = t.shared_ctx.llvm_promotions.load(Ordering::Relaxed);
            stats
        })
    }

    #[inline(always)]
    fn current_stack_addr() -> usize {
        // Portable stack probe: address of a local variable approximates current SP.
        let marker = 0u8;
        (&marker as *const u8) as usize
    }

    /// Get the base (highest address) of the current thread's stack.
    /// Used for GC conservative scanning — scans from stack base down to current SP.
    fn thread_stack_base() -> usize {
        #[cfg(target_os = "macos")]
        {
            unsafe {
                let thread = libc::pthread_self();
                let addr = libc::pthread_get_stackaddr_np(thread) as usize;
                if addr != 0 {
                    return addr;
                }
            }
        }
        #[cfg(target_os = "linux")]
        {
            // pthread_getattr_np, NOT pthread_attr_init: init creates a
            // FRESH default attr whose stack fields are null, so the old
            // code returned 0 — which zeroed stack_top, which disabled
            // every triggered collection (only the exhaustion backstop ever
            // ran: mandelbrot parked at 590MB RSS) AND skipped the
            // conservative machine-stack scan entirely on Linux, a
            // soundness hole the short corpus only survived because interp
            // snapshots and globals happened to cover it.
            unsafe {
                let mut attr: libc::pthread_attr_t = std::mem::zeroed();
                if libc::pthread_getattr_np(libc::pthread_self(), &mut attr) == 0 {
                    let mut stack_addr: *mut libc::c_void = std::ptr::null_mut();
                    let mut stack_size: libc::size_t = 0;
                    let ok = libc::pthread_attr_getstack(&attr, &mut stack_addr, &mut stack_size)
                        == 0
                        && !stack_addr.is_null()
                        && stack_size != 0;
                    libc::pthread_attr_destroy(&mut attr);
                    if ok {
                        // getstack returns the LOWEST address; the top is
                        // base + size (stacks grow down).
                        return stack_addr as usize + stack_size;
                    }
                }
            }
        }
        // Fallback: use a high local address
        Self::current_stack_addr() + 8 * 1024 * 1024 // assume 8MB stack
    }

    fn ensure_gc_runtime_initialized(&mut self) {
        if self.gc_runtime_initialized {
            return;
        }
        if self.fn_gc_set_globals.is_null() || self.fn_gc_set_stack_top.is_null() {
            return;
        }
        let (globals_ptr, globals_len) = self.c_type_factory.globals_data();
        if globals_ptr.is_null() {
            return;
        }
        type FnSetGlobals = unsafe extern "C" fn(*const *mut c_void, usize);
        type FnSetStackTop = unsafe extern "C" fn(usize);
        let set_globals: FnSetGlobals = unsafe { std::mem::transmute(self.fn_gc_set_globals) };
        let set_stack_top: FnSetStackTop = unsafe { std::mem::transmute(self.fn_gc_set_stack_top) };
        unsafe {
            set_globals(globals_ptr as *const *mut c_void, globals_len);
            // Set stack top to the thread's stack base (highest address),
            // not the current frame. On ARM/x86 stacks grow downward,
            // so the GC scans from stack_top down to current SP.
            let stack_top = Self::thread_stack_base();
            set_stack_top(stack_top);
        }
        self.gc_runtime_initialized = true;
    }

    /// Publish interpreter register memory as conservative GC scan ranges.
    /// This keeps live values held in bytecode registers visible to the std GC.
    fn sync_gc_scan_roots(&mut self) {
        if self.fn_gc_clear_scan_roots.is_null() || self.fn_gc_add_scan_root.is_null() {
            return;
        }
        type FnClear = unsafe extern "C" fn();
        type FnAdd = unsafe extern "C" fn(*const c_void, usize);
        let clear: FnClear = unsafe { std::mem::transmute(self.fn_gc_clear_scan_roots) };
        let add: FnAdd = unsafe { std::mem::transmute(self.fn_gc_add_scan_root) };

        // One call, one lock hold, when std offers the batched entry point.
        // The per-frame loop below is O(depth) cross-dylib calls each taking
        // and dropping the GC lock, and the interpreter publishes twice per
        // call — so root publication was quadratic in call depth.
        if !self.fn_gc_set_scan_roots.is_null() {
            type FnSet = unsafe extern "C" fn(*const (usize, usize), usize);
            let set: FnSet = unsafe { std::mem::transmute(self.fn_gc_set_scan_roots) };
            let mut buf = std::mem::take(&mut self.scan_range_buf);
            buf.clear();
            for frame in self
                .stack
                .iter()
                .chain(self.fiber_stacks.values().flatten())
            {
                let regs = frame.registers.as_slice();
                if !regs.is_empty() {
                    buf.push((regs.as_ptr() as usize, std::mem::size_of_val(regs)));
                }
            }
            unsafe { set(buf.as_ptr(), buf.len()) };
            self.scan_range_buf = buf;
            return;
        }

        unsafe { clear() };
        // Register each frame's LIVE register buffer, not a filtered copy.
        // The copy was a point-in-time snapshot: any value written to a
        // register after the last sync existed nowhere the collector could
        // see, and a collection landing in that window freed it — the same
        // one-world disease as the constants bug and the Reflect shadow
        // maps, this time in the root set itself. The buffers hold
        // NaN-boxed words; the conservative scanner decodes the box pattern
        // (see conservative_scan_range), so scanning them directly is
        // sound, always-current, and cheaper than rebuilding a Vec per
        // sync.
        for frame in self
            .stack
            .iter()
            .chain(self.fiber_stacks.values().flatten())
        {
            let regs = frame.registers.as_slice();
            if !regs.is_empty() {
                let ptr = regs.as_ptr() as *const c_void;
                let size = std::mem::size_of_val(regs);
                unsafe { add(ptr, size) };
            }
        }
        // The set is complete — a deferred collection is honored HERE, never
        // mid-registration against a half-built set.
        if !self.fn_gc_scan_roots_done.is_null() {
            type FnDone = unsafe extern "C" fn();
            let done: FnDone = unsafe { std::mem::transmute(self.fn_gc_scan_roots_done) };
            unsafe { done() };
        }
    }

    fn decode_utf16_chars(ptr: *const u16, len: i32) -> String {
        if ptr.is_null() || len <= 0 {
            return String::new();
        }
        let slice = unsafe { std::slice::from_raw_parts(ptr, len as usize) };
        String::from_utf16_lossy(slice)
    }

    fn value_to_string(&self, dyn_ptr: *mut hl::vdynamic) -> Option<String> {
        if self.fn_value_to_string.is_null() || dyn_ptr.is_null() {
            return None;
        }
        let f: FnValueToString = unsafe { std::mem::transmute(self.fn_value_to_string) };
        let mut len: i32 = 0;
        let out = unsafe { f(dyn_ptr, &mut len as *mut i32) };
        if out.is_null() || len <= 0 {
            None
        } else {
            Some(Self::decode_utf16_chars(out as *const u16, len))
        }
    }

    fn hash_literal_name(&self, name: &str) -> i32 {
        let mut utf16: Vec<u16> = name.encode_utf16().collect();
        utf16.push(0);
        if !self.fn_hash_gen.is_null() {
            let f: FnHashGen = unsafe { std::mem::transmute(self.fn_hash_gen) };
            return unsafe { f(utf16.as_ptr(), true) };
        }
        let mut h: i32 = 0;
        for c in &utf16[..utf16.len() - 1] {
            h = h.wrapping_mul(223).wrapping_add(*c as i32);
        }
        h.wrapping_rem(0x1FFFFF7B)
    }

    fn resolve_typed_field_hash(
        bytecode: &DecodedBytecode,
        obj_type_idx: usize,
        field_idx: usize,
    ) -> Option<i32> {
        let ty = bytecode.types.get(obj_type_idx)?;
        if let Some(obj) = ty.obj.as_ref() {
            if let Some(f) = obj.fields.get(field_idx) {
                if f.hashed_name != 0 {
                    return Some(f.hashed_name);
                }
            }
        }
        if let Some(virt) = ty.virt.as_ref() {
            if let Some(f) = virt.fields.get(field_idx) {
                if f.hashed_name != 0 {
                    return Some(f.hashed_name);
                }
            }
        }
        None
    }

    /// `virt->indexes[i]` is a byte offset into the virtual's OWN data area,
    /// which only exists when the virtual is self-backed — `hl_alloc_virtual`
    /// allocates `dataSize` extra bytes and points each field there.
    ///
    /// A virtual produced by `hl_to_virtual` is a VIEW: `value` holds the
    /// wrapped object, there is no data area, and the field lives in the
    /// wrapped object instead. Applying the offset to one of those addresses
    /// past the end of the allocation, so the caller must take the dynamic
    /// path and reach the field through `value`.
    ///
    /// `value.is_null()` is the discriminator upstream uses for exactly this
    /// (hashlink src/std/obj.c:784, mirrored by ash's own hlp_obj_lookup).
    unsafe fn resolve_virtual_field_offset(
        obj_ptr: *mut u8,
        c_type_ptr: *mut c_void,
        field_idx: usize,
    ) -> Option<usize> {
        if c_type_ptr.is_null() {
            return None;
        }
        if obj_ptr.is_null() {
            return None;
        }
        // Trust the runtime header: ToVirtual is a bare register copy, so an
        // HVIRTUAL-typed register may hold a raw object.
        let hdr = *(obj_ptr as *const *const hl_type);
        if hdr.is_null()
            || !(hdr as usize).is_multiple_of(std::mem::align_of::<usize>())
            || (*hdr).kind != hl::hl_type_kind_HVIRTUAL
        {
            return None;
        }
        if !(*(obj_ptr as *const hl::vvirtual)).value.is_null() {
            return None;
        }
        let t = c_type_ptr as *mut hl_type;
        if t.is_null() || (*t).kind != hl::hl_type_kind_HVIRTUAL {
            return None;
        }
        let virt = (*t).__bindgen_anon_1.virt;
        if virt.is_null() || (*virt).indexes.is_null() || field_idx >= (*virt).nfields as usize {
            return None;
        }
        let off = *(*virt).indexes.add(field_idx);
        if off < 0 {
            None
        } else {
            Some(off as usize)
        }
    }

    /// Box a primitive on its way into a Dynamic slot.
    ///
    /// A Dynamic in MEMORY is a pointer to a box (`vdynamic*`); HashLink reads
    /// every such slot that way. A Dynamic in a REGISTER may hold an unboxed
    /// primitive, which is a deliberate in-register optimisation — so the box
    /// has to be created at the crossing, not maintained throughout.
    ///
    /// Writing a raw primitive into an 8-byte slot leaves the upper bytes
    /// stale and the next read of that slot yields a torn pointer.
    #[allow(clippy::too_many_arguments)]
    fn box_for_dynamic_slot(
        fn_make_dyn: *mut c_void,
        t_i32: *mut c_void,
        t_f64: *mut c_void,
        t_bool: *mut c_void,
        dst_kind: hl::hl_type_kind,
        val: NanBoxedValue,
    ) -> NanBoxedValue {
        if !matches!(
            dst_kind,
            hl::hl_type_kind_HDYN | hl::hl_type_kind_HNULL | hl::hl_type_kind_HDYNOBJ
        ) {
            return val;
        }
        if fn_make_dyn.is_null() || val.is_null() || val.is_void() {
            return val;
        }
        let (t, mut data): (*mut c_void, i64) = if val.is_i32() {
            (t_i32, val.as_i32() as i64)
        } else if val.is_bool() {
            (t_bool, val.as_bool() as i64)
        } else if val.is_f64() {
            (t_f64, val.as_f64().to_bits() as i64)
        } else {
            return val; // already a pointer
        };
        if t.is_null() {
            return val;
        }
        let make_dyn: unsafe extern "C" fn(*mut c_void, *mut c_void) -> *mut c_void =
            unsafe { std::mem::transmute(fn_make_dyn) };
        let boxed = unsafe { make_dyn(&mut data as *mut i64 as *mut c_void, t) };
        if boxed.is_null() {
            val
        } else {
            NanBoxedValue::from_ptr(boxed as usize)
        }
    }

    /// Materialize the interpreter's unboxed Dynamic register value at a
    /// compiled ABI boundary (argument or return).
    ///
    /// AIR V2 native code represents `HDYN`/`HNULL` as `vdynamic*`. The
    /// interpreter deliberately keeps primitive values unboxed while they
    /// remain in registers, so forwarding their numeric payload as an i64
    /// turns values such as `5` into pointer `0x5`. Box exactly at this
    /// boundary; already self-describing heap values pass through unchanged.
    fn box_for_compiled_dynamic_value(&self, val: NanBoxedValue) -> NanBoxedValue {
        if self.fn_make_dyn.is_null() || val.is_null() || val.is_void() || val.is_ptr() {
            return val;
        }

        let (t, mut data): (*mut c_void, i64) = if val.is_i32() {
            (self.prim_t_i32, val.as_i32() as i64)
        } else if val.is_i64() {
            (self.prim_t_i64, val.as_i64_lossy())
        } else if val.is_bool() {
            (self.prim_t_bool, val.as_bool() as i64)
        } else if val.is_f64() {
            (self.prim_t_f64, val.as_f64().to_bits() as i64)
        } else if val.is_bytes() {
            (self.prim_t_bytes, val.as_ptr() as i64)
        } else {
            // Function-index sentinels should already have been materialized
            // by StaticClosure before a Dynamic call. Preserve the value if a
            // future representation reaches this boundary instead of making
            // up a vdynamic header for it.
            return val;
        };
        if t.is_null() {
            return val;
        }

        let make_dyn: unsafe extern "C" fn(*mut c_void, *mut c_void) -> *mut c_void =
            unsafe { std::mem::transmute(self.fn_make_dyn) };
        let boxed = unsafe { make_dyn((&mut data as *mut i64).cast(), t) };
        if boxed.is_null() {
            val
        } else {
            NanBoxedValue::from_ptr(boxed as usize)
        }
    }

    #[inline]
    fn is_primitive_or_bytes_kind(kind: hl::hl_type_kind) -> bool {
        matches!(
            kind,
            hl::hl_type_kind_HI32
                | hl::hl_type_kind_HUI8
                | hl::hl_type_kind_HUI16
                | hl::hl_type_kind_HI64
                | hl::hl_type_kind_HF32
                | hl::hl_type_kind_HF64
                | hl::hl_type_kind_HBOOL
        )
    }

    /// Kinds where a pointer return should be unboxed from vdynamic.
    /// HBYTES is excluded — bytes pointers are raw buffers, not boxed primitives.
    fn is_unboxable_primitive_kind(kind: hl::hl_type_kind) -> bool {
        matches!(
            kind,
            hl::hl_type_kind_HI32
                | hl::hl_type_kind_HUI8
                | hl::hl_type_kind_HUI16
                | hl::hl_type_kind_HI64
                | hl::hl_type_kind_HF32
                | hl::hl_type_kind_HF64
                | hl::hl_type_kind_HBOOL
        )
    }

    #[inline]
    fn is_numeric_or_bool_kind(kind: hl::hl_type_kind) -> bool {
        matches!(
            kind,
            hl::hl_type_kind_HI32
                | hl::hl_type_kind_HUI8
                | hl::hl_type_kind_HUI16
                | hl::hl_type_kind_HI64
                | hl::hl_type_kind_HF32
                | hl::hl_type_kind_HF64
                | hl::hl_type_kind_HBOOL
        )
    }

    fn box_value_as_dynamic_with_type(
        &self,
        val: NanBoxedValue,
        field_t: *mut hl_type,
    ) -> NanBoxedValue {
        if val.is_null() || val.is_void() {
            return NanBoxedValue::null();
        }
        if field_t.is_null() {
            return if val.is_ptr() {
                NanBoxedValue::from_ptr(val.as_ptr())
            } else {
                NanBoxedValue::null()
            };
        }
        let kind = unsafe { (*field_t).kind };
        if !Self::is_primitive_or_bytes_kind(kind) {
            // A Dynamic-typed source says nothing about what it holds, but the
            // value's own tag does, and a tagged primitive must still become a
            // real box before it lands in Dynamic STORAGE: upstream's contract
            // is that an HDYN slot always holds a vdynamic*, and every reader
            // (hl_dyn_casti and friends) dereferences it unconditionally.
            // Returning the payload as a pointer here wrote the integer itself
            // into the array, and the first compiled reader faulted on it.
            if !val.is_ptr() {
                return self.box_for_compiled_dynamic_value(val);
            }
            return NanBoxedValue::from_ptr(val.as_ptr());
        }
        if self.fn_make_dyn.is_null() {
            return val;
        }
        let mut data: i64 = match kind {
            hl::hl_type_kind_HI32 | hl::hl_type_kind_HUI8 | hl::hl_type_kind_HUI16 => {
                val.as_i32() as i64
            }
            hl::hl_type_kind_HI64 => val.as_i64_lossy(),
            hl::hl_type_kind_HF32 | hl::hl_type_kind_HF64 => val.as_f64().to_bits() as i64,
            hl::hl_type_kind_HBOOL => {
                if val.as_bool() {
                    1
                } else {
                    0
                }
            }
            hl::hl_type_kind_HBYTES => val.as_ptr() as i64,
            _ => val.as_ptr() as i64,
        };
        let make_dyn: unsafe extern "C" fn(*mut c_void, *mut c_void) -> *mut c_void =
            unsafe { std::mem::transmute(self.fn_make_dyn) };
        let dyn_ptr =
            unsafe { make_dyn(&mut data as *mut i64 as *mut c_void, field_t as *mut c_void) };
        if dyn_ptr.is_null() {
            NanBoxedValue::null()
        } else {
            NanBoxedValue::from_ptr(dyn_ptr as usize)
        }
    }

    #[inline]
    fn coerce_value_for_static_kind(
        val: NanBoxedValue,
        dst_kind: hl::hl_type_kind,
    ) -> NanBoxedValue {
        if val.is_ptr()
            && !val.is_null()
            && val.as_ptr() != 0
            && Self::is_unboxable_primitive_kind(dst_kind)
        {
            // Only attempt unboxing if the pointer looks like a valid vdynamic:
            // aligned, non-tiny address, and type pointer field also looks valid.
            let addr = val.as_ptr();
            if addr > 0x10000 && addr.is_multiple_of(std::mem::align_of::<usize>()) {
                let d = addr as *const hl::vdynamic;
                let t = unsafe { (*d).t };
                if !t.is_null() && (t as usize).is_multiple_of(std::mem::align_of::<usize>()) {
                    let kind = unsafe { (*t).kind };
                    // Only unbox if the source type IS a boxed primitive
                    if Self::is_unboxable_primitive_kind(kind) {
                        return unsafe {
                            Self::unbox_dynamic_to_kind(addr as *mut hl::vdynamic, dst_kind)
                                .unwrap_or(val)
                        };
                    }
                }
            }
        }
        if val.is_null() {
            return match dst_kind {
                hl::hl_type_kind_HI32 | hl::hl_type_kind_HUI8 | hl::hl_type_kind_HUI16 => {
                    NanBoxedValue::from_i32(0)
                }
                hl::hl_type_kind_HI64 => NanBoxedValue::from_i64(0),
                hl::hl_type_kind_HF32 | hl::hl_type_kind_HF64 => NanBoxedValue::from_f64(0.0),
                hl::hl_type_kind_HBOOL => NanBoxedValue::from_bool(false),
                _ => val,
            };
        }

        // Dynamic registers deliberately keep primitives unboxed.  When one
        // crosses back into a statically typed primitive register, however,
        // its NanBox tag must agree with that destination.  Leaving I32(5) in
        // an HF64 register makes the next float comparison see unlike value
        // representations even though both operands are numerically 5.  This
        // mirrors hl_dyn_cast{ i, i64, f, d } for the in-register form that
        // never reaches those pointer-based helpers.
        let as_i64 = if val.is_i32() {
            Some(val.as_i32() as i64)
        } else if val.is_i64() {
            Some(val.as_i64_lossy())
        } else if val.is_f64() {
            Some(val.as_f64() as i64)
        } else if val.is_bool() {
            Some(if val.as_bool() { 1 } else { 0 })
        } else {
            None
        };
        let as_f64 = if val.is_i32() {
            Some(val.as_i32() as f64)
        } else if val.is_i64() {
            Some(val.as_i64_lossy() as f64)
        } else if val.is_f64() {
            Some(val.as_f64())
        } else if val.is_bool() {
            Some(if val.as_bool() { 1.0 } else { 0.0 })
        } else {
            None
        };
        match dst_kind {
            hl::hl_type_kind_HI32 => as_i64
                .map(|v| NanBoxedValue::from_i32(v as i32))
                .unwrap_or(val),
            hl::hl_type_kind_HUI8 => as_i64
                .map(|v| NanBoxedValue::from_i32(v as u8 as i32))
                .unwrap_or(val),
            hl::hl_type_kind_HUI16 => as_i64
                .map(|v| NanBoxedValue::from_i32(v as u16 as i32))
                .unwrap_or(val),
            hl::hl_type_kind_HI64 => as_i64.map(NanBoxedValue::from_i64).unwrap_or(val),
            hl::hl_type_kind_HF32 => as_f64
                .map(|v| NanBoxedValue::from_f64(v as f32 as f64))
                .unwrap_or(val),
            hl::hl_type_kind_HF64 => as_f64.map(NanBoxedValue::from_f64).unwrap_or(val),
            hl::hl_type_kind_HBOOL => as_i64
                .map(|v| NanBoxedValue::from_bool(v != 0))
                .unwrap_or(val),
            _ => val,
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn dyn_get_field_by_hash(
        obj_ptr: *mut c_void,
        hfield: i32,
        dst_kind: hl::hl_type_kind,
        dst_type_ptr: *mut c_void,
        fn_dyn_getd: *mut c_void,
        fn_dyn_getf: *mut c_void,
        fn_dyn_geti64: *mut c_void,
        fn_dyn_geti: *mut c_void,
        fn_dyn_getp: *mut c_void,
    ) -> NanBoxedValue {
        match dst_kind {
            hl::hl_type_kind_HF64 => {
                if fn_dyn_getd.is_null() {
                    NanBoxedValue::null()
                } else {
                    let f: FnDynGetD = unsafe { std::mem::transmute(fn_dyn_getd) };
                    NanBoxedValue::from_f64(unsafe { f(obj_ptr, hfield) })
                }
            }
            hl::hl_type_kind_HF32 => {
                if fn_dyn_getf.is_null() {
                    NanBoxedValue::null()
                } else {
                    let f: FnDynGetF = unsafe { std::mem::transmute(fn_dyn_getf) };
                    NanBoxedValue::from_f64(unsafe { f(obj_ptr, hfield) as f64 })
                }
            }
            hl::hl_type_kind_HI64 => {
                if fn_dyn_geti64.is_null() {
                    NanBoxedValue::null()
                } else {
                    let f: FnDynGetI64 = unsafe { std::mem::transmute(fn_dyn_geti64) };
                    NanBoxedValue::from_i64(unsafe { f(obj_ptr, hfield) })
                }
            }
            hl::hl_type_kind_HI32
            | hl::hl_type_kind_HBOOL
            | hl::hl_type_kind_HUI8
            | hl::hl_type_kind_HUI16 => {
                if fn_dyn_geti.is_null() {
                    NanBoxedValue::null()
                } else {
                    let f: FnDynGetI = unsafe { std::mem::transmute(fn_dyn_geti) };
                    let i = unsafe { f(obj_ptr, hfield, dst_type_ptr) };
                    if dst_kind == hl::hl_type_kind_HBOOL {
                        NanBoxedValue::from_bool(i != 0)
                    } else {
                        NanBoxedValue::from_i32(i)
                    }
                }
            }
            _ => {
                if fn_dyn_getp.is_null() {
                    NanBoxedValue::null()
                } else {
                    let f: FnDynGetP = unsafe { std::mem::transmute(fn_dyn_getp) };
                    let p = unsafe { f(obj_ptr, hfield, dst_type_ptr) };
                    if p.is_null() {
                        NanBoxedValue::null()
                    } else {
                        NanBoxedValue::from_ptr(p as usize)
                    }
                }
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn dyn_set_field_by_hash(
        obj_ptr: *mut c_void,
        hfield: i32,
        src_val: NanBoxedValue,
        src_kind: hl::hl_type_kind,
        src_type_ptr: *mut c_void,
        fn_dyn_setd: *mut c_void,
        fn_dyn_setf: *mut c_void,
        fn_dyn_seti64: *mut c_void,
        fn_dyn_seti: *mut c_void,
        fn_dyn_setp: *mut c_void,
    ) {
        match src_kind {
            hl::hl_type_kind_HF64 => {
                if !fn_dyn_setd.is_null() {
                    let f: FnDynSetD = unsafe { std::mem::transmute(fn_dyn_setd) };
                    unsafe { f(obj_ptr, hfield, src_val.as_f64()) };
                }
            }
            hl::hl_type_kind_HF32 => {
                if !fn_dyn_setf.is_null() {
                    let f: FnDynSetF = unsafe { std::mem::transmute(fn_dyn_setf) };
                    unsafe { f(obj_ptr, hfield, src_val.as_f64() as f32) };
                }
            }
            hl::hl_type_kind_HI64 => {
                if !fn_dyn_seti64.is_null() {
                    let f: FnDynSetI64 = unsafe { std::mem::transmute(fn_dyn_seti64) };
                    unsafe { f(obj_ptr, hfield, src_val.as_i64_lossy()) };
                }
            }
            hl::hl_type_kind_HI32
            | hl::hl_type_kind_HBOOL
            | hl::hl_type_kind_HUI8
            | hl::hl_type_kind_HUI16 => {
                if !fn_dyn_seti.is_null() {
                    let f: FnDynSetI = unsafe { std::mem::transmute(fn_dyn_seti) };
                    let i = if src_kind == hl::hl_type_kind_HBOOL {
                        if src_val.as_bool() {
                            1
                        } else {
                            0
                        }
                    } else {
                        src_val.as_i32()
                    };
                    unsafe { f(obj_ptr, hfield, src_type_ptr, i) };
                }
            }
            _ => {
                if !fn_dyn_setp.is_null() {
                    let f: FnDynSetP = unsafe { std::mem::transmute(fn_dyn_setp) };
                    let p = if src_val.is_null() || src_val.is_void() {
                        std::ptr::null_mut()
                    } else {
                        src_val.as_ptr() as *mut c_void
                    };
                    unsafe { f(obj_ptr, hfield, src_type_ptr, p) };
                }
            }
        }
    }

    fn dynamic_type_name(&self, d: *mut hl::vdynamic) -> Option<String> {
        if d.is_null() || self.fn_type_name.is_null() {
            return None;
        }
        let t = unsafe { (*d).t };
        if t.is_null() {
            return None;
        }
        let f: FnTypeName = unsafe { std::mem::transmute(self.fn_type_name) };
        let name_ptr = unsafe { f(t as *const hl::hl_type) };
        if name_ptr.is_null() {
            return None;
        }
        // hlp_type_name returns the type's UTF-16 uchar* name (obj.name /
        // tenum.name / abs_name — HashLink convention). Reading it as a
        // UTF-8 C string truncated every name to its first character (the
        // second byte of UTF-16LE 'S' is NUL), so "Strength",
        // "ScaleConstraint" and "StayConstraint" all reported "S" — which
        // the equality paths treat as String and then content-compare
        // arbitrary objects as if they had bytes/length fields. Decode the
        // full UTF-16 name instead.
        let name_u16 = name_ptr as *const u16;
        let mut n = 0usize;
        // 4096 chars is far beyond any real type name; bail rather than walk
        // an unterminated buffer.
        while n < 4096 && unsafe { *name_u16.add(n) } != 0 {
            n += 1;
        }
        let slice = unsafe { std::slice::from_raw_parts(name_u16, n) };
        Some(String::from_utf16_lossy(slice))
    }

    /// Turn a stub-bridge failure into a real HL exception on the native trap
    /// chain. **Never returns.**
    ///
    /// The bridge runs *inside* a JIT frame, so its return value is consumed
    /// as a raw word of the callee's declared return type. Reporting a failure
    /// by returning `0` therefore hands compiled code a null it immediately
    /// uses — the reported crash was a field load at offset 0x10 off that
    /// null, one JIT instruction after the bridge returned. There is no safe
    /// poison value, so every failure leaves through `hlp_throw`:
    ///
    /// * A propagating HL exception (`Throw`/`Rethrow` with a real value)
    ///   rethrows unchanged.
    /// * An interpreter-internal failure — a `NullCheck`'s "Null access", an
    ///   unknown findex — carries no throwable `vdynamic`, so one is minted
    ///   from the message exactly the way HashLink's `hl_error` does (a bytes
    ///   dynamic). Haxe sees what it would in interpreter mode, and heaps'
    ///   `catch(e:Dynamic)` in `runMainLoop` handles it identically.
    ///
    /// The throw longjmps to the `setjmp` in `call_compiled_function`, which
    /// restores the interpreter frame stack (the Rust frames between here and
    /// there are abandoned without unwinding) and returns the exception to the
    /// interpreted caller.
    ///
    /// Aborting is the last resort, used only when the stdlib offers no way to
    /// raise at all.
    ///
    /// # Safety
    /// Must be called from the stub bridge, i.e. with a trap armed by
    /// `call_compiled_function` somewhere up the stack.
    unsafe fn raise_stub_bridge_failure(
        resolver: &NativeFunctionResolver,
        findex: usize,
        err: anyhow::Error,
    ) -> ! {
        // One line per findex: the exception itself is the report, and the old
        // unconditional log turned a per-event failure into a stderr flood.
        static REPORTED: OnceLock<Mutex<HashSet<usize>>> = OnceLock::new();
        let first = REPORTED
            .get_or_init(|| Mutex::new(HashSet::new()))
            .lock()
            .map(|mut s| s.insert(findex))
            .unwrap_or(true);
        if first {
            eprintln!(
                "[ash] stub bridge: findex {} failed, raising into the HL trap chain: {:#}",
                findex, err
            );
        }

        let throw_fn = resolver
            .resolve_function("std", "hlp_throw")
            .unwrap_or(std::ptr::null_mut());
        if let Some(hl_exc) = err.downcast_ref::<HLExceptionPropagation>() {
            let val = hl_exc.value;
            if val.is_ptr() && val.as_ptr() != 0 && !throw_fn.is_null() {
                type FnThrow = unsafe extern "C" fn(*mut c_void) -> !;
                let f: FnThrow = std::mem::transmute(throw_fn);
                f(val.as_ptr() as *mut c_void);
            }
        }

        // No throwable value: mint one from the message, as `hl_error` does.
        let error_fn = resolver
            .resolve_function("std", "hlp_error")
            .unwrap_or(std::ptr::null_mut());
        if !error_fn.is_null() {
            let msg = Self::interned_utf16_message(&format!("{:#}", err));
            // `hlp_error` is variadic (`printf`-style), but it is called here
            // with only its fixed argument, and on AAPCS64 the fixed argument
            // lands in x0 either way — the variadic tail lives on the stack.
            type FnError = unsafe extern "C" fn(*const u16) -> !;
            let f: FnError = std::mem::transmute(error_fn);
            f(msg);
        }

        eprintln!(
            "[ash] FATAL: stub bridge cannot raise for findex {} (hlp_throw/hlp_error \
             unresolvable); aborting rather than returning a poison value: {:#}",
            findex, err
        );
        std::process::abort();
    }

    /// Intern `msg` as a leaked, NUL-terminated UTF-16 buffer.
    ///
    /// `hl_error`-style exception values keep a borrowed `uchar*`, so the
    /// buffer has to outlive the throw. Interning bounds the leak by the
    /// number of distinct messages instead of the number of throws.
    fn interned_utf16_message(msg: &str) -> *const u16 {
        static MESSAGES: OnceLock<Mutex<HashMap<String, &'static [u16]>>> = OnceLock::new();
        let map = MESSAGES.get_or_init(|| Mutex::new(HashMap::new()));
        let mut guard = match map.lock() {
            Ok(g) => g,
            Err(poisoned) => poisoned.into_inner(),
        };
        let buf = *guard.entry(msg.to_string()).or_insert_with(|| {
            let mut utf16: Vec<u16> = msg.encode_utf16().collect();
            utf16.push(0);
            &*Box::leak(utf16.into_boxed_slice())
        });
        // Drop the guard before returning: the caller longjmps out of its
        // frame, which would otherwise strand the lock held forever.
        drop(guard);
        buf.as_ptr()
    }

    /// Mint the `vdynamic(HBYTES)` value HashLink's `hl_error` would throw.
    ///
    /// Interpreter-only failures cannot call `hl_error` because it longjmps
    /// across Rust frames. They still need a real non-null Haxe value: catch
    /// wrapping uses it to distinguish a native exception (whose saved stack
    /// comes from `exception_stack_raw`) from an ordinary Exception constructor
    /// (whose stack starts inside `ValueException.new`).
    fn internal_exception_value(&self, message: &str) -> NanBoxedValue {
        if self.fn_make_dyn.is_null() || self.prim_t_bytes.is_null() {
            return NanBoxedValue::null();
        }
        let mut bytes = Self::interned_utf16_message(message) as *mut c_void;
        let make_dyn: unsafe extern "C" fn(*mut c_void, *mut c_void) -> *mut c_void =
            unsafe { std::mem::transmute(self.fn_make_dyn) };
        let value = unsafe {
            make_dyn(
                &mut bytes as *mut *mut c_void as *mut c_void,
                self.prim_t_bytes,
            )
        };
        if value.is_null() {
            NanBoxedValue::null()
        } else {
            NanBoxedValue::from_ptr(value as usize)
        }
    }

    /// Allocate a bound closure (`InstanceClosure` / `VirtualClosure`) whose
    /// `fun` field is the interpreter's stub sentinel (`findex + 1`).
    ///
    /// `closure_type` is the destination register's declared type — the
    /// signature *without* the bound `this`, which is exactly what a
    /// `vclosure.t` must carry. It must be non-null: `hl_dyn_castp` reads the
    /// source type out of the value's header and bails on null, so a null
    /// here makes every later cast of the closure yield null and the
    /// following `NullCheck` raise "Null access".
    ///
    /// Allocation goes through `hlp_alloc_closure_ptr`, the same helper the JIT
    /// uses, so the closure lives in the GC heap and the bound value stays
    /// traceable. The leaked `Box` fallback only runs if the stdlib helper is
    /// unresolvable.
    ///
    /// # Safety
    /// `alloc_closure_ptr`, when non-null, must be `hlp_alloc_closure_ptr`.
    unsafe fn alloc_bound_closure(
        alloc_closure_ptr: *mut c_void,
        closure_type: *mut hl_type,
        findex: usize,
        value: *mut c_void,
    ) -> NanBoxedValue {
        let fun = (findex + 1) as *mut c_void;
        if !alloc_closure_ptr.is_null() {
            type FnAllocClosurePtr =
                unsafe extern "C" fn(*mut hl_type, *mut c_void, *mut c_void) -> *mut _vclosure;
            let f: FnAllocClosurePtr = std::mem::transmute(alloc_closure_ptr);
            let c = f(closure_type, fun, value);
            if !c.is_null() {
                return NanBoxedValue::from_ptr(c as usize);
            }
        }
        let closure = Box::new(_vclosure {
            t: closure_type,
            fun,
            hasValue: 1,
            stackCount: 0,
            value,
        });
        NanBoxedValue::from_ptr(Box::into_raw(closure) as usize)
    }

    fn format_hl_exception(&self, val: NanBoxedValue) -> HLExceptionPropagation {
        let msg = if val.is_null() || val.is_void() {
            None
        } else {
            let dyn_ptr = val.as_ptr() as *mut hl::vdynamic;
            // Primitive exceptions such as the HBYTES value produced by
            // `hl_error` have no fields. Record that before string conversion,
            // which may allocate and retire this short-lived native exception.
            let has_fields = unsafe {
                let t = (*dyn_ptr).t;
                !t.is_null()
                    && matches!(
                        (*t).kind,
                        hl::hl_type_kind_HOBJ
                            | hl::hl_type_kind_HVIRTUAL
                            | hl::hl_type_kind_HDYNOBJ
                            | hl::hl_type_kind_HSTRUCT
                    )
            };
            let base = self.value_to_string(dyn_ptr);
            if has_fields && !self.fn_obj_get_field.is_null() {
                let get_field: FnObjGetField =
                    unsafe { std::mem::transmute(self.fn_obj_get_field) };
                let mut extracted: Option<String> = None;
                for field_name in ["__exceptionMessage", "message"] {
                    let h = self.hash_literal_name(field_name);
                    let msg_dyn = unsafe { get_field(dyn_ptr, h) };
                    if let Some(inner) = self.value_to_string(msg_dyn) {
                        if !inner.is_empty() && inner != "null" {
                            extracted = Some(inner);
                            break;
                        }
                    }
                }
                if let Some(inner) = extracted {
                    if let Some(base) = base.as_ref() {
                        if !inner.is_empty() && inner != *base {
                            Some(format!("{}: {}", base, inner))
                        } else {
                            Some(base.clone())
                        }
                    } else if !inner.is_empty() {
                        Some(inner)
                    } else {
                        base
                    }
                } else {
                    base
                }
            } else {
                base
            }
        };
        HLExceptionPropagation {
            value: val,
            message: msg,
            stack: Vec::new(),
        }
    }

    /// Intern a bytecode string as null-terminated UTF-16 and return a stable pointer.
    fn intern_utf16_string(
        &mut self,
        bytecode: &DecodedBytecode,
        str_idx: usize,
    ) -> Option<*const u16> {
        if let Some(cached) = self.utf16_strings.get(&str_idx) {
            return Some(cached.as_ptr());
        }
        let s = bytecode.strings.get(str_idx)?;
        let mut utf16: Vec<u16> = s.encode_utf16().collect();
        utf16.push(0);
        self.utf16_strings.insert(str_idx, utf16);
        Some(self.utf16_strings[&str_idx].as_ptr())
    }

    /// Execute starting from the bytecode entrypoint.
    pub fn execute_entrypoint(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
    ) -> Result<NanBoxedValue> {
        self.ensure_gc_runtime_initialized();
        // Initialize constants (pre-populated globals) before running
        self.init_constants(bytecode, native_resolver)?;

        // Register the fiber closure runner: ash_std's thread_create runs
        // Haxe thread bodies on krio fibers, and their vclosure fun pointers
        // are interpreter stubs (findex+1) natives cannot call — this shim
        // re-enters the interpreter for them. Raw-pointer context is safe:
        // fibers run on this same OS thread, strictly within
        // execute_entrypoint's dynamic extent.
        struct ClosureRunCtx {
            interp: *mut HLInterpreter,
            bytecode: *const DecodedBytecode,
            resolver: *const NativeFunctionResolver,
            fiber_is_root_closure: *mut c_void,
            fiber_is_worker_lane: *mut c_void,
            jit_closure_runner: *mut c_void,
            compiled_stub_ctx: Option<Arc<TieredSharedCtx>>,
        }
        static mut CLOSURE_RUN_CTX: Option<ClosureRunCtx> = None;
        unsafe extern "C" fn fiber_closure_runner(
            c: *mut c_void,
            args: *mut *mut c_void,
            nargs: i32,
        ) -> *mut c_void {
            let Some(ctx) = (&raw const CLOSURE_RUN_CTX).as_ref().unwrap().as_ref() else {
                return std::ptr::null_mut();
            };
            let cl = c as *const hl::_vclosure;
            if cl.is_null() {
                return std::ptr::null_mut();
            }
            let fun = (*cl).fun as usize;
            if fun == 0 {
                eprintln!("[ash] fiber runner: null closure function");
                return std::ptr::null_mut();
            }
            if fun >= ash_core::llvm::stub_bridge::STUB_SENTINEL_LIMIT as usize {
                if ctx.jit_closure_runner.is_null() {
                    eprintln!("[ash] fiber runner: compiled closure bridge unavailable");
                    return std::ptr::null_mut();
                }
                type JitClosureRunner =
                    unsafe extern "C" fn(*mut c_void, *mut *mut c_void, i32) -> *mut c_void;
                let run: JitClosureRunner = std::mem::transmute(ctx.jit_closure_runner);
                return run(c, args, nargs);
            }
            let findex = fun.wrapping_sub(1);
            let is_fiber_root = if ctx.fiber_is_root_closure.is_null() {
                false
            } else {
                let is_root: unsafe extern "C" fn(*mut c_void) -> bool =
                    std::mem::transmute(ctx.fiber_is_root_closure);
                is_root(c)
            };
            let mut args_v = Vec::new();
            if (*cl).hasValue != 0 && !(*cl).value.is_null() {
                args_v.push(NanBoxedValue::from_ptr((*cl).value as usize));
            }
            let interp = &mut *ctx.interp;
            let bytecode = &*ctx.bytecode;
            // Marshal the caller's arguments. These were dropped outright
            // until now — the parameters were literally named `_args` and
            // `_nargs` — so every stub closure invoked from native code ran
            // with `this` and nothing else, and the callee saw uninitialised
            // parameters. ArrayDyn.__cast(t) compares `t` against a type and
            // returned null for want of ever receiving one, which is what
            // made Array<Dynamic> -> Array<Int> casts fail under the
            // interpreter.
            //
            // The bridge contract is an array of `vdynamic*`, so each word is
            // converted against the callee's declared parameter kind — the
            // same rule try_handle_call_method_native applies.
            let n = nargs.max(0) as usize;
            // Stable backing for HREF arguments. The compiled/interpreted call
            // is synchronous, so keeping these boxes alive through
            // `call_function` gives the callee a real addressable cell.
            let mut ref_cells: Vec<Box<u64>> = Vec::new();
            if n > 0 && !args.is_null() {
                let declared: Vec<usize> = func_of(&interp.targets, findex)
                    .and_then(|fi| {
                        bytecode.types[bytecode.functions[fi].type_.0]
                            .fun
                            .as_ref()
                            .map(|f| f.args.iter().map(|a| a.0).collect())
                    })
                    .unwrap_or_default();
                if env_flag!("ASH_DBG_FIBER") {
                    let kinds: Vec<_> =
                        declared.iter().map(|&ty| bytecode.types[ty].kind).collect();
                    eprintln!(
                        "[fiber-runner] findex={findex} has_value={} nargs={n} declared={kinds:?}",
                        (*cl).hasValue
                    );
                }
                // `this` already occupies slot 0 when the closure carries a
                // value, so the caller's first argument is declared arg 1.
                let shift = args_v.len();
                for i in 0..n {
                    let raw = *(args as *mut *mut hl::vdynamic).add(i);
                    let expected_type_idx = declared.get(i + shift).copied();
                    let kind = expected_type_idx
                        .map(|ty| bytecode.types[ty].kind)
                        .unwrap_or(hl::hl_type_kind_HDYN);
                    if env_flag!("ASH_DBG_FIBER") {
                        let raw_kind = if raw.is_null() || (*raw).t.is_null() {
                            None
                        } else {
                            Some((*(*raw).t).kind)
                        };
                        eprintln!(
                            "[fiber-runner] arg={i} raw={raw:p} raw_kind={raw_kind:?} expected={kind}"
                        );
                    }
                    let value = if kind == hl::hl_type_kind_HNULL && !raw.is_null() {
                        // Nullable parameters consume the vdynamic box itself;
                        // its nullness is the default-argument discriminator.
                        NanBoxedValue::from_ptr(raw as usize)
                    } else if kind == hl::hl_type_kind_HREF && !raw.is_null() {
                        if !(*raw).t.is_null() && (*(*raw).t).kind == hl::hl_type_kind_HREF {
                            let cell = (*raw).v.ptr as usize;
                            if cell == 0 {
                                NanBoxedValue::null()
                            } else {
                                NanBoxedValue::from_ptr(cell)
                            }
                        } else {
                            let inner_kind = expected_type_idx
                                .and_then(|ty| bytecode.types[ty].tparam.as_ref())
                                .map(|ty| bytecode.types[ty.0].kind)
                                .unwrap_or(hl::hl_type_kind_HDYN);
                            let inner = interp.dynamic_to_value_for_kind(raw, inner_kind);
                            let mut cell = Box::new(0u64);
                            HLInterpreter::write_value_to_ptr(
                                (&mut *cell as *mut u64).cast::<u8>(),
                                inner,
                                inner_kind,
                            );
                            let cell_ptr = (&mut *cell as *mut u64) as usize;
                            ref_cells.push(cell);
                            NanBoxedValue::from_ptr(cell_ptr)
                        }
                    } else if kind == hl::hl_type_kind_HOBJ
                        && !raw.is_null()
                        && !interp.fn_dyn_castp.is_null()
                    {
                        // Native dynamic dispatch supplies a vdynamic*, but
                        // an HOBJ parameter needs an exact object-type cast,
                        // not merely a kind match. In particular,
                        // Reflect.callMethod can pass ArrayDyn to a method
                        // specialized for ArrayBytes<Int>; its __cast builds
                        // the representation whose field layout the callee
                        // uses. The ordinary interpreter CallMethod path does
                        // this already; the native closure runner must honor
                        // the same contract before entering compiled AIR V2.
                        let target_type = expected_type_idx
                            .map(|ty| interp.c_type_factory.get(ty))
                            .unwrap_or(std::ptr::null_mut());
                        let source_type = (*raw).t;
                        if source_type == target_type
                            || source_type.is_null()
                            || target_type.is_null()
                        {
                            NanBoxedValue::from_ptr(raw as usize)
                        } else if (*source_type).kind != hl::hl_type_kind_HOBJ {
                            interp.dynamic_to_value_for_kind(raw, kind)
                        } else {
                            type FnCastp = unsafe extern "C" fn(
                                *mut c_void,
                                *mut c_void,
                                *mut c_void,
                            )
                                -> *mut c_void;
                            let castp: FnCastp = std::mem::transmute(interp.fn_dyn_castp);
                            let mut data = raw as *mut c_void;
                            let casted = castp(
                                &mut data as *mut _ as *mut c_void,
                                source_type.cast(),
                                target_type.cast(),
                            );
                            if casted.is_null() {
                                NanBoxedValue::null()
                            } else {
                                NanBoxedValue::from_ptr(casted as usize)
                            }
                        }
                    } else {
                        interp.dynamic_to_value_for_kind(raw, kind)
                    };
                    args_v.push(value);
                }
            }
            match interp.call_function(bytecode, &*ctx.resolver, findex, &args_v) {
                Ok(v) => {
                    let ret_idx = func_of(&interp.targets, findex)
                        .and_then(|fi| {
                            bytecode.types[bytecode.functions[fi].type_.0]
                                .fun
                                .as_ref()
                                .map(|f| f.ret.0)
                        })
                        .unwrap_or(0);
                    let kind = bytecode.types[ret_idx].kind;
                    let scalar = matches!(
                        kind,
                        hl::hl_type_kind_HI32
                            | hl::hl_type_kind_HUI8
                            | hl::hl_type_kind_HUI16
                            | hl::hl_type_kind_HI64
                            | hl::hl_type_kind_HF32
                            | hl::hl_type_kind_HF64
                            | hl::hl_type_kind_HBOOL
                    );
                    // Box the result as a vdynamic* for the native caller.
                    // Thread bodies ignore it, but the virtual-dispatch
                    // fallback (hlp_vcall_virtual_hashed) needs real values —
                    // silently returning null turned hasNext() into false.
                    // Pointer-shaped return types remain raw: notably HBYTES
                    // is carried by NanBox's distinct bytes tag, so testing
                    // `is_ptr()` alone boxed an `__string` result and made the
                    // buffer interpret the vdynamic header as UTF-16.
                    if v.is_void() || v.is_null() {
                        std::ptr::null_mut()
                    } else if !scalar {
                        v.as_ptr() as *mut c_void
                    } else {
                        // Primitive: box via hlp_make_dyn with the callee's
                        // declared return type.
                        let mut raw = interp.value_to_i64(v, kind);
                        let c_t = interp.c_type_factory.get(ret_idx) as *mut c_void;
                        if interp.fn_make_dyn.is_null() || c_t.is_null() {
                            std::ptr::null_mut()
                        } else {
                            let make_dyn: unsafe extern "C" fn(
                                *mut c_void,
                                *mut c_void,
                            )
                                -> *mut c_void = std::mem::transmute(interp.fn_make_dyn);
                            make_dyn(&mut raw as *mut i64 as *mut c_void, c_t)
                        }
                    }
                }
                Err(e) => {
                    if is_fiber_root {
                        eprintln!("[ash] fiber thread uncaught exception: {:#}", e);
                        std::ptr::null_mut()
                    } else {
                        // Native virtual/dynamic helpers re-enter AIR V2
                        // through this same runner. Their call_native boundary
                        // has an HL trap armed, so preserve normal Haxe
                        // exception semantics instead of silently converting
                        // the exception to null.
                        HLInterpreter::raise_stub_bridge_failure(&*ctx.resolver, findex, e)
                    }
                }
            }
        }
        unsafe extern "C" fn fiber_switch_runner(from: u32, to: u32) {
            let Some(ctx) = (&raw const CLOSURE_RUN_CTX).as_ref().unwrap().as_ref() else {
                return;
            };
            let interp = &mut *ctx.interp;
            let outgoing = std::mem::take(&mut interp.stack);
            if !outgoing.is_empty() {
                let replaced = interp.fiber_stacks.insert(from, outgoing);
                debug_assert!(
                    replaced.is_none(),
                    "fiber {from} already had a suspended stack"
                );
            }
            interp.stack = interp.fiber_stacks.remove(&to).unwrap_or_default();
            interp.sync_gc_scan_roots();
        }
        unsafe extern "C" fn resolve_stack_symbol(
            symbol: *mut c_void,
            _buffer: *mut u8,
            buffer_len: *mut i32,
        ) -> *mut u8 {
            if symbol.is_null() {
                return std::ptr::null_mut();
            }
            let symbol = symbol.cast::<u16>();
            let mut len = 0usize;
            while *symbol.add(len) != 0 {
                len += 1;
            }
            if !buffer_len.is_null() {
                *buffer_len = len.min(i32::MAX as usize) as i32;
            }
            symbol.cast::<u8>()
        }
        unsafe extern "C" fn capture_stack_runner(output: *mut *mut c_void, capacity: i32) -> i32 {
            let Some(ctx) = (&raw const CLOSURE_RUN_CTX).as_ref().unwrap().as_ref() else {
                return 0;
            };
            // `prepare_call_stack` owns scratch Vecs on the one
            // `HLInterpreter`. Compiled AIR V2 workers have native frames and
            // may throw concurrently, so touching that main-lane scratch
            // storage here races both the interpreter and other workers.
            // Returning an empty interpreted stack is correct for this lane;
            // the native trap/JIT frames remain available to the ordinary
            // exception machinery.
            if !ctx.fiber_is_worker_lane.is_null() {
                let is_worker: unsafe extern "C" fn() -> bool =
                    std::mem::transmute(ctx.fiber_is_worker_lane);
                if is_worker() {
                    return 0;
                }
            }
            let interp = &mut *ctx.interp;
            if output.is_null() {
                let frame_hint = (&*ctx.resolver)
                    .resolve_function("std", "hlp_call_stack_frame")
                    .ok()
                    .filter(|address| !address.is_null())
                    .map_or(std::ptr::null(), |address| {
                        let get_frame: unsafe extern "C" fn() -> *const usize =
                            std::mem::transmute(address);
                        get_frame()
                    });
                interp.prepare_call_stack(&*ctx.bytecode, frame_hint) as i32
            } else {
                interp.write_call_stack(output, capacity)
            }
        }
        unsafe {
            let worker_compilation = self.tiered_runtime.is_some();
            let fiber_is_root_closure = native_resolver
                .resolve_function("std", "hlp_fiber_is_root_closure")
                .unwrap_or(std::ptr::null_mut());
            let fiber_is_worker_lane = native_resolver
                .resolve_function("std", "hlp_fiber_is_worker_lane")
                .unwrap_or(std::ptr::null_mut());
            let jit_closure_runner = native_resolver
                .resolve_function("std", "hlp_jit_closure_runner")
                .unwrap_or(std::ptr::null_mut());
            CLOSURE_RUN_CTX = Some(ClosureRunCtx {
                interp: self as *mut _,
                bytecode: bytecode as *const _,
                resolver: native_resolver as *const _,
                fiber_is_root_closure,
                fiber_is_worker_lane,
                jit_closure_runner,
                compiled_stub_ctx: self
                    .tiered_runtime
                    .as_ref()
                    .map(|tiered| Arc::clone(&tiered.shared_ctx)),
            });
            let set = native_resolver
                .resolve_function("std", "hlp_set_closure_runner")
                .unwrap_or(std::ptr::null_mut());
            if !set.is_null() {
                type SetRunner = unsafe extern "C" fn(
                    unsafe extern "C" fn(*mut c_void, *mut *mut c_void, i32) -> *mut c_void,
                );
                let f: SetRunner = std::mem::transmute(set);
                f(fiber_closure_runner);
            }
            let set_switch = native_resolver
                .resolve_function("std", "hlp_set_fiber_switch_hook")
                .unwrap_or(std::ptr::null_mut());
            if !set_switch.is_null() {
                type SetSwitch = unsafe extern "C" fn(unsafe extern "C" fn(u32, u32));
                let f: SetSwitch = std::mem::transmute(set_switch);
                f(fiber_switch_runner);
            }
            let set_worker_mode = native_resolver
                .resolve_function("std", "hlp_set_compiled_worker_mode")
                .unwrap_or(std::ptr::null_mut());
            if !set_worker_mode.is_null() {
                let set_mode: unsafe extern "C" fn(bool) = std::mem::transmute(set_worker_mode);
                set_mode(worker_compilation);
            }
            let set_stub_resolver = native_resolver
                .resolve_function("std", "hlp_set_stub_resolver")
                .unwrap_or(std::ptr::null_mut());
            if !set_stub_resolver.is_null() {
                let set_resolver: unsafe extern "C" fn(unsafe extern "C" fn(i32) -> *mut ()) =
                    std::mem::transmute(set_stub_resolver);
                set_resolver(jit_stub_resolver);
            }
            let setup_exception = native_resolver
                .resolve_function("std", "hlp_setup_exception")
                .unwrap_or(std::ptr::null_mut());
            if !setup_exception.is_null() {
                type SetupException = unsafe extern "C" fn(
                    unsafe extern "C" fn(*mut c_void, *mut u8, *mut i32) -> *mut u8,
                    unsafe extern "C" fn(*mut *mut c_void, i32) -> i32,
                );
                let setup: SetupException = std::mem::transmute(setup_exception);
                setup(resolve_stack_symbol, capture_stack_runner);
            }
        }

        // Register the JIT stub-call bridge: tiered/promoted code guards every
        // indirect call against interpreter stub sentinels (findex+1) left in
        // shared functions_ptrs/vtable/closure slots and re-enters the
        // interpreter through this bridge instead of SIGBUSing on them.
        // Args/result are raw i64 words per the callee's declared bytecode
        // signature (see ash_core::llvm::stub_bridge for the encoding contract).
        // Same raw-pointer-context justification as the closure runner above:
        // JIT code only runs within execute_entrypoint's dynamic extent, on
        // this OS thread.
        unsafe extern "C" fn jit_stub_resolver(findex: i32) -> *mut () {
            if findex < 0 {
                return std::ptr::null_mut();
            }
            let Some(ctx) = (&raw const CLOSURE_RUN_CTX).as_ref().unwrap().as_ref() else {
                return std::ptr::null_mut();
            };
            let Some(shared) = ctx.compiled_stub_ctx.as_ref() else {
                return std::ptr::null_mut();
            };
            // The mutator must not park behind a broker's compile. A worker
            // lane has no interpreter to fall back to, so it still waits.
            let may_block = if ctx.fiber_is_worker_lane.is_null() {
                false
            } else {
                let is_worker: unsafe extern "C" fn() -> bool =
                    std::mem::transmute(ctx.fiber_is_worker_lane);
                is_worker()
            };
            resolve_worker_stub(shared, findex as usize, may_block)
        }

        unsafe extern "C" fn jit_stub_call_bridge(
            findex: i32,
            caller_findex: i32,
            args: *const i64,
            nargs: i32,
        ) -> i64 {
            let Some(ctx) = (&raw const CLOSURE_RUN_CTX).as_ref().unwrap().as_ref() else {
                // Without the context there is no resolver to throw through
                // and no interpreter to run the callee: the only honest
                // outcomes are abort or a poison return, and a poison return
                // is a delayed crash somewhere else.
                eprintln!(
                    "[ash] FATAL: stub bridge called for findex {} with no interpreter \
                     context registered; aborting rather than returning a poison value",
                    findex
                );
                std::process::abort();
            };
            if !ctx.fiber_is_worker_lane.is_null() {
                let is_worker: unsafe extern "C" fn() -> bool =
                    std::mem::transmute(ctx.fiber_is_worker_lane);
                if is_worker() {
                    HLInterpreter::raise_stub_bridge_failure(
                        &*ctx.resolver,
                        findex.max(0) as usize,
                        anyhow!(
                            "compiled worker reached unprepared JIT sentinel for findex {}",
                            findex
                        ),
                    );
                }
            }
            let interp = &mut *ctx.interp;
            let bytecode = &*ctx.bytecode;
            let resolver = &*ctx.resolver;
            let findex = findex as usize;

            // The callee's declared signature drives raw-word decoding.
            let type_idx = if let Some(fi) = func_of(&interp.targets, findex) {
                bytecode.functions[fi].type_.0
            } else if let Some(ni) = native_of(&interp.targets, findex) {
                bytecode.natives[ni].type_.0
            } else {
                HLInterpreter::raise_stub_bridge_failure(
                    resolver,
                    findex,
                    anyhow!("stub bridge: unknown findex {}", findex),
                );
            };
            let Some(fun) = bytecode.types[type_idx].fun.as_ref() else {
                HLInterpreter::raise_stub_bridge_failure(
                    resolver,
                    findex,
                    anyhow!("stub bridge: findex {} has no function type", findex),
                );
            };

            let nargs = nargs.max(0) as usize;
            let raw_args: &[i64] = if nargs == 0 || args.is_null() {
                &[]
            } else {
                std::slice::from_raw_parts(args, nargs)
            };
            let mut vals: Vec<NanBoxedValue> = Vec::with_capacity(raw_args.len());
            for (i, &raw) in raw_args.iter().enumerate() {
                let kind = fun
                    .args
                    .get(i)
                    .map(|a| bytecode.types[a.0].kind)
                    .unwrap_or(hl::hl_type_kind_HDYN);
                vals.push(interp.wrap_native_result(raw, kind));
            }
            let ret_kind = bytecode.types[fun.ret.0].kind;

            let caller = usize::try_from(caller_findex)
                .ok()
                .and_then(|caller| func_of(&interp.targets, caller));
            if let Some(caller) = caller {
                interp.jit_bridge_callers.push(caller);
            }
            let result = interp.call_function(bytecode, resolver, findex, &vals);
            if caller.is_some() {
                interp.jit_bridge_callers.pop();
            }

            match result {
                Ok(v) => {
                    // The bridge is the inverse of `call_compiled_function`:
                    // interpreter Dynamic registers may carry primitives
                    // inline, but compiled AIR V2 consumes and returns a
                    // `vdynamic*`. Returning integer 2 as word 0x2 makes the
                    // first compiled SafeCast dereference address 0x2.
                    let v = if matches!(
                        ret_kind,
                        hl::hl_type_kind_HDYN
                            | hl::hl_type_kind_HNULL
                            | hl::hl_type_kind_HDYNOBJ
                    ) {
                        interp.box_for_compiled_dynamic_value(v)
                    } else {
                        v
                    };
                    if std::env::var_os("ASH_DBG_STUB").is_some() {
                        eprintln!(
                            "[stub] call findex={findex} ret_kind={ret_kind} value={v:?}"
                        );
                    }
                    interp.value_to_i64(v, ret_kind)
                }
                // Every failure leaves through the native trap chain — see
                // `raise_stub_bridge_failure`. Returning a value here would
                // hand compiled code a word it is about to use as a pointer.
                Err(e) => HLInterpreter::raise_stub_bridge_failure(resolver, findex, e),
            }
        }
        ash_core::llvm::stub_bridge::set_stub_resolver(jit_stub_resolver);
        ash_core::llvm::stub_bridge::set_stub_call_bridge(jit_stub_call_bridge);

        let entry_findex = bytecode.entrypoint as usize;
        let result = self.call_function(bytecode, native_resolver, entry_findex, &[]);

        // After main() returns, run the VM event loop.
        // haxe.MainLoop registers its tick function via hlp_sys_set_loop.
        // The HashLink VM calls this in a loop after the entrypoint returns.
        // This is what drives callbacks, timers, and the render loop.
        let get_loop = native_resolver
            .resolve_function("std", "hlp_sys_get_loop")
            .unwrap_or(std::ptr::null_mut());
        // Diagnostic, not output. It was unconditional, so every run printed a
        // pointer to stderr -- which the parity harness compares against an
        // oracle that will never print it.
        if env_flag!("ASH_TRACE_NATIVE") {
            eprintln!("[ash] Post-main: hlp_sys_get_loop={:p}", get_loop);
        }
        if !get_loop.is_null() {
            type FnGetLoop = unsafe extern "C" fn() -> *mut c_void;
            let get: FnGetLoop = unsafe { std::mem::transmute(get_loop) };
            let loop_fn = unsafe { get() };
            if !loop_fn.is_null() {
                // The loop function is a vclosure — extract findex from stub pointer
                let cl = loop_fn as *const hl::_vclosure;
                let loop_fun = unsafe { (*cl).fun as usize };
                let findex =
                    if (loop_fun as u64) < ash_core::llvm::stub_bridge::STUB_SENTINEL_LIMIT {
                        loop_fun.wrapping_sub(1)
                    } else {
                        // `hlp_sys_set_loop` was called from compiled code, so
                        // the closure carries a real entry address.
                        match self.findex_for_code_addr(loop_fun) {
                            Some(fi) => fi,
                            None => {
                                return Err(anyhow!(
                                    "VM event loop closure has an unknown compiled target"
                                ))
                            }
                        }
                    };
                let bound = unsafe {
                    if (*cl).hasValue != 0 && !(*cl).value.is_null() {
                        Some(NanBoxedValue::from_ptr((*cl).value as usize))
                    } else {
                        None
                    }
                };
                eprintln!("[ash] Entering VM event loop (findex={})", findex);
                // SDL pumping + frame pacing lives in the stdlib (lock_wait is a
                // pure counter check per HashLink !HL_THREADS semantics), so the
                // VM loop drives it: one pump + ~16ms sleep per tick.
                let pump = native_resolver
                    .resolve_function("std", "hlp_pump_and_sleep")
                    .unwrap_or(std::ptr::null_mut());
                type FnPump = unsafe extern "C" fn();
                loop {
                    let mut args = Vec::new();
                    if let Some(v) = bound {
                        args.push(v);
                    }
                    // The callee has copied the arguments into its own
                    // registers by the time this returns, so the buffer can go
                    // back before the result is even examined.
                    let call_result = self.call_function(bytecode, native_resolver, findex, &args);
                    if self.arg_pool.len() < POOL_CAP {
                        args.clear();
                        self.arg_pool.push(args);
                    }
                    match call_result {
                        Ok(_) => {}
                        Err(e) => {
                            eprintln!("[ash] VM event loop error: {:#}", e);
                            break;
                        }
                    }
                    if !pump.is_null() {
                        unsafe {
                            let f: FnPump = std::mem::transmute(pump);
                            f();
                        }
                    }
                }
            }
        }

        result
    }

    /// Initialize bytecode constants into the globals array.
    /// Constants are pre-allocated global type singletons that must be
    /// initialized before the entrypoint runs.
    fn init_constants(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
    ) -> Result<()> {
        if bytecode.constants.is_empty() {
            return Ok(());
        }

        // Resolve hlp_alloc_closure_void for function-typed constant fields
        let fn_alloc_closure_void = native_resolver
            .resolve_function("std", "hlp_alloc_closure_void")
            .unwrap_or(std::ptr::null_mut());

        for constant in &bytecode.constants {
            let global_idx = constant.global as usize;
            if global_idx >= bytecode.globals.len() {
                continue;
            }

            let type_idx = bytecode.globals[global_idx].0;
            let hl_type_rust = &bytecode.types[type_idx];
            let c_type_ptr = self.c_type_factory.get(type_idx);

            if c_type_ptr.is_null() {
                continue;
            }

            let kind = hl_type_rust.kind;

            if kind == hl::hl_type_kind_HOBJ || kind == hl::hl_type_kind_HSTRUCT {
                let obj_data = match hl_type_rust.obj.as_ref() {
                    Some(o) => o,
                    None => continue,
                };

                // Allocate the object via hlp_alloc_obj
                let alloc_fn = self.fn_alloc_obj;
                if alloc_fn.is_null() {
                    continue;
                }

                let f: FnAllocObj = unsafe { std::mem::transmute(alloc_fn) };
                let obj_ptr = unsafe { f(c_type_ptr as *mut c_void) };
                if obj_ptr.is_null() {
                    continue;
                }

                // Store in globals — BOTH stores, like the SetGlobal opcode.
                //
                // `self.globals` is a Rust Vec: malloc memory the conservative
                // scanner never sees. `globals_data` is the C array registered
                // as a GC root. A constant written only to the Vec — which is
                // what this did, with a comment warning about descriptor
                // slots that actually concerns `(*obj).global_value`, a
                // DIFFERENT pointer — has no root at all: it survives only
                // while some scanned register happens to reference it, and
                // any collection in a gap reclaims it. That was this
                // codebase's oldest intermittent corruption: string constants
                // (every string literal is one) dying under ASH_GC_STRESS,
                // and the map tests' nondeterminism reading freed keys.
                self.globals[global_idx] = NanBoxedValue::from_ptr(obj_ptr as usize);
                {
                    let (gd, nglobals) = self.c_type_factory.globals_data();
                    if !gd.is_null() && global_idx < nglobals {
                        unsafe { *gd.add(global_idx) = obj_ptr };
                    }
                }

                // Update the global_value slot ONLY when this constant IS the class descriptor
                // for its type (i.e., global_idx == type.obj.global_value - 1).
                // Do NOT update for regular instances (e.g., String constants), which would
                // overwrite the class descriptor slot with a plain data object.
                unsafe {
                    let obj = (*c_type_ptr).__bindgen_anon_1.obj;
                    if !obj.is_null() && !(*obj).global_value.is_null() {
                        let (gd, _) = self.c_type_factory.globals_data();
                        let slot_offset = (*obj).global_value.offset_from(gd as *const *mut c_void);
                        if slot_offset >= 0 && slot_offset as usize == global_idx {
                            *(*obj).global_value = obj_ptr;
                        }
                    }
                }

                // Get runtime object to compute field offsets
                let get_rt = self.fn_get_obj_rt;
                if get_rt.is_null() || constant.fields.is_empty() {
                    continue;
                }

                let rt = unsafe {
                    let get_rt_fn: FnGetObjRt = std::mem::transmute(get_rt);
                    get_rt_fn(c_type_ptr as *mut c_void) as *const hl_runtime_obj
                };

                if rt.is_null() {
                    continue;
                }

                // Calculate field start offset (skip parent fields)
                let start = unsafe { (*rt).nfields as usize - obj_data.fields.len() };

                // Get module context for function pointer stubs
                let module_ctx = self.c_type_factory.module_ctx();

                // Fill in constant fields
                for (j, &field_value) in constant.fields.iter().enumerate() {
                    if j >= obj_data.fields.len() {
                        break;
                    }

                    let field_type_idx = obj_data.fields[j].type_.0;
                    let field_kind = bytecode.types[field_type_idx].kind;
                    let field_c_type = self.c_type_factory.get(field_type_idx);

                    let field_offset = unsafe { *(*rt).fields_indexes.add(j + start) };
                    let field_addr = unsafe { (obj_ptr as *mut u8).add(field_offset as usize) };

                    match field_kind {
                        hl::hl_type_kind_HFUN | hl::hl_type_kind_HMETHOD => {
                            // field_value is a findex - create closure
                            if !fn_alloc_closure_void.is_null() {
                                let findex = field_value as usize;
                                let func_ptr = unsafe {
                                    if !module_ctx.is_null()
                                        && !(*module_ctx).functions_ptrs.is_null()
                                    {
                                        *(*module_ctx).functions_ptrs.add(findex)
                                    } else {
                                        (findex + 1) as *mut c_void
                                    }
                                };

                                let alloc_cl: FnAllocClosureVoid =
                                    unsafe { std::mem::transmute(fn_alloc_closure_void) };
                                let closure =
                                    unsafe { alloc_cl(field_c_type as *mut c_void, func_ptr) };
                                if !closure.is_null() {
                                    unsafe {
                                        *(field_addr as *mut *mut c_void) = closure;
                                    }
                                }
                            }
                        }
                        k if k == hl::hl_type_kind_HOBJ || k == hl::hl_type_kind_HSTRUCT => {
                            // field_value is a global index reference
                            let ref_global = field_value as usize;
                            if ref_global < self.globals.len() {
                                let ref_val = self.globals[ref_global];
                                unsafe {
                                    *(field_addr as *mut usize) = if ref_val.is_null() {
                                        0
                                    } else {
                                        ref_val.as_ptr()
                                    };
                                }
                            }
                        }
                        hl::hl_type_kind_HBYTES => {
                            // field_value is a string index → convert to UTF-16 pointer
                            let str_idx = field_value as usize;
                            if let Some(ptr) = self.intern_utf16_string(bytecode, str_idx) {
                                unsafe {
                                    *(field_addr as *mut *const u16) = ptr;
                                }
                            }
                        }
                        hl::hl_type_kind_HTYPE => {
                            // field_value is a type index → store c_type pointer (8 bytes)
                            let type_ptr = self.c_type_factory.get(field_value as usize);
                            unsafe {
                                *(field_addr as *mut usize) = type_ptr as usize;
                            }
                        }
                        hl::hl_type_kind_HI32
                        | hl::hl_type_kind_HBOOL
                        | hl::hl_type_kind_HUI8
                        | hl::hl_type_kind_HUI16 => {
                            // field_value is an index into ints table
                            let int_val = bytecode
                                .ints
                                .get(field_value as usize)
                                .copied()
                                .unwrap_or(field_value);
                            unsafe {
                                *(field_addr as *mut i32) = int_val;
                            }
                        }
                        hl::hl_type_kind_HI64 => {
                            let int_val = bytecode
                                .ints
                                .get(field_value as usize)
                                .copied()
                                .unwrap_or(field_value);
                            unsafe {
                                *(field_addr as *mut i64) = int_val as i64;
                            }
                        }
                        hl::hl_type_kind_HF64 => {
                            let float_val = bytecode
                                .floats
                                .get(field_value as usize)
                                .copied()
                                .unwrap_or(0.0);
                            unsafe {
                                *(field_addr as *mut f64) = float_val;
                            }
                        }
                        hl::hl_type_kind_HF32 => {
                            let float_val = bytecode
                                .floats
                                .get(field_value as usize)
                                .copied()
                                .unwrap_or(0.0);
                            unsafe {
                                *(field_addr as *mut f32) = float_val as f32;
                            }
                        }
                        _ => {
                            // All other types are pointer-like (HARRAY, HDYN, HENUM, etc.)
                            // field_value=0 means null; otherwise it's a global index.
                            if field_value == 0 {
                                unsafe {
                                    *(field_addr as *mut usize) = 0;
                                }
                            } else {
                                let ref_global = field_value as usize;
                                if ref_global < self.globals.len() {
                                    let ref_val = self.globals[ref_global];
                                    unsafe {
                                        *(field_addr as *mut usize) = if ref_val.is_null() {
                                            0
                                        } else {
                                            ref_val.as_ptr()
                                        };
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // Reconcile the two global stores into one view.
        //
        // There are two: `self.globals`, which the interpreter reads first, and
        // `globals_data`, the flat C array that native and *compiled* code
        // read. Neither was complete. Native code writes global_value slots
        // during binding setup that the interpreter never saw, and the constant
        // loop above writes `self.globals` for every constant but `globals_data`
        // only for class descriptors -- so a plain constant like the "\n" that
        // `Sys.println` appends lived in one store and was null in the other.
        //
        // The interpreter never noticed, because it falls back to `globals_data`
        // only when its own slot is null. Compiled code has no such fallback:
        // Cranelift's GetGlobal loads the `globals_data` slot address directly,
        // so it read null where the interpreter read a string. That is what kept
        // the tier's field-access gate shut -- the defect was never in the field
        // offsets, it was that admitting Field made these functions eligible at
        // all.
        //
        // Whichever store has a value wins; a slot set in both is left alone.
        let (gd, nglobals) = self.c_type_factory.globals_data();
        if !gd.is_null() {
            let n = nglobals.min(self.globals.len());
            for (i, slot) in self.globals.iter_mut().enumerate().take(n) {
                let raw = unsafe { *gd.add(i) };
                if !raw.is_null() && slot.is_null() {
                    *slot = NanBoxedValue::from_ptr(raw as usize);
                }
            }
            for (i, slot) in self.globals.iter().enumerate().take(n) {
                if unsafe { *gd.add(i) }.is_null() && !slot.is_null() && !slot.is_void() {
                    unsafe { *gd.add(i) = slot.as_ptr() as *mut c_void };
                }
            }
        }

        Ok(())
    }

    /// Call a function by its findex.
    /// Record that `func_idx` has a loop whose header is `header_pc` and which
    /// has gone round enough times to be worth compiling.
    ///
    /// Promotion counts calls, so this is the only signal that reaches a
    /// function invoked once. nbody's `main` is the case: its ten million
    /// iterations live in a single invocation, and the profiler charges 22.7%
    /// of the whole run to `call_function` -- the boundary crossings from that
    /// interpreted loop into compiled `advance`.
    /// Feed a hot loop's back-edges into the ordinary promotion machinery.
    ///
    /// This is the whole of the interpreter's part in OSR. Promotion counts
    /// invocations, so a loop inside a function called once never crosses a
    /// threshold however long it runs -- nbody's `main` is ten million
    /// iterations of a single call. Ticking the bead from a back-edge makes it
    /// promotable on the same terms as any other function: through the ladder,
    /// Cranelift and then LLVM, compiled on the broker.
    ///
    /// It deliberately does not compile anything itself. An earlier version
    /// did, and that was the mistake: OSR is not a way to get code compiled,
    /// it is a way for a frame that is already running to pick up code the
    /// tiering has already produced. Compiling here bypassed the ladder,
    /// ignored `--jit-tier`, blocked the main thread, and left no path to tier
    /// up afterwards -- one error with four symptoms.
    fn note_hot_loop(&mut self, bytecode: &DecodedBytecode, func_idx: usize, header_pc: usize) {
        let findex = self.bytecode_findex_of(bytecode, func_idx);

        // Publish the header before ticking: a tick can be the one that
        // submits the LLVM compile, and the broker reads this map when that
        // compile finishes to know which entries to build.
        if self.hot_loops.insert((findex, header_pc)) {
            if let Some(t) = self.tiered_runtime.as_ref() {
                t.shared_ctx
                    .hot_loop_pcs
                    .lock()
                    .expect("hot_loop_pcs mutex poisoned")
                    .entry(findex)
                    .or_default()
                    .push(header_pc);
            }
            self.report_hot_loop(bytecode, func_idx, findex, header_pc);
            self.late_osr_entry(bytecode, func_idx, findex, header_pc);
        }

        // Tick the bead. The returned entry is the function's *normal* entry
        // point and is deliberately dropped: calling it would restart the
        // function from the top, which is the one thing a mid-loop transfer
        // must not do. Only the tick matters here.
        let _ = self.tiered_on_invoke(bytecode, findex, func_idx);
    }

    /// Build and attach ONE OSR entry for a header that turned hot after its
    /// function was already LLVM-promoted.
    ///
    /// The promote-time batch covers headers probed before the code landed;
    /// this covers the other order, which is the common one for the loop
    /// that matters — nbody's probes that drove promotion came from short
    /// init loops, and the 10M-iteration loop was reached only afterwards.
    ///
    /// This is a synchronous LLVM compile on the main thread — the thing the
    /// tiering otherwise never does — accepted here because it is one entry,
    /// once per truly-hot header, for a loop that is by definition still
    /// running: ~80ms of compile against seconds of remaining
    /// interpretation. It does not compile the FUNCTION (the ladder already
    /// did, on the broker); it builds the door into code that already
    /// exists.
    fn late_osr_entry(
        &mut self,
        bytecode: &DecodedBytecode,
        func_idx: usize,
        findex: usize,
        header_pc: usize,
    ) {
        if !osr_transfer_enabled() {
            return;
        }
        let Some(tiered) = self.tiered_runtime.as_ref() else {
            return;
        };
        let ctx = Arc::clone(&tiered.shared_ctx);
        // Some tier must have installed code for this findex — an entry is a
        // door into code that already exists.
        let Some(addr) = self
            .tiered_runtime
            .as_ref()
            .and_then(|t| t.entries.get(findex))
            .and_then(|e| e.as_ref())
            .map(|e| e.fn_addr)
        else {
            return;
        };
        let llvm_installed = ctx
            .llvm_done
            .lock()
            .expect("llvm_done mutex poisoned")
            .contains(&findex);
        if !ash_core::air_pipeline::air_enabled() {
            return; // raw-opcode mode has no block_pcs to validate against
        }

        let raw = &bytecode.functions[func_idx];
        let m = ash_core::air_pipeline::AshModule::new(bytecode);
        let Ok(opt) = ash_core::air_pipeline::optimized(&m, raw) else {
            return;
        };
        let plan = ash_core::osr::analyze(&opt.ir);
        let eligible = plan
            .entry_headers
            .iter()
            .any(|&h| opt.ser.block_pcs.get(h as usize) == Some(&header_pc));
        if !eligible {
            return;
        }

        // Prefer the fast door. A Cranelift entry costs ~1ms — cheap enough
        // to build synchronously right here, which matters because this runs
        // on the MAIN thread mid-loop. The LLVM entry is a promote-sized
        // compile and is only worth that stall when LLVM code is what is
        // installed; and even then, never while a broker holds the module
        // lock — `try_lock`, and a miss just retries on a later probe.
        let entry_addr: u64 = if !llvm_installed {
            let cl = ctx.cranelift.lock().expect("cranelift mutex poisoned");
            let Some(tier) = cl.as_ref().and_then(|o| o.as_ref()).cloned() else {
                return;
            };
            drop(cl);
            let Some(bound) = self
                .tiered_runtime
                .as_ref()
                .and_then(|t| t.beads.get(findex))
                .and_then(|b| b.as_ref())
            else {
                return;
            };
            match ash_core::cranelift::codegen::compile_osr_entry(
                &tier.backend,
                &tier.ctx,
                bound.bead(),
                findex,
                &opt,
                header_pc,
            ) {
                Ok(a) => {
                    ash_core::profile::register_jit_code(
                        findex as u32,
                        ash_core::profile::Tier::Cranelift,
                        a,
                    );
                    a as u64
                }
                Err(e) => {
                    if osr_logging() {
                        eprintln!(
                            "[osr] late cranelift entry declined findex={findex} pc={header_pc}: {e:#}"
                        );
                    }
                    return;
                }
            }
        } else if !late_llvm_osr_enabled() {
            // The LLVM entry for a late header is an UPGRADE, not a rescue:
            // the Cranelift door above already took the frame out of the
            // interpreter, and entries for headers probed before the promote
            // are built ahead of it now. Paying a promote-sized compile on the
            // mutator to upgrade a frame that is already in compiled code cost
            // bench_binary_trees 42ms of a 442ms run -- and buying it back is
            // worth 10.3% there, with every other benchmark inside 0.7%.
            // `ASH_LATE_LLVM_OSR=1` restores it for a program that turns a
            // header hot long after its promote and runs there for a while.
            return;
        } else {
            let Ok(mut guard) = ctx.llvm.try_lock() else {
                // A broker is compiling; blocking the interpreter behind it
                // was 11.5% of nbody's execute. The header stays in
                // `hot_loops`... which would stop this from retrying, so put
                // it back on the retry path by forgetting it was seen.
                self.hot_loops.remove(&(findex, header_pc));
                return;
            };
            let LlvmState::Ready(module) = &mut *guard else {
                return;
            };
            match module.0.compile_osr_entry(findex, header_pc, &opt) {
                Ok(a) if a != 0 => a,
                Ok(_) => return,
                Err(e) => {
                    if osr_logging() {
                        eprintln!(
                            "[osr] late entry compile failed findex={findex} pc={header_pc}: {e:#}"
                        );
                    }
                    return;
                }
            }
        };

        // Incremental attach: the swap replaces the whole table, so resend
        // what is already installed plus the new entry.
        let mut entries = self.osr_attached.get(&findex).cloned().unwrap_or_default();
        entries.push(OsrEntry {
            site: header_pc as u64,
            code: entry_addr as *mut (),
        });
        let Some(bound) = self
            .tiered_runtime
            .as_ref()
            .and_then(|t| t.beads.get(findex))
            .and_then(|b| b.as_ref())
        else {
            return;
        };
        if bound
            .bead()
            .swap_compiled_with_osr(addr as *mut (), entries.clone())
            .is_some()
        {
            self.osr_attached.insert(findex, entries);
            if osr_logging() {
                eprintln!("[osr] late-attached entry findex={findex} pc={header_pc}");
            }
        }
    }

    /// The once-per-header eligibility log, split out of the probe path.
    fn report_hot_loop(
        &self,
        bytecode: &DecodedBytecode,
        func_idx: usize,
        findex: usize,
        header_pc: usize,
    ) {
        // `osr::analyze` answers from the IR whether a transfer may enter at
        // all: a live trap region or a register whose address escaped means no.
        if osr_logging() {
            let m = ash_core::air_pipeline::AshModule::new(bytecode);
            let opts = ash_core::air_pipeline::AirPassOptions::default();
            let plan = ash_core::air_pipeline::prepare_ir(
                &m,
                &bytecode.functions[func_idx],
                ash_core::air_pipeline::AirOptLevel::O2,
                &opts,
            )
            .ok()
            .map(|(f, _)| ash_core::osr::analyze(&f));
            match &plan {
                Some(p) if p.eligible() => {
                    eprintln!("[osr] hot loop findex={findex} pc={header_pc} ELIGIBLE")
                }
                Some(p) => eprintln!(
                    "[osr] hot loop findex={findex} pc={header_pc} refused: {:?}",
                    p.refusals
                ),
                None => eprintln!("[osr] hot loop findex={findex} pc={header_pc} refused: no IR"),
            }
        }
    }

    /// Take an OSR entry if the tiering has installed one for this header.
    ///
    /// The entry is looked up on the bead, not compiled here: `Bead::osr_entry`
    /// is a binary search over the table a tier-up installed alongside its
    /// code. beadie only lets entries be attached by `swap_compiled_with_osr`
    /// -- the tier-up path -- and not by the first install, which is the
    /// library saying that OSR belongs between tiers rather than in front of
    /// them.
    ///
    /// Returns the function's result, because the compiled code runs the loop
    /// and everything after it; control does not come back for this
    /// invocation. `None` means no entry is installed yet and the interpreter
    /// should carry on.
    fn try_osr_transfer(
        &mut self,
        bytecode: &DecodedBytecode,
        func_idx: usize,
        header_pc: usize,
    ) -> Result<Option<NanBoxedValue>> {
        if !osr_transfer_enabled() {
            return Ok(None);
        }
        let findex = self.bytecode_findex_of(bytecode, func_idx);
        let site = header_pc as u64;
        let addr = {
            let tiered = self.tiered_runtime.as_ref();
            let Some(bound) = tiered
                .and_then(|t| t.beads.get(findex))
                .and_then(|b| b.as_ref())
            else {
                return Ok(None);
            };
            match bound.bead().osr_entry(site) {
                Some(p) if !p.is_null() => p as u64,
                _ => return Ok(None),
            }
        };

        let body = self.air.body(bytecode, func_idx);
        let Some(fun_ty) = bytecode.types[body.type_.0].fun.as_ref() else {
            return Ok(None);
        };
        let ret_kind = bytecode.types[fun_ty.ret.0].kind;

        let mut buf: Vec<u64> = Vec::with_capacity(body.regs.len());
        {
            let frame = self.stack.last().expect("frame for the running function");
            for (i, reg) in body.regs.iter().enumerate() {
                let kind = bytecode.types[reg.0].kind;
                let v = frame.registers.get(i as u32);
                buf.push(self.value_to_i64(v, kind) as u64);
            }
        }
        if osr_logging() {
            eprintln!(
                "[osr] entering findex={findex} pc={header_pc} regs={} at {addr:#x}",
                buf.len()
            );
        }

        // A frame stepping through the fast door is the loudest demand signal
        // the ladder gets, and the last one this thread will ever emit for
        // this findex: the transfer ends the interpreted ticks that counter-
        // based proposal runs on, so whether tier 1 was already proposed is a
        // race between the count reaching threshold and the cranelift install
        // -- won locally, lost on CI, which is the whole closure_call mode
        // split (llvm=2 at ~175ms against llvm=1 at ~317ms). Propose it here,
        // explicitly. `force_promote`'s queued CAS makes this a no-op when
        // the counter already got there, and `llvm_done` skips it once the
        // top tier is installed.
        if !self.osr_forced.contains(&findex) {
            self.osr_forced.insert(findex);
            if let Some(t) = self.tiered_runtime.as_ref() {
                let already_llvm = t
                    .shared_ctx
                    .llvm_done
                    .lock()
                    .expect("llvm_done mutex poisoned")
                    .contains(&findex);
                if !already_llvm {
                    if let Some(bound) = t.beads.get(findex).and_then(|b| b.as_ref()) {
                        let ctx = Arc::clone(&t.shared_ctx);
                        let submitted = t.adapter.force_promote(bound, 1, move |b| {
                            tiered_compile_tier(&ctx, 1, findex, b, true)
                        });
                        if submitted && t.shared_ctx.tier_log {
                            eprintln!("[tier] osr-transfer proposes findex={findex} tier=llvm");
                        }
                    }
                }
            }
        }

        // Armed exactly as the ordinary call boundary arms one: the compiled
        // code can call something that throws, and a throw crossing this frame
        // needs a live setjmp landing point here.
        let stack_depth = self.stack.len();
        let fn_setup_trap = self.fn_setup_trap_jit;
        let fn_remove_trap = self.fn_remove_trap_jit;
        let mut raw = None;
        let jumped = run_with_hl_trap(fn_setup_trap, fn_remove_trap, || {
            raw = Some(unsafe {
                if matches!(ret_kind, hl::hl_type_kind_HF32 | hl::hl_type_kind_HF64) {
                    type FnF64 = unsafe extern "C" fn(*mut u64) -> f64;
                    let f: FnF64 = std::mem::transmute(addr as usize);
                    f(buf.as_mut_ptr()).to_bits() as i64
                } else {
                    type FnI64 = unsafe extern "C" fn(*mut u64) -> i64;
                    let f: FnI64 = std::mem::transmute(addr as usize);
                    f(buf.as_mut_ptr())
                }
            });
        });
        if jumped != 0 {
            for f in self.stack.drain(stack_depth..) {
                if self.reg_pool.len() < POOL_CAP {
                    self.reg_pool.push(f.into_buffer());
                }
            }
            let fn_get_exc = self.fn_get_exc_value;
            let fn_clear_exc = self.fn_clear_exc_value;
            if !fn_get_exc.is_null() {
                type FnGetExc = unsafe extern "C" fn() -> *mut c_void;
                let exc_ptr =
                    unsafe { (std::mem::transmute::<*mut c_void, FnGetExc>(fn_get_exc))() };
                if !exc_ptr.is_null() {
                    // Preserve the runtime's pending-exception state while
                    // formatting. Clearing it first made the formatter's
                    // follow-up probe race the short-lived native exception
                    // on Darwin and hid the original SQLite error behind a
                    // misleading SIGSEGV.
                    let exception =
                        self.format_hl_exception(NanBoxedValue::from_ptr(exc_ptr as usize));
                    if !fn_clear_exc.is_null() {
                        type FnClearExc = unsafe extern "C" fn();
                        unsafe { (std::mem::transmute::<*mut c_void, FnClearExc>(fn_clear_exc))() };
                    }
                    return Err(anyhow::Error::new(exception));
                }
            }
            return Err(anyhow!(
                "osr transfer longjmp without exception: findex {findex}"
            ));
        }

        ash_core::profile::count("osr transfers", 1);
        let raw = raw.ok_or_else(|| anyhow!("osr trap boundary did not run findex {findex}"))?;
        Ok(Some(self.wrap_native_result(raw, ret_kind)))
    }

    /// The findex of a function by its index in `bytecode.functions`.
    ///
    /// `targets` is built as exactly this inversion, so scanning it to undo
    /// the inversion is work the bytecode already answers directly. It ran
    /// twice per 64 back-edges from note_hot_loop and try_osr_transfer — the
    /// interpreter's hottest loop path.
    fn bytecode_findex_of(&self, bytecode: &DecodedBytecode, func_idx: usize) -> usize {
        bytecode
            .functions
            .get(func_idx)
            .map(|f| f.findex as usize)
            .unwrap_or(func_idx)
    }

    /// `ASH_TIERED_SHADOW` membership, parsed once.
    fn shadow_findex(findex: usize) -> bool {
        static V: std::sync::OnceLock<Vec<usize>> = std::sync::OnceLock::new();
        V.get_or_init(|| {
            std::env::var("ASH_TIERED_SHADOW")
                .map(|v| v.split(',').filter_map(|s| s.trim().parse().ok()).collect())
                .unwrap_or_default()
        })
        .contains(&findex)
    }

    pub fn call_function(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        findex: usize,
        args: &[NanBoxedValue],
    ) -> Result<NanBoxedValue> {
        self.ensure_gc_runtime_initialized();

        // Check if it's a bytecode function or native
        if let Some(func_idx) = func_of(&self.targets, findex) {
            let compiled_only = self
                .tiered_runtime
                .as_ref()
                .is_some_and(|tiered| tiered.config.compiled_only);
            if compiled_only {
                let entry = self.compiled_only_entry(bytecode, findex, func_idx)?;
                let result = self.call_compiled_function(findex, &entry, args);
                if result.is_ok() {
                    if let Some(tiered) = self.tiered_runtime.as_mut() {
                        tiered.stats.compiled_calls += 1;
                    }
                }
                // There is deliberately no execute_hl_function fallback in
                // this mode. A lowering or invocation failure is a JIT error
                // (or a propagated Haxe exception), not permission to execute
                // a different engine.
                return result;
            }
            // Hybrid tiered call path: tick the bead and dispatch to compiled
            // code once beadie's broker has installed it.
            if self.tiered_runtime.is_some() {
                // Demand, read off the frame we are about to return into.
                // Both shapes ask the same question -- does that frame still
                // have work left? -- and both are O(1) reads of the caller,
                // taking a lock only on the first sighting of a findex.
                let caller = self.stack.last();
                let live_frame = caller.map(|fr| fr.function_index) == Some(func_idx);
                // A caller past its first HOT_LOOP_BACKEDGES iterations is one
                // the back-edge probe has already called hot; this callee is
                // part of that loop's body.
                let under_loop = caller.is_some_and(|fr| fr.backedges >= HOT_LOOP_BACKEDGES);
                if live_frame && !self.demand_seen(findex, DEMAND_LIVE_FRAME) {
                    self.note_demand(findex, DEMAND_LIVE_FRAME);
                }
                if under_loop && !self.demand_seen(findex, DEMAND_UNDER_LOOP) {
                    self.note_demand(findex, DEMAND_UNDER_LOOP);
                }
                if let Some(entry) = self.tiered_on_invoke(bytecode, findex, func_idx) {
                    // ASH_TIERED_SHADOW=1,2,3: run the listed findexes through
                    // BOTH the compiled entry and the interpreter, compare the
                    // NaN-boxed results bit for bit, and log any divergence
                    // with the argument bits. Returns the interpreter's answer
                    // so the program's continued behaviour says whether the
                    // divergence is the whole story. Only sound for functions
                    // free of side effects — it executes them twice.
                    if Self::shadow_findex(findex) {
                        let compiled = self.call_compiled_function(findex, &entry, args);
                        let interp =
                            self.execute_hl_function(bytecode, native_resolver, func_idx, args);
                        match (&compiled, &interp) {
                            (Ok(c), Ok(i)) => {
                                if c.raw_bits() != i.raw_bits() {
                                    eprintln!(
                                        "[shadow] findex={} DIVERGE compiled={:#018x} interp={:#018x} args={:?}",
                                        findex,
                                        c.raw_bits(),
                                        i.raw_bits(),
                                        args.iter().map(|a| a.raw_bits()).collect::<Vec<u64>>()
                                    );
                                }
                            }
                            _ => eprintln!(
                                "[shadow] findex={} compiled_ok={} interp_ok={}",
                                findex,
                                compiled.is_ok(),
                                interp.is_ok()
                            ),
                        }
                        return interp;
                    }
                    match self.call_compiled_function(findex, &entry, args) {
                        Ok(v) => {
                            if let Some(tiered) = self.tiered_runtime.as_mut() {
                                tiered.stats.compiled_calls += 1;
                            }
                            return Ok(v);
                        }
                        Err(e) => {
                            self.record_tiered_fallback(
                                findex,
                                format!("compiled invoke failed: {}", e),
                            );
                        }
                    }
                }
            }
            self.execute_hl_function(bytecode, native_resolver, func_idx, args)
        } else if let Some(native_idx) = native_of(&self.targets, findex) {
            self.call_native(bytecode, native_resolver, native_idx, args)
        } else {
            Err(anyhow!("Function findex {} not found", findex))
        }
    }

    /// Whether the given demand bit is already published for this findex.
    fn demand_seen(&self, findex: usize, bit: u8) -> bool {
        self.demand_local.get(findex).is_some_and(|f| f & bit != 0)
    }

    /// Publish demand bits, locally and for the broker.
    ///
    /// Each bit names its own set, so passing more than one publishes to all
    /// of them rather than silently picking a branch.
    fn note_demand(&mut self, findex: usize, bits: u8) {
        if findex >= self.demand_local.len() {
            self.demand_local.resize(findex + 1, 0);
        }
        self.demand_local[findex] |= bits;
        let Some(t) = self.tiered_runtime.as_ref() else {
            return;
        };
        for (bit, shape, set) in [
            (DEMAND_LIVE_FRAME, "live-frame", &t.shared_ctx.live_frame),
            (
                DEMAND_UNDER_LOOP,
                "caller-loop",
                &t.shared_ctx.called_from_loop,
            ),
        ] {
            if bits & bit == 0 {
                continue;
            }
            if t.shared_ctx.tier_log {
                eprintln!("[tier] demand findex={findex} shape={shape}");
            }
            set.lock().expect("demand mutex poisoned").insert(findex);
        }
    }

    /// Give cooperative Haxe worker fibers bounded execution time even when
    /// the main Haxe thread is a non-returning event loop.
    #[inline(always)]
    fn fiber_safe_point(&mut self, work: u32) {
        if self.fiber_poll_budget > work {
            self.fiber_poll_budget -= work;
            return;
        }
        self.fiber_poll_budget = FIBER_POLL_WORK;
        if self.fn_fiber_poll.is_null() {
            return;
        }
        type FnPoll = unsafe extern "C" fn();
        unsafe {
            (std::mem::transmute::<*mut c_void, FnPoll>(self.fn_fiber_poll))();
        }
    }

    /// beadie `TieredAdapter::on_invoke` semantics for one bytecode-function
    /// call: tick the bead, let the adapter submit tier-0 and tier-1 compile
    /// jobs when their policies fire, and return the currently installed code.
    ///
    /// The marshaling entry is rebuilt whenever the installed pointer changes,
    /// which is exactly how a tier-1 upgrade (`swap_compiled`) is observed —
    /// the signature itself comes from the bytecode and is tier-independent.
    /// Returns `Some(entry)` when compiled code should be dispatched, `None`
    /// to keep interpreting.
    fn tiered_on_invoke(
        &mut self,
        bytecode: &DecodedBytecode,
        findex: usize,
        func_idx: usize,
    ) -> Option<CompiledFunctionEntry> {
        // One-time registration gate: untierable findexes get no bead.
        {
            let tiered = self.tiered_runtime.as_mut()?;
            if !tiered.config.enabled {
                return None;
            }
            if findex >= tiered.beads.len() {
                tiered.beads.resize_with(findex + 1, || None);
                tiered.gate_checked.resize(findex + 1, false);
                tiered.entries.resize_with(findex + 1, || None);
                tiered.sigs.resize_with(findex + 1, || None);
            }
            if !tiered.gate_checked[findex] {
                tiered.gate_checked[findex] = true;
                // The bytecode is published once, by enable_tiered, from an
                // Arc the context owns — not lazily from a borrow here.
                match Self::tierable_reason(bytecode, func_idx, &tiered.config) {
                    Ok(()) => {
                        let bound = tiered.adapter.register(findex as beadie::CoreHandle, None);
                        tiered.beads[findex] = Some(bound);
                        let f = &bytecode.functions[func_idx];
                        if let Some(tf) = bytecode.types[f.type_.0].fun.as_ref() {
                            let mut arg_kinds = [hl::hl_type_kind_HVOID; 8];
                            let nargs = tf.args.len().min(8);
                            for (i, a) in tf.args.iter().take(8).enumerate() {
                                arg_kinds[i] = bytecode.types[a.0].kind;
                            }
                            let ret_kind = bytecode.types[tf.ret.0].kind;
                            tiered.sigs[findex] = Some((arg_kinds, nargs as u8, ret_kind));
                        }
                    }
                    Err(reason) => {
                        if tiered.config.log_promotions {
                            eprintln!("[tiered] skip findex={} reason={}", findex, reason);
                        }
                        return None;
                    }
                }
            }
        }

        // Immutable phase: tick + (possibly) submit compile jobs.
        let code = {
            let tiered = self.tiered_runtime.as_ref()?;
            let bound = tiered.beads[findex].as_ref()?;
            let ctx = Arc::clone(&tiered.shared_ctx);
            tiered.adapter.on_invoke(bound, move |tier, bead| {
                tiered_compile_tier(&ctx, tier, findex, bead, true)
            })?
        };

        let addr = code as usize;
        let tiered = self.tiered_runtime.as_mut()?;
        // Steady state: the bead is already compiled and its entry is cached.
        // This is the path ~10M invocations take, so it must be a compare and a
        // copy — nothing refcounted, nothing rebuilt.
        if let Some(entry) = tiered.entries[findex].as_ref() {
            if entry.fn_addr == addr {
                return Some(*entry);
            }
        }
        // Freshly installed (or newly swapped-in) code.
        //
        // Attach any OSR entries the promote staged. The re-swap with the
        // SAME code pointer is beadie's install path for a table: it bumps
        // the generation, which is what activates the entries — a table
        // installed before the adapter's own install would be orphaned by
        // that install's bump. For a single-invocation hot loop this branch
        // is reached by the loop's own back-edge ticks, so the entries are
        // live within 64 iterations of the code landing.
        if let Some(entries) = tiered
            .shared_ctx
            .pending_osr
            .lock()
            .expect("pending_osr mutex poisoned")
            .remove(&findex)
        {
            if let Some(bound) = tiered.beads[findex].as_ref() {
                let n = entries.len();
                if bound
                    .bead()
                    .swap_compiled_with_osr(addr as *mut (), entries.clone())
                    .is_some()
                {
                    self.osr_attached.insert(findex, entries);
                    if osr_logging() {
                        eprintln!("[osr] attached {n} entries findex={findex}");
                    }
                } else if osr_logging() {
                    eprintln!("[osr] attach refused findex={findex} (bead not compiled)");
                }
            }
        }
        let (arg_kinds, nargs, ret_kind) = tiered.sigs[findex]?;
        let entry = CompiledFunctionEntry {
            fn_addr: addr,
            arg_kinds,
            nargs,
            ret_kind,
        };
        tiered.stats.successful_promotions += 1;
        if tiered.config.log_promotions {
            eprintln!(
                "[tiered] promoted findex={} addr=0x{:x} gen={}",
                findex,
                addr,
                tiered.beads[findex]
                    .as_ref()
                    .map(|b| b.generation())
                    .unwrap_or(0)
            );
        }
        tiered.entries[findex] = Some(entry);
        Some(entry)
    }

    /// Return native code for a reached Haxe function without ever executing
    /// its bytecode in the interpreter.
    ///
    /// Tier 0 is compiled synchronously so the caller has a body to enter.
    /// In auto mode, a baseline with a real re-tier site is then submitted to
    /// the existing LLVM promotion broker; Cranelift keeps running while LLVM
    /// compiles and its AIR V2 OSR entries are published into those slots.
    fn compiled_only_entry(
        &mut self,
        bytecode: &DecodedBytecode,
        findex: usize,
        func_idx: usize,
    ) -> Result<CompiledFunctionEntry> {
        let config = self
            .tiered_runtime
            .as_ref()
            .ok_or_else(|| anyhow!("compiled-only runtime is not enabled"))?
            .config
            .clone();
        Self::tierable_reason(bytecode, func_idx, &config).map_err(|reason| {
            anyhow!(
                "JIT cannot compile findex {} ({}): {}",
                findex,
                bytecode.functions[func_idx].name(),
                reason
            )
        })?;

        // `tiered_on_invoke` owns the one-time bead registration and cached
        // ABI metadata. Its primary threshold is unreachable in this mode,
        // so this first call can only register and tick.
        let _ = self.tiered_on_invoke(bytecode, findex, func_idx);

        let (ctx, bead) = {
            let tiered = self
                .tiered_runtime
                .as_ref()
                .ok_or_else(|| anyhow!("compiled-only runtime disappeared"))?;
            let bound = tiered
                .beads
                .get(findex)
                .and_then(|bound| bound.as_ref())
                .ok_or_else(|| anyhow!("JIT did not register findex {}", findex))?;
            (Arc::clone(&tiered.shared_ctx), Arc::clone(bound.bead()))
        };

        if bead.compiled().is_none() {
            let code = tiered_compile_tier(&ctx, 0, findex, &bead, true);
            if code.is_null() {
                return Err(anyhow!(
                    "JIT tier 0 failed to compile findex {} ({})",
                    findex,
                    bytecode.functions[func_idx].name()
                ));
            }
            if !bead.eager_install(code) && bead.compiled().is_none() {
                return Err(anyhow!(
                    "JIT tier 0 could not install findex {} ({})",
                    findex,
                    bytecode.functions[func_idx].name()
                ));
            }
        }

        // Observe the eager install through the ordinary cache path. This
        // builds the typed call entry and keeps all pointer-change handling in
        // one place.
        let entry = self
            .tiered_on_invoke(bytecode, findex, func_idx)
            .ok_or_else(|| anyhow!("JIT installed no callable entry for findex {}", findex))?;

        // A direct or closure call made by compiled code can use the guarded
        // stub bridge and trigger compilation at the call boundary. A closure
        // handed to native code cannot: HashLink's native ABI calls
        // `vclosure.fun` directly, and an uncompiled findex sentinel (for
        // example 0x11b in Array.sort) is an instruction-address crash.
        //
        // Compile only closure bodies materialized by THIS optimized AIR V2
        // function. This preserves lazy per-function compilation while making
        // every closure that can escape from the body natively callable before
        // the body starts. Reading the IR directly is important: O3 may inline
        // the closure-producing callee, and serializing AIR back into HL
        // opcodes here would recreate the legacy architecture we removed.
        if self.compiled_only_deps_ready.insert(findex) {
            let closure_targets = {
                let tiered = self
                    .tiered_runtime
                    .as_ref()
                    .ok_or_else(|| anyhow!("compiled-only runtime disappeared"))?;
                let tier = tiered
                    .shared_ctx
                    .cranelift
                    .lock()
                    .expect("cranelift mutex poisoned")
                    .as_ref()
                    .and_then(|tier| tier.as_ref())
                    .cloned();
                let Some(tier) = tier else {
                    return Err(anyhow!("JIT lost its Cranelift baseline"));
                };
                let raw = &bytecode.functions[func_idx];
                let optimized = ash_core::air_pipeline::optimized(tier.ctx.air_module(), raw)
                    .map_err(|e| anyhow!("AIR V2 closure scan failed: {}", e.brief()))?;
                let mut targets = Vec::new();
                for block in &optimized.ir.blocks {
                    for instr in &block.instrs {
                        let target = match instr {
                            air::v2::Instr::StaticClosure { fun, .. }
                            | air::v2::Instr::InstanceClosure { fun, .. } => Some(*fun),
                            _ => None,
                        };
                        if let Some(target) = target {
                            if !targets.contains(&target) {
                                targets.push(target);
                            }
                        }
                    }
                }
                targets
            };
            for target in closure_targets {
                let Some(target_idx) = func_of(&self.targets, target) else {
                    continue; // native closures already carry native pointers
                };
                self.compiled_only_entry(bytecode, target, target_idx)
                    .with_context(|| {
                        format!("JIT closure dependency {target} reached from findex {findex}")
                    })?;
            }
        }

        let baseline_is_llvm = ctx
            .llvm_done
            .lock()
            .expect("llvm_done mutex poisoned")
            .contains(&findex);
        let has_retier_site = ctx
            .hot_loop_pcs
            .lock()
            .expect("hot_loop_pcs mutex poisoned")
            .contains_key(&findex);
        if config.tier_mode == TierMode::Auto
            && bead.generation() == 0
            && !baseline_is_llvm
            && has_retier_site
        {
            let queued = {
                let tiered = self
                    .tiered_runtime
                    .as_ref()
                    .ok_or_else(|| anyhow!("compiled-only runtime disappeared"))?;
                let bound = tiered.beads[findex]
                    .as_ref()
                    .ok_or_else(|| anyhow!("JIT lost bead for findex {}", findex))?;
                let promote_ctx = Arc::clone(&tiered.shared_ctx);
                tiered.adapter.force_promote(bound, 1, move |promote_bead| {
                    tiered_compile_tier(&promote_ctx, 1, findex, promote_bead, true)
                })
            };
            if !queued && config.log_promotions {
                eprintln!("[tiered] LLVM queue busy for findex={findex}; keeping Cranelift");
            }
        }

        Ok(entry)
    }

    /// One-time tierability gate, run at bead registration (not per call).
    fn tierable_reason(
        bytecode: &DecodedBytecode,
        func_idx: usize,
        config: &TieredConfig,
    ) -> std::result::Result<(), String> {
        if !config.strict_mode {
            return Err("non_strict_mode".to_string());
        }
        let func = &bytecode.functions[func_idx];
        // Debug escape hatch: ASH_TIERED_SKIP_FINDEXES=1,2,3 excludes specific
        // findexes from promotion (for bisecting a miscompiled hot function).
        {
            static SKIP: std::sync::OnceLock<Vec<usize>> = std::sync::OnceLock::new();
            let skip = SKIP.get_or_init(|| {
                std::env::var("ASH_TIERED_SKIP_FINDEXES")
                    .map(|v| v.split(',').filter_map(|s| s.trim().parse().ok()).collect())
                    .unwrap_or_default()
            });
            if skip.contains(&(func.findex as usize)) {
                return Err("skipped_by_env".to_string());
            }
        }
        // The inverse gate: ASH_TIERED_ONLY_FINDEXES=1,2,3 promotes ONLY the
        // listed findexes. Skip-lists cannot bisect promotion defects — every
        // exclusion lets the next-hottest function promote instead, so the
        // tested set never converges. This pins it exactly.
        {
            static ONLY: std::sync::OnceLock<Vec<usize>> = std::sync::OnceLock::new();
            let only = ONLY.get_or_init(|| {
                std::env::var("ASH_TIERED_ONLY_FINDEXES")
                    .map(|v| v.split(',').filter_map(|s| s.trim().parse().ok()).collect())
                    .unwrap_or_default()
            });
            if !only.is_empty() && !only.contains(&(func.findex as usize)) {
                return Err("not_in_only_set".to_string());
            }
        }
        // Static signature arg count; call_compiled_function marshals at most 8.
        let nargs = bytecode.types[func.type_.0]
            .fun
            .as_ref()
            .map(|f| f.args.len())
            .unwrap_or(0);
        if nargs > config.max_jit_args || nargs > 8 {
            return Err("arg_count_over_limit".to_string());
        }
        let func_name = func.name();
        if !config.compiled_only
            && (func_name == "init"
                || func_name == "main"
                || func_name == "__constructor__"
                || func_name.starts_with("__"))
        {
            return Err("name_blacklisted".to_string());
        }
        if config.min_ops_for_promotion > 0 && func.ops.len() < config.min_ops_for_promotion {
            return Err("op_count_below_min".to_string());
        }
        if !config.compiled_only {
            if let Some(bad) = func.ops.iter().find(|op| !Self::is_v1_tierable_opcode(op)) {
                return Err(format!("unsupported_opcode op={:?}", bad));
            }
        }
        // Do not run the classic opcode lowerer's gate here, even in
        // Cranelift-only mode. The mandatory AIR V2 path accepts operations
        // (Type and StaticClosure are common examples) that the legacy flat
        // opcode emitter rejects. `tiered_compile_tier` asks AIR V2 first and
        // reports a null result if both Cranelift lowering paths genuinely
        // decline; pre-screening on HashLink opcodes would reject valid AIR
        // before the compiler sees it.
        Ok(())
    }

    fn is_v1_tierable_opcode(op: &Opcode) -> bool {
        // Allow all opcodes the full JIT supports. The v1 whitelist was too
        // conservative — it blocked GetThis/GetGlobal/Call/Field etc., preventing
        // any real Heaps functions from being promoted.
        !matches!(op, Opcode::Prefetch { .. } | Opcode::Asm { .. })
    }

    /// Deopt after a compiled invoke failed: invalidate the bead (permanent
    /// Deopt via beadie) and drop the cached marshaling entry so the function
    /// falls back to the interpreter.
    fn record_tiered_fallback(&mut self, findex: usize, reason: String) {
        if let Some(tiered) = self.tiered_runtime.as_mut() {
            tiered.stats.fallback_calls += 1;
            tiered.stats.failed_promotions += 1;
            if let Some(slot) = tiered.entries.get_mut(findex) {
                *slot = None;
            }
            if let Some(bound) = tiered.beads.get(findex).and_then(|b| b.as_ref()) {
                bound.bead().invalidate();
            }
            if tiered.config.log_promotions {
                eprintln!("[tiered] fallback findex={} reason={}", findex, reason);
            }
        }
    }

    /// Restore interpreter-owned state after an HL longjmp and turn the
    /// runtime's pending value into the Rust error used by the call paths.
    fn longjmp_error(
        &mut self,
        bytecode: Option<&DecodedBytecode>,
        stack_depth: usize,
        fallback: String,
    ) -> anyhow::Error {
        // BEFORE the drain below: a Haxe `throw` reaches hl_throw as a native
        // longjmp and lands here, and the frames it was thrown from are the
        // ones about to be discarded. Capturing after would leave the trace
        // holding only the entrypoint, which is what it used to report.
        let stack = bytecode
            .map(|bc| self.capture_call_stack(bc))
            .unwrap_or_default();
        for frame in self.stack.drain(stack_depth..) {
            if self.reg_pool.len() < POOL_CAP {
                self.reg_pool.push(frame.into_buffer());
            }
        }
        self.sync_gc_scan_roots();

        if !self.fn_get_exc_value.is_null() {
            type FnGetExc = unsafe extern "C" fn() -> *mut c_void;
            let exc_ptr =
                unsafe { (std::mem::transmute::<*mut c_void, FnGetExc>(self.fn_get_exc_value))() };
            if !exc_ptr.is_null() {
                // Formatting may allocate, so preserve the pending runtime
                // exception until its Rust representation is complete.
                let mut exception =
                    self.format_hl_exception(NanBoxedValue::from_ptr(exc_ptr as usize));
                exception.stack = stack;
                if !self.fn_clear_exc_value.is_null() {
                    type FnClearExc = unsafe extern "C" fn();
                    unsafe {
                        (std::mem::transmute::<*mut c_void, FnClearExc>(self.fn_clear_exc_value))()
                    };
                }
                return anyhow::Error::new(exception);
            }
        }

        anyhow!(fallback)
    }

    fn call_compiled_function(
        &mut self,
        findex: usize,
        entry: &CompiledFunctionEntry,
        args: &[NanBoxedValue],
    ) -> Result<NanBoxedValue> {
        if args.len() > 8 {
            return Err(anyhow!(
                "Compiled call {} has {} args (max 8)",
                findex,
                args.len()
            ));
        }

        self.sync_gc_scan_roots();

        let func_ptr = entry.fn_addr as *mut c_void;
        // `args()`, not the raw array: the array is fixed at eight slots and
        // only the first `nargs` are real. Iterating the whole thing would put
        // trailing zero kinds into the float mask and misroute the call.
        let arg_kinds = entry.args();
        let ret_kind = entry.ret_kind;

        // Compiled AIR V2 uses the native HashLink ABI: Dynamic parameters
        // are vdynamic pointers. Interpreter registers may carry their
        // primitive payload inline, so materialize those boxes before the
        // typed call instead of forwarding (for example) integer 5 as 0x5.
        let mut marshaled_args = [NanBoxedValue::null(); 8];
        for (index, &arg) in args.iter().enumerate() {
            let kind = arg_kinds
                .get(index)
                .copied()
                .unwrap_or(hl::hl_type_kind_HVOID);
            marshaled_args[index] = if matches!(
                kind,
                hl::hl_type_kind_HDYN | hl::hl_type_kind_HNULL | hl::hl_type_kind_HDYNOBJ
            ) {
                self.box_for_compiled_dynamic_value(arg)
            } else {
                arg
            };
        }
        let args = &marshaled_args[..args.len()];
        // Boxing may allocate. Republish the complete interpreted root set
        // before entering code that can itself trigger a collection.
        self.sync_gc_scan_roots();

        let is_float_kind =
            |k: hl::hl_type_kind| k == hl::hl_type_kind_HF32 || k == hl::hl_type_kind_HF64;
        let ret_is_float = is_float_kind(ret_kind);
        let float_mask: u32 = arg_kinds.iter().enumerate().fold(0u32, |acc, (i, &k)| {
            if is_float_kind(k) {
                acc | (1 << i)
            } else {
                acc
            }
        });

        let extract_arg = |idx: usize| -> i64 {
            let kind = if idx < arg_kinds.len() {
                arg_kinds[idx]
            } else {
                0
            };
            self.value_to_i64(args[idx], kind)
        };

        let fn_setup_trap = self.fn_setup_trap_jit;
        let fn_remove_trap = self.fn_remove_trap_jit;
        let fn_get_exc = self.fn_get_exc_value;
        let fn_clear_exc = self.fn_clear_exc_value;
        // Frame depth at the setjmp site. A throw from inside the compiled
        // callee — including one raised by the stub bridge re-entering the
        // interpreter — longjmps back here without unwinding, so the frames
        // those abandoned Rust activations pushed are still on `self.stack`
        // and have to be dropped explicitly.
        let stack_depth = self.stack.len();
        let mut dispatch_res = None;
        let jumped = run_with_hl_trap(fn_setup_trap, fn_remove_trap, || {
            dispatch_res = Some(if ret_is_float || float_mask != 0 {
                self.dispatch_float_native(
                    func_ptr,
                    args,
                    arg_kinds,
                    float_mask,
                    ret_is_float,
                    ret_kind == hl::hl_type_kind_HF32,
                )
            } else {
                Ok(unsafe {
                    match args.len() {
                        0 => {
                            let f: unsafe extern "C" fn() -> i64 = std::mem::transmute(func_ptr);
                            f()
                        }
                        1 => {
                            let f: unsafe extern "C" fn(i64) -> i64 = std::mem::transmute(func_ptr);
                            f(extract_arg(0))
                        }
                        2 => {
                            let f: unsafe extern "C" fn(i64, i64) -> i64 =
                                std::mem::transmute(func_ptr);
                            f(extract_arg(0), extract_arg(1))
                        }
                        3 => {
                            let f: unsafe extern "C" fn(i64, i64, i64) -> i64 =
                                std::mem::transmute(func_ptr);
                            f(extract_arg(0), extract_arg(1), extract_arg(2))
                        }
                        4 => {
                            let f: unsafe extern "C" fn(i64, i64, i64, i64) -> i64 =
                                std::mem::transmute(func_ptr);
                            f(
                                extract_arg(0),
                                extract_arg(1),
                                extract_arg(2),
                                extract_arg(3),
                            )
                        }
                        5 => {
                            let f: unsafe extern "C" fn(i64, i64, i64, i64, i64) -> i64 =
                                std::mem::transmute(func_ptr);
                            f(
                                extract_arg(0),
                                extract_arg(1),
                                extract_arg(2),
                                extract_arg(3),
                                extract_arg(4),
                            )
                        }
                        6 => {
                            let f: unsafe extern "C" fn(i64, i64, i64, i64, i64, i64) -> i64 =
                                std::mem::transmute(func_ptr);
                            f(
                                extract_arg(0),
                                extract_arg(1),
                                extract_arg(2),
                                extract_arg(3),
                                extract_arg(4),
                                extract_arg(5),
                            )
                        }
                        7 => {
                            let f: unsafe extern "C" fn(i64, i64, i64, i64, i64, i64, i64) -> i64 =
                                std::mem::transmute(func_ptr);
                            f(
                                extract_arg(0),
                                extract_arg(1),
                                extract_arg(2),
                                extract_arg(3),
                                extract_arg(4),
                                extract_arg(5),
                                extract_arg(6),
                            )
                        }
                        8 => {
                            let f: unsafe extern "C" fn(
                                i64,
                                i64,
                                i64,
                                i64,
                                i64,
                                i64,
                                i64,
                                i64,
                            ) -> i64 = std::mem::transmute(func_ptr);
                            f(
                                extract_arg(0),
                                extract_arg(1),
                                extract_arg(2),
                                extract_arg(3),
                                extract_arg(4),
                                extract_arg(5),
                                extract_arg(6),
                                extract_arg(7),
                            )
                        }
                        _ => 0i64,
                    }
                })
            });
        });

        if jumped != 0 {
            for f in self.stack.drain(stack_depth..) {
                if self.reg_pool.len() < POOL_CAP {
                    self.reg_pool.push(f.into_buffer());
                }
            }
            self.sync_gc_scan_roots();
            if !fn_get_exc.is_null() {
                type FnGetExc = unsafe extern "C" fn() -> *mut c_void;
                let exc_ptr =
                    unsafe { (std::mem::transmute::<*mut c_void, FnGetExc>(fn_get_exc))() };
                if !exc_ptr.is_null() {
                    // Preserve the pending exception until formatting has
                    // finished, matching the other native trap boundaries.
                    let exception =
                        self.format_hl_exception(NanBoxedValue::from_ptr(exc_ptr as usize));
                    if !fn_clear_exc.is_null() {
                        type FnClearExc = unsafe extern "C" fn();
                        unsafe { (std::mem::transmute::<*mut c_void, FnClearExc>(fn_clear_exc))() };
                    }
                    return Err(anyhow::Error::new(exception));
                }
            }
            return Err(anyhow!(
                "Compiled call longjmp without exception: findex {}",
                findex
            ));
        }

        let raw_result = dispatch_res
            .ok_or_else(|| anyhow!("Compiled call boundary did not run findex {findex}"))??;
        Ok(self.wrap_native_result(raw_result, ret_kind))
    }

    /// Execute a HashLink bytecode function.
    fn execute_hl_function(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        func_idx: usize,
        args: &[NanBoxedValue],
    ) -> Result<NanBoxedValue> {
        // Use reloaded bytecode if available (hot-reload swapped function bodies)
        let using_reloaded = self.reloaded_bytecode.is_some();
        let bc: &DecodedBytecode = self.reloaded_bytecode.unwrap_or(bytecode);
        // Decide this function's body before the frame exists, so every site
        // that resolves it below — register count, argument binding, the
        // dispatch loop — sees the same opcode array and the same `pc` means
        // the same instruction throughout the call.
        //
        // The SSA form is tried first and is a different shape of body
        // entirely: a CFG over a value-indexed frame rather than an opcode
        // array over a register file, so it takes its own entry path. A
        // function it refuses falls through to the opcode dispatcher below,
        // which is the reference either way.
        // Coerce arguments to the callee's DECLARED parameter kinds before
        // either body shape binds them.
        //
        // A dynamic call site can pass a BOXED primitive to a parameter the
        // callee declares as Int/Float/Bool, and the callee would then operate
        // on the box. Unbox against the declared parameter kinds.
        //
        // `coerce_value_for_static_kind` unboxes only when a pointer really is
        // a boxed primitive (aligned, plausible, primitive type header). It
        // also normalizes unboxed Dynamic scalars to the declared destination
        // representation, returns unrelated values untouched, and maps null
        // to a typed zero.
        //
        // Placed here, ahead of the body-shape dispatch, so the serialize and
        // SSA paths cannot disagree. The scan precedes the Vec so the common
        // case — every argument already correctly shaped — allocates nothing.
        let coerced_args: Vec<NanBoxedValue>;
        let args: &[NanBoxedValue] = {
            let params = bc.types[bc.functions[func_idx].type_.0]
                .fun
                .as_ref()
                .map(|f| f.args.as_slice())
                .unwrap_or(&[]);
            let needs = args.iter().enumerate().any(|(i, a)| {
                i < params.len()
                    && (a.is_ptr() || a.is_null())
                    && Self::is_unboxable_primitive_kind(bc.types[params[i].0].kind)
            });
            if needs {
                coerced_args = args
                    .iter()
                    .enumerate()
                    .map(|(i, a)| {
                        if i < params.len() {
                            Self::coerce_value_for_static_kind(*a, bc.types[params[i].0].kind)
                        } else {
                            *a
                        }
                    })
                    .collect();
                &coerced_args
            } else {
                args
            }
        };

        self.ssa.prepare(bc, func_idx);
        if let Some(prep) = self.ssa.body(func_idx) {
            return self.execute_ssa_function(bc, native_resolver, func_idx, prep, args);
        }
        self.air.prepare(bc, func_idx);
        let func = self.air.body(bc, func_idx);
        if using_reloaded && env_flag!("ASH_DBG_RELOAD") {
            eprintln!(
                "[reload-exec] func_idx={} name={} nops={} using=reloaded",
                func_idx,
                func.name(),
                func.ops.len()
            );
        }

        if self.stack.len() >= self.max_stack_depth {
            return Err(anyhow!("Stack overflow (depth {})", self.stack.len()));
        }

        // Create frame with registers
        let reg_count = func.regs.len();
        let buf = self.reg_pool.pop().unwrap_or_default();
        let mut frame = InterpreterFrame::with_buffer(func_idx, reg_count, buf);

        // Bind arguments to first N registers
        let type_fun = bc.types[func.type_.0]
            .fun
            .as_ref()
            .expect("function should have fun type");
        for (i, arg) in args.iter().enumerate() {
            if i < type_fun.args.len() {
                frame.registers.set(i as u32, *arg);
            }
        }

        self.stack.push(frame);
        self.sync_gc_scan_roots();

        // Main interpretation loop — always pop the frame even on error.
        // The findex is published for the sampling profiler so a sample landing
        // in the interpreter (or in a runtime helper it called) can be charged
        // to the bytecode function being executed, not just to the loop.
        let prev_findex = ash_core::profile::enter_interp(bc.functions[func_idx].findex as u32);
        let result = self.interpret_loop(bc, native_resolver, func_idx);
        ash_core::profile::leave_interp(prev_findex);
        if let Some(f) = self.stack.pop() {
            if self.reg_pool.len() < POOL_CAP {
                self.reg_pool.push(f.into_buffer());
            }
        }
        self.sync_gc_scan_roots();
        result
    }

    /// Main opcode dispatch loop for a function.
    fn interpret_loop(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        func_idx: usize,
    ) -> Result<NanBoxedValue> {
        loop {
            self.fiber_safe_point(1);
            let func = self.air.body(bytecode, func_idx);
            let frame = self.stack.last().unwrap();
            let pc = frame.pc;

            if pc >= func.ops.len() {
                return Ok(NanBoxedValue::void());
            }

            // Borrowed, not cloned. `Cache::body` hands back a reference tied
            // to `bytecode` rather than to `self`, so the dispatch loop never
            // needed a copy -- and `Opcode` carries a `Vec<Reg>` in its CallN,
            // CallMethod, CallThis, CallClosure and Switch variants, so the
            // copy was a heap allocation per dispatch on exactly those. The
            // sampler charged 2% of a whole nbody run to `Opcode::clone`.
            let op = &func.ops[pc];
            if matches!(op, Opcode::Throw { .. }) {
                self.capture_exception_stack(bytecode);
            }
            if env_flag!("ASH_TRACE_ASSERT") {
                eprintln!(
                    "[TRACE] f{} {} pc={} op={:?}",
                    func_idx,
                    func.name(),
                    pc,
                    op
                );
            }
            let result = match self.execute_opcode(bytecode, op, func_idx) {
                Ok(result) => result,
                Err(err) => {
                    // Opcode helpers (NullCheck, Assert, checked casts, and
                    // similar runtime operations) report Haxe exceptions as
                    // HLExceptionPropagation.  They must enter a Trap in this
                    // very frame; using `?` here skipped the local handler and
                    // only gave callers a chance to catch the value.
                    let exc = err
                        .downcast_ref::<HLExceptionPropagation>()
                        .map(|exception| exception.value);
                    if let Some(exc) = exc {
                        if matches!(op, Opcode::NullCheck { .. }) {
                            self.capture_exception_stack(bytecode);
                        }
                        let frame = self.stack.last_mut().unwrap();
                        if let Some((target_pc, exc_reg)) = frame.trap_stack.pop() {
                            frame.registers.set(exc_reg, exc);
                            frame.pc = target_pc;
                            continue;
                        }
                    }
                    return Err(err);
                }
            };

            match result {
                StepResult::Continue => {
                    self.stack.last_mut().unwrap().pc += 1;
                }
                StepResult::Jump(offset) => {
                    // offset is relative to the NEXT instruction
                    let (next_pc, hot) = {
                        let frame = self.stack.last_mut().unwrap();
                        let next_pc = (frame.pc as i64) + 1 + (offset as i64);
                        let hot = if offset < 0 {
                            frame.backedges = frame.backedges.wrapping_add(1);
                            // Every 64th, not once. Promotion is driven by a
                            // count crossing a threshold, and there are two of
                            // them to cross -- Cranelift then LLVM -- so a
                            // single signal would stall the ladder at the
                            // first. The mask keeps the cost on the hot path
                            // to an add and a test.
                            frame.backedges & (HOT_LOOP_BACKEDGES - 1) == 0
                        } else {
                            false
                        };
                        frame.pc = next_pc as usize;
                        (next_pc as usize, hot)
                    };
                    if hot {
                        self.note_hot_loop(bytecode, func_idx, next_pc);
                        if let Some(ret) = self.try_osr_transfer(bytecode, func_idx, next_pc)? {
                            return Ok(ret);
                        }
                    }
                }
                StepResult::JumpAbs(target_pc) => {
                    self.stack.last_mut().unwrap().pc = target_pc;
                }
                StepResult::Return(value) => {
                    return Ok(value);
                }
                StepResult::Call { findex, args, dst } => {
                    if env_flag!("ASH_TRACE_NATIVE") {
                        let is_bc = func_of(&self.targets, findex).is_some();
                        let is_nat = native_of(&self.targets, findex).is_some();
                        let depth = self.stack.len();
                        eprintln!(
                            "[trace] call findex={} bc={} nat={} args={} depth={}",
                            findex,
                            is_bc,
                            is_nat,
                            args.len(),
                            depth
                        );
                        use std::io::Write;
                        std::io::stderr().flush().ok();
                    }
                    let call_result = self.call_function(bytecode, native_resolver, findex, &args);
                    // Hand the buffer back. The Call arms pop from arg_pool but
                    // nothing on this path ever pushed, so the pool was
                    // permanently empty and every interpreted call above arity
                    // zero paid a malloc and a free. The callee has already
                    // copied the arguments into its own registers by the time
                    // call_function returns, on both the Ok and Err paths — so
                    // recycle on both, or an exception unwind drains the pool.
                    {
                        let mut args = args;
                        if self.arg_pool.len() < POOL_CAP {
                            args.clear();
                            self.arg_pool.push(args);
                        }
                    }
                    match call_result {
                        Ok(ret) => {
                            let dst_kind = bytecode.types[func.regs[dst as usize].0].kind;
                            let coerced = Self::coerce_value_for_static_kind(ret, dst_kind);
                            self.stack.last_mut().unwrap().registers.set(dst, coerced);
                            self.stack.last_mut().unwrap().pc += 1;

                            // Check deferred hot-reload flag after native calls
                            if ash_core::reload::take_reload_pending() {
                                if let Some(new_bc) = ash_core::reload::do_reload() {
                                    // Leak the old utf16_strings cache — live NanBoxed registers
                                    // in the current (old) frame hold raw pointers into those
                                    // Vec<u16> buffers. Clearing would create dangling pointers.
                                    let old_cache = std::mem::take(&mut self.utf16_strings);
                                    Box::leak(Box::new(old_cache));

                                    // Pre-populate the new cache from the new bytecode's string
                                    // table. This ensures all Opcode::String hits return new
                                    // strings regardless of which bytecode ref interpret_loop holds.
                                    for (idx, s) in new_bc.strings.iter().enumerate() {
                                        let mut buf: Vec<u16> = s.encode_utf16().collect();
                                        buf.push(0);
                                        self.utf16_strings.insert(idx, buf);
                                    }

                                    self.field_hash_cache.clear();

                                    // Invalidate tiered JIT cache — compiled functions still
                                    // point to old code. Forces fallback to interpreter which
                                    // uses the new bytecode. Beads reload (Compiled →
                                    // Interpreted, will recompile); deopt'd beads are cleared
                                    // so the gate re-registers them fresh.
                                    if let Some(tiered) = self.tiered_runtime.as_mut() {
                                        for slot in tiered.entries.iter_mut() {
                                            *slot = None;
                                        }
                                        // Reset every bead to the interpreter
                                        // and clear all tier-promotion flags.
                                        for (findex, slot) in tiered.beads.iter_mut().enumerate() {
                                            let dead = slot
                                                .as_ref()
                                                .map(|b| {
                                                    b.reset_to_interpreter();
                                                    !b.bead().is_valid()
                                                })
                                                .unwrap_or(false);
                                            if dead {
                                                *slot = None;
                                                tiered.gate_checked[findex] = false;
                                            }
                                        }
                                        tiered
                                            .shared_ctx
                                            .llvm_done
                                            .lock()
                                            .expect("llvm_done mutex poisoned")
                                            .clear();
                                    }

                                    // Re-initialize constants from the new bytecode so that
                                    // globals (string literals, class descriptors) reflect V2.
                                    if let Err(e) = self.init_constants(&new_bc, native_resolver) {
                                        eprintln!(
                                            "[hot-reload] warning: init_constants failed: {}",
                                            e
                                        );
                                    }

                                    // Bodies optimized from V1 describe V1's
                                    // functions; a findex may not even be the
                                    // same function in V2.
                                    self.air.invalidate();
                                    self.ssa.invalidate();

                                    let leaked: &'static _ = Box::leak(Box::new(new_bc));
                                    self.reloaded_bytecode = Some(leaked);
                                }
                            }
                        }
                        Err(e) => {
                            if let Some(hl_exc) = e.downcast_ref::<HLExceptionPropagation>() {
                                let exc_val = hl_exc.value;
                                let frame = self.stack.last_mut().unwrap();
                                if let Some((target_pc, exc_reg)) = frame.trap_stack.pop() {
                                    frame.registers.set(exc_reg, exc_val);
                                    frame.pc = target_pc;
                                    // Continue from catch block (no pc increment)
                                } else {
                                    return Err(e);
                                }
                            } else {
                                return Err(e);
                            }
                        }
                    }
                }
            }
        }
    }

    /// Execute a single opcode. Returns what action the loop should take.
    fn execute_opcode(
        &mut self,
        bytecode: &DecodedBytecode,
        op: &Opcode,
        func_idx: usize,
    ) -> Result<StepResult> {
        let func = self.air.body(bytecode, func_idx);
        let frame = self.stack.last_mut().unwrap();

        match op {
            // ===== Movement / Constants =====
            Opcode::Mov { dst, src } => {
                let val = frame.registers.get(src.0);
                frame.registers.set(dst.0, val);
            }
            Opcode::Int { dst, ptr } => {
                let val = bytecode.ints[ptr.0];
                frame.registers.set(dst.0, NanBoxedValue::from_i32(val));
            }
            Opcode::Float { dst, ptr } => {
                let val = bytecode.floats[ptr.0];
                frame.registers.set(dst.0, NanBoxedValue::from_f64(val));
            }
            Opcode::Bool { dst, value } => {
                frame.registers.set(dst.0, NanBoxedValue::from_bool(*value));
            }
            Opcode::Bytes { dst, ptr } => {
                let pos = bytecode.bytes_pos[ptr.0];
                let bytes_ptr = bytecode
                    .bytes_data
                    .get(pos..)
                    .ok_or_else(|| anyhow!("Bytes constant out of bounds: {}", ptr.0))?
                    .as_ptr();
                frame
                    .registers
                    .set(dst.0, NanBoxedValue::from_bytes_ptr(bytes_ptr as usize));
            }
            Opcode::String { dst, ptr } => {
                // HashLink uses UTF-16 strings internally.
                // Get or create a cached null-terminated UTF-16 version of the string.
                let utf16_ptr = if let Some(cached) = self.utf16_strings.get(&ptr.0) {
                    cached.as_ptr()
                } else {
                    let s = bytecode
                        .strings
                        .get(ptr.0)
                        .ok_or_else(|| anyhow!("String constant out of bounds: {}", ptr.0))?;
                    let mut buf: Vec<u16> = s.encode_utf16().collect();
                    buf.push(0);
                    self.utf16_strings.insert(ptr.0, buf);
                    self.utf16_strings[&ptr.0].as_ptr()
                };
                frame
                    .registers
                    .set(dst.0, NanBoxedValue::from_bytes_ptr(utf16_ptr as usize));
            }
            Opcode::Null { dst } => {
                frame.registers.set(dst.0, NanBoxedValue::null());
            }

            // ===== Arithmetic =====
            Opcode::Add { dst, a, b } => {
                let va = frame.registers.get(a.0);
                let vb = frame.registers.get(b.0);
                let result = if let Some(r) = va.binary_int_op(vb, IntBinOp::Add) {
                    r
                } else if let Some(r) = va.binary_float_op(vb, FloatBinOp::Add) {
                    r
                } else {
                    return Err(anyhow!(
                        "Add: incompatible types {:?} + {:?} in {} at pc={} (dst=r{}, a=r{}, b=r{})",
                        va,
                        vb,
                        func.name(),
                        frame.pc,
                        dst.0,
                        a.0,
                        b.0
                    ));
                };
                frame.registers.set(dst.0, result);
            }
            Opcode::Sub { dst, a, b } => {
                let va = frame.registers.get(a.0);
                let vb = frame.registers.get(b.0);
                let result = if let Some(r) = va.binary_int_op(vb, IntBinOp::Sub) {
                    r
                } else if let Some(r) = va.binary_float_op(vb, FloatBinOp::Sub) {
                    r
                } else {
                    return Err(anyhow!("Sub: incompatible types"));
                };
                frame.registers.set(dst.0, result);
            }
            Opcode::Mul { dst, a, b } => {
                let va = frame.registers.get(a.0);
                let vb = frame.registers.get(b.0);
                let result = if let Some(r) = va.binary_int_op(vb, IntBinOp::Mul) {
                    r
                } else if let Some(r) = va.binary_float_op(vb, FloatBinOp::Mul) {
                    r
                } else {
                    return Err(anyhow!("Mul: incompatible types"));
                };
                frame.registers.set(dst.0, result);
            }
            Opcode::SDiv { dst, a, b } => {
                let va = frame.registers.get(a.0);
                let vb = frame.registers.get(b.0);
                let result = if let Some(r) = va.binary_int_op(vb, IntBinOp::SDiv) {
                    r
                } else if let Some(r) = va.binary_float_op(vb, FloatBinOp::SDiv) {
                    r
                } else {
                    return Err(anyhow!("SDiv: incompatible types or div by zero"));
                };
                frame.registers.set(dst.0, result);
            }
            Opcode::UDiv { dst, a, b } => {
                let va = frame.registers.get(a.0);
                let vb = frame.registers.get(b.0);
                let result = va
                    .binary_int_op(vb, IntBinOp::UDiv)
                    .ok_or_else(|| anyhow!("UDiv: incompatible types or div by zero"))?;
                frame.registers.set(dst.0, result);
            }
            Opcode::SMod { dst, a, b } => {
                let va = frame.registers.get(a.0);
                let vb = frame.registers.get(b.0);
                let result = if let Some(r) = va.binary_int_op(vb, IntBinOp::SMod) {
                    r
                } else if let Some(r) = va.binary_float_op(vb, FloatBinOp::SMod) {
                    r
                } else {
                    return Err(anyhow!("SMod: incompatible types or div by zero"));
                };
                frame.registers.set(dst.0, result);
            }
            Opcode::UMod { dst, a, b } => {
                let va = frame.registers.get(a.0);
                let vb = frame.registers.get(b.0);
                // UMod only on integers
                let l = va.as_i32() as u32;
                let r = vb.as_i32() as u32;
                if r == 0 {
                    return Err(anyhow!("UMod: division by zero"));
                }
                frame
                    .registers
                    .set(dst.0, NanBoxedValue::from_i32((l % r) as i32));
            }
            Opcode::Shl { dst, a, b } => {
                self.int_binop(func, IntBinOp::Shl, dst.0, a.0, b.0)?;
            }
            Opcode::SShr { dst, a, b } => {
                self.int_binop(func, IntBinOp::SShr, dst.0, a.0, b.0)?;
            }
            Opcode::UShr { dst, a, b } => {
                self.int_binop(func, IntBinOp::UShr, dst.0, a.0, b.0)?;
            }
            Opcode::And { dst, a, b } => {
                self.int_binop(func, IntBinOp::And, dst.0, a.0, b.0)?;
            }
            Opcode::Or { dst, a, b } => {
                self.int_binop(func, IntBinOp::Or, dst.0, a.0, b.0)?;
            }
            Opcode::Xor { dst, a, b } => {
                self.int_binop(func, IntBinOp::Xor, dst.0, a.0, b.0)?;
            }
            Opcode::Neg { dst, src } => {
                let val = frame.registers.get(src.0);
                let result = if val.is_i32() {
                    NanBoxedValue::from_i32(val.as_i32().wrapping_neg())
                } else if val.is_i64() {
                    NanBoxedValue::from_i64(val.as_i64_lossy().wrapping_neg())
                } else if val.is_f64() {
                    NanBoxedValue::from_f64(-val.as_f64())
                } else {
                    return Err(anyhow!("Neg: unsupported type {:?}", val));
                };
                frame.registers.set(dst.0, result);
            }
            Opcode::Not { dst, src } => {
                let val = frame.registers.get(src.0);
                let result = if val.is_i32() {
                    NanBoxedValue::from_i32(!val.as_i32())
                } else if val.is_i64() {
                    NanBoxedValue::from_i64(!val.as_i64_lossy())
                } else if val.is_bool() {
                    NanBoxedValue::from_bool(!val.as_bool())
                } else {
                    return Err(anyhow!("Not: unsupported type {:?}", val));
                };
                frame.registers.set(dst.0, result);
            }
            Opcode::Incr { dst } => {
                let val = frame.registers.get(dst.0);
                if val.is_i32() {
                    frame
                        .registers
                        .set(dst.0, NanBoxedValue::from_i32(val.as_i32().wrapping_add(1)));
                } else if val.is_i64() {
                    frame.registers.set(
                        dst.0,
                        NanBoxedValue::from_i64(val.as_i64_lossy().wrapping_add(1)),
                    );
                } else if val.is_f64() {
                    frame
                        .registers
                        .set(dst.0, NanBoxedValue::from_f64(val.as_f64() + 1.0));
                }
            }
            Opcode::Decr { dst } => {
                let val = frame.registers.get(dst.0);
                if val.is_i32() {
                    frame
                        .registers
                        .set(dst.0, NanBoxedValue::from_i32(val.as_i32().wrapping_sub(1)));
                } else if val.is_i64() {
                    frame.registers.set(
                        dst.0,
                        NanBoxedValue::from_i64(val.as_i64_lossy().wrapping_sub(1)),
                    );
                } else if val.is_f64() {
                    frame
                        .registers
                        .set(dst.0, NanBoxedValue::from_f64(val.as_f64() - 1.0));
                }
            }

            // ===== Function Calls =====
            Opcode::Call0 { dst, fun } => {
                // An empty Vec never allocated, but taking one keeps every
                // Call arm symmetrical with the trampoline's reclaim.
                let mut args = self.arg_pool.pop().unwrap_or_default();
                args.clear();
                return Ok(StepResult::Call {
                    findex: fun.0,
                    args,
                    dst: dst.0,
                });
            }
            Opcode::Call1 { dst, fun, arg0 } => {
                let a0 = frame.registers.get(arg0.0);
                let mut args = self.arg_pool.pop().unwrap_or_default();
                args.clear();
                args.push(a0);
                return Ok(StepResult::Call {
                    findex: fun.0,
                    args,
                    dst: dst.0,
                });
            }
            Opcode::Call2 {
                dst,
                fun,
                arg0,
                arg1,
            } => {
                let a0 = frame.registers.get(arg0.0);
                let a1 = frame.registers.get(arg1.0);
                let mut args = self.arg_pool.pop().unwrap_or_default();
                args.clear();
                args.push(a0);
                args.push(a1);
                return Ok(StepResult::Call {
                    findex: fun.0,
                    args,
                    dst: dst.0,
                });
            }
            Opcode::Call3 {
                dst,
                fun,
                arg0,
                arg1,
                arg2,
            } => {
                let a0 = frame.registers.get(arg0.0);
                let a1 = frame.registers.get(arg1.0);
                let a2 = frame.registers.get(arg2.0);
                let mut args = self.arg_pool.pop().unwrap_or_default();
                args.clear();
                args.push(a0);
                args.push(a1);
                args.push(a2);
                return Ok(StepResult::Call {
                    findex: fun.0,
                    args,
                    dst: dst.0,
                });
            }
            Opcode::Call4 {
                dst,
                fun,
                arg0,
                arg1,
                arg2,
                arg3,
            } => {
                let a0 = frame.registers.get(arg0.0);
                let a1 = frame.registers.get(arg1.0);
                let a2 = frame.registers.get(arg2.0);
                let a3 = frame.registers.get(arg3.0);
                let mut args = self.arg_pool.pop().unwrap_or_default();
                args.clear();
                args.push(a0);
                args.push(a1);
                args.push(a2);
                args.push(a3);
                return Ok(StepResult::Call {
                    findex: fun.0,
                    args,
                    dst: dst.0,
                });
            }
            Opcode::CallN { dst, fun, args } => {
                let mut arg_vals = self.arg_pool.pop().unwrap_or_default();
                arg_vals.clear();
                arg_vals.extend(args.iter().map(|r| frame.registers.get(r.0)));
                return Ok(StepResult::Call {
                    findex: fun.0,
                    args: arg_vals,
                    dst: dst.0,
                });
            }
            Opcode::CallMethod { dst, field, args } | Opcode::CallThis { dst, field, args } => {
                return self.op_call_method(
                    bytecode,
                    func,
                    func_idx,
                    matches!(op, Opcode::CallThis { .. }),
                    dst.0,
                    field.0,
                    args,
                );
            }
            Opcode::CallClosure { dst, fun, args } => {
                return self.op_call_closure(bytecode, func, func_idx, dst.0, fun.0, args);
            }

            // ===== Closures =====
            Opcode::StaticClosure { dst, fun } => {
                return self.op_static_closure(bytecode, func, func_idx, dst.0, fun.0);
            }
            Opcode::InstanceClosure { dst, fun, obj } => {
                return self.op_instance_closure(bytecode, func, func_idx, dst.0, fun.0, obj.0);
            }
            Opcode::VirtualClosure { dst, obj, field } => {
                return self.op_virtual_closure(bytecode, func, func_idx, dst.0, obj.0, field.0);
            }

            // ===== Globals =====
            Opcode::GetGlobal { dst, global } => {
                let mut val = if global.0 < self.globals.len() {
                    self.globals[global.0]
                } else {
                    NanBoxedValue::null()
                };
                // If our NanBoxed store is null, check the shared globals_data array.
                // Native stdlib may have written to global_value slots (which point into
                // globals_data) without going through the interpreter's SetGlobal.
                if val.is_null() {
                    let (gd, nglobals) = self.c_type_factory.globals_data();
                    if !gd.is_null() && global.0 < nglobals {
                        let raw = unsafe { *gd.add(global.0) };
                        if !raw.is_null() {
                            val = NanBoxedValue::from_ptr(raw as usize);
                            // Cache it in our globals for future reads
                            self.globals[global.0] = val;
                        }
                    }
                }
                frame.registers.set(dst.0, val);
            }
            Opcode::SetGlobal { global, src } => {
                let val = frame.registers.get(src.0);
                if global.0 >= self.globals.len() {
                    self.globals.resize(global.0 + 1, NanBoxedValue::null());
                }
                self.globals[global.0] = val;
                // Also update globals_data so native code sees the new value
                let (gd, nglobals) = self.c_type_factory.globals_data();
                if !gd.is_null() && global.0 < nglobals {
                    unsafe {
                        *gd.add(global.0) = if val.is_null() || val.is_void() {
                            std::ptr::null_mut()
                        } else {
                            val.as_ptr() as *mut c_void
                        };
                    }
                }
            }

            // ===== Fields =====
            Opcode::Field { dst, obj, field } => {
                return self.op_field_get(bytecode, func, func_idx, dst.0, obj.0, field.0);
            }
            Opcode::GetThis { dst, field } => {
                let obj_type_idx = func.regs[0].0;
                let obj_kind = bytecode.types[obj_type_idx].kind;
                let obj_c_type = self.c_type_factory.get(obj_type_idx) as *mut c_void;
                let dst_kind = bytecode.types[func.regs[dst.0 as usize].0].kind;
                let get_rt = self.fn_get_obj_rt;
                let obj_val = frame.registers.get(0); // reg 0 is 'this'
                if obj_val.is_null() || obj_val.is_void() {
                    frame.registers.set(dst.0, NanBoxedValue::null());
                } else if obj_kind == hl::hl_type_kind_HOBJ || obj_kind == hl::hl_type_kind_HSTRUCT
                {
                    let obj_ptr = obj_val.as_ptr() as *mut u8;
                    let val = unsafe {
                        Self::read_obj_field(
                            obj_ptr, field.0, dst_kind, obj_c_type, obj_kind, get_rt,
                        )
                    };
                    if env_flag!("ASH_DBG_FIELD") {
                        eprintln!(
                            "[GETTHIS-OBJ] f{} pc={} obj_ty={} obj_kind={} field={} dst_kind={} -> {:?}",
                            func_idx, frame.pc, obj_type_idx, obj_kind, field.0, dst_kind, val
                        );
                    }
                    frame.registers.set(dst.0, val);
                } else if obj_kind == hl::hl_type_kind_HVIRTUAL {
                    if let Some(offset) = unsafe {
                        Self::resolve_virtual_field_offset(
                            obj_val.as_ptr() as *mut u8,
                            obj_c_type,
                            field.0,
                        )
                    } {
                        let obj_ptr = obj_val.as_ptr() as *mut u8;
                        let addr = unsafe { obj_ptr.add(offset) };
                        let val = unsafe { Self::read_value_at(addr, dst_kind) };
                        if env_flag!("ASH_DBG_FIELD") {
                            eprintln!(
                                "[GETTHIS-VIRT] f{} pc={} obj_ty={} field={} off={} dst_kind={} -> {:?}",
                                func_idx, frame.pc, obj_type_idx, field.0, offset, dst_kind, val
                            );
                        }
                        frame.registers.set(dst.0, val);
                    } else {
                        let key = (obj_val.as_ptr(), field.0);
                        let val = if let Some(v) = self.virtual_fields.get(&key).copied() {
                            v
                        } else if let Some(hfield) =
                            Self::resolve_typed_field_hash(bytecode, obj_type_idx, field.0)
                        {
                            let dst_type_idx = func.regs[dst.0 as usize].0;
                            let dst_type_ptr = self.c_type_factory.get(dst_type_idx) as *mut c_void;
                            Self::dyn_get_field_by_hash(
                                obj_val.as_ptr() as *mut c_void,
                                hfield,
                                dst_kind,
                                dst_type_ptr,
                                self.fn_dyn_getd,
                                self.fn_dyn_getf,
                                self.fn_dyn_geti64,
                                self.fn_dyn_geti,
                                self.fn_dyn_getp,
                            )
                        } else {
                            NanBoxedValue::null()
                        };
                        if env_flag!("ASH_DBG_FIELD") {
                            eprintln!(
                                "[GETTHIS-VIRT-FALLBACK] f{} pc={} obj_ty={} field={} -> {:?}",
                                func_idx, frame.pc, obj_type_idx, field.0, val
                            );
                        }
                        frame.registers.set(dst.0, val);
                    }
                } else if let Some(hfield) =
                    Self::resolve_typed_field_hash(bytecode, obj_type_idx, field.0)
                {
                    let obj_ptr = obj_val.as_ptr() as *mut c_void;
                    let dst_type_idx = func.regs[dst.0 as usize].0;
                    let dst_type_ptr = self.c_type_factory.get(dst_type_idx) as *mut c_void;
                    let out = Self::dyn_get_field_by_hash(
                        obj_ptr,
                        hfield,
                        dst_kind,
                        dst_type_ptr,
                        self.fn_dyn_getd,
                        self.fn_dyn_getf,
                        self.fn_dyn_geti64,
                        self.fn_dyn_geti,
                        self.fn_dyn_getp,
                    );
                    frame.registers.set(dst.0, out);
                } else {
                    frame.registers.set(dst.0, NanBoxedValue::null());
                }
            }
            Opcode::SetField { obj, field, src } => {
                return self.op_field_set(bytecode, func, func_idx, obj.0, field.0, src.0);
            }
            Opcode::SetThis { field, src } => {
                let obj_type_idx = func.regs[0].0;
                let obj_kind = bytecode.types[obj_type_idx].kind;
                let obj_c_type = self.c_type_factory.get(obj_type_idx) as *mut c_void;
                let src_type_idx = func.regs[src.0 as usize].0;
                let src_kind = bytecode.types[src_type_idx].kind;
                let get_rt = self.fn_get_obj_rt;
                let (mk_dyn, pt_i32, pt_f64, pt_bool) = (
                    self.fn_make_dyn,
                    self.prim_t_i32,
                    self.prim_t_f64,
                    self.prim_t_bool,
                );
                let obj_val = frame.registers.get(0); // reg 0 is 'this'
                if !obj_val.is_null() && !obj_val.is_void() {
                    let src_val = Self::box_for_dynamic_slot(
                        mk_dyn,
                        pt_i32,
                        pt_f64,
                        pt_bool,
                        src_kind,
                        frame.registers.get(src.0),
                    );
                    if obj_kind == hl::hl_type_kind_HOBJ || obj_kind == hl::hl_type_kind_HSTRUCT {
                        let obj_ptr = obj_val.as_ptr() as *mut u8;
                        if env_flag!("ASH_DBG_FIELD") {
                            eprintln!(
                                "[SETTHIS-OBJ] f{} pc={} obj_ty={} obj_kind={} field={} src_kind={} src={:?}",
                                func_idx, frame.pc, obj_type_idx, obj_kind, field.0, src_kind, src_val
                            );
                        }
                        unsafe {
                            Self::write_obj_field(
                                obj_ptr, field.0, src_kind, src_val, obj_c_type, obj_kind, get_rt,
                            );
                        }
                    } else if obj_kind == hl::hl_type_kind_HVIRTUAL {
                        if let Some(offset) = unsafe {
                            Self::resolve_virtual_field_offset(
                                obj_val.as_ptr() as *mut u8,
                                obj_c_type,
                                field.0,
                            )
                        } {
                            let obj_ptr = obj_val.as_ptr() as *mut u8;
                            let addr = unsafe { obj_ptr.add(offset) };
                            if env_flag!("ASH_DBG_FIELD") {
                                eprintln!(
                                    "[SETTHIS-VIRT] f{} pc={} obj_ty={} field={} off={} src_kind={} src={:?}",
                                    func_idx, frame.pc, obj_type_idx, field.0, offset, src_kind, src_val
                                );
                            }
                            unsafe { Self::write_value_at(addr, src_kind, src_val) };
                        } else {
                            self.virtual_fields
                                .insert((obj_val.as_ptr(), field.0), src_val);
                            if let Some(hfield) =
                                Self::resolve_typed_field_hash(bytecode, obj_type_idx, field.0)
                            {
                                let obj_ptr = obj_val.as_ptr() as *mut c_void;
                                let src_type_ptr =
                                    self.c_type_factory.get(src_type_idx) as *mut c_void;
                                Self::dyn_set_field_by_hash(
                                    obj_ptr,
                                    hfield,
                                    src_val,
                                    src_kind,
                                    src_type_ptr,
                                    self.fn_dyn_setd,
                                    self.fn_dyn_setf,
                                    self.fn_dyn_seti64,
                                    self.fn_dyn_seti,
                                    self.fn_dyn_setp,
                                );
                            }
                            if env_flag!("ASH_DBG_FIELD") {
                                eprintln!(
                                    "[SETTHIS-VIRT-FALLBACK] f{} pc={} obj_ty={} field={} src={:?}",
                                    func_idx, frame.pc, obj_type_idx, field.0, src_val
                                );
                            }
                        }
                    } else if let Some(hfield) =
                        Self::resolve_typed_field_hash(bytecode, obj_type_idx, field.0)
                    {
                        let obj_ptr = obj_val.as_ptr() as *mut c_void;
                        let src_type_ptr = self.c_type_factory.get(src_type_idx) as *mut c_void;
                        Self::dyn_set_field_by_hash(
                            obj_ptr,
                            hfield,
                            src_val,
                            src_kind,
                            src_type_ptr,
                            self.fn_dyn_setd,
                            self.fn_dyn_setf,
                            self.fn_dyn_seti64,
                            self.fn_dyn_seti,
                            self.fn_dyn_setp,
                        );
                    }
                }
            }
            Opcode::DynGet { dst, obj, field } => {
                return self.op_dyn_get(bytecode, func, func_idx, dst.0, obj.0, field.0);
            }
            Opcode::DynSet { obj, field, src } => {
                return self.op_dyn_set(bytecode, func, func_idx, obj.0, field.0, src.0);
            }

            // ===== Conditional Jumps =====
            Opcode::JTrue { cond, offset } => {
                let val = frame.registers.get(cond.0);
                if val.to_bool() {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JFalse { cond, offset } => {
                let val = frame.registers.get(cond.0);
                if !val.to_bool() {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JNull { reg, offset } => {
                let val = frame.registers.get(reg.0);
                if val.is_null() {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JNotNull { reg, offset } => {
                let val = frame.registers.get(reg.0);
                if !val.is_null() {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JSLt { a, b, offset } => {
                if self.compare_regs(bytecode, func_idx, a.0, b.0, CmpOp::SLt) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JSGte { a, b, offset } => {
                if self.compare_regs(bytecode, func_idx, a.0, b.0, CmpOp::SGte) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JSGt { a, b, offset } => {
                if self.compare_regs(bytecode, func_idx, a.0, b.0, CmpOp::SGt) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JSLte { a, b, offset } => {
                if self.compare_regs(bytecode, func_idx, a.0, b.0, CmpOp::SLte) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JULt { a, b, offset } => {
                if self.compare_regs(bytecode, func_idx, a.0, b.0, CmpOp::ULt) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JUGte { a, b, offset } => {
                if self.compare_regs(bytecode, func_idx, a.0, b.0, CmpOp::UGte) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            // "Not less than" is the NEGATION of Lt, which is not Gte once
            // floats are involved: every comparison against NaN is false, so
            // !(a < b) is TRUE for NaN while (a >= b) is FALSE. Integers are a
            // total order, where the two agree. HashLink special-cases these
            // opcodes for the same reason — jit.c:1845 tests JNParity and
            // hand-sets the flags after COMISD.
            Opcode::JNotLt { a, b, offset } => {
                if !self.compare_regs(bytecode, func_idx, a.0, b.0, CmpOp::SLt) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JNotGte { a, b, offset } => {
                if !self.compare_regs(bytecode, func_idx, a.0, b.0, CmpOp::SGte) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JEq { a, b, offset } => {
                if self.compare_regs(bytecode, func_idx, a.0, b.0, CmpOp::Eq) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JNotEq { a, b, offset } => {
                if self.compare_regs(bytecode, func_idx, a.0, b.0, CmpOp::NotEq) {
                    return Ok(StepResult::Jump(*offset));
                }
            }
            Opcode::JAlways { offset } => {
                return Ok(StepResult::Jump(*offset));
            }

            // ===== Control Flow =====
            Opcode::Ret { ret } => {
                let val = frame.registers.get(ret.0);
                return Ok(StepResult::Return(val));
            }
            Opcode::Switch { reg, offsets, end } => {
                let val = frame.registers.get(reg.0);
                let index = val.as_i32();
                if index >= 0 && (index as usize) < offsets.len() {
                    return Ok(StepResult::Jump(offsets[index as usize]));
                } else {
                    // Out-of-range switch values fall through to the default arm,
                    // which is encoded as the next instruction in HL bytecode.
                    let _ = end;
                    return Ok(StepResult::Continue);
                }
            }
            Opcode::Label => {
                // No-op marker
            }
            Opcode::Nop => {}
            Opcode::Assert => {
                // Upstream OAssert is hl_assert(): hl_error("assert") — a
                // CATCHABLE exception, and the unit suite executes it
                // deliberately. A hard interpreter error killed the whole
                // suite at the first assert-testing case (Issue3702).
                let stack = self.capture_call_stack(bytecode);
                return Err(anyhow::Error::new(HLExceptionPropagation {
                    value: self.internal_exception_value("assert"),
                    message: Some("assert".to_string()),
                    stack,
                }));
            }

            // ===== Type Operations =====
            Opcode::Type { dst, ty } => {
                // Store C-level hl_type pointer for native interop
                let c_type_ptr = self.c_type_factory.get(ty.0);
                frame
                    .registers
                    .set(dst.0, NanBoxedValue::from_ptr(c_type_ptr as usize));
            }
            Opcode::GetType { dst, src } => {
                // GetType returns the RUNTIME hl_type* of the value.
                // For reference types (HDYN, HOBJ, HSTRUCT, etc.) the hl_type*
                // is stored in the first 8 bytes of the pointed-to object.
                // For primitives, return the static C type pointer.
                let type_ref = &func.regs[src.0 as usize];
                let src_kind = bytecode.types[type_ref.0].kind;
                let val = frame.registers.get(src.0);

                let type_ptr: usize = if val.is_null() || val.is_void() {
                    // hl_typeof(NULL) is &hlt_void regardless of the
                    // register's declared type (normally HDYN here).  Using
                    // the static Dynamic type makes Type.typeof(null) fall
                    // through to TUnknown instead of TNull.
                    let void_idx = bytecode
                        .types
                        .iter()
                        .position(|t| t.kind == hl::hl_type_kind_HVOID)
                        .unwrap_or(type_ref.0);
                    self.c_type_factory.get(void_idx) as usize
                } else if val.is_ptr()
                    && !val.is_null()
                    && Self::is_derefable_dynamic(val.as_ptr() as *const hl::vdynamic)
                {
                    match src_kind {
                        hl::hl_type_kind_HDYN
                        | hl::hl_type_kind_HOBJ
                        | hl::hl_type_kind_HSTRUCT
                        | hl::hl_type_kind_HVIRTUAL
                        | hl::hl_type_kind_HENUM
                        | hl::hl_type_kind_HDYNOBJ
                        | hl::hl_type_kind_HNULL => {
                            // First 8 bytes of object is the hl_type*
                            unsafe { *(val.as_ptr() as *const usize) }
                        }
                        _ => self.c_type_factory.get(type_ref.0) as usize,
                    }
                } else {
                    self.c_type_factory.get(type_ref.0) as usize
                };

                frame
                    .registers
                    .set(dst.0, NanBoxedValue::from_ptr(type_ptr));
            }
            Opcode::GetTID { dst, src } => {
                // GetTID returns the kind field of the hl_type* in src.
                // src should hold an hl_type* (result of GetType).
                // hl_type.kind is the C enum at offset 0 — read it through the
                // bindgen alias, whose width is the platform's enum width.
                let val = frame.registers.get(src.0);
                let kind = if val.is_ptr() && !val.is_null() && val.as_ptr() != 0 {
                    unsafe { *(val.as_ptr() as *const hl::hl_type_kind) as i32 }
                } else {
                    // Fallback to static type kind
                    let type_ref = &func.regs[src.0 as usize];
                    bytecode.types[type_ref.0].kind as i32
                };
                frame.registers.set(dst.0, NanBoxedValue::from_i32(kind));
            }

            // ===== Casting =====
            Opcode::ToDyn { dst, src } => {
                return self.op_to_dyn(bytecode, func, func_idx, dst.0, src.0);
            }
            Opcode::ToSFloat { dst, src } => {
                let val = frame.registers.get(src.0);
                let f = if val.is_i32() {
                    val.as_i32() as f64
                } else {
                    val.as_f64()
                };
                frame.registers.set(dst.0, NanBoxedValue::from_f64(f));
            }
            Opcode::ToUFloat { dst, src } => {
                let val = frame.registers.get(src.0);
                let f = if val.is_i32() {
                    (val.as_i32() as u32) as f64
                } else {
                    val.as_f64()
                };
                frame.registers.set(dst.0, NanBoxedValue::from_f64(f));
            }
            Opcode::ToInt { dst, src } => {
                // OToInt converts to the DESTINATION register's int width.
                // haxe.Int64 lowers i32->i64 through this opcode; boxing the
                // result as i32 regardless made every Int64 widen produce a
                // 32-bit value (unit suite Issue4842: And saw I32 where the
                // mask field was I64).
                let val = frame.registers.get(src.0);
                let dst_kind = bytecode.types[func.regs[dst.0 as usize].0].kind;
                let out = if dst_kind == hl::hl_type_kind_HI64 {
                    let i = if val.is_f64() {
                        val.as_f64() as i64
                    } else if val.is_i32() {
                        val.as_i32() as i64
                    } else {
                        val.as_i64_lossy()
                    };
                    NanBoxedValue::from_i64(i)
                } else {
                    let i = if val.is_f64() {
                        val.as_f64() as i32
                    } else if val.is_i32() {
                        val.as_i32()
                    } else {
                        val.as_i64_lossy() as i32
                    };
                    NanBoxedValue::from_i32(i)
                };
                frame.registers.set(dst.0, out);
            }
            Opcode::SafeCast { dst, src } => {
                return self.op_safe_cast(bytecode, func, func_idx, dst.0, src.0);
            }
            Opcode::UnsafeCast { dst, src } => {
                let val = frame.registers.get(src.0);
                frame.registers.set(dst.0, val);
            }
            Opcode::ToVirtual { dst, src } => {
                return self.op_to_virtual(func, dst.0, src.0);
            }

            // ===== Object Creation =====
            Opcode::New { dst } => {
                return self.op_new(bytecode, func, func_idx, dst.0);
            }

            // ===== Array Operations =====
            Opcode::GetArray { dst, array, index } => {
                return self.op_get_array(bytecode, func, func_idx, dst.0, array.0, index.0);
            }
            Opcode::SetArray { array, index, src } => {
                return self.op_set_array(bytecode, func, func_idx, array.0, index.0, src.0);
            }
            Opcode::ArraySize { dst, array } => {
                // Read array size: varray layout has size (i32) at offset 16.
                // Only read if the register static type is HARRAY and value is non-null.
                let arr_type_kind = bytecode.types[func.regs[array.0 as usize].0].kind;
                let arr_val = frame.registers.get(array.0);
                let size = if arr_type_kind == hl::hl_type_kind_HARRAY
                    && !arr_val.is_null()
                    && !arr_val.is_void()
                {
                    let arr_ptr = arr_val.as_ptr() as *const u8;
                    // varray: t@0, at@8, size@16 (i32)
                    unsafe { *(arr_ptr.add(16) as *const i32) }
                } else {
                    0i32
                };
                frame.registers.set(dst.0, NanBoxedValue::from_i32(size));
            }

            // ===== Memory Access =====
            // bytes = base pointer, index = byte offset
            Opcode::GetI8 { dst, bytes, index } => {
                let base = frame.registers.get(bytes.0);
                let idx = frame.registers.get(index.0).as_i32();
                let val = if base.is_null() || base.is_void() || idx < 0 {
                    NanBoxedValue::from_i32(0)
                } else {
                    let addr = (base.as_ptr() as *const u8).wrapping_add(idx as usize);
                    NanBoxedValue::from_i32(unsafe { *addr as i32 })
                };
                frame.registers.set(dst.0, val);
            }
            Opcode::GetI16 { dst, bytes, index } => {
                let base = frame.registers.get(bytes.0);
                let idx = frame.registers.get(index.0).as_i32();
                let val = if base.is_null() || base.is_void() || idx < 0 {
                    NanBoxedValue::from_i32(0)
                } else {
                    let addr = (base.as_ptr() as *const u8).wrapping_add(idx as usize);
                    NanBoxedValue::from_i32(unsafe { *(addr as *const u16) as i32 })
                };
                frame.registers.set(dst.0, val);
            }
            Opcode::GetMem { dst, bytes, index } => {
                let base = frame.registers.get(bytes.0);
                let idx = frame.registers.get(index.0).as_i32();
                let dst_kind = bytecode.types[func.regs[dst.0 as usize].0].kind;
                let val = if base.is_null() || base.is_void() || idx < 0 {
                    NanBoxedValue::from_i32(0)
                } else {
                    let addr = (base.as_ptr() as *const u8).wrapping_add(idx as usize);
                    Self::read_value_from_ptr(addr, dst_kind)
                };
                frame.registers.set(dst.0, val);
            }
            Opcode::SetI8 { bytes, index, src } => {
                let base = frame.registers.get(bytes.0);
                let idx = frame.registers.get(index.0).as_i32();
                let src_val = frame.registers.get(src.0);
                if !base.is_null() && !base.is_void() && idx >= 0 {
                    let addr = (base.as_ptr() as *mut u8).wrapping_add(idx as usize);
                    unsafe { *addr = src_val.as_i32() as u8 };
                }
            }
            Opcode::SetI16 { bytes, index, src } => {
                let base = frame.registers.get(bytes.0);
                let idx = frame.registers.get(index.0).as_i32();
                let src_val = frame.registers.get(src.0);
                if !base.is_null() && !base.is_void() && idx >= 0 {
                    let addr = (base.as_ptr() as *mut u8).wrapping_add(idx as usize);
                    unsafe { *(addr as *mut u16) = src_val.as_i32() as u16 };
                }
            }
            Opcode::SetMem { bytes, index, src } => {
                let base = frame.registers.get(bytes.0);
                let idx = frame.registers.get(index.0).as_i32();
                let src_val = frame.registers.get(src.0);
                let src_kind = bytecode.types[func.regs[src.0 as usize].0].kind;
                if !base.is_null() && !base.is_void() && idx >= 0 {
                    let addr = (base.as_ptr() as *mut u8).wrapping_add(idx as usize);
                    // Crash guard: check if address is accessible
                    if (addr as usize) < 0x1000 {
                        eprintln!(
                            "[CRASH GUARD] SetMem bad addr={:p} base={:?} idx={} in {} pc={}",
                            addr,
                            base,
                            idx,
                            func.name(),
                            0
                        );
                    } else {
                        Self::write_value_to_ptr(addr, src_val, src_kind);
                    }
                }
            }

            // ===== References =====
            Opcode::Ref { dst, src } => {
                // Store a pointer DIRECTLY to the src register's NanBoxedValue storage.
                //
                // HashLink semantics: native code writes through the ref pointer and the
                // source register is updated in-place (no Unref needed by bytecode).
                //
                // This works because NanBoxedValue is 8 bytes and, on little-endian,
                // the i32 payload occupies the low 4 bytes. A native c_int write of N
                // lands in bytes 0-3 → `reg.as_i32()` returns N immediately after return.
                //
                // The pointer is stable for the duration of the native call: pushing new
                // frames may move Vec<InterpreterFrame> but the inner Vec<NanBoxedValue>
                // data (separate heap allocation) does not move.
                let slot = frame.registers.slot_ptr(src.0) as usize;
                let ref_ptr = NanBoxedValue::from_ptr(slot);
                frame.registers.set(dst.0, ref_ptr);
            }
            Opcode::Unref { dst, src } => {
                // Dereference a Ref pointer to read back the value.
                let ptr_val = frame.registers.get(src.0);
                let ptr = ptr_val.as_ptr() as *const i64;
                if (ptr as usize) != 0 && (ptr as usize) < 0x10000 {
                    return Err(anyhow!(
                        "Unref of non-pointer {:#x} (raw {:#018x}) at pc {} in {} (src reg={})",
                        ptr as usize,
                        ptr_val.raw_bits(),
                        frame.pc,
                        func.name(),
                        src.0
                    ));
                }
                if !ptr.is_null() {
                    let val = unsafe { *ptr };
                    let dst_kind = bytecode.types[func.regs[dst.0 as usize].0].kind;
                    let result = match dst_kind {
                        hl::hl_type_kind_HI32 | hl::hl_type_kind_HUI8 | hl::hl_type_kind_HUI16 => {
                            NanBoxedValue::from_i32(val as i32)
                        }
                        hl::hl_type_kind_HF64 | hl::hl_type_kind_HF32 => {
                            NanBoxedValue::from_f64(f64::from_bits(val as u64))
                        }
                        // Read low 32 bits: native writes a c_int (0/1) at byte offset 0.
                        // Must NOT check the full i64 because NAN_TAG bits are always nonzero.
                        hl::hl_type_kind_HBOOL => NanBoxedValue::from_bool((val as i32) != 0),
                        _ => NanBoxedValue::from_ptr(val as usize),
                    };
                    frame.registers.set(dst.0, result);
                } else {
                    frame.registers.set(dst.0, NanBoxedValue::null());
                }
            }
            Opcode::Setref { dst, value } => {
                // Write through a Ref pointer: *dst = value.
                // The pointer is the address of another register's NanBoxedValue storage.
                // Write the full NanBoxedValue so the tag bits are preserved.
                let ptr_val = frame.registers.get(dst.0);
                let ptr = ptr_val.as_ptr() as *mut NanBoxedValue;
                if !ptr.is_null() {
                    let val = frame.registers.get(value.0);
                    unsafe { *ptr = val };
                }
            }

            // ===== Enums =====
            Opcode::MakeEnum {
                dst,
                construct,
                args,
            } => {
                let type_idx = func.regs[dst.0 as usize].0;
                let c_type_ptr = self.c_type_factory.get(type_idx);
                let fn_alloc_enum = self.fn_alloc_enum;
                let val = Self::alloc_enum_value(fn_alloc_enum, c_type_ptr, construct.0 as i32);
                if !val.is_null() {
                    // Write each argument at its construct offset
                    unsafe {
                        let tenum = (*c_type_ptr).__bindgen_anon_1.tenum;
                        let c = &*(*tenum).constructs.add(construct.0);
                        let base = val;
                        for (i, arg_reg) in args.iter().enumerate() {
                            if i >= c.nparams as usize {
                                break;
                            }
                            let offset = *c.offsets.add(i) as usize;
                            let arg_val = frame.registers.get(arg_reg.0);
                            let param_kind = (*(*c.params.add(i))).kind;
                            Self::write_value_to_ptr(base.add(offset), arg_val, param_kind);
                        }
                    }
                }
                frame.registers.set(
                    dst.0,
                    if val.is_null() {
                        NanBoxedValue::null()
                    } else {
                        NanBoxedValue::from_ptr(val as usize)
                    },
                );
            }
            Opcode::EnumAlloc { dst, construct } => {
                let type_idx = func.regs[dst.0 as usize].0;
                let c_type_ptr = self.c_type_factory.get(type_idx);
                let fn_alloc_enum = self.fn_alloc_enum;
                let val = Self::alloc_enum_value(fn_alloc_enum, c_type_ptr, construct.0 as i32);
                frame.registers.set(
                    dst.0,
                    if val.is_null() {
                        NanBoxedValue::null()
                    } else {
                        NanBoxedValue::from_ptr(val as usize)
                    },
                );
            }
            Opcode::EnumIndex { dst, value } => {
                let val = frame.registers.get(value.0);
                let index = if val.is_null() || val.is_void() {
                    0i32
                } else {
                    // venum layout: t@0 (8 bytes), index@8 (i32)
                    unsafe { *(val.as_ptr() as *const u8).add(8).cast::<i32>() }
                };
                frame.registers.set(dst.0, NanBoxedValue::from_i32(index));
            }
            Opcode::EnumField {
                dst,
                value,
                construct,
                field,
            } => {
                let val = frame.registers.get(value.0);
                let type_idx = func.regs[value.0 as usize].0;
                let c_type_ptr = self.c_type_factory.get(type_idx);
                let result = if val.is_null() || val.is_void() || c_type_ptr.is_null() {
                    NanBoxedValue::null()
                } else {
                    unsafe {
                        let tenum = (*c_type_ptr).__bindgen_anon_1.tenum;
                        if tenum.is_null() || construct.0 >= (*tenum).nconstructs as usize {
                            NanBoxedValue::null()
                        } else {
                            let c = &*(*tenum).constructs.add(construct.0);
                            if field.0 >= c.nparams as usize {
                                NanBoxedValue::null()
                            } else {
                                let offset = *c.offsets.add(field.0) as usize;
                                let param_kind = (*(*c.params.add(field.0))).kind;
                                let base = val.as_ptr() as *const u8;
                                Self::read_value_from_ptr(base.add(offset), param_kind)
                            }
                        }
                    }
                };
                frame.registers.set(dst.0, result);
            }
            Opcode::SetEnumField { value, field, src } => {
                let val = frame.registers.get(value.0);
                let src_val = frame.registers.get(src.0);
                let type_idx = func.regs[value.0 as usize].0;
                let c_type_ptr = self.c_type_factory.get(type_idx);
                if !val.is_null() && !val.is_void() && !c_type_ptr.is_null() {
                    unsafe {
                        let tenum = (*c_type_ptr).__bindgen_anon_1.tenum;
                        if !tenum.is_null() {
                            // Get construct index from the actual venum value
                            let construct_idx =
                                *(val.as_ptr() as *const u8).add(8).cast::<i32>() as usize;
                            if construct_idx < (*tenum).nconstructs as usize {
                                let c = &*(*tenum).constructs.add(construct_idx);
                                if field.0 < c.nparams as usize {
                                    let offset = *c.offsets.add(field.0) as usize;
                                    let param_kind = (*(*c.params.add(field.0))).kind;
                                    let base = val.as_ptr() as *mut u8;
                                    Self::write_value_to_ptr(base.add(offset), src_val, param_kind);
                                }
                            }
                        }
                    }
                }
            }

            // ===== Exception Handling =====
            Opcode::Trap { exc, offset } => {
                let target_pc = (frame.pc as i64 + 1 + *offset as i64) as usize;
                frame.trap_stack.push((target_pc, exc.0));
            }
            // The operand is Haxe's `OEndTrap of bool` — a flag, not a
            // register, which is why the JIT ignores it too. Clearing
            // `registers[exc]` nulled whichever local happened to live in r0
            // or r1, so a `try` that assigned and then exited normally lost
            // the assignment.
            Opcode::EndTrap { exc: _ } => {
                frame.trap_stack.pop();
            }
            Opcode::Throw { exc } => {
                let val = frame.registers.get(exc.0);
                if let Some((target_pc, exc_reg)) = frame.trap_stack.pop() {
                    frame.registers.set(exc_reg, val);
                    return Ok(StepResult::JumpAbs(target_pc));
                } else {
                    let mut exc = self.format_hl_exception(val);
                    exc.stack = self.capture_call_stack(bytecode);
                    return Err(anyhow::Error::new(exc));
                }
            }
            Opcode::Rethrow { exc } => {
                let val = frame.registers.get(exc.0);
                if let Some((target_pc, exc_reg)) = frame.trap_stack.pop() {
                    frame.registers.set(exc_reg, val);
                    return Ok(StepResult::JumpAbs(target_pc));
                } else {
                    let mut exc = self.format_hl_exception(val);
                    exc.stack = self.capture_call_stack(bytecode);
                    return Err(anyhow::Error::new(exc));
                }
            }
            Opcode::NullCheck { reg } => {
                let val = frame.registers.get(reg.0);
                if val.is_null() {
                    // Throw as an HL exception (like HashLink does) so it can
                    // be caught by a Trap in the call stack.
                    if env_flag!("ASH_TRACE_NULLACC") {
                        eprintln!("[nullacc] {} pc={} r{}", func.name(), frame.pc, reg.0);
                    }
                    let stack = self.capture_call_stack(bytecode);
                    return Err(anyhow::Error::new(HLExceptionPropagation {
                        value: self.internal_exception_value("Null access"),
                        message: Some("Null access".to_string()),
                        stack,
                    }));
                }
            }

            // ===== Misc =====
            Opcode::RefData { dst, src } => {
                let val = frame.registers.get(src.0);
                // ORefData is not an identity operation: HashLink defines it
                // as a pointer to the first element after the varray header.
                // Returning the array itself made atomic operations read and
                // overwrite `varray::t`; pointer atomics then treated an
                // hl_type* as the stored object and crashed.
                let data = val.as_ptr() + std::mem::size_of::<hl::varray>();
                frame.registers.set(dst.0, NanBoxedValue::from_ptr(data));
            }
            Opcode::RefOffset { dst, reg, offset } => {
                let base = frame.registers.get(reg.0);
                let off = frame.registers.get(offset.0);
                let result = NanBoxedValue::from_ptr(base.as_ptr() + off.as_i32() as usize);
                frame.registers.set(dst.0, result);
            }
            Opcode::Prefetch { .. } => {
                // No-op on interpreter
            }
            Opcode::Asm { .. } => {
                // No-op on interpreter (x86 specific)
            }
            // IndirectCall: same as CallN in the interpreter (dispatch is always by findex)
            Opcode::IndirectCall { dst, fun, args } => {
                let arg_vals: Vec<NanBoxedValue> =
                    args.iter().map(|r| frame.registers.get(r.0)).collect();
                return Ok(StepResult::Call {
                    findex: fun.0,
                    args: arg_vals,
                    dst: dst.0,
                });
            }
        }

        Ok(StepResult::Continue)
    }

    fn execute_ssa_function(
        &mut self,
        bc: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        func_idx: usize,
        prep: &'static crate::ssa::Prepared,
        args: &[NanBoxedValue],
    ) -> Result<NanBoxedValue> {
        if self.stack.len() >= self.max_stack_depth {
            return Err(anyhow!("Stack overflow (depth {})", self.stack.len()));
        }

        let ir = prep.ir;
        let buf = self.reg_pool.pop().unwrap_or_default();
        let mut frame =
            InterpreterFrame::with_buffer(func_idx, ir.values.len() + ir.cells.len(), buf);

        // A pinned argument register never gets a `Param`: lowering emits those
        // only for registers it promoted to SSA. On the serialize path that is
        // harmless because a cell *is* its HL register and the caller already
        // bound it; here the cell is a frame slot of its own, so the binding has
        // to happen explicitly or an argument taken by `Ref` reads as void.
        for (ci, cell) in ir.cells.iter().enumerate() {
            if let Some(v) = args.get(cell.reg as usize) {
                frame.registers.set(prep.cell_base + ci as u32, *v);
            }
        }

        self.stack.push(frame);
        self.sync_gc_scan_roots();

        let prev_findex = ash_core::profile::enter_interp(bc.functions[func_idx].findex as u32);
        let result = self.ssa_loop(bc, native_resolver, func_idx, prep, args);
        ash_core::profile::leave_interp(prev_findex);
        if let Some(f) = self.stack.pop() {
            if self.reg_pool.len() < POOL_CAP {
                self.reg_pool.push(f.into_buffer());
            }
        }
        self.sync_gc_scan_roots();
        result
    }

    fn int_binop(
        &mut self,
        func: &HLFunction,
        op: IntBinOp,
        dst: u32,
        a: u32,
        b: u32,
    ) -> Result<()> {
        let frame = self.stack.last_mut().unwrap();
        let va = frame.registers.get(a);
        let vb = frame.registers.get(b);
        let result = va.binary_int_op(vb, op).ok_or_else(|| {
            anyhow!(
                "{:?}: incompatible types {:?}, {:?} in {} at pc={} (dst=r{}, a=r{}, b=r{})",
                op,
                va,
                vb,
                func.name(),
                frame.pc,
                dst,
                a,
                b
            )
        })?;
        frame.registers.set(dst, result);
        Ok(())
    }

    /// Helper: compare two register values.
    fn compare_regs(
        &self,
        bytecode: &DecodedBytecode,
        func_idx: usize,
        a: u32,
        b: u32,
        op: CmpOp,
    ) -> bool {
        let func = self.air.body(bytecode, func_idx);
        self.compare_regs_in(bytecode, func, func_idx, a, b, op)
    }

    /// [`compare_regs`](Self::compare_regs) against an explicit function.
    ///
    /// The comparison is type-directed — HNULL unboxing, string-object and
    /// dynamic equality all depend on the operands' static kinds — so the SSA
    /// dispatcher in [`crate::ssa`] passes its value-type view here instead of
    /// having a second implementation of those rules.
    #[allow(clippy::too_many_arguments)]
    fn compare_regs_in(
        &self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        func_idx: usize,
        a: u32,
        b: u32,
        op: CmpOp,
    ) -> bool {
        let frame = self.stack.last().unwrap();
        let va = frame.registers.get(a);
        let vb = frame.registers.get(b);
        let ak = bytecode.types[func.regs[a as usize].0].kind;
        let bk = bytecode.types[func.regs[b as usize].0].kind;
        if let Some(result) = unsafe {
            self.try_compare_nullable_operands(
                bytecode, func, a as usize, va, ak, b as usize, vb, bk, op,
            )
        } {
            if env_flag!("ASH_TRACE_EQ") {
                eprintln!(
                    "[CMP-HNULL] f{} op={:?} ak={} bk={} va={:?} vb={:?} -> {}",
                    func_idx, op, ak, bk, va, vb, result
                );
            }
            return result;
        }
        // The generic NanBox comparison fast path only knows i32 and f64.
        // Type-directed numeric comparison is also required for HI64 and for
        // mixed-width operands (the checks inside Int64.parseString and
        // Int64.toInt are ordinary jump opcodes, not nullable comparisons).
        if let Some(result) = Self::compare_numeric_values(va, ak, vb, bk, op) {
            return result;
        }
        // Haxe lowers relational operators on `Dynamic` values to the same
        // jump opcodes as concrete values.  The registers still have HDYN
        // type, though, so the concrete numeric path above cannot interpret
        // their boxes.  HashLink compares the boxed payloads (and String
        // contents) rather than the box addresses.
        if matches!(
            op,
            CmpOp::SLt | CmpOp::SGt | CmpOp::SLte | CmpOp::SGte | CmpOp::ULt | CmpOp::UGte
        ) && (ak == hl::hl_type_kind_HDYN || bk == hl::hl_type_kind_HDYN)
        {
            if let Some(ord) = unsafe { self.dynamic_value_cmp(va, vb) } {
                return match op {
                    CmpOp::SLt | CmpOp::ULt => ord.is_lt(),
                    CmpOp::SGt => ord.is_gt(),
                    CmpOp::SLte => ord.is_le(),
                    CmpOp::SGte | CmpOp::UGte => ord.is_ge(),
                    _ => unreachable!(),
                };
            }
        }
        // Ordering between strings. Without this the operands fall through to
        // NanBoxedValue::compare, which has no ordering for pointers and
        // answers None -> false, so every `<` and `>` between strings was
        // false. That is not merely a wrong answer: haxe.ds.ArraySort — which
        // is what Array<String>.sort delegates to, there being no native
        // object sort — relies on a consistent comparator, and an always-false
        // one walks its merge off the end of the array and segfaults.
        if matches!(op, CmpOp::SLt | CmpOp::SGt | CmpOp::SLte | CmpOp::SGte) {
            let sa = unsafe { self.string_operand_utf16(va, ak) };
            let sb = unsafe { self.string_operand_utf16(vb, bk) };
            if let (Some((ap, al)), Some((bp, bl))) = (sa, sb) {
                let ord = unsafe { Self::utf16_cmp(ap, al, bp, bl) };
                let result = match op {
                    CmpOp::SLt => ord.is_lt(),
                    CmpOp::SGt => ord.is_gt(),
                    CmpOp::SLte => ord.is_le(),
                    _ => ord.is_ge(),
                };
                if env_flag!("ASH_TRACE_EQ") {
                    eprintln!(
                        "[CMP] f{} op={:?} ak={} bk={} (string-order) -> {}",
                        func_idx, op, ak, bk, result
                    );
                }
                return result;
            }
        }
        if op == CmpOp::Eq || op == CmpOp::NotEq {
            // Upstream `hl_dyn_compare` compares a virtual by its wrapped
            // value (TK2(HOBJ,HVIRTUAL) and friends): a view over an object
            // IS that object for equality. Unwrap before any pointer
            // identity below, or `interface_var == object` is always false.
            let unwrap_view = |v: NanBoxedValue, declared: hl::hl_type_kind| -> NanBoxedValue {
                // Only kinds whose register value starts with an hl_type
                // header may be probed — an HBYTES register holds raw UTF-16
                // data, and a Dynamic register can carry a pointer-shaped
                // immediate (hence the 0x10000 floor other probes here use).
                let headered = matches!(
                    declared,
                    hl::hl_type_kind_HVIRTUAL
                        | hl::hl_type_kind_HDYN
                        | hl::hl_type_kind_HOBJ
                        | hl::hl_type_kind_HDYNOBJ
                );
                if headered && v.is_ptr() && !v.is_null() && !v.is_void() && v.as_ptr() >= 0x10000
                {
                    unsafe {
                        let hdr = *(v.as_ptr() as *const *mut hl_type);
                        if !hdr.is_null()
                            && (hdr as usize) >= 0x10000
                            && (*hdr).kind == hl::hl_type_kind_HVIRTUAL
                        {
                            let value = (*(v.as_ptr() as *const hl::vvirtual)).value;
                            if !value.is_null() {
                                return NanBoxedValue::from_ptr(value as usize);
                            }
                        }
                    }
                }
                v
            };
            let va = unwrap_view(va, ak);
            let vb = unwrap_view(vb, bk);
            // Identity after unwrapping settles it for every pointer kind:
            // a view and its object, or two views over one object, are equal.
            // Decided here because the declared-kind arms below want matching
            // kinds on both sides, which a view/object mix never has.
            if va.is_ptr()
                && vb.is_ptr()
                && !va.is_null()
                && !vb.is_null()
                && va.as_ptr() == vb.as_ptr()
            {
                return op == CmpOp::Eq;
            }
            if ak == hl::hl_type_kind_HBYTES && bk == hl::hl_type_kind_HBYTES {
                let pa = if va.is_null() || va.is_void() {
                    std::ptr::null()
                } else {
                    va.as_ptr() as *const u16
                };
                let pb = if vb.is_null() || vb.is_void() {
                    std::ptr::null()
                } else {
                    vb.as_ptr() as *const u16
                };
                let eq = unsafe { Self::utf16z_eq(pa, pb) };
                if env_flag!("ASH_TRACE_EQ") {
                    eprintln!(
                        "[CMP] f{} op={:?} ak={} bk={} (bytes) -> {}",
                        func_idx, op, ak, bk, eq
                    );
                }
                return if op == CmpOp::Eq { eq } else { !eq };
            }
            if ak == hl::hl_type_kind_HOBJ && bk == hl::hl_type_kind_HOBJ {
                let pa = if va.is_null() || va.is_void() {
                    std::ptr::null_mut()
                } else {
                    va.as_ptr() as *mut hl::vdynamic
                };
                let pb = if vb.is_null() || vb.is_void() {
                    std::ptr::null_mut()
                } else {
                    vb.as_ptr() as *mut hl::vdynamic
                };
                if !pa.is_null() && !pb.is_null() {
                    // Same object ⇒ equal, whatever the type — including
                    // String, where identity implies content equality. Also
                    // skips two name decodes on the hot object-compare path.
                    if pa == pb {
                        return op == CmpOp::Eq;
                    }
                    let ta_name = self.dynamic_type_name(pa);
                    let tb_name = self.dynamic_type_name(pb);
                    if env_flag!("ASH_TRACE_EQ") {
                        eprintln!(
                            "[CMP-OBJ] f{} op={:?} ta={:?} tb={:?} pa={:#x} pb={:#x}",
                            func_idx,
                            op,
                            ta_name,
                            tb_name,
                            va.as_ptr(),
                            vb.as_ptr()
                        );
                    }
                    if ta_name == tb_name && matches!(ta_name.as_deref(), Some("String")) {
                        let sa = unsafe {
                            self.try_extract_string_object_raw(va.as_ptr() as *mut c_void)
                        };
                        let sb = unsafe {
                            self.try_extract_string_object_raw(vb.as_ptr() as *mut c_void)
                        };
                        if env_flag!("ASH_TRACE_EQ") {
                            eprintln!(
                                "[CMP-OBJ] f{} string-extract sa={} sb={}",
                                func_idx,
                                sa.is_some(),
                                sb.is_some()
                            );
                        }
                        if let (Some((ab, al)), Some((bb, bl))) = (sa, sb) {
                            let eq = al == bl && unsafe { Self::utf16_len_eq(ab, bb, al as usize) };
                            if env_flag!("ASH_TRACE_EQ") {
                                eprintln!(
                                    "[CMP] f{} op={:?} ak={} bk={} (string-obj) -> {}",
                                    func_idx, op, ak, bk, eq
                                );
                            }
                            return if op == CmpOp::Eq { eq } else { !eq };
                        }
                    }
                }
            }
            // A Dynamic register holding an object carries the raw object
            // pointer, exactly like an HOBJ register.  Optimisation can keep
            // the other operand at its concrete HOBJ kind (`DynamicString ==
            // "literal"` is one such shape), so restricting content-aware
            // comparison to HDYN/HDYN made equal Strings compare unequal.
            // Both layouts have an hl_type header and are safe inputs to
            // dynamic_eq; primitive mixed-kind cases take other paths.
            if (ak == hl::hl_type_kind_HDYN && bk == hl::hl_type_kind_HDYN)
                || (ak == hl::hl_type_kind_HDYN && bk == hl::hl_type_kind_HOBJ)
                || (ak == hl::hl_type_kind_HOBJ && bk == hl::hl_type_kind_HDYN)
            {
                let pa = if !va.is_ptr() || va.is_null() || va.is_void() {
                    std::ptr::null_mut()
                } else {
                    va.as_ptr() as *mut hl::vdynamic
                };
                let pb = if !vb.is_ptr() || vb.is_null() || vb.is_void() {
                    std::ptr::null_mut()
                } else {
                    vb.as_ptr() as *mut hl::vdynamic
                };
                let eq = unsafe { self.dynamic_value_eq(va, vb) };
                if env_flag!("ASH_TRACE_EQ") {
                    eprintln!(
                        "[CMP] f{} op={:?} ak={} bk={} va={:?} vb={:?} (dyn) -> {}",
                        func_idx, op, ak, bk, va, vb, eq
                    );
                    if !eq {
                        let ka_dyn = if pa.is_null()
                            || !Self::is_derefable_dynamic(pa)
                            || unsafe { (*pa).t.is_null() }
                        {
                            0
                        } else {
                            unsafe { (*(*pa).t).kind }
                        };
                        let kb_dyn = if pb.is_null()
                            || !Self::is_derefable_dynamic(pb)
                            || unsafe { (*pb).t.is_null() }
                        {
                            0
                        } else {
                            unsafe { (*(*pb).t).kind }
                        };
                        eprintln!(
                            "[CMP_DYN] ka_dyn={} kb_dyn={} pa={:#x} pb={:#x}",
                            ka_dyn, kb_dyn, pa as usize, pb as usize
                        );
                    }
                }
                return if op == CmpOp::Eq { eq } else { !eq };
            }
        }
        let result = va.compare(vb, op).unwrap_or(false);
        if env_flag!("ASH_TRACE_EQ") && (op == CmpOp::Eq || op == CmpOp::NotEq) {
            eprintln!(
                "[CMP] f{} op={:?} ak={} bk={} va={:?} vb={:?} -> {}",
                func_idx, op, ak, bk, va, vb, result
            );
        }
        result
    }

    #[allow(clippy::too_many_arguments)]
    unsafe fn try_compare_nullable_operands(
        &self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        a_idx: usize,
        va: NanBoxedValue,
        ak: hl::hl_type_kind,
        b_idx: usize,
        vb: NanBoxedValue,
        bk: hl::hl_type_kind,
        op: CmpOp,
    ) -> Option<bool> {
        if ak != hl::hl_type_kind_HNULL && bk != hl::hl_type_kind_HNULL {
            return None;
        }

        let (av, ak_eff) =
            self.normalize_nullable_compare_operand(bytecode, func, a_idx, ak, va)?;
        let (bv, bk_eff) =
            self.normalize_nullable_compare_operand(bytecode, func, b_idx, bk, vb)?;

        if av.is_none() || bv.is_none() {
            let eq = av.is_none() && bv.is_none();
            return Some(match op {
                CmpOp::Eq => eq,
                CmpOp::NotEq => !eq,
                _ => false,
            });
        }

        let av = av.unwrap();
        let bv = bv.unwrap();
        if let Some(result) = Self::compare_numeric_values(av, ak_eff, bv, bk_eff, op) {
            return Some(result);
        }

        if op == CmpOp::Eq || op == CmpOp::NotEq {
            if let Some(result) = av.compare(bv, op) {
                return Some(result);
            }
        }

        None
    }

    unsafe fn normalize_nullable_compare_operand(
        &self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        reg_idx: usize,
        reg_kind: hl::hl_type_kind,
        val: NanBoxedValue,
    ) -> Option<(Option<NanBoxedValue>, hl::hl_type_kind)> {
        if reg_kind != hl::hl_type_kind_HNULL {
            return Some((Some(val), reg_kind));
        }
        if val.is_null() || val.is_void() || (val.is_ptr() && val.as_ptr() == 0) {
            return Some((None, reg_kind));
        }
        if !val.is_ptr() {
            return Some((Some(val), reg_kind));
        }

        let reg_type_idx = match func.regs.get(reg_idx) {
            Some(r) => r.0,
            None => return Some((Some(val), reg_kind)),
        };
        let reg_type = match bytecode.types.get(reg_type_idx) {
            Some(t) => t,
            None => return Some((Some(val), reg_kind)),
        };
        let tparam_idx = match reg_type.tparam.as_ref() {
            Some(tp) => tp.0,
            None => return Some((Some(val), reg_kind)),
        };
        let inner_kind = match bytecode.types.get(tparam_idx) {
            Some(t) => t.kind,
            None => return Some((Some(val), reg_kind)),
        };

        if !Self::is_primitive_or_bytes_kind(inner_kind) {
            return Some((Some(val), reg_kind));
        }

        let d = val.as_ptr() as *mut hl::vdynamic;
        if d.is_null() {
            return Some((None, inner_kind));
        }
        if let Some(unboxed) = Self::unbox_dynamic_to_kind(d, inner_kind) {
            if unboxed.is_null() || unboxed.is_void() {
                return Some((None, inner_kind));
            }
            return Some((Some(unboxed), inner_kind));
        }

        Some((Some(val), reg_kind))
    }

    fn compare_numeric_values(
        av: NanBoxedValue,
        ak: hl::hl_type_kind,
        bv: NanBoxedValue,
        bk: hl::hl_type_kind,
        op: CmpOp,
    ) -> Option<bool> {
        if !Self::is_numeric_or_bool_kind(ak) || !Self::is_numeric_or_bool_kind(bk) {
            return None;
        }

        let has_float = ak == hl::hl_type_kind_HF32
            || ak == hl::hl_type_kind_HF64
            || bk == hl::hl_type_kind_HF32
            || bk == hl::hl_type_kind_HF64;

        if has_float {
            let l = Self::numeric_as_f64(av, ak)?;
            let r = Self::numeric_as_f64(bv, bk)?;
            return Some(match op {
                CmpOp::SLt | CmpOp::ULt => l < r,
                CmpOp::SGte | CmpOp::UGte => l >= r,
                CmpOp::SGt => l > r,
                CmpOp::SLte => l <= r,
                CmpOp::Eq => l == r,
                CmpOp::NotEq => l != r,
            });
        }

        match op {
            CmpOp::ULt | CmpOp::UGte => {
                let l = Self::numeric_as_u64(av, ak)?;
                let r = Self::numeric_as_u64(bv, bk)?;
                Some(match op {
                    CmpOp::ULt => l < r,
                    CmpOp::UGte => l >= r,
                    _ => unreachable!(),
                })
            }
            CmpOp::SLt | CmpOp::SGte | CmpOp::SGt | CmpOp::SLte | CmpOp::Eq | CmpOp::NotEq => {
                let l = Self::numeric_as_i64(av, ak)?;
                let r = Self::numeric_as_i64(bv, bk)?;
                Some(match op {
                    CmpOp::SLt => l < r,
                    CmpOp::SGte => l >= r,
                    CmpOp::SGt => l > r,
                    CmpOp::SLte => l <= r,
                    CmpOp::Eq => l == r,
                    CmpOp::NotEq => l != r,
                    _ => unreachable!(),
                })
            }
        }
    }

    fn numeric_as_f64(v: NanBoxedValue, kind: hl::hl_type_kind) -> Option<f64> {
        match kind {
            k if k == hl::hl_type_kind_HI32 => Some(v.as_i32() as f64),
            k if k == hl::hl_type_kind_HUI8 => Some((v.as_i32() as u8) as f64),
            k if k == hl::hl_type_kind_HUI16 => Some((v.as_i32() as u16) as f64),
            k if k == hl::hl_type_kind_HI64 => Some(v.as_i64_lossy() as f64),
            k if k == hl::hl_type_kind_HF32 || k == hl::hl_type_kind_HF64 => Some(v.as_f64()),
            k if k == hl::hl_type_kind_HBOOL => Some(if v.as_bool() { 1.0 } else { 0.0 }),
            _ => None,
        }
    }

    fn numeric_as_i64(v: NanBoxedValue, kind: hl::hl_type_kind) -> Option<i64> {
        match kind {
            k if k == hl::hl_type_kind_HI32 => Some(v.as_i32() as i64),
            k if k == hl::hl_type_kind_HUI8 => Some((v.as_i32() as u8) as i64),
            k if k == hl::hl_type_kind_HUI16 => Some((v.as_i32() as u16) as i64),
            k if k == hl::hl_type_kind_HI64 => Some(v.as_i64_lossy()),
            k if k == hl::hl_type_kind_HBOOL => Some(if v.as_bool() { 1 } else { 0 }),
            _ => None,
        }
    }

    fn numeric_as_u64(v: NanBoxedValue, kind: hl::hl_type_kind) -> Option<u64> {
        match kind {
            k if k == hl::hl_type_kind_HI32 => Some((v.as_i32() as u32) as u64),
            k if k == hl::hl_type_kind_HUI8 => Some((v.as_i32() as u8) as u64),
            k if k == hl::hl_type_kind_HUI16 => Some((v.as_i32() as u16) as u64),
            k if k == hl::hl_type_kind_HI64 => Some(v.as_i64_lossy() as u64),
            k if k == hl::hl_type_kind_HBOOL => Some(if v.as_bool() { 1 } else { 0 }),
            _ => None,
        }
    }

    unsafe fn utf16z_eq(a: *const u16, b: *const u16) -> bool {
        if a == b {
            return true;
        }
        if a.is_null() || b.is_null() {
            return false;
        }
        let mut i = 0usize;
        loop {
            let ca = *a.add(i);
            let cb = *b.add(i);
            if ca != cb {
                return false;
            }
            if ca == 0 {
                return true;
            }
            i += 1;
        }
    }

    /// The UTF-16 buffer behind a comparison operand, for HBYTES (which is
    /// NUL-terminated) and for a String object (which carries an explicit
    /// length). Returns None for anything that is not a string.
    unsafe fn string_operand_utf16(
        &self,
        v: NanBoxedValue,
        kind: hl::hl_type_kind,
    ) -> Option<(*const u16, i32)> {
        if v.is_null() || v.is_void() {
            return None;
        }
        if kind == hl::hl_type_kind_HBYTES {
            let p = v.as_ptr() as *const u16;
            if p.is_null() {
                return None;
            }
            let mut n = 0i32;
            while *p.add(n as usize) != 0 {
                n += 1;
            }
            return Some((p, n));
        }
        if kind == hl::hl_type_kind_HOBJ {
            let name = self.dynamic_type_name(v.as_ptr() as *mut hl::vdynamic);
            if !matches!(name.as_deref(), Some("String")) {
                return None;
            }
            return self.try_extract_string_object_raw(v.as_ptr() as *mut c_void);
        }
        None
    }

    /// Lexicographic order over UTF-16 code units, shorter-is-less on a
    /// common prefix — the ordering `hl_dyn_compare` gives strings, and the
    /// one Haxe's `<` on String is defined to produce.
    unsafe fn utf16_cmp(a: *const u16, alen: i32, b: *const u16, blen: i32) -> std::cmp::Ordering {
        let n = alen.min(blen).max(0) as usize;
        for i in 0..n {
            let (x, y) = (*a.add(i), *b.add(i));
            if x != y {
                return x.cmp(&y);
            }
        }
        alen.cmp(&blen)
    }

    unsafe fn utf16_len_eq(a: *const u16, b: *const u16, len: usize) -> bool {
        if a.is_null() || b.is_null() {
            return false;
        }
        for i in 0..len {
            if *a.add(i) != *b.add(i) {
                return false;
            }
        }
        true
    }

    unsafe fn try_extract_string_object_raw(
        &self,
        obj_ptr: *mut c_void,
    ) -> Option<(*const u16, i32)> {
        if obj_ptr.is_null() || self.fn_get_obj_rt.is_null() {
            return None;
        }
        let type_ptr = *(obj_ptr as *const *mut hl::hl_type);
        if type_ptr.is_null() || (*type_ptr).kind != hl::hl_type_kind_HOBJ {
            return None;
        }
        let bytes_val = Self::read_obj_field(
            obj_ptr as *mut u8,
            0,
            hl::hl_type_kind_HBYTES,
            type_ptr as *mut c_void,
            hl::hl_type_kind_HOBJ,
            self.fn_get_obj_rt,
        );
        let len_val = Self::read_obj_field(
            obj_ptr as *mut u8,
            1,
            hl::hl_type_kind_HI32,
            type_ptr as *mut c_void,
            hl::hl_type_kind_HOBJ,
            self.fn_get_obj_rt,
        );
        if bytes_val.is_null() || len_val.is_null() || len_val.is_void() {
            return None;
        }
        let bytes = bytes_val.as_ptr() as *const u16;
        let len = len_val.as_i32();
        if bytes.is_null() || len < 0 {
            return None;
        }
        Some((bytes, len))
    }

    unsafe fn dynamic_eq(&self, a: *mut hl::vdynamic, b: *mut hl::vdynamic) -> bool {
        if a == b {
            return true;
        }
        if a.is_null() || b.is_null() {
            return false;
        }
        // Unboxed payloads in Dynamic slots are not boxes — see
        // is_derefable_dynamic. Distinct non-box words are simply unequal
        // (the identity case above already answered equal ones).
        if !Self::is_derefable_dynamic(a) || !Self::is_derefable_dynamic(b) {
            return false;
        }
        let ta = (*a).t;
        let tb = (*b).t;
        if ta.is_null() || tb.is_null() {
            return false;
        }
        let ka = (*ta).kind;
        let kb = (*tb).kind;
        match (ka, kb) {
            (ka, kb)
                if matches!(ka, hl::hl_type_kind_HOBJ | hl::hl_type_kind_HDYNOBJ)
                    && kb == hl::hl_type_kind_HVIRTUAL =>
            {
                let value = (*(b as *mut hl::vvirtual)).value;
                return !value.is_null() && self.dynamic_eq(a, value);
            }
            (ka, kb)
                if ka == hl::hl_type_kind_HVIRTUAL
                    && matches!(kb, hl::hl_type_kind_HOBJ | hl::hl_type_kind_HDYNOBJ) =>
            {
                let value = (*(a as *mut hl::vvirtual)).value;
                return !value.is_null() && self.dynamic_eq(value, b);
            }
            (ka, kb) if ka == hl::hl_type_kind_HVIRTUAL && kb == hl::hl_type_kind_HVIRTUAL => {
                let av = (*(a as *mut hl::vvirtual)).value;
                let bv = (*(b as *mut hl::vvirtual)).value;
                // HashLink reports an invalid comparison for two distinct
                // self-backed virtual records. For equality that means false,
                // not "compare their null value slots as equal".
                return !av.is_null() && !bv.is_null() && self.dynamic_eq(av, bv);
            }
            _ => {}
        }
        if ka == kb {
            return match ka {
                k if k == hl::hl_type_kind_HI32 => (*a).v.i == (*b).v.i,
                k if k == hl::hl_type_kind_HUI8 => (*a).v.ui8 == (*b).v.ui8,
                k if k == hl::hl_type_kind_HUI16 => (*a).v.ui16 == (*b).v.ui16,
                k if k == hl::hl_type_kind_HI64 => (*a).v.i64_ == (*b).v.i64_,
                k if k == hl::hl_type_kind_HF32 => (*a).v.f == (*b).v.f,
                k if k == hl::hl_type_kind_HF64 => (*a).v.d == (*b).v.d,
                k if k == hl::hl_type_kind_HBOOL => (*a).v.b == (*b).v.b,
                k if k == hl::hl_type_kind_HBYTES => {
                    Self::utf16z_eq((*a).v.bytes as *const u16, (*b).v.bytes as *const u16)
                }
                _ => {
                    if ka == hl::hl_type_kind_HOBJ {
                        let ta_name = self.dynamic_type_name(a);
                        let tb_name = self.dynamic_type_name(b);
                        if ta_name == tb_name && matches!(ta_name.as_deref(), Some("String")) {
                            if let (Some((ab, al)), Some((bb, bl))) = (
                                self.try_extract_string_object_raw(a.cast()),
                                self.try_extract_string_object_raw(b.cast()),
                            ) {
                                return al == bl && Self::utf16_len_eq(ab, bb, al as usize);
                            }
                        }

                        // `a` and `b` are the objects themselves, not boxes
                        // whose payload starts at `v.ptr`.  Reading that union
                        // member therefore reads offset 8 of the object -- its
                        // first field.  Distinct objects with the same first
                        // field consequently compared equal (two IntWrap(1)
                        // instances made Array.remove remove the wrong one).
                        // Strings are the content-equality exception handled
                        // above; every other object uses identity, matching
                        // hlp_dyn_compare's HOBJ fallback.
                        return false;
                    }
                    if ka == hl::hl_type_kind_HENUM {
                        // Enum values are heap objects whose first word is
                        // their hl_type*.  They are not vdynamic boxes, so
                        // reading `v.ptr` observes the constructor index at
                        // offset 8.  That made any two zero-argument enum
                        // values with the same constructor index compare
                        // equal, even when they belonged to different enum
                        // types.  HashLink's HENUM/HENUM comparison is pointer
                        // identity; the equal-pointer case was handled above.
                        return false;
                    }
                    (*a).v.ptr == (*b).v.ptr
                }
            };
        }
        // Cross-kind numeric equality (e.g. Int dynamic vs Float dynamic)
        let a_num = match ka {
            k if k == hl::hl_type_kind_HI32 => Some((*a).v.i as f64),
            k if k == hl::hl_type_kind_HUI8 => Some((*a).v.ui8 as f64),
            k if k == hl::hl_type_kind_HUI16 => Some((*a).v.ui16 as f64),
            k if k == hl::hl_type_kind_HI64 => Some((*a).v.i64_ as f64),
            k if k == hl::hl_type_kind_HF32 => Some((*a).v.f as f64),
            k if k == hl::hl_type_kind_HF64 => Some((*a).v.d),
            _ => None,
        };
        let b_num = match kb {
            k if k == hl::hl_type_kind_HI32 => Some((*b).v.i as f64),
            k if k == hl::hl_type_kind_HUI8 => Some((*b).v.ui8 as f64),
            k if k == hl::hl_type_kind_HUI16 => Some((*b).v.ui16 as f64),
            k if k == hl::hl_type_kind_HI64 => Some((*b).v.i64_ as f64),
            k if k == hl::hl_type_kind_HF32 => Some((*b).v.f as f64),
            k if k == hl::hl_type_kind_HF64 => Some((*b).v.d),
            _ => None,
        };
        match (a_num, b_num) {
            (Some(x), Some(y)) => x == y,
            _ => false,
        }
    }

    unsafe fn dynamic_value_eq(&self, a: NanBoxedValue, b: NanBoxedValue) -> bool {
        if a.is_null() || a.is_void() || b.is_null() || b.is_void() {
            return (a.is_null() || a.is_void()) && (b.is_null() || b.is_void());
        }
        if a.is_ptr() && b.is_ptr() {
            return self.dynamic_eq(
                a.as_ptr() as *mut hl::vdynamic,
                b.as_ptr() as *mut hl::vdynamic,
            );
        }
        if a.raw_bits() == b.raw_bits() {
            return true;
        }

        match (Self::dynamic_scalar(a), Self::dynamic_scalar(b)) {
            (Some(DynamicScalar::Int(x)), Some(DynamicScalar::Int(y))) => x == y,
            (Some(DynamicScalar::Float(x)), Some(DynamicScalar::Float(y))) => x == y,
            (Some(DynamicScalar::Int(x)), Some(DynamicScalar::Float(y))) => x as f64 == y,
            (Some(DynamicScalar::Float(x)), Some(DynamicScalar::Int(y))) => x == y as f64,
            (Some(DynamicScalar::Bool(x)), Some(DynamicScalar::Bool(y))) => x == y,
            _ => false,
        }
    }

    unsafe fn dynamic_value_cmp(
        &self,
        a: NanBoxedValue,
        b: NanBoxedValue,
    ) -> Option<std::cmp::Ordering> {
        use std::cmp::Ordering;

        let a_null = a.is_null() || a.is_void();
        let b_null = b.is_null() || b.is_void();
        if a_null || b_null {
            return Some(match (a_null, b_null) {
                (true, true) => Ordering::Equal,
                (true, false) => Ordering::Less,
                (false, true) => Ordering::Greater,
                _ => unreachable!(),
            });
        }
        if a.raw_bits() == b.raw_bits() {
            return Some(Ordering::Equal);
        }

        let scalar_number = |v| match v {
            DynamicScalar::Int(x) => x as f64,
            DynamicScalar::Float(x) => x,
            DynamicScalar::Bool(x) => {
                if x {
                    1.0
                } else {
                    0.0
                }
            }
        };
        if let (Some(x), Some(y)) = (Self::dynamic_scalar(a), Self::dynamic_scalar(b)) {
            // Match hl_dyn_compare: NaN is neither less nor greater, so it
            // compares equal for ordering purposes.
            let (x, y) = (scalar_number(x), scalar_number(y));
            return Some(if x < y {
                Ordering::Less
            } else if x > y {
                Ordering::Greater
            } else {
                Ordering::Equal
            });
        }

        if a.is_ptr() && b.is_ptr() {
            let (ap, bp) = (
                a.as_ptr() as *mut hl::vdynamic,
                b.as_ptr() as *mut hl::vdynamic,
            );
            if Self::is_derefable_dynamic(ap)
                && Self::is_derefable_dynamic(bp)
                && !(*ap).t.is_null()
                && !(*bp).t.is_null()
            {
                let (ak, bk) = ((*(*ap).t).kind, (*(*bp).t).kind);
                if ak == hl::hl_type_kind_HBYTES && bk == hl::hl_type_kind_HBYTES {
                    let (ab, bb) = ((*ap).v.bytes as *const u16, (*bp).v.bytes as *const u16);
                    let mut al = 0i32;
                    let mut bl = 0i32;
                    while !ab.is_null() && *ab.add(al as usize) != 0 {
                        al += 1;
                    }
                    while !bb.is_null() && *bb.add(bl as usize) != 0 {
                        bl += 1;
                    }
                    return Some(Self::utf16_cmp(ab, al, bb, bl));
                }
                if ak == hl::hl_type_kind_HOBJ && bk == hl::hl_type_kind_HOBJ {
                    if let (Some((ab, al)), Some((bb, bl))) = (
                        self.try_extract_string_object_raw(ap.cast()),
                        self.try_extract_string_object_raw(bp.cast()),
                    ) {
                        return Some(Self::utf16_cmp(ab, al, bb, bl));
                    }
                }
            }
            return Some(a.as_ptr().cmp(&b.as_ptr()));
        }

        None
    }

    unsafe fn dynamic_scalar(v: NanBoxedValue) -> Option<DynamicScalar> {
        if v.is_i32() {
            return Some(DynamicScalar::Int(v.as_i32() as i64));
        }
        if v.is_i64() {
            return Some(DynamicScalar::Int(v.as_i64_lossy()));
        }
        if v.is_f64() {
            return Some(DynamicScalar::Float(v.as_f64()));
        }
        if v.is_bool() {
            return Some(DynamicScalar::Bool(v.as_bool()));
        }
        if !v.is_ptr() {
            return None;
        }

        let d = v.as_ptr() as *mut hl::vdynamic;
        if !Self::is_derefable_dynamic(d) || (*d).t.is_null() {
            return None;
        }
        match (*(*d).t).kind {
            hl::hl_type_kind_HI32 => Some(DynamicScalar::Int((*d).v.i as i64)),
            hl::hl_type_kind_HUI8 => Some(DynamicScalar::Int((*d).v.ui8 as i64)),
            hl::hl_type_kind_HUI16 => Some(DynamicScalar::Int((*d).v.ui16 as i64)),
            hl::hl_type_kind_HI64 => Some(DynamicScalar::Int((*d).v.i64_)),
            hl::hl_type_kind_HF32 => Some(DynamicScalar::Float((*d).v.f as f64)),
            hl::hl_type_kind_HF64 => Some(DynamicScalar::Float((*d).v.d)),
            hl::hl_type_kind_HBOOL => Some(DynamicScalar::Bool((*d).v.b)),
            _ => None,
        }
    }

    /// Whether a word from a Dynamic-typed slot can be dereferenced as a
    /// `vdynamic`.
    ///
    /// Slots typed Dynamic do not always hold a box: an unboxed small value
    /// (a bool's 0x1, a raw enum index) arrives as a "pointer", and under
    /// Rust's UB checks dereferencing one is a misaligned-pointer ABORT —
    /// the whole VM dies where a C runtime would have read garbage. A real
    /// box is word-aligned and above the first page.
    #[inline]
    fn is_derefable_dynamic(d: *const hl::vdynamic) -> bool {
        let addr = d as usize;
        addr >= 0x10000 && addr.is_multiple_of(std::mem::align_of::<usize>())
    }

    unsafe fn unbox_dynamic_to_kind(
        d: *mut hl::vdynamic,
        dst_kind: hl::hl_type_kind,
    ) -> Option<NanBoxedValue> {
        if !Self::is_derefable_dynamic(d) {
            return None;
        }
        if d.is_null() || !Self::is_derefable_dynamic((*d).t.cast()) {
            return None;
        }
        let sk = (*(*d).t).kind;
        let as_i64 = match sk {
            hl::hl_type_kind_HI32 => Some((*d).v.i as i64),
            hl::hl_type_kind_HUI8 => Some((*d).v.ui8 as i64),
            hl::hl_type_kind_HUI16 => Some((*d).v.ui16 as i64),
            hl::hl_type_kind_HI64 => Some((*d).v.i64_),
            hl::hl_type_kind_HF32 => Some((*d).v.f as i64),
            hl::hl_type_kind_HF64 => Some((*d).v.d as i64),
            hl::hl_type_kind_HBOOL => Some(if (*d).v.b { 1 } else { 0 }),
            _ => None,
        };
        let as_f64 = match sk {
            hl::hl_type_kind_HI32 => Some((*d).v.i as f64),
            hl::hl_type_kind_HUI8 => Some((*d).v.ui8 as f64),
            hl::hl_type_kind_HUI16 => Some((*d).v.ui16 as f64),
            hl::hl_type_kind_HI64 => Some((*d).v.i64_ as f64),
            hl::hl_type_kind_HF32 => Some((*d).v.f as f64),
            hl::hl_type_kind_HF64 => Some((*d).v.d),
            hl::hl_type_kind_HBOOL => Some(if (*d).v.b { 1.0 } else { 0.0 }),
            _ => None,
        };
        match dst_kind {
            hl::hl_type_kind_HI32 => as_i64.map(|v| NanBoxedValue::from_i32(v as i32)),
            hl::hl_type_kind_HUI8 => as_i64.map(|v| NanBoxedValue::from_i32((v as u8) as i32)),
            hl::hl_type_kind_HUI16 => as_i64.map(|v| NanBoxedValue::from_i32((v as u16) as i32)),
            hl::hl_type_kind_HI64 => as_i64.map(NanBoxedValue::from_i64),
            hl::hl_type_kind_HF32 | hl::hl_type_kind_HF64 => as_f64.map(NanBoxedValue::from_f64),
            hl::hl_type_kind_HBOOL => as_i64.map(|v| NanBoxedValue::from_bool(v != 0)),
            hl::hl_type_kind_HBYTES => {
                if sk == hl::hl_type_kind_HBYTES {
                    Some(NanBoxedValue::from_bytes_ptr((*d).v.bytes as usize))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// How many fields this type inherits, so a binding's chain-wide field
    /// index can be turned back into one of the type's own fields.
    fn inherited_field_count(
        bytecode: &DecodedBytecode,
        obj: &ash_core::types::HLTypeObj,
    ) -> usize {
        let mut count = 0usize;
        let mut parent = obj.super_.clone();
        // Bounded by the type table: a malformed chain must not spin here.
        for _ in 0..bytecode.types.len() {
            let Some(tref) = parent.clone() else { break };
            let Some(sup) = bytecode.types.get(tref.0).and_then(|t| t.obj.as_ref()) else {
                break;
            };
            count += sup.fields.len();
            parent = sup.super_.clone();
        }
        count
    }

    /// findex -> "Class.method", built once from the type tables.
    ///
    /// A function does not carry its own name: `HLFunction::field_name` is set
    /// only in the cases the reader can attribute directly, so most functions
    /// render as `Fun_<findex>` and a stack trace naming five of those is
    /// barely a trace at all. The names live on the TYPES — every `obj` lists
    /// its protos (method name + findex) and its bindings (field index +
    /// findex) — so one pass over the type table recovers them.
    ///
    /// Built lazily and only when something needs to print a trace, so a run
    /// that never faults never pays for it.
    /// findex -> "Class.method", built once per bytecode image.
    ///
    /// This is a pure function of the image, and building it walks every type,
    /// every proto and every binding, allocating a String for each. Rebuilding
    /// it per call made throwing an exception cost O(program): MBHaxe throws
    /// while loading a level, and the loading screen sat in this function
    /// through thousands of rebuilds. A trace is only ever read once, and only
    /// if it escapes -- the table has no business being rebuilt to produce it.
    fn function_name_table(&self, bytecode: &DecodedBytecode) -> NameTable {
        thread_local! {
            static CACHE: NameTableCache = const { RefCell::new(None) };
        }
        let key = bytecode as *const DecodedBytecode as usize;
        if let Some(hit) = CACHE.with(|c| {
            c.borrow()
                .as_ref()
                .and_then(|(k, t)| (*k == key).then(|| Rc::clone(t)))
        }) {
            return hit;
        }
        let table = Rc::new(self.build_function_name_table(bytecode));
        CACHE.with(|c| *c.borrow_mut() = Some((key, Rc::clone(&table))));
        table
    }

    fn build_function_name_table(&self, bytecode: &DecodedBytecode) -> HashMap<usize, String> {
        let mut names: HashMap<usize, String> = HashMap::new();
        for ty in &bytecode.types {
            let Some(obj) = ty.obj.as_ref() else { continue };
            for proto in &obj.proto {
                if proto.findex >= 0 {
                    names.insert(proto.findex as usize, format!("{}.{}", obj.name, proto.name));
                }
            }
            // Bindings are (field index, findex) pairs — how a class's STATIC
            // functions are named, which is most of what a Haxe stack trace
            // shows. The index counts from the top of the inheritance chain,
            // not from this type's own fields: `$NullAcc` binds field 6 while
            // owning only two, because its parent contributes the first five.
            let inherited = Self::inherited_field_count(bytecode, obj);
            let (pairs, _) = obj.bindings.as_chunks::<2>();
            for &[field_idx, findex] in pairs {
                if findex < 0 {
                    continue;
                }
                let own = usize::try_from(field_idx)
                    .ok()
                    .and_then(|i| i.checked_sub(inherited));
                if let Some(field) = own.and_then(|i| obj.fields.get(i)) {
                    // `$Name` is HashLink's own marker for a class's statics
                    // type; upstream traces read `Name.method`.
                    let owner = obj.name.strip_prefix('$').unwrap_or(&obj.name);
                    names
                        .entry(findex as usize)
                        .or_insert_with(|| format!("{owner}.{}", field.name));
                }
            }
        }
        names
    }

    /// The interpreted call stack as HashLink reports it: innermost first,
    /// `Class.method(file:line)` per frame, using the debug info the bytecode
    /// already carries.
    fn capture_call_stack(&self, bytecode: &DecodedBytecode) -> Vec<String> {
        let bc = self.reloaded_bytecode.unwrap_or(bytecode);
        let names = self.function_name_table(bc);
        self.stack
            .iter()
            .rev()
            .map(|frame| {
                // Never drop a frame: a trace that silently omits the frames
                // it could not name is worse than one that admits them, since
                // the gap is invisible and the caller looks like the callee.
                let Some(func) = bc.functions.get(frame.function_index) else {
                    return format!("<unresolved findex {}>", frame.function_index);
                };
                let name = names
                    .get(&(func.findex as usize))
                    .cloned()
                    .unwrap_or_else(|| func.name());
                let debug_pc = frame.pc.min(func.ops.len().saturating_sub(1));
                let file_idx = func.debug.get(debug_pc * 2).copied().unwrap_or(-1);
                let line = func.debug.get(debug_pc * 2 + 1).copied().unwrap_or(0);
                match usize::try_from(file_idx)
                    .ok()
                    .and_then(|i| bc.debug_files.get(i))
                {
                    Some(file) => format!("{name}({file}:{line})"),
                    // No debug info (a release build): the name alone still
                    // says which function, which beats printing nothing.
                    None => name,
                }
            })
            .collect()
    }

    fn stack_symbol_for_function(
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        pc: usize,
    ) -> Box<[u16]> {
        let debug_pc = pc.min(func.ops.len().saturating_sub(1));
        let file_idx = func.debug.get(debug_pc * 2).copied().unwrap_or(-1);
        let line = func.debug.get(debug_pc * 2 + 1).copied().unwrap_or(0);
        let file = usize::try_from(file_idx)
            .ok()
            .and_then(|idx| bytecode.debug_files.get(idx))
            .map(String::as_str)
            .unwrap_or("unknown");
        let mut symbol: Vec<u16> = format!("fun${}({file}:{line})", func.findex)
            .encode_utf16()
            .collect();
        symbol.push(0);
        symbol.into_boxed_slice()
    }

    fn stack_symbol(
        bytecode: &DecodedBytecode,
        function_index: usize,
        pc: usize,
    ) -> Option<Box<[u16]>> {
        let func = bytecode.functions.get(function_index)?;
        Some(Self::stack_symbol_for_function(bytecode, func, pc))
    }

    fn interpreter_stack_symbol(
        &self,
        bytecode: &DecodedBytecode,
        function_index: usize,
        pc: usize,
    ) -> Option<Box<[u16]>> {
        bytecode.functions.get(function_index)?;
        // AIR V2's serializer renumbers opcodes. Cache::prepare builds a
        // matching debug table for that optimized body, so frame.pc must be
        // resolved against the body the interpreter actually executes rather
        // than the original bytecode function at the same numeric index.
        let func = self.air.body(bytecode, function_index);
        Some(Self::stack_symbol_for_function(bytecode, func, pc))
    }

    /// Return true when the loader owns `pc` as part of the executable or a
    /// shared library. JIT code lives in anonymous executable mappings, so a
    /// loader-owned address must never be fed to the nearest-JIT-entry
    /// fallback: doing so made ASLR occasionally report an unrelated Haxe
    /// function for one of Ash's own native stack frames.
    #[cfg(unix)]
    fn native_image_owns_pc(pc: usize) -> bool {
        if pc == 0 {
            return false;
        }
        // Answered per PAGE, because `dladdr` is not cheap: it walks the
        // loaded images to find the closest symbol, and this runs for every
        // frame of every captured stack. On MBHaxe, which throws while
        // loading a level, dyld's findClosestSymbol was the busiest non-idle
        // leaf in the whole process. Ownership is a property of the mapping,
        // so every address in a page shares one answer, and a stack walk
        // revisits the same handful of pages over and over.
        const PAGE: usize = 4096;
        thread_local! {
            static OWNED: RefCell<HashMap<usize, bool>> =
                RefCell::new(HashMap::new());
        }
        let page = pc / PAGE;
        if let Some(hit) = OWNED.with(|c| c.borrow().get(&page).copied()) {
            return hit;
        }
        let owned = unsafe {
            let mut info: libc::Dl_info = std::mem::zeroed();
            libc::dladdr(pc as *const c_void, &mut info) != 0 && !info.dli_fbase.is_null()
        };
        OWNED.with(|c| {
            let mut m = c.borrow_mut();
            // JIT code is mapped and unmapped over a run, so the map is a
            // cache and not a registry: bound it rather than let a long run
            // accumulate a page entry per compiled function.
            if m.len() > 8192 {
                m.clear();
            }
            m.insert(page, owned);
        });
        owned
    }

    #[cfg(not(unix))]
    fn native_image_owns_pc(_pc: usize) -> bool {
        false
    }

    /// Capture return addresses from the native stack. Generated code ranges
    /// are registered by both AIR V2 backends, so this works for Cranelift,
    /// LLVM promotion, and a stack containing frames from both tiers.
    fn compiled_stack_functions(&self, _frame_hint: *const usize) -> Vec<usize> {
        const MAX_FRAMES: usize = 256;
        let mut functions = Vec::new();

        #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
        if !_frame_hint.is_null() {
            unsafe {
                let mut attr: libc::pthread_attr_t = std::mem::zeroed();
                let mut stack_base = std::ptr::null_mut::<c_void>();
                let mut stack_size = 0usize;
                let have_attr = libc::pthread_getattr_np(libc::pthread_self(), &mut attr) == 0;
                let have_bounds = have_attr
                    && libc::pthread_attr_getstack(&attr, &mut stack_base, &mut stack_size) == 0;
                if have_attr {
                    libc::pthread_attr_destroy(&mut attr);
                }

                if have_bounds {
                    let stack_low = stack_base as usize;
                    let stack_high = stack_low.saturating_add(stack_size);
                    let mut frame = _frame_hint as usize;
                    for _ in 0..MAX_FRAMES {
                        if frame < stack_low
                            || frame > stack_high.saturating_sub(2 * std::mem::size_of::<usize>())
                            || !frame.is_multiple_of(std::mem::align_of::<usize>())
                        {
                            break;
                        }
                        let words = frame as *const usize;
                        let caller = *words;
                        let return_pc = *words.add(1);
                        if !Self::native_image_owns_pc(return_pc) {
                            if let Some((findex, _, _)) =
                                ash_core::profile::describe_jit_pc(return_pc)
                            {
                                if let Some(function_index) =
                                    func_of(&self.targets, findex as usize)
                                {
                                    if functions.last().copied() != Some(function_index) {
                                        functions.push(function_index);
                                    }
                                }
                            }
                        }
                        if caller <= frame
                            || caller >= stack_high
                            || caller - frame > stack_size
                            || !caller.is_multiple_of(std::mem::align_of::<usize>())
                        {
                            break;
                        }
                        frame = caller;
                    }
                }
            }
        }

        if !functions.is_empty() {
            return functions;
        }

        let mut pcs = [std::ptr::null_mut::<c_void>(); MAX_FRAMES];

        #[cfg(unix)]
        let count = unsafe { libc::backtrace(pcs.as_mut_ptr(), MAX_FRAMES as i32).max(0) as usize };

        #[cfg(windows)]
        let count = unsafe {
            windows_sys::Win32::System::Diagnostics::Debug::RtlCaptureStackBackTrace(
                0,
                MAX_FRAMES as u32,
                pcs.as_mut_ptr(),
                std::ptr::null_mut(),
            ) as usize
        };

        #[cfg(not(any(unix, windows)))]
        let count = 0;

        for pc in pcs.iter().take(count) {
            if Self::native_image_owns_pc(*pc as usize) {
                continue;
            }
            let Some((findex, _, _)) = ash_core::profile::describe_jit_pc(*pc as usize) else {
                continue;
            };
            let Some(function_index) = func_of(&self.targets, findex as usize) else {
                continue;
            };
            if functions.last().copied() != Some(function_index) {
                functions.push(function_index);
            }
        }
        functions
    }

    /// Render the live interpreter and generated-code frames as HashLink
    /// `hl_symbol` tokens.
    ///
    /// The public ABI treats a symbol as opaque until `resolve_symbol`; using
    /// a stable UTF-16 buffer address as the token lets that second call return
    /// the already-rendered value without exposing Rust frame storage to Haxe.
    fn stack_symbols(
        &self,
        bytecode: &DecodedBytecode,
        frame_hint: *const usize,
    ) -> Vec<Box<[u16]>> {
        let compiled = self.compiled_stack_functions(frame_hint);
        let mut symbols: Vec<Box<[u16]>> = compiled
            .iter()
            // Cranelift does not currently expose per-instruction native PC
            // offsets. Use the function's first debug position; the opaque
            // token remains structurally valid and identifies the exact Haxe
            // function while source-map plumbing is added independently.
            .filter_map(|&function_index| Self::stack_symbol(bytecode, function_index, 0))
            .collect();
        let mut last = compiled.last().copied();
        for &function_index in self.jit_bridge_callers.iter().rev() {
            if last == Some(function_index) {
                continue;
            }
            if let Some(symbol) = Self::stack_symbol(bytecode, function_index, 0) {
                symbols.push(symbol);
                last = Some(function_index);
            }
        }
        for frame in self.stack.iter().rev() {
            if last == Some(frame.function_index) {
                continue;
            }
            if let Some(symbol) =
                self.interpreter_stack_symbol(bytecode, frame.function_index, frame.pc)
            {
                symbols.push(symbol);
                last = Some(frame.function_index);
            }
        }

        // NativeStackTrace deliberately discards the outermost raw entry.
        // HashLink's platform unwinders naturally include a C runtime frame;
        // append an equivalent opaque terminator so the last Haxe frame is
        // retained even when Ash filters all non-JIT PCs above.
        if !symbols.is_empty() {
            let mut terminator: Vec<u16> = "fun$0(unknown:0)".encode_utf16().collect();
            terminator.push(0);
            symbols.push(terminator.into_boxed_slice());
        }
        symbols
    }

    fn prepare_call_stack(
        &mut self,
        bytecode: &DecodedBytecode,
        frame_hint: *const usize,
    ) -> usize {
        let symbols = self.stack_symbols(bytecode, frame_hint);
        self.call_stack_symbols = symbols
            .iter()
            .map(|symbol| symbol.as_ptr() as usize)
            .collect();
        self.stack_symbol_arena.extend(symbols);
        self.call_stack_symbols.len()
    }

    unsafe fn write_call_stack(&mut self, output: *mut *mut c_void, capacity: i32) -> i32 {
        if !output.is_null() {
            for (index, symbol) in self
                .call_stack_symbols
                .iter()
                .take(capacity.max(0) as usize)
                .enumerate()
            {
                *output.add(index) = *symbol as *mut c_void;
            }
        }
        self.call_stack_symbols.len() as i32
    }

    fn capture_exception_stack(&mut self, bytecode: &DecodedBytecode) {
        self.prepare_call_stack(bytecode, std::ptr::null());
        self.exception_stack_symbols = self.call_stack_symbols.clone();
    }

    fn stack_raw_native(
        &mut self,
        bytecode: &DecodedBytecode,
        args: &[NanBoxedValue],
        exception: bool,
    ) -> Result<NanBoxedValue> {
        if exception {
            if self.exception_stack_symbols.is_empty() {
                self.capture_exception_stack(bytecode);
            }
        } else {
            self.prepare_call_stack(bytecode, std::ptr::null());
        }

        let symbols = if exception {
            &self.exception_stack_symbols
        } else {
            &self.call_stack_symbols
        };
        if let Some(arr) = args.first().filter(|v| !v.is_null() && !v.is_void()) {
            let arr = arr.as_ptr() as *mut hl::varray;
            if !arr.is_null() {
                let capacity = unsafe { (*arr).size.max(0) as usize };
                let data = unsafe {
                    (arr as *mut u8).add(std::mem::size_of::<hl::varray>()) as *mut *const u16
                };
                for (i, symbol) in symbols.iter().take(capacity).enumerate() {
                    unsafe { *data.add(i) = *symbol as *const u16 };
                }
            }
        }
        Ok(NanBoxedValue::from_i32(symbols.len() as i32))
    }

    /// Call a native function via FFI.
    fn call_native(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        native_idx: usize,
        args: &[NanBoxedValue],
    ) -> Result<NanBoxedValue> {
        let native = &bytecode.natives[native_idx];

        // Trace every native call when ASH_TRACE_NATIVE is set
        if env_flag!("ASH_TRACE_NATIVE") {
            eprintln!(
                "[trace] native hlp_{} lib={} args={}",
                native.name,
                native.lib,
                args.len()
            );
        }

        let debug_native = env_flag!("ASH_DBG_NATIVE");
        let func_name = HlpName(&native.name);
        if debug_native
            && (native.name.contains("compare")
                || native.name.contains("eq")
                || native.name.contains("trim")
                || native.name.contains("date"))
        {
            eprintln!("[NATIVE] {} args={} vals={:?}", func_name, args.len(), args);
        }
        let debug_dyn = env_flag!("ASH_DBG_DYN");
        if debug_dyn
            && (native.name == "hash"
                || native.name == "obj_get_field"
                || native.name == "obj_set_field"
                || native.name == "obj_has_field"
                || native.name == "obj_delete_field"
                || native.name == "no_closure"
                || native.name == "get_closure_value"
                || native.name == "call_method")
        {
            eprintln!(
                "[NATIVE-DYN] {} args={} vals={:?}",
                func_name,
                args.len(),
                args
            );
            if (native.name == "obj_get_field"
                || native.name == "obj_set_field"
                || native.name == "obj_has_field"
                || native.name == "obj_delete_field"
                || native.name == "no_closure"
                || native.name == "get_closure_value"
                || native.name == "call_method")
                && !args.is_empty()
                && args[0].is_ptr()
                && args[0].as_ptr() != 0
            {
                let d = args[0].as_ptr() as *mut hl::vdynamic;
                unsafe {
                    if !d.is_null() && !(*d).t.is_null() {
                        eprintln!(
                            "[NATIVE-DYN] {} obj_kind={} obj_t={:p}",
                            func_name,
                            (*(*d).t).kind,
                            (*d).t
                        );
                    }
                    if native.name == "obj_set_field"
                        && args.len() >= 3
                        && args[2].is_ptr()
                        && args[2].as_ptr() != 0
                    {
                        let v = args[2].as_ptr() as *mut hl::vdynamic;
                        if !v.is_null() && !(*v).t.is_null() {
                            eprintln!(
                                "[NATIVE-DYN] {} val_kind={} val_t={:p}",
                                func_name,
                                (*(*v).t).kind,
                                (*v).t
                            );
                        }
                    }
                }
            }
        }

        // Intercept sort natives: they call back into bytecode closures via C function pointers,
        // which doesn't work in interpreter mode. Implement sorting here instead.
        match native.name.as_str() {
            "call_stack_raw" => return self.stack_raw_native(bytecode, args, false),
            "exception_stack_raw" => return self.stack_raw_native(bytecode, args, true),
            "resolve_symbol" => {
                let symbol = args.first().copied().unwrap_or_else(NanBoxedValue::null);
                return Ok(if symbol.is_null() || symbol.is_void() {
                    NanBoxedValue::null()
                } else {
                    NanBoxedValue::from_bytes_ptr(symbol.as_ptr())
                });
            }
            "bsort_i32" => return self.sort_bytes_i32(bytecode, native_resolver, args),
            "bsort_f64" => return self.sort_bytes_f64(bytecode, native_resolver, args),
            "bsort_i64" => return self.sort_bytes_i64(bytecode, native_resolver, args),
            "call_method" => {
                if let Some(v) =
                    self.try_handle_call_method_native(bytecode, native_resolver, args)?
                {
                    return Ok(v);
                }
            }
            // Reflect/hl.Api field operations go to the ash_std natives like
            // every other caller. They used to be intercepted into
            // interpreter-PRIVATE shadow HashMaps ("HVIRTUAL fallback
            // storage") that the real object never learned about — state in
            // one world again, and the reason a Reflect.setField was lost
            // the moment any tier compiled a Reflect wrapper: the compiled
            // wrapper calls hlp_obj_get_field directly and reads the actual
            // object, while the interpreted setField had only fed the shadow.
            // (Traced end-to-end on test_feature_typedef_anon at
            // --jit-threshold 1: [VSET] stored shadow maps 1/1, the read
            // never re-entered the interpreter, hlp_obj_get_field returned
            // the stale field.) The natives handle virtuals correctly since
            // the Phase-11/12 fixes; the crutch now only creates divergence.
            // macOS OpenGL core profile jumps from GLSL 1.20 → 1.50 (no 1.30/1.40).
            // Heaps probes #version 130, fails, and treats it as fatal.
            // Patch the source bytes in-place so the probe succeeds.
            #[cfg(target_os = "macos")]
            "gl_shader_source" if args.len() >= 2 && args[1].is_ptr() => {
                let obj_ptr = args[1].as_ptr() as *const u8;
                if !obj_ptr.is_null() && (obj_ptr as usize) > 0x10000 {
                    // HOBJ String: UTF-16 data pointer at offset 8
                    let data_ptr = unsafe { *(obj_ptr.add(8) as *const *mut u16) };
                    if !data_ptr.is_null() && (data_ptr as usize) > 0x10000 {
                        // Read first 12 UTF-16 chars: "#version 1X0"
                        let prefix = unsafe { std::slice::from_raw_parts(data_ptr, 12) };
                        let prefix_str = String::from_utf16_lossy(prefix);
                        if prefix_str.starts_with("#version 130")
                            || prefix_str.starts_with("#version 140")
                        {
                            // Patch char at index 10: '3'/'4' → '5' (UTF-16 LE)
                            unsafe {
                                *data_ptr.add(10) = b'5' as u16;
                            }
                        }
                    }
                }
                // Fall through to normal dispatch
            }
            // No thread/event/lock interceptions needed — the stdlib's
            // non-blocking lock_wait handles single-threaded mode correctly.
            _ => {}
        }

        // Resolve the native function pointer: per-native cache first, then
        // the process-global symbol table (falls back to lazy dlsym once).
        let mut func_ptr = self
            .native_fn_cache
            .get(native_idx)
            .copied()
            .unwrap_or(std::ptr::null_mut());
        if func_ptr.is_null() {
            func_ptr =
                native_resolver.resolve_function(&native.lib, &format!("hlp_{}", native.name))?;
            if let Some(slot) = self.native_fn_cache.get_mut(native_idx) {
                *slot = func_ptr;
            }
        }

        // Get the function type signature for type-aware marshaling
        let type_fun = bytecode.types[native.type_.0]
            .fun
            .as_ref()
            .ok_or_else(|| anyhow!("Native {} has no function type", func_name))?;

        // Get return type kind for wrapping the result
        let ret_kind = bytecode.types[type_fun.ret.0].kind;

        // Get argument type kinds for extraction
        let arg_kinds: Vec<hl::hl_type_kind> = type_fun
            .args
            .iter()
            .map(|a| bytecode.types[a.0].kind)
            .collect();
        if debug_dyn
            && (native.name == "obj_get_field"
                || native.name == "obj_set_field"
                || native.name == "obj_has_field"
                || native.name == "obj_delete_field"
                || native.name == "no_closure"
                || native.name == "get_closure_value"
                || native.name == "call_method")
        {
            eprintln!(
                "[NATIVE-DYN] {} arg_kinds={:?} ret_kind={}",
                func_name, arg_kinds, ret_kind
            );
        }

        // Check if any argument or return type involves floats.
        // On ARM64, floats use separate FP registers (d0-d7) vs integer registers (x0-x7),
        // so we must use typed dispatch with explicit f64 in the right positions.
        let is_float_kind =
            |k: hl::hl_type_kind| k == hl::hl_type_kind_HF32 || k == hl::hl_type_kind_HF64;
        let ret_is_float = is_float_kind(ret_kind);
        let float_mask: u32 = arg_kinds.iter().enumerate().fold(0u32, |acc, (i, &k)| {
            if is_float_kind(k) {
                acc | (1 << i)
            } else {
                acc
            }
        });

        // Set up a setjmp/longjmp trap so hlp_throw can propagate through native C ABI safely.
        // This covers BOTH float and integer dispatch paths.
        let fn_setup_trap = self.fn_setup_trap_jit;
        let fn_remove_trap = self.fn_remove_trap_jit;
        // Same frame-stack invariant as `call_compiled_function`: a native that
        // re-enters the interpreter (closure runner, dynamic dispatch) and then
        // throws longjmps straight back here, leaving the frames it pushed
        // behind.
        let stack_depth = self.stack.len();

        if ret_is_float || float_mask != 0 {
            let mut raw = None;
            let mut recovered_signal = false;
            let jumped = run_with_hl_trap(fn_setup_trap, fn_remove_trap, || {
                // Arm recovery for float-dispatch native calls too.
                let recovered = unsafe { crate::native_recovery::arm_native_recovery() };
                if recovered != 0 {
                    crate::native_recovery::disarm_native_recovery();
                    recovered_signal = true;
                    return;
                }
                raw = Some(self.dispatch_float_native(
                    func_ptr,
                    args,
                    &arg_kinds,
                    float_mask,
                    ret_is_float,
                    ret_kind == hl::hl_type_kind_HF32,
                ));
                crate::native_recovery::disarm_native_recovery();
            });
            if jumped != 0 {
                crate::native_recovery::disarm_native_recovery();
                return Err(self.longjmp_error(
                    Some(bytecode),
                    stack_depth,
                    format!("Native longjmp without exception value: {func_name}"),
                ));
            }
            if recovered_signal {
                let sig = crate::native_recovery::last_recovery_signal();
                let addr = crate::native_recovery::last_recovery_fault_addr();
                eprintln!(
                    "[ash] Recovered from signal {} (fault_addr={:#x}) in native float call: {}",
                    sig, addr, func_name
                );
                return Ok(self.wrap_native_result(0i64, ret_kind));
            }
            let raw =
                raw.ok_or_else(|| anyhow!("Native trap boundary did not run: {func_name}"))??;
            return Ok(self.wrap_native_result(raw, ret_kind));
        }

        // Type-aware argument extraction.
        // For HNULL parameters: if the value is a primitive (I32/F64/Bool),
        // box it into a vdynamic via hlp_make_dyn so the native gets a pointer.
        let extract_arg = |idx: usize| -> i64 {
            let kind = if idx < arg_kinds.len() {
                arg_kinds[idx]
            } else {
                0 // HVOID fallback
            };

            // HNULL(T) parameters expect a vdynamic* pointer, not raw values
            if env_flag!("ASH_DBG_ALLOC") && kind == hl::hl_type_kind_HNULL {
                eprintln!(
                    "[extract_arg] idx={} kind=HNULL val={:?} is_i32={} is_ptr={}",
                    idx,
                    args[idx],
                    args[idx].is_i32(),
                    args[idx].is_ptr()
                );
            }
            if kind == hl::hl_type_kind_HNULL && !self.fn_make_dyn.is_null() {
                let val = args[idx];
                if val.is_null() || val.is_void() {
                    return 0; // null pointer
                }
                if val.is_i32()
                    || val.is_f64()
                    || val.is_bool()
                    || (val.is_ptr() && val.as_ptr() < 0x10000)
                {
                    // Box the primitive into a vdynamic
                    // Determine the inner type from the type signature
                    let inner_type_idx = if idx < type_fun.args.len() {
                        let arg_type = &bytecode.types[type_fun.args[idx].0];
                        arg_type.tparam.as_ref().map(|t| t.0).unwrap_or(0)
                    } else {
                        0
                    };
                    let inner_c_type = self.c_type_factory.get(inner_type_idx) as *mut c_void;
                    let mut data: i64 = if val.is_i32() {
                        val.as_i32() as i64
                    } else if val.is_f64() {
                        val.as_f64().to_bits() as i64
                    } else {
                        val.as_bool() as i64
                    };
                    let make_dyn: unsafe extern "C" fn(*mut c_void, *mut c_void) -> *mut c_void =
                        unsafe { std::mem::transmute(self.fn_make_dyn) };
                    let boxed =
                        unsafe { make_dyn(&mut data as *mut i64 as *mut c_void, inner_c_type) };
                    return boxed as i64;
                }
            }

            self.value_to_i64(args[idx], kind)
        };

        if args.len() > 12 {
            return Err(anyhow!(
                "Native call with {} args not yet supported",
                args.len()
            ));
        }

        // Arm the native call recovery point so SIGSEGV/SIGBUS from native code
        // (e.g., macOS GL driver bugs) is caught and turned into a recoverable error.
        let mut raw_result = None;
        let mut recovered_signal = false;
        let jumped = run_with_hl_trap(fn_setup_trap, fn_remove_trap, || {
            let recovered = unsafe { crate::native_recovery::arm_native_recovery() };
            if recovered != 0 {
                crate::native_recovery::disarm_native_recovery();
                recovered_signal = true;
                return;
            }

            // Dispatch based on argument count, using type-aware extraction and wrapping.
            raw_result = Some(unsafe {
                match args.len() {
                    0 => {
                        let f: unsafe extern "C" fn() -> i64 = std::mem::transmute(func_ptr);
                        f()
                    }
                    1 => {
                        let f: unsafe extern "C" fn(i64) -> i64 = std::mem::transmute(func_ptr);
                        f(extract_arg(0))
                    }
                    2 => {
                        let f: unsafe extern "C" fn(i64, i64) -> i64 =
                            std::mem::transmute(func_ptr);
                        f(extract_arg(0), extract_arg(1))
                    }
                    3 => {
                        let f: unsafe extern "C" fn(i64, i64, i64) -> i64 =
                            std::mem::transmute(func_ptr);
                        f(extract_arg(0), extract_arg(1), extract_arg(2))
                    }
                    4 => {
                        let f: unsafe extern "C" fn(i64, i64, i64, i64) -> i64 =
                            std::mem::transmute(func_ptr);
                        f(
                            extract_arg(0),
                            extract_arg(1),
                            extract_arg(2),
                            extract_arg(3),
                        )
                    }
                    5 => {
                        let f: unsafe extern "C" fn(i64, i64, i64, i64, i64) -> i64 =
                            std::mem::transmute(func_ptr);
                        f(
                            extract_arg(0),
                            extract_arg(1),
                            extract_arg(2),
                            extract_arg(3),
                            extract_arg(4),
                        )
                    }
                    6 => {
                        let f: unsafe extern "C" fn(i64, i64, i64, i64, i64, i64) -> i64 =
                            std::mem::transmute(func_ptr);
                        f(
                            extract_arg(0),
                            extract_arg(1),
                            extract_arg(2),
                            extract_arg(3),
                            extract_arg(4),
                            extract_arg(5),
                        )
                    }
                    7 => {
                        let f: unsafe extern "C" fn(i64, i64, i64, i64, i64, i64, i64) -> i64 =
                            std::mem::transmute(func_ptr);
                        f(
                            extract_arg(0),
                            extract_arg(1),
                            extract_arg(2),
                            extract_arg(3),
                            extract_arg(4),
                            extract_arg(5),
                            extract_arg(6),
                        )
                    }
                    8 => {
                        let f: unsafe extern "C" fn(i64, i64, i64, i64, i64, i64, i64, i64) -> i64 =
                            std::mem::transmute(func_ptr);
                        f(
                            extract_arg(0),
                            extract_arg(1),
                            extract_arg(2),
                            extract_arg(3),
                            extract_arg(4),
                            extract_arg(5),
                            extract_arg(6),
                            extract_arg(7),
                        )
                    }
                    9 => {
                        let f: unsafe extern "C" fn(
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                        ) -> i64 = std::mem::transmute(func_ptr);
                        f(
                            extract_arg(0),
                            extract_arg(1),
                            extract_arg(2),
                            extract_arg(3),
                            extract_arg(4),
                            extract_arg(5),
                            extract_arg(6),
                            extract_arg(7),
                            extract_arg(8),
                        )
                    }
                    10 => {
                        let f: unsafe extern "C" fn(
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                        ) -> i64 = std::mem::transmute(func_ptr);
                        f(
                            extract_arg(0),
                            extract_arg(1),
                            extract_arg(2),
                            extract_arg(3),
                            extract_arg(4),
                            extract_arg(5),
                            extract_arg(6),
                            extract_arg(7),
                            extract_arg(8),
                            extract_arg(9),
                        )
                    }
                    11 => {
                        let f: unsafe extern "C" fn(
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                        ) -> i64 = std::mem::transmute(func_ptr);
                        f(
                            extract_arg(0),
                            extract_arg(1),
                            extract_arg(2),
                            extract_arg(3),
                            extract_arg(4),
                            extract_arg(5),
                            extract_arg(6),
                            extract_arg(7),
                            extract_arg(8),
                            extract_arg(9),
                            extract_arg(10),
                        )
                    }
                    12 => {
                        let f: unsafe extern "C" fn(
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                            i64,
                        ) -> i64 = std::mem::transmute(func_ptr);
                        f(
                            extract_arg(0),
                            extract_arg(1),
                            extract_arg(2),
                            extract_arg(3),
                            extract_arg(4),
                            extract_arg(5),
                            extract_arg(6),
                            extract_arg(7),
                            extract_arg(8),
                            extract_arg(9),
                            extract_arg(10),
                            extract_arg(11),
                        )
                    }
                    _ => 0i64, // arg count is pre-validated above
                }
            });
            crate::native_recovery::disarm_native_recovery();
        });
        if jumped != 0 {
            crate::native_recovery::disarm_native_recovery();
            return Err(self.longjmp_error(
                Some(bytecode),
                stack_depth,
                format!("Native longjmp without exception value: {func_name}"),
            ));
        }
        if recovered_signal {
            let sig = crate::native_recovery::last_recovery_signal();
            let addr = crate::native_recovery::last_recovery_fault_addr();
            let sig_name = match sig {
                11 => "SIGSEGV",
                10 => "SIGBUS",
                _ => "SIGNAL",
            };
            eprintln!(
                "[ash] Recovered from {} (fault_addr={:#x}) in native call: {}",
                sig_name, addr, func_name
            );
            return Ok(self.wrap_native_result(0i64, ret_kind));
        }
        let raw_result =
            raw_result.ok_or_else(|| anyhow!("Native trap boundary did not run: {func_name}"))?;

        // Wrap return value using the correct NanBoxedValue type
        let wrapped = self.wrap_native_result(raw_result, ret_kind);
        if debug_native
            && (native.name.contains("compare")
                || native.name.contains("eq")
                || native.name.contains("trim")
                || native.name.contains("date"))
        {
            eprintln!(
                "[NATIVE] {} -> raw={} wrapped={:?}",
                func_name, raw_result, wrapped
            );
        }
        if debug_dyn
            && (native.name == "hash"
                || native.name == "obj_get_field"
                || native.name == "obj_set_field"
                || native.name == "obj_has_field"
                || native.name == "obj_delete_field"
                || native.name == "no_closure"
                || native.name == "get_closure_value"
                || native.name == "call_method")
        {
            eprintln!(
                "[NATIVE-DYN] {} -> raw={} wrapped={:?}",
                func_name, raw_result, wrapped
            );
            if wrapped.is_ptr() && wrapped.as_ptr() != 0 {
                let d = wrapped.as_ptr() as *mut hl::vdynamic;
                unsafe {
                    if !d.is_null() && !(*d).t.is_null() {
                        eprintln!(
                            "[NATIVE-DYN] {} result_kind={} result_t={:p}",
                            func_name,
                            (*(*d).t).kind,
                            (*d).t
                        );
                    }
                }
            }
        }
        Ok(wrapped)
    }

    /// Dispatch a native call that involves float arguments or float return value.
    ///
    /// Extract (findex, optional_bound_value) from a closure NanBoxedValue.
    ///
    /// Closures can be stored as:
    /// - TAG_FUNC: just a function index (StaticClosure with no capture)
    /// - TAG_PTR: pointer to a _vclosure struct (InstanceClosure or heap-allocated)
    fn closure_findex_and_value(
        &mut self,
        val: NanBoxedValue,
    ) -> (usize, Option<NanBoxedValue>) {
        if val.is_func() {
            (val.as_func_index(), None)
        } else if val.is_ptr() {
            let cl_ptr = val.as_ptr() as *const hl::_vclosure;
            unsafe {
                // `fun` is a `findex + 1` stub only when the interpreter
                // built the closure; compiled code stores the real entry it
                // loaded from `functions_ptrs`.
                let stub = (*cl_ptr).fun as usize;
                let findex = if (stub as u64) < ash_core::llvm::stub_bridge::STUB_SENTINEL_LIMIT {
                    stub.wrapping_sub(1)
                } else {
                    self.findex_for_code_addr(stub).unwrap_or(usize::MAX)
                };
                let bound = if (*cl_ptr).hasValue != 0 && !(*cl_ptr).value.is_null() {
                    Some(NanBoxedValue::from_ptr((*cl_ptr).value as usize))
                } else {
                    None
                };
                (findex, bound)
            }
        } else {
            // Fallback: treat raw i32 payload as findex
            (val.as_ptr(), None)
        }
    }

    /// Call a closure value (FUNC-tagged or PTR-to-vclosure) with the given arguments.
    /// Prepends the bound value if the closure has one (InstanceClosure pattern).
    fn call_closure_val(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        closure_val: NanBoxedValue,
        args: Vec<NanBoxedValue>,
    ) -> Result<NanBoxedValue> {
        let (findex, bound) = self.closure_findex_and_value(closure_val);
        let mut full_args = args;
        if let Some(v) = bound {
            full_args.insert(0, v);
        }
        self.call_function(bytecode, native_resolver, findex, &full_args)
    }

    fn dynamic_to_value_for_kind(
        &self,
        d: *mut hl::vdynamic,
        dst_kind: hl::hl_type_kind,
    ) -> NanBoxedValue {
        if d.is_null() {
            return NanBoxedValue::null();
        }
        // Not every word arriving here is a box — see is_derefable_dynamic.
        if !Self::is_derefable_dynamic(d) {
            return NanBoxedValue::from_ptr(d as usize);
        }
        if dst_kind == hl::hl_type_kind_HDYN {
            return NanBoxedValue::from_ptr(d as usize);
        }
        let sk = unsafe {
            if (*d).t.is_null() {
                return NanBoxedValue::null();
            }
            (*(*d).t).kind
        };
        if Self::is_primitive_or_bytes_kind(dst_kind) {
            return unsafe { Self::unbox_dynamic_to_kind(d, dst_kind) }
                .unwrap_or(NanBoxedValue::null());
        }
        if sk == dst_kind {
            match sk {
                hl::hl_type_kind_HOBJ
                | hl::hl_type_kind_HSTRUCT
                | hl::hl_type_kind_HARRAY
                | hl::hl_type_kind_HFUN
                | hl::hl_type_kind_HVIRTUAL
                | hl::hl_type_kind_HDYNOBJ
                | hl::hl_type_kind_HENUM => {
                    return NanBoxedValue::from_ptr(d as usize);
                }
                hl::hl_type_kind_HBYTES => {
                    let p = unsafe { (*d).v.bytes } as usize;
                    return if p == 0 {
                        NanBoxedValue::null()
                    } else {
                        NanBoxedValue::from_ptr(p)
                    };
                }
                _ => {
                    let p = unsafe { (*d).v.ptr } as usize;
                    return if p == 0 {
                        NanBoxedValue::null()
                    } else {
                        NanBoxedValue::from_ptr(p)
                    };
                }
            }
        }
        if sk == hl::hl_type_kind_HBYTES {
            let p = unsafe { (*d).v.bytes } as usize;
            return if p == 0 {
                NanBoxedValue::null()
            } else {
                NanBoxedValue::from_ptr(p)
            };
        }
        let p = unsafe { (*d).v.ptr } as usize;
        if p == 0 {
            NanBoxedValue::null()
        } else {
            NanBoxedValue::from_ptr(p)
        }
    }

    /// The callee's argument TYPE INDICES (not just kinds) and return type
    /// index. HREF marshalling needs the full type — the ref's tparam decides
    /// the cell the value is coerced into, and a kind alone has lost it.
    fn closure_arg_type_idxs_and_ret(
        &self,
        bytecode: &DecodedBytecode,
        findex: usize,
    ) -> Option<(Vec<usize>, usize)> {
        let t_idx = match func_of(&self.targets, findex) {
            Some(fidx) => bytecode.functions[fidx].type_.0,
            None => bytecode.natives[native_of(&self.targets, findex)?].type_.0,
        };
        let tf = bytecode.types[t_idx].fun.as_ref()?;
        Some((tf.args.iter().map(|a| a.0).collect(), tf.ret.0))
    }

    fn try_handle_call_method_native(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        args: &[NanBoxedValue],
    ) -> Result<Option<NanBoxedValue>> {
        let dbg = env_flag!("ASH_DBG_DYN");
        if args.len() < 2
            || args[0].is_null()
            || args[0].is_void()
            || args[1].is_null()
            || args[1].is_void()
        {
            return Ok(Some(NanBoxedValue::null()));
        }

        let closure_val = args[0];
        let varray_ptr = args[1].as_ptr() as *const hl::varray;
        if varray_ptr.is_null() {
            return Ok(Some(NanBoxedValue::null()));
        }

        let (findex, bound) = self.closure_findex_and_value(closure_val);
        let (arg_type_idxs, ret_type_idx) = self
            .closure_arg_type_idxs_and_ret(bytecode, findex)
            .unwrap_or((Vec::new(), 0));
        let arg_kinds: Vec<hl::hl_type_kind> = arg_type_idxs
            .iter()
            .map(|&ti| bytecode.types[ti].kind)
            .collect();
        let arg_shift = if bound.is_some() { 1usize } else { 0usize };
        if dbg {
            eprintln!(
                "[CALL_METHOD] findex={} bound={} arg_kinds={:?} ret_type_idx={}",
                findex,
                bound.is_some(),
                arg_kinds,
                ret_type_idx
            );
        }

        let argc = unsafe { (*varray_ptr).size.max(0) as usize };
        let data_ptr = unsafe {
            (varray_ptr as *const u8).add(std::mem::size_of::<hl::varray>())
                as *const *mut hl::vdynamic
        };

        let mut call_args = Vec::with_capacity(argc);
        // Storage backing HREF arguments for the duration of the synchronous
        // call below. Box keeps each cell stable if this Vec grows.
        let mut ref_cells: Vec<Box<u64>> = Vec::new();
        for i in 0..argc {
            let dyn_arg = unsafe { *data_ptr.add(i) };
            let expected_type_idx = arg_type_idxs.get(i + arg_shift).copied();
            let expected_kind = arg_kinds
                .get(i + arg_shift)
                .copied()
                .unwrap_or(hl::hl_type_kind_HDYN);
            // A byref parameter: upstream hl_dyn_castp coerces the boxed
            // value into a fresh GC cell and passes the CELL — passing the
            // box's payload gave the callee an "address" of 0x2 for
            // Type.createInstance(ClassWithCtorDefaultValues, [2, "bar"]).
            // Null stays null: that is the callee's use-the-default signal.
            let v = if expected_kind == hl::hl_type_kind_HNULL && !dyn_arg.is_null() {
                // A provided nullable argument remains boxed. Extracting its
                // primitive payload turns `2 : Null<Int>` into pointer 0x2;
                // the callee expects the vdynamic* so its nullable prologue
                // can distinguish it from an omitted/null argument.
                NanBoxedValue::from_ptr(dyn_arg as usize)
            } else if expected_kind == hl::hl_type_kind_HREF && !dyn_arg.is_null() {
                if unsafe {
                    !(*dyn_arg).t.is_null() && (*(*dyn_arg).t).kind == hl::hl_type_kind_HREF
                } {
                    // `hlp_make_dyn` boxes HREF by preserving its cell
                    // pointer in `v.ptr`. The wrapper itself is non-null even
                    // when that pointer is null (an omitted optional
                    // argument), so testing only `dyn_arg` manufactured a
                    // non-null cell containing zero and suppressed defaults.
                    let cell = unsafe { (*dyn_arg).v.ptr } as usize;
                    if cell == 0 {
                        NanBoxedValue::null()
                    } else {
                        NanBoxedValue::from_ptr(cell)
                    }
                } else {
                    let href_type = &bytecode.types[arg_type_idxs[i + arg_shift]];
                    let inner_kind = href_type
                        .tparam
                        .as_ref()
                        .and_then(|t| bytecode.types.get(t.0))
                        .map(|t| t.kind)
                        .unwrap_or(hl::hl_type_kind_HDYN);
                    let inner = self.dynamic_to_value_for_kind(dyn_arg, inner_kind);
                    let mut cell = Box::new(0u64);
                    Self::write_value_to_ptr(
                        (&mut *cell as *mut u64).cast::<u8>(),
                        inner,
                        inner_kind,
                    );
                    let cell_ptr = (&mut *cell as *mut u64) as usize;
                    ref_cells.push(cell);
                    NanBoxedValue::from_ptr(cell_ptr)
                }
            } else if expected_kind == hl::hl_type_kind_HOBJ && !self.fn_dyn_castp.is_null() {
                // A dynamic HOBJ still needs an exact-type cast. Kind-only
                // conversion passes ArrayDyn to a method expecting
                // ArrayBytes<Int>, while HashLink's hl_dyn_call routes that
                // through hl_dyn_castp so ArrayDyn.__cast can materialize the
                // specialized representation.
                if let Some(expected_type_idx) = expected_type_idx {
                    let target_type = self.c_type_factory.get(expected_type_idx);
                    let source_type = unsafe { (*dyn_arg).t };
                    if source_type == target_type || target_type.is_null() || source_type.is_null()
                    {
                        NanBoxedValue::from_ptr(dyn_arg as usize)
                    } else if unsafe { (*source_type).kind } != hl::hl_type_kind_HOBJ {
                        // Default-argument method shims can present their
                        // receiver through HREF. Preserve the established
                        // wrapper unboxing for those non-object sources.
                        self.dynamic_to_value_for_kind(dyn_arg, expected_kind)
                    } else {
                        type FnCastp = unsafe extern "C" fn(
                            *mut c_void,
                            *mut c_void,
                            *mut c_void,
                        ) -> *mut c_void;
                        let castp: FnCastp = unsafe { std::mem::transmute(self.fn_dyn_castp) };
                        let mut data = dyn_arg as *mut c_void;
                        let casted = unsafe {
                            castp(
                                &mut data as *mut _ as *mut c_void,
                                source_type.cast(),
                                target_type.cast(),
                            )
                        };
                        if casted.is_null() {
                            NanBoxedValue::null()
                        } else {
                            NanBoxedValue::from_ptr(casted as usize)
                        }
                    }
                } else {
                    self.dynamic_to_value_for_kind(dyn_arg, expected_kind)
                }
            } else {
                self.dynamic_to_value_for_kind(dyn_arg, expected_kind)
            };
            if dbg {
                let sk = unsafe {
                    if dyn_arg.is_null() || (*dyn_arg).t.is_null() {
                        0
                    } else {
                        (*(*dyn_arg).t).kind
                    }
                };
                eprintln!(
                    "[CALL_METHOD] arg{} dyn={:p} sk={} expect={} -> {:?}",
                    i, dyn_arg, sk, expected_kind, v
                );
            }
            call_args.push(v);
        }

        // Reflect.callMethod sizes its NativeArray to the arguments it needs
        // to materialize; trailing optional parameters are omitted.  The
        // interpreter register file starts as Void, whereas HashLink presents
        // an omitted optional (HREF) parameter as null so the callee's default
        // prologue runs.  Pad to the declared signature before dispatch.
        let explicit_params = arg_kinds.len().saturating_sub(arg_shift);
        call_args.resize(explicit_params, NanBoxedValue::null());

        let ret = self.call_closure_val(bytecode, native_resolver, closure_val, call_args)?;
        if dbg {
            eprintln!("[CALL_METHOD] raw_ret={:?}", ret);
        }
        let out = if ret.is_void() {
            NanBoxedValue::null()
        } else {
            let ret_t = self.c_type_factory.get(ret_type_idx);
            self.box_value_as_dynamic_with_type(ret, ret_t)
        };
        if dbg {
            eprintln!("[CALL_METHOD] out={:?}", out);
        }
        Ok(Some(out))
    }

    /// Interpreter-side implementation of bsort_i32 that uses the interpreter's
    /// call mechanism for the comparator closure (bytecode closures can't be called
    /// as raw C functions in interpreter mode).
    fn sort_bytes_i32(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        args: &[NanBoxedValue],
    ) -> Result<NanBoxedValue> {
        if args.len() < 4 {
            return Ok(NanBoxedValue::void());
        }
        let bytes_ptr = args[0].as_ptr() as *mut i32;
        let pos = args[1].as_i32() as isize;
        let len = args[2].as_i32() as usize;
        let cmp_val = args[3];

        if len == 0 || bytes_ptr as usize == 0 {
            return Ok(NanBoxedValue::void());
        }

        let mut data: Vec<i32> =
            unsafe { std::slice::from_raw_parts(bytes_ptr.offset(pos), len) }.to_vec();

        // Use raw pointer to avoid borrow conflict inside sort_by closure
        let self_raw = self as *mut Self;
        let bytecode_raw = bytecode as *const DecodedBytecode;
        let resolver_raw = native_resolver as *const ash_core::native_lib::NativeFunctionResolver;
        let mut sort_err: Option<anyhow::Error> = None;

        data.sort_by(|&a, &b| {
            if sort_err.is_some() {
                return std::cmp::Ordering::Equal;
            }
            let interp = unsafe { &mut *self_raw };
            let bc = unsafe { &*bytecode_raw };
            let nr = unsafe { &*resolver_raw };
            let call_args = vec![NanBoxedValue::from_i32(a), NanBoxedValue::from_i32(b)];
            match interp.call_closure_val(bc, nr, cmp_val, call_args) {
                Ok(r) => r.as_i32().cmp(&0),
                Err(e) => {
                    sort_err = Some(e);
                    std::cmp::Ordering::Equal
                }
            }
        });

        if let Some(e) = sort_err {
            return Err(e);
        }

        unsafe {
            let slice = std::slice::from_raw_parts_mut(bytes_ptr.offset(pos), len);
            slice.copy_from_slice(&data);
        }
        Ok(NanBoxedValue::void())
    }

    /// Interpreter-side bsort_i64.
    fn sort_bytes_i64(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        args: &[NanBoxedValue],
    ) -> Result<NanBoxedValue> {
        if args.len() < 4 {
            return Ok(NanBoxedValue::void());
        }
        let bytes_ptr = args[0].as_ptr() as *mut i64;
        let pos = args[1].as_i32() as isize;
        let len = args[2].as_i32() as usize;
        let cmp_val = args[3];

        if len == 0 || bytes_ptr as usize == 0 {
            return Ok(NanBoxedValue::void());
        }

        let mut data: Vec<i64> =
            unsafe { std::slice::from_raw_parts(bytes_ptr.offset(pos), len) }.to_vec();

        let self_raw = self as *mut Self;
        let bytecode_raw = bytecode as *const DecodedBytecode;
        let resolver_raw = native_resolver as *const ash_core::native_lib::NativeFunctionResolver;
        let mut sort_err: Option<anyhow::Error> = None;

        data.sort_by(|&a, &b| {
            if sort_err.is_some() {
                return std::cmp::Ordering::Equal;
            }
            let interp = unsafe { &mut *self_raw };
            let bc = unsafe { &*bytecode_raw };
            let nr = unsafe { &*resolver_raw };
            let call_args = vec![NanBoxedValue::from_i64(a), NanBoxedValue::from_i64(b)];
            match interp.call_closure_val(bc, nr, cmp_val, call_args) {
                Ok(r) => r.as_i32().cmp(&0),
                Err(e) => {
                    sort_err = Some(e);
                    std::cmp::Ordering::Equal
                }
            }
        });

        if let Some(e) = sort_err {
            return Err(e);
        }

        unsafe {
            let slice = std::slice::from_raw_parts_mut(bytes_ptr.offset(pos), len);
            slice.copy_from_slice(&data);
        }
        Ok(NanBoxedValue::void())
    }

    /// Interpreter-side bsort_f64.
    fn sort_bytes_f64(
        &mut self,
        bytecode: &DecodedBytecode,
        native_resolver: &NativeFunctionResolver,
        args: &[NanBoxedValue],
    ) -> Result<NanBoxedValue> {
        if args.len() < 4 {
            return Ok(NanBoxedValue::void());
        }
        let bytes_ptr = args[0].as_ptr() as *mut f64;
        let pos = args[1].as_i32() as isize;
        let len = args[2].as_i32() as usize;
        let cmp_val = args[3];

        if len == 0 || bytes_ptr as usize == 0 {
            return Ok(NanBoxedValue::void());
        }

        let mut data: Vec<f64> =
            unsafe { std::slice::from_raw_parts(bytes_ptr.offset(pos), len) }.to_vec();

        let self_raw = self as *mut Self;
        let bytecode_raw = bytecode as *const DecodedBytecode;
        let resolver_raw = native_resolver as *const ash_core::native_lib::NativeFunctionResolver;
        let mut sort_err: Option<anyhow::Error> = None;

        data.sort_by(|&a, &b| {
            if sort_err.is_some() {
                return std::cmp::Ordering::Equal;
            }
            let interp = unsafe { &mut *self_raw };
            let bc = unsafe { &*bytecode_raw };
            let nr = unsafe { &*resolver_raw };
            let call_args = vec![NanBoxedValue::from_f64(a), NanBoxedValue::from_f64(b)];
            match interp.call_closure_val(bc, nr, cmp_val, call_args) {
                Ok(r) => r.as_i32().cmp(&0),
                Err(e) => {
                    sort_err = Some(e);
                    std::cmp::Ordering::Equal
                }
            }
        });

        if let Some(e) = sort_err {
            return Err(e);
        }

        unsafe {
            let slice = std::slice::from_raw_parts_mut(bytes_ptr.offset(pos), len);
            slice.copy_from_slice(&data);
        }
        Ok(NanBoxedValue::void())
    }

    /// On ARM64 (and x86-64), floating-point arguments go into FP registers (d0-d7 / xmm0-xmm7),
    /// separate from integer/pointer registers (x0-x7 / rdi-rdi). Using a generic
    /// `fn(i64,...)->i64` transmute would put float bits into the wrong registers.
    ///
    /// This function uses typed Rust fn signatures to ensure the compiler emits
    /// correct calling-convention instructions for each pattern.
    ///
    /// Returns the raw i64 result (float results are returned as their bit representation).
    fn dispatch_float_native(
        &self,
        func_ptr: *mut std::ffi::c_void,
        args: &[NanBoxedValue],
        arg_kinds: &[hl::hl_type_kind],
        float_mask: u32,
        ret_is_float: bool,
        ret_is_f32: bool,
    ) -> Result<i64> {
        let gf = |i: usize| -> f64 { args[i].as_f64() };
        let gf32 = |i: usize| -> f32 { args[i].as_f64() as f32 };
        let gi = |i: usize| -> i64 { self.value_to_i64(args[i], arg_kinds[i]) };

        let raw: i64 = unsafe {
            match (args.len(), ret_is_float, float_mask) {
                // --- 0 args ---
                (0, true, 0b0) if ret_is_f32 => {
                    let f: unsafe extern "C" fn() -> f32 = std::mem::transmute(func_ptr);
                    (f() as f64).to_bits() as i64
                }
                (0, true, 0b0) => {
                    // () -> f64
                    let f: unsafe extern "C" fn() -> f64 = std::mem::transmute(func_ptr);
                    f().to_bits() as i64
                }
                // --- 1 arg ---
                (1, true, 0b0) if ret_is_f32 => {
                    let f: unsafe extern "C" fn(i64) -> f32 = std::mem::transmute(func_ptr);
                    (f(gi(0)) as f64).to_bits() as i64
                }
                (1, true, 0b0) => {
                    // (i64) -> f64  e.g. date_get_time(t:Int)
                    let f: unsafe extern "C" fn(i64) -> f64 = std::mem::transmute(func_ptr);
                    f(gi(0)).to_bits() as i64
                }
                (1, true, 0b1) if ret_is_f32 && arg_kinds[0] == hl::hl_type_kind_HF32 => {
                    let f: unsafe extern "C" fn(f32) -> f32 = std::mem::transmute(func_ptr);
                    (f(gf32(0)) as f64).to_bits() as i64
                }
                (1, true, 0b1) => {
                    // (f64) -> f64  e.g. math_sqrt, math_abs, math_floor, ...
                    let f: unsafe extern "C" fn(f64) -> f64 = std::mem::transmute(func_ptr);
                    f(gf(0)).to_bits() as i64
                }
                (1, false, 0b1) if arg_kinds[0] == hl::hl_type_kind_HF32 => {
                    let f: unsafe extern "C" fn(f32) = std::mem::transmute(func_ptr);
                    f(gf32(0));
                    0
                }
                (1, false, 0b1) => {
                    // (f64) -> i64  e.g. math_ffloor, math_isnan, math_isfinite
                    let f: unsafe extern "C" fn(f64) -> i64 = std::mem::transmute(func_ptr);
                    f(gf(0))
                }
                // --- 2 args ---
                (2, false, 0b01) => {
                    // (f64, i64) -> i64  e.g. hlp_ftos(d, len)
                    let f: unsafe extern "C" fn(f64, i64) -> i64 = std::mem::transmute(func_ptr);
                    f(gf(0), gi(1))
                }
                (2, true, 0b01) => {
                    // (f64, i64) -> f64
                    let f: unsafe extern "C" fn(f64, i64) -> f64 = std::mem::transmute(func_ptr);
                    f(gf(0), gi(1)).to_bits() as i64
                }
                (2, false, 0b10) if arg_kinds[1] == hl::hl_type_kind_HF32 => {
                    let f: unsafe extern "C" fn(i64, f32) = std::mem::transmute(func_ptr);
                    f(gi(0), gf32(1));
                    0
                }
                (2, false, 0b10) => {
                    // (i64, f64) -> i64
                    let f: unsafe extern "C" fn(i64, f64) -> i64 = std::mem::transmute(func_ptr);
                    f(gi(0), gf(1))
                }
                (2, true, 0b10) if ret_is_f32 && arg_kinds[1] == hl::hl_type_kind_HF32 => {
                    let f: unsafe extern "C" fn(i64, f32) -> f32 = std::mem::transmute(func_ptr);
                    (f(gi(0), gf32(1)) as f64).to_bits() as i64
                }
                (2, true, 0b10) => {
                    // (i64, f64) -> f64
                    let f: unsafe extern "C" fn(i64, f64) -> f64 = std::mem::transmute(func_ptr);
                    f(gi(0), gf(1)).to_bits() as i64
                }
                (2, true, 0b11) => {
                    // (f64, f64) -> f64  e.g. math_pow, math_atan2
                    let f: unsafe extern "C" fn(f64, f64) -> f64 = std::mem::transmute(func_ptr);
                    f(gf(0), gf(1)).to_bits() as i64
                }
                (2, true, 0b00) if ret_is_f32 => {
                    let f: unsafe extern "C" fn(i64, i64) -> f32 = std::mem::transmute(func_ptr);
                    (f(gi(0), gi(1)) as f64).to_bits() as i64
                }
                (2, true, 0b00) => {
                    // (i64, i64) -> f64
                    let f: unsafe extern "C" fn(i64, i64) -> f64 = std::mem::transmute(func_ptr);
                    f(gi(0), gi(1)).to_bits() as i64
                }
                (2, false, 0b11) => {
                    // (f64, f64) -> i64
                    let f: unsafe extern "C" fn(f64, f64) -> i64 = std::mem::transmute(func_ptr);
                    f(gf(0), gf(1))
                }
                // --- 3 args ---
                (3, true, 0b000) => {
                    // (i64, i64, i64) -> f64  e.g. hlp_parse_float(bytes, pos, len)
                    let f: unsafe extern "C" fn(i64, i64, i64) -> f64 =
                        std::mem::transmute(func_ptr);
                    f(gi(0), gi(1), gi(2)).to_bits() as i64
                }
                (3, false, 0b001) => {
                    // (f64, i64, i64) -> i64
                    let f: unsafe extern "C" fn(f64, i64, i64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(gf(0), gi(1), gi(2))
                }
                (3, true, 0b001) => {
                    // (f64, i64, i64) -> f64
                    let f: unsafe extern "C" fn(f64, i64, i64) -> f64 =
                        std::mem::transmute(func_ptr);
                    f(gf(0), gi(1), gi(2)).to_bits() as i64
                }
                (3, false, 0b011) => {
                    // Two scalar values followed by comparison context.
                    let f: unsafe extern "C" fn(f64, f64, i64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(gf(0), gf(1), gi(2))
                }
                (3, false, 0b100) if arg_kinds[2] == hl::hl_type_kind_HF32 => {
                    // (i64, i64, f32) -> void, used by hlsdl's
                    // gl_tex_parameterf(target, parameter, value).
                    let f: unsafe extern "C" fn(i64, i64, f32) = std::mem::transmute(func_ptr);
                    f(gi(0), gi(1), gf32(2));
                    0
                }
                (3, false, 0b100) => {
                    // (i64, i64, f64) -> i64
                    let f: unsafe extern "C" fn(i64, i64, f64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(gi(0), gi(1), gf(2))
                }
                (3, false, 0b111) => {
                    // (f64, f64, f64) -> i64
                    let f: unsafe extern "C" fn(f64, f64, f64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(gf(0), gf(1), gf(2))
                }
                (3, true, 0b111) => {
                    // (f64, f64, f64) -> f64
                    let f: unsafe extern "C" fn(f64, f64, f64) -> f64 =
                        std::mem::transmute(func_ptr);
                    f(gf(0), gf(1), gf(2)).to_bits() as i64
                }
                // --- 4 args ---
                (4, false, 0b1110)
                    if arg_kinds[1..].iter().all(|&k| k == hl::hl_type_kind_HF32) =>
                {
                    // OpenAL listener3f(parameter, x, y, z).
                    let f: unsafe extern "C" fn(i64, f32, f32, f32) = std::mem::transmute(func_ptr);
                    f(gi(0), gf32(1), gf32(2), gf32(3));
                    0
                }
                (4, false, 0b1110) => {
                    // Compiled AIR functions and native vector helpers with
                    // a receiver followed by three doubles.
                    let f: unsafe extern "C" fn(i64, f64, f64, f64) = std::mem::transmute(func_ptr);
                    f(gi(0), gf(1), gf(2), gf(3));
                    0
                }
                (4, false, 0b1000) if arg_kinds[3] == hl::hl_type_kind_HF32 => {
                    let f: unsafe extern "C" fn(i64, i64, i64, f32) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(gi(0), gi(1), gi(2), gf32(3))
                }
                (4, false, 0b1000) => {
                    // AIR functions such as structural equality carry the
                    // comparison epsilon after three pointer-like operands.
                    let f: unsafe extern "C" fn(i64, i64, i64, f64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(gi(0), gi(1), gi(2), gf(3))
                }
                (4, false, 0b0110) => {
                    let f: unsafe extern "C" fn(i64, f64, f64, i64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(gi(0), gf(1), gf(2), gi(3))
                }
                (4, true, 0b0000) if ret_is_f32 => {
                    let f: unsafe extern "C" fn(i64, i64, i64, i64) -> f32 =
                        std::mem::transmute(func_ptr);
                    (f(gi(0), gi(1), gi(2), gi(3)) as f64).to_bits() as i64
                }
                (4, true, 0b0000) => {
                    let f: unsafe extern "C" fn(i64, i64, i64, i64) -> f64 =
                        std::mem::transmute(func_ptr);
                    f(gi(0), gi(1), gi(2), gi(3)).to_bits() as i64
                }
                (4, false, 0b1111) => {
                    // (f64, f64, f64, f64) -> i64  e.g. gl_clear_color(r, g, b, a)
                    let f: unsafe extern "C" fn(f64, f64, f64, f64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(gf(0), gf(1), gf(2), gf(3))
                }
                // --- 5 args ---
                (5, false, 0b11100)
                    if arg_kinds[2..].iter().all(|&k| k == hl::hl_type_kind_HF32) =>
                {
                    // OpenAL source3f/buffer3f(object, parameter, x, y, z).
                    let f: unsafe extern "C" fn(i64, i64, f32, f32, f32) =
                        std::mem::transmute(func_ptr);
                    f(gi(0), gi(1), gf32(2), gf32(3), gf32(4));
                    0
                }
                (5, false, 0b11100) => {
                    let f: unsafe extern "C" fn(i64, i64, f64, f64, f64) =
                        std::mem::transmute(func_ptr);
                    f(gi(0), gi(1), gf(2), gf(3), gf(4));
                    0
                }
                (5, false, 0b00011) => {
                    let f: unsafe extern "C" fn(f64, f64, i64, i64, i64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(gf(0), gf(1), gi(2), gi(3), gi(4))
                }
                (5, false, 0b11110) => {
                    let f: unsafe extern "C" fn(i64, f64, f64, f64, f64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(gi(0), gf(1), gf(2), gf(3), gf(4))
                }
                // --- 6 args ---
                (6, false, 0b100000) => {
                    // (i64, i64, i64, i64, i64, f64) -> i64
                    // e.g. socket_select(read, write, other, tmp, size, timeout)
                    let f: unsafe extern "C" fn(i64, i64, i64, i64, i64, f64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(gi(0), gi(1), gi(2), gi(3), gi(4), gf(5))
                }
                // --- 8 args ---
                (8, false, 0b0011_1100) => {
                    // Haxe graphics helpers commonly carry an object and
                    // flags around four scalar coordinates:
                    // (i64, i64, f64, f64, f64, f64, i64, i64) -> word.
                    let f: unsafe extern "C" fn(i64, i64, f64, f64, f64, f64, i64, i64) -> i64 =
                        std::mem::transmute(func_ptr);
                    f(gi(0), gi(1), gf(2), gf(3), gf(4), gf(5), gi(6), gi(7))
                }
                _ => {
                    return Err(anyhow!(
                        "Float native dispatch: {} args, float_mask={:#b}, ret_float={} not yet supported",
                        args.len(),
                        float_mask,
                        ret_is_float
                    ));
                }
            }
        };
        Ok(raw)
    }

    /// Convert a NanBoxedValue to an i64 for FFI passing.
    /// Uses the HL type kind to correctly interpret the value.
    fn value_to_i64(&self, val: NanBoxedValue, type_kind: hl::hl_type_kind) -> i64 {
        use ValueTypeKind::*;
        match ValueTypeKind::try_from(kind_u32(type_kind)).unwrap_or(HNULL) {
            HVOID => 0,
            HI32 | HUI8 | HUI16 => val.as_i32() as i64,
            HI64 => val.as_i64_lossy(),
            HF32 | HF64 => {
                // Floats passed through integer registers via transmute
                val.as_f64().to_bits() as i64
            }
            HBOOL => val.as_bool() as i64,
            _ => {
                // All other types are pointer-like (HOBJ, HDYN, HBYTES, HFUN, etc.)
                if val.is_null() || val.is_void() {
                    0
                } else if val.is_ptr() {
                    val.as_ptr() as i64
                } else if val.is_i32() {
                    // Sometimes an i32 is used where a pointer is expected (e.g., 0 for null)
                    val.as_i32() as i64
                } else {
                    // TAG_I64, TAG_BYTES, TAG_FUNC, or unknown - extract raw payload
                    val.as_ptr() as i64
                }
            }
        }
    }

    /// Wrap a raw i64 return value from a native function based on the HL return type.
    fn wrap_native_result(&self, raw: i64, ret_kind: hl::hl_type_kind) -> NanBoxedValue {
        use ValueTypeKind::*;
        match ValueTypeKind::try_from(kind_u32(ret_kind)).unwrap_or(HNULL) {
            HVOID => NanBoxedValue::void(),
            HI32 => NanBoxedValue::from_i32(raw as i32),
            // A callee returning bool/u8/u16 only defines the low bits of the
            // return register — SysV x86-64 leaves the rest undefined, and the
            // dispatch reads the full register. Truncate to the ABI width or
            // Linux garbage bits turn a returned `false` into `true`.
            HUI8 => NanBoxedValue::from_i32(raw as u8 as i32),
            HUI16 => NanBoxedValue::from_i32(raw as u16 as i32),
            HI64 => NanBoxedValue::from_i64(raw),
            HF32 | HF64 => NanBoxedValue::from_f64(f64::from_bits(raw as u64)),
            HBOOL => NanBoxedValue::from_bool((raw as u8) != 0),
            HBYTES => {
                if raw == 0 {
                    NanBoxedValue::null()
                } else {
                    NanBoxedValue::from_bytes_ptr(raw as usize)
                }
            }
            _ => {
                // All other types are pointer-like (HOBJ, HDYN, HFUN, HARRAY, etc.)
                if raw == 0 {
                    NanBoxedValue::null()
                } else {
                    NanBoxedValue::from_ptr(raw as usize)
                }
            }
        }
    }

    /// Read a field from an object at the given field index.
    /// Uses the runtime object's fields_indexes to compute the byte offset.
    unsafe fn read_obj_field(
        obj_ptr: *mut u8,
        field_idx: usize,
        dst_kind: hl::hl_type_kind,
        obj_c_type: *mut c_void,
        obj_kind: hl::hl_type_kind,
        fn_get_obj_rt: *mut c_void,
    ) -> NanBoxedValue {
        if fn_get_obj_rt.is_null() {
            return NanBoxedValue::null();
        }

        // For HOBJ, prefer the object's own header type (supports polymorphism).
        // For HSTRUCT, use the register's declared type (structs have no header).
        let type_ptr = if obj_kind != hl_type_kind_HSTRUCT {
            let header = *(obj_ptr as *const *mut c_void);
            if !header.is_null() {
                header
            } else {
                obj_c_type
            }
        } else {
            obj_c_type
        };

        if type_ptr.is_null() {
            return NanBoxedValue::null();
        }

        // Corruption tripwire: a type pointer must be 8-aligned; a NaN-boxed
        // double here means the object's memory was reclaimed and reused.
        // Print the evidence (cross-reference with ASH_GC_TRACE_FREED) before
        // the misaligned deref aborts without it.
        {
            let bad_align = (type_ptr as usize) & 7 != 0;
            // An aligned-but-garbage header (a reused line of doubles) passes
            // the alignment check; the type's kind field gives it away.
            let bad_kind = !bad_align && {
                let k = *(type_ptr as *const i32);
                !(0..=22).contains(&k)
            };
            if bad_align || bad_kind {
                eprintln!(
                    "[gc-corrupt] FieldGet obj={:#x} header={:#x} field={field_idx}",
                    obj_ptr as usize, type_ptr as usize
                );
            }
        }
        let get_rt: FnGetObjRt = std::mem::transmute(fn_get_obj_rt);
        let rt = get_rt(type_ptr) as *const hl_runtime_obj;
        if rt.is_null() || (*rt).fields_indexes.is_null() {
            return NanBoxedValue::null();
        }

        if field_idx >= (*rt).nfields as usize {
            return NanBoxedValue::null();
        }

        let offset = *(*rt).fields_indexes.add(field_idx);
        let field_addr = obj_ptr.add(offset as usize);

        // Use dst_kind (register type) for reading — the compiler knows the correct
        // read width. The field's declared type is only used for WRITING to prevent
        // 8-byte NanBox spill into adjacent fields.
        Self::read_value_at(field_addr, dst_kind)
    }

    /// Write a value to an object field at the given field index.
    unsafe fn write_obj_field(
        obj_ptr: *mut u8,
        field_idx: usize,
        src_kind: hl::hl_type_kind,
        val: NanBoxedValue,
        obj_c_type: *mut c_void,
        obj_kind: hl::hl_type_kind,
        fn_get_obj_rt: *mut c_void,
    ) {
        if fn_get_obj_rt.is_null() {
            return;
        }

        let type_ptr = if obj_kind != hl_type_kind_HSTRUCT {
            let header = *(obj_ptr as *const *mut c_void);
            if !header.is_null() {
                header
            } else {
                obj_c_type
            }
        } else {
            obj_c_type
        };

        if type_ptr.is_null() {
            return;
        }

        let get_rt: FnGetObjRt = std::mem::transmute(fn_get_obj_rt);
        let rt = get_rt(type_ptr) as *const hl_runtime_obj;
        if rt.is_null() || (*rt).fields_indexes.is_null() {
            return;
        }

        if field_idx >= (*rt).nfields as usize {
            return;
        }

        let offset = *(*rt).fields_indexes.add(field_idx);
        let field_addr = obj_ptr.add(offset as usize);

        Self::write_value_at(field_addr, src_kind, val);
    }

    /// Read a value from a raw memory address based on the HL type kind.
    unsafe fn read_value_at(addr: *const u8, kind: hl::hl_type_kind) -> NanBoxedValue {
        use ValueTypeKind::*;
        match ValueTypeKind::try_from(kind_u32(kind)).unwrap_or(HDYN) {
            HVOID => NanBoxedValue::void(),
            HUI8 => NanBoxedValue::from_i32(*addr as i32),
            HUI16 => NanBoxedValue::from_i32(*(addr as *const u16) as i32),
            HI32 => NanBoxedValue::from_i32(*(addr as *const i32)),
            HI64 => NanBoxedValue::from_i64(*(addr as *const i64)),
            HF32 => NanBoxedValue::from_f64(*(addr as *const f32) as f64),
            HF64 => NanBoxedValue::from_f64(*(addr as *const f64)),
            HBOOL => NanBoxedValue::from_bool(*addr != 0),
            _ => {
                // Pointer types (OBJ, DYN, FUN, ARRAY, BYTES, ENUM, etc.)
                let ptr = *(addr as *const usize);
                if ptr == 0 {
                    NanBoxedValue::null()
                } else {
                    NanBoxedValue::from_ptr(ptr)
                }
            }
        }
    }

    /// Upstream's `hl_is_ptr`: kinds at or above HBYTES live in a machine
    /// word that holds a pointer; kinds below it are value scalars.
    #[inline(always)]
    fn is_ptr_kind(kind: hl::hl_type_kind) -> bool {
        kind >= hl::hl_type_kind_HBYTES
    }

    /// Resolve a real compiled entry address back to the findex it belongs to.
    ///
    /// Compiled code allocates closures from `functions_ptrs[findex]`, so the
    /// `fun` field of a closure that crossed the compiled→interpreter boundary
    /// may hold an entry address where the interpreter expects a `findex + 1`
    /// stub sentinel. `functions_ptrs` is the table that address came from, so
    /// it is also the map back.
    ///
    /// The scan is amortised: a miss indexes the whole table at once, and
    /// promotion only ever adds addresses, so a cached entry stays true.
    fn findex_for_code_addr(&mut self, addr: usize) -> Option<usize> {
        if let Some(&fi) = self.code_addr_findex.get(&addr) {
            return Some(fi);
        }
        // Every install registers its entry, and that registry accumulates
        // rather than overwriting, so it answers for superseded tiers too.
        if let Some(fi) = ash_core::profile::findex_at_entry(addr) {
            let fi = fi as usize;
            self.code_addr_findex.insert(addr, fi);
            return Some(fi);
        }
        let module_ctx = self.c_type_factory.module_ctx();
        if module_ctx.is_null() {
            return None;
        }
        // SAFETY: `module_ctx` is the process-lifetime context the type
        // factory owns; `functions_ptrs` is its findex-indexed slot table,
        // sized to hold every findex in `targets`.
        let ptrs = unsafe { (*module_ctx).functions_ptrs };
        if ptrs.is_null() {
            return None;
        }
        for findex in 0..self.targets.len() {
            let slot = unsafe { *ptrs.add(findex) } as usize;
            if slot as u64 >= ash_core::llvm::stub_bridge::STUB_SENTINEL_LIMIT {
                self.code_addr_findex.entry(slot).or_insert(findex);
            }
        }
        self.code_addr_findex.get(&addr).copied()
    }

    /// Resolve a vtable slot to its findex from the object's RUNTIME type:
    /// walk the C proto chain child-first for the entry with this absolute
    /// `pindex`, so an override shadows its ancestor. This is the same truth
    /// `vobj_proto` itself is built from.
    ///
    /// # Safety contract
    /// `type_ptr` must be a live `hl_type` (it came from an object header).
    unsafe fn find_runtime_proto_findex(type_ptr: *mut hl_type, pindex: usize) -> Option<usize> {
        let mut t = type_ptr;
        while !t.is_null()
            && ((*t).kind == hl::hl_type_kind_HOBJ || (*t).kind == hl::hl_type_kind_HSTRUCT)
        {
            let obj = (*t).__bindgen_anon_1.obj;
            if obj.is_null() {
                break;
            }
            for i in 0..(*obj).nproto as usize {
                let pr = &*(*obj).proto.add(i);
                if pr.pindex >= 0 && pr.pindex as usize == pindex {
                    return Some(pr.findex as usize);
                }
            }
            t = (*obj).super_;
        }
        None
    }

    /// Resolve a method findex from bytecode type proto (fallback when vobj_proto unavailable).
    fn resolve_method_findex_from_bytecode(
        &self,
        bytecode: &DecodedBytecode,
        func: &HLFunction,
        this_reg: &Reg,
        proto_index: usize,
    ) -> Option<usize> {
        let type_idx = func.regs[this_reg.0 as usize].0;
        self.find_proto_findex(bytecode, type_idx, proto_index)
    }

    /// Recursively search type and its supers for a proto with matching pindex.
    fn find_proto_findex(
        &self,
        bytecode: &DecodedBytecode,
        type_idx: usize,
        proto_index: usize,
    ) -> Option<usize> {
        let hl_type_rust = &bytecode.types[type_idx];
        if let Some(ref obj) = hl_type_rust.obj {
            for proto in &obj.proto {
                if proto.pindex as usize == proto_index {
                    return Some(proto.findex as usize);
                }
            }
            // Check super type
            if let Some(ref super_) = obj.super_ {
                return self.find_proto_findex(bytecode, super_.0, proto_index);
            }
        }
        None
    }

    /// Write a value to a raw memory address based on the HL type kind.
    unsafe fn write_value_at(addr: *mut u8, kind: hl::hl_type_kind, val: NanBoxedValue) {
        use ValueTypeKind::*;
        match ValueTypeKind::try_from(kind_u32(kind)).unwrap_or(HDYN) {
            HVOID => {}
            HUI8 => *addr = val.as_i32() as u8,
            HUI16 => *(addr as *mut u16) = val.as_i32() as u16,
            HI32 => *(addr as *mut i32) = val.as_i32(),
            HI64 => *(addr as *mut i64) = val.as_i64_lossy(),
            HF32 => *(addr as *mut f32) = val.as_f64() as f32,
            HF64 => *(addr as *mut f64) = val.as_f64(),
            HBOOL => *addr = val.as_bool() as u8,
            _ => {
                // Pointer types — but the NanBoxed value might actually
                // be a primitive (e.g., HDYN register holding an I32).
                if val.is_null() || val.is_void() {
                    *(addr as *mut usize) = 0;
                } else if val.is_i32() {
                    *(addr as *mut i32) = val.as_i32();
                } else if val.is_f64() {
                    *(addr as *mut f64) = val.as_f64();
                } else {
                    *(addr as *mut usize) = val.as_ptr();
                }
            }
        }
    }

    /// Allocate a venum value for the given type and construct index using the GC allocator.
    /// Takes fn_alloc_enum as a parameter to avoid conflicting with the frame mutable borrow.
    fn alloc_enum_value(
        fn_alloc_enum: *mut c_void,
        c_type_ptr: *mut hl_type,
        construct_idx: i32,
    ) -> *mut u8 {
        if fn_alloc_enum.is_null() || c_type_ptr.is_null() {
            return std::ptr::null_mut();
        }
        unsafe {
            let f: unsafe extern "C" fn(*mut hl_type, i32) -> *mut u8 =
                std::mem::transmute(fn_alloc_enum);
            f(c_type_ptr, construct_idx)
        }
    }

    /// Read a NanBoxedValue from a raw memory pointer using the given type kind.
    fn read_value_from_ptr(ptr: *const u8, kind: hl::hl_type_kind) -> NanBoxedValue {
        unsafe { Self::read_value_at(ptr, kind) }
    }

    /// Write a NanBoxedValue to a raw memory pointer using the given type kind.
    fn write_value_to_ptr(ptr: *mut u8, val: NanBoxedValue, kind: hl::hl_type_kind) {
        unsafe { Self::write_value_at(ptr, kind, val) }
    }
}

/// Set once the program's entrypoint has returned, so the broker stops
/// starting promotions whose result nothing can call any more.
static RETIER_ABANDON: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

/// Whether speculative re-tier work should give up now.
pub(crate) fn retier_abandoned() -> bool {
    RETIER_ABANDON.load(std::sync::atomic::Ordering::Relaxed)
}

/// Stop the broker taking on new promotions. Called once the entrypoint has
/// returned; `compile_with_llvm` checks it while holding the module lock, so
/// an already-queued promotion drops out at lock-handoff speed rather than
/// running a compile for a program that has finished.
pub fn retier_abandon() {
    RETIER_ABANDON.store(true, std::sync::atomic::Ordering::Relaxed);
}

/// Whether the array-layout probe is on. See its use in `GetArray`.
fn stride_probe_enabled() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var("ASH_STRIDE_PROBE").is_ok())
}

/// Report, once per program, whether the referenced objects in one array sit
/// at a constant stride.
///
/// # Safety
/// `arr` must be a live `varray`: header at 0, size at 16, data at 24.
unsafe fn stride_probe(arr: *const u8, func: &str) {
    static DONE: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);
    let size = *(arr.add(16) as *const i32);
    if size < 4 || DONE.swap(true, std::sync::atomic::Ordering::Relaxed) {
        return;
    }
    let n = size.min(64) as usize;
    let data = arr.add(24) as *const usize;
    let mut ptrs = Vec::with_capacity(n);
    for i in 0..n {
        let p = *data.add(i);
        // Only pointer-like elements say anything about object layout.
        if p < 0x1000 || p % 8 != 0 {
            return;
        }
        ptrs.push(p);
    }
    let deltas: Vec<i64> = ptrs.windows(2).map(|w| w[1] as i64 - w[0] as i64).collect();
    let first = deltas[0];
    let constant = deltas.iter().all(|&d| d == first);
    let mut uniq: Vec<i64> = deltas.clone();
    uniq.sort_unstable();
    uniq.dedup();
    // The histogram matters more than the verdict: a stride that holds for
    // most of an array with a few jumps is a different (and checkable)
    // situation from one that is genuinely scattered.
    {
        let mut counts: std::collections::HashMap<i64, usize> = std::collections::HashMap::new();
        for &d in &deltas {
            *counts.entry(d).or_default() += 1;
        }
        let mut rows: Vec<(i64, usize)> = counts.into_iter().collect();
        rows.sort_by_key(|&(_, c)| std::cmp::Reverse(c));
        let dominant = rows[0];
        eprintln!(
            "[stride-probe] delta histogram: {:?} — dominant {} covers {}/{}",
            &rows[..rows.len().min(6)],
            dominant.0,
            dominant.1,
            deltas.len()
        );
    }
    eprintln!(
        "[stride-probe] {func}: {n} elements, deltas {} — {}",
        if uniq.len() <= 4 {
            format!("{uniq:?}")
        } else {
            format!("{} distinct, first={first}", uniq.len())
        },
        if constant {
            format!("CONSTANT STRIDE {first} bytes: a[i].field is strided, not a gather")
        } else {
            "NOT constant: a[i].field needs a gather".to_string()
        }
    );
}

#[cfg(test)]
mod stub_bridge_tests {
    use super::*;
    use ash_core::native_lib::init_std_library;

    /// The stub bridge must never report a failure by returning a value:
    /// compiled code consumes that word as the callee's declared return type,
    /// so a `0` becomes a null pointer it dereferences immediately.
    ///
    /// This drives the exact failure the crash report came from — an
    /// interpreter-internal "Null access" with no throwable `vdynamic` — and
    /// asserts it arrives at an armed native trap as a real HL exception
    /// value, which is what lets the Haxe-side `catch` see it.
    #[test]
    fn interpreter_internal_failure_raises_into_the_trap_chain() {
        init_std_library().expect("std library");
        let resolver = NativeFunctionResolver::new();

        let setup = resolver
            .resolve_function("std", "hlp_setup_trap_jit")
            .expect("hlp_setup_trap_jit");
        let remove = resolver
            .resolve_function("std", "hlp_remove_trap_jit")
            .expect("hlp_remove_trap_jit");
        let get_exc = resolver
            .resolve_function("std", "hlp_get_exc_value")
            .expect("hlp_get_exc_value");
        let clear_exc = resolver
            .resolve_function("std", "hlp_clear_exc_value")
            .expect("hlp_clear_exc_value");

        unsafe {
            let jumped = run_with_hl_trap(setup, remove, || {
                // No HL value on this error, exactly like a `NullCheck`
                // failure raised while the bridge re-enters the interpreter.
                let err = anyhow::Error::new(HLExceptionPropagation {
                    value: NanBoxedValue::null(),
                    message: Some("Null access".to_string()),
                    // No frames: the bridge raises before any interpreter
                    // frame exists to capture.
                    stack: Vec::new(),
                });
                HLInterpreter::raise_stub_bridge_failure(&resolver, 698, err);
            });
            assert_eq!(jumped, 1, "stub bridge failure did not longjmp");

            // Reached only via longjmp out of the raise.
            type FnGetExc = unsafe extern "C" fn() -> *mut hl::vdynamic;
            let exc = std::mem::transmute::<*mut c_void, FnGetExc>(get_exc)();
            assert!(
                !exc.is_null(),
                "raise produced no exception value — the trap chain saw nothing to catch"
            );

            // `hl_error`-shaped value: a bytes dynamic carrying the message.
            let msg_ptr = (*exc).v.bytes as *const u16;
            assert!(!msg_ptr.is_null(), "exception carries no message");
            let mut units = Vec::new();
            let mut i = 0isize;
            while *msg_ptr.offset(i) != 0 && i < 512 {
                units.push(*msg_ptr.offset(i));
                i += 1;
            }
            let msg = String::from_utf16_lossy(&units);
            assert!(
                msg.contains("Null access"),
                "unexpected exception message: {msg}"
            );

            type FnClearExc = unsafe extern "C" fn();
            std::mem::transmute::<*mut c_void, FnClearExc>(clear_exc)();
        }
    }
}
