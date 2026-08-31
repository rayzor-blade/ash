//! Cranelift backend wiring: production flag set, bulk symbol registration,
//! and the runtime handles the lowering needs.
//!
//! Construction is deliberately cheap (~1 ms measured in the spike) and
//! happens lazily on the broker thread at the first promotion, so nothing is
//! added to process startup — the LLVM tier's pre-warm keeps that slot.

use anyhow::{anyhow, bail, Result};
use std::collections::HashMap;
use std::ffi::c_void;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex, OnceLock};

use beadie::{Bead, CraneliftBackend, CraneliftConfig, CraneliftFunctionDef, JitBackend};
use cranelift_codegen::ir::{Function, Signature};
use cranelift_codegen::isa::CallConv;

use crate::air_pipeline::AshModule;
use crate::bytecode::DecodedBytecode;
use crate::hl_bindings as hl;
use crate::native_lib::NativeFunctionResolver;
use crate::opcodes::Reg;
use crate::types::TypeRef;

use super::lower::{lower_function, LoweredFunction};

// ─────────────────────────────────────────────────────────────────────────────
// Backend
// ─────────────────────────────────────────────────────────────────────────────

/// ash's wrapper around beadie's `CraneliftBackend`.
///
/// One `JITModule` lives inside for the whole process; it is never dropped
/// while compiled pointers can still be called (dropping it would `mprotect`
/// the code pages back to RW and free them).
pub struct AshCraneliftBackend {
    inner: CraneliftBackend,
    /// Monotonic suffix so a recompiled findex never collides with an
    /// already-declared symbol in the module.
    seq: AtomicU64,
    /// Number of `lib@symbol` pairs registered with the `JITBuilder`.
    registered_symbols: usize,
}

impl AshCraneliftBackend {
    /// Build the backend with the production flag set from the spike:
    /// `opt_level=speed`, no probestack, no PIC, no colocated libcalls,
    /// frame pointers preserved (so the GC's conservative stack walk and
    /// crash backtraces still work through Cranelift frames).
    ///
    /// Every entry of the process-global symbol table is registered here:
    /// `JITBuilder::symbol` is build-time only, so a native discovered later
    /// could not be linked at all.
    pub fn new() -> Result<Self> {
        // Knobs for re-running the tier-configuration measurement, not for
        // production use. What they showed on deltablue/closure_call/
        // binary_trees, best of three, warm binary:
        //
        //   enable_verifier=false      no change -- already off in release
        //   regalloc_algorithm         28.1ms -> 26.3ms of compile (6%),
        //     =single_pass             and slightly worse code
        //   opt_level=none             28.1ms -> 28.1ms of compile (0%),
        //                              and deltablue 0.07s -> 0.45s
        //
        // The last one is the informative one: this tier's compile time is
        // spent in lowering, regalloc and emission rather than in the
        // optimizer, so trading the optimizer away buys nothing and costs 6x
        // execution. A faster first rung has to come from somewhere else.
        let opt = std::env::var("ASH_CL_OPT").unwrap_or_else(|_| "speed".into());
        let mut cfg = CraneliftConfig::new()
            .opt_level(&opt)
            .set("enable_probestack", "false")
            .set("is_pic", "false")
            .set("use_colocated_libcalls", "false")
            .set("preserve_frame_pointers", "true");
        if std::env::var("ASH_CL_REGALLOC").as_deref() == Ok("single_pass") {
            cfg = cfg.set("regalloc_algorithm", "single_pass");
        }
        if std::env::var("ASH_CL_VERIFIER").as_deref() == Ok("0") {
            cfg = cfg.set("enable_verifier", "false");
        }

        let entries = NativeFunctionResolver::symbol_table_entries();
        for (name, addr) in &entries {
            cfg = cfg.symbol(name, *addr as *const u8);
        }
        let registered_symbols = entries.len();

        let inner = cfg
            .build()
            .map_err(|e| anyhow!("cranelift backend build failed: {e}"))?;
        Ok(Self {
            inner,
            seq: AtomicU64::new(0),
            registered_symbols,
        })
    }

    pub fn registered_symbols(&self) -> usize {
        self.registered_symbols
    }

    pub fn default_call_conv(&self) -> CallConv {
        self.inner.isa().default_call_conv()
    }

    pub fn make_signature(&self) -> Signature {
        self.inner.make_signature()
    }

    pub fn new_def(
        &self,
        sig: Signature,
        name: &str,
    ) -> std::result::Result<CraneliftFunctionDef, cranelift_module::ModuleError> {
        self.inner.new_def(sig, name)
    }

    pub fn import_function(
        &self,
        name: &str,
        sig: &Signature,
        func: &mut Function,
    ) -> std::result::Result<cranelift_codegen::ir::FuncRef, cranelift_module::ModuleError> {
        self.inner.import_function(name, sig, func)
    }

    pub fn unique_name(&self, findex: usize, name: &str) -> String {
        let n = self.seq.fetch_add(1, Ordering::Relaxed);
        // Keep the HL name in the symbol for readability in profiles, but
        // sanitise it — HL names contain characters cranelift-module accepts
        // but that make disassembly noisy.
        let sanitized: String = name
            .chars()
            .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
            .take(48)
            .collect();
        format!("ash_cl_{findex}_{sanitized}_{n}")
    }

    /// Compile an already-lowered definition and return its code pointer.
    /// The OSR-entry path uses this: it builds its own `def` (different
    /// signature, different prologue) and needs only the address back.
    pub fn compile_def(&self, bead: &Arc<Bead>, def: CraneliftFunctionDef) -> Result<*mut ()> {
        let code = self
            .inner
            .compile(bead, def)
            .map_err(|e| anyhow!("cranelift compile failed: {e}"))?;
        if code.is_null() {
            bail!("cranelift returned a null entry pointer");
        }
        Ok(code)
    }

    /// Lower and compile one bytecode function. Returns the entry address and
    /// the marshaling metadata the interpreter needs.
    ///
    /// An `Err` means "this tier declines" — the caller should fall back to
    /// the LLVM tier, not blacklist the function.
    pub fn compile_findex(
        &self,
        bead: &Arc<Bead>,
        ctx: &CraneliftTierContext,
        findex: usize,
    ) -> Result<(usize, LoweredMeta)> {
        let _phase = crate::profile::scope("cranelift compile");
        crate::profile::count("cranelift attempts", 1);
        let LoweredFunction {
            def,
            arg_kinds,
            ret_kind,
            num_ops,
        } = {
            let _phase = crate::profile::scope("clif lower");
            // AIR codegen first, opcode lowering behind it. See
            // `super::air::lower_best`.
            super::air::lower_best(self, ctx, findex)?
        };
        let code = {
            let _phase = crate::profile::scope("clif codegen");
            self.inner.compile(bead, def).map_err(|e| {
                let msg = e.to_string();
                let detail = format!("{e:?}");
                // A verifier or regalloc-checker error is not a decline — it
                // means the lowering emitted invalid CLIF, and the LLVM
                // fallback would otherwise mask the bug entirely. Report it
                // unconditionally; the caller may still fall back, but never
                // silently. (`CodegenError::Verifier` displays as "Verifier
                // errors"; the detail rides in the source chain.)
                if msg.contains("Verifier") || msg.contains("Regalloc") {
                    eprintln!(
                        "[cranelift] INVALID CLIF for findex={findex}: {detail} — \
                         this is an ash lowering bug, not an unsupported function"
                    );
                }
                anyhow!("cranelift compile failed: {msg}")
            })?
        };
        if code.is_null() {
            bail!("cranelift returned a null entry pointer");
        }
        Ok((
            code as usize,
            LoweredMeta {
                arg_kinds,
                ret_kind,
                num_ops,
            },
        ))
    }
}

/// Marshaling metadata produced alongside the compiled code.
pub struct LoweredMeta {
    // Alias-typed on purpose: bindgen gives `hl_type_kind` u32 under clang
    // and i32 under MSVC, so a fixed-width field here breaks one platform.
    pub arg_kinds: Vec<hl::hl_type_kind>,
    pub ret_kind: hl::hl_type_kind,
    pub num_ops: usize,
}

// ─────────────────────────────────────────────────────────────────────────────
// Runtime handles for lowering
// ─────────────────────────────────────────────────────────────────────────────

/// Everything the lowering reads that is fixed for the whole run: the decoded
/// bytecode, the shared `globals_data` / `functions_ptrs` arrays, findex
/// lookup tables, and interned constants.
///
/// Raw pointers make this neither `Send` nor `Sync` by default; both are
/// asserted below because the referenced memory is (a) immutable for the
/// bytecode and (b) the same shared arrays the interpreter and the LLVM tier
/// already read and write from multiple threads.
pub struct CraneliftTierContext {
    bytecode: *const DecodedBytecode,
    globals_data: usize,
    nglobals: usize,
    functions_ptrs: usize,
    max_findex: usize,
    findex_to_func: HashMap<usize, usize>,
    findex_to_native: HashMap<usize, usize>,
    /// Per native index: the canonical `lib@symbol` key, when resolved.
    native_keys: Vec<Option<String>>,
    /// Interned NUL-terminated UTF-16 buffers for `Opcode::String`, leaked
    /// (compiled code embeds their addresses), keyed by string index.
    strings: Mutex<HashMap<usize, usize>>,
    /// Interned byte-array constants for `Opcode::Bytes`, keyed by constant
    /// index. Leaked for the same reason `strings` is, and copied out of the
    /// decoded table rather than pointed into it so a Cranelift frame holds
    /// the same kind of private, immutable buffer the LLVM tier's
    /// `Bytes_<n>` global gives it.
    bytes: Mutex<HashMap<usize, usize>>,
    /// Interned UTF-16 messages for runtime error calls.
    messages: Mutex<HashMap<String, usize>>,
    /// Bytecode string index -> collision-resolved HashLink field hash.
    /// `hlp_hash_gen(..., true)` also records the reverse name mapping used
    /// by Reflect.fields, so this cannot be replaced by the arithmetic hash
    /// alone.
    field_hashes: Mutex<HashMap<usize, i32>>,
    dyn_compare: usize,
    same_type: usize,
    hash_gen: usize,
    hl_error: usize,
    /// Runtime `hl_type*` per bytecode type index, copied from the
    /// interpreter's `CTypeFactory`. These are the identities compiled code
    /// must hand to the allocators and store in object headers — the decoded
    /// `DecodedBytecode::types` is a description, not a runtime object.
    c_types: Vec<usize>,
    /// Allocation helpers, resolved once. Zero means unavailable, which
    /// declines the instruction that needs it rather than the tier.
    alloc_obj: usize,
    alloc_dynobj: usize,
    alloc_virtual: usize,
    /// `hlp_alloc_closure_void(type, fun) -> vclosure` and
    /// `hlp_alloc_closure_ptr(type, fun, value) -> vclosure`: the two
    /// closure constructors, split on whether a receiver is bound.
    alloc_closure_void: usize,
    alloc_closure_ptr: usize,
    /// The cast helpers: `hlp_make_dyn(data, t)` boxes, `hlp_dyn_castp(data,
    /// src_t, dst_t)` is the checked reference cast, `hl_to_virtual(vt, obj)`
    /// wraps an object in a virtual.
    throw: usize,
    rethrow: usize,
    make_dyn: usize,
    dyn_castp: usize,
    dyn_castd: usize,
    dyn_castf: usize,
    dyn_casti64: usize,
    dyn_casti: usize,
    to_virtual: usize,
    /// `hlp_alloc_enum(type, construct) -> venum`. It sizes the allocation
    /// from the construct it is handed, so the type has to be the initialized
    /// runtime `hl_type`: a bare one carries no `tenum` to read the construct
    /// out of.
    alloc_enum: usize,
    /// The `hlp_dyn_get*` / `hlp_dyn_set*` family: the accessors a field
    /// resolved by name hash goes through. ash_std picks between them by kind
    /// at run time in `hlp_get_dynget` / `hlp_get_dynset`; [`dyn_shape`] is
    /// that same split made at compile time, once per [`DynShape`].
    dyn_getd: usize,
    dyn_getf: usize,
    dyn_geti64: usize,
    dyn_geti: usize,
    dyn_getp: usize,
    dyn_setd: usize,
    dyn_setf: usize,
    dyn_seti64: usize,
    dyn_seti: usize,
    dyn_setp: usize,
    /// Boxed numeric coercions used by `SafeCast` from `Dynamic`/nullable to
    /// a primitive. These accept the `vdynamic*` directly and return zero for
    /// null, matching the LLVM tier's unboxing path.
    dyn_todouble: usize,
    dyn_tofloat: usize,
    dyn_toi64: usize,
    dyn_toint: usize,
    /// Boxing-aware closure dispatch for a `CallClosure` whose operand is
    /// statically `Dynamic`/nullable. Unlike a direct call, this derives the
    /// concrete argument and return ABI from the closure's runtime `hl_type`.
    dyn_call: usize,
    /// Helpers used by HVIRTUAL method calls. Arguments cross the dynamic
    /// boundary boxed in a `varray`; the result comes back as `vdynamic*` and
    /// is unboxed by the same `dyn_to*` family above.
    alloc_dyn_array: usize,
    vcall_dyn: usize,
    setup_trap_jit: usize,
    remove_trap_jit: usize,
    get_exc_value: usize,
    clear_exc_value: usize,
    get_obj_proto: usize,
    /// Cooperative scheduler safe point used by compiled AIR V2 loop
    /// headers. Keeping the address in the tier context avoids native symbol
    /// lookup in every per-function compile.
    fiber_poll: usize,
    /// Address of ash_std's monotonically increasing poll epoch. Loop headers
    /// compare it with the last generation handled by this activation.
    fiber_poll_epoch: usize,
    call_conv: CallConv,
    /// AIR v2's view of the module, built on first use. Only the `ASH_AIR=v2`
    /// path touches it. Building it is O(functions + natives), so it is held
    /// per context rather than rebuilt per compile — that is the difference
    /// between a constant and a per-promotion cost on a module with 20k
    /// functions.
    air_module: OnceLock<AshModule<'static>>,
}

// SAFETY: `bytecode` points at the `DecodedBytecode` owned by the process
// entry point, which outlives every tier and is never mutated after decoding.
// `globals_data` / `functions_ptrs` are the shared runtime arrays the
// interpreter, the LLVM tier and native code already share.
unsafe impl Send for CraneliftTierContext {}
unsafe impl Sync for CraneliftTierContext {}

impl CraneliftTierContext {
    /// # Safety
    /// `bytecode` must outlive this context and must not be mutated for its
    /// lifetime. `globals_data` and `functions_ptrs` must be the shared
    /// runtime arrays (`CTypeFactory::globals_data` and
    /// `hl_module_context::functions_ptrs`), which live for the process.
    pub unsafe fn new(
        backend: &AshCraneliftBackend,
        bytecode: &DecodedBytecode,
        globals_data: *mut *mut c_void,
        nglobals: usize,
        functions_ptrs: *mut *mut c_void,
        max_findex: usize,
        c_types: &[usize],
    ) -> Result<Self> {
        let mut findex_to_func = HashMap::new();
        for (i, f) in bytecode.functions.iter().enumerate() {
            findex_to_func.insert(f.findex as usize, i);
        }
        let mut findex_to_native = HashMap::new();
        let mut native_keys = Vec::with_capacity(bytecode.natives.len());
        for (i, n) in bytecode.natives.iter().enumerate() {
            findex_to_native.insert(n.findex as usize, i);
            let clean = n.lib.strip_prefix('?').unwrap_or(&n.lib);
            let sym = format!("hlp_{}", n.name);
            let key = format!("{clean}@{sym}");
            // Only usable if the symbol table already resolved it — that is
            // exactly the set registered with the JITBuilder.
            let resolved = NativeFunctionResolver::lookup_symbol(&n.lib, &sym)
                .filter(|p| !p.is_null())
                .is_some();
            native_keys.push(if resolved { Some(key) } else { None });
        }

        let resolver = NativeFunctionResolver::new();
        let dyn_compare = resolver
            .resolve_function("std", "hlp_dyn_compare")
            .map(|p| p as usize)
            .unwrap_or(0);
        let same_type = resolver
            .resolve_function("std", "hlp_same_type")
            .map(|p| p as usize)
            .unwrap_or(0);
        let hash_gen = resolver
            .resolve_function("std", "hlp_hash_gen")
            .map(|p| p as usize)
            .unwrap_or(0);
        let hl_error = resolver
            .resolve_function("std", "hlp_error")
            .map(|p| p as usize)
            .unwrap_or(0);
        let helper = |name: &str| {
            resolver
                .resolve_function("std", name)
                .map(|p| p as usize)
                .unwrap_or(0)
        };
        let alloc_obj = helper("hlp_alloc_obj");
        let alloc_dynobj = helper("hlp_alloc_dynobj");
        let alloc_virtual = helper("hlp_alloc_virtual");
        let alloc_closure_void = helper("hlp_alloc_closure_void");
        let alloc_closure_ptr = helper("hlp_alloc_closure_ptr");
        let throw = helper("hlp_throw");
        let rethrow = helper("hlp_rethrow");
        let make_dyn = helper("hlp_make_dyn");
        let dyn_castp = helper("hlp_dyn_castp");
        let dyn_castd = helper("hlp_dyn_castd");
        let dyn_castf = helper("hlp_dyn_castf");
        let dyn_casti64 = helper("hlp_dyn_casti64");
        let dyn_casti = helper("hlp_dyn_casti");
        let to_virtual = helper("hl_to_virtual");
        let alloc_enum = helper("hlp_alloc_enum");
        let dyn_getd = helper("hlp_dyn_getd");
        let dyn_getf = helper("hlp_dyn_getf");
        let dyn_geti64 = helper("hlp_dyn_geti64");
        let dyn_geti = helper("hlp_dyn_geti");
        let dyn_getp = helper("hlp_dyn_getp");
        let dyn_setd = helper("hlp_dyn_setd");
        let dyn_setf = helper("hlp_dyn_setf");
        let dyn_seti64 = helper("hlp_dyn_seti64");
        let dyn_seti = helper("hlp_dyn_seti");
        let dyn_setp = helper("hlp_dyn_setp");
        let dyn_todouble = helper("hlp_dyn_todouble");
        let dyn_tofloat = helper("hlp_dyn_tofloat");
        let dyn_toi64 = helper("hlp_dyn_toi64");
        let dyn_toint = helper("hlp_dyn_toint");
        let dyn_call = helper("hlp_dyn_call");
        let alloc_dyn_array = helper("hlp_alloc_dyn_array");
        let vcall_dyn = helper("hlp_vcall_dyn");
        let setup_trap_jit = helper("hlp_setup_trap_jit");
        let remove_trap_jit = helper("hlp_remove_trap_jit");
        let get_exc_value = helper("hlp_get_exc_value");
        let clear_exc_value = helper("hlp_clear_exc_value");
        let get_obj_proto = helper("hl_get_obj_proto");
        let fiber_poll = helper("hlp_fiber_poll");
        let fiber_poll_epoch = {
            let getter = helper("hlp_fiber_poll_epoch_address");
            if getter == 0 {
                0
            } else {
                let getter: unsafe extern "C" fn() -> *const u64 = std::mem::transmute(getter);
                getter() as usize
            }
        };

        Ok(Self {
            bytecode: bytecode as *const _,
            globals_data: globals_data as usize,
            nglobals,
            functions_ptrs: functions_ptrs as usize,
            max_findex,
            findex_to_func,
            findex_to_native,
            native_keys,
            strings: Mutex::new(HashMap::new()),
            bytes: Mutex::new(HashMap::new()),
            messages: Mutex::new(HashMap::new()),
            field_hashes: Mutex::new(HashMap::new()),
            dyn_compare,
            same_type,
            hash_gen,
            hl_error,
            c_types: c_types.to_vec(),
            alloc_obj,
            alloc_dynobj,
            alloc_virtual,
            alloc_closure_void,
            alloc_closure_ptr,
            throw,
            rethrow,
            make_dyn,
            dyn_castp,
            dyn_castd,
            dyn_castf,
            dyn_casti64,
            dyn_casti,
            to_virtual,
            alloc_enum,
            dyn_getd,
            dyn_getf,
            dyn_geti64,
            dyn_geti,
            dyn_getp,
            dyn_setd,
            dyn_setf,
            dyn_seti64,
            dyn_seti,
            dyn_setp,
            dyn_todouble,
            dyn_tofloat,
            dyn_toi64,
            dyn_toint,
            dyn_call,
            alloc_dyn_array,
            vcall_dyn,
            setup_trap_jit,
            remove_trap_jit,
            get_exc_value,
            clear_exc_value,
            get_obj_proto,
            fiber_poll,
            fiber_poll_epoch,
            call_conv: backend.default_call_conv(),
            air_module: OnceLock::new(),
        })
    }

    pub fn bytecode(&self) -> &DecodedBytecode {
        // SAFETY: see the type-level contract.
        unsafe { &*self.bytecode }
    }

    /// The module view AIR v2 lowers against.
    pub fn air_module(&self) -> &AshModule<'static> {
        self.air_module.get_or_init(|| {
            // SAFETY: the same contract `bytecode` is held to — the decoded
            // bytecode outlives this context and is never mutated after
            // decoding — which is exactly what the `'static` claims.
            AshModule::new(unsafe { &*self.bytecode })
        })
    }

    pub fn call_conv(&self) -> CallConv {
        self.call_conv
    }

    pub fn func_index(&self, findex: usize) -> Option<usize> {
        self.findex_to_func.get(&findex).copied()
    }

    pub fn native_index(&self, findex: usize) -> Option<usize> {
        self.findex_to_native.get(&findex).copied()
    }

    pub fn native_symbol_key(&self, native_idx: usize) -> Option<String> {
        self.native_keys.get(native_idx).cloned().flatten()
    }

    pub fn type_kind(&self, type_idx: usize) -> Result<hl::hl_type_kind> {
        self.bytecode()
            .types
            .get(type_idx)
            .map(|t| t.kind)
            .ok_or_else(|| anyhow!("type index {type_idx} out of range"))
    }

    /// Collision-resolved field hash, with its UTF-16 name registered in the
    /// runtime's reverse lookup table for reflection and serialization.
    pub fn field_name_hash(&self, string_idx: usize) -> Result<i32> {
        if let Some(&hash) = self
            .field_hashes
            .lock()
            .expect("Cranelift field hash cache poisoned")
            .get(&string_idx)
        {
            return Ok(hash);
        }
        let name = self
            .bytecode()
            .strings
            .get(string_idx)
            .ok_or_else(|| anyhow!("dynamic field name {string_idx} out of range"))?;
        let hash = if self.hash_gen == 0 {
            crate::layout::field_name_hash(name)
        } else {
            let utf16: Vec<u16> = name.encode_utf16().chain(std::iter::once(0)).collect();
            type HashGen = unsafe extern "C" fn(*const u16, bool) -> i32;
            let hash_gen: HashGen = unsafe { std::mem::transmute(self.hash_gen) };
            unsafe { hash_gen(utf16.as_ptr(), true) }
        };
        self.field_hashes
            .lock()
            .expect("Cranelift field hash cache poisoned")
            .insert(string_idx, hash);
        Ok(hash)
    }

    /// Compile-time hash of one field in an `HVIRTUAL` declaration.
    ///
    /// A virtual field opcode carries the field's slot, not its name. When
    /// the wrapper has no resolved `vfields[slot]` pointer, HashLink falls
    /// back to a dynamic lookup on the wrapped value using this hash. Keep
    /// the lookup on the decoded type here so both Cranelift reads and writes
    /// use exactly the hash the loader computed.
    pub fn virtual_field_hash(&self, type_idx: usize, field: usize) -> Result<i32> {
        self.bytecode()
            .types
            .get(type_idx)
            .and_then(|t| t.virt.as_ref())
            .and_then(|v| v.fields.get(field))
            .map(|f| f.hashed_name)
            .ok_or_else(|| anyhow!("virtual type {type_idx} has no field {field}"))
    }

    pub fn virtual_field_kind(&self, type_idx: usize, field: usize) -> Result<hl::hl_type_kind> {
        let field_ty = self
            .bytecode()
            .types
            .get(type_idx)
            .and_then(|t| t.virt.as_ref())
            .and_then(|v| v.fields.get(field))
            .map(|f| f.type_.0)
            .ok_or_else(|| anyhow!("virtual type {type_idx} has no field {field}"))?;
        self.type_kind(field_ty)
    }

    /// Kind of the register at `reg` in a register-type table. The table is
    /// passed in rather than read off an `HLFunction` because the array being
    /// compiled may be AIR v2's serialization, whose table can be longer than
    /// the function's own (see [`super::air::Body`]).
    pub fn reg_kind(&self, regs: &[TypeRef], reg: Reg) -> Result<hl::hl_type_kind> {
        let tr = regs
            .get(reg.0 as usize)
            .ok_or_else(|| anyhow!("register {} out of range", reg.0))?;
        self.type_kind(tr.0)
    }

    /// Address of `globals_data[index]` — the same slot the interpreter's
    /// `SetGlobal` mirrors into and native code reads through `global_value`.
    pub fn global_slot_addr(&self, index: usize) -> Result<usize> {
        if self.globals_data == 0 || index >= self.nglobals {
            bail!("global {index} out of range");
        }
        Ok(self.globals_data + index * std::mem::size_of::<usize>())
    }

    /// Address of `functions_ptrs[findex]`.
    /// Base of the shared `functions_ptrs` array, for code that computes a
    /// slot address at run time (the stub self-heal).
    pub fn functions_ptrs_base(&self) -> usize {
        self.functions_ptrs
    }

    pub fn function_slot_addr(&self, findex: usize) -> Result<usize> {
        if self.functions_ptrs == 0 || findex >= self.max_findex {
            bail!("findex {findex} out of functions_ptrs range");
        }
        Ok(self.functions_ptrs + findex * std::mem::size_of::<usize>())
    }

    /// Interned NUL-terminated UTF-16 buffer for a bytecode string constant,
    /// mirroring the LLVM tier's `String_<n>` globals.
    pub fn string_ptr(&self, index: usize) -> Result<usize> {
        if let Some(&p) = self
            .strings
            .lock()
            .expect("cranelift string cache poisoned")
            .get(&index)
        {
            return Ok(p);
        }
        let s = self
            .bytecode()
            .strings
            .get(index)
            .ok_or_else(|| anyhow!("string constant {index} out of range"))?;
        let buf: Vec<u16> = s.encode_utf16().chain(std::iter::once(0)).collect();
        let addr = Box::leak(buf.into_boxed_slice()).as_ptr() as usize;
        self.strings
            .lock()
            .expect("cranelift string cache poisoned")
            .insert(index, addr);
        Ok(addr)
    }

    /// Interned byte-array constant, mirroring the LLVM tier's `Bytes_<n>`
    /// globals: `bytes_pos[index]` up to the next entry, or the end of the
    /// blob for the last one — the constant pool stores offsets, not lengths.
    ///
    /// No NUL is appended, matching the LLVM tier's `const_string(.., false)`;
    /// the blob already carries whatever terminator the constant needs.
    pub fn bytes_ptr(&self, index: usize) -> Result<usize> {
        if let Some(&p) = self
            .bytes
            .lock()
            .expect("cranelift bytes cache poisoned")
            .get(&index)
        {
            return Ok(p);
        }
        let bc = self.bytecode();
        let start = *bc
            .bytes_pos
            .get(index)
            .ok_or_else(|| anyhow!("bytes constant {index} out of range"))?;
        let end = bc
            .bytes_pos
            .get(index + 1)
            .copied()
            .unwrap_or(bc.bytes_data.len());
        let slice = bc
            .bytes_data
            .get(start..end)
            .ok_or_else(|| anyhow!("bytes constant {index} spans past the blob"))?;
        let addr = Box::leak(slice.to_vec().into_boxed_slice()).as_ptr() as usize;
        self.bytes
            .lock()
            .expect("cranelift bytes cache poisoned")
            .insert(index, addr);
        Ok(addr)
    }

    /// Interned NUL-terminated UTF-16 message for a runtime error call.
    pub fn utf16_message(&self, msg: &str) -> usize {
        let mut cache = self
            .messages
            .lock()
            .expect("cranelift message cache poisoned");
        if let Some(&p) = cache.get(msg) {
            return p;
        }
        let buf: Vec<u16> = msg.encode_utf16().chain(std::iter::once(0)).collect();
        let addr = Box::leak(buf.into_boxed_slice()).as_ptr() as usize;
        cache.insert(msg.to_string(), addr);
        addr
    }

    pub fn dyn_compare_addr(&self) -> Result<usize> {
        if self.dyn_compare == 0 {
            bail!("hlp_dyn_compare unavailable");
        }
        Ok(self.dyn_compare)
    }

    pub fn same_type_addr(&self) -> Result<usize> {
        if self.same_type == 0 {
            bail!("hlp_same_type unavailable");
        }
        Ok(self.same_type)
    }

    pub fn hl_error_addr(&self) -> Result<usize> {
        if self.hl_error == 0 {
            bail!("hlp_error unavailable");
        }
        Ok(self.hl_error)
    }

    /// The runtime `hl_type*` for a bytecode type index.
    pub fn type_ptr(&self, type_idx: usize) -> Result<usize> {
        match self.c_types.get(type_idx) {
            Some(&p) if p != 0 => Ok(p),
            Some(_) => bail!("type {type_idx} has no runtime hl_type"),
            None => bail!("type index {type_idx} out of range"),
        }
    }

    /// `hlp_throw` / `hlp_rethrow`. Neither returns — both longjmp to the
    /// nearest armed trap — so a call to either is followed by unreachable
    /// code rather than a jump.
    pub fn throw_helper(&self, rethrow: bool) -> Result<usize> {
        let a = if rethrow { self.rethrow } else { self.throw };
        if a == 0 {
            bail!("throw helper unavailable");
        }
        Ok(a)
    }

    /// Yield to Ash's cooperative scheduler. Compiled AIR V2 calls this from
    /// event-driven loop-header safe points so CPU-bound Haxe code cannot
    /// starve sibling logical threads.
    pub fn fiber_poll_helper(&self) -> Result<usize> {
        if self.fiber_poll == 0 {
            bail!("hlp_fiber_poll unavailable");
        }
        Ok(self.fiber_poll)
    }

    pub fn fiber_poll_epoch_address(&self) -> Result<usize> {
        if self.fiber_poll_epoch == 0 {
            bail!("hlp_fiber_poll_epoch_address unavailable");
        }
        Ok(self.fiber_poll_epoch)
    }

    /// `hlp_make_dyn`, for `Cast::ToDyn`.
    pub fn make_dyn_helper(&self) -> Result<usize> {
        if self.make_dyn == 0 {
            bail!("hlp_make_dyn unavailable");
        }
        Ok(self.make_dyn)
    }

    /// `hlp_dyn_castp`, for `Cast::SafeCast` between reference types. It
    /// raises on a bad cast, which is why the caller must treat the call as
    /// able to throw.
    pub fn dyn_castp_helper(&self) -> Result<usize> {
        if self.dyn_castp == 0 {
            bail!("hlp_dyn_castp unavailable");
        }
        Ok(self.dyn_castp)
    }

    /// HashLink's checked scalar cast helper and its return shape.
    ///
    /// Unlike the `hlp_dyn_to*` convenience wrappers, these accept a slot in
    /// the source's native representation together with its static
    /// `hl_type*`. That distinction matters for a checked cast such as
    /// `String -> Int`: treating the String object as a boxed Dynamic would
    /// happen to find its header, but raw pointer families have no such
    /// header. The typed helpers reject every impossible source uniformly.
    /// The boolean says whether the helper also takes the destination type
    /// (`hlp_dyn_casti` does; the type-specific helpers do not).
    pub fn scalar_cast_helper(&self, kind: hl::hl_type_kind) -> Result<(usize, DynShape, bool)> {
        let (addr, shape, takes_dst_type) = match kind {
            hl::hl_type_kind_HF64 => (self.dyn_castd, DynShape::F64, false),
            hl::hl_type_kind_HF32 => (self.dyn_castf, DynShape::F32, false),
            hl::hl_type_kind_HI64 => (self.dyn_casti64, DynShape::I64, false),
            hl::hl_type_kind_HI32
            | hl::hl_type_kind_HBOOL
            | hl::hl_type_kind_HUI8
            | hl::hl_type_kind_HUI16 => (self.dyn_casti, DynShape::Int, true),
            _ => bail!("type kind {kind} is not a SafeCast primitive"),
        };
        if addr == 0 {
            bail!("checked scalar cast helper for kind {kind} unavailable");
        }
        Ok((addr, shape, takes_dst_type))
    }

    /// `hl_to_virtual`, for `Cast::ToVirtual`.
    pub fn to_virtual_helper(&self) -> Result<usize> {
        if self.to_virtual == 0 {
            bail!("hl_to_virtual unavailable");
        }
        Ok(self.to_virtual)
    }

    /// The closure constructors. `bound` selects the one that carries a
    /// receiver (`hlp_alloc_closure_ptr`) over the plain one.
    pub fn closure_helper(&self, bound: bool) -> Result<usize> {
        let addr = if bound {
            self.alloc_closure_ptr
        } else {
            self.alloc_closure_void
        };
        if addr == 0 {
            bail!("closure allocation helper unavailable");
        }
        Ok(addr)
    }

    /// The enum allocator, which `EnumAlloc` and the allocating half of
    /// `MakeEnum` both go through.
    pub fn alloc_enum_helper(&self) -> Result<usize> {
        if self.alloc_enum == 0 {
            bail!("hlp_alloc_enum unavailable");
        }
        Ok(self.alloc_enum)
    }

    /// The runtime type pointer for a callable findex, which a closure carries
    /// so `hl_dyn_call` and friends can read its signature. StaticClosure can
    /// legally target either bytecode or a native (for example
    /// `std@dyn_compare`), and both live in `functions_ptrs`/the findex space.
    pub fn func_type_ptr(&self, findex: usize) -> Result<usize> {
        let bc = self.bytecode();
        let type_idx = if let Some(i) = self.func_index(findex) {
            bc.functions[i].type_.0
        } else if let Some(i) = self.native_index(findex) {
            bc.natives[i].type_.0
        } else {
            bail!("findex {findex} is not a callable function")
        };
        self.type_ptr(type_idx)
    }

    /// The `hlp_dyn_get*` accessor a value of `kind` is read through, and the
    /// shape of the call.
    pub fn dyn_get_helper(&self, kind: hl::hl_type_kind) -> Result<(usize, DynShape)> {
        let shape = dyn_shape(kind);
        let addr = match shape {
            DynShape::F64 => self.dyn_getd,
            DynShape::F32 => self.dyn_getf,
            DynShape::I64 => self.dyn_geti64,
            DynShape::Int => self.dyn_geti,
            DynShape::Ptr => self.dyn_getp,
        };
        if addr == 0 {
            bail!("dynamic field getter for kind {kind} unavailable");
        }
        Ok((addr, shape))
    }

    /// The `hlp_dyn_set*` accessor a value of `kind` is written through, and
    /// the shape of the call.
    pub fn dyn_set_helper(&self, kind: hl::hl_type_kind) -> Result<(usize, DynShape)> {
        let shape = dyn_shape(kind);
        let addr = match shape {
            DynShape::F64 => self.dyn_setd,
            DynShape::F32 => self.dyn_setf,
            DynShape::I64 => self.dyn_seti64,
            DynShape::Int => self.dyn_seti,
            DynShape::Ptr => self.dyn_setp,
        };
        if addr == 0 {
            bail!("dynamic field setter for kind {kind} unavailable");
        }
        Ok((addr, shape))
    }

    /// Primitive unboxer for a `SafeCast` whose source is `HDYN`/`HNULL`.
    pub fn dyn_unbox_helper(&self, kind: hl::hl_type_kind) -> Result<(usize, DynShape)> {
        let (addr, shape) = match kind {
            hl::hl_type_kind_HF64 => (self.dyn_todouble, DynShape::F64),
            hl::hl_type_kind_HF32 => (self.dyn_tofloat, DynShape::F32),
            hl::hl_type_kind_HI64 => (self.dyn_toi64, DynShape::I64),
            hl::hl_type_kind_HI32
            | hl::hl_type_kind_HBOOL
            | hl::hl_type_kind_HUI8
            | hl::hl_type_kind_HUI16 => (self.dyn_toint, DynShape::Int),
            _ => bail!("type kind {kind} is not a SafeCast primitive"),
        };
        if addr == 0 {
            bail!("dynamic unboxer for kind {kind} unavailable");
        }
        Ok((addr, shape))
    }

    /// Boxing-aware dynamic closure dispatch.
    pub fn dyn_call_helper(&self) -> Result<usize> {
        if self.dyn_call == 0 {
            bail!("hlp_dyn_call unavailable");
        }
        Ok(self.dyn_call)
    }

    /// Allocate a dynamic argument array for an HVIRTUAL call.
    pub fn alloc_dyn_array_helper(&self) -> Result<usize> {
        if self.alloc_dyn_array == 0 {
            bail!("hlp_alloc_dyn_array unavailable");
        }
        Ok(self.alloc_dyn_array)
    }

    /// Runtime HVIRTUAL dispatch using the method's concrete ABI.
    pub fn vcall_dyn_helper(&self) -> Result<usize> {
        if self.vcall_dyn == 0 {
            bail!("hlp_vcall_dyn unavailable");
        }
        Ok(self.vcall_dyn)
    }

    pub fn setup_trap_helper(&self) -> Result<usize> {
        if self.setup_trap_jit == 0 {
            bail!("hlp_setup_trap_jit unavailable");
        }
        Ok(self.setup_trap_jit)
    }

    pub fn remove_trap_helper(&self) -> Result<usize> {
        if self.remove_trap_jit == 0 {
            bail!("hlp_remove_trap_jit unavailable");
        }
        Ok(self.remove_trap_jit)
    }

    pub fn get_exc_helper(&self) -> Result<usize> {
        if self.get_exc_value == 0 {
            bail!("hlp_get_exc_value unavailable");
        }
        Ok(self.get_exc_value)
    }

    pub fn clear_exc_helper(&self) -> Result<usize> {
        if self.clear_exc_value == 0 {
            bail!("hlp_clear_exc_value unavailable");
        }
        Ok(self.clear_exc_value)
    }

    pub fn get_obj_proto_helper(&self) -> Result<usize> {
        if self.get_obj_proto == 0 {
            bail!("hl_get_obj_proto unavailable");
        }
        Ok(self.get_obj_proto)
    }

    /// `hlp_alloc_obj`, `hlp_alloc_dynobj` and `hlp_alloc_virtual` — the three
    /// allocators `New` dispatches to on the destination's type kind, the same
    /// split the LLVM tier makes.
    pub fn alloc_helper(&self, kind: hl::hl_type_kind) -> Result<(usize, bool)> {
        let (addr, takes_type) = match kind {
            hl::hl_type_kind_HOBJ | hl::hl_type_kind_HSTRUCT => (self.alloc_obj, true),
            hl::hl_type_kind_HDYNOBJ => (self.alloc_dynobj, false),
            hl::hl_type_kind_HVIRTUAL => (self.alloc_virtual, true),
            _ => bail!("New on type kind {kind}"),
        };
        if addr == 0 {
            bail!("allocation helper for kind {kind} unavailable");
        }
        Ok((addr, takes_type))
    }
}

/// Which member of the `hlp_dyn_get*` / `hlp_dyn_set*` family a value is
/// accessed through.
///
/// The family is split by machine shape, not by HL kind: the four integer
/// kinds up to 32 bits share one accessor and every reference kind shares
/// another, so five members cover the whole kind space.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DynShape {
    F64,
    F32,
    I64,
    /// `HI32`, `HBOOL`, `HUI8`, `HUI16` — all carried as a C `int`.
    Int,
    /// Every reference kind, plus anything with no accessor of its own.
    Ptr,
}

impl DynShape {
    /// Whether the accessor takes the value's `hl_type*`.
    ///
    /// The int and pointer accessors need it because they serve several kinds
    /// each and cast the stored field to (or from) the one asked for. The
    /// float and 64-bit ones serve exactly one kind, which their name already
    /// says.
    pub fn takes_type(self) -> bool {
        matches!(self, DynShape::Int | DynShape::Ptr)
    }
}

/// The kind → accessor split, mirroring `hlp_get_dynget` / `hlp_get_dynset`
/// in `std/src/obj.rs`. Held in one place so a read and a write of the same
/// field can never pick differently shaped halves of the pair.
fn dyn_shape(kind: hl::hl_type_kind) -> DynShape {
    match kind {
        hl::hl_type_kind_HF64 => DynShape::F64,
        hl::hl_type_kind_HF32 => DynShape::F32,
        hl::hl_type_kind_HI64 => DynShape::I64,
        hl::hl_type_kind_HI32
        | hl::hl_type_kind_HBOOL
        | hl::hl_type_kind_HUI8
        | hl::hl_type_kind_HUI16 => DynShape::Int,
        _ => DynShape::Ptr,
    }
}
