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
use std::sync::{Arc, Mutex};

use beadie::{Bead, CraneliftBackend, CraneliftConfig, CraneliftFunctionDef, JitBackend};
use cranelift_codegen::ir::{Function, Signature};
use cranelift_codegen::isa::CallConv;

use crate::bytecode::DecodedBytecode;
use crate::hl_bindings as hl;
use crate::native_lib::NativeFunctionResolver;
use crate::opcodes::Reg;
use crate::types::HLFunction;

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
        let mut cfg = CraneliftConfig::new()
            .opt_level("speed")
            .set("enable_probestack", "false")
            .set("is_pic", "false")
            .set("use_colocated_libcalls", "false")
            .set("preserve_frame_pointers", "true");

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
            lower_function(self, ctx, findex)?
        };
        let code = {
            let _phase = crate::profile::scope("clif codegen");
            self.inner
                .compile(bead, def)
                .map_err(|e| anyhow!("cranelift compile failed: {e}"))?
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
    pub arg_kinds: Vec<u32>,
    pub ret_kind: u32,
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
    /// Interned UTF-16 messages for runtime error calls.
    messages: Mutex<HashMap<String, usize>>,
    dyn_compare: usize,
    hl_error: usize,
    call_conv: CallConv,
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
        let hl_error = resolver
            .resolve_function("std", "hlp_error")
            .map(|p| p as usize)
            .unwrap_or(0);

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
            messages: Mutex::new(HashMap::new()),
            dyn_compare,
            hl_error,
            call_conv: backend.default_call_conv(),
        })
    }

    pub fn bytecode(&self) -> &DecodedBytecode {
        // SAFETY: see the type-level contract.
        unsafe { &*self.bytecode }
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

    pub fn reg_kind(&self, func: &HLFunction, reg: Reg) -> Result<hl::hl_type_kind> {
        let tr = func
            .regs
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

    pub fn hl_error_addr(&self) -> Result<usize> {
        if self.hl_error == 0 {
            bail!("hlp_error unavailable");
        }
        Ok(self.hl_error)
    }
}
