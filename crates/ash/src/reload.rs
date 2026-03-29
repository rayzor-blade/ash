//! Hot-reload infrastructure: bytecode diffing and reload coordination.
//!
//! Compares old and new `DecodedBytecode` to determine which functions changed,
//! which were added/removed, and whether type layouts are compatible.

use crate::bytecode::DecodedBytecode;
use crate::hl;
use crate::jit::module::SharedRuntimeHandles;
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::Mutex;

/// Result of diffing two bytecode versions.
#[derive(Debug)]
pub struct ReloadDiff {
    /// Functions whose bytecode hash changed (need recompilation).
    pub changed: Vec<usize>,
    /// Functions present in new but not old bytecode.
    pub added: Vec<usize>,
    /// Functions present in old but not new bytecode.
    pub removed: Vec<usize>,
    /// True if any HOBJ/HSTRUCT type changed its field layout.
    /// When set, the reload must be aborted (existing heap objects would be corrupted).
    pub type_layout_changed: bool,
    /// True if the number of globals changed (globals array is fixed-size).
    pub globals_count_changed: bool,
}

impl ReloadDiff {
    /// Returns true if this diff is safe to apply (no layout changes, no global count changes).
    pub fn is_safe(&self) -> bool {
        !self.type_layout_changed && !self.globals_count_changed
    }

    /// Returns true if there are any actual changes to apply.
    pub fn has_changes(&self) -> bool {
        !self.changed.is_empty() || !self.added.is_empty() || !self.removed.is_empty()
    }
}

/// Compute the diff between old and new bytecode.
pub fn diff_bytecode(old: &DecodedBytecode, new: &DecodedBytecode) -> ReloadDiff {
    let old_hashes = old.compute_function_hashes();
    let new_hashes = new.compute_function_hashes();

    let old_findexes: HashSet<usize> = old_hashes.keys().copied().collect();
    let new_findexes: HashSet<usize> = new_hashes.keys().copied().collect();

    let changed: Vec<usize> = old_findexes
        .intersection(&new_findexes)
        .filter(|&&findex| old_hashes[&findex] != new_hashes[&findex])
        .copied()
        .collect();

    let added: Vec<usize> = new_findexes.difference(&old_findexes).copied().collect();
    let removed: Vec<usize> = old_findexes.difference(&new_findexes).copied().collect();

    let type_layout_changed = check_type_layout_compatibility(&old.types, &new.types);
    let globals_count_changed = old.globals.len() != new.globals.len();

    ReloadDiff {
        changed,
        added,
        removed,
        type_layout_changed,
        globals_count_changed,
    }
}

/// Check if any HOBJ/HSTRUCT type changed its field layout between old and new bytecode.
/// Returns `true` if an incompatible layout change was detected.
fn check_type_layout_compatibility(
    old_types: &[crate::types::HLType],
    new_types: &[crate::types::HLType],
) -> bool {
    let count = old_types.len().min(new_types.len());
    for i in 0..count {
        let old_t = &old_types[i];
        let new_t = &new_types[i];

        // Only check object/struct types (they have heap-allocated instances)
        if old_t.kind != new_t.kind {
            if is_obj_kind(old_t.kind) || is_obj_kind(new_t.kind) {
                return true; // Kind changed for an object type
            }
            continue;
        }

        if !is_obj_kind(old_t.kind) {
            continue;
        }

        // Compare field count and field types
        if let (Some(old_obj), Some(new_obj)) = (&old_t.obj, &new_t.obj) {
            if old_obj.fields.len() != new_obj.fields.len() {
                return true; // Field count changed
            }
            for (old_f, new_f) in old_obj.fields.iter().zip(new_obj.fields.iter()) {
                if old_f.name != new_f.name || old_f.type_.0 != new_f.type_.0 {
                    return true; // Field name or type changed
                }
            }
        }
    }

    // If type count changed and the extra types are objects, that's potentially unsafe
    // (but adding new types is generally OK — only changing existing ones is dangerous)
    false
}

fn is_obj_kind(kind: u32) -> bool {
    kind == hl::hl_type_kind_HOBJ || kind == hl::hl_type_kind_HSTRUCT
}

/// Collect the set of native function findexes from bytecode.
/// Used by `IndirectCallRewritePass` to know which calls should stay direct.
pub fn native_findexes(bytecode: &DecodedBytecode) -> HashSet<usize> {
    bytecode
        .natives
        .iter()
        .map(|n| n.findex as usize)
        .collect()
}

/// Perform a full hot-reload cycle.
///
/// 1. Re-decode bytecode from `path`
/// 2. Diff against `old_bytecode`
/// 3. Compile changed functions in a new LLVM context
/// 4. Patch `functions_ptrs` and flush affected vtable protos
///
/// Returns the diff (for logging) or an error if the reload is unsafe.
pub fn perform_reload(
    path: &std::path::Path,
    old_bytecode: &DecodedBytecode,
    functions_ptrs: &mut Vec<*mut std::ffi::c_void>,
    shared_runtime: &crate::jit::module::SharedRuntimeHandles,
) -> anyhow::Result<ReloadDiff> {
    use crate::bytecode::BytecodeDecoder;
    use crate::jit::module::JITModule;
    use inkwell::context::Context;

    // Step 1: Re-decode
    let new_bytecode = BytecodeDecoder::decode(path)?;

    // Step 2: Diff
    let diff = diff_bytecode(old_bytecode, &new_bytecode);

    if !diff.is_safe() {
        if diff.type_layout_changed {
            return Err(anyhow::anyhow!(
                "Hot reload aborted: type field layout changed (existing heap objects would be corrupted)"
            ));
        }
        if diff.globals_count_changed {
            return Err(anyhow::anyhow!(
                "Hot reload aborted: global count changed ({} -> {})",
                old_bytecode.globals.len(),
                new_bytecode.globals.len()
            ));
        }
    }

    if !diff.has_changes() {
        return Ok(diff);
    }

    // Step 3: Compile changed functions in a fresh LLVM context
    // (LLVM modules are immutable post-compilation; we leak the context
    //  intentionally — old JIT code may still be on active call stacks)
    let context = Box::leak(Box::new(Context::create()));
    let mut jit =
        JITModule::new_with_shared_runtime(context, path, shared_runtime.clone());

    for &findex in &diff.changed {
        match jit.promote_function_strict(findex) {
            Ok(meta) => {
                if findex < functions_ptrs.len() {
                    functions_ptrs[findex] = meta.fn_addr as *mut std::ffi::c_void;
                }
            }
            Err(e) => {
                eprintln!("[reload] failed to compile findex {}: {}", findex, e);
            }
        }
    }

    // Step 4: Flush vtable protos for types that have changed proto entries
    flush_affected_protos(shared_runtime, &diff.changed);

    // Step 5: Propagate changed field defaults to existing heap objects.
    // Compare-and-swap: only updates fields that still hold the V1 default,
    // preserving runtime modifications.
    let patches = compute_field_patches(old_bytecode, &new_bytecode, shared_runtime);
    if !patches.is_empty() {
        eprintln!(
            "[hot-reload] propagating {} field patch(es) to live heap objects",
            patches.len()
        );
        apply_field_patches(&patches);
    }

    // Leak the JIT module — its native code must stay alive
    std::mem::forget(jit);

    Ok(diff)
}

/// Flush vtable protos for all HOBJ/HSTRUCT types that might reference changed functions.
fn flush_affected_protos(
    shared: &crate::jit::module::SharedRuntimeHandles,
    _changed_findexes: &[usize],
) {
    // Resolve hlp_flush_proto dynamically from the std library
    let flush_fn = crate::native_lib::NativeFunctionResolver::new()
        .resolve_function("std", "hlp_flush_proto")
        .ok();
    let flush_fn = match flush_fn {
        Some(f) => f,
        None => return,
    };
    type FnFlush = unsafe extern "C" fn(*mut crate::hl::hl_type);
    let flush: FnFlush = unsafe { std::mem::transmute(flush_fn) };

    // Flush all HOBJ/HSTRUCT types — we don't track which types
    // reference which findexes, and the cost is just a lazy re-init on next dispatch.
    for c_type in &shared.c_types {
        if c_type.is_null() {
            continue;
        }
        unsafe {
            let kind = (**c_type).kind;
            if kind == hl::hl_type_kind_HOBJ || kind == hl::hl_type_kind_HSTRUCT {
                flush(*c_type);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Heap object property propagation
// ---------------------------------------------------------------------------

/// A single field update: for objects of a given type, if field at `offset`
/// still holds `old_value`, overwrite with `new_value`.
#[derive(Debug)]
struct FieldPatch {
    /// C-level hl_type pointer identifying which objects to patch
    type_ptr: *mut hl::hl_type,
    /// Byte offset of the field within the object
    offset: usize,
    /// Size of the field in bytes (4 for i32, 8 for pointer/f64, etc.)
    size: usize,
    /// Old default value bytes (from V1 constants)
    old_bytes: Vec<u8>,
    /// New default value bytes (from V2 constants)
    new_bytes: Vec<u8>,
}

unsafe impl Send for FieldPatch {}

/// Compute field patches by diffing V1 and V2 constant tables.
///
/// For each constant (global slot with pre-initialized HOBJ/HSTRUCT fields),
/// compare field values between V1 and V2. Fields that differ become patches.
pub fn compute_field_patches(
    old_bc: &DecodedBytecode,
    new_bc: &DecodedBytecode,
    shared: &SharedRuntimeHandles,
) -> Vec<FieldPatch> {
    use crate::types::HLConstant;

    let mut patches = Vec::new();
    let resolver = crate::native_lib::NativeFunctionResolver::new();

    // Resolve hlp_get_obj_rt for field offset lookup
    let get_rt_fn = match resolver.resolve_function("std", "hlp_get_obj_rt") {
        Ok(f) => f,
        Err(_) => return patches,
    };
    type FnGetRt = unsafe extern "C" fn(*mut std::ffi::c_void) -> *const hl::hl_runtime_obj;

    let min_constants = old_bc.constants.len().min(new_bc.constants.len());

    for ci in 0..min_constants {
        let old_c = &old_bc.constants[ci];
        let new_c = &new_bc.constants[ci];

        let global_idx = old_c.global as usize;
        if global_idx >= old_bc.globals.len() || global_idx >= new_bc.globals.len() {
            continue;
        }

        let type_idx = old_bc.globals[global_idx].0;
        if type_idx >= shared.c_types.len() {
            continue;
        }
        let c_type = shared.c_types[type_idx];
        if c_type.is_null() {
            continue;
        }
        let kind = unsafe { (*c_type).kind };
        if kind != hl::hl_type_kind_HOBJ && kind != hl::hl_type_kind_HSTRUCT {
            continue;
        }

        // Get runtime object for field offsets
        let rt = unsafe { std::mem::transmute::<_, FnGetRt>(get_rt_fn)(c_type as *mut _) };
        if rt.is_null() {
            continue;
        }

        let obj_data = match old_bc.types[type_idx].obj.as_ref() {
            Some(o) => o,
            None => continue,
        };

        let min_fields = old_c.fields.len().min(new_c.fields.len());
        let start = unsafe { (*rt).nfields as usize - obj_data.fields.len() };

        for fi in 0..min_fields {
            if old_c.fields[fi] == new_c.fields[fi] {
                continue; // Field value unchanged
            }

            let field_type_idx = obj_data.fields[fi].type_.0;
            let field_kind = old_bc.types[field_type_idx].kind;
            let field_offset = unsafe { *(*rt).fields_indexes.add(fi + start) } as usize;
            let field_size = field_byte_size(field_kind);

            if field_size == 0 {
                continue; // Unknown/unsupported field type
            }

            // Encode old and new values as raw bytes
            let old_val = encode_constant_value(old_c.fields[fi], field_kind, old_bc);
            let new_val = encode_constant_value(new_c.fields[fi], field_kind, new_bc);

            if old_val == new_val {
                continue;
            }

            patches.push(FieldPatch {
                type_ptr: c_type,
                offset: field_offset,
                size: field_size,
                old_bytes: old_val,
                new_bytes: new_val,
            });
        }
    }

    patches
}

fn field_byte_size(kind: u32) -> usize {
    match kind {
        hl::hl_type_kind_HBOOL | hl::hl_type_kind_HUI8 => 1,
        hl::hl_type_kind_HUI16 => 2,
        hl::hl_type_kind_HI32 => 4,
        hl::hl_type_kind_HF32 => 4,
        hl::hl_type_kind_HF64 | hl::hl_type_kind_HI64 => 8,
        _ => 8, // Pointer types
    }
}

fn encode_constant_value(field_value: i32, kind: u32, bc: &DecodedBytecode) -> Vec<u8> {
    match kind {
        hl::hl_type_kind_HI32 | hl::hl_type_kind_HBOOL | hl::hl_type_kind_HUI8
        | hl::hl_type_kind_HUI16 => {
            // field_value is index into ints table
            let val = bc.ints.get(field_value as usize).copied().unwrap_or(field_value);
            val.to_le_bytes().to_vec()
        }
        hl::hl_type_kind_HF64 => {
            let val = bc.floats.get(field_value as usize).copied().unwrap_or(0.0);
            val.to_le_bytes().to_vec()
        }
        hl::hl_type_kind_HBYTES => {
            // field_value is string index — encode the string content as an identifier
            let s = bc.strings.get(field_value as usize).cloned().unwrap_or_default();
            s.as_bytes().to_vec()
        }
        _ => {
            // For pointer types, encode the raw field_value (global index or findex)
            (field_value as u64).to_le_bytes().to_vec()
        }
    }
}

/// Apply field patches to all live heap objects.
///
/// For each patch, walks the GC heap and updates fields of matching objects
/// using compare-and-swap semantics: only update if the field still holds
/// the old default value (preserving runtime modifications).
pub fn apply_field_patches(patches: &[FieldPatch]) {
    if patches.is_empty() {
        return;
    }

    let resolver = crate::native_lib::NativeFunctionResolver::new();
    let walk_fn = match resolver.resolve_function("std", "hlp_gc_walk_heap") {
        Ok(f) => f,
        Err(_) => return,
    };
    type FnWalk = unsafe extern "C" fn(
        unsafe extern "C" fn(*mut hl::vdynamic, *mut hl::hl_type, *mut std::ffi::c_void),
        *mut std::ffi::c_void,
    );

    let walk: FnWalk = unsafe { std::mem::transmute(walk_fn) };

    // Pack (ptr, len) into a struct so we can recover the slice in the visitor
    struct PatchCtx {
        ptr: *const FieldPatch,
        len: usize,
    }
    let pctx = PatchCtx {
        ptr: patches.as_ptr(),
        len: patches.len(),
    };
    let ctx = &pctx as *const PatchCtx as *mut std::ffi::c_void;
    unsafe { walk(heap_patch_visitor, ctx) };
}

/// Visitor callback for hlp_gc_walk_heap. Applies matching patches to each object.
unsafe extern "C" fn heap_patch_visitor(
    obj: *mut hl::vdynamic,
    t: *mut hl::hl_type,
    ctx: *mut std::ffi::c_void,
) {
    struct PatchCtx {
        ptr: *const FieldPatch,
        len: usize,
    }
    let pctx = &*(ctx as *const PatchCtx);
    let patches = std::slice::from_raw_parts(pctx.ptr, pctx.len);

    for patch in patches {
        if patch.type_ptr != t {
            continue;
        }

        let field_ptr = (obj as *mut u8).add(patch.offset);
        let current = std::slice::from_raw_parts(field_ptr, patch.size);

        // Compare-and-swap: only update if field still holds old default
        if current == patch.old_bytes.as_slice() {
            std::ptr::copy_nonoverlapping(
                patch.new_bytes.as_ptr(),
                field_ptr,
                patch.size,
            );
        }
    }
}

// ---------------------------------------------------------------------------
// Global reload context — bridges the stdlib callback to perform_reload
// ---------------------------------------------------------------------------

struct ReloadContext {
    bytecode_path: PathBuf,
    old_bytecode: DecodedBytecode,
    functions_ptrs: Vec<*mut std::ffi::c_void>,
    shared_runtime: SharedRuntimeHandles,
}

// ReloadContext holds raw pointers from SharedRuntimeHandles.
unsafe impl Send for ReloadContext {}

static RELOAD_CTX: Mutex<Option<ReloadContext>> = Mutex::new(None);

/// Initialize the global reload context. Called once during runtime startup
/// when `--hot-reload` is active.
pub fn init_reload_context(
    bytecode_path: PathBuf,
    old_bytecode: DecodedBytecode,
    functions_ptrs: Vec<*mut std::ffi::c_void>,
    shared_runtime: SharedRuntimeHandles,
) {
    *RELOAD_CTX.lock().unwrap() = Some(ReloadContext {
        bytecode_path,
        old_bytecode,
        functions_ptrs,
        shared_runtime,
    });
}

/// Atomic flag set by the stdlib callback when a file change is detected.
/// The interpreter polls this after native calls and triggers `do_reload()`.
static RELOAD_PENDING: std::sync::atomic::AtomicBool =
    std::sync::atomic::AtomicBool::new(false);

/// Callback invoked by the stdlib when a bytecode file change is detected.
/// Sets the pending flag — actual recompilation is deferred to the interpreter
/// loop to avoid doing heavy work inside a native call stack.
pub unsafe extern "C" fn reload_callback(_path_utf16: *const u16) -> bool {
    RELOAD_PENDING.store(true, std::sync::atomic::Ordering::Release);
    true
}

/// Check and clear the pending reload flag. Called by the interpreter after
/// returning from native calls.
pub fn take_reload_pending() -> bool {
    RELOAD_PENDING.swap(false, std::sync::atomic::Ordering::AcqRel)
}

/// Execute the deferred reload and return the new bytecode (for interpreter update).
/// Called by the interpreter when `take_reload_pending` returns true.
/// This runs on the interpreter's thread, outside any native call stack.
pub fn do_reload() -> Option<DecodedBytecode> {
    let mut guard = match RELOAD_CTX.lock() {
        Ok(g) => g,
        Err(_) => return None,
    };
    let ctx = match guard.as_mut() {
        Some(c) => c,
        None => return None,
    };

    match perform_reload(
        &ctx.bytecode_path,
        &ctx.old_bytecode,
        &mut ctx.functions_ptrs,
        &ctx.shared_runtime,
    ) {
        Ok(diff) => {
            if diff.has_changes() {
                eprintln!(
                    "[hot-reload] reloaded {} changed function(s)",
                    diff.changed.len()
                );
                // Re-decode for both the stored state and the caller
                if let Ok(new_bc) =
                    crate::bytecode::BytecodeDecoder::decode(&ctx.bytecode_path)
                {
                    let ret = new_bc.clone();
                    ctx.old_bytecode = new_bc;
                    return Some(ret);
                }
            }
            None
        }
        Err(e) => {
            eprintln!("[hot-reload] reload failed: {}", e);
            None
        }
    }
}
