//! Hot-reload infrastructure: bytecode diffing and reload coordination.
//!
//! Compares old and new `DecodedBytecode` to determine which functions changed,
//! which were added/removed, and whether type layouts are compatible.

use crate::bytecode::DecodedBytecode;
use crate::hl;
use crate::runtime_handles::SharedRuntimeHandles;
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::{LazyLock, Mutex};

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

// Kind params carry the bindgen alias, not a bare integer: MSVC types the C
// enum i32 where clang types it u32, so only the alias compiles on both.
fn is_obj_kind(kind: hl::hl_type_kind) -> bool {
    kind == hl::hl_type_kind_HOBJ || kind == hl::hl_type_kind_HSTRUCT
}

/// Collect the set of native function findexes from bytecode.
/// Natives are not reloadable, so calls to them stay direct.
pub fn native_findexes(bytecode: &DecodedBytecode) -> HashSet<usize> {
    bytecode.natives.iter().map(|n| n.findex as usize).collect()
}

/// Flush vtable protos for all HOBJ/HSTRUCT types that might reference changed functions.
fn flush_affected_protos(shared: &SharedRuntimeHandles, _changed_findexes: &[usize]) {
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
// Global reload context — bridges the stdlib callback to do_reload
// ---------------------------------------------------------------------------

struct ReloadContext {
    bytecode_path: PathBuf,
    old_bytecode: DecodedBytecode,
    functions_ptrs: Vec<*mut std::ffi::c_void>,
    shared_runtime: SharedRuntimeHandles,
    /// The program `stage_reload` accepted, for `do_reload` to apply.
    staged: Option<DecodedBytecode>,
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
        staged: None,
    });
}

/// The program at `path`, given every host registration `like` took: a
/// reload must not drop the natives and classes a host added to the program
/// it replaces.
fn decode_as(path: &std::path::Path, like: &DecodedBytecode) -> anyhow::Result<DecodedBytecode> {
    let mut bc = crate::bytecode::BytecodeDecoder::decode(path)?;
    bc.register_host_modules_of(&like.host_modules)?;
    Ok(bc)
}

/// The name of every function the type table names, by findex:
/// `Type.proto` for a method, `Type.field` for a bound one.
fn function_names(bc: &DecodedBytecode) -> HashMap<usize, String> {
    let mut names = HashMap::new();
    for t in &bc.types {
        let Some(obj) = &t.obj else { continue };
        for p in &obj.proto {
            names.insert(p.findex as usize, format!("{}.{}", obj.name, p.name));
        }
        // A binding names a field by its index over the whole chain.
        let mut chain = Vec::new();
        let mut cur = Some(t);
        while let Some(ct) = cur {
            chain.push(ct);
            cur = ct
                .obj
                .as_ref()
                .and_then(|o| o.super_.clone())
                .and_then(|r| bc.types.get(r.0));
        }
        let fields: Vec<&str> = chain
            .iter()
            .rev()
            .flat_map(|ct| ct.obj.iter().flat_map(|o| o.fields.iter()))
            .map(|f| f.name.as_str())
            .collect();
        for pair in obj.bindings.chunks(2) {
            let [fid, findex] = pair else { continue };
            if let Some(field) = fields.get(*fid as usize) {
                names.insert(*findex as usize, format!("{}.{field}", obj.name));
            }
        }
    }
    names
}

/// Why `new` cannot replace `old` in place, if it cannot: compiled code
/// and live objects are laid out by the running program, so a type's
/// layout, the globals count and the function table's shape must stay.
/// A function table of a different shape is one where a findex names
/// another function now, which happens when a method is added or
/// removed; the diff sees every function after it as changed, and the
/// vtables built from the old table would call the wrong bodies.
fn refusal(old: &DecodedBytecode, new: &DecodedBytecode, diff: &ReloadDiff) -> Option<String> {
    if diff.type_layout_changed {
        return Some(
            "a type's field layout changed; live objects are laid out by the old one".into(),
        );
    }
    if diff.globals_count_changed {
        return Some(format!(
            "the globals count changed ({} -> {})",
            old.globals.len(),
            new.globals.len()
        ));
    }
    if !diff.added.is_empty() || !diff.removed.is_empty() {
        return Some(format!(
            "the function table changed shape ({} added, {} removed); a method was added or removed",
            diff.added.len(),
            diff.removed.len()
        ));
    }
    let (old_names, new_names) = (function_names(old), function_names(new));
    let moved = diff.changed.iter().find(|findex| {
        matches!((old_names.get(findex), new_names.get(findex)), (Some(a), Some(b)) if a != b)
    });
    if let Some(findex) = moved {
        return Some(format!(
            "functions moved: findex {findex} was {} and is {}; a method was added or removed",
            old_names[findex], new_names[findex]
        ));
    }
    None
}

/// Read the program at the registered path again and check it against
/// the running one. An accepted program is staged and the reload flagged:
/// the interpreter applies it on its own thread when it next returns from
/// a native call (`take_reload_pending`, then `do_reload`). `Ok` carries
/// the diff, which may be empty. `Err` says why the program cannot
/// replace the running one in place, or that it did not read.
pub fn stage_reload() -> Result<ReloadDiff, String> {
    let mut guard = RELOAD_CTX
        .lock()
        .map_err(|_| "the reload context is poisoned".to_string())?;
    let ctx = guard
        .as_mut()
        .ok_or_else(|| "reload is not enabled for this program".to_string())?;
    let new_bytecode = decode_as(&ctx.bytecode_path, &ctx.old_bytecode)
        .map_err(|e| format!("{}: {e}", ctx.bytecode_path.display()))?;
    let diff = diff_bytecode(&ctx.old_bytecode, &new_bytecode);
    if let Some(why) = refusal(&ctx.old_bytecode, &new_bytecode, &diff) {
        return Err(format!("the program cannot reload in place: {why}"));
    }
    if diff.has_changes() {
        ctx.staged = Some(new_bytecode);
        RELOAD_PENDING.store(true, std::sync::atomic::Ordering::Release);
    }
    Ok(diff)
}

/// The callees each compiled body bound to, by findex: copied in, or
/// called by address rather than through the callee's slot. What a reload
/// of a callee leaves stale besides the callee itself.
static INLINED: LazyLock<Mutex<HashMap<usize, Vec<usize>>>> = LazyLock::new(Default::default);

/// Record that the body compiled for `findex` bound `callees` into itself.
/// A tier calls this with the inline sites of the AIR it compiled from,
/// and with the calls it emits by address.
pub fn note_inlined(findex: usize, callees: impl Iterator<Item = usize>) {
    let mut callees: Vec<usize> = callees.collect();
    callees.sort_unstable();
    callees.dedup();
    let mut table = INLINED.lock().unwrap_or_else(|e| e.into_inner());
    if callees.is_empty() {
        table.remove(&findex);
    } else {
        table.insert(findex, callees);
    }
}

/// The compiled bodies that bound one of `changed`.
fn inline_dependents(changed: &[usize]) -> Vec<usize> {
    let table = INLINED.lock().unwrap_or_else(|e| e.into_inner());
    let mut out: Vec<usize> = table
        .iter()
        .filter(|(findex, callees)| {
            !changed.contains(findex) && callees.iter().any(|c| changed.contains(c))
        })
        .map(|(findex, _)| *findex)
        .collect();
    out.sort_unstable();
    out
}

/// The program a reload applies: the staged one, else the file read
/// again.
fn next_program(ctx: &mut ReloadContext) -> anyhow::Result<DecodedBytecode> {
    match ctx.staged.take() {
        Some(bc) => Ok(bc),
        None => decode_as(&ctx.bytecode_path, &ctx.old_bytecode),
    }
}

/// Atomic flag set by the stdlib callback when a file change is detected.
/// The interpreter polls this after native calls and triggers `do_reload()`.
static RELOAD_PENDING: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

/// Callback invoked by the stdlib when a bytecode file change is detected.
/// Checks and stages the new program; recompilation is deferred to the
/// interpreter loop to avoid doing heavy work inside a native call stack.
/// `false` when nothing will reload: the program is unchanged, unreadable,
/// or refused.
pub unsafe extern "C" fn reload_callback(_path_utf16: *const u16) -> bool {
    match stage_reload() {
        Ok(diff) => diff.has_changes(),
        Err(why) => {
            eprintln!("[hot-reload] reload refused: {why}");
            false
        }
    }
}

/// Check and clear the pending reload flag. Called by the interpreter after
/// every call returns, so the common answer is a plain load; the swap runs
/// only once a reload is pending.
#[inline]
pub fn take_reload_pending() -> bool {
    RELOAD_PENDING.load(std::sync::atomic::Ordering::Acquire)
        && RELOAD_PENDING.swap(false, std::sync::atomic::Ordering::AcqRel)
}

/// Apply the staged program and return it for the interpreter to take.
/// Runs on the interpreter's thread, outside any native call stack.
///
/// Nothing is compiled here. A changed body goes back to the interpreter:
/// its slot holds the stub sentinel again, so a compiled caller's guarded
/// call re-enters the interpreter, which runs the new body once it has
/// taken the program returned here; the tiers promote it again from the
/// new program by the usual hotness rules. A compiled body that inlined a
/// changed one goes back the same way. Other compiled bodies stay, and the
/// vtables are flushed so they read the slots again. Constants are not
/// patched in place: the interpreter allocates them again from the new
/// program and stores them in their globals.
pub fn do_reload() -> Option<DecodedBytecode> {
    let mut guard = RELOAD_CTX.lock().ok()?;
    let ctx = guard.as_mut()?;
    let new_bytecode = match next_program(ctx) {
        Ok(bc) => bc,
        Err(e) => {
            eprintln!("[hot-reload] reload failed: {e}");
            return None;
        }
    };
    let diff = diff_bytecode(&ctx.old_bytecode, &new_bytecode);
    if let Some(why) = refusal(&ctx.old_bytecode, &new_bytecode, &diff) {
        eprintln!("[hot-reload] reload failed: {why}");
        return None;
    }
    if !diff.has_changes() {
        return None;
    }
    let mut stale = inline_dependents(&diff.changed);
    stale.extend_from_slice(&diff.changed);
    // The optimized-AIR cache and the LLVM gate's ceilings are keyed by
    // findex, and the same findex has a different body now.
    crate::air_pipeline::invalidate_optimized();
    #[cfg(feature = "llvm")]
    crate::llvm::air::invalidate_ceilings();
    let live_ptrs = unsafe {
        if ctx.shared_runtime.module_ctx.is_null() {
            std::ptr::null_mut()
        } else {
            (*ctx.shared_runtime.module_ctx).functions_ptrs
        }
    };
    for &findex in &stale {
        let sentinel = (findex + 1) as *mut std::ffi::c_void;
        if findex < ctx.functions_ptrs.len() {
            ctx.functions_ptrs[findex] = sentinel;
            if !live_ptrs.is_null() {
                unsafe { *live_ptrs.add(findex) = sentinel };
            }
        }
    }
    flush_affected_protos(&ctx.shared_runtime, &stale);
    eprintln!(
        "[hot-reload] reloaded {} changed function(s)",
        diff.changed.len()
    );
    ctx.old_bytecode = new_bytecode.clone();
    Some(new_bytecode)
}
