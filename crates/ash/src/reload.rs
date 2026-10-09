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
/// vtables built from the old table would call the wrong bodies. The
/// globals are already on the running program's slots, by `remap_globals`.
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

/// What a global is, wherever a program put it. Adding or removing a
/// string literal or a static shifts every global after it, so a global
/// is matched across a reload by this rather than by its index.
#[derive(Clone, PartialEq, Eq, Hash)]
enum GlobalKey {
    /// A class's or an enum's descriptor, by its type.
    Descriptor(String),
    /// A constant, by its type and contents; the last field tells apart
    /// constants that are otherwise the same.
    Constant(String, Vec<String>, usize),
    /// Anything else, by its type and its order among those of that type.
    Other(String, usize),
}

/// A type's name across two programs, whose type tables may differ.
fn type_key(bc: &DecodedBytecode, t: usize) -> String {
    let ty = &bc.types[t];
    let name = ty
        .obj
        .as_ref()
        .map(|o| o.name.clone())
        .or_else(|| ty.tenum.as_ref().map(|e| e.name.clone()))
        .or_else(|| ty.abs_name.clone())
        .filter(|n| !n.is_empty())
        .unwrap_or_else(|| format!("#{t}"));
    format!("{}:{name}", ty.kind)
}

/// Whether a constant's field of this kind holds a global's index, as
/// `init_constants` reads it; `nullable` fields hold 0 for null.
fn field_names_global(kind: hl::hl_type_kind) -> Option<bool> {
    match kind {
        hl::hl_type_kind_HOBJ | hl::hl_type_kind_HSTRUCT => Some(false),
        hl::hl_type_kind_HFUN
        | hl::hl_type_kind_HMETHOD
        | hl::hl_type_kind_HBYTES
        | hl::hl_type_kind_HTYPE
        | hl::hl_type_kind_HI32
        | hl::hl_type_kind_HBOOL
        | hl::hl_type_kind_HUI8
        | hl::hl_type_kind_HUI16
        | hl::hl_type_kind_HI64
        | hl::hl_type_kind_HF64
        | hl::hl_type_kind_HF32 => None,
        _ => Some(true),
    }
}

/// The fields of the constant `c` with what each field's value means:
/// a string's text, a number, a type, or another global's index.
fn constant_fields<'a>(
    bc: &'a DecodedBytecode,
    c: &'a crate::types::HLConstant,
) -> impl Iterator<Item = (usize, hl::hl_type_kind)> + 'a {
    let fields = bc.types[bc.globals[c.global as usize].0]
        .obj
        .as_ref()
        .map(|o| o.fields.as_slice())
        .unwrap_or_default();
    (0..c.fields.len().min(fields.len())).map(move |j| (j, bc.types[fields[j].type_.0].kind))
}

/// Every global's key, by index.
fn global_keys(bc: &DecodedBytecode) -> Vec<GlobalKey> {
    let mut keys: Vec<Option<GlobalKey>> = vec![None; bc.globals.len()];
    for (t, ty) in bc.types.iter().enumerate() {
        let gv = ty
            .obj
            .as_ref()
            .map(|o| o.global_value)
            .or_else(|| ty.tenum.as_ref().map(|e| e.global_value))
            .unwrap_or(0) as usize;
        if gv > 0 && gv <= keys.len() {
            keys[gv - 1] = Some(GlobalKey::Descriptor(type_key(bc, t)));
        }
    }
    // A constant's field that names another global names it by that
    // global's type: its index is what moves.
    let mut seen: HashMap<(String, Vec<String>), usize> = HashMap::new();
    for c in &bc.constants {
        let g = c.global as usize;
        if g >= keys.len() || keys[g].is_some() {
            continue;
        }
        let contents: Vec<String> = constant_fields(bc, c)
            .map(|(j, kind)| {
                let v = c.fields[j];
                match (kind, field_names_global(kind)) {
                    (_, Some(nullable)) if !(nullable && v == 0) => bc
                        .globals
                        .get(v as usize)
                        .map_or_else(|| format!("g?{v}"), |t| format!("g:{}", type_key(bc, t.0))),
                    (hl::hl_type_kind_HBYTES, _) => {
                        format!(
                            "s:{}",
                            bc.strings.get(v as usize).map_or("", |s| s.as_str())
                        )
                    }
                    (hl::hl_type_kind_HF64 | hl::hl_type_kind_HF32, _) => {
                        format!("f:{}", bc.floats.get(v as usize).map_or(0, |f| f.to_bits()))
                    }
                    (hl::hl_type_kind_HTYPE, _) => format!("t:{}", type_key(bc, v as usize)),
                    (hl::hl_type_kind_HFUN | hl::hl_type_kind_HMETHOD, _) => format!("fn:{v}"),
                    _ => format!("i:{}", bc.ints.get(v as usize).copied().unwrap_or(v)),
                }
            })
            .collect();
        let ty = type_key(bc, bc.globals[g].0);
        let n = seen.entry((ty.clone(), contents.clone())).or_default();
        keys[g] = Some(GlobalKey::Constant(ty, contents, *n));
        *n += 1;
    }
    let mut seen: HashMap<String, usize> = HashMap::new();
    keys.into_iter()
        .enumerate()
        .map(|(g, k)| {
            k.unwrap_or_else(|| {
                let ty = type_key(bc, bc.globals[g].0);
                let n = seen.entry(ty.clone()).or_default();
                *n += 1;
                GlobalKey::Other(ty, *n - 1)
            })
        })
        .collect()
}

/// Renumber `new`'s globals onto the slots of the running program `old`.
///
/// A global keeps the slot that holds the same thing in `old`, so the
/// value the running program keeps there (a class's descriptor and its
/// statics) is the one the new code reads. A constant `old` does not have
/// takes the slot of one of the same type that `new` dropped; the reload
/// writes every constant into its slot again, so a frame still running the
/// old body reads the new constant there until it returns. Anything else
/// `old` does not have cannot be placed, and `Err` says which.
fn remap_globals(old: &DecodedBytecode, new: &mut DecodedBytecode) -> Result<(), String> {
    let (old_keys, new_keys) = (global_keys(old), global_keys(new));
    if old_keys == new_keys {
        return Ok(());
    }
    let slot_of: HashMap<&GlobalKey, usize> =
        old_keys.iter().enumerate().map(|(g, k)| (k, g)).collect();
    let mut taken = vec![false; old_keys.len()];
    let mut map: Vec<Option<usize>> = new_keys.iter().map(|k| slot_of.get(k).copied()).collect();
    for &slot in map.iter().flatten() {
        taken[slot] = true;
    }
    for g in 0..map.len() {
        if map[g].is_some() {
            continue;
        }
        let GlobalKey::Constant(ty, ..) = &new_keys[g] else {
            let what = match &new_keys[g] {
                GlobalKey::Descriptor(t) => format!("the descriptor of {t}"),
                GlobalKey::Other(t, n) => format!("a {t} global (#{n} of its type)"),
                GlobalKey::Constant(..) => unreachable!(),
            };
            return Err(format!(
                "the new program has {what}, which the running one does not; a class or a static was added"
            ));
        };
        let free = (0..old_keys.len())
            .find(|&s| !taken[s] && matches!(&old_keys[s], GlobalKey::Constant(t, ..) if t == ty));
        let Some(slot) = free else {
            return Err(format!(
                "the new program has more {ty} constants than the running one has slots for"
            ));
        };
        taken[slot] = true;
        map[g] = Some(slot);
    }
    let map: Vec<usize> = map
        .into_iter()
        .map(|s| s.expect("every global placed"))
        .collect();

    let mut globals = old.globals.clone();
    for (g, &slot) in map.iter().enumerate() {
        globals[slot] = new.globals[g].clone();
    }
    let at = |g: usize| map.get(g).copied().unwrap_or(g);
    // A constant's nullable field reads 0 as null, so no global it names
    // may land on slot 0.
    let mut constants = std::mem::take(&mut new.constants);
    for c in &mut constants {
        let kinds: Vec<(usize, hl::hl_type_kind)> = constant_fields(new, c).collect();
        for (j, kind) in kinds {
            let Some(nullable) = field_names_global(kind) else {
                continue;
            };
            let v = c.fields[j];
            if nullable && v == 0 {
                continue;
            }
            let slot = at(v as usize);
            if nullable && slot == 0 {
                return Err("a constant would name global 0, which reads as null".into());
            }
            c.fields[j] = slot as i32;
        }
        c.global = at(c.global as usize) as u32;
    }
    new.constants = constants;
    let rebase = |gv: &mut u32| {
        if *gv > 0 {
            *gv = at(*gv as usize - 1) as u32 + 1;
        }
    };
    for ty in &mut new.types {
        if let Some(o) = ty.obj.as_mut() {
            rebase(&mut o.global_value);
        }
        if let Some(e) = ty.tenum.as_mut() {
            rebase(&mut e.global_value);
        }
    }
    fn rewrite(
        f: &mut crate::types::HLFunction,
        at: &dyn Fn(usize) -> usize,
        rebase: &dyn Fn(&mut u32),
    ) {
        for op in f.ops_mut() {
            match op {
                crate::opcodes::Opcode::GetGlobal { global, .. }
                | crate::opcodes::Opcode::SetGlobal { global, .. } => global.0 = at(global.0),
                _ => {}
            }
        }
        if let Some(o) = f.obj.as_mut() {
            rebase(&mut o.global_value);
        }
        if let Some(r) = f.field_ref.as_mut() {
            rewrite(r, at, rebase);
        }
    }
    for f in &mut new.functions {
        rewrite(f, &at, &rebase);
    }
    new.globals = globals;
    Ok(())
}

/// The program at `path`, read as the next version of `running`: with
/// the host registrations `running` took and its globals on `running`'s
/// slots. `Err` says why it cannot be, or that it did not read.
fn decode_next(
    path: &std::path::Path,
    running: &DecodedBytecode,
) -> Result<DecodedBytecode, String> {
    let mut bc = decode_as(path, running).map_err(|e| format!("{}: {e}", path.display()))?;
    remap_globals(running, &mut bc)
        .map_err(|why| format!("the program cannot reload in place: {why}"))?;
    Ok(bc)
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
    let new_bytecode = decode_next(&ctx.bytecode_path, &ctx.old_bytecode)?;
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
        None => decode_next(&ctx.bytecode_path, &ctx.old_bytecode).map_err(anyhow::Error::msg),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::opcodes::{Opcode, RefGlobal, Reg};
    use crate::types::{HLConstant, HLFunction, HLObjField, HLType, HLTypeObj, TypeRef};

    const I32: usize = 0;
    const BYTES: usize = 1;
    const STRING: usize = 2;
    const MAIN: usize = 3;
    const MAIN_STATICS: usize = 4;

    fn obj(name: &str, fields: &[(&str, usize)], global_value: u32) -> HLType {
        HLType {
            kind: hl::hl_type_kind_HOBJ,
            obj: Some(HLTypeObj {
                name: name.into(),
                fields: fields
                    .iter()
                    .map(|(n, t)| HLObjField {
                        name: (*n).into(),
                        type_: TypeRef(*t),
                        hashed_name: 0,
                    })
                    .collect(),
                global_value,
                ..Default::default()
            }),
            ..Default::default()
        }
    }

    /// A program whose globals are its string literals, in order, around
    /// `Main`'s descriptor at `main_at`, and one function reading `Main`'s
    /// descriptor and then each literal.
    fn program(literals: &[&str], main_at: usize) -> DecodedBytecode {
        let mut bc = DecodedBytecode {
            strings: literals.iter().map(|s| s.to_string()).collect(),
            ints: literals.iter().map(|s| s.len() as i32).collect(),
            ..Default::default()
        };
        bc.types = vec![
            HLType {
                kind: hl::hl_type_kind_HI32,
                ..Default::default()
            },
            HLType {
                kind: hl::hl_type_kind_HBYTES,
                ..Default::default()
            },
            obj("String", &[("bytes", BYTES), ("length", I32)], 0),
            obj("Main", &[], main_at as u32 + 1),
            obj("$Main", &[("counter", I32)], 0),
        ];
        let mut ops = vec![Opcode::GetGlobal {
            dst: Reg(0),
            global: RefGlobal(main_at),
        }];
        let mut next = 0;
        for g in 0..=literals.len() {
            if g == main_at {
                bc.globals.push(TypeRef(MAIN_STATICS));
                continue;
            }
            bc.globals.push(TypeRef(STRING));
            bc.constants.push(HLConstant {
                global: g as u32,
                fields: vec![next, next],
            });
            ops.push(Opcode::GetGlobal {
                dst: Reg(0),
                global: RefGlobal(g),
            });
            next += 1;
        }
        let mut function = HLFunction::default();
        function.set_ops(ops);
        bc.functions = vec![function];
        bc
    }

    fn literal_at(bc: &DecodedBytecode, g: usize) -> Option<&str> {
        let c = bc.constants.iter().find(|c| c.global as usize == g)?;
        Some(bc.strings[c.fields[0] as usize].as_str())
    }

    fn reads(bc: &DecodedBytecode) -> Vec<usize> {
        bc.functions[0]
            .ops()
            .iter()
            .map(|op| match op {
                Opcode::GetGlobal { global, .. } => global.0,
                _ => unreachable!(),
            })
            .collect()
    }

    /// A new literal ahead of the descriptor shifts it; every global goes
    /// back to the slot that held it, and the new literal takes the slot
    /// of the one dropped.
    #[test]
    fn shifted_globals_go_back_to_their_slots() {
        let old = program(&["same", "gone", "end"], 2);
        let mut new = program(&["fresh", "same", "gone"], 3);
        remap_globals(&old, &mut new).expect("remaps");

        assert_eq!(new.globals.len(), old.globals.len());
        assert_eq!(new.types[MAIN].obj.as_ref().unwrap().global_value, 3);
        assert_eq!(new.globals[2].0, MAIN_STATICS);
        assert_eq!(literal_at(&new, 0), Some("same"));
        assert_eq!(literal_at(&new, 1), Some("gone"));
        assert_eq!(literal_at(&new, 3), Some("fresh"));
        // Main's descriptor, then "fresh", "same", "gone" as the new code
        // reads them.
        assert_eq!(reads(&new), vec![2, 3, 0, 1]);
    }

    #[test]
    fn unchanged_globals_stay() {
        let old = program(&["a", "b"], 1);
        let mut new = program(&["a", "b"], 1);
        remap_globals(&old, &mut new).expect("remaps");
        assert_eq!(reads(&new), reads(&old));
    }

    /// A literal with no dropped one to replace has no slot.
    #[test]
    fn a_literal_too_many_is_refused() {
        let old = program(&["a"], 1);
        let mut new = program(&["a", "b"], 2);
        let why = remap_globals(&old, &mut new).unwrap_err();
        assert!(why.contains("String constants"), "{why}");
    }
}
