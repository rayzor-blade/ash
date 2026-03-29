//! Hot-reload infrastructure: bytecode diffing and reload coordination.
//!
//! Compares old and new `DecodedBytecode` to determine which functions changed,
//! which were added/removed, and whether type layouts are compatible.

use crate::bytecode::DecodedBytecode;
use crate::hl;
use std::collections::{HashMap, HashSet};

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
