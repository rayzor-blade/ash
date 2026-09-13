//! Rapid type analysis over a whole program: which classes are ever
//! allocated, and which implementations a vtable slot can reach through
//! them.
//!
//! Sound only for a closed program, which an ahead-of-time build is. The
//! answer is advisory: a caller guards the direct call it makes with it, so
//! an object a native allocated outside the analysis costs a miss, never a
//! wrong call.

use crate::bytecode::DecodedBytecode;
use crate::types::{HLType, TypeRef};
use air::opcodes::Opcode;

pub struct ReachableTargets {
    /// By type index: some `New` in the program allocates this class.
    instantiated: Vec<bool>,
    /// By type index: the classes whose `super` is this one.
    children: Vec<Vec<usize>>,
}

impl ReachableTargets {
    pub fn analyze(bc: &DecodedBytecode) -> Self {
        let n = bc.types.len();
        let mut instantiated = vec![false; n];
        for f in &bc.functions {
            for op in &f.ops {
                if let Opcode::New { dst } = op {
                    if let Some(&TypeRef(t)) = f.regs.get(dst.0 as usize) {
                        if t < n {
                            instantiated[t] = true;
                        }
                    }
                }
            }
        }
        let mut children = vec![Vec::new(); n];
        for (i, t) in bc.types.iter().enumerate() {
            if let Some(&TypeRef(s)) = t.obj.as_ref().and_then(|o| o.super_.as_ref()) {
                if s < n {
                    children[s].push(i);
                }
            }
        }
        Self {
            instantiated,
            children,
        }
    }

    /// The distinct functions `slot` resolves to over the allocated classes
    /// in `type_idx`'s subtree (itself included), in first-seen order.
    /// Empty when no class in the subtree is allocated.
    pub fn slot_targets(&self, types: &[HLType], type_idx: usize, slot: usize) -> Vec<usize> {
        let mut out = Vec::new();
        let mut stack = vec![type_idx];
        while let Some(t) = stack.pop() {
            if self.instantiated.get(t).copied().unwrap_or(false) {
                if let Some(f) = proto_findex_for_slot(types, t, slot) {
                    if !out.contains(&f) {
                        out.push(f);
                    }
                }
            }
            if let Some(kids) = self.children.get(t) {
                stack.extend(kids.iter().copied());
            }
        }
        out
    }
}

/// The function behind vtable slot `slot` of class `type_idx`, found by
/// walking the super chain for the proto entry with that pindex.
pub fn proto_findex_for_slot(types: &[HLType], type_idx: usize, slot: usize) -> Option<usize> {
    let mut cur = Some(type_idx);
    while let Some(ti) = cur {
        let obj = types.get(ti)?.obj.as_ref()?;
        if let Some(p) = obj.proto.iter().find(|p| p.pindex as usize == slot) {
            return Some(p.findex as usize);
        }
        cur = obj.super_.as_ref().map(|t| t.0);
    }
    None
}
