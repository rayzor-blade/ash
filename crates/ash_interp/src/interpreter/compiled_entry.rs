//! Entries into the interpreter from compiled code.
//!
//! A child module of `interpreter` so it reaches `HLInterpreter`'s private
//! fields without widening them.

use std::sync::Arc;

use ash_core::bytecode::DecodedBytecode;

use super::{DEMAND_FROM_COMPILED, HLInterpreter};
use crate::tiering::TierMode;

impl HLInterpreter {
    /// The stub bridge enters a function that a compiled caller reached. That
    /// caller was compiled because it ran often, so the function will be
    /// entered again, and compiled callers tick nothing when it is. The
    /// hand-off ledger takes this as its re-entry: a promote the OSR hand-off
    /// postponed is paid, and one not yet decided is not postponed.
    pub(super) fn note_entered_from_compiled(&mut self, findex: usize) {
        if self.demand_seen(findex, DEMAND_FROM_COMPILED) {
            return;
        }
        if findex >= self.demand_local.len() {
            self.demand_local.resize(findex + 1, 0);
        }
        self.demand_local[findex] |= DEMAND_FROM_COMPILED;
        let Some(t) = self.tiered_runtime.as_ref() else {
            return;
        };
        if t.config.tier_mode != TierMode::Auto || t.config.compiled_only {
            return;
        }
        let ctx = Arc::clone(&t.shared_ctx);
        let calls = t.calls.get(findex).copied().unwrap_or(0);
        match t.beads.get(findex).and_then(|b| b.as_ref()) {
            Some(bound) => {
                crate::tiering::pay_owed_promote(&ctx, &t.adapter, bound, findex, calls);
            }
            None => {
                ctx.handoff.note_reentry(findex);
            }
        }
    }

    /// A frame that took its OSR hand-off has finished. If the function it
    /// returns into is compiled, that function's next activations call this
    /// one without entering the interpreter, so a promote postponed for it
    /// is paid now, and one not yet decided is not postponed.
    pub(super) fn settle_hand_off(&mut self, bytecode: &DecodedBytecode, findex: usize) {
        let Some(caller) = self
            .stack
            .len()
            .checked_sub(2)
            .and_then(|i| self.stack.get(i))
            .map(|f| f.function_index)
        else {
            return;
        };
        let caller_findex = self.bytecode_findex_of(bytecode, caller);
        let Some(t) = self.tiered_runtime.as_ref() else {
            return;
        };
        if t.config.tier_mode != TierMode::Auto || t.config.compiled_only {
            return;
        }
        let compiled = |f: usize| {
            t.beads
                .get(f)
                .and_then(|b| b.as_ref())
                .is_some_and(|b| b.bead().compiled().is_some())
        };
        if !compiled(caller_findex) {
            return;
        }
        let Some(bound) = t.beads.get(findex).and_then(|b| b.as_ref()) else {
            return;
        };
        let ctx = Arc::clone(&t.shared_ctx);
        let calls = t.calls.get(findex).copied().unwrap_or(0);
        crate::tiering::pay_owed_promote(&ctx, &t.adapter, bound, findex, calls);
    }
}
