//! What the top tier knows about a function whose running frame left through
//! a re-tier exit.
//!
//! The promote that would follow is postponed for a function entered once,
//! because nothing is going to call it again. When something does, the
//! ledger is how that call finds the promote it is owed. It sees the call
//! when the interpreter enters the function a second time, when compiled code
//! enters it through the stub bridge, and when its hand-off frame returns
//! into a compiled caller. Compiled callers reach it after that without
//! entering the interpreter at all.
use super::{TieredSharedCtx, tiered_compile_tier};
use beadie::{TieredAdapter, TieredBound};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

#[derive(Clone, Copy, PartialEq, Eq)]
enum State {
    /// The interpreter has entered the function a second time.
    Reentered,
    /// The promote was postponed and no second entry has happened yet.
    Owed,
}

#[derive(Default)]
pub(crate) struct HandoffLedger {
    states: Mutex<HashMap<usize, State>>,
}

impl HandoffLedger {
    /// Records the promote of `findex` as postponed. Returns false when the
    /// function has already been entered a second time, which means the
    /// promote must go ahead. One lock covers the check and the record, so a
    /// second entry cannot slip between them and find nothing owed.
    pub(crate) fn postpone(&self, findex: usize) -> bool {
        let mut states = self.states.lock().expect("handoff mutex poisoned");
        match states.get(&findex) {
            Some(State::Reentered) => false,
            _ => {
                states.insert(findex, State::Owed);
                true
            }
        }
    }

    /// `findex` was entered again, or will be. Returns true when a postponed
    /// promote is owed to it; the debt is cleared.
    pub(crate) fn note_reentry(&self, findex: usize) -> bool {
        let mut states = self.states.lock().expect("handoff mutex poisoned");
        states.insert(findex, State::Reentered) == Some(State::Owed)
    }

    pub(crate) fn clear(&self) {
        self.states.lock().expect("handoff mutex poisoned").clear();
    }
}

/// Called when `findex` is entered again. Queues the top-tier promote that
/// the function's hand-off postponed, if one is owed. Returns whether a
/// promote was queued.
pub(crate) fn pay_owed_promote(
    ctx: &Arc<TieredSharedCtx>,
    adapter: &TieredAdapter,
    bound: &TieredBound,
    findex: usize,
    calls: u32,
) -> bool {
    if !ctx.handoff.note_reentry(findex) {
        return false;
    }
    // The postponing job lowers the bead's queued flag as it returns, a
    // moment after it records the debt; a submit in that gap is refused, so
    // it is retried briefly.
    for _ in 0..256 {
        let job_ctx = Arc::clone(ctx);
        if adapter.force_promote(bound, 1, move |b| {
            tiered_compile_tier(&job_ctx, 1, findex, b, true, calls)
        }) {
            if ctx.tier_log {
                eprintln!("[tier] reentry proposes findex={findex} tier=llvm");
            }
            return true;
        }
        std::hint::spin_loop();
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_second_entry_after_the_postponement_is_owed_the_promote() {
        let ledger = HandoffLedger::default();
        assert!(ledger.postpone(7));
        assert!(ledger.note_reentry(7));
        assert!(!ledger.note_reentry(7));
    }

    #[test]
    fn a_second_entry_before_the_postponement_refuses_it() {
        let ledger = HandoffLedger::default();
        assert!(!ledger.note_reentry(7));
        assert!(!ledger.postpone(7));
    }

    #[test]
    fn functions_are_independent() {
        let ledger = HandoffLedger::default();
        assert!(ledger.postpone(1));
        assert!(!ledger.note_reentry(2));
        assert!(ledger.note_reentry(1));
    }
}
