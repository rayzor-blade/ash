//! The GC's write barrier, inlined for the interpreter's stores.
//!
//! The runtime's own `hlp_gc_write_barrier` marks the card of a heap address
//! the program stored a pointer into. The interpreter stores through raw
//! pointers on every field, array and reference write, so it marks the card
//! itself with the parameters `ash_core::card_table` reports.

use std::sync::atomic::{AtomicU8, AtomicUsize, Ordering};

const CARD_SHIFT: usize = ash_core::card_table::CARD_SHIFT as usize;

static BIAS: AtomicUsize = AtomicUsize::new(0);
static BASE: AtomicUsize = AtomicUsize::new(0);
/// The heap's length once known, or `OFF` when card mode is off.
static LEN: AtomicUsize = AtomicUsize::new(0);
const OFF: usize = usize::MAX;

/// Record a pointer store into the word at `addr`. Outside the heap, or with
/// card mode off, it does nothing.
#[inline(always)]
pub(crate) fn write_barrier(addr: *mut u8) {
    let mut len = LEN.load(Ordering::Relaxed);
    if len == 0 {
        len = init();
    }
    let addr = addr as usize;
    if len != OFF && addr.wrapping_sub(BASE.load(Ordering::Relaxed)) < len {
        let card = BIAS
            .load(Ordering::Relaxed)
            .wrapping_add(addr >> CARD_SHIFT);
        unsafe { (*(card as *const AtomicU8)).store(1, Ordering::Relaxed) };
    }
}

#[cold]
fn init() -> usize {
    let Some(t) = ash_core::card_table::card_table() else {
        LEN.store(OFF, Ordering::Relaxed);
        return OFF;
    };
    BIAS.store(t.bias, Ordering::Relaxed);
    BASE.store(t.base, Ordering::Relaxed);
    LEN.store(t.len, Ordering::Release);
    t.len
}
