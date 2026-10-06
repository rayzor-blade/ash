//! Where the GC's write-barrier cards are, for code that marks them inline.
//!
//! A store of a pointer into an existing heap object marks the card covering
//! the stored-to word: `*(bias + (addr >> CARD_SHIFT)) = 1` when `addr` lies in
//! `base..base + len`. The numbers come from the runtime copy the program
//! runs on (`hlp_gc_card_info`) and are fixed once its heap exists.

use std::sync::OnceLock;

/// Bytes of heap per card; must match `CARD_SHIFT` in `std/src/gc.rs`.
pub const CARD_SHIFT: u32 = 9;

/// The card table's parameters.
#[derive(Clone, Copy, Debug)]
pub struct CardTable {
    /// The table's address minus `base >> CARD_SHIFT`.
    pub bias: usize,
    pub base: usize,
    pub len: usize,
}

/// The running runtime's card table, or `None` when it exports none or card
/// mode is off -- then no barrier is needed, now or later.
pub fn card_table() -> Option<CardTable> {
    static INFO: OnceLock<Option<CardTable>> = OnceLock::new();
    *INFO.get_or_init(|| {
        type Info = unsafe extern "C" fn(*mut usize, *mut usize, *mut usize);
        let addr = crate::native_lib::std_symbol_addr("hlp_gc_card_info")?;
        let (mut bias, mut base, mut len) = (0usize, 0usize, 0usize);
        unsafe { std::mem::transmute::<usize, Info>(addr)(&mut bias, &mut base, &mut len) };
        (len != 0).then_some(CardTable { bias, base, len })
    })
}
