//! Reaching into the guest's linear memory from a page.
//!
//! A host function is handed pointers, and in a page the memory behind them
//! is a `WebAssembly.Memory` on the JavaScript side. Every access is a copy
//! across that boundary, so the rule here is to copy the bytes the call needs
//! and never the heap: an `fd_write` of one line reads eight bytes per
//! `iovec` and then the line, not the megabytes around them.
//!
//! # The detached buffer
//!
//! `Memory.buffer` is invalidated when the memory grows, and a typed array
//! over the old buffer then reads nothing. So a view is made per access and
//! never kept. That is not a performance mistake to fix later: keeping one is
//! a correctness mistake that appears only after the first allocation big
//! enough to grow the heap.

use js_sys::{Uint8Array, WebAssembly};

/// The guest's memory, as a page sees it.
#[derive(Clone)]
pub struct Guest {
    memory: WebAssembly::Memory,
}

impl Guest {
    pub fn new(memory: WebAssembly::Memory) -> Self {
        Self { memory }
    }

    /// A fresh view. Never stored: see the note above about growing.
    fn view(&self) -> Uint8Array {
        Uint8Array::new(&self.memory.buffer())
    }

    fn len(&self) -> u32 {
        self.view().length()
    }

    /// `len` bytes at `ptr`, or `None` if that is not inside the guest.
    pub fn read(&self, ptr: u32, len: u32) -> Option<Vec<u8>> {
        let end = ptr.checked_add(len)?;
        if end > self.len() {
            return None;
        }
        let mut out = vec![0u8; len as usize];
        self.view().subarray(ptr, end).copy_to(&mut out);
        Some(out)
    }

    /// Write `bytes` at `ptr`. `false` if that is not inside the guest, and
    /// nothing is written in that case.
    pub fn write(&self, ptr: u32, bytes: &[u8]) -> bool {
        let Some(end) = ptr.checked_add(bytes.len() as u32) else {
            return false;
        };
        if end > self.len() {
            return false;
        }
        self.view().subarray(ptr, end).copy_from(bytes);
        true
    }

    pub fn write_u32(&self, ptr: u32, value: u32) -> bool {
        self.write(ptr, &value.to_le_bytes())
    }

    pub fn write_u64(&self, ptr: u32, value: u64) -> bool {
        self.write(ptr, &value.to_le_bytes())
    }

    /// A reader shaped for [`crate::wasi_abi::gather`].
    pub fn reader(&self) -> impl FnMut(u32, u32) -> Option<Vec<u8>> + '_ {
        move |ptr, len| self.read(ptr, len)
    }
}
