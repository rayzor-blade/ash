//! WASI preview 1 in a page.
//!
//! A browser has no WASI. Everything a module imports under
//! `wasi_snapshot_preview1` has to be answered by the page, and until it is,
//! a module cannot be instantiated at all -- not because it calls these, but
//! because an import that nothing supplies is a link error before a line
//! runs. So the whole surface is here, all 45 of them, and the ones a page
//! cannot do answer the way a kernel refusing would.
//!
//! # What a page can actually do
//!
//! | asked for | answered with |
//! |---|---|
//! | `fd_write` to 1 and 2 | `console.log` / `console.error`, a line at a time |
//! | `fd_read` from 0 | end of input |
//! | `fd_fdstat_get` on 0, 1, 2 | a character device, so libc line-buffers |
//! | `clock_time_get` | `Date.now()` for real time, `performance.now()` for monotonic |
//! | `random_get` | `crypto.getRandomValues` |
//! | `args_get`, `environ_get` | what the embedder passed in |
//! | `proc_exit` | recorded, and the call traps to unwind the guest |
//! | `sched_yield` | success, having yielded to nothing |
//! | every path and every other fd | `ENOTSUP`, or `EBADF` for a descriptor |
//!
//! # Three differences a program can see
//!
//! **There is no filesystem.** `path_open` answers `ENOTSUP` and
//! `fd_prestat_get` answers `EBADF`, which is what libc reads as "no
//! preopened directories" and is why it stops asking after the first. A Haxe
//! program that reads a file fails the way it would with the file absent.
//!
//! **Output is a line at a time.** `console.log` puts a line break after
//! every call, and `Sys.print` does not end its writes with a newline, so
//! writing each call straight through would break one line into several.
//! Bytes are held until a newline arrives and flushed then; whatever is left
//! is flushed at `proc_exit`. A program that prints without ever a newline
//! and then blocks forever will not have shown its last line -- which is the
//! same bargain as a line-buffered terminal.
//!
//! **Nothing waits.** A page has one thread and it is the event loop, so
//! `poll_oneoff` reports its clock subscriptions as expired immediately
//! rather than sleeping. `Sys.sleep` therefore returns at once. A program
//! that wants to wait yields to the host, which is what
//! `ash_host_fiber_yield` is for.

use web_sys::console;

use crate::wasi_abi::{clock, errno, filetype, gather, parse_iovecs, vector_sizes, vectors};

use super::memory::Guest;

use wasm_bindgen::JsValue;

/// The rights a character device has: everything to do with a stream and
/// nothing to do with a file. libc only checks that the seek right is absent
/// before deciding a descriptor is not seekable.
const TTY_RIGHTS: u64 = 0x2000_0082;

/// What a page answers `wasi_snapshot_preview1` with.
pub struct Wasi {
    /// `Sys.args()`, argv[0] included.
    args: Vec<String>,
    /// `Sys.environment()`, each entry already `NAME=value`.
    environ: Vec<String>,
    /// Bytes written to 1 and 2 and not yet ended by a newline.
    pending: [Vec<u8>; 2],
    /// Set by `proc_exit`, for the embedder to read after the guest unwinds.
    exit: Option<i32>,
}

impl Default for Wasi {
    fn default() -> Self {
        Self::new(vec!["ash".to_string()], Vec::new())
    }
}

impl Wasi {
    pub fn new(args: Vec<String>, environ: Vec<String>) -> Self {
        Self {
            args,
            environ,
            pending: [Vec::new(), Vec::new()],
            exit: None,
        }
    }

    /// What the program exited with, once it has.
    pub fn exit_status(&self) -> Option<i32> {
        self.exit
    }

    // --- output -----------------------------------------------------------

    /// Hand a stream's completed lines to the console, keeping the rest.
    fn buffer(&mut self, which: usize, bytes: &[u8]) {
        self.pending[which].extend_from_slice(bytes);
        while let Some(at) = self.pending[which].iter().position(|b| *b == b'\n') {
            let line: Vec<u8> = self.pending[which].drain(..=at).collect();
            let text = String::from_utf8_lossy(&line[..line.len() - 1]).into_owned();
            Self::say(which, &text);
        }
    }

    /// Whatever is held back, said now. Called when the program exits, so a
    /// last line without a newline is not lost.
    pub fn flush(&mut self) {
        for (which, buffered) in self.pending.iter_mut().enumerate() {
            if buffered.is_empty() {
                continue;
            }
            let line = std::mem::take(buffered);
            Self::say(which, &String::from_utf8_lossy(&line));
        }
    }

    fn say(which: usize, text: &str) {
        let value = JsValue::from_str(text);
        if which == 0 {
            console::log_1(&value);
        } else {
            console::error_1(&value);
        }
    }

    // --- the surface ------------------------------------------------------

    pub fn fd_write(&mut self, guest: &Guest, fd: i32, iovs: u32, iovs_len: u32, out: u32) -> i32 {
        let which = match fd {
            1 => 0,
            2 => 1,
            _ => return errno::BADF,
        };
        let Some(region) = guest.read(iovs, iovs_len.saturating_mul(8)) else {
            return errno::FAULT;
        };
        let Some(vecs) = parse_iovecs(&region, iovs_len) else {
            return errno::FAULT;
        };
        let Some(bytes) = gather(&vecs, guest.reader()) else {
            return errno::FAULT;
        };
        let written = bytes.len() as u32;
        self.buffer(which, &bytes);
        if !guest.write_u32(out, written) {
            return errno::FAULT;
        }
        errno::SUCCESS
    }

    /// Standard input is empty: a page has none to give. Zero bytes read is
    /// end of input, which is what a program reading stdin should see rather
    /// than an error.
    pub fn fd_read(&mut self, guest: &Guest, fd: i32, _iovs: u32, _len: u32, out: u32) -> i32 {
        if fd != 0 {
            return errno::BADF;
        }
        if !guest.write_u32(out, 0) {
            return errno::FAULT;
        }
        errno::SUCCESS
    }

    pub fn fd_fdstat_get(&self, guest: &Guest, fd: i32, out: u32) -> i32 {
        if !(0..=2).contains(&fd) {
            return errno::BADF;
        }
        let stat = crate::wasi_abi::fdstat(filetype::CHARACTER_DEVICE, 0, TTY_RIGHTS, 0);
        if !guest.write(out, &stat) {
            return errno::FAULT;
        }
        errno::SUCCESS
    }

    pub fn fd_filestat_get(&self, guest: &Guest, fd: i32, out: u32) -> i32 {
        if !(0..=2).contains(&fd) {
            return errno::BADF;
        }
        if !guest.write(out, &crate::wasi_abi::filestat(filetype::CHARACTER_DEVICE)) {
            return errno::FAULT;
        }
        errno::SUCCESS
    }

    /// Closing a standard stream succeeds and does nothing. Anything else was
    /// never open.
    pub fn fd_close(&mut self, fd: i32) -> i32 {
        if (0..=2).contains(&fd) {
            errno::SUCCESS
        } else {
            errno::BADF
        }
    }

    /// The standard streams are pipes, and a pipe does not seek. libc asks
    /// this to decide whether it may buffer by block.
    pub fn fd_seek(&self, fd: i32) -> i32 {
        if (0..=2).contains(&fd) {
            errno::SPIPE
        } else {
            errno::BADF
        }
    }

    /// No preopened directories. libc walks descriptors from 3 until one
    /// answers `EBADF`, so this is what stops the walk -- and answering
    /// anything else here makes libc believe in a filesystem that is not
    /// there.
    pub fn fd_prestat_get(&self, _fd: i32) -> i32 {
        errno::BADF
    }

    pub fn clock_time_get(&self, guest: &Guest, id: u32, out: u32) -> i32 {
        let millis = match id {
            clock::REALTIME => js_sys::Date::now(),
            clock::MONOTONIC => now_monotonic(),
            // The two process clocks are not something a page can answer,
            // and a wrong number is worse than a refusal.
            _ => return errno::NOTSUP,
        };
        let nanos = (millis * 1.0e6) as u64;
        if !guest.write_u64(out, nanos) {
            return errno::FAULT;
        }
        errno::SUCCESS
    }

    /// A millisecond. Browsers coarsen their clocks deliberately, against
    /// timing attacks, so claiming nanoseconds would be a lie a program might
    /// act on.
    pub fn clock_res_get(&self, guest: &Guest, id: u32, out: u32) -> i32 {
        if !matches!(id, clock::REALTIME | clock::MONOTONIC) {
            return errno::NOTSUP;
        }
        if !guest.write_u64(out, 1_000_000) {
            return errno::FAULT;
        }
        errno::SUCCESS
    }

    pub fn random_get(&self, guest: &Guest, ptr: u32, len: u32) -> i32 {
        let mut bytes = vec![0u8; len as usize];
        if crypto_fill(&mut bytes).is_err() {
            return errno::IO;
        }
        if !guest.write(ptr, &bytes) {
            return errno::FAULT;
        }
        errno::SUCCESS
    }

    pub fn args_sizes_get(&self, guest: &Guest, count_at: u32, bytes_at: u32) -> i32 {
        Self::sizes(guest, &self.args, count_at, bytes_at)
    }

    pub fn args_get(&self, guest: &Guest, pointers_at: u32, block_at: u32) -> i32 {
        Self::write_vectors(guest, &self.args, pointers_at, block_at)
    }

    pub fn environ_sizes_get(&self, guest: &Guest, count_at: u32, bytes_at: u32) -> i32 {
        Self::sizes(guest, &self.environ, count_at, bytes_at)
    }

    pub fn environ_get(&self, guest: &Guest, pointers_at: u32, block_at: u32) -> i32 {
        Self::write_vectors(guest, &self.environ, pointers_at, block_at)
    }

    fn sizes(guest: &Guest, items: &[String], count_at: u32, bytes_at: u32) -> i32 {
        let (count, bytes) = vector_sizes(items);
        if !guest.write_u32(count_at, count) || !guest.write_u32(bytes_at, bytes) {
            return errno::FAULT;
        }
        errno::SUCCESS
    }

    fn write_vectors(guest: &Guest, items: &[String], pointers_at: u32, block_at: u32) -> i32 {
        let (pointers, block) = vectors(items, block_at);
        if !guest.write(pointers_at, &pointers) || !guest.write(block_at, &block) {
            return errno::FAULT;
        }
        errno::SUCCESS
    }

    /// Record the status and say so. The caller traps the guest, because
    /// `proc_exit` does not return and a guest that carried on after it would
    /// run past its own end.
    pub fn proc_exit(&mut self, code: i32) {
        self.flush();
        self.exit = Some(code);
    }

    /// A page yields by returning to the event loop, which this call is not
    /// in a position to do. Saying so as an error would fail programs that
    /// only meant to be polite.
    pub fn sched_yield(&self) -> i32 {
        errno::SUCCESS
    }

    /// Nothing waits here. Every subscription is reported as ready at once,
    /// which turns a sleep into a no-op and a wait for input into an
    /// immediate answer. See the note at the top.
    pub fn poll_oneoff(&self, guest: &Guest, _subs: u32, _events: u32, n: u32, out: u32) -> i32 {
        // Reporting zero events would spin a caller that waits for one, so
        // the count is honest about how many subscriptions were considered
        // even though none of them slept.
        if !guest.write_u32(out, n) {
            return errno::FAULT;
        }
        errno::SUCCESS
    }

    /// Everything a page has no answer for.
    ///
    /// One function rather than forty, because they are one decision:
    /// `ENOTSUP` says the host cannot do this, which is true, and is what a
    /// program should report rather than a wrong success. The descriptor
    /// calls answer `EBADF` instead, since the descriptor genuinely does not
    /// exist.
    pub fn unsupported(&self) -> i32 {
        errno::NOTSUP
    }

    pub fn bad_descriptor(&self) -> i32 {
        errno::BADF
    }
}

/// `performance.now()` from a window or a worker, and `Date.now()` if this is
/// somehow neither.
fn now_monotonic() -> f64 {
    if let Some(window) = web_sys::window() {
        if let Ok(p) = window.performance().ok_or(()) {
            return p.now();
        }
    }
    js_sys::Date::now()
}

/// `crypto.getRandomValues`, from a window or a worker.
fn crypto_fill(bytes: &mut [u8]) -> Result<(), ()> {
    let crypto = web_sys::window()
        .and_then(|w| w.crypto().ok())
        .ok_or(())?;
    crypto
        .get_random_values_with_u8_array(bytes)
        .map(|_| ())
        .map_err(|_| ())
}
