//! The wasm side of the ash runtime, and the hosts that complete it.
//!
//! Three parts, and which side of the module boundary each lives on is the
//! whole design:
//!
//! * [`guest`] is compiled INTO the program. `ash_std` depends on it when
//!   built for wasm, so its contents are ordinary Rust linkage, not wasm
//!   imports. Anything that can be done inside the sandbox belongs here, and
//!   most things can: WASI already gives the standard library a clock,
//!   randomness, stdout and a filesystem.
//! * [`native`], and a browser host to come, are hosts on the far side.
//!   They exist only for what a module cannot do for itself: suspending a
//!   fiber, because a wasm module has no addressable stack and no instruction
//!   that moves between two; and sockets, because WASI preview 1 has no
//!   descriptor that can be one.
//!
//! The hosts implement the same contract:
//!
//! * `native` runs the module under `wasmtime`: the conformance lane and CI.
//!   No browser, no JavaScript, no `wasm-bindgen`, and wasmtime's own fibers
//!   answer the suspending import.
//! * The browser host is not written yet. It will be the same contract with
//!   the browser's own APIs behind it, through `web-sys`, and still no
//!   hand-written JavaScript: `wasm-bindgen` generates its glue the way a
//!   compiler generates an object file. Its feature and dependencies are
//!   declared so the shape is fixed before the code exists.
//!
//! What the guest imports, and what a host owes it:
//!
//! | import | native | browser |
//! |---|---|---|
//! | `wasi_snapshot_preview1.*` | `wasmtime-wasi` | `web-sys`: `console`, `Performance`, `Crypto` |
//! | `env.ash_host_fiber_yield` | a `wasmtime` fiber suspend | JSPI, or a worker parked on `Atomics.wait` |
//! | `env.ash_host_socket_*`, twelve of them | the OS's sockets through `libc` (`native::sockets`) | WebSocket for the client half, `NOTSUP` for the server half (`browser::sockets`) |
//!
//! The native column is implemented; the browser column is the plan, except
//! for its socket table, which is written and not yet wired to a module.
//!
//! The second row is the whole reason a host exists rather than a library.
//! [`guest`] holds the program's side of it; `docs/wasm-target.md` explains
//! why choosing between JSPI, a worker pool and Asyncify is the host's
//! business and not the program's.
//!
//! The third row is one contract for every host. `open`, `connect`, `bind`,
//! `listen`, `accept`, `send`, `recv`, `shutdown`, `close`, `name`, `set` and
//! `poll`, every argument and result an `i32`: a call that yields a value
//! (`open`, `accept`, `send`, `recv`, `poll`) returns it when non-negative and
//! `-errno` otherwise, a call that yields nothing returns 0 or `+errno`, and
//! the errno numbers are WASI preview 1's (`AGAIN` 6, `INPROGRESS` 26,
//! `NOTSUP` 58, ...). Descriptors are the host's own namespace from 0, never
//! a WASI fd. `poll` reads and writes 8-byte `{ fd: i32, events: u16,
//! revents: u16 }` records with ash's bits (`RD` 1, `WR` 2, `PRI` 4, `ERR` 8,
//! `HUP` 16, `NVAL` 32), not any libc's. A host without sockets installs the
//! twelve and answers `NOTSUP`; the guest then fails the call the way a
//! kernel would have, instead of failing to instantiate. The full table is in
//! `docs/wasm-target.md`.
//!
//! # Not done yet
//!
//! Threads, which want a worker pool over shared memory, and the collector's
//! rendezvous across them. Both belong here; neither is written.

/// Compiled into the program, and only where that means something.
///
/// It declares an import no native target can satisfy, so compiling it
/// elsewhere leaves an undefined symbol behind for whatever links next. That
/// is invisible on unix, where a shared object may carry one, and fatal on
/// Windows, where a DLL may not.
#[cfg(target_family = "wasm")]
pub mod guest;

#[cfg(feature = "native")]
pub mod native;

#[cfg(all(feature = "browser", target_family = "wasm"))]
pub mod browser;

/// Reading a module: `ash wasm` is this, and so is the emitter's own gate.
#[cfg(feature = "validate")]
pub mod validate;

/// The import a host must supply beyond WASI, named once so the two backends
/// and the guest cannot drift apart.
pub const FIBER_YIELD_IMPORT: (&str, &str) = ("env", "ash_host_fiber_yield");
