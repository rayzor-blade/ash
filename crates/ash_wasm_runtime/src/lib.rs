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
//!   They exist only
//!   for what a module cannot do for itself, which today is one operation:
//!   suspending a fiber. A wasm module has no addressable stack and no
//!   instruction that moves between two, so that one has to be an import
//!   whatever else is not.
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
//!
//! The native column is implemented; the browser column is the plan.
//!
//! The second row is the whole reason a host exists rather than a library.
//! [`guest`] holds the program's side of it; `docs/wasm-target.md` explains
//! why choosing between JSPI, a worker pool and Asyncify is the host's
//! business and not the program's.
//!
//! # Not done yet
//!
//! Threads, which want a worker pool over shared memory, and the collector's
//! rendezvous across them. Both belong here; neither is written.

/// Compiled into the program. Always available, since `ash_std` depends on
/// it for wasm and a native build still wants to read it.
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
