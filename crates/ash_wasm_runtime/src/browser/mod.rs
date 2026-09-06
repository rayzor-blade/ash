//! The browser host.
//!
//! The same contract [`crate::native`] implements with `wasmtime`, reached
//! through the browser's own APIs instead. Rust throughout: the only
//! JavaScript in this path is the glue `wasm-bindgen` generates from these
//! signatures, which is build output in the way an object file is.
//!
//! [`sockets`] is the part that exists. A page cannot open a TCP connection
//! to an arbitrary host and port -- that is a security boundary rather than a
//! missing feature -- so sockets here are WebSocket connections, which is
//! what a page may open and what a relay can bridge to TCP. The guest asks
//! the host for every socket operation -- the twelve `ash_host_socket_*`
//! imports, since preview 1 cannot carry a socket at all -- and a page answers
//! the client half over WebSocket and refuses the server half.
//!
//! [`wasi`] is the other part: the whole `wasi_snapshot_preview1` surface,
//! answered by the page. All 45 of them, because an import nothing supplies
//! is a link error before a line runs -- so a module cannot be instantiated
//! until every one has an answer, even the ones a page can only refuse.
//! [`memory`] is how both reach the guest's heap.
//!
//! [`imports`] binds all of it to a module: 69 imports, every one of which
//! must be answered before a module can be instantiated at all.
//!
//! [`run`] is the entry point: it instantiates a module against all of it and
//! calls the module's entrypoint.
//!
//! What is still missing: suspending a fiber, which needs JSPI or a worker
//! parked on `Atomics.wait`, and loading a native library, which is the
//! native host's loader against `WebAssembly.instantiate`.

pub mod imports;
pub mod run;
pub mod memory;
pub mod sockets;
pub mod wasi;
