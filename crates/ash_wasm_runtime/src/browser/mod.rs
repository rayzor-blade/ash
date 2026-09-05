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
//! What is still missing before a module runs in a page: the WASI preview 1
//! surface its standard library calls (stdout, clock, randomness, arguments),
//! and the fiber suspension `ash_host_fiber_yield` names. Both are host work
//! of the same shape as the sockets below.

pub mod sockets;
