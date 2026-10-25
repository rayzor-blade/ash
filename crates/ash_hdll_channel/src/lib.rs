//! `ash_channel`: byte messages between a program and a page agent, as a wasm
//! side module.
//!
//! A program in a browser cannot open a connection a page would need an event
//! loop for: it runs in a Worker that never returns to its own. What it can do
//! is share memory with the page. This library allocates a block in the
//! program's shared memory, asks the page to start an agent over it
//! (`ash_host_agent`), and exchanges messages through two rings in the block;
//! the agent, on the page's thread, owns the connection. A blocked `recv` is
//! woken through `ash_host_watch`. The layout and the records are in [`ring`];
//! the page's half is `haxelib/ash-channel/js/channel-agent.mjs`.
//!
//! The Haxe side is `ash.channel.Channel`. Each function below is exported as
//! `hlp_<name>`, the resolver `DEFINE_PRIM` would have written.
//!
//! Needs the shared-memory (`wasm32-wasip1-threads`) build on a
//! cross-origin-isolated page: without shared memory `ash_host_agent` answers
//! 0 and `open` returns 0.

#![cfg_attr(
    target_family = "wasm",
    feature(thread_local, stdarch_wasm_atomic_wait)
)]

pub mod ring;

#[cfg(target_family = "wasm")]
mod natives;
