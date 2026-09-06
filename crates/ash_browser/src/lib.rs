//! The browser host, as a page loads it.
//!
//! Nothing of its own: the host is [`ash_wasm_runtime::browser`], and this
//! crate exists to give it a `cdylib` for `wasm-bindgen` to package. See its
//! `Cargo.toml` for why that cannot live in the runtime crate.
//!
//! Build it with:
//!
//! ```text
//! cargo build --release -p ash_browser --target wasm32-unknown-unknown
//! wasm-bindgen --target web --out-dir <dir> \
//!   target/wasm32-unknown-unknown/release/ash_browser.wasm
//! ```
//!
//! and then, from the page, hand `run` a module and the arguments it should
//! see. Everything else -- fetching, when to run, what to do with the exit
//! status -- is the page's.

// Elsewhere this is an empty library rather than a build error: the workspace
// builds every crate on every platform, and a browser host is not something a
// native target has any use for.
#![cfg(target_family = "wasm")]

pub use ash_wasm_runtime::browser::run::{run, Outcome};
