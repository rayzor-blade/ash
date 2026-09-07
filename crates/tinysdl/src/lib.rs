//! Small `sdl` and `ui` libraries for wasm -- enough of them for a Heaps
//! program, and no more.
//!
//! It builds `sdl.wasm` and `ui.wasm` from one archive, each linked with only
//! its own primitives exported. `sdl.wasm`, because `@:hlNative("sdl", ...)` is what a program
//! asks for and the file stem is the name. The crate is `tinysdl` because
//! this is not SDL and is not offered as one: it is sixty-seven of
//! `sdl.hdll`'s two hundred and eight primitives, chosen by tracing what
//! `examples/heaps_base3d` actually calls, and a program that reaches
//! anything else will not find it.
//!
//! # What it does not do
//!
//! Decide anything. Every function forwards, and the host settles what a
//! window and a frame actually are -- a canvas and WebGL2 in a page, a
//! recorder under a headless host. That is what keeps it small enough to
//! generate and small enough to check.
//!
//! # How it is built
//!
//! [`generated`] comes from `scripts/generate_sdl_shim.py`, which reads the
//! signatures the real `sdl.hdll` reports for itself. See
//! `crates/ash/tests/native_signatures.rs`: `DEFINE_PRIM` carries the
//! signature by design, so the library is the authority on its own ABI and
//! the Haxe externs are a second-hand account of it.

#![cfg(target_family = "wasm")]

/// This library allocates through the program. See [`hl_abi::ProgramAllocator`].
#[global_allocator]
static ALLOCATOR: hl_abi::ProgramAllocator = hl_abi::ProgramAllocator;

mod generated;
mod manual;
mod generated_ui;
