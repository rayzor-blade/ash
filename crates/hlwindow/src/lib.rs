//! A window for HashLink, over winit.
//!
//! hlwgpu draws into a surface and does not make one; this is where a native
//! program gets something to draw into. Kept apart so hlwgpu depends on
//! nothing, and so a program that renders offscreen never loads it.
//!
//! Native only. Every side but the implementation comes from `window.api`.

#![cfg(not(target_family = "wasm"))]

mod bindings;
mod imp;
