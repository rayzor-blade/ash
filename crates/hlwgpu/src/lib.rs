//! WebGPU for HashLink.
//!
//! Natively this crate is `wgpu.hdll`: it holds the implementation and
//! answers the VM directly, so it loads in upstream HashLink too. Built for
//! wasm it is `wgpu.wasm` and holds nothing -- a wasip1 module has no GPU and
//! no JavaScript, so the primitives forward and a host answers. `IMPORTS.md`
//! is what a host has to supply.
//!
//! Every side but the implementation comes from `wgpu.api`; see `build.rs`.

// wgpu's types nest deeply enough that proving `Slab<wgpu::Instance>: Send`
// overflows the default limit of 128.
#![recursion_limit = "256"]

mod bindings;
// Public: the host crate answering the wasm imports reuses this.
pub mod handles;

#[cfg(feature = "native")]
mod imp;
