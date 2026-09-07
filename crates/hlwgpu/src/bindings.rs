//! Generated from `wgpu.api` by `hl_native_gen`, via `build.rs`.
//!
//! Natively the primitives call `crate::imp`; on wasm they forward to the
//! host, because a wasip1 module has neither a GPU nor JavaScript.

pub mod kinds {
    include!(concat!(env!("OUT_DIR"), "/kinds.rs"));
}

#[cfg(not(target_family = "wasm"))]
mod native {
    include!(concat!(env!("OUT_DIR"), "/native.rs"));
}

#[cfg(target_family = "wasm")]
mod wasm {
    include!(concat!(env!("OUT_DIR"), "/wasm.rs"));
}
