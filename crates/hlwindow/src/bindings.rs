//! Generated from `window.api` by `hl_native_gen`, via `build.rs`.

pub mod kinds {
    include!(concat!(env!("OUT_DIR"), "/kinds.rs"));
}

mod native {
    include!(concat!(env!("OUT_DIR"), "/native.rs"));
}
