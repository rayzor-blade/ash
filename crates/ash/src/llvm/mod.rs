pub mod air;
pub mod aot_build;
pub mod aot_data;
pub mod aot_link;
pub mod aot_shard;
pub mod aot_trampoline;
pub mod function;
pub mod lines;
pub mod module;
pub mod object;
pub mod tbaa;
pub mod type_utils;
pub mod types;

// Both live outside this module so the interpreter and Cranelift tiers can
// use them without LLVM; the old paths still resolve here.
pub use crate::jit_memory as win_jit_memory;
pub use crate::stub_bridge;

#[cfg(test)]
mod module_test;
