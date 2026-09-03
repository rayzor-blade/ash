//! HashLink standard library reimplemented in Rust, exported as C ABI
//! entry points for the interpreter and both JIT tiers.
// This crate's public surface is `#[no_mangle] extern "C"` shims consumed
// through the HashLink FFI contract, not a Rust API: per-function `# Safety`
// sections would restate the single contract (pointers come from the VM and
// follow HashLink's layout rules), and the FFI signatures are what they are.
#![allow(clippy::missing_safety_doc)]
#![allow(clippy::type_complexity)]
#![allow(clippy::too_many_arguments)]
#![feature(once_cell_get_mut)]
// c_variadic stabilized in 1.99; the attribute stays so older nightlies
// still build, and stable_features quiets newer ones.
#![allow(stable_features)]
#![feature(c_variadic)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(clippy::not_unsafe_ptr_arg_deref)]

#[macro_use]
pub mod macros;

pub mod aot_native;
pub mod array;
pub mod buffer;
pub mod bytes;
pub mod cast;
pub mod date;
pub mod error;
pub mod fiber;
#[cfg(target_family = "wasm")]
pub mod fiber_host;
pub mod file;
pub mod fun;
pub mod gc;
pub mod hl;
pub mod hl_compat;
pub mod maps;
pub mod math;
pub mod obj;
pub mod process;
pub mod random;
pub mod regexp;
pub mod socket;
pub mod strings;
pub mod sys;
pub mod thread;
pub mod types;
#[macro_use]
pub mod debugger;
pub(crate) mod sort;
pub(crate) mod ucs2;
pub(crate) mod unicase;
