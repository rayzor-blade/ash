#![ allow(warnings)]
// #![feature(new_zeroed_alloc)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

pub mod bytecode;
pub mod types;
pub mod values;
pub mod functions;
pub mod opcodes;
pub mod module;
pub mod native_lib;
pub mod jit;
pub mod c_types;
pub mod hl_bindings;

use hl_bindings as hl;

#[cfg(test)]
mod test_macro;