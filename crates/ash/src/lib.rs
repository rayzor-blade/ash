#![allow(warnings)]
// #![feature(new_zeroed_alloc)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

pub mod bytecode;
pub mod c_types;
pub mod functions;
pub mod hl_bindings {
    #![allow(non_upper_case_globals)]
    #![allow(non_camel_case_types)]
    #![allow(non_snake_case)]
    include!(concat!(env!("OUT_DIR"), "/hl_bindings.rs"));
}
pub mod jit;
pub mod module;
pub mod native_lib;
pub mod opcodes;
pub mod reload;
pub mod types;
pub mod values;

use hl_bindings as hl;

#[cfg(test)]
mod test_macro;
