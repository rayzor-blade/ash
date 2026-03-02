#![feature(once_cell_get_mut)]
#![feature(portable_simd)]
#![feature(c_variadic)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(clippy::not_unsafe_ptr_arg_deref)]

#[macro_use]
pub mod macros;

pub mod array;
pub mod buffer;
pub mod bytes;
pub mod cast;
pub mod date;
pub mod error;
pub mod fun;
pub mod gc;
pub mod hl;
pub mod maps;
pub mod math;
pub mod obj;
pub mod random;
pub mod regexp;
pub mod strings;
pub mod sys;
pub mod types;
#[macro_use]
pub mod debugger;
pub(crate) mod sort;
pub(crate) mod ucs2;
pub(crate) mod unicase;
