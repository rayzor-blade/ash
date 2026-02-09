#![feature(once_cell_get_mut)]
#![feature(portable_simd)]
#![feature(c_variadic)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

#[macro_use]
pub mod macros;



pub mod gc;
pub mod hl;
pub mod types;
pub mod obj;
pub mod maps;
pub mod strings;
pub mod random;
pub mod bytes;
pub mod array;
pub mod buffer;
pub mod sys;
pub mod error;
pub mod cast;
pub mod fun;
pub mod date;
pub mod math;
#[macro_use]
pub mod debugger;
pub (crate) mod ucs2;
pub (crate) mod unicase;
pub (crate) mod sort;





