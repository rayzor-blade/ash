//! ASH SDL HDLL — Rust replacement for HashLink's C sdl.hdll.
//!
//! Exports the same DEFINE_PRIM resolver API so the ASH interpreter
//! can load it as a drop-in replacement. Links directly against SDL2
//! and OpenGL.

#![allow(non_upper_case_globals, non_camel_case_types, unused)]

mod resolver;
mod types;
mod sdl;
mod gl;
