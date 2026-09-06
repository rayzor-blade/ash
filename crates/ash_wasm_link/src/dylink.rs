//! Reading a native library shipped as a wasm side module.
//!
//! An HDLL for wasm is a `dylink.0` module: not a component, because a
//! component owns its linear memory and the canonical ABI copies values
//! across the boundary, and `DEFINE_PRIM` signatures pass `vbyte*`,
//! `varray*`, `vdynamic*` and `vclosure*` -- raw pointers into the heap the
//! collector scans. A side module instead imports the program's memory and
//! function table, so it works on that same heap and a function pointer it
//! returns is an index the program can call. That is the same relationship an
//! HDLL has with libhl, which is why the `DEFINE_PRIM` resolver protocol
//! needs no change: the resolver is looked up by name among the module's
//! exports.
//!
//! What is read here is only what the program has to answer for: the names
//! the library imports. The program exports exactly those, so hosting a
//! library widens its ABI by what that library actually asks for and no more.

use anyhow::Result;
use wasmparser::{Parser, Payload};

/// What a side module needs from the program that hosts it.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SideModule {
    /// Functions it imports by name, in the order it declares them.
    pub functions: Vec<String>,
    /// Bytes of linear memory the loader must set aside for its data, and the
    /// alignment that address needs.
    pub memory_size: u32,
    pub memory_align: u32,
    /// Table slots the loader must append for its function pointers.
    pub table_size: u32,
}

/// The `dylink.0` subsection that carries the memory and table requirements.
const WASM_DYLINK_MEM_INFO: u8 = 1;

/// Whether a file's opening bytes could be a side module.
///
/// A cheap filter, so that scanning a directory does not read whole modules
/// to learn they are not libraries -- the program's own module sits in that
/// directory and is measured in megabytes. `dylink.0` is the first section
/// wasm-ld writes, so its name appears within the first few bytes of a side
/// module and nowhere near the front of anything else.
pub fn looks_like_side_module(prefix: &[u8]) -> bool {
    prefix
        .windows(b"dylink.0".len())
        .any(|w| w == b"dylink.0")
}

/// How much of a file [`looks_like_side_module`] needs.
pub const SIDE_MODULE_PREFIX: usize = 64;

/// Read a side module, or `None` if this is not one.
///
/// Not being a side module is the ordinary case -- the program itself, or a
/// module built for some other host -- so it is not an error.
pub fn read_side_module(bytes: &[u8]) -> Result<Option<SideModule>> {
    let mut out = SideModule::default();
    let mut is_side_module = false;
    for payload in Parser::new(0).parse_all(bytes) {
        match payload? {
            Payload::CustomSection(c) if c.name() == "dylink.0" => {
                is_side_module = true;
                read_mem_info(c.data(), &mut out);
            }
            Payload::ImportSection(imports) => {
                for import in imports.into_imports() {
                    let import = import?;
                    if matches!(import.ty, wasmparser::TypeRef::Func(_)) {
                        out.functions.push(import.name.to_string());
                    }
                }
            }
            _ => {}
        }
    }
    Ok(is_side_module.then_some(out))
}

/// The subsections are LEB-encoded and a reader that does not know one must
/// skip it by its length, so an unknown subsection is not a failure.
fn read_mem_info(data: &[u8], out: &mut SideModule) {
    let mut p = 0usize;
    while p < data.len() {
        let id = data[p];
        p += 1;
        let Some((len, next)) = uleb(data, p) else {
            return;
        };
        p = next;
        let end = p + len as usize;
        if end > data.len() {
            return;
        }
        if id == WASM_DYLINK_MEM_INFO {
            let mut q = p;
            for slot in [
                &mut out.memory_size,
                &mut out.memory_align,
                &mut out.table_size,
            ] {
                let Some((v, next)) = uleb(data, q) else {
                    return;
                };
                *slot = v;
                q = next;
            }
        }
        p = end;
    }
}

fn uleb(data: &[u8], mut p: usize) -> Option<(u32, usize)> {
    let (mut value, mut shift) = (0u32, 0u32);
    loop {
        let byte = *data.get(p)?;
        p += 1;
        value |= u32::from(byte & 0x7f).checked_shl(shift)?;
        if byte & 0x80 == 0 {
            return Some((value, p));
        }
        shift += 7;
        if shift > 31 {
            return None;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A module without the section is not a side module, and saying so is
    /// not an error.
    #[test]
    fn a_plain_module_is_not_a_side_module() {
        let empty = b"\0asm\x01\0\0\0";
        assert_eq!(read_side_module(empty).unwrap(), None);
    }

    /// The reference the rest of this is checked against: a side module built
    /// by `wasm-ld --experimental-pic -shared`, pointed at by an env var so
    /// the test says what it needs rather than carrying a binary.
    #[test]
    fn reads_what_a_side_module_asks_for() {
        let Ok(path) = std::env::var("ASH_SIDE_MODULE") else {
            eprintln!("set ASH_SIDE_MODULE to a dylink.0 module to run this");
            return;
        };
        let bytes = std::fs::read(&path).expect("reading the side module");
        let side = read_side_module(&bytes)
            .expect("parsing")
            .expect("this is not a side module");
        eprintln!("{path}: {side:?}");
        assert!(
            side.functions.iter().any(|f| f.starts_with("hlp_")),
            "a wasm HDLL imports the runtime functions it calls by name"
        );
    }
}
