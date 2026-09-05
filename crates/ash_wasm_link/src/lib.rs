//! A WebAssembly linker, so that compiling Haxe to wasm needs one tool.
//!
//! # Why this exists
//!
//! `ash --build --target wasm32-*` used to spawn `wasm-ld`, which meant a
//! Haxe developer had to install LLVM or a wasi-sdk before they could build,
//! and meant ash hunting through PATH for a linker whose version might not
//! match its own. Every part of that is a tool the user did not ask for.
//!
//! # What it links, and what it therefore does not implement
//!
//! Exactly two relocatable objects: the program ash emitted, and one
//! prelinked runtime object carrying `ash_std`, a wasi libc and `libsetjmp`,
//! joined once when ash itself is built. That single decision removes the
//! largest part of a general linker -- `ar` reading, lazy archive pull, and
//! the symbol-resolution passes that go with it -- because the 1,132 archive
//! members are resolved before a user ever runs ash.
//!
//! Measured across every object ash actually links, the following are absent
//! and are refused rather than half-handled: COMDAT groups, thread-local
//! storage, the six 64-bit relocation forms, and position-independent output.
//! Debug sections are dropped, which removes three more relocation types and
//! about a quarter of all patches.
//!
//! Refusing rather than dropping has already earned itself: a survey of these
//! objects reported no constructors, and the runtime object turned out to
//! have them. Had they been skipped quietly, the result would have been a
//! program whose allocator was never initialised.
//!
//! What remains is eleven relocation types and the work of assigning six
//! index spaces.
//!
//! # The one dangerous property
//!
//! A relocation writes a value into a fixed-width slot. Every wrong value
//! that happens to be in range produces a module that passes validation and
//! runs -- wrongly. There are four `TAG_INDEX_LEB` relocations in a link of
//! 62,000, and getting those four wrong yields a program that cannot catch
//! its own exceptions while validating perfectly.
//!
//! So this crate is written to be checkable rather than to be clever: patch
//! sites are verified to hold what the symbol table says they hold before
//! anything is written, and the linker is tested by differential comparison
//! against `wasm-ld` rather than by inspection.

use anyhow::Result;

pub mod body;
pub mod link;
pub mod object;
pub mod suspend;

pub use link::{link, LinkOptions};
pub use object::{read, Object, Symbol, SymbolTarget};

/// What a patch site held before the linker touched it, and what the symbol
/// table says it should have held.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SiteCheck {
    pub kind: String,
    pub offset: u32,
    pub found: u32,
    pub expected: u32,
}

/// Verify that relocation offsets mean what this linker assumes they mean.
///
/// A relocatable object writes the object-local index into each patch site
/// and records the *symbol* index in the relocation entry. So for every
/// function relocation the site must already hold the index that symbol
/// resolves to. If the offset convention were wrong -- measured from the
/// section header rather than its contents, say -- this would disagree
/// immediately and everywhere, instead of producing a module that runs and
/// computes the wrong answer.
///
/// Returns the disagreements. An empty result is the reason to trust every
/// later patch.
pub fn check_function_sites(obj: &Object) -> Result<Vec<SiteCheck>> {
    use wasmparser::RelocationType;

    let mut wrong = Vec::new();
    for entry in &obj.code_relocs {
        if entry.ty != RelocationType::FunctionIndexLeb {
            continue;
        }
        let Some(symbol) = obj.symbols.get(entry.index as usize) else {
            wrong.push(SiteCheck {
                kind: "symbol index out of range".into(),
                offset: entry.offset,
                found: 0,
                expected: entry.index,
            });
            continue;
        };
        // An undefined function symbol's site holds its import index, which
        // is the index the import occupies in this object's function space.
        let expected = match symbol.target {
            SymbolTarget::Function { index } => index,
            SymbolTarget::Undefined | SymbolTarget::UndefinedData => continue,
            _ => continue,
        };
        let found = object::peek_u32_leb5(&obj.code_payload, entry.offset as usize)?;
        if found != expected {
            wrong.push(SiteCheck {
                kind: format!("FUNCTION_INDEX_LEB {}", symbol.name),
                offset: entry.offset,
                found,
                expected,
            });
        }
    }
    Ok(wrong)
}

/// A short human summary of what an object holds, for diagnosis.
pub fn describe(obj: &Object) -> String {
    let defined_functions = obj.functions.len();
    let undefined = obj.symbols.iter().filter(|s| s.is_undefined()).count();
    format!(
        "{}: {} types, {} imports ({} funcs), {} functions, {} data segments, \
         {} symbols ({undefined} undefined), {} code relocs, {} data relocs, \
         {} table entries, {} tags, {} ctors",
        obj.name,
        obj.types.len(),
        obj.imports.len(),
        obj.imported_functions(),
        defined_functions,
        obj.data_segments.len(),
        obj.symbols.len(),
        obj.code_relocs.len(),
        obj.data_relocs.len(),
        obj.elements.len(),
        obj.tags.len(),
        obj.init_funcs.len(),
    )
}
