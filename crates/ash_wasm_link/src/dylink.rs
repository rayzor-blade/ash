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
use wasm_encoder::ExportKind;
use wasm_encoder::reencode::{self, Reencode};
use wasmparser::{Parser, Payload, TypeRef};

/// What a side module needs from the program that hosts it.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SideModule {
    /// Functions it expects the PROGRAM to provide: the ones it calls, from
    /// the `env` module, and the ones whose address it takes, from
    /// `GOT.func`. Not the ones it defines itself, nor the host's -- WASI,
    /// and anything named `ash_host_*`.
    pub functions: Vec<String>,
    /// The `ash_host_*` functions it imports. The host supplies these, except
    /// one the runtime defines itself, which the program exports instead.
    pub host_functions: Vec<String>,
    /// Data symbols it expects to find outside itself, imported as
    /// `GOT.mem.<name>` -- globals holding an address. `errno` is the one
    /// every C library wants. A name it defines itself resolves against its
    /// own exports, so only the rest are the program's to answer.
    pub data: Vec<String>,
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
    prefix.windows(b"dylink.0".len()).any(|w| w == b"dylink.0")
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
    // What it exports, so that a GOT entry for one of its OWN symbols is not
    // asked of the program. Collected as it goes and applied at the end,
    // because the export section comes after the imports.
    let mut defined = std::collections::HashSet::new();
    // Functions whose address it takes, kept apart until the exports are
    // known: most are its own.
    let mut addressed: Vec<String> = Vec::new();
    for payload in Parser::new(0).parse_all(bytes) {
        match payload? {
            Payload::CustomSection(c) if c.name() == "dylink.0" => {
                is_side_module = true;
                read_mem_info(c.data(), &mut out);
            }
            Payload::ExportSection(exports) => {
                for export in exports {
                    defined.insert(export?.name.to_string());
                }
            }
            Payload::ImportSection(imports) => {
                for import in imports.into_imports() {
                    let import = import?;
                    if import.module == "env"
                        && matches!(import.ty, wasmparser::TypeRef::Func(_))
                        // `ash_host_*` is the host's namespace, not the
                        // program's -- it is how the program itself reaches
                        // suspension and sockets, and a library that draws or
                        // plays sound reaches the host the same way. Asking
                        // the program to export one would refuse the link
                        // over a name the program never had.
                        && !import.name.starts_with("ash_host_")
                    {
                        out.functions.push(import.name.to_string());
                    }
                    if import.module == "env"
                        && matches!(import.ty, wasmparser::TypeRef::Func(_))
                        && import.name.starts_with("ash_host_")
                    {
                        out.host_functions.push(import.name.to_string());
                    }
                    if import.module == "GOT.mem" {
                        out.data.push(import.name.to_string());
                    }
                    // The ADDRESS of a function, not a call to it. The
                    // program must still export it, because that is how the
                    // loader finds it to give it a table slot.
                    if import.module == "GOT.func" {
                        addressed.push(import.name.to_string());
                    }
                }
            }
            _ => {}
        }
    }
    out.data.retain(|name| !defined.contains(name));
    addressed.retain(|name| !defined.contains(name));
    out.functions.extend(addressed);
    out.functions.sort();
    out.functions.dedup();
    out.host_functions.sort();
    out.host_functions.dedup();
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

/// The module with the memory it imports also exported as `memory`.
///
/// A side module imports the program's memory. A WASI function the host
/// answers it with reads and writes through the export named `memory` of the
/// instance that CALLED it, so a module that exports none fails its first
/// WASI call ("missing required memory export"). Exporting the very memory it
/// imports changes nothing else about it. A module that imports no memory,
/// already exports `memory`, or has no export section is returned unchanged.
pub fn expose_memory(bytes: &[u8]) -> Result<Vec<u8>> {
    let mut imported = false;
    let mut exported = false;
    let mut has_exports = false;
    for payload in Parser::new(0).parse_all(bytes) {
        match payload? {
            Payload::ImportSection(section) => {
                for import in section.into_imports() {
                    imported |= matches!(import?.ty, TypeRef::Memory(_));
                }
            }
            Payload::ExportSection(section) => {
                has_exports = true;
                for export in section {
                    exported |= export?.name == "memory";
                }
            }
            _ => {}
        }
    }
    if !imported || exported || !has_exports {
        return Ok(bytes.to_vec());
    }
    let mut module = wasm_encoder::Module::new();
    ExposeMemory.parse_core_module(&mut module, Parser::new(0), bytes)?;
    Ok(module.finish())
}

struct ExposeMemory;

impl Reencode for ExposeMemory {
    type Error = std::convert::Infallible;

    fn parse_export_section(
        &mut self,
        exports: &mut wasm_encoder::ExportSection,
        section: wasmparser::ExportSectionReader<'_>,
    ) -> Result<(), reencode::Error<Self::Error>> {
        reencode::utils::parse_export_section(self, exports, section)?;
        // An imported memory is memory 0.
        exports.export("memory", ExportKind::Memory, 0);
        Ok(())
    }
}

/// What a side module exports its thread-local base as. See
/// [`expose_tls_base`].
pub const TLS_BASE_EXPORT: &str = "__ash_tls_base";

/// The module with its thread-local base exported as [`TLS_BASE_EXPORT`].
///
/// A shared library built with thread-local storage has no `__wasm_init_tls`
/// for a loader to call. Its start function points the module's own mutable
/// `__tls_base` global at a block inside the library's memory and sets a flag,
/// and an instance started after that finds the flag set and leaves
/// `__tls_base` at zero. Exporting the global lets the loader read the first
/// instance's block and give every other instance a block of its own.
///
/// The global is the one the start function assigns: the only mutable `i32`
/// global the module defines that it sets there. A module whose start function
/// sets no such global, or several, has no thread-local base to find and is
/// returned unchanged.
pub fn expose_tls_base(bytes: &[u8]) -> Result<Vec<u8>> {
    let (mut imported_functions, mut imported_globals) = (0u32, 0u32);
    let mut start = None;
    let mut mutable_i32: Vec<u32> = Vec::new();
    let mut set_in_start: Vec<u32> = Vec::new();
    let mut body = 0u32;
    let mut exported = false;
    for payload in Parser::new(0).parse_all(bytes) {
        match payload? {
            Payload::ImportSection(section) => {
                for import in section.into_imports() {
                    match import?.ty {
                        TypeRef::Func(_) | TypeRef::FuncExact(_) => imported_functions += 1,
                        TypeRef::Global(_) => imported_globals += 1,
                        _ => {}
                    }
                }
            }
            Payload::GlobalSection(section) => {
                for (i, global) in section.into_iter().enumerate() {
                    let ty = global?.ty;
                    if ty.mutable && ty.content_type == wasmparser::ValType::I32 {
                        mutable_i32.push(imported_globals + i as u32);
                    }
                }
            }
            Payload::ExportSection(section) => {
                for export in section {
                    exported |= export?.name == TLS_BASE_EXPORT;
                }
            }
            Payload::StartSection { func, .. } => start = Some(func),
            Payload::CodeSectionEntry(function) => {
                if start == Some(imported_functions + body) {
                    for op in function.get_operators_reader()? {
                        if let wasmparser::Operator::GlobalSet { global_index } = op? {
                            set_in_start.push(global_index);
                        }
                    }
                }
                body += 1;
            }
            _ => {}
        }
    }
    set_in_start.sort_unstable();
    set_in_start.dedup();
    let candidates: Vec<u32> = set_in_start
        .into_iter()
        .filter(|g| mutable_i32.contains(g))
        .collect();
    let [global] = candidates[..] else {
        return Ok(bytes.to_vec());
    };
    if exported {
        return Ok(bytes.to_vec());
    }
    let mut module = wasm_encoder::Module::new();
    ExposeTlsBase { global }.parse_core_module(&mut module, Parser::new(0), bytes)?;
    Ok(module.finish())
}

struct ExposeTlsBase {
    global: u32,
}

impl Reencode for ExposeTlsBase {
    type Error = std::convert::Infallible;

    fn parse_export_section(
        &mut self,
        exports: &mut wasm_encoder::ExportSection,
        section: wasmparser::ExportSectionReader<'_>,
    ) -> Result<(), reencode::Error<Self::Error>> {
        reencode::utils::parse_export_section(self, exports, section)?;
        exports.export(TLS_BASE_EXPORT, ExportKind::Global, self.global);
        Ok(())
    }
}

/// The module without its initial data.
///
/// A second instance of a side module -- a thread's -- shares the memory the
/// first instance already filled, and instantiating runs every active data
/// segment again: the library's statics would be reset to their initial
/// values under a thread that is running. Each active segment is replaced by
/// an empty passive one, so segment numbers, and any `memory.init` naming
/// them, are unchanged.
pub fn without_initial_data(bytes: &[u8]) -> Result<Vec<u8>> {
    let mut module = wasm_encoder::Module::new();
    WithoutInitialData.parse_core_module(&mut module, Parser::new(0), bytes)?;
    Ok(module.finish())
}

struct WithoutInitialData;

impl Reencode for WithoutInitialData {
    type Error = std::convert::Infallible;

    fn parse_data_section(
        &mut self,
        data: &mut wasm_encoder::DataSection,
        section: wasmparser::DataSectionReader<'_>,
    ) -> Result<(), reencode::Error<Self::Error>> {
        for segment in section {
            let segment = segment?;
            if matches!(segment.kind, wasmparser::DataKind::Active { .. }) {
                data.passive([]);
            } else {
                self.parse_data(data, segment)?;
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The global the start function sets is exported; a module whose start
    /// function sets none is left as it was.
    #[test]
    fn the_global_the_start_function_assigns_is_exported() {
        use wasm_encoder::{
            CodeSection, ConstExpr, ExportSection, Function, FunctionSection, GlobalSection,
            GlobalType, Instruction, Module, StartSection, TypeSection, ValType,
        };
        let build = |sets: bool| {
            let mut module = Module::new();
            let mut types = TypeSection::new();
            types.ty().function([], []);
            module.section(&types);
            let mut functions = FunctionSection::new();
            functions.function(0);
            module.section(&functions);
            let mut globals = GlobalSection::new();
            let ty = GlobalType {
                val_type: ValType::I32,
                mutable: true,
                shared: false,
            };
            globals.global(ty, &ConstExpr::i32_const(0));
            globals.global(ty, &ConstExpr::i32_const(0));
            module.section(&globals);
            let mut exports = ExportSection::new();
            exports.export("start", ExportKind::Func, 0);
            module.section(&exports);
            module.section(&StartSection { function_index: 0 });
            let mut code = CodeSection::new();
            let mut start = Function::new([]);
            if sets {
                start.instruction(&Instruction::I32Const(64));
                start.instruction(&Instruction::GlobalSet(1));
            }
            start.instruction(&Instruction::End);
            code.function(&start);
            module.section(&code);
            module.finish()
        };
        let exported = |bytes: &[u8]| -> Vec<(String, u32)> {
            let mut out = Vec::new();
            for payload in Parser::new(0).parse_all(bytes) {
                if let Payload::ExportSection(section) = payload.unwrap() {
                    for export in section {
                        let export = export.unwrap();
                        out.push((export.name.to_string(), export.index));
                    }
                }
            }
            out
        };
        let with = expose_tls_base(&build(true)).unwrap();
        assert_eq!(
            exported(&with),
            [("start".to_string(), 0), (TLS_BASE_EXPORT.to_string(), 1)]
        );
        let without = build(false);
        assert_eq!(expose_tls_base(&without).unwrap(), without);
    }

    /// Active segments become empty passive ones; passive ones are kept, and
    /// the numbering does not move.
    #[test]
    fn initial_data_is_dropped_without_renumbering() {
        use wasm_encoder::{
            ConstExpr, DataCountSection, DataSection, EntityType, ImportSection, MemoryType, Module,
        };
        let mut module = Module::new();
        let mut imports = ImportSection::new();
        imports.import(
            "env",
            "memory",
            EntityType::Memory(MemoryType {
                minimum: 1,
                maximum: None,
                memory64: false,
                shared: false,
                page_size_log2: None,
            }),
        );
        module.section(&imports);
        module.section(&DataCountSection { count: 2 });
        let mut data = DataSection::new();
        data.active(0, &ConstExpr::i32_const(8), [1, 2, 3, 4]);
        data.passive([9, 9]);
        module.section(&data);

        let stripped = without_initial_data(&module.finish()).unwrap();
        let mut kinds = Vec::new();
        for payload in Parser::new(0).parse_all(&stripped) {
            if let Payload::DataSection(section) = payload.unwrap() {
                for segment in section {
                    let segment = segment.unwrap();
                    kinds.push((
                        matches!(segment.kind, wasmparser::DataKind::Passive),
                        segment.data.to_vec(),
                    ));
                }
            }
        }
        assert_eq!(kinds, [(true, vec![]), (true, vec![9, 9])]);
    }

    /// A module without the section is not a side module, and saying so is
    /// not an error.
    #[test]
    fn a_plain_module_is_not_a_side_module() {
        let empty = b"\0asm\x01\0\0\0";
        assert_eq!(read_side_module(empty).unwrap(), None);
    }

    /// The host's `ash_host_*` imports are kept apart from the program's.
    #[test]
    fn host_functions_are_listed_apart_from_the_programs() {
        use wasm_encoder::{
            CustomSection, EntityType, FunctionSection, ImportSection, Module, TypeSection,
        };
        let mut module = Module::new();
        module.section(&CustomSection {
            name: "dylink.0".into(),
            data: [WASM_DYLINK_MEM_INFO, 4, 16, 2, 1, 0][..].into(),
        });
        let mut types = TypeSection::new();
        types.ty().function([], []);
        module.section(&types);
        let mut imports = ImportSection::new();
        for name in ["hlp_alloc_bytes", "ash_host_watch", "ash_host_agent"] {
            imports.import("env", name, EntityType::Function(0));
        }
        module.section(&imports);
        module.section(&FunctionSection::new());
        let side = read_side_module(&module.finish()).unwrap().unwrap();
        assert_eq!(side.functions, ["hlp_alloc_bytes"]);
        assert_eq!(side.host_functions, ["ash_host_agent", "ash_host_watch"]);
        assert_eq!((side.memory_size, side.memory_align, side.table_size), (16, 2, 1));
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
