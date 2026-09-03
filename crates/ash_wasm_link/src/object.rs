//! Reading one relocatable WebAssembly object.
//!
//! A relocatable object is not a module and cannot be instantiated: it
//! imports `env.__linear_memory` instead of owning memory, its function
//! indices are placeholders, and it carries two custom sections that say so
//! -- `linking`, holding the symbol table, and `reloc.*`, holding the list of
//! places whose contents are not final.
//!
//! # Why the payload is kept whole
//!
//! Every relocation names a byte offset into a section and a width, and every
//! width is fixed: the `_LEB` and `_SLEB` forms are five-byte encodings
//! padded to that size precisely so a linker can overwrite them without
//! moving anything, and the `_I32` forms are four raw bytes. So this reader
//! keeps each relocated section's payload as one contiguous `Vec<u8>` and
//! patches it in place at the offsets the relocation records give.
//!
//! The alternative -- parsing bodies out first and recomputing where each
//! relocation landed inside its body -- introduces exactly one arithmetic
//! step, and that step is the one that silently corrupts a module. Bodies are
//! split out *after* patching, from bytes that are already correct.

use anyhow::{anyhow, bail, Context, Result};
use wasmparser::{
    Data, DataKind, Element, ElementItems, ElementKind, Export, FuncType, Global, Import, Linking,
    LinkingSectionReader, MemoryType, Parser, Payload, RelocSectionReader, RelocationEntry,
    SymbolInfo, TableType, TagType, ValType,
};

/// What a symbol points at, once the object-local details are resolved away.
#[derive(Debug, Clone)]
pub enum SymbolTarget {
    /// A function, by index in this object's function index space.
    Function { index: u32 },
    /// Data, by segment and offset within this object.
    Data {
        segment: u32,
        offset: u32,
        size: u32,
    },
    /// A global, by index in this object's global index space.
    Global { index: u32 },
    /// A table, by index in this object's table index space.
    Table { index: u32 },
    /// A tag, by index in this object's tag index space.
    Tag { index: u32 },
    /// A section symbol. Only referenced by the debug relocations, which this
    /// linker drops, but it still occupies a symbol index.
    Section { index: u32 },
    /// Named but not defined here.
    Undefined,
}

/// One entry of the object's symbol table.
#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub target: SymbolTarget,
    pub flags: u32,
}

impl Symbol {
    /// Bit 0 of the flags. A weak definition loses to a strong one.
    pub fn is_weak(&self) -> bool {
        self.flags & 0x1 != 0
    }
    /// Bit 2. A local symbol is not visible to other objects at all, so two
    /// objects may each have one of the same name without conflicting.
    pub fn is_local(&self) -> bool {
        self.flags & 0x4 != 0
    }
    /// Bit 4. Undefined symbols name something another object must define.
    pub fn is_undefined(&self) -> bool {
        self.flags & 0x10 != 0
    }
    /// Bit 6. The symbol is exported from the linked module.
    pub fn is_exported(&self) -> bool {
        self.flags & 0x40 != 0
    }
    /// Bit 7. The symbol carries an explicit name for its import.
    pub fn has_explicit_name(&self) -> bool {
        self.flags & 0x80 != 0
    }
    /// Bit 8. Do not remove, even if nothing references it.
    pub fn is_no_strip(&self) -> bool {
        self.flags & 0x100 != 0
    }
}

/// A data segment's name and alignment, from the `WASM_SEGMENT_INFO`
/// subsection. Segments are merged by name prefix, and the alignment is the
/// one thing that cannot be guessed.
#[derive(Debug, Clone)]
pub struct SegmentInfo {
    pub name: String,
    pub align_log2: u32,
    pub flags: u32,
}

/// An import, with the shape needed to decide whether it survives.
#[derive(Debug, Clone)]
pub struct ObjImport {
    pub module: String,
    pub name: String,
    pub kind: ImportKind,
}

#[derive(Debug, Clone)]
pub enum ImportKind {
    Function { type_index: u32 },
    Table(TableType),
    Memory(MemoryType),
    Global { ty: ValType, mutable: bool },
    Tag { type_index: u32 },
}

/// A defined global: its type and the raw bytes of its init expression.
#[derive(Debug, Clone)]
pub struct ObjGlobal {
    pub ty: ValType,
    pub mutable: bool,
    /// The init expression as written, still holding any relocated index.
    pub init: Vec<u8>,
}

/// A data segment as it appears in the object: always passive or active at a
/// placeholder offset, since an object has no say over where it lands.
#[derive(Debug, Clone)]
pub struct ObjData {
    /// Byte range of this segment's contents within the data section payload,
    /// so patches applied to the payload are visible here without copying.
    pub range: std::ops::Range<usize>,
    pub passive: bool,
}

/// One parsed object.
pub struct Object {
    pub name: String,
    pub types: Vec<FuncType>,
    pub imports: Vec<ObjImport>,
    /// Type index per locally defined function, in order.
    pub functions: Vec<u32>,
    pub tables: Vec<TableType>,
    pub memories: Vec<MemoryType>,
    pub globals: Vec<ObjGlobal>,
    pub tags: Vec<u32>,
    pub exports: Vec<(String, wasmparser::ExternalKind, u32)>,
    /// Function indices named by the object's element segments, which is how
    /// a function that has its address taken reaches the table.
    pub elements: Vec<u32>,
    /// The entire code section payload, patched in place.
    pub code_payload: Vec<u8>,
    /// Byte range of each function body within `code_payload`, in order.
    pub code_bodies: Vec<std::ops::Range<usize>>,
    /// The entire data section payload, patched in place.
    pub data_payload: Vec<u8>,
    pub data_segments: Vec<ObjData>,
    pub symbols: Vec<Symbol>,
    pub segment_info: Vec<SegmentInfo>,
    pub code_relocs: Vec<RelocationEntry>,
    pub data_relocs: Vec<RelocationEntry>,
    /// Constructors, by symbol index, to be run before `main` in ascending
    /// priority order.
    pub init_funcs: Vec<InitFunc>,
}

/// One entry of `WASM_INIT_FUNCS`.
#[derive(Debug, Clone)]
pub struct InitFunc {
    pub priority: u32,
    pub symbol: u32,
}

impl Object {
    /// How many functions this object's function index space holds: imported
    /// first, then defined. Every `FUNCTION_INDEX_LEB` in it is an index into
    /// this space.
    pub fn imported_functions(&self) -> u32 {
        self.imports
            .iter()
            .filter(|i| matches!(i.kind, ImportKind::Function { .. }))
            .count() as u32
    }

    pub fn imported_globals(&self) -> u32 {
        self.imports
            .iter()
            .filter(|i| matches!(i.kind, ImportKind::Global { .. }))
            .count() as u32
    }

    pub fn imported_tables(&self) -> u32 {
        self.imports
            .iter()
            .filter(|i| matches!(i.kind, ImportKind::Table(_)))
            .count() as u32
    }

    pub fn imported_tags(&self) -> u32 {
        self.imports
            .iter()
            .filter(|i| matches!(i.kind, ImportKind::Tag { .. }))
            .count() as u32
    }
}

/// Parse one relocatable object.
pub fn read(name: &str, bytes: &[u8]) -> Result<Object> {
    let mut obj = Object {
        name: name.to_string(),
        types: Vec::new(),
        imports: Vec::new(),
        functions: Vec::new(),
        tables: Vec::new(),
        memories: Vec::new(),
        globals: Vec::new(),
        tags: Vec::new(),
        exports: Vec::new(),
        elements: Vec::new(),
        code_payload: Vec::new(),
        code_bodies: Vec::new(),
        data_payload: Vec::new(),
        data_segments: Vec::new(),
        symbols: Vec::new(),
        segment_info: Vec::new(),
        code_relocs: Vec::new(),
        data_relocs: Vec::new(),
        init_funcs: Vec::new(),
    };

    // Section indices as the object counts them, because `reloc.*` names its
    // target section by index and the debug sections must be told apart from
    // CODE and DATA by that number.
    let mut section_index: u32 = 0;
    let mut code_section_index: Option<u32> = None;
    let mut data_section_index: Option<u32> = None;
    let mut code_payload_start: usize = 0;
    #[allow(unused_assignments)]
    let mut data_payload_start: usize = 0;
    // reloc sections may appear before or after what they describe, so they
    // are collected and matched at the end.
    let mut pending_relocs: Vec<(u32, Vec<RelocationEntry>)> = Vec::new();

    for payload in Parser::new(0).parse_all(bytes) {
        let payload = payload.with_context(|| format!("parsing {name}"))?;
        match payload {
            Payload::Version { .. } => {}
            Payload::TypeSection(reader) => {
                section_index += 1;
                for group in reader {
                    for ty in group?.into_types() {
                        let composite = ty.composite_type;
                        match composite.inner {
                            wasmparser::CompositeInnerType::Func(f) => obj.types.push(f),
                            _ => bail!("{name}: only function types are supported"),
                        }
                    }
                }
            }
            Payload::ImportSection(reader) => {
                section_index += 1;
                for import in reader.into_imports() {
                    obj.imports.push(convert_import(import?)?);
                }
            }
            Payload::FunctionSection(reader) => {
                section_index += 1;
                for ty in reader {
                    obj.functions.push(ty?);
                }
            }
            Payload::TableSection(reader) => {
                section_index += 1;
                for table in reader {
                    obj.tables.push(table?.ty);
                }
            }
            Payload::MemorySection(reader) => {
                section_index += 1;
                for memory in reader {
                    obj.memories.push(memory?);
                }
            }
            Payload::TagSection(reader) => {
                section_index += 1;
                for tag in reader {
                    let TagType { func_type_idx, .. } = tag?;
                    obj.tags.push(func_type_idx);
                }
            }
            Payload::GlobalSection(reader) => {
                section_index += 1;
                for global in reader {
                    obj.globals.push(convert_global(bytes, global?)?);
                }
            }
            Payload::ExportSection(reader) => {
                section_index += 1;
                for export in reader {
                    let Export { name, kind, index } = export?;
                    obj.exports.push((name.to_string(), kind, index));
                }
            }
            Payload::ElementSection(reader) => {
                section_index += 1;
                for element in reader {
                    collect_element(element?, &mut obj.elements)?;
                }
            }
            Payload::DataCountSection { .. } => {
                section_index += 1;
            }
            Payload::CodeSectionStart { range, .. } => {
                code_section_index = Some(section_index);
                section_index += 1;
                // `range` covers the section's contents, which is what a
                // relocation offset is measured from.
                code_payload_start = range.start as usize;
                obj.code_payload = bytes[range.start as usize..range.end as usize].to_vec();
            }
            Payload::CodeSectionEntry(body) => {
                let r = body.range();
                obj.code_bodies.push(
                    r.start as usize - code_payload_start..r.end as usize - code_payload_start,
                );
            }
            Payload::DataSection(reader) => {
                data_section_index = Some(section_index);
                section_index += 1;
                let range = reader.range();
                data_payload_start = range.start as usize;
                obj.data_payload = bytes[range.start as usize..range.end as usize].to_vec();
                for data in reader {
                    let Data { kind, data: d, .. } = data?;
                    // Segment contents are located by where they sit in the
                    // payload, so a patch to the payload is a patch to the
                    // segment with no second copy to keep in step.
                    let start = d.as_ptr() as usize - bytes.as_ptr() as usize;
                    obj.data_segments.push(ObjData {
                        range: start - data_payload_start..start - data_payload_start + d.len(),
                        passive: matches!(kind, DataKind::Passive),
                    });
                }
            }
            Payload::CustomSection(section) => {
                let name_of = section.name();
                if name_of == "linking" {
                    read_linking(&mut obj, &section)?;
                    section_index += 1;
                } else if let Some(target) = name_of.strip_prefix("reloc.") {
                    let _ = target;
                    let reader = RelocSectionReader::new(section.data_reader())
                        .with_context(|| format!("{name}: reading {name_of}"))?;
                    let target_section = reader.section_index();
                    let entries: Vec<RelocationEntry> =
                        reader.entries().into_iter().collect::<Result<_, _>>()?;
                    pending_relocs.push((target_section, entries));
                    section_index += 1;
                } else {
                    section_index += 1;
                }
            }
            _ => {
                section_index += 1;
            }
        }
    }

    for (target, entries) in pending_relocs {
        if Some(target) == code_section_index {
            obj.code_relocs = entries;
        } else if Some(target) == data_section_index {
            obj.data_relocs = entries;
        }
        // Anything else targets a debug section, which this linker drops.
    }

    Ok(obj)
}

fn convert_import(import: Import<'_>) -> Result<ObjImport> {
    use wasmparser::TypeRef;
    let kind = match import.ty {
        TypeRef::Func(type_index) => ImportKind::Function { type_index },
        TypeRef::Table(t) => ImportKind::Table(t),
        TypeRef::Memory(m) => ImportKind::Memory(m),
        TypeRef::Global(g) => ImportKind::Global {
            ty: g.content_type,
            mutable: g.mutable,
        },
        TypeRef::Tag(t) => ImportKind::Tag {
            type_index: t.func_type_idx,
        },
        // An exact-type import is a typed-function-references feature; ash
        // emits none, and guessing at one would be worse than refusing.
        other => bail!("unsupported import kind: {other:?}"),
    };
    Ok(ObjImport {
        module: import.module.to_string(),
        name: import.name.to_string(),
        kind,
    })
}

fn convert_global(bytes: &[u8], global: Global<'_>) -> Result<ObjGlobal> {
    let range = global.init_expr.get_binary_reader().range();
    Ok(ObjGlobal {
        ty: global.ty.content_type,
        mutable: global.ty.mutable,
        init: bytes[range.start as usize..range.end as usize].to_vec(),
    })
}

fn collect_element(element: Element<'_>, out: &mut Vec<u32>) -> Result<()> {
    // An object's element segment exists to say "these functions need table
    // slots". Where they land is the linker's business, so only the function
    // list is kept.
    match element.kind {
        ElementKind::Active { .. } | ElementKind::Passive | ElementKind::Declared => {}
    }
    match element.items {
        ElementItems::Functions(reader) => {
            for f in reader {
                out.push(f?);
            }
        }
        ElementItems::Expressions(_, _) => {
            bail!("element segments of expressions are not supported")
        }
    }
    Ok(())
}

fn read_linking(obj: &mut Object, section: &wasmparser::CustomSectionReader<'_>) -> Result<()> {
    let reader = LinkingSectionReader::new(section.data_reader())
        .with_context(|| format!("{}: reading the linking section", obj.name))?;
    for subsection in reader.subsections() {
        match subsection? {
            Linking::SymbolTable(map) => {
                for info in map {
                    obj.symbols.push(convert_symbol(info?)?);
                }
            }
            Linking::SegmentInfo(map) => {
                for segment in map {
                    let s = segment?;
                    obj.segment_info.push(SegmentInfo {
                        name: s.name.to_string(),
                        align_log2: s.alignment,
                        flags: s.flags.bits(),
                    });
                }
            }
            // Constructors: the runtime's own and libc's, which must run
            // before main. A prelinked runtime object has real ones, so the
            // linker synthesises `__wasm_call_ctors` from them; dropping them
            // would leave a program whose allocator was never initialised.
            Linking::InitFuncs(map) => {
                for func in map {
                    let f = func?;
                    obj.init_funcs.push(InitFunc {
                        priority: f.priority,
                        symbol: f.symbol_index,
                    });
                }
            }
            Linking::ComdatInfo(map) => {
                if map.into_iter().next().is_some() {
                    bail!(
                        "{}: has COMDAT groups, which this linker does not deduplicate",
                        obj.name
                    );
                }
            }
            // A subsection this wasmparser does not know. Refusing is the
            // safe reading: it may carry something a correct link depends on.
            Linking::Unknown { ty, .. } => {
                bail!("{}: unknown linking subsection {ty}", obj.name);
            }
        }
    }
    Ok(())
}

fn convert_symbol(info: SymbolInfo<'_>) -> Result<Symbol> {
    let (name, target, flags) = match info {
        SymbolInfo::Func { flags, index, name } => (
            name.map(str::to_string),
            SymbolTarget::Function { index },
            flags,
        ),
        SymbolInfo::Data {
            flags,
            name,
            symbol,
        } => {
            let target = match symbol {
                Some(d) => SymbolTarget::Data {
                    segment: d.index,
                    offset: d.offset,
                    size: d.size,
                },
                None => SymbolTarget::Undefined,
            };
            (Some(name.to_string()), target, flags)
        }
        SymbolInfo::Global { flags, index, name } => (
            name.map(str::to_string),
            SymbolTarget::Global { index },
            flags,
        ),
        SymbolInfo::Table { flags, index, name } => (
            name.map(str::to_string),
            SymbolTarget::Table { index },
            flags,
        ),
        SymbolInfo::Event { flags, index, name } => {
            (name.map(str::to_string), SymbolTarget::Tag { index }, flags)
        }
        SymbolInfo::Section { flags, section } => (
            None,
            SymbolTarget::Section {
                index: section,
            },
            flags,
        ),
    };
    let flags = flags.bits();
    let mut symbol = Symbol {
        name: name.unwrap_or_default(),
        target,
        flags,
    };
    // An undefined symbol's target is whatever index space it names, but it
    // defines nothing; saying so once here keeps every later test simple.
    if symbol.is_undefined() {
        symbol.target = SymbolTarget::Undefined;
    }
    Ok(symbol)
}

/// Read a five-byte LEB at `offset`, for checking that a patch site holds
/// what the relocation record says it should.
pub fn peek_u32_leb5(buf: &[u8], offset: usize) -> Result<u32> {
    let slice = buf
        .get(offset..offset + 5)
        .ok_or_else(|| anyhow!("relocation at {offset} is past the end of its section"))?;
    let mut value: u32 = 0;
    for (i, byte) in slice.iter().enumerate() {
        value |= ((byte & 0x7f) as u32) << (7 * i);
    }
    Ok(value)
}
