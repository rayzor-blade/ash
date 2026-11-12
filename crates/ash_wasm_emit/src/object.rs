//! A relocatable wasm object, in the layout `wasm-ld` and `ash_wasm_link` read:
//! type, import, function and code sections, then a `linking` section holding
//! the symbol table and a `reloc.CODE` section naming every site the linker
//! must patch.
//!
//! Functions are declared first, so that a body can call one defined later;
//! bodies are set afterwards. A call to a symbol is written with a 5-byte
//! padded LEB, which the linker rewrites in place, and recorded as a site.
//! Until the object is finished the index in that LEB is a placeholder: the
//! function index space puts imports first, so a defined function's index is
//! not known until every import has been declared.
#![forbid(unsafe_code)]

use std::collections::HashMap;
use wasm_encoder::{self as we, Instruction, ValType};

/// A symbol of the object: a defined function or an undefined (imported) one.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Sym(pub u32);

const FUNCTION_INDEX_LEB: u8 = 0;
const TABLE_INDEX_SLEB: u8 = 1;
const TABLE_INDEX_I32: u8 = 2;
const MEMORY_ADDR_SLEB: u8 = 4;
const MEMORY_ADDR_I32: u8 = 5;
const TYPE_INDEX_LEB: u8 = 6;
const GLOBAL_INDEX_LEB: u8 = 7;
const TAG_INDEX_LEB: u8 = 10;
const TABLE_NUMBER_LEB: u8 = 20;

const FLAG_LOCAL: u32 = 0x02;
const FLAG_HIDDEN: u32 = 0x04;
const FLAG_UNDEFINED: u32 = 0x10;

#[derive(Clone)]
struct SymbolInfo {
    name: String,
    /// Index into `imports` for an undefined function, into `defined` for a
    /// defined one.
    slot: Slot,
    flags: u32,
}

#[derive(Clone, Copy)]
enum Slot {
    Import(usize),
    Defined(usize),
    /// Data another object defines, named but not placed.
    UndefinedData,
    /// Data this object places, at `offset` in its one data segment.
    DefinedData { offset: u32, size: u32 },
    /// The function table the object imports.
    Table,
    /// The stack pointer global the object imports.
    StackPointer,
    /// The tag a long jump is thrown with, which the object imports.
    LongJmpTag,
}

/// Who can see a defined function's name.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Linkage {
    /// Only this object.
    Local,
    /// Other objects, but not the final module's exports.
    Hidden,
    /// Other objects and the final module's exports.
    Default,
}

/// What a patch site holds the index or address of.
#[derive(Clone, Copy)]
enum Target {
    Function(Sym),
    /// The address of a data symbol plus an addend.
    Address(Sym, i32),
    /// The slot of a function in the function table, which is what a function
    /// pointer is.
    TableSlot(Sym),
    /// The type index of the signature at this position of the body's list.
    Signature(usize),
    /// The function table, for `call_indirect`.
    Table,
    /// The stack pointer global.
    StackPointer,
    /// The size of the body's frame, known when the body is finished.
    FrameSize,
    /// The tag a long jump is thrown with.
    LongJmpTag,
}

/// What a reference stored in data points at.
#[derive(Clone, Copy, Debug)]
pub enum DataRef {
    /// The address of a data symbol, plus an addend.
    Address(Sym, i32),
    /// A function pointer.
    Function(Sym),
}

struct Import {
    name: String,
    ty: u32,
}

struct Defined {
    ty: u32,
    body: Option<Body>,
}

/// A function body under construction: the encoded instructions, and where in
/// them a symbol's index is written.
pub struct Body {
    func: we::Function,
    /// Byte offset within the function's own bytes of each 5-byte index, and
    /// the symbol it names.
    sites: Vec<(usize, Target)>,
    /// Signatures of the body's indirect calls.
    sigs: Vec<(Vec<ValType>, Vec<ValType>)>,
    frame_size: u32,
}

impl Body {
    pub fn new(locals: Vec<(u32, ValType)>) -> Self {
        Body {
            func: we::Function::new(locals),
            sites: Vec::new(),
            sigs: Vec::new(),
            frame_size: 0,
        }
    }

    /// `i32.const` of the frame's size, which is not known until the body is
    /// done.
    pub fn frame_size_const(&mut self) {
        self.site(0x41, Target::FrameSize);
    }

    pub fn set_frame_size(&mut self, bytes: u32) {
        self.frame_size = bytes;
    }

    /// An opcode followed by a 5-byte index the linker rewrites.
    fn site(&mut self, opcode: u8, target: Target) {
        self.func.raw([opcode]);
        let at = self.func.byte_len();
        self.func.raw([0x80, 0x80, 0x80, 0x80, 0x00]);
        self.sites.push((at, target));
    }

    /// `call_indirect` through the function table, to a function of this
    /// signature. The callee's index is on the stack above its arguments.
    pub fn call_indirect(&mut self, params: Vec<ValType>, results: Vec<ValType>) {
        let sig = match self.sigs.iter().position(|s| s.0 == params && s.1 == results) {
            Some(i) => i,
            None => {
                self.sigs.push((params, results));
                self.sigs.len() - 1
            }
        };
        self.site(0x11, Target::Signature(sig));
        // The table operand follows the type, with no opcode of its own.
        let at = self.func.byte_len();
        self.func.raw([0x80, 0x80, 0x80, 0x80, 0x00]);
        self.sites.push((at, Target::Table));
    }

    /// `i32.const` of a function's slot in the function table.
    pub fn function_pointer(&mut self, sym: Sym) {
        self.site(0x41, Target::TableSlot(sym));
    }

    /// `try_table` with one clause: a long jump thrown inside it branches to
    /// `label`, counted from outside the block, with the thrown pointer on
    /// the stack. The block is closed with `end`.
    pub fn try_long_jump(&mut self, label: u32) {
        self.func.raw([0x1f, 0x40, 0x01, 0x00]);
        let at = self.func.byte_len();
        self.func.raw([0x80, 0x80, 0x80, 0x80, 0x00]);
        self.sites.push((at, Target::LongJmpTag));
        let mut bytes = Vec::new();
        leb_u32(&mut bytes, label);
        self.func.raw(bytes);
    }

    /// `global.get` of the stack pointer.
    pub fn stack_pointer_get(&mut self) {
        self.site(0x23, Target::StackPointer);
    }

    /// `global.set` of the stack pointer.
    pub fn stack_pointer_set(&mut self) {
        self.site(0x24, Target::StackPointer);
    }

    pub fn instruction(&mut self, i: &Instruction) {
        self.func.instruction(i);
    }

    /// The finished function, for a module that numbers its own functions. A
    /// body that calls by symbol belongs in an object instead.
    pub fn into_function(self) -> we::Function {
        assert!(self.sites.is_empty(), "a body with symbol calls is not module-ready");
        self.func
    }

    /// `call sym`, with an index the linker resolves.
    pub fn call(&mut self, sym: Sym) {
        self.site(0x10, Target::Function(sym));
    }

    /// `i32.const` of a data symbol's address plus `addend`, which the linker
    /// fills in.
    pub fn address_of(&mut self, sym: Sym, addend: i32) {
        self.site(0x41, Target::Address(sym, addend));
    }
}

#[derive(Default)]
pub struct ObjectBuilder {
    types: Vec<(Vec<ValType>, Vec<ValType>)>,
    type_index: HashMap<(Vec<ValType>, Vec<ValType>), u32>,
    imports: Vec<Import>,
    defined: Vec<Defined>,
    symbols: Vec<SymbolInfo>,
    by_name: HashMap<String, Sym>,
    memory_min: Option<u64>,
    data_align: u32,
    /// The one data segment, with every defined data symbol laid out in it.
    data: Vec<u8>,
    /// Sites in `data` that hold an address or function pointer, as
    /// (offset in `data`, what it points at).
    data_relocs: Vec<(u32, DataRef)>,
}

impl ObjectBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    fn ty(&mut self, params: Vec<ValType>, results: Vec<ValType>) -> u32 {
        let key = (params, results);
        if let Some(&t) = self.type_index.get(&key) {
            return t;
        }
        let t = self.types.len() as u32;
        self.types.push(key.clone());
        self.type_index.insert(key, t);
        t
    }

    /// Import `env.__linear_memory`, which code that loads or stores needs.
    pub fn import_memory(&mut self, min_pages: u64) {
        self.memory_min = Some(min_pages);
    }

    /// A function another object defines. Asking twice for one name gives the
    /// same symbol.
    pub fn import_function(&mut self, name: &str, params: Vec<ValType>, results: Vec<ValType>) -> Sym {
        if let Some(&s) = self.by_name.get(name) {
            return s;
        }
        let ty = self.ty(params, results);
        self.imports.push(Import {
            name: name.to_string(),
            ty,
        });
        let sym = Sym(self.symbols.len() as u32);
        self.symbols.push(SymbolInfo {
            name: name.to_string(),
            slot: Slot::Import(self.imports.len() - 1),
            flags: FLAG_UNDEFINED,
        });
        self.by_name.insert(name.to_string(), sym);
        sym
    }

    /// Data another object defines, to take the address of.
    pub fn import_data(&mut self, name: &str) -> Sym {
        if let Some(&s) = self.by_name.get(name) {
            return s;
        }
        let sym = Sym(self.symbols.len() as u32);
        self.symbols.push(SymbolInfo {
            name: name.to_string(),
            slot: Slot::UndefinedData,
            flags: FLAG_UNDEFINED,
        });
        self.by_name.insert(name.to_string(), sym);
        sym
    }

    /// A function this object defines.
    pub fn declare_function(
        &mut self,
        name: &str,
        params: Vec<ValType>,
        results: Vec<ValType>,
        linkage: Linkage,
    ) -> Sym {
        let ty = self.ty(params, results);
        self.defined.push(Defined { ty, body: None });
        let sym = Sym(self.symbols.len() as u32);
        self.symbols.push(SymbolInfo {
            name: name.to_string(),
            slot: Slot::Defined(self.defined.len() - 1),
            flags: match linkage {
                Linkage::Local => FLAG_LOCAL,
                Linkage::Hidden => FLAG_HIDDEN,
                Linkage::Default => 0,
            },
        });
        self.by_name.insert(name.to_string(), sym);
        sym
    }

    pub fn set_body(&mut self, sym: Sym, body: Body) {
        match self.symbols[sym.0 as usize].slot {
            Slot::Defined(i) => self.defined[i].body = Some(body),
            _ => panic!("set_body on a function this object does not define"),
        }
    }

    /// Reserve `size` bytes of data under `name`, aligned to `1 << align_log2`,
    /// so that other data and code can refer to it before its contents are
    /// known.
    ///
    /// A symbol this object already named as data another object defines
    /// becomes the definition, so code written before the data was can have
    /// asked for it by name.
    pub fn declare_data(&mut self, name: &str, size: u32, align_log2: u32, linkage: Linkage) -> Sym {
        let align = 1usize << align_log2;
        let offset = self.data.len().div_ceil(align) * align;
        self.data.resize(offset + size as usize, 0);
        self.data_align = self.data_align.max(align_log2);
        let slot = Slot::DefinedData {
            offset: offset as u32,
            size,
        };
        let flags = match linkage {
            Linkage::Local => FLAG_LOCAL,
            Linkage::Hidden => FLAG_HIDDEN,
            Linkage::Default => 0,
        };
        if let Some(&sym) = self.by_name.get(name)
            && matches!(self.symbols[sym.0 as usize].slot, Slot::UndefinedData)
        {
            let info = &mut self.symbols[sym.0 as usize];
            info.slot = slot;
            info.flags = flags;
            return sym;
        }
        let sym = Sym(self.symbols.len() as u32);
        self.symbols.push(SymbolInfo {
            name: name.to_string(),
            slot,
            flags,
        });
        self.by_name.insert(name.to_string(), sym);
        sym
    }

    /// The symbol this object has under `name`, defined or not.
    pub fn lookup(&self, name: &str) -> Option<Sym> {
        self.by_name.get(name).copied()
    }

    /// Whether `name` is data this object names and does not define.
    pub fn is_undefined_data(&self, name: &str) -> bool {
        self.by_name
            .get(name)
            .is_some_and(|s| matches!(self.symbols[s.0 as usize].slot, Slot::UndefinedData))
    }

    /// Fill a declared symbol: `bytes` from its start, and the references
    /// among them as (offset within the symbol, what they point at). The
    /// bytes at a reference's offset are its addend for a function pointer,
    /// and ignored for an address, whose addend is in the reference.
    pub fn set_data(&mut self, sym: Sym, bytes: &[u8], refs: &[(u32, DataRef)]) {
        let Slot::DefinedData { offset, size } = self.symbols[sym.0 as usize].slot else {
            panic!("set_data on a symbol this object does not define as data");
        };
        assert!(bytes.len() <= size as usize, "more bytes than were declared");
        let at = offset as usize;
        self.data[at..at + bytes.len()].copy_from_slice(bytes);
        for &(rel, target) in refs {
            assert!(rel + 4 <= size, "a reference past the end of its symbol");
            self.data_relocs.push((offset + rel, target));
        }
    }

    pub fn finish(mut self) -> Vec<u8> {
        let imported = self.imports.len() as u32;
        let function_index: Vec<Option<u32>> = self
            .symbols
            .iter()
            .map(|s| match s.slot {
                Slot::Import(i) => Some(i as u32),
                Slot::Defined(i) => Some(imported + i as u32),
                _ => None,
            })
            .collect();
        let findex = |this: &Self, sym: Sym| -> u32 {
            function_index[sym.0 as usize].unwrap_or_else(|| {
                panic!("{} is data, not a function", this.symbols[sym.0 as usize].name)
            })
        };

        // Symbols the code names without the caller having declared them.
        let mut table_sym: Option<Sym> = None;
        let mut sp_sym: Option<Sym> = None;
        let mut tag_sym: Option<Sym> = None;
        for d in &self.defined {
            let Some(body) = &d.body else { continue };
            for &(_, target) in &body.sites {
                match target {
                    Target::Table if table_sym.is_none() => {
                        table_sym = Some(Sym(self.symbols.len() as u32));
                        self.symbols.push(SymbolInfo {
                            name: "__indirect_function_table".to_string(),
                            slot: Slot::Table,
                            flags: FLAG_UNDEFINED,
                        });
                    }
                    Target::StackPointer if sp_sym.is_none() => {
                        sp_sym = Some(Sym(self.symbols.len() as u32));
                        self.symbols.push(SymbolInfo {
                            name: "__stack_pointer".to_string(),
                            slot: Slot::StackPointer,
                            flags: FLAG_UNDEFINED,
                        });
                    }
                    Target::LongJmpTag if tag_sym.is_none() => {
                        tag_sym = Some(Sym(self.symbols.len() as u32));
                        self.symbols.push(SymbolInfo {
                            name: "__c_longjmp".to_string(),
                            slot: Slot::LongJmpTag,
                            flags: FLAG_UNDEFINED,
                        });
                    }
                    _ => {}
                }
            }
        }
        let tag_type = tag_sym.map(|_| self.ty(vec![ValType::I32], vec![]));

        // Bodies, with their indices written and the relocations located.
        let mut code_payload: Vec<u8> = Vec::new();
        // (type, offset, symbol, addend)
        let mut relocs: Vec<(u8, u32, u32, Option<i32>)> = Vec::new();
        leb_u32(&mut code_payload, self.defined.len() as u32);
        for di in 0..self.defined.len() {
            let body = self.defined[di].body.take().expect("a declared function has no body");
            let sig_types: Vec<u32> = body
                .sigs
                .iter()
                .map(|(p, r)| self.ty(p.clone(), r.clone()))
                .collect();
            let mut bytes = body.func.clone().into_raw_body();
            for &(at, target) in &body.sites {
                match target {
                    Target::Function(sym) => {
                        bytes[at..at + 5].copy_from_slice(&padded_leb(findex(&self, sym)))
                    }
                    Target::Signature(i) => {
                        bytes[at..at + 5].copy_from_slice(&padded_leb(sig_types[i]))
                    }
                    // The linker overwrites these; the placeholder is zero.
                    Target::Address(..)
                    | Target::TableSlot(_)
                    | Target::Table
                    | Target::StackPointer
                    | Target::LongJmpTag => bytes[at..at + 5].copy_from_slice(&padded_leb(0)),
                    Target::FrameSize => {
                        bytes[at..at + 5].copy_from_slice(&padded_leb(body.frame_size))
                    }
                }
            }
            leb_u32(&mut code_payload, bytes.len() as u32);
            let base = code_payload.len();
            for &(at, target) in &body.sites {
                let off = (base + at) as u32;
                match target {
                    Target::Function(sym) => relocs.push((FUNCTION_INDEX_LEB, off, sym.0, None)),
                    Target::Address(sym, addend) => {
                        relocs.push((MEMORY_ADDR_SLEB, off, sym.0, Some(addend)))
                    }
                    Target::TableSlot(sym) => relocs.push((TABLE_INDEX_SLEB, off, sym.0, None)),
                    Target::Signature(i) => relocs.push((TYPE_INDEX_LEB, off, sig_types[i], None)),
                    Target::Table => {
                        relocs.push((TABLE_NUMBER_LEB, off, table_sym.expect("table symbol").0, None))
                    }
                    Target::StackPointer => {
                        relocs.push((GLOBAL_INDEX_LEB, off, sp_sym.expect("stack pointer symbol").0, None))
                    }
                    Target::FrameSize => {}
                    Target::LongJmpTag => {
                        relocs.push((TAG_INDEX_LEB, off, tag_sym.expect("tag symbol").0, None))
                    }
                }
            }
            code_payload.extend_from_slice(&bytes);
        }

        let mut module = we::Module::new();
        let mut types = we::TypeSection::new();
        for (p, r) in &self.types {
            types.ty().function(p.iter().copied(), r.iter().copied());
        }
        module.section(&types);

        let mut sections = 1; // the type section
        if !self.imports.is_empty()
            || self.memory_min.is_some()
            || table_sym.is_some()
            || sp_sym.is_some()
            || tag_sym.is_some()
        {
            let mut imports = we::ImportSection::new();
            if let Some(min) = self.memory_min {
                imports.import(
                    "env",
                    "__linear_memory",
                    we::EntityType::Memory(we::MemoryType {
                        minimum: min,
                        maximum: None,
                        memory64: false,
                        shared: false,
                        page_size_log2: None,
                    }),
                );
            }
            for i in &self.imports {
                imports.import("env", &i.name, we::EntityType::Function(i.ty));
            }
            if table_sym.is_some() {
                imports.import(
                    "env",
                    "__indirect_function_table",
                    we::EntityType::Table(we::TableType {
                        element_type: we::RefType::FUNCREF,
                        minimum: 0,
                        maximum: None,
                        table64: false,
                        shared: false,
                    }),
                );
            }
            if sp_sym.is_some() {
                imports.import(
                    "env",
                    "__stack_pointer",
                    we::EntityType::Global(we::GlobalType {
                        val_type: ValType::I32,
                        mutable: true,
                        shared: false,
                    }),
                );
            }
            if let Some(ty) = tag_type {
                imports.import(
                    "env",
                    "__c_longjmp",
                    we::EntityType::Tag(we::TagType {
                        kind: we::TagKind::Exception,
                        func_type_idx: ty,
                    }),
                );
            }
            module.section(&imports);
            sections += 1;
        }
        let mut funcs = we::FunctionSection::new();
        for d in &self.defined {
            funcs.function(d.ty);
        }
        module.section(&funcs);
        sections += 1;
        let code_section_index = sections;
        module.section(&we::RawSection {
            id: 10,
            data: &code_payload,
        });
        sections += 1;

        // The data section: one active segment at a placeholder address.
        let mut data_section_index = 0;
        let mut data_payload: Vec<u8> = Vec::new();
        let mut data_header = 0usize;
        if !self.data.is_empty() {
            leb_u32(&mut data_payload, 1);
            data_payload.push(0); // active, memory 0
            data_payload.extend_from_slice(&[0x41, 0x00, 0x0b]); // i32.const 0; end
            leb_u32(&mut data_payload, self.data.len() as u32);
            data_header = data_payload.len();
            data_payload.extend_from_slice(&self.data);
            // Each reference is written into place the way the linker will:
            // a function pointer's slot is patched later, so the bytes hold
            // zero; an address holds its addend.
            for &(at, target) in &self.data_relocs {
                if let DataRef::Address(_, addend) = target {
                    let p = data_header + at as usize;
                    data_payload[p..p + 4].copy_from_slice(&addend.to_le_bytes());
                }
            }
            // Its index among the sections is the number written before it.
            data_section_index = sections;
            module.section(&we::RawSection {
                id: 11,
                data: &data_payload,
            });
        }

        // linking: version 2, then the symbol table and the segment.
        let mut linking: Vec<u8> = Vec::new();
        leb_u32(&mut linking, 2);
        let mut table: Vec<u8> = Vec::new();
        leb_u32(&mut table, self.symbols.len() as u32);
        for s in &self.symbols {
            match s.slot {
                Slot::Import(i) => {
                    table.push(0); // a function
                    leb_u32(&mut table, s.flags);
                    leb_u32(&mut table, i as u32);
                }
                Slot::Defined(i) => {
                    table.push(0);
                    leb_u32(&mut table, s.flags);
                    leb_u32(&mut table, imported + i as u32);
                    string(&mut table, &s.name);
                }
                Slot::UndefinedData => {
                    table.push(1); // data
                    leb_u32(&mut table, s.flags);
                    string(&mut table, &s.name);
                }
                Slot::DefinedData { offset, size } => {
                    table.push(1);
                    leb_u32(&mut table, s.flags);
                    string(&mut table, &s.name);
                    leb_u32(&mut table, 0); // the segment
                    leb_u32(&mut table, offset);
                    leb_u32(&mut table, size);
                }
                Slot::StackPointer => {
                    table.push(2); // a global
                    leb_u32(&mut table, s.flags);
                    leb_u32(&mut table, 0);
                }
                Slot::Table => {
                    table.push(5); // a table
                    leb_u32(&mut table, s.flags);
                    leb_u32(&mut table, 0);
                }
                Slot::LongJmpTag => {
                    table.push(4); // a tag
                    leb_u32(&mut table, s.flags);
                    leb_u32(&mut table, 0);
                }
            }
        }
        if !self.data.is_empty() {
            // WASM_SEGMENT_INFO: the segment's name and alignment.
            let mut info: Vec<u8> = Vec::new();
            leb_u32(&mut info, 1);
            string(&mut info, ".data.ash");
            leb_u32(&mut info, self.data_align);
            leb_u32(&mut info, 0);
            linking.push(5);
            leb_u32(&mut linking, info.len() as u32);
            linking.extend_from_slice(&info);
        }
        linking.push(8); // WASM_SYMBOL_TABLE
        leb_u32(&mut linking, table.len() as u32);
        linking.extend_from_slice(&table);
        module.section(&we::CustomSection {
            name: "linking".into(),
            data: linking.into(),
        });

        let write_relocs = |entries: &[(u8, u32, u32, Option<i32>)], section: u32| -> Vec<u8> {
            let mut out: Vec<u8> = Vec::new();
            leb_u32(&mut out, section);
            leb_u32(&mut out, entries.len() as u32);
            for (ty, offset, symbol, addend) in entries {
                out.push(*ty);
                leb_u32(&mut out, *offset);
                leb_u32(&mut out, *symbol);
                if let Some(addend) = addend {
                    leb_i32(&mut out, *addend);
                }
            }
            out
        };
        // A memory-address relocation of a 5-byte immediate carries its
        // addend; the signature, table, global and function kinds do not.
        let mut code_relocs: Vec<(u8, u32, u32, Option<i32>)> = relocs
            .iter()
            .map(|&(ty, off, sym, add)| (ty, off, sym, if ty == MEMORY_ADDR_SLEB { add } else { None }))
            .collect();
        // A linker reads a section's relocations in offset order.
        code_relocs.sort_by_key(|r| r.1);
        module.section(&we::CustomSection {
            name: "reloc.CODE".into(),
            data: write_relocs(&code_relocs, code_section_index).into(),
        });
        if !self.data_relocs.is_empty() {
            let mut entries: Vec<(u8, u32, u32, Option<i32>)> = self
                .data_relocs
                .iter()
                .map(|&(at, target)| {
                    let off = (data_header as u32) + at;
                    match target {
                        DataRef::Address(sym, addend) => (MEMORY_ADDR_I32, off, sym.0, Some(addend)),
                        DataRef::Function(sym) => (TABLE_INDEX_I32, off, sym.0, None),
                    }
                })
                .collect();
            entries.sort_by_key(|r| r.1);
            module.section(&we::CustomSection {
                name: "reloc.DATA".into(),
                data: write_relocs(&entries, data_section_index).into(),
            });
        }
        module.finish()
    }
}

fn leb_u32(out: &mut Vec<u8>, mut v: u32) {
    loop {
        let byte = (v & 0x7f) as u8;
        v >>= 7;
        if v == 0 {
            out.push(byte);
            return;
        }
        out.push(byte | 0x80);
    }
}

fn leb_i32(out: &mut Vec<u8>, mut v: i32) {
    loop {
        let byte = (v & 0x7f) as u8;
        v >>= 7;
        let done = (v == 0 && byte & 0x40 == 0) || (v == -1 && byte & 0x40 != 0);
        if done {
            out.push(byte);
            return;
        }
        out.push(byte | 0x80);
    }
}

/// A u32 in exactly five bytes, which a relocation can rewrite in place.
fn padded_leb(v: u32) -> [u8; 5] {
    [
        (v & 0x7f) as u8 | 0x80,
        ((v >> 7) & 0x7f) as u8 | 0x80,
        ((v >> 14) & 0x7f) as u8 | 0x80,
        ((v >> 21) & 0x7f) as u8 | 0x80,
        ((v >> 28) & 0x0f) as u8,
    ]
}

fn string(out: &mut Vec<u8>, s: &str) {
    leb_u32(out, s.len() as u32);
    out.extend_from_slice(s.as_bytes());
}
