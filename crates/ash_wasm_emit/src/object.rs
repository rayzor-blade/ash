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
const MEMORY_ADDR_SLEB: u8 = 4;

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
}

/// What a patch site holds the index or address of.
#[derive(Clone, Copy)]
enum Target {
    Function(Sym),
    /// The address of a data symbol plus an addend.
    Address(Sym, i32),
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
}

impl Body {
    pub fn new(locals: Vec<(u32, ValType)>) -> Self {
        Body {
            func: we::Function::new(locals),
            sites: Vec::new(),
        }
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
        // The opcode, then the index at the byte after it.
        self.func.raw([0x10]);
        let at = self.func.byte_len();
        self.func.raw([0x80, 0x80, 0x80, 0x80, 0x00]);
        self.sites.push((at, Target::Function(sym)));
    }

    /// `i32.const` of a data symbol's address plus `addend`, which the linker
    /// fills in.
    pub fn address_of(&mut self, sym: Sym, addend: i32) {
        self.func.raw([0x41]);
        let at = self.func.byte_len();
        self.func.raw([0x80, 0x80, 0x80, 0x80, 0x00]);
        self.sites.push((at, Target::Address(sym, addend)));
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

    /// A function this object defines. `local` hides it from other objects.
    pub fn declare_function(
        &mut self,
        name: &str,
        params: Vec<ValType>,
        results: Vec<ValType>,
        local: bool,
    ) -> Sym {
        let ty = self.ty(params, results);
        self.defined.push(Defined { ty, body: None });
        let sym = Sym(self.symbols.len() as u32);
        self.symbols.push(SymbolInfo {
            name: name.to_string(),
            slot: Slot::Defined(self.defined.len() - 1),
            flags: if local { FLAG_LOCAL } else { FLAG_HIDDEN },
        });
        self.by_name.insert(name.to_string(), sym);
        sym
    }

    pub fn set_body(&mut self, sym: Sym, body: Body) {
        match self.symbols[sym.0 as usize].slot {
            Slot::Defined(i) => self.defined[i].body = Some(body),
            Slot::Import(_) | Slot::UndefinedData => panic!("set_body on a function this object does not define"),
        }
    }

    pub fn finish(self) -> Vec<u8> {
        let imported = self.imports.len() as u32;
        let function_index = |sym: Sym| -> u32 {
            match self.symbols[sym.0 as usize].slot {
                Slot::Import(i) => i as u32,
                Slot::Defined(i) => imported + i as u32,
                Slot::UndefinedData => panic!("{} is data, not a function", self.symbols[sym.0 as usize].name),
            }
        };

        // Bodies, with their indices written and the relocations located.
        let mut code_payload: Vec<u8> = Vec::new();
        // (offset, symbol, addend or None for a function index)
        let mut relocs: Vec<(u32, u32, Option<i32>)> = Vec::new();
        leb_u32(&mut code_payload, self.defined.len() as u32);
        for d in &self.defined {
            let body = d.body.as_ref().expect("a declared function has no body");
            let mut bytes = body.func.clone().into_raw_body();
            for &(at, target) in &body.sites {
                if let Target::Function(sym) = target {
                    bytes[at..at + 5].copy_from_slice(&padded_leb(function_index(sym)));
                }
            }
            leb_u32(&mut code_payload, bytes.len() as u32);
            let base = code_payload.len();
            for &(at, target) in &body.sites {
                match target {
                    Target::Function(sym) => relocs.push(((base + at) as u32, sym.0, None)),
                    Target::Address(sym, addend) => {
                        relocs.push(((base + at) as u32, sym.0, Some(addend)))
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
        if !self.imports.is_empty() || self.memory_min.is_some() {
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

        // linking: version 2, then the symbol table.
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
            }
        }
        linking.push(8); // WASM_SYMBOL_TABLE
        leb_u32(&mut linking, table.len() as u32);
        linking.extend_from_slice(&table);
        module.section(&we::CustomSection {
            name: "linking".into(),
            data: linking.into(),
        });

        let mut reloc: Vec<u8> = Vec::new();
        leb_u32(&mut reloc, code_section_index);
        leb_u32(&mut reloc, relocs.len() as u32);
        for (offset, symbol, addend) in &relocs {
            reloc.push(if addend.is_some() { MEMORY_ADDR_SLEB } else { FUNCTION_INDEX_LEB });
            leb_u32(&mut reloc, *offset);
            leb_u32(&mut reloc, *symbol);
            if let Some(addend) = addend {
                leb_i32(&mut reloc, *addend);
            }
        }
        module.section(&we::CustomSection {
            name: "reloc.CODE".into(),
            data: reloc.into(),
        });
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
