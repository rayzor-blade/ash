//! Building an object by hand, and reading back what the linker did with it.
//!
//! Shared by the tests that check a threads build, which need the same small
//! object: what makes them different is what they ask the linker for and what
//! they look at afterwards.

#![allow(dead_code)]

use ash_wasm_link::object::{ObjData, ObjGlobal, Object, SegmentInfo, SymbolTarget};
use ash_wasm_link::Symbol;
use wasmparser::{FuncType, RelocationEntry, RelocationType, SymbolFlags, ValType};

/// A body that pushes two five-byte `i32.const` slots and drops them, so
/// there are two patch sites at known offsets.
///
/// `[locals] i32.const <5> drop i32.const <5> drop end`
pub const BODY: [u8; 16] = [
    0x00, // no local groups
    0x41, 0x80, 0x80, 0x80, 0x80, 0x00, // i32.const, padded to five bytes
    0x1a, // drop
    0x41, 0x80, 0x80, 0x80, 0x80, 0x00, //
    0x1a, //
    0x0b, // end
];
/// Where the two five-byte slots start inside `BODY`.
pub const FIRST_SLOT: u32 = 2;
pub const SECOND_SLOT: u32 = 9;

pub fn data_symbol(name: &str, segment: u32, offset: u32, size: u32, tls: bool) -> Symbol {
    Symbol {
        name: name.to_string(),
        target: SymbolTarget::Data {
            segment,
            offset,
            size,
        },
        flags: if tls {
            SymbolFlags::TLS
        } else {
            SymbolFlags::empty()
        },
    }
}

/// One object: a `main`, four bytes of ordinary data, and eight bytes of
/// thread-local data holding two variables.
///
/// The segment's name and flags are the caller's, because they are the two
/// things that say a segment is thread-local and only one of them survives a
/// relocatable link.
pub fn object(segment_name: &str, segment_flags: u32) -> Object {
    let whole_body = 0..BODY.len();
    Object {
        name: "tls.o".into(),
        types: vec![
            FuncType::new([], []),
            FuncType::new([ValType::I32], []),
        ],
        imports: Vec::<ash_wasm_link::object::ObjImport>::new(),
        functions: vec![0],
        tables: Vec::new(),
        memories: Vec::new(),
        globals: Vec::<ObjGlobal>::new(),
        tags: Vec::new(),
        exports: Vec::new(),
        elements: Vec::new(),
        code_payload: BODY.to_vec(),
        code_bodies: vec![whole_body],
        // `.data` first, `.tdata` after it, which is the order an object
        // holds them in and NOT the order they are placed in.
        data_payload: vec![
            0xaa, 0xaa, 0xaa, 0xaa, // .data
            1, 0, 0, 0, 2, 0, 0, 0, // .tdata: two i32s
        ],
        data_segments: vec![
            ObjData {
                range: 0..4,
                passive: false,
            },
            ObjData {
                range: 4..12,
                passive: false,
            },
        ],
        segment_info: vec![
            SegmentInfo {
                name: ".data".into(),
                align_log2: 2,
                flags: 0,
            },
            SegmentInfo {
                name: segment_name.into(),
                align_log2: 3,
                flags: segment_flags,
            },
        ],
        symbols: vec![
            Symbol {
                name: "main".into(),
                target: SymbolTarget::Function { index: 0 },
                flags: SymbolFlags::empty(),
            },
            data_symbol("first", 1, 0, 4, true),
            data_symbol("second", 1, 4, 4, true),
        ],
        code_relocs: vec![
            RelocationEntry {
                ty: RelocationType::MemoryAddrTlsSleb,
                offset: FIRST_SLOT,
                index: 1,
                addend: 0,
            },
            RelocationEntry {
                ty: RelocationType::MemoryAddrTlsSleb,
                offset: SECOND_SLOT,
                index: 2,
                addend: 0,
            },
        ],
        data_relocs: Vec::new(),
        init_funcs: Vec::new(),
    }
}

/// Read back the value of a five-byte signed LEB at `offset` in `body`.
pub fn sleb5(body: &[u8], offset: usize) -> i32 {
    let mut value: i64 = 0;
    for (i, byte) in body[offset..offset + 5].iter().enumerate() {
        value |= i64::from(byte & 0x7f) << (7 * i);
    }
    // Five groups of seven is thirty-five bits; the value is the low
    // thirty-two.
    value as i32
}

pub struct Module {
    pub globals: Vec<(bool, i32)>,
    pub functions: Vec<Vec<u8>>,
    /// Each data segment: where it is written, which a passive one does not
    /// say, and what it holds.
    pub data: Vec<(Option<u32>, Vec<u8>)>,
}

pub fn read(bytes: &[u8]) -> Module {
    use wasmparser::{DataKind, Operator, Parser, Payload};

    let mut out = Module {
        globals: Vec::new(),
        functions: Vec::new(),
        data: Vec::new(),
    };
    for payload in Parser::new(0).parse_all(bytes) {
        match payload.expect("parsing the module") {
            Payload::GlobalSection(section) => {
                for global in section {
                    let global = global.expect("a global");
                    let mut ops = global.init_expr.get_operators_reader();
                    let value = match ops.read().expect("an init expression") {
                        Operator::I32Const { value } => value,
                        other => panic!("a global is initialised by {other:?}"),
                    };
                    out.globals.push((global.ty.mutable, value));
                }
            }
            Payload::CodeSectionEntry(body) => {
                let range = body.range();
                out.functions
                    .push(bytes[range.start as usize..range.end as usize].to_vec());
            }
            Payload::DataSection(section) => {
                for segment in section {
                    let segment = segment.expect("a data segment");
                    let at = match segment.kind {
                        DataKind::Passive => None,
                        DataKind::Active { offset_expr, .. } => {
                            let mut ops = offset_expr.get_operators_reader();
                            let Operator::I32Const { value } = ops.read().expect("an offset")
                            else {
                                panic!("a data segment at a computed offset");
                            };
                            Some(value as u32)
                        }
                    };
                    out.data.push((at, segment.data.to_vec()));
                }
            }
            _ => {}
        }
    }
    out
}

