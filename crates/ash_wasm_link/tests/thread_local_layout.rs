//! Where a thread-local ends up, checked on a module small enough to read.
//!
//! The dangerous property this crate warns about applies here twice over. A
//! `MEMORY_ADDR_TLS_SLEB` relocation writes an offset into a five-byte slot,
//! and every wrong offset that lands inside the block produces a module that
//! validates and runs -- reading the neighbouring variable instead of the one
//! asked for. Nothing about that is visible in a backtrace.
//!
//! So the object here is built by hand rather than read from disk: two data
//! segments, one of them `.tdata`, one relocation that has to come out
//! holding zero because the thread-local it names is the first thing in the
//! block, and a second variable after it that has to come out holding four.

use ash_wasm_link::object::{ObjData, ObjGlobal, Object, SegmentInfo, SymbolTarget};
use ash_wasm_link::{link, LinkOptions, Symbol};
use wasmparser::{FuncType, RelocationEntry, RelocationType, SymbolFlags, ValType};

/// A body that pushes two five-byte `i32.const` slots and drops them, so
/// there are two patch sites at known offsets.
///
/// `[locals] i32.const <5> drop i32.const <5> drop end`
const BODY: [u8; 16] = [
    0x00, // no local groups
    0x41, 0x80, 0x80, 0x80, 0x80, 0x00, // i32.const, padded to five bytes
    0x1a, // drop
    0x41, 0x80, 0x80, 0x80, 0x80, 0x00, //
    0x1a, //
    0x0b, // end
];
/// Where the two five-byte slots start inside `BODY`.
const FIRST_SLOT: u32 = 2;
const SECOND_SLOT: u32 = 9;

fn data_symbol(name: &str, segment: u32, offset: u32, size: u32, tls: bool) -> Symbol {
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
fn object(segment_name: &str, segment_flags: u32) -> Object {
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
fn sleb5(body: &[u8], offset: usize) -> i32 {
    let mut value: i64 = 0;
    for (i, byte) in body[offset..offset + 5].iter().enumerate() {
        value |= i64::from(byte & 0x7f) << (7 * i);
    }
    // Five groups of seven is thirty-five bits; the value is the low
    // thirty-two.
    value as i32
}

struct Module {
    globals: Vec<(bool, i32)>,
    functions: Vec<Vec<u8>>,
    data: Vec<(u32, Vec<u8>)>,
}

fn read(bytes: &[u8]) -> Module {
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
                    let DataKind::Active { offset_expr, .. } = segment.kind else {
                        panic!("a passive data segment");
                    };
                    let mut ops = offset_expr.get_operators_reader();
                    let Operator::I32Const { value } = ops.read().expect("an offset") else {
                        panic!("a data segment at a computed offset");
                    };
                    out.data.push((value as u32, segment.data.to_vec()));
                }
            }
            _ => {}
        }
    }
    out
}

#[test]
fn a_thread_local_is_addressed_from_the_block_and_not_from_memory() {
    let module = link(vec![object(".tdata", 0)], &LinkOptions::default()).expect("link");

    let mut features = wasmparser::WasmFeatures::default();
    features.insert(wasmparser::WasmFeatures::THREADS);
    wasmparser::Validator::new_with_features(features)
        .validate_all(&module)
        .expect("the output validates");

    let read = read(&module);

    // __stack_pointer, __memory_base, __table_base, then the three a threads
    // build needs.
    assert_eq!(read.globals.len(), 6, "globals: {:?}", read.globals);
    let (base_mutable, tls_base) = read.globals[3];
    let (_, tls_size) = read.globals[4];
    let (_, tls_align) = read.globals[5];
    assert!(base_mutable, "__tls_base has to move as each thread arrives");
    assert_eq!(tls_size, 8, "two i32s");
    assert_eq!(tls_align, 8, "the .tdata segment asked for eight");

    // The relocations: an offset within the block, not an address. The first
    // variable is at the start of it and the second four bytes in, and both
    // would be six-figure addresses if they were measured from memory.
    let main = read.functions.first().expect("a body for main");
    assert_eq!(sleb5(main, FIRST_SLOT as usize), 0);
    assert_eq!(sleb5(main, SECOND_SLOT as usize), 4);

    // Two copies of the thread-local data are placed: the template a new
    // thread copies from, and the main thread's own block, which
    // `__tls_base` starts at. They have to hold the same bytes, or a thread
    // starting later gets different initial values from the one that started
    // first.
    let (start, image) = read.data.first().expect("a data segment");
    let at = |address: i32| {
        let offset = (address as u32 - start) as usize;
        image[offset..offset + 8].to_vec()
    };
    let expected = vec![1, 0, 0, 0, 2, 0, 0, 0];
    assert_eq!(at(tls_base), expected, "the main thread's block");
    let template = tls_base - tls_size;
    assert_eq!(at(template), expected, "the template");

    // And `__wasm_init_tls`, which is what gives a thread a block of its own:
    // it stores its argument into `__tls_base` and copies the template over
    // it.
    let init_tls = &read.functions[read.functions.len() - 1];
    assert!(
        init_tls.contains(&0xfc) && init_tls.windows(2).any(|w| w == [0xfc, 0x0a]),
        "__wasm_init_tls does not copy the template: {init_tls:02x?}"
    );
}

/// The same layout, whichever of the two things says the segment is
/// thread-local: the name LLVM gives it -- with the per-symbol suffix it
/// carries under `-fdata-sections` -- or the flag that `lld -r` drops.
#[test]
fn either_the_name_or_the_flag_makes_a_segment_thread_local() {
    for (name, flags) in [(".tdata.first", 0u32), (".tbss", 0), ("anything", 0x2)] {
        let module =
            link(vec![object(name, flags)], &LinkOptions::default()).expect("link");
        let read = read(&module);
        assert_eq!(read.globals.len(), 6, "{name} flags={flags:#x}");
        assert_eq!(read.globals[4].1, 8, "__tls_size for {name}");
        let main = read.functions.first().expect("a body for main");
        assert_eq!(sleb5(main, FIRST_SLOT as usize), 0, "{name}");
        assert_eq!(sleb5(main, SECOND_SLOT as usize), 4, "{name}");
    }
}

/// A segment nothing says is thread-local holds symbols nothing says are
/// either, and a link where the two disagree is refused rather than laid out
/// from one of them.
#[test]
fn a_symbol_and_its_segment_have_to_agree() {
    // The symbols still claim to be thread-local; the segment no longer is.
    let error = link(vec![object("ordinary", 0)], &LinkOptions::default())
        .expect_err("a TLS symbol in a segment that is not")
        .to_string();
    assert!(error.contains("first"), "{error}");
    assert!(error.contains("thread-local"), "{error}");

    // And the other way: an ordinary symbol in a thread-local segment.
    let mut obj = object(".tdata", 0);
    obj.symbols[1].flags = SymbolFlags::empty();
    let error = link(vec![obj], &LinkOptions::default())
        .expect_err("a plain symbol in a TLS segment")
        .to_string();
    assert!(error.contains("first"), "{error}");
}

/// A thread-local reached as if it were at an address would be one thread's
/// copy, shared by every thread that ran the code. Nothing this linker links
/// does that, and one arriving is refused rather than placed.
#[test]
fn a_thread_local_cannot_be_reached_by_address() {
    let mut obj = object(".tdata", 0);
    obj.code_relocs[0].ty = RelocationType::MemoryAddrSleb;
    let error = link(vec![obj], &LinkOptions::default())
        .expect_err("an absolute address for a thread-local");
    // The whole chain: `patch` wraps its failures in which section they were
    // in, and the detail is under that.
    let error = format!("{error:#}");
    assert!(error.contains("first"), "{error}");
    assert!(error.contains("thread-local"), "{error}");
}
