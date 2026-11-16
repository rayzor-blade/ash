//! Static libraries are linked the way a native linker links them: a member joins only for a
//! symbol the link names and lacks, and a definition the link already has wins over a member's.

use ash_wasm_link::archive::{Archive, is_archive, pull, read_archive};
use ash_wasm_link::object::{ImportKind, ObjImport, Object, SymbolTarget};
use ash_wasm_link::{LinkOptions, Symbol, link};
use wasmparser::{FuncType, RelocationEntry, RelocationType, SymbolFlags, ValType};

#[derive(Clone, Copy)]
enum Body {
    /// Returns this constant, which is under 64.
    Const(u8),
    /// Returns what the named function returns.
    Call(&'static str),
    /// As `Call`, through a weak reference.
    WeakCall(&'static str),
}

struct Def {
    name: &'static str,
    body: Body,
    weak: bool,
}

fn def(name: &'static str, body: Body) -> Def {
    Def {
        name,
        body,
        weak: false,
    }
}

fn weak_def(name: &'static str, body: Body) -> Def {
    Def {
        name,
        body,
        weak: true,
    }
}

/// The five-byte padded unsigned LEB a relocation patches.
fn uleb5(v: u32) -> [u8; 5] {
    [
        (v & 0x7f) as u8 | 0x80,
        ((v >> 7) & 0x7f) as u8 | 0x80,
        ((v >> 14) & 0x7f) as u8 | 0x80,
        ((v >> 21) & 0x7f) as u8 | 0x80,
        ((v >> 28) & 0x0f) as u8,
    ]
}

/// An object of functions `() -> i32`, each returning a constant or what another returns.
/// A function it calls and does not define is an undefined symbol with an import entry, as in an
/// object a compiler wrote.
fn object(name: &str, defs: &[Def]) -> Object {
    let callee = |b: Body| match b {
        Body::Const(_) => None,
        Body::Call(n) | Body::WeakCall(n) => Some(n),
    };
    let mut undefined: Vec<(&str, bool)> = Vec::new();
    for d in defs {
        if let Some(n) = callee(d.body)
            && !defs.iter().any(|x| x.name == n)
            && !undefined.iter().any(|(u, _)| *u == n)
        {
            undefined.push((n, matches!(d.body, Body::WeakCall(_))));
        }
    }
    let imports: Vec<ObjImport> = undefined
        .iter()
        .map(|(n, _)| ObjImport {
            module: "env".into(),
            name: (*n).into(),
            kind: ImportKind::Function { type_index: 0 },
        })
        .collect();
    let imported = imports.len() as u32;

    let mut symbols: Vec<Symbol> = defs
        .iter()
        .enumerate()
        .map(|(k, d)| Symbol {
            name: d.name.into(),
            target: SymbolTarget::Function {
                index: imported + k as u32,
            },
            flags: if d.weak {
                SymbolFlags::BINDING_WEAK
            } else {
                SymbolFlags::empty()
            },
        })
        .collect();
    for (i, (n, weak)) in undefined.iter().enumerate() {
        let mut flags = SymbolFlags::UNDEFINED | SymbolFlags::EXPLICIT_NAME;
        if *weak {
            flags |= SymbolFlags::BINDING_WEAK;
        }
        symbols.push(Symbol {
            name: (*n).into(),
            target: SymbolTarget::Function { index: i as u32 },
            flags,
        });
    }

    let mut payload = Vec::new();
    let mut bodies = Vec::new();
    let mut relocs = Vec::new();
    for d in defs {
        let start = payload.len();
        payload.push(0x00); // no locals
        match d.body {
            Body::Const(v) => payload.extend([0x41, v & 0x3f]),
            Body::Call(n) | Body::WeakCall(n) => {
                let symbol = symbols.iter().position(|s| s.name == n).unwrap() as u32;
                let local = match symbols[symbol as usize].target {
                    SymbolTarget::Function { index } => index,
                    _ => unreachable!(),
                };
                payload.push(0x10); // call
                relocs.push(RelocationEntry {
                    ty: RelocationType::FunctionIndexLeb,
                    offset: payload.len() as u32,
                    index: symbol,
                    addend: 0,
                });
                payload.extend(uleb5(local));
            }
        }
        payload.push(0x0b);
        bodies.push(start..payload.len());
    }

    Object {
        name: name.into(),
        // The second type is the one the linker's own constructor function takes.
        types: vec![FuncType::new([], [ValType::I32]), FuncType::new([], [])],
        imports,
        functions: vec![0; defs.len()],
        tables: Vec::new(),
        memories: Vec::new(),
        globals: Vec::new(),
        tags: Vec::new(),
        exports: Vec::new(),
        elements: Vec::new(),
        code_payload: payload,
        code_bodies: bodies,
        data_payload: Vec::new(),
        data_segments: Vec::new(),
        symbols,
        segment_info: Vec::new(),
        code_relocs: relocs,
        data_relocs: Vec::new(),
        init_funcs: Vec::new(),
    }
}

fn names(objects: &[Object]) -> Vec<&str> {
    objects.iter().map(|o| o.name.as_str()).collect()
}

/// Links `objects` and calls `main`.
fn run_main(objects: Vec<Object>) -> anyhow::Result<i32> {
    let opts = LinkOptions {
        export_all_functions: true,
        ..LinkOptions::default()
    };
    let bytes = link(objects, &opts)?;
    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &bytes)?;
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[])?;
    let main = instance.get_typed_func::<(), i32>(&mut store, "main")?;
    Ok(main.call(&mut store, ())?)
}

#[test]
fn a_member_joins_only_for_a_symbol_the_link_names() {
    let program = object("program.o", &[def("main", Body::Call("needed"))]);
    let lib = Archive::from_objects(
        "lib.a",
        vec![
            object("unused.o", &[def("other", Body::Const(9))]),
            object("needed.o", &[def("needed", Body::Const(7))]),
        ],
    );
    let pulled = pull(&[program], vec![lib]).unwrap();
    assert_eq!(names(&pulled.objects), ["needed.o"]);
    assert_eq!(pulled.taken, [1]);
    assert!(pulled.shadowed.is_empty());
}

#[test]
fn a_pulled_member_is_searched_for_what_it_names() {
    let program = object("program.o", &[def("main", Body::Call("a"))]);
    let lib = Archive::from_objects(
        "lib.a",
        vec![
            // Listed ahead of what it needs, so one pass over the archive would miss it.
            object("a.o", &[def("a", Body::Call("b"))]),
            object("c.o", &[def("c", Body::Const(1))]),
            object("b.o", &[def("b", Body::Call("c"))]),
        ],
    );
    let pulled = pull(&[program], vec![lib]).unwrap();
    assert_eq!(names(&pulled.objects), ["a.o", "b.o", "c.o"]);
}

#[test]
fn a_weak_reference_pulls_nothing() {
    let program = object("program.o", &[def("main", Body::WeakCall("maybe"))]);
    let lib = Archive::from_objects(
        "lib.a",
        vec![object("maybe.o", &[def("maybe", Body::Const(1))])],
    );
    let pulled = pull(&[program], vec![lib]).unwrap();
    assert!(pulled.objects.is_empty());
}

#[test]
fn a_symbol_nobody_defines_stays_undefined() {
    let program = object("program.o", &[def("main", Body::Call("from_the_host"))]);
    let lib = Archive::from_objects(
        "lib.a",
        vec![object("other.o", &[def("other", Body::Const(1))])],
    );
    let pulled = pull(&[program], vec![lib]).unwrap();
    assert!(pulled.objects.is_empty());
}

#[test]
fn the_links_own_definition_wins_over_a_members() {
    let program = object("program.o", &[def("main", Body::Call("needed"))]);
    let runtime = object("runtime.o", &[def("shared", Body::Const(1))]);
    let member = object(
        "needed.o",
        &[
            def("needed", Body::Call("shared")),
            def("shared", Body::Const(2)),
        ],
    );

    // As plain objects the two `shared` definitions are the error they always were.
    let both = link(
        vec![program.clone(), member.clone(), runtime.clone()],
        &LinkOptions::default(),
    );
    assert!(
        both.unwrap_err()
            .to_string()
            .contains("shared is defined in both"),
    );

    let lib = Archive::from_objects("lib.a", vec![member]);
    let mut inputs = vec![program, runtime];
    let pulled = pull(&inputs, vec![lib]).unwrap();
    assert_eq!(pulled.shadowed.len(), 1);
    assert_eq!(pulled.shadowed[0].symbol, "shared");
    assert_eq!(pulled.shadowed[0].member, "needed.o");
    assert_eq!(pulled.shadowed[0].kept, "runtime.o");
    inputs.extend(pulled.objects);
    // The member's own call to `shared` reaches the runtime's: 1, not 2.
    assert_eq!(run_main(inputs).unwrap(), 1);
}

#[test]
fn the_first_library_wins_between_libraries() {
    let program = object(
        "program.o",
        &[def("main", Body::Call("a")), def("also", Body::Call("b"))],
    );
    let first = Archive::from_objects(
        "first.a",
        vec![object(
            "a.o",
            &[def("a", Body::Call("dup")), def("dup", Body::Const(3))],
        )],
    );
    let second = Archive::from_objects(
        "second.a",
        vec![object(
            "b.o",
            &[def("b", Body::Call("dup")), def("dup", Body::Const(4))],
        )],
    );
    let pulled = pull(std::slice::from_ref(&program), vec![first, second]).unwrap();
    assert_eq!(names(&pulled.objects), ["a.o", "b.o"]);
    assert_eq!(pulled.taken, [1, 1]);
    assert_eq!(pulled.shadowed.len(), 1);
    assert_eq!(pulled.shadowed[0].member, "b.o");
    assert_eq!(pulled.shadowed[0].kept, "a.o");

    let mut inputs = vec![program];
    inputs.extend(pulled.objects);
    assert_eq!(run_main(inputs).unwrap(), 3);
}

#[test]
fn a_strong_member_definition_displaces_a_weak_one() {
    let program = object("program.o", &[def("main", Body::Call("needed"))]);
    let runtime = object("runtime.o", &[weak_def("shared", Body::Const(1))]);
    let member = object(
        "needed.o",
        &[
            def("needed", Body::Call("shared")),
            def("shared", Body::Const(2)),
        ],
    );
    let mut inputs = vec![program, runtime];
    let pulled = pull(&inputs, vec![Archive::from_objects("lib.a", vec![member])]).unwrap();
    assert!(pulled.shadowed.is_empty());
    inputs.extend(pulled.objects);
    assert_eq!(run_main(inputs).unwrap(), 2);
}

#[test]
fn a_program_that_names_nothing_pulls_nothing() {
    let program = object("program.o", &[def("main", Body::Const(5))]);
    let lib = Archive::from_objects("lib.a", vec![object("a.o", &[def("a", Body::Const(1))])]);
    let pulled = pull(std::slice::from_ref(&program), vec![lib]).unwrap();
    assert!(pulled.objects.is_empty());
    assert_eq!(run_main(vec![program]).unwrap(), 5);
}

/// A minimal wasm module: only the header, so it reads as an object with nothing in it.
const EMPTY_OBJECT: &[u8] = b"\0asm\x01\0\0\0";

/// An `ar` archive in the GNU form: the long-name table, then each member padded to even length.
fn gnu_archive(members: &[(&str, &[u8])]) -> Vec<u8> {
    let mut long_names = Vec::new();
    let mut refs = Vec::new();
    for (name, _) in members {
        if name.len() > 15 {
            refs.push(Some(long_names.len()));
            long_names.extend_from_slice(name.as_bytes());
            long_names.extend_from_slice(b"/\n");
        } else {
            refs.push(None);
        }
    }
    let mut out = b"!<arch>\n".to_vec();
    let entry = |out: &mut Vec<u8>, name: &str, data: &[u8]| {
        out.extend(
            format!(
                "{name:<16}{:<12}{:<6}{:<6}{:<8}{:<10}`\n",
                0,
                0,
                0,
                644,
                data.len()
            )
            .bytes(),
        );
        out.extend_from_slice(data);
        if data.len() % 2 == 1 {
            out.push(b'\n');
        }
    };
    if !long_names.is_empty() {
        entry(&mut out, "//", &long_names);
    }
    for ((name, data), long) in members.iter().zip(refs) {
        match long {
            Some(at) => entry(&mut out, &format!("/{at}"), data),
            None => entry(&mut out, &format!("{name}/"), data),
        }
    }
    out
}

#[test]
fn an_archive_is_read_member_by_member_with_long_names() {
    let long = "greeter_hl-0f9d9926b8742830.greeter_hl.234b0f7070e8dd35-cgu.0.rcgu.o";
    let bytes = gnu_archive(&[
        ("short.o", EMPTY_OBJECT),
        (long, EMPTY_OBJECT),
        ("notes.txt", b"not an object"),
    ]);
    assert!(is_archive(&bytes));
    let archive = read_archive("lib.a", &bytes).unwrap();
    // The text member is not an object and is skipped.
    assert_eq!(
        archive.member_names(),
        ["lib.a(short.o)".to_string(), format!("lib.a({long})")]
    );
}

#[test]
fn something_that_is_not_an_archive_is_refused() {
    assert!(!is_archive(EMPTY_OBJECT));
    assert!(read_archive("x.a", b"not an archive at all").is_err());
    assert!(
        read_archive("thin.a", b"!<thin>\n")
            .unwrap_err()
            .to_string()
            .contains("thin archive")
    );
}

/// A position-independent program: each function returns the GOT global for the function it
/// names, which holds that function's table slot.
fn got_program(entries: &[(&str, &str)]) -> Object {
    let mut imports = Vec::new();
    let mut symbols = Vec::new();
    let mut targets: Vec<&str> = Vec::new();
    for (_, target) in entries {
        if !targets.contains(target) {
            targets.push(target);
        }
    }
    for t in &targets {
        imports.push(ObjImport {
            module: "env".into(),
            name: (*t).into(),
            kind: ImportKind::Function { type_index: 0 },
        });
    }
    for t in &targets {
        imports.push(ObjImport {
            module: "GOT.func".into(),
            name: (*t).into(),
            kind: ImportKind::Global {
                ty: ValType::I32,
                mutable: false,
            },
        });
    }
    let imported = targets.len() as u32;
    for (k, (name, _)) in entries.iter().enumerate() {
        symbols.push(Symbol {
            name: (*name).into(),
            target: SymbolTarget::Function {
                index: imported + k as u32,
            },
            flags: SymbolFlags::empty(),
        });
    }
    for (i, t) in targets.iter().enumerate() {
        symbols.push(Symbol {
            name: (*t).into(),
            target: SymbolTarget::Function { index: i as u32 },
            flags: SymbolFlags::UNDEFINED | SymbolFlags::EXPLICIT_NAME,
        });
    }
    let mut payload = Vec::new();
    let mut bodies = Vec::new();
    let mut relocs = Vec::new();
    for (_, target) in entries {
        let start = payload.len();
        payload.push(0x00);
        payload.push(0x23); // global.get
        let global = targets.iter().position(|t| t == target).unwrap() as u32;
        relocs.push(RelocationEntry {
            ty: RelocationType::GlobalIndexLeb,
            offset: payload.len() as u32,
            index: (entries.len() + global as usize) as u32,
            addend: 0,
        });
        payload.extend(uleb5(global));
        payload.push(0x0b);
        bodies.push(start..payload.len());
    }
    Object {
        name: "program.o".into(),
        types: vec![FuncType::new([], [ValType::I32]), FuncType::new([], [])],
        imports,
        functions: vec![0; entries.len()],
        tables: Vec::new(),
        memories: Vec::new(),
        globals: Vec::new(),
        tags: Vec::new(),
        exports: Vec::new(),
        elements: Vec::new(),
        code_payload: payload,
        code_bodies: bodies,
        data_payload: Vec::new(),
        data_segments: Vec::new(),
        symbols,
        segment_info: Vec::new(),
        code_relocs: relocs,
        data_relocs: Vec::new(),
        init_funcs: Vec::new(),
    }
}

#[test]
fn a_function_reached_only_through_the_got_lands_on_its_own_table_slot() {
    // `main` and `other` read the slots of `seven` and `nine`, which nothing calls.
    let program = got_program(&[("main", "seven"), ("other", "nine")]);
    let lib = Archive::from_objects(
        "lib.a",
        vec![
            object("seven.o", &[def("seven", Body::Const(7))]),
            object("nine.o", &[def("nine", Body::Const(9))]),
            object("unused.o", &[def("unused", Body::Const(1))]),
        ],
    );
    let pulled = pull(std::slice::from_ref(&program), vec![lib]).unwrap();
    assert_eq!(names(&pulled.objects), ["seven.o", "nine.o"]);
    let mut inputs = vec![program];
    inputs.extend(pulled.objects);
    let opts = LinkOptions {
        roots: vec!["main".into(), "other".into()],
        // Naming functions a library would import is what makes the module export its table.
        hdll_imports: vec!["main".into(), "other".into()],
        prune_imports: true,
        ..LinkOptions::default()
    };
    let bytes = link(inputs, &opts).unwrap();

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &bytes).unwrap();
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();
    let table = instance
        .get_table(&mut store, "__indirect_function_table")
        .expect("the table is exported");
    let mut through_table = |export: &str| -> i32 {
        let slot = instance
            .get_typed_func::<(), i32>(&mut store, export)
            .unwrap()
            .call(&mut store, ())
            .unwrap();
        assert_ne!(slot, 0, "slot zero is the null function");
        let entry = table.get(&mut store, slot as u64).expect("a slot in range");
        let func = *entry.unwrap_func().expect("a function, not null");
        func.typed::<(), i32>(&store)
            .unwrap()
            .call(&mut store, ())
            .unwrap()
    };
    assert_eq!(through_table("main"), 7);
    assert_eq!(through_table("other"), 9);
}

/// A program whose `main` calls `target` with zeroed arguments of the type `sig` says.
fn call_program(sig: FuncType, target: &str) -> Object {
    let mut body = vec![0x00];
    for p in sig.params() {
        match p {
            ValType::I32 => body.extend([0x41, 0]),
            ValType::I64 => body.extend([0x42, 0]),
            ValType::F32 => body.extend([0x43, 0, 0, 0, 0]),
            ValType::F64 => body.extend([0x44, 0, 0, 0, 0, 0, 0, 0, 0]),
            other => panic!("no zero for {other:?}"),
        }
    }
    body.push(0x10);
    let slot = body.len() as u32;
    body.extend(uleb5(0));
    body.extend(sig.results().iter().map(|_| 0x1a)); // drop each result
    body.push(0x0b);
    let len = body.len();
    Object {
        name: "program.o".into(),
        types: vec![sig, FuncType::new([], [])],
        imports: vec![ObjImport {
            module: "env".into(),
            name: target.into(),
            kind: ImportKind::Function { type_index: 0 },
        }],
        functions: vec![1],
        tables: Vec::new(),
        memories: Vec::new(),
        globals: Vec::new(),
        tags: Vec::new(),
        exports: Vec::new(),
        elements: Vec::new(),
        code_payload: body,
        code_bodies: vec![std::ops::Range { start: 0, end: len }],
        data_payload: Vec::new(),
        data_segments: Vec::new(),
        symbols: vec![
            Symbol {
                name: "main".into(),
                target: SymbolTarget::Function { index: 1 },
                flags: SymbolFlags::empty(),
            },
            Symbol {
                name: target.into(),
                target: SymbolTarget::Function { index: 0 },
                flags: SymbolFlags::UNDEFINED | SymbolFlags::EXPLICIT_NAME,
            },
        ],
        segment_info: Vec::new(),
        code_relocs: vec![RelocationEntry {
            ty: RelocationType::FunctionIndexLeb,
            offset: slot,
            index: 1,
            addend: 0,
        }],
        data_relocs: Vec::new(),
        init_funcs: Vec::new(),
    }
}

/// A real Rust static library against the real runtime object.
///
/// Set `ASH_LINK_TEST_ARCHIVE` to the library, `ASH_LINK_TEST_RUNTIME` to the matching
/// `ash_runtime.o` and `ASH_LINK_TEST_SYMBOL` to a function the library defines;
/// `ASH_LINK_TEST_THREADS=1` for a `-threads` target.
#[test]
fn a_real_library_links_against_the_real_runtime() {
    let (Some(archive), Some(runtime), Some(symbol)) = (
        std::env::var_os("ASH_LINK_TEST_ARCHIVE"),
        std::env::var_os("ASH_LINK_TEST_RUNTIME"),
        std::env::var("ASH_LINK_TEST_SYMBOL").ok(),
    ) else {
        eprintln!(
            "skipped: set ASH_LINK_TEST_ARCHIVE, ASH_LINK_TEST_RUNTIME and ASH_LINK_TEST_SYMBOL"
        );
        return;
    };
    let read_archive_file = || {
        let bytes = std::fs::read(&archive).unwrap();
        read_archive("lib.a", &bytes).unwrap()
    };
    let runtime = ash_wasm_link::read("ash_runtime.o", &std::fs::read(&runtime).unwrap()).unwrap();
    let total = read_archive_file().len();

    // Which member defines the symbol tells the signature to call it with.
    let probe = pull(
        &[
            call_program(FuncType::new([], []), &symbol),
            runtime.clone(),
        ],
        vec![read_archive_file()],
    )
    .unwrap();
    let definer = probe
        .objects
        .iter()
        .find_map(|o| {
            let (i, sym) = o
                .symbols
                .iter()
                .enumerate()
                .find(|(_, s)| s.name == symbol && s.defines())?;
            let SymbolTarget::Function { index } = sym.target else {
                return None;
            };
            let _ = i;
            Some(o.types[o.functions[(index - o.imported_functions()) as usize] as usize].clone())
        })
        .unwrap_or_else(|| panic!("no member defines {symbol}"));

    let program = call_program(definer, &symbol);
    let pulled = pull(
        &[program.clone(), runtime.clone()],
        vec![read_archive_file()],
    )
    .unwrap();
    eprintln!(
        "{} of {total} members pulled; {} definitions dropped for the runtime's or an earlier one's",
        pulled.objects.len(),
        pulled.shadowed.len()
    );
    let mut dropped: Vec<&str> = pulled.shadowed.iter().map(|s| s.symbol.as_str()).collect();
    dropped.sort_unstable();
    dropped.dedup();
    eprintln!("dropped: {dropped:?}");
    assert!(!pulled.objects.is_empty());
    assert!(pulled.objects.len() < total, "the whole library was pulled");

    let mut inputs = vec![program];
    inputs.extend(pulled.objects);
    inputs.push(runtime);
    let opts = LinkOptions {
        shared_memory: std::env::var_os("ASH_LINK_TEST_THREADS").is_some(),
        prune_imports: true,
        ..LinkOptions::default()
    };
    let module = link(inputs, &opts).expect("the link");
    wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::all())
        .validate_all(&module)
        .expect("the linked module validates");
    eprintln!("linked {} bytes", module.len());
}

#[test]
fn a_dropped_definition_can_be_looked_up_by_the_names_the_program_calls() {
    let program = object("program.o", &[def("main", Body::Call("needed"))]);
    let runtime = object("runtime.o", &[def("shared", Body::Const(1))]);
    let member = object(
        "needed.o",
        &[def("needed", Body::Call("shared")), def("shared", Body::Const(2))],
    );
    let pulled = pull(
        &[program, runtime],
        vec![Archive::from_objects("lib.a", vec![member])],
    )
    .unwrap();
    let hit = pulled.shadowed_named(&["shared".to_string(), "other".to_string()]);
    assert_eq!(hit.len(), 1);
    assert_eq!((hit[0].symbol.as_str(), hit[0].kept.as_str()), ("shared", "runtime.o"));
    assert!(pulled.shadowed_named(&["other".to_string()]).is_empty());
    assert!(pulled.shadowed_named(&[]).is_empty());
}
