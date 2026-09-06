//! What a second instance must not do to the first one's memory.
//!
//! A thread on wasm is another instance of the same module over the same
//! memory. The failure this guards against is silent and total: an active
//! data segment is written at instantiation, so starting a second thread
//! would put the program's initial data back over everything the first one
//! had reached -- every allocation, every global, the collector's own
//! bookkeeping. Nothing traps. The program simply goes back in time.
//!
//! So the test instantiates twice against one shared memory with a sentinel
//! written in between, and the sentinel has to survive.

mod common;

use ash_wasm_link::{link, LinkOptions};
use common::{object, read};
use wasmtime::{Config, Engine, Linker, MemoryType, Module, SharedMemory, Store};

fn threads_build() -> LinkOptions {
    LinkOptions {
        shared_memory: true,
        ..Default::default()
    }
}

#[test]
fn a_shared_memory_is_imported_and_the_data_is_passive() {
    let module = link(vec![object(".tdata", 0)], &threads_build()).expect("link");

    let mut features = wasmparser::WasmFeatures::default();
    features.insert(wasmparser::WasmFeatures::THREADS);
    wasmparser::Validator::new_with_features(features)
        .validate_all(&module)
        .expect("the output validates");

    let mut imported_memory = None;
    let mut passive = 0;
    let mut active = 0;
    for payload in wasmparser::Parser::new(0).parse_all(&module) {
        match payload.expect("parsing") {
            wasmparser::Payload::ImportSection(section) => {
                for group in section {
                  for import in group.expect("an import group") {
                    let (_, import) = import.expect("an import");
                    if let wasmparser::TypeRef::Memory(ty) = import.ty {
                        imported_memory =
                            Some((import.module.to_string(), import.name.to_string(), ty));
                    }
                  }
                }
            }
            wasmparser::Payload::DataSection(section) => {
                for segment in section {
                    match segment.expect("a data segment").kind {
                        wasmparser::DataKind::Passive => passive += 1,
                        wasmparser::DataKind::Active { .. } => active += 1,
                    }
                }
            }
            _ => {}
        }
    }

    let (module_name, name, ty) = imported_memory.expect("the memory is imported");
    assert_eq!((module_name.as_str(), name.as_str()), ("env", "memory"));
    assert!(ty.shared, "the memory has to be shared");
    assert!(
        ty.maximum.is_some(),
        "a shared memory has to declare a maximum"
    );
    assert_eq!((passive, active), (1, 0), "the data image stays passive");

    // The thread-local base starts at zero rather than at the main thread's
    // block: each instance is given one, and one that reads a thread-local
    // before it has been should not quietly get the main thread's.
    let read = read(&module);
    assert_eq!(read.globals[3].1, 0, "__tls_base");
}

/// The whole point: instantiate twice over one memory, and the second one
/// must leave what the first one wrote alone.
#[test]
fn the_second_instance_does_not_initialise_the_memory_again() {
    let bytes = link(vec![object(".tdata", 0)], &threads_build()).expect("link");

    let mut config = Config::new();
    config.wasm_threads(true);
    config.shared_memory(true);
    let engine = Engine::new(&config).expect("engine");
    let module = Module::new(&engine, &bytes).expect("compiling the module");

    // The minimum the module asks for, so instantiation is not refused for a
    // memory too small to hold the data.
    let wants = module
        .imports()
        .find_map(|import| match import.ty() {
            wasmtime::ExternType::Memory(ty) => Some(ty),
            _ => None,
        })
        .expect("an imported memory");
    let memory = SharedMemory::new(
        &engine,
        MemoryType::shared(wants.minimum() as u32, wants.maximum().unwrap() as u32),
    )
    .expect("a shared memory");

    let mut linker: Linker<()> = Linker::new(&engine);
    linker
        .define(&Store::new(&engine, ()), "env", "memory", memory.clone())
        .expect("defining the memory");

    // The first instance: its start function initialises the image.
    let mut first = Store::new(&engine, ());
    linker
        .instantiate(&mut first, &module)
        .expect("instantiating once");

    // Where the ordinary data landed, and what it holds -- the object puts
    // 0xaa at the start of its `.data`. A passive segment carries no address
    // of its own; the start function writes it at the bottom of the data
    // region, which is the top of the shadow stack, and `.data` is the first
    // thing placed there.
    let (_, image) = read(&bytes).data.first().cloned().expect("a data segment");
    let at = threads_build().stack_size as usize;
    let peek = |offset: usize| {
        // SAFETY: nothing else is running against this memory -- the two
        // instances have returned from their start functions and neither has
        // a thread of its own.
        unsafe { *memory.data()[offset].get() }
    };
    assert_eq!(image[0], 0xaa, "the object's own data");
    assert_eq!(peek(at), 0xaa, "the first instance initialised the memory");

    // Something the second instance must not undo.
    // SAFETY: as above.
    unsafe { *memory.data()[at].get() = 0x55 };

    let mut second = Store::new(&engine, ());
    linker
        .instantiate(&mut second, &module)
        .expect("instantiating twice");

    assert_eq!(
        peek(at),
        0x55,
        "the second instance wrote the initial data back over the first one's"
    );
}
