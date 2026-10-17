//! A side module's WASI call finds the program's memory through its own export.
//!
//! A WASI function the host supplies reads and writes guest memory through the
//! export named `memory` of the instance that called it. A side module imports
//! the program's memory and exports none, so its first WASI call failed with
//! "missing required memory export". `expose_memory` exports the memory it
//! imports; this runs a module through both forms against a host function that
//! behaves as the WASI ones do.

use ash_wasm_link::dylink::expose_memory;
use wasm_encoder::{
    CodeSection, EntityType, ExportKind, ExportSection, Function, FunctionSection, ImportSection,
    Instruction, MemoryType, Module, TypeSection, ValType,
};
use wasmtime::{Engine, Extern, Instance, Linker, Memory, MemoryType as HostMemoryType, Store};

/// `(import "env" "memory") (import "wasi_snapshot_preview1" "random_get")`
/// and `run()`, which calls `random_get(16, 4)` and returns its result.
fn side_module(exports_memory: bool) -> Vec<u8> {
    let mut types = TypeSection::new();
    types
        .ty()
        .function([ValType::I32, ValType::I32], [ValType::I32]);
    types.ty().function([], [ValType::I32]);
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
    imports.import(
        "wasi_snapshot_preview1",
        "random_get",
        EntityType::Function(0),
    );
    let mut functions = FunctionSection::new();
    functions.function(1);
    let mut exports = ExportSection::new();
    exports.export("run", ExportKind::Func, 1);
    if exports_memory {
        exports.export("memory", ExportKind::Memory, 0);
    }
    let mut code = CodeSection::new();
    let mut run = Function::new([]);
    run.instruction(&Instruction::I32Const(16));
    run.instruction(&Instruction::I32Const(4));
    run.instruction(&Instruction::Call(0));
    run.instruction(&Instruction::End);
    code.function(&run);
    let mut module = Module::new();
    module
        .section(&types)
        .section(&imports)
        .section(&functions)
        .section(&exports)
        .section(&code);
    module.finish()
}

/// Run `run()` against a `random_get` that, like a WASI function, needs the
/// caller to export `memory`. Returns its result or the error it trapped with.
fn run(bytes: &[u8]) -> Result<i32, String> {
    let engine = Engine::default();
    let mut store = Store::new(&engine, ());
    let mut linker = Linker::new(&engine);
    let memory = Memory::new(&mut store, HostMemoryType::new(1, None)).unwrap();
    linker.define(&store, "env", "memory", memory).unwrap();
    linker
        .func_wrap(
            "wasi_snapshot_preview1",
            "random_get",
            |mut caller: wasmtime::Caller<'_, ()>, ptr: i32, len: i32| -> wasmtime::Result<i32> {
                let Some(Extern::Memory(memory)) = caller.get_export("memory") else {
                    wasmtime::bail!("missing required memory export");
                };
                memory.write(&mut caller, ptr as usize, &vec![7u8; len as usize])?;
                Ok(0)
            },
        )
        .unwrap();
    let module = wasmtime::Module::new(&engine, bytes).map_err(|e| e.to_string())?;
    let instance = linker
        .instantiate(&mut store, &module)
        .map_err(|e| e.to_string())?;
    let run = instance
        .get_typed_func::<(), i32>(&mut store, "run")
        .map_err(|e| e.to_string())?;
    let result = run.call(&mut store, ()).map_err(|e| format!("{e:#}"))?;
    let written = memory.data(&store)[16..20].to_vec();
    assert_eq!(
        written,
        [7, 7, 7, 7],
        "the host wrote into the shared memory"
    );
    Ok(result)
}

#[test]
fn a_module_that_exports_no_memory_fails_its_wasi_call() {
    let err = run(&side_module(false)).unwrap_err();
    assert!(err.contains("missing required memory export"), "{err}");
}

#[test]
fn exposing_the_imported_memory_lets_the_wasi_call_through() {
    let exposed = expose_memory(&side_module(false)).unwrap();
    assert_eq!(run(&exposed), Ok(0));
}

#[test]
fn a_module_that_already_exports_memory_is_left_alone() {
    let bytes = side_module(true);
    assert_eq!(expose_memory(&bytes).unwrap(), bytes);
    assert_eq!(run(&bytes), Ok(0));
}

#[test]
fn a_module_without_an_imported_memory_is_left_alone() {
    let mut module = Module::new();
    let mut types = TypeSection::new();
    types.ty().function([], []);
    let mut functions = FunctionSection::new();
    functions.function(0);
    let mut exports = ExportSection::new();
    exports.export("f", ExportKind::Func, 0);
    let mut code = CodeSection::new();
    let mut f = Function::new([]);
    f.instruction(&Instruction::End);
    code.function(&f);
    module
        .section(&types)
        .section(&functions)
        .section(&exports)
        .section(&code);
    let bytes = module.finish();
    assert_eq!(expose_memory(&bytes).unwrap(), bytes);
    // Instantiable on its own, so the unchanged bytes are a valid module.
    let engine = Engine::default();
    let mut store = Store::new(&engine, ());
    let module = wasmtime::Module::new(&engine, &bytes).unwrap();
    Instance::new(&mut store, &module, &[]).unwrap();
}
