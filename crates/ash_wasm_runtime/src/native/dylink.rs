//! Loading a native library that arrives as a wasm side module.
//!
//! # Why a side module rather than a component
//!
//! An HDLL's contract is pointers. `DEFINE_PRIM` signatures pass `vbyte*`,
//! `varray*`, `vdynamic*` and `vclosure*` -- addresses in the heap ash's
//! collector scans -- and a library allocates objects that collector must
//! find, and calls closures back into the VM. A component owns its linear
//! memory and the canonical ABI copies values across the boundary, so none of
//! that survives one: a `hl.Bytes` would arrive as a copy, an object the
//! library allocated would be invisible to the collector, and a callback
//! would have nothing to call.
//!
//! A `dylink.0` side module is the mechanism that does carry it. It imports
//! the program's memory and its function table, so it works on the same heap
//! and a function pointer it returns is an index the program can call. It
//! then imports each runtime function it uses by name, exactly as an HDLL
//! links against libhl -- which is why `DEFINE_PRIM` needs no change here:
//! the resolver is found by name among the module's exports, and the protocol
//! runs as it always has.
//!
//! # What the loader has to do that a dynamic linker would
//!
//! A side module is position-independent and arrives knowing neither where
//! its data will sit nor where its function pointers will land. It asks, in
//! its `dylink.0` section, for so many bytes and so many table slots; the
//! loader takes the bytes from the program's own allocator so they are part
//! of one heap, appends the slots to the one table, and hands back both
//! addresses as the `__memory_base` and `__table_base` globals it imports.
//! Its data relocations are then applied by a function it exports for the
//! purpose.
//!
//! Loading happens before the program's own initialisation runs, so nothing
//! instantiates a module from inside a call the guest is making.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use anyhow::{anyhow, Result};
use wasmtime::{AsContextMut, Extern, Global, GlobalType, Instance, Module, Mutability, Ref, Store, Val};

use super::Host;

/// A library that has been loaded, and the primitives already handed out.
pub(crate) struct Library {
    instance: Instance,
    /// Name to the table index a previous lookup returned. A primitive
    /// resolved twice must be the same pointer, or the VM has two functions
    /// where the program has one.
    resolved: HashMap<String, i32>,
}

/// Every library loaded beside the program, by the name a program calls it.
#[derive(Default)]
pub(crate) struct Libraries {
    by_name: HashMap<String, Library>,
    /// The program's function table, which is the one every library shares.
    /// Kept here because a side module IMPORTS it, and an import is not an
    /// export: it cannot be read back off the library's own instance.
    table: Option<wasmtime::Table>,
}

impl Libraries {
    pub(crate) fn is_empty(&self) -> bool {
        self.by_name.is_empty()
    }

    /// Whether a library of this name is loaded. The guest asks before it
    /// asks for any primitive, the way it would `dlopen` first.
    pub(crate) fn contains(&self, lib: &str) -> bool {
        self.by_name.contains_key(lib)
    }

    pub(crate) fn names(&self) -> Vec<String> {
        let mut names: Vec<String> = self.by_name.keys().cloned().collect();
        names.sort();
        names
    }
}

/// The name a program calls a library, taken from its file name.
///
/// `sqlite.wasm` is the `sqlite` of `@:hlNative("sqlite", ...)`, which is the
/// same rule the native side uses for `sqlite.hdll`.
fn library_name(path: &Path) -> Option<String> {
    Some(path.file_stem()?.to_str()?.to_string())
}

/// Read only enough of a file to rule it out. The program's own module is in
/// this directory and is measured in megabytes.
fn head_looks_like_side_module(path: &Path) -> bool {
    use std::io::Read;
    let Ok(mut f) = std::fs::File::open(path) else {
        return false;
    };
    let mut head = [0u8; ash_wasm_link::SIDE_MODULE_PREFIX];
    let Ok(n) = f.read(&mut head) else {
        return false;
    };
    ash_wasm_link::looks_like_side_module(&head[..n])
}

/// Load every side module beside `program`.
///
/// A file that is not a side module is skipped rather than refused: the
/// directory holds the program's own module, and may hold anything else. One
/// that IS a side module and still will not load is reported, because it is
/// the interesting case and staying quiet about it leaves only "primitive not
/// found", which points at the wrong thing entirely.
pub(crate) async fn load_beside(
    store: &mut Store<Host>,
    main: &Instance,
    program: &Path,
) -> Result<Libraries> {
    let mut out = Libraries::default();
    // A bare file name has an EMPTY parent rather than none, and reading an
    // empty path finds nothing.
    let dir = match program.parent() {
        Some(p) if !p.as_os_str().is_empty() => p,
        _ => Path::new("."),
    };
    let Ok(entries) = std::fs::read_dir(dir) else {
        return Ok(out);
    };
    let mut paths: Vec<PathBuf> = entries
        .flatten()
        .map(|e| e.path())
        .filter(|p| {
            p.file_name() != program.file_name()
                && p.extension().and_then(|e| e.to_str()) == Some("wasm")
        })
        .collect();
    paths.sort();

    for path in paths {
        if !head_looks_like_side_module(&path) {
            continue;
        }
        let Ok(bytes) = std::fs::read(&path) else {
            continue;
        };
        let Ok(Some(side)) = ash_wasm_link::read_side_module(&bytes) else {
            continue;
        };
        let Some(name) = library_name(&path) else {
            continue;
        };
        match load_one(store, main, &bytes, &side).await {
            Ok((instance, table)) => {
                out.table = Some(table);
                out.by_name.insert(
                    name,
                    Library {
                        instance,
                        resolved: HashMap::new(),
                    },
                );
            }
            Err(e) => eprintln!("[ash] cannot load {}: {e}", path.display()),
        }
    }
    Ok(out)
}

async fn load_one(
    store: &mut Store<Host>,
    main: &Instance,
    bytes: &[u8],
    side: &ash_wasm_link::SideModule,
) -> Result<(Instance, wasmtime::Table)> {
    let module = Module::new(store.engine(), bytes)?;

    let memory = main
        .get_memory(&mut *store, "memory")
        .ok_or_else(|| anyhow!("the program exports no memory to share"))?;
    let table = main
        .get_table(&mut *store, "__indirect_function_table")
        .ok_or_else(|| {
            anyhow!(
                "the program exports no __indirect_function_table, so it was not built to \
                 host a native library. Build it with the library beside it."
            )
        })?;

    // Its data goes in the program's heap, taken from the program's own
    // allocator so that one allocator owns all of it.
    let memory_base = if side.memory_size > 0 {
        let malloc = main
            .get_typed_func::<i32, i32>(&mut *store, "malloc")
            .map_err(|_| anyhow!("the program exports no malloc to place this library's data"))?;
        let addr = malloc
            .call_async(&mut *store, side.memory_size as i32)
            .await?;
        if addr == 0 {
            return Err(anyhow!("no room for {} bytes of data", side.memory_size));
        }
        addr
    } else {
        0
    };

    // Its function pointers are appended to the one table, so an index it
    // returns is an index the program can call.
    let table_base = table.size(&mut *store) as i32;
    if side.table_size > 0 {
        table
            .grow(&mut *store, side.table_size as u64, Ref::Func(None))
            .map_err(|e| anyhow!("growing the function table by {}: {e}", side.table_size))?;
    }

    let immutable = |store: &mut Store<Host>, v: i32| {
        Global::new(
            store.as_context_mut(),
            GlobalType::new(wasmtime::ValType::I32, Mutability::Const),
            Val::I32(v),
        )
    };
    let memory_base_global = immutable(store, memory_base)?;
    let table_base_global = immutable(store, table_base)?;

    // Imports are supplied positionally, so they are resolved in the order
    // the module declares them.
    let mut imports: Vec<Extern> = Vec::new();
    for import in module.imports() {
        let found = match import.name() {
            "memory" => Some(Extern::Memory(memory)),
            "__indirect_function_table" => Some(Extern::Table(table)),
            "__memory_base" => Some(Extern::Global(memory_base_global)),
            "__table_base" => Some(Extern::Global(table_base_global)),
            name => main.get_export(&mut *store, name),
        };
        let found = found.ok_or_else(|| {
            anyhow!(
                "it imports {}::{}, which the program does not export. A runtime function \
                 is exported only when a library beside the program imports it, so this \
                 usually means the program was linked without this library present.",
                import.module(),
                import.name()
            )
        })?;
        imports.push(found);
    }

    let instance = Instance::new_async(&mut *store, &module, &imports).await?;

    // Its data holds addresses that were not known until the loader placed
    // it, and this is what writes them.
    if let Ok(relocs) = instance.get_typed_func::<(), ()>(&mut *store, "__wasm_apply_data_relocs") {
        relocs.call_async(&mut *store, ()).await?;
    }
    if let Ok(ctors) = instance.get_typed_func::<(), ()>(&mut *store, "__wasm_call_ctors") {
        ctors.call_async(&mut *store, ()).await?;
    }
    Ok((instance, table))
}

/// Find `symbol` in `lib` and return a pointer the program can call.
///
/// A wasm function is only callable through the table, so what a pointer
/// means here is a table index -- which is exactly what the program's own
/// function pointers are. Zero means not found, and is the null the
/// `DEFINE_PRIM` resolver protocol already treats as "this primitive is not
/// in this library".
pub(crate) fn resolve(
    mut store: impl AsContextMut<Data = Host>,
    lib: &str,
    symbol: &str,
) -> Result<i32> {
    let mut store = store.as_context_mut();
    let Some(library) = store.data().libraries.by_name.get(lib) else {
        return Ok(0);
    };
    if let Some(&index) = library.resolved.get(symbol) {
        return Ok(index);
    }
    let instance = library.instance;
    let Some(table) = store.data().libraries.table else {
        return Ok(0);
    };
    let Some(Extern::Func(func)) = instance.get_export(&mut store, symbol) else {
        return Ok(0);
    };
    let index = table.size(&mut store) as i32;
    table
        .grow(&mut store, 1, Ref::Func(Some(func)))
        .map_err(|e| anyhow!("no room in the function table for {lib}@{symbol}: {e}"))?;
    if let Some(library) = store.data_mut().libraries.by_name.get_mut(lib) {
        library.resolved.insert(symbol.to_string(), index);
    }
    Ok(index)
}
