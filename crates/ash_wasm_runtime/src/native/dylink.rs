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
    linker: &wasmtime::Linker<Host>,
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
        match load_one(store, linker, main, &bytes, &side).await {
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
    linker: &wasmtime::Linker<Host>,
    main: &Instance,
    bytes: &[u8],
    side: &ash_wasm_link::SideModule,
) -> Result<(Instance, wasmtime::Table)> {
    let module = Module::new(store.engine(), bytes)?;

    // As an `Extern` rather than a `Memory`, because a program built for
    // threads exports a shared memory and `get_memory` answers `None` for one.
    // The library imports whichever kind the program has.
    let memory = main
        .get_export(&mut *store, "memory")
        .filter(|e| matches!(e, Extern::Memory(_) | Extern::SharedMemory(_)))
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
    //
    // The global offset table is the part that cannot be answered yet. A
    // `GOT.mem.x` or `GOT.func.x` import is a mutable global that must end up
    // holding where `x` was placed -- and for a symbol the library itself
    // defines, that is not known until it has been instantiated. So they are
    // supplied as zeroes here and filled in below, before anything reads
    // them.
    let mut imports: Vec<Extern> = Vec::new();
    let mut got: Vec<(GotKind, String, Global)> = Vec::new();
    for import in module.imports() {
        let kind = match import.module() {
            "GOT.mem" => Some(GotKind::Data),
            "GOT.func" => Some(GotKind::Function),
            _ => None,
        };
        if let Some(kind) = kind {
            let slot = Global::new(
                store.as_context_mut(),
                GlobalType::new(wasmtime::ValType::I32, Mutability::Var),
                Val::I32(0),
            )?;
            got.push((kind, import.name().to_string(), slot));
            imports.push(Extern::Global(slot));
            continue;
        }
        // `env` is mostly the program: its memory, its table, and the runtime
        // functions it exports. But a library whose work is the host's --
        // drawing to a canvas, say -- reaches the host through `env` too, so
        // a name the program does not export is asked of the host before it
        // is called missing. Everything outside `env`, WASI above all, is the
        // host's outright, and the host answers a library exactly as it
        // answers the program.
        let found = if import.module() == "env" {
            match import.name() {
                "memory" => Some(memory.clone()),
                "__indirect_function_table" => Some(Extern::Table(table)),
                "__memory_base" => Some(Extern::Global(memory_base_global)),
                "__table_base" => Some(Extern::Global(table_base_global)),
                name => main
                    .get_export(&mut *store, name)
                    .or_else(|| linker.get(&mut *store, "env", name)),
            }
        } else {
            linker.get(&mut *store, import.module(), import.name())
        };
        let found = found.ok_or_else(|| {
            anyhow!(
                "it imports {}::{}, which nothing here provides. A runtime function is \
                 exported only when a library beside the program imports it, so this \
                 usually means the program was linked without this library present.",
                import.module(),
                import.name()
            )
        })?;
        imports.push(found);
    }

    let instance = Instance::new_async(&mut *store, &module, &imports).await?;

    // Now that it has been placed, say where. A library's own symbol comes
    // from its exports -- a data symbol as a global holding its address, a
    // function as a function that has to be given a table slot; one it does
    // not define is the program's, and comes from there.
    let mut placed: HashMap<String, i32> = HashMap::new();
    for (kind, name, slot) in got {
        let value = match kind {
            GotKind::Data => data_address(&mut *store, &instance, main, &name),
            GotKind::Function => {
                function_address(&mut *store, &instance, main, table, &name, &mut placed)?
            }
        };
        let value = value.ok_or_else(|| {
            anyhow!("it needs the address of {name}, which neither it nor the program defines")
        })?;
        slot.set(&mut *store, Val::I32(value))?;
    }

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

/// Which of the two global offset tables an import belongs to.
#[derive(Clone, Copy)]
enum GotKind {
    /// An address in linear memory.
    Data,
    /// A slot in the function table.
    Function,
}

/// Where a data symbol was placed, as its own module reports it: a
/// position-independent module exports each such symbol as a global holding
/// its address.
fn data_address(
    store: &mut Store<Host>,
    instance: &Instance,
    main: &Instance,
    name: &str,
) -> Option<i32> {
    for owner in [instance, main] {
        if let Some(Extern::Global(g)) = owner.get_export(&mut *store, name) {
            if let Some(v) = g.get(&mut *store).i32() {
                return Some(v);
            }
        }
    }
    None
}

/// A table slot holding `name`, appending one if it has none yet.
///
/// Cached, because a function whose address is taken twice must be the same
/// pointer both times or a program comparing them sees two functions where it
/// has one.
fn function_address(
    store: &mut Store<Host>,
    instance: &Instance,
    main: &Instance,
    table: wasmtime::Table,
    name: &str,
    placed: &mut HashMap<String, i32>,
) -> Result<Option<i32>> {
    if let Some(&index) = placed.get(name) {
        return Ok(Some(index));
    }
    let mut func = None;
    for owner in [instance, main] {
        if let Some(Extern::Func(f)) = owner.get_export(&mut *store, name) {
            func = Some(f);
            break;
        }
    }
    let Some(func) = func else {
        return Ok(None);
    };
    let index = table.size(&mut *store) as i32;
    table
        .grow(&mut *store, 1, Ref::Func(Some(func)))
        .map_err(|e| anyhow!("no room in the function table for {name}: {e}"))?;
    placed.insert(name.to_string(), index);
    Ok(Some(index))
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

/// How the guest reaches a native library that was loaded beside it.
///
/// Two imports, and they are `dlopen` and `dlsym` under other names, because
/// that is what `crate::aot_native` on every other target calls at exactly
/// this point. The library is already loaded by the time either is asked --
/// see [`dylink`] -- so "open" is a lookup, and "sym" answers with a table
/// index, which is what a function pointer is in a wasm module.
///
/// Answering zero is not an error. It is the null the `DEFINE_PRIM` resolver
/// protocol already reads as "not in this library", and the call site raises
/// the same "not loaded" a native binary raises for a missing HDLL -- only if
/// the primitive is actually reached.
pub(crate) fn install(linker: &mut wasmtime::Linker<Host>) -> Result<()> {
    linker
        .func_wrap(
            super::fibers::YIELD_MODULE,
            "ash_host_dlopen",
            |mut caller: wasmtime::Caller<'_, Host>, name: i32, name_len: i32| -> i32 {
                let Some(name) = super::guest_slice(&mut caller, name, name_len) else {
                    return 0;
                };
                let name = String::from_utf8_lossy(&name).into_owned();
                caller.data().libraries.contains(&name) as i32
            },
        )
        .map_err(|e| anyhow!("installing the library import: {e}"))?;

    linker
        .func_wrap(
            super::fibers::YIELD_MODULE,
            "ash_host_dlsym",
            |mut caller: wasmtime::Caller<'_, Host>, lib: i32, lib_len: i32, sym: i32, sym_len: i32| -> i32 {
                let Some(lib) = super::guest_slice(&mut caller, lib, lib_len) else {
                    return 0;
                };
                let Some(sym) = super::guest_slice(&mut caller, sym, sym_len) else {
                    return 0;
                };
                let lib = String::from_utf8_lossy(&lib).into_owned();
                let sym = String::from_utf8_lossy(&sym).into_owned();
                resolve(&mut caller, &lib, &sym).unwrap_or(0)
            },
        )
        .map_err(|e| anyhow!("installing the symbol import: {e}"))?;
    Ok(())
}
