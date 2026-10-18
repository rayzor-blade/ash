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
use std::sync::Arc;

use anyhow::{Result, anyhow};
use wasmtime::{
    AsContextMut, Extern, Global, GlobalType, Instance, Module, Mutability, Ref, Store, Val,
};

use super::Host;

/// A library that has been loaded, and the primitives already handed out.
pub(crate) struct Library {
    instance: Instance,
    /// Export name to the table index it was given. Fixed when the libraries
    /// are loaded, so a primitive resolved twice is the same pointer and
    /// every thread's table can be laid out the same way.
    resolved: Arc<HashMap<String, i32>>,
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

/// How loading the libraries laid out the program's table and memory, in an
/// order another instance of the program can repeat.
///
/// A thread is a second instance of the program, and a table belongs to an
/// instance: the slots loading appended to the first instance's table do not
/// exist in the thread's, so a function pointer the first instance handed out
/// -- a primitive's -- would index past the end of it. The thread instead
/// loads the same libraries again, over the same shared memory, at the bases
/// the first load chose, and appends the same slots in the same order.
pub(crate) struct Layout {
    libraries: Vec<Placed>,
    /// The slots appended after the last library, one per exported function:
    /// the table index, the library and the export.
    primitives: Vec<(i32, String, String)>,
}

/// One library as the first load placed it.
struct Placed {
    name: String,
    /// Without its initial data, which the first instance already wrote and
    /// which a running program has since changed.
    module: Module,
    memory_base: i32,
    table_base: i32,
    table_size: u32,
    /// What the global offset table was answered with.
    got: Vec<(GotKind, String, i32)>,
    /// The slots appended for a function whose address the library takes.
    slots: Vec<(i32, Owner, String)>,
    resolved: Arc<HashMap<String, i32>>,
}

/// Which instance a function in the table came from.
#[derive(Clone, Copy)]
enum Owner {
    Library,
    Program,
}

/// What loading one library decided, for a thread to repeat.
struct Placement {
    memory_base: i32,
    table_base: i32,
    table_size: u32,
    got: Vec<(GotKind, String, i32)>,
    slots: Vec<(i32, Owner, String)>,
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
///
/// With `for_threads`, also the [`Layout`] a thread needs to repeat the load.
pub(crate) async fn load_beside(
    store: &mut Store<Host>,
    linker: &wasmtime::Linker<Host>,
    main: &Instance,
    program: &Path,
    for_threads: bool,
) -> Result<(Libraries, Option<Layout>)> {
    let mut out = Libraries::default();
    let mut placed: Vec<Placed> = Vec::new();
    // The order they loaded in, which the table was laid out in.
    let mut order: Vec<String> = Vec::new();
    // A bare file name has an EMPTY parent rather than none, and reading an
    // empty path finds nothing.
    let dir = match program.parent() {
        Some(p) if !p.as_os_str().is_empty() => p,
        _ => Path::new("."),
    };
    let Ok(entries) = std::fs::read_dir(dir) else {
        return Ok((out, None));
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
            Ok((instance, table, placement)) => {
                out.table = Some(table);
                order.push(name.clone());
                // Compiled again only for a program that can start threads.
                let module = if for_threads {
                    Some(library_module(store.engine(), &bytes, true)?)
                } else {
                    None
                };
                if let Some(module) = module {
                    placed.push(Placed {
                        name: name.clone(),
                        module,
                        memory_base: placement.memory_base,
                        table_base: placement.table_base,
                        table_size: placement.table_size,
                        got: placement.got,
                        slots: placement.slots,
                        resolved: Arc::default(),
                    });
                }
                out.by_name.insert(
                    name,
                    Library {
                        instance,
                        resolved: Arc::default(),
                    },
                );
            }
            Err(e) => eprintln!("[ash] cannot load {}: {e}", path.display()),
        }
    }

    // Every exported function gets its slot now. A primitive resolved on
    // demand would be appended to whichever thread's table asked first, and
    // the index the guest then keeps in shared memory would mean nothing to
    // the others.
    let mut primitives: Vec<(i32, String, String)> = Vec::new();
    if let Some(table) = out.table {
        for name in order {
            let Some(instance) = out.by_name.get(&name).map(|l| l.instance) else {
                continue;
            };
            let mut funcs: Vec<(String, wasmtime::Func)> = instance
                .exports(&mut *store)
                .filter_map(|e| {
                    let export = e.name().to_string();
                    e.into_func().map(|f| (export, f))
                })
                .collect();
            funcs.sort_by(|a, b| a.0.cmp(&b.0));
            let mut resolved = HashMap::new();
            for (export, func) in funcs {
                let index = table.size(&mut *store) as i32;
                table
                    .grow(&mut *store, 1, Ref::Func(Some(func)))
                    .map_err(|e| {
                        anyhow!("no room in the function table for {name}@{export}: {e}")
                    })?;
                resolved.insert(export.clone(), index);
                primitives.push((index, name.clone(), export));
            }
            let resolved = Arc::new(resolved);
            if let Some(library) = out.by_name.get_mut(&name) {
                library.resolved = resolved.clone();
            }
            if let Some(p) = placed.iter_mut().find(|p| p.name == name) {
                p.resolved = resolved;
            }
        }
    }
    // Nothing to repeat for a program that loaded no library.
    let layout = (for_threads && !placed.is_empty()).then_some(Layout {
        libraries: placed,
        primitives,
    });
    Ok((out, layout))
}

/// A library compiled for this host. `for_thread` leaves its initial data
/// out: see [`Placed::module`].
fn library_module(engine: &wasmtime::Engine, bytes: &[u8], for_thread: bool) -> Result<Module> {
    let bytes = if for_thread {
        std::borrow::Cow::Owned(ash_wasm_link::dylink::without_initial_data(bytes)?)
    } else {
        std::borrow::Cow::Borrowed(bytes)
    };
    // The library imports the program's memory, so it is given an export of
    // it: the host's WASI functions find the memory to use through the export
    // `memory` of whichever instance calls them.
    Module::new(
        engine,
        ash_wasm_link::dylink::expose_memory(&ash_wasm_link::waits::instrument(&bytes)?)?,
    )
    .map_err(Into::into)
}

async fn load_one(
    store: &mut Store<Host>,
    linker: &wasmtime::Linker<Host>,
    main: &Instance,
    bytes: &[u8],
    side: &ash_wasm_link::SideModule,
) -> Result<(Instance, wasmtime::Table, Placement)> {
    let module = library_module(store.engine(), bytes, false)?;
    let (memory, table) = program_memory_and_table(store, main)?;

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

    // The global offset table is the part that cannot be answered yet. A
    // `GOT.mem.x` or `GOT.func.x` import is a mutable global that must end up
    // holding where `x` was placed -- and for a symbol the library itself
    // defines, that is not known until it has been instantiated. So they are
    // supplied as zeroes here and filled in below, before anything reads
    // them.
    let (imports, got) = supply_imports(
        store,
        linker,
        main,
        &module,
        &Bases {
            memory,
            table,
            memory_base,
            table_base,
        },
        None,
    )?;

    let instance = Instance::new_async(&mut *store, &module, &imports).await?;

    // Now that it has been placed, say where. A library's own symbol comes
    // from its exports -- a data symbol as a global holding its address, a
    // function as a function that has to be given a table slot; one it does
    // not define is the program's, and comes from there.
    let mut placed: HashMap<String, i32> = HashMap::new();
    let mut answers: Vec<(GotKind, String, i32)> = Vec::new();
    let mut slots: Vec<(i32, Owner, String)> = Vec::new();
    for (kind, name, slot) in got {
        let value = match kind {
            GotKind::Data => data_address(&mut *store, &instance, main, &name, memory_base),
            GotKind::Function => function_address(
                &mut *store,
                &instance,
                main,
                table,
                &name,
                &mut placed,
                &mut slots,
            )?,
        };
        let value = value.ok_or_else(|| {
            anyhow!("it needs the address of {name}, which neither it nor the program defines")
        })?;
        slot.set(&mut *store, Val::I32(value))?;
        answers.push((kind, name, value));
    }

    // Its data holds addresses that were not known until the loader placed
    // it, and this is what writes them.
    if let Ok(relocs) = instance.get_typed_func::<(), ()>(&mut *store, "__wasm_apply_data_relocs") {
        relocs.call_async(&mut *store, ()).await?;
    }
    if let Ok(ctors) = instance.get_typed_func::<(), ()>(&mut *store, "__wasm_call_ctors") {
        ctors.call_async(&mut *store, ()).await?;
    }
    Ok((
        instance,
        table,
        Placement {
            memory_base,
            table_base,
            table_size: side.table_size,
            got: answers,
            slots,
        },
    ))
}

/// The program's memory and function table, which every library shares.
///
/// The memory is an `Extern` rather than a `Memory`, because a program built
/// for threads exports a shared memory and `get_memory` answers `None` for
/// one. The library imports whichever kind the program has.
fn program_memory_and_table(
    store: &mut Store<Host>,
    main: &Instance,
) -> Result<(Extern, wasmtime::Table)> {
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
    Ok((memory, table))
}

/// A global offset table import's slot: its kind, its name and the global.
type GotSlot = (GotKind, String, Global);

/// What a library is placed at in one instance of the program.
struct Bases {
    memory: Extern,
    table: wasmtime::Table,
    memory_base: i32,
    table_base: i32,
}

/// The imports of `module`, in the order it declares them (they are supplied
/// positionally), with the global offset table's slots alongside.
///
/// A thread already knows what the table was answered with, and passes it as
/// `known`; the first load passes none and fills the slots in itself.
fn supply_imports(
    store: &mut Store<Host>,
    linker: &wasmtime::Linker<Host>,
    main: &Instance,
    module: &Module,
    bases: &Bases,
    known: Option<&[(GotKind, String, i32)]>,
) -> Result<(Vec<Extern>, Vec<GotSlot>)> {
    let immutable = |store: &mut Store<Host>, v: i32| {
        Global::new(
            store.as_context_mut(),
            GlobalType::new(wasmtime::ValType::I32, Mutability::Const),
            Val::I32(v),
        )
    };
    let memory_base_global = immutable(store, bases.memory_base)?;
    let table_base_global = immutable(store, bases.table_base)?;

    let mut imports: Vec<Extern> = Vec::new();
    let mut got: Vec<(GotKind, String, Global)> = Vec::new();
    for import in module.imports() {
        let kind = match import.module() {
            "GOT.mem" => Some(GotKind::Data),
            "GOT.func" => Some(GotKind::Function),
            _ => None,
        };
        if let Some(kind) = kind {
            let initial = match known {
                None => 0,
                Some(known) => known
                    .iter()
                    .find(|(k, n, _)| *k == kind && n == import.name())
                    .map(|(_, _, v)| *v)
                    .ok_or_else(|| {
                        anyhow!("{} was not answered by the first load", import.name())
                    })?,
            };
            let slot = Global::new(
                store.as_context_mut(),
                GlobalType::new(wasmtime::ValType::I32, Mutability::Var),
                Val::I32(initial),
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
                "memory" => Some(bases.memory.clone()),
                "__indirect_function_table" => Some(Extern::Table(bases.table)),
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
    Ok((imports, got))
}

/// Load the libraries again in a thread's instance of the program, laid out
/// as the first load laid them out.
///
/// The memory is shared, so the libraries' data is already there and is not
/// written again: no initial data, no relocations, no constructors. What a
/// thread has to make for itself is what an instance owns -- the library
/// instances, and the slots in its own table at the same indices.
pub(crate) async fn replay(
    store: &mut Store<Host>,
    linker: &wasmtime::Linker<Host>,
    main: &Instance,
    layout: &Layout,
) -> Result<Libraries> {
    let (memory, table) = program_memory_and_table(store, main)?;
    let mut out = Libraries {
        table: Some(table),
        ..Libraries::default()
    };
    let mut instances: HashMap<&str, Instance> = HashMap::new();
    let at = |store: &mut Store<Host>, want: i32, what: &str| -> Result<()> {
        let have = table.size(&mut *store) as i32;
        if have == want {
            Ok(())
        } else {
            Err(anyhow!(
                "this thread's function table is {have} long where {what} goes at {want}"
            ))
        }
    };
    for lib in &layout.libraries {
        at(store, lib.table_base, &lib.name)?;
        if lib.table_size > 0 {
            table
                .grow(&mut *store, u64::from(lib.table_size), Ref::Func(None))
                .map_err(|e| anyhow!("growing the function table for {}: {e}", lib.name))?;
        }
        let (imports, _) = supply_imports(
            store,
            linker,
            main,
            &lib.module,
            &Bases {
                memory: memory.clone(),
                table,
                memory_base: lib.memory_base,
                table_base: lib.table_base,
            },
            Some(&lib.got),
        )?;
        let instance = Instance::new_async(&mut *store, &lib.module, &imports)
            .await
            .map_err(|e| anyhow!("instantiating {} for the thread: {e}", lib.name))?;
        for (index, owner, export) in &lib.slots {
            at(store, *index, export)?;
            let from = match owner {
                Owner::Library => &instance,
                Owner::Program => main,
            };
            let Some(Extern::Func(func)) = from.get_export(&mut *store, export) else {
                return Err(anyhow!(
                    "{export} is not a function this thread's instance has"
                ));
            };
            table
                .grow(&mut *store, 1, Ref::Func(Some(func)))
                .map_err(|e| anyhow!("no room in the function table for {export}: {e}"))?;
        }
        instances.insert(&lib.name, instance);
        out.by_name.insert(
            lib.name.clone(),
            Library {
                instance,
                resolved: lib.resolved.clone(),
            },
        );
    }
    for (index, lib, export) in &layout.primitives {
        at(store, *index, export)?;
        let Some(instance) = instances.get(lib.as_str()) else {
            return Err(anyhow!("{lib} was not loaded for this thread"));
        };
        let Some(Extern::Func(func)) = instance.get_export(&mut *store, export) else {
            return Err(anyhow!("{lib}@{export} is not a function this thread has"));
        };
        table
            .grow(&mut *store, 1, Ref::Func(Some(func)))
            .map_err(|e| anyhow!("no room in the function table for {lib}@{export}: {e}"))?;
    }
    Ok(out)
}

/// Which of the two global offset tables an import belongs to.
#[derive(Clone, Copy, PartialEq, Eq)]
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
    memory_base: i32,
) -> Option<i32> {
    // The library exports its own data symbols as offsets from `__memory_base`;
    // the program is not position-independent, so its exports are absolute.
    for (owner, base) in [(instance, memory_base), (main, 0)] {
        if let Some(Extern::Global(g)) = owner.get_export(&mut *store, name)
            && let Some(v) = g.get(&mut *store).i32()
        {
            return Some(v + base);
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
    slots: &mut Vec<(i32, Owner, String)>,
) -> Result<Option<i32>> {
    if let Some(&index) = placed.get(name) {
        return Ok(Some(index));
    }
    let mut found = None;
    for (owner, from) in [(Owner::Library, instance), (Owner::Program, main)] {
        if let Some(Extern::Func(f)) = from.get_export(&mut *store, name) {
            found = Some((owner, f));
            break;
        }
    }
    let Some((owner, func)) = found else {
        return Ok(None);
    };
    let index = table.size(&mut *store) as i32;
    table
        .grow(&mut *store, 1, Ref::Func(Some(func)))
        .map_err(|e| anyhow!("no room in the function table for {name}: {e}"))?;
    placed.insert(name.to_string(), index);
    slots.push((index, owner, name.to_string()));
    Ok(Some(index))
}

/// Find `symbol` in `lib` and return a pointer the program can call.
///
/// A wasm function is only callable through the table, so what a pointer
/// means here is a table index -- which is exactly what the program's own
/// function pointers are. Zero means not found, and is the null the
/// `DEFINE_PRIM` resolver protocol already treats as "this primitive is not
/// in this library".
///
/// Every exported function was given its slot when the libraries were
/// loaded, so this only looks one up.
pub(crate) fn resolve(
    store: impl AsContextMut<Data = Host>,
    lib: &str,
    symbol: &str,
) -> Result<i32> {
    let store = store.as_context();
    Ok(store
        .data()
        .libraries
        .by_name
        .get(lib)
        .and_then(|library| library.resolved.get(symbol))
        .copied()
        .unwrap_or(0))
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
            |mut caller: wasmtime::Caller<'_, Host>,
             lib: i32,
             lib_len: i32,
             sym: i32,
             sym_len: i32|
             -> i32 {
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
