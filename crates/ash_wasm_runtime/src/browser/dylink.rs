//! Loading a native library that arrives as a wasm side module, in a page.
//!
//! The steps are `native::dylink`'s, which says why a side module and what a
//! loader owes one: take its data from the program's allocator, append its
//! function slots to the program's table, answer its global offset table, then
//! let it apply its own data relocations and run its constructors. Here they
//! are made against `WebAssembly.Module` and `WebAssembly.Instance`.
//!
//! The bytes arrive with the program, already fetched: a guest asking for a
//! library is inside a synchronous call and cannot wait for a fetch. So every
//! library is loaded before the entrypoint runs, as `ash run` does, and the
//! two imports the guest asks through answer from what was loaded. Compiling
//! synchronously is allowed here because the program runs in a Worker.
//!
//! A thread's agent has its own table, so each thread loads the libraries
//! again: [`load`] leaves a layout describing where everything went, the page
//! hands it to the agent with the thread, and [`replay`] makes the same
//! libraries in that agent's instance at the same table indices.

use std::collections::HashMap;

use js_sys::{Array, Function, Object, Reflect, Uint8Array, WebAssembly};
use wasm_bindgen::{JsCast, JsValue};

/// A loaded library, and the primitives already handed out.
struct Library {
    exports: Object,
    /// Export name to the table index it was given, fixed when the libraries
    /// are loaded: a primitive resolved twice is the same pointer, and every
    /// thread's table can be laid out the same way.
    resolved: HashMap<String, i32>,
}

/// Every library loaded beside the program, by the name a program calls it.
#[derive(Default)]
pub struct Libraries {
    by_name: HashMap<String, Library>,
    /// The program's function table, which every library shares.
    table: Option<WebAssembly::Table>,
    /// How the first load laid the table and memory out, for a thread to
    /// repeat; a plain object a Worker can be sent. Absent for a program
    /// that cannot start threads.
    layout: Option<JsValue>,
}

impl Libraries {
    pub fn is_empty(&self) -> bool {
        self.by_name.is_empty()
    }

    pub fn names(&self) -> Vec<String> {
        let mut names: Vec<String> = self.by_name.keys().cloned().collect();
        names.sort();
        names
    }

    /// Whether a library of this name is loaded; the guest's `dlopen`.
    pub fn contains(&self, lib: &str) -> bool {
        self.by_name.contains_key(lib)
    }

    /// A table index for `symbol` in `lib`, or 0 when it has none; the
    /// guest's `dlsym`.
    pub fn resolve(&self, lib: &str, symbol: &str) -> i32 {
        self.by_name
            .get(lib)
            .and_then(|library| library.resolved.get(symbol))
            .copied()
            .unwrap_or(0)
    }

    /// What a thread needs to load the same libraries; see [`replay`].
    pub fn layout(&self) -> Option<JsValue> {
        self.layout.clone()
    }
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
    /// What the global offset table was answered with.
    got: Vec<(GotKind, String, i32)>,
    /// The slots appended for a function whose address the library takes.
    slots: Vec<(i32, Owner, String)>,
    /// The thread-local block the first instance started with, to give every
    /// other instance a copy of. See [`capture_tls`].
    tls: Option<Vec<u8>>,
}

/// A library as the first load placed it.
struct Placed {
    name: String,
    /// Without its initial data, which the first instance already wrote and
    /// which a running program has since changed.
    module: WebAssembly::Module,
    placement: Placement,
}

/// Load each library in `libraries`, an object from the name a program calls
/// it to its bytes, against the program instantiated with `host_imports`.
///
/// One that will not load is reported and skipped: a program that never
/// reaches its primitives still runs, and one that does raises "not loaded"
/// there, after this message has said why.
///
/// With `for_threads`, the result also carries the layout a thread needs.
pub fn load(
    libraries: &Object,
    main_exports: &Object,
    host_imports: &Object,
    for_threads: bool,
) -> Result<Libraries, JsValue> {
    let mut out = Libraries::default();
    let mut placed: Vec<Placed> = Vec::new();
    // The order they loaded in, which the table was laid out in.
    let mut order: Vec<String> = Vec::new();
    let mut names: Vec<String> = Object::keys(libraries)
        .iter()
        .filter_map(|k| k.as_string())
        .collect();
    names.sort();
    for name in names {
        let Ok(bytes) = Reflect::get(libraries, &name.as_str().into()) else {
            continue;
        };
        let Some(bytes) = bytes.dyn_ref::<Uint8Array>() else {
            report(&format!("[ash] cannot load {name}: its bytes are not a Uint8Array"));
            continue;
        };
        let bytes = bytes.to_vec();
        match load_one(&bytes, main_exports, host_imports) {
            Ok((exports, table, placement)) => {
                out.table = Some(table);
                order.push(name.clone());
                if for_threads {
                    placed.push(Placed {
                        name: name.clone(),
                        module: thread_module(&bytes)?,
                        placement,
                    });
                }
                out.by_name.insert(
                    name,
                    Library {
                        exports,
                        resolved: HashMap::new(),
                    },
                );
            }
            Err(e) => report(&format!("[ash] cannot load {name}: {}", describe(&e))),
        }
    }

    // Every exported function gets its slot now. A primitive resolved on
    // demand would be appended to whichever thread's table asked first, and
    // the index the guest then keeps in shared memory would mean nothing to
    // the others.
    let mut primitives: Vec<(i32, String, String)> = Vec::new();
    if let Some(table) = out.table.clone() {
        for name in &order {
            let Some(library) = out.by_name.get_mut(name) else {
                continue;
            };
            let mut exported: Vec<(String, Function)> = Object::keys(&library.exports)
                .iter()
                .filter_map(|key| {
                    let key = key.as_string()?;
                    let func = Reflect::get(&library.exports, &key.as_str().into())
                        .ok()?
                        .dyn_into::<Function>()
                        .ok()?;
                    Some((key, func))
                })
                .collect();
            exported.sort_by(|a, b| a.0.cmp(&b.0));
            for (export, func) in exported {
                let index = append(&table, &func)?;
                library.resolved.insert(export.clone(), index);
                primitives.push((index, name.clone(), export));
            }
        }
    }
    // Nothing to repeat for a program that loaded no library.
    if for_threads && !placed.is_empty() {
        out.layout = Some(layout_object(&placed, &out, &primitives)?);
    }
    Ok(out)
}

/// A library compiled for a thread's instance: no initial data, see
/// [`Placed::module`].
fn thread_module(bytes: &[u8]) -> Result<WebAssembly::Module, JsValue> {
    let bytes = ash_wasm_link::dylink::without_initial_data(bytes)
        .and_then(|b| ash_wasm_link::waits::instrument(&b))
        .and_then(|b| ash_wasm_link::dylink::expose_tls_base(&b))
        .map_err(|e| JsValue::from_str(&e.to_string()))?;
    WebAssembly::Module::new(&Uint8Array::from(bytes.as_slice()))
}

/// Which of the two global offset tables an import belongs to.
#[derive(Clone, Copy, PartialEq, Eq)]
enum GotKind {
    Data,
    Function,
}

fn load_one(
    bytes: &[u8],
    main_exports: &Object,
    host_imports: &Object,
) -> Result<(Object, WebAssembly::Table, Placement), JsValue> {
    let side = ash_wasm_link::read_side_module(bytes)
        .map_err(|e| JsValue::from_str(&e.to_string()))?
        .ok_or_else(|| JsValue::from_str("it is not a side module (no dylink.0 section)"))?;
    let bytes = ash_wasm_link::waits::instrument(bytes)
        .and_then(|b| ash_wasm_link::dylink::expose_tls_base(&b))
        .map_err(|e| JsValue::from_str(&e.to_string()))?;
    let module = WebAssembly::Module::new(&Uint8Array::from(bytes.as_slice()))?;

    let (memory, table) = program_memory_and_table(main_exports)?;

    // Its data goes in the program's heap, from the program's own allocator.
    let memory_base = if side.memory_size > 0 {
        let malloc: Function = Reflect::get(main_exports, &"malloc".into())?
            .dyn_into()
            .map_err(|_| JsValue::from_str("the program exports no malloc to place its data"))?;
        let addr = malloc
            .call1(&JsValue::UNDEFINED, &(side.memory_size as i32).into())?
            .as_f64()
            .unwrap_or(0.0) as i32;
        if addr == 0 {
            return Err(format!("no room for {} bytes of data", side.memory_size).into());
        }
        addr
    } else {
        0
    };
    // Its function slots are appended to the one table.
    let table_base = table.length() as i32;
    if side.table_size > 0 {
        table.grow(side.table_size)?;
    }

    // The global offset table cannot be answered until the library is placed,
    // so its entries start as zeroes and are filled before anything reads them.
    let (imports, got) = supply_imports(
        &module,
        main_exports,
        host_imports,
        &Bases {
            memory: memory.clone(),
            table: table.clone(),
            memory_base,
            table_base,
        },
        None,
    )?;

    let instance = WebAssembly::Instance::new(&module, &imports)?;
    let exports = instance.exports();

    // Now that it has been placed, say where: its own symbols from its exports,
    // anything else from the program's.
    let mut placed: HashMap<String, i32> = HashMap::new();
    let mut answers: Vec<(GotKind, String, i32)> = Vec::new();
    let mut slots: Vec<(i32, Owner, String)> = Vec::new();
    for (kind, name, slot) in got {
        let value = match kind {
            GotKind::Data => data_address(&exports, main_exports, &name, memory_base)?,
            GotKind::Function => function_address(
                &exports,
                main_exports,
                &table,
                &name,
                &mut placed,
                &mut slots,
            )?,
        };
        let value = value.ok_or_else(|| {
            JsValue::from_str(&format!(
                "it needs the address of {name}, which neither it nor the program defines"
            ))
        })?;
        slot.set_value(&value.into());
        answers.push((kind, name, value));
    }

    let mut tls = None;
    for init in ["__wasm_apply_data_relocs", "__wasm_call_ctors"] {
        // After the relocations that patch it and before any constructor
        // that writes to it: this is the image a thread's block starts from.
        if init == "__wasm_call_ctors" {
            tls = capture_tls(&exports, &memory, memory_base, side.memory_size);
        }
        if let Ok(f) = Reflect::get(&exports, &init.into())?.dyn_into::<Function>() {
            f.call0(&JsValue::UNDEFINED)?;
        }
    }
    Ok((
        exports,
        table,
        Placement {
            memory_base,
            table_base,
            table_size: side.table_size,
            got: answers,
            slots,
            tls,
        },
    ))
}

/// The program's memory and function table, which every library shares.
fn program_memory_and_table(
    main_exports: &Object,
) -> Result<(JsValue, WebAssembly::Table), JsValue> {
    let memory = Reflect::get(main_exports, &"memory".into())?;
    if memory.is_undefined() {
        return Err("the program exports no memory to share".into());
    }
    let table: WebAssembly::Table =
        Reflect::get(main_exports, &"__indirect_function_table".into())?
            .dyn_into()
            .map_err(|_| {
                JsValue::from_str(
                    "the program exports no __indirect_function_table, so it was not built to host \
                 a native library. Build it with the library beside it.",
                )
            })?;
    Ok((memory, table))
}

/// A global offset table import's slot: its kind, its name and the global.
type GotSlot = (GotKind, String, WebAssembly::Global);

/// What a library is placed at in one instance of the program.
struct Bases {
    memory: JsValue,
    table: WebAssembly::Table,
    memory_base: i32,
    table_base: i32,
}

/// The import object for `module`, with the global offset table's slots
/// alongside.
///
/// A thread already knows what the table was answered with, and passes it as
/// `known`; the first load passes none and fills the slots in itself.
fn supply_imports(
    module: &WebAssembly::Module,
    main_exports: &Object,
    host_imports: &Object,
    bases: &Bases,
    known: Option<&[(GotKind, String, i32)]>,
) -> Result<(Object, Vec<GotSlot>), JsValue> {
    let memory_base = global(bases.memory_base, false)?;
    let table_base = global(bases.table_base, false)?;
    let env: Object = Reflect::get(host_imports, &"env".into())?.unchecked_into();

    let imports = Object::new();
    let mut got: Vec<(GotKind, String, WebAssembly::Global)> = Vec::new();
    for descriptor in WebAssembly::Module::imports(module).iter() {
        let from = Reflect::get(&descriptor, &"module".into())?
            .as_string()
            .unwrap_or_default();
        let name = Reflect::get(&descriptor, &"name".into())?
            .as_string()
            .unwrap_or_default();
        let value: JsValue = match from.as_str() {
            "GOT.mem" | "GOT.func" => {
                let kind = if from == "GOT.mem" {
                    GotKind::Data
                } else {
                    GotKind::Function
                };
                let initial = match known {
                    None => 0,
                    Some(known) => known
                        .iter()
                        .find(|(k, n, _)| *k == kind && *n == name)
                        .map(|(_, _, v)| *v)
                        .ok_or_else(|| {
                            JsValue::from_str(&format!("{name} was not answered by the first load"))
                        })?,
                };
                let slot = global(initial, true)?;
                got.push((kind, name.clone(), slot.clone()));
                slot.into()
            }
            // `env` is mostly the program: its memory, its table and the runtime
            // functions it exports; what it does not export is the host's.
            "env" => match name.as_str() {
                "memory" => bases.memory.clone(),
                "__indirect_function_table" => bases.table.clone().into(),
                "__memory_base" => memory_base.clone().into(),
                "__table_base" => table_base.clone().into(),
                _ => {
                    let program = Reflect::get(main_exports, &name.as_str().into())?;
                    if program.is_undefined() {
                        Reflect::get(&env, &name.as_str().into())?
                    } else {
                        program
                    }
                }
            },
            _ => Reflect::get(host_imports, &from.as_str().into())
                .ok()
                .filter(|ns| ns.is_object())
                .map(|ns| Reflect::get(&ns, &name.as_str().into()))
                .transpose()?
                .unwrap_or(JsValue::UNDEFINED),
        };
        if value.is_undefined() {
            return Err(format!(
                "it imports {from}::{name}, which nothing here provides. A runtime function is \
                 exported only when a library beside the program imports it, so this usually \
                 means the program was linked without this library present."
            )
            .into());
        }
        let namespace = match Reflect::get(&imports, &from.as_str().into())? {
            ns if ns.is_object() => ns,
            _ => {
                let ns: JsValue = Object::new().into();
                Reflect::set(&imports, &from.as_str().into(), &ns)?;
                ns
            }
        };
        Reflect::set(&namespace, &name.as_str().into(), &value)?;
    }
    Ok((imports, got))
}

/// The bytes of the library's thread-local block as its first instance
/// started with it, or nothing if it has none.
///
/// The block runs from the module's `__tls_base` to the end of the library's
/// memory. That takes in whatever follows it (the library's start flag, in
/// what wasm-ld lays out), which a copy carries along harmlessly; stopping
/// short would cut thread-local data off.
fn capture_tls(
    exports: &Object,
    memory: &JsValue,
    memory_base: i32,
    memory_size: u32,
) -> Option<Vec<u8>> {
    let global: WebAssembly::Global =
        Reflect::get(exports, &ash_wasm_link::dylink::TLS_BASE_EXPORT.into())
            .ok()?
            .dyn_into()
            .ok()?;
    let base = global.value().as_f64()? as i32;
    let end = memory_base.checked_add(i32::try_from(memory_size).ok()?)?;
    if base < memory_base || base >= end {
        return None;
    }
    let buffer = Reflect::get(memory, &"buffer".into()).ok()?;
    let view =
        Uint8Array::new_with_byte_offset_and_length(&buffer, base as u32, (end - base) as u32);
    Some(view.to_vec())
}

/// A block of the program's memory for this instance's thread-locals, holding
/// the first instance's initial image, with `__tls_base` pointed at it.
fn give_thread_block(
    main_exports: &Object,
    memory: &JsValue,
    exports: &Object,
    image: &Uint8Array,
) -> Result<(), JsValue> {
    let malloc: Function = Reflect::get(main_exports, &"malloc".into())?
        .dyn_into()
        .map_err(|_| JsValue::from_str("the program exports no malloc to place it"))?;
    let size = image.length() + 16;
    let addr = malloc
        .call1(&JsValue::UNDEFINED, &size.into())?
        .as_f64()
        .unwrap_or(0.0) as u32;
    if addr == 0 {
        return Err(format!("no room for {} bytes", image.length()).into());
    }
    // 16 covers what a thread-local asks for.
    let block = (addr + 15) & !15;
    let buffer = Reflect::get(memory, &"buffer".into())?;
    Uint8Array::new_with_byte_offset_and_length(&buffer, block, image.length()).set(image, 0);
    let global: WebAssembly::Global =
        Reflect::get(exports, &ash_wasm_link::dylink::TLS_BASE_EXPORT.into())?
            .dyn_into()
            .map_err(|_| JsValue::from_str("the thread's instance has no thread-local base"))?;
    global.set_value(&JsValue::from_f64(f64::from(block)));
    Ok(())
}

/// Load the libraries again in a thread's instance of the program, laid out
/// as the first load laid them out.
///
/// The memory is shared, so the libraries' data is already there and is not
/// written again: no initial data, no relocations, no constructors. What a
/// thread has to make for itself is what an instance owns -- the library
/// instances, and the slots in its own table at the same indices.
pub fn replay(
    layout: &JsValue,
    main_exports: &Object,
    host_imports: &Object,
) -> Result<Libraries, JsValue> {
    let (memory, table) = program_memory_and_table(main_exports)?;
    let mut out = Libraries {
        table: Some(table.clone()),
        ..Libraries::default()
    };
    let at = |want: i32, what: &str| -> Result<(), JsValue> {
        let have = table.length() as i32;
        if have == want {
            Ok(())
        } else {
            Err(
                format!("this thread's function table is {have} long where {what} goes at {want}")
                    .into(),
            )
        }
    };
    for lib in array(layout, "libraries")?.iter() {
        let name = text(&field(&lib, "name")?)?;
        let module: WebAssembly::Module = field(&lib, "module")?
            .dyn_into()
            .map_err(|_| JsValue::from_str("a library's module did not arrive as a module"))?;
        let (memory_base, table_base) = (
            number(&field(&lib, "memoryBase")?)?,
            number(&field(&lib, "tableBase")?)?,
        );
        let table_size = number(&field(&lib, "tableSize")?)?;
        let mut known = Vec::new();
        for entry in array(&lib, "got")?.iter() {
            let kind = match number(&Reflect::get_u32(&entry, 0)?)? {
                0 => GotKind::Data,
                _ => GotKind::Function,
            };
            known.push((
                kind,
                text(&Reflect::get_u32(&entry, 1)?)?,
                number(&Reflect::get_u32(&entry, 2)?)?,
            ));
        }

        at(table_base, &name)?;
        if table_size > 0 {
            table.grow(table_size as u32)?;
        }
        let (imports, _) = supply_imports(
            &module,
            main_exports,
            host_imports,
            &Bases {
                memory: memory.clone(),
                table: table.clone(),
                memory_base,
                table_base,
            },
            Some(&known),
        )?;
        let instance = WebAssembly::Instance::new(&module, &imports)?;
        let exports = instance.exports();
        if let Some(image) = field(&lib, "tls")?.dyn_ref::<Uint8Array>() {
            give_thread_block(main_exports, &memory, &exports, image).map_err(|e| {
                JsValue::from_str(&format!(
                    "thread-local storage for {name}: {}",
                    describe(&e)
                ))
            })?;
        }
        for entry in array(&lib, "slots")?.iter() {
            let index = number(&Reflect::get_u32(&entry, 0)?)?;
            let owner = match number(&Reflect::get_u32(&entry, 1)?)? {
                0 => &exports,
                _ => main_exports,
            };
            let export = text(&Reflect::get_u32(&entry, 2)?)?;
            at(index, &export)?;
            let func: Function = Reflect::get(owner, &export.as_str().into())?
                .dyn_into()
                .map_err(|_| JsValue::from_str(&format!("{export} is not a function here")))?;
            append(&table, &func)?;
        }

        let mut resolved = HashMap::new();
        let table_of = field(&lib, "resolved")?;
        for key in Object::keys(table_of.unchecked_ref::<Object>()).iter() {
            if let Some(key) = key.as_string() {
                let index = number(&Reflect::get(&table_of, &key.as_str().into())?)?;
                resolved.insert(key, index);
            }
        }
        out.by_name.insert(name, Library { exports, resolved });
    }
    for entry in array(layout, "primitives")?.iter() {
        let index = number(&Reflect::get_u32(&entry, 0)?)?;
        let lib = text(&Reflect::get_u32(&entry, 1)?)?;
        let export = text(&Reflect::get_u32(&entry, 2)?)?;
        at(index, &export)?;
        let Some(library) = out.by_name.get(&lib) else {
            return Err(format!("{lib} was not loaded for this thread").into());
        };
        let func: Function = Reflect::get(&library.exports, &export.as_str().into())?
            .dyn_into()
            .map_err(|_| JsValue::from_str(&format!("{lib}@{export} is not a function here")))?;
        append(&table, &func)?;
    }
    out.layout = Some(layout.clone());
    Ok(out)
}

/// The layout as an object a Worker can be sent: numbers, strings, arrays,
/// and the compiled modules, which structured cloning carries.
fn layout_object(
    placed: &[Placed],
    loaded: &Libraries,
    primitives: &[(i32, String, String)],
) -> Result<JsValue, JsValue> {
    let set = |object: &Object, key: &str, value: JsValue| -> Result<(), JsValue> {
        Reflect::set(object, &key.into(), &value).map(|_| ())
    };
    let triple = |a: JsValue, b: JsValue, c: JsValue| -> JsValue {
        let entry = Array::new();
        entry.push(&a);
        entry.push(&b);
        entry.push(&c);
        entry.into()
    };
    let libraries = Array::new();
    for lib in placed {
        let object = Object::new();
        let p = &lib.placement;
        set(&object, "name", lib.name.as_str().into())?;
        set(&object, "module", lib.module.clone().into())?;
        set(&object, "memoryBase", p.memory_base.into())?;
        set(&object, "tableBase", p.table_base.into())?;
        set(&object, "tableSize", p.table_size.into())?;
        let got = Array::new();
        for (kind, name, value) in &p.got {
            let kind = match kind {
                GotKind::Data => 0,
                GotKind::Function => 1,
            };
            got.push(&triple(kind.into(), name.as_str().into(), (*value).into()));
        }
        set(&object, "got", got.into())?;
        let slots = Array::new();
        for (index, owner, export) in &p.slots {
            let owner = match owner {
                Owner::Library => 0,
                Owner::Program => 1,
            };
            slots.push(&triple(
                (*index).into(),
                owner.into(),
                export.as_str().into(),
            ));
        }
        set(&object, "slots", slots.into())?;
        if let Some(image) = &p.tls {
            set(&object, "tls", Uint8Array::from(image.as_slice()).into())?;
        }
        let resolved = Object::new();
        if let Some(library) = loaded.by_name.get(&lib.name) {
            for (export, index) in &library.resolved {
                set(&resolved, export, (*index).into())?;
            }
        }
        set(&object, "resolved", resolved.into())?;
        libraries.push(&object);
    }
    let prims = Array::new();
    for (index, lib, export) in primitives {
        prims.push(&triple(
            (*index).into(),
            lib.as_str().into(),
            export.as_str().into(),
        ));
    }
    let layout = Object::new();
    set(&layout, "libraries", libraries.into())?;
    set(&layout, "primitives", prims.into())?;
    Ok(layout.into())
}

fn field(object: &JsValue, key: &str) -> Result<JsValue, JsValue> {
    Reflect::get(object, &key.into())
}

fn array(object: &JsValue, key: &str) -> Result<Array, JsValue> {
    field(object, key)?
        .dyn_into()
        .map_err(|_| JsValue::from_str(&format!("the layout's {key} is not an array")))
}

fn number(value: &JsValue) -> Result<i32, JsValue> {
    value
        .as_f64()
        .map(|n| n as i32)
        .ok_or_else(|| JsValue::from_str("a number was expected in the layout"))
}

fn text(value: &JsValue) -> Result<String, JsValue> {
    value
        .as_string()
        .ok_or_else(|| JsValue::from_str("a string was expected in the layout"))
}

/// An i32 global, as a side module's imports take one.
fn global(value: i32, mutable: bool) -> Result<WebAssembly::Global, JsValue> {
    let descriptor = Object::new();
    Reflect::set(&descriptor, &"value".into(), &"i32".into())?;
    Reflect::set(&descriptor, &"mutable".into(), &mutable.into())?;
    WebAssembly::Global::new(&descriptor, &value.into())
}

/// Where a data symbol was placed: a position-independent module exports each
/// as a global holding its address.
fn data_address(
    exports: &Object,
    main: &Object,
    name: &str,
    memory_base: i32,
) -> Result<Option<i32>, JsValue> {
    // The library exports its own data symbols as offsets from `__memory_base`;
    // the program is not position-independent, so its exports are absolute.
    for (owner, base) in [(exports, memory_base), (main, 0)] {
        let value = Reflect::get(owner, &name.into())?;
        if let Some(g) = value.dyn_ref::<WebAssembly::Global>() {
            return Ok(g.value().as_f64().map(|v| v as i32 + base));
        }
    }
    Ok(None)
}

/// A table slot holding `name`, appending one the first time. Cached, so a
/// function whose address is taken twice is the same pointer both times.
fn function_address(
    exports: &Object,
    main: &Object,
    table: &WebAssembly::Table,
    name: &str,
    placed: &mut HashMap<String, i32>,
    slots: &mut Vec<(i32, Owner, String)>,
) -> Result<Option<i32>, JsValue> {
    if let Some(&index) = placed.get(name) {
        return Ok(Some(index));
    }
    for (owner, from) in [(Owner::Library, exports), (Owner::Program, main)] {
        let value = Reflect::get(from, &name.into())?;
        if let Some(func) = value.dyn_ref::<Function>() {
            let index = append(table, func)?;
            placed.insert(name.to_string(), index);
            slots.push((index, owner, name.to_string()));
            return Ok(Some(index));
        }
    }
    Ok(None)
}

/// Append `func` to `table`, answering its index.
fn append(table: &WebAssembly::Table, func: &Function) -> Result<i32, JsValue> {
    let index = table.grow(1)?;
    table.set(index, func)?;
    Ok(index as i32)
}

fn describe(error: &JsValue) -> String {
    error
        .as_string()
        .or_else(|| {
            Reflect::get(error, &"message".into())
                .ok()
                .and_then(|m| m.as_string())
        })
        .unwrap_or_else(|| format!("{error:?}"))
}

/// What a page can see: the console, which the worker forwards.
fn report(text: &str) {
    web_sys::console::error_1(&text.into());
}
