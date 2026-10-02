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
//! A thread's agent has its own table, so a library loaded here is reachable
//! from the program's own thread only, which is also what `ash run` offers.

use std::collections::HashMap;

use js_sys::{Function, Object, Reflect, Uint8Array, WebAssembly};
use wasm_bindgen::{JsCast, JsValue};

/// A loaded library, and the primitives already handed out.
struct Library {
    exports: Object,
    /// Name to the table index a previous lookup returned: a primitive
    /// resolved twice must be the same pointer.
    resolved: HashMap<String, i32>,
}

/// Every library loaded beside the program, by the name a program calls it.
#[derive(Default)]
pub struct Libraries {
    by_name: HashMap<String, Library>,
    /// The program's function table, which every library shares.
    table: Option<WebAssembly::Table>,
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
    pub fn resolve(&mut self, lib: &str, symbol: &str) -> i32 {
        let Some(table) = self.table.clone() else {
            return 0;
        };
        let Some(library) = self.by_name.get_mut(lib) else {
            return 0;
        };
        if let Some(&index) = library.resolved.get(symbol) {
            return index;
        }
        let Ok(func) = Reflect::get(&library.exports, &symbol.into()) else {
            return 0;
        };
        let Some(func) = func.dyn_ref::<Function>() else {
            return 0;
        };
        let Ok(index) = append(&table, func) else {
            return 0;
        };
        library.resolved.insert(symbol.to_string(), index);
        index
    }
}

/// Load each library in `libraries`, an object from the name a program calls
/// it to its bytes, against the program instantiated with `host_imports`.
///
/// One that will not load is reported and skipped: a program that never
/// reaches its primitives still runs, and one that does raises "not loaded"
/// there, after this message has said why.
pub fn load(
    libraries: &Object,
    main_exports: &Object,
    host_imports: &Object,
) -> Result<Libraries, JsValue> {
    let mut out = Libraries::default();
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
        match load_one(&bytes.to_vec(), main_exports, host_imports) {
            Ok((exports, table)) => {
                out.table = Some(table);
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
    Ok(out)
}

/// Which of the two global offset tables an import belongs to.
#[derive(Clone, Copy)]
enum GotKind {
    Data,
    Function,
}

fn load_one(
    bytes: &[u8],
    main_exports: &Object,
    host_imports: &Object,
) -> Result<(Object, WebAssembly::Table), JsValue> {
    let side = ash_wasm_link::read_side_module(bytes)
        .map_err(|e| JsValue::from_str(&e.to_string()))?
        .ok_or_else(|| JsValue::from_str("it is not a side module (no dylink.0 section)"))?;
    let bytes = ash_wasm_link::waits::instrument(bytes)
        .map_err(|e| JsValue::from_str(&e.to_string()))?;
    let module = WebAssembly::Module::new(&Uint8Array::from(bytes.as_slice()))?;

    let memory = Reflect::get(main_exports, &"memory".into())?;
    if memory.is_undefined() {
        return Err("the program exports no memory to share".into());
    }
    let table: WebAssembly::Table = Reflect::get(main_exports, &"__indirect_function_table".into())?
        .dyn_into()
        .map_err(|_| {
            JsValue::from_str(
                "the program exports no __indirect_function_table, so it was not built to host \
                 a native library. Build it with the library beside it.",
            )
        })?;

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

    let memory_base = global(memory_base, false)?;
    let table_base = global(table_base, false)?;
    let env: Object = Reflect::get(host_imports, &"env".into())?.unchecked_into();

    // The global offset table cannot be answered until the library is placed,
    // so its entries start as zeroes and are filled before anything reads them.
    let imports = Object::new();
    let mut got: Vec<(GotKind, String, WebAssembly::Global)> = Vec::new();
    for descriptor in WebAssembly::Module::imports(&module).iter() {
        let from = Reflect::get(&descriptor, &"module".into())?
            .as_string()
            .unwrap_or_default();
        let name = Reflect::get(&descriptor, &"name".into())?
            .as_string()
            .unwrap_or_default();
        let value: JsValue = match from.as_str() {
            "GOT.mem" | "GOT.func" => {
                let slot = global(0, true)?;
                let kind = if from == "GOT.mem" {
                    GotKind::Data
                } else {
                    GotKind::Function
                };
                got.push((kind, name.clone(), slot.clone()));
                slot.into()
            }
            // `env` is mostly the program: its memory, its table and the runtime
            // functions it exports; what it does not export is the host's.
            "env" => match name.as_str() {
                "memory" => memory.clone(),
                "__indirect_function_table" => table.clone().into(),
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

    let instance = WebAssembly::Instance::new(&module, &imports)?;
    let exports = instance.exports();

    // Now that it has been placed, say where: its own symbols from its exports,
    // anything else from the program's.
    let mut placed: HashMap<String, i32> = HashMap::new();
    for (kind, name, slot) in got {
        let value = match kind {
            GotKind::Data => data_address(&exports, main_exports, &name)?,
            GotKind::Function => {
                function_address(&exports, main_exports, &table, &name, &mut placed)?
            }
        };
        let value = value.ok_or_else(|| {
            JsValue::from_str(&format!(
                "it needs the address of {name}, which neither it nor the program defines"
            ))
        })?;
        slot.set_value(&value.into());
    }

    for init in ["__wasm_apply_data_relocs", "__wasm_call_ctors"] {
        if let Ok(f) = Reflect::get(&exports, &init.into())?.dyn_into::<Function>() {
            f.call0(&JsValue::UNDEFINED)?;
        }
    }
    Ok((exports, table))
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
fn data_address(exports: &Object, main: &Object, name: &str) -> Result<Option<i32>, JsValue> {
    for owner in [exports, main] {
        let value = Reflect::get(owner, &name.into())?;
        if let Some(g) = value.dyn_ref::<WebAssembly::Global>() {
            return Ok(g.value().as_f64().map(|v| v as i32));
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
) -> Result<Option<i32>, JsValue> {
    if let Some(&index) = placed.get(name) {
        return Ok(Some(index));
    }
    for owner in [exports, main] {
        let value = Reflect::get(owner, &name.into())?;
        if let Some(func) = value.dyn_ref::<Function>() {
            let index = append(table, func)?;
            placed.insert(name.to_string(), index);
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
