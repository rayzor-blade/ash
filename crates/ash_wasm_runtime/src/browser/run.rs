//! Running an ash program in a page.
//!
//! The same three steps the native host takes -- instantiate, hand the host
//! the memory the instance turned out to have, call the entrypoint -- against
//! `WebAssembly.instantiate` instead of wasmtime.
//!
//! # What the page supplies, and what it does not
//!
//! The bytes. Fetching is the page's business: it knows its own origin, its
//! caching and whether it wants a stream, and none of that is this crate's to
//! decide. So [`run`] takes a module that has already been fetched.
//!
//! # Why the entrypoint is looked up rather than named
//!
//! ash links a program as a LIBRARY, not a command: it exports `main` and
//! `ash_module_init` and imports what a sandbox cannot do for itself. A
//! module built for a WASI command exports `_start` instead. Both are
//! accepted, because which one a module has follows from how it was linked
//! and not from anything the page chose.
//!
//! Either way the entrypoint is the only thing called. `ash_module_init` is
//! exported for a host that wants to initialise without running; calling it
//! first here reaches the GC before the entrypoint has started it.

use js_sys::{Function, Object, Reflect, Uint8Array, WebAssembly};
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use wasm_bindgen_futures::JsFuture;

use std::rc::Rc;

use super::fibers::Fibers;
use super::imports::{imports, Host};
use super::memory::Guest;
use crate::imported_memory::{imported_memory, MemoryLimits};
use super::threads::Threads;

/// How a run ended, as a page sees it.
#[wasm_bindgen]
pub struct Outcome {
    status: i32,
    trapped: Option<String>,
}

#[wasm_bindgen]
impl Outcome {
    /// What the program exited with.
    #[wasm_bindgen(getter)]
    pub fn status(&self) -> i32 {
        self.status
    }

    /// The trap, if it ended in one. A program that called `Sys.exit` did
    /// not, however it looked from inside: `proc_exit` unwinds by throwing,
    /// which is how a wasm program leaves a stack it cannot return through.
    #[wasm_bindgen(getter)]
    pub fn trapped(&self) -> Option<String> {
        self.trapped.clone()
    }
}

/// Run a module, and answer how it ended.
///
/// `args` is `Sys.args()`, argv[0] included; `environ` is
/// `Sys.environment()`, each entry already `NAME=value`.
#[wasm_bindgen]
pub async fn run(
    module: Uint8Array,
    args: Vec<String>,
    environ: Vec<String>,
    spawn: Option<Function>,
) -> Result<Outcome, JsValue> {
    let bytes = module.to_vec();
    let host = Host::new(args.clone(), environ.clone());
    // A threads build does not define its memory. It imports one, shared, so
    // that every thread instantiates against the same one -- and the host is
    // what makes it. See [`super::module::imported_memory`] for why the
    // limits are read out of the module rather than asked of the engine.
    let shared = imported_memory(&bytes)
        .map(shared_memory)
        .transpose()?;
    let threads = Rc::new(Threads::new(spawn, args, environ));

    let (compiled, instance) =
        instantiate(&bytes, &imports(&host, shared.as_ref(), &threads)).await?;
    let exports: Object = Reflect::get(&instance, &"exports".into())?.unchecked_into();

    // The host functions need the memory, and the instance is the first thing
    // that knows which one it got -- a module that made its own, at least.
    // One that imported it is being handed back what was just given to it.
    let memory: WebAssembly::Memory = Reflect::get(&exports, &"memory".into())?.unchecked_into();
    host.attach(Guest::new(memory));
    // What a thread will instantiate. The module is passed on already
    // compiled, because a Worker starting a thread should not fetch and
    // compile megabytes again to run one function.
    threads.attach(compiled, shared);
    // And the transform's globals, if this module was built with fibers.
    // Absent is the ordinary case and not an error.
    host.attach_fibers(Fibers::from_exports(&exports));

    // `ash_module_init` is NOT called here, though the module exports it:
    // the emitted `main` calls it itself, and calling it first reaches the
    // GC before the entrypoint has started it. Exported for a host that
    // wants to initialise without running, which this is not.
    let outcome = match export(&exports, "_start") {
        Ok(start) => start.call0(&JsValue::UNDEFINED).map(|_| 0),
        Err(_) => export(&exports, "main")?
            .call2(&JsValue::UNDEFINED, &0.into(), &0.into())
            .map(|status| status.as_f64().unwrap_or(0.0) as i32),
    };

    Ok(match outcome {
        Ok(status) => Outcome {
            status,
            trapped: None,
        },
        Err(error) => {
            // `proc_exit` leaves through a throw, so an exit arrives here
            // looking like a failure. The host recorded the real status when
            // it was asked to exit, and that is the one to report.
            match host.wasi.borrow().exit_status() {
                Some(status) => Outcome {
                    status,
                    trapped: None,
                },
                None => Outcome {
                    status: -1,
                    trapped: Some(describe(&error)),
                },
            }
        }
    })
}

/// Compile and instantiate, and keep both: the instance to run, and the
/// module because a thread instantiates that same one again.
async fn instantiate(
    bytes: &[u8],
    imports: &Object,
) -> Result<(WebAssembly::Module, JsValue), JsValue> {
    let result = JsFuture::from(WebAssembly::instantiate_buffer(bytes, imports)).await?;
    let compiled: WebAssembly::Module = Reflect::get(&result, &"module".into())?.unchecked_into();
    Ok((compiled, Reflect::get(&result, &"instance".into())?))
}

/// The memory a threads build imports: shared, and reserving the maximum it
/// declared so that growing it never moves it out from under another thread.
///
/// This needs `SharedArrayBuffer`, which a page only has when it is
/// cross-origin isolated -- COOP and COEP on every response it serves. The
/// failure without them is this constructor throwing, so it is said here
/// rather than left as a stack trace.
fn shared_memory(limits: MemoryLimits) -> Result<WebAssembly::Memory, JsValue> {
    let descriptor = Object::new();
    let set = |key: &str, value: JsValue| {
        let _ = Reflect::set(&descriptor, &JsValue::from_str(key), &value);
    };
    set("initial", limits.minimum.into());
    set("maximum", limits.maximum.into());
    set("shared", JsValue::TRUE);
    WebAssembly::Memory::new(&descriptor).map_err(|e| {
        JsValue::from_str(&format!(
            "this module needs a shared memory, which needs SharedArrayBuffer, which needs \
             the page to be cross-origin isolated: serve it with Cross-Origin-Opener-Policy: \
             same-origin and Cross-Origin-Embedder-Policy: require-corp. ({})",
            describe(&e)
        ))
    })
}

/// Run one thread: the same module, the same memory, entered where wasi-libc
/// leaves off.
///
/// This is what the Worker a page started in `spawn` calls. It instantiates
/// rather than fetches, because the module arrives already compiled, and it
/// calls `wasi_thread_start` rather than an entrypoint -- the guest's own
/// `pthread_create` prepared `start_arg`, and everything about what this
/// thread will do is in there.
#[wasm_bindgen]
pub async fn run_thread(
    module: WebAssembly::Module,
    memory: WebAssembly::Memory,
    tid: i32,
    start_arg: i32,
    args: Vec<String>,
    environ: Vec<String>,
    spawn: Option<Function>,
) -> Result<(), JsValue> {
    let host = Host::new(args.clone(), environ.clone());
    let threads = Rc::new(Threads::new(spawn, args, environ));
    let ready = WebAssembly::instantiate_module(&module, &imports(&host, Some(&memory), &threads));
    let instance: WebAssembly::Instance = JsFuture::from(ready).await?.unchecked_into();
    let exports: Object = Reflect::get(&instance, &"exports".into())?.unchecked_into();

    host.attach(Guest::new(memory.clone()));
    host.attach_fibers(Fibers::from_exports(&exports));
    // A thread may start a thread, so it is given what it would need to.
    threads.attach(module, Some(memory));

    export(&exports, "wasi_thread_start")?
        .call2(&JsValue::UNDEFINED, &tid.into(), &start_arg.into())
        .map(|_| ())
}

fn export(exports: &Object, name: &str) -> Result<Function, JsValue> {
    let value = Reflect::get(exports, &name.into())?;
    value
        .dyn_into::<Function>()
        .map_err(|_| JsValue::from_str(&format!("the module exports no {name}")))
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
