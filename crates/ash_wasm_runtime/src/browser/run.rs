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

use super::imports::{imports, Host};
use super::memory::Guest;

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
) -> Result<Outcome, JsValue> {
    let host = Host::new(args, environ);
    let instance = instantiate(&module, &imports(&host)).await?;
    let exports: Object = Reflect::get(&instance, &"exports".into())?.unchecked_into();

    // The host functions need the memory, and the instance is the first thing
    // that knows which one it got.
    let memory: WebAssembly::Memory = Reflect::get(&exports, &"memory".into())?.unchecked_into();
    host.attach(Guest::new(memory));

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

async fn instantiate(module: &Uint8Array, imports: &Object) -> Result<JsValue, JsValue> {
    let ready = WebAssembly::instantiate_buffer(&module.to_vec(), imports);
    let result = JsFuture::from(ready).await?;
    Reflect::get(&result, &"instance".into())
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
