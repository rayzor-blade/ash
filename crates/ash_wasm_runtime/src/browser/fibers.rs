//! Suspending a fiber in a page.
//!
//! Not JSPI, and not a worker parked on `Atomics.wait`. ash already carries a
//! link-time transform (`docs/wasm-fibers.md`) that rewrites a module so its
//! frames can unwind themselves back to a scheduler and rewind to exactly
//! where they stopped, and it works: the Haxe threads suite goes from a
//! timeout to 22 of 22 under it. A module built with `ASH_WASM_FIBERS=1`
//! carries the transform, and all a host has to do is drive the three globals
//! it added.
//!
//! So this is the browser's half of what `native::install_fiber_yield`
//! already does, against `WebAssembly.Global` instead of wasmtime's. That is
//! the whole point of having the transform: a host needs to be able to
//! suspend, not to have an engine that can.
//!
//! # What a module without the transform gets
//!
//! Nothing, and it must not be an error. The globals are absent, every call
//! here answers zero, and a fiber runs to completion at the point it would
//! have suspended -- which is what the native host does for the same module,
//! and what `sys.thread` on wasm did before the transform existed.

use js_sys::{Object, Reflect, WebAssembly};
use wasm_bindgen::{JsCast, JsValue};

/// The names the transform gives its globals, and the module's own shadow
/// stack pointer, which the linker exports when the transform is applied.
const STATE: &str = "ash_fiber_state";
const DATA: &str = "ash_fiber_data";
const STACK_POINTER: &str = "__stack_pointer";

/// The state values the transform reads, which are Asyncify's.
const UNWINDING: i32 = 1;
const REWINDING: i32 = 2;

/// The globals a transformed module exports, if it was transformed.
#[derive(Default)]
pub struct Fibers {
    state: Option<WebAssembly::Global>,
    data: Option<WebAssembly::Global>,
    /// A fiber runs on a shadow stack of its own, so the host swaps this on
    /// the way in and out. See `arm`.
    stack_pointer: Option<WebAssembly::Global>,
}

impl Fibers {
    /// Read the three off an instance's exports. Absent is the ordinary case.
    pub fn from_exports(exports: &Object) -> Self {
        let global = |name: &str| -> Option<WebAssembly::Global> {
            Reflect::get(exports, &JsValue::from_str(name))
                .ok()
                .and_then(|v| v.dyn_into::<WebAssembly::Global>().ok())
        };
        Self {
            state: global(STATE),
            data: global(DATA),
            stack_pointer: global(STACK_POINTER),
        }
    }

    /// Whether this module can suspend at all.
    pub fn present(&self) -> bool {
        self.state.is_some()
    }

    /// Suspend, by telling every instrumented frame between here and the
    /// scheduler to return on its way out.
    ///
    /// On a rewind this is the call the fiber stopped at, so reaching it
    /// again means the rewind is over and the state goes back to running.
    pub fn yield_now(&self) {
        let Some(state) = &self.state else {
            return;
        };
        let next = if read(state) == REWINDING {
            0
        } else {
            UNWINDING
        };
        write(state, next);
    }

    /// What the state global says. The guest cannot read it itself: the
    /// global is added after the guest was compiled, so there is no name in
    /// the guest to refer to it by.
    pub fn state(&self) -> i32 {
        self.state.as_ref().map_or(0, read)
    }

    /// Point the transform at a fiber's side stack, say whether the next
    /// entry is a rewind, and answer whose shadow stack was in place.
    ///
    /// Two fibers cannot share one shadow stack: the frames between a suspend
    /// and the scheduler return and restore the pointer above the suspended
    /// fiber's frames, and the next allocation overwrites them. So each fiber
    /// runs on a region of its own and the caller is put back exactly where
    /// it was.
    pub fn arm(&self, data: i32, rewind: i32, stack: i32) -> i32 {
        if let Some(g) = &self.data {
            write(g, data);
        }
        if let Some(g) = &self.state {
            write(g, if rewind != 0 { REWINDING } else { 0 });
        }
        match &self.stack_pointer {
            Some(g) if stack != 0 => {
                let was = read(g);
                write(g, stack);
                was
            }
            Some(g) => read(g),
            None => 0,
        }
    }
}

/// A `WebAssembly.Global` of type i32 reads as a JavaScript number.
fn read(global: &WebAssembly::Global) -> i32 {
    global.value().as_f64().unwrap_or(0.0) as i32
}

fn write(global: &WebAssembly::Global, value: i32) {
    global.set_value(&JsValue::from_f64(f64::from(value)));
}
