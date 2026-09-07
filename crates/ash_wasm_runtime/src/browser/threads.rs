//! Starting a thread in a page, which means starting another Worker.
//!
//! Same shape as the native host: a thread is a second instance of the same
//! module over the same memory, entered through `wasi_thread_start`. What
//! differs is who makes the agent. `wasmtime` can start an operating system
//! thread; a page starts a Worker, and a Worker is made from a URL -- which
//! this crate cannot know, any more than it knows where the module was
//! fetched from.
//!
//! So the page supplies a function and this supplies everything that function
//! needs: the compiled module, the shared memory, the thread id, and the
//! argument the guest's `pthread_create` prepared. The page's side of it is
//! `new Worker(...)` and one `postMessage`, and [`crate::browser::run_thread`]
//! is what that Worker calls.
//!
//! # Why the id is the host's to hand out
//!
//! wasi-threads requires a thread id to be positive and distinct, and the
//! guest gets it back as the return value of the import -- before the Worker
//! has started, let alone reported anything. So it cannot come from the
//! Worker.

use std::cell::{Cell, RefCell};

use js_sys::{Function, Object, Reflect, WebAssembly};
use wasm_bindgen::prelude::*;

/// What a page needs to start one, and what it was given to do it with.
#[derive(Default)]
pub struct Threads {
    control: super::control::Control,
    /// The page's `spawn`. Absent means this program cannot start a thread,
    /// which is the ordinary case for a build that never wanted one.
    spawn: Option<Function>,
    /// Set once the module is compiled, because a Worker instantiates the
    /// same one rather than fetching and compiling it again.
    module: RefCell<Option<WebAssembly::Module>>,
    memory: RefCell<Option<WebAssembly::Memory>>,
    args: Vec<String>,
    environ: Vec<String>,
    next_id: Cell<i32>,
}

impl Threads {
    pub fn new(spawn: Option<Function>, args: Vec<String>, environ: Vec<String>) -> Self {
        Self::with_control(spawn, args, environ, super::control::Control::default())
    }

    pub(super) fn with_control(
        spawn: Option<Function>,
        args: Vec<String>,
        environ: Vec<String>,
        control: super::control::Control,
    ) -> Self {
        Self {
            control,
            spawn,
            module: RefCell::new(None),
            memory: RefCell::new(None),
            args,
            environ,
            next_id: Cell::new(1),
        }
    }

    /// What a thread will instantiate, once there is something to instantiate.
    pub fn attach(&self, module: WebAssembly::Module, memory: Option<WebAssembly::Memory>) {
        *self.module.borrow_mut() = Some(module);
        *self.memory.borrow_mut() = memory;
    }

    /// Answer `wasi.thread-spawn`: the new thread's id, or a negative number.
    ///
    /// Negative is what the interface has for a thread that could not be
    /// started, and what wasi-libc's `pthread_create` turns into `EAGAIN`.
    /// Three things make it negative: a page that supplied no `spawn`, a
    /// module whose memory is its own -- nothing to share, so a second
    /// instance would be a second program -- and a `spawn` that threw.
    pub fn spawn(&self, start_arg: i32) -> i32 {
        self.control.check();
        let Some(spawn) = &self.spawn else {
            return -1;
        };
        let (module, memory) = (self.module.borrow(), self.memory.borrow());
        let (Some(module), Some(memory)) = (module.as_ref(), memory.as_ref()) else {
            return -1;
        };
        let id = self.next_id.get();
        self.next_id.set(id + 1);

        let request = Object::new();
        let set = |key: &str, value: JsValue| {
            let _ = Reflect::set(&request, &JsValue::from_str(key), &value);
        };
        set("tid", id.into());
        set("startArg", start_arg.into());
        set("module", module.clone().into());
        set("memory", memory.clone().into());
        set("args", to_array(&self.args));
        set("environ", to_array(&self.environ));
        set("control", self.control.buffer());

        match spawn.call1(&JsValue::UNDEFINED, &request) {
            // False rather than an exception: a page with no agent free is
            // answering, not failing, and the thread runs on the scheduler
            // instead. Only a throw is worth a console error.
            Ok(answer) if answer.is_falsy() => -1,
            Ok(_) => id,
            Err(e) => {
                web_sys::console::error_1(&JsValue::from_str(&format!(
                    "[ash] could not start thread {id}: {e:?}"
                )));
                -1
            }
        }
    }
}

fn to_array(values: &[String]) -> JsValue {
    let array = js_sys::Array::new();
    for value in values {
        array.push(&JsValue::from_str(value));
    }
    array.into()
}
