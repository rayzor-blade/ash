//! Asking the page to start an agent beside the program.
//!
//! The agent running HashLink is inside a call into the program for as long
//! as the program runs, so it can never start a Worker itself: creating one
//! needs the creating agent's event loop. The page can. A library that wants a
//! service running beside the program -- a WebGPU device serving a mailbox in
//! shared memory, say -- names it and hands over an address, and the page
//! loads the library's shim `./<name>.mjs` and calls its
//! `start({ memory, address, canvas })`.
//!
//! One import, `env.ash_host_agent(name, name_len, address)`: 1 when the page
//! took it, 0 when it installed no `ashAgent` hook, the name is not a plain
//! identifier, or the memory is not shared -- an agent could not see the
//! program's memory, so there would be nothing for it to serve.

use std::rc::Rc;

use js_sys::{Function, Object, Reflect};
use wasm_bindgen::JsCast;
use wasm_bindgen::prelude::*;

use super::imports::Host;

/// Whether `name` can be put into a module path without escaping it: letters,
/// digits, `_` and `-`, nothing else.
fn plain(name: &str) -> bool {
    !name.is_empty()
        && name.len() <= 64
        && name
            .bytes()
            .all(|b| b.is_ascii_alphanumeric() || b == b'_' || b == b'-')
}

fn request(host: &Host, name_ptr: u32, name_len: u32, address: u32) -> i32 {
    let guest = host.guest.borrow();
    let Some(guest) = guest.as_ref() else {
        return 0;
    };
    let Some(bytes) = guest.read(name_ptr, name_len) else {
        return 0;
    };
    let Ok(name) = std::str::from_utf8(&bytes) else {
        return 0;
    };
    if !plain(name) {
        return 0;
    }
    let memory = guest.memory();
    if !memory
        .buffer()
        .is_instance_of::<js_sys::SharedArrayBuffer>()
    {
        return 0;
    }
    let Some(hook) = Reflect::get(&js_sys::global(), &JsValue::from_str("ashAgent"))
        .ok()
        .and_then(|v| v.dyn_into::<Function>().ok())
    else {
        return 0;
    };
    let ask = Object::new();
    let set = |key: &str, value: JsValue| {
        let _ = Reflect::set(&ask, &JsValue::from_str(key), &value);
    };
    set("name", JsValue::from_str(name));
    // The memory rather than its buffer: a memory that grows detaches the
    // buffer, and the agent has to be able to take a fresh view.
    set("memory", memory.clone().into());
    set("address", address.into());
    match hook.call1(&JsValue::UNDEFINED, &ask) {
        Ok(answer) if answer.is_falsy() => 0,
        Ok(_) => 1,
        Err(_) => 0,
    }
}

/// Bind the import. Always bound, whether or not the page installed a hook:
/// an import nothing answers is a link error before a line runs.
pub fn install(env: &Object, host: &Rc<Host>) -> Result<(), JsValue> {
    let host = Rc::clone(host);
    let closure = Closure::wrap(Box::new(move |name: u32, len: u32, address: u32| -> i32 {
        request(&host, name, len, address)
    }) as Box<dyn FnMut(u32, u32, u32) -> i32>);
    Reflect::set(
        env,
        &JsValue::from_str("ash_host_agent"),
        &closure.into_js_value(),
    )?;
    Ok(())
}
