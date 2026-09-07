//! Handing a frame to whatever is going to show it.
//!
//! **The agent running HashLink cannot be the one that paints.** It is inside
//! a call into the program for as long as the program runs, so it never
//! returns to its event loop -- and a canvas only reaches the page at the end
//! of a task. An `OffscreenCanvas` drawn from here does not appear late, it
//! does not appear at all until the program ends: measured, the whole run
//! composited as a single frame after every thread had finished.
//!
//! So nothing is drawn here. The program's framebuffer is already in shared
//! memory, which every agent and the page can read; this hands over a
//! description of it -- the memory, the address, the size -- and whoever
//! takes it paints on its own clock, at display rate, while the program keeps
//! drawing. That decoupling is what makes it real time, and it is also why
//! the frame does not have to be copied on the way out.
//!
//! Who paints is not this crate's business, for the same reason starting a
//! Worker is not: the page decides. A page installs `ashPresent` on this
//! agent's global and gets frames; a page that installs none has no display,
//! and the program is told so rather than stalled.
//!
//! One import, `env.ash_host_canvas_present`: here are `w * h` RGBA pixels at
//! this address in my memory.

use std::cell::RefCell;
use std::rc::Rc;

use js_sys::{Function, Object, Reflect};
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;

use super::imports::Host;

/// The page's frame sink, found once and kept.
#[derive(Default)]
pub struct Canvas {
    show: RefCell<Option<Function>>,
    /// Whether the page was asked for one at all, resolved once. A page that
    /// installed none is not an error: the program runs and draws into its
    /// own memory, and `present` answers that nobody is looking.
    looked: RefCell<bool>,
}

impl Canvas {
    fn show(&self) -> Option<Function> {
        if let Some(show) = self.show.borrow().as_ref() {
            return Some(show.clone());
        }
        if *self.looked.borrow() {
            return None;
        }
        *self.looked.borrow_mut() = true;
        let show: Function = Reflect::get(&js_sys::global(), &JsValue::from_str("ashPresent"))
            .ok()?
            .dyn_into()
            .ok()?;
        *self.show.borrow_mut() = Some(show.clone());
        Some(show)
    }

    /// Offer `width` by `height` RGBA pixels at `data` in the guest's memory.
    pub fn present(&self, host: &Host, data: u32, width: i32, height: i32) -> i32 {
        let (Ok(w), Ok(h)) = (u32::try_from(width), u32::try_from(height)) else {
            return 0;
        };
        let Some(len) = w.checked_mul(h).and_then(|p| p.checked_mul(4)) else {
            return 0;
        };
        if len == 0 {
            return 0;
        }
        let Some(show) = self.show() else {
            return 0;
        };
        let guest = host.guest.borrow();
        let Some(guest) = guest.as_ref() else {
            return 0;
        };
        // Checked here rather than by whoever paints: this side knows how big
        // the guest is, and a window handed out past the end of it would be
        // read every frame from another agent.
        if !guest.holds(data, len) {
            return 0;
        }

        let frame = Object::new();
        let set = |key: &str, value: JsValue| {
            let _ = Reflect::set(&frame, &JsValue::from_str(key), &value);
        };
        // The memory rather than its buffer: a `WebAssembly.Memory` that
        // grows detaches the buffer, and whoever paints has to be able to
        // take a fresh view rather than holding a dead one.
        set("memory", guest.memory().clone().into());
        set("address", data.into());
        set("width", w.into());
        set("height", h.into());

        match show.call1(&JsValue::UNDEFINED, &frame) {
            // False rather than an exception, for the same reason `spawn`
            // answers that way: a page with nowhere to put a frame is
            // answering, not failing.
            Ok(answer) if answer.is_falsy() => 0,
            Ok(_) => 1,
            Err(_) => 0,
        }
    }
}

/// Bind the import. Always bound, whether or not the page installed a sink:
/// an import nothing answers is a link error before a line runs.
pub fn install(env: &Object, host: &Rc<Host>) -> Result<(), JsValue> {
    let showing = Rc::clone(host);
    let closure = Closure::wrap(Box::new(move |data: u32, width: i32, height: i32| -> i32 {
        showing.canvas.present(&showing, data, width, height)
    }) as Box<dyn FnMut(u32, i32, i32) -> i32>);
    Reflect::set(
        env,
        &JsValue::from_str("ash_host_canvas_present"),
        &closure.into_js_value(),
    )?;
    Ok(())
}
