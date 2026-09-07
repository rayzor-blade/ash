//! Showing a frame the program drew, on a canvas the page handed over.
//!
//! HashLink runs in a Worker, and a Worker inside a call into the program
//! never returns to its event loop until that call ends -- which for a program
//! with a main loop is never. So nothing on the page can be asked to draw: not
//! `requestAnimationFrame`, not a `postMessage` the page would have to receive.
//!
//! An `OffscreenCanvas` is the way out, and the only one. The page creates a
//! canvas, calls `transferControlToOffscreen()`, and posts the result here;
//! drawing to it from this Worker takes effect on the page without this thread
//! ever yielding. The page sets it as `ashCanvas` on the Worker's global,
//! which is where the other things a page lends this host are found --
//! `crypto`, `performance` -- rather than being threaded through `run`.
//!
//! One import, `env.ash_host_canvas_present`: here are `w * h` RGBA pixels at
//! this address in my memory, show them.

use std::cell::RefCell;
use std::rc::Rc;

use js_sys::{Object, Reflect};
use wasm_bindgen::prelude::*;
use wasm_bindgen::{Clamped, JsCast};
use web_sys::{ImageData, OffscreenCanvas, OffscreenCanvasRenderingContext2d};

use super::imports::Host;

/// The canvas and its context, found once and kept.
///
/// Kept because `getContext` on the same canvas returns the same context
/// every time and the lookup is not free, and because a program presenting at
/// sixty frames a second would otherwise do it sixty times a second.
#[derive(Default)]
pub struct Canvas {
    /// Kept beside the context so a frame of a size the page did not expect
    /// resizes the surface rather than being cropped by it.
    surface: RefCell<Option<OffscreenCanvas>>,
    context: RefCell<Option<OffscreenCanvasRenderingContext2d>>,
    /// Whether the page gave this Worker a canvas at all, resolved once. A
    /// page that did not is not an error: the program runs and draws into its
    /// own memory, and `present` answers that nobody is looking.
    looked: RefCell<bool>,
}

impl Canvas {
    fn context(&self) -> Option<OffscreenCanvasRenderingContext2d> {
        if let Some(ctx) = self.context.borrow().as_ref() {
            return Some(ctx.clone());
        }
        if *self.looked.borrow() {
            return None;
        }
        *self.looked.borrow_mut() = true;
        let canvas: OffscreenCanvas =
            Reflect::get(&js_sys::global(), &JsValue::from_str("ashCanvas"))
                .ok()?
                .dyn_into()
                .ok()?;
        let ctx: OffscreenCanvasRenderingContext2d = canvas
            .get_context("2d")
            .ok()??
            .dyn_into()
            .ok()?;
        *self.surface.borrow_mut() = Some(canvas);
        *self.context.borrow_mut() = Some(ctx.clone());
        Some(ctx)
    }

    /// Make the surface `w` by `h` if it is not already. The program decides
    /// the size of a frame; the page only declared one for the element's
    /// layout, and a mismatch would otherwise crop rather than scale.
    fn fit(&self, w: u32, h: u32) {
        let surface = self.surface.borrow();
        let Some(canvas) = surface.as_ref() else {
            return;
        };
        if canvas.width() != w {
            canvas.set_width(w);
        }
        if canvas.height() != h {
            canvas.set_height(h);
        }
    }

    /// Draw `width` by `height` RGBA pixels read from the guest at `data`.
    pub fn present(&self, host: &Host, data: u32, width: i32, height: i32) -> i32 {
        let (Ok(w), Ok(h)) = (u32::try_from(width), u32::try_from(height)) else {
            return 0;
        };
        let Some(len) = w.checked_mul(h).and_then(|p| p.checked_mul(4)) else {
            return 0;
        };
        let Some(ctx) = self.context() else {
            return 0;
        };
        self.fit(w, h);
        let guest = host.guest.borrow();
        let Some(guest) = guest.as_ref() else {
            return 0;
        };
        // Copied out rather than viewed in place: the guest's memory is
        // shared, other threads are writing it, and `ImageData` will not take
        // a view backed by a `SharedArrayBuffer`.
        let Some(pixels) = guest.read(data, len) else {
            return 0;
        };
        let Ok(image) = ImageData::new_with_u8_clamped_array_and_sh(Clamped(&pixels), w, h) else {
            return 0;
        };
        match ctx.put_image_data(&image, 0.0, 0.0) {
            Ok(()) => 1,
            Err(_) => 0,
        }
    }
}

/// Bind the import. Always bound, whether or not the page lent a canvas: an
/// import nothing answers is a link error before a line runs.
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
