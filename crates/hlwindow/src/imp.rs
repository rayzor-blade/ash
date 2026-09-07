//! What the primitives do, over winit.
//!
//! # Why the event loop lives in a thread local
//!
//! winit's event loop is not `Send`, and on macOS it has to stay on the thread
//! that made it -- which for a HashLink program is the one running `main`. A
//! `static` would demand `Send`; a thread local says the same thing honestly.

use std::cell::RefCell;
use std::ffi::c_int;
use std::time::Duration;

use raw_window_handle::{HasDisplayHandle, HasWindowHandle, RawDisplayHandle, RawWindowHandle};
use winit::application::ApplicationHandler;
use winit::event::WindowEvent;
use winit::event_loop::{ActiveEventLoop, EventLoop};
use winit::platform::pump_events::EventLoopExtPumpEvents;
use winit::window::{Window, WindowAttributes};

use hl_abi::vbyte;

/// What `poll` reports, as bits.
const CLOSED: i32 = 1;
const RESIZED: i32 = 2;

/// The platform codes `platform` answers with.
const APPKIT: i32 = 1;
const WIN32: i32 = 2;
const XLIB: i32 = 3;
const WAYLAND: i32 = 4;

/// Opened once, in `resumed`, because winit will not make a window before it.
struct App {
    attributes: WindowAttributes,
    window: Option<Window>,
    events: i32,
}

impl ApplicationHandler for App {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        if self.window.is_none() {
            self.window = event_loop.create_window(self.attributes.clone()).ok();
        }
    }

    fn window_event(&mut self, _: &ActiveEventLoop, _: winit::window::WindowId, event: WindowEvent) {
        match event {
            WindowEvent::CloseRequested => self.events |= CLOSED,
            WindowEvent::Resized(_) => self.events |= RESIZED,
            _ => {}
        }
    }
}

struct Open {
    event_loop: EventLoop<()>,
    app: App,
}

thread_local! {
    /// A handle is an index into this, plus one, so zero is never a window.
    /// Deliberately simpler than hlwgpu's table: a program has one window or
    /// two, not a million, and they are closed when it exits.
    static WINDOWS: RefCell<Vec<Option<Open>>> = const { RefCell::new(Vec::new()) };
}

/// Runs `body` on an open window, or answers `miss`.
fn with<T>(handle: i32, miss: T, body: impl FnOnce(&mut Open) -> T) -> T {
    WINDOWS.with(|windows| {
        let mut windows = windows.borrow_mut();
        let index = (handle - 1).max(-1);
        if index < 0 {
            return miss;
        }
        match windows.get_mut(index as usize).and_then(|slot| slot.as_mut()) {
            Some(open) => body(open),
            None => miss,
        }
    })
}

unsafe fn ucs2_in(bytes: *const vbyte) -> String {
    if bytes.is_null() {
        return String::new();
    }
    let mut units = Vec::new();
    let mut at = bytes as *const u16;
    while *at != 0 {
        units.push(*at);
        at = at.add(1);
    }
    String::from_utf16_lossy(&units)
}

pub unsafe fn open(title: *mut vbyte, width: i32, height: i32) -> i32 {
    let Ok(event_loop) = EventLoop::new() else {
        return 0;
    };
    let attributes = Window::default_attributes()
        .with_title(ucs2_in(title))
        .with_inner_size(winit::dpi::LogicalSize::new(width.max(1), height.max(1)));

    let mut open = Open {
        event_loop,
        app: App { attributes, window: None, events: 0 },
    };

    // winit creates windows in `resumed`, so the loop has to run before there
    // is one. A few passes, because on some platforms it is not the first.
    for _ in 0..16 {
        open.event_loop
            .pump_app_events(Some(Duration::ZERO), &mut open.app);
        if open.app.window.is_some() {
            break;
        }
    }
    if open.app.window.is_none() {
        return 0;
    }

    WINDOWS.with(|windows| {
        let mut windows = windows.borrow_mut();
        windows.push(Some(open));
        windows.len() as i32
    })
}

pub unsafe fn poll(handle: i32) -> i32 {
    with(handle, 0, |open| {
        open.app.events = 0;
        open.event_loop
            .pump_app_events(Some(Duration::ZERO), &mut open.app);
        // Asked for once, then forgotten: a caller that polls every frame
        // should not see the same resize forever.
        std::mem::take(&mut open.app.events)
    })
}

pub unsafe fn width(handle: i32) -> i32 {
    with(handle, 0, |open| {
        open.app.window.as_ref().map_or(0, |w| w.inner_size().width as i32)
    })
}

pub unsafe fn height(handle: i32) -> i32 {
    with(handle, 0, |open| {
        open.app.window.as_ref().map_or(0, |w| w.inner_size().height as i32)
    })
}

pub unsafe fn platform(handle: i32) -> i32 {
    with(handle, 0, |open| {
        let Some(window) = open.app.window.as_ref() else {
            return 0;
        };
        let Ok(raw) = window.window_handle() else {
            return 0;
        };
        match raw.as_raw() {
            RawWindowHandle::AppKit(_) => APPKIT,
            RawWindowHandle::Win32(_) => WIN32,
            RawWindowHandle::Xlib(_) => XLIB,
            RawWindowHandle::Wayland(_) => WAYLAND,
            _ => 0,
        }
    })
}

/// 0 and 1 are the window handle's fields, 2 and 3 the display's.
///
/// Reported as plain integers because the two libraries are separate: a Rust
/// type cannot cross between them, but the pointer inside it can.
pub unsafe fn raw(handle: i32, which: i32) -> i64 {
    with(handle, 0, |open| {
        let Some(window) = open.app.window.as_ref() else {
            return 0;
        };
        match which {
            0 | 1 => match window.window_handle().map(|h| h.as_raw()) {
                Ok(RawWindowHandle::AppKit(h)) if which == 0 => h.ns_view.as_ptr() as i64,
                Ok(RawWindowHandle::Win32(h)) if which == 0 => h.hwnd.get() as i64,
                Ok(RawWindowHandle::Win32(h)) => h.hinstance.map_or(0, |v| v.get() as i64),
                Ok(RawWindowHandle::Xlib(h)) if which == 0 => h.window as i64,
                Ok(RawWindowHandle::Xlib(h)) => h.visual_id as i64,
                Ok(RawWindowHandle::Wayland(h)) if which == 0 => h.surface.as_ptr() as i64,
                _ => 0,
            },
            2 | 3 => match window.display_handle().map(|h| h.as_raw()) {
                Ok(RawDisplayHandle::Xlib(h)) if which == 2 => {
                    h.display.map_or(0, |p| p.as_ptr() as i64)
                }
                Ok(RawDisplayHandle::Xlib(h)) => h.screen as i64,
                Ok(RawDisplayHandle::Wayland(h)) if which == 2 => h.display.as_ptr() as i64,
                _ => 0,
            },
            _ => 0,
        }
    })
}

pub unsafe fn close(handle: i32) {
    WINDOWS.with(|windows| {
        let mut windows = windows.borrow_mut();
        let index = handle - 1;
        if index >= 0 {
            if let Some(slot) = windows.get_mut(index as usize) {
                *slot = None;
            }
        }
    });
}

/// Silences the unused warning for a type the ABI file declares for everyone.
const _: Option<c_int> = None;
