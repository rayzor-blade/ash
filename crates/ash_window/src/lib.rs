//! The program's side of the window block that a page's `window.mjs` fills.
//!
//! One block of shared memory: a header, an event ring the page writes and a
//! command ring the program writes. [`Window::init`] lays it out; the program
//! then calls `ash_host_agent("window", block)` and reads events with
//! [`Window::poll`] and asks for things with [`Window::send`]. The format is
//! `docs/wasm/window.md`, and this crate must stay in step with it and with
//! `crates/ash_page/page/window.mjs`.
#![no_std]

use core::sync::atomic::{AtomicU32, Ordering};

/// `"ASHW"`, little-endian.
pub const MAGIC: u32 = 0x5748_5341;
pub const VERSION: u32 = 1;
/// Bytes before the event ring.
pub const HEADER: usize = 64;

const W_MAGIC: usize = 0;
const W_VERSION: usize = 1;
const W_EVENT_CAP: usize = 2;
const W_COMMAND_CAP: usize = 3;
const W_EVENT_HEAD: usize = 4;
const W_EVENT_TAIL: usize = 5;
const W_COMMAND_HEAD: usize = 6;
const W_COMMAND_TAIL: usize = 7;
const W_DROPPED: usize = 8;
const W_ATTACHED: usize = 9;
const W_FOCUSED: usize = 10;
const W_WIDTH: usize = 11;
const W_HEIGHT: usize = 12;
const W_SCALE: usize = 13;
const W_SCREEN_WIDTH: usize = 14;
const W_SCREEN_HEIGHT: usize = 15;

/// Event kinds, page to program.
pub mod event {
    pub const RESIZED: u16 = 1;
    pub const FOCUSED: u16 = 2;
    pub const OCCLUDED: u16 = 3;
    pub const CLOSE: u16 = 4;
    pub const POINTER_ENTERED: u16 = 5;
    pub const POINTER_LEFT: u16 = 6;
    pub const POINTER_MOVED: u16 = 7;
    pub const POINTER_BUTTON: u16 = 8;
    pub const POINTER_CANCELLED: u16 = 9;
    pub const WHEEL: u16 = 10;
    pub const KEY: u16 = 11;
    pub const MODIFIERS: u16 = 12;
    pub const THEME: u16 = 13;
    pub const REDRAW: u16 = 14;
    pub const SCALE_FACTOR: u16 = 15;
    pub const IME: u16 = 16;
    pub const FILE_HOVERED: u16 = 17;
    pub const FILE_HOVER_CANCELLED: u16 = 18;
    pub const FILE_DROPPED: u16 = 19;
    pub const FILE_READ: u16 = 20;
    pub const PINCH: u16 = 21;
    pub const TOUCHPAD_PRESSURE: u16 = 22;
    pub const MOUSE_MOTION: u16 = 23;
    pub const SUSPENDED: u16 = 24;
    pub const RESUMED: u16 = 25;
    pub const MONITOR: u16 = 26;
    pub const FULLSCREEN: u16 = 27;
    pub const POINTER_LOCK: u16 = 28;
}

/// Command kinds, program to page.
pub mod command {
    pub const SET_TITLE: u16 = 1;
    pub const SET_CURSOR: u16 = 2;
    pub const REQUEST_REDRAW: u16 = 3;
    pub const SET_SIZE: u16 = 4;
    pub const FOCUS: u16 = 5;
    pub const FULLSCREEN: u16 = 6;
    pub const POINTER_LOCK: u16 = 7;
    pub const SET_MIN_SIZE: u16 = 8;
    pub const SET_MAX_SIZE: u16 = 9;
    pub const SET_VISIBLE: u16 = 10;
    pub const SET_CURSOR_IMAGE: u16 = 11;
    pub const SET_IME_ALLOWED: u16 = 12;
    pub const SET_IME_AREA: u16 = 13;
    pub const READ_FILE: u16 = 14;
    pub const REQUEST_MONITOR: u16 = 15;
}

/// Modifier bits in [`Event::Wheel`], [`Event::Key`] and [`Event::Modifiers`].
pub mod modifiers {
    pub const SHIFT: u32 = 1;
    pub const CONTROL: u32 = 2;
    pub const ALT: u32 = 4;
    pub const META: u32 = 8;
    pub const CAPS_LOCK: u32 = 16;
}

/// Bits in [`Event::Modifiers`]'s `sides`: which side each modifier is held on.
pub mod sides {
    pub const LEFT_SHIFT: u32 = 1;
    pub const RIGHT_SHIFT: u32 = 2;
    pub const LEFT_CONTROL: u32 = 4;
    pub const RIGHT_CONTROL: u32 = 8;
    pub const LEFT_ALT: u32 = 16;
    pub const RIGHT_ALT: u32 = 32;
    pub const LEFT_META: u32 = 64;
    pub const RIGHT_META: u32 = 128;
}

/// `phase` values in [`Event::Pinch`].
pub mod phase {
    pub const STARTED: u32 = 0;
    pub const MOVED: u32 = 1;
    pub const ENDED: u32 = 2;
    pub const CANCELLED: u32 = 3;
}

/// What an [`Event::Ime`] reports.
pub mod ime {
    pub const ENABLED: u32 = 0;
    pub const PREEDIT: u32 = 1;
    pub const COMMIT: u32 = 2;
    pub const DISABLED: u32 = 3;
}

/// `pointer_type` values.
pub mod pointer {
    pub const MOUSE: u32 = 0;
    pub const PEN: u32 = 1;
    pub const TOUCH: u32 = 2;
}

/// Flag bits in [`Event::Key`].
pub mod key_flags {
    pub const REPEAT: u32 = 1;
    pub const SYNTHETIC: u32 = 2;
    pub const COMPOSING: u32 = 4;
}

/// What the page reports. Positions and sizes are physical pixels unless
/// noted; see docs/wasm/window.md for each event's browser source.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Event<'a> {
    Resized {
        width: u32,
        height: u32,
        scale: f64,
    },
    Focused(bool),
    Occluded(bool),
    Close,
    PointerEntered {
        id: i32,
        pointer_type: u32,
    },
    PointerLeft {
        id: i32,
        pointer_type: u32,
    },
    PointerMoved {
        id: i32,
        pointer_type: u32,
        x: f64,
        y: f64,
        dx: f64,
        dy: f64,
        pressure: f32,
        buttons: u32,
    },
    PointerButton {
        id: i32,
        pointer_type: u32,
        pressed: bool,
        button: u32,
        x: f64,
        y: f64,
        pressure: f32,
    },
    PointerCancelled {
        id: i32,
        pointer_type: u32,
    },
    Wheel {
        mode: u32,
        modifiers: u32,
        dx: f64,
        dy: f64,
        x: f64,
        y: f64,
    },
    Key {
        pressed: bool,
        location: u32,
        flags: u32,
        modifiers: u32,
        code: &'a str,
        key: &'a str,
    },
    Modifiers {
        modifiers: u32,
        sides: u32,
    },
    Theme {
        dark: bool,
    },
    Redraw,
    ScaleFactor(f64),
    /// `start`/`end` are UTF-8 byte offsets into `text`, `-1` for none.
    Ime {
        what: u32,
        start: i32,
        end: i32,
        text: &'a str,
    },
    /// MIME types, one per line; names arrive only with the drop.
    FileHovered {
        count: u32,
        types: &'a str,
    },
    FileHoverCancelled,
    FileDropped {
        index: u32,
        size: f64,
        name: &'a str,
        mime: &'a str,
    },
    /// Bytes copied by a read-file command, or `-1`.
    FileRead {
        index: u32,
        bytes: i32,
    },
    Pinch {
        phase: u32,
        delta: f64,
        x: f64,
        y: f64,
    },
    TouchpadPressure {
        pressure: f32,
        stage: u32,
    },
    /// Raw motion while the pointer is locked.
    MouseMotion {
        dx: f64,
        dy: f64,
    },
    Suspended,
    Resumed,
    /// CSS pixels; `label` needs the window-management permission.
    Monitor {
        width: u32,
        height: u32,
        scale: f64,
        label: &'a str,
    },
    Fullscreen(bool),
    PointerLock(bool),
    /// A kind this version does not know, from a newer page.
    Unknown {
        kind: u16,
        payload: &'a [u8],
    },
}

/// What the program asks of the page.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Command<'a> {
    SetTitle(&'a str),
    /// A CSS cursor keyword; `"none"` hides it.
    SetCursor(&'a str),
    RequestRedraw,
    /// CSS pixels; `0` leaves that side to the page.
    SetSize {
        width: f64,
        height: f64,
    },
    Focus,
    Fullscreen(bool),
    PointerLock(bool),
    /// CSS pixels; `0` clears that side.
    SetMinSize {
        width: f64,
        height: f64,
    },
    /// CSS pixels; `0` clears that side.
    SetMaxSize {
        width: f64,
        height: f64,
    },
    SetVisible(bool),
    /// RGBA pixels at `address` in this program's memory, read when the page
    /// drains the command, so they must stay put until the next frame.
    SetCursorImage {
        address: u32,
        width: u32,
        height: u32,
        hot_x: u32,
        hot_y: u32,
    },
    SetImeAllowed(bool),
    /// Physical pixels in the canvas.
    SetImeArea {
        x: f64,
        y: f64,
        width: f64,
        height: f64,
    },
    /// Copy up to `capacity` bytes of dropped file `index` to `address`.
    ReadFile {
        index: u32,
        address: u32,
        capacity: u32,
    },
    RequestMonitor,
}

/// A window block, laid out by [`Window::init`].
pub struct Window {
    block: *mut u8,
}

// The block is shared memory by design: the page reads and writes it from
// another agent, and every index is published atomically.
unsafe impl Send for Window {}

impl Window {
    /// Bytes a block with these ring capacities needs.
    pub const fn block_size(event_capacity: u32, command_capacity: u32) -> usize {
        HEADER + event_capacity as usize + command_capacity as usize
    }

    /// Lay out a block of [`Window::block_size`] bytes at `block`.
    ///
    /// # Safety
    /// `block` must be 8-byte aligned, that many bytes long, and stay valid
    /// and otherwise untouched for as long as the page may write it. Both
    /// capacities must be powers of two, at least 256.
    pub unsafe fn init(block: *mut u8, event_capacity: u32, command_capacity: u32) -> Self {
        debug_assert!(event_capacity.is_power_of_two() && event_capacity >= 256);
        debug_assert!(command_capacity.is_power_of_two() && command_capacity >= 256);
        unsafe { core::ptr::write_bytes(block, 0, HEADER) };
        let window = Window { block };
        window
            .word(W_EVENT_CAP)
            .store(event_capacity, Ordering::Relaxed);
        window
            .word(W_COMMAND_CAP)
            .store(command_capacity, Ordering::Relaxed);
        window.word(W_VERSION).store(VERSION, Ordering::Relaxed);
        window.word(W_MAGIC).store(MAGIC, Ordering::Release);
        window
    }

    /// The block, for `ash_host_agent("window", ...)`.
    pub fn address(&self) -> *mut u8 {
        self.block
    }

    /// Whether the page's shim has taken the block.
    pub fn attached(&self) -> bool {
        self.word(W_ATTACHED).load(Ordering::Acquire) != 0
    }

    /// Whether the canvas has focus.
    pub fn has_focus(&self) -> bool {
        self.word(W_FOCUSED).load(Ordering::Acquire) != 0
    }

    /// The canvas's size in physical pixels.
    pub fn size(&self) -> (u32, u32) {
        (
            self.word(W_WIDTH).load(Ordering::Acquire),
            self.word(W_HEIGHT).load(Ordering::Acquire),
        )
    }

    /// The device pixel ratio.
    pub fn scale_factor(&self) -> f64 {
        f32::from_bits(self.word(W_SCALE).load(Ordering::Acquire)) as f64
    }

    /// The screen's size in CSS pixels.
    pub fn screen_size(&self) -> (u32, u32) {
        (
            self.word(W_SCREEN_WIDTH).load(Ordering::Acquire),
            self.word(W_SCREEN_HEIGHT).load(Ordering::Acquire),
        )
    }

    /// Events the page dropped because the ring was full.
    pub fn dropped(&self) -> u32 {
        self.word(W_DROPPED).load(Ordering::Relaxed)
    }

    /// The event head, for a program that waits on it (`memory.atomic.wait32`)
    /// while there is nothing to read; the page notifies it on every event.
    pub fn event_head(&self) -> &AtomicU32 {
        self.word(W_EVENT_HEAD)
    }

    /// Hand every event the page has written to `f`, oldest first, and return
    /// how many there were. A string in an event is valid only during the call.
    pub fn poll(&self, mut f: impl FnMut(Event<'_>)) -> usize {
        let mut count = 0;
        self.events().read(|kind, payload| {
            f(decode(kind, payload));
            count += 1;
        });
        count
    }

    /// Queue `command` for the page, which drains it at its next animation
    /// frame. `false` when the ring has no room for it.
    pub fn send(&self, command: Command<'_>) -> bool {
        let ring = self.commands();
        match command {
            Command::SetTitle(s) => ring.write(command::SET_TITLE, &[Field::Str(s)]),
            Command::SetCursor(s) => ring.write(command::SET_CURSOR, &[Field::Str(s)]),
            Command::RequestRedraw => ring.write(command::REQUEST_REDRAW, &[]),
            Command::SetSize { width, height } => {
                ring.write(command::SET_SIZE, &[Field::F64(width), Field::F64(height)])
            }
            Command::Focus => ring.write(command::FOCUS, &[]),
            Command::Fullscreen(on) => ring.write(command::FULLSCREEN, &[Field::U32(on as u32)]),
            Command::PointerLock(on) => ring.write(command::POINTER_LOCK, &[Field::U32(on as u32)]),
            Command::SetMinSize { width, height } => ring.write(
                command::SET_MIN_SIZE,
                &[Field::F64(width), Field::F64(height)],
            ),
            Command::SetMaxSize { width, height } => ring.write(
                command::SET_MAX_SIZE,
                &[Field::F64(width), Field::F64(height)],
            ),
            Command::SetVisible(on) => ring.write(command::SET_VISIBLE, &[Field::U32(on as u32)]),
            Command::SetCursorImage {
                address,
                width,
                height,
                hot_x,
                hot_y,
            } => ring.write(
                command::SET_CURSOR_IMAGE,
                &[
                    Field::U32(address),
                    Field::U32(width),
                    Field::U32(height),
                    Field::U32(hot_x),
                    Field::U32(hot_y),
                ],
            ),
            Command::SetImeAllowed(on) => {
                ring.write(command::SET_IME_ALLOWED, &[Field::U32(on as u32)])
            }
            Command::SetImeArea {
                x,
                y,
                width,
                height,
            } => ring.write(
                command::SET_IME_AREA,
                &[
                    Field::F64(x),
                    Field::F64(y),
                    Field::F64(width),
                    Field::F64(height),
                ],
            ),
            Command::ReadFile {
                index,
                address,
                capacity,
            } => ring.write(
                command::READ_FILE,
                &[Field::U32(index), Field::U32(address), Field::U32(capacity)],
            ),
            Command::RequestMonitor => ring.write(command::REQUEST_MONITOR, &[]),
        }
    }

    fn word(&self, index: usize) -> &AtomicU32 {
        // SAFETY: `init`'s contract: the header is 64 valid, aligned bytes.
        unsafe { &*(self.block.add(index * 4) as *const AtomicU32) }
    }

    fn capacity(&self, index: usize) -> u32 {
        self.word(index).load(Ordering::Relaxed)
    }

    fn events(&self) -> Ring<'_> {
        Ring {
            data: unsafe { self.block.add(HEADER) },
            capacity: self.capacity(W_EVENT_CAP),
            head: self.word(W_EVENT_HEAD),
            tail: self.word(W_EVENT_TAIL),
            dropped: Some(self.word(W_DROPPED)),
        }
    }

    fn commands(&self) -> Ring<'_> {
        let event_capacity = self.capacity(W_EVENT_CAP) as usize;
        Ring {
            data: unsafe { self.block.add(HEADER + event_capacity) },
            capacity: self.capacity(W_COMMAND_CAP),
            head: self.word(W_COMMAND_HEAD),
            tail: self.word(W_COMMAND_TAIL),
            dropped: None,
        }
    }
}

/// One payload field, little-endian and 4-aligned.
#[derive(Clone, Copy)]
enum Field<'a> {
    U32(u32),
    // Only events carry these, and the page writes events; tests stand in for it.
    #[cfg_attr(not(test), allow(dead_code))]
    I32(i32),
    #[cfg_attr(not(test), allow(dead_code))]
    F32(f32),
    F64(f64),
    Str(&'a str),
}

impl Field<'_> {
    fn size(&self) -> usize {
        match self {
            Field::U32(_) | Field::I32(_) | Field::F32(_) => 4,
            Field::F64(_) => 8,
            Field::Str(s) => 4 + ((s.len() + 3) & !3),
        }
    }
}

/// A single-writer, single-reader ring of whole records.
struct Ring<'a> {
    data: *mut u8,
    capacity: u32,
    head: &'a AtomicU32,
    tail: &'a AtomicU32,
    dropped: Option<&'a AtomicU32>,
}

impl Ring<'_> {
    /// Append one record. A record never wraps: one that does not fit before
    /// the end is preceded by a pad record filling it.
    fn write(&self, kind: u16, fields: &[Field<'_>]) -> bool {
        let length = 4 + fields.iter().map(Field::size).sum::<usize>();
        let cap = self.capacity as usize;
        if length > cap || length > 0xfffc {
            return self.drop_one();
        }
        let mut head = self.head.load(Ordering::Relaxed);
        let tail = self.tail.load(Ordering::Acquire);
        let used = head.wrapping_sub(tail) as usize;
        let pos = head as usize % cap;
        let to_end = cap - pos;
        let need = if length > to_end {
            to_end + length
        } else {
            length
        };
        if cap - used < need {
            return self.drop_one();
        }
        let mut at = pos;
        if length > to_end {
            self.put_u32(pos, (to_end as u32) << 16);
            head = head.wrapping_add(to_end as u32);
            at = 0;
        }
        self.put_u32(at, kind as u32 | ((length as u32) << 16));
        let mut offset = at + 4;
        for field in fields {
            match *field {
                Field::U32(v) => self.put_u32(offset, v),
                Field::I32(v) => self.put_u32(offset, v as u32),
                Field::F32(v) => self.put_u32(offset, v.to_bits()),
                Field::F64(v) => self.put(offset, &v.to_bits().to_le_bytes()),
                Field::Str(s) => {
                    self.put_u32(offset, s.len() as u32);
                    self.put(offset + 4, s.as_bytes());
                    let pad = ((s.len() + 3) & !3) - s.len();
                    self.put(offset + 4 + s.len(), &[0u8; 3][..pad]);
                }
            }
            offset += field.size();
        }
        self.head
            .store(head.wrapping_add(length as u32), Ordering::Release);
        true
    }

    fn drop_one(&self) -> bool {
        if let Some(dropped) = self.dropped {
            dropped.fetch_add(1, Ordering::Relaxed);
        }
        false
    }

    /// Hand every published record to `f`, then release the space.
    fn read(&self, mut f: impl FnMut(u16, &[u8])) {
        let cap = self.capacity as usize;
        let head = self.head.load(Ordering::Acquire);
        let mut tail = self.tail.load(Ordering::Relaxed);
        while tail != head {
            let at = tail as usize % cap;
            let word = self.get_u32(at);
            let kind = (word & 0xffff) as u16;
            let length = (word >> 16) as usize;
            if length < 4 || length > cap - at {
                break; // corrupt; stop rather than read past the ring
            }
            if kind != 0 {
                // SAFETY: within the ring, and published by the writer.
                let payload =
                    unsafe { core::slice::from_raw_parts(self.data.add(at + 4), length - 4) };
                f(kind, payload);
            }
            tail = tail.wrapping_add(length as u32);
            self.tail.store(tail, Ordering::Release);
        }
    }

    fn put(&self, at: usize, bytes: &[u8]) {
        // SAFETY: `write` keeps every record inside the ring.
        unsafe { core::ptr::copy_nonoverlapping(bytes.as_ptr(), self.data.add(at), bytes.len()) };
    }

    fn put_u32(&self, at: usize, v: u32) {
        self.put(at, &v.to_le_bytes());
    }

    fn get_u32(&self, at: usize) -> u32 {
        // SAFETY: `at` is a record start inside the ring.
        u32::from_le_bytes(unsafe {
            core::ptr::read_unaligned(self.data.add(at) as *const [u8; 4])
        })
    }
}

/// Reads fields in order from a payload; a short payload reads as zeros.
struct Fields<'a> {
    bytes: &'a [u8],
    at: usize,
}

impl<'a> Fields<'a> {
    fn take<const N: usize>(&mut self) -> [u8; N] {
        let mut out = [0u8; N];
        if let Some(src) = self.bytes.get(self.at..self.at + N) {
            out.copy_from_slice(src);
        }
        self.at += N;
        out
    }
    fn u32(&mut self) -> u32 {
        u32::from_le_bytes(self.take())
    }
    fn i32(&mut self) -> i32 {
        i32::from_le_bytes(self.take())
    }
    fn f32(&mut self) -> f32 {
        f32::from_le_bytes(self.take())
    }
    fn f64(&mut self) -> f64 {
        f64::from_le_bytes(self.take())
    }
    fn str(&mut self) -> &'a str {
        let n = self.u32() as usize;
        let s = self.bytes.get(self.at..self.at + n).unwrap_or(&[]);
        self.at += (n + 3) & !3;
        core::str::from_utf8(s).unwrap_or("")
    }
}

fn decode(kind: u16, payload: &[u8]) -> Event<'_> {
    let mut p = Fields {
        bytes: payload,
        at: 0,
    };
    match kind {
        event::RESIZED => Event::Resized {
            width: p.u32(),
            height: p.u32(),
            scale: p.f64(),
        },
        event::FOCUSED => Event::Focused(p.u32() != 0),
        event::OCCLUDED => Event::Occluded(p.u32() != 0),
        event::CLOSE => Event::Close,
        event::POINTER_ENTERED => Event::PointerEntered {
            id: p.i32(),
            pointer_type: p.u32(),
        },
        event::POINTER_LEFT => Event::PointerLeft {
            id: p.i32(),
            pointer_type: p.u32(),
        },
        event::POINTER_MOVED => Event::PointerMoved {
            id: p.i32(),
            pointer_type: p.u32(),
            x: p.f64(),
            y: p.f64(),
            dx: p.f64(),
            dy: p.f64(),
            pressure: p.f32(),
            buttons: p.u32(),
        },
        event::POINTER_BUTTON => Event::PointerButton {
            id: p.i32(),
            pointer_type: p.u32(),
            pressed: p.u32() != 0,
            button: p.u32(),
            x: p.f64(),
            y: p.f64(),
            pressure: p.f32(),
        },
        event::POINTER_CANCELLED => Event::PointerCancelled {
            id: p.i32(),
            pointer_type: p.u32(),
        },
        event::WHEEL => Event::Wheel {
            mode: p.u32(),
            modifiers: p.u32(),
            dx: p.f64(),
            dy: p.f64(),
            x: p.f64(),
            y: p.f64(),
        },
        event::KEY => Event::Key {
            pressed: p.u32() != 0,
            location: p.u32(),
            flags: p.u32(),
            modifiers: p.u32(),
            code: p.str(),
            key: p.str(),
        },
        event::MODIFIERS => Event::Modifiers {
            modifiers: p.u32(),
            sides: p.u32(),
        },
        event::THEME => Event::Theme { dark: p.u32() != 0 },
        event::REDRAW => Event::Redraw,
        event::SCALE_FACTOR => Event::ScaleFactor(p.f64()),
        event::IME => Event::Ime {
            what: p.u32(),
            start: p.i32(),
            end: p.i32(),
            text: p.str(),
        },
        event::FILE_HOVERED => Event::FileHovered {
            count: p.u32(),
            types: p.str(),
        },
        event::FILE_HOVER_CANCELLED => Event::FileHoverCancelled,
        event::FILE_DROPPED => Event::FileDropped {
            index: p.u32(),
            size: p.f64(),
            name: p.str(),
            mime: p.str(),
        },
        event::FILE_READ => Event::FileRead {
            index: p.u32(),
            bytes: p.i32(),
        },
        event::PINCH => Event::Pinch {
            phase: p.u32(),
            delta: p.f64(),
            x: p.f64(),
            y: p.f64(),
        },
        event::TOUCHPAD_PRESSURE => Event::TouchpadPressure {
            pressure: p.f32(),
            stage: p.u32(),
        },
        event::MOUSE_MOTION => Event::MouseMotion {
            dx: p.f64(),
            dy: p.f64(),
        },
        event::SUSPENDED => Event::Suspended,
        event::RESUMED => Event::Resumed,
        event::MONITOR => Event::Monitor {
            width: p.u32(),
            height: p.u32(),
            scale: p.f64(),
            label: p.str(),
        },
        event::FULLSCREEN => Event::Fullscreen(p.u32() != 0),
        event::POINTER_LOCK => Event::PointerLock(p.u32() != 0),
        kind => Event::Unknown { kind, payload },
    }
}

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;
    use std::vec::Vec;

    /// A block in 8-aligned memory owned by the test.
    fn block(event_cap: u32, command_cap: u32) -> (Vec<u64>, Window) {
        let mut memory = std::vec![0u64; Window::block_size(event_cap, command_cap).div_ceil(8)];
        let window =
            unsafe { Window::init(memory.as_mut_ptr() as *mut u8, event_cap, command_cap) };
        (memory, window)
    }

    /// What window.mjs does, through the same writer: page-side events.
    fn page_emit(window: &Window, kind: u16, fields: &[Field<'_>]) -> bool {
        window.events().write(kind, fields)
    }

    #[test]
    fn events_decode_in_order_with_their_fields() {
        let (_memory, window) = block(256, 256);
        assert!(!window.attached());
        page_emit(
            &window,
            event::RESIZED,
            &[Field::U32(1280), Field::U32(720), Field::F64(2.0)],
        );
        page_emit(
            &window,
            event::KEY,
            &[
                Field::U32(1),
                Field::U32(0),
                Field::U32(key_flags::REPEAT),
                Field::U32(modifiers::SHIFT),
                Field::Str("KeyA"),
                Field::Str("A"),
            ],
        );
        page_emit(&window, event::REDRAW, &[]);
        let mut seen = Vec::new();
        let n = window.poll(|e| seen.push(std::format!("{e:?}")));
        assert_eq!(n, 3);
        assert_eq!(seen[0], "Resized { width: 1280, height: 720, scale: 2.0 }");
        assert_eq!(
            seen[1],
            "Key { pressed: true, location: 0, flags: 1, modifiers: 1, code: \"KeyA\", key: \"A\" }"
        );
        assert_eq!(seen[2], "Redraw");
        assert_eq!(window.poll(|_| panic!("already read")), 0);
    }

    #[test]
    fn a_record_that_would_straddle_the_end_starts_after_a_pad() {
        let (_memory, window) = block(256, 256);
        // 40-byte pointer-button records: six fill 240 of 256 bytes.
        let button = [
            Field::I32(1),
            Field::U32(0),
            Field::U32(1),
            Field::U32(0),
            Field::F64(1.0),
            Field::F64(2.0),
            Field::F32(0.5),
        ];
        for _ in 0..6 {
            assert!(page_emit(&window, event::POINTER_BUTTON, &button));
        }
        assert_eq!(window.poll(|_| {}), 6);
        // The seventh does not fit in the 16 bytes left: pad, then wrap.
        assert!(page_emit(&window, event::POINTER_BUTTON, &button));
        let mut right = false;
        assert_eq!(window.poll(|e| {
            right = matches!(e, Event::PointerButton { pressed: true, x, y, .. } if x == 1.0 && y == 2.0)
        }), 1);
        assert!(right);
    }

    #[test]
    fn a_full_ring_drops_and_counts() {
        let (_memory, window) = block(256, 256);
        let mut written = 0;
        while page_emit(&window, event::THEME, &[Field::U32(1)]) {
            written += 1;
        }
        assert_eq!(written, 256 / 8);
        assert_eq!(window.dropped(), 1);
        assert_eq!(window.poll(|_| {}), written);
    }

    #[test]
    fn text_input_and_file_events_carry_their_strings() {
        let (_memory, window) = block(512, 256);
        page_emit(
            &window,
            event::IME,
            &[
                Field::U32(ime::PREEDIT),
                Field::I32(3),
                Field::I32(6),
                Field::Str("にほ"),
            ],
        );
        page_emit(
            &window,
            event::FILE_DROPPED,
            &[
                Field::U32(0),
                Field::F64(12.0),
                Field::Str("notes.txt"),
                Field::Str("text/plain"),
            ],
        );
        page_emit(
            &window,
            event::MODIFIERS,
            &[Field::U32(modifiers::SHIFT), Field::U32(sides::LEFT_SHIFT)],
        );
        let mut seen = Vec::new();
        window.poll(|e| seen.push(std::format!("{e:?}")));
        assert_eq!(seen[0], "Ime { what: 1, start: 3, end: 6, text: \"にほ\" }");
        assert_eq!(
            seen[1],
            "FileDropped { index: 0, size: 12.0, name: \"notes.txt\", mime: \"text/plain\" }"
        );
        assert_eq!(seen[2], "Modifiers { modifiers: 1, sides: 1 }");
    }

    #[test]
    fn commands_encode_as_the_page_reads_them() {
        let (_memory, window) = block(256, 256);
        assert!(window.send(Command::SetTitle("Tally")));
        assert!(window.send(Command::SetSize {
            width: 640.0,
            height: 0.0
        }));
        assert!(window.send(Command::Fullscreen(true)));
        assert!(window.send(Command::ReadFile {
            index: 2,
            address: 0x1000,
            capacity: 64
        }));
        let mut seen = Vec::new();
        window
            .commands()
            .read(|kind, payload| seen.push((kind, Vec::from(payload))));
        assert_eq!(seen[0].0, command::SET_TITLE);
        assert_eq!(
            seen[0].1,
            [5, 0, 0, 0, b'T', b'a', b'l', b'l', b'y', 0, 0, 0]
        );
        assert_eq!(seen[1].0, command::SET_SIZE);
        assert_eq!(
            f64::from_le_bytes(seen[1].1[..8].try_into().unwrap()),
            640.0
        );
        assert_eq!(seen[2], (command::FULLSCREEN, std::vec![1, 0, 0, 0]));
        assert_eq!(
            seen[3],
            (
                command::READ_FILE,
                std::vec![2, 0, 0, 0, 0, 0x10, 0, 0, 64, 0, 0, 0]
            )
        );
    }
}
