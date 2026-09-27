# The window in a page

A program in a page reaches its window through one block of shared memory and
the page's `window.mjs` shim. The page writes what happens to the canvas into
the block; the program writes what it wants done. No import beyond
`ash_host_agent` is involved, and the Rust side of the format is the
`ash_window` crate. The events and commands cover every window member and
event that has a browser counterpart; the ones that have none are listed at
the end.

## Opening it

1. Allocate a block of `64 + event_capacity + command_capacity` bytes in the
   program's shared memory, 8-byte aligned. Both capacities are powers of two,
   at least 256.
2. Write the header below: magic, version, the two capacities, every other
   word zero.
3. Call `ash_host_agent("window", block)`. The page imports its `window.mjs`
   and starts it; `1` means a page took the request. The shim sets `attached`
   once it has checked the header, fills the state words, then writes a
   `resized`, a `focused` and a `theme` event describing the window as it is.

The memory must be shared (a `wasm32-wasip1-threads` program), or the page
cannot see it and `ash_host_agent` answers `0`. A host with no page answers
`0` too.

## The header

Little-endian `u32` words at byte offsets from the block:

| Offset | Field | Written by |
|---|---|---|
| 0 | magic, `0x57485341` (`"ASHW"`) | program |
| 4 | version, `1` | program |
| 8 | event ring capacity in bytes | program |
| 12 | command ring capacity in bytes | program |
| 16 | event head | page |
| 20 | event tail | program |
| 24 | command head | program |
| 28 | command tail | page |
| 32 | events dropped because the ring was full | page |
| 36 | attached: `1` once the page's shim runs | page |
| 40 | focused: `1` while the canvas has focus | page |
| 44 | width, physical pixels | page |
| 48 | height, physical pixels | page |
| 52 | scale factor (device pixel ratio), `f32` bits | page |
| 56 | screen width, CSS pixels | page |
| 60 | screen height, CSS pixels | page |

The page stores the state words atomically whenever they change, before the
event that reports the change, so a program can read them at any time instead
of keeping its own copy. The event ring starts at byte 64; the command ring
follows it.

## Rings

Each ring has one writer and one reader. Head and tail are byte counters that
only grow, modulo 2^32; the position in the ring is the counter modulo the
capacity, and the space in use is `head - tail`. The writer stores the record,
then publishes it by storing the new head atomically; the reader loads the
head atomically, reads the records up to it, then stores the new tail
atomically. The page notifies the event head (`Atomics.notify`) after each
publish, so a program with nothing to do may `Atomics.wait` on it.

A record is a `u32` header, `kind | (length << 16)`, where `length` is the
whole record in bytes, a multiple of 4, then its payload. A record never
wraps: when it does not fit before the end of the ring, the writer fills the
rest with a pad record (kind `0`) and starts at the ring's beginning. A record
that does not fit at all is dropped, and for events the page counts it in the
header.

Payload fields are little-endian and packed in the order listed, with no
alignment beyond 4 bytes: an `f64` may sit at an offset that is a multiple of
4 only. A string is a `u32` byte length, then UTF-8, padded with zeros to a
multiple of 4.

## Events: page to program

Positions and sizes are physical pixels (CSS pixels times the device pixel
ratio) unless a row says otherwise. `pointer_type` is 0 for a mouse, 1 for a
pen, 2 for touch; a touch is a pointer of type 2, so its phases are entered,
button pressed (started), moved, button released (ended) and cancelled, and
its force is the pressure. `modifiers` is a bit set: shift 1, control 2, alt
4, meta 8, caps lock 16. A `phase` is 0 started, 1 moved, 2 ended,
3 cancelled.

| Kind | Event | Payload | Browser source |
|---|---|---|---|
| 1 | resized | `u32` width, `u32` height, `f64` scale | `ResizeObserver` (device-pixel-content-box) |
| 2 | focused | `u32` focused | `focus`, `blur` |
| 3 | occluded | `u32` hidden | `visibilitychange` |
| 4 | close | none | `pagehide` when the page is not kept for back/forward |
| 5 | pointer entered | `i32` id, `u32` pointer_type | `pointerenter` |
| 6 | pointer left | `i32` id, `u32` pointer_type | `pointerleave` |
| 7 | pointer moved | `i32` id, `u32` pointer_type, `f64` x, `f64` y, `f64` dx, `f64` dy, `f32` pressure, `u32` buttons | `pointermove`, coalesced events included; `dx`/`dy` from `movementX`/`movementY` |
| 8 | pointer button | `i32` id, `u32` pointer_type, `u32` pressed, `u32` button, `f64` x, `f64` y, `f32` pressure | `pointerdown`, `pointerup`; button 0-4 is left, middle, right, back, forward, higher numbers are other buttons |
| 9 | pointer cancelled | `i32` id, `u32` pointer_type | `pointercancel` |
| 10 | wheel | `u32` mode, `u32` modifiers, `f64` dx, `f64` dy, `f64` x, `f64` y | `wheel`; mode 0 pixels (physical), 1 lines, 2 pages |
| 11 | key | `u32` pressed, `u32` location, `u32` flags, `u32` modifiers, string code, string key | `keydown`, `keyup`; location 0 standard, 1 left, 2 right, 3 numpad; flags: repeat 1, synthetic 2 (`isTrusted` false), composing 4. `code` is the physical key, `key` the logical key or the character typed |
| 12 | modifiers | `u32` modifiers, `u32` sides | sent when either changes; sides: left shift 1, right shift 2, left control 4, right control 8, left alt 16, right alt 32, left meta 64, right meta 128 |
| 13 | theme | `u32` dark | `prefers-color-scheme` |
| 14 | redraw | none | the animation frame after a redraw request |
| 15 | scale factor | `f64` scale | `matchMedia("(resolution: <n>dppx)")` change |
| 16 | ime | `u32` what, `i32` start, `i32` end, string text | what: 0 enabled, 1 preedit, 2 commit, 3 disabled. Preedit carries the selection as UTF-8 byte offsets into its text, `-1` for none. `EditContext` where the browser has it, a hidden `<textarea>` with composition events where it does not |
| 17 | file hovered | `u32` count, string types | `dragenter`; types are the MIME types, one per line. A page learns names only on drop |
| 18 | file hover cancelled | none | `dragleave` out of the canvas |
| 19 | file dropped | `u32` index, `f64` size, string name, string type | `drop`, one per file. The index names it in a read-file command |
| 20 | file read | `u32` index, `i32` bytes | the answer to a read-file command: bytes written, or `-1` |
| 21 | pinch | `u32` phase, `f64` delta, `f64` x, `f64` y | `wheel` with control set, as browsers report a trackpad pinch; delta is the change in scale |
| 22 | touchpad pressure | `f32` pressure, `u32` stage | `webkitmouseforcechanged` where there is one, otherwise a mouse pointer's `pressure`; stage 1 click, 2 force click |
| 23 | mouse motion | `f64` dx, `f64` dy | `pointermove` while the pointer is locked: raw motion, as a device event |
| 24 | suspended | none | `pagehide` when the page is kept for back/forward |
| 25 | resumed | none | `pageshow` from back/forward |
| 26 | monitor | `u32` width, `u32` height, `f64` scale, string label | the answer to a request-monitor command: `getScreenDetails` where permitted (the label needs the window-management permission), otherwise `screen` and an empty label; CSS pixels |
| 27 | fullscreen | `u32` on | `fullscreenchange` |
| 28 | pointer lock | `u32` on | `pointerlockchange` |

Keyboard events are taken while the canvas (or, for text input without
`EditContext`, its hidden textarea) has focus; the shim makes the canvas
focusable (`tabIndex`) and focuses it on a pointer press, and focus moving
between the canvas and its textarea is not a focus change. Every key arrives
as a key event. While text input is allowed (set ime allowed), keys also keep
their default action, so typed text, from a physical keyboard, an input
method or a phone's on-screen keyboard, arrives as ime commit events; while
it is not, the shim suppresses keys' default actions so they do not scroll or
navigate the page.

## Commands: program to page

The page drains the command ring once per animation frame.

| Kind | Command | Payload | Browser effect |
|---|---|---|---|
| 1 | set title | string | `document.title` |
| 2 | set cursor | string | the canvas's CSS `cursor`, a CSS keyword; `none` hides it |
| 3 | request redraw | none | a redraw event at the next animation frame |
| 4 | set size | `f64` width, `f64` height | the canvas's CSS size in CSS pixels; `0` leaves that side to the page's CSS. The resized event that follows carries the physical size, which is what the drawing buffer's owner sets |
| 5 | focus | none | focuses the canvas |
| 6 | fullscreen | `u32` on | `requestFullscreen` / `exitFullscreen` |
| 7 | pointer lock | `u32` on | `requestPointerLock` / `exitPointerLock` |
| 8 | set min size | `f64` width, `f64` height | CSS `min-width`/`min-height`, CSS pixels; `0` clears |
| 9 | set max size | `f64` width, `f64` height | CSS `max-width`/`max-height`, CSS pixels; `0` clears |
| 10 | set visible | `u32` visible | the canvas's `hidden` |
| 11 | set cursor image | `u32` address, `u32` width, `u32` height, `u32` hot_x, `u32` hot_y | RGBA pixels at `address` in the program's memory, read when the command is drained, become `cursor: url(...) x y` |
| 12 | set ime allowed | `u32` allowed | attaches or detaches the text input; reported by ime enabled/disabled |
| 13 | set ime area | `f64` x, `f64` y, `f64` width, `f64` height | physical pixels in the canvas; the composition window follows it (`updateControlBounds`, `updateSelectionBounds`, or the textarea's position) |
| 14 | read file | `u32` index, `u32` address, `u32` capacity | copies up to `capacity` bytes of a dropped file to `address`, then a file-read event |
| 15 | request monitor | none | a monitor event |

A browser grants fullscreen and pointer lock only inside a user's input, so
those two run in the next `pointerdown` or `keydown` handler after they
arrive; exiting runs at once.

## What a page cannot do

These window members and events have no browser API, so a page never
produces or honours them: the window's position (moved, set position),
decorations, resizability, maximizing, blur, content protection and window
level; pan, rotation and double-tap gestures; axis motion and raw device
added, removed, button and key events; activation tokens; memory warnings;
and closing the window from the program.
