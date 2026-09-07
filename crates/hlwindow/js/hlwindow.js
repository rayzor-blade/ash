// GENERATED from `window.api`, with the prelude below taken verbatim from
// `js/prelude.js`. Edit one of those, not this file.

// The handle kind numbering, from the same line of the declaration that
// `kinds.rs` comes from.
const KINDS = { window: 1 };




// The import object a page merges into `env`: one entry per primitive,
// each body taken from the declaration.
export function hlwindowImports(rt) {
  const H = makeHandles(rt);
  return {
    hlwindow_open: (title, width, height) => { throw new Error("window: open has no meaning in a page"); },
    // Drains the events waiting and returns what happened: 1 if the window was
    // asked to close, 2 if it was resized. Never blocks.
    hlwindow_poll: (window) => { throw new Error("window: poll has no meaning in a page"); },
    hlwindow_width: (window) => { throw new Error("window: width has no meaning in a page"); },
    hlwindow_height: (window) => { throw new Error("window: height has no meaning in a page"); },
    // Which set of raw handles `raw` reports: 1 AppKit, 2 Win32, 3 Xlib,
    // 4 Wayland, 0 none this library knows.
    hlwindow_platform: (window) => { throw new Error("window: platform has no meaning in a page"); },
    // A pointer-sized field of the raw handle, by index: 0 and 1 are the window's,
    // 2 and 3 the display's. What each means depends on `platform`, and hlwgpu is
    // what puts them back together.
    hlwindow_raw: (window, which) => { throw new Error("window: raw has no meaning in a page"); },
    hlwindow_close: (window) => { throw new Error("window: close has no meaning in a page"); },
  };
}
