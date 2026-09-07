# What a host must supply for `window`

GENERATED from `window.api`.

Natively none of this applies: `window.hdll` holds the implementation and
answers the VM directly.

On wasm, `window.wasm` holds no implementation. It imports the following
from `env`, and whatever instantiates the module has to provide them.
A page gets them from `hlwgpu.js`; any other host implements this table.

Every handle is an `i32`, and every string is a pointer to
NUL-terminated UTF-16 allocated through the program's `hlp_alloc_bytes`.

| import | parameters | returns |
|---|---|---|
| `hlwindow_open` | `title: i32`, `width: i32`, `height: i32` | `i32` |
| `hlwindow_poll` | `window: i32` | `i32` |
| `hlwindow_width` | `window: i32` | `i32` |
| `hlwindow_height` | `window: i32` | `i32` |
| `hlwindow_platform` | `window: i32` | `i32` |
| `hlwindow_raw` | `window: i32`, `which: i32` | `i64` |
| `hlwindow_close` | `window: i32` | `void` |
