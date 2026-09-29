# Ash Future

`ash.Future<T>` carries one result or Haxe exception from an asynchronous
native operation. `await()` returns `T` or raises the rejection. A pending
await parks the current Ash fiber; on stock HashLink it blocks the calling
HashLink thread while allowing collection. `isReady()` is true for either
completion, and `isRejected()` distinguishes an error. `then(onValue, onError)`
schedules a Haxe continuation on a Haxe thread.

```haxe
var future:ash.Future<String> = NativeApi.request();
future.then(value -> Sys.println(value), error -> Sys.println(error));
var value:String = future.await();
```

Ash builds use the existing `std@future_*` primitives and fiber scheduler.
For stock HashLink, compile with `-D ash_future_stock` and `-lib ash-future`;
the Haxelib release ZIP stages `ash_future.hdll` beside the generated `.hl`.
Use the ZIP from the Ash release assets: a source checkout does not include
platform binaries. The same Haxe `Future<T>` API works with both runtimes.
For stock CLI runs, make the bytecode directory visible to the OS library
loader (for example, use `LD_LIBRARY_PATH` on Linux, `DYLD_LIBRARY_PATH` on
macOS, or `PATH` on Windows), or install `ash_future.hdll` in HashLink's
library directory.
Stock HashLink must have a thread or event loop able to complete a pending
future while another thread waits in `await()`.

Native libraries and wasm objects include `std/ash_future.h` or use the Rust
`ash_future_abi` crate alongside `hl_abi`. They call the `hlp_future_*`
functions supplied by **the running implementation**: Ash's program runtime
or the stock `ash_future.hdll`. They must not link a second copy of `ash_std`.
A Haxe native returning `hl.Abstract<"ash_future">` can return the pointer
from `hlp_future_create()` directly.

```c
ash_future *future = hlp_future_create();
/* Return future to Haxe, then complete it from the native callback. */
bool first = hlp_future_resolve(future, boxed_result);
```

`boxed_result` and a rejection value are HashLink `vdynamic *` values (or
null), allocated in the same runtime. Keep a value rooted while a callback
owns it before completion; the Future roots it from the successful completion
until the Future is collected. The pending handle remains rooted until its
first completion, even when Haxe drops its copy. After completion, native
code may use the handle only while another live owner retains it. A second
completion returns false, and a rejected `await()` raises the original Haxe
value.

Wasm builds work with or without the fiber transform. Without it, Ash threads
run to completion when the scheduler reaches them. A pending `await()` can
drive an already scheduled Ash thread, but it cannot make a browser callback
run while the current synchronous wasm call remains on the stack. Use a fiber
build when completion needs that call to suspend.

The C header is deliberately independent of `hl.h`. Rust plugins use
`ash_future_abi` alongside `hl_abi`; it declares the same imports without
linking a runtime crate into the plugin.

For a native plugin on stock HashLink, link its `hlp_future_*` imports to
`ash_future.hdll` and load that library from the same installation. The Rust
HDLL in `crates/ash_hdll_future` uses `hl_abi` and imports HashLink's GC and
thread APIs; it does not link Ash's runtime. A completion callback from a
foreign OS thread is registered with HashLink's collector for the call. Keep
any Haxe value rooted until that call returns. The Windows Haxelib package
includes `native/windows-x86_64/ash_future.lib` for linking native plugins.
On Linux, a Rust plugin can link directly to the staged HDLL with
`-L native=<directory>` and `-C link-arg=-Wl,-l:ash_future.hdll`; the linker
records `ash_future.hdll` as its dependency. Native callers of
`hlp_future_await` should inspect `hlp_future_state` for rejection on stock
HashLink. Haxe callers use `Future.await()`, which raises on both runtimes.
