# Ash Future

`ash.Future<T>` carries one result or Haxe exception from an asynchronous
native operation. `await()` returns `T` or raises the rejection. A pending
await parks the current Ash fiber. `isReady()` is true for either completion,
and `isRejected()` distinguishes an error. `then(onValue, onError)` schedules
a Haxe continuation on an Ash thread.

```haxe
var future:ash.Future<String> = NativeApi.request();
future.then(value -> Sys.println(value), error -> Sys.println(error));
var value:String = future.await();
```

Native libraries and wasm objects include `std/ash_future.h` and call its
`hlp_future_*` functions from the **program's** runtime. They must not link a
second copy of `ash_std`. A Haxe native returning `hl.Abstract<"ash_future">`
can return the pointer from `hlp_future_create()` directly.

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
