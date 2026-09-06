# Running HashLink in WebAssembly

HashLink bytecode compiled to a WebAssembly module by ash, with a browser
compatible runtime. The host below answers what a sandbox cannot do for
itself.

    # the host
    cargo build --release -p ash_browser --target wasm32-unknown-unknown
    wasm-bindgen --target web --out-dir examples/browser \
      target/wasm32-unknown-unknown/release/ash_browser.wasm

    # the program the page runs. ASH_WASM_FIBERS is what makes its thread
    # able to suspend; without it the worker runs to its first block and
    # stays there, and the page never finishes.
    cd examples/browser/demo && haxe -main Demo -hl demo.hl
    ASH_WASM_FIBERS=1 ash --build ../demo.wasm --target wasm32-wasip1 demo.hl

    python3 -m http.server -d examples/browser

Then open the page. It fetches `demo.wasm` and runs it; there is nothing to
click. A module has to be served rather than opened from a file, because
`WebAssembly.instantiate` and ES modules both refuse a `file://` origin.

`demo/Demo.hx` is deliberately not a hello world -- a prime sieve, a Leibniz
series, UTF-16 strings with non-ASCII, a `Map` with closures and sorting, and
a caught exception. A browser that prints the right numbers has run Haxe
rather than merely loaded a module.

The browser needs the standardised exception-handling proposal, because ash's
exception handling is `setjmp` lowered into those instructions and every
module carries `exnref`. Browsers that have shipped it need nothing; node
needs `--experimental-wasm-exnref`.

## Without a browser

`run-node.js` runs the same host under node, which is the only way to
exercise it in a test lane:

    cargo build --release -p ash_browser --target wasm32-unknown-unknown
    wasm-bindgen --target nodejs --out-dir /tmp/host \
      target/wasm32-unknown-unknown/release/ash_browser.wasm
    cp examples/browser/run-node.js /tmp/host/
    node --experimental-wasm-exnref /tmp/host/run-node.js prog.wasm

The code path is identical: the same imports, the same instantiate, the same
entrypoint. What differs is what the global object offers, which is why
`crypto` and `performance` are looked up there rather than on `window` -- a
worker has no `window` either.

## What a page can and cannot do for a program

A browser has no WASI, so the host answers all forty-five of those imports
itself: output goes to `console.log` a line at a time, clocks come from
`Date.now` and `performance.now`, randomness from `crypto.getRandomValues`,
and there is no filesystem -- `path_open` is `ENOTSUP` and every descriptor
above the standard three is `EBADF`. A program that reads a file fails the
way it would with the file absent.

Nothing waits. `poll_oneoff` reports its subscriptions expired at once, so
`Sys.sleep` returns immediately.

## Threads: concurrency, not parallelism

A `sys.thread` worker suspends and resumes in the middle of its body, in a
page. It does not run *alongside* anything: `Thread.create` gives a fiber, and
control moves between fibers only where one of them blocks -- a `Deque.pop`, a
lock, an explicit yield. One thread, one stack running at a time, taking turns.

That is why this needs no `SharedArrayBuffer` and no COOP/COEP headers: there
is no second thread and nothing shared between threads to protect. The saving
is real but it is the saving of a different feature. A program that expects
two threads to make progress at once will not get it here, and a busy loop in
a fiber starves every other fiber and the page with it.

**Parallelism in a browser is a different mechanism entirely** -- Web Workers,
each with its own instance, over one `SharedArrayBuffer` memory, which is what
COOP/COEP are for. ash does not do that on wasm: its collector, its shadow
stacks and its allocator are all written for one thread of execution, and a
worker pool cannot multiplex them. `docs/wasm-fibers.md` says the same thing
about the primitive itself -- "no parallelism (this is suspension, like the
JSPI row, not the worker row)".

What it is instead: ash's link-time transform rewrites the module so its
frames can unwind back to a scheduler and rewind to exactly where they
stopped, and the host drives the three globals that transform adds.
`browser/fibers.rs` is the same thing `native::install_fiber_yield` does,
against `WebAssembly.Global`.

A module built without `ASH_WASM_FIBERS=1` has no such globals. Every call
answers zero and a fiber runs to completion at the point it would have
suspended, which is what the native host does for the same module -- so the
worker in this demo would reach its first `pop` and stay there.

## Not yet

**Loading a native library.** `ash_host_dlopen` answers "no such library", so
a primitive from one raises when it is reached. The steps are the native
host's loader (`docs/wasm-hdlls.md`) against `WebAssembly.instantiate`.
