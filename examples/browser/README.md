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

    # the threaded program, and the other demo the page can run
    haxe -cp . -main Threads -hl threads.hl
    ASH_WASM_FIBERS=1 ash --build ../threads.wasm \
      --target wasm32-wasip1-threads threads.hl

    ./examples/browser/serve.py

Then open <http://127.0.0.1:8731>. It starts a worker, which fetches the
module and runs it; there is nothing to click. `?demo=threads` runs the other
one.

`serve.py` rather than `python3 -m http.server` because of the threaded demo:
it needs a `SharedArrayBuffer`, a page only has one when it is cross-origin
isolated, and that means two headers on every response. The single-threaded
demo does not care and works under either.

## Threads

`?demo=threads` runs four Haxe threads, each in a Worker of its own, and times
them against the same work done one after another. A thread on wasm is a
second instance of the same module over one shared memory, entered through
`wasi_thread_start` -- so the page's job is to make the agent, and everything
about what the thread does was decided by the guest's own `pthread_create`
before it asked for one.

Who does what: `worker.js` gives `run` a `spawn` function; the host hands that
function the compiled module, the shared memory, a thread id and the guest's
`startArg`; `thread.js` is the Worker that receives them and calls
`run_thread`. The crate never names a URL, for the same reason it does not
fetch the module.

**The agents are started before the program is, and that is the one thing
here that cannot be arranged any other way.** Creating a Worker needs the
creating agent to return to its event loop. The agent asking for a thread is
inside a synchronous call into wasm which will not return until that thread
has answered -- so a Worker created at that moment never loads, and the
program waits for it forever. Measured, exactly: on-demand creation hung with
no output at all, and warming the same agents first ran in 338ms against
1016ms serial. Emscripten's `PTHREAD_POOL_SIZE` exists for this reason.

It is also the one real bound on "as many threads as you like" in a page: the
bound is how many agents the page warmed, not anything the runtime asked for.
The runtime asks for an agent per thread and takes what it gets; a thread with
none free runs on the main scheduler, and the page says so.

One thing to know when reading the output: the lines a thread prints arrive
after the program's, because they are forwarded through `worker.js`, which
cannot run its message handler until the program returns. It is a forwarding
order, not an execution order.

## Testing it without a browser

    wasm-bindgen --target nodejs --out-dir <dir> \
      target/wasm32-unknown-unknown/release/ash_browser.wasm
    cp examples/browser/run-node*.js <dir>/
    node --experimental-wasm-exnref <dir>/run-node.js --agents 7 threads.wasm

Same host, same module, same shared memory, same entry point: node's
`worker_threads` stand in for Workers, and only who makes the agent differs.
It is how this path is checked when there is no browser to hand, and it agrees
with one -- the same four answers, and 2.17x on the same machine that gives a
browser 3.01x.

Nothing configures it. A page has no environment to configure it through and
no count to give: the runtime asks for an agent per Haxe thread, this page
answers by starting a Worker, and it keeps answering for as long as the
browser keeps saying yes. A page that supplies no `spawn` gets threads that
take turns, and the demo says so.

The demo computes rather than allocates, deliberately. Two instances over
one memory are two mutators on one heap and ash's collector is
single-mutator: threads that allocate do not survive yet. `docs/wasm-target.md`
has the measurements and what is left to do.

## The module runs in a worker

Not on the page's thread, and not as an optimisation. A Haxe program's main
loop does not return, and a fiber that computes without blocking never yields
-- on the page's thread either of those is a frozen tab. In a worker the UI
thread is free throughout, and the worst a runaway fiber can do is stall the
worker it is in.

It buys a second thing that matters more than it looks. `memory.atomic.wait`
traps on a browser's main thread and is permitted in a worker, so blocking
synchronisation is only ever possible there.

Nothing is given up by moving: HashLink has no DOM API to lose. Output is
posted to the page because a worker's console is not the page's document.

The demo spends a second on solid arithmetic that yields to nothing, and the
page counts the frames it draws while that happens. Counted with
`requestAnimationFrame` rather than animated with CSS on purpose: a CSS
animation can run off the main thread and would keep going even if that thread
were blocked, which would make it no evidence at all. A module has to be served rather than opened from a file, because
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
