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

    # the threaded programs, and the other two demos the page can run
    haxe -cp . -main Threads -hl threads.hl
    ASH_WASM_FIBERS=1 ash --build ../threads.wasm \
      --target wasm32-wasip1-threads threads.hl
    haxe -cp . -main Entities -hl entities.hl
    ASH_WASM_FIBERS=1 ash --build ../entities.wasm \
      --target wasm32-wasip1-threads entities.hl

    ./examples/browser/serve.py

Then open <http://127.0.0.1:8731>. It starts a worker, which fetches the
module and runs it; there is nothing to click. `?demo=threads` and
`?demo=entities` run the other two.

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

That is what `spawn` returning false means, and why it is a return value
rather than an exception: a busy page is answering the question, not failing
at it. A throw is kept for the case where making the agent actually went
wrong, and only that reaches the console.

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

The timed part computes rather than allocates, so that what it times is the
threads and not the collector. The part after it does nothing but allocate, on
four threads at once, and checks its answers against the same work done on one
thread -- because that is the case that used to fail, and a page is where it
matters that it no longer does.

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

## Fibers, which are the other mechanism

Threads above are Workers, each with its own instance over one shared memory.
A fiber is the unrelated thing: one instance, one stack running at a time,
control moving between fibers only where one of them blocks -- a `Deque.pop`,
a lock, an explicit yield. `docs/wasm-fibers.md` calls it suspension rather
than parallelism, and it is.

Both are needed and neither replaces the other. A thread that blocks has to be
able to give its agent up, which is what the fiber transform is for; a page
that warms no agent still runs a threaded program, on fibers, taking turns --
and the threads demo says so when it happens.

ash's link-time transform rewrites the module so its frames can unwind back to
a scheduler and rewind to exactly where they stopped, and the host drives the
three globals that transform adds. `browser/fibers.rs` is the same thing
`native::install_fiber_yield` does, against `WebAssembly.Global`.

A module built without `ASH_WASM_FIBERS=1` has no such globals. Every call
answers zero and a fiber runs to completion at the point it would have
suspended -- so a worker in these demos would reach its first `pop` and stay
there. It is why every build line above sets it.

## Drawing

`?demo=entities` gives every Haxe thread a horizontal band of one framebuffer
and the entities inside it. No thread touches another's pixels, so there is no
lock and nothing to contend for, and the picture is what the threads did: a
band that stops moving is a thread that stopped.

The threads never present. They write RGBA into the guest's own memory, and
the main thread hands the host that address sixty times a second through one
import, `ash_host_canvas_present`. That is the whole graphics interface --
`std/src/canvas.rs` on the guest side, `browser/canvas.rs` on the page's.

**It has to be an `OffscreenCanvas`.** The module runs in a Worker, and by the
time it is drawing it is inside a call into wasm that will not return for the
length of the program. A `<canvas>` belongs to the document and can only be
drawn from the thread that owns it, which is exactly the thread that is not
free. `transferControlToOffscreen()` moves the drawing surface to the worker
instead, and the page keeps the element without keeping the right to draw on
it. The page transfers it in the same message that starts the program, and
`worker.js` puts it on `globalThis.ashCanvas` where the host finds it.

Pixels are copied on the way out rather than passed. `ImageData` refuses a
view onto a `SharedArrayBuffer`, and a threaded module's memory is one.

Where the host has no canvas -- wasmtime, node, a page that transferred none
-- `present` answers false and the program says frames were drawn and not
shown. It is the same program either way; only the last line differs.

Where an entity is is a function of the clock rather than of frames drawn, so
the picture is the same picture whatever rate a band manages.

**Every band holds itself to sixty frames a second, and that is not a
politeness.** Uncapped, a band draws around three thousand frames a second and
allocates some twenty megabytes a second doing it, while nobody sees more than
sixty: the frames past the sixtieth are invisible and the garbage behind them
is not. Four threads of that bury the collector -- measured, a 512 MB heap
full after fifteen collections, world stops abandoned after two seconds each
-- and every one of those stops pauses the thread that presents. The picture
arrives in lurches and then the program dies. Drawing what is shown and no
more is what makes it real time; capped, the same run holds 60/s on all four
bands for its whole length.

A third argument takes the cap off (`entities.wasm 4 15 100000`). It is only
worth doing to put the collector under a load a page must never give it.

`?demo=entities&threads=8` asks for eight bands instead of four. The page
warms one agent per core and no more, so asking for more threads than that is
the way to see what a thread with no agent does: it runs on the main
scheduler, taking turns with another band, and the page says which.

## Not yet

**Loading a native library.** `ash_host_dlopen` answers "no such library", so
a primitive from one raises when it is reached. The steps are the native
host's loader (`docs/wasm-hdlls.md`) against `WebAssembly.instantiate`.
