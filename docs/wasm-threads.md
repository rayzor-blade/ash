# Shared memory and Workers on wasm

Companion to [`wasm-target.md`](wasm-target.md), which decides the route. This
records what krio has built underneath the worker-per-fiber row, what it
measured, and what it costs to use.

Threads should still stay out of a first release. This is the map for when they
come back.

## Suspension is not parallelism

| | suspend a fiber | run two Haxe threads at once |
|---|---|---|
| JSPI / wasmtime async | yes | no — single agent |
| Asyncify | yes | no — single agent |
| Workers over shared memory | yes | yes |

JSPI suspends and resumes one call stack on one agent, which is what a
cooperative fiber needs and not what a `sys.thread` program expecting
simultaneous progress gets. The rows are alternatives for fiber semantics and
*not* for parallelism.

That decides `Deque`, `Mutex` and `Lock`. A Haxe producer/consumer pair works
under JSPI — one runs while the other is suspended. A program that expects a
worker to keep computing while the main thread blocks on `lock.wait()` does
not, because there is one agent and it is the one that blocked.

**So: JSPI first. Workers when a program needs threads that actually run at the
same time, or when JSPI is absent.**

## What is built

`krio-parallel` (target-neutral) and `krio-wasm` (the only crate that knows
about browsers), both on krio's `main`:

- Work stealing over `Box<dyn Task + Send>` — bounded Chase–Lev deques per
  agent, injector overflow, placement via `spawn_on`.
- Parking on `memory.atomic.wait32`/`notify`. These are instructions, not
  imports, so no host is involved.
- A waker registry, so a task returning `Pending` leaves the run queues instead
  of spinning its agent.
- Stop-the-world safepoints, and an epoch clock in shared memory.
- Agent creation as an installable hook, `krio_wasm::set_spawn`.

Chrome 152, 10 cores, 96 tasks × 24 rounds, one main-thread agent plus Workers:

```
spawn() — shared injector, every agent pulls its own
 agents   wall (ms)   speedup  max concurrent   steals
      1         223     1.00x               1        0
      2         119     1.87x               2        0
      4          60     3.69x               4        0
      8          42     5.31x               8        0

spawn_on(agent 0) — one agent owns it all; the rest must steal
      1         219     1.00x               1        0
      2         111     1.97x               2       48
      4          57     3.84x               4       72
      8          36     6.12x               8       83
```

`max concurrent` counts agents inside `Task::step` at one instant, from an
atomic rather than inferred from a clock — it is the overlap evidence. The two
tables are not interchangeable: the first balances through the injector and
needs no steals, so only the second exercises the deque.

Runnable: `examples/browser-demo/` in krio.

## What it does not give you

**Fibers.** `Fiber::new` panics on wasm and will keep panicking. The module
exports a mutable `__stack_pointer` and that does not help: it names the shadow
stack in linear memory, while call frames and the operand stack live inside the
engine, so swapping it moves the data half and leaves the control half behind.
ash's own transform is the answer — see [`wasm-fibers.md`](wasm-fibers.md).

krio's complementary piece is small: `krio_fiber::set_suspender(fn())` routes
`yield_now()` to a host suspender on targets with no native switch, so library
code written against krio's free functions compiles and behaves on wasm. With
nothing installed it panics rather than silently no-opping. ash installs its
suspender at the first yield, so library code reaching for
`krio_fiber::yield_now` suspends the same way the scheduler does.

**M:N.** A cluster agent is an OS thread, so mapping a fiber to a Worker prices
a fiber at a thread while ash's scheduler is M:N. Workers are where ash's
worker-affine endpoints live, not where its fibers live. `Fiber` being `!Send`
makes that a compile error rather than a convention.

## Three traps, measured in Chrome 152

Two fail silently, and all three cost real time to find.

### The spawner must never block

A dedicated Worker's children are started through its **parent's event loop**,
so an agent that calls `new Worker()` and then blocks leaves that child
permanently unstarted:

```
parent spawns child, then blocks    ->  child never runs
parent spawns child, stays awake    ->  child runs
parent asks the page, then blocks   ->  child runs
```

If ash's main runs on a Worker this is the common path, not an edge case:

```haxe
var t = Thread.create(() -> work());
lock.wait();          // ash main blocks -> t never started -> hang
```

Post a request and let the browser main thread call `new Worker()`; it never
blocks, so it can always oblige. This is why `set_spawn` is a hook rather than
a declared import — the right way to start an agent depends on *which agent is
asking*, and only the host knows who is safe to ask.

### A blocked Worker cannot service its message queue

Same cause, different symptom. Inside `atomic.wait32` an agent processes no
messages, so nothing may be routed through it — a child reporting readiness to
a parent that has started running hangs.

Use `BroadcastChannel`, or post to the page directly. Never build a message
topology whose interior nodes are agents that block.

### `--import-memory`, or every Worker gets its own memory

Without it the module *defines* its own memory and each Worker instantiates a
separate one. Nothing errors. The agents run in separate address spaces and any
parallelism measured is measuring nothing.

```js
WebAssembly.Module.imports(module).filter(i => i.kind === 'memory')
// must be non-empty
```

`krio_wasm::cluster_support()` returns `None` when the build lacks atomics, and
the demo page refuses to start unless `crossOriginIsolated` and
`memory.buffer instanceof SharedArrayBuffer` both hold. Prefer refusing to
degrading: a program that silently ran single-agent reads as a performance
mystery months later.

## The mutator rendezvous

ash's compiler already emits a safepoint in every loop and a word the scheduler
can tick. krio provides the barrier that consumes it:

```rust
cluster.stop_the_world(AgentId(0), || {
    // No other agent is inside Task::step while this runs.
    collector.mark_and_sweep();
});
```

krio stops the world and knows nothing about roots. That division is the useful
one: krio guarantees *when* it is safe to scan, ash decides *what*.

Agents reach the barrier three ways. Two are free — between steps the scheduler
checks on their behalf, and an idle agent is woken to report in. The third is
the loop safepoint ash already emits:

```rust
if cluster.safepoint_requested() {   // one relaxed load
    cluster.enter_safepoint();
}
```

A host that never polls corrupts nothing; it merely cannot be collected until
that loop ends, and `stop_the_world` waits. A pause is diagnosable, a torn heap
is not.

Two bugs, both found by running it rather than reading it. The
arrived-agent count did not reset between rounds, so a second stop read a
leftover count and handed the closure a heap three agents were still writing.
And both barrier loops spun without yielding, which with more runnable threads
than cores holds a core against the very agents being waited for — an apparent
deadlock about one run in thirty. Both fixed; the barrier waits through the
backend's `Park`.

Untested: rendezvous cost at scale. Cheap with four agents, unproven with forty.

## The clock

`SystemTime::now()` **traps** on `wasm32-unknown-unknown`. krio-fiber reads
deadlines through an installable clock, and krio-preempt reads
`krio_fiber::now_ms()` rather than keeping its own, so a slice and the deadline
it sets share an origin.

`krio_wasm::EpochClock` reads a `u64` in shared memory; one ticker publishes:

```rust
krio_wasm::install_fiber_clock();   // feature = "fiber-clock"
```

```js
setInterval(() => wasm.publish_epoch_ms(performance.now()), 4);
```

A deadline check is then one relaxed load. Importing `performance.now()` and
calling it also works and is the wrong shape: the call site is
`should_yield_early()`, polled at every checkpoint, and a JS boundary crossing
per poll costs more than the work being scheduled.

wasm32 reports `target_has_atomic = "64"`, so the counter is one
`i64.atomic.*` with no torn halves, and `align_of::<AtomicU64>()` is 8 — which
matters because wasm's atomic instructions *trap* on a misaligned address
rather than degrading.

## Build contract

Shared memory is a whole-program mode, not a cargo feature — every crate
including `std`:

```sh
RUSTFLAGS='-Ctarget-feature=+atomics,+bulk-memory,+mutable-globals \
  -Clink-arg=--import-memory \
  -Clink-arg=--shared-memory \
  -Clink-arg=--max-memory=536870912 \
  -Clink-arg=--export=__wasm_init_tls \
  -Clink-arg=--export=__tls_size \
  -Clink-arg=--export=__tls_align \
  -Clink-arg=--export=__tls_base' \
  cargo build --release --target wasm32-unknown-unknown \
  -Zbuild-std=std,panic_abort
```

Nightly, for `-Zbuild-std`. Of those link arguments only
`--export=__wasm_init_tls` fails loudly — wasm-bindgen refuses to generate glue
without it. `--import-memory` is the silent one.

Plus cross-origin isolation on **every** response the app serves, not just the
wasm:

```
Cross-Origin-Opener-Policy: same-origin
Cross-Origin-Embedder-Policy: require-corp
```

Each Worker instantiates the same module against the same memory, and must not
re-run data-segment initialisation — that resets every shared static, the epoch
clock included.

## If threads ship anyway

The order that de-risks fastest:

1. **`wasm32-wasip1-threads` under wasmtime, not a browser.** Same shape, no
   COOP, no worker bootstrap, no driver; krio's suite runs there. Threads were
   removed from WASI Preview 2, so treat it as a probe, never a production
   target — which is why agent creation is a hook rather than a WASI import.
2. **Single-mutator GC correctness first.** The rendezvous is worth nothing if
   root discovery is still conservative-scanning a shadow stack that cannot see
   wasm locals.
3. **Then the browser**, with the three traps above checked explicitly.

## How ash's threads work here

**Every thread said it was the same thread**, and that — not the collector —
was what stopped allocating threads from working. `thread_self_fast` and
`hlp_thread_current` both fell through to a constant under a
`cfg(not(any(unix, windows)))` that wasi matches. So the collector's world held
one mutator record, `stop_mutator_world` found nobody to stop and marked a heap
another thread was writing; the reentrant GC lock had one owner; the TLAB map
had one entry, so two threads bump-allocated the same buffer. The `stop=0.00ms`
this document once reported as a working rendezvous was the symptom.

`__tls_base` differs per thread, so the address of any thread-local is distinct,
stable for the thread's life, and one add to fetch. That is the identity now.

| threads | before | after |
|---|---|---|
| 2 | hung, trapped or passed, differently each run | 10 of 10 correct |
| 4 | hung or trapped every time | correct, 0.21s |
| 8 | never tried | correct, 0.58s |

Parallelism on 8 performance cores, 400M iterations per thread: 1 thread 0.50x,
2 threads 0.97x, 4 threads 1.90x, 8 threads 3.65x. The flat ~250ms floor is
instantiating the module for a thread, paid about once because the
instantiations overlap.

**Nothing configures the pool.** A page has no environment to read a worker
count from, and a wasm worker runs a fiber body straight through, so a pool of
N could only run N Haxe threads. The pool grows instead: an agent per live
thread, as many as the host will give. A host that says no runs those threads
on the main scheduler.

A thread does not share its WASI context, socket table or loaded libraries:
preview 1 cannot hand one descriptor table to two instances, so each thread
builds its own, as wasmtime's own wasi-threads does. A file opened on one
thread is not open on another.

### What the linker had to learn

A thread is another instance of the same module over the same memory, so the
memory becomes `env.memory` — imported, shared, carrying the 1GiB maximum the
Rust target declares. The data image cannot then be an active segment: the
second instance would write the program's initial data back over everything the
first had reached, with nothing trapping. It becomes one passive segment, and
`__wasm_init_memory` races every instance on a flag word above the image — the
winner copies the data in and runs constructors, the losers wait on that word.

Thread-local storage took `.tdata` placed twice, once as the template a new
thread copies and once as the main thread's own block, a
`MEMORY_ADDR_TLS_SLEB` relocation writing an offset rather than an address, and
a synthesised `__wasm_init_tls`. `__tls_base` starts at zero, so an instance
reading a thread-local before it has a block traps rather than quietly reading
the main thread's.

### In a browser

A page starts a Worker where wasmtime starts an OS thread. Measured: 338ms for
four threads against 1016ms serial, 3.01x. Under node's `worker_threads`,
2.17x.

Two things a page costs that a host does not:

- **Shared memory needs cross-origin isolation** — COOP and COEP on every
  response. `examples/browser/serve.py` exists to say so; without them the
  memory constructor throws in a way that reads as the module being broken.
- **Agents must be warmed before the program starts.** Creating a Worker needs
  the creating agent to return to its event loop, and the agent asking for a
  thread is inside a synchronous call into wasm that will not return until the
  thread answers. A Worker created at that moment never loads. This is also the
  real bound on how many threads a page can run.

## Related

In krio: `crates/krio-wasm/src/agent.rs` (spawn contract),
`crates/krio-parallel/src/safepoint.rs` (the barrier),
`crates/krio-wasm/src/clock.rs`, `examples/browser-demo/README.md`.
