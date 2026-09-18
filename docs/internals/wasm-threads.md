# wasm: shared memory, Workers, threads

What krio built under the worker-per-fiber row, what it measured, and what
it costs to use. [wasm-fibers.md](wasm-fibers.md) is the single-agent
alternative.

## Suspension is not parallelism

| | suspend a fiber | run two Haxe threads at once |
|---|---|---|
| JSPI / wasmtime async | yes | no — single agent |
| Asyncify / ash's transform | yes | no — single agent |
| Workers over shared memory | yes | yes |

The first two rows give fiber semantics: a producer/consumer pair works
because one runs while the other is suspended. A program that expects a
worker to keep computing while the main thread blocks on `lock.wait()` needs
the third row, because with one agent the blocked one is the only one.

## What krio built

`krio-parallel` (target-neutral) and `krio-wasm` (the only crate that knows
about browsers):

- Work stealing over `Box<dyn Task + Send>`: bounded Chase–Lev deques per
  agent, injector overflow, placement via `spawn_on`.
- Parking on `memory.atomic.wait32`/`notify` — instructions, not imports, so
  no host is involved.
- A waker registry, so a task returning `Pending` leaves the run queues
  instead of spinning its agent.
- Stop-the-world safepoints and an epoch clock in shared memory.
- Agent creation as an installable hook, `krio_wasm::set_spawn`.

Chrome 152, 10 cores, 96 tasks × 24 rounds, main-thread agent plus Workers:

```
spawn() — shared injector
 agents   wall (ms)   speedup  max concurrent   steals
      1         223     1.00x               1        0
      2         119     1.87x               2        0
      4          60     3.69x               4        0
      8          42     5.31x               8        0

spawn_on(agent 0) — one agent owns everything; the rest steal
      1         219     1.00x               1        0
      2         111     1.97x               2       48
      4          57     3.84x               4       72
      8          36     6.12x               8       83
```

`max concurrent` counts agents inside `Task::step` at one instant, from an
atomic. Only the second table exercises the deque. Runnable:
`examples/browser-demo/` in krio.

## What it does not give

**Fibers.** `Fiber::new` panics on wasm. Exporting a mutable
`__stack_pointer` does not help: it names the shadow stack in linear memory,
while call frames and the operand stack live in the engine, so swapping it
moves the data half and leaves the control half behind. ash's transform is
the answer. krio's `krio_fiber::set_suspender(fn())` routes `yield_now()` to
a host suspender on targets with no native switch; ash installs its
suspender at the first yield.

**M:N.** A cluster agent is an OS thread, so mapping a fiber to a Worker
prices a fiber at a thread. `Fiber` being `!Send` makes that a compile error.

## Three traps, measured in Chrome 152

**The spawner must never block.** A dedicated Worker's children are started
through its parent's event loop, so an agent that calls `new Worker()` and
then blocks leaves that child permanently unstarted. If ash's main runs on a
Worker this is the common path:

```haxe
var t = Thread.create(() -> work());
lock.wait();          // main blocks -> t never started -> hang
```

Post a request and let the browser main thread create the Worker; it never
blocks. This is why `set_spawn` is a hook rather than a declared import: the
right way to start an agent depends on which agent is asking.

**A blocked Worker cannot service its message queue.** Inside `atomic.wait32`
an agent processes no messages, so nothing may be routed through it. Use
`BroadcastChannel` or post to the page directly.

**`--import-memory`, or every Worker gets its own memory.** Without it the
module defines its own memory and each Worker instantiates a separate one.
Nothing errors; the agents run in separate address spaces. Check
`WebAssembly.Module.imports(module).filter(i => i.kind === 'memory')` is
non-empty. `krio_wasm::cluster_support()` returns `None` when the build
lacks atomics, and the demo page refuses to start unless
`crossOriginIsolated` and `memory.buffer instanceof SharedArrayBuffer` both
hold. Refusing beats degrading: a program that silently ran single-agent
reads as a performance mystery months later.

## The mutator rendezvous

ash's compiler emits a safepoint in every loop and a word the scheduler can
tick. krio provides the barrier that consumes it:

```rust
cluster.stop_the_world(AgentId(0), || {
    // No other agent is inside Task::step while this runs.
    collector.mark_and_sweep();
});
```

krio decides when it is safe to scan; ash decides what. Agents reach the
barrier three ways: between steps the scheduler checks on their behalf, an
idle agent is woken to report in, and the loop safepoint:

```rust
if cluster.safepoint_requested() {   // one relaxed load
    cluster.enter_safepoint();
}
```

A host that never polls corrupts nothing; it cannot be collected until the
loop ends, and `stop_the_world` waits. Two bugs found by running it: the
arrived-agent count did not reset between rounds, and both barrier loops
spun without yielding, which with more runnable threads than cores held a
core against the agents being waited for. Both fixed; the barrier waits
through the backend's `Park`. Rendezvous cost at forty agents is unmeasured.

## The clock

`SystemTime::now()` traps on `wasm32-unknown-unknown`. krio-fiber reads
deadlines through an installable clock; `krio_wasm::EpochClock` reads a `u64`
in shared memory that one ticker publishes:

```rust
krio_wasm::install_fiber_clock();   // feature = "fiber-clock"
```

```js
setInterval(() => wasm.publish_epoch_ms(performance.now()), 4);
```

A deadline check is one relaxed load. Importing `performance.now()` works
and is the wrong shape: the call site is `should_yield_early()`, polled at
every checkpoint. wasm32 has `target_has_atomic = "64"` and
`align_of::<AtomicU64>()` is 8, which matters because wasm atomics trap on a
misaligned address.

## Build contract

Shared memory is a whole-program mode, every crate including `std`:

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

Of those link arguments only `--export=__wasm_init_tls` fails loudly;
`--import-memory` is the silent one. Plus cross-origin isolation on every
response the app serves:

```
Cross-Origin-Opener-Policy: same-origin
Cross-Origin-Embedder-Policy: require-corp
```

Each Worker instantiates the same module against the same memory and must
not re-run data-segment initialisation.

## How ash's threads work here

Thread identity is `__tls_base`: it differs per thread, is stable for the
thread's life, and costs one add to fetch. Before that, every thread
reported the same identity (both `thread_self_fast` and `hlp_thread_current`
fell through to a constant under a `cfg` that wasi matched), so the
collector held one mutator record, the reentrant GC lock had one owner, and
two threads bump-allocated the same TLAB.

Parallelism on 8 performance cores, 400M iterations per thread: 1 thread
0.50x, 2 threads 0.97x, 4 threads 1.90x, 8 threads 3.65x. The ~250 ms floor
is module instantiation per thread, paid about once because the
instantiations overlap.

Nothing configures the pool. A page has no environment to read a worker
count from, and a wasm worker runs a fiber body straight through, so the
pool grows: an agent per live thread, as many as the host gives. A host that
says no runs those threads on the main scheduler. A thread does not share
its WASI context, socket table or loaded libraries; each instance builds its
own, as wasmtime's wasi-threads does.

**What the linker had to learn.** A thread is another instance of the same
module over the same memory, so memory becomes `env.memory` — imported,
shared, with the 1 GiB maximum the Rust target declares. The data image
cannot be an active segment, or the second instance writes the initial data
over everything the first has reached; it is one passive segment, and
`__wasm_init_memory` races every instance on a flag word above the image.
TLS needed `.tdata` placed twice (the template a new thread copies and the
main thread's own block), a `MEMORY_ADDR_TLS_SLEB` relocation writing an
offset rather than an address, and a synthesised `__wasm_init_tls`.
`__tls_base` starts at zero, so an instance reading a thread-local before it
has a block traps rather than reading the main thread's.

**In a browser.** A page starts a Worker where wasmtime starts an OS thread:
338 ms for four threads against 1016 ms serial, 3.01x; under node's
`worker_threads`, 2.17x. Two costs a page adds: shared memory needs COOP and
COEP on every response (`examples/browser/serve.py` sets them), and agents
must be warmed before the program starts, because a Worker created from
inside a synchronous wasm call never loads — which is also the real bound on
how many threads a page can run.

## Order of work if threads ship

1. `wasm32-wasip1-threads` under wasmtime first: same shape, no COOP, no
   worker bootstrap. Threads were removed from WASI preview 2, so it is a
   probe, not a target — which is why agent creation is a hook rather than a
   WASI import.
2. Single-mutator GC root correctness before the rendezvous means anything.
3. The browser, with the three traps checked explicitly.

Related, in krio: `crates/krio-wasm/src/agent.rs`,
`crates/krio-parallel/src/safepoint.rs`, `crates/krio-wasm/src/clock.rs`,
`examples/browser-demo/README.md`.
