# Shared memory and Workers on wasm

Companion to [`wasm-target.md`](wasm-target.md) §Phase 6. That document
decides the *route*; this one records what is now built and measured
underneath the second row of its table, and what it costs to use.

Nothing here argues with the deferral. Threads should stay out of the
first release for exactly the reasons Phase 6 gives. This is the map for
when they come back.

---

## 1. What this is and is not an alternative to

Phase 6's table treats engine suspension and worker-per-fiber as three
ways to implement one thing. That is right for *suspension*, and it
hides a distinction worth making explicit:

| | suspend a fiber | run two Haxe threads at once |
|---|---|---|
| JSPI / `wasmtime` async | ✅ | ❌ single agent |
| Asyncify | ✅ | ❌ single agent |
| Workers over shared memory | ✅ | ✅ |

JSPI does not make anything parallel. It suspends and resumes one
call stack on one agent, which is precisely what a cooperative fiber
needs and precisely what a `sys.thread` program expecting two threads to
make progress simultaneously does not get. So the rows are alternatives
for fiber semantics and *not* alternatives for parallelism.

That matters for `Deque`, `Mutex` and `Lock`. A Haxe producer/consumer
pair works under JSPI — one runs while the other is suspended. A Haxe
program that expects a worker thread to keep computing while the main
thread blocks on `lock.wait()` does not, because there is only one agent
and it is the one that blocked.

**So: JSPI first, as Phase 6 says. Workers when a program needs threads
that actually run at the same time, or when JSPI is absent.**

---

## 2. What is built

`krio-parallel` (target-neutral) and `krio-wasm` (the only crate that
knows about browsers), both on `main` in krio.

- Work stealing over `Box<dyn Task + Send>` — bounded Chase–Lev deques
  per agent, injector overflow, placement via `spawn_on`.
- Parking on `memory.atomic.wait32` / `notify`. No host involvement:
  these are instructions, not imports.
- A waker registry, so a task that returns `Pending` leaves the run
  queues instead of spinning its agent.
- Stop-the-world safepoints (§5).
- An epoch clock in shared memory (§6).
- Agent creation as an installable hook, `krio_wasm::set_spawn` (§4.1).

Measured in Chrome 152, 10 cores, 96 tasks × 24 rounds, one main-thread
agent plus Workers:

```
spawn() — shared injector, every agent pulls its own
 agents   wall (ms)   speedup  max concurrent   steals   steps
      1         223     1.00x               1        0    2400 ✓
      2         119     1.87x               2        0    2400 ✓
      4          60     3.69x               4        0    2400 ✓
      8          42     5.31x               8        0    2400 ✓

spawn_on(agent 0) — one agent owns it all; the rest must steal
      1         219     1.00x               1        0    2400 ✓
      2         111     1.97x               2       48    2400 ✓
      4          57     3.84x               4       72    2400 ✓
      8          36     6.12x               8       83    2400 ✓
```

`max concurrent` counts agents inside `Task::step` at the same instant,
from an atomic rather than inferred from a clock — it is the overlap
evidence. The two tables are not interchangeable: the first balances
through the injector and needs no steals at all, so only the second
exercises the deque.

Runnable: `examples/browser-demo/` in krio.

---

## 3. What it does not give you

**Fibers.** `Fiber::new` still panics on wasm and will keep panicking.
The module exports a mutable `__stack_pointer` and it does not help:
that names the shadow stack in linear memory, while call frames and the
operand stack live inside the engine. Swapping it moves the data half
and leaves the control half behind. Ash's own backend remains the
answer, and `ash_host_fiber_yield` remains the right shape. It now lives
in `crates/ash_wasm_runtime/src/guest.rs`, linked into the program
rather than sitting in `ash_std`.

What krio added for that path is small and complementary:
`krio_fiber::set_suspender(fn())` routes `yield_now()` to a host
suspender on targets with no native switch, so library code written
against krio's free functions — `yield_now`, `should_yield_early`,
`is_cancelled` — compiles and behaves on wasm. With nothing installed it
panics rather than silently no-opping. It is the same decision Ash's
backend already made, offered to every host instead of each reinventing
it — and Ash now installs its suspender into krio at the first yield, so
library code reaching for `krio_fiber::yield_now` suspends the same way
the scheduler does.

**M:N.** A cluster agent is an OS thread. Phase 6's objection stands
unchanged: mapping a fiber to a Worker prices a fiber at a thread, and
Ash's scheduler is M:N. Workers are where Ash's *worker-affine
endpoints* live, not where its fibers live — the existing
`SchedulerEndpoint` shape, with balancing done before installation
because krio stacks are `!Send`, is the right one on wasm too. `Fiber`
being `!Send` makes that a compile error rather than a convention:

```
error[E0277]: `*mut c_void` cannot be sent between threads safely
```

---

## 4. Three traps, measured in Chrome 152

Two fail silently. All three cost real time to find.

### 4.1 The spawner must never block

A dedicated Worker's children are started through its **parent's event
loop**. An agent that calls `new Worker()` and then blocks leaves that
child permanently unstarted:

```
parent spawns child, then blocks    ->  child never runs
parent spawns child, stays awake    ->  child runs
parent asks the page, then blocks   ->  child runs
```

If Ash's main runs on a Worker, this is the common path rather than an
edge case:

```haxe
var t = Thread.create(() -> work());
lock.wait();          // Ash main blocks -> t never started -> hang
```

**Do:** post a request and let the browser main thread call
`new Worker()`. It never blocks, so it can always oblige. This is why
`set_spawn` is a hook and not a declared import — the right way to start
an agent depends on *which agent is asking*, and only the host knows who
is safe to ask.

### 4.2 A blocked Worker cannot service its message queue

Same root cause, different symptom. Inside `atomic.wait32` an agent
processes no messages at all, so **nothing may be routed through it**. A
child reporting readiness to its parent hangs if that parent has started
running.

**Do:** `BroadcastChannel`, or post to the page directly. Never build a
message topology whose interior nodes are agents that block.

### 4.3 `--import-memory`, or every Worker gets its own memory

Without it the module *defines* its own memory and each Worker
instantiates a separate one. Nothing errors. The agents run on separate
address spaces and any parallelism measured is measuring nothing.

**Verify, do not assume:**

```js
WebAssembly.Module.imports(module).filter(i => i.kind === 'memory')
// must be non-empty
```

`krio_wasm::cluster_support()` returns `None` when the build lacks
atomics, and the demo page refuses to start unless `crossOriginIsolated`
and `memory.buffer instanceof SharedArrayBuffer` both hold. Prefer
refusing over degrading: a program that silently ran single-agent reads
as a performance mystery months later.

---

## 5. The mutator rendezvous

Phase 6 lists this as part of the threads work, and it is the piece that
does not follow from shared memory alone.

Ash's compiler already emits what it needs: *"a safe point in every loop
and a word the scheduler can tick."* krio now provides the other half —
the barrier that consumes it:

```rust
cluster.stop_the_world(AgentId(0), || {
    // No other agent is inside Task::step while this runs.
    collector.mark_and_sweep();
});
```

krio stops the world and knows nothing about roots. Given
`wasm-target.md` §Phase 4 — that root discovery, not allocation, is the
hard problem, and that shadow-root frames are the answer — the division
is the useful one: krio guarantees *when* it is safe to scan, Ash decides
*what* to scan.

Agents reach the barrier three ways. Two are free: between steps the
scheduler checks on their behalf, and an idle agent is woken to report
in. The third is the loop safepoint Ash already emits — the poll is:

```rust
if cluster.safepoint_requested() {   // one relaxed load
    cluster.enter_safepoint();
}
```

A host that never polls corrupts nothing; it merely cannot be collected
until that loop ends, and `stop_the_world` waits. A pause is
diagnosable, a torn heap is not.

Two bugs worth knowing about because they are the shape this code fails
in, both found by running it rather than reading it. The arrived-agent
count did not reset between rounds, so a second stop starting
immediately read a leftover count and handed the closure a heap three
agents were still writing to. And both barrier loops spun without
yielding, which with more runnable threads than cores holds a core
against the very agents being waited for — an apparent deadlock about
one run in thirty. Both fixed; the barrier now waits through the
backend's `Park`.

**Untested:** rendezvous cost at scale. Cheap with four agents, unproven
with forty.

---

## 6. The clock

`SystemTime::now()` **traps** on `wasm32-unknown-unknown` — *time not
implemented on this platform*. krio-fiber now reads deadlines through an
installable clock, and krio-preempt reads `krio_fiber::now_ms()` rather
than keeping its own, so a slice and the deadline it sets always share
an origin.

This is the same mechanism as Ash's *"word the scheduler can tick"*.
`krio_wasm::EpochClock` reads a `u64` in shared memory; one ticker
publishes into it:

```rust
krio_wasm::install_fiber_clock();   // feature = "fiber-clock"
```

```js
setInterval(() => wasm.publish_epoch_ms(performance.now()), 4);
```

A deadline check is then one relaxed load. Importing `performance.now()`
and calling it also works and is the wrong shape: the call site is
`should_yield_early()`, polled at every checkpoint, and a JS boundary
crossing per poll costs more than the work being scheduled.

Worth confirming for the 64-bit counter on a 32-bit target: wasm32
reports `target_has_atomic = "64"`, so this is one `i64.atomic.*` with
no torn halves, and `align_of::<AtomicU64>()` is 8 — which matters
because wasm's atomic instructions *trap* on a misaligned address rather
than degrading.

---

## 7. Build contract

Shared memory is a whole-program mode, not a cargo feature — every
crate including `std`:

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
`--export=__wasm_init_tls` fails loudly — wasm-bindgen refuses to
generate glue without it. `--import-memory` is the silent one (§4.3).

Plus cross-origin isolation on every response:

```
Cross-Origin-Opener-Policy: same-origin
Cross-Origin-Embedder-Policy: require-corp
```

Each Worker instantiates the **same module** against the **same
memory**, and must **not** re-run data-segment initialisation — that
resets every shared static, the epoch clock included.

This is the deployment burden Phase 6 names, and it is real: it applies
to every response the app serves, not just the wasm.

---

## 8. If it goes in the first release anyway

It should not. But if a program needs two Haxe threads running at once
badly enough to pay for it, the order that de-risks fastest:

1. **`wasm32-wasip1-threads` under wasmtime first, not a browser.** Same
   shape — one instance per agent over one shared memory — and no COOP,
   no worker bootstrap, no driver. krio's own suite runs there.
   Caveat: threads were removed from WASI Preview 2, so treat it as a
   probe, never a production target. That is why agent creation is a
   hook rather than a WASI import: when the engine goes, the harness is
   what changes.
2. **Single-mutator GC correctness before any of this**, per Phase 4.
   The rendezvous is worth nothing if root discovery is still
   conservative-scanning a shadow stack that does not see wasm locals.
3. **Then the browser**, with §4's three traps checked explicitly rather
   than assumed.

## Related

In krio: `crates/krio-wasm/src/agent.rs` (spawn contract and
invariants), `crates/krio-parallel/src/safepoint.rs` (the barrier),
`crates/krio-wasm/src/clock.rs` (epoch clock),
`examples/browser-demo/README.md` (build, run, measure).
