# Collections that free nothing

**Status: fixed; regression coverage added.** Object-granular tracing removes
the packed-neighbour retention chain without rounding allocations to whole
lines. Both wasm hosts propagate fatal worker outcomes and wake blocked atomic
waits. The original mechanism and diagnostic evidence are retained below.

## What happens

Before the fix, collections succeeded and reclaimed nothing:

```
#26 origin=exhaustion pause=512.10ms freed=0 blocks live=16384 blocks (512.0MB) free=0 blocks
#27 origin=exhaustion pause=478.95ms freed=0 blocks live=16384 blocks (512.0MB) free=0 blocks
#28 origin=exhaustion pause=507.94ms freed=0 blocks live=16384 blocks (512.0MB) free=0 blocks
```

The world stops in well under a millisecond; marking takes half a second and
frees zero blocks, with a live set of a few megabytes. The heap reaches its
cap and a thread raises out-of-memory.

Everything else follows from that, and the order matters because it is easy to
get backwards:

    conservative false retention
      -> heap exhaustion
      -> a worker raises OOM and calls process::exit
      -> the host lets the other instances keep running
      -> that worker's GC mutator stays registered
      -> world stops abandoned waiting for a thread that no longer exists
      -> main waits forever on a Deque.pop; 0% CPU

**A stale mutator is indistinguishable from a live one that refuses to stop.**
It reports `polls=0` (it runs no safepoint code, because it runs nothing) and
`at=running` (the last site it recorded before dying). Hours went into
characterising that thread as "running, not allocating, never polling" before a
thread listing showed `wasi-thread-2` simply absent. Check for a prior
`out of memory` or `proc_exit` before treating a straggler as live.

## Why nothing is freed

Two mechanisms compound.

**Line sharing.** `gc_alloc` packs allocations at 16-byte alignment into
128-byte mark lines, and `scan_line_shared` traces all 128 bytes of a marked
line — including unrelated allocations that happen to share it. Their outgoing
pointers mark more objects and more lines, so the cost is not "a few retained
neighbours": it is an arbitrarily long chain. Instrumented, one chain went from
depth 1 to depth 2,602 and on past 227,000 edges. In this workload the repeat
unit is an 80-byte allocation (a `varray` of 13 pointers) followed by the next
Haxe `Array` object at line offset 96, whose backing pointer at offset 104
reaches the next line — so retaining one trail's buffer traces the *next*
trail, with no real reference between them.

**False roots.** The wasm32 conservative scan reads scalar halves as pointers.
A concrete seed, from the demo's own source:

```
Entities.TRAIL_STEP = 0.035
  -> IEEE-754 words 0x1eb851ec, 0x3fa1eb85
  -> low word 0x1eb851ec lies inside the reserved heap 0x160000..0x20160000
  -> traced to depth 304,794
```

A `Float` constant is a heap pointer as far as the marker is concerned. It is
not the only seed; stack words qualify too.

**The control.** Rounding allocation requests up to whole lines, so no two
allocations share one, makes all three runs complete: 2,164 collections with a
maximum retention of 52 blocks (1.6 MB) against 512 MB. That is not a proposed
fix — it inflates allocation traffic — but it isolates the mechanism.

## Reproducing it

```
scripts/gc_straggler_repro.py --runs 6
```

Four Haxe threads drawing into one framebuffer for twenty seconds
(`entities.wasm 4 20 400`). Roughly a third of runs at 400 fps; more at
`--fps 100000`, which also allocates hard enough that the heap fills faster.
`--host node` runs the browser host instead, `--sample` captures thread states
on macOS.

Judge a run by **`freed=`**, not by how it died:

| verdict | meaning |
| --- | --- |
| `BUG` | a collection freed nothing, or the run ran out of memory |
| `hung` | no barren collection recorded; read the log |
| `clean` | finished, with what it peaked at |

An earlier version of this script reported "out of memory with no abandoned
stop" as an *honest full heap* and told you not to report it. That was
backwards. Allocation throughput is not live memory; a collection that frees
zero blocks with a 2 MB live set is the defect itself, caught one step before
the symptom that used to be treated as the signal.

Likewise `ASH_GC_HEAP_MB=64` fails in eight seconds every time and looks like a
sharper reproducer. It is the same bug arriving sooner, not a different one —
but it gives the marker less room, so it is a poor place to study retention.

## Reading a failing log

- `grep 'origin='` — the collections. `freed=0 blocks` beside a large `live=`
  is the defect.
- `grep 'mutators:'` — only when stops were abandoned. The table gives every
  thread's role, whether it parked, its safepoint count since the stop
  (`polls=`), and the last waiting place it entered (`at=`). Check first
  whether a worker had already exited: a dead thread's record looks exactly
  like a live thread that will not stop.

## Where safepoints are, for reference

`ReentrantGcLock::acquire` (so every allocation slow path); `fiber::park`'s
loops and `hlp_fiber_poll`; `gc_set_blocking(true)`; `worker_main`'s loop head.
The TLAB bump path polls nothing, and AOT wasm emits no safepoint of its own.

## Refuted — do not re-derive

These were tried against the *symptom* before the cause was known. Each is a
real hole and each is now closed, and **none changed the failure rate**:

1. `worker_main` announced a blocking section while holding the scheduler
   command lock — a lock held across a safepoint park. 3/4 hung before, 3/4
   after.
2. A safepoint poll added to the TLAB bump path. 5/5 hung; reverted as a
   hot-path load for nothing. `ASH_GC_TLAB=0` does pass 4/4 against 1/4, but it
   perturbs timing rather than removing the fault.
3. `worker_main` spinning on `if schedule_step() { continue; }` without a poll.
   5/5 hung.

Also ruled out: a lost wakeup on the GC lock's condvar (`wake_for_world_stop`
is an unconditional `notify_all` under the lock); duplicate thread ids (all
distinct); and collection *frequency* (`ASH_GC_TRIGGER_MB=8` gives 5/5 clean).

## Accompanying host and diagnostic fixes

- **Fatal worker outcomes now end the run.** Native stores share an exit/trap
  outcome; the first one wins, preserving `proc_exit`'s status or the original
  trap. Epoch interruption stops guest computation. Both hosts interpose
  `memory.atomic.wait32/64` at load time, register the exact wait address, and
  notify registered waits until they drain, closing the register/wait race.
  Normal waits keep their original timeout, mismatch and notification
  semantics. No mutex words are overwritten and no stale mutator is removed
  to let an unsafe program continue.
- **Browser agents share termination state directly.** The `spawn` request
  now carries a `control` SharedArrayBuffer; the Worker passes it as the last
  argument to `run_thread`. Waiting agents and host calls observe cancellation
  without servicing their event queues. The example owner terminates its
  agents when the run finishes. Unlike Wasmtime, JS engines offer no epoch
  interrupt: a main Worker in a pure wasm loop without host calls still needs
  its owner to terminate it. Native synchronous host I/O is not universally
  preemptible either; this fix covers guest computation and atomic waits.
- **Cross-thread clocks now share an origin.** Native WASI contexts use one
  run-wide `Instant`; browser contexts use `performance.timeOrigin +
  performance.now()`. Late-safepoint arithmetic also saturates rather than
  wrapping into `18446744073709.x ms`.

## GC implementation

`std/src/gc.rs` separates object tracing from line reclamation:

- One atomic metadata byte per 16-byte allocation quantum records small
  starts/sizes and an independent object mark. Spans use the existing line
  size table. The table costs 6.25% of the heap reservation, is allocated
  zeroed, and is touched as blocks are used; allocation packing is unchanged.
- Both locked allocation and TLAB bumps publish object boundaries. Candidate
  pointers, including interior pointers, resolve to their containing
  allocation. Free space and a preceding span that does not contain the
  candidate are rejected.
- The serial and parallel marker queues contain allocations, not lines, and
  trace only those allocations' bytes. Two live objects on one line are
  claimed independently; a dead neighbour does not become reachable.
- Line marks still govern sweep/recycling. Object claims reset each cycle;
  boundaries reset when blocks or recycled lines are reused. Dedicated span
  padding is zeroed too. The sweep audit inspects traced objects, not dead
  neighbours sharing their lines.

Tracing is still conservative **within** allocations and root ranges. Type/
pointer maps and pointer-free allocation kinds remain follow-up precision
work: a scalar may still retain the real object it happens to point into and
that object's reference graph, but cannot acquire unrelated neighbours'
graphs merely by sharing their mark line.

## Validation and rebuilding

With the packed allocator, 4 allocating fibers for 20 seconds, and the
corrected `freed=` classifier:

| Host / rate | Runs | Collections | Peak retained heap, rounded |
| --- | ---: | ---: | ---: |
| Wasmtime / 400 fps | 3 clean | 111 / 115 / 115 | 1 MB |
| Wasmtime / 100000 fps | 3 clean | 200 / 235 / 258 | 1 MB |
| Browser host under Node / 100000 fps | 2 clean | 378 / 416 | 1 MB |

Logs are under `target/gc-straggler-fixed`,
`target/gc-straggler-fixed-uncapped` and
`target/gc-straggler-browser-fixed`. Running the **old leaking module** with
the new hosts and a 64 MB cap still produces OOM, as expected, but now exits
promptly in both hosts instead of leaving main in `Deque.pop` (logs:
`gc-straggler-native-exit` and `gc-straggler-browser-exit`).

After rebuilding the default demo and host bindings, another three Wasmtime
runs (155 / 154 / 218 collections) and two Node runs (242 / 247) at 100000 fps
were clean, also peaking at 1 MB. These logs are in
`target/gc-straggler-final-native` and `target/gc-straggler-final-browser`.

Regression tests cover the packed-neighbour chain, two roots on one line,
interior pointers and span bounds, recycled metadata, real TLAB bumps, parallel
claims, worker exit/trap during waits, and unchanged wait semantics. Native
tests also cover compute-loop interruption and stores starting after exit.
The native AOT smoke corpus is checked against JIT output as well.

Rebuild **both** the linked guest runtime and the host; rebuilding only the
host cannot remove retention in an already-linked module:

```sh
scripts/build_wasm_runtime.py --target wasm32-wasip1-threads
ASH_WASM_FIBERS=1 target/release/ash --build examples/browser/entities.wasm \
  --target wasm32-wasip1-threads examples/browser/demo/entities.hl
cargo build --release -p ash_wasm_runtime
cargo build --release -p ash_browser --target wasm32-unknown-unknown
wasm-bindgen --target web --out-dir examples/browser \
  target/wasm32-unknown-unknown/release/ash_browser.wasm
scripts/gc_straggler_repro.py --runs 3 --fps 100000
```

The Node/browser regression is opt-in because it needs generated bindings:

```sh
wasm-bindgen --target nodejs --out-dir target/gc-straggler-browser-host \
  target/wasm32-unknown-unknown/release/ash_browser.wasm
cp examples/browser/run-node*.js target/gc-straggler-browser-host/
ASH_BROWSER_HOST_DIR="$PWD/target/gc-straggler-browser-host" \
  cargo test -p ash_wasm_runtime --release --test thread_exit -- --ignored
```
