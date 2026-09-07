# Collections that free nothing

**Status: cause found, not fixed.** The mechanism below is supported by a
control that removes the failure entirely. No production fix has been written.

## What happens

Collections succeed and reclaim nothing:

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

## Known defects that are not the retention bug

- **A worker's fatal exit does not stop the program.** `out_of_memory` calls
  `std::process::exit`; the native host's spawned worker catches the error,
  prints it, and returns, while `Program::run` awaits only the main instance.
  The dead worker's GC registration survives. The node agent does the same and
  the browser path needs the same audit. Simply deleting a timed-out mutator is
  not the fix — the thread may have died holding runtime locks.
- **Late-safepoint times print as `18446744073709.x ms`.** That is unsigned
  underflow, not a real delay: each native worker gets a fresh WASI context and
  wasmtime's default monotonic clock starts at *its* construction, while the
  guest shares `GC_EPOCH` across workers. Needs a common time origin and
  checked duration arithmetic. The browser has the same shape with per-Worker
  `performance.now()`.

## Proposed direction

Separate object tracing from line reclamation: keep compact start/size
metadata for small allocations (TLAB included), resolve conservative
candidates — interior pointers included — to the containing object, and scan
only that object's bytes. Keep line bits for sweep and recycling, but an
already-marked line must not suppress discovering another live object on it.
Reset metadata on line recycling and block reuse, and validate span
containment rather than promoting an earlier unrelated span. Use type and
pointer maps where they exist, especially to exclude numeric fields like the
double above; conservative roots stay where exact maps do not.

Worth testing: a packed-neighbour chain with only its first allocation rooted;
two live objects on one marked line; interior pointers; recycled spans and TLAB
reuse; a numeric field whose bits land in the heap; allocating fibers; and a
worker `proc_exit` while the main thread waits.
