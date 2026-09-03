# hl2wasm — a WebAssembly target from optimized AIR

**Goal.** Turn HL bytecode into a `.wasm` module through the AIR and native
AOT pipeline, so Heaps and other Haxe frameworks have a WebAssembly target to
build against.

**First target.** `wasm32-wasip1`, as a command module runnable by Wasmtime.
A browser runs the same core module through a WASI Preview 1 shim. A smaller
`wasm32-unknown-unknown` target with a custom host ABI can follow if its size
or embedding benefits justify maintaining a second platform surface.

**HDLL boundary.** Native AOT now supports HDLLs: it detects non-`std`
primitives, links the shared runtime, stages it beside the executable, and
resolves `DEFINE_PRIM` entries at startup. That work is complete for native
AOT and is not a wasm blocker. A native `.hdll` still cannot be loaded inside
a wasm sandbox. A wasm build must reject non-`std` natives clearly; framework
authors guard them with `#if wasm` or provide a separate wasm/host import.

---

## Current state

Measured in September 2026, not inferred from the old spike:

* **The AIR pipeline is whole-program capable.** `--emit-optimized` runs every
  function through AIR and writes ordinary HL bytecode. The existing corpus
  executes equivalently under stock HashLink.
* **Native AOT works end to end.** `ash --build` lowers the complete program,
  emits code and data objects, links the static runtime for `std`-only
  programs, and links the shared runtime for programs using HDLLs. The AOT
  smoke and benchmark lanes exercise the same CLI path users invoke.
* **LLVM's WebAssembly backend accepts a complete Ash AOT module.** This
  command succeeds for the full `bench_fib` program, not a hand-written LLVM
  function:

      ash --emit-aot /tmp/ash-fib-wasi.o \
          --target wasm32-wasip1 --quiet bench_fib.hl

  The result is a WebAssembly relocatable object. Linking it temporarily with
  `--allow-undefined` produces a valid core module containing 372 defined
  functions, a 370-entry `funcref` table, an element segment, and 110
  `call_indirect` sites.
* **Function-pointer lowering is substantially done.** In AOT mode,
  `ash_functions` contains real function symbols or null and generated calls
  do not use interpreter stub sentinels. LLVM and `wasm-ld` lower those
  function addresses into the WebAssembly table. Dynamic closures and
  runtime-produced callbacks still need integration tests, but an explicit
  sentinel-to-table compiler rewrite is no longer the plan.
* **The runtime is a real static library.** `ash_std` now builds as `staticlib`
  as well as `cdylib` and `rlib`. The former fake-archive problem is gone.

The permissively linked `bench_fib.wasm` imports 72 unresolved `hlp_*`,
`hl_*`, and `_setjmp` functions. That is diagnostic evidence that codegen
reached the runtime boundary, not a runnable binary: `tools/wasm/host.mjs`
implements four host functions, not the Ash runtime.

## Target ABI: done, and checked

`TargetAbi` now exists (`crates/ash/src/target_abi.rs`) and is chosen before
anything is decoded. It carries the triple, pointer width, every HashLink
layout derived from that width, and the target's capabilities. The decoder
takes it (`BytecodeDecoder::decode_for_abi`), so enum offsets are computed for
the target rather than inherited from the compiler process; lowering asks it
for field offsets and array element sizes; the module's triple and data layout
are set before a single body is emitted, and the middle end runs on that
machine.

What that produces for `bench_fib` at `wasm32-wasip1`, measured rather than
assumed: 32-bit data layout in the IR, no `hlp_*` declaration taking a
pointer-width `i64`, an object `wasm-tools` validates, and, linked
permissively, a core module that also validates -- 372 functions, a 370-entry
`funcref` table, 110 `call_indirect` sites, exports for `main` and
`ash_module_init`, and 73 imports of which every one is an `hlp_*`, `hl_*` or
`setjmp` symbol. `cargo test -p ash --test wasm_target` is that check, and it
fails if an import outside the runtime's own surface ever appears.

The first thing it caught was a data symbol. `ash_fiber_poll_epoch` -- the loop
safe point's word -- was referenced directly, and WebAssembly has no
relocation that reaches an undefined data symbol, while `--allow-undefined`
covers functions only. The answer is not to drop the safe point: **fibers are
part of the wasm target, driven by the host**, and that word is what a host
scheduler ticks. Generated code now reaches it through a pointer that
`ash_late_init` fills from the runtime's getter, which is the same indirection
a Mach-O dylib already needed for the same reason. `TargetAbi` records it as
`direct_data_relocations`.

The remaining items below were the original list; those still open are marked.

Known examples:

* ~~`layout.rs` fixes `HL_WSIZE` at 8 and fixes the `varray` payload at offset
  24.~~ Done: word size is a parameter, and the AOT paths pass the target's.
* ~~enum layout in `bytecode.rs` uses the compiler process's pointer size.~~
  Done: `decode_for_abi` passes the target's pointer size.
* AOT constants assume an eight-byte object header.
* AOT helper signatures use `i64` where the Rust runtime takes `usize`, which
  is `i32` on wasm32.
* AOT data reads `hl_runtime_obj` offsets with host `offset_of!`.
* `RefData` uses the host binding's `size_of::<varray>()`.
* static closure emission assumes the 64-bit-only `stackCount` field and a
  32-byte `vclosure`.

HashLink's C ABI supports both `HL_WSIZE=4` and `HL_WSIZE=8`; these are Ash
implementation assumptions, not limitations of HL bytecode. They must be
replaced by one target ABI description used by decoding/layout, LLVM
lowering, AOT data emission, and the runtime.

## Route

Keep **AIR → LLVM IR → wasm32 object → WASI link**. The experiment has now
answered Route A's main question: LLVM's WebAssembly backend accepts the
complete generated module and supplies the structured control-flow and
function-table lowering.

Do not build a direct AIR→wasm backend first. It would add CFG structuring,
instruction selection, ABI lowering, relocations and debug metadata while
leaving every runtime problem below untouched.

---

## Delivery phases

Each phase has an artifact or test that decides whether it is complete.

### Phase 0 — full Ash IR to a wasm object ✅ DONE

The old `wasm_spike` proved only that LLVM could emit a hand-written `add`
function. Native AOT has superseded it: the CLI now lowers and emits all of
`bench_fib` for `wasm32-wasip1`, and `wasm-ld` builds its function table and
indirect calls.

Keep the tiny spike only as a toolchain diagnostic. Replace its status as the
project's wasm smoke test once Phase 3 runs a real Ash program.

### Phase 1 — make the compiler target-aware ✅ SUBSTANTIALLY DONE

`TargetAbi` exists and is threaded through decoding, layout, lowering and the
middle end; the wasm32 and 64-bit layout fixtures pass, and the emitted module
is validated by an external toolchain (see "Target ABI: done, and checked").
What remains of this phase is the exhaustive C-header fixture: the layouts
asserted today are the ones the emitter uses, not every structure that crosses
the program/runtime boundary.

The original plan follows.

Create a `TargetAbi` (name provisional) before decoding or lowering. It owns:

* triple, LLVM target machine and data layout;
* pointer-sized LLVM integer type, size and alignment;
* HashLink value, object, array, enum, virtual and closure layouts;
* C ABI types such as `size_t`, `intptr_t` and function pointers;
* target capabilities: WASI, threads, SJLJ/EH and native dynamic loading.

Set the LLVM module triple and data layout before emitting any type or body.
Run the middle end with this target machine instead of recreating the host
machine. Parameterise or remove every host `size_of!`, `offset_of!` and
hard-coded pointer-width offset used by emitted code.

The bytecode decoder should retain semantic enum information rather than
committing it permanently to the host layout. Native JITs can still select the
host ABI; wasm AOT selects the wasm32 ABI.

Add ABI fixtures compiled from `hl.h` for both 32- and 64-bit layouts. They
must verify every structure consumed on both sides of the program/runtime
boundary, not merely the few structures currently checked against the host
MCJIT engine.

*Done when:* a wasm32 layout test covers all shared structures and generated
IR contains no host-derived layout constant or pointer-sized `i64` ABI
parameter.

### Phase 2 — build a single-threaded WASI runtime

**Unblocked, and the work is now visible.** `cargo check -p ash_std --target
wasm32-wasip1` reaches the crate's own code. Two gates were in the way and are
gone:

* bindgen had no C library to parse `hl.h` against. It now takes a WASI
  sysroot -- `WASI_SYSROOT`, else the usual install paths (`brew install
  wasi-libc` supplies one in 10 MB) -- and `-mexception-handling`, without
  which WASI's `setjmp.h` refuses to be included at all, since setjmp there
  IS exception handling.
* `krio-fiber` was an unconditional dependency and cannot work here: a wasm
  module has no addressable stack and no instruction that moves between two.
  It is now native-only, and `std/src/fiber_host.rs` is the wasm backend --
  same four operations, with the one that must suspend routed to the host.

What remains is 114 compiler errors, and they are not evenly spread:

| file | errors | what they are |
|---|---|---|
| `socket.rs` | 58 | a facility WASI preview 1 does not have |
| `sys.rs` | 10 | process and OS services |
| `obj.rs` | 10 | **32-bit layout**: `stackCount` is absent from a 32-bit `vclosure`, `vdynamic` gains `__pad` |
| `process.rs` | 9 | subprocesses |
| `gc.rs` | 8 | the heap wants `mmap`; linear memory has no such call |
| `fiber.rs` | 4 | what the host backend does not yet cover |
| `buffer.rs`, `fun.rs`, `aot_native.rs`, `error.rs`, `debugger.rs` | 13 | `dlopen`, `longjmp` spelling, small ABI differences |

About eighty of them are one decision rather than eighty: socket, process,
thread and debugger are 109 natives that a sandbox cannot provide, and they
should compile to explicit "unsupported" errors rather than be ported. The
rest -- roughly twenty-five -- are the real work: the 32-bit object layout,
a heap in linear memory, and the fiber backend.



Target `wasm32-wasip1` first. Rust supports it as a Tier 2 cross target and
supplies Rust `std`, WASI libc libraries and common OS services. Building a
Rust `staticlib`, using an external linker, and running bindgen against C
headers still calls for a configured [WASI SDK](https://github.com/WebAssembly/wasi-sdk).

`cargo check -p ash_std --target wasm32-wasip1` currently stops in bindgen:

    ./hl.h:213:10: fatal error: 'stdlib.h' file not found

Give bindgen the WASI SDK sysroot, or check in generated target bindings.
Then make the runtime compile by providing or gating:

* `HeapMemory` backed by stable linear memory; no `mmap`, `VirtualAlloc`,
  unmap, `madvise`, or page handback;
* one non-zero mutator identity for the initial single-threaded target;
* WASI paths for stdout, clocks, randomness and allowed file operations;
* explicit unsupported errors for process creation, sockets, native library
  loading, threads and fibers;
* target-specific dependencies so `krio-fiber` and native loader code are not
  required by the single-threaded artifact.

Preserve the mutator/collector interface even when it has one member, so the
later threads target does not require replacing the GC API.

*Done when:* `ash_std` produces a wasm32 archive and a small linked fixture can
initialise it, allocate an object, build a string, and print it under
Wasmtime.

### The host, in Rust: `crates/ash_wasm_runtime`

Three parts, and which side of the module boundary each sits on is the design.

`guest` is compiled **into** the program: `ash_std` depends on it when built
for wasm, so its contents are ordinary Rust linkage and not wasm imports.
Everything that can be done inside the sandbox belongs there, and most things
can, because WASI already gives the standard library a clock, randomness,
stdout and a filesystem. The fiber backend lives there now, moved out of
`ash_std` itself.

`native` is a `wasmtime` host, and it is what the conformance lane will use:
no browser, no JavaScript, no `wasm-bindgen`. wasmtime's own fibers answer the
suspending import, so the capability a browser gets from JSPI is available
here without a browser. The browser host is not written yet: it will be the same
contract behind `web-sys`, and the only JavaScript in that path will be glue
`wasm-bindgen` generates, which is build output in the way an object file is.

One import crosses the boundary today, `env.ash_host_fiber_yield`, and it has
to: a wasm module has no addressable stack and no instruction that moves
between two, so suspension is the one operation it cannot perform for itself.

### Reading a module: `ash wasm`

The compiler can read back what it emitted, which during the port is the
question actually being asked:

```
ash wasm prog.wasm             # the report
ash wasm --validate prog.wasm  # runnable or not, and fail if not
```

The report gives functions, indirect call sites, tables, exports, and the
imports grouped by whether a host could supply them. `--validate` answers one
question for a build gate and exits non-zero when the answer is no, naming
what is missing. On today's `bench_fib.wasm` -- emitted, but linked without
the runtime -- it reports all 72 `hlp_*` symbols as unsatisfied. When that
list is empty, the module needs only WASI and the fiber import, and phase 3
is done.

It reads the module with ash's own parser rather than an external tool, so a
build machine needs nothing installed and `cargo test -p ash --test
wasm_target` asserts on a struct rather than on someone's text output.
`ash-wasm-run` is the same thing plus an engine, for actually running one.

### Phase 3 — link and run a real Ash program

Add a wasm branch to `ash --build` instead of passing a wasm object to the
native `cc`/`clang` driver. The link owns:

* program object plus wasm32 `ash_std` archive;
* WASI libc, compiler builtins and startup objects;
* one deliberate command entrypoint (`_start` calling `main`) and exported
  memory;
* section GC and an explicit import allow-list;
* no `--allow-undefined` escape hatch in a shipping build.

For the first runnable milestone, use a bounded, non-reclaiming heap or disable
collection. That isolates ABI, startup and linking from the root-discovery
work without pretending the result is production-ready.

The current `main(argc, argv)` may continue ignoring arguments for this
milestone. Argument forwarding is required before the target is declared
complete.

*Done when:* `ash --build bench_fib.wasm --target wasm32-wasip1 bench_fib.hl`
runs under Wasmtime, prints the reference checksum, and imports only the
expected `wasi_snapshot_preview1` surface.

### Phase 4 — production GC and exceptions

#### Roots

Linear-memory allocation is not the difficult GC problem. Root discovery is.
The current collector scans native stacks and callee-saved registers
conservatively. WebAssembly locals and operand-stack values are not addresses
in linear memory, so scanning the LLVM shadow stack finds only spills and
address-taken values.

Add explicit roots for pointer-bearing AIR values, using LLVM's GC-root
support or an Ash shadow-root frame. Optimisation must not promote a live
pointer out of the root set. Runtime Rust code also needs scoped roots for raw
pointers held across an allocating call; generated-code roots alone are not
enough.

Do not accept “works with optimisation off” as proof. The backend may still
place values in wasm locals.

#### Exceptions

The old premise that wasm has no `setjmp` is stale. WebAssembly exception
handling is part of Core 3.0, LLVM has WebAssembly SJLJ lowering, and WASI SDK
ships optional `libsetjmp` support. Preserve the existing trap model first:

* enable WebAssembly SJLJ/EH consistently for program and runtime objects;
* link `libsetjmp`;
* use the target's `setjmp`/`longjmp` symbols rather than native
  `_setjmp`/`_longjmp` assumptions;
* validate nested traps, rethrows and the outer entrypoint shield in both
  Wasmtime and the browser engine chosen for CI.

Explicit AIR result-tag lowering remains the fallback if SJLJ portability or
cost is unacceptable.

Native frame-pointer stack walking does not work on wasm. Name-section/source
map based call stacks are separate from exception control flow and may land
after catch/throw correctness.

*Done when:* allocation-heavy programs pass with collection forced at every
safe point, and the native AOT exception corpus passes unchanged as wasm.

## Why the runtime does not use `web-sys`

It comes up because the browser is the destination, and `web-sys` is how Rust
talks to a browser. It is the wrong layer for this crate, for three reasons
that are worth writing down once.

**It supplies the wrong things.** `web-sys` is generated from WebIDL: the DOM,
WebGL, `Worker`, `crypto`, `performance`. The runtime does not want those. It
wants a clock, randomness, stdout and a filesystem -- operating-system
services, which is what WASI is. Measured against the actual port: of the
errors left in `ash_std` for `wasm32-wasip1`, the ones outside `socket.rs` are
seventeen struct-layout mismatches (a 32-bit `vclosure` has no `stackCount`, a
32-bit `vdynamic` gains padding), a heap that wants `mmap`, and cfg fallout.
`web-sys` fixes none of them. It is not a shortcut through this work; it is
orthogonal to it.

**It costs the target.** `web-sys` rides on `wasm-bindgen`, whose supported
target is `wasm32-unknown-unknown` -- a target with no libc and a std whose OS
layer is stubbed out. No clock, no stdout, no files, and no `setjmp`, which is
the trap model. Everything WASI hands over for free would have to be rebuilt
against JavaScript, and the result would run in a browser and nowhere else:
no `wasmtime`, so no CI lane and no server embedding. It also changes the link
model, since `wasm-bindgen` expects to post-process a module rustc produced
and to ship JS glue beside it, where ash links its own LLVM-emitted object
against the runtime archive with `wasm-ld`.

**The browser's extra capabilities are the harness's, not the runtime's.**
What a browser genuinely offers beyond WASI is JSPI for suspending a fiber,
`Worker` plus `SharedArrayBuffer` for threads, and later WebGL, audio and
input for Heaps. Every one of those is something a host provides to the
module, and the module already has the interfaces: one import for fiber
suspension, the mutator interface for threads, HDLL-shaped imports for a
framework backend. A browser host that wants to be written in Rust rather
than JavaScript can use `web-sys` freely -- in its own crate, compiled for
its own target, on the other side of those imports.

So: one runtime, built for `wasm32-wasip1`, reaching the browser through a
small WASI preview-1 shim (phase 5). `web-sys` belongs to the host harness and
to the framework backend, and using it there costs the runtime nothing.

### Phase 5 — browser ABI and conformance

Ship a small WASI Preview 1 loader for Node and browsers, covering stdout,
clock, randomness, arguments/environment and memory. Canvas, WebGL, audio and
input belong to an embedder/framework API rather than the language runtime.

Run the Haxe conformance suite per case, as the interpreter lane does.

**The denominator is measured, and it is almost the whole suite: 1,186 of
1,195 cases, 99.2%.**

A case can only run on wasm if every native it calls can. That set is
observable without a wasm runtime: run the case under the interpreter with
`ASH_TRACE_NATIVE=1`, which prints one line per native call with its library,
and take the union. Two subtractions matter, and getting them wrong moves the
answer by an order of magnitude:

* **The suite's own startup is not the case.** Running a case name that does
  not exist gives the baseline -- 38 natives, including `hlp_ssl_init`,
  `hlp_socket_init` and `hlp_thread_current`. Counting those against every
  case excludes every case.
* **A mutex is not a thread.** `hlp_mutex_*`, `hlp_lock_*`, the thread-locals
  and the atomics are all implementable single-threaded, and wasm has atomics
  besides. Only real thread creation, sockets, subprocesses, the debugger and
  dynamic loading are genuinely out of reach. Treating all 109 natives of
  `thread.rs` as impossible put the answer at 10.5%; it is 99.2%.

The nine cases out of scope, and why:

| case | why |
|---|---|
| `unit.TestMisc`, `unit.spec.TestUnicode`, `unit.spec.haxe.crypto.TestSha1`, `TestMd5`, `TestHmac`, `unit.spec.haxe.zip.TestCompress`, `unit.issues.Issue2861`, `unit.issues.Issue5090` | the `fmt` HDLL, which a sandbox cannot load |
| `unit.spec.sys.net.TestSocket` | BSD sockets, which WASI preview 1 does not have |

Those eight `fmt` cases are compression and hashing, not language semantics:
they come back into scope the day `fmt`'s primitives are provided by the wasm
build rather than by a native library. The score is reported against the 1,186
with these nine named, never quietly dropped.

Add focused wasm tests before the broad suite:

* object, enum, array, virtual and closure layouts;
* direct, indirect, dynamic and reflective calls;
* GC stress across generated code and runtime helpers;
* nested exceptions and uncaught reporting;
* import allow-list and code-size checks;
* identical program output under Wasmtime and the browser runner.

*Done when:* the same `.wasm` runs unmodified under Wasmtime and the browser
loader, and the published conformance result is reproducible in CI.

### Phase 6 — threads, fibers and Heaps

Threads and fibers are different problems here, and only one of them is
deferred.

**Fibers are part of the target, driven by the host.** A fiber is cooperative:
it suspends at a point the program chose, and something outside decides when it
resumes. On a native target that something is `krio-fiber` switching stacks.
A wasm module cannot switch its own stack, so `std/src/fiber_host.rs` is the
backend there: the same four operations the scheduler above uses, with the one
that must suspend routed to a single import, `ash_host_fiber_yield`.

Deliberately one import and not a topology, because there are three ways to
implement it and they trade differently:

| how | needs | costs |
|---|---|---|
| engine suspension (JSPI, `wasmtime` async) | an engine that has it, which as of Safari 27 is all three | none beyond the call; single-threaded, no headers |
| a worker per fiber over shared memory, parked on `Atomics.wait` | `wasm32-wasip1-threads`, and COOP/COEP in a browser | a fiber becomes an OS thread; every collection becomes a rendezvous |
| Asyncify | nothing | roughly double the code size, and a tax on every call |

The middle row is the one that works with no engine feature, and it is why
this cannot simply be declared solved by shared memory and workers: it pulls
the whole threads target forward into the first release, and it prices a fiber
at a thread when ash's scheduler is M:N. Where JSPI exists it is strictly
cheaper. So the module marks where it may be suspended, the harness decides
how, and the choice can differ between the browser and the server without the
program changing.

**krio reached the same conclusion, and settled the engine question.** Its
"krio Across Workers" design note records JSPI as having landed in all three
engines -- Chrome 137+, Firefox 153+, Safari 27 beta -- which promotes engine
suspension from a one-browser bet to the route a browser host should take by
default, and it quotes this document's own pricing of the worker-per-fiber
alternative back at us. It also plans `krio-fiber` as a JSPI backend at that
tier, with two new crates beside it: `krio-parallel` for work stealing over
`Send` tasks, and `krio-wasm` as the only crate that knows about JS, workers
or COOP/COEP. Two consequences for ash. The backend in
`ash_wasm_runtime::guest` is an interim: when krio's JSPI backend lands, the
wasm build should take it and keep the shim only for hosts without JSPI. And
the import stays as it is either way -- with JSPI a host binds
`ash_host_fiber_yield` to a suspending function, which is exactly what that
import is for.

What the compiler owes all three is the same, and it already emits it: a safe
point in every loop and a word the scheduler can tick, the epoch reached
through a pointer since wasm cannot relocate an undefined data symbol.

**Threads are deferred.** Do not make `wasm32-wasip1-threads` part of the first
release. The target exists, but Rust still describes it as in flux, engines
need WASI-threads support, and browser shared memory imposes COOP/COEP
deployment requirements. A Worker is not by itself a replacement for stackful
fiber semantics.

Add threads only after single-mutator GC correctness. The work includes shared
memory, worker startup, mutator rendezvous, fiber semantics and host deployment
documentation.

Heaps follows the single-threaded language/runtime target. Its rendering work
is a framework-side wasm/WebGL backend. Ash's acceptance gate is that Heaps'
non-rendering code, allocation, exceptions, reflection and callbacks are
correct before graphics-specific imports are introduced.

---

## Risk register

| risk | current evidence | deciding measurement |
|---|---|---|
| **32-bit ABI** | the full module emits, but several generated layouts still use host/8-byte constants | exhaustive C-header vs `TargetAbi` layout fixtures plus runtime allocation tests |
| **GC roots** | native conservative scanning cannot see values kept only in wasm locals | collect at every safe point across generated and runtime code |
| **Exceptions** | LLVM/WASI now has an SJLJ route, but Ash emits native symbol spellings and flags | unchanged nested trap/rethrow corpus on Wasmtime and browser CI |
| **Indirect calls** | LLVM/`wasm-ld` already emits a table, element segment and `call_indirect` | closures, vtables, reflection and runtime-created callbacks |
| **Runtime surface** | `ash_std` currently fails before compilation at bindgen; several modules have only Unix/Windows branches | wasm archive with no unintended imports, followed by stdlib/conformance lanes |
| **Code size** | no runtime has yet been linked into a real program | stripped `bench_fib.wasm` and `test_stdlib.wasm`, with per-section accounting |
| **Threads** | target and proposals exist, but the scheduler/fiber implementation is native | separate post-MVP worker, GC rendezvous and deployment tests |

GC roots and the 32-bit ABI are the correctness risks. Exceptions and indirect
calls now have credible toolchain answers, but still need Ash integration
tests. Code size is measured only after the correct runtime links; the current
permissive module is not representative.

## What this is not

* Not a wasm interpreter for HL. It emits compiled wasm.
* Not a replacement for the native interpreter, Cranelift, LLVM JIT or native
  AOT outputs.
* Not a way to load native `.hdll` files in a sandbox. A future wasm-native
  extension/import ABI would be a different artifact and contract.
