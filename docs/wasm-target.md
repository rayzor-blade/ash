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

### Phase 5 — browser ABI and conformance

Ship a small WASI Preview 1 loader for Node and browsers, covering stdout,
clock, randomness, arguments/environment and memory. Canvas, WebGL, audio and
input belong to an embedder/framework API rather than the language runtime.

Run the Haxe conformance suite per case, as the interpreter lane does. Add
focused wasm tests before the broad suite:

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
resumes. On a native target that something is `krio-fiber` switching stacks; in
a wasm module it is the host, through JavaScript Promise Integration or the
stack-switching proposal where an engine has it, and an explicit host-driven
scheduler where it does not. What the compiler owes either arrangement is the
same: a safe point in every loop and a word the scheduler can tick, both of
which the wasm build now emits (the epoch is reached through a pointer, since
wasm cannot relocate an undefined data symbol). The backend that actually
suspends a fiber is the outstanding work; the compiled program is already
asking to be preempted.

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
