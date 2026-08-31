# hl2wasm — a WebAssembly target from optimized AIR

**Goal.** Turn HL bytecode into a `.wasm` module, through the AIR pipeline, so
Heaps and other Haxe frameworks have a WebAssembly target to build against.

**Explicitly out of scope.** HDLLs. A `.hdll` is a native shared object and
there is nowhere to load it in a wasm sandbox. Framework authors guard those
call sites with `#if wasm`, exactly as they already do for other targets. We
do not emulate them, stub them, or apologise for them.

---

## What already exists

Measured, not assumed:

* **The AIR pipeline already produces a shippable artifact.** `--emit-optimized`
  (612a0b5) runs every function through AIR and writes HL bytecode back out;
  all 68 corpus modules execute identically under stock HashLink 1.15.0, and
  fib runs 68.3% faster there. That proves the optimized AIR is *complete and
  correct as a program*, not merely as something our own engines can consume.
* **LLVM here has the WebAssembly backend built** (`llvm-config
  --targets-built` lists `WebAssembly`, LLVM 21.1.2).
* **The AIR→LLVM lowering is target-agnostic except for one line.**
  `llvm/module.rs:175` takes `TargetMachine::get_default_triple()`. Nothing
  else in the lowering names the host.
* **The runtime is not deeply 64-bit.** Four `target_pointer_width` sites in
  `std/`, and `sys.rs:136` already carries a 32-bit branch.
* **`ash_std` is 363 `hlp_*` natives**, the majority pure computation —
  strings, math, JSON, arrays, maps.

## The runtime must be statically linked

Today `ash_std` is a **cdylib** whose bytes are embedded with `include_bytes!`
and written to disk at runtime for `dlopen` (`native_lib.rs:278`). The
`libash_std.a` in `OUT_DIR` is a dylib wearing a `.a` filename — `file` says
"Mach-O 64-bit dynamically linked shared library" — and the name is fixed only
because `include_bytes!` needs a stable path (`build.rs:301`).

That mechanism has no analogue in either AOT target. wasm has no `dlopen`, and
for native AOT a dylib dependency merely relocates the C-wrapper problem
instead of removing it. So AOT needs genuine static linking, in three
ascending forms:

1. **`declare` + static archive.** `External` declarations in the IR — which
   says nothing about static vs dynamic, only "not defined here" — plus a real
   `staticlib` crate-type so the linker pulls `libash_std.a` into the output.
2. **Whole-archive.** Natives are reached through `functions_ptrs` and closure
   tables, so the linker cannot see they are live and will garbage-collect
   them. Needs `--whole-archive` or an explicit retained list; `reachable.rs`
   already answers that question for us.
3. **Bitcode + LTO.** Emit `ash_std` as LLVM bitcode, link it into the module
   at IR level, and run the middle end over program and runtime together.

**Three is what "optimal AOT" means here.** `gc.rs:138` says the TLAB cursor
and limit are exported statics "so a JIT tier can inline the bump sequence
later" — and that was never built, because a JIT cannot see across an opaque
native call. Under IR-level LTO it needs no new code: `hlp_alloc_obj` becomes
inlinable at the allocation site and the bump folds into the caller. On
binary_trees that body alone is ~12% of the run. The same argument applies to
the closure guards that LICM could not hoist past a call boundary.

## AOT works end to end

Four programs — `bench_fib`, `test_basic`, `test_stdlib`, `bench_deltablue` —
compile to native objects, link against `libash_std.a`, and print **byte for
byte what the JIT prints**. `tools/aot/smoke.sh` is that check; it gates on the
diff, not on the exit status, because a module that starts and runs and prints
something slightly different is the failure worth catching. No C compiler and
no C source is in the path: `clang` appears only as the driver that knows where
crt and libc live.

    bench_fib      OK (lowered 332/332)
    test_basic     OK (lowered 335/335)
    test_stdlib    OK (lowered 387/387)
    bench_deltablue OK (lowered 407/407)

### The code half

A fork at the tail — `emit_object` where the JIT calls
`get_function_address` — plus two corrections: natives become `External`
declarations behind a forwarding thunk rather than baked addresses, and
everything lowers into ONE module instead of a promo module per function.

Every remaining site that resolved a pointer at compile time had to become a
symbol or a refusal, and the list is short enough to state: type descriptors,
`functions_ptrs` slots, per-findex `HFUN` descriptors, the fiber poll epoch,
`setjmp`, and `hlp_error`. What could not become a symbol fails the function
rather than emitting an address — `reject_in_aot`. A refused body is then
*sealed*: the emitter stops where it failed and leaves blocks with no
terminator behind, which are invisible until the next middle-end run walks the
module and dies inside `SimplifyCFG`, far from the cause.

### The data half

* **The type table is emitted as object data.** `aot_data.rs` walks the type
  graph the compiler already built in memory and writes an LLVM global per
  node, so an AOT module cannot disagree with the JIT about what the table
  contains — there is only one builder. Layouts are checked against
  `size_of` before anything is written; a silent drift here would be an object
  that links, runs, and reads the wrong words.
* **The constant pool is emitted as startup code.** `init_constants` allocates
  and fills in the compiling process; `emit_module_init` emits the identical
  sequence as `ash_module_init`, reading field offsets from
  `hlp_get_obj_rt` at startup rather than baking them, because computing them
  twice is how the two come to disagree.
* **Globals are `ash_globals`**, a real array in the object, published to the
  collector with `hlp_gc_set_globals` before the first allocation.

Three arrays anchor the result — `ash_globals`, `ash_functions`,
`ash_function_types` — and they are what `hl_module_context` points at, so
reflection, bindings and closure allocation reach the same tables they do
under the JIT.

### Two findings worth keeping

**Symbols collide.** A bytecode function carries its Haxe name, and in an
object file that name competes with libc. A Haxe method called `write` linked
ahead of `libSystem`'s, and the runtime's own `println!` jumped into Haxe code
and faulted before a single line came out. Every bytecode body is now internal;
the object exports exactly `main` and `ash_module_init`. Internalizing happens
after lowering, not at creation — a not-yet-lowered callee is declared through
the same path, and an internal declaration is invalid IR.

**The trap shield is not optional.** A trap lowers to `setjmp`, and C's rule
applies to the IR: a local that is not in memory has an indeterminate value
after the jump. `mem2reg` alone is enough — with the shield down, a nested
try/catch reported "none" for an exception the inner handler did catch, while
single-level catches still worked. The JIT raises this shield before every
promotion; AOT optimizes once, at the end, and has to raise it there.

AOT optimizes once rather than per function on purpose. There is no address to
ask for, so nothing forces codegen per body, and running the module pipeline
per lowered function is both quadratic and destructive: it deletes emitted data
that no lowered body happens to reference *yet*, and the next body to want that
type finds a handle pointing at freed memory.

### What AOT still cannot do

* **HDLL primitives.** They resolve through a `DEFINE_PRIM` resolver in a
  shared library, and there is no shared library to load. Refused by name.
* **Cross-compilation is untested.** The host-CPU stamp is suppressed under
  AOT so the target machine handed to `emit_object` decides, but only the host
  triple has been run.
* **`main` ignores `argc`/`argv`.** Nothing forwards them to the program yet.

This is the same information HL/C writes out as C data tables, which is why its
output needs a C compiler and ours does not. It is also **exactly what wasm
needs** — a wasm module has no access to the compiler's heap either — so it was
built once, against the native target where a debugger still works.

## Route

**Take the LLVM WebAssembly backend (Route A).** AIR → LLVM IR → wasm32
object → `wasm-ld`. It reuses `llvm/function.rs` wholesale: every opcode,
every guard, the TBAA tree, the devirtualisation, the FMA policy. The work is
retargeting, not rewriting.

**Do not write a direct AIR→wasm emitter (Route B) first.** wasm demands
*structured* control flow — blocks, loops, `br` to enclosing labels — while
AIR is an arbitrary CFG. Bridging that needs a Relooper or Stackifier, which
is a real algorithm with real bugs, and it buys nothing until Route A has
proven the rest of the stack. Revisit only if LLVM's output disappoints or the
toolchain dependency becomes a problem; the `bytecode_encode` writer is the
precedent that we can emit a format directly when it is worth it.

---

## Phases

Each phase ends in something runnable, and each names the measurement that
says it worked.

### Phase 0 — spike: one function to wasm  ✅ DONE

`crates/ash/examples/wasm_spike.rs` builds a `wasm32-unknown-unknown`
`TargetMachine` and emits an object; `tools/wasm/` links and runs it. The
chain works end to end:

    wasm target : wasm32 (WebAssembly 32-bit)
    emitted     : /tmp/spike.o (257 bytes)   -- WebAssembly binary, version 0x1
    linked      : spike.wasm (265 bytes)
    wasmtime add(2,3) = 5
    node     add(2,3) = 5

Two toolchain notes worth keeping:

* **Homebrew's `wasm-ld` is unusable here.** It is lld 20.1.7 resolving
  against LLVM 21.1.2's `libLLVM` and aborts on a missing
  `ELFAttributeParser` symbol. `tools/wasm/link.sh` uses the Rust toolchain's
  own `rust-lld -flavor wasm`, which is version-matched to itself.
* **The rustup wasm targets are already installed** —
  `wasm32-unknown-unknown`, `wasm32-wasip1`, `wasm32-wasip1-threads`. Phase 1
  has its toolchain today.

The original Phase 0 text follows, for the part still outstanding.

### Phase 0 (remainder) — our own IR to wasm

Build a `TargetMachine` for `wasm32-unknown-unknown` instead of the host
triple and emit an object file for a single arithmetic function
(`bench_fib`'s inner). No runtime, no GC, no calls.

*Done when:* `wasm2wat` shows a plausible function body and `wasmtime`
executes it with the right answer.

*This phase exists to fail fast.* If LLVM's wasm backend rejects what our
lowering emits — address-space assumptions, `inttoptr` of absolute host
addresses, the `returns_twice` setjmp shim — we learn it in a day rather than
after the runtime port.

### Phase 1 — the runtime on wasm

**Target `wasm32-wasip1` first, not `wasm32-unknown-unknown`.** Attempting
the latter fails immediately and instructively:

    ./hl.h:206:10: fatal error: 'stdlib.h' file not found
    panicked at std/build.rs:126: Unable to generate bindings

`hl.h` includes libc headers and `wasm32-unknown-unknown` has no sysroot to
find them in. WASI does, and it also supplies the clock, randomness and
stdout that Phase 3's host ABI would otherwise have to invent. Browsers reach
WASI through a JS shim, which is a smaller problem than porting libc.

So: **wasip1 for the runtime and conformance, `unknown-unknown` plus the
custom ABI later for the smallest possible browser artifact.** Either way the
blockers below are the same, in the order they bite:

1. **GC memory.** Today the heap comes from `mmap`/`VirtualAlloc`. On wasm it
   is linear memory plus `memory.grow`, which never moves and never unmaps —
   simpler than either host path. Immix's block structure is unaffected;
   `release_tlab_region` and the handback path become no-ops.
2. **Thread identity.** `thread_self_fast` reads `TPIDRRO_EL0` via inline asm
   (`gc.rs:201`). Single-threaded wasm has exactly one mutator, so this is a
   constant. See `foreign-thread-identity` for why returning 0 is not safe:
   pick a non-zero constant.
3. **Exceptions.** `hlp_throw` is `setjmp`/`longjmp`. wasm has no such
   primitive. Two options, in preference order: the **exception-handling
   proposal** (`try`/`catch`, now widely shipped), or lowering every HL
   `Trap`/`EndTrap` to an explicit result-tag check in AIR. The second is
   portable to every engine and is a pass we could reuse elsewhere; the first
   is far less work. Decide with a spike, not in this document.
4. **Threads and fibers — implementable, not deferred.** Ten sites reference
   `krio-fiber` or `thread::spawn`. Both have real wasm answers:

   * **Threads** via the threads proposal: `wasm32-wasip1-threads` is already
     an installed rustup target, and in a browser shared memory needs
     cross-origin isolation (COOP/COEP headers) to get `SharedArrayBuffer`.
     Workers then run the same module against one shared linear memory.
   * **Stackful fibers on Workers.** A fiber is not an OS thread — it is a
     stack plus a switch — so a Worker per fiber is a legitimate
     implementation, with the scheduler handing off through the shared
     memory. `krio-fiber`'s switch is the part that needs a wasm backend, not
     the concept.

   Phase 1 still *starts* single-threaded — `hl_blocking` a no-op, the fiber
   poll compiled away, `Thread.create` raising — because it is the shortest
   path to a running program. But the design must not paint threads out: the
   GC in particular has to keep its mutator registry, since a single-mutator
   shortcut would have to be unpicked to add Workers later. COEP is a
   deployment constraint on the embedder, so it belongs in the Phase 3 host
   documentation rather than being discovered by a framework author.

*Done when:* `ash_std.wasm` links and a hand-written test module can allocate
an object, build a string, and throw and catch.

### Phase 2 — codegen and linking

* Parameterise the target: `ash --target wasm32 in.hl -o out.wasm`.
* Emit one object per module and link against the runtime with `wasm-ld`.
* Resolve the calls the JIT resolves at runtime today — `functions_ptrs`
  entries, closure `fun` fields — through a **wasm function table** with
  `call_indirect`. This is the piece with no host analogue: our stub-sentinel
  trick (`findex + 1` in a pointer) has no meaning in a wasm table, so
  closures must carry a table index. Note the divergence found in
  `--emit-optimized`: a register whose address is taken is where ash and stock
  HashLink already disagree, and a table index is a similar re-encoding
  question. Expect to spend real time here.

*Done when:* `bench_fib.wasm` runs under `wasmtime` and prints the reference
checksum.

### Phase 3 — the host ABI

A small, documented import surface, and nothing more: write to stdout, read
the clock, random bytes, and grow memory. Everything a program needs beyond
that — canvas, WebGL, audio, input — belongs to the embedder, not to us.
Ship a minimal JS loader that satisfies the imports so a framework author can
`instantiate` and go.

*Done when:* the same `.wasm` runs unmodified under `wasmtime` and in a
browser.

### Phase 4 — conformance

Run the Haxe conformance suite against the wasm target, per-case isolated, the
same way the interpreter is gated (`conformance-isolation`). Publish the
number beside the existing ones. It will start low; that is the point of
having it.

*Done when:* the suite runs end to end and the figure is on the site.

### Phase 5 — Heaps

Only now. Heaps needs a GL backend, and that is `#if wasm` work in Heaps
against WebGL, not work in ash. Our deliverable to them is a language target
that runs their non-rendering code correctly and fast.

---

## Risks, each with the measurement that settles it

| risk | why it might bite | how we find out |
|---|---|---|
| **32-bit pointers** | wasm32 is 32-bit; `vdynamic` layouts, `hl_type` fields and the closure ABI assume 8-byte pointers | Phase 0 spike plus a `size_of` audit. HL itself ships 32-bit builds, so the *format* is width-agnostic; only our implementation may not be |
| **Exceptions** | no `setjmp` in wasm | Phase 1.3 spike: wasm EH vs an AIR result-tag lowering |
| **Indirect calls** | stub sentinels (`findex+1`) are not table indices | Phase 2; closures carry a table index instead |
| **GC scanning** | conservative stack scanning has no stack to scan in wasm | wasm has no readable stack. This may force a **precise** shadow stack for roots — the single largest unknown in the plan, and the one most likely to change its shape |
| **Code size** | a whole-module AOT compile of a Haxe program plus runtime could be large | measure `bench_fib.wasm` and `test_stdlib.wasm` at the end of Phase 2 |

The GC row is the one to worry about. Every other risk has a known answer
somewhere; that one may require a different collector design for this target.
Find out in Phase 1, before Phase 2 makes it expensive.

## What this is not

* Not a wasm *interpreter* for HL. We emit compiled wasm.
* Not a replacement for the native tiers. This is a fourth output, beside
  interpreter, Cranelift and LLVM.
* Not HDLL-compatible, now or later.
