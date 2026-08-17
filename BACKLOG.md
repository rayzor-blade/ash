# ash — Development Backlog

Known gaps, deferred refinements, and open defects. Code docs describe what the
code guarantees today; anything that should change lives here.

**Last updated**: 2026-08-17

**Status**: Heaps Base2D renders through the real init path, and hybrid mode
(`ash_cli --mode hybrid examples/heaps_base2d/bin/game.hl`) sustains 19
promoted functions with no crashes, backed by a beadie broker, a shared
symbol table, thread-safe GC, and a 0.05s tiered pre-warm. The
full-JIT-everything path (`ash <file>.hl`) still has one open crash; see
[JIT & tiering](#jit--tiering).

---

## AIR v2

The typed phi-SSA IR (`crates/air/src/v2`) is in place with lowering,
verification, a serializer back to standard HL bytecode, a native-import
declaration table, loop and alias-class analyses, and a pass manager running
null-check elimination, GVN/CSE, LICM, the FMA peephole and DCE. v1 remains
the production path until the backends switch over.

- **Remaining optimization passes over the typed IR**, in payoff order
  established by the mandelbrot trace: static field-offset resolution,
  inlining, escape analysis and scalar replacement (below), bounds-check
  elimination, box/unbox forwarding. (Field-load GVN with the HL alias
  classes, dominance-based null-check elimination and LICM have landed.)
- **`Function::float_types` needs module info.** A `TypeRef` is an index into
  the module's type table, so `air` cannot tell floats apart on its own: the
  embedder answers `ModuleInfo::is_float`. Lowering through the bare `lower()`
  wrapper records no float types, which makes the FMA peephole inert — the
  JIT must lower through `lower_with`/`ModuleBuilder` to get fusion.
- **The FMA peephole is same-block only.** Sinking a multiply into a
  dominated block is legal for a pure operation but is refused today, so a
  product computed in a loop preheader and consumed in the body does not
  fuse. It also refuses both negating forms when an operand register happens
  to be the product's register, rather than allocating around it.
- **Live-range extension uses per-value private registers.** Any pass that
  lengthens a value's live range appends a register for that value instead of
  running a real allocator, and refuses outright when the value is a `Param`.
  Correct and cheap, but it grows the register table and turns elided copies
  into real `Mov`s on the serialize path.
- **LICM skips loops whose entry edges cross a trap-region boundary** (and
  loops whose header is a trap handler). A preheader has to share the
  header's handler, so a loop opened by a `Trap` terminator never gets one.
- **GVN's clobber-free check is quadratic.** Each `ReadMem` candidate walks
  the whole reachable-and-reaching block region. Fine at fixture scale;
  a per-class memory-SSA numbering would replace it if it shows up in
  compile-time profiles.
- **Scalar replacement of aggregates, and the inlining it depends on.** The
  traced mandelbrot inner loop spends two heap allocations and three calls
  per iteration because every `Complex` is a `New` plus a constructor call —
  the float math is a minority of the work. Replacing a non-escaping `New`
  with SSA values for its fields (and the same for `EnumAlloc`/`MakeEnum`
  payloads, `vvirtual` boxes, and `ToDyn` boxing) is what turns that loop
  into straight-line arithmetic, and it is the prerequisite that makes
  vectorization possible on such loops at all.
  **Ordering matters in HL: inlining must come first.** `new C(...)` passes
  the fresh object to its constructor, so *every* allocation escapes by
  definition until that constructor is inlined; escape analysis run before
  inlining will find nothing. Other guards: `Ref`-taken values are cells and
  cannot be scalarized; an object live across a trap-region boundary is
  currently pinned to a cell by the conservative in-region rule, so SROA
  inside `try`/`catch` stays blocked until liveness refinement lands.
  Removing an allocation is always safe for the conservative GC — it strictly
  reduces what must be traced — but partial scalarization (some fields
  promoted while the object still exists) must keep the object's memory and
  the promoted values coherent.
- **JIT lowers from v2** instead of raw opcodes — the point at which
  malformed-IR crashes and verifier rejections become structurally impossible
  rather than blacklisted.
- **Liveness-refined trap-region pinning** — every register written inside a
  trap region is currently pinned to a cell. Liveness would narrow this to
  registers actually live into the handler.
- **Landing-pad design for exceptional-edge copies** — would lift the
  handler-block non-trivial-phi restriction.
- **Tail-recursion elimination.** In phi-SSA this is a natural rewrite: a
  `Call` to the enclosing function whose result flows straight into `Ret`
  becomes a parallel copy of the arguments into the entry block's parameters
  plus a jump to a loop header. It pays off on all three engines — the
  interpreter recurses a host frame per HL call, so deep recursion costs
  real stack there. Guards it must respect: a call inside a trap region is
  not a tail call (the handler stays live), `Ref`-taken or `Incr`/`Decr`
  parameters are cells and cannot be silently re-bound, and mutual recursion
  is out of scope for a first cut.
- **Loop vectorization.** Earlier analysis deferred this to LLVM's loop/SLP
  vectorizers, but that assumed LLVM was the only compiled backend. Cranelift
  has SIMD types and instructions (`i8x16`…`f64x2`, including a vector `fma`)
  and *no* vectorizing pass, so whatever the middle tier should vectorize,
  AIR must vectorize. Prerequisites are mostly in place or planned: the alias
  classes give memory-dependence information, loop analysis gives the trip
  structure; still needed are unit-stride access detection over varray/Bytes
  data and a cost model. Best targets are `Bytes`/varray loops (clean stride,
  no cross-class aliasing) and pixel loops once inlining and scalar
  replacement have de-abstracted them.

## JIT & tiering

- **`FunctionLookupError` is the last blacklist class** — 47 occurrences on
  `game.hl`. Functions reachable only indirectly are never code-generated
  before MCJIT finalization, so `get_function_address` cannot find them.
  Fixing it needs either eager declaration of indirectly-reachable callees
  before finalization, or an execution engine that compiles on demand.
- **27 std natives remain unimplemented** (file, process, and rnd families).
  Each compiles to an `hlp_error` call-time trap, so they only fail if
  actually called; implement as workloads demand them.
- **Full-JIT `game.hl` crashes** in LLVM `SimplifyCFG` during MCJIT
  finalization after compiling ~5000 functions (malformed-IR family). Tiered
  promotion sidesteps it; AIR v2 lowering should remove the cause.
- **Hybrid `test_mandelbrot_small` post-main `SIGSEGV`** (exit 139) after
  printing the correct checksum. Pre-existing, reproduces on baseline.
- **MCJIT is legacy** — isolate the execution-engine surface
  (`create_jit_execution_engine`, `get_function_address`, `run_function`,
  `add_global_mapping`) behind a trait so an ORC backend can replace it
  without touching opcode lowering.

## Cranelift middle tier

Validated on aarch64-apple-darwin: 0.04 ms per small function, no MAP_JIT
hazard, bit-exact AAPCS64 through transmuted pointers, safe under the beadie
broker shape (backend on one thread, compile+finalize on another, execute on
a third). Pins: cranelift 0.130.2, `wasmtime-internal-jit-icache-coherence`
43.0.2.

- **Build the tier**: `beadie` `TieredAdapter` ladder (interpreter → Cranelift
  `opt_level=speed` → LLVM), AIR v2 → CLIF lowering, `enable_probestack=false`
  and `preserve_frame_pointers=true`.
- **Shared layout oracle first** — extract the compile-time decisions
  (vtable slot and findex resolution, field offsets per `HOBJ`/`HVIRTUAL`/
  `HDYNOBJ`, enum construct offsets, unbox decisions) out of the LLVM
  lowering so both backends consume one implementation. Every one of these
  opcodes has produced a corruption bug at least once.
- **One `hl_kind → AbiClass` mapping** shared by the CLIF signature builder,
  the LLVM function type builder, and the interpreter's `arg_kinds` /
  `float_mask`, with a differential test calling the same findex through all
  three engines and comparing bit-exactly.
- **Exclude trap-containing functions** — Cranelift has no `returns_twice`,
  so a resumed longjmp into a Cranelift frame is unsound. Measured at 1.0% of
  functions (4.0% of opcodes) in `game.hl`; throw-only functions stay
  eligible because the interpreter's per-call setjmp wrapper guarantees the
  jump exits the frame. Three ways to lift the exclusion, cheapest first:
  *(a)* **outline the trap region** so the setjmp lives in an interpreter or
  LLVM stub and the protected body is a separate Cranelift function —
  longjmp then always exits Cranelift frames, and AIR v2's first-class trap
  regions already provide the structure for the transform; *(b)* adopt
  Cranelift's **`try_call` / exception tables** (present since 0.134), whose
  landing pads are real CFG edges the register allocator understands — this
  requires moving the compiled tiers from longjmp to unwinding-based throws,
  and would also let the LLVM tier drop `returns_twice`, which today
  pessimizes optimization around every try/catch; *(c)* **fiber-based
  unwinding**, sound because a fiber switch restores callee-saved registers
  cooperatively (unlike longjmp), but grain-mismatched — it needs a
  discardable fiber per active trap region, nested for nested traps, paying
  setup cost even when nothing throws.
- **Cross-tier FMA policy — emit explicit `fma`, do not rely on contraction.**
  Measured against a C port of `Mandelbrot.hx` (`clang -O2`, checksums for
  298² / 875×500): unfused `-ffp-contract=off` gives 22816350 / 112790102 and
  matches ash's interpreter bit-for-bit; fused `-ffp-contract=on` (clang's
  default, 4 `fmadd` in the binary) gives 22825041 / 112798515 and matches
  ash's LLVM tier exactly on 298² — and 112798515 is also the hxcpp/hxjava
  value. So **fusion is what every reference implementation does**, and the
  unfused number appears nowhere but a strict interpreter. Cranelift has an
  `fma` instruction (spike-verified bit-exact with `f64::mul_add`) but does
  not auto-contract, so the tier must emit `fma` explicitly — best as an AIR
  peephole (`Mul` feeding a single-use `Add`/`Sub` → `Fma`) that both
  backends lower literally, which also removes ash's dependence on LLVM
  DAGCombine heuristics (the residual 72-unit gap from hxcpp at 875×500 is a
  fusion-*pattern* difference, not a fusion-presence one). Repro:
  `scratchpad/fma_repro/mandel.c`.
- **`TieredAdapter` has no batched-flush path** — tier ≥ 1 promotions pay one
  `finalize_definitions` per function. Acceptable at hot-tier volume; the
  null-sentinel trick (backend performs `swap_compiled_with_osr` itself)
  applies if it shows up in profiles.

## Native symbols & libraries

- **Lazy HDLL loading via forward declarations** — build the symbol table
  from bytecode native declarations as `{lib, name, Resolved(addr) |
  Unresolved}`, and `dlopen` each hdll on first lookup against it, resolving
  all of its `DEFINE_PRIM` exports in one pass. Startup then loads zero
  hdlls.
- **Stale `/usr/local/lib/libhl.dylib`** — a root-owned March build is
  preferred by C hdlls even though ash now detects it (canary symbol) and
  falls back to the embedded stdlib. Refresh it with
  `sudo cp target/debug/libash_std.dylib /usr/local/lib/libhl.dylib`.

## GC

Heap is demand-committed with adaptive triggers, external pressure
accounting, and `ASH_GC_STATS` / `ASH_GC_STRESS` observability
(`ASH_GC_HEAP_MB`, `ASH_GC_TRIGGER_MB` configure it).

- **Interpreter root snapshots go stale** — constant buffers and globals are
  registered once, so `ASH_GC_STRESS=1` under the interpreter frees live
  data. Roots need re-registration per snapshot, or per allocating native
  call. Real-frequency collections are unaffected.
- **Worker-triggered collections scan only the triggering thread's stack** —
  block-level reclamation mitigates it. Keeping GC-allocating work on the
  main thread is the current discipline.
- **128-byte line rounding** wastes up to 8× on small objects (a 16–24 byte
  `vdynamic` occupies a full line).
- **Block-level reclamation only** — line-level reclamation needs marking
  precise enough to prove a line dead, which conservative scanning cannot
  give today.
- **Per-fiber scan ranges** — the fiber runtime swaps trap/exception state
  per fiber, but interpreter scan ranges are still global.

## Runtime & Heaps

- **`hxd.Window.event` null-closure `Null access`** — thrown once per startup
  during `sdl.Sdl.processEvents`, caught by `runMainLoop`; rendering
  continues.
- **`win_create_ex` argument-swap heuristic** (`crates/ash_sdl/src/sdl.rs`)
  papers over caller-side marshaling: it guesses based on value magnitude.
- **~40 no-op stubs in `ash_sdl`** — controllers, joystick, haptics, cursors,
  clipboard, displays, surfaces.
- **`hlp_thread_create` fibers**: `hlp_tls_*` is still pthread-keyed rather
  than per-fiber, and blocking primitives yield only when fibers exist.
- **Debug output sweep** — `ash_sdl` first-N-call counters, the
  `ASH_DUMP_TYPES` block with hardcoded Heaps indices in `ash_cli`, and the
  post-main `hlp_sys_get_loop` print (the last two also break
  `parity_matrix`, which compares stderr).
- **Committed build artifacts** in `examples/heaps_base2d/bin/` (`game.hl`
  plus eight `.hdll` files).

## Dead code

- **Delete the pre-JIT module path and drop the `hlbc` dependency.**
  `crates/ash/src/{module.rs, functions.rs, values.rs}` (916 + 574 + 209
  lines) reference only each other and `types.rs`'s `FunPtr` enum, and
  `AshModule` is constructed in exactly one place — its own unit test at
  `module.rs:888`. It is the pre-`jit/` prototype: `translate_opcode` covers
  about twelve opcodes, bails with "not yet implemented" on the rest, and
  never implemented jump targets at all. The live entry point is
  `main.rs` → `ash::jit::module::JITModule`, and `jit/`, `bytecode.rs` and
  `c_types.rs` contain no `hlbc` at all (they use ash's own decoder and
  AIR's vendored opcode definitions). Removing the three files, the `FunPtr`
  enum with its `use hlbc::types::{Function, Native}`, the three `pub mod`
  lines in `lib.rs`, and the `hlbc` entry in `crates/ash/Cargo.toml`
  eliminates the last dependency on an upstream that has been unmaintained
  for two years — without forking or vendoring anything.

## Build & tooling

- **`build.rs` dylib path** — nightly cargo emits `libash_std.dylib` to
  `target/debug/`, while `crates/ash/build.rs` reads
  `target/aarch64-apple-darwin/debug/` and panics if it is missing. It should
  fall back.
- **No test runner** — there is no `run_tests.sh`; the parity matrix
  (`crates/ash/test/tests/run_stdlib_matrix.sh` + `parity_cases.toml`) and
  `scripts/run_perf_matrix.py` are the closest things.
- **README is stale** — it documents O0/O1/O2 AIR levels and describes MCJIT
  codegen as "O3 optimizations"; no LLVM middle-end pipeline runs at all
  (adding one is the cheapest large win available).

## Cranelift tier — follow-ups

- **Own the backend; implement `beadie::JitBackend` directly.** beadie-cranelift
  is a reference implementation, not a dependency — neither rayzor
  (`BeadieJit`) nor zyntax (`ZyntaxCraneliftBackend`) uses its concrete
  backend. `AshCraneliftBackend` should own its own
  `Arc<Mutex<cranelift_jit::JITModule>>` and `FunctionDef`, depending only on
  the cranelift-agnostic `beadie-core` / `beadie-backend` traits. This also
  frees ash to pick its own cranelift version: while ash consumes
  beadie-cranelift's `CraneliftFunctionDef`, both crates must resolve to the
  same cranelift or the `ir::Function` types are distinct.
- **Move to cranelift from wasmtime upstream** (git rev, 0.136.0-dev — the
  crates.io maximum is 0.134.3). Verify the flag names the tier sets survive
  the bump: `opt_level`, `enable_probestack`, `preserve_frame_pointers`,
  `is_pic`.
