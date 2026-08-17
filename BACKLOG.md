# ash — Development Backlog

Known gaps, deferred refinements, and open defects. Code docs describe what the
code guarantees today; anything that should change lives here.

**Last updated**: 2026-08-17

**Status**: Heaps Base2D renders through the real init path under the
interpreter (`ash_cli --mode interp examples/heaps_base2d/bin/game.hl`), with
tiered promotion brokered by beadie, thread-safe GC, and a 0.05s tiered
pre-warm. The full-JIT-everything path (`ash <file>.hl`) still has one open
crash; see [JIT & tiering](#jit--tiering).

---

## AIR v2

The typed phi-SSA IR (`crates/air/src/v2`) is in place with lowering,
verification, and a serializer back to standard HL bytecode. v1 remains the
production path until the backends switch over.

- **Native import declarations in the IR builder** — a module-level
  `NativeImport { lib, name, signature, findex }` table populated at lowering
  and consumed by every backend (LLVM `declare`, Cranelift `import_function`,
  symbol-table binding). Prerequisite for any backend consuming v2, because
  Cranelift requires imports declared before the function builder exists.
- **Optimization passes over the typed IR**, in payoff order established by
  the mandelbrot trace: static field-offset resolution, field-load GVN with
  HL alias classes (`(type, field-slot)`, varray data/length, enum params,
  globals), inlining + escape analysis / scalar replacement, bounds-check
  LICM, dominance-based null-check elimination, box/unbox forwarding.
- **JIT lowers from v2** instead of raw opcodes — the point at which
  malformed-IR crashes and verifier rejections become structurally impossible
  rather than blacklisted.
- **Liveness-refined trap-region pinning** — every register written inside a
  trap region is currently pinned to a cell. Liveness would narrow this to
  registers actually live into the handler.
- **Landing-pad design for exceptional-edge copies** — would lift the
  handler-block non-trivial-phi restriction.
- Auto-vectorization is deliberately *not* planned here: with de-abstraction
  done at the AIR level, LLVM's loop/SLP vectorizers handle it.

## JIT & tiering

- **Promotions do not stick on `game.hl`** — promoted code calls through
  shared `functions_ptrs` / vtable / closure slots that still hold interpreter
  stub sentinels (`findex + 1`, always `< 0x100000`), faulting at
  `fault_addr = findex + 1`. Needs stub guards at every JIT indirect call
  site, dispatching to interpreter re-entry (the `hlp_set_closure_runner`
  bridge) instead of calling the sentinel.
- **`ASH_JIT_NATIVE_TRAPS` defaults off** — unresolved natives compile to
  call-time trap stubs (upstream `disabled_primitive` semantics) which yields
  the first successful `game.hl` promotions, but must stay opt-in until the
  stub guards above land.
- **Missing std natives** referenced by Heaps: `hlp_string_compare`,
  `hlp_alloc_enum_dyn`, `hlp_type_super`, `hlp_type_enum_eq`,
  `hlp_sys_exe_path` (`hlp_haptic_close` can be a no-op). The interpreter
  never notices because it resolves lazily; the JIT resolves eagerly.
- **`FunctionLookupError` blacklist class** — functions reachable only
  indirectly are never code-generated before MCJIT finalization, so their
  addresses cannot be looked up. Affects ~9 findexes on `game.hl`.
- **Full-JIT `game.hl` crashes** in LLVM `SimplifyCFG` during MCJIT
  finalization after compiling ~5000 functions (malformed-IR family). Tiered
  promotion sidesteps it; AIR v2 lowering should remove the cause.
- **`SIGABRT` in `hl_to_virtual`** (`std/src/obj.rs`) surfaces after
  promotions land. Pre-existing, reproduces on baseline builds.
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
  jump exits the frame.
- **Cross-tier FMA policy** — Cranelift at `speed` never contracts
  `fmul`+`fadd` (matching hxcpp: mandelbrot checksum 112798515), while the
  LLVM tier contracts (112798587), so promoting a hot float function changes
  bit patterns mid-run. Either drop the contract flag or emit explicit `fma`
  in both tiers.
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
- **Single shared symbol table for all tiers** — one canonical
  name → address map consumed by the interpreter's `call_native`, the LLVM
  tier's native resolution, and Cranelift's build-time `JITBuilder::symbol`.
- **Per-call `dlsym` and `getenv` on the interpreter's hot path** — sampling
  put `getenv` alone at roughly a quarter of interpretation time. Resolve
  once into the table; gate every env read behind `OnceLock`.
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
