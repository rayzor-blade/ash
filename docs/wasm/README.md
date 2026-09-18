# The wasm target

```sh
ash --build game.wasm --target wasm32-wasip1 game.hl
```

ash compiles HL bytecode to a WebAssembly module through the same AIR and
AOT pipeline as the native target: AIR → LLVM IR → wasm32 object → link.
The linker is built into `ash`, and the libc was joined into the runtime
object at build time, so the command above needs no other toolchain. The
target is `wasm32-wasip1`; a browser runs the same core module through a
WASI preview-1 shim.

What works: the language, the standard library, exceptions (`setjmp`-based,
as natively), `sys.thread` threads that block and resume inside the module
(`ASH_WASM_FIBERS=1`), real parallel threads via Workers or wasmtime
threads, sockets through host imports. Native `.hdll` files do not load; a
library ships a `.wasm` side module instead ([hdlls.md](hdlls.md)), and Haxe
code that needs a native library guards it with `#if wasm`.

## Running the result

The module is a library, not a command. It exports `main` and
`ash_module_init` and imports what a sandbox cannot do for itself. Something
has to instantiate it and answer those imports — a **host**.

Two hosts ship with ash, both in `crates/ash_wasm_runtime`:

- **`ash-wasm-run`**, a wasmtime host. `ash-wasm-run game.wasm` runs the
  module from a terminal; the conformance suite runs on it. wasmtime's own
  fibers answer the suspending import.
- **The browser host** (`crates/ash_browser`, `examples/browser/`): the same
  contract behind a WASI shim, with Workers for threads and WebSocket for
  the socket client half. `examples/browser/README.md` walks through
  serving a module.

Writing your own host: [host-abi.md](host-abi.md) is the contract — the
imports, the socket API, and the one import every host must supply.

## Inspecting a module

```bash
ash wasm game.wasm             # functions, indirect call sites, tables, exports, imports
ash wasm --validate game.wasm  # exit non-zero and name what a host would still have to supply
```

Imports are grouped by who answers them: the program itself, WASI, or the
host. `ash wasm` uses ash's own parser, so a build machine needs nothing
installed.

## Size

Only reachable code is emitted — about a third of a module is dropped.
Reachability is generous on purpose: all data is kept, so any function whose
address appears in data survives, because compiled code reaches most of the
runtime through tables built in data. Debug sections are dropped.

A hello world is a few megabytes, nearly all of it runtime. A library
compiled into the runtime is in every module whether used or not, which is
why SQLite became a side module (1.9 MB out of a 3.96 MB hello world).

## Threads

`sys.thread` on wasm has two modes:

- **Fibers** (`ASH_WASM_FIBERS=1` at build time): threads are cooperative,
  on one agent. A worker blocked in `Deque.pop(true)` suspends inside the
  module and resumes where it stopped when the main thread pushes. No
  parallelism; the whole `threads` suite passes.
- **Workers / wasmtime threads**: each Haxe thread is another instance of the
  same module over shared memory, and they run at the same time. A page
  needs cross-origin isolation (COOP/COEP) for shared memory, and Workers
  must be created before the program starts because a Worker created from
  inside a synchronous wasm call never loads.

Design and measurements: [internals/wasm-fibers.md](../internals/wasm-fibers.md),
[internals/wasm-threads.md](../internals/wasm-threads.md).

## Conformance

1,186 of the 1,195 Haxe suite cases are in scope for wasm and all pass. The
nine out of scope: eight need the `fmt` HDLL's compression and hashing
primitives, and `unit.spec.sys.net.TestSocket` passes only on a host that
implements the socket imports.

## Not this

- Not a wasm interpreter for HL bytecode. It emits compiled wasm.
- Not a way to load native `.hdll` files in a sandbox.
- Not a replacement for the native tiers; the same program runs natively
  through the interpreter, the JIT or an AOT binary.

## Internals

The pieces that make this work — the 32-bit ABI, the setjmp lowering and the
LLVM patch it needs, the fiber transform, shared memory — are contributor
material under [docs/internals/](../internals/).
