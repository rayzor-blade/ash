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

What works: the language, the standard library, exceptions (a `try` is a
wasm exception handler, a throw a `longjmp`), `sys.thread` threads that block and resume inside the module
(`ASH_WASM_FIBERS=1`), real parallel threads via Workers or wasmtime
threads, sockets through host imports. Native `.hdll` files do not load; a
library ships a `.wasm` side module instead ([hdlls.md](hdlls.md)), and Haxe
code that needs a native library guards it with `#if wasm`.

## Running the result

The module is a library, not a command. It exports `main` and
`ash_module_init` and imports what a sandbox cannot do for itself. Something
has to instantiate it and answer those imports — a **host**.

Two hosts ship with ash, both in `crates/ash_wasm_runtime`:

- **`ash run game.wasm`**, a wasmtime host built into `ash`, runs the module
  from a terminal. The conformance suite runs the same host as the
  `ash-wasm-run` binary, which starts threads only when given `--threads`.
- **The browser host** (`crates/ash_browser`): the same contract behind a
  WASI shim, with Workers for threads and WebSocket for the socket client
  half. A wasm build writes a page for it beside the module, and
  `ash serve <dir>` serves that directory with the headers a threaded module
  needs. `examples/browser/README.md` explains the page.

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

## SIMD

Modules are built with the SIMD128 proposal on, so vector code -- the
`ash-simd` value types, and loops the optimiser widens -- runs as v128
instructions. Every current engine supports it (wasmtime, V8, SpiderMonkey,
JavaScriptCore). `ASH_WASM_SIMD=0` at build time leaves it off for an engine
that does not; the same code then runs lane by lane.

## Threads

`sys.thread` on wasm is built from two mechanisms, and a program gets real
parallelism in a browser by using both:

- **Agents** (`--target wasm32-wasip1-threads`): each Haxe thread is another
  instance of the same module over shared memory, on an agent of its own — a
  Worker in a page, an OS thread under wasmtime — and they run at the same
  time on separate cores.
- **Fibers** (`ASH_WASM_FIBERS=1` at build time): a thread blocked in
  `Deque.pop(true)`, a lock or `Sys.sleep` suspends inside the module and
  resumes where it stopped. Every agent has its own instance, so its own
  fiber state and its own scheduler; fibers on different agents suspend and
  resume independently of each other.

Built with both, Haxe threads are fibers spread over agents: one agent per
thread for as many agents as the host gives, in parallel, each able to block
without stopping the others. A thread created when no agent is free runs as a
fiber on the main scheduler and takes turns there. `examples/browser/` runs
four threads this way in a page, one Worker each.

| build | threads run | a blocking thread |
|---|---|---|
| `wasm32-wasip1` | one at a time, each body run to completion | cannot block: waiting on another thread hangs |
| `wasm32-wasip1` + fibers | taking turns on one agent | suspends; the others run |
| `wasm32-wasip1-threads` + fibers | in parallel, one agent each | suspends; the others run |

A page needs cross-origin isolation (COOP/COEP) for shared memory, and its
Workers must be created before the program starts, because a Worker created
from inside a synchronous wasm call never loads. The number of Workers the
page starts is the number of threads that run in parallel.

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
