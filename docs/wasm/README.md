# The wasm target

ash compiles HL bytecode to a `.wasm` module through the same AIR and AOT
pipeline the native target uses. `wasm32-wasip1` is the target; a browser runs
the same core module through a WASI preview-1 shim.

The route is **AIR → LLVM IR → wasm32 object → WASI link**. LLVM's WebAssembly
backend supplies structured control flow and function-table lowering, so a
direct AIR→wasm backend would add CFG structuring, instruction selection, ABI
lowering, relocations and debug metadata while leaving every runtime problem
below untouched.

Native `.hdll` files cannot load in a sandbox. A wasm build rejects non-`std`
natives; framework authors guard them with `#if wasm` or supply a host import.

## The documents

| | |
|---|---|
| [abi.md](abi.md) | target layouts, relocations, and how a call is made |
| [exceptions.md](exceptions.md) | setjmp as a codegen mode, and the LLVM gap ash patches |
| [host-abi.md](host-abi.md) | what a host must implement: the runtime crate and the socket imports |
| [fibers.md](fibers.md) | the link-time transform that suspends a fiber with no engine feature |
| [threads.md](threads.md) | shared memory, Workers, and how ash's threads work here |
| [hdlls.md](hdlls.md) | native libraries as `dylink.0` side modules |

## Building and inspecting

`ash --build game.wasm --target wasm32-wasip1 game.hl` does the whole thing;
[aot.md](../aot.md#webassembly) covers it. `ash-wasm-run` runs the result.

`ash wasm prog.wasm` reports functions, indirect call sites, tables, exports
and imports grouped by whether a host can supply them. `ash wasm --validate`
exits non-zero and names what is missing. It uses ash's own parser, so a build
machine needs nothing installed.

## Rebuilding `ash_runtime.o`

`ash_runtime.o` is ash_std, wasi libc and libsetjmp joined into one relocatable
object. Nothing rebuilds it automatically, so it goes stale the moment ash_std
gains an export `ash_module_init` calls — the module links, then fails at
instantiate with `unknown import: env::<name>`.
`crates/ash/tests/wasm_runtime_fresh.rs` fails first, naming the symbol.

```bash
cargo rustc -p ash_std --target wasm32-wasip1 --release --crate-type staticlib

rust-lld -flavor wasm -r -o target/release/wasm32-wasip1/ash_runtime.o \
  --whole-archive target/wasm32-wasip1/release/libash_std.a --no-whole-archive \
  -L$(brew --prefix wasi-libc)/share/wasi-sysroot/lib/wasm32-wasip1 -lc -lsetjmp
```

`--no-whole-archive` is load-bearing. Without it libc's `crt1` and the
long-double `printf` are force-included, the module imports `__main_argc_argv`
and `__multc3`, and there is no wasm compiler-rt to satisfy the latter. A
correct object is about 6.35 MB; the broken one was 6.89 MB.

Two toolchain requirements: `rust-lld` must be no older than the installed
wasi-libc, or it fails on the linker-defined `__wasm_first_page_end`; and the
engine needs the exceptions proposal (`wasmtime -W exceptions`, or
`Config::wasm_exceptions`, which `ash-wasm-run` sets).

## GC roots are the open correctness problem

Linear-memory allocation is easy; root discovery is not. The collector scans
native stacks and callee-saved registers conservatively, but WebAssembly locals
and operand-stack values are not addresses in linear memory, so scanning the
LLVM shadow stack finds only spills and address-taken values.

The work is explicit roots for pointer-bearing AIR values, plus scoped roots
for raw pointers Rust holds across an allocating call. Optimisation must not
promote a live pointer out of the root set, and "works with optimisation off"
is not proof — the backend may still place values in wasm locals.

krio supplies the rendezvous half: `cluster.stop_the_world(agent, || ...)`
guarantees no other agent is inside a task step. krio decides *when* it is safe
to scan; ash still decides *what*.

## Threads and fibers

A wasm module cannot switch its own stack, so `std/src/fiber_host.rs` routes
the one operation that must suspend to `ash_host_fiber_yield`. There are three
ways to implement that and they trade differently. [threads.md](threads.md)
has the comparison, what krio built under the worker row, and how ash's own
threads work here. [fibers.md](fibers.md) is the transform that needs no engine
feature at all.

## Conformance

**1,186 of 1,195 cases are in scope, 99.2%.** A case can run on wasm if every
native it calls can, which is observable without a wasm runtime: run it under
the interpreter with `ASH_TRACE_NATIVE=1` and take the union.

Two subtractions decide the answer. The suite's own startup is not the case —
running a nonexistent case name gives a 38-native baseline including
`hlp_ssl_init` and `hlp_socket_init`, and counting those against every case
excludes every case. And a mutex is not a thread: `hlp_mutex_*`, `hlp_lock_*`,
thread-locals and atomics are all implementable single-threaded. Treating all
109 natives of `thread.rs` as impossible put the answer at 10.5%.

The nine out of scope: eight need the `fmt` HDLL (compression and hashing, not
language semantics — they return the day `fmt`'s primitives are provided by the
wasm build), and `unit.spec.sys.net.TestSocket` passes only on a host that
implements the socket imports. Report against 1,186 with these named, never
quietly dropped.

## What this is not

- Not a wasm interpreter for HL. It emits compiled wasm.
- Not a replacement for the interpreter, Cranelift, LLVM JIT or native AOT.
- Not a way to load native `.hdll` files in a sandbox.
