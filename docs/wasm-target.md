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

## Building and inspecting

`ash --build game.wasm --target wasm32-wasip1 game.hl` does the whole thing;
[aot.md](aot.md#webassembly) covers it. `ash-wasm-run` runs the result.

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

## Target ABI

`TargetAbi` (`crates/ash/src/target_abi.rs`) is chosen before anything is
decoded. It carries the triple, pointer width, every HashLink layout derived
from that width, and the target's capabilities. `BytecodeDecoder::decode_for_abi`
takes it, so enum offsets are computed for the target rather than inherited
from the compiler process; lowering asks it for field offsets and array element
sizes; the module's triple and data layout are set before a body is emitted.

`cargo test -p ash --test wasm_target` fails if an import outside the runtime's
own surface appears.

**No relocation reaches an undefined data symbol**, and `--allow-undefined`
covers functions only. `ash_fiber_poll_epoch` — the loop safepoint's word —
was referenced directly and broke the link. Generated code reaches it through a
pointer `ash_late_init` fills from the runtime's getter, the same indirection a
Mach-O dylib needs. `TargetAbi` records this as `direct_data_relocations`.

Still host-derived, and each is a bug waiting on a 32-bit target: AOT constants
assuming an eight-byte object header, AOT helper signatures using `i64` where
the runtime takes `usize`, AOT data reading `hl_runtime_obj` offsets with host
`offset_of!`, `RefData` using the host `size_of::<varray>()`, and static
closure emission assuming a 32-byte `vclosure` with `stackCount`.

## setjmp is a codegen mode, not a link flag

WASI's `libsetjmp.a` does not define `_setjmp`/`longjmp`. It defines
`__wasm_setjmp`, `__wasm_setjmp_test` and `__wasm_longjmp` — the forms LLVM's
WebAssembly SJLJ pass rewrites a `setjmp` call into. So both halves need
`-wasm-enable-sjlj` **and** `+exception-handling`; the backend refuses one
without the other.

**An object with only half the rewrite links and runs.** Ours referenced
`__wasm_setjmp` and not `__wasm_setjmp_test`, so the program was correct until
it threw, and then printed "thrown Wasm exception" instead of catching.

The missing half cannot be passed as a flag. Expressing the catch side needs
the target machine's *exception model* to be `wasm`, because `TargetPassConfig`
adds `LowerInvoke` whenever the asm info reports no exception handling, and
that pass deletes the `invoke`s the rewrite just created. `llc` takes
`-exception-model=wasm`; the LLVM C API has no equivalent, so neither llvm-sys
nor inkwell does. The WebAssembly target tries to infer it, but
`basicCheckForEHAndSjLj` runs after `initAsmInfo()`, so the asm info keeps
`ExceptionHandling::None` for life.

ash therefore carries one C++ translation unit,
`crates/ash/cpp/wasm_exception_model.cpp`, setting both fields after
construction. Fifteen lines. Without it a wasm build is refused rather than
emitted wrong.

ash also passes `-wasm-use-legacy-eh=false`: LLVM still defaults to the
withdrawn `try`/`catch`, which no current engine accepts.

The regression test asserts on *both* halves of the lowering, because the
broken form is the one that looks fine.

## A function pointer is a small integer here

The tiered runtime names a not-yet-compiled body with a `findex + 1` sentinel
and tells it from real code by magnitude — below `0x100000` is a sentinel,
because no native code address is. On wasm a function pointer is a table index
in the low hundreds, so every real function answered the test. `hlp_call_method`
handed one to the closure runner, which called back into `hlp_call_method`,
recursing until the shadow stack wrapped — surfacing as an out-of-bounds access
at `0xffffffb0`.

One place asks that question now, `fiber::is_stub_sentinel`, and it answers no
on wasm: nothing there creates a sentinel, since the target has no interpreter
and no tiers. Whether a code pointer can be a small integer is a property of
the target, not of what the host installed — an earlier version keyed on
whether the stub resolver was installed, and `--mode jit` creates sentinels
without one.

## Dynamic calls use trampolines, not registers

`ash_static_call` places values in registers and jumps. WebAssembly has no
registers and checks the signature of every indirect call, so a call whose
shape is known only at run time cannot be assembled.

The compiler sees every function type in the program, so it emits one
trampoline per distinct signature — `(fun, args, out) -> ptr`, unpacking
arguments, making one statically-typed call, storing the result — registered
under a key computed from the `hl_type`
(`crates/ash/src/llvm/aot_trampoline.rs`). A miss reports its key and argument
count rather than guessing.

Three things that found: a method's closure form is not in the type table, so
the emitter registers the `hlp_get_closure_type` shape as well; registration
belongs in `ash_late_init`, not `emit_module_init`, which runs during `build`
before trampolines exist; and the count is a `usize` — declaring it `i64` gives
`rust-lld: warning: function signature mismatch`, which is a warning and a
corrupt call.

## The host crate

`crates/ash_wasm_runtime`, in three parts, and which side of the module
boundary each sits on is the design.

**`guest`** is compiled *into* the program — `ash_std` depends on it for wasm —
so its contents are ordinary Rust linkage, not imports. Everything doable
inside the sandbox belongs there, and most is, because WASI supplies a clock,
randomness, stdout and a filesystem.

**`native`** is a wasmtime host and is what the conformance lane uses: no
browser, no JavaScript, no wasm-bindgen. wasmtime's own fibers answer the
suspending import.

**`browser`** is the same contract behind a WASI preview-1 shim.

One import crosses the boundary and must: `env.ash_host_fiber_yield`. A wasm
module has no addressable stack and no instruction that moves between two, so
suspension is the one operation it cannot perform for itself.

### Why not `web-sys`

It supplies the wrong things — `web-sys` is generated from WebIDL (DOM, WebGL,
`Worker`), while the runtime wants operating-system services, which is WASI.
And it rides on wasm-bindgen, whose target is `wasm32-unknown-unknown`: no
libc, no clock, no stdout, no files, and no `setjmp`, which is the trap model.
The result would run in a browser and nowhere else, so no wasmtime, no CI lane,
no server embedding.

What a browser adds beyond WASI — JSPI, `Worker` + `SharedArrayBuffer`, WebGL —
is something a *host* provides, and the module already has an interface for
each. A browser host written in Rust may use `web-sys` freely, on the other
side of those imports.

## Sockets: twelve host imports

WASI preview 1 has no usable sockets: it names four calls and nothing that
creates, connects, binds, listens, resolves or waits, and under wasmtime those
four answer `ENOTSOCK`. Rust's `std::net` compiles here and answers
`Unsupported` to everything, `-S inherit-network` included.

So the guest asks the host for all of it. Every argument and result is an
`i32`; pointers are guest addresses the host reads through exported memory:

```
env.ash_host_socket_open(udp)                -> fd >= 0            | -errno
env.ash_host_socket_connect(fd, ip, port)    -> 0                  | errno
env.ash_host_socket_bind(fd, ip, port)       -> 0                  | errno   host sets SO_REUSEADDR
env.ash_host_socket_listen(fd, backlog)      -> 0                  | errno
env.ash_host_socket_accept(fd)               -> fd >= 0            | -errno
env.ash_host_socket_send(fd, buf, len)       -> bytes >= 0         | -errno
env.ash_host_socket_recv(fd, buf, len)       -> bytes > 0, 0 = EOF | -errno
env.ash_host_socket_shutdown(fd, how)        -> 0                  | errno   how: 1 read, 2 write
env.ash_host_socket_close(fd)                -> 0                  | errno
env.ash_host_socket_name(fd, which, out)     -> 0                  | errno   which: 0 local, 1 peer
env.ash_host_socket_set(fd, opt, value)      -> 0                  | errno   opt: 0 blocking, 1 NODELAY, 2 BROADCAST, 3 timeout ms
env.ash_host_socket_poll(fds, nfds, timeout) -> ready >= 0         | -errno  timeout ms, negative waits
```

Descriptors are the host's own namespace starting at 0; they never meet a WASI
fd. `ip` is an `s_addr` in wire order, `port` in host order. Errors cross as
WASI preview-1 errno numbers, the one numbering both sides agree on. The guest
turns `AGAIN`/`ALREADY`/`INPROGRESS` into the -1 `sys.net.Socket` reads as
`Blocked`, everything else into -2.

`poll` takes 8-byte records `{ fd: i32, events: u16, revents: u16 }` with ash's
own bits (`RD` 1, `WR` 2, `PRI` 4, `ERR` 8, `HUP` 16, `NVAL` 32), because
`POLLIN` differs between Darwin and Linux and a pass-through would be right on
one kernel and wrong on the next.

`ash-wasm-run` implements all twelve over `libc`. It evaluates readiness with
`select(2)`, not `poll(2)`: on Darwin `poll` reports a stream whose peer closed
as `POLLIN|POLLPRI|POLLHUP` and not writable, while `select` — and the unix
runtime, and the Haxe suite — say readable, writable, not exceptional. On
Windows all twelve answer `NOTSUP`.

The browser host implements the client half over WebSocket and refuses `bind`,
`listen`, `accept` and `name`, since a page cannot listen. WebSocket delivers
whole messages while `recv` hands back bytes, so messages are queued whole and
drained by count.

Not implemented: datagram addressing and name resolution.

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
ways to implement that and they trade differently — see
[`wasm-threads.md`](wasm-threads.md) for the comparison, what krio built under
the worker row, and how ash's own threads work on this target.
[`wasm-fibers.md`](wasm-fibers.md) is the transform that needs no engine
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
