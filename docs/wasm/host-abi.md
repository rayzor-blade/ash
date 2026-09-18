# Hosting a wasm build

A module built by `ash --build --target wasm32-wasip1` imports what a
sandbox cannot do for itself. This is the contract a host implements.
`ash wasm --validate module.wasm` lists what a given module still needs.

## The runtime crate

`crates/ash_wasm_runtime` has three parts, split by which side of the module
boundary each sits on:

- **`guest`** is compiled into the program. `ash_std` depends on it for wasm,
  so its contents are ordinary linkage, not imports. Everything a sandbox can
  do by itself lives there: WASI already supplies a clock, randomness, stdout
  and a filesystem.
- **`native`** is a wasmtime host, shipped as the `ash-wasm-run` binary. No
  browser, no JavaScript, no wasm-bindgen. The conformance suite runs on it.
- **`browser`** is the same contract behind a WASI preview-1 shim.

A host in another language implements the same imports.

## The one required import

`env.ash_host_fiber_yield`. A wasm module has no addressable call stack and
no instruction that switches between two, so suspending a fiber is the one
operation it cannot perform for itself. wasmtime answers it with its own
fibers; a page answers it with JSPI, or the module is built with
`ASH_WASM_FIBERS=1` and suspends without engine support.

Two companions when fibers are on: `ash_host_fiber_state` and
`ash_host_fiber_arm`, through which the host reads and writes the transform's
globals on the guest's behalf. `arm` also swaps the shadow stack and returns
the pointer it replaced.

## Sockets

WASI preview 1 has no usable sockets, so the guest asks the host for all of
it. Every argument and result is an `i32`; pointers are guest addresses the
host reads through the exported memory.

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

Descriptors are the host's own namespace from 0; they never meet a WASI fd.
`ip` is an `s_addr` in wire order, `port` in host order. Errors are WASI
preview-1 errno numbers, the one numbering both sides share; the guest maps
`AGAIN`/`ALREADY`/`INPROGRESS` to the -1 `sys.net.Socket` reads as
`Blocked` and everything else to -2.

`poll` takes 8-byte records `{ fd: i32, events: u16, revents: u16 }` with
ash's own bits (`RD` 1, `WR` 2, `PRI` 4, `ERR` 8, `HUP` 16, `NVAL` 32),
because `POLLIN` differs between Darwin and Linux.

`ash-wasm-run` implements all twelve over libc, evaluating readiness with
`select(2)` rather than `poll(2)` so a peer-closed stream reads as
"readable, writable, not exceptional" on Darwin as well as Linux. On Windows
all twelve return `NOTSUP`. The browser host implements the client half over
WebSocket and refuses `bind`, `listen`, `accept` and `name`; WebSocket
delivers whole messages, so they are queued and drained by byte count.

Not implemented: datagram addressing, name resolution.

## Everything else in `env`

Names beginning `ash_host_` are host imports; `env` resolves against the
program's own exports first and the host second, so a library's imports
under that prefix are never demanded of the program. `ash wasm module.wasm`
prints the exact set a module needs. The native and browser hosts each
answer what they can and refuse the rest by name at instantiation.

## Threads

A Haxe thread that must run in parallel is another instance of the same
module over shared memory (`env.memory`, imported and shared). The host
supplies the agent — a Worker in a page, an OS thread under wasmtime — through
an installable spawn hook, because which agent is safe to ask depends on who
is asking: a Worker that blocks after spawning a child leaves that child
unstarted. Each instance has its own WASI context, socket table and loaded
libraries. Details and measurements: [internals/wasm-threads.md](../internals/wasm-threads.md).

## Why not web-sys

`web-sys` is generated from WebIDL and supplies the DOM, WebGL and `Worker`;
the runtime wants operating-system services, which is WASI. It also rides on
wasm-bindgen and `wasm32-unknown-unknown`: no libc, no clock, no files, no
`setjmp`, which is the trap model. A browser host written in Rust may use
`web-sys` freely on its own side of the imports.
