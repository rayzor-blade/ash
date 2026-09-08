# What a host must implement

A wasm build asks its embedder for everything a sandbox cannot do itself.
This is that contract. [README.md](README.md) is the overview.

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
