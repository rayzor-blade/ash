# ash-channel

Byte messages between an Ash wasm program and a page agent.

A program in a browser runs in a Worker that never returns to its own event
loop, so a WebSocket opened there never connects or delivers. The page's thread
has a live event loop. `ash-channel` shares a block of the program's memory with
an agent on that thread; the agent owns the connection, and the program sends
and receives whole messages through two rings in the block.

```haxe
final channel = ash.channel.Channel.open("my_agent");
channel.send(Bytes.ofString("hello"));
final reply = channel.recv();   // parks the fiber until a message arrives
channel.close();
```

## Requirements

- A **shared-memory build**: `ash --build out.wasm --target wasm32-wasip1-threads`.
  `ASH_WASM_FIBERS=1` is not required.
- A **cross-origin-isolated page** (`Cross-Origin-Opener-Policy: same-origin` and
  `Cross-Origin-Embedder-Policy: require-corp`). `ash serve` sends both.
- `ash_channel.wasm` beside the output, so `ash --build` lists it.
- The agent's file on the page: `<name>.mjs`, beside the page.

`Channel.open` throws when the runtime cannot hand the block to the page.
The named agent file must be deployed; transport failures then close the
channel and wake pending receivers.

## Build

```sh
scripts/build_wasm_channel.sh          # target/wasm-channel/ash_channel.wasm and channel-agent.mjs
```

Needs nightly Rust with `rust-src` and the `wasm32-wasip1-threads` target. The
example in `examples/ring-echo` builds and runs the whole chain:

```sh
examples/ring-echo/build.sh            # Haxe client, side module, wasm, page
node examples/ring-echo/run-chrome.mjs # headless Chrome check; or: ash serve examples/ring-echo/out
```

In a Haxe build, use `-cp haxelib/ash-channel -cp haxelib/ash-future` (or
`-lib ash-channel` with the library registered).

## The page's side

The page imports `./<name>.mjs` and calls its `start({ memory, address })`. That
file hands the block to `serve` from `channel-agent.mjs` with a transport:

```js
import { serve } from "./channel-agent.mjs";
export const start = (ask) => serve(ask, {
  open({ name, deliver, closed }) {
    const socket = new WebSocket("wss://play.example/session");   // any URL, any protocol
    socket.binaryType = "arraybuffer";
    socket.onmessage = (e) => deliver(new Uint8Array(e.data));    // a frame from the peer
    socket.onclose = () => closed("peer closed");
    return new Promise((resolve, reject) => {
      socket.onopen = () => resolve({ send: (frame) => socket.send(frame), close: () => socket.close() });
      socket.onerror = () => reject(new Error("could not connect"));
    });
  },
});
```

`open` may return the connection or a promise of it. Messages the program sends
before it resolves are held in the ring. `deliver` takes one message per call,
as a `Uint8Array`; `closed(reason)` once, when the connection is gone. A
message is opaque: the program's `send` bytes arrive at `send(frame)` unchanged,
and what `deliver` is given is what `recv` returns. Alternatively, leave
`<name>.mjs` as `export { start } from "./channel-agent.mjs"` and register
`globalThis.ashChannels = { my_agent: transport }`.

## Behaviour

- `send` waits (sleeping 1 ms at a time) while the ring is full, throws
  `ChannelClosed` after a close, and throws a string for a message that can
  never fit: one message may take at most half a ring less 8 bytes, capped at 16777204
  bytes by the record header. Oversized incoming messages close the channel.
- `recv` returns the oldest message not yet taken; with none, the current fiber
  parks until one arrives. After the connection closes, messages already
  received can still be taken, then `recv` throws `ChannelClosed`.
- `capacity` (default 65536) is the size of each ring in bytes: a power of two
  from 4096 to 67108864.
- `isOpen` is false until the page reports the connection open; `send` before
  then just queues.

## The block and the rings

One block of the program's shared memory, 8-byte aligned: a 128-byte header,
the event ring (page to program), the command ring (program to page). Header
words are little-endian `u32`; the layout is in
`crates/ash_hdll_channel/src/ring.rs`.

A ring has one writer and one reader. `head` and `tail` are byte counters that
only grow (modulo 2^32); a position is the counter modulo the capacity. A writer
stores a record, then publishes it with an atomic store of the new head; the
reader loads the head atomically, reads, and stores the new tail. A record is a
`u32` header, `kind | (length << 8)` (length is the whole record, a multiple of
4, under 2^24), then the payload; it never wraps: the writer pads the rest of
the ring with a kind-0 record first. Kinds: 0 pad, 1 message, 2 opened (page to
program), 3 closed (either way).

A message record's payload is a `u32` byte length, then that many bytes. The
page adds one to the program's wake word and notifies it after it publishes, so
a program asleep in its scheduler wakes at once (`ash_host_watch`); the program
notifies the command head after it publishes (`Atomics.waitAsync` on the page).
