// The page's half of an ash-channel: serve a block of the program's shared
// memory with a transport the page provides.
//
// The program (ash_channel.wasm) allocates the block and asks the page for an
// agent by name through `ash_host_agent`; the page imports `./<name>.mjs` and
// calls its `start({ memory, address })` on the page's thread, where the event
// loop is alive and a WebSocket can open. That file is three lines:
//
//     import { serve } from "./channel-agent.mjs";
//     export const start = (ask) => serve(ask, myTransport);
//
// or, with no file of its own beyond a re-export, `export { start } from
// "./channel-agent.mjs"` and the page registers its transport under the agent
// name: `globalThis.ashChannels = { name: transport }`.
//
// A transport is `{ open(connection) }` (or a function taking `connection`),
// returning, or resolving to, `{ send(frame), close() }`. `connection` is
// `{ name, deliver(frame), closed(reason) }`: call `deliver` with each message
// from the peer as a Uint8Array, and `closed` once when the connection is gone.
// A frame is whatever the program sent or will receive; this file never looks
// inside one. The layout of the block and its two rings is in
// crates/ash_hdll_channel/src/ring.rs.

const MAGIC = 0x43485341; // "ASHC"
const HEADER = 128;
// Word indices (byte offset / 4) of the header.
const EVENT_CAP = 2, COMMAND_CAP = 3, EVENT_HEAD = 4, EVENT_TAIL = 5;
const COMMAND_HEAD = 6, COMMAND_TAIL = 7, DROPPED = 8, ATTACHED = 9;
const WAKE = 10, FLAGS = 11, NAME_LEN = 12;
const NAME_AT = 56;
const FLAG_OPEN = 1, FLAG_CLOSED_BY_TRANSPORT = 2, FLAG_CLOSED_BY_PROGRAM = 4;
const PAD = 0, MESSAGE = 1, OPENED = 2, CLOSED = 3;

export function serve({ memory, address }, transport) {
  let i32, u8;
  const refresh = () => {
    if (u8?.buffer !== memory.buffer) {
      i32 = new Int32Array(memory.buffer);
      u8 = new Uint8Array(memory.buffer);
    }
  };
  refresh();
  const w = address >>> 2;
  if (!(memory.buffer instanceof SharedArrayBuffer) || !Number.isInteger(address)
      || address < 0 || (address & 7) !== 0 || address + HEADER > u8.length
      || i32[w] !== MAGIC || i32[w + 1] !== 1) {
    throw new Error("not an ash-channel block");
  }
  const eventCap = i32[w + EVENT_CAP] >>> 0;
  const commandCap = i32[w + COMMAND_CAP] >>> 0;
  const eventRing = address + HEADER;
  const commandRing = eventRing + eventCap;
  const nameLength = i32[w + NAME_LEN] >>> 0;
  if (![eventCap, commandCap].every(cap => cap >= 4096 && cap <= 67108864 && (cap & (cap - 1)) === 0)
      || nameLength > 64 || commandRing + commandCap > u8.length) {
    throw new Error("invalid ash-channel block bounds");
  }
  const name = new TextDecoder().decode(u8.slice(address + NAME_AT, address + NAME_AT + nameLength));

  let connection = null;
  let finished = false;
  const backlog = []; // records the event ring had no room for
  let retry = null;

  const put32 = (at, value) => {
    u8[at] = value & 255;
    u8[at + 1] = (value >>> 8) & 255;
    u8[at + 2] = (value >>> 16) & 255;
    u8[at + 3] = (value >>> 24) & 255;
  };
  const get32 = (at) => (u8[at] | (u8[at + 1] << 8) | (u8[at + 2] << 16) | (u8[at + 3] << 24)) >>> 0;

  // Wake whoever sleeps on the program's wake word, as ash_host_watch asks.
  const wake = () => {
    refresh();
    const at = Atomics.load(i32, w + WAKE) >>> 0;
    if (at) {
      Atomics.add(i32, at >> 2, 1);
      Atomics.notify(i32, at >> 2);
    }
  };

  // Append one record to the event ring; false when it does not fit now.
  const write = (kind, payload) => {
    refresh();
    const total = (4 + payload.length + 3) & ~3;
    if (total > eventCap / 2 || total >= 2 ** 24) throw new Error("channel record too large");
    const head = Atomics.load(i32, w + EVENT_HEAD) >>> 0;
    const tail = Atomics.load(i32, w + EVENT_TAIL) >>> 0;
    const used = (head - tail) >>> 0;
    const pos = head % eventCap;
    const contiguous = eventCap - pos;
    const pad = total > contiguous ? contiguous : 0;
    const advance = pad ? contiguous + total : total;
    if (used + advance > eventCap) return false;
    if (pad) put32(eventRing + pos, (PAD | (pad << 8)) >>> 0);
    const at = eventRing + (pad ? 0 : pos);
    put32(at, (kind | (total << 8)) >>> 0);
    u8.set(payload, at + 4);
    u8.fill(0, at + 4 + payload.length, at + total);
    Atomics.store(i32, w + EVENT_HEAD, (head + advance) | 0);
    return true;
  };

  const flush = () => {
    let wrote = false;
    while (backlog.length && write(backlog[0][0], backlog[0][1])) {
      backlog.shift();
      wrote = true;
    }
    if (wrote) wake();
    if (backlog.length && retry === null) {
      // The program has not read enough yet; look again shortly.
      retry = setTimeout(() => {
        retry = null;
        flush();
      }, 2);
    }
  };

  const publish = (kind, payload) => {
    backlog.push([kind, payload]);
    flush();
  };

  const finish = (reason) => {
    if (finished) return;
    finished = true;
    Atomics.or(i32, w + FLAGS, FLAG_CLOSED_BY_TRANSPORT);
    const reasonBytes = new TextEncoder().encode(String(reason ?? ""));
    publish(CLOSED, reasonBytes.subarray(0, Math.min(eventCap / 2 - 4, 2 ** 24 - 8)));
    try { connection?.close(); } catch (_) {}
  };

  const deliver = (frame) => {
    if (finished) return;
    const bytes = frame instanceof Uint8Array ? frame : new Uint8Array(frame);
    if (bytes.length > Math.min(eventCap / 2 - 8, 2 ** 24 - 12)) {
      Atomics.add(i32, w + DROPPED, 1);
      finish("incoming message exceeds channel capacity");
      return;
    }
    const payload = new Uint8Array(4 + bytes.length);
    put32Into(payload, bytes.length);
    payload.set(bytes, 4);
    publish(MESSAGE, payload);
  };
  const put32Into = (array, value) => {
    array[0] = value & 255;
    array[1] = (value >>> 8) & 255;
    array[2] = (value >>> 16) & 255;
    array[3] = (value >>> 24) & 255;
  };

  // Take every record the program has published, oldest first.
  const drain = () => {
    refresh();
    if (Atomics.load(i32, w + FLAGS) & FLAG_CLOSED_BY_PROGRAM) {
      finish("closed by the program");
      return;
    }
    const head = Atomics.load(i32, w + COMMAND_HEAD) >>> 0;
    let tail = Atomics.load(i32, w + COMMAND_TAIL) >>> 0;
    while (tail !== head) {
      const pos = tail % commandCap;
      const header = get32(commandRing + pos);
      const kind = header & 255;
      const length = header >>> 8;
      if (((head - tail) >>> 0) > commandCap || length < 4 || length % 4
          || length > commandCap - pos || length > ((head - tail) >>> 0)) {
        finish("invalid channel command record");
        break;
      }
      if (kind === MESSAGE) {
        const n = get32(commandRing + pos + 4);
        if (length < 8 || n > length - 8) {
          finish("invalid channel message length");
          break;
        }
        const frame = u8.slice(commandRing + pos + 8, commandRing + pos + 8 + n);
        try {
          connection.send(frame);
        } catch (e) {
          finish(e && e.message ? e.message : e);
        }
      } else if (kind === CLOSED) {
        try {
          connection.close();
        } catch (_) {}
        finish("closed by the program");
      }
      tail = (tail + length) >>> 0;
      if (finished) break;
    }
    Atomics.store(i32, w + COMMAND_TAIL, tail | 0);
  };

  const run = async () => {
    while (!finished) {
      refresh();
      const seen = Atomics.load(i32, w + COMMAND_HEAD);
      drain();
      if (finished) break;
      if (Atomics.waitAsync) {
        const waiting = Atomics.waitAsync(i32, w + COMMAND_HEAD, seen);
        if (waiting.async) await waiting.value;
      } else {
        await new Promise((resolve) => setTimeout(resolve, 4));
      }
    }
  };

  Atomics.store(i32, w + ATTACHED, 1);
  return Promise.resolve().then(() => {
    const open = typeof transport === "function" ? transport : transport.open.bind(transport);
    return open({ name, deliver, closed: finish });
  }).then((opened) => {
    connection = opened;
    refresh();
    if (finished) {
      try { opened?.close(); } catch (_) {}
      return;
    }
    if (Atomics.load(i32, w + FLAGS) & FLAG_CLOSED_BY_PROGRAM) {
      finish("closed by the program");
      return;
    }
    if (!opened || typeof opened.send !== "function" || typeof opened.close !== "function") {
      throw new Error("channel transport must provide send and close");
    }
    Atomics.or(i32, w + FLAGS, FLAG_OPEN);
    publish(OPENED, new Uint8Array(0));
    void run().catch(finish);
    return { name, close: () => finish("closed by the page") };
  }).catch(e => finish(e?.message ?? e));
}

// The default entry: the transport the page registered under the agent's name.
export function start(ask) {
  const i32 = new Int32Array(ask.memory.buffer);
  const w = ask.address >> 2;
  const length = i32[w + NAME_LEN] >>> 0;
  const name = new TextDecoder().decode(
    new Uint8Array(ask.memory.buffer).slice(ask.address + NAME_AT, ask.address + NAME_AT + length),
  );
  const transport = globalThis.ashChannels && globalThis.ashChannels[name];
  if (!transport) throw new Error(`no transport registered for channel "${name}" (globalThis.ashChannels)`);
  return serve(ask, transport);
}
