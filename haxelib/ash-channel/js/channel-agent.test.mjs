import { test } from "node:test";
import assert from "node:assert/strict";
import { serve, start } from "./channel-agent.mjs";

const FLAGS = 11, HEAD = 6, TAIL = 7, EHEAD = 4, ETAIL = 5;
function fixture() {
  const memory = new WebAssembly.Memory({ initial: 2, maximum: 4, shared: true });
  const address = 128, capacity = 4096, w = address / 4;
  const words = () => new Int32Array(memory.buffer);
  words().set([0x43485341, 1, capacity, capacity], w);
  return { memory, address, w, words, capacity };
}
const tick = () => new Promise(resolve => setTimeout(resolve, 10));
function events(f) {
  const words = f.words(), bytes = new DataView(f.memory.buffer);
  let tail = Atomics.load(words, f.w + ETAIL) >>> 0;
  const head = Atomics.load(words, f.w + EHEAD) >>> 0, result = [];
  while (tail !== head) {
    const at = f.address + 128 + tail % f.capacity, header = bytes.getUint32(at, true);
    const length = header >>> 8;
    assert.ok(length >= 4 && length <= f.capacity);
    if ((header & 255) !== 0) result.push(header & 255);
    tail = (tail + length) >>> 0;
  }
  Atomics.store(words, f.w + ETAIL, tail);
  return result;
}
const close = f => {
  Atomics.or(f.words(), f.w + FLAGS, 4);
  Atomics.notify(f.words(), f.w + HEAD);
};

test("synchronous and asynchronous open failures publish close", async () => {
  for (const open of [() => { throw Error("connect"); }, () => Promise.reject(Error("connect"))]) {
    const f = fixture();
    await serve(f, { open });
    assert.ok(Atomics.load(f.words(), f.w + FLAGS) & 2);
    assert.deepEqual(events(f), [3]);
  }
});

test("close while opening disposes the eventual connection", async () => {
  const f = fixture(); let opened, closes = 0;
  const ready = serve(f, { open: () => new Promise(resolve => opened = resolve) });
  await Promise.resolve(); close(f);
  opened({ send() {}, close() { closes++; } }); await ready;
  assert.equal(closes, 1);
  assert.equal(Atomics.load(f.words(), f.w + FLAGS) & 1, 0);
});

test("a full command ring cannot prevent closing the transport", async () => {
  const f = fixture(); let closes = 0;
  const ready = serve(f, { open: () => ({ send() {}, close() { closes++; } }) });
  await ready;
  // No close record fits, and notify does not change the head's value.
  Atomics.store(f.words(), f.w + HEAD, 4096);
  close(f); await tick();
  assert.equal(closes, 1);
  assert.ok(Atomics.load(f.words(), f.w + FLAGS) & 2);
});

test("incoming overflow closes instead of silently dropping a message", async () => {
  const f = fixture(); let deliver;
  const running = await serve(f, { open: callbacks => {
    deliver = callbacks.deliver; return { send() {}, close() {} };
  } });
  deliver(new Uint8Array(2041));
  assert.ok(Atomics.load(f.words(), f.w + FLAGS) & 2);
  assert.deepEqual(events(f), [2, 3]); running.close();
});

test("memory growth refreshes the wake word view", async () => {
  const f = fixture(); let deliver;
  const running = await serve(f, { open: callbacks => {
    deliver = callbacks.deliver; return { send() {}, close() {} };
  } });
  const wake = f.memory.buffer.byteLength;
  f.memory.grow(1);
  Atomics.store(f.words(), f.w + 10, wake);
  deliver(new Uint8Array([1, 2]));
  assert.equal(Atomics.load(f.words(), wake / 4), 1);
  assert.deepEqual(events(f), [2, 1]); running.close();
});

test("corrupt commands cannot read beyond their published record", async () => {
  const f = fixture(); let sent = 0;
  const running = await serve(f, { open: () => ({ send() { sent++; }, close() {} }) });
  const view = new DataView(f.memory.buffer), at = f.address + 128 + f.capacity;
  view.setUint32(at, 1 | (8 << 8), true); view.setUint32(at + 4, 100, true);
  Atomics.store(f.words(), f.w + HEAD, 8); Atomics.notify(f.words(), f.w + HEAD);
  await tick(); assert.equal(sent, 0);
  assert.ok(Atomics.load(f.words(), f.w + FLAGS) & 2); running.close();
});

test("records queued behind a full event ring retain order before close", async () => {
  const f = fixture(); let deliver, closed;
  await serve(f, { open: callbacks => {
    ({ deliver, closed } = callbacks); return { send() {}, close() {} };
  } });
  for (let i = 0; i < 12; i++) deliver(new Uint8Array(1000));
  closed("peer closed");
  const kinds = [];
  for (let i = 0; i < 20 && !kinds.includes(3); i++) { kinds.push(...events(f)); await tick(); }
  assert.deepEqual(kinds, [2, ...Array(12).fill(1), 3]);
});

test("block bounds and shared memory are required", () => {
  const f = fixture();
  assert.throws(() => serve({ ...f, address: -8 }, {}));
  Atomics.store(f.words(), f.w + 2, 1);
  assert.throws(() => serve(f, {}));
});

test("an unregistered default transport closes waiting receivers", async () => {
  const f = fixture();
  await start(f);
  assert.ok(Atomics.load(f.words(), f.w + FLAGS) & 2);
  assert.deepEqual(events(f), [3]);
});
