// Runtime support for the primitives: a table mapping integer handles to
// JavaScript objects, in-flight requests the guest polls, UTF-16 string
// allocation into the program's heap, and the canvas registry.
//
// `rt` comes from the instantiated program:
//
//   rt.memory   its WebAssembly.Memory
//   rt.alloc    its exported `hlp_alloc_bytes`

const INDEX_BITS = 20;
const GEN_BITS = 7;
const INDEX_MASK = (1 << INDEX_BITS) - 1;
const GEN_MASK = (1 << GEN_BITS) - 1;

// Limits `adapter_limit` can ask for, by index. Haxe's `wgpu.Limit` is the
// same list in the same order.
const LIMITS = [
  "maxTextureDimension1D",
  "maxTextureDimension2D",
  "maxTextureDimension3D",
  "maxBindGroups",
  "maxBufferSize",
  "maxComputeWorkgroupSizeX",
  "maxComputeInvocationsPerWorkgroup",
];

export function makeHandles(rt) {
  const slabs = {};
  for (const k of Object.keys(KINDS)) slabs[k] = { obj: [], gen: [], free: [] };

  const requests = new Map();
  let nextRequest = 1;
  const canvases = new Map();
  const queueOf_ = new Map();

  // Stores an object and returns its handle: kind, generation, slot index.
  function put(kind, object) {
    if (!object) return 0;
    const s = slabs[kind];
    let i;
    if (s.free.length) {
      // Oldest free slot first, or one slot's generation cycles back to a
      // value a stale handle still holds.
      i = s.free.shift();
      s.gen[i] = (s.gen[i] + 1) & GEN_MASK;
    } else {
      i = s.obj.length;
      if (i > INDEX_MASK) throw new Error(`hlwgpu: too many live ${kind} handles`);
      s.gen[i] = 0;
    }
    s.obj[i] = object;
    return (KINDS[kind] << (INDEX_BITS + GEN_BITS)) | (s.gen[i] << INDEX_BITS) | i;
  }

  // The object, if the handle is live and of this kind.
  function get(kind, handle) {
    const s = slabs[kind];
    const i = handle & INDEX_MASK;
    const g = (handle >> INDEX_BITS) & GEN_MASK;
    if ((handle >>> (INDEX_BITS + GEN_BITS)) !== KINDS[kind]
        || s.obj[i] === undefined || s.gen[i] !== g) {
      throw new Error(`hlwgpu: handle ${handle} is not a live ${kind}`);
    }
    return s.obj[i];
  }

  // Releases a handle. Repeating it, or dropping one that was never live,
  // does nothing.
  function drop(kind, handle) {
    if (!handle) return;
    const s = slabs[kind];
    const i = handle & INDEX_MASK;
    const g = (handle >> INDEX_BITS) & GEN_MASK;
    if ((handle >>> (INDEX_BITS + GEN_BITS)) !== KINDS[kind]) return;
    if (s.obj[i] === undefined || s.gen[i] !== g) return;
    const object = s.obj[i];
    s.obj[i] = undefined;
    s.free.push(i);
    // Buffers, textures and devices hold GPU memory; the rest are collected.
    if (object && typeof object.destroy === "function") object.destroy();
  }

  // Parks a promise where the guest can poll for it. Reaching requestReady
  // again means returning to the event loop, which is the only way the
  // promise settles, so nothing here waits.
  function pending(promise) {
    const id = nextRequest++;
    const slot = { done: false, result: 0, error: null };
    requests.set(id, slot);
    Promise.resolve(promise).then(
      (v) => { slot.result = typeof v === "number" ? v : 0; slot.done = true; },
      (e) => { slot.error = e; slot.done = true; },
    );
    return id;
  }

  function requestReady(id) {
    const slot = requests.get(id);
    if (!slot) throw new Error(`hlwgpu: no such request ${id}`);
    return slot.done;
  }

  // Collects a finished request and forgets it. Reading early raises, so a
  // caller that polls wrongly finds out here.
  function requestResult(id) {
    const slot = requests.get(id);
    if (!slot) throw new Error(`hlwgpu: no such request ${id}`);
    if (!slot.done) throw new Error(`hlwgpu: request ${id} read before it was ready`);
    requests.delete(id);
    if (slot.error) throw slot.error;
    return slot.result;
  }

  // Copies a string into the program's heap as NUL-terminated UTF-16, which
  // is what String.fromUCS2 reads. Its own allocator, so the result is an
  // ordinary hl.Bytes its collector already tracks.
  function str(text) {
    const s = String(text ?? "");
    const ptr = rt.alloc((s.length + 1) * 2);
    if (!ptr) return 0;
    // Fresh each time: growing the memory detaches existing views of its buffer.
    const out = new Uint16Array(rt.memory.buffer, ptr, s.length + 1);
    for (let i = 0; i < s.length; i++) out[i] = s.charCodeAt(i);
    out[s.length] = 0;
    return ptr;
  }

  // A device and its queue arrive together, so they are kept together.
  function putDevice(device) {
    const h = put("device", device);
    if (h) queueOf_.set(h, put("queue", device.queue));
    return h;
  }

  function queueOf(device) {
    return queueOf_.get(device) ?? 0;
  }

  function dropDevice(device) {
    const q = queueOf_.get(device);
    if (q) drop("queue", q);
    queueOf_.delete(device);
    drop("device", device);
  }

  // Views into the program's heap. Taken fresh: growing the memory detaches
  // every existing view of its buffer.
  function view(ptr, len) {
    return new Uint8Array(rt.memory.buffer, ptr, len);
  }

  function handles(ptr, count) {
    return Array.from(new Int32Array(rt.memory.buffer, ptr, count));
  }

  function writeInto(ptr, source) {
    const src = new Uint8Array(source);
    new Uint8Array(rt.memory.buffer, ptr, src.length).set(src);
    return true;
  }

  // A NUL-terminated UTF-16 string out of the heap, the other direction from
  // str().
  function readStr(ptr) {
    const mem = new Uint16Array(rt.memory.buffer);
    let at = ptr >> 1;
    let out = "";
    while (mem[at]) out += String.fromCharCode(mem[at++]);
    return out;
  }

  function limitName(which) {
    return LIMITS[which] ?? null;
  }

  // Canvases the page lends, by name. HashLink runs on a Worker with no DOM,
  // so a canvas is transferred with transferControlToOffscreen() and
  // registered rather than looked up by id.
  function registerCanvas(name, offscreen) {
    canvases.set(name, offscreen);
  }

  function canvas(name) {
    const c = canvases.get(name);
    if (!c) throw new Error(`hlwgpu: no canvas registered as ${name}`);
    return c;
  }

  return {
    put, get, drop, pending, requestReady, requestResult,
    putDevice, queueOf, dropDevice,
    str, readStr, view, handles, writeInto,
    limitName, registerCanvas, canvas, LIMITS,
  };
}
