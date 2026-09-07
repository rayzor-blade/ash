// GENERATED from `wgpu.api`, with the prelude below taken verbatim from
// `js/prelude.js`. Edit one of those, not this file.

// The handle kind numbering, from the same line of the declaration that
// `kinds.rs` comes from.
const KINDS = { instance: 1, adapter: 2, device: 3, queue: 4, buffer: 5, texture: 6, view: 7, sampler: 8, shader: 9, bindgroup: 10, layout: 11, pipeline: 12, encoder: 13, pass: 14, surface: 15 };

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


// The import object a page merges into `env`: one entry per primitive,
// each body taken from the declaration.
export function hlwgpuImports(rt) {
  const H = makeHandles(rt);
  return {
    // The entry point. Natively this is a wgpu Instance; in a page it is
    // `navigator.gpu` itself, which is why a page with WebGPU disabled returns 0
    // here rather than failing later with something less obvious.
    hlwgpu_instance_create: () => H.put("instance", navigator.gpu),
    hlwgpu_instance_destroy: (inst) => { H.drop("instance", inst); },
    // Returns a request id, not an adapter. `requestAdapter` is a promise in a
    // page and nothing may block on it, so async work is started here and
    // collected through `request_ready` / `request_result`. Natively it resolves
    // before this returns; the id exists anyway, so both look the same.
    // 
    // `power` is 0 for low-power and 1 for high-performance.
    hlwgpu_adapter_request: (inst, power) => H.pending(H.get("instance", inst).requestAdapter({ powerPreference: power === 1 ? "high-performance" : "low-power" }).then(a => a ? H.put("adapter", a) : 0)),
    hlwgpu_request_ready: (request) => (H.requestReady(request)) ? 1 : 0,
    // Only meaningful once `request_ready` is true. Reading early raises rather
    // than answering zero, so a caller that polls wrongly finds out here.
    hlwgpu_request_result: (request) => H.requestResult(request),
    // UTF-16 and NUL-terminated, which is what `String.fromUCS2` expects.
    hlwgpu_adapter_name: (adapter) => H.str(H.get("adapter", adapter).info.description || H.get("adapter", adapter).info.vendor || "WebGPU adapter"),
    // 0 unknown, 1 vulkan, 2 metal, 3 dx12, 4 gl, 5 webgpu.
    hlwgpu_adapter_backend: (adapter) => (H.get("adapter", adapter), 5),
    // One limit by index, so that adding a limit does not add a primitive. The
    // indices are declared once, in Haxe, as `wgpu.Limit`.
    hlwgpu_adapter_limit: (adapter, which) => (H.get("adapter", adapter).limits[H.limitName(which)] | 0),
    hlwgpu_adapter_destroy: (adapter) => { H.drop("adapter", adapter); },
    // The queue comes back with the device, so `device_queue` needs no request of
    // its own.
    hlwgpu_device_request: (adapter) => H.pending(H.get("adapter", adapter).requestDevice().then(d => H.putDevice(d))),
    hlwgpu_device_queue: (device) => H.queueOf(device),
    // Lets finished work report itself. A page does this from the event loop, so
    // there is nothing to do there.
    hlwgpu_device_poll: (device) => { H.get("device", device); },
    hlwgpu_device_destroy: (device) => { H.dropDevice(device); },
    // `usage` is WebGPU's GPUBufferUsage bitmask, which wgpu numbers identically:
    // 1 MAP_READ, 2 MAP_WRITE, 4 COPY_SRC, 8 COPY_DST, 128 STORAGE.
    hlwgpu_buffer_create: (device, size, usage) => H.put("buffer", H.get("device", device).createBuffer({ size, usage })),
    hlwgpu_queue_write_buffer: (queue, buffer, offset, data, len) => { H.get("queue", queue).writeBuffer(H.get("buffer", buffer), offset, H.view(data, len)); },
    // Starts a read mapping. Poll it with `request_ready`, then `buffer_copy_out`.
    hlwgpu_buffer_map_begin: (device, buffer, offset, size) => H.pending(H.get("buffer", buffer).mapAsync(1, offset, size).then(() => 1)),
    // Copies from a mapped buffer into `out`. False if it was not mapped.
    hlwgpu_buffer_copy_out: (buffer, offset, out, len) => (H.writeInto(out, H.get("buffer", buffer).getMappedRange(offset, len))) ? 1 : 0,
    hlwgpu_buffer_unmap: (buffer) => { H.get("buffer", buffer).unmap(); },
    hlwgpu_buffer_destroy: (buffer) => { H.drop("buffer", buffer); },
    hlwgpu_shader_create: (device, wgsl) => H.put("shader", H.get("device", device).createShaderModule({ code: H.readStr(wgsl) })),
    hlwgpu_shader_destroy: (shader) => { H.drop("shader", shader); },
    // Layout is inferred from the shader, so a bind group only needs its buffers.
    hlwgpu_compute_pipeline_create: (device, shader, entry) => H.put("pipeline", H.get("device", device).createComputePipeline({ layout: "auto", compute: { module: H.get("shader", shader), entryPoint: H.readStr(entry) } })),
    hlwgpu_pipeline_destroy: (pipeline) => { H.drop("pipeline", pipeline); },
    // `buffers` is `count` handles, one per binding, in binding order.
    hlwgpu_bind_group_create: (device, pipeline, group, buffers, count) => H.put("bindgroup", H.get("device", device).createBindGroup({ layout: H.get("pipeline", pipeline).getBindGroupLayout(group), entries: H.handles(buffers, count).map((h, i) => ({ binding: i, resource: { buffer: H.get("buffer", h) } })) })),
    hlwgpu_bind_group_destroy: (bindgroup) => { H.drop("bindgroup", bindgroup); },
    hlwgpu_encoder_create: (device) => H.put("encoder", H.get("device", device).createCommandEncoder()),
    // A whole compute pass: one pipeline, one bind group, one dispatch. Passes
    // with several dispatches get their own primitives when something needs them.
    hlwgpu_encoder_compute: (encoder, pipeline, bindgroup, x, y, z) => { const p = H.get("encoder", encoder).beginComputePass(); p.setPipeline(H.get("pipeline", pipeline)); p.setBindGroup(0, H.get("bindgroup", bindgroup)); p.dispatchWorkgroups(x, y, z); p.end(); },
    hlwgpu_encoder_copy_buffer: (encoder, src, src_offset, dst, dst_offset, size) => { H.get("encoder", encoder).copyBufferToBuffer(H.get("buffer", src), src_offset, H.get("buffer", dst), dst_offset, size); },
    // Finishes the encoder and submits it. The encoder is spent afterwards.
    hlwgpu_encoder_submit: (encoder, queue) => { H.get("queue", queue).submit([H.get("encoder", encoder).finish()]); H.drop("encoder", encoder); },
    // Finishes when everything submitted so far has run. Takes the device because
    // natively the callback fires only when one is polled, and polling is what
    // `request_ready` does while waiting.
    hlwgpu_queue_work_done: (device, queue) => H.pending(H.get("queue", queue).onSubmittedWorkDone().then(() => 1)),
  };
}
