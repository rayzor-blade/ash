// GENERATED from `wgpu.api`, with the prelude below taken verbatim from
// `js/prelude.js`. Edit one of those, not this file.

// The handle kind numbering, from the same line of the declaration that
// `kinds.rs` comes from.
const KINDS = { instance: 1, adapter: 2, device: 3, queue: 4, buffer: 5, texture: 6, view: 7, sampler: 8, shader: 9, bindgroup: 10, pipeline: 11, renderpipeline: 12, encoder: 13, surface: 14, builder: 15 };

// BlendFactor from the WebGPU IDL, indexed as the declaration numbers it.
const BLEND_FACTOR = ["zero", "one", "src", "one-minus-src", "src-alpha", "one-minus-src-alpha", "dst", "one-minus-dst", "dst-alpha", "one-minus-dst-alpha", "src-alpha-saturated", "constant", "one-minus-constant", "src1", "one-minus-src1", "src1-alpha", "one-minus-src1-alpha"];

// BlendOperation from the WebGPU IDL, indexed as the declaration numbers it.
const BLEND_OPERATION = ["add", "subtract", "reverse-subtract", "min", "max"];

// CompareFunction from the WebGPU IDL, indexed as the declaration numbers it.
const COMPARE_FUNCTION = ["never", "less", "equal", "less-equal", "greater", "not-equal", "greater-equal", "always"];

// PrimitiveTopology from the WebGPU IDL, indexed as the declaration numbers it.
const PRIMITIVE_TOPOLOGY = ["point-list", "line-list", "line-strip", "triangle-list", "triangle-strip"];

// CullMode from the WebGPU IDL, indexed as the declaration numbers it.
const CULL_MODE = ["none", "front", "back"];

// FrontFace from the WebGPU IDL, indexed as the declaration numbers it.
const FRONT_FACE = ["ccw", "cw"];

// VertexStepMode from the WebGPU IDL, indexed as the declaration numbers it.
const VERTEX_STEP_MODE = ["vertex", "instance"];

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

// Texture formats, by index. Haxe's `wgpu.TextureFormat` and the match in
// `imp.rs` are the same list in the same order.
const FORMATS = ["rgba8unorm", "bgra8unorm", "rgba8unorm-srgb", "depth32float", "bgra8unorm-srgb"];

// Vertex attribute formats, likewise `wgpu.VertexFormat`.
const VERTEX_FORMATS = ["float32x2", "float32x3", "float32x4", "uint32"];

// Bytes each takes, in the same order. Grows with VERTEX_FORMATS.
const VERTEX_FORMAT_SIZES = [8, 12, 16, 4];

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
  const passes = new Map();
  const frames = new Map();
  const described = new Map();

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

  // A pass belongs to the encoder that opened it, until it is ended.
  function beginPass(encoder, descriptor) {
    passes.set(encoder, get("encoder", encoder).beginRenderPass(descriptor));
  }

  function pass(encoder) {
    const p = passes.get(encoder);
    if (!p) throw new Error(`hlwgpu: encoder ${encoder} has no open pass`);
    return p;
  }

  function endPass(encoder) {
    const p = passes.get(encoder);
    if (p) {
      p.end();
      passes.delete(encoder);
    }
  }

  // What a handle binds as. The kind is in the handle, so a bind group does
  // not have to be told what each entry is.
  function resource(handle) {
    const kind = handle >>> (INDEX_BITS + GEN_BITS);
    if (kind === KINDS.buffer) return { buffer: get("buffer", handle) };
    if (kind === KINDS.view) return get("view", handle);
    if (kind === KINDS.sampler) return get("sampler", handle);
    throw new Error(`hlwgpu: handle ${handle} cannot be bound`);
  }

  // Compute and render pipelines both have bind group layouts.
  function layoutOf(pipeline, group) {
    const kind = pipeline >>> (INDEX_BITS + GEN_BITS);
    const p = kind === KINDS.renderpipeline
      ? get("renderpipeline", pipeline)
      : get("pipeline", pipeline);
    return p.getBindGroupLayout(group);
  }

  // A page presents when its task ends, so all this does is let go of the
  // view that `surface_acquire` handed out.
  function releaseFrame(surface) {
    const view = frames.get(surface);
    if (view) {
      drop("view", view);
      frames.delete(surface);
    }
  }

  // What a canvas actually wants, rather than a guess. -1 for a name this
  // library has none for, which is what the native side answers too.
  function canvasFormat() {
    const at = FORMATS.indexOf(navigator.gpu.getPreferredCanvasFormat());
    return at < 0 ? -1 : at;
  }

  // Assembles what the builder calls accumulated, and spends the builder.
  function buildPipeline(builder) {
    const d = get("builder", builder);
    const pipeline = get("device", d.device).createRenderPipeline({
      layout: "auto",
      vertex: {
        module: d.module,
        entryPoint: d.vs,
        buffers: d.vertex.buffers.map((b) => ({
          arrayStride: b.arrayStride || b.attributes.reduce(
            (w, a) => Math.max(w, a.offset + VERTEX_FORMAT_SIZES[a.formatIndex]), 0),
          stepMode: b.stepMode,
          attributes: b.attributes.map(({ format, offset, shaderLocation }) =>
            ({ format, offset, shaderLocation })),
        })),
      },
      fragment: { module: d.module, entryPoint: d.fs, targets: d.fragment.targets },
      primitive: d.primitive,
      depthStencil: d.depthStencil ?? undefined,
    });
    drop("builder", builder);
    return put("renderpipeline", pipeline);
  }

  // Packs an attribute against the previous one, at the next free location.
  // Locations count across every buffer of the pipeline, not within one.
  function packAttribute(builder, format) {
    const buffers = get("builder", builder).vertex.buffers;
    const into = buffers[buffers.length - 1];
    const last = into.attributes[into.attributes.length - 1];
    const offset = last ? last.offset + VERTEX_FORMAT_SIZES[last.formatIndex] : 0;
    const location = buffers.reduce((n, b) => n + b.attributes.length, 0);
    into.attributes.push({
      format: VERTEX_FORMATS[format],
      formatIndex: format,
      offset,
      shaderLocation: location,
    });
  }

  // A pass is described before it is opened; these hold the description.
  function resetPass(encoder) {
    described.set(encoder, { colour: [], depth: null });
  }

  function describing(encoder) {
    const d = described.get(encoder);
    if (!d) throw new Error(`hlwgpu: encoder ${encoder} is not describing a pass`);
    return d;
  }

  function addColour(encoder, view, clearValue) {
    describing(encoder).colour.push({ view, clearValue, loadOp: "clear", storeOp: "store" });
  }

  function addDepth(encoder, view, clear) {
    describing(encoder).depth = {
      view,
      depthClearValue: clear,
      depthLoadOp: "clear",
      depthStoreOp: "store",
    };
  }

  function beginDescribedPass(encoder) {
    const d = describing(encoder);
    beginPass(encoder, {
      colorAttachments: d.colour,
      depthStencilAttachment: d.depth ?? undefined,
    });
    described.delete(encoder);
  }

  function formatName(which) {
    return FORMATS[which] ?? FORMATS[0];
  }

  // `count` triples of format, byte offset and shader location.
  function attributes(ptr, count) {
    const raw = new Int32Array(rt.memory.buffer, ptr, count * 3);
    const out = [];
    for (let i = 0; i < count; i++) {
      out.push({
        format: VERTEX_FORMATS[raw[i * 3]] ?? VERTEX_FORMATS[0],
        offset: raw[i * 3 + 1],
        shaderLocation: raw[i * 3 + 2],
      });
    }
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
    beginPass, pass, endPass, formatName, canvasFormat, attributes, packAttribute, buildPipeline,
    resetPass, addColour, addDepth, beginDescribedPass, resource, layoutOf, releaseFrame,
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
    // `bound` is `count` handles, one per binding, in binding order. Each may be a
    // buffer, a texture view or a sampler: a handle carries its own kind, so what
    // it binds as does not have to be said twice.
    // 
    // The pipeline may be a compute or a render one.
    hlwgpu_bind_group_create: (device, pipeline, group, bound, count) => H.put("bindgroup", H.get("device", device).createBindGroup({ layout: H.layoutOf(pipeline, group), entries: H.handles(bound, count).map((h, i) => ({ binding: i, resource: H.resource(h) })) })),
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
    // `format` is `wgpu.TextureFormat`; `usage` is GPUTextureUsage's bits:
    // 1 COPY_SRC, 2 COPY_DST, 4 TEXTURE_BINDING, 8 STORAGE_BINDING,
    // 16 RENDER_ATTACHMENT.
    hlwgpu_texture_create: (device, width, height, format, usage) => H.put("texture", H.get("device", device).createTexture({ size: [width, height], format: H.formatName(format), usage })),
    hlwgpu_texture_view: (texture) => H.put("view", H.get("texture", texture).createView()),
    hlwgpu_texture_destroy: (texture) => { H.drop("texture", texture); },
    hlwgpu_view_destroy: (view) => { H.drop("view", view); },
    // A render pipeline is built by a run of calls rather than one descriptor:
    // every argument stays a typed scalar the compiler checks, and there is no
    // packed layout for the two sides to disagree about. The crossings cost
    // nothing, because a pipeline is built at load and not per frame.
    hlwgpu_pipeline_begin: (device) => H.put("builder", { device, vertex: { buffers: [] }, fragment: { targets: [] }, primitive: {}, depthStencil: null }),
    hlwgpu_pipeline_shader: (builder, shader, vs, fs) => { const d = H.get("builder", builder); d.module = H.get("shader", shader); d.vs = H.readStr(vs); d.fs = H.readStr(fs); },
    // Opens a vertex buffer layout; the attributes that follow belong to it.
    // A stride of 0 means "as wide as the attributes turn out to be", worked out
    // when the pipeline is built.
    hlwgpu_pipeline_vertex_buffer: (builder, stride, step) => { H.get("builder", builder).vertex.buffers.push({ arrayStride: stride, stepMode: VERTEX_STEP_MODE[step], attributes: [] }); },
    // Appends an attribute packed against the one before it, at the next free
    // shader location. What a vertex layout almost always is, and one fewer pair
    // of numbers to get wrong.
    hlwgpu_pipeline_attribute_packed: (builder, format) => { H.packAttribute(builder, format); },
    hlwgpu_pipeline_attribute: (builder, format, offset, location) => { const b = H.get("builder", builder).vertex.buffers; b[b.length - 1].attributes.push({ format: VERTEX_FORMATS[format], offset, shaderLocation: location }); },
    // Opens a colour target; a blend that follows belongs to it.
    hlwgpu_pipeline_target: (builder, format, write_mask) => { H.get("builder", builder).fragment.targets.push({ format: H.formatName(format), writeMask: write_mask }); },
    hlwgpu_pipeline_blend: (builder, src, dst, op, src_alpha, dst_alpha, op_alpha) => { const t = H.get("builder", builder).fragment.targets; t[t.length - 1].blend = { color: { srcFactor: BLEND_FACTOR[src], dstFactor: BLEND_FACTOR[dst], operation: BLEND_OPERATION[op] }, alpha: { srcFactor: BLEND_FACTOR[src_alpha], dstFactor: BLEND_FACTOR[dst_alpha], operation: BLEND_OPERATION[op_alpha] } }; },
    hlwgpu_pipeline_depth: (builder, format, write, compare) => { H.get("builder", builder).depthStencil = { format: H.formatName(format), depthWriteEnabled: !!write, depthCompare: COMPARE_FUNCTION[compare] }; },
    hlwgpu_pipeline_primitive: (builder, topology, cull, front) => { const p = H.get("builder", builder).primitive; p.topology = PRIMITIVE_TOPOLOGY[topology]; p.cullMode = CULL_MODE[cull]; p.frontFace = FRONT_FACE[front]; },
    // Builds the pipeline and spends the builder.
    hlwgpu_render_pipeline_build: (builder) => H.buildPipeline(builder),
    hlwgpu_render_pipeline_destroy: (pipeline) => { H.drop("renderpipeline", pipeline); },
    // A pass is described before it is opened, because it can have more than one
    // colour target and may or may not have depth. `pass_reset` starts describing,
    // `pass_begin` opens what was described.
    hlwgpu_pass_reset: (encoder) => { H.resetPass(encoder); },
    // Adds a colour target and what to clear it to. Their order is the order the
    // fragment shader's `@location`s are numbered in.
    hlwgpu_pass_colour: (encoder, view, r, g, b, a) => { H.addColour(encoder, H.get("view", view), { r, g, b, a }); },
    hlwgpu_pass_depth: (encoder, view, clear) => { H.addDepth(encoder, H.get("view", view), clear); },
    hlwgpu_pass_begin: (encoder) => { H.beginDescribedPass(encoder); },
    hlwgpu_render_set_pipeline: (encoder, pipeline) => { H.pass(encoder).setPipeline(H.get("renderpipeline", pipeline)); },
    hlwgpu_render_set_vertex_buffer: (encoder, slot, buffer) => { H.pass(encoder).setVertexBuffer(slot, H.get("buffer", buffer)); },
    // Where in the target the clip space -1..1 lands, and what depth range it
    // maps onto.
    hlwgpu_render_set_viewport: (encoder, x, y, width, height, min_depth, max_depth) => { H.pass(encoder).setViewport(x, y, width, height, min_depth, max_depth); },
    // Throws away anything drawn outside this rectangle. Unlike a viewport it
    // does not squeeze what is drawn, it cuts it.
    hlwgpu_render_set_scissor_rect: (encoder, x, y, width, height) => { H.pass(encoder).setScissorRect(x, y, width, height); },
    hlwgpu_render_draw: (encoder, vertices, instances) => { H.pass(encoder).draw(vertices, instances); },
    hlwgpu_encoder_render_end: (encoder) => { H.endPass(encoder); },
    // The other direction. `bytes_per_row` is a multiple of 256 here too.
    hlwgpu_encoder_copy_buffer_to_texture: (encoder, buffer, bytes_per_row, texture, width, height) => { H.get("encoder", encoder).copyBufferToTexture({ buffer: H.get("buffer", buffer), bytesPerRow: bytes_per_row }, { texture: H.get("texture", texture) }, [width, height]); },
    hlwgpu_encoder_copy_texture_to_texture: (encoder, src, dst, width, height) => { H.get("encoder", encoder).copyTextureToTexture({ texture: H.get("texture", src) }, { texture: H.get("texture", dst) }, [width, height]); },
    // Zeroes a range of a buffer without uploading zeroes to it.
    hlwgpu_encoder_clear_buffer: (encoder, buffer, offset, size) => { H.get("encoder", encoder).clearBuffer(H.get("buffer", buffer), offset, size); },
    // `bytes_per_row` must be a multiple of 256, which is WebGPU's rule and not
    // ours: a width of 64 RGBA pixels is exactly one row.
    hlwgpu_encoder_copy_texture_to_buffer: (encoder, texture, buffer, width, height, bytes_per_row) => { H.get("encoder", encoder).copyTextureToBuffer({ texture: H.get("texture", texture) }, { buffer: H.get("buffer", buffer), bytesPerRow: bytes_per_row }, [width, height]); },
    // `filter` is 0 nearest, 1 linear. `address` is 0 clamp-to-edge, 1 repeat.
    hlwgpu_sampler_create: (device, filter, address) => H.put("sampler", H.get("device", device).createSampler({ magFilter: filter === 1 ? "linear" : "nearest", minFilter: filter === 1 ? "linear" : "nearest", addressModeU: address === 1 ? "repeat" : "clamp-to-edge", addressModeV: address === 1 ? "repeat" : "clamp-to-edge" })),
    hlwgpu_sampler_destroy: (sampler) => { H.drop("sampler", sampler); },
    // Unlike a copy out of a texture, this has no row alignment to honour.
    hlwgpu_queue_write_texture: (queue, texture, data, width, height, bytes_per_row) => { H.get("queue", queue).writeTexture({ texture: H.get("texture", texture) }, H.view(data, bytes_per_row * height), { bytesPerRow: bytes_per_row }, [width, height]); },
    hlwgpu_render_set_bind_group: (encoder, group, bindgroup) => { H.pass(encoder).setBindGroup(group, H.get("bindgroup", bindgroup)); },
    // `format` is 0 for uint16 and 1 for uint32.
    hlwgpu_render_set_index_buffer: (encoder, buffer, format) => { H.pass(encoder).setIndexBuffer(H.get("buffer", buffer), format === 1 ? "uint32" : "uint16"); },
    hlwgpu_render_draw_indexed: (encoder, indices, instances) => { H.pass(encoder).drawIndexed(indices, instances); },
    // A surface on a native window, from the raw handle fields `hlwindow` reports.
    // Integers because the two libraries are separate: a Rust type cannot cross
    // between them, the pointer inside it can.
    // 
    // A page has no such thing -- its surface comes from a canvas it already owns.
    hlwgpu_surface_create: (instance, platform, wa, wb, da, db) => { throw new Error("wgpu: surface_create has no meaning in a page"); },
    // What this surface would rather be configured as, as a `wgpu.TextureFormat`.
    hlwgpu_surface_preferred_format: (surface, adapter) => H.canvasFormat(),
    hlwgpu_surface_configure: (device, surface, width, height, format) => { H.get("surface", surface).configure({ device: H.get("device", device), format: H.formatName(format), alphaMode: "opaque" }); },
    // The view to draw this frame into, or 0 if the surface needs configuring
    // again -- which is what a resize looks like from here.
    hlwgpu_surface_acquire: (surface) => H.put("view", H.get("surface", surface).getCurrentTexture().createView()),
    // Hands the frame over, after the work drawing it has been submitted. A page
    // presents at the end of its task, so this only releases the view there.
    hlwgpu_surface_present: (queue, surface) => { H.releaseFrame(surface); },
    hlwgpu_surface_destroy: (surface) => { H.drop("surface", surface); },
  };
}
