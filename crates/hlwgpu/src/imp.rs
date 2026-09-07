//! What the primitives do, over `wgpu`.
//!
//! Serves the native library, and later a host answering the wasm imports.
//! Nothing here is generated; `bindings` is the part that is.

// A primitive's arity comes from `wgpu.api`, not from a style choice here:
// these signatures have to match what the generated bindings call.
#![allow(clippy::too_many_arguments)]

use std::sync::atomic::{AtomicBool, AtomicI32, Ordering};
use std::sync::{Arc, LazyLock, Mutex};

use hl_abi::{hlp_alloc_bytes, vbyte};
use crate::bindings::kinds::Kind;
use crate::handles::{kind_of, PendingRequests, Slab};

/// A device and the queue that came back with it.
struct DeviceEntry {
    device: wgpu::Device,
    queue: i32,
}

/// An encoder and whatever pass is open on it.
///
/// A `RenderPass` borrows its encoder, which a handle table cannot express, so
/// `forget_lifetime` erases it and the two are kept together instead. The
/// encoder is `Option` because `finish()` consumes it.
#[derive(Default)]
struct EncoderEntry {
    encoder: Option<wgpu::CommandEncoder>,
    pass: Option<wgpu::RenderPass<'static>>,
    /// What the next pass will attach, in the order it was described. Held as
    /// handles rather than views because the views have to outlive the
    /// descriptor, and that is easier to arrange when the pass opens.
    colour: Vec<(i32, wgpu::Color)>,
    depth: Option<(i32, f64)>,
}

type Encoder = Mutex<EncoderEntry>;

/// Plain `Mutex`es, not HL ones: threads a library makes are foreign to the
/// VM, and HashLink's own locks give them no exclusion.
macro_rules! slab {
    ($name:ident, $ty:ty, $kind:expr) => {
        static $name: LazyLock<Mutex<Slab<$ty>>> = LazyLock::new(|| Mutex::new(Slab::new($kind)));
    };
}

slab!(INSTANCES, wgpu::Instance, Kind::Instance);
slab!(ADAPTERS, wgpu::Adapter, Kind::Adapter);
slab!(DEVICES, DeviceEntry, Kind::Device);
slab!(QUEUES, wgpu::Queue, Kind::Queue);
slab!(BUFFERS, wgpu::Buffer, Kind::Buffer);
slab!(SHADERS, wgpu::ShaderModule, Kind::Shader);
slab!(PIPELINES, wgpu::ComputePipeline, Kind::Pipeline);
slab!(RENDER_PIPELINES, wgpu::RenderPipeline, Kind::Renderpipeline);
slab!(TEXTURES, wgpu::Texture, Kind::Texture);
slab!(VIEWS, wgpu::TextureView, Kind::View);
slab!(SAMPLERS, wgpu::Sampler, Kind::Sampler);
slab!(BINDGROUPS, wgpu::BindGroup, Kind::Bindgroup);
slab!(ENCODERS, Encoder, Kind::Encoder);

static REQUESTS: LazyLock<Mutex<PendingRequests>> = LazyLock::new(Mutex::default);

/// Looks a handle up and lets go of the slab before the object is used, so no
/// two of these locks are ever held at once.
macro_rules! find {
    ($slab:ident, $handle:expr) => {
        match $slab.lock().unwrap().get($handle) {
            Some(found) => found,
            None => return Default::default(),
        }
    };
    ($slab:ident, $handle:expr, $miss:expr) => {
        match $slab.lock().unwrap().get($handle) {
            Some(found) => found,
            None => return $miss,
        }
    };
}

/// A string into the VM's heap as NUL-terminated UTF-16, which is what
/// `String.fromUCS2` reads.
unsafe fn ucs2_out(text: &str) -> *mut vbyte {
    let units: Vec<u16> = text.encode_utf16().collect();
    let bytes = hlp_alloc_bytes(((units.len() + 1) * 2) as std::ffi::c_int);
    if bytes.is_null() {
        return bytes;
    }
    let out = bytes as *mut u16;
    for (i, unit) in units.iter().enumerate() {
        out.add(i).write(*unit);
    }
    out.add(units.len()).write(0);
    bytes
}

/// The other direction: a NUL-terminated UTF-16 string the VM passed in.
unsafe fn ucs2_in(bytes: *const vbyte) -> String {
    if bytes.is_null() {
        return String::new();
    }
    let mut units = Vec::new();
    let mut at = bytes as *const u16;
    while *at != 0 {
        units.push(*at);
        at = at.add(1);
    }
    String::from_utf16_lossy(&units)
}

// -- instance ---------------------------------------------------------------

pub unsafe fn instance_create() -> i32 {
    let instance = wgpu::Instance::new(wgpu::InstanceDescriptor::new_without_display_handle());
    INSTANCES.lock().unwrap().put(instance)
}

pub unsafe fn instance_destroy(inst: i32) {
    INSTANCES.lock().unwrap().remove(inst);
}

// -- adapter ----------------------------------------------------------------

pub unsafe fn adapter_request(inst: i32, power: i32) -> i32 {
    let instance = find!(INSTANCES, inst, 0);
    let options = wgpu::RequestAdapterOptions {
        power_preference: if power == 1 {
            wgpu::PowerPreference::HighPerformance
        } else {
            wgpu::PowerPreference::LowPower
        },
        ..Default::default()
    };
    // Resolved before this returns on every native backend. The request id
    // exists so a page, where it genuinely is a promise, looks the same.
    let handle = match pollster::block_on(instance.request_adapter(&options)) {
        Ok(adapter) => ADAPTERS.lock().unwrap().put(adapter),
        Err(_) => 0,
    };
    REQUESTS.lock().unwrap().settled(handle)
}

pub unsafe fn adapter_name(adapter: i32) -> *mut vbyte {
    let adapter = find!(ADAPTERS, adapter, std::ptr::null_mut());
    ucs2_out(&adapter.get_info().name)
}

pub unsafe fn adapter_backend(adapter: i32) -> i32 {
    let adapter = find!(ADAPTERS, adapter, 0);
    match adapter.get_info().backend {
        wgpu::Backend::Vulkan => 1,
        wgpu::Backend::Metal => 2,
        wgpu::Backend::Dx12 => 3,
        wgpu::Backend::Gl => 4,
        wgpu::Backend::BrowserWebGpu => 5,
        _ => 0,
    }
}

pub unsafe fn adapter_limit(adapter: i32, which: i32) -> i32 {
    let adapter = find!(ADAPTERS, adapter, 0);
    let limits = adapter.limits();
    // The order `js/prelude.js` LIMITS and Haxe's `wgpu.Limit` both use.
    let value: u64 = match which {
        0 => limits.max_texture_dimension_1d as u64,
        1 => limits.max_texture_dimension_2d as u64,
        2 => limits.max_texture_dimension_3d as u64,
        3 => limits.max_bind_groups as u64,
        4 => limits.max_buffer_size,
        5 => limits.max_compute_workgroup_size_x as u64,
        6 => limits.max_compute_invocations_per_workgroup as u64,
        _ => 0,
    };
    value.min(i32::MAX as u64) as i32
}

pub unsafe fn adapter_destroy(adapter: i32) {
    ADAPTERS.lock().unwrap().remove(adapter);
}

// -- requests ---------------------------------------------------------------

pub unsafe fn request_ready(request: i32) -> bool {
    // Natively a callback runs only when the device is asked, so a caller
    // polling this is what drives its own answer. `Poll` never blocks.
    let device = REQUESTS.lock().unwrap().device_of(request);
    if device != 0 {
        if let Some(entry) = DEVICES.lock().unwrap().get(device) {
            let _ = entry.device.poll(wgpu::PollType::Poll);
        }
    }
    REQUESTS.lock().unwrap().ready(request)
}

pub unsafe fn request_result(request: i32) -> i32 {
    REQUESTS.lock().unwrap().take(request)
}

// -- device -----------------------------------------------------------------

pub unsafe fn device_request(adapter: i32) -> i32 {
    let adapter = find!(ADAPTERS, adapter, 0);
    let handle = match pollster::block_on(adapter.request_device(&wgpu::DeviceDescriptor::default()))
    {
        Ok((device, queue)) => {
            let queue = QUEUES.lock().unwrap().put(queue);
            DEVICES.lock().unwrap().put(DeviceEntry { device, queue })
        }
        Err(_) => 0,
    };
    REQUESTS.lock().unwrap().settled(handle)
}

pub unsafe fn device_queue(device: i32) -> i32 {
    let entry = find!(DEVICES, device, 0);
    entry.queue
}

pub unsafe fn device_poll(device: i32) {
    let entry = find!(DEVICES, device);
    let _ = entry.device.poll(wgpu::PollType::Poll);
}

pub unsafe fn device_destroy(device: i32) {
    let queue = DEVICES.lock().unwrap().get(device).map(|e| e.queue);
    if let Some(queue) = queue {
        QUEUES.lock().unwrap().remove(queue);
    }
    DEVICES.lock().unwrap().remove(device);
}

// -- buffers ----------------------------------------------------------------

pub unsafe fn buffer_create(device: i32, size: i32, usage: i32) -> i32 {
    let entry = find!(DEVICES, device, 0);
    let buffer = entry.device.create_buffer(&wgpu::BufferDescriptor {
        label: None,
        size: size.max(0) as u64,
        usage: wgpu::BufferUsages::from_bits_truncate(usage as u32),
        mapped_at_creation: false,
    });
    BUFFERS.lock().unwrap().put(buffer)
}

pub unsafe fn queue_write_buffer(queue: i32, buffer: i32, offset: i32, data: *mut vbyte, len: i32) {
    if data.is_null() || len <= 0 {
        return;
    }
    let queue = find!(QUEUES, queue);
    let buffer = find!(BUFFERS, buffer);
    let bytes = std::slice::from_raw_parts(data, len as usize);
    queue.write_buffer(&buffer, offset.max(0) as u64, bytes);
}

pub unsafe fn buffer_map_begin(device: i32, buffer: i32, offset: i32, size: i32) -> i32 {
    let buffer = find!(BUFFERS, buffer, 0);
    let done = Arc::new(AtomicBool::new(false));
    let result = Arc::new(AtomicI32::new(0));
    let (flag, value) = (done.clone(), result.clone());
    let start = offset.max(0) as u64;
    buffer.map_async(
        wgpu::MapMode::Read,
        start..start + size.max(0) as u64,
        move |outcome| {
            value.store(i32::from(outcome.is_ok()), Ordering::Release);
            flag.store(true, Ordering::Release);
        },
    );
    REQUESTS.lock().unwrap().waiting(done, result, device)
}

pub unsafe fn buffer_copy_out(buffer: i32, offset: i32, out: *mut vbyte, len: i32) -> bool {
    if out.is_null() || len <= 0 {
        return false;
    }
    let buffer = find!(BUFFERS, buffer, false);
    let start = offset.max(0) as u64;
    let Ok(view) = buffer.get_mapped_range(start..start + len as u64) else {
        return false;
    };
    std::ptr::copy_nonoverlapping(view.as_ptr(), out, len as usize);
    true
}

pub unsafe fn buffer_unmap(buffer: i32) {
    let buffer = find!(BUFFERS, buffer);
    buffer.unmap();
}

pub unsafe fn buffer_destroy(buffer: i32) {
    BUFFERS.lock().unwrap().remove(buffer);
}

// -- shaders and pipelines --------------------------------------------------

pub unsafe fn shader_create(device: i32, wgsl: *mut vbyte) -> i32 {
    let entry = find!(DEVICES, device, 0);
    let source = ucs2_in(wgsl);
    let module = entry.device.create_shader_module(wgpu::ShaderModuleDescriptor {
        label: None,
        source: wgpu::ShaderSource::Wgsl(source.into()),
    });
    SHADERS.lock().unwrap().put(module)
}

pub unsafe fn shader_destroy(shader: i32) {
    SHADERS.lock().unwrap().remove(shader);
}

pub unsafe fn compute_pipeline_create(device: i32, shader: i32, entry: *mut vbyte) -> i32 {
    let device_entry = find!(DEVICES, device, 0);
    let module = find!(SHADERS, shader, 0);
    let name = ucs2_in(entry);
    let pipeline = device_entry
        .device
        .create_compute_pipeline(&wgpu::ComputePipelineDescriptor {
            label: None,
            // Inferred from the shader, so a bind group only needs buffers.
            layout: None,
            module: &module,
            entry_point: Some(name.as_str()),
            compilation_options: Default::default(),
            cache: None,
        });
    PIPELINES.lock().unwrap().put(pipeline)
}

pub unsafe fn pipeline_destroy(pipeline: i32) {
    PIPELINES.lock().unwrap().remove(pipeline);
}

/// One entry of a bind group, held so the descriptor below can borrow it.
enum Bound {
    Buffer(Arc<wgpu::Buffer>),
    View(Arc<wgpu::TextureView>),
    Sampler(Arc<wgpu::Sampler>),
}

pub unsafe fn bind_group_create(
    device: i32,
    pipeline: i32,
    group: i32,
    bound: *mut vbyte,
    count: i32,
) -> i32 {
    if bound.is_null() || count <= 0 {
        return 0;
    }
    let entry = find!(DEVICES, device, 0);
    let group = group.max(0) as u32;

    // A compute or a render pipeline; both have layouts, and the handle says
    // which it is.
    let layout = if kind_of(pipeline) == Kind::Renderpipeline as i32 {
        find!(RENDER_PIPELINES, pipeline, 0).get_bind_group_layout(group)
    } else {
        find!(PIPELINES, pipeline, 0).get_bind_group_layout(group)
    };

    let handles = std::slice::from_raw_parts(bound as *const i32, count as usize);
    let mut found = Vec::with_capacity(handles.len());
    for handle in handles {
        // The kind is in the handle, so an entry does not have to say what it
        // binds as -- and a wrong handle is refused here rather than bound as
        // whatever it happens to overlap.
        let one = match kind_of(*handle) {
            k if k == Kind::Buffer as i32 => BUFFERS.lock().unwrap().get(*handle).map(Bound::Buffer),
            k if k == Kind::View as i32 => VIEWS.lock().unwrap().get(*handle).map(Bound::View),
            k if k == Kind::Sampler as i32 => {
                SAMPLERS.lock().unwrap().get(*handle).map(Bound::Sampler)
            }
            _ => None,
        };
        match one {
            Some(one) => found.push(one),
            None => return 0,
        }
    }

    let entries: Vec<wgpu::BindGroupEntry> = found
        .iter()
        .enumerate()
        .map(|(binding, one)| wgpu::BindGroupEntry {
            binding: binding as u32,
            resource: match one {
                Bound::Buffer(buffer) => buffer.as_entire_binding(),
                Bound::View(view) => wgpu::BindingResource::TextureView(view),
                Bound::Sampler(sampler) => wgpu::BindingResource::Sampler(sampler),
            },
        })
        .collect();

    let bind_group = entry.device.create_bind_group(&wgpu::BindGroupDescriptor {
        label: None,
        layout: &layout,
        entries: &entries,
    });
    BINDGROUPS.lock().unwrap().put(bind_group)
}

pub unsafe fn bind_group_destroy(bindgroup: i32) {
    BINDGROUPS.lock().unwrap().remove(bindgroup);
}

// -- commands ---------------------------------------------------------------

pub unsafe fn encoder_create(device: i32) -> i32 {
    let entry = find!(DEVICES, device, 0);
    let encoder = entry
        .device
        .create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });
    ENCODERS
        .lock()
        .unwrap()
        .put(Mutex::new(EncoderEntry { encoder: Some(encoder), ..Default::default() }))
}

pub unsafe fn encoder_compute(
    encoder: i32,
    pipeline: i32,
    bindgroup: i32,
    x: i32,
    y: i32,
    z: i32,
) {
    let encoder = find!(ENCODERS, encoder);
    let pipeline = find!(PIPELINES, pipeline);
    let bind_group = find!(BINDGROUPS, bindgroup);
    let mut held = encoder.lock().unwrap();
    let Some(encoder) = held.encoder.as_mut() else { return };

    let mut pass = encoder.begin_compute_pass(&wgpu::ComputePassDescriptor {
        label: None,
        timestamp_writes: None,
    });
    pass.set_pipeline(&pipeline);
    pass.set_bind_group(0, &*bind_group, &[]);
    pass.dispatch_workgroups(x.max(0) as u32, y.max(0) as u32, z.max(0) as u32);
}

pub unsafe fn encoder_copy_buffer(
    encoder: i32,
    src: i32,
    src_offset: i32,
    dst: i32,
    dst_offset: i32,
    size: i32,
) {
    let encoder = find!(ENCODERS, encoder);
    let source = find!(BUFFERS, src);
    let target = find!(BUFFERS, dst);
    let mut held = encoder.lock().unwrap();
    let Some(encoder) = held.encoder.as_mut() else { return };
    encoder.copy_buffer_to_buffer(
        &source,
        src_offset.max(0) as u64,
        &target,
        dst_offset.max(0) as u64,
        size.max(0) as u64,
    );
}

pub unsafe fn encoder_submit(encoder: i32, queue: i32) {
    let handle = encoder;
    let encoder = find!(ENCODERS, handle);
    let queue = find!(QUEUES, queue);
    let taken = encoder.lock().unwrap().encoder.take();
    if let Some(encoder) = taken {
        queue.submit([encoder.finish()]);
    }
    // Spent either way: an encoder cannot be finished twice.
    ENCODERS.lock().unwrap().remove(handle);
}

pub unsafe fn queue_work_done(device: i32, queue: i32) -> i32 {
    let queue = find!(QUEUES, queue, 0);
    let done = Arc::new(AtomicBool::new(false));
    let result = Arc::new(AtomicI32::new(0));
    let (flag, value) = (done.clone(), result.clone());
    queue.on_submitted_work_done(move || {
        value.store(1, Ordering::Release);
        flag.store(true, Ordering::Release);
    });
    REQUESTS.lock().unwrap().waiting(done, result, device)
}

// -- textures ---------------------------------------------------------------

/// The order `js/prelude.js` FORMATS and Haxe's `wgpu.TextureFormat` use.
fn texture_format(which: i32) -> wgpu::TextureFormat {
    match which {
        1 => wgpu::TextureFormat::Bgra8Unorm,
        2 => wgpu::TextureFormat::Rgba8UnormSrgb,
        3 => wgpu::TextureFormat::Depth32Float,
        4 => wgpu::TextureFormat::Bgra8UnormSrgb,
        _ => wgpu::TextureFormat::Rgba8Unorm,
    }
}

/// Likewise `wgpu.VertexFormat`.
fn vertex_format(which: i32) -> wgpu::VertexFormat {
    match which {
        1 => wgpu::VertexFormat::Float32x3,
        2 => wgpu::VertexFormat::Float32x4,
        3 => wgpu::VertexFormat::Uint32,
        _ => wgpu::VertexFormat::Float32x2,
    }
}

pub unsafe fn texture_create(device: i32, width: i32, height: i32, format: i32, usage: i32) -> i32 {
    let entry = find!(DEVICES, device, 0);
    let texture = entry.device.create_texture(&wgpu::TextureDescriptor {
        label: None,
        size: wgpu::Extent3d {
            width: width.max(1) as u32,
            height: height.max(1) as u32,
            depth_or_array_layers: 1,
        },
        mip_level_count: 1,
        sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format: texture_format(format),
        usage: wgpu::TextureUsages::from_bits_truncate(usage as u32),
        view_formats: &[],
    });
    TEXTURES.lock().unwrap().put(texture)
}

pub unsafe fn texture_view(texture: i32) -> i32 {
    let texture = find!(TEXTURES, texture, 0);
    let view = texture.create_view(&wgpu::TextureViewDescriptor::default());
    VIEWS.lock().unwrap().put(view)
}

pub unsafe fn texture_destroy(texture: i32) {
    TEXTURES.lock().unwrap().remove(texture);
}

pub unsafe fn view_destroy(view: i32) {
    VIEWS.lock().unwrap().remove(view);
}

// -- render pipelines -------------------------------------------------------

pub unsafe fn render_pipeline_destroy(pipeline: i32) {
    RENDER_PIPELINES.lock().unwrap().remove(pipeline);
}

// -- render passes ----------------------------------------------------------

pub unsafe fn pass_reset(encoder: i32) {
    let entry = find!(ENCODERS, encoder);
    let mut held = entry.lock().unwrap();
    held.colour.clear();
    held.depth = None;
}

pub unsafe fn pass_colour(encoder: i32, view: i32, r: f64, g: f64, b: f64, a: f64) {
    let entry = find!(ENCODERS, encoder);
    entry
        .lock()
        .unwrap()
        .colour
        .push((view, wgpu::Color { r, g, b, a }));
}

pub unsafe fn pass_depth(encoder: i32, view: i32, clear: f64) {
    let entry = find!(ENCODERS, encoder);
    entry.lock().unwrap().depth = Some((view, clear));
}

/// Opens what was described. Anything not attached by then is not in the pass.
pub unsafe fn pass_begin(encoder: i32) {
    let entry = find!(ENCODERS, encoder);
    let mut held = entry.lock().unwrap();

    // Resolved before the descriptor is built, so the views outlive it.
    let mut colour = Vec::with_capacity(held.colour.len());
    for (handle, clear) in &held.colour {
        match VIEWS.lock().unwrap().get(*handle) {
            Some(view) => colour.push((view, *clear)),
            None => return,
        }
    }
    let depth = match held.depth {
        Some((handle, clear)) => match VIEWS.lock().unwrap().get(handle) {
            Some(view) => Some((view, clear)),
            None => return,
        },
        None => None,
    };

    let attachments: Vec<Option<wgpu::RenderPassColorAttachment>> = colour
        .iter()
        .map(|(view, clear)| {
            Some(wgpu::RenderPassColorAttachment {
                view,
                resolve_target: None,
                depth_slice: None,
                ops: wgpu::Operations {
                    load: wgpu::LoadOp::Clear(*clear),
                    store: wgpu::StoreOp::Store,
                },
            })
        })
        .collect();

    let pass = {
        let Some(encoder) = held.encoder.as_mut() else { return };
        encoder
            .begin_render_pass(&wgpu::RenderPassDescriptor {
                label: None,
                color_attachments: &attachments,
                depth_stencil_attachment: depth.as_ref().map(|(view, clear)| {
                    wgpu::RenderPassDepthStencilAttachment {
                        view,
                        depth_ops: Some(wgpu::Operations {
                            load: wgpu::LoadOp::Clear(*clear as f32),
                            store: wgpu::StoreOp::Store,
                        }),
                        stencil_ops: None,
                    }
                }),
                timestamp_writes: None,
                occlusion_query_set: None,
                multiview_mask: Default::default(),
            })
            .forget_lifetime()
    };
    held.pass = Some(pass);
    held.colour.clear();
    held.depth = None;
}

pub unsafe fn render_set_pipeline(encoder: i32, pipeline: i32) {
    let entry = find!(ENCODERS, encoder);
    let pipeline = find!(RENDER_PIPELINES, pipeline);
    let mut held = entry.lock().unwrap();
    if let Some(pass) = held.pass.as_mut() {
        pass.set_pipeline(&pipeline);
    }
}

pub unsafe fn render_set_vertex_buffer(encoder: i32, slot: i32, buffer: i32) {
    let entry = find!(ENCODERS, encoder);
    let buffer = find!(BUFFERS, buffer);
    let mut held = entry.lock().unwrap();
    if let Some(pass) = held.pass.as_mut() {
        pass.set_vertex_buffer(slot.max(0) as u32, buffer.slice(..));
    }
}

pub unsafe fn render_set_viewport(
    encoder: i32,
    x: f64,
    y: f64,
    width: f64,
    height: f64,
    min_depth: f64,
    max_depth: f64,
) {
    let entry = find!(ENCODERS, encoder);
    let mut held = entry.lock().unwrap();
    if let Some(pass) = held.pass.as_mut() {
        pass.set_viewport(
            x as f32,
            y as f32,
            width as f32,
            height as f32,
            min_depth as f32,
            max_depth as f32,
        );
    }
}

pub unsafe fn render_set_scissor_rect(encoder: i32, x: i32, y: i32, width: i32, height: i32) {
    let entry = find!(ENCODERS, encoder);
    let mut held = entry.lock().unwrap();
    if let Some(pass) = held.pass.as_mut() {
        pass.set_scissor_rect(
            x.max(0) as u32,
            y.max(0) as u32,
            width.max(0) as u32,
            height.max(0) as u32,
        );
    }
}

pub unsafe fn render_draw(encoder: i32, vertices: i32, instances: i32) {
    let entry = find!(ENCODERS, encoder);
    let mut held = entry.lock().unwrap();
    if let Some(pass) = held.pass.as_mut() {
        pass.draw(0..vertices.max(0) as u32, 0..instances.max(1) as u32);
    }
}

pub unsafe fn encoder_render_end(encoder: i32) {
    let entry = find!(ENCODERS, encoder);
    // Dropping the pass is what ends it.
    entry.lock().unwrap().pass = None;
}

/// The texture side of a copy, always the whole of mip level zero.
fn whole_texture(texture: &wgpu::Texture) -> wgpu::TexelCopyTextureInfo<'_> {
    wgpu::TexelCopyTextureInfo {
        texture,
        mip_level: 0,
        origin: wgpu::Origin3d::ZERO,
        aspect: wgpu::TextureAspect::All,
    }
}

fn extent(width: i32, height: i32) -> wgpu::Extent3d {
    wgpu::Extent3d {
        width: width.max(1) as u32,
        height: height.max(1) as u32,
        depth_or_array_layers: 1,
    }
}

pub unsafe fn encoder_copy_buffer_to_texture(
    encoder: i32,
    buffer: i32,
    bytes_per_row: i32,
    texture: i32,
    width: i32,
    height: i32,
) {
    let entry = find!(ENCODERS, encoder);
    let buffer = find!(BUFFERS, buffer);
    let texture = find!(TEXTURES, texture);
    let mut held = entry.lock().unwrap();
    let Some(encoder) = held.encoder.as_mut() else { return };
    encoder.copy_buffer_to_texture(
        wgpu::TexelCopyBufferInfo {
            buffer: &buffer,
            layout: wgpu::TexelCopyBufferLayout {
                offset: 0,
                bytes_per_row: Some(bytes_per_row.max(0) as u32),
                rows_per_image: Some(height.max(1) as u32),
            },
        },
        whole_texture(&texture),
        extent(width, height),
    );
}

pub unsafe fn encoder_copy_texture_to_texture(
    encoder: i32,
    src: i32,
    dst: i32,
    width: i32,
    height: i32,
) {
    let entry = find!(ENCODERS, encoder);
    let source = find!(TEXTURES, src);
    let target = find!(TEXTURES, dst);
    let mut held = entry.lock().unwrap();
    let Some(encoder) = held.encoder.as_mut() else { return };
    encoder.copy_texture_to_texture(
        whole_texture(&source),
        whole_texture(&target),
        extent(width, height),
    );
}

pub unsafe fn encoder_clear_buffer(encoder: i32, buffer: i32, offset: i32, size: i32) {
    let entry = find!(ENCODERS, encoder);
    let buffer = find!(BUFFERS, buffer);
    let mut held = entry.lock().unwrap();
    let Some(encoder) = held.encoder.as_mut() else { return };
    encoder.clear_buffer(
        &buffer,
        offset.max(0) as u64,
        Some(size.max(0) as u64),
    );
}

pub unsafe fn encoder_copy_texture_to_buffer(
    encoder: i32,
    texture: i32,
    buffer: i32,
    width: i32,
    height: i32,
    bytes_per_row: i32,
) {
    let entry = find!(ENCODERS, encoder);
    let texture = find!(TEXTURES, texture);
    let buffer = find!(BUFFERS, buffer);
    let mut held = entry.lock().unwrap();
    let Some(encoder) = held.encoder.as_mut() else { return };
    encoder.copy_texture_to_buffer(
        wgpu::TexelCopyTextureInfo {
            texture: &texture,
            mip_level: 0,
            origin: wgpu::Origin3d::ZERO,
            aspect: wgpu::TextureAspect::All,
        },
        wgpu::TexelCopyBufferInfo {
            buffer: &buffer,
            layout: wgpu::TexelCopyBufferLayout {
                offset: 0,
                bytes_per_row: Some(bytes_per_row.max(0) as u32),
                rows_per_image: Some(height.max(1) as u32),
            },
        },
        wgpu::Extent3d {
            width: width.max(1) as u32,
            height: height.max(1) as u32,
            depth_or_array_layers: 1,
        },
    );
}

// -- samplers ---------------------------------------------------------------

pub unsafe fn sampler_create(device: i32, filter: i32, address: i32) -> i32 {
    let entry = find!(DEVICES, device, 0);
    let filter = if filter == 1 {
        wgpu::FilterMode::Linear
    } else {
        wgpu::FilterMode::Nearest
    };
    let address = if address == 1 {
        wgpu::AddressMode::Repeat
    } else {
        wgpu::AddressMode::ClampToEdge
    };
    let sampler = entry.device.create_sampler(&wgpu::SamplerDescriptor {
        address_mode_u: address,
        address_mode_v: address,
        address_mode_w: address,
        mag_filter: filter,
        min_filter: filter,
        ..Default::default()
    });
    SAMPLERS.lock().unwrap().put(sampler)
}

pub unsafe fn sampler_destroy(sampler: i32) {
    SAMPLERS.lock().unwrap().remove(sampler);
}

pub unsafe fn queue_write_texture(
    queue: i32,
    texture: i32,
    data: *mut vbyte,
    width: i32,
    height: i32,
    bytes_per_row: i32,
) {
    if data.is_null() || width <= 0 || height <= 0 {
        return;
    }
    let queue = find!(QUEUES, queue);
    let texture = find!(TEXTURES, texture);
    let bytes = std::slice::from_raw_parts(data, (bytes_per_row.max(0) * height) as usize);
    queue.write_texture(
        wgpu::TexelCopyTextureInfo {
            texture: &texture,
            mip_level: 0,
            origin: wgpu::Origin3d::ZERO,
            aspect: wgpu::TextureAspect::All,
        },
        bytes,
        wgpu::TexelCopyBufferLayout {
            offset: 0,
            bytes_per_row: Some(bytes_per_row.max(0) as u32),
            rows_per_image: Some(height as u32),
        },
        wgpu::Extent3d {
            width: width as u32,
            height: height as u32,
            depth_or_array_layers: 1,
        },
    );
}

// -- indexed drawing --------------------------------------------------------

pub unsafe fn render_set_bind_group(encoder: i32, group: i32, bindgroup: i32) {
    let entry = find!(ENCODERS, encoder);
    let bind_group = find!(BINDGROUPS, bindgroup);
    let mut held = entry.lock().unwrap();
    if let Some(pass) = held.pass.as_mut() {
        pass.set_bind_group(group.max(0) as u32, &*bind_group, &[]);
    }
}

pub unsafe fn render_set_index_buffer(encoder: i32, buffer: i32, format: i32) {
    let entry = find!(ENCODERS, encoder);
    let buffer = find!(BUFFERS, buffer);
    let format = if format == 1 {
        wgpu::IndexFormat::Uint32
    } else {
        wgpu::IndexFormat::Uint16
    };
    let mut held = entry.lock().unwrap();
    if let Some(pass) = held.pass.as_mut() {
        pass.set_index_buffer(buffer.slice(..), format);
    }
}

pub unsafe fn render_draw_indexed(encoder: i32, indices: i32, instances: i32) {
    let entry = find!(ENCODERS, encoder);
    let mut held = entry.lock().unwrap();
    if let Some(pass) = held.pass.as_mut() {
        pass.draw_indexed(0..indices.max(0) as u32, 0, 0..instances.max(1) as u32);
    }
}

// -- surfaces ---------------------------------------------------------------

/// A surface and the frame currently acquired on it.
///
/// The frame has to be held between `acquire` and `present`: presenting is
/// what consumes it, and the view handed out points into it.
struct SurfaceEntry {
    surface: wgpu::Surface<'static>,
    frame: Option<wgpu::SurfaceTexture>,
    view: i32,
}

slab!(SURFACES, Mutex<SurfaceEntry>, Kind::Surface);

/// Puts a raw window handle back together from the integers that crossed.
///
/// `hlwindow` took it apart; the two libraries share no Rust type, only these
/// numbers and the platform code that says how to read them.
unsafe fn raw_handles(
    platform: i32,
    wa: i64,
    wb: i64,
    da: i64,
    db: i64,
) -> Option<(raw_window_handle::RawDisplayHandle, raw_window_handle::RawWindowHandle)> {
    use raw_window_handle as rwh;
    use std::ffi::c_void;
    use std::ptr::NonNull;

    Some(match platform {
        1 => (
            rwh::RawDisplayHandle::AppKit(rwh::AppKitDisplayHandle::new()),
            rwh::RawWindowHandle::AppKit(rwh::AppKitWindowHandle::new(NonNull::new(
                wa as *mut c_void,
            )?)),
        ),
        2 => {
            let mut window = rwh::Win32WindowHandle::new(std::num::NonZeroIsize::new(wa as isize)?);
            window.hinstance = std::num::NonZeroIsize::new(wb as isize);
            (
                rwh::RawDisplayHandle::Windows(rwh::WindowsDisplayHandle::new()),
                rwh::RawWindowHandle::Win32(window),
            )
        }
        3 => {
            let mut window = rwh::XlibWindowHandle::new(wa as u64);
            window.visual_id = wb as u64;
            (
                rwh::RawDisplayHandle::Xlib(rwh::XlibDisplayHandle::new(
                    NonNull::new(da as *mut c_void),
                    db as i32,
                )),
                rwh::RawWindowHandle::Xlib(window),
            )
        }
        4 => (
            rwh::RawDisplayHandle::Wayland(rwh::WaylandDisplayHandle::new(NonNull::new(
                da as *mut c_void,
            )?)),
            rwh::RawWindowHandle::Wayland(rwh::WaylandWindowHandle::new(NonNull::new(
                wa as *mut c_void,
            )?)),
        ),
        _ => return None,
    })
}

pub unsafe fn surface_create(
    instance: i32,
    platform: i32,
    wa: i64,
    wb: i64,
    da: i64,
    db: i64,
) -> i32 {
    let instance = find!(INSTANCES, instance, 0);
    let Some((display, window)) = raw_handles(platform, wa, wb, da, db) else {
        return 0;
    };
    // Unsafe because nothing here can prove the window outlives the surface.
    // The Haxe side owns both and closes them in order.
    let made = instance.create_surface_unsafe(wgpu::SurfaceTargetUnsafe::RawHandle {
        raw_display_handle: Some(display),
        raw_window_handle: window,
    });
    match made {
        Ok(surface) => SURFACES
            .lock()
            .unwrap()
            .put(Mutex::new(SurfaceEntry { surface, frame: None, view: 0 })),
        Err(_) => 0,
    }
}

pub unsafe fn surface_preferred_format(surface: i32, adapter: i32) -> i32 {
    let entry = find!(SURFACES, surface, 0);
    let adapter = find!(ADAPTERS, adapter, 0);
    let held = entry.lock().unwrap();
    let formats = held.surface.get_capabilities(&adapter).formats;
    // -1 rather than a default, so a format this library has no name for
    // cannot pass itself off as Rgba8Unorm and fail later inside configure.
    match formats.first() {
        Some(wgpu::TextureFormat::Rgba8Unorm) => 0,
        Some(wgpu::TextureFormat::Bgra8Unorm) => 1,
        Some(wgpu::TextureFormat::Rgba8UnormSrgb) => 2,
        Some(wgpu::TextureFormat::Bgra8UnormSrgb) => 4,
        _ => -1,
    }
}

pub unsafe fn surface_configure(device: i32, surface: i32, width: i32, height: i32, format: i32) {
    let entry = find!(DEVICES, device);
    let surface = find!(SURFACES, surface);
    let held = surface.lock().unwrap();
    held.surface.configure(
        &entry.device,
        &wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: texture_format(format),
            width: width.max(1) as u32,
            height: height.max(1) as u32,
            color_space: wgpu::SurfaceColorSpace::Auto,
            present_mode: wgpu::PresentMode::Fifo,
            desired_maximum_frame_latency: 2,
            alpha_mode: wgpu::CompositeAlphaMode::Auto,
            view_formats: vec![],
        },
    );
}

pub unsafe fn surface_acquire(surface: i32) -> i32 {
    let entry = find!(SURFACES, surface, 0);
    let mut held = entry.lock().unwrap();
    // Suboptimal is still a frame -- a resize usually reports it before it
    // reports Outdated, and refusing it would drop every frame in between.
    let frame = match held.surface.get_current_texture() {
        wgpu::CurrentSurfaceTexture::Success(frame)
        | wgpu::CurrentSurfaceTexture::Suboptimal(frame) => frame,
        _ => return 0,
    };
    let view = frame
        .texture
        .create_view(&wgpu::TextureViewDescriptor::default());
    let handle = VIEWS.lock().unwrap().put(view);
    held.frame = Some(frame);
    held.view = handle;
    handle
}

pub unsafe fn surface_present(queue: i32, surface: i32) {
    let queue = find!(QUEUES, queue);
    let entry = find!(SURFACES, surface);
    let mut held = entry.lock().unwrap();
    // The view points into the frame, so it goes first.
    if held.view != 0 {
        VIEWS.lock().unwrap().remove(held.view);
        held.view = 0;
    }
    if let Some(frame) = held.frame.take() {
        queue.present(frame);
    }
}

pub unsafe fn surface_destroy(surface: i32) {
    SURFACES.lock().unwrap().remove(surface);
}

// -- render pipeline builder ------------------------------------------------

/// A render pipeline under construction.
///
/// Built by a run of calls rather than one packed descriptor, so every value
/// crossing the boundary is a scalar the compiler checks on both sides.
#[derive(Default)]
struct PipelineBuild {
    device: i32,
    shader: i32,
    vertex_entry: String,
    fragment_entry: String,
    buffers: Vec<(u64, wgpu::VertexStepMode, Vec<wgpu::VertexAttribute>)>,
    targets: Vec<(wgpu::TextureFormat, wgpu::ColorWrites, Option<wgpu::BlendState>)>,
    depth: Option<(wgpu::TextureFormat, bool, wgpu::CompareFunction)>,
    primitive: wgpu::PrimitiveState,
}

slab!(BUILDERS, Mutex<PipelineBuild>, Kind::Builder);

// The orders below are the WebGPU IDL's, which is where the Haxe enums and the
// JavaScript name arrays come from too. wgpu's spelling is not derivable from
// the spec's, so this one mapping is written out.
fn blend_factor(i: i32) -> wgpu::BlendFactor {
    use wgpu::BlendFactor as F;
    match i {
        1 => F::One,
        2 => F::Src,
        3 => F::OneMinusSrc,
        4 => F::SrcAlpha,
        5 => F::OneMinusSrcAlpha,
        6 => F::Dst,
        7 => F::OneMinusDst,
        8 => F::DstAlpha,
        9 => F::OneMinusDstAlpha,
        10 => F::SrcAlphaSaturated,
        11 => F::Constant,
        12 => F::OneMinusConstant,
        13 => F::Src1,
        14 => F::OneMinusSrc1,
        15 => F::Src1Alpha,
        16 => F::OneMinusSrc1Alpha,
        _ => F::Zero,
    }
}

fn blend_operation(i: i32) -> wgpu::BlendOperation {
    use wgpu::BlendOperation as O;
    match i {
        1 => O::Subtract,
        2 => O::ReverseSubtract,
        3 => O::Min,
        4 => O::Max,
        _ => O::Add,
    }
}

fn compare_function(i: i32) -> wgpu::CompareFunction {
    use wgpu::CompareFunction as C;
    match i {
        0 => C::Never,
        2 => C::Equal,
        3 => C::LessEqual,
        4 => C::Greater,
        5 => C::NotEqual,
        6 => C::GreaterEqual,
        7 => C::Always,
        _ => C::Less,
    }
}

fn topology(i: i32) -> wgpu::PrimitiveTopology {
    use wgpu::PrimitiveTopology as T;
    match i {
        0 => T::PointList,
        1 => T::LineList,
        2 => T::LineStrip,
        4 => T::TriangleStrip,
        _ => T::TriangleList,
    }
}

/// `none` is the absence of a face rather than a third face, which is why
/// this one is an `Option` where the spec has an enum.
fn cull_mode(i: i32) -> Option<wgpu::Face> {
    match i {
        1 => Some(wgpu::Face::Front),
        2 => Some(wgpu::Face::Back),
        _ => None,
    }
}

fn front_face(i: i32) -> wgpu::FrontFace {
    match i {
        1 => wgpu::FrontFace::Cw,
        _ => wgpu::FrontFace::Ccw,
    }
}

fn step_mode(i: i32) -> wgpu::VertexStepMode {
    match i {
        1 => wgpu::VertexStepMode::Instance,
        _ => wgpu::VertexStepMode::Vertex,
    }
}

pub unsafe fn pipeline_begin(device: i32) -> i32 {
    if DEVICES.lock().unwrap().get(device).is_none() {
        return 0;
    }
    BUILDERS
        .lock()
        .unwrap()
        .put(Mutex::new(PipelineBuild { device, ..Default::default() }))
}

/// Runs `body` on a builder, or does nothing.
fn building(handle: i32, body: impl FnOnce(&mut PipelineBuild)) {
    let Some(entry) = BUILDERS.lock().unwrap().get(handle) else {
        return;
    };
    body(&mut entry.lock().unwrap());
}

pub unsafe fn pipeline_shader(builder: i32, shader: i32, vs: *mut vbyte, fs: *mut vbyte) {
    let (vs, fs) = (ucs2_in(vs), ucs2_in(fs));
    building(builder, |build| {
        build.shader = shader;
        build.vertex_entry = vs;
        build.fragment_entry = fs;
    });
}

pub unsafe fn pipeline_vertex_buffer(builder: i32, stride: i32, step: i32) {
    building(builder, |build| {
        build
            .buffers
            .push((stride.max(0) as u64, step_mode(step), Vec::new()));
    });
}

pub unsafe fn pipeline_attribute(builder: i32, format: i32, offset: i32, location: i32) {
    building(builder, |build| {
        // Belongs to the buffer opened last; the Haxe builder's types are what
        // stop this being reached with none open.
        if let Some((_, _, attributes)) = build.buffers.last_mut() {
            attributes.push(wgpu::VertexAttribute {
                format: vertex_format(format),
                offset: offset.max(0) as u64,
                shader_location: location.max(0) as u32,
            });
        }
    });
}

/// Appends packed against the previous attribute, at the next free location.
///
/// Locations count across every buffer of the pipeline; offsets restart with
/// each buffer, because that is what an offset is relative to.
pub unsafe fn pipeline_attribute_packed(builder: i32, format: i32) {
    building(builder, |build| {
        let location = build
            .buffers
            .iter()
            .map(|(_, _, attributes)| attributes.len())
            .sum::<usize>() as u32;
        if let Some((_, _, attributes)) = build.buffers.last_mut() {
            let offset = attributes
                .last()
                .map_or(0, |a| a.offset + a.format.size());
            attributes.push(wgpu::VertexAttribute {
                format: vertex_format(format),
                offset,
                shader_location: location,
            });
        }
    });
}

pub unsafe fn pipeline_target(builder: i32, format: i32, write_mask: i32) {
    building(builder, |build| {
        build.targets.push((
            texture_format(format),
            wgpu::ColorWrites::from_bits_truncate(write_mask as u32),
            None,
        ));
    });
}

pub unsafe fn pipeline_blend(
    builder: i32,
    src: i32,
    dst: i32,
    op: i32,
    src_alpha: i32,
    dst_alpha: i32,
    op_alpha: i32,
) {
    building(builder, |build| {
        if let Some((_, _, blend)) = build.targets.last_mut() {
            *blend = Some(wgpu::BlendState {
                color: wgpu::BlendComponent {
                    src_factor: blend_factor(src),
                    dst_factor: blend_factor(dst),
                    operation: blend_operation(op),
                },
                alpha: wgpu::BlendComponent {
                    src_factor: blend_factor(src_alpha),
                    dst_factor: blend_factor(dst_alpha),
                    operation: blend_operation(op_alpha),
                },
            });
        }
    });
}

pub unsafe fn pipeline_depth(builder: i32, format: i32, write: bool, compare: i32) {
    building(builder, |build| {
        build.depth = Some((texture_format(format), write, compare_function(compare)));
    });
}

pub unsafe fn pipeline_primitive(builder: i32, topology_of: i32, cull: i32, front: i32) {
    building(builder, |build| {
        build.primitive = wgpu::PrimitiveState {
            topology: topology(topology_of),
            cull_mode: cull_mode(cull),
            front_face: front_face(front),
            ..Default::default()
        };
    });
}

pub unsafe fn render_pipeline_build(builder: i32) -> i32 {
    let Some(entry) = BUILDERS.lock().unwrap().get(builder) else {
        return 0;
    };
    let build = entry.lock().unwrap();
    let device = find!(DEVICES, build.device, 0);
    let module = find!(SHADERS, build.shader, 0);

    // Held so the descriptor below can borrow them.
    let layouts: Vec<wgpu::VertexBufferLayout> = build
        .buffers
        .iter()
        .map(|(stride, step, attributes)| wgpu::VertexBufferLayout {
            // Zero means the caller did not say, so it is as wide as the
            // attributes turned out to be.
            array_stride: if *stride != 0 {
                *stride
            } else {
                attributes
                    .iter()
                    .map(|a| a.offset + a.format.size())
                    .max()
                    .unwrap_or(0)
            },
            step_mode: *step,
            attributes,
        })
        .collect();
    let buffers: Vec<Option<wgpu::VertexBufferLayout>> =
        layouts.into_iter().map(Some).collect();
    let targets: Vec<Option<wgpu::ColorTargetState>> = build
        .targets
        .iter()
        .map(|(format, write_mask, blend)| {
            Some(wgpu::ColorTargetState {
                format: *format,
                blend: *blend,
                write_mask: *write_mask,
            })
        })
        .collect();
    let depth_stencil = build
        .depth
        .map(|(format, write, compare)| wgpu::DepthStencilState {
            format,
            depth_write_enabled: Some(write),
            depth_compare: Some(compare),
            stencil: Default::default(),
            bias: Default::default(),
        });

    let pipeline = device
        .device
        .create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: None,
            layout: None,
            vertex: wgpu::VertexState {
                module: &module,
                entry_point: Some(build.vertex_entry.as_str()),
                buffers: &buffers,
                compilation_options: Default::default(),
            },
            fragment: Some(wgpu::FragmentState {
                module: &module,
                entry_point: Some(build.fragment_entry.as_str()),
                targets: &targets,
                compilation_options: Default::default(),
            }),
            primitive: build.primitive,
            depth_stencil,
            multisample: Default::default(),
            multiview_mask: Default::default(),
            cache: None,
        });
    drop(build);
    BUILDERS.lock().unwrap().remove(builder);
    RENDER_PIPELINES.lock().unwrap().put(pipeline)
}
