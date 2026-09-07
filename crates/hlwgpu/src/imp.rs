//! What the primitives do, over `wgpu`.
//!
//! Serves the native library, and later a host answering the wasm imports.
//! Nothing here is generated; `bindings` is the part that is.

use std::sync::atomic::{AtomicBool, AtomicI32, Ordering};
use std::sync::{Arc, LazyLock, Mutex};

use hl_abi::{hlp_alloc_bytes, vbyte};
use crate::bindings::kinds::Kind;
use crate::handles::{PendingRequests, Slab};

/// A device and the queue that came back with it.
struct DeviceEntry {
    device: wgpu::Device,
    queue: i32,
}

/// An encoder is consumed by `finish()`, so it has to be takeable.
type Encoder = Mutex<Option<wgpu::CommandEncoder>>;

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

pub unsafe fn bind_group_create(
    device: i32,
    pipeline: i32,
    group: i32,
    buffers: *mut vbyte,
    count: i32,
) -> i32 {
    if buffers.is_null() || count <= 0 {
        return 0;
    }
    let entry = find!(DEVICES, device, 0);
    let pipeline = find!(PIPELINES, pipeline, 0);
    let handles = std::slice::from_raw_parts(buffers as *const i32, count as usize);

    // Held so the borrows below outlive the descriptor.
    let mut found = Vec::with_capacity(handles.len());
    for handle in handles {
        match BUFFERS.lock().unwrap().get(*handle) {
            Some(buffer) => found.push(buffer),
            None => return 0,
        }
    }
    let entries: Vec<wgpu::BindGroupEntry> = found
        .iter()
        .enumerate()
        .map(|(binding, buffer)| wgpu::BindGroupEntry {
            binding: binding as u32,
            resource: buffer.as_entire_binding(),
        })
        .collect();

    let bind_group = entry.device.create_bind_group(&wgpu::BindGroupDescriptor {
        label: None,
        layout: &pipeline.get_bind_group_layout(group.max(0) as u32),
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
    ENCODERS.lock().unwrap().put(Mutex::new(Some(encoder)))
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
    let Some(encoder) = held.as_mut() else { return };

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
    let Some(encoder) = held.as_mut() else { return };
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
    let taken = encoder.lock().unwrap().take();
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
