# What a host must supply for `wgpu`

GENERATED from `wgpu.api`.

Natively none of this applies: `wgpu.hdll` holds the implementation and
answers the VM directly.

On wasm, `wgpu.wasm` holds no implementation. It imports the following
from `env`, and whatever instantiates the module has to provide them.
A page gets them from `hlwgpu.js`; any other host implements this table.

Every handle is an `i32`, and every string is a pointer to
NUL-terminated UTF-16 allocated through the program's `hlp_alloc_bytes`.

| import | parameters | returns |
|---|---|---|
| `hlwgpu_instance_create` | -- | `i32` |
| `hlwgpu_instance_destroy` | `inst: i32` | `void` |
| `hlwgpu_adapter_request` | `inst: i32`, `power: i32` | `i32` |
| `hlwgpu_request_ready` | `request: i32` | `i32` |
| `hlwgpu_request_result` | `request: i32` | `i32` |
| `hlwgpu_adapter_name` | `adapter: i32` | `i32` |
| `hlwgpu_adapter_backend` | `adapter: i32` | `i32` |
| `hlwgpu_adapter_limit` | `adapter: i32`, `which: i32` | `i32` |
| `hlwgpu_adapter_destroy` | `adapter: i32` | `void` |
| `hlwgpu_device_request` | `adapter: i32` | `i32` |
| `hlwgpu_device_queue` | `device: i32` | `i32` |
| `hlwgpu_device_poll` | `device: i32` | `void` |
| `hlwgpu_device_destroy` | `device: i32` | `void` |
| `hlwgpu_buffer_create` | `device: i32`, `size: i32`, `usage: i32` | `i32` |
| `hlwgpu_queue_write_buffer` | `queue: i32`, `buffer: i32`, `offset: i32`, `data: i32`, `len: i32` | `void` |
| `hlwgpu_buffer_map_begin` | `device: i32`, `buffer: i32`, `offset: i32`, `size: i32` | `i32` |
| `hlwgpu_buffer_copy_out` | `buffer: i32`, `offset: i32`, `out: i32`, `len: i32` | `i32` |
| `hlwgpu_buffer_unmap` | `buffer: i32` | `void` |
| `hlwgpu_buffer_destroy` | `buffer: i32` | `void` |
| `hlwgpu_shader_create` | `device: i32`, `wgsl: i32` | `i32` |
| `hlwgpu_shader_destroy` | `shader: i32` | `void` |
| `hlwgpu_compute_pipeline_create` | `device: i32`, `shader: i32`, `entry: i32` | `i32` |
| `hlwgpu_pipeline_destroy` | `pipeline: i32` | `void` |
| `hlwgpu_bind_group_create` | `device: i32`, `pipeline: i32`, `group: i32`, `bound: i32`, `count: i32` | `i32` |
| `hlwgpu_bind_group_destroy` | `bindgroup: i32` | `void` |
| `hlwgpu_encoder_create` | `device: i32` | `i32` |
| `hlwgpu_encoder_compute` | `encoder: i32`, `pipeline: i32`, `bindgroup: i32`, `x: i32`, `y: i32`, `z: i32` | `void` |
| `hlwgpu_encoder_copy_buffer` | `encoder: i32`, `src: i32`, `src_offset: i32`, `dst: i32`, `dst_offset: i32`, `size: i32` | `void` |
| `hlwgpu_encoder_submit` | `encoder: i32`, `queue: i32` | `void` |
| `hlwgpu_queue_work_done` | `device: i32`, `queue: i32` | `i32` |
| `hlwgpu_texture_create` | `device: i32`, `width: i32`, `height: i32`, `format: i32`, `usage: i32` | `i32` |
| `hlwgpu_texture_view` | `texture: i32` | `i32` |
| `hlwgpu_texture_destroy` | `texture: i32` | `void` |
| `hlwgpu_view_destroy` | `view: i32` | `void` |
| `hlwgpu_render_pipeline_create` | `device: i32`, `shader: i32`, `vs: i32`, `fs: i32`, `format: i32`, `stride: i32`, `attrs: i32`, `count: i32` | `i32` |
| `hlwgpu_render_pipeline_destroy` | `pipeline: i32` | `void` |
| `hlwgpu_encoder_render_begin` | `encoder: i32`, `view: i32`, `r: f64`, `g: f64`, `b: f64`, `a: f64` | `void` |
| `hlwgpu_render_set_pipeline` | `encoder: i32`, `pipeline: i32` | `void` |
| `hlwgpu_render_set_vertex_buffer` | `encoder: i32`, `slot: i32`, `buffer: i32` | `void` |
| `hlwgpu_render_draw` | `encoder: i32`, `vertices: i32`, `instances: i32` | `void` |
| `hlwgpu_encoder_render_end` | `encoder: i32` | `void` |
| `hlwgpu_encoder_copy_texture_to_buffer` | `encoder: i32`, `texture: i32`, `buffer: i32`, `width: i32`, `height: i32`, `bytes_per_row: i32` | `void` |
| `hlwgpu_sampler_create` | `device: i32`, `filter: i32`, `address: i32` | `i32` |
| `hlwgpu_sampler_destroy` | `sampler: i32` | `void` |
| `hlwgpu_queue_write_texture` | `queue: i32`, `texture: i32`, `data: i32`, `width: i32`, `height: i32`, `bytes_per_row: i32` | `void` |
| `hlwgpu_render_set_bind_group` | `encoder: i32`, `group: i32`, `bindgroup: i32` | `void` |
| `hlwgpu_render_set_index_buffer` | `encoder: i32`, `buffer: i32`, `format: i32` | `void` |
| `hlwgpu_render_draw_indexed` | `encoder: i32`, `indices: i32`, `instances: i32` | `void` |
| `hlwgpu_surface_create` | `instance: i32`, `platform: i32`, `wa: i64`, `wb: i64`, `da: i64`, `db: i64` | `i32` |
| `hlwgpu_surface_preferred_format` | `surface: i32`, `adapter: i32` | `i32` |
| `hlwgpu_surface_configure` | `device: i32`, `surface: i32`, `width: i32`, `height: i32`, `format: i32` | `void` |
| `hlwgpu_surface_acquire` | `surface: i32` | `i32` |
| `hlwgpu_surface_present` | `queue: i32`, `surface: i32` | `void` |
| `hlwgpu_surface_destroy` | `surface: i32` | `void` |
