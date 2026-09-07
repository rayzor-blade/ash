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
| `hlwgpu_bind_group_create` | `device: i32`, `pipeline: i32`, `group: i32`, `buffers: i32`, `count: i32` | `i32` |
| `hlwgpu_bind_group_destroy` | `bindgroup: i32` | `void` |
| `hlwgpu_encoder_create` | `device: i32` | `i32` |
| `hlwgpu_encoder_compute` | `encoder: i32`, `pipeline: i32`, `bindgroup: i32`, `x: i32`, `y: i32`, `z: i32` | `void` |
| `hlwgpu_encoder_copy_buffer` | `encoder: i32`, `src: i32`, `src_offset: i32`, `dst: i32`, `dst_offset: i32`, `size: i32` | `void` |
| `hlwgpu_encoder_submit` | `encoder: i32`, `queue: i32` | `void` |
| `hlwgpu_queue_work_done` | `device: i32`, `queue: i32` | `i32` |
