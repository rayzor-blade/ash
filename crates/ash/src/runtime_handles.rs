//! Runtime handles shared between the interpreter and every compiled tier.
//! LLVM-free so the Cranelift tier and hot reload can use them without it.

use crate::hl::*;
use std::ffi::c_void;

#[derive(Debug, Clone)]
pub struct SharedRuntimeHandles {
    pub globals_data_ptr: *mut *mut c_void,
    pub nglobals: usize,
    pub c_types: Vec<*mut hl_type>,
    pub module_ctx: *mut hl_module_context,
}

// SharedRuntimeHandles carries runtime pointers that are process-global for an HL module
// and are read by the background JIT worker. Synchronization remains the caller's responsibility.
unsafe impl Send for SharedRuntimeHandles {}
unsafe impl Sync for SharedRuntimeHandles {}

#[derive(Debug, Clone)]
pub struct CompiledFunctionMeta {
    pub findex: usize,
    pub fn_addr: usize,
    pub arg_kinds: Vec<hl_type_kind>,
    pub ret_kind: hl_type_kind,
}

impl CompiledFunctionMeta {
    /// An AOT lowering has no address: the function is a symbol in the module
    /// and gets one when the object is linked. `fn_addr == 0` is the marker,
    /// and nothing in the AOT path dispatches through it.
    pub fn aot_placeholder(findex: usize) -> Self {
        CompiledFunctionMeta {
            findex,
            fn_addr: 0,
            arg_kinds: Vec::new(),
            ret_kind: 0,
        }
    }
}
