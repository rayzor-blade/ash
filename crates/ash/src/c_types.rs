use crate::hl::*;
use crate::types::{HLType, TypeRef};
use crate::bytecode::DecodedBytecode;
use std::collections::HashMap;
use std::ptr;

/// Convert a Rust &str to a null-terminated UTF-16 pointer.
/// HashLink uses UCS-2/UTF-16 strings internally, so all name fields
/// in hl_type, hl_obj_field, etc. must be UTF-16 null-terminated.
fn str_to_utf16_ptr(s: &str) -> *const u16 {
    let mut utf16: Vec<u16> = s.encode_utf16().collect();
    utf16.push(0); // null terminator
    let ptr = utf16.as_ptr();
    std::mem::forget(utf16);
    ptr
}

/// Factory for creating C-level `hl_type` structures from decoded bytecode.
/// These are needed by native stdlib functions that expect `*mut hl_type` pointers.
pub struct CTypeFactory {
    /// Cached C-level type pointers, indexed by type index
    c_types: Vec<*mut hl_type>,
    /// Module context with function pointer stubs (leaked, never freed)
    module_ctx: *mut hl_module_context,
    /// Shared globals data array - global_value pointers point into this.
    /// This is the canonical store that native stdlib code reads/writes.
    globals_data: *mut *mut std::ffi::c_void,
    /// Number of globals
    nglobals: usize,
}

impl CTypeFactory {
    /// Create C-level type structures for all types in the bytecode.
    pub fn new(bytecode: &DecodedBytecode) -> Self {
        let nglobals = bytecode.globals.len();

        // Allocate shared globals_data array (leaked, never freed)
        let globals_data = {
            let mut data: Vec<*mut std::ffi::c_void> = vec![ptr::null_mut(); nglobals];
            let ptr = data.as_mut_ptr();
            std::mem::forget(data);
            ptr
        };

        let mut factory = CTypeFactory {
            c_types: Vec::with_capacity(bytecode.types.len()),
            module_ctx: ptr::null_mut(),
            globals_data,
            nglobals,
        };

        // First pass: create placeholder types with correct `kind`
        for hl_type_rust in &bytecode.types {
            let c_type = Box::into_raw(Box::new(hl_type {
                kind: hl_type_rust.kind,
                __bindgen_anon_1: hl_type__bindgen_ty_1 {
                    abs_name: ptr::null(),
                },
                vobj_proto: ptr::null_mut(),
                mark_bits: ptr::null_mut(),
            }));
            factory.c_types.push(c_type);
        }

        // Second pass: fill in type-specific data with cross-references.
        // Wire global_value pointers into the shared globals_data array.
        for (i, hl_type_rust) in bytecode.types.iter().enumerate() {
            unsafe {
                factory.fill_type_data(i, hl_type_rust);
            }
        }

        // Third pass: create module context with stub function pointers
        // and assign it to all obj/struct types so that bindings work.
        unsafe {
            factory.create_module_context(bytecode);
        }

        factory
    }

    /// Get the C-level type pointer for a given type index.
    pub fn get(&self, index: usize) -> *mut hl_type {
        if index < self.c_types.len() {
            self.c_types[index]
        } else {
            ptr::null_mut()
        }
    }

    /// Get all C type pointers.
    pub fn as_slice(&self) -> &[*mut hl_type] {
        &self.c_types
    }

    /// Get the module context pointer (for accessing function pointer stubs).
    pub fn module_ctx(&self) -> *mut hl_module_context {
        self.module_ctx
    }

    /// Get the shared globals_data array and its size.
    /// Native stdlib writes to global_value slots which point into this array.
    pub fn globals_data(&self) -> (*mut *mut std::ffi::c_void, usize) {
        (self.globals_data, self.nglobals)
    }

    unsafe fn fill_type_data(&mut self, index: usize, rust_type: &HLType) {
        let c_type_ptr = self.c_types[index];

        match rust_type.kind {
            hl_type_kind_HOBJ | hl_type_kind_HSTRUCT => {
                if let Some(ref obj) = rust_type.obj {
                    // Create fields array
                    let fields = if obj.fields.is_empty() {
                        ptr::null_mut()
                    } else {
                        let mut c_fields: Vec<hl_obj_field> = obj
                            .fields
                            .iter()
                            .map(|f| {
                                hl_obj_field {
                                    name: str_to_utf16_ptr(&f.name),
                                    t: self.resolve_type_ref(&f.type_),
                                    hashed_name: f.hashed_name,
                                }
                            })
                            .collect();
                        let ptr = c_fields.as_mut_ptr();
                        std::mem::forget(c_fields);
                        ptr
                    };

                    // Create proto array
                    let proto = if obj.proto.is_empty() {
                        ptr::null_mut()
                    } else {
                        let mut c_protos: Vec<hl_obj_proto> = obj
                            .proto
                            .iter()
                            .map(|p| hl_obj_proto {
                                name: str_to_utf16_ptr(&p.name),
                                findex: p.findex,
                                pindex: p.pindex,
                                hashed_name: p.hashed_name,
                            })
                            .collect();
                        let ptr = c_protos.as_mut_ptr();
                        std::mem::forget(c_protos);
                        ptr
                    };

                    // Wire global_value to point into the shared globals_data array.
                    // NOTE: bytecoded global_value is 1-BASED (0 means "no global").
                    // The C HashLink VM does: globals_indexes[global_value - 1]
                    // We simplify to: globals_data[global_value - 1] since our array
                    // uses one void* pointer per global slot.
                    let global_value = if obj.global_value > 0 {
                        let gv_idx = (obj.global_value - 1) as usize;
                        if gv_idx < self.nglobals {
                            self.globals_data.add(gv_idx)
                        } else {
                            // Fallback: allocate independent slot
                            Box::into_raw(Box::new(ptr::null_mut::<std::ffi::c_void>()))
                        }
                    } else {
                        // global_value=0 means no global → NULL pointer
                        ptr::null_mut()
                    };

                    let c_obj = Box::new(hl_type_obj {
                        nfields: obj.fields.len() as i32,
                        nproto: obj.proto.len() as i32,
                        nbindings: (obj.bindings.len() / 2) as i32,
                        name: str_to_utf16_ptr(&obj.name),
                        super_: obj
                            .super_
                            .as_ref()
                            .map(|s| self.resolve_type_ref(s))
                            .unwrap_or(ptr::null_mut()),
                        fields,
                        proto,
                        bindings: if obj.bindings.is_empty() {
                            ptr::null_mut()
                        } else {
                            let mut bindings = obj.bindings.clone();
                            let ptr = bindings.as_mut_ptr();
                            std::mem::forget(bindings);
                            ptr
                        },
                        global_value,
                        m: ptr::null_mut(),
                        rt: ptr::null_mut(),
                    });
                    (*c_type_ptr).__bindgen_anon_1.obj = Box::into_raw(c_obj);
                }
            }
            hl_type_kind_HFUN | hl_type_kind_HMETHOD => {
                if let Some(ref fun) = rust_type.fun {
                    let args = if fun.args.is_empty() {
                        ptr::null_mut()
                    } else {
                        let mut c_args: Vec<*mut hl_type> = fun
                            .args
                            .iter()
                            .map(|a| self.resolve_type_ref(a))
                            .collect();
                        let ptr = c_args.as_mut_ptr();
                        std::mem::forget(c_args);
                        ptr
                    };

                    let c_fun = Box::new(hl_type_fun {
                        args,
                        ret: self.resolve_type_ref(&fun.ret),
                        nargs: fun.args.len() as i32,
                        parent: fun
                            .parent
                            .as_ref()
                            .map(|p| self.resolve_type_ref(p))
                            .unwrap_or(ptr::null_mut()),
                        closure_type: hl_type_fun__bindgen_ty_1 {
                            kind: 0,
                            p: ptr::null_mut(),
                        },
                        closure: hl_type_fun__bindgen_ty_2 {
                            args: ptr::null_mut(),
                            ret: ptr::null_mut(),
                            nargs: 0,
                            parent: ptr::null_mut(),
                        },
                    });
                    (*c_type_ptr).__bindgen_anon_1.fun = Box::into_raw(c_fun);
                }
            }
            hl_type_kind_HENUM => {
                if let Some(ref tenum) = rust_type.tenum {
                    let constructs = if tenum.constructs.is_empty() {
                        ptr::null_mut()
                    } else {
                        let mut c_constructs: Vec<hl_enum_construct> = tenum
                            .constructs
                            .iter()
                            .map(|c| {
                                let params = if c.params.is_empty() {
                                    ptr::null_mut()
                                } else {
                                    let mut c_params: Vec<*mut hl_type> = c
                                        .params
                                        .iter()
                                        .map(|p| self.resolve_type_ref(p))
                                        .collect();
                                    let ptr = c_params.as_mut_ptr();
                                    std::mem::forget(c_params);
                                    ptr
                                };
                                let offsets = if c.offsets.is_empty() {
                                    ptr::null_mut()
                                } else {
                                    let mut offs = c.offsets.clone();
                                    let ptr = offs.as_mut_ptr();
                                    std::mem::forget(offs);
                                    ptr
                                };
                                hl_enum_construct {
                                    name: str_to_utf16_ptr(&c.name),
                                    nparams: c.params.len() as i32,
                                    params,
                                    size: c.size,
                                    hasptr: c.hasptr,
                                    offsets,
                                }
                            })
                            .collect();
                        let ptr = c_constructs.as_mut_ptr();
                        std::mem::forget(c_constructs);
                        ptr
                    };

                    // Wire enum global_value into shared globals_data array
                    // (1-based: 0 means no global, N means slot N-1)
                    let global_value = if tenum.global_value > 0 {
                        let gv_idx = (tenum.global_value - 1) as usize;
                        if gv_idx < self.nglobals {
                            self.globals_data.add(gv_idx)
                        } else {
                            Box::into_raw(Box::new(ptr::null_mut::<std::ffi::c_void>()))
                        }
                    } else {
                        ptr::null_mut()
                    };

                    let c_enum = Box::new(hl_type_enum {
                        name: str_to_utf16_ptr(&tenum.name),
                        global_value,
                        nconstructs: tenum.constructs.len() as i32,
                        constructs,
                    });
                    (*c_type_ptr).__bindgen_anon_1.tenum = Box::into_raw(c_enum);
                }
            }
            hl_type_kind_HVIRTUAL => {
                if let Some(ref virt) = rust_type.virt {
                    let fields = if virt.fields.is_empty() {
                        ptr::null_mut()
                    } else {
                        let mut c_fields: Vec<hl_obj_field> = virt
                            .fields
                            .iter()
                            .map(|f| {
                                hl_obj_field {
                                    name: str_to_utf16_ptr(&f.name),
                                    t: self.resolve_type_ref(&f.type_),
                                    hashed_name: f.hashed_name,
                                }
                            })
                            .collect();
                        let ptr = c_fields.as_mut_ptr();
                        std::mem::forget(c_fields);
                        ptr
                    };

                    let c_virt = Box::new(hl_type_virtual {
                        fields,
                        nfields: virt.fields.len() as i32,
                        dataSize: virt.data_size as i32,
                        indexes: if virt.indexes.is_empty() {
                            ptr::null_mut()
                        } else {
                            let mut idxs = virt.indexes.clone();
                            let ptr = idxs.as_mut_ptr();
                            std::mem::forget(idxs);
                            ptr
                        },
                        lookup: ptr::null_mut(),
                    });
                    (*c_type_ptr).__bindgen_anon_1.virt = Box::into_raw(c_virt);
                }
            }
            hl_type_kind_HABSTRACT => {
                if let Some(ref name) = rust_type.abs_name {
                    (*c_type_ptr).__bindgen_anon_1.abs_name = str_to_utf16_ptr(name);
                }
            }
            hl_type_kind_HNULL | hl_type_kind_HREF | hl_type_kind_HPACKED => {
                if let Some(ref tparam) = rust_type.tparam {
                    (*c_type_ptr).__bindgen_anon_1.tparam = self.resolve_type_ref(tparam);
                }
            }
            _ => {
                // Primitive types (void, i32, f64, bool, bytes, etc.) need no extra data
            }
        }
    }

    /// Create a module context with stub function pointers for all functions.
    /// This is needed so that `hlp_get_obj_rt` can set up bindings (which reference
    /// function pointers from the module context), and `hlp_alloc_obj` can initialize
    /// object fields with closure objects.
    ///
    /// The stub pointers encode the findex but are never actually called as machine code.
    /// The interpreter dispatches calls by findex, not by function pointer.
    unsafe fn create_module_context(&mut self, bytecode: &DecodedBytecode) {
        // Compute max findex to size the arrays
        let max_findex = bytecode
            .functions
            .iter()
            .map(|f| f.findex as usize)
            .chain(bytecode.natives.iter().map(|n| n.findex as usize))
            .max()
            .unwrap_or(0)
            + 1;

        // Allocate function pointer arrays (leaked, never freed)
        let mut func_ptrs: Vec<*mut std::ffi::c_void> = vec![ptr::null_mut(); max_findex];
        let mut func_types: Vec<*mut hl_type> = vec![ptr::null_mut(); max_findex];

        // Fill in bytecode function stubs (non-null so bindings work)
        for f in &bytecode.functions {
            let idx = f.findex as usize;
            if idx < max_findex {
                // Use findex+1 as a non-null stub (never called as code)
                func_ptrs[idx] = (idx + 1) as *mut std::ffi::c_void;
                func_types[idx] = self.get(f.type_.0);
            }
        }

        // Fill in native function stubs similarly
        for n in &bytecode.natives {
            let idx = n.findex as usize;
            if idx < max_findex {
                func_ptrs[idx] = (idx + 1) as *mut std::ffi::c_void;
                func_types[idx] = self.get(n.type_.0);
            }
        }

        let ptrs_ptr = func_ptrs.as_mut_ptr();
        std::mem::forget(func_ptrs);
        let types_ptr = func_types.as_mut_ptr();
        std::mem::forget(func_types);

        // Create the module context
        let ctx = Box::into_raw(Box::new(hl_module_context {
            alloc: hl_alloc {
                cur: ptr::null_mut(),
            },
            functions_ptrs: ptrs_ptr,
            functions_types: types_ptr,
        }));
        self.module_ctx = ctx;

        // Assign the module context to all obj/struct types
        for c_type_ptr in &self.c_types {
            let kind = (**c_type_ptr).kind;
            if kind == hl_type_kind_HOBJ || kind == hl_type_kind_HSTRUCT {
                let obj = (**c_type_ptr).__bindgen_anon_1.obj;
                if !obj.is_null() {
                    (*obj).m = ctx;
                }
            }
        }
    }

    fn resolve_type_ref(&self, type_ref: &TypeRef) -> *mut hl_type {
        if type_ref.0 < self.c_types.len() {
            self.c_types[type_ref.0]
        } else {
            ptr::null_mut()
        }
    }
}
