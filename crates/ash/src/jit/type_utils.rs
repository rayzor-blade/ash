use crate::hl::*;
use crate::types::*;
use anyhow::{anyhow, Result};
use libc::c_char;
use libc::{c_void, malloc};
use num_enum::TryFromPrimitive;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::CStr;
use std::ffi::CString;
use std::mem;
use std::ptr;
use std::rc::Rc;
use std::slice;

use super::module::JITModule; 



impl<'ctx> JITModule<'ctx> {
    pub(crate) fn convert_to_c_type(
        &mut self,
        index: usize,
        hl_type: &HLType,
        cache: Rc<RefCell<HashMap<usize, *mut hl_type>>>,
    ) -> Result<*mut hl_type> {
        let mut c_type = hl_type {
            kind: hl_type.kind,
            __bindgen_anon_1: hl_type__bindgen_ty_1 {
                abs_name: ptr::null(),
            },
            vobj_proto: ptr::null_mut(),
            mark_bits: ptr::null_mut(),
        };

        if let Some(found) = self.c_ptr_to_type_index.iter().find(|(_, i)| **i == index) {
            return Ok(*found.0 as *mut hl_type);
        }

        // Create a cache to store types we've started converting
        let cache: Rc<RefCell<HashMap<usize, *mut hl_type>>> = Rc::clone(&cache);

        match hl_type.kind {
            hl_type_kind_HABSTRACT => {
                if let Some(ref name) = hl_type.abs_name {
                    let c_str = CString::new(name.as_str())?;
                    c_type.__bindgen_anon_1.abs_name = c_str.into_raw() as *const u16;
                    // mem::forget(c_str); // Prevent deallocation
                }
            }
            hl_type_kind_HOBJ | hl_type_kind_HSTRUCT => {
                if let Some(ref obj) = hl_type.obj {
                    let c_obj = Box::new(hl_type_obj {
                        nfields: obj.fields.len() as i32,
                        nproto: obj.proto.len() as i32,
                        nbindings: obj.bindings.len() as i32 / 2,
                        name: CString::new(obj.name.as_str())?.into_raw() as *const u16,
                        super_: if let Some(ref super_type) = obj.super_ {
                            self.convert_to_c_type(
                                super_type.0.clone(),
                                &self.types_[super_type.0].clone(),
                                Rc::clone(&cache)
                            )?
                        } else {
                            ptr::null_mut()
                        },
                        fields: self.convert_fields_to_c(&obj.fields, Rc::clone(&cache))?,
                        proto: self.convert_proto_to_c(&obj.proto)?,
                        bindings: self.convert_bindings_to_c(&obj.bindings),
                        global_value: if obj.global_value > 0 {
                            let gv_idx = (obj.global_value - 1) as usize;
                            if gv_idx < self.globals_data.len() {
                                unsafe { self.globals_data.as_ptr().add(gv_idx) as *mut *mut c_void }
                            } else {
                                ptr::null_mut()
                            }
                        } else {
                            ptr::null_mut()
                        },
                        m: ptr::null_mut(),
                        rt: ptr::null_mut(),
                    });
                    c_type.__bindgen_anon_1.obj = Box::into_raw(c_obj);
                }
            }
            hl_type_kind_HFUN | hl_type_kind_HMETHOD => {
                if let Some(ref fun) = hl_type.fun {
                    let c_fun = Box::new(hl_type_fun {
                        args: self.convert_type_refs_to_c(&fun.args, Rc::clone(&cache))?,
                        ret: self.convert_to_c_type(
                            fun.ret.0.clone(),
                            &self.types_[fun.ret.0].clone(),
                            Rc::clone(&cache)
                        )?,
                        nargs: fun.args.len() as i32,
                         parent: if let Some(parent) = &fun.parent{
                            self.convert_to_c_type(
                                parent.0.clone(),
                                &self.types_[parent.0].clone(),
                                Rc::clone(&cache)
                            )?
                        }else {ptr::null_mut()},
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
                    c_type.__bindgen_anon_1.fun = Box::into_raw(c_fun);
                }
            }
            hl_type_kind_HENUM => {
                if let Some(ref tenum) = hl_type.tenum {
                    let c_enum = Box::new(hl_type_enum {
                        name: CString::new(tenum.name.as_str())?.into_raw() as *const u16,
                        global_value: if tenum.global_value > 0 {
                            let gv_idx = (tenum.global_value - 1) as usize;
                            if gv_idx < self.globals_data.len() {
                                unsafe { self.globals_data.as_ptr().add(gv_idx) as *mut *mut c_void }
                            } else {
                                ptr::null_mut()
                            }
                        } else {
                            ptr::null_mut()
                        },
                        nconstructs: tenum.constructs.len() as i32,
                        constructs: self.convert_constructs_to_c(&tenum.constructs, Rc::clone(&cache))?,
                    });
                    c_type.__bindgen_anon_1.tenum = Box::into_raw(c_enum);
                }
            }
            hl_type_kind_HVIRTUAL => {
                if let Some(ref virt) = hl_type.virt {
                    let c_virt = Box::new(hl_type_virtual {
                        fields: self.convert_fields_to_c(&virt.fields, Rc::clone(&cache))?,
                        nfields: virt.fields.len() as i32,
                        dataSize: virt.data_size as i32,
                        indexes: self.convert_indexes_to_c(&virt.indexes),
                        lookup: ptr::null_mut(), // You might need to handle this field
                    });
                    c_type.__bindgen_anon_1.virt = Box::into_raw(c_virt);
                }
            }
            _ => {} //return Err(anyhow!("Unsupported HL type kind: {:?}", ValueTypeKind::try_from_primitive(hl_type.kind).unwrap())),
        }
        let t = Box::into_raw(Box::new(c_type));
        self.c_ptr_to_type_index.insert(t as usize, index);
        Ok(t)
    }

    pub(crate) fn get_module_context(&mut self, cache: Rc<RefCell<HashMap<usize, *mut hl_type>>>) -> Result<*mut hl_module_context> {
        let mut context = hl_module_context {
            alloc: unsafe { mem::zeroed() },
            functions_ptrs: std::ptr::null_mut(),
            functions_types: std::ptr::null_mut(),
        };

        // Create a cache to store types we've started converting
        let cache: Rc<RefCell<HashMap<usize, *mut hl_type>>> = Rc::clone(&cache);

        // context.functions_types = self.convert_type_refs_to_c(&self.func_types.clone(), Rc::clone(&cache))?;

        Ok(Box::into_raw(Box::new(context)))
    }

    pub(crate) fn convert_fields_to_c(
        &mut self,
        fields: &[HLObjField],
        cache: Rc<RefCell<HashMap<usize, *mut hl_type>>>
    ) -> Result<*mut hl_obj_field> {
        let mut c_fields = Vec::with_capacity(fields.len());
        for field in fields {
            let c_field = hl_obj_field {
                name: CString::new(field.name.as_str())?.into_raw() as *const u16,
                t: self.convert_type_ref_to_c_cached(
                    &field.type_.clone(),
                    Rc::clone(&cache)
                )?,
                hashed_name: field.hashed_name,
            };
            c_fields.push(c_field);
        }
        let ptr = c_fields.as_mut_ptr();
        mem::forget(c_fields);
        Ok(ptr)
    }

    fn convert_indexes_to_c(&self, indexes: &[i32]) -> *mut i32 {
        let mut c_indexes = indexes.to_vec();
        let ptr = c_indexes.as_mut_ptr();
        mem::forget(c_indexes);
        ptr
    }

    fn convert_proto_to_c(&self, proto: &[HLObjProto]) -> Result<*mut hl_obj_proto> {
        let mut c_proto = Vec::with_capacity(proto.len());
        for p in proto {
            let c_p = hl_obj_proto {
                name: CString::new(p.name.as_str())?.into_raw() as *const u16,
                findex: p.findex,
                pindex: p.pindex,
                hashed_name: p.hashed_name,
            };

            c_proto.push(c_p);
        }
        let ptr = c_proto.as_mut_ptr();
        mem::forget(c_proto);
        Ok(ptr)
    }

    fn convert_bindings_to_c(&self, bindings: &[i32]) -> *mut i32 {
        let mut c_bindings = bindings.to_vec();
        let ptr = c_bindings.as_mut_ptr();
        mem::forget(c_bindings);
        ptr
    }

    // fn convert_type_refs_to_c(&mut self, type_refs: &[TypeRef]) -> Result<*mut *mut hl_type> {
    //     let mut c_type_refs = Vec::with_capacity(type_refs.len());
    //     for type_ref in type_refs {
    //         let c_type = self.convert_to_c_type(type_ref.0, &self.types_[type_ref.0].clone())?;
    //         c_type_refs.push(c_type);
    //     }
    //     let ptr = c_type_refs.as_mut_ptr();
    //     mem::forget(c_type_refs);
    //     Ok(ptr)
    // }

    pub fn convert_type_refs_to_c(
        &mut self,
        type_refs: &[TypeRef],
        cache: Rc<RefCell<HashMap<usize, *mut hl_type>>>,
    ) -> Result<*mut *mut hl_type> {
        let mut c_type_refs = Vec::with_capacity(type_refs.len());
        for type_ref in type_refs {
            let c_type = self.convert_type_ref_to_c_cached(type_ref, Rc::clone(&cache))?;
            c_type_refs.push(c_type);
        }

        let ptr = c_type_refs.as_mut_ptr();
        std::mem::forget(c_type_refs);
        Ok(ptr)
    }

    fn create_c_string(&self, s: &str) -> Result<*const u16> {
        let c_str = std::ffi::CString::new(s)?;
        let ptr = c_str.into_raw() as *const u16;
        Ok(ptr)
    }

    pub (crate) fn convert_type_ref_to_c_cached(
        &mut self,
        type_ref: &TypeRef,
        cache: Rc<RefCell<HashMap<usize, *mut hl_type>>>,
    ) -> Result<*mut hl_type> {
        // Check if we've already started converting this type
        if let Some(&c_type) = cache.borrow().get(&type_ref.0) {
            return Ok(c_type);
        }

        // Create a placeholder and add it to the cache
        let placeholder = Box::into_raw(Box::new(hl_type {
            kind: hl_type_kind_HVOID, // Temporary placeholder
            __bindgen_anon_1: hl_type__bindgen_ty_1 {
                abs_name: std::ptr::null(),
            },
            vobj_proto: std::ptr::null_mut(),
            mark_bits: std::ptr::null_mut(),
        }));
        cache.borrow_mut().insert(type_ref.0, placeholder);

        // Now convert the type
        let rust_type = self.types_[type_ref.0].clone();
        let mut c_type = hl_type {
            kind: rust_type.kind,
            __bindgen_anon_1: hl_type__bindgen_ty_1 {
                abs_name: std::ptr::null(),
            },
            vobj_proto: std::ptr::null_mut(),
            mark_bits: std::ptr::null_mut(),
        };

        // Fill in the type-specific data
        match rust_type.kind {
            hl_type_kind_HABSTRACT => {
                if let Some(ref name) = rust_type.abs_name {
                    c_type.__bindgen_anon_1.abs_name = self.create_c_string(name)?;
                }
            }
            hl_type_kind_HOBJ | hl_type_kind_HSTRUCT => {
                if let Some(ref obj) = rust_type.obj {
                    let c_obj = Box::new(hl_type_obj {
                        nfields: obj.fields.len() as i32,
                        nproto: obj.proto.len() as i32,
                        nbindings: obj.bindings.len() as i32 / 2,
                        name: CString::new(obj.name.as_str())?.into_raw() as *const u16,
                        super_: if let Some(ref super_type) = obj.super_ {
                            self.convert_type_ref_to_c_cached(
                                &super_type.clone(),
                                Rc::clone(&cache)
                            )?
                        } else {
                            ptr::null_mut()
                        },
                        fields: self.convert_fields_to_c(&obj.fields, Rc::clone(&cache))?,
                        proto: self.convert_proto_to_c(&obj.proto)?,
                        bindings: self.convert_bindings_to_c(&obj.bindings),
                        global_value: if obj.global_value > 0 {
                            let gv_idx = (obj.global_value - 1) as usize;
                            if gv_idx < self.globals_data.len() {
                                unsafe { self.globals_data.as_ptr().add(gv_idx) as *mut *mut c_void }
                            } else {
                                ptr::null_mut()
                            }
                        } else {
                            ptr::null_mut()
                        },
                        m: ptr::null_mut(),
                        rt: ptr::null_mut(),
                    });
                    c_type.__bindgen_anon_1.obj = Box::into_raw(c_obj);
                }
            }
            hl_type_kind_HFUN | hl_type_kind_HMETHOD => {
                if let Some(ref fun) = rust_type.fun {
                    let c_fun = Box::new(hl_type_fun {
                        args: self.convert_type_refs_to_c(&fun.args, Rc::clone(&cache))?,
                        ret: self.convert_type_ref_to_c_cached(
                            &fun.ret.clone(),
                            Rc::clone(&cache)
                        )?,
                        nargs: fun.args.len() as i32,
                        parent: if let Some(parent) = &fun.parent{
                            self.convert_type_ref_to_c_cached(
                                &parent.clone(),
                                Rc::clone(&cache)
                            )?
                        }else {ptr::null_mut()},
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
                    c_type.__bindgen_anon_1.fun = Box::into_raw(c_fun);
                }
            }
            hl_type_kind_HENUM => {
                if let Some(ref tenum) = rust_type.tenum {
                    let c_enum = Box::new(hl_type_enum {
                        name: CString::new(tenum.name.as_str())?.into_raw() as *const u16,
                        global_value: if tenum.global_value > 0 {
                            let gv_idx = (tenum.global_value - 1) as usize;
                            if gv_idx < self.globals_data.len() {
                                unsafe { self.globals_data.as_ptr().add(gv_idx) as *mut *mut c_void }
                            } else {
                                ptr::null_mut()
                            }
                        } else {
                            ptr::null_mut()
                        },
                        nconstructs: tenum.constructs.len() as i32,
                        constructs: self.convert_constructs_to_c(&tenum.constructs, Rc::clone(&cache))?,
                    });
                    c_type.__bindgen_anon_1.tenum = Box::into_raw(c_enum);
                }
            }
            hl_type_kind_HVIRTUAL => {
                if let Some(ref virt) = rust_type.virt {
                    let c_virt = Box::new(hl_type_virtual {
                        fields: self.convert_fields_to_c(&virt.fields, Rc::clone(&cache))?,
                        nfields: virt.fields.len() as i32,
                        dataSize: virt.data_size as i32,
                        indexes: self.convert_indexes_to_c(&virt.indexes),
                        lookup: ptr::null_mut(), // You might need to handle this field
                    });
                    c_type.__bindgen_anon_1.virt = Box::into_raw(c_virt);
                }
            }
            hl_type_kind_HPACKED | hl_type_kind_HNULL | hl_type_kind_HREF => {
                if let Some(ref tparam) = rust_type.tparam {
                    c_type.__bindgen_anon_1.tparam = self.convert_to_c_type(
                        tparam.0.clone(),
                        &self.types_[tparam.0].clone(),
                        Rc::clone(&cache)
                    )?
                }
            }
            _ => {}
        }

        // Update the placeholder with the actual data
        unsafe {
            *placeholder = c_type;
        }

        self.c_ptr_to_type_index.insert(placeholder as usize, type_ref.0);

        Ok(placeholder)
    }

    fn convert_constructs_to_c(
        &mut self,
        constructs: &[HLEnumConstruct],
        cache: Rc<RefCell<HashMap<usize, *mut hl_type>>>,
    ) -> Result<*mut hl_enum_construct> {
        let mut c_constructs = Vec::with_capacity(constructs.len());
        for construct in constructs {
            let offsets = construct.offsets.clone().as_mut_ptr();
            let mut c_construct = hl_enum_construct {
                name: CString::new(construct.name.as_str())?.into_raw() as *const u16,
                nparams: construct.params.len() as i32,
                params: self.convert_type_refs_to_c(&construct.params, Rc::clone(&cache))?,
                size: construct.size,
                hasptr: construct.hasptr,
                offsets,
            };

            c_constructs.push(c_construct);
            mem::forget(offsets);
        }
        let ptr = c_constructs.as_mut_ptr();
        mem::forget(c_constructs);
        Ok(ptr)
    }

    // Don't forget to implement a corresponding free function to deallocate all this memory!
    pub unsafe fn free_hl_type_c(&self, hl_type: *mut hl_type) {
        if hl_type.is_null() {
            return;
        }

        let c_type = &mut *hl_type;

        match c_type.kind {
            hl_type_kind_HABSTRACT => {
                if !c_type.__bindgen_anon_1.abs_name.is_null() {
                    let _ = CString::from_raw(c_type.__bindgen_anon_1.abs_name as *mut i8);
                }
            }
            hl_type_kind_HOBJ | hl_type_kind_HSTRUCT => {
                if !c_type.__bindgen_anon_1.obj.is_null() {
                    let c_obj = Box::from_raw(c_type.__bindgen_anon_1.obj);
                    let _ = CString::from_raw(c_obj.name as *mut i8);
                    if !c_obj.super_.is_null() {
                        self.free_hl_type_c(c_obj.super_);
                    }
                    self.free_fields_c(c_obj.fields, c_obj.nfields);
                    self.free_proto_c(c_obj.proto, c_obj.nproto);
                    self.free_bindings_c(c_obj.bindings);
                }
            }
            hl_type_kind_HFUN | hl_type_kind_HMETHOD => {
                if !c_type.__bindgen_anon_1.fun.is_null() {
                    let c_fun = Box::from_raw(c_type.__bindgen_anon_1.fun);
                    self.free_type_refs_c(c_fun.args, c_fun.nargs);
                    self.free_hl_type_c(c_fun.ret);
                }
            }
            hl_type_kind_HENUM => {
                if !c_type.__bindgen_anon_1.tenum.is_null() {
                    let c_enum = Box::from_raw(c_type.__bindgen_anon_1.tenum);
                    let _ = CString::from_raw(c_enum.name as *mut i8);
                    self.free_constructs_c(c_enum.constructs, c_enum.nconstructs);
                }
            }
            hl_type_kind_HVIRTUAL => {
                if !c_type.__bindgen_anon_1.virt.is_null() {
                    let c_virt = Box::from_raw(c_type.__bindgen_anon_1.virt);
                    self.free_fields_c(c_virt.fields, c_virt.nfields);
                }
            }
            _ => {} // Do nothing for unsupported types
        }

        free(hl_type as *mut c_void);
    }

    unsafe fn free_fields_c(&self, fields: *mut hl_obj_field, nfields: i32) {
        if fields.is_null() {
            return;
        }
        for i in 0..nfields as isize {
            let field = &mut *fields.offset(i);
            let _ = CString::from_raw(field.name as *mut i8);
            self.free_hl_type_c(field.t);
        }
        free(fields as *mut c_void);
    }

    unsafe fn free_proto_c(&self, proto: *mut hl_obj_proto, nproto: i32) {
        if proto.is_null() {
            return;
        }
        for i in 0..nproto as isize {
            let p = &mut *proto.offset(i);
            let _ = CString::from_raw(p.name as *mut i8);
        }
        free(proto as *mut c_void);
    }

    unsafe fn free_bindings_c(&self, bindings: *mut i32) {
        if !bindings.is_null() {
            free(bindings as *mut c_void);
        }
    }

    unsafe fn free_type_refs_c(&self, type_refs: *mut *mut hl_type, nrefs: i32) {
        if type_refs.is_null() {
            return;
        }
        for i in 0..nrefs as isize {
            let type_ref = *type_refs.offset(i);
            self.free_hl_type_c(type_ref);
        }
        free(type_refs as *mut c_void);
    }

    unsafe fn free_constructs_c(&self, constructs: *mut hl_enum_construct, nconstructs: i32) {
        if constructs.is_null() {
            return;
        }
        for i in 0..nconstructs as isize {
            let construct = &mut *constructs.offset(i);
            let _ = CString::from_raw(construct.name as *mut i8);
            self.free_type_refs_c(construct.params, construct.nparams);
        }
        free(constructs as *mut c_void);
    }

    pub unsafe fn convert_from_c_type(&mut self, c_type: *mut hl_type) -> Result<TypeRef> {
        if c_type.is_null() {
            return Err(anyhow!("Null pointer passed to convert_from_c_type"));
        }

        let ptr = c_type as usize;

        let c_type = &*c_type;
        let kind = c_type.kind;

        // Find the corresponding Rust type in types_
        let type_index = self.c_ptr_to_type_index.clone().get(&ptr).unwrap().clone();

        match kind {
            hl_type_kind_HABSTRACT => {
                if !c_type.__bindgen_anon_1.abs_name.is_null() {
                    self.types_[type_index].abs_name = Some(
                        CStr::from_ptr(c_type.__bindgen_anon_1.abs_name as *const i8)
                            .to_str()?
                            .to_string(),
                    );
                }
            }
            hl_type_kind_HOBJ | hl_type_kind_HSTRUCT => {
                if !c_type.__bindgen_anon_1.obj.is_null() {
                    let obj = self.update_hl_type_obj(&*c_type.__bindgen_anon_1.obj)?;
                    self.types_[type_index].obj = Some(obj);
                }
            }
            hl_type_kind_HFUN | hl_type_kind_HMETHOD => {
                if !c_type.__bindgen_anon_1.fun.is_null() {
                    self.types_[type_index].fun =
                        Some(self.update_hl_type_fun(&*c_type.__bindgen_anon_1.fun)?);
                }
            }
            hl_type_kind_HENUM => {
                if !c_type.__bindgen_anon_1.tenum.is_null() {
                    self.types_[type_index].tenum =
                        Some(self.update_hl_type_enum(&*c_type.__bindgen_anon_1.tenum)?);
                }
            }
            hl_type_kind_HVIRTUAL => {
                if !c_type.__bindgen_anon_1.virt.is_null() {
                    self.types_[type_index].virt =
                        Some(self.update_hl_type_virtual(&*c_type.__bindgen_anon_1.virt)?);
                }
            }
            hl_type_kind_HPACKED | hl_type_kind_HNULL | hl_type_kind_HREF => {
                if !c_type.__bindgen_anon_1.tparam.is_null() {
                    self.types_[type_index].tparam =
                        Some(self.convert_from_c_type(c_type.__bindgen_anon_1.tparam)?);
                }
            }
            _ => {}
        }

        // // Store the mapping between C pointer and Rust type index
        // self.c_ptr_to_type_index.insert(ptr, type_index);

        Ok(TypeRef(type_index))
    }

    unsafe fn update_hl_type_obj(
        &mut self,
        // rust_obj: &mut Option<HLTypeObj>,
        c_obj: &hl_type_obj,
    ) -> Result<HLTypeObj> {
        let mut rust_obj = HLTypeObj::default(); //rust_obj.get_or_insert_with(Default::default);
        rust_obj.name = CStr::from_ptr(c_obj.name as *const i8)
            .to_str()?
            .to_string();
        rust_obj.super_ = if !c_obj.super_.is_null() {
            Some(self.convert_from_c_type(c_obj.super_)?)
        } else {
            None
        };
        rust_obj.fields = self.update_fields(c_obj.fields, c_obj.nfields)?;
        rust_obj.proto = self.update_proto(c_obj.proto, c_obj.nproto)?;
        rust_obj.bindings =
            slice::from_raw_parts(c_obj.bindings, c_obj.nbindings as usize * 2).to_vec();
        rust_obj.global_value = c_obj.global_value as u32;
        Ok(rust_obj.clone())
    }

    unsafe fn update_hl_type_fun(
        &mut self,
        // rust_fun: &mut Option<HLTypeFun>,
        c_fun: &hl_type_fun,
    ) -> Result<HLTypeFun> {
        let mut rust_fun = HLTypeFun::default(); //rust_fun.get_or_insert_with(Default::default);
        rust_fun.args = self.update_type_refs(c_fun.args, c_fun.nargs)?;
        rust_fun.ret = self.convert_from_c_type(c_fun.ret)?;
        Ok(rust_fun.clone())
    }

    unsafe fn update_hl_type_enum(
        &mut self,
        c_enum: &hl_type_enum,
    ) -> Result<HLTypeEnum> {
        let mut rust_enum = HLTypeEnum::default(); 
        rust_enum.name = CString::from_raw(c_enum.name as *mut c_char).to_string_lossy().to_string();
        rust_enum.constructs = self.update_constructs(c_enum.constructs, c_enum.nconstructs)?;
        rust_enum.global_value = c_enum.global_value as u32;
        Ok(rust_enum.clone())
    }

    unsafe fn update_hl_type_virtual(
        &mut self,
        c_virt: &hl_type_virtual,
    ) -> Result<HLTypeVirtual> {
        let mut rust_virt = HLTypeVirtual::default(); //rust_virt.get_or_insert_with(Default::default);
        rust_virt.fields = self.update_fields(c_virt.fields, c_virt.nfields)?;
        rust_virt.data_size = c_virt.dataSize as usize;
        rust_virt.indexes = slice::from_raw_parts(c_virt.indexes, c_virt.nfields as usize).to_vec();
        if !c_virt.lookup.is_null() {
            let lookup = c_virt.lookup.read();
            if !lookup.t.is_null() {
                rust_virt.lookup = HLFieldLookup {
                    t: self.convert_from_c_type(lookup.t)?,
                    hashed_name: lookup.hashed_name,
                    field_index: lookup.field_index as usize,
                };
            }
        }
        Ok(rust_virt.clone())
    }

    unsafe fn update_fields(
        &mut self,
        c_fields: *mut hl_obj_field,
        nfields: i32,
    ) -> Result<Vec<HLObjField>> {
        let mut fields = Vec::new();
        for i in 0..nfields {
            let c_field = &*c_fields.offset(i as isize);
            fields.push(HLObjField {
                name: CStr::from_ptr(c_field.name as *const i8)
                    .to_str()?
                    .to_string(),
                type_: self.convert_from_c_type(c_field.t)?,
                hashed_name: c_field.hashed_name,
            });
        }
        Ok(fields)
    }

    unsafe fn update_proto(
        &self,
        c_proto: *mut hl_obj_proto,
        nproto: i32,
    ) -> Result<Vec<HLObjProto>> {
        let mut proto = Vec::new();
        for i in 0..nproto {
            let c_p = &*c_proto.offset(i as isize);
            proto.push(HLObjProto {
                name: CStr::from_ptr(c_p.name as *const i8).to_str()?.to_string(),
                findex: c_p.findex,
                pindex: c_p.pindex,
                hashed_name: c_p.hashed_name,
            });
        }
        Ok(proto)
    }

    unsafe fn update_type_refs(
        &mut self,
        c_type_refs: *mut *mut hl_type,
        nrefs: i32,
    ) -> Result<Vec<TypeRef>> {
        let mut type_refs = Vec::new();
        for i in 0..nrefs {
            let c_type_ref = *c_type_refs.offset(i as isize);
            type_refs.push(self.convert_from_c_type(c_type_ref)?.clone());
        }
        Ok(type_refs)
    }

    unsafe fn update_constructs(
        &mut self,
        c_constructs: *mut hl_enum_construct,
        nconstructs: i32,
    ) -> Result<Vec<HLEnumConstruct>> {
        let mut constructs = Vec::new();
        for i in 0..nconstructs {
            let c_construct = &*c_constructs.offset(i as isize);
            constructs.push(HLEnumConstruct {
                name: CString::from_raw(c_construct.name as *mut c_char).to_str()?.to_string(),
                params: self.update_type_refs(c_construct.params, c_construct.nparams)?,
                size: c_construct.size,
                hasptr: c_construct.hasptr,
                offsets: if !c_construct.offsets.is_null() {
                    slice::from_raw_parts(c_construct.offsets, c_construct.nparams as usize)
                        .to_vec()
                } else {
                    vec![]
                },
            });
        }
        Ok(constructs)
    }
}
