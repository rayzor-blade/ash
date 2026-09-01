//! The module's data half, emitted as object data.
//!
//! A JIT can put the type table wherever it likes: it builds `hl_type`
//! structures with `Box::into_raw` and bakes their addresses into the IR as
//! integer constants, because the code and the data share one address space
//! and one process. An object file has neither. Every address the emitted
//! code needs must therefore become a *symbol* the linker resolves, and every
//! structure that address pointed at must become bytes in a section.
//!
//! This module does that by walking the type graph the compiler has already
//! built in memory and writing an LLVM global for each node it reaches. The
//! walk reads the real `hl_type`/`hl_type_obj`/... structures rather than
//! re-deriving them from the bytecode, so an AOT module cannot disagree with
//! the JIT about what the type table contains -- there is only one builder.
//!
//! Three arrays anchor the result:
//!
//! * `ash_globals` -- the global slots. Replaces the `globals_data` Vec that
//!   the JIT hands out `inttoptr` pointers into.
//! * `ash_functions` -- `functions_ptrs`, filled in with real function
//!   symbols once every body has been lowered.
//! * `ash_function_types` -- `functions_types`, the per-findex `HFUN`
//!   descriptors.
//!
//! Together they are what `hl_module_context` points at, so reflection,
//! bindings and closure allocation reach the same tables they do under the
//! JIT.

use anyhow::{anyhow, Result};
use inkwell::module::Linkage;
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, GlobalValue, PointerValue};
use inkwell::AddressSpace;
use std::cell::RefCell;
use std::ffi::c_void;
use std::collections::HashMap;
use std::rc::Rc;

use super::module::JITModule;
use crate::hl::*;
use crate::types::TypeRef;

impl<'ctx> JITModule<'ctx> {
    /// Emit the type table, the global slots and the function tables.
    ///
    /// Must run before any function body is lowered: lowering asks for type
    /// pointers and global slots, and in AOT mode those answers are the
    /// globals created here.
    pub(crate) fn emit_aot_data(&mut self) -> Result<()> {
        self.check_hl_layouts()?;

        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let nglobals = self.bytecode.globals.len().max(1);
        let nfun = self.functions_ptrs.len().max(1);

        let globals_ty = ptr_type.array_type(nglobals as u32);
        let globals = self.module.add_global(globals_ty, None, "ash_globals");
        globals.set_initializer(&globals_ty.const_zero());
        globals.set_linkage(Linkage::Internal);
        globals.set_alignment(8);
        self.aot_globals = Some(globals);

        let funs_ty = ptr_type.array_type(nfun as u32);
        let functions = self.module.add_global(funs_ty, None, "ash_functions");
        functions.set_initializer(&funs_ty.const_zero());
        functions.set_linkage(Linkage::Internal);
        functions.set_alignment(8);
        self.aot_functions = Some(functions);

        let ftypes = self
            .module
            .add_global(funs_ty, None, "ash_function_types");
        ftypes.set_initializer(&funs_ty.const_zero());
        ftypes.set_linkage(Linkage::Internal);
        ftypes.set_alignment(8);
        self.aot_function_types = Some(ftypes);

        // Global slot pointers stop being addresses in this process and
        // become offsets into an array the linker places.
        self.globals.clear();
        for index in 0..self.bytecode.globals.len() {
            let slot = self.aot_global_slot(index)?;
            self.globals.insert(index, slot);
        }

        // Convert every type up front. `get_initialized_type` fabricates a
        // bare descriptor for any index it cannot find in `c_ptr_to_type_index`,
        // and a descriptor fabricated during lowering would be an address in
        // this process that no symbol names. Converting all of them first
        // makes that branch unreachable.
        let cache: Rc<RefCell<HashMap<usize, *mut hl_type>>> =
            Rc::new(RefCell::new(HashMap::new()));
        for (&ptr, &index) in self.c_ptr_to_type_index.iter() {
            cache.borrow_mut().insert(index, ptr as *mut hl_type);
        }
        for index in 0..self.types_.len() {
            let converted =
                self.convert_type_ref_to_c_cached(&TypeRef(index), Rc::clone(&cache))?;
            self.emit_c_type(converted)?;
        }

        // `init_indexes` primed `initialized_type_cache` with `inttoptr`
        // constants for every HOBJ, HENUM and HVIRTUAL. That cache is
        // consulted before anything else, so leaving it in place would let
        // this process's addresses back into the emitted code through the
        // front door. Repoint it at the emitted data instead.
        self.initialized_type_cache.clear();
        let emitted: Vec<(usize, PointerValue<'ctx>)> = self
            .c_ptr_to_type_index
            .iter()
            .filter_map(|(&raw, &index)| {
                self.aot_types
                    .get(&raw)
                    .map(|global| (index, global.as_pointer_value()))
            })
            .collect();
        for (index, value) in emitted {
            self.initialized_type_cache.insert(index, value.into());
        }

        // Per-findex HFUN descriptors, built by `init_indexes` and referenced
        // by closure allocation.
        let func_types = self.func_types.clone();
        let mut entries: Vec<PointerValue<'ctx>> = Vec::with_capacity(nfun);
        for index in 0..nfun {
            let emitted = match func_types.get(index) {
                Some(&t) if !t.is_null() => self.emit_c_type(t)?,
                _ => ptr_type.const_null(),
            };
            entries.push(emitted);
        }
        ftypes.set_initializer(&ptr_type.const_array(&entries));

        Ok(())
    }

    /// Fill `ash_functions` with the addresses of the bodies just lowered.
    ///
    /// Runs after lowering, because that is when the `FunctionValue`s exist.
    /// A findex with no body -- refused, or never reached -- stays null, and
    /// an indirect call through it would fault rather than land somewhere
    /// arbitrary.
    pub fn finalize_aot_data(&mut self) -> Result<()> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let functions = self
            .aot_functions
            .ok_or_else(|| anyhow!("finalize_aot_data before emit_aot_data"))?;

        let count = self.functions_ptrs.len().max(1);
        let mut entries: Vec<PointerValue<'ctx>> = vec![ptr_type.const_null(); count];
        for (findex, slot) in entries.iter_mut().enumerate() {
            if let Some(compiled) = self.func_cache.get(&findex) {
                *slot = compiled.as_global_value().as_pointer_value();
            }
        }
        functions.set_initializer(&ptr_type.const_array(&entries));

        // A bytecode function carries its Haxe name, and in an object file
        // that name is a symbol competing with libc for it. A Haxe method
        // called `write` linked ahead of `libSystem`'s, and the runtime's own
        // `println!` jumped into Haxe code and faulted -- before printing a
        // single line. Nothing outside this object is meant to call a
        // bytecode body: `main` and `ash_module_init` are the only entries,
        // and `ash_functions` above is the only table. Internalizing here
        // rather than at creation is deliberate -- a not-yet-lowered callee
        // is declared through the same path, and an internal declaration is
        // invalid IR.
        for function in self.module.get_functions() {
            if function.count_basic_blocks() == 0 {
                continue;
            }
            let name = function.get_name().to_string_lossy();
            if name == "main" || name == "ash_module_init" {
                continue;
            }
            function.as_global_value().set_linkage(Linkage::Internal);
        }
        Ok(())
    }

    /// The address of global slot `index`, as a constant GEP into
    /// `ash_globals` rather than as this process's address.
    pub(crate) fn aot_global_slot(&self, index: usize) -> Result<PointerValue<'ctx>> {
        let globals = self
            .aot_globals
            .ok_or_else(|| anyhow!("global slot {index} requested before emit_aot_data"))?;
        let i64_type = self.context.i64_type();
        let array = globals.get_value_type().into_array_type();
        Ok(unsafe {
            globals.as_pointer_value().const_gep(
                array,
                &[i64_type.const_zero(), i64_type.const_int(index as u64, false)],
            )
        })
    }

    /// The address of `functions_ptrs[findex]`, as a constant GEP into
    /// `ash_functions`.
    pub(crate) fn aot_function_slot(&self, findex: usize) -> Result<PointerValue<'ctx>> {
        let functions = self
            .aot_functions
            .ok_or_else(|| anyhow!("function slot {findex} requested before emit_aot_data"))?;
        let i64_type = self.context.i64_type();
        let array = functions.get_value_type().into_array_type();
        Ok(unsafe {
            functions.as_pointer_value().const_gep(
                array,
                &[i64_type.const_zero(), i64_type.const_int(findex as u64, false)],
            )
        })
    }

    /// The emitted global standing in for a C type pointer.
    ///
    /// An error rather than a fallback: a missing type in AOT means the
    /// lowering wants a descriptor that no symbol names, and baking the
    /// compiler's address would produce an object that links, runs, and reads
    /// whatever happens to live at that address in the new process.
    pub(crate) fn aot_type_ptr(&self, ptr: *mut hl_type) -> Result<PointerValue<'ctx>> {
        if ptr.is_null() {
            return Ok(self.context.ptr_type(AddressSpace::default()).const_null());
        }
        self.aot_types
            .get(&(ptr as usize))
            .map(|global| global.as_pointer_value())
            .ok_or_else(|| anyhow!("type descriptor {ptr:p} was never emitted as object data"))
    }

    fn emit_c_type(&mut self, ptr: *mut hl_type) -> Result<PointerValue<'ctx>> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        if ptr.is_null() {
            return Ok(ptr_type.const_null());
        }
        if let Some(global) = self.aot_types.get(&(ptr as usize)) {
            return Ok(global.as_pointer_value());
        }

        // Created before the arm is walked, so a cycle through `super_` or a
        // field type finds the global instead of recursing forever.
        let struct_type = self.get_hl_type_struct_type()?;
        let name = format!("ash_type_{}", self.aot_types.len());
        let global = self.module.add_global(struct_type, None, &name);
        global.set_linkage(Linkage::Internal);
        global.set_alignment(8);
        self.aot_types.insert(ptr as usize, global);

        let kind = unsafe { (*ptr).kind };
        let arm: BasicValueEnum<'ctx> = unsafe {
            match kind {
                hl_type_kind_HOBJ | hl_type_kind_HSTRUCT => {
                    self.emit_type_obj((*ptr).__bindgen_anon_1.obj)?.into()
                }
                hl_type_kind_HFUN | hl_type_kind_HMETHOD => {
                    self.emit_type_fun((*ptr).__bindgen_anon_1.fun)?.into()
                }
                hl_type_kind_HENUM => self.emit_type_enum((*ptr).__bindgen_anon_1.tenum)?.into(),
                hl_type_kind_HVIRTUAL => {
                    self.emit_type_virtual((*ptr).__bindgen_anon_1.virt)?.into()
                }
                hl_type_kind_HABSTRACT => {
                    self.emit_utf16((*ptr).__bindgen_anon_1.abs_name)?.into()
                }
                hl_type_kind_HNULL | hl_type_kind_HREF | hl_type_kind_HPACKED => {
                    self.emit_c_type((*ptr).__bindgen_anon_1.tparam)?.into()
                }
                _ => ptr_type.const_null().into(),
            }
        };

        global.set_initializer(&struct_type.const_named_struct(&[
            self.context.i32_type().const_int(kind as u64, false).into(),
            arm,
            // The runtime owns both. `vobj_proto` is filled by the first
            // `hlp_get_obj_rt`. `mark_bits` is NOT the collector's -- that is
            // the unrelated per-block line array in gc.rs; this one is written
            // by `hlp_get_obj_rt` for HOBJ, `hlp_init_virtual` for HVIRTUAL,
            // and the enum path in types.rs, and read only to copy a parent's
            // down to a child. Left null here, it is filled by whichever of
            // those runs first.
            ptr_type.const_null().into(),
            ptr_type.const_null().into(),
        ]));
        Ok(global.as_pointer_value())
    }

    fn emit_type_obj(&mut self, obj: *mut hl_type_obj) -> Result<PointerValue<'ctx>> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        if obj.is_null() {
            return Ok(ptr_type.const_null());
        }
        let o = unsafe { *obj };

        let name = self.emit_utf16(o.name)?;
        let super_ = self.emit_c_type(o.super_)?;
        let fields = self.emit_obj_fields(o.fields, o.nfields as usize)?;
        let proto = self.emit_obj_protos(o.proto, o.nproto as usize)?;
        let bindings = self.emit_i32_array(o.bindings, (o.nbindings * 2) as usize, "bindings")?;
        let global_value = self.emit_global_slot_ref(o.global_value as *mut u8)?;
        let module_ctx = if o.m.is_null() {
            ptr_type.const_null()
        } else {
            self.emit_module_context()?
        };

        let i32_type = self.context.i32_type();
        let value = self.context.const_struct(
            &[
                i32_type.const_int(o.nfields as u64, false).into(),
                i32_type.const_int(o.nproto as u64, false).into(),
                i32_type.const_int(o.nbindings as u64, false).into(),
                name.into(),
                super_.into(),
                fields.into(),
                proto.into(),
                bindings.into(),
                global_value.into(),
                module_ctx.into(),
                // `rt` is a runtime cache, filled by `hlp_get_obj_rt`.
                ptr_type.const_null().into(),
            ],
            false,
        );
        Ok(self.intern_global(value.as_basic_value_enum(), "ash_obj"))
    }

    fn emit_type_fun(&mut self, fun: *mut hl_type_fun) -> Result<PointerValue<'ctx>> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        if fun.is_null() {
            return Ok(ptr_type.const_null());
        }
        let f = unsafe { *fun };
        let i32_type = self.context.i32_type();

        let mut args = Vec::with_capacity(f.nargs.max(0) as usize);
        for i in 0..f.nargs.max(0) as usize {
            args.push(self.emit_c_type(unsafe { *f.args.add(i) })?);
        }
        let args_ptr = if args.is_empty() {
            ptr_type.const_null()
        } else {
            self.intern_global(
                ptr_type.const_array(&args).as_basic_value_enum(),
                "ash_fun_args",
            )
        };
        let ret = self.emit_c_type(f.ret)?;
        let parent = self.emit_c_type(f.parent)?;

        // `closure_type` and `closure` are scratch the runtime fills when it
        // strips a bound argument; emitted zeroed, as the compiler builds them.
        let closure_type = self.context.const_struct(
            &[i32_type.const_zero().into(), ptr_type.const_null().into()],
            false,
        );
        let closure = self.context.const_struct(
            &[
                ptr_type.const_null().into(),
                ptr_type.const_null().into(),
                i32_type.const_zero().into(),
                ptr_type.const_null().into(),
            ],
            false,
        );

        let value = self.context.const_struct(
            &[
                args_ptr.into(),
                ret.into(),
                i32_type.const_int(f.nargs as u64, false).into(),
                parent.into(),
                closure_type.into(),
                closure.into(),
            ],
            false,
        );
        Ok(self.intern_global(value.as_basic_value_enum(), "ash_fun"))
    }

    fn emit_type_enum(&mut self, tenum: *mut hl_type_enum) -> Result<PointerValue<'ctx>> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        if tenum.is_null() {
            return Ok(ptr_type.const_null());
        }
        let e = unsafe { *tenum };
        let i32_type = self.context.i32_type();
        let i8_type = self.context.i8_type();

        let name = self.emit_utf16(e.name)?;
        let global_value = self.emit_global_slot_ref(e.global_value as *mut u8)?;

        let mut constructs = Vec::with_capacity(e.nconstructs.max(0) as usize);
        for i in 0..e.nconstructs.max(0) as usize {
            let c = unsafe { *e.constructs.add(i) };
            let cname = self.emit_utf16(c.name)?;
            let mut params = Vec::with_capacity(c.nparams.max(0) as usize);
            for j in 0..c.nparams.max(0) as usize {
                params.push(self.emit_c_type(unsafe { *c.params.add(j) })?);
            }
            let params_ptr = if params.is_empty() {
                ptr_type.const_null()
            } else {
                self.intern_global(
                    ptr_type.const_array(&params).as_basic_value_enum(),
                    "ash_enum_params",
                )
            };
            let offsets =
                self.emit_i32_array(c.offsets, c.nparams.max(0) as usize, "ash_enum_offsets")?;
            constructs.push(self.context.const_struct(
                &[
                    cname.into(),
                    i32_type.const_int(c.nparams as u64, false).into(),
                    params_ptr.into(),
                    i32_type.const_int(c.size as u64, false).into(),
                    i8_type.const_int(c.hasptr as u64, false).into(),
                    offsets.into(),
                ],
                false,
            ));
        }
        let constructs_ptr = if constructs.is_empty() {
            ptr_type.const_null()
        } else {
            let array = constructs[0].get_type().const_array(&constructs);
            self.intern_global(array.as_basic_value_enum(), "ash_enum_constructs")
        };

        let value = self.context.const_struct(
            &[
                name.into(),
                i32_type.const_int(e.nconstructs as u64, false).into(),
                constructs_ptr.into(),
                global_value.into(),
            ],
            false,
        );
        Ok(self.intern_global(value.as_basic_value_enum(), "ash_enum"))
    }

    fn emit_type_virtual(&mut self, virt: *mut hl_type_virtual) -> Result<PointerValue<'ctx>> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        if virt.is_null() {
            return Ok(ptr_type.const_null());
        }
        let v = unsafe { *virt };
        let i32_type = self.context.i32_type();

        let fields = self.emit_obj_fields(v.fields, v.nfields as usize)?;
        // One index per field, absolute byte offsets from the vvirtual header.
        let indexes = self.emit_i32_array(v.indexes, v.nfields.max(0) as usize, "ash_virt_idx")?;

        let value = self.context.const_struct(
            &[
                fields.into(),
                i32_type.const_int(v.nfields as u64, false).into(),
                i32_type.const_int(v.dataSize as u64, false).into(),
                indexes.into(),
                // `lookup` is built by the first virtual field access.
                ptr_type.const_null().into(),
            ],
            false,
        );
        Ok(self.intern_global(value.as_basic_value_enum(), "ash_virt"))
    }

    fn emit_obj_fields(
        &mut self,
        fields: *mut hl_obj_field,
        count: usize,
    ) -> Result<PointerValue<'ctx>> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        if fields.is_null() || count == 0 {
            return Ok(ptr_type.const_null());
        }
        let i32_type = self.context.i32_type();
        let mut values = Vec::with_capacity(count);
        for i in 0..count {
            let f = unsafe { *fields.add(i) };
            let name = self.emit_utf16(f.name)?;
            let t = self.emit_c_type(f.t)?;
            values.push(self.context.const_struct(
                &[
                    name.into(),
                    t.into(),
                    i32_type.const_int(f.hashed_name as u64, false).into(),
                ],
                false,
            ));
        }
        let array = values[0].get_type().const_array(&values);
        Ok(self.intern_global(array.as_basic_value_enum(), "ash_fields"))
    }

    fn emit_obj_protos(
        &mut self,
        proto: *mut hl_obj_proto,
        count: usize,
    ) -> Result<PointerValue<'ctx>> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        if proto.is_null() || count == 0 {
            return Ok(ptr_type.const_null());
        }
        let i32_type = self.context.i32_type();
        let mut values = Vec::with_capacity(count);
        for i in 0..count {
            let p = unsafe { *proto.add(i) };
            let name = self.emit_utf16(p.name)?;
            values.push(self.context.const_struct(
                &[
                    name.into(),
                    i32_type.const_int(p.findex as u64, false).into(),
                    i32_type.const_int(p.pindex as u64, true).into(),
                    i32_type.const_int(p.hashed_name as u64, false).into(),
                ],
                false,
            ));
        }
        let array = values[0].get_type().const_array(&values);
        Ok(self.intern_global(array.as_basic_value_enum(), "ash_protos"))
    }

    fn emit_i32_array(
        &mut self,
        data: *mut i32,
        count: usize,
        name: &str,
    ) -> Result<PointerValue<'ctx>> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        if data.is_null() || count == 0 {
            return Ok(ptr_type.const_null());
        }
        let i32_type = self.context.i32_type();
        let values: Vec<_> = (0..count)
            .map(|i| i32_type.const_int(unsafe { *data.add(i) } as u32 as u64, false))
            .collect();
        Ok(self.intern_global(i32_type.const_array(&values).as_basic_value_enum(), name))
    }

    /// A null-terminated UTF-16 name, emitted as the bytes it already is.
    fn emit_utf16(&mut self, text: *const u16) -> Result<PointerValue<'ctx>> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        if text.is_null() {
            return Ok(ptr_type.const_null());
        }
        if let Some(global) = self.aot_strings.get(&(text as usize)) {
            return Ok(global.as_pointer_value());
        }
        let mut units: Vec<u16> = Vec::new();
        let mut cursor = text;
        loop {
            let unit = unsafe { *cursor };
            units.push(unit);
            if unit == 0 {
                break;
            }
            cursor = unsafe { cursor.add(1) };
        }
        let bytes: Vec<u8> = units.iter().flat_map(|u| u.to_le_bytes()).collect();
        let value = self.context.const_string(&bytes, false);
        let global = self.module.add_global(
            self.context.i8_type().array_type(bytes.len() as u32),
            None,
            "ash_name",
        );
        global.set_initializer(&value);
        global.set_linkage(Linkage::Internal);
        global.set_constant(true);
        global.set_alignment(2);
        self.aot_strings.insert(text as usize, global);
        Ok(global.as_pointer_value())
    }

    /// A `global_value` pointer, which addresses one slot of the compiler's
    /// `globals_data`, re-expressed as a GEP into `ash_globals`.
    fn emit_global_slot_ref(&mut self, slot: *mut u8) -> Result<PointerValue<'ctx>> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        if slot.is_null() {
            return Ok(ptr_type.const_null());
        }
        let base = self.globals_data.as_ptr() as usize;
        let end = base + self.globals_data.len() * std::mem::size_of::<*mut c_void>();
        let address = slot as usize;
        if address < base || address >= end {
            // `convert_type_ref_to_c_cached` allocates a standalone slot for a
            // `global_value` index past the table. Nothing reads it, so an
            // emitted null is as good as an emitted orphan.
            return Ok(ptr_type.const_null());
        }
        let index = (address - base) / std::mem::size_of::<*mut c_void>();
        self.aot_global_slot(index)
    }

    fn emit_module_context(&mut self) -> Result<PointerValue<'ctx>> {
        if let Some(global) = self.aot_module_ctx {
            return Ok(global.as_pointer_value());
        }
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let functions = self
            .aot_functions
            .ok_or_else(|| anyhow!("module context before emit_aot_data"))?;
        let ftypes = self
            .aot_function_types
            .ok_or_else(|| anyhow!("module context before emit_aot_data"))?;

        let value = self.context.const_struct(
            &[
                // hl_alloc::cur
                ptr_type.const_null().into(),
                functions.as_pointer_value().into(),
                ftypes.as_pointer_value().into(),
            ],
            false,
        );
        let global = self
            .module
            .add_global(value.get_type(), None, "ash_module_ctx");
        global.set_initializer(&value);
        global.set_linkage(Linkage::Internal);
        global.set_alignment(8);
        self.aot_module_ctx = Some(global);
        Ok(global.as_pointer_value())
    }

    fn intern_global(&mut self, value: BasicValueEnum<'ctx>, name: &str) -> PointerValue<'ctx> {
        let global = self.module.add_global(value.get_type(), None, name);
        global.set_initializer(&value);
        global.set_linkage(Linkage::Internal);
        global.set_alignment(8);
        global.as_pointer_value()
    }

    /// Compare what LLVM will lay out against what the linked runtime will
    /// read.
    ///
    /// The emitted structures are read by `ash_std`, compiled from the same
    /// `hl.h` bindings this compiler uses. If the two ever disagree about a
    /// size, every field past the divergence is silently wrong -- an object
    /// that links and runs and reads the wrong words. Checking once, loudly,
    /// costs nothing.
    fn check_hl_layouts(&mut self) -> Result<()> {
        use inkwell::types::BasicType;
        let hl_type_struct = self.get_hl_type_struct_type()?;
        let target = self.execution_engine.get_target_data();
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let i32_type = self.context.i32_type();
        let i8_type = self.context.i8_type();
        let checks: [(&str, inkwell::types::StructType<'ctx>, usize); 8] = [
            ("hl_type", hl_type_struct, std::mem::size_of::<hl_type>()),
            (
                "hl_type_obj",
                self.context.struct_type(
                    &[
                        i32_type.into(),
                        i32_type.into(),
                        i32_type.into(),
                        ptr_type.into(),
                        ptr_type.into(),
                        ptr_type.into(),
                        ptr_type.into(),
                        ptr_type.into(),
                        ptr_type.into(),
                        ptr_type.into(),
                        ptr_type.into(),
                    ],
                    false,
                ),
                std::mem::size_of::<hl_type_obj>(),
            ),
            (
                "hl_obj_field",
                self.context
                    .struct_type(&[ptr_type.into(), ptr_type.into(), i32_type.into()], false),
                std::mem::size_of::<hl_obj_field>(),
            ),
            (
                "hl_obj_proto",
                self.context.struct_type(
                    &[
                        ptr_type.into(),
                        i32_type.into(),
                        i32_type.into(),
                        i32_type.into(),
                    ],
                    false,
                ),
                std::mem::size_of::<hl_obj_proto>(),
            ),
            (
                "hl_type_fun",
                self.context.struct_type(
                    &[
                        ptr_type.into(),
                        ptr_type.into(),
                        i32_type.into(),
                        ptr_type.into(),
                        self.context
                            .struct_type(&[i32_type.into(), ptr_type.into()], false)
                            .into(),
                        self.context
                            .struct_type(
                                &[
                                    ptr_type.into(),
                                    ptr_type.into(),
                                    i32_type.into(),
                                    ptr_type.into(),
                                ],
                                false,
                            )
                            .into(),
                    ],
                    false,
                ),
                std::mem::size_of::<hl_type_fun>(),
            ),
            (
                "hl_type_enum",
                self.context.struct_type(
                    &[
                        ptr_type.into(),
                        i32_type.into(),
                        ptr_type.into(),
                        ptr_type.into(),
                    ],
                    false,
                ),
                std::mem::size_of::<hl_type_enum>(),
            ),
            (
                "hl_enum_construct",
                self.context.struct_type(
                    &[
                        ptr_type.into(),
                        i32_type.into(),
                        ptr_type.into(),
                        i32_type.into(),
                        i8_type.into(),
                        ptr_type.into(),
                    ],
                    false,
                ),
                std::mem::size_of::<hl_enum_construct>(),
            ),
            (
                "hl_module_context",
                self.context
                    .struct_type(&[ptr_type.into(), ptr_type.into(), ptr_type.into()], false),
                std::mem::size_of::<hl_module_context>(),
            ),
        ];

        for (name, llvm, rust) in checks {
            let emitted = target.get_store_size(&llvm.as_basic_type_enum()) as usize;
            if emitted != rust {
                return Err(anyhow!(
                    "{name} would be emitted as {emitted} bytes but the runtime reads {rust}"
                ));
            }
        }
        Ok(())
    }
}

impl<'ctx> JITModule<'ctx> {
    /// Emit `ash_module_init`: the startup work a JIT does in its own process.
    ///
    /// `init_constants` allocates the constant pool -- the String literals and
    /// class descriptors the bytecode expects to already exist -- and fills
    /// their fields. A JIT can do that at compile time because compile time
    /// and run time are the same process. AOT cannot, so the identical
    /// sequence is emitted as code instead: allocate, root, look up the field
    /// offsets, store.
    ///
    /// The field offsets are read from the runtime object at startup rather
    /// than baked in. `hlp_get_obj_rt` computes them from the type's layout,
    /// and computing them twice -- once here, once in the runtime -- is how
    /// the two silently disagree.
    /// Build the module's constant objects as initialised DATA.
    ///
    /// The startup routine used to allocate every constant with
    /// `hlp_alloc_obj`, ask `hlp_get_obj_rt` for its field offsets, and store
    /// each field one call at a time. None of that depends on anything only
    /// known at run time: the offsets come from [`crate::layout`], which is
    /// already what every compiled field access is lowered against, and the
    /// values are string data, type descriptors and integers this module also
    /// emits. So the objects are emitted as one initialised blob and the
    /// loader's relocations do the work the routine was doing.
    ///
    /// Returns the global slots it handled; the caller leaves those out of the
    /// startup routine. A constant is handled only when its layout can be
    /// reproduced exactly and every field is one of the statically knowable
    /// kinds -- a field holding a closure or a reference to another constant
    /// still goes the old way, so this narrows the routine rather than
    /// replacing it.
    fn emit_static_constants(
        &mut self,
        type_globals: &HashMap<usize, PointerValue<'ctx>>,
    ) -> Result<std::collections::HashSet<usize>> {
        use inkwell::types::BasicTypeEnum;

        let mut handled = std::collections::HashSet::new();
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let i8_type = self.context.i8_type();
        let i32_type = self.context.i32_type();
        let bytecode = self.bytecode.clone();

        // A field this pass can place without asking the runtime anything.
        // Deliberately excludes HFUN/HMETHOD (needs a vclosure allocated) and
        // HOBJ/HSTRUCT (needs another constant's address, which would make
        // eligibility mutually recursive).
        let placeable = |k: hl_type_kind| {
            matches!(k, hl_type_kind_HBYTES | hl_type_kind_HTYPE)
                || !matches!(
                    k,
                    hl_type_kind_HFUN
                        | hl_type_kind_HMETHOD
                        | hl_type_kind_HOBJ
                        | hl_type_kind_HSTRUCT
                )
        };

        struct Candidate {
            global_idx: usize,
            type_idx: usize,
            layout: crate::layout::ObjLayout,
            start: usize,
            fields: Vec<i32>,
        }

        let mut candidates: Vec<Candidate> = Vec::new();
        for constant in &bytecode.constants {
            let global_idx = constant.global as usize;
            if global_idx >= bytecode.globals.len() || global_idx >= self.globals_data.len() {
                continue;
            }
            let type_idx = bytecode.globals[global_idx].0;
            let ty = &bytecode.types[type_idx];
            if ty.kind != hl_type_kind_HOBJ && ty.kind != hl_type_kind_HSTRUCT {
                continue;
            }
            if !type_globals.contains_key(&type_idx) {
                continue;
            }
            let Some(obj_data) = ty.obj.as_ref() else { continue };
            let Some(layout) = crate::layout::object_layout(&bytecode.types, type_idx) else {
                continue; // layout says "ask the runtime", so we must
            };
            if constant.fields.len() > obj_data.fields.len()
                || layout.field_offsets.len() < obj_data.fields.len()
            {
                continue;
            }
            let start = layout.field_offsets.len() - obj_data.fields.len();
            let ok = constant.fields.iter().enumerate().all(|(j, _)| {
                obj_data
                    .fields
                    .get(j)
                    .and_then(|f| bytecode.types.get(f.type_.0))
                    .is_some_and(|ft| placeable(ft.kind))
            });
            if !ok {
                continue;
            }
            candidates.push(Candidate {
                global_idx,
                type_idx,
                layout,
                start,
                fields: constant.fields.clone(),
            });
        }
        if candidates.is_empty() {
            return Ok(handled);
        }

        // Pass 1: the shape of each object, so the blob has a type before any
        // initialiser needs to name an address inside it.
        let mut member_types: Vec<BasicTypeEnum<'ctx>> = Vec::new();
        for c in &candidates {
            let mut parts: Vec<BasicTypeEnum<'ctx>> = Vec::new();
            let mut cursor: i32 = 0;
            let is_obj = bytecode.types[c.type_idx].kind == hl_type_kind_HOBJ;
            if is_obj {
                parts.push(ptr_type.into()); // the `t` header hlp_alloc_obj writes
                cursor = 8;
            }
            let obj_data = bytecode.types[c.type_idx].obj.as_ref().unwrap();
            for j in 0..c.fields.len() {
                let off = c.layout.field_offsets[c.start + j];
                if off < cursor {
                    parts.clear();
                    break;
                }
                if off > cursor {
                    parts.push(i8_type.array_type((off - cursor) as u32).into());
                }
                let fk = bytecode.types[obj_data.fields[j].type_.0].kind;
                let width = match fk {
                    hl_type_kind_HBYTES | hl_type_kind_HTYPE => {
                        parts.push(ptr_type.into());
                        8
                    }
                    _ => {
                        // Mirrors the routine this replaces: the value is
                        // stored as an i32 whatever the field's declared
                        // width, and any remainder stays zero.
                        parts.push(i32_type.into());
                        4
                    }
                };
                cursor = off + width;
            }
            if parts.is_empty() && !c.fields.is_empty() {
                member_types.push(i8_type.array_type(c.layout.size.max(0) as u32).into());
                continue;
            }
            if c.layout.size > cursor {
                parts.push(i8_type.array_type((c.layout.size - cursor) as u32).into());
            }
            member_types.push(self.context.struct_type(&parts, true).into());
        }

        let blob_ty = self.context.struct_type(&member_types, true);
        let blob = self.module.add_global(blob_ty, None, "ash_constants");
        blob.set_linkage(Linkage::Internal);
        blob.set_alignment(8);

        // Pass 2: the values.
        let mut member_values: Vec<BasicValueEnum<'ctx>> = Vec::new();
        for (ci, c) in candidates.iter().enumerate() {
            let BasicTypeEnum::StructType(member_ty) = member_types[ci] else {
                member_values.push(member_types[ci].const_zero());
                continue;
            };
            let mut vals: Vec<BasicValueEnum<'ctx>> = Vec::new();
            let mut cursor: i32 = 0;
            let is_obj = bytecode.types[c.type_idx].kind == hl_type_kind_HOBJ;
            if is_obj {
                vals.push(type_globals[&c.type_idx].into());
                cursor = 8;
            }
            let obj_data = bytecode.types[c.type_idx].obj.as_ref().unwrap();
            for (j, &field_value) in c.fields.iter().enumerate() {
                let off = c.layout.field_offsets[c.start + j];
                if off > cursor {
                    vals.push(i8_type.array_type((off - cursor) as u32).const_zero().into());
                }
                let fk = bytecode.types[obj_data.fields[j].type_.0].kind;
                let width = match fk {
                    hl_type_kind_HBYTES => {
                        let v = self
                            .string_globals
                            .get(field_value as usize)
                            .and_then(|g| g.as_ref())
                            .map(|g| g.as_pointer_value())
                            .unwrap_or_else(|| ptr_type.const_null());
                        vals.push(v.into());
                        8
                    }
                    hl_type_kind_HTYPE => {
                        let v = type_globals
                            .get(&(field_value as usize))
                            .copied()
                            .unwrap_or_else(|| ptr_type.const_null());
                        vals.push(v.into());
                        8
                    }
                    _ => {
                        let value = bytecode
                            .ints
                            .get(field_value as usize)
                            .copied()
                            .unwrap_or(field_value);
                        vals.push(i32_type.const_int(value as u32 as u64, false).into());
                        4
                    }
                };
                cursor = off + width;
            }
            if c.layout.size > cursor {
                vals.push(i8_type.array_type((c.layout.size - cursor) as u32).const_zero().into());
            }
            member_values.push(member_ty.const_named_struct(&vals).into());
        }
        blob.set_initializer(&blob_ty.const_named_struct(&member_values));

        // Point the global slots at the blob, statically. These were stores
        // executed at startup; as constant GEPs the loader resolves them.
        if let Some(globals) = self.aot_globals {
            let nglobals = self.bytecode.globals.len().max(1);
            let mut slots: Vec<PointerValue<'ctx>> = vec![ptr_type.const_null(); nglobals];
            for (ci, c) in candidates.iter().enumerate() {
                let addr = unsafe {
                    blob.as_pointer_value().const_in_bounds_gep(
                        blob_ty,
                        &[
                            self.context.i32_type().const_zero(),
                            self.context.i32_type().const_int(ci as u64, false),
                        ],
                    )
                };
                slots[c.global_idx] = addr;
                handled.insert(c.global_idx);
            }
            globals.set_initializer(&ptr_type.const_array(&slots));
        }

        Ok(handled)
    }

    pub(crate) fn emit_module_init(&mut self) -> Result<()> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let i32_type = self.context.i32_type();
        let i64_type = self.context.i64_type();
        let void_type = self.context.void_type();
        let i8_type = self.context.i8_type();

        let init = self
            .module
            .add_function("ash_module_init", void_type.fn_type(&[], false), None);
        let entry = self.context.append_basic_block(init, "entry");
        self.builder.position_at_end(entry);

        let alloc_obj_ty = ptr_type.fn_type(&[ptr_type.into()], false);
        let register_root_ty = void_type.fn_type(&[ptr_type.into()], false);
        let get_obj_rt_ty = ptr_type.fn_type(&[ptr_type.into()], false);
        let alloc_closure_ty = ptr_type.fn_type(&[ptr_type.into(), ptr_type.into()], false);
        let set_globals_ty = void_type.fn_type(&[ptr_type.into(), i64_type.into()], false);
        let setup_callbacks_ty =
            void_type.fn_type(&[ptr_type.into(), ptr_type.into(), i32_type.into()], false);
        let set_runner_ty = void_type.fn_type(&[ptr_type.into()], false);

        let alloc_obj = self.aot_symbol("hlp_alloc_obj", alloc_obj_ty);
        let register_root = self.aot_symbol("hlp_gc_register_root", register_root_ty);
        let get_obj_rt = self.aot_symbol("hlp_get_obj_rt", get_obj_rt_ty);
        let alloc_closure = self.aot_symbol("hlp_alloc_closure_void", alloc_closure_ty);

        // The global slots hold GC pointers and live in a data section, which
        // neither the conservative stack scan nor the TLAB walk reaches. The
        // collector has to be told about them explicitly, and before the
        // first allocation below, or a String literal held only by a global
        // is collected out from under the program. Same call the JIT makes,
        // in the same place.
        if let Some(globals) = self.aot_globals {
            let set_globals = self.aot_symbol("hlp_gc_set_globals", set_globals_ty);
            self.builder.build_indirect_call(
                set_globals_ty,
                set_globals,
                &[
                    globals.as_pointer_value().into(),
                    i64_type.const_int(self.bytecode.globals.len() as u64, false).into(),
                ],
                "",
            )?;
        }

        // Dynamic dispatch (Type.createInstance, Reflect.callMethod) reaches
        // compiled code through these two, and nothing else installs them in
        // a standalone binary.
        let setup_callbacks = self.aot_symbol("hl_setup_callbacks2", setup_callbacks_ty);
        let static_call = self.aot_symbol("ash_static_call", void_type.fn_type(&[], false));
        self.builder.build_indirect_call(
            setup_callbacks_ty,
            setup_callbacks,
            &[
                static_call.into(),
                ptr_type.const_null().into(),
                i32_type.const_zero().into(),
            ],
            "",
        )?;
        let set_runner = self.aot_symbol("hlp_set_closure_runner", set_runner_ty);
        let runner = self.aot_symbol("hlp_jit_closure_runner", void_type.fn_type(&[], false));
        self.builder
            .build_indirect_call(set_runner_ty, set_runner, &[runner.into()], "")?;

        // type index -> the global that names its descriptor
        let mut type_globals: HashMap<usize, PointerValue<'ctx>> = HashMap::new();
        for (&raw, &index) in self.c_ptr_to_type_index.iter() {
            if let Some(global) = self.aot_types.get(&raw) {
                type_globals.insert(index, global.as_pointer_value());
            }
        }

        // Everything this can place as data is placed as data; what comes
        // back is what still needs the routine below.
        let statically_built = self.emit_static_constants(&type_globals)?;
        if let Some(blob) = self.module.get_global("ash_constants") {
            // The blob lives in a data section, which neither the conservative
            // stack scan nor the TLAB walk reaches. Constants are immortal --
            // outside the arena, so never marked and never swept -- but a field
            // can be assigned a heap object later, and that pointer has to be
            // found. One range covers every constant, where the routine needed
            // a `hlp_gc_register_root` per object.
            let add_scan_ty = void_type.fn_type(&[ptr_type.into(), i64_type.into()], false);
            let add_scan = self.aot_symbol("hlp_gc_add_scan_root", add_scan_ty);
            let blob_size = blob
                .get_value_type()
                .into_struct_type()
                .size_of()
                .ok_or_else(|| anyhow!("ash_constants has no size"))?;
            self.builder.build_indirect_call(
                add_scan_ty,
                add_scan,
                &[blob.as_pointer_value().into(), blob_size.into()],
                "",
            )?;
        }

        let bytecode = self.bytecode.clone();
        let nfields_offset = std::mem::offset_of!(hl_runtime_obj, nfields) as u64;
        let indexes_offset = std::mem::offset_of!(hl_runtime_obj, fields_indexes) as u64;

        for constant in &bytecode.constants {
            let global_idx = constant.global as usize;
            if statically_built.contains(&global_idx) {
                continue;
            }
            if global_idx >= bytecode.globals.len() || global_idx >= self.globals_data.len() {
                continue;
            }
            let type_idx = bytecode.globals[global_idx].0;
            let hl_type_rust = &bytecode.types[type_idx];
            let Some(&type_ptr) = type_globals.get(&type_idx) else {
                continue;
            };
            let kind = hl_type_rust.kind;
            if kind != hl_type_kind_HOBJ && kind != hl_type_kind_HSTRUCT {
                continue;
            }
            let Some(obj_data) = hl_type_rust.obj.as_ref() else {
                continue;
            };

            let obj = self
                .builder
                .build_indirect_call(alloc_obj_ty, alloc_obj, &[type_ptr.into()], "const_obj")?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| anyhow!("hlp_alloc_obj returned void"))?
                .into_pointer_value();
            let slot = self.aot_global_slot(global_idx)?;
            self.builder.build_store(slot, obj)?;
            self.builder
                .build_indirect_call(register_root_ty, register_root, &[obj.into()], "")?;

            if constant.fields.is_empty() {
                continue;
            }

            let rt = self
                .builder
                .build_indirect_call(get_obj_rt_ty, get_obj_rt, &[type_ptr.into()], "const_rt")?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| anyhow!("hlp_get_obj_rt returned void"))?
                .into_pointer_value();

            // start = rt->nfields - <this type's own field count>: the parent
            // fields come first, and a constant only carries its own.
            let nfields_ptr = unsafe {
                self.builder.build_gep(
                    i8_type,
                    rt,
                    &[i64_type.const_int(nfields_offset, false)],
                    "rt_nfields_ptr",
                )?
            };
            let nfields = self
                .builder
                .build_load(i32_type, nfields_ptr, "rt_nfields")?
                .into_int_value();
            let start = self.builder.build_int_sub(
                nfields,
                i32_type.const_int(obj_data.fields.len() as u64, false),
                "field_start",
            )?;
            let indexes_ptr = unsafe {
                self.builder.build_gep(
                    i8_type,
                    rt,
                    &[i64_type.const_int(indexes_offset, false)],
                    "rt_indexes_ptr",
                )?
            };
            let indexes = self
                .builder
                .build_load(ptr_type, indexes_ptr, "rt_indexes")?
                .into_pointer_value();

            for (j, &field_value) in constant.fields.iter().enumerate() {
                if j >= obj_data.fields.len() {
                    break;
                }
                let field_type_idx = obj_data.fields[j].type_.0;
                let field_kind = bytecode.types[field_type_idx].kind;

                let slot_index = self.builder.build_int_add(
                    start,
                    i32_type.const_int(j as u64, false),
                    "field_slot",
                )?;
                let offset_ptr = unsafe {
                    self.builder
                        .build_gep(i32_type, indexes, &[slot_index], "field_off_ptr")?
                };
                let offset = self
                    .builder
                    .build_load(i32_type, offset_ptr, "field_off")?
                    .into_int_value();
                let offset = self
                    .builder
                    .build_int_s_extend(offset, i64_type, "field_off64")?;
                let addr = unsafe {
                    self.builder
                        .build_gep(i8_type, obj, &[offset], "field_addr")?
                };

                match field_kind {
                    hl_type_kind_HFUN | hl_type_kind_HMETHOD => {
                        let findex = field_value as usize;
                        let Some(&field_type) = type_globals.get(&field_type_idx) else {
                            continue;
                        };
                        if findex >= self.functions_ptrs.len() {
                            continue;
                        }
                        // Read the address out of the function table rather
                        // than naming the body: a findex whose body was
                        // refused has no symbol, and the table is where every
                        // other indirect call already looks.
                        let fslot = self.aot_function_slot(findex)?;
                        let target = self
                            .builder
                            .build_load(ptr_type, fslot, "const_fun")?
                            .into_pointer_value();
                        let closure = self
                            .builder
                            .build_indirect_call(
                                alloc_closure_ty,
                                alloc_closure,
                                &[field_type.into(), target.into()],
                                "const_closure",
                            )?
                            .try_as_basic_value()
                            .basic()
                            .ok_or_else(|| anyhow!("hlp_alloc_closure_void returned void"))?;
                        self.builder.build_store(addr, closure)?;
                    }
                    hl_type_kind_HTYPE => {
                        let referenced = field_value as usize;
                        if let Some(&value) = type_globals.get(&referenced) {
                            self.builder.build_store(addr, value)?;
                        }
                    }
                    hl_type_kind_HOBJ | hl_type_kind_HSTRUCT => {
                        let referenced = field_value as usize;
                        if referenced < self.globals_data.len() {
                            let source = self.aot_global_slot(referenced)?;
                            let value = self.builder.build_load(ptr_type, source, "const_ref")?;
                            self.builder.build_store(addr, value)?;
                        }
                    }
                    hl_type_kind_HBYTES => {
                        let str_idx = field_value as usize;
                        if let Some(Some(global)) = self.string_globals.get(str_idx) {
                            let text = global.as_pointer_value();
                            self.builder.build_store(addr, text)?;
                        }
                    }
                    _ => {
                        // Both the integer and boolean cases, and the
                        // fallthrough the JIT takes: the field value is an
                        // index into the ints table, not the value itself.
                        let value = bytecode
                            .ints
                            .get(field_value as usize)
                            .copied()
                            .unwrap_or(field_value);
                        self.builder
                            .build_store(addr, i32_type.const_int(value as u32 as u64, false))?;
                    }
                }
            }
        }

        self.builder.build_return(None)?;
        if !init.verify(true) {
            return Err(anyhow!("ash_module_init failed LLVM verification"));
        }
        // Whole-module, not just this function: the emitted data is where a
        // mistake would sit, and a malformed global initializer does not
        // surface until a pass walks it.
        self.module
            .verify()
            .map_err(|e| anyhow!("emitted module failed LLVM verification: {}", e.to_string()))?;
        Ok(())
    }

    /// Emit `main`: initialise the module, then enter the bytecode entrypoint.
    pub fn emit_main(&mut self) -> Result<()> {
        let i32_type = self.context.i32_type();
        let i64_type = self.context.i64_type();
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let void_type = self.context.void_type();

        let entry_findex = self.bytecode.entrypoint as usize;
        let entry_fn = self
            .func_cache
            .get(&entry_findex)
            .copied()
            .ok_or_else(|| anyhow!("entrypoint findex {entry_findex} was never lowered"))?;

        let main = self.module.add_function(
            "main",
            i32_type.fn_type(&[i32_type.into(), ptr_type.into()], false),
            None,
        );
        let block = self.context.append_basic_block(main, "entry");
        self.builder.position_at_end(block);

        // The runtime bootstrap the JIT performs in its own process before it
        // runs anything: bring the collector up, and tell it where this
        // thread's stack begins, since conservative scanning starts there.
        // Both belong in the entry frame, not in `ash_module_init`, because
        // the stack top has to be an address in the frame that will still be
        // live when the program allocates.
        let gc_init_ty = void_type.fn_type(&[], false);
        let gc_init = self.aot_symbol("hlp_gc_init", gc_init_ty);
        self.builder
            .build_indirect_call(gc_init_ty, gc_init, &[], "")?;

        let anchor = self
            .builder
            .build_alloca(self.context.i8_type(), "stack_top")?;
        let stack_top = self
            .builder
            .build_ptr_to_int(anchor, i64_type, "stack_top_addr")?;
        let set_stack_top_ty = void_type.fn_type(&[i64_type.into()], false);
        let set_stack_top = self.aot_symbol("hlp_gc_set_stack_top", set_stack_top_ty);
        self.builder.build_indirect_call(
            set_stack_top_ty,
            set_stack_top,
            &[stack_top.into()],
            "",
        )?;

        let init = self
            .module
            .get_function("ash_module_init")
            .ok_or_else(|| anyhow!("emit_main before emit_module_init"))?;
        self.builder
            .build_indirect_call(
                void_type.fn_type(&[], false),
                init.as_global_value().as_pointer_value(),
                &[],
                "",
            )?;
        self.builder
            .build_call(entry_fn, &[], "entrypoint")?;
        self.builder.build_return(Some(&i32_type.const_zero()))?;
        if !main.verify(true) {
            return Err(anyhow!("main failed LLVM verification"));
        }
        Ok(())
    }

    /// Run the middle end once, over the finished module.
    ///
    /// The JIT interleaves optimization with lowering because asking for a
    /// function's address forces its codegen, so each promotion has to be
    /// optimized before it is installed. AOT has no such moment: the object
    /// is written once, at the end, and optimizing there means the inliner
    /// and GVN see every body and every reference at once instead of a
    /// module that is still being built.
    pub fn optimize_module(&self) -> Result<()> {
        // The same shield the JIT raises before every promotion. A trap is
        // lowered as `setjmp`, and C's rule applies to the IR too: a local
        // that is not in memory has an indeterminate value after the jump.
        // `mem2reg` alone is enough to break it -- with the shield down, a
        // nested try/catch reported "none" for an exception the inner handler
        // did catch, while single-level catches still worked, which is the
        // shape of a value that survives one frame and not two.
        let shielded = self.shield_trap_functions_from_optimization();
        if std::env::var_os("ASH_MIDDLE_END_LOG").is_some() {
            eprintln!("[me-aot] shielded {shielded} trap functions");
        }
        // O3, not the JIT's O2. Compile time is a build step here, not
        // something the measured program waits for.
        super::module::run_middle_end_at(&self.module, "default<O3>")
    }

    /// Write the module as LLVM IR, the textual counterpart of `emit_object`.
    pub fn write_ir(&self, path: &std::path::Path) -> Result<()> {
        self.module
            .print_to_file(path)
            .map_err(|e| anyhow!("write {}: {}", path.display(), e.to_string()))
    }

    /// Declare a symbol that `libash_std.a` will supply, and mark it
    /// non-preemptible.
    ///
    /// The default for an external symbol under PIC is that something could
    /// interpose it, so every reference goes through the GOT and every call
    /// through the PLT. For a runtime linked statically into this very binary
    /// that indirection buys nothing and costs a great deal: the loop safe
    /// point reads `ash_fiber_poll_epoch` once per iteration, and as a
    /// preemptible symbol that read is a GOT load the optimizer may not hoist.
    /// One closure_call object carried 140 such loads and 1362 PLT calls,
    /// where the JIT -- which bakes absolute addresses -- pays neither.
    ///
    /// Hidden is right for `hlp_*`/`hl_*`/`ash_*` and wrong for libc: marking
    /// a symbol the dynamic linker must supply, such as `_setjmp`, would fail
    /// the link outright.
    pub(crate) fn aot_runtime_fn(
        &self,
        name: &str,
        signature: inkwell::types::FunctionType<'ctx>,
    ) -> FunctionValue<'ctx> {
        if let Some(existing) = self.module.get_function(name) {
            return existing;
        }
        let function = self
            .module
            .add_function(name, signature, Some(Linkage::External));
        function
            .as_global_value()
            .set_visibility(inkwell::GlobalVisibility::Hidden);
        function
    }

    /// The data counterpart of `aot_runtime_fn`.
    pub(crate) fn aot_runtime_global(
        &self,
        name: &str,
        ty: impl inkwell::types::BasicType<'ctx>,
    ) -> GlobalValue<'ctx> {
        if let Some(existing) = self.module.get_global(name) {
            return existing;
        }
        let global = self.module.add_global(ty, None, name);
        global.set_linkage(Linkage::External);
        global.set_visibility(inkwell::GlobalVisibility::Hidden);
        global
    }

    /// A runtime symbol, declared but not defined here. Calls through it use
    /// the caller's own signature, so a declaration another path made with a
    /// different shape cannot silently retype the call.
    fn aot_symbol(
        &self,
        name: &str,
        fallback: inkwell::types::FunctionType<'ctx>,
    ) -> PointerValue<'ctx> {
        self.aot_runtime_fn(name, fallback)
            .as_global_value()
            .as_pointer_value()
    }
}
