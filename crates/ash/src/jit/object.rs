use crate::types::{HLObjField, HLObjProto, HLTypeObj};
use anyhow::{anyhow, Result};
use inkwell::{
    types::StructType,
    values::{PointerValue, StructValue},
    AddressSpace,
};

use super::module::JITModule;

impl<'ctx> JITModule<'ctx> {
    pub fn create_hl_type_obj_struct_value(
        &mut self,
        hl_type_obj: &HLTypeObj,
        llvmtype: StructType<'ctx>,
    ) -> Result<StructValue<'ctx>> {
        let obj_type = llvmtype;

        // Calculate the global_value
        let global_value = if hl_type_obj.global_value != 0 {
            let index = hl_type_obj.global_value as usize - 1;
            if let Some(global) = self.globals.get(&index) {
                global.as_pointer_value()
            } else {
                return Err(anyhow!("Global value not found for index: {}", index));
            }
        } else {
            self.context.ptr_type(AddressSpace::default()).const_null()
        };

        // Create the struct value
        let struct_value = obj_type.const_named_struct(&[
            // nfields
            self.context
                .i32_type()
                .const_int(hl_type_obj.fields.len() as u64, false)
                .into(),
            // nproto
            self.context
                .i32_type()
                .const_int(hl_type_obj.proto.len() as u64, false)
                .into(),
            // nbindings
            self.context
                .i32_type()
                .const_int(hl_type_obj.bindings.len() as u64, false)
                .into(),
            // name
            self.context
                .const_string(hl_type_obj.name.as_bytes(), false)
                .into(),
            // super_
            match &hl_type_obj.super_ {
                Some(super_type) => {
                    if let Some(type_struct_value) =
                        self.initialized_type_cache.get(&(super_type.0 as usize))
                    {
                        // self.struct_value_to_pointer(*)?.into()
                        (*type_struct_value).into()
                    } else {
                        self.context
                            .ptr_type(AddressSpace::default())
                            .const_zero()
                            .into()
                    }
                }
                None => self
                    .context
                    .ptr_type(AddressSpace::default())
                    .const_zero()
                    .into(),
            },
            // fields
            self.create_obj_fields_array(&hl_type_obj.name, &hl_type_obj.fields)?
                .into(),
            // proto
            self.create_obj_proto_array(&hl_type_obj.name, &hl_type_obj.proto)?
                .into(),
            // bindings
            self.context
                .i32_type()
                .const_array(
                    &hl_type_obj
                        .bindings
                        .iter()
                        .map(|&b| self.context.i32_type().const_int(b as u64, false))
                        .collect::<Vec<_>>(),
                )
                .into(),
            // global_value
            global_value.into(),
            // module_context (null for now)
            self.context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .const_null()
                .into(),
            // rt (null for now)
            self.context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .const_null()
                .into(),
        ]);

        Ok(struct_value)
    }

    pub(crate) fn create_obj_fields_array(
        &mut self,
        obj_name: &str,
        mut fields: &[HLObjField],
    ) -> Result<PointerValue<'ctx>> {
        let field_type = self.context.opaque_struct_type("hl_obj_field");
        field_type.set_body(
            &[
                self.context.ptr_type(AddressSpace::default()).into(), // name
                self.context.ptr_type(AddressSpace::default()).into(), // t
                self.context.i32_type().into(),                        // hashed_name
            ],
            false,
        );

        let _fields = fields
            .iter()
            .map(|field| {
                field_type.const_named_struct(&[
                    self.context
                        .const_string(field.name.as_bytes(), false)
                        .into(),
                    self.context
                        .ptr_type(AddressSpace::default())
                        .const_zero()
                        .into(),
                    self.context
                        .i32_type()
                        .const_int(field.hashed_name as u64, false)
                        .into(),
                ])
            })
            .collect::<Vec<StructValue<'ctx>>>();

        let field_array = field_type.const_array(&_fields);

        let global = self.module.add_global(
            field_array.get_type(),
            None,
            &format!("{}_fields_array", obj_name),
        );
        global.set_initializer(&field_array);
        Ok(global.as_pointer_value())
    }

    fn create_obj_proto_array(
        &self,
        obj_name: &str,
        proto: &[HLObjProto],
    ) -> Result<PointerValue<'ctx>> {
        let proto_type = self.context.opaque_struct_type("hl_obj_proto");
        proto_type.set_body(
            &[
                self.context.ptr_type(AddressSpace::default()).into(), // name
                self.context.i32_type().into(),                        // findex
                self.context.i32_type().into(),                        // pindex
                self.context.i32_type().into(),                        // hashed_name
            ],
            false,
        );

        let proto_array = proto_type.const_array(
            &proto
                .iter()
                .map(|p| {
                    proto_type.const_named_struct(&[
                        self.context.const_string(p.name.as_bytes(), false).into(),
                        self.context
                            .i32_type()
                            .const_int(p.findex as u64, false)
                            .into(),
                        self.context
                            .i32_type()
                            .const_int(p.pindex as u64, false)
                            .into(),
                        self.context
                            .i32_type()
                            .const_int(p.hashed_name as u64, false)
                            .into(),
                    ])
                })
                .collect::<Vec<StructValue<'ctx>>>(),
        );

        let global = self.module.add_global(
            proto_array.get_type(),
            None,
            &format!("{}_proto_array", obj_name),
        );
        global.set_initializer(&proto_array);
        Ok(global.as_pointer_value())
    }
}
