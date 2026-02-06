use std::fmt::Write;
use proc_macro::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{parse_macro_input, FnArg, ForeignItem, ForeignItemFn, Ident, ItemFn, ItemForeignMod, Pat, ReturnType, Signature, Type};

#[proc_macro_attribute]
pub fn load_symbol(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ItemForeignMod);
    
    let mut expanded = quote! {};

    for item in input.items.iter() {
        if let ForeignItem::Fn(func) = item {
            let fn_name = &func.sig.ident;
            let inputs = &func.sig.inputs;
            let output = &func.sig.output;

            let symbol_name = fn_name.to_string();

            let arg_types: Vec<_> = inputs.iter().map(|arg| {
                if let FnArg::Typed(pat_type) = arg {
                    &pat_type.ty
                } else {
                    panic!("Unsupported argument type")
                }
            }).collect();

            let arg_names: Vec<_> = inputs.iter().map(|arg| {
                if let FnArg::Typed(pat_type) = arg {
                    &pat_type.pat
                } else {
                    panic!("Unsupported argument type")
                }
            }).collect();

            let return_type = match output {
                ReturnType::Default => quote! { () },
                ReturnType::Type(_, ty) => ty.to_token_stream(),
            };

            let wrapper_name = Ident::new(&format!("__wrap_{}", fn_name), fn_name.span());
            let f_name = Ident::new(&format!("__{}", fn_name), fn_name.span());
            expanded = quote! {
                #expanded

                #[allow(non_upper_case_globals)]
                static #wrapper_name: std::sync::OnceLock<libloading::Symbol<unsafe extern "C" fn(#(#arg_types),*) -> #return_type>> = std::sync::OnceLock::new();

                #[allow(non_snake_case)]
                pub unsafe fn #f_name(#inputs) #output {
                    let symbol = #wrapper_name.get_or_init(|| {
                        let lib = crate::native_lib::STD_LIBRARY
                        .as_ref().expect("Standard library not initialized");
                        lib.get(#symbol_name.as_bytes()).expect("Failed to load symbol")
                    });
                    symbol(#(#arg_names),*)
                }
            };
        }
    }

    let final_expanded = quote! {
        #input

        #expanded
    };

    TokenStream::from(final_expanded)
}



#[proc_macro_attribute]
pub fn to_llvm(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ItemForeignMod);
    
    let extern_functions: Vec<_> = input.items.iter().filter_map(|item| {
        if let ForeignItem::Fn(func) = item {
            Some(func)
        } else {
            None
        }
    }).collect();

    let generated_functions = extern_functions.iter().map(|func| {
        let fn_name = &func.sig.ident;
        let fn_args = &func.sig.inputs;
        let fn_output = &func.sig.output;

        let llvm_fn_name = format_ident!("{}_to_llvm", fn_name);

        // Generate function parameters
        let params: Vec<(Ident, &Box<Type>, proc_macro2::TokenStream)>= fn_args.iter().enumerate().map(|(i, arg)| {
            if let FnArg::Typed(pat_type) = arg {
                let ident = if let Pat::Ident(pat_ident) = &*pat_type.pat {
                    format_ident!("{}", pat_ident.ident)
                } else {
                    format_ident!("param_{}", i)
                };
                let param_type = &pat_type.ty;
                let type_tokens = get_type_tokens(param_type);
                (ident, param_type, type_tokens)
            } else {
                let ident = format_ident!("param_{}", i);
                let param_type = match arg {
                    FnArg::Receiver(_) => panic!("Self parameters are not supported"),
                    _ => panic!("Unexpected argument type"),
                };
                let type_tokens = quote! { context.i8_type().ptr_type(inkwell::AddressSpace::default()).into() };
                (ident, param_type, type_tokens)
            }
        }).collect();

        let param_indices: Vec<usize> = (0..params.len()).collect();
        let param_names: Vec<&Ident> = params.iter().map(|(name, ..)| name).collect();
        let param_types: Vec<&proc_macro2::TokenStream> = params.iter().map(|(_, _, type_tokens)| type_tokens).collect();

        let return_type = match fn_output {
            ReturnType::Default => quote! { context.void_type().into() },
            ReturnType::Type(_, ty) => get_type_tokens(ty),
        };

        let handle_return = match fn_output {
            ReturnType::Default => quote! {
                builder.build_return(None)?;
            },
            ReturnType::Type(_, _) => quote! {
                let params_array: Vec<inkwell::values::BasicMetadataValueEnum<'ctx>> = vec![#(#param_names.as_basic_value_enum().into()),*];
                let call_site = builder.build_call(
                    function,
                    params_array.as_slice(),
                    "call_result"
                )?;
                let return_value = call_site.try_as_basic_value().left().unwrap();
                builder.build_return(Some(&return_value))?;
            },
        };

        quote! {
            pub fn #llvm_fn_name<'ctx>(
                context: &'ctx inkwell::context::Context,
                module: &inkwell::module::Module<'ctx>,
                builder: &inkwell::builder::Builder<'ctx>
            ) -> anyhow::Result<inkwell::values::FunctionValue<'ctx>> {
                use inkwell::values::{BasicValue, FunctionValue, BasicValueEnum, BasicMetadataValueEnum};
                use inkwell::types::BasicType;
                use inkwell::types::BasicMetadataTypeEnum;
                
                let return_type = #return_type;
                let mut param_types:Vec<inkwell::types::BasicMetadataTypeEnum<'ctx>> = vec![#(#param_types),*];
               
                let fn_type:inkwell::types::FunctionType<'ctx> = match return_type {
                    BasicMetadataTypeEnum::ArrayType(t) => t.fn_type(&param_types, false),
                    BasicMetadataTypeEnum::FloatType(t) => t.fn_type(&param_types, false),
                   
                        BasicMetadataTypeEnum::IntType(t) => t.fn_type(&param_types, false),
                        BasicMetadataTypeEnum::PointerType(t) => t.fn_type(&param_types, false),
                        BasicMetadataTypeEnum::StructType(t) => t.fn_type(&param_types, false),
                        BasicMetadataTypeEnum::VectorType(t) => t.fn_type(&param_types, false),
                        _=> return Err(anyhow::anyhow!("Invalid return type"))
                };
                
              
         
                let function: inkwell::values::FunctionValue<'ctx> = 
                    module.add_function(stringify!(#fn_name), fn_type, Some(inkwell::module::Linkage::External));

                let basic_block = context.append_basic_block(function, "entry");
                builder.position_at_end(basic_block);

                // Get function parameters
                #(let #param_names: BasicValueEnum<'ctx> = function.get_nth_param(#param_indices as u32).unwrap();)*

                // Handle return
                #handle_return

                Ok(function)
            }
        }
    });

    let generated = quote! {
        // #input

        #(#generated_functions)*
    };

    TokenStream::from(generated)
}

fn get_type_tokens(ty: &Type) -> proc_macro2::TokenStream {
    match ty {
        Type::Path(type_path) => {
            let type_name = type_path.path.segments.last().unwrap().ident.to_string();
            match type_name.as_str() {
                "i8" => quote! { { let t:inkwell::types::BasicMetadataTypeEnum<'ctx> = context.i8_type().into(); t} },
                "i16" => quote! { { let t:inkwell::types::BasicMetadataTypeEnum<'ctx> = context.i16_type().into(); t} },
                "i32" => quote! { { let t:inkwell::types::BasicMetadataTypeEnum<'ctx> =context.i32_type().into(); t} },
                "i64" => quote! { { let t:inkwell::types::BasicMetadataTypeEnum<'ctx> =context.i64_type().into(); t} },
                "u8" => quote! { { let t:inkwell::types::BasicMetadataTypeEnum<'ctx> =context.i8_type().into(); t} },
                "u16" => quote! { { let t:inkwell::types::BasicMetadataTypeEnum<'ctx> =context.i16_type().into(); t} },
                "u32" => quote! { { let t:inkwell::types::BasicMetadataTypeEnum<'ctx> =context.i32_type().into(); t} },
                "u64" => quote! { { let t:inkwell::types::BasicMetadataTypeEnum<'ctx> =context.i64_type().into(); t} },
                "f32" => quote! { { let t:inkwell::types::BasicMetadataTypeEnum<'ctx> =context.f32_type().into(); t} },
                "f64" => quote! { { let t:inkwell::types::BasicMetadataTypeEnum<'ctx> =context.f64_type().into(); t} },
                "bool" => quote! { { let t:inkwell::types::BasicMetadataTypeEnum<'ctx> =context.bool_type().into(); t} },
                "c_void" => quote! { { let t:inkwell::types::BasicMetadataTypeEnum<'ctx> =context.i8_type().ptr_type(inkwell::AddressSpace::default()).into(); t} },
                _ => quote! { { let t:inkwell::types::BasicMetadataTypeEnum<'ctx> =context.i8_type().ptr_type(inkwell::AddressSpace::default()).into(); t} },
            }
        },
        Type::Ptr(_) => quote! { context.i8_type().ptr_type(inkwell::AddressSpace::default()).into() },
        Type::Reference(_) => quote! { context.i8_type().ptr_type(inkwell::AddressSpace::default()).into() },
        _ => quote! { context.i8_type().ptr_type(inkwell::AddressSpace::default()).into() },
    }
}