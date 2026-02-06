use ash::module::AshModule;
use anyhow::Result;
use std::env;
use std::path::PathBuf;
use std::str::FromStr;
use std::collections::HashMap;
use inkwell::context::Context;

pub fn main() {
    let context = Context::create();
    let mut libraries = HashMap::new();
 
    let mut ash_module = AshModule::new(&context, "test_module");

    let mut cwd = PathBuf::from_str(env!("CARGO_MANIFEST_DIR")).unwrap();
    cwd.push("test/test.hl");

    // Note: This test will fail unless you provide a valid bytecode file
    ash_module.load_bytecode(&cwd).unwrap();
    // Load libraries before initializing
    ash_module.load_libraries(libraries).unwrap();
    ash_module.initialize().unwrap();
    //
    // Test global lookups
    assert!(ash_module.get_int_global(0).is_some());
    assert!(ash_module.get_float_global(0).is_some());
    assert!(ash_module.get_string_global(0).is_some());
    //

    ash_module.print_llvm_ir();
 

}