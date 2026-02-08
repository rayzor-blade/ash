use ash::jit::module::JITModule;
use inkwell::context::Context;
use std::path::PathBuf;
use std::str::FromStr;

pub fn main() {
    let context = Context::create();
    let cwd = PathBuf::from_str(env!("CARGO_MANIFEST_DIR"))
        .unwrap()
        .join("test/test.hl");

    let mut module = JITModule::new(&context, &cwd);
    module.execute_main().expect("Failed to execute main");
}
