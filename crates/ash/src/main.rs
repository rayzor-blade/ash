use ash::jit::module::JITModule;
use inkwell::context::Context;
use std::path::PathBuf;
use std::str::FromStr;

pub fn main() {
    let context = Context::create();
    let args: Vec<String> = std::env::args().collect();
    let hl_path = if let Some(path) = args.iter().find(|a| !a.starts_with("--") && a.ends_with(".hl")) {
        PathBuf::from(path)
    } else {
        PathBuf::from_str(env!("CARGO_MANIFEST_DIR"))
            .unwrap()
            .join("test/test.hl")
    };

    let mut module = JITModule::new(&context, &hl_path);
    module.execute_main().expect("Failed to execute main");
}
