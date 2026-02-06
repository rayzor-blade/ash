use ash_macro::to_llvm;
use inkwell::context::Context;

#[to_llvm]
extern "C" {
    fn test_function(x: i32) -> i32;
}

#[test]
fn test() -> anyhow::Result<()> {
    let context = Context::create();
    let module = context.create_module("example");
    let builder = context.create_builder();

    let fn_llvm = test_function_to_llvm(&context, &module, &builder)?;
    println!("{}", module.print_to_string().to_string());
    Ok(())
}