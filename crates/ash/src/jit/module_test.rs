use std::path::PathBuf;
use std::str::FromStr;
use super::module::*;
use anyhow::{anyhow, Result};
use inkwell::context::Context;

#[test]
fn test_init_module() -> Result<()> {
    let mut test_hl =
        PathBuf::from_str(env!("CARGO_MANIFEST_DIR")).expect("Expected to get manifest path");
    test_hl.push("test/test.hl");

    println!("{:?}", test_hl.try_exists().err());

    let context = Context::create();
    let module = JITModule::new(&context, &test_hl);
    println!("{}", module.module.print_to_string().to_str()?);

    Ok(())
}
