pub mod air;
pub mod function;
pub mod module;
pub mod object;
pub mod stub_bridge;
pub mod tbaa;
pub mod type_utils;
pub mod types;

#[cfg(test)]
mod module_test;

/// Run a bytecode file under the standalone whole-module LLVM JIT — the
/// `--mode jit` rung of the unified CLI. Owns the LLVM context for the
/// process lifetime; the profile scopes match what the old `ash` binary
/// reported, so historical setup/compile/execute splits stay comparable.
pub fn run_whole_module(hl_path: &std::path::Path) -> anyhow::Result<()> {
    let context = inkwell::context::Context::create();
    let mut module = {
        let _p = crate::profile::scope("jit init");
        module::JITModule::new(&context, hl_path)
    };
    // The whole-module compile happens inside execute_main, so the two are
    // separated here rather than in the callee.
    {
        let _p = crate::profile::scope("compile + setup + run");
        module.execute_main()?;
    }
    Ok(())
}
