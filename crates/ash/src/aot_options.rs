//! Optimization policy scoped to one AOT build and inherited by its workers.
use std::cell::RefCell;
use std::sync::Arc;

use anyhow::{Result, bail};

#[derive(Clone, Copy, Debug, PartialEq, Eq, clap::ValueEnum)]
pub enum LlvmOptLevel {
    #[value(name = "0", alias = "o0")]
    O0,
    #[value(name = "1", alias = "o1")]
    O1,
    #[value(name = "2", alias = "o2")]
    O2,
    #[value(name = "3", alias = "o3")]
    O3,
    #[value(name = "s", alias = "os")]
    Size,
    #[value(name = "z", alias = "oz")]
    MinSize,
}
impl LlvmOptLevel {
    fn pipeline(self) -> &'static str {
        match self {
            Self::O0 => "default<O0>",
            Self::O1 => "default<O1>",
            Self::O2 => "default<O2>",
            Self::O3 => "default<O3>",
            Self::Size => "default<Os>",
            Self::MinSize => "default<Oz>",
        }
    }
    fn codegen(self) -> u8 {
        match self {
            Self::O0 => 0,
            Self::O1 => 1,
            Self::O3 => 3,
            _ => 2,
        }
    }
}

/// Explicit settings override their environment equivalents. AIR stays independent of LLVM.
#[derive(Clone, Copy, Debug, Default)]
pub struct AotOptions {
    pub llvm: Option<LlvmOptLevel>,
    pub air: Option<u8>,
    pub codegen: Option<u8>,
}
#[derive(Debug)]
pub(crate) struct Resolved {
    air: Option<air::v2::OptLevel>,
    pipeline: Option<String>,
    codegen: u8,
    pub(crate) size: Option<LlvmOptLevel>,
}
thread_local! { static CURRENT: RefCell<Option<Arc<Resolved>>> = const { RefCell::new(None) }; }
pub(crate) struct Guard(Option<Arc<Resolved>>);
impl Drop for Guard {
    fn drop(&mut self) {
        CURRENT.with(|s| s.replace(self.0.take()));
    }
}
pub(crate) fn current() -> Option<Arc<Resolved>> {
    CURRENT.with(|s| s.borrow().clone())
}
pub(crate) fn inherit(options: Option<Arc<Resolved>>) -> Guard {
    Guard(CURRENT.with(|s| s.replace(options)))
}
fn environment_codegen() -> Result<u8> {
    match std::env::var("ASH_AOT_CODEGEN_LEVEL") {
        Err(std::env::VarError::NotPresent) => Ok(3),
        Ok(value) => match value.as_str() {
            "0" => Ok(0),
            "1" => Ok(1),
            "2" => Ok(2),
            "3" => Ok(3),
            _ => bail!("ASH_AOT_CODEGEN_LEVEL must be 0, 1, 2 or 3"),
        },
        Err(_) => bail!("ASH_AOT_CODEGEN_LEVEL must be 0, 1, 2 or 3"),
    }
}
pub(crate) fn enter(options: AotOptions) -> Result<Guard> {
    let air = match options.air {
        None => None,
        Some(0) => Some(air::v2::OptLevel::O0),
        Some(1) => Some(air::v2::OptLevel::O1),
        Some(2) => Some(air::v2::OptLevel::O2),
        Some(3) => Some(air::v2::OptLevel::O3),
        Some(_) => bail!("AIR optimization level must be 0, 1, 2 or 3"),
    };
    let codegen = match options.codegen.or(options.llvm.map(LlvmOptLevel::codegen)) {
        Some(n @ 0..=3) => n,
        Some(_) => bail!("codegen optimization level must be 0, 1, 2 or 3"),
        None => environment_codegen()?,
    };
    let pipeline = options
        .llvm
        .map(|level| level.pipeline().to_string())
        .or_else(|| std::env::var("ASH_LLVM_PASSES").ok());
    Ok(inherit(Some(Arc::new(Resolved {
        air,
        pipeline,
        codegen,
        size: options
            .llvm
            .filter(|level| matches!(level, LlvmOptLevel::Size | LlvmOptLevel::MinSize)),
    }))))
}
pub(crate) fn air_level() -> Option<air::v2::OptLevel> {
    current().and_then(|s| s.air)
}
pub(crate) fn codegen_level() -> Result<u8> {
    current().map_or_else(environment_codegen, |s| Ok(s.codegen))
}
pub(crate) fn pass_pipeline(default: &str) -> String {
    match current() {
        Some(s) => s.pipeline.clone().unwrap_or_else(|| default.to_owned()),
        None => std::env::var("ASH_LLVM_PASSES").unwrap_or_else(|_| default.to_owned()),
    }
}
