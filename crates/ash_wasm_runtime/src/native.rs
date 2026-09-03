//! The native host: `wasmtime`.
//!
//! This is the one the conformance lane uses, and it is deliberately the
//! simpler of the two. `wasmtime` already implements WASI preview 1 properly,
//! and it has real fibers, so the import that must suspend suspends: with
//! async support on, a host function may await, and the engine puts the
//! guest's stack aside while something else runs. That is the same capability
//! JSPI gives a browser, available here without a browser.
//!
//! It is also useful before the runtime is finished. A module that still
//! imports `hlp_*` -- because `ash_std` has not been linked into it yet --
//! does not fail with a linker's idea of an error; [`Program::missing`]
//! reports exactly which imports nothing satisfies, which during the port is
//! the question being asked.

use std::path::Path;

use anyhow::{anyhow, Result};
use wasmtime::{Config, Engine, Linker, Module, Store};
use wasmtime_wasi::p1::{self, WasiP1Ctx};
use wasmtime_wasi::WasiCtxBuilder;

/// What the guest gets to see of the outside world.
pub struct Program {
    engine: Engine,
    module: Module,
}

/// How a run ended.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Outcome {
    /// The program returned, with this status.
    Exited(i32),
    /// The program trapped. The string is the trap, already formatted.
    Trapped(String),
}

impl Program {
    /// Load a module, without running it.
    pub fn load(path: &Path) -> Result<Self> {
        let mut config = Config::new();
        // Fibers need a host function that can suspend. In this wasmtime it
        // is the default rather than a switch, so nothing is set here; the
        // capability is what matters, and `func_wrap_async` below uses it.
        let _ = &mut config;
        let engine =
            Engine::new(&config).map_err(|e| anyhow!("creating the wasmtime engine: {e}"))?;
        let module = Module::from_file(&engine, path)
            .map_err(|e| anyhow!("loading {}: {e}", path.display()))?;
        Ok(Self { engine, module })
    }

    /// The imports this module needs that neither WASI nor this host supplies.
    ///
    /// Empty is the goal. Anything listed is a symbol the runtime has not
    /// provided yet, and during the port that list IS the work.
    pub fn missing(&self) -> Vec<String> {
        self.module
            .imports()
            .filter(|import| {
                let module = import.module();
                let known_wasi =
                    module.starts_with("wasi_snapshot_preview1") || module.starts_with("wasi_");
                let known_host = module == FIBER_YIELD_MODULE && import.name() == FIBER_YIELD_NAME;
                !(known_wasi || known_host)
            })
            .map(|import| format!("{}.{}", import.module(), import.name()))
            .collect()
    }

    /// Run the program to completion.
    pub async fn run(&self, args: &[String]) -> Result<Outcome> {
        let missing = self.missing();
        if !missing.is_empty() {
            return Err(anyhow!(
                "the module imports {} symbol(s) no host can supply, the first few being {}. \
                 A program linked against the wasm runtime imports only WASI and {}.{}.",
                missing.len(),
                missing
                    .iter()
                    .take(5)
                    .cloned()
                    .collect::<Vec<_>>()
                    .join(", "),
                FIBER_YIELD_MODULE,
                FIBER_YIELD_NAME
            ));
        }

        let mut wasi = WasiCtxBuilder::new();
        wasi.inherit_stdout().inherit_stderr();
        for arg in args {
            wasi.arg(arg);
        }
        let mut store = Store::new(&self.engine, wasi.build_p1());

        let mut linker: Linker<WasiP1Ctx> = Linker::new(&self.engine);
        p1::add_to_linker_async(&mut linker, |ctx: &mut WasiP1Ctx| ctx)
            .map_err(|e| anyhow!("adding WASI to the linker: {e}"))?;
        install_fiber_yield(&mut linker)?;

        let instance = linker
            .instantiate_async(&mut store, &self.module)
            .await
            .map_err(|e| anyhow!("instantiating the module: {e}"))?;

        // A command module is entered through `_start`; one linked without a
        // command entry is entered through `main`.
        let entry = instance
            .get_typed_func::<(), ()>(&mut store, "_start")
            .ok()
            .map(Entry::Start)
            .or_else(|| {
                instance
                    .get_typed_func::<(i32, i32), i32>(&mut store, "main")
                    .ok()
                    .map(Entry::Main)
            })
            .ok_or_else(|| anyhow!("the module exports neither _start nor main"))?;

        match entry.call(&mut store).await {
            Ok(code) => Ok(Outcome::Exited(code)),
            Err(err) => {
                // `proc_exit` unwinds by trapping, and a status is how it
                // reports itself rather than a failure.
                if let Some(exit) = err.downcast_ref::<wasmtime_wasi::I32Exit>() {
                    return Ok(Outcome::Exited(exit.0));
                }
                Ok(Outcome::Trapped(format!("{err:?}")))
            }
        }
    }
}

enum Entry {
    Start(wasmtime::TypedFunc<(), ()>),
    Main(wasmtime::TypedFunc<(i32, i32), i32>),
}

impl Entry {
    async fn call(self, store: &mut Store<WasiP1Ctx>) -> Result<i32> {
        match self {
            Entry::Start(f) => {
                f.call_async(store, ()).await?;
                Ok(0)
            }
            // argc/argv: the program reads its arguments through WASI, so the
            // C-shaped pair is passed empty.
            Entry::Main(f) => Ok(f.call_async(store, (0, 0)).await?),
        }
    }
}

const FIBER_YIELD_MODULE: &str = crate::FIBER_YIELD_IMPORT.0;
const FIBER_YIELD_NAME: &str = crate::FIBER_YIELD_IMPORT.1;

/// Supply the one import that has to suspend.
///
/// With async support on, awaiting inside a host function is a real
/// suspension: the engine parks the guest's stack and returns to the
/// scheduler, and the guest resumes where it stopped. Yielding to the async
/// runtime is the smallest honest implementation -- it gives other tasks a
/// turn -- and a scheduler that wants to decide the order can replace this
/// with one that blocks on its own signal.
fn install_fiber_yield(linker: &mut Linker<WasiP1Ctx>) -> Result<()> {
    linker
        .func_wrap_async(
            FIBER_YIELD_MODULE,
            FIBER_YIELD_NAME,
            |_caller, _params: ()| {
                Box::new(async {
                    tokio::task::yield_now().await;
                })
            },
        )
        .map_err(|e| anyhow!("installing the fiber yield import: {e}"))?;
    Ok(())
}
