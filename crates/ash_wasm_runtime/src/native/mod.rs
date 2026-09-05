//! The native host: `wasmtime`.
//!
//! This is the one the conformance lane uses, and it is deliberately the
//! simpler of the two. `wasmtime` already implements WASI preview 1 properly,
//! and it has real fibers, so the import that must suspend suspends: with
//! async support on, a host function may await, and the engine puts the
//! guest's stack aside while something else runs. That is the same capability
//! JSPI gives a browser, available here without a browser.
//!
//! What WASI preview 1 does not have is sockets, and the guest asks for those
//! through its own `env.ash_host_socket_*` imports; [`sockets`] answers them
//! with the operating system's.
//!
//! It is also useful before the runtime is finished. A module that still
//! imports `hlp_*` -- because `ash_std` has not been linked into it yet --
//! does not fail with a linker's idea of an error; [`Program::missing`]
//! reports exactly which imports nothing satisfies, which during the port is
//! the question being asked.

mod sockets;

use std::path::Path;

use anyhow::{anyhow, Result};
use wasmtime::{Config, Engine, Linker, Module, Store};
use wasmtime_wasi::p1::{self, WasiP1Ctx};
use wasmtime_wasi::{DirPerms, FilePerms, WasiCtxBuilder};

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

/// The store's data: everything a host function reaches through its
/// `Caller`. WASI keeps its own descriptor table in `wasi`; the guest's
/// sockets live in a second table because a socket is not a WASI fd and must
/// never be mistaken for one -- closing guest socket 3 through `fd_close`
/// would close the preopened working directory.
pub(crate) struct Host {
    wasi: WasiP1Ctx,
    sockets: sockets::Table,
}

impl Program {
    /// Load a module, without running it.
    pub fn load(path: &Path) -> Result<Self> {
        let mut config = Config::new();
        // Fibers need a host function that can suspend. In this wasmtime that
        // is the default rather than a switch, so nothing is set for it; the
        // capability is what matters, and `func_wrap_async` below uses it.
        //
        // Exceptions are not the default. ash's trap model is `setjmp`, which
        // the WebAssembly backend lowers into the exception-handling
        // instructions, so a module built from it does not even parse without
        // this: "exceptions proposal not enabled".
        config.wasm_exceptions(true);
        // Compiling a module is the cost of running one: the conformance
        // suite's module is 23MB and takes wasmtime six seconds and every
        // core, and isolation runs it once per case. wasmtime keeps compiled
        // code keyed by module hash in the user's cache directory, the same
        // one its own CLI uses; with the cache a second run of the same
        // module is a load. A cache that cannot be set up is not a reason
        // not to run, so that failure only costs the speed.
        if let Ok(cache) = wasmtime::Cache::from_file(None) {
            config.cache(Some(cache));
        }
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
                let known_host = module == FIBER_YIELD_MODULE
                    && (import.name() == FIBER_YIELD_NAME
                        || import.name().starts_with("ash_host_"));
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
                 A program linked against the wasm runtime imports only WASI and the \
                 {}.ash_host_* imports this host provides.",
                missing.len(),
                missing
                    .iter()
                    .take(5)
                    .cloned()
                    .collect::<Vec<_>>()
                    .join(", "),
                FIBER_YIELD_MODULE,
            ));
        }

        let mut wasi = WasiCtxBuilder::new();
        wasi.inherit_stdout().inherit_stderr();
        // The working directory, as the program's own. A native ash program
        // can write a file beside itself; a wasm one can only reach what the
        // host preopens, and with nothing preopened every `File.write` failed
        // with "Can't open" while the same program ran natively. The
        // directory is the one the host was started in, nothing above it.
        if let Err(e) = wasi.preopened_dir(".", ".", DirPerms::all(), FilePerms::all()) {
            eprintln!("[ash-wasm-run] the working directory is not available to the program: {e}");
        }
        for arg in args {
            wasi.arg(arg);
        }
        // The runtime inside the module reads its switches from the
        // environment, exactly as the native one does -- ASH_GC_STRESS and the
        // rest. Without this the guest sees an empty environment and every
        // diagnostic is unreachable, which is the difference between being
        // able to ask a question of a wasm build and not.
        //
        // Only ASH_ names cross. A wasm module is a sandbox and the host's
        // environment is not its business; handing over PATH and credentials
        // to get one debugging flag through is not a trade worth making.
        for (key, value) in std::env::vars() {
            if key.starts_with("ASH_") {
                wasi.env(&key, &value);
            }
        }
        let mut store = Store::new(
            &self.engine,
            Host {
                wasi: wasi.build_p1(),
                sockets: sockets::Table::default(),
            },
        );

        let mut linker: Linker<Host> = Linker::new(&self.engine);
        p1::add_to_linker_async(&mut linker, |host: &mut Host| &mut host.wasi)
            .map_err(|e| anyhow!("adding WASI to the linker: {e}"))?;
        install_fiber_yield(&mut linker)?;
        sockets::install(&mut linker)?;

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
    async fn call(self, store: &mut Store<Host>) -> Result<i32> {
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
fn install_fiber_yield(linker: &mut Linker<Host>) -> Result<()> {
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
