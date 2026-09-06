//! The native host: `wasmtime`.
//!
//! This is the one the conformance lane uses, and it is deliberately the
//! simpler of the two. `wasmtime` already implements WASI preview 1 properly,
//! and it has real fibers, so the import that must suspend suspends: with
//! async support on, a host function may await, and the engine puts the
//! guest's stack aside while something else runs. That is the same capability
//! JSPI gives a browser, available here without a browser.
//!
//! What is here is the assembly: a store, a linker with every import
//! answered, and the call that enters the module. The imports themselves are
//! one module each, because what they answer for is what makes them
//! different: [`fibers`] suspends one and drives the transform's globals,
//! [`process`] starts a command WASI preview 1 has no interface for,
//! [`sockets`] opens what preview 1 cannot, [`dylink`] loads a native library
//! beside the program, [`sdl`] draws for a host with no screen, and
//! [`threads`] answers the one import a threads build asks for -- a thread,
//! which on wasm means another instance of this same module over this same
//! memory.
//!
//! It is also useful before the runtime is finished. A module that still
//! imports `hlp_*` -- because `ash_std` has not been linked into it yet --
//! does not fail with a linker's idea of an error; [`Program::missing`]
//! reports exactly which imports nothing satisfies, which during the port is
//! the question being asked.

pub(crate) mod dylink;
mod fibers;
mod process;
pub(crate) mod sdl;
mod sdl_generated;
mod sockets;
mod threads;

use std::path::{Path, PathBuf};
use std::sync::Arc;

use anyhow::{anyhow, Result};
use wasmtime::{Caller, Config, Engine, Linker, Module, Store};

use process::Finished;
use threads::Spawner;
use wasmtime_wasi::p1::{self, WasiP1Ctx};
use wasmtime_wasi::{DirPerms, FilePerms, WasiCtxBuilder};

/// What the guest gets to see of the outside world.
pub struct Program {
    engine: Engine,
    module: Module,
    /// Where it was loaded from, because a native library shipped with it is
    /// found beside it -- the same rule an HDLL has always followed.
    path: PathBuf,
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
    /// What each `sys.io.Process` the guest started said. See
    /// [`install_process`] for why the whole of it is here at once.
    processes: Vec<Option<Finished>>,
    /// Native libraries loaded beside the program. See [`dylink`].
    pub(crate) libraries: dylink::Libraries,
    /// A window and a frame, for a host with no screen. See [`sdl`].
    pub(crate) sdl: sdl::Sdl,
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
        // A module built for `wasm32-wasip1-threads` has atomics in it and a
        // shared memory under it. Neither costs anything when a module has
        // neither, and refusing to load one is worse than being ready for it.
        config.wasm_threads(true);
        config.shared_memory(true);
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
        Ok(Self {
            engine,
            module,
            path: path.to_path_buf(),
        })
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
                let known_wasi = module.starts_with("wasi_snapshot_preview1")
                    || module.starts_with("wasi_")
                    || (module == "wasi" && import.name() == "thread-spawn");
                let known_host = module == fibers::YIELD_MODULE
                    && (import.name() == fibers::YIELD_NAME
                        || import.name() == "memory"
                        || import.name().starts_with("ash_host_"));
                !(known_wasi || known_host)
            })
            .map(|import| format!("{}.{}", import.module(), import.name()))
            .collect()
    }

    /// The shared memory this module asks for, if it asks for one.
    ///
    /// Built to the type the module declares rather than to a size of the
    /// host's choosing: the minimum is how much data the module has to write
    /// before it runs, and the maximum is what the engine reserves so that
    /// growing the memory never moves it out from under another thread.
    fn shared_memory(&self) -> Result<Option<wasmtime::SharedMemory>> {
        let Some(ty) = self.module.imports().find_map(|import| {
            match (import.module(), import.name(), import.ty()) {
                ("env", "memory", wasmtime::ExternType::Memory(ty)) => Some(ty),
                _ => None,
            }
        }) else {
            return Ok(None);
        };
        let maximum = ty.maximum().ok_or_else(|| {
            anyhow!("the module imports a memory with no maximum, which cannot be shared")
        })?;
        let ty = wasmtime::MemoryType::shared(
            u32::try_from(ty.minimum()).map_err(|_| anyhow!("the memory's minimum is too large"))?,
            u32::try_from(maximum).map_err(|_| anyhow!("the memory's maximum is too large"))?,
        );
        wasmtime::SharedMemory::new(&self.engine, ty)
            .map(Some)
            .map_err(|e| anyhow!("creating the shared memory the module imports: {e}"))
    }

    /// Run the program to completion.
    pub async fn run(
        &self,
        args: &[String],
        dirs: &[std::path::PathBuf],
        threads: bool,
    ) -> Result<Outcome> {
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
                fibers::YIELD_MODULE,
            ));
        }

        let mut store = store_for(&self.engine, args, dirs);
        // A module that shares its memory has a thread to spawn into; one
        // that does not has neither, and asks for neither.
        // The memory is made whenever the module imports one, because
        // without it the module does not instantiate. Whether anything may
        // START a thread on it is a separate question and the host's to
        // answer: a module built for threads runs single-threaded perfectly
        // well, and `--threads` is how this one says yes.
        let memory = self.shared_memory()?;
        let spawner = memory.clone().filter(|_| threads).map(|memory| {
            Spawner::new(self.engine.clone(), self.module.clone(), memory, args, dirs)
        });

        let linker = linker_for(&self.engine, &store, memory.as_ref(), spawner.as_ref())?;

        let instance = linker
            .instantiate_async(&mut store, &self.module)
            .await
            .map_err(|e| anyhow!("instantiating the module: {e}"))?;

        // Before the program initialises, because that is when it resolves
        // its primitives -- and because instantiating a module from inside a
        // call the guest is making is a knot not worth tying.
        let libraries = dylink::load_beside(&mut store, &linker, &instance, &self.path).await?;
        if !libraries.is_empty() {
            eprintln!(
                "[ash] loaded native {}: {}",
                if libraries.names().len() == 1 {
                    "library"
                } else {
                    "libraries"
                },
                libraries.names().join(", ")
            );
        }
        store.data_mut().libraries = libraries;

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
                // A run that rendered what it was asked for, which is an
                // ending rather than a failure: a main loop that never
                // returns can only be left by unwinding it.
                if err.downcast_ref::<sdl::FramesDone>().is_some() {
                    eprint!("{}", store.data().sdl.report());
                    return Ok(Outcome::Exited(0));
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









/// The guest's memory as bytes, and the store's data beside it.
///
/// A module built for threads exports a `SharedMemory` and not a `Memory`,
/// and wasmtime keeps the two apart deliberately: another thread may be
/// writing a shared memory while a host function holds a slice of it, so
/// `Memory`'s own accessors assert they are not looking at one. Matching on
/// `Extern::Memory` alone does not fail loudly on such a module -- it reports
/// that the guest exports no memory, and every host call that needs one
/// starts returning errors.
///
/// Every use of this is a copy in or out at a pointer the guest passed and is
/// waiting on the answer for: an argument buffer, or the destination for a
/// reply. No other thread has a reason to be writing that range, whatever it
/// is doing with the rest of memory.
pub(crate) fn guest_memory<'a>(
    caller: &'a mut Caller<'_, Host>,
) -> Option<(&'a mut [u8], &'a mut Host)> {
    match caller.get_export("memory")? {
        wasmtime::Extern::Memory(memory) => Some(memory.data_and_store_mut(caller)),
        wasmtime::Extern::SharedMemory(memory) => {
            let cells = memory.data();
            let (base, len) = (cells.as_ptr(), cells.len());
            // SAFETY: two borrows that do not overlap -- the memory is not in
            // the store's data -- which is the same split `Memory` makes for
            // the unshared case and cannot express through the borrow
            // checker. The base outlives the call because the instance holds
            // the memory, and it does not move when the memory grows: a
            // shared memory reserves its declared maximum up front, which is
            // why declaring one is required.
            unsafe {
                Some((
                    std::slice::from_raw_parts_mut(base.cast::<u8>().cast_mut(), len),
                    &mut *(caller.data_mut() as *mut Host),
                ))
            }
        }
        _ => None,
    }
}

/// A guest pointer and length as host bytes, or nothing if it does not name
/// memory the guest has.
fn guest_slice(caller: &mut Caller<'_, Host>, ptr: i32, len: i32) -> Option<Vec<u8>> {
    let (start, len) = (usize::try_from(ptr).ok()?, usize::try_from(len).ok()?);
    let (data, _) = guest_memory(caller)?;
    Some(data.get(start..start.checked_add(len)?)?.to_vec())
}






/// The WASI context a store gets: the program's arguments, what the operator
/// opened for it, and the part of the host's environment it is entitled to.
///
/// A free function because every thread needs one of its own. Preview 1 has
/// no way to hand a descriptor table to two instances, so each thread gets a
/// context built the same way rather than the same context -- which is what
/// wasmtime's own wasi-threads does, and it means a file opened on one thread
/// is not open on another.
fn wasi_context(args: &[String], dirs: &[std::path::PathBuf]) -> WasiP1Ctx {
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
    // Anything else the operator named, on the command line or in
    // ASH_WASM_DIRS. A module can open only what has been opened for it,
    // so a program that legitimately reaches outside its own directory --
    // upwards, most often -- needs the host to say so rather than to be
    // refused at the boundary with nothing to do about it.
    let named: Vec<std::path::PathBuf> = std::env::var("ASH_WASM_DIRS")
        .unwrap_or_default()
        .split(',')
        .map(str::trim)
        .filter(|d| !d.is_empty())
        .map(std::path::PathBuf::from)
        .collect();
    for dir in dirs.iter().chain(named.iter()) {
        let at = dir.to_string_lossy().into_owned();
        if let Err(e) = wasi.preopened_dir(dir, &at, DirPerms::all(), FilePerms::all()) {
            eprintln!("[ash-wasm-run] {at} is not available to the program: {e}");
        }
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
    // Only ASH_ names cross by default. A wasm module is a sandbox and
    // the host's environment is not its business; handing over PATH and
    // credentials to get one debugging flag through is not a trade worth
    // making.
    //
    // ASH_WASM_ENV names what else may. A program that legitimately reads
    // a variable -- `Sys.getEnv` means the same thing on every other
    // target -- would otherwise see nothing at all on wasm, and the host
    // is the only party that can say which of its variables the guest is
    // entitled to. Naming them keeps that decision explicit and with the
    // side that owns the secret.
    let allowed: Vec<String> = std::env::var("ASH_WASM_ENV")
        .unwrap_or_default()
        .split([',', ' '])
        .map(str::trim)
        .filter(|n| !n.is_empty())
        .map(str::to_string)
        .collect();
    for (key, value) in std::env::vars() {
        if key.starts_with("ASH_") || allowed.contains(&key) {
            wasi.env(&key, &value);
        }
    }    wasi.build_p1()
}

/// A store with a fresh host in it.
fn store_for(engine: &Engine, args: &[String], dirs: &[std::path::PathBuf]) -> Store<Host> {
    Store::new(
        engine,
        Host {
            wasi: wasi_context(args, dirs),
            sockets: sockets::Table::default(),
            processes: Vec::new(),
            libraries: dylink::Libraries::default(),
            sdl: sdl::Sdl::new(),
        },
    )
}

/// Everything a store needs answered, including a thread to run in.
fn linker_for(
    engine: &Engine,
    store: &Store<Host>,
    memory: Option<&wasmtime::SharedMemory>,
    spawner: Option<&Arc<Spawner>>,
) -> Result<Linker<Host>> {
    let mut linker: Linker<Host> = Linker::new(engine);
    p1::add_to_linker_async(&mut linker, |host: &mut Host| &mut host.wasi)
        .map_err(|e| anyhow!("adding WASI to the linker: {e}"))?;
    fibers::install(&mut linker)?;
    process::install(&mut linker)?;
    sockets::install(&mut linker)?;
    dylink::install(&mut linker)?;
    sdl::install(&mut linker)?;
    threads::install(&mut linker, store, memory, spawner.cloned())?;
    Ok(linker)
}







