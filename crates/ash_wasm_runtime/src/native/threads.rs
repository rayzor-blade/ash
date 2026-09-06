//! Starting a thread, which on wasm means starting another instance.
//!
//! A module built for `wasm32-wasip1-threads` asks for a thread through one
//! import, `wasi.thread-spawn`. What the host owes it is not a stack: it is a
//! second instance of the same module over the same memory, entered through
//! the export wasi-libc leaves for exactly this.

use std::path::PathBuf;
use std::sync::Arc;

use anyhow::{anyhow, Result};
use wasmtime::{Caller, Engine, Linker, Module};

use super::{linker_for, store_for, Host};

/// What a thread needs in order to become another instance of this program.
///
/// A thread on wasm is not a stack. It is a second instance of the same
/// module over the same memory, entered through the export wasi-libc leaves
/// for exactly this: `wasi_thread_start(tid, start_arg)`, where `start_arg`
/// points at a structure the guest's own `pthread_create` filled in -- the
/// stack it allocated included. So the host allocates nothing and knows
/// nothing about what the thread will do; it makes the instance and makes the
/// call.
///
/// Everything `run` needed to make the first instance is therefore needed
/// again from inside a host call, and this is it.
pub(crate) struct Spawner {
    engine: Engine,
    module: Module,
    memory: wasmtime::SharedMemory,
    /// What the program was started with, because each thread builds a WASI
    /// context the same way. See [`wasi_context`] for why it is built again
    /// rather than shared.
    args: Vec<String>,
    dirs: Vec<PathBuf>,
    /// wasi-threads requires a thread id to be positive and distinct.
    next_id: std::sync::atomic::AtomicI32,
}

impl Spawner {
    pub(crate) fn new(
        engine: Engine,
        module: Module,
        memory: wasmtime::SharedMemory,
        args: &[String],
        dirs: &[PathBuf],
    ) -> Arc<Self> {
        Arc::new(Self {
            engine,
            module,
            memory,
            args: args.to_vec(),
            dirs: dirs.to_vec(),
            next_id: std::sync::atomic::AtomicI32::new(1),
        })
    }

    /// Start one, and answer with its id.
    ///
    /// A negative answer is what the interface has for a thread that could
    /// not be started, and what wasi-libc's `pthread_create` turns into
    /// `EAGAIN`. ash's own worker pool asks for threads and counts what it
    /// got, so a refusal costs it workers rather than the run.
    fn spawn(self: &Arc<Self>, start_arg: i32) -> i32 {
        let id = self
            .next_id
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let me = Arc::clone(self);
        let started = std::thread::Builder::new()
            .name(format!("wasi-thread-{id}"))
            .spawn(move || {
                if let Err(e) = me.run_thread(id, start_arg) {
                    eprintln!("[ash-wasm-run] thread {id}: {e}");
                }
            });
        match started {
            Ok(_) => id,
            Err(e) => {
                eprintln!("[ash-wasm-run] could not start thread {id}: {e}");
                -1
            }
        }
    }

    /// The thread itself: a store of its own, this module again, and the one
    /// call.
    ///
    /// The store is new because a `Store` belongs to the thread that runs it,
    /// and the runtime is new because the calls here are the async ones the
    /// fiber import needs -- a thread that borrowed the main one would be
    /// running the host's futures on the guest's thread.
    ///
    /// What is NOT new is the memory, which is the whole point: this instance
    /// sees the first one's heap, and its `__wasm_init_memory` finds the flag
    /// already set and leaves the data alone.
    fn run_thread(self: &Arc<Self>, id: i32, start_arg: i32) -> Result<()> {
        let runtime = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .map_err(|e| anyhow!("a runtime for the thread: {e}"))?;
        runtime.block_on(async {
            let mut store = store_for(&self.engine, &self.args, &self.dirs);
            let linker = linker_for(&self.engine, &store, Some(self))?;
            let instance = linker
                .instantiate_async(&mut store, &self.module)
                .await
                .map_err(|e| anyhow!("instantiating the module for the thread: {e}"))?;
            let start = instance
                .get_typed_func::<(i32, i32), ()>(&mut store, "wasi_thread_start")
                .map_err(|e| {
                    anyhow!(
                        "the module exports no wasi_thread_start, so a thread has nothing \
                         to enter: {e}"
                    )
                })?;
            start
                .call_async(&mut store, (id, start_arg))
                .await
                .map_err(|e| anyhow!("the thread trapped: {e}"))
        })
    }
}

/// Where a threads build asks for a thread.
///
/// The import has to be answered whether or not this program can start one:
/// an import nothing supplies is a link error before a line runs, so a
/// module built for the threads target and never starting a thread would not
/// start at all. Without a shared memory there is nothing for a second
/// instance to run on, and the answer is the negative one.
pub(crate) fn install(
    linker: &mut Linker<Host>,
    store: &wasmtime::Store<Host>,
    spawner: Option<Arc<Spawner>>,
) -> Result<()> {
    // A module that shares its memory cannot make one: every thread
    // instantiates that same module, and a memory it defined would be one per
    // thread. So the host makes it, once, and hands it to all of them.
    if let Some(spawner) = &spawner {
        linker
            .define(store, "env", "memory", spawner.memory.clone())
            .map_err(|e| anyhow!("giving the module its shared memory: {e}"))?;
    }
    linker
        .func_wrap(
            "wasi",
            "thread-spawn",
            move |_: Caller<'_, Host>, start_arg: i32| -> i32 {
                match &spawner {
                    Some(spawner) => spawner.spawn(start_arg),
                    None => -1,
                }
            },
        )
        .map_err(|e| anyhow!("installing the thread-spawn import: {e}"))?;
    Ok(())
}
