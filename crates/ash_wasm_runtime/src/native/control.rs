//! One lifetime and one clock for all instances of a guest program.
use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Condvar, Mutex};
use std::time::{Duration, Instant};

use super::{Host, Outcome};
use wasmtime::{Caller, Engine, Linker, SharedMemory};

#[derive(Default)]
struct State {
    outcome: Option<Outcome>,
    waiters: HashMap<u64, usize>,
}

pub(super) struct Control {
    state: Mutex<State>,
    changed: Condvar,
    async_changed: tokio::sync::Notify,
    engine: Engine,
    memory: Option<SharedMemory>,
    epoch: Instant,
    stores: AtomicUsize,
}

impl Control {
    pub fn new(engine: Engine, memory: Option<SharedMemory>) -> Arc<Self> {
        Arc::new(Self {
            state: Mutex::new(State::default()),
            changed: Condvar::new(),
            async_changed: tokio::sync::Notify::new(),
            engine,
            memory,
            epoch: Instant::now(),
            stores: AtomicUsize::new(0),
        })
    }

    pub fn outcome(&self) -> Option<Outcome> {
        self.state.lock().unwrap().outcome.clone()
    }

    pub async fn cancelled(&self) {
        let notified = self.async_changed.notified();
        tokio::pin!(notified);
        notified.as_mut().enable();
        if self.outcome().is_none() {
            notified.await;
        }
    }

    /// The first exit/trap wins. Wake real atomic waits, including the race
    /// between registering a wait and actually entering the engine's wait.
    /// Do not change guest mutex words or pretend that a dead mutator parked.
    pub fn finish(self: &Arc<Self>, outcome: Outcome) {
        {
            let mut state = self.state.lock().unwrap();
            if state.outcome.is_some() {
                return;
            }
            state.outcome = Some(outcome);
        }
        self.engine.increment_epoch();
        self.async_changed.notify_waiters();
        // An unrelated run on the same Engine can already be inside its
        // deadline callback when we exit. Wasmtime sets the new deadline
        // AFTER that callback returns, so a single increment can be lost.
        // Repeat only during shutdown, until all this run's stores drop.
        if self.stores.load(Ordering::Acquire) != 0 {
            let control = self.clone();
            if let Err(error) = std::thread::Builder::new()
                .name("ash-wasm-exit".into())
                .spawn(move || {
                    while control.stores.load(Ordering::Acquire) != 0 {
                        control.engine.increment_epoch();
                        std::thread::sleep(Duration::from_millis(1));
                    }
                })
            {
                eprintln!("[ash-wasm-run] cannot repeat shutdown interrupts: {error}");
            }
        }
        if let Some(memory) = &self.memory {
            let mut state = self.state.lock().unwrap();
            while !state.waiters.is_empty() {
                let addresses: Vec<_> = state.waiters.keys().copied().collect();
                drop(state);
                for address in addresses {
                    let _ = memory.atomic_notify(address, u32::MAX);
                }
                state = self.state.lock().unwrap();
                if !state.waiters.is_empty() {
                    state = self
                        .changed
                        .wait_timeout(state, Duration::from_millis(1))
                        .unwrap()
                        .0;
                }
            }
        }
    }

    pub fn enter(self: &Arc<Self>) -> InstanceGuard {
        self.stores.fetch_add(1, Ordering::AcqRel);
        InstanceGuard(self.clone())
    }

    fn wait(
        &self,
        memory: &SharedMemory,
        address: u32,
        offset: u64,
        expected: u64,
        timeout: i64,
        wide: bool,
    ) -> wasmtime::Result<u32> {
        let address = (address as u64)
            .checked_add(offset)
            .ok_or(wasmtime::Trap::MemoryOutOfBounds)?;
        {
            let mut state = self.state.lock().unwrap();
            if state.outcome.is_some() {
                return Err(wasmtime::Error::msg("guest program terminated"));
            }
            *state.waiters.entry(address).or_default() += 1;
        }
        let timeout = (timeout >= 0).then(|| Duration::from_nanos(timeout as u64));
        let result = if wide {
            memory.atomic_wait64(address, expected, timeout)
        } else {
            memory.atomic_wait32(address, expected as u32, timeout)
        };
        let mut state = self.state.lock().unwrap();
        let count = state.waiters.get_mut(&address).unwrap();
        *count -= 1;
        if *count == 0 {
            state.waiters.remove(&address);
        }
        self.changed.notify_all();
        if state.outcome.is_some() {
            return Err(wasmtime::Error::msg("guest program terminated"));
        }
        Ok(match result? {
            wasmtime::WaitResult::Ok => 0,
            wasmtime::WaitResult::Mismatch => 1,
            wasmtime::WaitResult::TimedOut => 2,
        })
    }
}

pub(super) struct InstanceGuard(Arc<Control>);
impl Drop for InstanceGuard {
    fn drop(&mut self) {
        self.0.stores.fetch_sub(1, Ordering::AcqRel);
    }
}

/// Cancellation of an embedding's run future also stops the guest's workers.
pub(super) struct RunGuard(pub Arc<Control>);
impl Drop for RunGuard {
    fn drop(&mut self) {
        self.0
            .finish(Outcome::Trapped("guest run cancelled".into()));
    }
}

pub(super) struct Clock(pub Arc<Control>);
impl wasmtime_wasi::clocks::HostMonotonicClock for Clock {
    fn resolution(&self) -> u64 {
        1
    }
    fn now(&self) -> u64 {
        self.0.epoch.elapsed().as_nanos() as u64
    }
}

pub(super) fn install(linker: &mut Linker<Host>) -> anyhow::Result<()> {
    linker.func_wrap(
        "env",
        ash_wasm_link::waits::WAIT32,
        |mut caller: Caller<'_, Host>, address: u32, expected: u32, timeout: i64, offset: u64| {
            let memory = wait_memory(&mut caller)?;
            caller
                .data()
                .control
                .wait(&memory, address, offset, expected as u64, timeout, false)
        },
    )?;
    linker.func_wrap(
        "env",
        ash_wasm_link::waits::WAIT64,
        |mut caller: Caller<'_, Host>, address: u32, expected: u64, timeout: i64, offset: u64| {
            let memory = wait_memory(&mut caller)?;
            caller
                .data()
                .control
                .wait(&memory, address, offset, expected, timeout, true)
        },
    )?;
    Ok(())
}

fn wait_memory(caller: &mut Caller<'_, Host>) -> wasmtime::Result<SharedMemory> {
    if let Some(memory) = &caller.data().control.memory {
        return Ok(memory.clone());
    }
    // Non-threaded embedders may define and export a shared memory instead
    // of importing env.memory. Preserve their wait/timeout semantics too.
    match caller.get_export("memory") {
        Some(wasmtime::Extern::SharedMemory(memory)) => Ok(memory),
        _ => Err(wasmtime::Error::msg("atomic wait without shared memory")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn a_store_created_after_exit_never_enters_guest_code() {
        let mut config = wasmtime::Config::new();
        config.epoch_interruption(true);
        let engine = Engine::new(&config).unwrap();
        let control = Control::new(engine.clone(), None);
        control.finish(Outcome::Exited(23));
        let mut store = super::super::store_for(&engine, &[], &[], control.clone());
        let module = wasmtime::Module::new(
            &engine,
            "(module (func (export \"main\") (result i32) i32.const 99))",
        )
        .unwrap();
        let instance = wasmtime::Instance::new_async(&mut store, &module, &[])
            .await
            .unwrap();
        let main = instance
            .get_typed_func::<(), i32>(&mut store, "main")
            .unwrap();
        assert!(main.call_async(&mut store, ()).await.is_err());
        assert_eq!(control.outcome(), Some(Outcome::Exited(23)));
    }
}
