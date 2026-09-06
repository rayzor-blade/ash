//! Suspending a fiber, and the three globals that say one is suspended.
//!
//! Two mechanisms, and they are not interchangeable. `wasmtime` has real
//! fibers, so an import that awaits suspends the guest with no rewrite at
//! all -- that is the cheap one, and what to use where the engine has it.
//! ash's link-time transform is the other: it adds three globals to the
//! module and every instrumented frame reads them on its way out, which is
//! what makes a fiber work on an engine with no suspension of its own. The
//! imports here drive both.

use anyhow::{anyhow, Result};
use wasmtime::{Caller, Linker};

use super::Host;

/// The module and name the guest imports its yield under.
pub(crate) const YIELD_MODULE: &str = crate::FIBER_YIELD_IMPORT.0;
pub(crate) const YIELD_NAME: &str = crate::FIBER_YIELD_IMPORT.1;

/// The globals the link-time fiber transform adds, if it was applied.
const STATE_GLOBAL: &str = "ash_fiber_state";
const DATA_GLOBAL: &str = "ash_fiber_data";
/// The module's own shadow stack pointer, exported only when the linker
/// instrumented it, because a fiber has to run on a region of its own.
const SP_GLOBAL: &str = "__stack_pointer";
/// State values shared with the transform, which are Asyncify's.
const UNWINDING: i32 = 1;
const REWINDING: i32 = 2;

/// The transform's state global, if this module has one.
fn fiber_global(caller: &mut Caller<'_, Host>, name: &str) -> Option<wasmtime::Global> {
    match caller.get_export(name) {
        Some(wasmtime::Extern::Global(g)) => Some(g),
        _ => None,
    }
}

/// Supply the imports that have to reach outside the sandbox to suspend.
///
/// Two mechanisms, and the module says which it wants. A module the linker
/// instrumented exports the transform's state global: suspending it means
/// setting that global and letting the instrumented frames unwind themselves
/// back to the scheduler, and the engine is not involved. A module without it
/// has no way to unwind, so the engine has to do the suspending -- which
/// wasmtime's async support can, by parking the guest stack while a host
/// function awaits.
///
/// Both are here because they are not interchangeable. Engine suspension is
/// cheaper and needs no rewrite, and is what to use where the engine has it;
/// the transform is what makes a fiber work where it does not.
pub(crate) fn install(linker: &mut Linker<Host>) -> Result<()> {
    linker
        .func_wrap_async(
            YIELD_MODULE,
            YIELD_NAME,
            |mut caller: Caller<'_, Host>, _params: ()| {
                // Setting the state is what makes every instrumented frame
                // between here and the scheduler return on its way out.
                if let Some(state) = fiber_global(&mut caller, STATE_GLOBAL) {
                    let now = state.get(&mut caller).i32().unwrap_or(0);
                    // On a rewind this is the call the fiber stopped at, so
                    // reaching it again means the rewind is over.
                    let next = if now == REWINDING { 0 } else { UNWINDING };
                    let _ = state.set(&mut caller, wasmtime::Val::I32(next));
                    return Box::new(async {}) as _;
                }
                Box::new(async {
                    tokio::task::yield_now().await;
                })
            },
        )
        .map_err(|e| anyhow!("installing the fiber yield import: {e}"))?;

    // What the transform's state global says. The guest cannot read it: the
    // global is added after the guest has been compiled, so there is no name
    // in the guest to refer to it by.
    linker
        .func_wrap(
            YIELD_MODULE,
            "ash_host_fiber_state",
            |mut caller: Caller<'_, Host>| -> i32 {
                fiber_global(&mut caller, STATE_GLOBAL)
                    .and_then(|g| g.get(&mut caller).i32())
                    .unwrap_or(0)
            },
        )
        .map_err(|e| anyhow!("installing the fiber state import: {e}"))?;

    // Point the transform at a fiber's side stack and say whether the next
    // entry is a rewind. An uninstrumented module has neither global and
    // needs neither: this is then a no-op and its fibers run to completion.
    linker
        .func_wrap(
            YIELD_MODULE,
            "ash_host_fiber_arm",
            |mut caller: Caller<'_, Host>, data: i32, rewind: i32, sp: i32| -> i32 {
                if let Some(g) = fiber_global(&mut caller, DATA_GLOBAL) {
                    let _ = g.set(&mut caller, wasmtime::Val::I32(data));
                }
                if let Some(g) = fiber_global(&mut caller, STATE_GLOBAL) {
                    let next = if rewind != 0 { REWINDING } else { 0 };
                    let _ = g.set(&mut caller, wasmtime::Val::I32(next));
                }
                // Swap in the fiber's own shadow stack and hand back whose it
                // was, so the caller can be put back exactly where it was.
                match fiber_global(&mut caller, SP_GLOBAL) {
                    Some(g) if sp != 0 => {
                        let was = g.get(&mut caller).i32().unwrap_or(0);
                        let _ = g.set(&mut caller, wasmtime::Val::I32(sp));
                        was
                    }
                    Some(g) => g.get(&mut caller).i32().unwrap_or(0),
                    None => 0,
                }
            },
        )
        .map_err(|e| anyhow!("installing the fiber arm import: {e}"))?;
    Ok(())
}
