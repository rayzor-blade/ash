//! Natives that take more words than the interpreter's register dispatch.
//!
//! The dispatch is a ladder of fixed signatures that ends where the integer
//! registers do. Past that the callee reads its later arguments from the
//! stack, and where each one sits is the platform's rule: Apple's arm64 packs
//! a narrow argument into the bytes it needs, SysV and Windows give every one
//! a full word. Compiled code gets that right because Cranelift lowers the
//! call. A wide native is called the same way, through a thunk built from its
//! signature, made when it is first called and kept for the process.

use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use anyhow::{Result, anyhow};
use ash_core::cranelift::backend::AshCraneliftBackend;
use ash_core::cranelift::codegen::compile_native_thunk;
use ash_core::hl_bindings as hl;

/// One parameter word per entry, as a buffer. The result is one word, a float
/// as its `f64` bits.
pub(super) type Thunk = unsafe extern "C" fn(*const u64) -> u64;

#[derive(Hash, PartialEq, Eq)]
struct Signature {
    target: usize,
    context: bool,
    args: Vec<hl::hl_type_kind>,
    ret: hl::hl_type_kind,
}

struct Thunks {
    backend: AshCraneliftBackend,
    /// The bead owns each thunk's code, so it stays beside the pointer.
    made: HashMap<Signature, (Thunk, Arc<beadie::Bead>)>,
}

static THUNKS: Mutex<Option<Thunks>> = Mutex::new(None);

/// The thunk that calls `target` with `arg_kinds`, and `context` ahead of them
/// when the native has one.
pub(super) fn thunk_for(
    target: usize,
    context: bool,
    arg_kinds: &[hl::hl_type_kind],
    ret_kind: hl::hl_type_kind,
) -> Result<Thunk> {
    let signature = Signature {
        target,
        context,
        args: arg_kinds.to_vec(),
        ret: ret_kind,
    };
    let mut guard = THUNKS.lock().unwrap_or_else(|e| e.into_inner());
    if guard.is_none() {
        let backend = AshCraneliftBackend::new()
            .map_err(|e| anyhow!("no compiler for a native call with many arguments: {e}"))?;
        *guard = Some(Thunks {
            backend,
            made: HashMap::new(),
        });
    }
    let thunks = guard.as_mut().expect("initialised above");
    if let Some((thunk, _)) = thunks.made.get(&signature) {
        return Ok(*thunk);
    }
    let bead = beadie::Bead::new(target as beadie::CoreHandle, None);
    let code = compile_native_thunk(&thunks.backend, &bead, target, context, arg_kinds, ret_kind)?;
    // SAFETY: the thunk takes the buffer pointer and returns one word, the
    // signature `compile_native_thunk` emits.
    let thunk: Thunk = unsafe { std::mem::transmute::<usize, Thunk>(code) };
    thunks.made.insert(signature, (thunk, bead));
    Ok(thunk)
}
