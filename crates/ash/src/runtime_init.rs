//! Process-global runtime setup shared by every tier: choose and initialize
//! the std runtime, dlopen the HDLLs, register the dynamic-call hook.

use crate::native_lib::{init_std_library, NativeFunctionResolver};
use anyhow::Result;
use std::ffi::c_void;
use std::path::Path;

/// Main thread, once, before any compiled tier exists. These are global and
/// ordered, which is why they are not part of any module's constructor.
pub fn prepare_process_globals(
    path: &Path,
    natives: &[crate::types::HLNative],
    resolver: &mut NativeFunctionResolver,
) -> Result<()> {
    crate::native_lib::choose_std_linkage(path);
    init_std_library();
    let search_dir = path.parent().unwrap_or(Path::new("."));
    // This path prepares the host process, so the host's answer is the
    // right one.
    resolver.discover_and_load_libraries(search_dir, natives, true)?;
    setup_callbacks_global(resolver);
    Ok(())
}

/// The dynamic-call hook `hlp_call_method` needs (Type.createInstance and
/// friends), registered once per process.
///
/// Deliberately NOT the closure runner that `setup_callbacks` also
/// installs: which runner is correct depends on the host. The interpreter
/// installs its own; only standalone JIT execution wants the typed native
/// bridge. Registering that from a shared startup path would override the
/// interpreter's.
pub fn setup_callbacks_global(resolver: &NativeFunctionResolver) {
    if let (Ok(setup_fn_ptr), Ok(static_call_ptr)) = (
        resolver.resolve_function("std", "hl_setup_callbacks2"),
        resolver.resolve_function("std", "ash_static_call"),
    ) {
        type FnSetupCallbacks2 = unsafe extern "C" fn(*mut c_void, *mut c_void, i32);
        let setup: FnSetupCallbacks2 = unsafe { std::mem::transmute(setup_fn_ptr) };
        // flags=0: fun arg is the direct function pointer (not double-indirection)
        // wrapper=null: we don't use the wrapper mechanism
        unsafe { setup(static_call_ptr, std::ptr::null_mut(), 0) };
    }
}
