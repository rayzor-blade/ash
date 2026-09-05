//! The prelinked wasm runtime must define what the emitter calls.
//!
//! `ash_runtime.o` is ash_std, a wasi libc and libsetjmp joined into one
//! relocatable object, built separately from the crate that emits calls into
//! it. Nothing rebuilds it when ash_std gains an export, so it goes stale in
//! silence: the module links, and then fails at instantiate with
//! `unknown import: env::<name>` -- which reads as a linker problem rather
//! than a stale artifact. That happened the day `hlp_set_null_write_raises`
//! was added.
//!
//! So this asserts that every runtime entry point `ash_module_init` emits a
//! call to is defined in the object sitting beside `ash`. It does not link or
//! run anything; it reads the object with ash's own reader.
//!
//! Skipped, not failed, when the object is absent: a checkout that has never
//! built the wasm runtime is not broken, and CI builds it before running this.

use std::path::PathBuf;

/// Names `emit_module_init` calls. Adding a call there without adding it here
/// is what this test exists to catch, so keep them together.
const REQUIRED: &[&str] = &[
    "hlp_hash_gen",
    "hlp_register_aot_symbols",
    "hlp_set_null_write_raises",
    "hlp_set_compiled_worker_mode",
    "hlp_install_static_call",
    "hlp_install_closure_runner",
    "hlp_gc_set_globals",
    "hlp_gc_add_scan_root",
    "hlp_error",
    "hlp_gc_init",
    "hlp_register_aot_debug_files",
    // Not module init: every compiled body of a wasm module opens and closes
    // its shadow frame through these, so a runtime without them fails the
    // same way, at instantiate.
    "hlp_shadow_push",
    "hlp_shadow_pop",
];

/// How to rebuild it, quoted in the failure so nobody has to go looking.
const RECIPE: &str = "\
    cargo rustc -p ash_std --target wasm32-wasip1 --release --crate-type staticlib\n\
    rust-lld -flavor wasm -r -o target/release/wasm32-wasip1/ash_runtime.o \\\n\
      --whole-archive target/wasm32-wasip1/release/libash_std.a --no-whole-archive \\\n\
      -L$(brew --prefix wasi-libc)/share/wasi-sysroot/lib/wasm32-wasip1 -lc -lsetjmp\n\
    \n\
    --no-whole-archive is load-bearing: without it libc's crt1 and long-double\n\
    printf are pulled in, and the module then imports __main_argc_argv and\n\
    __multc3, which no wasm compiler-rt here can satisfy.";

fn runtime_object() -> Option<PathBuf> {
    if let Some(explicit) = std::env::var_os("ASH_RUNTIME") {
        let p = PathBuf::from(explicit);
        return p.is_file().then_some(p);
    }
    // Walk up rather than assuming a layout. This binary is not in
    // `target/<profile>/deps/` -- it lands under
    // `target/<profile>/build/<crate>/<hash>/out/` -- and guessing `deps`
    // made the test skip itself against a runtime it should have rejected,
    // which is worse than having no test.
    let exe = std::env::current_exe().ok()?;
    exe.ancestors()
        .map(|dir| dir.join("wasm32-wasip1").join("ash_runtime.o"))
        .find(|p| p.is_file())
}

#[test]
fn the_wasm_runtime_defines_what_module_init_calls() {
    let Some(path) = runtime_object() else {
        eprintln!("no wasm32-wasip1/ash_runtime.o beside the test binary; skipping");
        return;
    };
    let bytes = std::fs::read(&path).expect("reading the runtime object");
    let obj = ash_wasm_link::read("ash_runtime.o", &bytes).expect("parsing the runtime object");

    let defined: std::collections::HashSet<&str> = obj
        .symbols
        .iter()
        .filter(|s| !s.is_undefined())
        .map(|s| s.name.as_str())
        .collect();

    let missing: Vec<&str> = REQUIRED
        .iter()
        .copied()
        .filter(|n| !defined.contains(n))
        .collect();

    assert!(
        missing.is_empty(),
        "{} is stale: it does not define {}.\n\n\
         A wasm build will link and then fail at instantiate with\n\
         `unknown import: env::{}`. Rebuild it:\n\n{RECIPE}",
        path.display(),
        missing.join(", "),
        missing[0],
    );
}
