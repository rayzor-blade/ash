//! A host module registered into a decoded program, run the way the CLI
//! runs one: decode, register, resolver, `HLInterpreter::new`,
//! `execute_entrypoint`, in `--mode interp`, `--mode hybrid` and `--mode jit`.
//!
//! Each mode runs in a child process (this binary re-invoked), because the
//! runtime is process-global and because a hybrid run has to leave through
//! `_exit` as the CLI does: LLVM's atexit handlers tear down state a
//! pre-warm or a compile still in flight is using. `harness = false` so the
//! child is plain `main`.

use ash_core::bytecode::BytecodeDecoder;
use ash_core::host_module::{HostClass, HostMethod, HostModule, HostType};
use ash_core::native_lib::{self, NativeFunctionResolver};
use ash_interp::interpreter::{HLInterpreter, TieredConfig};
use std::collections::HashMap;
use std::ffi::c_void;
use std::path::PathBuf;
use std::process::Command;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};

const CHILD_MODE: &str = "ASH_HOST_MODULE_CHILD";

fn fixture() -> PathBuf {
    let mut p = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    p.push("tests/fixtures/host_module/main.hl");
    p
}

fn main() {
    if let Ok(mode) = std::env::var(CHILD_MODE) {
        child(&mode);
    }
    if !cfg!(unix) {
        println!("host_module: skipped, unix only");
        return;
    }
    let exe = std::env::current_exe().expect("current exe");
    let mut failed = false;
    for mode in ["interp", "hybrid", "jit"] {
        let out = Command::new(&exe)
            .env(CHILD_MODE, mode)
            .output()
            .expect("spawn child");
        let stdout = String::from_utf8_lossy(&out.stdout);
        let ok = out.status.success() && stdout.trim() == "3";
        println!(
            "host_module::{mode} ... {}",
            if ok { "ok" } else { "FAILED" }
        );
        if !ok {
            failed = true;
            eprintln!(
                "--- {mode}: status {:?}\n--- stdout:\n{stdout}\n--- stderr:\n{}",
                out.status,
                String::from_utf8_lossy(&out.stderr)
            );
        }
    }
    if failed {
        std::process::exit(1);
    }
}

/// The program's `test.Greeter` C type, and `hlp_alloc_obj`, handed to the
/// natives once the interpreter exists.
static GREETER_TYPE: AtomicUsize = AtomicUsize::new(0);
static ALLOC_OBJ: AtomicUsize = AtomicUsize::new(0);
/// The abstract field is opaque, so the count lives beside the object.
static COUNTS: Mutex<Option<HashMap<usize, i32>>> = Mutex::new(None);

extern "C" fn greeter_make() -> *mut c_void {
    type Alloc = unsafe extern "C" fn(*mut c_void) -> *mut c_void;
    let alloc: Alloc = unsafe { std::mem::transmute(ALLOC_OBJ.load(Ordering::Acquire)) };
    let obj = unsafe { alloc(GREETER_TYPE.load(Ordering::Acquire) as *mut c_void) };
    COUNTS
        .lock()
        .unwrap()
        .get_or_insert_with(HashMap::new)
        .insert(obj as usize, 0);
    obj
}

extern "C" fn greeter_bump(g: *mut c_void) -> i32 {
    let mut counts = COUNTS.lock().unwrap();
    let count = counts
        .get_or_insert_with(HashMap::new)
        .entry(g as usize)
        .or_insert(0);
    *count += 1;
    *count
}

fn host_module() -> HostModule {
    HostModule {
        lib: "host".into(),
        classes: vec![HostClass {
            name: "host.Greeters".into(),
            superclass: None,
            fields: vec![],
            methods: vec![],
            statics: vec![
                HostMethod {
                    name: "make".into(),
                    symbol: "greeter_make".into(),
                    params: vec![],
                    ret: HostType::Obj("test.Greeter".into()),
                    func: greeter_make as *const c_void,
                },
                HostMethod {
                    name: "bump".into(),
                    symbol: "greeter_bump".into(),
                    params: vec![HostType::Obj("test.Greeter".into())],
                    ret: HostType::I32,
                    func: greeter_bump as *const c_void,
                },
            ],
            ctor: None,
        }],
    }
}

/// Run the fixture with the module registered and leave the way the CLI
/// does. The program's output is this process's stdout.
fn child(mode: &str) -> ! {
    let path = fixture();
    assert!(path.exists(), "fixture not built: {}", path.display());
    native_lib::choose_std_linkage(&path);
    native_lib::init_std_library().expect("std library");

    let mut bc = BytecodeDecoder::decode(&path).expect("decode");
    bc.register_host_module(&host_module()).expect("register");
    assert!(bc.type_index_of("host.Greeters").is_some());
    assert!(bc.type_index_of("host.$Greeters").is_some());
    let greeter = bc.type_index_of("test.Greeter").expect("program class");
    let bc = Arc::new(bc);

    let mut resolver = NativeFunctionResolver::new().with_host_natives(&bc.host_natives);
    let search_dir = path.parent().unwrap();
    // Lib `host` is covered by the registration: nothing on disk is wanted.
    resolver
        .discover_and_load_libraries(search_dir, &bc.natives, true)
        .expect("discovery skips a host-covered library");
    let make = resolver
        .resolve_function("host", "hlp_greeter_make")
        .expect("host native resolves");
    assert_eq!(make as usize, greeter_make as *const () as usize);
    ALLOC_OBJ.store(
        resolver.resolve_function("std", "hlp_alloc_obj").unwrap() as usize,
        Ordering::Release,
    );

    let mut interp = HLInterpreter::new(&bc, &resolver);
    GREETER_TYPE.store(interp.c_type_of(greeter) as usize, Ordering::Release);
    if mode != "interp" {
        // `jit` is the CLI's `--mode jit`: every reached function compiled
        // before its first call, so the host natives are reached from
        // compiled code for certain rather than when a broker gets to it.
        let cfg = TieredConfig {
            enabled: true,
            compiled_only: mode == "jit",
            jit_threshold: 1,
            ..TieredConfig::default()
        };
        interp
            .enable_tiered(&path, &resolver, &bc, cfg)
            .expect("tiered pre-warm");
        assert!(interp.tiered_stats().is_some(), "the tiers are armed");
    }
    // The host class is a decoded class to the layout oracle.
    let mismatches = interp
        .verify_layout_oracle(&bc, &resolver)
        .expect("layout oracle");
    assert!(mismatches.is_empty(), "layout mismatches: {mismatches:?}");

    interp
        .execute_entrypoint(&bc, &resolver)
        .expect("program runs");

    // The host class got its class object: what `Type` access reads.
    type TypeGetGlobal = unsafe extern "C" fn(*mut c_void) -> *mut c_void;
    let get_global: TypeGetGlobal = unsafe {
        std::mem::transmute(
            resolver
                .resolve_function("std", "hlp_type_get_global")
                .unwrap(),
        )
    };
    let greeters = bc.type_index_of("host.Greeters").unwrap();
    let class_obj = unsafe { get_global(interp.c_type_of(greeters)) };
    assert!(!class_obj.is_null(), "host class has no class object");
    if mode != "interp" {
        interp.quiesce_promotions();
    }
    // Leaked, as the CLI leaks them: a compile thread may still hold shares.
    std::mem::forget(interp);
    std::mem::forget(bc);
    use std::io::Write;
    let _ = std::io::stdout().flush();
    let _ = std::io::stderr().flush();
    #[cfg(unix)]
    unsafe {
        libc::_exit(0)
    }
    #[cfg(not(unix))]
    std::process::exit(0)
}
