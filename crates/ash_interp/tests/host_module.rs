//! A host module registered into a decoded program, run the way the CLI
//! runs one: decode, register, resolver, `HLInterpreter::new`,
//! `execute_entrypoint`, in `--mode interp`, `--mode hybrid` and `--mode jit`,
//! and with each compiled tier alone, so a host native with a context word
//! is reached from every tier's code.
//!
//! Each mode runs in a child process (this binary re-invoked), because the
//! runtime is process-global and because a hybrid run has to leave through
//! `_exit` as the CLI does: LLVM's atexit handlers tear down state a
//! pre-warm or a compile still in flight is using. `harness = false` so the
//! child is plain `main`.

use ash_core::bytecode::BytecodeDecoder;
use ash_core::host_module::{HostClass, HostMethod, HostModule, HostType};
use ash_core::native_lib::{self, NativeFunctionResolver};
use ash_interp::interpreter::{HLInterpreter, TierMode, TieredConfig};
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
    for mode in ["interp", "hybrid", "cranelift", "llvm", "jit"] {
        let out = Command::new(&exe)
            .env(CHILD_MODE, mode)
            .output()
            .expect("spawn child");
        let stdout = String::from_utf8_lossy(&out.stdout);
        let ok = out.status.success() && stdout.trim() == "45";
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

/// What a bump adds, reached through the native's context word: the
/// answer is right only if every tier passes the word first.
struct Step {
    by: i32,
}
static STEP: Step = Step { by: 10 };

extern "C" fn greeter_bump(step: *const Step, g: *mut c_void) -> i32 {
    let mut counts = COUNTS.lock().unwrap();
    let count = counts
        .get_or_insert_with(HashMap::new)
        .entry(g as usize)
        .or_insert(0);
    *count += unsafe { (*step).by };
    *count
}

/// `hlp_alloc_closure_ptr` and `hlp_make_var_args`, handed to the native
/// once the resolver has them.
static ALLOC_CLOSURE_PTR: AtomicUsize = AtomicUsize::new(0);
static MAKE_VAR_ARGS: AtomicUsize = AtomicUsize::new(0);
static DYN_TYPE: AtomicUsize = AtomicUsize::new(0);
static ARRAY_TYPE: AtomicUsize = AtomicUsize::new(0);
static I32_TYPE: AtomicUsize = AtomicUsize::new(0);
static ALLOC_DYNAMIC: AtomicUsize = AtomicUsize::new(0);

/// The C entry behind the adder: its bound value is the amount to add,
/// as a pointer-sized integer, and `args` the array Haxe's call was
/// packed into.
extern "C" fn adder_entry(bound: *mut c_void, args: *mut ash_core::hl_bindings::varray) -> *mut ash_core::hl_bindings::vdynamic {
    use ash_core::hl_bindings::{hl_type, vdynamic};
    type AllocDynamic = unsafe extern "C" fn(*mut hl_type) -> *mut vdynamic;
    let first = unsafe {
        *((args as *mut u8).add(std::mem::size_of::<ash_core::hl_bindings::varray>())
            as *const *const vdynamic)
    };
    let n = unsafe { (*first).v.i };
    let alloc: AllocDynamic = unsafe { std::mem::transmute(ALLOC_DYNAMIC.load(Ordering::Acquire)) };
    let out = unsafe { alloc(I32_TYPE.load(Ordering::Acquire) as *mut hl_type) };
    unsafe { (*out).v.i = n + bound as i32 };
    out
}

/// The var-args closure over `adder_entry`, adding 10.
extern "C" fn greeter_adder() -> *mut c_void {
    use ash_core::hl_bindings::{hl_type, hl_type__bindgen_ty_1, hl_type_fun, hl_type_kind_HFUN, hl_type_kind_HVOID};
    type AllocClosurePtr = unsafe extern "C" fn(*mut hl_type, *mut c_void, *mut c_void) -> *mut c_void;
    type MakeVarArgs = unsafe extern "C" fn(*mut c_void) -> *mut c_void;
    // The inner closure's full type, `(bound, Array<Dynamic>) -> Dynamic`;
    // ash derives the bound-less type from it on first use.
    let args = Box::leak(Box::new([
        DYN_TYPE.load(Ordering::Acquire) as *mut hl_type,
        ARRAY_TYPE.load(Ordering::Acquire) as *mut hl_type,
    ]));
    let mut fun: hl_type_fun = unsafe { std::mem::zeroed() };
    fun.args = args.as_mut_ptr();
    fun.ret = DYN_TYPE.load(Ordering::Acquire) as *mut hl_type;
    fun.nargs = 2;
    fun.closure_type.kind = hl_type_kind_HVOID;
    let fun = Box::leak(Box::new(fun));
    let t = Box::leak(Box::new(hl_type {
        kind: hl_type_kind_HFUN,
        __bindgen_anon_1: hl_type__bindgen_ty_1 { fun },
        vobj_proto: std::ptr::null_mut(),
        mark_bits: std::ptr::null_mut(),
    }));
    let alloc: AllocClosurePtr = unsafe { std::mem::transmute(ALLOC_CLOSURE_PTR.load(Ordering::Acquire)) };
    let make: MakeVarArgs = unsafe { std::mem::transmute(MAKE_VAR_ARGS.load(Ordering::Acquire)) };
    let inner = unsafe { alloc(t, adder_entry as *mut c_void, 10usize as *mut c_void) };
    unsafe { make(inner) }
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
                    context: std::ptr::null(),
                },
                HostMethod {
                    name: "bump".into(),
                    symbol: "greeter_bump".into(),
                    params: vec![HostType::Obj("test.Greeter".into())],
                    ret: HostType::I32,
                    func: greeter_bump as *const c_void,
                    context: &STEP as *const Step as *const c_void,
                },
                HostMethod {
                    name: "adder".into(),
                    symbol: "greeter_adder".into(),
                    params: vec![],
                    ret: HostType::Fun,
                    func: greeter_adder as *const c_void,
                    context: std::ptr::null(),
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
    for (slot, name) in [
        (&ALLOC_CLOSURE_PTR, "hlp_alloc_closure_ptr"),
        (&MAKE_VAR_ARGS, "hlp_make_var_args"),
        (&ALLOC_DYNAMIC, "hlp_alloc_dynamic"),
    ] {
        let addr = resolver.resolve_function("std", name).expect(name) as usize;
        slot.store(addr, Ordering::Release);
    }

    let mut interp = HLInterpreter::new(&bc, &resolver);
    GREETER_TYPE.store(interp.c_type_of(greeter) as usize, Ordering::Release);
    // The C types of Dynamic, Array and Int, which every program has.
    for (slot, kind) in [
        (&DYN_TYPE, ash_core::hl_bindings::hl_type_kind_HDYN),
        (&ARRAY_TYPE, ash_core::hl_bindings::hl_type_kind_HARRAY),
        (&I32_TYPE, ash_core::hl_bindings::hl_type_kind_HI32),
    ] {
        let index = bc.types.iter().position(|t| t.kind == kind).expect("a type of the kind");
        slot.store(interp.c_type_of(index) as usize, Ordering::Release);
    }
    if mode != "interp" {
        // `jit` is the CLI's `--mode jit`: every reached function compiled
        // before its first call, so the host natives are reached from
        // compiled code for certain rather than when a broker gets to it.
        let tier_mode = match mode {
            "cranelift" => TierMode::Cranelift,
            "llvm" => TierMode::Llvm,
            _ => TierMode::default(),
        };
        let cfg = TieredConfig {
            enabled: true,
            compiled_only: mode == "jit",
            jit_threshold: 1,
            tier_mode,
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
