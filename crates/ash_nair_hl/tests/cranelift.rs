//! What nAIR lowers is compiled by Cranelift and run as machine code, with no bytecode anywhere:
//! the functions arrive as AIR, the module answers what the code generator asks of it, and the
//! compiled kernels give the answers the source means, before and after the AIR passes.
//!
//! Needs the Haxe with the `ash_tast` plugin (`ASH_HAXE_NATIVE`). Skipped without it.

mod common;

use std::ffi::c_void;
use std::sync::Arc;

use air::v2::ir::Function;
use air::v2::passes::OptLevel;
use ash_core::cranelift::{AshCraneliftBackend, CraneliftTierContext};
use ash_nair::eval::Val;
use ash_nair::info::{NairModule, optimize};
use ash_nair::lower::Module;
use ash_tast::Tast;

fn fib(n: i32) -> i32 {
    if n < 2 { n } else { fib(n - 1) + fib(n - 2) }
}

fn gcd(mut a: i32, mut b: i32) -> i32 {
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

fn collatz(mut n: i32) -> i32 {
    let mut steps = 0;
    while n != 1 {
        n = if n % 2 == 0 { n >> 1 } else { n * 3 + 1 };
        steps += 1;
    }
    steps
}

fn mix(x: f64, n: i32) -> f64 {
    let mut r = x;
    for i in 0..n {
        r = r * 1.5 + f64::from(i);
    }
    r
}

fn is_prime(n: i32) -> bool {
    if n < 2 {
        return false;
    }
    let mut i = 2;
    while i * i <= n {
        if n % i == 0 {
            return false;
        }
        i += 1;
    }
    true
}

/// Calls compiled code at `p` as the kernel `name` takes its arguments, which the signature of
/// each kernel fixes.
fn call(p: usize, name: &str, args: &[Val]) -> Val {
    use Val::{B, F, I};
    // SAFETY: `p` is the entry of a function Cranelift compiled for exactly this signature.
    unsafe {
        match (name, args) {
            ("fib" | "sum" | "collatz" | "abs" | "firstBig" | "skipEven" | "nested", [I(a)]) => {
                I(std::mem::transmute::<usize, extern "C" fn(i32) -> i32>(p)(*a))
            }
            ("gcd", [I(a), I(b)]) => {
                I(std::mem::transmute::<usize, extern "C" fn(i32, i32) -> i32>(p)(*a, *b))
            }
            ("mix", [F(x), I(n)]) => {
                F(std::mem::transmute::<usize, extern "C" fn(f64, i32) -> f64>(p)(*x, *n))
            }
            ("isPrime", [I(a)]) => {
                B(std::mem::transmute::<usize, extern "C" fn(i32) -> u8>(p)(*a) & 1 != 0)
            }
            ("both", [I(a), I(b)]) => {
                B(std::mem::transmute::<usize, extern "C" fn(i32, i32) -> u8>(p)(*a, *b) & 1 != 0)
            }
            ("clamp", [F(x), F(lo), F(hi)]) => F(std::mem::transmute::<
                usize,
                extern "C" fn(f64, f64, f64) -> f64,
            >(p)(*x, *lo, *hi)),
            _ => panic!("no calling shape for {name}{args:?}"),
        }
    }
}

/// The compiled entry of every kernel, keeping alive what the code needs.
struct Compiled {
    entries: Vec<(usize, usize)>,
    _backend: AshCraneliftBackend,
    _ctx: CraneliftTierContext,
    _table: Box<[*mut c_void]>,
    _owners: Vec<Arc<dyn std::any::Any + Send + Sync>>,
}

fn compile(module: Arc<NairModule>, kernels: &[(usize, Function)]) -> Compiled {
    let backend = AshCraneliftBackend::new().expect("the Cranelift backend");
    let max = kernels.iter().map(|(i, _)| *i).max().unwrap();
    let mut table: Box<[*mut c_void]> = vec![std::ptr::null_mut(); max + 1].into_boxed_slice();
    // SAFETY: `table` is boxed, so it does not move, and it outlives `ctx` and the code (both
    // are held in the returned value, which drops them together).
    let ctx = unsafe {
        CraneliftTierContext::for_module(&backend, module, table.as_mut_ptr(), max)
            .expect("a context over the module")
    };
    let mut entries = Vec::new();
    let mut owners: Vec<Arc<dyn std::any::Any + Send + Sync>> = Vec::new();
    for (findex, air) in kernels {
        let (code, bead) = backend
            .compile_air_detached(&ctx, *findex, air)
            .unwrap_or_else(|e| panic!("findex {findex}: {e:#}\n{}", air.dump()));
        table[*findex] = code as *mut c_void;
        entries.push((*findex, code));
        owners.push(bead);
    }
    Compiled {
        entries,
        _backend: backend,
        _ctx: ctx,
        _table: table,
        _owners: owners,
    }
}

fn check(compiled: &Compiled, entry: &dyn Fn(&str) -> usize, what: &str) {
    let find = |name: &str| {
        let findex = entry(name);
        compiled
            .entries
            .iter()
            .find(|(i, _)| *i == findex)
            .unwrap_or_else(|| panic!("{name} was compiled"))
            .1
    };
    let (i, f) = (Val::I, Val::F);
    for n in [0, 1, 2, 5, 10, 15] {
        assert_eq!(call(find("fib"), "fib", &[i(n)]), i(fib(n)), "{what}: fib({n})");
    }
    for n in [0, 1, 10, 100] {
        assert_eq!(call(find("sum"), "sum", &[i(n)]), i((0..n).sum()), "{what}: sum({n})");
    }
    for (a, b) in [(12, 18), (17, 5), (0, 9), (100, 75)] {
        assert_eq!(call(find("gcd"), "gcd", &[i(a), i(b)]), i(gcd(a, b)), "{what}: gcd({a},{b})");
    }
    for n in [1, 6, 7, 27] {
        assert_eq!(
            call(find("collatz"), "collatz", &[i(n)]),
            i(collatz(n)),
            "{what}: collatz({n})"
        );
    }
    for (x, n) in [(1.0, 0), (1.0, 3), (-2.5, 6)] {
        assert_eq!(
            call(find("mix"), "mix", &[f(x), i(n)]),
            f(mix(x, n)),
            "{what}: mix({x},{n})"
        );
    }
    for n in [-3, 0, 1, 2, 9, 97, 100] {
        assert_eq!(
            call(find("isPrime"), "isPrime", &[i(n)]),
            Val::B(is_prime(n)),
            "{what}: isPrime({n})"
        );
    }
    for x in [-1.0, 0.5, 3.0] {
        assert_eq!(
            call(find("clamp"), "clamp", &[f(x), f(0.0), f(1.0)]),
            f(x.clamp(0.0, 1.0)),
            "{what}: clamp({x})"
        );
    }
    for (a, b) in [(1, 2), (1, -2), (-1, 2), (-20, 5)] {
        let expect = (a > 0 && b > 0) || a < -10;
        assert_eq!(
            call(find("both"), "both", &[i(a), i(b)]),
            Val::B(expect),
            "{what}: both({a},{b})"
        );
    }
}

#[test]
fn numeric_kernels_compile_to_machine_code_from_ir_alone_and_compute_what_the_source_means() {
    let Some(compiler) = common::native_haxe() else {
        eprintln!("skipped: set ASH_HAXE_NATIVE to a haxe with the ash_tast plugin built");
        return;
    };
    // the runtime the compiled code calls into (the poll at a loop header)
    ash_core::native_lib::choose_std_linkage(&std::env::temp_dir().join("nair.hl"));
    ash_core::native_lib::init_std_library().expect("the runtime starts");

    let bytes = common::export(&compiler, "tests/lower", "cranelift");
    let tast = Tast::parse(&bytes).expect("a valid TAST file");
    let mut module = Module::new(tast);
    let class = module.types.class_id("Main").expect("the class");
    let rec = tast.classes()[class as usize];
    let mut done = Vec::new();
    for f in rec.fields_first.get()..rec.fields_first.get() + rec.fields_count.get() {
        let field = tast.fields()[f as usize];
        if field.func.get() == ash_tast::format::NONE || module.types.field_name(f) == "main" {
            continue;
        }
        done.push(module.lower_field(f).expect("the kernel lowers"));
    }
    let info = Arc::new(NairModule::new(&module, &done));
    let entry = |name: &str| {
        done.iter()
            .find(|l| l.name == format!("Main.{name}"))
            .unwrap_or_else(|| panic!("{name} was lowered"))
            .findex as usize
    };

    // as lowered
    let plain: Vec<(usize, Function)> = done
        .iter()
        .map(|l| (l.findex as usize, l.function.clone().with_findex(l.findex as usize)))
        .collect();
    check(&compile(Arc::clone(&info), &plain), &entry, "as lowered");

    // after the AIR passes, with what the typed program told them
    let mut optimized = plain.clone();
    for (_, f) in &mut optimized {
        optimize(&info, f, OptLevel::O3, false).expect("the passes run");
    }
    check(&compile(Arc::clone(&info), &optimized), &entry, "after O3");
}
