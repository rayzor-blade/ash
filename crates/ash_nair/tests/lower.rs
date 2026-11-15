//! Function bodies lowered to AIR verify, and compute what the Haxe source means.

mod common;

use std::collections::HashMap;

use air::v2::passes::OptLevel;
use air::v2::verify::verify;
use ash_nair::eval::{Program, Val, run};
use ash_nair::info::{NairModule, describe_mix, instruction_count, optimize};
use ash_nair::lower::{Lowered, Module};
use ash_tast::Tast;

fn lowered(module: &mut Module, class: &str) -> (Vec<Lowered>, Vec<(String, String)>) {
    let tast = module.types.tast();
    let c = module.types.class_id(class).expect("the class");
    let rec = tast.classes()[c as usize];
    let (mut done, mut refused) = (Vec::new(), Vec::new());
    for f in rec.fields_first.get()..rec.fields_first.get() + rec.fields_count.get() {
        let field = tast.fields()[f as usize];
        let name = tast.opt_string(field.name).unwrap_or("?").to_owned();
        if field.func.get() == ash_tast::format::NONE || name == "main" {
            continue;
        }
        match module.lower_field(f) {
            Ok(l) => done.push(l),
            Err(e) => refused.push((name, e)),
        }
    }
    (done, refused)
}

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

#[test]
fn numeric_kernels_lower_verify_and_compute_what_the_source_means() {
    let Some(compiler) = common::native_haxe() else {
        eprintln!("skipped: set ASH_HAXE_NATIVE to a haxe with the ash_tast plugin built");
        return;
    };
    let bytes = common::export(&compiler, "tests/lower", "lower");
    let tast = Tast::parse(&bytes).expect("a valid TAST file");
    let mut module = Module::new(tast);
    let (done, refused) = lowered(&mut module, "Main");
    assert!(refused.is_empty(), "refused: {refused:?}");
    assert!(done.len() >= 13, "lowered only {}", done.len());
    if std::env::var("ASH_NAIR_DUMP").is_ok() {
        for l in &done {
            eprintln!("== {}\n{}", l.name, l.function.dump());
        }
    }
    for l in &done {
        verify(&l.function)
            .unwrap_or_else(|e| panic!("{} does not verify: {e}\n{}", l.name, l.function.dump()));
    }

    let functions: HashMap<usize, _> = done
        .iter()
        .map(|l| (l.findex as usize, &l.function))
        .collect();
    let program = Program::new(functions, &module.ints, &module.floats, &module.types.table);
    let find = |name: &str| {
        done.iter()
            .find(|l| l.name == format!("Main.{name}"))
            .unwrap_or_else(|| panic!("{name} was lowered"))
            .findex as usize
    };
    let call = |name: &str, args: &[Val]| {
        run(&program, find(name), args).unwrap_or_else(|e| panic!("{name}{args:?}: {e}"))
    };
    check_kernels(&call);

    // the same kernels after the AIR passes: nothing but nAIR and the air crate is involved
    let info = NairModule::new(&module, &done);
    // numeric kernels that call each other write no memory
    for l in &done {
        let s = info
            .summary(l.findex as usize)
            .unwrap_or_else(|| panic!("{} has a summary", l.name));
        assert!(s.writes.is_none(), "{} writes {:?}", l.name, s.writes);
    }
    let mut optimized = Vec::new();
    for l in &done {
        let mut f = l.function.clone().with_findex(l.findex as usize);
        let before = instruction_count(&f);
        optimize(&info, &mut f, OptLevel::O3, false)
            .unwrap_or_else(|e| panic!("{}: the passes failed: {e}", l.name));
        verify(&f).unwrap_or_else(|e| {
            panic!(
                "{} does not verify after the passes: {e}\n{}",
                l.name,
                f.dump()
            )
        });
        if std::env::var("ASH_NAIR_DUMP").is_ok() {
            eprintln!(
                "== {} after O3 ({before} -> {} instructions): {}\n{}",
                l.name,
                instruction_count(&f),
                describe_mix(&f),
                f.dump()
            );
        }
        optimized.push((l.findex as usize, f));
    }
    let program = Program::new(
        optimized.iter().map(|(i, f)| (*i, f)).collect(),
        &module.ints,
        &module.floats,
        &module.types.table,
    );
    let call = |name: &str, args: &[Val]| {
        run(&program, find(name), args).unwrap_or_else(|e| panic!("optimized {name}{args:?}: {e}"))
    };
    check_kernels(&call);
}

/// What every kernel must compute, through `call`.
fn check_kernels(call: &dyn Fn(&str, &[Val]) -> Val) {
    let i = Val::I;
    let f = Val::F;

    for n in [0, 1, 2, 5, 10, 15] {
        assert_eq!(call("fib", &[i(n)]), i(fib(n)), "fib({n})");
    }
    for n in [0, 1, 10, 100] {
        assert_eq!(call("sum", &[i(n)]), i((0..n).sum()), "sum({n})");
    }
    for (a, b) in [(12, 18), (17, 5), (0, 9), (100, 75)] {
        assert_eq!(call("gcd", &[i(a), i(b)]), i(gcd(a, b)), "gcd({a}, {b})");
    }
    for n in [1, 6, 7, 27] {
        assert_eq!(call("collatz", &[i(n)]), i(collatz(n)), "collatz({n})");
    }
    for (x, n) in [(1.0, 0), (1.0, 3), (-2.5, 6)] {
        assert_eq!(call("mix", &[f(x), i(n)]), f(mix(x, n)), "mix({x}, {n})");
    }
    for n in [-3, 0, 1, 2, 9, 97, 100] {
        assert_eq!(
            call("isPrime", &[i(n)]),
            Val::B(is_prime(n)),
            "isPrime({n})"
        );
    }
    for n in [-7, 0, 5] {
        assert_eq!(call("abs", &[i(n)]), i(n.abs()), "abs({n})");
    }
    for x in [-1.0, 0.5, 3.0] {
        assert_eq!(
            call("clamp", &[f(x), f(0.0), f(1.0)]),
            f(x.clamp(0.0, 1.0)),
            "clamp({x})"
        );
    }
    for (a, b) in [(1, 2), (1, -2), (-1, 2), (-20, 5)] {
        let expect = (a > 0 && b > 0) || a < -10;
        assert_eq!(
            call("both", &[i(a), i(b)]),
            Val::B(expect),
            "both({a}, {b})"
        );
    }
    for n in [1, 5, 10] {
        assert_eq!(call("sumDo", &[i(n)]), i((0..n.max(1)).sum()), "sumDo({n})");
    }
    for n in [0, 1, 50, 99] {
        let expect = (0..).find(|k| k * k > n).unwrap();
        assert_eq!(call("firstBig", &[i(n)]), i(expect), "firstBig({n})");
    }
    for n in [0, 1, 9, 10] {
        let expect: i32 = (1..=n).filter(|k| k % 2 != 0).sum();
        assert_eq!(call("skipEven", &[i(n)]), i(expect), "skipEven({n})");
    }
    for n in [0, 1, 6, 12] {
        let mut c = 0;
        for a in 0..n {
            for b in 0..a {
                if (a + b) % 3 == 0 {
                    c += 1;
                }
            }
        }
        assert_eq!(call("nested", &[i(n)]), i(c), "nested({n})");
    }
}
