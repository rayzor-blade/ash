//! The collector finds where generic code is used at concrete types, including through a
//! specialized body that itself uses another generic.

mod common;

use ash_nair::collect::Collector;
use ash_tast::Tast;

#[test]
fn it_finds_generic_code_used_at_concrete_types() {
    let Some(compiler) = common::native_haxe() else {
        eprintln!("skipped: set ASH_HAXE_NATIVE to a haxe with the ash_tast plugin built");
        return;
    };
    let bytes = common::export(&compiler, "tests/native", "instances");
    let tast = Tast::parse(&bytes).expect("a valid TAST file");
    let mut c = Collector::new(tast);
    c.run();

    let instances: Vec<_> = c.found.keys().cloned().collect();
    let named: Vec<(String, bool)> = instances
        .iter()
        .map(|i| (c.describe(i), c.has_value_arg(i)))
        .collect();
    let find = |text: &str| named.iter().find(|(n, _)| n == text).map(|(_, v)| *v);

    // a method type parameter, bound to a value type and to a class
    assert_eq!(find("Main.floats<T=Float>"), Some(true));
    assert_eq!(find("Main.strings<T=String>"), Some(false));
    // a parameter constrained by an interface, bound to the class that is passed
    assert_eq!(find("Main.constrained<T=Point>"), Some(false));
    // class parameters, from the receiver and from `new`
    assert_eq!(find("Pair.new<A=Int, B=String>"), Some(true));
    assert_eq!(find("Pair.swap<A=Int, B=String>"), Some(true));
    // found only inside the specialized body of `swap`, whose `new Pair(second, first)` is at
    // `Pair<B, A>` and so `Pair<String, Int>` once the arguments are in place
    assert_eq!(find("Pair.new<A=String, B=Int>"), Some(true));
    assert_eq!(c.unresolved, 0);
}
