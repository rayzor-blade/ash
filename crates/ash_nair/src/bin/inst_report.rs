//! Lists where generic code is used at concrete types in a TAST file, and which of those uses
//! pass a value type that HashLink must box.
//!
//!     cargo run -p ash_nair --bin inst_report -- program.tast [top]

use ash_nair::collect::Collector;
use ash_tast::Tast;

fn main() {
    let mut args = std::env::args().skip(1);
    let Some(path) = args.next() else {
        eprintln!("usage: inst_report program.tast [top]");
        std::process::exit(2);
    };
    let top: usize = args.next().and_then(|n| n.parse().ok()).unwrap_or(25);
    let bytes = std::fs::read(path).expect("read the TAST file");
    let tast = Tast::parse(&bytes).expect("a valid TAST file");
    let mut c = Collector::new(tast);
    c.run();

    let mut rows: Vec<_> = c.found.iter().map(|(i, n)| (i.clone(), *n)).collect();
    rows.sort_by(|a, b| {
        b.1.cmp(&a.1)
            .then_with(|| c.describe(&a.0).cmp(&c.describe(&b.0)))
    });
    let mut with_value = 0;
    let mut sites = 0;
    let mut value_sites = 0;
    for (i, n) in &rows {
        sites += n;
        if c.has_value_arg(i) {
            with_value += 1;
            value_sites += n;
        }
    }
    println!(
        "{} specializable instances at {sites} call and new sites; {with_value} instances ({value_sites} sites) pass a value type; {} uses left unresolved",
        rows.len(),
        c.unresolved
    );
    println!(
        "{} generic classes instantiated at concrete arguments",
        c.classes.len()
    );
    println!("\nmost used:");
    for (i, n) in rows.iter().take(top) {
        let mark = if c.has_value_arg(i) { "value" } else { "     " };
        println!("{n:6}  {mark}  {}", c.describe(i));
    }
    println!("\nmost used with a value type argument:");
    let mut shown = 0;
    for (i, n) in &rows {
        if shown == top {
            break;
        }
        if c.has_value_arg(i) {
            println!("{n:6}  {}", c.describe(i));
            shown += 1;
        }
    }
}
