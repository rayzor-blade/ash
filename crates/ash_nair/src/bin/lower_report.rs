//! Lowers each function body of a TAST file on its own and counts why the ones that fail do.
//!
//!     cargo run -p ash_nair --bin lower_report -- program.tast [Class ...]
//!
//! With class names, only those classes' functions are tried; without, every class is. With
//! `LOWER_REPORT_OK` set, the functions that do lower are listed too. A body
//! stops at the first thing it uses that is not lowered, so the counts say which construct to
//! lower next, not how many each body needs.

use std::collections::BTreeMap;

use ash_nair::lower::Module;
use ash_tast::Tast;
use ash_tast::format::NONE;

fn main() {
    let mut args = std::env::args().skip(1);
    let Some(path) = args.next() else {
        eprintln!("usage: lower_report program.tast [Class ...]");
        std::process::exit(2);
    };
    let only: Vec<String> = args.collect();
    let bytes = std::fs::read(path).expect("read the TAST file");
    let tast = Tast::parse(&bytes).expect("a valid TAST file");

    let mut ok = 0;
    let mut refused: BTreeMap<String, Vec<String>> = BTreeMap::new();
    for (ci, class) in tast.classes().iter().enumerate() {
        let probe = Module::new(tast);
        let cname = probe.types.class_name(ci as u32).to_owned();
        if !only.is_empty() && !only.contains(&cname) {
            continue;
        }
        for f in class.fields_first.get()..class.fields_first.get() + class.fields_count.get() {
            if tast.fields()[f as usize].func.get() == NONE {
                continue;
            }
            let mut module = Module::new(tast);
            let name = format!("{cname}.{}", module.types.field_name(f));
            match module.lower_field(f) {
                Ok(_) => {
                    ok += 1;
                    if std::env::var_os("LOWER_REPORT_OK").is_some() {
                        println!("ok  {name}");
                    }
                }
                Err(why) => refused.entry(why).or_default().push(name),
            }
        }
    }
    let total: usize = ok + refused.values().map(Vec::len).sum::<usize>();
    println!("{ok} of {total} bodies lower on their own\n");
    let mut rows: Vec<_> = refused.into_iter().collect();
    rows.sort_by(|a, b| b.1.len().cmp(&a.1.len()).then_with(|| a.0.cmp(&b.0)));
    for (why, names) in rows {
        let shown: Vec<_> = names.iter().take(3).cloned().collect();
        println!("{:5}  {why}   e.g. {}", names.len(), shown.join(", "));
    }
}
