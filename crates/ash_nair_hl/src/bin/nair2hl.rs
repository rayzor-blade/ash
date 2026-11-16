//! Lowers a program's entry function and everything it calls to a `.hl` file, which starts in the
//! function that sets the classes up.
//!
//!     cargo run -p ash_nair_hl --bin nair2hl -- program.tast out.hl Main.main

use ash_nair::lower::Module;
use ash_tast::Tast;
use ash_tast::format::{NONE, field_flag};

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let [tast_path, out, entry] = args.as_slice() else {
        eprintln!("usage: nair2hl program.tast out.hl Class.method");
        std::process::exit(2);
    };
    let bytes = std::fs::read(tast_path).expect("read the TAST file");
    let tast = Tast::parse(&bytes).expect("a valid TAST file");
    let Some((class, method)) = entry.rsplit_once('.') else {
        eprintln!("the entry is Class.method");
        std::process::exit(2);
    };
    let mut module = Module::new(tast);
    let Some(c) = module.types.class_id(class) else {
        eprintln!("no class {class}");
        std::process::exit(1);
    };
    let rec = tast.classes()[c as usize];
    let field =
        (rec.fields_first.get()..rec.fields_first.get() + rec.fields_count.get()).find(|f| {
            let fr = &tast.fields()[*f as usize];
            tast.opt_string(fr.name) == Some(method)
                && fr.flags.get() & field_flag::STATIC != 0
                && fr.func.get() != NONE
        });
    let Some(field) = field else {
        eprintln!("no static function {entry}");
        std::process::exit(1);
    };
    // the file starts in the function that sets the classes up and then calls the entry
    let classes = module.program_classes();
    let init = module
        .lower_init(&classes, Some(field))
        .unwrap_or_else(|e| {
            eprintln!("cannot lower the start-up: {e}");
            std::process::exit(1);
        });
    let mut lowered = module.lower_all(&[field]).unwrap_or_else(|e| {
        eprintln!("cannot lower: {e}");
        std::process::exit(1);
    });
    let entry_findex = init.findex;
    lowered.push(init);
    // the method tables name methods the program never calls: they are stubs in the file
    let stubs = module.stubs_for_layouts(&lowered).unwrap_or_else(|e| {
        eprintln!("cannot stub the method tables: {e}");
        std::process::exit(1);
    });
    lowered.extend(stubs);
    let bc = ash_nair_hl::assemble(&module, &lowered, entry_findex).unwrap_or_else(|e| {
        eprintln!("cannot assemble: {e}");
        std::process::exit(1);
    });
    // With `NAIR2HL_TYPES` set, the type table is listed: a class, its index, and its superclass's.
    if std::env::var_os("NAIR2HL_TYPES").is_some() {
        for (i, t) in bc.types.iter().enumerate() {
            match &t.obj {
                Some(o) => println!(
                    "{i}: kind {} {} super {:?} global {}",
                    t.kind,
                    o.name,
                    o.super_.as_ref().map(|s| s.0),
                    o.global_value
                ),
                None => println!("{i}: kind {}", t.kind),
            }
        }
    }
    let file = ash_nair_hl::encode(&bc).unwrap_or_else(|e| {
        eprintln!("cannot encode: {e}");
        std::process::exit(1);
    });
    std::fs::write(out, &file).expect("write the .hl file");
    println!(
        "{} functions, {} natives, {} bytes",
        lowered.len(),
        module.natives.len(),
        file.len()
    );
}
