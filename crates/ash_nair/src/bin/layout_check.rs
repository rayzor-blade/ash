//! Compares the layouts computed from a TAST file with the ones a `.hl` file carries.
//!
//!     cargo run -p ash_nair --bin layout_check -- program.hl program.tast

use ash_nair::build::TypeBuilder;
use ash_nair::compare::{differences, program_names};
use ash_nair::hlfile::read_types;
use ash_tast::Tast;

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let [hl, tast] = args.as_slice() else {
        eprintln!("usage: layout_check program.hl program.tast");
        std::process::exit(2);
    };
    let hl = std::fs::read(hl).expect("read the .hl");
    let tast = std::fs::read(tast).expect("read the TAST file");
    let theirs = read_types(&hl).expect("a readable .hl");
    let file = Tast::parse(&tast).expect("a valid TAST file");
    let mut builder = TypeBuilder::new(file);
    if let Err(e) = builder.build_program() {
        eprintln!("the program does not lay out: {e}");
        std::process::exit(1);
    }
    let (compared, lazy, problems) = differences(&builder.table, &theirs, &program_names(&file));
    println!(
        "{compared} classes and enums compared, {} differ; {lazy} more are created only while compiling bodies",
        problems.len()
    );
    for p in problems.iter().take(40) {
        println!("{p}");
    }
    if !problems.is_empty() {
        std::process::exit(1);
    }
}
