//! Prints the registers and opcodes of the functions in a `.hl` file whose name contains a
//! string, so two compilers' versions of one function can be read side by side.
//!
//!     cargo run -p ash_nair_hl --bin hl_ops -- program.hl fib

use ash_core::bytecode::BytecodeDecoder;
use ash_core::types::function_names;

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let [path, needle] = args.as_slice() else {
        eprintln!("usage: hl_ops program.hl name");
        std::process::exit(2);
    };
    let bc = BytecodeDecoder::decode(std::path::Path::new(path)).expect("decode the .hl");
    let names = function_names(&bc.types);
    for f in &bc.functions {
        let name = names.get(&(f.findex as u32)).cloned().unwrap_or_default();
        if !name.contains(needle.as_str()) {
            continue;
        }
        println!(
            "== {name} (findex {}, {} registers, {} ops)",
            f.findex,
            f.regs.len(),
            f.ops().len()
        );
        for (i, op) in f.ops().iter().enumerate() {
            println!("{i:4}  {op:?}");
        }
    }
}
