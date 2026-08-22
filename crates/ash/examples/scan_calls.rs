//! Find every function whose ops reference a given findex, and print the
//! target's declared signature and register types — the static answer to
//! "who calls this and with what", for chasing a bad argument to its source.
//!
//! Usage: scan_calls <file.hl> <findex>
fn main() -> anyhow::Result<()> {
    let mut args = std::env::args().skip(1);
    let path = args.next().expect("usage: scan_calls <file.hl> <findex>");
    let want: usize = args.next().expect("findex").parse()?;

    ash_core::native_lib::init_std_library()?;
    let bc = ash_core::bytecode::BytecodeDecoder::decode(std::path::Path::new(&path))?;

    for f in &bc.functions {
        if f.findex as usize != want {
            continue;
        }
        println!("=== target findex={want} {} ===", f.name());
        let t = &bc.types[f.type_.0];
        if let Some(fun) = t.fun.as_ref() {
            let args_s: Vec<String> = fun
                .args
                .iter()
                .map(|a| format!("t{}:{:?}", a.0, bc.types[a.0].kind))
                .collect();
            println!(
                "  fun args=[{}] ret=t{}:{:?}",
                args_s.join(", "),
                fun.ret.0,
                bc.types[fun.ret.0].kind
            );
        }
        for (i, r) in f.regs.iter().enumerate() {
            println!("  reg{i} = t{}:{:?}", r.0, bc.types[r.0].kind);
        }
    }

    for n in &bc.natives {
        if n.findex as usize == want {
            println!("=== target findex={want} is NATIVE {}@{} ===", n.lib, n.name);
        }
    }

    let needle = format!("RefFun({want})");
    for f in &bc.functions {
        for (pc, op) in f.ops.iter().enumerate() {
            let s = format!("{op:?}");
            if s.contains(&needle) {
                println!("caller findex={} {} pc={pc}: {s}", f.findex, f.name());
            }
        }
    }
    Ok(())
}
