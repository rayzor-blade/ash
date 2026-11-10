//! Print what a relocatable wasm object holds: `wasm_obj_dump <file.o> [name-substring]`.
fn main() -> anyhow::Result<()> {
    let mut args = std::env::args().skip(1);
    let path = args.next().expect("usage: wasm_obj_dump <file.o> [substring]");
    let needle = args.next();
    let bytes = std::fs::read(&path)?;
    let obj = ash_wasm_link::read(&path, &bytes)?;
    println!("{}", ash_wasm_link::describe(&obj));
    for (i, s) in obj.symbols.iter().enumerate() {
        if needle.as_ref().is_none_or(|n| s.name.contains(n.as_str())) {
            println!("  sym {i}: {} {:?} flags={:?}", s.name, s.target, s.flags);
        }
    }
    println!("imports:");
    for i in &obj.imports {
        println!("  {}.{} {:?}", i.module, i.name, i.kind);
    }
    Ok(())
}
