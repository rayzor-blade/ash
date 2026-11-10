//! Print one function of a linked wasm module: `wasm_func_dump <module> <function index>`.
//! The index counts imports first, as every other tool does.
use wasmparser::{Parser, Payload};

fn main() -> anyhow::Result<()> {
    let mut args = std::env::args().skip(1);
    let path = args.next().expect("usage: wasm_func_dump <module> <index>");
    let want: u32 = args.next().expect("function index").parse()?;
    let bytes = std::fs::read(path)?;
    let mut imported = 0u32;
    let mut next_body = 0u32;
    for payload in Parser::new(0).parse_all(&bytes) {
        match payload? {
            Payload::ImportSection(s) => {
                for i in s.into_imports() {
                    if let wasmparser::TypeRef::Func(_) | wasmparser::TypeRef::FuncExact(_) = i?.ty {
                        imported += 1;
                    }
                }
            }
            Payload::CodeSectionEntry(body) => {
                let index = imported + next_body;
                next_body += 1;
                if index != want {
                    continue;
                }
                println!("function {index} ({} imports before it)", imported);
                let mut locals = body.get_locals_reader()?;
                for _ in 0..locals.get_count() {
                    let (n, t) = locals.read()?;
                    println!("  local x{n} {t:?}");
                }
                let mut ops = body.get_operators_reader()?;
                let mut n = 0;
                while !ops.eof() {
                    let (op, off) = ops.read_with_offset()?;
                    println!("  [{n:3}] +{off:6}: {op:?}");
                    n += 1;
                }
            }
            _ => {}
        }
    }
    Ok(())
}
