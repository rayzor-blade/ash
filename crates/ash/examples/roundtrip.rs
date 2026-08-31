// decode -> encode -> decode, and compare what the format actually carries.
// Debug info is deliberately not written, so it is excluded from the compare.
use ash_core::bytecode::BytecodeDecoder;
use std::path::Path;

fn main() -> anyhow::Result<()> {
    let mut bad = 0usize;
    let mut ok = 0usize;
    for arg in std::env::args().skip(1) {
        let p = Path::new(&arg);
        ash_core::native_lib::choose_std_linkage(p);
        let _ = ash_core::native_lib::init_std_library();
        let a = match BytecodeDecoder::decode(p) {
            Ok(a) => a,
            Err(e) => { println!("SKIP  {arg}: decode failed: {e}"); continue }
        };
        let bytes = match ash_core::bytecode_encode::encode(&a, 5) {
            Ok(b) => b,
            Err(e) => { println!("FAIL  {arg}: encode: {e}"); bad += 1; continue }
        };
        let tmp = std::path::PathBuf::from("/tmp/ash_roundtrip.hl");
        std::fs::write(&tmp, &bytes)?;
        let b = match BytecodeDecoder::decode(&tmp) {
            Ok(b) => b,
            Err(e) => { println!("FAIL  {arg}: re-decode: {e}"); bad += 1; continue }
        };
        let mut diffs: Vec<String> = Vec::new();
        macro_rules! cmp {
            ($f:ident) => {
                if a.$f.len() != b.$f.len() {
                    diffs.push(format!("{}: {} -> {}", stringify!($f), a.$f.len(), b.$f.len()));
                }
            };
        }
        cmp!(ints); cmp!(floats); cmp!(strings); cmp!(types);
        cmp!(globals); cmp!(natives); cmp!(functions); cmp!(constants); cmp!(bytes_pos);
        if a.entrypoint != b.entrypoint {
            diffs.push(format!("entrypoint: {} -> {}", a.entrypoint, b.entrypoint));
        }
        if a.ints != b.ints { diffs.push("ints CONTENT".into()); }
        if a.floats != b.floats { diffs.push("floats CONTENT".into()); }
        if a.strings != b.strings { diffs.push("strings CONTENT".into()); }
        let mut opdiff = 0;
        for (x, y) in a.functions.iter().zip(b.functions.iter()) {
            if x.findex != y.findex || x.regs.len() != y.regs.len()
                || format!("{:?}", x.ops) != format!("{:?}", y.ops) { opdiff += 1; }
        }
        if opdiff > 0 { diffs.push(format!("{opdiff} functions differ in ops/regs")); }
        let mut tydiff = 0;
        for (x, y) in a.types.iter().zip(b.types.iter()) {
            if format!("{:?}", x.kind) != format!("{:?}", y.kind) { tydiff += 1; }
        }
        if tydiff > 0 { diffs.push(format!("{tydiff} type kinds differ")); }
        if diffs.is_empty() {
            println!("OK    {arg}  ({} fns, {} types, {} bytes)", a.functions.len(), a.types.len(), bytes.len());
            ok += 1;
        } else {
            println!("DIFF  {arg}: {}", diffs.join("; "));
            bad += 1;
        }
    }
    println!("\nround trip: {ok} ok, {bad} bad");
    if bad > 0 { std::process::exit(1); }
    Ok(())
}
