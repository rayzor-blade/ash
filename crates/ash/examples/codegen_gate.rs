//! THROWAWAY: what the AIR codegen accepts that the opcode lowerer refuses.
//!
//! Per function, over one `.hl` file: the opcode gate's verdict on the
//! bytecode against `cranelift::codegen::reject_reason`'s verdict on the
//! optimized AIR. Delete once the measurement is recorded.
fn main() -> anyhow::Result<()> {
    let path = std::env::args()
        .nth(1)
        .expect("usage: codegen_gate <file.hl>");
    ash::native_lib::init_std_library()?;
    let bc = ash::bytecode::BytecodeDecoder::decode(std::path::Path::new(&path))?;
    let m = ash::air_pipeline::AshModule::new(&bc);

    let (mut both, mut neither, mut widened, mut narrowed, mut failed) = (0, 0, 0, 0, 0);
    let mut opcode_reasons: Vec<(String, usize)> = Vec::new();
    let mut air_reasons: Vec<(String, usize)> = Vec::new();
    let mut moved: Vec<String> = Vec::new();

    let tally = |acc: &mut Vec<(String, usize)>, r: &str| match acc.iter_mut().find(|(k, _)| k == r)
    {
        Some((_, n)) => *n += 1,
        None => acc.push((r.to_string(), 1)),
    };

    for f in &bc.functions {
        let by_opcode = ash::cranelift::lower::lowering_reject_reason(&bc, f);
        if let Some(r) = &by_opcode {
            tally(&mut opcode_reasons, r);
        }
        let opt = match ash::air_pipeline::optimized(&m, f) {
            Ok(o) => o,
            Err(_) => {
                failed += 1;
                continue;
            }
        };
        let by_air = ash::cranelift::lower::signature_reject_reason(&bc, f)
            .or_else(|| ash::cranelift::codegen::reject_reason(&opt.ir));
        if let Some(r) = &by_air {
            tally(&mut air_reasons, r);
        }
        match (&by_opcode, &by_air) {
            (None, None) => both += 1,
            (Some(_), Some(_)) => neither += 1,
            (Some(b), None) => {
                widened += 1;
                if moved.len() < 60 {
                    moved.push(format!(
                        "  WIDENED  findex={:<6} {:<40} opcode gate said: {b}",
                        f.findex,
                        f.name()
                    ));
                }
            }
            (None, Some(a)) => {
                narrowed += 1;
                if moved.len() < 60 {
                    moved.push(format!(
                        "  NARROWED findex={:<6} {:<40} air codegen says: {a}",
                        f.findex,
                        f.name()
                    ));
                }
            }
        }
    }

    let top = |mut v: Vec<(String, usize)>| -> Vec<String> {
        v.sort_by(|a, b| b.1.cmp(&a.1).then(a.0.cmp(&b.0)));
        v.into_iter()
            .take(14)
            .map(|(r, n)| format!("  [{n:>6}x] {r}"))
            .collect()
    };

    println!("{} functions in {path}", bc.functions.len());
    println!(
        "accepted by both={both} refused by both={neither} widened={widened} narrowed={narrowed} pipeline-declined={failed}"
    );
    println!("opcode gate refusals:");
    for l in top(opcode_reasons) {
        println!("{l}");
    }
    println!("air codegen refusals:");
    for l in top(air_reasons) {
        println!("{l}");
    }
    for l in moved {
        println!("{l}");
    }
    Ok(())
}
