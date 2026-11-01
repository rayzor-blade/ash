// Rewrites the body of a two-argument Int function that returns a + b into HashLink `$asm` ops:
//   asm_fixture <in.hl> <out.hl> add|sub|imul
// eax = a; ecx = b; <op> eax, ecx; r = eax.
use ash_core::bytecode::BytecodeDecoder;
use ash_core::opcodes::{Opcode, Reg};
use std::path::Path;

fn main() -> anyhow::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    let (input, output, op) = (&args[1], &args[2], args[3].as_str());
    let p = Path::new(input);
    ash_core::native_lib::choose_std_linkage(p);
    let _ = ash_core::native_lib::init_std_library();
    let mut bc = BytecodeDecoder::decode(p)?;
    let bytes: &[u8] = match op {
        "add" => &[0x01, 0xC8],
        "sub" => &[0x29, 0xC8],
        "imul" => &[0x0F, 0xAF, 0xC1],
        other => anyhow::bail!("unknown op {other}"),
    };
    let mut done = false;
    for f in bc.functions.iter_mut() {
        let ops = f.ops();
        if f.regs.len() == 3
            && ops.len() == 2
            && matches!(ops[0], Opcode::Add { .. })
            && matches!(ops[1], Opcode::Ret { .. })
        {
            // Registers are 0 (a), 1 (b), 2 (result); the register operand is stored plus one.
            let mut body = vec![
                Opcode::Asm {
                    mode: 2,
                    value: 0,
                    reg: Reg(1),
                },
                Opcode::Asm {
                    mode: 2,
                    value: 1,
                    reg: Reg(2),
                },
            ];
            for b in bytes {
                body.push(Opcode::Asm {
                    mode: 0,
                    value: *b as i32,
                    reg: Reg(0),
                });
            }
            body.push(Opcode::Asm {
                mode: 3,
                value: 0,
                reg: Reg(3),
            });
            body.push(Opcode::Ret { ret: Reg(2) });
            f.set_ops(body);
            done = true;
            break;
        }
    }
    anyhow::ensure!(done, "no function shaped like add(a, b) found");
    std::fs::write(output, ash_core::bytecode_encode::encode(&bc, 5)?)?;
    Ok(())
}
