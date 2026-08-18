//! THROWAWAY: drives `cranelift::air::gate_report` over a .hl file.
//! Delete once the measurement is recorded.
fn main() -> anyhow::Result<()> {
    let path = std::env::args()
        .nth(1)
        .expect("usage: gate_delta <file.hl>");
    let level = std::env::args()
        .nth(2)
        .and_then(|s| ash::air_pipeline::parse_level(&s))
        .unwrap_or(ash::air_pipeline::AirOptLevel::O2);
    ash::native_lib::init_std_library()?;
    let bc = ash::bytecode::BytecodeDecoder::decode(std::path::Path::new(&path))?;
    let opts = ash::air_pipeline::AirPassOptions::default();
    for line in ash::cranelift::air::gate_report(&bc, level, &opts) {
        println!("{line}");
    }
    Ok(())
}
