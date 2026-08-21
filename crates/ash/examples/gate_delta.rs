//! THROWAWAY: drives `cranelift::air::gate_report` over a .hl file.
//! Delete once the measurement is recorded.
fn main() -> anyhow::Result<()> {
    let path = std::env::args()
        .nth(1)
        .expect("usage: gate_delta <file.hl>");
    let level = std::env::args()
        .nth(2)
        .and_then(|s| ash_core::air_pipeline::parse_level(&s))
        .unwrap_or(ash_core::air_pipeline::AirOptLevel::O2);
    ash_core::native_lib::init_std_library()?;
    let bc = ash_core::bytecode::BytecodeDecoder::decode(std::path::Path::new(&path))?;
    let opts = ash_core::air_pipeline::AirPassOptions::default();
    for line in ash_core::cranelift::air::gate_report(&bc, level, &opts) {
        println!("{line}");
    }
    Ok(())
}
