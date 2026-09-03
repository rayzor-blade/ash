//! What target the middle end actually optimises for.
//!
//! The middle end reads the triple back off the module and asks TargetAbi for
//! a machine. TargetAbi decides "is this the host" by comparing triple
//! STRINGS, and answers `generic` with no features when they differ -- which
//! on a machine whose vector width is not baseline is most of the
//! optimisation.
fn main() -> anyhow::Result<()> {
    use inkwell::targets::TargetMachine;
    inkwell::targets::Target::initialize_all(&inkwell::targets::InitializationConfig::default());

    let context = inkwell::context::Context::create();
    let module = context.create_module("probe");

    let abi = ash_core::target_abi::TargetAbi::host()?;
    abi.apply_to_module(&module)?;

    let default = TargetMachine::get_default_triple();
    let on_module = module.get_triple();
    println!("default triple : {:?}", default.as_str());
    println!("module triple  : {:?}", on_module.as_str());
    println!("equal          : {}", on_module == default);
    println!(
        "host cpu       : {:?}",
        TargetMachine::get_host_cpu_name().to_str()
    );
    println!(
        "host features  : {:.90}...",
        TargetMachine::get_host_cpu_features().to_string_lossy()
    );

    // The machine the middle end would actually build, by the same route.
    let triple = on_module.as_str().to_string_lossy().into_owned();
    let abi_from_module = ash_core::target_abi::TargetAbi::for_triple(&triple)?;
    let (tt, machine) = abi_from_module.target_machine(inkwell::OptimizationLevel::Aggressive)?;
    println!("machine triple : {:?}", tt.as_str());
    println!("machine cpu    : {:?}", machine.get_cpu().to_str());
    println!(
        "machine feats  : {:.90}...",
        machine.get_feature_string().to_string_lossy()
    );
    Ok(())
}
