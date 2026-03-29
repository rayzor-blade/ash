pub(crate) mod copy_prop;
mod dead_reg_elim;
pub mod indirect_call_rewrite;
mod null_check_elim;
pub(crate) mod ssa_copy_prop;
pub(crate) mod ssa_dead_elim;

pub use copy_prop::CopyPropPass;
pub use dead_reg_elim::DeadRegElimPass;
pub use indirect_call_rewrite::IndirectCallRewritePass;
pub use null_check_elim::NullCheckElimPass;
pub use ssa_copy_prop::SSACopyPropPass;
pub use ssa_dead_elim::SSADeadElimPass;
