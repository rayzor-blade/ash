mod null_check_elim;
mod copy_prop;
mod dead_reg_elim;

pub use null_check_elim::NullCheckElimPass;
pub use copy_prop::CopyPropPass;
pub use dead_reg_elim::DeadRegElimPass;
