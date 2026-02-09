use crate::cfg::CFG;
use crate::opcodes::Opcode;

/// A bytecode optimization pass.
pub trait Pass {
    fn name(&self) -> &str;
    /// Run the pass, mutating ops in-place. Returns number of opcodes eliminated (replaced with Nop).
    fn run(&self, ops: &mut Vec<Opcode>, num_regs: usize, cfg: &CFG) -> usize;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptLevel {
    O0,
    O1,
}

pub struct PassManager {
    passes: Vec<Box<dyn Pass>>,
}

impl PassManager {
    pub fn new(level: OptLevel) -> Self {
        let passes: Vec<Box<dyn Pass>> = match level {
            OptLevel::O0 => vec![],
            OptLevel::O1 => vec![
                Box::new(crate::passes::NullCheckElimPass),
                Box::new(crate::passes::CopyPropPass),
                Box::new(crate::passes::DeadRegElimPass),
            ],
        };
        PassManager { passes }
    }

    /// Run all passes on the function's opcodes. Returns total opcodes eliminated.
    pub fn run(&self, ops: &mut Vec<Opcode>, num_regs: usize) -> usize {
        if self.passes.is_empty() {
            return 0;
        }
        let cfg = CFG::build(ops);
        let mut total = 0;
        for pass in &self.passes {
            let eliminated = pass.run(ops, num_regs, &cfg);
            total += eliminated;
        }
        total
    }
}
