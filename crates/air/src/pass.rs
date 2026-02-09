use crate::cfg::CFG;
use crate::dominance::DominatorTree;
use crate::opcodes::Opcode;
use crate::ssa::SSAForm;

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
    O2,
}

pub struct PassManager {
    level: OptLevel,
}

impl PassManager {
    pub fn new(level: OptLevel) -> Self {
        PassManager { level }
    }

    /// Run all passes on the function's opcodes. Returns total opcodes eliminated.
    pub fn run(&self, ops: &mut Vec<Opcode>, num_regs: usize) -> usize {
        match self.level {
            OptLevel::O0 => 0,
            OptLevel::O1 => self.run_o1(ops, num_regs),
            OptLevel::O2 => self.run_o2(ops, num_regs),
        }
    }

    fn run_o1(&self, ops: &mut Vec<Opcode>, num_regs: usize) -> usize {
        let cfg = CFG::build(ops);
        let mut total = 0;
        let passes: Vec<Box<dyn Pass>> = vec![
            Box::new(crate::passes::NullCheckElimPass),
            Box::new(crate::passes::CopyPropPass),
            Box::new(crate::passes::DeadRegElimPass),
        ];
        for pass in &passes {
            total += pass.run(ops, num_regs, &cfg);
        }
        total
    }

    fn run_o2(&self, ops: &mut Vec<Opcode>, num_regs: usize) -> usize {
        let mut total = 0;

        // Phase 1: Pre-SSA passes
        {
            let cfg = CFG::build(ops);
            let pre_ssa: Vec<Box<dyn Pass>> = vec![
                Box::new(crate::passes::NullCheckElimPass),
            ];
            for pass in &pre_ssa {
                total += pass.run(ops, num_regs, &cfg);
            }
        }

        // Phase 2: SSA construction + SSA passes + destruction
        {
            let cfg = CFG::build(ops);
            let dom = DominatorTree::build(&cfg);
            let mut ssa = SSAForm::construct(ops, num_regs, &cfg, &dom);

            let ssa_copy = crate::passes::SSACopyPropPass;
            total += ssa_copy.run_ssa(ops, &mut ssa, &cfg, &dom);

            let ssa_dead = crate::passes::SSADeadElimPass;
            total += ssa_dead.run_ssa(ops, &mut ssa, &cfg, &dom);

            ssa.destroy(ops);
        }

        // Phase 3: Post-SSA cleanup
        {
            let cfg = CFG::build(ops);
            let post_ssa: Vec<Box<dyn Pass>> = vec![
                Box::new(crate::passes::DeadRegElimPass),
            ];
            for pass in &post_ssa {
                total += pass.run(ops, num_regs, &cfg);
            }
        }

        total
    }
}
