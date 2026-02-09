use crate::cfg::CFG;
use crate::dominance::DominatorTree;
use crate::opcodes::Opcode;
use crate::opcode_info;
use crate::ssa::SSAForm;

pub struct SSADeadElimPass;

impl SSADeadElimPass {
    pub fn name(&self) -> &str { "ssa_dead_elim" }

    pub fn run_ssa(
        &self,
        ops: &mut Vec<Opcode>,
        ssa: &mut SSAForm,
        _cfg: &CFG,
        _dom: &DominatorTree,
    ) -> usize {
        // Count uses of each SSA register
        let mut use_count: Vec<u32> = vec![0; ssa.num_ssa_regs];

        for op in ops.iter() {
            if matches!(op, Opcode::Nop) { continue; }
            for r in opcode_info::reads(op) {
                if (r.0 as usize) < use_count.len() {
                    use_count[r.0 as usize] += 1;
                }
            }
        }

        // Count phi source uses
        for block_phis in &ssa.phis {
            for phi in block_phis {
                for &(_, src) in &phi.sources {
                    if (src.0 as usize) < use_count.len() {
                        use_count[src.0 as usize] += 1;
                    }
                }
            }
        }

        // Build def-site map: ssa_reg → opcode index (for cascading elimination)
        let mut def_site: Vec<Option<usize>> = vec![None; ssa.num_ssa_regs];
        for (i, op) in ops.iter().enumerate() {
            if matches!(op, Opcode::Nop) { continue; }
            for w in opcode_info::writes(op) {
                if (w.0 as usize) < def_site.len() {
                    def_site[w.0 as usize] = Some(i);
                }
            }
        }

        // Worklist: all pure defs with zero uses
        let mut worklist: Vec<usize> = Vec::new();
        for (i, op) in ops.iter().enumerate() {
            if opcode_info::is_pure(op) {
                let written = opcode_info::writes(op);
                if written.len() == 1 && use_count[written[0].0 as usize] == 0 {
                    worklist.push(i);
                }
            }
        }

        let mut eliminated = 0;
        while let Some(i) = worklist.pop() {
            if matches!(ops[i], Opcode::Nop) { continue; }

            // Decrement use counts for read operands before eliminating
            let reads = opcode_info::reads(&ops[i]);
            ops[i] = Opcode::Nop;
            eliminated += 1;

            for r in reads {
                if (r.0 as usize) < use_count.len() {
                    use_count[r.0 as usize] = use_count[r.0 as usize].saturating_sub(1);
                    // If the operand's def is now dead too, add it to worklist
                    if use_count[r.0 as usize] == 0 {
                        if let Some(def_idx) = def_site[r.0 as usize] {
                            if opcode_info::is_pure(&ops[def_idx]) {
                                worklist.push(def_idx);
                            }
                        }
                    }
                }
            }
        }

        // Eliminate dead phi nodes
        for block_phis in &mut ssa.phis {
            block_phis.retain(|phi| {
                if (phi.dst.0 as usize) < use_count.len() {
                    use_count[phi.dst.0 as usize] > 0
                } else {
                    true
                }
            });
        }

        eliminated
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cfg::CFG;
    use crate::dominance::DominatorTree;
    use crate::opcodes::*;
    use crate::ssa::SSAForm;

    #[test]
    fn test_dead_code_eliminated() {
        // r0 = Int(0)
        // r0 = Int(1)  ← overwrites, first is dead
        // Ret(r0)
        let mut ops = vec![
            Opcode::Int { dst: Reg(0), ptr: RefInt(0) },
            Opcode::Int { dst: Reg(0), ptr: RefInt(1) },
            Opcode::Ret { ret: Reg(0) },
        ];
        let cfg = CFG::build(&ops);
        let dom = DominatorTree::build(&cfg);
        let mut ssa = SSAForm::construct(&mut ops, 1, &cfg, &dom);

        let pass = SSADeadElimPass;
        let elim = pass.run_ssa(&mut ops, &mut ssa, &cfg, &dom);

        // First Int should be eliminated (dead in SSA)
        assert!(elim >= 1);
    }

    #[test]
    fn test_cascading_dead_code() {
        // r0 = Int(0)
        // r1 = Add(r0, r0)  ← dead (r1 never used)
        // r2 = Int(1)
        // Ret(r2)
        let mut ops = vec![
            Opcode::Int { dst: Reg(0), ptr: RefInt(0) },
            Opcode::Add { dst: Reg(1), a: Reg(0), b: Reg(0) },
            Opcode::Int { dst: Reg(2), ptr: RefInt(1) },
            Opcode::Ret { ret: Reg(2) },
        ];
        let cfg = CFG::build(&ops);
        let dom = DominatorTree::build(&cfg);
        let mut ssa = SSAForm::construct(&mut ops, 3, &cfg, &dom);

        let pass = SSADeadElimPass;
        let elim = pass.run_ssa(&mut ops, &mut ssa, &cfg, &dom);

        // Both Add and first Int should be eliminated (cascade)
        assert!(elim >= 2, "expected at least 2 eliminations, got {}", elim);
    }

    #[test]
    fn test_call_not_eliminated() {
        // r0 = Call0(fun)  ← side effect, NOT eliminated even if unused
        // r1 = Int(0)
        // Ret(r1)
        let mut ops = vec![
            Opcode::Call0 { dst: Reg(0), fun: RefFun(0) },
            Opcode::Int { dst: Reg(1), ptr: RefInt(0) },
            Opcode::Ret { ret: Reg(1) },
        ];
        let cfg = CFG::build(&ops);
        let dom = DominatorTree::build(&cfg);
        let mut ssa = SSAForm::construct(&mut ops, 2, &cfg, &dom);

        let pass = SSADeadElimPass;
        let elim = pass.run_ssa(&mut ops, &mut ssa, &cfg, &dom);

        // Call0 should NOT be eliminated (not pure)
        assert_eq!(elim, 0);
    }
}
