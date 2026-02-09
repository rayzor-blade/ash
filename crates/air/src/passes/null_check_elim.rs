use crate::cfg::CFG;
use crate::opcodes::Opcode;
use crate::opcode_info;
use crate::pass::Pass;
use std::collections::HashSet;

pub struct NullCheckElimPass;

impl Pass for NullCheckElimPass {
    fn name(&self) -> &str { "null_check_elim" }

    fn run(&self, ops: &mut Vec<Opcode>, _num_regs: usize, cfg: &CFG) -> usize {
        let mut eliminated = 0;

        // Per-block forward dataflow: track registers known to be non-null.
        // At block entry: intersect exit states of all predecessors.
        // This is a simple fixed-point iteration.

        let num_blocks = cfg.blocks.len();
        if num_blocks == 0 { return 0; }

        // State: set of register indices known non-null at block exit
        let mut block_exit_nonnull: Vec<HashSet<u32>> = vec![HashSet::new(); num_blocks];
        let mut changed = true;

        // Fixed-point iteration
        while changed {
            changed = false;

            for block_idx in 0..num_blocks {
                let block = &cfg.blocks[block_idx];

                // Compute entry state: intersection of all predecessor exit states
                let mut nonnull: HashSet<u32> = if block.predecessors.is_empty() {
                    HashSet::new()
                } else {
                    let mut iter = block.predecessors.iter();
                    let first = *iter.next().unwrap();
                    let mut set = block_exit_nonnull[first].clone();
                    for &pred in iter {
                        set = set.intersection(&block_exit_nonnull[pred]).copied().collect();
                    }
                    set
                };

                // Forward walk through block
                for i in block.start..=block.end {
                    let op = &ops[i];

                    // Any write to a register invalidates its non-null status
                    for w in opcode_info::writes(op) {
                        nonnull.remove(&w.0);
                    }

                    // Opcodes that produce known non-null results
                    match op {
                        Opcode::New { dst } | Opcode::EnumAlloc { dst, .. }
                        | Opcode::MakeEnum { dst, .. }
                        | Opcode::StaticClosure { dst, .. }
                        | Opcode::InstanceClosure { dst, .. }
                        | Opcode::VirtualClosure { dst, .. }
                        | Opcode::String { dst, .. } => {
                            nonnull.insert(dst.0);
                        }
                        Opcode::NullCheck { reg } => {
                            // If already known non-null → this check is redundant
                            if nonnull.contains(&reg.0) {
                                // Will be eliminated below
                            } else {
                                // After a successful NullCheck, the register is non-null
                                nonnull.insert(reg.0);
                            }
                        }
                        _ => {}
                    }
                }

                // Check if exit state changed
                if nonnull != block_exit_nonnull[block_idx] {
                    block_exit_nonnull[block_idx] = nonnull;
                    changed = true;
                }
            }
        }

        // Second pass: actually eliminate redundant NullChecks
        for block_idx in 0..num_blocks {
            let block = &cfg.blocks[block_idx];

            let mut nonnull: HashSet<u32> = if block.predecessors.is_empty() {
                HashSet::new()
            } else {
                let mut iter = block.predecessors.iter();
                let first = *iter.next().unwrap();
                let mut set = block_exit_nonnull[first].clone();
                for &pred in iter {
                    set = set.intersection(&block_exit_nonnull[pred]).copied().collect();
                }
                set
            };

            for i in block.start..=block.end {
                let op = &ops[i];

                // Check for eliminable NullCheck before processing writes
                if let Opcode::NullCheck { reg } = op {
                    if nonnull.contains(&reg.0) {
                        ops[i] = Opcode::Nop;
                        eliminated += 1;
                        continue;
                    }
                }

                // Update state
                for w in opcode_info::writes(op) {
                    nonnull.remove(&w.0);
                }

                match &ops[i] {
                    Opcode::New { dst } | Opcode::EnumAlloc { dst, .. }
                    | Opcode::MakeEnum { dst, .. }
                    | Opcode::StaticClosure { dst, .. }
                    | Opcode::InstanceClosure { dst, .. }
                    | Opcode::VirtualClosure { dst, .. }
                    | Opcode::String { dst, .. } => {
                        nonnull.insert(dst.0);
                    }
                    Opcode::NullCheck { reg } => {
                        nonnull.insert(reg.0);
                    }
                    _ => {}
                }
            }
        }

        eliminated
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::opcodes::*;

    #[test]
    fn test_redundant_nullcheck_eliminated() {
        let mut ops = vec![
            Opcode::NullCheck { reg: Reg(0) },
            Opcode::Field { dst: Reg(1), obj: Reg(0), field: RefField(0) },
            Opcode::NullCheck { reg: Reg(0) },  // redundant
            Opcode::Ret { ret: Reg(1) },
        ];
        let cfg = CFG::build(&ops);
        let pass = NullCheckElimPass;
        let elim = pass.run(&mut ops, 2, &cfg);
        assert_eq!(elim, 1);
        assert!(matches!(ops[2], Opcode::Nop));
        assert!(matches!(ops[0], Opcode::NullCheck { .. }));
    }

    #[test]
    fn test_nullcheck_after_new_eliminated() {
        let mut ops = vec![
            Opcode::New { dst: Reg(0) },
            Opcode::NullCheck { reg: Reg(0) },  // redundant: New always non-null
            Opcode::Ret { ret: Reg(0) },
        ];
        let cfg = CFG::build(&ops);
        let pass = NullCheckElimPass;
        let elim = pass.run(&mut ops, 1, &cfg);
        assert_eq!(elim, 1);
        assert!(matches!(ops[1], Opcode::Nop));
    }

    #[test]
    fn test_nullcheck_after_overwrite_kept() {
        let mut ops = vec![
            Opcode::NullCheck { reg: Reg(0) },
            Opcode::Mov { dst: Reg(0), src: Reg(1) },  // overwrites r0
            Opcode::NullCheck { reg: Reg(0) },  // NOT redundant: r0 was overwritten
            Opcode::Ret { ret: Reg(0) },
        ];
        let cfg = CFG::build(&ops);
        let pass = NullCheckElimPass;
        let elim = pass.run(&mut ops, 2, &cfg);
        assert_eq!(elim, 0);
    }
}
