use crate::cfg::CFG;
use crate::opcodes::Opcode;
use crate::opcode_info;
use crate::pass::Pass;
use std::collections::HashSet;

pub struct DeadRegElimPass;

impl Pass for DeadRegElimPass {
    fn name(&self) -> &str { "dead_reg_elim" }

    fn run(&self, ops: &mut Vec<Opcode>, _num_regs: usize, cfg: &CFG) -> usize {
        let num_blocks = cfg.blocks.len();
        if num_blocks == 0 { return 0; }

        // Compute live-in sets for each block via backward fixed-point iteration.
        // live_in[block] = registers live at the entry of the block.
        let mut live_in: Vec<HashSet<u32>> = vec![HashSet::new(); num_blocks];
        let mut live_out: Vec<HashSet<u32>> = vec![HashSet::new(); num_blocks];

        let mut changed = true;
        while changed {
            changed = false;

            // Process blocks in reverse order for faster convergence
            for block_idx in (0..num_blocks).rev() {
                let block = &cfg.blocks[block_idx];

                // live_out = union of live_in of all successors
                let mut out: HashSet<u32> = HashSet::new();
                for &succ in &block.successors {
                    for &r in &live_in[succ] {
                        out.insert(r);
                    }
                }

                // Walk opcodes backward to compute live_in
                let mut live = out.clone();
                for i in (block.start..=block.end).rev() {
                    let op = &ops[i];
                    // Remove written registers
                    for w in opcode_info::writes(op) {
                        live.remove(&w.0);
                    }
                    // Add read registers
                    for r in opcode_info::reads(op) {
                        live.insert(r.0);
                    }
                }

                if live != live_in[block_idx] {
                    live_in[block_idx] = live;
                    live_out[block_idx] = out;
                    changed = true;
                }
            }
        }

        // Elimination pass: walk each block backward, track live set,
        // eliminate pure writes to dead registers.
        let mut eliminated = 0;

        for block_idx in 0..num_blocks {
            let block = &cfg.blocks[block_idx];

            // Start with live_out for this block
            let mut live: HashSet<u32> = HashSet::new();
            for &succ in &block.successors {
                for &r in &live_in[succ] {
                    live.insert(r);
                }
            }

            for i in (block.start..=block.end).rev() {
                let op = &ops[i];
                let written = opcode_info::writes(op);
                let read_regs = opcode_info::reads(op);

                // Check if this is a pure opcode writing to a dead register
                if written.len() == 1 && opcode_info::is_pure(op) {
                    let w = written[0].0;
                    if !live.contains(&w) {
                        ops[i] = Opcode::Nop;
                        eliminated += 1;
                        continue;
                    }
                }

                // Update liveness: remove written, add read
                for w in &written {
                    live.remove(&w.0);
                }
                for r in &read_regs {
                    live.insert(r.0);
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
    fn test_dead_write_eliminated() {
        let mut ops = vec![
            Opcode::Int { dst: Reg(0), ptr: RefInt(5) },   // dead: overwritten at op1
            Opcode::Int { dst: Reg(0), ptr: RefInt(10) },
            Opcode::Ret { ret: Reg(0) },
        ];
        let cfg = CFG::build(&ops);
        let pass = DeadRegElimPass;
        let elim = pass.run(&mut ops, 1, &cfg);
        assert_eq!(elim, 1);
        assert!(matches!(ops[0], Opcode::Nop));
        assert!(matches!(ops[1], Opcode::Int { .. }));
    }

    #[test]
    fn test_live_write_kept() {
        let mut ops = vec![
            Opcode::Int { dst: Reg(0), ptr: RefInt(5) },
            Opcode::Add { dst: Reg(1), a: Reg(0), b: Reg(0) },
            Opcode::Ret { ret: Reg(1) },
        ];
        let cfg = CFG::build(&ops);
        let pass = DeadRegElimPass;
        let elim = pass.run(&mut ops, 2, &cfg);
        assert_eq!(elim, 0); // r0 is read by Add, so Int is not dead
    }

    #[test]
    fn test_call_not_eliminated_even_if_dead() {
        let mut ops = vec![
            Opcode::Call0 { dst: Reg(0), fun: RefFun(0) },  // has side effects
            Opcode::Int { dst: Reg(0), ptr: RefInt(5) },
            Opcode::Ret { ret: Reg(0) },
        ];
        let cfg = CFG::build(&ops);
        let pass = DeadRegElimPass;
        let elim = pass.run(&mut ops, 1, &cfg);
        assert_eq!(elim, 0); // Call0 is not pure, must not be eliminated
    }

    #[test]
    fn test_unused_register_eliminated() {
        let mut ops = vec![
            Opcode::Int { dst: Reg(0), ptr: RefInt(5) },
            Opcode::Int { dst: Reg(1), ptr: RefInt(10) },  // r1 never read
            Opcode::Ret { ret: Reg(0) },
        ];
        let cfg = CFG::build(&ops);
        let pass = DeadRegElimPass;
        let elim = pass.run(&mut ops, 2, &cfg);
        assert_eq!(elim, 1);
        assert!(matches!(ops[1], Opcode::Nop));
    }
}
