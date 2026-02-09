use crate::opcodes::Opcode;
use crate::opcode_info;

pub type BlockId = usize;

#[derive(Debug)]
pub struct BasicBlock {
    pub id: BlockId,
    pub start: usize,
    pub end: usize,
    pub successors: Vec<BlockId>,
    pub predecessors: Vec<BlockId>,
}

#[derive(Debug)]
pub struct CFG {
    pub blocks: Vec<BasicBlock>,
    /// Maps opcode index → block id
    pub block_of: Vec<BlockId>,
}

impl CFG {
    /// Build a CFG from an opcode array.
    /// Block boundaries occur at jump targets, instructions after jumps/terminators, and function entry.
    pub fn build(ops: &[Opcode]) -> Self {
        let n = ops.len();
        if n == 0 {
            return CFG { blocks: vec![], block_of: vec![] };
        }

        // Pass 1: identify block leaders (first opcode of each basic block)
        let mut is_leader = vec![false; n];
        is_leader[0] = true;

        for (i, op) in ops.iter().enumerate() {
            if opcode_info::is_terminator(op) || opcode_info::is_conditional_branch(op) {
                // The instruction after a branch/terminator starts a new block
                if i + 1 < n {
                    is_leader[i + 1] = true;
                }
            }

            // Jump targets are leaders
            match op {
                Opcode::Switch { offsets, end, .. } => {
                    for off in offsets {
                        let target = (i as i32 + 1 + off) as usize;
                        if target < n { is_leader[target] = true; }
                    }
                    let target = (i as i32 + 1 + end) as usize;
                    if target < n { is_leader[target] = true; }
                }
                _ => {
                    if let Some(offset) = opcode_info::jump_offset(op) {
                        let target = (i as i32 + 1 + offset) as usize;
                        if target < n { is_leader[target] = true; }
                    }
                }
            }
        }

        // Pass 2: create basic blocks
        let mut blocks: Vec<BasicBlock> = Vec::new();
        let mut block_of = vec![0usize; n];
        let mut current_start = 0;

        for i in 0..n {
            if is_leader[i] && i > current_start {
                // Close the previous block
                let id = blocks.len();
                blocks.push(BasicBlock {
                    id,
                    start: current_start,
                    end: i - 1,
                    successors: vec![],
                    predecessors: vec![],
                });
                for j in current_start..i {
                    block_of[j] = id;
                }
                current_start = i;
            }
        }
        // Close last block
        let id = blocks.len();
        blocks.push(BasicBlock {
            id,
            start: current_start,
            end: n - 1,
            successors: vec![],
            predecessors: vec![],
        });
        for j in current_start..n {
            block_of[j] = id;
        }

        // Pass 3: compute edges
        for block_idx in 0..blocks.len() {
            let last_op_idx = blocks[block_idx].end;
            let succs = opcode_info::successors(&ops[last_op_idx], last_op_idx, n);
            for &target_op in &succs {
                if target_op < n {
                    let target_block = block_of[target_op];
                    if !blocks[block_idx].successors.contains(&target_block) {
                        blocks[block_idx].successors.push(target_block);
                    }
                }
            }
        }

        // Build predecessor lists from successor lists
        for i in 0..blocks.len() {
            let succs = blocks[i].successors.clone();
            for s in succs {
                if !blocks[s].predecessors.contains(&i) {
                    blocks[s].predecessors.push(i);
                }
            }
        }

        CFG { blocks, block_of }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::opcodes::*;

    #[test]
    fn test_linear_cfg() {
        let ops = vec![
            Opcode::Int { dst: Reg(0), ptr: RefInt(0) },
            Opcode::Int { dst: Reg(1), ptr: RefInt(1) },
            Opcode::Add { dst: Reg(2), a: Reg(0), b: Reg(1) },
            Opcode::Ret { ret: Reg(2) },
        ];
        let cfg = CFG::build(&ops);
        assert_eq!(cfg.blocks.len(), 1);
        assert_eq!(cfg.blocks[0].start, 0);
        assert_eq!(cfg.blocks[0].end, 3);
    }

    #[test]
    fn test_branch_cfg() {
        // op0: JTrue(r0, +2)  → target is op3
        // op1: Int(r1, 0)
        // op2: JAlways(+1)    → target is op4
        // op3: Int(r1, 1)     ← branch target, new block
        // op4: Ret(r1)        ← fallthrough target, new block
        let ops = vec![
            Opcode::JTrue { cond: Reg(0), offset: 2 },
            Opcode::Int { dst: Reg(1), ptr: RefInt(0) },
            Opcode::JAlways { offset: 1 },
            Opcode::Int { dst: Reg(1), ptr: RefInt(1) },
            Opcode::Ret { ret: Reg(1) },
        ];
        let cfg = CFG::build(&ops);
        // Block 0: ops 0 (JTrue splits)
        // Block 1: ops 1-2 (fallthrough from JTrue, ends with JAlways)
        // Block 2: ops 3 (target of JTrue offset +2)
        // Block 3: ops 4 (target of JAlways +1)
        assert_eq!(cfg.blocks.len(), 4);

        // Block 0 successors: block 1 (fallthrough) and block 2 (jump target)
        assert!(cfg.blocks[0].successors.contains(&1));
        assert!(cfg.blocks[0].successors.contains(&2));
    }

    #[test]
    fn test_loop_cfg() {
        // op0: Int(r0, 0)
        // op1: Label          ← loop header
        // op2: Incr(r0)
        // op3: JSLt(r0, r1, -2)  → target is op2 (i=3, 3+1+(-2)=2), but op1 is the leader
        // op4: Ret(r0)
        let ops = vec![
            Opcode::Int { dst: Reg(0), ptr: RefInt(0) },
            Opcode::Label,
            Opcode::Incr { dst: Reg(0) },
            Opcode::JSLt { a: Reg(0), b: Reg(1), offset: -2 },
            Opcode::Ret { ret: Reg(0) },
        ];
        let cfg = CFG::build(&ops);
        // Block boundaries at: 0 (entry), 1 (Label target of backward jump), 2 (target of JSLt -2 → op2), 4 (after JSLt)
        // Actually: op3 JSLt offset=-2 → target = 3+1+(-2) = 2. So op2 is a leader.
        // Also op4 is a leader (after conditional branch op3).
        // And op1 is NOT necessarily a leader unless something jumps to it.
        // Leaders: 0 (entry), 2 (jump target), 4 (after branch)
        // Wait - Label at op1 doesn't make it a leader. Only being a jump TARGET does.
        // op3 JSLt offset=-2 → target = 2. So op2 is leader.
        // Blocks: [0-1], [2-3], [4]
        assert_eq!(cfg.blocks.len(), 3);

        // Block 1 (ops 2-3) should have itself as a successor (loop back-edge to op2)
        let loop_block = cfg.block_of[2];
        assert!(cfg.blocks[loop_block].successors.contains(&loop_block));
    }
}
