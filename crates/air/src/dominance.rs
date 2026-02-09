use crate::cfg::{BlockId, CFG};

/// Dominator tree and dominance frontiers, computed via Cooper-Harvey-Kennedy algorithm.
pub struct DominatorTree {
    /// Immediate dominator: idom[b] = immediate dominator of block b.
    /// idom[entry] = entry (self-loop).
    pub idom: Vec<BlockId>,
    /// Children in the dominator tree.
    pub dom_children: Vec<Vec<BlockId>>,
    /// Dominance frontier: df[b] = set of blocks in the dominance frontier of b.
    pub dom_frontier: Vec<Vec<BlockId>>,
    /// Reverse postorder traversal of the CFG.
    pub rpo: Vec<BlockId>,
    /// rpo_number[b] = position of block b in RPO (lower = earlier). Unreachable blocks get usize::MAX.
    pub rpo_number: Vec<usize>,
}

const UNDEF: usize = usize::MAX;

impl DominatorTree {
    pub fn build(cfg: &CFG) -> Self {
        let n = cfg.blocks.len();
        if n == 0 {
            return DominatorTree {
                idom: vec![],
                dom_children: vec![],
                dom_frontier: vec![],
                rpo: vec![],
                rpo_number: vec![],
            };
        }

        // Step 1: Compute reverse postorder via DFS
        let rpo = compute_rpo(cfg);
        let mut rpo_number = vec![UNDEF; n];
        for (pos, &block) in rpo.iter().enumerate() {
            rpo_number[block] = pos;
        }

        // Step 2: Compute immediate dominators (Cooper-Harvey-Kennedy)
        let mut idom = vec![UNDEF; n];
        idom[rpo[0]] = rpo[0]; // entry dominates itself

        let mut changed = true;
        while changed {
            changed = false;
            // Iterate in RPO order, skipping entry
            for &b in rpo.iter().skip(1) {
                // Find first processed predecessor
                let mut new_idom = UNDEF;
                for &p in &cfg.blocks[b].predecessors {
                    if idom[p] != UNDEF {
                        if new_idom == UNDEF {
                            new_idom = p;
                        } else {
                            new_idom = intersect(&idom, &rpo_number, new_idom, p);
                        }
                    }
                }
                if new_idom != UNDEF && idom[b] != new_idom {
                    idom[b] = new_idom;
                    changed = true;
                }
            }
        }

        // Step 3: Build dom_children from idom
        let mut dom_children = vec![vec![]; n];
        for b in 0..n {
            if idom[b] != UNDEF && idom[b] != b {
                dom_children[idom[b]].push(b);
            }
        }

        // Step 4: Compute dominance frontiers
        let mut dom_frontier: Vec<Vec<BlockId>> = vec![vec![]; n];
        for b in 0..n {
            if cfg.blocks[b].predecessors.len() >= 2 {
                for &pred in &cfg.blocks[b].predecessors {
                    let mut runner = pred;
                    // Walk up the dominator tree from pred until we reach idom[b]
                    while runner != UNDEF && runner != idom[b] {
                        if !dom_frontier[runner].contains(&b) {
                            dom_frontier[runner].push(b);
                        }
                        runner = idom[runner];
                    }
                }
            }
        }

        DominatorTree { idom, dom_children, dom_frontier, rpo, rpo_number }
    }

    /// Returns true if block `a` dominates block `b`.
    pub fn dominates(&self, a: BlockId, b: BlockId) -> bool {
        if a == b { return true; }
        let mut cur = b;
        while cur != self.idom[cur] {
            cur = self.idom[cur];
            if cur == a { return true; }
        }
        false
    }
}

/// Intersect two nodes in the dominator tree using RPO numbers.
fn intersect(idom: &[usize], rpo_number: &[usize], mut b1: BlockId, mut b2: BlockId) -> BlockId {
    while b1 != b2 {
        while rpo_number[b1] > rpo_number[b2] {
            b1 = idom[b1];
        }
        while rpo_number[b2] > rpo_number[b1] {
            b2 = idom[b2];
        }
    }
    b1
}

/// Compute reverse postorder of the CFG via iterative DFS from block 0.
fn compute_rpo(cfg: &CFG) -> Vec<BlockId> {
    let n = cfg.blocks.len();
    let mut visited = vec![false; n];
    let mut postorder = Vec::with_capacity(n);

    // Iterative DFS using explicit stack: (block, next_successor_index)
    let mut stack: Vec<(BlockId, usize)> = vec![(0, 0)];
    visited[0] = true;

    while let Some((block, idx)) = stack.last_mut() {
        let b = *block;
        if *idx < cfg.blocks[b].successors.len() {
            let succ = cfg.blocks[b].successors[*idx];
            *idx += 1;
            if !visited[succ] {
                visited[succ] = true;
                stack.push((succ, 0));
            }
        } else {
            postorder.push(b);
            stack.pop();
        }
    }

    postorder.reverse();
    postorder
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cfg::CFG;
    use crate::opcodes::*;

    #[test]
    fn test_single_block() {
        let ops = vec![
            Opcode::Int { dst: Reg(0), ptr: RefInt(0) },
            Opcode::Ret { ret: Reg(0) },
        ];
        let cfg = CFG::build(&ops);
        let dom = DominatorTree::build(&cfg);
        assert_eq!(dom.idom[0], 0);
        assert!(dom.dom_frontier[0].is_empty());
        assert!(dom.dom_children[0].is_empty());
        assert_eq!(dom.rpo, vec![0]);
    }

    #[test]
    fn test_diamond() {
        // Block 0: JTrue → block 2; fallthrough → block 1
        // Block 1: JAlways → block 3
        // Block 2: (fallthrough → block 3)
        // Block 3: Ret (join point)
        let ops = vec![
            Opcode::JTrue { cond: Reg(0), offset: 2 },      // op0: block 0
            Opcode::Int { dst: Reg(1), ptr: RefInt(0) },     // op1: block 1
            Opcode::JAlways { offset: 1 },                    // op2: block 1
            Opcode::Int { dst: Reg(1), ptr: RefInt(1) },     // op3: block 2
            Opcode::Ret { ret: Reg(1) },                      // op4: block 3
        ];
        let cfg = CFG::build(&ops);
        let dom = DominatorTree::build(&cfg);

        // 4 blocks: [0], [1-2], [3], [4]
        assert_eq!(cfg.blocks.len(), 4);

        // idom: block 0 dominates all
        assert_eq!(dom.idom[0], 0); // entry
        assert_eq!(dom.idom[1], 0); // then-branch
        assert_eq!(dom.idom[2], 0); // else-branch
        assert_eq!(dom.idom[3], 0); // join

        // Dominance frontier of then/else branches should include the join block
        let join = 3;
        assert!(dom.dom_frontier[1].contains(&join));
        assert!(dom.dom_frontier[2].contains(&join));
        // Entry has no DF
        assert!(dom.dom_frontier[0].is_empty());
    }

    #[test]
    fn test_loop() {
        // Block 0: Int → fallthrough to block 1
        // Block 1: Incr, JSLt back to block 1; fallthrough → block 2
        // Block 2: Ret
        let ops = vec![
            Opcode::Int { dst: Reg(0), ptr: RefInt(0) },
            Opcode::Incr { dst: Reg(0) },                        // op1: block 1 (leader: jump target)
            Opcode::JSLt { a: Reg(0), b: Reg(1), offset: -2 },   // op2: back to op1
            Opcode::Ret { ret: Reg(0) },                          // op3: block 2
        ];
        let cfg = CFG::build(&ops);
        let dom = DominatorTree::build(&cfg);

        // Blocks: [0], [1-2], [3]
        assert_eq!(cfg.blocks.len(), 3);
        let loop_block = cfg.block_of[1];

        // Loop header (block 1) has itself in its dominance frontier (back-edge)
        assert!(dom.dom_frontier[loop_block].contains(&loop_block));
    }

    #[test]
    fn test_dominates() {
        let ops = vec![
            Opcode::JTrue { cond: Reg(0), offset: 2 },
            Opcode::Int { dst: Reg(1), ptr: RefInt(0) },
            Opcode::JAlways { offset: 1 },
            Opcode::Int { dst: Reg(1), ptr: RefInt(1) },
            Opcode::Ret { ret: Reg(1) },
        ];
        let cfg = CFG::build(&ops);
        let dom = DominatorTree::build(&cfg);

        // Block 0 dominates everything
        assert!(dom.dominates(0, 0));
        assert!(dom.dominates(0, 1));
        assert!(dom.dominates(0, 2));
        assert!(dom.dominates(0, 3));

        // Block 1 does not dominate block 2 (they are siblings)
        assert!(!dom.dominates(1, 2));
        assert!(!dom.dominates(2, 1));

        // Neither branch dominates the join
        assert!(!dom.dominates(1, 3));
        assert!(!dom.dominates(2, 3));
    }

    #[test]
    fn test_linear_chain() {
        // 3 blocks in a chain: 0 → 1 → 2
        let ops = vec![
            Opcode::JAlways { offset: 0 },   // op0: block 0, jumps to op1
            Opcode::JAlways { offset: 0 },   // op1: block 1, jumps to op2
            Opcode::Ret { ret: Reg(0) },      // op2: block 2
        ];
        let cfg = CFG::build(&ops);
        let dom = DominatorTree::build(&cfg);

        assert_eq!(cfg.blocks.len(), 3);
        assert_eq!(dom.idom[0], 0);
        assert_eq!(dom.idom[1], 0);
        assert_eq!(dom.idom[2], 1);

        // Linear chain: no dominance frontiers
        assert!(dom.dom_frontier[0].is_empty());
        assert!(dom.dom_frontier[1].is_empty());
        assert!(dom.dom_frontier[2].is_empty());
    }
}
