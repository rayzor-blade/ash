use super::copy_prop::substitute_reads;
use crate::cfg::CFG;
use crate::dominance::DominatorTree;
use crate::opcode_info;
use crate::opcodes::{Opcode, Reg};
use crate::ssa::SSAForm;
use std::collections::HashMap;

pub struct SSACopyPropPass;

impl SSACopyPropPass {
    pub fn name(&self) -> &str {
        "ssa_copy_prop"
    }

    pub fn run_ssa(
        &self,
        ops: &mut [Opcode],
        ssa: &mut SSAForm,
        _cfg: &CFG,
        _dom: &DominatorTree,
    ) -> usize {
        // Build copy_of map: for each Mov { dst, src }, record dst → src
        // Skip pinned registers — they're not in SSA form and may have multiple definitions
        let mut copy_of: HashMap<Reg, Reg> = HashMap::new();

        // Count how many SSA definitions exist per base register.
        // If a base register has multiple SSA versions, substituting across
        // register families is unsafe after de-SSA: all SSA versions collapse
        // back to the same base register, so a read might pick up a later
        // redefinition instead of the intended value.
        let mut base_def_count: HashMap<u32, usize> = HashMap::new();
        for op in ops.iter() {
            let writes = opcode_info::writes(op);
            for w in writes {
                if !ssa.pinned.contains(&w.0) {
                    let base = if (w.0 as usize) < ssa.base_reg.len() {
                        ssa.base_reg[w.0 as usize]
                    } else {
                        w.0
                    };
                    *base_def_count.entry(base).or_insert(0) += 1;
                }
            }
        }

        for op in ops.iter() {
            if let Opcode::Mov { dst, src } = op {
                if !ssa.pinned.contains(&dst.0) && !ssa.pinned.contains(&src.0) {
                    // Safety check: if src's base register has multiple SSA defs
                    // and dst has a different base register, the substitution is
                    // unsafe after de-SSA because the different defs collapse to
                    // one register and reads may pick up the wrong value.
                    let src_base = if (src.0 as usize) < ssa.base_reg.len() {
                        ssa.base_reg[src.0 as usize]
                    } else {
                        src.0
                    };
                    let dst_base = if (dst.0 as usize) < ssa.base_reg.len() {
                        ssa.base_reg[dst.0 as usize]
                    } else {
                        dst.0
                    };
                    let src_defs = base_def_count.get(&src_base).copied().unwrap_or(1);
                    if src_base == dst_base || src_defs <= 1 {
                        copy_of.insert(*dst, *src);
                    }
                }
            }
        }

        // Also record phi nodes that have a single unique source value as copies
        for block_phis in &ssa.phis {
            for phi in block_phis {
                if !phi.sources.is_empty() {
                    let first_src = phi.sources[0].1;
                    if phi.sources.iter().all(|(_, s)| *s == first_src) {
                        // Apply same safety check as Mov copies
                        let src_base = if (first_src.0 as usize) < ssa.base_reg.len() {
                            ssa.base_reg[first_src.0 as usize]
                        } else {
                            first_src.0
                        };
                        let dst_base = if (phi.dst.0 as usize) < ssa.base_reg.len() {
                            ssa.base_reg[phi.dst.0 as usize]
                        } else {
                            phi.dst.0
                        };
                        let src_defs = base_def_count.get(&src_base).copied().unwrap_or(1);
                        if src_base == dst_base || src_defs <= 1 {
                            copy_of.insert(phi.dst, first_src);
                        }
                    }
                }
            }
        }

        if copy_of.is_empty() {
            return 0;
        }

        // Transitively resolve copy chains (with cycle detection)
        fn resolve(copy_of: &HashMap<Reg, Reg>, r: Reg) -> Reg {
            let mut current = r;
            let mut steps = 0;
            while let Some(&src) = copy_of.get(&current) {
                if src == current || steps > 100 {
                    break;
                }
                current = src;
                steps += 1;
            }
            current
        }

        // Replace all reads in opcodes
        let mut eliminated = 0;
        #[allow(clippy::needless_range_loop)]
        for i in 0..ops.len() {
            if matches!(ops[i], Opcode::Nop) {
                continue;
            }

            let reads = opcode_info::reads(&ops[i]);
            for r in reads {
                let resolved = resolve(&copy_of, r);
                if resolved != r {
                    substitute_reads(&mut ops[i], r, resolved);
                }
            }

            // Eliminate self-moves
            if let Opcode::Mov { dst, src } = &ops[i] {
                if dst == src {
                    ops[i] = Opcode::Nop;
                    eliminated += 1;
                }
            }
        }

        // NOTE: Do NOT substitute phi sources. Our simple de-SSA (rename back to base regs)
        // assumes all phis are trivially `r = phi(r, r, ...)`. Substituting phi sources
        // can create non-trivial phis (e.g., `r5 = phi(r5, r20)`) that require Mov insertion
        // during de-SSA, which we don't support.

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
    fn test_global_copy_prop_across_blocks() {
        // Block 0: Mov(r1, r0), JAlways → block 1
        // Block 1: Add(r2, r1, r1), Ret(r2)
        // In SSA, Mov creates a copy. Global copy prop should substitute r1→r0 in Add.
        let mut ops = vec![
            Opcode::Mov {
                dst: Reg(1),
                src: Reg(0),
            },
            Opcode::JAlways { offset: 0 },
            Opcode::Add {
                dst: Reg(2),
                a: Reg(1),
                b: Reg(1),
            },
            Opcode::Ret { ret: Reg(2) },
        ];
        let cfg = CFG::build(&ops);
        let dom = DominatorTree::build(&cfg);
        let mut ssa = SSAForm::construct(&mut ops, 3, &cfg, &dom);

        let pass = SSACopyPropPass;
        pass.run_ssa(&mut ops, &mut ssa, &cfg, &dom);
        ssa.destroy(&mut ops);

        // After copy prop + de-SSA, the Add should use r0 instead of r1
        if let Opcode::Add { a, b, .. } = &ops[2] {
            assert_eq!(*a, Reg(0));
            assert_eq!(*b, Reg(0));
        } else {
            panic!("expected Add");
        }
    }

    #[test]
    fn test_copy_chain_resolved() {
        // r1 = r0, r2 = r1 → chain resolves to r0
        let mut ops = vec![
            Opcode::Mov {
                dst: Reg(1),
                src: Reg(0),
            },
            Opcode::Mov {
                dst: Reg(2),
                src: Reg(1),
            }, // chain: r2 → r1 → r0
            Opcode::Ret { ret: Reg(2) },
        ];
        let cfg = CFG::build(&ops);
        let dom = DominatorTree::build(&cfg);
        let mut ssa = SSAForm::construct(&mut ops, 3, &cfg, &dom);

        let pass = SSACopyPropPass;
        pass.run_ssa(&mut ops, &mut ssa, &cfg, &dom);
        ssa.destroy(&mut ops);

        // After copy prop + de-SSA, Ret should use r0 (chain fully resolved)
        if let Opcode::Ret { ret } = &ops[2] {
            assert_eq!(*ret, Reg(0));
        } else {
            panic!("expected Ret");
        }
    }
}
