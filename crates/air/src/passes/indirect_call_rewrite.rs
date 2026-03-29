use crate::opcodes::Opcode;
use std::collections::HashSet;

/// Rewrites direct `Call0/1/2/3/4/N` opcodes targeting bytecode functions
/// into `IndirectCall` opcodes that dispatch through `functions_ptrs[findex]`.
///
/// This enables hot-reload: when a function is recompiled and its address
/// updated in the pointer table, all callers using `IndirectCall` see
/// the new code without needing recompilation themselves.
///
/// Native function calls are left as direct calls since they never change.
pub struct IndirectCallRewritePass {
    /// Set of findexes that are native (should NOT be rewritten).
    native_findexes: HashSet<usize>,
}

impl IndirectCallRewritePass {
    pub fn new(native_findexes: HashSet<usize>) -> Self {
        Self { native_findexes }
    }

    /// Run the pass, rewriting direct calls to bytecode functions into indirect calls.
    /// Returns the number of opcodes rewritten.
    pub fn run(&self, ops: &mut Vec<Opcode>) -> usize {
        let mut rewritten = 0;

        for i in 0..ops.len() {
            let rewrite = match &ops[i] {
                Opcode::Call0 { dst, fun } if !self.native_findexes.contains(&fun.0) => {
                    Some(Opcode::IndirectCall {
                        dst: *dst,
                        fun: *fun,
                        args: vec![],
                    })
                }
                Opcode::Call1 { dst, fun, arg0 } if !self.native_findexes.contains(&fun.0) => {
                    Some(Opcode::IndirectCall {
                        dst: *dst,
                        fun: *fun,
                        args: vec![*arg0],
                    })
                }
                Opcode::Call2 {
                    dst,
                    fun,
                    arg0,
                    arg1,
                } if !self.native_findexes.contains(&fun.0) => Some(Opcode::IndirectCall {
                    dst: *dst,
                    fun: *fun,
                    args: vec![*arg0, *arg1],
                }),
                Opcode::Call3 {
                    dst,
                    fun,
                    arg0,
                    arg1,
                    arg2,
                } if !self.native_findexes.contains(&fun.0) => Some(Opcode::IndirectCall {
                    dst: *dst,
                    fun: *fun,
                    args: vec![*arg0, *arg1, *arg2],
                }),
                Opcode::Call4 {
                    dst,
                    fun,
                    arg0,
                    arg1,
                    arg2,
                    arg3,
                } if !self.native_findexes.contains(&fun.0) => Some(Opcode::IndirectCall {
                    dst: *dst,
                    fun: *fun,
                    args: vec![*arg0, *arg1, *arg2, *arg3],
                }),
                Opcode::CallN { dst, fun, args } if !self.native_findexes.contains(&fun.0) => {
                    Some(Opcode::IndirectCall {
                        dst: *dst,
                        fun: *fun,
                        args: args.clone(),
                    })
                }
                _ => None,
            };

            if let Some(new_op) = rewrite {
                ops[i] = new_op;
                rewritten += 1;
            }
        }

        rewritten
    }
}
