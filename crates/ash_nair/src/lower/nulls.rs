//! `null`: the constant, and `==` and `!=` against it.

use air::v2::ir::*;
use ash_tast::format::*;

use super::{FnLower, Result};

impl<'m, 'a> FnLower<'m, 'a> {
    /// The constant `null`, of the type the typed program gives it.
    pub(super) fn null_value(&mut self, n: &NodeRec) -> Result<ValueId> {
        let ty = self.node_ty(n)?;
        if !self.m.types.table.is_nullable(ty) {
            return Err(format!("null as {:?}", self.m.types.table.types[ty]));
        }
        let dst = self.value(ty);
        self.emit(Instr::Null { dst });
        Ok(dst)
    }

    /// For `x == null`, `null == x` and the `!=` forms, the node of `x`.
    pub(super) fn null_operand(&self, n: &NodeRec) -> Option<u32> {
        let [a, b] = self.kids(n)[..] else {
            return None;
        };
        if self.is_null_literal(b) {
            Some(a)
        } else if self.is_null_literal(a) {
            Some(b)
        } else {
            None
        }
    }

    fn is_null_literal(&self, mut id: u32) -> bool {
        loop {
            let n = *self.tast.node(id);
            match n.kind {
                node_kind::PARENTHESIS => id = self.kid(&n, 0),
                node_kind::CONST => return n.flags == const_kind::NULL,
                _ => return false,
            }
        }
    }

    /// Branches to `yes` when `operand` is null (`is_null`) or when it is not, otherwise to `no`.
    pub(super) fn null_test(
        &mut self,
        operand: u32,
        is_null: bool,
        yes: BlockId,
        no: BlockId,
    ) -> Result<()> {
        let Some(v) = self.expr(operand)? else {
            return Ok(());
        };
        let ty = self.val_ty(v);
        if !self.m.types.table.is_nullable(ty) {
            return Err(format!(
                "{:?} is compared with null",
                self.m.types.table.types[ty]
            ));
        }
        self.terminate(Terminator::CondJump {
            cond: if is_null {
                CondKind::Null
            } else {
                CondKind::NotNull
            },
            a: v,
            b: None,
            if_true: yes,
            if_false: no,
        });
        Ok(())
    }
}
