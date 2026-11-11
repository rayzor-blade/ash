//! Exceptions. A throw reaches a trap the way a long jump does: the runtime
//! finds the innermost armed trap and jumps to its buffer, which on wasm is a
//! throw of the `__c_longjmp` tag carrying a copy of that buffer. A body that
//! has a trap region arms the trap, keeping the buffer in a local; every
//! instruction in the region that can throw runs inside a `try_table` that
//! catches the tag. The catch compares the thrown buffer's first word, which
//! names the buffer it was copied from, with the one in its own local: when
//! they are the same the handler block runs, and when they are not the jump is
//! thrown again for the trap outside.
use super::*;

fn mem(offset: u64, align: u32) -> we::MemArg {
    we::MemArg {
        offset,
        align,
        memory_index: 0,
    }
}

/// A trap region, named by its handler block.
#[derive(Clone, Copy)]
pub(crate) struct TrapInfo {
    /// The cell the exception is written to.
    pub exc_cell: air::v2::ir::CellId,
    /// The local that holds the armed buffer, zero while the trap is not armed.
    pub slot: u32,
    /// The trap this one is armed inside.
    pub parent: Option<usize>,
}

/// Whether an instruction can throw, or call something that can.
pub(crate) fn may_throw(ins: &Instr) -> bool {
    !matches!(
        ins,
        Instr::Param { .. }
            | Instr::Pos { .. }
            | Instr::Copy { .. }
            | Instr::Int { .. }
            | Instr::Float { .. }
            | Instr::Bool { .. }
            | Instr::Bytes { .. }
            | Instr::String { .. }
            | Instr::Null { .. }
            | Instr::BinOp { .. }
            | Instr::UnOp { .. }
            | Instr::TypeConst { .. }
            | Instr::GetGlobal { .. }
            | Instr::SetGlobal { .. }
            | Instr::CellGet { .. }
            | Instr::CellSet { .. }
            | Instr::CellIncr { .. }
            | Instr::CellDecr { .. }
            | Instr::CellRef { .. }
            | Instr::Fma { .. }
            | Instr::ArraySize { .. }
            | Instr::MemGet { .. }
            | Instr::MemSet { .. }
            | Instr::RefData { .. }
            | Instr::RefOffset { .. }
            | Instr::Unref { .. }
            | Instr::SetRef { .. }
            | Instr::EnumIndex { .. }
            | Instr::EnumField { .. }
            | Instr::SetEnumField { .. }
            | Instr::GetType { .. }
            | Instr::GetTID { .. }
            | Instr::EndTrap { .. }
    )
}

impl<P: Program> Fx<'_, P> {
    fn trap(&self, handler: usize) -> Res<TrapInfo> {
        self.traps
            .get(&handler)
            .copied()
            .ok_or_else(|| Unsupported("a trap region whose trap is not armed in this body".to_string()))
    }

    /// `throw exc` and `rethrow exc`: the runtime jumps, and does not return.
    pub(crate) fn throw(&mut self, x: usize, exc: ValueId, rethrow: bool) -> Res<()> {
        if self.ty(exc) != Ty::Ptr {
            return unsupported("a throw of other than a reference");
        }
        let name = if rethrow { "hlp_rethrow" } else { "hlp_throw" };
        let handler = self.f.blocks[x].handler.map(|h| h.idx());
        self.guarded(handler, &mut |fx| {
            fx.get(exc);
            let throw = fx.runtime(name)?;
            fx.call(throw);
            Ok(())
        })?;
        self.emit(&I::Unreachable);
        Ok(())
    }

    /// Open the trap region whose handler is `handler`, then go on to `normal`.
    pub(crate) fn arm_trap(&mut self, x: usize, handler: usize, normal: usize) -> Res<()> {
        let trap = self.trap(handler)?;
        let setup = self.runtime("hlp_setup_trap_jit")?;
        self.call(setup);
        self.emit(&I::LocalSet(trap.slot));
        // The buffer names itself, for the catch to compare; the word after it
        // is the label `setjmp` keeps there, which must not match a frame
        // lowered by `setjmp`.
        self.emit(&I::LocalGet(trap.slot));
        self.emit(&I::LocalGet(trap.slot));
        self.emit(&I::I32Store(mem(0, 2)));
        self.emit(&I::LocalGet(trap.slot));
        self.emit(&I::I32Const(-1));
        self.emit(&I::I32Store(mem(4, 2)));
        self.branch(x, normal)
    }

    /// The end of the region the block `x` is in.
    pub(crate) fn end_trap(&mut self, x: usize) -> Res<()> {
        let remove = self.runtime("hlp_remove_trap_jit")?;
        self.call(remove);
        if let Some(h) = self.f.blocks[x].handler {
            let trap = self.trap(h.idx())?;
            self.emit(&I::I32Const(0));
            self.emit(&I::LocalSet(trap.slot));
        }
        Ok(())
    }

    /// What a handler block does first: disarm its trap, and hand the
    /// exception to the cell the region wrote it to.
    pub(crate) fn landing(&mut self, handler: usize) -> Res<()> {
        let trap = self.trap(handler)?;
        self.emit(&I::I32Const(0));
        self.emit(&I::LocalSet(trap.slot));
        let get = self.runtime("hlp_get_exc_value")?;
        self.call(get);
        let tmp = self.tmp()?;
        self.emit(&I::LocalSet(tmp));
        let (at, kind) = self.cell_place(trap.exc_cell)?;
        if kind != Ty::Ptr {
            return unsupported("an exception cell that is not a reference");
        }
        self.frame_address(0);
        self.emit(&I::LocalGet(tmp));
        self.store_kind(kind, u64::from(at))?;
        let clear = self.runtime("hlp_clear_exc_value")?;
        self.call(clear);
        Ok(())
    }

    /// Run `body` so that a long jump thrown inside it goes to the trap
    /// `handler` when it was thrown to that trap, and on to the trap outside
    /// when it was not.
    pub(crate) fn guarded(
        &mut self,
        handler: Option<usize>,
        body: &mut dyn FnMut(&mut Self) -> Res<()>,
    ) -> Res<()> {
        let Some(h) = handler else {
            return body(self);
        };
        let trap = self.trap(h)?;
        let (thrown, env, val) = (
            self.scratch_word(0),
            self.scratch_word(1),
            self.scratch_word(2),
        );
        // block $done
        //   block $caught (result i32)
        //     try_table (catch __c_longjmp $caught) <body> br $done end
        //     unreachable
        //   end
        //   <dispatch on the thrown pointer>
        // end
        self.emit(&I::Block(BlockType::Empty));
        self.ctx.push(Frame::If);
        self.emit(&I::Block(BlockType::Result(ValType::I32)));
        self.ctx.push(Frame::If);
        self.out.try_long_jump(0);
        self.ctx.push(Frame::If);
        body(self)?;
        self.emit(&I::Br(2));
        self.ctx.pop();
        self.emit(&I::End);
        self.emit(&I::Unreachable);
        self.ctx.pop();
        self.emit(&I::End);

        self.emit(&I::LocalSet(thrown));
        self.emit(&I::LocalGet(thrown));
        self.emit(&I::I32Load(mem(0, 2)));
        self.emit(&I::LocalSet(env));
        self.emit(&I::LocalGet(thrown));
        self.emit(&I::I32Load(mem(4, 2)));
        self.emit(&I::LocalSet(val));
        self.emit(&I::LocalGet(env));
        self.emit(&I::I32Load(mem(0, 2)));
        self.emit(&I::LocalGet(trap.slot));
        self.emit(&I::I32Eq);
        self.emit(&I::If(BlockType::Empty));
        self.ctx.push(Frame::If);
        // Ours: the frames of anything that threw are gone, so the stack
        // pointer goes back to this body's.
        self.emit(&I::LocalGet(self.fp_local));
        self.out.stack_pointer_set();
        let to = self.label(Frame::Block(h));
        self.emit(&I::Br(to));
        self.emit(&I::Else);
        // Not ours: thrown again, to the trap outside this one if there is one.
        self.guarded(trap.parent, &mut |fx| {
            fx.emit(&I::LocalGet(env));
            fx.emit(&I::LocalGet(val));
            let jump = fx.runtime("__wasm_longjmp")?;
            fx.call(jump);
            Ok(())
        })?;
        self.emit(&I::Unreachable);
        self.ctx.pop();
        self.emit(&I::End);
        self.ctx.pop();
        self.emit(&I::End);
        Ok(())
    }
}
