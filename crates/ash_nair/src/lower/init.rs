//! The function that sets a program up: the statics objects, then each class's `__init__`, then
//! the static variable initializers, then `main`, as genhl's `fun$init` does.
//!
//! What it leaves out is the runtime's class values (the objects that give a class a name and a
//! type for reflection): a program that does not ask a class for its name or type does not need
//! them.

use air::v2::ir::*;
use ash_tast::format::*;

use super::{FnLower, Lowered, Module, Result};
use crate::hl::HlType;

/// One piece of the set-up, in the order it runs.
enum Step {
    /// Make the class's statics object and put it in its global.
    Statics { class: u32 },
    /// Run a class's `__init__` block.
    Block { root: u32 },
    /// `Class.field = initializer`.
    Assign { field: u32, root: u32 },
    /// Call the program's `main`.
    Main { field: u32 },
}

fn is_static_var(flags: u32) -> bool {
    flags & field_flag::STATIC != 0
        && flags
            & (field_flag::METHOD
                | field_flag::NOT_PHYSICAL
                | field_flag::EXTERN
                | field_flag::DYNAMIC)
            == 0
}

impl<'a> Module<'a> {
    /// The classes of the program in the order the compiler generates them, which is the order
    /// their initializers run in.
    pub fn program_classes(&self) -> Vec<u32> {
        self.tast
            .order()
            .iter()
            .filter(|o| o.kind.get() == u32::from(module_kind::CLASS))
            .map(|o| o.id.get())
            .collect()
    }

    /// The function that sets up `classes` (in the order given, which should be program order)
    /// and then calls `main`, when there is one. Functions it calls are requested, so a later
    /// [`Self::lower_all`] defines them.
    pub fn lower_init(&mut self, classes: &[u32], main: Option<u32>) -> Result<Lowered> {
        let tast = self.tast;
        let mut steps = Vec::new();
        let vars = |c: u32| {
            let rec = tast.classes()[c as usize];
            (rec.fields_first.get()..rec.fields_first.get() + rec.fields_count.get())
                .filter(|f| is_static_var(tast.fields()[*f as usize].flags.get()))
                .collect::<Vec<u32>>()
        };
        for c in classes {
            if !vars(*c).is_empty() {
                steps.push(Step::Statics { class: *c });
            }
        }
        for c in classes {
            let init = tast.classes()[*c as usize].init_func.get();
            if init != NONE {
                let root = tast.funcs()[init as usize].root.get();
                if root != NONE {
                    steps.push(Step::Block { root });
                }
            }
        }
        for c in classes {
            for f in vars(*c) {
                let func = tast.fields()[f as usize].func.get();
                if func != NONE {
                    let root = tast.funcs()[func as usize].root.get();
                    if root != NONE {
                        steps.push(Step::Assign { field: f, root });
                    }
                }
            }
        }
        if let Some(field) = main {
            steps.push(Step::Main { field });
        }

        let findex = self.types.fun_id("", "$init");
        let void = self.types.table.add(HlType::Void);
        let sig = self.types.table.add(HlType::Fun(Vec::new(), void));
        let function = FnLower::bare(self).run_init(&steps)?;
        Ok(Lowered {
            name: "$init".into(),
            findex,
            sig,
            function,
            ctor_of: None,
        })
    }
}

impl<'m, 'a> FnLower<'m, 'a> {
    fn run_init(mut self, steps: &[Step]) -> Result<Function> {
        let first = self.new_block();
        self.jump(first);
        self.seal(first);
        self.cur = Some(first);
        for step in steps {
            if self.cur.is_none() {
                return Err("a static initializer does not finish".into());
            }
            match step {
                Step::Statics { class } => {
                    let (global, ty) = self.m.types.class_global(*class, true)?;
                    let obj = self.value(ty);
                    self.emit(Instr::New { dst: obj });
                    self.emit(Instr::SetGlobal {
                        global: global as usize,
                        src: obj,
                    });
                }
                Step::Block { root } => {
                    self.expr(*root)?;
                }
                Step::Assign { field, root } => {
                    let (global, slot) = self.static_slot(*field)?;
                    let Some(v) = self.expr(*root)? else {
                        return Err("a static initializer with no value".into());
                    };
                    let v = self.coerce(v, slot.ty)?;
                    let obj = self.value(slot.owner);
                    self.emit(Instr::GetGlobal {
                        dst: obj,
                        global: global as usize,
                    });
                    self.emit(Instr::FieldSet {
                        obj,
                        obj_ty: TypeRef(slot.owner as u32),
                        field: slot.index as usize,
                        src: v,
                    });
                }
                Step::Main { field } => {
                    let rec = self.tast.fields()[*field as usize];
                    let class = self.m.types.class_name(rec.owner.get()).to_owned();
                    let name = self.m.types.field_name(*field).to_owned();
                    let fun = self.m.types.fun_id(&class, &name);
                    self.m.called.insert(fun);
                    self.m.request(*field);
                    let void = self.ty_of(HlType::Void);
                    let dst = self.value(void);
                    self.emit(Instr::Call {
                        dst,
                        fun: fun as usize,
                        args: Vec::new(),
                    });
                }
            }
        }
        if self.cur.is_some() {
            let value = self.void_value();
            self.terminate(Terminator::Ret { value });
        }
        self.finish()
    }
}
