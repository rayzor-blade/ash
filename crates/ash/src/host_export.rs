//! Program members a host calls by symbol: the reverse of `HostLink`.
//!
//! An AOT build given `HostExport`s defines one C function per export,
//! `symbol(w0, .., wn: u64) -> u64`, one word per argument (the receiver
//! first for an instance member). Each argument goes through its cast into
//! the member's parameter type, the member runs, and the result comes back
//! through `ret_cast`. A member that throws does not unwind into the caller:
//! the exception is handed to `raise` and the function returns `unit`.
//!
//! What a member does is written as a small bytecode function appended to
//! the program, so it is lowered like any other: a method call dispatches
//! through the class's vtable, a constructor allocates and runs `new`, a
//! getter or setter reads or writes the field. A static method needs no
//! such function; its own is called.

use crate::bytecode::DecodedBytecode;
use crate::hl;
use crate::opcodes::{Opcode, RefField, RefFun, RefGlobal, Reg};
use crate::types::{HLFunction, HLType, HLTypeFun, TypeRef};
use anyhow::{Result, anyhow, bail};

/// What an export reaches in its class.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExportKind {
    /// A static function.
    Static,
    /// An instance method, called on the receiver the first word names.
    Method,
    /// `new`: allocates an object of the class and runs its constructor.
    Constructor,
    /// Reads a variable: an instance field of the receiver, or a static.
    Getter,
    /// Writes a variable, the same two ways; returns `unit`.
    Setter,
}

/// A program member a host calls by `symbol`.
///
/// `class` is the class's name as the bytecode has it (`game.Player`) and
/// `member` the field or method (`new` for the constructor). `arg_casts[i]`
/// names a function called as `cast(word: u64, t: *mut hl_type)` that
/// answers the member's parameter `i` as the program holds it, `t` being
/// that parameter's type; `None` passes the word as it is, narrowed to the
/// parameter's width. `ret_cast` is called as `cast(value, t) -> u64` with
/// the result and its type; `None` widens the result to a word. `raise` is
/// called with the exception a member threw. `unit` is what a `Void` member
/// returns, and what any member returns after a raise.
///
/// An export whose member cannot throw has no trap: it is called as a plain
/// function. So is one whose member can throw only on a null receiver, which
/// the export tests first.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HostExport {
    pub symbol: String,
    pub class: String,
    pub member: String,
    pub kind: ExportKind,
    pub arg_casts: Vec<Option<String>>,
    pub ret_cast: Option<String>,
    pub raise: String,
    pub unit: u64,
    /// The host's word that none of the casts it names raises, so they need
    /// no trap around them. The `ash:` casts never raise either way.
    pub casts_nothrow: bool,
}

impl HostExport {
    pub(crate) fn casts_cannot_throw(&self) -> bool {
        self.casts_nothrow
            || self
                .arg_casts
                .iter()
                .chain(std::iter::once(&self.ret_cast))
                .flatten()
                .all(|cast| cast.starts_with("ash:"))
    }
}

/// An export resolved against one program: the function its symbol calls,
/// and that function's parameter and result types.
#[derive(Debug, Clone)]
pub(crate) struct ResolvedExport {
    pub export: HostExport,
    pub findex: usize,
    /// A copy of an instance member's export stub without its receiver null
    /// check. Only the export's checked non-null branch may call it.
    pub fast_findex: Option<usize>,
    pub params: Vec<usize>,
    pub ret: usize,
    /// Whether the first parameter is the receiver of an instance member.
    pub receiver: bool,
}

impl DecodedBytecode {
    /// Resolve `exports` against the program, appending the functions they
    /// call. Fails on a class or member the program does not have, or casts
    /// that do not cover the member's parameters.
    pub(crate) fn add_host_exports(
        &mut self,
        exports: &[HostExport],
    ) -> Result<Vec<ResolvedExport>> {
        let mut resolved: Vec<ResolvedExport> = exports
            .iter()
            .map(|e| self.add_host_export(e))
            .collect::<Result<_>>()?;
        // Every instance stub starts with NullCheck r0. If that is the only
        // throw it can make, the public export checks r0 before calling a
        // second, frameless copy. The original keeps its null check and frame
        // for the trapped branch, so a null access still has its Haxe trace.
        let module = crate::air_pipeline::AshModule::new(self);
        let fast: Vec<bool> = resolved
            .iter()
            .map(|e| {
                e.receiver
                    && e.export.casts_cannot_throw()
                    && module.frameless_given_receiver(e.findex)
            })
            .collect();
        drop(module);
        for (e, fast) in resolved.iter_mut().zip(fast) {
            if !fast {
                continue;
            }
            let (type_, regs, mut ops) = self
                .functions
                .iter()
                .find(|f| f.findex as usize == e.findex)
                .map(|stub| (stub.type_.clone(), stub.regs.clone(), stub.ops.clone()))
                .expect("an instance export has a bytecode stub");
            if !matches!(ops.first(), Some(Opcode::NullCheck { reg: Reg(0) })) {
                continue;
            }
            ops.remove(0);
            e.fast_findex = Some(self.push_export_function(type_, regs, ops));
        }
        Ok(resolved)
    }

    fn add_host_export(&mut self, e: &HostExport) -> Result<ResolvedExport> {
        let class = self.type_index_of(&e.class).ok_or_else(|| {
            anyhow!(
                "export `{}`: the program has no class `{}`",
                e.symbol,
                e.class
            )
        })?;
        let (findex, receiver) = match e.kind {
            ExportKind::Static => (self.static_function(class, e)?, false),
            ExportKind::Method => (self.method_caller(class, e)?, true),
            ExportKind::Constructor => (self.constructor_caller(class, e)?, false),
            ExportKind::Getter | ExportKind::Setter => self.variable_accessor(class, e)?,
        };
        let fun = self.function_type_of(findex)?;
        if fun.args.len() != e.arg_casts.len() {
            bail!(
                "export `{}`: `{}.{}` takes {} arguments here, and the export casts {}",
                e.symbol,
                e.class,
                e.member,
                fun.args.len(),
                e.arg_casts.len()
            );
        }
        Ok(ResolvedExport {
            export: e.clone(),
            findex,
            fast_findex: None,
            params: fun.args.iter().map(|a| a.0).collect(),
            ret: fun.ret.0,
            receiver,
        })
    }

    fn function_type_of(&self, findex: usize) -> Result<HLTypeFun> {
        let type_ = self
            .functions
            .iter()
            .find(|f| f.findex as usize == findex)
            .map(|f| f.type_.clone())
            .or_else(|| {
                self.natives
                    .iter()
                    .find(|n| n.findex as usize == findex)
                    .map(|n| n.type_.clone())
            })
            .ok_or_else(|| anyhow!("no function {findex}"))?;
        self.types[type_.0]
            .fun
            .clone()
            .ok_or_else(|| anyhow!("function {findex} has no function type"))
    }

    /// The class's `$Class` companion type and the global holding it.
    fn companion(&self, class: usize, e: &HostExport) -> Result<(usize, usize)> {
        let gv = self.types[class].obj.as_ref().map_or(0, |o| o.global_value);
        if gv == 0 {
            bail!("export `{}`: `{}` has no class object", e.symbol, e.class);
        }
        let global = gv as usize - 1;
        let companion = self.globals[global].0;
        Ok((companion, global))
    }

    /// `name`'s index among the fields of `type_index` and its ancestors,
    /// counted as a field operand counts them: every ancestor's first.
    fn flat_field(&self, type_index: usize, name: &str) -> Option<(usize, TypeRef)> {
        let mut chain = Vec::new();
        let mut cur = Some(type_index);
        while let Some(i) = cur {
            let obj = self.types.get(i)?.obj.as_ref()?;
            chain.push(i);
            cur = obj.super_.as_ref().map(|s| s.0);
        }
        let mut base = 0;
        let mut found = None;
        for &i in chain.iter().rev() {
            let obj = self.types[i].obj.as_ref()?;
            if let Some(at) = obj.fields.iter().position(|f| f.name == name) {
                found = Some((base + at, obj.fields[at].type_.clone()));
            }
            base += obj.fields.len();
        }
        found
    }

    /// The function a binding on `type_index` gives its field `name`.
    fn bound_function(&self, type_index: usize, name: &str) -> Option<usize> {
        let (field, _) = self.flat_field(type_index, name)?;
        let obj = self.types[type_index].obj.as_ref()?;
        obj.bindings
            .chunks_exact(2)
            .find(|b| b[0] as usize == field)
            .map(|b| b[1] as usize)
    }

    fn static_function(&self, class: usize, e: &HostExport) -> Result<usize> {
        let (companion, _) = self.companion(class, e)?;
        self.bound_function(companion, &e.member).ok_or_else(|| {
            anyhow!(
                "export `{}`: `{}` has no static function `{}`",
                e.symbol,
                e.class,
                e.member
            )
        })
    }

    fn method_caller(&mut self, class: usize, e: &HostExport) -> Result<usize> {
        let mut cur = Some(class);
        let mut found = None;
        while let Some(i) = cur {
            let obj = self.types[i]
                .obj
                .as_ref()
                .expect("a class chain is objects");
            if let Some(p) = obj.proto.iter().find(|p| p.name == e.member) {
                found = Some((p.findex as usize, p.pindex));
                break;
            }
            cur = obj.super_.as_ref().map(|s| s.0);
        }
        let (target, pindex) = found.ok_or_else(|| {
            anyhow!(
                "export `{}`: `{}` has no method `{}`",
                e.symbol,
                e.class,
                e.member
            )
        })?;
        let fun = self.function_type_of(target)?;
        let type_ = self
            .functions
            .iter()
            .find(|f| f.findex as usize == target)
            .map(|f| f.type_.clone())
            .ok_or_else(|| anyhow!("export `{}`: method {target} has no body", e.symbol))?;
        // The receiver's slot types as the exported class, so a subclass's
        // method called through a superclass's export dispatches on it.
        let mut regs = fun.args.clone();
        regs[0] = TypeRef(class);
        let dst = Reg(regs.len() as u32);
        regs.push(fun.ret.clone());
        let args: Vec<Reg> = (0..fun.args.len() as u32).map(Reg).collect();
        // A method no class below this one overrides is called directly, so
        // what it can throw is what its own body can.
        let call = if pindex >= 0 && self.overridden_below(class, &e.member, target) {
            Opcode::CallMethod {
                dst,
                field: RefField(pindex as usize),
                args,
            }
        } else {
            Opcode::CallN {
                dst,
                fun: RefFun(target),
                args,
            }
        };
        let type_ = if fun.args[0].0 == class {
            type_
        } else {
            let mut own = fun.clone();
            own.args[0] = TypeRef(class);
            self.push_fun_type(own)
        };
        let ops = vec![
            Opcode::NullCheck { reg: Reg(0) },
            call,
            Opcode::Ret { ret: dst },
        ];
        Ok(self.push_export_function(type_, regs, ops))
    }

    /// Whether a class at or below `class` gives `member` a body other than
    /// `target`.
    fn overridden_below(&self, class: usize, member: &str, target: usize) -> bool {
        self.types.iter().enumerate().any(|(i, t)| {
            let Some(obj) = t.obj.as_ref() else {
                return false;
            };
            let mut cur = Some(i);
            let mut below = false;
            while let Some(c) = cur {
                if c == class {
                    below = true;
                    break;
                }
                cur = self.types[c]
                    .obj
                    .as_ref()
                    .and_then(|o| o.super_.as_ref().map(|s| s.0));
            }
            below
                && obj
                    .proto
                    .iter()
                    .any(|p| p.name == member && p.findex as usize != target)
        })
    }

    fn constructor_caller(&mut self, class: usize, e: &HostExport) -> Result<usize> {
        let (companion, _) = self.companion(class, e)?;
        let ctor = self
            .bound_function(companion, "__constructor__")
            .ok_or_else(|| anyhow!("export `{}`: `{}` has no constructor", e.symbol, e.class))?;
        let fun = self.function_type_of(ctor)?;
        let params = fun.args[1..].to_vec();
        let void = self.kind_type(hl::hl_type_kind_HVOID);
        let mut regs = params.clone();
        let obj = Reg(regs.len() as u32);
        regs.push(TypeRef(class));
        let unit = Reg(regs.len() as u32);
        regs.push(TypeRef(void));
        let mut args = vec![obj];
        args.extend((0..params.len() as u32).map(Reg));
        let ops = vec![
            Opcode::New { dst: obj },
            Opcode::CallN {
                dst: unit,
                fun: RefFun(ctor),
                args,
            },
            Opcode::Ret { ret: obj },
        ];
        let type_ = self.push_fun_type(HLTypeFun {
            args: params,
            ret: TypeRef(class),
            ..Default::default()
        });
        Ok(self.push_export_function(type_, regs, ops))
    }

    fn variable_accessor(&mut self, class: usize, e: &HostExport) -> Result<(usize, bool)> {
        let setter = e.kind == ExportKind::Setter;
        let void = self.kind_type(hl::hl_type_kind_HVOID);
        if let Some((field, ty)) = self.flat_field(class, &e.member) {
            let field = RefField(field);
            let (args, regs, ops) = if setter {
                (
                    vec![TypeRef(class), ty.clone()],
                    vec![TypeRef(class), ty.clone(), TypeRef(void)],
                    vec![
                        Opcode::NullCheck { reg: Reg(0) },
                        Opcode::SetField {
                            obj: Reg(0),
                            field,
                            src: Reg(1),
                        },
                        Opcode::Ret { ret: Reg(2) },
                    ],
                )
            } else {
                (
                    vec![TypeRef(class)],
                    vec![TypeRef(class), ty.clone()],
                    vec![
                        Opcode::NullCheck { reg: Reg(0) },
                        Opcode::Field {
                            dst: Reg(1),
                            obj: Reg(0),
                            field,
                        },
                        Opcode::Ret { ret: Reg(1) },
                    ],
                )
            };
            let ret = if setter { TypeRef(void) } else { ty };
            let type_ = self.push_fun_type(HLTypeFun {
                args,
                ret,
                ..Default::default()
            });
            return Ok((self.push_export_function(type_, regs, ops), true));
        }
        let (companion, global) = self.companion(class, e)?;
        let is_function = self.bound_function(companion, &e.member).is_some();
        let Some((field, ty)) = self
            .flat_field(companion, &e.member)
            .filter(|_| !is_function)
        else {
            bail!(
                "export `{}`: `{}` has no variable `{}`",
                e.symbol,
                e.class,
                e.member
            );
        };
        let field = RefField(field);
        let global = RefGlobal(global);
        let (args, regs, ops) = if setter {
            (
                vec![ty.clone()],
                vec![ty.clone(), TypeRef(companion), TypeRef(void)],
                vec![
                    Opcode::GetGlobal {
                        dst: Reg(1),
                        global,
                    },
                    Opcode::SetField {
                        obj: Reg(1),
                        field,
                        src: Reg(0),
                    },
                    Opcode::Ret { ret: Reg(2) },
                ],
            )
        } else {
            (
                Vec::new(),
                vec![TypeRef(companion), ty.clone()],
                vec![
                    Opcode::GetGlobal {
                        dst: Reg(0),
                        global,
                    },
                    Opcode::Field {
                        dst: Reg(1),
                        obj: Reg(0),
                        field,
                    },
                    Opcode::Ret { ret: Reg(1) },
                ],
            )
        };
        let ret = if setter { TypeRef(void) } else { ty };
        let type_ = self.push_fun_type(HLTypeFun {
            args,
            ret,
            ..Default::default()
        });
        Ok((self.push_export_function(type_, regs, ops), false))
    }

    /// The program's type of a primitive kind, created if it has none.
    fn kind_type(&mut self, kind: hl::hl_type_kind) -> usize {
        match self.types.iter().position(|t| t.kind == kind) {
            Some(i) => i,
            None => {
                self.types.push(HLType {
                    kind,
                    ..Default::default()
                });
                self.types.len() - 1
            }
        }
    }

    fn push_fun_type(&mut self, fun: HLTypeFun) -> TypeRef {
        self.types.push(HLType {
            kind: hl::hl_type_kind_HFUN,
            fun: Some(fun),
            ..Default::default()
        });
        TypeRef(self.types.len() - 1)
    }

    fn push_export_function(
        &mut self,
        type_: TypeRef,
        regs: Vec<TypeRef>,
        ops: Vec<Opcode>,
    ) -> usize {
        let findex = self
            .functions
            .iter()
            .map(|f| f.findex)
            .chain(self.natives.iter().map(|n| n.findex))
            .max()
            .unwrap_or(-1)
            + 1;
        let debug = if self.has_debug {
            vec![0; ops.len() * 2]
        } else {
            Vec::new()
        };
        self.functions.push(HLFunction {
            type_,
            findex,
            ops,
            regs,
            debug,
            ref_: 0,
            obj: None,
            field_name: None,
            field_ref: None,
        });
        findex as usize
    }
}
