//! Finds where generic code is used at concrete types.
//!
//! A generic function or class is erased to one copy; specializing it means knowing the
//! arguments at each use. This walks every body the program has, starting from the functions that
//! are not generic, and at each call or `new` of generic code works out what its type parameters
//! stand for. A specialized body is then walked in turn with those arguments in place, so a
//! generic that calls another at its own parameter is followed.
//!
//! The result is a lower bound: a call the walk cannot resolve (through an interface, or a
//! parameter that stays unbound) is not counted, and the erased copy still serves it.

use std::collections::{HashMap, HashSet};

use ash_tast::Tast;
use ash_tast::format::*;

use crate::ty::{Env, TyId, TyKind, Types};

/// A function with its type parameters bound.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Instance {
    /// The field that holds the function.
    pub field: u32,
    /// What each type parameter of the field and its class stands for, by parameter.
    pub args: Vec<(u32, TyId)>,
}

pub struct Collector<'a> {
    pub types: Types<'a>,
    tast: Tast<'a>,
    ctor_of: HashMap<u32, u32>,
    /// Every instance found, with how many call sites use it.
    pub found: HashMap<Instance, usize>,
    /// Classes instantiated with arguments, with how many `new` sites.
    pub classes: HashMap<(u32, Vec<TyId>), usize>,
    visited: HashSet<(u32, Vec<(u32, TyId)>)>,
    work: Vec<(u32, Env)>,
    /// Instances left unresolved because a parameter stayed unbound or the type grew too deep.
    pub unresolved: usize,
}

/// How deeply an argument may nest before a generic stops being specialized at it.
const MAX_DEPTH: u32 = 6;

impl<'a> Collector<'a> {
    pub fn new(tast: Tast<'a>) -> Collector<'a> {
        let mut ctor_of = HashMap::new();
        for (c, rec) in tast.classes().iter().enumerate() {
            let first = rec.fields_first.get();
            if rec.fields_count.get() > 0 {
                let f = &tast.fields()[first as usize];
                if f.flags.get() & field_flag::STATIC == 0
                    && f.flags.get() & field_flag::METHOD != 0
                    && tast.opt_string(f.name) == Some("new")
                {
                    ctor_of.insert(c as u32, first);
                }
            }
        }
        Collector {
            types: Types::new(tast),
            tast,
            ctor_of,
            found: HashMap::new(),
            classes: HashMap::new(),
            visited: HashSet::new(),
            work: Vec::new(),
            unresolved: 0,
        }
    }

    /// Walks the program from its non-generic functions.
    pub fn run(&mut self) {
        if let Some(entry) = self.tast.entry() {
            self.work.push((entry, Env::new()));
        }
        let program: Vec<u32> = self
            .tast
            .order()
            .iter()
            .filter(|o| o.kind.get() == u32::from(module_kind::CLASS))
            .map(|o| o.id.get())
            .collect();
        for c in program {
            let class = self.tast.classes()[c as usize];
            if class.init_func.get() != NONE {
                self.work.push((class.init_func.get(), Env::new()));
            }
            if !self.types.params_of(meta_owner::CLASS, c).is_empty() {
                continue;
            }
            let first = class.fields_first.get();
            for f in first..first + class.fields_count.get() {
                let rec = self.tast.fields()[f as usize];
                if rec.func.get() != NONE && self.types.params_of(meta_owner::FIELD, f).is_empty() {
                    self.work.push((rec.func.get(), Env::new()));
                }
            }
        }
        while let Some((func, env)) = self.work.pop() {
            let key = (func, env.iter().map(|(p, t)| (*p, *t)).collect::<Vec<_>>());
            if self.visited.insert(key) {
                self.walk(func, &env);
            }
        }
    }

    fn walk(&mut self, func: u32, env: &Env) {
        let root = self.tast.funcs()[func as usize].root.get();
        if root == NONE {
            return;
        }
        let mut stack = vec![root];
        while let Some(id) = stack.pop() {
            let node = *self.tast.node(id);
            match node.kind {
                node_kind::NEW => self.on_new(&node, env),
                node_kind::CALL => {
                    let callee = *self.tast.node(self.tast.kid_ids(&node).start);
                    if callee.kind == node_kind::FIELD {
                        self.on_call(&callee, env);
                    }
                }
                node_kind::FUNCTION => self.work.push((node.a.get(), env.clone())),
                _ => {}
            }
            stack.extend(self.tast.kid_ids(&node));
        }
    }

    fn node_ty(&mut self, n: &NodeRec, env: &Env) -> Option<TyId> {
        if n.ty.get() == NONE {
            return None;
        }
        let t = self.types.from_tast(n.ty.get());
        Some(self.types.subst(t, env))
    }

    fn on_new(&mut self, node: &NodeRec, env: &Env) {
        let class = node.a.get();
        let Some(ty) = self.node_ty(node, env) else {
            return;
        };
        let TyKind::Class(c, args) = self.types.kind(ty).clone() else {
            return;
        };
        if c != class || args.is_empty() {
            return;
        }
        let params = self.types.params_of(meta_owner::CLASS, class);
        if params.len() != args.len() {
            return;
        }
        *self.classes.entry((class, args.clone())).or_default() += 1;
        let Some(ctor) = self.ctor_of.get(&class).copied() else {
            return;
        };
        let callenv: Env = params.into_iter().zip(args).collect();
        self.record(ctor, callenv);
    }

    fn on_call(&mut self, callee: &NodeRec, env: &Env) {
        let access = callee.a.get();
        if access != field_access::INSTANCE
            && access != field_access::STATIC
            && access != field_access::CLOSURE
        {
            return;
        }
        let f = callee.b.get();
        if f == NONE {
            return;
        }
        let rec = self.tast.fields()[f as usize];
        let owner = rec.owner.get();
        let instance = access != field_access::STATIC;
        let owner_params = if instance {
            self.types.params_of(meta_owner::CLASS, owner)
        } else {
            Vec::new()
        };
        let method_params = self.types.params_of(meta_owner::FIELD, f);
        if owner_params.is_empty() && method_params.is_empty() {
            return;
        }
        let mut callenv = Env::new();
        // the receiver says what the class's own parameters stand for
        if instance && !owner_params.is_empty() {
            let object = *self.tast.node(self.tast.kid_ids(callee).start);
            if let Some(rty) = self.node_ty(&object, env)
                && let Some(args) = self.upcast(rty, owner, 0)
                && args.len() == owner_params.len()
            {
                for (p, a) in owner_params.iter().zip(args) {
                    callenv.insert(*p, a);
                }
            }
        }
        // the signature, as used here against as declared, says the rest
        let declared = self.types.from_tast(rec.ty.get());
        let pattern = self.types.subst(declared, &callenv);
        if let Some(actual) = self.node_ty(callee, env) {
            self.types.bind(pattern, actual, &mut callenv);
        }
        let wanted: Vec<u32> = owner_params.iter().chain(&method_params).copied().collect();
        callenv.retain(|p, _| wanted.contains(p));
        if wanted.iter().any(|p| !callenv.contains_key(p)) {
            self.unresolved += 1;
            return;
        }
        self.record(f, callenv);
    }

    /// The arguments of `owner` when `t` is that class or a subclass of it.
    fn upcast(&mut self, t: TyId, owner: u32, depth: u32) -> Option<Vec<TyId>> {
        let TyKind::Class(c, args) = self.types.kind(t).clone() else {
            return None;
        };
        if c == owner {
            return Some(args);
        }
        if depth > 16 {
            return None;
        }
        let params = self.types.params_of(meta_owner::CLASS, c);
        let env: Env = params.into_iter().zip(args).collect();
        let class = self.tast.classes()[c as usize];
        let mut parents: Vec<u32> = self
            .tast
            .type_args(class.interfaces_first, class.interfaces_count)
            .iter()
            .map(|t| t.get())
            .collect();
        if class.super_type.get() != NONE {
            parents.insert(0, class.super_type.get());
        }
        for p in parents {
            let pt = self.types.from_tast(p);
            let pt = self.types.subst(pt, &env);
            if let Some(found) = self.upcast(pt, owner, depth + 1) {
                return Some(found);
            }
        }
        None
    }

    fn record(&mut self, field: u32, env: Env) {
        let too_deep = env
            .values()
            .any(|t| self.types.has_params(*t) || self.types.depth(*t) > MAX_DEPTH);
        if too_deep {
            self.unresolved += 1;
            return;
        }
        let args: Vec<(u32, TyId)> = env.iter().map(|(p, t)| (*p, *t)).collect();
        *self
            .found
            .entry(Instance {
                field,
                args: args.clone(),
            })
            .or_default() += 1;
        let func = self.tast.fields()[field as usize].func.get();
        if func != NONE {
            self.work.push((func, env));
        }
    }

    /// `Class.method` for the field an instance names.
    pub fn name_of(&self, field: u32) -> String {
        let rec = &self.tast.fields()[field as usize];
        format!(
            "{}.{}",
            self.types.class_name(rec.owner.get()),
            self.tast.opt_string(rec.name).unwrap_or("?")
        )
    }

    /// The instance as `Class.method<A=Int, B=String>`.
    pub fn describe(&self, i: &Instance) -> String {
        let args: Vec<String> = i
            .args
            .iter()
            .map(|(p, t)| {
                let name = self.types.class_name(*p);
                let name = name.rsplit('.').next().unwrap_or(name);
                format!("{name}={}", self.types.describe(*t))
            })
            .collect();
        format!("{}<{}>", self.name_of(i.field), args.join(", "))
    }

    /// Whether any argument is held unboxed by compiled code, so erasing it costs a box.
    pub fn has_value_arg(&mut self, i: &Instance) -> bool {
        let args: Vec<TyId> = i.args.iter().map(|(_, t)| *t).collect();
        args.into_iter().any(|t| self.types.is_value(t))
    }
}
