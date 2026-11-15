//! Types as the lowering reasons about them: interned, with type parameters that can be
//! substituted.
//!
//! The TAST file holds each type once, with its parameters in place. Specializing a generic means
//! putting arguments where the parameters were, so a type here is a value that `subst` can
//! rewrite. An anonymous structure stays a reference to its record in the file, with the
//! parameters that occur in it bound beside it, because a structure can contain itself.

use std::collections::{BTreeMap, HashMap, HashSet};

use ash_tast::Tast;
use ash_tast::format::*;

pub type TyId = u32;

/// Type parameter (the id of its class) to the type that replaces it.
pub type Env = BTreeMap<u32, TyId>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TyKind {
    Dyn,
    /// A type parameter, by the id of its class.
    Param(u32),
    Class(u32, Vec<TyId>),
    Enum(u32, Vec<TyId>),
    Abstract(u32, Vec<TyId>),
    Fun {
        args: Vec<TyId>,
        ret: TyId,
        opt: u32,
    },
    /// An anonymous structure: its record in the file, and what its free parameters stand for.
    Anon {
        tast: u32,
        env: Vec<(u32, TyId)>,
    },
}

pub struct Types<'a> {
    pub tast: Tast<'a>,
    kinds: Vec<TyKind>,
    intern: HashMap<TyKind, TyId>,
    cache: HashMap<u32, TyId>,
    free: HashMap<u32, Vec<u32>>,
    params: HashMap<(u32, u32), Vec<u32>>,
}

impl<'a> Types<'a> {
    pub fn new(tast: Tast<'a>) -> Types<'a> {
        Types {
            tast,
            kinds: Vec::new(),
            intern: HashMap::new(),
            cache: HashMap::new(),
            free: HashMap::new(),
            params: HashMap::new(),
        }
    }

    fn add(&mut self, k: TyKind) -> TyId {
        if let Some(id) = self.intern.get(&k) {
            return *id;
        }
        let id = self.kinds.len() as TyId;
        self.kinds.push(k.clone());
        self.intern.insert(k, id);
        id
    }

    pub fn kind(&self, t: TyId) -> &TyKind {
        &self.kinds[t as usize]
    }

    pub fn dynamic(&mut self) -> TyId {
        self.add(TyKind::Dyn)
    }

    fn args_of(
        &mut self,
        first: zerocopy::little_endian::U32,
        count: zerocopy::little_endian::U32,
    ) -> Vec<TyId> {
        let tast = self.tast;
        tast.type_args(first, count)
            .iter()
            .map(|a| self.from_tast(a.get()))
            .collect()
    }

    /// The type a TAST type id names, with its parameters unbound.
    pub fn from_tast(&mut self, id: u32) -> TyId {
        if let Some(t) = self.cache.get(&id) {
            return *t;
        }
        let rec = self.tast.types()[id as usize];
        let kind = match rec.kind {
            type_kind::CLASS => {
                let c = rec.def.get();
                let flags = self.tast.classes()[c as usize].flags.get();
                if flags & class_flag::TYPE_PARAM != 0 {
                    TyKind::Param(c)
                } else {
                    let args = self.args_of(rec.args_first, rec.args_count);
                    TyKind::Class(c, args)
                }
            }
            type_kind::ENUM => {
                let args = self.args_of(rec.args_first, rec.args_count);
                TyKind::Enum(rec.def.get(), args)
            }
            type_kind::ABSTRACT => {
                let args = self.args_of(rec.args_first, rec.args_count);
                TyKind::Abstract(rec.def.get(), args)
            }
            type_kind::FUN => {
                let args = self.args_of(rec.args_first, rec.args_count);
                let ret = self.from_tast(rec.def.get());
                TyKind::Fun {
                    args,
                    ret,
                    opt: rec.opt_mask.get(),
                }
            }
            type_kind::ANON => TyKind::Anon {
                tast: id,
                env: Vec::new(),
            },
            _ => TyKind::Dyn,
        };
        let t = self.add(kind);
        self.cache.insert(id, t);
        t
    }

    /// The type parameters a class, method, enum or abstract declares, as parameter ids.
    pub fn params_of(&mut self, owner_kind: u32, owner: u32) -> Vec<u32> {
        if let Some(p) = self.params.get(&(owner_kind, owner)) {
            return p.clone();
        }
        let ids: Vec<u32> = self
            .tast
            .generics_of(owner_kind, owner)
            .iter()
            .map(|t| t.get())
            .collect();
        let out: Vec<u32> = ids
            .into_iter()
            .filter_map(|t| {
                let ty = self.from_tast(t);
                match self.kind(ty) {
                    TyKind::Param(p) => Some(*p),
                    _ => None,
                }
            })
            .collect();
        self.params.insert((owner_kind, owner), out.clone());
        out
    }

    /// The parameters that occur in the structure the record `tast` describes.
    fn anon_free(&mut self, tast: u32) -> Vec<u32> {
        if let Some(f) = self.free.get(&tast) {
            return f.clone();
        }
        let mut seen = HashSet::new();
        let mut out = Vec::new();
        let mut stack = vec![tast];
        while let Some(id) = stack.pop() {
            if !seen.insert(id) {
                continue;
            }
            let rec = self.tast.types()[id as usize];
            match rec.kind {
                type_kind::CLASS => {
                    let c = rec.def.get();
                    if self.tast.classes()[c as usize].flags.get() & class_flag::TYPE_PARAM != 0 {
                        out.push(c);
                    } else {
                        stack.extend(
                            self.tast
                                .type_args(rec.args_first, rec.args_count)
                                .iter()
                                .map(|a| a.get()),
                        );
                    }
                }
                type_kind::ENUM | type_kind::ABSTRACT => stack.extend(
                    self.tast
                        .type_args(rec.args_first, rec.args_count)
                        .iter()
                        .map(|a| a.get()),
                ),
                type_kind::FUN => {
                    stack.extend(
                        self.tast
                            .type_args(rec.args_first, rec.args_count)
                            .iter()
                            .map(|a| a.get()),
                    );
                    stack.push(rec.def.get());
                }
                type_kind::ANON => {
                    let first = rec.def.get() as usize;
                    stack.extend(
                        self.tast.anon_fields()[first..first + rec.args_count.get() as usize]
                            .iter()
                            .map(|f| f.ty.get()),
                    );
                }
                _ => {}
            }
        }
        out.sort_unstable();
        out.dedup();
        self.free.insert(tast, out.clone());
        out
    }

    /// `t` with the parameters in `env` replaced.
    pub fn subst(&mut self, t: TyId, env: &Env) -> TyId {
        if env.is_empty() {
            return t;
        }
        match self.kind(t).clone() {
            TyKind::Dyn => t,
            TyKind::Param(p) => env.get(&p).copied().unwrap_or(t),
            TyKind::Class(c, args) => {
                let args = args.into_iter().map(|a| self.subst(a, env)).collect();
                self.add(TyKind::Class(c, args))
            }
            TyKind::Enum(e, args) => {
                let args = args.into_iter().map(|a| self.subst(a, env)).collect();
                self.add(TyKind::Enum(e, args))
            }
            TyKind::Abstract(a, args) => {
                let args = args.into_iter().map(|x| self.subst(x, env)).collect();
                self.add(TyKind::Abstract(a, args))
            }
            TyKind::Fun { args, ret, opt } => {
                let args = args.into_iter().map(|a| self.subst(a, env)).collect();
                let ret = self.subst(ret, env);
                self.add(TyKind::Fun { args, ret, opt })
            }
            TyKind::Anon { tast, env: bound } => {
                let free = self.anon_free(tast);
                let mut merged: BTreeMap<u32, TyId> = BTreeMap::new();
                for (p, v) in bound {
                    let v = self.subst(v, env);
                    merged.insert(p, v);
                }
                for p in free {
                    if !merged.contains_key(&p)
                        && let Some(v) = env.get(&p)
                    {
                        merged.insert(p, *v);
                    }
                }
                self.add(TyKind::Anon {
                    tast,
                    env: merged.into_iter().collect(),
                })
            }
        }
    }

    /// Binds the parameters in `pattern` to what stands in their place in `actual`. The first
    /// binding of a parameter wins; where the two do not have the same shape nothing is bound.
    pub fn bind(&mut self, pattern: TyId, actual: TyId, env: &mut Env) {
        match (self.kind(pattern).clone(), self.kind(actual).clone()) {
            (TyKind::Param(p), _) => {
                env.entry(p).or_insert(actual);
            }
            (TyKind::Class(a, x), TyKind::Class(b, y))
            | (TyKind::Enum(a, x), TyKind::Enum(b, y))
            | (TyKind::Abstract(a, x), TyKind::Abstract(b, y))
                if a == b && x.len() == y.len() =>
            {
                for (p, q) in x.into_iter().zip(y) {
                    self.bind(p, q, env);
                }
            }
            // `Null<T>` against a type that is not null: the parameter is the type itself
            (TyKind::Abstract(a, x), _) if x.len() == 1 && self.is_null_abstract(a) => {
                self.bind(x[0], actual, env);
            }
            (
                TyKind::Fun {
                    args: x, ret: r, ..
                },
                TyKind::Fun {
                    args: y, ret: s, ..
                },
            ) if x.len() == y.len() => {
                for (p, q) in x.into_iter().zip(y) {
                    self.bind(p, q, env);
                }
                self.bind(r, s, env);
            }
            (TyKind::Anon { tast: a, env: x }, TyKind::Anon { tast: b, env: y }) if a == b => {
                for ((_, p), (_, q)) in x.into_iter().zip(y) {
                    self.bind(p, q, env);
                }
            }
            _ => {}
        }
    }

    fn is_null_abstract(&self, a: u32) -> bool {
        self.tast.opt_string(self.tast.abstracts()[a as usize].name) == Some("Null")
    }

    pub fn class_name(&self, c: u32) -> &'a str {
        self.tast
            .opt_string(self.tast.classes()[c as usize].name)
            .unwrap_or("?")
    }

    pub fn abstract_name(&self, a: u32) -> &'a str {
        self.tast
            .opt_string(self.tast.abstracts()[a as usize].name)
            .unwrap_or("?")
    }

    /// The type an abstract unwraps to, with its arguments applied.
    pub fn underlying(&mut self, a: u32, args: &[TyId]) -> Option<TyId> {
        let rec = self.tast.abstracts()[a as usize];
        if rec.underlying.get() == NONE {
            return None;
        }
        let params = self.params_of(meta_owner::ABSTRACT, a);
        let env: Env = params.into_iter().zip(args.iter().copied()).collect();
        let under = self.from_tast(rec.underlying.get());
        Some(self.subst(under, &env))
    }

    /// A value type: held unboxed by compiled code, which a generic that erases it must box.
    pub fn is_value(&mut self, t: TyId) -> bool {
        self.value_depth(t, 0)
    }

    fn value_depth(&mut self, t: TyId, depth: u32) -> bool {
        if depth > 8 {
            return false;
        }
        let TyKind::Abstract(a, args) = self.kind(t).clone() else {
            return false;
        };
        match self.abstract_name(a) {
            "Int" | "UInt" | "Float" | "Single" | "Bool" | "hl.UI8" | "hl.UI16" | "hl.I64" => true,
            "Null" => false,
            _ => match self.underlying(a, &args) {
                Some(u) => self.value_depth(u, depth + 1),
                None => false,
            },
        }
    }

    /// How deeply `t` nests, to stop a generic from specializing itself at ever larger types.
    pub fn depth(&self, t: TyId) -> u32 {
        match self.kind(t) {
            TyKind::Class(_, a) | TyKind::Enum(_, a) | TyKind::Abstract(_, a) => {
                1 + a.iter().map(|x| self.depth(*x)).max().unwrap_or(0)
            }
            TyKind::Fun { args, ret, .. } => {
                1 + args
                    .iter()
                    .chain(std::iter::once(ret))
                    .map(|x| self.depth(*x))
                    .max()
                    .unwrap_or(0)
            }
            TyKind::Anon { env, .. } => {
                1 + env.iter().map(|(_, x)| self.depth(*x)).max().unwrap_or(0)
            }
            _ => 0,
        }
    }

    /// Whether any parameter is left in `t`.
    pub fn has_params(&self, t: TyId) -> bool {
        match self.kind(t) {
            TyKind::Param(_) => true,
            TyKind::Class(_, a) | TyKind::Enum(_, a) | TyKind::Abstract(_, a) => {
                a.iter().any(|x| self.has_params(*x))
            }
            TyKind::Fun { args, ret, .. } => {
                args.iter().any(|x| self.has_params(*x)) || self.has_params(*ret)
            }
            TyKind::Anon { env, .. } => env.iter().any(|(_, x)| self.has_params(*x)),
            TyKind::Dyn => false,
        }
    }

    pub fn describe(&self, t: TyId) -> String {
        let list = |a: &[TyId]| {
            a.iter()
                .map(|x| self.describe(*x))
                .collect::<Vec<_>>()
                .join(",")
        };
        match self.kind(t) {
            TyKind::Dyn => "Dynamic".into(),
            TyKind::Param(p) => self.class_name(*p).to_owned(),
            TyKind::Class(c, a) if a.is_empty() => self.class_name(*c).to_owned(),
            TyKind::Class(c, a) => format!("{}<{}>", self.class_name(*c), list(a)),
            TyKind::Enum(e, a) => {
                let name = self
                    .tast
                    .opt_string(self.tast.enums()[*e as usize].name)
                    .unwrap_or("?");
                if a.is_empty() {
                    name.to_owned()
                } else {
                    format!("{name}<{}>", list(a))
                }
            }
            TyKind::Abstract(x, a) if a.is_empty() => self.abstract_name(*x).to_owned(),
            TyKind::Abstract(x, a) => format!("{}<{}>", self.abstract_name(*x), list(a)),
            TyKind::Fun { args, ret, .. } => format!("({})->{}", list(args), self.describe(*ret)),
            TyKind::Anon { env, .. } if env.is_empty() => "{..}".into(),
            TyKind::Anon { env, .. } => {
                format!(
                    "{{..<{}>}}",
                    env.iter()
                        .map(|(_, x)| self.describe(*x))
                        .collect::<Vec<_>>()
                        .join(",")
                )
            }
        }
    }
}
