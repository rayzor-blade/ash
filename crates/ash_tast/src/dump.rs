//! A canonical text of a function, with names in place of ids.
//!
//! Ids differ between two exports of the same program (and between a program
//! exported whole and one exported on top of a library layer), but the text does
//! not: classes, fields, enum constructors and types print by name, and locals by
//! the order they are first met. Two functions are the same function when their
//! text is the same, which is what the tests and the layer tool compare.

use std::collections::{BTreeMap, HashMap};
use std::fmt::Write;

use zerocopy::little_endian::U32;

use crate::format::*;
use crate::stack::Stack;

/// Prints functions of a stack.
pub struct Dumper<'s, 'a> {
    stack: &'s Stack<'a>,
}

/// How many locals a dump has numbered so far.
#[derive(Default)]
struct Locals {
    ordinals: HashMap<(usize, u32), usize>,
}

impl<'s, 'a> Dumper<'s, 'a> {
    pub fn new(stack: &'s Stack<'a>) -> Dumper<'s, 'a> {
        Dumper { stack }
    }

    fn string(&self, layer: usize, id: U32) -> &'a str {
        self.stack.layers()[layer].opt_string(id).unwrap_or("?")
    }

    /// A type as text: a class, enum or abstract by name with its arguments, a
    /// function as `(args)->ret`, a structure as `{name:type}`.
    pub fn type_text(&self, from: usize, id: U32) -> String {
        self.type_at(from, id, 0)
    }

    fn type_at(&self, from: usize, id: U32, depth: usize) -> String {
        if id.get() == NONE {
            return "_".to_owned();
        }
        if depth > 4 {
            return "…".to_owned();
        }
        let (layer, t) = self.stack.type_rec(from, id);
        let tast = &self.stack.layers()[layer];
        let args = |t: &TypeRec| -> String {
            let list = tast.type_args(t.args_first, t.args_count);
            if list.is_empty() {
                String::new()
            } else {
                let parts: Vec<String> = list
                    .iter()
                    .map(|a| self.type_at(layer, *a, depth + 1))
                    .collect();
                format!("<{}>", parts.join(","))
            }
        };
        match t.kind {
            type_kind::CLASS => {
                let (l, c) = self.stack.class_rec(layer, t.def);
                format!("{}{}", self.string(l, c.name), args(t))
            }
            type_kind::ENUM => {
                let (l, e) = self.stack.enum_rec(layer, t.def);
                format!("{}{}", self.string(l, e.name), args(t))
            }
            type_kind::ABSTRACT => {
                let (l, a) = self.stack.abstract_rec(layer, t.def);
                format!("{}{}", self.string(l, a.name), args(t))
            }
            type_kind::FUN => {
                let list = tast.type_args(t.args_first, t.args_count);
                let parts: Vec<String> = list
                    .iter()
                    .map(|a| self.type_at(layer, *a, depth + 1))
                    .collect();
                format!(
                    "({})->{}",
                    parts.join(","),
                    self.type_at(layer, t.def, depth + 1)
                )
            }
            type_kind::ANON => {
                let first = t.def.get() as usize;
                let fields = &tast.anon_fields()[first..first + t.args_count.get() as usize];
                let parts: Vec<String> = fields
                    .iter()
                    .map(|f| {
                        format!(
                            "{}:{}",
                            self.string(layer, f.name),
                            self.type_at(layer, f.ty, depth + 1)
                        )
                    })
                    .collect();
                format!("{{{}}}", parts.join(","))
            }
            type_kind::DYNAMIC => "Dynamic".to_owned(),
            _ => "?".to_owned(),
        }
    }

    fn class_name(&self, from: usize, id: U32) -> String {
        let (l, c) = self.stack.class_rec(from, id);
        self.string(l, c.name).to_owned()
    }

    /// A field as `Class#name`, where `Class` is the class that declares it.
    pub fn field_text(&self, from: usize, id: U32) -> String {
        let (l, f) = self.stack.field_rec(from, id);
        format!("{}#{}", self.class_name(l, f.owner), self.string(l, f.name))
    }

    fn enum_ctor_text(&self, from: usize, id: U32) -> String {
        let (l, c) = self.stack.enum_ctor_rec(from, id);
        let (el, e) = self.stack.enum_rec(l, c.enum_id);
        format!("{}.{}", self.string(el, e.name), self.string(l, c.name))
    }

    fn module_text(&self, from: usize, module: u8, index: U32) -> String {
        match module {
            module_kind::CLASS => self.class_name(from, index),
            module_kind::ENUM => {
                let (l, e) = self.stack.enum_rec(from, index);
                self.string(l, e.name).to_owned()
            }
            _ => {
                let (l, a) = self.stack.abstract_rec(from, index);
                self.string(l, a.name).to_owned()
            }
        }
    }

    fn local(&self, out: &mut String, locals: &mut Locals, layer: usize, id: U32, declare: bool) {
        let next = locals.ordinals.len();
        let ordinal = *locals.ordinals.entry((layer, id.get())).or_insert(next);
        let tast = &self.stack.layers()[layer];
        let l = &tast.locals()[id.get() as usize];
        if declare {
            let _ = write!(
                out,
                "v{ordinal}:{}:{}",
                self.string(layer, l.name),
                self.type_text(layer, l.ty)
            );
        } else {
            let _ = write!(out, "v{ordinal}");
        }
    }

    /// The text of every function of the fields of `layer`, keyed `Class#i:name` or
    /// `Class#s:name`.
    pub fn own_functions(&self, layer: usize) -> BTreeMap<String, String> {
        let tast = &self.stack.layers()[layer];
        let mut out = BTreeMap::new();
        for class in tast.classes() {
            let first = class.fields_first.get() as usize;
            for field in &tast.fields()[first..first + class.fields_count.get() as usize] {
                if field.func.get() == NONE {
                    continue;
                }
                let key = self.field_key(layer, class, field);
                out.insert(key, self.function_text(layer, field.func.get() as usize));
            }
        }
        out
    }

    /// The text of the functions of what `layer` kept from the layers below it, keyed
    /// the same way.
    pub fn kept_functions(&self, layer: usize) -> BTreeMap<String, String> {
        let mut out = BTreeMap::new();
        for kept in self.stack.layers()[layer].kept() {
            if kept.kind.get() != kept_kind::FIELD {
                continue;
            }
            let (l, field) = self.stack.field_rec(layer, kept.id);
            if field.func.get() == NONE {
                continue;
            }
            let (_, class) = self.stack.class_rec(l, field.owner);
            out.insert(
                self.field_key(l, class, field),
                self.function_text(l, field.func.get() as usize),
            );
        }
        out
    }

    fn field_key(&self, layer: usize, class: &ClassRec, field: &FieldRec) -> String {
        let marker = if field.flags.get() & field_flag::STATIC != 0 {
            "#s:"
        } else {
            "#i:"
        };
        format!(
            "{}{marker}{}",
            self.string(layer, class.name),
            self.string(layer, field.name)
        )
    }

    /// The text of function `func` of layer `layer`: its parameters, its result and its body.
    pub fn function_text(&self, layer: usize, func: usize) -> String {
        let mut out = String::new();
        let mut locals = Locals::default();
        self.function(&mut out, &mut locals, layer, func, 0);
        out
    }

    fn function(
        &self,
        out: &mut String,
        locals: &mut Locals,
        layer: usize,
        func: usize,
        depth: usize,
    ) {
        let tast = &self.stack.layers()[layer];
        let f = &tast.funcs()[func];
        let _ = write!(out, "fn(");
        for (i, p) in tast.func_params(f).iter().enumerate() {
            if i > 0 {
                out.push_str(", ");
            }
            self.local(out, locals, layer, p.local, true);
            if p.default_value.get() != NONE {
                out.push_str(" = ");
                self.node(out, locals, layer, p.default_value.get(), depth + 1);
            }
        }
        let _ = writeln!(out, ") -> {}", self.type_text(layer, f.ret));
        if f.root.get() != NONE {
            self.node(out, locals, layer, f.root.get(), depth + 1);
        }
    }

    fn node(&self, out: &mut String, locals: &mut Locals, layer: usize, id: u32, depth: usize) {
        use node_kind as k;
        let tast = &self.stack.layers()[layer];
        let n = tast.node(id);
        let _ = write!(out, "{:indent$}", "", indent = depth * 2);
        let name = NODE_KINDS
            .iter()
            .find(|(_, v)| *v == n.kind)
            .map_or("?", |(n, _)| *n);
        out.push_str(name);
        let (a, b, flags) = (n.a, n.b, n.flags);
        match n.kind {
            k::CONST => match flags {
                const_kind::INT => {
                    let _ = write!(out, " {}", a.get() as i32);
                }
                const_kind::FLOAT | const_kind::STRING => {
                    let _ = write!(out, " {:?}", self.string(layer, a));
                }
                const_kind::BOOL => {
                    let _ = write!(out, " {}", a.get() != 0);
                }
                other => {
                    let _ = write!(out, " kind{other}");
                }
            },
            k::LOCAL => {
                out.push(' ');
                self.local(out, locals, layer, a, false);
            }
            k::VAR | k::FOR | k::CATCH => {
                out.push(' ');
                self.local(out, locals, layer, a, true);
            }
            k::BINOP => {
                let _ = write!(out, " {}", BINOPS[a.get() as usize]);
                if a.get() == BINOP_ASSIGN_OP {
                    let _ = write!(out, "/{}", BINOPS[b.get() as usize]);
                }
            }
            k::UNOP => {
                let _ = write!(
                    out,
                    " {}{}",
                    UNOPS[a.get() as usize],
                    if flags & node_flag::POSTFIX != 0 {
                        " postfix"
                    } else {
                        ""
                    }
                );
            }
            k::FIELD => match a.get() {
                field_access::INSTANCE | field_access::STATIC | field_access::CLOSURE => {
                    let kind = FIELD_ACCESSES
                        .iter()
                        .find(|(_, v)| *v == a.get())
                        .map_or("?", |(n, _)| *n);
                    let _ = write!(out, " {kind} {}", self.field_text(layer, b));
                }
                field_access::ENUM => {
                    let _ = write!(out, " ENUM {}", self.enum_ctor_text(layer, b));
                }
                other => {
                    let kind = FIELD_ACCESSES
                        .iter()
                        .find(|(_, v)| *v == other)
                        .map_or("?", |(n, _)| *n);
                    let _ = write!(out, " {kind} {:?}", self.string(layer, b));
                }
            },
            k::TYPE_EXPR => {
                let _ = write!(out, " {}", self.module_text(layer, flags, a));
            }
            k::NEW => {
                let _ = write!(out, " {}", self.class_name(layer, a));
            }
            k::CAST => {
                if flags & node_flag::HAS_TYPE != 0 {
                    let _ = write!(out, " {}", self.module_text(layer, flags >> 1, a));
                }
            }
            k::META | k::IDENT | k::OBJECT_FIELD => {
                let _ = write!(out, " {:?}", self.string(layer, a));
            }
            k::ENUM_PARAMETER => {
                let _ = write!(out, " {}[{}]", self.enum_ctor_text(layer, a), b.get());
            }
            k::CASE => {
                let _ = write!(out, " values={}", a.get());
            }
            k::IF | k::RETURN | k::WHILE | k::SWITCH => {
                let _ = write!(out, " flags={flags}");
            }
            _ => {}
        }
        if n.ty.get() != NONE {
            let _ = write!(out, " : {}", self.type_text(layer, n.ty));
        }
        out.push('\n');
        if n.kind == k::FUNCTION {
            self.function(out, locals, layer, a.get() as usize, depth + 1);
        }
        for kid in tast.kid_ids(n) {
            self.node(out, locals, layer, kid, depth + 1);
        }
    }
}
