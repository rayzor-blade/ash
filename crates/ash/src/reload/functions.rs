//! Renumbering a reloaded program onto the functions of the running one.
//!
//! Compiled code, vtables, closures and the interpreter's own tables name a
//! function by its findex, a position that moves when a closure is added
//! ahead of it. The running program's numbering only grows instead. A
//! function the new program shares with it keeps its findex and its place in
//! `functions`, one the new program adds takes the next free findex, and one
//! the new program drops stays where it is, so a closure that already exists
//! keeps its code.
//!
//! Functions are matched by what survives an edit. A method or a bound static
//! has its name from the type table. A closure has none, so it is matched
//! from its parent: by body shape first, then by creation order among the
//! siblings that are left.

use crate::bytecode::{DecodedBytecode, H32, hash_bytes};
use crate::opcodes::{Opcode, RefFun};
use crate::types::{HLFunction, HLNative, HashSkips, function_names};
use std::collections::{HashMap, HashSet, VecDeque};

/// What renumbering a program did besides moving numbers.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct Renumbered {
    /// Functions and natives the new program has and the running one did not.
    pub added: usize,
    /// Functions the new program dropped; their bodies stay in the table.
    pub kept: usize,
}

/// The findex of the function an op calls or makes a closure of.
fn fun_ref(op: &Opcode) -> Option<usize> {
    match op {
        Opcode::Call0 { fun, .. }
        | Opcode::Call1 { fun, .. }
        | Opcode::Call2 { fun, .. }
        | Opcode::Call3 { fun, .. }
        | Opcode::Call4 { fun, .. }
        | Opcode::CallN { fun, .. }
        | Opcode::IndirectCall { fun, .. }
        | Opcode::StaticClosure { fun, .. }
        | Opcode::InstanceClosure { fun, .. } => Some(fun.0),
        _ => None,
    }
}

fn fun_ref_mut(op: &mut Opcode) -> Option<&mut RefFun> {
    match op {
        Opcode::Call0 { fun, .. }
        | Opcode::Call1 { fun, .. }
        | Opcode::Call2 { fun, .. }
        | Opcode::Call3 { fun, .. }
        | Opcode::Call4 { fun, .. }
        | Opcode::CallN { fun, .. }
        | Opcode::IndirectCall { fun, .. }
        | Opcode::StaticClosure { fun, .. }
        | Opcode::InstanceClosure { fun, .. } => Some(fun),
        _ => None,
    }
}

/// One program as the matching reads it.
struct Side<'a> {
    bc: &'a DecodedBytecode,
    /// findex to position in `bc.functions`.
    at: HashMap<usize, usize>,
    /// The name the type table gives a function.
    names: HashMap<usize, String>,
    /// `lib@name` of a native, by findex.
    natives: HashMap<usize, String>,
    /// The functions with no name that each function reaches, in the order
    /// it first reaches them.
    children: HashMap<usize, Vec<usize>>,
}

impl<'a> Side<'a> {
    fn new(bc: &'a DecodedBytecode) -> Self {
        let at: HashMap<usize, usize> = bc
            .functions
            .iter()
            .enumerate()
            .map(|(i, f)| (f.findex as usize, i))
            .collect();
        let names: HashMap<usize, String> = function_names(&bc.types)
            .into_iter()
            .map(|(findex, name)| (findex as usize, name))
            .filter(|(findex, _)| at.contains_key(findex))
            .collect();
        let natives = bc
            .natives
            .iter()
            .map(|n| (n.findex as usize, format!("{}@{}", n.lib, n.name)))
            .collect();
        let children = bc
            .functions
            .iter()
            .map(|f| {
                let mut reached = Vec::new();
                for op in f.ops() {
                    let Some(r) = fun_ref(op) else { continue };
                    if r != f.findex as usize
                        && at.contains_key(&r)
                        && !names.contains_key(&r)
                        && !reached.contains(&r)
                    {
                        reached.push(r);
                    }
                }
                (f.findex as usize, reached)
            })
            .collect();
        Side {
            bc,
            at,
            names,
            natives,
            children,
        }
    }

    fn function(&self, findex: usize) -> &'a HLFunction {
        &self.bc.functions[self.at[&findex]]
    }

    /// The functions with no name, in table order.
    fn unnamed(&self) -> impl Iterator<Item = usize> + '_ {
        self.bc
            .functions
            .iter()
            .map(|f| f.findex as usize)
            .filter(|findex| !self.names.contains_key(findex))
    }

    /// A hash of the body that does not move when a table grows: the pool
    /// entries by content and the functions it names by name. A closure it
    /// reaches counts as a closure and no more; which one is for the tree to
    /// settle.
    fn shape(&self, findex: usize) -> u32 {
        let bc = self.bc;
        let f = self.function(findex);
        let skip = HashSkips {
            pools: true,
            funs: true,
            globals: false,
        };
        let mut h = f.compute_hash_skipping(skip);
        for op in f.ops() {
            match op {
                Opcode::Int { ptr, .. } => {
                    if let Some(v) = bc.ints.get(ptr.0) {
                        h = H32(h, *v as u32);
                    }
                }
                Opcode::Float { ptr, .. } => {
                    if let Some(v) = bc.floats.get(ptr.0) {
                        let bits = v.to_bits();
                        h = H32(H32(h, bits as u32), (bits >> 32) as u32);
                    }
                }
                Opcode::String { ptr, .. } => {
                    if let Some(s) = bc.strings.get(ptr.0) {
                        h = hash_bytes(h, s.as_bytes());
                    }
                }
                Opcode::DynGet { field, .. } | Opcode::DynSet { field, .. } => {
                    if let Some(s) = bc.strings.get(field.0) {
                        h = hash_bytes(h, s.as_bytes());
                    }
                }
                Opcode::Bytes { ptr, .. } => h = hash_bytes(h, bc.bytes_entry(ptr.0)),
                op => {
                    if let Some(r) = fun_ref(op) {
                        h = match self.names.get(&r).or_else(|| self.natives.get(&r)) {
                            Some(name) => hash_bytes(h, name.as_bytes()),
                            None => H32(h, 0x636c),
                        };
                    }
                }
            }
        }
        h
    }
}

/// Pairs the functions of the new program with those of the running one.
struct Matcher<'a> {
    old: &'a Side<'a>,
    new: &'a Side<'a>,
    old_shapes: HashMap<usize, u32>,
    new_shapes: HashMap<usize, u32>,
    /// New findex to running findex.
    to_old: HashMap<usize, usize>,
    taken: HashSet<usize>,
}

impl<'a> Matcher<'a> {
    fn new(old: &'a Side<'a>, new: &'a Side<'a>) -> Self {
        Matcher {
            old_shapes: old.unnamed().map(|f| (f, old.shape(f))).collect(),
            new_shapes: new.unnamed().map(|f| (f, new.shape(f))).collect(),
            old,
            new,
            to_old: HashMap::new(),
            taken: HashSet::new(),
        }
    }

    fn pair(&mut self, new: usize, old: usize) {
        self.to_old.insert(new, old);
        self.taken.insert(old);
    }

    /// Whether the two take and return the same types, which is what a
    /// closure that already exists relies on.
    fn same_signature(&self, new: usize, old: usize) -> bool {
        self.new.function(new).type_.0 == self.old.function(old).type_.0
    }

    /// Methods and bound statics, by name. A name only one program has is a
    /// method added or removed, which moves vtable slots and is not handled.
    fn pair_named(&mut self) -> Result<(), String> {
        fn by_name(names: &HashMap<usize, String>) -> HashMap<&str, Vec<usize>> {
            let mut out: HashMap<&str, Vec<usize>> = HashMap::new();
            for (findex, name) in names {
                out.entry(name.as_str()).or_default().push(*findex);
            }
            for findexes in out.values_mut() {
                findexes.sort_unstable();
            }
            out
        }
        let (old_side, new_side) = (self.old, self.new);
        let (old, new) = (by_name(&old_side.names), by_name(&new_side.names));
        let mut order: Vec<&str> = new.keys().copied().collect();
        order.sort_unstable();
        for name in &order {
            let Some(olds) = old.get(name) else {
                return Err(format!(
                    "{name} is in the new program and not in the running one; a method was added"
                ));
            };
            if olds.len() != new[name].len() {
                return Err(format!(
                    "{name} names {} functions in the new program and {} in the running one",
                    new[name].len(),
                    olds.len()
                ));
            }
        }
        if let Some(name) = old.keys().filter(|n| !new.contains_key(*n)).min() {
            return Err(format!(
                "{name} is in the running program and not in the new one; a method was removed"
            ));
        }
        let mut pairs = Vec::new();
        for name in &order {
            for (&n, &o) in new[name].iter().zip(&old[name]) {
                self.pair(n, o);
                pairs.push((o, n));
            }
        }
        for (o, n) in pairs {
            self.pair_children(o, n);
        }
        Ok(())
    }

    /// The closures a matched pair makes or calls: those with the same shape,
    /// then, when both sides have as many left and each pair takes the same
    /// types, the rest in creation order. Then the closures of each pair made.
    fn pair_children(&mut self, old: usize, new: usize) {
        let mut work = vec![(old, new)];
        while let Some((o, n)) = work.pop() {
            let mut left_old: Vec<usize> = self.old.children[&o]
                .iter()
                .copied()
                .filter(|c| !self.taken.contains(c))
                .collect();
            let mut left_new: Vec<usize> = self.new.children[&n]
                .iter()
                .copied()
                .filter(|c| !self.to_old.contains_key(c))
                .collect();
            let mut made = Vec::new();
            let mut unmatched = Vec::new();
            for c in left_new {
                let hit = left_old.iter().position(|&d| {
                    self.old_shapes[&d] == self.new_shapes[&c] && self.same_signature(c, d)
                });
                match hit {
                    Some(i) => {
                        let d = left_old.remove(i);
                        self.pair(c, d);
                        made.push((d, c));
                    }
                    None => unmatched.push(c),
                }
            }
            left_new = unmatched;
            if left_old.len() == left_new.len()
                && left_old
                    .iter()
                    .zip(&left_new)
                    .all(|(&d, &c)| self.same_signature(c, d))
            {
                for (d, c) in left_old.into_iter().zip(left_new) {
                    self.pair(c, d);
                    made.push((d, c));
                }
            }
            work.extend(made);
        }
    }

    /// The entry function has no name; the two programs' are the same one.
    fn pair_entry(&mut self) {
        let old = self.old.bc.entrypoint as usize;
        let new = self.new.bc.entrypoint as usize;
        if self.old.at.contains_key(&old)
            && self.new.at.contains_key(&new)
            && !self.taken.contains(&old)
            && !self.to_old.contains_key(&new)
            && self.same_signature(new, old)
        {
            self.pair(new, old);
            self.pair_children(old, new);
        }
    }

    /// What is left on both sides, by shape: a closure that moved to another
    /// parent. Bodies of one shape do the same work, so the order within a
    /// shape does not matter.
    fn pair_by_shape(&mut self) {
        let mut keys: Vec<(u32, usize)> = Vec::new();
        let mut olds: HashMap<(u32, usize), Vec<usize>> = HashMap::new();
        let mut news: HashMap<(u32, usize), Vec<usize>> = HashMap::new();
        for f in self.old.unnamed().filter(|f| !self.taken.contains(f)) {
            let key = (self.old_shapes[&f], self.old.function(f).type_.0);
            olds.entry(key).or_default().push(f);
        }
        for f in self.new.unnamed().filter(|f| !self.to_old.contains_key(f)) {
            let key = (self.new_shapes[&f], self.new.function(f).type_.0);
            if !keys.contains(&key) {
                keys.push(key);
            }
            news.entry(key).or_default().push(f);
        }
        let mut made = Vec::new();
        for key in keys {
            let Some(old) = olds.get(&key) else { continue };
            for (&o, &n) in old.iter().zip(&news[&key]) {
                self.pair(n, o);
                made.push((o, n));
            }
        }
        for (o, n) in made {
            self.pair_children(o, n);
        }
    }
}

/// Where every findex of the new program lands in the running numbering.
struct Numbering {
    to_running: HashMap<usize, usize>,
    added: usize,
}

fn plan(old: &DecodedBytecode, new: &DecodedBytecode, slots: usize) -> Result<Numbering, String> {
    let (old_side, new_side) = (Side::new(old), Side::new(new));
    let mut m = Matcher::new(&old_side, &new_side);
    m.pair_named()?;
    m.pair_entry();
    m.pair_by_shape();

    let mut top = old
        .functions
        .iter()
        .map(|f| f.findex)
        .chain(old.natives.iter().map(|n| n.findex))
        .max()
        .map_or(0, |max| max as usize + 1);
    let mut to_running = HashMap::new();
    let mut added = 0;
    for f in &new.functions {
        let findex = f.findex as usize;
        match m.to_old.get(&findex) {
            Some(&running) => to_running.insert(findex, running),
            None => {
                added += 1;
                top += 1;
                to_running.insert(findex, top - 1)
            }
        };
    }
    // A native is declared once per signature it is used with, so a name can
    // have several findexes; they pair up in table order.
    let mut running_natives: HashMap<(&str, &str, usize), VecDeque<usize>> = HashMap::new();
    for n in &old.natives {
        running_natives
            .entry((n.lib.as_str(), n.name.as_str(), n.type_.0))
            .or_default()
            .push_back(n.findex as usize);
    }
    for n in &new.natives {
        let findex = n.findex as usize;
        let key = (n.lib.as_str(), n.name.as_str(), n.type_.0);
        match running_natives.get_mut(&key).and_then(VecDeque::pop_front) {
            Some(running) => to_running.insert(findex, running),
            None => {
                added += 1;
                top += 1;
                to_running.insert(findex, top - 1)
            }
        };
    }
    if top > slots {
        return Err(format!(
            "the new program needs {top} function slots and the running one has {slots}; restart it"
        ));
    }
    Ok(Numbering { to_running, added })
}

/// The index of the entry of `pool` that `same` accepts, adding `value` if
/// none does.
fn pool_index<T>(pool: &mut Vec<T>, same: impl Fn(&T) -> bool, value: T) -> usize {
    pool.iter().position(same).unwrap_or_else(|| {
        pool.push(value);
        pool.len() - 1
    })
}

/// A function of the running program with its pool indices and debug files
/// named in the new program's tables, which hold what it names or get it.
fn rehome(mut f: HLFunction, old: &DecodedBytecode, new: &mut DecodedBytecode) -> HLFunction {
    for op in f.ops_mut() {
        match op {
            Opcode::Int { ptr, .. } => {
                if let Some(&v) = old.ints.get(ptr.0) {
                    ptr.0 = pool_index(&mut new.ints, |x| *x == v, v);
                }
            }
            Opcode::Float { ptr, .. } => {
                if let Some(&v) = old.floats.get(ptr.0) {
                    ptr.0 = pool_index(&mut new.floats, |x| x.to_bits() == v.to_bits(), v);
                }
            }
            Opcode::String { ptr, .. } => {
                if let Some(s) = old.strings.get(ptr.0) {
                    ptr.0 = pool_index(&mut new.strings, |x| x == s, s.clone());
                }
            }
            Opcode::DynGet { field, .. } | Opcode::DynSet { field, .. } => {
                if let Some(s) = old.strings.get(field.0) {
                    field.0 = pool_index(&mut new.strings, |x| x == s, s.clone());
                }
            }
            Opcode::Bytes { ptr, .. } => {
                let data = old.bytes_entry(ptr.0).to_vec();
                ptr.0 = (0..new.bytes_pos.len())
                    .find(|&i| new.bytes_entry(i) == data.as_slice())
                    .unwrap_or_else(|| {
                        new.bytes_pos.push(new.bytes_data.len());
                        new.bytes_data.extend_from_slice(&data);
                        new.bytes_pos.len() - 1
                    });
            }
            _ => {}
        }
    }
    if !f.debug().is_empty() {
        let mut files: HashMap<i32, i32> = HashMap::new();
        let debug: Vec<i32> = f
            .debug()
            .chunks_exact(2)
            .flat_map(|pair| {
                let file = *files.entry(pair[0]).or_insert_with(|| {
                    match usize::try_from(pair[0])
                        .ok()
                        .and_then(|i| old.debug_files.get(i))
                    {
                        Some(name) => {
                            pool_index(&mut new.debug_files, |x| x == name, name.clone()) as i32
                        }
                        None => pair[0],
                    }
                });
                [file, pair[1]]
            })
            .collect();
        f.set_debug(debug);
    }
    f
}

/// Put `new` on the running program's numbering and positions.
fn apply(old: &DecodedBytecode, new: &mut DecodedBytecode, numbering: &Numbering) -> Renumbered {
    let at = |findex: usize| numbering.to_running.get(&findex).copied().unwrap_or(findex);
    for f in &mut new.functions {
        if f.ops().iter().filter_map(fun_ref).any(|r| at(r) != r) {
            for op in f.ops_mut() {
                if let Some(r) = fun_ref_mut(op) {
                    r.0 = at(r.0);
                }
            }
        }
        f.findex = at(f.findex as usize) as i32;
    }
    for n in &mut new.natives {
        n.findex = at(n.findex as usize) as i32;
    }
    for t in &mut new.types {
        let Some(obj) = t.obj.as_mut() else { continue };
        for p in &mut obj.proto {
            if p.findex >= 0 {
                p.findex = at(p.findex as usize) as i32;
            }
        }
        for binding in obj.bindings.chunks_exact_mut(2) {
            if binding[1] >= 0 {
                binding[1] = at(binding[1] as usize) as i32;
            }
        }
    }
    new.entrypoint = at(new.entrypoint as usize) as u32;

    // A function sits where the running program has it, and a dropped one
    // stays, with the pools of the new program behind it.
    let position: HashMap<usize, usize> = old
        .functions
        .iter()
        .enumerate()
        .map(|(i, f)| (f.findex as usize, i))
        .collect();
    let mut placed: Vec<Option<HLFunction>> = (0..old.functions.len()).map(|_| None).collect();
    let mut appended = Vec::new();
    for f in std::mem::take(&mut new.functions) {
        match position.get(&(f.findex as usize)) {
            Some(&p) => placed[p] = Some(f),
            None => appended.push(f),
        }
    }
    appended.sort_by_key(|f| f.findex);
    let mut kept = 0;
    let mut functions = Vec::with_capacity(placed.len() + appended.len());
    for (p, slot) in placed.into_iter().enumerate() {
        functions.push(match slot {
            Some(f) => f,
            None => {
                kept += 1;
                rehome(old.functions[p].clone(), old, new)
            }
        });
    }
    functions.extend(appended);
    new.functions = functions;

    let position: HashMap<usize, usize> = old
        .natives
        .iter()
        .enumerate()
        .map(|(i, n)| (n.findex as usize, i))
        .collect();
    let mut placed: Vec<Option<HLNative>> = (0..old.natives.len()).map(|_| None).collect();
    let mut appended = Vec::new();
    for n in std::mem::take(&mut new.natives) {
        match position.get(&(n.findex as usize)) {
            Some(&p) => placed[p] = Some(n),
            None => appended.push(n),
        }
    }
    appended.sort_by_key(|n| n.findex);
    new.natives = placed
        .into_iter()
        .enumerate()
        .map(|(p, n)| n.unwrap_or_else(|| old.natives[p].clone()))
        .chain(appended)
        .collect();

    new.body_facts = std::sync::OnceLock::new();
    Renumbered {
        added: numbering.added,
        kept,
    }
}

/// Renumber `new` onto the functions and natives of `old`, the program that
/// is running, so that a findex means the same function in both. `slots` is
/// how many findexes the running tables have room for. `Err` says why the
/// new program cannot be put on the running one's numbering.
pub fn remap_functions(
    old: &DecodedBytecode,
    new: &mut DecodedBytecode,
    slots: usize,
) -> Result<Renumbered, String> {
    let numbering = plan(old, new, slots)?;
    Ok(apply(old, new, &numbering))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hl;
    use crate::opcodes::{RefBytes, RefFloat, RefInt, RefString, Reg};
    use crate::reload::diff_bytecode;
    use crate::types::{HLObjField, HLObjProto, HLType, HLTypeObj, TypeRef};

    fn function(findex: i32, ops: Vec<Opcode>) -> HLFunction {
        let mut f = HLFunction::with_body(ops, Vec::new());
        f.findex = findex;
        f
    }

    fn make(fun: usize) -> Opcode {
        Opcode::StaticClosure {
            dst: Reg(0),
            fun: RefFun(fun),
        }
    }

    fn call(fun: usize) -> Opcode {
        Opcode::Call0 {
            dst: Reg(0),
            fun: RefFun(fun),
        }
    }

    /// A body that returns `ints[at]`.
    fn gives(at: usize) -> Vec<Opcode> {
        vec![
            Opcode::Int {
                dst: Reg(0),
                ptr: RefInt(at),
            },
            Opcode::Ret { ret: Reg(0) },
        ]
    }

    /// A body that returns `strings[at]`.
    fn says(at: usize) -> Vec<Opcode> {
        vec![
            Opcode::String {
                dst: Reg(0),
                ptr: RefString(at),
            },
            Opcode::Ret { ret: Reg(0) },
        ]
    }

    /// A program whose class `Main` declares `methods` as `(name, findex)`.
    fn program(
        methods: &[(&str, i32)],
        functions: Vec<HLFunction>,
        natives: &[(&str, &str, i32)],
        entry: u32,
    ) -> DecodedBytecode {
        let main = HLTypeObj {
            name: "Main".into(),
            proto: methods
                .iter()
                .enumerate()
                .map(|(i, (name, findex))| HLObjProto {
                    name: name.to_string(),
                    findex: *findex,
                    pindex: i as i32,
                    hashed_name: 0,
                })
                .collect(),
            ..Default::default()
        };
        DecodedBytecode {
            types: vec![
                HLType {
                    kind: hl::hl_type_kind_HVOID,
                    ..Default::default()
                },
                HLType {
                    kind: hl::hl_type_kind_HOBJ,
                    obj: Some(main),
                    ..Default::default()
                },
            ],
            functions,
            natives: natives
                .iter()
                .map(|(lib, name, findex)| HLNative {
                    lib: lib.to_string(),
                    name: name.to_string(),
                    findex: *findex,
                    type_: TypeRef(0),
                })
                .collect(),
            entrypoint: entry,
            ..Default::default()
        }
    }

    fn with_ints(mut bc: DecodedBytecode, ints: &[i32]) -> DecodedBytecode {
        bc.ints = ints.to_vec();
        bc
    }

    fn with_strings(mut bc: DecodedBytecode, strings: &[&str]) -> DecodedBytecode {
        bc.strings = strings.iter().map(|s| s.to_string()).collect();
        bc
    }

    fn findexes(bc: &DecodedBytecode) -> Vec<i32> {
        bc.functions.iter().map(|f| f.findex).collect()
    }

    fn made(f: &HLFunction) -> Vec<usize> {
        f.ops().iter().filter_map(fun_ref).collect()
    }

    fn sorted(mut v: Vec<usize>) -> Vec<usize> {
        v.sort_unstable();
        v
    }

    fn int_of(bc: &DecodedBytecode, findex: i32) -> i32 {
        let f = bc.functions.iter().find(|f| f.findex == findex).unwrap();
        match &f.ops()[0] {
            Opcode::Int { ptr, .. } => bc.ints[ptr.0],
            op => panic!("{op:?}"),
        }
    }

    const ROOM: usize = usize::MAX;

    /// A closure added in the middle of a method's closures takes a findex
    /// of its own, and the ones after it keep theirs.
    #[test]
    fn a_closure_added_between_two_others_shifts_neither() {
        let old = with_ints(
            program(
                &[("main", 0)],
                vec![
                    function(0, vec![make(1), make(2)]),
                    function(1, gives(0)),
                    function(2, gives(1)),
                ],
                &[],
                0,
            ),
            &[11, 22],
        );
        let mut new = with_ints(
            program(
                &[("main", 0)],
                vec![
                    function(0, vec![make(1), make(2), make(3)]),
                    function(1, gives(0)),
                    function(2, gives(2)),
                    function(3, gives(1)),
                ],
                &[],
                0,
            ),
            &[11, 22, 99],
        );
        let done = remap_functions(&old, &mut new, ROOM).unwrap();
        assert_eq!(done, Renumbered { added: 1, kept: 0 });
        assert_eq!(findexes(&new), vec![0, 1, 2, 3]);
        assert_eq!(made(&new.functions[0]), vec![1, 3, 2]);
        assert_eq!(int_of(&new, 2), 22);
        assert_eq!(int_of(&new, 3), 99);

        let diff = diff_bytecode(&old, &new);
        assert_eq!(diff.changed, vec![0]);
        assert_eq!(diff.added, vec![3]);
        assert!(diff.removed.is_empty());
    }

    /// A closure the new program drops stays in the table, and the strings
    /// it reads are in the new program's pool.
    #[test]
    fn a_dropped_closure_stays_and_reads_its_own_strings() {
        let old = with_strings(
            program(
                &[("main", 0)],
                vec![
                    function(0, vec![make(1), make(2)]),
                    function(1, says(0)),
                    function(2, says(1)),
                ],
                &[],
                0,
            ),
            &["gone", "stay"],
        );
        let mut new = with_strings(
            program(
                &[("main", 0)],
                vec![function(0, vec![make(1)]), function(1, says(0))],
                &[],
                0,
            ),
            &["stay"],
        );
        let done = remap_functions(&old, &mut new, ROOM).unwrap();
        assert_eq!(done, Renumbered { added: 0, kept: 1 });
        assert_eq!(findexes(&new), vec![0, 1, 2]);
        assert_eq!(made(&new.functions[0]), vec![2]);
        let Opcode::String { ptr, .. } = &new.functions[1].ops()[0] else {
            panic!()
        };
        assert_eq!(new.strings[ptr.0], "gone");
        let Opcode::String { ptr, .. } = &new.functions[2].ops()[0] else {
            panic!()
        };
        assert_eq!(new.strings[ptr.0], "stay");

        let diff = diff_bytecode(&old, &new);
        assert_eq!(diff.changed, vec![0]);
        assert!(diff.added.is_empty() && diff.removed.is_empty());
    }

    /// A closure whose body was edited is the same closure: same findex, and
    /// the only function that changed.
    #[test]
    fn an_edited_closure_keeps_its_findex() {
        let old = with_ints(
            program(
                &[("main", 0)],
                vec![function(0, vec![make(1)]), function(1, gives(0))],
                &[],
                0,
            ),
            &[11],
        );
        let mut new = with_ints(
            program(
                &[("main", 0)],
                vec![function(0, vec![make(1)]), function(1, gives(0))],
                &[],
                0,
            ),
            &[12],
        );
        let done = remap_functions(&old, &mut new, ROOM).unwrap();
        assert_eq!(done, Renumbered::default());
        assert_eq!(diff_bytecode(&old, &new).changed, vec![1]);
    }

    /// Closures of one body are interchangeable: a third copy is the added
    /// one.
    #[test]
    fn identical_closures_pair_up_and_the_extra_one_is_added() {
        let old = with_ints(
            program(
                &[("main", 0)],
                vec![
                    function(0, vec![make(1), make(2)]),
                    function(1, gives(0)),
                    function(2, gives(0)),
                ],
                &[],
                0,
            ),
            &[5],
        );
        let mut new = with_ints(
            program(
                &[("main", 0)],
                vec![
                    function(0, vec![make(1), make(2), make(3)]),
                    function(1, gives(0)),
                    function(2, gives(0)),
                    function(3, gives(0)),
                ],
                &[],
                0,
            ),
            &[5],
        );
        let done = remap_functions(&old, &mut new, ROOM).unwrap();
        assert_eq!(done, Renumbered { added: 1, kept: 0 });
        assert_eq!(sorted(made(&new.functions[0])), vec![1, 2, 3]);
    }

    /// A closure made inside a closure follows its parent when something is
    /// added ahead of it.
    #[test]
    fn a_nested_closure_follows_its_parent() {
        let old = with_ints(
            program(
                &[("main", 0)],
                vec![
                    function(0, vec![make(1)]),
                    function(1, vec![make(2)]),
                    function(2, gives(0)),
                ],
                &[],
                0,
            ),
            &[7],
        );
        let mut new = with_ints(
            program(
                &[("main", 0)],
                vec![
                    function(0, vec![make(1), make(2)]),
                    function(1, gives(1)),
                    function(2, vec![make(3)]),
                    function(3, gives(0)),
                ],
                &[],
                0,
            ),
            &[7, 8],
        );
        let done = remap_functions(&old, &mut new, ROOM).unwrap();
        assert_eq!(done, Renumbered { added: 1, kept: 0 });
        // The new closure takes findex 3; the old pair keep 1 and 2.
        assert_eq!(made(&new.functions[0]), vec![3, 1]);
        assert_eq!(made(&new.functions[1]), vec![2]);
        assert_eq!(int_of(&new, 3), 8);
        assert_eq!(diff_bytecode(&old, &new).changed, vec![0]);
    }

    /// A closure made by another method now is still the same closure.
    #[test]
    fn a_closure_that_moves_to_another_method_is_matched_by_shape() {
        let old = with_ints(
            program(
                &[("main", 0), ("other", 1)],
                vec![
                    function(0, vec![make(2)]),
                    function(1, vec![]),
                    function(2, gives(0)),
                ],
                &[],
                0,
            ),
            &[3],
        );
        let mut new = with_ints(
            program(
                &[("main", 0), ("other", 1)],
                vec![
                    function(0, vec![]),
                    function(1, vec![make(2)]),
                    function(2, gives(0)),
                ],
                &[],
                0,
            ),
            &[3],
        );
        let done = remap_functions(&old, &mut new, ROOM).unwrap();
        assert_eq!(done, Renumbered::default());
        assert_eq!(made(&new.functions[1]), vec![2]);
    }

    /// The entry function has no name and is matched as the entry.
    #[test]
    fn the_entry_function_is_matched_as_the_entry() {
        let old = program(
            &[("main", 0)],
            vec![function(0, vec![]), function(1, vec![call(0)])],
            &[],
            1,
        );
        let mut new = program(
            &[("main", 0)],
            vec![function(0, vec![]), function(1, vec![call(0), call(0)])],
            &[],
            1,
        );
        let done = remap_functions(&old, &mut new, ROOM).unwrap();
        assert_eq!(done, Renumbered::default());
        assert_eq!(new.entrypoint, 1);
        assert_eq!(diff_bytecode(&old, &new).changed, vec![1]);
    }

    /// A program with the same functions comes out as it went in.
    #[test]
    fn the_same_program_is_left_alone() {
        let build = || {
            with_ints(
                program(
                    &[("main", 0)],
                    vec![function(0, vec![make(1)]), function(1, gives(0))],
                    &[("std", "sin", 2)],
                    0,
                ),
                &[4],
            )
        };
        let old = build();
        let mut new = build();
        assert_eq!(
            remap_functions(&old, &mut new, ROOM).unwrap(),
            Renumbered::default()
        );
        assert!(!diff_bytecode(&old, &new).has_changes());
    }

    #[test]
    fn a_method_added_or_removed_is_refused() {
        let one = || program(&[("main", 0)], vec![function(0, vec![])], &[], 0);
        let two = || {
            program(
                &[("main", 0), ("extra", 1)],
                vec![function(0, vec![]), function(1, vec![])],
                &[],
                0,
            )
        };
        let why = remap_functions(&one(), &mut two(), ROOM).unwrap_err();
        assert!(why.contains("Main.extra") && why.contains("a method was added"), "{why}");
        let why = remap_functions(&two(), &mut one(), ROOM).unwrap_err();
        assert!(why.contains("Main.extra") && why.contains("a method was removed"), "{why}");
    }

    /// Natives follow their (lib, name) when an addition shifts them, and a
    /// native the program had not used takes a findex past the old ones.
    #[test]
    fn natives_keep_their_findex_and_new_ones_come_after() {
        let old = program(
            &[("main", 0)],
            vec![function(0, vec![call(1)])],
            &[("std", "sin", 1)],
            0,
        );
        let mut new = program(
            &[("main", 0)],
            vec![function(0, vec![call(2), call(3)]), function(1, vec![])],
            &[("std", "sin", 2), ("std", "cos", 3)],
            0,
        );
        // `new` also has an unnamed function at 1; it is a closure nobody makes.
        let done = remap_functions(&old, &mut new, ROOM).unwrap();
        assert_eq!(done.added, 2);
        let sin = new.natives.iter().find(|n| n.name == "sin").unwrap();
        let cos = new.natives.iter().find(|n| n.name == "cos").unwrap();
        assert_eq!(sin.findex, 1);
        assert!(cos.findex > 1);
        assert_eq!(made(&new.functions[0]), vec![1, cos.findex as usize]);
        // The running natives keep their positions; the new one is last.
        assert_eq!(new.natives[0].name, "sin");
        assert_eq!(new.natives.last().unwrap().name, "cos");
    }

    /// A native is declared once per signature, and a name can be declared
    /// twice with one: the two pair up in table order, so the callers of each
    /// keep calling the one they called.
    #[test]
    fn natives_declared_twice_under_one_name_pair_in_table_order() {
        let old = program(
            &[("main", 0)],
            vec![function(0, vec![call(1), call(2)])],
            &[("std", "alloc", 1), ("std", "alloc", 2)],
            0,
        );
        let mut new = program(
            &[("main", 0)],
            vec![function(0, vec![call(5), call(6)]), function(1, vec![])],
            &[("std", "alloc", 5), ("std", "alloc", 6)],
            0,
        );
        remap_functions(&old, &mut new, ROOM).unwrap();
        assert_eq!(made(&new.functions[0]), vec![1, 2]);
        assert_eq!(
            new.natives.iter().map(|n| n.findex).collect::<Vec<_>>(),
            vec![1, 2]
        );
    }

    /// A dropped function that reads a field by name finds the name in the
    /// new program's strings.
    #[test]
    fn a_dropped_function_keeps_the_field_names_it_reads() {
        let get = vec![
            Opcode::DynGet {
                dst: Reg(0),
                obj: Reg(1),
                field: RefString(1),
            },
            Opcode::Ret { ret: Reg(0) },
        ];
        let old = with_strings(
            program(
                &[("main", 0)],
                vec![function(0, vec![make(1)]), function(1, get)],
                &[],
                0,
            ),
            &["", "gone"],
        );
        let mut new = with_strings(
            program(&[("main", 0)], vec![function(0, vec![])], &[], 0),
            &["", "other"],
        );
        remap_functions(&old, &mut new, ROOM).unwrap();
        let Opcode::DynGet { field, .. } = &new.functions[1].ops()[0] else {
            panic!()
        };
        assert_eq!(new.strings[field.0], "gone");
    }

    /// A method and a bound static that sit after a shifted closure keep
    /// the findex the running program has for them, in the type table too.
    #[test]
    fn protos_and_bindings_follow_their_functions() {
        let mut old = with_ints(
            program(
                &[("main", 0), ("helper", 2)],
                vec![
                    function(0, vec![make(1)]),
                    function(1, gives(0)),
                    function(2, vec![]),
                    function(3, vec![]),
                ],
                &[],
                0,
            ),
            &[1, 2],
        );
        let mut new = with_ints(
            program(
                &[("main", 0), ("helper", 3)],
                vec![
                    function(0, vec![make(1), make(2)]),
                    function(1, gives(0)),
                    function(2, gives(1)),
                    function(3, vec![]),
                    function(4, vec![]),
                ],
                &[],
                0,
            ),
            &[1, 2],
        );
        let stat = HLObjField {
            name: "stat".into(),
            ..Default::default()
        };
        for (bc, findex) in [(&mut old, 3), (&mut new, 4)] {
            let main = bc.types[1].obj.as_mut().unwrap();
            main.fields = vec![stat.clone()];
            main.bindings = vec![0, findex];
        }
        let done = remap_functions(&old, &mut new, ROOM).unwrap();
        assert_eq!(done, Renumbered { added: 1, kept: 0 });
        let main = new.types[1].obj.as_ref().unwrap();
        assert_eq!(main.proto[1].findex, 2);
        assert_eq!(main.bindings, vec![0, 3]);
        assert_eq!(made(&new.functions[0]), vec![1, 4]);
    }

    /// A dropped function that reads a float and bytes finds them in the new
    /// program's pools.
    #[test]
    fn a_dropped_function_keeps_its_floats_and_bytes() {
        let body = vec![
            Opcode::Float {
                dst: Reg(0),
                ptr: RefFloat(1),
            },
            Opcode::Bytes {
                dst: Reg(1),
                ptr: RefBytes(1),
            },
            Opcode::Ret { ret: Reg(0) },
        ];
        let mut old = program(
            &[("main", 0)],
            vec![function(0, vec![make(1)]), function(1, body)],
            &[],
            0,
        );
        old.floats = vec![0.5, 2.5];
        old.bytes_pos = vec![0, 2];
        old.bytes_data = vec![1, 2, 3, 4, 5];
        let mut new = program(&[("main", 0)], vec![function(0, vec![])], &[], 0);
        new.floats = vec![9.0];
        new.bytes_pos = vec![0];
        new.bytes_data = vec![7, 7];
        remap_functions(&old, &mut new, ROOM).unwrap();
        let ops = new.functions[1].ops();
        let Opcode::Float { ptr, .. } = &ops[0] else {
            panic!()
        };
        assert_eq!(new.floats[ptr.0], 2.5);
        let Opcode::Bytes { ptr, .. } = &ops[1] else {
            panic!()
        };
        assert_eq!(new.bytes_entry(ptr.0), &[3, 4, 5]);
    }

    /// Reloads follow one another. Each program is put on the numbering of
    /// the one running, a closure one of them drops is taken up again by a
    /// later one, and the tables stop growing.
    #[test]
    fn successive_reloads_reuse_the_slots_of_dropped_closures() {
        crate::native_lib::init_std_library();
        let load = |v: &str| {
            let path = format!(
                "{}/test/hot_reload/closures/{v}.hl",
                env!("CARGO_MANIFEST_DIR")
            );
            crate::bytecode::BytecodeDecoder::decode(std::path::Path::new(&path)).unwrap()
        };
        let mut running = load("v1");
        let mut added = Vec::new();
        let mut sizes = Vec::new();
        for next in ["v2", "v3", "v2", "v4", "v1"] {
            let mut bc = load(next);
            remap_functions(&running, &mut bc, ROOM).unwrap();
            for (at, f) in running.functions.iter().enumerate() {
                assert_eq!(bc.functions[at].findex, f.findex, "{next}");
            }
            added.push(diff_bytecode(&running, &bc).added.len());
            sizes.push(bc.functions.len());
            running = bc;
        }
        assert_eq!(added, vec![1, 0, 0, 0, 0]);
        assert!(sizes.windows(2).all(|w| w[0] == w[1]), "{sizes:?}");
    }

    #[test]
    fn a_program_too_big_for_the_running_tables_is_refused() {
        let old = program(&[("main", 0)], vec![function(0, vec![make(1)]), function(1, vec![])], &[], 0);
        let mut new = program(
            &[("main", 0)],
            vec![function(0, vec![make(1), make(2)]), function(1, vec![]), function(2, gives(0))],
            &[],
            0,
        );
        let why = remap_functions(&old, &mut new, 2).unwrap_err();
        assert!(why.contains("function slots"), "{why}");
    }

    /// A dropped function's source files are named in the new program's
    /// list, and its positions point at them.
    #[test]
    fn a_dropped_function_keeps_its_source_positions() {
        let mut old = program(
            &[("main", 0)],
            vec![function(0, vec![make(1)]), function(1, vec![])],
            &[],
            0,
        );
        old.debug_files = vec!["Old.hx".into()];
        old.functions[1].set_debug(vec![0, 5]);
        let mut new = program(&[("main", 0)], vec![function(0, vec![])], &[], 0);
        new.debug_files = vec!["New.hx".into()];
        remap_functions(&old, &mut new, ROOM).unwrap();
        assert_eq!(new.debug_files, vec!["New.hx", "Old.hx"]);
        assert_eq!(new.functions[1].debug(), &[1, 5]);
    }
}
