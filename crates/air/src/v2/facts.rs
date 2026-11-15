//! What a front end knows about the program that the instructions do not say.
//!
//! Bytecode names a callee and nothing about it, so every call is a barrier to every load. A
//! front end that sees the whole program can say what a callee may write, and the passes then
//! move and reuse loads across calls that cannot touch them. The facts are keyed by program
//! entity rather than by instruction, so they survive the passes that move instructions, and
//! they are empty for a function lowered from bytecode: an absent fact means "unknown", which
//! every query reads as the old, conservative answer.

use std::collections::{HashMap, HashSet};

use super::analysis::{AliasClass, write_class};
use super::ir::{Function, Instr};

/// Most distinct classes a footprint names before it means "anything".
const MAX_CLASSES: usize = 32;

/// The storage a call may write: a set of alias classes, or anything.
///
/// A field is named by the object type the writing code used, as an instruction names it, so a
/// summary is only as precise as the type naming of the lowering that produced the code.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, Default)]
pub struct Footprint {
    classes: Vec<AliasClass>,
}

/// Equal as sets: the order classes were added in is not part of the footprint.
impl PartialEq for Footprint {
    fn eq(&self, other: &Footprint) -> bool {
        self.classes.len() == other.classes.len()
            && self.classes.iter().all(|c| other.classes.contains(c))
    }
}

impl Eq for Footprint {}

impl Footprint {
    /// Writes nothing.
    pub fn none() -> Footprint {
        Footprint::default()
    }

    /// Writes anything.
    pub fn any() -> Footprint {
        Footprint {
            classes: vec![AliasClass::Any],
        }
    }

    pub fn of(classes: &[AliasClass]) -> Footprint {
        let mut fp = Footprint::none();
        for c in classes {
            fp.add(*c);
        }
        fp
    }

    pub fn is_any(&self) -> bool {
        self.classes.contains(&AliasClass::Any)
    }

    pub fn is_none(&self) -> bool {
        self.classes.is_empty()
    }

    pub fn classes(&self) -> &[AliasClass] {
        &self.classes
    }

    /// True when a write in this footprint may land on storage `class` names.
    pub fn may_alias(&self, class: AliasClass) -> bool {
        self.classes.iter().any(|c| c.may_alias(class))
    }

    /// Adds a class; true when the footprint grew.
    pub fn add(&mut self, class: AliasClass) -> bool {
        if self.is_any() {
            return false;
        }
        if class == AliasClass::Any {
            *self = Footprint::any();
            return true;
        }
        if self.classes.contains(&class) {
            return false;
        }
        if self.classes.len() == MAX_CLASSES {
            *self = Footprint::any();
        } else {
            self.classes.push(class);
        }
        true
    }

    /// Adds every class of `other`; true when the footprint grew.
    pub fn join(&mut self, other: &Footprint) -> bool {
        let mut grew = false;
        for c in &other.classes {
            grew |= self.add(*c);
        }
        grew
    }
}

/// What a call may do, as far as moving and reusing loads is concerned.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct CallSummary {
    /// Storage the callee, or anything it reaches, may write.
    pub writes: Footprint,
}

/// The facts a function carries, as the front end that lowered it learned them.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Facts {
    /// What a direct call to each function may do, by callee findex.
    pub calls: HashMap<usize, CallSummary>,
}

impl Facts {
    pub fn is_empty(&self) -> bool {
        self.calls.is_empty()
    }

    /// Storage retained beyond the struct itself, rounded up.
    pub fn allocated_bytes(&self) -> usize {
        let per = std::mem::size_of::<(usize, CallSummary)>() + 8 + MAX_CLASSES / 2 * 16;
        self.calls.capacity() * per
    }

    /// The summary of a direct call, when the front end gave one.
    pub fn call(&self, findex: usize) -> Option<&CallSummary> {
        self.calls.get(&findex)
    }

    /// Takes in the facts of a body copied into this function. A callee both sides know is
    /// the same function, so the wider footprint stands if they ever differ.
    pub fn merge(&mut self, other: &Facts) {
        for (findex, s) in &other.calls {
            self.calls
                .entry(*findex)
                .and_modify(|mine| {
                    mine.writes.join(&s.writes);
                })
                .or_insert_with(|| s.clone());
        }
    }
}

/// What each of `fns` may write, following direct calls between them to a fixed point.
///
/// A call to a function outside the set is a write to anything unless the caller already
/// carries a summary for it, and so is a method or closure call: nothing here says which body
/// runs. A callee's own cells are its frame's, so they never appear in what it writes.
pub fn summarize<'a>(
    fns: impl IntoIterator<Item = (usize, &'a Function)>,
) -> HashMap<usize, CallSummary> {
    let fns: Vec<(usize, &Function)> = fns.into_iter().collect();
    let index: HashMap<usize, usize> = fns.iter().enumerate().map(|(i, (f, _))| (*f, i)).collect();

    let mut own: Vec<Footprint> = Vec::with_capacity(fns.len());
    let mut callees: Vec<Vec<usize>> = Vec::with_capacity(fns.len());
    for (_, f) in &fns {
        let mut fp = Footprint::none();
        let mut calls = Vec::new();
        for ins in f.blocks.iter().flat_map(|b| b.instrs.iter()) {
            match ins {
                Instr::Call { fun, .. } => {
                    if let Some(&i) = index.get(fun) {
                        calls.push(i);
                    } else if let Some(s) = f.facts.call(*fun) {
                        fp.join(&s.writes);
                    } else {
                        fp.add(AliasClass::Any);
                    }
                }
                Instr::CallMethod { .. } | Instr::CallClosure { .. } => {
                    fp.add(AliasClass::Any);
                }
                _ => match write_class(ins) {
                    Some(AliasClass::Cell(_)) | None => {}
                    Some(c) => {
                        fp.add(c);
                    }
                },
            }
        }
        calls.sort_unstable();
        calls.dedup();
        own.push(fp);
        callees.push(calls);
    }

    let mut callers: Vec<Vec<usize>> = vec![Vec::new(); fns.len()];
    for (f, cs) in callees.iter().enumerate() {
        for &c in cs {
            callers[c].push(f);
        }
    }

    let mut writes = own.clone();
    let mut queued: HashSet<usize> = (0..fns.len()).collect();
    let mut work: Vec<usize> = (0..fns.len()).rev().collect();
    while let Some(f) = work.pop() {
        queued.remove(&f);
        let mut now = own[f].clone();
        for &c in &callees[f] {
            now.join(&writes[c]);
        }
        if now != writes[f] {
            writes[f] = now;
            for &caller in &callers[f] {
                if queued.insert(caller) {
                    work.push(caller);
                }
            }
        }
    }

    fns.iter()
        .zip(writes)
        .map(|((findex, _), writes)| (*findex, CallSummary { writes }))
        .collect()
}

/// Gives `f` the summary of every function it calls directly that `summaries` has.
/// Returns how many it took.
pub fn attach(f: &mut Function, summaries: &HashMap<usize, CallSummary>) -> usize {
    let mut taken = 0;
    for b in &f.blocks {
        for ins in &b.instrs {
            if let Instr::Call { fun, .. } = ins
                && let Some(s) = summaries.get(fun)
                && !f.facts.calls.contains_key(fun)
            {
                f.facts.calls.insert(*fun, s.clone());
                taken += 1;
            }
        }
    }
    taken
}
