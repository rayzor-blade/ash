//! Which functions a fiber can be suspended inside.
//!
//! A link-time fiber transform instruments a function so that it can unwind
//! out of a call and later rewind back into it. Instrumenting one costs code
//! size and speed, so the analysis that decides *which* is not a detail of the
//! transform -- it is the thing that makes the transform affordable or not.
//! `docs/wasm-fibers.md` records the measurement that settled the shape of
//! this module; the short version is that the answer at this level is bad, and
//! knowing exactly how bad is why the analysis ships before the rewrite.
//!
//! A function must be instrumented when it can be on the stack at the moment a
//! fiber suspends: it calls a suspend point, or it calls something that can.
//! That is a backward closure over the call graph from a set of seeds, and the
//! only hard part is the edges an indirect call contributes.
//!
//! # The indirect-call problem, which is the whole problem
//!
//! A `call_indirect` names a *type*, not a callee, so soundness requires
//! assuming it reaches every function that could be in the table with that
//! type. [`Policy::TypedTable`] does exactly that and no worse: only
//! address-taken functions can be in the table, and only those whose type
//! matches the call site. That is strictly tighter than "any indirect call
//! suspends" and it is the best this level of the program allows.
//!
//! It is also worth almost nothing, and this module exists so that stays
//! measurable rather than being rediscovered: `tests/suspend_set.rs` prints
//! both policies over a real module, and `docs/wasm-fibers.md` records what
//! they came to. A Haxe program has thousands of functions sharing a few
//! dozen wasm signatures, so once any suspending function is in the table
//! under a common signature every `call_indirect` of that signature has to be
//! assumed to suspend. The information that would separate them -- a virtual
//! call reaches one vtable slot, a closure call reaches only what was made
//! into a closure -- exists in ash's bytecode and does not survive into wasm.
//! Narrowing has to happen before this point or not at all.

use std::collections::{BTreeMap, BTreeSet};

use anyhow::{anyhow, Result};

/// What one function does that the closure cares about.
#[derive(Debug, Default, Clone)]
pub struct Edges {
    /// Functions this one calls by index.
    pub direct: BTreeSet<u32>,
    /// Type indices this one calls through the table.
    pub indirect: BTreeSet<u32>,
    /// Functions whose address this body takes with `ref.func`, which is the
    /// only way one reaches a table. Not an edge; collected here because a
    /// body is scanned once and [`program_from_module`] needs it.
    pub ref_func: BTreeSet<u32>,
}

/// How much an indirect call is assumed to reach.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Policy {
    /// Ignore indirect calls. Unsound for any real program -- Haxe closures,
    /// virtual dispatch and the HL function table all go through the table --
    /// and kept only as the lower bound a measurement is read against.
    DirectOnly,
    /// An indirect call reaches the address-taken functions whose type matches
    /// the call site. Sound, and the tightest this level allows.
    TypedTable,
    /// An indirect call reaches anything. What a tool with no table
    /// information must assume.
    AnyIndirect,
}

/// The inputs the closure needs about a module, however they were obtained.
///
/// Deliberately not tied to a parsed module or to a linker `Layout`: the
/// transform runs inside the linker where indices come from `func_out`, and
/// the measurements run over a finished `.wasm`. Both can fill this in.
#[derive(Debug, Default)]
pub struct Program {
    /// Edges per function index. Imports have no body and no edges.
    pub edges: BTreeMap<u32, Edges>,
    /// Type index of every function, imports included.
    pub types: BTreeMap<u32, u32>,
    /// Functions whose address is taken, so they can be in the table.
    pub address_taken: BTreeSet<u32>,
}

impl Program {
    /// Functions that must be instrumented, given the seeds.
    ///
    /// The seeds are the suspend points themselves -- the import a fiber
    /// yields through, and any runtime entry point that parks. They are named
    /// by the caller rather than guessed at here, because which primitives
    /// block is a property of ash's runtime and not something to infer from a
    /// module.
    pub fn suspend_closure(&self, seeds: &BTreeSet<u32>, policy: Policy) -> BTreeSet<u32> {
        self.suspend_closure_with_barriers(seeds, policy, &BTreeSet::new())
    }

    /// The same closure, stopped at `barriers`.
    ///
    /// An unwind travels exactly as far as the instrumentation does: a
    /// function with no epilogue sees the callee return and carries on with
    /// its own locals intact. A barrier is a function chosen to be that
    /// edge -- the scheduler, which has to still be running after the fiber
    /// under it suspends, so it can put the fiber aside and pick another.
    ///
    /// So a barrier is not instrumented, and nothing reaching a suspend point
    /// only through one is either: the unwind never gets that far. That makes
    /// the set smaller as well as correct, since everything above the
    /// scheduler drops out of it.
    ///
    /// A barrier is a promise the caller makes and this cannot check: that the
    /// function does not need to resume in the middle. It observes that its
    /// callee suspended and returns; it must not expect the callee's result.
    pub fn suspend_closure_with_barriers(
        &self,
        seeds: &BTreeSet<u32>,
        policy: Policy,
        barriers: &BTreeSet<u32>,
    ) -> BTreeSet<u32> {
        // Address-taken functions grouped by type: what an indirect call of a
        // given type could reach.
        let mut by_type: BTreeMap<u32, BTreeSet<u32>> = BTreeMap::new();
        for f in &self.address_taken {
            if let Some(t) = self.types.get(f) {
                by_type.entry(*t).or_default().insert(*f);
            }
        }

        let mut set: BTreeSet<u32> = seeds.difference(barriers).copied().collect();
        loop {
            // Types whose table entries include something that can suspend.
            // Recomputed each round because the set grows.
            let hot: BTreeSet<u32> = match policy {
                // AnyIndirect does not consult this: it sweeps in any function
                // that calls through the table at all, whatever the type.
                Policy::DirectOnly | Policy::AnyIndirect => BTreeSet::new(),
                Policy::TypedTable => by_type
                    .iter()
                    .filter(|(_, fs)| fs.iter().any(|f| set.contains(f)))
                    .map(|(t, _)| *t)
                    .collect(),
            };
            let mut grew = false;
            for (&f, e) in &self.edges {
                if set.contains(&f) || barriers.contains(&f) {
                    continue;
                }
                let reaches = e.direct.iter().any(|c| set.contains(c))
                    || match policy {
                        Policy::DirectOnly => false,
                        // Binaryen's rule when it has no table information:
                        // calling through the table at all is enough.
                        Policy::AnyIndirect => !e.indirect.is_empty(),
                        Policy::TypedTable => e.indirect.iter().any(|t| hot.contains(t)),
                    };
                if reaches {
                    grew = true;
                    // Recorded after the scan would be simpler, but growing
                    // the set inside the pass converges in fewer rounds and
                    // the result is identical: this is a monotone fixpoint.
                    set.insert(f);
                }
            }
            if !grew {
                return set;
            }
        }
    }
}

/// The calls one function body makes.
///
/// `contents` is body contents as [`crate::body`] defines them: locals
/// declarations and instructions, no size prefix.
pub fn scan_body(contents: &[u8]) -> Result<Edges> {
    let body = wasmparser::FunctionBody::new(wasmparser::BinaryReader::new_features(
        contents,
        0,
        wasmparser::WasmFeatures::all(),
    ));
    let mut reader = body
        .get_operators_reader()
        .map_err(|e| anyhow!("reading operators: {e}"))?;
    let mut edges = Edges::default();
    while !reader.eof() {
        match reader
            .read()
            .map_err(|e| anyhow!("decoding an operator: {e}"))?
        {
            wasmparser::Operator::Call { function_index } => {
                edges.direct.insert(function_index);
            }
            // A tail call is a call whose frame is already gone, which the
            // transform cannot wrap in an unwind check. It is recorded as an
            // ordinary edge so the analysis is honest about reachability; a
            // rewrite that meets one must refuse rather than mis-instrument.
            wasmparser::Operator::ReturnCall { function_index } => {
                edges.direct.insert(function_index);
            }
            wasmparser::Operator::CallIndirect { type_index, .. } => {
                edges.indirect.insert(type_index);
            }
            wasmparser::Operator::ReturnCallIndirect { type_index, .. } => {
                edges.indirect.insert(type_index);
            }
            wasmparser::Operator::RefFunc { function_index } => {
                edges.ref_func.insert(function_index);
            }
            _ => {}
        }
    }
    Ok(edges)
}

/// Read a finished `.wasm` into a [`Program`].
///
/// This is the measurement path, and the one that has to be right about what
/// can be in a table: a function reaches one only by having its address taken
/// with `ref.func`, whether in an element segment, a global initialiser or a
/// body. Exports are deliberately not counted -- the host can call an export,
/// but a `call_indirect` inside the module cannot reach it that way.
///
/// The linker will build the same structure from its own `Layout` rather than
/// by re-parsing what it just wrote; both fill in the same fields.
pub fn program_from_module(bytes: &[u8]) -> Result<Program> {
    let mut p = Program::default();
    // Imports occupy the low function indices, then the defined functions in
    // code-section order. Both index spaces are one, so this counter is the
    // only thing keeping them aligned.
    let mut next: u32 = 0;
    let mut defined: Vec<u32> = Vec::new();
    let mut body_at = 0usize;

    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        match payload.map_err(|e| anyhow!("parsing the module: {e}"))? {
            wasmparser::Payload::ImportSection(r) => {
                // An entry is a group rather than a single import: the compact
                // encoding shares one module name across many names, so this
                // nests.
                for group in r {
                    for import in group.map_err(|e| anyhow!("reading imports: {e}"))? {
                        let (_, import) = import.map_err(|e| anyhow!("reading an import: {e}"))?;
                        if let wasmparser::TypeRef::Func(t) = import.ty {
                            p.types.insert(next, t);
                            next += 1;
                        }
                    }
                }
            }
            wasmparser::Payload::FunctionSection(r) => {
                for t in r {
                    let t = t.map_err(|e| anyhow!("reading a function type: {e}"))?;
                    p.types.insert(next, t);
                    defined.push(next);
                    next += 1;
                }
            }
            wasmparser::Payload::GlobalSection(r) => {
                for g in r {
                    let g = g.map_err(|e| anyhow!("reading a global: {e}"))?;
                    take_refs(g.init_expr.get_operators_reader(), &mut p.address_taken)?;
                }
            }
            wasmparser::Payload::ElementSection(r) => {
                for e in r {
                    let e = e.map_err(|e| anyhow!("reading an element segment: {e}"))?;
                    match e.items {
                        wasmparser::ElementItems::Functions(fs) => {
                            for f in fs {
                                p.address_taken
                                    .insert(f.map_err(|e| anyhow!("an element entry: {e}"))?);
                            }
                        }
                        wasmparser::ElementItems::Expressions(_, exprs) => {
                            for expr in exprs {
                                let expr = expr.map_err(|e| anyhow!("an element expr: {e}"))?;
                                take_refs(expr.get_operators_reader(), &mut p.address_taken)?;
                            }
                        }
                    }
                }
            }
            wasmparser::Payload::CodeSectionEntry(body) => {
                let range = body.range();
                let f = *defined.get(body_at).ok_or_else(|| {
                    anyhow!("code section has more bodies than the function section declared")
                })?;
                body_at += 1;
                let edges = scan_body(&bytes[range.start as usize..range.end as usize])?;
                p.address_taken.extend(edges.ref_func.iter().copied());
                p.edges.insert(f, edges);
            }
            _ => {}
        }
    }
    Ok(p)
}

/// Collect the `ref.func` targets of a constant expression.
fn take_refs(mut r: wasmparser::OperatorsReader, into: &mut BTreeSet<u32>) -> Result<()> {
    while !r.eof() {
        if let wasmparser::Operator::RefFunc { function_index } =
            r.read().map_err(|e| anyhow!("a const expression: {e}"))?
        {
            into.insert(function_index);
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn program() -> Program {
        // 0 imports the yield; 1 calls it; 2 calls 1; 3 is unrelated;
        // 4 reaches 1 only through the table, where 1 is address-taken.
        let mut p = Program::default();
        p.types.extend([(0, 0), (1, 0), (2, 1), (3, 1), (4, 1)]);
        p.address_taken.insert(1);
        let edge = |direct: &[u32], indirect: &[u32]| Edges {
            direct: direct.iter().copied().collect(),
            indirect: indirect.iter().copied().collect(),
            ref_func: BTreeSet::new(),
        };
        p.edges.insert(1, edge(&[0], &[]));
        p.edges.insert(2, edge(&[1], &[]));
        p.edges.insert(3, edge(&[], &[]));
        p.edges.insert(4, edge(&[], &[0]));
        p
    }

    #[test]
    fn the_closure_follows_direct_calls() {
        let p = program();
        let seeds = BTreeSet::from([0]);
        let set = p.suspend_closure(&seeds, Policy::DirectOnly);
        assert!(set.contains(&1), "the caller of a suspend point");
        assert!(set.contains(&2), "its caller in turn");
        assert!(!set.contains(&3), "an unrelated function");
        assert!(!set.contains(&4), "reaches it only indirectly");
    }

    #[test]
    fn a_typed_table_edge_is_followed_only_for_a_matching_type() {
        let p = program();
        let seeds = BTreeSet::from([0]);
        let set = p.suspend_closure(&seeds, Policy::TypedTable);
        // Function 1 is address-taken with type 0 and can suspend, so an
        // indirect call of type 0 reaches it.
        assert!(set.contains(&4), "an indirect call of a suspending type");
        assert!(!set.contains(&3), "still unrelated");
    }

    #[test]
    fn a_type_with_no_suspending_target_is_not_followed() {
        let mut p = program();
        // Move the only suspending address-taken function to another type, so
        // type 0 has no suspending entry any more.
        p.types.insert(1, 7);
        let set = p.suspend_closure(&BTreeSet::from([0]), Policy::TypedTable);
        assert!(
            !set.contains(&4),
            "an indirect call whose type has no suspending target must not be swept in"
        );
    }

    #[test]
    fn any_indirect_is_the_upper_bound() {
        let mut p = program();
        p.types.insert(1, 7);
        let typed = p.suspend_closure(&BTreeSet::from([0]), Policy::TypedTable);
        let any = p.suspend_closure(&BTreeSet::from([0]), Policy::AnyIndirect);
        assert!(
            any.len() > typed.len(),
            "AnyIndirect must be no tighter than TypedTable: {any:?} vs {typed:?}"
        );
    }

    #[test]
    fn a_body_yields_its_calls() {
        use wasm_encoder::Instruction;
        let mut f = wasm_encoder::Function::new([]);
        f.instruction(&Instruction::Call(11));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::CallIndirect {
            type_index: 4,
            table_index: 0,
        });
        f.instruction(&Instruction::End);
        let edges = scan_body(&f.into_raw_body()).expect("scan");
        assert_eq!(edges.direct, BTreeSet::from([11]));
        assert_eq!(edges.indirect, BTreeSet::from([4]));
    }
}
