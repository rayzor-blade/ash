//! Module-level declarations carried by the IR: native imports, the set of
//! float types, and callee bodies.
//!
//! Lowering is handed a [`ModuleInfo`] — the embedder's view of the bytecode
//! module — and records everything it learns from it into the function it
//! builds. Backends then read declarations off the IR instead of re-deriving
//! them from bytecode:
//!
//! * LLVM materializes one `declare` per [`NativeImport`],
//! * Cranelift calls `import_function` per [`NativeImport`] *before* its
//!   `FunctionBuilder` exists, which is only possible because the declaration
//!   travels with the IR,
//! * the process-global symbol table binds an address against the same
//!   `(lib, name)` pair and `findex`.
//!
//! [`ModuleInfo::callee`] serves the same purpose for the one optimization
//! that needs to see past the function it is given: inlining. `air` holds one
//! function at a time and has no view of the module's function pool, so the
//! embedder answers with the callee's body.
//!
//! `air` stays dependency-free: [`ModuleInfo`] is a trait the embedder
//! implements (or a prebuilt [`ModuleTables`] it fills in), never a
//! dependency on the bytecode crate.

use super::ir::{Function, TypeRef};
use crate::opcodes::Opcode;
use anyhow::{bail, Result};
use std::collections::BTreeMap;

/// A native function the module imports from a shared library.
///
/// One declaration per `findex`, produced once during lowering and consumed
/// verbatim by every backend.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NativeImport {
    /// Function index in the module's function/native pool. This is the key
    /// the `Call*` opcodes reference and the identity used everywhere.
    pub findex: usize,
    /// Library the symbol is resolved from (`"std"`, `"sdl"`, ...).
    pub lib: String,
    /// Symbol name inside that library.
    pub name: String,
    /// Argument types, in call order.
    pub args: Vec<TypeRef>,
    /// Return type.
    pub ret: TypeRef,
}

impl NativeImport {
    pub fn new(
        findex: usize,
        lib: impl Into<String>,
        name: impl Into<String>,
        args: Vec<TypeRef>,
        ret: TypeRef,
    ) -> Self {
        NativeImport {
            findex,
            lib: lib.into(),
            name: name.into(),
            args,
            ret,
        }
    }

    /// `lib@name`, the shape HashLink's `DEFINE_PRIM` resolver protocol uses.
    pub fn symbol(&self) -> String {
        format!("{}@{}", self.lib, self.name)
    }
}

/// Forward declarations of natives, keyed by `findex`.
///
/// The table guarantees: one entry per `findex`, insertion order preserved
/// (so backends can emit declarations deterministically), and conflicting
/// re-declarations of the same `findex` rejected rather than silently
/// overwritten.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct NativeTable {
    imports: Vec<NativeImport>,
    by_findex: BTreeMap<usize, usize>,
}

impl NativeTable {
    pub fn new() -> Self {
        NativeTable::default()
    }

    /// Declare a native. Returns `true` if this added a new entry, `false` if
    /// an identical declaration was already present. A different declaration
    /// for the same `findex` is an error.
    pub fn declare(&mut self, imp: NativeImport) -> Result<bool> {
        match self.by_findex.get(&imp.findex) {
            Some(&i) => {
                if self.imports[i] != imp {
                    bail!(
                        "conflicting native declarations for findex {}: {}@{} vs {}@{}",
                        imp.findex,
                        self.imports[i].lib,
                        self.imports[i].name,
                        imp.lib,
                        imp.name
                    );
                }
                Ok(false)
            }
            None => {
                self.by_findex.insert(imp.findex, self.imports.len());
                self.imports.push(imp);
                Ok(true)
            }
        }
    }

    pub fn get(&self, findex: usize) -> Option<&NativeImport> {
        self.by_findex.get(&findex).map(|&i| &self.imports[i])
    }

    pub fn contains(&self, findex: usize) -> bool {
        self.by_findex.contains_key(&findex)
    }

    pub fn len(&self) -> usize {
        self.imports.len()
    }

    pub fn is_empty(&self) -> bool {
        self.imports.is_empty()
    }

    /// Declarations in insertion order.
    pub fn as_slice(&self) -> &[NativeImport] {
        &self.imports
    }

    pub fn iter(&self) -> std::slice::Iter<'_, NativeImport> {
        self.imports.iter()
    }

    /// Merge another table into this one, rejecting conflicts.
    pub fn merge(&mut self, other: &NativeTable) -> Result<usize> {
        let mut added = 0;
        for imp in other.iter() {
            if self.declare(imp.clone())? {
                added += 1;
            }
        }
        Ok(added)
    }
}

impl<'a> IntoIterator for &'a NativeTable {
    type Item = &'a NativeImport;
    type IntoIter = std::slice::Iter<'a, NativeImport>;
    fn into_iter(self) -> Self::IntoIter {
        self.imports.iter()
    }
}

/// The body of a bytecode function, in either of the two forms the embedder
/// may find cheaper to produce.
///
/// [`CalleeBody::Bytecode`] is the form an embedder that keeps raw opcode
/// arrays hands over; the inliner lowers it on demand.
/// [`CalleeBody::Air`] is the form an embedder that already lowered the whole
/// module hands over, which avoids re-lowering the same callee once per call
/// site.
#[derive(Debug, Clone)]
pub enum CalleeBody {
    /// Already lowered to AIR.
    Air(Function),
    /// Raw bytecode: the function's opcode array and register-type table.
    Bytecode {
        ops: Vec<Opcode>,
        reg_types: Vec<TypeRef>,
    },
}

impl CalleeBody {
    /// The lowered body, tagged with `findex`.
    ///
    /// Bytecode is lowered against `info`, so a callee carries the same native
    /// declarations and float types it would have been given had the embedder
    /// lowered it itself — which is what lets the inliner merge them into the
    /// caller.
    pub fn into_function(self, findex: usize, info: &dyn ModuleInfo) -> Result<Function> {
        let f = match self {
            CalleeBody::Air(f) => f,
            CalleeBody::Bytecode { ops, reg_types } => {
                super::lower::lower_with(&ops, &reg_types, info)?
            }
        };
        Ok(f.with_findex(findex))
    }
}

/// The embedder's view of the bytecode module, consulted during lowering and
/// by the inliner.
///
/// Every method defaults to "nothing known", which makes lowering without
/// module info produce a function with an empty native table and no float
/// types, and makes inlining find no callee — every module-info-dependent
/// rewrite is then inert rather than wrong.
pub trait ModuleInfo {
    /// The native declaration for `findex`, or `None` when `findex` is a
    /// bytecode function.
    fn native(&self, findex: usize) -> Option<NativeImport> {
        let _ = findex;
        None
    }

    /// True when values of `ty` are HL floats (`f32`/`f64`). Float-only
    /// rewrites — notably [`Fma`](super::ir::Instr::Fma) formation — consult
    /// this and refuse when it is false.
    fn is_float(&self, ty: TypeRef) -> bool {
        let _ = ty;
        false
    }

    /// The body of bytecode function `findex`, when the embedder can supply
    /// one.
    ///
    /// `None` — the default — is the answer for a native, for a body the
    /// embedder has not got, and for a function it does not want copied;
    /// [`inline`](super::passes::inline) treats all three the same way and
    /// leaves the call alone. Because the default answers `None`, a function
    /// lowered through the bare [`lower`](super::lower::lower) wrapper is
    /// never inlined into.
    fn callee(&self, findex: usize) -> Option<CalleeBody> {
        let _ = findex;
        None
    }
}

/// Module info that knows nothing: lowering records no natives and no float
/// types, and no call is inlinable.
#[derive(Debug, Clone, Copy, Default)]
pub struct NoModuleInfo;

impl ModuleInfo for NoModuleInfo {}

/// The shared "nothing known" instance, so a pipeline built without module
/// info can still hold an inliner (an inert one).
pub static NO_MODULE_INFO: NoModuleInfo = NoModuleInfo;

/// Prebuilt module info: a native table, the float type refs, and the callee
/// bodies the inliner may copy. For callers that would rather fill in tables
/// than implement [`ModuleInfo`].
#[derive(Debug, Clone, Default)]
pub struct ModuleTables {
    pub natives: NativeTable,
    pub float_types: Vec<TypeRef>,
    /// Bodies offered to the inliner, keyed by `findex`. A `findex` that is
    /// absent is simply never inlined.
    pub callees: BTreeMap<usize, CalleeBody>,
}

impl ModuleTables {
    pub fn new() -> Self {
        ModuleTables::default()
    }

    pub fn with_natives(mut self, natives: NativeTable) -> Self {
        self.natives = natives;
        self
    }

    pub fn with_float_types(mut self, tys: impl IntoIterator<Item = TypeRef>) -> Self {
        self.float_types = tys.into_iter().collect();
        self.float_types.sort_unstable();
        self.float_types.dedup();
        self
    }

    /// Offer one function body to the inliner.
    pub fn with_callee(mut self, findex: usize, body: CalleeBody) -> Self {
        self.callees.insert(findex, body);
        self
    }
}

impl ModuleInfo for ModuleTables {
    fn native(&self, findex: usize) -> Option<NativeImport> {
        self.natives.get(findex).cloned()
    }
    fn is_float(&self, ty: TypeRef) -> bool {
        self.float_types.binary_search(&ty).is_ok()
    }
    fn callee(&self, findex: usize) -> Option<CalleeBody> {
        self.callees.get(&findex).cloned()
    }
}

impl ModuleInfo for NativeTable {
    fn native(&self, findex: usize) -> Option<NativeImport> {
        self.get(findex).cloned()
    }
}

impl<T: ModuleInfo + ?Sized> ModuleInfo for &T {
    fn native(&self, findex: usize) -> Option<NativeImport> {
        (**self).native(findex)
    }
    fn is_float(&self, ty: TypeRef) -> bool {
        (**self).is_float(ty)
    }
    fn callee(&self, findex: usize) -> Option<CalleeBody> {
        (**self).callee(findex)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn imp(findex: usize, name: &str) -> NativeImport {
        NativeImport::new(findex, "std", name, vec![TypeRef(0)], TypeRef(1))
    }

    #[test]
    fn table_dedups_by_findex() {
        let mut t = NativeTable::new();
        assert!(t.declare(imp(7, "alloc_bytes")).unwrap());
        assert!(!t.declare(imp(7, "alloc_bytes")).unwrap());
        assert_eq!(t.len(), 1);
        assert_eq!(t.get(7).unwrap().name, "alloc_bytes");
        assert!(t.get(8).is_none());
    }

    #[test]
    fn table_rejects_conflicting_declaration() {
        let mut t = NativeTable::new();
        t.declare(imp(7, "alloc_bytes")).unwrap();
        let err = t.declare(imp(7, "free_bytes")).unwrap_err();
        assert!(err.to_string().contains("conflicting"), "{err}");
    }

    #[test]
    fn table_preserves_insertion_order_and_merges() {
        let mut a = NativeTable::new();
        a.declare(imp(9, "b")).unwrap();
        a.declare(imp(3, "a")).unwrap();
        let names: Vec<&str> = a.iter().map(|i| i.name.as_str()).collect();
        assert_eq!(names, vec!["b", "a"]);

        let mut b = NativeTable::new();
        b.declare(imp(3, "a")).unwrap();
        b.declare(imp(4, "c")).unwrap();
        assert_eq!(a.merge(&b).unwrap(), 1);
        assert_eq!(a.len(), 3);
    }

    #[test]
    fn symbol_uses_define_prim_shape() {
        assert_eq!(imp(1, "utf8_length").symbol(), "std@utf8_length");
    }

    #[test]
    fn module_tables_answer_queries() {
        let mut nt = NativeTable::new();
        nt.declare(imp(2, "sin")).unwrap();
        let m = ModuleTables::new()
            .with_natives(nt)
            .with_float_types([TypeRef(5), TypeRef(1)]);
        assert_eq!(m.native(2).unwrap().name, "sin");
        assert!(m.native(3).is_none());
        assert!(m.is_float(TypeRef(1)));
        assert!(m.is_float(TypeRef(5)));
        assert!(!m.is_float(TypeRef(4)));
        assert!(!NoModuleInfo.is_float(TypeRef(1)));
    }
}
