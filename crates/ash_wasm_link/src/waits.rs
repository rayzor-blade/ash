//! Route atomic waits through the host so program shutdown can wake them.
//!
//! Epoch interruption cannot interrupt a thread inside memory.atomic.wait.
//! These thunks let the host register wait addresses before doing the SAME
//! atomic wait, with its original timeout and notification semantics.

use anyhow::{bail, Result};
use wasm_encoder::reencode::{self, Reencode};
use wasm_encoder::{EntityType, ImportSection, Instruction, SectionId, TypeSection, ValType};
use wasmparser::{Operator, Parser, Payload, TypeRef};

pub const WAIT32: &str = "ash_host_atomic_wait32";
pub const WAIT64: &str = "ash_host_atomic_wait64";

pub fn instrument(bytes: &[u8]) -> Result<Vec<u8>> {
    let mut imports = 0;
    let mut types = 0;
    let mut waits = false;
    let mut memory64 = false;
    for payload in Parser::new(0).parse_all(bytes) {
        match payload? {
            Payload::TypeSection(section) => {
                for group in section {
                    types += group?.types().len() as u32;
                }
            }
            Payload::ImportSection(section) => {
                for import in section.into_imports() {
                    let import = import?;
                    if matches!(import.ty, TypeRef::Func(_)) {
                        imports += 1;
                    }
                    if let TypeRef::Memory(ty) = import.ty {
                        memory64 |= ty.memory64;
                    }
                }
            }
            Payload::MemorySection(section) => {
                for memory in section {
                    memory64 |= memory?.memory64;
                }
            }
            Payload::CodeSectionEntry(body) => {
                for op in body.get_operators_reader()? {
                    match op? {
                        Operator::MemoryAtomicWait32 { memarg }
                        | Operator::MemoryAtomicWait64 { memarg } => {
                            if memarg.memory != 0 {
                                bail!("atomic waits on multiple memories are not supported");
                            }
                            waits = true;
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }
    if !waits {
        return Ok(bytes.to_vec());
    }
    if memory64 {
        bail!("host atomic waits require wasm32 memory");
    }
    // Do not turn an invalid wait (for example an illegal alignment) into
    // a valid call merely by replacing its opcode.
    wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::all())
        .validate_all(bytes)?;
    let mut module = wasm_encoder::Module::new();
    Waits {
        imports,
        types,
        imported: false,
    }
    .parse_core_module(&mut module, Parser::new(0), bytes)?;
    Ok(module.finish())
}


struct Waits {
    imports: u32,
    types: u32,
    imported: bool,
}
impl Waits {
    fn add_imports(&mut self, imports: &mut ImportSection) {
        imports.import("env", WAIT32, EntityType::Function(self.types));
        imports.import("env", WAIT64, EntityType::Function(self.types + 1));
        self.imported = true;
    }
}
impl Reencode for Waits {
    type Error = std::convert::Infallible;

    fn function_index(&mut self, index: u32) -> Result<u32, reencode::Error<Self::Error>> {
        Ok(if index >= self.imports {
            index + 2
        } else {
            index
        })
    }

    fn parse_type_section(
        &mut self,
        types: &mut TypeSection,
        section: wasmparser::TypeSectionReader<'_>,
    ) -> Result<(), reencode::Error<Self::Error>> {
        reencode::utils::parse_type_section(self, types, section)?;
        types.ty().function(
            [ValType::I32, ValType::I32, ValType::I64, ValType::I64],
            [ValType::I32],
        );
        types.ty().function(
            [ValType::I32, ValType::I64, ValType::I64, ValType::I64],
            [ValType::I32],
        );
        Ok(())
    }

    fn parse_import_section(
        &mut self,
        imports: &mut ImportSection,
        section: wasmparser::ImportSectionReader<'_>,
    ) -> Result<(), reencode::Error<Self::Error>> {
        reencode::utils::parse_import_section(self, imports, section)?;
        self.add_imports(imports);
        Ok(())
    }

    fn intersperse_section_hook(
        &mut self,
        module: &mut wasm_encoder::Module,
        after: Option<SectionId>,
        before: Option<SectionId>,
    ) -> Result<(), reencode::Error<Self::Error>> {
        if !self.imported && after == Some(SectionId::Type) && before != Some(SectionId::Import) {
            let mut imports = ImportSection::new();
            self.add_imports(&mut imports);
            module.section(&imports);
        }
        Ok(())
    }

    fn parse_function_body(
        &mut self,
        code: &mut wasm_encoder::CodeSection,
        body: wasmparser::FunctionBody<'_>,
    ) -> Result<(), reencode::Error<Self::Error>> {
        let mut function = self.new_function_with_parsed_locals(&body)?;
        for op in body.get_operators_reader()? {
            match op? {
                Operator::MemoryAtomicWait32 { memarg } => {
                    function.instruction(&Instruction::I64Const(memarg.offset as i64));
                    function.instruction(&Instruction::Call(self.imports));
                }
                Operator::MemoryAtomicWait64 { memarg } => {
                    function.instruction(&Instruction::I64Const(memarg.offset as i64));
                    function.instruction(&Instruction::Call(self.imports + 1));
                }
                other => {
                    function.instruction(&self.instruction(other)?);
                }
            }
        }
        code.function(&function);
        Ok(())
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use wasm_encoder::{
        CodeSection, ExportKind, ExportSection, Function, FunctionSection, MemArg, MemorySection,
        MemoryType,
    };

    fn module(align: u32) -> Vec<u8> {
        let mut module = wasm_encoder::Module::new();
        let mut types = TypeSection::new();
        types.ty().function([], [ValType::I32]);
        module.section(&types);
        let mut functions = FunctionSection::new();
        functions.function(0);
        module.section(&functions);
        let mut memory = MemorySection::new();
        memory.memory(MemoryType {
            minimum: 1,
            maximum: Some(1),
            memory64: false,
            shared: true,
            page_size_log2: None,
        });
        module.section(&memory);
        let mut exports = ExportSection::new();
        exports.export("wait", ExportKind::Func, 0);
        module.section(&exports);
        let mut function = Function::new([]);
        function.instruction(&Instruction::I32Const(0));
        function.instruction(&Instruction::I32Const(1));
        function.instruction(&Instruction::I64Const(0));
        function.instruction(&Instruction::MemoryAtomicWait32(MemArg {
            offset: 4,
            align,
            memory_index: 0,
        }));
        function.instruction(&Instruction::End);
        let mut code = CodeSection::new();
        code.function(&function);
        module.section(&code);
        module.finish()
    }

    #[test]
    fn a_missing_import_section_is_inserted_and_exports_are_renumbered() {
        let bytes = instrument(&module(2)).unwrap();
        wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::all())
            .validate_all(&bytes)
            .unwrap();
        let mut imports = 0;
        for payload in Parser::new(0).parse_all(&bytes) {
            match payload.unwrap() {
                Payload::ImportSection(section) => imports = section.count(),
                Payload::ExportSection(section) => {
                    for export in section {
                        assert_eq!(export.unwrap().index, 2);
                    }
                }
                _ => {}
            }
        }
        assert_eq!(imports, 2);
        // Already instrumented modules contain no waits, so a second pass
        // must not add imports or move indices again.
        assert_eq!(instrument(&bytes).unwrap(), bytes);
    }

    #[test]
    fn an_invalid_wait_cannot_become_a_valid_host_call() {
        assert!(
            instrument(&module(1)).is_err(),
            "atomic alignment must be natural"
        );
    }
}
