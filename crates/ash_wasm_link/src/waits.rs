//! Route atomic waits through the host so program shutdown can wake them.
//!
//! Epoch interruption cannot interrupt a thread inside memory.atomic.wait.
//! These thunks let the host register wait addresses before doing the SAME
//! atomic wait, with its original timeout and notification semantics.

use anyhow::{Result, bail};
use wasm_encoder::reencode::{self, Reencode};
use wasm_encoder::{EntityType, ImportSection, SectionId, TypeSection, ValType};
use wasmparser::{Operator, Parser, Payload, TypeRef};

pub const WAIT32: &str = "ash_host_atomic_wait32";
pub const WAIT64: &str = "ash_host_atomic_wait64";

/// Whether `bytes` could hold a `memory.atomic.wait`: its opcode is `0xFE`
/// followed by `0x01` (32-bit) or `0x02` (64-bit). Only a filter, since the
/// pair can also turn up in an immediate or in data, but a module without it
/// has no waits and needs no further reading.
fn may_wait(bytes: &[u8]) -> bool {
    let mut at = 0;
    while let Some(found) = bytes[at..].iter().position(|&b| b == 0xFE) {
        let next = at + found + 1;
        if matches!(bytes.get(next), Some(0x01 | 0x02)) {
            return true;
        }
        at = next;
    }
    false
}

/// What the rewrite needs from before the code section: how many functions
/// the module imports, how many types it has, and whether a memory is 64-bit.
fn before_code(bytes: &[u8]) -> Result<(u32, u32, bool)> {
    let (mut imports, mut types, mut memory64) = (0, 0, false);
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
            Payload::CodeSectionStart { .. } => break,
            _ => {}
        }
    }
    Ok((imports, types, memory64))
}

pub fn instrument(bytes: &[u8]) -> Result<Vec<u8>> {
    if !may_wait(bytes) {
        return Ok(bytes.to_vec());
    }
    let (imports, types, memory64) = before_code(bytes)?;
    let mut rewrite = Waits {
        imports,
        types,
        imported: false,
        found: false,
    };
    let mut module = wasm_encoder::Module::new();
    rewrite.parse_core_module(&mut module, Parser::new(0), bytes)?;
    if !rewrite.found {
        return Ok(bytes.to_vec());
    }
    if memory64 {
        bail!("host atomic waits require wasm32 memory");
    }
    Ok(module.finish())
}

struct Waits {
    imports: u32,
    types: u32,
    imported: bool,
    /// Whether any function held a wait.
    found: bool,
}

/// A wait the host cannot take in place of the instruction.
#[derive(Debug)]
struct Unsupported(&'static str);

impl std::fmt::Display for Unsupported {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0)
    }
}

impl std::error::Error for Unsupported {}

/// The unsigned LEB128 encoding of `value`.
fn leb_u32(out: &mut Vec<u8>, mut value: u32) {
    loop {
        let byte = (value & 0x7F) as u8;
        value >>= 7;
        if value == 0 {
            out.push(byte);
            return;
        }
        out.push(byte | 0x80);
    }
}

/// The signed LEB128 encoding of `value`.
fn leb_i64(out: &mut Vec<u8>, mut value: i64) {
    loop {
        let byte = (value & 0x7F) as u8;
        value >>= 7;
        let done = (value == 0 && byte & 0x40 == 0) || (value == -1 && byte & 0x40 != 0);
        if done {
            out.push(byte);
            return;
        }
        out.push(byte | 0x80);
    }
}
impl Waits {
    fn add_imports(&mut self, imports: &mut ImportSection) {
        imports.import("env", WAIT32, EntityType::Function(self.types));
        imports.import("env", WAIT64, EntityType::Function(self.types + 1));
        self.imported = true;
    }
}
impl Reencode for Waits {
    type Error = Unsupported;

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

    /// A body is copied as it is, bar the instructions that change: a wait
    /// becomes the host call, and a function index past the imports moves up by
    /// the two imports added. Re-encoding every instruction would give the same
    /// bytes at several times the cost.
    fn parse_function_body(
        &mut self,
        code: &mut wasm_encoder::CodeSection,
        body: wasmparser::FunctionBody<'_>,
    ) -> Result<(), reencode::Error<Self::Error>> {
        let raw = body.as_bytes();
        let origin = body.range().start as usize;
        let mut reader = body.get_operators_reader()?;
        let first = reader.original_position() as usize - origin;
        let mut out = Vec::with_capacity(raw.len() + 16);
        out.extend_from_slice(&raw[..first]);
        let mut copied = first;
        while !reader.eof() {
            let at = reader.original_position() as usize - origin;
            let op = reader.read()?;
            let moved = |index: u32| index >= self.imports;
            match op {
                Operator::Call { function_index } if moved(function_index) => {
                    out.extend_from_slice(&raw[copied..at]);
                    out.push(0x10);
                    leb_u32(&mut out, function_index + 2);
                }
                Operator::ReturnCall { function_index } if moved(function_index) => {
                    out.extend_from_slice(&raw[copied..at]);
                    out.push(0x12);
                    leb_u32(&mut out, function_index + 2);
                }
                Operator::RefFunc { function_index } if moved(function_index) => {
                    out.extend_from_slice(&raw[copied..at]);
                    out.push(0xD2);
                    leb_u32(&mut out, function_index + 2);
                }
                Operator::MemoryAtomicWait32 { memarg }
                | Operator::MemoryAtomicWait64 { memarg } => {
                    let wide = matches!(op, Operator::MemoryAtomicWait64 { .. });
                    if memarg.memory != 0 {
                        return Err(reencode::Error::UserError(Unsupported(
                            "atomic waits on multiple memories are not supported",
                        )));
                    }
                    // Not a valid wait to begin with: replacing the opcode
                    // would make a valid call of it.
                    if memarg.align != if wide { 3 } else { 2 } {
                        return Err(reencode::Error::UserError(Unsupported(
                            "atomic alignment must be natural",
                        )));
                    }
                    self.found = true;
                    out.extend_from_slice(&raw[copied..at]);
                    out.push(0x42);
                    leb_i64(&mut out, memarg.offset as i64);
                    out.push(0x10);
                    leb_u32(&mut out, self.imports + u32::from(wide));
                }
                _ => continue,
            }
            copied = reader.original_position() as usize - origin;
        }
        out.extend_from_slice(&raw[copied..]);
        code.raw(&out);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use wasm_encoder::{
        CodeSection, ExportKind, ExportSection, Function, FunctionSection, Instruction, MemArg,
        MemorySection, MemoryType,
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

    /// Two functions behind one import. The first waits (32-bit, with an
    /// offset), calls the import and calls the second by a padded index, as
    /// wasm-ld writes them; the second waits (64-bit).
    fn two_functions() -> Vec<u8> {
        use wasm_encoder::{ImportSection, MemArg};
        let mut module = wasm_encoder::Module::new();
        let mut types = TypeSection::new();
        types.ty().function([], []);
        module.section(&types);
        let mut imports = ImportSection::new();
        imports.import("env", "outside", EntityType::Function(0));
        module.section(&imports);
        let mut functions = FunctionSection::new();
        functions.function(0);
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
        exports.export("second", ExportKind::Func, 2);
        module.section(&exports);
        let mut first = Function::new([]);
        first.instruction(&Instruction::I32Const(0));
        first.instruction(&Instruction::I32Const(0));
        first.instruction(&Instruction::I64Const(-1));
        first.instruction(&Instruction::MemoryAtomicWait32(MemArg {
            offset: 8,
            align: 2,
            memory_index: 0,
        }));
        first.instruction(&Instruction::Drop);
        first.instruction(&Instruction::Call(0));
        // call 2, its index padded to five bytes.
        first.raw([0x10, 0x82, 0x80, 0x80, 0x80, 0x00]);
        first.instruction(&Instruction::End);
        let mut second = Function::new([(2, ValType::I32)]);
        second.instruction(&Instruction::I32Const(0));
        second.instruction(&Instruction::I64Const(0));
        second.instruction(&Instruction::I64Const(0));
        second.instruction(&Instruction::MemoryAtomicWait64(MemArg {
            offset: 1 << 31,
            align: 3,
            memory_index: 0,
        }));
        second.instruction(&Instruction::Drop);
        second.instruction(&Instruction::End);
        let mut code = CodeSection::new();
        code.function(&first);
        code.function(&second);
        module.section(&code);
        module.finish()
    }

    #[test]
    fn waits_become_host_calls_and_function_indices_move_past_the_new_imports() {
        let original = two_functions();
        wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::all())
            .validate_all(&original)
            .unwrap();
        let bytes = instrument(&original).unwrap();
        wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::all())
            .validate_all(&bytes)
            .unwrap();
        let mut bodies: Vec<Vec<String>> = Vec::new();
        let mut export = None;
        let mut locals = Vec::new();
        for payload in Parser::new(0).parse_all(&bytes) {
            match payload.unwrap() {
                Payload::CodeSectionEntry(body) => {
                    locals.push(body.get_locals_reader().unwrap().into_iter().count());
                    bodies.push(
                        body.get_operators_reader()
                            .unwrap()
                            .into_iter()
                            .map(|op| format!("{:?}", op.unwrap()))
                            .collect(),
                    );
                }
                Payload::ExportSection(section) => {
                    export = section.into_iter().next().map(|e| e.unwrap().index);
                }
                _ => {}
            }
        }
        // One import, so the waits are functions 1 and 2, and the two defined
        // functions that were 1 and 2 are now 3 and 4.
        assert_eq!(export, Some(4));
        assert_eq!(
            bodies[0],
            [
                "I32Const { value: 0 }",
                "I32Const { value: 0 }",
                "I64Const { value: -1 }",
                "I64Const { value: 8 }",
                "Call { function_index: 1 }",
                "Drop",
                "Call { function_index: 0 }",
                "Call { function_index: 4 }",
                "End",
            ]
        );
        assert_eq!(
            bodies[1],
            [
                "I32Const { value: 0 }",
                "I64Const { value: 0 }",
                "I64Const { value: 0 }",
                "I64Const { value: 2147483648 }",
                "Call { function_index: 2 }",
                "Drop",
                "End",
            ]
        );
        // The second function's locals were copied, not rebuilt.
        assert_eq!(locals, [0, 1]);
    }

    #[test]
    fn an_invalid_wait_cannot_become_a_valid_host_call() {
        assert!(
            instrument(&module(1)).is_err(),
            "atomic alignment must be natural"
        );
    }
}
