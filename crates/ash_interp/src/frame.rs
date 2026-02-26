use crate::values::NanBoxedValue;

/// Register file for a single function execution frame.
/// Uses NaN boxing for efficient value storage (8 bytes per register, Copy semantics).
#[derive(Debug)]
pub struct RegisterFile {
    registers: Vec<NanBoxedValue>,
}

impl RegisterFile {
    pub fn new(register_count: usize) -> Self {
        Self {
            registers: vec![NanBoxedValue::void(); register_count],
        }
    }

    #[inline(always)]
    pub fn get(&self, index: u32) -> NanBoxedValue {
        self.registers[index as usize]
    }

    #[inline(always)]
    pub fn set(&mut self, index: u32, value: NanBoxedValue) {
        self.registers[index as usize] = value;
    }

    /// Get a mutable pointer to a register slot (for Ref opcode).
    #[inline(always)]
    pub fn slot_ptr(&mut self, index: u32) -> *mut NanBoxedValue {
        &mut self.registers[index as usize] as *mut NanBoxedValue
    }

    #[inline(always)]
    pub fn as_ptr(&self) -> *const NanBoxedValue {
        self.registers.as_ptr()
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.registers.len()
    }

    #[inline(always)]
    pub fn as_slice(&self) -> &[NanBoxedValue] {
        &self.registers
    }
}

/// Interpreter execution frame (one per function call).
#[derive(Debug)]
pub struct InterpreterFrame {
    /// Index into bytecode.functions (or native findex)
    pub function_index: usize,
    /// Register file for this invocation
    pub registers: RegisterFile,
    /// Program counter: current opcode index within the function's ops
    pub pc: usize,
    /// Active exception traps: (target_pc, exc_reg).
    /// target_pc is the absolute opcode index of the catch block.
    pub trap_stack: Vec<(usize, u32)>,
}

impl InterpreterFrame {
    pub fn new(function_index: usize, register_count: usize) -> Self {
        Self {
            function_index,
            registers: RegisterFile::new(register_count),
            pc: 0,
            trap_stack: Vec::new(),
        }
    }
}
