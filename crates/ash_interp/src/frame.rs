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

    /// Build a register file on a recycled buffer.
    ///
    /// Every call used to allocate one: `vec![void; register_count]` is a
    /// malloc per invocation, on top of the one for the argument list. Calls
    /// nest, so the buffers come back in the order they were handed out and a
    /// plain stack of them is enough to keep the allocator out of the call
    /// path entirely.
    pub fn from_buffer(mut buf: Vec<NanBoxedValue>, register_count: usize) -> Self {
        buf.clear();
        buf.resize(register_count, NanBoxedValue::void());
        Self { registers: buf }
    }

    /// Hand the buffer back so the next call can have it.
    pub fn into_buffer(self) -> Vec<NanBoxedValue> {
        self.registers
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
    pub fn is_empty(&self) -> bool {
        self.registers.is_empty()
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
    /// Backward jumps taken in this invocation.
    ///
    /// Promotion counts calls, so a hot loop inside a function called once
    /// can never reach the threshold -- nbody's `main` is exactly that shape,
    /// ten million iterations in a single invocation. Counting back-edges is
    /// what notices it. A plain `u32` on the frame keeps the probe to an
    /// increment and a compare on the interpreter's hottest path.
    pub backedges: u32,
}

impl InterpreterFrame {
    pub fn new(function_index: usize, register_count: usize) -> Self {
        Self {
            function_index,
            registers: RegisterFile::new(register_count),
            pc: 0,
            trap_stack: Vec::new(),
            backedges: 0,
        }
    }

    /// Same, on a recycled register buffer. See [`RegisterFile::from_buffer`].
    pub fn with_buffer(
        function_index: usize,
        register_count: usize,
        buf: Vec<NanBoxedValue>,
    ) -> Self {
        Self {
            function_index,
            registers: RegisterFile::from_buffer(buf, register_count),
            pc: 0,
            trap_stack: Vec::new(),
            backedges: 0,
        }
    }

    /// Reclaim the register buffer when the frame is finished with.
    pub fn into_buffer(self) -> Vec<NanBoxedValue> {
        self.registers.into_buffer()
    }
}
