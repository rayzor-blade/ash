use crate::values::NanBoxedValue;

/// Register file for a single function execution frame.
/// Uses NaN boxing for efficient value storage (8 bytes per register, Copy semantics).
#[derive(Debug)]
pub struct RegisterFile {
    registers: Vec<NanBoxedValue>,
    /// The function this frame was sized for. Carried only so an out-of-range
    /// register can name it: the bare index panic says a frame of 5 was
    /// written at 15 without saying whose opcodes were running, and the two
    /// disagreeing is the whole bug.
    owner: usize,
}

impl RegisterFile {
    pub fn new(register_count: usize) -> Self {
        Self {
            registers: vec![NanBoxedValue::void(); register_count],
            owner: usize::MAX,
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
        Self {
            registers: buf,
            owner: usize::MAX,
        }
    }

    /// Hand the buffer back so the next call can have it.
    pub fn into_buffer(self) -> Vec<NanBoxedValue> {
        self.registers
    }

    /// Record which function the frame belongs to, for the panic message.
    #[inline(always)]
    pub fn set_owner(&mut self, function_index: usize) {
        self.owner = function_index;
    }

    #[inline(always)]
    pub fn get(&self, index: u32) -> NanBoxedValue {
        let i = index as usize;
        match self.registers.get(i) {
            Some(v) => *v,
            None => self.out_of_range(i, "read"),
        }
    }

    #[inline(always)]
    pub fn set(&mut self, index: u32, value: NanBoxedValue) {
        let i = index as usize;
        let owner = self.owner;
        match self.registers.get_mut(i) {
            Some(slot) => *slot = value,
            None => RegisterFile::report(owner, i, 0, "write"),
        }
    }

    /// Get a mutable pointer to a register slot (for Ref opcode).
    #[inline(always)]
    pub fn slot_ptr(&mut self, index: u32) -> *mut NanBoxedValue {
        let i = index as usize;
        let owner = self.owner;
        let len = self.registers.len();
        match self.registers.get_mut(i) {
            Some(slot) => slot as *mut NanBoxedValue,
            None => RegisterFile::report(owner, i, len, "take a reference to"),
        }
    }

    #[cold]
    #[inline(never)]
    fn out_of_range(&self, index: usize, what: &str) -> ! {
        RegisterFile::report(self.owner, index, self.registers.len(), what)
    }

    #[cold]
    #[inline(never)]
    fn report(owner: usize, index: usize, len: usize, what: &str) -> ! {
        panic!(
            "cannot {what} register {index}: the frame holds {len}, and was built \
             for function index {}. A frame sized for one function is running \
             another's opcodes -- the mismatch is the bug, not the index.",
            if owner == usize::MAX {
                "<unrecorded>".to_string()
            } else {
                owner.to_string()
            }
        )
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
    /// Lanes of the vector values this frame holds, by `ValueId`.
    ///
    /// The register file is one `NanBoxedValue` per value and the interpreter
    /// is scalar, so a widened value has nowhere else to live. Empty for every
    /// frame running a function the vectorizer did not touch, which is all of
    /// them until one is -- the map is only allocated on first use.
    ///
    /// Lane-at-a-time execution is slower than the scalar loop it replaces,
    /// and that is the right trade: the interpreter must be able to RUN a
    /// vectorized function, so the same AIR works on every tier. Refusing
    /// instead would mean a widened function could not be interpreted, and a
    /// function that cannot fall back cannot be deoptimized.
    pub vec_lanes: std::collections::HashMap<u32, Vec<NanBoxedValue>>,
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
    /// Callee returns into this frame from a SELF-recursive call. A recursive
    /// frame has no back-edge, so this is its only recurring transfer point.
    pub self_returns: u32,
}

impl InterpreterFrame {
    pub fn new(function_index: usize, register_count: usize) -> Self {
        Self {
            function_index,
            registers: {
                let mut r = RegisterFile::new(register_count);
                r.set_owner(function_index);
                r
            },
            pc: 0,
            vec_lanes: std::collections::HashMap::new(),
            trap_stack: Vec::new(),
            backedges: 0,
            self_returns: 0,
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
            registers: {
                let mut r = RegisterFile::from_buffer(buf, register_count);
                r.set_owner(function_index);
                r
            },
            pc: 0,
            vec_lanes: std::collections::HashMap::new(),
            trap_stack: Vec::new(),
            backedges: 0,
            self_returns: 0,
        }
    }

    /// Reclaim the register buffer when the frame is finished with.
    pub fn into_buffer(self) -> Vec<NanBoxedValue> {
        self.registers.into_buffer()
    }
}
