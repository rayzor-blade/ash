// Copied from HLBC https://github.com/Gui-Yom/hlbc/blob/master/crates/hlbc/src/types.rs
// Copyright to the hlbc authors.

use std::slice;

use crate::hl::*;

/// Offset for a jump instruction. Can be negative, indicating a backward jump.
pub type JumpOffset = i32;

pub type InlineInt = i32;
pub type InlineBool = bool;

/// A register argument
///
/// Registers are a function local variables.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Default, Hash)]
pub struct Reg(pub u32);

/// A reference to the i32 constant pool
#[derive(Debug, Copy, Clone, Eq, PartialEq, Default)]
pub struct RefInt(pub usize);

/// A reference to the f64 constant pool
#[derive(Debug, Copy, Clone, Eq, PartialEq, Default)]
pub struct RefFloat(pub usize);

/// A reference to the bytes constant pool
#[derive(Debug, Copy, Clone, Eq, PartialEq, Default)]
pub struct RefBytes(pub usize);

/// Reference to the string constant pool
#[derive(Debug, Copy, Clone, Eq, PartialEq, Default)]
pub struct RefString(pub usize);

impl RefString {
    /// If a [RefString] is null, it indicates an element has no name
    pub fn is_null(&self) -> bool {
        self.0 == 0
    }
}

/// A reference to a global
#[derive(Debug, Copy, Clone, Eq, PartialEq, Default, Hash)]
pub struct RefGlobal(pub usize);

/// Index reference to a function or a native in the pool (findex)
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Default)]
pub struct RefFun(pub usize);

/// A reference to an object field
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Default)]
pub struct RefField(pub usize);

/// A reference to an enum variant
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct RefEnumConstruct(pub usize);

/// Reference to a type in the constant pool
#[derive(Debug, Copy, Clone, Eq, PartialEq, Default)]
pub struct RefType(pub usize);

/// Opcodes definitions. The fields are the opcode arguments.
///
/// The methods for this struct are generated through a macro because there is no way I would have written code for 98
/// opcodes. The opcode name is directly derived from the variant name. The opcode description is derived from the doc
/// comment on each variant.
///
/// The order of opcodes here is important as it defines the number used for serialization.
#[derive(Debug, Clone)]
pub enum Opcode {
    /// Copy value from *src* into *dst*
    ///
    /// `dst = src`
    Mov {
        dst: Reg,
        src: Reg,
    },
    /// Get an **i32** from the constant pool
    ///
    /// `dst = @ptr`
    Int {
        dst: Reg,
        ptr: RefInt,
    },
    /// Get a **f64** from the constant pool
    ///
    /// `dst = @ptr`
    Float {
        dst: Reg,
        ptr: RefFloat,
    },
    /// Set a **bool** value
    ///
    /// `dst = <true|false>`
    Bool {
        dst: Reg,
        value: InlineBool,
    },
    /// Get a byte array from the constant pool
    ///
    /// `dst = @ptr`
    Bytes {
        dst: Reg,
        ptr: RefBytes,
    },
    /// Get a **string** from the constant pool
    ///
    /// `dst = @ptr`
    String {
        dst: Reg,
        ptr: RefString,
    },
    /// Nullify a register
    ///
    /// `dst = null`
    Null {
        dst: Reg,
    },
    /// Add two numbers
    ///
    /// `dst = a + b`
    Add {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    /// Subtracts two numbers
    ///
    /// `dst = a - b`
    Sub {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    /// Multiply two numbers
    ///
    /// `dst = a * b`
    Mul {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    /// Signed division
    ///
    /// `dst = a / b`
    SDiv {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    /// Unsigned division
    ///
    /// `dst = a / b`
    UDiv {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    /// Signed modulo
    ///
    /// `dst = a % b`
    SMod {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    /// Unsigned modulo
    ///
    /// `dst = a % b`
    UMod {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    /// Shift bits left
    ///
    /// `dst = a << b`
    Shl {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    /// Signed shift bits right
    ///
    /// `dst = a >> b`
    SShr {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    /// Unsigned shift bits right
    ///
    /// `dst = a >>> b`
    UShr {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    /// Logical and
    ///
    /// `dst = a & b`
    And {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    /// Logical or
    ///
    /// `dst = a | b`
    Or {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    /// Logical xor
    ///
    /// `dst = a ^ b`
    Xor {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    /// Negate a number
    ///
    /// `dst = -src`
    Neg {
        dst: Reg,
        src: Reg,
    },
    /// Invert a boolean value
    ///
    /// dst = !src`
    Not {
        dst: Reg,
        src: Reg,
    },
    /// Increment a number
    ///
    /// `dst++`
    Incr {
        dst: Reg,
    },
    /// Decrement a number
    ///
    /// `dst--`
    Decr {
        dst: Reg,
    },
    /// Call a function with no argument
    ///
    /// `dst = fun()`
    Call0 {
        dst: Reg,
        fun: RefFun,
    },
    /// Call a function with one argument
    ///
    /// `dst = fun(arg0)`
    Call1 {
        dst: Reg,
        fun: RefFun,
        arg0: Reg,
    },
    /// Call a function with two arguments
    ///
    /// `dst = fun(arg0, arg1)`
    Call2 {
        dst: Reg,
        fun: RefFun,
        arg0: Reg,
        arg1: Reg,
    },
    /// Call a function with three arguments
    ///
    /// `dst = fun(arg0, arg1, arg2)`
    Call3 {
        dst: Reg,
        fun: RefFun,
        arg0: Reg,
        arg1: Reg,
        arg2: Reg,
    },
    /// Call a function with four arguments
    ///
    /// `dst = fun(arg0, arg1, arg2, arg3)`
    Call4 {
        dst: Reg,
        fun: RefFun,
        arg0: Reg,
        arg1: Reg,
        arg2: Reg,
        arg3: Reg,
    },
    /// Call a function with N arguments
    ///
    /// `dst = fun(arg0, arg1, ...)`
    CallN {
        dst: Reg,
        fun: RefFun,
        args: Vec<Reg>,
    },
    /// Call a function with N arguments, using the first argument as the receiver
    ///
    /// `dst = arg0.field(arg1, arg2, ...)`
    CallMethod {
        dst: Reg,
        field: RefField,
        // obj is the first arg
        args: Vec<Reg>,
    },
    /// Call a function with N arguments.
    ///
    /// `dst = this.field(arg0, arg1, ...)`
    CallThis {
        dst: Reg,
        field: RefField,
        args: Vec<Reg>,
    },
    /// Call a closure with N arguments. Here *fun* is a register.
    ///
    /// `dst = fun(arg0, arg1, ...)`
    CallClosure {
        dst: Reg,
        fun: Reg,
        args: Vec<Reg>,
    },
    /// Create a closure from a function reference.
    ///
    /// `dst = fun`
    StaticClosure {
        dst: Reg,
        fun: RefFun,
    },
    /// Create a closure from an object method.
    ///
    /// `dst = obj.fun`
    InstanceClosure {
        dst: Reg,
        fun: RefFun,
        obj: Reg,
    },
    /// Create a closure from an object field.
    ///
    /// `dst = obj.field`
    VirtualClosure {
        dst: Reg,
        obj: Reg,
        field: Reg,
    },
    /// Get a global value.
    ///
    /// `dst = @global`
    GetGlobal {
        dst: Reg,
        global: RefGlobal,
    },
    /// Set a global value.
    ///
    /// `@global = src`
    SetGlobal {
        global: RefGlobal,
        src: Reg,
    },
    /// Access an object field
    ///
    /// `dst = obj.field`
    Field {
        dst: Reg,
        obj: Reg,
        field: RefField,
    },
    /// Set an object field
    ///
    /// `obj.field = src`
    SetField {
        obj: Reg,
        field: RefField,
        src: Reg,
    },
    /// Get a field from the *this* instance.
    /// *this* = *reg0*.
    ///
    /// `dst = this.field`
    GetThis {
        dst: Reg,
        field: RefField,
    },
    /// Set a field from the *this* instance.
    /// *this* = *reg0*.
    ///
    /// `dst = this.field`
    SetThis {
        field: RefField,
        src: Reg,
    },
    /// Access a field of a **dyn** instance by its name.
    ///
    /// `dst = obj[field]`
    DynGet {
        dst: Reg,
        obj: Reg,
        field: RefString,
    },
    /// Set a field of a **dyn** instance by its name.
    ///
    /// `obj[field] = src`
    DynSet {
        obj: Reg,
        field: RefString,
        src: Reg,
    },
    /// Jump by an offset if the condition is true
    ///
    /// `if cond jump by offset`
    JTrue {
        cond: Reg,
        offset: JumpOffset,
    },
    /// Jump by an offset if the condition is false
    ///
    /// `if !cond jump by offset`
    JFalse {
        cond: Reg,
        offset: JumpOffset,
    },
    /// Jump by an offset if the value is null
    ///
    /// `if reg == null jump by offset`
    JNull {
        reg: Reg,
        offset: JumpOffset,
    },
    /// Jump by an offset if the value is not null
    ///
    /// `if reg != null jump by offset`
    JNotNull {
        reg: Reg,
        offset: JumpOffset,
    },
    /// Jump by an offset if signed lesser than.
    ///
    /// `if a < b jump by offset`
    JSLt {
        a: Reg,
        b: Reg,
        offset: JumpOffset,
    },
    /// Jump by an offset if signed greater than or equal
    ///
    /// `if a >= b jump by offset`
    JSGte {
        a: Reg,
        b: Reg,
        offset: JumpOffset,
    },
    /// Jump by an offset if signed greater than
    ///
    /// `if a > b jump by offset`
    JSGt {
        a: Reg,
        b: Reg,
        offset: JumpOffset,
    },
    /// Jump by an offset if signed lesser than or equal
    ///
    /// `if a < b jump by offset`
    JSLte {
        a: Reg,
        b: Reg,
        offset: JumpOffset,
    },
    /// Jump by an offset if unsigned lesser than
    ///
    /// `if a < b jump by offset`
    JULt {
        a: Reg,
        b: Reg,
        offset: JumpOffset,
    },
    /// Jump by an offset if unsigned greater than or equal
    ///
    /// `if a >= b jump by offset`
    JUGte {
        a: Reg,
        b: Reg,
        offset: JumpOffset,
    },
    /// Jump by an offset if not lesser than
    ///
    /// `if !(a < b) jump by offset`
    JNotLt {
        a: Reg,
        b: Reg,
        offset: JumpOffset,
    },
    /// Jump by an offset if not greater than or equal
    ///
    /// `if !(a >= b) jump by offset`
    JNotGte {
        a: Reg,
        b: Reg,
        offset: JumpOffset,
    },
    /// Jump by an offset if equal
    ///
    /// `if a == b jump by offset`
    JEq {
        a: Reg,
        b: Reg,
        offset: JumpOffset,
    },
    /// Jump by an offset if not equal
    ///
    /// `if a != b jump by offset`
    JNotEq {
        a: Reg,
        b: Reg,
        offset: JumpOffset,
    },
    /// Jump by an offset unconditionally
    ///
    /// `jump by offset`
    JAlways {
        offset: JumpOffset,
    },
    /// Convert a value to a **dyn** value
    ///
    /// `dst = (dyn) src`
    ToDyn {
        dst: Reg,
        src: Reg,
    },
    /// Convert a value to a signed **float**
    ///
    /// `dst = (float) src`
    ToSFloat {
        dst: Reg,
        src: Reg,
    },
    /// Convert a value to an unsigned **float**
    ///
    /// `dst = (float) src`
    ToUFloat {
        dst: Reg,
        src: Reg,
    },
    /// Convert a value to an **int**
    ///
    /// `dst = (int) src`
    ToInt {
        dst: Reg,
        src: Reg,
    },
    /// Cast a value to another type. Throw an exception if the cast is invalid.
    ///
    /// `dst = (typeof dst) src`
    SafeCast {
        dst: Reg,
        src: Reg,
    },
    /// Cast a value to another type. Will not throw an exception. Might crash the program at a later point.
    ///
    /// `dst = (typeof dst) src`
    UnsafeCast {
        dst: Reg,
        src: Reg,
    },
    /// Convert a value to a **virtual** value
    ///
    /// `dst = (virtual) src`
    ToVirtual {
        dst: Reg,
        src: Reg,
    },
    /// No-op, mark a position as being the target of a backward jump. Corresponds to a loop.
    ///
    /// Negative jump offsets must always target a label.
    Label,
    /// Return a value from the current function
    ///
    /// `return ret`
    Ret {
        ret: Reg,
    },
    /// Throw an exception
    Throw {
        exc: Reg,
    },
    /// Rethrow an exception, without touching the exception stack trace.
    Rethrow {
        exc: Reg,
    },
    /// Select a jump offset based on the integer value. The offsets array is no bigger than 255.
    ///
    /// `jump by offsets[reg] else jump by end`
    Switch {
        reg: Reg,
        offsets: Vec<JumpOffset>,
        end: JumpOffset,
    },
    /// Throw an exception if *reg* is null.
    ///
    /// `if reg == null throw exception`
    NullCheck {
        reg: Reg,
    },
    /// Setup a try-catch block. If an exception occurs, store it in the given register and jump by an offset.
    Trap {
        exc: Reg,
        offset: JumpOffset,
    },
    /// End the **latest** trap section.
    EndTrap {
        exc: Reg,
    },
    /// Read an **i8** from a byte array.
    ///
    /// `dst = bytes[index]`
    GetI8 {
        dst: Reg,
        bytes: Reg,
        index: Reg,
    },
    /// Read an **i16** from a byte array.
    ///
    /// `dst = bytes[index]`
    GetI16 {
        dst: Reg,
        bytes: Reg,
        index: Reg,
    },
    /// Read memory directly.
    ///
    /// `dst = bytes[index]`
    GetMem {
        dst: Reg,
        bytes: Reg,
        index: Reg,
    },
    /// Get the value of an array at an index.
    ///
    /// `dst = array[index]`
    GetArray {
        dst: Reg,
        array: Reg,
        index: Reg,
    },
    /// Write an **i8** to a byte array.
    ///
    /// `bytes[index] = src`
    SetI8 {
        bytes: Reg,
        index: Reg,
        src: Reg,
    },
    /// Write an **i16** to a byte array.
    ///
    /// `bytes[index] = src`
    SetI16 {
        bytes: Reg,
        index: Reg,
        src: Reg,
    },
    /// Write to memory directly.
    ///
    /// `bytes[index] = src`
    SetMem {
        bytes: Reg,
        index: Reg,
        src: Reg,
    },
    /// Write a value in an array.
    ///
    /// `array[index] = src`
    SetArray {
        array: Reg,
        index: Reg,
        src: Reg,
    },
    /// Allocate an object.
    ///
    /// `dst = new (typeof dst)`
    New {
        dst: Reg,
    },
    /// Get the length of an array.
    ///
    /// `dst = len(array)`
    ArraySize {
        dst: Reg,
        array: Reg,
    },
    /// Get the type object from its identifier.
    ///
    /// `dst = type ty`
    Type {
        dst: Reg,
        ty: RefType,
    },
    /// Get the type object of a value.
    ///
    /// `dst = typeof src`
    GetType {
        dst: Reg,
        src: Reg,
    },
    /// Get the type kind identifier of a value. Useful for switch statements on types.
    ///
    /// `dst = typeof src`
    GetTID {
        dst: Reg,
        src: Reg,
    },
    /// Get a reference to a value.
    ///
    /// `dst = &src`
    Ref {
        dst: Reg,
        src: Reg,
    },
    /// Read a reference value.
    ///
    /// `dst = *src`
    Unref {
        dst: Reg,
        src: Reg,
    },
    /// Write into a reference value.
    ///
    /// `*dst = src`
    Setref {
        dst: Reg,
        value: Reg,
    },
    /// Create an enum variant.
    ///
    /// `dst = construct(args...)`
    MakeEnum {
        dst: Reg,
        construct: RefEnumConstruct,
        args: Vec<Reg>,
    },
    /// Create an enum variant using the default values.
    ///
    /// `dst = construct()`
    EnumAlloc {
        dst: Reg,
        construct: RefEnumConstruct,
    },
    /// Get the enum value variant index (the enum tag). Useful for switch statements.
    ///
    /// `dst = variantof value`
    EnumIndex {
        dst: Reg,
        value: Reg,
    },
    /// Access a field of an enum.
    ///
    /// `dst = (value as construct).field`
    EnumField {
        dst: Reg,
        value: Reg,
        construct: RefEnumConstruct,
        field: RefField,
    },
    /// Set a field of an enum. Uses the first enum variant.
    ///
    /// `value.field = src`
    SetEnumField {
        value: Reg,
        field: RefField,
        src: Reg,
    },
    /// Debug break, calls `hl_assert()` under the hood.
    Assert,
    // Not sure what those last 2 opcodes do.
    RefData {
        dst: Reg,
        src: Reg,
    },
    RefOffset {
        dst: Reg,
        reg: Reg,
        offset: Reg,
    },
    /// No-op, useful to mark removed opcodes without breaking jump offsets.
    Nop,
    /// x86 prefetch. Move data closer to the processor using hints.
    Prefetch {
        /// Value to prefetch
        value: Reg,
        /// Non-zero if we are accessing a field in value.
        field: RefField,
        /// https://github.com/HaxeFoundation/hashlink/blob/733b6a14a0a7e7cfba6c21cdf0ee03595cafafb4/src/jit.c#L4310
        /// https://www.felixcloutier.com/x86/prefetchh
        /// https://www.felixcloutier.com/x86/prefetchw
        mode: InlineInt,
    },
    /// Inline x86 assembly
    Asm {
        /// https://github.com/HaxeFoundation/hashlink/blob/733b6a14a0a7e7cfba6c21cdf0ee03595cafafb4/src/jit.c#L4334
        mode: InlineInt,
        value: InlineInt,
        /// Warning ! Only non-zero values indicates valid reg. Register index is reg-1.
        reg: Reg,
    },
}

impl Opcode {
    pub fn from_native(opcode: hl_opcode) -> Opcode {
        match opcode.op {
            hl_op_OMov => Opcode::Mov {
                dst: Reg(opcode.p1 as u32),
                src: Reg(opcode.p2 as u32),
            },
            hl_op_OInt => Opcode::Int {
                dst: Reg(opcode.p1 as u32),
                ptr: RefInt(opcode.p2 as usize),
            },
            hl_op_OFloat => Opcode::Float {
                dst: Reg(opcode.p1 as u32),
                ptr: RefFloat(opcode.p2 as usize),
            },
            hl_op_OBool => Opcode::Bool {
                dst: Reg(opcode.p1 as u32),
                value: opcode.p2 != 0,
            },
            hl_op_OBytes => Opcode::Bytes {
                dst: Reg(opcode.p1 as u32),
                ptr: RefBytes(opcode.p2 as usize),
            },
            hl_op_OString => Opcode::String {
                dst: Reg(opcode.p1 as u32),
                ptr: RefString(opcode.p2 as usize),
            },
            hl_op_ONull => Opcode::Null {
                dst: Reg(opcode.p1 as u32),
            },
            hl_op_OAdd => Opcode::Add {
                dst: Reg(opcode.p1 as u32),
                a: Reg(opcode.p2 as u32),
                b: Reg(opcode.p3 as u32),
            },
            hl_op_OSub => Opcode::Sub {
                dst: Reg(opcode.p1 as u32),
                a: Reg(opcode.p2 as u32),
                b: Reg(opcode.p3 as u32),
            },
            hl_op_OMul => Opcode::Mul {
                dst: Reg(opcode.p1 as u32),
                a: Reg(opcode.p2 as u32),
                b: Reg(opcode.p3 as u32),
            },
            hl_op_OSDiv => Opcode::SDiv {
                dst: Reg(opcode.p1 as u32),
                a: Reg(opcode.p2 as u32),
                b: Reg(opcode.p3 as u32),
            },
            hl_op_OUDiv => Opcode::UDiv {
                dst: Reg(opcode.p1 as u32),
                a: Reg(opcode.p2 as u32),
                b: Reg(opcode.p3 as u32),
            },
            hl_op_OSMod => Opcode::SMod {
                dst: Reg(opcode.p1 as u32),
                a: Reg(opcode.p2 as u32),
                b: Reg(opcode.p3 as u32),
            },
            hl_op_OUMod => Opcode::UMod {
                dst: Reg(opcode.p1 as u32),
                a: Reg(opcode.p2 as u32),
                b: Reg(opcode.p3 as u32),
            },
            hl_op_OShl => Opcode::Shl {
                dst: Reg(opcode.p1 as u32),
                a: Reg(opcode.p2 as u32),
                b: Reg(opcode.p3 as u32),
            },
            hl_op_OSShr => Opcode::SShr {
                dst: Reg(opcode.p1 as u32),
                a: Reg(opcode.p2 as u32),
                b: Reg(opcode.p3 as u32),
            },
            hl_op_OUShr => Opcode::UShr {
                dst: Reg(opcode.p1 as u32),
                a: Reg(opcode.p2 as u32),
                b: Reg(opcode.p3 as u32),
            },
            hl_op_OAnd => Opcode::And {
                dst: Reg(opcode.p1 as u32),
                a: Reg(opcode.p2 as u32),
                b: Reg(opcode.p3 as u32),
            },
            hl_op_OOr => Opcode::Or {
                dst: Reg(opcode.p1 as u32),
                a: Reg(opcode.p2 as u32),
                b: Reg(opcode.p3 as u32),
            },
            hl_op_OXor => Opcode::Xor {
                dst: Reg(opcode.p1 as u32),
                a: Reg(opcode.p2 as u32),
                b: Reg(opcode.p3 as u32),
            },
            hl_op_ONeg => Opcode::Neg {
                dst: Reg(opcode.p1 as u32),
                src: Reg(opcode.p2 as u32),
            },
            hl_op_ONot => Opcode::Not {
                dst: Reg(opcode.p1 as u32),
                src: Reg(opcode.p2 as u32),
            },
            hl_op_OIncr => Opcode::Incr {
                dst: Reg(opcode.p1 as u32),
            },
            hl_op_ODecr => Opcode::Decr {
                dst: Reg(opcode.p1 as u32),
            },
            hl_op_OCall0 => Opcode::Call0 {
                dst: Reg(opcode.p1 as u32),
                fun: RefFun(opcode.p2 as usize),
            },
            hl_op_OCall1 => Opcode::Call1 {
                dst: Reg(opcode.p1 as u32),
                fun: RefFun(opcode.p2 as usize),
                arg0: Reg(opcode.p3 as u32),
            },
            hl_op_OCall2 => {
                let extras = unsafe { slice::from_raw_parts(opcode.extra, 1) };
                Opcode::Call2 {
                    dst: Reg(opcode.p1 as u32),
                    fun: RefFun(opcode.p2 as usize),
                    arg0: Reg(opcode.p3 as u32),
                    arg1: Reg(extras[0] as u32),
                }
            }
            hl_op_OCall3 => {
                let extras = unsafe { Vec::<i32>::from_raw_parts(opcode.extra, 2, 2) };
                println!("extras {:?}", extras);
               let code = Opcode::Call3 {
                    dst: Reg(opcode.p1 as u32),
                    fun: RefFun(opcode.p2 as usize),
                    arg0: Reg(opcode.p3 as u32),
                    arg1: Reg(extras[0] as u32),
                    arg2: Reg(extras[1] as u32),
                };

                unsafe {
                    std::mem::forget(extras);
                }

                code
            }
            hl_op_OCall4 => {
                let extras = unsafe { slice::from_raw_parts(opcode.extra, 3) };
                Opcode::Call4 {
                    dst: Reg(opcode.p1 as u32),
                    fun: RefFun(opcode.p2 as usize),
                    arg0: Reg(opcode.p3 as u32),
                    arg1: Reg(extras[0] as u32),
                    arg2: Reg(extras[1] as u32),
                    arg3: Reg(extras[2] as u32),
                }
            }
            hl_op_OCallN => {
                let extras = unsafe { slice::from_raw_parts(opcode.extra, opcode.p3 as usize) };
                let mut args = Vec::new();
                for i in 0..opcode.p3 as usize {
                    args.push(Reg(extras[i] as u32));
                }
                Opcode::CallN {
                    dst: Reg(opcode.p1 as u32),
                    fun: RefFun(opcode.p2 as usize),
                    args,
                }
            }
            hl_op_OCallMethod => {
                let extras = unsafe { slice::from_raw_parts(opcode.extra, opcode.p3 as usize) };
                let mut args = Vec::new();
                for i in 0..opcode.p3 as usize {
                    args.push(Reg(extras[i] as u32));
                }
                Opcode::CallMethod {
                    dst: Reg(opcode.p1 as u32),
                    field: RefField(opcode.p2 as usize),
                    args,
                }
            }
            hl_op_OCallThis => {
                let extras = unsafe { slice::from_raw_parts(opcode.extra, opcode.p3 as usize) };
                let mut args = Vec::new();
                for i in 0..opcode.p3 as usize {
                    args.push(Reg(extras[i] as u32));
                }
                Opcode::CallThis {
                    dst: Reg(opcode.p1 as u32),
                    field: RefField(opcode.p2 as usize),
                    args,
                }
            }
            hl_op_OCallClosure => {
                let extras = unsafe { slice::from_raw_parts(opcode.extra, opcode.p3 as usize) };
                let mut args = Vec::new();
                for i in 0..opcode.p3 as usize {
                    args.push(Reg(extras[i] as u32));
                }
                Opcode::CallClosure {
                    dst: Reg(opcode.p1 as u32),
                    fun: Reg(opcode.p2 as u32),
                    args,
                }
            }
            hl_op_OStaticClosure => Opcode::StaticClosure {
                dst: Reg(opcode.p1 as u32),
                fun: RefFun(opcode.p2 as usize),
            },
            hl_op_OInstanceClosure => Opcode::InstanceClosure {
                dst: Reg(opcode.p1 as u32),
                fun: RefFun(opcode.p2 as usize),
                obj: Reg(opcode.p3 as u32),
            },
            hl_op_OVirtualClosure => Opcode::VirtualClosure {
                dst: Reg(opcode.p1 as u32),
                obj: Reg(opcode.p2 as u32),
                field: Reg(opcode.p3 as u32),
            },
            hl_op_OGetGlobal => Opcode::GetGlobal {
                dst: Reg(opcode.p1 as u32),
                global: RefGlobal(opcode.p2 as usize),
            },
            hl_op_OSetGlobal => Opcode::SetGlobal {
                global: RefGlobal(opcode.p1 as usize),
                src: Reg(opcode.p2 as u32),
            },
            hl_op_OField => Opcode::Field {
                dst: Reg(opcode.p1 as u32),
                obj: Reg(opcode.p2 as u32),
                field: RefField(opcode.p3 as usize),
            },
            hl_op_OSetField => Opcode::SetField {
                obj: Reg(opcode.p1 as u32),
                field: RefField(opcode.p2 as usize),
                src: Reg(opcode.p3 as u32),
            },
            hl_op_OGetThis => Opcode::GetThis {
                dst: Reg(opcode.p1 as u32),
                field: RefField(opcode.p2 as usize),
            },
            hl_op_OSetThis => Opcode::SetThis {
                field: RefField(opcode.p1 as usize),
                src: Reg(opcode.p2 as u32),
            },
            hl_op_ODynGet => Opcode::DynGet {
                dst: Reg(opcode.p1 as u32),
                obj: Reg(opcode.p2 as u32),
                field: RefString(opcode.p3 as usize),
            },
            hl_op_ODynSet => Opcode::DynSet {
                obj: Reg(opcode.p1 as u32),
                field: RefString(opcode.p2 as usize),
                src: Reg(opcode.p3 as u32),
            },
            hl_op_OJTrue => Opcode::JTrue {
                cond: Reg(opcode.p1 as u32),
                offset: opcode.p2 as JumpOffset,
            },
            hl_op_OJFalse => Opcode::JFalse {
                cond: Reg(opcode.p1 as u32),
                offset: opcode.p2 as JumpOffset,
            },
            hl_op_OJNull => Opcode::JNull {
                reg: Reg(opcode.p1 as u32),
                offset: opcode.p2 as JumpOffset,
            },
            hl_op_OJNotNull => Opcode::JNotNull {
                reg: Reg(opcode.p1 as u32),
                offset: opcode.p2 as JumpOffset,
            },
            hl_op_OJSLt => Opcode::JSLt {
                a: Reg(opcode.p1 as u32),
                b: Reg(opcode.p2 as u32),
                offset: opcode.p3 as JumpOffset,
            },
            hl_op_OJSGte => Opcode::JSGte {
                a: Reg(opcode.p1 as u32),
                b: Reg(opcode.p2 as u32),
                offset: opcode.p3 as JumpOffset,
            },
            hl_op_OJSGt => Opcode::JSGt {
                a: Reg(opcode.p1 as u32),
                b: Reg(opcode.p2 as u32),
                offset: opcode.p3 as JumpOffset,
            },
            hl_op_OJSLte => Opcode::JSLte {
                a: Reg(opcode.p1 as u32),
                b: Reg(opcode.p2 as u32),
                offset: opcode.p3 as JumpOffset,
            },
            hl_op_OJULt => Opcode::JULt {
                a: Reg(opcode.p1 as u32),
                b: Reg(opcode.p2 as u32),
                offset: opcode.p3 as JumpOffset,
            },
            hl_op_OJUGte => Opcode::JUGte {
                a: Reg(opcode.p1 as u32),
                b: Reg(opcode.p2 as u32),
                offset: opcode.p3 as JumpOffset,
            },
            hl_op_OJNotLt => Opcode::JNotLt {
                a: Reg(opcode.p1 as u32),
                b: Reg(opcode.p2 as u32),
                offset: opcode.p3 as JumpOffset,
            },
            hl_op_OJNotGte => Opcode::JNotGte {
                a: Reg(opcode.p1 as u32),
                b: Reg(opcode.p2 as u32),
                offset: opcode.p3 as JumpOffset,
            },
            hl_op_OJEq => Opcode::JEq {
                a: Reg(opcode.p1 as u32),
                b: Reg(opcode.p2 as u32),
                offset: opcode.p3 as JumpOffset,
            },
            hl_op_OJNotEq => Opcode::JNotEq {
                a: Reg(opcode.p1 as u32),
                b: Reg(opcode.p2 as u32),
                offset: opcode.p3 as JumpOffset,
            },
            hl_op_OJAlways => Opcode::JAlways {
                offset: opcode.p1 as JumpOffset,
            },
            hl_op_OToDyn => Opcode::ToDyn {
                dst: Reg(opcode.p1 as u32),
                src: Reg(opcode.p2 as u32),
            },
            hl_op_OToSFloat => Opcode::ToSFloat {
                dst: Reg(opcode.p1 as u32),
                src: Reg(opcode.p2 as u32),
            },
            hl_op_OToUFloat => Opcode::ToUFloat {
                dst: Reg(opcode.p1 as u32),
                src: Reg(opcode.p2 as u32),
            },
            hl_op_OToInt => Opcode::ToInt {
                dst: Reg(opcode.p1 as u32),
                src: Reg(opcode.p2 as u32),
            },
            hl_op_OSafeCast => Opcode::SafeCast {
                dst: Reg(opcode.p1 as u32),
                src: Reg(opcode.p2 as u32),
            },
            hl_op_OUnsafeCast => Opcode::UnsafeCast {
                dst: Reg(opcode.p1 as u32),
                src: Reg(opcode.p2 as u32),
            },
            hl_op_OToVirtual => Opcode::ToVirtual {
                dst: Reg(opcode.p1 as u32),
                src: Reg(opcode.p2 as u32),
            },
            hl_op_OLabel => Opcode::Label,
            hl_op_ORet => Opcode::Ret {
                ret: Reg(opcode.p1 as u32),
            },
            hl_op_OThrow => Opcode::Throw {
                exc: Reg(opcode.p1 as u32),
            },
            hl_op_ORethrow => Opcode::Rethrow {
                exc: Reg(opcode.p1 as u32),
            },
            hl_op_OSwitch => {
                let extras = unsafe { slice::from_raw_parts(opcode.extra, opcode.p2 as usize) };
                let mut offsets = Vec::new();
                for i in 0..opcode.p2 as usize {
                    offsets.push(extras[i] as JumpOffset);
                }
                Opcode::Switch {
                    reg: Reg(opcode.p1 as u32),
                    offsets,
                    end: opcode.p3 as JumpOffset,
                }
            }
            hl_op_ONullCheck => Opcode::NullCheck {
                reg: Reg(opcode.p1 as u32),
            },
            hl_op_OTrap => Opcode::Trap {
                exc: Reg(opcode.p1 as u32),
                offset: opcode.p2 as JumpOffset,
            },
            hl_op_OEndTrap => Opcode::EndTrap {
                exc: Reg(opcode.p1 as u32),
            },
            hl_op_OGetI8 => Opcode::GetI8 {
                dst: Reg(opcode.p1 as u32),
                bytes: Reg(opcode.p2 as u32),
                index: Reg(opcode.p3 as u32),
            },
            hl_op_OGetI16 => Opcode::GetI16 {
                dst: Reg(opcode.p1 as u32),
                bytes: Reg(opcode.p2 as u32),
                index: Reg(opcode.p3 as u32),
            },
            hl_op_OGetMem => Opcode::GetMem {
                dst: Reg(opcode.p1 as u32),
                bytes: Reg(opcode.p2 as u32),
                index: Reg(opcode.p3 as u32),
            },
            hl_op_OGetArray => Opcode::GetArray {
                dst: Reg(opcode.p1 as u32),
                array: Reg(opcode.p2 as u32),
                index: Reg(opcode.p3 as u32),
            },
            hl_op_OSetI8 => Opcode::SetI8 {
                bytes: Reg(opcode.p1 as u32),
                index: Reg(opcode.p2 as u32),
                src: Reg(opcode.p3 as u32),
            },
            hl_op_OSetI16 => Opcode::SetI16 {
                bytes: Reg(opcode.p1 as u32),
                index: Reg(opcode.p2 as u32),
                src: Reg(opcode.p3 as u32),
            },
            hl_op_OSetMem => Opcode::SetMem {
                bytes: Reg(opcode.p1 as u32),
                index: Reg(opcode.p2 as u32),
                src: Reg(opcode.p3 as u32),
            },
            hl_op_OSetArray => Opcode::SetArray {
                array: Reg(opcode.p1 as u32),
                index: Reg(opcode.p2 as u32),
                src: Reg(opcode.p3 as u32),
            },
            hl_op_ONew => Opcode::New {
                dst: Reg(opcode.p1 as u32),
            },
            hl_op_OArraySize => Opcode::ArraySize {
                dst: Reg(opcode.p1 as u32),
                array: Reg(opcode.p2 as u32),
            },
            hl_op_OType => Opcode::Type {
                dst: Reg(opcode.p1 as u32),
                ty: RefType(opcode.p2 as usize),
            },
            hl_op_OGetType => Opcode::GetType {
                dst: Reg(opcode.p1 as u32),
                src: Reg(opcode.p2 as u32),
            },
            hl_op_OGetTID => Opcode::GetTID {
                dst: Reg(opcode.p1 as u32),
                src: Reg(opcode.p2 as u32),
            },
            hl_op_ORef => Opcode::Ref {
                dst: Reg(opcode.p1 as u32),
                src: Reg(opcode.p2 as u32),
            },
            hl_op_OUnref => Opcode::Unref {
                dst: Reg(opcode.p1 as u32),
                src: Reg(opcode.p2 as u32),
            },
            hl_op_OSetref => Opcode::Setref {
                dst: Reg(opcode.p1 as u32),
                value: Reg(opcode.p2 as u32),
            },
            hl_op_OMakeEnum => {
                let extras = unsafe { slice::from_raw_parts(opcode.extra, opcode.p3 as usize) };
                let mut args = Vec::new();
                for i in 0..opcode.p3 as usize {
                    args.push(Reg(extras[i] as u32));
                }
                Opcode::MakeEnum {
                    dst: Reg(opcode.p1 as u32),
                    construct: RefEnumConstruct(opcode.p2 as usize),
                    args,
                }
            }
            hl_op_OEnumAlloc => Opcode::EnumAlloc {
                dst: Reg(opcode.p1 as u32),
                construct: RefEnumConstruct(opcode.p2 as usize),
            },
            hl_op_OEnumIndex => Opcode::EnumIndex {
                dst: Reg(opcode.p1 as u32),
                value: Reg(opcode.p2 as u32),
            },
            hl_op_OEnumField => {
                let extras = unsafe { slice::from_raw_parts(opcode.extra, 1) };
                Opcode::EnumField {
                    dst: Reg(opcode.p1 as u32),
                    value: Reg(opcode.p2 as u32),
                    construct: RefEnumConstruct(opcode.p3 as usize),
                    field: RefField(extras[0] as usize),
                }
            }
            hl_op_OSetEnumField => Opcode::SetEnumField {
                value: Reg(opcode.p1 as u32),
                field: RefField(opcode.p2 as usize),
                src: Reg(opcode.p3 as u32),
            },
            hl_op_OAssert => Opcode::Assert,
            hl_op_ORefData => Opcode::RefData {
                dst: Reg(opcode.p1 as u32),
                src: Reg(opcode.p2 as u32),
            },
            hl_op_ORefOffset => Opcode::RefOffset {
                dst: Reg(opcode.p1 as u32),
                reg: Reg(opcode.p2 as u32),
                offset: Reg(opcode.p3 as u32),
            },
            hl_op_ONop => Opcode::Nop,
            hl_op_OPrefetch => Opcode::Prefetch {
                value: Reg(opcode.p1 as u32),
                field: RefField(opcode.p2 as usize),
                mode: opcode.p3,
            },
            hl_op_OAsm => Opcode::Asm {
                mode: opcode.p1,
                value: opcode.p2,
                reg: Reg(opcode.p3 as u32),
            },
            _ => panic!("Unknown opcode: {:?}", opcode.op),
        }
    }
}
