// HashLink bytecode opcode definitions.
// Originally from HLBC https://github.com/Gui-Yom/hlbc/blob/master/crates/hlbc/src/types.rs
// Copyright to the hlbc authors.

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
    /// `dst = !src`
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
    /// `this.field = src`
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
    /// `if a <= b jump by offset`
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
    /// Cast a value to another type. Will not throw an exception.
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
    /// Select a jump offset based on the integer value.
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
    /// Setup a try-catch block.
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
    /// Get the type kind identifier of a value.
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
    /// Get the enum value variant index (the enum tag).
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
    /// Set a field of an enum.
    ///
    /// `value.field = src`
    SetEnumField {
        value: Reg,
        field: RefField,
        src: Reg,
    },
    /// Debug break.
    Assert,
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
    /// x86 prefetch hint.
    Prefetch {
        value: Reg,
        field: RefField,
        mode: InlineInt,
    },
    /// Inline x86 assembly
    Asm {
        mode: InlineInt,
        value: InlineInt,
        reg: Reg,
    },
}
