// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! High-Level Intermediate Representation (HIR) for Oblíbený.
//!
//! HIR is a desugared AST with:
//! - All macros expanded
//! - Explicit type annotations on all nodes
//! - Simplified control flow
//! - No syntactic sugar
//! - Symbol IDs instead of names

use crate::ast::{
    ArithmeticOp, BinaryArithOp, CheckpointOp, ComparisonOp, DataSize, DataTransferType,
    DeployTarget, JumpOp, LogicalOp, MemoryOpType, ObfuscationLevel, PrimitiveType,
    ResourceBounds, StackOpType, TargetProfile,
};
use crate::span::Span;
use crate::symbol::{SymbolId, SymbolPhase};

/// A HIR program is a collection of items.
#[derive(Debug, Clone)]
pub struct HirProgram {
    /// All items in the program.
    pub items: Vec<HirItem>,
    /// Entry point function (if any).
    pub entry: Option<SymbolId>,
}

/// Top-level HIR items.
#[derive(Debug, Clone)]
pub enum HirItem {
    /// Function definition.
    Function(HirFunction),
    /// Global variable.
    Global(HirGlobal),
    /// Constant.
    Constant(HirConstant),
    /// Struct definition.
    Struct(HirStruct),
    /// Enum definition.
    Enum(HirEnum),
    /// Deployment specification.
    Deployment(HirDeployment),
}

/// A function in HIR.
#[derive(Debug, Clone)]
pub struct HirFunction {
    /// Symbol ID of the function.
    pub id: SymbolId,
    /// Function name (for debugging).
    pub name: String,
    /// Parameters.
    pub params: Vec<HirParam>,
    /// Return type.
    pub return_type: HirType,
    /// Function body.
    pub body: HirBlock,
    /// Phase (compile-only, deploy, or both).
    pub phase: SymbolPhase,
    /// Is this function pure?
    pub is_pure: bool,
    /// Maximum iterations (for bounded loops).
    pub max_iterations: Option<u64>,
    /// Source span.
    pub span: Span,
}

/// A function parameter.
#[derive(Debug, Clone)]
pub struct HirParam {
    /// Symbol ID.
    pub id: SymbolId,
    /// Parameter name.
    pub name: String,
    /// Parameter type.
    pub ty: HirType,
    /// Source span.
    pub span: Span,
}

/// A global variable.
#[derive(Debug, Clone)]
pub struct HirGlobal {
    /// Symbol ID.
    pub id: SymbolId,
    /// Variable name.
    pub name: String,
    /// Type.
    pub ty: HirType,
    /// Initial value (must be constant).
    pub init: Option<HirConstValue>,
    /// Is mutable?
    pub is_mutable: bool,
    /// Phase availability.
    pub phase: SymbolPhase,
    /// Source span.
    pub span: Span,
}

/// A constant definition.
#[derive(Debug, Clone)]
pub struct HirConstant {
    /// Symbol ID.
    pub id: SymbolId,
    /// Constant name.
    pub name: String,
    /// Type.
    pub ty: HirType,
    /// Value.
    pub value: HirConstValue,
    /// Source span.
    pub span: Span,
}

/// A struct definition.
#[derive(Debug, Clone)]
pub struct HirStruct {
    /// Symbol ID.
    pub id: SymbolId,
    /// Struct name.
    pub name: String,
    /// Fields.
    pub fields: Vec<HirField>,
    /// Source span.
    pub span: Span,
}

/// A struct field.
#[derive(Debug, Clone)]
pub struct HirField {
    /// Field name.
    pub name: String,
    /// Field type.
    pub ty: HirType,
    /// Field offset (computed later).
    pub offset: Option<u64>,
}

/// An enum definition.
#[derive(Debug, Clone)]
pub struct HirEnum {
    /// Symbol ID.
    pub id: SymbolId,
    /// Enum name.
    pub name: String,
    /// Variants.
    pub variants: Vec<HirVariant>,
    /// Source span.
    pub span: Span,
}

/// An enum variant.
#[derive(Debug, Clone)]
pub struct HirVariant {
    /// Variant name.
    pub name: String,
    /// Discriminant value.
    pub discriminant: u64,
    /// Associated data types (if any).
    pub fields: Vec<HirType>,
}

/// Deployment specification.
#[derive(Debug, Clone)]
pub struct HirDeployment {
    /// Target profile.
    pub profile: TargetProfile,
    /// Resource bounds.
    pub bounds: ResourceBounds,
    /// Obfuscation level.
    pub obfuscation: ObfuscationLevel,
    /// Source span.
    pub span: Span,
}

/// HIR types.
#[derive(Debug, Clone, PartialEq)]
pub enum HirType {
    /// Unit type (void).
    Unit,
    /// Boolean.
    Bool,
    /// Integer types.
    Int(IntType),
    /// Float types.
    Float(FloatType),
    /// Array with known size.
    Array(Box<HirType>, u64),
    /// Pointer to type.
    Ptr(Box<HirType>),
    /// Struct type (by symbol ID).
    Struct(SymbolId),
    /// Enum type (by symbol ID).
    Enum(SymbolId),
    /// Function type.
    Func {
        params: Vec<HirType>,
        ret: Box<HirType>,
    },
    /// Error type (for error recovery).
    Error,
}

/// Integer types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntType {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
}

/// Float types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FloatType {
    F32,
    F64,
}

impl HirType {
    /// Convert from AST primitive type.
    pub fn from_primitive(p: PrimitiveType) -> Self {
        match p {
            PrimitiveType::U8 => HirType::Int(IntType::U8),
            PrimitiveType::U16 => HirType::Int(IntType::U16),
            PrimitiveType::U32 => HirType::Int(IntType::U32),
            PrimitiveType::U64 => HirType::Int(IntType::U64),
            PrimitiveType::I8 => HirType::Int(IntType::I8),
            PrimitiveType::I16 => HirType::Int(IntType::I16),
            PrimitiveType::I32 => HirType::Int(IntType::I32),
            PrimitiveType::I64 => HirType::Int(IntType::I64),
            PrimitiveType::F32 => HirType::Float(FloatType::F32),
            PrimitiveType::F64 => HirType::Float(FloatType::F64),
            PrimitiveType::Bool => HirType::Bool,
            PrimitiveType::String => HirType::Ptr(Box::new(HirType::Int(IntType::U8))),
        }
    }

    /// Get the size of this type in bytes.
    pub fn size(&self) -> u64 {
        match self {
            HirType::Unit => 0,
            HirType::Bool => 1,
            HirType::Int(it) => match it {
                IntType::I8 | IntType::U8 => 1,
                IntType::I16 | IntType::U16 => 2,
                IntType::I32 | IntType::U32 => 4,
                IntType::I64 | IntType::U64 => 8,
            },
            HirType::Float(ft) => match ft {
                FloatType::F32 => 4,
                FloatType::F64 => 8,
            },
            HirType::Array(elem, size) => elem.size() * size,
            HirType::Ptr(_) => 8, // Assuming 64-bit pointers
            HirType::Struct(_) => 0, // Must be computed from struct definition
            HirType::Enum(_) => 8,   // Tag + max variant size
            HirType::Func { .. } => 8, // Function pointer
            HirType::Error => 0,
        }
    }

    /// Check if this type is numeric.
    pub fn is_numeric(&self) -> bool {
        matches!(self, HirType::Int(_) | HirType::Float(_))
    }

    /// Check if this type is integral.
    pub fn is_integral(&self) -> bool {
        matches!(self, HirType::Int(_))
    }
}

/// Constant values (compile-time known).
#[derive(Debug, Clone, PartialEq)]
pub enum HirConstValue {
    /// Unit value.
    Unit,
    /// Boolean.
    Bool(bool),
    /// Integer.
    Int(i64, IntType),
    /// Unsigned integer.
    UInt(u64, IntType),
    /// Float.
    Float(f64, FloatType),
    /// Array of constants.
    Array(Vec<HirConstValue>),
    /// Struct literal.
    Struct(SymbolId, Vec<HirConstValue>),
    /// Zero-initialized value.
    Zero(HirType),
}

/// A block of statements.
#[derive(Debug, Clone)]
pub struct HirBlock {
    /// Statements in the block.
    pub stmts: Vec<HirStmt>,
    /// Final expression (if any).
    pub expr: Option<Box<HirExpr>>,
    /// Source span.
    pub span: Span,
}

/// HIR statements.
#[derive(Debug, Clone)]
pub enum HirStmt {
    /// Local variable declaration.
    Let {
        id: SymbolId,
        name: String,
        ty: HirType,
        init: Option<HirExpr>,
        span: Span,
    },
    /// Assignment.
    Assign {
        target: HirPlace,
        value: HirExpr,
        span: Span,
    },
    /// Expression statement (for side effects).
    Expr(HirExpr),
    /// If statement.
    If {
        cond: HirExpr,
        then_block: HirBlock,
        else_block: Option<HirBlock>,
        span: Span,
    },
    /// Bounded loop.
    BoundedLoop {
        var: SymbolId,
        start: HirExpr,
        end: HirExpr,
        body: HirBlock,
        span: Span,
    },
    /// While loop (compile-time only).
    While {
        cond: HirExpr,
        body: HirBlock,
        span: Span,
    },
    /// Return statement.
    Return {
        value: Option<HirExpr>,
        span: Span,
    },
    /// Break statement.
    Break { span: Span },
    /// Continue statement.
    Continue { span: Span },
    /// Assembly instruction.
    Asm(HirAsm),
    /// Label definition.
    Label { name: String, span: Span },
}

/// A place (assignable location).
#[derive(Debug, Clone)]
pub enum HirPlace {
    /// Local variable.
    Local(SymbolId),
    /// Global variable.
    Global(SymbolId),
    /// Array element.
    Index {
        base: Box<HirPlace>,
        index: Box<HirExpr>,
    },
    /// Struct field.
    Field { base: Box<HirPlace>, field: String },
    /// Pointer dereference.
    Deref(Box<HirExpr>),
}

/// HIR expressions.
#[derive(Debug, Clone)]
pub enum HirExpr {
    /// Constant value.
    Const(HirConstValue, HirType, Span),
    /// Read a local or global variable.
    Read(HirPlace, HirType, Span),
    /// Binary operation.
    Binary {
        op: HirBinOp,
        left: Box<HirExpr>,
        right: Box<HirExpr>,
        ty: HirType,
        span: Span,
    },
    /// Unary operation.
    Unary {
        op: HirUnaryOp,
        operand: Box<HirExpr>,
        ty: HirType,
        span: Span,
    },
    /// Function call.
    Call {
        func: SymbolId,
        args: Vec<HirExpr>,
        ty: HirType,
        span: Span,
    },
    /// Type cast.
    Cast {
        expr: Box<HirExpr>,
        from: HirType,
        to: HirType,
        span: Span,
    },
    /// Address-of.
    AddrOf {
        place: HirPlace,
        ty: HirType,
        span: Span,
    },
    /// Array indexing.
    Index {
        array: Box<HirExpr>,
        index: Box<HirExpr>,
        ty: HirType,
        span: Span,
    },
    /// Struct field access.
    Field {
        base: Box<HirExpr>,
        field: String,
        ty: HirType,
        span: Span,
    },
    /// Struct literal.
    Struct {
        struct_id: SymbolId,
        ty: HirType,
        fields: Vec<(String, HirExpr)>,
        span: Span,
    },
    /// Array literal.
    Array {
        elements: Vec<HirExpr>,
        ty: HirType,
        span: Span,
    },
    /// Capability invocation.
    Capability {
        op: String,
        token: Box<HirExpr>,
        args: Vec<HirExpr>,
        ty: HirType,
        span: Span,
    },
}

impl HirExpr {
    /// Get the type of this expression.
    pub fn ty(&self) -> &HirType {
        match self {
            HirExpr::Const(_, ty, _) => ty,
            HirExpr::Read(_, ty, _) => ty,
            HirExpr::Binary { ty, .. } => ty,
            HirExpr::Unary { ty, .. } => ty,
            HirExpr::Call { ty, .. } => ty,
            HirExpr::Cast { to, .. } => to,
            HirExpr::AddrOf { ty, .. } => ty,
            HirExpr::Index { ty, .. } => ty,
            HirExpr::Field { ty, .. } => ty,
            HirExpr::Struct { ty, .. } => ty,
            HirExpr::Array { ty, .. } => ty,
            HirExpr::Capability { ty, .. } => ty,
        }
    }

    /// Get the span of this expression.
    pub fn span(&self) -> Span {
        match self {
            HirExpr::Const(_, _, span) => *span,
            HirExpr::Read(_, _, span) => *span,
            HirExpr::Binary { span, .. } => *span,
            HirExpr::Unary { span, .. } => *span,
            HirExpr::Call { span, .. } => *span,
            HirExpr::Cast { span, .. } => *span,
            HirExpr::AddrOf { span, .. } => *span,
            HirExpr::Index { span, .. } => *span,
            HirExpr::Field { span, .. } => *span,
            HirExpr::Struct { span, .. } => *span,
            HirExpr::Array { span, .. } => *span,
            HirExpr::Capability { span, .. } => *span,
        }
    }
}

/// Binary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HirBinOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    // Bitwise
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,

    // Comparison
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,

    // Logical
    And,
    Or,
}

impl HirBinOp {
    /// Convert from AST arithmetic op.
    pub fn from_arith(op: BinaryArithOp) -> Self {
        match op {
            BinaryArithOp::Add => HirBinOp::Add,
            BinaryArithOp::Sub => HirBinOp::Sub,
            BinaryArithOp::Mul => HirBinOp::Mul,
            BinaryArithOp::Div => HirBinOp::Div,
            BinaryArithOp::Mod => HirBinOp::Mod,
        }
    }

    /// Convert from AST comparison op.
    pub fn from_cmp(op: ComparisonOp) -> Self {
        match op {
            ComparisonOp::Eq => HirBinOp::Eq,
            ComparisonOp::Neq => HirBinOp::Ne,
            ComparisonOp::Lt => HirBinOp::Lt,
            ComparisonOp::Le => HirBinOp::Le,
            ComparisonOp::Gt => HirBinOp::Gt,
            ComparisonOp::Ge => HirBinOp::Ge,
        }
    }

    /// Convert from AST logical op.
    pub fn from_logical(op: LogicalOp) -> Option<Self> {
        match op {
            LogicalOp::And => Some(HirBinOp::And),
            LogicalOp::Or => Some(HirBinOp::Or),
            LogicalOp::Not => None, // Not is unary
        }
    }

    /// Check if this is a comparison operator.
    pub fn is_comparison(&self) -> bool {
        matches!(
            self,
            HirBinOp::Eq
                | HirBinOp::Ne
                | HirBinOp::Lt
                | HirBinOp::Le
                | HirBinOp::Gt
                | HirBinOp::Ge
        )
    }
}

/// Unary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HirUnaryOp {
    /// Logical not.
    Not,
    /// Arithmetic negation.
    Neg,
    /// Bitwise not.
    BitNot,
}

/// Assembly instruction in HIR.
#[derive(Debug, Clone)]
pub enum HirAsm {
    /// Data transfer.
    Mov {
        dest: HirAsmOperand,
        src: HirAsmOperand,
        span: Span,
    },
    /// Load effective address.
    Lea {
        dest: HirAsmOperand,
        src: HirAsmOperand,
        span: Span,
    },
    /// Arithmetic.
    Arith {
        op: ArithmeticOp,
        dest: HirAsmOperand,
        src: HirAsmOperand,
        span: Span,
    },
    /// Compare.
    Cmp {
        left: HirAsmOperand,
        right: HirAsmOperand,
        span: Span,
    },
    /// Jump.
    Jump {
        op: JumpOp,
        target: String,
        span: Span,
    },
    /// Push.
    Push {
        operand: HirAsmOperand,
        span: Span,
    },
    /// Pop.
    Pop {
        operand: HirAsmOperand,
        span: Span,
    },
    /// Call.
    Call {
        target: String,
        span: Span,
    },
    /// Return.
    Ret { span: Span },
    /// Data directive.
    Data {
        size: DataSize,
        label: String,
        values: Vec<HirConstValue>,
        span: Span,
    },
}

/// Assembly operand.
#[derive(Debug, Clone)]
pub enum HirAsmOperand {
    /// Register.
    Reg(String),
    /// Immediate value.
    Imm(i64),
    /// Memory reference.
    Mem {
        base: Option<String>,
        index: Option<String>,
        scale: u8,
        disp: i64,
    },
    /// Label reference.
    Label(String),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hir_type_size() {
        assert_eq!(HirType::Unit.size(), 0);
        assert_eq!(HirType::Bool.size(), 1);
        assert_eq!(HirType::Int(IntType::I32).size(), 4);
        assert_eq!(HirType::Int(IntType::I64).size(), 8);
        assert_eq!(HirType::Float(FloatType::F64).size(), 8);
        assert_eq!(
            HirType::Array(Box::new(HirType::Int(IntType::I32)), 10).size(),
            40
        );
    }

    #[test]
    fn test_hir_type_from_primitive() {
        assert_eq!(
            HirType::from_primitive(PrimitiveType::I32),
            HirType::Int(IntType::I32)
        );
        assert_eq!(
            HirType::from_primitive(PrimitiveType::F64),
            HirType::Float(FloatType::F64)
        );
        assert_eq!(HirType::from_primitive(PrimitiveType::Bool), HirType::Bool);
    }

    #[test]
    fn test_hir_binop_conversion() {
        assert_eq!(
            HirBinOp::from_arith(BinaryArithOp::Add),
            HirBinOp::Add
        );
        assert_eq!(
            HirBinOp::from_cmp(ComparisonOp::Lt),
            HirBinOp::Lt
        );
        assert_eq!(
            HirBinOp::from_logical(LogicalOp::And),
            Some(HirBinOp::And)
        );
    }
}
