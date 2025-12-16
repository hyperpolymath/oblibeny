// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Abstract Syntax Tree definitions for Oblíbený.

use crate::span::Span;

/// Top-level forms in an Oblíbený program.
#[derive(Debug, Clone, PartialEq)]
pub enum TopLevelForm {
    /// Module definition
    Module(ModuleDef),
    /// Package declaration
    Package(PackageDecl),
    /// Function definition
    Function(FunctionDef),
    /// Macro definition
    Macro(MacroDef),
    /// Global data definition
    GlobalData(GlobalDataDef),
    /// Assembly directive
    AssemblyDirective(AssemblyDirective),
    /// Conditional assembly
    ConditionalAssembly(ConditionalAssembly),
    /// Deployment specification
    DeploymentSpec(DeploymentSpec),
    /// Compile-time block
    CompileTimeBlock(CompileTimeBlock),
}

/// Module definition.
#[derive(Debug, Clone, PartialEq)]
pub struct ModuleDef {
    /// Module name
    pub name: Identifier,
    /// Module body items
    pub items: Vec<ModuleItem>,
    /// Source span
    pub span: Span,
}

/// Items that can appear in a module body.
#[derive(Debug, Clone, PartialEq)]
pub enum ModuleItem {
    /// Import statement
    Import(ImportStmt),
    /// Export statement
    Export(ExportStmt),
    /// Top-level form
    TopLevel(Box<TopLevelForm>),
}

/// Import statement.
#[derive(Debug, Clone, PartialEq)]
pub struct ImportStmt {
    /// Module to import from
    pub module: Identifier,
    /// Specific items to import (None = all)
    pub items: Option<Vec<Identifier>>,
    /// Source span
    pub span: Span,
}

/// Export statement.
#[derive(Debug, Clone, PartialEq)]
pub struct ExportStmt {
    /// Items to export
    pub items: Vec<Identifier>,
    /// Source span
    pub span: Span,
}

/// Package declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct PackageDecl {
    /// Package name
    pub name: Identifier,
    /// Package version
    pub version: String,
    /// Dependencies
    pub dependencies: Vec<Dependency>,
    /// Deployment manifest
    pub manifest: Option<DeploymentManifest>,
    /// Source span
    pub span: Span,
}

/// Package dependency.
#[derive(Debug, Clone, PartialEq)]
pub struct Dependency {
    /// Dependency name
    pub name: Identifier,
    /// Version string
    pub version: String,
    /// Version constraint
    pub constraint: Option<VersionConstraint>,
}

/// Version constraint.
#[derive(Debug, Clone, PartialEq)]
pub struct VersionConstraint {
    /// Comparison operator
    pub op: ComparisonOp,
    /// Version to compare against
    pub version: String,
}

/// Function definition.
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDef {
    /// Function name
    pub name: Identifier,
    /// Parameters
    pub params: Vec<Parameter>,
    /// Return type (optional)
    pub return_type: Option<TypeExpr>,
    /// Function annotations
    pub annotations: Vec<FunctionAnnotation>,
    /// Function body
    pub body: Vec<Statement>,
    /// Source span
    pub span: Span,
}

/// Function parameter.
#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    /// Parameter name
    pub name: Identifier,
    /// Parameter type (optional in compile-time, required in deploy-time)
    pub ty: Option<TypeExpr>,
    /// Source span
    pub span: Span,
}

/// Function annotations.
#[derive(Debug, Clone, PartialEq)]
pub enum FunctionAnnotation {
    /// Termination proof
    TerminationProof(TerminationStrategy),
    /// Complexity bound
    Complexity(ComplexityExpr),
    /// Pure function
    Pure,
    /// Idempotent function
    Idempotent,
    /// Deployment target
    DeployTarget(DeployTarget),
}

/// Termination proof strategy.
#[derive(Debug, Clone, PartialEq)]
pub enum TerminationStrategy {
    /// Bounded loop strategy
    BoundedLoop,
    /// Structural recursion (compile-time only)
    StructuralRecursion,
    /// Well-founded order
    WellFoundedOrder,
}

/// Complexity expression.
#[derive(Debug, Clone, PartialEq)]
pub enum ComplexityExpr {
    /// O(1) constant
    Constant,
    /// O(n) linear
    Linear,
    /// O(n^k) polynomial
    Polynomial(u32),
    /// O(log n) logarithmic
    Logarithmic,
    /// O(n log n) linearithmic
    Linearithmic,
}

/// Deployment target.
#[derive(Debug, Clone, PartialEq)]
pub enum DeployTarget {
    /// Runtime only
    RuntimeOnly,
    /// Compile only
    CompileOnly,
    /// Both
    Both,
}

/// Macro definition.
#[derive(Debug, Clone, PartialEq)]
pub struct MacroDef {
    /// Macro name
    pub name: Identifier,
    /// Parameters
    pub params: Vec<Parameter>,
    /// Macro body
    pub body: Vec<Statement>,
    /// Source span
    pub span: Span,
}

/// Global data definition.
#[derive(Debug, Clone, PartialEq)]
pub struct GlobalDataDef {
    /// Variable name
    pub name: Identifier,
    /// Type
    pub ty: TypeExpr,
    /// Initial value (optional)
    pub init: Option<Expression>,
    /// Visible during deployment
    pub deploy_visible: bool,
    /// Source span
    pub span: Span,
}

/// Assembly directive.
#[derive(Debug, Clone, PartialEq)]
pub struct AssemblyDirective {
    /// Directive type
    pub directive: DirectiveType,
    /// Argument
    pub arg: Identifier,
    /// Source span
    pub span: Span,
}

/// Directive types.
#[derive(Debug, Clone, PartialEq)]
pub enum DirectiveType {
    /// Target architecture
    Target,
    /// Section
    Section,
    /// Global symbol
    Global,
    /// External symbol
    Extern,
}

/// Conditional assembly.
#[derive(Debug, Clone, PartialEq)]
pub struct ConditionalAssembly {
    /// Condition identifier
    pub condition: Identifier,
    /// Then branch
    pub then_branch: Vec<Statement>,
    /// Else branch (optional)
    pub else_branch: Option<Vec<Statement>>,
    /// Source span
    pub span: Span,
}

/// Deployment specification.
#[derive(Debug, Clone, PartialEq)]
pub struct DeploymentSpec {
    /// Target profile
    pub profile: TargetProfile,
    /// Resource bounds
    pub bounds: ResourceBounds,
    /// Obfuscation level
    pub obfuscation: ObfuscationLevel,
    /// Security constraints
    pub constraints: Option<SecurityConstraints>,
    /// Source span
    pub span: Span,
}

/// Target profiles.
#[derive(Debug, Clone, PartialEq)]
pub enum TargetProfile {
    /// Minimal edge profile
    EdgeMinimal,
    /// Secure IoT profile
    IotSecure,
    /// Hardened embedded profile
    EmbeddedHardened,
    /// Sensor node profile
    SensorNode,
    /// Minimal actuator profile
    ActuatorMinimal,
}

/// Resource bounds.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct ResourceBounds {
    /// Maximum iterations
    pub max_iterations: Option<u64>,
    /// Maximum stack depth
    pub max_stack_depth: Option<u64>,
    /// Maximum memory
    pub max_memory: Option<u64>,
    /// Maximum call depth
    pub max_call_depth: Option<u64>,
    /// Maximum execution time
    pub max_execution_time: Option<u64>,
}

/// Obfuscation levels.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ObfuscationLevel {
    /// No obfuscation
    #[default]
    None,
    /// Basic name mangling
    Minimal,
    /// Semantic metamorphism
    Aggressive,
    /// Full code morphing
    Paranoid,
}

/// Security constraints.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct SecurityConstraints {
    /// Forbidden features
    pub forbidden: Vec<ForbiddenFeature>,
    /// Required proofs
    pub required: Vec<RequiredProof>,
}

/// Forbidden features.
#[derive(Debug, Clone, PartialEq)]
pub enum ForbiddenFeature {
    /// No syscalls
    Syscalls,
    /// No recursion
    Recursion,
    /// No backward jumps
    BackwardJumps,
    /// No dynamic allocation
    DynamicAllocation,
    /// No function pointers
    FunctionPointers,
}

/// Required proofs.
#[derive(Debug, Clone, PartialEq)]
pub enum RequiredProof {
    /// Termination proof required
    Termination,
    /// Bounded memory proof required
    BoundedMemory,
    /// No side effects proof required
    NoSideEffects,
}

/// Compile-time block.
#[derive(Debug, Clone, PartialEq)]
pub struct CompileTimeBlock {
    /// Block body
    pub body: Vec<Statement>,
    /// Source span
    pub span: Span,
}

/// Deploy-time block.
#[derive(Debug, Clone, PartialEq)]
pub struct DeployTimeBlock {
    /// Block body (restricted statements only)
    pub body: Vec<Statement>,
    /// Source span
    pub span: Span,
}

/// Deployment manifest.
#[derive(Debug, Clone, PartialEq)]
pub struct DeploymentManifest {
    /// Granted capabilities
    pub capabilities: Vec<CapabilityGrant>,
}

/// Capability grant.
#[derive(Debug, Clone, PartialEq)]
pub struct CapabilityGrant {
    /// Capability token
    pub token: Identifier,
    /// Budget (optional)
    pub budget: Option<u64>,
}

/// Statements.
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    /// Expression statement
    Expression(Expression),
    /// Local data definition
    LocalData(LocalDataDef),
    /// Assembly label
    Label(Identifier),
    /// Data directive
    DataDirective(DataDirective),
    /// Instruction
    Instruction(Instruction),
    /// Transaction (compile-time only)
    Transaction(Transaction),
    /// Metamorphic if (compile-time only)
    MetamorphicIf(MetamorphicIf),
    /// Function call
    Call(FunctionCall),
    /// Try-catch (compile-time only)
    TryCatch(TryCatch),
    /// Loop statement
    Loop(LoopStmt),
    /// Conditional assembly
    ConditionalAssembly(ConditionalAssembly),
    /// Deploy-time block
    DeployTimeBlock(DeployTimeBlock),
}

/// Local data definition.
#[derive(Debug, Clone, PartialEq)]
pub struct LocalDataDef {
    /// Variable name
    pub name: Identifier,
    /// Type (optional in compile-time, required in deploy-time)
    pub ty: Option<TypeExpr>,
    /// Initial value (optional)
    pub init: Option<Expression>,
    /// Source span
    pub span: Span,
}

/// Data directive.
#[derive(Debug, Clone, PartialEq)]
pub struct DataDirective {
    /// Size (db, dw, dd, dq)
    pub size: DataSize,
    /// Label name
    pub name: Identifier,
    /// Values
    pub values: Vec<DataValue>,
    /// Source span
    pub span: Span,
}

/// Data sizes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DataSize {
    /// Byte (8-bit)
    Byte,
    /// Word (16-bit)
    Word,
    /// Double word (32-bit)
    DWord,
    /// Quad word (64-bit)
    QWord,
}

/// Data values.
#[derive(Debug, Clone, PartialEq)]
pub enum DataValue {
    /// String value
    String(String),
    /// Integer value
    Integer(i64),
    /// Identifier reference
    Identifier(Identifier),
}

/// Instructions.
#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    /// Data transfer (mov, lea)
    DataTransfer(DataTransferOp),
    /// Arithmetic instruction
    Arithmetic(ArithmeticInstr),
    /// Control flow
    ControlFlow(ControlFlowOp),
    /// System operation (compile-time only)
    System(SystemOp),
    /// Stack operation
    Stack(StackOp),
    /// Memory operation
    Memory(MemoryOp),
    /// Checkpoint operation
    Checkpoint(CheckpointOp),
}

/// Data transfer operations.
#[derive(Debug, Clone, PartialEq)]
pub struct DataTransferOp {
    /// Operation (mov or lea)
    pub op: DataTransferType,
    /// Destination register
    pub dest: Register,
    /// Source operand
    pub src: Operand,
    /// Source span
    pub span: Span,
}

/// Data transfer types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DataTransferType {
    /// Move
    Mov,
    /// Load effective address
    Lea,
}

/// Arithmetic instruction.
#[derive(Debug, Clone, PartialEq)]
pub struct ArithmeticInstr {
    /// Operation
    pub op: ArithmeticOp,
    /// Destination register
    pub dest: Register,
    /// Source operand
    pub src: Operand,
    /// Source span
    pub span: Span,
}

/// Arithmetic operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArithmeticOp {
    /// Add
    Add,
    /// Subtract
    Sub,
    /// Multiply
    Mul,
    /// Divide
    Div,
    /// Increment
    Inc,
    /// Decrement
    Dec,
}

/// Control flow operations.
#[derive(Debug, Clone, PartialEq)]
pub enum ControlFlowOp {
    /// Compare
    Cmp { reg: Register, operand: Operand, span: Span },
    /// Jump
    Jump { op: JumpOp, target: Identifier, span: Span },
}

/// Jump operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JumpOp {
    /// Unconditional jump
    Jmp,
    /// Jump if equal
    Je,
    /// Jump if not equal
    Jne,
    /// Jump if greater
    Jg,
    /// Jump if less
    Jl,
    /// Jump if greater or equal
    Jge,
    /// Jump if less or equal
    Jle,
}

/// System operations (compile-time only).
#[derive(Debug, Clone, PartialEq)]
pub struct SystemOp {
    /// Operand (optional)
    pub operand: Option<Operand>,
    /// Source span
    pub span: Span,
}

/// Stack operations.
#[derive(Debug, Clone, PartialEq)]
pub struct StackOp {
    /// Operation (push or pop)
    pub op: StackOpType,
    /// Operand
    pub operand: Operand,
    /// Source span
    pub span: Span,
}

/// Stack operation types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StackOpType {
    /// Push onto stack
    Push,
    /// Pop from stack
    Pop,
}

/// Memory operations.
#[derive(Debug, Clone, PartialEq)]
pub struct MemoryOp {
    /// Operation (alloc or free)
    pub op: MemoryOpType,
    /// Register
    pub reg: Register,
    /// Size (for alloc)
    pub size: Option<u64>,
    /// Source span
    pub span: Span,
}

/// Memory operation types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemoryOpType {
    /// Allocate memory
    Alloc,
    /// Free memory
    Free,
}

/// Checkpoint operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CheckpointOp {
    /// Commit transaction
    Commit,
    /// Rollback transaction
    Rollback,
    /// Fail transaction
    Fail,
}

/// Transaction block (compile-time only).
#[derive(Debug, Clone, PartialEq)]
pub struct Transaction {
    /// Transaction body
    pub body: Vec<Statement>,
    /// Checkpoint operation (optional)
    pub checkpoint: Option<CheckpointOp>,
    /// Source span
    pub span: Span,
}

/// Metamorphic if (compile-time only).
#[derive(Debug, Clone, PartialEq)]
pub struct MetamorphicIf {
    /// Condition
    pub condition: Expression,
    /// Rewrite action
    pub rewrite: RewriteAction,
    /// Then branch
    pub then_branch: Vec<Statement>,
    /// Else branch (optional)
    pub else_branch: Option<Vec<Statement>>,
    /// Source span
    pub span: Span,
}

/// Rewrite action.
#[derive(Debug, Clone, PartialEq)]
pub struct RewriteAction {
    /// Target identifier or string
    pub target: RewriteTarget,
    /// Source span
    pub span: Span,
}

/// Rewrite target.
#[derive(Debug, Clone, PartialEq)]
pub enum RewriteTarget {
    /// Identifier target
    Identifier(Identifier),
    /// String target
    String(String),
}

/// Function call.
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
    /// Function name
    pub name: Identifier,
    /// Arguments
    pub args: Vec<Expression>,
    /// Source span
    pub span: Span,
}

/// Try-catch block (compile-time only).
#[derive(Debug, Clone, PartialEq)]
pub struct TryCatch {
    /// Try body
    pub try_body: Vec<Statement>,
    /// Catch clauses
    pub catches: Vec<CatchClause>,
    /// Finally clause (optional)
    pub finally: Option<Vec<Statement>>,
    /// Source span
    pub span: Span,
}

/// Catch clause.
#[derive(Debug, Clone, PartialEq)]
pub struct CatchClause {
    /// Exception pattern (optional)
    pub pattern: Option<ExceptionPattern>,
    /// Binding name
    pub binding: Identifier,
    /// Catch body
    pub body: Vec<Statement>,
    /// Source span
    pub span: Span,
}

/// Exception pattern.
#[derive(Debug, Clone, PartialEq)]
pub enum ExceptionPattern {
    /// Identifier pattern
    Identifier(Identifier),
    /// String pattern
    String(String),
}

/// Loop statements.
#[derive(Debug, Clone, PartialEq)]
pub enum LoopStmt {
    /// While loop (compile-time only)
    While(WhileLoop),
    /// For loop (compile-time only)
    For(ForLoop),
    /// Bounded for loop (deploy-time safe)
    BoundedFor(BoundedForLoop),
}

/// While loop (compile-time only).
#[derive(Debug, Clone, PartialEq)]
pub struct WhileLoop {
    /// Condition
    pub condition: Expression,
    /// Loop body
    pub body: Vec<Statement>,
    /// Source span
    pub span: Span,
}

/// For loop (compile-time only).
#[derive(Debug, Clone, PartialEq)]
pub struct ForLoop {
    /// Loop variable
    pub var: Identifier,
    /// Iterable expression
    pub iter: Expression,
    /// Loop body
    pub body: Vec<Statement>,
    /// Source span
    pub span: Span,
}

/// Bounded for loop (deploy-time safe).
#[derive(Debug, Clone, PartialEq)]
pub struct BoundedForLoop {
    /// Loop variable
    pub var: Identifier,
    /// Start expression (compile-time constant)
    pub start: Expression,
    /// End expression (compile-time constant)
    pub end: Expression,
    /// Loop body
    pub body: Vec<Statement>,
    /// Source span
    pub span: Span,
}

/// Expressions.
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    /// Literal value
    Literal(Literal),
    /// Variable reference
    Variable(Identifier),
    /// Function call expression
    Call(FunctionCall),
    /// Arithmetic expression
    Arithmetic(ArithmeticExpr),
    /// Comparison expression
    Comparison(ComparisonExpr),
    /// Logical expression
    Logical(LogicalExpr),
    /// Memory expression
    Memory(MemoryExpr),
    /// Capability invocation
    Capability(CapabilityInvoke),
}

/// Literal values.
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    /// Integer literal
    Integer(i64),
    /// Float literal
    Float(f64),
    /// String literal
    String(String),
    /// Boolean literal
    Bool(bool),
}

/// Arithmetic expression.
#[derive(Debug, Clone, PartialEq)]
pub struct ArithmeticExpr {
    /// Operator
    pub op: BinaryArithOp,
    /// Left operand
    pub left: Box<Expression>,
    /// Right operand
    pub right: Box<Expression>,
    /// Source span
    pub span: Span,
}

/// Binary arithmetic operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryArithOp {
    /// Addition
    Add,
    /// Subtraction
    Sub,
    /// Multiplication
    Mul,
    /// Division
    Div,
    /// Modulo
    Mod,
}

/// Comparison expression.
#[derive(Debug, Clone, PartialEq)]
pub struct ComparisonExpr {
    /// Operator
    pub op: ComparisonOp,
    /// Left operand
    pub left: Box<Expression>,
    /// Right operand
    pub right: Box<Expression>,
    /// Source span
    pub span: Span,
}

/// Comparison operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ComparisonOp {
    /// Equal
    Eq,
    /// Not equal
    Neq,
    /// Less than
    Lt,
    /// Greater than
    Gt,
    /// Less than or equal
    Le,
    /// Greater than or equal
    Ge,
}

/// Logical expression.
#[derive(Debug, Clone, PartialEq)]
pub struct LogicalExpr {
    /// Operator
    pub op: LogicalOp,
    /// Operands
    pub operands: Vec<Expression>,
    /// Source span
    pub span: Span,
}

/// Logical operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogicalOp {
    /// Logical AND
    And,
    /// Logical OR
    Or,
    /// Logical NOT
    Not,
}

/// Memory expression.
#[derive(Debug, Clone, PartialEq)]
pub struct MemoryExpr {
    /// Base address expression
    pub base: Box<Expression>,
    /// Offset expression (optional)
    pub offset: Option<Box<Expression>>,
    /// Source span
    pub span: Span,
}

/// Capability invocation.
#[derive(Debug, Clone, PartialEq)]
pub struct CapabilityInvoke {
    /// Capability token
    pub token: Identifier,
    /// Arguments
    pub args: Vec<Expression>,
    /// Source span
    pub span: Span,
}

/// Operand (for assembly instructions).
#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    /// Register operand
    Register(Register),
    /// Expression operand
    Expression(Expression),
    /// Memory operand
    Memory(MemoryOperand),
}

/// Memory operand (for assembly instructions).
#[derive(Debug, Clone, PartialEq)]
pub struct MemoryOperand {
    /// Base register
    pub base: Register,
    /// Index register (optional)
    pub index: Option<Register>,
    /// Scale factor (optional)
    pub scale: Option<u8>,
    /// Displacement (optional)
    pub displacement: Option<i64>,
    /// Source span
    pub span: Span,
}

/// Register.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Register {
    /// Register name
    pub name: String,
    /// Source span
    pub span: Span,
}

/// Type expressions.
#[derive(Debug, Clone, PartialEq)]
pub enum TypeExpr {
    /// Primitive type
    Primitive(PrimitiveType),
    /// Array type
    Array { elem: Box<TypeExpr>, size: u64 },
    /// Pointer type
    Pointer(Box<TypeExpr>),
    /// Struct type
    Struct(Vec<FieldDef>),
    /// Enum type
    Enum(Vec<Identifier>),
    /// User-defined type
    User(Identifier),
}

/// Primitive types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveType {
    /// Unsigned 8-bit
    U8,
    /// Unsigned 16-bit
    U16,
    /// Unsigned 32-bit
    U32,
    /// Unsigned 64-bit
    U64,
    /// Signed 8-bit
    I8,
    /// Signed 16-bit
    I16,
    /// Signed 32-bit
    I32,
    /// Signed 64-bit
    I64,
    /// 32-bit float
    F32,
    /// 64-bit float
    F64,
    /// Boolean
    Bool,
    /// String
    String,
}

/// Field definition (for structs).
#[derive(Debug, Clone, PartialEq)]
pub struct FieldDef {
    /// Field name
    pub name: Identifier,
    /// Field type
    pub ty: TypeExpr,
}

/// Identifier with span.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier {
    /// Identifier name
    pub name: String,
    /// Source span
    pub span: Span,
}

impl Identifier {
    /// Create a new identifier.
    pub fn new(name: impl Into<String>, span: Span) -> Self {
        Self {
            name: name.into(),
            span,
        }
    }
}
