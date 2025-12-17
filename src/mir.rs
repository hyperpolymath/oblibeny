// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Mid-Level Intermediate Representation (MIR) for the Oblíbený compiler.
//!
//! MIR is a control-flow-graph-based representation using SSA (Static Single Assignment)
//! form. It bridges between HIR and the final code generation, providing a suitable
//! representation for optimization passes.
//!
//! # Key Features
//!
//! - **Basic Blocks**: Linear sequences of instructions with single entry/exit
//! - **SSA Form**: Each variable assigned exactly once, with phi nodes at joins
//! - **Explicit Control Flow**: Jumps and branches represented as terminators
//! - **Typed Values**: All values carry type information
//!
//! # Structure
//!
//! ```text
//! MirProgram
//! └── MirFunction[]
//!     ├── BasicBlock[]
//!     │   ├── MirInstr[]
//!     │   └── Terminator
//!     └── LocalDecl[]
//! ```

use crate::hir::{HirBinOp, HirType, HirUnaryOp};
use crate::span::Span;
use crate::symbol::SymbolId;
use std::collections::HashMap;

/// A MIR program containing all functions
#[derive(Debug, Clone)]
pub struct MirProgram {
    /// All functions in the program
    pub functions: Vec<MirFunction>,
    /// Global constants
    pub constants: Vec<MirConstant>,
    /// Static data
    pub statics: Vec<MirStatic>,
}

impl MirProgram {
    /// Create an empty MIR program
    pub fn new() -> Self {
        MirProgram {
            functions: Vec::new(),
            constants: Vec::new(),
            statics: Vec::new(),
        }
    }
}

impl Default for MirProgram {
    fn default() -> Self {
        Self::new()
    }
}

/// A global constant value
#[derive(Debug, Clone)]
pub struct MirConstant {
    /// Unique identifier
    pub id: ConstantId,
    /// Type of the constant
    pub ty: MirType,
    /// The constant value
    pub value: ConstValue,
    /// Source span
    pub span: Span,
}

/// A static variable
#[derive(Debug, Clone)]
pub struct MirStatic {
    /// Unique identifier
    pub id: StaticId,
    /// Name of the static
    pub name: String,
    /// Type of the static
    pub ty: MirType,
    /// Optional initializer
    pub init: Option<ConstValue>,
    /// Whether mutable
    pub mutable: bool,
    /// Source span
    pub span: Span,
}

/// Unique identifier for constants
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ConstantId(pub u32);

/// Unique identifier for statics
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StaticId(pub u32);

/// A function in MIR form
#[derive(Debug, Clone)]
pub struct MirFunction {
    /// Symbol ID from name resolution
    pub id: SymbolId,
    /// Function name
    pub name: String,
    /// Parameter types
    pub params: Vec<MirType>,
    /// Return type
    pub ret_ty: MirType,
    /// Basic blocks forming the CFG
    pub blocks: Vec<BasicBlock>,
    /// Local variable declarations
    pub locals: Vec<LocalDecl>,
    /// Entry block ID
    pub entry: BlockId,
    /// Whether this is a deploy-time function
    pub is_deploy: bool,
    /// Source span
    pub span: Span,
}

impl MirFunction {
    /// Create a new MIR function
    pub fn new(
        id: SymbolId,
        name: String,
        params: Vec<MirType>,
        ret_ty: MirType,
        is_deploy: bool,
        span: Span,
    ) -> Self {
        MirFunction {
            id,
            name,
            params,
            ret_ty,
            blocks: Vec::new(),
            locals: Vec::new(),
            entry: BlockId(0),
            is_deploy,
            span,
        }
    }

    /// Add a basic block and return its ID
    pub fn add_block(&mut self) -> BlockId {
        let id = BlockId(self.blocks.len() as u32);
        self.blocks.push(BasicBlock::new(id));
        id
    }

    /// Get a block by ID
    pub fn block(&self, id: BlockId) -> Option<&BasicBlock> {
        self.blocks.get(id.0 as usize)
    }

    /// Get a mutable block by ID
    pub fn block_mut(&mut self, id: BlockId) -> Option<&mut BasicBlock> {
        self.blocks.get_mut(id.0 as usize)
    }

    /// Add a local variable and return its ID
    pub fn add_local(&mut self, name: String, ty: MirType, span: Span) -> Local {
        let local = Local(self.locals.len() as u32);
        self.locals.push(LocalDecl { name, ty, span });
        local
    }

    /// Get successor blocks for a given block
    pub fn successors(&self, block: BlockId) -> Vec<BlockId> {
        self.block(block)
            .map(|b| b.terminator.successors())
            .unwrap_or_default()
    }

    /// Get predecessor blocks for a given block
    pub fn predecessors(&self, block: BlockId) -> Vec<BlockId> {
        self.blocks
            .iter()
            .filter(|b| b.terminator.successors().contains(&block))
            .map(|b| b.id)
            .collect()
    }
}

/// Unique identifier for a basic block
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub u32);

/// A basic block in the control flow graph
#[derive(Debug, Clone)]
pub struct BasicBlock {
    /// Block identifier
    pub id: BlockId,
    /// Phi nodes at the start of the block
    pub phis: Vec<PhiNode>,
    /// Instructions in the block
    pub instructions: Vec<MirInstr>,
    /// Terminator instruction
    pub terminator: Terminator,
}

impl BasicBlock {
    /// Create a new basic block
    pub fn new(id: BlockId) -> Self {
        BasicBlock {
            id,
            phis: Vec::new(),
            instructions: Vec::new(),
            terminator: Terminator::Unreachable,
        }
    }

    /// Add a phi node
    pub fn add_phi(&mut self, dest: Local, ty: MirType) -> &mut PhiNode {
        self.phis.push(PhiNode {
            dest,
            ty,
            incoming: Vec::new(),
        });
        self.phis.last_mut().unwrap()
    }

    /// Add an instruction
    pub fn add_instruction(&mut self, instr: MirInstr) {
        self.instructions.push(instr);
    }

    /// Set the terminator
    pub fn set_terminator(&mut self, term: Terminator) {
        self.terminator = term;
    }
}

/// A phi node for SSA form
#[derive(Debug, Clone)]
pub struct PhiNode {
    /// Destination local
    pub dest: Local,
    /// Type of the phi
    pub ty: MirType,
    /// Incoming values from predecessor blocks
    pub incoming: Vec<(BlockId, Operand)>,
}

/// A local variable reference
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Local(pub u32);

/// Declaration for a local variable
#[derive(Debug, Clone)]
pub struct LocalDecl {
    /// Variable name (for debugging)
    pub name: String,
    /// Type of the local
    pub ty: MirType,
    /// Source span
    pub span: Span,
}

/// MIR types (simplified from HIR)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MirType {
    /// Unit/void type
    Unit,
    /// Boolean type
    Bool,
    /// Signed integer (bits: 8, 16, 32, 64)
    Int(u8),
    /// Unsigned integer (bits: 8, 16, 32, 64)
    UInt(u8),
    /// Floating point (bits: 32, 64)
    Float(u8),
    /// Fixed-size array
    Array(Box<MirType>, u64),
    /// Pointer to a type
    Ptr(Box<MirType>),
    /// Aggregate type (struct)
    Aggregate(SymbolId),
    /// Function pointer
    FnPtr {
        /// Parameter types
        params: Vec<MirType>,
        /// Return type
        ret: Box<MirType>,
    },
    /// Error placeholder
    Error,
}

impl MirType {
    /// Get the size in bits for primitive types
    pub fn size_bits(&self) -> Option<u32> {
        match self {
            MirType::Unit => Some(0),
            MirType::Bool => Some(1),
            MirType::Int(bits) | MirType::UInt(bits) | MirType::Float(bits) => Some(*bits as u32),
            MirType::Ptr(_) => Some(64), // Assume 64-bit pointers
            _ => None,
        }
    }

    /// Check if this is a numeric type
    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            MirType::Int(_) | MirType::UInt(_) | MirType::Float(_)
        )
    }

    /// Check if this is an integer type
    pub fn is_integer(&self) -> bool {
        matches!(self, MirType::Int(_) | MirType::UInt(_))
    }

    /// Check if this is a signed integer
    pub fn is_signed(&self) -> bool {
        matches!(self, MirType::Int(_))
    }

    /// Convert from HIR type
    pub fn from_hir(hir_ty: &HirType) -> Self {
        match hir_ty {
            HirType::Unit => MirType::Unit,
            HirType::Bool => MirType::Bool,
            HirType::Int(int_ty) => {
                use crate::hir::IntType;
                match int_ty {
                    IntType::I8 => MirType::Int(8),
                    IntType::I16 => MirType::Int(16),
                    IntType::I32 => MirType::Int(32),
                    IntType::I64 => MirType::Int(64),
                    IntType::U8 => MirType::UInt(8),
                    IntType::U16 => MirType::UInt(16),
                    IntType::U32 => MirType::UInt(32),
                    IntType::U64 => MirType::UInt(64),
                }
            }
            HirType::Float(float_ty) => {
                use crate::hir::FloatType;
                match float_ty {
                    FloatType::F32 => MirType::Float(32),
                    FloatType::F64 => MirType::Float(64),
                }
            }
            HirType::Array(elem, size) => {
                MirType::Array(Box::new(MirType::from_hir(elem)), *size)
            }
            HirType::Ptr(inner) => MirType::Ptr(Box::new(MirType::from_hir(inner))),
            HirType::Struct(id) | HirType::Enum(id) => MirType::Aggregate(*id),
            HirType::Func { params, ret } => MirType::FnPtr {
                params: params.iter().map(MirType::from_hir).collect(),
                ret: Box::new(MirType::from_hir(ret)),
            },
            HirType::Error => MirType::Error,
        }
    }
}

/// An operand in MIR instructions
#[derive(Debug, Clone)]
pub enum Operand {
    /// Reference to a local variable
    Local(Local),
    /// A constant value
    Const(ConstValue),
    /// Reference to a global constant
    ConstRef(ConstantId),
    /// Reference to a static variable
    StaticRef(StaticId),
}

impl Operand {
    /// Create a local operand
    pub fn local(l: Local) -> Self {
        Operand::Local(l)
    }

    /// Create a constant integer operand
    pub fn const_int(value: i64) -> Self {
        Operand::Const(ConstValue::Int(value))
    }

    /// Create a constant unsigned integer operand
    pub fn const_uint(value: u64) -> Self {
        Operand::Const(ConstValue::UInt(value))
    }

    /// Create a constant boolean operand
    pub fn const_bool(value: bool) -> Self {
        Operand::Const(ConstValue::Bool(value))
    }
}

/// A constant value
#[derive(Debug, Clone, PartialEq)]
pub enum ConstValue {
    /// Boolean constant
    Bool(bool),
    /// Signed integer constant
    Int(i64),
    /// Unsigned integer constant
    UInt(u64),
    /// Float constant
    Float(f64),
    /// String constant (index into string table)
    Str(u32),
    /// Array of constants
    Array(Vec<ConstValue>),
    /// Struct/aggregate constant
    Aggregate(Vec<ConstValue>),
    /// Zero-initialized value
    ZeroInit,
    /// Undefined value (for uninitialized memory)
    Undef,
}

/// A MIR instruction
#[derive(Debug, Clone)]
pub enum MirInstr {
    /// No operation
    Nop,

    /// Assign a value: dest = src
    Assign {
        /// Destination local
        dest: Local,
        /// Source value
        value: RValue,
        /// Source span
        span: Span,
    },

    /// Store to memory: *ptr = value
    Store {
        /// Pointer to store to
        ptr: Operand,
        /// Value to store
        value: Operand,
        /// Source span
        span: Span,
    },

    /// Call a function
    Call {
        /// Destination for return value (None for void)
        dest: Option<Local>,
        /// Function to call
        func: CallTarget,
        /// Arguments
        args: Vec<Operand>,
        /// Source span
        span: Span,
    },

    /// Invoke a capability
    InvokeCapability {
        /// Destination for return value
        dest: Option<Local>,
        /// Operation name
        op: String,
        /// Capability token
        token: Operand,
        /// Additional arguments
        args: Vec<Operand>,
        /// Source span
        span: Span,
    },

    /// Inline assembly
    InlineAsm {
        /// Assembly string
        asm: String,
        /// Output operands
        outputs: Vec<(String, Local)>,
        /// Input operands
        inputs: Vec<(String, Operand)>,
        /// Clobbered registers
        clobbers: Vec<String>,
        /// Source span
        span: Span,
    },
}

/// Target of a function call
#[derive(Debug, Clone)]
pub enum CallTarget {
    /// Direct call to a known function
    Direct(SymbolId),
    /// Indirect call through a function pointer
    Indirect(Operand),
}

/// An r-value (right-hand side of assignment)
#[derive(Debug, Clone)]
pub enum RValue {
    /// Use an operand directly
    Use(Operand),

    /// Load from memory
    Load {
        /// Pointer to load from
        ptr: Operand,
        /// Type being loaded
        ty: MirType,
    },

    /// Get address of a place
    AddressOf {
        /// The place to get address of
        place: Place,
    },

    /// Binary operation
    BinOp {
        /// The operation
        op: BinOp,
        /// Left operand
        left: Operand,
        /// Right operand
        right: Operand,
    },

    /// Unary operation
    UnaryOp {
        /// The operation
        op: UnaryOp,
        /// Operand
        operand: Operand,
    },

    /// Type cast
    Cast {
        /// Source operand
        operand: Operand,
        /// Source type
        from: MirType,
        /// Destination type
        to: MirType,
    },

    /// Create an aggregate value
    Aggregate {
        /// Type of the aggregate
        ty: MirType,
        /// Field values
        fields: Vec<Operand>,
    },

    /// Extract a field from an aggregate
    ExtractField {
        /// Aggregate operand
        aggregate: Operand,
        /// Field index
        field: u32,
    },

    /// Index into an array
    Index {
        /// Array operand
        array: Operand,
        /// Index operand
        index: Operand,
    },

    /// Get length of an array
    Len {
        /// Array operand
        array: Operand,
    },
}

/// A place (memory location)
#[derive(Debug, Clone)]
pub enum Place {
    /// A local variable
    Local(Local),
    /// A static variable
    Static(StaticId),
    /// Dereference a pointer
    Deref(Box<Operand>),
    /// Field projection
    Field(Box<Place>, u32),
    /// Array index projection
    Index(Box<Place>, Box<Operand>),
}

/// Binary operations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    // Arithmetic
    /// Addition
    Add,
    /// Subtraction
    Sub,
    /// Multiplication
    Mul,
    /// Division
    Div,
    /// Remainder
    Rem,

    // Bitwise
    /// Bitwise AND
    BitAnd,
    /// Bitwise OR
    BitOr,
    /// Bitwise XOR
    BitXor,
    /// Shift left
    Shl,
    /// Shift right (arithmetic for signed, logical for unsigned)
    Shr,

    // Comparison
    /// Equal
    Eq,
    /// Not equal
    Ne,
    /// Less than
    Lt,
    /// Less than or equal
    Le,
    /// Greater than
    Gt,
    /// Greater than or equal
    Ge,

    // Logical (for booleans)
    /// Logical AND
    And,
    /// Logical OR
    Or,
}

impl BinOp {
    /// Convert from HIR binary operation
    pub fn from_hir(op: HirBinOp) -> Self {
        match op {
            HirBinOp::Add => BinOp::Add,
            HirBinOp::Sub => BinOp::Sub,
            HirBinOp::Mul => BinOp::Mul,
            HirBinOp::Div => BinOp::Div,
            HirBinOp::Mod => BinOp::Rem,
            HirBinOp::BitAnd => BinOp::BitAnd,
            HirBinOp::BitOr => BinOp::BitOr,
            HirBinOp::BitXor => BinOp::BitXor,
            HirBinOp::Shl => BinOp::Shl,
            HirBinOp::Shr => BinOp::Shr,
            HirBinOp::Eq => BinOp::Eq,
            HirBinOp::Ne => BinOp::Ne,
            HirBinOp::Lt => BinOp::Lt,
            HirBinOp::Le => BinOp::Le,
            HirBinOp::Gt => BinOp::Gt,
            HirBinOp::Ge => BinOp::Ge,
            HirBinOp::And => BinOp::And,
            HirBinOp::Or => BinOp::Or,
        }
    }
}

/// Unary operations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    /// Negation (arithmetic)
    Neg,
    /// Bitwise NOT
    Not,
    /// Logical NOT
    LogicalNot,
}

impl UnaryOp {
    /// Convert from HIR unary operation
    pub fn from_hir(op: HirUnaryOp) -> Self {
        match op {
            HirUnaryOp::Neg => UnaryOp::Neg,
            HirUnaryOp::BitNot => UnaryOp::Not,
            HirUnaryOp::Not => UnaryOp::LogicalNot,
        }
    }
}

/// Block terminator instruction
#[derive(Debug, Clone)]
pub enum Terminator {
    /// Unconditional jump
    Goto(BlockId),

    /// Conditional branch
    Branch {
        /// Condition to test
        cond: Operand,
        /// Block if true
        then_block: BlockId,
        /// Block if false
        else_block: BlockId,
    },

    /// Switch on an integer value
    Switch {
        /// Value to switch on
        value: Operand,
        /// Cases: (value, target block)
        cases: Vec<(i64, BlockId)>,
        /// Default block
        default: BlockId,
    },

    /// Return from function
    Return(Option<Operand>),

    /// Unreachable code (should never execute)
    Unreachable,

    /// Abort execution (for panic/assert)
    Abort,
}

impl Terminator {
    /// Get successor blocks
    pub fn successors(&self) -> Vec<BlockId> {
        match self {
            Terminator::Goto(target) => vec![*target],
            Terminator::Branch {
                then_block,
                else_block,
                ..
            } => vec![*then_block, *else_block],
            Terminator::Switch { cases, default, .. } => {
                let mut succs: Vec<_> = cases.iter().map(|(_, b)| *b).collect();
                succs.push(*default);
                succs
            }
            Terminator::Return(_) | Terminator::Unreachable | Terminator::Abort => vec![],
        }
    }

    /// Get mutable references to successor blocks
    pub fn successors_mut(&mut self) -> Vec<&mut BlockId> {
        match self {
            Terminator::Goto(target) => vec![target],
            Terminator::Branch {
                then_block,
                else_block,
                ..
            } => vec![then_block, else_block],
            Terminator::Switch { cases, default, .. } => {
                let mut succs: Vec<_> = cases.iter_mut().map(|(_, b)| b).collect();
                succs.push(default);
                succs
            }
            Terminator::Return(_) | Terminator::Unreachable | Terminator::Abort => vec![],
        }
    }
}

/// Builder for constructing MIR functions
pub struct MirBuilder<'a> {
    func: &'a mut MirFunction,
    current_block: BlockId,
    local_map: HashMap<SymbolId, Local>,
    next_temp: u32,
}

impl<'a> MirBuilder<'a> {
    /// Create a new MIR builder for a function
    pub fn new(func: &'a mut MirFunction) -> Self {
        let entry = func.entry;
        MirBuilder {
            func,
            current_block: entry,
            local_map: HashMap::new(),
            next_temp: 0,
        }
    }

    /// Get the current block ID
    pub fn current_block(&self) -> BlockId {
        self.current_block
    }

    /// Switch to a different block
    pub fn switch_to_block(&mut self, block: BlockId) {
        self.current_block = block;
    }

    /// Create a new basic block
    pub fn create_block(&mut self) -> BlockId {
        self.func.add_block()
    }

    /// Create a new temporary local
    pub fn create_temp(&mut self, ty: MirType, span: Span) -> Local {
        let name = format!("_t{}", self.next_temp);
        self.next_temp += 1;
        self.func.add_local(name, ty, span)
    }

    /// Declare a named local variable
    pub fn declare_local(&mut self, id: SymbolId, name: String, ty: MirType, span: Span) -> Local {
        let local = self.func.add_local(name, ty, span);
        self.local_map.insert(id, local);
        local
    }

    /// Look up a local by symbol ID
    pub fn lookup_local(&self, id: SymbolId) -> Option<Local> {
        self.local_map.get(&id).copied()
    }

    /// Add an instruction to the current block
    pub fn add_instruction(&mut self, instr: MirInstr) {
        if let Some(block) = self.func.block_mut(self.current_block) {
            block.add_instruction(instr);
        }
    }

    /// Add an assignment instruction
    pub fn assign(&mut self, dest: Local, value: RValue, span: Span) {
        self.add_instruction(MirInstr::Assign { dest, value, span });
    }

    /// Add a store instruction
    pub fn store(&mut self, ptr: Operand, value: Operand, span: Span) {
        self.add_instruction(MirInstr::Store { ptr, value, span });
    }

    /// Add a call instruction
    pub fn call(
        &mut self,
        dest: Option<Local>,
        func: CallTarget,
        args: Vec<Operand>,
        span: Span,
    ) {
        self.add_instruction(MirInstr::Call {
            dest,
            func,
            args,
            span,
        });
    }

    /// Set the terminator for the current block
    pub fn terminate(&mut self, term: Terminator) {
        if let Some(block) = self.func.block_mut(self.current_block) {
            block.set_terminator(term);
        }
    }

    /// Emit a goto terminator and switch to the target block
    pub fn goto(&mut self, target: BlockId) {
        self.terminate(Terminator::Goto(target));
    }

    /// Emit a conditional branch
    pub fn branch(&mut self, cond: Operand, then_block: BlockId, else_block: BlockId) {
        self.terminate(Terminator::Branch {
            cond,
            then_block,
            else_block,
        });
    }

    /// Emit a return
    pub fn return_value(&mut self, value: Option<Operand>) {
        self.terminate(Terminator::Return(value));
    }

    /// Add a phi node to a block
    pub fn add_phi(&mut self, block: BlockId, dest: Local, ty: MirType) {
        if let Some(b) = self.func.block_mut(block) {
            b.add_phi(dest, ty);
        }
    }

    /// Add an incoming edge to a phi node
    pub fn add_phi_incoming(&mut self, block: BlockId, phi_idx: usize, from: BlockId, value: Operand) {
        if let Some(b) = self.func.block_mut(block) {
            if let Some(phi) = b.phis.get_mut(phi_idx) {
                phi.incoming.push((from, value));
            }
        }
    }
}

/// Dominance information for a function
#[derive(Debug)]
pub struct DominanceInfo {
    /// Immediate dominator for each block
    pub idom: HashMap<BlockId, BlockId>,
    /// Dominance frontier for each block
    pub frontier: HashMap<BlockId, Vec<BlockId>>,
}

impl DominanceInfo {
    /// Compute dominance information for a function
    pub fn compute(func: &MirFunction) -> Self {
        // Simple dominator computation using data flow
        let mut idom: HashMap<BlockId, BlockId> = HashMap::new();
        let entry = func.entry;

        // Initialize: entry dominates itself
        idom.insert(entry, entry);

        // Iterate until fixed point
        let mut changed = true;
        while changed {
            changed = false;
            for block in &func.blocks {
                if block.id == entry {
                    continue;
                }

                let preds = func.predecessors(block.id);
                if preds.is_empty() {
                    continue;
                }

                // Find common dominator of all predecessors
                let mut new_idom = preds[0];
                for &pred in &preds[1..] {
                    new_idom = Self::intersect(&idom, new_idom, pred, entry);
                }

                if idom.get(&block.id) != Some(&new_idom) {
                    idom.insert(block.id, new_idom);
                    changed = true;
                }
            }
        }

        // Compute dominance frontiers
        let mut frontier: HashMap<BlockId, Vec<BlockId>> = HashMap::new();
        for block in &func.blocks {
            frontier.insert(block.id, Vec::new());
        }

        for block in &func.blocks {
            let preds = func.predecessors(block.id);
            if preds.len() >= 2 {
                for &pred in &preds {
                    let mut runner = pred;
                    while runner != *idom.get(&block.id).unwrap_or(&entry) {
                        frontier.get_mut(&runner).unwrap().push(block.id);
                        runner = *idom.get(&runner).unwrap_or(&entry);
                        if runner == entry {
                            break;
                        }
                    }
                }
            }
        }

        DominanceInfo { idom, frontier }
    }

    fn intersect(idom: &HashMap<BlockId, BlockId>, mut b1: BlockId, mut b2: BlockId, entry: BlockId) -> BlockId {
        while b1 != b2 {
            while b1.0 > b2.0 {
                b1 = *idom.get(&b1).unwrap_or(&entry);
            }
            while b2.0 > b1.0 {
                b2 = *idom.get(&b2).unwrap_or(&entry);
            }
        }
        b1
    }

    /// Check if block a dominates block b
    pub fn dominates(&self, a: BlockId, b: BlockId) -> bool {
        let mut current = b;
        while let Some(&dom) = self.idom.get(&current) {
            if dom == a {
                return true;
            }
            if dom == current {
                // Reached entry
                break;
            }
            current = dom;
        }
        a == b
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mir_function_creation() {
        let mut func = MirFunction::new(
            SymbolId(0),
            "test".to_string(),
            vec![MirType::Int(32)],
            MirType::Int(32),
            true,
            Span::new(0, 0),
        );

        let entry = func.add_block();
        func.entry = entry;

        let local = func.add_local("x".to_string(), MirType::Int(32), Span::new(0, 0));

        assert_eq!(func.blocks.len(), 1);
        assert_eq!(func.locals.len(), 1);
        assert_eq!(local.0, 0);
    }

    #[test]
    fn test_mir_builder() {
        let mut func = MirFunction::new(
            SymbolId(0),
            "test".to_string(),
            vec![],
            MirType::Unit,
            true,
            Span::new(0, 0),
        );

        let entry = func.add_block();
        func.entry = entry;

        {
            let mut builder = MirBuilder::new(&mut func);

            let temp = builder.create_temp(MirType::Int(32), Span::new(0, 0));
            builder.assign(
                temp,
                RValue::Use(Operand::const_int(42)),
                Span::new(0, 0),
            );
            builder.return_value(Some(Operand::local(temp)));
        }

        assert_eq!(func.blocks[0].instructions.len(), 1);
        assert!(matches!(func.blocks[0].terminator, Terminator::Return(_)));
    }

    #[test]
    fn test_mir_type_conversion() {
        let hir_ty = HirType::Int(crate::hir::IntType::I32);
        let mir_ty = MirType::from_hir(&hir_ty);
        assert_eq!(mir_ty, MirType::Int(32));

        let hir_arr = HirType::Array(Box::new(HirType::Bool), 10);
        let mir_arr = MirType::from_hir(&hir_arr);
        assert_eq!(mir_arr, MirType::Array(Box::new(MirType::Bool), 10));
    }

    #[test]
    fn test_terminator_successors() {
        let goto = Terminator::Goto(BlockId(1));
        assert_eq!(goto.successors(), vec![BlockId(1)]);

        let branch = Terminator::Branch {
            cond: Operand::const_bool(true),
            then_block: BlockId(1),
            else_block: BlockId(2),
        };
        assert_eq!(branch.successors(), vec![BlockId(1), BlockId(2)]);

        let ret = Terminator::Return(None);
        assert!(ret.successors().is_empty());
    }
}
