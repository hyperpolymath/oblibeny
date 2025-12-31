// SPDX-License-Identifier: MIT OR Palimpsest-0.8
// Copyright (c) 2024 Hyperpolymath

//! OIR (Oblivious Intermediate Representation) types
//!
//! These types mirror the OCaml frontend's OIR definitions and are
//! deserialized from JSON.

use serde::{Deserialize, Serialize};

/// Security label for information flow
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Security {
    Low,
    High,
}

/// Primitive types
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum PrimType {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    Bool,
    Unit,
}

/// Type representation
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Type {
    Prim(PrimType),
    Array(Box<Type>, Option<usize>),
    OArray(Box<Type>, Option<usize>),
    Ref(Box<Type>),
    Struct(String),
    Fn(Vec<Type>, Box<Type>),
}

/// Type with security annotation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnnotatedType {
    pub typ: Type,
    pub security: Security,
}

/// Binary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
}

/// Unary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnOp {
    Neg,
    Not,
    BitNot,
}

/// Literal values
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Literal {
    Int(i64),
    Bool(bool),
    Unit,
}

/// Variable identifier
pub type VarId = String;

/// Expressions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Expr {
    Lit(Literal),
    Var(VarId),
    Binop(BinOp, Box<Expr>, Box<Expr>),
    Unop(UnOp, Box<Expr>),
    Call(String, Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    Field(Box<Expr>, String),
    Cmov(Box<Expr>, Box<Expr>, Box<Expr>),
    OramRead(Box<Expr>, Box<Expr>),
    Struct(String, Vec<(String, Expr)>),
}

/// Instructions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Instr {
    Let(VarId, AnnotatedType, Expr),
    Assign(Expr, Expr),
    OramWrite(Expr, Expr, Expr),
    If(Expr, Block, Block),
    While(Expr, Block),
    For(VarId, Expr, Expr, Block),
    Return(Option<Expr>),
    Expr(Expr),
}

/// A block of instructions
pub type Block = Vec<Instr>;

/// Function definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Function {
    pub name: String,
    pub params: Vec<(VarId, AnnotatedType)>,
    pub return_type: AnnotatedType,
    pub body: Block,
    pub is_oblivious: bool,
    pub is_constant_time: bool,
}

/// Struct definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<(String, AnnotatedType)>,
}

/// External function declaration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExternFunc {
    pub name: String,
    pub params: Vec<AnnotatedType>,
    pub return_type: AnnotatedType,
}

/// A complete module
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Module {
    pub name: Option<String>,
    pub structs: Vec<StructDef>,
    pub externs: Vec<ExternFunc>,
    pub functions: Vec<Function>,
}

impl Type {
    /// Convert type to Rust type string
    pub fn to_rust(&self) -> String {
        match self {
            Type::Prim(p) => p.to_rust().to_string(),
            Type::Array(elem, size) => match size {
                Some(n) => format!("[{}; {}]", elem.to_rust(), n),
                None => format!("Vec<{}>", elem.to_rust()),
            },
            Type::OArray(elem, size) => match size {
                Some(n) => format!("OArray<{}, {}>", elem.to_rust(), n),
                None => format!("OArray<{}>", elem.to_rust()),
            },
            Type::Ref(inner) => format!("&mut {}", inner.to_rust()),
            Type::Struct(name) => name.clone(),
            Type::Fn(params, ret) => {
                let params_str = params.iter().map(|p| p.to_rust()).collect::<Vec<_>>().join(", ");
                format!("fn({}) -> {}", params_str, ret.to_rust())
            }
        }
    }
}

impl PrimType {
    /// Convert primitive type to Rust type string
    pub fn to_rust(&self) -> &'static str {
        match self {
            PrimType::I8 => "i8",
            PrimType::I16 => "i16",
            PrimType::I32 => "i32",
            PrimType::I64 => "i64",
            PrimType::U8 => "u8",
            PrimType::U16 => "u16",
            PrimType::U32 => "u32",
            PrimType::U64 => "u64",
            PrimType::Bool => "bool",
            PrimType::Unit => "()",
        }
    }
}

impl BinOp {
    /// Convert binary operator to Rust operator string
    pub fn to_rust(&self) -> &'static str {
        match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Mod => "%",
            BinOp::Eq => "==",
            BinOp::Ne => "!=",
            BinOp::Lt => "<",
            BinOp::Le => "<=",
            BinOp::Gt => ">",
            BinOp::Ge => ">=",
            BinOp::And => "&&",
            BinOp::Or => "||",
            BinOp::BitAnd => "&",
            BinOp::BitOr => "|",
            BinOp::BitXor => "^",
            BinOp::Shl => "<<",
            BinOp::Shr => ">>",
        }
    }
}

impl UnOp {
    /// Convert unary operator to Rust operator string
    pub fn to_rust(&self) -> &'static str {
        match self {
            UnOp::Neg => "-",
            UnOp::Not => "!",
            UnOp::BitNot => "!",
        }
    }
}
