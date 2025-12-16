// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Token definitions for the Oblíbený lexer.

use logos::Logos;

/// Token type for Oblíbený source code.
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\n\r]+")]
#[logos(skip r";[^\n]*")]
#[logos(skip r"\(\*([^*]|\*[^)])*\*\)")]
pub enum Token {
    // Delimiters
    /// Left parenthesis
    #[token("(")]
    LParen,

    /// Right parenthesis
    #[token(")")]
    RParen,

    /// Colon (for type annotations)
    #[token(":")]
    Colon,

    // Keywords - Phase Control
    /// compile-time block
    #[token("compile-time")]
    CompileTime,

    /// deploy-time block
    #[token("deploy-time")]
    DeployTime,

    /// deployment specification
    #[token("deployment")]
    Deployment,

    /// profile
    #[token("profile")]
    Profile,

    /// bounds
    #[token("bounds")]
    Bounds,

    /// obfuscate
    #[token("obfuscate")]
    Obfuscate,

    /// constraints
    #[token("constraints")]
    Constraints,

    // Keywords - Definitions
    /// defun (function definition)
    #[token("defun")]
    Defun,

    /// defmacro
    #[token("defmacro")]
    Defmacro,

    /// def-data
    #[token("def-data")]
    DefData,

    /// let binding
    #[token("let")]
    Let,

    /// module
    #[token("module")]
    Module,

    /// package
    #[token("package")]
    Package,

    /// use (import)
    #[token("use")]
    Use,

    /// export
    #[token("export")]
    Export,

    // Keywords - Control Flow
    /// if
    #[token("if")]
    If,

    /// else
    #[token("else")]
    Else,

    /// while (compile-time only)
    #[token("while")]
    While,

    /// for (compile-time only)
    #[token("for")]
    For,

    /// bounded-for (deploy-time safe)
    #[token("bounded-for")]
    BoundedFor,

    /// ifdef (conditional assembly)
    #[token("ifdef")]
    Ifdef,

    /// call
    #[token("call")]
    Call,

    // Keywords - Error Handling
    /// try
    #[token("try")]
    Try,

    /// catch
    #[token("catch")]
    Catch,

    /// finally
    #[token("finally")]
    Finally,

    // Keywords - Transactions
    /// transaction
    #[token("transaction")]
    Transaction,

    /// commit
    #[token("commit")]
    Commit,

    /// rollback
    #[token("rollback")]
    Rollback,

    /// fail
    #[token("fail")]
    Fail,

    // Keywords - Metamorphic
    /// metamorphic-if
    #[token("metamorphic-if")]
    MetamorphicIf,

    /// rewrite-self
    #[token("rewrite-self")]
    RewriteSelf,

    // Keywords - Annotations
    /// prove-terminates
    #[token("prove-terminates")]
    ProveTerminates,

    /// strategy
    #[token("strategy")]
    Strategy,

    /// complexity
    #[token("complexity")]
    Complexity,

    /// pure
    #[token("pure")]
    Pure,

    /// idempotent
    #[token("idempotent")]
    Idempotent,

    /// deploy-target
    #[token("deploy-target")]
    DeployTarget,

    // Keywords - Capabilities
    /// capability
    #[token("capability")]
    Capability,

    /// grant
    #[token("grant")]
    Grant,

    /// manifest
    #[token("manifest")]
    Manifest,

    /// budget
    #[token("budget")]
    Budget,

    // Keywords - Resource Bounds
    /// max-iterations
    #[token("max-iterations")]
    MaxIterations,

    /// max-stack-depth
    #[token("max-stack-depth")]
    MaxStackDepth,

    /// max-memory
    #[token("max-memory")]
    MaxMemory,

    /// max-call-depth
    #[token("max-call-depth")]
    MaxCallDepth,

    /// max-execution-time
    #[token("max-execution-time")]
    MaxExecutionTime,

    // Keywords - Security
    /// forbid
    #[token("forbid")]
    Forbid,

    /// require
    #[token("require")]
    Require,

    // Keywords - Assembly
    /// mov
    #[token("mov")]
    Mov,

    /// lea
    #[token("lea")]
    Lea,

    /// add
    #[token("add")]
    Add,

    /// sub
    #[token("sub")]
    Sub,

    /// mul
    #[token("mul")]
    Mul,

    /// div
    #[token("div")]
    Div,

    /// inc
    #[token("inc")]
    Inc,

    /// dec
    #[token("dec")]
    Dec,

    /// cmp
    #[token("cmp")]
    Cmp,

    /// jmp
    #[token("jmp")]
    Jmp,

    /// je
    #[token("je")]
    Je,

    /// jne
    #[token("jne")]
    Jne,

    /// jg
    #[token("jg")]
    Jg,

    /// jl
    #[token("jl")]
    Jl,

    /// jge
    #[token("jge")]
    Jge,

    /// jle
    #[token("jle")]
    Jle,

    /// push
    #[token("push")]
    Push,

    /// pop
    #[token("pop")]
    Pop,

    /// syscall (compile-time only)
    #[token("syscall")]
    Syscall,

    /// alloc
    #[token("alloc")]
    Alloc,

    /// free
    #[token("free")]
    Free,

    /// mem
    #[token("mem")]
    Mem,

    // Keywords - Data Directives
    /// db
    #[token("db")]
    Db,

    /// dw
    #[token("dw")]
    Dw,

    /// dd
    #[token("dd")]
    Dd,

    /// dq
    #[token("dq")]
    Dq,

    /// section
    #[token("section")]
    Section,

    /// global
    #[token("global")]
    Global,

    /// extern
    #[token("extern")]
    Extern,

    /// target
    #[token("target")]
    Target,

    // Keywords - Types
    /// struct
    #[token("struct")]
    Struct,

    /// enum
    #[token("enum")]
    Enum,

    /// array
    #[token("array")]
    Array,

    /// ptr
    #[token("ptr")]
    Ptr,

    // Primitive Types
    /// u8
    #[token("u8")]
    U8,

    /// u16
    #[token("u16")]
    U16,

    /// u32
    #[token("u32")]
    U32,

    /// u64
    #[token("u64")]
    U64,

    /// i8
    #[token("i8")]
    I8,

    /// i16
    #[token("i16")]
    I16,

    /// i32
    #[token("i32")]
    I32,

    /// i64
    #[token("i64")]
    I64,

    /// f32
    #[token("f32")]
    F32,

    /// f64
    #[token("f64")]
    F64,

    /// bool type
    #[token("bool")]
    BoolType,

    /// string type
    #[token("string")]
    StringType,

    // Operators
    /// +
    #[token("+")]
    Plus,

    /// -
    #[token("-")]
    Minus,

    /// *
    #[token("*")]
    Star,

    /// /
    #[token("/")]
    Slash,

    /// %
    #[token("%")]
    Percent,

    /// =
    #[token("=")]
    Eq,

    /// !=
    #[token("!=")]
    Neq,

    /// <
    #[token("<")]
    Lt,

    /// >
    #[token(">")]
    Gt,

    /// <=
    #[token("<=")]
    Le,

    /// >=
    #[token(">=")]
    Ge,

    // Logical Keywords
    /// and
    #[token("and")]
    And,

    /// or
    #[token("or")]
    Or,

    /// not
    #[token("not")]
    Not,

    // Booleans
    /// true
    #[token("true")]
    True,

    /// false
    #[token("false")]
    False,

    // Literals
    /// Integer literal (decimal, hex, or binary)
    #[regex(r"-?[0-9]+", |lex| lex.slice().parse::<i64>().ok())]
    #[regex(r"-?0x[0-9a-fA-F]+", |lex| i64::from_str_radix(&lex.slice().replace("0x", "").replace("-", ""), 16).ok().map(|v| if lex.slice().starts_with('-') { -v } else { v }))]
    #[regex(r"-?0b[01]+", |lex| i64::from_str_radix(&lex.slice().replace("0b", "").replace("-", ""), 2).ok().map(|v| if lex.slice().starts_with('-') { -v } else { v }))]
    Integer(i64),

    /// String literal
    #[regex(r#""([^"\\]|\\.)*""#, |lex| {
        let s = lex.slice();
        // Remove quotes and handle escapes
        let inner = &s[1..s.len()-1];
        Some(inner.replace("\\n", "\n")
                  .replace("\\t", "\t")
                  .replace("\\r", "\r")
                  .replace("\\\"", "\"")
                  .replace("\\\\", "\\"))
    })]
    String(String),

    /// Identifier
    #[regex(r"[a-zA-Z][a-zA-Z0-9_-]*", |lex| lex.slice().to_string())]
    Identifier(String),

    /// Register (x86-64)
    #[regex(r"r(ax|bx|cx|dx|si|di|sp|bp|8|9|10|11|12|13|14|15)", |lex| lex.slice().to_string())]
    Register(String),
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::Colon => write!(f, ":"),
            Token::Integer(n) => write!(f, "{}", n),
            Token::String(s) => write!(f, "\"{}\"", s),
            Token::Identifier(s) => write!(f, "{}", s),
            Token::Register(r) => write!(f, "{}", r),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            _ => write!(f, "{:?}", self),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex_integers() {
        let mut lexer = Token::lexer("42 -17 0x1A 0b1010");
        assert_eq!(lexer.next(), Some(Ok(Token::Integer(42))));
        assert_eq!(lexer.next(), Some(Ok(Token::Integer(-17))));
        assert_eq!(lexer.next(), Some(Ok(Token::Integer(26))));
        assert_eq!(lexer.next(), Some(Ok(Token::Integer(10))));
    }

    #[test]
    fn test_lex_string() {
        let mut lexer = Token::lexer(r#""hello\nworld""#);
        assert_eq!(lexer.next(), Some(Ok(Token::String("hello\nworld".into()))));
    }

    #[test]
    fn test_lex_keywords() {
        let mut lexer = Token::lexer("defun let if while bounded-for");
        assert_eq!(lexer.next(), Some(Ok(Token::Defun)));
        assert_eq!(lexer.next(), Some(Ok(Token::Let)));
        assert_eq!(lexer.next(), Some(Ok(Token::If)));
        assert_eq!(lexer.next(), Some(Ok(Token::While)));
        assert_eq!(lexer.next(), Some(Ok(Token::BoundedFor)));
    }

    #[test]
    fn test_lex_comments() {
        let mut lexer = Token::lexer("42 ; comment\n43 (* block *) 44");
        assert_eq!(lexer.next(), Some(Ok(Token::Integer(42))));
        assert_eq!(lexer.next(), Some(Ok(Token::Integer(43))));
        assert_eq!(lexer.next(), Some(Ok(Token::Integer(44))));
    }
}
