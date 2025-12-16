// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! # Oblíbený
//!
//! Dual-Language Paradigm compiler for secure edge computing.
//!
//! ## Overview
//!
//! Oblíbený implements a novel compilation model:
//! - **Master Language**: Turing-complete for development (macros, metaprogramming)
//! - **Deployment Subset**: Turing-incomplete for deployment (provably terminating)
//!
//! ## Compilation Stages
//!
//! 1. **Expansion** (Stage 0): Macro expansion, compile-time execution
//! 2. **Verification** (Stage 1): Type checking, termination proofs, resource analysis
//! 3. **Generation** (Stage 2): Optimization, obfuscation, code generation
//!
//! ## Security Features
//!
//! - Capability-based I/O (no syscalls in deployment)
//! - Resource bounds (max iterations, memory, stack, call depth)
//! - Semantic obfuscation (metamorphic code generation)
//! - Termination proofs (call graph acyclicity, bounded loops)

#![warn(missing_docs)]
#![warn(rust_2018_idioms)]
#![deny(unsafe_code)]

pub mod ast;
pub mod error;
pub mod lexer;
pub mod parser;
pub mod span;
pub mod token;

// Re-exports for convenience
pub use ast::TopLevelForm;
pub use error::{Error, Result};
pub use lexer::Lexer;
pub use parser::Parser;
pub use span::Span;
pub use token::Token;

/// Compiler version
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Grammar version this compiler implements
pub const GRAMMAR_VERSION: &str = "0.6";

/// Parse source code into an AST
///
/// # Example
///
/// ```ignore
/// use oblibeny::parse;
///
/// let source = "(defun add (a b) (+ a b))";
/// let ast = parse(source).unwrap();
/// ```
pub fn parse(source: &str) -> Result<Vec<TopLevelForm>> {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer);
    parser.parse_program()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_version() {
        assert_eq!(GRAMMAR_VERSION, "0.6");
    }
}
