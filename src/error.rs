// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Error types for the Oblíbený compiler.

use crate::span::Span;
use miette::Diagnostic;
use thiserror::Error;

/// Result type alias for Oblíbený operations.
pub type Result<T> = std::result::Result<T, Error>;

/// Compilation phase where an error occurred.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Phase {
    /// Lexing phase
    Lexer,
    /// Parsing phase
    Parser,
    /// Macro expansion phase
    Expansion,
    /// Type checking phase
    TypeCheck,
    /// Verification phase
    Verification,
    /// Code generation phase
    CodeGen,
}

/// Main error type for the Oblíbený compiler.
#[derive(Debug, Clone, Error, Diagnostic)]
pub enum Error {
    /// Lexer error: unexpected character
    #[error("unexpected character: {char}")]
    #[diagnostic(code(oblibeny::lexer::unexpected_char))]
    UnexpectedChar {
        /// The unexpected character
        char: char,
        /// Location in source
        #[label("here")]
        span: Span,
    },

    /// Lexer error: unterminated string
    #[error("unterminated string literal")]
    #[diagnostic(code(oblibeny::lexer::unterminated_string))]
    UnterminatedString {
        /// Location of string start
        #[label("string started here")]
        span: Span,
    },

    /// Parser error: unexpected token
    #[error("unexpected token: expected {expected}, found {found}")]
    #[diagnostic(code(oblibeny::parser::unexpected_token))]
    UnexpectedToken {
        /// What was expected
        expected: String,
        /// What was found
        found: String,
        /// Location in source
        #[label("here")]
        span: Span,
    },

    /// Parser error: unexpected end of file
    #[error("unexpected end of file")]
    #[diagnostic(code(oblibeny::parser::unexpected_eof))]
    UnexpectedEof {
        /// What was expected
        expected: String,
        /// Location in source
        #[label("end of file")]
        span: Span,
    },

    /// Parser error: unmatched parenthesis
    #[error("unmatched parenthesis")]
    #[diagnostic(code(oblibeny::parser::unmatched_paren))]
    UnmatchedParen {
        /// Location of the unmatched paren
        #[label("unmatched")]
        span: Span,
    },

    /// Phase violation: compile-only construct in deploy-time context
    #[error("{construct} is only allowed in compile-time blocks")]
    #[diagnostic(
        code(oblibeny::phase::compile_only),
        help("wrap this in (compile-time ...) or remove from deploy-time code")
    )]
    PhaseViolation {
        /// The construct that violated phase rules
        construct: String,
        /// Location in source
        #[label("compile-only construct")]
        span: Span,
    },

    /// Recursion detected in deploy-time code
    #[error("recursion detected: {cycle}")]
    #[diagnostic(
        code(oblibeny::termination::recursion),
        help("deploy-time code must have an acyclic call graph")
    )]
    RecursionDetected {
        /// Description of the cycle
        cycle: String,
        /// Location of the recursive call
        #[label("recursive call")]
        span: Span,
    },

    /// Unbounded loop in deploy-time code
    #[error("unbounded loop in deploy-time code")]
    #[diagnostic(
        code(oblibeny::termination::unbounded_loop),
        help("use bounded-for instead of while/for in deploy-time code")
    )]
    UnboundedLoop {
        /// Location of the loop
        #[label("unbounded loop")]
        span: Span,
    },

    /// Resource bounds exceeded
    #[error("{resource} exceeds limit: {actual} > {limit}")]
    #[diagnostic(code(oblibeny::resources::exceeded))]
    ResourceExceeded {
        /// Which resource was exceeded
        resource: String,
        /// Actual usage
        actual: u64,
        /// Limit
        limit: u64,
        /// Location in source
        #[label("exceeds limit")]
        span: Span,
    },

    /// Type mismatch
    #[error("type mismatch: expected {expected}, found {actual}")]
    #[diagnostic(code(oblibeny::types::mismatch))]
    TypeMismatch {
        /// Expected type
        expected: String,
        /// Actual type
        actual: String,
        /// Location in source
        #[label("type mismatch")]
        span: Span,
    },

    /// Undefined variable
    #[error("undefined variable: {name}")]
    #[diagnostic(code(oblibeny::resolve::undefined))]
    UndefinedVariable {
        /// Variable name
        name: String,
        /// Location in source
        #[label("not defined")]
        span: Span,
    },

    /// Capability not granted
    #[error("capability not granted: {capability}")]
    #[diagnostic(
        code(oblibeny::capability::not_granted),
        help("add this capability to the deployment manifest")
    )]
    CapabilityNotGranted {
        /// Capability token
        capability: String,
        /// Location in source
        #[label("requires capability")]
        span: Span,
    },

    /// Termination proof failed
    #[error("cannot prove termination for function {function}")]
    #[diagnostic(
        code(oblibeny::termination::unprovable),
        help("add (prove-terminates (strategy bounded-loop)) annotation")
    )]
    TerminationUnprovable {
        /// Function name
        function: String,
        /// Location in source
        #[label("termination not proven")]
        span: Span,
    },

    /// Duplicate definition
    #[error("duplicate definition: {name}")]
    #[diagnostic(code(oblibeny::resolve::duplicate))]
    DuplicateDefinition {
        /// Name that was redefined
        name: String,
        /// Location of the duplicate definition
        #[label("duplicate definition")]
        span: Span,
    },

    /// I/O error
    #[error("I/O error: {message}")]
    #[diagnostic(code(oblibeny::io::error))]
    IoError {
        /// Error message
        message: String,
    },

    /// Internal compiler error
    #[error("internal compiler error: {message}")]
    #[diagnostic(code(oblibeny::internal::error))]
    InternalError {
        /// Error message
        message: String,
    },

    /// Runtime error during interpretation
    #[error("runtime error: {message}")]
    #[diagnostic(code(oblibeny::runtime::error))]
    RuntimeError {
        /// Error message
        message: String,
        /// Location in source
        #[label("here")]
        span: Span,
    },
}

impl Error {
    /// Create a runtime error
    pub fn runtime(message: String, span: Span) -> Self {
        Error::RuntimeError { message, span }
    }
}

impl Error {
    /// Get the span where the error occurred, if available.
    pub fn span(&self) -> Option<Span> {
        match self {
            Error::UnexpectedChar { span, .. } => Some(*span),
            Error::UnterminatedString { span, .. } => Some(*span),
            Error::UnexpectedToken { span, .. } => Some(*span),
            Error::UnexpectedEof { span, .. } => Some(*span),
            Error::UnmatchedParen { span, .. } => Some(*span),
            Error::PhaseViolation { span, .. } => Some(*span),
            Error::RecursionDetected { span, .. } => Some(*span),
            Error::UnboundedLoop { span, .. } => Some(*span),
            Error::ResourceExceeded { span, .. } => Some(*span),
            Error::TypeMismatch { span, .. } => Some(*span),
            Error::UndefinedVariable { span, .. } => Some(*span),
            Error::CapabilityNotGranted { span, .. } => Some(*span),
            Error::TerminationUnprovable { span, .. } => Some(*span),
            Error::DuplicateDefinition { span, .. } => Some(*span),
            Error::RuntimeError { span, .. } => Some(*span),
            Error::IoError { .. } => None,
            Error::InternalError { .. } => None,
        }
    }
}
