// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Standard library for Oblíbený.
//!
//! This module provides the standard library functions and types
//! available to Oblíbený programs.
//!
//! # Modules
//!
//! - **core**: Core functions (identity, composition, etc.)
//! - **math**: Mathematical operations
//! - **string**: String manipulation
//! - **collections**: List and array operations
//! - **io**: I/O primitives (capability-gated)
//!
//! # Usage
//!
//! The standard library is automatically available in the interpreter
//! and REPL environments. Functions can be called directly:
//!
//! ```ignore
//! (abs -5)      ; => 5
//! (max 1 2 3)   ; => 3
//! (len "hello") ; => 5
//! ```

pub mod collections;
pub mod core;
pub mod io;
pub mod math;
pub mod string;

use crate::ast::TopLevelForm;
use std::collections::HashMap;

/// Standard library manager
pub struct StdLib {
    /// Module definitions as source code
    modules: HashMap<String, String>,
    /// Pre-parsed definitions
    definitions: Vec<TopLevelForm>,
}

impl StdLib {
    /// Create a new standard library instance
    pub fn new() -> Self {
        let mut stdlib = StdLib {
            modules: HashMap::new(),
            definitions: Vec::new(),
        };
        stdlib.load_modules();
        stdlib
    }

    /// Load all standard library modules
    fn load_modules(&mut self) {
        // Core module
        self.modules.insert(
            "core".to_string(),
            r#"
; Identity function
(defun identity (x) x)

; Constant function
(defun const (x y) x)

; Function composition
(defun compose (f g x) (f (g x)))

; Flip argument order
(defun flip (f x y) (f y x))

; Apply function to value
(defun apply (f x) (f x))
"#
            .to_string(),
        );

        // Math module
        self.modules.insert(
            "math".to_string(),
            r#"
; Mathematical constants approximations
(defconst PI 3.14159265358979)
(defconst E 2.71828182845905)
(defconst TAU 6.28318530717959)

; Square a number
(defun square (x) (* x x))

; Cube a number
(defun cube (x) (* x x x))

; Check if number is even
(defun even? (n) (= (mod n 2) 0))

; Check if number is odd
(defun odd? (n) (= (mod n 2) 1))

; Check if number is positive
(defun positive? (n) (> n 0))

; Check if number is negative
(defun negative? (n) (< n 0))

; Check if number is zero
(defun zero? (n) (= n 0))

; Clamp a value between min and max
(defun clamp (x lo hi)
  (if (< x lo) lo
    (if (> x hi) hi x)))

; Linear interpolation
(defun lerp (a b t)
  (+ a (* (- b a) t)))

; Sign of a number (-1, 0, or 1)
(defun sign (x)
  (if (< x 0) -1
    (if (> x 0) 1 0)))
"#
            .to_string(),
        );

        // String module
        self.modules.insert(
            "string".to_string(),
            r#"
; Check if string is empty
(defun empty? (s) (= (len s) 0))

; Check if string is not empty
(defun not-empty? (s) (> (len s) 0))
"#
            .to_string(),
        );

        // Collections module
        self.modules.insert(
            "collections".to_string(),
            r#"
; Check if array/list is empty
(defun empty-list? (xs) (= (len xs) 0))

; Get first element
(defun first (xs) (nth xs 0))

; Get second element
(defun second (xs) (nth xs 1))

; Get third element
(defun third (xs) (nth xs 2))

; Get last element
(defun last (xs) (nth xs (- (len xs) 1)))

; Sum all elements
(defun sum (xs)
  (bounded-for (acc i 0) ((< i (len xs))) ((+ i 1))
    (+ acc (nth xs i))))

; Product of all elements
(defun product (xs)
  (bounded-for (acc i 0) ((< i (len xs))) ((+ i 1))
    (* acc (nth xs i)) 1))
"#
            .to_string(),
        );
    }

    /// Get source code for a module
    pub fn get_module_source(&self, name: &str) -> Option<&str> {
        self.modules.get(name).map(|s| s.as_str())
    }

    /// Get all module names
    pub fn module_names(&self) -> Vec<&str> {
        self.modules.keys().map(|s| s.as_str()).collect()
    }

    /// Get all definitions
    pub fn definitions(&self) -> &[TopLevelForm] {
        &self.definitions
    }

    /// Parse and store all definitions
    pub fn parse_all(&mut self) -> crate::error::Result<()> {
        self.definitions.clear();

        for source in self.modules.values() {
            let lexer = crate::lexer::Lexer::new(source);
            let mut parser = crate::parser::Parser::new(lexer);
            let forms = parser.parse_program()?;
            self.definitions.extend(forms);
        }

        Ok(())
    }

    /// Get built-in function signatures for type checking
    pub fn builtin_signatures() -> HashMap<String, BuiltinSignature> {
        let mut sigs = HashMap::new();

        // Math builtins
        sigs.insert(
            "abs".to_string(),
            BuiltinSignature {
                params: vec![("x".to_string(), TypeHint::Numeric)],
                return_type: TypeHint::Numeric,
                description: "Absolute value".to_string(),
            },
        );
        sigs.insert(
            "min".to_string(),
            BuiltinSignature {
                params: vec![
                    ("a".to_string(), TypeHint::Numeric),
                    ("b".to_string(), TypeHint::Numeric),
                ],
                return_type: TypeHint::Numeric,
                description: "Minimum of two values".to_string(),
            },
        );
        sigs.insert(
            "max".to_string(),
            BuiltinSignature {
                params: vec![
                    ("a".to_string(), TypeHint::Numeric),
                    ("b".to_string(), TypeHint::Numeric),
                ],
                return_type: TypeHint::Numeric,
                description: "Maximum of two values".to_string(),
            },
        );
        sigs.insert(
            "sqrt".to_string(),
            BuiltinSignature {
                params: vec![("x".to_string(), TypeHint::Float)],
                return_type: TypeHint::Float,
                description: "Square root".to_string(),
            },
        );
        sigs.insert(
            "pow".to_string(),
            BuiltinSignature {
                params: vec![
                    ("base".to_string(), TypeHint::Float),
                    ("exp".to_string(), TypeHint::Float),
                ],
                return_type: TypeHint::Float,
                description: "Raise to power".to_string(),
            },
        );
        sigs.insert(
            "sin".to_string(),
            BuiltinSignature {
                params: vec![("x".to_string(), TypeHint::Float)],
                return_type: TypeHint::Float,
                description: "Sine".to_string(),
            },
        );
        sigs.insert(
            "cos".to_string(),
            BuiltinSignature {
                params: vec![("x".to_string(), TypeHint::Float)],
                return_type: TypeHint::Float,
                description: "Cosine".to_string(),
            },
        );
        sigs.insert(
            "tan".to_string(),
            BuiltinSignature {
                params: vec![("x".to_string(), TypeHint::Float)],
                return_type: TypeHint::Float,
                description: "Tangent".to_string(),
            },
        );
        sigs.insert(
            "floor".to_string(),
            BuiltinSignature {
                params: vec![("x".to_string(), TypeHint::Float)],
                return_type: TypeHint::Int,
                description: "Floor (round down)".to_string(),
            },
        );
        sigs.insert(
            "ceil".to_string(),
            BuiltinSignature {
                params: vec![("x".to_string(), TypeHint::Float)],
                return_type: TypeHint::Int,
                description: "Ceiling (round up)".to_string(),
            },
        );
        sigs.insert(
            "round".to_string(),
            BuiltinSignature {
                params: vec![("x".to_string(), TypeHint::Float)],
                return_type: TypeHint::Int,
                description: "Round to nearest integer".to_string(),
            },
        );
        sigs.insert(
            "log".to_string(),
            BuiltinSignature {
                params: vec![("x".to_string(), TypeHint::Float)],
                return_type: TypeHint::Float,
                description: "Natural logarithm".to_string(),
            },
        );
        sigs.insert(
            "exp".to_string(),
            BuiltinSignature {
                params: vec![("x".to_string(), TypeHint::Float)],
                return_type: TypeHint::Float,
                description: "e^x".to_string(),
            },
        );

        // Array builtins
        sigs.insert(
            "len".to_string(),
            BuiltinSignature {
                params: vec![("x".to_string(), TypeHint::Any)],
                return_type: TypeHint::Int,
                description: "Length of array or string".to_string(),
            },
        );
        sigs.insert(
            "nth".to_string(),
            BuiltinSignature {
                params: vec![
                    ("arr".to_string(), TypeHint::Array),
                    ("idx".to_string(), TypeHint::Int),
                ],
                return_type: TypeHint::Any,
                description: "Get element at index".to_string(),
            },
        );
        sigs.insert(
            "push".to_string(),
            BuiltinSignature {
                params: vec![
                    ("arr".to_string(), TypeHint::Array),
                    ("elem".to_string(), TypeHint::Any),
                ],
                return_type: TypeHint::Array,
                description: "Append element to array".to_string(),
            },
        );
        sigs.insert(
            "pop".to_string(),
            BuiltinSignature {
                params: vec![("arr".to_string(), TypeHint::Array)],
                return_type: TypeHint::Array,
                description: "Remove last element".to_string(),
            },
        );
        sigs.insert(
            "concat".to_string(),
            BuiltinSignature {
                params: vec![
                    ("a".to_string(), TypeHint::Array),
                    ("b".to_string(), TypeHint::Array),
                ],
                return_type: TypeHint::Array,
                description: "Concatenate arrays".to_string(),
            },
        );
        sigs.insert(
            "slice".to_string(),
            BuiltinSignature {
                params: vec![
                    ("arr".to_string(), TypeHint::Array),
                    ("start".to_string(), TypeHint::Int),
                    ("end".to_string(), TypeHint::Int),
                ],
                return_type: TypeHint::Array,
                description: "Get subarray".to_string(),
            },
        );

        // String builtins
        sigs.insert(
            "str".to_string(),
            BuiltinSignature {
                params: vec![("x".to_string(), TypeHint::Any)],
                return_type: TypeHint::String,
                description: "Convert to string".to_string(),
            },
        );
        sigs.insert(
            "substr".to_string(),
            BuiltinSignature {
                params: vec![
                    ("s".to_string(), TypeHint::String),
                    ("start".to_string(), TypeHint::Int),
                    ("len".to_string(), TypeHint::Int),
                ],
                return_type: TypeHint::String,
                description: "Get substring".to_string(),
            },
        );
        sigs.insert(
            "char-at".to_string(),
            BuiltinSignature {
                params: vec![
                    ("s".to_string(), TypeHint::String),
                    ("idx".to_string(), TypeHint::Int),
                ],
                return_type: TypeHint::String,
                description: "Get character at index".to_string(),
            },
        );
        sigs.insert(
            "str-concat".to_string(),
            BuiltinSignature {
                params: vec![
                    ("a".to_string(), TypeHint::String),
                    ("b".to_string(), TypeHint::String),
                ],
                return_type: TypeHint::String,
                description: "Concatenate strings".to_string(),
            },
        );

        // Type predicates
        sigs.insert(
            "int?".to_string(),
            BuiltinSignature {
                params: vec![("x".to_string(), TypeHint::Any)],
                return_type: TypeHint::Bool,
                description: "Check if integer".to_string(),
            },
        );
        sigs.insert(
            "float?".to_string(),
            BuiltinSignature {
                params: vec![("x".to_string(), TypeHint::Any)],
                return_type: TypeHint::Bool,
                description: "Check if float".to_string(),
            },
        );
        sigs.insert(
            "bool?".to_string(),
            BuiltinSignature {
                params: vec![("x".to_string(), TypeHint::Any)],
                return_type: TypeHint::Bool,
                description: "Check if boolean".to_string(),
            },
        );
        sigs.insert(
            "string?".to_string(),
            BuiltinSignature {
                params: vec![("x".to_string(), TypeHint::Any)],
                return_type: TypeHint::Bool,
                description: "Check if string".to_string(),
            },
        );
        sigs.insert(
            "array?".to_string(),
            BuiltinSignature {
                params: vec![("x".to_string(), TypeHint::Any)],
                return_type: TypeHint::Bool,
                description: "Check if array".to_string(),
            },
        );
        sigs.insert(
            "function?".to_string(),
            BuiltinSignature {
                params: vec![("x".to_string(), TypeHint::Any)],
                return_type: TypeHint::Bool,
                description: "Check if function".to_string(),
            },
        );

        // I/O (capability gated)
        sigs.insert(
            "print".to_string(),
            BuiltinSignature {
                params: vec![("x".to_string(), TypeHint::Any)],
                return_type: TypeHint::Unit,
                description: "Print value (no newline)".to_string(),
            },
        );
        sigs.insert(
            "println".to_string(),
            BuiltinSignature {
                params: vec![("x".to_string(), TypeHint::Any)],
                return_type: TypeHint::Unit,
                description: "Print value with newline".to_string(),
            },
        );

        // Misc
        sigs.insert(
            "type-of".to_string(),
            BuiltinSignature {
                params: vec![("x".to_string(), TypeHint::Any)],
                return_type: TypeHint::String,
                description: "Get type name".to_string(),
            },
        );
        sigs.insert(
            "panic".to_string(),
            BuiltinSignature {
                params: vec![("msg".to_string(), TypeHint::String)],
                return_type: TypeHint::Never,
                description: "Abort with error message".to_string(),
            },
        );

        sigs
    }

    /// Get the prelude source code
    pub fn prelude_source() -> &'static str {
        r#"
; Oblíbený Prelude
; Automatically loaded definitions

; Boolean negation
(defun not (x) (if x false true))

; Logical and (short-circuit)
(defmacro and (a b) (if a b false))

; Logical or (short-circuit)
(defmacro or (a b) (if a true b))

; Identity function
(defun id (x) x)

; Constant function
(defun const (x _) x)
"#
    }
}

impl Default for StdLib {
    fn default() -> Self {
        Self::new()
    }
}

/// Type hint for builtin signatures
#[derive(Debug, Clone, PartialEq)]
pub enum TypeHint {
    /// Any type
    Any,
    /// Unit type
    Unit,
    /// Never type (for functions that don't return)
    Never,
    /// Boolean
    Bool,
    /// Integer
    Int,
    /// Float
    Float,
    /// Numeric (int or float)
    Numeric,
    /// String
    String,
    /// Array
    Array,
    /// Function
    Function,
}

/// Builtin function signature
#[derive(Debug, Clone)]
pub struct BuiltinSignature {
    /// Parameters (name, type hint)
    pub params: Vec<(String, TypeHint)>,
    /// Return type hint
    pub return_type: TypeHint,
    /// Description
    pub description: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_stdlib_creation() {
        let stdlib = StdLib::new();
        assert!(!stdlib.module_names().is_empty());
    }

    #[test]
    fn test_stdlib_modules() {
        let stdlib = StdLib::new();
        assert!(stdlib.get_module_source("core").is_some());
        assert!(stdlib.get_module_source("math").is_some());
        assert!(stdlib.get_module_source("string").is_some());
        assert!(stdlib.get_module_source("collections").is_some());
    }

    #[test]
    fn test_builtin_signatures() {
        let sigs = StdLib::builtin_signatures();
        assert!(sigs.contains_key("abs"));
        assert!(sigs.contains_key("len"));
        assert!(sigs.contains_key("println"));
    }

    #[test]
    fn test_prelude() {
        let prelude = StdLib::prelude_source();
        assert!(prelude.contains("defun not"));
    }
}
