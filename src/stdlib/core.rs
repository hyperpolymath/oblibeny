// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Core standard library functions.
//!
//! This module provides fundamental functions that are commonly used
//! across all programs.

/// Core function implementations (used by interpreter)
pub mod builtins {
    use crate::error::{Error, Result};
    use crate::interpreter::Value;
    use crate::span::Span;

    /// Identity function - returns its argument unchanged
    pub fn identity(args: &[Value], _span: Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::runtime(
                "identity requires exactly 1 argument".to_string(),
                _span,
            ));
        }
        Ok(args[0].clone())
    }

    /// Assert that a condition is true
    pub fn assert(args: &[Value], span: Span) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::runtime(
                "assert requires at least 1 argument".to_string(),
                span,
            ));
        }

        match &args[0] {
            Value::Bool(true) => Ok(Value::Unit),
            Value::Bool(false) => {
                let msg = if args.len() > 1 {
                    format!("{}", args[1])
                } else {
                    "assertion failed".to_string()
                };
                Err(Error::runtime(msg, span))
            }
            _ => Err(Error::runtime(
                "assert requires a boolean".to_string(),
                span,
            )),
        }
    }

    /// Assert that two values are equal
    pub fn assert_eq(args: &[Value], span: Span) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::runtime(
                "assert-eq requires at least 2 arguments".to_string(),
                span,
            ));
        }

        if args[0] == args[1] {
            Ok(Value::Unit)
        } else {
            let msg = if args.len() > 2 {
                format!("{}", args[2])
            } else {
                format!("assertion failed: {} != {}", args[0], args[1])
            };
            Err(Error::runtime(msg, span))
        }
    }

    /// Error/panic function
    pub fn error(args: &[Value], span: Span) -> Result<Value> {
        let msg = if args.is_empty() {
            "error".to_string()
        } else {
            format!("{}", args[0])
        };
        Err(Error::runtime(msg, span))
    }

    /// Unreachable marker
    pub fn unreachable(args: &[Value], span: Span) -> Result<Value> {
        let msg = if args.is_empty() {
            "reached unreachable code".to_string()
        } else {
            format!("reached unreachable: {}", args[0])
        };
        Err(Error::runtime(msg, span))
    }

    /// Debug print (prints to stderr)
    pub fn dbg(args: &[Value], _span: Span) -> Result<Value> {
        for arg in args {
            eprintln!("[debug] {:?}", arg);
        }
        if args.len() == 1 {
            Ok(args[0].clone())
        } else {
            Ok(Value::Unit)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::builtins::*;
    use crate::interpreter::Value;
    use crate::span::Span;

    fn dummy_span() -> Span {
        Span::new(0, 0)
    }

    #[test]
    fn test_identity() {
        let result = identity(&[Value::Int(42)], dummy_span()).unwrap();
        assert_eq!(result, Value::Int(42));
    }

    #[test]
    fn test_assert_true() {
        let result = assert(&[Value::Bool(true)], dummy_span()).unwrap();
        assert_eq!(result, Value::Unit);
    }

    #[test]
    fn test_assert_false() {
        let result = assert(&[Value::Bool(false)], dummy_span());
        assert!(result.is_err());
    }

    #[test]
    fn test_assert_eq_pass() {
        let result = assert_eq(&[Value::Int(1), Value::Int(1)], dummy_span()).unwrap();
        assert_eq!(result, Value::Unit);
    }

    #[test]
    fn test_assert_eq_fail() {
        let result = assert_eq(&[Value::Int(1), Value::Int(2)], dummy_span());
        assert!(result.is_err());
    }
}
