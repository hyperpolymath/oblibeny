// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Mathematical standard library functions.
//!
//! This module provides mathematical operations and constants.

/// Mathematical constants
pub mod constants {
    /// Pi
    pub const PI: f64 = std::f64::consts::PI;
    /// Tau (2*Pi)
    pub const TAU: f64 = std::f64::consts::TAU;
    /// Euler's number
    pub const E: f64 = std::f64::consts::E;
    /// Square root of 2
    pub const SQRT_2: f64 = std::f64::consts::SQRT_2;
    /// Natural log of 2
    pub const LN_2: f64 = std::f64::consts::LN_2;
    /// Natural log of 10
    pub const LN_10: f64 = std::f64::consts::LN_10;
}

/// Math function implementations (used by interpreter)
pub mod builtins {
    use crate::error::{Error, Result};
    use crate::interpreter::Value;
    use crate::span::Span;

    /// Convert value to float for math operations
    fn to_float(v: &Value) -> Option<f64> {
        match v {
            Value::Int(i) => Some(*i as f64),
            Value::Float(f) => Some(*f),
            _ => None,
        }
    }

    /// Absolute value
    pub fn abs(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::runtime(
                "abs requires exactly 1 argument".to_string(),
                span,
            ));
        }

        match &args[0] {
            Value::Int(i) => Ok(Value::Int(i.abs())),
            Value::Float(f) => Ok(Value::Float(f.abs())),
            _ => Err(Error::runtime(
                "abs requires a numeric argument".to_string(),
                span,
            )),
        }
    }

    /// Minimum of values
    pub fn min(args: &[Value], span: Span) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::runtime(
                "min requires at least 1 argument".to_string(),
                span,
            ));
        }

        let mut result = to_float(&args[0]).ok_or_else(|| {
            Error::runtime("min requires numeric arguments".to_string(), span)
        })?;

        let mut all_ints = matches!(args[0], Value::Int(_));

        for arg in &args[1..] {
            let val = to_float(arg).ok_or_else(|| {
                Error::runtime("min requires numeric arguments".to_string(), span)
            })?;
            if val < result {
                result = val;
            }
            if !matches!(arg, Value::Int(_)) {
                all_ints = false;
            }
        }

        if all_ints {
            Ok(Value::Int(result as i64))
        } else {
            Ok(Value::Float(result))
        }
    }

    /// Maximum of values
    pub fn max(args: &[Value], span: Span) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::runtime(
                "max requires at least 1 argument".to_string(),
                span,
            ));
        }

        let mut result = to_float(&args[0]).ok_or_else(|| {
            Error::runtime("max requires numeric arguments".to_string(), span)
        })?;

        let mut all_ints = matches!(args[0], Value::Int(_));

        for arg in &args[1..] {
            let val = to_float(arg).ok_or_else(|| {
                Error::runtime("max requires numeric arguments".to_string(), span)
            })?;
            if val > result {
                result = val;
            }
            if !matches!(arg, Value::Int(_)) {
                all_ints = false;
            }
        }

        if all_ints {
            Ok(Value::Int(result as i64))
        } else {
            Ok(Value::Float(result))
        }
    }

    /// Square root
    pub fn sqrt(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::runtime(
                "sqrt requires exactly 1 argument".to_string(),
                span,
            ));
        }

        let val = to_float(&args[0]).ok_or_else(|| {
            Error::runtime("sqrt requires a numeric argument".to_string(), span)
        })?;

        if val < 0.0 {
            return Err(Error::runtime(
                "sqrt of negative number".to_string(),
                span,
            ));
        }

        Ok(Value::Float(val.sqrt()))
    }

    /// Power function
    pub fn pow(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::runtime(
                "pow requires exactly 2 arguments".to_string(),
                span,
            ));
        }

        let base = to_float(&args[0]).ok_or_else(|| {
            Error::runtime("pow requires numeric arguments".to_string(), span)
        })?;
        let exp = to_float(&args[1]).ok_or_else(|| {
            Error::runtime("pow requires numeric arguments".to_string(), span)
        })?;

        Ok(Value::Float(base.powf(exp)))
    }

    /// Sine
    pub fn sin(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::runtime(
                "sin requires exactly 1 argument".to_string(),
                span,
            ));
        }

        let val = to_float(&args[0]).ok_or_else(|| {
            Error::runtime("sin requires a numeric argument".to_string(), span)
        })?;

        Ok(Value::Float(val.sin()))
    }

    /// Cosine
    pub fn cos(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::runtime(
                "cos requires exactly 1 argument".to_string(),
                span,
            ));
        }

        let val = to_float(&args[0]).ok_or_else(|| {
            Error::runtime("cos requires a numeric argument".to_string(), span)
        })?;

        Ok(Value::Float(val.cos()))
    }

    /// Tangent
    pub fn tan(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::runtime(
                "tan requires exactly 1 argument".to_string(),
                span,
            ));
        }

        let val = to_float(&args[0]).ok_or_else(|| {
            Error::runtime("tan requires a numeric argument".to_string(), span)
        })?;

        Ok(Value::Float(val.tan()))
    }

    /// Arc sine
    pub fn asin(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::runtime(
                "asin requires exactly 1 argument".to_string(),
                span,
            ));
        }

        let val = to_float(&args[0]).ok_or_else(|| {
            Error::runtime("asin requires a numeric argument".to_string(), span)
        })?;

        if val < -1.0 || val > 1.0 {
            return Err(Error::runtime(
                "asin argument must be in [-1, 1]".to_string(),
                span,
            ));
        }

        Ok(Value::Float(val.asin()))
    }

    /// Arc cosine
    pub fn acos(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::runtime(
                "acos requires exactly 1 argument".to_string(),
                span,
            ));
        }

        let val = to_float(&args[0]).ok_or_else(|| {
            Error::runtime("acos requires a numeric argument".to_string(), span)
        })?;

        if val < -1.0 || val > 1.0 {
            return Err(Error::runtime(
                "acos argument must be in [-1, 1]".to_string(),
                span,
            ));
        }

        Ok(Value::Float(val.acos()))
    }

    /// Arc tangent
    pub fn atan(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::runtime(
                "atan requires exactly 1 argument".to_string(),
                span,
            ));
        }

        let val = to_float(&args[0]).ok_or_else(|| {
            Error::runtime("atan requires a numeric argument".to_string(), span)
        })?;

        Ok(Value::Float(val.atan()))
    }

    /// Two-argument arc tangent
    pub fn atan2(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::runtime(
                "atan2 requires exactly 2 arguments".to_string(),
                span,
            ));
        }

        let y = to_float(&args[0]).ok_or_else(|| {
            Error::runtime("atan2 requires numeric arguments".to_string(), span)
        })?;
        let x = to_float(&args[1]).ok_or_else(|| {
            Error::runtime("atan2 requires numeric arguments".to_string(), span)
        })?;

        Ok(Value::Float(y.atan2(x)))
    }

    /// Natural logarithm
    pub fn log(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::runtime(
                "log requires exactly 1 argument".to_string(),
                span,
            ));
        }

        let val = to_float(&args[0]).ok_or_else(|| {
            Error::runtime("log requires a numeric argument".to_string(), span)
        })?;

        if val <= 0.0 {
            return Err(Error::runtime(
                "log of non-positive number".to_string(),
                span,
            ));
        }

        Ok(Value::Float(val.ln()))
    }

    /// Base-10 logarithm
    pub fn log10(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::runtime(
                "log10 requires exactly 1 argument".to_string(),
                span,
            ));
        }

        let val = to_float(&args[0]).ok_or_else(|| {
            Error::runtime("log10 requires a numeric argument".to_string(), span)
        })?;

        if val <= 0.0 {
            return Err(Error::runtime(
                "log10 of non-positive number".to_string(),
                span,
            ));
        }

        Ok(Value::Float(val.log10()))
    }

    /// Base-2 logarithm
    pub fn log2(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::runtime(
                "log2 requires exactly 1 argument".to_string(),
                span,
            ));
        }

        let val = to_float(&args[0]).ok_or_else(|| {
            Error::runtime("log2 requires a numeric argument".to_string(), span)
        })?;

        if val <= 0.0 {
            return Err(Error::runtime(
                "log2 of non-positive number".to_string(),
                span,
            ));
        }

        Ok(Value::Float(val.log2()))
    }

    /// Exponential (e^x)
    pub fn exp(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::runtime(
                "exp requires exactly 1 argument".to_string(),
                span,
            ));
        }

        let val = to_float(&args[0]).ok_or_else(|| {
            Error::runtime("exp requires a numeric argument".to_string(), span)
        })?;

        Ok(Value::Float(val.exp()))
    }

    /// Floor
    pub fn floor(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::runtime(
                "floor requires exactly 1 argument".to_string(),
                span,
            ));
        }

        let val = to_float(&args[0]).ok_or_else(|| {
            Error::runtime("floor requires a numeric argument".to_string(), span)
        })?;

        Ok(Value::Int(val.floor() as i64))
    }

    /// Ceiling
    pub fn ceil(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::runtime(
                "ceil requires exactly 1 argument".to_string(),
                span,
            ));
        }

        let val = to_float(&args[0]).ok_or_else(|| {
            Error::runtime("ceil requires a numeric argument".to_string(), span)
        })?;

        Ok(Value::Int(val.ceil() as i64))
    }

    /// Round
    pub fn round(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::runtime(
                "round requires exactly 1 argument".to_string(),
                span,
            ));
        }

        let val = to_float(&args[0]).ok_or_else(|| {
            Error::runtime("round requires a numeric argument".to_string(), span)
        })?;

        Ok(Value::Int(val.round() as i64))
    }

    /// Truncate
    pub fn trunc(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::runtime(
                "trunc requires exactly 1 argument".to_string(),
                span,
            ));
        }

        let val = to_float(&args[0]).ok_or_else(|| {
            Error::runtime("trunc requires a numeric argument".to_string(), span)
        })?;

        Ok(Value::Int(val.trunc() as i64))
    }

    /// Modulo operation
    pub fn modulo(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::runtime(
                "mod requires exactly 2 arguments".to_string(),
                span,
            ));
        }

        match (&args[0], &args[1]) {
            (Value::Int(a), Value::Int(b)) => {
                if *b == 0 {
                    return Err(Error::runtime("division by zero".to_string(), span));
                }
                Ok(Value::Int(a % b))
            }
            _ => {
                let a = to_float(&args[0]).ok_or_else(|| {
                    Error::runtime("mod requires numeric arguments".to_string(), span)
                })?;
                let b = to_float(&args[1]).ok_or_else(|| {
                    Error::runtime("mod requires numeric arguments".to_string(), span)
                })?;

                if b == 0.0 {
                    return Err(Error::runtime("division by zero".to_string(), span));
                }

                Ok(Value::Float(a % b))
            }
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
    fn test_abs() {
        assert_eq!(abs(&[Value::Int(-5)], dummy_span()).unwrap(), Value::Int(5));
        assert_eq!(
            abs(&[Value::Float(-3.14)], dummy_span()).unwrap(),
            Value::Float(3.14)
        );
    }

    #[test]
    fn test_min_max() {
        assert_eq!(
            min(&[Value::Int(1), Value::Int(2), Value::Int(3)], dummy_span()).unwrap(),
            Value::Int(1)
        );
        assert_eq!(
            max(&[Value::Int(1), Value::Int(2), Value::Int(3)], dummy_span()).unwrap(),
            Value::Int(3)
        );
    }

    #[test]
    fn test_sqrt() {
        assert_eq!(
            sqrt(&[Value::Float(4.0)], dummy_span()).unwrap(),
            Value::Float(2.0)
        );
    }

    #[test]
    fn test_pow() {
        assert_eq!(
            pow(&[Value::Float(2.0), Value::Float(3.0)], dummy_span()).unwrap(),
            Value::Float(8.0)
        );
    }

    #[test]
    fn test_floor_ceil_round() {
        assert_eq!(
            floor(&[Value::Float(3.7)], dummy_span()).unwrap(),
            Value::Int(3)
        );
        assert_eq!(
            ceil(&[Value::Float(3.2)], dummy_span()).unwrap(),
            Value::Int(4)
        );
        assert_eq!(
            round(&[Value::Float(3.5)], dummy_span()).unwrap(),
            Value::Int(4)
        );
    }
}
