// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! String standard library functions.
//!
//! This module provides string manipulation operations.

/// String function implementations (used by interpreter)
pub mod builtins {
    use crate::error::{Error, Result};
    use crate::interpreter::Value;
    use crate::span::Span;

    /// Convert value to string
    pub fn str_convert(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::runtime(
                "str requires exactly 1 argument".to_string(),
                span,
            ));
        }

        Ok(Value::String(format!("{}", args[0])))
    }

    /// Get length of string
    pub fn str_len(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::runtime(
                "str-len requires exactly 1 argument".to_string(),
                span,
            ));
        }

        match &args[0] {
            Value::String(s) => Ok(Value::Int(s.chars().count() as i64)),
            _ => Err(Error::runtime(
                "str-len requires a string argument".to_string(),
                span,
            )),
        }
    }

    /// Get character at index
    pub fn char_at(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::runtime(
                "char-at requires exactly 2 arguments".to_string(),
                span,
            ));
        }

        let s = match &args[0] {
            Value::String(s) => s,
            _ => {
                return Err(Error::runtime(
                    "char-at requires a string as first argument".to_string(),
                    span,
                ))
            }
        };

        let idx = match &args[1] {
            Value::Int(i) => *i as usize,
            _ => {
                return Err(Error::runtime(
                    "char-at requires an integer index".to_string(),
                    span,
                ))
            }
        };

        s.chars()
            .nth(idx)
            .map(|c| Value::String(c.to_string()))
            .ok_or_else(|| Error::runtime("index out of bounds".to_string(), span))
    }

    /// Get substring
    pub fn substr(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 3 {
            return Err(Error::runtime(
                "substr requires exactly 3 arguments".to_string(),
                span,
            ));
        }

        let s = match &args[0] {
            Value::String(s) => s,
            _ => {
                return Err(Error::runtime(
                    "substr requires a string as first argument".to_string(),
                    span,
                ))
            }
        };

        let start = match &args[1] {
            Value::Int(i) => *i as usize,
            _ => {
                return Err(Error::runtime(
                    "substr requires an integer start index".to_string(),
                    span,
                ))
            }
        };

        let len = match &args[2] {
            Value::Int(i) => *i as usize,
            _ => {
                return Err(Error::runtime(
                    "substr requires an integer length".to_string(),
                    span,
                ))
            }
        };

        let result: String = s.chars().skip(start).take(len).collect();
        Ok(Value::String(result))
    }

    /// Concatenate strings
    pub fn str_concat(args: &[Value], span: Span) -> Result<Value> {
        let mut result = String::new();

        for arg in args {
            match arg {
                Value::String(s) => result.push_str(s),
                _ => {
                    return Err(Error::runtime(
                        "str-concat requires string arguments".to_string(),
                        span,
                    ))
                }
            }
        }

        Ok(Value::String(result))
    }

    /// Convert string to uppercase
    pub fn str_upper(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::runtime(
                "str-upper requires exactly 1 argument".to_string(),
                span,
            ));
        }

        match &args[0] {
            Value::String(s) => Ok(Value::String(s.to_uppercase())),
            _ => Err(Error::runtime(
                "str-upper requires a string argument".to_string(),
                span,
            )),
        }
    }

    /// Convert string to lowercase
    pub fn str_lower(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::runtime(
                "str-lower requires exactly 1 argument".to_string(),
                span,
            ));
        }

        match &args[0] {
            Value::String(s) => Ok(Value::String(s.to_lowercase())),
            _ => Err(Error::runtime(
                "str-lower requires a string argument".to_string(),
                span,
            )),
        }
    }

    /// Trim whitespace
    pub fn str_trim(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::runtime(
                "str-trim requires exactly 1 argument".to_string(),
                span,
            ));
        }

        match &args[0] {
            Value::String(s) => Ok(Value::String(s.trim().to_string())),
            _ => Err(Error::runtime(
                "str-trim requires a string argument".to_string(),
                span,
            )),
        }
    }

    /// Check if string starts with prefix
    pub fn str_starts_with(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::runtime(
                "str-starts-with? requires exactly 2 arguments".to_string(),
                span,
            ));
        }

        match (&args[0], &args[1]) {
            (Value::String(s), Value::String(prefix)) => Ok(Value::Bool(s.starts_with(prefix))),
            _ => Err(Error::runtime(
                "str-starts-with? requires string arguments".to_string(),
                span,
            )),
        }
    }

    /// Check if string ends with suffix
    pub fn str_ends_with(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::runtime(
                "str-ends-with? requires exactly 2 arguments".to_string(),
                span,
            ));
        }

        match (&args[0], &args[1]) {
            (Value::String(s), Value::String(suffix)) => Ok(Value::Bool(s.ends_with(suffix))),
            _ => Err(Error::runtime(
                "str-ends-with? requires string arguments".to_string(),
                span,
            )),
        }
    }

    /// Check if string contains substring
    pub fn str_contains(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::runtime(
                "str-contains? requires exactly 2 arguments".to_string(),
                span,
            ));
        }

        match (&args[0], &args[1]) {
            (Value::String(s), Value::String(needle)) => Ok(Value::Bool(s.contains(needle))),
            _ => Err(Error::runtime(
                "str-contains? requires string arguments".to_string(),
                span,
            )),
        }
    }

    /// Find index of substring
    pub fn str_index_of(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::runtime(
                "str-index-of requires exactly 2 arguments".to_string(),
                span,
            ));
        }

        match (&args[0], &args[1]) {
            (Value::String(s), Value::String(needle)) => {
                match s.find(needle) {
                    Some(idx) => Ok(Value::Int(idx as i64)),
                    None => Ok(Value::Int(-1)),
                }
            }
            _ => Err(Error::runtime(
                "str-index-of requires string arguments".to_string(),
                span,
            )),
        }
    }

    /// Replace substring
    pub fn str_replace(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 3 {
            return Err(Error::runtime(
                "str-replace requires exactly 3 arguments".to_string(),
                span,
            ));
        }

        match (&args[0], &args[1], &args[2]) {
            (Value::String(s), Value::String(from), Value::String(to)) => {
                Ok(Value::String(s.replace(from, to)))
            }
            _ => Err(Error::runtime(
                "str-replace requires string arguments".to_string(),
                span,
            )),
        }
    }

    /// Split string by delimiter
    pub fn str_split(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::runtime(
                "str-split requires exactly 2 arguments".to_string(),
                span,
            ));
        }

        match (&args[0], &args[1]) {
            (Value::String(s), Value::String(delim)) => {
                let parts: Vec<Value> = s.split(delim).map(|p| Value::String(p.to_string())).collect();
                Ok(Value::Array(parts))
            }
            _ => Err(Error::runtime(
                "str-split requires string arguments".to_string(),
                span,
            )),
        }
    }

    /// Join array of strings
    pub fn str_join(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::runtime(
                "str-join requires exactly 2 arguments".to_string(),
                span,
            ));
        }

        let arr = match &args[0] {
            Value::Array(a) => a,
            _ => {
                return Err(Error::runtime(
                    "str-join requires an array as first argument".to_string(),
                    span,
                ))
            }
        };

        let sep = match &args[1] {
            Value::String(s) => s,
            _ => {
                return Err(Error::runtime(
                    "str-join requires a string separator".to_string(),
                    span,
                ))
            }
        };

        let mut parts: Vec<String> = Vec::new();
        for elem in arr {
            match elem {
                Value::String(s) => parts.push(s.clone()),
                _ => parts.push(format!("{}", elem)),
            }
        }

        Ok(Value::String(parts.join(sep)))
    }

    /// Repeat string n times
    pub fn str_repeat(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::runtime(
                "str-repeat requires exactly 2 arguments".to_string(),
                span,
            ));
        }

        let s = match &args[0] {
            Value::String(s) => s,
            _ => {
                return Err(Error::runtime(
                    "str-repeat requires a string as first argument".to_string(),
                    span,
                ))
            }
        };

        let n = match &args[1] {
            Value::Int(i) => *i as usize,
            _ => {
                return Err(Error::runtime(
                    "str-repeat requires an integer count".to_string(),
                    span,
                ))
            }
        };

        Ok(Value::String(s.repeat(n)))
    }

    /// Reverse string
    pub fn str_reverse(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::runtime(
                "str-reverse requires exactly 1 argument".to_string(),
                span,
            ));
        }

        match &args[0] {
            Value::String(s) => Ok(Value::String(s.chars().rev().collect())),
            _ => Err(Error::runtime(
                "str-reverse requires a string argument".to_string(),
                span,
            )),
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
    fn test_str_convert() {
        assert_eq!(
            str_convert(&[Value::Int(42)], dummy_span()).unwrap(),
            Value::String("42".to_string())
        );
    }

    #[test]
    fn test_str_len() {
        assert_eq!(
            str_len(&[Value::String("hello".to_string())], dummy_span()).unwrap(),
            Value::Int(5)
        );
    }

    #[test]
    fn test_char_at() {
        assert_eq!(
            char_at(
                &[Value::String("hello".to_string()), Value::Int(1)],
                dummy_span()
            )
            .unwrap(),
            Value::String("e".to_string())
        );
    }

    #[test]
    fn test_substr() {
        assert_eq!(
            substr(
                &[
                    Value::String("hello world".to_string()),
                    Value::Int(0),
                    Value::Int(5)
                ],
                dummy_span()
            )
            .unwrap(),
            Value::String("hello".to_string())
        );
    }

    #[test]
    fn test_str_concat() {
        assert_eq!(
            str_concat(
                &[
                    Value::String("hello".to_string()),
                    Value::String(" ".to_string()),
                    Value::String("world".to_string())
                ],
                dummy_span()
            )
            .unwrap(),
            Value::String("hello world".to_string())
        );
    }

    #[test]
    fn test_str_upper_lower() {
        assert_eq!(
            str_upper(&[Value::String("hello".to_string())], dummy_span()).unwrap(),
            Value::String("HELLO".to_string())
        );
        assert_eq!(
            str_lower(&[Value::String("HELLO".to_string())], dummy_span()).unwrap(),
            Value::String("hello".to_string())
        );
    }

    #[test]
    fn test_str_trim() {
        assert_eq!(
            str_trim(&[Value::String("  hello  ".to_string())], dummy_span()).unwrap(),
            Value::String("hello".to_string())
        );
    }

    #[test]
    fn test_str_contains() {
        assert_eq!(
            str_contains(
                &[
                    Value::String("hello world".to_string()),
                    Value::String("world".to_string())
                ],
                dummy_span()
            )
            .unwrap(),
            Value::Bool(true)
        );
    }

    #[test]
    fn test_str_split_join() {
        let split_result = str_split(
            &[
                Value::String("a,b,c".to_string()),
                Value::String(",".to_string()),
            ],
            dummy_span(),
        )
        .unwrap();

        if let Value::Array(arr) = &split_result {
            assert_eq!(arr.len(), 3);
        }

        let join_result = str_join(
            &[split_result, Value::String("-".to_string())],
            dummy_span(),
        )
        .unwrap();

        assert_eq!(join_result, Value::String("a-b-c".to_string()));
    }
}
