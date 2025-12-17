// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Collections standard library functions.
//!
//! This module provides array and list manipulation operations.

/// Collection function implementations (used by interpreter)
pub mod builtins {
    use crate::error::{Error, Result};
    use crate::interpreter::Value;
    use crate::span::Span;

    /// Get length of array or string
    pub fn len(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::runtime(
                "len requires exactly 1 argument".to_string(),
                span,
            ));
        }

        match &args[0] {
            Value::Array(arr) => Ok(Value::Int(arr.len() as i64)),
            Value::String(s) => Ok(Value::Int(s.chars().count() as i64)),
            _ => Err(Error::runtime(
                "len requires an array or string".to_string(),
                span,
            )),
        }
    }

    /// Get element at index
    pub fn nth(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::runtime(
                "nth requires exactly 2 arguments".to_string(),
                span,
            ));
        }

        let arr = match &args[0] {
            Value::Array(a) => a,
            _ => {
                return Err(Error::runtime(
                    "nth requires an array as first argument".to_string(),
                    span,
                ))
            }
        };

        let idx = match &args[1] {
            Value::Int(i) => *i as usize,
            _ => {
                return Err(Error::runtime(
                    "nth requires an integer index".to_string(),
                    span,
                ))
            }
        };

        arr.get(idx)
            .cloned()
            .ok_or_else(|| Error::runtime("index out of bounds".to_string(), span))
    }

    /// Append element to array (returns new array)
    pub fn push(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::runtime(
                "push requires exactly 2 arguments".to_string(),
                span,
            ));
        }

        let arr = match &args[0] {
            Value::Array(a) => a,
            _ => {
                return Err(Error::runtime(
                    "push requires an array as first argument".to_string(),
                    span,
                ))
            }
        };

        let mut new_arr = arr.clone();
        new_arr.push(args[1].clone());
        Ok(Value::Array(new_arr))
    }

    /// Remove last element (returns new array)
    pub fn pop(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::runtime(
                "pop requires exactly 1 argument".to_string(),
                span,
            ));
        }

        let arr = match &args[0] {
            Value::Array(a) => a,
            _ => {
                return Err(Error::runtime(
                    "pop requires an array argument".to_string(),
                    span,
                ))
            }
        };

        if arr.is_empty() {
            return Err(Error::runtime("cannot pop from empty array".to_string(), span));
        }

        let mut new_arr = arr.clone();
        new_arr.pop();
        Ok(Value::Array(new_arr))
    }

    /// Get first element
    pub fn first(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::runtime(
                "first requires exactly 1 argument".to_string(),
                span,
            ));
        }

        let arr = match &args[0] {
            Value::Array(a) => a,
            _ => {
                return Err(Error::runtime(
                    "first requires an array argument".to_string(),
                    span,
                ))
            }
        };

        arr.first()
            .cloned()
            .ok_or_else(|| Error::runtime("empty array".to_string(), span))
    }

    /// Get last element
    pub fn last(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::runtime(
                "last requires exactly 1 argument".to_string(),
                span,
            ));
        }

        let arr = match &args[0] {
            Value::Array(a) => a,
            _ => {
                return Err(Error::runtime(
                    "last requires an array argument".to_string(),
                    span,
                ))
            }
        };

        arr.last()
            .cloned()
            .ok_or_else(|| Error::runtime("empty array".to_string(), span))
    }

    /// Get rest of array (all but first)
    pub fn rest(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::runtime(
                "rest requires exactly 1 argument".to_string(),
                span,
            ));
        }

        let arr = match &args[0] {
            Value::Array(a) => a,
            _ => {
                return Err(Error::runtime(
                    "rest requires an array argument".to_string(),
                    span,
                ))
            }
        };

        if arr.is_empty() {
            Ok(Value::Array(vec![]))
        } else {
            Ok(Value::Array(arr[1..].to_vec()))
        }
    }

    /// Get init of array (all but last)
    pub fn init(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::runtime(
                "init requires exactly 1 argument".to_string(),
                span,
            ));
        }

        let arr = match &args[0] {
            Value::Array(a) => a,
            _ => {
                return Err(Error::runtime(
                    "init requires an array argument".to_string(),
                    span,
                ))
            }
        };

        if arr.is_empty() {
            Ok(Value::Array(vec![]))
        } else {
            Ok(Value::Array(arr[..arr.len() - 1].to_vec()))
        }
    }

    /// Concatenate arrays
    pub fn concat(args: &[Value], span: Span) -> Result<Value> {
        let mut result = Vec::new();

        for arg in args {
            match arg {
                Value::Array(a) => result.extend(a.clone()),
                _ => {
                    return Err(Error::runtime(
                        "concat requires array arguments".to_string(),
                        span,
                    ))
                }
            }
        }

        Ok(Value::Array(result))
    }

    /// Get slice of array
    pub fn slice(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 3 {
            return Err(Error::runtime(
                "slice requires exactly 3 arguments".to_string(),
                span,
            ));
        }

        let arr = match &args[0] {
            Value::Array(a) => a,
            _ => {
                return Err(Error::runtime(
                    "slice requires an array as first argument".to_string(),
                    span,
                ))
            }
        };

        let start = match &args[1] {
            Value::Int(i) => (*i).max(0) as usize,
            _ => {
                return Err(Error::runtime(
                    "slice requires integer indices".to_string(),
                    span,
                ))
            }
        };

        let end = match &args[2] {
            Value::Int(i) => (*i).max(0) as usize,
            _ => {
                return Err(Error::runtime(
                    "slice requires integer indices".to_string(),
                    span,
                ))
            }
        };

        let start = start.min(arr.len());
        let end = end.min(arr.len());

        if start >= end {
            Ok(Value::Array(vec![]))
        } else {
            Ok(Value::Array(arr[start..end].to_vec()))
        }
    }

    /// Reverse array
    pub fn reverse(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::runtime(
                "reverse requires exactly 1 argument".to_string(),
                span,
            ));
        }

        let arr = match &args[0] {
            Value::Array(a) => a,
            _ => {
                return Err(Error::runtime(
                    "reverse requires an array argument".to_string(),
                    span,
                ))
            }
        };

        let mut result = arr.clone();
        result.reverse();
        Ok(Value::Array(result))
    }

    /// Check if array contains element
    pub fn contains(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::runtime(
                "contains? requires exactly 2 arguments".to_string(),
                span,
            ));
        }

        let arr = match &args[0] {
            Value::Array(a) => a,
            _ => {
                return Err(Error::runtime(
                    "contains? requires an array as first argument".to_string(),
                    span,
                ))
            }
        };

        Ok(Value::Bool(arr.contains(&args[1])))
    }

    /// Find index of element
    pub fn index_of(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::runtime(
                "index-of requires exactly 2 arguments".to_string(),
                span,
            ));
        }

        let arr = match &args[0] {
            Value::Array(a) => a,
            _ => {
                return Err(Error::runtime(
                    "index-of requires an array as first argument".to_string(),
                    span,
                ))
            }
        };

        match arr.iter().position(|x| x == &args[1]) {
            Some(idx) => Ok(Value::Int(idx as i64)),
            None => Ok(Value::Int(-1)),
        }
    }

    /// Check if array is empty
    pub fn is_empty(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 1 {
            return Err(Error::runtime(
                "empty? requires exactly 1 argument".to_string(),
                span,
            ));
        }

        match &args[0] {
            Value::Array(a) => Ok(Value::Bool(a.is_empty())),
            Value::String(s) => Ok(Value::Bool(s.is_empty())),
            _ => Err(Error::runtime(
                "empty? requires an array or string".to_string(),
                span,
            )),
        }
    }

    /// Create range array [start, end)
    pub fn range(args: &[Value], span: Span) -> Result<Value> {
        if args.is_empty() || args.len() > 3 {
            return Err(Error::runtime(
                "range requires 1-3 arguments".to_string(),
                span,
            ));
        }

        let (start, end, step) = match args.len() {
            1 => {
                let end = match &args[0] {
                    Value::Int(i) => *i,
                    _ => {
                        return Err(Error::runtime(
                            "range requires integer arguments".to_string(),
                            span,
                        ))
                    }
                };
                (0, end, 1)
            }
            2 => {
                let start = match &args[0] {
                    Value::Int(i) => *i,
                    _ => {
                        return Err(Error::runtime(
                            "range requires integer arguments".to_string(),
                            span,
                        ))
                    }
                };
                let end = match &args[1] {
                    Value::Int(i) => *i,
                    _ => {
                        return Err(Error::runtime(
                            "range requires integer arguments".to_string(),
                            span,
                        ))
                    }
                };
                (start, end, 1)
            }
            3 => {
                let start = match &args[0] {
                    Value::Int(i) => *i,
                    _ => {
                        return Err(Error::runtime(
                            "range requires integer arguments".to_string(),
                            span,
                        ))
                    }
                };
                let end = match &args[1] {
                    Value::Int(i) => *i,
                    _ => {
                        return Err(Error::runtime(
                            "range requires integer arguments".to_string(),
                            span,
                        ))
                    }
                };
                let step = match &args[2] {
                    Value::Int(i) => *i,
                    _ => {
                        return Err(Error::runtime(
                            "range requires integer arguments".to_string(),
                            span,
                        ))
                    }
                };
                (start, end, step)
            }
            _ => unreachable!(),
        };

        if step == 0 {
            return Err(Error::runtime("range step cannot be zero".to_string(), span));
        }

        let mut result = Vec::new();
        let mut i = start;

        // Limit range size to prevent memory exhaustion
        const MAX_RANGE_SIZE: usize = 1_000_000;

        if step > 0 {
            while i < end && result.len() < MAX_RANGE_SIZE {
                result.push(Value::Int(i));
                i += step;
            }
        } else {
            while i > end && result.len() < MAX_RANGE_SIZE {
                result.push(Value::Int(i));
                i += step;
            }
        }

        Ok(Value::Array(result))
    }

    /// Create array of n copies of value
    pub fn repeat(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::runtime(
                "repeat requires exactly 2 arguments".to_string(),
                span,
            ));
        }

        let n = match &args[1] {
            Value::Int(i) => {
                if *i < 0 {
                    return Err(Error::runtime(
                        "repeat count cannot be negative".to_string(),
                        span,
                    ));
                }
                *i as usize
            }
            _ => {
                return Err(Error::runtime(
                    "repeat requires an integer count".to_string(),
                    span,
                ))
            }
        };

        // Limit to prevent memory exhaustion
        const MAX_REPEAT_SIZE: usize = 1_000_000;
        if n > MAX_REPEAT_SIZE {
            return Err(Error::runtime(
                format!("repeat count too large (max {})", MAX_REPEAT_SIZE),
                span,
            ));
        }

        Ok(Value::Array(vec![args[0].clone(); n]))
    }

    /// Take first n elements
    pub fn take(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::runtime(
                "take requires exactly 2 arguments".to_string(),
                span,
            ));
        }

        let arr = match &args[0] {
            Value::Array(a) => a,
            _ => {
                return Err(Error::runtime(
                    "take requires an array as first argument".to_string(),
                    span,
                ))
            }
        };

        let n = match &args[1] {
            Value::Int(i) => (*i).max(0) as usize,
            _ => {
                return Err(Error::runtime(
                    "take requires an integer count".to_string(),
                    span,
                ))
            }
        };

        let n = n.min(arr.len());
        Ok(Value::Array(arr[..n].to_vec()))
    }

    /// Drop first n elements
    pub fn drop(args: &[Value], span: Span) -> Result<Value> {
        if args.len() != 2 {
            return Err(Error::runtime(
                "drop requires exactly 2 arguments".to_string(),
                span,
            ));
        }

        let arr = match &args[0] {
            Value::Array(a) => a,
            _ => {
                return Err(Error::runtime(
                    "drop requires an array as first argument".to_string(),
                    span,
                ))
            }
        };

        let n = match &args[1] {
            Value::Int(i) => (*i).max(0) as usize,
            _ => {
                return Err(Error::runtime(
                    "drop requires an integer count".to_string(),
                    span,
                ))
            }
        };

        let n = n.min(arr.len());
        Ok(Value::Array(arr[n..].to_vec()))
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
    fn test_len() {
        assert_eq!(
            len(
                &[Value::Array(vec![Value::Int(1), Value::Int(2), Value::Int(3)])],
                dummy_span()
            )
            .unwrap(),
            Value::Int(3)
        );
    }

    #[test]
    fn test_nth() {
        let arr = Value::Array(vec![Value::Int(10), Value::Int(20), Value::Int(30)]);
        assert_eq!(
            nth(&[arr.clone(), Value::Int(1)], dummy_span()).unwrap(),
            Value::Int(20)
        );
    }

    #[test]
    fn test_push_pop() {
        let arr = Value::Array(vec![Value::Int(1), Value::Int(2)]);
        let pushed = push(&[arr.clone(), Value::Int(3)], dummy_span()).unwrap();

        if let Value::Array(a) = &pushed {
            assert_eq!(a.len(), 3);
        }

        let popped = pop(&[pushed], dummy_span()).unwrap();
        if let Value::Array(a) = &popped {
            assert_eq!(a.len(), 2);
        }
    }

    #[test]
    fn test_first_last() {
        let arr = Value::Array(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        assert_eq!(first(&[arr.clone()], dummy_span()).unwrap(), Value::Int(1));
        assert_eq!(last(&[arr], dummy_span()).unwrap(), Value::Int(3));
    }

    #[test]
    fn test_concat() {
        let a = Value::Array(vec![Value::Int(1), Value::Int(2)]);
        let b = Value::Array(vec![Value::Int(3), Value::Int(4)]);
        let result = concat(&[a, b], dummy_span()).unwrap();

        if let Value::Array(arr) = result {
            assert_eq!(arr.len(), 4);
        }
    }

    #[test]
    fn test_slice() {
        let arr = Value::Array(vec![
            Value::Int(1),
            Value::Int(2),
            Value::Int(3),
            Value::Int(4),
            Value::Int(5),
        ]);
        let result = slice(&[arr, Value::Int(1), Value::Int(4)], dummy_span()).unwrap();

        if let Value::Array(a) = result {
            assert_eq!(a.len(), 3);
            assert_eq!(a[0], Value::Int(2));
        }
    }

    #[test]
    fn test_range() {
        let result = range(&[Value::Int(5)], dummy_span()).unwrap();
        if let Value::Array(a) = result {
            assert_eq!(a.len(), 5);
        }

        let result = range(&[Value::Int(2), Value::Int(5)], dummy_span()).unwrap();
        if let Value::Array(a) = result {
            assert_eq!(a.len(), 3);
            assert_eq!(a[0], Value::Int(2));
        }
    }

    #[test]
    fn test_take_drop() {
        let arr = Value::Array(vec![
            Value::Int(1),
            Value::Int(2),
            Value::Int(3),
            Value::Int(4),
        ]);

        let taken = take(&[arr.clone(), Value::Int(2)], dummy_span()).unwrap();
        if let Value::Array(a) = taken {
            assert_eq!(a.len(), 2);
        }

        let dropped = drop(&[arr, Value::Int(2)], dummy_span()).unwrap();
        if let Value::Array(a) = dropped {
            assert_eq!(a.len(), 2);
            assert_eq!(a[0], Value::Int(3));
        }
    }
}
