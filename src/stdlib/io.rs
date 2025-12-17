// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! I/O standard library functions.
//!
//! This module provides I/O primitives. In deployment mode, these are
//! capability-gated and require explicit capability tokens to use.
//!
//! # Security Model
//!
//! In the Dual-Language Paradigm:
//! - **Master phase (compile-time)**: Full I/O access for tooling
//! - **Deployment phase (runtime)**: I/O requires capability tokens
//!
//! This prevents deployment code from performing unexpected I/O operations
//! while allowing full flexibility during development.

/// I/O function implementations (used by interpreter)
pub mod builtins {
    use crate::error::{Error, Result};
    use crate::interpreter::Value;
    use crate::span::Span;

    /// Print value without newline
    pub fn print(args: &[Value], _span: Span) -> Result<Value> {
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                print!(" ");
            }
            print!("{}", arg);
        }
        Ok(Value::Unit)
    }

    /// Print value with newline
    pub fn println(args: &[Value], _span: Span) -> Result<Value> {
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                print!(" ");
            }
            print!("{}", arg);
        }
        println!();
        Ok(Value::Unit)
    }

    /// Print formatted string (like printf)
    pub fn printf(args: &[Value], span: Span) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::runtime(
                "printf requires at least 1 argument".to_string(),
                span,
            ));
        }

        let format_str = match &args[0] {
            Value::String(s) => s,
            _ => {
                return Err(Error::runtime(
                    "printf requires a format string".to_string(),
                    span,
                ))
            }
        };

        // Simple format string processing
        let mut result = String::new();
        let mut chars = format_str.chars().peekable();
        let mut arg_idx = 1;

        while let Some(c) = chars.next() {
            if c == '%' {
                match chars.peek() {
                    Some('%') => {
                        chars.next();
                        result.push('%');
                    }
                    Some('s') | Some('d') | Some('v') => {
                        chars.next();
                        if arg_idx < args.len() {
                            result.push_str(&format!("{}", args[arg_idx]));
                            arg_idx += 1;
                        } else {
                            result.push_str("<missing>");
                        }
                    }
                    Some('n') => {
                        chars.next();
                        result.push('\n');
                    }
                    Some('t') => {
                        chars.next();
                        result.push('\t');
                    }
                    _ => {
                        result.push('%');
                    }
                }
            } else {
                result.push(c);
            }
        }

        print!("{}", result);
        Ok(Value::Unit)
    }

    /// Debug print value
    pub fn eprint(args: &[Value], _span: Span) -> Result<Value> {
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                eprint!(" ");
            }
            eprint!("{}", arg);
        }
        Ok(Value::Unit)
    }

    /// Debug print value with newline
    pub fn eprintln(args: &[Value], _span: Span) -> Result<Value> {
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                eprint!(" ");
            }
            eprint!("{}", arg);
        }
        eprintln!();
        Ok(Value::Unit)
    }

    /// Format value to string
    pub fn format(args: &[Value], span: Span) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::String(String::new()));
        }

        let format_str = match &args[0] {
            Value::String(s) => s,
            _ => {
                return Err(Error::runtime(
                    "format requires a format string".to_string(),
                    span,
                ))
            }
        };

        // Simple format string processing
        let mut result = String::new();
        let mut chars = format_str.chars().peekable();
        let mut arg_idx = 1;

        while let Some(c) = chars.next() {
            if c == '%' {
                match chars.peek() {
                    Some('%') => {
                        chars.next();
                        result.push('%');
                    }
                    Some('s') | Some('d') | Some('v') => {
                        chars.next();
                        if arg_idx < args.len() {
                            result.push_str(&format!("{}", args[arg_idx]));
                            arg_idx += 1;
                        } else {
                            result.push_str("<missing>");
                        }
                    }
                    Some('n') => {
                        chars.next();
                        result.push('\n');
                    }
                    Some('t') => {
                        chars.next();
                        result.push('\t');
                    }
                    _ => {
                        result.push('%');
                    }
                }
            } else {
                result.push(c);
            }
        }

        Ok(Value::String(result))
    }

    // Note: File I/O operations are intentionally not provided here.
    // In the deployment subset, all I/O must go through capability tokens.
    // File operations would be provided through a capability-based filesystem
    // API that requires explicit capability grants.

    /// Placeholder for capability-gated file read
    /// In deployment mode, this requires a FileRead capability token.
    pub fn read_file_capability(args: &[Value], span: Span) -> Result<Value> {
        // This is a stub - actual implementation would check capabilities
        if args.len() < 2 {
            return Err(Error::runtime(
                "read-file requires a capability token and path".to_string(),
                span,
            ));
        }

        // In compile-time/master mode, we could allow this
        // In deployment mode, we'd verify the capability token

        Err(Error::runtime(
            "read-file: capability system not yet implemented".to_string(),
            span,
        ))
    }

    /// Placeholder for capability-gated file write
    /// In deployment mode, this requires a FileWrite capability token.
    pub fn write_file_capability(args: &[Value], span: Span) -> Result<Value> {
        // This is a stub - actual implementation would check capabilities
        if args.len() < 3 {
            return Err(Error::runtime(
                "write-file requires a capability token, path, and content".to_string(),
                span,
            ));
        }

        Err(Error::runtime(
            "write-file: capability system not yet implemented".to_string(),
            span,
        ))
    }
}

/// Capability types for I/O operations
#[derive(Debug, Clone, PartialEq)]
pub enum IoCapability {
    /// Read from stdout/stderr
    ConsoleRead,
    /// Write to stdout/stderr
    ConsoleWrite,
    /// Read from specific file path
    FileRead(String),
    /// Write to specific file path
    FileWrite(String),
    /// Network access to specific host
    Network(String),
}

/// Capability token for runtime verification
#[derive(Debug, Clone)]
pub struct CapabilityToken {
    /// Unique token ID
    pub id: u64,
    /// Capability granted
    pub capability: IoCapability,
    /// Whether token has been revoked
    pub revoked: bool,
}

impl CapabilityToken {
    /// Create a new capability token
    pub fn new(id: u64, capability: IoCapability) -> Self {
        CapabilityToken {
            id,
            capability,
            revoked: false,
        }
    }

    /// Check if this token grants the requested capability
    pub fn grants(&self, requested: &IoCapability) -> bool {
        if self.revoked {
            return false;
        }
        &self.capability == requested
    }

    /// Revoke this token
    pub fn revoke(&mut self) {
        self.revoked = true;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_capability_token() {
        let mut token = CapabilityToken::new(1, IoCapability::ConsoleWrite);

        assert!(token.grants(&IoCapability::ConsoleWrite));
        assert!(!token.grants(&IoCapability::ConsoleRead));

        token.revoke();
        assert!(!token.grants(&IoCapability::ConsoleWrite));
    }

    #[test]
    fn test_file_capability() {
        let token = CapabilityToken::new(2, IoCapability::FileRead("/tmp/test.txt".to_string()));

        assert!(token.grants(&IoCapability::FileRead("/tmp/test.txt".to_string())));
        assert!(!token.grants(&IoCapability::FileRead("/etc/passwd".to_string())));
    }
}
