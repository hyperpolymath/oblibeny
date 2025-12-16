// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! S-expression lexer for Oblíbený.

use crate::error::{Error, Result};
use crate::span::Span;
use crate::token::Token;
use logos::Logos;

/// A token with its source span.
#[derive(Debug, Clone)]
pub struct SpannedToken {
    /// The token.
    pub token: Token,
    /// The source span.
    pub span: Span,
}

/// Lexer for Oblíbený source code.
pub struct Lexer<'a> {
    inner: logos::Lexer<'a, Token>,
    source: &'a str,
    peeked: Option<Result<SpannedToken>>,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer for the given source code.
    pub fn new(source: &'a str) -> Self {
        Self {
            inner: Token::lexer(source),
            source,
            peeked: None,
        }
    }

    /// Get the source code.
    pub fn source(&self) -> &'a str {
        self.source
    }

    /// Peek at the next token without consuming it.
    pub fn peek(&mut self) -> Option<&Result<SpannedToken>> {
        if self.peeked.is_none() {
            self.peeked = self.next_inner();
        }
        self.peeked.as_ref()
    }

    /// Get the next token.
    pub fn next_token(&mut self) -> Option<Result<SpannedToken>> {
        if let Some(peeked) = self.peeked.take() {
            Some(peeked)
        } else {
            self.next_inner()
        }
    }

    fn next_inner(&mut self) -> Option<Result<SpannedToken>> {
        let token_result = self.inner.next()?;
        let span = Span::from(self.inner.span());

        Some(match token_result {
            Ok(token) => Ok(SpannedToken { token, span }),
            Err(()) => {
                // Get the problematic character
                let slice = span.slice(self.source);
                let char = slice.chars().next().unwrap_or('?');
                Err(Error::UnexpectedChar { char, span })
            }
        })
    }

    /// Check if there are more tokens.
    pub fn is_eof(&mut self) -> bool {
        self.peek().is_none()
    }

    /// Get the current position in the source.
    pub fn position(&self) -> usize {
        self.inner.span().end
    }

    /// Create a span from the current position.
    pub fn span_from(&self, start: usize) -> Span {
        Span::new(start, self.position())
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<SpannedToken>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_basic() {
        // Note: "add" is a keyword, so we use "plus" as function name
        let lexer = Lexer::new("(defun plus (a b) (+ a b))");
        let tokens: Vec<_> = lexer
            .filter_map(|r| r.ok())
            .map(|t| t.token)
            .collect();

        assert_eq!(tokens[0], Token::LParen);
        assert_eq!(tokens[1], Token::Defun);
        assert_eq!(tokens[2], Token::Identifier("plus".into()));
    }

    #[test]
    fn test_lexer_peek() {
        let mut lexer = Lexer::new("42 43");

        // Peek should not consume
        assert!(matches!(
            lexer.peek().unwrap().as_ref().unwrap().token,
            Token::Integer(42)
        ));
        assert!(matches!(
            lexer.peek().unwrap().as_ref().unwrap().token,
            Token::Integer(42)
        ));

        // Next should consume
        assert!(matches!(
            lexer.next_token().unwrap().unwrap().token,
            Token::Integer(42)
        ));
        assert!(matches!(
            lexer.next_token().unwrap().unwrap().token,
            Token::Integer(43)
        ));
    }

    #[test]
    fn test_lexer_spans() {
        let mut lexer = Lexer::new("hello");
        let token = lexer.next_token().unwrap().unwrap();
        assert_eq!(token.span.start, 0);
        assert_eq!(token.span.end, 5);
    }
}
