// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! S-expression parser for Oblíbený.

use crate::ast::*;
use crate::error::{Error, Result};
use crate::lexer::{Lexer, SpannedToken};
use crate::span::Span;
use crate::token::Token;

/// Parser for Oblíbený source code.
pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    /// Create a new parser from a lexer.
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self { lexer }
    }

    /// Parse a complete program.
    pub fn parse_program(&mut self) -> Result<Vec<TopLevelForm>> {
        let mut forms = Vec::new();
        while !self.is_eof() {
            forms.push(self.parse_top_level()?);
        }
        Ok(forms)
    }

    /// Parse a top-level form.
    fn parse_top_level(&mut self) -> Result<TopLevelForm> {
        let start = self.expect(Token::LParen)?;

        let form = match self.peek_token()? {
            Token::Module => TopLevelForm::Module(self.parse_module(start.span)?),
            Token::Package => TopLevelForm::Package(self.parse_package(start.span)?),
            Token::Defun => TopLevelForm::Function(self.parse_function(start.span)?),
            Token::Defmacro => TopLevelForm::Macro(self.parse_macro(start.span)?),
            Token::DefData => TopLevelForm::GlobalData(self.parse_global_data(start.span)?),
            Token::Deployment => TopLevelForm::DeploymentSpec(self.parse_deployment(start.span)?),
            Token::CompileTime => TopLevelForm::CompileTimeBlock(self.parse_compile_time(start.span)?),
            Token::Ifdef => TopLevelForm::ConditionalAssembly(self.parse_conditional_assembly(start.span)?),
            Token::Target | Token::Section | Token::Global | Token::Extern => {
                TopLevelForm::AssemblyDirective(self.parse_assembly_directive(start.span)?)
            }
            token => {
                return Err(Error::UnexpectedToken {
                    expected: "top-level form keyword".into(),
                    found: token.to_string(),
                    span: self.current_span(),
                });
            }
        };

        Ok(form)
    }

    /// Parse a function definition.
    fn parse_function(&mut self, start: Span) -> Result<FunctionDef> {
        self.expect(Token::Defun)?;
        let name = self.parse_identifier()?;
        let params = self.parse_parameter_list()?;

        // Optional return type
        let return_type = if self.check(Token::Colon) {
            self.advance()?;
            Some(self.parse_type()?)
        } else {
            None
        };

        // Parse annotations
        let mut annotations = Vec::new();
        while self.check(Token::LParen) && self.peek_ahead_is_annotation()? {
            annotations.push(self.parse_annotation()?);
        }

        // Parse body statements until closing paren
        let mut body = Vec::new();
        while !self.check(Token::RParen) {
            body.push(self.parse_statement()?);
        }

        let end = self.expect(Token::RParen)?;

        Ok(FunctionDef {
            name,
            params,
            return_type,
            annotations,
            body,
            span: start.merge(end.span),
        })
    }

    /// Parse a parameter list.
    fn parse_parameter_list(&mut self) -> Result<Vec<Parameter>> {
        self.expect(Token::LParen)?;
        let mut params = Vec::new();

        while !self.check(Token::RParen) {
            params.push(self.parse_parameter()?);
        }

        self.expect(Token::RParen)?;
        Ok(params)
    }

    /// Parse a single parameter.
    fn parse_parameter(&mut self) -> Result<Parameter> {
        let name = self.parse_identifier()?;
        let start = name.span;

        let ty = if self.check(Token::Colon) {
            self.advance()?;
            Some(self.parse_type()?)
        } else {
            None
        };

        Ok(Parameter {
            span: start,
            name,
            ty,
        })
    }

    /// Parse a type expression.
    fn parse_type(&mut self) -> Result<TypeExpr> {
        // Check for primitive types first
        if let Some(prim) = self.try_parse_primitive_type()? {
            return Ok(TypeExpr::Primitive(prim));
        }

        // Check for compound types
        if self.check(Token::LParen) {
            self.advance()?;
            let ty = match self.peek_token()? {
                Token::Array => {
                    self.advance()?;
                    let elem = Box::new(self.parse_type()?);
                    let size = self.parse_integer()? as u64;
                    self.expect(Token::RParen)?;
                    TypeExpr::Array { elem, size }
                }
                Token::Ptr => {
                    self.advance()?;
                    let inner = Box::new(self.parse_type()?);
                    self.expect(Token::RParen)?;
                    TypeExpr::Pointer(inner)
                }
                Token::Struct => {
                    self.advance()?;
                    let mut fields = Vec::new();
                    while !self.check(Token::RParen) {
                        let name = self.parse_identifier()?;
                        self.expect(Token::Colon)?;
                        let ty = self.parse_type()?;
                        fields.push(FieldDef { name, ty });
                    }
                    self.expect(Token::RParen)?;
                    TypeExpr::Struct(fields)
                }
                Token::Enum => {
                    self.advance()?;
                    let mut variants = Vec::new();
                    while !self.check(Token::RParen) {
                        variants.push(self.parse_identifier()?);
                    }
                    self.expect(Token::RParen)?;
                    TypeExpr::Enum(variants)
                }
                _ => {
                    return Err(Error::UnexpectedToken {
                        expected: "type constructor".into(),
                        found: self.peek_token()?.to_string(),
                        span: self.current_span(),
                    });
                }
            };
            return Ok(ty);
        }

        // User-defined type
        let name = self.parse_identifier()?;
        Ok(TypeExpr::User(name))
    }

    /// Try to parse a primitive type.
    fn try_parse_primitive_type(&mut self) -> Result<Option<PrimitiveType>> {
        let prim = match self.peek_token()? {
            Token::U8 => Some(PrimitiveType::U8),
            Token::U16 => Some(PrimitiveType::U16),
            Token::U32 => Some(PrimitiveType::U32),
            Token::U64 => Some(PrimitiveType::U64),
            Token::I8 => Some(PrimitiveType::I8),
            Token::I16 => Some(PrimitiveType::I16),
            Token::I32 => Some(PrimitiveType::I32),
            Token::I64 => Some(PrimitiveType::I64),
            Token::F32 => Some(PrimitiveType::F32),
            Token::F64 => Some(PrimitiveType::F64),
            Token::BoolType => Some(PrimitiveType::Bool),
            Token::StringType => Some(PrimitiveType::String),
            _ => None,
        };

        if prim.is_some() {
            self.advance()?;
        }

        Ok(prim)
    }

    /// Parse a statement.
    fn parse_statement(&mut self) -> Result<Statement> {
        if self.check(Token::LParen) {
            let start = self.advance()?;
            let stmt = match self.peek_token()? {
                Token::Let => Statement::LocalData(self.parse_local_data(start.span)?),
                Token::Call => Statement::Call(self.parse_call(start.span)?),
                Token::While => Statement::Loop(LoopStmt::While(self.parse_while(start.span)?)),
                Token::For => Statement::Loop(LoopStmt::For(self.parse_for(start.span)?)),
                Token::BoundedFor => Statement::Loop(LoopStmt::BoundedFor(self.parse_bounded_for(start.span)?)),
                Token::Transaction => Statement::Transaction(self.parse_transaction(start.span)?),
                Token::Try => Statement::TryCatch(self.parse_try_catch(start.span)?),
                Token::Ifdef => Statement::ConditionalAssembly(self.parse_conditional_assembly(start.span)?),
                Token::DeployTime => Statement::DeployTimeBlock(self.parse_deploy_time(start.span)?),
                Token::Mov | Token::Lea => Statement::Instruction(self.parse_data_transfer(start.span)?),
                Token::Add | Token::Sub | Token::Mul | Token::Div | Token::Inc | Token::Dec => {
                    Statement::Instruction(self.parse_arithmetic_instr(start.span)?)
                }
                Token::Push | Token::Pop => Statement::Instruction(self.parse_stack_op(start.span)?),
                Token::Alloc | Token::Free => Statement::Instruction(self.parse_memory_op(start.span)?),
                Token::Syscall => Statement::Instruction(self.parse_syscall(start.span)?),
                Token::Commit | Token::Rollback | Token::Fail => {
                    Statement::Instruction(self.parse_checkpoint(start.span)?)
                }
                _ => {
                    // Try to parse as expression
                    Statement::Expression(self.parse_expression_from_paren(start.span)?)
                }
            };
            Ok(stmt)
        } else if let Token::Identifier(_) = self.peek_token()? {
            // Could be a label or variable
            let ident = self.parse_identifier()?;
            if self.check(Token::Colon) {
                self.advance()?;
                Ok(Statement::Label(ident))
            } else {
                Ok(Statement::Expression(Expression::Variable(ident)))
            }
        } else {
            // Literal expression
            Ok(Statement::Expression(self.parse_literal_expression()?))
        }
    }

    /// Parse an expression.
    fn parse_expression(&mut self) -> Result<Expression> {
        if self.check(Token::LParen) {
            let start = self.advance()?;
            self.parse_expression_from_paren(start.span)
        } else {
            self.parse_literal_expression()
        }
    }

    /// Parse expression after seeing opening paren.
    fn parse_expression_from_paren(&mut self, _start: Span) -> Result<Expression> {
        let expr = match self.peek_token()? {
            Token::Plus | Token::Minus | Token::Star | Token::Slash | Token::Percent => {
                self.parse_arithmetic_expr()?
            }
            Token::Eq | Token::Neq | Token::Lt | Token::Gt | Token::Le | Token::Ge => {
                self.parse_comparison_expr()?
            }
            Token::And | Token::Or | Token::Not => {
                self.parse_logical_expr()?
            }
            Token::Mem => {
                self.parse_memory_expr()?
            }
            Token::Capability => {
                self.parse_capability_invoke()?
            }
            Token::Identifier(_) => {
                // Function call expression
                let name = self.parse_identifier()?;
                let mut args = Vec::new();
                while !self.check(Token::RParen) {
                    args.push(self.parse_expression()?);
                }
                self.expect(Token::RParen)?;
                Expression::Call(FunctionCall {
                    span: name.span,
                    name,
                    args,
                })
            }
            _ => {
                return Err(Error::UnexpectedToken {
                    expected: "expression".into(),
                    found: self.peek_token()?.to_string(),
                    span: self.current_span(),
                });
            }
        };
        Ok(expr)
    }

    /// Parse a literal expression.
    fn parse_literal_expression(&mut self) -> Result<Expression> {
        let token = self.advance()?;
        match token.token {
            Token::Integer(n) => Ok(Expression::Literal(Literal::Integer(n))),
            Token::String(s) => Ok(Expression::Literal(Literal::String(s))),
            Token::True => Ok(Expression::Literal(Literal::Bool(true))),
            Token::False => Ok(Expression::Literal(Literal::Bool(false))),
            Token::Identifier(name) => Ok(Expression::Variable(Identifier::new(name, token.span))),
            _ => Err(Error::UnexpectedToken {
                expected: "literal or identifier".into(),
                found: token.token.to_string(),
                span: token.span,
            }),
        }
    }

    /// Parse an arithmetic expression.
    fn parse_arithmetic_expr(&mut self) -> Result<Expression> {
        let op_token = self.advance()?;
        let op = match op_token.token {
            Token::Plus => BinaryArithOp::Add,
            Token::Minus => BinaryArithOp::Sub,
            Token::Star => BinaryArithOp::Mul,
            Token::Slash => BinaryArithOp::Div,
            Token::Percent => BinaryArithOp::Mod,
            _ => unreachable!(),
        };

        let left = Box::new(self.parse_expression()?);
        let right = Box::new(self.parse_expression()?);
        let end = self.expect(Token::RParen)?;

        Ok(Expression::Arithmetic(ArithmeticExpr {
            op,
            left,
            right,
            span: op_token.span.merge(end.span),
        }))
    }

    /// Parse a comparison expression.
    fn parse_comparison_expr(&mut self) -> Result<Expression> {
        let op_token = self.advance()?;
        let op = match op_token.token {
            Token::Eq => ComparisonOp::Eq,
            Token::Neq => ComparisonOp::Neq,
            Token::Lt => ComparisonOp::Lt,
            Token::Gt => ComparisonOp::Gt,
            Token::Le => ComparisonOp::Le,
            Token::Ge => ComparisonOp::Ge,
            _ => unreachable!(),
        };

        let left = Box::new(self.parse_expression()?);
        let right = Box::new(self.parse_expression()?);
        let end = self.expect(Token::RParen)?;

        Ok(Expression::Comparison(ComparisonExpr {
            op,
            left,
            right,
            span: op_token.span.merge(end.span),
        }))
    }

    /// Parse a logical expression.
    fn parse_logical_expr(&mut self) -> Result<Expression> {
        let op_token = self.advance()?;
        let op = match op_token.token {
            Token::And => LogicalOp::And,
            Token::Or => LogicalOp::Or,
            Token::Not => LogicalOp::Not,
            _ => unreachable!(),
        };

        let mut operands = Vec::new();
        while !self.check(Token::RParen) {
            operands.push(self.parse_expression()?);
        }
        let end = self.expect(Token::RParen)?;

        Ok(Expression::Logical(LogicalExpr {
            op,
            operands,
            span: op_token.span.merge(end.span),
        }))
    }

    /// Parse a memory expression.
    fn parse_memory_expr(&mut self) -> Result<Expression> {
        let start = self.expect(Token::Mem)?;
        let base = Box::new(self.parse_expression()?);

        let offset = if self.check(Token::Plus) {
            self.advance()?;
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };

        let end = self.expect(Token::RParen)?;

        Ok(Expression::Memory(MemoryExpr {
            base,
            offset,
            span: start.span.merge(end.span),
        }))
    }

    /// Parse a capability invocation.
    fn parse_capability_invoke(&mut self) -> Result<Expression> {
        let start = self.expect(Token::Capability)?;
        let token = self.parse_identifier()?;

        let mut args = Vec::new();
        while !self.check(Token::RParen) {
            args.push(self.parse_expression()?);
        }

        let end = self.expect(Token::RParen)?;

        Ok(Expression::Capability(CapabilityInvoke {
            token,
            args,
            span: start.span.merge(end.span),
        }))
    }

    // Helper methods

    /// Check if at end of file.
    fn is_eof(&mut self) -> bool {
        self.lexer.is_eof()
    }

    /// Peek at the next token without consuming.
    fn peek_token(&mut self) -> Result<Token> {
        match self.lexer.peek() {
            Some(Ok(t)) => Ok(t.token.clone()),
            Some(Err(e)) => Err(e.clone()),
            None => Err(Error::UnexpectedEof {
                expected: "token".into(),
                span: Span::point(self.lexer.position()),
            }),
        }
    }

    /// Check if the next token matches.
    fn check(&mut self, expected: Token) -> bool {
        matches!(self.lexer.peek(), Some(Ok(t)) if std::mem::discriminant(&t.token) == std::mem::discriminant(&expected))
    }

    /// Advance and return the next token.
    fn advance(&mut self) -> Result<SpannedToken> {
        match self.lexer.next_token() {
            Some(Ok(t)) => Ok(t),
            Some(Err(e)) => Err(e),
            None => Err(Error::UnexpectedEof {
                expected: "token".into(),
                span: Span::point(self.lexer.position()),
            }),
        }
    }

    /// Expect a specific token.
    fn expect(&mut self, expected: Token) -> Result<SpannedToken> {
        let token = self.advance()?;
        if std::mem::discriminant(&token.token) == std::mem::discriminant(&expected) {
            Ok(token)
        } else {
            Err(Error::UnexpectedToken {
                expected: expected.to_string(),
                found: token.token.to_string(),
                span: token.span,
            })
        }
    }

    /// Get current span.
    fn current_span(&mut self) -> Span {
        match self.lexer.peek() {
            Some(Ok(t)) => t.span,
            _ => Span::point(self.lexer.position()),
        }
    }

    /// Parse an identifier.
    fn parse_identifier(&mut self) -> Result<Identifier> {
        let token = self.advance()?;
        match token.token {
            Token::Identifier(name) => Ok(Identifier::new(name, token.span)),
            _ => Err(Error::UnexpectedToken {
                expected: "identifier".into(),
                found: token.token.to_string(),
                span: token.span,
            }),
        }
    }

    /// Parse an integer.
    fn parse_integer(&mut self) -> Result<i64> {
        let token = self.advance()?;
        match token.token {
            Token::Integer(n) => Ok(n),
            _ => Err(Error::UnexpectedToken {
                expected: "integer".into(),
                found: token.token.to_string(),
                span: token.span,
            }),
        }
    }

    // Stub implementations for remaining parse methods
    // These would be fully implemented in a complete parser

    fn parse_module(&mut self, _start: Span) -> Result<ModuleDef> {
        todo!("parse_module")
    }

    fn parse_package(&mut self, _start: Span) -> Result<PackageDecl> {
        todo!("parse_package")
    }

    fn parse_macro(&mut self, _start: Span) -> Result<MacroDef> {
        todo!("parse_macro")
    }

    fn parse_global_data(&mut self, _start: Span) -> Result<GlobalDataDef> {
        todo!("parse_global_data")
    }

    fn parse_deployment(&mut self, _start: Span) -> Result<DeploymentSpec> {
        todo!("parse_deployment")
    }

    fn parse_compile_time(&mut self, _start: Span) -> Result<CompileTimeBlock> {
        todo!("parse_compile_time")
    }

    fn parse_conditional_assembly(&mut self, _start: Span) -> Result<ConditionalAssembly> {
        todo!("parse_conditional_assembly")
    }

    fn parse_assembly_directive(&mut self, _start: Span) -> Result<AssemblyDirective> {
        todo!("parse_assembly_directive")
    }

    fn parse_annotation(&mut self) -> Result<FunctionAnnotation> {
        todo!("parse_annotation")
    }

    fn peek_ahead_is_annotation(&mut self) -> Result<bool> {
        Ok(false) // Stub
    }

    fn parse_local_data(&mut self, _start: Span) -> Result<LocalDataDef> {
        todo!("parse_local_data")
    }

    fn parse_call(&mut self, _start: Span) -> Result<FunctionCall> {
        todo!("parse_call")
    }

    fn parse_while(&mut self, _start: Span) -> Result<WhileLoop> {
        todo!("parse_while")
    }

    fn parse_for(&mut self, _start: Span) -> Result<ForLoop> {
        todo!("parse_for")
    }

    fn parse_bounded_for(&mut self, _start: Span) -> Result<BoundedForLoop> {
        todo!("parse_bounded_for")
    }

    fn parse_transaction(&mut self, _start: Span) -> Result<Transaction> {
        todo!("parse_transaction")
    }

    fn parse_try_catch(&mut self, _start: Span) -> Result<TryCatch> {
        todo!("parse_try_catch")
    }

    fn parse_deploy_time(&mut self, _start: Span) -> Result<DeployTimeBlock> {
        todo!("parse_deploy_time")
    }

    fn parse_data_transfer(&mut self, _start: Span) -> Result<Instruction> {
        todo!("parse_data_transfer")
    }

    fn parse_arithmetic_instr(&mut self, _start: Span) -> Result<Instruction> {
        todo!("parse_arithmetic_instr")
    }

    fn parse_stack_op(&mut self, _start: Span) -> Result<Instruction> {
        todo!("parse_stack_op")
    }

    fn parse_memory_op(&mut self, _start: Span) -> Result<Instruction> {
        todo!("parse_memory_op")
    }

    fn parse_syscall(&mut self, _start: Span) -> Result<Instruction> {
        todo!("parse_syscall")
    }

    fn parse_checkpoint(&mut self, _start: Span) -> Result<Instruction> {
        todo!("parse_checkpoint")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_function() {
        // Note: "add" is a keyword, so we use "plus" as function name
        let source = "(defun plus (a b) (+ a b))";
        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer);

        let result = parser.parse_program();
        assert!(result.is_ok(), "Parse error: {:?}", result.err());

        let forms = result.unwrap();
        assert_eq!(forms.len(), 1);

        match &forms[0] {
            TopLevelForm::Function(f) => {
                assert_eq!(f.name.name, "plus");
                assert_eq!(f.params.len(), 2);
                assert_eq!(f.params[0].name.name, "a");
                assert_eq!(f.params[1].name.name, "b");
            }
            _ => panic!("Expected function"),
        }
    }
}
