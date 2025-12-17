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

    /// Parse a single expression (for REPL use)
    pub fn parse_repl_expression(&mut self) -> Result<Expression> {
        self.parse_expression()
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

    /// Parse a register.
    fn parse_register(&mut self) -> Result<Register> {
        let token = self.advance()?;
        match token.token {
            Token::Register(name) => Ok(Register {
                name,
                span: token.span,
            }),
            Token::Identifier(name) if is_register_name(&name) => Ok(Register {
                name,
                span: token.span,
            }),
            _ => Err(Error::UnexpectedToken {
                expected: "register".into(),
                found: token.token.to_string(),
                span: token.span,
            }),
        }
    }

    /// Parse an operand.
    fn parse_operand(&mut self) -> Result<Operand> {
        if self.check(Token::LParen) {
            let start = self.advance()?;
            if self.check(Token::Mem) {
                // Memory operand
                self.advance()?;
                let base = self.parse_register()?;
                let mut index = None;
                let mut scale = None;
                let mut displacement = None;

                while !self.check(Token::RParen) {
                    if self.check(Token::Plus) {
                        self.advance()?;
                        if let Ok(reg) = self.parse_register() {
                            index = Some(reg);
                            if self.check(Token::Star) {
                                self.advance()?;
                                scale = Some(self.parse_integer()? as u8);
                            }
                        } else {
                            displacement = Some(self.parse_integer()?);
                        }
                    } else {
                        break;
                    }
                }

                let end = self.expect(Token::RParen)?;
                Ok(Operand::Memory(MemoryOperand {
                    base,
                    index,
                    scale,
                    displacement,
                    span: start.span.merge(end.span),
                }))
            } else {
                // Expression operand in parens
                let expr = self.parse_expression_from_paren(start.span)?;
                Ok(Operand::Expression(expr))
            }
        } else if matches!(self.peek_token()?, Token::Register(_))
            || matches!(self.peek_token()?, Token::Identifier(ref s) if is_register_name(s))
        {
            Ok(Operand::Register(self.parse_register()?))
        } else {
            Ok(Operand::Expression(self.parse_literal_expression()?))
        }
    }

    // =========================================================================
    // Complete implementations for all parse methods
    // =========================================================================

    fn parse_module(&mut self, start: Span) -> Result<ModuleDef> {
        self.expect(Token::Module)?;
        let name = self.parse_identifier()?;

        let mut items = Vec::new();
        while !self.check(Token::RParen) {
            if self.check(Token::LParen) {
                let item_start = self.advance()?;
                let item = match self.peek_token()? {
                    Token::Use => ModuleItem::Import(self.parse_import(item_start.span)?),
                    Token::Export => ModuleItem::Export(self.parse_export(item_start.span)?),
                    _ => {
                        let form = self.parse_top_level_from_paren(item_start.span)?;
                        ModuleItem::TopLevel(Box::new(form))
                    }
                };
                items.push(item);
            } else {
                break;
            }
        }

        let end = self.expect(Token::RParen)?;

        Ok(ModuleDef {
            name,
            items,
            span: start.merge(end.span),
        })
    }

    fn parse_import(&mut self, start: Span) -> Result<ImportStmt> {
        self.expect(Token::Use)?;
        let module = self.parse_identifier()?;

        let items = if self.check(Token::LParen) {
            self.advance()?;
            let mut ids = Vec::new();
            while !self.check(Token::RParen) {
                ids.push(self.parse_identifier()?);
            }
            self.expect(Token::RParen)?;
            Some(ids)
        } else {
            None
        };

        let end = self.expect(Token::RParen)?;

        Ok(ImportStmt {
            module,
            items,
            span: start.merge(end.span),
        })
    }

    fn parse_export(&mut self, start: Span) -> Result<ExportStmt> {
        self.expect(Token::Export)?;

        let mut items = Vec::new();
        while !self.check(Token::RParen) {
            items.push(self.parse_identifier()?);
        }

        let end = self.expect(Token::RParen)?;

        Ok(ExportStmt {
            items,
            span: start.merge(end.span),
        })
    }

    fn parse_top_level_from_paren(&mut self, start: Span) -> Result<TopLevelForm> {
        match self.peek_token()? {
            Token::Module => Ok(TopLevelForm::Module(self.parse_module(start)?)),
            Token::Package => Ok(TopLevelForm::Package(self.parse_package(start)?)),
            Token::Defun => Ok(TopLevelForm::Function(self.parse_function(start)?)),
            Token::Defmacro => Ok(TopLevelForm::Macro(self.parse_macro(start)?)),
            Token::DefData => Ok(TopLevelForm::GlobalData(self.parse_global_data(start)?)),
            Token::Deployment => Ok(TopLevelForm::DeploymentSpec(self.parse_deployment(start)?)),
            Token::CompileTime => Ok(TopLevelForm::CompileTimeBlock(self.parse_compile_time(start)?)),
            Token::Ifdef => Ok(TopLevelForm::ConditionalAssembly(self.parse_conditional_assembly(start)?)),
            _ => Err(Error::UnexpectedToken {
                expected: "top-level form".into(),
                found: self.peek_token()?.to_string(),
                span: self.current_span(),
            }),
        }
    }

    fn parse_package(&mut self, start: Span) -> Result<PackageDecl> {
        self.expect(Token::Package)?;
        let name = self.parse_identifier()?;
        let version = self.parse_string()?;

        // Parse dependency list
        self.expect(Token::LParen)?;
        let mut dependencies = Vec::new();
        while !self.check(Token::RParen) {
            self.expect(Token::LParen)?;
            let dep_name = self.parse_identifier()?;
            let dep_version = self.parse_string()?;
            let constraint = if self.check(Token::LParen) {
                self.advance()?;
                let op = self.parse_comparison_op()?;
                let ver = self.parse_string()?;
                self.expect(Token::RParen)?;
                Some(VersionConstraint { op, version: ver })
            } else {
                None
            };
            self.expect(Token::RParen)?;
            dependencies.push(Dependency {
                name: dep_name,
                version: dep_version,
                constraint,
            });
        }
        self.expect(Token::RParen)?;

        // Optional manifest
        let manifest = if self.check(Token::LParen) {
            self.advance()?;
            if self.check(Token::Manifest) {
                Some(self.parse_manifest()?)
            } else {
                // Not a manifest, put paren back by parsing as something else
                None
            }
        } else {
            None
        };

        let end = self.expect(Token::RParen)?;

        Ok(PackageDecl {
            name,
            version,
            dependencies,
            manifest,
            span: start.merge(end.span),
        })
    }

    fn parse_string(&mut self) -> Result<String> {
        let token = self.advance()?;
        match token.token {
            Token::String(s) => Ok(s),
            _ => Err(Error::UnexpectedToken {
                expected: "string".into(),
                found: token.token.to_string(),
                span: token.span,
            }),
        }
    }

    fn parse_comparison_op(&mut self) -> Result<ComparisonOp> {
        let token = self.advance()?;
        match token.token {
            Token::Eq => Ok(ComparisonOp::Eq),
            Token::Neq => Ok(ComparisonOp::Neq),
            Token::Lt => Ok(ComparisonOp::Lt),
            Token::Gt => Ok(ComparisonOp::Gt),
            Token::Le => Ok(ComparisonOp::Le),
            Token::Ge => Ok(ComparisonOp::Ge),
            _ => Err(Error::UnexpectedToken {
                expected: "comparison operator".into(),
                found: token.token.to_string(),
                span: token.span,
            }),
        }
    }

    fn parse_manifest(&mut self) -> Result<DeploymentManifest> {
        self.expect(Token::Manifest)?;

        let mut capabilities = Vec::new();
        while !self.check(Token::RParen) {
            self.expect(Token::LParen)?;
            self.expect(Token::Grant)?;
            let token = self.parse_identifier()?;
            let budget = if self.check(Token::LParen) {
                self.advance()?;
                self.expect(Token::Budget)?;
                let b = self.parse_integer()? as u64;
                self.expect(Token::RParen)?;
                Some(b)
            } else {
                None
            };
            self.expect(Token::RParen)?;
            capabilities.push(CapabilityGrant { token, budget });
        }
        self.expect(Token::RParen)?;

        Ok(DeploymentManifest { capabilities })
    }

    fn parse_macro(&mut self, start: Span) -> Result<MacroDef> {
        self.expect(Token::Defmacro)?;
        let name = self.parse_identifier()?;
        let params = self.parse_parameter_list()?;

        let mut body = Vec::new();
        while !self.check(Token::RParen) {
            body.push(self.parse_statement()?);
        }

        let end = self.expect(Token::RParen)?;

        Ok(MacroDef {
            name,
            params,
            body,
            span: start.merge(end.span),
        })
    }

    fn parse_global_data(&mut self, start: Span) -> Result<GlobalDataDef> {
        self.expect(Token::DefData)?;
        let name = self.parse_identifier()?;
        self.expect(Token::Colon)?;
        let ty = self.parse_type()?;

        let init = if !self.check(Token::RParen) && !self.check(Token::LParen) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let deploy_visible = if self.check(Token::LParen) {
            let peek = self.lexer.peek();
            if matches!(peek, Some(Ok(t)) if matches!(t.token, Token::Identifier(ref s) if s == "deploy-visible")) {
                self.advance()?;
                self.advance()?; // deploy-visible
                let val = matches!(self.peek_token()?, Token::True);
                self.advance()?;
                self.expect(Token::RParen)?;
                val
            } else {
                false
            }
        } else {
            false
        };

        let end = self.expect(Token::RParen)?;

        Ok(GlobalDataDef {
            name,
            ty,
            init,
            deploy_visible,
            span: start.merge(end.span),
        })
    }

    fn parse_deployment(&mut self, start: Span) -> Result<DeploymentSpec> {
        self.expect(Token::Deployment)?;

        // Parse profile
        self.expect(Token::LParen)?;
        self.expect(Token::Profile)?;
        let profile = self.parse_target_profile()?;
        self.expect(Token::RParen)?;

        // Parse bounds
        self.expect(Token::LParen)?;
        self.expect(Token::Bounds)?;
        let bounds = self.parse_resource_bounds()?;
        self.expect(Token::RParen)?;

        // Parse obfuscation
        self.expect(Token::LParen)?;
        self.expect(Token::Obfuscate)?;
        let obfuscation = self.parse_obfuscation_level()?;
        self.expect(Token::RParen)?;

        // Optional constraints
        let constraints = if self.check(Token::LParen) {
            self.advance()?;
            if self.check(Token::Constraints) {
                Some(self.parse_security_constraints()?)
            } else {
                None
            }
        } else {
            None
        };

        let end = self.expect(Token::RParen)?;

        Ok(DeploymentSpec {
            profile,
            bounds,
            obfuscation,
            constraints,
            span: start.merge(end.span),
        })
    }

    fn parse_target_profile(&mut self) -> Result<TargetProfile> {
        let token = self.advance()?;
        match &token.token {
            Token::Identifier(s) => match s.as_str() {
                "edge-minimal" => Ok(TargetProfile::EdgeMinimal),
                "iot-secure" => Ok(TargetProfile::IotSecure),
                "embedded-hardened" => Ok(TargetProfile::EmbeddedHardened),
                "sensor-node" => Ok(TargetProfile::SensorNode),
                "actuator-minimal" => Ok(TargetProfile::ActuatorMinimal),
                _ => Err(Error::UnexpectedToken {
                    expected: "target profile".into(),
                    found: s.clone(),
                    span: token.span,
                }),
            },
            _ => Err(Error::UnexpectedToken {
                expected: "target profile".into(),
                found: token.token.to_string(),
                span: token.span,
            }),
        }
    }

    fn parse_resource_bounds(&mut self) -> Result<ResourceBounds> {
        let mut bounds = ResourceBounds::default();

        while self.check(Token::LParen) {
            self.advance()?;
            match self.peek_token()? {
                Token::MaxIterations => {
                    self.advance()?;
                    bounds.max_iterations = Some(self.parse_integer()? as u64);
                }
                Token::MaxStackDepth => {
                    self.advance()?;
                    bounds.max_stack_depth = Some(self.parse_integer()? as u64);
                }
                Token::MaxMemory => {
                    self.advance()?;
                    bounds.max_memory = Some(self.parse_integer()? as u64);
                }
                Token::MaxCallDepth => {
                    self.advance()?;
                    bounds.max_call_depth = Some(self.parse_integer()? as u64);
                }
                Token::MaxExecutionTime => {
                    self.advance()?;
                    bounds.max_execution_time = Some(self.parse_integer()? as u64);
                }
                _ => break,
            }
            self.expect(Token::RParen)?;
        }

        Ok(bounds)
    }

    fn parse_obfuscation_level(&mut self) -> Result<ObfuscationLevel> {
        let token = self.advance()?;
        match &token.token {
            Token::Identifier(s) => match s.as_str() {
                "none" => Ok(ObfuscationLevel::None),
                "minimal" => Ok(ObfuscationLevel::Minimal),
                "aggressive" => Ok(ObfuscationLevel::Aggressive),
                "paranoid" => Ok(ObfuscationLevel::Paranoid),
                _ => Err(Error::UnexpectedToken {
                    expected: "obfuscation level".into(),
                    found: s.clone(),
                    span: token.span,
                }),
            },
            _ => Err(Error::UnexpectedToken {
                expected: "obfuscation level".into(),
                found: token.token.to_string(),
                span: token.span,
            }),
        }
    }

    fn parse_security_constraints(&mut self) -> Result<SecurityConstraints> {
        self.expect(Token::Constraints)?;

        let mut constraints = SecurityConstraints::default();

        while self.check(Token::LParen) {
            self.advance()?;
            match self.peek_token()? {
                Token::Forbid => {
                    self.advance()?;
                    constraints.forbidden.push(self.parse_forbidden_feature()?);
                }
                Token::Require => {
                    self.advance()?;
                    constraints.required.push(self.parse_required_proof()?);
                }
                _ => break,
            }
            self.expect(Token::RParen)?;
        }

        self.expect(Token::RParen)?;

        Ok(constraints)
    }

    fn parse_forbidden_feature(&mut self) -> Result<ForbiddenFeature> {
        let token = self.advance()?;
        match &token.token {
            Token::Identifier(s) => match s.as_str() {
                "syscalls" => Ok(ForbiddenFeature::Syscalls),
                "recursion" => Ok(ForbiddenFeature::Recursion),
                "backward-jumps" => Ok(ForbiddenFeature::BackwardJumps),
                "dynamic-allocation" => Ok(ForbiddenFeature::DynamicAllocation),
                "function-pointers" => Ok(ForbiddenFeature::FunctionPointers),
                _ => Err(Error::UnexpectedToken {
                    expected: "forbidden feature".into(),
                    found: s.clone(),
                    span: token.span,
                }),
            },
            _ => Err(Error::UnexpectedToken {
                expected: "forbidden feature".into(),
                found: token.token.to_string(),
                span: token.span,
            }),
        }
    }

    fn parse_required_proof(&mut self) -> Result<RequiredProof> {
        let token = self.advance()?;
        match &token.token {
            Token::Identifier(s) => match s.as_str() {
                "termination" => Ok(RequiredProof::Termination),
                "bounded-memory" => Ok(RequiredProof::BoundedMemory),
                "no-side-effects" => Ok(RequiredProof::NoSideEffects),
                _ => Err(Error::UnexpectedToken {
                    expected: "required proof".into(),
                    found: s.clone(),
                    span: token.span,
                }),
            },
            _ => Err(Error::UnexpectedToken {
                expected: "required proof".into(),
                found: token.token.to_string(),
                span: token.span,
            }),
        }
    }

    fn parse_compile_time(&mut self, start: Span) -> Result<CompileTimeBlock> {
        self.expect(Token::CompileTime)?;

        let mut body = Vec::new();
        while !self.check(Token::RParen) {
            body.push(self.parse_statement()?);
        }

        let end = self.expect(Token::RParen)?;

        Ok(CompileTimeBlock {
            body,
            span: start.merge(end.span),
        })
    }

    fn parse_conditional_assembly(&mut self, start: Span) -> Result<ConditionalAssembly> {
        self.expect(Token::Ifdef)?;
        let condition = self.parse_identifier()?;

        let mut then_branch = Vec::new();
        while !self.check(Token::RParen) && !self.check(Token::LParen) {
            then_branch.push(self.parse_statement()?);
        }

        let else_branch = if self.check(Token::LParen) {
            self.advance()?;
            if self.check(Token::Else) {
                self.advance()?;
                let mut stmts = Vec::new();
                while !self.check(Token::RParen) {
                    stmts.push(self.parse_statement()?);
                }
                self.expect(Token::RParen)?;
                Some(stmts)
            } else {
                // Not else clause, parse as statement
                then_branch.push(self.parse_statement()?);
                None
            }
        } else {
            None
        };

        let end = self.expect(Token::RParen)?;

        Ok(ConditionalAssembly {
            condition,
            then_branch,
            else_branch,
            span: start.merge(end.span),
        })
    }

    fn parse_assembly_directive(&mut self, start: Span) -> Result<AssemblyDirective> {
        let directive = match self.peek_token()? {
            Token::Target => {
                self.advance()?;
                DirectiveType::Target
            }
            Token::Section => {
                self.advance()?;
                DirectiveType::Section
            }
            Token::Global => {
                self.advance()?;
                DirectiveType::Global
            }
            Token::Extern => {
                self.advance()?;
                DirectiveType::Extern
            }
            _ => {
                return Err(Error::UnexpectedToken {
                    expected: "assembly directive".into(),
                    found: self.peek_token()?.to_string(),
                    span: self.current_span(),
                });
            }
        };

        let arg = self.parse_identifier()?;
        let end = self.expect(Token::RParen)?;

        Ok(AssemblyDirective {
            directive,
            arg,
            span: start.merge(end.span),
        })
    }

    fn parse_annotation(&mut self) -> Result<FunctionAnnotation> {
        self.expect(Token::LParen)?;

        let annotation = match self.peek_token()? {
            Token::ProveTerminates => {
                self.advance()?;
                let strategy = if self.check(Token::LParen) {
                    self.advance()?;
                    self.expect(Token::Strategy)?;
                    let strat = self.parse_termination_strategy()?;
                    self.expect(Token::RParen)?;
                    strat
                } else {
                    TerminationStrategy::BoundedLoop
                };
                FunctionAnnotation::TerminationProof(strategy)
            }
            Token::Complexity => {
                self.advance()?;
                let expr = self.parse_complexity_expr()?;
                FunctionAnnotation::Complexity(expr)
            }
            Token::Pure => {
                self.advance()?;
                FunctionAnnotation::Pure
            }
            Token::Idempotent => {
                self.advance()?;
                FunctionAnnotation::Idempotent
            }
            Token::DeployTarget => {
                self.advance()?;
                let target = self.parse_deploy_target()?;
                FunctionAnnotation::DeployTarget(target)
            }
            _ => {
                return Err(Error::UnexpectedToken {
                    expected: "function annotation".into(),
                    found: self.peek_token()?.to_string(),
                    span: self.current_span(),
                });
            }
        };

        self.expect(Token::RParen)?;
        Ok(annotation)
    }

    fn parse_termination_strategy(&mut self) -> Result<TerminationStrategy> {
        let token = self.advance()?;
        match &token.token {
            Token::Identifier(s) => match s.as_str() {
                "bounded-loop" => Ok(TerminationStrategy::BoundedLoop),
                "structural-recursion" => Ok(TerminationStrategy::StructuralRecursion),
                "well-founded-order" => Ok(TerminationStrategy::WellFoundedOrder),
                _ => Err(Error::UnexpectedToken {
                    expected: "termination strategy".into(),
                    found: s.clone(),
                    span: token.span,
                }),
            },
            _ => Err(Error::UnexpectedToken {
                expected: "termination strategy".into(),
                found: token.token.to_string(),
                span: token.span,
            }),
        }
    }

    fn parse_complexity_expr(&mut self) -> Result<ComplexityExpr> {
        let token = self.advance()?;
        match &token.token {
            Token::Identifier(s) => match s.as_str() {
                "O(1)" => Ok(ComplexityExpr::Constant),
                "O(n)" => Ok(ComplexityExpr::Linear),
                "O(log(n))" => Ok(ComplexityExpr::Logarithmic),
                "O(n*log(n))" => Ok(ComplexityExpr::Linearithmic),
                s if s.starts_with("O(n^") => {
                    let exp = s.trim_start_matches("O(n^").trim_end_matches(')');
                    Ok(ComplexityExpr::Polynomial(exp.parse().unwrap_or(2)))
                }
                _ => Err(Error::UnexpectedToken {
                    expected: "complexity expression".into(),
                    found: s.clone(),
                    span: token.span,
                }),
            },
            _ => Err(Error::UnexpectedToken {
                expected: "complexity expression".into(),
                found: token.token.to_string(),
                span: token.span,
            }),
        }
    }

    fn parse_deploy_target(&mut self) -> Result<DeployTarget> {
        let token = self.advance()?;
        match &token.token {
            Token::Identifier(s) => match s.as_str() {
                "runtime-only" => Ok(DeployTarget::RuntimeOnly),
                "compile-only" => Ok(DeployTarget::CompileOnly),
                "both" => Ok(DeployTarget::Both),
                _ => Err(Error::UnexpectedToken {
                    expected: "deploy target".into(),
                    found: s.clone(),
                    span: token.span,
                }),
            },
            _ => Err(Error::UnexpectedToken {
                expected: "deploy target".into(),
                found: token.token.to_string(),
                span: token.span,
            }),
        }
    }

    fn peek_ahead_is_annotation(&mut self) -> Result<bool> {
        // Check if the next form is an annotation
        if !self.check(Token::LParen) {
            return Ok(false);
        }
        // We'd need lookahead here; for simplicity, check known annotation keywords
        Ok(false) // Conservative: let parse_annotation handle it
    }

    fn parse_local_data(&mut self, start: Span) -> Result<LocalDataDef> {
        self.expect(Token::Let)?;
        let name = self.parse_identifier()?;

        let ty = if self.check(Token::Colon) {
            self.advance()?;
            Some(self.parse_type()?)
        } else {
            None
        };

        let init = if !self.check(Token::RParen) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let end = self.expect(Token::RParen)?;

        Ok(LocalDataDef {
            name,
            ty,
            init,
            span: start.merge(end.span),
        })
    }

    fn parse_call(&mut self, start: Span) -> Result<FunctionCall> {
        self.expect(Token::Call)?;
        let name = self.parse_identifier()?;

        self.expect(Token::LParen)?;
        let mut args = Vec::new();
        while !self.check(Token::RParen) {
            args.push(self.parse_expression()?);
        }
        self.expect(Token::RParen)?;

        let end = self.expect(Token::RParen)?;

        Ok(FunctionCall {
            name,
            args,
            span: start.merge(end.span),
        })
    }

    fn parse_while(&mut self, start: Span) -> Result<WhileLoop> {
        self.expect(Token::While)?;
        let condition = self.parse_expression()?;

        let mut body = Vec::new();
        while !self.check(Token::RParen) {
            body.push(self.parse_statement()?);
        }

        let end = self.expect(Token::RParen)?;

        Ok(WhileLoop {
            condition,
            body,
            span: start.merge(end.span),
        })
    }

    fn parse_for(&mut self, start: Span) -> Result<ForLoop> {
        self.expect(Token::For)?;
        let var = self.parse_identifier()?;
        let iter = self.parse_expression()?;

        let mut body = Vec::new();
        while !self.check(Token::RParen) {
            body.push(self.parse_statement()?);
        }

        let end = self.expect(Token::RParen)?;

        Ok(ForLoop {
            var,
            iter,
            body,
            span: start.merge(end.span),
        })
    }

    fn parse_bounded_for(&mut self, start: Span) -> Result<BoundedForLoop> {
        self.expect(Token::BoundedFor)?;
        let var = self.parse_identifier()?;
        let start_expr = self.parse_expression()?;
        let end_expr = self.parse_expression()?;

        let mut body = Vec::new();
        while !self.check(Token::RParen) {
            body.push(self.parse_statement()?);
        }

        let end = self.expect(Token::RParen)?;

        Ok(BoundedForLoop {
            var,
            start: start_expr,
            end: end_expr,
            body,
            span: start.merge(end.span),
        })
    }

    fn parse_transaction(&mut self, start: Span) -> Result<Transaction> {
        self.expect(Token::Transaction)?;

        let mut body = Vec::new();
        let mut checkpoint = None;

        while !self.check(Token::RParen) {
            if self.check(Token::LParen) {
                let stmt_start = self.advance()?;
                match self.peek_token()? {
                    Token::Commit => {
                        self.advance()?;
                        checkpoint = Some(CheckpointOp::Commit);
                        self.expect(Token::RParen)?;
                    }
                    Token::Rollback => {
                        self.advance()?;
                        checkpoint = Some(CheckpointOp::Rollback);
                        self.expect(Token::RParen)?;
                    }
                    Token::Fail => {
                        self.advance()?;
                        checkpoint = Some(CheckpointOp::Fail);
                        self.expect(Token::RParen)?;
                    }
                    _ => {
                        body.push(self.parse_statement_from_paren(stmt_start.span)?);
                    }
                }
            } else {
                body.push(self.parse_statement()?);
            }
        }

        let end = self.expect(Token::RParen)?;

        Ok(Transaction {
            body,
            checkpoint,
            span: start.merge(end.span),
        })
    }

    fn parse_statement_from_paren(&mut self, start: Span) -> Result<Statement> {
        match self.peek_token()? {
            Token::Let => Ok(Statement::LocalData(self.parse_local_data(start)?)),
            Token::Call => Ok(Statement::Call(self.parse_call(start)?)),
            Token::While => Ok(Statement::Loop(LoopStmt::While(self.parse_while(start)?))),
            Token::For => Ok(Statement::Loop(LoopStmt::For(self.parse_for(start)?))),
            Token::BoundedFor => Ok(Statement::Loop(LoopStmt::BoundedFor(self.parse_bounded_for(start)?))),
            _ => Ok(Statement::Expression(self.parse_expression_from_paren(start)?)),
        }
    }

    fn parse_try_catch(&mut self, start: Span) -> Result<TryCatch> {
        self.expect(Token::Try)?;

        let mut try_body = Vec::new();
        while !self.check(Token::RParen) && !self.check(Token::LParen) {
            try_body.push(self.parse_statement()?);
        }

        // If next is paren, check for catch/finally
        let mut catches = Vec::new();
        let mut finally = None;

        while self.check(Token::LParen) {
            self.advance()?;
            match self.peek_token()? {
                Token::Catch => {
                    self.advance()?;
                    let pattern = if !matches!(self.peek_token()?, Token::Identifier(_)) {
                        None
                    } else {
                        let tok = self.advance()?;
                        match tok.token {
                            Token::Identifier(s) => Some(ExceptionPattern::Identifier(
                                Identifier::new(s, tok.span),
                            )),
                            Token::String(s) => Some(ExceptionPattern::String(s)),
                            _ => None,
                        }
                    };
                    let binding = self.parse_identifier()?;
                    let mut body = Vec::new();
                    while !self.check(Token::RParen) {
                        body.push(self.parse_statement()?);
                    }
                    self.expect(Token::RParen)?;
                    catches.push(CatchClause {
                        pattern,
                        binding,
                        body,
                        span: start,
                    });
                }
                Token::Finally => {
                    self.advance()?;
                    let mut body = Vec::new();
                    while !self.check(Token::RParen) {
                        body.push(self.parse_statement()?);
                    }
                    self.expect(Token::RParen)?;
                    finally = Some(body);
                }
                _ => {
                    try_body.push(self.parse_statement()?);
                }
            }
        }

        let end = self.expect(Token::RParen)?;

        Ok(TryCatch {
            try_body,
            catches,
            finally,
            span: start.merge(end.span),
        })
    }

    fn parse_deploy_time(&mut self, start: Span) -> Result<DeployTimeBlock> {
        self.expect(Token::DeployTime)?;

        let mut body = Vec::new();
        while !self.check(Token::RParen) {
            body.push(self.parse_statement()?);
        }

        let end = self.expect(Token::RParen)?;

        Ok(DeployTimeBlock {
            body,
            span: start.merge(end.span),
        })
    }

    fn parse_data_transfer(&mut self, start: Span) -> Result<Instruction> {
        let op_type = match self.peek_token()? {
            Token::Mov => {
                self.advance()?;
                DataTransferType::Mov
            }
            Token::Lea => {
                self.advance()?;
                DataTransferType::Lea
            }
            _ => {
                return Err(Error::UnexpectedToken {
                    expected: "mov or lea".into(),
                    found: self.peek_token()?.to_string(),
                    span: self.current_span(),
                });
            }
        };

        let dest = self.parse_register()?;
        let src = self.parse_operand()?;
        let end = self.expect(Token::RParen)?;

        Ok(Instruction::DataTransfer(DataTransferOp {
            op: op_type,
            dest,
            src,
            span: start.merge(end.span),
        }))
    }

    fn parse_arithmetic_instr(&mut self, start: Span) -> Result<Instruction> {
        let op = match self.peek_token()? {
            Token::Add => {
                self.advance()?;
                ArithmeticOp::Add
            }
            Token::Sub => {
                self.advance()?;
                ArithmeticOp::Sub
            }
            Token::Mul => {
                self.advance()?;
                ArithmeticOp::Mul
            }
            Token::Div => {
                self.advance()?;
                ArithmeticOp::Div
            }
            Token::Inc => {
                self.advance()?;
                ArithmeticOp::Inc
            }
            Token::Dec => {
                self.advance()?;
                ArithmeticOp::Dec
            }
            _ => {
                return Err(Error::UnexpectedToken {
                    expected: "arithmetic instruction".into(),
                    found: self.peek_token()?.to_string(),
                    span: self.current_span(),
                });
            }
        };

        let dest = self.parse_register()?;
        let src = self.parse_operand()?;
        let end = self.expect(Token::RParen)?;

        Ok(Instruction::Arithmetic(ArithmeticInstr {
            op,
            dest,
            src,
            span: start.merge(end.span),
        }))
    }

    fn parse_stack_op(&mut self, start: Span) -> Result<Instruction> {
        let op = match self.peek_token()? {
            Token::Push => {
                self.advance()?;
                StackOpType::Push
            }
            Token::Pop => {
                self.advance()?;
                StackOpType::Pop
            }
            _ => {
                return Err(Error::UnexpectedToken {
                    expected: "push or pop".into(),
                    found: self.peek_token()?.to_string(),
                    span: self.current_span(),
                });
            }
        };

        let operand = self.parse_operand()?;
        let end = self.expect(Token::RParen)?;

        Ok(Instruction::Stack(StackOp {
            op,
            operand,
            span: start.merge(end.span),
        }))
    }

    fn parse_memory_op(&mut self, start: Span) -> Result<Instruction> {
        let op = match self.peek_token()? {
            Token::Alloc => {
                self.advance()?;
                MemoryOpType::Alloc
            }
            Token::Free => {
                self.advance()?;
                MemoryOpType::Free
            }
            _ => {
                return Err(Error::UnexpectedToken {
                    expected: "alloc or free".into(),
                    found: self.peek_token()?.to_string(),
                    span: self.current_span(),
                });
            }
        };

        let reg = self.parse_register()?;
        let size = if !self.check(Token::RParen) {
            Some(self.parse_integer()? as u64)
        } else {
            None
        };
        let end = self.expect(Token::RParen)?;

        Ok(Instruction::Memory(MemoryOp {
            op,
            reg,
            size,
            span: start.merge(end.span),
        }))
    }

    fn parse_syscall(&mut self, start: Span) -> Result<Instruction> {
        self.expect(Token::Syscall)?;

        let operand = if !self.check(Token::RParen) {
            Some(self.parse_operand()?)
        } else {
            None
        };

        let end = self.expect(Token::RParen)?;

        Ok(Instruction::System(SystemOp {
            operand,
            span: start.merge(end.span),
        }))
    }

    fn parse_checkpoint(&mut self, start: Span) -> Result<Instruction> {
        let op = match self.peek_token()? {
            Token::Commit => {
                self.advance()?;
                CheckpointOp::Commit
            }
            Token::Rollback => {
                self.advance()?;
                CheckpointOp::Rollback
            }
            Token::Fail => {
                self.advance()?;
                CheckpointOp::Fail
            }
            _ => {
                return Err(Error::UnexpectedToken {
                    expected: "commit, rollback, or fail".into(),
                    found: self.peek_token()?.to_string(),
                    span: self.current_span(),
                });
            }
        };

        let end = self.expect(Token::RParen)?;
        let _ = end; // Span not stored in CheckpointOp

        Ok(Instruction::Checkpoint(op))
    }
}

/// Check if a name is a register.
fn is_register_name(name: &str) -> bool {
    matches!(
        name,
        "rax" | "rbx" | "rcx" | "rdx" | "rsi" | "rdi" | "rsp" | "rbp"
            | "r8" | "r9" | "r10" | "r11" | "r12" | "r13" | "r14" | "r15"
    )
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
