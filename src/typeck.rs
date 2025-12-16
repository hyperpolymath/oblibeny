// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Type checking for Oblíbený.
//!
//! This module implements type inference and checking for Oblíbený programs.
//! In deploy-time code, all types must be fully annotated.
//! In compile-time code, type inference is permitted.

use crate::ast::*;
use crate::error::{Error, Result};
use crate::span::Span;
use std::collections::HashMap;

/// Type environment mapping variable names to types.
#[derive(Debug, Clone, Default)]
pub struct TypeEnv {
    /// Scopes stack (innermost first).
    scopes: Vec<HashMap<String, Type>>,
    /// Function signatures.
    functions: HashMap<String, FunctionSig>,
    /// Whether we're in deploy-time context (requires strict typing).
    in_deploy_time: bool,
}

/// Internal type representation.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// Unknown type (for inference).
    Unknown,
    /// Unit type (void).
    Unit,
    /// Primitive type.
    Primitive(PrimitiveType),
    /// Array type.
    Array { elem: Box<Type>, size: u64 },
    /// Pointer type.
    Pointer(Box<Type>),
    /// Struct type.
    Struct(Vec<(String, Type)>),
    /// Enum type.
    Enum(Vec<String>),
    /// Function type.
    Function(Box<FunctionSig>),
    /// User-defined type.
    User(String),
    /// Error type (for error recovery).
    Error,
}

/// Function signature.
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSig {
    /// Parameter types.
    pub params: Vec<Type>,
    /// Return type.
    pub return_type: Type,
    /// Whether the function is pure.
    pub is_pure: bool,
}

/// Type checker state.
pub struct TypeChecker {
    /// Type environment.
    env: TypeEnv,
    /// Collected errors.
    errors: Vec<Error>,
}

impl TypeChecker {
    /// Create a new type checker.
    pub fn new() -> Self {
        let mut checker = Self {
            env: TypeEnv::default(),
            errors: Vec::new(),
        };
        checker.env.push_scope();
        checker.register_builtins();
        checker
    }

    /// Register built-in functions.
    fn register_builtins(&mut self) {
        // Arithmetic operators
        for op in &["+", "-", "*", "/", "%"] {
            self.env.functions.insert(
                op.to_string(),
                FunctionSig {
                    params: vec![Type::Primitive(PrimitiveType::I64), Type::Primitive(PrimitiveType::I64)],
                    return_type: Type::Primitive(PrimitiveType::I64),
                    is_pure: true,
                },
            );
        }

        // Comparison operators
        for op in &["<", ">", "<=", ">=", "=", "!="] {
            self.env.functions.insert(
                op.to_string(),
                FunctionSig {
                    params: vec![Type::Primitive(PrimitiveType::I64), Type::Primitive(PrimitiveType::I64)],
                    return_type: Type::Primitive(PrimitiveType::Bool),
                    is_pure: true,
                },
            );
        }

        // Logical operators
        self.env.functions.insert(
            "and".to_string(),
            FunctionSig {
                params: vec![Type::Primitive(PrimitiveType::Bool), Type::Primitive(PrimitiveType::Bool)],
                return_type: Type::Primitive(PrimitiveType::Bool),
                is_pure: true,
            },
        );
        self.env.functions.insert(
            "or".to_string(),
            FunctionSig {
                params: vec![Type::Primitive(PrimitiveType::Bool), Type::Primitive(PrimitiveType::Bool)],
                return_type: Type::Primitive(PrimitiveType::Bool),
                is_pure: true,
            },
        );
        self.env.functions.insert(
            "not".to_string(),
            FunctionSig {
                params: vec![Type::Primitive(PrimitiveType::Bool)],
                return_type: Type::Primitive(PrimitiveType::Bool),
                is_pure: true,
            },
        );
    }

    /// Check a program.
    pub fn check_program(&mut self, program: &[TopLevelForm]) -> Result<()> {
        // First pass: collect function signatures
        for form in program {
            self.collect_signatures(form);
        }

        // Second pass: type check bodies
        for form in program {
            self.check_top_level(form);
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            // Return first error (could be changed to return all)
            Err(self.errors.remove(0))
        }
    }

    /// Collect function signatures from top-level forms.
    fn collect_signatures(&mut self, form: &TopLevelForm) {
        match form {
            TopLevelForm::Function(f) => {
                let params: Vec<Type> = f
                    .params
                    .iter()
                    .map(|p| p.ty.as_ref().map(|t| self.type_from_expr(t)).unwrap_or(Type::Unknown))
                    .collect();
                let return_type = f
                    .return_type
                    .as_ref()
                    .map(|t| self.type_from_expr(t))
                    .unwrap_or(Type::Unit);
                let is_pure = f.annotations.iter().any(|a| matches!(a, FunctionAnnotation::Pure));

                self.env.functions.insert(
                    f.name.name.clone(),
                    FunctionSig {
                        params,
                        return_type,
                        is_pure,
                    },
                );
            }
            TopLevelForm::Macro(m) => {
                // Macros are compile-time only, less strict typing
                let params = vec![Type::Unknown; m.params.len()];
                self.env.functions.insert(
                    m.name.name.clone(),
                    FunctionSig {
                        params,
                        return_type: Type::Unknown,
                        is_pure: false,
                    },
                );
            }
            TopLevelForm::Module(module) => {
                for item in &module.items {
                    if let ModuleItem::TopLevel(inner) = item {
                        self.collect_signatures(inner);
                    }
                }
            }
            _ => {}
        }
    }

    /// Type check a top-level form.
    fn check_top_level(&mut self, form: &TopLevelForm) {
        match form {
            TopLevelForm::Function(f) => {
                self.check_function(f);
            }
            TopLevelForm::Macro(m) => {
                self.check_macro(m);
            }
            TopLevelForm::Module(module) => {
                for item in &module.items {
                    if let ModuleItem::TopLevel(inner) = item {
                        self.check_top_level(inner);
                    }
                }
            }
            TopLevelForm::GlobalData(g) => {
                self.check_global_data(g);
            }
            TopLevelForm::CompileTimeBlock(block) => {
                let was_deploy_time = self.env.in_deploy_time;
                self.env.in_deploy_time = false;
                for stmt in &block.body {
                    self.check_statement(stmt);
                }
                self.env.in_deploy_time = was_deploy_time;
            }
            TopLevelForm::DeploymentSpec(spec) => {
                self.check_deployment_spec(spec);
            }
            _ => {}
        }
    }

    /// Type check a function definition.
    fn check_function(&mut self, f: &FunctionDef) {
        self.env.push_scope();

        // Determine if function is deploy-time
        let is_deploy_time = f.annotations.iter().any(|a| {
            matches!(
                a,
                FunctionAnnotation::DeployTarget(DeployTarget::RuntimeOnly)
                    | FunctionAnnotation::DeployTarget(DeployTarget::Both)
            )
        });
        let was_deploy_time = self.env.in_deploy_time;
        self.env.in_deploy_time = is_deploy_time;

        // Add parameters to scope
        for param in &f.params {
            let ty = param
                .ty
                .as_ref()
                .map(|t| self.type_from_expr(t))
                .unwrap_or(Type::Unknown);

            // In deploy-time, parameters must be typed
            if is_deploy_time && matches!(ty, Type::Unknown) {
                self.errors.push(Error::TypeMismatch {
                    expected: "explicit type annotation".to_string(),
                    actual: "untyped parameter".to_string(),
                    span: param.span,
                });
            }

            self.env.define(&param.name.name, ty);
        }

        // Check function body
        for stmt in &f.body {
            self.check_statement(stmt);
        }

        self.env.in_deploy_time = was_deploy_time;
        self.env.pop_scope();
    }

    /// Type check a macro definition.
    fn check_macro(&mut self, m: &MacroDef) {
        self.env.push_scope();

        // Macros are compile-time only
        let was_deploy_time = self.env.in_deploy_time;
        self.env.in_deploy_time = false;

        // Add parameters to scope (untyped is OK for macros)
        for param in &m.params {
            self.env.define(&param.name.name, Type::Unknown);
        }

        // Check macro body
        for stmt in &m.body {
            self.check_statement(stmt);
        }

        self.env.in_deploy_time = was_deploy_time;
        self.env.pop_scope();
    }

    /// Type check global data.
    fn check_global_data(&mut self, g: &GlobalDataDef) {
        let declared_type = self.type_from_expr(&g.ty);

        if let Some(init) = &g.init {
            let init_type = self.infer_expr(init);
            if !self.types_compatible(&declared_type, &init_type) {
                self.errors.push(Error::TypeMismatch {
                    expected: format!("{:?}", declared_type),
                    actual: format!("{:?}", init_type),
                    span: g.span,
                });
            }
        }

        self.env.define(&g.name.name, declared_type);
    }

    /// Type check deployment specification.
    fn check_deployment_spec(&mut self, _spec: &DeploymentSpec) {
        // Deployment specs are validated for consistency
        // (resource bounds, profile compatibility, etc.)
    }

    /// Type check a statement.
    fn check_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Expression(expr) => {
                self.infer_expr(expr);
            }
            Statement::LocalData(local) => {
                let ty = if let Some(ty_expr) = &local.ty {
                    self.type_from_expr(ty_expr)
                } else if let Some(init) = &local.init {
                    self.infer_expr(init)
                } else {
                    if self.env.in_deploy_time {
                        self.errors.push(Error::TypeMismatch {
                            expected: "type annotation or initializer".to_string(),
                            actual: "neither".to_string(),
                            span: local.span,
                        });
                    }
                    Type::Unknown
                };

                // Check initializer matches declared type
                if let (Some(ty_expr), Some(init)) = (&local.ty, &local.init) {
                    let declared = self.type_from_expr(ty_expr);
                    let init_ty = self.infer_expr(init);
                    if !self.types_compatible(&declared, &init_ty) {
                        self.errors.push(Error::TypeMismatch {
                            expected: format!("{:?}", declared),
                            actual: format!("{:?}", init_ty),
                            span: local.span,
                        });
                    }
                }

                self.env.define(&local.name.name, ty);
            }
            Statement::Call(call) => {
                self.check_call(call);
            }
            Statement::Transaction(tx) => {
                if self.env.in_deploy_time {
                    self.errors.push(Error::PhaseViolation {
                        construct: "transaction".to_string(),
                        span: tx.span,
                    });
                }
                for stmt in &tx.body {
                    self.check_statement(stmt);
                }
            }
            Statement::MetamorphicIf(mif) => {
                if self.env.in_deploy_time {
                    self.errors.push(Error::PhaseViolation {
                        construct: "metamorphic-if".to_string(),
                        span: mif.span,
                    });
                }
                let cond_ty = self.infer_expr(&mif.condition);
                if !matches!(cond_ty, Type::Primitive(PrimitiveType::Bool) | Type::Unknown) {
                    self.errors.push(Error::TypeMismatch {
                        expected: "bool".to_string(),
                        actual: format!("{:?}", cond_ty),
                        span: mif.span,
                    });
                }
                for stmt in &mif.then_branch {
                    self.check_statement(stmt);
                }
                if let Some(else_branch) = &mif.else_branch {
                    for stmt in else_branch {
                        self.check_statement(stmt);
                    }
                }
            }
            Statement::TryCatch(tc) => {
                if self.env.in_deploy_time {
                    self.errors.push(Error::PhaseViolation {
                        construct: "try-catch".to_string(),
                        span: tc.span,
                    });
                }
                for stmt in &tc.try_body {
                    self.check_statement(stmt);
                }
                for catch in &tc.catches {
                    self.env.push_scope();
                    self.env.define(&catch.binding.name, Type::Unknown);
                    for stmt in &catch.body {
                        self.check_statement(stmt);
                    }
                    self.env.pop_scope();
                }
                if let Some(finally) = &tc.finally {
                    for stmt in finally {
                        self.check_statement(stmt);
                    }
                }
            }
            Statement::Loop(loop_stmt) => {
                self.check_loop(loop_stmt);
            }
            Statement::ConditionalAssembly(ca) => {
                for stmt in &ca.then_branch {
                    self.check_statement(stmt);
                }
                if let Some(else_branch) = &ca.else_branch {
                    for stmt in else_branch {
                        self.check_statement(stmt);
                    }
                }
            }
            Statement::DeployTimeBlock(block) => {
                let was_deploy_time = self.env.in_deploy_time;
                self.env.in_deploy_time = true;
                for stmt in &block.body {
                    self.check_statement(stmt);
                }
                self.env.in_deploy_time = was_deploy_time;
            }
            _ => {}
        }
    }

    /// Type check a loop statement.
    fn check_loop(&mut self, loop_stmt: &LoopStmt) {
        match loop_stmt {
            LoopStmt::While(w) => {
                if self.env.in_deploy_time {
                    self.errors.push(Error::PhaseViolation {
                        construct: "while".to_string(),
                        span: w.span,
                    });
                }
                let cond_ty = self.infer_expr(&w.condition);
                if !matches!(cond_ty, Type::Primitive(PrimitiveType::Bool) | Type::Unknown) {
                    self.errors.push(Error::TypeMismatch {
                        expected: "bool".to_string(),
                        actual: format!("{:?}", cond_ty),
                        span: w.span,
                    });
                }
                self.env.push_scope();
                for stmt in &w.body {
                    self.check_statement(stmt);
                }
                self.env.pop_scope();
            }
            LoopStmt::For(f) => {
                if self.env.in_deploy_time {
                    self.errors.push(Error::PhaseViolation {
                        construct: "for".to_string(),
                        span: f.span,
                    });
                }
                self.env.push_scope();
                self.env.define(&f.var.name, Type::Unknown);
                for stmt in &f.body {
                    self.check_statement(stmt);
                }
                self.env.pop_scope();
            }
            LoopStmt::BoundedFor(bf) => {
                // Bounded-for is allowed in deploy-time
                let start_ty = self.infer_expr(&bf.start);
                let end_ty = self.infer_expr(&bf.end);

                // Both should be integers
                if !self.is_integer_type(&start_ty) && !matches!(start_ty, Type::Unknown) {
                    self.errors.push(Error::TypeMismatch {
                        expected: "integer".to_string(),
                        actual: format!("{:?}", start_ty),
                        span: bf.span,
                    });
                }
                if !self.is_integer_type(&end_ty) && !matches!(end_ty, Type::Unknown) {
                    self.errors.push(Error::TypeMismatch {
                        expected: "integer".to_string(),
                        actual: format!("{:?}", end_ty),
                        span: bf.span,
                    });
                }

                self.env.push_scope();
                self.env.define(&bf.var.name, Type::Primitive(PrimitiveType::I64));
                for stmt in &bf.body {
                    self.check_statement(stmt);
                }
                self.env.pop_scope();
            }
        }
    }

    /// Type check a function call.
    fn check_call(&mut self, call: &FunctionCall) -> Type {
        let sig = self.env.functions.get(&call.name.name).cloned();

        if let Some(sig) = sig {
            // Check argument count
            if call.args.len() != sig.params.len() {
                self.errors.push(Error::TypeMismatch {
                    expected: format!("{} arguments", sig.params.len()),
                    actual: format!("{} arguments", call.args.len()),
                    span: call.span,
                });
            }

            // Check argument types
            for (arg, param_ty) in call.args.iter().zip(sig.params.iter()) {
                let arg_ty = self.infer_expr(arg);
                if !self.types_compatible(param_ty, &arg_ty) {
                    self.errors.push(Error::TypeMismatch {
                        expected: format!("{:?}", param_ty),
                        actual: format!("{:?}", arg_ty),
                        span: call.span,
                    });
                }
            }

            sig.return_type
        } else {
            // Unknown function - might be external
            Type::Unknown
        }
    }

    /// Infer the type of an expression.
    fn infer_expr(&mut self, expr: &Expression) -> Type {
        match expr {
            Expression::Literal(lit) => self.infer_literal(lit),
            Expression::Variable(var) => {
                if let Some(ty) = self.env.lookup(&var.name) {
                    ty.clone()
                } else {
                    self.errors.push(Error::UndefinedVariable {
                        name: var.name.clone(),
                        span: var.span,
                    });
                    Type::Error
                }
            }
            Expression::Call(call) => self.check_call(call),
            Expression::Arithmetic(arith) => {
                let left = self.infer_expr(&arith.left);
                let right = self.infer_expr(&arith.right);

                if self.is_numeric_type(&left) && self.is_numeric_type(&right) {
                    // Return the "wider" type
                    self.wider_numeric_type(&left, &right)
                } else if matches!(left, Type::Unknown) || matches!(right, Type::Unknown) {
                    Type::Unknown
                } else {
                    self.errors.push(Error::TypeMismatch {
                        expected: "numeric types".to_string(),
                        actual: format!("{:?} and {:?}", left, right),
                        span: arith.span,
                    });
                    Type::Error
                }
            }
            Expression::Comparison(cmp) => {
                let left = self.infer_expr(&cmp.left);
                let right = self.infer_expr(&cmp.right);

                if !self.types_compatible(&left, &right) {
                    self.errors.push(Error::TypeMismatch {
                        expected: format!("{:?}", left),
                        actual: format!("{:?}", right),
                        span: cmp.span,
                    });
                }

                Type::Primitive(PrimitiveType::Bool)
            }
            Expression::Logical(log) => {
                for operand in &log.operands {
                    let ty = self.infer_expr(operand);
                    if !matches!(ty, Type::Primitive(PrimitiveType::Bool) | Type::Unknown) {
                        self.errors.push(Error::TypeMismatch {
                            expected: "bool".to_string(),
                            actual: format!("{:?}", ty),
                            span: log.span,
                        });
                    }
                }
                Type::Primitive(PrimitiveType::Bool)
            }
            Expression::Memory(mem) => {
                let base_ty = self.infer_expr(&mem.base);
                if let Some(offset) = &mem.offset {
                    let offset_ty = self.infer_expr(offset);
                    if !self.is_integer_type(&offset_ty) && !matches!(offset_ty, Type::Unknown) {
                        self.errors.push(Error::TypeMismatch {
                            expected: "integer".to_string(),
                            actual: format!("{:?}", offset_ty),
                            span: mem.span,
                        });
                    }
                }

                // Dereferencing a pointer gives the pointed-to type
                if let Type::Pointer(inner) = base_ty {
                    *inner
                } else {
                    Type::Unknown
                }
            }
            Expression::Capability(cap) => {
                // Capability invocations have unknown return type
                for arg in &cap.args {
                    self.infer_expr(arg);
                }
                Type::Unknown
            }
        }
    }

    /// Infer the type of a literal.
    fn infer_literal(&self, lit: &Literal) -> Type {
        match lit {
            Literal::Integer(_) => Type::Primitive(PrimitiveType::I64),
            Literal::Float(_) => Type::Primitive(PrimitiveType::F64),
            Literal::String(_) => Type::Primitive(PrimitiveType::String),
            Literal::Bool(_) => Type::Primitive(PrimitiveType::Bool),
        }
    }

    /// Convert a type expression to internal type representation.
    fn type_from_expr(&self, ty: &TypeExpr) -> Type {
        match ty {
            TypeExpr::Primitive(p) => Type::Primitive(*p),
            TypeExpr::Array { elem, size } => Type::Array {
                elem: Box::new(self.type_from_expr(elem)),
                size: *size,
            },
            TypeExpr::Pointer(inner) => Type::Pointer(Box::new(self.type_from_expr(inner))),
            TypeExpr::Struct(fields) => Type::Struct(
                fields
                    .iter()
                    .map(|f| (f.name.name.clone(), self.type_from_expr(&f.ty)))
                    .collect(),
            ),
            TypeExpr::Enum(variants) => Type::Enum(variants.iter().map(|v| v.name.clone()).collect()),
            TypeExpr::User(name) => Type::User(name.name.clone()),
        }
    }

    /// Check if two types are compatible.
    fn types_compatible(&self, expected: &Type, actual: &Type) -> bool {
        if matches!(expected, Type::Unknown) || matches!(actual, Type::Unknown) {
            return true;
        }
        if matches!(expected, Type::Error) || matches!(actual, Type::Error) {
            return true;
        }

        match (expected, actual) {
            (Type::Primitive(a), Type::Primitive(b)) => a == b,
            (Type::Array { elem: e1, size: s1 }, Type::Array { elem: e2, size: s2 }) => {
                s1 == s2 && self.types_compatible(e1, e2)
            }
            (Type::Pointer(a), Type::Pointer(b)) => self.types_compatible(a, b),
            (Type::Struct(a), Type::Struct(b)) => {
                a.len() == b.len()
                    && a.iter()
                        .zip(b.iter())
                        .all(|((n1, t1), (n2, t2))| n1 == n2 && self.types_compatible(t1, t2))
            }
            (Type::Enum(a), Type::Enum(b)) => a == b,
            (Type::User(a), Type::User(b)) => a == b,
            _ => false,
        }
    }

    /// Check if a type is numeric.
    fn is_numeric_type(&self, ty: &Type) -> bool {
        matches!(
            ty,
            Type::Primitive(
                PrimitiveType::I8
                    | PrimitiveType::I16
                    | PrimitiveType::I32
                    | PrimitiveType::I64
                    | PrimitiveType::U8
                    | PrimitiveType::U16
                    | PrimitiveType::U32
                    | PrimitiveType::U64
                    | PrimitiveType::F32
                    | PrimitiveType::F64
            )
        )
    }

    /// Check if a type is an integer.
    fn is_integer_type(&self, ty: &Type) -> bool {
        matches!(
            ty,
            Type::Primitive(
                PrimitiveType::I8
                    | PrimitiveType::I16
                    | PrimitiveType::I32
                    | PrimitiveType::I64
                    | PrimitiveType::U8
                    | PrimitiveType::U16
                    | PrimitiveType::U32
                    | PrimitiveType::U64
            )
        )
    }

    /// Get the wider of two numeric types.
    fn wider_numeric_type(&self, a: &Type, b: &Type) -> Type {
        // Simplified: just return I64 or F64
        if matches!(a, Type::Primitive(PrimitiveType::F32 | PrimitiveType::F64))
            || matches!(b, Type::Primitive(PrimitiveType::F32 | PrimitiveType::F64))
        {
            Type::Primitive(PrimitiveType::F64)
        } else {
            Type::Primitive(PrimitiveType::I64)
        }
    }

    /// Get collected errors.
    pub fn errors(&self) -> &[Error] {
        &self.errors
    }

    /// Take collected errors.
    pub fn take_errors(&mut self) -> Vec<Error> {
        std::mem::take(&mut self.errors)
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeEnv {
    /// Push a new scope.
    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    /// Pop the current scope.
    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    /// Define a variable in the current scope.
    pub fn define(&mut self, name: &str, ty: Type) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), ty);
        }
    }

    /// Look up a variable in all scopes.
    pub fn lookup(&self, name: &str) -> Option<&Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty);
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_env_scopes() {
        let mut env = TypeEnv::default();
        env.push_scope();
        env.define("x", Type::Primitive(PrimitiveType::I64));

        assert!(env.lookup("x").is_some());

        env.push_scope();
        env.define("y", Type::Primitive(PrimitiveType::Bool));

        assert!(env.lookup("x").is_some());
        assert!(env.lookup("y").is_some());

        env.pop_scope();

        assert!(env.lookup("x").is_some());
        assert!(env.lookup("y").is_none());
    }

    #[test]
    fn test_types_compatible() {
        let checker = TypeChecker::new();

        assert!(checker.types_compatible(
            &Type::Primitive(PrimitiveType::I64),
            &Type::Primitive(PrimitiveType::I64)
        ));

        assert!(!checker.types_compatible(
            &Type::Primitive(PrimitiveType::I64),
            &Type::Primitive(PrimitiveType::Bool)
        ));

        // Unknown is compatible with anything
        assert!(checker.types_compatible(&Type::Unknown, &Type::Primitive(PrimitiveType::I64)));
    }

    #[test]
    fn test_infer_literal() {
        let checker = TypeChecker::new();

        assert_eq!(
            checker.infer_literal(&Literal::Integer(42)),
            Type::Primitive(PrimitiveType::I64)
        );
        assert_eq!(
            checker.infer_literal(&Literal::Bool(true)),
            Type::Primitive(PrimitiveType::Bool)
        );
    }
}
