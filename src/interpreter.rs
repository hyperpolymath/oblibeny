// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Interpreter for Oblíbený compile-time code.
//!
//! This module provides an AST interpreter for evaluating compile-time
//! expressions and functions. It's used by the REPL and macro system.

use crate::ast::{
    ArithmeticExpr, BinaryArithOp, ComparisonExpr, ComparisonOp, Expression, FunctionCall,
    FunctionDef, Literal, LogicalExpr, LogicalOp, LoopStmt, Statement, TopLevelForm,
};
use crate::error::{Error, Result};
use crate::span::Span;
use std::collections::HashMap;
use std::fmt;

/// Runtime value
#[derive(Debug, Clone)]
pub enum Value {
    /// Unit/void value
    Unit,
    /// Boolean
    Bool(bool),
    /// 64-bit signed integer
    Int(i64),
    /// 64-bit floating point
    Float(f64),
    /// String
    String(String),
    /// Array of values
    Array(Vec<Value>),
    /// Struct with named fields
    Struct {
        /// Struct type name
        name: String,
        /// Field values
        fields: HashMap<String, Value>,
    },
    /// Function value (closure)
    Function(Function),
    /// Built-in function
    Builtin(String),
}

impl Value {
    /// Check if this value is unit (void)
    pub fn is_unit(&self) -> bool {
        matches!(self, Value::Unit)
    }

    /// Check if this value is truthy
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Unit => false,
            Value::Bool(b) => *b,
            Value::Int(i) => *i != 0,
            Value::Float(f) => *f != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::Array(a) => !a.is_empty(),
            Value::Struct { .. } => true,
            Value::Function(_) => true,
            Value::Builtin(_) => true,
        }
    }

    /// Get type name
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Unit => "unit",
            Value::Bool(_) => "bool",
            Value::Int(_) => "i64",
            Value::Float(_) => "f64",
            Value::String(_) => "string",
            Value::Array(_) => "array",
            Value::Struct { .. } => "struct",
            Value::Function(_) => "function",
            Value::Builtin(_) => "builtin",
        }
    }

    /// Convert to integer if possible
    pub fn as_int(&self) -> Option<i64> {
        match self {
            Value::Int(i) => Some(*i),
            Value::Float(f) => Some(*f as i64),
            Value::Bool(b) => Some(if *b { 1 } else { 0 }),
            _ => None,
        }
    }

    /// Convert to float if possible
    pub fn as_float(&self) -> Option<f64> {
        match self {
            Value::Float(f) => Some(*f),
            Value::Int(i) => Some(*i as f64),
            _ => None,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Int(i) => write!(f, "{}", i),
            Value::Float(fl) => write!(f, "{}", fl),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Array(arr) => {
                write!(f, "[")?;
                for (i, v) in arr.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, "]")
            }
            Value::Struct { name, fields } => {
                write!(f, "{} {{", name)?;
                for (i, (k, v)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", k, v)?;
                }
                write!(f, "}}")
            }
            Value::Function(func) => write!(f, "<function: {}>", func.name),
            Value::Builtin(name) => write!(f, "<builtin: {}>", name),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Unit, Value::Unit) => true,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Array(a), Value::Array(b)) => a == b,
            _ => false,
        }
    }
}

/// A function value
#[derive(Debug, Clone)]
pub struct Function {
    /// Function name
    pub name: String,
    /// Parameter names
    pub params: Vec<String>,
    /// Function body
    pub body: Vec<Statement>,
    /// Captured environment (closure)
    pub env: Environment,
}

/// Environment for variable bindings
#[derive(Debug, Clone, Default)]
pub struct Environment {
    /// Variable bindings in current scope
    bindings: HashMap<String, Value>,
    /// Parent environment
    parent: Option<Box<Environment>>,
}

impl Environment {
    /// Create a new empty environment
    pub fn new() -> Self {
        Environment {
            bindings: HashMap::new(),
            parent: None,
        }
    }

    /// Create a child environment
    pub fn child(&self) -> Self {
        Environment {
            bindings: HashMap::new(),
            parent: Some(Box::new(self.clone())),
        }
    }

    /// Define a variable in the current scope
    pub fn define(&mut self, name: String, value: Value) {
        self.bindings.insert(name, value);
    }

    /// Get a variable's value
    pub fn get(&self, name: &str) -> Option<&Value> {
        self.bindings
            .get(name)
            .or_else(|| self.parent.as_ref().and_then(|p| p.get(name)))
    }

    /// Set a variable's value (must exist)
    pub fn set(&mut self, name: &str, value: Value) -> bool {
        if self.bindings.contains_key(name) {
            self.bindings.insert(name.to_string(), value);
            true
        } else if let Some(ref mut parent) = self.parent {
            parent.set(name, value)
        } else {
            false
        }
    }

    /// Get all top-level bindings (for display)
    pub fn bindings(&self) -> &HashMap<String, Value> {
        &self.bindings
    }
}

/// The interpreter
pub struct Interpreter {
    /// Global environment
    globals: Environment,
    /// Maximum recursion depth
    max_depth: usize,
    /// Current recursion depth
    current_depth: usize,
    /// Maximum iterations for loops
    max_iterations: u64,
}

impl Interpreter {
    /// Create a new interpreter
    pub fn new() -> Self {
        let mut interp = Interpreter {
            globals: Environment::new(),
            max_depth: 1000,
            current_depth: 0,
            max_iterations: 1_000_000,
        };
        interp.register_builtins();
        interp
    }

    /// Set maximum recursion depth
    pub fn set_max_depth(&mut self, depth: usize) {
        self.max_depth = depth;
    }

    /// Set maximum iterations for loops
    pub fn set_max_iterations(&mut self, iterations: u64) {
        self.max_iterations = iterations;
    }

    /// Get user-defined bindings (excluding builtins)
    pub fn get_bindings(&self) -> Vec<(String, &Value)> {
        self.globals
            .bindings()
            .iter()
            .filter(|(_, v)| !matches!(v, Value::Builtin(_)))
            .map(|(k, v)| (k.clone(), v))
            .collect()
    }

    /// Load a top-level definition into the interpreter
    pub fn load_definition(&mut self, form: &TopLevelForm) {
        let _ = self.eval_toplevel(form);
    }

    /// Call a named function with arguments
    pub fn call_function(&mut self, name: &str, args: Vec<Value>) -> Result<Value> {
        let func = self
            .globals
            .get(name)
            .cloned()
            .ok_or_else(|| Error::UndefinedVariable {
                name: name.to_string(),
                span: Span::new(0, 0),
            })?;

        self.call_function_internal(func, args, Span::new(0, 0))
    }

    /// Register built-in functions
    fn register_builtins(&mut self) {
        let builtins = [
            "abs", "min", "max", "sqrt", "pow", "sin", "cos", "tan", "floor", "ceil", "round",
            "len", "push", "pop", "nth", "concat",
            "print", "println", "debug",
            "type-of", "to-string", "to-int", "to-float",
        ];

        for name in builtins {
            self.globals
                .define(name.to_string(), Value::Builtin(name.to_string()));
        }
    }

    /// Define a function
    fn define_function(&mut self, func: &FunctionDef) -> Result<()> {
        let params: Vec<String> = func.params.iter().map(|p| p.name.name.clone()).collect();

        let function = Function {
            name: func.name.name.clone(),
            params,
            body: func.body.clone(),
            env: self.globals.clone(),
        };

        self.globals
            .define(func.name.name.clone(), Value::Function(function));
        Ok(())
    }

    /// Evaluate a top-level form
    pub fn eval_toplevel(&mut self, form: &TopLevelForm) -> Result<Value> {
        match form {
            TopLevelForm::Function(func) => {
                self.define_function(func)?;
                Ok(Value::Function(Function {
                    name: func.name.name.clone(),
                    params: func.params.iter().map(|p| p.name.name.clone()).collect(),
                    body: func.body.clone(),
                    env: self.globals.clone(),
                }))
            }
            TopLevelForm::CompileTimeBlock(block) => {
                let mut env = self.globals.child();
                for stmt in &block.body {
                    self.eval_statement(stmt, &mut env)?;
                }
                Ok(Value::Unit)
            }
            _ => Ok(Value::Unit),
        }
    }

    /// Evaluate an expression
    pub fn eval_expr(&mut self, expr: &Expression, env: &Environment) -> Result<Value> {
        match expr {
            Expression::Literal(lit) => self.eval_literal(lit),

            Expression::Variable(id) => env
                .get(&id.name)
                .or_else(|| self.globals.get(&id.name))
                .cloned()
                .ok_or_else(|| Error::UndefinedVariable {
                    name: id.name.clone(),
                    span: id.span,
                }),

            Expression::Call(call) => self.eval_call(call, env),

            Expression::Arithmetic(arith) => self.eval_arithmetic(arith, env),

            Expression::Comparison(cmp) => self.eval_comparison(cmp, env),

            Expression::Logical(log) => self.eval_logical(log, env),

            Expression::Memory(_) => Ok(Value::Unit),

            Expression::Capability(_) => Ok(Value::Unit),
        }
    }

    /// Evaluate a literal
    fn eval_literal(&self, lit: &Literal) -> Result<Value> {
        match lit {
            Literal::Integer(i) => Ok(Value::Int(*i)),
            Literal::Float(f) => Ok(Value::Float(*f)),
            Literal::String(s) => Ok(Value::String(s.clone())),
            Literal::Bool(b) => Ok(Value::Bool(*b)),
        }
    }

    /// Evaluate a function call
    fn eval_call(&mut self, call: &FunctionCall, env: &Environment) -> Result<Value> {
        let func_name = &call.name.name;

        // Check for builtins first
        if let Some(Value::Builtin(name)) = self.globals.get(func_name) {
            let name = name.clone();
            let args: Vec<Value> = call
                .args
                .iter()
                .map(|a| self.eval_expr(a, env))
                .collect::<Result<_>>()?;
            return self.call_builtin(&name, args, call.span);
        }

        // Look up function
        let func_val = env
            .get(func_name)
            .or_else(|| self.globals.get(func_name))
            .cloned()
            .ok_or_else(|| Error::UndefinedVariable {
                name: func_name.clone(),
                span: call.span,
            })?;

        let args: Vec<Value> = call
            .args
            .iter()
            .map(|a| self.eval_expr(a, env))
            .collect::<Result<_>>()?;

        self.call_function_internal(func_val, args, call.span)
    }

    /// Evaluate an arithmetic expression
    fn eval_arithmetic(&mut self, arith: &ArithmeticExpr, env: &Environment) -> Result<Value> {
        let left = self.eval_expr(&arith.left, env)?;
        let right = self.eval_expr(&arith.right, env)?;

        match arith.op {
            BinaryArithOp::Add => match (&left, &right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a.wrapping_add(*b))),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
                (Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 + b)),
                (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a + *b as f64)),
                (Value::String(a), Value::String(b)) => Ok(Value::String(format!("{}{}", a, b))),
                _ => Err(Error::TypeMismatch {
                    expected: "numeric or string".to_string(),
                    actual: format!("{} and {}", left.type_name(), right.type_name()),
                    span: arith.span,
                }),
            },
            BinaryArithOp::Sub => match (&left, &right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a.wrapping_sub(*b))),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
                (Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 - b)),
                (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a - *b as f64)),
                _ => Err(Error::TypeMismatch {
                    expected: "numeric".to_string(),
                    actual: format!("{} and {}", left.type_name(), right.type_name()),
                    span: arith.span,
                }),
            },
            BinaryArithOp::Mul => match (&left, &right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a.wrapping_mul(*b))),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
                (Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 * b)),
                (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a * *b as f64)),
                _ => Err(Error::TypeMismatch {
                    expected: "numeric".to_string(),
                    actual: format!("{} and {}", left.type_name(), right.type_name()),
                    span: arith.span,
                }),
            },
            BinaryArithOp::Div => match (&left, &right) {
                (Value::Int(a), Value::Int(b)) if *b != 0 => Ok(Value::Int(a / b)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a / b)),
                (Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 / b)),
                (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a / *b as f64)),
                _ => Err(Error::InternalError {
                    message: "division by zero".to_string(),
                }),
            },
            BinaryArithOp::Mod => match (&left, &right) {
                (Value::Int(a), Value::Int(b)) if *b != 0 => Ok(Value::Int(a % b)),
                _ => Err(Error::InternalError {
                    message: "modulo by zero".to_string(),
                }),
            },
        }
    }

    /// Evaluate a comparison expression
    fn eval_comparison(&mut self, cmp: &ComparisonExpr, env: &Environment) -> Result<Value> {
        let left = self.eval_expr(&cmp.left, env)?;
        let right = self.eval_expr(&cmp.right, env)?;

        let result = match cmp.op {
            ComparisonOp::Eq => left == right,
            ComparisonOp::Neq => left != right,
            ComparisonOp::Lt => match (&left, &right) {
                (Value::Int(a), Value::Int(b)) => a < b,
                (Value::Float(a), Value::Float(b)) => a < b,
                _ => false,
            },
            ComparisonOp::Le => match (&left, &right) {
                (Value::Int(a), Value::Int(b)) => a <= b,
                (Value::Float(a), Value::Float(b)) => a <= b,
                _ => false,
            },
            ComparisonOp::Gt => match (&left, &right) {
                (Value::Int(a), Value::Int(b)) => a > b,
                (Value::Float(a), Value::Float(b)) => a > b,
                _ => false,
            },
            ComparisonOp::Ge => match (&left, &right) {
                (Value::Int(a), Value::Int(b)) => a >= b,
                (Value::Float(a), Value::Float(b)) => a >= b,
                _ => false,
            },
        };

        Ok(Value::Bool(result))
    }

    /// Evaluate a logical expression
    fn eval_logical(&mut self, log: &LogicalExpr, env: &Environment) -> Result<Value> {
        if log.operands.is_empty() {
            return Ok(Value::Bool(true));
        }

        match log.op {
            LogicalOp::And => {
                for operand in &log.operands {
                    let val = self.eval_expr(operand, env)?;
                    if !val.is_truthy() {
                        return Ok(Value::Bool(false));
                    }
                }
                Ok(Value::Bool(true))
            }
            LogicalOp::Or => {
                for operand in &log.operands {
                    let val = self.eval_expr(operand, env)?;
                    if val.is_truthy() {
                        return Ok(Value::Bool(true));
                    }
                }
                Ok(Value::Bool(false))
            }
            LogicalOp::Not => {
                let val = self.eval_expr(&log.operands[0], env)?;
                Ok(Value::Bool(!val.is_truthy()))
            }
        }
    }

    /// Call a function (internal implementation)
    fn call_function_internal(&mut self, func_val: Value, args: Vec<Value>, span: Span) -> Result<Value> {
        self.current_depth += 1;
        if self.current_depth > self.max_depth {
            self.current_depth -= 1;
            return Err(Error::RecursionDetected {
                cycle: "maximum recursion depth exceeded".to_string(),
                span,
            });
        }

        let result = match func_val {
            Value::Function(func) => {
                if args.len() != func.params.len() {
                    return Err(Error::TypeMismatch {
                        expected: format!("{} arguments", func.params.len()),
                        actual: format!("{} arguments", args.len()),
                        span,
                    });
                }

                let mut call_env = func.env.child();
                for (param, arg) in func.params.iter().zip(args) {
                    call_env.define(param.clone(), arg);
                }

                self.eval_statements(&func.body, &mut call_env)
            }
            Value::Builtin(name) => self.call_builtin(&name, args, span),
            _ => Err(Error::TypeMismatch {
                expected: "function".to_string(),
                actual: func_val.type_name().to_string(),
                span,
            }),
        };

        self.current_depth -= 1;
        result
    }

    /// Call a built-in function
    fn call_builtin(&mut self, name: &str, args: Vec<Value>, span: Span) -> Result<Value> {
        match name {
            "abs" => {
                require_args(name, &args, 1, span)?;
                match &args[0] {
                    Value::Int(i) => Ok(Value::Int(i.abs())),
                    Value::Float(f) => Ok(Value::Float(f.abs())),
                    _ => Err(Error::TypeMismatch {
                        expected: "numeric".to_string(),
                        actual: args[0].type_name().to_string(),
                        span,
                    }),
                }
            }
            "min" => {
                require_args(name, &args, 2, span)?;
                match (&args[0], &args[1]) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(*a.min(b))),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a.min(*b))),
                    _ => Err(Error::TypeMismatch {
                        expected: "numeric".to_string(),
                        actual: "mixed types".to_string(),
                        span,
                    }),
                }
            }
            "max" => {
                require_args(name, &args, 2, span)?;
                match (&args[0], &args[1]) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(*a.max(b))),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a.max(*b))),
                    _ => Err(Error::TypeMismatch {
                        expected: "numeric".to_string(),
                        actual: "mixed types".to_string(),
                        span,
                    }),
                }
            }
            "sqrt" => {
                require_args(name, &args, 1, span)?;
                args[0].as_float().map(|f| Value::Float(f.sqrt())).ok_or_else(|| Error::TypeMismatch {
                    expected: "numeric".to_string(),
                    actual: args[0].type_name().to_string(),
                    span,
                })
            }
            "pow" => {
                require_args(name, &args, 2, span)?;
                match (args[0].as_float(), args[1].as_float()) {
                    (Some(base), Some(exp)) => Ok(Value::Float(base.powf(exp))),
                    _ => Err(Error::TypeMismatch {
                        expected: "numeric".to_string(),
                        actual: "other".to_string(),
                        span,
                    }),
                }
            }
            "sin" => {
                require_args(name, &args, 1, span)?;
                args[0].as_float().map(|f| Value::Float(f.sin())).ok_or_else(|| Error::TypeMismatch {
                    expected: "numeric".to_string(),
                    actual: args[0].type_name().to_string(),
                    span,
                })
            }
            "cos" => {
                require_args(name, &args, 1, span)?;
                args[0].as_float().map(|f| Value::Float(f.cos())).ok_or_else(|| Error::TypeMismatch {
                    expected: "numeric".to_string(),
                    actual: args[0].type_name().to_string(),
                    span,
                })
            }
            "tan" => {
                require_args(name, &args, 1, span)?;
                args[0].as_float().map(|f| Value::Float(f.tan())).ok_or_else(|| Error::TypeMismatch {
                    expected: "numeric".to_string(),
                    actual: args[0].type_name().to_string(),
                    span,
                })
            }
            "floor" => {
                require_args(name, &args, 1, span)?;
                args[0].as_float().map(|f| Value::Int(f.floor() as i64)).ok_or_else(|| Error::TypeMismatch {
                    expected: "numeric".to_string(),
                    actual: args[0].type_name().to_string(),
                    span,
                })
            }
            "ceil" => {
                require_args(name, &args, 1, span)?;
                args[0].as_float().map(|f| Value::Int(f.ceil() as i64)).ok_or_else(|| Error::TypeMismatch {
                    expected: "numeric".to_string(),
                    actual: args[0].type_name().to_string(),
                    span,
                })
            }
            "round" => {
                require_args(name, &args, 1, span)?;
                args[0].as_float().map(|f| Value::Int(f.round() as i64)).ok_or_else(|| Error::TypeMismatch {
                    expected: "numeric".to_string(),
                    actual: args[0].type_name().to_string(),
                    span,
                })
            }
            "len" => {
                require_args(name, &args, 1, span)?;
                match &args[0] {
                    Value::Array(arr) => Ok(Value::Int(arr.len() as i64)),
                    Value::String(s) => Ok(Value::Int(s.len() as i64)),
                    _ => Err(Error::TypeMismatch {
                        expected: "array or string".to_string(),
                        actual: args[0].type_name().to_string(),
                        span,
                    }),
                }
            }
            "push" => {
                require_args(name, &args, 2, span)?;
                match args[0].clone() {
                    Value::Array(mut arr) => {
                        arr.push(args[1].clone());
                        Ok(Value::Array(arr))
                    }
                    _ => Err(Error::TypeMismatch {
                        expected: "array".to_string(),
                        actual: args[0].type_name().to_string(),
                        span,
                    }),
                }
            }
            "pop" => {
                require_args(name, &args, 1, span)?;
                match args[0].clone() {
                    Value::Array(mut arr) => {
                        arr.pop();
                        Ok(Value::Array(arr))
                    }
                    _ => Err(Error::TypeMismatch {
                        expected: "array".to_string(),
                        actual: args[0].type_name().to_string(),
                        span,
                    }),
                }
            }
            "nth" => {
                require_args(name, &args, 2, span)?;
                match (&args[0], args[1].as_int()) {
                    (Value::Array(arr), Some(i)) => {
                        if i >= 0 && (i as usize) < arr.len() {
                            Ok(arr[i as usize].clone())
                        } else {
                            Ok(Value::Unit)
                        }
                    }
                    _ => Err(Error::TypeMismatch {
                        expected: "array and integer".to_string(),
                        actual: "other".to_string(),
                        span,
                    }),
                }
            }
            "concat" => {
                let mut result = Vec::new();
                for arg in args {
                    match arg {
                        Value::Array(arr) => result.extend(arr),
                        _ => return Err(Error::TypeMismatch {
                            expected: "arrays".to_string(),
                            actual: arg.type_name().to_string(),
                            span,
                        }),
                    }
                }
                Ok(Value::Array(result))
            }
            "print" => {
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        print!(" ");
                    }
                    print!("{}", arg);
                }
                Ok(Value::Unit)
            }
            "println" => {
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        print!(" ");
                    }
                    print!("{}", arg);
                }
                println!();
                Ok(Value::Unit)
            }
            "debug" => {
                for arg in &args {
                    eprintln!("[DEBUG] {:?}", arg);
                }
                Ok(Value::Unit)
            }
            "type-of" => {
                require_args(name, &args, 1, span)?;
                Ok(Value::String(args[0].type_name().to_string()))
            }
            "to-string" => {
                require_args(name, &args, 1, span)?;
                Ok(Value::String(format!("{}", args[0])))
            }
            "to-int" => {
                require_args(name, &args, 1, span)?;
                match &args[0] {
                    Value::Int(i) => Ok(Value::Int(*i)),
                    Value::Float(f) => Ok(Value::Int(*f as i64)),
                    Value::Bool(b) => Ok(Value::Int(if *b { 1 } else { 0 })),
                    Value::String(s) => s.parse::<i64>().map(Value::Int).map_err(|_| {
                        Error::TypeMismatch {
                            expected: "integer string".to_string(),
                            actual: "invalid string".to_string(),
                            span,
                        }
                    }),
                    _ => Err(Error::TypeMismatch {
                        expected: "convertible to int".to_string(),
                        actual: args[0].type_name().to_string(),
                        span,
                    }),
                }
            }
            "to-float" => {
                require_args(name, &args, 1, span)?;
                match &args[0] {
                    Value::Float(f) => Ok(Value::Float(*f)),
                    Value::Int(i) => Ok(Value::Float(*i as f64)),
                    Value::String(s) => s.parse::<f64>().map(Value::Float).map_err(|_| {
                        Error::TypeMismatch {
                            expected: "float string".to_string(),
                            actual: "invalid string".to_string(),
                            span,
                        }
                    }),
                    _ => Err(Error::TypeMismatch {
                        expected: "convertible to float".to_string(),
                        actual: args[0].type_name().to_string(),
                        span,
                    }),
                }
            }
            _ => Err(Error::UndefinedVariable {
                name: name.to_string(),
                span,
            }),
        }
    }

    /// Evaluate a list of statements
    fn eval_statements(&mut self, stmts: &[Statement], env: &mut Environment) -> Result<Value> {
        let mut result = Value::Unit;
        for stmt in stmts {
            result = self.eval_statement(stmt, env)?;
        }
        Ok(result)
    }

    /// Evaluate a statement
    fn eval_statement(&mut self, stmt: &Statement, env: &mut Environment) -> Result<Value> {
        match stmt {
            Statement::Expression(expr) => self.eval_expr(expr, env),

            Statement::LocalData(local) => {
                let value = if let Some(ref init) = local.init {
                    self.eval_expr(init, env)?
                } else {
                    Value::Unit
                };
                env.define(local.name.name.clone(), value);
                Ok(Value::Unit)
            }

            Statement::Call(call) => self.eval_call(call, env),

            Statement::Loop(loop_stmt) => match loop_stmt {
                LoopStmt::While(while_loop) => {
                    let mut iterations = 0u64;
                    let mut loop_env = env.child();

                    while self.eval_expr(&while_loop.condition, &loop_env)?.is_truthy() {
                        iterations += 1;
                        if iterations > self.max_iterations {
                            return Err(Error::UnboundedLoop {
                                span: while_loop.span,
                            });
                        }
                        self.eval_statements(&while_loop.body, &mut loop_env)?;
                    }
                    Ok(Value::Unit)
                }
                LoopStmt::For(for_loop) => {
                    let collection = self.eval_expr(&for_loop.iter, env)?;
                    let mut loop_env = env.child();

                    match collection {
                        Value::Array(arr) => {
                            for item in arr {
                                loop_env.define(for_loop.var.name.clone(), item);
                                self.eval_statements(&for_loop.body, &mut loop_env)?;
                            }
                        }
                        _ => {
                            return Err(Error::TypeMismatch {
                                expected: "iterable".to_string(),
                                actual: collection.type_name().to_string(),
                                span: for_loop.span,
                            })
                        }
                    }
                    Ok(Value::Unit)
                }
                LoopStmt::BoundedFor(bounded) => {
                    let start = self.eval_expr(&bounded.start, env)?.as_int().unwrap_or(0);
                    let end = self.eval_expr(&bounded.end, env)?.as_int().unwrap_or(0);

                    let mut loop_env = env.child();
                    for i in start..end {
                        loop_env.define(bounded.var.name.clone(), Value::Int(i));
                        self.eval_statements(&bounded.body, &mut loop_env)?;
                    }
                    Ok(Value::Unit)
                }
            },

            _ => Ok(Value::Unit),
        }
    }

    /// Evaluate a single expression string (for REPL)
    pub fn eval(&mut self, source: &str) -> Result<Value> {
        let lexer = crate::lexer::Lexer::new(source);
        let mut parser = crate::parser::Parser::new(lexer);

        match parser.parse_program() {
            Ok(forms) => {
                let mut result = Value::Unit;
                for form in forms {
                    result = self.eval_toplevel(&form)?;
                }
                Ok(result)
            }
            Err(e) => Err(e),
        }
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

/// Helper to check argument count
fn require_args(name: &str, args: &[Value], expected: usize, span: Span) -> Result<()> {
    if args.len() != expected {
        Err(Error::TypeMismatch {
            expected: format!("{} takes {} argument(s)", name, expected),
            actual: format!("got {}", args.len()),
            span,
        })
    } else {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_value_types() {
        assert_eq!(Value::Int(42).type_name(), "i64");
        assert_eq!(Value::Float(3.14).type_name(), "f64");
        assert_eq!(Value::Bool(true).type_name(), "bool");
        assert_eq!(Value::String("hello".to_string()).type_name(), "string");
    }

    #[test]
    fn test_value_equality() {
        assert_eq!(Value::Int(42), Value::Int(42));
        assert_ne!(Value::Int(42), Value::Int(43));
        assert_eq!(Value::Bool(true), Value::Bool(true));
    }

    #[test]
    fn test_value_truthy() {
        assert!(Value::Bool(true).is_truthy());
        assert!(!Value::Bool(false).is_truthy());
        assert!(Value::Int(1).is_truthy());
        assert!(!Value::Int(0).is_truthy());
        assert!(Value::String("hello".to_string()).is_truthy());
        assert!(!Value::String(String::new()).is_truthy());
    }

    #[test]
    fn test_environment() {
        let mut env = Environment::new();
        env.define("x".to_string(), Value::Int(42));
        assert_eq!(env.get("x"), Some(&Value::Int(42)));

        let child = env.child();
        assert_eq!(child.get("x"), Some(&Value::Int(42)));
    }

    #[test]
    fn test_interpreter_creation() {
        let interp = Interpreter::new();
        assert!(interp.globals.get("abs").is_some());
        assert!(interp.globals.get("len").is_some());
    }
}
