// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Phase separation validation for Oblíbený.
//!
//! This module enforces the dual-language paradigm by ensuring that
//! compile-time-only constructs do not appear in deploy-time code.
//!
//! ## Phase Rules
//!
//! **Compile-time only constructs** (forbidden in deploy-time):
//! - `while` loops (unbounded)
//! - `for` loops (unbounded)
//! - `transaction` blocks
//! - `metamorphic-if` rewrites
//! - `try-catch` exception handling
//! - `defmacro` definitions
//! - `syscall` instructions
//! - Recursion (direct or mutual)
//!
//! **Deploy-time allowed constructs**:
//! - `bounded-for` loops (provably terminating)
//! - `if`/`cond` conditionals
//! - Function calls (to acyclic call graph)
//! - Capability invocations
//! - All primitive operations

use crate::ast::*;
use crate::callgraph::CallGraph;
use crate::error::{Error, Result};
use crate::span::Span;

/// Phase context.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Phase {
    /// Compile-time phase (Turing-complete).
    CompileTime,
    /// Deploy-time phase (Turing-incomplete).
    DeployTime,
}

/// Phase violation.
#[derive(Debug, Clone)]
pub struct PhaseViolation {
    /// The construct that violated phase rules.
    pub construct: String,
    /// The phase context where violation occurred.
    pub phase: Phase,
    /// Source location.
    pub span: Span,
    /// Explanation.
    pub reason: String,
}

/// Phase validator.
pub struct PhaseValidator {
    /// Current phase context.
    current_phase: Phase,
    /// Collected violations.
    violations: Vec<PhaseViolation>,
    /// Call graph for recursion detection.
    call_graph: Option<CallGraph>,
    /// Current function name (for recursion tracking).
    current_function: Option<String>,
}

impl PhaseValidator {
    /// Create a new phase validator.
    pub fn new() -> Self {
        Self {
            current_phase: Phase::CompileTime,
            violations: Vec::new(),
            call_graph: None,
            current_function: None,
        }
    }

    /// Create a validator with a call graph for recursion detection.
    pub fn with_call_graph(call_graph: CallGraph) -> Self {
        Self {
            current_phase: Phase::CompileTime,
            violations: Vec::new(),
            call_graph: Some(call_graph),
            current_function: None,
        }
    }

    /// Validate a program.
    pub fn validate_program(&mut self, program: &[TopLevelForm]) -> Result<()> {
        // Build call graph if not provided
        if self.call_graph.is_none() {
            self.call_graph = Some(CallGraph::from_program(program));
        }

        for form in program {
            self.validate_top_level(form);
        }

        if self.violations.is_empty() {
            Ok(())
        } else {
            // Return first violation as error
            let v = &self.violations[0];
            Err(Error::PhaseViolation {
                construct: v.construct.clone(),
                span: v.span,
            })
        }
    }

    /// Get all collected violations.
    pub fn violations(&self) -> &[PhaseViolation] {
        &self.violations
    }

    /// Take collected violations.
    pub fn take_violations(&mut self) -> Vec<PhaseViolation> {
        std::mem::take(&mut self.violations)
    }

    /// Validate a top-level form.
    fn validate_top_level(&mut self, form: &TopLevelForm) {
        match form {
            TopLevelForm::Function(f) => {
                self.validate_function(f);
            }
            TopLevelForm::Macro(m) => {
                // Macros are compile-time only
                let prev_phase = self.current_phase;
                self.current_phase = Phase::CompileTime;
                self.current_function = Some(m.name.name.clone());
                for stmt in &m.body {
                    self.validate_statement(stmt);
                }
                self.current_function = None;
                self.current_phase = prev_phase;
            }
            TopLevelForm::Module(module) => {
                for item in &module.items {
                    if let ModuleItem::TopLevel(inner) = item {
                        self.validate_top_level(inner);
                    }
                }
            }
            TopLevelForm::CompileTimeBlock(block) => {
                let prev_phase = self.current_phase;
                self.current_phase = Phase::CompileTime;
                for stmt in &block.body {
                    self.validate_statement(stmt);
                }
                self.current_phase = prev_phase;
            }
            TopLevelForm::DeploymentSpec(spec) => {
                // Deployment specs imply deploy-time context
                self.validate_deployment_spec(spec);
            }
            TopLevelForm::GlobalData(g) => {
                // Global data with deploy_visible flag
                if g.deploy_visible {
                    let prev_phase = self.current_phase;
                    self.current_phase = Phase::DeployTime;
                    if let Some(init) = &g.init {
                        self.validate_expression(init);
                    }
                    self.current_phase = prev_phase;
                }
            }
            _ => {}
        }
    }

    /// Validate a function definition.
    fn validate_function(&mut self, f: &FunctionDef) {
        // Check function annotations for phase info
        let is_deploy_time = f.annotations.iter().any(|a| {
            matches!(
                a,
                FunctionAnnotation::DeployTarget(DeployTarget::RuntimeOnly)
                    | FunctionAnnotation::DeployTarget(DeployTarget::Both)
            )
        });

        let prev_phase = self.current_phase;
        if is_deploy_time {
            self.current_phase = Phase::DeployTime;

            // Check for recursion in deploy-time functions
            if let Some(cg) = &self.call_graph {
                let cycles = cg.find_cycles();
                for cycle in cycles {
                    if cycle.functions.contains(&f.name.name) {
                        self.violations.push(PhaseViolation {
                            construct: "recursion".to_string(),
                            phase: Phase::DeployTime,
                            span: f.span,
                            reason: format!(
                                "function '{}' is part of recursive cycle: {:?}",
                                f.name.name, cycle.functions
                            ),
                        });
                    }
                }
            }
        }

        self.current_function = Some(f.name.name.clone());
        for stmt in &f.body {
            self.validate_statement(stmt);
        }
        self.current_function = None;
        self.current_phase = prev_phase;
    }

    /// Validate a deployment specification.
    fn validate_deployment_spec(&mut self, spec: &DeploymentSpec) {
        // Check for conflicting security constraints
        if let Some(constraints) = &spec.constraints {
            // Validate that forbidden features are respected
            for forbidden in &constraints.forbidden {
                match forbidden {
                    ForbiddenFeature::Recursion => {
                        // Recursion is checked via call graph
                    }
                    ForbiddenFeature::BackwardJumps => {
                        // Would need control flow analysis
                    }
                    _ => {}
                }
            }
        }
    }

    /// Validate a statement.
    fn validate_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Expression(expr) => {
                self.validate_expression(expr);
            }
            Statement::LocalData(local) => {
                if let Some(init) = &local.init {
                    self.validate_expression(init);
                }
            }
            Statement::Call(call) => {
                self.validate_call(call);
            }
            Statement::Transaction(tx) => {
                if self.current_phase == Phase::DeployTime {
                    self.violations.push(PhaseViolation {
                        construct: "transaction".to_string(),
                        phase: Phase::DeployTime,
                        span: tx.span,
                        reason: "transactions are compile-time only constructs".to_string(),
                    });
                }
                // Still validate body for other issues
                for stmt in &tx.body {
                    self.validate_statement(stmt);
                }
            }
            Statement::MetamorphicIf(mif) => {
                if self.current_phase == Phase::DeployTime {
                    self.violations.push(PhaseViolation {
                        construct: "metamorphic-if".to_string(),
                        phase: Phase::DeployTime,
                        span: mif.span,
                        reason: "metamorphic rewrites are compile-time only".to_string(),
                    });
                }
                self.validate_expression(&mif.condition);
                for stmt in &mif.then_branch {
                    self.validate_statement(stmt);
                }
                if let Some(else_branch) = &mif.else_branch {
                    for stmt in else_branch {
                        self.validate_statement(stmt);
                    }
                }
            }
            Statement::TryCatch(tc) => {
                if self.current_phase == Phase::DeployTime {
                    self.violations.push(PhaseViolation {
                        construct: "try-catch".to_string(),
                        phase: Phase::DeployTime,
                        span: tc.span,
                        reason: "exception handling is compile-time only".to_string(),
                    });
                }
                for stmt in &tc.try_body {
                    self.validate_statement(stmt);
                }
                for catch in &tc.catches {
                    for stmt in &catch.body {
                        self.validate_statement(stmt);
                    }
                }
                if let Some(finally) = &tc.finally {
                    for stmt in finally {
                        self.validate_statement(stmt);
                    }
                }
            }
            Statement::Loop(loop_stmt) => {
                self.validate_loop(loop_stmt);
            }
            Statement::Instruction(instr) => {
                self.validate_instruction(instr);
            }
            Statement::ConditionalAssembly(ca) => {
                for stmt in &ca.then_branch {
                    self.validate_statement(stmt);
                }
                if let Some(else_branch) = &ca.else_branch {
                    for stmt in else_branch {
                        self.validate_statement(stmt);
                    }
                }
            }
            Statement::DeployTimeBlock(block) => {
                let prev_phase = self.current_phase;
                self.current_phase = Phase::DeployTime;
                for stmt in &block.body {
                    self.validate_statement(stmt);
                }
                self.current_phase = prev_phase;
            }
            _ => {}
        }
    }

    /// Validate a loop statement.
    fn validate_loop(&mut self, loop_stmt: &LoopStmt) {
        match loop_stmt {
            LoopStmt::While(w) => {
                if self.current_phase == Phase::DeployTime {
                    self.violations.push(PhaseViolation {
                        construct: "while".to_string(),
                        phase: Phase::DeployTime,
                        span: w.span,
                        reason: "unbounded while loops are compile-time only".to_string(),
                    });
                }
                self.validate_expression(&w.condition);
                for stmt in &w.body {
                    self.validate_statement(stmt);
                }
            }
            LoopStmt::For(f) => {
                if self.current_phase == Phase::DeployTime {
                    self.violations.push(PhaseViolation {
                        construct: "for".to_string(),
                        phase: Phase::DeployTime,
                        span: f.span,
                        reason: "unbounded for loops are compile-time only".to_string(),
                    });
                }
                self.validate_expression(&f.iter);
                for stmt in &f.body {
                    self.validate_statement(stmt);
                }
            }
            LoopStmt::BoundedFor(bf) => {
                // Bounded-for is allowed in deploy-time
                self.validate_expression(&bf.start);
                self.validate_expression(&bf.end);
                for stmt in &bf.body {
                    self.validate_statement(stmt);
                }
            }
        }
    }

    /// Validate an instruction.
    fn validate_instruction(&mut self, instr: &Instruction) {
        match instr {
            Instruction::System(sys) => {
                if self.current_phase == Phase::DeployTime {
                    self.violations.push(PhaseViolation {
                        construct: "syscall".to_string(),
                        phase: Phase::DeployTime,
                        span: sys.span,
                        reason: "system calls are compile-time only".to_string(),
                    });
                }
            }
            Instruction::Memory(mem) => {
                if self.current_phase == Phase::DeployTime {
                    match mem.op {
                        MemoryOpType::Alloc => {
                            self.violations.push(PhaseViolation {
                                construct: "alloc".to_string(),
                                phase: Phase::DeployTime,
                                span: mem.span,
                                reason: "dynamic allocation is forbidden in deploy-time".to_string(),
                            });
                        }
                        MemoryOpType::Free => {
                            self.violations.push(PhaseViolation {
                                construct: "free".to_string(),
                                phase: Phase::DeployTime,
                                span: mem.span,
                                reason: "dynamic deallocation is forbidden in deploy-time".to_string(),
                            });
                        }
                    }
                }
            }
            _ => {}
        }
    }

    /// Validate a function call.
    fn validate_call(&mut self, call: &FunctionCall) {
        // Check for recursion in deploy-time
        if self.current_phase == Phase::DeployTime {
            if let Some(current) = &self.current_function {
                if call.name.name == *current {
                    self.violations.push(PhaseViolation {
                        construct: "direct recursion".to_string(),
                        phase: Phase::DeployTime,
                        span: call.span,
                        reason: format!(
                            "function '{}' calls itself recursively",
                            current
                        ),
                    });
                }
            }
        }

        // Validate arguments
        for arg in &call.args {
            self.validate_expression(arg);
        }
    }

    /// Validate an expression.
    fn validate_expression(&mut self, expr: &Expression) {
        match expr {
            Expression::Call(call) => {
                self.validate_call(call);
            }
            Expression::Arithmetic(arith) => {
                self.validate_expression(&arith.left);
                self.validate_expression(&arith.right);
            }
            Expression::Comparison(cmp) => {
                self.validate_expression(&cmp.left);
                self.validate_expression(&cmp.right);
            }
            Expression::Logical(log) => {
                for operand in &log.operands {
                    self.validate_expression(operand);
                }
            }
            Expression::Memory(mem) => {
                self.validate_expression(&mem.base);
                if let Some(offset) = &mem.offset {
                    self.validate_expression(offset);
                }
            }
            Expression::Capability(cap) => {
                // Capability invocations are allowed in deploy-time
                for arg in &cap.args {
                    self.validate_expression(arg);
                }
            }
            _ => {}
        }
    }

    /// Check if a construct is allowed in deploy-time.
    pub fn is_deploy_time_allowed(construct: &str) -> bool {
        !matches!(
            construct,
            "while"
                | "for"
                | "transaction"
                | "metamorphic-if"
                | "try-catch"
                | "defmacro"
                | "syscall"
                | "alloc"
                | "free"
        )
    }
}

impl Default for PhaseValidator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_deploy_function(name: &str, body: Vec<Statement>) -> TopLevelForm {
        TopLevelForm::Function(FunctionDef {
            name: Identifier::new(name, Span::default()),
            params: vec![],
            return_type: None,
            annotations: vec![FunctionAnnotation::DeployTarget(DeployTarget::RuntimeOnly)],
            body,
            span: Span::default(),
        })
    }

    #[test]
    fn test_while_forbidden_in_deploy_time() {
        let program = vec![make_deploy_function(
            "test",
            vec![Statement::Loop(LoopStmt::While(WhileLoop {
                condition: Expression::Literal(Literal::Bool(true)),
                body: vec![],
                span: Span::default(),
            }))],
        )];

        let mut validator = PhaseValidator::new();
        let result = validator.validate_program(&program);

        assert!(result.is_err());
        assert_eq!(validator.violations.len(), 1);
        assert_eq!(validator.violations[0].construct, "while");
    }

    #[test]
    fn test_bounded_for_allowed_in_deploy_time() {
        let program = vec![make_deploy_function(
            "test",
            vec![Statement::Loop(LoopStmt::BoundedFor(BoundedForLoop {
                var: Identifier::new("i", Span::default()),
                start: Expression::Literal(Literal::Integer(0)),
                end: Expression::Literal(Literal::Integer(10)),
                body: vec![],
                span: Span::default(),
            }))],
        )];

        let mut validator = PhaseValidator::new();
        let result = validator.validate_program(&program);

        assert!(result.is_ok());
        assert!(validator.violations.is_empty());
    }

    #[test]
    fn test_syscall_forbidden_in_deploy_time() {
        let program = vec![make_deploy_function(
            "test",
            vec![Statement::Instruction(Instruction::System(SystemOp {
                operand: None,
                span: Span::default(),
            }))],
        )];

        let mut validator = PhaseValidator::new();
        let result = validator.validate_program(&program);

        assert!(result.is_err());
        assert_eq!(validator.violations[0].construct, "syscall");
    }

    #[test]
    fn test_is_deploy_time_allowed() {
        assert!(!PhaseValidator::is_deploy_time_allowed("while"));
        assert!(!PhaseValidator::is_deploy_time_allowed("syscall"));
        assert!(PhaseValidator::is_deploy_time_allowed("bounded-for"));
        assert!(PhaseValidator::is_deploy_time_allowed("if"));
    }
}
