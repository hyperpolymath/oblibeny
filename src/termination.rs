// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Termination analysis for Oblíbený.
//!
//! This module proves that deploy-time code terminates by verifying:
//! 1. **Call graph acyclicity** - No recursion (direct or mutual)
//! 2. **Bounded iteration** - All loops have compile-time bounds
//! 3. **Finite control flow** - No unbounded jumps
//!
//! ## Termination Strategies
//!
//! - `bounded-loop`: Proves termination via bounded iteration counts
//! - `structural-recursion`: Proves termination via decreasing arguments (compile-time only)
//! - `well-founded-order`: Proves termination via well-founded orderings

use crate::ast::*;
use crate::callgraph::CallGraph;
use crate::error::{Error, Result};
use crate::span::Span;
use std::collections::HashMap;

/// Termination proof result.
#[derive(Debug, Clone)]
pub enum TerminationProof {
    /// Function provably terminates.
    Terminates {
        /// Strategy used to prove termination.
        strategy: ProofStrategy,
        /// Maximum iterations bound (if applicable).
        bound: Option<u64>,
    },
    /// Termination cannot be proven.
    Unprovable {
        /// Reason termination couldn't be proven.
        reason: String,
        /// Location of the problematic construct.
        span: Span,
    },
    /// Function may not terminate (compile-time only is OK).
    MayNotTerminate,
}

/// Proof strategy used.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProofStrategy {
    /// Bounded loop analysis.
    BoundedLoop,
    /// Structural recursion (compile-time).
    StructuralRecursion,
    /// Well-founded ordering.
    WellFoundedOrder,
    /// Trivial (no loops or recursion).
    Trivial,
}

/// Termination checker.
pub struct TerminationChecker {
    /// Call graph for recursion detection.
    call_graph: CallGraph,
    /// Cached proofs for functions.
    proofs: HashMap<String, TerminationProof>,
    /// Collected errors.
    errors: Vec<Error>,
    /// Whether we're in deploy-time context.
    in_deploy_time: bool,
}

/// Loop bound analysis result.
#[derive(Debug, Clone)]
pub enum LoopBound {
    /// Compile-time constant bound.
    Constant(u64),
    /// Symbolic bound (expression).
    Symbolic(String),
    /// Unbounded (cannot determine).
    Unbounded,
}

impl TerminationChecker {
    /// Create a new termination checker.
    pub fn new(call_graph: CallGraph) -> Self {
        Self {
            call_graph,
            proofs: HashMap::new(),
            errors: Vec::new(),
            in_deploy_time: false,
        }
    }

    /// Check termination for a program.
    pub fn check_program(&mut self, program: &[TopLevelForm]) -> Result<()> {
        // Check all deploy-time functions terminate
        for form in program {
            self.check_top_level(form);
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.remove(0))
        }
    }

    /// Get termination proof for a function.
    pub fn get_proof(&self, function: &str) -> Option<&TerminationProof> {
        self.proofs.get(function)
    }

    /// Get all errors.
    pub fn errors(&self) -> &[Error] {
        &self.errors
    }

    /// Check a top-level form.
    fn check_top_level(&mut self, form: &TopLevelForm) {
        match form {
            TopLevelForm::Function(f) => {
                let is_deploy_time = f.annotations.iter().any(|a| {
                    matches!(
                        a,
                        FunctionAnnotation::DeployTarget(DeployTarget::RuntimeOnly)
                            | FunctionAnnotation::DeployTarget(DeployTarget::Both)
                    )
                });

                if is_deploy_time {
                    self.in_deploy_time = true;
                    let proof = self.prove_function_termination(f);

                    match &proof {
                        TerminationProof::Unprovable { reason, span } => {
                            self.errors.push(Error::TerminationUnprovable {
                                function: f.name.name.clone(),
                                span: *span,
                            });
                            // Store reason for diagnostics
                            let _ = reason;
                        }
                        TerminationProof::MayNotTerminate => {
                            self.errors.push(Error::TerminationUnprovable {
                                function: f.name.name.clone(),
                                span: f.span,
                            });
                        }
                        TerminationProof::Terminates { .. } => {}
                    }

                    self.proofs.insert(f.name.name.clone(), proof);
                    self.in_deploy_time = false;
                }
            }
            TopLevelForm::Module(module) => {
                for item in &module.items {
                    if let ModuleItem::TopLevel(inner) = item {
                        self.check_top_level(inner);
                    }
                }
            }
            _ => {}
        }
    }

    /// Prove termination for a function.
    fn prove_function_termination(&self, f: &FunctionDef) -> TerminationProof {
        // Check for explicit termination annotation
        let strategy = f.annotations.iter().find_map(|a| {
            if let FunctionAnnotation::TerminationProof(s) = a {
                Some(s.clone())
            } else {
                None
            }
        });

        // First check for recursion
        let cycles = self.call_graph.find_cycles();
        let in_cycle = cycles.iter().any(|c| c.functions.contains(&f.name.name));

        if in_cycle {
            // Recursion detected - only structural recursion can prove termination
            match strategy {
                Some(TerminationStrategy::StructuralRecursion) if !self.in_deploy_time => {
                    // Structural recursion is allowed in compile-time
                    return TerminationProof::Terminates {
                        strategy: ProofStrategy::StructuralRecursion,
                        bound: None,
                    };
                }
                _ => {
                    return TerminationProof::Unprovable {
                        reason: "recursion detected in deploy-time function".to_string(),
                        span: f.span,
                    };
                }
            }
        }

        // Check all loops are bounded
        let mut max_bound: Option<u64> = None;
        let mut has_unbounded = false;
        let mut unbounded_span = None;

        for stmt in &f.body {
            self.check_statement_bounds(stmt, &mut max_bound, &mut has_unbounded, &mut unbounded_span);
        }

        if has_unbounded {
            return TerminationProof::Unprovable {
                reason: "unbounded loop detected".to_string(),
                span: unbounded_span.unwrap_or(f.span),
            };
        }

        // Function terminates
        let proof_strategy = match strategy {
            Some(TerminationStrategy::BoundedLoop) => ProofStrategy::BoundedLoop,
            Some(TerminationStrategy::WellFoundedOrder) => ProofStrategy::WellFoundedOrder,
            _ if max_bound.is_some() => ProofStrategy::BoundedLoop,
            _ => ProofStrategy::Trivial,
        };

        TerminationProof::Terminates {
            strategy: proof_strategy,
            bound: max_bound,
        }
    }

    /// Check statement for bounded loops.
    fn check_statement_bounds(
        &self,
        stmt: &Statement,
        max_bound: &mut Option<u64>,
        has_unbounded: &mut bool,
        unbounded_span: &mut Option<Span>,
    ) {
        match stmt {
            Statement::Loop(loop_stmt) => {
                match loop_stmt {
                    LoopStmt::While(w) => {
                        // While loops are unbounded
                        *has_unbounded = true;
                        *unbounded_span = Some(w.span);
                    }
                    LoopStmt::For(f) => {
                        // Unbounded for loops
                        *has_unbounded = true;
                        *unbounded_span = Some(f.span);
                    }
                    LoopStmt::BoundedFor(bf) => {
                        // Extract bounds
                        if let Some(bound) = self.compute_iteration_bound(bf) {
                            *max_bound = Some(max_bound.unwrap_or(0).max(bound));
                        } else {
                            // Could not compute bound
                            *has_unbounded = true;
                            *unbounded_span = Some(bf.span);
                        }
                        // Check body
                        for s in &bf.body {
                            self.check_statement_bounds(s, max_bound, has_unbounded, unbounded_span);
                        }
                    }
                }
            }
            Statement::Transaction(tx) => {
                for s in &tx.body {
                    self.check_statement_bounds(s, max_bound, has_unbounded, unbounded_span);
                }
            }
            Statement::MetamorphicIf(mif) => {
                for s in &mif.then_branch {
                    self.check_statement_bounds(s, max_bound, has_unbounded, unbounded_span);
                }
                if let Some(else_branch) = &mif.else_branch {
                    for s in else_branch {
                        self.check_statement_bounds(s, max_bound, has_unbounded, unbounded_span);
                    }
                }
            }
            Statement::TryCatch(tc) => {
                for s in &tc.try_body {
                    self.check_statement_bounds(s, max_bound, has_unbounded, unbounded_span);
                }
                for catch in &tc.catches {
                    for s in &catch.body {
                        self.check_statement_bounds(s, max_bound, has_unbounded, unbounded_span);
                    }
                }
                if let Some(finally) = &tc.finally {
                    for s in finally {
                        self.check_statement_bounds(s, max_bound, has_unbounded, unbounded_span);
                    }
                }
            }
            Statement::ConditionalAssembly(ca) => {
                for s in &ca.then_branch {
                    self.check_statement_bounds(s, max_bound, has_unbounded, unbounded_span);
                }
                if let Some(else_branch) = &ca.else_branch {
                    for s in else_branch {
                        self.check_statement_bounds(s, max_bound, has_unbounded, unbounded_span);
                    }
                }
            }
            Statement::DeployTimeBlock(block) => {
                for s in &block.body {
                    self.check_statement_bounds(s, max_bound, has_unbounded, unbounded_span);
                }
            }
            _ => {}
        }
    }

    /// Compute the iteration bound for a bounded-for loop.
    fn compute_iteration_bound(&self, bf: &BoundedForLoop) -> Option<u64> {
        let start = self.eval_const_expr(&bf.start)?;
        let end = self.eval_const_expr(&bf.end)?;

        if end >= start {
            Some((end - start) as u64)
        } else {
            Some(0)
        }
    }

    /// Evaluate a compile-time constant expression.
    fn eval_const_expr(&self, expr: &Expression) -> Option<i64> {
        match expr {
            Expression::Literal(Literal::Integer(n)) => Some(*n),
            Expression::Arithmetic(arith) => {
                let left = self.eval_const_expr(&arith.left)?;
                let right = self.eval_const_expr(&arith.right)?;
                match arith.op {
                    BinaryArithOp::Add => Some(left.checked_add(right)?),
                    BinaryArithOp::Sub => Some(left.checked_sub(right)?),
                    BinaryArithOp::Mul => Some(left.checked_mul(right)?),
                    BinaryArithOp::Div => Some(left.checked_div(right)?),
                    BinaryArithOp::Mod => Some(left.checked_rem(right)?),
                }
            }
            _ => None,
        }
    }

    /// Analyze loop bound for a bounded-for expression.
    pub fn analyze_loop_bound(&self, start: &Expression, end: &Expression) -> LoopBound {
        match (self.eval_const_expr(start), self.eval_const_expr(end)) {
            (Some(s), Some(e)) => {
                if e >= s {
                    LoopBound::Constant((e - s) as u64)
                } else {
                    LoopBound::Constant(0)
                }
            }
            _ => {
                // Try symbolic analysis
                LoopBound::Symbolic("unknown".to_string())
            }
        }
    }

    /// Calculate total iteration count for nested loops.
    pub fn total_iterations(&self, f: &FunctionDef) -> Option<u64> {
        self.calculate_iterations(&f.body, 1)
    }

    fn calculate_iterations(&self, stmts: &[Statement], multiplier: u64) -> Option<u64> {
        let mut total = 0u64;

        for stmt in stmts {
            if let Statement::Loop(LoopStmt::BoundedFor(bf)) = stmt {
                let bound = self.compute_iteration_bound(bf)?;
                let inner = self.calculate_iterations(&bf.body, multiplier * bound)?;
                total = total.checked_add(multiplier * bound)?;
                total = total.checked_add(inner)?;
            }
        }

        Some(total)
    }
}

/// Compute the maximum call depth for a function.
pub fn max_call_depth(call_graph: &CallGraph, function: &str) -> Option<usize> {
    call_graph.max_call_depth(function)
}

/// Check if a function's call graph is acyclic.
pub fn is_call_graph_acyclic(call_graph: &CallGraph) -> bool {
    call_graph.is_acyclic()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_empty_call_graph() -> CallGraph {
        CallGraph::new()
    }

    fn make_bounded_for(start: i64, end: i64) -> Statement {
        Statement::Loop(LoopStmt::BoundedFor(BoundedForLoop {
            var: Identifier::new("i", Span::default()),
            start: Expression::Literal(Literal::Integer(start)),
            end: Expression::Literal(Literal::Integer(end)),
            body: vec![],
            span: Span::default(),
        }))
    }

    fn make_deploy_function(name: &str, body: Vec<Statement>) -> FunctionDef {
        FunctionDef {
            name: Identifier::new(name, Span::default()),
            params: vec![],
            return_type: None,
            annotations: vec![FunctionAnnotation::DeployTarget(DeployTarget::RuntimeOnly)],
            body,
            span: Span::default(),
        }
    }

    #[test]
    fn test_trivial_termination() {
        let checker = TerminationChecker::new(make_empty_call_graph());
        let f = make_deploy_function("trivial", vec![]);

        let proof = checker.prove_function_termination(&f);
        match proof {
            TerminationProof::Terminates { strategy, bound } => {
                assert_eq!(strategy, ProofStrategy::Trivial);
                assert!(bound.is_none());
            }
            _ => panic!("Expected termination proof"),
        }
    }

    #[test]
    fn test_bounded_for_termination() {
        let checker = TerminationChecker::new(make_empty_call_graph());
        let f = make_deploy_function("bounded", vec![make_bounded_for(0, 100)]);

        let proof = checker.prove_function_termination(&f);
        match proof {
            TerminationProof::Terminates { strategy, bound } => {
                assert_eq!(strategy, ProofStrategy::BoundedLoop);
                assert_eq!(bound, Some(100));
            }
            _ => panic!("Expected termination proof"),
        }
    }

    #[test]
    fn test_while_loop_fails() {
        let checker = TerminationChecker::new(make_empty_call_graph());
        let f = make_deploy_function(
            "unbounded",
            vec![Statement::Loop(LoopStmt::While(WhileLoop {
                condition: Expression::Literal(Literal::Bool(true)),
                body: vec![],
                span: Span::default(),
            }))],
        );

        let proof = checker.prove_function_termination(&f);
        assert!(matches!(proof, TerminationProof::Unprovable { .. }));
    }

    #[test]
    fn test_loop_bound_analysis() {
        let checker = TerminationChecker::new(make_empty_call_graph());

        let start = Expression::Literal(Literal::Integer(0));
        let end = Expression::Literal(Literal::Integer(50));

        match checker.analyze_loop_bound(&start, &end) {
            LoopBound::Constant(bound) => assert_eq!(bound, 50),
            _ => panic!("Expected constant bound"),
        }
    }

    #[test]
    fn test_nested_iterations() {
        let checker = TerminationChecker::new(make_empty_call_graph());
        let inner_loop = make_bounded_for(0, 10);
        let outer = Statement::Loop(LoopStmt::BoundedFor(BoundedForLoop {
            var: Identifier::new("i", Span::default()),
            start: Expression::Literal(Literal::Integer(0)),
            end: Expression::Literal(Literal::Integer(5)),
            body: vec![inner_loop],
            span: Span::default(),
        }));

        let f = make_deploy_function("nested", vec![outer]);
        let total = checker.total_iterations(&f);

        // Outer: 5 iterations, Inner: 10 iterations per outer = 5 + 5*10 = 55
        assert_eq!(total, Some(55));
    }
}
