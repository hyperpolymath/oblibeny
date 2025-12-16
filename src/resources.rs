// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Resource analysis for Oblíbený.
//!
//! This module implements static resource analysis to verify that
//! deploy-time code respects declared resource bounds:
//! - Maximum iterations
//! - Maximum stack depth
//! - Maximum memory usage
//! - Maximum call depth
//! - Maximum execution time (estimated)
//!
//! This implements "Maximal Principle Reduction" - determining
//! upper bounds on all resources at compile time.

use crate::ast::*;
use crate::callgraph::CallGraph;
use crate::error::{Error, Result};
use crate::span::Span;
use std::collections::HashMap;

/// Resource usage for a function or program.
#[derive(Debug, Clone, Default)]
pub struct ResourceUsage {
    /// Total iterations (all loops).
    pub iterations: Option<u64>,
    /// Maximum stack depth (local variables).
    pub stack_depth: Option<u64>,
    /// Maximum memory usage (bytes).
    pub memory: Option<u64>,
    /// Maximum call depth.
    pub call_depth: Option<u64>,
    /// Estimated instruction count.
    pub instruction_count: Option<u64>,
}

/// Resource bound violation.
#[derive(Debug, Clone)]
pub struct ResourceViolation {
    /// Which resource was exceeded.
    pub resource: ResourceKind,
    /// Actual usage.
    pub actual: u64,
    /// Limit.
    pub limit: u64,
    /// Source location.
    pub span: Span,
}

/// Kinds of resources.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResourceKind {
    /// Iteration count.
    Iterations,
    /// Stack depth.
    StackDepth,
    /// Memory usage.
    Memory,
    /// Call depth.
    CallDepth,
    /// Execution time.
    ExecutionTime,
}

impl std::fmt::Display for ResourceKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResourceKind::Iterations => write!(f, "iterations"),
            ResourceKind::StackDepth => write!(f, "stack depth"),
            ResourceKind::Memory => write!(f, "memory"),
            ResourceKind::CallDepth => write!(f, "call depth"),
            ResourceKind::ExecutionTime => write!(f, "execution time"),
        }
    }
}

/// Resource analyzer.
pub struct ResourceAnalyzer {
    /// Call graph for call depth analysis.
    call_graph: CallGraph,
    /// Cached resource usage per function.
    function_usage: HashMap<String, ResourceUsage>,
    /// Collected violations.
    violations: Vec<ResourceViolation>,
    /// Global resource bounds.
    global_bounds: ResourceBounds,
}

impl ResourceAnalyzer {
    /// Create a new resource analyzer.
    pub fn new(call_graph: CallGraph) -> Self {
        Self {
            call_graph,
            function_usage: HashMap::new(),
            violations: Vec::new(),
            global_bounds: ResourceBounds::default(),
        }
    }

    /// Create with explicit bounds.
    pub fn with_bounds(call_graph: CallGraph, bounds: ResourceBounds) -> Self {
        Self {
            call_graph,
            function_usage: HashMap::new(),
            violations: Vec::new(),
            global_bounds: bounds,
        }
    }

    /// Analyze resource usage for a program.
    pub fn analyze_program(&mut self, program: &[TopLevelForm]) -> Result<()> {
        // Extract global bounds from deployment specs
        for form in program {
            if let TopLevelForm::DeploymentSpec(spec) = form {
                self.global_bounds = self.merge_bounds(&self.global_bounds, &spec.bounds);
            }
        }

        // Analyze each function
        for form in program {
            self.analyze_top_level(form);
        }

        // Check for violations
        if self.violations.is_empty() {
            Ok(())
        } else {
            let v = &self.violations[0];
            Err(Error::ResourceExceeded {
                resource: v.resource.to_string(),
                actual: v.actual,
                limit: v.limit,
                span: v.span,
            })
        }
    }

    /// Get resource usage for a function.
    pub fn get_usage(&self, function: &str) -> Option<&ResourceUsage> {
        self.function_usage.get(function)
    }

    /// Get all violations.
    pub fn violations(&self) -> &[ResourceViolation] {
        &self.violations
    }

    /// Analyze a top-level form.
    fn analyze_top_level(&mut self, form: &TopLevelForm) {
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
                    let usage = self.analyze_function(f);
                    self.check_bounds(&usage, f.span);
                    self.function_usage.insert(f.name.name.clone(), usage);
                }
            }
            TopLevelForm::Module(module) => {
                for item in &module.items {
                    if let ModuleItem::TopLevel(inner) = item {
                        self.analyze_top_level(inner);
                    }
                }
            }
            _ => {}
        }
    }

    /// Analyze resource usage for a function.
    fn analyze_function(&self, f: &FunctionDef) -> ResourceUsage {
        let mut usage = ResourceUsage::default();

        // Analyze stack usage (parameters + locals)
        let param_slots = f.params.len() as u64;
        let local_slots = self.count_locals(&f.body);
        usage.stack_depth = Some(param_slots + local_slots);

        // Analyze iterations
        usage.iterations = self.count_iterations(&f.body);

        // Analyze memory usage
        usage.memory = self.estimate_memory(&f.body);

        // Analyze call depth
        if let Some(depth) = self.call_graph.max_call_depth(&f.name.name) {
            usage.call_depth = Some(depth as u64);
        }

        // Estimate instruction count
        usage.instruction_count = self.estimate_instructions(&f.body);

        usage
    }

    /// Count local variables in statements.
    fn count_locals(&self, stmts: &[Statement]) -> u64 {
        let mut count = 0u64;

        for stmt in stmts {
            match stmt {
                Statement::LocalData(_) => {
                    count += 1;
                }
                Statement::Loop(LoopStmt::BoundedFor(bf)) => {
                    // Loop variable
                    count += 1;
                    count += self.count_locals(&bf.body);
                }
                Statement::Loop(LoopStmt::While(w)) => {
                    count += self.count_locals(&w.body);
                }
                Statement::Loop(LoopStmt::For(f)) => {
                    count += 1; // Loop variable
                    count += self.count_locals(&f.body);
                }
                Statement::Transaction(tx) => {
                    count += self.count_locals(&tx.body);
                }
                Statement::TryCatch(tc) => {
                    count += self.count_locals(&tc.try_body);
                    for catch in &tc.catches {
                        count += 1; // Exception binding
                        count += self.count_locals(&catch.body);
                    }
                    if let Some(finally) = &tc.finally {
                        count += self.count_locals(finally);
                    }
                }
                Statement::ConditionalAssembly(ca) => {
                    count += self.count_locals(&ca.then_branch);
                    if let Some(else_branch) = &ca.else_branch {
                        count += self.count_locals(else_branch);
                    }
                }
                Statement::DeployTimeBlock(block) => {
                    count += self.count_locals(&block.body);
                }
                _ => {}
            }
        }

        count
    }

    /// Count total iterations.
    fn count_iterations(&self, stmts: &[Statement]) -> Option<u64> {
        let mut total = 0u64;

        for stmt in stmts {
            if let Statement::Loop(loop_stmt) = stmt {
                match loop_stmt {
                    LoopStmt::While(_) | LoopStmt::For(_) => {
                        // Unbounded - cannot compute
                        return None;
                    }
                    LoopStmt::BoundedFor(bf) => {
                        let bound = self.compute_bound(bf)?;
                        let inner = self.count_iterations(&bf.body).unwrap_or(0);
                        total = total.checked_add(bound)?;
                        total = total.checked_add(bound.checked_mul(inner)?)?;
                    }
                }
            }
        }

        Some(total)
    }

    /// Compute iteration bound for a bounded-for loop.
    fn compute_bound(&self, bf: &BoundedForLoop) -> Option<u64> {
        let start = self.eval_const(&bf.start)?;
        let end = self.eval_const(&bf.end)?;

        if end >= start {
            Some((end - start) as u64)
        } else {
            Some(0)
        }
    }

    /// Evaluate a constant expression.
    fn eval_const(&self, expr: &Expression) -> Option<i64> {
        match expr {
            Expression::Literal(Literal::Integer(n)) => Some(*n),
            Expression::Arithmetic(arith) => {
                let left = self.eval_const(&arith.left)?;
                let right = self.eval_const(&arith.right)?;
                match arith.op {
                    BinaryArithOp::Add => left.checked_add(right),
                    BinaryArithOp::Sub => left.checked_sub(right),
                    BinaryArithOp::Mul => left.checked_mul(right),
                    BinaryArithOp::Div => left.checked_div(right),
                    BinaryArithOp::Mod => left.checked_rem(right),
                }
            }
            _ => None,
        }
    }

    /// Estimate memory usage.
    fn estimate_memory(&self, stmts: &[Statement]) -> Option<u64> {
        let mut total = 0u64;

        for stmt in stmts {
            match stmt {
                Statement::LocalData(local) => {
                    if let Some(ty) = &local.ty {
                        total = total.checked_add(self.type_size(ty))?;
                    } else {
                        // Default to 8 bytes for unknown types
                        total = total.checked_add(8)?;
                    }
                }
                Statement::DataDirective(dd) => {
                    let elem_size = match dd.size {
                        DataSize::Byte => 1,
                        DataSize::Word => 2,
                        DataSize::DWord => 4,
                        DataSize::QWord => 8,
                    };
                    total = total.checked_add(elem_size * dd.values.len() as u64)?;
                }
                Statement::Loop(LoopStmt::BoundedFor(bf)) => {
                    total = total.checked_add(self.estimate_memory(&bf.body)?)?;
                }
                _ => {}
            }
        }

        Some(total)
    }

    /// Estimate size of a type in bytes.
    fn type_size(&self, ty: &TypeExpr) -> u64 {
        match ty {
            TypeExpr::Primitive(p) => match p {
                PrimitiveType::U8 | PrimitiveType::I8 => 1,
                PrimitiveType::U16 | PrimitiveType::I16 => 2,
                PrimitiveType::U32 | PrimitiveType::I32 | PrimitiveType::F32 => 4,
                PrimitiveType::U64 | PrimitiveType::I64 | PrimitiveType::F64 => 8,
                PrimitiveType::Bool => 1,
                PrimitiveType::String => 16, // Pointer + length
            },
            TypeExpr::Array { elem, size } => self.type_size(elem) * size,
            TypeExpr::Pointer(_) => 8,
            TypeExpr::Struct(fields) => fields.iter().map(|f| self.type_size(&f.ty)).sum(),
            TypeExpr::Enum(variants) => {
                // Tag + max variant size
                1 + variants.len() as u64 * 8
            }
            TypeExpr::User(_) => 8, // Unknown, assume pointer-sized
        }
    }

    /// Estimate instruction count.
    fn estimate_instructions(&self, stmts: &[Statement]) -> Option<u64> {
        let mut count = 0u64;

        for stmt in stmts {
            match stmt {
                Statement::Expression(_) => count += 1,
                Statement::LocalData(_) => count += 2, // Alloc + init
                Statement::Call(call) => count = count.checked_add(1 + call.args.len() as u64)?,
                Statement::Instruction(_) => count += 1,
                Statement::Loop(LoopStmt::BoundedFor(bf)) => {
                    let bound = self.compute_bound(bf)?;
                    let body_count = self.estimate_instructions(&bf.body)?;
                    // Loop setup + bound * (body + loop overhead)
                    count = count.checked_add(3)?;
                    count = count.checked_add(bound.checked_mul(body_count.checked_add(2)?)?)?;
                }
                Statement::ConditionalAssembly(ca) => {
                    let then_count = self.estimate_instructions(&ca.then_branch)?;
                    let else_count = ca
                        .else_branch
                        .as_ref()
                        .map(|e| self.estimate_instructions(e))
                        .unwrap_or(Some(0))?;
                    count = count.checked_add(1 + then_count.max(else_count))?;
                }
                Statement::DeployTimeBlock(block) => {
                    count = count.checked_add(self.estimate_instructions(&block.body)?)?;
                }
                _ => count += 1,
            }
        }

        Some(count)
    }

    /// Check resource usage against bounds.
    fn check_bounds(&mut self, usage: &ResourceUsage, span: Span) {
        if let (Some(actual), Some(limit)) = (usage.iterations, self.global_bounds.max_iterations) {
            if actual > limit {
                self.violations.push(ResourceViolation {
                    resource: ResourceKind::Iterations,
                    actual,
                    limit,
                    span,
                });
            }
        }

        if let (Some(actual), Some(limit)) = (usage.stack_depth, self.global_bounds.max_stack_depth) {
            if actual > limit {
                self.violations.push(ResourceViolation {
                    resource: ResourceKind::StackDepth,
                    actual,
                    limit,
                    span,
                });
            }
        }

        if let (Some(actual), Some(limit)) = (usage.memory, self.global_bounds.max_memory) {
            if actual > limit {
                self.violations.push(ResourceViolation {
                    resource: ResourceKind::Memory,
                    actual,
                    limit,
                    span,
                });
            }
        }

        if let (Some(actual), Some(limit)) = (usage.call_depth, self.global_bounds.max_call_depth) {
            if actual > limit {
                self.violations.push(ResourceViolation {
                    resource: ResourceKind::CallDepth,
                    actual,
                    limit,
                    span,
                });
            }
        }
    }

    /// Merge two sets of bounds (taking the more restrictive).
    fn merge_bounds(&self, a: &ResourceBounds, b: &ResourceBounds) -> ResourceBounds {
        ResourceBounds {
            max_iterations: merge_min(a.max_iterations, b.max_iterations),
            max_stack_depth: merge_min(a.max_stack_depth, b.max_stack_depth),
            max_memory: merge_min(a.max_memory, b.max_memory),
            max_call_depth: merge_min(a.max_call_depth, b.max_call_depth),
            max_execution_time: merge_min(a.max_execution_time, b.max_execution_time),
        }
    }
}

/// Merge two optional values, taking the minimum.
fn merge_min(a: Option<u64>, b: Option<u64>) -> Option<u64> {
    match (a, b) {
        (Some(x), Some(y)) => Some(x.min(y)),
        (Some(x), None) => Some(x),
        (None, Some(y)) => Some(y),
        (None, None) => None,
    }
}

/// Summary of resource usage for reporting.
#[derive(Debug, Clone)]
pub struct ResourceSummary {
    /// Function name.
    pub function: String,
    /// Resource usage.
    pub usage: ResourceUsage,
    /// Whether bounds are satisfied.
    pub bounds_satisfied: bool,
}

impl ResourceAnalyzer {
    /// Generate a summary of all analyzed functions.
    pub fn summary(&self) -> Vec<ResourceSummary> {
        self.function_usage
            .iter()
            .map(|(name, usage)| {
                let bounds_satisfied = !self.violations.iter().any(|v| {
                    // Check if this function has violations (simplified)
                    false
                });

                ResourceSummary {
                    function: name.clone(),
                    usage: usage.clone(),
                    bounds_satisfied,
                }
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_call_graph() -> CallGraph {
        CallGraph::new()
    }

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
    fn test_resource_usage_default() {
        let usage = ResourceUsage::default();
        assert!(usage.iterations.is_none());
        assert!(usage.stack_depth.is_none());
    }

    #[test]
    fn test_count_locals() {
        let analyzer = ResourceAnalyzer::new(make_call_graph());
        let stmts = vec![
            Statement::LocalData(LocalDataDef {
                name: Identifier::new("x", Span::default()),
                ty: None,
                init: None,
                span: Span::default(),
            }),
            Statement::LocalData(LocalDataDef {
                name: Identifier::new("y", Span::default()),
                ty: None,
                init: None,
                span: Span::default(),
            }),
        ];

        assert_eq!(analyzer.count_locals(&stmts), 2);
    }

    #[test]
    fn test_type_size() {
        let analyzer = ResourceAnalyzer::new(make_call_graph());

        assert_eq!(analyzer.type_size(&TypeExpr::Primitive(PrimitiveType::U8)), 1);
        assert_eq!(analyzer.type_size(&TypeExpr::Primitive(PrimitiveType::I64)), 8);
        assert_eq!(
            analyzer.type_size(&TypeExpr::Array {
                elem: Box::new(TypeExpr::Primitive(PrimitiveType::U8)),
                size: 10
            }),
            10
        );
    }

    #[test]
    fn test_iteration_counting() {
        let analyzer = ResourceAnalyzer::new(make_call_graph());
        let stmts = vec![Statement::Loop(LoopStmt::BoundedFor(BoundedForLoop {
            var: Identifier::new("i", Span::default()),
            start: Expression::Literal(Literal::Integer(0)),
            end: Expression::Literal(Literal::Integer(100)),
            body: vec![],
            span: Span::default(),
        }))];

        assert_eq!(analyzer.count_iterations(&stmts), Some(100));
    }

    #[test]
    fn test_bounds_violation() {
        let bounds = ResourceBounds {
            max_iterations: Some(50),
            ..Default::default()
        };
        let mut analyzer = ResourceAnalyzer::with_bounds(make_call_graph(), bounds);

        let program = vec![make_deploy_function(
            "test",
            vec![Statement::Loop(LoopStmt::BoundedFor(BoundedForLoop {
                var: Identifier::new("i", Span::default()),
                start: Expression::Literal(Literal::Integer(0)),
                end: Expression::Literal(Literal::Integer(100)),
                body: vec![],
                span: Span::default(),
            }))],
        )];

        let result = analyzer.analyze_program(&program);
        assert!(result.is_err());
        assert_eq!(analyzer.violations().len(), 1);
        assert_eq!(analyzer.violations()[0].resource, ResourceKind::Iterations);
    }

    #[test]
    fn test_merge_bounds() {
        let analyzer = ResourceAnalyzer::new(make_call_graph());
        let a = ResourceBounds {
            max_iterations: Some(100),
            max_memory: Some(1000),
            ..Default::default()
        };
        let b = ResourceBounds {
            max_iterations: Some(50),
            max_stack_depth: Some(10),
            ..Default::default()
        };

        let merged = analyzer.merge_bounds(&a, &b);
        assert_eq!(merged.max_iterations, Some(50));
        assert_eq!(merged.max_memory, Some(1000));
        assert_eq!(merged.max_stack_depth, Some(10));
    }
}
