// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Call graph construction and analysis for Oblíbený.
//!
//! This module builds a call graph from the AST and provides algorithms
//! for detecting cycles (required for termination analysis in deploy-time code).

use crate::ast::*;
use crate::span::Span;
use petgraph::algo::tarjan_scc;
use petgraph::graph::{DiGraph, NodeIndex};
use petgraph::visit::EdgeRef;
use std::collections::HashMap;

/// A call graph representing function call relationships.
#[derive(Debug)]
pub struct CallGraph {
    /// The underlying directed graph.
    graph: DiGraph<FunctionNode, CallEdge>,
    /// Map from function name to node index.
    name_to_node: HashMap<String, NodeIndex>,
}

/// A node in the call graph representing a function.
#[derive(Debug, Clone)]
pub struct FunctionNode {
    /// Function name.
    pub name: String,
    /// Source span of the function definition.
    pub span: Span,
    /// Whether this function is in a deploy-time context.
    pub is_deploy_time: bool,
    /// Whether this function is a macro.
    pub is_macro: bool,
}

/// An edge in the call graph representing a function call.
#[derive(Debug, Clone)]
pub struct CallEdge {
    /// Source span of the call site.
    pub call_site: Span,
    /// Whether this is a recursive call (self-call).
    pub is_direct_recursion: bool,
}

/// A cycle in the call graph.
#[derive(Debug, Clone)]
pub struct Cycle {
    /// Function names in the cycle.
    pub functions: Vec<String>,
    /// Representative span for error reporting.
    pub span: Span,
}

impl CallGraph {
    /// Create a new empty call graph.
    pub fn new() -> Self {
        Self {
            graph: DiGraph::new(),
            name_to_node: HashMap::new(),
        }
    }

    /// Build a call graph from a program.
    pub fn from_program(program: &[TopLevelForm]) -> Self {
        let mut cg = CallGraph::new();

        // First pass: collect all function definitions
        for form in program {
            cg.collect_definitions(form, false);
        }

        // Second pass: collect all call edges
        for form in program {
            cg.collect_calls(form, None, false);
        }

        cg
    }

    /// Collect function definitions from a top-level form.
    fn collect_definitions(&mut self, form: &TopLevelForm, in_deploy_time: bool) {
        match form {
            TopLevelForm::Function(f) => {
                self.add_function(&f.name.name, f.span, in_deploy_time, false);
            }
            TopLevelForm::Macro(m) => {
                self.add_function(&m.name.name, m.span, false, true);
            }
            TopLevelForm::Module(m) => {
                for item in &m.items {
                    if let ModuleItem::TopLevel(inner) = item {
                        self.collect_definitions(inner, in_deploy_time);
                    }
                }
            }
            TopLevelForm::CompileTimeBlock(block) => {
                self.collect_definitions_from_stmts(&block.body, false);
            }
            TopLevelForm::DeploymentSpec(_) => {
                // Deployment specs contain deploy-time code
            }
            _ => {}
        }
    }

    /// Collect definitions from statements.
    fn collect_definitions_from_stmts(&mut self, stmts: &[Statement], in_deploy_time: bool) {
        for stmt in stmts {
            if let Statement::DeployTimeBlock(block) = stmt {
                self.collect_definitions_from_stmts(&block.body, true);
            }
        }
    }

    /// Add a function to the call graph.
    fn add_function(&mut self, name: &str, span: Span, is_deploy_time: bool, is_macro: bool) {
        if !self.name_to_node.contains_key(name) {
            let node = FunctionNode {
                name: name.to_string(),
                span,
                is_deploy_time,
                is_macro,
            };
            let idx = self.graph.add_node(node);
            self.name_to_node.insert(name.to_string(), idx);
        }
    }

    /// Collect call edges from a top-level form.
    fn collect_calls(&mut self, form: &TopLevelForm, current_fn: Option<&str>, in_deploy_time: bool) {
        match form {
            TopLevelForm::Function(f) => {
                let fn_name = &f.name.name;
                // Ensure the function exists in the graph
                self.add_function(fn_name, f.span, in_deploy_time, false);

                for stmt in &f.body {
                    self.collect_calls_from_stmt(stmt, Some(fn_name), in_deploy_time);
                }
            }
            TopLevelForm::Macro(m) => {
                let fn_name = &m.name.name;
                for stmt in &m.body {
                    self.collect_calls_from_stmt(stmt, Some(fn_name), false);
                }
            }
            TopLevelForm::Module(module) => {
                for item in &module.items {
                    if let ModuleItem::TopLevel(inner) = item {
                        self.collect_calls(inner, current_fn, in_deploy_time);
                    }
                }
            }
            TopLevelForm::CompileTimeBlock(block) => {
                for stmt in &block.body {
                    self.collect_calls_from_stmt(stmt, current_fn, false);
                }
            }
            _ => {}
        }
    }

    /// Collect call edges from a statement.
    fn collect_calls_from_stmt(&mut self, stmt: &Statement, current_fn: Option<&str>, in_deploy_time: bool) {
        match stmt {
            Statement::Expression(expr) => {
                self.collect_calls_from_expr(expr, current_fn, in_deploy_time);
            }
            Statement::Call(call) => {
                if let Some(caller) = current_fn {
                    self.add_call(caller, &call.name.name, call.span);
                }
                for arg in &call.args {
                    self.collect_calls_from_expr(arg, current_fn, in_deploy_time);
                }
            }
            Statement::LocalData(local) => {
                if let Some(init) = &local.init {
                    self.collect_calls_from_expr(init, current_fn, in_deploy_time);
                }
            }
            Statement::Transaction(tx) => {
                for s in &tx.body {
                    self.collect_calls_from_stmt(s, current_fn, in_deploy_time);
                }
            }
            Statement::MetamorphicIf(mif) => {
                self.collect_calls_from_expr(&mif.condition, current_fn, in_deploy_time);
                for s in &mif.then_branch {
                    self.collect_calls_from_stmt(s, current_fn, in_deploy_time);
                }
                if let Some(else_branch) = &mif.else_branch {
                    for s in else_branch {
                        self.collect_calls_from_stmt(s, current_fn, in_deploy_time);
                    }
                }
            }
            Statement::TryCatch(tc) => {
                for s in &tc.try_body {
                    self.collect_calls_from_stmt(s, current_fn, in_deploy_time);
                }
                for catch in &tc.catches {
                    for s in &catch.body {
                        self.collect_calls_from_stmt(s, current_fn, in_deploy_time);
                    }
                }
                if let Some(finally) = &tc.finally {
                    for s in finally {
                        self.collect_calls_from_stmt(s, current_fn, in_deploy_time);
                    }
                }
            }
            Statement::Loop(loop_stmt) => {
                match loop_stmt {
                    LoopStmt::While(w) => {
                        self.collect_calls_from_expr(&w.condition, current_fn, in_deploy_time);
                        for s in &w.body {
                            self.collect_calls_from_stmt(s, current_fn, in_deploy_time);
                        }
                    }
                    LoopStmt::For(f) => {
                        self.collect_calls_from_expr(&f.iter, current_fn, in_deploy_time);
                        for s in &f.body {
                            self.collect_calls_from_stmt(s, current_fn, in_deploy_time);
                        }
                    }
                    LoopStmt::BoundedFor(bf) => {
                        self.collect_calls_from_expr(&bf.start, current_fn, in_deploy_time);
                        self.collect_calls_from_expr(&bf.end, current_fn, in_deploy_time);
                        for s in &bf.body {
                            self.collect_calls_from_stmt(s, current_fn, in_deploy_time);
                        }
                    }
                }
            }
            Statement::ConditionalAssembly(ca) => {
                for s in &ca.then_branch {
                    self.collect_calls_from_stmt(s, current_fn, in_deploy_time);
                }
                if let Some(else_branch) = &ca.else_branch {
                    for s in else_branch {
                        self.collect_calls_from_stmt(s, current_fn, in_deploy_time);
                    }
                }
            }
            Statement::DeployTimeBlock(block) => {
                for s in &block.body {
                    self.collect_calls_from_stmt(s, current_fn, true);
                }
            }
            _ => {}
        }
    }

    /// Collect call edges from an expression.
    fn collect_calls_from_expr(&mut self, expr: &Expression, current_fn: Option<&str>, _in_deploy_time: bool) {
        match expr {
            Expression::Call(call) => {
                if let Some(caller) = current_fn {
                    self.add_call(caller, &call.name.name, call.span);
                }
                for arg in &call.args {
                    self.collect_calls_from_expr(arg, current_fn, _in_deploy_time);
                }
            }
            Expression::Arithmetic(arith) => {
                self.collect_calls_from_expr(&arith.left, current_fn, _in_deploy_time);
                self.collect_calls_from_expr(&arith.right, current_fn, _in_deploy_time);
            }
            Expression::Comparison(cmp) => {
                self.collect_calls_from_expr(&cmp.left, current_fn, _in_deploy_time);
                self.collect_calls_from_expr(&cmp.right, current_fn, _in_deploy_time);
            }
            Expression::Logical(log) => {
                for operand in &log.operands {
                    self.collect_calls_from_expr(operand, current_fn, _in_deploy_time);
                }
            }
            Expression::Memory(mem) => {
                self.collect_calls_from_expr(&mem.base, current_fn, _in_deploy_time);
                if let Some(offset) = &mem.offset {
                    self.collect_calls_from_expr(offset, current_fn, _in_deploy_time);
                }
            }
            Expression::Capability(cap) => {
                for arg in &cap.args {
                    self.collect_calls_from_expr(arg, current_fn, _in_deploy_time);
                }
            }
            _ => {}
        }
    }

    /// Add a call edge between two functions.
    fn add_call(&mut self, caller: &str, callee: &str, call_site: Span) {
        // Ensure both functions exist
        let caller_idx = match self.name_to_node.get(caller) {
            Some(&idx) => idx,
            None => {
                // Caller should exist, but add it just in case
                let node = FunctionNode {
                    name: caller.to_string(),
                    span: Span::default(),
                    is_deploy_time: false,
                    is_macro: false,
                };
                let idx = self.graph.add_node(node);
                self.name_to_node.insert(caller.to_string(), idx);
                idx
            }
        };

        let callee_idx = match self.name_to_node.get(callee) {
            Some(&idx) => idx,
            None => {
                // Callee might be an external or built-in function
                let node = FunctionNode {
                    name: callee.to_string(),
                    span: Span::default(),
                    is_deploy_time: false,
                    is_macro: false,
                };
                let idx = self.graph.add_node(node);
                self.name_to_node.insert(callee.to_string(), idx);
                idx
            }
        };

        let is_direct_recursion = caller == callee;
        let edge = CallEdge {
            call_site,
            is_direct_recursion,
        };
        self.graph.add_edge(caller_idx, callee_idx, edge);
    }

    /// Find all strongly connected components (cycles) in the call graph.
    pub fn find_cycles(&self) -> Vec<Cycle> {
        let sccs = tarjan_scc(&self.graph);
        let mut cycles = Vec::new();

        for scc in sccs {
            // A cycle exists if:
            // 1. The SCC has more than one node, OR
            // 2. The SCC has one node with a self-edge
            if scc.len() > 1 {
                let functions: Vec<_> = scc
                    .iter()
                    .map(|&idx| self.graph[idx].name.clone())
                    .collect();
                let span = self.graph[scc[0]].span;
                cycles.push(Cycle { functions, span });
            } else if scc.len() == 1 {
                let idx = scc[0];
                // Check for self-edge
                if self.graph.edges(idx).any(|e| e.target() == idx) {
                    cycles.push(Cycle {
                        functions: vec![self.graph[idx].name.clone()],
                        span: self.graph[idx].span,
                    });
                }
            }
        }

        cycles
    }

    /// Find cycles that involve deploy-time functions.
    pub fn find_deploy_time_cycles(&self) -> Vec<Cycle> {
        self.find_cycles()
            .into_iter()
            .filter(|cycle| {
                cycle.functions.iter().any(|name| {
                    self.name_to_node
                        .get(name)
                        .map(|&idx| self.graph[idx].is_deploy_time)
                        .unwrap_or(false)
                })
            })
            .collect()
    }

    /// Check if the call graph is acyclic (required for deploy-time code).
    pub fn is_acyclic(&self) -> bool {
        self.find_cycles().is_empty()
    }

    /// Get all functions called by a given function.
    pub fn callees(&self, function: &str) -> Vec<&str> {
        match self.name_to_node.get(function) {
            Some(&idx) => self
                .graph
                .neighbors(idx)
                .map(|neighbor| self.graph[neighbor].name.as_str())
                .collect(),
            None => Vec::new(),
        }
    }

    /// Get all functions that call a given function.
    pub fn callers(&self, function: &str) -> Vec<&str> {
        match self.name_to_node.get(function) {
            Some(&idx) => self
                .graph
                .neighbors_directed(idx, petgraph::Direction::Incoming)
                .map(|neighbor| self.graph[neighbor].name.as_str())
                .collect(),
            None => Vec::new(),
        }
    }

    /// Get the maximum call depth starting from a function.
    pub fn max_call_depth(&self, function: &str) -> Option<usize> {
        let idx = self.name_to_node.get(function)?;
        self.max_depth_from(*idx, &mut HashMap::new())
    }

    fn max_depth_from(
        &self,
        idx: NodeIndex,
        visited: &mut HashMap<NodeIndex, Option<usize>>,
    ) -> Option<usize> {
        if visited.contains_key(&idx) {
            // Cycle detected or already computed
            return visited.get(&idx).copied().flatten();
        }

        // Mark as being visited (None indicates in-progress)
        visited.insert(idx, None);

        let mut max_child_depth = 0usize;
        for neighbor in self.graph.neighbors(idx) {
            if let Some(depth) = self.max_depth_from(neighbor, visited) {
                max_child_depth = max_child_depth.max(depth);
            } else {
                // Cycle detected
                return None;
            }
        }

        let depth = max_child_depth + 1;
        visited.insert(idx, Some(depth));
        Some(depth)
    }

    /// Get the number of functions in the call graph.
    pub fn function_count(&self) -> usize {
        self.graph.node_count()
    }

    /// Get the number of call edges in the call graph.
    pub fn call_count(&self) -> usize {
        self.graph.edge_count()
    }

    /// Get a function node by name.
    pub fn get_function(&self, name: &str) -> Option<&FunctionNode> {
        self.name_to_node.get(name).map(|&idx| &self.graph[idx])
    }

    /// Iterate over all functions.
    pub fn functions(&self) -> impl Iterator<Item = &FunctionNode> {
        self.graph.node_weights()
    }
}

impl Default for CallGraph {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_simple_function(name: &str) -> TopLevelForm {
        TopLevelForm::Function(FunctionDef {
            name: Identifier::new(name, Span::default()),
            params: vec![],
            return_type: None,
            annotations: vec![],
            body: vec![],
            span: Span::default(),
        })
    }

    fn make_function_with_call(name: &str, calls: &str) -> TopLevelForm {
        TopLevelForm::Function(FunctionDef {
            name: Identifier::new(name, Span::default()),
            params: vec![],
            return_type: None,
            annotations: vec![],
            body: vec![Statement::Call(FunctionCall {
                name: Identifier::new(calls, Span::default()),
                args: vec![],
                span: Span::default(),
            })],
            span: Span::default(),
        })
    }

    #[test]
    fn test_empty_call_graph() {
        let cg = CallGraph::new();
        assert_eq!(cg.function_count(), 0);
        assert_eq!(cg.call_count(), 0);
        assert!(cg.is_acyclic());
    }

    #[test]
    fn test_simple_call_graph() {
        let program = vec![
            make_simple_function("main"),
            make_function_with_call("foo", "bar"),
            make_simple_function("bar"),
        ];
        let cg = CallGraph::from_program(&program);

        assert_eq!(cg.function_count(), 3);
        assert!(cg.is_acyclic());
        assert_eq!(cg.callees("foo"), vec!["bar"]);
        assert_eq!(cg.callers("bar"), vec!["foo"]);
    }

    #[test]
    fn test_direct_recursion() {
        let program = vec![make_function_with_call("recursive", "recursive")];
        let cg = CallGraph::from_program(&program);

        assert!(!cg.is_acyclic());
        let cycles = cg.find_cycles();
        assert_eq!(cycles.len(), 1);
        assert_eq!(cycles[0].functions, vec!["recursive"]);
    }

    #[test]
    fn test_mutual_recursion() {
        let program = vec![
            make_function_with_call("ping", "pong"),
            make_function_with_call("pong", "ping"),
        ];
        let cg = CallGraph::from_program(&program);

        assert!(!cg.is_acyclic());
        let cycles = cg.find_cycles();
        assert_eq!(cycles.len(), 1);
        assert!(cycles[0].functions.contains(&"ping".to_string()));
        assert!(cycles[0].functions.contains(&"pong".to_string()));
    }

    #[test]
    fn test_max_call_depth() {
        let program = vec![
            make_function_with_call("a", "b"),
            make_function_with_call("b", "c"),
            make_simple_function("c"),
        ];
        let cg = CallGraph::from_program(&program);

        assert_eq!(cg.max_call_depth("a"), Some(3));
        assert_eq!(cg.max_call_depth("b"), Some(2));
        assert_eq!(cg.max_call_depth("c"), Some(1));
    }
}
