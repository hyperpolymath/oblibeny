// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Symbol table and name resolution for Oblíbený.
//!
//! This module implements:
//! - Symbol table with scoped name binding
//! - Name resolution for identifiers
//! - Module and import resolution
//! - Visibility checking (public/private)

use crate::ast::*;
use crate::error::{Error, Result};
use crate::span::Span;
use std::collections::HashMap;

/// Unique identifier for a symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(pub u32);

impl SymbolId {
    /// Create a new symbol ID.
    pub fn new(id: u32) -> Self {
        Self(id)
    }
}

/// Symbol kinds.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolKind {
    /// Function definition.
    Function,
    /// Macro definition.
    Macro,
    /// Global variable.
    Global,
    /// Local variable.
    Local,
    /// Function parameter.
    Parameter,
    /// Loop variable.
    LoopVar,
    /// Module.
    Module,
    /// Type alias.
    TypeAlias,
    /// Struct definition.
    Struct,
    /// Enum definition.
    Enum,
    /// Enum variant.
    EnumVariant,
    /// Imported symbol.
    Import,
}

/// Visibility of a symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Visibility {
    /// Private to the current module.
    #[default]
    Private,
    /// Public, exported from the module.
    Public,
}

/// Phase availability of a symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolPhase {
    /// Available only at compile-time.
    CompileOnly,
    /// Available only at deploy-time.
    DeployOnly,
    /// Available in both phases.
    Both,
}

impl Default for SymbolPhase {
    fn default() -> Self {
        Self::Both
    }
}

/// A symbol in the symbol table.
#[derive(Debug, Clone)]
pub struct Symbol {
    /// Unique identifier.
    pub id: SymbolId,
    /// Symbol name.
    pub name: String,
    /// Symbol kind.
    pub kind: SymbolKind,
    /// Visibility.
    pub visibility: Visibility,
    /// Phase availability.
    pub phase: SymbolPhase,
    /// Definition span.
    pub span: Span,
    /// Type (if resolved).
    pub ty: Option<ResolvedType>,
    /// Parent module (if any).
    pub module: Option<SymbolId>,
}

/// Resolved type information.
#[derive(Debug, Clone, PartialEq)]
pub enum ResolvedType {
    /// Unknown/unresolved type.
    Unknown,
    /// Primitive type.
    Primitive(PrimitiveType),
    /// Array type.
    Array { elem: Box<ResolvedType>, size: u64 },
    /// Pointer type.
    Pointer(Box<ResolvedType>),
    /// Function type.
    Function {
        params: Vec<ResolvedType>,
        ret: Box<ResolvedType>,
    },
    /// Struct type.
    Struct(SymbolId),
    /// Enum type.
    Enum(SymbolId),
    /// User-defined type reference.
    User(SymbolId),
    /// Unit type.
    Unit,
    /// Error type (for error recovery).
    Error,
}

/// A scope in the symbol table.
#[derive(Debug, Clone)]
pub struct Scope {
    /// Symbols defined in this scope.
    symbols: HashMap<String, SymbolId>,
    /// Parent scope.
    parent: Option<ScopeId>,
    /// Scope kind.
    kind: ScopeKind,
}

/// Scope identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(pub u32);

/// Kinds of scopes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeKind {
    /// Global/module scope.
    Global,
    /// Module scope.
    Module,
    /// Function body scope.
    Function,
    /// Block scope (let, loop body, etc.).
    Block,
}

/// The symbol table.
#[derive(Debug)]
pub struct SymbolTable {
    /// All symbols.
    symbols: Vec<Symbol>,
    /// All scopes.
    scopes: Vec<Scope>,
    /// Current scope stack.
    scope_stack: Vec<ScopeId>,
    /// Next symbol ID.
    next_symbol_id: u32,
    /// Module registry (name -> symbol ID).
    modules: HashMap<String, SymbolId>,
}

impl SymbolTable {
    /// Create a new symbol table.
    pub fn new() -> Self {
        let mut table = Self {
            symbols: Vec::new(),
            scopes: Vec::new(),
            scope_stack: Vec::new(),
            next_symbol_id: 0,
            modules: HashMap::new(),
        };
        // Create global scope
        let global_scope = Scope {
            symbols: HashMap::new(),
            parent: None,
            kind: ScopeKind::Global,
        };
        table.scopes.push(global_scope);
        table.scope_stack.push(ScopeId(0));
        table
    }

    /// Enter a new scope.
    pub fn enter_scope(&mut self, kind: ScopeKind) -> ScopeId {
        let parent = self.current_scope_id();
        let scope = Scope {
            symbols: HashMap::new(),
            parent: Some(parent),
            kind,
        };
        let id = ScopeId(self.scopes.len() as u32);
        self.scopes.push(scope);
        self.scope_stack.push(id);
        id
    }

    /// Exit the current scope.
    pub fn exit_scope(&mut self) {
        if self.scope_stack.len() > 1 {
            self.scope_stack.pop();
        }
    }

    /// Get the current scope ID.
    pub fn current_scope_id(&self) -> ScopeId {
        *self.scope_stack.last().unwrap()
    }

    /// Get the current scope.
    fn current_scope(&self) -> &Scope {
        let id = self.current_scope_id();
        &self.scopes[id.0 as usize]
    }

    /// Get the current scope mutably.
    fn current_scope_mut(&mut self) -> &mut Scope {
        let id = self.current_scope_id();
        &mut self.scopes[id.0 as usize]
    }

    /// Define a new symbol in the current scope.
    pub fn define(&mut self, name: String, kind: SymbolKind, span: Span) -> Result<SymbolId> {
        // Check for redefinition in current scope
        if self.current_scope().symbols.contains_key(&name) {
            return Err(Error::DuplicateDefinition {
                name: name.clone(),
                span,
            });
        }

        let id = SymbolId(self.next_symbol_id);
        self.next_symbol_id += 1;

        let symbol = Symbol {
            id,
            name: name.clone(),
            kind,
            visibility: Visibility::Private,
            phase: SymbolPhase::Both,
            span,
            ty: None,
            module: None,
        };

        self.symbols.push(symbol);
        self.current_scope_mut().symbols.insert(name, id);
        Ok(id)
    }

    /// Define a symbol with full details.
    pub fn define_full(
        &mut self,
        name: String,
        kind: SymbolKind,
        visibility: Visibility,
        phase: SymbolPhase,
        span: Span,
    ) -> Result<SymbolId> {
        // Check for redefinition in current scope
        if self.current_scope().symbols.contains_key(&name) {
            return Err(Error::DuplicateDefinition {
                name: name.clone(),
                span,
            });
        }

        let id = SymbolId(self.next_symbol_id);
        self.next_symbol_id += 1;

        let symbol = Symbol {
            id,
            name: name.clone(),
            kind,
            visibility,
            phase,
            span,
            ty: None,
            module: None,
        };

        self.symbols.push(symbol);
        self.current_scope_mut().symbols.insert(name, id);
        Ok(id)
    }

    /// Look up a symbol by name in all accessible scopes.
    pub fn lookup(&self, name: &str) -> Option<SymbolId> {
        let mut scope_id = Some(self.current_scope_id());

        while let Some(id) = scope_id {
            let scope = &self.scopes[id.0 as usize];
            if let Some(&sym_id) = scope.symbols.get(name) {
                return Some(sym_id);
            }
            scope_id = scope.parent;
        }

        None
    }

    /// Look up a symbol only in the current scope.
    pub fn lookup_local(&self, name: &str) -> Option<SymbolId> {
        self.current_scope().symbols.get(name).copied()
    }

    /// Look up a qualified name (e.g., "module.submodule.name").
    pub fn lookup_qualified(&self, path: &[String]) -> Option<SymbolId> {
        if path.is_empty() {
            return None;
        }

        if path.len() == 1 {
            return self.lookup(&path[0]);
        }

        // Look up the module chain
        let mut current_module = self.modules.get(&path[0]).copied()?;

        for segment in &path[1..path.len() - 1] {
            // Find submodule
            let module_sym = self.get(current_module)?;
            if module_sym.kind != SymbolKind::Module {
                return None;
            }
            // Look up in module's scope
            // (This would require tracking module scopes, simplified here)
            current_module = self.lookup(segment)?;
        }

        // Look up the final name
        self.lookup(path.last().unwrap())
    }

    /// Get a symbol by ID.
    pub fn get(&self, id: SymbolId) -> Option<&Symbol> {
        self.symbols.get(id.0 as usize)
    }

    /// Get a symbol by ID mutably.
    pub fn get_mut(&mut self, id: SymbolId) -> Option<&mut Symbol> {
        self.symbols.get_mut(id.0 as usize)
    }

    /// Register a module.
    pub fn register_module(&mut self, name: String, id: SymbolId) {
        self.modules.insert(name, id);
    }

    /// Get a module by name.
    pub fn get_module(&self, name: &str) -> Option<SymbolId> {
        self.modules.get(name).copied()
    }

    /// Set the type of a symbol.
    pub fn set_type(&mut self, id: SymbolId, ty: ResolvedType) {
        if let Some(sym) = self.get_mut(id) {
            sym.ty = Some(ty);
        }
    }

    /// Set the visibility of a symbol.
    pub fn set_visibility(&mut self, id: SymbolId, visibility: Visibility) {
        if let Some(sym) = self.get_mut(id) {
            sym.visibility = visibility;
        }
    }

    /// Set the phase of a symbol.
    pub fn set_phase(&mut self, id: SymbolId, phase: SymbolPhase) {
        if let Some(sym) = self.get_mut(id) {
            sym.phase = phase;
        }
    }

    /// Get all symbols in the current scope.
    pub fn current_scope_symbols(&self) -> impl Iterator<Item = SymbolId> + '_ {
        self.current_scope().symbols.values().copied()
    }

    /// Check if a symbol is accessible from the current scope.
    pub fn is_accessible(&self, id: SymbolId) -> bool {
        let sym = match self.get(id) {
            Some(s) => s,
            None => return false,
        };

        // Public symbols are always accessible
        if sym.visibility == Visibility::Public {
            return true;
        }

        // Private symbols are accessible within the same module
        // (Simplified: check if it's in an ancestor scope)
        let mut scope_id = Some(self.current_scope_id());
        while let Some(sid) = scope_id {
            let scope = &self.scopes[sid.0 as usize];
            if scope.symbols.values().any(|&s| s == id) {
                return true;
            }
            scope_id = scope.parent;
        }

        false
    }

    /// Iterator over all symbols.
    pub fn all_symbols(&self) -> impl Iterator<Item = &Symbol> {
        self.symbols.iter()
    }

    /// Get the number of symbols.
    pub fn len(&self) -> usize {
        self.symbols.len()
    }

    /// Check if empty.
    pub fn is_empty(&self) -> bool {
        self.symbols.is_empty()
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

/// Name resolver that populates a symbol table from an AST.
pub struct NameResolver {
    /// The symbol table being built.
    pub symbols: SymbolTable,
    /// Collected errors.
    errors: Vec<Error>,
    /// Current module context.
    current_module: Option<SymbolId>,
}

impl NameResolver {
    /// Create a new name resolver.
    pub fn new() -> Self {
        Self {
            symbols: SymbolTable::new(),
            errors: Vec::new(),
            current_module: None,
        }
    }

    /// Resolve names in a program.
    pub fn resolve_program(&mut self, program: &[TopLevelForm]) -> Result<()> {
        // First pass: collect all top-level definitions
        for form in program {
            self.collect_definition(form);
        }

        // Second pass: resolve references
        for form in program {
            self.resolve_form(form);
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.remove(0))
        }
    }

    /// Collect a top-level definition.
    fn collect_definition(&mut self, form: &TopLevelForm) {
        match form {
            TopLevelForm::Function(f) => {
                let phase = Self::extract_phase(&f.annotations);
                if let Err(e) = self.symbols.define_full(
                    f.name.name.clone(),
                    SymbolKind::Function,
                    Visibility::Private,
                    phase,
                    f.name.span,
                ) {
                    self.errors.push(e);
                }
            }
            TopLevelForm::Macro(m) => {
                if let Err(e) = self.symbols.define_full(
                    m.name.name.clone(),
                    SymbolKind::Macro,
                    Visibility::Private,
                    SymbolPhase::CompileOnly,
                    m.name.span,
                ) {
                    self.errors.push(e);
                }
            }
            TopLevelForm::GlobalData(g) => {
                let phase = if g.deploy_visible {
                    SymbolPhase::Both
                } else {
                    SymbolPhase::CompileOnly
                };
                if let Err(e) = self.symbols.define_full(
                    g.name.name.clone(),
                    SymbolKind::Global,
                    Visibility::Private,
                    phase,
                    g.name.span,
                ) {
                    self.errors.push(e);
                }
            }
            TopLevelForm::Module(module) => {
                match self.symbols.define(
                    module.name.name.clone(),
                    SymbolKind::Module,
                    module.name.span,
                ) {
                    Ok(id) => {
                        self.symbols.register_module(module.name.name.clone(), id);
                        let prev_module = self.current_module;
                        self.current_module = Some(id);
                        self.symbols.enter_scope(ScopeKind::Module);

                        for item in &module.items {
                            self.collect_module_item(item);
                        }

                        self.symbols.exit_scope();
                        self.current_module = prev_module;
                    }
                    Err(e) => self.errors.push(e),
                }
            }
            TopLevelForm::CompileTimeBlock(block) => {
                for stmt in &block.body {
                    self.collect_statement_defs(stmt);
                }
            }
            _ => {}
        }
    }

    /// Collect definitions from a module item.
    fn collect_module_item(&mut self, item: &ModuleItem) {
        match item {
            ModuleItem::TopLevel(form) => {
                self.collect_definition(form);
            }
            ModuleItem::Export(export) => {
                // Mark exported symbols as public
                for ident in &export.items {
                    if let Some(id) = self.symbols.lookup(&ident.name) {
                        self.symbols.set_visibility(id, Visibility::Public);
                    }
                }
            }
            ModuleItem::Import(_import) => {
                // Handle in resolve pass
            }
        }
    }

    /// Collect definitions from statements.
    fn collect_statement_defs(&mut self, stmt: &Statement) {
        if let Statement::LocalData(local) = stmt {
            if let Err(e) = self.symbols.define(
                local.name.name.clone(),
                SymbolKind::Local,
                local.name.span,
            ) {
                self.errors.push(e);
            }
        }
    }

    /// Extract phase from function annotations.
    fn extract_phase(annotations: &[FunctionAnnotation]) -> SymbolPhase {
        for ann in annotations {
            if let FunctionAnnotation::DeployTarget(target) = ann {
                return match target {
                    DeployTarget::RuntimeOnly => SymbolPhase::DeployOnly,
                    DeployTarget::CompileOnly => SymbolPhase::CompileOnly,
                    DeployTarget::Both => SymbolPhase::Both,
                };
            }
        }
        SymbolPhase::Both
    }

    /// Resolve references in a top-level form.
    fn resolve_form(&mut self, form: &TopLevelForm) {
        match form {
            TopLevelForm::Function(f) => {
                self.symbols.enter_scope(ScopeKind::Function);

                // Add parameters
                for param in &f.params {
                    if let Err(e) = self.symbols.define(
                        param.name.name.clone(),
                        SymbolKind::Parameter,
                        param.name.span,
                    ) {
                        self.errors.push(e);
                    }
                }

                // Resolve body
                for stmt in &f.body {
                    self.resolve_statement(stmt);
                }

                self.symbols.exit_scope();
            }
            TopLevelForm::Macro(m) => {
                self.symbols.enter_scope(ScopeKind::Function);

                for param in &m.params {
                    if let Err(e) = self.symbols.define(
                        param.name.name.clone(),
                        SymbolKind::Parameter,
                        param.name.span,
                    ) {
                        self.errors.push(e);
                    }
                }

                for stmt in &m.body {
                    self.resolve_statement(stmt);
                }

                self.symbols.exit_scope();
            }
            TopLevelForm::Module(module) => {
                self.symbols.enter_scope(ScopeKind::Module);

                // Handle imports
                for item in &module.items {
                    if let ModuleItem::Import(import) = item {
                        self.resolve_import(import);
                    }
                }

                // Resolve nested forms
                for item in &module.items {
                    if let ModuleItem::TopLevel(form) = item {
                        self.resolve_form(form);
                    }
                }

                self.symbols.exit_scope();
            }
            TopLevelForm::GlobalData(g) => {
                if let Some(init) = &g.init {
                    self.resolve_expression(init);
                }
            }
            TopLevelForm::CompileTimeBlock(block) => {
                for stmt in &block.body {
                    self.resolve_statement(stmt);
                }
            }
            _ => {}
        }
    }

    /// Resolve an import statement.
    fn resolve_import(&mut self, import: &ImportStmt) {
        // Look up the module
        let module_id = match self.symbols.get_module(&import.module.name) {
            Some(id) => id,
            None => {
                self.errors.push(Error::UndefinedVariable {
                    name: import.module.name.clone(),
                    span: import.module.span,
                });
                return;
            }
        };

        // Import specific items or all
        if let Some(items) = &import.items {
            for item in items {
                // Create an alias in current scope
                if let Err(e) = self.symbols.define(
                    item.name.clone(),
                    SymbolKind::Import,
                    item.span,
                ) {
                    self.errors.push(e);
                }
            }
        } else {
            // Import all public symbols from module
            // (Simplified: would need to track module exports)
            let _ = module_id;
        }
    }

    /// Resolve references in a statement.
    fn resolve_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Expression(expr) => {
                self.resolve_expression(expr);
            }
            Statement::LocalData(local) => {
                if let Some(init) = &local.init {
                    self.resolve_expression(init);
                }
                // Define after resolving init (no self-reference)
                if let Err(e) = self.symbols.define(
                    local.name.name.clone(),
                    SymbolKind::Local,
                    local.name.span,
                ) {
                    self.errors.push(e);
                }
            }
            Statement::Call(call) => {
                self.resolve_call(call);
            }
            Statement::Loop(loop_stmt) => {
                self.resolve_loop(loop_stmt);
            }
            Statement::Transaction(tx) => {
                self.symbols.enter_scope(ScopeKind::Block);
                for s in &tx.body {
                    self.resolve_statement(s);
                }
                self.symbols.exit_scope();
            }
            Statement::MetamorphicIf(mif) => {
                self.resolve_expression(&mif.condition);
                self.symbols.enter_scope(ScopeKind::Block);
                for s in &mif.then_branch {
                    self.resolve_statement(s);
                }
                self.symbols.exit_scope();
                if let Some(else_branch) = &mif.else_branch {
                    self.symbols.enter_scope(ScopeKind::Block);
                    for s in else_branch {
                        self.resolve_statement(s);
                    }
                    self.symbols.exit_scope();
                }
            }
            Statement::TryCatch(tc) => {
                self.symbols.enter_scope(ScopeKind::Block);
                for s in &tc.try_body {
                    self.resolve_statement(s);
                }
                self.symbols.exit_scope();

                for catch in &tc.catches {
                    self.symbols.enter_scope(ScopeKind::Block);
                    if let Err(e) = self.symbols.define(
                        catch.binding.name.clone(),
                        SymbolKind::Local,
                        catch.binding.span,
                    ) {
                        self.errors.push(e);
                    }
                    for s in &catch.body {
                        self.resolve_statement(s);
                    }
                    self.symbols.exit_scope();
                }

                if let Some(finally) = &tc.finally {
                    self.symbols.enter_scope(ScopeKind::Block);
                    for s in finally {
                        self.resolve_statement(s);
                    }
                    self.symbols.exit_scope();
                }
            }
            Statement::ConditionalAssembly(ca) => {
                self.symbols.enter_scope(ScopeKind::Block);
                for s in &ca.then_branch {
                    self.resolve_statement(s);
                }
                self.symbols.exit_scope();
                if let Some(else_branch) = &ca.else_branch {
                    self.symbols.enter_scope(ScopeKind::Block);
                    for s in else_branch {
                        self.resolve_statement(s);
                    }
                    self.symbols.exit_scope();
                }
            }
            Statement::DeployTimeBlock(block) => {
                self.symbols.enter_scope(ScopeKind::Block);
                for s in &block.body {
                    self.resolve_statement(s);
                }
                self.symbols.exit_scope();
            }
            _ => {}
        }
    }

    /// Resolve references in a loop.
    fn resolve_loop(&mut self, loop_stmt: &LoopStmt) {
        match loop_stmt {
            LoopStmt::While(w) => {
                self.resolve_expression(&w.condition);
                self.symbols.enter_scope(ScopeKind::Block);
                for s in &w.body {
                    self.resolve_statement(s);
                }
                self.symbols.exit_scope();
            }
            LoopStmt::For(f) => {
                self.resolve_expression(&f.iter);
                self.symbols.enter_scope(ScopeKind::Block);
                if let Err(e) = self.symbols.define(
                    f.var.name.clone(),
                    SymbolKind::LoopVar,
                    f.var.span,
                ) {
                    self.errors.push(e);
                }
                for s in &f.body {
                    self.resolve_statement(s);
                }
                self.symbols.exit_scope();
            }
            LoopStmt::BoundedFor(bf) => {
                self.resolve_expression(&bf.start);
                self.resolve_expression(&bf.end);
                self.symbols.enter_scope(ScopeKind::Block);
                if let Err(e) = self.symbols.define(
                    bf.var.name.clone(),
                    SymbolKind::LoopVar,
                    bf.var.span,
                ) {
                    self.errors.push(e);
                }
                for s in &bf.body {
                    self.resolve_statement(s);
                }
                self.symbols.exit_scope();
            }
        }
    }

    /// Resolve a function call.
    fn resolve_call(&mut self, call: &FunctionCall) {
        // Check if function exists
        if self.symbols.lookup(&call.name.name).is_none() {
            self.errors.push(Error::UndefinedVariable {
                name: call.name.name.clone(),
                span: call.name.span,
            });
        }

        // Resolve arguments
        for arg in &call.args {
            self.resolve_expression(arg);
        }
    }

    /// Resolve references in an expression.
    fn resolve_expression(&mut self, expr: &Expression) {
        match expr {
            Expression::Variable(var) => {
                if self.symbols.lookup(&var.name).is_none() {
                    self.errors.push(Error::UndefinedVariable {
                        name: var.name.clone(),
                        span: var.span,
                    });
                }
            }
            Expression::Call(call) => {
                self.resolve_call(call);
            }
            Expression::Arithmetic(arith) => {
                self.resolve_expression(&arith.left);
                self.resolve_expression(&arith.right);
            }
            Expression::Comparison(cmp) => {
                self.resolve_expression(&cmp.left);
                self.resolve_expression(&cmp.right);
            }
            Expression::Logical(log) => {
                for operand in &log.operands {
                    self.resolve_expression(operand);
                }
            }
            Expression::Memory(mem) => {
                self.resolve_expression(&mem.base);
                if let Some(offset) = &mem.offset {
                    self.resolve_expression(offset);
                }
            }
            Expression::Capability(cap) => {
                for arg in &cap.args {
                    self.resolve_expression(arg);
                }
            }
            Expression::Literal(_) => {}
        }
    }

    /// Get collected errors.
    pub fn errors(&self) -> &[Error] {
        &self.errors
    }

    /// Take the completed symbol table.
    pub fn into_symbol_table(self) -> SymbolTable {
        self.symbols
    }
}

impl Default for NameResolver {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol_table_basic() {
        let mut table = SymbolTable::new();

        let id = table
            .define("foo".to_string(), SymbolKind::Function, Span::default())
            .unwrap();

        assert!(table.lookup("foo").is_some());
        assert_eq!(table.lookup("foo"), Some(id));
        assert!(table.lookup("bar").is_none());
    }

    #[test]
    fn test_symbol_table_scopes() {
        let mut table = SymbolTable::new();

        table
            .define("outer".to_string(), SymbolKind::Local, Span::default())
            .unwrap();

        table.enter_scope(ScopeKind::Block);
        table
            .define("inner".to_string(), SymbolKind::Local, Span::default())
            .unwrap();

        // Can see both in inner scope
        assert!(table.lookup("outer").is_some());
        assert!(table.lookup("inner").is_some());

        table.exit_scope();

        // Can only see outer after exiting
        assert!(table.lookup("outer").is_some());
        assert!(table.lookup("inner").is_none());
    }

    #[test]
    fn test_symbol_table_duplicate_error() {
        let mut table = SymbolTable::new();

        table
            .define("foo".to_string(), SymbolKind::Function, Span::default())
            .unwrap();

        let result = table.define("foo".to_string(), SymbolKind::Function, Span::default());
        assert!(result.is_err());
    }

    #[test]
    fn test_symbol_table_shadow() {
        let mut table = SymbolTable::new();

        let outer_id = table
            .define("x".to_string(), SymbolKind::Local, Span::default())
            .unwrap();

        table.enter_scope(ScopeKind::Block);
        let inner_id = table
            .define("x".to_string(), SymbolKind::Local, Span::default())
            .unwrap();

        // Inner shadows outer
        assert_eq!(table.lookup("x"), Some(inner_id));

        table.exit_scope();

        // Outer is visible again
        assert_eq!(table.lookup("x"), Some(outer_id));
    }

    #[test]
    fn test_symbol_visibility() {
        let mut table = SymbolTable::new();

        let id = table
            .define_full(
                "public_fn".to_string(),
                SymbolKind::Function,
                Visibility::Public,
                SymbolPhase::Both,
                Span::default(),
            )
            .unwrap();

        let sym = table.get(id).unwrap();
        assert_eq!(sym.visibility, Visibility::Public);
    }
}
