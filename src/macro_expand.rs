// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Macro expansion system for Oblíbený.
//!
//! This module implements the macro expansion phase (Stage 0) of compilation.
//! Macros in Oblíbený are compile-time code transformations that operate on
//! the AST before any semantic analysis.
//!
//! # Macro Types
//!
//! - **Function-like macros**: `(macro name (params...) body)`
//! - **Reader macros**: Custom syntax extensions
//! - **Hygiene**: Lexical scoping to prevent variable capture
//!
//! # Status
//!
//! This is a skeleton implementation. Full macro expansion support is
//! planned for Phase 1 milestone 2.

use crate::ast::{MacroDef, Statement, TopLevelForm};
use crate::error::Result;
use crate::span::Span;
use std::collections::HashMap;

/// Macro expander for Oblíbený programs
pub struct MacroExpander {
    /// Defined macros
    macros: HashMap<String, MacroDefinition>,
    /// Gensym counter for hygienic expansion
    gensym_counter: u32,
    /// Maximum expansion depth (to prevent infinite recursion)
    max_depth: u32,
}

/// A macro definition
#[derive(Debug, Clone)]
pub struct MacroDefinition {
    /// Macro name
    pub name: String,
    /// Parameter names
    pub params: Vec<String>,
    /// Macro body (template)
    pub body: Vec<Statement>,
    /// Source span
    pub span: Span,
    /// Whether this is a variadic macro
    pub variadic: bool,
}

impl MacroExpander {
    /// Create a new macro expander
    pub fn new() -> Self {
        MacroExpander {
            macros: HashMap::new(),
            gensym_counter: 0,
            max_depth: 256,
        }
    }

    /// Set the maximum expansion depth
    pub fn with_max_depth(mut self, depth: u32) -> Self {
        self.max_depth = depth;
        self
    }

    /// Generate a unique symbol for hygienic expansion
    pub fn gensym(&mut self, base: &str) -> String {
        let sym = format!("{}#g{}", base, self.gensym_counter);
        self.gensym_counter += 1;
        sym
    }

    /// Register a macro definition
    pub fn define_macro(&mut self, def: MacroDefinition) {
        self.macros.insert(def.name.clone(), def);
    }

    /// Check if a name is a defined macro
    pub fn is_macro(&self, name: &str) -> bool {
        self.macros.contains_key(name)
    }

    /// Get a macro definition
    pub fn get_macro(&self, name: &str) -> Option<&MacroDefinition> {
        self.macros.get(name)
    }

    /// Expand all macros in a program
    ///
    /// This is a stub implementation that:
    /// 1. Collects macro definitions
    /// 2. Returns the program with macros removed (but no expansion)
    ///
    /// Full macro expansion will be implemented in a future milestone.
    pub fn expand_program(&mut self, forms: Vec<TopLevelForm>) -> Result<Vec<TopLevelForm>> {
        // First pass: collect macro definitions
        for form in &forms {
            if let TopLevelForm::Macro(macro_def) = form {
                self.collect_macro_def(macro_def);
            }
        }

        // Second pass: filter out macro definitions, return rest unchanged
        // (In the full implementation, we'd expand macro invocations here)
        let expanded = forms
            .into_iter()
            .filter(|form| !matches!(form, TopLevelForm::Macro(_)))
            .collect();

        Ok(expanded)
    }

    /// Collect a macro definition
    fn collect_macro_def(&mut self, macro_def: &MacroDef) {
        let params: Vec<String> = macro_def
            .params
            .iter()
            .map(|p| p.name.name.clone())
            .collect();

        // Check for variadic parameter (last param starts with &)
        let variadic = params.last().map_or(false, |p| p.starts_with('&'));

        let def = MacroDefinition {
            name: macro_def.name.name.clone(),
            params,
            body: macro_def.body.clone(),
            span: macro_def.span,
            variadic,
        };

        self.macros.insert(def.name.clone(), def);
    }

    /// Get the number of defined macros
    pub fn macro_count(&self) -> usize {
        self.macros.len()
    }
}

impl Default for MacroExpander {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gensym() {
        let mut expander = MacroExpander::new();
        let s1 = expander.gensym("x");
        let s2 = expander.gensym("x");
        assert_ne!(s1, s2);
        assert!(s1.starts_with("x#g"));
    }

    #[test]
    fn test_expander_creation() {
        let expander = MacroExpander::new();
        assert_eq!(expander.macro_count(), 0);
    }

    #[test]
    fn test_expand_empty_program() {
        let mut expander = MacroExpander::new();
        let result = expander.expand_program(vec![]).unwrap();
        assert!(result.is_empty());
    }
}
