// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Compiler driver for Oblíbený.
//!
//! This module provides the main compilation pipeline that wires together
//! all compiler passes from source to executable.
//!
//! # Pipeline
//!
//! ```text
//! Source → Lexer → Parser → AST
//!                            ↓
//!                      Macro Expansion
//!                            ↓
//!                      Name Resolution
//!                            ↓
//!                       Type Checking
//!                            ↓
//!                     Phase Validation
//!                            ↓
//!                   Termination Checking
//!                            ↓
//!                    Resource Analysis
//!                            ↓
//!                      HIR Lowering
//!                            ↓
//!                      MIR Lowering
//!                            ↓
//!                      Optimization
//!                            ↓
//!                     Code Generation
//! ```

use crate::ast::TopLevelForm;
use crate::callgraph::CallGraph;
use crate::codegen::{CodeGenerator, Target};
use crate::error::{Error, Result};
use crate::hir::HirProgram;
use crate::lexer::Lexer;
use crate::lower::lower;
use crate::macro_expand::MacroExpander;
use crate::mir::MirProgram;
use crate::opt::{optimize, OptLevel};
use crate::parser::Parser;
use crate::phase::PhaseValidator;
use crate::resources::ResourceAnalyzer;
use crate::symbol::{NameResolver, SymbolTable};
use crate::termination::TerminationChecker;
use crate::typeck::TypeChecker;
use std::path::Path;

/// Compilation options
#[derive(Debug, Clone)]
pub struct CompileOptions {
    /// Target architecture
    pub target: Target,
    /// Optimization level
    pub opt_level: OptLevel,
    /// Whether to emit debug info
    pub debug_info: bool,
    /// Whether to verify termination
    pub verify_termination: bool,
    /// Whether to verify resource bounds
    pub verify_resources: bool,
    /// Output path
    pub output: Option<String>,
}

impl Default for CompileOptions {
    fn default() -> Self {
        CompileOptions {
            target: Target::X86_64,
            opt_level: OptLevel::Standard,
            debug_info: false,
            verify_termination: true,
            verify_resources: true,
            output: None,
        }
    }
}

/// Compilation result containing all intermediate representations
#[derive(Debug)]
pub struct CompileResult {
    /// The AST after macro expansion
    pub ast: Vec<TopLevelForm>,
    /// Symbol table from name resolution
    pub symbols: SymbolTable,
    /// High-level IR
    pub hir: HirProgram,
    /// Mid-level IR
    pub mir: MirProgram,
    /// Generated assembly code
    pub assembly: String,
    /// Any warnings generated during compilation
    pub warnings: Vec<String>,
}

/// The compiler driver
pub struct Compiler {
    /// Compilation options
    options: CompileOptions,
    /// Accumulated warnings
    warnings: Vec<String>,
}

impl Compiler {
    /// Create a new compiler with default options
    pub fn new() -> Self {
        Compiler {
            options: CompileOptions::default(),
            warnings: Vec::new(),
        }
    }

    /// Create a new compiler with the given options
    pub fn with_options(options: CompileOptions) -> Self {
        Compiler {
            options,
            warnings: Vec::new(),
        }
    }

    /// Compile source code to assembly
    pub fn compile(&mut self, source: &str) -> Result<CompileResult> {
        self.warnings.clear();

        // Stage 0: Lexing and Parsing
        let ast = self.parse(source)?;

        // Stage 0.5: Macro Expansion
        let ast = self.expand_macros(ast)?;

        // Stage 1: Semantic Analysis
        let symbols = self.resolve_names(&ast)?;
        self.type_check(&ast)?;
        self.validate_phases(&ast)?;

        if self.options.verify_termination {
            self.check_termination(&ast)?;
        }

        if self.options.verify_resources {
            self.analyze_resources(&ast)?;
        }

        // Stage 2: IR Generation
        let hir = self.lower_to_hir(&ast)?;
        let mir = self.lower_to_mir(&hir)?;

        // Stage 2.5: Optimization
        let mir = self.optimize(mir);

        // Stage 3: Code Generation
        let assembly = self.generate_code(&mir)?;

        Ok(CompileResult {
            ast,
            symbols,
            hir,
            mir,
            assembly,
            warnings: std::mem::take(&mut self.warnings),
        })
    }

    /// Parse source code into an AST
    pub fn parse(&mut self, source: &str) -> Result<Vec<TopLevelForm>> {
        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer);
        parser.parse_program()
    }

    /// Expand macros in the AST
    pub fn expand_macros(&mut self, ast: Vec<TopLevelForm>) -> Result<Vec<TopLevelForm>> {
        let mut expander = MacroExpander::new();
        expander.expand_program(ast)
    }

    /// Resolve names and build symbol table
    pub fn resolve_names(&mut self, ast: &[TopLevelForm]) -> Result<SymbolTable> {
        let mut resolver = NameResolver::new();
        resolver.resolve_program(ast)?;
        Ok(resolver.into_symbol_table())
    }

    /// Type check the AST
    pub fn type_check(&mut self, ast: &[TopLevelForm]) -> Result<()> {
        let mut checker = TypeChecker::new();
        for form in ast {
            checker.check_top_level(form);
        }
        // Check for errors from the type checker
        let errors = checker.get_errors();
        if !errors.is_empty() {
            return Err(Error::TypeMismatch {
                expected: "type-correct program".to_string(),
                actual: format!("{} type error(s)", errors.len()),
                span: crate::span::Span::new(0, 0),
            });
        }
        Ok(())
    }

    /// Validate phase separation (compile-time vs deploy-time)
    pub fn validate_phases(&mut self, ast: &[TopLevelForm]) -> Result<()> {
        let mut validator = PhaseValidator::new();
        validator.validate_program(ast)
    }

    /// Check termination of deploy-time code
    pub fn check_termination(&mut self, ast: &[TopLevelForm]) -> Result<()> {
        let call_graph = CallGraph::from_program(ast);
        let mut checker = TerminationChecker::new(call_graph);
        checker.check_program(ast)
    }

    /// Analyze resource usage
    pub fn analyze_resources(&mut self, ast: &[TopLevelForm]) -> Result<()> {
        let call_graph = CallGraph::from_program(ast);
        let mut analyzer = ResourceAnalyzer::new(call_graph);
        analyzer.analyze_program(ast)?;

        // Report any resource violations as warnings for now
        for violation in analyzer.violations() {
            self.warnings.push(format!("Resource violation: {:?}", violation));
        }

        Ok(())
    }

    /// Lower AST to HIR
    pub fn lower_to_hir(&mut self, ast: &[TopLevelForm]) -> Result<HirProgram> {
        lower(ast)
    }

    /// Lower HIR to MIR
    pub fn lower_to_mir(&mut self, hir: &HirProgram) -> Result<MirProgram> {
        use crate::hir::HirItem;
        use crate::symbol::SymbolPhase;

        let mut mir = MirProgram::new();

        // For each HIR function, create a MIR function
        for item in &hir.items {
            if let HirItem::Function(hir_func) = item {
                let is_deploy = matches!(hir_func.phase, SymbolPhase::DeployOnly | SymbolPhase::Both);
                let mir_func = crate::mir::MirFunction::new(
                    hir_func.id,
                    hir_func.name.clone(),
                    hir_func
                        .params
                        .iter()
                        .map(|p| crate::mir::MirType::from_hir(&p.ty))
                        .collect(),
                    crate::mir::MirType::from_hir(&hir_func.return_type),
                    is_deploy,
                    hir_func.span,
                );

                // TODO: Full HIR to MIR lowering with CFG construction
                // For now, create a minimal function
                mir.functions.push(mir_func);
            }
        }

        Ok(mir)
    }

    /// Optimize MIR
    pub fn optimize(&mut self, mut mir: MirProgram) -> MirProgram {
        for func in &mut mir.functions {
            optimize(func, self.options.opt_level);
        }
        mir
    }

    /// Generate code from MIR
    pub fn generate_code(&mut self, mir: &MirProgram) -> Result<String> {
        let mut codegen = CodeGenerator::new(self.options.target);
        Ok(codegen.generate(mir))
    }

    /// Compile a file
    pub fn compile_file(&mut self, path: &Path) -> Result<CompileResult> {
        let source = std::fs::read_to_string(path).map_err(|e| Error::IoError {
            message: format!("Failed to read file: {}", e),
        })?;
        self.compile(&source)
    }

    /// Get accumulated warnings
    pub fn warnings(&self) -> &[String] {
        &self.warnings
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

/// Quick compilation function for simple use cases
pub fn compile(source: &str) -> Result<String> {
    let mut compiler = Compiler::new();
    let result = compiler.compile(source)?;
    Ok(result.assembly)
}

/// Check source code without generating code
pub fn check(source: &str) -> Result<()> {
    let mut compiler = Compiler::new();

    let ast = compiler.parse(source)?;
    let ast = compiler.expand_macros(ast)?;
    compiler.resolve_names(&ast)?;
    compiler.type_check(&ast)?;
    compiler.validate_phases(&ast)?;
    compiler.check_termination(&ast)?;
    compiler.analyze_resources(&ast)?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile_empty() {
        let mut compiler = Compiler::new();
        let result = compiler.compile("");
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_simple_function() {
        // Use untyped syntax that the parser accepts
        let source = "(defun plus (a b) (+ a b))";
        let mut compiler = Compiler::new();
        let result = compiler.compile(source);
        if let Err(e) = &result {
            eprintln!("Compilation error: {:?}", e);
        }
        assert!(result.is_ok());
    }

    #[test]
    fn test_check_source() {
        // Use untyped syntax that the parser accepts
        let source = "(defun id (x) x)";
        let result = check(source);
        if let Err(e) = &result {
            eprintln!("Check error: {:?}", e);
        }
        assert!(result.is_ok());
    }
}
