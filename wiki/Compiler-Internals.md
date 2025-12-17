# Compiler Internals

This document describes the architecture and implementation of the Oblíbený compiler (`oblc`).

---

## Table of Contents

1. [Architecture Overview](#architecture-overview)
2. [Compilation Pipeline](#compilation-pipeline)
3. [Lexer](#lexer)
4. [Parser](#parser)
5. [Semantic Analysis](#semantic-analysis)
6. [Type Checker](#type-checker)
7. [Phase Validator](#phase-validator)
8. [Termination Checker](#termination-checker)
9. [Resource Analyzer](#resource-analyzer)
10. [Code Generation](#code-generation)
11. [Obfuscation](#obfuscation)

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                      OBLÍBENÝ COMPILER                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Source      ┌─────────┐      ┌─────────┐      ┌─────────┐     │
│  (.obl) ────▶│  Lexer  │─────▶│ Parser  │─────▶│   AST   │     │
│              └─────────┘      └─────────┘      └────┬────┘     │
│                                                      │          │
│              ┌──────────────────────────────────────┘          │
│              ▼                                                  │
│  ┌───────────────────────────────────────────────────────────┐ │
│  │                    SEMANTIC ANALYSIS                       │ │
│  │  ┌───────────┐ ┌───────────┐ ┌────────────┐ ┌──────────┐ │ │
│  │  │   Name    │ │   Type    │ │   Phase    │ │ Termina- │ │ │
│  │  │Resolution │ │  Checker  │ │ Validator  │ │  tion    │ │ │
│  │  └───────────┘ └───────────┘ └────────────┘ └──────────┘ │ │
│  │  ┌───────────┐ ┌───────────┐ ┌────────────┐              │ │
│  │  │  Call     │ │ Resource  │ │ Capability │              │ │
│  │  │  Graph    │ │ Analyzer  │ │  Checker   │              │ │
│  │  └───────────┘ └───────────┘ └────────────┘              │ │
│  └───────────────────────────────────────────────────────────┘ │
│              │                                                  │
│              ▼                                                  │
│  ┌───────────────────────────────────────────────────────────┐ │
│  │                    CODE GENERATION                         │ │
│  │  ┌───────────┐ ┌───────────┐ ┌────────────┐ ┌──────────┐ │ │
│  │  │   HIR     │ │   MIR     │ │    LIR     │ │  Target  │ │ │
│  │  │ (desugar) │ │   (CFG)   │ │  (linear)  │ │   Code   │ │ │
│  │  └───────────┘ └───────────┘ └────────────┘ └──────────┘ │ │
│  │  ┌───────────┐ ┌───────────┐                             │ │
│  │  │Optimiza-  │ │Obfusca-   │                             │ │
│  │  │  tion     │ │  tion     │                             │ │
│  │  └───────────┘ └───────────┘                             │ │
│  └───────────────────────────────────────────────────────────┘ │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Compilation Pipeline

### Three-Stage Model

| Stage | Name | Description |
|-------|------|-------------|
| 0 | Expansion | Macro expansion, compile-time execution |
| 1 | Verification | Type checking, phase validation, termination proofs |
| 2 | Generation | Optimization, obfuscation, code generation |

### Stage 0: Expansion (Turing-Complete)

```rust
// Pseudo-code for expansion stage
fn expand(ast: AST) -> AST {
    loop {
        let macros = find_macro_invocations(&ast);
        if macros.is_empty() {
            break;
        }
        for macro_call in macros {
            let expansion = execute_macro(macro_call);
            ast = substitute(ast, macro_call, expansion);
        }
    }
    execute_compile_time_blocks(&mut ast);
    ast
}
```

### Stage 1: Verification (Critical)

```rust
fn verify(ast: &AST) -> Result<VerifiedAST, Errors> {
    let symbols = resolve_names(ast)?;
    let types = type_check(ast, &symbols)?;
    let call_graph = build_call_graph(ast)?;

    validate_phases(ast, &call_graph)?;
    check_termination(ast, &call_graph)?;
    analyze_resources(ast, &call_graph)?;
    check_capabilities(ast)?;

    Ok(VerifiedAST { ast, symbols, types, call_graph })
}
```

### Stage 2: Generation (Turing-Incomplete Output)

```rust
fn generate(verified: VerifiedAST, config: &Config) -> Binary {
    let hir = lower_to_hir(verified.ast);
    let mir = lower_to_mir(hir);

    let mir = optimize(mir, config.opt_level);
    let mir = obfuscate(mir, config.obfuscation);

    let lir = lower_to_lir(mir, config.target);
    emit_code(lir, config.target)
}
```

---

## Lexer

The lexer (`src/lexer.rs`) tokenizes source code using the Logos library.

### Token Types

```rust
#[derive(Logos, Debug, Clone, PartialEq)]
pub enum Token {
    // Delimiters
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,

    // Keywords
    #[token("defun")]
    Defun,
    #[token("defmacro")]
    Defmacro,
    #[token("if")]
    If,
    #[token("let")]
    Let,
    #[token("bounded-for")]
    BoundedFor,
    // ... more keywords

    // Literals
    #[regex(r"-?[0-9]+", parse_integer)]
    Integer(i64),
    #[regex(r"-?[0-9]+\.[0-9]+", parse_float)]
    Float(f64),
    #[regex(r#""[^"]*""#, parse_string)]
    String(String),

    // Identifiers
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_-]*", |lex| lex.slice().to_string())]
    Identifier(String),

    // Comments
    #[regex(r";[^\n]*", logos::skip)]
    Comment,

    // Whitespace
    #[regex(r"[ \t\n\r]+", logos::skip)]
    Whitespace,
}
```

### Span Tracking

```rust
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn merge(&self, other: &Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}
```

---

## Parser

The parser (`src/parser.rs`) implements a recursive descent parser for S-expressions.

### Core Parser Structure

```rust
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current: Option<Token>,
    span: Span,
}

impl<'a> Parser<'a> {
    pub fn parse_program(&mut self) -> Result<Vec<TopLevelForm>> {
        let mut forms = Vec::new();
        while self.current.is_some() {
            forms.push(self.parse_top_level()?);
        }
        Ok(forms)
    }

    fn parse_top_level(&mut self) -> Result<TopLevelForm> {
        self.expect(Token::LParen)?;
        match self.current {
            Some(Token::Defun) => self.parse_function(),
            Some(Token::Defmacro) => self.parse_macro(),
            Some(Token::Module) => self.parse_module(),
            Some(Token::Deployment) => self.parse_deployment(),
            // ... more forms
            _ => self.parse_expression(),
        }
    }

    fn parse_function(&mut self) -> Result<TopLevelForm> {
        self.advance(); // consume 'defun'
        let name = self.parse_identifier()?;
        let params = self.parse_param_list()?;
        let annotations = self.parse_annotations()?;
        let body = self.parse_body()?;
        self.expect(Token::RParen)?;

        Ok(TopLevelForm::Function(FunctionDef {
            name,
            params,
            annotations,
            body,
            span: self.span,
        }))
    }
}
```

### AST Types

```rust
pub enum TopLevelForm {
    Function(FunctionDef),
    Macro(MacroDef),
    Module(ModuleDef),
    GlobalData(GlobalDataDef),
    DeploymentSpec(DeploymentSpec),
    CompileTimeBlock(CompileTimeBlock),
}

pub struct FunctionDef {
    pub name: Identifier,
    pub params: Vec<Parameter>,
    pub return_type: Option<TypeExpr>,
    pub annotations: Vec<FunctionAnnotation>,
    pub body: Vec<Statement>,
    pub span: Span,
}

pub enum Statement {
    Expression(Expression),
    LocalData(LocalDataDef),
    Loop(LoopStmt),
    Call(FunctionCall),
    // ... more
}
```

---

## Semantic Analysis

### Name Resolution

```rust
pub struct SymbolTable {
    scopes: Vec<HashMap<String, Symbol>>,
}

impl SymbolTable {
    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(sym) = scope.get(name) {
                return Some(sym);
            }
        }
        None
    }

    pub fn define(&mut self, name: String, symbol: Symbol) -> Result<()> {
        let scope = self.scopes.last_mut().unwrap();
        if scope.contains_key(&name) {
            return Err(Error::DuplicateDefinition { name });
        }
        scope.insert(name, symbol);
        Ok(())
    }
}
```

---

## Type Checker

The type checker (`src/typeck.rs`) implements bidirectional type checking.

### Type Representation

```rust
pub enum Type {
    Unknown,
    Unit,
    Primitive(PrimitiveType),
    Array { elem: Box<Type>, size: u64 },
    Pointer(Box<Type>),
    Struct(Vec<(String, Type)>),
    Enum(Vec<String>),
    Function(Box<FunctionSig>),
    User(String),
    Error,
}

pub struct FunctionSig {
    pub params: Vec<Type>,
    pub return_type: Type,
    pub is_pure: bool,
}
```

### Type Checking Algorithm

```rust
impl TypeChecker {
    pub fn check_function(&mut self, f: &FunctionDef) -> Result<()> {
        self.env.push_scope();

        // In deploy-time, all params must be typed
        for param in &f.params {
            let ty = if let Some(t) = &param.ty {
                self.type_from_expr(t)
            } else if self.is_deploy_time() {
                return Err(Error::MissingTypeAnnotation { span: param.span });
            } else {
                Type::Unknown
            };
            self.env.define(&param.name, ty);
        }

        for stmt in &f.body {
            self.check_statement(stmt)?;
        }

        self.env.pop_scope();
        Ok(())
    }

    fn infer_expr(&mut self, expr: &Expression) -> Type {
        match expr {
            Expression::Literal(lit) => self.infer_literal(lit),
            Expression::Variable(var) => self.lookup(&var.name),
            Expression::Call(call) => self.check_call(call),
            Expression::Arithmetic(arith) => {
                let left = self.infer_expr(&arith.left);
                let right = self.infer_expr(&arith.right);
                self.unify_numeric(&left, &right)
            }
            // ... more
        }
    }
}
```

---

## Phase Validator

The phase validator (`src/phase.rs`) enforces compile-time vs deploy-time separation.

### Phase Rules

```rust
pub enum Phase {
    CompileTime,
    DeployTime,
}

impl PhaseValidator {
    pub fn validate_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Loop(LoopStmt::While(w)) if self.phase == Phase::DeployTime => {
                self.error(PhaseViolation {
                    construct: "while".into(),
                    phase: Phase::DeployTime,
                    span: w.span,
                    reason: "unbounded loops forbidden in deploy-time".into(),
                });
            }
            Statement::Loop(LoopStmt::BoundedFor(_)) => {
                // OK in both phases
            }
            Statement::Instruction(Instruction::System(_)) if self.phase == Phase::DeployTime => {
                self.error(PhaseViolation {
                    construct: "syscall".into(),
                    phase: Phase::DeployTime,
                    span: stmt.span(),
                    reason: "syscalls forbidden in deploy-time".into(),
                });
            }
            // ... more rules
        }
    }
}
```

---

## Termination Checker

The termination checker (`src/termination.rs`) proves that deploy-time code terminates.

### Call Graph Construction

```rust
pub struct CallGraph {
    graph: DiGraph<FunctionNode, CallEdge>,
    name_to_node: HashMap<String, NodeIndex>,
}

impl CallGraph {
    pub fn find_cycles(&self) -> Vec<Cycle> {
        let sccs = tarjan_scc(&self.graph);
        sccs.into_iter()
            .filter(|scc| scc.len() > 1 || self.has_self_edge(scc[0]))
            .map(|scc| Cycle { nodes: scc })
            .collect()
    }
}
```

### Termination Proof

```rust
impl TerminationChecker {
    pub fn prove_termination(&self, f: &FunctionDef) -> TerminationProof {
        // Check for recursion
        if self.call_graph.is_in_cycle(&f.name) {
            return TerminationProof::Unprovable {
                reason: "recursion detected".into(),
            };
        }

        // Check all loops are bounded
        let mut max_iterations = 0u64;
        for stmt in &f.body {
            match self.check_loop_bounds(stmt) {
                Some(bound) => max_iterations = max_iterations.saturating_add(bound),
                None => return TerminationProof::Unprovable {
                    reason: "unbounded loop".into(),
                },
            }
        }

        TerminationProof::Terminates {
            strategy: ProofStrategy::BoundedLoop,
            bound: Some(max_iterations),
        }
    }
}
```

---

## Resource Analyzer

The resource analyzer (`src/resources.rs`) computes static resource bounds.

### Resource Computation

```rust
impl ResourceAnalyzer {
    pub fn analyze_function(&self, f: &FunctionDef) -> ResourceUsage {
        ResourceUsage {
            iterations: self.count_iterations(&f.body),
            stack_depth: self.count_stack_slots(&f.params, &f.body),
            memory: self.estimate_memory(&f.body),
            call_depth: self.call_graph.max_depth(&f.name),
            instructions: self.estimate_instructions(&f.body),
        }
    }

    fn count_iterations(&self, stmts: &[Statement]) -> Option<u64> {
        let mut total = 0u64;
        for stmt in stmts {
            if let Statement::Loop(LoopStmt::BoundedFor(bf)) = stmt {
                let bound = self.eval_const(&bf.end)? - self.eval_const(&bf.start)?;
                let inner = self.count_iterations(&bf.body)?;
                total = total.checked_add(bound as u64 * (1 + inner))?;
            }
        }
        Some(total)
    }
}
```

---

## Code Generation

### Intermediate Representations

```
AST → HIR → MIR → LIR → Machine Code
```

| IR | Purpose |
|----|---------|
| HIR | Desugared AST, explicit types |
| MIR | Control flow graph, SSA form |
| LIR | Linear, register-allocated |

### HIR (High-Level IR)

```rust
pub enum HirExpr {
    Literal(Literal, Type),
    Variable(String, Type),
    Call { func: String, args: Vec<HirExpr>, ret_type: Type },
    BinaryOp { op: BinOp, left: Box<HirExpr>, right: Box<HirExpr>, ty: Type },
    // No macros, no syntactic sugar
}
```

### MIR (Mid-Level IR)

```rust
pub struct MirFunction {
    pub name: String,
    pub blocks: Vec<BasicBlock>,
    pub locals: Vec<Local>,
}

pub struct BasicBlock {
    pub id: BlockId,
    pub statements: Vec<MirStatement>,
    pub terminator: Terminator,
}

pub enum Terminator {
    Return(Option<Operand>),
    Goto(BlockId),
    Branch { cond: Operand, then_block: BlockId, else_block: BlockId },
    Call { func: String, args: Vec<Operand>, dest: Local, next: BlockId },
}
```

---

## Obfuscation

### Obfuscation Levels

| Level | Transformations |
|-------|-----------------|
| None | No obfuscation |
| Minimal | Name mangling |
| Aggressive | + Control flow flattening |
| Paranoid | + Opaque predicates + instruction substitution |

### Semantic Obfuscation

```rust
impl Obfuscator {
    pub fn obfuscate(&mut self, mir: Mir) -> Mir {
        match self.level {
            Level::None => mir,
            Level::Minimal => self.mangle_names(mir),
            Level::Aggressive => {
                let mir = self.mangle_names(mir);
                let mir = self.flatten_control_flow(mir);
                mir
            }
            Level::Paranoid => {
                let mir = self.mangle_names(mir);
                let mir = self.flatten_control_flow(mir);
                let mir = self.insert_opaque_predicates(mir);
                let mir = self.substitute_instructions(mir);
                mir
            }
        }
    }

    fn flatten_control_flow(&mut self, mir: Mir) -> Mir {
        // Convert structured control flow to switch-based dispatch
        // while preserving semantics
    }
}
```

---

## Source Files

| File | Purpose |
|------|---------|
| `src/lib.rs` | Library entry point |
| `src/main.rs` | CLI entry point |
| `src/lexer.rs` | Tokenization |
| `src/parser.rs` | Parsing |
| `src/ast.rs` | AST types |
| `src/span.rs` | Source locations |
| `src/error.rs` | Error types |
| `src/token.rs` | Token definitions |
| `src/typeck.rs` | Type checking |
| `src/phase.rs` | Phase validation |
| `src/termination.rs` | Termination analysis |
| `src/resources.rs` | Resource analysis |
| `src/callgraph.rs` | Call graph construction |

---

## Next Steps

- [IR Design](IR-Design.md)
- [Optimization Passes](Optimization-Passes.md)
- [Code Generation](Code-Generation.md)
