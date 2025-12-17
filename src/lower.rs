// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! AST to HIR lowering for Oblíbený.
//!
//! This module transforms the parsed AST into HIR by:
//! - Resolving names to symbol IDs
//! - Adding explicit type information
//! - Desugaring complex constructs
//! - Simplifying control flow

use crate::ast::*;
use crate::error::{Error, Result};
use crate::hir::*;
use crate::span::Span;
use crate::symbol::{NameResolver, ResolvedType, SymbolId, SymbolKind, SymbolPhase, SymbolTable};
use crate::typeck::{Type, TypeChecker};

/// Lowers AST to HIR.
pub struct Lowering {
    /// Symbol table.
    symbols: SymbolTable,
    /// Type checker (for type information).
    type_checker: TypeChecker,
    /// Collected errors.
    errors: Vec<Error>,
    /// Generated HIR items.
    items: Vec<HirItem>,
    /// Current function's local variable counter.
    local_counter: u32,
}

impl Lowering {
    /// Create a new lowering pass.
    pub fn new(symbols: SymbolTable) -> Self {
        Self {
            symbols,
            type_checker: TypeChecker::new(),
            errors: Vec::new(),
            items: Vec::new(),
            local_counter: 0,
        }
    }

    /// Lower a program from AST to HIR.
    pub fn lower_program(&mut self, program: &[TopLevelForm]) -> Result<HirProgram> {
        // Type check first
        if let Err(e) = self.type_checker.check_program(program) {
            return Err(e);
        }

        // Lower each top-level form
        for form in program {
            self.lower_top_level(form);
        }

        if !self.errors.is_empty() {
            return Err(self.errors.remove(0));
        }

        // Find entry point
        let entry = self.symbols.lookup("main");

        Ok(HirProgram {
            items: std::mem::take(&mut self.items),
            entry,
        })
    }

    /// Lower a top-level form.
    fn lower_top_level(&mut self, form: &TopLevelForm) {
        match form {
            TopLevelForm::Function(f) => {
                if let Some(hir_fn) = self.lower_function(f) {
                    self.items.push(HirItem::Function(hir_fn));
                }
            }
            TopLevelForm::GlobalData(g) => {
                if let Some(hir_global) = self.lower_global(g) {
                    self.items.push(HirItem::Global(hir_global));
                }
            }
            TopLevelForm::DeploymentSpec(spec) => {
                let hir_deploy = self.lower_deployment(spec);
                self.items.push(HirItem::Deployment(hir_deploy));
            }
            TopLevelForm::Module(module) => {
                // Lower module contents
                for item in &module.items {
                    if let ModuleItem::TopLevel(inner) = item {
                        self.lower_top_level(inner);
                    }
                }
            }
            TopLevelForm::CompileTimeBlock(block) => {
                // Compile-time blocks are evaluated during lowering
                // For now, just lower their contents
                for stmt in &block.body {
                    // Would execute compile-time code here
                    let _ = stmt;
                }
            }
            TopLevelForm::Macro(_) => {
                // Macros should be expanded before lowering
                // Skip for now
            }
            _ => {}
        }
    }

    /// Lower a function definition.
    fn lower_function(&mut self, f: &FunctionDef) -> Option<HirFunction> {
        self.local_counter = 0;

        // Look up function symbol
        let id = self.symbols.lookup(&f.name.name)?;

        // Extract phase from annotations
        let phase = Self::extract_phase(&f.annotations);
        let is_pure = f
            .annotations
            .iter()
            .any(|a| matches!(a, FunctionAnnotation::Pure));
        let max_iterations = Self::extract_max_iterations(&f.annotations);

        // Lower parameters
        let params = f
            .params
            .iter()
            .filter_map(|p| self.lower_param(p))
            .collect();

        // Lower return type
        let return_type = f
            .return_type
            .as_ref()
            .map(|t| self.lower_type(t))
            .unwrap_or(HirType::Unit);

        // Lower body
        let body = self.lower_block(&f.body, f.span);

        Some(HirFunction {
            id,
            name: f.name.name.clone(),
            params,
            return_type,
            body,
            phase,
            is_pure,
            max_iterations,
            span: f.span,
        })
    }

    /// Lower a parameter.
    fn lower_param(&mut self, p: &Parameter) -> Option<HirParam> {
        let id = self.symbols.lookup(&p.name.name)?;
        let ty = p
            .ty
            .as_ref()
            .map(|t| self.lower_type(t))
            .unwrap_or(HirType::Int(IntType::I64)); // Default to i64

        Some(HirParam {
            id,
            name: p.name.name.clone(),
            ty,
            span: p.span,
        })
    }

    /// Lower a global data definition.
    fn lower_global(&mut self, g: &GlobalDataDef) -> Option<HirGlobal> {
        let id = self.symbols.lookup(&g.name.name)?;
        let ty = self.lower_type(&g.ty);
        let init = g.init.as_ref().and_then(|e| self.try_eval_const(e));
        let phase = if g.deploy_visible {
            SymbolPhase::Both
        } else {
            SymbolPhase::CompileOnly
        };

        Some(HirGlobal {
            id,
            name: g.name.name.clone(),
            ty,
            init,
            is_mutable: true, // TODO: track mutability
            phase,
            span: g.span,
        })
    }

    /// Lower a deployment specification.
    fn lower_deployment(&self, spec: &DeploymentSpec) -> HirDeployment {
        HirDeployment {
            profile: spec.profile.clone(),
            bounds: spec.bounds.clone(),
            obfuscation: spec.obfuscation,
            span: spec.span,
        }
    }

    /// Lower a type expression.
    fn lower_type(&self, ty: &TypeExpr) -> HirType {
        match ty {
            TypeExpr::Primitive(p) => HirType::from_primitive(*p),
            TypeExpr::Array { elem, size } => {
                HirType::Array(Box::new(self.lower_type(elem)), *size)
            }
            TypeExpr::Pointer(inner) => HirType::Ptr(Box::new(self.lower_type(inner))),
            TypeExpr::Struct(fields) => {
                // Would need to create or look up struct type
                // For now, return error type
                HirType::Error
            }
            TypeExpr::Enum(variants) => {
                // Would need to create or look up enum type
                HirType::Error
            }
            TypeExpr::User(name) => {
                if let Some(id) = self.symbols.lookup(&name.name) {
                    if let Some(sym) = self.symbols.get(id) {
                        match sym.kind {
                            SymbolKind::Struct => return HirType::Struct(id),
                            SymbolKind::Enum => return HirType::Enum(id),
                            _ => {}
                        }
                    }
                }
                HirType::Error
            }
        }
    }

    /// Lower a block of statements.
    fn lower_block(&mut self, stmts: &[Statement], span: Span) -> HirBlock {
        let mut hir_stmts = Vec::new();
        let mut final_expr = None;

        for (i, stmt) in stmts.iter().enumerate() {
            let is_last = i == stmts.len() - 1;

            match stmt {
                Statement::Expression(expr) if is_last => {
                    // Last expression might be the block's value
                    final_expr = Some(Box::new(self.lower_expr(expr)));
                }
                _ => {
                    if let Some(hir_stmt) = self.lower_stmt(stmt) {
                        hir_stmts.push(hir_stmt);
                    }
                }
            }
        }

        HirBlock {
            stmts: hir_stmts,
            expr: final_expr,
            span,
        }
    }

    /// Lower a statement.
    fn lower_stmt(&mut self, stmt: &Statement) -> Option<HirStmt> {
        match stmt {
            Statement::Expression(expr) => Some(HirStmt::Expr(self.lower_expr(expr))),

            Statement::LocalData(local) => {
                let id = self.symbols.lookup(&local.name.name)?;
                let ty = local
                    .ty
                    .as_ref()
                    .map(|t| self.lower_type(t))
                    .unwrap_or_else(|| {
                        // Infer from init
                        local
                            .init
                            .as_ref()
                            .map(|e| self.infer_expr_type(e))
                            .unwrap_or(HirType::Int(IntType::I64))
                    });
                let init = local.init.as_ref().map(|e| self.lower_expr(e));

                Some(HirStmt::Let {
                    id,
                    name: local.name.name.clone(),
                    ty,
                    init,
                    span: local.span,
                })
            }

            Statement::Call(call) => {
                let hir_call = self.lower_call(call);
                Some(HirStmt::Expr(hir_call))
            }

            Statement::Loop(loop_stmt) => self.lower_loop(loop_stmt),

            Statement::Transaction(tx) => {
                // Lower transaction as a block (simplified)
                let block = self.lower_block(&tx.body, tx.span);
                // Return the block's statements
                Some(HirStmt::Expr(HirExpr::Const(
                    HirConstValue::Unit,
                    HirType::Unit,
                    tx.span,
                )))
            }

            Statement::MetamorphicIf(mif) => {
                let cond = self.lower_expr(&mif.condition);
                let then_block = self.lower_block(&mif.then_branch, mif.span);
                let else_block = mif.else_branch.as_ref().map(|b| self.lower_block(b, mif.span));

                Some(HirStmt::If {
                    cond,
                    then_block,
                    else_block,
                    span: mif.span,
                })
            }

            Statement::TryCatch(tc) => {
                // Lower try-catch as sequential blocks (simplified)
                // Would need proper exception handling in a real implementation
                let try_block = self.lower_block(&tc.try_body, tc.span);
                Some(HirStmt::Expr(HirExpr::Const(
                    HirConstValue::Unit,
                    HirType::Unit,
                    tc.span,
                )))
            }

            Statement::ConditionalAssembly(ca) => {
                // Lower conditional assembly
                let then_block = self.lower_block(&ca.then_branch, ca.span);
                let else_block = ca.else_branch.as_ref().map(|b| self.lower_block(b, ca.span));

                Some(HirStmt::If {
                    cond: HirExpr::Const(
                        HirConstValue::Bool(true), // Would evaluate condition
                        HirType::Bool,
                        ca.span,
                    ),
                    then_block,
                    else_block,
                    span: ca.span,
                })
            }

            Statement::DeployTimeBlock(block) => {
                let hir_block = self.lower_block(&block.body, block.span);
                // Return as expression
                Some(HirStmt::Expr(
                    hir_block
                        .expr
                        .map(|e| *e)
                        .unwrap_or(HirExpr::Const(HirConstValue::Unit, HirType::Unit, block.span)),
                ))
            }

            Statement::Label(label) => Some(HirStmt::Label {
                name: label.name.clone(),
                span: label.span,
            }),

            Statement::Instruction(instr) => {
                let hir_asm = self.lower_instruction(instr);
                Some(HirStmt::Asm(hir_asm))
            }

            Statement::DataDirective(dd) => {
                let values = dd
                    .values
                    .iter()
                    .filter_map(|v| self.lower_data_value(v))
                    .collect();
                Some(HirStmt::Asm(HirAsm::Data {
                    size: dd.size,
                    label: dd.name.name.clone(),
                    values,
                    span: dd.span,
                }))
            }
        }
    }

    /// Lower a loop statement.
    fn lower_loop(&mut self, loop_stmt: &LoopStmt) -> Option<HirStmt> {
        match loop_stmt {
            LoopStmt::While(w) => {
                let cond = self.lower_expr(&w.condition);
                let body = self.lower_block(&w.body, w.span);

                Some(HirStmt::While {
                    cond,
                    body,
                    span: w.span,
                })
            }

            LoopStmt::For(f) => {
                // Desugar for loop to while loop
                let iter = self.lower_expr(&f.iter);
                let body = self.lower_block(&f.body, f.span);

                // Simplified: treat as while with iterator
                Some(HirStmt::While {
                    cond: HirExpr::Const(HirConstValue::Bool(true), HirType::Bool, f.span),
                    body,
                    span: f.span,
                })
            }

            LoopStmt::BoundedFor(bf) => {
                let var = self.symbols.lookup(&bf.var.name)?;
                let start = self.lower_expr(&bf.start);
                let end = self.lower_expr(&bf.end);
                let body = self.lower_block(&bf.body, bf.span);

                Some(HirStmt::BoundedLoop {
                    var,
                    start,
                    end,
                    body,
                    span: bf.span,
                })
            }
        }
    }

    /// Lower an expression.
    fn lower_expr(&mut self, expr: &Expression) -> HirExpr {
        match expr {
            Expression::Literal(lit) => self.lower_literal(lit),

            Expression::Variable(var) => {
                if let Some(id) = self.symbols.lookup(&var.name) {
                    let ty = self.infer_expr_type(expr);
                    HirExpr::Read(HirPlace::Local(id), ty, var.span)
                } else {
                    HirExpr::Const(HirConstValue::Unit, HirType::Error, var.span)
                }
            }

            Expression::Call(call) => self.lower_call(call),

            Expression::Arithmetic(arith) => {
                let left = Box::new(self.lower_expr(&arith.left));
                let right = Box::new(self.lower_expr(&arith.right));
                let ty = left.ty().clone(); // Use left operand's type

                HirExpr::Binary {
                    op: HirBinOp::from_arith(arith.op),
                    left,
                    right,
                    ty,
                    span: arith.span,
                }
            }

            Expression::Comparison(cmp) => {
                let left = Box::new(self.lower_expr(&cmp.left));
                let right = Box::new(self.lower_expr(&cmp.right));

                HirExpr::Binary {
                    op: HirBinOp::from_cmp(cmp.op),
                    left,
                    right,
                    ty: HirType::Bool,
                    span: cmp.span,
                }
            }

            Expression::Logical(log) => {
                if log.operands.is_empty() {
                    return HirExpr::Const(HirConstValue::Bool(true), HirType::Bool, log.span);
                }

                if log.operands.len() == 1 && log.op == LogicalOp::Not {
                    let operand = Box::new(self.lower_expr(&log.operands[0]));
                    return HirExpr::Unary {
                        op: HirUnaryOp::Not,
                        operand,
                        ty: HirType::Bool,
                        span: log.span,
                    };
                }

                // Chain binary operations
                let mut result = self.lower_expr(&log.operands[0]);
                let bin_op = HirBinOp::from_logical(log.op).unwrap_or(HirBinOp::And);

                for operand in &log.operands[1..] {
                    let right = self.lower_expr(operand);
                    result = HirExpr::Binary {
                        op: bin_op,
                        left: Box::new(result),
                        right: Box::new(right),
                        ty: HirType::Bool,
                        span: log.span,
                    };
                }

                result
            }

            Expression::Memory(mem) => {
                let base = self.lower_expr(&mem.base);
                let ty = if let HirType::Ptr(inner) = base.ty() {
                    (**inner).clone()
                } else {
                    HirType::Error
                };

                if let Some(offset) = &mem.offset {
                    let index = self.lower_expr(offset);
                    HirExpr::Index {
                        array: Box::new(base),
                        index: Box::new(index),
                        ty,
                        span: mem.span,
                    }
                } else {
                    // Simple dereference
                    HirExpr::Read(HirPlace::Deref(Box::new(base)), ty, mem.span)
                }
            }

            Expression::Capability(cap) => {
                let token = Box::new(HirExpr::Read(
                    HirPlace::Local(
                        self.symbols
                            .lookup(&cap.token.name)
                            .unwrap_or(SymbolId(0)),
                    ),
                    HirType::Int(IntType::U64), // Capability tokens are u64
                    cap.token.span,
                ));
                let args = cap.args.iter().map(|a| self.lower_expr(a)).collect();

                HirExpr::Capability {
                    op: "invoke".to_string(),
                    token,
                    args,
                    ty: HirType::Int(IntType::I64), // Default return type
                    span: cap.span,
                }
            }
        }
    }

    /// Lower a function call.
    fn lower_call(&mut self, call: &FunctionCall) -> HirExpr {
        let func = self.symbols.lookup(&call.name.name).unwrap_or(SymbolId(0));
        let args = call.args.iter().map(|a| self.lower_expr(a)).collect();

        // Get return type from symbol table
        let ty = self
            .symbols
            .get(func)
            .and_then(|s| s.ty.as_ref())
            .map(|t| self.resolved_type_to_hir(t))
            .unwrap_or(HirType::Int(IntType::I64));

        HirExpr::Call {
            func,
            args,
            ty,
            span: call.span,
        }
    }

    /// Lower a literal.
    fn lower_literal(&self, lit: &Literal) -> HirExpr {
        match lit {
            Literal::Integer(n) => HirExpr::Const(
                HirConstValue::Int(*n, IntType::I64),
                HirType::Int(IntType::I64),
                Span::default(),
            ),
            Literal::Float(f) => HirExpr::Const(
                HirConstValue::Float(*f, FloatType::F64),
                HirType::Float(FloatType::F64),
                Span::default(),
            ),
            Literal::Bool(b) => {
                HirExpr::Const(HirConstValue::Bool(*b), HirType::Bool, Span::default())
            }
            Literal::String(s) => {
                // Strings are pointers to u8 arrays
                HirExpr::Const(
                    HirConstValue::Zero(HirType::Ptr(Box::new(HirType::Int(IntType::U8)))),
                    HirType::Ptr(Box::new(HirType::Int(IntType::U8))),
                    Span::default(),
                )
            }
        }
    }

    /// Lower an instruction.
    fn lower_instruction(&self, instr: &Instruction) -> HirAsm {
        match instr {
            Instruction::DataTransfer(dt) => {
                let dest = self.lower_asm_reg(&dt.dest);
                let src = self.lower_asm_operand(&dt.src);
                match dt.op {
                    DataTransferType::Mov => HirAsm::Mov {
                        dest,
                        src,
                        span: dt.span,
                    },
                    DataTransferType::Lea => HirAsm::Lea {
                        dest,
                        src,
                        span: dt.span,
                    },
                }
            }
            Instruction::Arithmetic(arith) => {
                let dest = self.lower_asm_reg(&arith.dest);
                let src = self.lower_asm_operand(&arith.src);
                HirAsm::Arith {
                    op: arith.op,
                    dest,
                    src,
                    span: arith.span,
                }
            }
            Instruction::ControlFlow(cf) => match cf {
                ControlFlowOp::Cmp { reg, operand, span } => {
                    let left = self.lower_asm_reg(reg);
                    let right = self.lower_asm_operand(operand);
                    HirAsm::Cmp {
                        left,
                        right,
                        span: *span,
                    }
                }
                ControlFlowOp::Jump { op, target, span } => HirAsm::Jump {
                    op: *op,
                    target: target.name.clone(),
                    span: *span,
                },
            },
            Instruction::Stack(stack) => {
                let operand = self.lower_asm_operand(&stack.operand);
                match stack.op {
                    StackOpType::Push => HirAsm::Push {
                        operand,
                        span: stack.span,
                    },
                    StackOpType::Pop => HirAsm::Pop {
                        operand,
                        span: stack.span,
                    },
                }
            }
            Instruction::System(sys) => HirAsm::Ret { span: sys.span },
            Instruction::Memory(_mem) => HirAsm::Ret { span: Span::default() },
            Instruction::Checkpoint(_cp) => HirAsm::Ret { span: Span::default() },
        }
    }

    /// Lower a register to ASM operand.
    fn lower_asm_reg(&self, reg: &Register) -> HirAsmOperand {
        HirAsmOperand::Reg(reg.name.clone())
    }

    /// Lower an operand to ASM operand.
    fn lower_asm_operand(&self, operand: &Operand) -> HirAsmOperand {
        match operand {
            Operand::Register(reg) => HirAsmOperand::Reg(reg.name.clone()),
            Operand::Expression(expr) => {
                if let Expression::Literal(Literal::Integer(n)) = expr {
                    HirAsmOperand::Imm(*n)
                } else {
                    HirAsmOperand::Imm(0)
                }
            }
            Operand::Memory(mem) => HirAsmOperand::Mem {
                base: Some(mem.base.name.clone()),
                index: mem.index.as_ref().map(|r| r.name.clone()),
                scale: mem.scale.unwrap_or(1),
                disp: mem.displacement.unwrap_or(0),
            },
        }
    }

    /// Lower a data value.
    fn lower_data_value(&self, value: &DataValue) -> Option<HirConstValue> {
        match value {
            DataValue::Integer(n) => Some(HirConstValue::Int(*n, IntType::I64)),
            DataValue::String(s) => {
                // Would create string constant
                Some(HirConstValue::Zero(HirType::Ptr(Box::new(HirType::Int(
                    IntType::U8,
                )))))
            }
            DataValue::Identifier(id) => {
                // Would look up symbol
                Some(HirConstValue::Int(0, IntType::I64))
            }
        }
    }

    /// Try to evaluate an expression as a constant.
    fn try_eval_const(&self, expr: &Expression) -> Option<HirConstValue> {
        match expr {
            Expression::Literal(Literal::Integer(n)) => Some(HirConstValue::Int(*n, IntType::I64)),
            Expression::Literal(Literal::Float(f)) => {
                Some(HirConstValue::Float(*f, FloatType::F64))
            }
            Expression::Literal(Literal::Bool(b)) => Some(HirConstValue::Bool(*b)),
            _ => None,
        }
    }

    /// Infer the type of an expression.
    fn infer_expr_type(&self, expr: &Expression) -> HirType {
        match expr {
            Expression::Literal(Literal::Integer(_)) => HirType::Int(IntType::I64),
            Expression::Literal(Literal::Float(_)) => HirType::Float(FloatType::F64),
            Expression::Literal(Literal::Bool(_)) => HirType::Bool,
            Expression::Literal(Literal::String(_)) => {
                HirType::Ptr(Box::new(HirType::Int(IntType::U8)))
            }
            Expression::Variable(var) => {
                if let Some(id) = self.symbols.lookup(&var.name) {
                    if let Some(sym) = self.symbols.get(id) {
                        if let Some(ty) = &sym.ty {
                            return self.resolved_type_to_hir(ty);
                        }
                    }
                }
                HirType::Int(IntType::I64)
            }
            Expression::Arithmetic(_) => HirType::Int(IntType::I64),
            Expression::Comparison(_) => HirType::Bool,
            Expression::Logical(_) => HirType::Bool,
            Expression::Call(call) => {
                if let Some(id) = self.symbols.lookup(&call.name.name) {
                    if let Some(sym) = self.symbols.get(id) {
                        if let Some(ty) = &sym.ty {
                            return self.resolved_type_to_hir(ty);
                        }
                    }
                }
                HirType::Int(IntType::I64)
            }
            _ => HirType::Int(IntType::I64),
        }
    }

    /// Convert resolved type to HIR type.
    fn resolved_type_to_hir(&self, ty: &ResolvedType) -> HirType {
        match ty {
            ResolvedType::Unknown => HirType::Int(IntType::I64),
            ResolvedType::Primitive(p) => HirType::from_primitive(*p),
            ResolvedType::Array { elem, size } => {
                HirType::Array(Box::new(self.resolved_type_to_hir(elem)), *size)
            }
            ResolvedType::Pointer(inner) => {
                HirType::Ptr(Box::new(self.resolved_type_to_hir(inner)))
            }
            ResolvedType::Function { params, ret } => HirType::Func {
                params: params.iter().map(|p| self.resolved_type_to_hir(p)).collect(),
                ret: Box::new(self.resolved_type_to_hir(ret)),
            },
            ResolvedType::Struct(id) => HirType::Struct(*id),
            ResolvedType::Enum(id) => HirType::Enum(*id),
            ResolvedType::User(id) => HirType::Struct(*id), // Assume struct
            ResolvedType::Unit => HirType::Unit,
            ResolvedType::Error => HirType::Error,
        }
    }

    /// Extract phase from annotations.
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

    /// Extract max iterations from annotations.
    fn extract_max_iterations(annotations: &[FunctionAnnotation]) -> Option<u64> {
        // Would parse from complexity annotation
        None
    }
}

/// Convenience function to lower a program.
pub fn lower(program: &[TopLevelForm]) -> Result<HirProgram> {
    // First resolve names
    let mut resolver = NameResolver::new();
    resolver.resolve_program(program)?;
    let symbols = resolver.into_symbol_table();

    // Then lower to HIR
    let mut lowering = Lowering::new(symbols);
    lowering.lower_program(program)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lower_literal() {
        let lowering = Lowering::new(SymbolTable::new());
        let lit = Literal::Integer(42);
        let hir = lowering.lower_literal(&lit);

        match hir {
            HirExpr::Const(HirConstValue::Int(n, _), _, _) => assert_eq!(n, 42),
            _ => panic!("Expected integer constant"),
        }
    }

    #[test]
    fn test_hir_type_conversion() {
        assert_eq!(
            HirType::from_primitive(PrimitiveType::I32),
            HirType::Int(IntType::I32)
        );
    }
}
