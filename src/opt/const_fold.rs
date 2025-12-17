// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Constant folding optimization pass.
//!
//! This pass evaluates constant expressions at compile time, replacing
//! them with their computed values. This reduces runtime overhead and
//! enables further optimizations.
//!
//! # Examples
//!
//! ```text
//! // Before constant folding
//! _t0 = 2 + 3
//! _t1 = _t0 * 4
//!
//! // After constant folding
//! _t0 = 5
//! _t1 = 20
//! ```

use crate::mir::{
    BasicBlock, BinOp, BlockId, ConstValue, Local, MirFunction, MirInstr, Operand, RValue,
    Terminator, UnaryOp,
};
use crate::opt::OptimizationPass;
use std::collections::HashMap;

/// Constant folding optimization pass
pub struct ConstantFolder {
    /// Map from locals to their known constant values
    constants: HashMap<Local, ConstValue>,
    /// Whether any changes were made
    changed: bool,
}

impl ConstantFolder {
    /// Create a new constant folder
    pub fn new() -> Self {
        ConstantFolder {
            constants: HashMap::new(),
            changed: false,
        }
    }

    /// Reset state for a new function
    fn reset(&mut self) {
        self.constants.clear();
        self.changed = false;
    }

    /// Try to get a constant value for an operand
    fn get_const<'a>(&'a self, operand: &'a Operand) -> Option<&'a ConstValue> {
        match operand {
            Operand::Const(c) => Some(c),
            Operand::Local(l) => self.constants.get(l),
            _ => None,
        }
    }

    /// Evaluate a binary operation on constants
    fn eval_binop(&self, op: BinOp, left: &ConstValue, right: &ConstValue) -> Option<ConstValue> {
        match (op, left, right) {
            // Integer arithmetic
            (BinOp::Add, ConstValue::Int(l), ConstValue::Int(r)) => {
                Some(ConstValue::Int(l.wrapping_add(*r)))
            }
            (BinOp::Sub, ConstValue::Int(l), ConstValue::Int(r)) => {
                Some(ConstValue::Int(l.wrapping_sub(*r)))
            }
            (BinOp::Mul, ConstValue::Int(l), ConstValue::Int(r)) => {
                Some(ConstValue::Int(l.wrapping_mul(*r)))
            }
            (BinOp::Div, ConstValue::Int(l), ConstValue::Int(r)) if *r != 0 => {
                Some(ConstValue::Int(l.wrapping_div(*r)))
            }
            (BinOp::Rem, ConstValue::Int(l), ConstValue::Int(r)) if *r != 0 => {
                Some(ConstValue::Int(l.wrapping_rem(*r)))
            }

            // Unsigned integer arithmetic
            (BinOp::Add, ConstValue::UInt(l), ConstValue::UInt(r)) => {
                Some(ConstValue::UInt(l.wrapping_add(*r)))
            }
            (BinOp::Sub, ConstValue::UInt(l), ConstValue::UInt(r)) => {
                Some(ConstValue::UInt(l.wrapping_sub(*r)))
            }
            (BinOp::Mul, ConstValue::UInt(l), ConstValue::UInt(r)) => {
                Some(ConstValue::UInt(l.wrapping_mul(*r)))
            }
            (BinOp::Div, ConstValue::UInt(l), ConstValue::UInt(r)) if *r != 0 => {
                Some(ConstValue::UInt(l.wrapping_div(*r)))
            }
            (BinOp::Rem, ConstValue::UInt(l), ConstValue::UInt(r)) if *r != 0 => {
                Some(ConstValue::UInt(l.wrapping_rem(*r)))
            }

            // Float arithmetic
            (BinOp::Add, ConstValue::Float(l), ConstValue::Float(r)) => {
                Some(ConstValue::Float(l + r))
            }
            (BinOp::Sub, ConstValue::Float(l), ConstValue::Float(r)) => {
                Some(ConstValue::Float(l - r))
            }
            (BinOp::Mul, ConstValue::Float(l), ConstValue::Float(r)) => {
                Some(ConstValue::Float(l * r))
            }
            (BinOp::Div, ConstValue::Float(l), ConstValue::Float(r)) => {
                Some(ConstValue::Float(l / r))
            }

            // Bitwise operations (integers)
            (BinOp::BitAnd, ConstValue::Int(l), ConstValue::Int(r)) => {
                Some(ConstValue::Int(l & r))
            }
            (BinOp::BitOr, ConstValue::Int(l), ConstValue::Int(r)) => Some(ConstValue::Int(l | r)),
            (BinOp::BitXor, ConstValue::Int(l), ConstValue::Int(r)) => {
                Some(ConstValue::Int(l ^ r))
            }
            (BinOp::Shl, ConstValue::Int(l), ConstValue::Int(r)) if *r >= 0 && *r < 64 => {
                Some(ConstValue::Int(l.wrapping_shl(*r as u32)))
            }
            (BinOp::Shr, ConstValue::Int(l), ConstValue::Int(r)) if *r >= 0 && *r < 64 => {
                Some(ConstValue::Int(l.wrapping_shr(*r as u32)))
            }

            // Bitwise operations (unsigned)
            (BinOp::BitAnd, ConstValue::UInt(l), ConstValue::UInt(r)) => {
                Some(ConstValue::UInt(l & r))
            }
            (BinOp::BitOr, ConstValue::UInt(l), ConstValue::UInt(r)) => {
                Some(ConstValue::UInt(l | r))
            }
            (BinOp::BitXor, ConstValue::UInt(l), ConstValue::UInt(r)) => {
                Some(ConstValue::UInt(l ^ r))
            }
            (BinOp::Shl, ConstValue::UInt(l), ConstValue::UInt(r)) if *r < 64 => {
                Some(ConstValue::UInt(l.wrapping_shl(*r as u32)))
            }
            (BinOp::Shr, ConstValue::UInt(l), ConstValue::UInt(r)) if *r < 64 => {
                Some(ConstValue::UInt(l.wrapping_shr(*r as u32)))
            }

            // Integer comparisons
            (BinOp::Eq, ConstValue::Int(l), ConstValue::Int(r)) => Some(ConstValue::Bool(l == r)),
            (BinOp::Ne, ConstValue::Int(l), ConstValue::Int(r)) => Some(ConstValue::Bool(l != r)),
            (BinOp::Lt, ConstValue::Int(l), ConstValue::Int(r)) => Some(ConstValue::Bool(l < r)),
            (BinOp::Le, ConstValue::Int(l), ConstValue::Int(r)) => Some(ConstValue::Bool(l <= r)),
            (BinOp::Gt, ConstValue::Int(l), ConstValue::Int(r)) => Some(ConstValue::Bool(l > r)),
            (BinOp::Ge, ConstValue::Int(l), ConstValue::Int(r)) => Some(ConstValue::Bool(l >= r)),

            // Unsigned comparisons
            (BinOp::Eq, ConstValue::UInt(l), ConstValue::UInt(r)) => Some(ConstValue::Bool(l == r)),
            (BinOp::Ne, ConstValue::UInt(l), ConstValue::UInt(r)) => Some(ConstValue::Bool(l != r)),
            (BinOp::Lt, ConstValue::UInt(l), ConstValue::UInt(r)) => Some(ConstValue::Bool(l < r)),
            (BinOp::Le, ConstValue::UInt(l), ConstValue::UInt(r)) => {
                Some(ConstValue::Bool(l <= r))
            }
            (BinOp::Gt, ConstValue::UInt(l), ConstValue::UInt(r)) => Some(ConstValue::Bool(l > r)),
            (BinOp::Ge, ConstValue::UInt(l), ConstValue::UInt(r)) => {
                Some(ConstValue::Bool(l >= r))
            }

            // Float comparisons
            (BinOp::Eq, ConstValue::Float(l), ConstValue::Float(r)) => {
                Some(ConstValue::Bool(l == r))
            }
            (BinOp::Ne, ConstValue::Float(l), ConstValue::Float(r)) => {
                Some(ConstValue::Bool(l != r))
            }
            (BinOp::Lt, ConstValue::Float(l), ConstValue::Float(r)) => {
                Some(ConstValue::Bool(l < r))
            }
            (BinOp::Le, ConstValue::Float(l), ConstValue::Float(r)) => {
                Some(ConstValue::Bool(l <= r))
            }
            (BinOp::Gt, ConstValue::Float(l), ConstValue::Float(r)) => {
                Some(ConstValue::Bool(l > r))
            }
            (BinOp::Ge, ConstValue::Float(l), ConstValue::Float(r)) => {
                Some(ConstValue::Bool(l >= r))
            }

            // Boolean comparisons
            (BinOp::Eq, ConstValue::Bool(l), ConstValue::Bool(r)) => Some(ConstValue::Bool(l == r)),
            (BinOp::Ne, ConstValue::Bool(l), ConstValue::Bool(r)) => Some(ConstValue::Bool(l != r)),

            // Logical operations
            (BinOp::And, ConstValue::Bool(l), ConstValue::Bool(r)) => {
                Some(ConstValue::Bool(*l && *r))
            }
            (BinOp::Or, ConstValue::Bool(l), ConstValue::Bool(r)) => {
                Some(ConstValue::Bool(*l || *r))
            }

            _ => None,
        }
    }

    /// Evaluate a unary operation on a constant
    fn eval_unaryop(&self, op: UnaryOp, operand: &ConstValue) -> Option<ConstValue> {
        match (op, operand) {
            (UnaryOp::Neg, ConstValue::Int(v)) => Some(ConstValue::Int(v.wrapping_neg())),
            (UnaryOp::Neg, ConstValue::Float(v)) => Some(ConstValue::Float(-v)),
            (UnaryOp::Not, ConstValue::Int(v)) => Some(ConstValue::Int(!v)),
            (UnaryOp::Not, ConstValue::UInt(v)) => Some(ConstValue::UInt(!v)),
            (UnaryOp::LogicalNot, ConstValue::Bool(v)) => Some(ConstValue::Bool(!v)),
            _ => None,
        }
    }

    /// Fold constants in an r-value
    fn fold_rvalue(&mut self, rvalue: &mut RValue) -> Option<ConstValue> {
        match rvalue {
            RValue::Use(operand) => {
                if let Some(c) = self.get_const(operand) {
                    return Some(c.clone());
                }
                None
            }
            RValue::BinOp { op, left, right } => {
                let left_const = self.get_const(left);
                let right_const = self.get_const(right);

                if let (Some(l), Some(r)) = (left_const, right_const) {
                    return self.eval_binop(*op, l, r);
                }

                // Partial folding: identity and absorption rules
                self.fold_binop_partial(*op, left, right)
            }
            RValue::UnaryOp { op, operand } => {
                if let Some(c) = self.get_const(operand) {
                    return self.eval_unaryop(*op, c);
                }
                None
            }
            _ => None,
        }
    }

    /// Partial folding for binary operations (identity, absorption)
    fn fold_binop_partial(
        &self,
        op: BinOp,
        left: &mut Operand,
        right: &mut Operand,
    ) -> Option<ConstValue> {
        let left_const = self.get_const(left);
        let right_const = self.get_const(right);

        match op {
            // x + 0 = x, 0 + x = x
            BinOp::Add => {
                if matches!(left_const, Some(ConstValue::Int(0)) | Some(ConstValue::UInt(0))) {
                    // We can't modify the rvalue structure here, just return None
                    return None;
                }
                if matches!(right_const, Some(ConstValue::Int(0)) | Some(ConstValue::UInt(0))) {
                    return None;
                }
            }
            // x * 0 = 0
            BinOp::Mul => {
                if matches!(left_const, Some(ConstValue::Int(0))) {
                    return Some(ConstValue::Int(0));
                }
                if matches!(left_const, Some(ConstValue::UInt(0))) {
                    return Some(ConstValue::UInt(0));
                }
                if matches!(right_const, Some(ConstValue::Int(0))) {
                    return Some(ConstValue::Int(0));
                }
                if matches!(right_const, Some(ConstValue::UInt(0))) {
                    return Some(ConstValue::UInt(0));
                }
                // x * 1 = x
                if matches!(left_const, Some(ConstValue::Int(1)) | Some(ConstValue::UInt(1))) {
                    return None;
                }
                if matches!(right_const, Some(ConstValue::Int(1)) | Some(ConstValue::UInt(1))) {
                    return None;
                }
            }
            // false && x = false, true || x = true
            BinOp::And => {
                if matches!(left_const, Some(ConstValue::Bool(false))) {
                    return Some(ConstValue::Bool(false));
                }
                if matches!(right_const, Some(ConstValue::Bool(false))) {
                    return Some(ConstValue::Bool(false));
                }
            }
            BinOp::Or => {
                if matches!(left_const, Some(ConstValue::Bool(true))) {
                    return Some(ConstValue::Bool(true));
                }
                if matches!(right_const, Some(ConstValue::Bool(true))) {
                    return Some(ConstValue::Bool(true));
                }
            }
            _ => {}
        }

        None
    }

    /// Fold constants in a basic block
    fn fold_block(&mut self, block: &mut BasicBlock) {
        // Process instructions
        for instr in &mut block.instructions {
            if let MirInstr::Assign { dest, value, .. } = instr {
                if let Some(c) = self.fold_rvalue(value) {
                    // Record the constant value
                    self.constants.insert(*dest, c.clone());
                    // Replace the rvalue with the constant
                    *value = RValue::Use(Operand::Const(c));
                    self.changed = true;
                }
            }
        }

        // Fold terminator conditions
        self.fold_terminator(&mut block.terminator);
    }

    /// Fold constants in a terminator
    fn fold_terminator(&mut self, term: &mut Terminator) {
        match term {
            Terminator::Branch {
                cond,
                then_block,
                else_block,
            } => {
                if let Some(ConstValue::Bool(b)) = self.get_const(cond) {
                    // Convert conditional branch to unconditional
                    let target = if *b { *then_block } else { *else_block };
                    *term = Terminator::Goto(target);
                    self.changed = true;
                }
            }
            Terminator::Switch {
                value,
                cases,
                default,
            } => {
                if let Some(ConstValue::Int(v)) = self.get_const(value) {
                    // Find matching case or use default
                    let target = cases
                        .iter()
                        .find(|(case_val, _)| *case_val == *v)
                        .map(|(_, block)| *block)
                        .unwrap_or(*default);
                    *term = Terminator::Goto(target);
                    self.changed = true;
                }
            }
            _ => {}
        }
    }
}

impl Default for ConstantFolder {
    fn default() -> Self {
        Self::new()
    }
}

impl OptimizationPass for ConstantFolder {
    fn name(&self) -> &'static str {
        "constant-folding"
    }

    fn run(&mut self, func: &mut MirFunction) -> bool {
        self.reset();

        // Process blocks in order
        // Note: This is a simplified single-pass algorithm
        // A more sophisticated version would iterate until fixed point
        for i in 0..func.blocks.len() {
            let block = &mut func.blocks[i];
            self.fold_block(block);
        }

        self.changed
    }
}

/// Simplify a binary operation where one operand is constant
pub fn simplify_binop(op: BinOp, left: &Operand, right: &Operand) -> Option<RValue> {
    match (op, left, right) {
        // x + 0 = x
        (BinOp::Add, left, Operand::Const(ConstValue::Int(0)))
        | (BinOp::Add, left, Operand::Const(ConstValue::UInt(0))) => {
            Some(RValue::Use(left.clone()))
        }
        // 0 + x = x
        (BinOp::Add, Operand::Const(ConstValue::Int(0)), right)
        | (BinOp::Add, Operand::Const(ConstValue::UInt(0)), right) => {
            Some(RValue::Use(right.clone()))
        }

        // x - 0 = x
        (BinOp::Sub, left, Operand::Const(ConstValue::Int(0)))
        | (BinOp::Sub, left, Operand::Const(ConstValue::UInt(0))) => {
            Some(RValue::Use(left.clone()))
        }

        // x * 1 = x
        (BinOp::Mul, left, Operand::Const(ConstValue::Int(1)))
        | (BinOp::Mul, left, Operand::Const(ConstValue::UInt(1))) => {
            Some(RValue::Use(left.clone()))
        }
        // 1 * x = x
        (BinOp::Mul, Operand::Const(ConstValue::Int(1)), right)
        | (BinOp::Mul, Operand::Const(ConstValue::UInt(1)), right) => {
            Some(RValue::Use(right.clone()))
        }

        // x * 0 = 0
        (BinOp::Mul, _, Operand::Const(c @ ConstValue::Int(0)))
        | (BinOp::Mul, Operand::Const(c @ ConstValue::Int(0)), _) => {
            Some(RValue::Use(Operand::Const(c.clone())))
        }
        (BinOp::Mul, _, Operand::Const(c @ ConstValue::UInt(0)))
        | (BinOp::Mul, Operand::Const(c @ ConstValue::UInt(0)), _) => {
            Some(RValue::Use(Operand::Const(c.clone())))
        }

        // x / 1 = x
        (BinOp::Div, left, Operand::Const(ConstValue::Int(1)))
        | (BinOp::Div, left, Operand::Const(ConstValue::UInt(1))) => {
            Some(RValue::Use(left.clone()))
        }

        // x && true = x, true && x = x
        (BinOp::And, left, Operand::Const(ConstValue::Bool(true))) => {
            Some(RValue::Use(left.clone()))
        }
        (BinOp::And, Operand::Const(ConstValue::Bool(true)), right) => {
            Some(RValue::Use(right.clone()))
        }

        // x && false = false, false && x = false
        (BinOp::And, _, Operand::Const(c @ ConstValue::Bool(false)))
        | (BinOp::And, Operand::Const(c @ ConstValue::Bool(false)), _) => {
            Some(RValue::Use(Operand::Const(c.clone())))
        }

        // x || false = x, false || x = x
        (BinOp::Or, left, Operand::Const(ConstValue::Bool(false))) => {
            Some(RValue::Use(left.clone()))
        }
        (BinOp::Or, Operand::Const(ConstValue::Bool(false)), right) => {
            Some(RValue::Use(right.clone()))
        }

        // x || true = true, true || x = true
        (BinOp::Or, _, Operand::Const(c @ ConstValue::Bool(true)))
        | (BinOp::Or, Operand::Const(c @ ConstValue::Bool(true)), _) => {
            Some(RValue::Use(Operand::Const(c.clone())))
        }

        // x | 0 = x, 0 | x = x
        (BinOp::BitOr, left, Operand::Const(ConstValue::Int(0)))
        | (BinOp::BitOr, left, Operand::Const(ConstValue::UInt(0))) => {
            Some(RValue::Use(left.clone()))
        }
        (BinOp::BitOr, Operand::Const(ConstValue::Int(0)), right)
        | (BinOp::BitOr, Operand::Const(ConstValue::UInt(0)), right) => {
            Some(RValue::Use(right.clone()))
        }

        // x & 0 = 0
        (BinOp::BitAnd, _, Operand::Const(c @ ConstValue::Int(0)))
        | (BinOp::BitAnd, Operand::Const(c @ ConstValue::Int(0)), _) => {
            Some(RValue::Use(Operand::Const(c.clone())))
        }
        (BinOp::BitAnd, _, Operand::Const(c @ ConstValue::UInt(0)))
        | (BinOp::BitAnd, Operand::Const(c @ ConstValue::UInt(0)), _) => {
            Some(RValue::Use(Operand::Const(c.clone())))
        }

        // x ^ 0 = x, 0 ^ x = x
        (BinOp::BitXor, left, Operand::Const(ConstValue::Int(0)))
        | (BinOp::BitXor, left, Operand::Const(ConstValue::UInt(0))) => {
            Some(RValue::Use(left.clone()))
        }
        (BinOp::BitXor, Operand::Const(ConstValue::Int(0)), right)
        | (BinOp::BitXor, Operand::Const(ConstValue::UInt(0)), right) => {
            Some(RValue::Use(right.clone()))
        }

        // x << 0 = x, x >> 0 = x
        (BinOp::Shl, left, Operand::Const(ConstValue::Int(0)))
        | (BinOp::Shl, left, Operand::Const(ConstValue::UInt(0)))
        | (BinOp::Shr, left, Operand::Const(ConstValue::Int(0)))
        | (BinOp::Shr, left, Operand::Const(ConstValue::UInt(0))) => {
            Some(RValue::Use(left.clone()))
        }

        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mir::{MirType, Terminator};
    use crate::span::Span;
    use crate::symbol::SymbolId;

    fn create_test_func() -> MirFunction {
        MirFunction::new(
            SymbolId(0),
            "test".to_string(),
            vec![],
            MirType::Int(32),
            true,
            Span::new(0, 0),
        )
    }

    #[test]
    fn test_fold_integer_add() {
        let mut func = create_test_func();
        let entry = func.add_block();
        func.entry = entry;

        let dest = func.add_local("x".to_string(), MirType::Int(32), Span::new(0, 0));

        func.blocks[0].instructions.push(MirInstr::Assign {
            dest,
            value: RValue::BinOp {
                op: BinOp::Add,
                left: Operand::const_int(2),
                right: Operand::const_int(3),
            },
            span: Span::new(0, 0),
        });
        func.blocks[0].terminator = Terminator::Return(Some(Operand::local(dest)));

        let mut folder = ConstantFolder::new();
        let changed = folder.run(&mut func);

        assert!(changed);
        match &func.blocks[0].instructions[0] {
            MirInstr::Assign { value, .. } => match value {
                RValue::Use(Operand::Const(ConstValue::Int(5))) => {}
                _ => panic!("Expected constant 5"),
            },
            _ => panic!("Expected assignment"),
        }
    }

    #[test]
    fn test_fold_boolean_and() {
        let folder = ConstantFolder::new();
        let result = folder.eval_binop(
            BinOp::And,
            &ConstValue::Bool(true),
            &ConstValue::Bool(false),
        );
        assert_eq!(result, Some(ConstValue::Bool(false)));

        let result =
            folder.eval_binop(BinOp::And, &ConstValue::Bool(true), &ConstValue::Bool(true));
        assert_eq!(result, Some(ConstValue::Bool(true)));
    }

    #[test]
    fn test_fold_comparisons() {
        let folder = ConstantFolder::new();

        assert_eq!(
            folder.eval_binop(BinOp::Lt, &ConstValue::Int(1), &ConstValue::Int(2)),
            Some(ConstValue::Bool(true))
        );
        assert_eq!(
            folder.eval_binop(BinOp::Gt, &ConstValue::Int(1), &ConstValue::Int(2)),
            Some(ConstValue::Bool(false))
        );
        assert_eq!(
            folder.eval_binop(BinOp::Eq, &ConstValue::Int(5), &ConstValue::Int(5)),
            Some(ConstValue::Bool(true))
        );
    }

    #[test]
    fn test_fold_branch() {
        let mut func = create_test_func();
        let entry = func.add_block();
        let then_block = func.add_block();
        let else_block = func.add_block();
        func.entry = entry;

        func.blocks[0].terminator = Terminator::Branch {
            cond: Operand::const_bool(true),
            then_block,
            else_block,
        };
        func.blocks[1].terminator = Terminator::Return(Some(Operand::const_int(1)));
        func.blocks[2].terminator = Terminator::Return(Some(Operand::const_int(0)));

        let mut folder = ConstantFolder::new();
        folder.run(&mut func);

        assert!(matches!(func.blocks[0].terminator, Terminator::Goto(b) if b == then_block));
    }

    #[test]
    fn test_simplify_binop_identity() {
        // x + 0 = x
        let x = Operand::Local(Local(0));
        let result = simplify_binop(BinOp::Add, &x, &Operand::const_int(0));
        assert!(matches!(result, Some(RValue::Use(Operand::Local(Local(0))))));

        // x * 1 = x
        let result = simplify_binop(BinOp::Mul, &x, &Operand::const_int(1));
        assert!(matches!(result, Some(RValue::Use(Operand::Local(Local(0))))));

        // x * 0 = 0
        let result = simplify_binop(BinOp::Mul, &x, &Operand::const_int(0));
        assert!(matches!(
            result,
            Some(RValue::Use(Operand::Const(ConstValue::Int(0))))
        ));
    }
}
