// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Dead Code Elimination (DCE) optimization pass.
//!
//! This pass removes:
//! - Unreachable basic blocks (blocks with no predecessors except entry)
//! - Dead assignments (assignments to unused locals)
//! - Trivially dead instructions
//!
//! # Algorithm
//!
//! 1. Mark the entry block as reachable
//! 2. Propagate reachability through control flow edges
//! 3. Remove unreachable blocks
//! 4. Compute liveness for locals
//! 5. Remove dead assignments

use crate::mir::{BasicBlock, BlockId, Local, MirFunction, MirInstr, Operand, RValue, Terminator};
use crate::opt::OptimizationPass;
use std::collections::{HashSet, VecDeque};

/// Dead code elimination pass
pub struct DeadCodeEliminator {
    /// Set of reachable blocks
    reachable: HashSet<BlockId>,
    /// Set of live locals
    live: HashSet<Local>,
    /// Whether changes were made
    changed: bool,
}

impl DeadCodeEliminator {
    /// Create a new dead code eliminator
    pub fn new() -> Self {
        DeadCodeEliminator {
            reachable: HashSet::new(),
            live: HashSet::new(),
            changed: false,
        }
    }

    /// Reset state for a new function
    fn reset(&mut self) {
        self.reachable.clear();
        self.live.clear();
        self.changed = false;
    }

    /// Compute reachable blocks starting from entry
    fn compute_reachability(&mut self, func: &MirFunction) {
        let mut worklist = VecDeque::new();
        worklist.push_back(func.entry);
        self.reachable.insert(func.entry);

        while let Some(block_id) = worklist.pop_front() {
            if let Some(block) = func.block(block_id) {
                for succ in block.terminator.successors() {
                    if self.reachable.insert(succ) {
                        worklist.push_back(succ);
                    }
                }
            }
        }
    }

    /// Remove unreachable blocks from the function
    fn remove_unreachable_blocks(&mut self, func: &mut MirFunction) {
        let original_count = func.blocks.len();

        // Nothing to do if no blocks
        if original_count == 0 {
            return;
        }

        // Create mapping from old block IDs to new ones
        let mut id_map: Vec<Option<BlockId>> = Vec::with_capacity(original_count);
        let mut new_id = 0u32;

        for i in 0..original_count {
            let block_id = BlockId(i as u32);
            if self.reachable.contains(&block_id) {
                id_map.push(Some(BlockId(new_id)));
                new_id += 1;
            } else {
                id_map.push(None);
            }
        }

        // Remove unreachable blocks
        func.blocks.retain(|b| self.reachable.contains(&b.id));

        // Update block IDs and terminator targets
        for (i, block) in func.blocks.iter_mut().enumerate() {
            block.id = BlockId(i as u32);

            // Update terminator targets
            for succ in block.terminator.successors_mut() {
                if let Some(new_id) = id_map[succ.0 as usize] {
                    *succ = new_id;
                }
            }

            // Update phi node predecessors
            for phi in &mut block.phis {
                phi.incoming.retain(|(pred, _)| {
                    if let Some(new_id) = id_map[pred.0 as usize] {
                        true
                    } else {
                        false
                    }
                });
                for (pred, _) in &mut phi.incoming {
                    if let Some(new_id) = id_map[pred.0 as usize] {
                        *pred = new_id;
                    }
                }
            }
        }

        // Update entry block
        if let Some(new_entry) = id_map[func.entry.0 as usize] {
            func.entry = new_entry;
        }

        if func.blocks.len() < original_count {
            self.changed = true;
        }
    }

    /// Compute live locals using backward dataflow
    fn compute_liveness(&mut self, func: &MirFunction) {
        // Simple liveness: a local is live if it's used anywhere
        // A more sophisticated analysis would compute per-block liveness

        for block in &func.blocks {
            // Check terminator uses
            self.mark_operands_live_in_terminator(&block.terminator);

            // Check instruction uses
            for instr in &block.instructions {
                self.mark_operands_live_in_instr(instr);
            }

            // Check phi uses
            for phi in &block.phis {
                for (_, operand) in &phi.incoming {
                    self.mark_operand_live(operand);
                }
            }
        }
    }

    /// Mark operands in a terminator as live
    fn mark_operands_live_in_terminator(&mut self, term: &Terminator) {
        match term {
            Terminator::Branch { cond, .. } => {
                self.mark_operand_live(cond);
            }
            Terminator::Switch { value, .. } => {
                self.mark_operand_live(value);
            }
            Terminator::Return(Some(operand)) => {
                self.mark_operand_live(operand);
            }
            _ => {}
        }
    }

    /// Mark operands in an instruction as live
    fn mark_operands_live_in_instr(&mut self, instr: &MirInstr) {
        match instr {
            MirInstr::Assign { value, .. } => {
                self.mark_operands_live_in_rvalue(value);
            }
            MirInstr::Store { ptr, value, .. } => {
                self.mark_operand_live(ptr);
                self.mark_operand_live(value);
            }
            MirInstr::Call { args, func, .. } => {
                for arg in args {
                    self.mark_operand_live(arg);
                }
                if let crate::mir::CallTarget::Indirect(operand) = func {
                    self.mark_operand_live(operand);
                }
            }
            MirInstr::InvokeCapability { token, args, .. } => {
                self.mark_operand_live(token);
                for arg in args {
                    self.mark_operand_live(arg);
                }
            }
            MirInstr::InlineAsm { inputs, .. } => {
                for (_, operand) in inputs {
                    self.mark_operand_live(operand);
                }
            }
            MirInstr::Nop => {}
        }
    }

    /// Mark operands in an rvalue as live
    fn mark_operands_live_in_rvalue(&mut self, rvalue: &RValue) {
        match rvalue {
            RValue::Use(operand) => {
                self.mark_operand_live(operand);
            }
            RValue::Load { ptr, .. } => {
                self.mark_operand_live(ptr);
            }
            RValue::AddressOf { .. } => {
                // Place doesn't use operands directly
            }
            RValue::BinOp { left, right, .. } => {
                self.mark_operand_live(left);
                self.mark_operand_live(right);
            }
            RValue::UnaryOp { operand, .. } => {
                self.mark_operand_live(operand);
            }
            RValue::Cast { operand, .. } => {
                self.mark_operand_live(operand);
            }
            RValue::Aggregate { fields, .. } => {
                for field in fields {
                    self.mark_operand_live(field);
                }
            }
            RValue::ExtractField { aggregate, .. } => {
                self.mark_operand_live(aggregate);
            }
            RValue::Index { array, index } => {
                self.mark_operand_live(array);
                self.mark_operand_live(index);
            }
            RValue::Len { array } => {
                self.mark_operand_live(array);
            }
        }
    }

    /// Mark a single operand as live
    fn mark_operand_live(&mut self, operand: &Operand) {
        if let Operand::Local(local) = operand {
            self.live.insert(*local);
        }
    }

    /// Remove dead assignments
    fn remove_dead_assignments(&mut self, func: &mut MirFunction) {
        for block in &mut func.blocks {
            let original_len = block.instructions.len();

            block.instructions.retain(|instr| {
                match instr {
                    MirInstr::Assign { dest, .. } => {
                        // Keep if destination is live
                        self.live.contains(dest)
                    }
                    // Keep all other instructions (they have side effects)
                    _ => true,
                }
            });

            if block.instructions.len() < original_len {
                self.changed = true;
            }
        }
    }

    /// Simplify terminators where possible
    fn simplify_terminators(&mut self, func: &mut MirFunction) {
        // First, collect the simplifications to make
        let mut simplifications: Vec<(usize, BlockId)> = Vec::new();

        for (i, block) in func.blocks.iter().enumerate() {
            if let Terminator::Goto(target) = &block.terminator {
                if let Some(target_block) = func.blocks.get(target.0 as usize) {
                    if target_block.instructions.is_empty() && target_block.phis.is_empty() {
                        if let Terminator::Goto(next) = &target_block.terminator {
                            simplifications.push((i, *next));
                        }
                    }
                }
            }
        }

        // Then apply the simplifications
        for (block_idx, new_target) in simplifications {
            func.blocks[block_idx].terminator = Terminator::Goto(new_target);
            self.changed = true;
        }
    }
}

impl Default for DeadCodeEliminator {
    fn default() -> Self {
        Self::new()
    }
}

impl OptimizationPass for DeadCodeEliminator {
    fn name(&self) -> &'static str {
        "dead-code-elimination"
    }

    fn run(&mut self, func: &mut MirFunction) -> bool {
        self.reset();

        // Phase 1: Remove unreachable blocks
        self.compute_reachability(func);
        self.remove_unreachable_blocks(func);

        // Phase 2: Remove dead assignments
        self.compute_liveness(func);
        self.remove_dead_assignments(func);

        self.changed
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
            MirType::Unit,
            true,
            Span::new(0, 0),
        )
    }

    #[test]
    fn test_remove_unreachable_block() {
        let mut func = create_test_func();
        let entry = func.add_block();
        let reachable = func.add_block();
        let _unreachable = func.add_block();
        func.entry = entry;

        // entry -> reachable, unreachable has no predecessors
        func.blocks[0].terminator = Terminator::Goto(reachable);
        func.blocks[1].terminator = Terminator::Return(None);
        func.blocks[2].terminator = Terminator::Return(None);

        let mut dce = DeadCodeEliminator::new();
        let changed = dce.run(&mut func);

        assert!(changed);
        assert_eq!(func.blocks.len(), 2);
    }

    #[test]
    fn test_remove_dead_assignment() {
        let mut func = create_test_func();
        let entry = func.add_block();
        func.entry = entry;

        // Create a local that's never used
        let dead_local = func.add_local("dead".to_string(), MirType::Int(32), Span::new(0, 0));
        let live_local = func.add_local("live".to_string(), MirType::Int(32), Span::new(0, 0));

        // Assign to both
        func.blocks[0].instructions.push(MirInstr::Assign {
            dest: dead_local,
            value: RValue::Use(Operand::const_int(42)),
            span: Span::new(0, 0),
        });
        func.blocks[0].instructions.push(MirInstr::Assign {
            dest: live_local,
            value: RValue::Use(Operand::const_int(100)),
            span: Span::new(0, 0),
        });

        // Only use live_local in return
        func.blocks[0].terminator = Terminator::Return(Some(Operand::local(live_local)));

        let mut dce = DeadCodeEliminator::new();
        let changed = dce.run(&mut func);

        assert!(changed);
        assert_eq!(func.blocks[0].instructions.len(), 1);
    }

    #[test]
    fn test_preserve_all_reachable() {
        let mut func = create_test_func();
        let entry = func.add_block();
        let then_block = func.add_block();
        let else_block = func.add_block();
        let merge = func.add_block();
        func.entry = entry;

        // Diamond CFG: all blocks reachable
        func.blocks[0].terminator = Terminator::Branch {
            cond: Operand::const_bool(true),
            then_block,
            else_block,
        };
        func.blocks[1].terminator = Terminator::Goto(merge);
        func.blocks[2].terminator = Terminator::Goto(merge);
        func.blocks[3].terminator = Terminator::Return(None);

        let mut dce = DeadCodeEliminator::new();
        dce.run(&mut func);

        // All 4 blocks should be preserved
        assert_eq!(func.blocks.len(), 4);
    }
}
