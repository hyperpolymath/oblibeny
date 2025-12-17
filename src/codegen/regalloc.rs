// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Register allocation for x86-64.
//!
//! This module implements a simple linear scan register allocator.
//! It assigns physical registers to MIR locals, spilling to the stack
//! when necessary.

use crate::codegen::emitter::Reg;
use crate::mir::{BlockId, Local, MirFunction};
use std::collections::{HashMap, HashSet};

/// Live range for a local variable
#[derive(Debug, Clone)]
pub struct LiveRange {
    /// Local variable
    pub local: Local,
    /// Start instruction index (inclusive)
    pub start: u32,
    /// End instruction index (inclusive)
    pub end: u32,
}

impl LiveRange {
    /// Check if this range overlaps with another
    pub fn overlaps(&self, other: &LiveRange) -> bool {
        self.start <= other.end && other.start <= self.end
    }
}

/// Allocation result for a local
#[derive(Debug, Clone, Copy)]
pub enum Allocation {
    /// Allocated to a register
    Reg(Reg),
    /// Spilled to a stack slot (offset from RBP)
    Stack(i64),
}

/// Register allocator
pub struct RegisterAllocator {
    /// Available registers for allocation
    available_regs: Vec<Reg>,
    /// Live ranges for all locals
    live_ranges: Vec<LiveRange>,
    /// Allocation results
    allocations: HashMap<Local, Allocation>,
    /// Currently active live ranges (sorted by end point)
    active: Vec<usize>,
    /// Current stack offset for spills
    stack_offset: i64,
    /// Registers currently in use
    used_regs: HashSet<Reg>,
}

impl RegisterAllocator {
    /// Create a new register allocator
    pub fn new() -> Self {
        // Use callee-saved and caller-saved registers (excluding RSP, RBP)
        let available_regs = vec![
            Reg::Rax,
            Reg::Rcx,
            Reg::Rdx,
            Reg::Rsi,
            Reg::Rdi,
            Reg::R8,
            Reg::R9,
            Reg::R10,
            Reg::R11,
            Reg::Rbx,
            Reg::R12,
            Reg::R13,
            Reg::R14,
            Reg::R15,
        ];

        RegisterAllocator {
            available_regs,
            live_ranges: Vec::new(),
            allocations: HashMap::new(),
            active: Vec::new(),
            stack_offset: 8, // Start after saved RBP
            used_regs: HashSet::new(),
        }
    }

    /// Compute live ranges for a function
    pub fn compute_live_ranges(&mut self, func: &MirFunction) {
        self.live_ranges.clear();

        // Simple approximation: live range spans from first def to last use
        let mut first_def: HashMap<Local, u32> = HashMap::new();
        let mut last_use: HashMap<Local, u32> = HashMap::new();

        let mut instr_idx = 0u32;

        for block in &func.blocks {
            // Process instructions
            for instr in &block.instructions {
                self.update_ranges_for_instr(instr, instr_idx, &mut first_def, &mut last_use);
                instr_idx += 1;
            }

            // Process terminator uses
            self.update_ranges_for_terminator(&block.terminator, instr_idx, &mut last_use);
            instr_idx += 1;
        }

        // Create live ranges
        for (local, start) in &first_def {
            let end = last_use.get(local).copied().unwrap_or(*start);
            self.live_ranges.push(LiveRange {
                local: *local,
                start: *start,
                end,
            });
        }

        // Sort by start point
        self.live_ranges.sort_by_key(|r| r.start);
    }

    /// Update live ranges based on instruction definitions and uses
    fn update_ranges_for_instr(
        &self,
        instr: &crate::mir::MirInstr,
        idx: u32,
        first_def: &mut HashMap<Local, u32>,
        last_use: &mut HashMap<Local, u32>,
    ) {
        use crate::mir::{MirInstr, Operand, RValue};

        match instr {
            MirInstr::Assign { dest, value, .. } => {
                // Definition
                first_def.entry(*dest).or_insert(idx);

                // Uses in rvalue
                match value {
                    RValue::Use(op) => self.mark_operand_use(op, idx, last_use),
                    RValue::Load { ptr, .. } => self.mark_operand_use(ptr, idx, last_use),
                    RValue::BinOp { left, right, .. } => {
                        self.mark_operand_use(left, idx, last_use);
                        self.mark_operand_use(right, idx, last_use);
                    }
                    RValue::UnaryOp { operand, .. } => {
                        self.mark_operand_use(operand, idx, last_use);
                    }
                    RValue::Cast { operand, .. } => {
                        self.mark_operand_use(operand, idx, last_use);
                    }
                    RValue::Aggregate { fields, .. } => {
                        for field in fields {
                            self.mark_operand_use(field, idx, last_use);
                        }
                    }
                    RValue::ExtractField { aggregate, .. } => {
                        self.mark_operand_use(aggregate, idx, last_use);
                    }
                    RValue::Index { array, index } => {
                        self.mark_operand_use(array, idx, last_use);
                        self.mark_operand_use(index, idx, last_use);
                    }
                    RValue::Len { array } => {
                        self.mark_operand_use(array, idx, last_use);
                    }
                    RValue::AddressOf { .. } => {}
                }
            }
            MirInstr::Store { ptr, value, .. } => {
                self.mark_operand_use(ptr, idx, last_use);
                self.mark_operand_use(value, idx, last_use);
            }
            MirInstr::Call { dest, args, func, .. } => {
                if let Some(d) = dest {
                    first_def.entry(*d).or_insert(idx);
                }
                for arg in args {
                    self.mark_operand_use(arg, idx, last_use);
                }
                if let crate::mir::CallTarget::Indirect(op) = func {
                    self.mark_operand_use(op, idx, last_use);
                }
            }
            MirInstr::InvokeCapability { dest, token, args, .. } => {
                if let Some(d) = dest {
                    first_def.entry(*d).or_insert(idx);
                }
                self.mark_operand_use(token, idx, last_use);
                for arg in args {
                    self.mark_operand_use(arg, idx, last_use);
                }
            }
            MirInstr::InlineAsm { outputs, inputs, .. } => {
                for (_, local) in outputs {
                    first_def.entry(*local).or_insert(idx);
                }
                for (_, op) in inputs {
                    self.mark_operand_use(op, idx, last_use);
                }
            }
            MirInstr::Nop => {}
        }
    }

    /// Update last use for terminator
    fn update_ranges_for_terminator(
        &self,
        term: &crate::mir::Terminator,
        idx: u32,
        last_use: &mut HashMap<Local, u32>,
    ) {
        use crate::mir::Terminator;

        match term {
            Terminator::Branch { cond, .. } => {
                self.mark_operand_use(cond, idx, last_use);
            }
            Terminator::Switch { value, .. } => {
                self.mark_operand_use(value, idx, last_use);
            }
            Terminator::Return(Some(value)) => {
                self.mark_operand_use(value, idx, last_use);
            }
            _ => {}
        }
    }

    /// Mark an operand as used at a given index
    fn mark_operand_use(
        &self,
        operand: &crate::mir::Operand,
        idx: u32,
        last_use: &mut HashMap<Local, u32>,
    ) {
        if let crate::mir::Operand::Local(local) = operand {
            last_use.insert(*local, idx);
        }
    }

    /// Perform linear scan register allocation
    pub fn allocate(&mut self, func: &MirFunction) {
        self.compute_live_ranges(func);
        self.allocations.clear();
        self.active.clear();
        self.used_regs.clear();
        self.stack_offset = 8;

        // Process intervals in order of increasing start point
        for i in 0..self.live_ranges.len() {
            // Copy the values we need before mutable operations
            let start = self.live_ranges[i].start;
            let local = self.live_ranges[i].local;

            // Expire old intervals
            self.expire_old_intervals(start);

            // Try to allocate a register
            if let Some(reg) = self.try_allocate_register(i) {
                self.allocations.insert(local, Allocation::Reg(reg));
                self.used_regs.insert(reg);
                self.active.push(i);
                // Keep active sorted by end point
                self.active.sort_by_key(|&idx| self.live_ranges[idx].end);
            } else {
                // Spill: either this interval or one from active
                self.spill_at_interval(i);
            }
        }
    }

    /// Expire intervals that end before the given point
    fn expire_old_intervals(&mut self, point: u32) {
        self.active.retain(|&idx| {
            let range = &self.live_ranges[idx];
            if range.end < point {
                // Free the register
                if let Some(Allocation::Reg(reg)) = self.allocations.get(&range.local) {
                    self.used_regs.remove(reg);
                }
                false
            } else {
                true
            }
        });
    }

    /// Try to allocate a free register
    fn try_allocate_register(&self, _interval_idx: usize) -> Option<Reg> {
        for &reg in &self.available_regs {
            if !self.used_regs.contains(&reg) {
                return Some(reg);
            }
        }
        None
    }

    /// Spill an interval (either the current one or one from active)
    fn spill_at_interval(&mut self, interval_idx: usize) {
        let interval = &self.live_ranges[interval_idx];

        if let Some(&last_active_idx) = self.active.last() {
            let last_active = &self.live_ranges[last_active_idx];

            if last_active.end > interval.end {
                // Spill the longest active interval, give its register to current
                if let Some(Allocation::Reg(reg)) =
                    self.allocations.get(&last_active.local).copied()
                {
                    // Spill the active one
                    let spill_offset = self.stack_offset;
                    self.stack_offset += 8;
                    self.allocations
                        .insert(last_active.local, Allocation::Stack(spill_offset));

                    // Give the register to the current interval
                    self.allocations.insert(interval.local, Allocation::Reg(reg));
                    self.active.pop();
                    self.active.push(interval_idx);
                    self.active.sort_by_key(|&idx| self.live_ranges[idx].end);
                    return;
                }
            }
        }

        // Spill the current interval
        let spill_offset = self.stack_offset;
        self.stack_offset += 8;
        self.allocations
            .insert(interval.local, Allocation::Stack(spill_offset));
    }

    /// Get the allocation for a local
    pub fn get_allocation(&self, local: Local) -> Option<Allocation> {
        self.allocations.get(&local).copied()
    }

    /// Get the total stack space needed for spills
    pub fn stack_space_needed(&self) -> i64 {
        // Align to 16 bytes
        (self.stack_offset + 15) & !15
    }

    /// Get the set of callee-saved registers used
    pub fn callee_saved_used(&self) -> Vec<Reg> {
        self.used_regs
            .iter()
            .filter(|r| r.is_callee_saved())
            .copied()
            .collect()
    }
}

impl Default for RegisterAllocator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_live_range_overlap() {
        let r1 = LiveRange {
            local: Local(0),
            start: 0,
            end: 10,
        };
        let r2 = LiveRange {
            local: Local(1),
            start: 5,
            end: 15,
        };
        let r3 = LiveRange {
            local: Local(2),
            start: 11,
            end: 20,
        };

        assert!(r1.overlaps(&r2));
        assert!(r2.overlaps(&r1));
        assert!(!r1.overlaps(&r3));
        assert!(r2.overlaps(&r3));
    }

    #[test]
    fn test_allocator_creation() {
        let alloc = RegisterAllocator::new();
        assert!(!alloc.available_regs.is_empty());
    }
}
