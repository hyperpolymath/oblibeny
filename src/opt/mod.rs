// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Optimization passes for the Oblíbený compiler.
//!
//! This module contains various optimization passes that operate on MIR.
//! All optimizations preserve semantics and are safe for both compile-time
//! and deploy-time code.
//!
//! # Passes
//!
//! - **Constant Folding**: Evaluate constant expressions at compile time
//! - **Dead Code Elimination**: Remove unreachable code
//! - **Copy Propagation**: Eliminate unnecessary copies
//! - **Strength Reduction**: Replace expensive operations with cheaper ones

pub mod const_fold;
pub mod dce;

pub use const_fold::ConstantFolder;
pub use dce::DeadCodeEliminator;

use crate::mir::MirFunction;

/// Trait for optimization passes
pub trait OptimizationPass {
    /// Name of the optimization pass
    fn name(&self) -> &'static str;

    /// Run the optimization pass on a function
    /// Returns true if any changes were made
    fn run(&mut self, func: &mut MirFunction) -> bool;
}

/// Run all optimization passes on a function
pub fn optimize(func: &mut MirFunction, level: OptLevel) {
    let mut passes: Vec<Box<dyn OptimizationPass>> = Vec::new();

    // Always run constant folding
    passes.push(Box::new(ConstantFolder::new()));

    match level {
        OptLevel::None => {
            // No optimizations
            return;
        }
        OptLevel::Basic => {
            passes.push(Box::new(DeadCodeEliminator::new()));
        }
        OptLevel::Standard => {
            passes.push(Box::new(DeadCodeEliminator::new()));
            // Add more passes for standard optimization
        }
        OptLevel::Aggressive => {
            passes.push(Box::new(DeadCodeEliminator::new()));
            // Add more passes for aggressive optimization
        }
    }

    // Run passes until no more changes
    let mut changed = true;
    let mut iterations = 0;
    const MAX_ITERATIONS: u32 = 100;

    while changed && iterations < MAX_ITERATIONS {
        changed = false;
        for pass in &mut passes {
            if pass.run(func) {
                changed = true;
            }
        }
        iterations += 1;
    }
}

/// Optimization level
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptLevel {
    /// No optimizations
    None,
    /// Basic optimizations (constant folding, dead code elimination)
    Basic,
    /// Standard optimizations
    Standard,
    /// Aggressive optimizations (may increase compile time)
    Aggressive,
}

impl Default for OptLevel {
    fn default() -> Self {
        OptLevel::Standard
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_opt_level_default() {
        assert_eq!(OptLevel::default(), OptLevel::Standard);
    }
}
