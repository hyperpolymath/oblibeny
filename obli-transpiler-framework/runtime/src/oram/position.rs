// SPDX-License-Identifier: MIT OR Palimpsest-0.8
// Copyright (c) 2024 Hyperpolymath

//! Position map for ORAM
//!
//! Maps logical addresses to random leaf positions in the ORAM tree.
//! For small ORAMs, uses a simple array. For large ORAMs, this would
//! itself be stored in a recursive ORAM.

use crate::crypto::{prf, SecretKey};
use rand::{Rng, SeedableRng};
use rand_chacha::ChaCha20Rng;

/// Position map interface
pub trait PositionMap {
    /// Get the current position for an address
    fn get(&self, addr: u64) -> u64;

    /// Update position and return the old position
    fn update(&mut self, addr: u64, new_pos: u64) -> u64;

    /// Get and update to a new random position
    fn get_and_remap(&mut self, addr: u64) -> (u64, u64);

    /// Number of leaves in the tree
    fn num_leaves(&self) -> u64;
}

/// Simple in-memory position map (for small ORAMs)
pub struct SimplePositionMap {
    positions: Vec<u64>,
    num_leaves: u64,
    rng: ChaCha20Rng,
}

impl SimplePositionMap {
    /// Create a new position map
    pub fn new(capacity: u64, num_leaves: u64, key: &SecretKey) -> Self {
        // Derive RNG seed from key
        let seed = crate::crypto::sha256(key.as_bytes());

        let mut rng = ChaCha20Rng::from_seed(seed);

        // Initialize all positions randomly
        let positions: Vec<u64> = (0..capacity)
            .map(|_| rng.gen_range(0..num_leaves))
            .collect();

        SimplePositionMap {
            positions,
            num_leaves,
            rng,
        }
    }

    /// Get a new random leaf position
    fn random_leaf(&mut self) -> u64 {
        self.rng.gen_range(0..self.num_leaves)
    }
}

impl PositionMap for SimplePositionMap {
    fn get(&self, addr: u64) -> u64 {
        self.positions.get(addr as usize).copied().unwrap_or(0)
    }

    fn update(&mut self, addr: u64, new_pos: u64) -> u64 {
        let idx = addr as usize;
        if idx < self.positions.len() {
            let old = self.positions[idx];
            self.positions[idx] = new_pos;
            old
        } else {
            0
        }
    }

    fn get_and_remap(&mut self, addr: u64) -> (u64, u64) {
        let old_pos = self.get(addr);
        let new_pos = self.random_leaf();
        self.update(addr, new_pos);
        (old_pos, new_pos)
    }

    fn num_leaves(&self) -> u64 {
        self.num_leaves
    }
}

/// PRF-based position map (for use with recursive ORAM)
///
/// Uses a PRF to deterministically compute positions, avoiding
/// the need to store positions explicitly (at the cost of no
/// position updates - used for read-only scenarios or as base case).
pub struct PrfPositionMap {
    key: SecretKey,
    num_leaves: u64,
}

impl PrfPositionMap {
    pub fn new(key: SecretKey, num_leaves: u64) -> Self {
        PrfPositionMap { key, num_leaves }
    }
}

impl PositionMap for PrfPositionMap {
    fn get(&self, addr: u64) -> u64 {
        prf(&self.key, addr) % self.num_leaves
    }

    fn update(&mut self, _addr: u64, _new_pos: u64) -> u64 {
        // PRF-based map doesn't support updates
        panic!("PrfPositionMap does not support updates")
    }

    fn get_and_remap(&mut self, _addr: u64) -> (u64, u64) {
        panic!("PrfPositionMap does not support remapping")
    }

    fn num_leaves(&self) -> u64 {
        self.num_leaves
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_position_map() {
        let key = SecretKey::generate();
        let mut pm = SimplePositionMap::new(100, 16, &key);

        // Get initial position
        let pos1 = pm.get(42);
        assert!(pos1 < 16);

        // Update and verify
        let old = pm.update(42, 7);
        assert_eq!(old, pos1);
        assert_eq!(pm.get(42), 7);

        // Remap
        let (old, new) = pm.get_and_remap(42);
        assert_eq!(old, 7);
        assert!(new < 16);
    }

    #[test]
    fn test_prf_position_map() {
        let key = SecretKey::generate();
        let pm = PrfPositionMap::new(key.clone(), 16);

        // PRF should be deterministic
        let pos1 = pm.get(42);
        let pos2 = pm.get(42);
        assert_eq!(pos1, pos2);
        assert!(pos1 < 16);

        // Different addresses should (usually) have different positions
        let pos3 = pm.get(43);
        // Not guaranteed but very likely
        assert!(pos1 < 16 && pos3 < 16);
    }
}
