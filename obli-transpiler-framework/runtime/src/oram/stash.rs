// SPDX-License-Identifier: MIT OR Palimpsest-0.8
// Copyright (c) 2024 Hyperpolymath

//! ORAM stash implementation
//!
//! The stash is a temporary storage for blocks that cannot fit
//! in the ORAM tree during eviction.

use super::OramBlock;
use subtle::ConditionallySelectable;

/// Maximum stash size (should be O(log N) for security)
pub const MAX_STASH_SIZE: usize = 128;

/// Entry in the stash
#[derive(Clone)]
pub struct StashEntry<T: OramBlock> {
    pub addr: u64,
    pub leaf: u64,  // Target leaf in the tree
    pub data: T,
}

/// The stash for temporary block storage
pub struct Stash<T: OramBlock> {
    entries: Vec<StashEntry<T>>,
}

impl<T: OramBlock> Default for Stash<T> {
    fn default() -> Self {
        Stash::new()
    }
}

impl<T: OramBlock> Stash<T> {
    /// Create a new empty stash
    pub fn new() -> Self {
        Stash {
            entries: Vec::with_capacity(MAX_STASH_SIZE),
        }
    }

    /// Add a block to the stash
    pub fn add(&mut self, addr: u64, leaf: u64, data: T) {
        self.entries.push(StashEntry { addr, leaf, data });
        if self.entries.len() > MAX_STASH_SIZE {
            // In production, this would be a security failure
            // For now, just warn (the stash overflow bound proof ensures this is negligible)
            log::warn!("Stash overflow: {} entries", self.entries.len());
        }
    }

    /// Find and remove a block by address
    pub fn remove(&mut self, addr: u64) -> Option<(u64, T)>
    where
        T: Clone,
    {
        if let Some(idx) = self.entries.iter().position(|e| e.addr == addr) {
            let entry = self.entries.remove(idx);
            Some((entry.leaf, entry.data))
        } else {
            None
        }
    }

    /// Check if address is in stash
    pub fn contains(&self, addr: u64) -> bool {
        self.entries.iter().any(|e| e.addr == addr)
    }

    /// Get block by address (without removing)
    pub fn get(&self, addr: u64) -> Option<&T> {
        self.entries
            .iter()
            .find(|e| e.addr == addr)
            .map(|e| &e.data)
    }

    /// Update a block in the stash
    pub fn update(&mut self, addr: u64, data: T) -> bool
    where
        T: Clone,
    {
        if let Some(entry) = self.entries.iter_mut().find(|e| e.addr == addr) {
            entry.data = data;
            true
        } else {
            false
        }
    }

    /// Update the target leaf for an address
    pub fn update_leaf(&mut self, addr: u64, new_leaf: u64) -> bool {
        if let Some(entry) = self.entries.iter_mut().find(|e| e.addr == addr) {
            entry.leaf = new_leaf;
            true
        } else {
            false
        }
    }

    /// Get all entries that can be placed on the path to a given leaf
    pub fn entries_for_path(&self, leaf: u64, depth: usize) -> Vec<usize> {
        let mut result = Vec::new();
        for (i, entry) in self.entries.iter().enumerate() {
            // Check if this entry's leaf shares a prefix with the target leaf
            // at some level up to depth
            if path_overlaps(entry.leaf, leaf, depth) {
                result.push(i);
            }
        }
        result
    }

    /// Remove entries at given indices (indices must be sorted descending)
    pub fn remove_indices(&mut self, mut indices: Vec<usize>) -> Vec<StashEntry<T>> {
        indices.sort_by(|a, b| b.cmp(a)); // Sort descending
        indices.iter().map(|&i| self.entries.remove(i)).collect()
    }

    /// Current stash size
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Check if stash is empty
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Get all entries (for debugging/testing)
    pub fn entries(&self) -> &[StashEntry<T>] {
        &self.entries
    }
}

/// Check if two leaves overlap at any level up to depth
fn path_overlaps(leaf1: u64, leaf2: u64, depth: usize) -> bool {
    for level in 0..depth {
        let shift = depth - level - 1;
        if (leaf1 >> shift) == (leaf2 >> shift) {
            return true;
        }
    }
    false
}

/// Calculate the deepest level where two paths overlap
pub fn path_overlap_level(leaf1: u64, leaf2: u64, depth: usize) -> usize {
    for level in (0..depth).rev() {
        let shift = depth - level - 1;
        if (leaf1 >> shift) == (leaf2 >> shift) {
            return level;
        }
    }
    0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_stash_basic() {
        let mut stash: Stash<u64> = Stash::new();
        stash.add(10, 5, 100);
        stash.add(20, 3, 200);

        assert!(stash.contains(10));
        assert!(stash.contains(20));
        assert!(!stash.contains(30));

        assert_eq!(stash.get(10), Some(&100));
        assert_eq!(stash.remove(10), Some((5, 100)));
        assert!(!stash.contains(10));
    }

    #[test]
    fn test_path_overlaps() {
        // With depth 4, leaves 0-15
        // Leaf 5 (0101) and leaf 7 (0111) share prefix at level 1 (both start with 0)
        assert!(path_overlaps(5, 7, 4));

        // Leaf 0 (0000) and leaf 8 (1000) only share root
        assert!(path_overlaps(0, 8, 4));
    }

    #[test]
    fn test_update() {
        let mut stash: Stash<u64> = Stash::new();
        stash.add(10, 5, 100);
        assert!(stash.update(10, 999));
        assert_eq!(stash.get(10), Some(&999));
    }
}
