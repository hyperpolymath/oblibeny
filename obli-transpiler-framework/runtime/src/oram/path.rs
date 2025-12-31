// SPDX-License-Identifier: MIT OR Palimpsest-0.8
// Copyright (c) 2024 Hyperpolymath

//! Path ORAM implementation
//!
//! Path ORAM provides O(log N) bandwidth overhead per access with
//! O(log N) client storage. This implementation follows the original
//! Path ORAM paper by Stefanov et al.

use super::bucket::{Bucket, BUCKET_SIZE};
use super::position::{PositionMap, SimplePositionMap};
use super::stash::{path_overlap_level, Stash, StashEntry};
use super::{OramAccess, OramBlock};
use crate::crypto::SecretKey;
use subtle::ConditionallySelectable;

/// Path ORAM implementation
pub struct PathOram<T: OramBlock> {
    /// The binary tree of buckets (stored as array)
    tree: Vec<Bucket<T>>,
    /// Position map: addr -> leaf
    position_map: SimplePositionMap,
    /// Stash for overflow blocks
    stash: Stash<T>,
    /// Tree depth (log2 of capacity)
    depth: usize,
    /// Number of leaves
    num_leaves: u64,
    /// Logical capacity
    capacity: u64,
}

/// Maximum supported ORAM capacity (2^32 blocks)
/// Limited to prevent integer overflow in tree size calculations
pub const MAX_ORAM_CAPACITY: u64 = 1u64 << 32;

impl<T: OramBlock + ConditionallySelectable> PathOram<T> {
    /// Create a new Path ORAM with given capacity
    ///
    /// # Panics
    /// Panics if capacity is 0 or exceeds MAX_ORAM_CAPACITY
    pub fn new(capacity: u64, key: SecretKey) -> Self {
        // Validate capacity to prevent overflow
        assert!(capacity > 0, "ORAM capacity must be > 0");
        assert!(
            capacity <= MAX_ORAM_CAPACITY,
            "ORAM capacity {} exceeds maximum {} (risk of integer overflow)",
            capacity,
            MAX_ORAM_CAPACITY
        );

        // Calculate tree depth (ceil(log2(capacity)))
        let depth = (64 - capacity.leading_zeros()) as usize;
        let num_leaves = 1u64 << depth;

        // Total nodes in complete binary tree: 2^(depth+1) - 1
        // Safe: depth <= 32, so 2^33 - 1 fits in usize on 64-bit
        let num_nodes = (1usize << (depth + 1)) - 1;

        // Initialize empty tree
        let tree: Vec<Bucket<T>> = (0..num_nodes).map(|_| Bucket::new()).collect();

        // Initialize position map
        let position_map = SimplePositionMap::new(capacity, num_leaves, &key);

        PathOram {
            tree,
            position_map,
            stash: Stash::new(),
            depth,
            num_leaves,
            capacity,
        }
    }

    /// Access (read or write) a block
    ///
    /// # Panics
    /// Panics if addr >= capacity
    fn access(&mut self, addr: u64, op: AccessOp<T>) -> T
    where
        T: Clone,
    {
        // Bounds check to prevent corruption
        assert!(
            addr < self.capacity,
            "ORAM address {} out of bounds (capacity: {})",
            addr,
            self.capacity
        );

        // 1. Look up position and remap
        let (old_leaf, new_leaf) = self.position_map.get_and_remap(addr);

        // 2. Read path from root to old leaf into stash
        self.read_path(old_leaf);

        // 3. Find block in stash and update
        let result = if let Some((_, data)) = self.stash.remove(addr) {
            data
        } else {
            T::default()
        };

        // 4. Prepare new data based on operation
        let new_data = match op {
            AccessOp::Read => result.clone(),
            AccessOp::Write(data) => data,
        };

        // 5. Add block back to stash with new leaf
        self.stash.add(addr, new_leaf, new_data);

        // 6. Evict: write path back
        self.write_path(old_leaf);

        result
    }

    /// Read a path from root to leaf into the stash
    fn read_path(&mut self, leaf: u64)
    where
        T: Clone,
    {
        for level in 0..=self.depth {
            let node_idx = self.path_node(leaf, level);
            let bucket = &mut self.tree[node_idx];

            // Move all real blocks from bucket to stash
            for entry in bucket.entries_mut() {
                if entry.addr != u64::MAX {
                    // Get the leaf for this block from position map
                    let block_leaf = self.position_map.get(entry.addr);
                    self.stash.add(entry.addr, block_leaf, entry.data.clone());
                    entry.addr = u64::MAX;
                    entry.data = T::default();
                }
            }
        }
    }

    /// Write path back from stash
    fn write_path(&mut self, leaf: u64)
    where
        T: Clone,
    {
        // For each level from leaf to root
        for level in (0..=self.depth).rev() {
            let node_idx = self.path_node(leaf, level);
            let bucket = &mut self.tree[node_idx];

            // Find blocks in stash that can be placed at this level
            let mut placed = 0;
            let mut to_remove = Vec::new();

            for (i, entry) in self.stash.entries().iter().enumerate() {
                if placed >= BUCKET_SIZE {
                    break;
                }

                // Check if this block's path passes through this node
                let overlap = path_overlap_level(entry.leaf, leaf, self.depth + 1);
                if overlap >= level {
                    to_remove.push(i);
                    placed += 1;
                }
            }

            // Remove from stash and add to bucket
            let removed: Vec<StashEntry<T>> = self.stash.remove_indices(to_remove);
            for entry in removed {
                bucket.try_add(entry.addr, entry.data);
            }
        }
    }

    /// Get the node index for a given level on the path to leaf
    fn path_node(&self, leaf: u64, level: usize) -> usize {
        // Level 0 is root, level depth is leaf
        // Node index in level-order traversal
        let leaf_offset = leaf as usize;
        let level_start = (1 << level) - 1;
        let node_in_level = leaf_offset >> (self.depth - level);
        level_start + node_in_level
    }
}

/// Access operation type
enum AccessOp<T> {
    Read,
    Write(T),
}

impl<T: OramBlock + ConditionallySelectable + Clone> OramAccess<T> for PathOram<T> {
    fn oram_read(&mut self, addr: u64) -> T {
        self.access(addr, AccessOp::Read)
    }

    fn oram_write(&mut self, addr: u64, value: T) {
        self.access(addr, AccessOp::Write(value));
    }

    fn capacity(&self) -> u64 {
        self.capacity
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_path_oram_basic() {
        let key = SecretKey::generate();
        let mut oram: PathOram<u64> = PathOram::new(100, key);

        // Write and read back
        oram.oram_write(42, 12345);
        assert_eq!(oram.oram_read(42), 12345);
    }

    #[test]
    fn test_path_oram_multiple() {
        let key = SecretKey::generate();
        let mut oram: PathOram<u64> = PathOram::new(100, key);

        for i in 0..20 {
            oram.oram_write(i, i * 100);
        }

        for i in 0..20 {
            assert_eq!(oram.oram_read(i), i * 100);
        }
    }

    #[test]
    fn test_path_oram_overwrite() {
        let key = SecretKey::generate();
        let mut oram: PathOram<u64> = PathOram::new(100, key);

        oram.oram_write(10, 100);
        oram.oram_write(10, 200);
        assert_eq!(oram.oram_read(10), 200);
    }

    #[test]
    fn test_path_node_calculation() {
        let key = SecretKey::generate();
        let oram: PathOram<u64> = PathOram::new(8, key);

        // For depth 3 (8 leaves), tree has 15 nodes
        // Root is node 0
        assert_eq!(oram.path_node(0, 0), 0); // Root for any leaf
        assert_eq!(oram.path_node(7, 0), 0); // Root for any leaf

        // Level 1 has nodes 1, 2
        assert_eq!(oram.path_node(0, 1), 1); // Left child
        assert_eq!(oram.path_node(4, 1), 2); // Right child
    }
}
